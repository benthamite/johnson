;;; johnson-worker.el --- Retrieval worker client for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is the parent-side client for the johnson retrieval
;; worker child Emacs.  It currently implements the receive path: a
;; process filter that only appends raw worker output to a receive
;; buffer, and a bounded decoder that handles at most one
;; newline-terminated frame per ordinary timer callback, deferring to
;; pending user input so lookups never make Emacs unresponsive.  The
;; protocol-failure path is defined here; the worker startup and stop
;; lifecycle is added separately.  The module also provides the default
;; entry-preparation dispatch that turns a retrieved entry into a
;; serializable packet using the format's `:worker-prepare-entry' hook,
;; and the child side: `johnson-worker-main', the persistent stdin
;; command loop a batch Emacs runs to serve retrieval requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'johnson-protocol)

;; The child entrypoint runs after `-l johnson', which requires this
;; file, so johnson proper cannot be required here without a cycle.
(declare-function johnson--get-format "johnson" (name))
(declare-function johnson-close-caches "johnson" ())
(defvar johnson-cache-directory)

;;;; State

(defconst johnson-worker--decode-delay 0.01
  "Seconds an ordinary timer waits before the next bounded decode step.")

(defconst johnson-worker--diagnostics-buffer-name
  " *johnson-worker-diagnostics*"
  "Name of the buffer collecting worker diagnostics and failure records.")

(defvar johnson-worker--process nil
  "Live retrieval worker child process, or nil when none is running.")

(defvar johnson-worker--receive-buffer nil
  "Buffer accumulating raw output received from the worker process.")

(defvar johnson-worker--decode-timer nil
  "Armed ordinary timer for the next bounded decode step, or nil.")

(defvar johnson-worker--message-function nil
  "Function called with each decoded worker message plist.")

(defvar johnson-worker--state 'stopped
  "Lifecycle state of the worker client.")

(defvar johnson-worker--entry-assemblies (make-hash-table :test #'equal)
  "Incomplete entry chunk assemblies keyed by entry identity.")

;;;; Entry preparation

(defun johnson-worker--prepare-entry (format dict match raw)
  "Return a serializable entry packet for FORMAT, DICT, MATCH, and RAW.
FORMAT is the format plist, DICT the dictionary plist, MATCH the
database match plist or nil, and RAW the retrieved entry string.  Call
FORMAT's `:worker-prepare-entry' hook when present; otherwise return
the default packet with a nil render context."
  (if-let* ((prepare (plist-get format :worker-prepare-entry)))
      (funcall prepare dict match raw)
    (list :raw raw :context nil)))

;;;; Receiving

(defun johnson-worker--process-filter (_process output)
  "Append worker OUTPUT and arm the bounded decoder."
  (when (buffer-live-p johnson-worker--receive-buffer)
    (with-current-buffer johnson-worker--receive-buffer
      (goto-char (point-max))
      (insert output)))
  (johnson-worker--schedule-decode))

(defun johnson-worker--schedule-decode ()
  "Schedule one ordinary decoder callback."
  (unless (timerp johnson-worker--decode-timer)
    (setq johnson-worker--decode-timer
          (run-at-time johnson-worker--decode-delay nil
                       #'johnson-worker--decode-next))))

;;;; Bounded decoding

(defun johnson-worker--decode-next ()
  "Handle at most one buffered worker line, deferring to user input.
Clear the armed timer, reschedule without touching the receive buffer
when user input is pending, and otherwise remove and handle one line."
  (setq johnson-worker--decode-timer nil)
  (cond ((eq johnson-worker--state 'failed) nil)
        ((input-pending-p) (johnson-worker--schedule-decode))
        ((buffer-live-p johnson-worker--receive-buffer)
         (johnson-worker--decode-one-line))))

(defun johnson-worker--decode-one-line ()
  "Handle one buffered line, then rearm while complete lines remain."
  (let ((line (johnson-worker--take-line)))
    (cond (line (johnson-worker--dispatch-line line))
          ((johnson-worker--partial-line-oversized-p)
           (johnson-worker--fail-oversized
            (with-current-buffer johnson-worker--receive-buffer
              (buffer-string)))))
    (when (and (not (eq johnson-worker--state 'failed))
               (johnson-worker--complete-line-buffered-p))
      (johnson-worker--schedule-decode))))

(defun johnson-worker--take-line ()
  "Remove and return the first newline-terminated line, or nil."
  (with-current-buffer johnson-worker--receive-buffer
    (goto-char (point-min))
    (when (search-forward "\n" nil t)
      (prog1 (buffer-substring-no-properties (point-min) (point))
        (delete-region (point-min) (point))))))

(defun johnson-worker--partial-line-oversized-p ()
  "Return non-nil when the buffered partial line exceeds the frame bound."
  (> (buffer-size johnson-worker--receive-buffer)
     johnson-protocol-max-frame-bytes))

(defun johnson-worker--complete-line-buffered-p ()
  "Return non-nil when the receive buffer holds a full line."
  (with-current-buffer johnson-worker--receive-buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n" nil t))))

(defun johnson-worker--dispatch-line (line)
  "Handle one complete LINE received from the worker.
An oversized LINE fails the protocol, a `johnson-protocol-prefix' LINE
is decoded and delivered, and any other LINE is recorded as a
diagnostic."
  (cond ((> (string-bytes line) johnson-protocol-max-frame-bytes)
         (johnson-worker--fail-oversized line))
        ((string-prefix-p johnson-protocol-prefix line)
         (johnson-worker--handle-frame line))
        (t (johnson-worker--append-diagnostic line))))

(defun johnson-worker--handle-frame (frame)
  "Decode FRAME and deliver its message, failing the client on error."
  (condition-case err
      (johnson-worker--deliver (johnson-protocol-decode frame))
    (johnson-protocol-error
     (johnson-worker--fail-protocol frame err))))

(defun johnson-worker--deliver (message)
  "Pass decoded MESSAGE to the registered message function."
  (when johnson-worker--message-function
    (funcall johnson-worker--message-function message)))

;;;; Protocol failure

(defun johnson-worker--fail-oversized (line)
  "Fail the worker client because LINE exceeds the maximum frame size."
  (johnson-worker--fail-protocol
   line (list 'johnson-protocol-error "frame exceeds maximum size")))

(defun johnson-worker--fail-protocol (frame error)
  "Fail the worker client after FRAME triggered protocol ERROR.
FRAME is the offending line and ERROR the signaled error condition.
Cancel the decode timer, drop incomplete entry assemblies, record FRAME
and ERROR in the diagnostics buffer, mark the client failed, terminate
the child process, and deliver exactly one protocol-error message."
  (when (timerp johnson-worker--decode-timer)
    (cancel-timer johnson-worker--decode-timer))
  (setq johnson-worker--decode-timer nil)
  (clrhash johnson-worker--entry-assemblies)
  (johnson-worker--append-diagnostic
   (format "Protocol error: %s\nOffending frame: %s"
           (error-message-string error) frame))
  (setq johnson-worker--state 'failed)
  (when (process-live-p johnson-worker--process)
    (delete-process johnson-worker--process))
  (johnson-worker--deliver
   (list :type 'protocol-error
         :message (error-message-string error)
         :diagnostics johnson-worker--diagnostics-buffer-name)))

(defun johnson-worker--append-diagnostic (text)
  "Append TEXT as one line to the worker diagnostics buffer."
  (with-current-buffer
      (get-buffer-create johnson-worker--diagnostics-buffer-name)
    (goto-char (point-max))
    (insert text)
    (unless (bolp)
      (insert "\n"))))

;;;; Child command loop

(defconst johnson-worker--source-file
  (and load-file-name (expand-file-name load-file-name))
  "Absolute path of the johnson-worker file that was loaded.")

(defun johnson-worker-main ()
  "Run the persistent Johnson retrieval worker.
Emit a `ready' frame, then serve newline-terminated command frames
from standard input until EOF or a `shutdown' command, closing the
backend caches before returning.

A `configure' command carries `:cache-directory' and `:formats', a
list of (:name NAME :config PLIST) entries applied through each
format's `:apply-worker-config' hook.  A `request' command carries
`:lookup' and `:dictionary' numbers, the dictionary's `:format',
`:path', and `:name' strings, the looked-up `:word', and `:matches', a
list of (:word WORD :offset OFFSET :length LENGTH) plists used with
`:retrieve-entry' when the format has no `:worker-query' hook.  Every
command plist must start with `:type'."
  (let ((coding-system-for-read 'binary)
        (coding-system-for-write 'binary)
        (configured nil)
        done)
    (johnson-worker--emit-ready)
    (unwind-protect
        (while (not done)
          (pcase (johnson-worker--read-command)
            (:eof (setq done t))
            (`(:type shutdown . ,_) (setq done t))
            ((and message `(:type configure . ,_))
             (johnson-worker--apply-configuration message)
             (setq configured t)
             (johnson-worker--emit '(:type configured)))
            ((and message `(:type request . ,_))
             (if configured
                 (johnson-worker--handle-request message)
               (johnson-worker--emit
                '(:type protocol-error :message "worker is not configured"))))
            (_
             (johnson-worker--emit
              '(:type protocol-error :message "unknown worker command")))))
      (johnson-close-caches))))

(defun johnson-worker--emit-ready ()
  "Emit the `ready' frame identifying this worker build."
  (johnson-worker--emit
   (list :type 'ready
         :protocol johnson-protocol-version
         :emacs-version emacs-version
         :worker-file johnson-worker--source-file
         :worker-sha256 (johnson-worker--source-sha256))))

(defun johnson-worker--source-sha256 ()
  "Return the SHA-256 of the loaded johnson-worker file."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally johnson-worker--source-file)
    (secure-hash 'sha256 (current-buffer))))

(defun johnson-worker--emit (message)
  "Encode MESSAGE and write the frame to standard output."
  (princ (johnson-protocol-encode message)))

(defun johnson-worker--read-command ()
  "Read and decode one command, or return `:eof'."
  (condition-case nil
      (johnson-protocol-decode (read-from-minibuffer ""))
    (end-of-file :eof)))

(defun johnson-worker--apply-configuration (message)
  "Apply the `configure' command MESSAGE to this worker process.
Assign the expanded `:cache-directory' to `johnson-cache-directory',
then hand each `:formats' entry's `:config' to the named format's
`:apply-worker-config' hook."
  (setq johnson-cache-directory
        (expand-file-name (plist-get message :cache-directory)))
  (dolist (entry (plist-get message :formats))
    (when-let* ((format (johnson--get-format (plist-get entry :name)))
                (apply-config (plist-get format :apply-worker-config)))
      (funcall apply-config (plist-get entry :config)))))

(defun johnson-worker--handle-request (message)
  "Serve the `request' command MESSAGE, emitting one dictionary reply.
Emit `dictionary-start', the `entry-chunk' frames of every prepared
entry packet, and `dictionary-complete'; on any error emit a terminal
`dictionary-error' for this dictionary instead."
  (let ((lookup (plist-get message :lookup))
        (dictionary (plist-get message :dictionary)))
    (condition-case err
        (let* ((format (johnson-worker--request-format message))
               (packets (johnson-worker--request-packets format message)))
          (johnson-worker--emit-dictionary
           lookup dictionary (plist-get message :name) packets))
      (error
       (johnson-worker--emit
        (list :type 'dictionary-error :lookup lookup :dictionary dictionary
              :message (error-message-string err)))))))

(defun johnson-worker--request-format (message)
  "Return the format plist named by request MESSAGE, or signal."
  (let ((name (plist-get message :format)))
    (or (johnson--get-format name)
        (error "Format %s is not registered in the worker" name))))

(defun johnson-worker--request-packets (format message)
  "Return the prepared entry packets for request MESSAGE using FORMAT.
Use FORMAT's `:worker-query' hook with the request's word when
present, and otherwise retrieve and prepare each of the request's
matches."
  (let ((dict (johnson-worker--request-dict message)))
    (if-let* ((query (plist-get format :worker-query)))
        (funcall query dict (plist-get message :word))
      (mapcar (lambda (match)
                (johnson-worker--retrieve-match format dict match))
              (plist-get message :matches)))))

(defun johnson-worker--request-dict (message)
  "Return the dictionary plist described by request MESSAGE."
  (list :path (plist-get message :path)
        :name (plist-get message :name)
        :format-name (plist-get message :format)))

(defun johnson-worker--retrieve-match (format dict match)
  "Retrieve and prepare one MATCH of DICT using FORMAT."
  (let ((raw (funcall (plist-get format :retrieve-entry)
                      (plist-get dict :path)
                      (plist-get match :offset)
                      (plist-get match :length))))
    (johnson-worker--prepare-entry format dict match raw)))

(defun johnson-worker--emit-dictionary (lookup dictionary name packets)
  "Emit the reply frames for the PACKETS of one dictionary.
LOOKUP and DICTIONARY identify the request and NAME is the dictionary
display name.  Emit `dictionary-start' only when PACKETS is non-empty,
then each packet's `entry-chunk' frames, then `dictionary-complete'."
  (when packets
    (johnson-worker--emit
     (list :type 'dictionary-start :lookup lookup :dictionary dictionary
           :name name)))
  (let ((entry -1))
    (dolist (packet packets)
      (setq entry (1+ entry))
      (dolist (frame (johnson-protocol-entry-frames
                      (list :lookup lookup :dictionary dictionary
                            :entry entry)
                      packet))
        (princ frame))))
  (johnson-worker--emit
   (list :type 'dictionary-complete :lookup lookup :dictionary dictionary
         :entries (length packets))))

(provide 'johnson-worker)
;;; johnson-worker.el ends here
