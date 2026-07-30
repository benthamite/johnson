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
;; lifecycle is added separately.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'johnson-protocol)

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

(provide 'johnson-worker)
;;; johnson-worker.el ends here
