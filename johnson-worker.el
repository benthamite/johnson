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
;; worker child Emacs.  It implements the receive path: a process
;; filter that only appends raw worker output to a receive buffer, and
;; a bounded decoder that handles at most one newline-terminated frame
;; per ordinary timer callback, deferring to pending user input so
;; lookups never make Emacs unresponsive.  On top of it sits the
;; nonblocking client lifecycle: `johnson-worker-start' spawns the
;; child and returns immediately, the handshake and retrieval state
;; transitions run inside the decoded-message and sentinel handlers,
;; entry chunks are assembled and delivered as single core messages,
;; and superseded lookups keep being validated while their results are
;; discarded.  The protocol-failure path is defined here as well.  The
;; module also provides the default entry-preparation dispatch that
;; turns a retrieved entry into a serializable packet using the
;; format's `:worker-prepare-entry' hook, and the child side:
;; `johnson-worker-main', the persistent stdin command loop a batch
;; Emacs runs to serve retrieval requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'johnson-protocol)

;; The child entrypoint runs after `-l johnson', which requires this
;; file, so johnson proper cannot be required here without a cycle.
(declare-function johnson--get-format "johnson" (name))
(declare-function johnson-close-caches "johnson" ())
(defvar johnson-cache-directory)
(defvar johnson--formats)

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

(defvar johnson-worker--core-function nil
  "Function receiving the client's core messages, or nil.
Registered by `johnson-worker-start'; called with completed `entry'
messages, dictionary messages of the current lookup, `protocol-error'
failures, and `worker-exit' reports.")

(defvar johnson-worker--terminating nil
  "Non-nil when the current worker termination is intentional.
Set before deliberately killing the child so the sentinel cleans up
without delivering a `worker-exit' message.")

(defvar johnson-worker--pending-requests nil
  "Queue of submitted request plists awaiting dispatch, oldest first.")

(defvar johnson-worker--active-request nil
  "Tracking plist of the in-flight request, or nil.
Carries the submitted `:request', its `:lookup' and `:dictionary'
sequences, the `:stale' supersession flag, the `:started' marker, and
the `:next-entry', `:entry', and `:chunk' sequence expectations.")

(defconst johnson-worker--source-file
  (and load-file-name (expand-file-name load-file-name))
  "Absolute path of the johnson-worker file that was loaded.")

(defvar johnson-worker-command-function #'johnson-worker--default-command
  "Function returning the command list that spawns the worker child.")

;;;; Lifecycle

(defun johnson-worker-start (message-function)
  "Start the retrieval worker child, reporting to MESSAGE-FUNCTION.
MESSAGE-FUNCTION is called with each core message plist: completed
`entry' messages, `dictionary-start' and terminal dictionary messages
of the current lookup, `protocol-error' failures, `worker-exit'
reports, and `worker-start-error' failures.  Stop any previous worker,
spawn the child with the command returned by
`johnson-worker-command-function', and return immediately after
`make-process' in the `starting' state; the handshake continues in the
decoded-message and sentinel handlers.  When spawning itself signals,
mark the client failed and deliver one `worker-start-error' message."
  (johnson-worker-stop)
  (setq johnson-worker--core-function message-function)
  (setq johnson-worker--message-function #'johnson-worker--handle-message)
  (setq johnson-worker--terminating nil)
  (setq johnson-worker--receive-buffer
        (generate-new-buffer " *johnson-worker-receive*"))
  (setq johnson-worker--state 'starting)
  (let (command)
    (condition-case err
        (progn
          (setq command (funcall johnson-worker-command-function))
          (setq johnson-worker--process
                (make-process
                 :name "johnson-worker"
                 :command command
                 :connection-type 'pipe
                 :coding 'binary
                 :noquery t
                 :filter #'johnson-worker--process-filter
                 :sentinel #'johnson-worker--sentinel)))
      (error (johnson-worker--fail-start command err)))))

(defun johnson-worker--default-command ()
  "Return the batch Emacs command list running `johnson-worker-main'."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        "-L" (file-name-directory johnson-worker--source-file)
        "-l" "johnson"
        "--funcall" "johnson-worker-main"))

(defun johnson-worker--fail-start (command error)
  "Fail the worker client because spawning COMMAND signaled ERROR.
COMMAND is the attempted command list, or nil when
`johnson-worker-command-function' itself signaled before producing
one, and ERROR the signaled error condition.  Record both in the
diagnostics buffer, drop the client state, mark the client failed, and
deliver exactly one `worker-start-error' message whose `:message'
names the attempted executable and whose `:diagnostics' names the
diagnostics buffer."
  (let ((message (format "%s (%s)" (error-message-string error)
                         (or (car command)
                             "johnson-worker-command-function"))))
    (johnson-worker--append-diagnostic
     (format "Worker start error: %s\nAttempted command: %s"
             (error-message-string error)
             (if command (string-join command " ")
               "none: the command function signaled")))
    (johnson-worker--clear-client-state)
    (setq johnson-worker--state 'failed)
    (johnson-worker--deliver-core
     (list :type 'worker-start-error
           :message message
           :diagnostics johnson-worker--diagnostics-buffer-name))))

(defun johnson-worker-submit (request)
  "Queue retrieval REQUEST, superseding queued work of older lookups.
REQUEST is a plist with `:lookup', `:dictionary', `:format', `:path',
`:name', `:word', and `:matches' keys.  A REQUEST for a new lookup
marks the in-flight request stale-discarding and drops queued requests
of other lookups; dispatch happens once the worker is ready."
  (let ((lookup (plist-get request :lookup)))
    (when (and johnson-worker--active-request
               (not (equal lookup
                           (plist-get johnson-worker--active-request
                                      :lookup))))
      (plist-put johnson-worker--active-request :stale t))
    (setq johnson-worker--pending-requests
          (append (cl-remove-if-not
                   (lambda (pending)
                     (equal (plist-get pending :lookup) lookup))
                   johnson-worker--pending-requests)
                  (list request)))
    (johnson-worker--dispatch-next)))

(defun johnson-worker--dispatch-next ()
  "Send the next queued request when the worker is ready."
  (when (and (eq johnson-worker--state 'ready)
             johnson-worker--pending-requests
             (process-live-p johnson-worker--process))
    (let ((request (pop johnson-worker--pending-requests)))
      (setq johnson-worker--active-request
            (list :request request
                  :lookup (plist-get request :lookup)
                  :dictionary (plist-get request :dictionary)
                  :stale nil
                  :started nil
                  :next-entry 0
                  :entry nil
                  :chunk nil))
      (setq johnson-worker--state 'retrieving)
      (johnson-worker--send (append (list :type 'request) request)))))

(defun johnson-worker-stop ()
  "Stop the retrieval worker immediately, without waiting.
Mark the termination intentional, kill the owned child process when it
is live, and clear the parent-side client state; the sentinel then
cleans up without reporting a failure."
  (setq johnson-worker--terminating t)
  (when (process-live-p johnson-worker--process)
    (delete-process johnson-worker--process))
  (johnson-worker--clear-client-state)
  (setq johnson-worker--state 'stopped))

(defun johnson-worker--shutdown-at-exit ()
  "Shut the worker down for Emacs exit, without waiting.
When the child is ready and reading commands, mark the termination
intentional, send the `shutdown' frame, and close the child's standard
input; in any other live state fall back to `johnson-worker-stop',
because a child blocked in a retrieval cannot read a shutdown frame."
  (cond ((not (johnson-worker-live-p)) nil)
        ((johnson-worker-ready-p)
         (setq johnson-worker--terminating t)
         (johnson-worker--send '(:type shutdown))
         (process-send-eof johnson-worker--process))
        (t (johnson-worker-stop))))

;; Installed at load time so every Emacs exit shuts the persistent
;; child down, without waiting, whatever state it is in.
(add-hook 'kill-emacs-hook #'johnson-worker--shutdown-at-exit)

(defun johnson-worker-live-p ()
  "Return non-nil when the worker child process is live."
  (process-live-p johnson-worker--process))

(defun johnson-worker-ready-p ()
  "Return non-nil when the worker is idle and ready for a request."
  (eq johnson-worker--state 'ready))

(defun johnson-worker--send (message)
  "Encode MESSAGE and send the frame to the worker child.
MESSAGE must start with `:type'; the child dispatches positionally.
Signal `johnson-protocol-error' without writing anything when the
encoded frame exceeds `johnson-protocol-max-frame-bytes', because the
child would reject the oversized frame anyway."
  (let ((frame (johnson-protocol-encode message)))
    (when (> (string-bytes frame) johnson-protocol-max-frame-bytes)
      (signal 'johnson-protocol-error '("frame exceeds maximum size")))
    (process-send-string johnson-worker--process frame)))

(defun johnson-worker--clear-client-state ()
  "Drop the worker process, decode timer, queues, and receive buffer."
  (when (timerp johnson-worker--decode-timer)
    (cancel-timer johnson-worker--decode-timer))
  (setq johnson-worker--decode-timer nil)
  (setq johnson-worker--process nil)
  (setq johnson-worker--pending-requests nil)
  (setq johnson-worker--active-request nil)
  (clrhash johnson-worker--entry-assemblies)
  (when (buffer-live-p johnson-worker--receive-buffer)
    (kill-buffer johnson-worker--receive-buffer))
  (setq johnson-worker--receive-buffer nil))

(defun johnson-worker--sentinel (process event)
  "Clean up after worker PROCESS terminated as described by EVENT.
Ignore state changes of processes other than the current worker and
events that leave PROCESS alive.  Clear the client state, retaining the
diagnostics buffer, and unless the termination was intentional, mark
the client failed and deliver one `worker-exit' message naming the
diagnostics buffer."
  (when (and (eq process johnson-worker--process)
             (not (process-live-p process)))
    (let ((intentional johnson-worker--terminating))
      (johnson-worker--clear-client-state)
      (cond ((not intentional)
             (setq johnson-worker--state 'failed)
             (johnson-worker--deliver-core
              (list :type 'worker-exit
                    :status (string-trim event)
                    :diagnostics johnson-worker--diagnostics-buffer-name)))
            ((not (eq johnson-worker--state 'failed))
             (setq johnson-worker--state 'stopped))))))

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

;;;; Client message handling

(defun johnson-worker--handle-message (message)
  "Advance the client state machine with decoded worker MESSAGE.
Handshake and lifecycle messages update the client state, entry chunks
feed the assembly table, and core-facing messages reach the function
registered by `johnson-worker-start'.  Signal `johnson-protocol-error'
on any MESSAGE that violates the protocol, so the decoder's failure
path runs."
  (pcase (plist-get message :type)
    ('ready (johnson-worker--handle-ready))
    ('configured (johnson-worker--handle-configured))
    ('dictionary-start (johnson-worker--handle-dictionary-start message))
    ('entry-chunk (johnson-worker--handle-entry-chunk message))
    ((or 'dictionary-complete 'dictionary-error)
     (johnson-worker--handle-terminal message))
    ('protocol-error (johnson-worker--handle-protocol-error message))
    (_ (signal 'johnson-protocol-error '("unexpected worker message")))))

(defun johnson-worker--handle-ready ()
  "Send the configuration frame in response to the child `ready' frame."
  (unless (eq johnson-worker--state 'starting)
    (signal 'johnson-protocol-error '("unexpected ready message")))
  (setq johnson-worker--state 'configuring)
  (johnson-worker--send (johnson-worker--configure-frame)))

(defun johnson-worker--configure-frame ()
  "Return the `configure' frame for the current parent configuration.
The frame always carries the expanded `johnson-cache-directory' and one
`:formats' entry for every loaded format with a `:worker-config' hook."
  (list :type 'configure
        :cache-directory (expand-file-name johnson-cache-directory)
        :formats (johnson-worker--format-configs)))

(defun johnson-worker--format-configs ()
  "Return the worker configuration entries of the loaded formats."
  (let (configs)
    (dolist (format johnson--formats)
      (when-let* ((worker-config (plist-get format :worker-config)))
        (push (list :name (plist-get format :name)
                    :config (funcall worker-config))
              configs)))
    (nreverse configs)))

(defun johnson-worker--handle-configured ()
  "Mark the client ready and dispatch queued work."
  (unless (eq johnson-worker--state 'configuring)
    (signal 'johnson-protocol-error '("unexpected configured message")))
  (setq johnson-worker--state 'ready)
  (johnson-worker--dispatch-next))

(defun johnson-worker--handle-dictionary-start (message)
  "Validate the `dictionary-start' MESSAGE and forward it to the core."
  (johnson-worker--check-identity message)
  (let ((active johnson-worker--active-request))
    (when (plist-get active :started)
      (signal 'johnson-protocol-error '("duplicate dictionary start")))
    (plist-put active :started t)
    (unless (plist-get active :stale)
      (johnson-worker--deliver-core message))))

(defun johnson-worker--handle-entry-chunk (message)
  "Validate chunk MESSAGE, store it, and complete its entry when final."
  (johnson-worker--check-identity message)
  (johnson-worker--check-chunk-sequence message)
  (johnson-worker--store-chunk message)
  (let ((active johnson-worker--active-request))
    (plist-put active :chunk (1+ (plist-get message :chunk)))
    (when (equal (plist-get active :chunk) (plist-get message :chunks))
      (johnson-worker--complete-entry message))))

(defun johnson-worker--check-chunk-sequence (message)
  "Require chunk MESSAGE to continue the expected entry and chunk order."
  (let ((active johnson-worker--active-request)
        (entry (plist-get message :entry))
        (chunk (plist-get message :chunk)))
    (if (plist-get active :entry)
        (unless (and (equal entry (plist-get active :entry))
                     (equal chunk (plist-get active :chunk)))
          (signal 'johnson-protocol-error '("entry chunk out of sequence")))
      (unless (and (equal entry (plist-get active :next-entry))
                   (equal chunk 0))
        (signal 'johnson-protocol-error '("entry out of sequence")))
      (plist-put active :entry entry)
      (plist-put active :chunk 0))))

(defun johnson-worker--store-chunk (message)
  "Append chunk MESSAGE to its entry's pending assembly."
  (let ((key (johnson-worker--assembly-key message)))
    (puthash key
             (cons message (gethash key johnson-worker--entry-assemblies))
             johnson-worker--entry-assemblies)))

(defun johnson-worker--assembly-key (message)
  "Return the assembly table key identifying MESSAGE's entry."
  (list (plist-get message :lookup)
        (plist-get message :dictionary)
        (plist-get message :entry)))

(defun johnson-worker--complete-entry (message)
  "Assemble the entry completed by chunk MESSAGE and deliver it.
Validate the collected chunks through `johnson-protocol-assemble-entry'
and pass the single core `entry' message on, unless the request was
superseded, in which case the completed entry is dropped."
  (let* ((active johnson-worker--active-request)
         (key (johnson-worker--assembly-key message))
         (chunks (nreverse (gethash key johnson-worker--entry-assemblies)))
         (packet (johnson-protocol-assemble-entry chunks)))
    (remhash key johnson-worker--entry-assemblies)
    (plist-put active :next-entry (1+ (plist-get active :entry)))
    (plist-put active :entry nil)
    (plist-put active :chunk nil)
    (unless (plist-get active :stale)
      (johnson-worker--deliver-core
       (list :type 'entry
             :lookup (plist-get message :lookup)
             :dictionary (plist-get message :dictionary)
             :entry (plist-get message :entry)
             :raw (plist-get packet :raw)
             :context (plist-get packet :context))))))

(defun johnson-worker--handle-terminal (message)
  "Handle terminal dictionary MESSAGE, then dispatch queued work.
Signal `johnson-protocol-error' when MESSAGE arrives while an entry of
the request is still mid-assembly, whether or not the request is
stale-discarding, because the child never legitimately truncates an
entry.  Mark the client ready before touching the core so a stale
request's terminal frame dispatches the current lookup; deliver
MESSAGE to the core only when the finished request was not
superseded."
  (johnson-worker--check-identity message)
  (when (plist-get johnson-worker--active-request :entry)
    (signal 'johnson-protocol-error '("terminal frame arrived mid-entry")))
  (let ((stale (plist-get johnson-worker--active-request :stale)))
    (johnson-worker--drop-request-assemblies johnson-worker--active-request)
    (setq johnson-worker--active-request nil)
    (setq johnson-worker--state 'ready)
    (johnson-worker--dispatch-next)
    (unless stale
      (johnson-worker--deliver-core message))))

(defun johnson-worker--drop-request-assemblies (active)
  "Remove the entry assemblies of the request tracked by ACTIVE."
  (let ((lookup (plist-get active :lookup))
        (dictionary (plist-get active :dictionary))
        (stale-keys nil))
    (maphash (lambda (key _value)
               (when (and (equal (nth 0 key) lookup)
                          (equal (nth 1 key) dictionary))
                 (push key stale-keys)))
             johnson-worker--entry-assemblies)
    (dolist (key stale-keys)
      (remhash key johnson-worker--entry-assemblies))))

(defun johnson-worker--check-identity (message)
  "Require MESSAGE to belong to the in-flight request."
  (let ((active johnson-worker--active-request))
    (unless (and active
                 (equal (plist-get message :lookup)
                        (plist-get active :lookup))
                 (equal (plist-get message :dictionary)
                        (plist-get active :dictionary)))
      (signal 'johnson-protocol-error
              '("message does not match the in-flight request")))))

(defun johnson-worker--handle-protocol-error (message)
  "Forward the failure MESSAGE, or fail on a child protocol complaint.
The client's own failure path delivers `protocol-error' after marking
the state failed; any other `protocol-error' comes from the child and
means the parent broke the protocol."
  (if (eq johnson-worker--state 'failed)
      (johnson-worker--deliver-core message)
    (signal 'johnson-protocol-error
            (list (or (plist-get message :message)
                      "worker reported a protocol error")))))

(defun johnson-worker--deliver-core (message)
  "Pass core MESSAGE to the function given to `johnson-worker-start'."
  (when johnson-worker--core-function
    (funcall johnson-worker--core-function message)))

;;;; Protocol failure

(defun johnson-worker--fail-oversized (line)
  "Fail the worker client because LINE exceeds the maximum frame size."
  (johnson-worker--fail-protocol
   line (list 'johnson-protocol-error "frame exceeds maximum size")))

(defun johnson-worker--fail-protocol (frame error)
  "Fail the worker client after FRAME triggered protocol ERROR.
FRAME is the offending line and ERROR the signaled error condition.
Cancel the decode timer, drop incomplete entry assemblies and queued
requests, record FRAME and ERROR in the diagnostics buffer, mark the
client failed, terminate the child process as an intentional
termination so the sentinel adds no `worker-exit' report, and deliver
exactly one protocol-error message."
  (when (timerp johnson-worker--decode-timer)
    (cancel-timer johnson-worker--decode-timer))
  (setq johnson-worker--decode-timer nil)
  (clrhash johnson-worker--entry-assemblies)
  (setq johnson-worker--pending-requests nil)
  (setq johnson-worker--active-request nil)
  (johnson-worker--append-diagnostic
   (format "Protocol error: %s\nOffending frame: %s"
           (error-message-string error) frame))
  (setq johnson-worker--state 'failed)
  (setq johnson-worker--terminating t)
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
            (:invalid
             (johnson-worker--emit
              '(:type protocol-error
                :message "malformed worker command frame")))
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
  "Read and decode one command, or return `:eof' or `:invalid'.
Return `:eof' when standard input is exhausted and `:invalid' when the
line is not a well-formed protocol frame, so a garbage line never
kills the command loop."
  (condition-case nil
      (johnson-protocol-decode (read-from-minibuffer ""))
    (end-of-file :eof)
    (johnson-protocol-error :invalid)))

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
