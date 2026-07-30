;;; johnson-worker-test.el --- Tests for johnson-worker -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>

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

;; ERT tests for the parent-side receive path of johnson-worker: the
;; process filter, the bounded one-frame-per-callback decoder, and the
;; protocol-failure path.  The filter tests use real `make-process'
;; pipes, not mocks.  The child tests spawn a real batch Emacs running
;; `johnson-worker-main' over the deterministic `worker-fixture' format
;; and assert the wire-level message sequences and exit codes.  The
;; client lifecycle tests start the real worker through
;; `johnson-worker-start' and assert the nonblocking handshake, entry
;; chunk assembly, strict sequence validation, stale-lookup
;; supersession, and the stop and crash paths.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'johnson-protocol)
(require 'johnson)
(require 'johnson-worker)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)
(require 'johnson-worker-fixture)

;;;; Fixtures

(defvar johnson-worker-test--messages-seen 0
  "Number of messages delivered to the test message function.")

(defvar johnson-worker-test--protocol-errors nil
  "List of protocol-error messages delivered to the test message function.")

(defvar johnson-worker-test--core-messages nil
  "All messages delivered to the test message function, newest first.")

(defun johnson-worker-test--record-message (message)
  "Record one delivered MESSAGE in the test counters."
  (push message johnson-worker-test--core-messages)
  (setq johnson-worker-test--messages-seen
        (1+ johnson-worker-test--messages-seen))
  (when (eq (plist-get message :type) 'protocol-error)
    (push message johnson-worker-test--protocol-errors)))

(defun johnson-worker-test--core-messages-of-type (type)
  "Return the recorded core messages of TYPE in arrival order."
  (seq-filter (lambda (message) (eq (plist-get message :type) type))
              (reverse johnson-worker-test--core-messages)))

(defmacro johnson-worker-test--with-client (process &rest body)
  "Run BODY with fresh worker client state and a live pipe process.
PROCESS is bound to a `cat' child whose filter is the real
`johnson-worker--process-filter'.  The decode delay is bound high so
armed timers never fire on their own; tests step the decoder manually.
All processes, buffers, and timers are cleaned up even on failure."
  (declare (indent 1))
  `(let* ((johnson-worker-test--messages-seen 0)
          (johnson-worker-test--protocol-errors nil)
          (johnson-worker-test--core-messages nil)
          (johnson-worker--decode-delay 60)
          (johnson-worker--receive-buffer
           (generate-new-buffer " *johnson-worker-test-receive*"))
          (johnson-worker--decode-timer nil)
          (johnson-worker--state 'stopped)
          (johnson-worker--terminating nil)
          (johnson-worker--pending-requests nil)
          (johnson-worker--active-request nil)
          (johnson-worker--core-function nil)
          (johnson-worker--message-function
           #'johnson-worker-test--record-message)
          (johnson-worker--entry-assemblies (make-hash-table :test #'equal))
          (,process (make-process
                     :name "johnson-worker-test"
                     :command '("cat")
                     :connection-type 'pipe
                     :coding 'utf-8-unix
                     :noquery t
                     :filter #'johnson-worker--process-filter))
          (johnson-worker--process ,process))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,process)
         (delete-process ,process))
       (cancel-function-timers #'johnson-worker--decode-next)
       (when (buffer-live-p johnson-worker--receive-buffer)
         (kill-buffer johnson-worker--receive-buffer))
       (when (get-buffer johnson-worker--diagnostics-buffer-name)
         (kill-buffer johnson-worker--diagnostics-buffer-name)))))

(defun johnson-worker-test--wait-for-bytes (process count)
  "Wait until the receive buffer echoes COUNT bytes back from PROCESS."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (buffer-size johnson-worker--receive-buffer) count)
                (< (float-time) deadline))
      (accept-process-output process 0.05))
    (should (>= (buffer-size johnson-worker--receive-buffer) count))))

(defun johnson-worker-test--drain ()
  "Step the decoder until it stops rearming itself."
  (while (timerp johnson-worker--decode-timer)
    (cancel-timer johnson-worker--decode-timer)
    (johnson-worker--decode-next)))

(defun johnson-worker-test--entry-burst (entries)
  "Return the concatenated frames of ENTRIES two-chunk entry packets."
  (let ((frames nil))
    (dotimes (i entries)
      (setq frames
            (nconc frames
                   (johnson-protocol-entry-frames
                    (list :lookup 1 :dictionary 1 :entry i)
                    (list :raw (make-string (* 32 1024) ?x)
                          :context nil)))))
    frames))

(defun johnson-worker-test--check-failure (process)
  "Assert the shared protocol-failure postconditions for PROCESS."
  (should (eq johnson-worker--state 'failed))
  (should-not (process-live-p process))
  (should (= (length johnson-worker-test--protocol-errors) 1))
  (should-not (timerp johnson-worker--decode-timer))
  (let ((message (car johnson-worker-test--protocol-errors)))
    (should (eq (plist-get message :type) 'protocol-error))
    (should (stringp (plist-get message :message)))
    (should (equal (plist-get message :diagnostics)
                   johnson-worker--diagnostics-buffer-name))))

;;;; Bounded decoding

(ert-deftest johnson-worker-test-burst-decodes-one-frame-per-callback ()
  (johnson-worker-test--with-client process
    (let* ((frames (johnson-worker-test--entry-burst 25))
           (burst (apply #'concat frames)))
      (should (= (length frames) 50))
      (process-send-string process burst)
      (johnson-worker-test--wait-for-bytes process (string-bytes burst))
      (should (= johnson-worker-test--messages-seen 0))
      (should (timerp johnson-worker--decode-timer))
      (johnson-worker--decode-next)
      (should (= johnson-worker-test--messages-seen 1))
      (should (> (buffer-size johnson-worker--receive-buffer) 0)))))

(ert-deftest johnson-worker-test-partial-frame-is-not-delivered ()
  (johnson-worker-test--with-client process
    (let* ((frame (johnson-protocol-encode '(:type ready)))
           (last (1- (length frame))))
      (dotimes (i last)
        (johnson-worker--process-filter process (string (aref frame i)))
        (johnson-worker-test--drain)
        (should (= johnson-worker-test--messages-seen 0)))
      (johnson-worker--process-filter process (string (aref frame last)))
      (johnson-worker-test--drain)
      (should (= johnson-worker-test--messages-seen 1))
      (should (= (buffer-size johnson-worker--receive-buffer) 0)))))

(ert-deftest johnson-worker-test-decode-defers-to-pending-input ()
  (johnson-worker-test--with-client process
    (let ((frame (johnson-protocol-encode '(:type ready))))
      (johnson-worker--process-filter process frame)
      (cl-letf (((symbol-function 'input-pending-p)
                 (lambda (&optional _check-timers) t)))
        (johnson-worker--decode-next))
      (should (= johnson-worker-test--messages-seen 0))
      (should (= (buffer-size johnson-worker--receive-buffer)
                 (length frame)))
      (should (timerp johnson-worker--decode-timer))
      (johnson-worker-test--drain)
      (should (= johnson-worker-test--messages-seen 1)))))

(ert-deftest johnson-worker-test-diagnostic-line-consumes-budget ()
  (johnson-worker-test--with-client process
    (let ((frame (johnson-protocol-encode '(:type ready))))
      (johnson-worker--process-filter
       process (concat "Loading library foo...\n" frame))
      (johnson-worker--decode-next)
      (should (= johnson-worker-test--messages-seen 0))
      (should (timerp johnson-worker--decode-timer))
      (with-current-buffer (get-buffer johnson-worker--diagnostics-buffer-name)
        (should (string-match-p "Loading library foo"
                                (buffer-string))))
      (johnson-worker-test--drain)
      (should (= johnson-worker-test--messages-seen 1)))))

;;;; Protocol failure

(ert-deftest johnson-worker-test-malformed-frame-fails-protocol ()
  (johnson-worker-test--with-client process
    (let ((line (concat johnson-protocol-prefix "@@@not-base64@@@\n")))
      (process-send-string process line)
      (johnson-worker-test--wait-for-bytes process (string-bytes line))
      (johnson-worker-test--drain)
      (johnson-worker-test--check-failure process)
      (should (= johnson-worker-test--messages-seen 1)))))

(ert-deftest johnson-worker-test-oversized-frame-fails-protocol ()
  (johnson-worker-test--with-client process
    (let ((line (concat johnson-protocol-prefix
                        (make-string (1+ johnson-protocol-max-frame-bytes) ?A)
                        "\n")))
      (process-send-string process line)
      (johnson-worker-test--wait-for-bytes process (string-bytes line))
      (johnson-worker-test--drain)
      (johnson-worker-test--check-failure process)
      (should (= johnson-worker-test--messages-seen 1)))))

(ert-deftest johnson-worker-test-oversized-partial-line-fails-protocol ()
  (johnson-worker-test--with-client process
    (johnson-worker--process-filter
     process (concat johnson-protocol-prefix
                     (make-string (1+ johnson-protocol-max-frame-bytes) ?A)))
    (johnson-worker-test--drain)
    (johnson-worker-test--check-failure process)
    (should (= johnson-worker-test--messages-seen 1))))

;;;; Child harness

(defmacro johnson-worker-test--with-child (child &rest body)
  "Run BODY with CHILD bound to a live worker child Emacs process.
The child runs `johnson-worker-main' over the `worker-fixture' format
with a temporary cache directory.  The child, its stderr buffer, and
the cache directory are cleaned up even when BODY fails."
  (declare (indent 1) (debug (symbolp body)))
  `(johnson-test-support-with-temp-cache-dir
     (let ((,child (johnson-worker-test--start-child)))
       (unwind-protect
           (progn ,@body)
         (johnson-worker-test--cleanup-child ,child)))))

(defun johnson-worker-test--start-child ()
  "Start and return a real worker child Emacs over the fixture format."
  (let* ((stderr (generate-new-buffer " *johnson-worker-child-stderr*"))
         (process (make-process
                   :name "johnson-worker-child"
                   :command (johnson-worker-test--child-command)
                   :connection-type 'pipe
                   :coding 'binary
                   :noquery t
                   :stderr stderr
                   :filter #'johnson-worker-test--child-filter)))
    (process-put process 'output "")
    (process-put process 'stderr-buffer stderr)
    process))

(defun johnson-worker-test--child-command ()
  "Return the command list that spawns the worker child Emacs."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        "-L" johnson-test-support-source-directory
        "-L" johnson-test-support-directory
        "-l" "johnson" "-l" "johnson-worker-fixture"
        "--funcall" "johnson-worker-main"))

(defun johnson-worker-test--child-filter (process output)
  "Append child OUTPUT to the accumulated output of PROCESS."
  (process-put process 'output
               (concat (process-get process 'output) output)))

(defun johnson-worker-test--cleanup-child (child)
  "Terminate CHILD and discard its stderr buffer."
  (johnson-test-support-delete-process child)
  (johnson-test-support-kill-buffer (process-get child 'stderr-buffer)))

(defun johnson-worker-test--child-messages (process)
  "Return the protocol messages decoded from PROCESS output so far."
  (let ((lines (butlast (split-string (process-get process 'output) "\n"))))
    (mapcar #'johnson-protocol-decode
            (seq-filter (lambda (line)
                          (string-prefix-p johnson-protocol-prefix line))
                        lines))))

(defun johnson-worker-test--wait-for-message (process type &optional count)
  "Wait until PROCESS emitted COUNT messages of TYPE, then return all.
COUNT defaults to 1.  Fail the test on timeout; return every message
decoded from PROCESS so far."
  (should (johnson-test-support-wait-for
           (lambda ()
             (>= (johnson-worker-test--count-messages process type)
                 (or count 1)))
           10 process))
  (johnson-worker-test--child-messages process))

(defun johnson-worker-test--count-messages (process type)
  "Return how many TYPE messages PROCESS has emitted."
  (seq-count (lambda (message) (eq (plist-get message :type) type))
             (johnson-worker-test--child-messages process)))

(defun johnson-worker-test--send (process message)
  "Send MESSAGE to child PROCESS as one encoded frame."
  (process-send-string process (johnson-protocol-encode message)))

(defun johnson-worker-test--configure (process)
  "Send the fixture configure frame to PROCESS and wait for the reply."
  (johnson-worker-test--send
   process
   (list :type 'configure
         :cache-directory johnson-cache-directory
         :formats nil))
  (johnson-worker-test--wait-for-message process 'configured))

(defun johnson-worker-test--request (process lookup dictionary offsets)
  "Send PROCESS a fixture request for the matches selected by OFFSETS.
LOOKUP and DICTIONARY identify the request; each element of OFFSETS
becomes one match behavior string."
  (johnson-worker-test--send
   process
   (append (list :type 'request)
           (johnson-worker-test--request-plist lookup dictionary offsets))))

(defun johnson-worker-test--request-plist (lookup dictionary offsets)
  "Return a fixture request plist for LOOKUP, DICTIONARY, and OFFSETS.
Each element of OFFSETS becomes one match behavior string."
  (list :lookup lookup
        :dictionary dictionary
        :format "worker-fixture"
        :path "/fixture/dictionary"
        :name "Worker Fixture"
        :word "fixture"
        :matches (mapcar (lambda (offset)
                           (list :word "fixture" :offset offset :length 0))
                         offsets)))

(defun johnson-worker-test--message-types (messages)
  "Return the `:type' symbols of MESSAGES in order."
  (mapcar (lambda (message) (plist-get message :type)) messages))

(defun johnson-worker-test--entry-raws (process)
  "Return the raw string of each single-chunk entry PROCESS emitted."
  (mapcar (lambda (message)
            (plist-get (johnson-protocol-assemble-entry (list message))
                       :raw))
          (seq-filter (lambda (message)
                        (eq (plist-get message :type) 'entry-chunk))
                      (johnson-worker-test--child-messages process))))

(defun johnson-worker-test--wait-for-exit (process)
  "Wait until PROCESS exits, then return its exit status."
  (should (johnson-test-support-wait-for
           (lambda () (eq (process-status process) 'exit)) 10 process))
  (process-exit-status process))

;;;; Child command loop

(ert-deftest johnson-worker-test-child-handshake-and-request ()
  (johnson-worker-test--with-child child
    (let ((ready (car (johnson-worker-test--wait-for-message child 'ready))))
      (should (equal (plist-get ready :protocol) johnson-protocol-version))
      (should (stringp (plist-get ready :emacs-version)))
      (let ((path (plist-get ready :worker-file)))
        (should (file-name-absolute-p path))
        (should (string-match-p "johnson-worker\\.elc?\\'" path))
        (should (equal (plist-get ready :worker-sha256)
                       (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally path)
                         (secure-hash 'sha256 (current-buffer)))))))
    (johnson-worker-test--configure child)
    (johnson-worker-test--request child 1 0 '("hello"))
    (johnson-worker-test--wait-for-message child 'dictionary-complete)
    (let ((messages (johnson-worker-test--child-messages child)))
      (should (equal (johnson-worker-test--message-types messages)
                     '(ready configured dictionary-start entry-chunk
                       dictionary-complete)))
      (let ((start (nth 2 messages))
            (complete (nth 4 messages))
            (packet (johnson-protocol-assemble-entry
                     (list (nth 3 messages)))))
        (should (equal (plist-get start :name) "Worker Fixture"))
        (should (equal (plist-get complete :entries) 1))
        (should (equal (plist-get packet :raw) "hello"))
        (should (equal (plist-get packet :context) '(:prepared t)))))))

(ert-deftest johnson-worker-test-child-round-trips-unibyte-entries ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (johnson-worker-test--configure child)
    (johnson-worker-test--request child 1 0 '("unibyte"))
    (johnson-worker-test--wait-for-message child 'dictionary-complete)
    (let ((raws (johnson-worker-test--entry-raws child)))
      (should (equal raws (list (unibyte-string 0 1 2 255)))))))

(ert-deftest johnson-worker-test-child-persists-across-requests ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (johnson-worker-test--configure child)
    (let ((pid (process-id child)))
      (johnson-worker-test--request child 1 0 '("count"))
      (johnson-worker-test--wait-for-message child 'dictionary-complete)
      (johnson-worker-test--request child 2 0 '("count" "pid"))
      (johnson-worker-test--wait-for-message child 'dictionary-complete 2)
      (should (process-live-p child))
      (should (equal (process-id child) pid))
      (should (equal (johnson-worker-test--entry-raws child)
                     (list "1" "2" (number-to-string pid)))))))

(ert-deftest johnson-worker-test-child-rejects-request-before-configure ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (johnson-worker-test--request child 1 0 '("hello"))
    (let* ((messages (johnson-worker-test--wait-for-message
                      child 'protocol-error))
           (rejection (seq-find (lambda (message)
                                  (eq (plist-get message :type)
                                      'protocol-error))
                                messages)))
      (should (equal (plist-get rejection :message)
                     "worker is not configured")))
    (johnson-worker-test--configure child)
    (should (process-live-p child))))

(ert-deftest johnson-worker-test-child-exits-nonzero-on-malformed-input ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (process-send-string child "this is not a protocol frame\n")
    (should-not (zerop (johnson-worker-test--wait-for-exit child)))))

(ert-deftest johnson-worker-test-child-exits-cleanly-on-eof ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (process-send-eof child)
    (should (zerop (johnson-worker-test--wait-for-exit child)))))

(ert-deftest johnson-worker-test-child-exits-cleanly-on-shutdown ()
  (johnson-worker-test--with-child child
    (johnson-worker-test--wait-for-message child 'ready)
    (johnson-worker-test--send child '(:type shutdown))
    (should (zerop (johnson-worker-test--wait-for-exit child)))))

;;;; Request handling units

(defun johnson-worker-test--handle-request-messages (message)
  "Run `johnson-worker--handle-request' on MESSAGE; return its output."
  (mapcar #'johnson-protocol-decode
          (split-string
           (with-output-to-string
             (johnson-worker--handle-request message))
           "\n" t)))

(ert-deftest johnson-worker-test-handle-request-uses-worker-query ()
  (let ((johnson--formats johnson--formats))
    (johnson-register-format
     :name "worker-query-fixture"
     :extensions nil
     :detect #'ignore
     :retrieve-entry (lambda (&rest _) (error "retrieve must not run"))
     :worker-query (lambda (dict word)
                     (list (list :raw (format "%s:%s"
                                              (plist-get dict :name) word)
                                 :context nil))))
    (let ((messages (johnson-worker-test--handle-request-messages
                     (list :type 'request :lookup 1 :dictionary 0
                           :format "worker-query-fixture"
                           :path "dict://example" :name "Remote"
                           :word "hello"))))
      (should (equal (johnson-worker-test--message-types messages)
                     '(dictionary-start entry-chunk dictionary-complete)))
      (should (equal (plist-get (johnson-protocol-assemble-entry
                                 (list (nth 1 messages)))
                                :raw)
                     "Remote:hello")))))

(ert-deftest johnson-worker-test-handle-request-unknown-format-errors ()
  (let ((messages (johnson-worker-test--handle-request-messages
                   (list :type 'request :lookup 3 :dictionary 1
                         :format "no-such-format" :path "/nowhere"
                         :name "Nowhere" :word "hello" :matches nil))))
    (should (equal (johnson-worker-test--message-types messages)
                   '(dictionary-error)))
    (let ((failure (car messages)))
      (should (equal (plist-get failure :lookup) 3))
      (should (equal (plist-get failure :dictionary) 1))
      (should (stringp (plist-get failure :message))))))

(ert-deftest johnson-worker-test-handle-request-retrieval-error-is-terminal ()
  (let ((johnson--formats johnson--formats))
    (johnson-register-format
     :name "worker-error-fixture"
     :extensions nil
     :detect #'ignore
     :retrieve-entry (lambda (&rest _) (error "backend exploded")))
    (let ((messages (johnson-worker-test--handle-request-messages
                     (list :type 'request :lookup 1 :dictionary 0
                           :format "worker-error-fixture" :path "/boom"
                           :name "Boom" :word "hello"
                           :matches '((:word "hello" :offset 0 :length 1))))))
      (should (equal (johnson-worker-test--message-types messages)
                     '(dictionary-error)))
      (should (string-match-p "backend exploded"
                              (plist-get (car messages) :message))))))

(ert-deftest johnson-worker-test-handle-request-empty-matches-skips-start ()
  (let ((messages (johnson-worker-test--handle-request-messages
                   (list :type 'request :lookup 1 :dictionary 0
                         :format "worker-fixture" :path "/fixture"
                         :name "Worker Fixture" :word "hello"
                         :matches nil))))
    (should (equal (johnson-worker-test--message-types messages)
                   '(dictionary-complete)))
    (should (equal (plist-get (car messages) :entries) 0))))

;;;; Client harness

(defmacro johnson-worker-test--forbidding-blocking (&rest body)
  "Run BODY with `accept-process-output' and `sit-for' made fatal."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'accept-process-output)
              (lambda (&rest _args) (error "blocking wait")))
             ((symbol-function 'sit-for)
              (lambda (&rest _args) (error "blocking wait"))))
     ,@body))

(defmacro johnson-worker-test--with-live-client (&rest body)
  "Run BODY with fresh client state ready for a real `johnson-worker-start'.
The worker command is rebound to the fixture child command and
`johnson-cache-directory' to a temporary directory.  The client is
stopped and its buffers and timers cleaned up even when BODY fails."
  (declare (indent 0) (debug t))
  `(johnson-test-support-with-temp-cache-dir
     (let ((johnson-worker-command-function
            #'johnson-worker-test--child-command)
           (johnson-worker--process nil)
           (johnson-worker--receive-buffer nil)
           (johnson-worker--decode-timer nil)
           (johnson-worker--message-function nil)
           (johnson-worker--core-function nil)
           (johnson-worker--state 'stopped)
           (johnson-worker--terminating nil)
           (johnson-worker--pending-requests nil)
           (johnson-worker--active-request nil)
           (johnson-worker--entry-assemblies (make-hash-table :test #'equal))
           (johnson-worker-test--messages-seen 0)
           (johnson-worker-test--core-messages nil)
           (johnson-worker-test--protocol-errors nil))
       (unwind-protect
           (progn ,@body)
         (johnson-worker-stop)
         (cancel-function-timers #'johnson-worker--decode-next)
         (when (get-buffer johnson-worker--diagnostics-buffer-name)
           (kill-buffer johnson-worker--diagnostics-buffer-name))))))

(defmacro johnson-worker-test--with-stepped-client (&rest body)
  "Run BODY like `johnson-worker-test--with-live-client', stepped manually.
The decode delay is bound high so armed timers never fire on their own;
BODY advances the decoder with `johnson-worker-test--pump-frame'."
  (declare (indent 0) (debug t))
  `(let ((johnson-worker--decode-delay 60))
     (johnson-worker-test--with-live-client ,@body)))

(defmacro johnson-worker-test--with-retrieving-client (process &rest body)
  "Run BODY with a client already in `retrieving' state over a `cat' child.
PROCESS is bound to the child; the client message handler and sentinel
are the real ones, the core function is the test recorder, and the
active request expects lookup 1, dictionary 0, entry 0.  All processes,
buffers, and timers are cleaned up even on failure."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((johnson-worker-test--messages-seen 0)
          (johnson-worker-test--protocol-errors nil)
          (johnson-worker-test--core-messages nil)
          (johnson-worker--decode-delay 60)
          (johnson-worker--receive-buffer
           (generate-new-buffer " *johnson-worker-test-receive*"))
          (johnson-worker--decode-timer nil)
          (johnson-worker--state 'retrieving)
          (johnson-worker--terminating nil)
          (johnson-worker--pending-requests nil)
          (johnson-worker--active-request
           (list :request nil :lookup 1 :dictionary 0 :stale nil :started t
                 :next-entry 0 :entry nil :chunk nil))
          (johnson-worker--message-function #'johnson-worker--handle-message)
          (johnson-worker--core-function #'johnson-worker-test--record-message)
          (johnson-worker--entry-assemblies (make-hash-table :test #'equal))
          (,process (make-process
                     :name "johnson-worker-test"
                     :command '("cat")
                     :connection-type 'pipe
                     :coding 'binary
                     :noquery t
                     :filter #'johnson-worker--process-filter
                     :sentinel #'johnson-worker--sentinel))
          (johnson-worker--process ,process))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,process)
         (delete-process ,process))
       (cancel-function-timers #'johnson-worker--decode-next)
       (when (buffer-live-p johnson-worker--receive-buffer)
         (kill-buffer johnson-worker--receive-buffer))
       (when (get-buffer johnson-worker--diagnostics-buffer-name)
         (kill-buffer johnson-worker--diagnostics-buffer-name)))))

(defun johnson-worker-test--feed (process frame)
  "Hand FRAME from PROCESS to the client filter and drain the decoder."
  (johnson-worker--process-filter process frame)
  (johnson-worker-test--drain))

(defun johnson-worker-test--cancel-decode-timer ()
  "Cancel and clear the armed decode timer, if any."
  (when (timerp johnson-worker--decode-timer)
    (cancel-timer johnson-worker--decode-timer))
  (setq johnson-worker--decode-timer nil))

(defun johnson-worker-test--peek-line ()
  "Return the first complete buffered line without consuming it."
  (with-current-buffer johnson-worker--receive-buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward "\n" nil t)
        (buffer-substring-no-properties (point-min) (1- (point)))))))

(defun johnson-worker-test--pump-frame ()
  "Decode buffered worker lines until one protocol frame is handled."
  (let (done)
    (while (not done)
      (should (johnson-test-support-wait-for
               #'johnson-worker--complete-line-buffered-p 10
               johnson-worker--process))
      (let ((line (johnson-worker-test--peek-line)))
        (johnson-worker-test--cancel-decode-timer)
        (johnson-worker--decode-next)
        (setq done (string-prefix-p johnson-protocol-prefix line))))))

(defun johnson-worker-test--wait-for-sentinel ()
  "Wait until the worker sentinel has cleared the client process."
  (should (johnson-test-support-wait-for
           (lambda () (null johnson-worker--process)) 10)))

;;;; Client lifecycle

(ert-deftest johnson-worker-test-start-is-nonblocking ()
  (johnson-worker-test--with-live-client
    (let* ((sent nil)
           (real-send (symbol-function 'johnson-worker--send)))
      (cl-letf (((symbol-function 'johnson-worker--send)
                 (lambda (message)
                   (push (cons johnson-worker--state
                               (plist-get message :type))
                         sent)
                   (funcall real-send message))))
        (johnson-worker-test--forbidding-blocking
          (johnson-worker-start #'johnson-worker-test--record-message))
        (should (eq johnson-worker--state 'starting))
        (should (johnson-worker-live-p))
        (should-not (johnson-worker-ready-p))
        (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                               johnson-worker--process))
        (should (equal (reverse sent) '((configuring . configure))))
        (johnson-worker-submit
         (johnson-worker-test--request-plist 1 0 '("slow:0.2:hello")))
        (should (johnson-test-support-wait-for
                 (lambda () (eq johnson-worker--state 'retrieving)) 10
                 johnson-worker--process))
        (should (johnson-test-support-wait-for
                 (lambda ()
                   (johnson-worker-test--core-messages-of-type
                    'dictionary-complete))
                 10 johnson-worker--process))
        (should (johnson-worker-ready-p))
        (should (equal (mapcar #'cdr (reverse sent)) '(configure request)))
        (should (equal (mapcar #'car (reverse sent))
                       '(configuring retrieving)))
        (let ((entry (car (johnson-worker-test--core-messages-of-type
                           'entry))))
          (should entry)
          (should (equal (plist-get entry :raw) "hello"))
          (should (equal (plist-get entry :context) '(:prepared t))))))))

(ert-deftest johnson-worker-test-submit-before-ready-is-queued ()
  (johnson-worker-test--with-live-client
    (let* ((sent nil)
           (real-send (symbol-function 'johnson-worker--send)))
      (cl-letf (((symbol-function 'johnson-worker--send)
                 (lambda (message)
                   (push (cons johnson-worker--state
                               (plist-get message :type))
                         sent)
                   (funcall real-send message))))
        (johnson-worker-start #'johnson-worker-test--record-message)
        (johnson-worker-submit
         (johnson-worker-test--request-plist 1 0 '("hello")))
        (should (eq johnson-worker--state 'starting))
        (should-not sent)
        (should (johnson-test-support-wait-for
                 (lambda ()
                   (johnson-worker-test--core-messages-of-type
                    'dictionary-complete))
                 10 johnson-worker--process))
        (should (equal (mapcar #'cdr (reverse sent)) '(configure request)))
        (should (equal (mapcar #'car (reverse sent))
                       '(configuring retrieving)))))))

(ert-deftest johnson-worker-test-configure-frame-includes-format-configs ()
  (let ((johnson--formats johnson--formats)
        (johnson-cache-directory "~/johnson-test-cache"))
    (johnson-register-format
     :name "config-fixture"
     :extensions nil
     :detect #'ignore
     :retrieve-entry #'ignore
     :worker-config (lambda () '(:enabled t))
     :apply-worker-config #'ignore)
    (let ((frame (johnson-worker--configure-frame)))
      (should (eq (nth 0 frame) :type))
      (should (eq (nth 1 frame) 'configure))
      (should (equal (plist-get frame :cache-directory)
                     (expand-file-name "~/johnson-test-cache")))
      (let ((entry (seq-find (lambda (fmt)
                               (equal (plist-get fmt :name) "config-fixture"))
                             (plist-get frame :formats))))
        (should entry)
        (should (equal (plist-get entry :config) '(:enabled t))))
      (should-not (seq-find (lambda (fmt)
                              (equal (plist-get fmt :name) "worker-fixture"))
                            (plist-get frame :formats))))))

;;;; Entry chunk assembly

(ert-deftest johnson-worker-test-chunks-assemble-into-single-entry ()
  (johnson-worker-test--with-retrieving-client process
    (let* ((raw (make-string 70000 ?x))
           (context '(:origin "assembly-test"))
           (frames (johnson-protocol-entry-frames
                    '(:lookup 1 :dictionary 0 :entry 0)
                    (list :raw raw :context context))))
      (should (= (length frames) 3))
      (johnson-worker-test--feed process (nth 0 frames))
      (should (= johnson-worker-test--messages-seen 0))
      (should (= (hash-table-count johnson-worker--entry-assemblies) 1))
      (johnson-worker-test--feed process (nth 1 frames))
      (should (= johnson-worker-test--messages-seen 0))
      (johnson-worker-test--feed process (nth 2 frames))
      (should (= johnson-worker-test--messages-seen 1))
      (should (= (hash-table-count johnson-worker--entry-assemblies) 0))
      (let ((entry (car (johnson-worker-test--core-messages-of-type 'entry))))
        (should (equal (plist-get entry :lookup) 1))
        (should (equal (plist-get entry :dictionary) 0))
        (should (equal (plist-get entry :entry) 0))
        (should (equal (plist-get entry :raw) raw))
        (should (equal (plist-get entry :context) context))))))

(ert-deftest johnson-worker-test-out-of-order-entry-fails-protocol ()
  (johnson-worker-test--with-retrieving-client process
    (let ((frames (johnson-protocol-entry-frames
                   '(:lookup 1 :dictionary 0 :entry 1)
                   '(:raw "wrong" :context nil))))
      (johnson-worker-test--feed process (car frames))
      (johnson-worker-test--check-failure process)
      (should-not (johnson-worker-test--core-messages-of-type 'entry))
      (should (= (hash-table-count johnson-worker--entry-assemblies) 0))
      (johnson-worker-test--wait-for-sentinel)
      (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))
      (should (= johnson-worker-test--messages-seen 1)))))

(ert-deftest johnson-worker-test-out-of-order-chunk-fails-protocol ()
  (johnson-worker-test--with-retrieving-client process
    (let ((frames (johnson-protocol-entry-frames
                   '(:lookup 1 :dictionary 0 :entry 0)
                   (list :raw (make-string 70000 ?x) :context nil))))
      (should (= (length frames) 3))
      (johnson-worker-test--feed process (nth 0 frames))
      (should (= (hash-table-count johnson-worker--entry-assemblies) 1))
      (johnson-worker-test--feed process (nth 2 frames))
      (johnson-worker-test--check-failure process)
      (should-not (johnson-worker-test--core-messages-of-type 'entry))
      (should (= (hash-table-count johnson-worker--entry-assemblies) 0))
      (johnson-worker-test--wait-for-sentinel)
      (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))
      (should (= johnson-worker-test--messages-seen 1)))))

(ert-deftest johnson-worker-test-mismatched-request-identity-fails-protocol ()
  (johnson-worker-test--with-retrieving-client process
    (let ((frames (johnson-protocol-entry-frames
                   '(:lookup 9 :dictionary 0 :entry 0)
                   '(:raw "other" :context nil))))
      (johnson-worker-test--feed process (car frames))
      (johnson-worker-test--check-failure process)
      (should-not (johnson-worker-test--core-messages-of-type 'entry)))))

(ert-deftest johnson-worker-test-stale-stream-violation-still-fails ()
  (johnson-worker-test--with-retrieving-client process
    (plist-put johnson-worker--active-request :stale t)
    (let ((frames (johnson-protocol-entry-frames
                   '(:lookup 1 :dictionary 0 :entry 1)
                   '(:raw "wrong" :context nil))))
      (johnson-worker-test--feed process (car frames))
      (johnson-worker-test--check-failure process))))

;;;; Supersession

(ert-deftest johnson-worker-test-supersession-discards-stale-lookup ()
  (johnson-worker-test--with-stepped-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (johnson-worker-test--pump-frame)
    (should (eq johnson-worker--state 'configuring))
    (johnson-worker-test--pump-frame)
    (should (johnson-worker-ready-p))
    (johnson-worker-submit
     (johnson-worker-test--request-plist 1 0 '("large:70000")))
    (should (eq johnson-worker--state 'retrieving))
    (johnson-worker-test--pump-frame)
    (johnson-worker-test--pump-frame)
    (should (= (hash-table-count johnson-worker--entry-assemblies) 1))
    (johnson-worker-submit
     (johnson-worker-test--request-plist 2 0 '("b-entry")))
    (should (plist-get johnson-worker--active-request :stale))
    (should (= (length johnson-worker--pending-requests) 1))
    (should (= (hash-table-count johnson-worker--entry-assemblies) 1))
    (johnson-worker-test--pump-frame)
    (should-not (eq johnson-worker--state 'failed))
    (should (= (hash-table-count johnson-worker--entry-assemblies) 1))
    (johnson-worker-test--pump-frame)
    (should-not (eq johnson-worker--state 'failed))
    (should-not (johnson-worker-test--core-messages-of-type 'entry))
    (should (equal (plist-get johnson-worker--active-request :lookup) 1))
    (johnson-worker-test--pump-frame)
    (should-not (eq johnson-worker--state 'failed))
    (should-not johnson-worker-test--protocol-errors)
    (should (eq johnson-worker--state 'retrieving))
    (should (equal (plist-get johnson-worker--active-request :lookup) 2))
    (should-not
     (seq-find (lambda (message) (equal (plist-get message :lookup) 1))
               (johnson-worker-test--core-messages-of-type
                'dictionary-complete)))
    (johnson-worker-test--pump-frame)
    (johnson-worker-test--pump-frame)
    (johnson-worker-test--pump-frame)
    (let ((entry (car (johnson-worker-test--core-messages-of-type 'entry))))
      (should entry)
      (should (equal (plist-get entry :lookup) 2))
      (should (equal (plist-get entry :raw) "b-entry")))
    (should (johnson-worker-ready-p))))

;;;; Stop, crash, and sentinel

(ert-deftest johnson-worker-test-stop-is-immediate-and-silent ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (let ((process johnson-worker--process))
      (johnson-worker-test--forbidding-blocking
        (johnson-worker-stop))
      (should (eq johnson-worker--state 'stopped))
      (should-not johnson-worker--process)
      (should-not (process-live-p process))
      (should-not (johnson-worker-live-p)))
    (johnson-test-support-wait-for #'ignore 0.3)
    (should (eq johnson-worker--state 'stopped))
    (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))))

(ert-deftest johnson-worker-test-crash-delivers-worker-exit ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (let ((process johnson-worker--process))
      (delete-process process)
      (should (johnson-test-support-wait-for
               (lambda () (eq johnson-worker--state 'failed)) 10)))
    (should-not johnson-worker--process)
    (let ((exits (johnson-worker-test--core-messages-of-type 'worker-exit)))
      (should (= (length exits) 1))
      (should (stringp (plist-get (car exits) :status)))
      (should (equal (plist-get (car exits) :diagnostics)
                     johnson-worker--diagnostics-buffer-name)))
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))))

(ert-deftest johnson-worker-test-late-sentinel-leaves-replacement-alone ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (let ((stale (make-process :name "johnson-worker-test-stale"
                               :command '("cat")
                               :connection-type 'pipe
                               :noquery t)))
      (delete-process stale)
      (johnson-worker--sentinel stale "killed\n"))
    (should (johnson-worker-ready-p))
    (should (johnson-worker-live-p))
    (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))))

(ert-deftest johnson-worker-test-shutdown-at-exit-sends-shutdown-when-ready ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (let ((process johnson-worker--process))
      (johnson-worker-test--forbidding-blocking
        (johnson-worker--shutdown-at-exit))
      (should (johnson-test-support-wait-for
               (lambda () (eq (process-status process) 'exit)) 10 process))
      (should (zerop (process-exit-status process)))
      (should (johnson-test-support-wait-for
               (lambda () (eq johnson-worker--state 'stopped)) 10))
      (should-not (johnson-worker-test--core-messages-of-type
                   'worker-exit)))))

(ert-deftest johnson-worker-test-start-failure-delivers-start-error ()
  (johnson-worker-test--with-live-client
    (let ((johnson-worker-command-function
           (lambda () (list "/nonexistent/johnson-worker-emacs"))))
      (johnson-worker-start #'johnson-worker-test--record-message))
    (should (eq johnson-worker--state 'failed))
    (should-not (johnson-worker-live-p))
    (let ((errors (johnson-worker-test--core-messages-of-type
                   'worker-start-error)))
      (should (= (length errors) 1))
      (should (string-match-p "/nonexistent/johnson-worker-emacs"
                              (plist-get (car errors) :message)))
      (should (equal (plist-get (car errors) :diagnostics)
                     johnson-worker--diagnostics-buffer-name)))
    (with-current-buffer (get-buffer johnson-worker--diagnostics-buffer-name)
      (should (string-match-p "/nonexistent/johnson-worker-emacs"
                              (buffer-string))))
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))))

(ert-deftest johnson-worker-test-shutdown-at-exit-hook-is-installed ()
  (should (memq #'johnson-worker--shutdown-at-exit kill-emacs-hook)))

(ert-deftest johnson-worker-test-shutdown-at-exit-stops-blocked-worker ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (johnson-worker-submit
     (johnson-worker-test--request-plist 1 0 '("slow:5:BLOCKED")))
    (should (johnson-test-support-wait-for
             (lambda () (eq johnson-worker--state 'retrieving)) 10
             johnson-worker--process))
    (let ((process johnson-worker--process))
      (johnson-worker-test--forbidding-blocking
        (johnson-worker--shutdown-at-exit))
      (should (eq johnson-worker--state 'stopped))
      (should-not johnson-worker--process)
      (should-not (process-live-p process)))
    (johnson-test-support-wait-for #'ignore 0.3)
    (should (eq johnson-worker--state 'stopped))
    (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))))

(ert-deftest johnson-worker-test-replacement-survives-stopped-blocked-sentinel ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                           johnson-worker--process))
    (johnson-worker-submit
     (johnson-worker-test--request-plist 1 0 '("slow:5:BLOCKED")))
    (should (johnson-test-support-wait-for
             (lambda () (eq johnson-worker--state 'retrieving)) 10
             johnson-worker--process))
    (let ((old-process johnson-worker--process))
      (johnson-worker-stop)
      (johnson-worker-start #'johnson-worker-test--record-message)
      (let ((new-process johnson-worker--process))
        (should-not (eq old-process new-process))
        (should (johnson-test-support-wait-for #'johnson-worker-ready-p 10
                                               new-process))
        ;; The old process's real sentinel has had every chance to fire
        ;; during the waits above; firing it again explicitly must also
        ;; leave the replacement untouched.
        (should-not (process-live-p old-process))
        (johnson-worker--sentinel old-process "killed\n")
        (should (johnson-worker-ready-p))
        (should (eq johnson-worker--process new-process))
        (should (process-live-p new-process))
        (should-not (equal (process-id old-process)
                           (process-id new-process)))
        (should-not (johnson-worker-test--core-messages-of-type
                     'worker-exit))))))

(ert-deftest johnson-worker-test-shutdown-at-exit-stops-unready-worker ()
  (johnson-worker-test--with-live-client
    (johnson-worker-start #'johnson-worker-test--record-message)
    (should (eq johnson-worker--state 'starting))
    (let ((process johnson-worker--process))
      (johnson-worker-test--forbidding-blocking
        (johnson-worker--shutdown-at-exit))
      (should (eq johnson-worker--state 'stopped))
      (should-not johnson-worker--process)
      (should-not (process-live-p process)))
    (johnson-test-support-wait-for #'ignore 0.3)
    (should (eq johnson-worker--state 'stopped))
    (should-not (johnson-worker-test--core-messages-of-type 'worker-exit))))

(provide 'johnson-worker-test)
;;; johnson-worker-test.el ends here
