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
;; and assert the wire-level message sequences and exit codes.

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

(defun johnson-worker-test--record-message (message)
  "Record one delivered MESSAGE in the test counters."
  (setq johnson-worker-test--messages-seen
        (1+ johnson-worker-test--messages-seen))
  (when (eq (plist-get message :type) 'protocol-error)
    (push message johnson-worker-test--protocol-errors)))

(defmacro johnson-worker-test--with-client (process &rest body)
  "Run BODY with fresh worker client state and a live pipe process.
PROCESS is bound to a `cat' child whose filter is the real
`johnson-worker--process-filter'.  The decode delay is bound high so
armed timers never fire on their own; tests step the decoder manually.
All processes, buffers, and timers are cleaned up even on failure."
  (declare (indent 1))
  `(let* ((johnson-worker-test--messages-seen 0)
          (johnson-worker-test--protocol-errors nil)
          (johnson-worker--decode-delay 60)
          (johnson-worker--receive-buffer
           (generate-new-buffer " *johnson-worker-test-receive*"))
          (johnson-worker--decode-timer nil)
          (johnson-worker--state 'stopped)
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
   (list :type 'request
         :lookup lookup
         :dictionary dictionary
         :format "worker-fixture"
         :path "/fixture/dictionary"
         :name "Worker Fixture"
         :word "fixture"
         :matches (mapcar (lambda (offset)
                            (list :word "fixture" :offset offset :length 0))
                          offsets))))

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

(provide 'johnson-worker-test)
;;; johnson-worker-test.el ends here
