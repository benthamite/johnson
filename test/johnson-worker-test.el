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
;; pipes, not mocks.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'johnson-protocol)
(require 'johnson-worker)

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

(provide 'johnson-worker-test)
;;; johnson-worker-test.el ends here
