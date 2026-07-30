;;; johnson-pty-test.el --- Real-terminal latency regression tests -*- lexical-binding: t; -*-

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

;; End-to-end latency regression over a real pseudo-terminal.  Each
;; test spawns an interactive `emacs -Q -nw' child (see
;; test/johnson-pty-child.el) on a PTY, waits for the results shell,
;; and sends literal key bytes while the retrieval worker and the
;; streaming renderer still have outstanding work.  For every key it
;; asserts the executed command, the expected point movement
;; direction, and that the child's own telemetry recorded outstanding
;; retrieval or render work at the moment the command completed.  The
;; maximum key-to-post-command latency must stay below
;; `johnson-pty-test-latency-ceiling'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)

(defconst johnson-pty-test-latency-ceiling 0.75
  "Maximum accepted key-to-post-command latency in seconds.")

(defconst johnson-pty-test--emacs
  (expand-file-name invocation-name invocation-directory)
  "Absolute path of the Emacs binary running these tests.")

;;;; Child lifecycle

(defun johnson-pty-test--run-scenario (scenario driver)
  "Spawn the PTY child for SCENARIO and call DRIVER with the test state.
DRIVER receives a state plist carrying `:process', `:telemetry', and
the mutable `:sent' key log.  The child process, its terminal buffer,
its telemetry file, and its cache directory are cleaned up even when
DRIVER fails."
  (let* ((telemetry (make-temp-file "johnson-pty-telemetry-"))
         (cache (make-temp-file "johnson-pty-cache-" t))
         (output (generate-new-buffer " *johnson-pty-terminal*"))
         (process-environment
          (append (list (format "JOHNSON_PTY_SCENARIO=%s" scenario)
                        (format "JOHNSON_PTY_TELEMETRY=%s" telemetry)
                        (format "JOHNSON_PTY_CACHE=%s" cache)
                        "TERM=xterm")
                  process-environment))
         (child (make-process
                 :name "johnson-pty-child"
                 :buffer output
                 :command (list johnson-pty-test--emacs "-Q" "-nw"
                                "-L" johnson-test-support-source-directory
                                "-L" johnson-test-support-directory
                                "-l" "johnson-pty-child")
                 :connection-type 'pty
                 :coding 'binary
                 :noquery t)))
    (set-process-window-size child 24 80)
    (unwind-protect
        (funcall driver (list :process child :telemetry telemetry :sent nil))
      (johnson-test-support-delete-process child)
      (kill-buffer output)
      (delete-file telemetry)
      (delete-directory cache t))))

;;;; Telemetry access

(defun johnson-pty-test--records (state)
  "Return every telemetry record the child of STATE has written so far."
  (let ((records nil))
    (with-temp-buffer
      (insert-file-contents (plist-get state :telemetry))
      (goto-char (point-min))
      (condition-case nil
          (while t
            (push (read (current-buffer)) records))
        (end-of-file nil)
        (invalid-read-syntax nil)))
    (nreverse records)))

(defun johnson-pty-test--command-records (state)
  "Return the child's command records, oldest first."
  (cl-remove-if-not (lambda (record) (plist-get record :command))
                    (johnson-pty-test--records state)))

(defun johnson-pty-test--marker (state name)
  "Return the `:marker' record NAME of STATE, or nil."
  (cl-find-if (lambda (record) (eq (plist-get record :marker) name))
              (johnson-pty-test--records state)))

(defun johnson-pty-test--latest-status (state)
  "Return the child's most recent `:status' record, or nil."
  (car (last (cl-remove-if-not (lambda (record) (plist-get record :status))
                               (johnson-pty-test--records state)))))

(defun johnson-pty-test--outstanding-p (record)
  "Return non-nil when RECORD shows outstanding retrieval or render work.
Outstanding work is a visible loading line, queued render units,
undone plan dictionaries, or a busy worker."
  (or (plist-get record :loading)
      (> (or (plist-get record :queue) 0) 0)
      (and (plist-get record :done) (plist-get record :total)
           (< (plist-get record :done) (plist-get record :total)))
      (memq (plist-get record :worker) '(starting configuring retrieving))))

(defun johnson-pty-test--wait-for (state predicate timeout what)
  "Wait until PREDICATE is non-nil, failing with context after TIMEOUT.
STATE is the scenario state and WHAT names the awaited condition in
the failure report.  Fail immediately when the child process dies."
  (let ((process (plist-get state :process)))
    (or (johnson-test-support-wait-for
         (lambda () (or (not (process-live-p process)) (funcall predicate)))
         timeout process)
        (johnson-pty-test--fail state what "timed out"))
    (unless (process-live-p process)
      (unless (funcall predicate)
        (johnson-pty-test--fail state what "child process died")))))

(defun johnson-pty-test--fail (state what why)
  "Fail the current test, reporting WHAT went wrong and WHY.
Attach the tail of STATE's telemetry records and of the child's
terminal output."
  (ert-fail
   (list what why
         :records (last (johnson-pty-test--records state) 10)
         :terminal
         (let ((buffer (process-buffer (plist-get state :process))))
           (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 800)) (point-max))))))))

;;;; Key driving

(defun johnson-pty-test--send-key (state byte command direction)
  "Send BYTE to the child of STATE and assert its command record.
COMMAND is the expected value of `this-command', DIRECTION is `down'
or `up' for the expected point movement.  Assert that the record shows
outstanding work overlapping the key, and return the key's latency in
seconds."
  (let* ((process (plist-get state :process))
         (index (length (plist-get state :sent)))
         (sent-at (float-time)))
    (plist-put state :sent (append (plist-get state :sent) (list byte)))
    (process-send-string process (string byte))
    (johnson-pty-test--wait-for
     state
     (lambda () (> (length (johnson-pty-test--command-records state)) index))
     10 (format "command record for key %d (%d)" index byte))
    (let ((record (nth index (johnson-pty-test--command-records state))))
      (should (equal (plist-get record :key) byte))
      (should (eq (plist-get record :command) command))
      (pcase direction
        ('down (should (> (plist-get record :after)
                          (plist-get record :before))))
        ('up (should (< (plist-get record :after)
                        (plist-get record :before)))))
      (should (johnson-pty-test--outstanding-p record))
      (- (plist-get record :completed-at) sent-at))))

(defun johnson-pty-test--await-shell (state)
  "Wait for the shell-visible marker of STATE and assert loading started."
  (johnson-pty-test--wait-for
   state (lambda () (johnson-pty-test--marker state 'shell-visible))
   60 "shell-visible marker")
  (let ((marker (johnson-pty-test--marker state 'shell-visible)))
    (should (plist-get marker :loading))
    (should (zerop (plist-get marker :sections)))
    (should (johnson-pty-test--outstanding-p marker))))

(defun johnson-pty-test--await-work (state sections)
  "Wait until SECTIONS sections are rendered with work still outstanding."
  (johnson-pty-test--wait-for
   state
   (lambda ()
     (let ((status (johnson-pty-test--latest-status state)))
       (and status
            (>= (plist-get status :sections) sections)
            (johnson-pty-test--outstanding-p status))))
   30 (format "%d sections with outstanding work" sections)))

(defun johnson-pty-test--line-movement-keys (state)
  "Send C-n and C-p to STATE's child, returning their latencies."
  (list (johnson-pty-test--send-key state ?\C-n 'next-line 'down)
        (johnson-pty-test--send-key state ?\C-p 'previous-line 'up)))

(defun johnson-pty-test--section-movement-keys (state)
  "Send the n/p section navigation series, returning the latencies.
The series is n n p n p, which ping-pongs across the first two
section headers and therefore always moves point."
  (list (johnson-pty-test--send-key state ?n 'johnson-next-section 'down)
        (johnson-pty-test--send-key state ?n 'johnson-next-section 'down)
        (johnson-pty-test--send-key state ?p 'johnson-prev-section 'up)
        (johnson-pty-test--send-key state ?n 'johnson-next-section 'down)
        (johnson-pty-test--send-key state ?p 'johnson-prev-section 'up)))

(defun johnson-pty-test--assert-latencies (latencies)
  "Assert that no latency in LATENCIES reaches the declared ceiling.
Report the measured distribution so test logs carry the evidence."
  (should latencies)
  (let* ((sorted (sort (copy-sequence latencies) #'<))
         (median (nth (/ (length sorted) 2) sorted)))
    (message "johnson-pty latency: n=%d max=%.3fs median=%.3fs"
             (length sorted) (car (last sorted)) median)
    (should (< (car (last sorted)) johnson-pty-test-latency-ceiling))))

(defun johnson-pty-test--drive-multi-section (scenario)
  "Run the SCENARIO whose plan renders at least two sections."
  (johnson-pty-test--run-scenario scenario
    (lambda (state)
      (johnson-pty-test--await-shell state)
      (let ((latencies (johnson-pty-test--line-movement-keys state)))
        (johnson-pty-test--await-work state 2)
        (setq latencies
              (append latencies
                      (johnson-pty-test--section-movement-keys state)))
        (johnson-pty-test--assert-latencies latencies)))))

;;;; Scenarios

(ert-deftest johnson-pty-test-slow-first ()
  "Movement keys respond while the first dictionary retrieval blocks."
  (johnson-pty-test--drive-multi-section 'slow-first))

(ert-deftest johnson-pty-test-fifty-dictionaries ()
  "Movement keys respond while fifty dictionaries stream in."
  (johnson-pty-test--drive-multi-section 'fifty-dictionaries))

(ert-deftest johnson-pty-test-fifty-matches ()
  "Movement keys respond while one dictionary streams fifty entries."
  (johnson-pty-test--run-scenario 'fifty-matches
    (lambda (state)
      (johnson-pty-test--await-shell state)
      (let ((latencies (johnson-pty-test--line-movement-keys state)))
        (johnson-pty-test--wait-for
         state
         (lambda ()
           (let ((status (johnson-pty-test--latest-status state)))
             (and status
                  (>= (plist-get status :sections) 1)
                  (> (plist-get status :queue) 0))))
         30 "one section with queued render units")
        (dotimes (_ 3)
          (setq latencies
                (append latencies
                        (johnson-pty-test--line-movement-keys state))))
        (johnson-pty-test--assert-latencies latencies)))))

(ert-deftest johnson-pty-test-frame-burst ()
  "Movement keys respond while oversized entries burst protocol frames."
  (johnson-pty-test--drive-multi-section 'frame-burst))

(provide 'johnson-pty-test)
;;; johnson-pty-test.el ends here
