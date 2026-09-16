;;; johnson-live-test.el --- Live cold-lookup acceptance over the real profile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Assisted-by: various LLMs (Claude, Codex)

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

;; Environment-gated live acceptance for cold real-dictionary lookups.
;; Every test skips unless JOHNSON_LIVE_ACCEPTANCE=1, because each
;; trial spawns an interactive `emacs -nw' child on a pseudo-terminal
;; running the user's REAL profile (--init-directory), the real
;; dictionary collection, and the PRODUCTION retrieval worker; see
;; test/johnson-live-child.el for the child side.
;;
;; Three high/control term pairs each run two fresh children.  For
;; every child the test asserts the term's dictionary count from the
;; child's own plan record before any latency gate, proves the results
;; shell was visible before the first entry, sends movement keys during
;; startup/configuration/first retrieval (and, for high trials, after
;; two sections with work still outstanding), and verifies the settled
;; buffer holds every expected section once, in priority order, with no
;; loading or error text.  The control trial's maximum key latency must
;; stay at or below 600 ms; the high trial's maximum must stay within
;; 150 ms of the control maximum and at or below 750 ms.  A separate
;; stale trial supersedes an in-flight lookup and verifies no text or
;; overlays of the superseded lookup remain.  Every child must exit
;; normally, and both the child PID and every worker PID it reported
;; must be gone afterwards.

;;; Code:

(require 'ert)
(require 'cl-lib)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)

(defconst johnson-live-test-pairs
  '(("ACROSTIC" . "ABECEGRAMA")
    ("ALBACORE" . "ACADIOS")
    ("AMANUENSIS" . "ABERGEAGE"))
  "High/control term pairs; car is the high term, cdr the control term.")

(defconst johnson-live-test-control-ceiling 0.600
  "Maximum accepted key latency, in seconds, for a control trial.")

(defconst johnson-live-test-high-margin 0.150
  "Maximum accepted excess, in seconds, of the high over the control maximum.")

(defconst johnson-live-test-high-ceiling 0.750
  "Absolute maximum accepted key latency, in seconds, for a high trial.")

(defconst johnson-live-test-profile-directory
  "/Users/pablostafforini/.config/emacs-profiles/8.3.0-dev"
  "Real profile directory the child Emacs runs via --init-directory.")

(defconst johnson-live-test--emacs
  (expand-file-name invocation-name invocation-directory)
  "Absolute path of the Emacs binary running these tests.")

(defun johnson-live-test--enabled-p ()
  "Return non-nil when the live acceptance suite is enabled."
  (equal (getenv "JOHNSON_LIVE_ACCEPTANCE") "1"))

;;;; Child lifecycle

(defun johnson-live-test--run-scenario (scenario term driver)
  "Spawn the live child for SCENARIO on TERM and call DRIVER with the state.
DRIVER receives a state plist carrying `:process', `:telemetry', and
the mutable `:sent' key log.  The child runs the user's real profile
and the production worker command; the child process, its terminal
buffer, its telemetry file, and its exit sentinel are cleaned up even
when DRIVER fails."
  (let* ((telemetry-file (make-temp-file "johnson-live-telemetry-"))
         (output (generate-new-buffer " *johnson-live-terminal*"))
         (process-environment (cons "TERM=xterm" process-environment))
         (child (make-process
                 :name "johnson-live-child"
                 :buffer output
                 :command
                 (list johnson-live-test--emacs
                       (concat "--init-directory="
                               johnson-live-test-profile-directory)
                       "-nw"
                       "-L" johnson-test-support-source-directory
                       "-L" johnson-test-support-directory
                       "-l" (expand-file-name "johnson-live-child.el"
                                              johnson-test-support-directory)
                       "--eval"
                       (prin1-to-string
                        `(johnson-live-child-run ',scenario ,term
                                                 ,telemetry-file)))
                 :connection-type 'pty
                 :coding 'binary
                 :noquery t)))
    (set-process-window-size child 24 80)
    (unwind-protect
        (funcall driver (list :process child :telemetry telemetry-file
                              :sent nil :records nil :bytes 0))
      (johnson-test-support-delete-process child)
      (kill-buffer output)
      (delete-file telemetry-file)
      (dolist (sentinel (list (concat telemetry-file ".exit")
                              (concat telemetry-file ".go")))
        (when (file-exists-p sentinel)
          (delete-file sentinel))))))

;;;; Telemetry access

(defun johnson-live-test--records (state)
  "Return every telemetry record the child of STATE has written so far.
Already parsed records are cached in STATE under `:records', with the
number of consumed bytes under `:bytes', so each poll reads and parses
only the complete lines appended since the previous poll instead of
re-reading the whole file."
  (let* ((file (plist-get state :telemetry))
         (start (or (plist-get state :bytes) 0))
         (size (or (file-attribute-size (file-attributes file)) 0)))
    (when (> size start)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally file nil start size)
        (goto-char (point-max))
        (when (search-backward "\n" nil t)
          (let ((tail (buffer-substring (point-min) (1+ (point)))))
            (plist-put state :bytes (+ start (length tail)))
            (plist-put state :records
                       (nconc (plist-get state :records)
                              (johnson-live-test--parse-records
                               (decode-coding-string tail 'utf-8))))))))
    (plist-get state :records)))

(defun johnson-live-test--parse-records (text)
  "Return the telemetry records printed in TEXT, oldest first."
  (let ((records nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (push (read (current-buffer)) records))
        (end-of-file nil)
        (invalid-read-syntax nil)))
    (nreverse records)))

(defun johnson-live-test--command-records (state)
  "Return the child's command records, oldest first."
  (cl-remove-if-not (lambda (record) (plist-get record :command))
                    (johnson-live-test--records state)))

(defun johnson-live-test--marker (state name)
  "Return the first `:marker' record NAME of STATE, or nil."
  (cl-find-if (lambda (record) (eq (plist-get record :marker) name))
              (johnson-live-test--records state)))

(defun johnson-live-test--latest-status (state)
  "Return the child's most recent `:status' record, or nil."
  (car (last (cl-remove-if-not (lambda (record) (plist-get record :status))
                               (johnson-live-test--records state)))))

(defun johnson-live-test--outstanding-p (record)
  "Return non-nil when RECORD shows outstanding retrieval or render work.
Outstanding work is a visible loading line, queued render units,
undone plan dictionaries, or a busy worker."
  (or (plist-get record :loading)
      (> (or (plist-get record :queue) 0) 0)
      (and (plist-get record :done) (plist-get record :total)
           (< (plist-get record :done) (plist-get record :total)))
      (memq (plist-get record :worker) '(starting configuring retrieving))))

(defun johnson-live-test--worker-pids (state)
  "Return every distinct worker PID the child of STATE reported."
  (let (pids)
    (dolist (record (johnson-live-test--records state))
      (when-let* ((pid (plist-get record :worker-pid)))
        (unless (member pid pids)
          (push pid pids))))
    (nreverse pids)))

(defun johnson-live-test--wait-for (state predicate timeout what)
  "Wait until PREDICATE is non-nil, failing with context after TIMEOUT.
STATE is the scenario state and WHAT names the awaited condition in
the failure report.  Fail immediately when the child process dies."
  (let ((process (plist-get state :process)))
    (or (johnson-test-support-wait-for
         (lambda () (or (not (process-live-p process)) (funcall predicate)))
         timeout process)
        (johnson-live-test--fail state what "timed out"))
    (unless (process-live-p process)
      (unless (funcall predicate)
        (johnson-live-test--fail state what "child process died")))))

(defun johnson-live-test--fail (state what why)
  "Fail the current test, reporting WHAT went wrong and WHY.
Attach the tail of STATE's telemetry records and of the child's
terminal output."
  (ert-fail
   (list what why
         :records (last (johnson-live-test--records state) 10)
         :terminal
         (let ((buffer (process-buffer (plist-get state :process))))
           (and (buffer-live-p buffer)
                (with-current-buffer buffer
                  (buffer-substring-no-properties
                   (max (point-min) (- (point-max) 800)) (point-max))))))))

;;;; Key driving

(defun johnson-live-test--send-key (state keys event command direction)
  "Send the KEYS string to the child of STATE and assert its command record.
KEYS is the literal byte sequence written to the pseudo-terminal, for
example \"n\" or the arrow escape sequence \"\\e[B\".  EVENT is the
`last-command-event' the child records once the terminal has decoded
KEYS: the character for a plain key, or a symbol such as `down' for a
decoded arrow key.  COMMAND is the expected value of `this-command',
DIRECTION is `down' or `up' for the expected point movement.  Assert
that the record shows outstanding work overlapping the key.  Return a
plist carrying the record under `:record' and the key-to-post-command
latency in seconds under `:latency'."
  (let* ((process (plist-get state :process))
         (index (length (plist-get state :sent)))
         (sent-at (float-time)))
    (plist-put state :sent (append (plist-get state :sent) (list keys)))
    (process-send-string process keys)
    (johnson-live-test--wait-for
     state
     (lambda () (> (length (johnson-live-test--command-records state)) index))
     30 (format "command record for key %d (%S)" index keys))
    (let ((record (nth index (johnson-live-test--command-records state))))
      (should (equal (plist-get record :key) event))
      (should (eq (plist-get record :command) command))
      (pcase direction
        ('down (should (> (plist-get record :after)
                          (plist-get record :before))))
        ('up (should (< (plist-get record :after)
                        (plist-get record :before)))))
      (should (johnson-live-test--outstanding-p record))
      (list :record record
            :latency (- (plist-get record :completed-at) sent-at)))))

(defun johnson-live-test--startup-keys (state)
  "Send repeated down/up arrows while the first retrieval is outstanding.
The arrow escape sequences are used instead of C-n/C-p because the
real profile rebinds C-n globally; the decoded arrows still run
`next-line' and `previous-line'.  The first key's record must show the
worker still starting, configuring, or retrieving, proving the keys
overlapped the startup/configuration/first-retrieval phase.  Return
the latencies."
  (let ((keys (list (johnson-live-test--send-key state "\e[B" 'down
                                                 'next-line 'down)
                    (johnson-live-test--send-key state "\e[A" 'up
                                                 'previous-line 'up)
                    (johnson-live-test--send-key state "\e[B" 'down
                                                 'next-line 'down)
                    (johnson-live-test--send-key state "\e[A" 'up
                                                 'previous-line 'up))))
    (should (memq (plist-get (plist-get (car keys) :record) :worker)
                  '(starting configuring retrieving)))
    (mapcar (lambda (key) (plist-get key :latency)) keys)))

(defun johnson-live-test--midstream-keys (state)
  "Send down/up arrows and the n/p section series while work remains.
The n n p n p series ping-pongs across the first two section headers,
so every key moves point.  Return the latencies."
  (mapcar
   (lambda (key) (plist-get key :latency))
   (list (johnson-live-test--send-key state "\e[B" 'down 'next-line 'down)
         (johnson-live-test--send-key state "\e[A" 'up 'previous-line 'up)
         (johnson-live-test--send-key state "n" ?n 'johnson-next-section
                                      'down)
         (johnson-live-test--send-key state "n" ?n 'johnson-next-section
                                      'down)
         (johnson-live-test--send-key state "p" ?p 'johnson-prev-section 'up)
         (johnson-live-test--send-key state "n" ?n 'johnson-next-section
                                      'down)
         (johnson-live-test--send-key state "p" ?p 'johnson-prev-section
                                      'up))))

;;;; Trial phases

(defun johnson-live-test--report-worker-source (state)
  "Surface the worker-source record of STATE, failing the trial if absent.
The child records the loaded johnson-worker file and its sha256 hash;
this reports both to the test log before the telemetry file is deleted,
so the controller can compare them against the built artifact."
  (johnson-live-test--wait-for
   state (lambda () (johnson-live-test--marker state 'worker-source))
   300 "worker-source record")
  (let ((record (johnson-live-test--marker state 'worker-source)))
    (should (plist-get record :file))
    (should (plist-get record :hash))
    (message "JOHNSON-LIVE worker-source: %s sha256 %s"
             (plist-get record :file) (plist-get record :hash))))

(defun johnson-live-test--assert-dictionary-count (state min max)
  "Wait for the plan record of STATE and assert its local match count.
The count of local dictionaries containing the term must lie between
MIN and MAX inclusive, asserted immediately before the trial's gates
rather than assumed from previously recorded counts."
  (johnson-live-test--wait-for
   state (lambda () (johnson-live-test--marker state 'plan))
   300 "plan record")
  (should (<= min
              (plist-get (johnson-live-test--marker state 'plan) :local)
              max)))

(defun johnson-live-test--await-shell (state)
  "Wait for the shell-visible marker of STATE and assert loading started.
The marker must show zero rendered sections, proving the results shell
was visible before the first entry."
  (johnson-live-test--wait-for
   state (lambda () (johnson-live-test--marker state 'shell-visible))
   300 "shell-visible marker")
  (let ((marker (johnson-live-test--marker state 'shell-visible)))
    (should (plist-get marker :loading))
    (should (zerop (plist-get marker :sections)))
    (should (johnson-live-test--outstanding-p marker))))

(defun johnson-live-test--await-work (state sections)
  "Wait until SECTIONS sections are rendered with work still outstanding."
  (johnson-live-test--wait-for
   state
   (lambda ()
     (let ((status (johnson-live-test--latest-status state)))
       (and status
            (>= (plist-get status :sections) sections)
            (johnson-live-test--outstanding-p status))))
   300 (format "%d sections with outstanding work" sections)))

(defun johnson-live-test--await-complete (state timeout)
  "Wait at most TIMEOUT seconds for the terminal record of STATE."
  (johnson-live-test--wait-for
   state (lambda () (johnson-live-test--marker state 'complete))
   timeout "complete record"))

(defun johnson-live-test--assert-complete (state)
  "Assert the settled buffer holds every expected section once, in order.
The expected sections are the local dictionary names from the plan
record of STATE, in priority order.  Remote sections may interleave,
so the rendered section list is filtered to the expected names before
the order comparison.  No section may render twice and no loading or
error text may remain."
  (let* ((expected (plist-get (johnson-live-test--marker state 'plan) :names))
         (complete (johnson-live-test--marker state 'complete))
         (names (plist-get complete :names)))
    (should complete)
    (should (equal (cl-remove-if-not (lambda (name) (member name expected))
                                     names)
                   expected))
    (should (equal names (delete-dups (copy-sequence names))))
    (should-not (plist-get complete :loading-text))
    (should-not (plist-get complete :error-text))))

(defun johnson-live-test--await-exit (state)
  "Ask the child of STATE to exit and verify both PIDs are gone.
Write the exit sentinel the child polls for, require a normal exit
with status zero, and then require that the child PID and every worker
PID the child reported have disappeared from the process table."
  (let* ((process (plist-get state :process))
         (child-pid (process-id process))
         (worker-pids (johnson-live-test--worker-pids state)))
    (should worker-pids)
    (with-temp-file (concat (plist-get state :telemetry) ".exit"))
    (johnson-live-test--wait-for
     state (lambda () (not (process-live-p process)))
     60 "child exit")
    (should (eq (process-status process) 'exit))
    (should (zerop (process-exit-status process)))
    (should (johnson-live-test--pid-exits-p child-pid))
    (dolist (pid worker-pids)
      (should (johnson-live-test--pid-exits-p pid)))))

(defun johnson-live-test--pid-exits-p (pid)
  "Poll until PID leaves the process table, bounded at 30 seconds.
Return non-nil when the PID disappeared."
  (johnson-test-support-wait-for
   (lambda () (null (process-attributes pid))) 30))

(defun johnson-live-test--report-latencies (term latencies)
  "Report the latency distribution for TERM and return the maximum."
  (should latencies)
  (let* ((sorted (sort (copy-sequence latencies) #'<))
         (maximum (car (last sorted))))
    (message "johnson-live %s: n=%d max=%.3fs median=%.3fs"
             term (length sorted) maximum
             (nth (/ (length sorted) 2) sorted))
    maximum))

;;;; Trials

(defun johnson-live-test--control-trial (term)
  "Run the cold control trial for TERM and return its maximum key latency."
  (johnson-live-test--run-scenario 'control term
    (lambda (state)
      (johnson-live-test--report-worker-source state)
      (johnson-live-test--assert-dictionary-count state 1 1)
      (johnson-live-test--await-shell state)
      (let ((latencies (johnson-live-test--startup-keys state)))
        (johnson-live-test--await-complete state 600)
        (johnson-live-test--assert-complete state)
        (johnson-live-test--await-exit state)
        (johnson-live-test--report-latencies term latencies)))))

(defun johnson-live-test--high-trial (term)
  "Run the cold high-coverage trial for TERM; return its maximum latency."
  (johnson-live-test--run-scenario 'high term
    (lambda (state)
      (johnson-live-test--report-worker-source state)
      (johnson-live-test--assert-dictionary-count state 48 52)
      (johnson-live-test--await-shell state)
      (let ((latencies (johnson-live-test--startup-keys state)))
        (johnson-live-test--await-work state 2)
        (setq latencies (append latencies
                                (johnson-live-test--midstream-keys state)))
        (johnson-live-test--await-complete state 900)
        (johnson-live-test--assert-complete state)
        (johnson-live-test--await-exit state)
        (johnson-live-test--report-latencies term latencies)))))

(defun johnson-live-test--run-pair (pair)
  "Run the control and high trials of PAIR and assert the latency gates.
PAIR is a (HIGH . CONTROL) cons of terms.  The control maximum must
stay at or below `johnson-live-test-control-ceiling'; the high maximum
must stay within `johnson-live-test-high-margin' of the control
maximum and at or below `johnson-live-test-high-ceiling'."
  (let* ((control-max (johnson-live-test--control-trial (cdr pair)))
         (high-max (johnson-live-test--high-trial (car pair))))
    (message "johnson-live pair %s/%s: control-max=%.3fs high-max=%.3fs"
             (car pair) (cdr pair) control-max high-max)
    (should (<= control-max johnson-live-test-control-ceiling))
    (should (<= high-max (+ control-max johnson-live-test-high-margin)))
    (should (<= high-max johnson-live-test-high-ceiling))))

;;;; Scenarios

(ert-deftest johnson-live-test-acrostic-pair ()
  "Cold ACROSTIC/ABECEGRAMA lookups stream without blocking input."
  (skip-unless (johnson-live-test--enabled-p))
  (johnson-live-test--run-pair (nth 0 johnson-live-test-pairs)))

(ert-deftest johnson-live-test-albacore-pair ()
  "Cold ALBACORE/ACADIOS lookups stream without blocking input."
  (skip-unless (johnson-live-test--enabled-p))
  (johnson-live-test--run-pair (nth 1 johnson-live-test-pairs)))

(ert-deftest johnson-live-test-amanuensis-pair ()
  "Cold AMANUENSIS/ABERGEAGE lookups stream without blocking input."
  (skip-unless (johnson-live-test--enabled-p))
  (johnson-live-test--run-pair (nth 2 johnson-live-test-pairs)))

(ert-deftest johnson-live-test-stale-supersession ()
  "A superseding lookup leaves no text or overlays of the stale lookup.
The midstream key series runs against the first lookup's stream, and
only then is the go sentinel written that lets the child start the
superseding lookup, so the keys can never race the buffer reset."
  (skip-unless (johnson-live-test--enabled-p))
  (johnson-live-test--run-scenario 'stale "ACROSTIC"
    (lambda (state)
      (johnson-live-test--report-worker-source state)
      (johnson-live-test--await-shell state)
      (johnson-live-test--startup-keys state)
      (johnson-live-test--await-work state 2)
      (johnson-live-test--midstream-keys state)
      (with-temp-file (concat (plist-get state :telemetry) ".go"))
      (johnson-live-test--wait-for
       state (lambda () (johnson-live-test--marker state 'second-lookup))
       600 "second-lookup record")
      (should (johnson-live-test--outstanding-p
               (johnson-live-test--marker state 'second-lookup)))
      (johnson-live-test--await-complete state 600)
      (let ((complete (johnson-live-test--marker state 'complete)))
        (should (equal (plist-get complete :word) "ABECEGRAMA"))
        (should-not (plist-get complete :stale-headers))
        (should-not (plist-get complete :stale-overlays))
        (should-not (plist-get complete :stale-loading))
        (should (zerop (plist-get complete :stale-occurrences)))
        (should-not (plist-get complete :loading-text))
        (should-not (plist-get complete :error-text)))
      (johnson-live-test--await-exit state))))

(provide 'johnson-live-test)
;;; johnson-live-test.el ends here
