;;; johnson-live-child.el --- Telemetry child for the live acceptance tests -*- lexical-binding: t; -*-

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

;; This file is loaded by test/johnson-live-test.el into a disposable
;; interactive `emacs -nw' child that runs the user's REAL profile via
;; --init-directory, so the real configuration, the real dictionary
;; collection, and the PRODUCTION retrieval worker command are all
;; active; no test fixture replaces any production path.  The outer
;; test then calls `johnson-live-child-run' via --eval with the
;; scenario symbol, the looked-up term, and the telemetry file path.
;;
;; Telemetry is appended to the telemetry file as single-line records,
;; mirroring test/johnson-pty-child.el: one command record per executed
;; command from a buffer-local `post-command-hook' in the results
;; buffer, `:marker' lifecycle records, and periodic `:status' records.
;; Additional live-only records carry the loaded worker source file and
;; its hash, the lookup plan's dictionary counts (asserted by the outer
;; test immediately before each trial), and a terminal `complete'
;; record describing the settled results buffer.
;;
;; The `stale' scenario starts a second, superseding lookup of the
;; term's partner word once the first lookup has rendered two sections
;; with work still outstanding and the outer test has written the go
;; sentinel file next to the telemetry file, and its terminal record
;; proves that no text or overlays of the superseded lookup remain.
;; The child exits normally, with status zero, when the outer test
;; writes the exit sentinel file next to the telemetry file.

;;; Code:

(require 'cl-lib)
(require 'johnson)
(require 'johnson-worker)

(defconst johnson-live-child--stale-partners
  '(("ACROSTIC" . "ABECEGRAMA")
    ("ALBACORE" . "ACADIOS")
    ("AMANUENSIS" . "ABERGEAGE"))
  "Alist mapping each stale-scenario first term to its superseding term.")

(defvar johnson-live-child--telemetry-file nil
  "File this child appends single-line telemetry records to.")

(defvar johnson-live-child--scenario nil
  "Scenario symbol this child is running: `control', `high', or `stale'.")

(defvar johnson-live-child--term nil
  "Term of the child's first lookup.")

(defvar johnson-live-child--phase 'first
  "Lookup phase: `first' until the stale scenario starts its second lookup.")

(defvar johnson-live-child--first-names nil
  "Dictionary names of every descriptor in the first lookup's plan.")

(defvar johnson-live-child--second-names nil
  "Dictionary names of every descriptor in the second lookup's plan.")

(defvar johnson-live-child--complete-emitted nil
  "Non-nil once the terminal `complete' record has been written.")

(defvar johnson-live-child--point-before nil
  "Value of point in the results buffer before the current command.")

(defvar johnson-live-child--status-timer nil
  "Repeating timer emitting `:status' telemetry records.")

(defun johnson-live-child-run (scenario term telemetry-file)
  "Run live acceptance SCENARIO for TERM over the real profile.
SCENARIO is one of the symbols `control', `high', and `stale'.  TERM is
the looked-up word; for the `stale' scenario it is the first word of
the A-to-B pair and the superseding word comes from
`johnson-live-child--stale-partners'.  Single-line telemetry records
are appended to TELEMETRY-FILE.  The production worker command is left
untouched, so retrieval runs in the real persistent child Emacs over
the user's real dictionaries."
  (setq debug-on-error nil)
  (setq johnson-live-child--telemetry-file telemetry-file)
  (setq johnson-live-child--scenario scenario)
  (setq johnson-live-child--term term)
  (setq johnson-live-child--phase 'first)
  (setq johnson-history-persist nil)
  (setq johnson--history-log-loaded t)
  (johnson-live-child--emit (list :marker 'starting :at (float-time)))
  (johnson-live-child--emit-worker-source)
  (johnson--ensure-dictionaries)
  (johnson-live-child--require-fresh-indexes)
  (johnson--ensure-indexed)
  (let ((plan (johnson-live-child--emit-plan term)))
    (setq johnson-live-child--first-names
          (johnson-live-child--plan-names plan))
    (johnson--display-lookup term plan))
  (johnson-live-child--instrument))

(defun johnson-live-child--emit-worker-source ()
  "Record which johnson-worker file is loaded and its content hash."
  (let ((file johnson-worker--source-file))
    (johnson-live-child--emit
     (list :marker 'worker-source :at (float-time)
           :file file
           :hash (and file (file-exists-p file)
                      (with-temp-buffer
                        (insert-file-contents-literally file)
                        (secure-hash 'sha256 (current-buffer))))))))

(defun johnson-live-child--require-fresh-indexes ()
  "Exit with a nonzero status when any dictionary index is stale.
`johnson--ensure-indexed' would prompt interactively about stale
indexes, which would hang the unattended child, so the child records a
`stale-index' marker and refuses to run instead."
  (let ((stale (cl-remove-if-not #'johnson-live-child--stale-dictionary-p
                                 johnson--dictionaries)))
    (when stale
      (johnson-live-child--emit
       (list :marker 'stale-index :at (float-time) :count (length stale)))
      (kill-emacs 1))))

(defun johnson-live-child--stale-dictionary-p (dict)
  "Return non-nil when DICT uses a file-based index that is stale."
  (let ((format (johnson--get-format (plist-get dict :format-name))))
    (and (not (plist-get format :skip-index))
         (johnson-db-stale-quick-p (plist-get dict :path)))))

(defun johnson-live-child--emit-plan (word)
  "Emit the plan record for WORD and return the computed plan.
The record carries the number of local dictionaries containing WORD,
the total plan length, and the local dictionary names in priority
order, so the outer test can assert dictionary counts immediately
before the trial's gates instead of assuming recorded counts."
  (let ((plan (johnson--lookup-plan word (johnson--dictionaries-by-priority))))
    (johnson-live-child--emit
     (list :marker 'plan :at (float-time) :word word
           :local (johnson--plan-local-count plan)
           :total (length plan)
           :names (johnson-live-child--indexed-names plan)))
    plan))

(defun johnson-live-child--indexed-names (plan)
  "Return the names of PLAN's indexed dictionaries, in plan order."
  (cl-loop for item in plan
           when (eq (plist-get item :kind) 'indexed)
           collect (plist-get (plist-get item :dict) :name)))

(defun johnson-live-child--plan-names (plan)
  "Return the names of every dictionary in PLAN, in plan order."
  (mapcar (lambda (item) (plist-get (plist-get item :dict) :name)) plan))

(defun johnson-live-child--instrument ()
  "Attach telemetry hooks to the results buffer and start the status timer."
  (with-current-buffer "*johnson*"
    (add-hook 'pre-command-hook #'johnson-live-child--note-point nil t)
    (add-hook 'post-command-hook #'johnson-live-child--record-command nil t)
    (setq johnson-live-child--status-timer
          (run-at-time 0.05 0.05 #'johnson-live-child--tick))
    (johnson-live-child--emit
     (append (list :marker 'shell-visible :at (float-time))
             (johnson-live-child--state-fields)))))

(defun johnson-live-child--note-point ()
  "Record point in the results buffer before the current command."
  (setq johnson-live-child--point-before (point)))

(defun johnson-live-child--record-command ()
  "Append the telemetry record of the command that just completed."
  (johnson-live-child--emit
   (append (list :key last-command-event
                 :command this-command
                 :before johnson-live-child--point-before
                 :after (point)
                 :section (get-text-property (point) 'johnson-section-header)
                 :completed-at (float-time))
           (johnson-live-child--state-fields))))

(defun johnson-live-child--tick ()
  "Emit a status record and advance the scenario state machine."
  (johnson-live-child--check-exit)
  (when-let* ((buffer (get-buffer "*johnson*")))
    (with-current-buffer buffer
      (johnson-live-child--emit
       (append (list :status t :at (float-time))
               (johnson-live-child--state-fields)))
      (johnson-live-child--maybe-start-second-lookup)
      (johnson-live-child--maybe-emit-complete))))

(defun johnson-live-child--check-exit ()
  "Exit Emacs normally once the outer test writes the exit sentinel."
  (when (file-exists-p (concat johnson-live-child--telemetry-file ".exit"))
    (johnson-live-child--emit (list :marker 'exiting :at (float-time)))
    (kill-emacs 0)))

(defun johnson-live-child--maybe-start-second-lookup ()
  "Start the superseding lookup once the first has visible progress.
Only the `stale' scenario has a second lookup.  It starts after two
sections have rendered while retrieval or render work is still
outstanding, so the first lookup is genuinely superseded mid-stream,
and only once the outer test has written the go sentinel, so the
outer test's midstream keys can never race the buffer reset.  The
`second-lookup' marker is emitted before the new lookup starts and
therefore records the outstanding work being superseded."
  (when (and (eq johnson-live-child--scenario 'stale)
             (eq johnson-live-child--phase 'first)
             (file-exists-p (concat johnson-live-child--telemetry-file ".go"))
             (>= (length (johnson-live-child--section-names)) 2)
             (johnson-live-child--outstanding-p))
    (johnson-live-child--emit
     (append (list :marker 'second-lookup :at (float-time))
             (johnson-live-child--state-fields)))
    (setq johnson-live-child--phase 'second)
    (let* ((partner (or (cdr (assoc johnson-live-child--term
                                    johnson-live-child--stale-partners))
                        (progn (johnson-live-child--emit
                                (list :marker 'missing-partner
                                      :word johnson-live-child--term))
                               (kill-emacs 1))))
           (plan (johnson-live-child--emit-plan partner)))
      (setq johnson-live-child--second-names
            (johnson-live-child--plan-names plan))
      (johnson--display-lookup partner plan))))

(defun johnson-live-child--maybe-emit-complete ()
  "Emit the terminal `complete' record once the final lookup settles.
For the `stale' scenario the record is only written after the second,
superseding lookup has completed."
  (when (and (not johnson-live-child--complete-emitted)
             (johnson-live-child--complete-p)
             (or (not (eq johnson-live-child--scenario 'stale))
                 (eq johnson-live-child--phase 'second)))
    (setq johnson-live-child--complete-emitted t)
    (johnson-live-child--emit (johnson-live-child--complete-record))))

(defun johnson-live-child--complete-p ()
  "Return non-nil when the current lookup has fully rendered."
  (let ((state johnson--section-state))
    (and state
         (>= (plist-get state :done) (plist-get state :total))
         (null johnson--render-queue)
         (not (and (markerp johnson--loading-marker)
                   (marker-position johnson--loading-marker))))))

(defun johnson-live-child--complete-record ()
  "Return the terminal record describing the settled results buffer."
  (append (list :marker 'complete :at (float-time)
                :word johnson--current-word
                :names (johnson-live-child--section-names)
                :loading-text (johnson-live-child--loading-text-p
                               johnson--current-word)
                :error-text (johnson-live-child--error-text))
          (when (eq johnson-live-child--scenario 'stale)
            (johnson-live-child--stale-fields))
          (johnson-live-child--state-fields)))

(defun johnson-live-child--stale-fields ()
  "Return the fields proving no first-lookup text or overlays remain.
Stale names are the dictionaries of the first plan that are absent
from the second plan, so any of their section headers or overlays in
the settled buffer must come from the superseded lookup."
  (let ((stale-names (cl-set-difference
                      johnson-live-child--first-names
                      (cons "Contents" johnson-live-child--second-names)
                      :test #'equal)))
    (list :stale-headers (cl-intersection
                          stale-names (johnson-live-child--section-names)
                          :test #'equal)
          :stale-overlays (cl-intersection
                           stale-names (johnson-live-child--overlay-names)
                           :test #'equal)
          :stale-loading (johnson-live-child--loading-text-p
                          johnson-live-child--term)
          :stale-occurrences (johnson-live-child--count-occurrences
                              johnson-live-child--term))))

(defun johnson-live-child--state-fields ()
  "Return the outstanding-work fields shared by every telemetry record."
  (list :queue (length johnson--render-queue)
        :done (plist-get johnson--section-state :done)
        :total (plist-get johnson--section-state :total)
        :loading (and (markerp johnson--loading-marker)
                      (marker-position johnson--loading-marker)
                      t)
        :worker johnson-worker--state
        :worker-pid (and (process-live-p johnson-worker--process)
                         (process-id johnson-worker--process))
        :sections (length (johnson-live-child--section-names))))

(defun johnson-live-child--outstanding-p ()
  "Return non-nil when retrieval or render work is outstanding."
  (or (and (markerp johnson--loading-marker)
           (marker-position johnson--loading-marker))
      johnson--render-queue
      (let ((state johnson--section-state))
        (and state (< (plist-get state :done) (plist-get state :total))))
      (memq johnson-worker--state '(starting configuring retrieving))))

(defun johnson-live-child--section-names ()
  "Return the rendered dictionary section header names, in buffer order."
  (let ((names nil)
        (pos (point-min)))
    (while pos
      (let ((name (get-text-property pos 'johnson-section-header)))
        (when (and name (not (equal name "Contents"))
                   (or (= pos (point-min))
                       (not (equal name (get-text-property
                                         (1- pos)
                                         'johnson-section-header)))))
          (push name names)))
      (setq pos (next-single-property-change pos 'johnson-section-header)))
    (nreverse names)))

(defun johnson-live-child--overlay-names ()
  "Return the distinct section names carried by the buffer's overlays."
  (let (names)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when-let* ((name (overlay-get overlay 'johnson-section)))
        (unless (member name names)
          (push name names))))
    (nreverse names)))

(defun johnson-live-child--loading-text-p (word)
  "Return non-nil when the loading line for WORD is present."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward (format "Looking up \"%s\"" word) nil t) t)))

(defun johnson-live-child--error-text ()
  "Return the failure markers present in the results buffer, if any."
  (save-excursion
    (let (found)
      (dolist (pattern '("[Error retrieving " "[Error rendering entry: "
                         "[Johnson retrieval"))
        (goto-char (point-min))
        (when (search-forward pattern nil t)
          (push pattern found)))
      (nreverse found))))

(defun johnson-live-child--count-occurrences (word)
  "Return how many times WORD occurs in the buffer, as a whole word."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (count 0))
      (while (re-search-forward
              (concat "\\b" (regexp-quote word) "\\b") nil t)
        (setq count (1+ count)))
      count)))

(defun johnson-live-child--emit (record)
  "Append RECORD to the telemetry file as one printed line."
  (let ((print-length nil)
        (print-level nil))
    (write-region (concat (prin1-to-string record) "\n") nil
                  johnson-live-child--telemetry-file t 'silent)))

(provide 'johnson-live-child)
;;; johnson-live-child.el ends here
