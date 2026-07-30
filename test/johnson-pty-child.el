;;; johnson-pty-child.el --- Telemetry child for the PTY latency tests -*- lexical-binding: t; -*-

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

;; This file is loaded by test/johnson-pty-test.el into a disposable
;; interactive `emacs -Q -nw' child running on a pseudo-terminal.  It
;; reads its scenario name, telemetry file, and cache directory from
;; the JOHNSON_PTY_SCENARIO, JOHNSON_PTY_TELEMETRY, and
;; JOHNSON_PTY_CACHE environment variables, builds temporary SQLite
;; dictionaries over the deterministic worker fixture format, runs
;; `johnson--display-lookup', and then drops into the normal
;; interactive command loop so keys sent over the pseudo-terminal are
;; processed like real user input.
;;
;; Telemetry is appended to the telemetry file as single-line records,
;; not written to stdout: stdout is the child's terminal, so records
;; there would interleave with escape sequences and redraws, while the
;; file is read back deterministically by the outer test.  Three
;; record kinds are emitted: one command record per executed command
;; from a buffer-local `post-command-hook' in the results buffer,
;; `:marker' lifecycle records, and periodic `:status' records.
;; Command records extend the required key/command/point/section shape
;; with the render queue length, plan progress, loading state, worker
;; state, and rendered section count, so the outer test can assert
;; that every key overlapped outstanding retrieval and render work.
;;
;; The worker command is rebound here, in the disposable child only,
;; so the retrieval worker grandchild adds `-L test' and
;; `-l johnson-worker-fixture'; production code carries no test mode.

;;; Code:

(require 'johnson)
(require 'johnson-worker)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)
(require 'johnson-worker-fixture)

(defvar johnson-pty-child--telemetry-file (getenv "JOHNSON_PTY_TELEMETRY")
  "File this child appends single-line telemetry records to.")

(defvar johnson-pty-child--point-before nil
  "Value of point in the results buffer before the current command.")

(defvar johnson-pty-child--status-timer nil
  "Repeating timer emitting `:status' telemetry records.")

(defun johnson-pty-child-start ()
  "Set up the scenario named by JOHNSON_PTY_SCENARIO and start the lookup."
  (setq debug-on-error nil)
  (setq johnson-cache-directory (getenv "JOHNSON_PTY_CACHE"))
  (setq johnson-worker-command-function #'johnson-pty-child--worker-command)
  (setq johnson-history-persist nil)
  (setq johnson--history-log-loaded t)
  (setq johnson--indexed-p t)
  (setq johnson--dictionaries
        (johnson-pty-child--scenario-dictionaries
         (intern (getenv "JOHNSON_PTY_SCENARIO"))))
  (johnson-pty-child--emit (list :marker 'starting :at (float-time)))
  (johnson--display-lookup
   "house" (johnson--lookup-plan "house" (johnson--dictionaries-by-priority)))
  (with-current-buffer "*johnson*"
    (add-hook 'pre-command-hook #'johnson-pty-child--note-point nil t)
    (add-hook 'post-command-hook #'johnson-pty-child--record-command nil t)
    (setq johnson-pty-child--status-timer
          (run-at-time 0.05 0.05 #'johnson-pty-child--emit-status))
    (johnson-pty-child--emit
     (append (list :marker 'shell-visible :at (float-time))
             (johnson-pty-child--state-fields)))))

(defun johnson-pty-child--worker-command ()
  "Return the worker child command extended with the fixture format."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        "-L" johnson-test-support-source-directory
        "-L" johnson-test-support-directory
        "-l" "johnson" "-l" "johnson-worker-fixture"
        "--funcall" "johnson-worker-main"))

(defun johnson-pty-child--note-point ()
  "Record point in the results buffer before the current command."
  (setq johnson-pty-child--point-before (point)))

(defun johnson-pty-child--record-command ()
  "Append the telemetry record of the command that just completed."
  (johnson-pty-child--emit
   (append (list :key last-command-event
                 :command this-command
                 :before johnson-pty-child--point-before
                 :after (point)
                 :section (get-text-property (point) 'johnson-section-header)
                 :completed-at (float-time))
           (johnson-pty-child--state-fields))))

(defun johnson-pty-child--emit-status ()
  "Append a `:status' record describing the results buffer, when live."
  (when-let* ((buffer (get-buffer "*johnson*")))
    (with-current-buffer buffer
      (johnson-pty-child--emit
       (append (list :status t :at (float-time))
               (johnson-pty-child--state-fields))))))

(defun johnson-pty-child--state-fields ()
  "Return the outstanding-work fields shared by every telemetry record."
  (list :queue (length johnson--render-queue)
        :done (plist-get johnson--section-state :done)
        :total (plist-get johnson--section-state :total)
        :loading (and (markerp johnson--loading-marker)
                      (marker-position johnson--loading-marker)
                      t)
        :worker johnson-worker--state
        :sections (johnson-pty-child--section-count)))

(defun johnson-pty-child--section-count ()
  "Return the number of rendered dictionary section headers."
  (let ((count 0)
        (pos (point-min)))
    (while pos
      (let ((name (get-text-property pos 'johnson-section-header)))
        (when (and name (not (equal name "Contents"))
                   (or (= pos (point-min))
                       (not (equal name (get-text-property
                                         (1- pos)
                                         'johnson-section-header)))))
          (setq count (1+ count))))
      (setq pos (next-single-property-change pos 'johnson-section-header)))
    count))

(defun johnson-pty-child--emit (record)
  "Append RECORD to the telemetry file as one printed line."
  (let ((print-length nil)
        (print-level nil))
    (write-region (concat (prin1-to-string record) "\n") nil
                  johnson-pty-child--telemetry-file t 'silent)))

(defun johnson-pty-child--scenario-dictionaries (scenario)
  "Return the freshly indexed dictionary list for SCENARIO."
  (pcase scenario
    ('slow-first
     (cons (johnson-pty-child--make-dictionary
            "Slow Dict" "/pty/slow"
            '(("house" "slow:2.0:SLOW-FIRST-ENTRY")) 0)
           (cl-loop for n from 1 to 4
                    collect (johnson-pty-child--make-dictionary
                             (format "Fast Dict %d" n)
                             (format "/pty/fast-%d" n)
                             (cl-loop for e from 0 to 3
                                      collect
                                      (list "house"
                                            (format "render-slow:0.08:FAST-%d-%d"
                                                    n e)))
                             n))))
    ('fifty-dictionaries
     (cl-loop for n from 0 to 49
              collect (johnson-pty-child--make-dictionary
                       (format "Dict %02d" n) (format "/pty/dict-%02d" n)
                       (list (list "house"
                                   (format "render-slow:0.05:ENTRY-%02d" n)))
                       n)))
    ('fifty-matches
     (list (johnson-pty-child--make-dictionary
            "Big Dict" "/pty/big"
            (cons '("house" "slow:1.0:MATCH-00")
                  (cl-loop for n from 1 to 49
                           collect (list "house"
                                         (format "render-slow:0.05:MATCH-%02d"
                                                 n))))
            0)))
    ('frame-burst
     (cons (johnson-pty-child--make-dictionary
            "Burst Dict" "/pty/burst"
            '(("house" "slow:1.5:BURST-FIRST")
              ("house" "large:70000")
              ("house" "large:70000")
              ("house" "render-slow:0.08:BURST-3")
              ("house" "render-slow:0.08:BURST-4"))
            0)
           (cl-loop for n from 1 to 3
                    collect (johnson-pty-child--make-dictionary
                             (format "Burst Tail %d" n)
                             (format "/pty/tail-%d" n)
                             (cl-loop for e from 0 to 3
                                      collect
                                      (list "house"
                                            (format "render-slow:0.08:TAIL-%d-%d"
                                                    n e)))
                             n))))
    (_ (error "Unknown PTY scenario %s" scenario))))

(defun johnson-pty-child--make-dictionary (name path entries priority)
  "Return dictionary NAME at PATH after indexing ENTRIES at PRIORITY.
ENTRIES is a list of (WORD OFFSET) pairs inserted as rows into the
dictionary's index database; OFFSET may be a fixture behavior string."
  (let ((db (johnson--get-db path)))
    (dolist (entry entries)
      (johnson-db-insert-entry db (nth 0 entry) (nth 1 entry) 0)))
  (list :path path :name name :format-name "worker-fixture"
        :priority priority))

(when (getenv "JOHNSON_PTY_SCENARIO")
  (johnson-pty-child-start))

(provide 'johnson-pty-child)
;;; johnson-pty-child.el ends here
