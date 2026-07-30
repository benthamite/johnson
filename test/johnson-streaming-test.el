;;; johnson-streaming-test.el --- Streaming lookup tests -*- lexical-binding: t; -*-

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

;; ERT tests for the streamed lookup flow: ordered lookup plans,
;; shell-first display over a real retrieval worker child, history
;; object identity, no-result handling, the same-dictionary reference
;; scope fallback, streaming render correctness (section order, entry
;; order, loading lifetime, dynamic TOC items, point and window-start
;; preservation, error sections), and stale lookup generations,
;; including supersession in the middle of a multi-chunk entry.

;;; Code:

(require 'ert)
(require 'johnson)
(require 'johnson-worker)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)

;;;; Environment

(defconst johnson-streaming-test--child-remote-format
  (prin1-to-string
   '(johnson-register-format
     :name "remote-fixture"
     :extensions nil
     :detect #'ignore
     :parse-metadata #'ignore
     :build-index #'ignore
     :retrieve-entry #'ignore
     :render-entry #'ignore
     :worker-query (lambda (dict word)
                     (when (string-suffix-p "/hit" (plist-get dict :path))
                       (list (list :raw (format "REMOTE:%s" word)
                                   :context nil))))))
  "Form the worker child evaluates to register the remote test format.
The registered `:worker-query' matches only dictionaries whose path
ends in \"/hit\", so tests choose hit or miss through the dictionary
path.")

(defun johnson-streaming-test--child-command ()
  "Return the worker child command over the fixture and remote formats."
  (list (expand-file-name invocation-name invocation-directory)
        "-Q" "--batch"
        "-L" johnson-test-support-source-directory
        "-L" johnson-test-support-directory
        "-l" "johnson" "-l" "johnson-worker-fixture"
        "--eval" johnson-streaming-test--child-remote-format
        "--funcall" "johnson-worker-main"))

(defmacro johnson-streaming-test--with-env (&rest body)
  "Run BODY with clean johnson, worker, and history state.
The worker command is bound to the fixture child command,
`johnson-cache-directory' to a temporary directory, and the history
log to a fresh non-persisted list.  No formats are registered.  The
worker, its timers, opened databases, and the results buffer are
cleaned up even when BODY fails."
  (declare (indent 0) (debug t))
  `(johnson-test-support-with-temp-cache-dir
     (let ((johnson-worker-command-function
            #'johnson-streaming-test--child-command)
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
           (johnson--formats nil)
           (johnson--dictionaries nil)
           (johnson--indexed-p t)
           (johnson--db-cache (make-hash-table :test #'equal))
           (johnson--navigating-history nil)
           (johnson--current-source-lang nil)
           (johnson--current-target-lang nil)
           (johnson--current-custom-group nil)
           (johnson-dictionary-groups nil)
           (johnson-dictionary-priorities nil)
           (johnson-default-search-scope 'all)
           (johnson-history nil)
           (johnson--history-log nil)
           (johnson--history-log-loaded t)
           (johnson-history-persist nil))
       (unwind-protect
           (progn ,@body)
         (johnson-worker-stop)
         (cancel-function-timers #'johnson-worker--decode-next)
         (cancel-function-timers #'johnson--render-step)
         (maphash (lambda (_path db)
                    (condition-case nil (johnson-db-close db) (error nil)))
                  johnson--db-cache)
         (clrhash johnson--db-cache)
         (when (get-buffer "*johnson*")
           (kill-buffer "*johnson*"))
         (when (get-buffer johnson-worker--diagnostics-buffer-name)
           (kill-buffer johnson-worker--diagnostics-buffer-name))))))

(defun johnson-streaming-test--forbidden-retrieve (&rest _)
  "Signal, because parent-side retrieval is forbidden while streaming."
  (error "Parent-side retrieval ran in the streamed lookup path"))

(defun johnson-streaming-test--register-local-format ()
  "Register the parent half of the fixture format with fatal retrieval."
  (johnson-register-format
   :name "worker-fixture"
   :extensions nil
   :detect #'ignore
   :parse-metadata #'ignore
   :build-index #'ignore
   :retrieve-entry #'johnson-streaming-test--forbidden-retrieve
   :render-entry #'insert))

(defun johnson-streaming-test--register-remote-format ()
  "Register the parent half of the remote format with fatal legacy hooks."
  (johnson-register-format
   :name "remote-fixture"
   :extensions nil
   :detect #'ignore
   :parse-metadata #'ignore
   :build-index #'ignore
   :retrieve-entry #'johnson-streaming-test--forbidden-retrieve
   :render-entry #'insert
   :query-exact (lambda (&rest _)
                  (error "Legacy query-exact ran during plan construction"))
   :worker-query #'ignore))

(defun johnson-streaming-test--local-dict (name path entries &optional priority)
  "Return a local fixture dictionary NAME at PATH with indexed ENTRIES.
ENTRIES is a list of (WORD OFFSET) pairs inserted as real rows into
the dictionary's index database; OFFSET may be a fixture behavior
string.  PRIORITY defaults to 0."
  (let ((db (johnson--get-db path)))
    (dolist (entry entries)
      (johnson-db-insert-entry db (nth 0 entry) (nth 1 entry) 0)))
  (list :path path :name name :format-name "worker-fixture"
        :priority (or priority 0)))

(defun johnson-streaming-test--remote-dict (name path &optional priority)
  "Return a remote fixture dictionary NAME at PATH.
A PATH ending in \"/hit\" matches in the worker; any other PATH
misses.  PRIORITY defaults to 1."
  (list :path path :name name :format-name "remote-fixture"
        :priority (or priority 1)))

(defun johnson-streaming-test--buffer-text ()
  "Return the text of the results buffer without properties."
  (with-current-buffer "*johnson*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun johnson-streaming-test--lookup-finished-p ()
  "Return non-nil when the streamed lookup finished rendering."
  (with-current-buffer "*johnson*"
    (and (null johnson--loading-marker)
         (null johnson--render-queue)
         (not (timerp johnson--render-timer)))))

(defun johnson-streaming-test--wait-for-completion ()
  "Wait until the streamed lookup in the results buffer completes."
  (should (johnson-test-support-wait-for
           #'johnson-streaming-test--lookup-finished-p 15)))

(defun johnson-streaming-test--display (word)
  "Run the streamed lookup of WORD over the current dictionaries."
  (johnson--display-lookup
   word (johnson--lookup-plan word (johnson--dictionaries-by-priority))))

;;;; Ordered plans

(ert-deftest johnson-streaming-test-plan-orders-kinds-without-legacy-query ()
  "Plans hold indexed matches and remote candidates in dictionary order."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (johnson-streaming-test--register-remote-format)
    (let* ((local (johnson-streaming-test--local-dict
                   "Local Fixture" "/fixture/local" '(("house" 100))))
           (remote (johnson-streaming-test--remote-dict
                    "Remote Fixture" "/fixture/remote/hit"))
           (plan (johnson--lookup-plan "house" (list local remote))))
      (should (equal (mapcar (lambda (item) (plist-get item :kind)) plan)
                     '(indexed remote)))
      (should (equal (plist-get (nth 0 plan) :matches) '(("house" 100 0))))
      (should (equal (plist-get (nth 1 plan) :word) "house"))
      (should (eq (plist-get (nth 0 plan) :dict) local))
      (should (eq (plist-get (nth 1 plan) :dict) remote)))))

(ert-deftest johnson-streaming-test-plan-skips-local-misses ()
  "Local dictionaries without matches produce no descriptor."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (let* ((local (johnson-streaming-test--local-dict
                   "Local Fixture" "/fixture/local" '(("other" 100))))
           (plan (johnson--lookup-plan "house" (list local))))
      (should (null plan)))))

;;;; History object identity

(ert-deftest johnson-streaming-test-history-pushes-distinct-objects ()
  "Pushing the same word twice yields independently updatable objects."
  (johnson-streaming-test--with-env
    (let* ((older (johnson--history-log-push "house" 1))
           (newer (johnson--history-log-push "house" 1)))
      (should older)
      (should newer)
      (should-not (eq older newer))
      (johnson--history-log-set-count older 3)
      (should (= (plist-get older :dict-count) 3))
      (should (= (plist-get newer :dict-count) 1))
      (johnson--history-log-increment newer)
      (should (= (plist-get older :dict-count) 3))
      (should (= (plist-get newer :dict-count) 2)))))

(ert-deftest johnson-streaming-test-history-survives-supersession ()
  "Superseding a lookup leaves its history object in the log."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Slow Fixture" "/fixture/slow"
                 '(("house" "slow:0.5:HOUSE-ENTRY") ("cat" "CAT-ENTRY")))))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (let ((entry (buffer-local-value 'johnson--history-entry
                                       (get-buffer "*johnson*"))))
        (should entry)
        (johnson-streaming-test--display "cat")
        (should (memq entry johnson--history-log))
        (johnson-streaming-test--wait-for-completion)
        (should (memq entry johnson--history-log))
        (should (= (plist-get entry :dict-count) 1))))))

(ert-deftest johnson-streaming-test-history-survives-buffer-kill ()
  "Killing the results buffer keeps the history object and the worker."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Slow Fixture" "/fixture/slow"
                 '(("house" "slow:0.5:HOUSE-ENTRY")))))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (let ((entry (buffer-local-value 'johnson--history-entry
                                       (get-buffer "*johnson*"))))
        (should entry)
        (kill-buffer "*johnson*")
        (should (memq entry johnson--history-log))
        (should (johnson-worker-live-p))
        ;; The worker finishes against the dead buffer without erroring
        ;; and returns to ready.
        (should (johnson-test-support-wait-for
                 (lambda () (johnson-worker-ready-p)) 15))
        (should (memq entry johnson--history-log))))))

(ert-deftest johnson-streaming-test-history-survives-worker-exit ()
  "A worker crash keeps the history object in the log."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Slow Fixture" "/fixture/slow"
                 '(("house" "slow:5:HOUSE-ENTRY")))))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (let ((entry (buffer-local-value 'johnson--history-entry
                                       (get-buffer "*johnson*"))))
        (should entry)
        (should (johnson-test-support-wait-for
                 (lambda () (eq johnson-worker--state 'retrieving)) 10))
        (delete-process johnson-worker--process)
        (should (johnson-test-support-wait-for
                 (lambda () (eq johnson-worker--state 'failed)) 10))
        (should (memq entry johnson--history-log))
        (should (johnson-test-support-wait-for
                 (lambda ()
                   (string-match-p "Worker failed"
                                   (johnson-streaming-test--buffer-text)))
                 10))
        (should (memq entry johnson--history-log))))))

;;;; Shell-first display

(ert-deftest johnson-streaming-test-shell-displays-before-retrieval ()
  "The results shell is selected and loading before retrieval finishes."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Slow Fixture" "/fixture/slow"
                 '(("house" "slow:1.5:SLOW-HOUSE-ENTRY")))))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (should (eq (window-buffer (selected-window)) (get-buffer "*johnson*")))
      (should (string-match-p "Looking up"
                              (johnson-streaming-test--buffer-text)))
      (should-not (string-match-p "SLOW-HOUSE-ENTRY"
                                  (johnson-streaming-test--buffer-text)))
      (should (johnson-test-support-wait-for
               (lambda () (eq johnson-worker--state 'retrieving)) 10))
      (should (johnson-worker-live-p))
      (should (string-match-p "Looking up"
                              (johnson-streaming-test--buffer-text)))
      (should-not (string-match-p "SLOW-HOUSE-ENTRY"
                                  (johnson-streaming-test--buffer-text)))
      (johnson-streaming-test--wait-for-completion)
      (should (string-match-p "SLOW-HOUSE-ENTRY"
                              (johnson-streaming-test--buffer-text)))
      (should-not (string-match-p "Looking up"
                                  (johnson-streaming-test--buffer-text))))))

;;;; No-result handling

(ert-deftest johnson-streaming-test-empty-plan-shows-no-results ()
  "An empty initial plan displays the no-results message immediately."
  (johnson-streaming-test--with-env
    (save-window-excursion
      (johnson--display-lookup "zzz" nil)
      (should (string-match-p "No results found for \"zzz\"\\."
                              (johnson-streaming-test--buffer-text)))
      (should-not (johnson-worker-live-p))
      (let ((entry (car johnson--history-log)))
        (should entry)
        (should (= (plist-get entry :dict-count) 0))))))

(ert-deftest johnson-streaming-test-all-remote-misses-show-no-results ()
  "A plan of only remote misses ends with the no-results message."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-remote-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--remote-dict
                 "Remote Fixture" "/fixture/remote/miss")))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (should (string-match-p "Looking up"
                              (johnson-streaming-test--buffer-text)))
      (johnson-streaming-test--wait-for-completion)
      (should (string-match-p "No results found for \"house\"\\."
                              (johnson-streaming-test--buffer-text)))
      (let ((entry (car johnson--history-log)))
        (should (memq entry johnson--history-log))
        (should (= (plist-get entry :dict-count) 0))))))

;;;; Same-dictionary scope fallback

(ert-deftest johnson-streaming-test-same-scope-local-miss-falls-back ()
  "A restricted local miss falls back once to the full plan."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Alpha" "/fixture/alpha" '(("other" "OTHER-ENTRY")) 0)
                (johnson-streaming-test--local-dict
                 "Beta" "/fixture/beta" '(("house" "HOUSE-BETA")) 1)))
    (save-window-excursion
      (johnson--lookup-same-dictionary "house" "Alpha")
      (johnson-streaming-test--wait-for-completion)
      (should (string-match-p "HOUSE-BETA"
                              (johnson-streaming-test--buffer-text)))
      (should (= (length johnson--history-log) 1))
      (with-current-buffer "*johnson*"
        (should (equal johnson--nav-history '("house")))
        (let ((entry (car johnson--history-log)))
          (should (eq entry johnson--history-entry))
          (should (= (plist-get entry :dict-count) 1)))))))

(ert-deftest johnson-streaming-test-same-scope-worker-miss-falls-back ()
  "A restricted remote miss falls back once to the full plan."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (johnson-streaming-test--register-remote-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Beta" "/fixture/beta" '(("house" "HOUSE-BETA")) 0)
                (johnson-streaming-test--remote-dict
                 "Remote Miss" "/fixture/remote/miss" 1)
                (johnson-streaming-test--remote-dict
                 "Remote Hit" "/fixture/remote/hit" 2)))
    (save-window-excursion
      (johnson--lookup-same-dictionary "house" "Remote Miss")
      (johnson-streaming-test--wait-for-completion)
      (should (string-match-p "HOUSE-BETA"
                              (johnson-streaming-test--buffer-text)))
      (should (string-match-p "REMOTE:house"
                              (johnson-streaming-test--buffer-text)))
      (should (= (length johnson--history-log) 1))
      (with-current-buffer "*johnson*"
        (should (equal johnson--nav-history '("house")))
        (let ((entry (car johnson--history-log)))
          (should (eq entry johnson--history-entry))
          ;; One full-plan local match plus one remote hit.
          (should (= (plist-get entry :dict-count) 2)))))))

(ert-deftest johnson-streaming-test-refresh-and-nav-suppress-pushes ()
  "Refresh and history navigation push neither history nor navigation."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Fixture" "/fixture/dict"
                 '(("house" "HOUSE-ENTRY") ("cat" "CAT-ENTRY")))))
    (save-window-excursion
      (johnson--history-push "house")
      (johnson--display-lookup
       "house" (johnson--lookup-plan "house"
                                     (johnson--dictionaries-by-priority)))
      (johnson-streaming-test--wait-for-completion)
      (johnson--history-push "cat")
      (johnson--display-lookup
       "cat" (johnson--lookup-plan "cat"
                                   (johnson--dictionaries-by-priority)))
      (johnson-streaming-test--wait-for-completion)
      (should (= (length johnson--history-log) 2))
      (with-current-buffer "*johnson*"
        (should (equal johnson--nav-history '("house" "cat")))
        (johnson-refresh))
      (johnson-streaming-test--wait-for-completion)
      (should (= (length johnson--history-log) 2))
      (with-current-buffer "*johnson*"
        (should (equal johnson--nav-history '("house" "cat")))
        (johnson-history-back))
      (johnson-streaming-test--wait-for-completion)
      (should (= (length johnson--history-log) 2))
      (with-current-buffer "*johnson*"
        (should (equal johnson--nav-history '("house" "cat")))
        (should (= johnson--nav-position 0))
        (should (equal johnson--current-word "house"))
        (should (string-match-p "HOUSE-ENTRY"
                                (johnson-streaming-test--buffer-text)))))))

;;;; Streaming correctness helpers

(defun johnson-streaming-test--section-names ()
  "Return the rendered section header names, top to bottom.
The synthetic \"Contents\" TOC header is excluded."
  (with-current-buffer "*johnson*"
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
      (nreverse names))))

(defun johnson-streaming-test--section-position (name)
  "Return the buffer position of the section header NAME, or nil."
  (with-current-buffer "*johnson*"
    (let ((pos (point-min))
          (found nil))
      (while (and pos (not found))
        (when (equal (get-text-property pos 'johnson-section-header) name)
          (setq found pos))
        (setq pos (next-single-property-change pos 'johnson-section-header)))
      found)))

(defun johnson-streaming-test--section-overlay (name)
  "Return the section content overlay covering section NAME, or nil."
  (with-current-buffer "*johnson*"
    (cl-find-if (lambda (overlay)
                  (and (overlay-get overlay 'johnson-section-content)
                       (equal (overlay-get overlay 'johnson-section) name)))
                (overlays-in (point-min) (point-max)))))

(defun johnson-streaming-test--drain-render-queue ()
  "Wait until the render queue of the results buffer is drained."
  (let ((deadline (+ (float-time) 10)))
    (while (and (< (float-time) deadline)
                (with-current-buffer "*johnson*"
                  (or johnson--render-queue
                      (timerp johnson--render-timer))))
      (sit-for 0.02)))
  (with-current-buffer "*johnson*"
    (should (null johnson--render-queue))
    (should-not (timerp johnson--render-timer))))

(defmacro johnson-streaming-test--with-stubbed-worker (submits &rest body)
  "Run BODY with worker submissions recorded in the SUBMITS variable.
`johnson-worker-submit' pushes each request onto SUBMITS instead of
talking to a child, and the worker reports itself live so no process
is ever started.  Requests are recorded oldest first."
  (declare (indent 1) (debug (symbol body)))
  `(cl-letf (((symbol-function 'johnson-worker-live-p) (lambda () t))
             ((symbol-function 'johnson-worker-start) (lambda (_callback) nil))
             ((symbol-function 'johnson-worker-submit)
              (lambda (request)
                (setq ,submits (append ,submits (list request)))
                nil)))
     ,@body))

(defun johnson-streaming-test--lookup-id ()
  "Return the lookup generation of the results buffer."
  (buffer-local-value 'johnson--lookup-id (get-buffer "*johnson*")))

(defun johnson-streaming-test--feed (message)
  "Deliver worker core MESSAGE to the results buffer."
  (johnson--worker-message message))

(defun johnson-streaming-test--feed-dictionary (lookup dictionary name entries)
  "Feed the full message series of one matching dictionary.
LOOKUP and DICTIONARY identify the request, NAME is the dictionary
display name, and ENTRIES the list of raw entry strings."
  (johnson-streaming-test--feed
   (list :type 'dictionary-start :lookup lookup :dictionary dictionary
         :name name))
  (let ((eseq -1))
    (dolist (raw entries)
      (setq eseq (1+ eseq))
      (johnson-streaming-test--feed
       (list :type 'entry :lookup lookup :dictionary dictionary
             :entry eseq :raw raw :context nil))))
  (johnson-streaming-test--feed
   (list :type 'dictionary-complete :lookup lookup :dictionary dictionary
         :entries (length entries))))

;;;; Streaming correctness

(ert-deftest johnson-streaming-test-fifty-dictionaries-in-priority-order ()
  "Fifty one-entry dictionaries render fifty sections once, in order."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (cl-loop for n from 0 to 49
                   collect (johnson-streaming-test--local-dict
                            (format "Dict %02d" n)
                            (format "/fixture/many-%02d" n)
                            (list (list "house" (format "ENTRY-%02d" n)))
                            n)))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (johnson-streaming-test--wait-for-completion)
      (should (equal (johnson-streaming-test--section-names)
                     (cl-loop for n from 0 to 49
                              collect (format "Dict %02d" n))))
      (let ((text (johnson-streaming-test--buffer-text)))
        (dotimes (n 50)
          (should (string-match-p (format "ENTRY-%02d" n) text)))))))

(ert-deftest johnson-streaming-test-fifty-matches-in-one-section ()
  "Fifty matches of one dictionary render one section of ordered entries."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Big Dict" "/fixture/big"
                 (cl-loop for n from 0 to 49
                          collect (list "house" (format "ENTRY-%02d." n))))))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (johnson-streaming-test--wait-for-completion)
      (should (equal (johnson-streaming-test--section-names) '("Big Dict")))
      (let ((text (johnson-streaming-test--buffer-text))
            (positions nil))
        (dotimes (n 50)
          (let ((start (string-match (format "ENTRY-%02d\\." n) text)))
            (should start)
            (push start positions)
            (should-not (string-match (format "ENTRY-%02d\\." n) text
                                      (1+ start)))))
        (let ((ordered (reverse positions)))
          (should (equal ordered (sort (copy-sequence ordered) #'<)))))
      (should (johnson-streaming-test--section-overlay "Big Dict")))))

(ert-deftest johnson-streaming-test-loading-persists-until-final-close ()
  "The loading line survives every close unit but the final one."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Alpha" "/fixture/alpha" '(("house" "ALPHA-ENTRY")) 0)
                (johnson-streaming-test--local-dict
                 "Slow" "/fixture/slow"
                 '(("house" "slow:1.0:SLOW-ENTRY")) 1)))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (should (johnson-test-support-wait-for
               (lambda ()
                 (string-match-p "ALPHA-ENTRY"
                                 (johnson-streaming-test--buffer-text)))
               10))
      ;; The first section is closed, yet the lookup keeps loading.
      (should (string-match-p "Looking up"
                              (johnson-streaming-test--buffer-text)))
      (johnson-streaming-test--wait-for-completion)
      (should (string-match-p "SLOW-ENTRY"
                              (johnson-streaming-test--buffer-text)))
      (should-not (string-match-p "Looking up"
                                  (johnson-streaming-test--buffer-text))))))

(ert-deftest johnson-streaming-test-toc-targets-dynamic-remote-section ()
  "The TOC item added for a remote hit jumps to its rendered section."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (johnson-streaming-test--register-remote-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Alpha" "/fixture/alpha" '(("house" "ALPHA-ENTRY")) 0)
                (johnson-streaming-test--remote-dict
                 "Remote Fixture" "/fixture/remote/hit" 1)))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (johnson-streaming-test--wait-for-completion)
      (with-current-buffer "*johnson*"
        (goto-char (point-min))
        (should (search-forward "• Remote Fixture" nil t))
        (let ((button (button-at (1- (point)))))
          (should button)
          (button-activate button)
          (should (equal (get-text-property (point) 'johnson-section-header)
                         "Remote Fixture")))))))

(ert-deftest johnson-streaming-test-remote-miss-adds-no-toc-item ()
  "A remote dictionary that misses never gets a dead TOC item."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (johnson-streaming-test--register-remote-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Alpha" "/fixture/alpha" '(("house" "ALPHA-ENTRY")) 0)
                (johnson-streaming-test--remote-dict
                 "Remote Miss" "/fixture/remote/miss" 1)))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (johnson-streaming-test--wait-for-completion)
      (let ((text (johnson-streaming-test--buffer-text)))
        (should (string-match-p "ALPHA-ENTRY" text))
        (should (string-match-p "• Alpha" text))
        (should-not (string-match-p "Remote Miss" text))))))

(ert-deftest johnson-streaming-test-insertion-preserves-point-and-start ()
  "A TOC insertion above point and window-start moves neither logically."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (johnson-streaming-test--register-remote-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Alpha" "/fixture/alpha" '(("house" "ALPHA-ENTRY")) 0)
                (johnson-streaming-test--remote-dict
                 "Remote Fixture" "/fixture/remote/hit" 1)))
    (let ((submits nil))
      (johnson-streaming-test--with-stubbed-worker submits
        (save-window-excursion
          (johnson-streaming-test--display "house")
          (should (equal (mapcar (lambda (request)
                                   (plist-get request :dictionary))
                                 submits)
                         '(0)))
          (let ((lookup (johnson-streaming-test--lookup-id)))
            (johnson-streaming-test--feed-dictionary
             lookup 0 "Alpha" '("ALPHA-ENTRY"))
            (johnson-streaming-test--drain-render-queue)
            ;; The terminal message of dictionary 0 submitted dictionary 1.
            (should (equal (mapcar (lambda (request)
                                     (plist-get request :dictionary))
                                   submits)
                           '(0 1)))
            (with-current-buffer "*johnson*"
              (let* ((window (get-buffer-window "*johnson*"))
                     (target (johnson-streaming-test--section-position
                              "Alpha")))
                (should (window-live-p window))
                (should target)
                (should (> target (marker-position johnson--toc-marker)))
                (set-window-point window target)
                (set-window-start window target)
                (let ((expected (buffer-substring-no-properties
                                 target (+ target 9))))
                  (johnson-streaming-test--feed
                   (list :type 'dictionary-start :lookup lookup
                         :dictionary 1 :name "Remote Fixture"))
                  (johnson-streaming-test--drain-render-queue)
                  (should (string-match-p
                           "• Remote Fixture"
                           (johnson-streaming-test--buffer-text)))
                  (should (equal (buffer-substring-no-properties
                                  (window-point window)
                                  (+ (window-point window) 9))
                                 expected))
                  (should (equal (buffer-substring-no-properties
                                  (window-start window)
                                  (+ (window-start window) 9))
                                 expected)))))))))))

(ert-deftest johnson-streaming-test-error-closes-section-and-continues ()
  "A dictionary error closes an error section and dispatches the next."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Broken" "/fixture/broken" '(("house" "error:boom")) 0)
                (johnson-streaming-test--local-dict
                 "Beta" "/fixture/beta" '(("house" "HOUSE-BETA")) 1)))
    (save-window-excursion
      (johnson-streaming-test--display "house")
      (johnson-streaming-test--wait-for-completion)
      (let ((text (johnson-streaming-test--buffer-text)))
        (should (string-match-p "\\[Error retrieving Broken: boom\\]" text))
        (should (string-match-p "HOUSE-BETA" text)))
      (should (equal (johnson-streaming-test--section-names)
                     '("Broken" "Beta")))
      (let ((overlay (johnson-streaming-test--section-overlay "Broken")))
        (should overlay)
        (with-current-buffer "*johnson*"
          (should (string-match-p
                   "\\[Error retrieving Broken: boom\\]"
                   (buffer-substring-no-properties
                    (overlay-start overlay) (overlay-end overlay))))))
      (should-not (string-match-p "Looking up"
                                  (johnson-streaming-test--buffer-text))))))

;;;; Stale generations

(ert-deftest johnson-streaming-test-stale-generation-is-discarded ()
  "Stale generation messages leave no text, buttons, or overlays behind."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (let ((submits nil))
      (johnson-streaming-test--with-stubbed-worker submits
        (save-window-excursion
          (setq johnson--dictionaries
                (list (johnson-streaming-test--local-dict
                       "Stale One" "/fixture/stale-1"
                       '(("house" "STALE-ONE-ENTRY")) 0)
                      (johnson-streaming-test--local-dict
                       "Stale Two" "/fixture/stale-2"
                       '(("house" "STALE-TWO-ENTRY")) 1)))
          (johnson-streaming-test--display "house")
          (let ((stale-lookup (johnson-streaming-test--lookup-id)))
            (should (= (length submits) 1))
            (setq johnson--dictionaries
                  (list (johnson-streaming-test--local-dict
                         "Fresh One" "/fixture/fresh-1"
                         '(("cat" "FRESH-ONE-ENTRY")) 0)
                        (johnson-streaming-test--local-dict
                         "Fresh Two" "/fixture/fresh-2"
                         '(("cat" "FRESH-TWO-ENTRY")) 1)))
            (johnson-streaming-test--display "cat")
            (let ((fresh-lookup (johnson-streaming-test--lookup-id)))
              (should-not (equal stale-lookup fresh-lookup))
              (should (= (length submits) 2))
              ;; The stale lookup's start, entry, and terminal messages
              ;; must be dropped without queuing any render unit or
              ;; submitting the stale lookup's next dictionary.
              (johnson-streaming-test--feed-dictionary
               stale-lookup 0 "Stale One" '("STALE-ONE-ENTRY"))
              (with-current-buffer "*johnson*"
                (should (null johnson--render-queue)))
              (should (= (length submits) 2))
              (johnson-streaming-test--feed-dictionary
               fresh-lookup 0 "Fresh One" '("FRESH-ONE-ENTRY"))
              (johnson-streaming-test--drain-render-queue)
              (should (= (length submits) 3))
              (johnson-streaming-test--feed-dictionary
               fresh-lookup 1 "Fresh Two" '("FRESH-TWO-ENTRY"))
              (johnson-streaming-test--drain-render-queue)
              ;; Only one dictionary was ever in flight: each submission
              ;; happened only after the previous terminal message, and
              ;; the stale terminal never submitted "house" again.
              (should (equal (mapcar (lambda (request)
                                       (list (plist-get request :word)
                                             (plist-get request :dictionary)))
                                     submits)
                             '(("house" 0) ("cat" 0) ("cat" 1))))
              (let ((text (johnson-streaming-test--buffer-text)))
                (should (string-match-p "FRESH-ONE-ENTRY" text))
                (should (string-match-p "FRESH-TWO-ENTRY" text))
                (should-not (string-match-p "STALE" text))
                (should-not (string-match-p "Looking up" text)))
              (should (equal (johnson-streaming-test--section-names)
                             '("Fresh One" "Fresh Two")))
              (should-not (johnson-streaming-test--section-overlay
                           "Stale One")))))))))

;;;; Multi-chunk supersession over the real worker client

(defun johnson-streaming-test--cancel-decode-timer ()
  "Cancel any armed decode timer and clear the timer slot.
Clearing the slot matters: the worker client only rearms decoding when
no timer is recorded, and a cancelled timer object would otherwise be
mistaken for an armed one."
  (cancel-function-timers #'johnson-worker--decode-next)
  (setq johnson-worker--decode-timer nil))

(defun johnson-streaming-test--await-retrieving-untimed ()
  "Wait for the retrieving state, then cancel the decode timer.
Uses short accepts so the decode timer cannot consume reply frames in
the same accept that dispatched the request."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (eq johnson-worker--state 'retrieving))
                (< (float-time) deadline))
      (accept-process-output johnson-worker--process 0.005))
    (johnson-streaming-test--cancel-decode-timer))
  (should (eq johnson-worker--state 'retrieving)))

(defun johnson-streaming-test--pump-until (predicate what)
  "Decode buffered worker lines one at a time until PREDICATE holds.
WHAT names the awaited condition.  The decode timer is kept cancelled
so exactly one line is consumed per pump."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (if (johnson-worker--complete-line-buffered-p)
          (progn (johnson-worker--decode-one-line)
                 (johnson-streaming-test--cancel-decode-timer))
        (let ((johnson-worker--decode-delay 9999))
          (accept-process-output johnson-worker--process 0.02)
          (johnson-streaming-test--cancel-decode-timer)))))
  (unless (funcall predicate)
    (ert-fail (list "pump timeout" what
                    :state johnson-worker--state
                    :active johnson-worker--active-request))))

(ert-deftest johnson-streaming-test-multichunk-supersession-discards-stale ()
  "Superseding after chunk 0 of a multi-chunk entry stays a stale discard."
  (johnson-streaming-test--with-env
    (johnson-streaming-test--register-local-format)
    (setq johnson--dictionaries
          (list (johnson-streaming-test--local-dict
                 "Aleph" "/fixture/aleph" '(("houseA" "large:70000")) 0)
                (johnson-streaming-test--local-dict
                 "Beth" "/fixture/beth" '(("cat" "CAT-ENTRY")) 1)))
    (save-window-excursion
      (johnson-streaming-test--display "houseA")
      (let ((stale-lookup (johnson-streaming-test--lookup-id)))
        (johnson-streaming-test--await-retrieving-untimed)
        ;; Consume the reply up to and including chunk 0 of the
        ;; three-chunk entry, then supersede the lookup.
        (johnson-streaming-test--pump-until
         (lambda ()
           (let ((active johnson-worker--active-request))
             (and active
                  (eql (plist-get active :entry) 0)
                  (eql (plist-get active :chunk) 1))))
         "chunk 0 of the multi-chunk entry")
        (johnson-streaming-test--display "cat")
        (let ((fresh-lookup (johnson-streaming-test--lookup-id)))
          (should (plist-get johnson-worker--active-request :stale))
          (should (= (length johnson-worker--pending-requests) 1))
          ;; The remaining ordered chunks are accepted into the stale
          ;; discard path, not treated as sequence corruption.
          (johnson-streaming-test--pump-until
           (lambda ()
             (let ((active johnson-worker--active-request))
               (and active
                    (eql (plist-get active :next-entry) 1)
                    (null (plist-get active :entry)))))
           "remaining chunks of the stale entry")
          (should-not (eq johnson-worker--state 'failed))
          (should (equal (plist-get johnson-worker--active-request :lookup)
                         stale-lookup))
          ;; The fresh request is still queued: it starts only after the
          ;; stale terminal frame releases the worker.
          (should (= (length johnson-worker--pending-requests) 1))
          (with-current-buffer "*johnson*"
            (should (cl-notany (lambda (unit)
                                 (equal (plist-get unit :lookup)
                                        stale-lookup))
                               johnson--render-queue)))
          (johnson-streaming-test--pump-until
           (lambda ()
             (equal (plist-get johnson-worker--active-request :lookup)
                    fresh-lookup))
           "dispatch of the fresh lookup after the stale terminal")
          (should-not (eq johnson-worker--state 'failed))
          (should (null johnson-worker--pending-requests))
          ;; Hand decoding back to the ordinary timer path.
          (when (johnson-worker--complete-line-buffered-p)
            (johnson-worker--schedule-decode))
          (johnson-streaming-test--wait-for-completion)
          (let ((text (johnson-streaming-test--buffer-text)))
            (should (string-match-p "CAT-ENTRY" text))
            (should-not (string-match-p "xxxxx" text))
            (should-not (string-match-p "Aleph" text))
            (should-not (string-match-p "Looking up" text)))
          (should (equal (johnson-streaming-test--section-names)
                         '("Beth"))))))))

(provide 'johnson-streaming-test)
;;; johnson-streaming-test.el ends here
