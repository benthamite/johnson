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
;; object identity, no-result handling, and the same-dictionary
;; reference scope fallback.

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

(provide 'johnson-streaming-test)
;;; johnson-streaming-test.el ends here
