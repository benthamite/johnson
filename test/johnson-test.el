;;; johnson-test.el --- Integration tests for johnson -*- lexical-binding: t; -*-

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

;; ERT integration tests for the johnson core module.

;;; Code:

(require 'ert)
(require 'johnson)
(require 'johnson-worker)
(eval-and-compile
  (add-to-list 'load-path
               (file-name-directory (or load-file-name buffer-file-name))))
(require 'johnson-test-support)

;;;; Helpers

(defvar johnson-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-test--fixture (name)
  "Return the full path to fixture file NAME."
  (expand-file-name name johnson-test--fixtures-dir))

(defun johnson-test--kill-cache-buffers ()
  "Kill all johnson cache buffers."
  (dolist (buf (buffer-list))
    (when (string-prefix-p " *johnson-cache: " (buffer-name buf))
      (kill-buffer buf))))

(defun johnson-test--cleanup ()
  "Clean up johnson state."
  (johnson-test--kill-cache-buffers)
  (when (get-buffer "*johnson*")
    (kill-buffer "*johnson*"))
  (clrhash johnson--db-cache)
  (setq johnson--dictionaries nil)
  (setq johnson--indexed-p nil)
  (setq johnson--formats nil)
  (setq johnson-history nil))

(defmacro johnson-test--with-env (&rest body)
  "Execute BODY with a clean johnson environment and temp cache.
Sets up `johnson-cache-directory' and `johnson-dictionary-directories'
pointing to a temporary directory containing the test fixtures.
Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(let* ((temp-cache (make-temp-file "johnson-test-cache-" t))
          (johnson-cache-directory temp-cache)
          (johnson-dictionary-directories (list johnson-test--fixtures-dir))
          (johnson--formats nil)
          (johnson--dictionaries nil)
          (johnson--indexed-p nil)
          (johnson--current-source-lang nil)
          (johnson--current-target-lang nil)
          (johnson--current-custom-group nil)
          (johnson--db-cache (make-hash-table :test #'equal))
          (johnson--navigating-history nil)
          (johnson-history nil)
          (johnson-default-search-scope 'all)
          (johnson-dictionary-groups nil)
          (johnson-dictionary-priorities nil)
          (johnson--history-log nil)
          (johnson--history-log-loaded t)
          (johnson-history-persist nil)
          (johnson-worker--process nil)
          (johnson-worker--receive-buffer nil)
          (johnson-worker--decode-timer nil)
          (johnson-worker--message-function nil)
          (johnson-worker--core-function nil)
          (johnson-worker--state 'stopped)
          (johnson-worker--terminating nil)
          (johnson-worker--pending-requests nil)
          (johnson-worker--active-request nil)
          (johnson-worker--entry-assemblies (make-hash-table :test #'equal)))
     ;; Re-register the DSL format.
     (johnson-register-format
      :name "dsl"
      :extensions '("dsl")
      :detect #'johnson-dsl-detect
      :parse-metadata #'johnson-dsl-parse-metadata
      :build-index #'johnson-dsl-build-index
      :retrieve-entry #'johnson-dsl-retrieve-entry
      :render-entry #'johnson-dsl-render-entry)
     (unwind-protect
         (progn ,@body)
       (johnson-worker-stop)
       (cancel-function-timers #'johnson-worker--decode-next)
       (cancel-function-timers #'johnson--render-step)
       (johnson-test--kill-cache-buffers)
       (condition-case nil
           (progn
             (maphash (lambda (_k db)
                        (condition-case nil (johnson-db-close db) (error nil)))
                      johnson--db-cache)
             (clrhash johnson--db-cache))
         (error nil))
       (when (get-buffer "*johnson*")
         (kill-buffer "*johnson*"))
       (delete-directory temp-cache t))))

(defun johnson-test--wait-for-lookup ()
  "Wait until the streamed lookup in the results buffer completes."
  (should (johnson-test-support-wait-for
           (lambda ()
             (with-current-buffer "*johnson*"
               (and (null johnson--loading-marker)
                    (null johnson--render-queue)
                    (not (timerp johnson--render-timer)))))
           15)))

(defun johnson-test--display-lookup-and-wait (word)
  "Run a streamed lookup of WORD and wait for it to finish rendering."
  (johnson--display-lookup
   word (johnson--lookup-plan word (johnson--dictionaries-by-priority)))
  (johnson-test--wait-for-lookup))

;;;; Format registry

(ert-deftest johnson-test-register-format ()
  "Registers and retrieves a format."
  (johnson-test--with-env
    (should (johnson--get-format "dsl"))
    (should (equal (plist-get (johnson--get-format "dsl") :name) "dsl"))))

(ert-deftest johnson-test-register-format-replace ()
  "Re-registering a format replaces the old one."
  (johnson-test--with-env
    (johnson-register-format :name "dsl" :extensions '("dsl2") :detect #'ignore)
    (should (equal (plist-get (johnson--get-format "dsl") :extensions) '("dsl2")))))

;;;; Format detection

(ert-deftest johnson-test-detect-format-dsl ()
  "Detects DSL files via the registered format."
  (johnson-test--with-env
    (let ((fmt (johnson--detect-format (johnson-test--fixture "test-dict.dsl"))))
      (should fmt)
      (should (equal (plist-get fmt :name) "dsl")))))

(ert-deftest johnson-test-detect-format-nil-for-non-dict ()
  "Returns nil for non-dictionary files."
  (johnson-test--with-env
    (let ((tmp (make-temp-file "johnson-test-" nil ".txt")))
      (unwind-protect
          (progn
            (with-temp-file tmp (insert "not a dict"))
            (should-not (johnson--detect-format tmp)))
        (delete-file tmp)))))

;;;; Discovery

(ert-deftest johnson-test-discover ()
  "Discovers dictionaries from the fixtures directory."
  (johnson-test--with-env
    (johnson--discover)
    (should (> (length johnson--dictionaries) 0))
    ;; Should find at least the main test-dict.dsl.
    (should (cl-find-if
             (lambda (d) (string-match-p "test-dict\\.dsl\\'" (plist-get d :path)))
             johnson--dictionaries))))

(ert-deftest johnson-test-discover-parses-metadata ()
  "Discovered dictionaries have parsed metadata."
  (johnson-test--with-env
    (johnson--discover)
    (let ((dict (cl-find-if
                 (lambda (d)
                   (equal (plist-get d :name) "Test Dictionary"))
                 johnson--dictionaries)))
      (should dict)
      (should (equal (plist-get dict :source-lang) "English"))
      (should (equal (plist-get dict :target-lang) "Spanish")))))

;;;; Normalize + query round-trip

(ert-deftest johnson-test-normalize-query-roundtrip ()
  "Normalizing \"cafe\" matches an entry indexed as \"cafe\"."
  (johnson-test--with-env
    (let* ((dict-path "/tmp/johnson-roundtrip-test.dsl")
           (db (johnson-db-open dict-path)))
      (unwind-protect
          (progn
            (johnson-db-insert-entry db "café" 100 50)
            (let ((results (johnson-db-query-exact db "cafe")))
              (should (= (length results) 1))
              (should (equal (caar results) "café"))))
        (johnson-db-close db)))))

;;;; Format number

(ert-deftest johnson-test-format-number ()
  "Formats numbers with comma separators."
  (should (equal (johnson--format-number 0) "0"))
  (should (equal (johnson--format-number 999) "999"))
  (should (equal (johnson--format-number 1000) "1,000"))
  (should (equal (johnson--format-number 45231) "45,231"))
  (should (equal (johnson--format-number 1000000) "1,000,000")))

;;;; Section header

(ert-deftest johnson-test-insert-section-header ()
  "Section header has correct face and properties."
  (with-temp-buffer
    (johnson--insert-section-header "TestDict")
    (goto-char (point-min))
    (should (get-text-property (point) 'johnson-section-header))
    (should (equal (get-text-property (point) 'johnson-section-header) "TestDict"))
    (should (eq (get-text-property (point) 'face) 'johnson-section-header-face))))

;;;; Display results

(ert-deftest johnson-test-display-lookup-no-results ()
  "Displays a no-results message when the lookup plan is empty."
  (johnson-test--with-env
    (save-window-excursion
      (johnson--display-lookup "zzzzz" nil)
      (with-current-buffer "*johnson*"
        (should (string-match-p "No results found"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

(ert-deftest johnson-test-display-lookup-with-data ()
  "Displays streamed results with section headers and overlays."
  (johnson-test--with-env
    (johnson--discover)
    ;; Index the main test dictionary.
    (let* ((dict (cl-find-if
                  (lambda (d) (string-match-p "test-dict\\.dsl\\'" (plist-get d :path)))
                  johnson--dictionaries))
           (path (plist-get dict :path))
           (fmt (johnson--get-format (plist-get dict :format-name)))
           (db (johnson--get-db path))
           (entries nil))
      (johnson-db-reset db)
      (funcall (plist-get fmt :build-index) path
               (lambda (hw offset len) (push (list hw offset len) entries)))
      (johnson-db-insert-entries-batch db (nreverse entries))
      (johnson-db-set-metadata db "mtime"
                               (format-time-string "%s"
                                                   (file-attribute-modification-time
                                                    (file-attributes path))))
      (setq johnson--indexed-p t)
      (setq johnson--dictionaries (list dict))
      (save-window-excursion
        (johnson-test--display-lookup-and-wait "apple")
        (with-current-buffer "*johnson*"
          ;; Should have section header.
          (goto-char (point-min))
          (should (equal (get-text-property (point-min)
                                            'johnson-section-header)
                         "Test Dictionary"))
          ;; Should have section content overlay.
          (let ((ovs (cl-remove-if-not
                      (lambda (ov) (overlay-get ov 'johnson-section-content))
                      (overlays-in (point-min) (point-max)))))
            (should (> (length ovs) 0))))))))

(ert-deftest johnson-test-render-step-drains-fast-units ()
  "The ordinary-timer renderer drains fast queued units in one step."
  (johnson-test--with-env
    (let ((johnson-render-batch-size 1)
          (johnson-render-batch-time-budget 1.0))
      (johnson-register-format
       :name "fake"
       :extensions nil
       :detect #'ignore
       :render-entry (lambda (raw) (insert raw)))
      (with-current-buffer (get-buffer-create "*johnson*")
        (let ((inhibit-read-only t)
              (dict (list :path "/fake" :name "Fake" :format-name "fake")))
          (johnson-mode)
          (erase-buffer)
          (setq johnson--current-word "house")
          (setq johnson--lookup-id 1)
          (setq johnson--lookup-plan
                (list (list :kind 'indexed :dict dict :matches nil)))
          (setq johnson--section-state (list :word "house" :total 1 :next 1
                                             :done 1 :matched 1
                                             :fallback nil
                                             :section-start nil
                                             :section-name nil))
          (setq johnson--render-marker (point-marker))
          (setq johnson--render-queue
                (append (list (list :type 'section-start :lookup 1
                                    :dict dict))
                        (cl-loop for n from 1 to 5
                                 collect (list :type 'entry :lookup 1
                                               :dict dict
                                               :packet
                                               (list :raw (format "entry-%d " n)
                                                     :context nil)))
                        (list (list :type 'section-complete :lookup 1
                                    :dict dict)
                              (list :type 'lookup-complete :lookup 1))))
          (johnson--render-step (current-buffer))
          (should-not johnson--render-queue)
          (should-not (timerp johnson--render-timer))
          (dotimes (n 5)
            (should (string-match-p (format "entry-%d" (1+ n))
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))))))))

;;;; Worker format hooks

(ert-deftest johnson-test-worker-format-hooks ()
  "New worker hooks dispatch preparation and context rendering."
  (johnson-test--with-env
    (johnson-register-format
     :name "worker-fake"
     :retrieve-entry (lambda (_path _offset _length) "raw")
     :render-entry (lambda (raw) (insert raw))
     :worker-prepare-entry
     (lambda (_dict _match raw)
       (list :raw raw :context '(:label "prepared")))
     :render-entry-with-context
     (lambda (raw context)
       (insert (plist-get context :label) ":" raw)))
    (let* ((fmt (johnson--get-format "worker-fake"))
           (packet (johnson-worker--prepare-entry
                    fmt '(:path "/tmp/fake") nil "raw")))
      (should (equal packet '(:raw "raw" :context (:label "prepared"))))
      (with-temp-buffer
        (johnson--render-entry-packet fmt packet)
        (should (equal (buffer-string) "prepared:raw"))))))

(ert-deftest johnson-test-worker-format-hooks-default ()
  "Formats with only old hooks get the default packet and renderer."
  (johnson-test--with-env
    (johnson-register-format
     :name "worker-old"
     :retrieve-entry (lambda (_path _offset _length) "raw")
     :render-entry (lambda (raw) (insert raw)))
    (let* ((fmt (johnson--get-format "worker-old"))
           (packet (johnson-worker--prepare-entry
                    fmt '(:path "/tmp/fake") nil "raw")))
      (should (equal packet '(:raw "raw" :context nil)))
      (with-temp-buffer
        (johnson--render-entry-packet fmt packet)
        (should (equal (buffer-string) "raw"))))))

;;;; Navigation history

(ert-deftest johnson-test-nav-history-push ()
  "Pushing words to nav history works."
  (with-temp-buffer
    (setq-local johnson--nav-history nil)
    (setq-local johnson--nav-position -1)
    (johnson--nav-push "alpha")
    (should (equal johnson--nav-history '("alpha")))
    (should (= johnson--nav-position 0))
    (johnson--nav-push "beta")
    (should (equal johnson--nav-history '("alpha" "beta")))
    (should (= johnson--nav-position 1))))

(ert-deftest johnson-test-nav-history-no-duplicates ()
  "Pushing the same word twice doesn't duplicate."
  (with-temp-buffer
    (setq-local johnson--nav-history nil)
    (setq-local johnson--nav-position -1)
    (johnson--nav-push "alpha")
    (johnson--nav-push "alpha")
    (should (= (length johnson--nav-history) 1))))

(ert-deftest johnson-test-nav-history-truncate-forward ()
  "Going back and pushing truncates forward history."
  (with-temp-buffer
    (setq-local johnson--nav-history nil)
    (setq-local johnson--nav-position -1)
    (johnson--nav-push "a")
    (johnson--nav-push "b")
    (johnson--nav-push "c")
    ;; Go back to position 1 ("b").
    (setq johnson--nav-position 1)
    (johnson--nav-push "d")
    ;; Forward history from "c" should be gone.
    (should (equal johnson--nav-history '("a" "b" "d")))))

;;;; Section collapsing

(ert-deftest johnson-test-toggle-section ()
  "Toggle section makes overlay invisible and back."
  (johnson-test--with-env
    (johnson--discover)
    (let* ((dict (cl-find-if
                  (lambda (d) (string-match-p "test-dict\\.dsl\\'" (plist-get d :path)))
                  johnson--dictionaries))
           (path (plist-get dict :path))
           (fmt (johnson--get-format (plist-get dict :format-name)))
           (db (johnson--get-db path))
           (entries nil))
      (johnson-db-reset db)
      (funcall (plist-get fmt :build-index) path
               (lambda (hw offset len) (push (list hw offset len) entries)))
      (johnson-db-insert-entries-batch db (nreverse entries))
      (setq johnson--indexed-p t)
      (setq johnson--dictionaries (list dict))
      (save-window-excursion
        (johnson-test--display-lookup-and-wait "apple")
        (with-current-buffer "*johnson*"
          (goto-char (point-min))
          ;; Find the section content overlay.
          (let ((ovs (cl-remove-if-not
                      (lambda (ov) (overlay-get ov 'johnson-section-content))
                      (overlays-in (point-min) (point-max)))))
            (should (> (length ovs) 0))
            (let ((ov (car ovs)))
              (should-not (overlay-get ov 'invisible))
              ;; Toggle to collapse.
              (johnson-toggle-section)
              (should (overlay-get ov 'invisible))
              ;; Toggle again to expand.
              (johnson-toggle-section)
              (should-not (overlay-get ov 'invisible)))))))))

;;;; Full integration: discover, index, query, display

(ert-deftest johnson-test-full-integration ()
  "Full round-trip: discover, index, query, display."
  (johnson-test--with-env
    (johnson--discover)
    (should (> (length johnson--dictionaries) 0))
    ;; Index all dictionaries.
    (dolist (dict johnson--dictionaries)
      (let* ((path (plist-get dict :path))
             (fmt (johnson--get-format (plist-get dict :format-name)))
             (db (johnson--get-db path))
             (entries nil))
        (johnson-db-reset db)
        (funcall (plist-get fmt :build-index) path
                 (lambda (hw offset len) (push (list hw offset len) entries)))
        (johnson-db-insert-entries-batch db (nreverse entries))
        (johnson-db-set-metadata db "mtime"
                                 (format-time-string "%s"
                                                     (file-attribute-modification-time
                                                      (file-attributes path))))))
    (setq johnson--indexed-p t)
    ;; Plan for "cat" which is in test-dict.dsl.
    (let ((plan (johnson--lookup-plan "cat" (johnson--dictionaries-by-priority))))
      (should (> (length plan) 0))
      (save-window-excursion
        (johnson--display-lookup "cat" plan)
        (johnson-test--wait-for-lookup)
        (with-current-buffer "*johnson*"
          (should (string-match-p "gato"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))))))

;;;; Eldoc

(ert-deftest johnson-test-eldoc-keeps-synchronous-doc ()
  "Eldoc's default strategy keeps a definition delivered via the callback.
The hook function must not return nil after calling CALLBACK, or
`eldoc--invoke-strategy' clears the echo area it just filled."
  (require 'eldoc)
  (let ((johnson--eldoc-cache (make-hash-table :test #'equal))
        (displayed nil)
        (cleared nil))
    (puthash "apple" "a round fruit" johnson--eldoc-cache)
    (with-temp-buffer
      (insert "apple")
      (goto-char (point-min))
      (setq-local eldoc-documentation-functions
                  (list #'johnson-eldoc-function))
      (let ((eldoc-documentation-strategy #'eldoc-documentation-default)
            (eldoc-display-functions
             (list (lambda (docs _interactive)
                     (setq displayed (mapcar #'car docs))))))
        (cl-letf (((symbol-function 'eldoc--message)
                   (lambda (&optional string)
                     (unless string (setq cleared t)))))
          (eldoc--invoke-strategy t))))
    (should (equal displayed '("a round fruit")))
    (should-not cleared)))

;;;; Completion table

(ert-deftest johnson-test-completion-table-keeps-normalized-matches ()
  "Completion keeps candidates that match only after normalization.
The database matches by case- and accent-folded prefix; the table must
not drop those rows again with a raw-string prefix filter."
  (johnson-test--with-env
    (let* ((dict-path (expand-file-name "completion-test.dsl"
                                        temp-cache))
           (johnson-db--completion-db nil)
           (johnson-completion-min-chars 3))
      (with-temp-file dict-path (insert "x"))
      (let ((db (johnson-db-open dict-path)))
        (johnson-db-insert-entries-batch
         db '(("Café" 0 1) ("café au lait" 2 1) ("cafeteria" 3 1)))
        (johnson-db-close db))
      (johnson-db-rebuild-completion-index (list dict-path))
      (unwind-protect
          (let* ((table (johnson--completion-table))
                 (all (funcall table "cafe" nil t)))
            (should (member "Café" all))
            (should (member "café au lait" all))
            (should (member "cafeteria" all))
            ;; A lone normalized match completes to the raw headword.
            (should (equal (funcall table "cafe a" nil nil) "café au lait"))
            ;; Exact-match testing still uses the raw headword.
            (should (funcall table "Café" nil 'lambda))
            (should-not (funcall table "cafe" nil 'lambda)))
        (johnson-db-close-completion-db)))))

;;;; Plain-text conversion

(ert-deftest johnson-test-plain-text-ignores-oversized-entity ()
  "A numeric entity beyond `max-char' is left in place instead of erroring."
  (should (equal (johnson--entry-to-plain-text "a &#5000000; b" "mdict")
                 "a &#5000000; b"))
  (should (equal (johnson--entry-to-plain-text "a &#65; b" "mdict") "a A b")))

;;;; Transient menu

(defun johnson-test--layout-keys (tree)
  "Return every `:key' string found in the transient layout TREE."
  (let ((keys nil))
    (cl-labels ((walk (node)
                  (cond
                   ((and (consp node) (plist-member node :key)
                         (stringp (plist-get node :key)))
                    (push (plist-get node :key) keys))
                   ((consp node) (walk (car node)) (walk (cdr node)))
                   ((vectorp node) (mapc #'walk node)))))
      (walk tree))
    (nreverse keys)))

(ert-deftest johnson-test-menu-keys-are-unique ()
  "No key in `johnson-menu' is bound to two different suffixes."
  (require 'johnson-transient)
  (let* ((keys (johnson-test--layout-keys
                (get 'johnson-menu 'transient--layout)))
         (dupes (cl-remove-if-not
                 (lambda (k) (> (cl-count k keys :test #'equal) 1))
                 (delete-dups (copy-sequence keys)))))
    (should (member "O" keys))
    (should (equal dupes nil))))

;;;; Section navigation

(ert-deftest johnson-test-section-navigation-header-at-point-min ()
  "A header at the buffer start is reachable by `j' and `p'.
Single-dictionary results render no table of contents, so their only
header starts at `point-min'."
  (with-temp-buffer
    (johnson--insert-section-header "Oxford")
    (insert "\nentry text\n\n")
    (goto-char (point-max))
    (johnson-prev-section)
    (should (= (point) (point-min)))
    (goto-char (point-max))
    (let ((offered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt names &rest _)
                   (setq offered names) (car names))))
        (johnson-jump-to-section))
      (should (equal offered '("Oxford")))
      (should (= (point) (point-min))))))

;;;; Scan mode

(ert-deftest johnson-test-scan-mode-reenable-does-not-leak-timer ()
  "Enabling `johnson-scan-mode' twice leaves one idle timer, disabling none."
  (let ((johnson-scan-trigger 'idle)
        (johnson--scan-idle-timer nil))
    (cl-flet ((timers ()
                (cl-count #'johnson-scan--on-idle timer-idle-list
                          :key #'timer--function)))
      (unwind-protect
          (progn
            (johnson-scan-mode 1)
            (johnson-scan-mode 1)
            (should (= (timers) 1))
            (johnson-scan-mode -1)
            (should (= (timers) 0)))
        (johnson-scan-mode -1)
        (cancel-function-timers #'johnson-scan--on-idle)))))

;;;; Full-text search buffer state

(ert-deftest johnson-test-fts-buffer-keeps-query-separately ()
  "FTS results keep their query out of `johnson--current-word'.
Refreshing re-runs the full-text search and bookmarking refuses the
pseudo-headword."
  (johnson-test--with-env
    (let ((queries nil)
          (johnson--bookmarks nil)
          (johnson--bookmarks-loaded t))
      (cl-letf (((symbol-function 'johnson--query-all-fts)
                 (lambda (query)
                   (push query queries)
                   (list (list (list :name "Fake" :path "/fake")
                               "apple" ">>>fruit<<< snippet"))))
                ((symbol-function 'johnson--ensure-indexed)
                 (lambda () t))
                ((symbol-function 'johnson--save-bookmarks) #'ignore))
        (save-window-excursion
          (johnson-search "fruit")
          (with-current-buffer "*johnson*"
            (should (equal johnson--current-fts-query "fruit"))
            (should-not johnson--current-word)
            (should (string-match-p "Full-text search: \"fruit\" (1 results)"
                                    (buffer-substring-no-properties
                                     (point-min) (point-max))))
            (johnson-refresh)
            (should (equal queries '("fruit" "fruit")))
            (should-error (johnson-bookmark-add) :type 'user-error)
            (should-not johnson--bookmarks)))))))

;;;; Resource extraction argument safety

(ert-deftest johnson-test-unzip-member-pattern-escapes-globs ()
  "Unzip glob characters in a resource name are escaped literally."
  (should (equal (johnson--unzip-member-pattern "a[1]*?.mp3")
                 "a\\[1]\\*\\?.mp3"))
  (should (equal (johnson--unzip-member-pattern "back\\slash")
                 "back\\\\slash"))
  (should (equal (johnson--unzip-member-pattern "plain.mp3") "plain.mp3")))

(provide 'johnson-test)
;;; johnson-test.el ends here
