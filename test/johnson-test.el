;;; johnson-test.el --- Integration tests for johnson -*- lexical-binding: t; -*-

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

;; ERT integration tests for the johnson core module.

;;; Code:

(require 'ert)
(require 'johnson)

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
          (johnson-dictionary-priorities nil))
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

(ert-deftest johnson-test-display-results-no-results ()
  "Displays a no-results message when results are empty."
  (johnson-test--with-env
    (save-window-excursion
      (johnson--display-results "zzzzz" nil)
      (with-current-buffer "*johnson*"
        (should (string-match-p "No results found"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

(ert-deftest johnson-test-display-results-with-data ()
  "Displays results with section headers and overlays."
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
      (let ((results (johnson--query-all-exact "apple")))
        (save-window-excursion
          (johnson--display-results "apple" results)
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
              (should (> (length ovs) 0)))))))))

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
      (let ((results (johnson--query-all-exact "apple")))
        (save-window-excursion
          (johnson--display-results "apple" results)
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
                (should-not (overlay-get ov 'invisible))))))))))

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
    ;; Query for "cat" which is in test-dict.dsl.
    (let ((results (johnson--query-all-exact "cat")))
      (should (> (length results) 0))
      (save-window-excursion
        (johnson--display-results "cat" results)
        (with-current-buffer "*johnson*"
          (should (string-match-p "gato"
                                  (buffer-substring-no-properties
                                   (point-min) (point-max)))))))))

(provide 'johnson-test)
;;; johnson-test.el ends here
