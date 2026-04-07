;;; johnson-db-test.el --- Tests for johnson-db -*- lexical-binding: t; -*-

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

;; ERT tests for the johnson-db sqlite index management module.

;;; Code:

(require 'ert)
(require 'johnson)

;;;; Helpers

(defun johnson-db-test--make-temp-cache ()
  "Create a temporary directory for use as `johnson-cache-directory'."
  (make-temp-file "johnson-db-test-" t))

(defmacro johnson-db-test--with-temp-cache (&rest body)
  "Execute BODY with `johnson-cache-directory' bound to a temp dir.
Cleans up the directory afterwards."
  (declare (indent 0) (debug t))
  `(let* ((johnson-cache-directory (johnson-db-test--make-temp-cache))
          (result nil))
     (unwind-protect
         (setq result (progn ,@body))
       (delete-directory johnson-cache-directory t))
     result))

(defmacro johnson-db-test--with-temp-db (&rest body)
  "Execute BODY with a temp cache dir and an open db bound to `db'.
Also binds `dict-path' to a dummy path.  Cleans up afterwards."
  (declare (indent 0) (debug t))
  `(johnson-db-test--with-temp-cache
     (let* ((dict-path "/tmp/johnson-test-dummy.dsl")
            (db (johnson-db-open dict-path)))
       (unwind-protect
           (progn ,@body)
         (johnson-db-close db)))))

;;;; johnson-db-normalize

(ert-deftest johnson-db-test-normalize-basic ()
  "Downcases and strips diacriticals."
  (should (equal (johnson-db-normalize "Hello") "hello"))
  (should (equal (johnson-db-normalize "café") "cafe"))
  (should (equal (johnson-db-normalize "naïve") "naive"))
  (should (equal (johnson-db-normalize "über") "uber")))

(ert-deftest johnson-db-test-normalize-nfkd ()
  "Performs NFKD decomposition."
  ;; ﬁ (U+FB01 LATIN SMALL LIGATURE FI) decomposes to "fi" under NFKD.
  (should (equal (johnson-db-normalize "ﬁnd") "find")))

(ert-deftest johnson-db-test-normalize-nil-and-empty ()
  "Returns empty string for nil and empty inputs."
  (should (equal (johnson-db-normalize nil) ""))
  (should (equal (johnson-db-normalize "") "")))

(ert-deftest johnson-db-test-normalize-ascii ()
  "ASCII stays unchanged (besides case)."
  (should (equal (johnson-db-normalize "APPLE") "apple"))
  (should (equal (johnson-db-normalize "test123") "test123")))

;;;; johnson-db-open

(ert-deftest johnson-db-test-open-creates-tables ()
  "Opening a database creates the required tables and index."
  (johnson-db-test--with-temp-db
    ;; If tables don't exist, these queries would error.
    (should (sqlite-select db "SELECT COUNT(*) FROM metadata"))
    (should (sqlite-select db "SELECT COUNT(*) FROM entries"))))

(ert-deftest johnson-db-test-open-creates-index-file ()
  "Opening a database creates a file in the cache directory."
  (johnson-db-test--with-temp-cache
    (let* ((dict-path "/tmp/johnson-test-dummy.dsl")
           (db (johnson-db-open dict-path)))
      (unwind-protect
          (should (file-exists-p (johnson-db--index-path dict-path)))
        (johnson-db-close db)))))

;;;; Metadata

(ert-deftest johnson-db-test-metadata-set-and-get ()
  "Setting and getting metadata round-trips correctly."
  (johnson-db-test--with-temp-db
    (johnson-db-set-metadata db "name" "Test Dict")
    (should (equal (johnson-db-get-metadata db "name") "Test Dict"))))

(ert-deftest johnson-db-test-metadata-get-missing ()
  "Getting nonexistent metadata returns nil."
  (johnson-db-test--with-temp-db
    (should (null (johnson-db-get-metadata db "nonexistent")))))

(ert-deftest johnson-db-test-metadata-overwrite ()
  "Setting a key twice overwrites the previous value."
  (johnson-db-test--with-temp-db
    (johnson-db-set-metadata db "version" "1")
    (johnson-db-set-metadata db "version" "2")
    (should (equal (johnson-db-get-metadata db "version") "2"))))

(ert-deftest johnson-db-test-metadata-get-all ()
  "Getting all metadata returns the correct alist."
  (johnson-db-test--with-temp-db
    (johnson-db-set-metadata db "name" "TestDict")
    (johnson-db-set-metadata db "format" "dsl")
    (let ((all (johnson-db-get-all-metadata db)))
      (should (equal (cdr (assoc "name" all)) "TestDict"))
      (should (equal (cdr (assoc "format" all)) "dsl")))))

;;;; Entry insertion and query

(ert-deftest johnson-db-test-insert-entry-and-query ()
  "Insert a single entry and query it back."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entry db "apple" 100 50)
    (let ((results (johnson-db-query-exact db "apple")))
      (should (= (length results) 1))
      (should (equal (car (nth 0 results)) "apple"))
      (should (= (nth 1 (nth 0 results)) 100))
      (should (= (nth 2 (nth 0 results)) 50)))))

(ert-deftest johnson-db-test-insert-entries-batch ()
  "Batch insert entries and verify they are queryable."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entries-batch
     db '(("apple" 100 50) ("banana" 200 60) ("cherry" 300 70)))
    (should (= (length (johnson-db-query-exact db "apple")) 1))
    (should (= (length (johnson-db-query-exact db "banana")) 1))
    (should (= (length (johnson-db-query-exact db "cherry")) 1))))

(ert-deftest johnson-db-test-query-exact-normalized ()
  "Exact query normalizes the search term."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entry db "café" 100 50)
    ;; Querying "cafe" (no accent) should match "café".
    (let ((results (johnson-db-query-exact db "cafe")))
      (should (= (length results) 1))
      (should (equal (car (nth 0 results)) "café")))))

(ert-deftest johnson-db-test-query-exact-case-insensitive ()
  "Exact query is case-insensitive."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entry db "Apple" 100 50)
    (should (= (length (johnson-db-query-exact db "apple")) 1))
    (should (= (length (johnson-db-query-exact db "APPLE")) 1))))

(ert-deftest johnson-db-test-query-exact-returns-triples ()
  "Exact query returns (HEADWORD BYTE-OFFSET BYTE-LENGTH) triples."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entry db "test" 42 99)
    (let ((result (car (johnson-db-query-exact db "test"))))
      (should (equal (nth 0 result) "test"))
      (should (= (nth 1 result) 42))
      (should (= (nth 2 result) 99)))))

;;;; Prefix query

(ert-deftest johnson-db-test-query-prefix-basic ()
  "Prefix query returns distinct headwords matching the prefix."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entries-batch
     db '(("apple" 100 50) ("application" 200 60) ("banana" 300 70)))
    (let ((results (johnson-db-query-prefix db "app")))
      (should (= (length results) 2))
      (should (member "apple" results))
      (should (member "application" results))
      (should-not (member "banana" results)))))

(ert-deftest johnson-db-test-query-prefix-limit ()
  "Prefix query respects the LIMIT parameter."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entries-batch
     db '(("aaa" 100 10) ("aab" 200 10) ("aac" 300 10) ("aad" 400 10)))
    (let ((results (johnson-db-query-prefix db "a" 2)))
      (should (= (length results) 2)))))

(ert-deftest johnson-db-test-query-prefix-returns-strings ()
  "Prefix query returns a list of headword strings."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entry db "dog" 100 50)
    (let ((results (johnson-db-query-prefix db "d")))
      (should (stringp (car results))))))

;;;; Entry count

(ert-deftest johnson-db-test-entry-count ()
  "Entry count returns the correct number of entries."
  (johnson-db-test--with-temp-db
    (should (= (johnson-db-entry-count db) 0))
    (johnson-db-insert-entries-batch
     db '(("a" 10 5) ("b" 20 5) ("c" 30 5)))
    (should (= (johnson-db-entry-count db) 3))))

;;;; Staleness detection

(ert-deftest johnson-db-test-stale-missing-index ()
  "Returns non-nil when the index file does not exist."
  (johnson-db-test--with-temp-cache
    (should (johnson-db-stale-p "/tmp/johnson-nonexistent-dict.dsl"))))

(ert-deftest johnson-db-test-stale-mismatched-mtime ()
  "Returns non-nil when the stored mtime does not match the file."
  (johnson-db-test--with-temp-cache
    (let* ((dict-file (make-temp-file "johnson-stale-test-" nil ".dsl"))
           (db (johnson-db-open dict-file)))
      (unwind-protect
          (progn
            ;; Store a fake mtime that won't match.
            (johnson-db-set-metadata db "mtime" "0")
            (johnson-db-close db)
            (should (johnson-db-stale-p dict-file)))
        (delete-file dict-file)))))

(ert-deftest johnson-db-test-stale-matching-mtime ()
  "Returns nil when the stored mtime matches the actual file."
  (johnson-db-test--with-temp-cache
    (let* ((dict-file (make-temp-file "johnson-stale-test-" nil ".dsl"))
           (db (johnson-db-open dict-file)))
      (unwind-protect
          (progn
            (johnson-db-insert-entry db "test" 0 4)
            (let ((actual-mtime (format-time-string
                                 "%s"
                                 (file-attribute-modification-time
                                  (file-attributes dict-file)))))
              (johnson-db-set-metadata db "mtime" actual-mtime)
              (johnson-db-close db)
              (should-not (johnson-db-stale-p dict-file))))
        (delete-file dict-file)))))

;;;; Reset

(ert-deftest johnson-db-test-reset-clears-entries ()
  "Reset clears all entries."
  (johnson-db-test--with-temp-db
    (johnson-db-insert-entries-batch
     db '(("a" 10 5) ("b" 20 5)))
    (should (= (johnson-db-entry-count db) 2))
    (johnson-db-reset db)
    (should (= (johnson-db-entry-count db) 0))))

(ert-deftest johnson-db-test-reset-preserves-metadata ()
  "Reset does not clear metadata."
  (johnson-db-test--with-temp-db
    (johnson-db-set-metadata db "name" "TestDict")
    (johnson-db-insert-entry db "word" 10 5)
    (johnson-db-reset db)
    (should (equal (johnson-db-get-metadata db "name") "TestDict"))))

;;;; Unified completion index

(ert-deftest johnson-db-test-completion-index-path ()
  "Completion index path is inside the cache directory."
  (johnson-db-test--with-temp-cache
    (should (string-prefix-p (expand-file-name johnson-cache-directory)
                             (johnson-db-completion-index-path)))))

(ert-deftest johnson-db-test-get-completion-db-nil-when-missing ()
  "Returns nil when no completion index exists yet."
  (johnson-db-test--with-temp-cache
    (let ((johnson-db--completion-db nil))
      (should (null (johnson-db-get-completion-db))))))

(ert-deftest johnson-db-test-rebuild-completion-index ()
  "Rebuilding aggregates headwords from multiple dictionaries."
  (johnson-db-test--with-temp-cache
    (let* ((dict1 "/tmp/johnson-comp-test-1.dsl")
           (dict2 "/tmp/johnson-comp-test-2.dsl")
           (db1 (johnson-db-open dict1))
           (db2 (johnson-db-open dict2)))
      (unwind-protect
          (progn
            (johnson-db-insert-entries-batch
             db1 '(("apple" 100 50) ("banana" 200 60)))
            (johnson-db-insert-entries-batch
             db2 '(("apple" 300 50) ("cherry" 400 70)))
            (let ((count (johnson-db-rebuild-completion-index
                          (list dict1 dict2))))
              (should (= count 3))  ; apple, banana, cherry
              ;; Open and query the built index.
              (let ((comp-db (johnson-db-get-completion-db)))
                (unwind-protect
                    (let ((results (johnson-db-query-completion comp-db "app")))
                      (should (= (length results) 1))
                      (should (equal (caar results) "apple"))
                      ;; apple appears in 2 dictionaries.
                      (should (= (cadr (car results)) 2)))
                  (johnson-db-close-completion-db)))))
        (johnson-db-close db1)
        (johnson-db-close db2)))))

(ert-deftest johnson-db-test-rebuild-completion-index-case-variants ()
  "Dict count aggregates across case variants of the same normalized form."
  (johnson-db-test--with-temp-cache
    (let* ((dict1 "/tmp/johnson-comp-case-1.dsl")
           (dict2 "/tmp/johnson-comp-case-2.dsl")
           (dict3 "/tmp/johnson-comp-case-3.dsl")
           (db1 (johnson-db-open dict1))
           (db2 (johnson-db-open dict2))
           (db3 (johnson-db-open dict3)))
      (unwind-protect
          (progn
            ;; dict1 has "Beatrice", dict2 has "beatrice", dict3 has "BEATRICE"
            (johnson-db-insert-entry db1 "Beatrice" 100 50)
            (johnson-db-insert-entry db2 "beatrice" 200 60)
            (johnson-db-insert-entry db3 "BEATRICE" 300 70)
            (let ((count (johnson-db-rebuild-completion-index
                          (list dict1 dict2 dict3))))
              (should (= count 3))  ; 3 distinct headword strings
              (let ((comp-db (johnson-db-get-completion-db)))
                (unwind-protect
                    (let ((results (johnson-db-query-completion comp-db "beatrice")))
                      ;; All three variants should appear.
                      (should (= (length results) 3))
                      ;; Each variant should report dict_count = 3
                      ;; (all 3 dicts have the same normalized form).
                      (dolist (row results)
                        (should (= (cadr row) 3))))
                  (johnson-db-close-completion-db)))))
        (johnson-db-close db1)
        (johnson-db-close db2)
        (johnson-db-close db3)))))

(ert-deftest johnson-db-test-rebuild-completion-index-empty ()
  "Rebuilding with no dictionaries produces zero headwords."
  (johnson-db-test--with-temp-cache
    (let ((count (johnson-db-rebuild-completion-index nil)))
      (should (= count 0))
      (let ((comp-db (johnson-db-get-completion-db)))
        (unwind-protect
            (should (null (johnson-db-query-completion comp-db "a")))
          (johnson-db-close-completion-db))))))

(ert-deftest johnson-db-test-close-completion-db ()
  "Closing clears the cached connection."
  (johnson-db-test--with-temp-cache
    (johnson-db-rebuild-completion-index nil)
    (should (johnson-db-get-completion-db))
    (johnson-db-close-completion-db)
    (should (null johnson-db--completion-db))))

(provide 'johnson-db-test)
;;; johnson-db-test.el ends here
