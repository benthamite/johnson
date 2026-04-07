;;; johnson-fts-test.el --- Tests for full-text search -*- lexical-binding: t; -*-

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

;; ERT tests for johnson full-text search (FTS) functionality.

;;; Code:

(require 'ert)
(require 'johnson-db)

;;;; Helpers

(defmacro johnson-fts-test--with-db (&rest body)
  "Execute BODY with a temporary database.
Binds `db' to an open sqlite connection with FTS tables."
  (declare (indent 0) (debug t))
  `(let* ((temp-dir (make-temp-file "johnson-fts-test-" t))
          (johnson-cache-directory temp-dir)
          (db (johnson-db-open "/tmp/johnson-fts-test.dsl")))
     (unwind-protect
         (progn ,@body)
       (johnson-db-close db)
       (delete-directory temp-dir t))))

;;;; FTS table creation

(ert-deftest johnson-fts-test-table-exists ()
  "FTS virtual table is created by `johnson-db-open'."
  (johnson-fts-test--with-db
    ;; Should not error — table exists.
    (should (johnson-db-insert-fts db "test" "test definition"))))

;;;; FTS insertion and query

(ert-deftest johnson-fts-test-insert-and-query ()
  "Insert and query FTS entries."
  (johnson-fts-test--with-db
    (johnson-db-insert-fts db "apple" "a round fruit with red skin")
    (johnson-db-insert-fts db "banana" "a yellow curved fruit")
    (johnson-db-insert-fts db "cat" "a small feline animal")
    (let ((results (johnson-db-query-fts db "fruit")))
      (should (= (length results) 2))
      (let ((headwords (mapcar #'car results)))
        (should (member "apple" headwords))
        (should (member "banana" headwords))))))

(ert-deftest johnson-fts-test-query-with-snippet ()
  "FTS query returns snippets with context markers."
  (johnson-fts-test--with-db
    (johnson-db-insert-fts db "hello" "a common greeting used worldwide")
    (let* ((results (johnson-db-query-fts db "greeting"))
           (snippet (cadar results)))
      (should (stringp snippet))
      (should (string-match-p "greeting" snippet)))))

(ert-deftest johnson-fts-test-query-no-results ()
  "FTS query returns nil for unmatched terms."
  (johnson-fts-test--with-db
    (johnson-db-insert-fts db "hello" "a common greeting")
    (should (null (johnson-db-query-fts db "xyznonexistent")))))

;;;; FTS metadata

(ert-deftest johnson-fts-test-indexed-flag ()
  "FTS indexed flag is set and read correctly."
  (johnson-fts-test--with-db
    (should-not (johnson-db-fts-indexed-p db))
    (johnson-db-set-fts-indexed db)
    (should (johnson-db-fts-indexed-p db))))

;;;; Wildcard query

(ert-deftest johnson-fts-test-wildcard-query ()
  "Wildcard query matches patterns."
  (johnson-fts-test--with-db
    (johnson-db-insert-entry db "apple" 0 10)
    (johnson-db-insert-entry db "application" 10 20)
    (johnson-db-insert-entry db "banana" 30 15)
    (let ((results (johnson-db-query-wildcard db "app*")))
      (should (= (length results) 2))
      (should (member "apple" results))
      (should (member "application" results)))))

(ert-deftest johnson-fts-test-wildcard-single-char ()
  "Wildcard ? matches single character."
  (johnson-fts-test--with-db
    (johnson-db-insert-entry db "cat" 0 10)
    (johnson-db-insert-entry db "car" 10 10)
    (johnson-db-insert-entry db "cap" 20 10)
    (johnson-db-insert-entry db "cart" 30 10)
    (let ((results (johnson-db-query-wildcard db "ca?")))
      (should (= (length results) 3))
      (should (member "cat" results))
      (should (member "car" results))
      (should (member "cap" results))
      (should-not (member "cart" results)))))

(provide 'johnson-fts-test)
;;; johnson-fts-test.el ends here
