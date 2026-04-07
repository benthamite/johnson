;;; johnson-epwing-test.el --- Tests for johnson-epwing -*- lexical-binding: t; -*-

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

;; ERT tests for the johnson-epwing format backend module.
;; Fixtures are synthetic binary EPWING files in test/fixtures/test-epwing/.

;;; Code:

(require 'ert)
(require 'johnson-epwing)
(require 'johnson)

;;;; Helpers

(defvar johnson-epwing-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-epwing-test--fixture (name)
  "Return the full path to the fixture file NAME."
  (expand-file-name name johnson-epwing-test--fixtures-dir))

(defun johnson-epwing-test--catalogs ()
  "Return the path to the test CATALOGS file."
  (johnson-epwing-test--fixture "test-epwing/CATALOGS"))

(defun johnson-epwing-test--honmon ()
  "Return the path to the test HONMON file."
  (johnson-epwing-test--fixture "test-epwing/TESTBOOK/DATA/HONMON"))

(defun johnson-epwing-test--cleanup ()
  "Clean up EPWING caches."
  (johnson-epwing-clear-caches))

;;;; Binary helpers

(ert-deftest johnson-epwing-test-u16be ()
  "Reads 16-bit big-endian integers correctly."
  (should (= (johnson-epwing--u16be (unibyte-string #x00 #x05) 0) 5))
  (should (= (johnson-epwing--u16be (unibyte-string #x01 #x00) 0) 256)))

(ert-deftest johnson-epwing-test-u32be ()
  "Reads 32-bit big-endian integers correctly."
  (should (= (johnson-epwing--u32be (unibyte-string 0 0 0 1) 0) 1))
  (should (= (johnson-epwing--u32be (unibyte-string 0 1 0 0) 0) 65536)))

;;;; Page addressing

(ert-deftest johnson-epwing-test-page-pos ()
  "Converts page+offset to absolute file position."
  (should (= (johnson-epwing--page-pos 1 0) 0))
  (should (= (johnson-epwing--page-pos 2 0) 2048))
  (should (= (johnson-epwing--page-pos 3 100) 4196)))

;;;; Escape sequence length

(ert-deftest johnson-epwing-test-escape-length ()
  "Returns correct lengths for escape sequences."
  (let ((data (unibyte-string #x1F #x02)))
    (should (= (johnson-epwing--escape-length data 0) 2)))
  (let ((data (unibyte-string #x1F #xE0 #x00 #x01)))
    (should (= (johnson-epwing--escape-length data 0) 4)))
  (let ((data (unibyte-string #x1F #x62 0 0 0 0 0 0)))
    (should (= (johnson-epwing--escape-length data 0) 8))))

;;;; Directory helpers

(ert-deftest johnson-epwing-test-find-child ()
  "Finds files case-insensitively in a directory."
  (let ((dir (johnson-epwing-test--fixture "test-epwing/")))
    (should (johnson-epwing--find-child dir "CATALOGS"))
    (should (johnson-epwing--find-child dir "TESTBOOK"))
    (should-not (johnson-epwing--find-child dir "NONEXISTENT"))))

;;;; CATALOGS parsing

(ert-deftest johnson-epwing-test-find-catalogs ()
  "Finds the CATALOGS file from a HONMON path."
  (let ((result (johnson-epwing--find-catalogs
                 (johnson-epwing-test--honmon))))
    (should result)
    (should (string-equal-ignore-case
             (file-name-nondirectory result) "CATALOGS"))))

(ert-deftest johnson-epwing-test-parse-catalogs ()
  "Parses CATALOGS file correctly."
  (let ((subbooks (johnson-epwing--parse-catalogs
                   (johnson-epwing-test--catalogs))))
    (should (= (length subbooks) 1))
    (should (equal (plist-get (car subbooks) :title) "Test Dictionary"))
    (should (equal (plist-get (car subbooks) :directory) "TESTBOOK"))
    (should (= (plist-get (car subbooks) :index-page) 1))))

;;;; Format detection

(ert-deftest johnson-epwing-test-detect-accepts-honmon ()
  "Accepts a valid EPWING HONMON file."
  (should (johnson-epwing-detect (johnson-epwing-test--honmon))))

(ert-deftest johnson-epwing-test-detect-rejects-non-epwing ()
  "Rejects a file that is not an EPWING HONMON file."
  (should-not (johnson-epwing-detect
               (johnson-epwing-test--fixture "test-bgl.bgl"))))

;;;; Metadata parsing

(ert-deftest johnson-epwing-test-parse-metadata ()
  "Parses metadata from HONMON file."
  (let ((meta (johnson-epwing-parse-metadata
               (johnson-epwing-test--honmon))))
    (should (equal (plist-get meta :name) "Test Dictionary"))))

;;;; Index management

(ert-deftest johnson-epwing-test-read-index-table ()
  "Reads the index management page correctly."
  (let ((table (johnson-epwing--read-index-table
                (johnson-epwing-test--honmon) 1)))
    (should (= (length table) 2))
    (should (= (plist-get (nth 0 table) :id) #x00))
    (should (= (plist-get (nth 0 table) :start-page) 3))
    (should (= (plist-get (nth 1 table) :id) #x71))
    (should (= (plist-get (nth 1 table) :start-page) 2))))

(ert-deftest johnson-epwing-test-find-search-index ()
  "Finds the search index (word search preferred)."
  (let* ((table (johnson-epwing--read-index-table
                 (johnson-epwing-test--honmon) 1))
         (idx (johnson-epwing--find-search-index table)))
    (should idx)
    (should (= (plist-get idx :id) #x71))
    (should (= (plist-get idx :start-page) 2))))

;;;; Index building

(ert-deftest johnson-epwing-test-build-index ()
  "Builds index with correct headwords and offsets."
  (unwind-protect
      (let ((entries nil))
        (johnson-epwing-build-index
         (johnson-epwing-test--honmon)
         (lambda (hw off len)
           (push (list hw off len) entries)))
        (setq entries (nreverse entries))
        (should (= (length entries) 3))
        (should (equal (nth 0 (nth 0 entries)) "apple"))
        (should (equal (nth 0 (nth 1 entries)) "banana"))
        (should (equal (nth 0 (nth 2 entries)) "cherry"))
        ;; Offsets should point to page 3
        (should (= (nth 1 (nth 0 entries)) 4096))
        (should (= (nth 1 (nth 1 entries)) 4196))
        (should (= (nth 1 (nth 2 entries)) 4296))
        ;; Lengths should be 0
        (should (= (nth 2 (nth 0 entries)) 0)))
    (johnson-epwing-test--cleanup)))

;;;; Entry retrieval

(ert-deftest johnson-epwing-test-retrieve-entry-apple ()
  "Retrieves and decodes the apple entry."
  (unwind-protect
      (let ((text (johnson-epwing-retrieve-entry
                   (johnson-epwing-test--honmon) 4096 0)))
        (should (stringp text))
        (should (string-match-p "A round fruit" text)))
    (johnson-epwing-test--cleanup)))

(ert-deftest johnson-epwing-test-retrieve-entry-banana ()
  "Retrieves and decodes the banana entry with bold markup."
  (unwind-protect
      (let ((text (johnson-epwing-retrieve-entry
                   (johnson-epwing-test--honmon) 4196 0)))
        (should (stringp text))
        (should (string-match-p "banana" text))
        (should (string-match-p "A yellow fruit" text)))
    (johnson-epwing-test--cleanup)))

;;;; Entry rendering

(ert-deftest johnson-epwing-test-render-entry-apple ()
  "Renders the apple entry as plain text."
  (unwind-protect
      (let ((text (johnson-epwing-retrieve-entry
                   (johnson-epwing-test--honmon) 4096 0)))
        (with-temp-buffer
          (johnson-epwing-render-entry text)
          (should (string-match-p "A round fruit"
                                  (buffer-string)))))
    (johnson-epwing-test--cleanup)))

(ert-deftest johnson-epwing-test-render-entry-banana-bold ()
  "Renders the banana entry with bold face on the word."
  (unwind-protect
      (let ((text (johnson-epwing-retrieve-entry
                   (johnson-epwing-test--honmon) 4196 0)))
        (with-temp-buffer
          (johnson-epwing-render-entry text)
          (let ((content (buffer-string)))
            (should (string-match-p "banana" content))
            (should (string-match-p "A yellow fruit" content))
            ;; Check bold face on "banana"
            (goto-char (point-min))
            (search-forward "banana")
            (should (eq (get-text-property (- (point) 1) 'face)
                        'johnson-bold-face)))))
    (johnson-epwing-test--cleanup)))

(ert-deftest johnson-epwing-test-render-entry-banana-newline ()
  "Renders the banana entry with a newline between parts."
  (unwind-protect
      (let ((text (johnson-epwing-retrieve-entry
                   (johnson-epwing-test--honmon) 4196 0)))
        (with-temp-buffer
          (johnson-epwing-render-entry text)
          (should (string-match-p "banana\nA yellow fruit"
                                  (buffer-string)))))
    (johnson-epwing-test--cleanup)))

;;;; Format registration

(ert-deftest johnson-epwing-test-format-registered ()
  "The EPWING format is registered with johnson."
  (require 'johnson)
  (let ((fmt (cl-find-if
              (lambda (f) (equal (plist-get f :name) "epwing"))
              johnson--formats)))
    (should fmt)
    (should (null (plist-get fmt :extensions)))
    (should (plist-get fmt :discover))
    (should (eq (plist-get fmt :detect) #'johnson-epwing-detect))
    (should (eq (plist-get fmt :retrieve-entry)
                #'johnson-epwing-retrieve-entry))
    (should (eq (plist-get fmt :render-entry)
                #'johnson-epwing-render-entry))))

;;;; Full round-trip

(ert-deftest johnson-epwing-test-round-trip ()
  "Full round-trip: detect, metadata, index, retrieve, render."
  (unwind-protect
      (let ((honmon (johnson-epwing-test--honmon)))
        ;; Detection
        (should (johnson-epwing-detect honmon))
        ;; Metadata
        (let ((meta (johnson-epwing-parse-metadata honmon)))
          (should (equal (plist-get meta :name) "Test Dictionary")))
        ;; Index
        (let ((entries nil))
          (johnson-epwing-build-index
           honmon
           (lambda (hw off len)
             (push (list hw off len) entries)))
          (setq entries (nreverse entries))
          (should (= (length entries) 3))
          ;; Retrieve and render each entry
          (dolist (entry entries)
            (let ((raw (johnson-epwing-retrieve-entry
                        honmon (nth 1 entry) (nth 2 entry))))
              (should (stringp raw))
              (with-temp-buffer
                (johnson-epwing-render-entry raw)
                (should (> (buffer-size) 0)))))))
    (johnson-epwing-test--cleanup)))

(provide 'johnson-epwing-test)
;;; johnson-epwing-test.el ends here
