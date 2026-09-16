;;; johnson-dictzip-test.el --- Tests for johnson-dictzip -*- lexical-binding: t; -*-

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

;; ERT tests for the johnson-dictzip module.

;;; Code:

(require 'ert)
(require 'johnson-dictzip)

;;;; Helpers

(defvar johnson-dictzip-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-dictzip-test--fixture (name)
  "Return the full path to the fixture file NAME."
  (expand-file-name name johnson-dictzip-test--fixtures-dir))

(defmacro johnson-dictzip-test--with-clean-cache (&rest body)
  "Execute BODY with clean dictzip caches."
  (declare (indent 0) (debug t))
  `(let ((johnson-dictzip--header-cache (make-hash-table :test #'equal))
         (johnson-dictzip--chunk-cache nil))
     ,@body))

;; The test fixture test.dict.dz contains:
;; Payload (131 bytes, chunk_size=50, 3 chunks):
;;   "Hello, dictzip world! This is entry one.\n"        (0-40, 41 bytes)
;;   "Second entry content here.\n"                      (41-68, 28 bytes)
;;   "Third entry for testing random access across chunk boundaries.\n"
;;                                                        (69-131, 62 bytes)

;;;; Header parsing

(ert-deftest johnson-dictzip-test-parse-header ()
  "Parses dictzip header and extracts RA fields."
  (johnson-dictzip-test--with-clean-cache
    (let ((header (johnson-dictzip--parse-header
                   (johnson-dictzip-test--fixture "test.dict.dz"))))
      (should (= (plist-get header :chlen) 50))
      (should (= (plist-get header :chcnt) 3))
      (should (vectorp (plist-get header :chunk-sizes)))
      (should (= (length (plist-get header :chunk-sizes)) 3))
      (should (integerp (plist-get header :data-offset))))))

(ert-deftest johnson-dictzip-test-header-cache ()
  "Header is cached after first parse."
  (johnson-dictzip-test--with-clean-cache
    (let ((path (johnson-dictzip-test--fixture "test.dict.dz")))
      (johnson-dictzip--parse-header path)
      (should (gethash path johnson-dictzip--header-cache))
      ;; Second call returns the same object.
      (let ((h1 (johnson-dictzip--parse-header path))
            (h2 (johnson-dictzip--parse-header path)))
        (should (eq h1 h2))))))

(ert-deftest johnson-dictzip-test-invalid-file ()
  "Signals error for non-gzip files."
  (johnson-dictzip-test--with-clean-cache
    (let ((tmp (make-temp-file "dictzip-test-" nil ".bin"))
          (auto-compression-mode nil))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'binary))
              (with-temp-file tmp (insert "not a gzip file")))
            (should-error (johnson-dictzip--parse-header tmp)))
        (delete-file tmp)))))

;;;; Random-access read

(ert-deftest johnson-dictzip-test-read-beginning ()
  "Reads from the start of the file."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 0 5)))
      (should (equal data "Hello")))))

(ert-deftest johnson-dictzip-test-read-within-chunk ()
  "Reads a range within a single chunk."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 7 7)))
      (should (equal data "dictzip")))))

(ert-deftest johnson-dictzip-test-read-across-chunks ()
  "Reads a range spanning chunk boundaries."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    ;; chunk_size=50, so chunk 0=[0,49], chunk 1=[50,99]
    ;; Read across boundary: bytes 45-55 (11 bytes)
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 45 15)))
      (should (= (length data) 15))
      (should (stringp data)))))

(ert-deftest johnson-dictzip-test-read-second-chunk ()
  "Reads from the second chunk."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    ;; "Second" starts at byte 41
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 41 6)))
      (should (equal data "Second")))))

(ert-deftest johnson-dictzip-test-read-last-chunk ()
  "Reads from the last chunk."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    ;; "boundaries" appears near the end
    (let* ((full (johnson-dictzip-read-full
                  (johnson-dictzip-test--fixture "test.dict.dz")))
           (pos (string-search "boundaries" full)))
      (should pos)
      (let ((data (johnson-dictzip-read
                   (johnson-dictzip-test--fixture "test.dict.dz")
                   pos 10)))
        (should (equal data "boundaries"))))))

;;;; Full decompression

(ert-deftest johnson-dictzip-test-read-full ()
  "Reads the entire uncompressed content."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read-full
                 (johnson-dictzip-test--fixture "test.dict.dz"))))
      (should (= (length data) 131))
      (should (string-prefix-p "Hello, dictzip world!" data))
      (should (string-suffix-p "boundaries.\n" data)))))

(ert-deftest johnson-dictzip-test-read-full-matches-read ()
  "Full read matches concatenation of individual reads."
  (skip-unless (fboundp 'zlib-decompress-region))
  (johnson-dictzip-test--with-clean-cache
    (let* ((path (johnson-dictzip-test--fixture "test.dict.dz"))
           (full (johnson-dictzip-read-full path))
           ;; Read in 20-byte slices
           (parts nil)
           (pos 0))
      (while (< pos (length full))
        (let ((len (min 20 (- (length full) pos))))
          (push (johnson-dictzip-read path pos len) parts)
          (cl-incf pos len)))
      (should (equal (apply #'concat (nreverse parts)) full)))))

;;;; Chunk cache

(ert-deftest johnson-dictzip-test-chunk-cache-lru ()
  "Chunk cache evicts oldest entries when full."
  (let ((johnson-dictzip--chunk-cache nil))
    (dotimes (i 10)
      (johnson-dictzip--chunk-cache-put (cons "test" i) (format "data-%d" i)))
    ;; Cache should contain at most johnson-dictzip--chunk-cache-size entries.
    (should (<= (length johnson-dictzip--chunk-cache)
                johnson-dictzip--chunk-cache-size))
    ;; Most recent entries should still be cached.
    (should (johnson-dictzip--chunk-cache-get (cons "test" 9)))
    ;; Oldest entries should have been evicted.
    (should-not (johnson-dictzip--chunk-cache-get (cons "test" 0)))))

;;;; Binary integer helpers

(ert-deftest johnson-dictzip-test-u16le ()
  "Reads u16le correctly."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x34 #x12)
    (should (= (johnson-dictzip--u16le 1) #x1234))))

(ert-deftest johnson-dictzip-test-u16be ()
  "Reads u16be correctly."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x12 #x34)
    (should (= (johnson-dictzip--u16be 1) #x1234))))

(ert-deftest johnson-dictzip-test-u32be ()
  "Reads u32be correctly."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x12 #x34 #x56 #x78)
    (should (= (johnson-dictzip--u32be 1) #x12345678))))

;;;; Corrupt and oversized input

(defun johnson-dictzip-test--write-bytes (bytes)
  "Write the unibyte string BYTES to a new temporary file and return its path."
  (let ((path (make-temp-file "johnson-dictzip-test-" nil ".dict.dz"))
        (coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert bytes))
    path))

(defun johnson-dictzip-test--file-bytes (path &optional end)
  "Return the first END bytes of PATH, or the whole file, as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 end)
    (buffer-string)))

(defun johnson-dictzip-test--u16le (n)
  "Return N encoded as two little-endian bytes."
  (unibyte-string (logand n #xff) (logand (ash n -8) #xff)))

(defun johnson-dictzip-test--header-bytes (chcnt)
  "Return a gzip header whose dictzip RA field declares CHCNT chunks."
  (let* ((ra (concat (johnson-dictzip-test--u16le 1)
                     (johnson-dictzip-test--u16le 58315)
                     (johnson-dictzip-test--u16le chcnt)
                     (apply #'concat
                            (make-list chcnt
                                       (johnson-dictzip-test--u16le 100)))))
         (subfield (concat "RA" (johnson-dictzip-test--u16le (length ra)) ra)))
    (concat (unibyte-string #x1f #x8b 8 4 0 0 0 0 0 3)
            (johnson-dictzip-test--u16le (length subfield))
            subfield)))

(ert-deftest johnson-dictzip-test-truncated-chunk-errors ()
  "A chunk cut off mid-stream signals an error and is not cached."
  (johnson-dictzip-test--with-clean-cache
    (let* ((fixture (johnson-dictzip-test--fixture "test.dict.dz"))
           (header (johnson-dictzip--parse-header fixture))
           (cut (+ (plist-get header :data-offset)
                   (/ (aref (plist-get header :chunk-sizes) 0) 2)))
           (path (johnson-dictzip-test--write-bytes
                  (johnson-dictzip-test--file-bytes fixture cut))))
      (unwind-protect
          (progn
            (let ((err (should-error (johnson-dictzip-read path 0 20))))
              (should (string-match-p "Truncated or corrupt dictzip chunk 0"
                                      (cadr err))))
            (should-error (johnson-dictzip-read-full path))
            (should-not (johnson-dictzip--chunk-cache-get (cons path 0))))
        (delete-file path)))))

(ert-deftest johnson-dictzip-test-short-chunk-errors ()
  "A chunk that inflates to less than the declared chunk length errors."
  (johnson-dictzip-test--with-clean-cache
    (let* ((bytes (johnson-dictzip-test--file-bytes
                   (johnson-dictzip-test--fixture "test.dict.dz")))
           ;; chlen follows SI1 SI2 LEN VER in the RA subfield.
           (chlen-pos (+ (string-match "RA" bytes) 6)))
      (should (= (aref bytes chlen-pos) 50))
      (aset bytes chlen-pos 60)
      (let ((path (johnson-dictzip-test--write-bytes bytes)))
        (unwind-protect
            (let ((err (should-error (johnson-dictzip-read path 0 20))))
              (should (string-match-p "Truncated or corrupt dictzip chunk 0"
                                      (cadr err))))
          (delete-file path))))))

(ert-deftest johnson-dictzip-test-parse-header-maximal-fextra ()
  "A header whose FEXTRA field is near the 64 KiB gzip limit parses."
  (johnson-dictzip-test--with-clean-cache
    (let* ((header-bytes (johnson-dictzip-test--header-bytes 32760))
           (path (johnson-dictzip-test--write-bytes
                  (concat header-bytes (make-string 200000 0)))))
      (unwind-protect
          (let ((header (johnson-dictzip--parse-header path)))
            (should (= (plist-get header :chcnt) 32760))
            (should (= (plist-get header :data-offset) (length header-bytes))))
        (delete-file path)))))

(ert-deftest johnson-dictzip-test-parse-header-truncated-fextra ()
  "A header cut off inside its FEXTRA field reports a truncated header."
  (johnson-dictzip-test--with-clean-cache
    (let ((path (johnson-dictzip-test--write-bytes
                 (substring (johnson-dictzip-test--header-bytes 100) 0 60))))
      (unwind-protect
          (let ((err (should-error (johnson-dictzip--parse-header path))))
            (should (string-match-p "Truncated dictzip header" (cadr err))))
        (delete-file path)))))

(provide 'johnson-dictzip-test)
;;; johnson-dictzip-test.el ends here
