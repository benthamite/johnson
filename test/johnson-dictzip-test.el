;;; johnson-dictzip-test.el --- Tests for johnson-dictzip -*- lexical-binding: t; -*-

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
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 0 5)))
      (should (equal data "Hello")))))

(ert-deftest johnson-dictzip-test-read-within-chunk ()
  "Reads a range within a single chunk."
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 7 7)))
      (should (equal data "dictzip")))))

(ert-deftest johnson-dictzip-test-read-across-chunks ()
  "Reads a range spanning chunk boundaries."
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
  (johnson-dictzip-test--with-clean-cache
    ;; "Second" starts at byte 41
    (let ((data (johnson-dictzip-read
                 (johnson-dictzip-test--fixture "test.dict.dz")
                 41 6)))
      (should (equal data "Second")))))

(ert-deftest johnson-dictzip-test-read-last-chunk ()
  "Reads from the last chunk."
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
  (johnson-dictzip-test--with-clean-cache
    (let ((data (johnson-dictzip-read-full
                 (johnson-dictzip-test--fixture "test.dict.dz"))))
      (should (= (length data) 131))
      (should (string-prefix-p "Hello, dictzip world!" data))
      (should (string-suffix-p "boundaries.\n" data)))))

(ert-deftest johnson-dictzip-test-read-full-matches-read ()
  "Full read matches concatenation of individual reads."
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

(provide 'johnson-dictzip-test)
;;; johnson-dictzip-test.el ends here
