;;; johnson-ebzip.el --- EBZIP decompression for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.5.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides random-access decompression for EBZIP-compressed
;; EPWING files (.ebz).  EBZIP divides uncompressed data into fixed-size
;; slices, each independently zlib-compressed.  A slice index at the
;; start of the file enables random access by mapping logical offsets to
;; compressed slice locations.

;;; Code:

(require 'cl-lib)

;;;; Constants

(defconst johnson-ebzip--header-size 22
  "Size of the EBZIP file header in bytes.")

(defconst johnson-ebzip--magic "EBZip"
  "Magic string at the start of EBZIP files.")

;;;; Binary helpers

(defsubst johnson-ebzip--u32be (data pos)
  "Read unsigned 32-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 24)
          (ash (aref data (1+ pos)) 16)
          (ash (aref data (+ pos 2)) 8)
          (aref data (+ pos 3))))

(defsubst johnson-ebzip--u40be (data pos)
  "Read unsigned 40-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 32)
          (ash (aref data (1+ pos)) 24)
          (ash (aref data (+ pos 2)) 16)
          (ash (aref data (+ pos 3)) 8)
          (aref data (+ pos 4))))

;;;; Header parsing

(defvar johnson-ebzip--header-cache (make-hash-table :test #'equal)
  "Cache mapping file paths to parsed EBZIP header plists.")

(defun johnson-ebzip--parse-header (path)
  "Parse the EBZIP header from file at PATH.
Returns a plist with keys :slice-size, :uncompressed-size,
:index-width, :slice-count, :index."
  (or (gethash path johnson-ebzip--header-cache)
      (let ((header (johnson-ebzip--do-parse-header path)))
        (puthash path header johnson-ebzip--header-cache)
        header)))

(defun johnson-ebzip--do-parse-header (path)
  "Parse the EBZIP header from PATH without caching."
  (let* ((raw (johnson-ebzip--read-raw path 0 johnson-ebzip--header-size))
         (magic (substring raw 0 5)))
    (unless (equal magic johnson-ebzip--magic)
      (error "Not an EBZIP file: %s" path))
    (let* ((zip-level (logand (aref raw 5) #x0F))
           (slice-size (ash 2048 zip-level))
           (file-size (johnson-ebzip--u40be raw 9))
           (slice-count (ceiling (float file-size) slice-size))
           (index-width (johnson-ebzip--index-entry-width file-size)))
      (list :slice-size slice-size
            :uncompressed-size file-size
            :index-width index-width
            :slice-count slice-count
            :index (johnson-ebzip--read-index
                    path slice-count index-width)))))

(defun johnson-ebzip--index-entry-width (file-size)
  "Return the byte width of index entries for FILE-SIZE."
  (cond
   ((< file-size (ash 1 16)) 2)
   ((< file-size (ash 1 24)) 3)
   ((< file-size (ash 1 32)) 4)
   (t 5)))

(defun johnson-ebzip--read-index (path slice-count index-width)
  "Read the slice index from PATH.
SLICE-COUNT is the number of slices.  INDEX-WIDTH is the byte
width per entry.  Returns a vector of (SLICE-COUNT + 1) offsets."
  (let* ((index-size (* (1+ slice-count) index-width))
         (raw (johnson-ebzip--read-raw
               path johnson-ebzip--header-size index-size))
         (offsets (make-vector (1+ slice-count) 0)))
    (dotimes (i (1+ slice-count))
      (aset offsets i
            (johnson-ebzip--read-index-entry
             raw (* i index-width) index-width)))
    offsets))

(defun johnson-ebzip--read-index-entry (data pos width)
  "Read an index entry of WIDTH bytes from DATA at POS."
  (pcase width
    (2 (logior (ash (aref data pos) 8) (aref data (1+ pos))))
    (3 (logior (ash (aref data pos) 16) (ash (aref data (1+ pos)) 8)
               (aref data (+ pos 2))))
    (4 (johnson-ebzip--u32be data pos))
    (5 (johnson-ebzip--u40be data pos))
    (_ (error "Unsupported EBZIP index width: %d" width))))

;;;; Raw file I/O

(defun johnson-ebzip--read-raw (path start length)
  "Read LENGTH bytes from PATH at START without decompression."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil start (+ start length))
    (buffer-string)))

;;;; Slice decompression

(defvar johnson-ebzip--slice-cache (make-hash-table :test #'equal)
  "LRU cache mapping (PATH . SLICE-NUM) to decompressed data.")

(defconst johnson-ebzip--cache-max 64
  "Maximum number of cached decompressed slices.")

(defun johnson-ebzip--decompress-slice (path slice-num)
  "Decompress slice SLICE-NUM from EBZIP file at PATH."
  (let ((cache-key (cons path slice-num)))
    (or (gethash cache-key johnson-ebzip--slice-cache)
        (let* ((header (johnson-ebzip--parse-header path))
               (index (plist-get header :index))
               (slice-size (plist-get header :slice-size))
               (comp-start (aref index slice-num))
               (comp-end (aref index (1+ slice-num)))
               (comp-len (- comp-end comp-start))
               (decompressed
                (if (= comp-len slice-size)
                    (johnson-ebzip--read-raw path comp-start comp-len)
                  (johnson-ebzip--zlib-decompress path comp-start comp-len))))
          (johnson-ebzip--cache-put cache-key decompressed)
          decompressed))))

(defun johnson-ebzip--zlib-decompress (path start length)
  "Read and zlib-decompress LENGTH bytes from PATH at START."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil start (+ start length))
    (unless (zlib-decompress-region (point-min) (point-max))
      (error "EBZIP: zlib decompression failed at offset %d in %s" start path))
    (buffer-string)))

(defun johnson-ebzip--cache-put (key value)
  "Store KEY-VALUE in the slice cache, evicting if full."
  (when (>= (hash-table-count johnson-ebzip--slice-cache)
            johnson-ebzip--cache-max)
    (let ((first-key nil))
      (maphash (lambda (k _v)
                 (unless first-key (setq first-key k)))
               johnson-ebzip--slice-cache)
      (when first-key
        (remhash first-key johnson-ebzip--slice-cache))))
  (puthash key value johnson-ebzip--slice-cache))

;;;; Public API

(defun johnson-ebzip-read (path start length)
  "Read LENGTH uncompressed bytes from EBZIP file PATH at offset START."
  (let* ((header (johnson-ebzip--parse-header path))
         (slice-size (plist-get header :slice-size))
         (first-slice (/ start slice-size))
         (last-slice (/ (+ start length -1) slice-size))
         (result nil)
         (collected 0))
    (cl-loop for i from first-slice to last-slice
             for slice-data = (johnson-ebzip--decompress-slice path i)
             for slice-start = (- start (* i slice-size))
             for slice-offset = (max 0 slice-start)
             for remaining = (- length collected)
             for available = (- (length slice-data) slice-offset)
             for take = (min remaining available)
             do (push (substring slice-data slice-offset
                                 (+ slice-offset take))
                      result)
             do (cl-incf collected take))
    (apply #'concat (nreverse result))))

(defun johnson-ebzip-uncompressed-size (path)
  "Return the uncompressed size of EBZIP file at PATH."
  (plist-get (johnson-ebzip--parse-header path) :uncompressed-size))

;;;; Cache clearing

(defun johnson-ebzip-clear-caches ()
  "Clear all EBZIP caches."
  (clrhash johnson-ebzip--header-cache)
  (clrhash johnson-ebzip--slice-cache))

(provide 'johnson-ebzip)

;;; johnson-ebzip.el ends here
