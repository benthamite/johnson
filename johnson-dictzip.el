;;; johnson-dictzip.el --- Dictzip random-access decompression for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "29.1"))

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

;; Dictzip random-access decompression.  Dictzip is a gzip-compatible
;; format that stores data in independently-compressed chunks, enabling
;; random access without decompressing the entire file.  Used by
;; StarDict (.dict.dz) and DSL (.dsl.dz) format backends.

;;; Code:

(require 'cl-lib)

(declare-function zlib-decompress-region "decompress.c" (start end &optional allow-partial))

;;;; Binary integer helpers

(defsubst johnson-dictzip--u16le (pos)
  "Read unsigned 16-bit little-endian integer at buffer position POS."
  (+ (char-after pos)
     (ash (char-after (1+ pos)) 8)))

(defsubst johnson-dictzip--u16be (pos)
  "Read unsigned 16-bit big-endian integer at buffer position POS."
  (+ (ash (char-after pos) 8)
     (char-after (1+ pos))))

(defsubst johnson-dictzip--u32be (pos)
  "Read unsigned 32-bit big-endian integer at buffer position POS."
  (+ (ash (char-after pos) 24)
     (ash (char-after (1+ pos)) 16)
     (ash (char-after (+ pos 2)) 8)
     (char-after (+ pos 3))))

(defsubst johnson-dictzip--u64be (pos)
  "Read unsigned 64-bit big-endian integer at buffer position POS."
  (+ (ash (char-after pos) 56)
     (ash (char-after (1+ pos)) 48)
     (ash (char-after (+ pos 2)) 40)
     (ash (char-after (+ pos 3)) 32)
     (ash (char-after (+ pos 4)) 24)
     (ash (char-after (+ pos 5)) 16)
     (ash (char-after (+ pos 6)) 8)
     (char-after (+ pos 7))))

;;;; Header cache

(defvar johnson-dictzip--header-cache (make-hash-table :test #'equal)
  "Cache of parsed dictzip headers (path → header plist).")

;;;; Chunk cache (simple LRU)

(defvar johnson-dictzip--chunk-cache nil
  "LRU cache of decompressed chunks.
List of (KEY . DATA) cons cells, most recent first.")

(defconst johnson-dictzip--chunk-cache-size 8
  "Maximum number of cached decompressed chunks.")

(defun johnson-dictzip--chunk-cache-get (key)
  "Get cached chunk for KEY, promoting it to front of LRU.
Returns the chunk data (unibyte string) or nil."
  (let ((entry (assoc key johnson-dictzip--chunk-cache)))
    (when entry
      (setq johnson-dictzip--chunk-cache
            (cons entry (delq entry johnson-dictzip--chunk-cache)))
      (cdr entry))))

(defun johnson-dictzip--chunk-cache-put (key data)
  "Cache chunk DATA under KEY, evicting oldest if full."
  (let ((entry (assoc key johnson-dictzip--chunk-cache)))
    (if entry
        (progn
          (setcdr entry data)
          (setq johnson-dictzip--chunk-cache
                (cons entry (delq entry johnson-dictzip--chunk-cache))))
      (push (cons key data) johnson-dictzip--chunk-cache)
      (when (> (length johnson-dictzip--chunk-cache)
               johnson-dictzip--chunk-cache-size)
        (setq johnson-dictzip--chunk-cache
              (seq-take johnson-dictzip--chunk-cache
                        johnson-dictzip--chunk-cache-size))))))

;;;; Gzip header parsing

(defun johnson-dictzip--parse-header (path)
  "Parse the dictzip header from file at PATH.
Returns a plist with keys:
  :chlen       - uncompressed chunk size
  :chcnt       - number of chunks
  :chunk-sizes - vector of compressed sizes per chunk
  :data-offset - file byte offset where compressed data begins"
  (or (gethash path johnson-dictzip--header-cache)
      (let ((header (johnson-dictzip--do-parse-header path)))
        (puthash path header johnson-dictzip--header-cache)
        header)))

(defun johnson-dictzip--do-parse-header (path)
  "Internal: parse the gzip/dictzip header from PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0
                                    (min 65536
                                         (file-attribute-size
                                          (file-attributes path))))
    ;; Verify gzip magic and deflate method.
    (unless (and (>= (point-max) 10)
                 (= (char-after 1) #x1f)
                 (= (char-after 2) #x8b)
                 (= (char-after 3) 8))
      (error "Not a gzip file: %s" path))
    (let* ((flg (char-after 4))
           ;; Fixed header: ID1(1) ID2(1) CM(1) FLG(1) MTIME(4) XFL(1) OS(1)
           (pos 11)
           (chlen nil)
           (chcnt nil)
           (chunk-sizes nil))
      ;; FEXTRA (bit 2)
      (when (/= (logand flg 4) 0)
        (let* ((xlen (johnson-dictzip--u16le pos))
               (xend (+ pos 2 xlen))
               (xpos (+ pos 2)))
          ;; Search for RA subfield (SI1='R'=0x52, SI2='A'=0x41).
          (while (< xpos xend)
            (let ((si1 (char-after xpos))
                  (si2 (char-after (1+ xpos)))
                  (slen (johnson-dictzip--u16le (+ xpos 2))))
              (if (and (= si1 #x52) (= si2 #x41))
                  (let ((sdata (+ xpos 4)))
                    ;; ver(u16le) chlen(u16le) chcnt(u16le) sizes(chcnt*u16le)
                    (setq chlen (johnson-dictzip--u16le (+ sdata 2)))
                    (setq chcnt (johnson-dictzip--u16le (+ sdata 4)))
                    (setq chunk-sizes (make-vector chcnt 0))
                    (dotimes (i chcnt)
                      (aset chunk-sizes i
                            (johnson-dictzip--u16le (+ sdata 6 (* i 2)))))
                    (setq xpos xend))
                (setq xpos (+ xpos 4 slen)))))
          (setq pos xend)))
      ;; FNAME (bit 3): null-terminated string
      (when (/= (logand flg 8) 0)
        (while (and (< pos (point-max))
                    (/= (char-after pos) 0))
          (cl-incf pos))
        (cl-incf pos))
      ;; FCOMMENT (bit 4): null-terminated string
      (when (/= (logand flg 16) 0)
        (while (and (< pos (point-max))
                    (/= (char-after pos) 0))
          (cl-incf pos))
        (cl-incf pos))
      ;; FHCRC (bit 1): 2-byte header CRC
      (when (/= (logand flg 2) 0)
        (cl-incf pos 2))
      (unless (and chlen chcnt chunk-sizes)
        (error "No dictzip RA header found in %s" path))
      ;; pos is 1-based buffer position; convert to 0-based file offset.
      (list :chlen chlen
            :chcnt chcnt
            :chunk-sizes chunk-sizes
            :data-offset (1- pos)))))

;;;; Chunk decompression

(defun johnson-dictzip--decompress-chunk (path header chunk-index)
  "Decompress chunk CHUNK-INDEX from the dictzip file at PATH.
HEADER is the parsed header plist.  Returns a unibyte string."
  (let* ((cache-key (cons path chunk-index))
         (cached (johnson-dictzip--chunk-cache-get cache-key)))
    (or cached
        (let* ((chunk-sizes (plist-get header :chunk-sizes))
               (data-offset (plist-get header :data-offset))
               ;; Calculate file offset of this chunk.
               (file-offset data-offset)
               (_ (dotimes (i chunk-index)
                    (cl-incf file-offset (aref chunk-sizes i))))
               (comp-size (aref chunk-sizes chunk-index))
               (result
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  ;; Prepend zlib header to make raw deflate into zlib stream.
                  (insert #x78 #x01)
                  (insert-file-contents-literally path nil
                                                  file-offset
                                                  (+ file-offset comp-size))
                  ;; zlib-decompress-region with ALLOW-PARTIAL returns:
                  ;;   t       - full success
                  ;;   integer - bytes consumed (partial; expected for dictzip
                  ;;             chunks which lack a zlib checksum trailer)
                  ;;   nil     - total failure
                  (let ((result (zlib-decompress-region 1 (point-max) t)))
                    (unless result
                      (error "Dictzip: decompression failed for chunk %d of %s"
                             chunk-index path)))
                  (buffer-string))))
          (johnson-dictzip--chunk-cache-put cache-key result)
          result))))

;;;; Public API

(defun johnson-dictzip-read (filename offset length)
  "Read LENGTH bytes at uncompressed OFFSET from dictzip FILENAME.
Returns a unibyte string with the decompressed data."
  (let* ((header (johnson-dictzip--parse-header filename))
         (chlen (plist-get header :chlen))
         (first-chunk (/ offset chlen))
         (last-chunk (/ (+ offset length -1) chlen))
         (parts nil))
    (dotimes (i (1+ (- last-chunk first-chunk)))
      (let* ((chunk-idx (+ first-chunk i))
             (chunk-data (johnson-dictzip--decompress-chunk
                          filename header chunk-idx))
             (chunk-start (* chunk-idx chlen))
             (slice-start (max 0 (- offset chunk-start)))
             (slice-end (min (length chunk-data)
                             (- (+ offset length) chunk-start))))
        (push (substring chunk-data slice-start slice-end) parts)))
    (apply #'concat (nreverse parts))))

(defun johnson-dictzip-read-full (filename)
  "Read the entire uncompressed content of dictzip FILENAME.
Returns a unibyte string."
  (let* ((header (johnson-dictzip--parse-header filename))
         (chcnt (plist-get header :chcnt))
         (parts nil))
    (dotimes (i chcnt)
      (push (johnson-dictzip--decompress-chunk filename header i) parts))
    (apply #'concat (nreverse parts))))

(defun johnson-dictzip-clear-caches ()
  "Clear all dictzip caches (headers and chunks)."
  (clrhash johnson-dictzip--header-cache)
  (setq johnson-dictzip--chunk-cache nil))

(provide 'johnson-dictzip)
;;; johnson-dictzip.el ends here
