;;; johnson-epwing.el --- EPWING format backend for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.5.0
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

;; This module provides the EPWING (JIS X 4081) format backend for the
;; johnson dictionary package.  It handles parsing of EPWING and EB
;; format electronic dictionaries, including CATALOGS/CATALOG parsing,
;; B-tree index traversal, text rendering with escape sequence
;; processing, and EBZIP decompression (via johnson-ebzip.el).
;;
;; EPWING dictionaries use a directory-based structure with a CATALOGS
;; file at the top level and HONMON data files in subbook directories.
;; Discovery scans for CATALOGS files and enumerates subbooks.
;;
;; Character encoding: EPWING books use either JIS X 0208 (raw 2-byte
;; characters with bytes in 0x21-0x7E) or ISO 8859-1.  JIS bytes are
;; converted to EUC-JP (by adding 0x80 to each byte) before decoding.
;; Fullwidth Latin characters are normalized to ASCII for readability.

;;; Code:

(require 'cl-lib)
(require 'johnson-binary)

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")
(declare-function johnson-ebzip-read "johnson-ebzip")
(declare-function johnson-ebzip-uncompressed-size "johnson-ebzip")
(defvar johnson-dictionary-directories)

;;;; Constants

(defconst johnson-epwing--page-size 2048
  "Size of a single EPWING page in bytes.")

(defconst johnson-epwing--escape-byte #x1F
  "Escape byte that begins all EPWING text commands.")

(defconst johnson-epwing--escape-lengths
  (let ((tbl (make-hash-table)))
    ;; 2-byte sequences
    (dolist (cmd '(#x02 #x03 #x04 #x05 #x06 #x07 #x0A #x0B #x0C
                   #x0E #x0F #x10 #x11 #x12 #x13 #x59 #x5C #x61
                   #x6A #x6B #x6C #x6D #x6F #xE1))
      (puthash cmd 2 tbl))
    ;; 4-byte sequences
    (dolist (cmd '(#x09 #x1A #x1B #x1C #x1D #x1E #x1F
                   #x41 #x42 #x43 #x45 #xE0))
      (puthash cmd 4 tbl))
    ;; 8-byte sequences
    (dolist (cmd '(#x4B #x52 #x62 #x63 #x64))
      (puthash cmd 8 tbl))
    ;; 10-byte
    (puthash #x53 10 tbl)
    ;; 12-byte
    (puthash #x44 12 tbl)
    ;; 18-byte
    (puthash #x4A 18 tbl)
    ;; 20-byte
    (dolist (cmd '(#x3C #x4D))
      (puthash cmd 20 tbl))
    ;; 34-byte
    (puthash #x4F 34 tbl)
    ;; 46-byte
    (puthash #x39 46 tbl)
    tbl)
  "Hash table mapping escape command bytes to total sequence length.")

;;;; File I/O

(defun johnson-epwing--ebzip-p (path)
  "Return non-nil if PATH is an EBZIP-compressed file."
  (string-suffix-p ".ebz" path t))

(defun johnson-epwing--read-bytes (path start length)
  "Read LENGTH bytes from file PATH starting at byte position START.
Transparently handles EBZIP-compressed files."
  (if (johnson-epwing--ebzip-p path)
      (johnson-ebzip-read path start length)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path nil start (+ start length))
      (buffer-string))))

(defun johnson-epwing--read-page (path page-number)
  "Read page PAGE-NUMBER (1-based) from HONMON file at PATH."
  (johnson-epwing--read-bytes
   path (* (1- page-number) johnson-epwing--page-size)
   johnson-epwing--page-size))

(defun johnson-epwing--page-pos (page offset)
  "Convert PAGE number (1-based) and OFFSET to absolute file position."
  (+ (* (1- page) johnson-epwing--page-size) offset))

(defun johnson-epwing--read-file-unibyte (path)
  "Read the entire file at PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

;;;; Escape sequence handling

(defun johnson-epwing--escape-length (data pos)
  "Return the total byte length of the escape sequence in DATA at POS.
POS must point to the 0x1F escape byte."
  (if (>= (1+ pos) (length data))
      1
    (let ((cmd (aref data (1+ pos))))
      (min (or (gethash cmd johnson-epwing--escape-lengths) 2)
           (- (length data) pos)))))

;;;; Directory helpers

(defun johnson-epwing--find-child (dir name)
  "Find a file or directory named NAME (case-insensitive) under DIR.
Returns the full path or nil."
  (when (file-directory-p dir)
    (cl-find-if
     (lambda (f)
       (string-equal-ignore-case (file-name-nondirectory f) name))
     (directory-files dir t "[^.]"))))

;;;; JIS/EUC-JP conversion

(defun johnson-epwing--jis-to-euc (data &optional start end)
  "Convert raw JIS bytes in DATA to EUC-JP by adding 0x80 to each byte.
Operates on the range from START to END (defaults to full string)."
  (let* ((s (or start 0))
         (e (or end (length data)))
         (result (make-string (- e s) 0)))
    (dotimes (i (- e s))
      (aset result i (+ (aref data (+ s i)) #x80)))
    result))

;;;; Character encoding detection

(defvar johnson-epwing--charcode-cache (make-hash-table :test #'equal)
  "Cache mapping HONMON path to its character encoding.
Values are `jis', `euc-jp', or `iso-8859-1'.")

(defun johnson-epwing--detect-charcode (honmon-path)
  "Detect the character encoding for the EPWING book at HONMON-PATH.
Returns `jis', `euc-jp', or `iso-8859-1'."
  (or (gethash honmon-path johnson-epwing--charcode-cache)
      (let* ((catalogs (johnson-epwing--find-catalogs honmon-path))
             (coding (if catalogs
                        (johnson-epwing--charcode-from-catalog catalogs)
                      'jis)))
        (puthash honmon-path coding johnson-epwing--charcode-cache)
        coding)))

(defun johnson-epwing--charcode-from-catalog (catalogs-path)
  "Detect the character encoding from the catalog at CATALOGS-PATH.
Examines the first subbook title bytes to distinguish JIS X 0208
\(0x21-0x7E byte pairs), EUC-JP (0xA1-0xFE byte pairs), and
ISO 8859-1."
  (let* ((data (johnson-epwing--read-file-unibyte catalogs-path))
         (title-start 18)
         (title-end (min (+ title-start 80) (length data))))
    (cond
     ((johnson-epwing--has-jis-p data title-start title-end) 'jis)
     ((johnson-epwing--has-euc-jp-p data title-start title-end) 'euc-jp)
     (t 'iso-8859-1))))

(defun johnson-epwing--has-jis-p (data start end)
  "Return non-nil if DATA between START and END has JIS byte pairs.
JIS X 0208 characters have both bytes in the 0x21-0x7E range."
  (cl-loop for i from start below (1- end) by 2
           thereis (and (>= (aref data i) #x21)
                        (<= (aref data i) #x7E)
                        (>= (aref data (1+ i)) #x21)
                        (<= (aref data (1+ i)) #x7E)
                        ;; Exclude pure ASCII pairs (both bytes > 0x40
                        ;; could be ASCII text like "Te")
                        (or (< (aref data i) #x30)
                            (and (>= (aref data i) #x21)
                                 (<= (aref data i) #x29))))))

(defun johnson-epwing--has-euc-jp-p (data start end)
  "Return non-nil if DATA between START and END has EUC-JP pairs."
  (cl-loop for i from start below (1- end) by 2
           thereis (and (>= (aref data i) #xA1)
                        (<= (aref data i) #xFE)
                        (>= (aref data (1+ i)) #xA1)
                        (<= (aref data (1+ i)) #xFE))))

;;;; CATALOGS parsing

(defun johnson-epwing--find-catalogs (honmon-path)
  "Find the CATALOGS file for the EPWING book containing HONMON-PATH.
Navigates up the directory tree from HONMON-PATH."
  (let ((dir (file-name-directory honmon-path)))
    (cl-loop for d = dir then (file-name-directory (directory-file-name d))
             repeat 4
             for cat = (or (johnson-epwing--find-child d "CATALOGS")
                           (johnson-epwing--find-child d "CATALOG"))
             when cat return cat)))

(defun johnson-epwing--epwing-catalog-p (catalogs-path)
  "Return non-nil if CATALOGS-PATH is an EPWING catalog (not EB)."
  (string-equal-ignore-case
   (file-name-nondirectory catalogs-path) "CATALOGS"))

(defun johnson-epwing--parse-catalogs (catalogs-path)
  "Parse the catalog file at CATALOGS-PATH.
Returns a list of subbook plists with keys :title, :directory,
:index-page."
  (let* ((data (johnson-epwing--read-file-unibyte catalogs-path))
         (count (johnson-binary-u16be data 0))
         (epwing-p (johnson-epwing--epwing-catalog-p catalogs-path)))
    (cl-loop for i below count
             collect (if epwing-p
                        (johnson-epwing--parse-epwing-subbook data i)
                      (johnson-epwing--parse-eb-subbook data i)))))

(defun johnson-epwing--parse-epwing-subbook (data index)
  "Parse the EPWING subbook entry at INDEX from catalog DATA."
  (let* ((off (+ 16 (* index 164)))
         (title (johnson-epwing--decode-catalog-title
                 (substring data (+ off 2) (+ off 82))))
         (directory (string-trim (substring data (+ off 82) (+ off 90))))
         (index-page (johnson-binary-u16be data (+ off 94))))
    (list :title title :directory directory :index-page index-page)))

(defun johnson-epwing--parse-eb-subbook (data index)
  "Parse the EB subbook entry at INDEX from catalog DATA."
  (let* ((off (+ 16 (* index 40)))
         (title (johnson-epwing--decode-catalog-title
                 (substring data (+ off 2) (+ off 32))))
         (directory (string-trim (substring data (+ off 32) (+ off 40)))))
    (list :title title :directory directory :index-page 1)))

(defun johnson-epwing--decode-catalog-title (raw)
  "Decode a catalog title from RAW unibyte bytes.
Titles may be in JIS X 0208 (bytes 0x21-0x7E), EUC-JP (bytes
0xA1-0xFE), or ASCII.  JIS bytes are converted to EUC-JP before
decoding."
  (let* ((trimmed (johnson-epwing--trim-null-bytes raw))
         (euc (if (johnson-epwing--has-jis-p trimmed 0 (length trimmed))
                  (johnson-epwing--jis-to-euc trimmed)
                trimmed))
         (decoded (johnson-epwing--normalize-fullwidth
                   (string-trim-right
                    (decode-coding-string euc 'euc-jp)
                    "[\x00\s\u3000]+"))))
    (if (string-empty-p decoded) "Unknown" decoded)))

(defun johnson-epwing--trim-null-bytes (data)
  "Remove trailing null bytes from unibyte string DATA."
  (let ((end (length data)))
    (while (and (> end 0) (zerop (aref data (1- end))))
      (cl-decf end))
    (substring data 0 end)))

;;;; HONMON file location

(defun johnson-epwing--find-honmon (book-dir sub-dir-name)
  "Find the HONMON data file for subbook SUB-DIR-NAME under BOOK-DIR.
Searches for both uncompressed and EBZIP-compressed variants."
  (when-let* ((sub-dir (johnson-epwing--find-child book-dir sub-dir-name)))
    (let ((data-dir (johnson-epwing--find-child sub-dir "DATA")))
      (if data-dir
          (or (johnson-epwing--find-child data-dir "HONMON")
              (johnson-epwing--find-child data-dir "HONMON.EBZ")
              (johnson-epwing--find-child data-dir "HONMON2")
              (johnson-epwing--find-child data-dir "HONMON2.EBZ"))
        (or (johnson-epwing--find-child sub-dir "START")
            (johnson-epwing--find-child sub-dir "START.EBZ"))))))

;;;; Index management page

(defun johnson-epwing--read-index-table (path index-page)
  "Read the index management table from HONMON at PATH.
INDEX-PAGE is the 1-based page number.  Returns a list of index
entry plists with keys :id, :start-page, :page-count, :flags."
  (let* ((data (johnson-epwing--read-page path index-page))
         (count (aref data 1)))
    (cl-loop for i below count
             for off = (+ 16 (* i 16))
             when (< (+ off 15) (length data))
             collect (list :id (aref data off)
                           :start-page (johnson-binary-u32be data (+ off 2))
                           :page-count (johnson-binary-u32be data (+ off 6))
                           :flags (aref data (+ off 10))))))

(defun johnson-epwing--find-search-index (index-table)
  "Find the best search index entry from INDEX-TABLE.
Tries word search (0x70-0x72), then endword search (0x90-0x92).
Returns the index entry plist or nil."
  (cl-loop for id in '(#x71 #x70 #x72 #x91 #x90 #x92)
           thereis (cl-find id index-table
                            :key (lambda (e) (plist-get e :id)))))

;;;; B-tree traversal

(defun johnson-epwing--traverse-index (path start-page coding callback)
  "Traverse the search index B-tree in HONMON at PATH.
Starts at START-PAGE.  CODING is the character encoding.  Calls
CALLBACK with (HEADWORD TEXT-POSITION 0) for each leaf entry."
  (let ((visited (make-hash-table)))
    (johnson-epwing--traverse-page path start-page coding callback visited)))

(defun johnson-epwing--traverse-page (path page-num coding callback visited)
  "Traverse one index page PAGE-NUM and recurse into children.
PATH is the HONMON file.  CODING is the character encoding.
CALLBACK receives (HEADWORD TEXT-POSITION 0).  VISITED tracks
pages already processed."
  (when (or (<= page-num 0) (gethash page-num visited))
    (cl-return-from johnson-epwing--traverse-page))
  (puthash page-num t visited)
  (let* ((data (johnson-epwing--read-page path page-num))
         (flags (aref data 0))
         (entry-len (aref data 1))
         (count (johnson-binary-u16be data 2))
         (leaf-p (not (zerop (logand flags #x80)))))
    (if leaf-p
        (johnson-epwing--process-leaf-entries
         data entry-len count coding callback)
      (johnson-epwing--process-internal-entries
       data entry-len count path coding callback visited))))

(defun johnson-epwing--process-leaf-entries (data entry-len count coding
                                                  callback)
  "Process leaf node entries in page DATA, calling CALLBACK for each.
ENTRY-LEN is the fixed key length (0 for variable).  COUNT is
the number of entries.  CODING is the character encoding."
  (let ((pos 4))
    (dotimes (_ count)
      (when (>= pos (- (length data) 12))
        (cl-return))
      (let* ((key-len (if (zerop entry-len)
                          (prog1 (aref data pos) (cl-incf pos))
                        entry-len))
             (key-end (min (+ pos key-len) (- (length data) 12)))
             (key-raw (substring data pos key-end))
             (headword (johnson-epwing--decode-search-key key-raw coding)))
        (setq pos key-end)
        (let* ((text-page (johnson-binary-u32be data pos))
               (text-off (johnson-binary-u16be data (+ pos 4)))
               (text-pos (johnson-epwing--page-pos text-page text-off)))
          (cl-incf pos 12)
          (when (and (> text-page 0)
                     (not (string-empty-p headword)))
            (funcall callback headword text-pos 0)))))))

(defun johnson-epwing--process-internal-entries (data entry-len count path
                                                  coding callback visited)
  "Process internal node entries in page DATA, recursing into children.
ENTRY-LEN is the fixed key length (0 for variable).  COUNT is
the number of entries.  PATH is the HONMON file.  CODING is the
character encoding.  CALLBACK and VISITED are forwarded."
  (let ((pos 4))
    (dotimes (_ count)
      (when (>= pos (- (length data) 4))
        (cl-return))
      (let ((key-len (if (zerop entry-len)
                         (prog1 (aref data pos) (cl-incf pos))
                       entry-len)))
        (cl-incf pos key-len)
        (let ((child-page (johnson-binary-u32be data pos)))
          (cl-incf pos 4)
          (when (> child-page 0)
            (johnson-epwing--traverse-page
             path child-page coding callback visited)))))))

(defun johnson-epwing--decode-search-key (raw coding)
  "Decode search key RAW using CODING, trimming trailing nulls.
For JIS encoding, converts to EUC-JP before decoding.
Normalizes fullwidth Latin characters to ASCII."
  (let* ((trimmed (johnson-epwing--trim-null-bytes raw))
         (len (length trimmed)))
    (if (zerop len)
        ""
      (condition-case nil
          (let* ((bytes (if (eq coding 'jis)
                            (johnson-epwing--jis-to-euc trimmed)
                          trimmed))
                 (decoded (decode-coding-string
                           bytes
                           (if (eq coding 'jis) 'euc-jp coding))))
            (let ((trim-re "[.\u3002\uff0e\s]+"))
              (string-trim (johnson-epwing--normalize-fullwidth decoded)
                           trim-re trim-re)))
        (error "")))))

(defun johnson-epwing--normalize-fullwidth (str)
  "Convert fullwidth characters in STR to ASCII equivalents.
Maps U+FF01-U+FF5E to U+0021-U+007E and U+3000 to space."
  (let ((result (copy-sequence str)))
    (dotimes (i (length result))
      (let ((ch (aref result i)))
        (cond
         ((<= #xFF01 ch #xFF5E)
          (aset result i (- ch #xFEE0)))
         ((= ch #x3000)
          (aset result i ?\s)))))
    result))

;;;; Text reading

(defun johnson-epwing--uncompressed-size (path)
  "Return the uncompressed data size for PATH.
For EBZIP files, reads the size from the header.  For plain
files, returns the file size."
  (if (johnson-epwing--ebzip-p path)
      (johnson-ebzip-uncompressed-size path)
    (file-attribute-size (file-attributes path))))

(defun johnson-epwing--read-text-raw (path offset)
  "Read raw text bytes from HONMON at PATH starting at OFFSET.
Reads until the stop code (0x1F 0x03) or end of file."
  (let* ((file-size (johnson-epwing--uncompressed-size path))
         (chunk-size (* 16 johnson-epwing--page-size))
         (result nil)
         (pos offset)
         (done nil))
    (while (and (not done) (< pos file-size))
      (let* ((read-len (min chunk-size (- file-size pos)))
             (chunk (johnson-epwing--read-bytes path pos read-len))
             (stop (johnson-epwing--find-entry-end chunk)))
        (if stop
            (progn
              (push (substring chunk 0 stop) result)
              (setq done t))
          (push chunk result)
          (cl-incf pos read-len))))
    (apply #'concat (nreverse result))))

(defun johnson-epwing--find-entry-end (data)
  "Find the end of the current entry in unibyte DATA.
Stops at 0x1F 0x03 (end text body) or the second occurrence of
0x1F 0x41 (next keyword marker, i.e. next entry).  Returns the
byte position or nil."
  (let ((limit (1- (length data)))
        (keyword-count 0))
    (cl-loop for i from 0 below limit
             when (= (aref data i) #x1F)
             do (let ((cmd (aref data (1+ i))))
                  (when (= cmd #x03) (cl-return i))
                  (when (= cmd #x41)
                    (cl-incf keyword-count)
                    (when (>= keyword-count 2) (cl-return i)))))))

;;;; Entry decoding

(defun johnson-epwing--decode-entry (raw coding)
  "Decode RAW unibyte EPWING text using character encoding CODING.
CODING is `jis', `euc-jp', or `iso-8859-1'.  Escape sequences
are preserved as character-code sequences starting with character
31.  Character data between escapes is decoded using CODING."
  (let ((segments nil)
        (text-start 0)
        (pos 0)
        (len (length raw)))
    (while (< pos len)
      (if (= (aref raw pos) johnson-epwing--escape-byte)
          (let ((esc-len (johnson-epwing--escape-length raw pos)))
            (when (> pos text-start)
              (push (johnson-epwing--decode-text-segment
                     raw text-start pos coding)
                    segments))
            (push (johnson-epwing--escape-to-string raw pos esc-len) segments)
            (cl-incf pos esc-len)
            (setq text-start pos))
        (cl-incf pos)))
    (when (> pos text-start)
      (push (johnson-epwing--decode-text-segment raw text-start pos coding)
            segments))
    (apply #'concat (nreverse segments))))

(defun johnson-epwing--decode-text-segment (raw start end coding)
  "Decode the text segment of RAW between START and END using CODING.
For JIS encoding, converts bytes to EUC-JP before decoding."
  (condition-case nil
      (let ((bytes (if (eq coding 'jis)
                       (johnson-epwing--jis-to-euc raw start end)
                     (substring raw start end))))
        (decode-coding-string bytes
                              (if (eq coding 'jis) 'euc-jp coding)))
    (error (substring raw start end))))

(defun johnson-epwing--escape-to-string (raw pos len)
  "Convert escape sequence in RAW at POS of length LEN to a string.
Each byte becomes a character in the returned multibyte string."
  (let ((chars nil))
    (dotimes (i len)
      (push (aref raw (+ pos i)) chars))
    (apply #'string (nreverse chars))))

;;;; Format detection

(defconst johnson-epwing--honmon-names
  '("HONMON" "HONMON2" "START"
    "HONMON.EBZ" "HONMON2.EBZ" "START.EBZ")
  "Valid HONMON file names (uppercase) for EPWING detection.")

(defun johnson-epwing-detect (path)
  "Return non-nil if PATH is a valid EPWING HONMON file."
  (and (file-regular-p path)
       (member (upcase (file-name-nondirectory path))
               johnson-epwing--honmon-names)
       (johnson-epwing--find-catalogs path)
       t))

;;;; Metadata parsing

(defun johnson-epwing-parse-metadata (path)
  "Parse metadata from the EPWING HONMON file at PATH.
Returns a plist with :name, :source-lang, :target-lang."
  (let* ((catalogs (johnson-epwing--find-catalogs path))
         (subbooks (johnson-epwing--parse-catalogs catalogs))
         (sub (johnson-epwing--subbook-for-honmon catalogs path subbooks)))
    (list :name (or (plist-get sub :title) "Unknown EPWING")
          :source-lang ""
          :target-lang "")))

(defun johnson-epwing--subbook-for-honmon (catalogs-path honmon-path subbooks)
  "Find the subbook entry in SUBBOOKS matching HONMON-PATH.
CATALOGS-PATH is used to resolve relative paths."
  (let ((book-dir (file-name-directory catalogs-path)))
    (or (cl-find-if
         (lambda (sub)
           (let* ((sub-dir (plist-get sub :directory))
                  (honmon (johnson-epwing--find-honmon book-dir sub-dir)))
             (and honmon (file-equal-p honmon honmon-path))))
         subbooks)
        (car subbooks))))

;;;; Discovery

(defun johnson-epwing-discover ()
  "Discover EPWING dictionaries in `johnson-dictionary-directories'.
Scans for CATALOGS/CATALOG files, parses each to enumerate
subbooks, and returns a list of dictionary plists."
  (let ((dicts nil)
        (case-fold-search t))
    (dolist (dir johnson-dictionary-directories)
      (let ((expanded (expand-file-name dir)))
        (when (file-directory-p expanded)
          (dolist (cat-file (directory-files-recursively
                             expanded "\\`catalogs?\\'" nil))
            (condition-case err
                (let* ((subbooks (johnson-epwing--parse-catalogs cat-file))
                       (book-dir (file-name-directory cat-file)))
                  (dolist (sub subbooks)
                    (when-let* ((honmon (johnson-epwing--find-honmon
                                         book-dir
                                         (plist-get sub :directory))))
                      (push (list :path honmon
                                  :format-name "epwing"
                                  :name (plist-get sub :title)
                                  :source-lang ""
                                  :target-lang ""
                                  :group "EPWING"
                                  :priority 0)
                            dicts))))
              (error
               (message "johnson-epwing: error reading %s: %s"
                        cat-file (error-message-string err))))))))
    (nreverse dicts)))

;;;; Index building

(defun johnson-epwing-build-index (path callback)
  "Build search index for the EPWING dictionary at PATH.
PATH is a HONMON file.  CALLBACK is called as
\(funcall CALLBACK HEADWORD BYTE-OFFSET BYTE-LENGTH) for each
entry.  BYTE-OFFSET is the absolute file position of the text.
BYTE-LENGTH is always 0 (entries are delimited by stop codes)."
  (let* ((catalogs (johnson-epwing--find-catalogs path))
         (subbooks (johnson-epwing--parse-catalogs catalogs))
         (sub (johnson-epwing--subbook-for-honmon catalogs path subbooks))
         (index-page (plist-get sub :index-page))
         (index-table (johnson-epwing--read-index-table path index-page))
         (search-idx (johnson-epwing--find-search-index index-table))
         (coding (johnson-epwing--detect-charcode path)))
    (unless search-idx
      (error "No search index found in %s" path))
    (johnson-epwing--traverse-index
     path (plist-get search-idx :start-page) coding callback)))

;;;; Entry retrieval

(defvar johnson-epwing--current-honmon-path nil
  "HONMON file path of the entry currently being rendered.
Set by `johnson-epwing-retrieve-entry', read by ref button
actions.")

(defun johnson-epwing-retrieve-entry (path byte-offset _byte-length)
  "Retrieve an entry from the EPWING dictionary at PATH.
BYTE-OFFSET is the absolute file position.  _BYTE-LENGTH is
ignored (entries are read until the stop code).  Returns a
decoded string with escape sequences preserved as character-31
markers."
  (setq johnson-epwing--current-honmon-path path)
  (let ((raw (johnson-epwing--read-text-raw path byte-offset))
        (coding (johnson-epwing--detect-charcode path)))
    (johnson-epwing--decode-entry raw coding)))

;;;; Entry rendering

(defun johnson-epwing-render-entry (data)
  "Render EPWING entry DATA into the current buffer.
DATA is a decoded string with escape sequences preserved as
character-31 markers."
  (setq data (johnson-epwing--normalize-fullwidth data))
  (let ((pos 0)
        (len (length data))
        (bold-start nil)
        (italic-start nil)
        (emphasis-start nil)
        (ref-start nil)
        (sup-start nil)
        (sub-start nil))
    (while (< pos len)
      (let ((ch (aref data pos)))
        (if (= ch johnson-epwing--escape-byte)
            (let ((cmd (and (< (1+ pos) len) (aref data (1+ pos)))))
              (pcase cmd
                (#x02 (cl-incf pos 2))
                (#x03 (cl-return))
                (#x04 (cl-incf pos 2))
                (#x05 (cl-incf pos 2))
                (#x06 (setq sub-start (point)) (cl-incf pos 2))
                (#x07
                 (when sub-start
                   (add-text-properties sub-start (point)
                                        '(display (raise -0.3)))
                   (setq sub-start nil))
                 (cl-incf pos 2))
                (#x0A (insert "\n") (cl-incf pos 2))
                (#x09
                 (let ((level (and (< (+ pos 3) len)
                                   (johnson-epwing--char-u16be data (+ pos 2)))))
                   (when (and level (> level 0))
                     (insert (make-string (min level 20) ?\s)))
                   (cl-incf pos 4)))
                (#x0E (setq sup-start (point)) (cl-incf pos 2))
                (#x0F
                 (when sup-start
                   (add-text-properties sup-start (point)
                                        '(display (raise 0.3)))
                   (setq sup-start nil))
                 (cl-incf pos 2))
                (#x12 (setq emphasis-start (point)) (cl-incf pos 2))
                (#x13
                 (when emphasis-start
                   (add-text-properties emphasis-start (point)
                                        '(face johnson-bold-face))
                   (setq emphasis-start nil))
                 (cl-incf pos 2))
                (#x41 (cl-incf pos 4))
                (#x42 (setq ref-start (point)) (cl-incf pos 4))
                (#x61 (cl-incf pos 2))
                (#x62
                 (when ref-start
                   (let* ((ref-page (and (< (+ pos 7) len)
                                         (johnson-epwing--char-u32be
                                          data (+ pos 2))))
                          (ref-off (and (< (+ pos 7) len)
                                        (johnson-epwing--char-u16be
                                         data (+ pos 6))))
                          (text (buffer-substring-no-properties
                                 ref-start (point))))
                     (delete-region ref-start (point))
                     (johnson-epwing--insert-ref-button text ref-page ref-off))
                   (setq ref-start nil))
                 (cl-incf pos 8))
                (#xE0
                 (let ((deco-type (and (< (+ pos 3) len)
                                       (johnson-epwing--char-u16be
                                        data (+ pos 2)))))
                   (pcase deco-type
                     (1 (setq italic-start (point)))
                     (3 (setq bold-start (point)))))
                 (cl-incf pos 4))
                (#xE1
                 (when bold-start
                   (add-text-properties bold-start (point)
                                        '(face johnson-bold-face))
                   (setq bold-start nil))
                 (when italic-start
                   (add-text-properties italic-start (point)
                                        '(face johnson-italic-face))
                   (setq italic-start nil))
                 (cl-incf pos 2))
                (_
                 (cl-incf pos (johnson-epwing--escape-length-in-string
                               data pos)))))
          (insert ch)
          (cl-incf pos))))))

(defun johnson-epwing--char-u16be (str pos)
  "Read a 16-bit big-endian value from characters in STR at POS."
  (logior (ash (aref str pos) 8)
          (aref str (1+ pos))))

(defun johnson-epwing--char-u32be (str pos)
  "Read a 32-bit big-endian value from characters in STR at POS."
  (logior (ash (aref str pos) 24)
          (ash (aref str (1+ pos)) 16)
          (ash (aref str (+ pos 2)) 8)
          (aref str (+ pos 3))))

(defun johnson-epwing--escape-length-in-string (data pos)
  "Return the length of the escape sequence in multibyte string DATA at POS.
Uses character codes (not byte values) to determine the command."
  (if (>= (1+ pos) (length data))
      1
    (let ((cmd (aref data (1+ pos))))
      (min (or (gethash cmd johnson-epwing--escape-lengths) 2)
           (- (length data) pos)))))

(defun johnson-epwing--insert-ref-button (text ref-page ref-off)
  "Insert a cross-reference button with TEXT linking to REF-PAGE:REF-OFF."
  (let ((honmon-path johnson-epwing--current-honmon-path))
    (insert-text-button
     text
     'face 'johnson-ref-face
     'action (lambda (btn)
               (let ((word (button-get btn 'johnson-epwing-text)))
                 (when (and word (not (string-empty-p word)))
                   (johnson-lookup word))))
     'johnson-epwing-path honmon-path
     'johnson-epwing-page ref-page
     'johnson-epwing-offset ref-off
     'johnson-epwing-text (johnson-epwing--normalize-fullwidth text)
     'help-echo (format "Look up: %s"
                        (johnson-epwing--normalize-fullwidth text)))))

;;;; Cache clearing

(defun johnson-epwing-clear-caches ()
  "Clear all EPWING caches."
  (clrhash johnson-epwing--charcode-cache)
  (setq johnson-epwing--current-honmon-path nil))

;;;; Provide and register

(provide 'johnson-epwing)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "epwing"
   :extensions nil
   :detect #'johnson-epwing-detect
   :parse-metadata #'johnson-epwing-parse-metadata
   :build-index #'johnson-epwing-build-index
   :retrieve-entry #'johnson-epwing-retrieve-entry
   :render-entry #'johnson-epwing-render-entry
   :discover #'johnson-epwing-discover))

;;; johnson-epwing.el ends here
