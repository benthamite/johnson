;;; johnson-mdict.el --- MDict format backend for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides the MDict (.mdx/.mdd) format backend for the
;; johnson dictionary package.  It handles parsing of MDict v1.2 and
;; v2.0 binary files, including header parsing, keyword index/block
;; decompression, record retrieval, and HTML rendering via
;; johnson-html.  Supports zlib compression and Encrypted="2"
;; keyword index decryption via RIPEMD-128.

;;; Code:

(require 'cl-lib)
(require 'johnson-html)
(require 'johnson-ripemd128)

(declare-function johnson-register-format "johnson")
(declare-function johnson-insert-audio-button "johnson")

;;;; Binary integer helpers

(defsubst johnson-mdict--u8 (data pos)
  "Read unsigned 8-bit integer from unibyte DATA at byte POS."
  (aref data pos))

(defsubst johnson-mdict--u16le (data pos)
  "Read unsigned 16-bit little-endian integer from DATA at POS."
  (logior (aref data pos)
          (ash (aref data (+ pos 1)) 8)))

(defsubst johnson-mdict--u16be (data pos)
  "Read unsigned 16-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 8)
          (aref data (+ pos 1))))

(defsubst johnson-mdict--u32be (data pos)
  "Read unsigned 32-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 24)
          (ash (aref data (+ pos 1)) 16)
          (ash (aref data (+ pos 2)) 8)
          (aref data (+ pos 3))))

(defsubst johnson-mdict--u32le (data pos)
  "Read unsigned 32-bit little-endian integer from DATA at POS."
  (logior (aref data pos)
          (ash (aref data (+ pos 1)) 8)
          (ash (aref data (+ pos 2)) 16)
          (ash (aref data (+ pos 3)) 24)))

(defsubst johnson-mdict--u64be (data pos)
  "Read unsigned 64-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 56)
          (ash (aref data (+ pos 1)) 48)
          (ash (aref data (+ pos 2)) 40)
          (ash (aref data (+ pos 3)) 32)
          (ash (aref data (+ pos 4)) 24)
          (ash (aref data (+ pos 5)) 16)
          (ash (aref data (+ pos 6)) 8)
          (aref data (+ pos 7))))

(defun johnson-mdict--read-number (data pos width)
  "Read a big-endian unsigned integer from DATA at POS.
WIDTH is 4 or 8 bytes."
  (if (= width 8)
      (johnson-mdict--u64be data pos)
    (johnson-mdict--u32be data pos)))

;;;; Adler-32

(defun johnson-mdict--adler32 (data)
  "Compute the Adler-32 checksum of unibyte DATA.
Returns a 32-bit integer."
  (let ((a 1) (b 0)
        (len (length data)))
    (dotimes (i len)
      (setq a (mod (+ a (aref data i)) 65521))
      (setq b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

;;;; Internal variables

(defvar johnson-mdict--header-cache (make-hash-table :test #'equal)
  "Cache of parsed MDict headers.
Maps file path to a header plist.")

(defvar johnson-mdict--keyword-cache (make-hash-table :test #'equal)
  "Cache of parsed keyword lists.
Maps file path to vector of (HEADWORD . RECORD-OFFSET) pairs.")

(defvar johnson-mdict--record-index-cache (make-hash-table :test #'equal)
  "Cache of record block index data.
Maps file path to a plist with :record-blocks-offset and :blocks.")

(defvar johnson-mdict--current-dict-dir nil
  "Directory of the dictionary being rendered.
Set by `johnson-mdict-retrieve-entry' for use by the renderer.")

;;;; Block cache (simple LRU)

(defvar johnson-mdict--block-cache nil
  "LRU cache of decompressed record blocks.
List of (KEY . DATA) cons cells, most recent first.")

(defconst johnson-mdict--block-cache-size 8
  "Maximum number of cached decompressed record blocks.")

(defun johnson-mdict--block-cache-get (key)
  "Get cached block for KEY, promoting it to front of LRU.
Returns the block data (unibyte string) or nil."
  (let ((entry (assoc key johnson-mdict--block-cache)))
    (when entry
      (setq johnson-mdict--block-cache
            (cons entry (delq entry johnson-mdict--block-cache)))
      (cdr entry))))

(defun johnson-mdict--block-cache-put (key data)
  "Cache block DATA under KEY, evicting oldest if full."
  (let ((entry (assoc key johnson-mdict--block-cache)))
    (if entry
        (progn
          (setcdr entry data)
          (setq johnson-mdict--block-cache
                (cons entry (delq entry johnson-mdict--block-cache))))
      (push (cons key data) johnson-mdict--block-cache)
      (when (> (length johnson-mdict--block-cache)
               johnson-mdict--block-cache-size)
        (setq johnson-mdict--block-cache
              (seq-take johnson-mdict--block-cache
                        johnson-mdict--block-cache-size))))))

;;;; Block decompression

(defun johnson-mdict--decompress-block (data)
  "Decompress a single MDict data block.
DATA is a unibyte string containing the raw block (with 8-byte header).
Returns the decompressed unibyte string."
  (let ((comp-type (johnson-mdict--u32be data 0)))
    (pcase comp-type
      (#x00000000
       ;; No compression: strip 8-byte header.
       (substring data 8))
      (#x02000000
       ;; Zlib compression: strip 8-byte header, decompress.
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert (substring data 8))
         (zlib-decompress-region (point-min) (point-max))
         (buffer-string)))
      (#x01000000
       (error "LZO compression not yet supported"))
      (_
       (error "Unknown MDict compression type: %08x" comp-type)))))

;;;; Header parsing

(defun johnson-mdict--parse-header (path)
  "Parse the MDict header from file at PATH.
Returns a plist with keys:
  :version     - float (1.2 or 2.0)
  :num-width   - number width in bytes (4 for v1.2, 8 for v2.0)
  :encoding    - coding system symbol
  :encrypted   - encryption flag integer
  :title       - dictionary title string
  :description - description string
  :header-end  - file byte offset past the header
  :data-offset - synonym for :header-end (where keyword section starts)"
  (or (gethash path johnson-mdict--header-cache)
      (let ((header (johnson-mdict--do-parse-header path)))
        (puthash path header johnson-mdict--header-cache)
        header)))

(defun johnson-mdict--do-parse-header (path)
  "Internal: parse the MDict header from PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; Read first 4 bytes: header size (big-endian).
    (insert-file-contents-literally path nil 0 4)
    (let* ((header-size (johnson-mdict--u32be (buffer-string) 0))
           ;; Read header-size bytes + 4 bytes checksum after header.
           (total-read (+ 4 header-size 4)))
      (erase-buffer)
      (insert-file-contents-literally path nil 0 total-read)
      (let* ((raw (buffer-string))
             ;; Header text is UTF-16LE encoded, starting at offset 4.
             (header-text (decode-coding-string
                           (substring raw 4 (+ 4 header-size))
                           'utf-16le))
             ;; Parse version, encoding, encrypted, title, description from XML attrs.
             (version (johnson-mdict--extract-header-attr
                       header-text "GeneratedByEngineVersion"))
             (ver-float (if version (string-to-number version) 2.0))
             (encoding-str (or (johnson-mdict--extract-header-attr
                                header-text "Encoding")
                               "utf-8"))
             (encrypted-str (or (johnson-mdict--extract-header-attr
                                 header-text "Encrypted")
                                "0"))
             (title (or (johnson-mdict--extract-header-attr
                         header-text "Title")
                        ""))
             (description (or (johnson-mdict--extract-header-attr
                               header-text "Description")
                              ""))
             (num-width (if (< ver-float 2.0) 4 8))
             (encoding (johnson-mdict--resolve-encoding encoding-str)))
        (when (>= ver-float 3.0)
          (error "MDict v3.0 is not supported"))
        (list :version ver-float
              :num-width num-width
              :encoding encoding
              :encrypted (string-to-number encrypted-str)
              :title title
              :description description
              :header-end total-read
              :data-offset total-read)))))

(defun johnson-mdict--extract-header-attr (header-text attr)
  "Extract the value of ATTR from MDict HEADER-TEXT.
HEADER-TEXT is the decoded XML-like header string."
  (when (string-match
         (concat attr "\\s-*=\\s-*\"\\([^\"]*\\)\"")
         header-text)
    (match-string 1 header-text)))

(defun johnson-mdict--resolve-encoding (encoding-str)
  "Resolve MDict ENCODING-STR to an Emacs coding system."
  (let ((lower (downcase (string-trim encoding-str))))
    (cond
     ((or (string= lower "utf-8") (string= lower "utf8"))
      'utf-8)
     ((or (string= lower "utf-16") (string= lower "utf-16le")
          (string= lower "utf16"))
      'utf-16le)
     ((string= lower "gbk")
      'gbk)
     ((string= lower "gb2312")
      'gb2312)
     ((string= lower "big5")
      'big5)
     ((or (string= lower "iso8859-1") (string= lower "latin1"))
      'iso-8859-1)
     (t 'utf-8))))

;;;; Keyword section: decryption

(defun johnson-mdict--decrypt-data (data checksum-bytes)
  "Decrypt MDict encrypted DATA using CHECKSUM-BYTES.
CHECKSUM-BYTES is a 4-byte unibyte string (little-endian Adler-32
of the keyword section header).  Returns a new unibyte string.
Used for both keyword index blocks and keyword data blocks when
Encrypted=\"2\"."
  ;; Key = RIPEMD-128(checksum_bytes + "\x95\x36\x00\x00")
  (let* ((key-input (concat checksum-bytes "\x95\x36\x00\x00"))
         (key (johnson-ripemd128-hash key-input))
         (key-len (length key))
         (data-len (length data))
         (result (make-string data-len 0))
         (previous #x36))
    (dotimes (i data-len)
      (let* ((byte (aref data i))
             ;; Nibble swap.
             (swapped (logand (logior (ash byte -4) (ash byte 4)) #xff))
             ;; XOR with previous, index, and key byte.
             (decrypted (logxor swapped previous (logand i #xff)
                                (aref key (mod i key-len)))))
        (aset result i decrypted)
        (setq previous byte)))
    result))

;;;; Keyword section parsing

(defun johnson-mdict--parse-keyword-section (path)
  "Parse the keyword section of the MDict file at PATH.
Returns a vector of (HEADWORD . RECORD-OFFSET) cons cells."
  (or (gethash path johnson-mdict--keyword-cache)
      (let ((result (johnson-mdict--do-parse-keyword-section path)))
        (puthash path result johnson-mdict--keyword-cache)
        result)))

(defun johnson-mdict--do-parse-keyword-section (path)
  "Internal: parse the keyword section from PATH."
  (let* ((header (johnson-mdict--parse-header path))
         (num-width (plist-get header :num-width))
         (encoding (plist-get header :encoding))
         (encrypted (plist-get header :encrypted))
         (kw-offset (plist-get header :header-end)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Read keyword section header.
      ;; For v2.0: 5 * 8 bytes = 40 bytes (+ 4 bytes checksum)
      ;; For v1.2: 4 * 4 bytes = 16 bytes
      (let* ((kw-header-size (if (= num-width 8) 44 16))
             (_ (insert-file-contents-literally path nil kw-offset
                                                (+ kw-offset kw-header-size)))
             (raw (buffer-string))
             (pos 0)
             (num-blocks (johnson-mdict--read-number raw pos num-width))
             (_num-entries (progn (cl-incf pos num-width)
                                  (johnson-mdict--read-number raw pos num-width)))
             (index-decompressed-size
              (when (= num-width 8)
                (cl-incf pos num-width)
                (johnson-mdict--read-number raw pos num-width)))
             (index-size (progn (cl-incf pos num-width)
                                (johnson-mdict--read-number raw pos num-width)))
             (_block-size (progn (cl-incf pos num-width)
                                  (johnson-mdict--read-number raw pos num-width)))
             ;; Checksum (4 bytes after the 5 numbers for v2.0).
             (checksum-bytes
              (when (= num-width 8)
                (substring raw 40 44)))
             ;; File position where keyword index data starts.
             (index-start (+ kw-offset kw-header-size)))
        ;; Ignore index-decompressed-size warning.
        (ignore index-decompressed-size)
        ;; Read the keyword index data.
        (erase-buffer)
        (insert-file-contents-literally path nil index-start
                                        (+ index-start index-size))
        (let ((index-data (buffer-string)))
          ;; Decrypt if needed.
          (when (= encrypted 2)
            (setq index-data
                  (johnson-mdict--decrypt-data
                   index-data checksum-bytes)))
          ;; For v2.0, index data may be compressed.
          (when (= num-width 8)
            (setq index-data
                  (johnson-mdict--decompress-block index-data)))
          ;; Parse keyword block info entries from index data.
          (let ((block-infos (johnson-mdict--parse-kw-block-infos
                              index-data num-blocks num-width encoding))
                ;; File position where keyword blocks start.
                (blocks-start (+ index-start index-size))
                (all-entries nil))
            ;; Read and parse each keyword block.
            (dolist (info block-infos)
              (let* ((comp-size (car info))
                     (decomp-size (cdr info)))
                ;; Ignore decomp-size for now (used for validation).
                (ignore decomp-size)
                (erase-buffer)
                (insert-file-contents-literally path nil blocks-start
                                                (+ blocks-start comp-size))
                (let* ((block-data (buffer-string))
                       ;; Decrypt keyword blocks if encrypted.
                       (block-data (if (= encrypted 2)
                                       (johnson-mdict--decrypt-data
                                        block-data checksum-bytes)
                                     block-data))
                       (decompressed
                        (johnson-mdict--decompress-block block-data))
                       (entries (johnson-mdict--parse-kw-block-entries
                                 decompressed num-width encoding)))
                  (setq all-entries (nconc all-entries entries)))
                (cl-incf blocks-start comp-size)))
            ;; Store keyword section end offset for record section.
            (puthash (concat path ":kw-end") blocks-start
                     johnson-mdict--header-cache)
            (vconcat all-entries)))))))

(defun johnson-mdict--parse-kw-block-infos (data num-blocks num-width encoding)
  "Parse keyword block info entries from DATA.
NUM-BLOCKS is the number of blocks.  NUM-WIDTH is 4 or 8.
ENCODING is the coding system.
Returns a list of (COMP-SIZE . DECOMP-SIZE) cons cells."
  (let ((pos 0)
        (infos nil))
    (dotimes (_ num-blocks)
      ;; Each entry: num-entries, first-word-size, first-word, last-word-size,
      ;; last-word, comp-size, decomp-size.
      (let* (;; Number of entries in this block.
             (_num-entries-in-block
              (johnson-mdict--read-number data pos num-width))
             (_ (cl-incf pos num-width))
             ;; First word size (in bytes for v2.0, chars for v1.2).
             (first-size (johnson-mdict--parse-kw-text-size data pos num-width encoding))
             (_ (cl-incf pos (car first-size)))
             ;; Last word.
             (last-size (johnson-mdict--parse-kw-text-size data pos num-width encoding))
             (_ (cl-incf pos (car last-size)))
             ;; Compressed and decompressed sizes.
             (comp-size (johnson-mdict--read-number data pos num-width))
             (_ (cl-incf pos num-width))
             (decomp-size (johnson-mdict--read-number data pos num-width))
             (_ (cl-incf pos num-width)))
        (push (cons comp-size decomp-size) infos)))
    (nreverse infos)))

(defun johnson-mdict--parse-kw-text-size (data pos num-width encoding)
  "Parse a keyword text field size and skip the text.
DATA is the index data, POS is the current position.
Returns (TOTAL-BYTES-CONSUMED)."
  (if (= num-width 8)
      ;; v2.0: 2-byte size (big-endian, in bytes), then text, then null terminator.
      (let* ((text-size (johnson-mdict--u16be data pos))
             (null-size (if (eq encoding 'utf-16le) 2 1))
             (total (+ 2 text-size null-size)))
        (cons total nil))
    ;; v1.2: 1-byte size (in chars), then text, then null byte.
    (let* ((char-count (aref data pos))
           (byte-size (if (eq encoding 'utf-16le) (* char-count 2) char-count))
           (total (+ 1 byte-size 1)))
      (cons total nil))))

(defun johnson-mdict--parse-kw-block-entries (data num-width encoding)
  "Parse keyword entries from a decompressed keyword block.
DATA is the decompressed block data.  NUM-WIDTH is 4 or 8.
ENCODING is the coding system.
Returns a list of (HEADWORD . RECORD-OFFSET) cons cells."
  (let ((pos 0)
        (data-len (length data))
        (entries nil)
        (null-size (if (eq encoding 'utf-16le) 2 1)))
    (while (< pos data-len)
      (let* ((record-offset (johnson-mdict--read-number data pos num-width))
             (_ (cl-incf pos num-width))
             ;; Find null terminator.
             (text-start pos)
             (text-end
              (if (= null-size 2)
                  ;; UTF-16LE: look for \0\0.
                  (let ((p pos))
                    (while (and (< (+ p 1) data-len)
                                (not (and (= (aref data p) 0)
                                          (= (aref data (+ p 1)) 0))))
                      (cl-incf p 2))
                    p)
                ;; Single byte null.
                (let ((p pos))
                  (while (and (< p data-len) (/= (aref data p) 0))
                    (cl-incf p))
                  p)))
             (headword (decode-coding-string
                        (substring data text-start text-end)
                        encoding)))
        (setq pos (+ text-end null-size))
        (push (cons headword record-offset) entries)))
    (nreverse entries)))

;;;; Record section parsing

(defun johnson-mdict--parse-record-section (path)
  "Parse the record section index of the MDict file at PATH.
Returns a plist with:
  :blocks - vector of (COMP-SIZE DECOMP-SIZE FILE-OFFSET) triples
  :record-blocks-offset - file offset where record block data starts"
  (or (gethash path johnson-mdict--record-index-cache)
      (let ((result (johnson-mdict--do-parse-record-section path)))
        (puthash path result johnson-mdict--record-index-cache)
        result)))

(defun johnson-mdict--do-parse-record-section (path)
  "Internal: parse the record section index from PATH."
  (let* ((header (johnson-mdict--parse-header path))
         (num-width (plist-get header :num-width))
         ;; Record section starts where keyword section ended.
         (kw-end (gethash (concat path ":kw-end")
                          johnson-mdict--header-cache))
         (rec-offset (or kw-end
                         ;; If not cached, we need to parse keywords first.
                         (progn
                           (johnson-mdict--parse-keyword-section path)
                           (gethash (concat path ":kw-end")
                                    johnson-mdict--header-cache)))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      ;; Record section header:
      ;; v2.0: num-blocks(8) + num-entries(8) + info-size(8) + blocks-total-size(8) = 32
      ;; v1.2: num-blocks(4) + num-entries(4) + info-size(4) + blocks-total-size(4) = 16
      (let* ((rec-header-size (* 4 num-width)))
        (insert-file-contents-literally path nil rec-offset
                                        (+ rec-offset rec-header-size))
        (let* ((raw (buffer-string))
               (num-blocks (johnson-mdict--read-number raw 0 num-width))
               ;; Skip num-entries.
               (info-size (johnson-mdict--read-number
                           raw (* 2 num-width) num-width))
               ;; Block info entries follow the header.
               (info-start (+ rec-offset rec-header-size)))
          (erase-buffer)
          (insert-file-contents-literally path nil info-start
                                          (+ info-start info-size))
          (let* ((info-raw (buffer-string))
                 (blocks (make-vector num-blocks nil))
                 (blocks-data-start (+ info-start info-size))
                 (file-off blocks-data-start))
            (dotimes (i num-blocks)
              (let* ((base (* i 2 num-width))
                     (comp-size (johnson-mdict--read-number
                                 info-raw base num-width))
                     (decomp-size (johnson-mdict--read-number
                                   info-raw (+ base num-width) num-width)))
                (aset blocks i (list comp-size decomp-size file-off))
                (cl-incf file-off comp-size)))
            (list :blocks blocks
                  :record-blocks-offset blocks-data-start)))))))

;;;; Record retrieval

(defun johnson-mdict--locate-record (path record-offset)
  "Find the record block and offset within it for RECORD-OFFSET.
Returns (BLOCK-INDEX . LOCAL-OFFSET)."
  (let* ((rec-info (johnson-mdict--parse-record-section path))
         (blocks (plist-get rec-info :blocks))
         (num-blocks (length blocks))
         (cumulative 0)
         (found nil))
    (dotimes (i num-blocks)
      (unless found
        (let ((decomp-size (nth 1 (aref blocks i))))
          (if (< record-offset (+ cumulative decomp-size))
              (setq found (cons i (- record-offset cumulative)))
            (cl-incf cumulative decomp-size)))))
    (or found
        (error "Record offset %d out of range" record-offset))))

(defun johnson-mdict--read-record-block (path block-index)
  "Read and decompress record BLOCK-INDEX from the MDict file at PATH.
Returns a unibyte string."
  (let* ((cache-key (cons path block-index))
         (cached (johnson-mdict--block-cache-get cache-key)))
    (or cached
        (let* ((rec-info (johnson-mdict--parse-record-section path))
               (block-info (aref (plist-get rec-info :blocks) block-index))
               (comp-size (nth 0 block-info))
               (file-off (nth 2 block-info))
               (raw (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally
                       path nil file-off (+ file-off comp-size))
                      (buffer-string)))
               (decompressed (johnson-mdict--decompress-block raw)))
          (johnson-mdict--block-cache-put cache-key decompressed)
          decompressed))))

;;;; Format detection

(defun johnson-mdict-detect (path)
  "Return non-nil if PATH appears to be an MDict .mdx file."
  (and (string-suffix-p ".mdx" path t)
       (condition-case nil
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally path nil 0 4)
             ;; Header size should be a reasonable positive number.
             (let ((header-size (johnson-mdict--u32be (buffer-string) 0)))
               (and (> header-size 0)
                    (< header-size 1048576))))
         (error nil))))

;;;; Metadata parsing

(defun johnson-mdict-parse-metadata (path)
  "Parse metadata from the MDict file at PATH.
Returns a plist (:name STRING :source-lang STRING :target-lang STRING)."
  (let* ((header (johnson-mdict--parse-header path))
         (title (plist-get header :title))
         (name (if (string-empty-p title)
                   (file-name-base path)
                 title)))
    (list :name name
          :source-lang ""
          :target-lang "")))

;;;; Index building

(defun johnson-mdict-build-index (path callback)
  "Parse the MDict dictionary at PATH, calling CALLBACK for each entry.
CALLBACK is called as (funcall CALLBACK headword record-offset 0)
where record-offset is the byte offset into the decompressed record stream."
  (let ((keywords (johnson-mdict--parse-keyword-section path)))
    (cl-loop for entry across keywords
             do (funcall callback (car entry) (cdr entry) 0))))

;;;; Entry retrieval

(defun johnson-mdict-retrieve-entry (path record-offset _byte-size)
  "Retrieve the entry data from the MDict dictionary at PATH.
RECORD-OFFSET is the offset into the decompressed record stream.
Returns a raw unibyte string."
  (setq johnson-mdict--current-dict-dir (file-name-directory path))
  (let* ((loc (johnson-mdict--locate-record path record-offset))
         (block-idx (car loc))
         (local-offset (cdr loc))
         (block-data (johnson-mdict--read-record-block path block-idx))
         (header (johnson-mdict--parse-header path))
         (encoding (plist-get header :encoding)))
    ;; Entry data starts at local-offset.
    ;; Find end: for MDict, entries end at the start of the next entry
    ;; or end of the block.  Since we stored cumulative offsets, we need
    ;; to find the length by looking at the next keyword's offset.
    ;; For simplicity, extract from local-offset to the next null-terminated
    ;; boundary or end of block.
    ;;
    ;; Actually, in MDict the record size is determined by the difference
    ;; between consecutive record offsets.  We compute it from the keyword list.
    (let* ((keywords (johnson-mdict--parse-keyword-section path))
           (entry-size (johnson-mdict--find-entry-size
                        keywords record-offset (length block-data) local-offset))
           (entry-data (substring block-data local-offset
                                  (+ local-offset entry-size))))
      ;; Return the decoded text.
      (encode-coding-string
       (decode-coding-string entry-data encoding)
       'utf-8))))

(defun johnson-mdict--find-entry-size (keywords record-offset block-data-len local-offset)
  "Find the size of the entry at RECORD-OFFSET.
KEYWORDS is the keyword vector.  BLOCK-DATA-LEN is the decompressed
block length.  LOCAL-OFFSET is the offset within the block.
Returns the number of bytes."
  (let ((next-offset nil)
        (len (length keywords)))
    ;; Find the next record offset after this one.
    (dotimes (i len)
      (let ((offset (cdr (aref keywords i))))
        (when (and (> offset record-offset)
                   (or (null next-offset)
                       (< offset next-offset)))
          (setq next-offset offset))))
    (if next-offset
        ;; The entry size is the difference between offsets, but capped
        ;; at the remaining block data.
        (min (- next-offset record-offset)
             (- block-data-len local-offset))
      ;; Last entry: extends to end of block.
      (- block-data-len local-offset))))

;;;; Entry rendering

(defun johnson-mdict-render-entry (raw-data)
  "Render MDict entry RAW-DATA into the current buffer.
RAW-DATA is a UTF-8 encoded string containing HTML content."
  (let ((text (decode-coding-string raw-data 'utf-8))
        (start (point))
        (johnson-html--current-dict-dir johnson-mdict--current-dict-dir))
    (insert text)
    (let ((end (point)))
      (johnson-html-render-region start end))))

;;;; MDD resource lookup

(defun johnson-mdict--mdd-path (mdx-path)
  "Return the .mdd file path corresponding to MDX-PATH, or nil."
  (let ((mdd (concat (file-name-sans-extension mdx-path) ".mdd")))
    (when (file-exists-p mdd)
      mdd)))

;;;; Cache clearing

(defun johnson-mdict-clear-caches ()
  "Clear all MDict caches."
  (clrhash johnson-mdict--header-cache)
  (clrhash johnson-mdict--keyword-cache)
  (clrhash johnson-mdict--record-index-cache)
  (setq johnson-mdict--block-cache nil))

;;;; Format registration

(provide 'johnson-mdict)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "mdict"
   :extensions '("mdx")
   :detect #'johnson-mdict-detect
   :parse-metadata #'johnson-mdict-parse-metadata
   :build-index #'johnson-mdict-build-index
   :retrieve-entry #'johnson-mdict-retrieve-entry
   :render-entry #'johnson-mdict-render-entry))

;;; johnson-mdict.el ends here
