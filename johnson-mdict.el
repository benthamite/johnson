;;; johnson-mdict.el --- MDict format backend for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides the MDict format backend for the johnson
;; dictionary package.  It handles parsing of MDict .mdx (dictionary)
;; and .mdd (resource) files, including support for v1.2 and v2.0
;; formats, zlib compression, and Encrypted="2" keyword decryption.

;;; Code:

(require 'cl-lib)
(require 'johnson-html)
(require 'johnson-ripemd128)

(declare-function johnson-register-format "johnson")
(declare-function johnson-insert-audio-button "johnson")

;;;; Binary integer helpers — from unibyte strings

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

;;;; Internal variables

(defvar johnson-mdict--header-cache (make-hash-table :test #'equal)
  "Cache of parsed MDict headers.
Maps file path to a header plist.")

(defvar johnson-mdict--record-meta-cache (make-hash-table :test #'equal)
  "Cache of parsed record section metadata.
Maps file path to a plist with :blocks and :data-start.")

(defvar johnson-mdict--offset-cache (make-hash-table :test #'equal)
  "Cache of sorted record offsets per dictionary path.
Maps path to a sorted vector of record offsets.")

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

;;;; LZO1X decompression

(defun johnson-mdict--lzo-decompress (data decompressed-size)
  "Decompress LZO1X-1 compressed DATA, returning a unibyte string.
DATA is a unibyte string of compressed bytes (raw LZO1X payload,
without MDict's 8-byte block header).  DECOMPRESSED-SIZE is the
expected output size in bytes.

Pure Elisp port of the canonical LZO1X decompressor from the Linux
kernel (lib/lzo/lzo1x_decompress.c)."
  (let* ((ip 0)
         (out (string-to-unibyte (make-string decompressed-size 0)))
         (op 0)
         (tk 0)                         ; token / working length
         (state nil)
         m-pos)
    (cl-flet*
        ((u8 ()
           (prog1 (aref data ip) (cl-incf ip)))
         (le16 ()
           (prog1 (logior (aref data ip) (ash (aref data (1+ ip)) 8))
             (cl-incf ip 2)))
         (copy-lit (n)
           (dotimes (_ n)
             (aset out op (aref data ip))
             (cl-incf op)
             (cl-incf ip)))
         (copy-match (len src)
           (dotimes (_ len)
             (aset out op (aref out src))
             (cl-incf op)
             (cl-incf src)))
         (varlen (base)
           (when (= tk 0)
             (while (= (aref data ip) 0)
               (cl-incf tk 255)
               (cl-incf ip))
             (cl-incf tk (+ base (u8))))))

      (let ((b (u8)))
        (cond
         ((> b 17)
          (setq tk (- b 17))
          (if (>= tk 4)
              (progn (copy-lit tk) (setq state :first-literal-run))
            (setq state :match-next)))
         (t
          (cl-decf ip)
          (setq state :outer))))

      (catch 'lzo-eof
        (while state
          (pcase state
            (:outer
             (setq tk (u8))
             (if (>= tk 16)
                 (setq state :match)
               (varlen 15)
               (copy-lit (+ tk 3))
               (setq state :first-literal-run)))
            (:first-literal-run
             (setq tk (u8))
             (if (>= tk 16)
                 (setq state :match)
               (setq m-pos (- op (+ 1 #x0800) (ash tk -2) (ash (u8) 2)))
               (copy-match 3 m-pos)
               (setq state :match-done)))
            (:match
             (cond
              ((>= tk 64)
               (let ((next-b (u8)))
                 (setq m-pos (- op 1
                                (logand (ash tk -2) 7)
                                (ash next-b 3)))
                 (copy-match (1+ (ash tk -5)) m-pos)))
              ((>= tk 32)
               (setq tk (logand tk 31))
               (varlen 31)
               (let ((w (le16)))
                 (setq m-pos (- op 1 (ash w -2)))
                 (copy-match (+ tk 2) m-pos)))
              ((>= tk 16)
               (setq m-pos op)
               (cl-decf m-pos (ash (logand tk 8) 11))
               (setq tk (logand tk 7))
               (varlen 7)
               (let ((w (le16)))
                 (cl-decf m-pos (ash w -2))
                 (when (= m-pos op)
                   (throw 'lzo-eof nil))
                 (cl-decf m-pos #x4000)
                 (copy-match (+ tk 2) m-pos)))
              (t
               (let ((next-b (u8)))
                 (setq m-pos (- op 1 (ash tk -2) (ash next-b 2)))
                 (copy-match 2 m-pos))))
             (setq state :match-done))
            (:match-done
             (setq tk (logand (aref data (- ip 2)) 3))
             (if (= tk 0)
                 (setq state :outer)
               (setq state :match-next)))
            (:match-next
             (copy-lit tk)
             (setq tk (u8))
             (setq state :match))))))
    out))

;;;; Block decompression

(defun johnson-mdict--decompress-block (data &optional decomp-size)
  "Decompress a single MDict data block.
DATA is a unibyte string containing the raw block (with 8-byte header).
DECOMP-SIZE is the expected decompressed size (required for LZO blocks).
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
       ;; LZO compression: strip 8-byte header, decompress.
       (unless decomp-size
         (error "LZO decompression requires decompressed size"))
       (johnson-mdict--lzo-decompress (substring data 8) decomp-size))
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
  :header-end  - file byte offset past the header"
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
              :header-end total-read)))))

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

;;;; File reading helper

(defun johnson-mdict--read-file-region (path start size)
  "Read SIZE bytes from PATH starting at file offset START.
Returns a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil start (+ start size))
    (buffer-string)))

;;;; Keyword section: decryption

(defun johnson-mdict--decrypt-data (data checksum-bytes)
  "Decrypt MDict encrypted DATA using CHECKSUM-BYTES.
CHECKSUM-BYTES is a 4-byte unibyte string (bytes 4-8 of the
compressed block, i.e. the Adler-32 checksum).  Returns a new
unibyte string."
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
Returns a list of (HEADWORD . RECORD-OFFSET) cons cells."
  (let* ((header (johnson-mdict--parse-header path))
         (num-width (plist-get header :num-width))
         (encoding (plist-get header :encoding))
         (encrypted (plist-get header :encrypted))
         (kw-offset (plist-get header :header-end)))
    ;; Read keyword section header.
    ;; v2.0: 5 * 8 = 40 bytes + 4 bytes checksum = 44 bytes
    ;; v1.2: 4 * 4 = 16 bytes (no checksum in older format)
    (let* ((kw-header-size (if (= num-width 8) 44 16))
           (kw-header-data (johnson-mdict--read-file-region
                            path kw-offset kw-header-size))
           (pos 0)
           (num-blocks (johnson-mdict--read-number kw-header-data pos num-width))
           (_num-entries (progn (cl-incf pos num-width)
                                (johnson-mdict--read-number
                                 kw-header-data pos num-width)))
           ;; v2.0 has decomp_info_size field, v1.2 does not.
           (_decomp-info-size
            (when (= num-width 8)
              (cl-incf pos num-width)
              (johnson-mdict--read-number kw-header-data pos num-width)))
           (index-comp-size (progn (cl-incf pos num-width)
                                   (johnson-mdict--read-number
                                    kw-header-data pos num-width)))
           (total-kw-block-size (progn (cl-incf pos num-width)
                                       (johnson-mdict--read-number
                                        kw-header-data pos num-width)))
           ;; File positions.
           (index-start (+ kw-offset kw-header-size))
           (blocks-start (+ index-start index-comp-size))
           (kw-section-end (+ blocks-start total-kw-block-size)))
      ;; Read keyword block info (index) data.
      (let ((index-data (johnson-mdict--read-file-region
                         path index-start index-comp-size)))
        ;; Decrypt the block info payload if encrypted.
        ;; Key is derived from bytes 4-8 (the Adler-32 checksum) of the
        ;; compressed index data.  Only the payload after the 8-byte
        ;; compression header is encrypted.
        (when (= encrypted 2)
          (let ((key-bytes (substring index-data 4 8)))
            (setq index-data
                  (concat (substring index-data 0 8)
                          (johnson-mdict--decrypt-data
                           (substring index-data 8) key-bytes)))))
        ;; Decompress (v2.0 always compresses; v1.2 may not).
        (when (= num-width 8)
          (setq index-data (johnson-mdict--decompress-block index-data)))
        ;; Parse block info to get per-block sizes.
        (let ((block-infos (johnson-mdict--parse-kw-block-infos
                            index-data num-blocks num-width encoding))
              ;; Read all keyword blocks data.
              (blocks-data (johnson-mdict--read-file-region
                            path blocks-start total-kw-block-size))
              (block-pos 0)
              (all-entries nil)
              (block-index 0))
          (dolist (info block-infos)
            (let* ((comp-size (car info))
                   (decomp-size (cdr info))
                   (block-data (substring blocks-data block-pos
                                          (+ block-pos comp-size))))
              ;; Decrypt even-indexed keyword blocks if encrypted.
              ;; Key from bytes 4-8, only payload after 8-byte header.
              (when (and (= encrypted 2) (= (mod block-index 2) 0))
                (let ((kb-key (substring block-data 4 8)))
                  (setq block-data
                        (concat (substring block-data 0 8)
                                (johnson-mdict--decrypt-data
                                 (substring block-data 8) kb-key)))))
              (let* ((decompressed (johnson-mdict--decompress-block
                                    block-data decomp-size))
                     (entries (johnson-mdict--parse-kw-block-entries
                               decompressed num-width encoding)))
                (push entries all-entries))
              (cl-incf block-pos comp-size)
              (cl-incf block-index)))
          ;; Cache the keyword section end for record section parsing.
          (puthash (concat path ":kw-end") kw-section-end
                   johnson-mdict--header-cache)
          ;; all-entries is a list of sub-lists (pushed in reverse
          ;; block order); flatten into a single list.
          (apply #'nconc (nreverse all-entries)))))))

(defun johnson-mdict--parse-kw-block-infos (data num-blocks num-width encoding)
  "Parse keyword block info entries from DATA.
NUM-BLOCKS is the number of blocks.  NUM-WIDTH is 4 or 8.
ENCODING is the coding system.
Returns a list of (COMP-SIZE . DECOMP-SIZE) cons cells."
  (let ((pos 0)
        (infos nil))
    (dotimes (_ num-blocks)
      ;; Number of entries in this block.
      (let ((_num-entries-in-block
             (johnson-mdict--read-number data pos num-width)))
        (cl-incf pos num-width)
        ;; First headword: skip text field.
        (let ((skip (johnson-mdict--skip-kw-text data pos num-width encoding)))
          (cl-incf pos skip))
        ;; Last headword: skip text field.
        (let ((skip (johnson-mdict--skip-kw-text data pos num-width encoding)))
          (cl-incf pos skip))
        ;; Compressed and decompressed sizes.
        (let ((comp-size (johnson-mdict--read-number data pos num-width))
              (decomp-size (johnson-mdict--read-number
                            data (+ pos num-width) num-width)))
          (cl-incf pos (* 2 num-width))
          (push (cons comp-size decomp-size) infos))))
    (nreverse infos)))

(defun johnson-mdict--skip-kw-text (data pos num-width encoding)
  "Compute how many bytes to skip for a keyword text field.
DATA is the index data, POS is the current position.
NUM-WIDTH is 4 or 8, ENCODING is the coding system.
Returns the number of bytes consumed."
  (if (= num-width 8)
      ;; v2.0: 2-byte size (BE, in bytes), then text, then null terminator.
      (let* ((text-size (johnson-mdict--u16be data pos))
             (null-size (if (eq encoding 'utf-16le) 2 1)))
        (+ 2 text-size null-size))
    ;; v1.2: 1-byte size (in chars), then text (no null terminator).
    (let* ((char-count (aref data pos))
           (byte-size (if (eq encoding 'utf-16le) (* char-count 2) char-count)))
      (+ 1 byte-size))))

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
             (text-start pos)
             ;; Find null terminator.
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
  :data-start - file offset where record block data starts"
  (or (gethash path johnson-mdict--record-meta-cache)
      (let ((result (johnson-mdict--do-parse-record-section path)))
        (puthash path result johnson-mdict--record-meta-cache)
        result)))

(defun johnson-mdict--do-parse-record-section (path)
  "Internal: parse the record section index from PATH."
  (let* ((header (johnson-mdict--parse-header path))
         (num-width (plist-get header :num-width))
         ;; Record section starts where keyword section ended.
         (kw-end (or (gethash (concat path ":kw-end")
                              johnson-mdict--header-cache)
                     ;; If not cached, parse keywords first to set it.
                     (progn
                       (johnson-mdict--parse-keyword-section path)
                       (gethash (concat path ":kw-end")
                                johnson-mdict--header-cache)))))
    ;; Record section header: 4 fields of num-width each.
    (let* ((rec-header-size (* 4 num-width))
           (rec-header-data (johnson-mdict--read-file-region
                             path kw-end rec-header-size))
           (num-blocks (johnson-mdict--read-number rec-header-data 0 num-width))
           ;; Skip num-entries (field 2).
           (info-size (johnson-mdict--read-number
                       rec-header-data (* 2 num-width) num-width))
           ;; Block info entries follow the header.
           (info-start (+ kw-end rec-header-size))
           (info-data (johnson-mdict--read-file-region
                       path info-start info-size))
           (blocks (make-vector num-blocks nil))
           (data-start (+ info-start info-size))
           (file-off data-start))
      (dotimes (i num-blocks)
        (let* ((base (* i 2 num-width))
               (comp-size (johnson-mdict--read-number info-data base num-width))
               (decomp-size (johnson-mdict--read-number
                             info-data (+ base num-width) num-width)))
          (aset blocks i (list comp-size decomp-size file-off))
          (cl-incf file-off comp-size)))
      (list :blocks blocks :data-start data-start))))

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
               (decomp-size (nth 1 block-info))
               (file-off (nth 2 block-info))
               (raw (johnson-mdict--read-file-region path file-off comp-size))
               (decompressed (johnson-mdict--decompress-block raw decomp-size)))
          (johnson-mdict--block-cache-put cache-key decompressed)
          decompressed))))

;;;; Offset cache for entry size computation

(defun johnson-mdict--ensure-offset-cache (path entries)
  "Ensure the sorted offset cache exists for PATH.
ENTRIES is a list of (HEADWORD . OFFSET) cons cells (or nil to
parse them from scratch)."
  (unless (gethash path johnson-mdict--offset-cache)
    (let* ((pairs (or entries
                      (johnson-mdict--parse-keyword-section path)))
           (offsets (sort (mapcar #'cdr pairs) #'<)))
      ;; `delete-dups' is O(n) on a sorted list, unlike
      ;; `cl-remove-duplicates' which is O(n²).
      (puthash path (vconcat (delete-dups offsets))
               johnson-mdict--offset-cache))))

(defun johnson-mdict--next-offset (path entry-offset)
  "Find the next record offset after ENTRY-OFFSET in PATH.
Returns the next offset, or -1 if this is the last entry."
  (johnson-mdict--ensure-offset-cache path nil)
  (let* ((offsets (gethash path johnson-mdict--offset-cache))
         (len (length offsets))
         ;; Binary search for entry-offset.
         (lo 0)
         (hi (1- len)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (aref offsets mid) entry-offset)
            (setq lo (1+ mid))
          (setq hi mid))))
    ;; lo should now point to entry-offset's position.
    (if (< (1+ lo) len)
        (aref offsets (1+ lo))
      -1)))

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
  (let ((entries (johnson-mdict--parse-keyword-section path)))
    ;; Build and cache the sorted offset vector.
    (johnson-mdict--ensure-offset-cache path entries)
    (dolist (entry entries)
      (funcall callback (car entry) (cdr entry) 0))))

;;;; Entry retrieval

(defun johnson-mdict-retrieve-entry (path record-offset _byte-size)
  "Retrieve the entry data from the MDict dictionary at PATH.
RECORD-OFFSET is the offset into the decompressed record stream.
Returns the entry data as a decoded string."
  (setq johnson-mdict--current-dict-dir (file-name-directory path))
  (let* ((header (johnson-mdict--parse-header path))
         (encoding (plist-get header :encoding))
         (loc (johnson-mdict--locate-record path record-offset))
         (block-idx (car loc))
         (local-offset (cdr loc))
         (block-data (johnson-mdict--read-record-block path block-idx))
         (next-offset (johnson-mdict--next-offset path record-offset))
         (entry-size
          (if (= next-offset -1)
              ;; Last entry: extends to end of block.
              (- (length block-data) local-offset)
            ;; Compute local end within the block.
            (min (- next-offset record-offset)
                 (- (length block-data) local-offset))))
         (entry-data (substring block-data local-offset
                                (+ local-offset entry-size))))
    (decode-coding-string entry-data encoding)))

;;;; Entry rendering

(defun johnson-mdict-render-entry (data)
  "Render MDict entry DATA into the current buffer.
DATA is a decoded string containing HTML content."
  (let ((start (point))
        (johnson-html--current-dict-dir johnson-mdict--current-dict-dir)
        (johnson-html--current-dict-path nil))
    (insert data)
    (johnson-html-render-region start (point))))

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
  (clrhash johnson-mdict--record-meta-cache)
  (clrhash johnson-mdict--offset-cache)
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
