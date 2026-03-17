;;; johnson-bgl.el --- BGL (Babylon) format backend for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides the BGL (Babylon) format backend for the johnson
;; dictionary package.  It handles parsing of Babylon .bgl dictionary
;; files, including gzip decompression of the data stream, block-based
;; parsing of metadata and entries, and HTML rendering of definitions.

;;; Code:

(require 'cl-lib)
(require 'johnson-html)
(require 'johnson-binary)

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")

;;;; Language code table

(defconst johnson-bgl--language-names
  ["English" "French" "Italian" "Spanish" "Dutch"
   "Portuguese" "German" "Russian" "Japanese"
   "Traditional Chinese" "Simplified Chinese" "Greek"
   "Korean" "Turkish" "Hebrew" "Arabic" "Thai" "Other"]
  "BGL language code to language name mapping.")

(defun johnson-bgl--language-name (code)
  "Return the language name for BGL language CODE."
  (if (< code (length johnson-bgl--language-names))
      (aref johnson-bgl--language-names code)
    "Other"))

;;;; Charset code table

(defconst johnson-bgl--charset-alist
  '((#x41 . windows-1252)
    (#x42 . windows-1252)
    (#x43 . windows-1250)
    (#x44 . cp1251)
    (#x45 . cp932)
    (#x46 . cp950)
    (#x47 . cp936)
    (#x48 . cp1257)
    (#x49 . cp1253)
    (#x4A . cp949)
    (#x4B . windows-1254)
    (#x4C . windows-1255)
    (#x4D . windows-1256)
    (#x4E . cp874))
  "BGL charset code to Emacs coding system mapping.")

(defun johnson-bgl--charset-coding-system (code)
  "Return the Emacs coding system for BGL charset CODE.
Falls back to `utf-8' if the code is unknown."
  (or (cdr (assq code johnson-bgl--charset-alist))
      'utf-8))

;;;; Cache buffer for decompressed stream

(defvar johnson-bgl--cache-buffers (make-hash-table :test #'equal)
  "Cache of decompressed BGL data buffers.
Maps file path to a buffer containing the decompressed gzip stream.")

(defvar johnson-bgl--metadata-cache (make-hash-table :test #'equal)
  "Cache of parsed BGL metadata.
Maps file path to a metadata plist.")

(defun johnson-bgl--cache-buffer-name (path)
  "Return the cache buffer name for the BGL dictionary at PATH."
  (format " *johnson-bgl-cache: %s" path))

(defun johnson-bgl--get-buffer (path)
  "Return (or create) a cache buffer with decompressed BGL data for PATH.
The buffer contains the raw decompressed gzip stream as unibyte data."
  (or (gethash path johnson-bgl--cache-buffers)
      (let* ((buf-name (johnson-bgl--cache-buffer-name path))
             (existing (get-buffer buf-name)))
        (if existing
            (progn
              (puthash path existing johnson-bgl--cache-buffers)
              existing)
          (let ((buf (johnson-bgl--create-cache-buffer path buf-name)))
            (puthash path buf johnson-bgl--cache-buffers)
            buf)))))

(defun johnson-bgl--create-cache-buffer (path buf-name)
  "Create and populate a cache buffer named BUF-NAME for BGL file at PATH."
  (with-current-buffer (generate-new-buffer buf-name)
    (buffer-disable-undo)
    (fundamental-mode)
    (set-buffer-multibyte nil)
    (let ((inhibit-read-only t))
      (johnson-bgl--decompress-into-buffer path))
    (setq buffer-read-only t)
    (current-buffer)))

(defun johnson-bgl--decompress-into-buffer (path)
  "Decompress the gzip stream from BGL file PATH into the current buffer.
Expects the current buffer to be unibyte and writable.

Some BGL files produced by Babylon have a zeroed-out CRC32 in the
gzip trailer.  Standard gzip decompression (via `jka-compr') rejects
these with a CRC error even though the deflate data is intact.  We
call gzip directly via `call-process' and tolerate exit code 1 (CRC
error) provided that output was produced."
  (let* ((gz-offset (johnson-bgl--gzip-offset path))
         (temp-file (make-temp-file "johnson-bgl-" nil ".gz")))
    (unwind-protect
        (progn
          ;; Extract the gzip portion to a temp file.
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path nil gz-offset)
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) temp-file nil 'silent)))
          ;; Decompress via gzip subprocess.  We use call-process
          ;; rather than jka-compr because some BGL files have invalid
          ;; gzip CRC32 (zeroed out), causing gzip to exit 1 even
          ;; though the deflate payload decompresses fully.
          (let ((exit-code (call-process "gzip" temp-file t nil
                                         "-c" "-q" "-d")))
            (when (and (/= exit-code 0) (= (buffer-size) 0))
              (error "BGL decompression failed for %s (gzip exit %d)"
                     (file-name-nondirectory path) exit-code))))
      (delete-file temp-file))))

;;;; Header parsing

(defun johnson-bgl--read-header (path)
  "Read and validate the BGL header from PATH.
Returns a plist with :magic and :gzip-offset."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 6)
    (let* ((data (buffer-string))
           (magic (substring data 0 4))
           (gz-offset (johnson-binary-u16be data 4)))
      (list :magic magic :gzip-offset gz-offset))))

(defun johnson-bgl--gzip-offset (path)
  "Return the gzip stream offset from the BGL header of PATH."
  (plist-get (johnson-bgl--read-header path) :gzip-offset))

;;;; Block parsing

(cl-defun johnson-bgl--parse-block-header (data pos)
  "Parse a BGL block header from DATA at POS.
Returns a list (TYPE LENGTH NEXT-POS) where TYPE is the block type,
LENGTH is the payload length, and NEXT-POS is the offset past the header.
Returns nil if there are not enough bytes to read the header."
  (let ((data-len (length data)))
    (when (>= pos data-len)
      (cl-return-from johnson-bgl--parse-block-header nil))
    (let* ((byte0 (aref data pos))
           (btype (logand byte0 #x0F))
           (length-nibble (ash byte0 -4))
           (cur (1+ pos)))
      (if (< length-nibble 4)
          ;; Extra bytes encode the length.
          (let* ((extra (1+ length-nibble)))
            (when (> (+ cur extra) data-len)
              (cl-return-from johnson-bgl--parse-block-header nil))
            (let ((length 0))
              (dotimes (i extra)
                (setq length (logior (ash length 8) (aref data (+ cur i)))))
              (list btype length (+ cur extra))))
        ;; Length encoded in the nibble itself.
        (list btype (- length-nibble 4) cur)))))

(defun johnson-bgl--parse-blocks (data)
  "Parse all blocks from decompressed BGL DATA string.
Returns a list of (TYPE PAYLOAD-START PAYLOAD-LENGTH) triples.
DATA is a unibyte string."
  (let ((pos 0)
        (data-len (length data))
        (blocks nil))
    (while (< pos data-len)
      (let ((header (johnson-bgl--parse-block-header data pos)))
        (unless header
          (setq pos data-len))
        (when header
          (let ((btype (nth 0 header))
                (length (nth 1 header))
                (payload-start (nth 2 header)))
            (push (list btype payload-start length) blocks)
            (setq pos (+ payload-start length))))))
    (nreverse blocks)))

;;;; Metadata extraction from blocks

(defun johnson-bgl--parse-metadata-from-blocks (data blocks)
  "Extract metadata from BGL block list BLOCKS with DATA.
Returns a plist with :name, :source-lang, :target-lang,
:source-charset, and :target-charset."
  (let ((name-raw nil)
        (source-lang nil)
        (target-lang nil)
        (source-charset nil)
        (target-charset nil))
    (dolist (block blocks)
      (let ((btype (nth 0 block))
            (payload-start (nth 1 block))
            (payload-length (nth 2 block)))
        ;; Type 3 blocks: byte 0 is the property ID and the value
        ;; follows from byte 1 onward.
        (when (and (= btype 3) (>= payload-length 1))
          (let ((prop-id (aref data payload-start)))
            (pcase prop-id
              (#x01 ; title (keep raw bytes; decode after charset is known)
               (when (> payload-length 1)
                 (setq name-raw (substring data (+ payload-start 1)
                                           (+ payload-start payload-length)))))
              (#x07 ; source language
               (when (>= payload-length 2)
                 (setq source-lang
                       (johnson-bgl--language-name
                        (aref data (+ payload-start 1))))))
              (#x08 ; target language
               (when (>= payload-length 2)
                 (setq target-lang
                       (johnson-bgl--language-name
                        (aref data (+ payload-start 1))))))
              (#x1A ; source charset
               (when (>= payload-length 2)
                 (setq source-charset (aref data (+ payload-start 1)))))
              (#x1B ; target charset
               (when (>= payload-length 2)
                 (setq target-charset (aref data (+ payload-start 1))))))))))
    (list :name (if name-raw
                    (decode-coding-string
                     name-raw
                     (johnson-bgl--charset-coding-system
                      (or source-charset #x41)))
                  "")
          :source-lang (or source-lang "")
          :target-lang (or target-lang "")
          :source-charset (or source-charset #x41)
          :target-charset (or target-charset #x41))))

;;;; Entry parsing from blocks

(defun johnson-bgl--entry-block-p (btype)
  "Return non-nil if BTYPE is a BGL entry block type."
  (memq btype '(1 7 10 13)))

(cl-defun johnson-bgl--parse-entry-block (data payload-start payload-length)
  "Parse an entry block from DATA at PAYLOAD-START with PAYLOAD-LENGTH.
Returns a list of (HEADWORD DEFINITION-OFFSET DEFINITION-LENGTH) triples.
DEFINITION-OFFSET and DEFINITION-LENGTH are byte positions in DATA.
Multiple triples are returned when alternate headwords are present."
  (when (< payload-length 4)
    (cl-return-from johnson-bgl--parse-entry-block nil))
  (let* ((hw-len (aref data payload-start))
         (hw-start (1+ payload-start))
         (hw-end (+ hw-start hw-len)))
    ;; Validate bounds.
    (when (> hw-end (+ payload-start payload-length))
      (cl-return-from johnson-bgl--parse-entry-block nil))
    (let* ((headword (decode-coding-string
                      (substring data hw-start hw-end)
                      'utf-8))
           (def-len-pos hw-end))
      ;; Need at least 2 bytes for definition length.
      (when (> (+ def-len-pos 2) (+ payload-start payload-length))
        (cl-return-from johnson-bgl--parse-entry-block nil))
      (let* ((def-len (johnson-binary-u16be data def-len-pos))
             (def-start (+ def-len-pos 2))
             (def-end (+ def-start def-len))
             (block-end (+ payload-start payload-length)))
        (when (> def-end block-end)
          (cl-return-from johnson-bgl--parse-entry-block nil))
        (let ((results (list (list headword def-start def-len))))
          ;; Parse alternate headwords after the definition.
          (let ((alt-pos def-end))
            (while (< alt-pos block-end)
              (let* ((alt-len (aref data alt-pos))
                     (alt-start (1+ alt-pos))
                     (alt-end (+ alt-start alt-len)))
                (when (> alt-end block-end)
                  (setq alt-pos block-end))
                (unless (> alt-end block-end)
                  (let ((alt-hw (decode-coding-string
                                 (substring data alt-start alt-end)
                                 'utf-8)))
                    (push (list alt-hw def-start def-len) results))
                  (setq alt-pos alt-end)))))
          (nreverse results))))))

(cl-defun johnson-bgl--parse-entry-block-type-11 (data payload-start payload-length)
  "Parse a type 11 entry block from DATA at PAYLOAD-START with PAYLOAD-LENGTH.
Type 11 has 4-byte length fields instead of 1/2-byte.
Returns a list of (HEADWORD DEFINITION-OFFSET DEFINITION-LENGTH) triples."
  (when (< payload-length 10)
    (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
  (let* ((block-end (+ payload-start payload-length))
         ;; Word: first byte is flags, next 4 bytes are word length.
         (_flags (aref data payload-start))
         (hw-len (johnson-binary-u32be data (1+ payload-start)))
         (hw-start (+ payload-start 5))
         (hw-end (+ hw-start hw-len)))
    (when (> hw-end block-end)
      (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
    (let ((headword (decode-coding-string
                     (substring data hw-start hw-end)
                     'utf-8))
          (pos hw-end)
          (results nil))
      ;; Alternates count: 4 bytes.
      (when (> (+ pos 4) block-end)
        (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
      (let ((alt-count (johnson-binary-u32be data pos)))
        (setq pos (+ pos 4))
        ;; Skip alternates for now, just index the primary.
        (dotimes (_ alt-count)
          (when (> (+ pos 4) block-end)
            (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
          (let ((alt-len (johnson-binary-u32be data pos)))
            (setq pos (+ pos 4))
            (when (> (+ pos alt-len) block-end)
              (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
            (let ((alt-hw (decode-coding-string
                           (substring data pos (+ pos alt-len))
                           'utf-8)))
              (push alt-hw results))
            (setq pos (+ pos alt-len))))
        ;; Definition: 4 bytes length + data.
        (when (> (+ pos 4) block-end)
          (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
        (let* ((def-len (johnson-binary-u32be data pos))
               (def-start (+ pos 4))
               (def-end (+ def-start def-len)))
          (when (> def-end block-end)
            (cl-return-from johnson-bgl--parse-entry-block-type-11 nil))
          (setq pos def-end)
          ;; Build result list: primary + alternates.
          (let ((all (list (list headword def-start def-len))))
            (dolist (alt-hw results)
              (push (list alt-hw def-start def-len) all))
            (nreverse all)))))))

;;;; Format detection

(defun johnson-bgl-detect (path)
  "Return non-nil if PATH appears to be a BGL dictionary file."
  (and (string-suffix-p ".bgl" path t)
       (condition-case nil
           (with-temp-buffer
             (set-buffer-multibyte nil)
             (insert-file-contents-literally path nil 0 6)
             (let* ((data (buffer-string))
                    (magic (substring data 0 4)))
               ;; BGL magic: 0x12 0x34 0x00 followed by version byte.
               (and (= (aref magic 0) #x12)
                    (= (aref magic 1) #x34)
                    (= (aref magic 2) #x00)
                    (memq (aref magic 3) '(#x01 #x02))
                    ;; Gzip offset should be reasonable.
                    (let ((gz-offset (johnson-binary-u16be data 4)))
                      (and (> gz-offset 0)
                           (< gz-offset 256))))))
         (error nil))))

;;;; Metadata parsing

(defun johnson-bgl-parse-metadata (path)
  "Parse metadata from the BGL file at PATH.
Returns a plist (:name STRING :source-lang STRING :target-lang STRING)."
  (or (gethash path johnson-bgl--metadata-cache)
      (let* ((buf (johnson-bgl--get-buffer path))
             (data (with-current-buffer buf (buffer-string)))
             (blocks (johnson-bgl--parse-blocks data))
             (meta (johnson-bgl--parse-metadata-from-blocks data blocks))
             (result (list :name (plist-get meta :name)
                           :source-lang (plist-get meta :source-lang)
                           :target-lang (plist-get meta :target-lang))))
        (puthash path result johnson-bgl--metadata-cache)
        result)))

;;;; Index building

(defun johnson-bgl-build-index (path callback)
  "Parse the BGL dictionary at PATH, calling CALLBACK for each entry.
CALLBACK is called as (funcall CALLBACK headword byte-offset byte-length)
where byte-offset and byte-length reference positions in the decompressed
stream (cached in a unibyte buffer)."
  (let* ((buf (johnson-bgl--get-buffer path))
         (data (with-current-buffer buf (buffer-string)))
         (blocks (johnson-bgl--parse-blocks data)))
    (dolist (block blocks)
      (let ((btype (nth 0 block))
            (payload-start (nth 1 block))
            (payload-length (nth 2 block)))
        (cond
         ((johnson-bgl--entry-block-p btype)
          (let ((entries (johnson-bgl--parse-entry-block
                          data payload-start payload-length)))
            (dolist (entry entries)
              (funcall callback (nth 0 entry) (nth 1 entry) (nth 2 entry)))))
         ((= btype 11)
          (let ((entries (johnson-bgl--parse-entry-block-type-11
                          data payload-start payload-length)))
            (dolist (entry entries)
              (funcall callback (nth 0 entry) (nth 1 entry) (nth 2 entry))))))))))

;;;; Definition encoding

(defvar johnson-bgl--charset-cache (make-hash-table :test #'equal)
  "Cache of target coding systems per BGL file path.")

(defun johnson-bgl--definition-coding-system (path)
  "Return the coding system for definitions in the BGL file at PATH.
Uses the target charset from the dictionary metadata, falling
back to UTF-8.  Result is cached."
  (or (gethash path johnson-bgl--charset-cache)
      (let* ((buf (johnson-bgl--get-buffer path))
             (data (with-current-buffer buf (buffer-string)))
             (blocks (johnson-bgl--parse-blocks data))
             (meta (johnson-bgl--parse-metadata-from-blocks data blocks))
             (charset-code (plist-get meta :target-charset))
             (coding (johnson-bgl--charset-coding-system charset-code)))
        (puthash path coding johnson-bgl--charset-cache)
        coding)))

;;;; Entry retrieval

(defun johnson-bgl-retrieve-entry (path byte-offset byte-size)
  "Retrieve the entry definition from the BGL dictionary at PATH.
BYTE-OFFSET and BYTE-SIZE reference positions in the decompressed
stream.  Returns the raw definition as a decoded string."
  (let* ((buf (johnson-bgl--get-buffer path))
         (data (with-current-buffer buf (buffer-string)))
         (raw (substring data byte-offset (+ byte-offset byte-size))))
    ;; Strip BGL control sequences before decoding.
    (let* ((cleaned (johnson-bgl--strip-control-codes raw))
           (coding (johnson-bgl--definition-coding-system path)))
      (decode-coding-string cleaned coding))))

;;;; Control code stripping

(defun johnson-bgl--strip-control-codes (data)
  "Strip BGL control codes from raw unibyte DATA.
Removes 0x14+sequence (POS markers), 0x1E/0x1F (resource refs),
and similar control bytes.  Returns cleaned unibyte string."
  (let ((result nil)
        (pos 0)
        (len (length data)))
    (while (< pos len)
      (let ((byte (aref data pos)))
        (cond
         ;; 0x14: POS/field marker, followed by a field code byte.
         ;; Skip until next 0x14 or end of control sequence.
         ((= byte #x14)
          (cl-incf pos)
          ;; Skip the field code and its data.
          ;; Field codes: 0x02=POS(1 byte), 0x18=title(1+N),
          ;; 0x28=title(2+N), 0x50=transcription(1+1+N),
          ;; 0x60=transcription(1+2+N).
          (when (< pos len)
            (let ((field-code (aref data pos)))
              (cl-incf pos)
              (cond
               ((= field-code #x02)
                ;; POS: 1 byte data.
                (when (< pos len) (cl-incf pos)))
               ((= field-code #x18)
                ;; Title: 1-byte length + data.
                (when (< pos len)
                  (let ((flen (aref data pos)))
                    (cl-incf pos)
                    (cl-incf pos flen))))
               ((= field-code #x28)
                ;; Title with transcription: 2-byte length + data.
                (when (< (1+ pos) len)
                  (let ((flen (johnson-binary-u16be data pos)))
                    (cl-incf pos 2)
                    (cl-incf pos flen))))
               ((= field-code #x50)
                ;; Transcription type 1: 1 byte type + 1 byte length + data.
                (when (< (1+ pos) len)
                  (cl-incf pos) ; type byte
                  (let ((flen (aref data pos)))
                    (cl-incf pos)
                    (cl-incf pos flen))))
               ((= field-code #x60)
                ;; Transcription type 2: 1 byte type + 2 byte length + data.
                (when (< (+ pos 2) len)
                  (cl-incf pos) ; type byte
                  (let ((flen (johnson-binary-u16be data pos)))
                    (cl-incf pos 2)
                    (cl-incf pos flen))))
               (t
                ;; Unknown field code: just skip the byte.
                nil)))))
         ;; 0x1E, 0x1F: resource reference markers.
         ((memq byte '(#x1E #x1F))
          (cl-incf pos))
         ;; 0x0A: line break.
         ((= byte #x0A)
          (push ?\n result)
          (cl-incf pos))
         ;; 0x0D: carriage return, skip.
         ((= byte #x0D)
          (cl-incf pos))
         ;; Normal byte.
         (t
          (push byte result)
          (cl-incf pos)))))
    (apply #'unibyte-string (nreverse result))))

;;;; Entry rendering

(defun johnson-bgl-render-entry (data)
  "Render BGL entry DATA into the current buffer.
DATA is a decoded string containing HTML content."
  (let ((start (point))
        (johnson-html--current-dict-dir nil)
        (johnson-html--current-dict-path nil))
    (insert data)
    (johnson-html-render-region start (point))))

;;;; Cache clearing

(defun johnson-bgl-clear-caches ()
  "Clear all BGL caches."
  (maphash (lambda (_path buf)
             (when (buffer-live-p buf)
               (kill-buffer buf)))
           johnson-bgl--cache-buffers)
  (clrhash johnson-bgl--cache-buffers)
  (clrhash johnson-bgl--metadata-cache)
  (clrhash johnson-bgl--charset-cache))

;;;; Format registration

(provide 'johnson-bgl)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "bgl"
   :extensions '("bgl")
   :detect #'johnson-bgl-detect
   :parse-metadata #'johnson-bgl-parse-metadata
   :build-index #'johnson-bgl-build-index
   :retrieve-entry #'johnson-bgl-retrieve-entry
   :render-entry #'johnson-bgl-render-entry))

;;; johnson-bgl.el ends here
