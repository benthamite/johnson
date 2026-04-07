;;; johnson-stardict.el --- StarDict format backend for johnson -*- lexical-binding: t; -*-

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

;; This module provides the StarDict format backend for the johnson
;; dictionary package.  It handles parsing of .ifo metadata files,
;; binary .idx index files, .syn synonym files, and entry retrieval
;; from .dict (plain) or .dict.dz (dictzip-compressed) data files.
;; Rendering supports content types m (plain text), h (HTML), x (XDXF),
;; g (Pango markup), and t (phonetic transcription).

;;; Code:

(require 'cl-lib)
(require 'johnson-dictzip)
(require 'johnson-html)
(require 'johnson-binary)

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")
(declare-function johnson-insert-audio-button "johnson")

;;;; Internal variables

(defvar johnson-stardict--ifo-cache (make-hash-table :test #'equal)
  "Cache of parsed .ifo metadata.
Maps ifo file path to an alist of (KEY . VALUE) pairs.")

(defvar johnson-stardict--current-sametypesequence nil
  "Sametypesequence string for the entry being retrieved/rendered.
Set by `johnson-stardict-retrieve-entry', read by
`johnson-stardict-render-entry'.  Safe because rendering is
synchronous.")

(defvar johnson-stardict--current-dict-dir nil
  "Directory of the dictionary being retrieved/rendered.
Set by `johnson-stardict-retrieve-entry' for use by the renderer.")

;;;; .ifo parsing

(defun johnson-stardict--parse-ifo (path)
  "Parse all key-value pairs from the StarDict .ifo file at PATH.
Returns an alist of (KEY . VALUE) string pairs.  The result is
cached in `johnson-stardict--ifo-cache'."
  (or (gethash path johnson-stardict--ifo-cache)
      (let ((result nil))
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          ;; First line must be the magic string.
          (unless (looking-at "StarDict's dict ifo file\n")
            (error "Not a StarDict .ifo file: %s" path))
          (forward-line 1)
          (while (not (eobp))
            (when (looking-at "^\\s-*\\([a-zA-Z_]+\\)\\s-*=\\s-*\\(.*?\\)\\s-*$")
              (push (cons (match-string 1) (match-string 2)) result))
            (forward-line 1)))
        (setq result (nreverse result))
        (puthash path result johnson-stardict--ifo-cache)
        result)))

(defun johnson-stardict--ifo-get (ifo key)
  "Return the value for KEY in the parsed IFO alist, or nil."
  (cdr (assoc key ifo)))

;;;; Format detection

(defun johnson-stardict-detect (path)
  "Return non-nil if PATH appears to be a StarDict .ifo file."
  (and (string-suffix-p ".ifo" path t)
       (condition-case nil
           (with-temp-buffer
             (insert-file-contents path nil 0 64)
             (goto-char (point-min))
             (looking-at "StarDict's dict ifo file"))
         (error nil))))

;;;; Metadata parsing

(defun johnson-stardict-parse-metadata (path)
  "Parse metadata from the StarDict .ifo file at PATH.
Returns a plist (:name STRING :source-lang STRING :target-lang STRING)."
  (let* ((ifo (johnson-stardict--parse-ifo path))
         (name (or (johnson-stardict--ifo-get ifo "bookname") "")))
    (list :name name
          :source-lang ""
          :target-lang "")))

;;;; Path resolution helpers

(defun johnson-stardict--base-path (ifo-path)
  "Return the base path (without extension) from IFO-PATH."
  (file-name-sans-extension ifo-path))

(defun johnson-stardict--idx-path (ifo-path)
  "Return the .idx or .idx.gz file path for the dictionary at IFO-PATH."
  (let ((base (johnson-stardict--base-path ifo-path)))
    (cond
     ((file-exists-p (concat base ".idx"))
      (concat base ".idx"))
     ((file-exists-p (concat base ".idx.gz"))
      (concat base ".idx.gz"))
     (t (concat base ".idx")))))

(defun johnson-stardict--dict-path (ifo-path)
  "Return the .dict or .dict.dz file path for the dictionary at IFO-PATH."
  (let ((base (johnson-stardict--base-path ifo-path)))
    (cond
     ((file-exists-p (concat base ".dict"))
      (concat base ".dict"))
     ((file-exists-p (concat base ".dict.dz"))
      (concat base ".dict.dz"))
     (t (concat base ".dict")))))

(defun johnson-stardict--syn-path (ifo-path)
  "Return the .syn file path for the dictionary at IFO-PATH, or nil if absent."
  (let ((path (concat (johnson-stardict--base-path ifo-path) ".syn")))
    (when (file-exists-p path)
      path)))

;;;; .idx parsing

(defun johnson-stardict--load-idx-data (ifo-path)
  "Load the raw .idx data for the dictionary at IFO-PATH.
Returns a unibyte string.  Handles both plain .idx and .idx.gz files."
  (let ((idx-path (johnson-stardict--idx-path ifo-path)))
    (unless (file-exists-p idx-path)
      (error "Missing .idx file: %s (dictionary is incomplete)" idx-path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (if (string-suffix-p ".idx.gz" idx-path)
          (progn
            (insert-file-contents-literally idx-path)
            (unless (zlib-decompress-region (point-min) (point-max))
              (error "Failed to decompress .idx.gz file: %s" idx-path))
            (buffer-string))
        (insert-file-contents-literally idx-path)
        (buffer-string)))))

(defun johnson-stardict--read-null-terminated-string (data pos)
  "Read a null-terminated UTF-8 string from unibyte DATA starting at POS.
Returns a cons cell (STRING . NEW-POS) where NEW-POS is past the null byte."
  (let ((end pos))
    (while (and (< end (length data))
                (/= (aref data end) 0))
      (cl-incf end))
    (cons (decode-coding-string (substring data pos end) 'utf-8)
          (1+ end))))


(defun johnson-stardict--do-parse-idx (data offset-bytes size-bytes)
  "Parse idx DATA using OFFSET-BYTES and SIZE-BYTES per entry.
Returns a vector of (HEADWORD OFFSET SIZE) triples, in file order."
  (let ((pos 0)
        (entries nil))
    (while (< pos (length data))
      (let* ((str-result (johnson-stardict--read-null-terminated-string data pos))
             (headword (car str-result))
             (cur-pos (cdr str-result))
             (offset (if (= offset-bytes 8)
                         (johnson-binary-u64be data cur-pos)
                       (johnson-binary-u32be data cur-pos)))
             (size (if (= size-bytes 8)
                       (johnson-binary-u64be data (+ cur-pos offset-bytes))
                     (johnson-binary-u32be data (+ cur-pos offset-bytes)))))
        (push (list headword offset size) entries)
        (setq pos (+ cur-pos offset-bytes size-bytes))))
    (vconcat (nreverse entries))))

(defun johnson-stardict--try-parse-idx (data offset-bytes size-bytes expected-count)
  "Try parsing idx DATA with OFFSET-BYTES and SIZE-BYTES per entry.
Return the entry vector when successful and its length matches
EXPECTED-COUNT (if non-nil), or nil on mismatch/error."
  (condition-case nil
      (let ((result (johnson-stardict--do-parse-idx data offset-bytes size-bytes)))
        (when (or (null expected-count)
                  (= (length result) expected-count))
          result))
    (args-out-of-range nil)))

(defun johnson-stardict--idx-values-padded-p (entries)
  "Return non-nil if ENTRIES look like 32-bit values padded to 64-bit.
Some non-standard dictionaries store 4-byte offset and size values each
padded to 8 bytes (4 data bytes + 4 zero bytes), resulting in values
shifted left by 32 bits when read as u64.  Detected by checking that all
offset and size values have zero in their lower 32 bits, with at least
one non-zero value present."
  (let ((all-zero-low t)
        (has-nonzero nil))
    (cl-loop for entry across entries
             while all-zero-low
             do (let ((offset (nth 1 entry))
                      (size (nth 2 entry)))
                  (when (or (/= offset 0) (/= size 0))
                    (setq has-nonzero t))
                  (when (or (/= (logand offset #xFFFFFFFF) 0)
                            (/= (logand size #xFFFFFFFF) 0))
                    (setq all-zero-low nil))))
    (and all-zero-low has-nonzero)))

(defun johnson-stardict--fix-padded-idx (entries)
  "Right-shift offset and size values in ENTRIES by 32 bits.
Modifies ENTRIES in place and returns it."
  (cl-loop for entry across entries
           do (setf (nth 1 entry) (ash (nth 1 entry) -32))
              (setf (nth 2 entry) (ash (nth 2 entry) -32)))
  entries)

(defun johnson-stardict--parse-idx (ifo-path)
  "Parse the .idx file for the dictionary at IFO-PATH.
Returns a vector of (HEADWORD OFFSET SIZE) triples, in file order.
OFFSET and SIZE are byte offsets/lengths into the .dict file.

When the .ifo declares idxoffsetbits=64, uses 8-byte offsets and
4-byte sizes (standard StarDict 3.0).  Otherwise, tries 4+4 (the
default), then 8+4, then 8+8 (for non-standard dictionaries whose
.ifo omits the idxoffsetbits key), validating against the declared
wordcount.  Detects and corrects 32-bit values padded to 8 bytes."
  (let* ((ifo (johnson-stardict--parse-ifo ifo-path))
         (wordcount (let ((v (johnson-stardict--ifo-get ifo "wordcount")))
                      (and v (string-to-number v))))
         (declared-64 (let ((v (johnson-stardict--ifo-get ifo "idxoffsetbits")))
                        (and v (equal v "64"))))
         (data (johnson-stardict--load-idx-data ifo-path)))
    (if declared-64
        ;; Standard 64-bit: 8-byte offset + 4-byte size.
        (johnson-stardict--do-parse-idx data 8 4)
      ;; Auto-detect: try 32-bit first, fall back to 64-bit variants.
      (or (johnson-stardict--try-parse-idx data 4 4 wordcount)
          (johnson-stardict--try-parse-idx data 8 4 wordcount)
          (let ((result (johnson-stardict--try-parse-idx data 8 8 wordcount)))
            (when result
              ;; Some dictionaries store 32-bit values padded to 8 bytes;
              ;; detect and correct by right-shifting by 32.
              (when (johnson-stardict--idx-values-padded-p result)
                (johnson-stardict--fix-padded-idx result))
              result))
          (error "Failed to parse StarDict .idx for %s" ifo-path)))))

;;;; .syn parsing

(defun johnson-stardict--parse-syn (ifo-path idx-entries)
  "Parse the .syn file for the dictionary at IFO-PATH.
IDX-ENTRIES is the vector returned by `johnson-stardict--parse-idx'.
Returns a list of (SYNONYM-WORD OFFSET SIZE) triples."
  (let ((syn-path (johnson-stardict--syn-path ifo-path)))
    (when syn-path
      (let ((data (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally syn-path)
                    (buffer-string)))
            (pos 0)
            (result nil))
        (while (< pos (length data))
          (let* ((str-result (johnson-stardict--read-null-terminated-string data pos))
                 (synonym (car str-result))
                 (cur-pos (cdr str-result))
                 (idx (johnson-binary-u32be data cur-pos)))
            (when (< idx (length idx-entries))
              (let ((main-entry (aref idx-entries idx)))
                (push (list synonym (nth 1 main-entry) (nth 2 main-entry))
                      result)))
            (setq pos (+ cur-pos 4))))
        (nreverse result)))))

;;;; Index building

(defun johnson-stardict-build-index (path callback)
  "Parse the StarDict dictionary at PATH, calling CALLBACK for each entry.
CALLBACK is called as (funcall CALLBACK headword byte-offset byte-size)
where byte-offset and byte-size refer to the .dict data file."
  (let* ((idx-entries (johnson-stardict--parse-idx path))
         (syn-entries (johnson-stardict--parse-syn path idx-entries)))
    ;; Index main entries.
    (cl-loop for entry across idx-entries
             do (funcall callback (nth 0 entry) (nth 1 entry) (nth 2 entry)))
    ;; Index synonyms.
    (dolist (syn syn-entries)
      (funcall callback (nth 0 syn) (nth 1 syn) (nth 2 syn)))))

;;;; Entry retrieval

(defun johnson-stardict-retrieve-entry (path byte-offset byte-size)
  "Retrieve the entry data from the StarDict dictionary at PATH.
PATH is the .ifo file path.  BYTE-OFFSET and BYTE-SIZE are byte
offsets into the uncompressed .dict data.  Returns a raw unibyte
string.  Also sets `johnson-stardict--current-sametypesequence'
for use by the renderer."
  (let* ((ifo (johnson-stardict--parse-ifo path))
         (sametypesequence (johnson-stardict--ifo-get ifo "sametypesequence"))
         (dict-path (johnson-stardict--dict-path path))
         (_ (unless (file-exists-p dict-path)
              (error "Missing .dict file: %s (dictionary is incomplete)" dict-path)))
         (raw (if (string-suffix-p ".dict.dz" dict-path)
                  (johnson-dictzip-read dict-path byte-offset byte-size)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally dict-path nil
                                                  byte-offset
                                                  (+ byte-offset byte-size))
                  (let ((actual (buffer-size)))
                    (unless (= actual byte-size)
                      (error "StarDict .dict: expected %d bytes at offset %d, got %d"
                             byte-size byte-offset actual)))
                  (buffer-string)))))
    (setq johnson-stardict--current-sametypesequence sametypesequence)
    (setq johnson-stardict--current-dict-dir (file-name-directory path))
    raw))

;;;; Content type rendering

(defun johnson-stardict--render-type-m (data)
  "Render type `m' (plain text) DATA into the current buffer."
  (insert (decode-coding-string data 'utf-8)))

(defun johnson-stardict--render-type-t (data)
  "Render phonetic transcription type DATA into the current buffer."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point)))
    (insert "[" text "]")
    (add-face-text-property start (point) 'johnson-italic-face)))

(defun johnson-stardict--render-type-h (data)
  "Render type `h' (HTML) DATA into the current buffer.
Strips HTML tags and converts common formatting tags to text properties."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point))
        (johnson-html--current-dict-dir johnson-stardict--current-dict-dir)
        (johnson-html--current-dict-path nil))
    (insert text)
    (let ((end (point)))
      (johnson-html-render-region start end))))

(defun johnson-stardict--render-type-x (data)
  "Render type `x' (XDXF) DATA into the current buffer.
Handles XDXF tags like <kref>, <gr>, <ex>, <abbr>, <dtrn>."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point)))
    (insert text)
    (let ((end (point)))
      ;; Use a marker for end so buffer modifications are tracked
      ;; automatically (e.g., bracket insertion in <tr> tags).
      (let ((end-marker (copy-marker end)))
        ;; Replace <br/> with newlines.
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<br\\s-*/?>\\|<br>" end-marker t)
            (replace-match "\n")))
        ;; Process paired XDXF tags.
        (let ((tag-re "<\\(/\\)?\\([a-zA-Z_]+\\)\\([^>]*\\)>")
              (stack nil))
          (save-excursion
            (goto-char start)
            (while (re-search-forward tag-re end-marker t)
              (let* ((closing-p (match-string 1))
                     (tag-name (downcase (match-string 2)))
                     (tag-attrs (or (match-string 3) ""))
                     (tag-beg (match-beginning 0))
                     (tag-end (match-end 0)))
                ;; Delete the tag.
                (delete-region tag-beg tag-end)
                (goto-char tag-beg)
                (cond
                 ;; Closing tag.
                 (closing-p
                  (let ((entry (cl-find tag-name stack :key #'car :test #'equal)))
                    (when entry
                      (setq stack (remove entry stack))
                      (let ((region-start (nth 1 entry)))
                        (johnson-stardict--apply-xdxf-tag
                         tag-name region-start (point))
                        (when (markerp region-start)
                          (set-marker region-start nil))))))
                 ;; Opening tag: use markers so positions track insertions.
                 (t
                  (push (list tag-name (copy-marker (point)) tag-attrs)
                        stack))))))
          ;; Clean up any remaining markers on the stack.
          (dolist (entry stack)
            (when (markerp (nth 1 entry))
              (set-marker (nth 1 entry) nil))))
        (set-marker end-marker nil)))))

(defun johnson-stardict--apply-xdxf-tag (tag-name region-start region-end)
  "Apply rendering for XDXF TAG-NAME over REGION-START to REGION-END."
  (pcase tag-name
    ("kref"
     (let ((ref-text (buffer-substring-no-properties region-start region-end)))
       (make-text-button region-start region-end
                         'face 'johnson-ref-face
                         'johnson-ref-word ref-text
                         'action (lambda (_btn) (johnson-lookup ref-text))
                         'help-echo (format "Look up \"%s\"" ref-text))))
    ("gr"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("ex"
     (add-face-text-property region-start region-end 'johnson-example-face))
    ("abbr"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("dtrn"
     nil)  ; Translation — render as-is.
    ("b"
     (add-face-text-property region-start region-end 'johnson-bold-face))
    ("i"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("c"
     (add-face-text-property region-start region-end 'johnson-color-default-face))
    ("k"
     ;; Key/headword — render bold.
     (add-face-text-property region-start region-end 'johnson-bold-face))
    ("tr"
     ;; Transcription — render in brackets with italic face.
     (save-excursion
       (goto-char region-end)
       (insert "]")
       (goto-char region-start)
       (insert "["))
     (add-face-text-property region-start (+ region-end 2) 'johnson-italic-face))))

(defun johnson-stardict--render-type-g (data)
  "Render type `g' (Pango markup) DATA into the current buffer.
Handles <b>, <i>, <u>, <span foreground=\"...\"> tags."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point))
        (johnson-html--current-dict-dir johnson-stardict--current-dict-dir)
        (johnson-html--current-dict-path nil))
    (insert text)
    (let ((end (point)))
      ;; Process Pango tags — essentially the same as HTML.
      (johnson-html-render-region start end))))

(defun johnson-stardict--render-type-r (data)
  "Render type `r' (resource) DATA.
For entries starting with \"snd:\", insert an audio play button
pointing to the resource file in the dictionary's res/ directory."
  (let ((text (decode-coding-string data 'utf-8)))
    (if (string-prefix-p "snd:" text)
        (let ((filename (substring text 4)))
          (when (and (not (string-empty-p filename))
                     johnson-stardict--current-dict-dir)
            (let ((audio-path (expand-file-name
                               filename
                               (expand-file-name
                                "res"
                                johnson-stardict--current-dict-dir))))
              (johnson-insert-audio-button audio-path))))
      ;; Non-sound resource: insert filename as reference.
      (insert text))))

(defun johnson-stardict--render-type-w (data)
  "Render type `W' (WAV audio) DATA by inserting a play button.
DATA is raw WAV audio bytes; written to a temp file for playback."
  (when (> (length data) 0)
    (let ((temp-file (make-temp-file "johnson-audio-" nil ".wav")))
      (with-temp-file temp-file
        (set-buffer-multibyte nil)
        (insert data))
      (when (boundp 'johnson--temp-audio-files)
        (push temp-file johnson--temp-audio-files))
      (johnson-insert-audio-button temp-file))))

;;;; Field splitting for sametypesequence

(defun johnson-stardict--split-fields-sametypesequence (raw-data type-seq)
  "Split RAW-DATA according to sametypesequence TYPE-SEQ.
TYPE-SEQ is a string like \"mh\" or \"tm\".
Returns a list of (TYPE-CHAR . DATA-BYTES) cons cells."
  (let ((fields nil)
        (pos 0)
        (num-types (length type-seq))
        (data-len (length raw-data)))
    (dotimes (i num-types)
      (let ((type-char (aref type-seq i))
            (is-last (= i (1- num-types))))
        (if is-last
            ;; Last field extends to end of data.
            (progn
              (push (cons type-char (substring raw-data pos)) fields)
              (setq pos data-len))
          ;; Non-last fields: for lowercase types, find null terminator.
          ;; For uppercase types, read 4-byte size prefix.
          (if (and (>= type-char ?a) (<= type-char ?z))  ; lowercase
              ;; Find null terminator.
              (let ((end pos))
                (while (and (< end data-len)
                            (/= (aref raw-data end) 0))
                  (cl-incf end))
                (push (cons type-char (substring raw-data pos end)) fields)
                (setq pos (1+ end)))  ; skip null byte
            ;; Uppercase: 4-byte size prefix.
            (let ((size (johnson-binary-u32be raw-data pos)))
              (push (cons type-char (substring raw-data (+ pos 4) (+ pos 4 size)))
                    fields)
              (setq pos (+ pos 4 size)))))))
    (nreverse fields)))

(defun johnson-stardict--split-fields-no-sametypesequence (raw-data)
  "Split RAW-DATA without a sametypesequence.
Each field is: 1 byte type char + data.
For lowercase types, data is null-terminated.
For uppercase types, next 4 bytes are big-endian size, then size bytes.
Returns a list of (TYPE-CHAR . DATA-BYTES) cons cells."
  (let ((fields nil)
        (pos 0)
        (data-len (length raw-data)))
    (while (< pos data-len)
      (let ((type-char (aref raw-data pos)))
        (cl-incf pos)
        (if (and (>= type-char ?a) (<= type-char ?z))
            ;; Lowercase: null-terminated data.
            (let ((end pos))
              (while (and (< end data-len)
                          (/= (aref raw-data end) 0))
                (cl-incf end))
              (push (cons type-char (substring raw-data pos end)) fields)
              (setq pos (1+ end)))
          ;; Uppercase: 4-byte size prefix.
          (let ((size (johnson-binary-u32be raw-data pos)))
            (push (cons type-char (substring raw-data (+ pos 4) (+ pos 4 size)))
                  fields)
            (setq pos (+ pos 4 size))))))
    (nreverse fields)))

;;;; Entry rendering

(defun johnson-stardict-render-entry (raw-data)
  "Render StarDict entry RAW-DATA into the current buffer.
Uses `johnson-stardict--current-sametypesequence' to determine how
to parse the data fields."
  (let* ((sametypesequence johnson-stardict--current-sametypesequence)
         (fields (if sametypesequence
                     (johnson-stardict--split-fields-sametypesequence
                      raw-data sametypesequence)
                   (johnson-stardict--split-fields-no-sametypesequence
                    raw-data))))
    (dolist (field fields)
      (let ((type-char (car field))
            (data (cdr field)))
        (pcase (downcase type-char)
          (?m (johnson-stardict--render-type-m data))
          (?h (johnson-stardict--render-type-h data))
          (?x (johnson-stardict--render-type-x data))
          (?g (johnson-stardict--render-type-g data))
          (?t (johnson-stardict--render-type-t data))
          (?r (johnson-stardict--render-type-r data))
          (?w (johnson-stardict--render-type-w data))
          (?p (johnson-stardict--render-type-w data))
          ;; For unrecognized types, render as plain text.
          (_ (johnson-stardict--render-type-m data)))))))

;;;; Format registration

(provide 'johnson-stardict)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "stardict"
   :extensions '("ifo")
   :detect #'johnson-stardict-detect
   :parse-metadata #'johnson-stardict-parse-metadata
   :build-index #'johnson-stardict-build-index
   :retrieve-entry #'johnson-stardict-retrieve-entry
   :render-entry #'johnson-stardict-render-entry))

;;; johnson-stardict.el ends here
