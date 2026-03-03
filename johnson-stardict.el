;;; johnson-stardict.el --- StarDict format backend for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

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

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")

;;;; Internal variables

(defvar johnson-stardict--ifo-cache (make-hash-table :test #'equal)
  "Cache of parsed .ifo metadata.
Maps ifo file path to an alist of (KEY . VALUE) pairs.")

(defvar johnson-stardict--current-sametypesequence nil
  "Sametypesequence string for the entry being retrieved/rendered.
Set by `johnson-stardict-retrieve-entry', read by
`johnson-stardict-render-entry'.  Safe because rendering is
synchronous.")

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
            (when (looking-at "^\\([a-zA-Z_]+\\)=\\(.*\\)$")
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
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (if (string-suffix-p ".idx.gz" idx-path)
          (progn
            (insert-file-contents-literally idx-path)
            (zlib-decompress-region (point-min) (point-max))
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

(defun johnson-stardict--u32be-from-string (data pos)
  "Read a 32-bit big-endian unsigned integer from DATA at byte POS."
  (logior (ash (aref data pos) 24)
          (ash (aref data (+ pos 1)) 16)
          (ash (aref data (+ pos 2)) 8)
          (aref data (+ pos 3))))

(defun johnson-stardict--u64be-from-string (data pos)
  "Read a 64-bit big-endian unsigned integer from DATA at byte POS."
  (logior (ash (aref data pos) 56)
          (ash (aref data (+ pos 1)) 48)
          (ash (aref data (+ pos 2)) 40)
          (ash (aref data (+ pos 3)) 32)
          (ash (aref data (+ pos 4)) 24)
          (ash (aref data (+ pos 5)) 16)
          (ash (aref data (+ pos 6)) 8)
          (aref data (+ pos 7))))

(defun johnson-stardict--parse-idx (ifo-path)
  "Parse the .idx file for the dictionary at IFO-PATH.
Returns a vector of (HEADWORD OFFSET SIZE) triples, in file order.
OFFSET and SIZE are byte offsets/lengths into the .dict file."
  (let* ((ifo (johnson-stardict--parse-ifo ifo-path))
         (offset-bits (let ((v (johnson-stardict--ifo-get ifo "idxoffsetbits")))
                        (if (and v (equal v "64")) 64 32)))
         (offset-bytes (if (= offset-bits 64) 8 4))
         (data (johnson-stardict--load-idx-data ifo-path))
         (pos 0)
         (entries nil))
    (while (< pos (length data))
      (let* ((str-result (johnson-stardict--read-null-terminated-string data pos))
             (headword (car str-result))
             (cur-pos (cdr str-result))
             (offset (if (= offset-bytes 8)
                         (johnson-stardict--u64be-from-string data cur-pos)
                       (johnson-stardict--u32be-from-string data cur-pos)))
             (size (johnson-stardict--u32be-from-string data (+ cur-pos offset-bytes))))
        (push (list headword offset size) entries)
        (setq pos (+ cur-pos offset-bytes 4))))
    (vconcat (nreverse entries))))

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
                 (idx (johnson-stardict--u32be-from-string data cur-pos)))
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
         (raw (if (string-suffix-p ".dict.dz" dict-path)
                  (johnson-dictzip-read dict-path byte-offset byte-size)
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally dict-path nil
                                                  byte-offset
                                                  (+ byte-offset byte-size))
                  (buffer-string)))))
    (setq johnson-stardict--current-sametypesequence sametypesequence)
    raw))

;;;; Content type rendering

(defun johnson-stardict--render-type-m (data)
  "Render type `m' (plain text) DATA into the current buffer."
  (insert (decode-coding-string data 'utf-8)))

(defun johnson-stardict--render-type-t (data)
  "Render type `t' (phonetic transcription) DATA into the current buffer."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point)))
    (insert "[" text "]")
    (add-face-text-property start (point) 'johnson-italic-face)))

(defun johnson-stardict--html-color-to-face (color)
  "Map an HTML color name or hex code COLOR to a johnson face."
  (let ((lower (downcase (string-trim color))))
    (cond
     ((or (string-prefix-p "#" lower)
          (string-match-p "^rgb" lower))
      ;; For hex/rgb colors, try to approximate.
      'johnson-color-default-face)
     ((or (string= lower "green") (string= lower "darkgreen"))
      'johnson-color-green-face)
     ((or (string= lower "red") (string= lower "darkred") (string= lower "crimson"))
      'johnson-color-red-face)
     ((or (string= lower "blue") (string= lower "darkblue") (string= lower "steelblue"))
      'johnson-color-blue-face)
     ((or (string= lower "gray") (string= lower "grey") (string= lower "dimgray")
          (string= lower "darkgray"))
      'johnson-color-gray-face)
     ((or (string= lower "brown") (string= lower "saddlebrown"))
      'johnson-color-brown-face)
     ((or (string= lower "violet") (string= lower "purple") (string= lower "darkviolet"))
      'johnson-color-violet-face)
     ((or (string= lower "orange") (string= lower "darkorange"))
      'johnson-color-orange-face)
     (t 'johnson-color-default-face))))

(defun johnson-stardict--render-type-h (data)
  "Render type `h' (HTML) DATA into the current buffer.
Strips HTML tags and converts common formatting tags to text properties."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point)))
    (insert text)
    (let ((end (point)))
      ;; Process tags from end to start to preserve positions.
      (johnson-stardict--process-html-region start end))))

(defun johnson-stardict--process-html-region (start end)
  "Process HTML tags in the region from START to END.
Replaces tags with text properties."
  (save-excursion
    ;; First pass: replace <br>, <br/>, <hr>, <hr/> with newlines.
    (goto-char start)
    (while (re-search-forward "<br\\s-*/?>\\|<hr\\s-*/?>" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "\n")
        (setq end (- end len -1))))
    ;; Replace <p> and </p> with blank lines.
    (goto-char start)
    (while (re-search-forward "</p>\\s-*" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "\n\n")
        (setq end (- end len -2))))
    (goto-char start)
    (while (re-search-forward "<p[^>]*>" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "")
        (setq end (- end len))))
    ;; Process paired tags using a stack-based approach.
    (let ((tag-re "<\\(/\\)?\\([a-zA-Z]+\\)\\([^>]*\\)>")
          (stack nil))
      (goto-char start)
      (while (re-search-forward tag-re end t)
        (let* ((closing-p (match-string 1))
               (tag-name (downcase (match-string 2)))
               (tag-attrs (or (match-string 3) ""))
               (tag-beg (match-beginning 0))
               (tag-end (match-end 0)))
          ;; Delete the tag.
          (delete-region tag-beg tag-end)
          (setq end (- end (- tag-end tag-beg)))
          (goto-char tag-beg)
          (cond
           ;; Closing tag.
           (closing-p
            (let ((entry (cl-find tag-name stack :key #'car :test #'equal)))
              (when entry
                (setq stack (remove entry stack))
                (let ((region-start (nth 1 entry))
                      (region-attrs (nth 2 entry)))
                  (johnson-stardict--apply-html-tag
                   tag-name region-start (point) region-attrs)))))
           ;; Opening tag.
           (t
            (push (list tag-name (point) tag-attrs) stack))))))))

(defun johnson-stardict--apply-html-tag (tag-name region-start region-end attrs)
  "Apply rendering for HTML TAG-NAME over REGION-START to REGION-END.
ATTRS is the raw attribute string from the opening tag."
  (pcase tag-name
    ("b"
     (add-face-text-property region-start region-end 'johnson-bold-face))
    ("strong"
     (add-face-text-property region-start region-end 'johnson-bold-face))
    ("i"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("em"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("u"
     (add-face-text-property region-start region-end 'johnson-underline-face))
    ("sup"
     (add-face-text-property region-start region-end 'johnson-bold-face)
     (put-text-property region-start region-end
                        'display '((raise 0.3) (height 0.7))))
    ("sub"
     (put-text-property region-start region-end
                        'display '((raise -0.3) (height 0.7))))
    ("font"
     (when (string-match "color\\s-*=\\s-*[\"']?\\([^\"' >]+\\)" attrs)
       (let ((face (johnson-stardict--html-color-to-face (match-string 1 attrs))))
         (add-face-text-property region-start region-end face))))
    ("span"
     (when (string-match "color\\s-*:\\s-*\\([^;\"' >]+\\)" attrs)
       (let ((face (johnson-stardict--html-color-to-face (match-string 1 attrs))))
         (add-face-text-property region-start region-end face))))
    ("a"
     (if (string-match "href\\s-*=\\s-*[\"']\\(?:bword://\\)?\\([^\"']+\\)[\"']" attrs)
         (let ((target (match-string 1 attrs)))
           (make-text-button region-start region-end
                             'face 'johnson-ref-face
                             'action (lambda (_btn) (johnson-lookup target))
                             'help-echo (format "Look up \"%s\"" target)))
       ;; No href, just make it look like a link.
       (add-face-text-property region-start region-end 'johnson-ref-face)))))

(defun johnson-stardict--render-type-x (data)
  "Render type `x' (XDXF) DATA into the current buffer.
Handles XDXF tags like <kref>, <gr>, <ex>, <abbr>, <dtrn>."
  (let ((text (decode-coding-string data 'utf-8))
        (start (point)))
    (insert text)
    (let ((end (point)))
      ;; Replace <br/> with newlines.
      (save-excursion
        (goto-char start)
        (while (re-search-forward "<br\\s-*/?>\\|<br>" end t)
          (let ((len (- (match-end 0) (match-beginning 0))))
            (replace-match "\n")
            (setq end (- end len -1)))))
      ;; Process paired XDXF tags.
      (let ((tag-re "<\\(/\\)?\\([a-zA-Z_]+\\)\\([^>]*\\)>")
            (stack nil))
        (save-excursion
          (goto-char start)
          (while (re-search-forward tag-re end t)
            (let* ((closing-p (match-string 1))
                   (tag-name (downcase (match-string 2)))
                   (tag-attrs (or (match-string 3) ""))
                   (tag-beg (match-beginning 0))
                   (tag-end (match-end 0)))
              ;; Delete the tag.
              (delete-region tag-beg tag-end)
              (setq end (- end (- tag-end tag-beg)))
              (goto-char tag-beg)
              (cond
               ;; Closing tag.
               (closing-p
                (let ((entry (cl-find tag-name stack :key #'car :test #'equal)))
                  (when entry
                    (setq stack (remove entry stack))
                    (let ((region-start (nth 1 entry)))
                      (johnson-stardict--apply-xdxf-tag
                       tag-name region-start (point))))))
               ;; Opening tag.
               (t
                (push (list tag-name (point) tag-attrs) stack))))))))))

(defun johnson-stardict--apply-xdxf-tag (tag-name region-start region-end)
  "Apply rendering for XDXF TAG-NAME over REGION-START to REGION-END."
  (pcase tag-name
    ("kref"
     (let ((ref-text (buffer-substring-no-properties region-start region-end)))
       (make-text-button region-start region-end
                         'face 'johnson-ref-face
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
        (start (point)))
    (insert text)
    (let ((end (point)))
      ;; Process Pango tags — essentially the same as HTML.
      (johnson-stardict--process-html-region start end))))

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
          (if (<= type-char ?z)  ; lowercase
              ;; Find null terminator.
              (let ((end pos))
                (while (and (< end data-len)
                            (/= (aref raw-data end) 0))
                  (cl-incf end))
                (push (cons type-char (substring raw-data pos end)) fields)
                (setq pos (1+ end)))  ; skip null byte
            ;; Uppercase: 4-byte size prefix.
            (let ((size (johnson-stardict--u32be-from-string raw-data pos)))
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
          (let ((size (johnson-stardict--u32be-from-string raw-data pos)))
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
