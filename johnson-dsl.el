;;; johnson-dsl.el --- DSL (ABBYY Lingvo) format backend for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides the DSL (ABBYY Lingvo) format backend for the
;; johnson dictionary package.  It handles encoding detection, parsing of
;; DSL metadata and entries, headword expansion (alternation and optional
;; parts), byte-offset indexing, entry retrieval via a raw-text buffer
;; cache, and full rendering of DSL markup tags into Emacs text properties
;; and faces.

;;; Code:

(require 'cl-lib)
(require 'johnson-dictzip)

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")
(declare-function johnson-insert-audio-button "johnson")
(declare-function johnson--image-file-p "johnson")
(declare-function johnson--insert-image "johnson")
(declare-function johnson--resolve-audio-file "johnson")

;;;; DSL-specific faces

(defface johnson-optional-face
  '((((background light)) :foreground "gray50")
    (((background dark)) :foreground "gray60"))
  "Face for optional/secondary text in DSL entries."
  :group 'johnson)

(defface johnson-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comment text in DSL entries."
  :group 'johnson)

(defface johnson-stress-face
  '((t :inherit bold))
  "Face for stress marks in DSL entries."
  :group 'johnson)

(defface johnson-abbreviation-face
  '((((background light)) :foreground "dark green")
    (((background dark)) :foreground "green3"))
  "Face for abbreviation markers ([p] tags) in DSL entries."
  :group 'johnson)

;;;; Color mapping

(defconst johnson-dsl--color-alist
  '(;; Green
    ("green"           . johnson-color-green-face)
    ("darkgreen"       . johnson-color-green-face)
    ("darkolivegreen"  . johnson-color-green-face)
    ("darkseagreen"    . johnson-color-green-face)
    ("forestgreen"     . johnson-color-green-face)
    ("limegreen"       . johnson-color-green-face)
    ("mediumseagreen"  . johnson-color-green-face)
    ("olive"           . johnson-color-green-face)
    ("olivedrab"       . johnson-color-green-face)
    ("seagreen"        . johnson-color-green-face)
    ("yellowgreen"     . johnson-color-green-face)
    ;; Red
    ("red"             . johnson-color-red-face)
    ("darkred"         . johnson-color-red-face)
    ("crimson"         . johnson-color-red-face)
    ("firebrick"       . johnson-color-red-face)
    ("indianred"       . johnson-color-red-face)
    ("lightcoral"      . johnson-color-red-face)
    ("maroon"          . johnson-color-red-face)
    ("tomato"          . johnson-color-red-face)
    ;; Blue
    ("blue"            . johnson-color-blue-face)
    ("aqua"            . johnson-color-blue-face)
    ("cadetblue"       . johnson-color-blue-face)
    ("cyan"            . johnson-color-blue-face)
    ("darkblue"        . johnson-color-blue-face)
    ("darkcyan"        . johnson-color-blue-face)
    ("darkturquoise"   . johnson-color-blue-face)
    ("dodgerblue"      . johnson-color-blue-face)
    ("lightblue"       . johnson-color-blue-face)
    ("lightseagreen"   . johnson-color-blue-face)
    ("mediumblue"      . johnson-color-blue-face)
    ("midnightblue"    . johnson-color-blue-face)
    ("navy"            . johnson-color-blue-face)
    ("royalblue"       . johnson-color-blue-face)
    ("skyblue"         . johnson-color-blue-face)
    ("steelblue"       . johnson-color-blue-face)
    ("teal"            . johnson-color-blue-face)
    ;; Gray
    ("black"           . johnson-color-gray-face)
    ("gray"            . johnson-color-gray-face)
    ("darkgray"        . johnson-color-gray-face)
    ("darkslategray"   . johnson-color-gray-face)
    ("dimgray"         . johnson-color-gray-face)
    ("lightgray"       . johnson-color-gray-face)
    ("lightslategray"  . johnson-color-gray-face)
    ("silver"          . johnson-color-gray-face)
    ("slategray"       . johnson-color-gray-face)
    ;; Brown
    ("brown"           . johnson-color-brown-face)
    ("burlywood"       . johnson-color-brown-face)
    ("chocolate"       . johnson-color-brown-face)
    ("coral"           . johnson-color-brown-face)
    ("darkgoldenrod"   . johnson-color-brown-face)
    ("goldenrod"       . johnson-color-brown-face)
    ("peru"            . johnson-color-brown-face)
    ("rosybrown"       . johnson-color-brown-face)
    ("saddlebrown"     . johnson-color-brown-face)
    ("sandybrown"      . johnson-color-brown-face)
    ("sienna"          . johnson-color-brown-face)
    ("tan"             . johnson-color-brown-face)
    ("wheat"           . johnson-color-brown-face)
    ;; Violet / purple / magenta
    ("blueviolet"      . johnson-color-violet-face)
    ("darkmagenta"     . johnson-color-violet-face)
    ("darkorchid"      . johnson-color-violet-face)
    ("darkslateblue"   . johnson-color-violet-face)
    ("deeppink"        . johnson-color-violet-face)
    ("fuchsia"         . johnson-color-violet-face)
    ("indigo"          . johnson-color-violet-face)
    ("magenta"         . johnson-color-violet-face)
    ("mediumslateblue" . johnson-color-violet-face)
    ("mediumvioletred" . johnson-color-violet-face)
    ("orchid"          . johnson-color-violet-face)
    ("palevioletred"   . johnson-color-violet-face)
    ("plum"            . johnson-color-violet-face)
    ("purple"          . johnson-color-violet-face)
    ("slateblue"       . johnson-color-violet-face)
    ("violet"          . johnson-color-violet-face)
    ("darkviolet"      . johnson-color-violet-face)
    ;; Orange
    ("darkorange"      . johnson-color-orange-face)
    ("lightsalmon"     . johnson-color-orange-face)
    ("orange"          . johnson-color-orange-face)
    ("orangered"       . johnson-color-orange-face))
  "Mapping of DSL color names (downcased) to johnson faces.")

(defun johnson-dsl--classify-color-rgb (r g b)
  "Classify 16-bit RGB values R, G, B to a johnson color face."
  (let ((mx (max r g b))
        (mn (min r g b)))
    (cond
     ;; Near-black or near-white or low saturation → gray
     ((or (< mx 6554) (> mn 58982) (< (- mx mn) (/ mx 7)))
      'johnson-color-gray-face)
     ;; Red dominant
     ((= mx r)
      (cond
       ((> b (/ (* mx 3) 5)) 'johnson-color-violet-face)
       ((> g (/ (* mx 7) 10)) 'johnson-color-orange-face)
       ((> g (/ (* mx 2) 5)) 'johnson-color-brown-face)
       (t 'johnson-color-red-face)))
     ;; Green dominant
     ((= mx g)
      (if (> b (/ (* mx 7) 10))
          'johnson-color-blue-face
        'johnson-color-green-face))
     ;; Blue dominant
     (t
      (if (> r (/ (* mx 3) 5))
          'johnson-color-violet-face
        'johnson-color-blue-face)))))

(defun johnson-dsl--color-face (name)
  "Return the face for DSL color NAME (case-insensitive).
If NAME is nil or empty, return `johnson-color-default-face'."
  (if (or (null name) (string-empty-p name))
      'johnson-color-default-face
    (or (cdr (assoc (downcase name) johnson-dsl--color-alist))
        (when-let* ((rgb (color-values name)))
          (johnson-dsl--classify-color-rgb
           (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
        'johnson-color-default-face)))

;;;; Dictionary context for rendering

(defvar johnson-dsl--current-dict-dir nil
  "Directory of the dictionary being rendered.
Set by `johnson-dsl-retrieve-entry' for use by the renderer.")

(defvar johnson-dsl--current-dict-path nil
  "Full path of the dictionary file being rendered.
Set by `johnson-dsl-retrieve-entry' for use by the abbreviation loader.")

(defvar johnson-dsl--abbreviation-cache (make-hash-table :test #'equal)
  "Cache of abbreviation tables.
Maps abbreviation file path to a hash table of abbreviation to expansion,
or nil if no abbreviation file exists.")

;;;; Dictzip helpers

(defun johnson-dsl--dictzip-p (path)
  "Return non-nil if PATH is a dictzip-compressed DSL file."
  (string-suffix-p ".dsl.dz" path t))

;;;; Encoding detection

(defun johnson-dsl--detect-encoding (path)
  "Detect the encoding of the DSL file at PATH.
Handles both plain and dictzip-compressed (.dsl.dz) files.
Returns a symbol: `utf-16-le', `utf-16-be', `utf-8-with-signature', or `utf-8'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if (johnson-dsl--dictzip-p path)
        (insert (johnson-dictzip-read path 0 4))
      (insert-file-contents-literally path nil 0 4))
    (let ((b1 (and (> (point-max) 1) (char-after 1)))
          (b2 (and (> (point-max) 2) (char-after 2)))
          (b3 (and (> (point-max) 3) (char-after 3))))
      (cond
       ((and b1 b2 (= b1 #xff) (= b2 #xfe)) 'utf-16-le)
       ((and b1 b2 (= b1 #xfe) (= b2 #xff)) 'utf-16-be)
       ((and b1 b2 b3 (= b1 #xef) (= b2 #xbb) (= b3 #xbf)) 'utf-8-with-signature)
       (t 'utf-8)))))

;;;; Buffer cache

(defvar-local johnson-dsl--encoding nil
  "Encoding of the dictionary file in this cache buffer.")

(defun johnson-dsl--cache-buffer-name (path)
  "Return the cache buffer name for the dictionary at PATH."
  (format " *johnson-cache: %s" path))

(defun johnson-dsl--get-buffer (path)
  "Return (or create) a decoded cache buffer for the dictionary at PATH.
Handles both plain and dictzip-compressed (.dsl.dz) files.
The content is decoded so that character positions can be used
directly for indexing and retrieval."
  (let ((buf-name (johnson-dsl--cache-buffer-name path)))
    (or (get-buffer buf-name)
        (let* ((encoding (johnson-dsl--detect-encoding path))
               (coding (johnson-dsl--coding-system encoding)))
          (with-current-buffer (generate-new-buffer buf-name)
            (buffer-disable-undo)
            (fundamental-mode)
            (let ((inhibit-read-only t))
              (if (johnson-dsl--dictzip-p path)
                  ;; Decompress the entire dictzip file, then decode.
                  (let ((raw (johnson-dictzip-read-full path)))
                    (set-buffer-multibyte nil)
                    (insert raw)
                    ;; Decode the unibyte buffer in place.
                    (decode-coding-region (point-min) (point-max) coding)
                    (set-buffer-multibyte t))
                ;; Plain file: read with encoding.
                (let ((coding-system-for-read coding))
                  (insert-file-contents path))))
            ;; Remove BOM character if present at buffer start.
            (goto-char (point-min))
            (when (and (not (eobp)) (eq (char-after) #xfeff))
              (let ((inhibit-read-only t))
                (delete-char 1)))
            (setq-local johnson-dsl--encoding encoding)
            (setq buffer-read-only t)
            (current-buffer))))))

;;;; Encoding utilities

(defun johnson-dsl--coding-system (encoding)
  "Return the Emacs coding system for ENCODING symbol."
  (pcase encoding
    ('utf-16-le 'utf-16-le)
    ('utf-16-be 'utf-16-be)
    ('utf-8-with-signature 'utf-8)
    ('utf-8 'utf-8)
    (_ 'utf-8)))

(defun johnson-dsl--bom-length (encoding)
  "Return the BOM length in bytes for ENCODING."
  (pcase encoding
    ('utf-16-le 2)
    ('utf-16-be 2)
    ('utf-8-with-signature 3)
    (_ 0)))

;;;; Abbreviation support

(defun johnson-dsl--abbreviation-path (dict-path)
  "Derive the abbreviation file path from DICT-PATH.
For \"foo.dsl\" returns \"foo_abrv.dsl\"; for \"foo.dsl.dz\" returns
\"foo_abrv.dsl\"."
  (let ((base (if (johnson-dsl--dictzip-p dict-path)
                  (file-name-sans-extension
                   (file-name-sans-extension dict-path))
                (file-name-sans-extension dict-path))))
    (concat base "_abrv.dsl")))

(defun johnson-dsl--load-abbreviations (dict-path)
  "Load the abbreviation table for the dictionary at DICT-PATH.
Returns a hash table mapping abbreviation strings to their
expansions, or nil if no abbreviation file exists.  Results are
cached per dictionary directory."
  (let* ((abrv-path (johnson-dsl--abbreviation-path dict-path))
         (cached (gethash abrv-path johnson-dsl--abbreviation-cache 'missing)))
    (if (not (eq cached 'missing))
        ;; Cache hit: return the value (may be nil).
        cached
      ;; Cache miss: load abbreviations.
      (if (not (file-exists-p abrv-path))
          (progn
            (puthash abrv-path nil johnson-dsl--abbreviation-cache)
            nil)
          (let ((table (make-hash-table :test #'equal))
                (buf (johnson-dsl--get-buffer abrv-path)))
            (with-current-buffer buf
              (save-excursion
                (goto-char (point-min))
                ;; Skip metadata header lines.
                (while (and (not (eobp))
                            (or (looking-at "^#")
                                (looking-at "^[\n\r]")))
                  (forward-line 1))
                ;; Parse entries: headword on flush-left line, body on
                ;; indented lines.
                (let ((headword nil))
                  (while (not (eobp))
                    (cond
                     ;; Blank line: skip.
                     ((looking-at "^[ \t]*$")
                      (forward-line 1))
                     ;; Indented line: body.
                     ((looking-at "^[\t ]")
                      (when (and headword
                                 (not (gethash headword table)))
                        (let* ((line (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))
                               (trimmed (string-trim line))
                               (stripped (replace-regexp-in-string
                                          "\\[/?[a-z!*'][^]]*\\]" ""
                                          trimmed)))
                          (puthash headword stripped table)))
                      ;; Skip remaining body lines.
                      (forward-line 1)
                      (while (and (not (eobp))
                                  (looking-at "^[\t ]"))
                        (forward-line 1))
                      (setq headword nil))
                     ;; Flush-left line: headword.
                     (t
                      (setq headword
                            (string-trim-right
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))
                             "[\r]"))
                      (forward-line 1)))))))
            (puthash abrv-path table johnson-dsl--abbreviation-cache)
            table)))))

;;;; Format detection

(defun johnson-dsl-detect (path)
  "Return non-nil if PATH appears to be a DSL dictionary file.
Checks for .dsl or .dsl.dz extension and verifies that the first
non-BOM content starts with `#'."
  (and (or (string-suffix-p ".dsl" path t)
           (string-suffix-p ".dsl.dz" path t))
       (condition-case nil
           (let* ((encoding (johnson-dsl--detect-encoding path))
                  (bom-len (johnson-dsl--bom-length encoding))
                  (read-len (if (memq encoding '(utf-16-le utf-16-be)) 64 32))
                  (coding (johnson-dsl--coding-system encoding)))
             (with-temp-buffer
               (if (johnson-dsl--dictzip-p path)
                   ;; For dictzip: decompress a small chunk and decode.
                   (let ((raw (johnson-dictzip-read path 0 (+ bom-len read-len))))
                     (set-buffer-multibyte nil)
                     (insert raw)
                     ;; Skip BOM bytes.
                     (when (> bom-len 0)
                       (delete-region 1 (1+ bom-len)))
                     (decode-coding-region (point-min) (point-max) coding)
                     (set-buffer-multibyte t))
                 (let ((coding-system-for-read coding))
                   (insert-file-contents path nil bom-len (+ bom-len read-len))))
               (goto-char (point-min))
               (skip-chars-forward "\n\r\t ")
               (eq (char-after) ?#)))
         (error nil))))

;;;; Metadata parsing

(defconst johnson-dsl--language-aliases
  '(("Egnlish" . "English")
    ("GermanNewSpelling" . "German")
    ("PortugueseStandard" . "Portuguese")
    ("SpanishModernSort" . "Spanish")
    ("SpanishTraditionalSort" . "Spanish"))
  "Map non-standard DSL language names to standard forms.")

(defun johnson-dsl--normalize-language (name)
  "Normalize DSL language NAME to a standard form."
  (or (cdr (assoc name johnson-dsl--language-aliases)) name))

(defun johnson-dsl-parse-metadata (path)
  "Parse metadata headers from the DSL dictionary at PATH.
Handles both plain and dictzip-compressed (.dsl.dz) files.
Returns a plist (:name STRING :source-lang STRING :target-lang STRING)."
  (let* ((encoding (johnson-dsl--detect-encoding path))
         (coding (johnson-dsl--coding-system encoding))
         (name nil)
         (source-lang nil)
         (target-lang nil))
    (with-temp-buffer
      (if (johnson-dsl--dictzip-p path)
          ;; For dictzip: decompress first chunk (headers are at the top).
          (let* ((bom-len (johnson-dsl--bom-length encoding))
                 (read-bytes (if (memq encoding '(utf-16-le utf-16-be)) 8192 4096))
                 (raw (johnson-dictzip-read path 0 (+ bom-len read-bytes))))
            (set-buffer-multibyte nil)
            (insert raw)
            (when (> bom-len 0)
              (delete-region 1 (1+ bom-len)))
            (decode-coding-region (point-min) (point-max) coding)
            (set-buffer-multibyte t))
        (let* ((coding-system-for-read coding)
               (bom-len (johnson-dsl--bom-length encoding))
               (read-bytes (if (memq encoding '(utf-16-le utf-16-be)) 8192 4096)))
          (insert-file-contents path nil bom-len (+ bom-len read-bytes))))
      (goto-char (point-min))
      ;; Skip BOM character if present (the decoded stream may start with it).
      (when (and (not (eobp)) (eq (char-after) #xfeff))
        (forward-char 1))
      (while (and (not (eobp))
                  (looking-at "^#"))
        (cond
         ((looking-at "^#NAME\\s-+\"\\([^\"]*\\)\"")
          (setq name (match-string 1)))
         ((looking-at "^#INDEX_LANGUAGE\\s-+\"\\([^\"]*\\)\"")
          (setq source-lang (match-string 1)))
         ((looking-at "^#CONTENTS_LANGUAGE\\s-+\"\\([^\"]*\\)\"")
          (setq target-lang (match-string 1))))
        (forward-line 1)))
    (list :name (or name "")
          :source-lang (johnson-dsl--normalize-language (or source-lang ""))
          :target-lang (johnson-dsl--normalize-language (or target-lang "")))))

;;;; Headword expansion

(defun johnson-dsl--unescape-headword (headword)
  "Strip backslash escapes from HEADWORD.
Handles \\[, \\], \\{, \\}, \\(, \\)."
  (replace-regexp-in-string
   "\\\\\\([][{}()]\\)" "\\1" headword))

(defun johnson-dsl--expand-alternations (headword)
  "Expand alternation and unsorted-part syntax in HEADWORD.
Alternation: \"pre{a/b}suf\" => (\"preasuf\" \"prebsuf\").
Unsorted parts: \"{the }sample\" => (\"the sample\").
Handles escaped braces.  Returns a list of expanded headwords."
  (let ((result (list headword))
        (changed t))
    ;; Iteratively expand one brace group at a time.
    (while changed
      (setq changed nil)
      (let ((new-result nil))
        (dolist (hw result)
          ;; Find the first unescaped { ... }.
          (let ((found nil)
                (start 0))
            (while (and (not found)
                        (string-match "\\(?:^\\|[^\\\\]\\){\\([^}]*\\)}" hw start))
              (let* ((match-beg (if (eq (aref hw (match-beginning 0)) ?{)
                                    (match-beginning 0)
                                  (1+ (match-beginning 0))))
                     (match-end (match-end 0))
                     (inner (substring hw (1+ match-beg) (1- match-end))))
                (if (string-match-p "/" inner)
                    ;; Alternation: expand into multiple headwords.
                    (let* ((prefix (substring hw 0 match-beg))
                           (suffix (substring hw match-end))
                           (alternatives (split-string inner "/")))
                      (dolist (alt alternatives)
                        (push (concat prefix alt suffix) new-result))
                      (setq found t)
                      (setq changed t))
                  ;; Unsorted part: strip braces, keep content.
                  (let* ((prefix (substring hw 0 match-beg))
                         (suffix (substring hw match-end))
                         (replacement (concat prefix inner suffix)))
                    (push replacement new-result)
                    (setq found t)
                    (setq changed t)))))
            (unless found
              (push hw new-result))))
        (setq result (nreverse new-result))))
    result))

(defun johnson-dsl--expand-optionals (headword)
  "Expand optional parts in HEADWORD.
E.g., \"go(es)\" => (\"go\" \"goes\").
Handles escaped parens.  Returns a list of expanded headwords."
  (let ((result (list headword))
        (changed t))
    (while changed
      (setq changed nil)
      (let ((new-result nil))
        (dolist (hw result)
          (let ((found nil)
                (start 0))
            (while (and (not found)
                        (string-match "\\(?:^\\|[^\\\\]\\)(\\([^)]*\\))" hw start))
              (let* ((match-beg (if (eq (aref hw (match-beginning 0)) ?\()
                                    (match-beginning 0)
                                  (1+ (match-beginning 0))))
                     (match-end (match-end 0))
                     (inner (substring hw (1+ match-beg) (1- match-end)))
                     (prefix (substring hw 0 match-beg))
                     (suffix (substring hw match-end)))
                (push (concat prefix suffix) new-result)
                (push (concat prefix inner suffix) new-result)
                (setq found t)
                (setq changed t)))
            (unless found
              (push hw new-result))))
        (setq result (nreverse new-result))))
    result))

(defun johnson-dsl--split-on-slash (headword)
  "Split HEADWORD on unescaped `{/}' markers.
E.g., \"colour{/}color\" => (\"colour\" \"color\").
If no `{/}' is found, returns (HEADWORD)."
  (if (string-match-p "\\(?:^\\|[^\\\\]\\){/}" headword)
      (let ((parts (split-string headword "{/}"))
            (result nil))
        (dolist (part parts)
          (unless (string-empty-p part)
            (push part result)))
        (or (nreverse result) (list headword)))
    (list headword)))

(defconst johnson-dsl--max-headword-variants 64
  "Maximum number of expanded variants per headword.
Limits combinatorial blowup from malformed data.")

(defun johnson-dsl--expand-headword (headword)
  "Expand HEADWORD into a list of all variant headwords.
Handles split markers ({/}), alternation ({alt1/alt2}), optional
parts ((opt)), and escaped characters.  Returns a list of strings."
  ;; Strip DSL media/link markers ({{...}}) which should never appear
  ;; in headwords but may if body text lacks proper indentation.
  (let* ((cleaned (replace-regexp-in-string "{{\\(?:[^}]\\|}[^}]\\)*}}" "" headword))
         ;; First split on {/} markers.
         (split (johnson-dsl--split-on-slash cleaned))
         ;; Then expand alternations and optionals on each part.
         (expanded (cl-mapcan #'johnson-dsl--expand-alternations split))
         (expanded (cl-mapcan #'johnson-dsl--expand-optionals expanded))
         (expanded (mapcar #'johnson-dsl--unescape-headword expanded)))
    ;; Cap to prevent exponential blowup from pathological input.
    (when (> (length expanded) johnson-dsl--max-headword-variants)
      (setq expanded (seq-take expanded johnson-dsl--max-headword-variants)))
    (or expanded (list (johnson-dsl--unescape-headword headword)))))

;;;; Index building

(defun johnson-dsl-build-index (path callback)
  "Parse the DSL dictionary at PATH, calling CALLBACK for each entry.
CALLBACK is called as (funcall CALLBACK headword char-offset char-length)
where char-offset and char-length are character positions in the decoded
buffer (1-based offset, suitable for `buffer-substring-no-properties')."
  (let* ((buf (johnson-dsl--get-buffer path))
         (skipped 0)
         (count 0))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        ;; Skip metadata header lines (lines starting with #).
        (while (and (not (eobp))
                    (or (looking-at "^#")
                        (looking-at "^[\n\r]")))
          (forward-line 1))
        ;; Parse entries.
        (let ((headwords nil)
              (body-start nil))
          (while (not (eobp))
            (cond
             ;; Blank line: skip.
             ((looking-at "^[ \t]*$")
              (forward-line 1))
             ;; Body line: indented, or flush-left starting with `['
             ;; (DSL headwords never start with unescaped `['; some
             ;; dictionaries omit tab indentation and use `[m' tags
             ;; at column 0 for body lines).
             ((looking-at "^\\(?:[\t ]\\|\\[\\)")
              (unless body-start
                (setq body-start (point)))
              (forward-line 1)
              ;; Consume remaining body lines.
              (while (and (not (eobp))
                          (looking-at "^\\(?:[\t ]\\|\\[\\)"))
                (forward-line 1))
              ;; End of body.  Trim trailing blank lines.
              (let ((body-end (point)))
                (save-excursion
                  (goto-char body-end)
                  (while (and (> (point) body-start)
                              (progn (forward-line -1)
                                     (looking-at "^[ \t]*$")))
                    (setq body-end (point))))
                (when (and headwords body-start (> body-end body-start))
                  ;; Store 1-based character positions directly.
                  (let ((char-offset body-start)
                        (char-length (- body-end body-start)))
                    (dolist (raw-hw headwords)
                      (condition-case _err
                          (let ((expanded (johnson-dsl--expand-headword raw-hw)))
                            (dolist (hw expanded)
                              (unless (string-empty-p hw)
                                (funcall callback hw char-offset char-length)
                                (cl-incf count))))
                        (error (cl-incf skipped))))))
                (setq headwords nil)
                (setq body-start nil)))
             ;; Flush-left line: headword.
             (t
              (let ((hw (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
                ;; Strip carriage return if present.
                (when (string-suffix-p "\r" hw)
                  (setq hw (substring hw 0 -1)))
                (unless (string-empty-p hw)
                  (push hw headwords)))
              (forward-line 1)))))))
    (when (> skipped 0)
      (message "johnson-dsl: %d entries skipped due to parse errors" skipped))
    nil))

;;;; Entry retrieval

(defun johnson-dsl-retrieve-entry (path char-offset nchars)
  "Retrieve the entry body from the DSL dictionary at PATH.
CHAR-OFFSET and NCHARS specify the entry's location as character
positions in the decoded buffer (1-based offset)."
  (setq johnson-dsl--current-dict-dir (file-name-directory path))
  (setq johnson-dsl--current-dict-path path)
  (let ((buf (johnson-dsl--get-buffer path)))
    (with-current-buffer buf
      (buffer-substring-no-properties char-offset (+ char-offset nchars)))))

;;;; Entry rendering

(defun johnson-dsl-render-entry (raw-text)
  "Render DSL markup in RAW-TEXT into the current buffer with text properties.
Inserts the rendered text at point."
  ;; Strip carriage returns (common in UTF-16LE dictzip files).
  (setq raw-text (string-replace "\r" "" raw-text))
  ;; Strip leading tab/indentation from each line.
  (let* ((lines (split-string raw-text "\n"))
         (stripped (mapcar (lambda (line)
                            (if (string-match "^[\t ]+" line)
                                (substring line (match-end 0))
                              line))
                          lines))
         (text (string-join stripped "\n")))
    ;; Replace non-breaking spaces with regular spaces.  Some DSL
    ;; dictionaries use U+00A0 for indentation, which Emacs highlights
    ;; via `nobreak-space' face, producing visible underlines.
    (setq text (string-replace "\u00a0" " " text))
    ;; Unescape backslash-space sequences (DSL uses `\ ' for literal spaces).
    (setq text (replace-regexp-in-string "\\\\ " " " text))
    ;; Strip standalone backslash lines (used as visual separators in some
    ;; DSL dictionaries, e.g. Oxford Advanced Pronunciation Dictionary).
    (setq text (replace-regexp-in-string "^\\\\$" "" text))
    ;; Replace escaped brackets with placeholders before tag processing
    ;; so that \[...\] is not falsely consumed as a DSL tag.
    (setq text (replace-regexp-in-string "\\\\\\[" "\0LBRK\0" text))
    (setq text (replace-regexp-in-string "\\\\\\]" "\0RBRK\0" text))
    ;; Process {{...}} media references: render images, strip others.
    (setq text
          (replace-regexp-in-string
           "{{\\(\\(?:[^}]\\|}[^}]\\)*\\)}}"
           (lambda (match)
             (let ((filename (match-string 1 match)))
               (if (and (fboundp 'johnson--image-file-p)
                        johnson-dsl--current-dict-dir
                        (johnson--image-file-p filename))
                   ;; Mark for post-insertion (can't insert into a string).
                   (format "\0IMG\0%s\0" filename)
                 "")))
           text))
    ;; Insert and parse tags in-place.
    (let ((start (point))
          (tag-re "\\[/?[a-z!*'][^]]*\\]")
          (stack nil)
          (case-fold-search nil))
      (insert text)
      (let ((end (copy-marker (point) t)))
        ;; First pass: handle <<...>> cross-references.
        (save-excursion
          (goto-char start)
          (while (re-search-forward "<<\\([^>]+\\)>>" end t)
            (let ((ref-text (match-string 1))
                  (m-beg (match-beginning 0))
                  (m-end (match-end 0)))
              (delete-region m-beg m-end)
              (goto-char m-beg)
              (let ((btn-start (point)))
                (insert ref-text)
                (make-text-button btn-start (point)
                                  'face 'johnson-ref-face
                                  'johnson-ref-word ref-text
                                  'action (lambda (_btn)
                                            (johnson-lookup ref-text))
                                  'help-echo (format "Look up \"%s\"" ref-text))))))
        ;; Second pass: process DSL tags.
        (save-excursion
          (goto-char start)
          (while (re-search-forward tag-re end t)
            (let* ((tag-str (match-string 0))
                   (tag-beg (match-beginning 0))
                   (tag-end (match-end 0))
                   (closing-p (string-prefix-p "[/" tag-str))
                   (tag-content (substring tag-str 1 -1)) ; strip [ and ]
                   tag-name tag-args)
              ;; Parse tag name and arguments.
              (if closing-p
                  (setq tag-name (substring tag-content 1)) ; strip /
                (if (string-match "^\\([a-z!*']+\\)\\(?:\\s-+\\(.*\\)\\)?" tag-content)
                    (progn
                      (setq tag-name (match-string 1 tag-content))
                      (setq tag-args (match-string 2 tag-content)))
                  (setq tag-name tag-content)))
              ;; Delete the tag text.
              (delete-region tag-beg tag-end)
              (goto-char tag-beg)
              (cond
               ;; Self-closing margin tags: [m], [m0]-[m9]
               ((and (not closing-p) (string-match "^m\\([0-9]?\\)$" tag-name))
                (let* ((level-str (match-string 1 tag-name))
                       (level (if (string-empty-p level-str) 0
                                (string-to-number level-str)))
                       (indent (make-string (* level 2) ?\s))
                       (eol (save-excursion (end-of-line) (point))))
                  (put-text-property (point) (min eol end)
                                     'line-prefix indent)
                  (put-text-property (point) (min eol end)
                                     'wrap-prefix indent)))
               ;; [/m] closing: just remove the tag (already deleted)
               ((and closing-p (string-match "^m[0-9]?$" tag-name))
                nil)
               ;; [s] media tag: extract filename and insert image or play button
               ((and (not closing-p) (equal tag-name "s"))
                (let ((s-end (save-excursion
                               (when (re-search-forward "\\[/s\\]" end t)
                                 (match-end 0)))))
                  (when s-end
                    (let* ((content-end (- s-end 4))
                           (filename (subst-char-in-string
                                      ?\\ ?/
                                      (string-trim
                                       (buffer-substring-no-properties
                                        tag-beg content-end)))))
                      (delete-region tag-beg s-end)
                      (when (and (not (string-empty-p filename))
                                 johnson-dsl--current-dict-dir)
                        (let* ((path (expand-file-name
                                      filename
                                      johnson-dsl--current-dict-dir))
                               (resolved
                                (johnson--resolve-audio-file
                                 path johnson-dsl--current-dict-path)))
                          (when resolved
                            (if (and (fboundp 'johnson--image-file-p)
                                     (johnson--image-file-p resolved))
                                (johnson--insert-image resolved)
                              (johnson-insert-audio-button
                               resolved nil johnson-dsl--current-dict-path)))))))))
               ;; Opening tags: push onto stack.
               ((not closing-p)
                (push (list tag-name (point) tag-args) stack))
               ;; Closing tags: pop from stack and apply properties.
               (closing-p
                (let ((entry (cl-find tag-name stack :key #'car :test #'equal)))
                  (when entry
                    (setq stack (remove entry stack))
                    (let ((region-start (nth 1 entry))
                          (region-args (nth 2 entry)))
                      (johnson-dsl--apply-tag tag-name region-start (point)
                                              region-args)))))))))
        ;; Restore escaped bracket placeholders.
        (save-excursion
          (goto-char start)
          (while (search-forward "\0LBRK\0" end t)
            (replace-match "[" t t)))
        (save-excursion
          (goto-char start)
          (while (search-forward "\0RBRK\0" end t)
            (replace-match "]" t t)))
        ;; Unescape remaining DSL backslash sequences.
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\\\\\\([][()<>{}~@\\\\]\\)" end t)
            (replace-match "\\1" t)))
        ;; Process deferred {{image}} markers.
        (save-excursion
          (goto-char start)
          (while (re-search-forward "\0IMG\0\\([^\0]+\\)\0" end t)
            (let* ((filename (match-string 1))
                   (m-beg (match-beginning 0))
                   (m-end (match-end 0)))
              (delete-region m-beg m-end)
              (goto-char m-beg)
              (when johnson-dsl--current-dict-dir
                (let* ((path (expand-file-name
                              filename johnson-dsl--current-dict-dir))
                       (resolved
                        (johnson--resolve-audio-file
                         path johnson-dsl--current-dict-path)))
                  (when resolved
                    (johnson--insert-image resolved)))))))
        (set-marker end nil)))))

(defun johnson-dsl--apply-tag (tag-name region-start region-end tag-args)
  "Apply rendering for TAG-NAME over REGION-START to REGION-END.
TAG-ARGS is the tag argument string (e.g., color name for [c])."
  (pcase tag-name
    ("b"
     (add-face-text-property region-start region-end 'johnson-bold-face))
    ("i"
     (add-face-text-property region-start region-end 'johnson-italic-face))
    ("u"
     (add-face-text-property region-start region-end 'johnson-underline-face))
    ("c"
     (let ((face (johnson-dsl--color-face tag-args)))
       (add-face-text-property region-start region-end face)))
    ("sup"
     (add-face-text-property region-start region-end 'johnson-bold-face)
     (put-text-property region-start region-end
                        'display '((raise 0.3) (height 0.7))))
    ("sub"
     (put-text-property region-start region-end
                        'display '((raise -0.3) (height 0.7))))
    ("ex"
     (add-face-text-property region-start region-end 'johnson-example-face))
    ("*"
     (add-face-text-property region-start region-end 'johnson-optional-face))
    ("ref"
     (let ((ref-text (buffer-substring-no-properties region-start region-end)))
       (make-text-button region-start region-end
                         'face 'johnson-ref-face
                         'johnson-ref-word ref-text
                         'action (lambda (_btn) (johnson-lookup ref-text))
                         'help-echo (format "Look up \"%s\"" ref-text))))
    ("url"
     (let* ((display (buffer-substring-no-properties region-start region-end))
            (url (if (and tag-args (not (string-empty-p tag-args)))
                     tag-args
                   display)))
       (make-text-button region-start region-end
                         'face 'johnson-url-face
                         'action (lambda (_btn) (browse-url url))
                         'help-echo (format "Open %s" url))))
    ("lang"
     (when (and tag-args (string-match "id=\\([0-9]+\\)" tag-args))
       (put-text-property region-start region-end
                          'johnson-lang (match-string 1 tag-args))))
    ("trn"
     ;; Ensure blank line before and after.
     (johnson-dsl--ensure-block-separation region-start region-end))
    ("!trn"
     (johnson-dsl--ensure-block-separation region-start region-end))
    ("com"
     (add-face-text-property region-start region-end 'johnson-comment-face))
    ("p"
     (add-face-text-property region-start region-end 'johnson-abbreviation-face)
     (when johnson-dsl--current-dict-path
       (let* ((abbr-table (johnson-dsl--load-abbreviations
                           johnson-dsl--current-dict-path))
              (text (buffer-substring-no-properties region-start region-end))
              (expansion (and abbr-table (gethash text abbr-table))))
         (when expansion
           (put-text-property region-start region-end
                              'help-echo expansion)))))
    ("'"
     (add-face-text-property region-start region-end 'johnson-stress-face))
    ("t"
     (add-face-text-property region-start region-end 'johnson-italic-face))))

(defun johnson-dsl--ensure-block-separation (region-start region-end)
  "Ensure blank line separation around the region from REGION-START to REGION-END."
  (save-excursion
    ;; Ensure blank line after.
    (goto-char region-end)
    (unless (or (eobp) (looking-at "\n\n"))
      (when (looking-at "\n")
        (forward-char 1))
      (insert "\n"))
    ;; Ensure blank line before.
    (goto-char region-start)
    (unless (or (bobp)
                (save-excursion
                  (forward-char -1)
                  (and (> (point) (point-min))
                       (eq (char-before) ?\n)
                       (eq (char-after) ?\n))))
      (insert "\n"))))

;;;; Format registration

(provide 'johnson-dsl)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "dsl"
   :extensions '("dsl" "dsl.dz")
   :detect #'johnson-dsl-detect
   :parse-metadata #'johnson-dsl-parse-metadata
   :build-index #'johnson-dsl-build-index
   :retrieve-entry #'johnson-dsl-retrieve-entry
   :render-entry #'johnson-dsl-render-entry))

;;; johnson-dsl.el ends here
