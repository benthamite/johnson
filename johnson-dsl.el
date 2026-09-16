;;; johnson-dsl.el --- DSL (ABBYY Lingvo) format backend for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Assisted-by: various LLMs (Claude, Codex)
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

;; This module provides the DSL (ABBYY Lingvo) format backend for the
;; johnson dictionary package.  It handles encoding detection, parsing of
;; DSL metadata and entries, headword expansion (alternation and optional
;; parts), byte-offset indexing, entry retrieval via a raw-text buffer
;; cache, and full rendering of DSL markup tags into Emacs text properties
;; and faces.

;;; Code:

(require 'cl-lib)
(require 'johnson-dictzip)
(require 'johnson-html)

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

;; Color name-to-face mapping and RGB classification are shared with
;; the HTML backend.  See `johnson-html-color-to-face' in johnson-html.el.
(defalias 'johnson-dsl--color-face #'johnson-html-color-to-face
  "Return the face for DSL color NAME (case-insensitive).
Delegates to the shared implementation in johnson-html.el.")

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

(defvar johnson-dsl--prepared nil
  "Non-nil when rendering uses an explicit prepared context.
Bound by `johnson-dsl-render-entry-with-context'.  When non-nil,
abbreviation and media lookups read only the prepared context
variables and never touch abbreviation files or archives.")

(defvar johnson-dsl--current-abbreviations nil
  "Alist of abbreviation to expansion for prepared rendering.
Bound by `johnson-dsl-render-entry-with-context' from the packet
context built by `johnson-dsl-worker-prepare-entry'.")

(defvar johnson-dsl--current-resources nil
  "Alist of media reference to resolved path for prepared rendering.
Bound by `johnson-dsl-render-entry-with-context' from the packet
context built by `johnson-dsl-worker-prepare-entry'.")

;;;; Dictzip helpers

(defun johnson-dsl--dictzip-p (path)
  "Return non-nil if PATH is a dictzip-compressed DSL file."
  (string-suffix-p ".dsl.dz" path t))

;;;; Encoding detection

(defconst johnson-dsl--encoding-sample-bytes 512
  "Number of leading bytes examined when detecting a BOM-less encoding.")

(defun johnson-dsl--detect-encoding (path)
  "Detect the encoding of the DSL file at PATH.
Handles both plain and dictzip-compressed (.dsl.dz) files.
Returns a symbol: `utf-16-le' or `utf-16-be' (UTF-16 with a BOM),
`utf-16le' or `utf-16be' (UTF-16 without a BOM), `utf-8-with-signature',
or `utf-8'."
  (let* ((sample (johnson-dsl--read-leading-bytes
                  path johnson-dsl--encoding-sample-bytes))
         (len (length sample))
         (b1 (and (> len 0) (aref sample 0)))
         (b2 (and (> len 1) (aref sample 1)))
         (b3 (and (> len 2) (aref sample 2))))
    (cond
     ((and b1 b2 (= b1 #xff) (= b2 #xfe)) 'utf-16-le)
     ((and b1 b2 (= b1 #xfe) (= b2 #xff)) 'utf-16-be)
     ((and b1 b2 b3 (= b1 #xef) (= b2 #xbb) (= b3 #xbf)) 'utf-8-with-signature)
     ((johnson-dsl--bomless-utf-16 sample))
     (t 'utf-8))))

(defun johnson-dsl--read-leading-bytes (path count)
  "Return up to COUNT leading bytes of the DSL file at PATH as a unibyte string.
Handles both plain and dictzip-compressed (.dsl.dz) files."
  (if (johnson-dsl--dictzip-p path)
      (let* ((header (johnson-dictzip--parse-header path))
             (size (* (plist-get header :chlen) (plist-get header :chcnt))))
        (johnson-dictzip-read path 0 (min count size)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path nil 0 count)
      (buffer-string))))

(defun johnson-dsl--bomless-utf-16 (sample)
  "Return the UTF-16 byte order of SAMPLE when it lacks a BOM, else nil.
Return `utf-16le' or `utf-16be'.  A leading header line yields the byte
pair `#' NUL (little endian) or NUL `#' (big endian).  Otherwise the NUL
bytes of mostly-ASCII text sit at odd positions for little endian and at
even positions for big endian; this heuristic requires NULs in at least a
quarter of SAMPLE and a three-to-one imbalance between the two parities."
  (let ((len (length sample)))
    (cond
     ((< len 2) nil)
     ((and (= (aref sample 0) ?#) (= (aref sample 1) 0)) 'utf-16le)
     ((and (= (aref sample 0) 0) (= (aref sample 1) ?#)) 'utf-16be)
     (t
      (let ((even 0)
            (odd 0))
        (dotimes (i len)
          (when (zerop (aref sample i))
            (if (cl-evenp i) (cl-incf even) (cl-incf odd))))
        (cond
         ((< (* 4 (+ even odd)) len) nil)
         ((> odd (* 3 even)) 'utf-16le)
         ((> even (* 3 odd)) 'utf-16be)))))))

(defun johnson-dsl--utf-16-p (encoding)
  "Return non-nil when ENCODING is one of the UTF-16 encoding symbols."
  (memq encoding '(utf-16-le utf-16-be utf-16le utf-16be)))

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
    ('utf-16le 'utf-16le)
    ('utf-16be 'utf-16be)
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
                  (read-len (if (johnson-dsl--utf-16-p encoding) 64 32))
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
                 (read-bytes (if (johnson-dsl--utf-16-p encoding) 8192 4096))
                 (raw (johnson-dictzip-read path 0 (+ bom-len read-bytes))))
            (set-buffer-multibyte nil)
            (insert raw)
            (when (> bom-len 0)
              (delete-region 1 (1+ bom-len)))
            (decode-coding-region (point-min) (point-max) coding)
            (set-buffer-multibyte t))
        (let* ((coding-system-for-read coding)
               (bom-len (johnson-dsl--bom-length encoding))
               (read-bytes (if (johnson-dsl--utf-16-p encoding) 8192 4096)))
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

(defconst johnson-dsl--max-headword-variants 64
  "Maximum number of expanded variants per headword.
Limits combinatorial blowup from malformed data.")

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
        (setq result (johnson-dsl--cap-variants (nreverse new-result)))))
    result))

(defun johnson-dsl--cap-variants (variants)
  "Return VARIANTS truncated to `johnson-dsl--max-headword-variants'.
Applied after every expansion pass so that the number of headwords in
flight, and hence the cost of the next pass, stays bounded."
  (if (> (length variants) johnson-dsl--max-headword-variants)
      (seq-take variants johnson-dsl--max-headword-variants)
    variants))

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
        (setq result (johnson-dsl--cap-variants (nreverse new-result)))))
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

(defun johnson-dsl--expand-headword (headword)
  "Expand HEADWORD into a list of all variant headwords.
Handles split markers ({/}), alternation ({alt1/alt2}), optional
parts ((opt)), and escaped characters.  Returns a list of strings."
  ;; Strip DSL media/link markers ({{...}}) which should never appear
  ;; in headwords but may if body text lacks proper indentation.
  (let* ((cleaned (replace-regexp-in-string "{{\\(?:[^}]\\|}[^}]\\)*}}" "" headword))
         ;; Remove formatting tags before any brace or paren expansion, so
         ;; the "/" of a closing tag is never taken for an alternation.
         (cleaned (johnson-dsl--strip-headword-tags cleaned))
         ;; First split on {/} markers.
         (split (johnson-dsl--split-on-slash cleaned))
         ;; Then expand alternations and optionals on each part.
         (expanded (cl-mapcan #'johnson-dsl--expand-alternations split))
         (expanded (cl-mapcan #'johnson-dsl--expand-optionals expanded))
         (expanded (mapcar #'johnson-dsl--unescape-headword expanded))
         (expanded (delete "" (mapcar #'string-trim expanded))))
    ;; Cap to prevent exponential blowup from pathological input.
    (when (> (length expanded) johnson-dsl--max-headword-variants)
      (setq expanded (seq-take expanded johnson-dsl--max-headword-variants)))
    expanded))

(defconst johnson-dsl--headword-annotation-re
  (concat "\\[\\(p\\|com\\|c\\(?: [^]]*\\)?\\)\\]"
          "[^][{}]*"
          "\\[/\\(?:p\\|com\\|c\\)\\]")
  "Regexp matching a `[p]', `[com]' or `[c]' element in a headword line.
The element must not contain another tag or cross a brace boundary.")

(defun johnson-dsl--strip-headword-tags (headword)
  "Remove DSL formatting tags from HEADWORD, keeping escaped brackets.
Part-of-speech `[p]', comment `[com]' and color `[c]' elements annotate
the headword (Spanish dictionaries write `lexicógrafo[p]f.[/p] [c]- fa[/c]'
for the part of speech and the feminine ending), so they are dropped with
their content when they do not cross a brace boundary.  Every other tag
is removed but its text kept, since tags like `[i]' wrap letters of the
headword itself."
  (let* ((protected (replace-regexp-in-string
                     "\\\\\\]" "\0RBRK\0"
                     (replace-regexp-in-string "\\\\\\[" "\0LBRK\0" headword)))
         (previous nil))
    ;; Nested annotations such as `[p][p]m.[/p][/p]' need repeated passes.
    (while (not (equal previous protected))
      (setq previous protected)
      (setq protected (replace-regexp-in-string
                       johnson-dsl--headword-annotation-re "" protected)))
    (setq protected (replace-regexp-in-string "\\[/?[a-z!*'][^]]*\\]" ""
                                              protected))
    (replace-regexp-in-string
     "\0RBRK\0" "\\\\]"
     (replace-regexp-in-string "\0LBRK\0" "\\\\[" protected))))

;;;; Index building

(defun johnson-dsl-build-index (path callback)
  "Parse the DSL dictionary at PATH, calling CALLBACK for each entry.
CALLBACK is called as (funcall CALLBACK headword char-offset char-length)
where char-offset and char-length are character positions in the decoded
buffer (1-based offset, suitable for `buffer-substring-no-properties')."
  (let* ((buf (johnson-dsl--get-buffer path))
         (skipped 0)
         (continued 0)
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
              (body-start nil)
              (in-comment nil))
          (while (not (eobp))
            (cond
             ;; Inside a multi-line {{ ... }} comment block: skip until
             ;; the line that closes it.
             (in-comment
              (when (looking-at ".*}}")
                (setq in-comment nil))
              (forward-line 1))
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
              ;; Consume remaining body lines, including body text that
              ;; a converter left at column 0 without indentation.
              (while (and (not (eobp))
                          (or (looking-at "^\\(?:[\t ]\\|\\[\\)")
                              (and (johnson-dsl--body-continuation-p
                                    (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                                   (cl-incf continued))))
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
                (if (johnson-dsl--opens-comment-p hw)
                    (setq in-comment t)
                  (unless (string-empty-p hw)
                    (push hw headwords))))
              (forward-line 1)))))))
    (when (> skipped 0)
      (message "johnson-dsl: %d entries skipped due to parse errors" skipped))
    (when (> continued 0)
      (message "johnson-dsl: %d unindented body lines treated as entry continuation in %s"
               continued (file-name-nondirectory path)))
    nil))

(defun johnson-dsl--opens-comment-p (line)
  "Return non-nil when LINE opens a {{ ... }} comment that it does not close."
  (string-match-p "{{" (replace-regexp-in-string
                        "{{\\(?:[^}]\\|}[^}]\\)*}}" "" line)))

(defconst johnson-dsl--max-headword-length 256
  "Longest headword, in characters, that a column-0 line may hold.
A longer line that follows an entry body is body text lacking indentation.")

(defun johnson-dsl--body-continuation-p (line)
  "Return non-nil when the column-0 LINE is body text rather than a headword.
Margin tags (`[m1]', `[/m]') are paragraph markup that never occurs in a
headword, and a headword never exceeds `johnson-dsl--max-headword-length'
once its tags are stripped.  Other closing tags are not evidence: Corominas
writes `Enfe[i]š[/i]tillar' and Harrap's `dureza [p]f[/p]' as headword
lines directly after a body."
  (or (string-match-p "\\[/?m[0-9]*\\]" line)
      (> (length (string-trim (johnson-dsl--strip-headword-tags line)))
         johnson-dsl--max-headword-length)))

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

;;;; Worker entry preparation

(defun johnson-dsl-worker-prepare-entry (dict _match raw)
  "Return the serializable entry packet for RAW retrieved from DICT.
DICT is the dictionary plist and MATCH the database match, which is
ignored.  Load the abbreviation table and resolve referenced media in
the retrieval worker, so the parent can render from the packet context
without touching abbreviation files or archives."
  (let* ((path (plist-get dict :path))
         (dir (file-name-directory path)))
    (list :raw raw
          :context
          (list :prepared t
                :dict-path path
                :dict-dir dir
                :abbreviations (johnson-dsl--referenced-abbreviations path raw)
                :resources (johnson-dsl--referenced-resources path dir raw)))))

(defun johnson-dsl--referenced-abbreviations (path raw)
  "Return an alist of the abbreviation expansions referenced by RAW.
PATH is the dictionary file whose abbreviation table is consulted.
Only expansions of abbreviations referenced by `[p]' tags in RAW are
retained."
  (when-let* ((table (johnson-dsl--load-abbreviations path)))
    (let ((abbrevs nil))
      (dolist (ref (johnson-dsl--abbreviation-references raw))
        (when-let* ((expansion (gethash ref table)))
          (push (cons ref expansion) abbrevs)))
      (nreverse abbrevs))))

(defun johnson-dsl--abbreviation-references (raw)
  "Return the deduplicated `[p]' abbreviation texts referenced in RAW."
  (let ((refs nil)
        (start 0))
    (while (string-match "\\[p\\]\\([^][]+\\)\\[/p\\]" raw start)
      (push (match-string 1 raw) refs)
      (setq start (match-end 0)))
    (delete-dups (nreverse refs))))

(defun johnson-dsl--referenced-resources (path dir raw)
  "Return an alist of the resolved media resources referenced by RAW.
PATH is the dictionary file and DIR its directory.  Each referenced
media file is resolved through `johnson--resolve-audio-file', which
extracts from companion archives when needed; unresolvable references
are omitted."
  (let ((resources nil))
    (dolist (ref (johnson-dsl--media-references raw))
      (when-let* ((resolved (johnson--resolve-audio-file
                             (expand-file-name ref dir) path)))
        (push (cons ref resolved) resources)))
    (nreverse resources)))

(defun johnson-dsl--media-references (raw)
  "Return the deduplicated media resources referenced in RAW.
Collects `{{RESOURCE}}' references and `[s]RESOURCE[/s]' media tags,
normalized the way rendering normalizes them."
  (let ((refs nil)
        (start 0))
    (while (string-match "{{\\(\\(?:[^}]\\|}[^}]\\)*\\)}}" raw start)
      (push (match-string 1 raw) refs)
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match "\\[s\\]\\([^][]+\\)\\[/s\\]" raw start)
      ;; Capture positions before normalizing: `string-trim' runs
      ;; `string-match' internally and clobbers the loop's match data.
      (let ((name (match-string 1 raw))
            (next (match-end 0)))
        (push (subst-char-in-string ?\\ ?/ (string-trim name)) refs)
        (setq start next)))
    (delete-dups (nreverse refs))))

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
                        (let ((resolved (johnson-dsl--resolve-media filename)))
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
                (let ((resolved (johnson-dsl--resolve-media filename)))
                  (when resolved
                    (johnson--insert-image resolved)))))))
        (set-marker end nil)))))

(defun johnson-dsl--resolve-media (filename)
  "Resolve media FILENAME against the current rendering context.
In prepared mode, consult only the prepared resource mapping and files
already present next to the dictionary; otherwise resolve through
`johnson--resolve-audio-file', which may extract from companion
archives."
  (let ((path (expand-file-name filename johnson-dsl--current-dict-dir)))
    (if johnson-dsl--prepared
        (or (cdr (assoc filename johnson-dsl--current-resources))
            (and (file-exists-p path) path))
      (johnson--resolve-audio-file path johnson-dsl--current-dict-path))))

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
     (let ((face (if tag-args
                     (johnson-dsl--color-face tag-args)
                   'johnson-color-default-face)))
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
     (unless (johnson-dsl--inline-translation-p region-start)
       (johnson-dsl--ensure-block-separation region-start region-end)))
    ("!trn"
     (johnson-dsl--ensure-block-separation region-start region-end))
    ("com"
     (add-face-text-property region-start region-end 'johnson-comment-face))
    ("p"
     (add-face-text-property region-start region-end 'johnson-abbreviation-face)
     (let* ((text (buffer-substring-no-properties region-start region-end))
            (expansion (johnson-dsl--abbreviation-expansion text)))
       (when expansion
         (put-text-property region-start region-end
                            'help-echo expansion))))
    ("'"
     (add-face-text-property region-start region-end 'johnson-stress-face))
    ("t"
     (add-face-text-property region-start region-end 'johnson-italic-face))))

(defun johnson-dsl--abbreviation-expansion (text)
  "Return the expansion for abbreviation TEXT, or nil.
In prepared mode, read only the prepared abbreviation alist; otherwise
load the abbreviation table for the current dictionary."
  (if johnson-dsl--prepared
      (cdr (assoc text johnson-dsl--current-abbreviations))
    (when johnson-dsl--current-dict-path
      (when-let* ((table (johnson-dsl--load-abbreviations
                          johnson-dsl--current-dict-path)))
        (gethash text table)))))

(defun johnson-dsl--inline-translation-p (region-start)
  "Return non-nil when REGION-START follows an inline translation arrow."
  (save-excursion
    (goto-char region-start)
    (let ((line-start (line-beginning-position)))
      (skip-chars-backward " \t" line-start)
      (and (> (point) line-start)
           (eq (char-before) ?▶)))))

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

(defun johnson-dsl-render-entry-with-context (raw context)
  "Render DSL entry RAW using the explicit render CONTEXT.
RAW is the raw entry text and CONTEXT a plist with :prepared,
:dict-path, :dict-dir, :abbreviations, and :resources as built by
`johnson-dsl-worker-prepare-entry'.  Bind the rendering globals from
CONTEXT and call `johnson-dsl-render-entry'; abbreviation and media
lookups read only the prepared context plus files already on disk."
  (let ((johnson-dsl--prepared (plist-get context :prepared))
        (johnson-dsl--current-dict-path (plist-get context :dict-path))
        (johnson-dsl--current-dict-dir (plist-get context :dict-dir))
        (johnson-dsl--current-abbreviations (plist-get context :abbreviations))
        (johnson-dsl--current-resources (plist-get context :resources)))
    (johnson-dsl-render-entry raw)))

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
   :render-entry #'johnson-dsl-render-entry
   :worker-prepare-entry #'johnson-dsl-worker-prepare-entry
   :render-entry-with-context #'johnson-dsl-render-entry-with-context))

;;; johnson-dsl.el ends here
