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

(declare-function johnson-register-format "johnson")
(declare-function johnson-lookup "johnson")

;;;; Faces

(defgroup johnson-dsl-faces nil
  "Faces for DSL dictionary rendering."
  :group 'johnson)

(defface johnson-bold-face
  '((t :inherit bold))
  "Face for bold text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-italic-face
  '((t :inherit italic))
  "Face for italic text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-underline-face
  '((t :underline t))
  "Face for underlined text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-example-face
  '((t :inherit italic :foreground "dim gray"))
  "Face for example text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-optional-face
  '((((background light)) :foreground "gray50")
    (((background dark)) :foreground "gray60"))
  "Face for optional/secondary text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-ref-face
  '((t :inherit link))
  "Face for cross-reference links in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-url-face
  '((t :inherit link))
  "Face for URL links in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comment text in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-stress-face
  '((t :inherit bold))
  "Face for stress marks in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-section-header-face
  '((t :inherit bold :extend t))
  "Face for section headers in DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-default-face
  '((((background light)) :foreground "dark green")
    (((background dark)) :foreground "green3"))
  "Default color face for DSL entries (green)."
  :group 'johnson-dsl-faces)

(defface johnson-color-green-face
  '((((background light)) :foreground "dark green")
    (((background dark)) :foreground "green3"))
  "Green color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-red-face
  '((((background light)) :foreground "dark red")
    (((background dark)) :foreground "indian red"))
  "Red color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-blue-face
  '((((background light)) :foreground "dark blue")
    (((background dark)) :foreground "steel blue"))
  "Blue color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-gray-face
  '((((background light)) :foreground "dim gray")
    (((background dark)) :foreground "dark gray"))
  "Gray color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-brown-face
  '((((background light)) :foreground "saddle brown")
    (((background dark)) :foreground "burlywood"))
  "Brown color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-violet-face
  '((((background light)) :foreground "dark violet")
    (((background dark)) :foreground "plum"))
  "Violet color face for DSL entries."
  :group 'johnson-dsl-faces)

(defface johnson-color-orange-face
  '((((background light)) :foreground "dark orange")
    (((background dark)) :foreground "orange"))
  "Orange color face for DSL entries."
  :group 'johnson-dsl-faces)

;;;; Color mapping

(defconst johnson-dsl--color-alist
  '(("green"      . johnson-color-green-face)
    ("darkgreen"   . johnson-color-green-face)
    ("red"         . johnson-color-red-face)
    ("darkred"     . johnson-color-red-face)
    ("crimson"     . johnson-color-red-face)
    ("blue"        . johnson-color-blue-face)
    ("darkblue"    . johnson-color-blue-face)
    ("steelblue"   . johnson-color-blue-face)
    ("gray"        . johnson-color-gray-face)
    ("darkgray"    . johnson-color-gray-face)
    ("dimgray"     . johnson-color-gray-face)
    ("brown"       . johnson-color-brown-face)
    ("saddlebrown" . johnson-color-brown-face)
    ("violet"      . johnson-color-violet-face)
    ("purple"      . johnson-color-violet-face)
    ("darkviolet"  . johnson-color-violet-face)
    ("orange"      . johnson-color-orange-face)
    ("darkorange"  . johnson-color-orange-face))
  "Mapping of DSL color names (downcased) to johnson faces.")

(defun johnson-dsl--color-face (name)
  "Return the face for DSL color NAME (case-insensitive).
If NAME is nil or empty, return `johnson-color-default-face'."
  (if (or (null name) (string-empty-p name))
      'johnson-color-default-face
    (or (cdr (assoc (downcase name) johnson-dsl--color-alist))
        'johnson-color-default-face)))

;;;; Encoding detection

(defun johnson-dsl--detect-encoding (path)
  "Detect the encoding of the DSL file at PATH.
Returns a symbol: `utf-16-le', `utf-16-be', `utf-8-with-signature', or `utf-8'."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 4)
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
The file is opened with its detected encoding so that character
positions can be used directly for indexing and retrieval."
  (let ((buf-name (johnson-dsl--cache-buffer-name path)))
    (or (get-buffer buf-name)
        (let* ((encoding (johnson-dsl--detect-encoding path))
               (coding-system-for-read (johnson-dsl--coding-system encoding)))
          (with-current-buffer (generate-new-buffer buf-name)
            (buffer-disable-undo)
            (fundamental-mode)
            (let ((inhibit-read-only t))
              (insert-file-contents path))
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

;;;; Format detection

(defun johnson-dsl-detect (path)
  "Return non-nil if PATH looks like a DSL dictionary file.
Checks for .dsl extension and verifies that the first non-BOM content
starts with `#'."
  (and (string-suffix-p ".dsl" path t)
       (condition-case nil
           (let* ((encoding (johnson-dsl--detect-encoding path))
                  (bom-len (johnson-dsl--bom-length encoding))
                  ;; For UTF-16 read enough bytes to get a few characters;
                  ;; for UTF-8, a small read suffices.
                  (read-len (if (memq encoding '(utf-16-le utf-16-be)) 64 32))
                  (coding (johnson-dsl--coding-system encoding)))
             (with-temp-buffer
               (let ((coding-system-for-read coding))
                 (insert-file-contents path nil bom-len (+ bom-len read-len)))
               (goto-char (point-min))
               (skip-chars-forward "\n\r\t ")
               (eq (char-after) ?#)))
         (error nil))))

;;;; Metadata parsing

(defun johnson-dsl-parse-metadata (path)
  "Parse metadata headers from the DSL dictionary at PATH.
Returns a plist (:name STRING :source-lang STRING :target-lang STRING)."
  (let* ((encoding (johnson-dsl--detect-encoding path))
         (coding (johnson-dsl--coding-system encoding))
         (name nil)
         (source-lang nil)
         (target-lang nil))
    (with-temp-buffer
      (let* ((coding-system-for-read coding)
             ;; Only read the first few KB — headers are always at the top.
             ;; UTF-16 uses 2 bytes per char, so read more bytes for those.
             (bom-len (johnson-dsl--bom-length encoding))
             (read-bytes (if (memq encoding '(utf-16-le utf-16-be)) 8192 4096)))
        (insert-file-contents path nil bom-len (+ bom-len read-bytes)))
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
          :source-lang (or source-lang "")
          :target-lang (or target-lang ""))))

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
  (let* ((cleaned (replace-regexp-in-string "{{[^}]*}}" "" headword))
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
             ;; Indented line: part of an entry body.
             ((looking-at "^[\t ]")
              (unless body-start
                (setq body-start (point)))
              (forward-line 1)
              ;; Consume remaining indented lines.
              (while (and (not (eobp))
                          (looking-at "^[\t ]"))
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
  (let ((buf (johnson-dsl--get-buffer path)))
    (with-current-buffer buf
      (buffer-substring-no-properties char-offset (+ char-offset nchars)))))

;;;; Entry rendering

(defun johnson-dsl-render-entry (raw-text)
  "Render DSL markup in RAW-TEXT into the current buffer with text properties.
Inserts the rendered text at point."
  ;; Strip leading tab/indentation from each line.
  (let* ((lines (split-string raw-text "\n"))
         (stripped (mapcar (lambda (line)
                            (if (string-match "^[\t ]+" line)
                                (substring line (match-end 0))
                              line))
                          lines))
         (text (string-join stripped "\n")))
    ;; Remove {{...}} media references.
    (setq text (replace-regexp-in-string "{{[^}]*}}" "" text))
    ;; Insert and parse tags in-place.
    (let ((start (point))
          (tag-re "\\[/?[a-z!*'][^]]*\\]")
          (stack nil))
      (insert text)
      (let ((end (point)))
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
                                  'action (lambda (_btn)
                                            (johnson-lookup ref-text))
                                  'help-echo (format "Look up \"%s\"" ref-text)))
              ;; Adjust end marker.
              (setq end (+ end (- (length ref-text) (- m-end m-beg)))))))
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
              (setq end (- end (- tag-end tag-beg)))
              (goto-char tag-beg)
              (cond
               ;; Self-closing margin tags: [m], [m0]-[m9]
               ((string-match "^m\\([0-9]?\\)$" tag-name)
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
               ;; [s] media tag: skip content until [/s]
               ((and (not closing-p) (equal tag-name "s"))
                (let ((s-end (save-excursion
                               (if (re-search-forward "\\[/s\\]" end t)
                                   (match-end 0)
                                 end))))
                  (delete-region tag-beg s-end)
                  (setq end (- end (- s-end tag-beg)))))
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
        ;; Handle [trn]/[!trn] block separation: ensure blank line separation.
        ;; This is handled by the blank-line logic already in the output.
        ))))

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
                         'action (lambda (_btn) (johnson-lookup ref-text))
                         'help-echo (format "Look up \"%s\"" ref-text))))
    ("url"
     (let ((url-text (buffer-substring-no-properties region-start region-end)))
       (make-text-button region-start region-end
                         'face 'johnson-url-face
                         'action (lambda (_btn) (browse-url url-text))
                         'help-echo (format "Open %s" url-text))))
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
     ;; Abbreviation marker: render content as-is.
     nil)
    ("'"
     (add-face-text-property region-start region-end 'johnson-stress-face))))

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
   :extensions '("dsl")
   :detect #'johnson-dsl-detect
   :parse-metadata #'johnson-dsl-parse-metadata
   :build-index #'johnson-dsl-build-index
   :retrieve-entry #'johnson-dsl-retrieve-entry
   :render-entry #'johnson-dsl-render-entry))

;;; johnson-dsl.el ends here
