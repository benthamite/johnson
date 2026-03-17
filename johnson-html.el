;;; johnson-html.el --- Shared HTML rendering utilities for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides shared HTML rendering utilities for the johnson
;; dictionary package.  It converts HTML markup into Emacs text properties
;; and faces.  Used by the StarDict and MDict format backends.

;;; Code:

(require 'cl-lib)

(autoload 'url-unhex-string "url-util")

(declare-function johnson-lookup "johnson")
(declare-function johnson-insert-audio-button "johnson")
(declare-function johnson--image-file-p "johnson")
(declare-function johnson--insert-image "johnson")

;;;; Internal variables

(defvar johnson-html--current-dict-dir nil
  "Directory of the dictionary being rendered.
Callers should set this before calling `johnson-html-render-region'
so that relative sound:// paths can be resolved.")

(defvar johnson-html--current-dict-path nil
  "Full path of the dictionary file being rendered.
Used to locate companion zip archives for audio extraction.")

(defvar johnson-html--resolve-resource-fn nil
  "When non-nil, a function to resolve resource paths.
Called with (DICT-PATH RESOURCE-NAME), should return a local file
path or nil.  Set by format backends (e.g., MDict for MDD lookup)
before calling `johnson-html-render-region'.")

;;;; Color mapping

(defun johnson-html--classify-color-rgb (r g b)
  "Classify 16-bit RGB values R, G, B to a johnson color face."
  (let ((mx (max r g b))
        (mn (min r g b)))
    (cond
     ((or (< mx 6554) (> mn 58982) (< (- mx mn) (/ mx 7)))
      'johnson-color-gray-face)
     ((= mx r)
      (cond
       ((> b (/ (* mx 3) 5)) 'johnson-color-violet-face)
       ((> g (/ (* mx 7) 10)) 'johnson-color-orange-face)
       ((> g (/ (* mx 2) 5)) 'johnson-color-brown-face)
       (t 'johnson-color-red-face)))
     ((= mx g)
      (if (> b (/ (* mx 7) 10))
          'johnson-color-blue-face
        'johnson-color-green-face))
     (t
      (if (> r (/ (* mx 3) 5))
          'johnson-color-violet-face
        'johnson-color-blue-face)))))

(defconst johnson-html--color-alist
  '(;; Green
    ("green" . johnson-color-green-face)
    ("darkgreen" . johnson-color-green-face)
    ("darkolivegreen" . johnson-color-green-face)
    ("darkseagreen" . johnson-color-green-face)
    ("forestgreen" . johnson-color-green-face)
    ("limegreen" . johnson-color-green-face)
    ("mediumseagreen" . johnson-color-green-face)
    ("olive" . johnson-color-green-face)
    ("olivedrab" . johnson-color-green-face)
    ("seagreen" . johnson-color-green-face)
    ("yellowgreen" . johnson-color-green-face)
    ;; Red
    ("red" . johnson-color-red-face)
    ("darkred" . johnson-color-red-face)
    ("crimson" . johnson-color-red-face)
    ("firebrick" . johnson-color-red-face)
    ("indianred" . johnson-color-red-face)
    ("lightcoral" . johnson-color-red-face)
    ("maroon" . johnson-color-red-face)
    ("tomato" . johnson-color-red-face)
    ;; Blue
    ("blue" . johnson-color-blue-face)
    ("aqua" . johnson-color-blue-face)
    ("cadetblue" . johnson-color-blue-face)
    ("cyan" . johnson-color-blue-face)
    ("darkblue" . johnson-color-blue-face)
    ("darkcyan" . johnson-color-blue-face)
    ("darkturquoise" . johnson-color-blue-face)
    ("dodgerblue" . johnson-color-blue-face)
    ("lightblue" . johnson-color-blue-face)
    ("lightseagreen" . johnson-color-blue-face)
    ("mediumblue" . johnson-color-blue-face)
    ("midnightblue" . johnson-color-blue-face)
    ("navy" . johnson-color-blue-face)
    ("royalblue" . johnson-color-blue-face)
    ("skyblue" . johnson-color-blue-face)
    ("steelblue" . johnson-color-blue-face)
    ("teal" . johnson-color-blue-face)
    ;; Gray
    ("black" . johnson-color-gray-face)
    ("gray" . johnson-color-gray-face)
    ("grey" . johnson-color-gray-face)
    ("darkgray" . johnson-color-gray-face)
    ("darkslategray" . johnson-color-gray-face)
    ("dimgray" . johnson-color-gray-face)
    ("lightgray" . johnson-color-gray-face)
    ("lightslategray" . johnson-color-gray-face)
    ("silver" . johnson-color-gray-face)
    ("slategray" . johnson-color-gray-face)
    ;; Brown
    ("brown" . johnson-color-brown-face)
    ("burlywood" . johnson-color-brown-face)
    ("chocolate" . johnson-color-brown-face)
    ("coral" . johnson-color-brown-face)
    ("darkgoldenrod" . johnson-color-brown-face)
    ("goldenrod" . johnson-color-brown-face)
    ("peru" . johnson-color-brown-face)
    ("rosybrown" . johnson-color-brown-face)
    ("saddlebrown" . johnson-color-brown-face)
    ("sandybrown" . johnson-color-brown-face)
    ("sienna" . johnson-color-brown-face)
    ("tan" . johnson-color-brown-face)
    ("wheat" . johnson-color-brown-face)
    ;; Violet / purple / magenta
    ("blueviolet" . johnson-color-violet-face)
    ("darkmagenta" . johnson-color-violet-face)
    ("darkorchid" . johnson-color-violet-face)
    ("darkslateblue" . johnson-color-violet-face)
    ("darkviolet" . johnson-color-violet-face)
    ("deeppink" . johnson-color-violet-face)
    ("fuchsia" . johnson-color-violet-face)
    ("indigo" . johnson-color-violet-face)
    ("magenta" . johnson-color-violet-face)
    ("mediumslateblue" . johnson-color-violet-face)
    ("mediumvioletred" . johnson-color-violet-face)
    ("orchid" . johnson-color-violet-face)
    ("palevioletred" . johnson-color-violet-face)
    ("plum" . johnson-color-violet-face)
    ("purple" . johnson-color-violet-face)
    ("slateblue" . johnson-color-violet-face)
    ("violet" . johnson-color-violet-face)
    ;; Orange
    ("darkorange" . johnson-color-orange-face)
    ("lightsalmon" . johnson-color-orange-face)
    ("orange" . johnson-color-orange-face)
    ("orangered" . johnson-color-orange-face))
  "Mapping of color names (downcased) to johnson faces.")

(defun johnson-html-color-to-face (color)
  "Map an HTML color name or hex code COLOR to a johnson face."
  (let ((lower (downcase (string-trim color))))
    (or (cdr (assoc lower johnson-html--color-alist))
        (when-let* ((rgb (color-values lower)))
          (johnson-html--classify-color-rgb
           (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
        'johnson-color-default-face)))

;;;; Tag application

(defun johnson-html--apply-tag (tag-name region-start region-end attrs)
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
       (let ((face (johnson-html-color-to-face (match-string 1 attrs))))
         (add-face-text-property region-start region-end face))))
    ("span"
     (when (string-match "color\\s-*:\\s-*\\([^;\"' >]+\\)" attrs)
       (let ((face (johnson-html-color-to-face (match-string 1 attrs))))
         (add-face-text-property region-start region-end face))))
    ("a"
     (cond
      ;; sound:// links -> audio button
      ((string-match "href\\s-*=\\s-*[\"']sound://\\([^\"']+\\)[\"']" attrs)
       (let ((filename (url-unhex-string
                        (subst-char-in-string
                         ?\\ ?/ (match-string 1 attrs)))))
         (if (and (not (string-empty-p filename))
                  johnson-html--current-dict-dir)
             (let ((audio-path (expand-file-name
                                filename
                                johnson-html--current-dict-dir)))
               (delete-region region-start region-end)
               (goto-char region-start)
               (johnson-insert-audio-button
                audio-path nil johnson-html--current-dict-path))
           (add-face-text-property region-start region-end 'johnson-ref-face))))
      ;; bword:// and entry:// links -> cross-reference button
      ((string-match "href\\s-*=\\s-*[\"']\\(?:bword://\\|entry://\\)?\\([^\"']+\\)[\"']" attrs)
       (let ((target (url-unhex-string (match-string 1 attrs))))
         (make-text-button region-start region-end
                           'face 'johnson-ref-face
                           'johnson-ref-word target
                           'action (lambda (_btn) (johnson-lookup target))
                           'help-echo (format "Look up \"%s\"" target))))
      ;; No href, just make it look like a link.
      (t
       (add-face-text-property region-start region-end 'johnson-ref-face))))
    ("div"
     ;; Block-level separation already handled by newline insertion.
     nil)))

;;;; HTML entity decoding

(defun johnson-html--decode-entities (start end)
  "Decode HTML entities in the region from START to END.
Uses a single-pass approach to avoid double-decoding (e.g.,
`&amp;lt;' should become `&lt;', not `<').
Returns the new end position."
  (save-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char start)
      (while (re-search-forward "&\\(?:#x\\([0-9a-fA-F]+\\)\\|#\\([0-9]+\\)\\|\\([a-zA-Z]+\\)\\);" end-marker t)
        (let ((replacement
               (cond
                ;; Hexadecimal numeric entity: &#xHHHH;
                ((match-string 1)
                 (let ((char (string-to-number (match-string 1) 16)))
                   (when (and (> char 0) (<= char (max-char)))
                     (string char))))
                ;; Decimal numeric entity: &#NNNN;
                ((match-string 2)
                 (let ((char (string-to-number (match-string 2))))
                   (when (and (> char 0) (<= char (max-char)))
                     (string char))))
                ;; Named entity.
                ((match-string 3)
                 (pcase (match-string 3)
                   ("amp"  "&")
                   ("lt"   "<")
                   ("gt"   ">")
                   ("nbsp" " ")
                   ("quot" "\"")
                   ("apos" "'")
                   (_ nil))))))
          (when replacement
            (replace-match replacement t t))))
      (prog1 (marker-position end-marker)
        (set-marker end-marker nil)))))

;;;; Region rendering

(defun johnson-html-render-region (start end)
  "Process HTML tags in the region from START to END.
Replaces tags with text properties."
  (save-excursion
    (let ((case-fold-search t))
    ;; Strip carriage returns (common in some MDict/StarDict entries).
    (goto-char start)
    (while (search-forward "\r" end t)
      (replace-match "")
      (setq end (1- end)))
    ;; Replace non-breaking spaces with regular spaces to avoid
    ;; visible underlines from the `nobreak-space' face.
    (goto-char start)
    (while (search-forward "\u00a0" end t)
      (replace-match " "))
    ;; Strip <style>...</style> and <script>...</script> blocks.
    (goto-char start)
    (while (re-search-forward "<style[^>]*>" end t)
      (let ((style-start (match-beginning 0)))
        (when (re-search-forward "</style>" end t)
          (let ((style-end (point)))
            (delete-region style-start style-end)
            (setq end (- end (- style-end style-start)))
            (goto-char style-start)))))
    (goto-char start)
    (while (re-search-forward "<script[^>]*>" end t)
      (let ((script-start (match-beginning 0)))
        (when (re-search-forward "</script>" end t)
          (let ((script-end (point)))
            (delete-region script-start script-end)
            (setq end (- end (- script-end script-start)))
            (goto-char script-start)))))
    ;; Strip <link> tags (CSS references, not useful in Emacs).
    (goto-char start)
    (while (re-search-forward "<link[^>]*/?>" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "")
        (setq end (- end len))))
    ;; Strip HTML comments <!-- ... -->.
    (goto-char start)
    (while (re-search-forward "<!--" end t)
      (let ((comment-start (match-beginning 0)))
        (if (search-forward "-->" end t)
            (let ((comment-end (point)))
              (delete-region comment-start comment-end)
              (setq end (- end (- comment-end comment-start)))
              (goto-char comment-start))
          (goto-char end))))
    ;; Replace <br>, <br/>, <hr>, <hr/> with newlines.
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
    ;; Replace <div> and </div> with newlines for block-level separation.
    (goto-char start)
    (while (re-search-forward "</?div[^>]*>" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "\n")
        (setq end (- end len -1))))
    ;; Process <img> tags (self-closing).
    (goto-char start)
    (while (re-search-forward "<img\\s-+\\([^>]*\\)/?>\\|<img\\s-+\\([^>]*\\)>" end t)
      (let* ((attrs (or (match-string 1) (match-string 2) ""))
             (tag-beg (match-beginning 0))
             (tag-end (match-end 0)))
        (delete-region tag-beg tag-end)
        (setq end (- end (- tag-end tag-beg)))
        (goto-char tag-beg)
        (when (string-match "src\\s-*=\\s-*[\"']\\([^\"']+\\)[\"']" attrs)
          (let* ((src (match-string 1 attrs))
                 (resolved
                  (cond
                   ;; Try disk relative to dict dir.
                   ((and johnson-html--current-dict-dir
                         (let ((f (expand-file-name src johnson-html--current-dict-dir)))
                           (when (file-exists-p f) f))))
                   ;; Try resource resolver (e.g., MDD).
                   ((and johnson-html--resolve-resource-fn
                         johnson-html--current-dict-path)
                    (funcall johnson-html--resolve-resource-fn
                             johnson-html--current-dict-path src))
                   (t nil))))
            (when resolved
              (if (and (fboundp 'johnson--image-file-p)
                       (johnson--image-file-p resolved))
                  (let ((before (point)))
                    (johnson--insert-image resolved)
                    (setq end (+ end (- (point) before))))
                (when (and (fboundp 'johnson-insert-audio-button)
                           (string-match-p
                            "\\.\\(?:wav\\|mp3\\|ogg\\|spx\\|opus\\)\\'"
                            resolved))
                  (let ((before (point)))
                    (johnson-insert-audio-button
                     resolved nil johnson-html--current-dict-path)
                    (setq end (+ end (- (point) before)))))))))))
    ;; Process paired tags using a stack-based approach.
    (let ((tag-re "<\\(/\\)?\\([a-zA-Z]+\\)\\([^>]*\\)>")
          (stack nil)
          (end-marker (copy-marker end t)))
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
                (let ((region-start (nth 1 entry))
                      (region-attrs (nth 2 entry)))
                  (johnson-html--apply-tag
                   tag-name region-start (point) region-attrs)
                  (when (markerp region-start)
                    (set-marker region-start nil))))))
           ;; Opening tag: use markers so positions track buffer changes.
           (t
            (push (list tag-name (copy-marker (point)) tag-attrs) stack)))))
      ;; Clean up any remaining markers on the stack.
      (dolist (entry stack)
        (when (markerp (nth 1 entry))
          (set-marker (nth 1 entry) nil)))
      ;; Decode HTML entities after all tag processing.
      (let ((new-end (johnson-html--decode-entities start (marker-position end-marker))))
        (set-marker end-marker nil)
        ;; Collapse runs of 3+ newlines (from nested div/p tags) to 2.
        (goto-char start)
        (while (re-search-forward "\n\\{3,\\}" new-end t)
          (let ((len (- (match-end 0) (match-beginning 0))))
            (replace-match "\n\n")
            (setq new-end (- new-end len -2))))
        new-end)))))

(provide 'johnson-html)

;;; johnson-html.el ends here
