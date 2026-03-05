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

(defun johnson-html-color-to-face (color)
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
       (let ((filename (subst-char-in-string
                        ?\\ ?/ (match-string 1 attrs))))
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
       (let ((target (match-string 1 attrs)))
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
Returns the new end position."
  (save-excursion
    ;; Named entities.
    (goto-char start)
    (while (re-search-forward "&amp;" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "&" t t)
        (setq end (- end len -1))))
    (goto-char start)
    (while (re-search-forward "&lt;" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "<" t t)
        (setq end (- end len -1))))
    (goto-char start)
    (while (re-search-forward "&gt;" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match ">" t t)
        (setq end (- end len -1))))
    (goto-char start)
    (while (re-search-forward "&nbsp;" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match " " t t)
        (setq end (- end len -1))))
    (goto-char start)
    (while (re-search-forward "&quot;" end t)
      (let ((len (- (match-end 0) (match-beginning 0))))
        (replace-match "\"" t t)
        (setq end (- end len -1))))
    ;; Decimal numeric entities: &#NNNN;
    (goto-char start)
    (while (re-search-forward "&#\\([0-9]+\\);" end t)
      (let ((len (- (match-end 0) (match-beginning 0)))
            (char (string-to-number (match-string 1))))
        (replace-match (string char) t t)
        (setq end (- end len -1))))
    ;; Hexadecimal numeric entities: &#xHHHH;
    (goto-char start)
    (while (re-search-forward "&#x\\([0-9a-fA-F]+\\);" end t)
      (let ((len (- (match-end 0) (match-beginning 0)))
            (char (string-to-number (match-string 1) 16)))
        (replace-match (string char) t t)
        (setq end (- end len -1))))
    end))

;;;; Region rendering

(defun johnson-html-render-region (start end)
  "Process HTML tags in the region from START to END.
Replaces tags with text properties."
  (save-excursion
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
                  (johnson-html--apply-tag
                   tag-name region-start (point) region-attrs)))))
           ;; Opening tag.
           (t
            (push (list tag-name (point) tag-attrs) stack))))))
    ;; Decode HTML entities after all tag processing.
    (johnson-html--decode-entities start end)))

(provide 'johnson-html)

;;; johnson-html.el ends here
