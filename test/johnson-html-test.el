;;; johnson-html-test.el --- Tests for johnson-html -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>

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

;; ERT tests for the johnson-html shared HTML rendering module.

;;; Code:

(require 'ert)
(require 'johnson-html)
(require 'johnson)

;;;; Basic tag rendering

(ert-deftest johnson-html-test-bold ()
  "HTML <b> tags apply johnson-bold-face."
  (with-temp-buffer
    (insert "<b>bold text</b> normal")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "<b>" text))
      (should-not (string-match-p "</b>" text))
      (should (string-match-p "bold text" text)))
    (let ((pos (text-property-any (point-min) (point-max)
                                  'face 'johnson-bold-face)))
      (should pos))))

(ert-deftest johnson-html-test-italic ()
  "HTML <i> tags apply johnson-italic-face."
  (with-temp-buffer
    (insert "<i>italic</i> text")
    (johnson-html-render-region (point-min) (point-max))
    (let ((pos (text-property-any (point-min) (point-max)
                                  'face 'johnson-italic-face)))
      (should pos))))

(ert-deftest johnson-html-test-underline ()
  "HTML <u> tags apply johnson-underline-face."
  (with-temp-buffer
    (insert "<u>underlined</u>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((pos (text-property-any (point-min) (point-max)
                                  'face 'johnson-underline-face)))
      (should pos))))

(ert-deftest johnson-html-test-strong ()
  "HTML <strong> tags apply johnson-bold-face."
  (with-temp-buffer
    (insert "<strong>strong</strong>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((pos (text-property-any (point-min) (point-max)
                                  'face 'johnson-bold-face)))
      (should pos))))

(ert-deftest johnson-html-test-em ()
  "HTML <em> tags apply johnson-italic-face."
  (with-temp-buffer
    (insert "<em>emphasis</em>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((pos (text-property-any (point-min) (point-max)
                                  'face 'johnson-italic-face)))
      (should pos))))

;;;; Line breaks and paragraphs

(ert-deftest johnson-html-test-br ()
  "HTML <br> and <br/> tags are converted to newlines."
  (with-temp-buffer
    (insert "line1<br>line2<br/>line3")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "line1\nline2\nline3" text)))))

(ert-deftest johnson-html-test-p ()
  "HTML <p> tags produce blank lines."
  (with-temp-buffer
    (insert "<p>para1</p><p>para2</p>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "para1" text))
      (should (string-match-p "para2" text))
      (should-not (string-match-p "<p>" text)))))

;;;; Superscript

(ert-deftest johnson-html-test-sup ()
  "HTML <sup> tags set display properties for superscript."
  (with-temp-buffer
    (insert "H<sup>2</sup>O")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal text "H2O")))
    ;; Check the "2" has a display property.
    (goto-char (1+ (point-min)))
    (let ((display (get-text-property (point) 'display)))
      (should display))))

;;;; Font color

(ert-deftest johnson-html-test-font-color ()
  "HTML <font color> tags apply color faces."
  (with-temp-buffer
    (insert "<font color=\"blue\">blue text</font>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((found nil)
          (pos (point-min)))
      (while (and (< pos (point-max)) (not found))
        (let ((face (get-text-property pos 'face)))
          (when (and face (or (eq face 'johnson-color-blue-face)
                              (and (listp face)
                                   (memq 'johnson-color-blue-face face))))
            (setq found t)))
        (setq pos (1+ pos)))
      (should found))))

;;;; Span style color

(ert-deftest johnson-html-test-span-style-color ()
  "HTML <span style=\"color:...\"> tags apply color faces."
  (with-temp-buffer
    (insert "<span style=\"color: green\">green text</span>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((found nil)
          (pos (point-min)))
      (while (and (< pos (point-max)) (not found))
        (let ((face (get-text-property pos 'face)))
          (when (and face (or (eq face 'johnson-color-green-face)
                              (and (listp face)
                                   (memq 'johnson-color-green-face face))))
            (setq found t)))
        (setq pos (1+ pos)))
      (should found))))

;;;; Cross-reference links

(ert-deftest johnson-html-test-bword-link ()
  "HTML <a href=\"bword://word\"> creates a cross-reference button."
  (with-temp-buffer
    (insert "See <a href=\"bword://test\">test</a> entry.")
    (johnson-html-render-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((btn (next-button (point-min))))
      (should btn)
      (should (equal (button-label btn) "test")))))

(ert-deftest johnson-html-test-entry-link ()
  "HTML <a href=\"entry://word\"> creates a cross-reference button."
  (with-temp-buffer
    (insert "See <a href=\"entry://lookup\">lookup</a> here.")
    (johnson-html-render-region (point-min) (point-max))
    (goto-char (point-min))
    (let ((btn (next-button (point-min))))
      (should btn)
      (should (equal (button-label btn) "lookup")))))

;;;; Sound links

(ert-deftest johnson-html-test-sound-link ()
  "HTML <a href=\"sound://file.mp3\"> inserts an audio button."
  (with-temp-buffer
    (let ((johnson-html--current-dict-dir "/tmp/"))
      (insert "<a href=\"sound://audio.mp3\">play</a>")
      (johnson-html-render-region (point-min) (point-max))
      ;; The link text is replaced with an audio button.
      (let ((btn (button-at (point-min))))
        (should btn)))))

;;;; HTML entity decoding

(ert-deftest johnson-html-test-entity-amp ()
  "Decodes &amp; to &."
  (with-temp-buffer
    (insert "foo &amp; bar")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "foo & bar"))))

(ert-deftest johnson-html-test-entity-lt-gt ()
  "Decodes &lt; and &gt; to < and >."
  (with-temp-buffer
    (insert "&lt;tag&gt;")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "<tag>"))))

(ert-deftest johnson-html-test-entity-nbsp ()
  "Decodes &nbsp; to space."
  (with-temp-buffer
    (insert "word&nbsp;word")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "word word"))))

(ert-deftest johnson-html-test-entity-quot ()
  "Decodes &quot; to double quote."
  (with-temp-buffer
    (insert "&quot;quoted&quot;")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "\"quoted\""))))

(ert-deftest johnson-html-test-entity-decimal ()
  "Decodes decimal numeric entities &#65; to A."
  (with-temp-buffer
    (insert "&#65;&#66;&#67;")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "ABC"))))

(ert-deftest johnson-html-test-entity-hex ()
  "Decodes hexadecimal numeric entities &#x41; to A."
  (with-temp-buffer
    (insert "&#x41;&#x42;&#x43;")
    (johnson-html-render-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "ABC"))))

;;;; Style and script stripping

(ert-deftest johnson-html-test-strip-style ()
  "Strips <style>...</style> blocks including content."
  (with-temp-buffer
    (insert "before<style>body { color: red; }</style>after")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "before" text))
      (should (string-match-p "after" text))
      (should-not (string-match-p "style" text))
      (should-not (string-match-p "color" text)))))

(ert-deftest johnson-html-test-strip-script ()
  "Strips <script>...</script> blocks including content."
  (with-temp-buffer
    (insert "before<script>alert('hi');</script>after")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "before" text))
      (should (string-match-p "after" text))
      (should-not (string-match-p "script" text))
      (should-not (string-match-p "alert" text)))))

(ert-deftest johnson-html-test-strip-comment ()
  "Strips HTML comments <!-- ... -->."
  (with-temp-buffer
    (insert "before<!--s054951a-->after")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal text "beforeafter")))))

(ert-deftest johnson-html-test-strip-comment-multiline ()
  "Strips multiline HTML comments."
  (with-temp-buffer
    (insert "before<!-- multi\nline\ncomment -->after")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal text "beforeafter")))))

;;;; Nested tags

(ert-deftest johnson-html-test-nested-tags ()
  "Nested HTML tags are handled correctly."
  (with-temp-buffer
    (insert "<b><i>bold italic</i></b>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal text "bold italic"))
      (should-not (string-match-p "<" text)))
    ;; Should have both bold and italic faces.
    (goto-char (point-min))
    (let ((face (get-text-property (point) 'face)))
      (should (listp face))
      (should (memq 'johnson-bold-face face))
      (should (memq 'johnson-italic-face face)))))

;;;; Div block separation

(ert-deftest johnson-html-test-div-block-separation ()
  "HTML <div> tags insert newlines for block-level separation."
  (with-temp-buffer
    (insert "<div>block1</div><div>block2</div>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "block1" text))
      (should (string-match-p "block2" text))
      (should-not (string-match-p "div" text))
      ;; There should be newlines separating the blocks.
      (should (string-match-p "\n" text)))))

;;;; Tags fully removed

(ert-deftest johnson-html-test-tags-removed ()
  "All HTML tags are removed from the rendered output."
  (with-temp-buffer
    (insert "<b>bold</b> <i>italic</i> <u>underline</u>")
    (johnson-html-render-region (point-min) (point-max))
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "<" text))
      (should-not (string-match-p ">" text))
      (should (string-match-p "bold" text))
      (should (string-match-p "italic" text))
      (should (string-match-p "underline" text)))))

;;;; Color mapping

(ert-deftest johnson-html-test-color-to-face ()
  "Maps HTML color names to johnson faces."
  (should (eq (johnson-html-color-to-face "blue") 'johnson-color-blue-face))
  (should (eq (johnson-html-color-to-face "Blue") 'johnson-color-blue-face))
  (should (eq (johnson-html-color-to-face "red") 'johnson-color-red-face))
  (should (eq (johnson-html-color-to-face "green") 'johnson-color-green-face))
  (should (eq (johnson-html-color-to-face "gray") 'johnson-color-gray-face))
  (should (eq (johnson-html-color-to-face "brown") 'johnson-color-brown-face))
  (should (eq (johnson-html-color-to-face "violet") 'johnson-color-violet-face))
  (should (eq (johnson-html-color-to-face "orange") 'johnson-color-orange-face))
  (should (eq (johnson-html-color-to-face "#ff0000") 'johnson-color-red-face))
  (should (eq (johnson-html-color-to-face "darkmagenta") 'johnson-color-violet-face))
  (should (eq (johnson-html-color-to-face "#8B008B") 'johnson-color-violet-face)))

(provide 'johnson-html-test)
;;; johnson-html-test.el ends here
