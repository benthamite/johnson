;;; johnson-transient.el --- Transient menu for johnson -*- lexical-binding: t; -*-

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

;; Transient-based dispatch menu for johnson, providing quick access
;; to all major commands and toggles.

;;; Code:

(require 'transient)
(require 'cl-lib)

;; Variables (defined in johnson.el)
(defvar johnson-dictionary-directories)
(defvar johnson-display-images)
(defvar johnson-fts-enabled)
(defvar johnson-history-persist)
(defvar johnson-ref-scope)
(defvar johnson--current-word)
(defvar johnson--current-source-lang)
(defvar johnson--current-target-lang)

;; Commands used as suffixes (defined in johnson.el)
(declare-function johnson-lookup "johnson")
(declare-function johnson-search "johnson")
(declare-function johnson--available-source-langs "johnson")
(declare-function johnson--available-target-langs "johnson")
(declare-function johnson-jump-to-section "johnson")
(declare-function johnson-next-section "johnson")
(declare-function johnson-prev-section "johnson")
(declare-function johnson-ace-link "johnson")
(declare-function johnson-follow-ref "johnson")
(declare-function johnson-history-back "johnson")
(declare-function johnson-history-forward "johnson")
(declare-function johnson-history-list "johnson")
(declare-function johnson-history-clear "johnson")
(declare-function johnson-bookmark-add "johnson")
(declare-function johnson-bookmark-remove "johnson")
(declare-function johnson-bookmark-list "johnson")
(declare-function johnson-refresh "johnson")
(declare-function johnson-copy-entry "johnson")
(declare-function johnson-copy-dictionary-name "johnson")
(declare-function johnson-play-audio-at-point "johnson")
(declare-function johnson-toggle-section "johnson")
(declare-function johnson-toggle-all-sections "johnson")
(declare-function johnson-expand-all "johnson")
(declare-function johnson-collapse-all "johnson")
(declare-function johnson-list-dictionaries "johnson")
(declare-function johnson-reorder-dictionaries "johnson")
(declare-function johnson-import-goldendict-order "johnson")
(declare-function johnson-index "johnson")
(declare-function johnson-stop-indexing "johnson")
(declare-function johnson-clear-index "johnson")
(declare-function johnson-clear-resource-cache "johnson")
(declare-function johnson-close-caches "johnson")
(declare-function johnson-browse-dictionary "johnson")
(declare-function johnson-eldoc-mode "johnson")
(declare-function johnson-scan-mode "johnson")

;;;; Boolean toggle infix class

(defclass johnson-transient-bool (transient-lisp-variable)
  ((always-read :initform t))
  "A boolean toggle infix for a Lisp variable.")

(cl-defmethod transient-infix-read ((obj johnson-transient-bool))
  "Toggle the boolean value of OBJ."
  (not (oref obj value)))

(cl-defmethod transient-format-value ((obj johnson-transient-bool))
  "Format OBJ value as on/off."
  (propertize (if (oref obj value) "on" "off")
              'face (if (oref obj value)
                        'transient-value
                      'transient-inactive-value)))

;;;; Cycle infix class

(defclass johnson-transient-cycle (transient-lisp-variable)
  ((choices :initarg :choices :initform nil)
   (always-read :initform t))
  "A cycling infix that rotates through a list of choices.")

(cl-defmethod transient-infix-read ((obj johnson-transient-cycle))
  "Cycle to the next choice for OBJ."
  (let* ((choices (oref obj choices))
         (current (oref obj value))
         (idx (or (cl-position current choices) -1)))
    (nth (mod (1+ idx) (length choices)) choices)))

(cl-defmethod transient-format-value ((obj johnson-transient-cycle))
  "Format OBJ value as the symbol name."
  (propertize (symbol-name (oref obj value))
              'face 'transient-value))

;;;; Custom set-value functions

(defun johnson-transient--set-images (_var val)
  "Set `johnson-display-images' to VAL and refresh."
  (setq johnson-display-images val)
  (when (and (bound-and-true-p johnson--current-word)
             (derived-mode-p 'johnson-mode))
    (johnson-refresh)))

(defun johnson-transient--set-eldoc (_var val)
  "Enable or disable `johnson-eldoc-mode' based on VAL."
  (johnson-eldoc-mode (if val 1 -1)))

(defun johnson-transient--set-scan (_var val)
  "Enable or disable `johnson-scan-mode' based on VAL."
  (johnson-scan-mode (if val 1 -1)))

(defun johnson-transient--set-source-lang (_var val)
  "Set source language to VAL and refresh the results buffer."
  (setq johnson--current-source-lang val)
  (when (and (bound-and-true-p johnson--current-word)
             (derived-mode-p 'johnson-mode))
    (johnson-refresh)))

(defun johnson-transient--set-target-lang (_var val)
  "Set target language to VAL and refresh the results buffer."
  (setq johnson--current-target-lang val)
  (when (and (bound-and-true-p johnson--current-word)
             (derived-mode-p 'johnson-mode))
    (johnson-refresh)))

;;;; Language infix class

(defclass johnson-transient-lang (transient-lisp-variable)
  ((always-read :initform t)
   (lang-type :initarg :lang-type :initform 'source))
  "An infix for selecting a source or target language.")

(cl-defmethod transient-infix-read ((obj johnson-transient-lang))
  "Prompt for a language with `completing-read'.
OBJ determines whether source or target languages are offered."
  (let* ((type (oref obj lang-type))
         (langs (if (eq type 'source)
                    (johnson--available-source-langs)
                  (johnson--available-target-langs
                   johnson--current-source-lang)))
         (choices (cons "<all>" langs))
         (selection (completing-read
                     (format "%s language: "
                             (capitalize (symbol-name type)))
                     choices nil t)))
    (if (equal selection "<all>") nil selection)))

(cl-defmethod transient-format-value ((obj johnson-transient-lang))
  "Format OBJ value as the language name or <all>."
  (let ((val (oref obj value)))
    (propertize (or val "<all>")
                'face (if val
                          'transient-value
                        'transient-inactive-value))))

;;;; Option infixes

(transient-define-infix johnson-transient:images ()
  :class 'johnson-transient-bool
  :variable 'johnson-display-images
  :set-value #'johnson-transient--set-images
  :description "Images")

(transient-define-infix johnson-transient:fts ()
  :class 'johnson-transient-bool
  :variable 'johnson-fts-enabled
  :description "Full-text search")

(transient-define-infix johnson-transient:eldoc ()
  :class 'johnson-transient-bool
  :variable 'johnson-eldoc-mode
  :set-value #'johnson-transient--set-eldoc
  :description "Eldoc mode")

(transient-define-infix johnson-transient:scan ()
  :class 'johnson-transient-bool
  :variable 'johnson-scan-mode
  :set-value #'johnson-transient--set-scan
  :description "Scan mode")

(transient-define-infix johnson-transient:persist-history ()
  :class 'johnson-transient-bool
  :variable 'johnson-history-persist
  :description "Persist history")

(transient-define-infix johnson-transient:source-lang ()
  :class 'johnson-transient-lang
  :variable 'johnson--current-source-lang
  :lang-type 'source
  :set-value #'johnson-transient--set-source-lang
  :description "Source language")

(transient-define-infix johnson-transient:target-lang ()
  :class 'johnson-transient-lang
  :variable 'johnson--current-target-lang
  :lang-type 'target
  :set-value #'johnson-transient--set-target-lang
  :description "Target language")

(transient-define-infix johnson-transient:ref-scope ()
  :class 'johnson-transient-cycle
  :variable 'johnson-ref-scope
  :choices '(all same)
  :description "Ref scope")

;;;; Directory-list infix class

(defclass johnson-transient-dirs (transient-lisp-variable)
  ((always-read :initform t))
  "An infix for editing a list of directories.")

(cl-defmethod transient-infix-read ((_obj johnson-transient-dirs))
  "Prompt for directories one at a time until the user enters an empty string."
  (let ((dirs '())
        dir)
    (while (progn
             (setq dir (read-directory-name
                        (format "Dictionary directory (%d so far, empty to finish): "
                                (length dirs))
                        nil nil nil))
             (not (string-empty-p dir)))
      (push (file-name-as-directory dir) dirs))
    (nreverse dirs)))

(cl-defmethod transient-format-value ((obj johnson-transient-dirs))
  "Format OBJ value as abbreviated directory paths."
  (let ((val (oref obj value)))
    (propertize (if val
                    (mapconcat #'abbreviate-file-name val ", ")
                  "(none)")
                'face (if val
                          'transient-value
                        'transient-inactive-value))))

(transient-define-infix johnson-transient:dict-dirs ()
  :class 'johnson-transient-dirs
  :variable 'johnson-dictionary-directories
  :description "Dictionary directories")

;;;; Main menu

;;;###autoload
(transient-define-prefix johnson-menu ()
  "Main transient menu for johnson."
  :info-manual "(johnson)"
  [["Search"
    ("/" "Look up word" johnson-lookup)
    ("S" "Full-text search" johnson-search)
    ("-s" johnson-transient:source-lang)
    ("-t" johnson-transient:target-lang)
    ("-f" johnson-transient:fts)]
   ["Bookmarks"
    ("b" "Add bookmark" johnson-bookmark-add)
    ("M" "Remove bookmark" johnson-bookmark-remove)
    ("B" "Bookmark list" johnson-bookmark-list)
    ("-p" johnson-transient:persist-history)]
   ["Dictionaries"
    ("d" "List dictionaries" johnson-list-dictionaries)
    ("r" "Reorder" johnson-reorder-dictionaries)
    ("I" "Import GoldenDict order" johnson-import-goldendict-order)
    ("D" "Browse directory" johnson-browse-dictionary)
    ("-d" johnson-transient:dict-dirs)]]
  [["Navigate"
    ("n" "Next section" johnson-next-section)
    ("p" "Previous section" johnson-prev-section)
    ("j" "Jump to section" johnson-jump-to-section)
    ("o" "Ace link" johnson-ace-link)
    ("RET" "Follow reference" johnson-follow-ref)]
   ["Copy & media"
    ("w" "Copy entry" johnson-copy-entry)
    ("W" "Copy dictionary name" johnson-copy-dictionary-name)
    ("a" "Play audio" johnson-play-audio-at-point)
    ("g" "Refresh" johnson-refresh)
    ("-i" johnson-transient:images)]
   ["Index"
    ("i" "Index/re-index" johnson-index)
    ("k" "Stop indexing" johnson-stop-indexing)
    ("X" "Clear index" johnson-clear-index)
    ("R" "Clear resource cache" johnson-clear-resource-cache)
    ("Q" "Close caches" johnson-close-caches)]]
  [["History"
    ("l" "Back" johnson-history-back)
    ("r" "Forward" johnson-history-forward)
    ("H" "History list" johnson-history-list)
    ("C" "Clear history" johnson-history-clear)]
   ["Sections"
    ("TAB" "Toggle section" johnson-toggle-section)
    ("<backtab>" "Toggle all" johnson-toggle-all-sections)
    ("e" "Expand all" johnson-expand-all)
    ("c" "Collapse all" johnson-collapse-all)]
   ["Modes"
    ("-e" johnson-transient:eldoc)
    ("-n" johnson-transient:scan)
    ("-r" johnson-transient:ref-scope)]])

(provide 'johnson-transient)
;;; johnson-transient.el ends here
