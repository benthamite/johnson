;;; johnson-transient.el --- Transient menu for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

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
(defvar johnson-display-images)
(defvar johnson-fts-enabled)
(defvar johnson-history-persist)
(defvar johnson-default-search-scope)
(defvar johnson-ref-scope)
(defvar johnson--current-word)

;; Commands used as suffixes (defined in johnson.el)
(declare-function johnson-lookup "johnson")
(declare-function johnson-search "johnson")
(declare-function johnson-select-group "johnson")
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

(transient-define-infix johnson-transient:search-scope ()
  :class 'johnson-transient-cycle
  :variable 'johnson-default-search-scope
  :choices '(all group)
  :description "Search scope")

(transient-define-infix johnson-transient:ref-scope ()
  :class 'johnson-transient-cycle
  :variable 'johnson-ref-scope
  :choices '(all same)
  :description "Ref scope")

;;;; Main menu

;;;###autoload
(transient-define-prefix johnson-menu ()
  "Main transient menu for johnson."
  :info-manual "(johnson)"
  [["Lookup"
    ("l" "Look up word" johnson-lookup)
    ("s" "Full-text search" johnson-search)
    ("G" "Select group" johnson-select-group)]
   ["Navigate"
    ("n" "Next section" johnson-next-section)
    ("p" "Previous section" johnson-prev-section)
    ("o" "Ace link" johnson-ace-link)
    ("RET" "Follow reference" johnson-follow-ref)]
   ["History"
    ("<" "Back" johnson-history-back)
    (">" "Forward" johnson-history-forward)
    ("H" "History list" johnson-history-list)
    ("C" "Clear history" johnson-history-clear)]]
  [["Bookmarks"
    ("m" "Add bookmark" johnson-bookmark-add)
    ("M" "Remove bookmark" johnson-bookmark-remove)
    ("B" "Bookmark list" johnson-bookmark-list)]
   ["Actions"
    ("g" "Refresh" johnson-refresh)
    ("w" "Copy entry" johnson-copy-entry)
    ("W" "Copy dictionary name" johnson-copy-dictionary-name)
    ("a" "Play audio" johnson-play-audio-at-point)
    ("D" "Browse dictionary dir" johnson-browse-dictionary)]
   ["Sections"
    ("TAB" "Toggle section" johnson-toggle-section)
    ("<backtab>" "Toggle all sections" johnson-toggle-all-sections)
    ("e" "Expand all" johnson-expand-all)
    ("c" "Collapse all" johnson-collapse-all)]]
  [["Manage"
    ("d" "List dictionaries" johnson-list-dictionaries)
    ("r" "Reorder dictionaries" johnson-reorder-dictionaries)
    ("i" "Index/re-index" johnson-index)
    ("k" "Stop indexing" johnson-stop-indexing)
    ("X" "Clear index" johnson-clear-index)
    ("R" "Clear resource cache" johnson-clear-resource-cache)
    ("Q" "Close caches" johnson-close-caches)]
   ["Options"
    ("-i" johnson-transient:images)
    ("-f" johnson-transient:fts)
    ("-e" johnson-transient:eldoc)
    ("-s" johnson-transient:scan)
    ("-p" johnson-transient:persist-history)
    ("-c" johnson-transient:search-scope)
    ("-r" johnson-transient:ref-scope)]])

(provide 'johnson-transient)
;;; johnson-transient.el ends here
