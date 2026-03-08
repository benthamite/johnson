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

;; Variables (defined in johnson.el)
(defvar johnson-display-images)
(defvar johnson-fts-enabled)
(defvar johnson-history-persist)
(defvar johnson-default-search-scope)
(defvar johnson-ref-scope)

;; Commands used as suffixes (defined in johnson.el)
(declare-function johnson-toggle-images "johnson")
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
(declare-function johnson-new-search "johnson")
(declare-function johnson-eldoc-mode "johnson")
(declare-function johnson-scan-mode "johnson")

;;;; Suffixes for toggleable options

(transient-define-suffix johnson-transient--toggle-images ()
  "Toggle inline image display."
  :description (lambda ()
                 (format "Images: %s" (if johnson-display-images "on" "off")))
  :transient t
  (interactive)
  (johnson-toggle-images))

(transient-define-suffix johnson-transient--toggle-fts ()
  "Toggle full-text search indexing."
  :description (lambda ()
                 (format "Full-text search: %s"
                         (if johnson-fts-enabled "on" "off")))
  :transient t
  (interactive)
  (setq johnson-fts-enabled (not johnson-fts-enabled))
  (message "Full-text search indexing %s (re-index to take effect)"
           (if johnson-fts-enabled "enabled" "disabled")))

(transient-define-suffix johnson-transient--toggle-eldoc ()
  "Toggle eldoc integration."
  :description (lambda ()
                 (format "Eldoc mode: %s"
                         (if (bound-and-true-p johnson-eldoc-mode) "on" "off")))
  :transient t
  (interactive)
  (johnson-eldoc-mode (if (bound-and-true-p johnson-eldoc-mode) -1 1)))

(transient-define-suffix johnson-transient--toggle-scan ()
  "Toggle scan-popup mode."
  :description (lambda ()
                 (format "Scan mode: %s"
                         (if (bound-and-true-p johnson-scan-mode) "on" "off")))
  :transient t
  (interactive)
  (johnson-scan-mode (if (bound-and-true-p johnson-scan-mode) -1 1)))

(transient-define-suffix johnson-transient--toggle-history-persist ()
  "Toggle history persistence."
  :description (lambda ()
                 (format "Persist history: %s"
                         (if johnson-history-persist "on" "off")))
  :transient t
  (interactive)
  (setq johnson-history-persist (not johnson-history-persist))
  (message "History persistence %s"
           (if johnson-history-persist "enabled" "disabled")))

(transient-define-suffix johnson-transient--cycle-search-scope ()
  "Cycle default search scope between `all' and `group'."
  :description (lambda ()
                 (format "Search scope: %s"
                         (if (eq johnson-default-search-scope 'all)
                             "all" "group")))
  :transient t
  (interactive)
  (setq johnson-default-search-scope
        (if (eq johnson-default-search-scope 'all) 'group 'all))
  (message "Default search scope: %s" johnson-default-search-scope))

(transient-define-suffix johnson-transient--cycle-ref-scope ()
  "Cycle cross-reference scope between `all' and `same'."
  :description (lambda ()
                 (format "Ref scope: %s"
                         (if (eq johnson-ref-scope 'all) "all" "same")))
  :transient t
  (interactive)
  (setq johnson-ref-scope
        (if (eq johnson-ref-scope 'all) 'same 'all))
  (message "Cross-reference scope: %s" johnson-ref-scope))

;;;; Main menu

;;;###autoload
(transient-define-prefix johnson-menu ()
  "Main transient menu for johnson."
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
    ("a" "Play audio" johnson-play-audio-at-point)]
   ["Sections"
    ("TAB" "Toggle section" johnson-toggle-section)
    ("<backtab>" "Toggle all sections" johnson-toggle-all-sections)
    ("+" "Expand all" johnson-expand-all)
    ("-" "Collapse all" johnson-collapse-all)]]
  [["Manage"
    ("d" "List dictionaries" johnson-list-dictionaries)
    ("r" "Reorder dictionaries" johnson-reorder-dictionaries)
    ("i" "Index/re-index" johnson-index)
    ("k" "Stop indexing" johnson-stop-indexing)
    ("X" "Clear index" johnson-clear-index)
    ("R" "Clear resource cache" johnson-clear-resource-cache)
    ("Q" "Close caches" johnson-close-caches)]
   ["Options"
    ("-i" johnson-transient--toggle-images)
    ("-f" johnson-transient--toggle-fts)
    ("-e" johnson-transient--toggle-eldoc)
    ("-s" johnson-transient--toggle-scan)
    ("-p" johnson-transient--toggle-history-persist)
    ("-c" johnson-transient--cycle-search-scope)
    ("-r" johnson-transient--cycle-ref-scope)]])

(provide 'johnson-transient)
;;; johnson-transient.el ends here
