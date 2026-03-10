;;; johnson.el --- Multi-format dictionary UI for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: dictionaries, i18n

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

;; `johnson' is a multi-format dictionary UI for Emacs, providing the
;; functionality of programs such as GoldenDict and StarDict.  It
;; implements native Elisp parsing for all supported formats — no
;; external tools required.
;;
;; Supported formats (v0.4): DSL (ABBYY Lingvo), StarDict, MDict, BGL (Babylon), DICT protocol.
;;
;; Usage:
;;   M-x johnson-lookup            Look up a word
;;   M-x johnson-index             Index/re-index all dictionaries
;;   M-x johnson-list-dictionaries Show discovered dictionaries
;;   M-x johnson-select-group      Switch dictionary group

;;; Code:

(require 'johnson-db)
(require 'cl-lib)

(autoload 'johnson-menu "johnson-transient" "Main transient menu for johnson." t)

;;;; User options

(defcustom johnson-dictionary-directories '("~/.local/share/dictionaries/")
  "Directories to scan recursively for dictionary files."
  :type '(repeat directory)
  :group 'johnson)

(defcustom johnson-default-search-scope 'all
  "Default search scope for lookups.
`all' searches all dictionaries; `group' searches only the active group."
  :type '(choice (const :tag "All dictionaries" all)
                 (const :tag "Active group" group))
  :group 'johnson)

(defcustom johnson-ref-scope 'all
  "Scope for following cross-reference links.
`all' searches all dictionaries (default).  `same' restricts the
lookup to the dictionary containing the link."
  :type '(choice (const :tag "All dictionaries" all)
                 (const :tag "Same dictionary" same))
  :group 'johnson)

(defcustom johnson-dictionary-groups nil
  "User-defined dictionary group overrides.
Alist of (GROUP-NAME . (LIST-OF-DICTIONARY-NAMES)).
When nil, groups are auto-detected from dictionary metadata."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'johnson)

(defcustom johnson-dictionary-priorities nil
  "Alist of (DICTIONARY-NAME . PRIORITY) for display ordering.
Lower numbers display first.  Default priority is 0."
  :type '(alist :key-type string :value-type integer)
  :group 'johnson)

(defcustom johnson-completion-min-chars 3
  "Minimum input length before completion candidates are fetched.
Shorter inputs return no candidates, avoiding expensive prefix
queries against the full headword index."
  :type 'natnum
  :group 'johnson)

(defcustom johnson-render-batch-size 5
  "Number of dictionary results to render per batch.
The first batch is rendered synchronously when results are
displayed; remaining results are rendered in the background
during idle time."
  :type '(integer 1 100)
  :group 'johnson)

(defcustom johnson-history-max 100
  "Maximum number of entries in lookup history."
  :type 'integer
  :group 'johnson)

(defcustom johnson-audio-player 'auto
  "Audio player for pronunciation playback.
If `auto', try Emacs built-in `play-sound-file' first, then fall
back to the first available external player (see
`johnson-audio-external-players').
If a string, use it as an external command (e.g., \"afplay\", \"mpv\")."
  :type '(choice (const :tag "Auto-detect" auto)
                 (string :tag "External command"))
  :group 'johnson)

(defcustom johnson-audio-external-players '("afplay" "mpv" "paplay" "aplay")
  "External audio players to try when `johnson-audio-player' is `auto'.
Each entry is a command name.  The first one found on the system is used."
  :type '(repeat string)
  :group 'johnson)

(defcustom johnson-display-images t
  "Whether to display inline images in dictionary entries.
When nil, images are replaced with `[image: FILENAME]' placeholders."
  :type 'boolean
  :group 'johnson)

(defcustom johnson-wildcard-max-results 200
  "Maximum number of results returned by wildcard search."
  :type 'integer
  :group 'johnson)

(defcustom johnson-fts-enabled nil
  "Whether full-text search indexing is enabled.
When non-nil, definitions are indexed for full-text search during
the indexing process.  This significantly increases index size and
indexing time."
  :type 'boolean
  :group 'johnson)

(defcustom johnson-eldoc-max-length 80
  "Maximum length of eldoc definition strings."
  :type 'integer
  :group 'johnson)

(defcustom johnson-scan-trigger 'selection
  "What triggers scan-popup lookups.
`selection' triggers on text selection, `idle' triggers after idle
delay, `both' triggers on either."
  :type '(choice (const :tag "Text selection" selection)
                 (const :tag "Idle timer" idle)
                 (const :tag "Both" both))
  :group 'johnson)

(defcustom johnson-scan-idle-delay 1.0
  "Idle delay in seconds before scan-popup looks up word at point."
  :type 'float
  :group 'johnson)

(defcustom johnson-scan-popup-duration 5.0
  "Duration in seconds to display the scan popup."
  :type 'float
  :group 'johnson)

(defcustom johnson-bookmarks-file
  (expand-file-name "bookmarks.el" "~/.cache/johnson/")
  "File where bookmarks are persisted."
  :type 'file
  :group 'johnson)

(defcustom johnson-history-file
  (expand-file-name "history.el" "~/.cache/johnson/")
  "File where the timestamped history log is persisted."
  :type 'file
  :group 'johnson)

(defcustom johnson-history-persist t
  "Whether to persist the timestamped history log to disk."
  :type 'boolean
  :group 'johnson)

;;;; Faces

(defface johnson-section-header-face
  '((t :inherit bold :extend t))
  "Face for dictionary section headers in the results buffer."
  :group 'johnson)

(defface johnson-toc-face
  '((t :inherit link))
  "Face for table-of-contents entries in the results buffer."
  :group 'johnson)

(defface johnson-audio-button-face
  '((t :inherit link))
  "Face for audio playback buttons in dictionary entries."
  :group 'johnson)

;;;; Internal variables

(defvar johnson--formats nil
  "List of registered format backend plists.")

(defvar johnson--dictionaries nil
  "List of discovered dictionary plists.
Each element has keys :path, :format-name, :name,
:source-lang, :target-lang, :group, :priority.")

(defvar johnson--current-group nil
  "Currently active dictionary group name, or nil for all.")

(defvar johnson-history nil
  "History of looked-up words for `completing-read'.")

(defvar johnson--indexed-p nil
  "Non-nil after initial index staleness check.")

(defvar johnson--indexing-in-progress nil
  "Non-nil when asynchronous indexing is running.")

(defvar johnson--indexing-process nil
  "The child Emacs process used for async indexing.")

(defvar johnson--indexing-callbacks nil
  "List of functions to call when async indexing completes.")

(defvar johnson--navigating-history nil
  "Non-nil when navigating history (suppresses nav-push).")

(defvar johnson--db-cache (make-hash-table :test #'equal)
  "Hash table mapping dictionary file paths to open sqlite connections.")

(defvar-local johnson--current-word nil
  "The currently displayed lookup word.")

(defvar-local johnson--nav-history nil
  "Navigation history list of looked-up words.")

(defvar-local johnson--nav-position -1
  "Current position in navigation history.")

(defvar-local johnson--pending-results nil
  "List of (DICT . MATCHES) results awaiting deferred rendering.")

(defvar-local johnson--render-timer nil
  "Timer for deferred rendering of remaining results.")

(defvar-local johnson--render-marker nil
  "Marker at the insertion point for the next deferred batch.")

(defvar-local johnson--temp-audio-files nil
  "List of temporary audio file paths to delete when the buffer is killed.")

(defvar johnson--bookmarks nil
  "In-memory list of bookmarked entries.
Each element is a plist (:headword STRING :dictionary STRING
:timestamp FLOAT :path STRING).")

(defvar johnson--bookmarks-loaded nil
  "Non-nil when bookmarks have been loaded from disk.")

(defvar johnson--history-log nil
  "Timestamped history of lookups.
Each element is a plist (:word STRING :timestamp FLOAT :dict-count INTEGER).")

(defvar johnson--history-log-loaded nil
  "Non-nil when history log has been loaded from disk.")

(defvar johnson--eldoc-cache (make-hash-table :test #'equal)
  "LRU cache for eldoc definitions.  Maps word to plain-text result.")

(defvar johnson--scan-idle-timer nil
  "Idle timer for `johnson-scan-mode'.")

(defvar johnson--scan-last-word nil
  "Last word looked up by `johnson-scan-mode' (debounce).")

(defvar johnson--scan-hide-timer nil
  "Timer to hide the scan popup.")

;;;; Format registry

;;;###autoload
(defun johnson-register-format (&rest props)
  "Register a dictionary format backend.
PROPS is a plist with keys :name, :extensions, :detect,
:parse-metadata, :build-index, :retrieve-entry, :render-entry."
  (let ((name (plist-get props :name)))
    (setq johnson--formats
          (cl-remove-if (lambda (fmt) (equal (plist-get fmt :name) name))
                        johnson--formats))
    (push props johnson--formats)))

(defun johnson--get-format (name)
  "Return the format plist for NAME, or nil."
  (cl-find-if (lambda (fmt) (equal (plist-get fmt :name) name))
              johnson--formats))

(defun johnson--detect-format (path)
  "Return the format plist for file at PATH, or nil."
  (cl-find-if (lambda (fmt)
                (condition-case err
                    (funcall (plist-get fmt :detect) path)
                  (error
                   (message "johnson: format detection error for %s: %s"
                            path (error-message-string err))
                   nil)))
              johnson--formats))

(defun johnson--file-extensions ()
  "Return a list of all registered file extensions."
  (cl-mapcan (lambda (fmt)
               (copy-sequence (plist-get fmt :extensions)))
             johnson--formats))

;;;; Database access

(defun johnson--get-db (dict-path)
  "Return an open sqlite connection for DICT-PATH, opening if needed."
  (or (gethash dict-path johnson--db-cache)
      (let ((db (johnson-db-open dict-path)))
        (puthash dict-path db johnson--db-cache)
        db)))

(defun johnson--close-all-dbs ()
  "Close all cached database connections."
  (maphash (lambda (_path db)
             (condition-case nil (johnson-db-close db) (error nil)))
           johnson--db-cache)
  (clrhash johnson--db-cache)
  (johnson-db-close-completion-db))

;;;; Dictionary discovery

(defun johnson--discover ()
  "Scan `johnson-dictionary-directories' and populate `johnson--dictionaries'."
  (unless johnson--formats
    (user-error "No dictionary formats registered; load a format backend (e.g., johnson-dsl)"))
  (let ((dicts nil)
        (ext-re (concat "\\." (regexp-opt (johnson--file-extensions)) "\\'")))
    (dolist (dir johnson-dictionary-directories)
      (let ((expanded (expand-file-name dir)))
        (when (file-directory-p expanded)
          (dolist (file (directory-files-recursively expanded ext-re))
            (unless (string-match-p "_abrv\\." file)
              (when-let* ((fmt (johnson--detect-format file)))
                (condition-case err
                    (let* ((metadata (funcall (plist-get fmt :parse-metadata) file))
                           (raw-name (plist-get metadata :name))
                           (name (if (and raw-name (not (string-empty-p raw-name)))
                                     raw-name
                                   (file-name-base file)))
                           (src (or (plist-get metadata :source-lang) ""))
                           (tgt (or (plist-get metadata :target-lang) ""))
                           (group (if (and (not (string-empty-p src))
                                           (not (string-empty-p tgt)))
                                      (format "%s → %s" src tgt)
                                    "Unknown"))
                           (priority (or (cdr (assoc name johnson-dictionary-priorities)) 0)))
                      (push (list :path file
                                  :format-name (plist-get fmt :name)
                                  :name name
                                  :source-lang src
                                  :target-lang tgt
                                  :group group
                                  :priority priority)
                            dicts))
                  (error
                   (message "johnson: error reading %s: %s"
                            (file-name-nondirectory file)
                            (error-message-string err))))))))))
    (setq johnson--dictionaries (nreverse dicts))
    ;; Append network-based dictionaries (e.g., DICT protocol).
    (dolist (fmt johnson--formats)
      (when-let* ((discover-fn (plist-get fmt :discover)))
        (condition-case err
            (let ((net-dicts (funcall discover-fn)))
              (setq johnson--dictionaries
                    (append johnson--dictionaries net-dicts)))
          (error
           (message "johnson: discovery error for %s: %s"
                    (plist-get fmt :name)
                    (error-message-string err))))))))

(defun johnson--ensure-dictionaries ()
  "Ensure dictionaries have been discovered."
  (unless johnson--dictionaries
    (johnson--discover)))

;;;; Groups

(defun johnson--available-groups ()
  "Return a list of available group names."
  (johnson--ensure-dictionaries)
  (if johnson-dictionary-groups
      (mapcar #'car johnson-dictionary-groups)
    (delete-dups
     (mapcar (lambda (d) (plist-get d :group))
             johnson--dictionaries))))

(defun johnson--dictionaries-in-scope ()
  "Return dictionaries matching the current search scope."
  (johnson--ensure-dictionaries)
  (if (or (eq johnson-default-search-scope 'all)
          (null johnson--current-group))
      johnson--dictionaries
    (cl-remove-if-not
     (lambda (d) (equal (plist-get d :group) johnson--current-group))
     johnson--dictionaries)))

;;;###autoload
(defun johnson-select-group ()
  "Select the active dictionary group."
  (interactive)
  (let* ((groups (johnson--available-groups))
         (choices (cons "All" groups))
         (selection (completing-read "Dictionary group: " choices nil t)))
    (if (equal selection "All")
        (progn
          (setq johnson--current-group nil)
          (setq johnson-default-search-scope 'all))
      (setq johnson--current-group selection)
      (setq johnson-default-search-scope 'group))
    (message "johnson: scope set to %s" (or johnson--current-group "All"))
    (when johnson--current-word
      (johnson-refresh))))

;;;; Indexing

(defun johnson--format-number (n)
  "Format integer N with comma thousand separators."
  (let* ((s (number-to-string n))
         (rev (nreverse (string-to-list s)))
         (parts nil)
         (i 0))
    (dolist (c rev)
      (when (and (> i 0) (zerop (% i 3)))
        (push ?, parts))
      (push c parts)
      (cl-incf i))
    (apply #'string parts)))

(defun johnson--index-one-dict-sync (dict buf)
  "Index a single dictionary DICT synchronously, logging to BUF.
Returns non-nil on success.  Used for batch mode and single-dict reindex."
  (let* ((path (plist-get dict :path))
         (name (plist-get dict :name))
         (format-name (plist-get dict :format-name))
         (fmt (johnson--get-format format-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "  %s..." name))
          (redisplay))))
    (condition-case err
        (if (not (johnson-db-stale-p path))
            (let* ((db (johnson--get-db path))
                   (count (johnson-db-entry-count db)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (format " up to date (%s entries)\n"
                                    (johnson--format-number count))))))
              t)
          (let ((db (johnson--get-db path))
                (entries nil)
                (count 0))
            (johnson-db-reset db)
            (johnson-db-set-metadata db "name" name)
            (johnson-db-set-metadata db "format" format-name)
            (johnson-db-set-metadata db "source-lang" (plist-get dict :source-lang))
            (johnson-db-set-metadata db "target-lang" (plist-get dict :target-lang))
            (johnson-db-set-metadata db "source-path" path)
            (funcall (plist-get fmt :build-index) path
                     (lambda (headword offset length)
                       (push (list headword offset length) entries)
                       (cl-incf count)))
            (johnson-db-insert-entries-batch db (nreverse entries))
            (johnson-db-set-metadata db "entry-count" (number-to-string count))
            (johnson-db-set-metadata db "mtime"
                                     (format-time-string
                                      "%s"
                                      (file-attribute-modification-time
                                       (file-attributes path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (goto-char (point-max))
                  (insert (format " done (%s entries)\n"
                                  (johnson--format-number count))))))
            t))
      ((error quit)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format " ERROR: %s\n" (error-message-string err))))))
       nil))))

(defun johnson--index-subprocess-script ()
  "Return the Elisp form for the child indexing process."
  (let ((dirs (mapcar #'expand-file-name johnson-dictionary-directories)))
    (format
     "(progn
        (require 'johnson)
        (setq johnson-dictionary-directories '%S)
        (setq johnson-cache-directory %S)
        (setq johnson-fts-enabled %S)
        (johnson--discover)
        (let ((total (length johnson--dictionaries))
              (done 0)
              (errors 0))
          (message \"JOHNSON-INDEX-START %%d\" total)
          (dolist (dict johnson--dictionaries)
            (let* ((path (plist-get dict :path))
                   (name (plist-get dict :name))
                   (format-name (plist-get dict :format-name))
                   (fmt (johnson--get-format format-name)))
              (cl-incf done)
              (condition-case err
                  (if (not (johnson-db-stale-p path))
                      (let* ((db (johnson--get-db path))
                             (count (johnson-db-entry-count db)))
                        (when (and johnson-fts-enabled
                                   (not (johnson-db-fts-indexed-p db))
                                   (plist-get fmt :retrieve-entry))
                          (message \"JOHNSON-INDEX-FTS %%s\" name)
                          (condition-case fts-err
                              (let ((retrieve-fn (plist-get fmt :retrieve-entry))
                                    (rows (sqlite-select db
                                            \"SELECT headword, byte_offset, byte_length FROM entries\"))
                                    (fts-count 0))
                                (sqlite-execute db \"BEGIN TRANSACTION\")
                                (dolist (row rows)
                                  (let* ((hw (nth 0 row))
                                         (off (nth 1 row))
                                         (len (nth 2 row))
                                         (raw (funcall retrieve-fn path off len))
                                         (plain (johnson--entry-to-plain-text
                                                 raw format-name)))
                                    (when (and plain (not (string-empty-p plain)))
                                      (johnson-db-insert-fts db hw plain)
                                      (cl-incf fts-count))))
                                (sqlite-execute db \"COMMIT\")
                                (johnson-db-set-fts-indexed db)
                                (message \"JOHNSON-INDEX-FTS-DONE %%s %%d\" name fts-count))
                            (error
                             (ignore-errors (sqlite-execute db \"ROLLBACK\"))
                             (message \"JOHNSON-INDEX-FTS-ERROR %%s %%s\"
                                      name (error-message-string fts-err)))))
                        (message \"JOHNSON-INDEX-PROGRESS %%d %%d up-to-date %%d %%s\"
                                 done total count name))
                    (let ((db (johnson--get-db path))
                          (entries nil)
                          (count 0))
                      (johnson-db-reset db)
                      (johnson-db-set-metadata db \"name\" name)
                      (johnson-db-set-metadata db \"format\" format-name)
                      (johnson-db-set-metadata db \"source-lang\"
                        (plist-get dict :source-lang))
                      (johnson-db-set-metadata db \"target-lang\"
                        (plist-get dict :target-lang))
                      (johnson-db-set-metadata db \"source-path\" path)
                      (funcall (plist-get fmt :build-index) path
                        (lambda (headword offset length)
                          (push (list headword offset length) entries)
                          (cl-incf count)))
                      (setq entries (nreverse entries))
                      (johnson-db-insert-entries-batch db entries)
                      (when (and johnson-fts-enabled
                                 (not (johnson-db-fts-indexed-p db))
                                 (plist-get fmt :retrieve-entry))
                        (message \"JOHNSON-INDEX-FTS %%s\" name)
                        (condition-case fts-err
                            (let ((retrieve-fn (plist-get fmt :retrieve-entry))
                                  (fts-count 0))
                              (sqlite-execute (johnson--get-db path)
                                              \"BEGIN TRANSACTION\")
                              (dolist (entry entries)
                                (let* ((hw (nth 0 entry))
                                       (off (nth 1 entry))
                                       (len (nth 2 entry))
                                       (raw (funcall retrieve-fn path off len))
                                       (plain (johnson--entry-to-plain-text
                                               raw format-name)))
                                  (when (and plain (not (string-empty-p plain)))
                                    (johnson-db-insert-fts db hw plain)
                                    (cl-incf fts-count))))
                              (sqlite-execute (johnson--get-db path) \"COMMIT\")
                              (johnson-db-set-fts-indexed db)
                              (message \"JOHNSON-INDEX-FTS-DONE %%s %%d\" name fts-count))
                          (error
                           (ignore-errors
                             (sqlite-execute (johnson--get-db path) \"ROLLBACK\"))
                           (message \"JOHNSON-INDEX-FTS-ERROR %%s %%s\"
                                    name (error-message-string fts-err)))))
                      (johnson-db-set-metadata db \"entry-count\"
                        (number-to-string count))
                      (johnson-db-set-metadata db \"mtime\"
                        (format-time-string \"%%s\"
                          (file-attribute-modification-time
                            (file-attributes path))))
                      (message \"JOHNSON-INDEX-PROGRESS %%d %%d indexed %%d %%s\"
                               done total count name)))
                (error
                 (cl-incf errors)
                 (message \"JOHNSON-INDEX-ERROR %%d %%d %%s\\t%%s\"
                          done total name (error-message-string err))))))
          (message \"JOHNSON-INDEX-DONE %%d %%d\" total errors)
          (message \"JOHNSON-INDEX-COMPLETION building...\")
          (let* ((paths (mapcar (lambda (d) (plist-get d :path))
                                johnson--dictionaries))
                 (count (johnson-db-rebuild-completion-index paths)))
            (message \"JOHNSON-INDEX-COMPLETION done %%d\" count))
          (kill-emacs 0)))"
     dirs
     (expand-file-name johnson-cache-directory)
     johnson-fts-enabled)))

(defun johnson--index-process-filter (proc output)
  "Process filter for the child indexing process PROC.
Parses structured messages from OUTPUT and updates the progress buffer."
  (let ((buf (process-get proc 'johnson-buf)))
    ;; Prepend any buffered partial line from the previous call.
    (let ((pending (process-get proc 'johnson-pending)))
      (when pending
        (setq output (concat pending output))
        (process-put proc 'johnson-pending nil)))
    ;; If output doesn't end with a newline, save the trailing partial line.
    (unless (string-suffix-p "\n" output)
      (let ((last-nl (string-match-p "\n[^\n]*\\'" output)))
        (if last-nl
            (progn
              (process-put proc 'johnson-pending (substring output (1+ last-nl)))
              (setq output (substring output 0 (1+ last-nl))))
          (process-put proc 'johnson-pending output)
          (setq output nil))))
    (when (and output (buffer-live-p buf))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (dolist (line (split-string output "\n" t))
            (cond
             ((string-match "^JOHNSON-INDEX-START \\([0-9]+\\)" line)
              (goto-char (point-max))
              (insert (format "Indexing %s dictionaries...\n\n"
                              (match-string 1 line))))
             ((string-match
               "^JOHNSON-INDEX-PROGRESS \\([0-9]+\\) \\([0-9]+\\) \\(\\S-+\\) \\([0-9]+\\) \\(.*\\)"
               line)
              (let ((done (match-string 1 line))
                    (total (match-string 2 line))
                    (status (match-string 3 line))
                    (count (string-to-number (match-string 4 line)))
                    (name (match-string 5 line)))
                (goto-char (point-max))
                (insert (format "  [%s/%s] %s... %s (%s entries)\n"
                                done total name status
                                (johnson--format-number count)))))
             ((string-match
               "^JOHNSON-INDEX-ERROR \\([0-9]+\\) \\([0-9]+\\) \\([^\t]*\\)\t\\(.*\\)"
               line)
              (let ((done (match-string 1 line))
                    (total (match-string 2 line))
                    (name (match-string 3 line))
                    (err (match-string 4 line)))
                (goto-char (point-max))
                (insert (format "  [%s/%s] %s... ERROR: %s\n"
                                done total name err))))
             ((string-match "^JOHNSON-INDEX-DONE \\([0-9]+\\) \\([0-9]+\\)" line)
              (goto-char (point-max))
              (insert (format "\nDone. %s dictionaries processed"
                              (match-string 1 line)))
              (let ((errs (string-to-number (match-string 2 line))))
                (when (> errs 0)
                  (insert (format " (%d errors)" errs))))
              (insert ".\n"))
             ((string-match "^JOHNSON-INDEX-FTS \\(.*\\)" line)
              (goto-char (point-max))
              (insert (format "    FTS indexing %s...\n" (match-string 1 line))))
             ((string-match "^JOHNSON-INDEX-FTS-DONE \\(\\S-+\\) \\([0-9]+\\)" line)
              (goto-char (point-max))
              (insert (format "    FTS done (%s entries)\n"
                              (johnson--format-number
                               (string-to-number (match-string 2 line))))))
             ((string-match "^JOHNSON-INDEX-FTS-ERROR \\(\\S-+\\) \\(.*\\)" line)
              (goto-char (point-max))
              (insert (format "    FTS error: %s\n" (match-string 2 line))))
             ((string-match "^JOHNSON-INDEX-COMPLETION building" line)
              (goto-char (point-max))
              (insert "\nBuilding completion index..."))
             ((string-match "^JOHNSON-INDEX-COMPLETION done \\([0-9]+\\)" line)
              (goto-char (point-max))
              (insert (format " done (%s headwords)\n"
                              (johnson--format-number
                               (string-to-number
                                (match-string 1 line)))))))))))))

(defun johnson--index-process-sentinel (proc _event)
  "Process sentinel for the child indexing process PROC.
Called when the process finishes."
  (when (memq (process-status proc) '(exit signal))
    (setq johnson--indexing-in-progress nil)
    (setq johnson--indexing-process nil)
    ;; Invalidate all cached database connections so the next
    ;; query picks up the freshly-built index.
    (johnson--close-all-dbs)
    (let ((exit-code (process-exit-status proc))
          (buf (process-get proc 'johnson-buf)))
      (if (zerop exit-code)
          (progn
            (setq johnson--indexed-p t)
            (message "johnson: indexing complete"))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "\nProcess exited with code %d.\n" exit-code))))))
      (when johnson--indexing-callbacks
        (let ((cbs johnson--indexing-callbacks))
          (setq johnson--indexing-callbacks nil)
          (when (zerop exit-code)
            (mapc #'funcall cbs)))))))

;;;###autoload
(defun johnson-index (&optional callback)
  "Index or re-index all discovered dictionaries.
Progress is shown in the *johnson-indexing* buffer.
In interactive mode, indexing runs in a child Emacs process so the
current session stays responsive.  CALLBACK is called with no
arguments when indexing completes successfully.
In batch/noninteractive mode, indexing runs synchronously."
  (interactive)
  (when johnson--indexing-in-progress
    (user-error "Indexing already in progress"))
  (johnson--discover)
  (let ((buf (get-buffer-create "*johnson-indexing*"))
        (total (length johnson--dictionaries)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Indexing %d dictionaries (starting background process)...\n" total))
        (special-mode)))
    (display-buffer buf)
    (when callback (push callback johnson--indexing-callbacks))
    (if noninteractive
        ;; Synchronous for batch mode (tests).
        (progn
          (setq johnson--indexing-in-progress t)
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (insert (format "Indexing %d dictionaries...\n\n" total))))
          (dolist (dict johnson--dictionaries)
            (johnson--index-one-dict-sync dict buf))
          ;; Build the unified completion index.
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert "\nBuilding completion index...")))
          (let* ((paths (mapcar (lambda (d) (plist-get d :path))
                                johnson--dictionaries))
                 (count (johnson-db-rebuild-completion-index paths)))
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert (format " done (%s headwords)\n"
                                (johnson--format-number count))))))
          (setq johnson--indexed-p t)
          (setq johnson--indexing-in-progress nil)
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "\nDone. %d dictionaries processed.\n" total))))
          (when johnson--indexing-callbacks
            (let ((cbs johnson--indexing-callbacks))
              (setq johnson--indexing-callbacks nil)
              (mapc #'funcall cbs))))
      ;; Async: spawn a child emacs --batch process.
      (setq johnson--indexing-in-progress t)
      (let* ((load-dir (file-name-directory (locate-library "johnson")))
             (script (johnson--index-subprocess-script))
             (proc (make-process
                    :name "johnson-indexing"
                    :buffer nil
                    :command (list
                              (expand-file-name invocation-name
                                                invocation-directory)
                              "--batch"
                              "-L" load-dir
                              "--eval" script)
                    :noquery t
                    :connection-type 'pipe
                    :filter #'johnson--index-process-filter
                    :sentinel #'johnson--index-process-sentinel)))
        (process-put proc 'johnson-buf buf)
        (setq johnson--indexing-process proc)))))

(defun johnson-stop-indexing ()
  "Stop the current asynchronous indexing run."
  (interactive)
  (when (and johnson--indexing-process
             (process-live-p johnson--indexing-process))
    (kill-process johnson--indexing-process))
  (setq johnson--indexing-in-progress nil)
  (setq johnson--indexing-process nil)
  (setq johnson--indexing-callbacks nil)
  (message "johnson: indexing stopped"))

(defun johnson--ensure-indexed ()
  "Ensure dictionaries have been indexed.
In interactive mode, prompts the user and indexes asynchronously.
Returns non-nil if dictionaries are ready for querying."
  (cond
   (johnson--indexed-p t)
   (johnson--indexing-in-progress
    (message "johnson: indexing in progress...")
    nil)
   (t
    ;; Use the fast filesystem-only check (no sqlite opens) to avoid
    ;; blocking Emacs when there are hundreds of dictionaries.
    ;; Skip formats that don't use file-based indexing (e.g., DICT protocol).
    (let ((stale (cl-remove-if-not
                  (lambda (d)
                    (let ((fmt (johnson--get-format
                                (plist-get d :format-name))))
                      (and (not (plist-get fmt :skip-index))
                           (johnson-db-stale-quick-p (plist-get d :path)))))
                  johnson--dictionaries)))
      (if (null stale)
          (progn (setq johnson--indexed-p t) t)
        (if noninteractive
            ;; Synchronous in batch mode.
            (progn (johnson-index) t)
          ;; Interactive: prompt and index async.
          (when (yes-or-no-p
                 (format "%d dictionaries need indexing.  Index now? "
                         (length stale)))
            (johnson-index
             (lambda ()
               (message "johnson: indexing complete — you can now look up words")))
            nil)))))))

;;;; Completion

(defun johnson--completion-table ()
  "Return a dynamic completion table for dictionary headwords.
Caches results for the broadest prefix queried and narrows in
memory when the user extends that prefix, avoiding redundant
database queries across all dictionaries."
  (let ((query-prefix nil)
        (last-candidates nil)
        (last-counts (make-hash-table :test #'equal))
        (last-truncated nil))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function
             . ,(lambda (candidate)
                  (let ((count (gethash candidate last-counts 0)))
                    (when (> count 1)
                      (format " (%d dicts)" count)))))
            (category . johnson-headword))
        (let ((normalized (johnson-db-normalize string)))
          ;; Re-query only when the new prefix is NOT an extension of
          ;; the already-queried prefix.  When it IS an extension, the
          ;; existing candidates are a superset of what we need and
          ;; `complete-with-action' will narrow them.  However, if the
          ;; previous query was truncated by LIMIT, the cached set may
          ;; be incomplete, so we must re-query.
          (when (and (not (equal normalized query-prefix))
                     (not (and query-prefix
                               last-candidates
                               (not last-truncated)
                               (string-prefix-p query-prefix normalized))))
            (setq query-prefix normalized)
            (let ((candidates nil)
                  (comp-db (johnson-db-get-completion-db))
                  (query-limit 200))
              (clrhash last-counts)
              (when (>= (length string) johnson-completion-min-chars)
                (if comp-db
                    ;; Unified completion index: single query.
                    (condition-case err
                        (dolist (row (johnson-db-query-completion
                                     comp-db string query-limit))
                          (let ((hw (car row))
                                (cnt (cadr row)))
                            (push hw candidates)
                            (puthash hw cnt last-counts)))
                      (error
                       (message "johnson: completion query error: %s"
                                (error-message-string err))))
                  ;; Fallback: per-dictionary queries.
                  (dolist (dict (johnson--dictionaries-in-scope))
                    (condition-case err
                        (let* ((path (plist-get dict :path))
                               (db (johnson--get-db path))
                               (words (johnson-db-query-prefix
                                       db string query-limit)))
                          (dolist (w words)
                            (push w candidates)
                            (puthash w (1+ (or (gethash w last-counts) 0))
                                     last-counts)))
                      (error
                       (message "johnson: completion query error: %s"
                                (error-message-string err)))))))
              (setq last-candidates (delete-dups candidates))
              (setq last-truncated
                    (>= (length last-candidates) query-limit)))))
        (complete-with-action action last-candidates string pred)))))

;;;; Query

(defun johnson--query-all-exact (word)
  "Query all in-scope dictionaries for exact matches on WORD.
Returns a list of (DICT-PLIST . MATCHES) sorted by priority."
  (let ((results nil))
    (dolist (dict (johnson--dictionaries-in-scope))
      (condition-case err
          (let* ((path (plist-get dict :path))
                 (format-name (plist-get dict :format-name))
                 (fmt (johnson--get-format format-name))
                 (query-fn (and fmt (plist-get fmt :query-exact))))
            (if query-fn
                ;; Format-specific query (e.g., DICT protocol).
                (let ((matches (funcall query-fn path word)))
                  (when matches
                    (push (cons dict matches) results)))
              ;; Standard DB-based query.
              (let* ((db (johnson--get-db path))
                     (matches (johnson-db-query-exact db word)))
                (when matches
                  (push (cons dict matches) results)))))
        (error
         (message "johnson: query error for %s: %s"
                  (plist-get dict :name) (error-message-string err)))))
    (sort (nreverse results)
          (lambda (a b)
            (< (or (plist-get (car a) :priority) 0)
               (or (plist-get (car b) :priority) 0))))))

(defun johnson--query-dict-exact (dict-name word)
  "Query the dictionary named DICT-NAME for exact matches on WORD.
Returns a list of (DICT-PLIST . MATCHES), like `johnson--query-all-exact'
but restricted to a single dictionary."
  (let ((dict (cl-find dict-name johnson--dictionaries
                       :key (lambda (d) (plist-get d :name))
                       :test #'equal)))
    (when dict
      (condition-case nil
          (let* ((path (plist-get dict :path))
                 (format-name (plist-get dict :format-name))
                 (fmt (johnson--get-format format-name))
                 (query-fn (and fmt (plist-get fmt :query-exact))))
            (if query-fn
                (let ((matches (funcall query-fn path word)))
                  (when matches (list (cons dict matches))))
              (let* ((db (johnson--get-db path))
                     (matches (johnson-db-query-exact db word)))
                (when matches (list (cons dict matches))))))
        (error nil)))))

;;;; Lookup

;;;###autoload
(defun johnson-lookup (&optional word)
  "Look up WORD in the dictionary.
If WORD is nil, prompt with `completing-read' (defaults to word at point)."
  (interactive)
  (johnson--ensure-dictionaries)
  (unless (johnson--ensure-indexed)
    ;; Indexing started or in progress; schedule lookup after completion
    ;; if called interactively.
    (when (and (called-interactively-p 'any) johnson--indexing-in-progress)
      (push (lambda () (call-interactively #'johnson-lookup))
            johnson--indexing-callbacks))
    (user-error "Dictionaries are being indexed; lookup will resume when done"))
  (unless word
    (johnson--load-history-log)
    (let ((default (thing-at-point 'word t)))
      (setq word (completing-read
                  (format-prompt "Look up" default)
                  (johnson--completion-table)
                  nil nil nil 'johnson-history default))))
  (when (or (null word) (string-empty-p word))
    (user-error "No word given"))
  ;; Wildcard handling: if word contains ? or *, expand first.
  (when (johnson--wildcard-pattern-p word)
    (let ((matches nil))
      (dolist (dict (johnson--dictionaries-in-scope))
        (condition-case nil
            (let* ((path (plist-get dict :path))
                   (db (johnson--get-db path))
                   (hits (johnson-db-query-wildcard
                          db word johnson-wildcard-max-results)))
              (dolist (hw hits)
                (push hw matches)))
          (error nil)))
      (setq matches (delete-dups matches))
      (if (null matches)
          (user-error "No matches for wildcard pattern \"%s\"" word)
        (setq word (completing-read
                    (format "Wildcard matches (%d): " (length matches))
                    matches nil t)))))
  (unless (equal word (car johnson-history))
    (push word johnson-history)
    (when (> (length johnson-history) johnson-history-max)
      (setcdr (nthcdr (1- johnson-history-max) johnson-history) nil)))
  (let ((results (johnson--query-all-exact word)))
    (johnson--history-log-push word (length results))
    (johnson--display-results word results)))

;;;; Results buffer

(defun johnson--render-one-result (result)
  "Render a single dictionary RESULT into the current buffer.
RESULT is a cons (DICT-PLIST . MATCHES).  Inserts the section
header, rendered entries, section overlay, and trailing newline
at point.  Caller must bind `inhibit-read-only'."
  (let* ((dict (car result))
         (matches (cdr result))
         (name (plist-get dict :name))
         (path (plist-get dict :path))
         (format-name (plist-get dict :format-name))
         (fmt (johnson--get-format format-name)))
    (johnson--insert-section-header name)
    (let ((section-start (point)))
      (dolist (match matches)
        (let* ((byte-offset (nth 1 match))
               (byte-length (nth 2 match))
               (raw (funcall (plist-get fmt :retrieve-entry)
                             path byte-offset byte-length)))
          (funcall (plist-get fmt :render-entry) raw)
          (unless (bolp) (insert "\n"))))
      (let ((ov (make-overlay section-start (point))))
        (overlay-put ov 'johnson-section name)
        (overlay-put ov 'johnson-section-content t)
        (overlay-put ov 'evaporate t)))
    (insert "\n")))

(defun johnson--cancel-pending-render ()
  "Cancel any in-progress deferred rendering."
  (when johnson--render-timer
    (cancel-timer johnson--render-timer)
    (setq johnson--render-timer nil))
  (setq johnson--pending-results nil)
  (when johnson--render-marker
    (set-marker johnson--render-marker nil)
    (setq johnson--render-marker nil)))

(defun johnson--render-next-batch ()
  "Render the next batch of deferred results.
Called by a timer scheduled from `johnson--display-results'."
  (when-let* ((buf (get-buffer "*johnson*")))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'johnson--pending-results buf))
      (with-current-buffer buf
        (save-excursion
          (let* ((inhibit-read-only t)
                 (batch (seq-take johnson--pending-results
                                  johnson-render-batch-size))
                 (remaining (seq-drop johnson--pending-results
                                      johnson-render-batch-size)))
            ;; Delete the "Loading..." line
            (goto-char johnson--render-marker)
            (delete-region johnson--render-marker (point-max))
            ;; Render this batch
            (dolist (result batch)
              (condition-case err
                  (johnson--render-one-result result)
                (error
                 (insert (propertize
                          (format "[Error rendering %s: %s]\n"
                                  (plist-get (car result) :name)
                                  (error-message-string err))
                          'face 'error)))))
            ;; Update state
            (setq johnson--pending-results remaining)
            (if remaining
                (progn
                  (set-marker johnson--render-marker (point))
                  (insert (propertize
                           (format "Loading %d more results...\n"
                                   (length remaining))
                           'face 'shadow))
                  (setq johnson--render-timer
                        (run-with-timer 0 nil
                                        #'johnson--render-next-batch)))
              ;; All done — clean up
              (set-marker johnson--render-marker nil)
              (setq johnson--render-marker nil)
              (setq johnson--render-timer nil))))))))

(defun johnson--jump-to-section (name)
  "Jump to the section header for dictionary NAME."
  (let ((pos (point-min))
        (found nil))
    (while (and pos (< pos (point-max)) (not found))
      (when (and (get-text-property pos 'johnson-section-header)
                 (equal (get-text-property pos 'johnson-section-header) name)
                 (not (equal name "Contents")))
        (goto-char pos)
        (setq found t))
      (setq pos (next-single-property-change pos 'johnson-section-header)))
    (unless found
      (message "Section \"%s\" not yet loaded" name))))

(defun johnson--insert-toc (results)
  "Insert a table of contents at point for RESULTS.
RESULTS is the full list of (DICT-PLIST . MATCHES) cons cells."
  (when (> (length results) 1)
    (johnson--insert-section-header "Contents")
    (let ((toc-start (point)))
      (dolist (result results)
        (let ((name (plist-get (car result) :name)))
          (insert "  \u2022 ")
          (let ((link-start (point)))
            (insert name)
            (make-text-button link-start (point)
                              'face 'johnson-toc-face
                              'action (lambda (_btn)
                                        (johnson--jump-to-section name))
                              'help-echo (format "Jump to %s" name)))
          (insert "\n")))
      (let ((ov (make-overlay toc-start (point))))
        (overlay-put ov 'johnson-section "Contents")
        (overlay-put ov 'johnson-section-content t)
        (overlay-put ov 'evaporate t))
      (insert "\n"))))

(defun johnson--display-results (word results)
  "Display lookup RESULTS for WORD in the *johnson* buffer."
  (let ((buf (get-buffer-create "*johnson*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (nav-hist johnson--nav-history)
            (nav-pos johnson--nav-position))
        (unless (derived-mode-p 'johnson-mode)
          (johnson-mode))
        (johnson--cancel-pending-render)
        (erase-buffer)
        (setq johnson--nav-history nav-hist)
        (setq johnson--nav-position nav-pos)
        (setq johnson--current-word word)
        (unless johnson--navigating-history
          (johnson--nav-push word))
        (if (null results)
            (insert (format "No results found for \"%s\".\n" word))
          ;; Insert TOC with all result names upfront.
          (johnson--insert-toc results)
          (let ((immediate (seq-take results johnson-render-batch-size))
                (deferred (seq-drop results johnson-render-batch-size)))
            (dolist (result immediate)
              (johnson--render-one-result result))
            (when deferred
              (setq johnson--pending-results deferred)
              (setq johnson--render-marker (point-marker))
              (insert (propertize
                       (format "Loading %d more results...\n"
                               (length deferred))
                       'face 'shadow))
              (setq johnson--render-timer
                    (run-with-timer 0 nil
                                    #'johnson--render-next-batch)))))
        (setq mode-line-buffer-identification
              (format "johnson: %s" word))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun johnson--insert-section-header (name)
  "Insert a section header for dictionary NAME."
  (let* ((prefix (concat "━━ " name " "))
         (fill (max 0 (- (or fill-column 70) (length prefix))))
         (line (concat prefix (make-string fill ?━) "\n")))
    (insert (propertize line
                        'face 'johnson-section-header-face
                        'johnson-section-header name))))

;;;; johnson-mode

(defvar johnson-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'johnson-next-section)
    (define-key map "p" #'johnson-prev-section)
    (define-key map (kbd "TAB") #'johnson-toggle-section)
    (define-key map (kbd "<backtab>") #'johnson-toggle-all-sections)
    (define-key map "P" #'johnson-prev-section-header)
    (define-key map (kbd "RET") #'johnson-follow-ref)
    (define-key map "l" #'johnson-history-back)
    (define-key map "r" #'johnson-history-forward)
    (define-key map "s" #'johnson-new-search)
    (define-key map "g" #'johnson-refresh)
    (define-key map "G" #'johnson-select-group)
    (define-key map "a" #'johnson-play-audio-at-point)
    (define-key map "o" #'johnson-ace-link)
    (define-key map "w" #'johnson-copy-entry)
    (define-key map "W" #'johnson-copy-dictionary-name)
    (define-key map "I" #'johnson-toggle-images)
    (define-key map "S" #'johnson-search)
    (define-key map "b" #'johnson-bookmark-add)
    (define-key map "M" #'johnson-bookmark-remove)
    (define-key map "H" #'johnson-history-list)
    (define-key map "m" #'johnson-menu)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `johnson-mode'.")

(defun johnson--cleanup-temp-audio-files ()
  "Delete any temporary audio files created during rendering."
  (dolist (file johnson--temp-audio-files)
    (when (file-exists-p file)
      (condition-case nil (delete-file file) (error nil))))
  (setq johnson--temp-audio-files nil))

(define-derived-mode johnson-mode special-mode "Johnson"
  "Major mode for displaying dictionary lookup results.
\\{johnson-mode-map}"
  (setq truncate-lines nil)
  (setq word-wrap t)
  (add-hook 'kill-buffer-hook #'johnson--cancel-pending-render nil t)
  (add-hook 'kill-buffer-hook #'johnson--cleanup-temp-audio-files nil t))

;;;; Section navigation

(defun johnson-next-section ()
  "Move to the next dictionary section header."
  (interactive)
  (let ((pos (point)))
    ;; Move past current header if on one.
    (when (get-text-property pos 'johnson-section-header)
      (setq pos (next-single-property-change pos 'johnson-section-header)))
    (when pos
      (let ((next (next-single-property-change pos 'johnson-section-header)))
        (if next
            (goto-char next)
          (message "No more sections"))))))

(defun johnson-prev-section ()
  "Move to the previous dictionary section header."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'johnson-section-header)))
    (if pos
        (progn
          ;; Go to the start of this header region.
          (unless (get-text-property pos 'johnson-section-header)
            (setq pos (previous-single-property-change pos 'johnson-section-header)))
          (when pos (goto-char pos)))
      (message "No previous section"))))

(defun johnson-prev-section-header ()
  "Move to the previous dictionary section header."
  (interactive)
  (johnson-prev-section))

;;;; Ace-link integration

(declare-function avy-process "avy")

(defun johnson-ace-link ()
  "Jump to a visible link using avy."
  (interactive)
  (unless (require 'avy nil t)
    (user-error "The `avy' package is required for ace-link"))
  (let ((candidates nil))
    (save-excursion
      (let ((pos (window-start))
            (wend (window-end nil t)))
        (while (and pos (< pos wend))
          (when (button-at pos)
            (push (cons pos (selected-window)) candidates))
          (setq pos (next-single-property-change pos 'button nil wend)))))
    (if candidates
        (let ((pt (avy-process (nreverse candidates))))
          (when (numberp pt)
            (goto-char pt)
            (let ((btn (button-at pt)))
              (when btn (button-activate btn)))))
      (message "No links visible"))))

(with-eval-after-load 'ace-link
  (when (boundp 'ace-link-major-mode-actions)
    (push '(johnson-mode . johnson-ace-link) ace-link-major-mode-actions)))

;;;; Section collapsing

(defun johnson-toggle-section ()
  "Toggle collapse of the section at point."
  (interactive)
  (let ((name (or (get-text-property (point) 'johnson-section-header)
                  (johnson--section-name-at (point)))))
    (when name
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (and (overlay-get ov 'johnson-section-content)
                   (equal (overlay-get ov 'johnson-section) name))
          (if (overlay-get ov 'invisible)
              (progn
                (overlay-put ov 'invisible nil)
                (overlay-put ov 'after-string nil))
            (overlay-put ov 'invisible t)
            (overlay-put ov 'after-string " [+]\n")))))))

(defun johnson-collapse-all ()
  "Collapse all sections in the results buffer."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'johnson-section-content)
      (overlay-put ov 'invisible t)
      (overlay-put ov 'after-string " [+]\n"))))

(defun johnson-expand-all ()
  "Expand all sections in the results buffer."
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'johnson-section-content)
      (overlay-put ov 'invisible nil)
      (overlay-put ov 'after-string nil))))

(defun johnson-toggle-all-sections ()
  "Toggle all sections: collapse all if any is expanded, else expand all."
  (interactive)
  (let ((any-expanded nil))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'johnson-section-content)
                 (not (overlay-get ov 'invisible)))
        (setq any-expanded t)))
    (if any-expanded
        (johnson-collapse-all)
      (johnson-expand-all))))

(defun johnson--section-name-at (pos)
  "Return the section name at POS, or nil."
  (cl-some (lambda (ov) (overlay-get ov 'johnson-section))
           (overlays-at pos)))

;;;; Follow reference

(defun johnson-follow-ref ()
  "Follow the cross-reference button at point.
When `johnson-ref-scope' is `same', restrict the lookup to the
dictionary whose section contains the link."
  (interactive)
  (let ((button (button-at (point))))
    (if (not button)
        (message "No cross-reference at point")
      (let ((word (button-get button 'johnson-ref-word)))
        (if (or (null word) (eq johnson-ref-scope 'all))
            (button-activate button)
          ;; Scoped lookup: find the enclosing dictionary section.
          (let ((dict-name (johnson--section-name-at (point))))
            (if (null dict-name)
                (johnson-lookup word)
              (let ((results (johnson--query-dict-exact dict-name word)))
                (if results
                    (progn
                      (unless (equal word (car johnson-history))
                        (push word johnson-history)
                        (when (> (length johnson-history) johnson-history-max)
                          (setcdr (nthcdr (1- johnson-history-max) johnson-history) nil)))
                      (johnson--history-log-push word (length results))
                      (johnson--display-results word results))
                  ;; Fallback to full lookup if no match in same dict.
                  (johnson-lookup word))))))))))

;;;; Refresh

(defun johnson-new-search ()
  "Start a new dictionary search."
  (interactive)
  (johnson-lookup))

(defun johnson-refresh ()
  "Re-display the current word."
  (interactive)
  (when johnson--current-word
    (let ((results (johnson--query-all-exact johnson--current-word)))
      (let ((johnson--navigating-history t))
        (johnson--display-results johnson--current-word results)))))

;;;; Navigation history

(defun johnson--nav-push (word)
  "Push WORD onto the navigation history."
  (when (and johnson--nav-history
             (>= johnson--nav-position 0)
             (< johnson--nav-position (1- (length johnson--nav-history))))
    (setq johnson--nav-history
          (seq-take johnson--nav-history (1+ johnson--nav-position))))
  (unless (and johnson--nav-history
               (equal word (car (last johnson--nav-history))))
    (setq johnson--nav-history (append johnson--nav-history (list word))))
  (setq johnson--nav-position (1- (length johnson--nav-history))))

(defun johnson-history-back ()
  "Go back in navigation history."
  (interactive)
  (if (or (null johnson--nav-history)
          (<= johnson--nav-position 0))
      (message "Beginning of history")
    (cl-decf johnson--nav-position)
    (let* ((word (nth johnson--nav-position johnson--nav-history))
           (johnson--navigating-history t)
           (results (johnson--query-all-exact word)))
      (johnson--display-results word results))))

(defun johnson-history-forward ()
  "Go forward in navigation history."
  (interactive)
  (if (or (null johnson--nav-history)
          (>= johnson--nav-position (1- (length johnson--nav-history))))
      (message "End of history")
    (cl-incf johnson--nav-position)
    (let* ((word (nth johnson--nav-position johnson--nav-history))
           (johnson--navigating-history t)
           (results (johnson--query-all-exact word)))
      (johnson--display-results word results))))

;;;; Copy

(defun johnson-copy-entry ()
  "Copy the current section's entry as plain text to the kill ring."
  (interactive)
  (let ((ov (cl-find-if (lambda (o) (overlay-get o 'johnson-section-content))
                         (overlays-at (point)))))
    (if ov
        (let ((text (buffer-substring-no-properties
                     (overlay-start ov) (overlay-end ov))))
          (kill-new text)
          (message "Entry copied to kill ring"))
      (message "No section at point"))))

(defun johnson-copy-dictionary-name ()
  "Copy the dictionary name of the section at point to the kill ring."
  (interactive)
  (let ((name (get-text-property (point) 'johnson-section-header)))
    (unless name
      (let ((ov (cl-find-if (lambda (o) (overlay-get o 'johnson-section))
                            (overlays-at (point)))))
        (when ov (setq name (overlay-get ov 'johnson-section)))))
    (if name
        (progn (kill-new name)
               (message "Copied dictionary name: %s" name))
      (message "No dictionary section at point"))))

;;;; Browse dictionary directory

(defun johnson-browse-dictionary ()
  "Open Dired on the directory of the dictionary at point."
  (interactive)
  (unless (derived-mode-p 'johnson-mode)
    (user-error "Not in a johnson buffer"))
  (let ((name (or (get-text-property (point) 'johnson-section-header)
                  (johnson--section-name-at (point)))))
    (unless name
      (user-error "No dictionary section at point"))
    (let ((dict (cl-find name johnson--dictionaries
                         :key (lambda (d) (plist-get d :name))
                         :test #'equal)))
      (unless dict
        (user-error "Dictionary %s not found" name))
      (dired (file-name-directory (plist-get dict :path))))))

;;;; Dictionary list

(defvar johnson-dict-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'johnson-dict-list-reindex)
    (define-key map (kbd "RET") #'johnson-dict-list-show-details)
    map)
  "Keymap for `johnson-dict-list-mode'.")

(define-derived-mode johnson-dict-list-mode tabulated-list-mode "Johnson-Dicts"
  "Major mode for displaying a list of dictionaries."
  (setq tabulated-list-format
        [("Name" 35 t)
         ("Format" 8 t)
         ("Languages" 25 t)
         ("Entries" 10 t :right-align t)
         ("Status" 14 t)
         ("Path" 0 t)])
  (setq tabulated-list-sort-key '("Name"))
  (add-hook 'tabulated-list-revert-hook #'johnson--dict-list-entries nil t)
  (tabulated-list-init-header))

;;;###autoload
(defun johnson-list-dictionaries ()
  "Display all discovered dictionaries in a tabulated list."
  (interactive)
  (johnson--ensure-dictionaries)
  (let ((buf (get-buffer-create "*johnson-dictionaries*")))
    (with-current-buffer buf
      (johnson-dict-list-mode)
      (johnson--dict-list-entries)
      (tabulated-list-print))
    (pop-to-buffer buf)))

(defun johnson--dict-list-entries ()
  "Populate `tabulated-list-entries' from `johnson--dictionaries'."
  (johnson--ensure-dictionaries)
  (setq tabulated-list-entries
        (mapcar
         (lambda (dict)
           (let* ((path (plist-get dict :path))
                  (name (plist-get dict :name))
                  (fmt (upcase (plist-get dict :format-name)))
                  (langs (format "%s → %s"
                                 (plist-get dict :source-lang)
                                 (plist-get dict :target-lang)))
                  (index-exists (file-exists-p (johnson-db--index-path path)))
                  (status (cond
                           ((not index-exists) "Not indexed")
                           ((johnson-db-stale-p path) "Needs reindex")
                           (t "Indexed")))
                  (entries (if index-exists
                               (condition-case nil
                                   (johnson--format-number
                                    (johnson-db-entry-count (johnson--get-db path)))
                                 (error "?"))
                             "—")))
             (list path (vector name fmt langs entries status
                                (abbreviate-file-name path)))))
         johnson--dictionaries)))

(defun johnson-dict-list-reindex ()
  "Re-index the dictionary at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (dict (cl-find-if (lambda (d) (equal (plist-get d :path) id))
                                johnson--dictionaries)))
    (let* ((name (plist-get dict :name)))
      (message "Re-indexing %s..." name)
      ;; Force staleness by deleting the index file first.
      (let ((index-path (johnson-db--index-path id)))
        (when (file-exists-p index-path)
          (let ((db (gethash id johnson--db-cache)))
            (when db
              (condition-case nil (johnson-db-close db) (error nil))))
          (remhash id johnson--db-cache)
          (delete-file index-path)))
      (johnson--index-one-dict-sync dict (get-buffer-create " *johnson-reindex*"))
      (message "Re-indexed %s" name))
    (johnson--dict-list-entries)
    (tabulated-list-print t)))

(defun johnson-dict-list-show-details ()
  "Show details for the dictionary at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (dict (cl-find-if (lambda (d) (equal (plist-get d :path) id))
                                johnson--dictionaries)))
    (let ((buf (get-buffer-create "*johnson-dict-details*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Dictionary: %s\n" (plist-get dict :name)))
          (insert (format "Format:     %s\n" (upcase (plist-get dict :format-name))))
          (insert (format "Source:     %s\n" (plist-get dict :source-lang)))
          (insert (format "Target:     %s\n" (plist-get dict :target-lang)))
          (insert (format "Group:      %s\n" (plist-get dict :group)))
          (insert (format "Priority:   %d\n" (plist-get dict :priority)))
          (insert (format "Path:       %s\n" (plist-get dict :path)))
          (let* ((path (plist-get dict :path))
                 (index-path (johnson-db--index-path path)))
            (insert (format "Index:      %s\n" index-path))
            (when (file-exists-p index-path)
              (let ((db (johnson--get-db path)))
                (insert (format "Entries:    %s\n"
                                (johnson--format-number
                                 (johnson-db-entry-count db))))
                (insert (format "Stale:      %s\n"
                                (if (johnson-db-stale-p path) "yes" "no"))))))
          (special-mode)))
      (pop-to-buffer buf))))

;;;; Dictionary reordering

(defvar johnson-reorder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'johnson-reorder-move-up)
    (define-key map (kbd "M-<down>") #'johnson-reorder-move-down)
    (define-key map (kbd "C-c C-c") #'johnson-reorder-save)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `johnson-reorder-mode'.")

(define-derived-mode johnson-reorder-mode special-mode "Johnson-Reorder"
  "Major mode for reordering dictionary priorities.
\\{johnson-reorder-mode-map}")

(defvar-local johnson-reorder--names nil
  "Ordered list of dictionary names in the reorder buffer.")

(defun johnson-reorder--refresh ()
  "Redraw the reorder buffer from `johnson-reorder--names'."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert "Reorder dictionaries (M-up/M-down to move, C-c C-c to save, q to quit)\n\n")
    (let ((i 0))
      (dolist (name johnson-reorder--names)
        (insert (propertize (format "%3d  %s\n" i name)
                            'johnson-reorder-name name))
        (cl-incf i)))
    (goto-char (point-min))
    (forward-line (1- (min line (+ 2 (length johnson-reorder--names)))))))

;;;###autoload
(defun johnson-reorder-dictionaries ()
  "Open a buffer for reordering dictionary priorities."
  (interactive)
  (johnson--ensure-dictionaries)
  (let* ((sorted (sort (copy-sequence johnson--dictionaries)
                       (lambda (a b)
                         (< (or (plist-get a :priority) 0)
                            (or (plist-get b :priority) 0)))))
         (names (mapcar (lambda (d) (plist-get d :name)) sorted))
         (buf (get-buffer-create "*johnson-reorder*")))
    (with-current-buffer buf
      (johnson-reorder-mode)
      (setq johnson-reorder--names names)
      (johnson-reorder--refresh))
    (pop-to-buffer buf)))

(defun johnson-reorder--current-name ()
  "Return the dictionary name on the current line, or nil."
  (get-text-property (line-beginning-position) 'johnson-reorder-name))

(defun johnson-reorder-move-up ()
  "Move the current entry up."
  (interactive)
  (let* ((name (johnson-reorder--current-name))
         (idx (and name (cl-position name johnson-reorder--names :test #'equal))))
    (when (and idx (> idx 0))
      (setf (nth idx johnson-reorder--names)
            (nth (1- idx) johnson-reorder--names))
      (setf (nth (1- idx) johnson-reorder--names) name)
      (johnson-reorder--refresh)
      (forward-line -1))))

(defun johnson-reorder-move-down ()
  "Move the current entry down."
  (interactive)
  (let* ((name (johnson-reorder--current-name))
         (idx (and name (cl-position name johnson-reorder--names :test #'equal))))
    (when (and idx (< idx (1- (length johnson-reorder--names))))
      (setf (nth idx johnson-reorder--names)
            (nth (1+ idx) johnson-reorder--names))
      (setf (nth (1+ idx) johnson-reorder--names) name)
      (johnson-reorder--refresh)
      (forward-line 1))))

(defun johnson-reorder-save ()
  "Save the current ordering as dictionary priorities."
  (interactive)
  (let* ((priorities (let ((i 0))
                       (mapcar (lambda (name)
                                 (prog1 (cons name i)
                                   (cl-incf i)))
                               johnson-reorder--names)))
         (choice (completing-read
                  "Save method: "
                  '("Customize (persistent)" "Session only" "Copy to kill ring")
                  nil t)))
    (pcase choice
      ("Customize (persistent)"
       (customize-save-variable 'johnson-dictionary-priorities priorities)
       (message "Saved via Customize"))
      ("Session only"
       (setq johnson-dictionary-priorities priorities)
       (message "Set for current session"))
      ("Copy to kill ring"
       (kill-new (format "(setq johnson-dictionary-priorities\n      '%S)" priorities))
       (message "Copied setq form to kill ring — paste into your init file")))
    ;; Update in-memory priorities.
    (setq johnson-dictionary-priorities priorities)
    (dolist (dict johnson--dictionaries)
      (plist-put dict :priority
                 (or (cdr (assoc (plist-get dict :name) priorities)) 0)))
    (quit-window)))

;;;; CAPF

(defun johnson-completion-at-point-function ()
  "Completion-at-point function backed by the johnson dictionary index."
  (when-let* ((bounds (bounds-of-thing-at-point 'word)))
    (list (car bounds) (cdr bounds)
          (johnson--completion-table)
          :exclusive 'no)))

;;;; Cache management

;;;###autoload
(defun johnson-close-caches ()
  "Kill all dictionary file cache buffers and close database connections."
  (interactive)
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (string-prefix-p " *johnson-cache: " (buffer-name buf))
        (kill-buffer buf)
        (cl-incf count)))
    (johnson--close-all-dbs)
    (when (boundp 'johnson-dsl--abbreviation-cache)
      (clrhash johnson-dsl--abbreviation-cache))
    (when (boundp 'johnson-mdict--header-cache)
      (clrhash johnson-mdict--header-cache))
    (when (boundp 'johnson-mdict--record-meta-cache)
      (clrhash johnson-mdict--record-meta-cache))
    (when (boundp 'johnson-mdict--offset-cache)
      (clrhash johnson-mdict--offset-cache))
    (when (boundp 'johnson-mdict--block-cache)
      (setq johnson-mdict--block-cache nil))
    (when (boundp 'johnson-mdict--mdd-index-cache)
      (clrhash johnson-mdict--mdd-index-cache))
    (when (boundp 'johnson-mdict--mdd-record-cache)
      (clrhash johnson-mdict--mdd-record-cache))
    (clrhash johnson--eldoc-cache)
    (setq johnson--dictionaries nil)
    (setq johnson--indexed-p nil)
    (message "Closed %d cache buffer%s and all database connections"
             count (if (= count 1) "" "s"))))

;;;###autoload
(defun johnson-clear-index ()
  "Delete all sqlite index files and reset state.
Dictionaries will be re-indexed on next lookup."
  (interactive)
  (when (yes-or-no-p "Delete all johnson index files? ")
    (johnson-close-caches)
    (let ((dir (expand-file-name johnson-cache-directory))
          (count 0))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.sqlite\\'"))
          (delete-file file)
          (cl-incf count)))
      (message "Deleted %d index file%s" count (if (= count 1) "" "s")))))

;;;; Resource extraction from companion zip archives

(defun johnson--resource-zip-path (dict-path)
  "Derive the companion `.dsl.files.zip' archive path from DICT-PATH.
Strip `.dz' if present, then append `.files.zip'.
Return the path if it exists, nil otherwise."
  (let* ((base (if (string-suffix-p ".dz" dict-path t)
                   (file-name-sans-extension dict-path)
                 dict-path))
         (zip (concat base ".files.zip")))
    (when (file-exists-p zip)
      zip)))

(defun johnson--resource-cache-dir (zip-path)
  "Return the cache subdirectory for resources from ZIP-PATH.
The directory is `johnson-cache-directory/resources/<md5-of-zip-path>/'."
  (expand-file-name (md5 zip-path)
                    (expand-file-name "resources" johnson-cache-directory)))

(defun johnson--extract-resource (zip-path filename)
  "Extract FILENAME from ZIP-PATH to the resource cache directory.
Uses `unzip -p' to extract to stdout, then writes to disk.
Return the cached file path on success, nil on failure.
Skip extraction if already cached."
  (let* ((cache-dir (johnson--resource-cache-dir zip-path))
         (cached (expand-file-name (file-name-nondirectory filename) cache-dir)))
    (if (file-exists-p cached)
        cached
      (make-directory (file-name-directory cached) t)
      (let ((exit-code
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (let ((code (call-process "unzip" nil t nil "-p" zip-path filename)))
                 (when (and (zerop code) (> (buffer-size) 0))
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region (point-min) (point-max) cached nil 'silent)))
                 code))))
        (if (file-exists-p cached)
            cached
          (message "johnson: failed to extract %s from %s (exit %s)"
                   filename zip-path exit-code)
          nil)))))

(defun johnson--find-resource-zips (dir)
  "Return all `.dsl.files.zip' archives in DIR, or nil."
  (when (file-directory-p dir)
    (directory-files dir t "\\.dsl\\.files\\.zip\\'" t)))

(defun johnson--resolve-audio-file (audio-path &optional dict-path)
  "Resolve AUDIO-PATH, extracting from a companion zip if needed.
If AUDIO-PATH exists on disk, return it.  Otherwise, search the
audio file's directory for companion `.dsl.files.zip' archives
and extract the file from the first matching one.  If DICT-PATH
is provided, try its companion zip first for efficiency.
Return the resolved path, or nil if unavailable."
  (if (file-exists-p audio-path)
      audio-path
    (let* ((filename (file-name-nondirectory audio-path))
           (dir (file-name-directory audio-path))
           (result nil))
      ;; If dict-path is provided, try its specific companion zip first.
      (when dict-path
        (let ((zip (johnson--resource-zip-path dict-path)))
          (when zip
            (setq result (johnson--extract-resource zip filename)))))
      ;; Otherwise, scan the directory for any companion zips.
      (unless result
        (let ((zips (johnson--find-resource-zips dir)))
          (while (and zips (not result))
            (setq result (johnson--extract-resource (pop zips) filename)))))
      result)))

;;;; Audio playback

(defun johnson--find-external-player ()
  "Return the first available external player, or nil."
  (cl-find-if #'executable-find johnson-audio-external-players))

(defun johnson--start-audio-process (player file)
  "Start PLAYER as an async process to play FILE.
A sentinel reports non-zero exit status."
  (let ((proc (start-process "johnson-audio" nil player file)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (unless (and (string-prefix-p "finished" event)
                    (zerop (process-exit-status process)))
         (message "johnson: %s exited with status %d"
                  (process-name process)
                  (process-exit-status process)))))))

(defun johnson-play-sound (file)
  "Play the audio FILE using the configured player.
See `johnson-audio-player'."
  (cond
   ((stringp johnson-audio-player)
    (johnson--start-audio-process johnson-audio-player file))
   ((eq johnson-audio-player 'auto)
    (condition-case _
        (play-sound-file file)
      (error
       (let ((player (johnson--find-external-player)))
         (if player
             (johnson--start-audio-process player file)
           (message "johnson: no audio player found")
           (info-other-window "(johnson) Audio playback"))))))
   (t
    (message "johnson: invalid `johnson-audio-player' value: %S"
             johnson-audio-player))))

(defun johnson--play-audio (file &optional dict-path)
  "Resolve and play audio FILE.
If the file doesn't exist on disk, search its directory for a
companion `.dsl.files.zip' archive and extract on demand.
DICT-PATH, if provided, is used as an optimization hint."
  (let ((resolved (johnson--resolve-audio-file file dict-path)))
    (if resolved
        (johnson-play-sound resolved)
      (message "Audio file not found: %s" file))))

(defun johnson-play-audio-at-point ()
  "Play the audio file referenced by the button at point."
  (interactive)
  (let ((file (get-text-property (point) 'johnson-audio-file))
        (dict-path (get-text-property (point) 'johnson-audio-dict-path)))
    (if file
        (johnson--play-audio file dict-path)
      (message "No audio at point"))))

(defun johnson-insert-audio-button (file &optional label dict-path)
  "Insert a play button for audio FILE with optional LABEL.
DICT-PATH, if non-nil, is the dictionary file path used to locate
a companion zip archive for on-demand extraction."
  (let ((start (point))
        (text (or label "\u25B6")))
    (insert text)
    (make-text-button start (point)
                      'face 'johnson-audio-button-face
                      'johnson-audio-file file
                      'johnson-audio-dict-path dict-path
                      'action (lambda (_btn)
                                (johnson--play-audio file dict-path))
                      'help-echo (format "Play %s" (file-name-nondirectory file)))
    (insert " ")))

;;;###autoload
(defun johnson-clear-resource-cache ()
  "Delete all extracted resource files from the cache.
Removes the `resources/' subdirectory of `johnson-cache-directory'."
  (interactive)
  (let ((dir (expand-file-name "resources" johnson-cache-directory)))
    (if (file-directory-p dir)
        (when (or noninteractive (yes-or-no-p (format "Delete resource cache %s? " dir)))
          (delete-directory dir t)
          (message "Deleted resource cache: %s" dir))
      (message "No resource cache to delete"))))

;;;; Image support

(defconst johnson--image-extensions
  '("png" "jpg" "jpeg" "gif" "bmp" "svg" "tiff" "webp" "ico")
  "File extensions recognized as images.")

(defun johnson--image-file-p (filename)
  "Return non-nil if FILENAME has an image extension."
  (let ((ext (downcase (or (file-name-extension filename) ""))))
    (member ext johnson--image-extensions)))

(defun johnson--insert-image (file-path &optional max-width)
  "Insert an inline image from FILE-PATH at point.
MAX-WIDTH defaults to the current window width in pixels.
Falls back to a placeholder if display is unavailable or file
is missing.  Respects `johnson-display-images'."
  (if (not johnson-display-images)
      (insert (format "[image: %s]" (file-name-nondirectory file-path)))
    (if (and (display-graphic-p) (file-exists-p file-path))
        (let* ((max-w (or max-width (window-body-width nil t)))
               (img (create-image file-path nil nil :max-width max-w)))
          (insert-image img (format "[image: %s]"
                                    (file-name-nondirectory file-path))))
      (insert (format "[image: %s]" (file-name-nondirectory file-path))))))

(defun johnson-toggle-images ()
  "Toggle inline image display and refresh the results buffer."
  (interactive)
  (setq johnson-display-images (not johnson-display-images))
  (message "Images %s" (if johnson-display-images "enabled" "disabled"))
  (when (and johnson--current-word (derived-mode-p 'johnson-mode))
    (johnson-refresh)))

;;;; Wildcard search

(defun johnson--wildcard-pattern-p (string)
  "Return non-nil if STRING includes unescaped `?' or `*'."
  (string-match-p "[?*]" string))

;;;; Full-text search

(defun johnson--entry-to-plain-text (raw-text format-name)
  "Strip markup from RAW-TEXT according to FORMAT-NAME.
Returns plain text suitable for FTS indexing or eldoc display."
  (if (null raw-text)
      ""
    (with-temp-buffer
      (insert raw-text)
    (goto-char (point-min))
    (pcase format-name
      ("dsl"
       ;; Strip [tag]...[/tag], <<refs>>, {{media}}.
       (while (re-search-forward "\\[/?[a-z!*'][^]]*\\]" nil t)
         (replace-match ""))
       (goto-char (point-min))
       (while (re-search-forward "<<\\([^>]+\\)>>" nil t)
         (replace-match "\\1"))
       (goto-char (point-min))
       (while (re-search-forward "{{[^}]*}}" nil t)
         (replace-match ""))
       ;; Unescape DSL backslash sequences.
       (goto-char (point-min))
       (while (re-search-forward "\\\\\\([][()<>{}~@\\\\ ]\\)" nil t)
         (replace-match "\\1" t)))
      (_
       ;; Strip HTML tags for stardict, mdict, bgl, dict-protocol.
       (while (re-search-forward "<[^>]+>" nil t)
         (replace-match ""))))
    ;; Decode common HTML entities.
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t) (replace-match "&" t t))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<" t t))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">" t t))
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t) (replace-match " " t t))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\"" t t))
    (goto-char (point-min))
    (while (re-search-forward "&#\\([0-9]+\\);" nil t)
      (replace-match (string (string-to-number (match-string 1))) t t))
    ;; Collapse whitespace and trim.
    (goto-char (point-min))
    (while (re-search-forward "[ \t\n\r]+" nil t)
      (replace-match " "))
    (string-trim (buffer-string)))))

(defun johnson--query-all-fts (query)
  "Run full-text search for QUERY across in-scope dictionaries.
Returns a list of (DICT-PLIST HEADWORD SNIPPET) triples."
  (let ((results nil))
    (dolist (dict (johnson--dictionaries-in-scope))
      (condition-case err
          (let* ((path (plist-get dict :path))
                 (db (johnson--get-db path))
                 (matches (johnson-db-query-fts db query 20)))
            (dolist (match matches)
              (push (list dict (car match) (cadr match)) results)))
        (error
         (message "johnson: FTS error for %s: %s"
                  (plist-get dict :name) (error-message-string err)))))
    (nreverse results)))

;;;###autoload
(defun johnson-search (query)
  "Search dictionary definitions for QUERY using full-text search.
Requires `johnson-fts-enabled' to have been non-nil during indexing."
  (interactive "sFull-text search: ")
  (johnson--ensure-dictionaries)
  (unless (johnson--ensure-indexed)
    (user-error "Dictionaries are being indexed"))
  (let ((results (johnson--query-all-fts query)))
    (if (null results)
        (message "No full-text results for \"%s\"" query)
      (let ((buf (get-buffer-create "*johnson*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (unless (derived-mode-p 'johnson-mode)
              (johnson-mode))
            (johnson--cancel-pending-render)
            (erase-buffer)
            (setq johnson--current-word (format "[FTS: %s]" query))
            (insert (propertize (format "Full-text search: \"%s\" (%d results)\n\n"
                                        query (length results))
                                'face 'johnson-section-header-face))
            (dolist (result results)
              (let* ((dict (nth 0 result))
                     (headword (nth 1 result))
                     (snippet (nth 2 result)))
                (let ((start (point)))
                  (insert headword)
                  (make-text-button start (point)
                                    'face 'johnson-ref-face
                                    'action (lambda (_btn)
                                              (johnson-lookup headword))
                                    'help-echo (format "Look up \"%s\"" headword)))
                (insert (format "  [%s]\n" (plist-get dict :name)))
                (let ((start (point)))
                  ;; Highlight >>> <<< markers in snippet.
                  (insert "  " snippet "\n\n")
                  (save-excursion
                    (goto-char start)
                    (while (re-search-forward ">>>\\([^<]*\\)<<<" nil t)
                      (let ((m-start (match-beginning 0))
                            (text (match-string 1)))
                        (replace-match text t t)
                        (add-face-text-property
                         m-start (+ m-start (length text))
                         'highlight)))))))
            (goto-char (point-min))))
        (pop-to-buffer buf)))))

;;;; Eldoc integration

(defun johnson-eldoc-function (callback &rest _args)
  "Eldoc documentation function for johnson dictionaries.
CALLBACK is called with the definition string."
  (when-let* ((word (thing-at-point 'word t)))
    (let ((cached (gethash word johnson--eldoc-cache)))
      (if cached
          (funcall callback cached)
        (condition-case nil
            (progn
              (johnson--ensure-dictionaries)
              (when johnson--indexed-p
                (let ((results (johnson--query-all-exact word)))
                  (when results
                    (let* ((first-result (car results))
                           (dict (car first-result))
                           (match (cadr first-result))
                           (fmt (johnson--get-format
                                 (plist-get dict :format-name)))
                           (path (plist-get dict :path))
                           (raw (funcall (plist-get fmt :retrieve-entry)
                                         path (nth 1 match) (nth 2 match)))
                           (plain (johnson--entry-to-plain-text
                                   raw (plist-get dict :format-name)))
                           (truncated (if (> (length plain)
                                             johnson-eldoc-max-length)
                                          (concat
                                           (substring plain 0
                                                      johnson-eldoc-max-length)
                                           "...")
                                        plain)))
                      ;; LRU cache: cap at 50 entries.
                      (when (> (hash-table-count johnson--eldoc-cache) 50)
                        (clrhash johnson--eldoc-cache))
                      (puthash word truncated johnson--eldoc-cache)
                      (funcall callback truncated))))))
          (error nil)))))
  nil)

;;;###autoload
(define-minor-mode johnson-eldoc-mode
  "Minor mode providing eldoc integration with johnson dictionaries."
  :lighter " JDict"
  (if johnson-eldoc-mode
      (add-hook 'eldoc-documentation-functions
                #'johnson-eldoc-function nil t)
    (remove-hook 'eldoc-documentation-functions
                 #'johnson-eldoc-function t)))

;;;; Scan-popup mode

(defun johnson-scan--show-popup (word definition)
  "Display a popup with DEFINITION for WORD.
Uses posframe if available, otherwise tooltip, otherwise echo area."
  (when johnson--scan-hide-timer
    (cancel-timer johnson--scan-hide-timer))
  (cond
   ((and (fboundp 'posframe-show)
         (display-graphic-p))
    (posframe-show " *johnson-scan*"
                   :string (format "%s: %s" word definition)
                   :timeout johnson-scan-popup-duration
                   :position (point)))
   ((display-graphic-p)
    (tooltip-show (format "%s: %s" word definition))
    (setq johnson--scan-hide-timer
          (run-with-timer johnson-scan-popup-duration nil
                          (lambda () (tooltip-hide)))))
   (t
    (message "%s: %s" word
             (truncate-string-to-width definition 60 nil nil "...")))))

(defun johnson-scan--lookup-word (word)
  "Look up WORD for scan-popup display."
  (when (and word
             (not (string-empty-p word))
             (not (equal word johnson--scan-last-word)))
    (setq johnson--scan-last-word word)
    (condition-case nil
        (progn
          (johnson--ensure-dictionaries)
          (when johnson--indexed-p
            (let ((results (johnson--query-all-exact word)))
              (when results
                (let* ((first-result (car results))
                       (dict (car first-result))
                       (match (cadr first-result))
                       (fmt (johnson--get-format
                             (plist-get dict :format-name)))
                       (path (plist-get dict :path))
                       (raw (funcall (plist-get fmt :retrieve-entry)
                                     path (nth 1 match) (nth 2 match)))
                       (plain (johnson--entry-to-plain-text
                               raw (plist-get dict :format-name)))
                       (truncated (truncate-string-to-width
                                   plain 200 nil nil "...")))
                  (johnson-scan--show-popup word truncated))))))
      (error nil))))

(defvar johnson-scan-mode)

(defun johnson-scan--on-selection ()
  "Hook for `activate-mark-hook' in scan mode."
  (when (and johnson-scan-mode (use-region-p))
    (let ((text (string-trim
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))))
      (when (and (> (length text) 0) (<= (length text) 50))
        (johnson-scan--lookup-word text)))))

(defun johnson-scan--on-idle ()
  "Idle timer callback for scan mode."
  (when johnson-scan-mode
    (let ((word (thing-at-point 'word t)))
      (johnson-scan--lookup-word word))))

;;;###autoload
(define-minor-mode johnson-scan-mode
  "Global minor mode for scan-popup dictionary lookups.
When enabled, looking up words via selection or idle timer."
  :global t
  :lighter " JScan"
  :group 'johnson
  (if johnson-scan-mode
      (progn
        (when (memq johnson-scan-trigger '(selection both))
          (add-hook 'activate-mark-hook #'johnson-scan--on-selection))
        (when (memq johnson-scan-trigger '(idle both))
          (setq johnson--scan-idle-timer
                (run-with-idle-timer johnson-scan-idle-delay t
                                     #'johnson-scan--on-idle))))
    (remove-hook 'activate-mark-hook #'johnson-scan--on-selection)
    (when johnson--scan-idle-timer
      (cancel-timer johnson--scan-idle-timer)
      (setq johnson--scan-idle-timer nil))
    (setq johnson--scan-last-word nil)))

;;;; Bookmarks

(defun johnson--load-bookmarks ()
  "Load bookmarks from `johnson-bookmarks-file'."
  (unless johnson--bookmarks-loaded
    (when (file-exists-p johnson-bookmarks-file)
      (condition-case nil
          (let ((data (with-temp-buffer
                        (insert-file-contents johnson-bookmarks-file)
                        (read (current-buffer)))))
            (setq johnson--bookmarks
                  (if (and (listp data)
                           (cl-every (lambda (b)
                                       (and (listp b)
                                            (stringp (plist-get b :headword))
                                            (stringp (plist-get b :dictionary))))
                                     data))
                      data
                    nil)))
        (error (setq johnson--bookmarks nil))))
    (setq johnson--bookmarks-loaded t)))

(defun johnson--save-bookmarks ()
  "Save bookmarks to `johnson-bookmarks-file'."
  (make-directory (file-name-directory johnson-bookmarks-file) t)
  (with-temp-file johnson-bookmarks-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 johnson--bookmarks (current-buffer)))))

(defun johnson-bookmark-add ()
  "Bookmark the entry at point."
  (interactive)
  (unless (derived-mode-p 'johnson-mode)
    (user-error "Not in a johnson buffer"))
  (johnson--load-bookmarks)
  (let* ((dict-name (johnson--section-name-at (point)))
         (headword johnson--current-word))
    (unless headword
      (user-error "No entry to bookmark"))
    ;; Duplicate check.
    (if (cl-find-if (lambda (b)
                      (and (equal (plist-get b :headword) headword)
                           (equal (plist-get b :dictionary) dict-name)))
                    johnson--bookmarks)
        (message "Already bookmarked: %s" headword)
      (push (list :headword headword
                  :dictionary (or dict-name "")
                  :timestamp (float-time)
                  :path "")
            johnson--bookmarks)
      (johnson--save-bookmarks)
      (message "Bookmarked: %s" headword))))

(defun johnson-bookmark-remove ()
  "Remove the bookmark for the entry at point."
  (interactive)
  (unless (derived-mode-p 'johnson-mode)
    (user-error "Not in a johnson buffer"))
  (johnson--load-bookmarks)
  (let ((headword johnson--current-word))
    (unless headword
      (user-error "No entry to unbookmark"))
    (let ((found (cl-find-if
                  (lambda (b)
                    (equal (plist-get b :headword) headword))
                  johnson--bookmarks)))
      (if found
          (progn
            (setq johnson--bookmarks (delq found johnson--bookmarks))
            (johnson--save-bookmarks)
            (message "Removed bookmark: %s" headword))
        (message "No bookmark for: %s" headword)))))

(defvar johnson-bookmark-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'johnson-bookmark-list-goto)
    (define-key map "d" #'johnson-bookmark-list-delete)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `johnson-bookmark-list-mode'.")

(define-derived-mode johnson-bookmark-list-mode tabulated-list-mode
  "Johnson Bookmarks"
  "Major mode for browsing johnson bookmarks."
  (setq tabulated-list-format [("Headword" 30 t)
                                ("Dictionary" 35 t)
                                ("Date" 20 t)])
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header))

(defun johnson-bookmark-list-goto ()
  "Look up the bookmarked entry at point."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (johnson-lookup (aref entry 0)))))

(defun johnson-bookmark-list-delete ()
  "Delete the bookmark at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when (and id (y-or-n-p "Delete bookmark? "))
      (setq johnson--bookmarks
            (cl-remove-if (lambda (b)
                            (and (equal (plist-get b :headword) (car id))
                                 (equal (plist-get b :dictionary) (cdr id))))
                          johnson--bookmarks))
      (johnson--save-bookmarks)
      (tabulated-list-delete-entry))))

;;;###autoload
(defun johnson-bookmark-list ()
  "Display a list of bookmarked dictionary entries."
  (interactive)
  (johnson--load-bookmarks)
  (let ((buf (get-buffer-create "*johnson-bookmarks*")))
    (with-current-buffer buf
      (johnson-bookmark-list-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (b)
                      (let ((hw (plist-get b :headword))
                            (dict (or (plist-get b :dictionary) ""))
                            (time (plist-get b :timestamp)))
                        (list (cons hw dict)
                              (vector hw dict
                                      (format-time-string
                                       "%Y-%m-%d %H:%M" time)))))
                    johnson--bookmarks))
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;;; History log (with timestamps)

(defun johnson--load-history-log ()
  "Load the timestamped history log from `johnson-history-file'."
  (unless johnson--history-log-loaded
    (when (and johnson-history-persist
               (file-exists-p johnson-history-file))
      (condition-case nil
          (let ((data (with-temp-buffer
                        (insert-file-contents johnson-history-file)
                        (read (current-buffer)))))
            (setq johnson--history-log
                  (if (and (listp data)
                           (cl-every #'listp data))
                      data
                    nil)))
        (error (setq johnson--history-log nil))))
    (setq johnson--history-log-loaded t)
    (unless johnson-history
      (let ((words (mapcar (lambda (entry) (plist-get entry :word))
                           johnson--history-log)))
        (setq johnson-history
              (seq-take (delete-dups words) johnson-history-max))))))

(defun johnson--save-history-log ()
  "Save the timestamped history log to `johnson-history-file'."
  (when johnson-history-persist
    (make-directory (file-name-directory johnson-history-file) t)
    (with-temp-file johnson-history-file
      (let ((print-length nil)
            (print-level nil))
        (prin1 johnson--history-log (current-buffer))))))

(defun johnson--history-log-push (word dict-count)
  "Push WORD with DICT-COUNT results to the timestamped history log."
  (johnson--load-history-log)
  (push (list :word word :timestamp (float-time) :dict-count dict-count)
        johnson--history-log)
  (let ((max-len (* 10 johnson-history-max)))
    (when (> (length johnson--history-log) max-len)
      (setcdr (nthcdr (1- max-len) johnson--history-log) nil)))
  (johnson--save-history-log))

(defvar johnson-history-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'johnson-history-list-goto)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `johnson-history-list-mode'.")

(define-derived-mode johnson-history-list-mode tabulated-list-mode
  "Johnson History"
  "Major mode for browsing johnson lookup history."
  (setq tabulated-list-format [("Headword" 30 t)
                                ("Time" 20 t)
                                ("Results" 10 t)])
  (setq tabulated-list-sort-key '("Time" . t))
  (tabulated-list-init-header))

(defun johnson-history-list-goto ()
  "Look up the history entry at point."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (johnson-lookup (aref entry 0)))))

;;;###autoload
(defun johnson-history-list ()
  "Display the timestamped lookup history."
  (interactive)
  (johnson--load-history-log)
  (let ((buf (get-buffer-create "*johnson-history*")))
    (with-current-buffer buf
      (johnson-history-list-mode)
      (setq tabulated-list-entries
            (let ((i 0))
              (mapcar (lambda (h)
                        (cl-incf i)
                        (let ((word (plist-get h :word))
                              (time (plist-get h :timestamp))
                              (count (plist-get h :dict-count)))
                          (list i (vector word
                                          (format-time-string
                                           "%Y-%m-%d %H:%M" time)
                                          (number-to-string
                                           (or count 0))))))
                      johnson--history-log)))
      (tabulated-list-print))
    (pop-to-buffer buf)))

;;;###autoload
(defun johnson-history-clear ()
  "Clear the lookup history with confirmation."
  (interactive)
  (when (y-or-n-p "Clear all lookup history? ")
    (setq johnson--history-log nil)
    (johnson--save-history-log)
    (setq johnson-history nil)
    (message "History cleared")))

;;;; Load built-in format backends

(require 'johnson-dsl nil t)
(require 'johnson-stardict nil t)
(require 'johnson-mdict nil t)
(require 'johnson-bgl nil t)
(require 'johnson-dict nil t)
(require 'johnson-transient nil t)

(provide 'johnson)
;;; johnson.el ends here
