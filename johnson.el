;;; johnson.el --- Multi-format dictionary UI for Emacs -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: dictionaries, i18n

;; This file is NOT part of GNU Emacs.

;; Named after Samuel Johnson, author of "A Dictionary of the English
;; Language" (1755).

;;; Commentary:

;; `johnson' is a multi-format dictionary UI for Emacs, providing the
;; functionality of programs such as GoldenDict and StarDict.  It
;; implements native Elisp parsing for all supported formats — no
;; external tools required.
;;
;; Supported formats (v0.1): DSL (ABBYY Lingvo).
;;
;; Usage:
;;   M-x johnson-lookup            Look up a word
;;   M-x johnson-index             Index/re-index all dictionaries
;;   M-x johnson-list-dictionaries Show discovered dictionaries
;;   M-x johnson-select-group      Switch dictionary group

;;; Code:

(require 'johnson-db)
(require 'cl-lib)

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

(defcustom johnson-history-max 100
  "Maximum number of entries in lookup history."
  :type 'integer
  :group 'johnson)

;;;; Faces

(defface johnson-section-header-face
  '((t :inherit bold :extend t))
  "Face for dictionary section headers in the results buffer."
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
                (condition-case nil
                    (funcall (plist-get fmt :detect) path)
                  (error nil)))
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
  (clrhash johnson--db-cache))

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
                           (name (or (plist-get metadata :name) (file-name-base file)))
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
    (setq johnson--dictionaries (nreverse dicts))))

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
    (message "johnson: scope set to %s" (or johnson--current-group "All"))))

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

;;;###autoload
(defun johnson-index ()
  "Index or re-index all discovered dictionaries.
Progress is shown in the *johnson-indexing* buffer."
  (interactive)
  (johnson--discover)
  (let ((buf (get-buffer-create "*johnson-indexing*"))
        (total (length johnson--dictionaries))
        (done 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Indexing %d dictionaries...\n\n" total))))
    (display-buffer buf)
    (dolist (dict johnson--dictionaries)
      (let* ((path (plist-get dict :path))
             (name (plist-get dict :name))
             (format-name (plist-get dict :format-name))
             (fmt (johnson--get-format format-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "Indexing %s..." name))
            (redisplay)))
        (condition-case err
            (if (not (johnson-db-stale-p path))
                (let* ((db (johnson--get-db path))
                       (count (johnson-db-entry-count db)))
                  (with-current-buffer buf
                    (let ((inhibit-read-only t))
                      (goto-char (point-max))
                      (insert (format " up to date (%s entries)\n"
                                      (johnson--format-number count))))))
              (let ((db (johnson--get-db path))
                    (entries nil)
                    (count 0))
                (johnson-db-reset db)
                (johnson-db-set-metadata db "name" name)
                (johnson-db-set-metadata db "format" format-name)
                (johnson-db-set-metadata db "source-lang" (plist-get dict :source-lang))
                (johnson-db-set-metadata db "target-lang" (plist-get dict :target-lang))
                (johnson-db-set-metadata db "source-path" path)
                (johnson-db-set-metadata db "mtime"
                                         (format-time-string
                                          "%s"
                                          (file-attribute-modification-time
                                           (file-attributes path))))
                (funcall (plist-get fmt :build-index) path
                         (lambda (headword offset length)
                           (push (list headword offset length) entries)
                           (cl-incf count)))
                (johnson-db-insert-entries-batch db (nreverse entries))
                (johnson-db-set-metadata db "entry-count" (number-to-string count))
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (goto-char (point-max))
                    (insert (format " done (%s entries)\n"
                                    (johnson--format-number count)))))))
          (error
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (format " ERROR: %s\n" (error-message-string err))))))))
      (cl-incf done))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\nDone. %d/%d dictionaries processed.\n" done total))))
    (setq johnson--indexed-p t)))

(defun johnson--ensure-indexed ()
  "Ensure dictionaries have been indexed."
  (unless johnson--indexed-p
    (when (cl-some (lambda (d) (johnson-db-stale-p (plist-get d :path)))
                   johnson--dictionaries)
      (johnson-index))
    (setq johnson--indexed-p t)))

;;;; Completion

(defun johnson--completion-table ()
  "Return a dynamic completion table for dictionary headwords."
  (let ((last-prefix nil)
        (last-candidates nil)
        (last-counts (make-hash-table :test #'equal)))
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
          (unless (equal normalized last-prefix)
            (setq last-prefix normalized)
            (let ((candidates nil))
              (clrhash last-counts)
              (when (> (length string) 0)
                (dolist (dict (johnson--dictionaries-in-scope))
                  (condition-case nil
                      (let* ((path (plist-get dict :path))
                             (db (johnson--get-db path))
                             (words (johnson-db-query-prefix db string 200)))
                        (dolist (w words)
                          (push w candidates)
                          (puthash w (1+ (or (gethash w last-counts) 0))
                                   last-counts)))
                    (error nil))))
              (setq last-candidates (delete-dups candidates)))))
        (complete-with-action action last-candidates string pred)))))

;;;; Query

(defun johnson--query-all-exact (word)
  "Query all in-scope dictionaries for exact matches on WORD.
Returns a list of (DICT-PLIST . MATCHES) sorted by priority."
  (let ((results nil))
    (dolist (dict johnson--dictionaries)
      (condition-case nil
          (let* ((path (plist-get dict :path))
                 (db (johnson--get-db path))
                 (matches (johnson-db-query-exact db word)))
            (when matches
              (push (cons dict matches) results)))
        (error nil)))
    (sort (nreverse results)
          (lambda (a b)
            (< (or (plist-get (car a) :priority) 0)
               (or (plist-get (car b) :priority) 0))))))

;;;; Lookup

;;;###autoload
(defun johnson-lookup (&optional word)
  "Look up WORD in the dictionary.
If WORD is nil, prompt with `completing-read' (defaults to word at point)."
  (interactive)
  (johnson--ensure-dictionaries)
  (johnson--ensure-indexed)
  (unless word
    (let ((default (thing-at-point 'word t)))
      (setq word (completing-read
                  (format-prompt "Look up" default)
                  (johnson--completion-table)
                  nil nil nil 'johnson-history default))))
  (when (or (null word) (string-empty-p word))
    (user-error "No word given"))
  (unless (equal word (car johnson-history))
    (push word johnson-history)
    (when (> (length johnson-history) johnson-history-max)
      (setcdr (nthcdr (1- johnson-history-max) johnson-history) nil)))
  (let ((results (johnson--query-all-exact word)))
    (johnson--display-results word results)))

;;;; Results buffer

(defun johnson--display-results (word results)
  "Display lookup RESULTS for WORD in the *johnson* buffer."
  (let ((buf (get-buffer-create "*johnson*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (nav-hist johnson--nav-history)
            (nav-pos johnson--nav-position))
        (unless (derived-mode-p 'johnson-mode)
          (johnson-mode))
        (erase-buffer)
        (setq johnson--nav-history nav-hist)
        (setq johnson--nav-position nav-pos)
        (setq johnson--current-word word)
        (unless johnson--navigating-history
          (johnson--nav-push word))
        (if (null results)
            (insert (format "No results found for \"%s\".\n" word))
          (dolist (result results)
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
              (insert "\n"))))
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
    (define-key map (kbd "<backtab>") #'johnson-prev-section-header)
    (define-key map (kbd "RET") #'johnson-follow-ref)
    (define-key map "l" #'johnson-history-back)
    (define-key map "r" #'johnson-history-forward)
    (define-key map "s" #'johnson-new-search)
    (define-key map "g" #'johnson-refresh)
    (define-key map "w" #'johnson-copy-entry)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `johnson-mode'.")

(define-derived-mode johnson-mode special-mode "Johnson"
  "Major mode for displaying dictionary lookup results.
\\{johnson-mode-map}"
  (setq truncate-lines nil)
  (setq word-wrap t))

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

(defun johnson--section-name-at (pos)
  "Return the section name at POS, or nil."
  (cl-some (lambda (ov) (overlay-get ov 'johnson-section))
           (overlays-at pos)))

;;;; Follow reference

(defun johnson-follow-ref ()
  "Follow the cross-reference button at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (button-activate button)
      (message "No cross-reference at point"))))

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
  (when-let* ((id (tabulated-list-get-id)))
    (let* ((dict (cl-find-if (lambda (d) (equal (plist-get d :path) id))
                             johnson--dictionaries))
           (path (plist-get dict :path))
           (name (plist-get dict :name))
           (format-name (plist-get dict :format-name))
           (fmt (johnson--get-format format-name)))
      (message "Re-indexing %s..." name)
      (let ((db (johnson--get-db path))
            (entries nil)
            (count 0))
        (johnson-db-reset db)
        (johnson-db-set-metadata db "name" name)
        (johnson-db-set-metadata db "format" format-name)
        (johnson-db-set-metadata db "source-lang" (plist-get dict :source-lang))
        (johnson-db-set-metadata db "target-lang" (plist-get dict :target-lang))
        (johnson-db-set-metadata db "source-path" path)
        (johnson-db-set-metadata db "mtime"
                                 (format-time-string
                                  "%s"
                                  (file-attribute-modification-time
                                   (file-attributes path))))
        (funcall (plist-get fmt :build-index) path
                 (lambda (headword offset length)
                   (push (list headword offset length) entries)
                   (cl-incf count)))
        (johnson-db-insert-entries-batch db (nreverse entries))
        (johnson-db-set-metadata db "entry-count" (number-to-string count))
        (message "Re-indexed %s: %s entries" name (johnson--format-number count))))
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
    (setq johnson--dictionaries nil)
    (setq johnson--indexed-p nil)
    (message "Closed %d cache buffer%s and all database connections"
             count (if (= count 1) "" "s"))))

;;;; Load built-in format backends

(require 'johnson-dsl nil t)

(provide 'johnson)
;;; johnson.el ends here
