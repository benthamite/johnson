;;; johnson-db.el --- Sqlite index management for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

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

;; This module handles sqlite index management for the johnson dictionary
;; package.  It provides functions for creating and querying per-dictionary
;; sqlite databases that store headword indices with byte offsets into the
;; source dictionary files.

;;; Code:

(require 'cl-lib)
(require 'ucs-normalize)

;;;; User options

(defgroup johnson nil
  "Multi-format dictionary UI."
  :group 'applications)

(defcustom johnson-cache-directory "~/.cache/johnson/"
  "Directory where sqlite index files are stored.
Each dictionary gets its own sqlite database file in this directory,
named by the MD5 hash of the dictionary file's absolute path."
  :type 'directory
  :group 'johnson)

;;;; Internal helpers

(defun johnson-db--index-path (dict-path)
  "Return the sqlite database path for the dictionary at DICT-PATH."
  (expand-file-name (concat (md5 (expand-file-name dict-path)) ".sqlite")
                    johnson-cache-directory))

(defun johnson-db--ensure-cache-directory ()
  "Ensure `johnson-cache-directory' exists."
  (make-directory johnson-cache-directory t))

;;;; Database lifecycle

(defun johnson-db-open (dict-path)
  "Open or create the sqlite database for the dictionary at DICT-PATH.
Creates the cache directory and database tables if they do not exist.
Returns the database connection object."
  (johnson-db--ensure-cache-directory)
  (let ((db (sqlite-open (johnson-db--index-path dict-path))))
    (sqlite-execute db
                    "CREATE TABLE IF NOT EXISTS metadata (
                       key TEXT PRIMARY KEY,
                       value TEXT)")
    (sqlite-execute db
                    "CREATE TABLE IF NOT EXISTS entries (
                       headword TEXT NOT NULL,
                       headword_normalized TEXT NOT NULL,
                       byte_offset INTEGER NOT NULL,
                       byte_length INTEGER NOT NULL)")
    (sqlite-execute db
                    "CREATE INDEX IF NOT EXISTS idx_normalized
                       ON entries(headword_normalized)")
    (sqlite-execute db
                    "CREATE VIRTUAL TABLE IF NOT EXISTS fts_entries
                       USING fts5(headword, definition)")
    db))

(defun johnson-db-close (db)
  "Close the sqlite database connection DB."
  (sqlite-close db))

;;;; Metadata

(defun johnson-db-set-metadata (db key value)
  "Set metadata KEY to VALUE in database DB."
  (sqlite-execute db
                  "INSERT OR REPLACE INTO metadata (key, value) VALUES (?, ?)"
                  (list key value)))

(defun johnson-db-get-metadata (db key)
  "Return the metadata value for KEY in database DB, or nil."
  (caar (sqlite-select db
                       "SELECT value FROM metadata WHERE key = ?"
                       (list key))))

(defun johnson-db-get-all-metadata (db)
  "Return all metadata from DB as an alist of (KEY . VALUE) pairs."
  (mapcar (lambda (row) (cons (car row) (cadr row)))
          (sqlite-select db "SELECT key, value FROM metadata")))

;;;; Headword normalization

(defun johnson-db-normalize (string)
  "Normalize STRING for case/accent-insensitive search.
Performs NFKD decomposition, strips combining diacritical marks
\(Unicode category Mn), and downcases the result."
  (if (or (null string) (string-empty-p string))
      ""
    (let* ((decomposed (ucs-normalize-NFKD-string string))
           (chars (string-to-list decomposed))
           (filtered (cl-remove-if
                      (lambda (ch)
                        (eq (get-char-code-property ch 'general-category) 'Mn))
                      chars)))
      (downcase (apply #'string filtered)))))

(defun johnson-db--prefix-upper-bound (prefix)
  "Return the exclusive upper bound for PREFIX in a range query.
Increments the last character of PREFIX to produce the first string
that does not begin with PREFIX in binary sort order."
  (concat (substring prefix 0 -1)
          (string (1+ (aref prefix (1- (length prefix)))))))

;;;; Entry insertion

(defun johnson-db-insert-entry (db headword byte-offset entry-length)
  "Insert a single entry into DB.
HEADWORD is the original headword text.  BYTE-OFFSET and ENTRY-LENGTH
specify the entry's location in the dictionary file."
  (let ((normalized (johnson-db-normalize headword)))
    (sqlite-execute db
                    "INSERT INTO entries (headword, headword_normalized, byte_offset, byte_length)
                     VALUES (?, ?, ?, ?)"
                    (list headword normalized byte-offset entry-length))))

(defun johnson-db-insert-entries-batch (db entries)
  "Insert ENTRIES into DB in a single transaction for performance.
ENTRIES is a list of (HEADWORD BYTE-OFFSET BYTE-LENGTH) triples."
  (sqlite-execute db "BEGIN TRANSACTION")
  (condition-case err
      (progn
        (dolist (entry entries)
          (let* ((headword (nth 0 entry))
                 (offset (nth 1 entry))
                 (len (nth 2 entry))
                 (normalized (johnson-db-normalize headword)))
            (sqlite-execute db
                            "INSERT INTO entries (headword, headword_normalized, byte_offset, byte_length)
                             VALUES (?, ?, ?, ?)"
                            (list headword normalized offset len))))
        (sqlite-execute db "COMMIT"))
    (error
     (ignore-errors (sqlite-execute db "ROLLBACK"))
     (signal (car err) (cdr err)))))

;;;; Queries

(defun johnson-db-query-exact (db word)
  "Query DB for entries with an exact match on the normalized form of WORD.
Returns a list of (HEADWORD BYTE-OFFSET BYTE-LENGTH) triples."
  (let ((normalized (johnson-db-normalize word)))
    (sqlite-select db
                   "SELECT headword, byte_offset, byte_length FROM entries
                    WHERE headword_normalized = ?"
                   (list normalized))))

(defun johnson-db-query-prefix (db prefix &optional limit)
  "Query DB for headwords matching the normalized PREFIX.
Returns a list of distinct headword strings.  LIMIT defaults to 100."
  (let* ((normalized (johnson-db-normalize prefix))
         (limit (or limit 100)))
    (when (> (length normalized) 0)
      (mapcar #'car
              (sqlite-select db
                             "SELECT DISTINCT headword FROM entries
                              WHERE headword_normalized >= ?
                                AND headword_normalized < ?
                              LIMIT ?"
                             (list normalized
                                   (johnson-db--prefix-upper-bound normalized)
                                   limit))))))

(defun johnson-db-query-wildcard (db pattern &optional limit)
  "Query DB for headwords matching wildcard PATTERN.
PATTERN uses `?' for single character and `*' for any characters.
Returns a list of distinct headword strings.  LIMIT defaults to 200."
  (let* ((normalized (johnson-db-normalize pattern))
         ;; Escape SQL wildcards first.
         (escaped (replace-regexp-in-string "[%_]" "\\\\\\&" normalized))
         ;; Convert user wildcards to SQL wildcards.
         (sql-pattern (replace-regexp-in-string
                       "\\*" "%"
                       (replace-regexp-in-string "\\?" "_" escaped)))
         (limit (or limit 200)))
    (mapcar #'car
            (sqlite-select db
                           "SELECT DISTINCT headword FROM entries
                            WHERE headword_normalized LIKE ? ESCAPE '\\'
                            LIMIT ?"
                           (list sql-pattern limit)))))

(defun johnson-db-entry-count (db)
  "Return the total number of entries in DB."
  (caar (sqlite-select db "SELECT COUNT(*) FROM entries")))

;;;; Staleness detection

(defun johnson-db-stale-p (dict-path)
  "Return non-nil if the index for DICT-PATH is stale or does not exist.
Compares the stored modification time in the database metadata against
the actual file modification time."
  (let ((index-path (johnson-db--index-path dict-path)))
    (if (not (file-exists-p index-path))
        t
      (if (not (file-attributes dict-path))
          t
        (let ((db (sqlite-open index-path)))
          (unwind-protect
              (let* ((stored-mtime (caar (sqlite-select db
                                                      "SELECT value FROM metadata WHERE key = ?"
                                                      '("mtime"))))
                   (actual-mtime (format-time-string
                                  "%s"
                                  (file-attribute-modification-time
                                   (file-attributes dict-path)))))
              (or (not (equal stored-mtime actual-mtime))
                  ;; Treat databases with zero entries as stale -- they
                  ;; were likely created by a broken parser version.
                  (condition-case nil
                      (zerop (johnson-db-entry-count db))
                    (sqlite-error t))))
          (sqlite-close db)))))))

(defun johnson-db-stale-quick-p (dict-path)
  "Fast filesystem-only staleness check for DICT-PATH.
Returns non-nil if the index file does not exist or the dictionary
file has been modified after the index was last written.  Unlike
`johnson-db-stale-p', this never opens a sqlite database."
  (let ((index-path (johnson-db--index-path dict-path)))
    (or (not (file-exists-p index-path))
        (time-less-p (file-attribute-modification-time
                      (file-attributes index-path))
                     (file-attribute-modification-time
                      (file-attributes dict-path))))))

;;;; Reset

(defun johnson-db-reset (db)
  "Delete all entries from DB for re-indexing."
  (sqlite-execute db "DELETE FROM entries")
  (ignore-errors (sqlite-execute db "DELETE FROM fts_entries"))
  (sqlite-execute db "DELETE FROM metadata WHERE key = 'fts-indexed'"))

;;;; Unified completion index

(defvar johnson-db--completion-db nil
  "Cached sqlite connection for the unified completion index.")

(defun johnson-db-completion-index-path ()
  "Return the path to the unified completion index database."
  (expand-file-name "completion.sqlite" johnson-cache-directory))

(defun johnson-db-get-completion-db ()
  "Return the cached completion index connection, opening on first use.
Returns nil if the completion index file does not exist."
  (cond
   (johnson-db--completion-db
    johnson-db--completion-db)
   ((file-exists-p (johnson-db-completion-index-path))
    (setq johnson-db--completion-db
          (sqlite-open (johnson-db-completion-index-path))))))

(defun johnson-db-close-completion-db ()
  "Close the cached completion index connection."
  (when johnson-db--completion-db
    (condition-case err
        (sqlite-close johnson-db--completion-db)
      (error (message "johnson-db: error closing completion db: %S" err)))
    (setq johnson-db--completion-db nil)))

(defun johnson-db-rebuild-completion-index (dict-paths)
  "Rebuild the unified completion index from per-dictionary databases.
DICT-PATHS is a list of dictionary file paths whose sqlite indexes
should be aggregated.  Returns the total number of unique headwords."
  (johnson-db--ensure-cache-directory)
  (johnson-db-close-completion-db)
  (let ((norm-count (make-hash-table :test #'equal))
        (all-headwords (make-hash-table :test #'equal))
        (db (sqlite-open (johnson-db-completion-index-path))))
    (unwind-protect
        (progn
          ;; Collect headwords from all per-dict databases.
          ;; Count dictionaries by *normalized* form so the annotation
          ;; matches what the normalized lookup will actually find.
          (dolist (path dict-paths)
            (let ((idx (johnson-db--index-path path)))
              (when (file-exists-p idx)
                (let ((per-db (sqlite-open idx))
                      (seen (make-hash-table :test #'equal)))
                  (unwind-protect
                      (dolist (row (sqlite-select per-db
                                    "SELECT DISTINCT headword FROM entries"))
                        (let* ((hw (car row))
                               (norm (johnson-db-normalize hw)))
                          (puthash hw t all-headwords)
                          ;; Count each normalized form at most once per dict.
                          (unless (gethash norm seen)
                            (puthash norm t seen)
                            (puthash norm (1+ (gethash norm norm-count 0))
                                     norm-count))))
                    (sqlite-close per-db))))))
          ;; Recreate the table and bulk-insert inside a single transaction.
          (condition-case err
              (progn
                (sqlite-execute db "BEGIN TRANSACTION")
                (sqlite-execute db "DROP TABLE IF EXISTS completions")
                (sqlite-execute db
                  "CREATE TABLE completions (
                     headword TEXT NOT NULL,
                     headword_normalized TEXT NOT NULL,
                     dict_count INTEGER NOT NULL DEFAULT 1)")
                (maphash (lambda (hw _)
                           (let* ((norm (johnson-db-normalize hw))
                                  (count (gethash norm norm-count 0)))
                             (sqlite-execute db
                               "INSERT INTO completions (headword, headword_normalized, dict_count)
                                VALUES (?, ?, ?)"
                               (list hw norm count))))
                         all-headwords)
                (sqlite-execute db
                  "CREATE INDEX idx_comp_normalized ON completions(headword_normalized)")
                (sqlite-execute db "COMMIT"))
            (error
             (ignore-errors (sqlite-execute db "ROLLBACK"))
             (signal (car err) (cdr err))))
          (hash-table-count all-headwords))
      (sqlite-close db))))

(defun johnson-db-query-completion (db prefix &optional limit)
  "Query the unified completion DB for headwords matching PREFIX.
Returns a list of (HEADWORD DICT-COUNT) pairs.  LIMIT defaults to 200."
  (let* ((normalized (johnson-db-normalize prefix))
         (limit (or limit 200)))
    (when (> (length normalized) 0)
      (sqlite-select db
                     "SELECT headword, dict_count FROM completions
                      WHERE headword_normalized >= ?
                        AND headword_normalized < ?
                      LIMIT ?"
                     (list normalized
                           (johnson-db--prefix-upper-bound normalized)
                           limit)))))

;;;; Full-text search

(defun johnson-db-insert-fts (db headword plain-text)
  "Insert HEADWORD and PLAIN-TEXT into the FTS table of DB."
  (sqlite-execute db
                  "INSERT INTO fts_entries (headword, definition) VALUES (?, ?)"
                  (list headword plain-text)))

(defun johnson-db--fts-escape (query)
  "Escape QUERY for FTS5 MATCH syntax.
Wraps the entire query in double-quotes so that FTS5 operators
like AND, OR, NOT, *, and parentheses are treated as literals."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" query) "\""))

(defun johnson-db-query-fts (db query &optional limit)
  "Query the FTS table of DB for QUERY.
Returns a list of (HEADWORD SNIPPET) pairs.  LIMIT defaults to 50."
  (let ((limit (or limit 50)))
    (sqlite-select db
                   "SELECT headword, snippet(fts_entries, 1, '>>>', '<<<', '...', 30)
                      FROM fts_entries
                     WHERE fts_entries MATCH ?
                     ORDER BY rank
                     LIMIT ?"
                   (list (johnson-db--fts-escape query) limit))))

(defun johnson-db-fts-indexed-p (db)
  "Return non-nil if DB has been indexed for full-text search."
  (equal (johnson-db-get-metadata db "fts-indexed") "yes"))

(defun johnson-db-set-fts-indexed (db)
  "Mark DB as having been indexed for full-text search."
  (johnson-db-set-metadata db "fts-indexed" "yes"))

(provide 'johnson-db)
;;; johnson-db.el ends here
