;;; johnson-db.el --- Sqlite index management for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

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
     (sqlite-execute db "ROLLBACK")
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
  (let ((normalized (johnson-db-normalize prefix))
        (limit (or limit 100)))
    (mapcar #'car
            (sqlite-select db
                           "SELECT DISTINCT headword FROM entries
                            WHERE headword_normalized LIKE ?
                            LIMIT ?"
                           (list (concat normalized "%") limit)))))

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
      (let* ((db (sqlite-open index-path))
             (stored-mtime (caar (sqlite-select db
                                                "SELECT value FROM metadata WHERE key = ?"
                                                '("mtime"))))
             (actual-mtime (format-time-string
                            "%s"
                            (file-attribute-modification-time
                             (file-attributes dict-path)))))
        (sqlite-close db)
        (not (equal stored-mtime actual-mtime))))))

;;;; Reset

(defun johnson-db-reset (db)
  "Delete all entries from DB for re-indexing."
  (sqlite-execute db "DELETE FROM entries"))

(provide 'johnson-db)
;;; johnson-db.el ends here
