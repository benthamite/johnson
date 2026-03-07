;;; johnson-dict.el --- DICT protocol (RFC 2229) client for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This module provides a DICT protocol (RFC 2229) client backend for
;; the johnson dictionary package.  It connects to DICT servers over
;; TCP, queries definitions, and integrates with the johnson format
;; registry.  Unlike the file-based backends, this module operates
;; over the network and uses a discover/query pattern instead of the
;; detect/index/retrieve pattern.

;;; Code:

(require 'cl-lib)

(declare-function johnson-register-format "johnson")

;;;; User options

(defcustom johnson-dict-servers '(("dict.org" . 2628))
  "List of DICT servers to query.
Each element is a cons cell (HOST . PORT)."
  :type '(repeat (cons (string :tag "Host")
                       (integer :tag "Port")))
  :group 'johnson)

(defcustom johnson-dict-enabled nil
  "Whether the DICT protocol backend is enabled.
When non-nil, johnson will connect to the servers listed in
`johnson-dict-servers' during dictionary discovery."
  :type 'boolean
  :group 'johnson)

;;;; Internal variables

(defvar johnson-dict--connection-cache (make-hash-table :test #'equal)
  "Cache of open DICT connections.
Maps \"host:port\" to a network process.")

(defvar johnson-dict--result-cache (make-hash-table :test #'equal)
  "Cache of DICT query results.
Maps \"path:word\" to a list of definition strings.")

(defconst johnson-dict--timeout 10
  "Timeout in seconds for DICT protocol operations.")

;;;; Connection management

(defun johnson-dict--cache-key (host port)
  "Return the connection cache key for HOST and PORT."
  (format "%s:%d" host port))

(defun johnson-dict--connect (host port)
  "Open a TCP connection to the DICT server at HOST:PORT.
Send CLIENT identification and read the server banner.
Return the network process."
  (let* ((name (format "johnson-dict-%s:%d" host port))
         (buf (generate-new-buffer (format " *%s*" name)))
         (proc nil))
    (condition-case err
        (progn
          (setq proc (open-network-stream name buf host port
                                          :type 'plain))
          (set-process-coding-system proc 'utf-8 'utf-8)
          (process-put proc 'johnson-dict-buffer buf)
          ;; Read and discard the 220 banner.
          (johnson-dict--read-response proc)
          ;; Identify ourselves.
          (johnson-dict--send proc "CLIENT johnson/0.4 (Emacs DICT client)")
          ;; Read the 250 OK response to CLIENT.
          (johnson-dict--read-response proc)
          proc)
      (error
       (when (and proc (process-live-p proc))
         (delete-process proc))
       (when (buffer-live-p buf)
         (kill-buffer buf))
       (signal (car err) (cdr err))))))

(defun johnson-dict--ensure-connection (host port)
  "Return an open connection to HOST:PORT, creating one if needed."
  (let* ((key (johnson-dict--cache-key host port))
         (proc (gethash key johnson-dict--connection-cache)))
    (if (and proc (process-live-p proc))
        proc
      ;; Clean up stale entry if present.
      (when proc
        (johnson-dict--close-process proc)
        (remhash key johnson-dict--connection-cache))
      (let ((new-proc (johnson-dict--connect host port)))
        (puthash key new-proc johnson-dict--connection-cache)
        new-proc))))

(defun johnson-dict--close-process (proc)
  "Close network process PROC and kill its buffer."
  (let ((buf (process-get proc 'johnson-dict-buffer)))
    (when (process-live-p proc)
      (ignore-errors
        (johnson-dict--send proc "QUIT")
        (accept-process-output proc 1)))
    (delete-process proc)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun johnson-dict--disconnect-all ()
  "Close all cached DICT connections."
  (maphash (lambda (_key proc)
             (johnson-dict--close-process proc))
           johnson-dict--connection-cache)
  (clrhash johnson-dict--connection-cache))

;;;; Protocol communication

(defun johnson-dict--send (proc command)
  "Send COMMAND string followed by CRLF to PROC."
  (process-send-string proc (concat command "\r\n")))

(defun johnson-dict--read-response (proc)
  "Read a DICT protocol response from PROC.
Return a cons cell (STATUS-CODE . BODY-LINES) where STATUS-CODE
is an integer and BODY-LINES is a list of strings (nil for
single-line responses).

For multi-line responses, lines are collected until a line
containing just \".\" is encountered.  Dot-stuffed lines (lines
beginning with \"..\") are unescaped."
  (let ((buf (process-get proc 'johnson-dict-buffer))
        (deadline (+ (float-time) johnson-dict--timeout)))
    (unless (buffer-live-p buf)
      (error "DICT connection buffer has been killed"))
    (with-current-buffer buf
      ;; Accumulate data until we have a complete response.
      ;; The buffer may contain unconsumed data from a previous call,
      ;; so start parsing from the beginning.
      (let ((status-code nil)
            (body-lines nil)
            (done nil)
            (in-body nil)
            (line-start (point-min)))
        (while (not done)
          (when (> (float-time) deadline)
            (error "DICT protocol timeout waiting for response"))
          (accept-process-output proc 0.1)
          ;; Parse lines from line-start onward.
          (goto-char line-start)
          (while (and (not done)
                      (re-search-forward "\r?\n" nil t))
            (let* ((eol (match-beginning 0))
                   (line (buffer-substring-no-properties line-start eol)))
              (setq line-start (point))
              (cond
               ;; We are reading a text body.
               (in-body
                (if (equal line ".")
                    ;; End of text body; the response is complete.
                    (progn
                      (setq in-body nil)
                      (setq done t))
                  ;; Unescape dot-stuffed lines.
                  (when (string-prefix-p ".." line)
                    (setq line (substring line 1)))
                  (push line body-lines)))
               ;; Status line: starts with a 3-digit code.
               ((string-match "\\`\\([0-9]\\{3\\}\\) " line)
                (setq status-code (string-to-number (match-string 1 line)))
                ;; 1xx codes (except 150) introduce a text body.
                (if (and (>= status-code 100) (< status-code 200)
                         (/= status-code 150))
                    (setq in-body t)
                  ;; Terminal status codes (2xx, 3xx, 4xx, 5xx) end
                  ;; the response.
                  (when (>= status-code 200)
                    (setq done t))))))))
        ;; Preserve any unconsumed data (e.g. next status line) in
        ;; the buffer; only delete what we have already parsed.
        (delete-region (point-min) line-start)
        (cons status-code (nreverse body-lines))))))

(defun johnson-dict--read-full-define-response (proc)
  "Read a complete DEFINE response from PROC.
A DEFINE response consists of:
  150 n definitions retrieved
  151 word db name - text follows (repeated n times)
  ...text body terminated by \".\"...
  250 ok
Return a list of definition strings, one per 151 block."
  (let ((buf (process-get proc 'johnson-dict-buffer))
        (deadline (+ (float-time) johnson-dict--timeout))
        (definitions nil)
        (current-def-lines nil)
        (in-body nil)
        (done nil)
        (line-start nil))
    (unless (buffer-live-p buf)
      (error "DICT connection buffer has been killed"))
    (with-current-buffer buf
      (setq line-start (point-min))
      (while (not done)
        (when (> (float-time) deadline)
          (error "DICT protocol timeout waiting for DEFINE response"))
        (accept-process-output proc 0.1)
        (goto-char line-start)
        (while (and (not done)
                    (re-search-forward "\r?\n" nil t))
          (let* ((eol (match-beginning 0))
                 (line (buffer-substring-no-properties line-start eol)))
            (setq line-start (point))
            (cond
             ;; Inside a definition text body.
             (in-body
              (if (equal line ".")
                  (progn
                    ;; End of this definition body.
                    (push (string-join (nreverse current-def-lines) "\n")
                          definitions)
                    (setq current-def-lines nil)
                    (setq in-body nil))
                ;; Unescape dot-stuffed lines.
                (when (string-prefix-p ".." line)
                  (setq line (substring line 1)))
                (push line current-def-lines)))
             ;; Status line.
             ((string-match "\\`\\([0-9]\\{3\\}\\) " line)
              (let ((code (string-to-number (match-string 1 line))))
                (cond
                 ;; 150: n definitions retrieved.  Keep reading.
                 ((= code 150) nil)
                 ;; 151: definition follows.  Start collecting body.
                 ((= code 151)
                  (setq in-body t)
                  (setq current-def-lines nil))
                 ;; 250: ok, all definitions sent.
                 ((= code 250) (setq done t))
                 ;; 550, 552: error, no match.
                 ((>= code 400) (setq done t)))))))))
      (delete-region (point-min) line-start))
    (nreverse definitions)))

;;;; Parameter sanitization

(defun johnson-dict--sanitize-param (s)
  "Sanitize S for use in a DICT protocol command.
Strip CR/LF characters and escape backslashes and double quotes."
  (replace-regexp-in-string
   "[\r\n]" ""
   (replace-regexp-in-string "[\\\\\"]" "\\\\\\&" s)))

;;;; DICT commands

(defun johnson-dict--show-databases (host port)
  "Query SHOW DB on the DICT server at HOST:PORT.
Return a list of (DB-NAME . DESCRIPTION) cons cells."
  (let* ((proc (johnson-dict--ensure-connection host port)))
    (johnson-dict--send proc "SHOW DB")
    (let ((response (johnson-dict--read-response proc)))
      (when (= (car response) 110)
        ;; Read the terminating 250 status.
        (johnson-dict--read-response proc)
        ;; Parse body lines: each is "db-name description"
        (let ((result nil))
          (dolist (line (cdr response))
            (when (string-match "\\`\\(\\S-+\\)\\s-+\"?\\(.*?\\)\"?\\'" line)
              (push (cons (match-string 1 line) (match-string 2 line))
                    result)))
          (nreverse result))))))

(defun johnson-dict--define (host port db word)
  "Look up WORD in database DB on the DICT server at HOST:PORT.
Return a list of definition strings."
  (let* ((proc (johnson-dict--ensure-connection host port)))
    (johnson-dict--send proc (format "DEFINE %s \"%s\""
                                     (johnson-dict--sanitize-param db)
                                     (johnson-dict--sanitize-param word)))
    (johnson-dict--read-full-define-response proc)))

(defun johnson-dict--match (host port db strategy word)
  "Match WORD in database DB using STRATEGY on HOST:PORT.
Return a list of matching word strings."
  (let* ((proc (johnson-dict--ensure-connection host port)))
    (johnson-dict--send proc (format "MATCH %s %s \"%s\""
                                     (johnson-dict--sanitize-param db)
                                     strategy
                                     (johnson-dict--sanitize-param word)))
    (let ((response (johnson-dict--read-response proc)))
      (when (= (car response) 152)
        ;; Read the terminating 250 status.
        (johnson-dict--read-response proc)
        ;; Parse body lines: each is "db-name word"
        (let ((result nil))
          (dolist (line (cdr response))
            (when (string-match "\\`\\S-+\\s-+\"?\\(.*?\\)\"?\\'" line)
              (push (match-string 1 line) result)))
          (nreverse result))))))

;;;; Path parsing

(defun johnson-dict--parse-path (path)
  "Parse a dict:// PATH into (HOST PORT DB-NAME).
PATH has the form \"dict://host:port/db-name\"."
  (when (string-match "\\`dict://\\([^:]+\\):\\([0-9]+\\)/\\(.+\\)\\'" path)
    (list (match-string 1 path)
          (string-to-number (match-string 2 path))
          (match-string 3 path))))

;;;; Integration with johnson core

(defun johnson-dict-discover ()
  "Discover DICT server databases as pseudo-dictionaries.
When `johnson-dict-enabled' is non-nil, connect to each server in
`johnson-dict-servers', list available databases, and return a
list of dictionary plists suitable for `johnson--dictionaries'."
  (when johnson-dict-enabled
    (let ((dicts nil))
      (dolist (server johnson-dict-servers)
        (let ((host (car server))
              (port (cdr server)))
          (condition-case err
              (let ((databases (johnson-dict--show-databases host port)))
                (dolist (db databases)
                  (let ((db-name (car db))
                        (description (cdr db)))
                    (push (list :path (format "dict://%s:%d/%s" host port db-name)
                                :format-name "dict-protocol"
                                :name description
                                :source-lang ""
                                :target-lang ""
                                :group "DICT Servers"
                                :priority 100)
                          dicts))))
            (error
             (message "johnson-dict: failed to connect to %s:%d: %s"
                      host port (error-message-string err))))))
      (nreverse dicts))))

(defun johnson-dict-query-exact (path word)
  "Query WORD in the DICT dictionary identified by PATH.
PATH is a dict:// URL.  Return results as a list of
\(HEADWORD OFFSET LENGTH) triples where OFFSET encodes the word
and index as \"WORD:INDEX\" for deterministic cache retrieval."
  (let ((parsed (johnson-dict--parse-path path)))
    (unless parsed
      (error "johnson-dict: invalid DICT path: %s" path))
    (let* ((host (nth 0 parsed))
           (port (nth 1 parsed))
           (db (nth 2 parsed))
           (definitions (condition-case err
                            (johnson-dict--define host port db word)
                          (error
                           (message "johnson-dict: query failed for %s: %s"
                                    word (error-message-string err))
                           nil)))
           (cache-key (format "%s:%s" path word)))
      (when definitions
        (puthash cache-key definitions johnson-dict--result-cache)
        (let ((results nil)
              (idx 0))
          (dolist (_def definitions)
            (push (list word (format "%s:%d" word idx) 0) results)
            (cl-incf idx))
          (nreverse results))))))

(defun johnson-dict-retrieve-entry (path offset _length)
  "Retrieve a cached DICT definition for PATH at OFFSET.
OFFSET is a string \"WORD:INDEX\" as produced by `johnson-dict-query-exact'.
The definition must have been previously cached."
  (unless (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" offset)
    (error "johnson-dict: malformed offset %S" offset))
  (let* ((word (match-string 1 offset))
         (idx (string-to-number (match-string 2 offset)))
         (cache-key (format "%s:%s" path word))
         (defs (gethash cache-key johnson-dict--result-cache)))
    (unless defs
      (error "johnson-dict: no cached definitions for %s word %s"
             path word))
    (unless (< idx (length defs))
      (error "johnson-dict: index %d out of range for %s word %s"
             idx path word))
    (nth idx defs)))

(defun johnson-dict-render-entry (text)
  "Render DICT definition TEXT into the current buffer.
DICT responses are plain text, so insert as-is."
  (insert text))

;;;; Cache clearing

(defun johnson-dict-clear-caches ()
  "Clear all DICT caches and disconnect from servers."
  (johnson-dict--disconnect-all)
  (clrhash johnson-dict--result-cache))

;;;; Format registration

(provide 'johnson-dict)

(with-eval-after-load 'johnson
  (johnson-register-format
   :name "dict-protocol"
   :extensions nil
   :detect #'ignore
   :skip-index t
   :parse-metadata #'ignore
   :build-index #'ignore
   :retrieve-entry #'johnson-dict-retrieve-entry
   :render-entry #'johnson-dict-render-entry
   :query-exact #'johnson-dict-query-exact
   :discover #'johnson-dict-discover))

;;; johnson-dict.el ends here
