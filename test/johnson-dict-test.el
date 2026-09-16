;;; johnson-dict-test.el --- Tests for johnson-dict -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Assisted-by: various LLMs (Claude, Codex)

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

;; ERT tests for the johnson-dict DICT protocol backend module.
;; Tests are divided into offline (parsing, caching) and live (network)
;; categories.  Live tests require connectivity to dict.org and are
;; skipped when the server is unreachable.

;;; Code:

(require 'ert)
(require 'johnson-dict)
(require 'johnson)

;;;; Helpers

(defun johnson-dict-test--cleanup ()
  "Clean up DICT caches."
  (johnson-dict-clear-caches))

(defun johnson-dict-test--network-available-p ()
  "Return non-nil if dict.org is reachable on port 2628."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "johnson-dict-test-probe"
                   :host "dict.org"
                   :service 2628
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

;;;; Fake local server

(defun johnson-dict-test--start-fake-server ()
  "Start a DICT-like server on the loopback interface and return it.
The server greets each client with a 220 banner, answers CLIENT with
250, and answers DEFINE per `johnson-dict-test--fake-handle'.  Accepted
client processes are collected in the server's `clients' property so
`johnson-dict-test--with-fake-server' can delete them.  The port is the
server's `:service' contact."
  (make-network-process
   :name "johnson-dict-test-server" :server t :host "127.0.0.1"
   :service t :family 'ipv4 :noquery t
   :filter #'johnson-dict-test--fake-filter
   :log (lambda (server client _message)
          (set-process-query-on-exit-flag client nil)
          (process-put server 'clients
                       (cons client (process-get server 'clients)))
          (process-send-string client "220 fake dict server\r\n"))))

(defun johnson-dict-test--fake-filter (proc string)
  "Split STRING from client PROC into CRLF lines and handle each."
  (let ((pending (concat (process-get proc 'pending) string)))
    (while (string-match "\\`\\(.*?\\)\r\n" pending)
      (let ((line (match-string 1 pending)))
        (setq pending (substring pending (match-end 0)))
        (johnson-dict-test--fake-handle proc line)))
    (process-put proc 'pending pending)))

(defun johnson-dict-test--fake-handle (proc line)
  "Answer command LINE from client PROC.
A DEFINE of \"slow\" is answered one second later, a DEFINE of \"drop\"
closes the connection without answering, and any other DEFINE is
answered at once with one definition reading \"DEF OF WORD\"."
  (cond ((string-prefix-p "CLIENT" line)
         (process-send-string proc "250 ok\r\n"))
        ((string-match "\\`DEFINE \\S-+ \"\\(.*\\)\"\\'" line)
         (let ((word (match-string 1 line)))
           (cond ((equal word "slow")
                  (run-at-time 1 nil #'johnson-dict-test--fake-define-reply
                               proc word))
                 ((equal word "drop") (delete-process proc))
                 (t (johnson-dict-test--fake-define-reply proc word)))))
        ((string-prefix-p "QUIT" line)
         (process-send-string proc "221 bye\r\n"))))

(defun johnson-dict-test--fake-define-reply (proc word)
  "Send client PROC a complete one-definition DEFINE response for WORD."
  (when (process-live-p proc)
    (process-send-string
     proc
     (format (concat "150 1 definitions retrieved\r\n"
                     "151 \"%s\" db \"Test\"\r\nDEF OF %s\r\n.\r\n250 ok\r\n")
             word word))))

(defmacro johnson-dict-test--with-fake-server (port &rest body)
  "Run BODY with PORT bound to a fake DICT server's port.
The connection and result caches are fresh; connections, accepted
clients, and the server are deleted afterwards, even when BODY fails."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((johnson-dict--connection-cache (make-hash-table :test #'equal))
          (johnson-dict--result-cache (make-hash-table :test #'equal))
          (server (johnson-dict-test--start-fake-server))
          (,port (process-contact server :service)))
     (unwind-protect
         (progn ,@body)
       (johnson-dict--disconnect-all)
       (mapc #'delete-process (process-get server 'clients))
       (delete-process server))))

(ert-deftest johnson-dict-test-timeout-drops-connection ()
  "A timed-out DEFINE drops the connection so its late reply is never read."
  (johnson-dict-test--with-fake-server port
    (let ((johnson-dict--timeout 0.3))
      (should-error (johnson-dict--define "127.0.0.1" port "db" "slow")
                    :type 'error))
    (should-not (gethash (johnson-dict--cache-key "127.0.0.1" port)
                         johnson-dict--connection-cache))
    ;; Give the slow reply time to arrive on the old connection.
    (sleep-for 1)
    (should (equal (johnson-dict--define "127.0.0.1" port "db" "fast")
                   '("DEF OF fast")))))

(ert-deftest johnson-dict-test-closed-connection-fails-fast ()
  "A server dropping the connection fails the command before the timeout."
  (johnson-dict-test--with-fake-server port
    (let* ((johnson-dict--timeout 5)
           (start (float-time))
           (err (should-error
                 (johnson-dict--define "127.0.0.1" port "db" "drop"))))
      (should (string-match-p "closed" (error-message-string err)))
      (should (< (- (float-time) start) 2)))))

;;;; Path parsing

(ert-deftest johnson-dict-test-parse-path ()
  "Parses dict:// URLs correctly."
  (let ((parsed (johnson-dict--parse-path "dict://dict.org:2628/wn")))
    (should (equal (nth 0 parsed) "dict.org"))
    (should (= (nth 1 parsed) 2628))
    (should (equal (nth 2 parsed) "wn"))))

(ert-deftest johnson-dict-test-parse-path-different-port ()
  "Parses dict:// URL with non-standard port."
  (let ((parsed (johnson-dict--parse-path "dict://example.com:9999/mydb")))
    (should (equal (nth 0 parsed) "example.com"))
    (should (= (nth 1 parsed) 9999))
    (should (equal (nth 2 parsed) "mydb"))))

(ert-deftest johnson-dict-test-parse-path-invalid ()
  "Returns nil for invalid paths."
  (should-not (johnson-dict--parse-path "/some/file/path.dsl"))
  (should-not (johnson-dict--parse-path "http://example.com/dict"))
  (should-not (johnson-dict--parse-path "")))

;;;; Cache key generation

(ert-deftest johnson-dict-test-cache-key ()
  "Generates correct connection cache keys."
  (should (equal (johnson-dict--cache-key "dict.org" 2628)
                 "dict.org:2628"))
  (should (equal (johnson-dict--cache-key "localhost" 9999)
                 "localhost:9999")))

;;;; Result cache

(ert-deftest johnson-dict-test-result-cache-store-retrieve ()
  "Stores and retrieves definitions from the result cache."
  (johnson-dict-test--cleanup)
  (let ((path "dict://dict.org:2628/wn")
        (word "test")
        (defs '("definition one" "definition two")))
    (puthash (format "%s:%s" path word) defs johnson-dict--result-cache)
    ;; Retrieve first definition.
    (should (equal (johnson-dict-retrieve-entry path "test:0" 0)
                   "definition one"))
    ;; Retrieve second definition.
    (should (equal (johnson-dict-retrieve-entry path "test:1" 0)
                   "definition two"))
    (johnson-dict-test--cleanup)))

(ert-deftest johnson-dict-test-result-cache-miss ()
  "Signals error for uncached definitions."
  (johnson-dict-test--cleanup)
  (should-error (johnson-dict-retrieve-entry
                 "dict://example.com:2628/nonexistent" 0 0)
                :type 'error)
  (johnson-dict-test--cleanup))

;;;; Rendering

(ert-deftest johnson-dict-test-render-plain-text ()
  "Renders plain text definition into buffer."
  (with-temp-buffer
    (johnson-dict-render-entry "A small domesticated feline.")
    (should (equal (buffer-string) "A small domesticated feline."))))

(ert-deftest johnson-dict-test-render-multiline ()
  "Renders multiline definition into buffer."
  (with-temp-buffer
    (johnson-dict-render-entry "Line one.\nLine two.\nLine three.")
    (should (equal (buffer-string) "Line one.\nLine two.\nLine three."))))

;;;; Query-exact integration

(ert-deftest johnson-dict-test-query-exact-caches-results ()
  "query-exact populates the result cache."
  (johnson-dict-test--cleanup)
  (skip-unless (johnson-dict-test--network-available-p))
  (let ((results (johnson-dict-query-exact
                  "dict://dict.org:2628/wn" "cat")))
    (should results)
    (should (> (length results) 0))
    ;; Each result should be (HEADWORD OFFSET LENGTH).
    (let ((first (car results)))
      (should (equal (nth 0 first) "cat"))
      (should (stringp (nth 1 first)))
      (should (string-match-p "\\`cat:[0-9]+\\'" (nth 1 first)))
      (should (= (nth 2 first) 0)))
    ;; Cache should contain the definitions.
    (let ((cached (gethash "dict://dict.org:2628/wn:cat"
                           johnson-dict--result-cache)))
      (should cached)
      (should (> (length cached) 0))))
  (johnson-dict-test--cleanup))

;;;; Worker hooks

(ert-deftest johnson-dict-test-worker-query-bypasses-parent-cache ()
  "Worker query returns entry packets without touching the result cache."
  (let ((johnson-dict--result-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'johnson-dict--define)
               (lambda (_host _port _db _word)
                 '("first definition" "second definition"))))
      (should
       (equal
        (mapcar (lambda (packet) (plist-get packet :raw))
                (johnson-dict-worker-query
                 '(:path "dict://dict.org:2628/gcide") "house"))
        '("first definition" "second definition")))
      (should (= (hash-table-count johnson-dict--result-cache) 0)))))

(ert-deftest johnson-dict-test-worker-query-no-match ()
  "Worker query returns nil when the server has no definitions."
  (let ((johnson-dict--result-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'johnson-dict--define)
               (lambda (_host _port _db _word) nil)))
      (should-not (johnson-dict-worker-query
                   '(:path "dict://dict.org:2628/gcide") "xyzzyplugh42"))
      (should (= (hash-table-count johnson-dict--result-cache) 0)))))

(ert-deftest johnson-dict-test-worker-query-network-error ()
  "Worker query lets network errors signal to the caller."
  (let ((johnson-dict--result-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'johnson-dict--define)
               (lambda (_host _port _db _word)
                 (error "DICT protocol timeout waiting for response"))))
      (should-error (johnson-dict-worker-query
                     '(:path "dict://dict.org:2628/gcide") "house")
                    :type 'error)
      (should (= (hash-table-count johnson-dict--result-cache) 0)))))

(ert-deftest johnson-dict-test-worker-config-roundtrip ()
  "Worker config serializes and restores servers and enabled state."
  (let* ((johnson-dict-enabled t)
         (johnson-dict-servers '(("dict.example.org" . 2628)
                                 ("localhost" . 9999)))
         (config (johnson-dict-worker-config)))
    (let ((johnson-dict-enabled nil)
          (johnson-dict-servers nil))
      (johnson-dict-apply-worker-config config)
      (should (eq johnson-dict-enabled t))
      (should (equal johnson-dict-servers
                     '(("dict.example.org" . 2628)
                       ("localhost" . 9999)))))))

(ert-deftest johnson-dict-test-worker-hooks-registered ()
  "DICT format registers worker query and config hooks."
  (let ((fmt (johnson--get-format "dict-protocol")))
    (should (eq (plist-get fmt :worker-query) #'johnson-dict-worker-query))
    (should (eq (plist-get fmt :worker-config) #'johnson-dict-worker-config))
    (should (eq (plist-get fmt :apply-worker-config)
                #'johnson-dict-apply-worker-config))))

;;;; Format registration

(ert-deftest johnson-dict-test-format-registered ()
  "DICT protocol format is registered with johnson."
  (let ((fmt (johnson--get-format "dict-protocol")))
    (should fmt)
    (should (equal (plist-get fmt :name) "dict-protocol"))
    (should (null (plist-get fmt :extensions)))
    (should (eq (plist-get fmt :skip-index) t))
    (should (eq (plist-get fmt :detect) #'ignore))
    (should (eq (plist-get fmt :retrieve-entry) #'johnson-dict-retrieve-entry))
    (should (eq (plist-get fmt :render-entry) #'johnson-dict-render-entry))
    (should (eq (plist-get fmt :query-exact) #'johnson-dict-query-exact))
    (should (eq (plist-get fmt :discover) #'johnson-dict-discover))))

;;;; Discover

(ert-deftest johnson-dict-test-discover-disabled ()
  "Discover returns nil when DICT is disabled."
  (let ((johnson-dict-enabled nil))
    (should-not (johnson-dict-discover))))

(ert-deftest johnson-dict-test-discover-enabled ()
  "Discover returns databases when DICT is enabled."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((johnson-dict-enabled t)
        (johnson-dict-servers '(("dict.org" . 2628))))
    (let ((dicts (johnson-dict-discover)))
      (should dicts)
      (should (> (length dicts) 0))
      ;; Each dict should have the expected properties.
      (let ((first (car dicts)))
        (should (string-prefix-p "dict://" (plist-get first :path)))
        (should (equal (plist-get first :format-name) "dict-protocol"))
        (should (equal (plist-get first :group) "DICT Servers"))
        (should (= (plist-get first :priority) 100)))))
  (johnson-dict-test--cleanup))

;;;; Live SHOW DB

(ert-deftest johnson-dict-test-show-databases ()
  "Lists databases from dict.org."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((databases (johnson-dict--show-databases "dict.org" 2628)))
    (should databases)
    (should (> (length databases) 0))
    ;; Each entry should be (DB-NAME . DESCRIPTION).
    (let ((first (car databases)))
      (should (stringp (car first)))
      (should (stringp (cdr first))))
    ;; dict.org should have a "wn" (WordNet) database.
    (should (cl-find "wn" databases :key #'car :test #'equal)))
  (johnson-dict-test--cleanup))

;;;; Live DEFINE

(ert-deftest johnson-dict-test-define-word ()
  "Defines a word from dict.org WordNet."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((definitions (johnson-dict--define "dict.org" 2628 "wn" "cat")))
    (should definitions)
    (should (> (length definitions) 0))
    ;; Definition text should mention feline-related content.
    (should (cl-some (lambda (def) (string-match-p "feline\\|Felis" def))
                     definitions)))
  (johnson-dict-test--cleanup))

(ert-deftest johnson-dict-test-define-no-match ()
  "Returns nil for a word not in the database."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((definitions (johnson-dict--define "dict.org" 2628 "wn"
                                           "xyzzyplugh42")))
    (should-not definitions))
  (johnson-dict-test--cleanup))

;;;; Live MATCH

(ert-deftest johnson-dict-test-match-exact ()
  "Matches a word with exact strategy."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((matches (johnson-dict--match "dict.org" 2628 "wn" "exact" "cat")))
    (should matches)
    (should (member "cat" matches)))
  (johnson-dict-test--cleanup))

(ert-deftest johnson-dict-test-match-prefix ()
  "Matches words with prefix strategy."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((matches (johnson-dict--match "dict.org" 2628 "wn" "prefix" "cat")))
    (should matches)
    (should (> (length matches) 1))
    ;; All matches should start with "cat".
    (should (cl-every (lambda (w) (string-prefix-p "cat" w)) matches)))
  (johnson-dict-test--cleanup))

;;;; Full round-trip

(ert-deftest johnson-dict-test-full-roundtrip ()
  "Full round-trip: discover, query, retrieve, render."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((johnson-dict-enabled t)
        (johnson-dict-servers '(("dict.org" . 2628))))
    ;; 1. Discover.
    (let ((dicts (johnson-dict-discover)))
      (should dicts)
      ;; Find the WordNet dictionary.
      (let ((wn (cl-find-if
                 (lambda (d)
                   (string-match-p "/wn\\'" (plist-get d :path)))
                 dicts)))
        (should wn)
        (let ((path (plist-get wn :path)))
          ;; 2. Query.
          (let ((results (johnson-dict-query-exact path "hello")))
            (should results)
            ;; 3. Retrieve.
            (let* ((first-result (car results))
                   (text (johnson-dict-retrieve-entry
                          path (nth 1 first-result) (nth 2 first-result))))
              (should (stringp text))
              (should (> (length text) 0))
              ;; 4. Render.
              (with-temp-buffer
                (johnson-dict-render-entry text)
                (should (> (buffer-size) 0)))))))))
  (johnson-dict-test--cleanup))

;;;; Connection management

(ert-deftest johnson-dict-test-connection-reuse ()
  "Connections are reused from the cache."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (let ((proc1 (johnson-dict--ensure-connection "dict.org" 2628))
        (proc2 (johnson-dict--ensure-connection "dict.org" 2628)))
    (should (eq proc1 proc2)))
  (johnson-dict-test--cleanup))

(ert-deftest johnson-dict-test-disconnect-all ()
  "disconnect-all closes all connections."
  (skip-unless (johnson-dict-test--network-available-p))
  (johnson-dict-test--cleanup)
  (johnson-dict--ensure-connection "dict.org" 2628)
  (should (> (hash-table-count johnson-dict--connection-cache) 0))
  (johnson-dict--disconnect-all)
  (should (= (hash-table-count johnson-dict--connection-cache) 0)))

(provide 'johnson-dict-test)
;;; johnson-dict-test.el ends here
