;;; johnson-dict-test.el --- Tests for johnson-dict -*- lexical-binding: t; -*-

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
