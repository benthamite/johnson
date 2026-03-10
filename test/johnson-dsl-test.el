;;; johnson-dsl-test.el --- Tests for johnson-dsl -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the johnson-dsl format backend module.

;;; Code:

(require 'ert)
(require 'johnson)

;;;; Helpers

(defvar johnson-dsl-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-dsl-test--fixture (name)
  "Return the full path to the fixture file NAME."
  (expand-file-name name johnson-dsl-test--fixtures-dir))

(defun johnson-dsl-test--kill-cache-buffers ()
  "Kill all johnson cache buffers."
  (dolist (buf (buffer-list))
    (when (string-prefix-p " *johnson-cache: " (buffer-name buf))
      (kill-buffer buf))))

;;;; Encoding detection

(ert-deftest johnson-dsl-test-detect-encoding-utf8 ()
  "Detects UTF-8 encoding (no BOM)."
  (should (eq (johnson-dsl--detect-encoding
               (johnson-dsl-test--fixture "test-dict.dsl"))
              'utf-8)))

(ert-deftest johnson-dsl-test-detect-encoding-utf16le ()
  "Detects UTF-16LE encoding (FF FE BOM)."
  (should (eq (johnson-dsl--detect-encoding
               (johnson-dsl-test--fixture "test-dict-utf16.dsl"))
              'utf-16-le)))

;;;; Format detection

(ert-deftest johnson-dsl-test-detect-accepts-dsl ()
  "Accepts a valid DSL file."
  (should (johnson-dsl-detect
           (johnson-dsl-test--fixture "test-dict.dsl"))))

(ert-deftest johnson-dsl-test-detect-rejects-non-dsl ()
  "Rejects a file that is not a DSL dictionary."
  (let ((non-dsl (make-temp-file "johnson-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file non-dsl
            (insert "This is not a DSL file.\n"))
          (should-not (johnson-dsl-detect non-dsl)))
      (delete-file non-dsl))))

(ert-deftest johnson-dsl-test-detect-rejects-wrong-extension ()
  "Rejects a file without .dsl extension even if content starts with #."
  (let ((wrong-ext (make-temp-file "johnson-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file wrong-ext
            (insert "#NAME \"Fake\"\n"))
          (should-not (johnson-dsl-detect wrong-ext)))
      (delete-file wrong-ext))))

;;;; Metadata parsing

(ert-deftest johnson-dsl-test-parse-metadata-utf8 ()
  "Parses metadata from a UTF-8 DSL file."
  (let ((meta (johnson-dsl-parse-metadata
               (johnson-dsl-test--fixture "test-dict.dsl"))))
    (should (equal (plist-get meta :name) "Test Dictionary"))
    (should (equal (plist-get meta :source-lang) "English"))
    (should (equal (plist-get meta :target-lang) "Spanish"))))

(ert-deftest johnson-dsl-test-parse-metadata-utf16 ()
  "Parses metadata from a UTF-16LE DSL file."
  (let ((meta (johnson-dsl-parse-metadata
               (johnson-dsl-test--fixture "test-dict-utf16.dsl"))))
    (should (equal (plist-get meta :name) "Test UTF-16 Dictionary"))
    (should (equal (plist-get meta :source-lang) "English"))
    (should (equal (plist-get meta :target-lang) "French"))))

(ert-deftest johnson-dsl-test-parse-metadata-alternation ()
  "Parses metadata from the alternation fixture."
  (let ((meta (johnson-dsl-parse-metadata
               (johnson-dsl-test--fixture "test-dict-alternation.dsl"))))
    (should (equal (plist-get meta :name) "Test Alternation Dictionary"))))

;;;; Headword expansion

(ert-deftest johnson-dsl-test-expand-simple ()
  "Simple headword returns a single-element list."
  (should (equal (johnson-dsl--expand-headword "hello")
                 '("hello"))))

(ert-deftest johnson-dsl-test-expand-slash-split ()
  "Split marker {/} produces separate headwords."
  (should (equal (johnson-dsl--expand-headword "colour{/}color")
                 '("colour" "color"))))

(ert-deftest johnson-dsl-test-expand-optional ()
  "Optional part (es) produces two variants."
  (let ((result (johnson-dsl--expand-headword "go(es)")))
    (should (member "go" result))
    (should (member "goes" result))
    (should (= (length result) 2))))

(ert-deftest johnson-dsl-test-expand-alternation-empty ()
  "Alternation {u/} with empty alternative produces both forms."
  (let ((result (johnson-dsl--expand-headword "favo{u/}rite")))
    (should (member "favourite" result))
    (should (member "favorite" result))
    (should (= (length result) 2))))

(ert-deftest johnson-dsl-test-expand-alternation-two ()
  "Alternation {ize/ise} produces two variants."
  (let ((result (johnson-dsl--expand-headword "real{ize/ise}")))
    (should (member "realize" result))
    (should (member "realise" result))
    (should (= (length result) 2))))

(ert-deftest johnson-dsl-test-expand-escaped-braces ()
  "Escaped braces produce literal braces."
  (should (equal (johnson-dsl--expand-headword "\\{curly\\}")
                 '("{curly}"))))

(ert-deftest johnson-dsl-test-expand-escaped-parens ()
  "Escaped parentheses produce literal parentheses."
  (should (equal (johnson-dsl--expand-headword "\\(round\\)")
                 '("(round)"))))

;;;; Index building

(ert-deftest johnson-dsl-test-build-index-utf8 ()
  "Builds index from UTF-8 fixture and produces correct entry count."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict.dsl")
     (lambda (hw offset len)
       (push (list hw offset len) entries)))
    (johnson-dsl-test--kill-cache-buffers)
    ;; test-dict.dsl has 11 headwords (apple..bookshelf).
    (should (>= (length entries) 11))))

(ert-deftest johnson-dsl-test-build-index-alternation ()
  "Builds index from alternation fixture, expanding headwords."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict-alternation.dsl")
     (lambda (hw offset len)
       (push (list hw offset len) entries)))
    (johnson-dsl-test--kill-cache-buffers)
    ;; Should contain expanded headwords.
    (let ((headwords (mapcar #'car entries)))
      (should (member "colour" headwords))
      (should (member "color" headwords))
      (should (member "go" headwords))
      (should (member "goes" headwords))
      (should (member "favourite" headwords))
      (should (member "favorite" headwords))
      (should (member "realize" headwords))
      (should (member "realise" headwords))
      (should (member "{curly}" headwords))
      (should (member "(round)" headwords)))))

(ert-deftest johnson-dsl-test-build-index-multihead ()
  "Multi-headword entries each get indexed."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict-multihead.dsl")
     (lambda (hw offset len)
       (push (list hw offset len) entries)))
    (johnson-dsl-test--kill-cache-buffers)
    (let ((headwords (mapcar #'car entries)))
      ;; big, large, great should all map to the same body.
      (should (member "big" headwords))
      (should (member "large" headwords))
      (should (member "great" headwords))
      ;; Verify they share the same offset.
      (let ((big-offset (nth 1 (cl-find "big" entries :key #'car :test #'equal)))
            (large-offset (nth 1 (cl-find "large" entries :key #'car :test #'equal))))
        (should (= big-offset large-offset))))))

(ert-deftest johnson-dsl-test-build-index-byte-offsets ()
  "Byte offsets are non-negative and lengths are positive."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict.dsl")
     (lambda (hw offset len)
       (push (list hw offset len) entries)))
    (johnson-dsl-test--kill-cache-buffers)
    (dolist (entry entries)
      (should (>= (nth 1 entry) 0))
      (should (> (nth 2 entry) 0)))))

;;;; Entry retrieval

(ert-deftest johnson-dsl-test-retrieve-entry-utf8 ()
  "Retrieves an entry from a UTF-8 fixture."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict.dsl")
     (lambda (hw offset len)
       (when (equal hw "apple")
         (push (list hw offset len) entries))))
    (johnson-dsl-test--kill-cache-buffers)
    (should entries)
    (let* ((entry (car entries))
           (raw (johnson-dsl-retrieve-entry
                 (johnson-dsl-test--fixture "test-dict.dsl")
                 (nth 1 entry) (nth 2 entry))))
      (johnson-dsl-test--kill-cache-buffers)
      (should (stringp raw))
      (should (string-match-p "apple" raw))
      (should (string-match-p "manzana" raw)))))

(ert-deftest johnson-dsl-test-retrieve-entry-utf16 ()
  "Retrieves an entry from a UTF-16LE fixture."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((entries nil))
    (johnson-dsl-build-index
     (johnson-dsl-test--fixture "test-dict-utf16.dsl")
     (lambda (hw offset len)
       (when (equal hw "hello")
         (push (list hw offset len) entries))))
    (johnson-dsl-test--kill-cache-buffers)
    (should entries)
    (let* ((entry (car entries))
           (raw (johnson-dsl-retrieve-entry
                 (johnson-dsl-test--fixture "test-dict-utf16.dsl")
                 (nth 1 entry) (nth 2 entry))))
      (johnson-dsl-test--kill-cache-buffers)
      (should (stringp raw))
      (should (string-match-p "bonjour" raw)))))

;;;; Entry rendering

(ert-deftest johnson-dsl-test-render-bold ()
  "Bold tags apply `johnson-bold-face'."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[b]bold text[/b]")
    (goto-char (point-min))
    (let* ((pos (text-property-any (point-min) (point-max) 'face nil))
           ;; Find the start of bold text.
           (bold-start (text-property-any (point-min) (point-max) 'face 'johnson-bold-face)))
      (should bold-start)
      (should (equal (get-text-property bold-start 'face) 'johnson-bold-face)))))

(ert-deftest johnson-dsl-test-render-color ()
  "Color tags apply the correct face."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[c green]green text[/c]")
    (let ((pos (point-min))
          (found nil))
      (while (and (< pos (point-max)) (not found))
        (let ((face (get-text-property pos 'face)))
          (when (and face (or (eq face 'johnson-color-green-face)
                              (and (listp face)
                                   (memq 'johnson-color-green-face face))))
            (setq found t)))
        (setq pos (1+ pos)))
      (should found))))

(ert-deftest johnson-dsl-test-render-ref-button ()
  "Ref tags create buttons."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[ref]some ref[/ref]")
    (goto-char (point-min))
    ;; The button starts at point-min, so use button-at rather than next-button.
    (let ((btn (button-at (point-min))))
      (should btn)
      (should (equal (button-label btn) "some ref")))))

(ert-deftest johnson-dsl-test-render-margin ()
  "Margin tags set line-prefix property."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[m2]indented text[/m]")
    (goto-char (point-min))
    (let ((prefix (get-text-property (point-min) 'line-prefix)))
      ;; The margin tag sets line-prefix to a string.
      (should (stringp prefix)))))

(ert-deftest johnson-dsl-test-render-tags-removed ()
  "DSL tags are not visible in the rendered text."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[b]word[/b] [c green]colored[/c]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "\\[b\\]" text))
      (should-not (string-match-p "\\[/b\\]" text))
      (should-not (string-match-p "\\[c " text))
      (should-not (string-match-p "\\[/c\\]" text))
      (should (string-match-p "word" text))
      (should (string-match-p "colored" text)))))

(ert-deftest johnson-dsl-test-render-crossref-button ()
  "Double-angle-bracket cross-references create buttons."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t<<reference>>")
    (goto-char (point-min))
    ;; The button starts at point-min, so use button-at rather than next-button.
    (let ((btn (button-at (point-min))))
      (should btn)
      (should (equal (button-label btn) "reference")))))

;;;; Abbreviation support

(ert-deftest johnson-dsl-test-abbreviation-path-plain ()
  "Derives abbreviation path from a plain DSL file."
  (should (equal (johnson-dsl--abbreviation-path "/tmp/dict/foo.dsl")
                 "/tmp/dict/foo_abrv.dsl")))

(ert-deftest johnson-dsl-test-abbreviation-path-dictzip ()
  "Derives abbreviation path from a dictzip DSL file."
  (should (equal (johnson-dsl--abbreviation-path "/tmp/dict/foo.dsl.dz")
                 "/tmp/dict/foo_abrv.dsl")))

(ert-deftest johnson-dsl-test-load-abbreviations ()
  "Loads abbreviation table from the test fixture."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (let* ((dict-path (johnson-dsl-test--fixture "test-dict.dsl"))
         (table (johnson-dsl--load-abbreviations dict-path)))
    (johnson-dsl-test--kill-cache-buffers)
    (clrhash johnson-dsl--abbreviation-cache)
    (should (hash-table-p table))
    (should (equal (gethash "noun" table) "sustantivo"))
    (should (equal (gethash "f" table) "femenino"))
    (should (equal (gethash "m" table) "masculino"))
    (should (equal (gethash "adj" table) "adjetivo"))))

(ert-deftest johnson-dsl-test-load-abbreviations-missing ()
  "Returns nil when no abbreviation file exists."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (let* ((dict-path (johnson-dsl-test--fixture "test-dict-alternation.dsl"))
         (table (johnson-dsl--load-abbreviations dict-path)))
    (johnson-dsl-test--kill-cache-buffers)
    (clrhash johnson-dsl--abbreviation-cache)
    (should (null table))))

(ert-deftest johnson-dsl-test-load-abbreviations-cache ()
  "Abbreviation loader caches results per directory."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (let* ((dict-path (johnson-dsl-test--fixture "test-dict.dsl"))
         (table1 (johnson-dsl--load-abbreviations dict-path))
         (table2 (johnson-dsl--load-abbreviations dict-path)))
    (johnson-dsl-test--kill-cache-buffers)
    (clrhash johnson-dsl--abbreviation-cache)
    ;; Same object returned from cache.
    (should (eq table1 table2))))

(ert-deftest johnson-dsl-test-render-p-face ()
  "[p] tags apply `johnson-abbreviation-face'."
  (with-temp-buffer
    (let ((johnson-dsl--current-dict-path nil))
      (johnson-dsl-render-entry "\t[p]noun[/p]")
      (let ((pos (point-min))
            (found nil))
        (while (and (< pos (point-max)) (not found))
          (let ((face (get-text-property pos 'face)))
            (when (and face (or (eq face 'johnson-abbreviation-face)
                                (and (listp face)
                                     (memq 'johnson-abbreviation-face face))))
              (setq found t)))
          (setq pos (1+ pos)))
        (should found)))))

(ert-deftest johnson-dsl-test-render-p-help-echo ()
  "[p] tags get `help-echo' when abbreviation file exists."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (with-temp-buffer
    (let ((johnson-dsl--current-dict-path
           (johnson-dsl-test--fixture "test-dict.dsl")))
      (johnson-dsl-render-entry "\t[p]noun[/p]")
      (let ((echo (get-text-property (point-min) 'help-echo)))
        (should (equal echo "sustantivo")))))
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache))

(ert-deftest johnson-dsl-test-render-p-no-abrv-file ()
  "[p] tags render without error when no abbreviation file exists."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (with-temp-buffer
    (let ((johnson-dsl--current-dict-path
           (johnson-dsl-test--fixture "test-dict-alternation.dsl")))
      (johnson-dsl-render-entry "\t[p]noun[/p]")
      ;; Should still have the face, but no help-echo.
      (let ((face (get-text-property (point-min) 'face))
            (echo (get-text-property (point-min) 'help-echo)))
        (should (or (eq face 'johnson-abbreviation-face)
                    (and (listp face)
                         (memq 'johnson-abbreviation-face face))))
        (should (null echo)))))
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache))

(ert-deftest johnson-dsl-test-render-strips-standalone-backslash ()
  "Standalone backslash lines are stripped during rendering."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t\\\n\t[p]noun[/p]\n\t\\")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should-not (string-match-p "\\\\" text)))))

(ert-deftest johnson-dsl-test-render-escaped-brackets ()
  "Escaped brackets \\=\\[ \\=\\] are unescaped and not consumed as tags."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[m1]\\[haus\\]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[haus\\]" text))
      (should-not (string-match-p "\\\\" text)))))

(ert-deftest johnson-dsl-test-render-escaped-brackets-uppercase ()
  "Escaped brackets with uppercase content are preserved."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[m1]\\[SER un DIP uh tee\\]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[SER un DIP uh tee\\]" text))
      (should-not (string-match-p "\\\\" text)))))

(provide 'johnson-dsl-test)
;;; johnson-dsl-test.el ends here
