;;; johnson-dsl-test.el --- Tests for johnson-dsl -*- lexical-binding: t; -*-

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

(ert-deftest johnson-dsl-test-color-darkmagenta ()
  "Color name `darkmagenta' maps to violet, not the green default."
  (should (eq (johnson-dsl--color-face "darkmagenta")
              'johnson-color-violet-face)))

(ert-deftest johnson-dsl-test-color-cadetblue ()
  "Color name `cadetblue' maps to blue."
  (should (eq (johnson-dsl--color-face "cadetblue")
              'johnson-color-blue-face)))

(ert-deftest johnson-dsl-test-color-darkcyan ()
  "Color name `darkcyan' maps to blue."
  (should (eq (johnson-dsl--color-face "darkcyan")
              'johnson-color-blue-face)))

(ert-deftest johnson-dsl-test-color-darkolivegreen ()
  "Color name `darkolivegreen' maps to green."
  (should (eq (johnson-dsl--color-face "darkolivegreen")
              'johnson-color-green-face)))

(ert-deftest johnson-dsl-test-color-no-argument ()
  "The [c] tag without a color argument uses the default face."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[c]{[/c]text[c]}[/c]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "{" text))
      (should (string-match-p "}" text))
      (should-not (string-match-p "\\[c\\]" text)))))

(ert-deftest johnson-dsl-test-render-transcription-tag ()
  "The [t] tag applies italic face to transcription content."
  (with-temp-buffer
    (johnson-dsl-render-entry "\t[m1]\\[[t]eɪ[/t]\\]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "\\[eɪ\\]" text)))
    ;; Check that the transcription text has italic face.
    (goto-char (point-min))
    (let* ((bracket-pos (text-property-search-forward 'face 'johnson-italic-face
                                                       (lambda (val prop)
                                                         (if (listp prop)
                                                             (memq val prop)
                                                           (eq val prop))))))
      (should bracket-pos))))

(ert-deftest johnson-dsl-test-render-inline-translation ()
  "Inline phrase translations after an arrow stay on the same line."
  (with-temp-buffer
    (johnson-dsl-render-entry
     "\t[m3][*][lang id=1033][c blue]besotted with drink[/c][/lang] ▶ [trn]embrutecido por la bebida[/trn][/*][/m]")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p
               "besotted with drink ▶ embrutecido por la bebida"
               text))
      (should-not (string-match-p "▶[ \t]*\n" text)))))

;;;; Worker context

(ert-deftest johnson-dsl-test-worker-prepares-abbreviations ()
  "Prepared packets render abbreviations without parent file access."
  (johnson-dsl-test--kill-cache-buffers)
  (clrhash johnson-dsl--abbreviation-cache)
  (unwind-protect
      (let* ((dict-path (johnson-dsl-test--fixture "test-dict.dsl"))
             (packet (johnson-dsl-worker-prepare-entry
                      (list :path dict-path) nil "\t[p]n[/p] [p]noun[/p]"))
             (context (plist-get packet :context)))
        (should (eq (plist-get context :prepared) t))
        (should (equal (plist-get context :dict-path) dict-path))
        (should (equal (plist-get context :dict-dir)
                       (file-name-directory dict-path)))
        (should (plist-member context :resources))
        (let ((abbrevs (plist-get context :abbreviations)))
          ;; Only referenced expansions are retained.
          (should (equal (cdr (assoc "n" abbrevs)) "noun"))
          (should (equal (cdr (assoc "noun" abbrevs)) "sustantivo"))
          (should-not (assoc "adj" abbrevs)))
        (cl-letf (((symbol-function 'johnson-dsl--load-abbreviations)
                   (lambda (_path) (error "parent abbreviation read")))
                  ((symbol-function 'johnson--resolve-audio-file)
                   (lambda (&rest _args) (error "parent archive read"))))
          (with-temp-buffer
            (johnson-dsl-render-entry-with-context
             (plist-get packet :raw) (plist-get packet :context))
            (should (equal (get-text-property (point-min) 'help-echo)
                           "noun")))))
    (johnson-dsl-test--kill-cache-buffers)
    (clrhash johnson-dsl--abbreviation-cache)))

(ert-deftest johnson-dsl-test-media-references-padded-after-prefix ()
  "Whitespace-padded [s] refs after a long prefix terminate promptly.
Regression: `string-trim' clobbered the loop's match data, so the scan
position rewound behind the current tag and re-matched forever."
  (should (equal (johnson-dsl--media-references
                  "padding padding padding [s] a.wav[/s]")
                 '("a.wav"))))

(ert-deftest johnson-dsl-test-worker-prepares-resources ()
  "Preparation resolves referenced media; parent renders from the map."
  (let* ((dict-path (johnson-dsl-test--fixture "test-dict.dsl"))
         (dict-dir (file-name-directory dict-path))
         (resolve-calls nil))
    (cl-letf (((symbol-function 'johnson--resolve-audio-file)
               (lambda (path dict)
                 (push (list path dict) resolve-calls)
                 (when (string-suffix-p "apple.wav" path)
                   "/cache/apple.wav"))))
      (let* ((packet (johnson-dsl-worker-prepare-entry
                      (list :path dict-path) nil
                      "\t[s]apple.wav[/s] {{missing.jpg}}"))
             (resources (plist-get (plist-get packet :context) :resources)))
        (should (equal (cdr (assoc "apple.wav" resources))
                       "/cache/apple.wav"))
        (should-not (assoc "missing.jpg" resources))
        ;; Both references were resolved at prepare time.
        (should (cl-find (expand-file-name "apple.wav" dict-dir)
                         resolve-calls :key #'car :test #'equal))
        (should (cl-find (expand-file-name "missing.jpg" dict-dir)
                         resolve-calls :key #'car :test #'equal))
        ;; Parent rendering reads only the prepared mapping.
        (cl-letf (((symbol-function 'johnson--resolve-audio-file)
                   (lambda (&rest _args) (error "parent archive read"))))
          (with-temp-buffer
            (johnson-dsl-render-entry-with-context
             (plist-get packet :raw) (plist-get packet :context))
            (should (equal (get-text-property (point-min) 'johnson-audio-file)
                           "/cache/apple.wav"))))))))

(ert-deftest johnson-dsl-test-render-with-context-sibling-media ()
  "Prepared rendering falls back to media files already on disk."
  (let* ((dir (make-temp-file "johnson-dsl-test-" t))
         (wav (expand-file-name "beep.wav" dir)))
    (unwind-protect
        (progn
          (with-temp-file wav (insert "RIFF"))
          (cl-letf (((symbol-function 'johnson--resolve-audio-file)
                     (lambda (&rest _args) (error "parent archive read"))))
            (with-temp-buffer
              (johnson-dsl-render-entry-with-context
               "\t[s]beep.wav[/s]"
               (list :prepared t
                     :dict-path (expand-file-name "d.dsl" dir)
                     :dict-dir (file-name-as-directory dir)
                     :abbreviations nil
                     :resources nil))
              (should (equal (get-text-property (point-min) 'johnson-audio-file)
                             wav)))))
      (delete-directory dir t))))

(ert-deftest johnson-dsl-test-worker-hooks-registered ()
  "The DSL format registers the worker context hooks."
  (let ((fmt (johnson--get-format "dsl")))
    (should (eq (plist-get fmt :worker-prepare-entry)
                #'johnson-dsl-worker-prepare-entry))
    (should (eq (plist-get fmt :render-entry-with-context)
                #'johnson-dsl-render-entry-with-context))))

;;;; Headword expansion bound (regression: exponential expansion)

(ert-deftest johnson-dsl-test-expand-headword-bounded ()
  "Expanding a headword with many optional groups is bounded in time and size."
  (let* ((headword (concat "w" (apply #'concat (make-list 20 "(a)"))))
         (start (float-time))
         (result (johnson-dsl--expand-headword headword)))
    (should (< (- (float-time) start) 1.0))
    (should (<= (length result) johnson-dsl--max-headword-variants))
    (should (member "w" result))))

;;;; Multi-line comments (regression: comment lines indexed as headwords)

(ert-deftest johnson-dsl-test-build-index-skips-multiline-comment ()
  "Lines inside a column-0 {{ ... }} comment block are not headwords."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((path (make-temp-file "johnson-dsl-comment-" nil ".dsl")))
    (unwind-protect
        (let ((headwords nil))
          (with-temp-file path
            (insert "#NAME \"Comment\"\n#INDEX_LANGUAGE \"English\"\n"
                    "#CONTENTS_LANGUAGE \"English\"\n\n"
                    "{{\nCopyright 2005 Someone\nAll rights reserved\n}}\n"
                    "apple\n\tfruit\n"))
          (johnson-dsl-build-index path (lambda (hw _offset _length)
                                          (push hw headwords)))
          (should (equal headwords '("apple"))))
      (johnson-dsl-test--kill-cache-buffers)
      (delete-file path))))

;;;; BOM-less UTF-16 (regression: accepted but indexed as empty)

(defun johnson-dsl-test--write-bomless (path coding)
  "Write a small BOM-less DSL dictionary to PATH using CODING."
  (with-temp-file path
    (set-buffer-multibyte nil)
    (insert (encode-coding-string
             (concat "#NAME \"NoBom\"\n#INDEX_LANGUAGE \"English\"\n"
                     "#CONTENTS_LANGUAGE \"English\"\n\napple\n\tfruit\n")
             coding))))

(ert-deftest johnson-dsl-test-bomless-utf16le ()
  "A UTF-16LE file without a BOM is detected, parsed and indexed."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((path (make-temp-file "johnson-dsl-nobom-le-" nil ".dsl"))
        (headwords nil))
    (unwind-protect
        (progn
          (johnson-dsl-test--write-bomless path 'utf-16le)
          (should (eq (johnson-dsl--detect-encoding path) 'utf-16le))
          (should (johnson-dsl-detect path))
          (should (equal (plist-get (johnson-dsl-parse-metadata path) :name)
                         "NoBom"))
          (johnson-dsl-build-index path (lambda (hw _offset _length)
                                          (push hw headwords)))
          (should (equal headwords '("apple"))))
      (johnson-dsl-test--kill-cache-buffers)
      (delete-file path))))

(ert-deftest johnson-dsl-test-bomless-utf16be ()
  "A UTF-16BE file without a BOM is detected, parsed and indexed."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((path (make-temp-file "johnson-dsl-nobom-be-" nil ".dsl"))
        (headwords nil))
    (unwind-protect
        (progn
          (johnson-dsl-test--write-bomless path 'utf-16be)
          (should (eq (johnson-dsl--detect-encoding path) 'utf-16be))
          (should (johnson-dsl-detect path))
          (should (equal (plist-get (johnson-dsl-parse-metadata path) :name)
                         "NoBom"))
          (johnson-dsl-build-index path (lambda (hw _offset _length)
                                          (push hw headwords)))
          (should (equal headwords '("apple"))))
      (johnson-dsl-test--kill-cache-buffers)
      (delete-file path))))

(ert-deftest johnson-dsl-test-bomless-utf16le-nul-density ()
  "BOM-less UTF-16LE whose first line is not a header is still detected."
  (let ((path (make-temp-file "johnson-dsl-nobom-nul-" nil ".dsl")))
    (unwind-protect
        (progn
          (with-temp-file path
            (set-buffer-multibyte nil)
            (insert (encode-coding-string
                     "\n\napple\n\tfruit\nbanana\n\tfruit\n" 'utf-16le)))
          (should (eq (johnson-dsl--detect-encoding path) 'utf-16le)))
      (delete-file path))))

;;;; Formatting tags in headword lines (regression: split on "/" of closing tags)

(ert-deftest johnson-dsl-test-expand-headword-drops-annotation-tags ()
  "Part-of-speech and color annotations in a headword line are not headwords.
Spanish dictionaries write `lexicógrafo[p]f.[/p] [c]- fa[/c]' (Larousse
wraps the same in braces: `abad{[p]f.[/p] [c]- desa[/c]}').  The [p]
element is the part of speech and the [c] element the feminine ending
shown next to the headword; neither is a form a user would look up, so
both elements are dropped with their content and only the headword proper
is indexed."
  (should (equal (johnson-dsl--expand-headword "lexicógrafo[p]f.[/p] [c]- fa[/c]")
                 '("lexicógrafo")))
  (should (equal (johnson-dsl--expand-headword "abad{[p]f.[/p] [c]- desa[/c]}")
                 '("abad")))
  (should (equal (johnson-dsl--expand-headword "cupo. [p][p]m.[/p][/p]")
                 '("cupo."))))

(ert-deftest johnson-dsl-test-expand-headword-keeps-formatted-text ()
  "Formatting tags are stripped but the text they wrap stays in the headword."
  (should (equal (johnson-dsl--expand-headword "Enfe[i]š[/i]tillar")
                 '("Enfeštillar")))
  (should (equal (johnson-dsl--expand-headword "{[i]}Alabama{[/i]} Dispute")
                 '("Alabama Dispute")))
  (should (equal (johnson-dsl--expand-headword "dureza [p]f[/p]")
                 '("dureza"))))

(ert-deftest johnson-dsl-test-expand-headword-tags-keep-escaped-brackets ()
  "Escaped brackets survive tag stripping and are unescaped as before."
  (should (equal (johnson-dsl--expand-headword "word \\[x\\] [p]m.[/p]")
                 '("word [x]"))))

(ert-deftest johnson-dsl-test-expand-headword-trims-whitespace ()
  "Surrounding whitespace is trimmed from every variant and empties dropped."
  (should (equal (johnson-dsl--expand-headword "Lexicography ")
                 '("Lexicography")))
  (should (equal (johnson-dsl--expand-headword "go(es) ")
                 '("go" "goes")))
  (should-not (johnson-dsl--expand-headword "[p]m.[/p]")))

(ert-deftest johnson-dsl-test-build-index-tagged-headword-keeps-body ()
  "A tagged headword line indexes the clean headword with the intact body."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((path (make-temp-file "johnson-dsl-tagged-" nil ".dsl"))
        (entries nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "#NAME \"Tagged\"\n#INDEX_LANGUAGE \"Spanish\"\n"
                    "#CONTENTS_LANGUAGE \"Spanish\"\n\n"
                    "lexicógrafo[p]f.[/p] [c]- fa[/c]\n"
                    "\t[m1]Persona que compone diccionarios.[/m]\n"
                    "Lexicography \n\t[m1]The craft.[/m]\n"))
          (johnson-dsl-build-index path (lambda (hw offset length)
                                          (push (list hw offset length) entries)))
          (should (equal (mapcar #'car entries) '("Lexicography" "lexicógrafo")))
          (let ((entry (cl-find "lexicógrafo" entries :key #'car :test #'equal)))
            (should (equal (johnson-dsl-retrieve-entry path (nth 1 entry) (nth 2 entry))
                           "\t[m1]Persona que compone diccionarios.[/m]\n"))))
      (johnson-dsl-test--kill-cache-buffers)
      (delete-file path))))

;;;; Unindented body lines (regression: article text indexed as headwords)

(ert-deftest johnson-dsl-test-body-continuation-p ()
  "Margin tags or excessive length mark a column-0 line as body text.
Closing tags alone do not: Corominas writes `Enfe[i]š[/i]tillar' and
Harrap's `dureza [p]f[/p]' as headword lines directly after a body."
  (should (johnson-dsl--body-continuation-p
           "Neugriechisch[/b][/c][/m]</li><li><a href=\"#x\">[m0][b]▪"))
  (should (johnson-dsl--body-continuation-p "text [m1]more"))
  (should (johnson-dsl--body-continuation-p (make-string 300 ?x)))
  (should-not (johnson-dsl--body-continuation-p "dureza [p]f[/p]"))
  (should-not (johnson-dsl--body-continuation-p "Enfe[i]š[/i]tillar"))
  (should-not (johnson-dsl--body-continuation-p "beta")))

(ert-deftest johnson-dsl-test-build-index-unindented-body-continuation ()
  "A column-0 body line after a body extends the entry and is reported once."
  (johnson-dsl-test--kill-cache-buffers)
  (let ((path (make-temp-file "johnson-dsl-unindented-" nil ".dsl"))
        (entries nil)
        (messages nil))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "#NAME \"Pauly\"\n#INDEX_LANGUAGE \"German\"\n"
                    "#CONTENTS_LANGUAGE \"German\"\n\n"
                    "alpha\n\t[m1]first body[/m]\n\t\n"
                    "Neugriechisch[/b][/c][/m]</li><li>continuation text\n"
                    "\t[m1]more body[/m]\n"
                    (make-string 300 ?x) "\n"
                    "beta\n\t[m1]second body[/m]\n"
                    "dureza [p]f[/p]\n\t[m1]third body[/m]\n"))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (push (apply #'format-message fmt args) messages))))
            (johnson-dsl-build-index path (lambda (hw offset length)
                                            (push (list hw offset length) entries))))
          (should (equal (mapcar #'car (reverse entries))
                         '("alpha" "beta" "dureza")))
          (let* ((alpha (cl-find "alpha" entries :key #'car :test #'equal))
                 (body (johnson-dsl-retrieve-entry path (nth 1 alpha) (nth 2 alpha))))
            (should (string-match-p "continuation text" body))
            (should (string-match-p "more body" body))
            (should (string-match-p "xxxx" body))
            (should-not (string-match-p "second body" body)))
          (should (= 1 (cl-count-if
                        (lambda (m) (and (string-match-p "2 " m)
                                         (string-match-p (file-name-nondirectory path) m)))
                        messages))))
      (johnson-dsl-test--kill-cache-buffers)
      (delete-file path))))

(provide 'johnson-dsl-test)
;;; johnson-dsl-test.el ends here
