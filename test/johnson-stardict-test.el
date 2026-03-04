;;; johnson-stardict-test.el --- Tests for johnson-stardict -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the johnson-stardict format backend module.

;;; Code:

(require 'ert)
(require 'johnson-stardict)
(require 'johnson)

;;;; Helpers

(defvar johnson-stardict-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-stardict-test--fixture (name)
  "Return the full path to the fixture file NAME."
  (expand-file-name name johnson-stardict-test--fixtures-dir))

(defun johnson-stardict-test--cleanup ()
  "Clean up stardict caches."
  (clrhash johnson-stardict--ifo-cache))

;;;; .ifo parsing

(ert-deftest johnson-stardict-test-parse-ifo ()
  "Parses all fields from a .ifo file."
  (johnson-stardict-test--cleanup)
  (let ((ifo (johnson-stardict--parse-ifo
              (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should (assoc "version" ifo))
    (should (equal (cdr (assoc "version" ifo)) "2.4.2"))
    (should (equal (cdr (assoc "bookname" ifo)) "Test Plain Dictionary"))
    (should (equal (cdr (assoc "wordcount" ifo)) "3"))
    (should (equal (cdr (assoc "idxfilesize" ifo)) "40"))
    (should (equal (cdr (assoc "sametypesequence" ifo)) "m"))
    (should (equal (cdr (assoc "author" ifo)) "Test Author"))
    (should (equal (cdr (assoc "description" ifo))
                   "A test dictionary for unit tests"))))

(ert-deftest johnson-stardict-test-parse-ifo-html ()
  "Parses .ifo file with sametypesequence=h."
  (johnson-stardict-test--cleanup)
  (let ((ifo (johnson-stardict--parse-ifo
              (johnson-stardict-test--fixture "test-stardict-html.ifo"))))
    (should (equal (cdr (assoc "bookname" ifo)) "Test HTML Dictionary"))
    (should (equal (cdr (assoc "sametypesequence" ifo)) "h"))))

(ert-deftest johnson-stardict-test-ifo-get ()
  "Tests the ifo-get helper function."
  (johnson-stardict-test--cleanup)
  (let ((ifo (johnson-stardict--parse-ifo
              (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should (equal (johnson-stardict--ifo-get ifo "bookname")
                   "Test Plain Dictionary"))
    (should (null (johnson-stardict--ifo-get ifo "nonexistent")))))

;;;; Format detection

(ert-deftest johnson-stardict-test-detect-accepts-ifo ()
  "Accepts a valid StarDict .ifo file."
  (should (johnson-stardict-detect
           (johnson-stardict-test--fixture "test-stardict.ifo"))))

(ert-deftest johnson-stardict-test-detect-rejects-non-ifo ()
  "Rejects a file that is not a StarDict .ifo file."
  (let ((non-ifo (make-temp-file "johnson-test-" nil ".ifo")))
    (unwind-protect
        (progn
          (with-temp-file non-ifo
            (insert "This is not a StarDict file.\n"))
          (should-not (johnson-stardict-detect non-ifo)))
      (delete-file non-ifo))))

(ert-deftest johnson-stardict-test-detect-rejects-wrong-extension ()
  "Rejects a file without .ifo extension even if content is valid."
  (let ((wrong-ext (make-temp-file "johnson-test-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file wrong-ext
            (insert "StarDict's dict ifo file\nversion=2.4.2\n"))
          (should-not (johnson-stardict-detect wrong-ext)))
      (delete-file wrong-ext))))

;;;; Metadata parsing

(ert-deftest johnson-stardict-test-parse-metadata ()
  "Parses metadata from a StarDict .ifo file."
  (johnson-stardict-test--cleanup)
  (let ((meta (johnson-stardict-parse-metadata
               (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should (equal (plist-get meta :name) "Test Plain Dictionary"))))

(ert-deftest johnson-stardict-test-parse-metadata-html ()
  "Parses metadata from the HTML fixture."
  (johnson-stardict-test--cleanup)
  (let ((meta (johnson-stardict-parse-metadata
               (johnson-stardict-test--fixture "test-stardict-html.ifo"))))
    (should (equal (plist-get meta :name) "Test HTML Dictionary"))))

;;;; .idx binary parsing

(ert-deftest johnson-stardict-test-parse-idx ()
  "Parses .idx file and returns correct headwords, offsets, and sizes."
  (johnson-stardict-test--cleanup)
  (let ((entries (johnson-stardict--parse-idx
                  (johnson-stardict-test--fixture "test-stardict.ifo"))))
    ;; Should have 3 entries.
    (should (= (length entries) 3))
    ;; Check first entry: "apple"
    (should (equal (nth 0 (aref entries 0)) "apple"))
    (should (= (nth 1 (aref entries 0)) 0))    ; offset
    (should (= (nth 2 (aref entries 0)) 60))   ; size
    ;; Check second entry: "cat"
    (should (equal (nth 0 (aref entries 1)) "cat"))
    (should (= (nth 1 (aref entries 1)) 60))
    (should (= (nth 2 (aref entries 1)) 35))
    ;; Check third entry: "hello"
    (should (equal (nth 0 (aref entries 2)) "hello"))
    (should (= (nth 1 (aref entries 2)) 95))
    (should (= (nth 2 (aref entries 2)) 37))))

(ert-deftest johnson-stardict-test-parse-idx-offsets-valid ()
  "All offsets are non-negative and sizes are positive."
  (johnson-stardict-test--cleanup)
  (let ((entries (johnson-stardict--parse-idx
                  (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (cl-loop for entry across entries
             do (progn
                  (should (>= (nth 1 entry) 0))
                  (should (> (nth 2 entry) 0))))))

(ert-deftest johnson-stardict-test-parse-idx-html ()
  "Parses .idx file for the HTML fixture."
  (johnson-stardict-test--cleanup)
  (let ((entries (johnson-stardict--parse-idx
                  (johnson-stardict-test--fixture "test-stardict-html.ifo"))))
    (should (= (length entries) 3))
    (let ((headwords (cl-loop for entry across entries
                              collect (nth 0 entry))))
      (should (member "bold" headwords))
      (should (member "link" headwords))
      (should (member "colored" headwords)))))

;;;; .syn parsing

(ert-deftest johnson-stardict-test-parse-syn ()
  "Parses .syn file and maps synonyms to correct entries."
  (johnson-stardict-test--cleanup)
  (let* ((idx-entries (johnson-stardict--parse-idx
                       (johnson-stardict-test--fixture "test-stardict.ifo")))
         (syn-entries (johnson-stardict--parse-syn
                       (johnson-stardict-test--fixture "test-stardict.ifo")
                       idx-entries)))
    ;; Should have 2 synonyms.
    (should (= (length syn-entries) 2))
    ;; "hi" should map to "hello"'s offset/size.
    (let ((hi-entry (cl-find "hi" syn-entries :key #'car :test #'equal)))
      (should hi-entry)
      (should (= (nth 1 hi-entry) (nth 1 (aref idx-entries 2))))  ; hello's offset
      (should (= (nth 2 hi-entry) (nth 2 (aref idx-entries 2))))) ; hello's size
    ;; "kitty" should map to "cat"'s offset/size.
    (let ((kitty-entry (cl-find "kitty" syn-entries :key #'car :test #'equal)))
      (should kitty-entry)
      (should (= (nth 1 kitty-entry) (nth 1 (aref idx-entries 1))))
      (should (= (nth 2 kitty-entry) (nth 2 (aref idx-entries 1)))))))

(ert-deftest johnson-stardict-test-no-syn-file ()
  "Returns nil when no .syn file exists."
  (johnson-stardict-test--cleanup)
  (let* ((idx-entries (johnson-stardict--parse-idx
                       (johnson-stardict-test--fixture "test-stardict-html.ifo")))
         (syn-entries (johnson-stardict--parse-syn
                       (johnson-stardict-test--fixture "test-stardict-html.ifo")
                       idx-entries)))
    (should (null syn-entries))))

;;;; Index building

(ert-deftest johnson-stardict-test-build-index ()
  "Builds index from the plain text fixture."
  (johnson-stardict-test--cleanup)
  (let ((entries nil))
    (johnson-stardict-build-index
     (johnson-stardict-test--fixture "test-stardict.ifo")
     (lambda (hw offset size)
       (push (list hw offset size) entries)))
    ;; 3 main entries + 2 synonyms = 5
    (should (= (length entries) 5))
    (let ((headwords (mapcar #'car entries)))
      (should (member "apple" headwords))
      (should (member "cat" headwords))
      (should (member "hello" headwords))
      (should (member "kitty" headwords))
      (should (member "hi" headwords)))))

(ert-deftest johnson-stardict-test-build-index-synonyms-share-offsets ()
  "Synonym entries share the same offsets as their main entries."
  (johnson-stardict-test--cleanup)
  (let ((entries nil))
    (johnson-stardict-build-index
     (johnson-stardict-test--fixture "test-stardict.ifo")
     (lambda (hw offset size)
       (push (list hw offset size) entries)))
    ;; "kitty" should have same offset as "cat".
    (let ((cat-entry (cl-find "cat" entries :key #'car :test #'equal))
          (kitty-entry (cl-find "kitty" entries :key #'car :test #'equal)))
      (should cat-entry)
      (should kitty-entry)
      (should (= (nth 1 cat-entry) (nth 1 kitty-entry)))
      (should (= (nth 2 cat-entry) (nth 2 kitty-entry))))))

;;;; Entry retrieval (plain .dict file)

(ert-deftest johnson-stardict-test-retrieve-entry-plain ()
  "Retrieves an entry from a plain .dict file."
  (johnson-stardict-test--cleanup)
  (let ((raw (johnson-stardict-retrieve-entry
              (johnson-stardict-test--fixture "test-stardict.ifo")
              0 60)))
    (should (stringp raw))
    (let ((text (decode-coding-string raw 'utf-8)))
      (should (string-match-p "round fruit" text)))))

(ert-deftest johnson-stardict-test-retrieve-entry-second ()
  "Retrieves the second entry correctly."
  (johnson-stardict-test--cleanup)
  (let ((raw (johnson-stardict-retrieve-entry
              (johnson-stardict-test--fixture "test-stardict.ifo")
              60 35)))
    (should (stringp raw))
    (let ((text (decode-coding-string raw 'utf-8)))
      (should (string-match-p "feline" text)))))

(ert-deftest johnson-stardict-test-retrieve-sets-sametypesequence ()
  "retrieve-entry sets the sametypesequence variable."
  (johnson-stardict-test--cleanup)
  (setq johnson-stardict--current-sametypesequence nil)
  (johnson-stardict-retrieve-entry
   (johnson-stardict-test--fixture "test-stardict.ifo")
   0 60)
  (should (equal johnson-stardict--current-sametypesequence "m")))

;;;; Rendering: type m (plain text)

(ert-deftest johnson-stardict-test-render-type-m ()
  "Renders plain text content correctly."
  (with-temp-buffer
    (let ((data (encode-coding-string "A simple definition." 'utf-8)))
      (johnson-stardict--render-type-m data)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "A simple definition.")))))

;;;; Rendering: type t (phonetic)

(ert-deftest johnson-stardict-test-render-type-t ()
  "Renders phonetic transcription in brackets with italic face."
  (with-temp-buffer
    (let ((data (encode-coding-string "helo" 'utf-8)))
      (johnson-stardict--render-type-t data)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "\\[helo\\]" text)))
      ;; Check for italic face.
      (goto-char (point-min))
      (let ((face (get-text-property (point) 'face)))
        (should (or (eq face 'johnson-italic-face)
                    (and (listp face)
                         (memq 'johnson-italic-face face))))))))

;;;; Rendering: type h (HTML tags -> text properties)

(ert-deftest johnson-stardict-test-render-type-h-bold ()
  "HTML <b> tags apply johnson-bold-face."
  (with-temp-buffer
    (let ((data (encode-coding-string "<b>bold text</b> normal" 'utf-8)))
      (johnson-stardict--render-type-h data)
      ;; Tags should be removed.
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "<b>" text))
        (should-not (string-match-p "</b>" text))
        (should (string-match-p "bold text" text)))
      ;; Bold face should be applied.
      (goto-char (point-min))
      (let* ((bold-start (text-property-any (point-min) (point-max)
                                            'face 'johnson-bold-face)))
        (should bold-start)))))

(ert-deftest johnson-stardict-test-render-type-h-italic ()
  "HTML <i> tags apply johnson-italic-face."
  (with-temp-buffer
    (let ((data (encode-coding-string "<i>italic</i> text" 'utf-8)))
      (johnson-stardict--render-type-h data)
      (let* ((pos (text-property-any (point-min) (point-max)
                                     'face 'johnson-italic-face)))
        (should pos)))))

(ert-deftest johnson-stardict-test-render-type-h-link ()
  "HTML <a href> tags create cross-reference buttons."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "See <a href=\"bword://test\">test</a> entry." 'utf-8)))
      (johnson-stardict--render-type-h data)
      ;; Should have a button.
      (goto-char (point-min))
      (let ((btn (next-button (point-min))))
        (should btn)
        (should (equal (button-label btn) "test"))))))

(ert-deftest johnson-stardict-test-render-type-h-br ()
  "HTML <br> tags are converted to newlines."
  (with-temp-buffer
    (let ((data (encode-coding-string "line1<br>line2<br/>line3" 'utf-8)))
      (johnson-stardict--render-type-h data)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "line1\nline2\nline3" text))))))

(ert-deftest johnson-stardict-test-render-type-h-font-color ()
  "HTML <font color> tags apply color faces."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "<font color=\"blue\">blue text</font>" 'utf-8)))
      (johnson-stardict--render-type-h data)
      (goto-char (point-min))
      (let ((found nil)
            (pos (point-min)))
        (while (and (< pos (point-max)) (not found))
          (let ((face (get-text-property pos 'face)))
            (when (and face (or (eq face 'johnson-color-blue-face)
                                (and (listp face)
                                     (memq 'johnson-color-blue-face face))))
              (setq found t)))
          (setq pos (1+ pos)))
        (should found)))))

(ert-deftest johnson-stardict-test-render-type-h-tags-removed ()
  "All HTML tags are removed from the rendered output."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "<b>bold</b> <i>italic</i> <u>underline</u>" 'utf-8)))
      (johnson-stardict--render-type-h data)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "<" text))
        (should-not (string-match-p ">" text))
        (should (string-match-p "bold" text))
        (should (string-match-p "italic" text))
        (should (string-match-p "underline" text))))))

(ert-deftest johnson-stardict-test-render-type-h-sup ()
  "HTML <sup> tags set display properties for superscript."
  (with-temp-buffer
    (let ((data (encode-coding-string "H<sup>2</sup>O" 'utf-8)))
      (johnson-stardict--render-type-h data)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (equal text "H2O")))
      ;; Check the "2" has a display property.
      (goto-char (1+ (point-min)))  ; position of "2"
      (let ((display (get-text-property (point) 'display)))
        (should display)))))

;;;; Rendering: type x (XDXF)

(ert-deftest johnson-stardict-test-render-type-x-kref ()
  "XDXF <kref> tags create cross-reference buttons."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "See also <kref>example</kref> for details." 'utf-8)))
      (johnson-stardict--render-type-x data)
      (goto-char (point-min))
      (let ((btn (next-button (point-min))))
        (should btn)
        (should (equal (button-label btn) "example"))))))

(ert-deftest johnson-stardict-test-render-type-x-example ()
  "XDXF <ex> tags apply example face."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "<ex>This is an example.</ex>" 'utf-8)))
      (johnson-stardict--render-type-x data)
      (goto-char (point-min))
      (let ((found nil)
            (pos (point-min)))
        (while (and (< pos (point-max)) (not found))
          (let ((face (get-text-property pos 'face)))
            (when (and face (or (eq face 'johnson-example-face)
                                (and (listp face)
                                     (memq 'johnson-example-face face))))
              (setq found t)))
          (setq pos (1+ pos)))
        (should found)))))

(ert-deftest johnson-stardict-test-render-type-x-grammar ()
  "XDXF <gr> tags apply italic face."
  (with-temp-buffer
    (let ((data (encode-coding-string "<gr>noun</gr>" 'utf-8)))
      (johnson-stardict--render-type-x data)
      (goto-char (point-min))
      (let ((pos (text-property-any (point-min) (point-max)
                                    'face 'johnson-italic-face)))
        (should pos)))))

(ert-deftest johnson-stardict-test-render-type-x-tags-removed ()
  "All XDXF tags are removed from the rendered output."
  (with-temp-buffer
    (let ((data (encode-coding-string
                 "<k>word</k><gr>noun</gr><dtrn>meaning</dtrn>" 'utf-8)))
      (johnson-stardict--render-type-x data)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should-not (string-match-p "<" text))
        (should-not (string-match-p ">" text))
        (should (string-match-p "word" text))
        (should (string-match-p "noun" text))
        (should (string-match-p "meaning" text))))))

;;;; Rendering: type g (Pango markup)

(ert-deftest johnson-stardict-test-render-type-g-bold ()
  "Pango <b> tags apply bold face."
  (with-temp-buffer
    (let ((data (encode-coding-string "<b>pango bold</b>" 'utf-8)))
      (johnson-stardict--render-type-g data)
      (let ((pos (text-property-any (point-min) (point-max)
                                    'face 'johnson-bold-face)))
        (should pos)))))

;;;; Rendering: multiple types in sametypesequence

(ert-deftest johnson-stardict-test-render-sametypesequence-tm ()
  "Renders multi-type sametypesequence (tm = phonetic + plain text)."
  (johnson-stardict-test--cleanup)
  ;; Retrieve the first entry from the multi fixture.
  (let ((raw (johnson-stardict-retrieve-entry
              (johnson-stardict-test--fixture "test-stardict-multi.ifo")
              0
              ;; "water" entry: phonetic null-terminated + plain text remainder
              ;; phonetic: ˈwɔː.tɚ (UTF-8 encoded) + \0 + definition
              46)))
    (should (equal johnson-stardict--current-sametypesequence "tm"))
    (with-temp-buffer
      (johnson-stardict-render-entry raw)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        ;; Should contain bracketed phonetic transcription.
        (should (string-match-p "\\[" text))
        (should (string-match-p "\\]" text))
        ;; Should contain the definition.
        (should (string-match-p "clear liquid" text))))))

;;;; Field splitting

(ert-deftest johnson-stardict-test-split-fields-sametypesequence-single ()
  "Splits a single-type sametypesequence correctly."
  (let* ((data (encode-coding-string "hello world" 'utf-8))
         (fields (johnson-stardict--split-fields-sametypesequence data "m")))
    (should (= (length fields) 1))
    (should (= (car (car fields)) ?m))
    (should (equal (cdr (car fields)) data))))

(ert-deftest johnson-stardict-test-split-fields-sametypesequence-multi ()
  "Splits a multi-type sametypesequence (tm) correctly."
  (let* ((phonetic (encode-coding-string "fon" 'utf-8))
         (definition (encode-coding-string "meaning" 'utf-8))
         ;; t field is null-terminated (not last), m field extends to end.
         (data (concat phonetic (string 0) definition))
         (fields (johnson-stardict--split-fields-sametypesequence data "tm")))
    (should (= (length fields) 2))
    ;; First field: type t.
    (should (= (car (nth 0 fields)) ?t))
    (should (equal (cdr (nth 0 fields)) phonetic))
    ;; Second field: type m (last, no null terminator).
    (should (= (car (nth 1 fields)) ?m))
    (should (equal (cdr (nth 1 fields)) definition))))

(ert-deftest johnson-stardict-test-split-fields-no-sametypesequence ()
  "Splits data without sametypesequence (per-entry type markers)."
  (let* ((text (encode-coding-string "A common fruit." 'utf-8))
         ;; Format: type byte + null-terminated data.
         (data (concat (string ?m) text (string 0)))
         (fields (johnson-stardict--split-fields-no-sametypesequence data)))
    (should (= (length fields) 1))
    (should (= (car (car fields)) ?m))
    (should (equal (cdr (car fields)) text))))

;;;; Rendering entries without sametypesequence

(ert-deftest johnson-stardict-test-render-no-sametypesequence ()
  "Renders entries without sametypesequence (per-entry type markers)."
  (johnson-stardict-test--cleanup)
  ;; The noseq fixture has apple: m + "A common fruit." + \0
  (let ((raw (johnson-stardict-retrieve-entry
              (johnson-stardict-test--fixture "test-stardict-noseq.ifo")
              0
              17)))  ; m(1) + "A common fruit."(15) + \0(1)
    (should (null johnson-stardict--current-sametypesequence))
    (with-temp-buffer
      (johnson-stardict-render-entry raw)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "A common fruit" text))))))

;;;; Full integration: parse -> index -> retrieve -> render

(ert-deftest johnson-stardict-test-full-integration ()
  "Full round-trip: detect, parse metadata, build index, retrieve, render."
  (johnson-stardict-test--cleanup)
  (let* ((ifo-path (johnson-stardict-test--fixture "test-stardict.ifo")))
    ;; 1. Detect.
    (should (johnson-stardict-detect ifo-path))
    ;; 2. Parse metadata.
    (let ((meta (johnson-stardict-parse-metadata ifo-path)))
      (should (equal (plist-get meta :name) "Test Plain Dictionary")))
    ;; 3. Build index.
    (let ((entries nil))
      (johnson-stardict-build-index ifo-path
                                    (lambda (hw offset size)
                                      (push (list hw offset size) entries)))
      (should (>= (length entries) 3))
      ;; 4. Retrieve and render "apple".
      (let* ((apple-entry (cl-find "apple" entries :key #'car :test #'equal)))
        (should apple-entry)
        (let ((raw (johnson-stardict-retrieve-entry
                    ifo-path (nth 1 apple-entry) (nth 2 apple-entry))))
          (with-temp-buffer
            (johnson-stardict-render-entry raw)
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (should (string-match-p "round fruit" text)))))))))

(ert-deftest johnson-stardict-test-full-integration-html ()
  "Full round-trip with HTML content type."
  (johnson-stardict-test--cleanup)
  (let* ((ifo-path (johnson-stardict-test--fixture "test-stardict-html.ifo"))
         (entries nil))
    (johnson-stardict-build-index ifo-path
                                  (lambda (hw offset size)
                                    (push (list hw offset size) entries)))
    ;; Retrieve and render "bold".
    (let* ((bold-entry (cl-find "bold" entries :key #'car :test #'equal)))
      (should bold-entry)
      (let ((raw (johnson-stardict-retrieve-entry
                  ifo-path (nth 1 bold-entry) (nth 2 bold-entry))))
        (with-temp-buffer
          (johnson-stardict-render-entry raw)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            ;; HTML tags should be removed.
            (should-not (string-match-p "<b>" text))
            (should (string-match-p "bold text" text))
            (should (string-match-p "normal" text)))
          ;; Bold face should be applied.
          (let ((pos (text-property-any (point-min) (point-max)
                                        'face 'johnson-bold-face)))
            (should pos)))))))

(ert-deftest johnson-stardict-test-full-integration-xdxf ()
  "Full round-trip with XDXF content type."
  (johnson-stardict-test--cleanup)
  (let* ((ifo-path (johnson-stardict-test--fixture "test-stardict-xdxf.ifo"))
         (entries nil))
    (johnson-stardict-build-index ifo-path
                                  (lambda (hw offset size)
                                    (push (list hw offset size) entries)))
    ;; Retrieve and render "crossref".
    (let* ((ref-entry (cl-find "crossref" entries :key #'car :test #'equal)))
      (should ref-entry)
      (let ((raw (johnson-stardict-retrieve-entry
                  ifo-path (nth 1 ref-entry) (nth 2 ref-entry))))
        (with-temp-buffer
          (johnson-stardict-render-entry raw)
          ;; Should have a button for the kref.
          (goto-char (point-min))
          (let ((btn (next-button (point-min))))
            (should btn)
            (should (equal (button-label btn) "example"))))))))

(ert-deftest johnson-stardict-test-full-integration-synonym ()
  "Full round-trip: synonym lookup returns the same content as main entry."
  (johnson-stardict-test--cleanup)
  (let* ((ifo-path (johnson-stardict-test--fixture "test-stardict.ifo"))
         (entries nil))
    (johnson-stardict-build-index ifo-path
                                  (lambda (hw offset size)
                                    (push (list hw offset size) entries)))
    ;; "kitty" (synonym) should have the same content as "cat".
    (let* ((cat-entry (cl-find "cat" entries :key #'car :test #'equal))
           (kitty-entry (cl-find "kitty" entries :key #'car :test #'equal)))
      (should cat-entry)
      (should kitty-entry)
      (let ((cat-raw (johnson-stardict-retrieve-entry
                      ifo-path (nth 1 cat-entry) (nth 2 cat-entry)))
            (kitty-raw (johnson-stardict-retrieve-entry
                        ifo-path (nth 1 kitty-entry) (nth 2 kitty-entry))))
        (should (equal cat-raw kitty-raw))))))

;;;; Path resolution

(ert-deftest johnson-stardict-test-base-path ()
  "Computes base path correctly."
  (should (equal (johnson-stardict--base-path "/foo/bar/dict.ifo")
                 "/foo/bar/dict")))

(ert-deftest johnson-stardict-test-idx-path ()
  "Resolves .idx path for existing fixture."
  (let ((path (johnson-stardict--idx-path
               (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should (string-suffix-p ".idx" path))
    (should (file-exists-p path))))

(ert-deftest johnson-stardict-test-dict-path ()
  "Resolves .dict path for existing fixture."
  (let ((path (johnson-stardict--dict-path
               (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should (string-suffix-p ".dict" path))
    (should (file-exists-p path))))

(ert-deftest johnson-stardict-test-syn-path-exists ()
  "Returns .syn path when file exists."
  (let ((path (johnson-stardict--syn-path
               (johnson-stardict-test--fixture "test-stardict.ifo"))))
    (should path)
    (should (file-exists-p path))))

(ert-deftest johnson-stardict-test-syn-path-nil-when-absent ()
  "Returns nil when .syn file does not exist."
  (let ((path (johnson-stardict--syn-path
               (johnson-stardict-test--fixture "test-stardict-html.ifo"))))
    (should (null path))))

;;;; Auto-detection of 64-bit .idx format

(ert-deftest johnson-stardict-test-parse-idx-64bit ()
  "Parses .idx file with 8-byte offsets and 8-byte sizes (no idxoffsetbits=64)."
  (johnson-stardict-test--cleanup)
  (let ((entries (johnson-stardict--parse-idx
                  (johnson-stardict-test--fixture "test-stardict-64bit.ifo"))))
    ;; Should detect the 64-bit format and return 3 entries.
    (should (= (length entries) 3))
    (should (equal (nth 0 (aref entries 0)) "apple"))
    (should (= (nth 1 (aref entries 0)) 0))
    (should (= (nth 2 (aref entries 0)) 60))
    (should (equal (nth 0 (aref entries 1)) "cat"))
    (should (= (nth 1 (aref entries 1)) 60))
    (should (= (nth 2 (aref entries 1)) 35))
    (should (equal (nth 0 (aref entries 2)) "hello"))
    (should (= (nth 1 (aref entries 2)) 95))
    (should (= (nth 2 (aref entries 2)) 37))))

(ert-deftest johnson-stardict-test-full-integration-64bit ()
  "Full round-trip with 64-bit .idx format."
  (johnson-stardict-test--cleanup)
  (let* ((ifo-path (johnson-stardict-test--fixture "test-stardict-64bit.ifo"))
         (entries nil))
    (johnson-stardict-build-index ifo-path
                                  (lambda (hw offset size)
                                    (push (list hw offset size) entries)))
    (should (= (length entries) 3))
    ;; Retrieve and render "apple".
    (let* ((apple-entry (cl-find "apple" entries :key #'car :test #'equal)))
      (should apple-entry)
      (let ((raw (johnson-stardict-retrieve-entry
                  ifo-path (nth 1 apple-entry) (nth 2 apple-entry))))
        (with-temp-buffer
          (johnson-stardict-render-entry raw)
          (let ((text (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-match-p "round fruit" text))))))))

(ert-deftest johnson-stardict-test-missing-idx-error ()
  "Signals a clear error when .idx file is missing."
  (johnson-stardict-test--cleanup)
  (let ((temp-dir (make-temp-file "johnson-test-" t)))
    (unwind-protect
        (let ((ifo-path (expand-file-name "missing.ifo" temp-dir)))
          (with-temp-file ifo-path
            (insert "StarDict's dict ifo file\nversion=2.4.2\n"
                    "wordcount=1\nidxfilesize=10\nbookname=Missing\n"))
          (should-error (johnson-stardict--load-idx-data ifo-path)
                        :type 'error))
      (delete-directory temp-dir t))))

(provide 'johnson-stardict-test)
;;; johnson-stardict-test.el ends here
