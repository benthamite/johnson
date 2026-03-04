;;; johnson-resource-test.el --- Tests for resource extraction -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for companion zip archive extraction and audio resolution.

;;; Code:

(require 'ert)
(require 'johnson)

;;;; Helpers

(defvar johnson-resource-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory (or load-file-name
                                             buffer-file-name
                                             default-directory)))
  "Path to the test fixtures directory.")

(defun johnson-resource-test--make-fixture-zip (dir)
  "Create a test `.dsl.files.zip' archive in DIR.
The archive contains a single file `test.mp3' with dummy content.
Returns the path to the zip file."
  (let* ((dsl-path (expand-file-name "test-audio.dsl" dir))
         (zip-path (concat dsl-path ".files.zip"))
         (content-file (expand-file-name "test.mp3" dir)))
    ;; Create dummy DSL file.
    (with-temp-file dsl-path
      (insert "#NAME \"Test Audio Dict\"\n"))
    ;; Create dummy audio file to add to zip.
    (with-temp-file content-file
      (insert "FAKE-MP3-DATA"))
    ;; Create the zip archive.
    (let ((default-directory dir))
      (call-process "zip" nil nil nil "-j" zip-path content-file))
    ;; Clean up the loose mp3 file so resolution must go through the zip.
    (delete-file content-file)
    (list dsl-path zip-path)))

;;;; johnson--resource-zip-path

(ert-deftest johnson-resource-test-zip-path-found ()
  "Finds companion zip when it exists."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (paths (johnson-resource-test--make-fixture-zip dir))
         (dsl-path (nth 0 paths)))
    (unwind-protect
        (let ((zip (johnson--resource-zip-path dsl-path)))
          (should zip)
          (should (string-suffix-p ".dsl.files.zip" zip))
          (should (file-exists-p zip)))
      (delete-directory dir t))))

(ert-deftest johnson-resource-test-zip-path-not-found ()
  "Returns nil when no companion zip exists."
  (should-not (johnson--resource-zip-path "/nonexistent/dict.dsl")))

(ert-deftest johnson-resource-test-zip-path-strips-dz ()
  "Strips .dz extension before appending .files.zip."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (dsl-path (expand-file-name "test-audio.dsl" dir))
         (dz-path (concat dsl-path ".dz"))
         (zip-path (concat dsl-path ".files.zip")))
    (unwind-protect
        (progn
          ;; Create just the zip file.
          (with-temp-file zip-path (insert "PK dummy"))
          (let ((result (johnson--resource-zip-path dz-path)))
            (should result)
            (should (string= result zip-path))))
      (delete-directory dir t))))

;;;; johnson--extract-resource

(ert-deftest johnson-resource-test-extract ()
  "Extracts a file from a companion zip to the cache."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (cache-dir (make-temp-file "johnson-res-cache-" t))
         (johnson-cache-directory cache-dir)
         (paths (johnson-resource-test--make-fixture-zip dir))
         (zip-path (nth 1 paths)))
    (unwind-protect
        (let ((cached (johnson--extract-resource zip-path "test.mp3")))
          (should cached)
          (should (file-exists-p cached))
          (should (string-match-p "resources/" cached))
          ;; Verify content.
          (with-temp-buffer
            (insert-file-contents cached)
            (should (string= (buffer-string) "FAKE-MP3-DATA"))))
      (delete-directory dir t)
      (delete-directory cache-dir t))))

(ert-deftest johnson-resource-test-extract-cached ()
  "Second extraction returns cached file without re-extracting."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (cache-dir (make-temp-file "johnson-res-cache-" t))
         (johnson-cache-directory cache-dir)
         (paths (johnson-resource-test--make-fixture-zip dir))
         (zip-path (nth 1 paths)))
    (unwind-protect
        (let ((first (johnson--extract-resource zip-path "test.mp3")))
          (should first)
          ;; Delete the zip so extraction would fail if attempted.
          (delete-file zip-path)
          (let ((second (johnson--extract-resource zip-path "test.mp3")))
            (should second)
            (should (string= first second))))
      (delete-directory dir t)
      (delete-directory cache-dir t))))

(ert-deftest johnson-resource-test-extract-missing-file ()
  "Returns nil when the file is not in the zip."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (cache-dir (make-temp-file "johnson-res-cache-" t))
         (johnson-cache-directory cache-dir)
         (paths (johnson-resource-test--make-fixture-zip dir))
         (zip-path (nth 1 paths)))
    (unwind-protect
        (should-not (johnson--extract-resource zip-path "nonexistent.mp3"))
      (delete-directory dir t)
      (delete-directory cache-dir t))))

;;;; johnson--resolve-audio-file

(ert-deftest johnson-resource-test-resolve-existing-file ()
  "Returns path unchanged when audio file exists on disk."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (audio (expand-file-name "test.mp3" dir)))
    (unwind-protect
        (progn
          (with-temp-file audio (insert "data"))
          (should (string= (johnson--resolve-audio-file audio nil) audio)))
      (delete-directory dir t))))

(ert-deftest johnson-resource-test-resolve-from-zip ()
  "Extracts from companion zip when file doesn't exist on disk."
  (let* ((dir (make-temp-file "johnson-res-test-" t))
         (cache-dir (make-temp-file "johnson-res-cache-" t))
         (johnson-cache-directory cache-dir)
         (paths (johnson-resource-test--make-fixture-zip dir))
         (dsl-path (nth 0 paths))
         (audio-path (expand-file-name "test.mp3" dir)))
    (unwind-protect
        (let ((resolved (johnson--resolve-audio-file audio-path dsl-path)))
          (should resolved)
          (should (file-exists-p resolved))
          (should (string-match-p "resources/" resolved)))
      (delete-directory dir t)
      (delete-directory cache-dir t))))

(ert-deftest johnson-resource-test-resolve-nil-without-dict ()
  "Returns nil when file doesn't exist and no dict-path given."
  (should-not (johnson--resolve-audio-file "/nonexistent/test.mp3" nil)))

;;;; johnson-insert-audio-button

(ert-deftest johnson-resource-test-audio-button-properties ()
  "Audio button stores dict-path as a text property."
  (with-temp-buffer
    (johnson-insert-audio-button "/tmp/test.mp3" nil "/tmp/dict.dsl")
    (goto-char (point-min))
    (should (get-text-property (point) 'johnson-audio-file))
    (should (string= (get-text-property (point) 'johnson-audio-file)
                      "/tmp/test.mp3"))
    (should (string= (get-text-property (point) 'johnson-audio-dict-path)
                      "/tmp/dict.dsl"))))

(ert-deftest johnson-resource-test-audio-button-no-dict-path ()
  "Audio button works without dict-path (nil property)."
  (with-temp-buffer
    (johnson-insert-audio-button "/tmp/test.mp3")
    (goto-char (point-min))
    (should (get-text-property (point) 'johnson-audio-file))
    (should-not (get-text-property (point) 'johnson-audio-dict-path))))

;;;; johnson-clear-resource-cache

(ert-deftest johnson-resource-test-clear-cache ()
  "Clears the resource cache directory."
  (let* ((cache-dir (make-temp-file "johnson-res-cache-" t))
         (johnson-cache-directory cache-dir)
         (res-dir (expand-file-name "resources" cache-dir)))
    (unwind-protect
        (progn
          (make-directory res-dir t)
          (with-temp-file (expand-file-name "dummy" res-dir)
            (insert "data"))
          (should (file-directory-p res-dir))
          (johnson-clear-resource-cache)
          (should-not (file-directory-p res-dir)))
      (when (file-directory-p cache-dir)
        (delete-directory cache-dir t)))))

(provide 'johnson-resource-test)
;;; johnson-resource-test.el ends here
