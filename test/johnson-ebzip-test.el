;;; johnson-ebzip-test.el --- Tests for johnson-ebzip -*- lexical-binding: t; -*-

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

;; ERT tests for the johnson-ebzip module.  The EBZIP files are built
;; in memory: a 22-byte header, a slice index, and slice data.

;;; Code:

(require 'ert)
(require 'johnson-ebzip)

;;;; Helpers

(defun johnson-ebzip-test--write-bytes (bytes)
  "Write the unibyte string BYTES to a new temporary file and return its path."
  (let ((path (make-temp-file "johnson-ebzip-test-" nil ".ebz"))
        (coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert bytes))
    path))

(defun johnson-ebzip-test--header (file-size &optional zip-level)
  "Return a 22-byte EBZIP header declaring FILE-SIZE and ZIP-LEVEL."
  (concat "EBZip"
          (unibyte-string (logior #x10 (or zip-level 0)) 0 0 0)
          (unibyte-string (logand (ash file-size -32) #xff)
                          (logand (ash file-size -24) #xff)
                          (logand (ash file-size -16) #xff)
                          (logand (ash file-size -8) #xff)
                          (logand file-size #xff))
          (make-string 8 0)))

(defmacro johnson-ebzip-test--with-file (var bytes &rest body)
  "Bind VAR to a temporary file holding BYTES and evaluate BODY.
Caches are cleared before BODY runs and the file is deleted afterwards."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((,var (johnson-ebzip-test--write-bytes ,bytes)))
     (johnson-ebzip-clear-caches)
     (unwind-protect
         (progn ,@body)
       (delete-file ,var))))

;;;; Reading

(ert-deftest johnson-ebzip-test-read-raw-slice ()
  "Reads data back from a file whose single slice is stored uncompressed."
  (let* ((payload "hello ebz\n")
         (slice (concat payload (make-string (- 2048 (length payload)) 0)))
         (index (unibyte-string 0 26 (logand (ash (+ 26 2048) -8) #xff)
                                (logand (+ 26 2048) #xff))))
    (johnson-ebzip-test--with-file path
        (concat (johnson-ebzip-test--header (length payload)) index slice)
      (should (= (johnson-ebzip-uncompressed-size path) (length payload)))
      (should (equal (johnson-ebzip-read path 0 (length payload)) payload)))))

;;;; Header validation

(ert-deftest johnson-ebzip-test-forged-size-rejected-before-allocation ()
  "A header declaring more slices than the file can index is rejected."
  (johnson-ebzip-test--with-file path
      (johnson-ebzip-test--header (1- (ash 1 32)))
    (let ((err (should-error (johnson-ebzip-uncompressed-size path))))
      (should (string-match-p "slice index does not fit" (cadr err))))))

(ert-deftest johnson-ebzip-test-unsupported-zip-level-rejected ()
  "A header with a compression level above 5 is rejected."
  (johnson-ebzip-test--with-file path
      (johnson-ebzip-test--header 100 6)
    (let ((err (should-error (johnson-ebzip-uncompressed-size path))))
      (should (string-match-p "compression level 6" (cadr err))))))

(provide 'johnson-ebzip-test)
;;; johnson-ebzip-test.el ends here
