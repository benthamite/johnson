;;; johnson-protocol-test.el --- Tests for johnson-protocol -*- lexical-binding: t; -*-

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

;; ERT tests for the johnson-protocol wire codec module.

;;; Code:

(require 'ert)
(require 'johnson-protocol)

;;;; Entry round-trips and chunking

(ert-deftest johnson-protocol-test-unibyte-entry-roundtrip ()
  (let* ((raw (unibyte-string 0 128 255 65))
         (frames (johnson-protocol-entry-frames
                  '(:lookup 7 :dictionary 2 :entry 0)
                  (list :raw raw :context '(:kind binary))))
         (decoded (mapcar #'johnson-protocol-decode frames)))
    (should-not (multibyte-string-p raw))
    (should (equal (plist-get
                    (johnson-protocol-assemble-entry decoded) :raw)
                   raw))
    (should-not
     (multibyte-string-p
      (plist-get (johnson-protocol-assemble-entry decoded) :raw)))))

(ert-deftest johnson-protocol-test-multibyte-entry-roundtrip ()
  (let* ((raw "café — 家")
         (frames (johnson-protocol-entry-frames
                  '(:lookup 7 :dictionary 2 :entry 1)
                  (list :raw raw :context '(:kind text)))))
    (should (equal
             (plist-get
              (johnson-protocol-assemble-entry
               (mapcar #'johnson-protocol-decode frames))
              :raw)
             raw))))

(ert-deftest johnson-protocol-test-entry-splits-at-32-kibibytes ()
  (let ((frames (johnson-protocol-entry-frames
                 '(:lookup 9 :dictionary 4 :entry 3)
                 (list :raw (make-string 32769 ?x) :context nil))))
    (should (= (length frames) 2))
    (should (equal (mapcar
                    (lambda (line)
                      (plist-get (johnson-protocol-decode line) :chunk))
                    frames)
                   '(0 1)))))

(ert-deftest johnson-protocol-test-oversized-context-is-chunked ()
  (let* ((context (list :resources
                        (list (cons "large"
                                    (make-string (* 4 32768) ?r)))))
         (packet (list :raw "entry" :context context))
         (frames (johnson-protocol-entry-frames
                  '(:lookup 10 :dictionary 5 :entry 0) packet)))
    (should (> (length frames) 1))
    (should (equal
             (johnson-protocol-assemble-entry
              (mapcar #'johnson-protocol-decode frames))
             packet))))

;;;; Malformed frames

(ert-deftest johnson-protocol-test-decode-rejects-unframed-noise ()
  (should-error (johnson-protocol-decode "noise\n")
                :type 'johnson-protocol-error))

(ert-deftest johnson-protocol-test-decode-rejects-unknown-version ()
  (should-error
   (johnson-protocol-decode
    (replace-regexp-in-string
     "JOHNSON/1 " "JOHNSON/2 "
     (johnson-protocol-encode '(:type ready))))
   :type 'johnson-protocol-error))

(ert-deftest johnson-protocol-test-assemble-rejects-missing-first-chunk ()
  (should-error
   (johnson-protocol-assemble-entry
    (list '(:lookup 1 :dictionary 0 :entry 0 :chunk 1 :chunks 2
            :multibyte nil :data "eA==")))
   :type 'johnson-protocol-error))

(provide 'johnson-protocol-test)
;;; johnson-protocol-test.el ends here
