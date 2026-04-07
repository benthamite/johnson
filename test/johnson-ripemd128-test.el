;;; johnson-ripemd128-test.el --- Tests for johnson-ripemd128 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>

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

;; ERT tests for the RIPEMD-128 hash implementation, verified against the
;; official test vectors from the RIPEMD-128 specification.

;;; Code:

(require 'ert)
(require 'johnson-ripemd128)

;;;; Official test vectors

(ert-deftest johnson-ripemd128-test-empty ()
  "Hash of empty string."
  (should (equal (johnson-ripemd128-hash-hex "")
                 "cdf26213a150dc3ecb610f18f6b38b46")))

(ert-deftest johnson-ripemd128-test-a ()
  "Hash of \"a\"."
  (should (equal (johnson-ripemd128-hash-hex "a")
                 "86be7afa339d0fc7cfc785e72f578d33")))

(ert-deftest johnson-ripemd128-test-abc ()
  "Hash of \"abc\"."
  (should (equal (johnson-ripemd128-hash-hex "abc")
                 "c14a12199c66e4ba84636b0f69144c77")))

(ert-deftest johnson-ripemd128-test-message-digest ()
  "Hash of \"message digest\"."
  (should (equal (johnson-ripemd128-hash-hex "message digest")
                 "9e327b3d6e523062afc1132d7df9d1b8")))

(ert-deftest johnson-ripemd128-test-alpha ()
  "Hash of the lowercase alphabet."
  (should (equal (johnson-ripemd128-hash-hex "abcdefghijklmnopqrstuvwxyz")
                 "fd2aa607f71dc8f510714922b371834e")))

(ert-deftest johnson-ripemd128-test-alphanum ()
  "Hash of upper + lower + digits."
  (should (equal (johnson-ripemd128-hash-hex
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
                 "d1e959eb179c911faea4624c60c5c702")))

(ert-deftest johnson-ripemd128-test-numeric-repeat ()
  "Hash of \"1234567890\" repeated 8 times."
  (should (equal (johnson-ripemd128-hash-hex
                  (apply #'concat (make-list 8 "1234567890")))
                 "3f45ef194732c2dbb2c4a2c769795fa3")))

;;;; Auxiliary tests

(ert-deftest johnson-ripemd128-test-returns-unibyte ()
  "Hash output is a 16-byte unibyte string."
  (let ((hash (johnson-ripemd128-hash "")))
    (should (= (length hash) 16))
    (should-not (multibyte-string-p hash))))

(ert-deftest johnson-ripemd128-test-rejects-multibyte ()
  "Passing a multibyte string to `johnson-ripemd128-hash' signals an error."
  (should-error (johnson-ripemd128-hash "café")))

(provide 'johnson-ripemd128-test)
;;; johnson-ripemd128-test.el ends here
