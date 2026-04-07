;;; johnson-binary.el --- Shared binary integer helpers for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

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

;; Shared helpers for reading big-endian integers from unibyte strings.
;; Used by all binary format backends (BGL, MDict, StarDict, EPWING, EBZIP).

;;; Code:

;;;; Big-endian integer readers — from unibyte strings

(defsubst johnson-binary-u16be (data pos)
  "Read unsigned 16-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 8)
          (aref data (1+ pos))))

(defsubst johnson-binary-u32be (data pos)
  "Read unsigned 32-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 24)
          (ash (aref data (1+ pos)) 16)
          (ash (aref data (+ pos 2)) 8)
          (aref data (+ pos 3))))

(defsubst johnson-binary-u64be (data pos)
  "Read unsigned 64-bit big-endian integer from DATA at POS."
  (logior (ash (aref data pos) 56)
          (ash (aref data (1+ pos)) 48)
          (ash (aref data (+ pos 2)) 40)
          (ash (aref data (+ pos 3)) 32)
          (ash (aref data (+ pos 4)) 24)
          (ash (aref data (+ pos 5)) 16)
          (ash (aref data (+ pos 6)) 8)
          (aref data (+ pos 7))))

(provide 'johnson-binary)

;;; johnson-binary.el ends here
