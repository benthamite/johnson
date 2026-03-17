;;; johnson-binary.el --- Shared binary integer helpers for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

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
