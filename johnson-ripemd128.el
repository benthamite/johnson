;;; johnson-ripemd128.el --- RIPEMD-128 hash for johnson -*- lexical-binding: t; -*-

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Pure Elisp RIPEMD-128 hash implementation.  Used by the MDict format
;; backend for decrypting keyword indices (Encrypted="2").  Performance
;; is irrelevant since the hash is computed only once per dictionary.

;;; Code:

(require 'cl-lib)

;;;; 32-bit arithmetic helpers

(defsubst johnson-ripemd128--mask (x)
  "Mask X to 32 bits."
  (logand x #xFFFFFFFF))

(defsubst johnson-ripemd128--add (&rest args)
  "Add ARGS modulo 2^32."
  (johnson-ripemd128--mask (apply #'+ args)))

(defsubst johnson-ripemd128--not (x)
  "Bitwise NOT of X, masked to 32 bits."
  (logxor x #xFFFFFFFF))

(defsubst johnson-ripemd128--rol (x n)
  "Rotate X left by N bits (32-bit)."
  (johnson-ripemd128--mask
   (logior (ash x n) (ash x (- n 32)))))

;;;; Round functions

(defsubst johnson-ripemd128--f (x y z)
  "Round function f: X XOR Y XOR Z."
  (logxor x y z))

(defsubst johnson-ripemd128--g (x y z)
  "Round function g: (X AND Y) OR (NOT X AND Z)."
  (logior (logand x y)
          (logand (johnson-ripemd128--not x) z)))

(defsubst johnson-ripemd128--h (x y z)
  "Round function h: (X OR NOT Y) XOR Z."
  (logxor (logior x (johnson-ripemd128--not y)) z))

(defsubst johnson-ripemd128--i (x y z)
  "Round function i: (X AND Z) OR (Y AND NOT Z)."
  (logior (logand x z)
          (logand y (johnson-ripemd128--not z))))

;;;; Specification constants

(defconst johnson-ripemd128--left-k
  [#x00000000 #x5A827999 #x6ED9EBA1 #x8F1BBCDC]
  "Left-round constants K0..K3.")

(defconst johnson-ripemd128--right-k
  [#x50A28BE6 #x5C4DD124 #x6D703EF3 #x00000000]
  "Right-round constants K0'..K3'.")

;; Left message word selection indices (r), 64 entries.
(defconst johnson-ripemd128--left-r
  [ 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15    ; round 1
    7  4 13  1 10  6 15  3 12  0  9  5  2 14 11  8    ; round 2
    3 10 14  4  9 15  8  1  2  7  0  6 13 11  5 12    ; round 3
    1  9 11 10  0  8 12  4 13  3  7 15 14  5  6  2]   ; round 4
  "Left message word selection indices.")

;; Right message word selection indices (r'), 64 entries.
(defconst johnson-ripemd128--right-r
  [ 5 14  7  0  9  2 11  4 13  6 15  8  1 10  3 12    ; round 1
    6 11  3  7  0 13  5 10 14 15  8 12  4  9  1  2    ; round 2
   15  5  1  3  7 14  6  9 11  8 12  2 10  0  4 13    ; round 3
    8  6  4  1  3 11 15  0  5 12  2 13  9  7 10 14]   ; round 4
  "Right message word selection indices.")

;; Left shift amounts (s), 64 entries.
(defconst johnson-ripemd128--left-s
  [11 14 15 12  5  8  7  9 11 13 14 15  6  7  9  8    ; round 1
    7  6  8 13 11  9  7 15  7 12 15  9 11  7 13 12    ; round 2
   11 13  6  7 14  9 13 15 14  8 13  6  5 12  7  5    ; round 3
   11 12 14 15 14 15  9  8  9 14  5  6  8  6  5 12]   ; round 4
  "Left shift amounts.")

;; Right shift amounts (s'), 64 entries.
(defconst johnson-ripemd128--right-s
  [ 8  9  9 11 13 15 15  5  7  7  8 11 14 14 12  6    ; round 1
    9 13 15  7 12  8  9 11  7  7 12  7  6 15 13 11    ; round 2
    9  7 15 11  8  6  6 14 12 13  5 14 13 13  7  5    ; round 3
   15  5  8 11 14 14  6 14  6  9 12  9 12  5 15  8]   ; round 4
  "Right shift amounts.")

;; Round function dispatch vectors (indexed by round 0..3).
(defconst johnson-ripemd128--left-fn
  [johnson-ripemd128--f johnson-ripemd128--g
   johnson-ripemd128--h johnson-ripemd128--i]
  "Left-round function selectors.")

(defconst johnson-ripemd128--right-fn
  [johnson-ripemd128--i johnson-ripemd128--h
   johnson-ripemd128--g johnson-ripemd128--f]
  "Right-round function selectors.")

;;;; Padding

(defun johnson-ripemd128--pad (data)
  "Return DATA (a unibyte string) with RIPEMD-128 padding appended.
The result length is a multiple of 64 bytes."
  (let* ((len (length data))
         (bit-len (* len 8))
         ;; We need: len + 1 (for 0x80) + padding-zeros + 8 (length) ≡ 0 mod 64
         ;; i.e. (len + 1 + zeros) ≡ 56 mod 64
         (pad-zeros (mod (- 55 len) 64))
         (padded (make-string (+ len 1 pad-zeros 8) 0)))
    ;; Copy original data.
    (dotimes (i len)
      (aset padded i (aref data i)))
    ;; Append 0x80 byte.
    (aset padded len #x80)
    ;; Append 64-bit little-endian bit length at the end.
    (let ((off (+ len 1 pad-zeros)))
      (dotimes (i 8)
        (aset padded (+ off i) (logand (ash bit-len (* i -8)) #xFF))))
    padded))

;;;; Block processing

(defun johnson-ripemd128--parse-block (data offset)
  "Parse 16 little-endian 32-bit words from DATA starting at OFFSET.
Returns a vector of 16 integers."
  (let ((x (make-vector 16 0)))
    (dotimes (i 16)
      (let ((base (+ offset (* i 4))))
        (aset x i (logior (aref data base)
                          (ash (aref data (+ base 1)) 8)
                          (ash (aref data (+ base 2)) 16)
                          (ash (aref data (+ base 3)) 24)))))
    x))

(defun johnson-ripemd128--process-block (h0 h1 h2 h3 x)
  "Process one 512-bit block X, updating hash state H0..H3.
X is a 16-element vector of 32-bit words.
Returns a list (h0 h1 h2 h3)."
  (let ((al h0) (bl h1) (cl h2) (dl h3)
        (ar h0) (br h1) (cr h2) (dr h3))
    ;; Left rounds.
    (dotimes (j 64)
      (let* ((round (/ j 16))
             (fn (aref johnson-ripemd128--left-fn round))
             (t1 (johnson-ripemd128--add
                  al
                  (funcall fn bl cl dl)
                  (aref x (aref johnson-ripemd128--left-r j))
                  (aref johnson-ripemd128--left-k round))))
        (setq t1 (johnson-ripemd128--rol t1 (aref johnson-ripemd128--left-s j)))
        (setq al dl  dl cl  cl bl  bl t1)))
    ;; Right rounds.
    (dotimes (j 64)
      (let* ((round (/ j 16))
             (fn (aref johnson-ripemd128--right-fn round))
             (t1 (johnson-ripemd128--add
                  ar
                  (funcall fn br cr dr)
                  (aref x (aref johnson-ripemd128--right-r j))
                  (aref johnson-ripemd128--right-k round))))
        (setq t1 (johnson-ripemd128--rol t1 (aref johnson-ripemd128--right-s j)))
        (setq ar dr  dr cr  cr br  br t1)))
    ;; Final combination:
    ;;   t  = (h1 + cl + dr) mod 2^32
    ;;   h1 = (h2 + dl + ar) mod 2^32
    ;;   h2 = (h3 + al + br) mod 2^32
    ;;   h3 = (h0 + bl + cr) mod 2^32
    ;;   h0 = t
    (let ((t1 (johnson-ripemd128--add h1 cl dr)))
      (list t1
            (johnson-ripemd128--add h2 dl ar)
            (johnson-ripemd128--add h3 al br)
            (johnson-ripemd128--add h0 bl cr)))))

;;;; Public API

(defun johnson-ripemd128-hash (data)
  "Compute the RIPEMD-128 hash of DATA.
DATA must be a unibyte string.  Returns a 16-byte unibyte string."
  (unless (and (stringp data) (not (multibyte-string-p data)))
    (error "johnson-ripemd128-hash: DATA must be a unibyte string"))
  (let* ((padded (johnson-ripemd128--pad data))
         (blocks (/ (length padded) 64))
         (h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476))
    (dotimes (i blocks)
      (let* ((x (johnson-ripemd128--parse-block padded (* i 64)))
             (result (johnson-ripemd128--process-block h0 h1 h2 h3 x)))
        (setq h0 (nth 0 result)
              h1 (nth 1 result)
              h2 (nth 2 result)
              h3 (nth 3 result))))
    ;; Serialize h0..h3 as little-endian bytes.
    (let ((out (make-string 16 0)))
      (dotimes (w 4)
        (let ((val (nth w (list h0 h1 h2 h3))))
          (dotimes (b 4)
            (aset out (+ (* w 4) b) (logand (ash val (* b -8)) #xFF)))))
      out)))

(defun johnson-ripemd128-hash-hex (data)
  "Compute the RIPEMD-128 hash of DATA as a hexadecimal string.
DATA may be a multibyte string (it will be encoded as UTF-8) or a
unibyte string."
  (let* ((unibyte (if (multibyte-string-p data)
                      (encode-coding-string data 'utf-8)
                    data))
         (hash (johnson-ripemd128-hash unibyte)))
    (mapconcat (lambda (byte) (format "%02x" byte)) hash "")))

(provide 'johnson-ripemd128)
;;; johnson-ripemd128.el ends here
