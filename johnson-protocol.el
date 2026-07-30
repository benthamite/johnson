;;; johnson-protocol.el --- Retrieval worker wire protocol for johnson -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Pablo Stafforini <pablostafforini@gmail.com>
;; Package-Requires: ((emacs "29.1"))

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

;; This module implements the wire protocol shared by the johnson parent
;; Emacs and its retrieval worker child Emacs.  Messages travel as
;; newline-terminated frames holding a base64-encoded printed plist.
;; Entry packets, which may exceed the frame budget, are normalized,
;; printed, UTF-8 encoded, and split into bounded chunks that the
;; receiving side validates and reassembles.  The module is pure: it
;; depends only on `subr-x' and never touches processes or live buffers.

;;; Code:

(require 'subr-x)

;; Declared special so the reader-safety bindings below are dynamic.
(defvar read-eval)

;;;; Constants

(defconst johnson-protocol-version 1
  "Version number of the Johnson retrieval worker wire protocol.")

(defconst johnson-protocol-prefix "JOHNSON/1 "
  "Literal prefix that starts every well-formed protocol frame.")

(defconst johnson-protocol-entry-chunk-bytes (* 32 1024)
  "Maximum number of packet bytes carried by a single entry chunk.")

(defconst johnson-protocol-max-frame-bytes (* 128 1024)
  "Maximum size in bytes of a single protocol frame.")

(defconst johnson-protocol-message-types
  '(ready configure configured request dictionary-start entry-chunk
    dictionary-complete dictionary-error protocol-error shutdown)
  "Symbols allowed as the `:type' of a protocol message.")

(define-error 'johnson-protocol-error "Invalid Johnson worker protocol")

;;;; Frame codec

(defun johnson-protocol-encode (message)
  "Encode protocol MESSAGE as one newline-terminated frame.

MESSAGE is a plist; a copy of it is stamped with the current protocol
`:version' before being printed, UTF-8 encoded, and base64 armored."
  (let* ((print-length nil)
         (print-level nil)
         (printed (prin1-to-string
                   (plist-put (copy-sequence message)
                              :version johnson-protocol-version)))
         (bytes (encode-coding-string printed 'utf-8 t)))
    (concat johnson-protocol-prefix
            (base64-encode-string bytes t)
            "\n")))

(defun johnson-protocol-decode (frame)
  "Decode and validate one protocol FRAME.

FRAME is a string as produced by `johnson-protocol-encode'.  Return the
decoded message plist, or signal `johnson-protocol-error' when FRAME
lacks the protocol prefix, exceeds `johnson-protocol-max-frame-bytes',
cannot be base64 decoded or read, contains trailing junk after the
printed message, is not a plist, carries an unsupported `:version', or
names an unknown `:type'."
  (unless (string-prefix-p johnson-protocol-prefix frame)
    (signal 'johnson-protocol-error '("missing frame prefix")))
  (when (> (string-bytes frame) johnson-protocol-max-frame-bytes)
    (signal 'johnson-protocol-error '("frame exceeds maximum size")))
  (let* ((payload (string-trim-right
                   (substring frame (length johnson-protocol-prefix))))
         (printed (decode-coding-string
                   (johnson-protocol--base64-decode payload) 'utf-8))
         (read-eval nil)
         (read-result (condition-case nil
                          (read-from-string printed)
                        (error (signal 'johnson-protocol-error
                                       '("unreadable frame payload")))))
         (message (car read-result)))
    (johnson-protocol--check-no-trailing-junk printed (cdr read-result))
    (johnson-protocol--check-message message)
    message))

(defun johnson-protocol--base64-decode (string)
  "Base64 decode STRING, signaling `johnson-protocol-error' on failure."
  (condition-case nil
      (base64-decode-string string)
    (error (signal 'johnson-protocol-error '("invalid base64 payload")))))

(defun johnson-protocol--check-no-trailing-junk (printed position)
  "Reject non-whitespace in PRINTED after the object ending at POSITION.

PRINTED is the decoded frame payload and POSITION is the index returned
by `read-from-string'.  Signal `johnson-protocol-error' when anything
other than whitespace follows the single printed object."
  (unless (string-match-p "\\`[ \t\n\r]*\\'" (substring printed position))
    (signal 'johnson-protocol-error '("trailing data after message"))))

(defun johnson-protocol--check-message (message)
  "Validate the shape, version, and type of decoded MESSAGE.

MESSAGE must be a plist whose `:version' matches
`johnson-protocol-version' and whose `:type' is one of
`johnson-protocol-message-types'; otherwise signal
`johnson-protocol-error'."
  (unless (johnson-protocol--plist-p message)
    (signal 'johnson-protocol-error '("frame payload is not a plist")))
  (unless (equal (plist-get message :version) johnson-protocol-version)
    (signal 'johnson-protocol-error '("unsupported protocol version")))
  (unless (memq (plist-get message :type) johnson-protocol-message-types)
    (signal 'johnson-protocol-error '("unknown message type"))))

(defun johnson-protocol--plist-p (object)
  "Return non-nil when OBJECT is a proper list of even length."
  (let ((length (proper-list-p object)))
    (and length (zerop (% length 2)))))

;;;; Entry chunking

(defun johnson-protocol-entry-frames (identity packet)
  "Encode entry PACKET for IDENTITY as a list of chunk frames.

IDENTITY is a plist with `:lookup', `:dictionary', and `:entry' keys.
PACKET is a plist with `:raw' (the entry string, unibyte or multibyte)
and `:context' (arbitrary render context).  The packet is normalized,
printed, UTF-8 encoded, split at `johnson-protocol-entry-chunk-bytes',
and returned as `entry-chunk' frames carrying IDENTITY, `:chunk',
`:chunks', and base64 `:data'."
  (let* ((bytes (johnson-protocol--print-packet
                 (johnson-protocol--normalize-packet packet)))
         (chunks (johnson-protocol--split-bytes
                  bytes johnson-protocol-entry-chunk-bytes))
         (total (length chunks))
         (index -1))
    (mapcar (lambda (chunk)
              (setq index (1+ index))
              (johnson-protocol-encode
               (append identity
                       (list :type 'entry-chunk
                             :chunk index
                             :chunks total
                             :data (base64-encode-string chunk t)))))
            chunks)))

(defun johnson-protocol--normalize-packet (packet)
  "Return PACKET with `:raw' replaced by byte-safe printable fields.

PACKET is a plist with `:raw' and `:context'.  The result carries
`:raw-multibyte' recording the string kind, base64 `:raw-bytes', and
the untouched `:context'."
  (let* ((raw (plist-get packet :raw))
         (multibyte (multibyte-string-p raw))
         (bytes (if multibyte (encode-coding-string raw 'utf-8 t) raw)))
    (list :raw-multibyte multibyte
          :raw-bytes (base64-encode-string bytes t)
          :context (plist-get packet :context))))

(defun johnson-protocol--print-packet (packet)
  "Print normalized PACKET and return its UTF-8 encoded bytes."
  (let ((print-length nil)
        (print-level nil))
    (encode-coding-string (prin1-to-string packet) 'utf-8 t)))

(defun johnson-protocol--split-bytes (bytes size)
  "Split the unibyte string BYTES into successive pieces of SIZE bytes.

The final piece may be shorter; a single piece is returned when BYTES
fits within SIZE."
  (let ((total (length bytes))
        (start 0)
        (chunks nil))
    (while (< start total)
      (push (substring bytes start (min total (+ start size))) chunks)
      (setq start (+ start size)))
    (nreverse chunks)))

;;;; Entry assembly

(defun johnson-protocol-assemble-entry (messages)
  "Reassemble the entry packet carried by decoded chunk MESSAGES.

MESSAGES is the complete list of decoded `entry-chunk' messages for one
entry, in arrival order.  Validate identity consistency and chunk
sequence, concatenate the decoded chunk bytes, read the printed packet,
and return it as a `(:raw ... :context ...)' plist with `:raw' restored
to its original unibyte or multibyte kind.  Signal
`johnson-protocol-error' on any inconsistency."
  (johnson-protocol--check-entry-messages messages)
  (let* ((bytes (mapconcat
                 (lambda (message)
                   (johnson-protocol--base64-decode
                    (plist-get message :data)))
                 messages ""))
         (read-eval nil)
         (packet (condition-case nil
                     (car (read-from-string
                           (decode-coding-string bytes 'utf-8)))
                   (error (signal 'johnson-protocol-error
                                  '("unreadable entry packet"))))))
    (johnson-protocol--restore-raw packet)))

(defun johnson-protocol--check-entry-messages (messages)
  "Validate identity and chunk sequence of decoded chunk MESSAGES.

MESSAGES must be non-empty, agree on `:lookup', `:dictionary',
`:entry', and `:chunks', number exactly `:chunks' items, and carry
consecutive `:chunk' values starting at 0; otherwise signal
`johnson-protocol-error'."
  (unless messages
    (signal 'johnson-protocol-error '("no entry chunks to assemble")))
  (let ((first (car messages))
        (expected 0))
    (unless (equal (plist-get first :chunks) (length messages))
      (signal 'johnson-protocol-error '("incomplete entry chunk set")))
    (dolist (message messages)
      (dolist (key '(:lookup :dictionary :entry :chunks))
        (unless (equal (plist-get message key) (plist-get first key))
          (signal 'johnson-protocol-error
                  '("inconsistent entry chunk identity"))))
      (unless (equal (plist-get message :chunk) expected)
        (signal 'johnson-protocol-error '("entry chunks out of sequence")))
      (setq expected (1+ expected)))))

(defun johnson-protocol--restore-raw (packet)
  "Return the entry plist encoded in the normalized PACKET.

PACKET is the plist printed by `johnson-protocol-entry-frames'.  Decode
`:raw-bytes' back into `:raw', restoring the multibyte kind recorded in
`:raw-multibyte', and pair it with the packet's `:context'."
  (unless (and (johnson-protocol--plist-p packet)
               (stringp (plist-get packet :raw-bytes)))
    (signal 'johnson-protocol-error '("malformed entry packet")))
  (let* ((bytes (johnson-protocol--base64-decode
                 (plist-get packet :raw-bytes)))
         (raw (if (plist-get packet :raw-multibyte)
                  (decode-coding-string bytes 'utf-8)
                bytes)))
    (list :raw raw :context (plist-get packet :context))))

(provide 'johnson-protocol)
;;; johnson-protocol.el ends here
