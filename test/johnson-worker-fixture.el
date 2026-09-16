;;; johnson-worker-fixture.el --- Deterministic worker test format -*- lexical-binding: t; -*-

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

;; This library registers the deterministic `worker-fixture' dictionary
;; format used by the johnson retrieval worker tests.  The match offset
;; string selects the retrieval behavior: "slow:SECONDS:TEXT" sleeps
;; before returning TEXT, "large:COUNT" returns COUNT `x' characters,
;; "unibyte" returns raw bytes including 0 and 255, "count" returns the
;; number of retrievals this process has served (the fixture's backend
;; cache, which makes worker persistence observable), "pid" returns the
;; Emacs process id, "error:MESSAGE" signals an error with MESSAGE, and
;; any other offset is returned as text.
;; Retrieval refuses to run in an interactive session so tests catch
;; work leaking into the parent Emacs.

;;; Code:

(require 'johnson)

(defvar johnson-worker-fixture-retrievals 0
  "Number of entries this process has retrieved.
This counter is the fixture's backend cache: it survives between
requests served by one worker process, so a second request over the
same child observes the retrievals of the first.")

(defun johnson-worker-fixture-retrieve-entry (_path offset _length)
  "Return the fixture entry selected by the behavior string OFFSET.
OFFSET \"slow:SECONDS:TEXT\" sleeps SECONDS then returns TEXT,
\"large:COUNT\" returns COUNT `x' characters, \"unibyte\" returns raw
bytes including 0 and 255, \"count\" returns the updated value of
`johnson-worker-fixture-retrievals', \"pid\" returns the Emacs process
id, \"error:MESSAGE\" signals an error with MESSAGE, and any other
OFFSET is returned as text.  _PATH and _LENGTH are ignored.  Signal an
error when called outside a batch session."
  (unless noninteractive
    (error "fixture retrieval ran in parent"))
  (setq johnson-worker-fixture-retrievals
        (1+ johnson-worker-fixture-retrievals))
  (cond ((string-match "\\`slow:\\([0-9.]+\\):\\(.*\\)\\'" offset)
         (sleep-for (string-to-number (match-string 1 offset)))
         (match-string 2 offset))
        ((string-match "\\`error:\\(.*\\)\\'" offset)
         (error "%s" (match-string 1 offset)))
        ((string-match "\\`large:\\([0-9]+\\)\\'" offset)
         (make-string (string-to-number (match-string 1 offset)) ?x))
        ((equal offset "unibyte")
         (unibyte-string 0 1 2 255))
        ((equal offset "count")
         (number-to-string johnson-worker-fixture-retrievals))
        ((equal offset "pid")
         (number-to-string (emacs-pid)))
        (t offset)))

(defun johnson-worker-fixture-render-entry (text)
  "Insert fixture entry TEXT, honoring a render-slow directive.
TEXT \"render-slow:SECONDS:TEXT\" sleeps SECONDS in an interactive
session before inserting TEXT; any other TEXT is inserted as-is."
  (if (string-match "\\`render-slow:\\([0-9.]+\\):\\(.*\\)\\'" text)
      (progn (unless noninteractive
               (sleep-for (string-to-number (match-string 1 text))))
             (insert (match-string 2 text)))
    (insert text)))

(defun johnson-worker-fixture-prepare-entry (_dict _match raw)
  "Return the worker entry packet for RAW with a fixture context marker.
_DICT and _MATCH are ignored."
  (list :raw raw :context (list :prepared t)))

(johnson-register-format
 :name "worker-fixture"
 :extensions nil
 :detect #'ignore
 :parse-metadata #'ignore
 :build-index #'ignore
 :retrieve-entry #'johnson-worker-fixture-retrieve-entry
 :render-entry #'johnson-worker-fixture-render-entry
 :worker-prepare-entry #'johnson-worker-fixture-prepare-entry)

(provide 'johnson-worker-fixture)
;;; johnson-worker-fixture.el ends here
