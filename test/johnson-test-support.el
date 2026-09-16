;;; johnson-test-support.el --- Shared helpers for johnson tests -*- lexical-binding: t; -*-

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

;; Shared, ERT-free helpers for the johnson test suite: bounded
;; polling, temporary cache directories, and process cleanup.  This
;; file deliberately contains no tests.

;;; Code:

(defvar johnson-cache-directory)

(defconst johnson-test-support-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Absolute directory holding the johnson test files.")

(defconst johnson-test-support-source-directory
  (file-name-directory (directory-file-name johnson-test-support-directory))
  "Absolute directory holding the johnson source files.")

(defun johnson-test-support-wait-for (predicate &optional timeout process)
  "Poll PREDICATE until it returns non-nil or TIMEOUT seconds elapse.
PREDICATE is called with no arguments.  TIMEOUT defaults to 10 seconds.
Accept output from PROCESS, or from any process when PROCESS is nil,
between polls.  Return the predicate's non-nil value, or nil on
timeout."
  (let ((deadline (+ (float-time) (or timeout 10)))
        result)
    (while (and (not (setq result (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output process 0.05))
    result))

(defmacro johnson-test-support-with-temp-cache-dir (&rest body)
  "Run BODY with `johnson-cache-directory' bound to a fresh directory.
The temporary directory is deleted, recursively, even when BODY fails."
  (declare (indent 0) (debug t))
  `(let ((johnson-cache-directory
          (make-temp-file "johnson-test-cache-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p johnson-cache-directory)
         (delete-directory johnson-cache-directory t)))))

(defun johnson-test-support-delete-process (process)
  "Delete PROCESS without querying, tolerating a dead PROCESS."
  (when (process-live-p process)
    (set-process-query-on-exit-flag process nil)
    (delete-process process)))

(defun johnson-test-support-kill-buffer (buffer)
  "Kill BUFFER and any process attached to it, tolerating nil BUFFER."
  (when (buffer-live-p buffer)
    (when-let* ((process (get-buffer-process buffer)))
      (johnson-test-support-delete-process process))
    (kill-buffer buffer)))

(provide 'johnson-test-support)
;;; johnson-test-support.el ends here
