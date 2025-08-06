;;; package-misc.el --- Miscellaneous Packaging Functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Philip Kaludercic

;; Author: Philip Kaludercic <philipk@posteo.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'package-core)

(require 'macroexp)

(defun package--print-email-button (recipient)
  "Insert a button whose action will send an email to RECIPIENT.
NAME should have the form (FULLNAME . EMAIL) where FULLNAME is
either a full name or nil, and EMAIL is a valid email address."
  (when (car recipient)
    (insert (car recipient)))
  (when (and (car recipient) (cdr recipient))
    (insert " "))
  (when (cdr recipient)
    (insert "<")
    (insert-text-button (cdr recipient)
                        'follow-link t
                        'action (lambda (_)
                                  (compose-mail
                                   (format "%s <%s>" (car recipient) (cdr recipient)))))
    (insert ">"))
  (insert "\n"))

(declare-function ietf-drums-parse-address "ietf-drums"
                  (string &optional decode))

(defun package-maintainers (pkg-desc &optional no-error)
  "Return an email address for the maintainers of PKG-DESC.
The email address may contain commas, if there are multiple
maintainers.  If no maintainers are found, an error will be
signaled.  If the optional argument NO-ERROR is non-nil no error
will be signaled in that case."
  (unless (package-desc-p pkg-desc)
    (error "Invalid package description: %S" pkg-desc))
  (let* ((name (package-desc-name pkg-desc))
         (extras (package-desc-extras pkg-desc))
         (maint (alist-get :maintainer extras)))
    (unless (listp (cdr maint))
      (setq maint (list maint)))
    (cond
     ((and (null maint) (null no-error))
      (user-error "Package `%s' has no explicit maintainer" name))
     ((and (not (progn
                  (require 'ietf-drums)
                  (ietf-drums-parse-address (cdar maint))))
           (null no-error))
      (user-error "Package `%s' has no maintainer address" name))
     (t
      (with-temp-buffer
        (mapc #'package--print-email-button maint)
        (replace-regexp-in-string
         "\n" ", " (string-trim
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))))

;;;###autoload
(defun package-report-bug (desc)
  "Prepare a message to send to the maintainers of a package.
DESC must be a `package-desc' object.

Of interest to package maintainers: By default, the command will use
`reporter-submit-bug-report' to generate a message buffer.  If your
package has specific needs, you can set the symbol property
`package-report-bug-function' of the symbol designating your package
name.
"
  (interactive (list (package--query-desc package-alist))
               package-menu-mode)
  (let ((maint (package-maintainers desc))
        (name (symbol-name (package-desc-name desc)))
        (pkgdir (package-desc-dir desc))
        vars)
    (when pkgdir
      (dolist-with-progress-reporter (group custom-current-group-alist)
          "Scanning for modified user options..."
        (when (and (car group)
                   (file-in-directory-p (car group) pkgdir))
          (dolist (ent (get (cdr group) 'custom-group))
            (when (and (custom-variable-p (car ent))
                       (boundp (car ent))
                       (not (eq (custom--standard-value (car ent))
                                (default-toplevel-value (car ent)))))
              (push (car ent) vars))))))
    (dlet ((reporter-prompt-for-summary-p t))
      (funcall (or (get name 'package-report-bug-function)
                   #'reporter-submit-bug-report)
               maint name vars))))

;;;; Inferring package from current buffer
(defun package-read-from-string (str)
  "Read a Lisp expression from STR.
Signal an error if the entire string was not used."
  (pcase-let ((`(,expr . ,offset) (read-from-string str)))
    (condition-case ()
        ;; The call to `ignore' suppresses a compiler warning.
        (progn (ignore (read-from-string str offset))
               (error "Can't read whole string"))
      (end-of-file expr))))


(defun package--alist-to-plist-args (alist)
  (mapcar #'macroexp-quote
          (apply #'nconc
                 (mapcar (lambda (pair) (list (car pair) (cdr pair))) alist))))

(provide 'package-misc)
;;; package-misc.el ends here
