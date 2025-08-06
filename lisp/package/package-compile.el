;;; package-compile.el --- Byte-Compilation of Packages  -*- lexical-binding: t; -*-

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

(defvar warning-minimum-level)
(defvar byte-compile-ignore-files)

(defcustom package-native-compile nil
  "Non-nil means to natively compile packages as part of their installation.
This controls ahead-of-time compilation of packages when they are
installed.  If this option is nil, packages will be natively
compiled when they are loaded for the first time.

This option does not have any effect if Emacs was not built with
native compilation support."
  :type '(boolean)
  :risky t
  :version "28.1"
  :group 'package)

(defun package--parse-elpaignore (pkg-desc)
  "Return a list of regular expressions to match files ignored by PKG-DESC."
  (let* ((pkg-dir (file-name-as-directory (package-desc-dir pkg-desc)))
         (ignore (expand-file-name ".elpaignore" pkg-dir))
         files)
    (when (file-exists-p ignore)
      (with-temp-buffer
        (insert-file-contents ignore)
        (goto-char (point-min))
        (while (not (eobp))
          (push (wildcard-to-regexp
                 (let ((line (buffer-substring
                              (line-beginning-position)
                              (line-end-position))))
                   (file-name-concat pkg-dir (string-trim-left line "/"))))
                files)
          (forward-line)))
      files)))

(defun package--compile (pkg-desc)
  "Byte-compile installed package PKG-DESC.
This assumes that `pkg-desc' has already been activated with
`package-activate-1'."
  (let ((byte-compile-ignore-files (package--parse-elpaignore pkg-desc))
        (warning-minimum-level :error)
        (load-path load-path))
    (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))

(defun package--native-compile-async (pkg-desc)
  "Native compile installed package PKG-DESC asynchronously.
This assumes that `pkg-desc' has already been activated with
`package-activate-1'."
  (when (native-comp-available-p)
    (let ((warning-minimum-level :error))
      (native-compile-async (package-desc-dir pkg-desc) t))))



;;;###autoload
(defun package-recompile (pkg)
  "Byte-compile package PKG again.
PKG should be either a symbol, the package name, or a `package-desc'
object."
  (interactive (list (intern (completing-read
                              "Recompile package: "
                              (mapcar #'symbol-name
                                      (mapcar #'car package-alist))))))
  (let ((pkg-desc (if (package-desc-p pkg)
                      pkg
                    (cadr (assq pkg package-alist)))))
    ;; Delete the old .elc files to ensure that we don't inadvertently
    ;; load them (in case they contain byte code/macros that are now
    ;; invalid).
    (dolist (elc (directory-files-recursively
                  (package-desc-dir pkg-desc) "\\.elc\\'"))
      (delete-file elc))
    (package--compile pkg-desc)))

;;;###autoload
(defun package-recompile-all ()
  "Byte-compile all installed packages.
This is meant to be used only in the case the byte-compiled files
are invalid due to changed byte-code, macros or the like."
  (interactive)
  (pcase-dolist (`(_ ,pkg-desc) package-alist)
    (with-demoted-errors "Error while recompiling: %S"
      (package-recompile pkg-desc))))

(provide 'package-compile)
;;; package-compile.el ends here
