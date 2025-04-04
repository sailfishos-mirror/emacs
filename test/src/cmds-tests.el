;;; cmds-tests.el --- Testing some Emacs commands -*- lexical-binding: t -*-

;; Copyright (C) 2013-2025 Free Software Foundation, Inc.

;; Author: Nicolas Richard <youngfrog@members.fsf.org>
;; Keywords:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(ert-deftest self-insert-command-with-negative-argument ()
  "Test `self-insert-command' with a negative argument."
  (let ((last-command-event ?a))
    (should-error (self-insert-command -1))))

(ert-deftest forward-line-with-bignum ()
  (with-temp-buffer
    (insert "x\n")
    (let ((shortage (forward-line (1- most-negative-fixnum))))
      (should (= shortage most-negative-fixnum)))
    (let ((shortage (forward-line (+ 2 most-positive-fixnum))))
      (should (= shortage (1+ most-positive-fixnum))))))

(provide 'cmds-tests)
;;; cmds-tests.el ends here
