;;; esh-opt.el --- command options processing  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2025 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;;; Code:

;;; User Functions:

;; Macro expansion of eshell-eval-using-options refers to eshell-stringify-list
;; defined in esh-util.
(require 'esh-util)

(defgroup eshell-opt nil
  "Functions for argument parsing in Eshell commands."
  :tag "Option parsing"
  :group 'eshell)

(defmacro eshell-eval-using-options (name macro-args options &rest body-forms)
  "Process NAME's MACRO-ARGS using a set of command line OPTIONS.
After doing so, stores settings in local symbols as declared by OPTIONS;
then evaluates BODY-FORMS -- assuming all was OK.

OPTIONS is a list, beginning with one or more elements of the form:
\(SHORT LONG VALUE SYMBOL HELP-STRING)
Each of these elements represents a particular command-line switch.

SHORT is either nil, or a character that can be used as a switch -SHORT.
LONG is either nil, or a string that can be used as a switch --LONG.
At least one of SHORT and LONG must be non-nil.
VALUE is the value associated with the option.  It can be either:
  t   - the option needs a value to be specified after the switch;
  nil - the option is given the value t;
  anything else - specifies the actual value for the option.
SYMBOL is either nil, or the name of the Lisp symbol that will be bound
to VALUE.  A nil SYMBOL calls `eshell-show-usage', and so is appropriate
for a \"--help\" type option.
HELP-STRING is a documentation string for the option.

Any remaining elements of OPTIONS are :KEYWORD arguments.  Some take
arguments, some do not.  The recognized :KEYWORDS are:

:external STRING
  STRING is an external command to run if there are unknown switches.

:usage STRING
  STRING is the initial part of the command's documentation string.
  It appears before the options are listed.

:post-usage STRING
  STRING is an optional trailing part of the command's documentation string.
  It appears after the options, but before the final part of the
  documentation about the associated external command (if there is one).

:show-usage
  If present, then show the usage message if the command is called with no
  arguments.

:preserve-args
  If present, do not pass MACRO-ARGS through `flatten-tree'
and `eshell-stringify-list'.

:parse-leading-options-only
  If present, do not parse dash or switch arguments after the first
positional argument.  Instead, treat them as positional arguments themselves.

For example, OPTIONS might look like:

   ((?C  nil         nil multi-column    \"multi-column display\")
    (nil \"help\"      nil nil             \"show this usage display\")
    (?r  \"reverse\"   nil reverse-list    \"reverse order while sorting\")
    :external \"ls\"
    :usage \"[OPTION]... [FILE]...
  List information about the FILEs (the current directory by default).
  Sort entries alphabetically across.\")

`eshell-eval-using-options' returns the value of the last form in
BODY-FORMS.  If instead an external command is run (because of
an unknown option), the tag `eshell-external' will be thrown with
the new process for its value.

Lastly, any remaining arguments will be available in the locally
let-bound variable `args'."
  (declare (debug (form form sexp body)))
  (let ((option-syms (eshell--get-option-symbols
                      ;; `options' is of the form (quote OPTS).
                      (cadr options))))
    `(let* ((temp-args
             ,(if (memq ':preserve-args (cadr options))
                  (list 'copy-tree macro-args)
                (list 'eshell-stringify-list
                      (list 'flatten-tree macro-args))))
            (args (eshell--do-opts ,name temp-args ,macro-args
                                   ,options ',option-syms))
            ;; Bind all the option variables.  When done, `args' will
            ;; contain any remaining positional arguments.
            ,@(mapcar (lambda (sym) `(,sym (pop args))) option-syms))
       ;; Silence unused lexical variable warning if body does not use `args'.
       (ignore args)
       ,@body-forms)))

;;; Internal Functions:

;; Documented part of the interface; see eshell-eval-using-options.
(defvar eshell--args)

(defun eshell--get-option-symbols (options)
  "Get a list of symbols for the specified OPTIONS.
OPTIONS is a list of command-line options from
`eshell-eval-using-options' (which see)."
  (delete-dups
   (delq nil (mapcar (lambda (opt) (and (listp opt) (nth 3 opt)))
                     options))))

(defun eshell--do-opts (name args orig-args options option-syms)
  "Helper function for `eshell-eval-using-options'.
This code doesn't really need to be macro expanded everywhere."
  (require 'esh-ext)
  (declare-function eshell-external-command "esh-ext" (command args))
  (let ((ext-command
         (catch 'eshell-ext-command
           (let ((usage-msg
                  (catch 'eshell-usage
                    (if (and (= (length args) 0)
                             (memq ':show-usage options))
                        (eshell-show-usage name options)
                      (setq args (eshell--process-args name args options
                                                       option-syms))
                      nil))))
             (when usage-msg
               (user-error "%s" usage-msg))))))
    (if ext-command
        (throw 'eshell-external
               (eshell-external-command ext-command orig-args))
      args)))

(defun eshell-show-usage (name options)
  "Display the usage message for NAME, using OPTIONS."
  (require 'esh-ext)
  (declare-function eshell-search-path "esh-ext" (name))
  (let ((usage (format "usage: %s %s\n\n" name
		       (cadr (memq ':usage options))))
	(extcmd (memq ':external options))
	(post-usage (memq ':post-usage options))
	had-option)
    (while options
      (when (listp (car options))
	(let ((opt (car options)))
	  (setq had-option t)
	  (cond ((and (nth 0 opt)
		      (nth 1 opt))
		 (setq usage
		       (concat usage
			       (format "    %-20s %s\n"
				       (format "-%c, --%s" (nth 0 opt)
					       (nth 1 opt))
				       (nth 4 opt)))))
		((nth 0 opt)
		 (setq usage
		       (concat usage
			       (format "    %-20s %s\n"
				       (format "-%c" (nth 0 opt))
				       (nth 4 opt)))))
		((nth 1 opt)
		 (setq usage
		       (concat usage
			       (format "    %-20s %s\n"
				       (format "    --%s" (nth 1 opt))
				       (nth 4 opt)))))
		(t (setq had-option nil)))))
      (setq options (cdr options)))
    (if post-usage
	(setq usage (concat usage (and had-option "\n")
			    (cadr post-usage))))
    (when extcmd
      (setq extcmd (eshell-search-path (cadr extcmd)))
      (if extcmd
	  (setq usage
		(concat usage
			(format-message "
This command is implemented in Lisp.  If an unrecognized option is
passed to this command, the external version `%s'
will be called instead." extcmd)))))
    (throw 'eshell-usage usage)))

(defun eshell--split-switch (switch kind)
  "Split SWITCH into its option name and potential value, if any.
KIND should be the integer 0 if SWITCH is a short option, or 1 if it's
a long option."
  (if (eq kind 0)
      ;; Short option
      (cons (aref switch 0)
            (and (> (length switch) 1) (substring switch 1)))
    ;; Long option
    (save-match-data
      (string-match "\\([^=]*\\)\\(?:=\\(.*\\)\\)?" switch)
      (cons (match-string 1 switch) (match-string 2 switch)))))

(defun eshell--set-option (name ai opt value options opt-vals)
  "Using NAME's remaining args (index AI), set the OPT within OPTIONS.
VALUE is the potential value of the OPT, coming from args like
\"-fVALUE\" or \"--foo=VALUE\", or nil if no value was supplied.  If
OPT doesn't consume a value, return VALUE unchanged so that it can be
processed later; otherwise, return nil.

If the OPT consumes an argument for its value and VALUE is nil, the
argument list will be modified."
  (if (not (nth 3 opt))
      (eshell-show-usage name options)
    (if (eq (nth 2 opt) t)
        (progn
          (setcdr (assq (nth 3 opt) opt-vals)
                  (or value
                      (if (> ai (length eshell--args))
                          (error "%s: missing option argument" name)
                        (pop (nthcdr ai eshell--args)))))
          nil)
      (setcdr (assq (nth 3 opt) opt-vals)
              (or (nth 2 opt) t))
      value)))

(defun eshell--process-option (name switch kind ai options opt-vals)
  "For NAME, process SWITCH (of type KIND), from args at index AI.
The SWITCH will be looked up in the set of OPTIONS.

SWITCH should be a string starting with the option to process,
possibly followed by its value, e.g. \"u\" or \"uUSER\".  KIND should
be the integer 0 if it's a short option, or 1 if it's a long option.

The SWITCH is then be matched against OPTIONS.  If KIND is 0 and the
SWITCH matches an option that doesn't take a value, return the
remaining characters in SWITCH to be processed later as further short
options.

If no matching handler is found, and an :external command is defined
\(and available), it will be called; otherwise, an error will be
triggered to say that the switch is unrecognized."
  (let ((switch (eshell--split-switch switch kind))
        (opts options)
	found remaining)
    (while opts
      (if (and (listp (car opts))
               (equal (car switch) (nth kind (car opts))))
	  (progn
	    (setq remaining (eshell--set-option name ai (car opts)
                                                (cdr switch) options opt-vals))
            (when (and remaining (eq kind 1))
              (error "%s: option --%s doesn't allow an argument"
                     name (car switch)))
	    (setq found t opts nil))
	(setq opts (cdr opts))))
    (if found
        remaining
      (let ((extcmd (memq ':external options)))
	(when extcmd
	  (setq extcmd (eshell-search-path (cadr extcmd))))
	(if extcmd
	    (throw 'eshell-ext-command extcmd)
          (error (if (characterp (car switch)) "%s: unrecognized option -%c"
                   "%s: unrecognized option --%s")
                 name (car switch)))))))

(defun eshell--process-args (name args options option-syms)
  "Process the given ARGS for the command NAME using OPTIONS.
OPTION-SYMS is a list of symbols that will hold the processed arguments.

Return a list of values corresponding to each element in OPTION-SYMS,
followed by any additional positional arguments."
  (let* ((opt-vals (mapcar #'list option-syms))
         (ai 0) arg
         (eshell--args args)
         (pos-argument-found nil))
    (while (and (< ai (length eshell--args))
                ;; Abort if we saw the first pos argument and option is set
                (not (and pos-argument-found
                          (memq :parse-leading-options-only options))))
      (setq arg (nth ai eshell--args))
      (if (not (and (stringp arg)
                    ;; A string of length 1 can't be an option; (if
                    ;; it's "-", that generally means stdin).
                    (> (length arg) 1)
		    (string-match "^-\\(-\\)?\\(.*\\)" arg)))
          ;; Positional argument found, skip
	  (setq ai (1+ ai)
                pos-argument-found t)
        ;; dash or switch argument found, parse
	(let* ((dash (match-string 1 arg))
	       (switch (match-string 2 arg)))
	  (pop (nthcdr ai eshell--args))
	  (if dash
	      (if (> (length switch) 0)
		  (eshell--process-option name switch 1 ai options opt-vals)
		(setq ai (length eshell--args)))
	    (while (> (length switch) 0)
	      (setq switch (eshell--process-option name switch 0
                                                   ai options opt-vals)))))))
    (nconc (mapcar #'cdr opt-vals) eshell--args)))

(provide 'esh-opt)
;;; esh-opt.el ends here
