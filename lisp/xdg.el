;;; xdg.el --- XDG specification and standard support -*- lexical-binding: t -*-

;; Copyright (C) 2017-2025 Free Software Foundation, Inc.

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Created: 27 January 2017
;; Keywords: files, data

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Library providing some convenience functions for the following XDG
;; standards and specifications
;;
;; - XDG Base Directory Specification
;; - Thumbnail Managing Standard
;; - xdg-user-dirs configuration
;; - Desktop Entry Specification
;; - Unofficial extension $XDG_SESSION_TYPE from systemd

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))


;; XDG Base Directory Specification
;; https://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html

(defun xdg--dir-home (environ default-path)
  (let ((env (getenv environ)))
    (if (or (null env) (not (file-name-absolute-p env)))
        (expand-file-name default-path)
      env)))

(defun xdg-config-home ()
  "Return the base directory for user specific configuration files.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_CONFIG_HOME defines the base directory relative to
    which user-specific configuration files should be stored.
    If $XDG_CONFIG_HOME is either not set or empty, a default
    equal to $HOME/.config should be used.\""
  (xdg--dir-home "XDG_CONFIG_HOME" "~/.config"))

(defun xdg-cache-home ()
  "Return the base directory for user specific cache files.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_CACHE_HOME defines the base directory relative to
    which user-specific non-essential data files should be
    stored.  If $XDG_CACHE_HOME is either not set or empty, a
    default equal to $HOME/.cache should be used.\""
  (xdg--dir-home "XDG_CACHE_HOME" "~/.cache"))

(defun xdg-data-home ()
  "Return the base directory for user specific data files.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_DATA_HOME defines the base directory relative to which
    user-specific data files should be stored.  If $XDG_DATA_HOME is
    either not set or empty, a default equal to $HOME/.local/share
    should be used.\""
  (xdg--dir-home "XDG_DATA_HOME" "~/.local/share"))

(defun xdg-state-home ()
  "Return the base directory for user-specific state data.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

  \"The $XDG_STATE_HOME contains state data that should persist
  between (application) restarts, but that is not important or
  portable enough to the user that it should be stored in
  $XDG_DATA_HOME.  It may contain:

  * actions history (logs, history, recently used files, …)

  * current state of the application that can be reused on a
    restart (view, layout, open files, undo history, …)\""
  (xdg--dir-home "XDG_STATE_HOME" "~/.local/state"))

(defun xdg-runtime-dir ()
  "Return the value of $XDG_RUNTIME_DIR.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_RUNTIME_DIR defines the base directory relative to
    which user-specific non-essential runtime files and other
    file objects (such as sockets, named pipes, ...) should be
    stored.\""
  (getenv "XDG_RUNTIME_DIR"))

(defun xdg-config-dirs ()
  "Return the config directory search path as a list.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_CONFIG_DIRS defines the preference-ordered set of base
    directories to search for configuration files in addition to
    the $XDG_CONFIG_HOME base directory.  The directories in
    $XDG_CONFIG_DIRS should be separated with a colon ':'.

    \"If $XDG_CONFIG_DIRS is either not set or empty, a value equal to
    /etc/xdg should be used.\""
  (let ((env (getenv "XDG_CONFIG_DIRS")))
    (if (or (null env) (string= env ""))
        '("/etc/xdg")
      (parse-colon-path env))))

(defun xdg-data-dirs ()
  "Return the data directory search path as a list.

According to the XDG Base Directory Specification version
0.8 (8th May 2021):

    \"$XDG_DATA_DIRS defines the preference-ordered set of base
    directories to search for data files in addition to the
    $XDG_DATA_HOME base directory.  The directories in
    $XDG_DATA_DIRS should be separated with a colon ':'.

    \"If $XDG_DATA_DIRS is either not set or empty, a value equal
    to /usr/local/share/:/usr/share/ should be used.\""
  (let ((env (getenv "XDG_DATA_DIRS")))
    (if (or (null env) (string= env ""))
        '("/usr/local/share/" "/usr/share/")
      (parse-colon-path env))))


;; Thumbnail Managing Standard
;; https://specifications.freedesktop.org/thumbnail-spec/thumbnail-spec-latest.html

(defun xdg-thumb-uri (filename)
  "Return the canonical URI for FILENAME.
If FILENAME has absolute file name /foo/bar.jpg, its canonical URI is
file:///foo/bar.jpg"
  (concat "file://" (expand-file-name filename)))

(defun xdg-thumb-name (filename)
  "Return the appropriate thumbnail filename for FILENAME."
  (concat (md5 (xdg-thumb-uri filename)) ".png"))

(defun xdg-thumb-mtime (filename)
  "Return modification time of FILENAME as an Emacs timestamp."
  (file-attribute-modification-time (file-attributes filename)))


;; XDG User Directories
;; https://www.freedesktop.org/wiki/Software/xdg-user-dirs/

(defconst xdg-line-regexp
  (rx "XDG_"
      (group-n 1 (or "DESKTOP" "DOWNLOAD" "TEMPLATES" "PUBLICSHARE"
                     "DOCUMENTS" "MUSIC" "PICTURES" "VIDEOS"))
      "_DIR=\""
      (group-n 2 (or "/" "$HOME/") (*? (or (not (any "\"")) "\\\"")))
      "\"")
  "Regexp matching non-comment lines in `xdg-user-dirs' config files.")

(defvar xdg-user-dirs nil
  "Alist of directory keys and values.")

(defun xdg--substitute-home-env (str)
  (if (file-name-absolute-p str) str
    (save-match-data
      (and (string-match "^\\$HOME/" str)
           (replace-match "~/" t nil str 0)))))

(defun xdg--user-dirs-parse-line ()
  "Return pair of user-dirs key to directory value in LINE, otherwise nil.
This should be called at the beginning of a line."
  (skip-chars-forward "[:blank:]")
  (when (and (/= (following-char) ?#)
             (looking-at xdg-line-regexp))
    (let ((k (match-string 1))
          (v (match-string 2)))
      (when (and k v) (cons k (xdg--substitute-home-env v))))))

(defun xdg--user-dirs-parse-file (filename)
  "Return alist of `xdg-user-dirs' from FILENAME."
  (let (elt res)
    (when (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (while (not (eobp))
          (setq elt (xdg--user-dirs-parse-line))
          (when (consp elt) (push elt res))
          (forward-line))))
    res))

(defun xdg-user-dir (name)
  "Return the directory referred to by NAME."
  (when (null xdg-user-dirs)
    (setq xdg-user-dirs
          (xdg--user-dirs-parse-file
           (expand-file-name "user-dirs.dirs" (xdg-config-home)))))
  (let ((dir (cdr (assoc name xdg-user-dirs))))
    (when dir (expand-file-name dir))))


;; Desktop Entry Specification
;; https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.1.html

(defconst xdg-desktop-group-regexp
  (rx "[" (group-n 1 (+? (in " -Z\\^-~"))) "]")
  "Regexp matching desktop file group header names.")

;; TODO Localized strings left out intentionally, as Emacs has no
;; notion of l10n/i18n
(defconst xdg-desktop-entry-regexp
  (rx (group-n 1 (+ (in "A-Za-z0-9-")))
      ;; (? "[" (group-n 3 (+ nonl)) "]")
      (* blank) "=" (* blank)
      (group-n 2 (* nonl)))
  "Regexp matching desktop file entry key-value pairs.")

(defun xdg-desktop-read-group ()
  "Return hash table of group of desktop entries in the current buffer."
  (let ((res (make-hash-table :test #'equal)))
    (while (not (or (eobp) (looking-at xdg-desktop-group-regexp)))
      (skip-chars-forward "[:blank:]")
      (cond
       ((eolp))
       ((= (following-char) ?#))
       ((looking-at xdg-desktop-entry-regexp)
        (puthash (match-string 1) (match-string 2) res))
       ;; Filter localized strings
       ((looking-at (rx (group-n 1 (+ (in alnum "-"))) (* blank) "[")))
       (t (error "Malformed line: %s"
                 (buffer-substring (point) (line-end-position)))))
      (forward-line))
    res))

(defun xdg-desktop-read-file (filename &optional group)
  "Return group contents of desktop file FILENAME as a hash table.
Optional argument GROUP defaults to the string \"Desktop Entry\"."
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (while (and (skip-chars-forward "[:blank:]" (line-end-position))
                (or (eolp) (= (following-char) ?#)))
      (forward-line))
    (unless (looking-at xdg-desktop-group-regexp)
      (error "Expected group name!  Instead saw: %s"
             (buffer-substring (point) (line-end-position))))
    (when group
      (while (and (re-search-forward xdg-desktop-group-regexp nil t)
                  (not (equal (match-string 1) group)))))
    (forward-line)
    (xdg-desktop-read-group)))

(defun xdg-desktop-strings (value)
  "Partition VALUE into elements delimited by unescaped semicolons."
  (let (res)
    (setq value (string-trim-left value))
    (dolist (x (split-string (string-replace "\\;" "\0" value) ";"))
      (push (string-replace "\0" ";" x) res))
    (when (null (string-match-p "[^[:blank:]]" (car res))) (pop res))
    (nreverse res)))

(defun xdg-current-desktop ()
  "Return a list of strings identifying the current desktop environment.

According to the XDG Desktop Entry Specification version 0.5:

    If $XDG_CURRENT_DESKTOP is set then it contains a
    colon-separated list of strings ... $XDG_CURRENT_DESKTOP
    should have been set by the login manager, according to the
    value of the DesktopNames found in the session file."
  (when-let* ((ret (getenv "XDG_CURRENT_DESKTOP")))
    (string-split ret ":")))


;; MIME apps specification
;; https://standards.freedesktop.org/mime-apps-spec/mime-apps-spec-1.0.1.html

(defvar xdg-mime-table nil
  "Table of MIME type to desktop file associations.
The table is an alist with keys being MIME major types (\"application\",
\"audio\", etc.), and values being hash tables.  Each hash table has
MIME subtypes as keys and lists of desktop file absolute filenames.")

(defun xdg-mime-apps-files ()
  "Return a list of files containing MIME/Desktop associations.
The list is in order of descending priority: user config, then
admin config, and finally system cached associations."
  (let ((xdg-data-dirs (xdg-data-dirs))
        (desktop (getenv "XDG_CURRENT_DESKTOP"))
        res)
    (when desktop
      (setq desktop (list (format "%s-mimeapps.list" desktop))))
    (dolist (name (cons "mimeapps.list" desktop))
      (push (expand-file-name name (xdg-config-home)) res)
      (push (expand-file-name (format "applications/%s" name) (xdg-data-home))
            res)
      (dolist (dir (xdg-config-dirs))
        (push (expand-file-name name dir) res))
      (dolist (dir xdg-data-dirs)
        (push (expand-file-name (format "applications/%s" name) dir) res)))
    (dolist (dir xdg-data-dirs)
      (push (expand-file-name "applications/mimeinfo.cache" dir) res))
    (nreverse res)))

(defun xdg-mime-collect-associations (mime files)
  "Return a list of desktop file names associated with MIME.
The associations are searched in the list of file names FILES,
which is expected to be ordered by priority as in
`xdg-mime-apps-files'."
  (let ((regexp (concat (regexp-quote mime) "=\\([^[:cntrl:]]*\\)$"))
        res sec defaults added removed cached)
    (with-temp-buffer
      (dolist (f (reverse files))
        (when (file-readable-p f)
          (insert-file-contents-literally f nil nil nil t)
          (goto-char (point-min))
          (let () ;; end
            (while (not (or (eobp))) ;; end
              (if (= (following-char) ?\[)
                  (progn (setq sec (char-after (1+ (point))))
                         (forward-line))
                (if (not (looking-at regexp))
                    (forward-line)
                  (dolist (str (xdg-desktop-strings (match-string 1)))
                    (cl-pushnew str
                                (cond ((eq sec ?D) defaults)
                                      ((eq sec ?A) added)
                                      ((eq sec ?R) removed)
                                      ((eq sec ?M) cached))
                                :test #'equal))
                  (while (and (zerop (forward-line))
                              (/= (following-char) ?\[)))))))
          ;; Accumulate results into res
          (dolist (f cached)
            (when (not (member f removed)) (cl-pushnew f res :test #'equal)))
          (dolist (f added)
            (when (not (member f removed)) (push f res)))
          (dolist (f removed)
            (setq res (delete f res)))
          (dolist (f defaults)
            (push f res))
          (setq defaults nil added nil removed nil cached nil))))
    (delete-dups res)))

(defun xdg-mime-apps (mime)
  "Return list of desktop files associated with MIME, otherwise nil.
The list is in order of descending priority, and each element is
an absolute file name of a readable file.
Results are cached in `xdg-mime-table'."
  (pcase-let ((`(,type ,subtype) (split-string mime "/"))
              (xdg-data-dirs (xdg-data-dirs))
              (caches (xdg-mime-apps-files))
              (files ()))
    (let ((mtim1 (get 'xdg-mime-table 'mtime))
          (mtim2 (cl-loop for f in caches when (file-readable-p f)
                          maximize (float-time
				    (file-attribute-modification-time
				     (file-attributes f))))))
      ;; If one of the MIME/Desktop cache files has been modified:
      (when (or (null mtim1) (time-less-p mtim1 mtim2))
        (setq xdg-mime-table nil)))
    (when (null (assoc type xdg-mime-table))
      (push (cons type (make-hash-table :test #'equal)) xdg-mime-table))
    (if (let ((table (cdr (assoc type xdg-mime-table))))
          (hash-table-contains-p subtype table))
        files
      (and files (setq files nil))
      (let ((dirs (mapcar (lambda (dir) (expand-file-name "applications" dir))
                          (cons (xdg-data-home) xdg-data-dirs))))
        ;; Not being particular about desktop IDs
        (dolist (f (nreverse (xdg-mime-collect-associations mime caches)))
          (push (locate-file f dirs) files))
        (when files
          (put 'xdg-mime-table 'mtime (current-time)))
        (puthash subtype (delq nil files) (cdr (assoc type xdg-mime-table)))))))


;; Unofficial extension from systemd.

(defun xdg-session-type ()
  "Return the value of $XDG_SESSION_TYPE.
Should be one of \"unspecified\", \"tty\", \"x11\", \"wayland\",
or \"mir\".

This is not part of any official Freedesktop.org standard, but is
documented in the man page `pam_systemd'."
  (getenv "XDG_SESSION_TYPE"))

(provide 'xdg)

;;; xdg.el ends here
