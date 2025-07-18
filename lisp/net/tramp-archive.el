;;; tramp-archive.el --- Tramp archive manager  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; Access functions for file archives.  This is possible only on
;; machines which have installed the virtual file system for the Gnome
;; Desktop (GVFS).  Internally, file archives are mounted via the GVFS
;; "archive" method.

;; A file archive is a regular file of kind "/path/to/dir/file.EXT".
;; The extension ".EXT" identifies the type of the file archive.  A
;; file inside a file archive, called archive file name, has the name
;; "/path/to/dir/file.EXT/dir/file".

;; Most of the magic file name operations are implemented for archive
;; file names, exceptions are all operations which write into a file
;; archive, and process related operations.  Therefore, functions like

;;   (copy-file "/path/to/dir/file.tar/dir/file" "/somewhere/else")

;; work out of the box.  This is also true for file name completion,
;; and for libraries like `dired' or `ediff', which accept archive
;; file names as well.

;; File archives are identified by the file name extension ".EXT".
;; Since GVFS uses internally the library libarchive(3), all suffixes,
;; which are accepted by this library, work also for archive file
;; names.  Accepted suffixes are listed in the constant
;; `tramp-archive-suffixes'.  They are

;; * ".7z" - 7-Zip archives
;; * ".apk" - Android package kits
;; * ".ar" - UNIX archiver formats
;; * ".cab", ".CAB" - Microsoft Windows cabinets
;; * ".cpio" - CPIO archives
;; * ".crate" - Cargo (Rust) packages
;; * ".deb" - Debian packages
;; * ".depot" - HP-UX SD depots
;; * ".epub" - Electronic publications
;; * ".exe" - Self extracting Microsoft Windows EXE files
;; * ".iso" - ISO 9660 images
;; * ".jar" - Java archives
;; * ".lzh", ".LZH" - Microsoft Windows compressed LHA archives
;; * ".msu", ".MSU" - Microsoft Windows Update packages
;; * ".mtree" - BSD mtree format
;; * ".odb" ".odf" ".odg" ".odp" ".ods" ".odt" - OpenDocument formats
;; * ".pax" - Posix archives
;; * ".rar" - RAR archives
;; * ".rpm" - Red Hat packages
;; * ".shar" - Shell archives
;; * ".tar", ".tbz", ".tgz", ".tlz", ".txz" - (Compressed) tape archives
;; * ".warc" - Web archives
;; * ".xar" - macOS XAR archives
;; * ".xpi" - XPInstall Mozilla addons
;; * ".xps" - Open XML Paper Specification (OpenXPS) documents
;; * ".zip", ".ZIP" - ZIP archives

;; File archives could also be compressed, identified by an additional
;; compression suffix.  Valid compression suffixes are listed in the
;; constant `tramp-archive-compression-suffixes'.  They are ".bz2",
;; ".gz", ".lrz", ".lz", ".lz4", ".lzma", ".lzo", ".uu", ".xz", ".Z",
;; and ".zst".  A valid archive file name would be
;; "/path/to/dir/file.tar.gz/dir/file".  Even several suffixes in a
;; row are possible, like "/path/to/dir/file.tar.gz.uu/dir/file".

;; An archive file name could be a remote file name, as in
;; "/ftp:anonymous@ftp.gnu.org:/gnu/tramp/tramp-2.3.2.tar.gz/INSTALL".
;; Since all file operations are mapped internally to GVFS operations,
;; remote file names supported by tramp-gvfs.el perform better,
;; because no local copy of the file archive must be downloaded first.
;; For example, "/sftp:user@host:..." performs better than the similar
;; "/scp:user@host:...".  See the constant
;; `tramp-archive-all-gvfs-methods' for a complete list of
;; tramp-gvfs.el supported method names.

;; If `url-handler-mode' is enabled, archives could be visited via
;; URLs, like "https://ftp.gnu.org/gnu/tramp/tramp-2.3.2.tar.gz/INSTALL".
;; This allows complex file operations like

;;   (ediff-directories
;;    "https://ftp.gnu.org/gnu/tramp/tramp-2.3.1.tar.gz/tramp-2.3.1"
;;    "https://ftp.gnu.org/gnu/tramp/tramp-2.3.2.tar.gz/tramp-2.3.2" "")

;; It is even possible to access file archives in file archives, as

;;   (find-file
;;    "https://ftp.debian.org/debian/pool/main/c/coreutils/coreutils_8.28-1_amd64.deb/control.tar.gz/control")

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'tramp-gvfs)

(autoload 'dired-uncache "dired")
(autoload 'url-tramp-convert-url-to-tramp "url-tramp")
(defvar url-handler-mode-hook)
(defvar url-handler-regexp)
(defvar url-tramp-protocols)

;; We cannot check `tramp-gvfs-enabled' in loaddefs.el, because this
;; would load Tramp.  So we make a cheaper check.
;;;###autoload
(defvar tramp-archive-enabled (featurep 'dbusbind)
  "Non-nil when file archive support is available.")

;; After loading tramp-gvfs.el, we know it better.
(setq tramp-archive-enabled tramp-gvfs-enabled)

;; <https://github.com/libarchive/libarchive/wiki/LibarchiveFormats>
;; Note: "arc" and "zoo" are supported by `archive-mode', but they
;; don't work here.
;;;###autoload
(defconst tramp-archive-suffixes
  ;; "cab", "lzh", "msu" and "zip" are included with lower and upper
  ;; letters, because Microsoft Windows provides them often with
  ;; capital letters.
  '("7z" ;; 7-Zip archives.
    "apk" ;; Android package kits.  Not in libarchive testsuite.
    "ar" ;; UNIX archiver formats.
    "cab" "CAB" ;; Microsoft Windows cabinets.
    "cpio" ;; CPIO archives.
    "crate" ;; Cargo (Rust) packages.  Not in libarchive testsuite.
    "deb" ;; Debian packages.  Not in libarchive testsuite.
    "depot" ;; HP-UX SD depot.  Not in libarchive testsuite.
    "epub" ;; Electronic publications.  Not in libarchive testsuite.
    "exe" ;; Self extracting Microsoft Windows EXE files.
    "iso" ;; ISO 9660 images.
    "jar" ;; Java archives.  Not in libarchive testsuite.
    "lzh" "LZH" ;; Microsoft Windows compressed LHA archives.
    "msu" "MSU" ;; Microsoft Windows Update packages.  Not in testsuite.
    "mtree" ;; BSD mtree format.
    "odb" "odf" "odg" "odp" "ods" "odt" ;; OpenDocument formats.  Not in testsuite.
    "pax" ;; Posix archives.
    "rar" ;; RAR archives.
    "rpm" ;; Red Hat packages.
    "shar" ;; Shell archives.  Not in libarchive testsuite.
    "tar" "tbz" "tgz" "tlz" "txz" "tzst" ;; (Compressed) tape archives.
    "warc" ;; Web archives.
    "xar" ;; macOS XAR archives.  Not in libarchive testsuite.
    "xpi" ;; XPInstall Mozilla addons.  Not in libarchive testsuite.
    "xps" ;; Open XML Paper Specification (OpenXPS) documents.
    "zip" "ZIP") ;; ZIP archives.
  "List of suffixes which indicate a file archive.
It must be supported by libarchive(3).")

;; <https://unix-memo.readthedocs.io/en/latest/vfs.html>
;;    read and write: tar, cpio, pax , gzip , zip, bzip2, xz, lzip,
;;                    lzma, ar, mtree, iso9660, compress.
;;    read only: 7-Zip, mtree, xar, lha/lzh, rar, microsoft cab.

;;;###autoload
(defconst tramp-archive-compression-suffixes
  '("bz2" "gz" "lrz" "lz" "lz4" "lzma" "lzo" "uu" "xz" "Z" "zst")
  "List of suffixes which indicate a compressed file.
It must be supported by libarchive(3).")

;; The definition of `tramp-archive-file-name-regexp' contains calls
;; to `rx', which cannot be autoloaded while loading loaddefs.el.  So
;; we use a macro, which is evaluated only when needed.
;;;###autoload
(progn (defmacro tramp-archive-autoload-file-name-regexp ()
  "Regular expression matching archive file names."
  `(rx
    bos
    ;; This group is used in `tramp-archive-file-name-archive'.
    (group
     (+ nonl)
     ;; Default suffixes ...
     "." (| ,@tramp-archive-suffixes)
     ;; ... with compression.
     (? "." (| ,@tramp-archive-compression-suffixes)))
    ;; This group is used in `tramp-archive-file-name-localname'.
    (group "/" (* nonl))
    eos)))

(put #'tramp-archive-autoload-file-name-regexp 'tramp-autoload t)

;; We must wrap it into `eval-when-compile'.  Otherwise, there could
;; be an "Eager macro-expansion failure" when unloading/reloading Tramp.
;;;###tramp-autoload
(defconst tramp-archive-file-name-regexp
  (eval-when-compile (ignore-errors (tramp-archive-autoload-file-name-regexp)))
  "Regular expression matching archive file names.")

;;;###tramp-autoload
(defconst tramp-archive-method "archive"
  "Method name for archives in GVFS.")

(defconst tramp-archive-all-gvfs-methods
  (cons tramp-archive-method
	(let ((values (cdadr (get 'tramp-gvfs-methods 'custom-type))))
	  (setq values (mapcar #'last values)
		values (mapcar #'car values))))
  "List of all methods `tramp-gvfs-methods' offers.")


;; New handlers should be added here.
;;;###tramp-autoload
(defconst tramp-archive-file-name-handler-alist
  '(;; `abbreviate-file-name' performed by default handler.
    (access-file . tramp-archive-handle-access-file)
    (add-name-to-file . tramp-archive-handle-not-implemented)
    ;; `byte-compiler-base-file-name' performed by default handler.
    ;; `copy-directory' performed by default handler.
    (copy-file . tramp-archive-handle-copy-file)
    (delete-directory . tramp-archive-handle-not-implemented)
    (delete-file . tramp-archive-handle-not-implemented)
    ;; `diff-latest-backup-file' performed by default handler.
    (directory-file-name . tramp-archive-handle-directory-file-name)
    (directory-files . tramp-archive-handle-directory-files)
    (directory-files-and-attributes
     . tramp-handle-directory-files-and-attributes)
    (dired-compress-file . tramp-archive-handle-not-implemented)
    (dired-uncache . tramp-archive-handle-dired-uncache)
    (exec-path . ignore)
    ;; `expand-file-name' performed by default handler.
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . tramp-archive-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . tramp-archive-handle-file-executable-p)
    (file-exists-p . tramp-archive-handle-file-exists-p)
    (file-group-gid . tramp-archive-handle-file-group-gid)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . tramp-archive-handle-file-local-copy)
    (file-locked-p . ignore)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . tramp-archive-handle-file-name-all-completions)
    ;; `file-name-as-directory' performed by default handler.
    (file-name-case-insensitive-p . ignore)
    (file-name-completion . tramp-handle-file-name-completion)
    ;; `file-name-directory' performed by default handler.
    ;; `file-name-nondirectory' performed by default handler.
    ;; `file-name-sans-versions' performed by default handler.
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-archive-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    ;; `file-remote-p' performed by default handler.
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-archive-handle-file-symlink-p)
    (file-system-info . tramp-archive-handle-file-system-info)
    (file-truename . tramp-archive-handle-file-truename)
    (file-user-uid . tramp-archive-handle-file-user-uid)
    (file-writable-p . ignore)
    (find-backup-file-name . ignore)
    ;; `get-file-buffer' performed by default handler.
    (insert-directory . tramp-archive-handle-insert-directory)
    (insert-file-contents . tramp-archive-handle-insert-file-contents)
    (list-system-processes . ignore)
    (load . tramp-archive-handle-load)
    (lock-file . ignore)
    (make-auto-save-file-name . ignore)
    (make-directory . tramp-archive-handle-not-implemented)
    (make-directory-internal . ignore)
    (make-lock-file-name . ignore)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . ignore)
    (make-symbolic-link . tramp-archive-handle-not-implemented)
    (memory-info . ignore)
    (process-attributes . ignore)
    (process-file . ignore)
    (rename-file . tramp-archive-handle-not-implemented)
    (set-file-acl . ignore)
    (set-file-modes . tramp-archive-handle-not-implemented)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-archive-handle-not-implemented)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-archive-handle-not-implemented)
    (start-file-process . tramp-archive-handle-not-implemented)
    ;; `substitute-in-file-name' performed by default handler.
    (temporary-file-directory . tramp-archive-handle-temporary-file-directory)
    (tramp-get-home-directory . ignore)
    (tramp-get-remote-gid . ignore)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . ignore)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . ignore)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . tramp-archive-handle-not-implemented))
  "Alist of handler functions for file archive method.
Operations not mentioned here will be handled by the default Emacs primitives.")

(defsubst tramp-archive-file-name-for-operation (operation &rest args)
  "Like `tramp-file-name-for-operation', but for archive file name syntax."
  (cl-letf (((symbol-function #'tramp-tramp-file-p)
	     #'tramp-archive-file-name-p))
    (apply #'tramp-file-name-for-operation operation args)))

;;;###tramp-autoload
(progn (defun tramp-archive-run-real-handler (operation args)
  "Invoke normal file name handler for OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION."
  (let* ((inhibit-file-name-handlers
	  `(tramp-archive-file-name-handler
	    .
	    ,(and (eq inhibit-file-name-operation operation)
		  inhibit-file-name-handlers)))
	 (inhibit-file-name-operation operation))
    (apply operation args))))

;;;###tramp-autoload
(defun tramp-archive-file-name-handler (operation &rest args)
  "Invoke the file archive related OPERATION.
First arg specifies the OPERATION, second arg ARGS is a list of
arguments to pass to the OPERATION."
    (if (not tramp-archive-enabled)
        ;; Unregister `tramp-archive-file-name-handler'.
        (progn
          (tramp-register-file-name-handlers)
          (tramp-archive-run-real-handler operation args))

      (let* ((filename (apply #'tramp-archive-file-name-for-operation
			      operation args))
	     (archive (tramp-archive-file-name-archive filename)))

        ;; `filename' could be a quoted file name.  Or the file
        ;; archive could be a directory, see Bug#30293.
        (if (or (null archive)
                (not (tramp-archive-run-real-handler
                      #'file-exists-p (list archive)))
	        (tramp-archive-run-real-handler
                 #'file-directory-p (list archive)))
            (tramp-archive-run-real-handler operation args)
	  ;; The default directory of the Tramp connection buffer
	  ;; cannot be accessed.  (Bug#56628)
	  ;; FIXME: It is superfluous to set it every single loop.
	  ;; But there is no place to set it when creating the buffer.
	  (with-current-buffer
	      (tramp-get-buffer (tramp-archive-dissect-file-name filename))
	    (setq default-directory (file-name-as-directory archive)))
          ;; Now run the handler.
          (let ((tramp-methods (cons `(,tramp-archive-method) tramp-methods))
	        (tramp-gvfs-methods tramp-archive-all-gvfs-methods)
	        ;; Set uid and gid.  gvfsd-archive could do it, but it doesn't.
	        (tramp-unknown-id-integer (user-uid))
	        (tramp-unknown-id-string (user-login-name))
	        (fn (assoc operation tramp-archive-file-name-handler-alist)))
	    (when (eq (cdr fn) #'tramp-archive-handle-not-implemented)
	      (setq args (cons operation args)))
	    (if fn
	        (save-match-data (apply (cdr fn) args))
	      (tramp-archive-run-real-handler operation args)))))))

;;;###autoload
(progn (defun tramp-archive-autoload-file-name-handler (operation &rest args)
  "Load Tramp archive file name handler, and perform OPERATION."
  (defvar tramp-archive-autoload)
  (let (;; We cannot use `tramp-compat-temporary-file-directory' here
	;; due to autoload.  When installing Tramp's GNU ELPA package,
	;; there might be an older, incompatible version active.  We
	;; try to overload this.
        (default-directory temporary-file-directory)
        (tramp-archive-autoload tramp-archive-enabled))
    (apply #'tramp-autoload-file-name-handler operation args))))

(put #'tramp-archive-autoload-file-name-handler 'tramp-autoload t)

;;;###autoload
(progn (defun tramp-register-archive-autoload-file-name-handler ()
  "Add archive file name handler to `file-name-handler-alist'."
  ;; Do not use read syntax #' for `tramp-archive-file-name-handler', it
  ;; isn't autoloaded.
  (when (and tramp-archive-enabled
             (not
              (rassq 'tramp-archive-file-name-handler file-name-handler-alist)))
    (add-to-list 'file-name-handler-alist
	         (cons (tramp-archive-autoload-file-name-regexp)
		       #'tramp-archive-autoload-file-name-handler))
    (put #'tramp-archive-autoload-file-name-handler 'safe-magic t))))

(put #'tramp-register-archive-autoload-file-name-handler 'tramp-autoload t)

;;;###autoload
(progn
  (add-hook 'after-init-hook #'tramp-register-archive-autoload-file-name-handler)
  (add-hook
   'tramp-archive-unload-hook
   (lambda ()
     (remove-hook
      'after-init-hook #'tramp-register-archive-autoload-file-name-handler))))

;; Mark `operations' the handler is responsible for.
(put #'tramp-archive-file-name-handler 'operations
     (mapcar #'car tramp-archive-file-name-handler-alist))

;; `tramp-archive-file-name-handler' must be placed before `url-file-handler'.
(when url-handler-mode (tramp-register-file-name-handlers))

(with-eval-after-load 'url-handler
  (add-hook 'url-handler-mode-hook #'tramp-register-file-name-handlers)
  (add-hook
   'tramp-archive-unload-hook
   (lambda ()
     (remove-hook
      'url-handler-mode-hook #'tramp-register-file-name-handlers))))


;; File name conversions.

;;;###tramp-autoload
(defun tramp-archive-file-name-p (name)
  "Return t if NAME is a string with archive file name syntax."
  (and (stringp name)
       ;; `tramp-archive-file-name-regexp' does not suppress quoted file names.
       (not (file-name-quoted-p name t))
       ;; We cannot use `string-match-p', the matches are used.
       (string-match tramp-archive-file-name-regexp name)
       t))

(defun tramp-archive-file-name-archive (name)
  "Return archive part of NAME."
  (and (tramp-archive-file-name-p name)
       (match-string 1 name)))

(defun tramp-archive-file-name-localname (name)
  "Return localname part of NAME."
  (and (tramp-archive-file-name-p name)
       (match-string 2 name)))

(defvar tramp-archive-hash (make-hash-table :test #'equal)
  "Hash table for archive local copies.
The hash key is the archive name.  The value is a cons of the
used `tramp-file-name' structure for tramp-gvfs, and the file
name of a local copy, if any.")

(defsubst tramp-archive-gvfs-host (archive)
  "Return host name of ARCHIVE as used in GVFS for mounting."
  (url-hexify-string (tramp-gvfs-url-file-name archive)))

(defun tramp-archive-dissect-file-name (name)
  "Return a `tramp-file-name' structure for NAME.
The structure consists of the `tramp-archive-method' method, the
hexified archive name as host, and the localname.  The archive
name is kept in slot `hop'"
  (save-match-data
    (unless (tramp-archive-file-name-p name)
      (tramp-user-error nil "Not an archive file name: \"%s\"" name))
    (let* ((localname (tramp-archive-file-name-localname name))
	   (archive (file-truename (tramp-archive-file-name-archive name)))
	   (vec (make-tramp-file-name
		 :method tramp-archive-method :hop archive)))

      (cond
       ;; The value is already in the hash table.
       ((gethash archive tramp-archive-hash)
	(setq vec (car (gethash archive tramp-archive-hash))))

       ;; File archives inside file archives.
       ((tramp-archive-file-name-p archive)
	(let ((archive
	       (tramp-make-tramp-file-name
                (tramp-archive-dissect-file-name archive))))
	  (setf (tramp-file-name-host vec) (tramp-archive-gvfs-host archive)))
	(puthash archive (list vec) tramp-archive-hash))

       ;; http://...
       ((and url-handler-mode
             (string-match-p url-handler-regexp archive)
	     (string-match-p
	      "https?" (url-type (url-generic-parse-url archive))))
	(let* ((url-tramp-protocols
		(cons
		 (url-type (url-generic-parse-url archive))
		 url-tramp-protocols))
	       (archive (url-tramp-convert-url-to-tramp archive)))
	  (setf (tramp-file-name-host vec) (tramp-archive-gvfs-host archive)))
	(puthash archive (list vec) tramp-archive-hash))

       ;; GVFS supported schemes.
       ((or (tramp-gvfs-file-name-p archive)
	    (not (file-remote-p archive)))
	(setf (tramp-file-name-host vec) (tramp-archive-gvfs-host archive))
	(puthash archive (list vec) tramp-archive-hash))

       ;; Anything else.  Here we call `file-local-copy', which we
       ;; have avoided so far.
       (t (let* ((inhibit-file-name-operation #'file-local-copy)
		 (inhibit-file-name-handlers
		  (cons #'jka-compr-handler inhibit-file-name-handlers))
		 (copy (file-local-copy archive)))
	    (setf (tramp-file-name-host vec) (tramp-archive-gvfs-host copy))
	    (puthash archive (cons vec copy) tramp-archive-hash))))

      ;; So far, `vec' handles just the mount point.  Add `localname',
      ;; which shouldn't be pushed to the hash.
      (setf (tramp-file-name-localname vec) localname)
      vec)))

(defun tramp-archive-cleanup-hash ()
  "Remove local copies of archives, used by GVFS."
  ;; Don't check for a proper method.
  (let ((non-essential t))
    (maphash
     (lambda (key value)
       ;; Unmount local copy.
       (ignore-errors
	 (tramp-message (car value) 3 "Unmounting %s" (or (cdr value) key))
	 (tramp-gvfs-unmount (car value)))
       ;; Delete local copy.
       (ignore-errors (delete-file (cdr value)))
       (remhash key tramp-archive-hash))
     tramp-archive-hash)
    (clrhash tramp-archive-hash)))

(add-hook 'tramp-cleanup-all-connections-hook #'tramp-archive-cleanup-hash)
(add-hook 'kill-emacs-hook #'tramp-archive-cleanup-hash)
(add-hook 'tramp-archive-unload-hook
	  (lambda ()
	    (remove-hook 'tramp-cleanup-all-connections-hook
			 #'tramp-archive-cleanup-hash)
	    (remove-hook 'kill-emacs-hook
			 #'tramp-archive-cleanup-hash)))

(defsubst tramp-file-name-archive (vec)
  "Extract the archive file name from VEC.
VEC is expected to be a `tramp-file-name', with the method being
`tramp-archive-method', and the host being a coded URL.  The
archive name is extracted from the hop part of the VEC structure."
  (and (tramp-file-name-p vec)
       (string-equal (tramp-file-name-method vec) tramp-archive-method)
       (tramp-file-name-hop vec)))

(defmacro with-parsed-tramp-archive-file-name (filename var &rest body)
  "Parse an archive filename and make components available in the BODY.
This works exactly as `with-parsed-tramp-file-name' for the Tramp
file name structure returned by `tramp-archive-dissect-file-name'.
A variable `foo-archive' (or `archive') will be bound to the
archive name part of FILENAME, assuming `foo' (or nil) is the
value of VAR.  OTOH, the variable `foo-hop' (or `hop') won't be
offered."
  (declare (debug (form symbolp &rest body))
           (indent 2))
  (let ((bindings
         (mapcar
	  (lambda (elem)
            `(,(if var (intern (format "%s-%s" var elem)) elem)
              (,(intern (format "tramp-file-name-%s" elem))
               ,(or var 'v))))
	  (cons
	   'archive
	   (delete
	    'hop
	    (cdr (mapcar #'car (cl-struct-slot-info 'tramp-file-name))))))))
    `(let* ((,(or var 'v) (tramp-archive-dissect-file-name ,filename))
            ,@bindings)
       ;; We don't know which of those vars will be used, so we bind them all,
       ;; and then add here a dummy use of all those variables, so we don't get
       ;; flooded by warnings about those vars `body' didn't use.
       (ignore ,@(mapcar #'car bindings))
       ,@body)))

(defun tramp-archive-gvfs-file-name (name)
  "Return NAME in GVFS syntax."
  (tramp-make-tramp-file-name (tramp-archive-dissect-file-name name)))

;; This is used in GNU ELPA package tramp-locproc.el.
(defun tramp-archive-local-file-name (filename)
  "Return local mount name of FILENAME."
  (let ((tramp-methods (cons `(,tramp-archive-method) tramp-methods)))
    (tramp-gvfs-local-file-name (tramp-archive-gvfs-file-name filename))))


;; File name primitives.

(defun tramp-archive-handle-access-file (filename string)
  "Like `access-file' for file archives."
  (access-file (tramp-archive-gvfs-file-name filename) string))

(defun tramp-archive-handle-copy-file
  (filename newname &optional ok-if-already-exists keep-date
   preserve-uid-gid preserve-extended-attributes)
  "Like `copy-file' for file archives."
  (when (tramp-archive-file-name-p newname)
    (tramp-compat-permission-denied
     (tramp-archive-dissect-file-name newname) newname))
  (copy-file
   (tramp-archive-gvfs-file-name filename) newname ok-if-already-exists
   keep-date preserve-uid-gid preserve-extended-attributes))

(defun tramp-archive-handle-directory-file-name (directory)
  "Like `directory-file-name' for file archives."
  (with-parsed-tramp-archive-file-name directory nil
    (if (and (length> localname 0)
	     (eq (aref localname (1- (length localname))) ?/)
	     (not (string= localname "/")))
	(substring directory 0 -1)
      ;; We do not want to leave the file archive.  This would require
      ;; unnecessary download of http-based file archives, for
      ;; example.  So we return `directory'.
      directory)))

(defun tramp-archive-handle-directory-files
    (directory &optional full match nosort count)
  "Like `directory-files' for Tramp files."
  (tramp-barf-if-file-missing (tramp-dissect-file-name directory) directory
    (when (file-directory-p directory)
      (setq directory (file-name-as-directory (expand-file-name directory)))
      (let ((temp (nreverse (file-name-all-completions "" directory)))
	    result item)

	(while temp
	  (setq item (directory-file-name (pop temp)))
	  (when (or (null match) (string-match-p match item))
	    (push (if full (concat directory item) item)
		  result)))
	(unless nosort
          (setq result (sort result #'string<)))
	(when (and (natnump count) (> count 0))
	  (setq result (tramp-compat-ntake count result)))
	result))))

(defun tramp-archive-handle-dired-uncache (dir)
  "Like `dired-uncache' for file archives."
  (dired-uncache (tramp-archive-gvfs-file-name dir)))

(defun tramp-archive-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for file archives."
  (file-attributes (tramp-archive-gvfs-file-name filename) id-format))

(defun tramp-archive-handle-file-executable-p (filename)
  "Like `file-executable-p' for file archives."
  (file-executable-p (tramp-archive-gvfs-file-name filename)))

(defun tramp-archive-handle-file-exists-p (filename)
  "Like `file-exists-p' for file archives."
  (file-exists-p (tramp-archive-gvfs-file-name filename)))

(defun tramp-archive-handle-file-group-gid ()
  "Like `file-group-gid' for file archives."
  (with-parsed-tramp-archive-file-name default-directory nil
    (let ((default-directory (file-name-directory archive)))
      ;; `file-group-gid' exists since Emacs 30.1.
      (tramp-compat-funcall 'file-group-gid))))

(defun tramp-archive-handle-file-local-copy (filename)
  "Like `file-local-copy' for file archives."
  (file-local-copy (tramp-archive-gvfs-file-name filename)))

(defun tramp-archive-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for file archives."
  (ignore-error file-missing
    (file-name-all-completions
     filename (tramp-archive-gvfs-file-name directory))))

(defun tramp-archive-handle-file-readable-p (filename)
  "Like `file-readable-p' for file archives."
  (file-readable-p (tramp-archive-gvfs-file-name filename)))

(defun tramp-archive-handle-file-symlink-p (filename)
  "Like `file-symlink-p' for file archives."
  (file-symlink-p (tramp-archive-gvfs-file-name filename)))

(defun tramp-archive-handle-file-system-info (filename)
  "Like `file-system-info' for file archives."
  (with-parsed-tramp-archive-file-name filename nil
    (list (file-attribute-size (file-attributes archive)) 0 0)))

(defun tramp-archive-handle-file-truename (filename)
  "Like `file-truename' for file archives."
  (with-parsed-tramp-archive-file-name filename nil
    (let ((local (or (file-symlink-p filename) localname)))
      (unless (file-name-absolute-p local)
	(setq local (expand-file-name local (file-name-directory localname))))
      (concat (file-truename archive) local))))

(defun tramp-archive-handle-file-user-uid ()
  "Like `file-user-uid' for file archives."
  (with-parsed-tramp-archive-file-name default-directory nil
    (let ((default-directory (file-name-directory archive)))
      ;; `file-user-uid' exists since Emacs 30.1.
      (tramp-compat-funcall 'file-user-uid))))

(defun tramp-archive-handle-insert-directory
  (filename switches &optional wildcard full-directory-p)
  "Like `insert-directory' for file archives."
  (insert-directory
   (tramp-archive-gvfs-file-name filename) switches wildcard full-directory-p)
  (goto-char (point-min))
  (while (search-forward (tramp-archive-gvfs-file-name filename) nil 'noerror)
    (replace-match filename)))

(defun tramp-archive-handle-insert-file-contents
  (filename &optional visit beg end replace)
  "Like `insert-file-contents' for file archives."
  (let ((result
	 (insert-file-contents
	  (tramp-archive-gvfs-file-name filename) visit beg end replace)))
    (when visit (setq buffer-file-name filename))
    (cons (expand-file-name filename) (cdr result))))

(defun tramp-archive-handle-load
    (file &optional noerror nomessage nosuffix must-suffix)
  "Like `load' for file archives."
  (load
   (tramp-archive-gvfs-file-name file) noerror nomessage nosuffix must-suffix))

(defun tramp-archive-handle-temporary-file-directory ()
  "Like `temporary-file-directory' for file archives."
  ;; If the default directory, the file archive, is located on a
  ;; mounted directory, it is returned as it.  Not what we want.
  (with-parsed-tramp-archive-file-name default-directory nil
    (let ((default-directory (file-name-directory archive)))
      (temporary-file-directory))))

(defun tramp-archive-handle-not-implemented (operation &rest args)
  "Generic handler for operations not implemented for file archives."
  (let ((v (ignore-errors
	     (tramp-archive-dissect-file-name
	      (apply #'tramp-archive-file-name-for-operation operation args)))))
    (tramp-message v 10 "%s" (cons operation args))
    (tramp-error
     v 'file-error
     "Operation `%s' not implemented for file archives" operation)))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-archive 'force)))

(provide 'tramp-archive)

;;; TODO:

;; * Check, whether we could retrieve better file attributes like uid,
;;   gid, permissions.  See gvfsbackendarchive.c
;;   (archive_file_set_info_from_entry), where it is commented out.
;;
;; * Implement write access, when possible.
;;   https://bugzilla.gnome.org/show_bug.cgi?id=589617

;;; tramp-archive.el ends here
