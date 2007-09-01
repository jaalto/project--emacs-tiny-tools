;;; tinylibenv.el --- Library for environment check functions
;; $Id: tinylibenv.el,v 2.23 2007/05/01 17:20:45 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    2003-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinyliba-version.
;; Look at the code with folding.el

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;; DO NOT LOAD THIS FILE, but load the central library "m". It loads this
;; file and backward compatible library "b"
;;
;;      (require 'tinylibm)

;;}}}

;;; Change Log:

;;; Code:

;;{{{ code: Init

(eval-when-compile
  (require 'backquote)
  (autoload 'executable-find "executable")
  (autoload 'ti::directory-up "tinylib")

  (if (not (or (boundp 'xemacs-logo)
               (featurep 'xemacs)))
      ;; Emacs function, but it's buried and not published.
      (autoload 'w32-system-shell-p "w32-fns")
    (unless (fboundp 'w32-system-shell-p)
      ;;  Emacs function => compatibility for XEmacs
      (defun w32-system-shell-p (shell-name)
        "Tinylib: Emacs an XEmacs compatibility."
        ;;  This is simplistic alternative if the original function
        ;;  is not available.
        (string-match "cmdproxy"
                      (or shell-name "")))))

  ;; defvar silences Byte Compiler
  (defvar byte-compile-dynamic nil "") ;; Introduced in 19.29
  (make-local-variable 'byte-compile-dynamic)
  (setq byte-compile-dynamic t))

(provide 'tinylibenv)

;;}}}
;;{{{ code: Macros, utility functions

;; These are from SEMI::APEL::poe.el

;;; ----------------------------------------------------------------------
;;;
(put 'defun-maybe 'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
            (and (fboundp name)
                 (string-match
                  "autoload"
                  (prin1-to-string
                   (symbol-function name)))))
    (` (progn
         (defun (, name) (,@ everything-else))
         (put (quote (, name)) 'defun-maybe t)))))

;;; ----------------------------------------------------------------------
;;;
(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
            (and (fboundp name)
                 (string-match
                  "autoload"
                  (prin1-to-string
                   (symbol-function name)))))
    (` (progn
         (defsubst (, name) (,@ everything-else))
         (put (quote (, name)) 'defsubst-maybe t)))))

;;; ----------------------------------------------------------------------
;;;
(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  (when (or (not (fboundp name))
            (and (fboundp name)
                 (string-match
                  "autoload"
                  (prin1-to-string
                   (symbol-function name)))))
    (` (progn
         (defmacro (, name) (,@ everything-else))
         (put (quote (, name)) 'defmacro-maybe t)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro defalias-maybe (sym newdef)
  "Make defalias SYM if it does not exist and NEWDEF exists."
  (`
   (when (and (not (fboundp (, sym)))
              (fboundp (, newdef)))
     (defalias (, sym) (, newdef)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro defconst-maybe (name &rest everything-else)
  (or (and (boundp name)
           (not (get name 'defconst-maybe)))
      (` (or (boundp (quote (, name)))
             (progn
               (defconst (, name) (,@ everything-else))
               (put (quote (, name)) 'defconst-maybe t))))))

;;}}}
;;{{{ Environment checks

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::xemacs-p (&optional version-string)
  "Check if running XEmacs. Optionally at least VERSION-STRING.
Tested string is like  \"20.4\". Value t is returned if version
is equal or greater than VERSION-STRING."
  ;; `emacs-version' can't be modified, be bomb sure
  (let ((case-fold-search t))
    (when (string-match "xemacs" (emacs-version))
      (if (or (boundp 'xemacs-logo)
              (featurep 'xemacs))       ;Appeared in 20.2+
          (cond
           ((null version-string)
            emacs-version)
           ((not (string< emacs-version version-string))
            emacs-version))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-p (&optional version-string)
  "Check if running Emacs. Optionally at least VERSION-STRING.
Tested string is like  \"20.4\". Value t is returned if version
is equal or greater than VERSION-STRING."
  (let ((case-fold-search t))
    (unless (string-match "xemacs" (emacs-version))
      (cond
       ((null version-string)
        emacs-version)
       ((not (string< emacs-version version-string))
        emacs-version)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-type-cygwin-p ()
  "Check if running Win32 Cygwin version."
  (let ((case-fold-search t))
    (string-match "cygwin" (emacs-version))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-type-win32-p ()
  "Check if running native Win32 version of Emacs or XEmacs."
  (and (ti::win32-p)
       (not (ti::emacs-type-cygwin-p))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-type-unix-like-p ()
  "Check if running Unix Emacs or Cygwin Emacs."
  (or (not (ti::win32-p))
      (ti::emacs-type-cygwin-p)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-version-number-as-string ()
  "Emacs and XEmacs compatibility. Return plain version number string."
  ;;  Emacs: "19.34", XEmacs: "19.14 XEmacs Lucid".
  ;;  The regexp will work for both Emacs
  (and (string-match "\\([0-9]+\\(\\.[0-9.]\\)+\\)" emacs-version)
       (substring emacs-version
                  (match-beginning 1)
                  (match-end 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::emacs-version-number-as-string-major ()
  "Return major version number string. 20.4.1 --> 20.4"
  (and (string-match "^\\([0-9]+\\.[0-9]+\\)" emacs-version)
       (substring emacs-version 0 (match-end 1))))

;;  Note: While Emacs would return 20.4.1 for version number,
;;  The installation directory is not emacs-20.4.1 but 20.4 for
;;  official releases.
;;
;;  Win32: (getenv "emacs_dir"))
;;  emacs_dir is one of the variables that are taken from
;;  the registry and mapped into the environment during startup
;;  of the emacs binary.
;;
;;  See also `invocation-directory', The directory in which the Emacs
;;  executable was found
;;
;;  See also `data-directory' Directory of machine-independent files that
;;  come with GNU Emacs. These are files intended for Emacs to use while
;;  it runs.

;;; ----------------------------------------------------------------------
;;;
(defun ti::emacs-install-root ()
  "Return Emacs install ROOT by searching emacs version number from `load-path'."
  (let ((regexp
         (concat
          ".*" (regexp-quote (ti::emacs-version-number-as-string-major))
          "[.0-9]*"))
        try
        ret)
    (dolist (elt load-path)
      (when (and (stringp elt)
                 (string-match regexp elt)
                 (setq try (match-string 0 elt))
                 ;;  load-path may contain whatever directories, but
                 ;;  is it on disk too?
                 (file-directory-p (concat try "/lisp" )))
        (setq ret try)
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::emacs-install-root-emacsen (binary)
  "Search `exec-path' to find BINARY (emacs, xemacs) install root."
  (let* ((bin (executable-find binary)))
    (when bin
      (ti::directory-up
       (file-name-directory bin)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::os-check-linux-p ()
  "Check if operating system is Linux."
  (or (string-match "linux" (emacs-version))
      (memq system-type '(gnu/linux))
      ;;  ... in case the above fails, this call is more expensive
      (or (file-exists-p "/boot/vmlinuz")
          (file-exists-p "/vmlinuz"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::os-check-linux-like-p ()
  "Check Operating system is Linux or If running under Cygwin Emacs."
  (or (ti::os-check-linux-p)
      (ti::emacs-type-cygwin-p)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::os-check-sunos-p ()
  "Check Operating system is SunOS."
  (or (string-match "sparc\\|sun\\|sunos\\|solaris" (emacs-version))
      ;;  ... in case the above fails
      (file-directory-p "/vol/bin")))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::os-check-hpux-p ()
  "Check Operating system is HP-UX Unix."
  (or (string-match "hpux\\|hppa" (emacs-version))))
      ;;  #todo: ... in case the above fails
      ;; (file-directory-p "/what/to/test/here?")))

;;; ----------------------------------------------------------------------
;;;
(defun ti::win32-p ()
  "Check if running under Win32 system.
NOTE: Running under Cygwin is not considered as Win32, use
functions `ti::os-check-linux-like-p' or `ti::win32-cygwin-p'."
  (cond
   ((memq system-type '(ms-dos windows-nt))) ;; Emacs
   ((fboundp 'console-type)
    ;; Quiet Emacs byte compiler
    (memq (funcall (symbol-function 'console-type))
          '(win32 w32 mswindows)))
   ((boundp 'window-system)
    (memq (symbol-value 'window-system)
          '(win32 w32 mswindows)))
   ((error "Internal alert, contact maintainer of TinyLib."))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::win32-shell-p ()
  "Check if shell filename is traditional win32 shell."
  ;;  Prevent loading w32-fns.el, which might cause trouble in Unix
  (and (ti::win32-p)
       (w32-system-shell-p (or shell-file-name ""))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::win32-nt-p ()
  "Check windows NT/W2K/XP."
  (when (ti::win32-p)
    (or (and (fboundp 'w32-using-nt)
             ;;  - This function is in w32-fns.el
             ;;  - Hide the call from Byte Compiler that does not understand
             ;;    already checked `fboundp'
             (funcall (symbol-function 'w32-using-nt)))
        (let ((nt-root  (getenv "systemroot")))
          (and nt-root
               (or (string-match "windows.*NT"  (or (getenv "OS") "" ))
                   (file-exists-p
                    (concat
                     (file-name-as-directory nt-root)
                     "system32/cmd.exe"))))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::win32-9x-p ()
  "Check windows 9x."
  (not (ti::win32-nt-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::win32-cygwin-p-1 (&optional use-no-cache)
  "You should use `ti::win32-cygwin-p'. Optionally USE-NO-CACHE value."
  (let (ret)
    (cond
     ((and (null use-no-cache)
           (get 'ti::win32-cygwin-p 'cache-set))
      (setq ret (get 'ti::win32-cygwin-p 'cache-value)))
     (t
      (put 'ti::win32-cygwin-p 'cache-set t)
      (dolist (path exec-path)
        ;;  Sometimes there can be $PATH errors like "/bin::/sbin" and
        ;;  Emacs exec-path gets corrupted to read "/:/bin"  etc. Fix those.
        (when (and (stringp path)
                   (not (string-match "^[a-z]:" path))
                   (string-match ".*:" path))
          (setq path (replace-match "" nil nil path)))
        (when (and (stringp path)
                   ;;  Many embedded programs do include *.dll, but
                   ;;  not the whole cygwin suite. Search also typical
                   ;;  cygpath.exe
                   (file-exists-p
                    (concat
                     (file-name-as-directory path) "cygwin1.dll"))
                   (file-exists-p
                    (concat
                     (file-name-as-directory path) "cygpath.exe")))
          ;;  The root directory is one DIR up from bin/cygwin1.dll
          ;;
          ;;  1) Drop the trailing slash  ../bin
          ;;  2) Go one directory up    ..
          ;;
          ;;  Leave a trailing slash, because the resulting
          ;;  directory may be in the worst case at C:/
          ;;  (which is NOT a recommended place for cygwin install)
          ;;
          (when (string-match "^\\(.*\\)[/\\]" path)
            (setq path
                  (match-string 1 path))
            (setq ret path)
            ;;  This is native Cygwin Emacs, not a Win32 version
            ;;  if path is empty: /bin => one up => ''
            (when (string= ret "")
              (setq ret "/"))
            (put 'ti::win32-cygwin-p 'cache-value ret)
            (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::win32-cygwin-p (&optional use-cache)
  "Return path if cygwin1.dll is found from `exec-path'.
 USE-CACHE is non-nil, retrieve cached value which is faster."
  (and (ti::win32-p)
       (ti::win32-cygwin-p-1)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::os-check-gnu-support-p ()
  "Check if GNU tools are available in this system.
at is, Linux and Cygwin qualifies."
  (or (ti::os-check-linux-p)
      (ti::win32-cygwin-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::win32-cygwin-binary-p (bin &optional use-cache)
  "Check if program BIN is from Cygwin. The program must be an .exe
 USE-CACHE is non-nil, retrieve cached value."
  (let ((cygwin (ti::win32-cygwin-p))
        path)
    (when (and cygwin
               (setq path (executable-find bin))
               (string-match (regexp-quote cygwin) path))
      path)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::emacs-debug-mode (&optional mode)
  "Toggle XEmacs/Emacs debug on and off."
  (interactive "P")
  ;;  The normal debug flag
  (cond
   ((null mode)
    (setq debug-on-error (not debug-on-error)))
   ((and (integerp mode)
         (> mode 0))
    (setq debug-on-error t))
   (t
    (setq debug-on-error nil)))
  ;;  Save state for later restoring
  (when (boundp 'debug-ignored-errors)
    (unless (get 'debug-ignored-errors 'tinyliba)
      (put 'debug-ignored-errors 'tinyliba t)
      (put 'debug-ignored-errors 'tinyliba-saved debug-ignored-errors)))
  (cond
   (debug-on-error
    ;;   Emacs 20. You want to see all errors when this variable is cleared.
    (when (boundp 'debug-ignored-errors)
      (set 'debug-ignored-errors nil))
    (setq debug-on-error t)
    ;;  Must be nil, otherwise it get's on your nervers
    ;;  too much when yo hit C-g to interrupt inputs.
    ;;  This only exists in New emacs releases.
    (if (boundp 'debug-on-quit)
        (setq debug-on-quit nil))
    (if (boundp 'debug-on-signal) ;;  This must *not* be on!
        (setq debug-on-signal nil))
    (if (boundp 'stack-trace-on-error) ;; XEmacs
        (set 'stack-trace-on-error t))
    (message "TinyLib: Emacs debug is ON"))
   (t
    (when (boundp 'debug-ignored-errors)
      (set 'debug-ignored-errors
           (get 'debug-ignored-errors 'tinyliba-value)))
    (if (boundp 'stack-trace-on-error) ;; XEmacs
        (set 'stack-trace-on-error nil))
    (message "TinyLib: Emacs debug is OFF"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::turn-on-emacs-debug ()
  "Turn on Emacs or XEmacs debug."
  (interactive)
  (ti::emacs-debug-mode 1))

;;; ----------------------------------------------------------------------
;;;
(defun ti::turn-off-emacs-debug ()
  "Turn off Emacs or XEmacs debug."
  (interactive)
  (ti::emacs-debug-mode -1))

;;}}}
;;{{{ Other

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-version (file)
  "Find 'Version:' tag from lisp FILE. Retun numric version string if any."
  (let* ((lib    (locate-library file))
         (buffer (and lib (find-file-noselect lib)))
         find-file-hooks
         version)
    (save-excursion
      (if (null find-file-hooks) ;; No-op, byte compiler silencer
          (setq find-file-hooks nil))
      (set-buffer buffer)
      (goto-char (point-min))
      (if (re-search-forward
           "^;+[ \t]+Version:[ \t]+\\(.+\\)" nil t)
          (setq version (match-string 1)))
      (kill-buffer buffer)
      version)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::executable-find (program &optional type)
  "Find PROGRAM, according to TYPE (default is 'cygwin). For example
Windows includes program `ftp', but also Cygwin distribution includes
program `ftp'. The one which is found depends on the order of paths in
`exec-path'. In some case the wanted location could be either windows or
Cygwin version, regardless of the path order.

Input:

  PROGRAM    the name of the program (.exe not required)
  TYPE       [optional]
             'cygwin, which means that windows %SYSTEMROOT% is ignored.
             'win32, which means Cygwin root path and below are ignored."
  (let* ((cygwin-root (ti::win32-cygwin-p))
         (win32-root  (getenv "SYSTEMROOT")) ; Win2000
         (list        exec-path))
    (cond
     ((and (eq type 'cygwin)
           win32-root)
      (dolist (path exec-path)
        ;;  1) backward slashes, 2) forward slashes
        (when (not (or (string-match (regexp-quote win32-root) path)
                       (string-match (regexp-quote
                                      (expand-file-name win32-root)) path)))
          (push path list))))
     ((and (eq type 'win32)
           cygwin-root)
      (dolist (path exec-path)
        (when (not (or (string-match (regexp-quote cygwin-root) path)
                       (string-match (regexp-quote
                                      (replace-regexp "/" "\\" cygwin-root))
                                     path)))
          (push path list)))))
    (let ((exec-path (nreverse list))) ;; Reverse preserves the order.
      (executable-find program))))

;;}}}

;;; tinylibenv.el ends here
