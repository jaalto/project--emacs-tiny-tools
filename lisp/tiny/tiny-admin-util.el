;;; tiny-admin-util.el --- Tiny Tools administrative utilities for maintainer

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    2001-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto

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
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;; Nothing to install. Load this file.

;;}}}

;;{{{ Documentation

;;; Commentary:

;;  Desription
;;
;;      This file contains administrative functions to maintain the
;;      project. There are no user functions.
;;
;;  Administration
;;
;;     Autoload files
;;
;;      If *loaddef* files were not included in the package or if they were
;;      mistakenly deleted. Use following functions
;;
;;          tiny-setup-generate-loaddefs-dir
;;          tiny-setup-generate-loaddefs-recursive
;;
;;          tiny-setup-autoload-build-for-file
;;          tiny-setup-autoload-build-for-dir
;;          tiny-setup-autoload-build-recursive
;;
;;     Compilation check
;;
;;      To check for possible leaks in code, ran the byte compilation
;;      function from shell by using XEmacs compiler. The Emacs byte
;;      compiler is not that good in findings all errors.
;;      See function `tiny-setup-compile-kit-all'.
;;
;;     Profiling
;;
;;      To check how much time each file load would take, see function
;;      `tiny-setup-test-load-time-libraries'. Here are results as of
;;      2001-03-18 running Win9x/512Meg/400Mhz, Emacs 20.7, non-compiled
;;      files:
;;
;;          Timing tinyliba,  took     2.025000 secs (autoloads)
;;          Timing tinylibb,  took     0.011000 secs
;;          Timing tinylibm,  took     0.977000 secs
;;          Timing tinylib,   took     0.982000 secs
;;          Timing tinylibid, took     0.006000 secs
;;          Timing tinylibo,  took     0.005000 secs
;;          Timing tinylibt,  took     0.011000 secs
;;          total time is 4.027999997138977 seconds

;;}}}

;;; Change Log:

;;; Code:

(eval-when-compile
  (load "cl-seq")
  (require 'cl))

(require 'tinylib)

(defconst tiny-setup-:library-compile-order
  '("tinylibenv.el"
    "tinyliba.el"
    "tinylibm.el"
    "tinylibb.el")
  "Order of compilation of the libraries.
This variable is list of REGEXPS.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      GENERAL UTILITIES
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tiny-setup-:ignore-dir-regexp
  "\\(\\.\\(bzr\\|hg\\|git\\|svn\\|mtn\\)\\|CVS\\|RCS\\|_MTN\\|\\.\\.?\\)$"
  "Regexp to ignore directories.")

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (not (string-match "[\\/]\\.\\.?$" elt)))
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(put 'tiny-setup-directory-recursive-macro 'lisp-indent-function 1)
(put 'tiny-setup-directory-recursive-macro 'edebug-form-spec '(body))
(defmacro tiny-setup-directory-recursive-macro (directory &rest body)
  "Start from DIRECTORY and run BODY recursively in each directory.

Following variables are set during BODY:

`dir'      Directrory name
`dir-list' All directories under `dir'."
  (`
   (flet ((recurse
           (dir)
           (let* ((dir-list (tiny-setup-directory-list dir)))
             (,@ body)
             (when dir-list
               (dolist (elt dir-list)
                 (unless (string-match tiny-setup-:ignore-dir-regexp elt)
                   (recurse elt)))))))
     (recurse (, directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      TIMING UTILITIES
;;      These are admistrative utilies for package maintainer(s)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g. if you want to calculate days; you'd do
\(/ (ti::date-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (multiple-value-bind (s0 s1 s2) a
    (setq a (+ (* (float (ash 1 16)) s0)
               (float s1) (* 0.0000001 s2))))
  (multiple-value-bind (s0 s1 s2) b
    (setq b (+ (* (float (ash 1 16)) s0)
               (float s1) (* 0.0000001 s2))))
  (- a b))

;;; ----------------------------------------------------------------------
;;;
(defvar tiny-setup-:time nil)
(put 'tiny-setup-time-this 'lisp-indent-function 0)
(put 'tiny-setup-time-this 'edebug-form-spec '(body))
(defmacro tiny-setup-time-this (&rest body)
  "Run BODY with and time execution. Time is in `my-:tmp-time-diff'."
  `(let* ((tmp-time-A (current-time))
          tmp-time-B)
     ,@body
     (setq tmp-time-B (current-time))
     (setq tiny-setup-:time
           (tiny-setup-time-difference tmp-time-B tmp-time-A))))

;;; ----------------------------------------------------------------------
;;;
(put 'tiny-setup-with-file-env-macro 'lisp-indent-function 0)
(put 'tiny-setup-with-file-env-macro 'edebug-form-spec '(body))
(defmacro tiny-setup-with-file-env-macro (&rest body)
  "Run BODY with all the interfering hooks turned off."
  `(let* (find-file-hook
          write-file-hooks
          font-lock-mode
          ;; buffer-auto-save-file-name
          auto-save-hook
          auto-save-default
          (auto-save-interval 0)
          (original-backup-inhibited backup-inhibited)
          (backup-inhibited t))
     ;; Reset also global
     (setq-default backup-inhibited t)
     ;;  When each file is loaded to emacs, do not turn on lisp-mode
     ;;  or anything else => cleared file hooks. These are byte compiler
     ;;  silencers:
     (if (null find-file-hook)
         (setq find-file-hook nil))
     (if (null write-file-hooks)
         (setq write-file-hooks nil))
     (if (null font-lock-mode)
         (setq font-lock-mode nil))
     (if (null auto-save-hook)
         (setq auto-save-hook nil))
     (if (null auto-save-default)
         (setq auto-save-default nil))
     (if auto-save-interval
         (setq auto-save-interval 0))
     (if backup-inhibited
         (setq backup-inhibited t))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-time-load-file (file)
  "Time lisp FILE loading."
  (interactive "fload file and time it: ")
  (tiny-setup-time-this
   (load file))
  (message "Tiny: Timing %-15s took %12f secs" file tiny-setup-:time))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-test-load-time-libraries ()
  "Time package load times."
  (interactive)
  (message "\n\n** Tiny setup: timing test start\n")
  (message "load-path: %s"
           (prin1-to-string load-path))
  (let* ((path (locate-library "tinylib.el"))
         (time-a (current-time))
         time-b)
    (if (not path)
        (message "Tiny: [timing] Can't find tinylib.el along `load-path'")
      (setq path (file-name-directory path))
      (dolist (pkg (directory-files path 'full "^tinylib.*el"))
        (tiny-setup-time-load-file pkg))
      (setq time-b (current-time))
      (message "Tiny: total time is %s seconds"
               (tiny-setup-time-difference time-b time-a))
      (display-buffer "*Messages*"))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-test-load-all ()
  "Load each package to check against errors."
  (interactive)
  (message "\n\n** Tiny setup: load test start\n")
  (let* ((path (locate-library "tinylib.el")))
    (if (not path)
        (message "Tiny: [load test] Can't find tinylib.el along `load-path'")
      (setq path (file-name-directory path))
      (dolist (pkg (directory-files path 'full "^tiny.*el"))
        (load pkg))
      (display-buffer "*Messages*"))))


;;; ----------------------------------------------------------------------
;;; FIXME: remove
;;; (tiny-setup-autoload-build-functions "~/elisp/tiny/lisp/tiny")
;;; (tiny-setup-autoload-build-functions "~/elisp/tiny/lisp/other")
;;;
(defun tiny-setup-autoload-build-functions (dir &optional regexp)
  "Build all autoloads form DIR, except for files matching REGEXP.
Store the autoloads to tiny-DIR-autoload.el"
  (let* (make-backup-files                 ;; Do not make backups
         (backup-enable-predicate 'ignore) ;; Really, no backups
         (files   (directory-files
                   dir
                   'full
                   "\\.el$"))
         ;; There is no mistake in name here: it is "tiny-autoload-DIRNAME".
         ;; the other autoload generater will generate
         ;; "tiny-autoload-loaddefs-DIRNAME"
         (to-file (tiny-setup-directory-to-file-name dir "tiny-autoload-"))
         (name    (file-name-nondirectory to-file)))
    (when files
      (with-temp-buffer
        (insert
         (format ";;; %s -- " name)
         "Autoload definitions\n"
         ";;  Generate date: " (format-time-string "%Y-%m-%d" (current-time))
         "\n\
;;  This file was automatically generated. Do not change."
         "\n\n")
        (dolist (file files)
          (if (and (stringp regexp)
                   (string-match regexp file))
              (message "Tiny: Ignoring autoload creation for %s" file)
            (ti::package-autoload-create-on-file
             file (current-buffer) 'no-show)))
        (insert (tinypath-tmp-autoload-file-footer to-file 'eof))
        (let ((backup-inhibited t))
          (write-region (point-min) (point-max) to-file))
        to-file))
    (message "TinySetup: Updated ALL autoloads in dir %s" dir)))

;;; ----------------------------------------------------------------------
;;;     This is autoload generator will generate ALL, that means ALL,
;;;     autoloads from EVERY function and macro.
;;;     The implementation is in tinylib.el
;;;
;;; (tiny-setup-autoload-build-functions-all "~/elisp/tiny/lisp/")
;;;
(defun tiny-setup-autoload-build-functions-all (dir)
  "Build all autoloads recursively below DIR."
  (interactive "Dautoload build root dir: ")
  (let* ((dirs (tiny-setup-directories
                (directory-files
                 (expand-file-name dir)
                 'abs)))
         (regexp "tinylib\\|autoload\\loaddefs"))
    (cond
     (dirs
      (tiny-setup-autoload-build-functions dir regexp)
      (dolist (dir dirs)
        (tiny-setup-autoload-build-functions-all dir)))
     (t
      (tiny-setup-autoload-build-functions dir regexp)))))

;;; ----------------------------------------------------------------------
;;; (tiny-setup-autoload-build-loaddefs-tiny-tools "~/elisp/tiny/lisp/" t)
;;; (tiny-setup-autoload-build-loaddefs-tiny-tools "~/elisp/tiny/lisp/other" t)
;;;
(defun tiny-setup-autoload-build-loaddefs-tiny-tools (dir &optional force)
  "Build Tiny Tools autoloads below DIR. FORCE recreates everything."
  (interactive "DAutoload root: \nP")
  (ti::package-autoload-loaddefs-build-recursive
   dir
   "autoload\\|loaddefs" ;; Exclude these files
   force
   (function
    (lambda (dir)
      (tiny-setup-directory-to-file-name
       (or dir
           (error "TinySetup: No DIR"))
       "tiny-autoload-loaddefs-")))))

;;; ----------------------------------------------------------------------
;;;     This is autoload generator will generate ONLY functions marked
;;;     with special ### autoload tag. The implementation used is in
;;;     core Emacs package autoload.el
;;;
;;; (tiny-setup-autoload-batch-update "~/elisp/tiny/lisp/" 'force)
;;;
;;; This function is invoked from the perl makefile.pl with the
;;; ROOT directory as sole argument in Emacs command line.
;;;
;;; The build command from prompt is
;;;
;;;    $ perl makefile.pl --verbose 2 --binary emacs  autoload
;;;
(defun tiny-setup-autoload-batch-update (&optional dir force)
  "Update autoloads in batch mode. Argument in command line is DIR. FORCE."
  (interactive "DAutoload dir to update: ")
  (unless dir
    (setq dir (pop command-line-args-left))
    (setq force t))
  (if dir                               ;Require slash
      (setq dir (file-name-as-directory dir)))
  (unless dir
    (message "Tiny: From what directory to make recursively autoloads?")
    ;; Self generate error for command line ...
    (error 'tiny-setup-autoload-batch-update))
  (message "TinySetup: Generating all autoloads under %s" dir)
  (let* ((default-directory (expand-file-name dir)))
    (message "Tiny: tiny-setup-autoload-batch-update %s"  default-directory)
    (when (not (string-match "^[/~]\\|^[a-zA-Z]:[/\\]"
                             default-directory))
      (message "Tiny: Autoload directory must be absolute path name.")
      (error 'tiny-setup-autoload-batch-update))
    (tiny-setup-autoload-build-loaddefs-tiny-tools
     default-directory force)))
    ;;  This would generate second set of autoloads. Don't do that any more,
    ;;  rely on Emacs autoload.el instead.
    ;; (tiny-setup-autoload-build-functions-all default-directory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      PACKAGE BYTE COMPILATION
;;      These are admistrative utilies for package maintainer(s)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defsubst tiny-setup-file-list-lisp (dir)
  "Return all lisp files under DIR."
  (directory-files dir 'full "\\.el$"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tiny-setup-file-list-lisp-compiled (dir)
  "Return all compiled lisp files under DIR."
  (directory-files dir 'full "\\.elc$"))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory (dir &optional function)
  "Compile all isp files in DIRECTORY.
Optional FUNCTION is passed one argument FILE, and it should return
t or nil if file is to be compiled."
  (dolist (file (tiny-setup-file-list-lisp dir))
    (when (or (null function)
              (funcall function file))
      (byte-compile-file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory-recursive (root &optional function)
  "Compile all files under ROOT directory.
Optional FUNCTION is passed one argument FILE, and it should return
t or nil if file is to be compiled."
  (tiny-setup-directory-recursive-macro
   root
   (message "TinySetup: compiling directory %s" dir)
   (tiny-setup-compile-directory
    dir function)))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-directory-delete-recursive (root)
  "Delete all compiled files under ROOT directory recursively."
  (tiny-setup-directory-recursive-macro
   root
   (dolist (file (tiny-setup-file-list-lisp-compiled dir))
     (message "TinySetup: deleting compiled file %s" file)
     (delete-file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-kit-libraries (dir)
  "Compile tiny tools libraries"
  (tiny-setup-directory-recursive-macro
   dir
   (let ((libs (directory-files dir 'abs-path "tinylib.*\\.el$")))
     (when libs	;;  Found correct directory
       (message "TinySetup: compiling libraries in right order.")
       (let ((default-directory dir)
	     compile-file)
	 ;; There is certain order of compilation. Low level libraries first.
	 (dolist (regexp tiny-setup-:library-compile-order)
	   (when (setq compile-file ;; compile these first
		       (find-if (function
				 (lambda (elt)
				   (string-match regexp elt)))
				libs))
	     (setq libs (delete compile-file libs))
	     (byte-compile-file compile-file)))
	 (message "TinySetup: compiling rest of the libraries.")
	 (dolist (file libs) ;; Rest of the libraries
	   (cond
	    ((find-if (function
		       (lambda (regexp)
			 (string-match regexp file)))
		      tiny-setup-:library-compile-exclude)
	     (message "TinySetup: ignoring library %s" file))
	    (t
	     (byte-compile-file file)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tiny-setup-compile-kit-all (&optional dir)
  "Compile tiny tools kit under DIR.
This function can be called from shell command line, where the
last argument is the DIR from where to start compiling.

Notice that there is `.' at the end of call to `tiny-setup-compile-kit-all':

$ cd root-dir
$ find . -name \"*elc\" -exec rm {} \\;
$ emacs -batch -l load-path.el -l tiny-setup.el -f tiny-setup-compile-kit-all .

If only the libraries need compilation, use this command:

$ emacs -batch -l load-path.el -l tiny-setup.el -f -eval '(tiny-setup-compile-kit-libraries \".\")

If only one file needs to be compiled:

$ emacs -batch -l load-path.el -l tiny-setup.el -f -eval batch-byte-compile <file>"
  (interactive "D[compile] installation root dir: ")
  (unless dir
    (setq dir (car-safe command-line-args-left)))
  (if dir                               ;Require slash
      (setq dir (file-name-as-directory dir))
    (error "Compile under which DIR? Give parameter"))
  (message "tinySetup: byte compiling root %s" dir)
  ;;  Remove compiled files first
  (tiny-setup-compile-directory-delete-recursive dir)
  ;;  Libraries first
  (tiny-setup-compile-kit-libraries dir)
  ;;  The rest follows, it doesn't matter if libs are are compiled twice.
  (tiny-setup-compile-directory-recursive
   dir
   (function
    (lambda (x)
      (not (string-match "tinylib" x))))))

(provide 'tiny-admin-util)

;;; tiny-admin-util.el ends here
