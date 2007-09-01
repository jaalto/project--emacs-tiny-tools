;;; load-path.el --- Used for compiling Emacs lisp files
;;; $Id: load-path.el,v 2.39 2007/05/01 17:20:41 jaalto Exp $

;;; Commentary:

;;
;;  File id
;;
;;      Copyright (C)  1997-2007 Jari Aalto
;;      Keywords:      extensions
;;      Author:        Jari Aalto
;;      Maintainer:    Jari Aalto
;;
;;      This code is free software in terms of GNU Gen. pub. Lic. v2 or later
;;
;;  Description
;;
;;      This file part of the Tiny Tools Kit for Emacs: collection of
;;      various utilities.
;;
;;      Before compiling, this file is included via -l FILE switch and it
;;      defines correct load path in order to find the files that are needed
;;      in compilation. If your private directory is not in ~/elisp or
;;      ~/lisp then add new path to the place shown below.

;;; Change Log:

;;; Code:

(require 'cl)
(setq debug-on-error nil) ;; Must be like this in batch byte compile

(autoload 'ti::package-autoload-create-on-file            "tinylib")
(autoload 'ti::package-autoload-loaddefs-build-recursive  "tinylib")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      PLEASE CONFIFURE THIS `dolist' to include paths in your system
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (path
         '(

           ;; Define any new path HERE. It won't matter if you
           ;; define non-exiting paths, they are stripped away.
           ;;
           ;;  some users prefer the directory called ~/lisp istead of
           ;;  ~/elisp (Emacs Lisp)

           "~/elisp"
           "~/lisp"

           ;;  Unix: Posisbly the best is to have
           ;;  this directory to be a symbolic link to latest distribution
           ;;
           ;;  Win32: Symbolic links don't work, change this to the absolute
           ;;  path of the kit location directories.

           "~/elisp/tiny"
           "~/elisp/tiny/lisp"
           "~/elisp/tiny/lisp/tiny"
           "~/elisp/tiny/lisp/other"

           ;;  Any other directories that you have in you ~/elips or

           "/usr/share/site-lisp"
           "/usr/share/site-lisp/net"

           ;; The best way to keep up with the development is to
           ;; use CVS. See BBDB and Gnus sites for CVS.

           "/usr/share/site-lisp/net/cvs-packages"
           "/usr/share/site-lisp/net/cvs-packages/bbdb/lisp"
           "/usr/share/site-lisp/net/cvs-packages/gnus/lisp"

           ;;  Any other directories that you have in you ~/elips or
           ;;  site wide /usr/share/site-lisp or under /opt hierarchy

           "~/elisp/other"
           "~/elisp/bbdb/lisp"        ;usually symbolic link to latest
           "~/elisp/rc"
           "."
           ".."
           "../other"
           "../.."))
  (when (file-exists-p path)
    (pushnew (expand-file-name path) load-path :test 'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      LOAD PATH self-check
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tiny-tmp-load-path-print (&optional function eval)
  "Print `load-path' using `function'.
Default is `message'. Eval optional EVAL."
  (let ((i 0))
    (if eval
        (eval eval))
    (dolist (path load-path)
      (if function
          (funcall function "  %d %s" i path)
        (message "  %d %s" i path))
      (incf i))))

(eval-and-compile
  ;;  Remove comment if you want to  see the load path
  ;;  before compiling starts. The printed path (order) may give a clue
  ;;  why compile might have failed.

  ;; (tiny-tmp-load-path-print)

  ;;  Check that load-path is in order
  (let ((path (locate-library "tinylibm")))
    (if path
        nil ;; (message "FOUND: %s" path)
      (tiny-tmp-load-path-print)
      (message
       "\
  **  Can't find library [tinylibm]. Please update
      file [load-path.el] which sets up load-path for compilation purposes."))))

;;; load-path.el ends here
