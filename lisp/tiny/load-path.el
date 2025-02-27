;;; load-path.el --- Developer only. Used for compiling Emacs lisp files

;; DO NOT TOUCH - THIS IS NOT A USER CONFIGURATION FILE
;; Instead write your won and use:
;;
;; cd bin/
;; make LIB=your-load-path.el compile

;;; Commentary:

;;
;;  File id
;;
;;      Copyright (C) 1997-2025 Jari Aalto
;;
;;      This program is free software; you can redistribute it and/or
;;      modify it under the terms of the GNU General Public License as
;;      published by the Free Software Foundation; either version 2 of
;;      the License, or (at your option) any later version.
;;
;;      This program is distributed in the hope that it will be
;;      useful, but WITHOUT ANY WARRANTY; without even the implied
;;      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;      PURPOSE. See the GNU General Public License for more details.
;;
;;      You should have received a copy of the GNU General Public License
;;      along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;      Visit <http://www.gnu.org/copyleft/gpl.html> for more information
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

;;; Code:

(eval-when-compile
  (or (require 'cl-lib nil 'noerr) ;; Emacs 29.x
      (require 'cl)))

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

	   ;; Directory that contains symbolic links to ALL
	   ;; emacs packages

           "~/var/link/emacs"

           ;; More diretories

           "~/.emacs.d"
           "~/.emacs.d/epackage/packages/htmlize"
           "~/.emacs.d/epackage/packages/igrep"
           "~/.emacs.d/epackage/packages/bbdb/lisp"
           "~/elisp"
           "~/lisp"

           ;;  Windows: Symbolic links don't work,

           "~/elisp/tiny"
           "~/elisp/tiny/lisp"
           "~/elisp/tiny/lisp/tiny"
           "~/elisp/tiny/lisp/other"

           ;;  Any other directories

           "/usr/share/site-lisp"
           "/usr/share/site-lisp/net"

           ;; The best way to keep up with the development is to
           ;; use version ontrol.

           "/usr/share/site-lisp/net/packages"
           "/usr/share/site-lisp/net/packages/bbdb/lisp"
           "/usr/share/site-lisp/net/packages/gnus/lisp"

           ;;  More directories

           "~/elisp/other"
           "~/elisp/bbdb/lisp"
           "~/elisp/rc"
           "."
           ".."
           "../other"
           "../.."))
  (when (file-exists-p path)
    (cl-pushnew (expand-file-name path) load-path :test 'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;      LOAD PATH self-check
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
      (setq i (1+ i)))))

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
