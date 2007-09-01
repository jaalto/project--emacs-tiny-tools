;;; tinylib-ad.el --- Library of adviced functions. Backward compatibility
;; $Id: tinylib-ad.el,v 2.45 2007/05/01 17:20:44 jaalto Exp $

;;{{{ Id

;; Copyright (C)    1999-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylib-ad-version
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
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;      (require 'tinylibm)    ;; Yes, there is no mistake. You require the "m"

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1999
;;
;;      This is lisp function library, package itself does nothing.
;;      It modifies existing Emacs functions with advice.el in order
;;      to provide backward compatibility for functions that have changed.
;;      E.g. Emacs 20.4 introduced new parameter NOERR to `require' command.
;;
;;      There is another way, load this library first and continue using
;;      your current Emacs version. This package will redefine functions
;;      only when needed, so it should be quite safe.
;;
;;  Usage
;;
;;      You must not autoload this package; but always include
;;
;;          (require 'tinylibm)
;;
;;      You don't need any other require commands: all other library
;;      functions get defined as well by using autoload. Repeat: you don't
;;      have to put these in your packages:
;;
;;          (require 'tinylib)     ;; leave this out
;;          (require 'tinyliby)    ;; not needed either.
;;          (require 'tinylib-ad)  ;; not needed either.

;;}}}

;;; Change Log:

;;; Code:

(require 'tinylibb)

(eval-when-compile
  (require 'advice))

(when (and (ti::emacs-p)
           (not (ti::emacs-p "20.2")))
  (defadvice shell-command (after tiny act)
    "The OUTPUT-BUFFER does not work in old releases. Fix it."
    (let* ((buffer (ad-get-arg 1)))
      (when (and buffer
                 (get-buffer buffer)
                 (get-buffer shell-command-output-buffer))
        (with-current-buffer buffer
          (erase-buffer)
          (insert-buffer shell-command-output-buffer)
          (ti::kill-buffer-safe shell-command-output-buffer))))))

;; Emacs 20.1 inroduced new argument (buffer-size &optional BUFFER)
(unless (string-match "buffer"
                      (or (ti::function-args-p 'buffer-size) ""))
  (defadvice buffer-size
    (around tinylib-ad (feature &optional buffer) act)
    "Emacs compatibility: optional parameter BUFFER."
    (if buffer
        (with-current-buffer buffer
          ad-do-it)
      ad-do-it)))

;;  Emacs 21.1
;;  (define-key-after KEYMAP KEY DEFINITION &optional AFTER)
;;  Emacs 20.7
;;  (define-key-after KEYMAP KEY DEFINITION AFTER)

(when (and (fboundp 'define-key-after)
           (string-match "optional"
                         (or (ti::function-args-p 'define-key-after) "")))
  (defadvice define-key-after
    (around tinylib-ad (keymap key definition &optional after) act)
    "Emacs compatibility: parameter AFTER is now optional."
    ad-do-it))

;; 20.4 introduced new arg NOERR
;; (require FEATURE &optional FILE-NAME NOERROR)

(unless (string-match "noerr"
                      (or (ti::function-args-p 'require) ""))
  (defadvice require
    (around tinylib-ad (feature &optional file-name noerror) act)
    "Emacs compatibility: Added parameter NOERR."
    (let* ((noerr (ad-get-arg 2)))
      (if noerr
          (or (featurep feature)
              (load (or file-name (symbol-name feature)) 'noerr 'nomsg))
        ad-do-it))))

;; Emacs includes more arguments

(when (and (fboundp 'read-char-exclusive)
           (not (string-match "prompt"
                              (or (ti::function-args-p 'read-char-exclusive) ""))))
  (defadvice read-char-exclusive
    (around tinylib-ad (&optional prompt inherit-input-method) act)
    "Emacs compatibility. Added parameters PROMPT INHERIT-INPUT-METHOD,
but INHERIT-INPUT-METHOD is not supported."
    (message prompt)
    (setq ad-return-value (read-char-exclusive))))

;; Older versions of `executable-find' did not search ".exe" or ".com" ...
;; extensions. This was true for XEmacs 21.2 also. In Emacs 20.4 it's ok.
;;
;; We instantiate this advice if it can't pass the test.

(when (and (ti::win32-p)
           ;;  Try and see if  executable-find adds extension .com and
           ;;  .exe, if these fail, then fix it.
           (let ((exec-path exec-path))
             (push "c:/windows" exec-path)
             (push "c:/winnt" exec-path)
             (null (or (executable-find "command")
                       (executable-find "cmd")))))
  (defadvice executable-find (around tinylib-ad act)
    "Replace function. In win32, try also extension .com .exe .bat ..."
    (let ((command (ad-get-arg 0))
          ret)
      (dolist (ext '(".exe" ".com" ".bat" ".cmd" ".btm" ""))
        (if (setq ret (ti::file-get-load-path
                       (concat command ext) exec-path))
            (return ret)))
      (setq ad-return-value ret))))

;;{{{ Version

;;; ......................................................... &version ...

(defconst tinylib-ad-version
  (substring "$Revision: 2.45 $" 11 15)
  "Latest version number.")

(defconst tinylib-ad-version-id
  "$Id: tinylib-ad.el,v 2.45 2007/05/01 17:20:44 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylib-ad-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylib-ad.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylib-ad-submit-bug-report ()
  "Submit bug report."
  (interactive)
  (ti::package-submit-bug-report
   "tinylib-ad.el"
   tinylib-ad-version-id
   '(tinylib-ad-version-id)))

;;}}}

(provide   'tinylib-ad)

;;; tinylib-ad.el ends here
