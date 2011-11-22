;;; tinylib-ad.el --- Library of adviced functions. Backward compatibility

;; Copyright (C)    1999-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto

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

;;; Install:

;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file
;;
;;      (require 'tinylib-ad)

;;; Commentary:

;;  Preface, 1999
;;
;;      This is lisp function library, package itself does nothing.
;;      It modifies existing Emacs functions with advice.el in order
;;      to provide backward compatibility for functions that have changed.
;;      E.g. Emacs 20.4 introduced new parameter NOERR to `require' command.
;;
;;      Load this library first and continue using your current Emacs
;;      version. This package will redefine functions only when
;;      needed, so it should be quite safe.
;;
;;	2010-11-29 Note: These compatibility functions are obsolete, now
;;	when Emacs is at version 23.x
;;
;;  Usage
;;
;;      This package cannot be autoloaded.

;;; Change Log:

;;; Code:

(require 'tinylibb)

(eval-when-compile
  (require 'cl)
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
    (let ((noerr (ad-get-arg 2)))
      (if noerr
          (or (featurep feature)
              (load (or file-name (symbol-name feature)) 'noerr 'nomsg))
        ad-do-it))))

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

(provide 'tinylib-ad)

;;; tinylib-ad.el ends here
