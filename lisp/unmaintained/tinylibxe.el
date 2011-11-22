;;; tinylibxe.el --- Compatibility library for both Emacs and XEmacs

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tilibxe-version.
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

;; ........................................................ &t-install ...
;;
;; Note: 2010-11-20 This library is obsolete and no longer maintained.
;; You're free to take over the maintenance. Use at your own risk.
;;
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; package that you're currently developing:
;;
;;      (require 'tinylibxe)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface 1996
;;
;;      o   This is library, package itself does nothing.
;;      o   Compatibility for both Emacsen, XEmacs and Emacs
;;      o   Compatibility for older Emacsen. Code written using later Emacs
;;          versions can be run under lower Emacs version. (e.g.
;;          `require' includes extra parameters in later Emacs versions.
;;
;;      There are incompatibilities between XEmacs and Emacs which
;;      prevent writing portable code. The bigger problematic things
;;      have been collected here. The trivial ones have been implemented
;;      in lower level libraries like in backward compatibility
;;      library *tinylibb.el*.
;;
;;  Overlay.el in XEmacs 19.15+
;;
;;      Good news; Latest XEmacs includes package that emulates
;;      Emacs overlay functions. Load it under XEmacs, if you run code
;;      written using Emacs.
;;
;;  What you should know -- keep this in mind
;;
;;      This library's intention is to make it possible to help using packages
;;      that are written only for XEmacs. Normally it is not possible to use
;;      package under another Emacs flavor, because there may be function calls
;;      differences.
;;
;;      When this file is loaded, it emulates unknown functions as much as
;;      it can. However, it may not be possible to reproduce exactly the
;;      same behavior that was not the primary target for the package. The
;;      emulation may at worst case be only so, that you are able to load
;;      the package without errors, but the functionality of the package
;;      doesn't correspond to the original's.

;;}}}

;;; Change Log:

;;; Code:

;; Note: it is safe to ignore Emacs dependant ange-ftp function warnings.

(require 'tinylibm)

(defconst tinyliba-version-time "2010.1129.0656"
  "Latest version number as last modified time.")

;;{{{ events, window, frames, misc

(cond
 ((ti::emacs-p)
  (defalias 'event-window       'posn-window)
  (defalias 'event-point        'posn-point)
  (defalias 'event-timestamp    'posn-timestamp)
  (defalias 'window-pixel-edges 'window-edges))
 (t
  (defalias 'posn-window        'event-window)
  (defalias 'posn-window        'event-window)
  (defalias 'posn-point         'event-point)
  (defalias 'posn-timestamp     'event-timestamp)
  (defalias 'help-print-return-message 'print-help-return-message)
  ;;  (defalias 'posn-col-row    ')
  (defalias 'window-edges       'window-pixel-edges)))

;;; From wid-edit.el
;;;
(ti::fboundp-check-autoload
 'button-release-event-p "tinylibxe"
 ;; XEmacs function missing from Emacs.
 (defun button-release-event-p (event)
   "Non-nil if EVENT is a mouse-button-release event object."
   (and (eventp event)
	(memq (ti::funcall 'event-basic-type event)
	      '(mouse-1 mouse-2 mouse-3))
	(or (memq 'click (event-modifiers event))
	    (memq  'drag (event-modifiers event))))))

(ti::fboundp-check-autoload
 'event-start "tinylibxe"
 (defun event-start (event)
   "tinylibxe.el"
   ;; In Emacs (WINDOW BUFFER-POSITION (X . Y) TIMESTAMP)
   (list
    (ti::funcall 'event-window event)
    (ti::funcall 'event-point event)
    (ti::funcall 'posn-x-y event)
    (ti::funcall 'event-timestamp event))))

(ti::fboundp-check-autoload
 'event-x "tinylibxe"
 (defun event-x (event)
   "tinylibxe.el"
   (let* ((data (ti::funcall 'event-start event)))
     (car data))))

(ti::fboundp-check-autoload
 'event-y "tinylibxe"
 (defun event-y (event)
   "tinylibxe.el"
   (let* ((data (ti::funcall 'event-start event)))
     (cdr data))))

(ti::fboundp-check-autoload
 'posn-x-y "tinylibxe"
 (defun posn-x-y (event)
   "tinylibxe.el"
   (cons (ti::funcall 'event-x event) (ti::funcall 'event-y event))))

(when (and (not (fboundp 'frame-parameters)) ;obsolete in 19.14
           (boundp 'frame-properties))
  (defun frame-parameters (&optional frame)
    "Return FRAME parameters."
    ;;  Emacs   ((arg1 . val) (arg2 . val) ..)
    ;;  XEmacs  (arg val arg2 val)
    (ti::list-to-cons (ti::funcall 'frame-properties))))

;;}}}
;;{{{ faces

;;; XEmacs /Emacs don't have compatible faces

(and (not (fboundp 'x-display-color-p))
     (fboundp 'device-class)
     (defalias 'x-display-color-p 'device-class))

(unless (ti::compat-face-p 'region)
  (make-face 'region)
  (set-face-foreground 'region "white")
  (set-face-background 'region "black"))

(ti::fboundp-check-autoload 'set-background-color "tinylibxe"
  (defun set-background-color (colour)
    "Tinylibxe. Emacs emulation"
    (ti::funcall 'set-face-background 'default colour)))

(ti::fboundp-check-autoload 'set-foreground-color "tinylibxe"
  (defun set-foreground-color (colour)
    "Tinylibxe. Emacs emulation"
    (ti::funcall 'set-face-foreground 'default colour)))

(ti::fboundp-check-autoload 'set-cursor-color "tinylibxe"
  (defun set-cursor-color (colour)
    "Tinylibxe. Emacs emulation"
    (ti::funcall 'set-face-foreground 'text-cursor colour)))

(ti::fboundp-check-autoload 'transient-mark-mode "tinylibxe"
  (defun transient-mark-mode (&optional mode)
    "Tinylibxe. Emacs emulation"
    (interactive)
    (set 'zmacs-regions (ti::bool-toggle mode))))

;;}}}
;;{{{ dired

(cond
 ((ti::emacs-p)
  (defalias  'dired-unmark-subdir-or-file   'dired-unmark)
  (defalias  'dired-mark-subdir-or-file     'dired-mark)
  (defalias  'dired-mark-get-files          'dired-get-marked-files)
  (defalias  'dired-mark-map                'dired-map-over-marks))
 (t
  (defalias 'dired-unmark           'dired-unmark-subdir-or-file)
  (defalias 'dired-mark             'dired-mark-subdir-or-file)
  (defalias 'dired-get-marked-files 'dired-mark-get-files)
  (defalias 'dired-map-over-marks   'dired-mark-map)))

;;}}}
;;{{{ glyphs

;;; Thanks to Kyle Jone,  kyle@wonderworks.com, in setnu.el

(when (and nil (ti::emacs-p)) ;; disabled now
  (defalias 'extent-live-p              'overlayp)
  (defalias 'extentp                    'overlayp)
  (defalias 'make-extent                'make-overlay)
  (defalias 'delete-extent              'delete-overlay)
  (defalias 'extent-property            'overlay-get)
  (defalias 'set-extent-property        'overlay-put)
  (defalias 'set-extent-endpoints       'move-overlay)
  (defalias 'extent-end-position        'overlay-end)
  (defalias 'extent-start-position      'overlay-start)
  (defalias 'extent-start-position      'overlay-buffer)
  (defalias 'extent-start-position      'overlay-buffer)
  (defalias 'next-extent-change         'next-overlay-change)
  (defalias 'extent-properties          'overlay-properties)

  (defun extent-list (buffer point)
    "tinylibxe.el -- arg3 not supported."
    (save-excursion
      (if buffer
          (set-buffer buffer))
      (ti::funcall 'overlays-at point)))

  (defun extent-length (e)
    "tinylibxe.el -- return overlay length."
    (- (ti::funcall 'overlay-end e) (ti::funcall 'overlay-start e))))

(defvar ti:xe-begin-glyph-property (if (fboundp 'extent-property)
                                       'begin-glyph
                                     'before-string)
  "Property name to use to set teh begin glyph of an extent.")

(ti::fboundp-check-autoload  'set-overlay-begin-glyph "tinylibxe"
  (defun set-overlay-begin-glyph (e g)
    "tinylibxe -- Set glyph G in overlay E."
    (ti::funcall 'overlay-put e ti:xe-begin-glyph-property g)))

(ti::fboundp-check-autoload  'make-glyph "tinylibxe"
  (defalias 'make-glyph 'identity))

(cond
 ((ti::emacs-p)
  (unless (fboundp  'set-glyph-face)
    (defun set-glyph-face (g face)
      "tinylibxe -- Set glyph G to FACE"
      (put-text-property 0 (length g) 'face face g))))
 (t
  ;;(defalias 'set-glyph-face 'ignore)
  nil))

;;}}}
;;{{{ misc

(defun-maybe frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.
If FRAME is nil, describe the currently selected frame."
  (assq parameter (fram-parameters frame)))

;;  XEmacs : replace-in-string
;;  Included in Emacs 20.4
(defun-maybe subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
INPLACE is ignored."
  (let ((len   (length string))
        (ret   (copy-sequence string))) ;because 'aset' is destructive
    (while (> len 0)
      (if (char-equal (aref string (1- len)) fromchar)
          (aset ret (1- len) tochar))
      (decf len))
    ret))

(when (and nil                          ;Idea only...
           (not (fboundp 'easy-menu-add-item))
           (fboundp 'add-menu-button))
  (defun easy-menu-add-item ()
    (cond
     ((fboundp 'easy-menu-add-item)     ;XEmacs 21.x
      (easy-menu-add-item 'rest-of-the-args))
     ((fboundp 'add-menu-button)        ;XEmacs
      ;; (add-menu-button
      ;; '("Tools")
      ;; ["List Ediff Sessions" ediff-show-registry t] "OO-Browser...")
      nil)
     (t
      (define-key
        ;; support for pre FSF 20.3
        'nothing-yet
        'nothing-yet)))))

;;; From wid-edit.el by Per Abrahamsen <abraham@dina.kvl.dk>
(when (and (not (fboundp 'error-message-string))
           (fboundp 'display-error))
  ;; Emacs function missing in XEmacs.
  (defun error-message-string (obj)
    "Convert an error value to an error message."
    (let ((buffer (get-buffer-create " *error-message*")))
      (with-current-buffer buffer
        (erase-buffer)
        ;;  Only exist in new emacs release
        (ti::funcall 'display-error obj buffer)
        (buffer-string)))))

;; XEmacs  doesn't have 'timer package; but 'itimer
(ti::fboundp-check-autoload 'run-at-time "tinylibxe"
  (defun run-at-time  (time repeat function &rest args)
    "tinylibxe -- XEmacs and Emacs Compatibility."
    (require 'itimer)
    ;;  start-itimer: (name function value &optional restart)
    ;;  start-itimer: (NAME FUNCTION VALUE &optional RESTART IS-IDLE WITH-ARGS
    ;;                 &rest FUNCTION-ARGUMENTS)
    ;;  We can't use following Emacs arguments: ARGS
    ;;  (run-at-time TIME REPEAT FUNCTION &rest ARGS)
    (ti::funcall
     'start-itimer
     (cond    ;ARG1 NAME
      ((symbolp function)
       (symbol-name function))
      (t
       "itimer-with-no-name"))
     function              ;ARG2 FUNCTION
     (if (integerp repeat) ;ARG3 VALUE
	 repeat 10)
     (if (integerp repeat) ;ARG4 &optional RESTART
	 repeat 10))))

(ti::fboundp-check-autoload  'cancel-timer "tinylibxe"
  (defun cancel-timer (timer)
    "tinylibxe -- XEmacs & Emacs Compatibility."
    (ti::funcall 'delete-itimer timer)))

;;}}}
;;{{{ advice: code from XEmacs --> Emacs

(when (ti::xemacs-p)
  (require 'advice)

  (unless (string-match "NOERROR" (documentation 'require))
    (defadvice require (around tinylibxe (feature &optional filename noerror) act)
      "tinylibxe -- Add support for 3rd parameter NOERROR."
      (cond
       ((null noerror)
	ad-do-it)
       (t
	(ignore-errors ad-do-it)))))

  (unless (string-match "RSTART" (documentation 'count-matches))
    (defadvice count-matches
      (around tinylibxe
	      (regexp &optional rstart rend interactive) act)
      "tinylibxe -- Add support for RSTART REND. Ignores arg 4."
      (cond
       (rend
	(save-restriction
	  (narrow-to-region (point) rend)
	  ad-do-it))
       (rstart
	(save-restriction
	  (narrow-to-region rstart (point-max))
	  ad-do-it))
       (t
	ad-do-it))))

  ;;  This is same as 'beep'
  ;;  Emacs, subr.el:(defalias 'beep 'ding) ;preserve lingual purity
  (defadvice ding (around tinylibxe (&optional arg &rest args) act)
    "tinylibxe -- Define Xemacs compatible ding comamnd. Ignores arg 2."
    ad-do-it)

  ;; Emacs includes more arguments
  (when (and (fboundp 'read-char-exclusive)
	     (not (string-match "prompt"
				(or (ti::function-args-p 'read-char-exclusive) ""))))

  (defadvice read-char-exclusive (around tinylibxe (message))
    "Emacs compatibility. Add parameters PROMPT INHERIT-INPUT-METHOD,
but INHERIT-INPUT-METHOD is not supported."
    (if (stringp prompt)
	(message prompt))
    (setq ad-return-value (read-char-exclusive))))

  (defadvice make-sparse-keymap (before tinylibxe (&optional no-op) act)
    "tinylibxe -- This advice does nothing except adding an optional argument
to keep the byte compiler happy when compiling Emacs specific code
with XEmacs."))

;;}}}

(provide 'tinylibxe)

;;; tinylibxe.el ends here
