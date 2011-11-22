;;; tinyxreg.el --- Restore points and window configuration with X-popup

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at the code with folding.el.

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

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;    (when window-system
;;      (global-set-key "\C-x/"  'tinyxreg-point-to-register)
;;      (global-set-key "\C-x\\" 'tinyxreg-remove-register)
;;      ;;  The non-graphical "jump" is at C-x j
;;      (global-set-key "\C-cj"  'tinyxreg-jump-to-register)
;;      (require 'tinyxreg))
;;
;; Or use autoload, preferfed, because your emacs starts up faster.
;;
;;    (when window-system
;;      (global-set-key "\C-x/"  'tinyxreg-point-to-register)
;;      (global-set-key "\C-x\\" 'tinyxreg-remove-register)
;;      (global-set-key "\C-cj"  'tinyxreg-jump-to-register)
;;      (autoload 'tinyxreg-jump-to-register        "tinyxreg" "" t)
;;      (autoload 'tinyxreg-jump-to-register-mouse  "tinyxreg" "" t)
;;      (autoload 'tinyxreg-point-to-register       "tinyxreg" "" t)
;;      (autoload 'tinyxreg-point-to-register-mouse "tinyxreg" "" t)
;;      (autoload 'tinyxreg-remove-reg              "tinyxreg" "" t)
;;      (autoload 'tinyxreg-trash                   "tinyxreg" "" t))

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Oct 1995
;;
;;      There was a post in comp.emacs by  <cpg@cs.utexas.edu> Carlos Puchol
;;
;;          I find that my life would be remarkably eased if only I could
;;          "jump" to the marks from a menu. Please, let me know if i can
;;          implement this myself through some sort of macro or something.
;;
;;      It was an interesteing idea and some sketching was flying in the air.
;;      The original plan wasn't to write any serious code; just tossing
;;      around some experiments with of functions.
;;      As a result it soon become a complete package and after a while
;;      a properly packaged set.
;;
;;  Overview of features
;;
;;      o   Store points and window configurations to registers.
;;      o   Use popup to pick register associated with the file. In short
;;          this package offers graphical user interface for the the
;;          C-x j "jump to register".
;;
;; Register update note
;;
;;      If you wonder why some of the registers disappear from the popup
;;      while you were sure you just stored some point to them, the reason
;;      is that If you kill some buffer, or reload it again with
;;      find-alternate-file that means that the register reference "dies".
;;      That's why the main function tinyxreg-jump-to-register calls a
;;      house keeping function tinyxreg-update to make sure you can't
;;      select invalid registers. So, trust the poup: it tells what
;;      registes are available.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: libraries

(require 'tinylibm)

(ti::package-defgroup-tiny TinyXreg tinyxreg-- tools
  "Restoring points/win cfg stroed in reg. via X-popup
  Overview of features

      o   Store points and window configurations to registers.
      o   Use popup to pick register associated with the file. In short
          this package offers graphical user interface for the the
          C-x j \"jump to register\".")

;;}}}
;;{{{ setup: hooks

(defcustom tinyxreg--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyXreg)

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinyxreg--x-coord 170
  "*Default menu coordinate."
  :type  'integer
  :group 'TinyXreg)

(defcustom tinyxreg--y-coord 170
  "*Default menu coordinate."
  :type  'integer
  :group 'TinyXreg)

(defcustom tinyxreg--description-func  'tinyxreg-description
  "*Function to return popup description string.
Function should accept two arguments: REGISTER and WINDOW-ARG"
  :type 'function
  :group 'TinyXreg)

(defcustom tinyxreg--title  "Register list"
  "*Popup title."
  :type  'string
  :group 'TinyXreg)

(defcustom tinyxreg--buffer-fmt "%-20s"
  "*Format for filename.
Filename length reserved for default popup description.

Note:  The entries itself are stored in this form, so changing this
affects only new entries."
  :type  '(string :tag "Format string")
  :group 'TinyXreg)

(defcustom tinyxreg--wcfg-fmt '(concat "\177 Win " bn)
  "*Lisp form to for window configuration.
This is the Window config FORM that is evaled when
the description is put into the list. You can use variable BN
to refer current buffer name.

Remember that list will be sorted later, so you may want to have
common beginning for all win cfg registers."
  :type  '(sexp :tag "Lisp form")
  :group 'TinyXreg)

;;}}}
;;{{{ setup: private

(defvar tinyxreg--preg  nil
  "Hold point markers.")

(defvar tinyxreg--wreg  nil
  "Hold window markers.")

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-event ()
  "Return fake event."
  (ti::compat-make-fake-event tinyxreg--x-coord tinyxreg--y-coord))

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-list ()
  "Return register list, point list + window list."
  (let ((ptr   tinyxreg--wreg)
	(list  (copy-sequence tinyxreg--preg)))
    ;;  concat two lists
    (dolist (elt ptr)
      (push elt list))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-install-default-key-bindings ()
  "Install default key bindings."
  (interactive)
  ;;  There is no other good use for these
  (global-set-key "\C-x/"           'tinyxreg-point-to-register)
  (global-set-key "\C-x\\"          'tinyxreg-remove-register)
  ;;  The "C-c j" is like C-x j , but showing the popup
  (global-set-key "\C-cj"           'tinyxreg-jump-to-register)
  ;;  C-x is so easy to reach with left hand... and free
  (global-set-key [(control c) (mouse-1)]   'tinyxreg-jump-to-register-mouse)
  (global-set-key [(control c) (shift mouse-1)] 'tinyxreg-point-to-register-mouse)
  (when (interactive-p)
    (message "TinyXreg: Register Keys bound ok.")))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyxreg-remove-reg (char &optional arg)
  "Remove register CHAR from stored window and point lists.
ARG suggests looking in window list."
  (interactive "cRemove register: \nP")
  (let ((ptr (if arg
		 tinyxreg--wreg
	       tinyxreg--preg))
	elt)
    (when (setq elt (rassq char ptr))
      (if arg
          (setq tinyxreg--wreg (delete elt tinyxreg--wreg))
        (setq tinyxreg--preg (delete elt tinyxreg--preg))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-update ()
  "Kill all registers from lists that are not alive any more.
Eg. marker dies if you revert the buffer; kill and load it again."
  (let ((ptr tinyxreg--preg)
	reg
         list)
    ;;  We simple copy valid elements to another list
    (dolist (elt ptr)
      (setq reg (cdr elt))
      (if (ti::register-live-p reg)
          (push elt list)))
    (setq tinyxreg--preg list)
    (setq ptr tinyxreg--wreg)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyxreg-trash ()
  "Empties both window and point caches."
  (interactive)
  (setq tinyxreg--preg nil   tinyxreg--wreg nil)
  (if (interactive-p)
      (message "TinyXreg: Register lists trashed.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-kill-reg (char)
  "Kill register CHAR from all lists."
  (tinyxreg-remove-reg char nil)
  (tinyxreg-remove-reg char 'window))

;;; ----------------------------------------------------------------------
;;;
(defun tinyxreg-add-reg (char arg &optional desc)
  "Store register CHAR to window or point list.
ARG tells to store to window list. DESC is string to use."
  (let* ((desc (if (stringp desc)
                   desc
                 (char-to-string char)))
         (data (cons desc char)))
    (if arg
        (push data tinyxreg--wreg)
      (push data tinyxreg--preg))))

;;}}}
;;{{{ storing

;; ----------------------------------------------------------------------
;;; So that you call this from mouse
;;;
(defun tinyxreg-description (register &optional arg)
  "Return description text for popup list.
REGISTER is stored register and if ARG is non-nil the register
contains window configuration."
  (let ((bn   (file-name-nondirectory (buffer-name)))
	(cfg  tinyxreg--wcfg-fmt))
    (format (concat tinyxreg--buffer-fmt " %4s %s")
            (if arg
                ;;  the 177 should print nice block
                ;;  so that sorting puts cfg entries last
                (eval cfg)
              bn)
            (if arg
                ""
              (number-to-string
               (count-lines (point-min-marker) (line-beginning-position))))
            (char-to-string register))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyxreg-point-to-register-mouse (event)
  "Call `tinyxreg-point-to-register' using mouse EVENT."
  (interactive "e")
  ;;    - User using "flying" mouse paste mode? See var mouse-yank-at-point
  ;;    - If he is, then move cursor visually to mouse point first.
  (if (null mouse-yank-at-point)
      (mouse-set-point event))
  (call-interactively 'tinyxreg-point-to-register))

;;; ----------------------------------------------------------------------
;;; based on register.el::point-to-register
;;;
;;;###autoload
(defun tinyxreg-point-to-register (char &optional arg)
  "Store point to CHAR and to X-popup list.
With prefix ARG, store current frame configuration. VERBOSE enables
message printing.

Use \\[tinyxreg-point-to-register]  to go to that location or restore the
configuration."
  (interactive
   (list
    (let (CHAR
          (msg
           (cond
            (current-prefix-arg
             "TinyXreg: Store Window cfg to register: " )
            (t
             "TinyXreg: Store point to register: "))))
      (setq CHAR (ti::read-char-safe-until msg))
      ;;  Show where it got stored.
      (message (concat msg (char-to-string CHAR)))
      CHAR)
    current-prefix-arg))
  (let ((dfunc tinyxreg--description-func)
	desc)
    (setq desc                          ;get the popup description
          (if (fboundp dfunc)
              (funcall dfunc char arg)
            nil))
    (tinyxreg-remove-reg char arg)
    (tinyxreg-add-reg    char arg desc)
    (set-register ;;   Now the normal emacs thing
     char
     (if (null arg)
         (point-marker)
       (current-frame-configuration)))))

;;}}}
;;{{{ jumping

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyxreg-remove-register ()
  "Remove register from popup list.
See `tinyxreg-jump-to-register-mouse' for more."
  (interactive)
  (tinyxreg-jump-to-register-mouse nil 'remove))

;;; ----------------------------------------------------------------------
;;; - for calling from keybord
;;;
;;;###autoload
(defun tinyxreg-jump-to-register (&optional remove)
  "Call `tinyxreg-jump-to-register-mouse' with REMOVE."
  (interactive)
  (tinyxreg-jump-to-register-mouse nil remove))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyxreg-jump-to-register-mouse (event &optional remove verb)
  "Displays list of registers using mouse EVENT.
Restore register or optionally REMOVE register from X-list.
Input:

  EVENT     mouse event
  REMOVE    flag, if non-nil, remove register.
  VERB      flag, Allow verbose messages."
  (interactive "e\nP")
  (let ((event (or event
		   (ti::compat-make-fake-event
		    tinyxreg--x-coord tinyxreg--y-coord)))
	(title  (interactive-p))
	ref-list
	list
	data
	char)
    (ti::verb)
    (tinyxreg-update)                   ;update register list
    (setq ref-list (tinyxreg-list)
          list     (mapcar 'car ref-list))
    (cond
     ((null (ti::compat-window-system))
      (message "TinyXreg: sorry, Requires X to use X-popup"))
     ((null list)
      (if verb
          (message "TinyXreg: sorry, both register lists are empty.")))
     (t
      (setq data (ti::compat-popup  list event nil title))
      (if (null data)
          (if verb
              (message "TinyXreg: register not selected."))
        (setq char (cdr-safe (assoc data ref-list)))
        (cond
         (remove
          ;;  Remove from both lists
          (tinyxreg-kill-reg char)
          (cond
           (verb
            (message
             (concat "TinyXreg: register ["
                     (char-to-string char) "] removed"))
            ;;  too fast otw when you move mouse..
            (sleep-for 1))))
         (t
          (jump-to-register char nil))))))))

;;}}}

(provide   'tinyxreg)
(run-hooks 'tinyxreg--load-hook)

;;; tinyxreg.el ends here
