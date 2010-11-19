;;; tinypad.el --- Emulate Windows notepad with extra menu

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1997-2010 Jari Aalto
;; Keywords:     emulations
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; Look at the code with folding.el.

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

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file. This must be the very first entry before
;;  any keybindings take in effect.
;;
;;      (require 'tinypad)
;;
;;  You can also use the preferred way: autoload
;;
;;      (autoload 'tinypad-mode "tinypad t t)
;;      ;;  Put all minor mode activations below C-c m map
;;      ;;  n)otepad emulation mode
;;      ;;
;;      (global-set-key "\C-cmn"  'tinypad-mode)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:
;;
;;  Preface, aug 1997
;;
;;      In gnu newsgroup there was a request that a class had been used to
;;      using Windows notepad and in order to make the transition to Emacs
;;      smooth, Emacs should have some notepad emulation mode so that
;;      pupils wouldnn't get lost completely in new envinronment. And here
;;      is it, a small notepad emulation. It installs one new menu to Emacs
;;      menu bar which is arranged exactly like the Windows notepad. I have
;;      included only the commands that are directly available from inside
;;      emacs and e.g. 'printer setup' is something that is not found there.
;;      But in order to be complete emulation, all the choices as in normal
;;      notepad are available.
;;
;;  Overview of features
;;
;;      o   Minor mode, but once turned on, occupies every emacs buffer
;;          until turned off.
;;      o   Adds menu 'TinyPad' which contains identical
;;          menu definitions that are found from Winbdows notepad
;;      o   The keybindings use `Meta' as the Alt key to access the
;;          menu items, so you may need to configure your keyboard
;;          with 'xmodmap' in order to get 'Alt' key produce `Meta'
;;      o   Windows specific commands are not emulated, like
;;          `Print' 'Setup'.
;;      o   Following famous windows shortcut keys are _not_
;;          Emulated; I was lazy and didn't try to reorganize the
;;          Emacs keys. Erm... for now you have to stick to emacs
;;          equivalents and live without these.
;;
;;          Undo   in   Control-z
;;          Cut    in   Control-x
;;          Copy   in   Control-c
;;          Paste  in   Control-v
;;
;;  Code note
;;
;;      Why on earth I made this package to use "global" minor mode?
;;      I can't remember the reason. A simple menubar entry may have
;;      sufficed just fine.... Oh, it was that remaping the bindings.
;;      You see, when minor mode is turned on, it conquers the mappings
;;      underneath.
;;
;;      [1997-10-23] Hey, I just saw pointer to package Map-zxcv.el which
;;      takes care oc mapping the missing zxcv, so I don't have to bother
;;      with those here. Nice. You can ask it from Kim F. Storm
;;      <storm@olicom.dk>

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-when-compile (require 'advice))

(ti::package-defgroup-tiny TinyPad tinypad-- tools
  "Emulate Windows notepad with extra menu")

(defcustom tinypad--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyPad)

;;}}}
;;{{{ minor mode

;;;###autoload (autoload 'tinypad-mode          "tinypad" "" t)
;;;###autoload (autoload 'turn-on-tinypad-mode  "tinypad" "" t)
;;;###autoload (autoload 'turn-off-tinypad-mode "tinypad" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinypad-" " TinyPad" nil  "TinyPad" 'TinyPad "tinypad--"
   "Emulate Windows Notepad (tm).
This mode is global to all buffers; allthough it is a minor mode.

Notice that the keybindings follow the Windows convention; where the
prefix key accesses each submenu; Like Meta-e accesses (e)edit menu.
If your physical Alt key in your keyboard does not produce `Meta'
code; then you use ESC to access e.g. Meta-e. Consult the Emacs Faq's
how you can make your Alt key produce Meta, so that the keybindings work
like in Windows.

Mode description:
\\{tinypad--mode-prefix-map}"

   "Notepad emulation menu"

   (progn                              ;Some mode specific things? No?
     (cond
      (tinypad-mode
       (put 'tinypad-mode 'global t)
       (add-hook (ti::compat-find-file-hook) 'tinypad-find-file-hook ))
      (t
       (put 'tinypad-mode 'global nil)
       (remove-hook (ti::compat-find-file-hook) 'tinypad-find-file-hook )))
     (when (null (get 'tinypad-mode 'self-call))
       (tinypad-mode-action)))
   "Tiny Notepad mode"
   (list                                ;arg 10
    tinypad--mode-easymenu-name
    (list
     "F)ile"
     ["N)ew"                 erase-buffer                t]
     ["O)pen"                find-file                   t]
     ["S)ave"                save-buffer                 t]
     ["Save A)s"             write-file                  t]
     ["P)rint"               print-buffer                t]
     ["Page Set)up"          tinypad-ignore              t]
     ["Pr)int Setup"         tinypad-ignore              t]
     "----"
     ["M)ode off"            tinypad-mode                        t]
     ["Ex)it Emacs"          save-buffers-kill-emacs     t])
    (list
     "E)dit"
     ["U)ndo"                undo                        t]
     ["Cut)"                 kill-region                 t]
     ["C)opy"                copy-region-as-kill         t]
     ["P)aste"               yank                        t]
     ["De)lete"              backward-delete-char        t]
     "----"
     ["Select A)ll"          mark-whole-buffer           t]
     ["Time/D)ate"           tinypad-insert-time         t]
     "----"
     ["W)ord wrap"           tinypad-ignore              t])
    (list
     "S)earch"
     ["F)ind"                isearch-forward             t]
     ["Find N)ext"           tinypad-ignore              t])
    (list
     "H)elp"
     ["C)ontents"            info                        t]
     ["S)earch for help on"  apropos                     t]
     ["H)ow to use help"     info                        t]
     ["A)bout Tinypad"       tinypad-version             t]
     ["V)ersion, mode desc." tinypad-mode-help           t]))
   (progn
;;;    (set map (setq tinypad--mode-map (make-keymap)))
     (define-key   root-map [(meta f) (n)]  'tinypad-erase-buffer)
     (define-key   root-map [(meta f) (o)]  'find-file)
     (define-key   root-map [(meta f) (s)]  'save-buffer)
     (define-key   root-map [(meta f) (a)]  'write-file)
     (define-key   root-map [(meta f) (p)]  'print-buffer)
     (define-key   root-map [(meta f) (t)]  'tinypad-ignore)
     (define-key   root-map [(meta f) (m)]  'tinypad-mode)
     (define-key   root-map [(meta f) (x)]  'save-buffers-kill-emacs)
     (define-key   root-map [(meta e) (u)]  'undo)
     (define-key   root-map [(meta e) (t)]  'kill-region)
     (define-key   root-map [(meta e) (c)]  'copy-region-as-kill)
     (define-key   root-map [(meta e) (p)]  'yank)
     (define-key   root-map [(meta e) (e)]  'backward-delete-char)
     (define-key   root-map [(meta e) (a)]  'mark-whole-buffer)
     (define-key   root-map [(meta e) (d)]  'tinypad-insert-time)
     (define-key   root-map [(meta e) (w)]  'tinypad-ignore)
     (define-key   root-map [(meta e) (f)]  'isearch-forward)
     (define-key   root-map [(meta e) (n)]  'tinypad-ignore)
     (define-key   root-map [(meta h) (c)]  'info)
     (define-key   root-map [(meta h) (s)]  'apropos)
     (define-key   root-map [(meta h) (h)]  'info)
     (define-key   root-map [(meta h) (a)]  'tinypad-version)
     (define-key   root-map [(meta h) (v)]  'tinypad-mode-help)
     ;;
     ;; Bad idea beacuse C-x/C-x are the crucial prefix keys in
     ;; emacs and occupying it causes havoc and grief
     ;;
;;;    (define-key   root-map [(control z)]  'undo)
;;;    (define-key   root-map [(control x)]  'kill-region)
;;;    (define-key   root-map [(control c)]  'copy-region-as-kill)
;;;    (define-key   root-map [(control v)]  'yank)
     ;;
     (define-key   root-map [(f5)]            'tinypad-insert-time)
     (define-key   root-map [(f3)]            'isearch-forward))))

;;}}}
;;{{{ Code

;;; ----------------------------------------------------------------------
;;;
(defadvice switch-to-buffer  (after tipad act)
  "Turn on `tinypad-mode' if if global Pad mode is non-nil."
  (when (and (interactive-p)
             (get 'tinypad-mode 'global)
             (null tinypad-mode))
    (setq tinypad-mode 1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypad-ignore  ()
  "Display that command does not exist."
  (interactive)
  (message "TinyPad: No eq WinNotepad emulation in Emacs for this command.")
  (sit-for 2))

;;; ----------------------------------------------------------------------
;;;
(defun tinypad-erase-buffer  ()
  "Erase buffer with confirmation."
  (interactive "*")
  (when (or (null (buffer-file-name))
            (and (buffer-file-name)
                 (not (buffer-modified-p)))
            (and (buffer-file-name)
                 (buffer-modified-p)
                 (y-or-n-p "Buffer modified, continue erasing? ")))
    (erase-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypad-find-file-hook  ()
  "Turn on tipad mode if `tinypad--mode-global' is non-nil."
  (when (and (get 'tinypad-mode 'global)
             (null tinypad-mode))
    (setq tinypad-mode 1)))

;;; ------------------------------------------------------------ &main ---
;;;
(defun tinypad-mode-action ()
  "Activate `tinypad-mode' on or off everywhere, depending on var `tinypad-mode'."
  (unless (get 'tinypad-mode 'self-call)
    (run-hooks 'tinypad--mode-define-keys-hook))
  (let ((i 0)
	tinypad--mode-define-keys-hook)
    (unwind-protect
        (progn
          ;;  Raise the flag to prevent calling us
          (put 'tinypad-mode 'self-call t)
          ;;  For every buffer, either turn mode on or off.
          (dolist (buffer (buffer-list))
            ;;  Exclude hidden buffers
            (incf  i)
            (if (not (string-match "^ " (buffer-name buffer)))
                (with-current-buffer buffer
                  (if (get 'tinypad-mode 'global)
                      (tinypad-mode 1)
                    (tinypad-mode 0))))))
      (message "TinyPad: Stepped through %d buffers" i)
      (sit-for 1)
      (put 'tinypad-mode 'self-call nil))))

;;}}}

(add-hook 'tinypad--mode-define-keys-hook 'tinypad-mode-define-keys)

(provide   'tinypad)
(run-hooks 'tinypad--load-hook)

;;; tinypad.el ends here
