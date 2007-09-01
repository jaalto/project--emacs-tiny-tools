;;; tinymacro.el --- Fast way to assign newly created macro to a key
;; $Id: tinymacro.el,v 2.43 2007/05/01 17:20:50 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1995-2007 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; To get information on this program, call M-x tinymacro-version.
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

;;; Intall:

;; ........................................................ &t-install ...
;;   Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file. Rip code with tinylib.el/ti::package-rip-magic
;;
;;       ;;  To use default keybinding "C-x("  and "C-x)", add this:
;;      (add-hook 'tinymacro-:load-hook 'tinymacro-install-default-keybindings)
;;      (require 'tinymacro)
;;
;;   or use autoload and your $HOME/.emacs starts faster
;;
;;      (global-set-key "\C-x)" 'tinymacro-end-kbd-macro-and-assign)
;;      (autoload 'tinymacro-end-kbd-macro-and-assign  "tinymacro" "" t)
;;
;;   If you have any questions, feedback, use this function
;;
;;      M-x tinymacro-submit-bug-report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1995
;;
;;      This started as a very little project when
;;      <mosh@ramanujan.cs.albany.edu> (Mohsin-Ahmed) 1995-03-17 in
;;      gnu.emacs.help post asked for easy way to assign newly created
;;      macro to some key. In reponse the author sent a simple function to do
;;      it, but he informaed that one macro, which was recycled every time,
;;      was too little. Author started modifying code more, and that was
;;      the birth of this package.
;;
;; Description
;;
;;      o   Two keystrokes to make a macro: one to record, one to
;;          assign it to key.
;;      o   To see the macro assignments to keys, just call `tinymacro-macro-info'
;;      o   Default macro count is 10, increase with `tinymacro-:stack-max'

;;}}}

;;; Change log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyMacro tinymacro-: extensions
  "Fast way to assign newly created macro to key
  Overview of features.

        o   Two keystrokes to make a macro: one to record, one to
            assign it to key.
        o   To see the macro assignments to keys, just call tinymacro-macro-info")

;;}}}
;;{{{ setup: hooks, private

(defcustom tinymacro-:macro-assigned-hook nil
  "*If new macro were asiigned, this hook will be run. The function
SYMBOL that was used is in variable tinymacro-:last-macro-func"
  :type  'hook
  :group 'TinyMacro)

(defcustom tinymacro-:load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyMacro)

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinymacro-:macro-function-name-prefix "tinymacro--macro"
  "*The function name prefix to use, when assigning name to last kbd macro"
  :type  'string
  :group 'TinyMacro)

(defcustom tinymacro-:ask-when-stack-wrap-flag nil
  "*Non-nil means ask user if used function stack wraps."
  :type  'boolean
  :group 'TinyMacro)

(defcustom tinymacro-:stack-max 10
  "*Maximum stack depth of unique macronames.
 The name run from 0..max, and wraps to 0 after max."
  :type  'integer
  :group 'TinyMacro)

(defcustom tinymacro-:tmp-buffer "*temp*"
  "*Temporary buffer. Eg. displaying the macro bindings to keys."
  :type  'string
  :group 'TinyMacro)

;;}}}
;;{{{ setup: private variables

(defvar tinymacro-:stack-ptr 0
  "Keep record of available stack space.")

(defvar tinymacro-:last-macro-func nil
  "Hold last function SYMBOL that were used in assignment.")

(defvar tinymacro-:last-macro-key nil
  "Hold last key STRING or VECTOR that were used in assignment.")

(defvar tinymacro-:function-list nil
  "List of original KEY -- FUNCTION pairs, whic are currently occupied
by macros")

;;}}}
;;{{{ setup: version

;;;###autoload (autoload 'tinymacro-version "tinymacro" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinymacro.el"
   "tinymacro"
   tinymacro-:version-id
   "$Id: tinymacro.el,v 2.43 2007/05/01 17:20:50 jaalto Exp $"
   '(tinymacro-:version-id
     tinymacro-:stack-ptr
     tinymacro-:last-macro-func
     tinymacro-:last-macro-key
     tinymacro-:function-list
     tinymacro-:macro-assigned-hook
     tinymacro-:load-hook
     tinymacro-:macro-function-name-prefix
     tinymacro-:ask-when-stack-wrap-flag
     tinymacro-:stack-max
     tinymacro-:tmp-buffer)))

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-restore ()
  "Restores all macro bindings, so that keys that occupy macros
are restored to original functions.

References:
  tinymacro-:function-list     list is cleared too."
  (interactive)
  (let* ((list  tinymacro-:function-list))
    (if (null list)
        (if (interactive-p)
            (message "TinyMacro: No macros active."))
      (dolist (elt  list)
        (global-set-key (nth 0 elt) (nth 1 elt)))
      (setq  tinymacro-:function-list nil))))

;;}}}
;;{{{ code: symbol

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-create-symbol()
  "Creates macro variable. Returns NEW or EXISTING SYMBOL."
  (let* ((max   tinymacro-:stack-max)
         (sp    tinymacro-:stack-ptr)
         (q     tinymacro-:ask-when-stack-wrap-flag)
         (name  tinymacro-:macro-function-name-prefix)
         sym2
         new
         ret)
    (if (or (null q)
            (< sp max))                 ; yes, go ahead with new
        (setq new (format "%s%d"
                          name
                          (if (< sp max) ; 0..max
                              (setq sp (1+ sp))
                            (setq sp 0))))
      (if (y-or-n-p "Macro stack full, wrap? ")
          (setq new
                (if (< sp max)          ; 0..max
                    (setq sp (1+ sp))
                  (setq sp 0)))))

    (when new                           ;  Must update stack
      (setq tinymacro-:stack-ptr sp
            ret (intern-soft new))      ; return symbol
      (if ret nil                       ; Already exist
        ;;   a) make it b)s et to nil c) put into ret val
        (setq sym2 (intern new))
        (set sym2 nil)
        (setq ret sym2)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-create-name ()
  "Creates macro name."
  (let* ((max   tinymacro-:stack-max)
         (sp    tinymacro-:stack-ptr)
         (q     tinymacro-:ask-when-stack-wrap-flag)
         (n     tinymacro-:macro-function-name-prefix)
         new)
    (if (or q (< sp max))               ; yes, go ahead with new
        (setq new
              (concat n (if (< sp max)  ; 0..max
                            (setq sp (1+ sp))
                          (setq sp 0))))
      (if (y-or-n-p "Macro stack full, wrap? ")
          (setq new (concat n (if (< sp max) ; 0..max
                                  (setq sp (1+ sp))
                                (setq sp 0))))))
    (if new                             ; Must update stack
        (setq tinymacro-:stack-ptr sp))
    new))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-macro-info ()
  "Show which macros are assigned to which keys."
  (interactive)
  (let* ((sp    tinymacro-:stack-ptr)
         (max   tinymacro-:stack-max)
         (buf   tinymacro-:tmp-buffer)
         (base  tinymacro-:macro-function-name-prefix)
         (i     0)
         (round 0)
         bp                             ;buffer pointer
         name
         key)
    (while (< i (1+ max))
      (setq name (concat base i)   i (1+ i)   key "")
      (if (null (intern-soft name)) nil ;not use yet
        (if (> round 0) nil             ;do only once
          (setq bp (get-buffer-create buf))
          (set-buffer bp) (erase-buffer)
          (insert (format "Stack pointer : %d\n" sp )))
        (if (null (setq key (ti::keymap-function-bind-info (intern name))))
            (setq key "[??]"))          ;should never happen
        (insert (format "%-10s %s\n" key name))
        (setq round (1+ round))))
    (if (and (interactive-p)
             (eq 0 round))
        (message "TinyMacro: No macros bound or set."))
    (switch-to-buffer-other-window bp)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymacro-end-kbd-macro-and-assign ()
  "Terminate reading macro and assign it to key."
  (interactive)
  (end-kbd-macro)
  (call-interactively 'tinymacro-assign))

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-install-default-keybindings ()
  "Install keybinding C-x) to record and assign macro to a key."
  (interactive)
  (global-set-key "\C-x)" 'tinymacro-end-kbd-macro-and-assign)
  (message
   (substitute-command-keys
    (concat
     "Tinymacro: command tinymacro-end-kbd-macro-and-assign set to key "
     "\\[tinymacro-end-kbd-macro-and-assign]"))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymacro-assign (&optional key verb)
  "Name last macro and assigns it to user defined KEY.
Runs tinymacro-:macro-assigned-hook if key macro gets installed.
The query options should be turned off if you call this within
function, since it always return nil if the options are on.

Input:

  KEY   Should be valid emacs key-bind-sequence/key-vector
  VERB  Boolean, verbose messages

Return:

  t    is assigned
  nil  not assigned `keyboard-quit'"
  (interactive)

  (let* ((f-name    "")                 ;func name
         do-it
         macro-name                     ;our new macro !
         lookup                         ;what was found

         ;; --- 1 ---
         ;; The bullet proof way to find key bind for abort
         ;; (ti::keymap-function-bind-info 'keyboard-quit global-map)

         ;; --- 2 --
         ;; - Or we just say where it is... Nobody relocates it anyway
         ;; - We use this because function2key does not work in XEmacs

         (abort-ch (char-to-string ?\007)))
    (ti::verb)
    (if (null key)
        (setq key
              (read-key-sequence "Tinymacro: Set last macro to key(s): ")))
    (if (equal key abort-ch)
        (progn
          (if (interactive-p)
              (message "Tinymacro: Skipping abort key. Not assigned."))
          nil)
      ;;  Search the key, if it's already assigned
      (setq lookup
            (or (and (current-local-map) ;in fundamental-mode this is nil.
                     (lookup-key (current-local-map) key))
                (lookup-key global-map key) key))
      ;; ................................................... occupied? ...
      (when lookup
        (if (and (symbolp lookup)
                 (fboundp lookup))      ;just plain function
            (setq f-name (symbol-name lookup))
          (setq f-name  (prin1-to-string lookup))))
      ;; ............................................. ask permission? ...
      (when
          (and verb
               (not (null lookup)))
        (setq do-it
              (y-or-n-p
               (format
                "Key already occupied by %s; continue? " f-name))))
      ;; ................................................ assign macro ...
      (cond
       ((and verb (null do-it))
        (message
         (substitute-command-keys
          "Tinymacro: Cancelled. Use \\[tinymacro-assign] to rebind.")))
       (t
        (setq macro-name (tinymacro-create-symbol))
        (name-last-kbd-macro macro-name)
        ;;  save previous
        (when (and (symbolp lookup)
                   (fboundp lookup)
                   (not (string-match "^tim" f-name))
                   (not (assoc key tinymacro-:function-list)))
          (push (list key lookup) tinymacro-:function-list))
        (global-set-key key macro-name)
        (setq tinymacro-:last-macro-func  macro-name ;set >> GLOBALS <<
              tinymacro-:last-macro-key   key)
        (if verb
            (message
             "TinyMacro: Created function: %s" (symbol-name macro-name)))
        (run-hooks 'tinymacro-:macro-assigned-hook)
        t)))))

;;}}}

(provide   'tinymacro)
(run-hooks 'tinymacro-:load-hook)

;;; tinymacro.el ends here
