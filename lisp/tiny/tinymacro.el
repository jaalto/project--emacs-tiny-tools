;;; tinymacro.el --- Fast way to assign newly created macro to a key

;; This file is not part of Emacs

;; Copyright (C) 1995-2013 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
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

;;; Intall:

;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file:
;;
;;      (autoload 'tinymacro-end-kbd-macro-and-assign "tinymacro" "" t)
;;      (global-set-key "\C-x)" 'tinymacro-end-kbd-macro-and-assign)


;;; Commentary:

;;  Preface, 1995
;;
;;      This started as a very small project when
;;      <mosh@ramanujan.cs.albany.edu> (Mohsin-Ahmed) 1995-03-17 in
;;      gnu.emacs.help asked for an easy way to assign newly created
;;      macro to some key. In reponse the author I sent a simple
;;      function to do it, but he informed that it was oly suitable
;;      for one macro; the key was recycled every time. After
;;      extending the idea, that little function become a package.
;;
;; Description
;;
;;      o   Two keystrokes to make a macro: one to record, one to
;;          assign it to key.
;;      o   To see the macro assignments to keys, just call
;;          `tinymacro-macro-info'
;;      o   Default macro count is 10, increase with `tinymacro--stack-max'


;;; Change log:

;;; Code:

(custom-declare-group
 'tinymacto
 nil
 "An easy way to assign newly created macro to key
  Overview of features.

        o   Two keystrokes to make a macro: one to record, one to
            assign it to key.
        o   To see the macro assignments to keys: M-x tinymacro-macro-info"
 :link'(url-link :tag "Wiki" "http://www.emacswiki.org/emacs/TinyTools")
 :prefix (symbol-name 'tinymacro--)
 :group 'extensions)

;; setup: hooks, private

(defcustom tinymacro--macro-assigned-hook nil
  "*If new macro were asiigned, this hook will be run. The function
SYMBOL that was used is in variable tinymacro--last-macro-func"
  :type  'hook
  :group 'tinymacro)

(defcustom tinymacro--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'tinymacro)

;; setup: public, user configurable

(defcustom tinymacro--macro-function-name-prefix "tinymacro--macro"
  "*The function name prefix to use, when assigning name to last kbd macro"
  :type  'string
  :group 'tinymacro)

(defcustom tinymacro--ask-when-stack-wrap-flag nil
  "*Non-nil means ask user if used function stack wraps."
  :type  'boolean
  :group 'tinymacro)

(defcustom tinymacro--stack-max 10
  "*Maximum stack depth of unique macronames.
 The name run from 0..max, and wraps to 0 after max."
  :type  'integer
  :group 'tinymacro)

(defcustom tinymacro--tmp-buffer "*temp*"
  "*Temporary buffer. Eg. displaying the macro bindings to keys."
  :type  'string
  :group 'tinymacro)

;; setup: private variables

(defvar tinymacro--stack-ptr 0
  "Keep record of available stack space.")

(defvar tinymacro--last-macro-func nil
  "Hold last function SYMBOL that were used in assignment.")

(defvar tinymacro--last-macro-key nil
  "Hold last key STRING or VECTOR that were used in assignment.")

(defvar tinymacro--function-list nil
  "List of original KEY -- FUNCTION pairs, whic are currently occupied
by macros")

;; code: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-restore ()
  "Restore all macro bindings, so that keys that occupy macros
are restored to original functions.

References:
  tinymacro--function-list     list is cleared too."
  (interactive)
  (let ((list tinymacro--function-list))
    (if (null list)
        (if (called-interactively-p 'interactive)
            (message "TinyMacro: No macros active."))
      (dolist (elt  list)
        (global-set-key (nth 0 elt) (nth 1 elt)))
      (setq  tinymacro--function-list nil))))

;; code: symbol

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-create-symbol ()
  "Creates macro variable. Returns NEW or EXISTING SYMBOL."
  (let ((max   tinymacro--stack-max)
	(q     tinymacro--ask-when-stack-wrap-flag)
	(name  tinymacro--macro-function-name-prefix)
	(stack-pointer tinymacro--stack-ptr)
	new
	ret)
    (if (or (null q)
            (< stack-pointer max))                 ; yes, go ahead with new
        (setq new (format "%s%d"
                          name
                          (if (< stack-pointer max) ; 0..max
                              (setq stack-pointer (1+ stack-pointer))
                            (setq stack-pointer 0))))
      (if (y-or-n-p "Macro stack full, wrap? ")
          (setq new
                (if (< stack-pointer max)          ; 0..max
                    (setq stack-pointer (1+ stack-pointer))
                  (setq stack-pointer 0)))))
    (when new                                      ;  Must update stack
      (setq tinymacro--stack-ptr stack-pointer
            ret (intern-soft new))                 ; return symbol
      (if ret
	  nil                                      ; Already exists
        ;;   a) make it b)s et to nil c) put into ret val
        (let ((sym (intern new)))
	  (set sym nil)
	  (setq ret sym))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-create-name ()
  "Creates macro name."
  (let ((max		tinymacro--stack-max)
	(stack-pointer	tinymacro--stack-ptr)
	(name-prefix	tinymacro--macro-function-name-prefix)
	(q		tinymacro--ask-when-stack-wrap-flag)
	new)
    (if (or q (< stack-pointer max))                  ; yes, go ahead with new
        (setq new
              (concat new (if (< stack-pointer max)     ; 0..max
			      (setq stack-pointer (1+ stack-pointer))
			    (setq stack-pointer 0))))
      (if (y-or-n-p "Macro stack full, wrap? ")
          (setq new (concat name-prefix
			    (if (< stack-pointer max) ; 0..max
				(setq stack-pointer (1+ stack-pointer))
			      (setq stack-pointer 0))))))
    (if new                                           ; Must update stack
        (setq tinymacro--stack-ptr stack-pointer))
    new))

;; code: main

(defun tinymacro-keymap-function-bind-info (function-sym &optional map)
  "Return binding information for FUNCTION-SYM from MAP as string or nil."
  (let ((gm (current-global-map))
	global-bindings
	local-bindings)
    (setq global-bindings (where-is-internal function-sym)
          local-bindings
          (prog2
              ;;  We have to set this to nil because where-is-internal
              ;;  searches global map too. We don't want that to happen
              ;;
              (use-global-map (make-keymap))
              (where-is-internal
               function-sym
               (or map (current-local-map)))
            (use-global-map gm)))
    (if (or global-bindings local-bindings)
	(format "%s%s%s"
		(if global-bindings
		    (format "global %s"
			    (mapcar 'key-description
				    global-bindings))
		  "")
		(if (and global-bindings local-bindings)
		    " and "
		  "")
		(if local-bindings
		    (format "local to %s"
			    (mapcar 'key-description
				    local-bindings))
		  "")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymacro-macro-info ()
  "Show which macros are assigned to which keys."
  (interactive)
  (let ((stack-pointer tinymacro--stack-ptr)
	(max    tinymacro--stack-max)
	(buffer tinymacro--tmp-buffer)
	(base   tinymacro--macro-function-name-prefix)
	(i      0)
	(round  0)
	buffer-pointer
	name
	key)
    (while (< i (1+ max))
      (setq name (concat base i)
	    i    (1+ i)
	    key  "")
      (if (null (intern-soft name)) nil ;not use yet
        (if (> round 0) nil             ;do only once
          (setq buffer-pointer (get-buffer-create buffer))
          (set-buffer buffer-pointer) (erase-buffer)
          (insert (format "Stack pointer : %d\n" stack-pointer)))
	(setq key (tinymacro-keymap-function-bind-info (intern name)))
        (if (null key)
            (setq key "[??]"))          ;should never happen
        (insert (format "%-10s %s\n" key name))
        (setq round (1+ round))))
    (if (and (called-interactively-p 'interactive)
             (eq 0 round))
        (message "TinyMacro: No macros bound or set."))
    (switch-to-buffer-other-window buffer-pointer)))

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
Runs tinymacro--macro-assigned-hook if key macro gets installed.
The query options should be turned off if you call this within
function, since it always return nil if the options are on.

Input:

  KEY   Should be valid emacs key-bind-sequence/key-vector
  VERB  Boolean, verbose messages

Return:

  t    is assigned
  nil  not assigned `keyboard-quit'"
  (interactive)
  (let ((funcname "")
	(abort-ch (char-to-string ?\007)) ; C-g
	do-it
	macro-name                     ;newly created macro
	lookup)                        ;what was found
    (or verb
	(setq verb (called-interactively-p 'interactive)))
    (if (null key)
        (setq key
              (read-key-sequence "Tinymacro: Set last macro to key(s): ")))
    (if (equal key abort-ch)
        (progn
          (if (called-interactively-p 'interactive)
              (message "Tinymacro: Skipping abort key. Not assigned."))
          nil)
      ;;  Search the key, if it's already assigned
      (setq lookup
            (or (and (current-local-map) ;in fundamental-mode this is nil.
                     (lookup-key (current-local-map) key))
                (lookup-key global-map key) key))
      ;;  occupied?
      (when lookup
        (if (and (symbolp lookup)
                 (fboundp lookup))      ;just plain function
            (setq funcname (symbol-name lookup))
          (setq funcname  (prin1-to-string lookup))))
      ;;  ask permission
      (when
          (and verb
               (not (null lookup)))
        (setq do-it
              (y-or-n-p
               (format
                "Key already occupied by %s; continue? " funcname))))
      ;;  assign macro
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
                   (not (string-match "^tinymacro" funcname))
                   (not (assoc key tinymacro--function-list)))
          (push (list key lookup) tinymacro--function-list))
        (global-set-key key macro-name)
        (setq tinymacro--last-macro-func macro-name ;set GLOBALS
              tinymacro--last-macro-key  key)
        (if verb
            (message
             "TinyMacro: Created function: %s" (symbol-name macro-name)))
        (run-hooks 'tinymacro--macro-assigned-hook)
        t)))))


(provide 'tinymacro)
(run-hooks 'tinymacro--load-hook)

;;; tinymacro.el ends here
