;;; tinychist.el --- Command history save/restore utility

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1996-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; To get information on this program, call M-x tinychist-version.
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

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      (require 'tinychist)
;;
;;  This package also installs function `tinychist-command-history-load' in
;;  `kill-emacs-hook' or if no such hook exist, you will be warned that
;;  other means must be used. Now, whenever you start emacs, name it with
;;  this option.
;;
;;      -name <TITLE>           e.g. -name Mail
;;
;;  and the history file associated with "Mail emacs" is loaded.
;;  If you have any questions, use this function
;;
;;      M-x tinychist-submit-bug-report

;;}}}
;;{{{ Docs

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, apr 1996
;;
;;      In newsgroup post to gnu.emacs.help there was discussion about
;;      saving and restoring emacs command history between session. Fred G.
;;      Athearn <fga@maple.sover.net> sent a help message to a person
;;      asking for it describing how to print out command-history and
;;      saving it into a file with `C-x' `C-w'. This little package tries
;;      to automate everything, so that when you load it, it will
;;      automatically load command history for the right emacs and when you
;;      exit emacs, the command history is saved to disk.
;;
;;  Overview of features
;;
;;      o   Save and restore emacs command history
;;      o   Suport simultaneous emacs sessions, different history for
;;          each one. Eg. "-name Mail" is associated with "mail" emacs history,
;;          "-name News" is associated with "news" history. This trick works
;;          on non-windowed tty too, since the switch is evaluated and cached
;;          internally in those cases.
;;
;;  Default save file from -name parameter
;;
;;      The default save name of command history file is extracted from
;;      the frame parameter. It is quite customary that people have
;;      several emacs open in their X display, each one dedicated to
;;      specific task.
;;
;;      The key here is, that you should make a habit of naming your
;;      emacs by task when you start it:
;;
;;          % emacs -name Mail &        # My mail emacs
;;          % emacs -name C++1 &        # My C++ project 1
;;          % emacs -name News          # for news reading
;;
;;      This effectively sets the frame's name to "-name" parameter's
;;      value. But old emacs versions are a little picky about the order of
;;      command line options, please look at the info pages in which order
;;      you must specify additional arguments. (info pages, Node:Initial
;;      Options) For non-windowed environment, this trick doesn't quite
;;      work out of the box, because emacs doesn't accept the name option
;;      at all. Let's try to start fresh emacs to an xterm, not to separate
;;      frame and see what happens. Order of the options is important here.
;;
;;          % emacs -nw -q -name Mail  &
;;
;;      What happens, is that you get two new buffers: "-name" and "Mail",
;;      and this is not what we intended. If we ask the frame name in this
;;      emacs, it says "terminal" or something similar. What we do instead,
;;      is ,that we install our own command line handler in non-windowed
;;      emacs and then we're able to intercept the "-name" option and it's
;;      parameter. When the emacs is killed, we then again look at the
;;      cached "-name" option to derive the save file postfix. If you're
;;      interested in adding your own command line option, see function
;;      ti::add-command-line-arg in tinylibm.el
;;
;;  How it works
;;
;;      Your emacs must support `kill-emacs-hook', so that the command
;;      history can be saved automatically when you exit emacs. If you have
;;      older than 19.xx, or other emacs that doesn't support the variable,
;;      there is alternative example which replaces you emacs exit. See at
;;      the end of file for examples. When emacs loads, the
;;      `tinychist-load' is runs (see installation) and correct
;;      `command-history' file is loaded.
;;
;;  File saving/loading
;;
;;      If you want to save/load the history session manually, you can call
;;      function
;;
;;          M-x tinychist-command-history-save
;;
;;  About using the frame name
;;
;;      I know it's a bit risky to rely on the frame name's first word,
;;      that it designates the purpose of your emacs. After all you _can_
;;      change the frame name to whatever you want in side emacs. But in
;;      general, the name labels your emacs and gives visible clue in X
;;      where the particular emacs is used to. (in non-X you can't see the
;;      name parameter at all, but I don't think that's a problem.)
;;
;;  See also
;;
;;      See file *chistory.el* in your emacs distribution, how to configure
;;      some parameters. E.g.:
;;
;;          M-x command-history-mode
;;          (setq list-command-history-max 50)

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;  Require not needed
;; (require 'chistory)

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyChist tinychist-: extensions
  "Automatic `command-history' save and restore utility.
  Overview of features

        o   Saving and restoring emacs command history
        o   Supports simultaneous emacs sessions, different history for
            each one. E.g. '-name Mail' is associated with 'mail' emacs history,
            '-name News' is associated with 'news' history. This trick works
            on non-windowed tty too, since the switch is evaluated and cached
            internally in those cases.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinychist-:load-hook '(tinychist-install)
  "*Hook run when file is loaded."
  :type  'hook
  :group 'TinyChist)

;;; ........................................................ &v-public ...
;;; user configurable

(defcustom tinychist-:file-prefix  (ti::package-config-file-prefix "tinychist-")
  "*File prefix for command history files."
  :type  'file
  :group 'TinyChist)

(defcustom tinychist-:non-x-function 'tinychist-non-x-name
  "*Function which return history file postfix in non-X Emacs."
  :type  'function
  :group 'TinyChist)

;;; ....................................................... &v-private ...
;;; do not touch this

(defvar tinychist-:load-flag  nil
  "Non-nil means that flag is t when history has been loaded.")

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinychist-version "tinychist" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinychist.el"
   "tinychist"
   tinychist-:version-id
   "$Id: tinychist.el,v 2.44 2007/05/07 10:50:07 jaalto Exp $"
   '(tinychist-:version-id
     tinychist-:load-hook
     tinychist-:load-flag
     tinychist-:file-prefix
     tinychist-:non-x-function)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;; - If you don't use -name in X, then the frame name looks like
;;;   EMACS-RUN-COMMAND@site.some.com
;;;
(defsubst tinychist-get-frame-name ()
  "Return first word of frame. "
  (let (ptr)
    (or (and (boundp 'command-line-args)
             (setq ptr (member "-name" command-line-args))
             (nth 1 ptr))
        (ti::string-match "^\\([^\t ]+\\)" 1
                          (frame-parameter (selected-frame) 'name)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinychist-get-command-line-cache ()
  "Return option's -name ARG, if it was cached in non window env."
  (let* (ret)
    ;; (("-name" . ignore) ("TITLE" . ignore) ..)
    (dolist (elt command-switch-alist)
      (when (string= "-name" (car elt))
        (setq ret (car elt))
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;; - This is just an sample, you propably want to modify this
;;;   to your env.
;;;
(defun tinychist-non-x-name ()
  "Return command history filename postfix.
Snoop what files are currently loaded into emacs. Eg. if RMAIL is found from
emacs, it is supposed that this emacs handles \"mail\"."
  (let* (elt)
    ;; The order is important, because first one matched is used
    (cond
     ((setq elt (tinychist-get-command-line-cache))
      ;; If this is non-window emacs, check if this option is "cached" by
      ;; this package.
      elt)
     ((ti::dolist-buffer-list
       (string-match "VM$\\|RMAIL$\\|MH$" (buffer-name)) 'temp-buffers)
      "mail")
     ((ti::dolist-buffer-list (string-match "gnus" (buffer-name)) 'temp-buffers)
      "news")
     (t
      "def"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinychist-get-file-name  ()
  "Return command history filename."
  (let* ((pfx    (expand-file-name tinychist-:file-prefix))
         frame)
    (if (ti::compat-window-system)
        (setq frame  (tinychist-get-frame-name))
      (setq frame (funcall tinychist-:non-x-function)))
    (concat pfx frame)))

;;; ----------------------------------------------------------------------
;;;
(defun tinychist-install (&optional uninstall)
  "Install the package. optionally UNINSTALL."
  (interactive)
  (let* ()
    (if uninstall
        (remove-hook 'kill-emacs-hook 'tinychist-command-history-load)
      (cond
       ((boundp 'kill-emacs-hook)
        (add-hook 'kill-emacs-hook 'tinychist-command-history-load))

       ;;  Too bad; this emacs does not have kill-emacs-hook,
       ;;  Check if the default exit function is bound to default key
       ;;  and notify user that he should add custom exit function there.

       ((memq (lookup-key global-map "\C-x\C-c")
              '(save-buffers-kill-emacs))

        (read-from-minibuffer
         (concat
          "Cannot auto-install. Use manual key binding C-xC-c installation."
          " See source code of `tinychist-install'."))))

      ;; load only once, prevent command-history wipe out
      ;; if package is loaded multiple times

      (if (null tinychist-:load-flag)
          (tinychist-load)              ;Autoload the command history
        (setq tinychist-:load-flag t)))))

;;}}}
;;{{{ code: main

;;; ............................................................ &main ...

;;; ----------------------------------------------------------------------
;;; - I admit, this is a bit tricky thing to do, but it also demonstrates
;;;   how you'd add new command line options
;;;
;;;      1. Install extra argument
;;;      2. read the argument content and put it into list too
;;;
(defun tinychist-load  ()
  "Load appropriate `command-history' file by looking at command line option.
The `-name' option is used. This function runs when package is loaded."
  (let* ((elt1                  (member "-name" command-line-args))
         (elt2                  (car-safe (cdr elt1)))
         (pfx                   (expand-file-name tinychist-:file-prefix))
         (debug-on-error        t)      ;trigger errors
         file)
    (cond
     ((not elt1)                             ;no option given
      (setq file (tinychist-get-file-name))) ;guess which file to load.
     (t
      ;;  We know what user wants to load...
      ;;  Prevent using this argument as buffer name. The "-name"
      ;;  option is valid only in X
      (unless (ti::compat-window-system)
        (ti::add-command-line-arg elt2)         ;; this is put after
        (ti::add-command-line-arg (car elt1)))  ;; this is put before
      (setq file (concat pfx elt2))))
    (if (and file (file-exists-p file))
        (load file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinychist-command-history-load  ()
  "Load the saved `command-history' file."
  (if command-history                   ;something to save ?
      (tinychist-command-history-save (tinychist-get-file-name))))

;;; ----------------------------------------------------------------------
;;;
(defun tinychist-command-history-save  (file &optional load)
  "Save history to FILE or optionally LOAD (prefix arg) command history."
  (interactive "FFile: \nP")
  (let* ((buffer         "*Command History*") ;see chistory.el
         (debug-on-error t)                   ;trigger errors

         ;;     XEmacs and Emacs don't agree with key commands,
         ;;     so we leave them out from history
         ;; (global-set-key
         ;;  (quote [#<keypress-event control-C> #<keypress-event 1>]) ...

         (re-incompatible "([^ ]+-key\\(-[^ ]+\\)* "))

    (cond
     (load
      (load-file file))
     (t
      (list-command-history)
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        ;;  Format it
        (ti::pmin)
        (flush-lines re-incompatible)
        (ti::pmin)
        (insert
         (concat
          ";;\n"
          ";; File: Emacs command history file\n"
          ";; Desc: Load this with M-x load-file to reset the history to "
          "these values\n"
          ;;    This is for tinymy.el -- RCS compatible times tamp
          ;;
          ";; $\Docid: $\n"
          "\n"))
        (insert "(defconst command-history\n'(\n")
        (ti::pmax)
        (insert
         (concat
          "))\n\n"
          ";;; " (file-name-nondirectory file) " ends here"))
        (lisp-mode)
        (indent-region (point-min) (point-max) nil)
        ;; Update the times tamp [if function exist] and save the configuration
        ;; See tinymy.el
        (when (fboundp 'tinymy-file-stamp)
          (ti::funcall 'tinymy-file-stamp))
        (write-region (point-min) (point-max) file))

      (ti::kill-buffer-safe buffer)))))

;;}}}
;;{{{ code: examples

;;; ......................................................... &example ...
;;; Draw region and extract code with tinylib.el / ti::package-rip-magic
;;; This is Manual C-xC-x installation
;;;
;;; - If your Emacs doesn't have `kill-emacs-hook' then you
;;;   need this manual installation example.
;; -  Copy this into your emacs BEFORE the (require 'tinychist)

;;* _
;;* (global-set-key "\C-x\C-c" 'my-kill-emacs)
;;* _
;;* ;; This works for 18.57 and 19.xx, whereas the
;;* ;; kill-emacs-query-functions doesn't, and this suffices for personal
;;* ;; needs.
;;* _
;;* (defun my-kill-emacs (&optional arg)
;;*   "Prevent accidental emacs kill. Calls save-buffers-kill-emacs"
;;*   (interactive "P")
;;*   (let* ((chist (tinychist-get-file-name))
;;*          ch)
;;*     (setq ch (ti::read-char-safe-until "Exit emacs y/[n]"))
;;*     (when (char= ch ?y)
;;*       (tinychist-command-history-save chist)
;;*       (save-buffers-kill-emacs arg))))

;;}}}
;;{{{ code: install

;;; .................................................... &auto-install ...

(provide   'tinychist)
(run-hooks 'tinychist-:load-hook)

;;}}}

;;; tinychist.el ends here
