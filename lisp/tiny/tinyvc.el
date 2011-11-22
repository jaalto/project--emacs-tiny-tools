;;; tinyvc.el --- CVS and RCS log minor mode. Checkout, Check-in...

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
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
;; ~/.emacs startup file. Before doing require see tinyvc--load-hook.
;;
;;      (require 'tinyvc)
;;
;; Or prefer autoload: your emacs loads this package only when you
;; need it. This is for 19.30+
;;
;;      (eval-after-load "vc" '(progn (require 'tinyvc)))
;;
;; In very old Emacs releases which have different `eval-after-load' or none
;; at all, use this code:
;;
;;      (defadvice vc-print-log (after tirl act)
;;        "Run hook tinyvc--vc-print-log-hook."
;;        (require 'tinyvc)
;;        (run-hooks 'tinyvc--vc-print-log-hook))
;;
;; If you define your own bindings and use menu, Update following variable
;; and call M-x `tinyvc-install-mode'.
;;
;;     tinyvc--mode-menu-main

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Dec 1996
;;
;;      In work there may be very complex RCS revision numbers, multiple
;;      branches and I may have several branches CheckedOut for testing,
;;      correcting, and developing new features. It seemed natural to
;;      handle this "multiversioning" control from the log output.
;;
;;  Overview of features
;;
;;      o   Companion to *vc.el*, Minor mode for the log buffer (C-x v l)
;;      o   Highlighing supported in Windowed emacs's.
;;      o   You can 1) Lock a file 2) unclock file
;;          3) show status 4) pop to buffer where log belongs and more
;;          5) ChekOut multiple revisions for viewing purposes
;;          6) (un/mark viewed versions) and more..
;;
;;  Do you need this package
;;
;;      If you don't use RCS/CVS don't load this package, it only works for
;;      `log' output and expects to parse buffers in that format. If you
;;      don't use many branches and thusly vc's log output much, this
;;      package may not be essential to you. This pacakge uses colors if
;;      window system is detected, but it partially copes with non-window
;;      system too, so that e.g. marks appear in the buffer as charaxter
;;      codes.
;;
;;          revision 1.25       locked by: xx;
;;          date: 1997/11/10 17:20:45;  author: xx;  state: Exp;  lines: +3
;;
;;      In the above lines the first line, starting from "1.25" is
;;      highlighted (version number). In next line: 97/11/10
;;      (the YY year is significant), "xx" and "Exp" are highlighted.

;;}}}
;;{{{ history

;;; Change Log:

;;; Code:

;;{{{ setup: library

(require 'tinylibm)

(eval-and-compile
  (autoload 'font-lock-mode "font-lock" t t))

(ti::package-defgroup-tiny TinyVc tinyvc-- tools
  "Version control rlog minor mode. ChecOut, CheckIn.
  Overview of features
        o   Companion to vc.el, Minor mode forlog buffer (C-x v l)
        o   Highlighing supported in windowed Emacs.
        o   You can do CheckOut, Lock a file, unclock file(s), show status
            for current rcs file in emacs and ChekOut multiple revision
            for viewing purposes (un/marking viewed versions)")

;;}}}
;;{{{ setup: mode

(defcustom tinyvc--menu-use-p t
  "*Should we use echo-area menu?."
  :type  'boolean
  :group 'TinyVc)

;;;###autoload (autoload 'tinyvc-mode          "tinyvc" "" t)
;;;###autoload (autoload 'turn-on-tinyvc-mode  "tinyvc" "" t)
;;;###autoload (autoload 'turn-off-tinyvc-mode "tinyvc" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinyvc-" " Rlog" "'" "Rlog" 'TinyVc "tinyvc--" ;1-6

   "RCS Log minor mode.
With this mode you can CheckOut, Lock, unlock the file whose version
log your're looking at. You can also 'find file' some specific version
to temporary buffer e.g. to look at some changes in that version.

By default the commands are accessed through guided echo menu. You
can use the normal Emacs keymap choice too by settings
`tinyvc--menu-use-p' to nil and calling `tinyvc-install-mode'.

Mode description:

\\{tinyvc--mode-prefix-map}"

   "RCS rlog "
   nil
   "RCS Rlog minor mode menu."
   (list
    tinyvc--mode-easymenu-name
    ["Do CheckOut at point"           tinyvc-do-co                    t]
    ["Do CheckOut at point (lock) "   tinyvc-do-co-l                  t]
    ["Do CheckOut head "              tinyvc-do-co-head               t]
    ["Unlock version"                 tinyvc-cancel-co                t]
    ["Unlock unsafely version"        tinyvc-unlock-unsafely          t]
    "----"
    ["Find (show) this revision"      tinyvc-find-file-tmp            t]
    ["Mark 'find' versions"           tinyvc-mark                     t]
    ["Pop to RCS buffer"              tinyvc-pop-to-buffer            t]
    ["Kill temporary files (flush)"   tinyvc-kill-tmp                 t]
    ["Reload rlog"                    tinyvc-reload                   t]
    "----"
    ["Mode help"                      tinyvc-mode-help                t]
    ["Mode off"                       tinyvc-mode                     t])
   (progn
     (define-key map  "h" 'tinyvc-do-co-head)
     (define-key map  "k" 'tinyvc-kill-tmp)
     (define-key map  "l" 'tinyvc-do-co-l)
     (define-key map  "m" 'tinyvc-mark)
     (define-key map  "f" 'tinyvc-find-file-tmp)
     (define-key map  "o" 'tinyvc-do-co)
     (define-key map  "p" 'tinyvc-pop-to-buffer)
     (define-key map  "r" 'tinyvc-reload)
     (define-key map  "u" 'tinyvc-cancel-co)
     (define-key map  "U" 'tinyvc-unlock-unsafely)
     (define-key map  "?"  'tinyvc-mode-help)
     (define-key map  "Hm" 'tinyvc-mode-help))))

;;; ......................................................... &v-hooks ...

(defcustom tinyvc--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyVc)

(defcustom tinyvc--vc-print-log-hook
  '(turn-on-tinyvc-mode
    tinyvc-rename-buffer
    tinyvc-select-backend
    turn-on-font-lock-mode-maybe)
  "*Hook run after `vc-print-log' command.
See also `tinyvc--invoked-buffer' what the functions in this hook
can examine."
  :type  'hook
  :group 'TinyVc)

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinyvc--cmd-function 'tinyvc-cmd-get-rcs
  "*Return RCS executable shell command.
See `tinyvc-cmd-get' source code. Input parameters are symbols:

  'co 'ci 'rcs 'rcsdiff.

Predefined functions you coud assign to this variable:

  `tinyvc-cmd-get-rcs'
  `tinyvc-cmd-get-vcs'

Note:

  This variable is set to buffer local and one of the above choices is
  set if `tinyvc-select-backend' function, which is installed in
  `tinyvc--vc-print-log-hook' recognized the backend."
  :type  'hook
  :group 'TinyVc)

(defcustom tinyvc--locker-name (user-login-name)
  "*Your RCS locker ID that apperas in the lock statement."
  :type  'string
  :group 'TinyVc)

(defcustom tinyvc--font-lock-keywords
  '((".*file:[ \t]+\\([^\n]+\\)"    1 'region)
    ("^head:.*"                     0 font-lock-reference-face)
    ("^locks:[ \t]+\\([^\n]+\\)"    1 font-lock-keyword-face)
    ("^total revisions:[ \t0-9]+"   0 font-lock-keyword-face)
    ("revision[ \t]+\\([^\n]+\\)"   1 font-lock-type-face)

    ("date: +..\\([^ \t\n]+\\)"     1 font-lock-reference-face)
    ("author: +\\([^ \t\n]+\\)"     1 font-lock-keyword-face)
    ("state: +\\([^ \t\n]+\\)"      1 font-lock-reference-face))
  "Font lock keywords."
  :type  'sexp
  :group 'TinyVc)

;;}}}
;;{{{ setup: private

(defvar tinyvc--invoked-buffer nil
  "When you call `vc-print-log', the buffer-pointer is recored here.")
(put 'tinyvc--invoked-buffer 'permanent-local t)

(defvar tinyvc--shell-buffer "*tinyvc-tmp*"
  "Shell buffer.")

;;}}}
;;{{{ Minor mode

(defvar tinyvc--mode-menu-main
  '("\
uU)nlock l)ock o=co l=co-l h=co-head  p)op f)ind  m)ark k)ill r)eload [scM]"
    ((?u  . ( (call-interactively 'tinyvc-cancel-co)))
     (?U  . ( (tinyvc-unlock-unsafely ti::menu--prefix-arg 'verb)))
     (?o  . ( (call-interactively 'tinyvc-do-co)))
     (?l  . ( (call-interactively 'tinyvc-do-co-l)))
     (?h  . ( (call-interactively 'tinyvc-do-co-head)))
     (?f  . ( (tinyvc-find-file-tmp ti::menu--prefix-arg 'verb)))
     (?p  . ( (call-interactively 'tinyvc-pop-to-buffer)))
     (?k  . ( (call-interactively 'tinyvc-kill-tmp)))
     (?m  . ( (tinyvc-mark ti::menu--prefix-arg 'verb)))
     (?r  . ( (call-interactively 'tinyvc-reload)))
     (?s  . ( (call-interactively 'tinyvc-status)))
     (?c  . ( (call-interactively 'tinyvc-chmod-toggle)))
     (?M  . ( (tinyurl-mode-help)))))
  "RCS Log browsing minor mode commands.

In alphabetical order.

c  = Toggle chmod in the file underneath for this buffer.
     You need this eg if you main version is locked. But you have
     made a branch where you want to continue.
f  = `find-file'. Load the version in the line to temporary buffer
h  = go to the head: string in the log buffer
k  = Kill all temporary version files that have been loaded with 'f' command
     above.
l  = lock the current version found in the line
m  = Mark this line.
p  = `pop-to-buffer'. Go to to buffer where this Log belongs to.
r  = Reload Rlog buffer (it may be old if you have deposited new versions)
s  = Status. Show some of the file's status information.
uU = Cancel Checkout with 'co'")

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-mode-menu (&optional arg)
  "Call Echo area mode menu with ARG."
  (interactive "P")
  (ti::menu-menu 'tinyvc--mode-menu-main arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-install-to-emacs (&optional off)
  "Turn on `tinyvc-mode' in appropriate buffers."
  (interactive "P")
  ;;  User may have multiple logs, loop through all buffers.
  (dolist (buffer (buffer-list))
    (when buffer-file-name
      (with-current-buffer buffer
        (save-excursion
          (ti::pmin)
          ;;  CVS log is similar to RCS
          ;;
          ;;  RCS file: /users/foo/RCS/file.txt,v
          ;;  Working file: file.txt
          ;;  head: 1.23
          ;;  branch:
          ;;  locks: strict
          (when (looking-at "^RCS file: .*,v")
            (if off
                (when tinyvc-mode
                  (message "TinyVc: Mode turned off, %s" (buffer-name))
                  (turn-off-tinyvc-mode))
              (unless tinyvc-mode
                (message "TinyVc: Mode turned on, %s" (buffer-name))
                (turn-on-tinyvc-mode)))))))))

;;}}}
;;{{{ Macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinyvc-do-macro 'lisp-indent-function 0)
(defmacro tinyvc-do-macro (&rest body)
  "Store info to variables 'ver' and 'file'. Variable VERB must e also bound.
If 'ver' of 'file' cannot be set, print message and do nothing with BODY."
  `(when (and (or (setq ver (tinyvc-get-version))
                  (error "No version found on the line."))
              (or (setq file (tinyvc-get-filename))
                  (error "Can't find rcs file name from buffer.")))
     ;;  We must find absolute path; this isn't enough
     ;;
     ;;  Working file: tm-view.el
     (unless (string-match "/" file)
       (let (buffer)
         (cond
          ((setq buffer (get-buffer file))
           (with-current-buffer buffer
             (setq file buffer-file-name)))
          (t
           (error "Can't find absolute filename %s" file)))))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinyvc-file-confirm-macro 'lisp-indent-function 2)
(defmacro tinyvc-file-confirm-macro (file verb &rest body)
  "Make sure FILE is read-only before continuing.
If VERB is nil, don't do any checkings or ask from user when
executing BODY."
  `(when (or (null ,verb)
             (and ,verb
                  (or (ti::file-read-only-p ,file)
                      (y-or-n-p
                       (format "Writable %s exist, are you sure "
                               (file-name-nondirectory ,file))))))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyvc-lock-listed-p ()
  "See if there is locks in listing."
  (save-excursion
    (ti::pmin)
    ;;    locks: strict
    ;;  jaalto: 1.1
    (re-search-forward "^locks:")
    (forward-line 1)
    (looking-at "^[ \t]+")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyvc-get-tmp-list (file)
  "Return used temporary buffers matching FILE."
  (when file
    (setq file (file-name-nondirectory file))
    (ti::dolist-buffer-list
     (string-match (format "\\*%s.*[0-9]+\\*" file) (buffer-name))
     'map-temporary-buffers-too)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinyvc-do-over-locks-macro 'lisp-indent-function 2)
(defmacro tinyvc-do-over-locks-macro (user ver &rest body)
  "USER and VER is currently unused. Execute BODY over 'locks:' keyword.

While the macro loops each line; the variables 'user' and 'ver'
are updated. If you want to terminate macro, move point away from the
lock lines: eg by (goto-char (point-min)))."
  `(save-excursion
     (ti::pmin) (re-search-forward "^locks:") (forward-line 1)
     (while (looking-at "^[ \t]+\\([^:]+\\):[ \t]\\([.0-9]+\\)")
       (setq user (match-string 1) ver (match-string 2))
       ,@body
       (forward-line 1))))

;;}}}
;;{{{ Rcs interface

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyvc-cmd-cd-template (file &optional options)
  "Create 'cd' command template: \"cd DIR; %s FILE OPTIONS\"."
  (interactive)
  (concat  "cd " (file-name-directory file) "; "
           "%s " (or options "") " " (file-name-nondirectory file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cmd-get-cvs (sym)
  "Return RCS executable according to SYM."
  ;; This is default function. User may return "my-co" for 'co
  ;; command in certain situations etc in his own function.
  ;;
  ;;  "cvs co" is for
  ;;  the initial checkout of a file only, after that,
  ;;  "cvs update" is used.
  (cond
   ((eq 'co sym)        "cvs update")
   ((eq 'ci sym)        "cvs commit")
   ((eq 'rcs sym)       "cvs rcs")
   ((eq 'rlog sym)      "cvs log")
   ((eq 'rcsdiff sym)   "cvs diff") ;; -j<old-rev> -j<new-rev>"
   (t
    (error "No cmd %s " sym))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cmd-get-rcs (sym)
  "Return RCS executable according to SYM."
  ;; This is default function. User may return "my-co" for 'co
  ;; command in certain situations etc in his own function.
  (cond
   ((eq 'co sym)        (symbol-name sym))
   ((eq 'ci sym)        (symbol-name sym))
   ((eq 'rcs sym)       (symbol-name sym))
   ((eq 'rlog sym)      (symbol-name sym))
   ((eq 'rcsdiff sym)   (symbol-name sym))
   (t
    (error "No cmd %s " sym))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cmd-exec (sym shell-cmd &optional buffer noerr)
  "Execute shell command. If error, show result buffer.

Input:

  SYM           Command symbol like 'co
  SHELL-CMD     Full shell command. If this command has %s in
                a string, then RCS-SH-EXE is sprintf'd into that position.
                Normally the rcs exe is prepended to the command.
  BUFFER        where to put shell command results
  NOERR         ignore errors

References:

  `tinyvc--cmd-function'"
  (let* ((exe    (funcall tinyvc--cmd-function sym))
         (send   (if (string-match "%s" shell-cmd)
                     (format shell-cmd exe sym)
                   (format "%s %s" exe shell-cmd)))
         (out   (or buffer (ti::temp-buffer tinyvc--shell-buffer 'clear))))
    (shell-command send out)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cmd-diff-p (file &optional options)
  "Return t if there is diff for FILE with diff OPTIONS."
  (if (zerop
       (tinyvc-cmd-exec
        'rcsdiff
        (tinyvc-cmd-cd-template file (or options "-q"))
        nil
        'noerr))
      nil
    t))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cmd-rcs (file &optional options)
  "Run rcs command on FILE with OPTIONS."
  (tinyvc-cmd-exec 'rcs (tinyvc-cmd-cd-template file options)))

;;}}}
;;{{{ Misc

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-lock-list ()
  "Return lock list. '((USER . LOCK-VER) ..)."
  (let (list
        user
        ver)
    (tinyvc-do-over-locks-macro user ver
                                (push (cons user ver) list))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-load-to-buffer (dest &optional noerr)
  "Examine `tinyvc--shell-buffer' and copy the output to DEST buffer.
If DEST does not exist, it is created. NOERR ignores errors."
  (interactive)
  (let ((shell tinyvc--shell-buffer)
        point)
    (with-current-buffer shell
      (ti::pmin)
      (cond                             ; -p switch
       ((looking-at ".*-->[ \t]+stdout")
        (forward-line 2) (setq point (point)))
       ((save-excursion                 ; rlog
          (forward-line 1)
          (looking-at "RCS file:")
          (setq point (point)))))
      (when point                       ;Only if start point set
        (if (not (get-buffer dest))
            (setq dest (ti::temp-buffer dest 'clear))
          (ti::erase-buffer dest))
        (append-to-buffer dest point (point-max))))
    (when (and (null noerr)
               (null point))
      (pop-to-buffer shell)
      (error "Nothing to load  from shell buffer."))

    ;; return success status
    point))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-reload (&optional verb)
  "Replace buffer with current log. VERB."
  (interactive)
  (let ((file (tinyvc-get-filename)))
    (ti::verb)
    (tinyvc-cmd-exec 'rlog file nil 'noerr)
    (erase-buffer)
    (tinyvc-load-to-buffer (current-buffer))
    (run-hook-with-args-until-success 'tinyvc--vc-print-log-hook)
    (if verb
        (message "Updated."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-status ()
  "Show file status."
  (interactive)
  (let* ((file   (tinyvc-get-filename))
         (buffer (get-file-buffer file))
         str
         fn
         ver)
    (if buffer
        (setq ver (ti::vc-rcs-buffer-version buffer)))
    (setq fn  (file-name-nondirectory file)
          str (ti::file-access-mode-to-string
               (file-modes file)))
    (message "%s%s has modes %s " (if ver (concat ver " "))  fn str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-chmod-toggle (&optional verb)
  "Toggle between =r and +w. VERB."
  (interactive)
  (let* ((file  (tinyvc-get-filename))
         (modes  (file-modes file)))
    (ti::verb)
    (if (ti::file-read-only-p file)
        (set-file-modes file (ti::file-mode-make-writable modes))
      (set-file-modes file (ti::file-mode-make-read-only-all modes)))
    (if verb
        (tinyvc-status))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-select-backend ()
  "Select RCS or CVS command for the log buffer: set `tinyvc--cmd-function'."
  (interactive)
  (let ((buffer  tinyvc--invoked-buffer)
	file
	type)
    (when (and
           buffer
           (get-buffer buffer)
           (setq file
                 (with-current-buffer buffer (buffer-file-name))))
      (if (fboundp 'vc-buffer-backend)  ;19.30+
          (setq type (ti::funcall 'vc-buffer-backend))
        ;;  nope; that function does not exist. (19.28, 21.2+)
        (setq type (vc-file-getprop file 'vc-backend)))
      (make-local-variable 'tinyvc--cmd-function)
      (cond
       ((equal type 'RCS)
        (setq tinyvc--cmd-function 'tinyvc-cmd-get-rcs))
       ((equal type 'CVS)
        (setq tinyvc--cmd-function 'tinyvc-cmd-get-rcs))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-rename-buffer ()
  "Rename buffer to *Rlog* if the the previous buffer name was *vc*.
Other vc commands normally destroy the log buffer, so renaming
it keeps it alive until next rlog command."
  (interactive)
  (let ((buffer (get-buffer "*Rlog*")))
    (when (string= "*vc*" (buffer-name))
      (if buffer (kill-buffer buffer)) ;  Remove old log buffer if it exists.
      (rename-buffer "*Rlog*"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyvc-char-mark-p (&optional remove)
  "Check if there is marker character at the beginning of line.
Move point. Optionally REMOVE marker."
  (beginning-of-line)
  (char-equal (following-char) ?>))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-char-mark (&optional unmark)
  "Mark the line, or UNMARK."
  (interactive)
  (cond
   ((and unmark (tinyvc-char-mark-p))
    (delete-char 1))
   ((and (null unmark) (tinyvc-char-mark-p))
    nil)                                ;there is already mark
   ((null unmark)
    (insert ">"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-buffer-version (file)
  "If FILE is in emacs, return version number."
  (if (get-file-buffer file)            ;Loaded into emacs already
      (ti::vc-rcs-buffer-version (get-file-buffer file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-get-filename ()
  "Return filename or nil."
  (save-excursion
    (ti::pmin)
    (cond
     ((re-search-forward "RCS file:[ \t]+\\([^ \n\t]+\\)")
      (ti::remove-properties (ti::vc-rcs-normal-file (match-string 1))))
     ((re-search-forward "Working file:[ \t]+\\([^ \n\t]+\\)")
      (ti::remove-properties (match-string 1))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-get-version ()
  "Return version on current line."
  (let* ((line  (ti::read-current-line))
         (ver   (ti::string-match "[0-9]+\\.[0-9.]+" 0 line))
         (dots  (and ver
                     (count-char-in-string ?. ver))))
    (if (and ver
             ;; Must be odd count
             (not (eq 1 (% dots 2))))   ;odd, 1.1  or 1.1.1.1
        (setq ver nil))                 ;cancel
    ver))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-file-untouched-p (file)
  "Check if buffer is in emacs and that FILE is not modified.
If the file is not in emacs, run rcsdiff.

Return:
 str    buffer's RCS version if untouched.
 t      if file was not in emacs and there was no rcsdiff."
  (let (buffer
	untouched
	ret)
    (setq buffer (get-file-buffer file))
    (cond
     ((null buffer)                     ;cond1
      (if (ti::file-read-only-p file) ;If it's readonly, suppose no diffs
          (setq ret t)
        (if (tinyvc-cmd-diff-p file)
            (setq ret t))))
     (buffer                            ;cond2:
      (with-current-buffer buffer	;already loaded into emacs
        (unless (buffer-modified-p)
          (setq untouched t))
        (if untouched
            (setq ret (ti::vc-rcs-buffer-version))))))
    ret))

;;}}}
;;{{{ interactive

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-mark (&optional unmark verb)
  "Mark revisions that were loaded by \\[tinyvc-find-file-tmp].
Optionally UNMARK. VERB."
  (interactive "P")
  (let* ((list (tinyvc-get-tmp-list (tinyvc-get-filename)))
         (len  (if list (length list)))
         beg
         end
         re
         ver)
    (ti::verb)
    (save-excursion
      (dolist (elt list)
        (setq ver (ti::string-match "[0-9]+[.0-9]+" 0 elt))
        (setq re  (format "\\(revision\\)[ \t]+%s[^ \t]*$" ver))
        (ti::pmin)
        (when (re-search-forward re nil t)
          (cond
           ((ti::compat-window-system)  ;Windowed -- use colors
            (setq beg (match-beginning 1)  end (match-end 1))
            (if unmark
                (put-text-property beg end 'face 'default)
              (put-text-property beg end 'face 'region)))
           (t                           ;Non-Windowed
            (tinyvc-char-mark unmark))))))
    (if verb
        (if (null len)
            (message "No temporary files.")
          (message "%smarked %d items" (if unmark "un" "") len)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-kill-tmp ()
  "Kill all tmp buffers that were loaded from call \\[tinyvc-find-file-tmp]."
  (interactive)
  (let ((file  (tinyvc-get-filename)))
    (if (null file)
        (message "No RCS filename found.")
      (dolist (file (tinyvc-get-tmp-list file))
        (kill-buffer file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-pop-to-buffer ()
  "Pop to buffer accordig to this rlog."
  (interactive)
  (let* ((file    (tinyvc-get-filename))
         (buffer  (get-file-buffer file)))
    (if buffer
        (pop-to-buffer buffer)
      (if (y-or-n-p (format "No %s buffer, load "
                            (file-name-nondirectory file)))
          (find-file file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-find-file-tmp (&optional no-pop verb)
  "Find the current version into Emacs.
The file will be Checked Out by using pipes and the created
temporary buffer will not have any filename association.

You can use this function to e.g. get version 1.1.1.1 and 1.1.1.2 into
emacs while your workfile stays somewhere else. Nice for pasting
text from other versions.

Input:
 NO-POP     do not `pop-to-buffer' after rcs call.
 VERB       Verbose messages."
  (interactive "P")
  (let (file
	ver
	buffer)
    (ti::verb)
    (tinyvc-do-macro
     (setq buffer (format "*%s %s*" (file-name-nondirectory file) ver))
     (if (get-buffer buffer)
         (if (null no-pop)
             (pop-to-buffer buffer))
       (tinyvc-cmd-exec 'co (format "-p -r%s %s" ver file))
       (tinyvc-load-to-buffer buffer)
       (if (null no-pop)
           (pop-to-buffer buffer))))
    (if (and verb no-pop)
        (message "Loaded %s" ver))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-unlock-unsafely (&optional all verb)
  "Read 'locks:' keyword and unlock first locked version in the list.
If there is no locks, then do nothing. ALL unlocks all locks.

This is unsafe function, because no attempt is made to
check if the file has changes. You may loose data is you call
this fnction without checking the diffs.

Input:

  ALL       Unlock all version locked.
  VERB      Verbose messags.

Notes:
 `tinyvc--locker-name'  other locks are not touched ever.
 No buffer reverting is attempted."
  (interactive)
  (let ((name  tinyvc--locker-name)
	user
	ver
	file
	done)
    (ti::verb)
    (if (and verb
             (null (y-or-n-p "unlock: Are you absolutely sure ")))
        (error "Aborted."))
    (setq file (tinyvc-get-filename))
    (if (and verb
             (not (ti::file-read-only-p file)))
        (if (null (y-or-n-p (format "%s is writable, proceed "
                                    (file-name-nondirectory file))))
            (error "Aborted.")))
    (set-file-modes file (ti::file-mode-make-read-only-all (file-modes file)))
    (tinyvc-do-over-locks-macro user ver
                                (when (string= user name)
                                  (if verb
                                      (message "Unlocking %s" ver))
                                  (tinyvc-cmd-exec 'co (format "-u%s %s" ver file))
                                  (setq done t)
                                  (if (null all)
                                      ;; Terminate lock macro loop
                                      (ti::pmin))))
    (when done
      (tinyvc-reload)
      (if verb
          (message "done.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-cancel-co (&optional verb)
  "Cancel Checkout for current revision, so that file is no more locked.
Notice that the lock status is based on the buffer content. Do
\\[tinyvc-reload] to update the log if needed. VERB.

Chmod undelying file to read-only."
  (interactive)
  (let (buffer
	ver
	file
	llist)
    (ti::verb)
    (tinyvc-do-macro
     (setq llist  (tinyvc-lock-list))
     (if (null llist)
         (if verb
             (message "Lock list seems to be empty."))
       (if (not (rassoc ver llist))
           (if verb
               (message "%s is not locked." ver))
         (set-file-modes file 292)      ;444oct, rrr
         (tinyvc-cmd-exec 'co (format "-u%s %s" ver file))
         (tinyvc-reload)                ;Update
         (when (setq buffer (get-file-buffer file))
           (pop-to-buffer buffer)
           (call-interactively 'revert-buffer)
           (message ""))
         (if verb
             (message "Revision %s unlocked." ver)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-do-co-l ()
  "Do co and lock the version number on the line."
  (interactive)
  (let (old-buffer
	ver
	file)
    (tinyvc-do-macro
     (setq old-buffer (get-file-buffer file))
     (if (not (tinyvc-file-untouched-p file))
         (error "'%s' modified or buffer not read-only." file))
     (if (file-writable-p file)
         (error "Can't lock: Underlying file is writable."))
     (when (or  (null (tinyvc-lock-listed-p))
                (y-or-n-p "There is already lock, proceed? "))
       (tinyvc-cmd-exec 'co (format "-l%s %s" ver file))
       (tinyvc-reload)
       (pop-to-buffer (find-file-noselect file))
       (when old-buffer
         (call-interactively 'revert-buffer)
         (message ""))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-do-co-head ()
  "CheckOut the HEAD revision."
  (interactive)
  (ti::pmin)
  (if (re-search-forward "^head: ")
      (call-interactively 'tinyvc-do-co)
    (message "Hm, Can't find the 'head:' tag anywhere? ")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyvc-do-co (&optional replace verb)
  "Checkout specific revision around current point.
REPLACE current emacs buffer with this version if the existing file in emacs
is read-only. VERB."
  (interactive "P")
  (let (verb
	ver
	file
	untouched
	buffer
	buffer-ver
	ret)
    (ti::verb)
    (tinyvc-do-macro
     (setq buffer     (find-buffer-visiting  file)
           untouched  (tinyvc-file-untouched-p file)
           buffer-ver (or (tinyvc-buffer-version file) ""))
     (tinyvc-file-confirm-macro file verb
       (cond
	((string= ver buffer-ver)
	 (if verb
	     (message (format "%s v%s already in emacs." buffer ver)))
	 (setq ret buffer))
	((or (and
	      (file-writable-p file)
	      (y-or-n-p "Writable file, needs chmod, ok? ")
	      (progn
		(set-file-modes
		 file (ti::file-mode-make-read-only (file-modes file)))
		t))
	     (null buffer)
	     untouched)
	 (when (or (null verb)
		   (null buffer)
		   (and verb
			(y-or-n-p
			 (format "Untouched %s, replace %s with version %s ?"
				 (file-name-nondirectory file)
				 buffer-ver ver))))
	   ;;  (if buffer (kill-buffer buffer))
	   (tinyvc-cmd-exec 'co (format "-r%s %s " ver file))
	   (with-current-buffer buffer
	     (revert-buffer nil 'no-confirm)
	     (setq buffer (current-buffer)))
	   (if verb
	       (display-buffer buffer))))
	(t
	 (if verb
	     (message (format "Changed buffer exist, cancelled.")))))))
    ret))

;;}}}

(if (boundp 'vc-print-log-hook)         ;Not Exist in 19.34
    (ti::add-hooks 'vc-print-log-hook tinyvc--vc-print-log-hook)
  (eval-when-compile (require 'advice))
  (defadvice vc-print-log (around tirl act)
    "Run hook `tinyvc--vc-print-log-hook'."
    (let ((BuffeR (current-buffer)))
      ad-do-it
      (make-local-variable 'tinyvc--invoked-buffer)
      (put 'tinyvc--invoked-buffer 'permanent-local t)
      (setq tinyvc--invoked-buffer BuffeR)
      (run-hooks 'tinyvc--vc-print-log-hook))))

(add-hook 'tinyvc--mode-define-keys-hook 'tinyvc-mode-define-keys)

(provide   'tinyvc)

(tinyvc-install-to-emacs)
(run-hooks 'tinyvc--load-hook)

;;; tinyvc.el ends here
