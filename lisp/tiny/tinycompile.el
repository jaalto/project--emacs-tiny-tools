;;; tinycompile.el --- Compile buffer extras. Minor mode.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
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

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      ;; You could also call M-x tinycompile-install / tinycompile-uninstall
;;      (add-hook tinycompile--load-hook 'tinycompile-install)
;;      (require 'tinycompile)
;;
;;  or use this autoload; your ~/.emacs loads quicker
;;
;;       (autoload 'tinycompile-mode            "tinycompile" "" t)
;;       (autoload 'turn-on-tinycompile-mode    "tinycompile" "" t)
;;       (add-hook 'compilation-mode-hook 'turn-on-tinycompile-mode 'append)
;;
;; If you find any incorrect behavior, please immediately
;;
;;      o   Turn on debug with `M-x' `tinycompile-debug-toggle'
;;      o   Repeat the task
;;      o   Send bug report with included debug buffer contents.

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, mar 1997
;;
;;      When I was doing grepping over multiple files with igrep.el the
;;      results that were inserted into buffer were too long: There were
;;      2-6 directory paths which occupied 40 characters and the actual
;;      grep hits were continued with \ character to the right. That was
;;      awfull to read. I couldn't get clear look at the grep results. I
;;      decided that there must be a way to clarify the results somehow, so
;;      I started writing this package.
;;
;;  Overview of features
;;
;;      o   Shorten long directory paths (to the right hand)
;;      o   Kill non-interesting files from the buffer
;;      o   Hide selected lines from display

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: misc

;;; ......................................................... &require ...

(require 'tinyliba)

(eval-when-compile
  (require 'cl)
  (defvar mode-line-mode-menu)
  (defvar tinyurl-mode))

(ti::package-defgroup-tiny TinyCompile tinycompile-- tools
  "Compile buffers additions.
  Overview of features

        o   Shorten long directory paths (to the right hand)
        o   Kill non-interesting files from the buffer
        o   Hide selected lines from display")

;;; .......................................................... &v-menu ...

(defcustom tinycompile--menu-use-flag t
  "*Non-nil means to use echo-area menu."
  :type  'boolean
  :group 'TinyCompile)

(defvar tinycompile--menu-main
  (list
   '(format
     "%sTinyCompile: k)ill files s)horten SPC)hide rRU)egexp RET)parse x)mode off"
     (if current-prefix-arg
         (format "%s "  (prin1-to-string current-prefix-arg)) "" ))
   '((?\177 . ( (tinycompile-kill-all-file-lines)))
     (?\b   . ( (tinycompile-kill-all-file-lines)))
     (?k    . ( (tinycompile-kill-all-file-lines)))
     (?s    . ( (tinycompile-shorten-lines)))
     (?\    . ( (tinycompile-show-hide-toggle)))
     (?r    . ( (call-interactively 'tinycompile-hide-by-regexp-whole-line)))
     (?R    . ( (call-interactively 'tinycompile-hide-by-regexp)))
     (?U    . ( (call-interactively 'tinycompile-unhide)))
     (?\C-m . ( (tinycompile-parse-line-goto-main)))
     (?x    . ( (turn-off-tinycompile-mode)))))
  "*TinyCompile echo menu.

k    Kill/Delete all lines that referer to current file
s    If possible, shorten long path names in display
SPC  Toggle hiding lines on/off
r    Hide whole line matching regexp
R    Hide (partial) text matching regexp
U    Unhide all
RET  Goto current file and line
x    Turn mode off.")

;;; ............................................................ &mode ...

;;;### (autoload 'tinycompile-debug-toggle "tinycompile" "" t)
;;;### (autoload 'tinycompile-debug-show   "tinycompile" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinycompile" "--"))

;;;###autoload (autoload 'turn-on-tinycompile-mode      "tinycompile" "" t)
;;;###autoload (autoload 'turn-off-tinycompile-mode     "tinycompile" "" t)
;;;###autoload (autoload 'tinycompile-mode              "tinycompile" "" t)
;;;###autoload (autoload 'tinycompile-commentary        "tinycompile" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinycompile-" " Tco" "\C-c:" "Tco" 'TinyCompile "tinycompile--" ;1-6

   "Additional commands to Compile buffer. You can kill lines or
shorten the file names and hide comments.

Defined keys:

Prefix key to access the minor mode is defined in
`tinycompile--mode-prefix-key'

\\{tinycompile--mode-map}
\\{tinycompile--mode-prefix-map}"

   "TinyCompile"
   (progn
     (if (and tinycompile-mode verb
              (not (string-match "compil" (symbol-name major-mode))))
         (message "TinyCompile: Are you sure this is compile buffer?")))
   "Compile buffer extras."
   (list
    tinycompile--mode-easymenu-name
    ["Kill all matching file lines at point"  tinycompile-kill-all-file-lines t]
    ["Shorten directory names"            tinycompile-shorten-lines           t]
    ["Goto file at point"                 tinycompile-parse-line-goto-main    t]
    "----"
    ["Show or hide comments (toggle)"     tinycompile-show-hide-toggle        t]
    ["Hide by regexp - partial"           tinycompile-hide-by-regexp          t]
    ["Hide by regexp - whole line"        tinycompile-hide-by-regexp-whole-line t]
    ["Unhide all"                         tinycompile-unhide                  t]
    "----"
    ["Keyboard menu"                      tinycompile-menu-main               t]
    ["Package version"                    tinycompile-version                 t]
    ["Package commentary"                 tinycompile-commentary              t]
    ["Mode help"                          tinycompile-mode-help               t]
    ["Mode off"                           tinycompile-mode                    t])

   (progn
     (if (ti::xemacs-p)
         (define-key root-map [(button2)] 'tinycompile-parse-line-goto-main)
       (define-key root-map [mouse-2]     'tinycompile-parse-line-goto-main))
     (cond
      (tinycompile--menu-use-flag
       ;;  Using menu to remeber commands is easier if you don't use
       ;;  menu bar at all.
       (define-key root-map p 'tinycompile-menu-main))
      (t
       (define-key map  "k"      'tinycompile-kill-all-file-lines)
       (define-key map  "s"      'tinycompile-shorten-lines)
       (define-key map  " "      'tinycompile-show-hide-toggle)
       (define-key map  "r"      'tinycompile-hide-by-regexp-whole-line)
       (define-key map  "R"      'tinycompile-hide-by-regexp)
       (define-key map  "U"      'tinycompile-unhide)
       (define-key map  "x"      'turn-off-tinycompile-mode)
       (define-key map  "?"      'tinycompile-mode-help)
       (define-key map  "Hm"     'tinycompile-mode-help)
       (define-key map  "Hc"     'tinycompile-commentary)
       (define-key map  "Hv"     'tinycompile-version)
       ;;  Overwrite {compilation-minor-mode|grep-mode} definition
       (define-key root-map "\C-m" 'tinycompile-parse-line-goto-main))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-menu-main (&optional arg)
  "Show echo area menu and pass ARG to `ti::menu-menu'."
  (interactive "P")
  (ti::menu-menu 'tinycompile--menu-main arg))

;;; ......................................................... &v-hooks ...

(defcustom tinycompile--load-hook nil
  "*Hook that is run when package is loaded."
  :type 'hook
  :group 'TinyCompile)

;;}}}
;;{{{ setup: public

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinycompile--table-hide
  '(("^.*\\.el:"                        ;lisp
     "^.*:[ \t]*[;\"'].*")
    ("^.*\\.\\([cC][cC]?\\|[hH][hH]?\\):" ;C/C++
     ":[ \t]*/[/*].*"))
  "*List of FILENAME and HIDE regexps.
If filename in the beginning of line matches elt1 then
show/hide all lines matching elt2.

Format:
 '((FILENAME-REGEXP HIDE-REGEXP)
   (FILENAME-REGEXP HIDE-REGEXP)
   ...)"
  :type  '(repeat
           (string :tag "File Regexp")
           (string :tag "Hide Regexp"))
  :group 'TinyCompile)

;;}}}
;;{{{ code: macros

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycompile-get-files  (&optional max-point)
  "Return all filenames in compile buffer, optionally until MAX-POINT."
  (beginning-of-line)
  (tinycompile-get-error-lines max-point 'car))

;;}}}
;;{{{ code: support functions

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-install (&optional uninstall)
  "Install or optinally UNINSTALL package with prefix arg."
  (interactive "p")
  (cond
   (uninstall
    (if (boundp 'grep-mode-hook)
        (add-hook 'grep-mode-hook 'turn-on-tinycompile-mode 'append))
    (add-hook 'compilation-mode-hook 'turn-on-tinycompile-mode 'append))
   (t
    (if (boundp 'grep-mode-hook)
        (remove-hook 'grep-mode-hook 'turn-on-tinycompile-mode))
    (remove-hook 'compilation-mode-hook 'turn-on-tinycompile-mode))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-uninstall ()
  "Uninstall package."
  (interactive)
  (tinycompile-install 'remove))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-cd-directory ()
  "Return the CD directory."
  ;; Emacs 22 uses line like:
  ;;      -*- mode: grep; default-directory: "~/elisp" -*-
  (save-excursion
    (goto-char (point-min))
    (or (ti::buffer-match "^-[*]- mode: grep.*\"\\([^\"]+\\)" 1)
        (ti::buffer-match "^cd +\\(.*\\)" 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-shorten-lines ()
  "Shorten the filenames in compile buffer.

Line format must be
  FILE:LINE: results"
  (interactive)
  (let* ( ;;  About 1000 lines, with 70 characters
         (treshold   (* 70 1000))
         (indicator  (and (> (- (point-max)
                                (point-min))
                             treshold)
                          t))
         count
         prev-point
         dir
         cd
         path
         prev
         file)
    (buffer-enable-undo)
    (save-excursion
      (ti::pmin)
      (setq cd (tinycompile-cd-directory))
      (while (re-search-forward "^\\([/.][^:]+\\):" nil t)
        (setq path (match-string 1))
        (when (and indicator
                   ;;  count percentages only after 1000 chars.
                   (> (point) (+ prev-point 1000)))
          (setq count (/ (* 100 (point)) (point-max)))
          (message "Tinycompile: Wait, processing %d %%" count))
        ;; ./pie-mail/hypb.el --> {cd}/pie-mail/hypb.el
        (if (char-equal (aref path 0) ?.)
            (setq path (concat cd (substring path 2))))
        (when path
          (setq file (file-name-nondirectory path))
          (setq path (file-name-directory path))
          (ti::replace-match 1 file)
          (when
              (or (null prev)
                  (null dir)
                  (string= dir prev))
            (setq dir path))
          (unless
              (string= dir prev)
            (setq prev dir   dir path)
            (beginning-of-line)
            (insert "\ncd " dir "\n\n")))
        (if indicator
            (message "Tinycompile: Wait, processing done."))
        (end-of-line)))))

;;; ----------------------------------------------------------------------
;;;
(defvar tinycompile--buffer-name nil
  "Buffer name is asked from user.
Varaible is made buffer local.
See `tinycompile-parse-line-goto-guess'.")

(defun tinycompile-parse-line-goto-guess ()
  "Go to line under cursor by guessing context."
  (interactive)
  (let* ((elt  (ti::buffer-parse-line-col))
	 (line (and elt (string-to-number (car elt))))
	 (col  (and elt (string-to-number (nth 1 elt)))))
    (when elt
      (let ((buffer
	     (or tinycompile--buffer-name
		 (completing-read
		  "TinyCompile, buffer to associate: "
		  (ti::list-to-assoc-menu
		   (mapcar 'buffer-name (buffer-list)))
		  nil
		  t))))
	(make-local-variable 'tinycompile--buffer-name)
	(setq tinycompile--buffer-name buffer)
	(pop-to-buffer tinycompile--buffer-name)
	(ti::goto-line line)
	(unless (zerop col)
	  (setq col (1- col))		;Emacs columns are zero based
	(move-to-column col))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-parse-line-goto-basic ()
  "Go to line under cursor.
The found file is loaded to Emacs and cursor put on the line.
This works like `compile-goto-error'.

Note:

  If `tinyurl' package is present and current point holds an overlay,
  then it is called to handle the line."
  (interactive)
  ;;    If TinyUrl is present, try it to resolve the line.
  ;;    If it marks anything, raise flag `tinyurl'
  (let* ((fid "tinycompile-parse-line-goto:")
         (elt        (ti::buffer-parse-line-main))
         (file       (and elt (car elt)))
         (absolute-p (and file (string-match "^[/\\~]" file)))
         tinyurl
         buffer
         win)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (when (and absolute-p
               (file-exists-p file)
               (ti::overlay-supported-p)
               (boundp 'tinyurl-mode)
               tinyurl-mode)
      (when (tinyurl-overlay-get)       ;line already marked
        (setq tinyurl t))
      (tinycompile-debug fid 'TinyUrl tinyurl (ti::read-current-line)))
    (cond
     (tinyurl                           ;Let this handle url first
      (tinyurl-dispatcher "\C-m" 'key)
      nil)
     (elt
      (multiple-value-bind (file line)
          elt
        (setq file (ti::file-name-for-correct-system file 'emacs))
        (setq buffer (or (find-buffer-visiting file)
                         (get-buffer file)
                         ;; We may have mistakenly grabbed 'cd' command and
                         ;; stucked it with buffers name.
                         ;; /users/foo/*scratch*  --> *scratch*
                         (get-buffer (file-name-nondirectory file))))
        ;;  If buffer exists and is diplayed in another frame, use it.
        (if buffer
            (setq win (get-buffer-window buffer t)))
        (tinycompile-debug fid "interactive" buffer 'file file)
        (cond
         ((and buffer win)
          (select-window win)
          (raise-frame (window-frame win)))
         (t
          (ti::select-frame-non-dedicated)
          (if (and buffer
                   (not (file-exists-p file)))
              (switch-to-buffer-other-window buffer)
            (switch-to-buffer-other-window
             (if (file-exists-p file)
                 (find-file-noselect file)
               (error "TinyCompile: file not found `%s'" file))))))
        (when line
	  (ti::goto-line line)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-parse-line-goto-pass ()
  "Let the mode handle the line."
  (message "TinyCompile: Can't read file/line information.")
  (let (tinycompile-mode
	func)
    (when (current-local-map)
      (setq func (lookup-key (current-local-map) "\C-m"))
      (when (fboundp func)
	(funcall func)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-parse-line-goto-main ()
  "Main controller for goto."
  (interactive)
  (or (tinycompile-parse-line-goto-basic)
      (tinycompile-parse-line-goto-guess)
      (tinycompile-parse-line-goto-pass)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-get-error-lines  (&optional max-point list-func)
  "Get error lines in compile buffer from current point forward.
Input:

  MAX-POINT     max search point, defaults to `point-max'
  LIST-FUNC     if given apply it to extract data member.
                Eg 'car, gives you only list of filenames

Return:

 '((\"filename\" . NBR) ...)
 or whatever format LIST-FUNC says."
  (let ((max-point (or max-point
		       (point-max)))
	table
	elt)
    (save-excursion
      (while (and (re-search-forward "^\\([^:]+\\):[0-9]+:" nil t)
                  (< (point) max-point))
        (setq elt (ti::buffer-parse-line-main))
        (if list-func
            (setq elt (funcall list-func elt)))
        (if (null (member elt table))
            (push elt table)))
      (nreverse table))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-kill-all-file-lines ()
  "Kill all lines associated with the file on the current line."
  (interactive)
  (let ((fid  'tinycompile-kill-all-file-lines)
	(elt  (ti::buffer-parse-line-main))
	(cd   (save-excursion
		(goto-char (point-min))
		(when (looking-at "^cd \\(.+\\)")
		  (match-string-no-properties 1))))
	file
	file2
	re
	point)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (if (null elt)
        (message "TinyCompile: Can't find file name in this line.")
      (beginning-of-line)
      (setq file  (car elt)
            file2 (when (and cd
                             (string-match
                              (concat (regexp-quote cd) "\\(.+\\)")
                              file))
                    (match-string 1 file))
            re    (format "^%s:\\|^%s:\\|^%s:\\|^%s:"
                          (file-name-nondirectory file)
                          (regexp-quote file)
                          (file-name-nondirectory file)
                          (if file2
                              file2
                            "#cannot-match-anything")))
      (tinycompile-debug fid 'file file 'RE re 'elt)
      ;;  Search previous line that is not the same as the line we want
      ;;  to kill
      (while (re-search-backward re nil t))
      (setq point (point))
      (buffer-enable-undo)
      (ti::pmin)
      (with-buffer-modified
	(delete-matching-lines re))
      (if (< point (point-max))
          (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-show-hide-toggle (&optional regexp)
  "Hide or show comment lines matching REGEXP.
References:
 `tinycompile--table-hide'"
  (interactive)
  (let ((list tinycompile--table-hide)
	search
	show)
    (save-excursion
      (unless regexp                    ;Find right value
        (setq show (y-or-n-p "Y = show, N = hide "))
        (dolist (elt list)
          (setq search (car elt))
          (if (ti::re-search-check search)
              (setq list   nil
                    regexp (nth 1 elt)))))
      (ti::pmin)
      (cond
       (show
        (set-text-properties (point-min) (point-max) nil)
        ;;  Won't update well otherwise
        (redraw-display))
       (t
        (if (null regexp)
            (message
             "TinyCompile: No matching regexp in tinycompile--table-hide")
          (ti::text-re-search
           regexp nil nil nil
           (if show
               'null
             '(owner tinycompile  invisible t)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-hide-by-regexp (regexp)
  "Hide lines matching REGEXP."
  (interactive "s[TinyCompile] Hide strings matching: ")
  (tinycompile-show-hide-toggle regexp))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-hide-by-regexp-whole-line (regexp)
  "If REGEXP is found, hide whole line."
  (interactive "s[TinyCompile] Hide lines matching: ")
  (tinycompile-show-hide-toggle
   (format "^.*\\(%s\\).*[\r\n]+" regexp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycompile-unhide ()
  "UNhide all hidden text or lines.
See `tinycompile-hide-by-regexp' and `tinycompile-hide-by-regexp-whole-line'."
  (interactive)
  (set-text-properties (point-min) (point-max) nil))

;;}}}

;; NOTE:  In some cases `tinycompile-mode' gets set globally
;; to value `t'. Reset this, because it would take out mouse-2.
;; Make sure that the global value is nil

(if (default-value 'tinycompile-mode)
    (setq-default tinycompile-mode nil))

(add-hook 'tinycompile--mode-define-keys-hook  'tinycompile-mode-define-keys)

(provide   'tinycompile)
(run-hooks 'tinycompile--load-hook)

;;; tinycompile.el ends here
