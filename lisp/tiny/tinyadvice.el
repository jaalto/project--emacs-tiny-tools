;;; tinyadvice.el --- Collection of adviced functions

;; This file is not part of Emacs

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari aalto
;;
;; To get information on this program, call M-x tinyadvice-version.
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


;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;     (require 'tinyadvice)
;;
;; Loading this package takes some time. A more faster startup "feel"
;; could be achieved with following delayed loading:
;;
;;     (require 'tinylib)
;;     (when (ti::emacs-p)                    ;Do not load in XEmacs
;;       (if (fboundp 'run-with-idle-timer)   ;Emacs
;;           (run-with-idle-time (* 4 60) nil
;;             '(lambda () (require 'tinyadvice)))
;;         (run-at-time "4 min" nil '(lambda () (require 'tinyadvice)))))
;;
;; But before you leap into that, make sure you want to do it.
;;
;;      CHECK IF YOUR EMACS IS SUPPORTED
;;      THESE ADVICES ARE FOR Emacs, expect trouble in XEmacs.
;;
;;      Advice activation  controlled by variable `tinyadvice-:re'
;;
;; This file modifies original Emacs functions, so read the
;; documentation carefully to tailor package for personal use
;; (enabling/disabling advices). The up to date documentation can be
;; generated from this file with ocmmands:
;;
;;      M-x eval-current-buffer
;;      M-x load-library tinyliby.el
;;      M-x ti::system-get-file-documentation RET tinyadvice.el RET
;;
;; If you have any questions, use this function
;;
;;      M-x tinyadvice-submit-bug-report      send bug report or feedback

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Apr 1996
;;
;;      What you see here is a selection of adviced functions that
;;      have proven to be useful. Some of them have been written by
;;      the author (if there is no author mentioned) and some of them
;;      have been collected form the Emacs newsgroups.
;;
;;      Here is one example how to to fontify automatically, whenever
;;      compilation buffer runs:
;;
;;          (add-hook 'tinyadvice-:compile-internal-hook 'my-compile-font-lock)
;;
;;          (defun my-compile-font-lock  ()
;;            "Compile buffer fontifying immediately."
;;            (interactive)
;;            (let* ((buffer tinyadvice-:compile-internal-buffer))
;;              ;; According to buffer you could set up different font
;;              ;; keyword parameters, say for
;;              ;; *compilation*
;;              ;; *grep*
;;              ;; *igrep*
;;              ;; ...
;;              ;;  My setup automatically turn on the lazy-lock too, see
;;              ;;  font-lock-mode-hook
;;              (with-current-buffer
;;               buffer
;;                (font-lock-mode 1))))
;;
;;  Note: XEmacs
;;
;;      These advices are for Emacs and it would be a surprise if any
;;      of them worked in XEmacs. Use at your own risk.
;;
;;  These advises and Emacs releases
;;
;;      Many of these enhancements could have shipped with the Emacs
;;      itself. And there was a time when these were suggested to be
;;      added to the next Emacs release, but the developers were not
;;      interested in improving or changing functions at those times.
;;
;;  How to use this package
;;
;;      The best way is to try. Load package read the comments about
;;      individual functions and how they change things.
;;
;;  Overview of features
;;
;;      In general, advices are activated only if Emacs release doesn't have
;;      similar kind of support.
;;
;;      o   `gud' highlights full line
;;      o   no dialogs in X for `y-or-n-p' styled questions. You shouldn't
;;          need to lift your hands from keyboard and grab mouse for these
;;          dialog questions.
;;      o   Mouse-3 cinfirms window delete (pointing at the mode line)
;;      o   `call-last-kbd-macro' ends the current macro
;;          before trying to execute it.
;;      o   `debugger-eval-expression',  Backtrace buffer's
;;           "e" offers current word for prompt
;;      o   `dired-man'       , make sure variables are initialized.
;;      o   `dired-do-rename' , you can edit the old filename
;;      o   `goto-line' and `imenu' now widens automatically before executing
;;      o   `rename-buffer'   , offers old buffer name for editing
;;      o   `recover-file'    , offers buffer filename by default
;;      o   `switch-to-buffer-other-frame' , selects some non existing frame
;;      o   `setenv'          , offer completion
;;      o   `write-file'      , confirm overwrite
;;      o   `write-region'    , confirm overwrite
;;
;;      o   `C-x' `;'   , `indent-for-comment' negative arg deletes comment.
;;      o   `C-x' `='   , `what-cursor-position' shows the line number too
;;      o   `C-x' `i'   , insert buffer offers other window
;;      o   `C-x' `C-c' , `save-buffers-kill-emacs' asks confirmation
;;                        to prevent accidents (Emacs 21 has this)
;;      o   `C-x' `b'   , `swich-to-buffer' ask confirmation
;;                        for non-existing buffers.
;;      o   `C-x' `C-b' , list-buffers puts cursor to "*Buffer List*"
;;
;;      o   compilation: buffer auto scroll (disabled, see 'handling advices')
;;          Smart save feature (only .cc .h files, not
;;          all emacs files). Find-file is done in non dedicated frame.
;;          TAB completes filenames.
;;
;;      o   completion: case sensitive filename completion
;;
;;      o   grep: filename and directory completion with TAB key
;;
;;      o   `vc-print-log', put cursor on the buffer's revision number.
;;          Smarter `vc-mode-line' , shows "b" if version is in the middle.
;;          `vc-register' creates RCS directory if does not exist and
;;          offers checking as "original" file with existing version
;;          numbers (tracking 3rd party sources).
;;          User to set the initial comment when doing 1st checkin.
;;          If `tinyadvice-:cvs-buffer-read-only' is nil, then keep.
;;          CVS files in writable mode (the default CVS behavior)
;;
;;  Handling advices
;;
;;      If you have some other Emacs version that is not supported in
;;      the `tinyadvice-:advice-table' you can modify the regexps in
;;      the list and try if the advice works. If it does, please drop
;;      me a mail to have the regecp updated. If some advice is
;;      annoying, there is simple method how you disable advice(s).
;;
;;          (setq tinyadvice-load-hook
;;                '(tinyadvice-install my-tinyadvice-load-hook))
;;
;;          (defun my-tinyadvice-load-hook ()
;;            "Configure advices"
;;            (interactive)
;;            ;; This diables two advices
;;            (tinyadvice-advice 'disable
;;               '(switch-to-buffer mouse-delete-other-windows)))
;;
;;          (require 'tinyadvice)
;;
;;  Disabling a disturbing advice by hand
;;
;;      If some piece of advice disturbs or causes trouble, it is
;;      possible to deactivate it immediately. First you have to know
;;      the function name that generates problems. Say you used `C-x'
;;      `C-b' `switch-to-buffer' and you don't like the confirmation
;;      for non-existent buffers. You can disable this behavior by
;;      calling:
;;
;;          C-u M-x tinyadvice-advice
;;
;;      and give the function name `switch-to-buffer' to it. To
;;      permanently turn the advice off, see previous lisp code.
;;
;;  Code note
;;
;;      You see this in the code:
;;
;;          (when (tinyadvice-activate-p)
;;              (defadvice ..
;;
;;      If emacs version is wrong, the advice is _never_ actually
;;      intrumented. You can't activate or deactivate those functions
;;      with `tinyadvice-advice'.
;;
;;  Many thanks to, in no particular order:
;;
;;      Vladimir Alexiev        <vladimir@cs.ualberta.ca>
;;      Kevin    Rodgers        <kevinr@ihs.com>
;;      Ilya     Zakharevich    <ilya@math.ohio-state.edu>
;;      Peter    Breton         <pbreton@i-kinetics.com>
;;      T. V.    Raman          <raman@adobe.com>

;;; Change Log:

;;; Code:

;;; ......................................................... &require ...

(require 'advice)
(require 'tinylibm)

(eval-and-compile
  (defvar vc-parent-buffer)             ;Emacs vc.el
  (defvar grep-command)
  (defvar grep-default)
  (defvar grep-history)
  (autoload 'grep-compute-defaults "compile")
  (when (ti::xemacs-p)
    (message "\
** TinyAdvice: You must configure this package manually to XEmacs
               In general, do not use this packaage on XEmacs.")
    (load "overlay" 'noerr)))           ;19.15+

;;; ......................................................... &v-hooks ...

(defvar tinyadvice-load-hook '(tinyadvice-install)
  "Hook that is run when package is loaded.")

;;; ........................................................ &v-public ...

(defvar tinyadvice-:cvs-buffer-read-only t
  "*nil makes CVS buffers writable.  Value t preserves vc.el's decision.
Many times vc.el sets read-only status to CVS buffers when there is no need.
In default case, CVS itself does not mark files read-only, unlike RCS.
But if you do \"cvs watch on\" on a tree then when you do \"cvs co tree\" it
will check files out read-only. You have to do \"cvs edit\" to make them
writable.

Setting this variable to nil, will override vc.el and
keep CVS buffers always writable. The t value preserves what vc.el does.")

(defvar tinyadvice-:compile-save-re
  "\\(\\.hh?\\|\\.cc?\\|\\.C?\\|\\.java\\)$"
  "*Regexp. Case sensitive. Which buffers to save when compiling.")

(defvar tinyadvice-:gud-overlay
  (when (and (ti::emacs-p)
             (not (fboundp 'make-extent)))
    (let* ((ov (make-overlay (point-min) (point-min))))
      (overlay-put ov 'face 'highlight)
      ov))
  "*Gud. Current line overlay.")

(defvar tinyadvice-:find-alternate-file-flag  t
  "*Non-nil means : `buffer-name' in \\[find-file] if no `buffer-file-name'.")

;;  Ignore tmp/ dir files
;;  like ~/T  ~/TT ~/T1 ~/T2 ~/T.test ~/T1.xx ...

(defconst tinyadvice-:write-file-no-confirm
  "^/tmp\\|/[Tt][Tt0-9]?\\.?\\|/[Tt]$"
  "*Do not verify overwriting these files. See advice `write-file'.")

(defvar tinyadvice-:switch-to-buffer-find-file  t
  "*Suggest `find-file' for non-existing buffers in `switch-to-buffer'.")

(defvar tinyadvice-:vc-main-rcs-dir  "~/RCS"
  "Main RCS repository. See advice of function `vc-register'.")

;;; ........................................................ &v-advice ...

(defvar tinyadvice-:re "19\\.2[7-9]\\|19\\.3[0-5]\\|2[01]\\."
  "General regexp for advices that work in variety of (X)Emacs versions.")

;; - Change the REGEXP is you know the advice works in your emacs ver.
;;   Drop me mail if you change any of these, so that I can update list
;;
;; - Functions that have ".", almost always get advice, see the code.
;;   In those rows the regexp value is almost always ignored.
;;
;; - If it says ";; always on", then the regexp has no effect,
;;   you have to disable feature by hand, if you don't want it.

(defconst tinyadvice-:advice-table      ;alphabetically ordered
  (list
   (list 'after-find-file               ".")    ;;always on

   (list 'call-last-kbd-macro
         ".")
   (list 'compile                       ".")
   (list 'compilation-find-file         ".")
   (list 'shell                         ".")

   (list 'debugger-eval-expression      ".")

   (list 'dired-do-rename               ".")
   (list 'dired-man                     ".") ;;always

   (list 'exchange-point-and-mark       ".")
   (list 'find-file                     ".")

   (list 'grep                          ".")
   (list 'igrep-read-expression         ".")
   (list 'igrep-read-options            ".")

   (list 'find-alternate-file           ".")
   (list 'find-file-literally           ".")
   (list 'find-tag                      ".")

   (list 'getenv                        ".") ;;always on
   (list 'goto-line                     ".")
   (list 'grep                          ".")
   (list 'gud-display-line              ".") ;;always

   (list 'hkey-help-show                ".") ;;hyberbole

   (list 'indent-for-comment            ".")
   (list 'insert-buffer                 tinyadvice-:re)
   (list 'list-buffers                  ".")
   (list 'line-move                     ".")

   (list 'map-y-or-n-p                  tinyadvice-:re)
   (list 'mouse-delete-other-windows    tinyadvice-:re)
   (list 'mouse-delete-window           tinyadvice-:re)
   (list 'mouse-wheel-scroll-screen     tinyadvice-:re)

   (list 'occur                         ".")
   (list 'PC-complete                   ".") ;;always on

   (list 'recompile                     ".")
   (list 'recover-file                  ".")
   (list 'rename-buffer                 tinyadvice-:re)

   (list 'save-some-buffers             ".")
   (list 'sendmail-pre-abbrev-expand-hook tinyadvice-:re)
   (list 'setenv                        ".") ;;always on
   (list 'set-mark-command              ".") ;;always on
   (list 'switch-to-buffer              tinyadvice-:re)
   (list 'switch-to-buffer-other-frame  ".")

   (list 'vc-mode-line                  tinyadvice-:re)

   (list 'what-cursor-position          tinyadvice-:re)
   (list 'write-file                    ".")
   (list 'write-region                  ".")

   (list 'y-or-n-p                      tinyadvice-:re))
  "*Flag table of enabled advices.
It is consulted if particular advice can be used in current emacs. Format is

  ((FUNCTION ALLOW-ADVICE-REGEXP [FLAG])
   (FUNCTION ALLOW-ADVICE-REGEXP)
   ..)

The FLAG is optional and values can be:

  nil   or missing: Only works in Emacs
  'xe   only works in Xemacs
  t     works both Emacs and XEmacs")

;;; ....................................................... &v-private ...

(defconst tinyadvice-:advice-re  "^tinyadvice"
  "Prefix name used in advices for TinyAdvice package.")

(defconst tinyadvice-:tmp-buffer  "*tinyadvice*"
  "Temporary working buffer.")

(defvar tinyadvice-:vc-p nil
  "Variable indicating if file in `vc-do-command' is version controlled.")

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinyadvice-version "tinyadvice" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinyadvice.el"
   "tinyadvice"
   tinyadvice-:version-id
   "$Id: tinyadvice.el,v 2.71 2007/05/07 10:50:07 jaalto Exp $"
   '(tinyadvice-version-id
     tinyadvice-:compile-save-re
     tinyadvice-:write-file-no-confirm
     tinyadvice-:re)))

;;; ########################################################### &Funcs ###

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyadvice-elts (elt func re type)
  "Decode ELT to variables FUNC RE TYPE."
  `(setq ,func (nth 0 ,elt)
         ,re   (nth 1 ,elt)
         ,type (if (eq 3 (length ,elt))
                      (nth 0 ,elt)
                    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-match (re &optional type)
  "Check if RE match emacs version according to TYPE.
TYPE :
  nil = Emacs
  t   = XEmacs and Emacs
  'xe = XEmacs"
  (let* ((ver   (emacs-version))
         ret)
    (when (stringp re)
      (cond
       ((and (eq type 'xe)
             (ti::xemacs-p)
             (string-match re ver))
        (setq ret 1))
       ((and (eq type nil)
             (ti::emacs-p)
             (string-match re ver))
        (setq ret 2))
       ((and (eq type t)
             (string-match re ver))
        (setq ret 3)))
      ret)))

;;; ----------------------------------------------------------------------
;;; Testing... (tinyadvice-activate-p 'compile-internal)
;;;
(defun tinyadvice-activate-p (func-sym)
  "Determine if we can advice FUNC-SYM."
  (let* ((elt   (assoc func-sym tinyadvice-:advice-table))
         func
         re
         type)
    (when elt
      (tinyadvice-elts elt func re type)
      ;;  XEmacs 19.14 ByteComp, Shut up "bound but not referenced"
      ;;  the `func' is set above.
      (if func
          (setq func 'ignore))
      (tinyadvice-match re type))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-ad-function-list  (&optional string-format)
  "Return list of tinyadvice ad-functions for current emacs.
Notice: all functions may not be adviced; this merely
return entries in the table. See source file's \"Code note\"

If STRING-FORMAT is non nil, then return string list.

Return:

  '(func    func ..)
  '(\"func\" \"func\" ..)"
  (let* (func
         re
         type
         list)
    (dolist (member tinyadvice-:advice-table)
      (tinyadvice-elts member func re type)
      (when (tinyadvice-match re type)
        (if  string-format
            (push (symbol-name func) list)
          (push func list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-install  ()
  "Activates advices that are listed in `tinyadvice-:advice-table'."
  (interactive)
  (tinyadvice-advice nil (tinyadvice-ad-function-list)))

;;; ----------------------------------------------------------------------
;;;
;;; This is slow, but returns only tinyadvice adviced functions...
;;;
;;; (ad-do-advised-functions (func)
;;;  (if (ad-find-some-advice func 'any tinyadvice-:advice-re)
;;;      (push func list)))
;;;
;;;
(defun tinyadvice-advice (&optional disable func-or-list)
  "Activate or optionally DISABLE tinyadvice advice for FUNC-OR-LIST."
  (interactive
   (list
    current-prefix-arg

    (let* (var)
      (setq var (completing-read
                 (concat
                  (if current-prefix-arg "un"  "")
                  "advice function: ")
                 (ti::list-to-assoc-menu (tinyadvice-ad-function-list 'strings))
                 nil t))
      (intern-soft var))))
  ;; This is in fact cheating a little; we check against full advice list,
  ;; not just "tinyadvice" owned functions.
  (when (and (symbolp func-or-list)
             (not (member (list (symbol-name func-or-list))
                          ad-advised-functions )))
    ;; This makes the call to 'ti::' after this if, unefective
    (setq func-or-list nil)
    (if (interactive-p)
        ;; more accurate: "No advice found..." but since we deal with
        ;; tinyadvice ones only the following is better.
        (message "\
TinyAdvice: Sorry, the function is not advice controlled by TinyAdvice.")))
  (ti::advice-control
   func-or-list tinyadvice-:advice-re disable (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-advice-control  (&optional disable verb)
  "Acivate all TinyAdvice advices. Use extra argument to DISABLE all. VERB."
  (interactive "P")
  (or verb
      (setq verb (interactive-p)))
  (let* (
         (re    tinyadvice-:advice-re)
         (doit  t)
         msg)
    (if verb ;;  This is rough! Be sure...
        (setq
         doit
         (y-or-n-p (format
                    "Advices will be turned %s. Are you sure? "
                    (if disable "OFF" "ON")))))
    (when doit
      (cond
       (disable
        (ad-disable-regexp re)          ;only sets flag
        (setq msg "Tinyadvice: All advices deactivated"))
       (t
        (ad-enable-regexp re)           ;only sets flag
        (setq msg "Tinyadvice: All TinyAdvice advices activated")))
      (ad-update-regexp re)
      (if verb
          (message msg)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-convert-filename  (file &optional cautious)
  "Return normal or compressed filename.

Input:

 FILE       full filename
 CAUTIOUS   if non-nil then when in doubt do not change the filename.
            (e.g. in clash situation, where there is bot un/compressed  file)

Return:

 string     possibly modified."
  (interactive)
  (unless (string-match "\\.Z$\\|\\.gz$"  file)
    (when (and (file-exists-p file)
               (or (file-exists-p (concat file ".gz"))
                   (file-exists-p (concat file ".Z"))))
      (message "TinyAdvice: clash, both un/compressed file found. %s " file)
      (sleep-for 1)
      (if (and
           (null cautious)              ;only if no cautious mode
           (setq
            file
            (or (ti::file-newer-exist file (concat file ".gz"))
                (ti::file-newer-exist file (concat file ".Z")))))
          ;;  We must load this package too to enable compress support.
          (require 'jka-compr))))
  file)

;;; ........................................................ &built-in ...

;;; ----------------------------------------------------------------------
;;;
(when (tinyadvice-activate-p 'rename-buffer)
  (defadvice rename-buffer (around tinyadvice dis)
    "Gives old buffer name for editing."
    (interactive
     (list
      (read-from-minibuffer
       "Rename buffer (to new name): "
       (buffer-name))))
    ad-do-it))


;;; ......................................................... &compile ...

;;; ----------------------------------------------------------------------
;;; (ad-disable-advice 'compilation-find-file 'before 'tinyadvice)
;;; (ad-activate       'compilation-find-file)
;;;
(defadvice compilation-find-file  (before tinyadvice act)
  "Move to some non dedicated frame."
  (ti::select-frame-non-dedicated))

;;; ----------------------------------------------------------------------
;;;
(defadvice shell (around tinyadvice dis)
  "If there is *shell* buffer, ask user to give new name for new shell.
If new buffer name is given, a new shell is created. pressing RET
does not create new buffer, but jumps to existing *shell* buffer."
  (let* (name
         prev-name)
    (when (and
           (interactive-p)
           (comint-check-proc "*shell*")
           (setq
            name
            (read-string
             "Create new shell by typing a buffer name for it [RET = cancel]? "))
           (not (ti::nil-p name)))
      (with-current-buffer "*shell*"
        (rename-uniquely)
        (setq prev-name (buffer-name))))
    ad-do-it
    (when (and (stringp name)
               (not (string= name "")))
      (with-current-buffer "*shell*"
        (rename-buffer name))
      (with-current-buffer prev-name
        (rename-buffer "*shell*")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-compile-save-buffers ()
  "Check what buffers for current compilation target should be saved."
  (interactive)
  (let ((case-fold-search      nil)    ;case sensitive
	(re-file               tinyadvice-:compile-save-re))
    ;; Save only interesting buffers, don't care about others.
    (ti::dolist-buffer-list
     (string-match re-file  (buffer-name))
     nil
     nil
     (and (buffer-modified-p)
          (y-or-n-p (format "Buffer %s modified. Save it? "
                            (buffer-name)))
          (save-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice igrep-read-expression (around tinyadvice dis)
  "Replace function: TAB key completes file names."
  (setq
   ad-return-value
   (let ((default-expression (igrep-default-arg igrep-expression-default)))
     (if (string= default-expression "")
         (setq default-expression nil))
     (ti::file-complete-filename-minibuffer-macro
       (read-from-minibuffer (igrep-prefix prompt-prefix "Expression: ")
                             default-expression map nil
                             'igrep-expression-history)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice igrep-read-options (around tinyadvice act)
  "Replace function: TAB key completes file names."
  (setq
   ad-return-value
   (if (or igrep-read-options
           (and (consp current-prefix-arg)
                (memq (prefix-numeric-value current-prefix-arg)
                      '(4 64))))
       (let ((prompt "Options: "))
         (ti::file-complete-filename-minibuffer-macro
           (read-from-minibuffer
            (igrep-prefix prompt-prefix prompt)
            (or igrep-options "-")
            map)))
     igrep-options)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-grep-default (arg)
  "Set default value. This function use dynamically bound variables.
See `grep' advice."
  (unless grep-command
    (grep-compute-defaults))
  ;; `arg' is bound during M-x grep
  (when arg
    (let* ((tag-default
            (funcall (or find-tag-default-function
                         (get major-mode 'find-tag-default-function)
                         ;; We use grep-tag-default instead of
                         ;; find-tag-default, to avoid loading etags.
                         'grep-tag-default))))
      (setq grep-default (or (car grep-history) grep-command))
      ;; Replace the thing matching for with that around cursor
      (when (string-match "[^ ]+\\s +\\(-[^ ]+\\s +\\)*\\(\"[^\"]+\"\\|[^ ]+\\)\\(\\s-+\\S-+\\)?" grep-default)
        (unless (or (match-beginning 3) (not (stringp buffer-file-name)))
          (setq grep-default (concat grep-default "*."
                                     (file-name-extension buffer-file-name))))
        (setq grep-default (replace-match (or tag-default "")
                                          t t grep-default 2))))))

;;; ----------------------------------------------------------------------
;;;
(defadvice grep (around tinyadvice act)
  "Modify interactive spec: TAB key completes file names."
  (interactive
   (let (grep-default (arg current-prefix-arg))
     (tinyadvice-grep-default arg)
     (list (ti::file-complete-filename-minibuffer-macro
             (read-from-minibuffer "Run grep (like this): "
                                   (or grep-default
                                       grep-command)
                                   map nil 'grep-history)))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;; - More smarter buffer saving.
;;;
(defadvice compile (around tinyadvice dis)
  "Replace original function. More smarter buffer saving.
See function `tinyadvice-compile-save-buffers'.
In addition, TAB key completes file names."
  (interactive
   (if compilation-read-command
       (list (ti::file-complete-filename-minibuffer-macro
               (read-from-minibuffer "Compile command: "
                                     compile-command map nil
                                     '(compile-history . 1))))
     (list compile-command)))
  (setq compile-command command)

  (if (null compilation-ask-about-save)
      (save-some-buffers (not compilation-ask-about-save) nil)
    (tinyadvice-compile-save-buffers))

  (compile-internal compile-command "No more errors"))

;;; ----------------------------------------------------------------------
;;; Run compile with the default command line
;;;
(defadvice recompile (around tinyadvice dis)
  "Replace original function.
More smarter buffer saving, seefunction `tinyadvice-compile-save-buffers'."
  (interactive)
  (if (null compilation-ask-about-save)
      (save-some-buffers (not compilation-ask-about-save) nil)
    (tinyadvice-compile-save-buffers))
  (compile-internal compile-command "No more errors"))


;;; ...................................................... &completion ...

;;; ----------------------------------------------------------------------
;;;
(defadvice call-last-kbd-macro (before tinyadvice dis)
  "If still defining a macro, end it before attempting to call-last.
  This prevents whacking the current definition."
  (if defining-kbd-macro
      (end-kbd-macro)))

;;; ----------------------------------------------------------------------
;;;
(defadvice PC-complete  (around tinyadvice dis)
  "In file name prompt, use case sensitive completion.
Set `completion-ignore-case' locally to nil."
  (let* ((completion-ignore-case  completion-ignore-case)
         word)
    (setq word (or (save-excursion (ti::buffer-read-space-word)) ""))

    (if (string-match "^[/~]" word)
        (setq completion-ignore-case nil))
    ad-do-it))

;;; ----------------------------------------------------------------------
;;;
(defadvice debugger-eval-expression (around tinyadvice dis)
  "Chnage interactive so that it offer word from buffer."
  (interactive
   (list
    (read-from-minibuffer
     "(tinyadvice) Eval: "
     (or (ti::buffer-read-space-word) "")
     read-expression-map t
     'read-expression-history)))
  ad-do-it)


;;; ........................................................... &dired ...

;;; ----------------------------------------------------------------------
;;;
(defadvice dired-mark-read-file-name (around tinyadvice dis)
  "Instead of asking directory, offer full filename for editing."
  (if (and dir (string-match "/" dir))
      (setq dir (dired-get-filename)))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice dired-do-rename  (around tinyadvice act)
  "Offer editing the current filename.
Without this advice you don't get the old filename for editing.
Activates advice 'dired-mark-read-file-name during call."
  (let* ((ADVICE 'dired-mark-read-file-name))
    (ad-enable-advice ADVICE 'around 'tinyadvice)
    (ad-activate ADVICE)
    ad-do-it
    (ad-disable-advice ADVICE 'around 'tinyadvice)
    (ad-activate ADVICE)))

;;; ----------------------------------------------------------------------
;;;
(defadvice dired-man (before tinyadvice dis)
  "Make sure man variables are initialized."
  (require 'man)
  (Man-init-defvars))

;;; ............................................................. &env ...

;;; ----------------------------------------------------------------------
;;;
(defun tinyadvice-read-envvar (prompt &optional require-match)
  "Read an environment variable name from the minibuffer.
Prompt with PROMPT and complete from `process-environment'.
If optional arg REQUIRE-MATCH is non-nil, only defined variable
names are allowed."
  (completing-read
   prompt
   (mapcar (function
            (lambda (var=value)
              (list (substring var=value 0
                               (string-match "=" var=value)))))
           process-environment)
   nil
   require-match))

;;; ----------------------------------------------------------------------
;;;
;;; Hangs sometimes, don't know why..
;;; Currently owned by "my" and disabled. Enable this manyally in load-hook
;;; if you want to try it.
;;;
(defadvice getenv (around my dis)
  "Offer completion."
  (interactive (list (tinyadvice-read-envvar "Get environment variable: " t)))
  ad-do-it
  (if (and (interactive-p)
           ad-return-value)
      (message "%s" ad-return-value)
    ad-return-value))

;;; ----------------------------------------------------------------------
;;;
(defadvice setenv (around tinyadvice dis)
  "Add interactive completion."
  (interactive
   (if current-prefix-arg
       (list (tinyadvice-read-envvar "Clear environment variable: " t) nil t)
     (let ((var (tinyadvice-read-envvar "Set environment variable: ")))
       (list var
             (read-from-minibuffer
              (format "Set %s to value: " var)
              (or (getenv var) ""))))))
  ad-do-it
  (if (and (interactive-p) value)
      (message "%s" value)
    value))

;;; ------------------------------------------------------------ &grep ---
;;;
(defadvice grep  (around tinyadvice  dis)
  "Complete filenames with TAB.
Read word from the current pointand put it into grep prompt."
  (interactive
   (ti::file-complete-filename-minibuffer-macro
     (list
      (read-from-minibuffer
       "(tinyadvice) Run grep: "
       (concat grep-command  (or (ti::buffer-read-space-word) ""))
       map
       nil
       'grep-history))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice find-tag (after tinyadvice-reposition-window act)
  "Call reposition-window after finding a tag."
  (reposition-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;                         files.el
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defadvice find-file-literally
  (around  tinyadvice-disable-write-file-functions dis)
  "Disable `write-file-functions' so that file can edited and saved in pure manner."
  ad-do-it
  (add-hook 'write-file-functions 'ignore nil 'local)
  (setq write-file-functions nil)
  ;; (setq indent-tabs-mode t)
  (message "TinyAdvice: write-file-hooks is now nil in %s" (buffer-name)))

;;; ----------------------------------------------------------------------
;;; 19.30 doesn't offer the filename, so enable this in all emacs versions
;;;
(defadvice find-alternate-file  (around tinyadvice dis)
  "Interactive change: offer buffer filename as default.
Reference:
  `tinyadvice-:find-alternate-file-flag'"
  (interactive
   (list
    (read-file-name
     "find alternate file: "
     (file-name-directory (or (buffer-file-name)
                              default-directory))
     nil
     t
     (if (buffer-file-name)
         (file-name-nondirectory (buffer-file-name))
       (if tinyadvice-:find-alternate-file-flag
           (buffer-name) "")))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice recover-file  (around tinyadvice dis)
  "Offer current buffer's filename in prompt."
  (interactive
   (list
    (read-file-name
     "(TinyAdvice) Recocer file: "
     (file-name-directory (or (buffer-file-name)
                              default-directory))
     nil
     t
     (if (buffer-file-name)
         (file-name-nondirectory (buffer-file-name))
       (if tinyadvice-:find-alternate-file-flag
           (buffer-name) "")))))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice write-file (around tinyadvice-file dis)
  "File handling additions.

Interactive change:

    Changes the interactive prompt so, that full `buffer-file-name' is given
    for editing.

Confirm overwrite:

    When called interactively, require confirmation if FILENAME already exists.
    If FILENAME matches `tinyadvice-:write-file-no-confirm', no confirmation
    is asked."
  (interactive
   ;; Change    "Fwrite to file: "
   (list
    (read-file-name
     "write to file: "
     (or (buffer-file-name)
         "~/"))))

  (let* ((fn           (ad-get-arg 0))
         ;;  Tmp buffers do not have filename
         (buffer-file  (or fn (buffer-file-name) ""))
         (pass         t))

    (if (stringp fn)
        (setq pass
              (ti::string-match-case tinyadvice-:write-file-no-confirm fn)))

    (if (or (not (interactive-p))   ;only when user call it, do checks
            (not (file-exists-p fn))
            pass
            (y-or-n-p (format "%s already exists; overwrite it? " fn)))
        ad-do-it
      (message "Aborted"))))

;;; ----------------------------------------------------------------------
;;;
(defadvice write-region (around tinyadvice-file dis)
  "See `write-file' which explains the advice behavior."
  (interactive "r\nFwrite region: ")
  (let* ((fn           (ad-get-arg 2))
         (buffer-file  fn)
         (pass         t))
    (if (stringp fn)
        (setq pass
              (ti::string-match-case tinyadvice-:write-file-no-confirm fn)))
    (if (or (not (interactive-p))
            (not (file-exists-p fn))
            pass
            (y-or-n-p (format "%s already exists; overwrite it? " fn)))
        ad-do-it
      (message "Aborted"))))

;;; ----------------------------------------------------------------------
;;;
(defadvice save-some-buffers (before tinyadvice dis)
  "Always save changed abbrevs without questions if `save-abbrevs' is set."
  (when (and save-abbrevs abbrevs-changed)
    (write-abbrev-file nil)
    (setq abbrevs-changed nil)))

;;; ............................................................. &gud ...

;;; ----------------------------------------------------------------------
;;; See gud.el
;;;
(defadvice gud-display-line (after tinyadvice dis)
  "Highlight current line."
  (when (and tinyadvice-:gud-overlay
             (fboundp 'move-overlay))
    (let ((ov tinyadvice-:gud-overlay)
	  (bf (gud-find-file true-file)))
      (save-excursion
        (set-buffer bf)
        (move-overlay
         ov
         (line-beginning-position)
         (line-end-position)
         (current-buffer))))))

;;; ............................................................ &mail ...

;;; ----------------------------------------------------------------------
;;; See mailabbrev.el
;;;
(defadvice sendmail-pre-abbrev-expand-hook
  (around tinyadvice-no-abbrevs-in-body dis)
  "Do not expand any abbrevs in the message body through `self-insert-command'."
  (if (or (mail-abbrev-in-expansion-header-p)
          ;; (not (eq last-command 'self-insert-command)) ; can't be used
          ;; since last-command is the previous, not the current command
          (not (integerp last-command-char))
          (eq (char-syntax last-command-char) ?w)) ; this uses that
      ;; the last char in {C-x '} {C-x a '} {C-x a e} is `w' syntax
      ad-do-it
    (setq abbrev-start-location (point) ; this hack stops expand-abbrev
          abbrev-start-location-buffer (current-buffer))))


;;; ......................................................... &map-ynp ...

;; 19.28
;; - map-ynp.el::map-y-or-n-p  Get's loaded in loadup.el, it pops up
;;   an dialog Box of questions if the input is event type and it is
;;   annoying to answer yes/no dialog boxes. It is much quicker to
;;   hit SPACE/DEL for yes/no.
;; - Hmm actually it looks back what the command was by looking at
;;   `last-nonmenu-event' variable, so I should reset it instead.
;; - *argh* I was wrong, it is the `y-or-n-p' (built-in) command that pops up
;;   the dialog, anyway the advice works for it too: built-in or not
;;   doesn't matter
;;
;; The way to do this in XEmacs is:
;;
;;    (setq use-dialog-box nil)

(when (and (ti::compat-window-system)
           (ti::emacs-p))
  (defadvice map-y-or-n-p (before tinyadvice dis)
    "Reset any mouse event to key event so that no dialogs are displayed."
    (if (listp last-nonmenu-event)
        ;; replace with some harmless value
        (setq last-nonmenu-event ?\n)))
  (defadvice y-or-n-p (before tinyadvice dis)
    "Reset any mouse event to key event so that no dialogs are displayed."
    (if (listp last-nonmenu-event)
        ;; replace with some harmless value
        (setq last-nonmenu-event ?\n))))

;;; ........................................................... &mouse ...

;;; ----------------------------------------------------------------------
;;;
(defadvice mouse-wheel-scroll-screen (around tinyadvice act)
  "Use tinymy.el scrolling if possible."
  (if (and (fboundp 'tinymy-scroll-down)
           (fboundp 'tinymy-scroll-up))
      (let ((event (ad-get-arg 0)))
        (ignore-errors
          (if (< (car (cdr (cdr event))) 0)
              (tinymy-scroll-down)
            (tinymy-scroll-up))))
    ad-do-it))

;;; ----------------------------------------------------------------------
;;;
(defadvice mouse-delete-other-windows  (around tinyadvice dis)
  "Confirm window delete."
  (if (y-or-n-p "Really delete _all_ windows ")
      ad-do-it
    (message "")))

;;; ----------------------------------------------------------------------
;;;
(defadvice mouse-delete-window  (around tinyadvice dis)
  "Confirms window delete."
  (if (y-or-n-p "Really delete _this_ window ")
      ad-do-it
    (message "")))


(defadvice occur  (before tinyadvice act)
  "Iinteractive change: ask if user want the occur to start from `point-min'.
also Possibly unfold/un-outline the code."
  (when (and (interactive-p)
             (not (eq (point) (point-min)))
             (y-or-n-p "TinyAdvice: Start occur from point-min? "))
    (if (and (or (and (featurep 'folding)
                      (symbol-value 'folding-mode))
                 (and (and (featurep 'outline)
                           (boundp  'outline-mode))
                      (symbol-value 'outline-mode)))
             (save-excursion
               (ti::pmin)
               (re-search-forward "\r" nil t))
             (y-or-n-p "TinyAdvice: Open buffer's selective display too? "))
        (ti::buffer-outline-widen))))

;;; .......................................................... &simple ...

;;; ----------------------------------------------------------------------
;;; See simple.el
;;;
(defadvice exchange-point-and-mark (around tinyadvice-pop-if-prefix dis)
  "If given prefix, call `set-mark-command' to pop previous mark positions."
  (if (and current-prefix-arg
           (interactive-p))
      (call-interactively 'set-mark-command))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice goto-line (around tinyadvice dis)
  "Widen the buffer before and after `goto-line' command."
  (widen)
  ad-do-it
  ;;  We do this because, the folding.el sets narrowing in effect,
  ;;  when the goto-line has finished.
  ;;  #todo: should we check featurep 'folding?
  (widen))

;;; ----------------------------------------------------------------------
;;;
(defadvice indent-for-comment (around tinyadvice dis)
  "Kill the comment with negative prefix."
  (if (eq current-prefix-arg '-)
      (kill-comment nil)
    ad-do-it))

;;; ----------------------------------------------------------------------
;;; Redefine insert-buffer to insert a visible buffer, if there's one.
;;;
(defadvice insert-buffer (before tinyadvice dis)
  "Use a more reasonable default, the other window's content."
  (interactive
   (list
    (progn
      (barf-if-buffer-read-only)
      (read-buffer "Insert buffer: "
                   (if (eq (selected-window)
                           (next-window (selected-window)))
                       (other-buffer (current-buffer))
                     (window-buffer (next-window (selected-window))))
                   t)))))

;;; ----------------------------------------------------------------------
;;; avoid deactivation of region when buffer end or beginning is reached
;;;
(defadvice line-move (around tinyadvice dis)
  "Avoid deactivation of region. in `beginning-of-buffer' or `end-of-buffer'."
  (condition-case ()
      ad-do-it
    ((beginning-of-buffer end-of-buffer)
     (if (bobp)
         (message "Beginning of buffer.")
       (message "End of buffer.")))))

;;; ----------------------------------------------------------------------
;;;
(defadvice set-mark-command (around tinyadvice-global-if-negative dis)
  "If the argument is negative, call `pop-global-mark'."
  (if (< (prefix-numeric-value current-prefix-arg) 0)
      (pop-global-mark)
    ad-do-it))

;;; ----------------------------------------------------------------------
;;;
(defadvice what-cursor-position (around tinyadvice dis)
  "Displays line number info too."
  ad-do-it
  ;;  we have to use 'princ' because there is percentage mark
  ;;  in returned string and that would run 'message' beserk,
  ;;  since it thinks it's formatting code
  (princ (concat
          ad-return-value
          (number-to-string (ti::widen-safe (ti::current-line-number))))))

;;; ----------------------------------------------------------------------
;;;
(defadvice switch-to-buffer (around tinyadvice dis)
  "When called interactively: Confirm switch to non-existing buffer.

References:

  `tinyadvice-:switch-to-buffer-find-file'
   if non-nil, suggest `find-file' for non-existing buffers"
  (interactive "Bbuffer name: ")
  (let ((buffer-name (ad-get-arg 0)))
    (if (or (not (interactive-p))       ;user didn't call us
            (get-buffer buffer-name))   ;it exists
        ad-do-it
      (cond
       ((y-or-n-p (format "`%s' does not exist, create? " buffer-name))
        ad-do-it)                       ;ceate new buffer

       (tinyadvice-:switch-to-buffer-find-file ;is this enabled ?
        (find-file (read-file-name "(tinyadvice) Find-file: "
                                   nil
                                   nil
                                   nil
                                   buffer-name)))))
    (message "")))                      ;clear the echo area

;;; ----------------------------------------------------------------------
;;;
(defadvice switch-to-buffer-other-frame  (around tinyadvice dis)
  "Replace function. Don't ever create new frame; reuse some existing frame."
  (let ((free-frames (ti::window-frame-list nil 'exclude-current))
        stat)
    (if (null free-frames)
        (pop-to-buffer buffer)
      (cond
       ((setq stat (ti::window-get-buffer-window-other-frame buffer))
        ;;  buffer is displayed already in some OTHER frame; go to it.
        (raise-frame (car stat))
        (select-frame (car stat))
        (select-window (cdr stat)))
       (t
        ;;  Go to some free frame and pop up there
        (raise-frame  (car free-frames))
        (select-frame (car free-frames))
        (switch-to-buffer buffer))))))

;;; ----------------------------------------------------------------------
;;; - This puts cursor to generated list. Propably what we
;;;   want 99% of the time.
;;;
(defadvice list-buffers  (after tinyadvice dis)
  "Select buffer list after display."
  (if (interactive-p)
      (select-window (get-buffer-window "*Buffer List*"))))

;;; ........................................................... &other ...

;;; ----------------------------------------------------------------------
;;;
(mapc
 (function
  (lambda (x)
    (eval
     (`
      (defadvice (, x) (around tinyadvice-kill-buffer act)
        "Kill the buffer if there is no process."
        (condition-case error
            ad-do-it
          (error
           (if (equal error '(error "Current buffer has no process"))
               (kill-buffer (current-buffer))))))))))
 '(term-copy-old-input
   term-send-input
   term-send-raw-string))

;;; ----------------------------------------------------------------------
;;; hyberbole package
;;;
(defadvice hkey-help-show (around tinyadvice-shrink-window act)
  "Shrink auxiliary windows to buffer size.
For `help-mode',switch `view-mode' off."
  ;;
  ;; hkey-help-show is part of Bob Wiener's Hyperbole. In pure emacs
  ;; a hook is more appropriate: with-output-to-temp-buffer asks the
  ;; function in the variable temp-buffer-show-function (if non-nil)
  ;; to take care of the showing. That function also must call
  ;; temp-buffer-show-hook. Take your pick.
  ;;
  (if (and (not current-window)         ; second arg
           (get-buffer-window buffer))
      (delete-window (get-buffer-window buffer))) ; force recreation
  ad-do-it
  (if (and (not current-window)         ; second arg
           (not (one-window-p t)))      ; not counting the minibuffer
      (shrink-window-if-larger-than-buffer (get-buffer-window buffer)))
  (if (and (eq major-mode 'help-mode)
           (boundp view-mode) view-mode)
      (view-exit)))

(provide   'tinyadvice)
(run-hooks 'tinyadvice-load-hook)

;;; tinyadvice.el ends here
