;;; tinydesk.el --- Save and restore files between Emacs sessions

;; This file is not part of Emacs

;; Copyright (C)    1995-2010 Jari Aalto
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
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;; Install:

;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   $HOME/.emacs startup file
;;
;;      ;; (add-hook 'tinydesk--load-hook 'tinydesk-recover-last-state)
;;      (add-hook 'tinydesk--load-hook 'tinydesk-install-default-keybindings)
;;      (require 'tinydesk)
;;
;;   or use the autoload feature. Notice that the automatic "file
;;   state backup feature" gets enables only when this file is loaded.
;;   If you want that feature, then use require.
;;
;;      (add-hook 'tinydesk--load-hook 'tinydesk-default-emacs-keybindings)
;;      (add-hook 'tinydesk--load-hook 'tinydesk-recover-last-state)
;;      (autoload 'tinydesk-mode            "tinydesk" "" t)
;;      (autoload 'tinydesk-save-state      "tinydesk" "" t)
;;      (autoload 'tinydesk-unload          "tinydesk" "" t)
;;      (autoload 'tinydesk-recover-state   "tinydesk" "" t)
;;      (autoload 'tinydesk-edit-state-file "tinydesk" "" t)
;;
;;   Suggested keybindings. These are inlcuded in function
;;   `tinydesk-install-default-keybindings'.
;;
;;      (define-key ctl-x-4-map "S" 'tinydesk-save-state)
;;      (define-key ctl-x-4-map "R" 'tinydesk-recover-state)
;;      (define-key ctl-x-4-map "E" 'tinydesk-edit-state-file)
;;      (define-key ctl-x-4-map "U" 'tinydesk-unload)

;;; Commentary:

;;  Preface, Feb 1995
;;
;;      At work working with windowed system, Emacs stays open from day to
;;      day. In fact people seldom even logout, so Emacs and the files
;;      just wait there nicely and there is seldom a need for a sophisticated
;;      session saver.
;;
;;      But sometimes it may be necessary to visit lab next
;;      floor to see what's troubling a C++ program. There has to be a way
;;      to transfer the list of files that was being editing and bring
;;      them into lab where person can replicate the setup.
;;
;;      These functions save Emacs file list into a file which can
;;      later be opened again in Emacs somewhere else. Later Emacs
;;      versions introduced "~/.saves*" files that I found disturbing
;;      occupying the home directory. With this package all the files
;;      are grouped in only one "state" state file, which can be
;;      reused.
;;
;;      Hopefully someone finds use for this also, although there exist
;;      much more better desktop savers, which save points, marks and
;;      modes.
;;
;;  Overview of features
;;
;;      o   Simple desktop: only filenames and directories are read/saved.
;;          Unlike the other desktop savers, this one can also UNLOAD files
;;          from Emacs. You just tell it to remove 'these files listed in
;;          state file state.XXX', and those files will be removed from
;;          your Emacs buffers. You can collect 'projects' and switch
;;          between them easily: after project1, It can can be unload and
;;          load project3 instead.
;;
;;      o   Parse any file that includes filenames and comments
;;
;;      o   If there were any invalid entries in the state file,
;;          the state file contents is shown to user and the entries which
;;          had problems are marked.
;;
;;      o   State file editing (tinydesk-mode):
;;
;;          --  load single file on the line
;;          --  clear face properties from buffer, so that they don't
;;              disturb your view.
;;          --  parse files for loading.
;;          --  Show files that cannot be loaded.
;;
;;      o   In regular intervals save the state of Emacs (files loaded)
;;          If Emacs crashes you can recover the previous session.
;;          See function `tinydesk-auto-save' for more. Similar functionality
;;          (".saves") is in new Emacs releases, but this package
;;          was originally written for Emacs 19.28.
;;
;;      o   CRASH RECOVERY: If Emacs crashes, or you have to kill it
;;          with `-HUP' if it hangs, it leaves autosaved files around. When
;;          you boot up again, you need to reload the existing files AND
;;          recover any autosaved files. The best way to get your Emacs
;;          back where it was, is that you load the state file for editing:
;;          `M-x' `tinydesk-edit-state-file' And from the edit mode
;;          hit command `tinydesk-find-file-whole-buffer' and
;;          `tinydesk-recover-file-whole-buffer' which and you'll be
;;          up again with your latest files.
;;
;;  Quick start
;;
;;      If you're just eager to use the package, here are the basics.
;;      I suppose you have copied the installation setup as is.
;;
;;      o   You have Emacs session open with bunch of files. Now you
;;          believe that it's time to save this session. You do
;;          `C-x' `4' `s' and give some name "state.c" if you worked
;;          with a C project.
;;
;;      Now, it all depends what you want to do after that. You may
;;      `find-file' more files to Emacs; or kill few unwanted
;;      buffers. Re-execute `C-x' `4' `s' whenever you like. You can
;;      even edit the state file with `C-x' `4' `e' to remove some files
;;      that you don't want to include to that "project".
;;
;;      o   Next time you open Emacs you can load any state file with
;;          C-x 4 r "state.c"
;;
;;      If you want to switch between projects; unload first the current
;;      project with `C-x' `4' `u' "state.c" and reload some other project
;;      with `C-x' `4' `r', e.g. your current C++ project "state.cc"
;;
;;  Automatic session handling
;;
;;      Some people just want to save the session on exit and reopen it
;;      when Emacs starts again. I must say that this is not necessarily
;;      the best, because when you start Emacs for some quick job, you
;;      don't necessarily want it to load the saved session (loading all
;;      files takes some time). Loading Emacs with `-q' is not the
;;      choice, if you still like to have your other Emacs goodies active.
;;
;;      Here is semi-automatic save and restore, put all these, in
;;      addition to ones mentioned at the "install" section, lines
;;      near the end of your $HOME/.emacs. The setup saves the state
;;      when Emacs exists and asks if you want to return to saved
;;      session on Emacs startup. Thanks to Gary
;;      <help-gnu-emacs@garydjones.name> for the command line option
;;      code.
;;
;;          ;; tinydesk-activate.el -- Automatic desktop save and recover
;;
;;          (defconst my-tinydesk-session-mode 'ask
;;            "If 'ask, confirm before resatoring last session.
;;          If nil, automatically restore last session; but if command
;;          line opion --no-desktop is set, do not load session.")
;;
;;          (setq tinydesk--directory-location "~/tmp")
;;
;;          (defconst my-tinydesk-session
;;            (format "%s/%s" tinydesk--directory-location "state.last-session"))
;;
;;          (autoload 'tinydesk-recover-state "tinydesk" "" t)
;;          (autoload 'tinydesk-save-state    "tinydesk" "" t)
;;
;;          (defun my-save-session ()
;;            "Save loaded files to state file."
;;            ;;  if you want to save dired buffers too.
;;            ;;  use (tinydesk-save-state my-tinydesk-session '(4))
;;            (tinydesk-save-state my-tinydesk-session)
;;            nil)
;;
;;          (defun my-tinydesk-activate ()
;;            ;; To save at exit
;;            (add-hook 'kill-emacs-hook 'my-save-session)
;;            ;; To save periodically
;;            (add-hook 'auto-save-hook 'my-save-session))
;;
;;          (my-tinydesk-activate)
;;
;;          ;; Should we recover?
;;          (cond
;;           (my-tinydesk-session-mode
;;            (when (and (file-exists-p my-tinydesk-session)
;;                   (y-or-n-p "Recover session "))
;;              (tinydesk-recover-state my-tinydesk-session)))
;;            ((member "--no-desktop" command-line-args)
;;             (message "My: option --no-desktop; bypass recovering Tiny Desktop session."))
;;            ((file-exists-p my-tinydesk-session)
;;             (message "My: recovering Tiny desktop session %s" my-tinydesk-session)
;;             (tinydesk-recover-state my-tinydesk-session)))
;;
;;          ;; End of file
;;
;;  Face setup
;;
;;      This program uses some faces to catch your attention when you're
;;      working with the state files. If you restore state from a file and
;;      some file reference cannot be loaded, the state file will be shown
;;      to you and the problematic lines are highlighted. If you open the
;;      state file for editing, you can selectively load files. The mouse
;;      pointer will change and the text is again highlighted. To make the
;;      highlight work for you, you must set some colors like this
;;
;;         (set-face-foreground 'italic "LightBlue")
;;
;;  About saving the files
;;
;;      While you may save your session files with any name, here is one
;;      convention that you could use. Name every filename so, that they
;;      have common prefix:
;;
;;          M-x tinydesk-save-state   ;; or any hotkey you have bound this to
;;          state.XXX
;;
;;      The XXX describes the name of the state file you just saved. Later
;;      on it's easier to use Emacs file name completion capability to load
;;      the file you want. If you don't exactly remember what files you
;;      saved, or which sessions you have in dir, you just type
;;
;;          state.[TAB]
;;
;;      when `tinydesk-recover-state' ask for filename.
;;      Prefix arg to `tinydesk-save-state saves' says to load directories too.
;;
;;  Automatic state file saving
;;
;;      Emacs 19.29+ has feature that makes it possible to recover a session.
;;      See bunch of `auto-save-list-*' variables.
;;
;;      Has it ever happened to you that Emacs crashed mystically when you
;;      were in the middle of your daily routines. You had several C++
;;      files open, perl code, text files, RMAIL, ... This package installs
;;      `tinydesk-auto-save' function to `write-file-functions' and in regular
;;      intervals all your Emacs session files are stored into the state
;;      file. After a crash you can easily recover your session by reading
;;      the saved state file information with `tinydesk-recover-state'
;;      <FILE>. The name of the file of the latest saved state is in file
;;      "periodic"
;;
;;  Development note
;;
;;      There is no plan to duplicate *desktop.el* functionality to save points
;;      and modes and so on. This is for simple file/directory restoring only.

;;; Change Log:

;;; Code:

(require 'tinylibm)

(ti::package-defgroup-tiny TinyDesk tinydesk-- extensions
  "Simple desktop: only filenames and directories are read/saved.

            Unlike the other desktop savers, this one can also UNLOAD files
            from Emacs. You just tell it to remove 'these files listed in
            state file state.XXX', and those files will be removed from
            your Emacs buffers. You can collect 'projects' and switch
            between them easily: after project1, It can can
            be unload and load project3 instead.

            Files that have been modified are not unloaded.
    ")

;;; ......................................................... &v-hooks ...

(defcustom tinydesk--mode-define-keys-hook
  '(tinydesk-default-mode-bindings)
  "*List of functions to run which define keys to `tinydesk-mode-map'."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--save-before-hook nil
  "*Hook run just before _writing_ files to STATE file.
begins. This is your chance to do something to the buffers."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--save-after-hook nil
  "*Hook just before _saving_ of STATE file.
The files are there, possibly in sorted order, and the title is there."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--mode-hook nil
  "*Hook run after the `tinydesk-mode' is turned on."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--recover-before-hook nil
  "*Hook run after recover file is loaded, just before processing start."
  :type  'hook
  :group 'TinyDesk)

(defcustom tinydesk--recover-after-hook nil
  "*Hook run after recover file is _parsed_ AND no errors during load."
  :type  'hook
  :group 'TinyDesk)

;;; ................................................... &v-user-config ...

(defcustom tinydesk--comment-characters ";#"
  "*A string containing comment start characters in state file.
The default value is ';#'."
  :type  'string
  :group 'TinyDesk)

(defcustom tinydesk--mode-name "TinyDesk"
  "*Editing STATE files mode name."
  :type  'string
  :group 'TinyDesk)

(defcustom tinydesk--directory-location
  (or
   (file-name-as-directory
    (file-name-directory (ti::package-config-file-prefix "tinydesk.el")))
   (and (file-directory-p "~/tmp")
        "~/tmp")
   (error "\
TinyDesk: Can't set default value for `tinydesk--directory-location'"))
  "*Default directory where to save and restore files."
  :type 'directory
  :group 'TinyDesk)

(defcustom tinydesk--directory-save-suggested 'default
  "*How the state file's directory location is suggested.
'last        Offer last saved directory.
'default     Always offer the default directory `tinydesk--directory-location'"
  :type  '(choice
           (const last)
           (const default))
  :group 'TinyDesk)

(defcustom tinydesk--auto-save-interval 5
  "*Interval between doing auto-save of Emacs state.
If set to 5, after every 5th `write-file' the state is saved.

The interval cannot be smaller than 5. It is reseted to 10 if
it's smaller than 5.

See variable `tinydesk--auto-save-name-function' and
function `tinydesk-auto-save' for more information."
  :type  '(integer :tag "Save interval")
  :group 'TinyDesk)

(defcustom tinydesk--auto-save-name-function  nil
  "*Function to return a full path name for auto-save file.
If this variable is nil, then default name is derived from frame's
first element and it used in `tinydesk--directory-location'

For full documentation, see function `tinydesk-auto-save'"
  :type 'function
  :group 'TinyDesk)

(defcustom tinydesk--save-exclude-regexp
  (concat
   ;; Gnus
   "dribble\\|drafts"
   ;;  Do save mail buffers; because you want to call M-x rmail
   ;;  instead.
   "\\|RMAIL\\|VM\\|MH"
   ;;  TinyDesk suggested "state.<name>" files
   "\\|state\\."
   ;;  No ange ftp buffers
   "\\(ftp\\|anonymous\\)@.*/"
   ;;  No files from these directories
   "\\|^/tmp/\\|/junk/\\|/trash/\\|/[aA]utosaved?/")
  "*Regexp of files that are not saved to state file.
match is case sensitive. If you do want not case sensitive match, you
have to do set this variable to nil and use your own line delete:

   (setq tinydesk--save-after-hook      'my-tinydesk--save-after-hook)
   (defun my-tinydesk--save-after-hook ()
      (flush-lines REGEXP))"

  :type  '(string :tag "Regexp")
  :group 'TinyDesk)

(defcustom tinydesk--save-title
  '(progn
     (format "\
;; Emacs tinydesk.el state file
;;
;;
;;       Date: %s
;;       M-x load-library RET tinydesk RET
;;       M-x tinydesk-recover-state RET %s RET

"
      (ti::date-standard-date 'short)
      (if (boundp 'file)
          file ;; visible in function `tinydesk-save-state'
        "<file>")))
  "*A lisp form to be included at the beginning of state file."
  :type  'sexp
  :group 'TinyDesk)

(defcustom tinydesk--save-and-sort t
  "*Non-nil to sort the file list in state file.
nil to preserve `buffer-list' order."
  :type  'boolean
  :group 'TinyDesk)

;;  Set to nil if you don't want title.

(defcustom tinydesk--get-save-file-function 'tinydesk-get-save-files
  "*Function to return list of filenames that are stored to state file.
This function isn't run if `tinydesk-save-state' is explicitely
run with parameter FILES.

Arguments passed to function:
  mode          flag passed by `tinydesk-save-state'"

  :type  'function
  :group 'TinyDesk)

(defcustom tinydesk--face-table
  '((file-pick .  highlight)
    (error     .  italic))
  "*Alist of faces used for marking text.
The default value is

\(setq tinydesk--face-table
  '((file-pick .  highlight)
    (error     .  italic)))"
  :type '(list
          (cons
           (const  file-pick)
           (symbol :tag "Face"))
          (cons
           (const error)
           (symbol :tag "Face")))
  :group 'TinyDesk)

;;; ....................................................... &v-private ...

(defvar tinydesk-mode-map nil
  "Local keymap for STATE files loaded by edit.")

(defvar tinydesk--directory-last nil
  "Directory that was used for last save.")

(defvar tinydesk--tmp-buffer "*tmp*"
  "The work buffer used, created and killed when needed.")

(defvar tinydesk--trash-tmp-buffer  t
  "If non-nil, the work buffer is always deleted.")

(defvar tinydesk--message-column 60
  "Column where to put possible messages regarding file.")

(defvar tinydesk--auto-save-counter 0
  "Counter incremented every every time `write-file' event happens.
See. `tinydesk-auto-save'.")

(defvar tinydesk--loaded-file-list nil
  "Overwritten when files are loaded. List.
Contain files that were loaded by `tinydesk-find-file-whole-buffer'.
Hooks may check the contents of this.")

(defvar tinydesk--rejected-file-list nil
  "Overwritten when files are loaded. List.
Contain files that were *not* loaded by
`tinydesk-find-file-whole-buffer'. Reason may be anything: incorrect filename,
path, garbage at line...Hooks may check the contents of this.")

(defconst tinydesk--comment-start-level 1
  "Which sub expression is the comment start.")

(defvar tinydesk--last-state-file  nil
  "Last state file loaded is stored here.")

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-install-default-keybindings ()
  "Install package under `ctl-x-4-map'
\\{ctl-x-4-map}"
  (interactive)
  (define-key ctl-x-4-map "S" 'tinydesk-save-state) ;; free in Emacs
  ;;  This was find-file-read-only-other-window
  (define-key ctl-x-4-map "R" 'tinydesk-recover-state)   ;; Not free
  (define-key ctl-x-4-map "E" 'tinydesk-edit-state-file) ;; free in Emacs
  (define-key ctl-x-4-map "U" 'tinydesk-unload)) ;; free in Emacs

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-default-mode-bindings ()
  "Define default key bindings to `tinydesk-mode-map'."
  (when (ti::emacs-p)
    ;;  - Don't want to use mouse-2 because it's for PASTE.
    ;;  - The others are put to mouse-2 because there is not
    ;;    not always 3 button mouse available.
    (define-key tinydesk-mode-map [mouse-3] 'tinydesk-mouse-load-file)
    ;;  - When editing a file, those colors might be too annoyinng,
    ;;    so you can remove properties with this. Loading is disabled too
    ;;  - Remeber, Emacs is slow with this... wait some time.
    (define-key tinydesk-mode-map [S-mouse-2]
      'tinydesk-clear-buffer-properties)
    ;;  To make buffer loadable by mouse again, run this
    (define-key tinydesk-mode-map [C-mouse-2]
      'tinydesk-mark-buffer-loadable)
    ;;  To mark files that are not loadable, check for possibly typo in
    ;;  filename
    (define-key tinydesk-mode-map [C-M-mouse-2]
      'tinydesk-set-face-non-files-buffer))

  (when (ti::xemacs-p)
    (define-key tinydesk-mode-map [(button3)]
      'tinydesk-mouse-load-file)
    (define-key tinydesk-mode-map [(shift button2)]
      'tinydesk-clear-buffer-properties)
    (define-key tinydesk-mode-map [(control button2)]
      'tinydesk-mark-buffer-loadable)
    (define-key tinydesk-mode-map [(control alt button2)]
      'tinydesk-set-face-non-files-buffer))

  ;;  Non-window system. Alphabetical order
  (define-key tinydesk-mode-map "\C-c\C-m" 'tinydesk-load-file)
  (define-key tinydesk-mode-map "\C-cb" 'tinydesk-find-file-whole-buffer)
  (define-key tinydesk-mode-map "\C-cB" 'tinydesk-recover-file-whole-buffer)
  (define-key tinydesk-mode-map "\C-cc" 'tinydesk-clear-buffer-properties)
  (define-key tinydesk-mode-map "\C-cl" 'tinydesk-mark-buffer-loadable)
  (define-key tinydesk-mode-map "\C-cu" 'tinydesk-unload-current-file)
  (define-key tinydesk-mode-map "\C-cx" 'tinydesk-expunge-unloaded)
  (define-key tinydesk-mode-map "\C-cr" 'tinydesk-remove-file-coments)
  (define-key tinydesk-mode-map "\C-cn" 'tinydesk-set-face-non-files-buffer))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-comment ()
  "Return comment."
  (make-string
   2
   (string-to-char
    (substring tinydesk--comment-characters 0 1 ))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-comment-re ()
  "Return comment regexp."
  `,(concat "[^\n"
	    tinydesk--comment-characters
	    "]*\\(["
	    tinydesk--comment-characters
	    "].*\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-read-word ()
  "Read filename word."
  ;;   Windows use spaces in file names
  (let ((word (ti::remove-properties
               (ti::string-remove-whitespace
                (ti::buffer-read-word "- a-zA-Z0-9_/.!@#%&{}[]+:;~`<>")))))
    ;;  Remove comments from the end
    (if (string-match "\\(.+[^ \t]\\);;" word)
        (match-string 1 word)
      word)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-tmp-buffer (&optional clear)
  "Return temp buffer. optionally CLEAR it."
  (ti::temp-buffer tinydesk--tmp-buffer clear))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-file-name-absolute  (file)
  "Add `default-directory' to FILE if it has no directory."
  (if file
      (if (not (string-match "/" (or file "")))
          (setq file (concat default-directory file))))
  file)

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydesk-mode-map-activate ()
  "Use local \\{tinydesk-mode-map} on this buffer."
  (use-local-map tinydesk-mode-map))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-add-space-if-non-space ()
  "Add extra space if previous character is non-space to make room."
  (let ((prev (char-to-string
	       (char-after
		(1- (point))))))
    (unless (string-match "[ \t]" prev)
      ;;  So that comment doe snot get inserted next to the filename
      (insert " "))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-get-save-dir ()
  "Return suggested save directory."
  (let* ((type      tinydesk--directory-save-suggested)
	 (last      tinydesk--directory-last)
	 (dir       tinydesk--directory-location)
	 (ret       dir))               ;set default return value
    (if (and (eq type 'last)
             (stringp last)
             (file-writable-p last))
        (setq ret last))
    (or ret
        dir
        "~/")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-only-files-buffer ()
  "Remove all comments and empty lines from buffer and leave 1st word."
  (interactive)
  (tinydesk-only-files-region (point-max) (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-only-files-region (beg end)
  "Remove comments BEG END and empty lines from region and leave 1st word.
This way you can rip off all comments and leave filenames."
  (interactive "r")
  (let ((sub-level     tinydesk--comment-start-level)
	(comment-re    (tinydesk-comment-re))
	(empty-re      "^[ \t]*$\\|$")
	mark-end
	p
	maxp
	word
	tmp)
    (if (> beg end)
        (setq tmp beg  beg end  end tmp))
    (save-excursion
      (goto-char end)
      ;;  markers has to be used, beacuse we delete lines and points move
      (setq mark-end (point-marker))
      (goto-char beg)
      (while (< (point) (marker-position mark-end))
        (setq p (point)  maxp nil)
        (catch 'next
          (if (null (looking-at empty-re))
              nil
            (ti::buffer-kill-line)
            (throw 'next t))
          (if (null (looking-at comment-re))
              nil
            (if (match-beginning sub-level)
                (setq maxp (match-beginning sub-level))))
          (if (and maxp (eq maxp p))    ;BEG of line comment
              (progn
                (ti::buffer-kill-line) (throw 'next t)))
          (setq word (tinydesk-read-word))
          (ti::buffer-kill-line)
          ;; The \n make cursor forward
          (if word
              (insert word "\n")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-trash ()
  "Kill temporary buffer if user has requested it.

References:
  `tinydesk--tmp-buffer'
  `tinydesk--trash-tmp-buffer'"
  (and tinydesk--trash-tmp-buffer
       (get-buffer tinydesk--tmp-buffer)
       (kill-buffer  (get-buffer tinydesk--tmp-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-dired-table  ()
  "Return dired buffer table '((directory  dired-buffer) ...)."
  (interactive)
  (let ((blist   (buffer-list))
        ;;  ByteCompiler doesn't know that I do
        ;;  (eq major-mode 'dired-mode) test before I use this variable,
        ;;  so hide it from it.
        ;;
        ;;  The variable is defined if it passed the eq test
        (sym     'dired-directory)
        list)
    (dolist (elt blist)
      (with-current-buffer elt
        (if (eq major-mode 'dired-mode)
            (push
             (list (symbol-value sym)
                   (current-buffer))
             list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-auto-save-file-name ()
  "Return state file name for auto save. See function `tinydesk-auto-save'.
References:
  `tinydesk--directory-location'
  `tinydesk--auto-save-name-function'."
  (let* ((func       tinydesk--auto-save-name-function)
         (dir        (or tinydesk--directory-location "~" ))
         (name       (or (and (boundp 'command-line-args)
                              (nth 1 (member "-name" command-line-args)))
                         "periodic"))
         (fn         (concat
                      (file-name-as-directory dir)
                      "emacs-config-tinydesk-autosave-"
                      (or name "saved"))) ;; default
         (save-to    fn))
    (if func
        (setq save-to (funcall func)))
    save-to))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-auto-save (&optional force)
  "This function is installed in `write-file-functions'. Periodic auto save.

Input:

  FORCE     Do autosave immediately

Every Nth time the state of the Emacs  (which files were loaded into Emacs)
is saved, so that you can recover the same session if Emacs crashes.

The default state name is derived in the following manner

o  use directory `tinydesk--directory-location'
o  add string \"emacs-tinydesk-autosave-\"
o  get frame's first word, usually the one that gets set when
   you use -name XXX switch in Emacs command line. If Emacs is being
   run with -nw option, the frame name returns \"terminal\"
o  if there is no frame name, then use \"periodic\"

   Possible yielding: ~/elisp/config/state.saved

References:

  `tinydesk--auto-save-counter'
  `tinydesk--auto-save-interval'       every Nth write"
  (interactive "P")
  (let ((backup-inhibited t)
	(save-to (tinydesk-auto-save-file-name)))
    (when (stringp save-to)
      ;;  - Be extra careful, because we're in write file hook
      ;;  - Make sure we always succeed
      (if (not (integerp tinydesk--auto-save-counter)) ;; init if not int
          (setq tinydesk--auto-save-counter 0))
      (if (not (integerp tinydesk--auto-save-interval)) ;; user didn't set this?
          (setq tinydesk--auto-save-interval 10))
      (if (< tinydesk--auto-save-interval 5) ;; Must be more than 5
          (setq tinydesk--auto-save-interval 10))
      (incf tinydesk--auto-save-counter)
      ;;  time's up? Select name if it's string.
      (cond
       ((or force
            (> tinydesk--auto-save-counter tinydesk--auto-save-interval))
        ;;   Actually tinydesk-save-state generates new call to this
        ;;   function but, it won't come in this COND, because the counter
        ;;   value is different.
        (setq tinydesk--auto-save-counter 0)
        ;; Try chmod, if it fails, then signal error
        (if (and (file-exists-p save-to)
                 (not (file-writable-p save-to)))
            (set-file-modes
             save-to
             (ti::file-mode-make-writable (file-modes save-to))))
        ;;  Still no luck after chmod ?
        (if (or (not (file-directory-p (file-name-directory save-to)))
                (and (file-exists-p save-to)
                     (not (file-writable-p save-to))))
            (error "\
TinyDesk: Can't do state autosave: [%s] is not writable." save-to))
        (save-window-excursion
          (save-excursion
            (message "TinyDesk: state backup in file %s" save-to)
            (tinydesk-save-state save-to)))))
      ;; `write-file-hook' function must return nil
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-face (face)
  "Return FACE."
  ;;  This way the global variable does not float around the file
  (cdr (assoc face tinydesk--face-table)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-clear-line-properties ()
  "Remove properties from the line."
  (set-text-properties (line-beginning-position) (line-end-position) nil)
  (set-buffer-modified-p nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-clear-buffer-properties ()
  "Remove properties and EOL comments from buffer."
  (interactive)
  (let ((c-chars       tinydesk--comment-characters)
	(c-lev         tinydesk--comment-start-level)
	(c-re          (tinydesk-comment-re))
	beg)
    (tinydesk-clear-region-properties (point-min) (point-max))
    (save-excursion
      (ti::pmin)
      (while (not (eobp))
        ;;  Skip over BOL comments
        (when (and (not (looking-at (concat "^[ \t]*[" c-chars "]+\\|$")))
                   (looking-at c-re)
                   (setq beg (match-beginning c-lev)))
          (goto-char beg)
          (skip-chars-backward "[ \t]") ; delete leading whitespace too
          (delete-region (point) (line-end-position)))
        (forward-line 1)))
    (set-buffer-modified-p nil)
    (if (interactive-p)
        (message "TinyDesk: properties cleared from buffer"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-clear-region-properties (beg end)
  "Remove properties from BEG END."
  (set-text-properties beg end nil)
  (set-buffer-modified-p nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-line-property-set-error ()
  "Set line face to signify error."
  (let (beg
	end)
    (save-excursion
      (beginning-of-line)               (setq beg (point))
      (skip-chars-forward "^ \t\n")     (setq end (point)))
    ;; clear first full line
    (put-text-property beg (line-end-position) 'face 'default)
    (put-text-property beg end 'face (tinydesk-face 'error))
    (set-buffer-modified-p nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-handle-text-property (p text)
  "Look property P and run command under TEXT."
  (let* ((file       (file-name-nondirectory text))
         (loaded     (get-buffer file)) ;in Emacs already ?
         (comment    (tinydesk-comment))
         (err-col    tinydesk--message-column))
    (cond
     ((eq p (tinydesk-face 'file-pick))
      (cond
       ((null (file-exists-p text))
        (message (concat "TinyDesk: File not exist, " text))
        (tinydesk-clear-line-properties))
       (loaded
        (message "TinyDesk: File already loaded.")
        (tinydesk-clear-line-properties))
       (t
        (find-file-noselect text)
        (message "TinyDesk: Loaded ok.")
        (tinydesk-clear-line-properties)
        (move-to-column err-col t)
        (unless (looking-at "$\\|[ \t]*$")
          (end-of-line))
        (tinydesk-add-space-if-non-space)
        (insert comment " loaded")
        (beginning-of-line)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-set-face-non-files-region (beg end)
  "In region BEG END set 'error face to invalid files (first word).
Also add textual comment to the end of line if needed."
  (interactive "r")
  (let ((empty-re      "^[ \t]*$")
	(sub-level     tinydesk--comment-start-level)
	(c-chars       tinydesk--comment-characters)
	(comment       (tinydesk-comment))
	(comment-re    (tinydesk-comment-re))
	(err-col       tinydesk--message-column)
	word)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (ti::pmin)
        (while (not (eobp))
          ;;  - ignore empty lines and BEG of line comments.
          (if (or (looking-at empty-re)
                  (and (looking-at comment-re)
                       (eq (match-beginning sub-level) (point))))
              nil
            (setq word (tinydesk-read-word))
            (if (and word (file-exists-p word))
                nil
              (tinydesk-line-property-set-error) ;put color on line
              ;;  Show to user that does not see the color
              (move-to-column err-col t)
              ;; Is the filename that long, that it goes past err-col?
              (cond
               ((eq (point) (line-end-position))) ;do nothing
               ((looking-at (concat "[ \t" c-chars "]"))
                (kill-line))      ;delete other marks
               (t                 ;no other choices. place is cccupied
                (end-of-line)
                (insert " ")))    ;separate word
              (insert (concat comment " invalid"))))
          (forward-line 1))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-set-face-non-files-buffer  ()
  "Change face to 'error in those lines whose first word is not valid file."
  (interactive)
  (tinydesk-set-face-non-files-region (point-min) (point-max))
  (if (interactive-p)
      (message "TinyDesk: marked non-lodable files")))

;;; ----------------------------------------------------------------------
;;; - This function is not general.
;;; #todo: rewrite it for this module only
;;;
(defun tinydesk-mark-region (beg end &optional com-re sub-level verb)
  "Make all filenames in the buffer loadable by mouse.
Supposes that the first _word_ on the line is filename.
If the first word isn't loadable file, its face isn't changed.
If there is no directory part, then `default-directory' is supposed.

Input:

  BEG END   region

  COM-RE    the file can have comments, but comments can be only
            _single span type_, that is, only shell like #, or C++
            like //. Everything after and included COM-RE is discarded
            from SUB-LEVEL.

  SUB-LEVEL subexpression match; default is 0.

  VERB      verbose messages.

Example:

  Myfile.sh   #comment

  com-re     = '.*\\\\(#\\\\)'
  sub-level  = 1 , because there is paren"
  (let ((err-col       tinydesk--message-column)
	(file-face     (tinydesk-face 'file-pick))
	(sub-level     (or sub-level 0))
	(c-chars       tinydesk--comment-characters)
	(comment       (tinydesk-comment))
	bp ep				;points
	file)
    (and verb                           ;this make take a while...
         (message "TinyDesk: Marking files..."))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
	(cond
	 ((looking-at
	   (concat "[ \t]*\\(.+[^ \t\r\n]\\)\\([ \t]*"
		   "[" tinydesk--comment-characters "]"
		   ".*\\)"))
	   (setq file (match-string-no-properties 1)
		 bp (match-beginning 1)
		 ep (match-end 1))
	   (delete-region (match-beginning 2)
			  (line-end-position)))
	  ((looking-at "[ \t]*\\(.*[^ \t\r\n]\\)")
	   (setq file (match-string-no-properties 1)
		 bp (match-beginning 1)
		 ep (match-end 1)))
	 (when file
	   (cond
	    ((get-file-buffer file)  ;already in Emacs ?
	     (move-to-column err-col t)
	     (if (not (looking-at (concat "$\\|[ \t" c-chars "]")))
		 (end-of-line)) ;no other choices, place is cccupied
	     (tinydesk-add-space-if-non-space)
	     (insert (concat comment " loaded")))
	    (t
	     ;; ............................... not loaded in Emacs ...
	     (if (or (string-match "@" file)
		     (file-exists-p file))
		 (put-text-property bp ep 'mouse-face file-face)))))
	(forward-line 1))
      (set-buffer-modified-p nil)
      (and verb                         ;this make take a while...
           (message "TinyDesk: Marking files...ok")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-mark-buffer-loadable (&optional verb)
  "Parse whole buffer and make first _word_ loadable with mouse. VERB.
Marking is only done if word is valid filename."
  (interactive)
  (save-excursion
    (tinydesk-mark-region
     (point-min)
     (point-max)
     (tinydesk-comment-re)
     tinydesk--comment-start-level
     (or (interactive-p)
         verb))))

;;; ........................................................... &mouse ...

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-mouse-load-file (event)
  "Load file under mouse. Use mouse EVENT."
  (interactive "e")
  (mouse-set-point event)               ;move point there
  (tinydesk-load-file))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-remove-file-coments  ()
  "Remove all comment at `tinydesk--message-column'."
  (interactive)
  (ti::save-line-column-macro nil nil
    (ti::pmin)
    (ti::buffer-remove-whitespace-eol)
    (ti::pmin)
    (while (re-search-forward (tinydesk-comment) nil t)
      (if (eq (current-column) (+ 2 tinydesk--message-column))
          (delete-region (- (point) 2) (line-end-position))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-expunge-unloaded  ()
  "Remove lines that have 'unloaded' flag."
  (interactive)
  (ti::save-line-column-macro nil nil
    (ti::pmin)
    (while (re-search-forward (format "%s unloaded$" (tinydesk-comment)) nil t)
      (ti::buffer-kill-line))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-unload-current-file  ()
  "Remove file on this line from Emacs."
  (interactive)
  (let ((file (tinydesk-file-name-absolute
	       (tinydesk-read-word)))
	buffer)
    (when file
      (if (setq buffer (find-buffer-visiting (expand-file-name file)))
          (kill-buffer buffer)
        (message "TinyDesk: No such buffer."))
      (beginning-of-line)
      (or (and (re-search-forward (tinydesk-comment) nil t)
               (progn (delete-region (point) (line-end-position)) t))
          (and (move-to-column tinydesk--message-column t)
               (insert ";;")))

      (insert " unloaded")
      (forward-line 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-load-file ()
  "Load file under point."
  (interactive)
  (let (prop
	word)
    (setq prop (get-text-property (point) 'mouse-face))
    (cond
     (prop                              ;property found?
      (setq word  (tinydesk-file-name-absolute
                   (tinydesk-read-word))) ;read word under cursor
      (cond
       (word                            ;grabbed
        (tinydesk-handle-text-property prop word))))
     ((interactive-p)
      (message
       (substitute-command-keys
        (concat
         "TinyDesk: Can't find mouse face...   Mark buffer first with "
         "\\[tinydesk-mark-buffer-loadable]")))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-unload (file &optional verb)
  "Unload all files from Emacs that are in state file FILE.

If VERB is non-nil offer verbose messages [for code calls]; interactive
call always turns on verbose."

  (interactive
   (list
    (let ((save-dir    (or (tinydesk-get-save-dir) "~/"))
	  (msg         (concat "Unload from state file: ")))
      (read-file-name msg  save-dir))))
  (let ((b      (tinydesk-tmp-buffer))
	(dlist  (tinydesk-dired-table))
	(count  0)
	(total  0)
	buffer
	elt
	fn)
    (ti::verb)
    (with-current-buffer b
      (erase-buffer)
      (insert-file-contents file)
      ;;  - Now process every line. We don't care if we read commented
      ;;    line as "buffer" because the test inside loop return nil
      ;;    for such lines...
      (ti::pmin)
      (if (eobp)
          (if verb
              (message "TinyDesk: Empty state file."))
        (while (not (eobp))
          (beginning-of-line)
          ;;  - Expect Win32 or Unix absolute path name in line
          ;;  - find-buffer-visiting function can find files that
          ;;    may be symlinks.

          (when (and (looking-at "[a-zA-Z]:/[^ \t\n\r]+\\|[~/][^ \t\n\r]+")
                     (setq fn (match-string 0))
                     (or (and (file-directory-p fn)
                              (setq elt (assoc (expand-file-name fn) dlist))
                              (setq buffer (nth 1 elt)))
                         (setq buffer (find-buffer-visiting fn))))
            (with-current-buffer buffer
              (cond
               ((not (buffer-modified-p))
                (kill-buffer (current-buffer))
                (incf count))
               (t
                (message "Tinydesk: Buffer %s modified. Won't unload."
                         (buffer-name)))))
            (incf  total))
          (forward-line))))
    (when verb
      (if (> count 0)
          (message (format "TinyDesk: Removed %d/%d files. %s"
                           count
                           total
                           (if (equal count total)
                               ""
                             " Modified buffer not unloaded.")))
        (message "TinyDesk: No files removed.")))
    (tinydesk-trash)))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-mode (&optional no-face verb)
  "Mark and parse buffer's fist words as loada files.
If NO-FACE is non-nil, the default mouse marking isn't performed. VERB.

Comments in the right tell what is the files status:
loaded      = file inside Emacs already
invalid     = the path is invalid, no such file exists

Mode description:

\\{tinydesk-mode-map}"
  (interactive "P")
  (ti::verb)
  ;; - If the file is already in buffer, remove extra marks, like
  ;;   non-loadable files.
  (tinydesk-clear-region-properties (point-min) (point-max))
  (tinydesk-remove-file-coments)
  (if (null no-face)
      (tinydesk-mark-buffer-loadable verb))
  (tinydesk-mode-map-activate)          ;turn on the map
  (setq  mode-name   tinydesk--mode-name)
  (setq  major-mode 'tinydesk-mode) ;; for C-h m
  (when verb
    (message
     (substitute-command-keys
      (concat
       "load \\[tinydesk-load-file] "
       "clear \\[tinydesk-clear-buffer-properties] "
       "error \\[tinydesk-set-face-non-files-buffer] "
       "mark \\[tinydesk-mark-buffer-loadable]")))
    (sleep-for 1))
  (run-hooks 'tinydesk--mode-hook))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinydesk-mode ()
  "Turn on `tinydesk-mode'."
  (interactive)
  (tinydesk-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-file-p ()
  "Return t if `tinydesk--save-title' at the beginning of buffer."
  (let ((string (substring
		 (or (eval tinydesk--save-title)
		     "####No-string-available###")
		 0 40)))
    (save-excursion
      (ti::pmin)
      (when (re-search-forward
             (concat "^" (regexp-quote string))
	     (min 300
		  (point-max))
	     'noerr)
        (turn-on-tinydesk-mode)))))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinydesk-mode-maybe ()
  "Turn on `tinydesk-mode' if `tinydesk-file-p' returns non-nil."
  (interactive)
  (if (tinydesk-file-p)
      (turn-on-tinydesk-mode)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-off-tinydesk-mode ()
  "Turn off `tinydesk-mode'."
  (interactive)
  (if (functionp major-mode)
      (funcall major-mode)
    (fundamental-mode)))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-edit-state-file (file)
  "Load state FILE into buffer for editing.
You can add comments and remove/add files. Turns on `tinydesk-mode'.

Following commands are available in `tinydesk-mode'.
\\{tinydesk-mode-map}"
  ;;  I can't use interactive "f" , beacuse I want the completions
  ;;  to come from the save-directory. The "f" uses by default the
  ;;  variable default-directory...
  (interactive
   (list
    (let ((save-dir    (or (tinydesk-get-save-dir)
			   "./"))
	  (msg (concat "Edit state file: ")))
      (read-file-name msg  save-dir))))
  ;; If file is already loaded, avoid creating duplicate window
  (pop-to-buffer (find-file-noselect file))
  (tinydesk-mode nil t))

;;; ............................................................ &save ...

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-get-save-files (&optional dirs)
  "Return list of files to save, optionally DIRS too."
  (let ( ;;  See function tinydesk-dired-table for explanation
        (sym  'dired-directory)
        tmp
        list)
    (dolist (elt (buffer-list))
      (with-current-buffer elt
        (cond
         ((and dirs
               (eq major-mode 'dired-mode))
          (push (symbol-value sym) list))
         ((setq tmp (buffer-file-name))
          (push tmp list)))))
    list))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-save-state (file &optional mode files verb)
  "Output all files in Emacs into FILE.
Notice, that this concerns only buffers with filenames.

Input:

  FILE          the STATE file being saved

  MODE          Control what is saved:
                 nil    only filenames
                 '(4)   \\[universal-argument], filenames and directories.
                 '(16)  \\[universal-argument] \\[universal-argument], Use absolute paths to HOME.

  FILES         filenames , absolute ones. If nil then
                `tinydesk--get-save-file-function' is run to get files.
  VERB          verbose flag"
  (interactive
   (list (read-file-name "Save state to: " (tinydesk-get-save-dir))
         current-prefix-arg))
  (let ((tmp-buffer    (tinydesk-tmp-buffer 'clear))
	(save-func     tinydesk--get-save-file-function)
	(sort          tinydesk--save-and-sort)
	(title         tinydesk--save-title)
	(re-no         tinydesk--save-exclude-regexp)
	(absolute-p    (equal mode '(16)))
	buffer)
    (ti::verb)
    (setq tinydesk--directory-last (file-name-directory file))
    ;;  #todo: Kill buffer if it is not modified and reload it
    ;;  after save
    (when (setq buffer (get-file-buffer file))
      (pop-to-buffer buffer)
      (error "\
TinyDesk: State saving aborted. Please save to new file or kill buffer: %s" file ))
    (run-hooks 'tinydesk--save-before-hook)
    (or files
        (setq files (and (fboundp save-func)
                         (funcall save-func mode))))
    (if (null files)
        (if verb                        ;no files
            (message "TinyDesk: no items to save"))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . do save . .
      (if (or  (null file)
               (and (file-exists-p file)
                    (null (file-name-directory file))))
          (error (format  "TinyDesk: access problem with: '%s'" file)))
      ;;  We kill this buffer later, so we don't need save-excursion
      (set-buffer tmp-buffer)
      (display-buffer tmp-buffer)
      ;; ... ... ... ... ... ... ... ... ... ... ... ...  insert files . .
      (dolist (elt files)
        ;;  Remove some files...
        (if (or (not (stringp re-no))
                (and (stringp re-no)
                     (not
                      (ti::string-match-case re-no elt))))
            ;;  win32 needs complete path name, not just ~/path/...
            (insert
             (if absolute-p
                 ;;  Don't try to expand ange-ftp filenames. It would
                 ;;  cause a ftp connections to be opened and that's slow....
                 (unless (ti::file-name-remote-p elt)
                   (expand-file-name elt))
               (abbreviate-file-name elt))
             "\n")))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  sort . .
      (if sort
          (sort-lines nil (point-min) (point-max)))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... title . .
      (when title
        (ti::pmin)
        (insert (eval title)))
      (run-hooks 'tinydesk--save-after-hook)
      (write-region (point-min) (point-max) file)
      (set-buffer-modified-p nil)
      (kill-buffer tmp-buffer)
      (if (interactive-p)
          (message (concat "TinyDesk: State saved to file " file)))
      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch ^^^
      nil)
    (tinydesk-trash)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-rename-buffer-maybe ()
  "Rename buffer it FILENAME-DIR if there is <N> in the buffer name.
If two or more of the files are loaded into Emacs with the same name
from different directories:

  ~/tmp/file.txt         => buffer file.txt
  ~/txt/file.txt         => buffer file.txt<1>
  ~/abc/file.txt         => buffer file.txt<2>
  ..

This function will change the buffer names to include previous
directory part, instead of the <N>, so that the names would read:

  file.txt
  file.txt-txt
  file.txt-abc"
  (interactive)
  (when (and (string-match "<[0-9]+>$" (buffer-name))
             (buffer-file-name))
    (let* ((dir  (file-name-directory    (buffer-file-name)))
           (file (file-name-nondirectory (buffer-file-name)))
           (dir1 (replace-regexp-in-string
                  "/" "-"
                  (or (ti::string-match
                       (concat
                        ;;  Get Two levels up
                        ".*\\([\\/][^\\/]+[\\/][^\\/]+\\)\\|"
                        ;;  Or one level if only one directory
                        ".*\\([\\/][^\\/]+\\)")
                       1
                       dir)
                      ""))))
      (rename-buffer (format "%s-%s" file dir1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-find-file (file)
  "Load FILE or `recover-file' as needed. Rename buffer if buffer<2>."
  (with-current-buffer (find-file-noselect file)
    (when (and (null (buffer-modified-p))
               (file-exists-p (make-auto-save-file-name)))
      ;; Can't use (recover-file file), because it asks confirmation.
      ;; Emacs should have flag for suppressing questions.
      (erase-buffer)
      (insert-file-contents-literally (make-auto-save-file-name))
      (set-buffer-modified-p t)         ;Not strictly needed...
      (message "TinyDesk: Recovered file %s"
               (make-auto-save-file-name))
      (tinydesk-rename-buffer-maybe)
      ;;  Return value
      (current-buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-recover-state (file &optional ulp pop verb)
  "Load all files listed in FILE into Emacs.
FILE can have empty lines or comments. No spaces allowed at the
beginning of filename. The state FILE itself is not left inside
Emacs if everything loads well. When all files are already
in Emacs, you may see message '0 files loaded'.

In case there were problems, the FILE will be shown and the
problematic lines are highlighted.

Prefix arg sets flag ULP, unload previous.

Input:

  FILE          state file to load

  ULP           'unload previous' if non-nil then unload previously
                loaded files according to `tinydesk--last-state-file'

  POP           if non-nil, then show (pop to) first buffer in saved
                state file. This flag is set to t in interactive calls.

  VERB          non-nil enables verbose messages. This flag is set to
                t in interactive calls.

References:

  `tinydesk--last-state-file'       Name of state file that was loaded.
  `tinydesk--recover-before-hook'   Hook to run before state file processing.
  `tinydesk--recover-after-hook'    Hook to run after state file processing."
  (interactive
   (list
    (read-file-name "Tinydesk: load state file: "
                    (tinydesk-get-save-dir))
    current-prefix-arg
    t))
  (let ((count         0)
	(state-file    (expand-file-name file))
	(last-state    tinydesk--last-state-file)
	buffer
	kill-buffer
	err
	not-loaded
	;; first-entry
	list)
    (ti::verb)
    ;; o  read the config file
    ;; o  raise the kill flag if the file ISN'T already loaded, user
    ;;    may be editing it.
    ;; o  There may be buffers with the same name, but different dirs..
    (unless (setq buffer (get-file-buffer state-file))
      (setq kill-buffer t)              ;different directory
      (unless (file-exists-p state-file)
        (error "TinyDesk: file does not exist %s" state-file))
      (setq buffer (find-file-noselect state-file)))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... unload previous ...
    (if (and ulp (stringp last-state))
        (if (not (file-exists-p last-state))
            (message
             (format "TinyDesk: Cannot unload, file does not exist '%s' "
                     last-state))
          (tinydesk-unload last-state)))
    (with-current-buffer (ti::temp-buffer buffer)
      (setq  list           (tinydesk-find-file-whole-buffer) ;; before hook
             count          (nth 0 list)
             err            (nth 1 list)
             ;; first-entry    (nth 3 list)
             not-loaded     (nth 2 list))
      (cond
       ((null err)
        (if verb (message (format "TinyDesk: %d files loaded" count)))
        (run-hooks 'tinydesk--recover-after-hook)
        ;;  kill the buffer only if it was loaded by us
        (and kill-buffer
             (kill-buffer buffer)))
       (t
        ;;  Show failed files
        (message (concat "TinyDesk: Not loaded> " not-loaded))
        (sleep-for 0)
        (pop-to-buffer buffer)
        (tinydesk-mode 'no-face 'verbosee)
        (tinydesk-set-face-non-files-buffer)
        (ti::pmin)))
      (setq tinydesk--last-state-file file))))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydesk-recover-last-state ()
  "If Emacs was closed / crashed, recover last saved session.
References:
  `tinydesk--auto-save-interval'
  `tinydesk--auto-save-name-function'"
  (let ((file (tinydesk-auto-save-file-name)))
    (if file
        (tinydesk-recover-state file)
      (message (concat
                "TinyDesk: [WARN] Couldn't recover *last* state file."
                "function `tinydesk-auto-save-file-name' returned nil")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-find-file-whole-buffer (&optional recover verb)
  "Load all files listed in buffer. Point is not preserved.

Input:

  RECOVER   Flag. If non-nil, use `recover-file' instead of `find-file'.
  VERB      Verbose flag.

References:

  `tinydesk--loaded-file-list'
  `tinydesk--rejected-file-list'
  `tinydesk--recover-before-hook'   Hook to run before state file processing.

Return:

   '(count err not-loaded-string first-entry)

   count                how many files actually loaded
   err                  error while load
   not-loaded-string    files that had problems.
   first-entry          first entry"
  (interactive "P")
  (let ((count         0)
	(sub-level     tinydesk--comment-start-level)
	(ignore-re     (tinydesk-comment-re))
	(empty-re      "^[ \t]*$")
	(msg-str       (if recover
			   "Recovering"
			 "Loading"))
	first-entry
	bp
	not-loaded
	load                       ;file ont the line to be processed
	maxp                           ;max point
	word
	err                            ;per file basis
	ERR)                           ;return status
    (ti::verb)
    (setq tinydesk--loaded-file-list   nil ;<< reset GLOBALS
          tinydesk--rejected-file-list nil)
    (run-hooks 'tinydesk--recover-before-hook)
    (ti::pmin)                          ;there is *no* save excursion
    (while (not (eobp))
      (setq bp (point)  err nil)        ;BEG of line
      (setq maxp (line-end-position))
      (beginning-of-line)
      (catch 'next
        ;; ... ... ... ... ... ... ... ... ... ... ... ... .. comments ...
        (if (looking-at empty-re)       ;emty lines
            (throw 'next t))
        (when (and (looking-at ignore-re)
                   (match-beginning sub-level))
          (setq maxp  (match-beginning sub-level)))
        (if (eq maxp bp)                ;full comment line ?
            (throw 'next t))
        ;; ... ... ... ... ... ... ... ... ... ... ...  read file name ...
        ;;  Now load the file, raise error if not loaded
        ;;  Remember that Windows fiels may contain spaces c:\Program Files\
        (setq word (tinydesk-read-word))
        ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
        (when word
          (setq load (expand-file-name word))
          (when (or recover
                    (or load            ;file grabbed from line
                        (not (get-file-buffer load)))) ;already in Emacs
            (if (not (ti::file-find-file-p load))
                (setq err t)
              (when (condition-case nil
                        (progn
                          (if verb
                              (message "TinyDesk: %s %s..." msg-str load))
                          (tinydesk-find-file load)
                          t)
                      (error
                       (setq err t)
                       nil))
                (setq count (1+ count))
                (if (null first-entry)
                    (setq first-entry word))
                (ti::nconc tinydesk--loaded-file-list word)))))
        ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
        (when err
          (setq ERR t)
          (push word tinydesk--rejected-file-list)
          (and (interactive-p)
               (tinydesk-line-property-set-error))
          (setq  not-loaded
                 (concat
                  (or not-loaded "")    ;start value
                  (or
                   (file-name-nondirectory load)
                   "[nil-word]")
                  " "))))               ;catch line
      (forward-line 1))
    (if verb
        (message "TinyDesk: %s...done" msg-str))
    (list count ERR not-loaded first-entry)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-recover-file-whole-buffer (&optional verb)
  "Call `tinydesk-find-file' with argument `recover'. VERB."
  (interactive)
  (save-excursion
    (tinydesk-find-file-whole-buffer 'recover (ti::verb))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydesk-install (&optional uninstall)
  "Install or UNINSTALL package."
  (interactive "p")
  (unless tinydesk-mode-map
    (setq tinydesk-mode-map (make-sparse-keymap))
    (run-hooks 'tinydesk--mode-define-keys-hook))
  (cond
   (uninstall
    (remove-hook 'write-file-functions 'tinydesk-auto-save)
    (remove-hook 'find-file-hook 'turn-on-tinydesk-mode-maybe))
   (t
    (add-hook 'write-file-functions 'tinydesk-auto-save)
    (add-hook 'find-file-hook 'turn-on-tinydesk-mode-maybe))))

(tinydesk-install)

(provide   'tinydesk)
(run-hooks 'tinydesk--load-hook)

;;; tinydesk.el ends here
