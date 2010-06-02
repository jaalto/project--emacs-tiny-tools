;;; tinydesk.el --- Save and restore files between Emacs sessions

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinydesk-version.
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
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   $HOME/.emacs startup file
;;
;;      ;; (add-hook 'tinydesk-:load-hook 'tinydesk-recover-last-state)
;;      (add-hook 'tinydesk-:load-hook 'tinydesk-install-default-keybindings)
;;      (require 'tinydesk)
;;
;;   or use the autoload feature. Notice that the automatic "file
;;   state backup feature" gets enables only when this file is loaded.
;;   If you want that feature, then use require.
;;
;;      (add-hook 'tinydesk-:load-hook 'tinydesk-default-emacs-keybindings)
;;      (add-hook 'tinydesk-:load-hook 'tinydesk-recover-last-state)
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
;;
;;   If you have any questions, use this function:
;;
;;      M-x tinydesk-submit-bug-report
;;
;;  To read the documentation after file has been loaded, call
;;
;;      M-x tinydesk-version

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, feb 1995
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
;;  Automatic one time session saver
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
;;      session on Emacs startup.
;;
;;          (defconst tinydesk-:directory-location "~/elisp/config")
;;
;;          (defconst my-tinydesk-session
;;            (concat tinydesk-:directory-location "/state.last-session"))
;;
;;          (add-hook 'kill-emacs-hook 'my-save-session)
;;
;;          (defun my-save-session ()
;;            "Save loaded files to state file."
;;            ;;  if you want to save dired buffers too.
;;            ;;  use (tinydesk-save-state my-tinydesk-session '(4))
;;            (tinydesk-save-state my-tinydesk-session) nil)
;;
;;          (if (and (file-exists-p my-tinydesk-session)
;;                   (y-or-n-p "Recover session "))
;;              (tinydesk-recover-state my-tinydesk-session))
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
;;      `tinydesk-auto-save' function to `write-file-hooks' and in regular
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

;;}}}

;;; Change Log:

;;; Code:

(require

(eval-when-compile

(ti::package-defgroup-tiny
  "Simple

            Unlike
            from
            state
            your
            between
            be

            Files
    ")

;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom
  '(tinydesk-default-mode-bindings)
  "*List
  :type
  :group

(defcustom
  "*Hook
  :type
  :group

(defcustom
  "*Hook
begins.
  :type
  :group

(defcustom
  "*Hook
The
  :type
  :group

(defcustom
  "*Hook
  :type
  :group

(defcustom
  "*Hook
  :type
  :group

(defcustom
  "*Hook
  :type
  :group

;;}}}
;;{{{ setup: user config

;;; ................................................... &v-user-config ...

(defcustom
  "*A
The
  :type
  :group

(defcustom
  "*Editing
  :type
  :group

(defcustom
  (or
   (file-name-as-directory
    (file-name-directory
   (and
        "~/tmp")
   (error
TinyDesk:
  "*Default
  :type
  :group

(defcustom
  "*How
'last
'default
  :type
           (const
           (const
  :group

(defcustom
  "*Interval
If

The
it's

See
function
  :type
  :group

(defcustom
  "*Function
If
first

For
  :type
  :group

(defcustom
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
  "*Regexp
match
have

   (setq
   (defun
      (flush-lines

  :type
  :group

(defcustom
  '(progn
     (format
;; Emacs tinydesk.el state file
;;
;;
;;       Date: %s
;;       M-x load-library RET tinydesk RET
;;       M-x tinydesk-version RET   ;; To read manual
;;       M-x tinydesk-recover-state RET %s RET

"
      (ti::date-standard-date
      (if
          file
        "<file>")))
  "*A
  :type
  :group

(defcustom
  "*Non-nil
nil
  :type
  :group

;;  Set to nil if you don't want title.

(defcustom
  "*Function
This
run

Arguments
  mode

  :type
  :group

(defcustom
  '((file-pick
    (error
  "*Alist
The

\(setq
  '((file-pick
    (error
  :type
          (cons
           (const
           (symbol
          (cons
           (const
           (symbol
  :group

;;}}}
;;{{{ setup: -- private

;;; ....................................................... &v-private ...

(defvar
  "Local

(defvar
  "Directory

(defvar
  "The

(defvar
  "If

(defvar
  "Column

(defvar
  "Counter
See.

(defvar
  "Overwritten
Contain
Hooks

(defvar
  "Overwritten
Contain
`tinydesk-find-file-whole-buffer'.
path,

(defconst
  "Which

(defvar
  "Last

;;}}}
;;{{{ setup: -- version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinydesk-version "tinydesk" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinydesk.el"
   "tinydesk"
   tinydesk-:version-id
   "$Id:
   '(tinydesk-version-id
     tinydesk-:mode-define-keys-hook
     tinydesk-mode-map
     tinydesk-:load-hook
     tinydesk-:save-before-hook
     tinydesk-:save-after-hook
     tinydesk-:mode-hook
     tinydesk-:recover-before-hook
     tinydesk-:recover-after-hook
     tinydesk-:directory-last
     tinydesk-:tmp-buffer
     tinydesk-:trash-tmp-buffer
     tinydesk-:message-column
     tinydesk-:loaded-file-list
     tinydesk-:rejected-file-list
     tinydesk-:comment-start-level
     tinydesk-:mode-name
     tinydesk-:directory-save-suggested
     tinydesk-:save-exclude-regexp
     tinydesk-:comment-characters
     tinydesk-:save-title
     tinydesk-:save-and-sort
     tinydesk-:face-table)))

;;}}}
;;{{{ Install: bindings

;;; ----------------------------------------------------------------------
;;;
(defun
  "Install
\\{ctl-x-4-map}"
  (interactive)
  (define-key
  ;;  This was find-file-read-only-other-window
  (define-key
  (define-key
  (define-key

;;; ----------------------------------------------------------------------
;;;
(defun
  "Define
  (when
    ;;  - Don't want to use mouse-2 because it's for PASTE.
    ;;  - The others are put to mouse-2 because there is not
    ;;    not always 3 button mouse available.

    (define-key

    ;;  - When editing a file, those colors might be too annoyinng,
    ;;    so you can remove properties with this. Loading is disabled too
    ;;  - Remeber, Emacs is slow with this... wait some time.

    (define-key
      'tinydesk-clear-buffer-properties)

    ;;  To make buffer loadable by mouse again, run this

    (define-key
      'tinydesk-mark-buffer-loadable)

    ;;  To mark files that are not loadable, check for possibly typo in
    ;;  filename

    (define-key
      'tinydesk-set-face-non-files-buffer))

  (when

    (define-key
      'tinydesk-mouse-load-file)

    (define-key
      'tinydesk-clear-buffer-properties)

    (define-key
      'tinydesk-mark-buffer-loadable)

    (define-key
      'tinydesk-set-face-non-files-buffer))

  ;; ............................................. users with no mouse ...

  (define-key

  (define-key
  (define-key
  (define-key
  (define-key
  (define-key
  (define-key
  (define-key
  (define-key

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Return
  (make-string
   2
   (string-to-char
    (substring

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Return
  (concat
          tinydesk-:comment-characters
          "]*\\(["
          tinydesk-:comment-characters
          "].*\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Read
  ;;   Windows use spaces in file names
  (let
               (ti::string-remove-whitespace
                (ti::buffer-read-word
    ;;  Remove comments from the end
    (if
        (match-string
      word)))

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Return
  (ti::temp-buffer

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Add
  (if
      (if
          (setq
  file)

;;; ----------------------------------------------------------------------
;;;
(defsubst
  "Use
  (use-local-map

;;; ----------------------------------------------------------------------
;;;
(defun
  "Add
  (let*
    (unless
      ;;  So that comment doe snot get inserted next to the filename
      (insert

;;; ----------------------------------------------------------------------
;;;
(defun
  "Return
  (let*
         (last
         (dir
         (ret
    (if
             (stringp
             (file-writable-p
        (setq
    (or
        dir
        "~/")))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (interactive)
  (tinydesk-only-files-region

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
This
  (interactive
  (let*
         (comment-re
         (empty-re
         mark-end
         p
         maxp
         word
         tmp)
    (if
        (setq
    (save-excursion
      (goto-char
      ;;  markers has to be used, beacuse we delete lines and points move
      (setq
      (goto-char
      (while
        (setq
        (catch
          (if
              nil
            (ti::buffer-kill-line)
            (throw
          (if
              nil
            (if
                (setq
          (if
              (progn
                (ti::buffer-kill-line)
          (setq
;;;       (setq word (tinydesk-read-word p maxp))
          (ti::buffer-kill-line)
          ;; The \n make cursor forward
          (if
              (insert

;;; ----------------------------------------------------------------------
;;;
(defun
  "Kill

References:
  `tinydesk-:tmp-buffer'
  `tinydesk-:trash-tmp-buffer'"
  (and
       (get-buffer
       (kill-buffer

;;; ----------------------------------------------------------------------
;;;
(defun
  "Return
  (interactive)
  (let
        ;;  ByteCompiler doesn't know that I do
        ;;  (eq major-mode 'dired-mode) test before I use this variable,
        ;;  so hide it from it.
        ;;
        ;;  The variable is defined if it passed the eq test
        (sym
        list)
    (dolist
      (with-current-buffer
        (if
            (push
             (list
                   (current-buffer))
             list))))
    list))

;;}}}
;;{{{ code: auto save

;;; ----------------------------------------------------------------------
;;;
(defun
  "Return
References:
  `tinydesk-:directory-location'
  `tinydesk-:auto-save-name-function'."
  (let*
         (dir
         (name
                              (nth
                         "periodic"))
         (fn
                      (file-name-as-directory
                      "emacs-config-tinydesk-autosave-"
                      (or
         (save-to
    (if
        (setq
    save-to))

;;; ----------------------------------------------------------------------
;;;
(defun
  "This

Input:

  FORCE

Every
is

The

o
o
o
   you
   run
o

   Possible

References:

  `tinydesk-:auto-save-counter'
  `tinydesk-:auto-save-interval'
  (interactive
  (let*
         (save-to
    (when
      ;;  - Be extra careful, because we're in write file hook
      ;;  - Make sure we always succeed
      (if
          (setq
      (if
          (setq
      (if
          (setq
      (incf
      ;;  time's up? Select name if it's string.
      (cond
       ((or
            (>
        ;;   Actually tinydesk-save-state generates new call to this
        ;;   function but, it won't come in this COND, because the counter
        ;;   value is different.
        (setq
        ;; Try chmod, if it fails, then signal error
        (if
                 (not
            (set-file-modes
             save-to
             (ti::file-mode-make-writable
        ;;  Still no luck after chmod ?
        (if
                (and
                     (not
            (error
TinyDesk:

        (save-window-excursion
          (save-excursion
            (message
            (tinydesk-save-state
      ;; `write-file-hook' function must return nil
      nil)))

;;}}}

;;{{{ Code: faces

;;; ----------------------------------------------------------------------
;;;
(defun
  "Return
  ;;  This way the global variable does not float around the file
  (cdr

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (set-text-properties
  (set-buffer-modified-p

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (interactive)
  (let*
         (c-lev
         (c-re
         beg)
    (tinydesk-clear-region-properties
    (save-excursion
      (ti::pmin)
      (while
        ;;  Skip over BOL comments
        (when
                   (looking-at
                   (setq
          (goto-char
          (skip-chars-backward
          (delete-region
        (forward-line
    (set-buffer-modified-p
    (if
        (message

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (set-text-properties
  (set-buffer-modified-p

;;; ----------------------------------------------------------------------
;;;
(defun
  "Set
  (let*
         end)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward
    ;; clear first full line
    (put-text-property
    (put-text-property
    (set-buffer-modified-p

;;; ----------------------------------------------------------------------
;;;
(defun
  "Look
  (let*
         (loaded
         (comment
         (err-col
    (cond
     ((eq
      (cond
       ((null
        (message
        (tinydesk-clear-line-properties))
       (loaded
        (message
        (tinydesk-clear-line-properties))
       (t
        (find-file-noselect
        (message
        (tinydesk-clear-line-properties)
        (move-to-column
        (unless
          (end-of-line))
        (tinydesk-add-space-if-non-space)
        (insert
        (beginning-of-line)))))))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Parse
Marking
  (interactive)
  (save-excursion
    (tinydesk-mark-region
     (point-min)
     (point-max)
     (tinydesk-comment-re)
     tinydesk-:comment-start-level
     (or
         verb))))

;;; ----------------------------------------------------------------------
;;;
(defun
  "In
Also
  (interactive
  (let*
         (sub-level
         (c-chars
         (comment
         (comment-re
         (err-col
         word)
    (save-excursion
      (save-restriction
        (narrow-to-region
        (ti::pmin)
        (while
          ;;  - ignore empty lines and BEG of line comments.
          (if
                  (and
                       (eq
              nil
            (setq
            (if
                nil
 ;;         (ti::d! word)
              (tinydesk-line-property-set-error)
              ;;  Show to user that does not see the color
              (move-to-column
              ;; Is the filename that long, that it goes past err-col?
              (cond
               ((eq
               ((looking-at
                (kill-line))
               (t
                (end-of-line)
                (insert
              (insert
          (forward-line

;;; ----------------------------------------------------------------------
;;;
(defun
  "Change
  (interactive)
  (tinydesk-set-face-non-files-region
  (if
      (message

;;; ----------------------------------------------------------------------
;;; - This function is not general.
;;; #todo: rewrite it for this module only
;;;
(defun
  "Make
Supposes
If
If

Input:

  BEG

  COM-RE
            _single
            like
            from

  SUB-LEVEL

  VERB

Example:

  Myfile.sh

  com-re
  sub-level
  (let*
         (file-face
         (sub-level
         (c-chars
         (comment
         bp
         elp
         maxlp
         file)
    (and
         (message
    (save-restriction
      (narrow-to-region
      (goto-char
      (while
        (if
            nil
          (setq
          (setq
          ;;  Does there exist comment on the line ?
          (save-excursion
            (when
                       (looking-at
              (setq
          (if
              (progn
;;;             (ti::d! "skipped" (ti::read-current-line))
                nil)
            (skip-syntax-forward
            (setq

            (skip-chars-forward
            (if
                (goto-char
            (setq
            (if
                nil
              ;;  Mark the word only if the WORD is valid file
              ;;  - If the filename has ange-ftp @ char, then mark
              ;;    automatically. Calling file-exists-p for ange
              ;;    file would start ange-ftp... and we don't
              ;;    want that here.
              (setq
              (setq
;;;           (ti::d! bp ep (point) file )
              (goto-char
              (if
                  (delete-region
              (cond
               ((get-file-buffer
                (move-to-column
                (if
                    (end-of-line))
                (tinydesk-add-space-if-non-space)
                (insert
               (t
                ;; ............................... not loaded in Emacs ...
                (if
                        (file-exists-p
                    (put-text-property
        (forward-line
      (set-buffer-modified-p
      (and
           (message

;;}}}
;;{{{ code: mouse

;;; ........................................................... &mouse ...

;;; ----------------------------------------------------------------------
;;;
(defun
  "Load
  (interactive
  (mouse-set-point
  (tinydesk-load-file))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (interactive)
  (ti::save-line-column-macro
    (ti::pmin)
    (while
      (if
          (delete-region

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (interactive)
  (ti::save-line-column-macro
    (ti::pmin)
    (while
      (ti::buffer-kill-line))))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Remove
  (interactive)
  (let*
                (tinydesk-read-word)))
         buffer)
    (when
      (if
          (kill-buffer
        (message
      (beginning-of-line)
      (or
               (progn
          (and
               (insert

      (insert
      (forward-line

;;; ----------------------------------------------------------------------
;;;
(defun
  "Load
  (interactive)
  (let*
         word)
    (setq
    (cond
     (prop
      (setq
                   (tinydesk-read-word)))
      (cond
       (word
        (tinydesk-handle-text-property
     ((interactive-p)
      (message
       (substitute-command-keys
        (concat
         "TinyDesk:
         "\\[tinydesk-mark-buffer-loadable]")))))))
;;}}}
;;{{{ code: edit, unload

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "Unload

If
call

  (interactive
   (list
    (let*
           (msg
      (read-file-name
  (let*
         (dlist
         (count
         (total
         buffer
         elt
         fn)
    (ti::verb)
    (with-current-buffer
      (erase-buffer)
      (insert-file-contents
      ;;  - Now process every line. We don't care if we read commented
      ;;    line as "buffer" because the test inside loop return nil
      ;;    for such lines...
      (ti::pmin)
      (if
          (if
              (message
        (while
          (beginning-of-line)
          ;;  - Expect Win32 or Unix absolute path name in line
          ;;  - find-buffer-visiting function can find files that
          ;;    may be symlinks.

          (when
                     (setq
                     (or
                              (setq
                              (setq
                         (setq
            (with-current-buffer
              (cond
               ((not
                (kill-buffer
                (incf
               (t
                (message
                         (buffer-name)))))
            (incf
          (forward-line))))
    (when
      (if
          (message
                           count
                           total
                           (if
                               ""
                             "
        (message
    (tinydesk-trash)))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "Mark
If

Comments
loaded
invalid

Mode

\\{tinydesk-mode-map}"
  (interactive
  (ti::verb)
  ;; - If the file is already in buffer, remove extra marks, like
  ;;   non-loadable files.
  (tinydesk-clear-region-properties
  (tinydesk-remove-file-coments)
  (if
      (tinydesk-mark-buffer-loadable
  (tinydesk-mode-map-activate)
  (setq
  (setq
  (when
    (message
     (substitute-command-keys
      (concat
       "load
       "clear
       "error
       "mark
    (sleep-for
  (run-hooks

;;; ----------------------------------------------------------------------
;;;
(defun
  "Turn
  (interactive)
  (tinydesk-mode))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Turn
  (interactive)
  (let*
                  (or
                      "####No-string-available###")
                  0
    (save-excursion
      (ti::pmin)
      (when
             (concat
        (turn-on-tinydesk-mode)))))

;;; ----------------------------------------------------------------------
;;;
(defun
  "Turn
  (interactive)
  (if
      (funcall
    (fundamental-mode)))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "Load
You

Following
\\{tinydesk-mode-map}"
  ;;  I can't use interactive "f" , beacuse I want the completions
  ;;  to come from the save-directory. The "f" uses by default the
  ;;  variable default-directory...
  (interactive
   (list
    (let*
           (save-dir
                            save-dir
                          "./"))
           (msg
      (read-file-name
  ;; If file is already loaded, avoid creating duplicate window
  (pop-to-buffer
  (tinydesk-mode

;;}}}
;;{{{ code: save

;;; ............................................................ &save ...

;;; ----------------------------------------------------------------------
;;;
(defun
  "Return
  (let
        (sym
        tmp
        list)
    (dolist
      (with-current-buffer
        (cond
         ((and
               (eq
          (push
         ((setq
          (push
    list))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "Output
Notice,

Input:

  FILE

  MODE
                 nil
                 '(4)
                 '(16)

  FILES
                `tinydesk-:get-save-file-function'
  VERB
  (interactive
   (list
         current-prefix-arg))
  (let*
         (save-func
         (sort
         (title
         (re-no
         (absolute-p
         buffer)
    (ti::verb)
    (setq
    ;;  #todo: Kill buffer if it is not modified and reload it
    ;;  after save
    (when
      (pop-to-buffer
      (error
TinyDesk:
    (run-hooks
    (or
        (setq
                         (funcall
    (if
        (if
            (message
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . do save . .
      (if
               (and
                    (null
          (error
      ;;  We kill this buffer later, so we don't need save-excursion
      (set-buffer
      ;; ... ... ... ... ... ... ... ... ... ... ... ...  insert files . .
      (dolist
        ;;  Remove some files...
        (if
                (and
                     (not
                      (ti::string-match-case
            ;;  win32 needs complete path name, not just ~/path/...
            (insert
             (if
                 ;;  Don't try to expand ange-ftp filenames. It would
                 ;;  cause a ftp connections to be opened and that's slow....
                 (unless
                   (expand-file-name
               (abbreviate-file-name
             "\n")))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  sort . .
      (if
          (sort-lines
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... title . .
      (when
        (ti::pmin)
        (insert
      (run-hooks
      (write-region
      (not-modified)
      (kill-buffer
      (if
          (message
      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch ^^^
      nil)
    (tinydesk-trash)))

;;}}}
;;{{{ code: recover

;;; ----------------------------------------------------------------------
;;;
(defun
  "Rename
If
from

  ~/tmp/file.txt
  ~/txt/file.txt
  ~/abc/file.txt
  ..

This
directory

  file.txt
  file.txt-txt
  file.txt-abc"
  (interactive)
  (when
             (buffer-file-name))
    (let*
           (file
           (dir1
                  "/"
                  (or
                       (concat
                        ;;  Get Two levels up
                        ".*\\([\\/][^\\/]+[\\/][^\\/]+\\)\\|"
                        ;;  Or one level if only one directory
                        ".*\\([\\/][^\\/]+\\)")
                       1
                       dir)
                      ""))))
      (rename-buffer

;;; ----------------------------------------------------------------------
;;;
(defun
  "Load
  (with-current-buffer
    (when
               (file-exists-p
      ;; Can't use (recover-file file), because it asks confirmation.
      ;; Emacs should have flag for suppressing questions.
      (erase-buffer)
      (insert-file-contents-literally
      (set-buffer-modified-p
      (message
               (make-auto-save-file-name))
      (tinydesk-rename-buffer-maybe)
      ;;  Return value
      (current-buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "Load
FILE
beginning
Emacs
in

In
problematic

Prefix

Input:

  FILE

  ULP
                loaded

  POP
                state

  VERB
                t

References:

  `tinydesk-:last-state-file'
  `tinydesk-:recover-before-hook'
  `tinydesk-:recover-after-hook'
  (interactive
   (list
    (read-file-name
                    (tinydesk-get-save-dir))
    current-prefix-arg
    t))
  (let*
         (state-file
         (last-state
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
    (unless
      (setq
      (unless
        (error
      (setq
    ;; ... ... ... ... ... ... ... ... ... ... ... ... unload previous ...
    (if
        (if
            (message
             (format
                     last-state))
          (tinydesk-unload
    (with-current-buffer
      (setq
             count
             err
             ;; first-entry    (nth 3 list)
             not-loaded
      (cond
       ((null
        (if
        (run-hooks
        ;;  kill the buffer only if it was loaded by us
        (and
             (kill-buffer
       (t
        ;;  Show failed files
        (message
        (sleep-for
        (pop-to-buffer
        (tinydesk-mode
        (tinydesk-set-face-non-files-buffer)
        (ti::pmin)))
      (setq

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun
  "If
References:
  `tinydesk-:auto-save-interval'
  `tinydesk-:auto-save-name-function'"
  (let
    (if
        (tinydesk-recover-state
      (message
                "TinyDesk:
                "function

;;; ----------------------------------------------------------------------
;;;
(defun
  "Call
  (interactive)
  (save-excursion
    (tinydesk-find-file-whole-buffer

;;; ----------------------------------------------------------------------
;;;
(defun
  "Load

Input:

  RECOVER
  VERB

References:

  `tinydesk-:loaded-file-list'
  `tinydesk-:rejected-file-list'
  `tinydesk-:recover-before-hook'

Return:

   '(count

   count
   err
   not-loaded-string
   first-entry
  (interactive
  (let*
         (sub-level
         (ignore-re
         (empty-re
         (msg-str
                            "Recovering"
                          "Loading"))
         first-entry
         bp
         not-loaded
         load
         maxp
         word
         err
         ERR)
    (ti::verb)
    (setq
            tinydesk-:rejected-file-list
    (run-hooks
    (ti::pmin)
    (while
      (setq
      (setq
      (beginning-of-line)
      (catch
        ;; ... ... ... ... ... ... ... ... ... ... ... ... .. comments ...
        (if
            (throw
        (when
                   (match-beginning
          (setq
        (if
            (throw
        ;; ... ... ... ... ... ... ... ... ... ... ...  read file name ...
        ;;  Now load the file, raise error if not loaded
        ;;  Remember that Windows fiels may contain spaces c:\Program Files\
        (setq
        ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
        (when
          (setq
;;;       (ti::d! "buffer?" (get-file-buffer load) (ti::file-find-file-p load) load)
          (when
                    (or
                        (not
            (if
                (setq
              (when
                        (progn
                          (if
                              (message
                          (tinydesk-find-file
                          t)
                      (error
                       (setq
                       nil))
                (setq
                (if
                    (setq
                (ti::nconc
        ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
        (when
          (setq
          (push
          (and
               (tinydesk-line-property-set-error))
          (setq
                 (concat
                  (or
                  (or
                   (file-name-nondirectory
                   "[nil-word]")
                  "
      (forward-line
    (if
        (message
;;;    (ti::d! "load-end" count ERR not-loaded)
    (list

;;}}}

;;; ----------------------------------------------------------------------
;;;
(defun
  "Install
  (interactive
  (unless
    (setq
    (run-hooks
  (cond
   (uninstall
    (remove-hook
    (remove-hook
   (t
    (add-hook
    (add-hook

(tinydesk-install)

(provide
(run-hooks

;;; tinydesk.el ends here
