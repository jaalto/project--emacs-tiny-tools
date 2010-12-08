;;; tinydired.el --- Dired enchancements. Backgroud Ange ftp support

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at code with folding.el

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
;;      ;;  Make sure the keys will be defined.
;;      (setq tinydired--force-add-keys-flag 'override)
;;
;;      (autoload 'tinydired-hook-control                   "tinydired" "" t)
;;      (autoload 'tinydired-switch-to-some-ange-ftp-buffer "tinydired" "" t)
;;      (add-hook 'tinydired--load-hook                 'tinydired-hook-control)
;;      (add-hook 'dired-mode-hook '(lambda () (require 'tinydired) nil))
;;
;;  For more personal setup, you have to look at the calls in function
;;  `tinydired-hook-control' and put your own initializations into
;;  `dired-mode-hook' and `dired-after-readin-hook'.
;;
;;  To select/kill ange buffers, use these bindings
;;
;;      (global-set-key "\C-cab" 'tinydired-switch-to-some-ange-ftp-buffer)
;;      (global-set-key "\C-cak" 'tinydired-kill-all-ange-buffers)
;;      (global-set-key "\C-caK" 'tinydired-kill-all-ange-and-dired-buffers)
;;
;;  If you don't want default keybindings, modify variable
;;
;;      tinydired--bind-hook
;;
;;  Help about keys - do this in dired buffer after you've loaded this file
;;
;;      t C-h           enchanced "tiny" dired commands
;;      a C-h           enchanced "ange-ftp" commands

;;}}}
;;{{{ Documentation
;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Jan 1996
;;

;;      This package started evolving, when there was need for something
;;      more from ange-ftp, like background file loading. Ange-ftp also had
;;      nasty habbit of putting user to just downloaded .zip or .tar.gz
;;      buffer. That not what was usually wanted, but to download the files
;;      somewhere other than inside emacs. There was need for ability to
;;      *mark* files for download and get them all at once to a download
;;      directory. With standard `ange-ftp' you would have to load them one
;;      by one. Sometimes you may want to go associated `ange-ftp' buffer
;;      and give commands directly there, so a command to switch between
;;      ange-ftp and dired buffers would be handy.
;;
;;      Now you can do this with standard `ange-ftp' and Emacs dired.
;;
;;      Note: This paskage is just extension to `ange-ftp', consider
;;      getting next generation ange-ftp, the `EFS', if you want
;;      overall better and more complete interface. Use this package if
;;      you only need features like batch put/get at backround.
;;      (Further note: EFS was later installed to XEmacs and it does not work
;;      any more with Emacs.)
;;
;;  Overview of features
;;
;;      o   Few enchancements to dired mode. Eg. keep only one
;;          dired buffer when ascending to directory. Shorten symlinks.
;;      o   User can mark and put files into STORE and start a backgroud
;;          ange-ftp session to get STORED files into download directory
;;      o   Easy switching between ange-ftp session buffer and dired buffer
;;      o   Dealing with ange ftp buffers in general
;;          (x)  killing all ange buffers at once
;;          (x)  killing all ange + dired ange buffers at once.
;;          (x)  switching to ange buffers with completion
;;      o   Run "!" on ange ftp dired buffer (operate on local copy)
;;      o   customizable backup file flagging.
;;      o   other handy dired commands, like "pop to this file in emacs."
;;          "find all marked files"...
;;
;;  XEmacs note
;;
;;      The dired and ange-ftp implementation (nowadays efs) is
;;      completely differen than in Emacs
;;
;;      ** THIS PACKAGE IS FOR Emacs ONLY **
;;
;;  General dired additions
;;
;;      In simplest form. This module installs some functions in your
;;      dired hooks. Their purpose is
;;
;;      o   To keep your dired buffer sorted so that directories are
;;          always put first.
;;      o   Delete unwanted files from dired buffer automatically.
;;      o   Shorten the symlink references, so that they don't spread
;;          multiple lines and ruin your view.
;;
;;      It also changes one dired function with `defadvice', so that you
;;      can control if you want to have only one dired buffer when
;;      ascending to another directory. See variable:
;;
;;          tinydired--use-only-one-buffer-flag
;;
;;  Dired and ange-ftp additions
;;
;;      When you want to start ftp session in emacs you just do
;;
;;          C-x C-f /login@site:/dir/dir/file
;;
;;      Let's take an example: To see what new things has arrived
;;      to GNU site, you'd do this:
;;
;;          C-x C-f /ftp@prep.ai.mit.edu:/pub/gnu/
;;
;;      After that you are put into the dired listing, where you
;;      can mark files with dired-mark command
;;
;;          m           Mark file
;;
;;      Now you have files ready. Next put files into batch STORAGE.
;;      There is "a" prefix for ange-ftp related commands.
;;
;;          a S         Big S put selected files into storage
;;          a q         To check what files you have batched
;;          a c         To clear the batch storage
;;
;;      Now start ftp'ding the files in background. You're prompted
;;      for the download directory.
;;
;;          a g         Get marked file(s)
;;
;;      If you want to operate on the associated ftp buffer
;;      directly, there is command
;;
;;          a b         For "buffer change"
;;
;;      that puts you into ftp, where the dired buffer refers. When
;;      you're in the ftp buffer you have some keybinding available.
;;
;;          C-c f       insert stored files on the line
;;          C-c d       insert directory name
;;          C-c b       back to dired window
;;
;;      It's sometimes handy that you can give direct ftp commands.
;;
;;  Setting up ange ftp
;;
;;      Here is my settings, which you can use as a reference so that you
;;      get the ange running. For more details, see the ange-ftp.el's
;;      source code. These settings include firewall "ftpgw.example.com"
;;
;;          ;; (setq ange-ftp-generate-anonymous-password t)
;;          (setq ange-ftp-dumb-unix-host-regexp  "tntpc") ;PC hosts
;;          (setq ange-ftp-gateway-host "ftpgw.example.com")
;;          (setq ange-ftp-smart-gateway t)
;;          (setq ange-ftp-local-host-regexp "\\.myhost\\.\\(com|fi\\)|^[^.]*$")
;;          ;;  Always use binary
;;          (setq ange-ftp-binary-file-name-regexp ".")
;;          (autoload 'ange-ftp-set-passwd "ange-ftp" t t)
;;          (setq ange-ftp-generate-anonymous-password "jdoe@example.com")
;;
;;  How to use this module 3 -- special vc
;;
;;      There are some extra commands that you may take a look at.
;;      See source code of bind function
;;
;;          tinydired-default-other-bindings
;;
;;      What additional commands you get when loading this module.
;;
;;      The VC special commands were programmed, because I felt that the
;;      C-x v v in dired mode didn't quite do what I wanted. I wanted
;;      simple ci/co/revert commands for files that were in VC control.
;;      And I wanted to handle them individually, expecially when ci'ing.
;;      (written for Emacs 19.28).
;;
;;      This VC part of the package is highly experimental.
;;      I'm not sure if I support it in further releases.
;;
;;  Important ange-ftp interface note
;;
;;      The ange ftp batch interface used here may cause unpredictable
;;      problems. Sometimes the `get' or `put' process doesn't start at all
;;      although you see message saying it started the job. I have had
;;      several occurrances where `lcd' cmd succeeded, but then nothing
;;      happened. Repeating the `put' or `get' command cleared the problem
;;      whatever it was.
;;
;;      So, never trust the message `completed', unless you saw that the
;;      download percentage count started running. If you're downloading
;;      important file, double check the real ftp buffer for correct response.
;;      Try again if ftp wasn't started. Another way to clear the problem: kill
;;      the ange ftp buffer and try the command from dired again. It
;;      automatically opens session to the site.
;;
;; Advertise -- other useful packages
;;
;;      There are exellent dired extensions around, please consider getting
;;      these packages:
;;
;;      o   dired-sort.el (requires date-parse.el)
;;      o   dired-tar.el
;;
;;  Note: Slow autoload
;;
;;      When you have added the autoloads into your .emacs, the first time
;;      you bring up dired buffer may be quite slow. This is normal, Emacs
;;      just need to load some additional files that this package uses.
;;
;;  Note: Refreshing the view takes long time / point isn't exatly the same
;;
;;      This is normal, dired is just slow and program has to do lot of
;;      work to maintain the "view". Eg. save view, save marks, delete
;;      marks, revert, sort, restore marks... Only the current line
;;      position is preserved where user was, not point.
;;
;;  Note: Code
;;
;;      Emacs ships with package `dired-x.el', which seems to offer some
;;      more goodies to dired. Currently, if the `dired-x' is detected the
;;      appropriate functions in this package are diabled, to prevent
;;      overlapping behavior. However, if the function behaves differently
;;      than the one in some dired extension package, then the function
;;      isn't disabled. Eg. see `tinydired-load-all-marked-files', which can turn
;;      off marks.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'backquote)
(require 'dired)
(require 'advice)

(require 'tinylibm)

(eval-and-compile
  (autoload 'dired-do-shell-command   "dired-x" "" t)
  (autoload 'dired-read-shell-command "dired-x" "" t)

  ;; We really don't need to load full packages, so use these..
  (defvar   vc-dired-mode)
  (autoload 'vc-dired-mode                      "vc")
  (autoload 'vc-finish-logentry                 "vc")
  (autoload 'vc-next-action-on-file             "vc")
  (autoload 'vc-workfile-unchanged-p            "vc")

  ;; Too bad that can't autoload this one...
  (defvar    vc-dired-mode                      nil)
  (autoload 'vc-registered                      "vc-hooks")

  ;; The ange interface in this package is based on Emacs only
  (if (ti::emacs-p)
      (autoload 'ange-ftp-ftp-name              "ange-ftp"))

  (autoload 'dired-bunch-files                  "dired-aux")
  (autoload 'dired-run-shell-command            "dired-aux")
  (autoload 'dired-shell-stuff-it               "dired-aux")

  (defvar   diff-switches) ;; in diff.el
  (autoload 'ediff-files                        "ediff" "" t))

(eval-when-compile
  (when (ti::xemacs-p)
    (message "\n\
  ** tinydired.el: This package is for Emacs only.\n\
                   Dired and ange-ftp interfaces are incompatible between\n\
                   Emacs and XEmacs.
                   If you see XEmacs byte compiler error:
                      evaluating (< nil 0): (wrong-type-argument..
                   you can ignore it safely. The problem is in
                   dired.el::dired-map-over-marks"))

  (unless (boundp 'dired-move-to-filename-regexp)
    (message "\
  ** tinydired.el: Error, this Emacs did not define dired-move-to-filename-regexp"))
  (defvar dired-move-to-filename-regexp))

(ti::package-defgroup-tiny TinyDired tinydired-- extensions
  "Dired enchancements. Backgroud Ange ftp support.
  Overview of features

        o   Few enchancements to dired mode. Eg. keep only one
            dired buffer when ascending to directory. Shorten symlinks.
        o   User can mark and put files into STORE and start a backgroud
            ange-ftp session to get STORED files into download directory
        o   Easy switching between ange-ftp session buffer and dired buffer
        o   Dealing with ange ftp buffers in general
            (x)  killing all ange buffers at once
            (x)  killing all ange + dired ange buffers at once.
            (x)  switching to ange buffers with completion
        o   Run ! on ange ftp dired buffer (operate on local copy)
        o   customizable backup file flagging.
        o   other handy dired commands, like: pop to this file in emacs.
            find all marked files ...")

;;}}}
;;{{{ setup: vars

;;; .......................................................... &v-bind ...
;;; handle extra keybindings.

(defcustom tinydired--bind-hook
  '(tinydired-default-ange-bindings
    tinydired-default-other-bindings)
  "*Single function or list of functions to bind keys.
These are installed to `dired-mode-hook' automatically when this package
is loaded."
  :type  'hook
  :group 'Tinydired)

;;; ......................................................... &v-hooks ...

(defcustom tinydired--readin-hook
  (delq nil
        (list
         (cond
          ((ti::emacs-p "21")
           ;; Includes variable ls-lisp-dirs-first
           (message "TinyDired: `ls-lisp-dirs-first' set to t, DO NOT CHANGE.")
           (setq ls-lisp-dirs-first t)
           nil)
          (t
           'tinydired-sort-dir))
         'tinydired-kill-files
         'tinydired-shorten-links))
  "*List of functions to run after dired read.
These are inserted into `dired-after-readin-hook' when package
installs itself. Do not remove 'tinydired-sort-dir' or
it paralyzes package, because it relies on seeing directories first
in the dired listing."
  :type  'hook
  :group 'Tinydired)

(defcustom tinydired--load-hook nil
  "*Hook run when package has been loaded."
  :type  'hook
  :group 'Tinydired)

;;; ....................................................... &v-private ...

(defvar tinydired--file-store nil
  "Private. Storage of filenames.")
(make-variable-buffer-local 'tinydired--file-store)

(defvar tinydired--directory nil
  "Private. Directory name.")

(defvar tinydired--mark-list nil
  "Private. Saved filename mark list.")

(defvar tinydired--mput-last-ftp nil
  "Private. Last ftp mput site string.")
(make-variable-buffer-local 'tinydired--mput-last-ftp)

(defvar tinydired--mput-history nil
  "Private. History variable.")

(defvar tinydired--previous-buffer nil
  "Private. Recorded buffer, before leaping in another.")

(defvar tinydired--dir-copy-buffer "*tinydired-dir*"
  "Private. Copy of current directory. Created every time when needed.")

(defvar tinydired--dired-directory-ange-regexp "[@:]"
  "Regexp to match `dired-directory' to find ange-ftp buffers.")

;;}}}
;;{{{ setup: User vars

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinydired--tmp-dir
  (dolist (dir '("~/tmp/dired/"
                 "~/tmp"
                 "~"))
    (when (file-directory-p dir)
      (return dir)))
  "*Temporary directory where to store ange ftp files.
This should be user's private directory, and _must_not_ not be
/tmp,  because someone else may be running tinydired too and using
same filenames."
  :type  'directory
  :group 'Tinydired)

(defcustom tinydired--download-dir
  (dolist (dir '("~/tmp/ftp"
                 "~/ftp"
                 "~/tmp"
                 "~"))
    (when (file-directory-p dir)
      (return dir)))
  "*Directory where to down load selected files in dired listing."
  :type  'directory
  :group 'Tinydired)

(defcustom tinydired--force-add-keys-flag 'overrride
  "*Non-nil means to install and override default keys to dired.
Normally the keys are defined _only_ if the prefix key is in state
'undefined"
  :type  'boolean
  :group 'Tinydired)

(defcustom tinydired--use-only-one-buffer-flag t
  "*Non-nil means the previous dired buffer is killed when ascending to next.
This makes sure you have only one dired buffer for each dired session.
This feature is not used if dired-x is present."
  :type 'boolean
  :group 'Tinydired)

(defcustom tinydired--page-step 10
  "*Page Up step size in lines."
  :type  'integer
  :group 'Tinydired)

(defcustom tinydired--unwanted-files-regexp
  "\\.o$\\|~$\\|\\.class\\|\\.pyc"
  "*Regexp to match files that should not be shown in dired buffer.
Set to nil, if you want to see all files.
This feature is not used if dired-x is present."
  :type  '(string :tag "Regexp")
  :group 'Tinydired)

(defcustom tinydired--backup-file-regexp
  ;;  Like files from CVS: .#ChangeLog.1.3288
  "\\(\\.bak\\|\\.backup\\|[~#]\\)\\|\\.#$"
  "*Backup filename regexp, used by advised `dired-flag-backup-files'."
  :type  '(string :tag "Regexp")
  :group 'Tinydired)

(defcustom tinydired--mput-sites nil
  "*List of ange-ftp style site location strings, where user can upload files.

Format '(\"ANGE-FTP-REF\" ..),  ange-ftp-ref is like /login@site:dir/dir/"
  :type  '(repeat (string :tag "Ange-Ftp"))
  :group 'Tinydired)

(defcustom tinydired--tmp-dir-function 'tinydired-create-tmp-dir
  "*Create directory for `tinydired--tmp-dir'."
  :type  'function
  :group 'Tinydired)

(defcustom tinydired--show-storage-function
  (function
   (lambda (args)
     (message  "%d: %s" (length args)  (ti::list-to-string args))))
  "*How to show the storage to user. Default is to use `message' function.
The function is called with list of files in storage."
  :type  'function
  :group 'Tinydired)

;;}}}
;;{{{ code: install, bind, hook control

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-default-ange-bindings (&optional force)
  "Add Extra dired bindings. Optionally FORCE adding bindings."
  (interactive)
  ;;  "a" for Ange ftp related commands, since the file information
  ;;  stored is best used in *ftp* buffer itself.
  (when (or tinydired--force-add-keys-flag
            force
            (eq 'undefined (lookup-key  dired-mode-map "a")))
    ;; clear this only it the map is not in our use.
    (if (not (keymapp (lookup-key  dired-mode-map "a")))
        (define-key dired-mode-map "a" nil))
    ;;  "b"  for buffer handling
    (define-key dired-mode-map "abb" 'tinydired-switch-to-ange-ftp-buffer)
    (define-key dired-mode-map "abp" 'tinydired-switch-to-mput-ange-ftp-buffer)
    (define-key dired-mode-map "abk" 'tinydired-kill-dired-and-ange-session)
    ;; Redefine key "q" too. Was 'dired-delete-and-exit'
    (define-key dired-mode-map "q" 'tinydired-kill-dired-and-ange-session)
    (when (ti::emacs-p)              ;XEmacs has EFS, these don't work
      (define-key dired-mode-map "as" 'tinydired-store-filename)
      (define-key dired-mode-map "ad" 'tinydired-store-delete-filename)
      (define-key dired-mode-map "aS" 'tinydired-store-add-marked)
      (define-key dired-mode-map "ar" 'tinydired-store-remove-file)
      (define-key dired-mode-map "aR" 'tinydired-store-delete-marked)
      (define-key dired-mode-map "ac" 'tinydired-store-clear)
      ;;  the "q" is just close to "a" key, no other particular logic used.
      (define-key dired-mode-map "aq" 'tinydired-store-show)
      ;;  "g"  for "get"
      (define-key dired-mode-map "ag" 'tinydired-store-ftp-mget)
      (define-key dired-mode-map "ap" 'tinydired-store-ftp-mput)))
  nil)

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-default-other-bindings (&optional force)
  "Add extra dired bindings. Optionally FORCE adding bindings."
  (when (or tinydired--force-add-keys-flag
            force
            (eq 'undefined (lookup-key  dired-mode-map "t")))
    ;;  make prefix key available for us.
    (ti::use-prefix-key dired-mode-map "t")
    ;;  You propably want to do also
    ;;  (define-key  dired-mode-map "!" 'tinydired-dired-do-shell-command)
    (define-key  dired-mode-map "t!"    'tinydired-dired-do-shell-command)
    (define-key  dired-mode-map "t-"    'tinydired-one-dir-up)
    ;;  "f" for find-file related
    (ti::use-prefix-key dired-mode-map "tf")
    (define-key  dired-mode-map "tff"   'tinydired-load-all-marked-files)
    (define-key  dired-mode-map "tfr"   'tinydired-marked-revert-files)
    (define-key  dired-mode-map "tg"    'tinydired-refresh-view)
    (define-key  dired-mode-map "tG"    'tinydired-read-dir-as-is)
    (ti::use-prefix-key dired-mode-map "tk")
    (define-key  dired-mode-map "tkk"   'tinydired-kill-lines)
    (define-key  dired-mode-map "tkm"   'tinydired-kill-marked-lines)
    (define-key  dired-mode-map "tkM"   'tinydired-kill-unmarked-lines)
    (define-key  dired-mode-map "tl"    'tinydired-leave-only-lines)
    (define-key  dired-mode-map "tp"    'tinydired-pop-to-buffer)
    (define-key  dired-mode-map "te"    'tinydired-ediff)
    (define-key  dired-mode-map "t<"    'tinydired-shorten-links)
    (define-key  dired-mode-map "t>"    'tinydired-lenghten-links)
    ;;  Mark related commands in "m" map
    (ti::use-prefix-key dired-mode-map "tf")
    (define-key  dired-mode-map "tme"   'tinydired-mark-files-in-Emacs)
    (define-key  dired-mode-map "tmd"   'tinydired-mark-today-files)
    (define-key  dired-mode-map "tmo"   'tinydired-mark-opposite-toggle)
    (define-key  dired-mode-map "tmr"   'tinydired-mark-read-only-files)
    (define-key  dired-mode-map "tms"   'tinydired-marks-save)
    (define-key  dired-mode-map "tmS"   'tinydired-marks-restore)
    (define-key  dired-mode-map "tmw"   'tinydired-mark-writable-files)
    (ti::use-prefix-key dired-mode-map "tmv")
    (define-key  dired-mode-map "tmvv"  'tinydired-mark-vc-files-in-Emacs)
    (define-key  dired-mode-map "tmvd"  'tinydired-mark-vc-has-diffs)
    ;;  some special VC functions for marked files in "v" map
    (ti::use-prefix-key dired-mode-map "tv")
    (define-key dired-mode-map "tvi"    'tinydired-marked-vc-ci)
    (define-key dired-mode-map "tvo"    'tinydired-marked-vc-co)
    (define-key dired-mode-map "tvu"    'tinydired-marked-vc-revert)
    ;;  Override some Emacs default bindings to better follow
    ;;  this buffer's content.
    (define-key dired-mode-map "\M-<"   'tinydired-first-line)
    (define-key dired-mode-map "\M->"   'tinydired-last-file)
    (define-key dired-mode-map [(home)]   'tinydired-first-file)
    (define-key dired-mode-map [(end)]    'tinydired-last-file)
    (define-key dired-mode-map [(select)] 'tinydired-last-file) ;; 'end' key
    (define-key dired-mode-map [(prior)]  'tinydired-pgup)
    (define-key dired-mode-map [(next)]   'tinydired-pgdown))
  nil)

;;; ----------------------------------------------------------------------
;;; - If user has relocated some keys...well, we don't handle those.
;;;
(defun tinydired-remove-bindings ()
  "Remove bindings from this dired session.
User must be in dired buffer. Makes the `dired-mode-map'
local to current buffer."
  (interactive)
  (let ((list
	 '("abb" "abp" "as" "aS" "ar" "aR" "ac" "aq" "ag" "ap"
	   "t!" "tf" "tg" "tk" "tl" "tp" "t<" "t>"
	   "tmd" "tml" "tms" "tmS" "tmv"
	   "tvi" "tvo" "tvu")))
    (when (and (memq major-mode '(dired-mode))
               dired-mode-map)
      (make-local-variable 'dired-mode-map)
      (dolist (elt list)
        (define-key dired-mode-map elt 'tinydired-ignore))
      ;;  And the rest
      (define-key dired-mode-map "\M-<"   'beginning-of-buffer)
      (define-key dired-mode-map "\M->"   'end-of-buffer)
      (define-key dired-mode-map [(home)] 'beginning-of-buffer)
      (define-key dired-mode-map [(end)]  'end-of-buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-hook-control (&optional remove)
  "Add hooks to dired mode. Optional REMOVE all hooks inserted by package."
  (interactive "P")
  (let ((list (ti::list-make tinydired--bind-hook)))
    (cond
     (remove
      (ti::add-hooks 'dired-after-readin-hook tinydired--readin-hook 'remove)
      (ti::add-hooks 'dired-mode-hook     tinydired--bind-hook  'remove))
     (t
      ;;  Now, install the package
      (ti::add-hooks 'dired-after-readin-hook tinydired--readin-hook)
      (dolist (x list)                  ;bind the keys
        (add-hook 'dired-mode-hook x)
        ;;  This is due to autoload: while the package is beeing loaded,
        ;;  it should also set the bindings immediately
        (if (boundp 'dired-mode-map)
            (funcall x)))))
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-install (&optional remove)
  "Install package. Optionally REMOVE."
  (interactive "P")
  (cond
   (remove
    (tinydired-hook-control   remove)
    (tinydired-advice-control remove))
   (t
    (tinydired-hook-control)
    (tinydired-advice-control)
    (tinydired-xemacs-note))))

;;}}}
;;{{{ XEmacs compatibility

;;; ----------------------------------------------------------------------
;;; Some functions are not found from XEmacs, mimic them
;;;
(defun tinydired-dired-unmark-all-files-no-query ()
  "XEmacs compatibility."
  (if (fboundp 'dired-unmark-all-files-no-query)
      (ti::funcall 'dired-unmark-all-files-no-query)
    (ti::save-line-column-macro nil nil
      (tinydired-first-line)
      (while (or (not (eobp))
                 (not (looking-at "^[ \t]*$")))
        ;;  Just use brute force for all lines.
        (dired-unmark 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-xemacs-note ()
  "Warn that tinydired.el may work improperly in XEmacs."
  (when (and (ti::xemacs-p)
             (not (y-or-n-p
                   "You know that TinyDired's features won't work in XEmacs?")))
    (tinydired-advice-control 'disable)
    (error "Abort.")))

;;}}}
;;{{{ code: ange-ftp.el

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-advice-control-old (&optional disable verb)
  "Activate all advises. Use extra argument to DISABLE all. VERB."
  (interactive "P")
  (let ((re    "^tdd")
	(doit  t)
	msg)
    (ti::verb)
    (if verb
        (setq
         doit
         (y-or-n-p (format "advices %s: No mistake here? "
                           (if disable
			       "off"
			     "on")))))
    (when doit
      (cond
       (disable
        (ad-disable-regexp re)          ;only sets flag
        (setq msg "All advices deactivated"))
       (t
        (ad-enable-regexp re)           ;only sets flag
        (setq msg "All advices activated")))
      (ad-update-regexp re)
      (if verb
          (message msg)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-advice-control (&optional disable)
  "Activate or DISABLE advices in this package."
  (interactive "P")
  (ti::advice-control
   '(ange-ftp-set-binary-mode
     dired-move-to-end-of-filename
     ange-ftp-get-pwd
     ange-ftp-expand-file-name
     ange-ftp-get-file-entry
     dired-flag-backup-files
     dired-find-file)
   "^tinydired-"
   disable
   'verbose
   "TinyDired advices "))

;;; ----------------------------------------------------------------------
;;;
(defadvice ange-ftp-set-binary-mode (before tinydired-error-prevent-fix dis)
  "Sometimes you can get error:
ash(nil -4)

  `ange-ftp-set-binary-mode'(\"ftp.uit.no\" \"ftp\")

Which is due to missing variables

  `ange-ftp-ascii-hash-mark-size'
  `ange-ftp-binary-hash-mark-size'

This advice resets them to some default values, so that you don't get
errors."
  (save-excursion
    (with-current-buffer (ange-ftp-ftp-process-buffer host user)
      (if (null ange-ftp-ascii-hash-mark-size)
	  (setq ange-ftp-ascii-hash-mark-size 1024))
      (if (null ange-ftp-binary-hash-mark-size)
	  (setq ange-ftp-binary-hash-mark-size 1024)))))

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;; - See dired-repeat-over-lines
;;;
(defmacro tinydired-map-over-regexp (re &rest body)
  "If '(looking-at RE)' then do BODY over all lines matching.
Start from current point. The point is positioned at the beginning of line.
Buffer read-only is removed.

The BODY should move the pointer to next file and bol, until eob reached."
  `(let ((end (tinydired-last-file-point))
	 buffer-read-only)
     (beginning-of-line)
     (while (and (not (eobp))
                 (< (point) end))
       (beginning-of-line)
       (if (looking-at ,re)
           (progn
             ,@ body)
         (forward-line 1)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydired-map-over-files 'lisp-indent-function 0)
(defmacro tinydired-map-over-files (&rest body)
  "Map over files. No No dirs are included.
You must advance the cursor in the BODY. See `tinydired-map-over-regexp'."
  `(progn
     (tinydired-first-file)
     (tinydired-map-over-regexp "^. +[^d]" ,@ body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydired-map-over-unmarked 'lisp-indent-function 0)
(defmacro tinydired-map-over-unmarked (&rest body)
  "Map over unmarked lines and execute BODY at the beginning of line.
The calling BODY should position the cursor for next search so
that current line is skipped when BODY finishes.

The buffer is writable during mapping."
  `(let (buffer-read-only
	 (ReGexp (dired-marker-regexp)))
     (progn
       (tinydired-map-over-files
        (if (looking-at ReGexp)
            (forward-line 1)
          (beginning-of-line)
          ,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydired-remember-marks (var-sym &optional beg end)
  "Save mark list to variable VAR-SYM between points BEG and END.
START and END defaults to all files"
  `(setq ,var-sym
	 (dired-remember-marks
          (or ,beg
              (tinydired-first-line-point))
          (or ,end
              (tinydired-last-file-point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-ignore ()
  "Ignore message."
  (interactive)
  (message "TinyDired: Function is not available in this dired buffer."))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-create-tmp-dir ()
  "Create directory `tinydired--tmp-dir' if possible."
  (make-directory (expand-file-name tinydired--tmp-dir)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-get-tmp-dir ()
  "Return temp directory with slash at the end."
  (let ((dir   tinydired--tmp-dir)
	(func  tinydired--tmp-dir-function))
    (unless (not (file-exists-p dir))
      (setq dir (funcall func)))
    (setq dir (expand-file-name dir))
    (unless (file-exists-p dir)
      (error "TinyDired: Directory not exist %s" dir))
    (file-name-as-directory dir)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-get-filename ()
  "Return only filename without directory."
  ;;  The (dired-get-filename t) almos does the same, but it _may_
  ;;  contains slahes.. docs say so.
  (ti::string-match "\\([^/]+\\)$" 1 (dired-get-filename)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-get-mark ()
  "Return first char to the left. Point is not preserved."
  (beginning-of-line)
  (following-char))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-get-marked-files ()
  "Signal no errors."
  (ignore-errors (dired-get-marked-files)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-get-marked-files-no-dir ()
  "Return LIST of marked files."
  ;; #todo:  See this code via macroexpand And you'll find test
  ;;
  ;;    (if (< nil 0) (nreverse results) results))
  ;;
  ;;  Which flags an compile error in XEmacs.
  ;;
  (dired-map-over-marks
   (tinydired-get-filename)
   nil))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-first-line-point ()
  "Return first file point."
  (save-excursion
    (tinydired-first-line)
    (line-beginning-position)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-last-file-point ()
  "Return last file point."
  (save-excursion (tinydired-last-file) (line-end-position)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-mark-re (re &optional unmark)
  "Mark files matching RE. Give prefix argument to UNMARK."
  (save-excursion
    (ti::pmin)
    (while (re-search-forward re nil t)
      (if unmark
          (dired-unmark 1)
        (dired-mark 1)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-mark-file-list (list &optional unmark)
  "Mark files in LIST. Give prefix argument to UNMARK."
  (dolist (elt (ti::list-make list))
    (tinydired-mark-re (concat (regexp-quote elt) "$") unmark)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-feature-p (arg)
  "Check if we already have this functionality in dired. See ARG from code."
  ;;  Emacs with with `dired-x', which I just noticed had some of
  ;;  the same functionality. We don't use TDD if those
  ;;  are present in some cases.
  (cond
   ((eq arg 'auto-delete)
    ;;  see dired-omit-files-p
    (and (featurep 'dired-x)
         (> emacs-minor-version 27)))
   (t
    nil)))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tinydired-normal-buffer-p ()
  "Check if buffer's first line look like dired."
  (interactive)
  (and (not (ti::buffer-narrowed-p))
       (save-excursion
         (ti::pmin)
         (and (looking-at "^[ \t]+\\([a-z]:\\)?/")
;;;  In VAX these don't exist.
;;;
;;;           (forward-line 1)
;;;           (looking-at "^[ \t]+total[ \t]+[0-9]")
              t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-kill-files ()
  "After each dired read, remove unwanted files."
  (let ((re     tinydired--unwanted-files-regexp)
	buffer-read-only)
    (unless (tinydired-feature-p 'auto-delete)
      ;;  Is this new directory buffer ..
      (if (and (eq major-mode 'dired-mode)
               (stringp re))
          (flush-lines re))             ;don't wanna see these
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-file-list (&optional arg mode)
  "Gets all files/dir entries in the view.
The ARG is `dired-get-filename' parameter.

Input:

  ARG           If non-nil, no absolute names
  MODE          if 're then make regexp out of files.
                if 'files then return just filenames

Return list:

  (re re ..)            If mode is 're
  (file file ...)       If mode is 'plain
  ((mark file) ..)      default

The `mark' is first character in the left for file or dir."
  (let (last-point
	list
	file)
    (save-excursion
      (setq last-point (tinydired-last-file-point))
      (tinydired-first-line)
      (if (setq file (ignore-errors
		       (dired-get-filename arg)))
          (cond
           ((eq mode 're)
            (beginning-of-line)
            (if (looking-at dired-re-sym)
                (push (concat (regexp-quote file) " +->") list)
              (push (format " %s$" (regexp-quote file)) list)))
           ((eq mode 'files)
            (push file list))
           (t
            (push (list (tinydired-get-mark) file) list))))
      (while (< (point) last-point)
        (dired-next-line 1)
        (if (setq file (ignore-errors (dired-get-filename arg)))
            (cond
             ((eq mode 're)
              (beginning-of-line)
              (if (looking-at dired-re-sym)
                  (push (concat (regexp-quote file) " +->") list)
                (push (format " %s$" (regexp-quote file)) list)))
             ((eq mode 'files)
              (push file list))
             (t
              (push (list (tinydired-get-mark) file) list))))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-sort-dir ()
  "Put directories first in dired listing."
  (let (buffer-read-only
        marks
        p1
	p2                           ;point
        region)
    ;;  - Buffer gets narrowed in some dired internal operations, like
    ;;    pressing "l", dired-do-redisplay
    ;;  - We do nothing in these cases
    ;;
    ;;  - We have to save position, because e.g. pressing "Z" to
    ;;    compress file, causes reading the whole dir --> point moved.
    ;;
    (when (tinydired-normal-buffer-p)
      (ti::save-with-marker-macro
        (tinydired-first-line)
        (tinydired-remember-marks marks (point))
        (tinydired-dired-unmark-all-files-no-query) ; sort goes nuts otherwise
        (message "")                 ; stupid message from dired-un...
        ;; Sort regexp by
        ;; 19 Nov 1995, sof@dcs.glasgow.ac.uk (Sigbjorn Finne), comp.Emacs
        ;;
        (tinydired-first-line) (beginning-of-line)
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))
        ;;  now, We prefer to have dirs first, and then links, allthough
        ;;  some links may be dirs (we can't know anything about links)
        ;;
        (ti::pmin)
        (when (re-search-forward "^[ \t]+lr" nil t)
          (setq p1 (line-beginning-position))
          ;;  We know that dirs are after links, because the listing is
          ;;  sorted.
          ;;
          (re-search-forward "^[ \t]+d" nil t)
          (setq p2 (line-beginning-position))
          (setq region (buffer-substring p1 p2))
          (delete-region p1 p2)
          (re-search-forward "^[ \t]+-" nil t) ;go after dirs
          (beginning-of-line)
          (insert region))
        (dired-mark-remembered marks)
        (set-buffer-modified-p nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-dir-original (dir &optional buffer)
  "Do same as `dired-insert-directory'.
Insert DIR to BUFFER, which defaults to `tinydired--dir-copy-buffer'"
  (save-excursion
    ;;  See dired.el dired-readin-insert
    (ti::temp-buffer (or buffer
			 tinydired--dir-copy-buffer)
		     'clear)
    (with-current-buffer (or buffer
			     tinydired--dir-copy-buffer)
      (insert-directory (expand-file-name dir)
			dired-listing-switches nil t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-dir-original-get-line (file)
  "Return original line for FILE.
Be sure you have called `tinydired-dir-original' first.
Signal no error. Use `regexp-quote' for FILE if it contains unusual characters.

Return:
  line
  nil   ,no line was found"
  (save-excursion
    (with-current-buffer tinydired--dir-copy-buffer
      (ti::pmin)
      ;;  Pick first match
      (if (re-search-forward (concat " " file) nil t)
	  (ti::read-current-line)))))

;;}}}
;;{{{ code: interactive

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-ediff (file &optional switches)
  "Compare file at point with file FILE using `ediff'.
FILE defaults to the file at the mark.
The prompted-for file is the first file given to `ediff'.
With prefix arg, prompt for second argument SWITCHES,
 which is options for `diff'."
  (interactive
   (let ((default (if (mark t)
                      (save-excursion (goto-char (mark t))
                                      (dired-get-filename t t)))))
     (list
      (read-file-name                   ;ARG 1
       (format "Ediff %s with: %s"
               (dired-get-filename t)
               (if default
                   (concat "(default " default ") ")
                 ""))
       (dired-current-directory) default t)
      (if current-prefix-arg            ;ARG 2
          (read-string
           "Options for diff: "
           (if (stringp diff-switches)
               diff-switches
             (mapconcat 'identity diff-switches " ")))))))
  ;; Interactive part end
  (ediff-files file
               (dired-get-filename t) switches))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-read-dir-as-is ()
  "Read the directory without any filtering."
  (interactive)
  (let (dired-after-readin-hook)
    (revert-buffer)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-mark-files-in-Emacs ()
  "Mark all files in current directory that are in Emacs."
  (interactive)
  (let* ((dir (expand-file-name dired-directory)) ;get rid of "~"
         (list (ti::dolist-buffer-list
                (and (buffer-file-name)
                     (string-match (regexp-quote dir) (buffer-file-name))))))
    (if (null dir)
        (setq dir dir))                 ;Shut up byteCompiler
    (dolist (elt list)
      (save-excursion
        (tinydired-first-file)
        (if (re-search-forward elt nil t)
            (dired-mark 1))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-marked-revert-files (&optional arg)
  "Revert ie. replace files in Emacs with true copies in directory.
If ARG is non-nil, remove any marks if file was loaded.

Exceptions:
  Only reload files in Emacs whose modify flag is non-nil.
  If file does not exist in Emacs, do nothing."
  (interactive "P")
  (let ((list (tinydired-get-marked-files))
	buffer)
    (dolist (file list)
      (when (setq buffer (get-file-buffer file))
        (with-current-buffer buffer
          (unless (buffer-modified-p)
            (revert-buffer nil t)       ;no confirmation
            (setq buffer 'done) )))
      (when (and arg  (eq 'done buffer))
        (tinydired-mark-re
         (regexp-quote (file-name-nondirectory file)) 'unmark)))))

;;; ----------------------------------------------------------------------
;;; - It's lot faster to use direct command, than search the buffer
;;;   for ".." and use "f" or click mouse over it.
;;;
;;;###autoload
(defun tinydired-one-dir-up ()
  "Go up one directory."
  (interactive)
  (find-file (concat dired-directory "..")))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-dired-do-shell-command (command &optional arg)
  "Like `dired-do-shell-command', but run running command in dired ange-ftp.
This is not remote shell, but instead it
transfers the file to your local system and then executes the dired
command on the file.

Remember: Every time you run this command this files are copied _blindly_
to your local directory. No file cache information is kept.

Input:

  COMMAND
  ARG

References:

  `tinydired--tmp-dir'"
  (interactive
   (list
    (dired-read-shell-command
     (concat "! on "
             "%s: ")
     current-prefix-arg
     (dired-get-marked-files
      t current-prefix-arg))
    current-prefix-arg))
  (let ((to-dir  (tinydired-get-tmp-dir))
	(ange    (ange-ftp-ftp-name dired-directory))
	(on-each (not (string-match "\\*" command)))
	host
	user
	dir
	file-list
	list)
    (cond
     ((null ange)
      ;; Simple local dired.
      (dired-do-shell-command command arg))
     (t
      (setq host  (nth 0 ange)
            user  (nth 1 ange)
            dir   (nth 2 ange))
      (setq file-list (dired-get-marked-files t))
      (ti::file-ange-file-handle 'get user host dir to-dir file-list 'foreground)
      (dolist (file file-list)       ; All directory to every filename
        (push (concat to-dir file)  list))
      (setq file-list list)
      ;; ......................................... copy from dired-aux ...
      (if on-each
          (dired-bunch-files
           (- 10000 (length command))
           (function
            (lambda (&rest files)
              (dired-run-shell-command
               (dired-shell-stuff-it command files t arg))))
           nil
           file-list)
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. then ...
        ;; execute the shell command
        (dired-run-shell-command
         (dired-shell-stuff-it command file-list nil arg)))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-lenghten-links ()
  "Opposite to `tinydired-shorten-links'.
This may take a while, because the whole directory structure must
be read again."
  (interactive)
  (let ((line   (ti::current-line-number))
	file
	marks
	buffer-read-only)
    (when (tinydired-normal-buffer-p)
      ;;        Now create copy of original directory.
      (tinydired-dir-original dired-directory)
      (tinydired-remember-marks marks)
      (tinydired-dired-unmark-all-files-no-query)
      (message "")
      (dired-mark-symlinks nil)
      ;;   This didn't update full line, only the data part, not the
      ;;   linked name portion "->"
      ;;      (dired-do-redisplay)
      (dired-map-over-marks
       (progn
         (setq file (dired-get-filename 'no-dir))
         (setq line (tinydired-dir-original-get-line (regexp-quote file)))
         ;;  now, delete line and relace it with original entry.
         (when line
           (beginning-of-line)
           (re-search-forward " l")
           (backward-char 1)
           (delete-region (point) (line-end-position))
           (insert line) ))
       nil)
      (dired-mark-symlinks 'unmark)
      (if marks
          (dired-mark-remembered marks))
      (set-buffer-modified-p nil))))

;;; ----------------------------------------------------------------------
;;; - It's awfull to see 30 linked files whyen they don't fit on one line...
;;;
;;;###autoload
(defun tinydired-shorten-links ()
  "Shortens all linked files. The link part is removed."
  (interactive)
  (let ((line (ti::current-line-number))
	buffer-read-only)
    (when (tinydired-normal-buffer-p)
      (ti::pmin)
      (while (not (eobp))
        (if (looking-at ".* +->\\([^\n]+\\)")
            (ti::replace-match 1))
        (forward-line 1))
      (ti::goto-line line)
      (dired-move-to-filename))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-marks-save ()
  "Save mark list to private storage.
Use this function if you know next operation will remove the marks.
Restore saved marks with `tinydired-marks-restore'."
  (interactive)
  (save-excursion                       ;due to next line
    (tinydired-remember-marks tinydired--mark-list)
    (message "TinyDired: Marks saved.")))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-marks-restore ()
  "Restore mark list saved by `tinydired-marks-save'."
  (interactive)
  (if (null tinydired--mark-list)
      (message
       (substitute-command-keys
        "No marks saved. Use '\\[tinydired-marks-save]' first."))
    (dired-mark-remembered tinydired--mark-list)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-pgup ()
  "Move cursor to last file in dired mode."
  (interactive)
  (dired-next-line (- tinydired--page-step))
  (if (bobp)
      (tinydired-first-line)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-pgdown ()
  "Move cursor up."
  (interactive)
  (dired-next-line tinydired--page-step)
  (if (eobp)
      (tinydired-last-file)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-first-line ()
  "Move to first _line_ in dired."
  (interactive)
  (let (point)
    (save-excursion
      (ti::pmin)
      (forward-line 2)
      (when (looking-at "^  .*[rwx]")
        (dired-move-to-filename)
        (setq point (point))))
    (if point
        (goto-char point)
      ;; Then,  it's some strange non-unix propably ...
      nil)))

;;; ----------------------------------------------------------------------
;;; - Supposing the directory is in order...dirs first then files...
;;;
;;;###autoload
(defun tinydired-first-file ()
  "Move to first file in dired."
  (interactive)
  (let (point)
    (save-excursion
      (ti::pmin)
      (while (and (null point)
                  (not (eobp)))
        (forward-line 1)
        (dired-move-to-filename)
        (unless (eq 0 (current-column))
          (setq point (point))) ))
    (if point
        (goto-char point)
      ;; Then, it's some strange non-unix propably ...
      nil)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-last-file ()
  "Move to last file in dired."
  (interactive)
  (let (point)
    (save-excursion
      (ti::pmax)
      (while (and (null point)
                  (not (bobp)))
        (forward-line -1)
        (dired-move-to-filename)
        (unless (eq 0 (current-column))
          (setq point (point))) ))
    (if point
        (goto-char point)
      ;; Then, it's some strange non-unix propably ...
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-kill-marked-lines ()
  "Remove lines that are unmarked."
  (interactive)
  (let (buffer-read-only
        list)
    (dired-map-over-marks
     (push (regexp-quote (ti::read-current-line)) list)
     nil)
    (dolist (re list)
      (ti::pmin)
      (if (re-search-forward re nil t)
          (ti::buffer-kill-line)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-kill-unmarked-lines ()
  "Remove unmarked lines. Ignore directories and symlinks."
  (interactive)
  (tinydired-map-over-unmarked
   (let (char)
     ;; We're at the beginning of line, suppose std unix 'ls'
     ;; drwx--x--x
     (setq char (buffer-substring (+ 2 (point)) (+ 3 (point))))
     (if (not (or (string= char "d")
                  (string= char "l")))
         (ti::buffer-kill-line)
       ;; Continue mapping
       (end-of-line))))
  (tinydired-first-file))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-kill-lines (re)
  "Delete lines matching RE."
  (interactive "sKill files re: ")
  (let (buffer-read-only)
    (unless (ti::nil-p re)
      (ti::save-line-column-macro (tinydired-first-file) (dired-move-to-filename)
        (tinydired-first-file) ;; do this in ti::save-line-column-macro
        (flush-lines re)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-leave-only-lines (re)
  "Leave only lines matching RE. Directory lines are skipped.
You can easily undo this with reverting the buffer (dired \"g\")."
  (interactive "sLeave regexp: ")
  (unless (ti::nil-p re)
    (ti::pmin)
    (tinydired-map-over-files
     (if (string-match re (ti::read-current-line))
         (forward-line 1)
       (ti::buffer-kill-line)))
    (tinydired-first-file)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-pop-to-buffer ()
  "Pop to buffer if it exists in Emacs."
  (interactive)
  (let ((file (ignore-errors (dired-get-filename)))
	buffer)
    (cond
     ((and (stringp file)
           (setq buffer (get-file-buffer file)))
      (pop-to-buffer buffer))
     (t
      (message (format "TinyDired: Can't pop ... Not in Emacs. [%s]"
                       file))))))

;;; ----------------------------------------------------------------------
;;; - This behaves differently than dired-x.el dired-do-find-marked-files
;;;
;;;###autoload
(defun tinydired-mark-today-files ()
  "Mark all files, not dirs, that are created today.
Point sits on first today file. If no today's files are found, point stays
on current filename."
  (interactive)
  (let* ((list   (ti::date-time-elements))
         (line   (ti::current-line-number))
         ;;      1024 Oct  3
         (re     (concat ".*[0-9] " (nth 5 list) " +"
                         (number-to-string (nth 0 list))
                         " +"
                         ;;  This year's file have time in this field
                         "[0-9]+:")))
    (tinydired-map-over-files
     (if (not (looking-at re))
         (forward-line)
       (dired-mark 1) ))
    (tinydired-first-file)
    (if (re-search-forward re nil t)
        (dired-move-to-filename)
      (ti::goto-line line)
      (dired-move-to-filename))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-mark-writable-files ()
  "Mark Your files that have writable flag set."
  (interactive)
  (let ((re ".*.w..[-w]..[-w]. "))
    (tinydired-map-over-files
     (if (not (looking-at re))
         (forward-line)
       (dired-mark 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-mark-read-only-files ()
  "Mark Your files that have writable flag set."
  (interactive)
  (let ((re ".*r-.[-r]..[-r].. "))
    (tinydired-map-over-files
     (if (not (looking-at re))
         (forward-line)
       (dired-mark 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-mark-opposite-toggle ()
  "Mark opposite files.
Ie. if you have marked some files, unmark those and mark all other files."
  (interactive)
  (let ((re (dired-marker-regexp)))
    (ti::save-line-column-macro nil nil
      (tinydired-map-over-files
       (beginning-of-line)
       (if (looking-at re)
           (dired-unmark 1)
         (dired-mark 1))))))

;;; ----------------------------------------------------------------------
;;; - This behaves differently than dired-x.el dired-do-find-marked-files
;;;
;;;###autoload
(defun tinydired-mark-vc-files-in-Emacs (&optional unmark verb)
  "Mark all files in the current _view_ that are in Emacs _and_ in VC control.
Optionally UNMARK. VERB."
  (interactive)
  (let ((dir (expand-file-name dired-directory))
	(msg (if unmark
		 "Unmarking..."
	       "Marking..."))
	list)
    (ti::verb)
    (if (null dir)
        (setq dir dir))                 ;Shut up XEmacs 19.14 ByteComp
    (setq list
          (ti::dolist-buffer-list
           (and buffer-file-name
                (string-match dir buffer-file-name)
                (vc-registered buffer-file-name))))
    (if verb
        (message msg))
    (cond
     ((and (null list)   verb)
      (message "Tinydired: No VC files of this dir in Emacs."))
     (t
      (tinydired-mark-file-list list unmark)
      (if verb
          (message (concat msg "Done")))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-refresh-view (&optional verb)
  "Refresh current dired view.
If you have used `tinydired-leave-only-lines' and have done some changes to
the files. You can use this function to re-read the current view.

The dired \"g\" will load full view back. This instead caches the
current view, executes read, and deletes lines that weren't in the
cache --> you get refreshed view. All this may take a while...

Input:

 VERB       Verbose messages

Return:

 t              if refreshed
 nil"
  (interactive)
  (let ((cache (tinydired-file-list 'no-path-names 're))
	(line  (ti::current-line-number)) ;save user position
	(re    "")
	buffer-read-only               ;allow write
	marks)
    (ti::verb)
    (cond
     (cache
      (setq re (mapconcat 'concat cache "\\|"))
      (setq marks
            (dired-remember-marks
             (tinydired-first-line-point)
             (tinydired-last-file-point)))
      ;; sort goes nuts otherwise
      (tinydired-dired-unmark-all-files-no-query)
      (message "")                   ; stupid message from dired-un...
      (revert-buffer)
      (ti::pmin)
      (forward-line 2)                  ;leave headers
      (tinydired-first-file)
      (beginning-of-line)
      (let ((case-fold-search nil))     ;case sensitive
        (delete-non-matching-lines re))
      (dired-mark-remembered marks)
      (ti::goto-line line)
      (dired-move-to-filename)
      (if verb
          (message "TinyDired: Refresh done."))
      t)
     (t
      (if verb
          (message "TinyDired: Can't cache view."))
      nil))))

;;; ----------------------------------------------------------------------
;;; - This behaves differently than dired-x.el dired-do-find-marked-files
;;;
;;;###autoload
(defun tinydired-load-all-marked-files (&optional arg verb)
  "Load all marked files into Emacs.
Does not load files which are already in Emacs.
If ARG is non-nil, remove mark if file was loaded. VERB."
  (interactive "P")
  (let ((files         (tinydired-get-marked-files))
	(loaded        0)
	(not-loaded    0)
	(all           0))
    (ti::verb)
    (cond
     ((and verb (null files))
      (message "Tinydired: No marked files."))
     ((y-or-n-p "Tinydired: Load all marked files, No kidding? ")
      (dolist (file files)
        (incf all)
        (if (get-file-buffer file)
            (incf  not-loaded)
          (incf  loaded)
          (find-file-noselect file))
        (if arg
            (save-excursion (dired-unmark 1))))))
    (if verb
        (cond
         ((eq all not-loaded)
          (message "Hmm, all files are in Emacs already.."))
         (t
          (message "Tinydired: %s files loaded." loaded))))))

;;}}}
;;{{{ code: vc special

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-mark-vc-has-diffs (&optional arg)
  "Leave mark to files: VC controlled, have diffs and are in Emacs.
If ARG is non-nil, examine file whether it was in Emacs or not.

Note:
  Please be patient, taking diffs may be slow per file."
  (interactive)
  (let ((list (tinydired-get-marked-files))
	fn
	buffer
	vc-reg-stat
	diff-no-stat)
    (dolist (file list)
      (setq fn          (file-name-nondirectory file)
            buffer      (get-file-buffer file)
            vc-reg-stat (vc-registered file))
      (cond
       ((or (not vc-reg-stat)
            ;;  Not exist in Emacs, do not bother looking
            (and (null arg) (null buffer)))
        (tinydired-mark-re (regexp-quote fn) 'unmark))
       (t
        (setq diff-no-stat (vc-workfile-unchanged-p file 'get-diffs))
        (if diff-no-stat
            (tinydired-mark-re (regexp-quote fn) 'unmark)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-marked-vc-revert (&optional arg verb)
  "Revert all version controlled/no changed/marked files. Ignore ARG. VERB."
  (interactive "P")
  (let* ((list          (tinydired-get-marked-files))
         (display       (if list
			    t))
         (vc-dired-mode nil)            ;turn mode off
         (count         0)
         (handled       0)
         load
         buffer
         vc-reg-stat
         diff-no-stat)
    (ti::verb)
    (dolist (file list)
      (setq buffer       (get-file-buffer file)
            vc-reg-stat  (vc-registered file)
            load         nil
            diff-no-stat nil)
      (incf  count)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... possible load . .
      (when (and (null buffer)
                 (file-writable-p file)
                 vc-reg-stat)
        (setq buffer (find-file-noselect file)
              load   t))
      (if buffer
          (setq diff-no-stat (vc-workfile-unchanged-p file 'get-diffs)))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. handle . .
      (cond
       ((null buffer)
        nil)                            ;no file, no vc controlled
       (diff-no-stat
        (incf  handled)
        (save-window-excursion
          (vc-next-action-on-file file 'verbose)
          (if load
              (kill-buffer buffer))))))
    (if display
        (dired-do-redisplay))
    (if verb
        (message "Tinydired: VC revert:  %s/%s handled "  handled count))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-marked-vc-co (&optional arg)
  "Check Out all marked files and load them inside Emacs.
Do some checking, before doing co.
o  if file is writable, skip over.
o  if file is not in RCS, skip over.

Optional ARG skips all load confirmations.

Marks are left only to files which were loaded into Emacs."
  (interactive "P")
  (let ((list          (tinydired-get-marked-files))
	(dired-vc-mode nil)            ;turn mode off
	(count         0)
	(loaded        0)
	(handled       0)
	fn
	buffer
	load
	vc-reg-stat
	modify-stat
	read-stat)
    (if dired-vc-mode
        (setq dired-vc-mode nil))       ;ByteComp silencer
    (dolist (file list)
      (setq fn          (file-name-nondirectory file)
            buffer      (get-file-buffer file)
            vc-reg-stat (vc-registered file)
            load        nil)
      (if buffer                      ;read stat only if it's in Emacs
          (save-excursion
	    (with-current-buffer buffer
	      (setq modify-stat (buffer-modified-p)
		    read-stat   buffer-read-only))))
      (incf count)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... possible load . .
      (when (and (null buffer)
		 vc-reg-stat                        ;; in VC
		 (not (file-writable-p file))       ;; -r--r--r--
		 (or arg
		     (y-or-n-p
		      (concat "file " fn " not in Emacs. Load? " ))))
        (incf loaded)
        (setq buffer (find-file-noselect file)
              load   t))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. handle . .
      (cond
       (load
        ;; nothing
        nil)
       ((and buffer ;; in Emacs, be extra carefull
             vc-reg-stat                        ;; in VC
             (not (file-writable-p file))       ;; -r--
             (null modify-stat)                 ;; %*
             read-stat)                         ;; %%
        ;; --> no-op, valid state
        nil)
       (t ;; User has modified it!
        ;;  This situation may occur very easily
        ;;  - You load -r-- file in Emacs that's in VC
        ;;  - you want to temporary play with it, like changing one
        ;;    flag in .mak temporarily
        ;;  - you go and M-x toggle-read-only, change it, C-x C-s
        ;;  ...
        ;;  Now you have modified the read-only file !
        (setq buffer nil)))
      (cond
       ((null buffer)
        ;;     18 15:09 test3.txt
        (tinydired-mark-re (concat "[0-9] +" fn) 'unmark)
        nil)
       (t
        (save-window-excursion
          (incf  handled)
          (vc-next-action-on-file file 'verbose)))))
    (if (not (eq 0 handled))
        (dired-do-redisplay))
    (message (format "Tinydired: VC co: %s/%s handled, loaded %s"
                     handled count loaded))))

;;; ----------------------------------------------------------------------
;;; - This is vastly different than C-x v v in dired mode
;;;
(defun tinydired-marked-vc-ci (&optional arg &optional verb)
  "Check In all marked files and load them inside Emacs. Ignore ARG.
Do some heavy checking, before doing ci.
o  if file is not writable, skip over
o  if file is not in Emacs, load it first
o  if file is in Emacs, but read only, suppose no diffs
o  if file is in Emacs, check rcsdiff, --> do nothing if no diffs
o  if file is in Emacs, check rcsdiff, if file not saved, offer save

Notice, that this function enters `recursive-edit' if it thinks file should
be Checked In. Use \\[exit-recursive-edit] to get back to this function
and continue with rest of the files.

Recursive edit is shown with those [ ] marks in the modeline.
VERB print verbose messages.

Note

  There is plenty of messages for each file in marked, because
  used should know if the marked file couldn't be processed with ci.

  Marks are removed from handled files.

Bugs:

  This function automatically removes marks from files where user has
  used recursive edit. If user didn't ci the file, this program
  can't know that.

  Anyway, the mark is gone."
  (interactive "P")
  (let ((list          (tinydired-get-marked-files))
	(count         0)
	(handled       0)
	(loaded        0)
	fn
	buffer
	load                           ;flag
	diff-no-stat
	modify-stat
	read-stat
	vc-reg-stat)
    (ti::verb)
    (if (and (null vc-dired-mode)
             (y-or-n-p "Buffer must be in VC dired mode. Turn it on? "))
        (vc-dired-mode)
      (error "Aborted."))
    (dolist (file list)
      (setq fn          (file-name-nondirectory file)
            buffer      (get-file-buffer file)
            vc-reg-stat (vc-registered file)
            load        nil)
      (incf  count)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... possible load . .
      (when (and (null buffer)
		 vc-reg-stat
		 (file-writable-p file)  ; "-r--r--r--" , not ci'able file
		 (y-or-n-p (concat "file " fn " not in Emacs. Load? " )))
        (setq buffer (find-file-noselect file)
              load   t)
        (incf loaded))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  stat . .
      (when (setq buffer (get-file-buffer file))
        (save-excursion
	  (with-current-buffer
	      (setq modify-stat (buffer-modified-p)
		    read-stat   buffer-read-only)
	    ;;  Can't ask stat if not in VC control
	    (and vc-reg-stat
		 (setq diff-no-stat
		       (vc-workfile-unchanged-p file 'get-diffs))))))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... set diff stat . .
      (cond
       ((and buffer
             (null vc-reg-stat))
        nil)
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((and buffer
             read-stat)                 ;is file in CheckOut state ?
        (ti::read-char-safe-until
         (concat fn " in VC, but _buffer_ is read-only. (ok)" ))
        (and load
             (y-or-n-p (concat "Unload " fn " ? "))
             (kill-buffer buffer)
             (decf loaded)))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((and buffer
             vc-reg-stat
             diff-no-stat
             (null modify-stat))
        (if (null load)
            (ti::read-char-safe-until
             (concat fn " contains NO changes. (ok) "))
          (if (y-or-n-p (concat fn " contains NO changes, unload NOW? "))
              (kill-buffer buffer))))
       ((and buffer
             vc-reg-stat)
        (incf handled)
        (save-excursion
          (save-window-excursion
            (unwind-protect
                (progn
                  (tinydired-mark-re (concat "[0-9] +" file) 'unmark)
                  (pop-to-buffer buffer)
                  (call-interactively 'vc-next-action)

                  (ad-enable-advice 'vc-finish-logentry
                                    'after 'tinydired-recursive-edit)
                  (ad-activate 'vc-finish-logentry)
                  (recursive-edit)
                  (message
                   (substitute-command-keys
                    (concat
                     "Use \\[exit-recursive-edit] to abort action."
                     "to next file")))
                  (sleep-for 1))
              (ad-disable-advice  'vc-finish-logentry
                                  'after 'tinydired-recursive-edit)
              (ad-activate 'vc-finish-logentry))))
        (ti::pmin)                      ;remove file after VC
        (if (re-search-forward fn nil t)
            (dired-unmark 1))))
      ;; ........................................................ loop ...
      nil)
    (if verb
        (message (format "VC ci: %s/%s handled, loaded %s"
                         handled count loaded)))))

;;}}}
;;{{{ code: advice

;;; ----------------------------------------------------------------------
;;; - Until someone fixes dired to honor the  backup-file-name-p
;;;   this stays replaced...
;;; - This is copy from 19.30 dired.el
;;;
(defadvice dired-flag-backup-files (around tdd dis)
  "Replace original function.
This function honours the `backup-file-name-p' function and
additionally flag files that match regexp `tinydired--backup-file-regexp'."
  (let ((dired-marker-char      (if unflag-p ?\040 dired-del-marker))
        (re                     tinydired--backup-file-regexp)
        file)
    (dired-mark-if
     (progn
       (beginning-of-line)
       (when (not (looking-at dired-re-dir))
         (setq file  (dired-get-filename t t))
         (if (stringp file)
             (or (backup-file-name-p file)
                 (and re
                      (string-match re file))))))
     "backup file")))

;;; ----------------------------------------------------------------------
;;;
(defadvice vc-finish-logentry (after tinydired-recursive-edit dis)
  "When this advice is enabled, it call `exit-recursive-edit'.
Only if f recursive edit is in effect.

This advice is controlled by function `tinydired-marked-vc-ci' and it is never
enabled outside of that function."
  (ignore-errors (exit-recursive-edit)))

;;; ----------------------------------------------------------------------
;;; - When using "f" it loads directory to same buffer.
;;; - only kills the Dired buffer if a prefix arg is given
;;;
(defadvice dired-find-file (around tinydired-kill-dired-buffer last dis)
  "If a prefix argument is given, kill the Dired buffer.

If you have loaded dired-x and it contains variable
`dired-find-subdir', this advice does nothing."
  (let ((dired-buffer (current-buffer)))
    (prog1
        ad-do-it
      (if (and (eq major-mode 'dired-mode)
               (not (eq (current-buffer) dired-buffer))
               (or current-prefix-arg
                   tinydired--use-only-one-buffer-flag)
               (or (not (featurep 'dired-x)) ;not loaded
                   (and (featurep 'dired-x) ;is loaded, but this var not exist
                        (not (boundp 'dired-find-subdir)))))
          (kill-buffer dired-buffer)))))

;;}}}
;;{{{ code: store

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydired-store-get-string ()
  "Return content of storage as string."
  (ti::list-to-string tinydired--file-store))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-show ()
  "Show filenames in storage."
  (interactive)
  (if (null tinydired--file-store)
      (message "Tinydired: Store is empty.")
    (funcall tinydired--show-storage-function tinydired--file-store)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-filename ()
  "Save current filename into variable."
  (interactive)
  (let ((file (tinydired-get-filename)))
    (if (member file tinydired--file-store)
        (message "TinyDireds: %s already in storage." file)
      (push  file tinydired--file-store) file)
    (if (interactive-p)
        (tinydired-store-show))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-delete-filename ()
  "Remove filename from store."
  (interactive)
  (let ((file (tinydired-get-filename)))
    (setq tinydired--file-store (delete file tinydired--file-store))
    (if (interactive-p)
        (message "Tinydired: %s" (ti::list-to-string tinydired--file-store)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-clear ()
  "Clear variable holding files."
  (interactive)
  (setq tinydired--file-store nil)
  (if (interactive-p)
      (message "Tinydired: Storage cleared.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-remove-file ()
  "Delete current filename from storage."
  (interactive)
  (let ((file   (tinydired-get-filename))
	(verb   (interactive-p))
	(store  tinydired--file-store)
	list)
    (if (null store)
        (if verb (message "Tinydired: Storage is empty."))
      (dolist (x store)
        (if (not (string= x file))
            (push x list)) )
      (setq tinydired--file-store list)
      (if verb
          (tinydired-store-show)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-delete-marked ()
  "Delete marked files from store."
  (interactive)
  (tinydired-store-add-marked 'delete (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-add-marked (&optional delete verb)
  "Add marked files into store. No duplicates are inserted.
If parameter DELETE is non-nil, removes marked files from store. VERB."
  (interactive)
  (let ((list    tinydired--file-store)
	(marked  (tinydired-get-marked-files-no-dir)))
    (ti::verb)
    (if (null delete)
        (dolist (x marked)
          (if (not (member x list))
              (push x tinydired--file-store)))
      (dolist (x marked)
        (if (member x list)
            (setq tinydired--file-store
                  (delete x tinydired--file-store)))))
    (if verb
        (tinydired-store-show))))

;;}}}
;;{{{ code: ange ftp

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-store-ftp-message (&rest args)
  "Show Message from ange ftp after finishing the mget. Ange ARGS."
  (message "Tinydired: Store, ftp completed.") (sleep-for 1))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-back-to-dired-buffer ()
  "Switch back to dired buffer, which is associated with ange-ftp buffer.
If no such buffer is found, do nothing."
  (interactive)
  (let ((buffer (ti::buffer-find-ange-to-dired-buffer)))
    (if buffer
        (pop-to-buffer (car buffer))
      (message "Tinydired: No dired buffer found."))))

;;; ----------------------------------------------------------------------
;;; - If I have 2-3 dired ftp sessions and I want to close the current
;;;   one, this is a handy command.
;;;
;;;###autoload
(defun tinydired-kill-dired-and-ange-session (&optional verb)
  "Kill the current dired buffer and possible ange-ftp buffer. VERB.
This is like `dired-delete-and-exit'."
  (interactive)
  (let ((buffer (tinydired-ange-ftp-buffer-for-this-dired)))
    (ti::verb)
    (if buffer
        (kill-buffer buffer))
    (kill-buffer (current-buffer))
    (if verb
        (message
         (if buffer
             "Ange buffer killed too."
           "No ange buffer associated with dired.")))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-kill-all-ange-and-dired-buffers (&optional verb)
  "Kill all ange-ftp buffers _and_ all remote dired buffers. VERB."
  (interactive)
  (let ((ange  (ti::buffer-get-ange-buffer-list))
	(dired (ti::dolist-buffer-list
		(and (eq major-mode 'dired-mode)
		     (string-match tinydired--dired-directory-ange-regexp
				   dired-directory))))
	(ange-count  0)
	(dired-count 0))
    (ti::verb)
    (dolist (elt ange)
      (kill-buffer elt)
      (incf ange-count))
    (dolist (elt dired)
      (kill-buffer elt)
      (incf  dired-count))
    (if verb
        (message "Tinydired: Killed %s ange, %s dired buffers."
                 ange-count dired-count))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-kill-all-ange-buffers ()
  "Kill all ange-ftp process buffers.
If you want to kill one buffer at a time, use
`tinydired-switch-to-some-ange-ftp-buffer' to switch to individual buffer
and use \\[kill-buffer] to kill session.

This function is primarily used for cleanups. After a while
you may end up with many ftp session and it's nice if
you can get rid of them fast.

Don't worry about the dired buffers, Ange will automatically
create connection, if you use \"g\" -- rever-buffer, in a dired
that is associated with ange-ftp."
  (interactive)
  (let ((list  (ti::buffer-get-ange-buffer-list))
	(i     0))
    (dolist (elt list)
      (incf  i) (kill-buffer elt))
    (if (> i 0 )
        (message (concat "Tinydired: Ange buffers killed: " i))
      (message "Tinydired: No ange buffers found."))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydired-switch-to-some-ange-ftp-buffer ()
  "Gather all ange FTP buffers and offer completion menu.
If there is only one Ange buffer, switches to it without asking."
  (interactive)
  (let ((list (ti::buffer-get-ange-buffer-list))
	buffer
	go)
    (if (null list)
        (message "no Ange-ftp sessions at the moment.")
      (if (eq 1 (length list))
          (setq buffer (car list))
        (setq buffer
              (completing-read "go ange: " (ti::list-to-assoc-menu  list))))
      (if (setq go (get-buffer buffer))
          (switch-to-buffer go)
        (message (concat "No ange buffer: " buffer))))))

;;; ----------------------------------------------------------------------
;;; - This is handy, when you want to check that the mput went ok.
;;;
(defun tinydired-switch-to-mput-ange-ftp-buffer ()
  "Switch to ange buffer where last mput was made.
Does nothing if no mput were recorded or such ange buffer does not exist.

Binds local keys to ftp buffer

  C - c b               switch back to previous buffer

References:

  `tinydired--previous-buffer'
  `tinydired--mput-last-ftp'

Return

  nil                   no action taken.
  t"
  (interactive)
  (let ((file    tinydired--mput-last-ftp)
	(buffer  (current-buffer))
	list
	host
	ret)
    (cond
     ((null tinydired--mput-last-ftp)
      (message "Tinydired: Sorry, No mput information."))
     ((not (string-match "/.*@.*:" file))
      (message "Tinydired: Sorry, No ange reference in `tinydired--mput-last-ft'p"))
     (t
      ;;  This return 3 member list: SITE LOGIN DIRECTORY/FILE
      (setq list  (ange-ftp-ftp-name file)
            host  (nth 0 list))
      (setq tinydired--previous-buffer buffer)
      ;;  Try to find buffer , ange uses SITE name for buffer names
      ;;  *ftp omc@venus*
      (cond
       ((and list
             (setq buffer (car (ti::dolist-buffer-list
                                (string-match
                                 (concat "[*]ftp.*" (regexp-quote host))
                                 (buffer-name))
                                'temp-buffers))))
        (switch-to-buffer-other-window buffer)
        (ti::pmax)
        ;; Switching back to previous (b)uffer
        (local-set-key "\C-cb"
                       (function
                        (lambda ()
                          "TinyDired: mput ange, back to previous buffer"
                          (interactive)
                          (pop-to-buffer tinydired--previous-buffer))))
        (setq ret t))
       (t
        (message
         "Tinydired: Sorry, can't find ange buffer for `%s'" host)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-ange-ftp-buffer-for-this-dired (&optional file)
  "Return ange ftp buffer-name-string for current dired or FILE, or nil."
  (let (host
	buffer
	list)
    (or file
	(setq file (dired-get-filename)))
    ;;  This return 3 member list: SITE LOGIN DIRECTORY/FILE
    (setq list  (ange-ftp-ftp-name file)
          host  (nth 0 list))
    (when list                       ;This dired is not in remote site
      ;;  Remove that ange-ftp site information from the string.
      (setq tinydired--directory
            (ti::string-index-substring dired-directory ?: nil 'right))
      ;;  Try to find buffer , ange uses SITE name for buffer names
      ;;  *ftp omc@venus*
      (when list
        (setq buffer
              (car (ti::dolist-buffer-list
                    (string-match
                     (concat "[*]ftp.*" (regexp-quote host))
                     (buffer-name))
                    'temp-buffers)))
        (unless (get-buffer buffer)
          (setq buffer nil))))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinydired-switch-to-ange-ftp-buffer (&optional verb)
  "If the dired is ange ftp buffer, switch to the real ftp buffer. VERB.

Sets global
 `tinydired--directory'   filename for current line

Binds local keys in ftp buffer

 C - c af           insert files stored in current point
 C - c ad           insert directory name
 C - c ab           switch back to dired buffer"
  (interactive)
  (let (buffer
	dir)
    (ti::verb)
    ;;  1.  try normal ange ftp
    ;;  2.  did user used 'put' to remove site ?
    ;;
    (setq buffer (tinydired-ange-ftp-buffer-for-this-dired))
    (cond
     ((and (null buffer)
           tinydired--file-store
           tinydired--mput-last-ftp
           (null (tinydired-switch-to-mput-ange-ftp-buffer)))
      (if verb
          (message "Tinydired: can't locate associated ftp buffer.")))
     ((null buffer)
      (if verb
          (message "Tinydired: can't locate associated ftp buffer.")))
     (buffer
      (switch-to-buffer-other-window buffer)
      (set (make-local-variable 'tinydired--directory) dir)
      (ti::pmax)
      ;;  "f"  for file information
      (local-set-key
       "\C-caf"
       (function
        (lambda (&optional arg)
          "TinyDired: Inserts file storage string."
          (interactive "P")
          (setq arg (tinydired-store-get-string))
          (if (ti::nil-p arg)
              (message "Tinydired: No files in storage.")
            (insert (tinydired-store-get-string))))))
      ;; "d" for directory information
      (local-set-key
       "\C-cad"
       (function
        (lambda ()
          "TinyDired: Inserts dired's directory string."
          (interactive)
          (insert tinydired--directory))))
      ;; Switching back to dired (b)uffer
      (local-set-key
       "\C-cab"
       (function
        (lambda ()
          "TinyDired: Back to dired buffer"
          (interactive)
          (tinydired-back-to-dired-buffer))))))))

;;; ----------------------------------------------------------------------
;;; mget = multiple get
;;;
(defun tinydired-store-ftp-mget ()
  "Send command to ange to fetch all files in store."
  (interactive)
  (let ((files         tinydired--file-store)
	(down          tinydired--download-dir)
	(store         (tinydired-store-get-string))
	(ange          (ange-ftp-ftp-name dired-directory))
	to-dir
	host
	user
	dir)
    (cond
     ((null ange)
      (message "Tinydired: Can't find ftp process. Start one first."))
     ((ti::nil-p files)
      (message "Tinydired: No files in store."))
     (t
      (if (or (not
               (y-or-n-p (concat "Tinydired: really get: "
                                 ;;   Get nicer prompt
                                 (if (> (length store) 50)
                                     (concat (substring store 0 50 )
                                             "...")
                                   (concat store " ")))))
              (ti::nil-p
               (setq to-dir
                     ;;  Hack to read directory easily
                     (let ((default-directory down))
                       (call-interactively
                        (function
                         (lambda (dir)
                           (interactive "Ddownload dir: ")
                           dir)))))))
          (message "Tinydired: Cancelled.")
        ;; ................................................. then part ...
        ;; - First update the value, so that user gets the old selection
        (setq tinydired--download-dir to-dir)
        ;; Next, get all needed parameters
        (setq host  (nth 0 ange)
              user  (nth 1 ange)
              dir   (nth 2 ange)
              to-dir (expand-file-name to-dir))
        (ti::file-ange-file-handle
         'get user host dir to-dir files))))))

;;; ----------------------------------------------------------------------
;;; - remember to be in DIRED before you call this
;;; mput = multiple put
;;;
(defun tinydired-store-ftp-mput (ange-ref-to)
  "Send all files in store to remote site ANGE-REF-TO."
  (interactive
   (list
    (completing-read
     "mput site: "
     (ti::list-to-assoc-menu tinydired--mput-sites)
     nil nil tinydired--mput-last-ftp
     'tinydired--mput-history)))
  (if (null dired-directory)
      (error "Tinydired: Must execute command in dired buffer."))
  ;;    Record the site name where the mput was made
  (setq tinydired--mput-last-ftp ange-ref-to)
  (let ((files         tinydired--file-store)
	(store         (tinydired-store-get-string))
	(dir           dired-directory)
	ange
	to-dir
	host
	user)
    ;;  If user is in remote dired buffer, signal error
    ;;  We don't support this. At least not now.
    ;;
    (if (string-match "@" dired-directory)
        (error "Tinydired: sorry, load files first to your site."))
    (if (not (ti::nil-p ange-ref-to))
        (setq ange   (ange-ftp-ftp-name ange-ref-to) ;crack it
              host   (nth 0 ange)
              user   (nth 1 ange)
              to-dir (nth 2 ange)))
    (cond
     ((ti::nil-p ange-ref-to)
      (message "Tinydired: No site given"))
     ((ti::nil-p files)
      (message "Tinydired: No files in store."))
     ((ti::nil-p to-dir)
      (message "Tinydired: No destination download directory given"))
     (t
      (if (not (y-or-n-p (concat "Put " host ": "
                                 ;;   Get nicer prompt
                                 (if (> (length store) 50)
                                     (concat (substring store 0 50)
                                             "...")
                                   (concat store " ")))))
          (message "Tinydired: Cancelled.")
        ;; ................................................. then part ...
        ;; (mode user host dir lcd file-list &optional not-bg msg-func)
        (ti::file-ange-file-handle 'put user host to-dir dir files))))))

;;}}}

(provide   'tinydired)

(tinydired-install)
(run-hooks 'tinydired--load-hook)

;;; tinydired.el ends here
