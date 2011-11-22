;;; tinyload.el --- Load set of packages when Emacs is idle (lazy load)

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
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
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file. Move all your `require' commands into
;; the load list.
;;
;;     (setq tinyload--load-list '("package" "package" ...))
;;     (require 'tinyload)
;;
;; TinyLoad can't be autoloaded, because it installs an idle-timer
;; function.
;;
;; See examples at the end of file how do I utilize this package in full.
;; If you have any questions, use 'submit' function. In case of error
;; or misbehavior, turn on the debug and send the debug results
;; From the *Messages* buffer and describe what was happening
;;
;;      M-x tinyload-debug-toggle

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Jul 1997
;;
;;      While it is possible to arrange Emacs `rc' (start-up) files to use
;;      all possible and imaginable autoloads, there are still packages
;;      that can't be autoloaded due to their setup nature or other
;;      behavior: `require' commands are necessary in `.emacs' in order
;;      to use those modules. This means that for every `require' command,
;;      the Emacs startup slows remarkably. Experienced Emacs users have
;;      very complex boot configurations, so waiting minutes for Emacs
;;      startup screen to appear is quite frustrating.
;;
;;      The described situation gave birth to this package. Now the emacs
;;      is ready to use within few seconds.
;;
;;      What this package does, is, that it caches the load requests and
;;      executes them when it thinks there is free time. Instead of setting
;;      up all at once on startup, the emacs configuration is built piece
;;      by piece, until the whole 100% configuration is there.
;;
;;      The benefit is that Emacs starts instantly, and when it is
;;      idle, the remaining packages, that you wanted to be
;;      available in your daily Emacs session, are loaded.
;;
;;  Overview of features
;;
;;      o   Delayed (Lazy) loading of packages (at some later time); after
;;          15 seconds of idle time, remaining files are loaded one by one.
;;      o   You no longer have to use `require' in your .emacs, instead,
;;          you define `tinyload--load-list' where you put the requests.
;;      o   Your .emacs starts faster when the extra `require' and
;;          `load' commands be moved to load list.
;;
;;      If you're a first time Emacs user or if you consider lisp
;;      difficult, have a look at simpler setup than what is described
;;      below from C-h v `tinyload--load-file'. The idea is that you
;;      tell the configuration file which lists packages that you
;;      want to load in format:
;;
;;          PACKAGE CONFIG-WORD
;;
;;      The CONFIG-WORD should be self explanatory: it instructs in which
;;      OS and in which Emacs flavor the package is loaded. Here isa
;;      sample: reportmail is only loaded under win32 and XEmacs.
;;
;;          paren
;;          autorevert win32
;;          gnus.el
;;          reportmail  win32-xemacs
;;
;;      Another easy interface is to use functions:
;;
;;          `tinyload-load-list-add-function'
;;          `tinyload-load-list-add-package'
;;          `tinyload-load-list-delete-function'
;;          `tinyload-load-list-delete-package'
;;
;;  First user notice
;;
;;      When you use this package for the first time, you may feel
;;      uncomfortable with the amount of messages you see displayed on
;;      the echo area. And if you're in echo-area prompt (e.g. after `C-x'
;;      `C-f') those messages may disturb the echo area prompt.
;;
;;      Just don't panic. Move your cursor key to the left (C-a)
;;      or start typing and the load will be interrupted. As long as
;;      there is activity in your Emacs the load will not happen.
;;
;;      The messages that are displayed in the echo area are important,
;;      because they get stored in *Messages* buffers and you can take a
;;      look if anything strange happened. Like if some package couldn't be
;;      loaded at all. Pay attention to *fatal* messages.
;;
;;  Messages in *Message* buffer
;;
;;      There are different types of messages
;;
;;          TinyLoad: fdb ok      (10% 1/10)                         [1]
;;          TinyLoad: elp 'noerr! (20% 2/10)                         [2]
;;          TinyLoad: [ERROR] loading  ~/elisp/rc/emacs-rc-init.el   [3]
;;
;;      o   [1] Package was loaded and the display shows some remaining
;;          statistics.
;;      o   [2] There was 'noerr parameter defined and the
;;          recent load of the package failed: perhaps it didn't exist along
;;          `load-path' or there was other problem in the package itself.
;;      o   [3] When file was loaded, some error happened. You
;;          should study this file by hand and spot the problem manually.
;;          Be sure that the syntax of the file is correct.
;;
;;      In addition to these basic messages, there are some internal
;;      messages that do not concern regular user, only the maintainer.
;;      When TinyLoad wakes up, you might see following message
;;
;;          Tinyload: timer expired; invoking load process..[busy;stop;N]
;;                                                           |    |    |
;;                                        user activity status    |    |
;;                                                  Continue status    |
;;                                    Busy count; and deadlock indicator
;;
;;      Which simply means that Emacs called the loader function and
;;      because *Continue* *status* was nil, user did nothing at the time
;;      of invocation. If the message [busy;stop;N] then user was doing
;;      something that weren't allowed to be interrupted. Usually this
;;      happens when cursor is in echo area e.g. after `C-x' `C-f'.
;;      If the cursor never leaves the echo area or if the busy situation
;;      continues for a certain period of time, the program automatically
;;      clears the busy signal and continues loading. You should not see
;;      infinite [busy;stop.N] messages. If you really see 10 such messages,
;;      then contact the author: there must be an unresolved deadlock and
;;      a bug in the program.
;;
;;      When the `tinyload--load-list' has been handled, the loader process
;;      terminates itself. The following message tells that the process has
;;      ceased to exist. If you want to start reading the list again,
;;      call `M-x' `tinyload-install'.
;;
;;          TinyLoad: Loader process terminated.
;;
;;  Tutorial
;;
;;      Let's supposes your emacs startup consists of following `rc' files
;;      The name `rc' comes from Unix resource files, like
;;      $HOME/.bashrc, $HOME/.cshrc ...
;;
;;          emacs-rc-main.el     -- the main load controller
;;          emacs-rc-path.el     -- settings `auto-mode-alist' etc.
;;          emacs-rc-bup.el      -- Backup settings
;;          emacs-rc-set.el      -- Emacs variable settings
;;
;;          emacs-rc-keys.el     -- Keyboard customizations
;;          emacs-rc-font.el     -- Fonts and Font lock; face settings
;;          emacs-rc-hooks.el    -- All add-hook commands and mode settings.
;;          emacs-rc-ding.el     -- Gnus customizations (symlink to ~/.gnus)
;;          emacs-rc-pkg-std.el  -- Loading std Emacs packages and their setup
;;          emacs-rc-pkg-misc.el -- Non-std distrib, additional packages
;;          emacs-rc-tips.el     -- Tips (code samples) from the Usenet
;;          emacs-rc-mail.el     -- mail agent, Rmail, VM, message.el etc. setup
;;
;;      Now suppose your .emacs loads all these files like this
;;
;;          ;; $HOME/.emacs -- Emacs startup controller
;;
;;          (require 'cl)   ;; Tell location of startup files
;;          (pushnew "~/elisp/rc" load-path :test 'string=)
;;
;;          (require 'emacs-rc-path)
;;          (require 'emacs-rc-bup)
;;          (require 'emacs-rc-set)
;;          (load "emacs-rc-keys.el")
;;          (require 'emacs-rc-font)
;;          (load "emacs-rc-hooks")
;;          (load "emacs-rc-ding")
;;          (load "emacs-rc-pkg-std")
;;          (load "emacs-rc-pkg-misc")
;;          (load "emacs-rc-tips")
;;          (add-hook 'mail-mode-hook '(lambda () (require 'emacs-rc-mail)))
;;
;;          ;; End of file $HOME/.emacs
;;
;;      The reason why there may be both `load' and `require' commands
;;      may be that you frequently make updates and changes to some of your
;;      start-up files. Like if you frequently update Setting for Gnus,
;;      and you want to reload your settings, the (load "emacs-rc-ding")
;;      is executed again. If you used `require' the new settings would not
;;      have been loaded. (See explanation of `load' and `require' from the
;;      Emacs info manual). So, to re-cap, if you would call:
;;
;;          M-x load-file ~/.emacs
;;
;;      Only the `load' commands' files would be loaded again. All the
;;      `require' files would have been skipped, because the `rc' resource
;;      features had already been defined.
;;
;;      Now, loading all these files, either with `require' or `load',
;;      takes too much time when you start Emacs. After some rearrangements
;;      you can put the delayed loading into use:
;;
;;          ;; $HOME/.emacs -- Emacs startup controller
;;
;;          (require 'cl)   ;; Tell location of startup files
;;          (pushnew "~/elisp/rc" load-path :test 'string=)
;;
;;          ;; Have these minimum features immediately available
;;
;;          (require 'emacs-rc-path)
;;          (require 'emacs-rc-bup)
;;          (require 'emacs-rc-set)
;;          (load "emacs-rc-keys.el")
;;
;;          ;;  Load this setup only when mail composing is started
;;
;;          (add-hook 'mail-mode-hook '(lambda () (require 'emacs-rc-mail)))
;;
;;          ;;  ........................................... lazy loading ....
;;          ;;  We can afford to load these later
;;
;;          (setq tinyload--load-list
;;            '(("emacs-rc-font")
;;              ("emacs-rc-hooks")
;;              ("emacs-rc-ding")
;;              ("emacs-rc-pkg-std")
;;              ("emacs-rc-pkg-misc")
;;              ("emacs-rc-tips")
;;              ("emacs-rc-mail")))
;;
;;          (require 'tinyload)
;;          ;; End of file $HOME/.emacs
;;
;;      When Emacs load this startup, only the most important files are
;;      loaded saving the start time considerably. After `tinyload' finds
;;      that your Emacs is idle it starts loading all the rest of the
;;      packages you defined in the `tinyload--load-list'. For more complex
;;      setup, refer to end of tinyload.el source file, where you can
;;      find a complete example setup.
;;
;;        NOTE: Please pay attention to one detail above. The `emacs-rc-mail'
;;        will be loaded from load list _and_ it will be loaded when
;;        you call M-x `mail'. Do you believe there is redundancy? The
;;        idea is that you may call M-x `mail' way before the TinyLoad
;;        reaches that file in its load list and the hook guarantees that
;;        you get the setup at mail invoke time.
;;
;;        But it may be the other way round: TinyLoad has already loaded
;;        the mail setup for you and thus invoking M-x `mail' is fast,
;;        because there is nothing to load any more.
;;
;;        Similar things you should do to GNUS, VM, RMAIL and others that
;;        you call and whose setup you want to have immediately available
;;
;;  Delayed loading, require and autoload
;;
;;      Above you saw how to load your Emacs `rc' files. But the delayed
;;      loading is not only suitable for those. It also helps you to load
;;      files, that can't be autoloaded.
;;
;;        If you can arrange loading a packages with `autoload' command,
;;        do that. Never put `require' or direct `load' command into your
;;        Emacs `rc' file, because load commands eat start time.
;;
;;      Packages usually explain in the *installation* section two ways
;;      how to load them: here is an example from tinytab.el
;;
;;          (require 'tinytab)
;;
;;          or use this; your .emacs is read quicker
;;
;;          (autoload 'tinytab-mode              "tinytab" "" t)
;;          (autoload 'tinytab-return-key-toggle "tinytab" "" t)
;;
;;      The first way forces loading the whole file (takes time); and
;;      the latter only tells that the package's functions
;;      `tinytab-return-key-toggle' and `tinytab-mode' exists. If you
;;      happen to call those functions, _only_ then the package gets
;;      loaded. The big difference here is that when you put the
;;      latter in your Emacs rc file, Emacs reads `autoload' statements much
;;      faster than the `require' command.
;;
;;      It is not always possible arrange to load package with autoloads,
;;      because the package may behave so that in order to get the features
;;      installed, package must do the setup by itself: you can't do it
;;      yourself. Here are few packages that can't be autoloaded:
;;
;;          crypt++     -- crypt(1) support.
;;          tinymy      -- collection of utilities
;;          fa-extras   -- Filling extras
;;
;;      When you would normally include a `require' command for these
;;      into your Emacs `rc' file, you can now move the packages to load
;;      list and keep only autoloads in the `rc' files.
;;
;;          ;; Old rc file
;;
;;          (autoload ....
;;          (autoload ....
;;          (require 'fa-extras)
;;          (autoload ....
;;
;;          ;; New rc file
;;
;;          (autoload ....
;;          (autoload ....
;;          (autoload ....
;;
;;      And the missing `require' entry has been moved to
;;      `tinyload--load-list'.
;;
;;  Use separate rc file for load definitions
;;
;;      It may be good idea to make a separate `rc' file that only has
;;      the load list definition and a call to tinyload.el, like this:
;;
;;          ;; emacs-rc-tinyload.el -- load definitions for tinyload.el
;;          ;;
;;          ;; If you compile this file, `defconst' shuts up Byte Compiler
;;
;;          (defconst tinyload--load-list
;;            '(...
;;              ...))
;;          (require 'tinyload)
;;          (provide 'emacs-rc-tinyload)
;;          ;; End of file
;;
;;      And then you add following call to your *$HOME/.emacs*, to the end
;;      of the file, although the place really doesn't matter.
;;
;;          (require 'emacs-rc-tinyload)
;;
;;  Used timer process
;;
;;      A normal timer process is used to load the packages from the load
;;      list. The timer awakens at regular intervals and loads one package at
;;      a time: more packages are not loaded if there was input pending at
;;      the time of previous load. The load messages are recorded to
;;      *Messages* buffer. In old Emacs releases this buffer does not
;;      exist; but it will be created for you.
;;
;;  About implementation
;;
;;      When `tinyload--load-list' is set, the value of the variable is
;;      saved under property `original'. When the idle timer runs, the list
;;      is read from the beginning and each package at a time is loaded.
;;      The last unloaded package position is saved under property 'pos.
;;
;;      The situation looks like this:
;;
;;          tinyload--load-list 'original   --> (list) original contents
;;          tinyload--load-list 'pos        --> (nth nbr) next package to load.
;;
;;      If your do something in your emacs while the list is being looped,
;;      or when the loader function is about to be called, that interrupts
;;      the work. Next time the timer functions run runs, happens:
;;
;;      o   It checks if the current list matches `original'. Yes, means that
;;          the list hasn't been modified. No, means that it should examine
;;          the list all aver again, starting from the beginning.
;;      o   If the list was original, it picks the `pos' point and
;;          loads all the remaining packages, one at a time until it
;;          sees activity.
;;      o   If there is nothing to load, the `pos' points to the end
;;          of list. Function returns immediately and does nothing.
;;          At this point the loader process terminates itself by
;;          clearing the idle timer list.
;;
;;  Force loading
;;
;;      There is also property `fatal-list' which contains entries that
;;      couldn't be loaded. The list is updated while the loading takes
;;      place. If you examine the failed files and make corrections;
;;      you can try to reload the whole load list again if you call
;;
;;          C-u M-x tinyload-loader-process
;;
;;  Special features
;;
;;      In case you want to load all packages and leave nothing in
;;      autoload state, add this code to your Emacs startup file. When the
;;      loader process exits, it will check all Emacs functions for autoload
;;      definitions and load those packages as well.
;;
;;          (add-hook 'tinyload--process-uninstall-hook
;;                    'tinyload-autoload-function-load)
;;
;;  Restart and cancel
;;
;;      If you want to restart the evaluation of load list, call `M-x'
;;      `tinyload-install', which will install the package again by removing
;;      old processes and resetting counters. To stop the loader process
;;      permanently, call `tinyload-cancel'.
;;
;;  Bugs
;;
;;      Every effort has been made to check that Emacs has no activity
;;      before the package is loaded at the background. A series of
;;      `sit-for' `input-pending-p' and more obscure mini-buffer
;;      checks have been run before the load kicks in. If a package
;;      still gets loaded while you are doing something, please send
;;      a suggestion how that event could be detected so that the load
;;      wouldn't interrupt you again. Unfortunately, there is no single
;;      solution to notice all user activity in a reliable way.
;;
;;      Despite of the efforts, an unlucky moment may cause loading the
;;      package, when it would not have been appropriate. Please hang on
;;      and wait for the load to finish, you're will regain control soon.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ require

;;; ......................................................... &require ...

(require 'tinylibm)

;; #todo: Does Xemacs reportmail.el define this function too?
;; #todo: 2000-11 Emacs 2?.7 seems to include reportmail.el

(eval-and-compile
  (autoload 'display-time "time"))

(ti::package-defgroup-tiny TinyLoad tinyload-- extensions
  "Overview of features
        o  Delayed loading of packages (in some later time)
        o  You no longer have to use `require' in your .emacs, instead,
           you can put the package to `tinyload--load-list' and have it loaded
           when Emacs is idle.
        o  Your .emacs starts faster when the `require' commands are out.")

;;}}}

;;{{{ Hooks

;;; ......................................................... &v-hooks ...

(defcustom tinyload--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyLoad)

(defcustom tinyload--process-install-hook nil
  "*Hook run when `tinyload-install' is called."
  :type  'hook
  :group 'TinyLoad)

(defcustom tinyload--process-uninstall-hook nil
  "*Hook run when `tinyload-cancel' is called."
  :type  'hook
  :group 'TinyLoad)

;;}}}
;;{{{ variables: public

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyload--idle-time 20
  "*When Emacs is this many seconds idle, start load process.
Warning: Do not set this value below 4 seconds, because the previous
call must complete before the timer process is called again. Some
big packages may take a while to load."
  :type  'integer
  :group 'TinyLoad)

(defcustom tinyload--init-time 2
  "*Time in seconds to wait before activating loader for the first time.
This is the initial time it takes before the loader process starts for the
first time. The default is 2 seconds."
  :type  'integer
  :group 'TinyLoad)

(defcustom tinyload--wait-next-load 0.5
  "*Time in seconds in load process to see if there is user activity.
This is the time loader process waits before it tries to load next package;
a time gap where any activity cancels the process from continuing
if user types something in Emacs.
Suggested value range: 0.2 - 1.5 seconds."
  :type   'integer
  :group  'TinyLoad)

(defcustom tinyload--load-file nil
  "*File to liast packages to load.
If you set this variable, you can't use `tinyload--load-list', because
`tinyload--load-list' is initalized from this file's content.

This variable is menat for simpler load control than what
could be done in lisp level with `tinyload--load-list'.

The format of the FILE is simple:

- Comments in file start with semicolon (;)
- Added file to load in one line, next to next line and so on
- Add check configuration-word right after the filename.
  This must be a SINGLE word.

An example:

    ;; tinyload configuration file start

    paren
    autorevert win32
    gnus.el
    reportmail  win32-xemacs

    ;; tinyload configuration file end

The above file's configuration words above are \"win32\" and
\"win32-xemacs\", where e.g. package autorevert will only be loaded under
win32. Similarly reportmail package is only loaded if current OS is win32
and Emacs flavor is XEmacs.

The recognized configuration tokens, that must form a single word, are:

    win32 emacs xemacs"
  :type   'file
  :group  'TinyLoad)

(defcustom tinyload--load-list nil
  "*List of packages to load when emacs has been idle.
The idle time in seconds to load packages is defined in `tinyload--idle-time'.

References:

    You can also manipulate this list with following functions:
    `tinyload-load-list-add-function'
    `tinyload-load-list-add-package'
    `tinyload-load-list-delete-function'
    `tinyload-load-list-delete-package'

Format:

  '((PACKAGE-OF-FILE [FEATURE-SYM] [NOERR] [NOMSG] [FORM-BEFORE] [FORM-AFTER])
     ...)

  PACKAGE-OR-FILE can be any valid `load' command filename parameter:

        \"package\"
        \"package.el\"
        \"package.elc\"
        \"~/elisp/package.el\"

  You must provide FEATURE-SYM if the package provides different feature than
  the package name; e.g. entry (\"~/rc/emacs-rc-my\" 'rc-my) says; that you
  want to do (load \"~/rc/emacs-rc-my\") only if (featurep 'rc-my) returns false.

  [NOERR] is optional and parameter for `load' command
  [NOMSG] is optional and parameter for `load' command

  [FORM-BEFORE] is evaluated before load command.
  [FORM-AFTER]  is evaluated after load command.

Note:

  Nil entries in this table are skipped. This allows you to construct
  dynamic load list entry like this:

      (setq tinyload--load-list
        (list
         (if (and (ti::emacs-p)
                  (= 28 emacs-minor-version))
             (list \"~/rc/emacs-rc-19.28\" 'rc-28))))

  The `tinyload--load-list' would be '(nil) in non-19.28 Emacs

Example:

  (setq tinyload--load-list
    '(\"ffap.el\"
      \"tinylibmail.el\"))"
  :type  '(repeat sexp)
  :group 'TinyLoad)

;;}}}
;;{{{ variables: private

;;; ....................................................... &v-private ...
;;; Private variables

(defvar tinyload--timer-elt nil
  "The timer process if used in current Emacs.")

(defvar tinyload--process-busy-p nil
  "When load process is loading something this flag is non-nil.
This prevents invoking multiple load processes.")

;;}}}
;;{{{ installation

(eval-and-compile (ti::macrof-debug-standard "tinyload" "--"))

;;; --------------------------------------------------------- &install ---
;;;
;;;###autoload
(defun tinyload-install (&optional remove)
  "Install package or REMOVE.
This function removes any previous TinyLoad timer process and resets
the list pointer to 0."
  (interactive "P")
  (tinyload-config-file-load-default)
  ;;  Kill old process(es)
  (ti::compat-timer-cancel-function 'tinyload-loader-process)
  (setq tinyload--timer-elt nil)
  (cond
   ((or remove
        (null tinyload--load-list))
    (let ((str (concat
		"TinyLoad: Loader process terminated."
		(if (null tinyload--load-list)
		    " `tinyload--load-list' is empty."
		  ""))))
      (tinyload-message str))
    (tinyload-debug "Tinyload: Install, stopped. HOOK"
                    tinyload--process-uninstall-hook)
    (run-hooks 'tinyload--process-uninstall-hook))
   (t
    (put 'tinyload--load-list 'pos 0)
    (put 'tinyload--load-list 'fatal-list nil)
    ;;  Put startup info into *Messages*" buffer
    (tinyload-message
     (format
      (concat "TinyLoad: Started with %d items in load list."
              " Init %d and interval %d seconds.")
      (length tinyload--load-list)
      tinyload--init-time
      tinyload--idle-time))
    (tinyload-debug "Tinyload: Install, started. HOOK"
                    tinyload--process-install-hook)
    (display-time)
    (setq tinyload--timer-elt
          (run-at-time
           (format "%d sec" tinyload--init-time)
           tinyload--idle-time
           'tinyload-loader-process))
    (tinyload-debug "tinyload-install: `run-at-time' timer elt"
                    tinyload--timer-elt)
    (run-hooks 'tinyload--process-install-hook)))
  (setq tinyload--process-busy-p nil)
  tinyload--timer-elt)

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-cancel ()
  "Kill the loaded process and stop loading.
To start loader process, call \\[tinyload-install]."
  (interactive)
  (tinyload-install 'remove))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-start ()
  "Start loader process. This function is synonym to ´tinyload-install'"
  (interactive)
  (tinyload-install))

;;}}}
;;{{{ support functions

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyload-autoload-function-load (&optional verb)
  "Load all autoloaded functions. VERB."
  (interactive)
  (ti::verb)
  (let* ((fid "tinyload-autoload-function-load:")
         (funcs (ti::system-autoload-function-list))
         (load (when funcs
                 (ti::system-autoload-function-file-list funcs)))
         (count 0)
         str)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinyload-debug
     (format "Tinyload: [debug] %s FUNCTIONS %s FILES %s"
             fid
             (prin1-to-string funcs)
             (prin1-to-string load)))
    (dolist (file load)
      (condition-case err
          (load file)
        (error
         (setq str (format
                    "Tinyload: autoload function load fail %s %s "
                    file (prin1-to-string err)))
         (message str)
         (tinyload-debug str)))
      (incf count)
      (when verb
        (message "Tinyload: autoloading clean %d/%d %s"
                 count (length load) file)))
    load))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-feature-p (pkg &optional feature)
  "Check if feature has been loaded.
See PKG and FEATURE from `tinyload--load-list'"
  ;;  User didn't give us separate feature name, construct
  ;;  one from package name ~/elisp/test.el --> "test"
  (let* ((fid "tinyload-feature-p")
         status)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinyload-debug
     (format "TinyLoad: [debug] %s (a) PACKAGE [%s] FEATURE [%s]"
             fid (prin1-to-string pkg) (prin1-to-string feature)))
    ;;  Make feature name out of the package name if
    ;;  it was not given  gnus.el -> 'gnus
    (when (and (null feature)
               (stringp pkg))
      (setq feature (file-name-nondirectory pkg))
      (if (and (string-match "^\\(.+\\)\\.el" feature)
               (match-end 1))
          (setq feature (match-string 1 feature))))
    (setq status
          (cond
           ((and (not (null feature))
                 (symbolp feature)
                 (featurep feature))
            'symbol)
           ((and (stringp feature)
                 (intern-soft feature)
                 (featurep (intern-soft feature)))
            'intern)
           (t nil)))
    (tinyload-debug
     (format "TinyLoad: [debug] %s (b) PACKAGE [%s] FEATURE [%s] stat %s"
             fid
             (prin1-to-string pkg)
             (prin1-to-string feature)
             (prin1-to-string status)))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-message (msg)
  "Display MSG and put it to *Messages* Buffer."
  (if (string-match "%" msg)
      (setq msg (subst-char-with-string msg ?% "%%")))
  (tinyload-debug msg)
  (message msg)
  ;;  Old releases don't have this buffer; generate one.
  (when (and (ti::emacs-p)
             (string-match "19.2[0-9]" emacs-version))
    (with-current-buffer (get-buffer-create "*Messages*")
      (ti::pmax) (insert msg "\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-status ()
  "Print status. How many packages are left in load list."
  (interactive)
  (if (null tinyload--timer-elt)
      (message "TinyLoad process is not alive any more.")
    (message "Position %s/%s in load list."
             (get 'tinyload--load-list 'pos)
             (length tinyload--load-list))))

;;}}}
;;{{{ Load list manipulation support functions

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-search-elt (search position)
  "SEARCH item in `tinyload--load-list' by checking POSITION.

package feature noerr nomsg before after
0       1       2     3     4      5

The SEARCH item is checked with `equal' function."
  (let (picked)
    (dolist (elt tinyload--load-list)
      ;;  package feature noerr nomsg before after
      (setq picked (nth position elt))
      (when (equal picked search)
        (return elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-search-function (function)
  "Search FUNCTION in `tinyload--load-list'."
  (tinyload-load-list-search-elt function 4))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-search-package (package)
  "Search PACKAGE in `tinyload--load-list'."
  (tinyload-load-list-search-elt package 0))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-add-function (function)
  "Add FUNCTION to `tinyload--load-list'.
This function places a null entry to the laod list, so that only the
load-before form is exected: it runs the FUNCTION."
  (let ((elt   (list "run-function-only" nil 'noerr 'nomsg function nil))
        (entry (tinyload-load-list-search-function function)))
    (unless entry
      (push elt tinyload--load-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-add-package (package &optional feature)
  "Add PACKAGE FEATURE with 'noerr 'nomsg attributes to `tinyload--load-list'."
  (let ((elt   (list package feature 'noerr 'nomsg))
        (entry (tinyload-load-list-search-package package)))
    (unless entry
      (push elt tinyload--load-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-delete-elt (elt)
  "Remove ELT from `tinyload--load-list'."
  (setq tinyload--load-list (delete elt tinyload--load-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-delete-function (function)
  "Remove FUNCTION from `tinyload--load-list'."
  (let ((entry (tinyload-load-list-search-function function)))
    (when entry
      (tinyload-load-list-delete-elt entry))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-list-delete-package (package)
  "Remove PACKAGE from `tinyload--load-list'."
  (let ((entry (tinyload-load-list-search-package package)))
    (when entry
      (tinyload-load-list-delete-elt entry))))

;;}}}
;;{{{ Config file interface

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-config-file-emacs-type-ok-p (string)
  "Test STRING for xemacs, emacs and win32."
  (if (null string)
      t
    (let ((emacs-ok 'not-tested)
	  (os-ok    'not-tested))
      (when (string-match "win32" string)
        (setq os-ok (ti::win32-p)))
      (when (string-match "emacs" string)
        (setq emacs-ok
              (or (and (string-match "xemacs" string)
                       (ti::xemacs-p))
                  (and (not (string-match "xemacs" string))
                       (string-match "emacs" string)
                       (ti::emacs-p)))))
      (and emacs-ok
           os-ok))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-config-file-parse ()
  "Parse entries ein configuration file and ignore comments.
File format is:

    ;; Comment
    ;; Another comment
    file win32-xemacs
    file.el emacs
    file.elc

    ;; End of file

In the above example, FILE means command \(load \"file\" 'noerr). You can
add additional .el or .elc extension to force loading uncompiled or
compiled version of the file.

The additional PARAMETER-WORD follows directly after the filename. It must
be only one word and you can separate different tests with dash(-). Valid
test names recognized are

    win32
    emacs
    xemacs

For example if line reads:

    file win32-xemacs

This means that package \"file\" if loaded only if current Emacs
flavor is XEmacs and the operating system is win32

Any empty lines, spaces and comment started with semicolon (;)
are ignored.

Return:

  Similar list than what is described for variable
  `tinyload--load-list'"
  (let ((fid "tinyload-config-file-parse")
	list
	test
	file)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (ti::pmin)
    (while (re-search-forward
            "^[ \t]*\\([^ ;\t\r\n]+\\)[ \t]*\\([^ ;\t\r\n]+\\)" nil t)
      (when (setq file (match-string 1))
        (setq test (match-string 2))
        (when (tinyload-config-file-emacs-type-ok-p test)
          (push (list file) list))))
    ;; Preserve read order
    (setq list (nreverse list))
    (tinyload-debug fid "RET" list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-config-file-load-1 (file)
  "Load configuration file and return list in format `tinyload--load-list'."
  (interactive "fTinyLoad configuration file: ")
  (with-temp-buffer
    (insert-file-contents file)
    (tinyload-config-file-parse)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-config-file-load-default ()
  "Load `tinyload--load-file' and return list in format `tinyload--load-list'."
  (let ((file tinyload--load-file))
    (tinyload-debug "tinyload-config-file-load-default"
                    "tinyload--load-file"
                    file)
    (cond
     ((not (stringp file))
      nil)
     ((not (file-exists-p tinyload--load-file))
      (message "Tinyload: tinyload--load-file does not exist %s"
               tinyload--load-file))
     (t
      (setq tinyload--load-list
            (tinyload-config-file-load-1 file))))))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-minibuffer-active-p ()
  "check if minibuffer is active."
  (if (fboundp 'active-minibuffer-window)
      (ti::funcall 'active-minibuffer-window)
    (eq (selected-window) (minibuffer-window))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-no-action ()
  "Check that Emacs is still."
  (and
   ;; (ti::no-action-in-progress-p 'timer) isn't working right
   (sit-for 0.2)
   (not cursor-in-echo-area)
   (not (tinyload-minibuffer-active-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-process-continue (&optional force)
  "Check if process is clear to continue and Emacs is not busy.
Return status '(continue no-action no-input)."
  (let ((fid "tinyload-process-continue")
	no-action
	no-input
	continue)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (setq no-action (tinyload-no-action)
          no-input  (null (input-pending-p))
          continue  (or force
                        (and no-input
                             no-action)))
    (tinyload-debug
     (format
      "TinyLoad: [debug] %s no-action: %s no-input: %s continue: %s busy: %s"
      fid
      (prin1-to-string no-action)
      (prin1-to-string no-input)
      (prin1-to-string continue)
      (if tinyload--process-busy-p
          "yes"
        "no")))

    (list continue no-action no-input)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-eval (form type)
  "Eval FORM. TYPE is string AFTER or BEFORE."
  (condition-case err
      (if form
          (eval form))
    (error
     (let ((str
	    (format "Tinyload: [ERROR] EVAL %s generated an error %s %s"
		    type
		    (prin1-to-string err)
		    (prin1-to-string form))))
       (message str)
       (tinyload-debug str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load (pkg noerr nomsg)
  "Load PKG with NOERR NOMSG. Return load status."
  (let (stat)
    (cond
     (noerr
      (condition-case data
          (setq stat (load pkg noerr nomsg))
        (error
         (message "TinyLoad: [%s] %s"
                  pkg
                  (prin1-to-string data))))
      (tinyload-debug "TinyLoad: 'noerr load %s: %s" pkg stat))
     (t
      (setq stat (ignore-errors (load pkg noerr nomsg)))))
    stat))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-failure (pkg elt)
  "Record PKG ELT failure to `tinyload--load-list'. Return failed-list."
  ;;  Record failed entries.
  (let ((failed-list (get 'tinyload--load-list 'failed-list)))
    (add-to-list 'failed-list elt)
    (put 'tinyload--load-list 'failed-list failed-list)
    (let ((str
           (format "TinyLoad: [ERROR] while loading %s" pkg)))
      (ding)
      (tinyload-debug str)
      (tinyload-message str))
    ;;  This will tell the path and put the message
    ;;  in *Message* buffers. It will also tell if
    ;;  it was .elc or .el that had troubles.
    ;;  >> FOR DEBUG PURPOSES
    (ignore-errors (locate-library pkg))
    failed-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-initialize ()
  "Initialise `tinyload--load-list'.
Return:

 '(load-list pointer)."
  (let ((orig (get 'tinyload--load-list 'original)))
    ;; first invocation
    (put 'tinyload--process-busy-p 'count 0)
    ;;  No original values available, so set defaults
    (unless orig
      (put 'tinyload--load-list 'original tinyload--load-list))
    (unless (integerp (get 'tinyload--load-list 'pos))
      (put 'tinyload--load-list 'pos 0))
    ;; user has recently changed "list", do update.
    (unless (equal orig tinyload--load-list)
      (put 'tinyload--load-list 'original tinyload--load-list)
      (put 'tinyload--load-list 'pos 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-terminate-process ()
  "Remove process."
  ;;  No more loading; do self kill so that this process is
  ;;  not unnecessarily held in timer list.
  ;;
  ;;  19.34 bug: Process can't remove itself. Ack. Fixed in
  ;;  new Emacs releases.
  (tinyload-message "TinyLoad: Bye, No more packages to load.")
  (setq tinyload--process-busy-p nil)
  (tinyload-install 'remove))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-busy-count ()
  "Return `tinyload--process-busy-p' busy count."
  (get 'tinyload--process-busy-p 'count))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-busy-count-incf ()
  "Increase `tinyload--process-busy-p' busy count."
  ;;  - If counter keeps incrementing all the time,
  ;;    then the main loop never cleared the flag
  ;;  - Keep on eye on the counter and prevent deadlock by resetting
  ;;    the busy signal.
  (let  ((busy-count (get 'tinyload--process-busy-p 'count)))
    (cond
     ;; Not yet defined, set initial value
     ((not (integerp busy-count))
      (setq busy-count 0))
     (t
      (incf busy-count)))
    (put 'tinyload--process-busy-p 'count  busy-count)
    (put 'tinyload--process-busy-p 'count2 busy-count)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-continue-check (&optional force)
  "Check if process can continue with FORCE.
Return CONTINUE if there is no activity."
  (multiple-value-bind (continue no-act no-input)
      (tinyload-process-continue force)
    (tinyload-message
     (format
      "TinyLoad: timer triggered; invoking load process... [%s;%s;%s;%d]"
      (if no-act          "not-busy"   "busy")
      (if (null continue) "stop"       "cont")
      (if no-input        ""           "input")
      (or (tinyload-busy-count) 0)))
    (tinyload-debug
     (format "tinyload-continue-check: %s" (prin1-to-string continue)))
    continue))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-failed-list-update (elt)
  "Update `tinyload--load-list' property 'failed-list with ELT."
  (let ((fid         "tinyload-failed-list-update")
	(failed-list (get 'tinyload--load-list 'failed-list)))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;;  Remove entry from failed list
    (setq failed-list (delete elt failed-list))
    (put 'tinyload--load-list 'failed-list failed-list)
    (tinyload-debug
     (format "TinyLoad: [Debug] %s failed-list: "  fid) failed-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-library-info (pkg noerr)
  "Record PKG NOERR library info under debug."
  (when tinyload--debug
    (message "TinyLoad: [debug] locating library %s %s"
             pkg (prin1-to-string noerr))
    (let ((tmp (locate-library pkg)))
      (tinyload-debug (format "TinyLoad: [debug] locate %s %s"
                              pkg (or tmp ""))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-ignore-message (pkg pos len)
  "Print PKG POS LEN status. Already in Emacs."
  (let ((str (format "\
TinyLoad: %-15s %s (%2d%% %2d/%2d) <ignored, feature already in emacs>"
		     pkg
		     "ok"
		     (/ (* 100 pos) len)
		     (1+ pos) (1+ len))))
    (tinyload-message str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-load-ok-message (pkg pos len stat)
  "Print PKG POS LEN status. Loaded."
  (let ((str (format "TinyLoad: %-15s %s (%2d%% %2d/%2d)"
		     pkg
		     (if stat
			 "ok"
		       "'noerr!")
		     (/ (* 100 (1+ pos)) len)
		     (1+ pos) len)))
    (tinyload-message str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyload-busy-count-controller ()
  "Handle busy checking and deadlocks.
Return:
  deadlock     if non-nil, deadlock was detected."
  (let ((busy-count (tinyload-busy-count-incf))
	deadlock)
    (incf  busy-count)
    (when (> busy-count 5)
      (tinyload-debug "Tinyload: busy count too high, clearing DEADLOCK")
      (tinyload-message "TinyLoad: Deadlock detected, clearing...")
      (setq tinyload--process-busy-p nil
            busy-count               0
            ;;  If there is infnite prompt open, we never would get
            ;;  past it, because the input-pending-p tests later would
            ;;  stop preceeding to load commands. FORCE going one load
            ;;  this time. The next busy, will again wait for deadlock,
            ;;  (if prompt is still open), but eventually the packages
            ;;  will get loaded.
            ;;
            ;;  Extended period of prompt open is an indication that
            ;;  use is not present.
            ;;
            ;;  #todo: to be asolutely sure, utilize top level `count2'
            ;;  which would keep track of deadlocks and never-loads.
            ;;  ==> if too hight, only then FORCE load.
            ;;
            deadlock t))
    (put  'tinyload--process-busy-p 'count busy-count)
    deadlock))

;;; ----------------------------------------------------------------------
;;; (tinyload-loader-process 'force)
;;;
;;;###autoload
(defun tinyload-loader-process (&optional force)
  "Load packages defined in `tinyload--load-list'.
If called interactively, FORCE loading all packages in the list."
  (interactive (list 'force))
  (let (continue
	list
	pos
	len
	stat)
    (tinyload-debug "TinyLoad: [debug] main()"
                    "INPUT PENDING STATUS"
                    (input-pending-p)
                    "TIMER ELT"
                    tinyload--timer-elt)
    ;; ................................................... zombie test ...
    ;;  tinyload--timer-elt
    ;;
    ;;  - Emacs 19.34 has a bug. If the load list has been finished and _this_
    ;;    function tries to remove itseld with (tinyload-install 'remove);
    ;;    the timer element is not removed. Suprise.
    ;;  - However If I manually execute C-u M-x tinyload-install; then
    ;;    the process is killed all right.
    ;;  - So when the (tinyload-install 'remove) is called below; it sets
    ;;    the timer elt to nil; _but_ emacs still keeps calling this
    ;;    function. We're are now a zombie; we did try to kill
    ;;    ourself; but Emacs didn't let that to happen.
    ;;  - While we're a zombie, we don't display any messages or
    ;;    do anything. Calling this zombie function  is no-op and won't
    ;;    take process time much.
    ;;
    ;;  There may be previous function still loading; don't
    ;;  interrupt it; but terminate this invocation.
    (when tinyload--timer-elt
      (setq continue (tinyload-continue-check force))
      (if (tinyload-busy-count-controller)
          (setq force     t
                continue  t)))
    (tinyload-debug "TinyLoad: [debug] main() continue status: "
                    continue
                    (if tinyload--process-busy-p
                        "process busy" "process not busy"))
    (when (or force
              (and continue
                   (null tinyload--process-busy-p)))
      (unwind-protect
          (catch 'exit
            (tinyload-initialize)
            ;; ........................................... load list ...
            (setq pos  (get 'tinyload--load-list 'pos)
                  len  (length tinyload--load-list)
                  list (nthcdr pos tinyload--load-list))
            (tinyload-debug
             (format "TinyLoad: [Debug] list pointer: pos %d len %d" pos len))
            (unless list
              (tinyload-terminate-process)
              (throw 'exit t))
            (tinyload-debug "TinyLoad: [Debug] list" list)
            (dolist (elt list)
              (setq tinyload--process-busy-p 'busy)
              ;;  simple STRING is package name only
              (when elt
                (setq elt (ti::list-make elt)))
              (multiple-value-bind (pkg feature noerr nomsg
                                        form-before form-after)
                  elt
                ;;  Remove entry from failed list
                (tinyload-failed-list-update elt)
                (tinyload-debug
                 (format (concat "TinyLoad: [Debug] LIST ELT "
                                 "pkg: %s feature: %s elt: %s ")
                         pkg
                         feature
                         (prin1-to-string elt)))
                ;; ........................................... load it ...
                (when (and elt pkg)
                  ;;  Try to sit for some time before preceeding, otherwise
                  ;;  if we can't sit still that long, user is
                  ;;  doing something..
                  (tinyload-debug "TinyLoad: >>> 1 -- input pending?")
                  (let ((wait (or tinyload--wait-next-load 0.3)))
                    (unless (and (sit-for wait)
                                 (not (input-pending-p)))
                      (tinyload-debug
                       (format "´THROW ´sit-for' didn't return t (activity) %d"
                               wait))
                      (throw 'exit t)))
                  (tinyload-debug "TinyLoad: >>> 2 -- feature present?")
                  (setq stat (tinyload-feature-p pkg feature))
                  (incf  pos)
                  (put 'tinyload--load-list 'pos pos)
                  (tinyload-debug
                   (format "TinyLoad: >>> 3, pkg %s feature `%s' status: %s"
                           (prin1-to-string pkg)
                           (prin1-to-string feature)
                           (prin1-to-string stat)))
                  (tinyload-debug
                   (format "TinyLoad: [Debug] pkg forms before:%s after:%s"
                           (prin1-to-string form-before)
                           (prin1-to-string form-after)))
                  (cond
                   ;; ................................. feature in Emacs ...
                   (stat
                    (tinyload-load-ignore-message pkg pos len))
                   ;; ..................................... not in emacs ...
                   (t
                    (tinyload-eval form-before "BEFORE")
                    (tinyload-library-info pkg noerr)
                    (unless (tinyload-process-continue)
                      (tinyload-debug
                       (format "THROW 2 input-p didn't return t (activity)"))
                      (throw 'exit t))
                    (setq stat (tinyload-load pkg noerr nomsg))
                    (cond
                     (stat
                      (tinyload-eval form-after "AFTER")
                      (tinyload-load-ok-message pkg pos len stat))
                     (t
                      (tinyload-load-failure pkg elt)
                      (setq stat 'fatal))))))
                (when (or (input-pending-p)
                          (eq stat 'fatal))
                  (throw 'exit t)))))
        ;; .................................................... unwind ...
        (setq tinyload--process-busy-p nil)))))

;;}}}
;;{{{ example

;;; ......................................................... &example ...
;;; - Here is example at the time of writing tinyload v1.14
;;; - Hope you get some ideas from this.

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- -- example --
(when nil ;;  Start of example - Emacs does not read code below

  ;; ~/elisp/rc/emacs-rc-tinyload.el -- Delayed loading of files
  ;;
  ;;  Docid
  ;;
  ;;      This is a personal Emacs (rc) resource files and it
  ;;      is loaded from .emacs in the following manner
  ;;
  ;;          (require 'emacs-rc-tinyload)
  ;;
  ;;      This file may be under some other name in the current directory
  ;;      where you found it, but do not mind that.. Just rename this file to
  ;;      whatever is shown in the first line.
  ;;
  ;;  Description
  ;;
  ;;      This file configures all packages and files that can be loaded
  ;;      later when Emacs sits idle for tinyload.el. See for full description
  ;;      of the usage from there.
  ;;
  ;;      `emacs-rc-xxx'  are all Emacs resource files of various kind.
  ;;
  ;;      `ti::compat-window-system' is Emacs independent window system check
  ;;      function found from tinylib.el

;;; ............................................................ &load ...

  (let* ((w  (ti::compat-window-system)) ;XEmacs and Emacs detection
         (x  (eq w 'x))                  ;x windowed
         (win32 (eq w 'win32)))          ;Windows
    (setq tinyload--load-list
          (list
           ;;  Those with 'noerr flag are not essential packages
           ;;
           ;;  In X Windowed emacs, Load non-compiled rc file in XEmacs, because
           ;;  the compiled faces are not compatible with XEmacs.
           (when w
             (list (if (ti::emacs-p) "emacs-rc-font" "emacs-rc-font.el")
                   'rc-font nil nil nil
                   ;;  The file defines function `my-face-change'
                   ;;  that is called after load.
                   ;;  It configures faces for this emacs.
                   '(progn
                      (cond
                       (nt (my-face-change 'pc))
                       (t  (my-face-change 'def))))))
           (list "emacs-rc-macros")
           (list "emacs-rc-setting")
           (list "emacs-rc-tiny")
           (list "emacs-rc-standard-packages")
           (list "emacs-rc-hooks")
           (unless win32 ;; I don't use mail here
             ;;  Package contain faces: load non-compiled version for XEmacs
             (list (if (ti::emacs-p)
                       "emacs-rc-mail"
                     "emacs-rc-mail.el")))
           ;;  Tiny Tools distribution
           (list "tinyef"       nil 'noerr)
           (list "tinytab"      nil 'noerr)
           (list "tinylisp"     nil)
           (list "tinymy"       nil)
           (list "tinysword"    nil 'noerr)
           (list "tinydiff"     nil 'noerr)
           (list "tinyreplace"  nil 'noerr)
           (list "tinytfo"      nil 'noerr)
           (list "tinydired"    nil 'noerr)
           (list "tinycache"    nil 'noerr)
           (list "tinyigrep"    nil 'noerr)
           (list "tinylibmenu"  nil)
           (list "tinymatch"    nil)
           (list "tinylibid"    nil 'noerr)
           (list "tinydesk"     nil 'noerr)
           (if (ti::emacs-p)
               (list "mldrag" nil 'noerr nil
                     '(progn (setq mldrag-load-hook 'mldrag-default-keys))))
           ;; Run extra fa-setup aftert package.
           (list "fa-extras" nil 'noerr nil nil '(progn (my-fa-setup)))
           ;;  Personal lisp function library. Run compression
           ;;  After loading this package.
           (list "mylib.el"
                 'mylib
                 nil
                 nil
                 nil
                 '(progn
                    (when (fboundp 'my-compress-household)
                      (my-compress-household)))))))

  (defun my-fa-setup ()
    "Filladapt setup."
    (when (boundp 'filladapt-token-table)
      (defvar filladapt-token-table nil)
      (defconst filladapt-mode-line-string " Fa")
      (let ((tok  "[*]+")
	    (elt (assoc tok filladapt-token-table)))
        ;;  Clear the old definition
        (cond
         ((setq  filladapt-token-table (delq elt filladapt-token-table))
          (setq  filladapt-token-table
                 (cons (cons tok 'citation->)
                       filladapt-token-table))))
        (setq tok ">+")
        ;; (setq tok adaptive-fill-regexp)
        (cond
         ((setq elt (assoc tok filladapt-token-table))
          (setq filladapt-token-table (delq elt filladapt-token-table))
          (setq  filladapt-token-table
                 (list (cons adaptive-fill-regexp 'citation->))))))))

  ;; (provide 'emacs-rc-tinyload)

  ;; ;; End of file emacs-rc-tinyload.el

  ) ;; ++Example-End++
;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- -- example --

;;}}}
;;{{{ final setup

(tinyload-install)
(provide   'tinyload)
(run-hooks 'tinyload--load-hook)

;;}}}

;;; tinyload.el ends here
