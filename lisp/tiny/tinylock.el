;;; tinylock.el --- Simple emacs locking utility

;; This file is not part of Emacs

;;{{{ Id

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
;; Put this file on your Emacs-Lisp `load-path', add one of these into your
;; ~/.emacs startup file
;;
;;   Normal load
;;
;;      (require 'tinylock)
;;
;;   Autoload, your emacs starts up faster, prefered, but doesn't
;;   activate the auto locking feature.
;;
;;      (autoload 'tinylock-lock "tinylock" "Lock emacs" t)
;;
;;   ESC-l, suggested keybinding, replaces downcase-word binding
;;   because you can accomplish the same with C-x C-l,
;;   downcase-region.
;;
;;      (global-set-key "\M-l" 'tinylock-lock)     ;; Suggested keybinding.
;;
;;   See also Example section at the end of file.

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;; Overview of features
;;
;;      o   Locks emacs completely until right key is entered.
;;      o   Auto-locks emacs after NN minutes idle time
;;      o   False login attemps are stored in history log.
;;      o   Blanks display or displays message buffer when locked.
;;      o   Hooks: before and after lock is activated and removed
;;
;;  About locking procedure
;;
;;      Don't get shocked now... When the lock gets in effect there must be
;;      no running processes inside emacs that would generate error and
;;      make emacs vulnerable to break in.  That's why all the running
;;      processes are killed before the lock takes in effect. If you have
;;      some valuable processes that are constantly running, you must make
;;      a separate "process control" function that would restart any such
;;      processes. Use the appropriate hook to activate those processes
;;      again after the emacs is unlocked. Use hooks
;;
;;          tinylock--before-lock-hook       ;; Save processes here
;;          tinylock--after-lock-hook        ;; restore processes here
;;
;;      and following function which tells you what processes are running.
;;
;;          M-x list-processes
;;
;;      All extra frames are also deleted. At least for now, because I
;;      don't know a reasonable way to save the frame configurations
;;      right now. Please send me piece of code or pointer to package
;;      that can save and restore frames and the windows back to previous
;;      state if you know good solution.
;;
;;  About auto locking feature, Emacs prior 19.34
;;
;;      When you load this package the `tinylock--load-hook' runs
;;      `tinylock-install-lock-timer' command that setup up a timer process that
;;      wakes up periodically. If the emacs has not changed compared to
;;      last saved emacs state, then the auto locking takes in effect
;;      immediately.
;;
;;      In old Emacs the activity is determined in simple way
;;
;;      o   if buffer list order has changed user is doing something.
;;      o   if `switch-buffer' was used, user is doing something
;;      o   if any buffer's size has changed, user is doing something.
;;
;;      This checking may not be enough: if user just scroll some
;;      text in buffer for NN minutes, then from `tinylock-process' 's point of
;;      view there has not been any activity and the user may suddenly
;;      notice that emacs locks up. Doing nothing but viewing one buffer
;;      all the time is fortunately rare.
;;
;;  About auto locking feature in new Emacs
;;
;;      New Emacs releases have command `run-with-idle-timer' which we use
;;      if it is available. When there has been no activity for NN minutes,
;;      your Emacs locks up.
;;
;;      The advice code and the other tricks we needed to detect idle
;;      activity in lower emacs versions aren't installed in these Emacs
;;      versions, so you don't have to worry about sudden lock.
;;
;;  Auto lock password
;;
;;      Do not put password in your ~/.emacs, but answer to the question
;;      which is asked when this file is loaded. If you want to change it
;;      during your emacs session, call function
;;
;;          M-x tinylock-auto-lock-set-password
;;
;;  Changing the auto lock interval
;;
;;      The auto lock interval depends on the wake up time of timer
;;      process. The default time is 20 minutes when you load this
;;      file. You can change the time by calling
;;
;;          M-x tinylock-auto-lock-set-interval
;;
;;      Or by putting this code in your ~/.emacs
;;
;;          ;; First define the hook, so that we can append to it
;;          (setq tinylock--load-hook
;;             '(tinylock-timer-control tinylock-auto-lock-set-password)
;;
;;          ;; add function to the end
;;          (add-hook 'tinylock--load-hook 'my-tinylock-auto-lock-set-interval 'append)
;;
;;
;;          (defun my-tinylock-auto-lock-set-interval ()
;;            "Change interval to 10 minutes."
;;            (tinylock-auto-lock-set-interval 10))
;;          ;; end of example

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-and-compile
  (ti::package-package-require-timer))

(ti::package-defgroup-tiny TinyLock tinylock-- extensions
  "Simple emacs locking utility.
 Overview of features

        o   Locks emacs completely until right key is entered.
        o   Auto-locks emacs after XXX idle time
        o   False attemps are stored in history log.
        o   Blanks or displays buffer message when locked.
        o   Hooks: before and after lock is entered/removed")

;;}}}
;;{{{ setup: variables

(defcustom tinylock--load-hook nil
  "*Hook run after file is loaded."
  :type  'hook
  :group 'TinyLock)

(defcustom tinylock--before-lock-hook  nil
  "*Hook that is run when the locking process initiates.
This is your chance to save frame configurations or processes before
they are killed."
  :type  'hook
  :group 'TinyLock)

(defcustom tinylock--after-lock-hook nil
  "*Hook that is run after lock is removed."
  :type  'hook
  :group 'TinyLock)

;;; ....................................................... &v-private ...

(defvar tinylock--history nil
  "\(DATE PASSWD\) A storage where attempts of entering locked Emacs is put.
Cleared every time lock takes effect.")

(defvar tinylock--auto-lock-data  nil
  "Data to tell about the idle state, updated by timer process.
Contains:
'(current-time          ;; time stamp of user activity
  (BUFFER-LIST)
  (SIZE SIZE SIZE ..))   ;; every buffers size.")

(defvar tinylock--auto-lock-password  nil
  "Password in auto lock situation.
Password is asked when you load this file. You shouldn't define
this in you ~/.emacs")

(defvar tinylock--auto-lock-interval  nil
  "The timer interval in minutes. Use \\[tinylock-auto-lock-set-interval].")

(defvar tinylock--idle-timer-process  nil
  "19.34+ timer process.")

;;; ........................................................ &v-config ...

(defcustom tinylock--login-error-sleep 2
  "*Time in seconds that is waited until new login to is possible."
  :type '(integer :tag "Seconds")
  :group 'TinyLock)

(defcustom tinylock--buffer-login-history "*tinylock-hist*"
  "*Buffer to output the history data."
  :type 'string
  :group 'TinyLock)

(defcustom tinylock--buffer-blank "*blank*"
  "*Buffer name used when screen is blanked."
  :type 'string
  :group 'TinyLock)

(defcustom tinylock--blank-when-locked-flag t
  "*Non-nil means show `tinylock--buffer-blank' buffer."
  :type 'string
  :group 'TinyLock)

;;}}}
;;{{{ code: macros, advices

(defmacro tinylock-time-dd (time)
  "Return Day from TIME."
  (list 'string-to-int (list 'substring time 8 10)))

(defmacro tinylock-time-hh (time)
  "Return hour from TIME."
  (list  'string-to-int (list 'substring time -13 -11)))

;;}}}
;;{{{ code: misc funcs

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-auto-lock-set-interval (minutes)
  "Set new MINUTES interval by stopping and restarting timer process."
  (interactive "Nminutes: ")
  (tinylock-install-lock-timer nil minutes)
  nil)

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-auto-lock-set-password ()
  "Set auto lock password."
  (interactive)
  (let (pass)
    (if (ti::nil-p (setq pass (ti::query-read-input-as-password
                               "TinyLock, give autolock password: ")))
        (error "Password is empty.")
      (setq tinylock--auto-lock-password pass))
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-process-on ()
  "Start auto lock process."
  (tinylock-install-lock-timer nil tinylock--auto-lock-interval))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-install-lock-timer (&optional uninstall interval)
  "Install process that locks Emacs when there is no activity.

Input:

  UNINSTALL   `tinylock-process'
  INTERVAL    in minutes, by default 20."
  (interactive "P")
  ;; .......................................................... kill ...
  (ti::compat-timer-cancel-function 'tinylock-process)
  (setq tinylock--idle-timer-process nil)
  ;; .................................................... set values ...
  (setq tinylock--auto-lock-interval
        (or interval
            tinylock--auto-lock-interval
            20))                        ;Default 20 minutes
  ;; ................................................... maybe start ...
  (unless uninstall
    (cond
     ((fboundp 'run-with-idle-timer)    ;19.34+
      (setq
       tinylock--idle-timer-process
       (ti::funcall
        'run-with-idle-timer
        (* tinylock--auto-lock-interval 60)
        'repeat
        'tinylock-lock-now)))
     (t
      (tinylock-process-data-set)
      (run-at-time
       "1 sec"
       (* tinylock--auto-lock-interval 60)
       'tinylock-process))))
  (if (interactive-p)
      (message "Autolock process %s"
               (if uninstall
                   "deleted"
                 "started"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-user-activity ()
  "Tell to timer process that the has bee user activity."
  (or
   (ignore-errors (setcar tinylock--auto-lock-data (current-time)))
   ;;  Hmm, data is corrupted... reset it.
   (tinylock-process-data-set)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-process-data-set ()
  "Update timer process data."
  (setq tinylock--auto-lock-data
        (list
         (current-time)
         (buffer-list)
         (mapcar
          (function
           (lambda (x)
             (with-current-buffer x
               (buffer-size))))
          (buffer-list))))
  nil)

;;; ----------------------------------------------------------------------
;;; Just testing... (tinylock-process-data-set) (tinylock-process-data-unchanged-p)
;;;
(defun tinylock-process-data-unchanged-p ()
  "Return t if timer data has not changed = No activity in."
  (let* ((data          tinylock--auto-lock-data)
         (time          (nth 0 data))
         (buffer-list   (nth 1 data))
         (size-list     (nth 2 data))
         (list          (buffer-list))
         (i             0)
         unchanged)
    (if (null tinylock--auto-lock-data)
        (tinylock-process-data-set)
      ;; o if buffer list order is the same, the user may not have
      ;;   done any new work...
      ;; o Next we check if buffer sizes have changed, if not, then
      ;;   user hasn't done any work in emacs.
      (condition-case nil
          (and (> (ti::date-time-difference (current-time) time)
                  (- (* tinylock--auto-lock-interval 60) 5)) ;5 sec timeframe
               (equal list buffer-list)
               (progn
                 (setq unchanged t)
                 (dolist (elt list)
                   (with-current-buffer elt
                     (if (not (eq (buffer-size)
                                  (nth i size-list)))
                         ;; Found changed buffer, stop there and
                         ;; reset lock status, and quit the loop by
                         ;; killing the list
                         ;;
                         (setq list nil  unchanged nil))
                     (incf  i)))))
        ;;  Data is corrupted somehow, fix it.
        (error
         (tinylock-process-data-set)))
      unchanged)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-process ()
  "Lock up Emacs if it there has not been any user activity."
  (when (tinylock-process-data-unchanged-p)
    ;;  When Emacs locks up, this function process will die too.
    (tinylock-lock-now))
  (tinylock-process-data-set))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-add-history (passwd)
  "Add login attempt to `tinylock--history'.PASSWD is the attempted login password."
  (let ((d (current-time-string)))
    (setq tinylock--history
          (append  tinylock--history
                   (list (list d passwd))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-kill-process-control (&optional kill)
  "Return all processes in string format, or KILL all processes (not timer)."
  (let ((list (process-list))
         ret)
    (dolist (x list)
      (cond
       ((null kill)
	(setq ret (concat (or ret "") (prin1-to-string x))))
       (t
	;;  let's not kill the timer
	(if (not (string-match "display-time\\|timer"
			       (prin1-to-string x)))
	    (delete-process x)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylock-history ()
  "Displays login history. Optionally to given buffer BUFFER."
  (interactive)
  (let ((i 0))
    (switch-to-buffer-other-window
     (get-buffer-create tinylock--buffer-login-history))
    (erase-buffer)
    (dolist (elt tinylock--history)
      (insert (format "%2d: %-27s %s\n" i (nth 0 elt) (or (nth 1 elt) "<nil>") ))
      (setq i (1+ i)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-blank-control (&optional unblank)
  "Blank display or UNBLANK."
  (let ((blank (get-buffer-create tinylock--buffer-blank)))
    (cond
     (unblank
      (ti::kill-buffer-safe blank))
     (t
      (ti::select-frame-non-dedicated)
      (dolist (frame (delq (selected-frame) (frame-list)))
        (delete-frame frame))
      (switch-to-buffer blank t)
      (delete-other-windows)            ;delete all other windows
      ;;   This is necessary in 19.28 for some unknown reason
      ;;   otw, the sreen is not shown
      (sit-for 1)))))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-lock-now ()
  "Lock up Emac."
  (tinylock-lock tinylock--auto-lock-password "Autolocking.. emacs " 'doit ))

;;; ------------------------------------------------------------ &main ---
;;;
;;;###autoload
(defun tinylock-lock (psw &optional msg lock-now)
  "Lock Emacs with PSW password and MSG.
Message is displayed if LOCK-NOW is nil.
If LOCK-NOW is non-nil emacs is immediately locked with PSW."
  (interactive
   (list
    (progn
      (message "Now enter lock string...") (sit-for 1)
      (ti::query-read-input-invisible))))
  (let ((cursor-in-echo-area nil)
	;;  It's good programming style NOT to use globals directly
	;;  inside code This way maintainer sees at glance what it uses.
	(key-msg        "This emacs is locked, enter key:")
	(entry-err      "Unauthorized access.")
	(wait           tinylock--login-error-sleep)
	(loop           t)
	ans)
    (or msg
	(setq msg "Lock Emacs ? "))
    (catch 'done
      (if (ti::nil-p psw)
          (error "Password is empty."))
      (if (and (null lock-now)
               (null (y-or-n-p msg)))
          (throw 'done t))
      (save-window-excursion
        (run-hooks 'tinylock--before-lock-hook))
      ;;  It's better to save work, you may forgot the password :-/
      (save-some-buffers 'noAsk)
      (ti::compat-timer-list-control     'save)
      (tinylock-install-lock-timer   'kill)     ;our process
      (tinylock-kill-process-control 'kill)     ;get rid of them
      (ti::compat-timer-list-control     'kill) ;Stop all timers
      (tinylock-blank-control)
      ;;   we need to restore windows config when we return
      (save-window-excursion
        (save-excursion
          ;; Now we make interrupting impossible, C-g won't work now on...
          (setq inhibit-quit t)
          (setq tinylock--history nil)  ;clear the log buffer
          (message "TinyLock: Emacs LOCKED %s " (ti::date-standard-date))
          (sleep-for 1)
          (message "")
          (while loop
            (when (input-pending-p)     ;wait for kbd event
              (discard-input)
              (message key-msg)
              (sleep-for 1)
              (message "")
              (discard-input)
              (setq ans (ti::query-read-input-invisible))
              (cond
               ((string-equal ans psw)
                (setq loop  nil))       ; right password, let user in
               (t
                (tinylock-add-history ans) ; record to log
                (message entry-err)
                (sit-for wait)))))))
      (tinylock-blank-control 'unblank)
      (message "TinyLock: Emacs unlocked %s" (ti::date-standard-date))
      (setq quit-flag nil inhibit-quit nil) ; restore flags
      (ti::compat-timer-list-control 'restore)
      (tinylock-process-on)
      (run-hooks 'tinylock--after-lock-hook)
      nil)))

;;}}}
;;{{{ Default: hook functions.

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-before-lock-function ()
  "Saves emacs state, so that you can recover from accidental crash."
  (when (fboundp 'tid-save-state)
    (message "TinyLock: wait, using TinyDesk to save emacs state...")
    (ti::funcall 'tid-save-state "~/emacs.lock-state.saved")
    (message "TinyLock: wait, using TinyDesk to save emacs state...done.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylock-after-lock-function ()
  "Restores Emacs state after lock"
  (display-time)                        ;re-enable process
  (when (fboundp 'timi-report-install-maybe)
    (ti::funcall 'timi-report-install-maybe)))

(add-hook 'tinylock--load-hook 'tinylock-process-on)

;; Ask lock password at startup

(if tinylock--auto-lock-password
    (remove-hook 'tinylock--load-hook 'tinylock-auto-lock-set-password)
  (add-hook 'tinylock--load-hook 'tinylock-auto-lock-set-password))

(add-hook 'tinylock--before-lock-hook 'tinylock-before-lock-function)
(add-hook 'tinylock--after-lock-hook  'tinylock-after-lock-function)

;;}}}

(provide   'tinylock)
(run-hooks 'tinylock--load-hook)

;;; tinylock.el ends here
