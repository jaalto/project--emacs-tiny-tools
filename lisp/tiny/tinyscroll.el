;;; tinyscroll.el --- Enable or disable auto-scroll for any buffer.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1996-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
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

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;    (require 'tinyscroll)
;;
;; or use autoload; your .emacs loads up a bit quicker. In this package
;; however the above method is preferred, since it automatically
;; marks *compilation* buffer for auto-scrolling. Using the autoload
;; puts the scroll in effect only when you add entry to scroll list
;; with M-x tinyscroll-control.
;;
;;    (autoload 'tinyscroll-control                  "tinyscroll" "" t)
;;    (autoload 'tinyscroll-list                     "tinyscroll" "" t)
;;    (autoload 'tinyscroll-timer-process-control    "tinyscroll" "" t)
;;    (eval-after-load "compile" '(require 'tinyscroll))
;;
;; To activate/deactivate scrolling for a buffer or to check list, call
;;
;;      M-x tinyscroll-control
;;      M-x tinyscroll-list
;;
;; To set default buffers to scroll, change this variable
;;
;;      tinyscroll--list
;;
;; To investigate problems:
;;
;;      M-x tinyscroll-debug-toggle             to toggle the package debug.

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, May 1996
;;
;;      I was in the middle of testing one of my new packages which didn't
;;      quite work as I wanted, I was loading all the lisp files to see if
;;      it breaks. I watched the *Message* buffer to fill with statements
;;
;;          Loading abbrev...
;;          Loading abbrev...done
;;          ...
;;          Loading rmail...
;;          loading rmail done...
;;          ...
;;
;;      But suddendly the emacs died. It kicked me off to the shell and I
;;      had no idea what package was the last one that got loaded.
;;
;;      You see, the *Message* buffer keeps growing, but you have to tap
;;      the pgDown key to get to the end, all the time. Instead I decided
;;      to pull out some lisp to do general auto-scrolling for any buffer,
;;      so that I can just sit back and watch the buffer move. No more
;;      guessing in *Message* buffer what was the last message before Emacs
;;      sunk :-)
;;
;;  Overview of features
;;
;;      o   Select buffer, and hit auto scroll on/off. You can scroll any
;;          buffer.
;;      o   All windows for the buffer are scrolled in all frames.
;;          If frame is miimized and contains window to sroll, frame will
;;          be maximized ("popped up")
;;      o   If buffer's point-max doesn't move, scroll is ignored.
;;      o   Default scroll activated for: *Compilation* *Grep* and *Messages*
;;
;;  How to use this package
;;
;;      The scroling here is based on timers, where the lowest interval can
;;      be one 1 second. This means that you don't get smooth and
;;      continuous scrolling, but regular update of the buffer, which may
;;      in rare cases seem jerky. However, using timers is the only
;;      possibility if we want to have general scroll utility for *any* buffer.
;;
;;      To enable/disable auto-scroll for current buffer, use these:
;;
;;          M-x tinyscroll-control              ;to activate scroll
;;          C-u M-x tinyscroll-control  ;to deactivate scroll
;;
;;  Lowest window of the same buffer always scrolls
;;
;;      It is an interesting problem, when you have SAME buffer in multiple
;;      windows, to decide which window to scroll.  I didn't want to scroll
;;      all windows, since otherwise I wouldn't have used two/or more
;;      windows for the same buffer.
;;
;;      I decided that the lowest window for the buffer always scrolls. You
;;      can't change that. This was a design decision and I won't support
;;      scrolling middle/upper buffers. Just arrange your windows so that
;;      the scrolling one goes to the bottom.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: libraries

(require 'tinylibm)

(eval-and-compile
  (ti::package-package-require-timer))

(ti::package-defgroup-tiny TinyScroll tinyscroll-- extensions
  "Enable or Disable autos-croll for any buffer.
  Overview of features

        o   Select buffer, and hit auto scroll on/off. You can scroll any
            buffer.
        o   If there are multiple windows for the same buffer, scroll only the
            bottom one. --> you can have \"permanent\" look window, while
            the buffer scrolls in other window.
        o   Smart scrolling: if buffer's point-max doesn't move, it ignores
            scrolling. This way you can browse buffer after there is
            no more output to window.
    ")

;;}}}
;;{{{ setuo: public, user configurable

(defcustom tinyscroll--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'boolean
  :group 'TinyScroll)

(defcustom tinyscroll--interval 3
  "*Interval in seconds when scrolling process activates.
Must be bigger that 1."
  :type  'integer
  :group 'TinyScroll)

;; Initalize this in tinyscroll--load-hook if you want to have some
;; other default buffers at startup.
(defcustom tinyscroll--list
  '(
    ("*compilation*" . 1)               ;set this to auto scroll
    ("*grep*"        . 1)
    ("*igrep*"       . 1)
    ("*Messages*"    . 1))
  "*List of buffers  that have auto scroll active.
Format: '((buffer-name-string . max-point) (BN . POINT) ..)"
  :type '(repeat
          (string :tag "buffer")
          (integer :tag "point"))
  :group 'TinyScroll)

;;}}}
;;{{{ setup: private

(defvar tinyscroll--tmp-buffer "*auto-scroll*"
  "Temporary buffer to display the active auto-scroll buffers.")

(defvar tinyscroll--timer-elt nil
  "Timer process.")

;;}}}
;;{{{ code: misc

;;;### (autoload 'tinyscroll-debug-toggle "tinyscroll" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinyscroll" "--"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-active-buffer-p (buffer-name)
  "Check is BUFFER-NAME name is in `tinyscroll--list'."
  (assoc buffer-name tinyscroll--list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-add-1 (buffer-name position)
  "Add BUFFER-NAME and last POSITION to scroll list."
  (push (cons buffer-name position) tinyscroll--list ))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-remove-1 (buffer-name)
  "Remove BUFFER-NAME from scroll list."
  (setq tinyscroll--list (adelete 'tinyscroll--list buffer-name)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-point-max-moved-p (buffer-name max)
  "Find BUFFER-NAME; return t if MAX is not stored `point-max' for BUFFER-NAME.
Also updates new `point-max' if MAX is different.
If buffer does not exist, do nothing and return nil."
  (let ((elt (tinyscroll-active-buffer-p buffer-name)))
    (tinyscroll-debug "Max-p check" elt buffer-name max "\n")
    (when (and elt (not (eq (cdr elt) max)))
      (setcdr elt max)
      max )))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-buffers ()
  "Return list of buffer that have auto scroll on."
  (mapcar 'car tinyscroll--list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-ti::temp-buffer ()
  "Set up temporary buffer and displays it."
  (ti::temp-buffer tinyscroll--tmp-buffer 'clear)
  (pop-to-buffer  tinyscroll--tmp-buffer) )

;;; ----------------------------------------------------------------------
;;; if easier to trap "t" error condition.
;;;
(defun tinyscroll--list-add (buffer-name position &optional remove)
  "Check is BUFFER-NAME name is in 'tisc:-list'.

Input:

  BUFFER-NAME   buffer name string
  POSITION      `point-max' in the buffer
  REMOVE        flag, remove buffer from list

Return:

  nil           Yes, buffer is in list
  t             action not done"
  (let ((exist (tinyscroll-active-buffer-p buffer-name))
	ret)
    (cond
     ((or (and remove           (null exist))
          (and (null remove)    exist))
      (setq ret t))
     (remove
      (tinyscroll-remove-1 buffer-name))
     (t
      (tinyscroll-add-1 buffer-name position)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyscroll-window-list ()
  "Return windows that have auto scroll enabled.
Return:
   window list or nil"
  (let (win
	win-list)
    (dolist (frame (frame-list))
      (dolist (buffer (tinyscroll-buffers))
        (if (setq win (get-buffer-window buffer frame))
            (push win win-list))))
    win-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinyscroll-process ()
  "Scroll all window buffers in `tinyscroll--list'.
Activate This process activates itself only when the window, which
should be scrolled, is visible"
  (let ((list   (tinyscroll-window-list))
        (oframe (selected-frame)))
    (when list                          ;if we bother to do anything?
      (dolist (win list)
        (ti::save-excursion-macro
          (set-buffer (window-buffer win))
          (tinyscroll-debug "tinyscroll-process: " (window-buffer win)
                            (buffer-name) (point-max) "\n")

          ;;  Scrolling in fact means that, the point-max is
          ;;  always visible
          (select-window win)
          (when (tinyscroll-point-max-moved-p (buffer-name) (point-max))
            (ti::pmax) )))
      (select-frame oframe))
    nil))

;;}}}
;;{{{ code: interactive

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyscroll-timer-process-control (&optional delete verb)
  "Keep the auto scroll process and timer process alive.
Optionally DELETE auto scroll process. VERB."
  (interactive "P")
  (setq tinyscroll--timer-elt
        (ti::compat-timer-control "1 sec"
                                  tinyscroll--interval
                                  'tinyscroll-process
                                  delete
                                  verb)))

;;; ---------------------------------------------------7-------------------
;;;
;;;###autoload
(defun tinyscroll-list (&optional print)
  "Show list of active auto scroll buffers.
Buffers are listed inecho-area if they fit there, otherwise in separate buffer.

If optional PRINT flag is non-nil, always generate report to temporary buffer.
If list if empty, do nothing.

Return:

 t      report generated to temporary buffer
 nil    no report"
  (interactive)
  (let ((str   (ti::list-to-string (mapcar 'car tinyscroll--list)))
	(verb  (interactive-p))
	ret)
    (if (and (string= str "")  verb)
        (message "TinyScroll: no entries in `tinyscroll--list'.")
      (setq ret t)
      (cond
       ((and (null print)
             (< (length str) 80))
        (message str))
       (t
        (tinyscroll-ti::temp-buffer)
        (insert (ti::list-to-string tinyscroll--list "\n"))
        (setq buffer-read-only t)
        (shrink-window-if-larger-than-buffer))))
    ret))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;; - It's a bit slow to create buffer comletions this way.
;;;   Anybody has a better suggestion to amulate "bBuffer"
;;;   interactive tag? Mail me if you know...
;;;
;;;   But I couldn't get the on/off information to the prompt
;;;   otherwise.
;;;
;;;###autoload
(defun tinyscroll-control (buffer-or-pointer &optional off verb)
  "Turn on auto scroll on/off for current buffer.
If this command is called from `tinyscroll--tmp-buffer' then the current
word in the line is read and offered for default buffer name.

Input:

  BUFFER-OR-POINTER     buffer to scroll
  OFF                   flag, prefix arg; is non-nil turn scrolling off
  VERB                  flag, allow verbose messages."
  (interactive
   (list
    (completing-read
     (format "Scroll [%s] buffer: " (if current-prefix-arg "off" "on"))
     (ti::list-to-assoc-menu
      (ti::dolist-buffer-list (string-match "." (buffer-name))))
     nil
     nil
     ;; Default buffer ...
     (if (string= (buffer-name) tinyscroll--tmp-buffer)
         (ti::read-current-line)
       (buffer-name))) ;; completing-read
    current-prefix-arg))
  (let ((bufferp       (if (bufferp buffer-or-pointer)
			   buffer-or-pointer
			 (get-buffer buffer-or-pointer)))
	buffern
	msg)
    (ti::verb)
    ;;  Check non-interactive errors
    (if (or (null bufferp)
            (not (buffer-live-p (get-buffer bufferp))))
        (error "Invalid arg, buffer %s" bufferp))
    (setq buffern (buffer-name bufferp))
    (if off
        (if (tinyscroll--list-add buffern 0 'remove)
            (setq msg "TinyScroll: buffer already removed."))
      ;;  Keep the process alive all the time
      (tinyscroll-timer-process-control)
      (with-current-buffer buffern
        ;;  We have to record the point-max
        (if (tinyscroll--list-add buffern (point-max))
            (setq msg "TinyScroll: Already in list.")) ))
    (if verb
        (message msg))))

;;}}}

(tinyscroll-timer-process-control) ;; wake it up !
(provide   'tinyscroll)
(run-hooks 'tinyscroll--load-hook)

;;; tinyscroll.el ends here
