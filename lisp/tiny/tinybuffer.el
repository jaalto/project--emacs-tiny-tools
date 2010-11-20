;;; tinybuffer.el --- Change buffers in current window.

;; This file is not part of Emacs.

;;{{{ Id

;; Copyright (C) 1996-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; Look at the code with folding.el

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
;;  Add following statement(s) to your ~/.emacs
;;
;;      (require 'tinybuffer)
;;
;;  or use autoload and your ~/.emacs starts lot faster. Preferred method.
;;
;;    (autoload 'tinybuffer-iswitch-to-buffer     "tinybuffer.el" "" t)
;;    (autoload 'tinybuffer-previous-buffer       "tinybuffer.el" "" t)
;;    (autoload 'tinybuffer-next-buffer           "tinybuffer.el" "" t)
;;    (autoload 'tinybuffer-sort-mode-toggle      "tinybuffer.el" "" t)
;;
;;  You don't need to copy these if you used the `require', but in order
;;  to trigger autoload you must insert these into your ~/.emacs. These
;;  are also the defaults bindings. If you use something other that these,
;;  reset the `tinybuffer--load-hook' variable.
;;
;;    (setq tinybuffer--load-hook nil)  ;; Don't load default bindings.
;;
;;    ;;    If you use Emacs with X window, these could be suitable keys.
;;
;;    (global-set-key [(control <)]         'tinybuffer-previous-buffer)
;;    (global-set-key [(control >)]         'tinybuffer-next-buffer)
;;    (global-set-key [(control meta <)]    'tinybuffer-iswitch-to-buffer)
;;    (global-set-key [(alt <)]             'tinybuffer-sort-mode-toggle)
;;
;;    ;;    For non-windowed Emacs; you want to program your own keys.
;;    ;;    Please check C-h l `view-lossage' for your keybindings, these
;;    ;;    examples are from HP-UX numpad:
;;
;;    (global-set-key "\eOq" 'tinybuffer-previous-buffer)  ;; numpad 1
;;    (global-set-key "\eOr" 'tinybuffer-sort-mode-toggle) ;; numpad 2
;;    (global-set-key "\eOs" 'tinybuffer-next-buffer)      ;; numpad 3
;;
;;    ;;    Here is code to switch between all visible windows
;;
;;    (global-set-key [(f5)]
;;                   (ti::definteractive        ; in tinylibm.el
;;                     (other-window 1 t)
;;                     (raise-frame (window-frame
;;                                   (get-buffer-window
;;                                    (current-buffer))))))
;;
;;   If you have any questions, use this function to contact author
;;
;;      M-x tinybuffer-submit-bug-report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;
;;  Preface, May 1996
;;
;;      With this small package you can switch to next or previous buffer
;;      in a current window. If you only have small amount of buffers in
;;      `buffer-list', this may be the fastest way to select a working
;;      buffer. On the other hand, if you have more than 20 working
;;      buffers, I'd recommend that you use exellent substring buffer
;;      switching utility instead: *iswitchb.el* which is included in
;;      standard Emacs distribution
;;
;;      If you have windowed environment and want to have hot list of your
;;      permanent buffers available, use *imenu.el* or *tinyhotlist.el* and
;;      you can select buffers instantly.
;;
;;  Description
;;
;;      If you don't want default bindings, clear the installation with
;;      following command. This must be prior the 'require statement.
;;
;;          (setq tinybuffer--load-hook nil)
;;
;;      To change buffers forward or backward, the default setup would install
;;      following key bindings:
;;
;;      o   Control->       Next buffer
;;      o   Control-<       Previous buffer
;;      o   Alt-Control-<   Iswitch mode, where you can scroll with < and >.
;;          Press RET to select or ESC/q to quit
;;          This may come handy if you have many buffers and just want to
;;          skip 2-5 buffers fast. E.g. if the buffers are font-lock
;;          controlled, switching to them with the C-, and C-, keys might
;;          be slow due to fontification which happens every time you
;;          switch over a buffer.
;;
;;      In iswitch mode, the command prompt looks like following. The
;;      mode name is put last if buffer has and associated file name,
;;      so that filename gets as much display as possible.
;;
;;              "TinyIswitch: my-lisp.el     ~/elisp/my-lisp.el <Emacs lisp>"
;;              "TinyIswitch: test           <dired> ~/tmp/test"
;;              "TinyIswitch: *Messages*     <fundamental-mode>"
;;
;;      Have a look at `tinybuffer--ignore-regex' which you can configure
;;      to ignore some buffers permanently.
;;
;;  Thanks
;;
;;      Original idea for this package comes from *yic-buffer.el*
;;      by choo@cs.yale.edu (young-il choo) 1990-08-07.

;;}}}

;;; Change Log:

;;; Code:

(require 'tinylibm)

;; (eval-when-compile (require 'cl))

(ti::package-defgroup-tiny TinyBuffer tinybuffer-- extensions
  "Changing buffers in current window.
        With this small package you can switch to next or previous buffer
        in a current window. If you only have small amount of buffers
        it may be the fastest way.")

;;{{{ setup: -- variables

;;; User configurable

(defcustom tinybuffer--load-hook '(tinybuffer-install-default-bindings)
  "*Hook run when file has been loaded.
Default value contains function `tinybuffer-install-default-bindings'."
  :type  'hook
  :group 'TinyBuffer)

(defcustom tinybuffer--ignore-regexp
  (concat
   "^ "                                 ;hidden buffers
   "\\|completion\\|summary"
   "\\|buffer list\\|help\\|ispell\\|abbrev"
   "\\|temp\\|tmp\\|vc\\|compile-log\\|occur")
  "*Buffers to ignore when changing to another."
  :type  'regexp
  :group 'TinyBuffer)

(defcustom tinybuffer--sort-flag nil
  "*Non-nil means that buffers are switched in sorted order."
  :type  'boolean
  :group 'TinyBuffer)

(defcustom tinybuffer--iswitch-to-buffer-keys  '(?< ?>)
  "*Keys to scroll buffers backward and forward in iswitch mode.
See \\[tinybuffer-iswitch-to-buffer]."
  :type '(list
          (character :tag "Backward")
          (character :tag "Forward"))
  :group 'TinyBuffer)

(defcustom tinybuffer--iswitch-show-directory-flag  t
  "*Non-nil means that directory name is shown in iswitch mode.
See \\[tinybuffer-iswitch-to-buffer]."
  :type  'boolean
  :group 'TinyBuffer)

;;; Internal variables

(defvar tinybuffer--buffer-list  nil
  "Global buffer list for `tinybuffer-iswitch-to-buffer'.")

;;}}}
;;{{{ code: macros, functions

;;; ----------------------------------------------------------------------
;;;
(defmacro tinybuffer-iswitch-next ()
  "Return next buffer in list."
  `(let* ((first (car tinybuffer--buffer-list))
          (rest  (cdr tinybuffer--buffer-list))
          (ret   (car rest)))
     (setq list rest)
     (ti::nconc list first)                     ;add to the end
     (setq tinybuffer--buffer-list list)        ;update list
     ret))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinybuffer-iswitch-previous ()
  "Return previous buffer in list."
  `(let* ((rev   (reverse tinybuffer--buffer-list))
          (last  (car rev))
          (rest  (reverse (cdr rev)))
          (ret   last))
     (setq list rest)
     (push last list)                           ;add to the end
     (setq tinybuffer--buffer-list list)        ;update list
     ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-install-default-bindings ()
  "Define default global keys."
  (interactive)
  (global-set-key [(control <)]      'tinybuffer-previous-buffer)
  (global-set-key [(control >)]      'tinybuffer-next-buffer)
  (global-set-key [(control meta <)] 'tinybuffer-iswitch-to-buffer)
  (global-set-key [(control meta >)] 'tinybuffer-sort-mode-toggle))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-start-list  (buffer-pointer list)
  "Let BUFFER-POINTER be first and arrange LIST."
  (let ((start (memq buffer-pointer list))
	(rev   (reverse list))
	before
	ret)
    ;; Basic idea is this, say pointer is at B:
    ;;
    ;; list:   A B C D
    ;; start:    B C D
    ;; rev     D C B A
    ;; before      B A  --> take cdr --> A
    ;;
    ;; ret     start + before = B C D A
    ;;
    (unless start
      (error "No such elt in list %s" buffer-pointer))
    (setq before (cdr-safe (memq buffer-pointer rev)))
    (setq ret start)
    (if before
        (union (reverse start) before))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-buffer-filter (&optional blist)
  "Filter BLIST, which defaults to `buffer-list'.
References:
  `tinybuffer--ignore-regexp'"
  (let (ret)
    (dolist (elt (or blist (buffer-list))  )
      (if (not (string-match tinybuffer--ignore-regexp
                             (buffer-name elt)))
          (push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-sort-buffer-list-1 (&optional blist reverse)
  "Sort BLIST, which defaults to `buffer-list'. Optionally REVERSE."
  (let ((list (or blist (buffer-list))))
    (setq
     list
     (if reverse
         (sort list
               (function
                (lambda (a b)
                  (string< (buffer-name b) (buffer-name a)))))
       (sort list
             (function
              (lambda (a b)
                (string< (buffer-name a) (buffer-name b)))))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-sort-buffer-list  (&optional reverse blist)
  "Sort buffer list, optionally REVERSE and use BLIST."
  (let (sorted
	part
	list)
    (setq sorted (tinybuffer-sort-buffer-list-1 blist reverse))
    ;;  What is this? Okay, you see, when we sort the buffer list...
    ;;     A B C D E F G
    ;;         ^
    ;;     #############  'sorted' holds all
    ;;           %%%%%%%  'part'   contains only these
    ;;
    ;;  We're currently in C, and next one must be D. But if we're
    ;;  at the end, we're in G, and no buffers follow.
    ;;
    ;;  So, to get past G, we have to make list in following way:
    ;;
    ;;      @@@@@@ =  %%%%%    ############
    ;;      list   =  'part' + 'sorted
    ;;
    ;;  Sure, there is redundancy, since the 'sorted' holds all elements,
    ;;  but since we actually ignore buffers in the moving loop, we need
    ;;  all buffers past G.
    (when (setq part (memq (current-buffer) sorted))
      (setq list (cdr part))
      (ti::nconc list sorted))
    sorted))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-buffer-list-next (&optional reverse)
  "Switch to next buffer in list, skipping unwanted ones. Optionally REVERSE.
See variable `tinybuffer--ignore-regexp'."
  (let ((re  tinybuffer--ignore-regexp)
	list
	go)
    (cond
     (tinybuffer--sort-flag
      (setq list (tinybuffer-sort-buffer-list reverse)))
     (reverse
      (setq list (reverse (buffer-list))))
     (t
      (setq list (buffer-list))))
    (setq list (delq (current-buffer) list))
    (dolist (buffer list)
      (unless (string-match re (buffer-name buffer))
        (setq go buffer)                ;Stop and select it
        (return)))
    (if (null go)
        (message
	 (concat
	  "TinyBuffer: No buffers to circulate; "
	  "see `tinybuffer--ignore-regexp'")))
    (if go
        (switch-to-buffer go))))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-init-buffer-list  ()
  "Initialize global variable `tinybuffer--buffer-list'."
  (let ((list (tinybuffer-buffer-filter)))
    (if tinybuffer--sort-flag
        (setq list (tinybuffer-start-list
                    (current-buffer)
                    (tinybuffer-sort-buffer-list-1 list))))
    (setq tinybuffer--buffer-list list)))

;;}}}
;;{{{ code: interactive

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybuffer-iswitch-to-buffer  ()
  "Switch to buffer when RETURN/SPACE/TAB pressed.
Show buffer at echo area. ESC to cancel prompt.

Note:

  The startup time of calling this function may be high, because it has
  to build list of choices and possibly filter out unwanted buffers.

References:

  `tinybuffer--iswitch-to-buffer-keys'    keys to scroll buffer list"
  (interactive)
  (let* ((keys      tinybuffer--iswitch-to-buffer-keys)
         (show-dir  tinybuffer--iswitch-show-directory-flag)
         (go-list   '(?\C-m ?\t ?\ ?\e ?\q ?\Q))
         (quit-list '(?\e ?\q ?\Q))
         (key-back  (nth 0 keys))
         (key-fw    (nth 1 keys))
         (str       (buffer-name))
         (loop      t)
         dir
         fmt
         list
         buffer
         mode
         ch)
    (tinybuffer-init-buffer-list)
    (while loop
      (setq mode
            (with-current-buffer (get-buffer str)
              (cond
               ((eq major-mode 'dired-mode)
                (format "<dired> %s"
                        (symbol-value 'dired-directory)))
               (t
                (format "<%s>" (symbol-name major-mode))))))
      (when show-dir
        (setq dir (or (buffer-file-name (get-buffer str))
                      nil)))
      ;;  This formats the line so that it is visually more pleasant
      ;;  to read. If the file and dir are sticked together, it's
      ;;  hard to distinguish the words.
      ;;
      ;;  FILE      DIR
      (setq fmt
            (if (and str (< (+ (length str) (length dir)) 55))
                "TinyIswich: %-20s %s %s"
              "TinyIswich: %s %s %s"))
      (unless dir
        (setq dir  mode
              mode nil))
      (setq ch (ti::read-char-safe-until
                (format fmt str (or dir "") (or mode "" ))))
      (cond
       ((and ch (char-equal ch key-back))
        (setq buffer (tinybuffer-iswitch-previous)))
       ((and ch (char-equal ch key-fw))
        (setq buffer (tinybuffer-iswitch-next)))
       ((and ch (ti::char-in-list-case ch go-list))
        (setq loop nil)))
      (if buffer
          (setq str (buffer-name buffer))))
    (if (and ch
             buffer
             (not (ti::char-in-list-case ch quit-list)))
        (switch-to-buffer buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybuffer-previous-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (tinybuffer-buffer-list-next 'reverse))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybuffer-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (tinybuffer-buffer-list-next))

;;; ----------------------------------------------------------------------
;;;
(defun tinybuffer-sort-mode-toggle ()
  "Sort mode on/off."
  (interactive)
  (setq tinybuffer--sort-flag (not tinybuffer--sort-flag))
  (message (concat "TinyBuffer: sort mode "
                   (if tinybuffer--sort-flag
                       "on"
                     "off"))))

;;}}}

(provide   'tinybuffer)
(run-hooks 'tinybuffer--load-hook)

;;; tinybuffer.el ends here
