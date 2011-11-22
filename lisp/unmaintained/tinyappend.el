;;; tinyappend.el --- A simple text gathering to buffer utility.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1994-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; To get information on this program, call M-x tinyappend-version
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

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;;
;;      (require 'tinyappend)
;;
;;  Autoload, prefer this one, your emacs starts quicker.
;;
;;      (autoload 'tinyappend-beg "tinyappend"  "" t)
;;      (autoload 'tinyappend-end "tinyappend"  "" t)
;;
;;  If you do not want the default key bindings, add this before the
;;  require command
;;
;;      (setq tinyappend-:load-hook nil)
;;
;;  If you have any questions, suggestions, use this function
;;
;;      M-x tinyappend-submit-bug-report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, March 1994
;;
;;      This package does nothing fancy, it gathers text from buffers with
;;      few key bindings. Later you can then peek on that buffer, arrange
;;      text etc. `C-x' `a' is handy when appending data to buffer, but
;;      it's annoying that you have to give "buffer name" all the time This
;;      one adds to buffer "*append*" automatically, creating one if it
;;      doesn't exist.
;;
;;      I'd strongly recommend you to keep `transient-mark-mode' (Emacs) on
;;      all the time, so that you can see if you're adding a selected
;;      region into the *append* buffer. If the region is not active, these
;;      functions normally add the current line to the *append* buffer.
;;
;;  Suggested default bindings
;;
;;          C-c +       Append to the end
;;          C-c _       (underscore) Append to the beginning
;;          C-c -       Kill (empty) *append* buffer
;;          C-c |       Yank text from append buffer
;;

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinyliba)

(eval-when-compile
  (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinyAppend tinyappend-: extensions
  "Gather text to separate cut buffer.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinyappend-:load-hook nil
  "*Hook run when file has been loaded."
  :type 'hook
  :group 'TinyAppend)

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyappend-:buffer  "~/.append"
  "*Buffer where to save text.
If this variable has star at the beginning of name, like *append*,
it is considered that the buffer doesn't need saving to any file.

Otherwise if file with the same name exists when tinyappend.el is
being loaded, the buffer will hold the contents of the file
_only_ if buffer is empty initially."
  :type 'string
  :group 'TinyAppend)

;;}}}
;;{{{ version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinyappend-version "tinyappend" "Display commentary." t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinyappend.el"
   "tinyappend"
   tinyappend-:version-id
   "$Id: tinyappend.el,v 2.40 2007/05/01 17:20:42 jaalto Exp $"
   '(tili-:version-id
     tinyappend-:load-hook
     tinyappend-:buffer)))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinyappend-install-default-key-bindings ()
  "Install default key bindings."
  (interactive)
  (global-set-key  "\C-c=" 'tinyappend-end)     ;; non-shift key
  (global-set-key  "\C-c-" 'tinyappend-beg)     ;; non-shift key
  (global-set-key  "\C-c_" 'tinyappend-kill)
  (global-set-key  "\C-c|" 'tinyappend-yank))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyappend-line-area-args (msg)
  "Return region of current line: (beg end MSG) including newline."
  `(list
    (line-beginning-position)
    (save-excursion
      (end-of-line)
      (ignore-errors (forward-char 1))  ;get newline, unless EOB
      (point))
    ,msg))

;;; ----------------------------------------------------------------------
;;;
(defun tinyappend-get-buffer ()
  "Create `tinyappend-:buffer' and initializes its content from file if it exists.

Return:
  buffer pointer"
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create tinyappend-:buffer))
    (when (not (char= (aref tinyappend-:buffer 0) ?\* ))
      (if (and (file-exists-p tinyappend-:buffer) ;history file exists ?
               (ti::buffer-empty-p))
          (insert-file-contents tinyappend-:buffer))
      ;;  link to file so that Emacs asks to save the buffer
      ;;  when you quit with C-x C-c
      (setq buffer-file-name (expand-file-name tinyappend-:buffer))
      (rename-buffer tinyappend-:buffer)))
  (get-buffer tinyappend-:buffer))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinyappend-append-to-buffer (beg end &optional arg msg verb)
  "Store BEG END with ARG and MSG to `tinyappend-:buffer'.
Default is to store the end of buffer. Prefix argument ARG means:

  0   = kill append buffer
  \\[universal-argument] = adds to the beginning

VERB allows verbose messages."
  (ti::verb)
  (setq msg (or msg  ""))
  (save-excursion
    (if (not (eq 0 arg))
        (copy-region-as-kill beg end))
    (set-buffer (tinyappend-get-buffer))

    (cond                               ; According to prefix
     ((eq arg 0)                        ; yank to the beginning
      (kill-buffer (current-buffer))
      (and verb (message (concat "TIA buffer killed"))))
     ((and (not (null arg))             ; yank to the beginning
           (listp arg))
      (goto-char (point-min)) (yank)
      (if verb
          (message (format "*appended %s BEG*" msg))))
     (t
      (goto-char (point-max)) (yank)
      (if verb
          (message  (format "*appended %s*" msg)))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyappend-end (&optional beg end msg)
  "Store region BEG END with MSG or current line to the end of `tinyappend-:buffer'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end) "region")
     (tinyappend-line-area-args "line")))
  (tinyappend-append-to-buffer beg end nil msg 'verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyappend-beg (&optional beg end msg)
  "Store BEG END with MSG or current line to the beginning of `tinyappend-:buffer'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end) "region")
     (tinyappend-line-area-args "line")))
  (tinyappend-append-to-buffer beg end '(4) msg 'verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyappend-kill ()
  "Kill `tinyappend-:buffer' buffer."
  (interactive)
  (if (get-buffer tinyappend-:buffer)
      (kill-buffer tinyappend-:buffer)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyappend-yank (&optional kill)
  "Yank `tinyappend-:buffer' to the current position. Optionally KILL `tinyappend-:buffer'."
  (interactive "P")
  (if (null (get-buffer tinyappend-:buffer))
      (message (concat "Can't yank, there is no buffer: " tinyappend-:buffer))
    (insert-buffer-substring tinyappend-:buffer)
    (if kill
        (tinyappend-kill))))

;;}}}

(if (not (get-file-buffer tinyappend-:buffer))
    (tinyappend-get-buffer))

(provide   'tinyappend)
(run-hooks 'tinyappend-:load-hook)

;;; tinyappend.el ends here
