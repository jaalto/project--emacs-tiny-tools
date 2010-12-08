;;; tinyindent.el --- Like indented-text-mode, but minor-mode.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1994-2010 Jari Aalto
;; Keywords:        tools
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
;;{{{ Installation

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into
;;  ~/.emacs startup file:
;;
;;      (require 'tinyindent)
;;
;; OR use this; your .emacs loads quicker
;;
;;      (autoload 'tinyindent-mode    "tinyindent" "" t)
;;      (autoload 'tinyindent-tt-mode "tinyindent" "" t)
;;
;; Suggested keybindings, you're going to use them a lot..
;;
;;      (global-set-key [C-tab]     'tinyindent-tt-mode) ;; this is toggle
;;
;;      ;;;  the first one is for some PC machines (XCeed emulated X)
;;      (global-set-key [S-kp-tab]  'tinyindent-mode)
;;      (global-set-key [S-backtab] 'tinyindent-mode)    ;; this is on/off mode
;;
;;      For some PC:s in nonWindowed, this is same as S-tab
;;      --> check out with C-h l
;;
;;      (define-key esc-map  "OI" 'tinyindent-mode)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, sep 1994
;;
;;      The original auto-indent-mode from autoindent.el was very short
;;      and limited, so I thought I extend it a little...here is the
;;      result.  Thank you Alan for giving me a push to extend your code
;;      into new directions.  When I spoke with Alan and he gave me free
;;      hands, because he hadn't used the .el for quite a long time.
;;
;;      I wasn't satisfied with the  indent-relative function, so
;;      I coded a preprocessor for it. Now the cursor won't jump
;;      all over the line if the previous one was empty. Just
;;      try original  M-x indent-relative when there is empty line above
;;      and you'll see what I mean.
;;
;;      And where this module really shines: Has it ever been easier to line
;;      up variables according to '=' or in within lisp 'let*', or writing
;;      mail messages while this mode is turned on...
;;
;;  Overview of features
;;
;;      o   General block editing or indentation MINOR mode. Replacement for
;;          `indented-text-mode'.
;;      o   Takes over the TAB and BACKSPACE key.
;;      o   Looks back to guess right indentation. and uses relative indent
;;          when not at BOL.
;;      o   Special indentation is suggested if cursor is at BOL and
;;          user defined regexp is matched in line above. (like adding
;;          multiple c++ comments)
;;      o   Extra tinyindent-tt-mode for writing descriptions within comments. This
;;          allows user to choose when to use HARD tab or SOFT tab = relative
;;          to the text above. TAB TAB inserts hard tab, TAB SPC inserts soft
;;          tab.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(ti::package-defgroup-tiny TinyIndent tinyindent-- tools
  "like `indented-text-mode', but minor-mode.
  Overview of features

        o   General block editing or indentation MINOR mode. Replacement for
            `indented-text-mode'.
        o   Takes over the TAB and BACKSPACE key.
        o   Looks back to guess right indentation. and uses relative indent
            when not at BOL.
        o   Special indentation is suggested if cursor is at BOL and
            user defined regexp is matched in line above. (like adding
            multiple c++ comments)
        o   Extra tinyindent-tt-mode for writing descriptions within comments. This
            allows user to choose when to use HARD tab or SOFT tab = relative
            to the text above. TAB TAB inserts hard tab, TAB SPC inserts soft
            tab.")

;;}}}
;;{{{ setup: all

;;; .......................................................... &v-bind ...

(defvar tinyindent--mode-map nil
  "Minor keymap, only for TAB key. Copy of `current-local-map'.")

(defvar tinyindent--mode-prefix-map nil
  "Prefix minor keymap, only for TAB key.")

;;; ......................................................... &v-hooks ...

(defcustom tinyindent--mode-load-hook nil
  "*Hook run when file is loaded."
  :type 'hook
  :group 'TinyIndent)

(defcustom tinyindent--mode-hook nil
  "*Hook run when function `tinyindent-mode' turned on."
  :type 'hook
  :group 'TinyIndent)

(defcustom tinyindent--mode-define-keys-hook 'tinyindent-mode-map-define-keys
  "*Hook to define keys for mode."
  :type 'hook
  :group 'TinyIndent)

;;; .......................................................... &v-mode ...

(defvar tinyindent-mode nil
  "If set, indicates that auto-indent mode is active.
This variable isautomatically set by invoking \\[tinyindent-mode].")
(make-variable-buffer-local 'tinyindent-mode)

(defvar tinyindent-tt-mode nil
  "Hard tab submode.")
(make-variable-buffer-local 'tinyindent-tt-mode)

;;; ....................................................... &v-private ...

(defvar tinyindent--RET nil
  "Global return value used, when multiple values are neede.
Shouldn't interest regular user.")
(make-variable-buffer-local 'tinyindent--RET)

(defvar tinyindent--cp 0  "Internal. Current point.")
(make-variable-buffer-local 'tinyindent--cp)

(defvar tinyindent--cl 0  "Internal. Current line.")
(make-variable-buffer-local 'tinyindent--cl)

;;; ........................................................ &v-public ...
;;; User configurable

;; - The BOL is special, because when you write code, the crucial
;;   point is line start: you decide indentation or cursor positioning with
;;   that first keystroke.

(defcustom tinyindent--bol t
  "*Flag that determines if beg. of line should be treated differently."
  :type  'boolean
  :group 'TinyIndent)

(defcustom tinyindent--special-regexp
  (concat
   "^[ \t]*\\(//\\|\#\\|!\\|REM\\)[ \t]*"
   ;;   don't put ;;+, since someone may draw ;;;;;;;;;;...
   "\\|^[ \t]*;;:?;?[ \t]*")
  "*Special indent at the beginning of line.
Sometimes single indent isn't enough. For example it would be convenient
to write long C++ comments by hitting the TAB on the next line. Original
RE handles considers these as special cases.
!          .Xdefauls or X-related files
#          Perl, awk, shell
//         C++
;;;        Lisp
REM        Oracle Sqlplus, SQL files in general"
  :type  'string
  :group 'TinyIndent)

(defcustom tinyindent--mode-str-orig " Tii"
  "*String to be displayed in mode line."
  :type 'string
  :group 'TinyIndent)

(defcustom tinyindent--tt-mode-str-orig " TiiT"
  "*String to be displayed in mode line."
  :type  'string
  :group 'TinyIndent)

;;  This is not a user variable

(defvar tinyindent--mode-name tinyindent--mode-str-orig
  "Current minor mode status displayed. Changed dynamically.")
(make-variable-buffer-local ' tinyindent--mode-name)

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinyindent-mode-map-define-keys ()
  "Defines keybindings to `tinyindent--mode-map'."

  (define-key  tinyindent--mode-map "\t" 'tinyindent-tab-key)

  ;;  e.g. lisp-mode uses backward-delete-char-untabify which is
  ;;  uncomfortable in editing.
  ;;
  ;;  The 2nd bind works in X env only

  (define-key  tinyindent--mode-map "\177" 'delete-backward-char))

;;; ----------------------------------------------------------------------
;;;
;;;### (autoload 'tinyindent-install-mode "tinyindent" t t)
(ti::macrof-minor-mode-install
 tinyindent-install-mode
 tinyindent-mode
 tinyindent--mode-map
 tinyindent--mode-prefix-map
 tinyindent--mode-name
 tinyindent--mode-define-keys-hook)

;;; ----------------------------------------------------------------------
;;;
(defun tinyindent-confirm (msg)
  "Confirms action with MSG.
RET/SPC = ok. The real character pressed is available
thru global variable `tinyindent--RET'."
  (setq tinyindent--RET (ti::read-char-safe msg))
  (if (and (characterp tinyindent--RET)
           (or (char-equal tinyindent--RET ?\C-m)
               (char-equal tinyindent--RET ?\ ))) ; RET/SPC
      t
    nil))

;;}}}
;;{{{ engine

;;; ----------------------------------------------------------------------
;;;
(defun tinyindent-special-handle ()
  "Handle some special lines -- like `gin-mode',  but simpler.
Supposes that point is at the beginning of investigated line.
Moves point 1 line forward af ter done.

Returns:
  filling pattern to use at front of line or nil"
  ;;  Look for some special lines, like C++
  (let ((s-re tinyindent--special-regexp)
	fill
	line)
    (when (looking-at s-re)
      ;;  back to original line
      ;;
      (forward-line 1)          ;otherwise visible to user when asking
      (when (tinyindent-confirm "indent special? ")
        (setq line
              (save-excursion
                (goto-char  (- (point) 1))
                (ti::read-current-line)))
        (string-match s-re line)
        (setq fill
              (substring line (match-beginning 0) (match-end 0)))))
    fill))

;;; ----------------------------------------------------------------------
;;;
(defun tinyindent-tab-key  ()
  "Handle tab key.
Check if TinyMail is present and call Header completions in header area,
in BODY use relative indent."
  (interactive)
  (or (and (featurep 'tinymail)
           (ti::mail-mail-p)
           (< (point) (ti::mail-hmax))
           (ti::funcall 'timi-complete-key))
      (tinyindent-relative)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyindent-relative ()
  "Almost like `indent-relative', but handles some special cases.
- if the above line if NOT empty, then we indent relatively automatically
- if above line IS empty, then ask if normal TAB/relative indent.

References:
  `tinyindent--special-regexp'"
  (interactive)
  (let ((bolp-flag     tinyindent--bol)
	(p             (point))
	(cur-col       (current-column))
	(imode         t)
	(SPC           ?\ )
	prev-empty
	prev-col
	bp ep                          ;BEG END point
	fill
	line
	ch
	skip)
    (catch 'cancel
      (save-excursion
        (save-excursion
          (setq bp (line-beginning-position))
          (setq ep (line-end-position))
          (forward-line -1)
          (setq prev-empty (looking-at "[ \t]*$"))
          (end-of-line)
          (setq prev-col (current-column)))
        ;;  make sure these are NOT nil
        (if (null tinyindent--cp)
	    (setq tinyindent--cp 0))
        (if (null tinyindent--cl)
	    (setq tinyindent--cl 0))
        ;;  Count lines has A BUG! , If I'm at the beg of line
        ;;  or 1 char forward it gives different values!
        (setq line (count-lines 1 p))
        (if (or (eq p bp)
		(eobp))
            (setq line (1+ line)))      ;BEG of line error
        ;;   - the user has answered to question, we are on the same line
        ;;   - if he is at the beginning, then ALWAYS ask (forced ask)
        (if prev-empty
            (if (and                    ;already asked ?
                 (>= tinyindent--cp bp)
                 (<= tinyindent--cp ep))
                (setq skip 1))
          (if (null (bolp))
              (setq skip 2))            ;BOL ?
          (if (< prev-col cur-col)
              ;;  previous line is shorter
              (setq skip 3)))
        (if skip
            (throw 'cancel t))          ;we were on this line already
        (setq tinyindent--cl line)      ;update line number
        (setq tinyindent--cp p)         ;current point position
        ;;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ;;  The real engine
        (setq tinyindent--RET nil)
        (cond
         ((bobp)
          (tab-to-tab-stop))
         ((bolp)
          (forward-line -1)             ;Check previous line
          (if (setq fill (tinyindent-special-handle))
              (progn
                (throw 'cancel t))
            (forward-line 1)
            (when tinyindent-tt-mode
              (if (null bolp-flag)
                  (setq imode t)
                (setq imode (tinyindent-confirm "indent relative?")))))
          ;; this was pressed
          (setq ch tinyindent--RET)))))
    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch end ^^^
    (if fill
        (insert fill)                 ;see save-excursion, fill is set
      (cond
       (tinyindent-tt-mode
        (setq ch (or ch (ti::read-char-safe)))
        (cond
         ((not (characterp ch)))
         ((eq ch SPC)
          (indent-relative))
         ((char-equal ch ?\t)
          (tab-to-tab-stop)
          ;;  tab stop already does this
          (setq ch nil))
         (t
          (indent-relative))))
       (t
        (cond
         (imode
          (indent-relative)             ;other char follows
          (if (eq ch SPC)            ;kill space, because it means YES
              (setq ch nil)))
         (t
          (tab-to-tab-stop)             ;use hard tab
          ;;  kill the TAB char
          (setq ch nil)))))
      ;;  the TAB char automatically moves to tab-to-tab-stop
      ;;  if it's inserted
      (if (and (characterp ch)
               (ti::print-p ch)
               (not (eq ch SPC)))
          ;; this is already handled
          ;; add the character, don't loose it
          (insert ch)))))

;;}}}
;;{{{ modes

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyindent-tt-mode (&optional arg)
  "Toggle variable `tinyindent-tt-mode' with ARG. See description in `tinyindent-mode'."
  (interactive "P")
  (ti::bool-toggle tinyindent-tt-mode arg) ;toggle mode variable
  (cond
   (tinyindent-tt-mode
    (unless tinyindent-mode    ;turn on the major mode tinyindent-mode
      (tinyindent-mode)                 ;turn it on
      (setq tinyindent-tt-mode t))
    (setq tinyindent--mode-name tinyindent--tt-mode-str-orig))
   (t
    (setq tinyindent--mode-name tinyindent--mode-str-orig)))
  (ti::compat-modeline-update))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyindent-mode (&optional arg)
  "Toggle relative indentation mode with ARG.

Indentation is determined according to previous lines. Special
indent happens only at the beginning of line, where user is asked if
he wants to have relative or \"hard\" indentation.

Abount function `tinyindent-tt-mode'

This isn't really mode. It just turns one flag on in `tinyindent-mode', so that
it behaves a little differently. If the `tinyindent-mode' is not running, it
wiil be turned on. turning off `tinyindent-tt-mode' _does_not_ end `tinyindent-mode'.

Sometimes you want to control between 'hard' tab and 'soft' tab, ie.
relative indent. This mode causes second character to be read after
tab key is hit. The following happens:

TAB TAB     inserts hard tab
TAB SPC     indent relative without inserting space char.
TAB x       indents relative and inserting character x

\\{tinyindent--mode-map}"
  (interactive "P")

  (if (null (assq 'tinyindent-mode minor-mode-alist))
      (tinyindent-install-mode))

  (ti::bool-toggle tinyindent-mode arg) ;toggle mode variable

  (cond
   (tinyindent-mode
    (unless tinyindent-tt-mode
      (setq tinyindent--mode-name
            tinyindent--mode-str-orig))
    (run-hooks 'tinyindent--mode-hook))
   (t
    (setq tinyindent-tt-mode nil)))
  (ti::compat-modeline-update))

;;}}}

(add-hook 'tinyindent--mode-define-keys-hook 'tinyindent-mode-map-define-keys)
(provide   'tinyindent)
(run-hooks 'tinyindent--mode-load-hook)

;;; tinyindent.el ends here
