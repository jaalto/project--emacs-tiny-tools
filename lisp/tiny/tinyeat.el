;; tinyeat.el --- Eat blocks of text at point, forward and backward

; This file is not part of Emacs

;;{{{ Documentation

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
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; emacs startup file.
;;
;;      ;; Rebind BACKSPACE and DEL-related keys
;;      (setq tinyeat--load-hook '(tinyeat-install))
;;      (require 'tinyeat)
;;      (global-set-key "\M-z"   'tinyeat-kill-buffer-lines-main)
;;
;; Or use autoload and Emacs starts up faster
;;
;;      (autoload 'tinyeat-kill-line                   "tinyeat" "" t)
;;      (autoload 'tinyeat-kill-line-backward          "tinyeat" "" t)
;;      (autoload 'tinyeat-kill-buffer-lines-point-max "tinyeat" "" t)
;;      (autoload 'tinyeat-kill-buffer-lines-point-min "tinyeat" "" t)
;;      (autoload 'tinyeat-forward-preserve            "tinyeat" "" t)
;;      (autoload 'tinyeat-backward-preserve           "tinyeat" "" t)
;;      (autoload 'tinyeat-delete-paragraph            "tinyeat" "" t)
;;      (autoload 'tinyeat-zap-line                    "tinyeat" "" t)
;;      (autoload 'tinyeat-join-lines                  "tinyeat" "" t)
;;
;;      ;; Lines
;;      (global-set-key "\M-k"                'tinyeat-kill-line-backward)
;;      (global-set-key "\C-\M-k"             'tinyeat-zap-line)
;;      (global-set-key (kbd "C-S-k")         'tinyeat-zap-line)
;;
;;      ;; Generic
;;      (global-set-key (kbd "<M-backspace>") 'tinyeat-backward-preserve)
;;      (global-set-key (kbd "<S-backspace>") 'tinyeat-delete-whole-word)
;;
;;      (unless window-system
;;        (global-set-key (kbd "M-SPC") 'tinyeat-delete-whole-word))
;;
;;      (global-set-key "\M-d"                'tinyeat-forward-preserve)
;;      (global-set-key "\C-\M-d"             'tinyeat-delete-paragraph)
;;
;;      ;; Other
;;      (global-set-key "\C-S-y"              'tinyeat-yank-overwrite)
;;      (global-set-key (kbd "C-S-SPC)        'tinyeat-join-lines)
;;
;; Investigate problems with:
;;
;;      M-x tinyeat-debug-toggle
;;      M-x tinyeat-debug-show

;;}}}
;;{{{ Documentation

;;; Commentary:

;;  Preface, overview of features
;;
;;      o   Determines how much text should be eaten around current cursor
;;          position. Eat extra spaces, extra newlines, next word
;;          next statement, next comment ... whatever is appropriate
;;      o   Can also eat inside mixed case word: WordsThatAreLikeThis
;;      o   Yank and "overwrite" text under cursor with Meta mouse-2 or
;;          `Meta' `C-y'. (Std Emacs in `overwrite-mode' doesn't allow you to
;;          yank and overwrite at the same time.)
;;
;;  Today's suggestion
;;
;;      If using Windowed Emacs and the prompt is at minibuffer and
;;      you would like to clean the whole prompt, hit key
;;      `Esc-backspace'. In non-windowed emacs, you have to repeat the
;;      keystroke as needed (this is due to "backspace key detection
;;      problem syndrome").
;;
;;  Non-windowed and Windowed Emacs
;;
;;      This package works _best_ in windowed Emacs, because in
;;      windowed environment you can use the modifiers *Control*,
;;      *Alt* and *Meta* freely with the backspace key. The idea of
;;      this package is to overload your single key, `backspace', as
;;      much as possible with various delete functionalities.
;;      The differences to non-windowed environment are as follows.
;;      Key marked with (*) is not available under non-window system.
;;
;;                          was             now
;;          ---------------------------------------------------------
;;          M-d             kill-word       tinyeat-forward-preserve
;;          M-k             kill-sentence   tinyeat-kill-line-backward
;;          S-Backspace     <none>          tinyeat-delete-whole-word  (*)
;;          C-M-d           down-list       tinyeat-delete-paragraph
;;          C-M-k           kill-sexp       tinyeat-zap-line
;;          C-S-y           yank-pop        tinyeat-yank-overwrite (*)
;;          C-S-SPC	    set-mark-command tinyeat-join-lines (*)
;;
;;  Story behind this package
;;
;;      One day the developer got frustrated of moving cursor around the
;;      point and using keys del or backspace to write C++ and LISP
;;      symbols. The start situation was like this while cursor was at (*):
;;
;;          (defun lisp-symbol-name-myname          ()
;;                                  *
;;
;;      He decided to change 'myname' to something else. Normally he
;;      would reach out for M-d for `kill-word' to delete `myname' and
;;      type the new name:
;;
;;          (defun lisp-symbol-name-mynew           ()
;;                                       *
;;
;;      Next, he noticed that there were extra spaces involved.
;;      A call to `fixup-whitespace' would make it go away ... Hmm that was
;;      not bound to any key by default (in this particular Emacs used
;;      at the time), so he had to type it the long way round: `M-x'
;;      `fixup-whitespace'. His thoughts were: "Oh, I should have bound it
;;      to some easily reacheable key". The story continues.
;;      He looked at the function once more and decided that the name
;;      `symbol-name-mynew' wasn't a good one after all. He decided to
;;      delete 3 words backward. Now, how do you do that?
;;
;;          (defun lisp-symbol-name-mynew ()
;;                                       *
;;
;;      He murmurs, "where is the command to delete backward ...". After
;;      spending valuable minutes to find the `delete-backward-word'
;;      command with the `M-x' `apropos',  hitting the page up and down
;;      to find anything that would look like what he wanted, he leaned
;;      back with despair, "Doh, there is no such command". Silently
;;      he ends up tapping the backspace until he reaches the correct point:
;;
;;          (defun lisp- ()
;;                      *
;;
;;      and starts typing a new name...
;;
;;          (defun lisp-my-func ()
;;
;;      All is perfect for a moment. Then, he notices that there are too
;;      many newlines above the newly created function and says to himself:
;;      "I really should delete those 5 extra empty lines above the
;;      function. Now, how do I kill backward 5 empty lines backward? The
;;      `kill-line' in C-k kills only forward" ...". The story teller
;;      rests here and leaves reader's imagination to fly forward.
;;
;;  Lesson learned
;;
;;      As you can notice, people often spend most of the time to
;;      position the cursor to the right spot and deleting text over
;;      there.. over here .. typing more .. changing our mind ... and
;;      so on.
;;
;;      It was time to do something creative, so that user wouldn't have to
;;      worry about the deletion of text so much. This package provides
;;      atempts to provide _smart_ deleting capabilities: whether it was
;;      to delete forward of backward. Naturally the art of deletion is
;;      not accurate, a few guesses need to be made and they may be
;;      wrong. If it so happens that a lot of text have suddenly
;;      retired (vanished, vaporized) from the buffer, remember, there
;;      is no need to panic. Emacs has friendly `undo' (C-_ or C-x u).
;;
;;  Default keybindings
;;
;;      Line delete
;;
;;          <<           >>           <<>>
;;          M-k          C-k          C-M-k or C-s-k
;;                                    To zap whole line
;;
;;      Chunk delete: words, spaces, symbols ...
;;
;;          <            >            <>                 \//\
;;          M-Backspace  C-backspace  S-Backspace        C-M-d  / C-S-backspace
;;                       M-d          Delete whole word  Paragraph delete
;;
;;      These function have no defult binding in `tinyeat-install', but
;;      you might find suitable keys for them:
;;
;;         M-x tinyeat-erase-buffer
;;         M-x tinyeat-kill-buffer-lines-main
;;
;;  Known Bugs
;;
;;      This package heavily relies on various modifiers that can be
;;      attached to the *BACKSPACE* key. Binding the backspace can be
;;      a difficult subject under Unix. For example the *Alt* (Meta)
;;      key may not be recognized under terminala. It may be possible
;;      to make backspace known by exming the key events with xev(1)
;;      and makign the needed modification to environment with
;;      `xmodmap(1)' or `keycaps(1)'. In some terminals that Alt-key
;;      simply won't be seen and the Meta-key substitute o use ESC
;;      under Emacs is not really practical in the sense where this
;;      package was developed: for quick editing.
;;
;;      Worse, in the same environment Emacs and XEmacs may disagree
;;      what BACKSPACE means. An example: here are the results from
;;      XEmacs 20.4 and Emacs 20.3 under Redhat Linux 6.2:
;;
;;                              XEmacs          Emacs
;;
;;          <esc backspace>     M-backspace     ESC DEL
;;          <shift backspace>   delete          S-delete
;;          <alt backspace>     <nothing>       <nothing>
;;
;;      There is nothing this package can do to cope with these
;;      changes in key symbol or the environment chnages. If you can,
;;      try to get the ALT and and SHIFT-modifiers working for use
;;      with backspace and everything is well. If that is not
;;      possible, the power of the predefined keybindings are mostly
;;      left unused and you have to find alternative key combinations.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: variables

(require 'tinylibm)

(ti::package-defgroup-tiny TinyEat tinyeat-- extension
  "Eat blocks of text forward, backward.
Overview of features

        o   Determine how much text should be eaten around current cursor
            position. Eat extra spaces, extra newlines, next word
            next statement , next comment ... whatever is appropriate
        o   Can also eat only 'inside' words: WordsThatAreLikeThis")

(defcustom tinyeat--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyEat)

(defcustom tinyeat--verbose-flag t
  "*Non-nil means allow informational messages to be displayed."
  :type  'boolean
  :group 'TinyEat)

(defcustom tinyeat--non-word-chars
  "][=_~+!@#$%&*:;'\"`,.<>(){}$<>?/|\\\\\n \t-"
  "*Characters that _stop_ eating word.
Character ][ be in this order and in the beginning of variable,
because this string is converted into regexp later."
  :type  '(string :tag "Charset")
  :group 'TinyEat)

(defcustom tinyeat--eat-full-word-charset  "^][ \t\n(){};'\","
  "*Character set to use when determining word boundary.
Normally word is terminated by whitespace or newlines."
  :type  '(string :tag "Charset")
  :group 'TinyEat)

;;}}}
;;{{{ install

;;;###autoload (autoload 'tinyeat-debug-toggle "tinyeat" "" t)
;;;###autoload (autoload 'tinyeat-debug-show   "tinyeat" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinyeat" "--"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-install-default-bindings-terminal ()
  "Install extra binding for dummy terminals."
  (let ((status (lookup-key global-map (kbd "ESC [ 3"))))
    ;;  Will be number, if this is a prefix key
    (when (or (integerp status)
              (and status
                   (keymapp status)))
      ;;  C-delete
      (global-set-key (kbd "ESC [ 3 ^") 'tinyeat-forward-preserve)
      ;;  S-delete
      (global-set-key (kbd "ESC [ 3 $") 'tinyeat-delete-whole-word)
      ;; C-S-delete
      (global-set-key (kbd "ESC [ 3 @") 'tinyeat-delete-paragraph))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-install-default-bindings ()
  "Bind default keys to various 'eat' functions."
  (interactive)

  ;; was `kill-sentence'
  (global-set-key "\M-k"                'tinyeat-kill-line-backward)

  ;; C-M-k: Works both in Windowed and non-Windowed Emacs. Unfortunately in
  ;; windowed Linux/Gnome C-M-k runs "Lock Screen", we define C-S-k
  ;; asbackup.

  (global-set-key "\C-\M-k"             'tinyeat-zap-line) ; kill-sexp
  (global-set-key (kbd "C-S-k")         'tinyeat-zap-line)

  (global-set-key (kbd "C-S-y")         'tinyeat-yank-overwrite)

  (global-set-key (kbd "C-S-y")         'tinyeat-yank-overwrite)
  (global-set-key (kbd "C-S-SPC")	'tinyeat-join-lines)

  ;;  Alt-backspace
  (global-set-key (kbd "<M-backspace>") 'tinyeat-backward-preserve)

  ;;  was `kill-word'
  (global-set-key "\M-d"                'tinyeat-forward-preserve)
  (global-set-key (kbd "<C-backspace>") 'tinyeat-forward-preserve)
  ;; secondary backup
  (global-set-key (kbd "<C-delete>")    'tinyeat-forward-preserve)
  (global-set-key (kbd "<C-deletechar>")'tinyeat-forward-preserve)
  (global-set-key (kbd "<M-delete>")    'tinyeat-forward-preserve)

  (global-set-key (kbd "<S-backspace>") 'tinyeat-delete-whole-word)
  (global-set-key (kbd "<S-delete>")    'tinyeat-delete-whole-word)
  ;; Was just-one-space
  (global-set-key "\M-\ "               'tinyeat-delete-whole-word)

;;;    (when (ti::xemacs-p)
;;;      (global-set-key (kbd "M-BS")   'tinyeat-backward-preserve)
;;;      (global-set-key (kbd "C-BS")   'tinyeat-forward-preserve))

  ;;  Was `down-list'
  (global-set-key "\C-\M-d"               'tinyeat-delete-paragraph)
  (global-set-key (kbd "<C-S-backspace>") 'tinyeat-delete-paragraph)
  (global-set-key (kbd "<C-S-delete>")    'tinyeat-delete-paragraph)

  (unless (ti::compat-window-system)
    (tinyeat-install-default-bindings-terminal))
  (message "TinyEat: ** [WARN] some existing keys were bound to TinyEat functions."))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-install (&optional arg)
  "Call `tinyeat-install-default-bindings' with ARG."
  (interactive)
  (tinyeat-install-default-bindings))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(put 'tinyeat-repeat-macro 'lisp-indent-function 1)
(defmacro tinyeat-repeat-macro (end &rest body)
  "Loop using VAR from BEG to END and do BODY."
  `(loop for var from 1 to ,end
	 do
	 (progn
	   ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinyeat-verbose-macro 'lisp-indent-function 0)
(defmacro tinyeat-verbose-macro (&rest body)
  "Run BODY if tinyeat--verbose-flag' is set.
Minibuffer is excluded."
  `(when (and (not (ti::buffer-minibuffer-p))
              tinyeat--verbose-flag)
     ,@body))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-erase-buffer  ()
  "Erase buffer. If read-only buffer, do nothing."
  (interactive)
  (unless buffer-read-only
    (if (ti::buffer-minibuffer-p)
        ;; `erase-buffer' signals error in minibuffer:
        ;;  read-only-text (like that in prompt)
        (delete-region
         (line-beginning-position)
         (line-end-position))
      (erase-buffer))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-zap-line (&optional count)
  "Kill COUNT times whole lines including the final newline."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (beginning-of-line)
                        (if (looking-at "\n")
                            (kill-line)
                          (kill-line 1))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-backward (&optional count)
  "Eat backward COUNT times. See `tinyeat-eat'."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (tinyeat-eat 'back)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-backward-preserve (&optional count)
  "Eat forward, but handle spaces differently. See `tinyeat-eat'."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (tinyeat-eat 'back 'preserve)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-forward (&optional count)
  "Eat forward COUNT times. See `tinyeat-eat'."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (tinyeat-eat)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-forward-preserve (&optional count)
  "Eat forward COUNT times. See `tinyeat-eat'."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (tinyeat-eat nil 'preserve)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-join-lines (&optional count)
  "Join this and next line with one space, and go to the joint."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (end-of-line)
                        (unless (eobp)
                          (kill-line)
                          (fixup-whitespace))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-delete-whole-word-1-charset (charset)
  "Delete word based on CHARSET. See `skip-chars-backward' and *-forward."
  (let (beg
	end)
    (skip-chars-backward charset)
    (setq beg (point))
    (skip-chars-forward  charset)
    (setq end (point))
    (delete-region beg end)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-delete-whole-word-1-main  (&optional charset)
  "Delete one word at point. Optional CHARSET is for `skip-chars-backward'.
References:
  `tinyeat--eat-full-word-charset'"
  (interactive)
  (or charset
      (setq charset tinyeat--eat-full-word-charset))
  (cond
   ((or (looking-at "[ \t\r\n][ \t\r\n]")
        (and (not (bolp))
             (string= " " (char-to-string (preceding-char)))
             (looking-at "[ \t\r\n]")))
    (fixup-whitespace))
   ((looking-at "[ \t\r\n]")
    (delete-horizontal-space))
   (t
    (tinyeat-delete-whole-word-1-charset charset)
    ;;      (unless (zerop (skip-chars-forward " \t"))   ; delete white space
;;;      (delete-region beg (point)))
    nil)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-delete-whole-word (&optional count)
  "Delete COUNT words at point.

- If there are multiple whitespaces around, call `fixup-whitespace'.
- If on top of only one whitespcae, call `delete-horizontal-space'.
- If on top of word, delete whole word.

References:
  `tinyeat--eat-full-word-charset'"
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (tinyeat-delete-whole-word-1-main)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-line (&optional count)
  "Like `kill-line'; COUNT times. Killed text isn't put into cut buffer.
This way you can retain mouse selection in cut buffer."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (cond
                         ((eobp))       ;Do nothing
                         ((eolp)
                          (delete-char 1))
                         (t
                          (delete-region (point) (line-end-position))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-line-backward (&optional count)
  "Like `kill-line' back; COUNT times. Killed text isn't put into cut buffer."
  (interactive "p")
  (tinyeat-repeat-macro (or count 1)
                        (when (not (bobp))
                          (if (bolp) ;Kill previous newline (shift line up)
                              (backward-delete-char 1)
                            (delete-region (point) (line-beginning-position))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-buffer-lines-point-max (&optional back)
  "Kill to the `point-max' or if BACK, then to the `point-min'."
  (interactive "P")
  (cond
   (back
    (delete-region (point) (point-min)))
   (t
    (delete-region (point) (point-max)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-buffer-lines-point-min ()
  "Kill until `point-min'."
  (interactive "p")
  (tinyeat-kill-buffer-lines-point-max 'back))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-kill-buffer-lines-main (&optional backward)
  "Kill until `point-max' or if BACKWARD, until `point-min'."
  (interactive "p")
  (if backward
      (tinyeat-kill-buffer-lines-point-min)
    (tinyeat-kill-buffer-lines-point-max)))

;;}}}
;;{{{ misc2

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun  tinyeat-delete-paragraph ()
  "Delete current paragraph, separated by empty lines."
  (interactive "*")
  (let ((re "^[ \t]*$")
	beg
	end)
    (cond
     ((save-excursion                   ;sitting on empty line
        (beginning-of-line)         ;kill empty lines around the point
        (looking-at "^[ \t]*$"))
      (skip-chars-backward " \t\n")
      (forward-line 1)
      (setq beg (point))
      (skip-chars-forward " \t\n")
      (forward-line -1)
      (setq end (point)))
     ((save-excursion
        ;;  Kill paragraph.
        (if (not (re-search-backward re nil t))
            (setq beg (point-min))
          (beginning-of-line)
          (forward-line 1)              ;exlude space
          (setq beg (point))))
      (save-excursion
        (cond
         ((re-search-forward re nil t)
          (beginning-of-line)
          (setq end (point)))
         (t
          (if (not (eq beg (point-max)))
              (setq end (point-max))
            (setq end (point-min))))))))
    (if (and (not (and beg end))
             (not (ti::buffer-minibuffer-p)))
        (message "TinyEat: Can't find paragraph bounds (empty line)")
      (unless (eq beg end)
        (kill-region beg end)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-space-delete-at-point (&optional back preserve)
  "Delete whitespace at point. Optionally BACK.
If optional PRESERVE is given, then deletes towards the BACK only.
if BACK is non-nil the deletion is headed backward."
  (let ( ;; character function selection
	(charf   (if back 'skip-chars-backward 'skip-chars-forward))
	(p       (point))
	(ch      (ti::buffer-read-char back 0)) ;sitting on it if looking fwd
	(ch-p    (ti::buffer-read-char back -1))
	(ch-n    (ti::buffer-read-char back 1)))
    (cond
     ((and back
           (ti::space-p (or ch-p ?\ ))
           (char-equal ch ?\n))
      (delete-horizontal-space)
      (if (null back)
          (tinyeat-verbose-macro
	    (message "TinyEat: line cleared")))
      t)
     ((char-equal ch ?\n)                    ;no spaces before, do nothing
      nil)
     ((or (and ch ch-n
               (ti::space-p ch)
               (ti::space-p ch-n))      ;at least two spaces
          (and ch ch-p
               (ti::space-p ch-p)
               (ti::space-p ch)))
      (if (null preserve)
          (fixup-whitespace)
        (funcall charf " \t")
        (delete-region p (point)))
      t)
     (t
      (delete-horizontal-space)
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyeat-word-move-point (&optional back)
  "Move to suitable word kill point. Mixed case words are special.
Optionally BACK.
See variable `tinyeat--non-word-chars' how to delimit word parts.

* = cursor position

ThisIsMixedWord --> ThisIsMixedWord
*                       *
THISmixedWord   --> THISmixedWord
*                       *"
  (let* ((fid         "tinyeat-word-move-point")
         (charf       (if back
			  'skip-chars-backward
			'skip-chars-forward))
         (non-word    tinyeat--non-word-chars)
         (nonw-re     (concat "[" non-word "]+"))
         (ch          (ti::buffer-read-char back))
         p
         str
         mb
         me                             ;match beg end
         mixed)
    (unless fid ;; Quiet XEmacs byte compiler
      (setq fid nil))
    (tinyeat-debug fid "ENTRY" 'back back
                   'char ch
                   (if ch
                       (char-to-string ch)
                     "no CHARACTER??"))
    ;;    Check if this is special mixedCase before vaporizing word...
    (save-excursion
      (setq p (point))
      (if back
          (backward-word 1)
        (forward-word 1))
      (setq str (buffer-substring p (point)))
      (setq mixed (ti::string-match-case "[A-Z][a-z]" str)))
    (cond
     (mixed
      (tinyeat-debug fid "CASE MIXED" 'point (point))
      (if (eq ch (downcase ch))
          (funcall charf "a-z")
        (setq p (point))
        ;;  Skip all big letters
        (funcall charf "A-Z")
        ;;  If this was only one letter, continue deleting. Otw stay put.
        (if (eq 1 (abs (- p (point))))
            (funcall charf "a-z")))
      ;;  The previous statements only moved 2 first statements
      ;;          ThisIsWord      start,
      ;;                   *
      ;;          ThisIsWord      after,
      ;;                 *
      ;;          ThisIsWord      correction. This is needed
      ;;                *
      (if (and back
               (not (bobp)))
          (backward-char 1)))
     (t
      ;; if there is non-word we must remove it.
      ;; - There is some problems in backward deltion, eg deleting "...."
      ;;   backward in text-mode does not delete all dots. Don't
      ;;   know why not.
      (cond
       ((if back                        ;select FWD of BCK looking
            (cond
             ((string-match nonw-re (char-to-string ch))
              (re-search-backward nonw-re nil t)))
          (looking-at nonw-re))
        (setq mb (match-beginning 0)
              me (match-end 0))
        (tinyeat-debug
         fid "CASE 1" ch 'point (point)
         'match-begin mb
         'match-end   me)
        ;;  1. if there is multiple items like "....", delete only
        ;;     those
        ;;  2. if there is only one member like ".member", delete
        ;;     dot and the word that follows it.
        ;;
        (if back (setq p mb)
          ;; selet direction
          (setq p me))
        (if (not (eq 1 (abs (- me mb))))
            (goto-char p)
          (goto-char p)
          (funcall charf (concat "^" non-word))))
       (t
        (tinyeat-debug "CASE default ")
        ;;  The skip-chars-forward _requires_ that the "-"
        ;;  character is the first item. That's why we have
        ;;  to add extra "-" to the front of string if user
        ;;  has defined "-" to be word stopper.
        (if (ti::string-match-case "-" non-word)
            (setq non-word (concat  "^-" non-word))
          (setq non-word (concat "^" non-word)))
        (tinyeat-debug "CASE default " charf non-word)
        (funcall charf non-word)))))))

;;}}}
;;{{{ Yanking

;;; ----------------------------------------------------------------------
;;; Having overwrite-mode on, does not support this kind of behavior?
;;;
(defun tinyeat-yank-overwrite ()
  "Yank text by overwriting previous content."
  (interactive)
  (let ((p (point))			;insertion point
	len
	end)
    (with-temp-buffer
      (yank)
      (setq len (1- (point-max))))      ;how many chars in there ?
    (cond
     ((= len 0)
      (unless (ti::buffer-minibuffer-p)
        (message "TinyEat: Nothing to yank")))
     (t
      ;;   we must untabify  the line, otw we get unpleasant results
      (untabify p (line-end-position))
      (setq end (+ p len))
      (if (> end (point-max))
          (setq end (point-max)))
      (delete-region p end)
      (yank)))))

;;}}}
;;{{{ engine

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyeat-eat (&optional back ti::space-preserve)
  "Eat *appropriate* text forward, if BACK then backward.

The optional SPACE-PRESERVE changes the space eating.

A.  when it is NIL and BACK is anything.   * marks the cursor.
         text1 text1        *     text2  text2
    -->  text1 text1 text2  text2                   ;one space left

B.  when it is NON-NIL and BACK nil
         text1 text1        *     text2  text2
    -->  text1 text1        *text2  text2            ;delete right spaces

C.  when it is NON-NIL and BACK t
         text1 text1        *     text2  text2
         text1 text1*     text2  text2               ;delete left spaces

References:

  `tinyeat--non-word-chars'"
  (let ((fid        "tinyeat-eat ")
        (p          (point))
        (syntaxf    (if back 'skip-syntax-backward 'skip-syntax-forward))
        (charf      (if back 'skip-chars-backward  'skip-chars-forward))
        ch
        ch-n)
    ;;  XEmacs byte compiler thinks 'fid' is unused? Well, on the contrary.
    ;;  Quiet it. This is no-op.
    (unless fid
      (setq fid nil))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
    (setq ch (ti::buffer-read-char back 0)) ;; sitting on it if looking fwd
    (setq ch-n (ti::buffer-read-char back 1)) ;; next
    (tinyeat-debug
     fid
     "CHARACTER " ch  (char-to-string ch)
     "NEXT CHARACTER" ch-n (char-to-string ch-n))
    (cond
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; BEG of buffer or END of buffer
     ((eq nil ch)
      (tinyeat-debug fid "CHARCTER is nil, maybe bop or eob")
      (tinyeat-verbose-macro
       (message
        "TinyEat: %s"
        (concat
         (if (bobp)
             "Beginning"
           "End")
         " of buffer"))))
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ((ti::space-p ch)                  ;one whitespace
      (tinyeat-debug fid
                     "SPACE-P choice" 'back back 'preserve ti::space-preserve)
      (tinyeat-space-delete-at-point back ti::space-preserve)
      (if (and (null back)
               (looking-at "$"))        ;it handled this
          (tinyeat-verbose-macro
           (message "TinyEat: line cleared."))))
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - Multiple  newlines, squeeze to one only
     ((and (char-equal ch ?\n)
           ch-n
           (char-equal ch-n ?\n))
      (funcall charf "\n")
      (if (null back)
          (backward-char 1)        ;do not join, leave 1 EMPTY newline
        (forward-char 1))
      (tinyeat-debug fid "MULTIPLE newlines" 'was-point p 'now-point (point))
      (delete-region p (point)))
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; - at the end of line I suppose add previous line to it.
     ((char-equal ch ?\n)
      (tinyeat-debug
       fid "NEWLINE" 'back back 'ti::space-preserve ti::space-preserve)
      (unless (tinyeat-space-delete-at-point back ti::space-preserve)
        (if (null back)                 ;which direction
            (delete-char 1)
          (if (not (eq 0 (funcall syntaxf  "_"))) ;try to move
              (delete-region p (point))           ;moveti::d!
            (backward-char 1)
            (delete-region p (point))))))
     ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
     ;; WORD handling (blocks)
     (t                                 ;eat next word
      (funcall syntaxf " ")             ;ignore spaces
      (tinyeat-debug fid "default - WORD CASE\n"
                     "CHARACTER " (char-to-string ch)
                     "CHARACTER SYNTAX " (char-to-string (char-syntax ch)))
      ;;   - What is next char after whitespace ??
      ;;   - With these following conditionals we set the point
      ;;     to appropriate position and after COND we run the kill command
      (cond
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       ((and (not  (ti::char-in-list-case ch  '(?- ?_ ?:)))
             (equal ?w (char-syntax ch)))
        (tinyeat-debug fid "-- CASE 1 syntaxes [-_:]")
        (tinyeat-word-move-point back))
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       ((and (ti::char-in-list-case ch   '(?- ?_ ?:))
             ch-n
             (memq (char-syntax ch-n)  '(?w ?\ )))
        (tinyeat-debug fid "-- CASE 2")
        ;;  This is really hard to understand... execpt for the author
        ;;  1) Is CH variable start, a delimiter ?
        ;;  2) AND is the NEXT-CH word or whitespace
        ;; (funcall syntaxf  "_w")
        ;; (funcall syntaxf  " w")
        (funcall charf "-_:"))
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
       (t
        ;; punctuation, comment, the rest ... skip non important stuff
        (tinyeat-debug fid "-- CASE other")
        (funcall charf "^ \t\na-zA-Z0-9")))
      (delete-region p (point))))))

;;}}}

(provide   'tinyeat)
(run-hooks 'tinyeat--load-hook)

;;; tinyeat.el ends here
