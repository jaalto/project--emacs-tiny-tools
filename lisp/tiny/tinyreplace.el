;;; tinyreplace.el --- Handy query-replace, area, case preserve, words

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
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
;; Put this file on your Emacs-Lisp `load-path' and add following into your
;; ~/.emacs startup file
;;
;;      (global-set-key "\M-&" 'tinyreplace-menu)
;;      (autoload 'tinyreplace-menu			"tinyreplace" "" t)
;;      (autoload 'tinyreplace-replace-forward          "tinyreplace" "" t)
;;      (autoload 'tinyreplace-replace-region           "tinyreplace" "" t)
;;      (autoload 'tinyreplace-replace-over-files       "tinyreplace" "" t)
;;      (autoload 'tinyreplace-define-keys-compile-map  "tinyreplace" "" t)
;;      (autoload 'tinyreplace-replace-over-files-compile-buffer "tinyreplace" "" t)
;;
;; Or you can load this file directly:
;;
;;      (add-hook 'tinyreplace-load-hook 'tinyreplace-install)       ;; M-& key
;;      (require 'tinyreplace)
;;
;; Check that you have colors on, otherwise the replaced region may
;; not be visible.
;;
;;      (set-face-background 'highlight "blue")

;;}}}
;;{{{ Documentation

;; .................................................... &t-commentary ...
;;; Commentary:

;;  Preface 1995
;;
;;      There was post in gnu.emacs.help where Brian Paul asked for
;;      help to replace his C variables: "Suppose I want to replace
;;      all occurances of the variable i in my C program with j." The
;;      normal emacs function query-replace wasn't suitable for this
;;      task because it offered too many false hits. Guess how many i
;;      characters are used in non-variable context.
;;
;;      Well later I rembered that one could have used \bi\b to search
;;      words. But the nature of "word" is very different here as it
;;      would have been with \b, which relies on syntax table which you
;;      seldom want to change, whilst the "word definition " here can be
;;      changed on the fly. (Remember that \bi\b still matches entries
;;      like "i.here" where you would want to match only plain "i")
;;
;;      Things are not that simple always, in fact, the first
;;      implementation of this package had to do with the latex math
;;      equation replace, so that program would automatically skip over
;;      normal text and perform replace within the blocks only.
;;      I decided to pull out the v1.0 and make it a complete package,
;;      so here it is folks.
;;
;;  Overview of features
;;
;;      o   Companion to emacs's query-replace. Simple interface.
;;      o   Text beeing replaced is highlighted AND terminals that cannot see
;;          the highlight will see "=>" string marking the line beeing
;;          processed
;;      o   Preserve case while replacing "FoO" --> "BaR" (symmetry)
;;      o   Toggle case sensitivity and symmetry during the replace.
;;      o   Word match mode on/off during replace: 'matchTHIS or THIS '
;;      o   "Narrow to function", go to "beginning of file" when you start
;;          replacing. You're put back to position where you were when
;;          you quit.
;;      o   Can replace over many files. (Reads compile buffer output)
;;          Checks Out files from RCS when needed (if they are not locked)
;;      o   Variable `tinyreplace-exlude-line-regexp' can be use
;;          to ignore lines
;;
;;  How to use
;;
;;      If you know lisp, you can go and take straight advantage of the
;;      engine function:
;;
;;          tinyreplace-replace-region-1
;;
;;      Normally functions work within area defined by you, but
;;      there is 'applications' section which offers several ready to run
;;      functions for various needs:
;;
;;      o   `tinyreplace-replace-region', like query replace but in selected area
;;      o   `tinyreplace-replace-forward', start from current point.
;;      o   `tinyreplace-latex-blk-replace', replace text surrounded
;;          by latex BLOCKS
;;      o   `tinyreplace-latex-math-replace', replace text within latex
;;          math equations only.
;;
;;  What commands do I have while replacing interactively?
;;
;;      There are some handy commands that normal emacs replace lacks:
;;
;;      o   toggle case sensitivity during replace
;;      o   toggle symmetry during replace (character by character conversion)
;;      o   Go to start of buffer _now_ (return back when you exit replace)
;;      o   search backward
;;      o   Narrow to current function, so that you can replace local variables
;;      o   Flash function name where you're (only for some programming
;;          languages.)
;;
;;      See function `tinyreplace-replace-region-1' for command
;;      explanation. To abort the search, you just press Ctrl-g or 'Q'
;;      and you'll be returned to the starting point of search.
;;
;;  Command line prompt explanation
;;
;;      The command line prompt will look like this
;;
;;          Replace 'xx' with 'yy' (a,bvuBFNU? [+CSX] !ynqQ)
;;
;;      Where the flag settings active are displayed between brackets.
;;      The '+' means that you have used (N)arrow command, C indicates
;;      case sensitivity, S tells that symmetry is activated and X
;;      means that line exclude is in effect. For full explanation of
;;      the commands, please press help key (?) Which will print the
;;      command summary.
;;
;;  Special commands in command line
;;
;;      When you edit the seach string or destination string, there are
;;      some keys that you can use:
;;
;;          C-l     Yank the text under point to current prompt
;;          C-o     Yank previous SRING1
;;
;;      The Yank command is `C-l' not `C-y', because if you edit and
;;      kill inside the prompt line, you can use regular `C-y' to yank
;;      text back. The `C-l' command reads a space separated text from
;;      the buffer and pastes it into the prompt for editing.
;;
;;      The `C-o' command yanks the SEARCH string to the prompt. It
;;      comes handy if you used `C-l' to yank the initial search
;;      string, edited yanked text and wanted to share it in the next
;;      prompt. This way you don't have to do the editing again, but
;;      only modify the previous string. To pick right word (Yank
;;      `C-l') from the buffer, when you don't have have mouse, you
;;      can use following keys. Text to the left shows you briefly
;;      where the point currenly is.
;;
;;          <           Moves buffer's point backward
;;          >           Moves buffer's point forward
;;
;;      This feature propably is at its best in a compile buffer where you
;;      have grep results and you draw region around the the files where
;;      you want the replace to happen. Move a little with [<>] and you
;;      will be soon in a line that has the grep word, then yank it to the
;;      replace prompt.
;;
;;  Note about the arrow pointer
;;
;;      Terminals that do not have highlight capability to see which
;;      portion of text will be replaced will appreciate the arrow at
;;      the beginning of line to show where the text is located.
;;
;;      The option "a" that refreshes the arrow marker is *forced* to
;;      ask a minibuffer question in order to change the state of
;;      arrow (hide or show). There was no other way to do this and I
;;      think it's a bug in 19.28 emacs, because the state is not
;;      immediately shown in buffer.
;;
;;  Note about commands
;;
;;      The commands are hard wired in this module and you cannot add
;;      new ones as you can in replace.el which is minor-mode based.
;;      This package is meant to be companion to replace.el, and for
;;      that reason the interface has been designed to be as simple as
;;      possible without any additional modes.
;;
;;      There is no plan to convert this module to minor mode.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-and-compile
  (autoload 'vc-registered "vc"))

(ti::package-defgroup-tiny TinyReplace tinyreplace-- tools
  "Overview of features

        o   Companion to emacs's query-replace. Simple interface.
        o   Text beeing replaced is highlighted AND terminals that cannot see
            the highlight will see '=>' string marking the line beeing
            processed
        o   Preserve case while replacing FoO --> BaR (symmetry)
        o   Toggle case sensitivity and symmetry during the replace.
        o   Word match mode on/off during replace: matchTHIS or THIS
        o   'Narrow to function', go to 'beg of file' when you start
            replacing. You're put back to position where you were when
            you quit.
        o   Can replace over many files. (Reads compile buffer output)
            ChecksOut files from RCS when needed (if they are not locked)
        o   You can define `tinyreplace-exlude-line-regexp' that skips any line
            matching looking-at regexp at the beginning of line.")

;;}}}
;;{{{ setup: hooks

(defcustom tinyreplace-load-hook nil
  "*Hook run when file has been loaded."
  :type 'hook
  :group 'TinyReplace)

(defcustom tinyreplace--args-keymap-hook  nil
  "*Hook which can define additional key bindings to `tinyreplace--args-keymap'."
  :type 'hook
  :group 'TinyReplace)

(defcustom tinyreplace--pre-replace-hook  nil
  "*Hook to run just before replacing start in a buffer."
  :type 'hook
  :group 'TinyReplace)

;;}}}
;;{{{ setup: public, User configurable

(defcustom tinyreplace--goto-region-beginning t
  "If non-nil go to beginning of region before replace starts."
  :type 'boolean
  :group 'TinyReplace)

(defcustom tinyreplace--exclude-line  nil
  "*When search stops to found position, this variable is consulted.

It can be:

  nil       do nothing special. Proceed replace.
  regexp    line which matches `looking-at' REGEXP at the beginning
            of line is skipped.
  function  if it returns t, then the line is skipped and search continues.
            Function takes no arguments and it can move point, since
            it is run under `save-excursion'. Point is at replace point when
            the function is called.

Example:

  (setq tinyreplace--exclude-line 'my-tinyreplace-exclude)
  (defun my-tinyreplace-exclude ()
    ;;  Exclude comment lines
    (cond
     ((eq major-mode 'c++-mode)
      (beginning-of-line)
      (looking-at \"^[ \t]*//\"))))"
  :type  '(string :tag "Regexp")
  :group 'TinyReplace)

(defcustom tinyreplace--arrow "=>"
  "*Line marker where the replace takes effect.
Especially useful, when term cannot display colours to show the
replacement place."
  :type  '(string :tag "Arrow string")
  :group 'TinyReplace)

(defcustom tinyreplace--arrow-initial-state 'show
  "*When replacing interactively, this is the default arrow state.
If your terminal supports highlighting you may want to set this to 'hide.

The only valid values are 'show and 'hide."
  :type '(choice
          (const show)
          (const hide))
  :group 'TinyReplace)

(defcustom tinyreplace--face 'highlight
  "*The match area overlay face."
  :type 'face
  :group 'TinyReplace)

(defcustom tinyreplace--word-boundary "[^a-zA-Z0-9_]"
  "*This is complement set of characters forming a word.
For example if you want to replace 'i' with 'j' , you don't want to match

     \"Ignore ThIs match, but replace i with j\"
                                     ^^^
The complement makes sure the word is full word."
  :type  '(string :tag "Word complement charset")
  :group 'TinyReplace)

(defcustom tinyreplace--symmetry nil
  "*Non-nil perform replacement using same symmetry.
When replacing text, it may be desirable to have the same symmetry,
the case of the characters, to be preserved while replace takes effect.
Suppose you have text

        FOO Foo fooUx foo

And you want to preserve the symmetry when doing \"foo\" --> \"bar\".
This is what you get:

        BAR Bar barUx bar

If the symmetry is nil, then the normal replace would have given:

        bar bar barUx bar"
  :type  'boolean
  :group 'TinyReplace)

(defcustom tinyreplace--symmetry-rest nil
  "*If non-nil then rest of the characters follow previous symmetry."
  :type  'boolean
  :group 'TinyReplace)

;; Not in defcustom; advanced feature and expert knows what to to with this.
;;
(defvar tinyreplace--read-args-function  'tinyreplace-read-args
  "*Function to ask two arguments ARG1 and ARG2 for replace.
Input:

  String

Output:

  '(\"arg1\" \"arg2\")

Function must terminate with error if it cannot return list of
two strings.")

(defcustom tinyreplace--user-function 'ti::buffer-outline-widen
  "*User function fun from command prompt key 'U'."
  :type  'function
  :group 'TinyReplace)

;;}}}
;;{{{ setup: private

(defvar tinyreplace--replace-region-overlay nil
  "Overlay used to show the replaced region.")

(defvar tinyreplace--transient-mark-mode nil
  "Transient mark mode state (Emacs).")

(defvar tinyreplace--arrow-state nil
  "Arrow display state.")

(defvar tinyreplace--narrow-state nil
  "Narrowed to function state.")

(defvar tinyreplace--args-history  nil
  "History of replace strings.")

(defvar tinyreplace--args-keymap nil
  "Keymap for reading arguments.")

(defvar tinyreplace--replace-buffer  nil
  "Buffer where to replace.")

(defvar tinyreplace--tmp-buffer  "*tinyreplace-temp*"
  "Temp buffer.")

(defvar tinyreplace--err-buffer  "*tinyreplace-error*"
  "Error message buffer.")

(defvar tinyreplace--read-point  nil
  "This variable is used in interactive word reading.
It tells where the current point is.")

(defvar tinyreplace--o-exclude  nil
  "Private. Temporary variable to keep state when calling another function.")

(defvar tinyreplace--string1  nil
  "Private. The asked string1. Set in `tinyreplace-read-args'.")

(defvar tinyreplace--word-match-mode nil
  "Not a user variable. Hold value t if user switch to exact word matching.
Property 're will have the original regexp.")

;;; ............................................................ &menu ...

;;  You propably want to copy this to your ~/.emacs and define your
;;  own key combinations. See tinylibmenu.el how to use the menu variable

(defvar tinyreplace--menu
  '("\
replace: (f)wd (r)eg (w)ord (c)ompile buffer (f)iles (lL)atex (?)help "
    (
     ;;  If `tinyreplace-menu' is bound to M-%, then the "5" key makes
     ;;  sense, because "%" is shift-5.
     (?f  . ( (call-interactively 'tinyreplace-replace-forward)))
     (?w  . ( (call-interactively 'tinyreplace-word-replace)))
     (?r  . ( (call-interactively 'tinyreplace-replace-region)))
     (?c  . ( (call-interactively
               'tinyreplace-replace-over-files-compile-buffer)))
     (?F  . ( (call-interactively 'tinyreplace-replace-over-files)))
     (?l  . ( (call-interactively 'tinyreplace-latex-blk-replace)))
     (?L  . ( (call-interactively 'tinyreplace-latex-math-replace)))))
  "Help menu for the commands. Press 'q' to return to menu.

Standard replace commands:

    f  calls function `tinyreplace-replace-forward'
    w  calls function `tinyreplace-word-replace'
    r  calls function `tinyreplace-replace-region'

    NOTE: If you select `f', You can select modes that affect how the
    replacing will be started. E.g. pressing `B' will put point to the
    beginning of buffer from which the *forward* action is carried out.

    Another useful mode to select before strting to replace is `w'
    for word mode toggle.

The following keys can be used in compile-like buffers, where each line
contains standard grep-like output. If you mark a region, the selected
files are searched for replacements.

    FILE:LINE-NUMBER:output
    FILE:LINE-NUMBER:output
    FILE:LINE-NUMBER:output

    c  calls function `tinyreplace-replace-over-files-compile-buffer'
    F  calls function `tinyreplace-replace-over-files'

Special commands:

    l  calls function `tinyreplace-latex-blk-replace'
    L  calls function `tinyreplace-latex-math-replace'")

;;}}}
;;{{{ install

;;; ----------------------------------------------------------------------
;;;###autoload
(defun tinyreplace-install-default-keybings ()
  "Install M-& default keybing."
  (interactive)
  (let* ((key "\M-&")
	 (def (lookup-key global-map key)))
    (when (featurep 'compile)
      (let (buffer (get-buffer "*compilation*"))
	(when buffer
	  (with-current-buffer buffer
	    (tinyreplace-define-keys-compile-map)))))
    (when (featurep 'grep)
      (let (buffer (get-buffer "*grep*"))
	(when buffer
	  (with-current-buffer buffer
	    (tinyreplace-define-keys-compile-map)))))
    (global-set-key "\M-&" 'tinyreplace-menu)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-define-keys-compile-map ()
  "Define key binding M-& in local map. For compilation like modes."
  (interactive)
  (local-set-key "\M-&" 'tinyreplace-replace-over-files-compile-buffer))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-install-hooks (&optional uninstall)
  "Install or UNINSTALL `tinyreplace-define-keys-compile-map' into hooks.
See:
  compilation-mode-hook
  compilation-minor-mode-hook
  grep-mode-hook"
  (interactive "P")
  (cond
   (uninstall
    (add-hook 'compilation-mode-hook 'tinyreplace-define-keys-compile-map)
    (add-hook 'compilation-minor-mode-hook 'tinyreplace-define-keys-compile-map)
    (add-hook 'grep-mode-hook 'tinyreplace-define-keys-compile-map))
   (t
    (remove-hook 'compilation-mode-hook 'tinyreplace-define-keys-compile-map)
    (remove-hook 'compilation-minor-mode-hook 'tinyreplace-define-keys-compile-map)
    (remove-hook 'grep-mode-hook 'tinyreplace-define-keys-compile-map))))

;;; ----------------------------------------------------------------------
;;;###autoload
(defun tinyreplace-install ()
  "Call `tinyreplace-install-default-keybings' and `tinyreplace-install-hooks'."
  (interactive)
  (tinyreplace-install-default-keybings)
  (tinyreplace-install-hooks))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;###autoload
(defun tinyreplace-menu ()
  "Run `tinyreplace--menu'."
  (interactive)
  (if buffer-read-only
      (message "My: Cannot start replace, buffer is read-only.")
    (ti::menu-menu 'tinyreplace--menu)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyreplace-interactive-region-args  (string)
  "Construct interactive tag for functions that need region.
STRING is argument to `tinyreplace--read-args-function'.

Return:
 '(BEG END ARG1-STRING ARG2-STRING)"
  `(if (region-active-p)
       (ti::list-merge-elements
	(region-beginning)
	(region-end)
	(funcall tinyreplace--read-args-function ,string))
     (error "TinyReplace: Region is not active. Please select one.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-make-word-regexp  (string)
  "See `tinyreplace--word-boundary'. Make regexp from STRING."
  (concat tinyreplace--word-boundary
          "\\(" (regexp-quote string) "\\)"
          tinyreplace--word-boundary))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-read-args (&optional prompt)
  "Read two arguments with PROMPT. Return '(ARG1 ARG2)."
  (let ((opoint (point))
	arg1
	arg2)
    ;; Disable electric file minor mode, which defines specilal
    ;; characters.
    (setq tinyreplace--replace-buffer   (current-buffer)
          tinyreplace--read-point       (point)
          tinyreplace--string1          nil)
    (tinyreplace-args-keymap-create)
    (setq arg1
          (ti::remove-properties
           (read-from-minibuffer
            (concat (or prompt "") " Search: ") nil
            tinyreplace--args-keymap nil tinyreplace--args-history)))
    (setq tinyreplace--string1 arg1)    ;Now available
    (setq arg2
          (ti::remove-properties
           (read-from-minibuffer
            "Replace with: " nil
            tinyreplace--args-keymap nil tinyreplace--args-history)))
    (goto-char opoint)                  ;restore
    (list arg1 arg2)))

;;; ----------------------------------------------------------------------
;;; - This is for user friendliness
;;;
;;;###autoload
(defun tinyreplace-symmetry-toggle (&optional arg verb)
  "Toggle variable` tinyreplace--symmetry' with ARG. VERB."
  (interactive "P")
  (ti::verb)
  (ti::bool-toggle tinyreplace--symmetry arg)
  (put 'tinyreplace-replace-1 'tinyreplace--symmetry tinyreplace--symmetry)
  (if verb
      (message "TinyReplace: Symmetry is now %s"
               (if tinyreplace--symmetry
                   "on"
                 "off"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-transient-mark-mode  (mode)
  "Record function  `transient-mark-mode' status.
This is done only if function exists. MODE can be 'write or 'read."
  (when (and (ti::emacs-p)                  ;#todo: zmacs-region-stays
             (boundp 'transient-mark-mode)) ;XEmacs doesn't have this
    (cond
     ((eq mode 'write)
      (setq tinyreplace--transient-mark-mode
            (let ((var 'transient-mark-mode)) ;XEmacs 19.14 byteComp silencer
              (symbol-value var))))
     ((eq mode 'read)
      tinyreplace--transient-mark-mode))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-arrow-control (buffer mode &optional str)
  "Handles showing the arrow.

Input:

  BUFFER        buffer pointer
  MODE          symbol 'show, 'hide, 'toggle, 'maybe or 'move.
  STR           Used for restoring the original contents when mode is 'hide

Sets global:

  `tinyreplace--arrow-state'

Returns:

  mode     current state"

  (cond
   ((eq mode 'toggle)
    (if (or (null overlay-arrow-position) ;doesn't exist
            (not (equal (marker-buffer overlay-arrow-position)
                        (current-buffer))))
        (setq mode 'show)
      (setq mode 'hide)))

   ((eq mode 'maybe)
    (cond
     ((null tinyreplace--arrow-state)
      (setq mode 'show))
     (t
      ;;  follow the mode which is active
      (setq mode tinyreplace--arrow-state)))))
  (cond
   ((or (eq mode 'show)
        (eq mode 'move))
    (ti::buffer-arrow-control buffer mode tinyreplace--arrow  (point))
    (setq tinyreplace--arrow-state 'show))
   ((eq mode 'hide)
    (ti::buffer-arrow-control buffer 'hide str)
    (setq tinyreplace--arrow-state 'hide)))
  mode)

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-replace-ask (buffer from-str to-str )
  "Perform asking while in interactive replace mode.

Input:

 BUFFER FROM-STR TO-STR

Note:

  `tinyreplace--o-exclude'    must be set in the calling function"
  (let ((o-exclude  tinyreplace--o-exclude)
	(loop       t)
	msg
	ans)
    (while loop
      (setq from-str (ti::string-format-percent from-str)
            to-str   (ti::string-format-percent to-str))
      (setq
       msg
       (format "Replace '%s' with '%s' (a,bvuBFNU [%s%s%s%s%s] ?!ynqQ) "
               ;; Make prompt fit nicely
               (if (> (length from-str) 18)
                   (concat (ti::string-left from-str 16) "..")
                 from-str)
               (if (> (length from-str) 18)
                   (concat (ti::string-left to-str 16) "..")
                 to-str)
               (if tinyreplace--narrow-state "N " "")
               (if tinyreplace--symmetry  "S" "")
               (if case-fold-search "" "C")
               (if tinyreplace--word-match-mode "W" "")
               (if tinyreplace--exclude-line "X" "")))
      (setq ans (ti::read-char-safe-until msg))
      ;;  There is purposively a dummy COND case.
      (cond
       ((char-equal ?a ans)
        (tinyreplace-arrow-control buffer 'toggle)
        (read-from-minibuffer
         "Arrow refreshed. Press RET to update view."))
       ((char-equal ?s ans)
        (tinyreplace-symmetry-toggle))
       ((char-equal ?w ans)
        (ti::bool-toggle tinyreplace--word-match-mode)
        (put 'tinyreplace-replace-1
             'tinyreplace--word-match-mode
             tinyreplace--word-match-mode))
       ((char-equal ?c ans)
        (ti::bool-toggle case-fold-search)
        (put 'tinyreplace-replace-1 'case-fold-search case-fold-search))
       ((char-equal ?\  ans)
        (setq ans ?y))
       ((char-equal ?x ans)                  ;exclude toggle
        (if tinyreplace--exclude-line
            (setq tinyreplace--exclude-line nil)
          ;; Dynamically bound in call func
          (setq tinyreplace--exclude-line o-exclude)))
       ((char-equal ?F ans)
        (tinyreplace-show-function-name (point)))
       ((char-equal ?N ans)
        (setq  tinyreplace--narrow-state t)
        (setq ans ?N))
       ((char-equal ?U ans)
        (funcall tinyreplace--user-function)
        (setq ans ?U))
       ((ti::char-in-list-case ans '(?\177 ?\b ?n))
        (setq ans ?n)))
      (if (ti::char-in-list-case ans '(?! ?? ?y ?n ?q ?Q ?b ?v ?u  ?B  ?N))
          (setq loop nil))) ;; while loop
    ans))

;;; ----------------------------------------------------------------------
;;; Press Ctrl-g to abort replace.
;;;
(defun tinyreplace-show-function-name (point)
  "Flashes function name briefly from POINT."
  (let* ((name (ti::buffer-defun-function-name point))
         (txt  (if name
                   name
                 "<not found>")))
    (message txt)
    (sit-for 1)))

;;; ----------------------------------------------------------------------
;;; Press Ctrl-g to abort replace.
;;;
(defun tinyreplace-move-overlay (beg end)
  "Move overlay to BEG END."
  (ti::compat-overlay-move 'tinyreplace--replace-region-overlay beg end  nil)
  (ti::compat-overlay-put
   'tinyreplace--replace-region-overlay
   'face tinyreplace--face))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-replace-1 (beg end str)
  "Replace region BEG END with STR, point with after replace."
  (when (and (integerp beg) (integerp end))
    (delete-region beg end) (goto-char beg)
    (insert str)))

;;}}}
;;{{{ key

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-key-clear-input  ()
  "Clear the line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-key-forward-word (&optional count)
  "Forward word. COUNT is argument to `forward-word', and defaults to 1.

This function is meant to position you to to right word which you can
then insert into the replace prompt with \\[tinyreplace-key-yank-word]. When"
  (interactive)
  (let* ((obuffer (current-buffer)))
    (select-window (get-buffer-window tinyreplace--replace-buffer))
    (forward-word (or count 1))
    (setq tinyreplace--read-point (point))
    (message "TinyReplace: reading point %d:%s..."
             (point)
             (ti::string-left
              (or (buffer-substring-no-properties (point) (line-end-position))
                  "")
              40))
    (select-window (get-buffer-window  obuffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-key-backward-word ()
  "See `tinyreplace-key-forward-word'."
  (interactive)
  (tinyreplace-key-forward-word -1))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-key-yank-string1  ()
  "Yank previous string (search string)."
  (interactive)
  (if tinyreplace--string1
      (insert tinyreplace--string1)
    (message "TinyReplace: Sorry, there is no STRING1 to yank yet ")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-key-yank-word ()
  "Yank word from buffer. `tinyreplace--replace-buffer' must be set."
  (interactive)
  (let (word)
    (with-current-buffer tinyreplace--replace-buffer
      (goto-char  tinyreplace--read-point)
      ;;  This is just because of the space-word-command
      ;;
      ;;     Word-here
      ;;     *             << advance this cursor one char
      (if (ti::char-in-list-case (preceding-char) '(?\ ?\t ?\n ))
          (forward-char 1))
      (setq word (ti::remove-properties (ti::buffer-read-space-word))))
    (when word
      (setq word (ti::remove-properties word))
      (insert word))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-args-keymap-create  ()
  "Create keymap."
  (setq tinyreplace--args-keymap (copy-keymap minibuffer-local-map))
  (define-key tinyreplace--args-keymap "\C-l"  'tinyreplace-key-yank-word)
  (define-key tinyreplace--args-keymap "\C-o"  'tinyreplace-key-yank-string1)
  (define-key tinyreplace--args-keymap "\C-p"  'tinyreplace-key-clear-input)
  (define-key tinyreplace--args-keymap "\C-b"  'tinyreplace-key-backward-word)
  (define-key tinyreplace--args-keymap "\C-f"  'tinyreplace-key-forward-word)
  (run-hooks 'tinyreplace--args-keymap-hook))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;; - The "v" has been chosen because it's close to "b". I first
;;;   used "B" for backward REPLACEMENT, but it was too much
;;;   trouble to reach extra shift key.
;;;
;;;   Eg. If I want to replace backward (to undo some changes),
;;;   you just press "v" and "u". Much more awkward would have
;;;   been "B" and "u". The shift-modifier is not good in this case.
;;;
;;; - The "undo" feature here is hand coded, because I couldn't find
;;;   any emacs command that would undo last change...one at a time.
;;;
(defun tinyreplace-replace-region-1 (beg end re str &optional level ask func)
  "Perform replace.

Input:

  BEG END   region
  RE        regexp to search
  STR       replace string
  LEVEL     subexpression level to match in RE. Default is 0, whole match.
  ASK       interactive
  FUNC      Call function

Commands while ASK is non-nil:     (simple undo backward is = 'v u' )

 y or SPACE             replace
 n or BACKSPACE         skip
 !                      replace rest
 q                      quit
 Q                      quit at current point.

Search modes

 s                      Mode: toggle symmetry.
                        When mode is on, the written case is preserved.
 c                      Mode: toggle case sensitivity in search.
 w                      Mode: toggle word only search.
 a                      Mode: toggle arrow display.
 x                      Mode: toggle `tinyreplace--exclude-line' variable on/off.

Search control forward/backward

 b                      search backward for MATCH
 v                      search backward for REPLACEMENT
 u                      undo -- This is very limited undo and works
                        only when replacing ordinary strings, not
                        regexps. The symmetry must not be used.

Moving point of search

 B                      go to beginning of search.
                        Beginning or end point depending of search direction

 F                      flash function name

 N                      narrow to function.
                        You can't cancel this if you use it. The
                        'N' is indicated in the command line,

 U                      Run user function `tinyreplace--user-function'

References:

  `tinyreplace--exclude-line'

Return:

  number  last replace area position. This is not same as the END
          parameter, because replacing text modifies the buffer's points."
  (let* (case-fold-search
         (arrow-orig    overlay-arrow-string)
         (arrow-init    tinyreplace--arrow-initial-state)
         (symm-rest     tinyreplace--symmetry-rest)
         (tinyreplace--exclude-line tinyreplace--exclude-line) ;Make local copy
         (o-exclude     tinyreplace--exclude-line) ;original value
         (level         (or level 0))              ;default value
         (buffer        (current-buffer))
         (func          (or func 're-search-forward))
         (MARK          (point-marker)) ;record user position
         PREV-ME
         MARK-MAXP                ;marker for the last point of search
         minp maxp                      ;logical point min, point-max
         c-exclude                      ;Current exclude value
         str-to
         do-ask
         quit-point
         read-string
;;;      undo-string
         fmin fmax
         pos
         mb me                          ;match area, beg end
         replace
         bypass)               ;This is flag to skip searching in loop
    ;; Se same defaults as previous time
    (when ask
      (setq tinyreplace--word-match-mode
            (get 'tinyreplace-replace-1 'tinyreplace--word-match-mode))
      (setq tinyreplace--symmetry
            (get 'tinyreplace-replace-1 'tinyreplace--symmetry))
      (setq case-fold-search
            (get 'tinyreplace-replace-1 'case-fold-search)))
    (put 'tinyreplace--word-match-mode 're re)
    (put 'tinyreplace--word-match-mode 'word (tinyreplace-make-word-regexp re))
    (setq tinyreplace--o-exclude o-exclude)
    (tinyreplace-transient-mark-mode 'write)
    (transient-mark-mode 0)             ;turn this off for now..
    (setq tinyreplace--arrow-state arrow-init) ;<< set global, see 'maybe
    (tinyreplace-arrow-control buffer 'maybe)
    (setq tinyreplace--narrow-state nil) ;reset
    (cond
     ((eq func 're-search-forward)
      (setq minp (min beg end)          ;logical min and max
            maxp (max beg end)))
     (t
      (setq minp (max beg end)
            maxp (min beg end))))
    (save-excursion
      (goto-char maxp)
      (setq MARK-MAXP (point-marker)))
    (goto-char minp)
    (save-excursion
      (unwind-protect
          (catch 'cancel
            (run-hooks 'tinyreplace--pre-replace-hook)
            ;;  Peek a little before we start
            ;;
            (cond
             ((and (not (eq (point)
                            (point-min))) ;Already ti::pmin ?
                   (save-excursion
                     (if (null (funcall func re nil t))
                         (y-or-n-p "\
There is no matches forward. Go to beginning of buffer? "))))
              (ti::pmin))
             ((and (region-active-p)
                   (or tinyreplace--goto-region-beginning
                       (y-or-n-p "\
Region is active. Go to beginning of region? "))
                   (if (eq func 're-search-forward)
                       (goto-char (region-beginning))
                     (goto-char (region-end))))))
            ;;   SEARCH
            (while (or bypass
                       (and (funcall func re nil t)
                            (<= (point)
                                (marker-position MARK-MAXP))))
              (or bypass ;; Not moved, do not calculate positions
                  (setq mb (match-beginning level)
                        me (match-end level)))
              (setq bypass nil)
              ;; .. .. .. .. .. .. .. .. .. .. .. .. .. exclude line ..
              (setq c-exclude tinyreplace--exclude-line)
              (save-excursion
                (cond
                 ((null c-exclude)   ;; No exclude patterns
                  t)                 ;; flag "ok"
                 ((and (symbolp c-exclude)
                       (fboundp c-exclude))
                  (if (funcall c-exclude)
                      (setq mb nil)))
                 ((stringp c-exclude)
                  (beginning-of-line)
                  (if (looking-at c-exclude)
                      ;; Force forgetting this point
                      (setq mb nil)))))
              ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ HANDLE it ^^^
              (if (null mb)
                  nil                   ;submatch error
                (tinyreplace-move-overlay mb me)
                ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ask user  ^^^
                (setq read-string
                      (buffer-substring-no-properties mb me))
                (setq str-to str)
                (if (null ask)
                    (setq replace t)    ;automatic
                  (tinyreplace-arrow-control buffer 'maybe)
                  (setq do-ask t)
                  (setq read-string
                        (buffer-substring-no-properties mb me))
                  (setq str-to str)
                  (while do-ask
                    (setq replace (tinyreplace-replace-ask
                                   buffer
                                   read-string
                                   str))
                    (setq do-ask nil)
                    (cond
                     (tinyreplace--word-match-mode
                      (setq re (get 'tinyreplace--word-match-mode 'word))
                      (setq level 1))
                     (t
                      (setq re (get 'tinyreplace--word-match-mode 're))
                      (setq level 0)))
                    ;; ... ... ... ... ... ... ... ... ... ... ... ... ...
                    (cond
                     ;; .......................... beginning of buffer ...
                     ((char-equal ?B replace)
                      (if (eq func 're-search-forward)
                          (progn
                            (ti::pmin)
                            (setq minp  (point)))
                        (ti::pmax)
                        (setq maxp (point)))
                      (redraw-display)
                      (setq replace nil))
                     ;; ... ... ... ... ... ... ... ... ... ... . help  ..
                     ((char-equal ?? replace)
                      (ti::menu-help 'tinyreplace-replace-region-1)
                      (setq bypass t)
                      (setq replace nil))
                     ;; ... ... ... ... ... ... ... ... ... ... narrow  ..
                     ((char-equal ?N replace)
                      (cond
                       ((eq func 're-search-forward)
                        ;;  The other call isn't executed if first fails
                        (and (setq fmin (ti::beginning-of-defun-point))
                             (setq fmax (ti::beginning-of-defun-point 'end)))
                        (cond
                         ((and fmin fmax  (not (eq fmin fmax)))
                          (setq minp fmin maxp fmax)
                          (goto-char maxp)
                          (setq MARK-MAXP (point-marker))
                          (goto-char minp))
                         (t
                          (setq tinyreplace--narrow-state nil)
                          (message "TinyReplace: Can't find narrow bounds.")
                          (sit-for 1))))
                       (t
                        (and (setq fmin (ti::beginning-of-defun-point))
                             (setq fmax (ti::beginning-of-defun-point 'end)))
                        (cond
                         ((and fmin fmax  (not (eq fmin fmax)))
                          (setq minp fmax maxp fmin)
                          (goto-char maxp)
                          (setq MARK-MAXP (point-marker))
                          (goto-char minp))
                         (t
                          (setq tinyreplace--narrow-state nil)
                          (message "TinyReplace: Can't find narrow bounds.")
                          (sit-for 1)))))
                      (setq replace nil))
                     ;; ... ... ... ... ... ... ... ... ... ... ... ... ..
                     ((char-equal ?b replace)
                      (save-excursion
                        (goto-char mb)
                        (setq mb nil me nil)
                        (when (re-search-backward re minp t)
                          (setq mb (match-beginning level)
                                me (match-end level))))
                      (if (null mb)
                          (progn
                            (message "TinyReplace: No more hits.")
                            (setq replace nil))
                        (goto-char me)
                        (tinyreplace-move-overlay mb me))
                      (setq replace nil
                            do-ask  t))
                     ;; ... ... ... ... ... ... ... ... ... ... ... ... ..
                     ((char-equal ?v replace)
                      (save-excursion
                        (if PREV-ME
                            (goto-char PREV-ME))
                        (setq mb nil me nil)
                        (when (re-search-backward (regexp-quote str) minp t)
                          (setq mb (match-beginning level)
                                me (match-end level))))
                      (if (null mb)
                          (progn
                            (message "TinyReplace: No previous hit.")
                            (setq replace nil))
                        (goto-char me)
                        (tinyreplace-move-overlay mb me))
                      (setq replace nil  do-ask t))
                     ;; ... ... ... ... ... ... ... ... ... ... ... ... ..
                     ((char-equal ?u replace)
                      (tinyreplace-replace-1
                       mb me
                       (ti::string-case-replace str read-string
                                                tinyreplace--symmetry symm-rest))
                      (tinyreplace-move-overlay mb me)
                      (setq replace nil  do-ask t))
                     ;; ... ... ... ... ... ... ... ... ... ... ... ... ..
                     ((char-equal ?! replace)
                      (setq   ask nil     replace t))
                     ((char-equal ?n replace)
                      (setq replace nil))
                     ((char-equal ?q replace)
                      (throw 'cancel t))
                     ((char-equal ?Q replace)
                      (setq quit-point mb)
                      (throw 'cancel t))
                     (t
                      (setq replace t)))))
                ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ do it ^^^
                (when replace
                  (setq PREV-ME me)

                  (tinyreplace-replace-1
                   mb me
                   (ti::string-case-replace
                    read-string str-to
                    tinyreplace--symmetry symm-rest)))))) ;; cancel - while
        ;; ............................ condition
        ;; - Clean up if Ctrl-g pressed
        ;; - We're done, dehilit and restore possible transient mode
        (if (and tinyreplace--replace-region-overlay
                 (ti::overlay-supported-p))
            (ti::funcall
             'overlay-put tinyreplace--replace-region-overlay 'face nil))
        (setq pos maxp)                 ;update return value
        (tinyreplace-arrow-control buffer 'hide arrow-orig) ;remove it
        (if (tinyreplace-transient-mark-mode 'read) ;if it were on previously
            (transient-mark-mode 1))))
    (if quit-point                      ;only when interactive "Q"
        (goto-char quit-point)          ;handy if you made mistake...
      (goto-char (marker-position MARK)))
    (message "")
    (setq MARK nil  MARK-MAXP nil)      ;kill markers
    pos))

;;}}}
;;{{{ applications

;;; ----------------------------------------------------------------------
;;;
(defun tinyreplace-read-compile-buffer-filename ()
  "Read filename on line."
  (let (;; allow drive letter at front d:/file/
	(re "^\\(\\(.:\\)?[^\n:]+\\):")
        dir
        file)
    (save-excursion
      (beginning-of-line)
      (when (setq file (ti::remove-properties (ti::buffer-match re 1)))
        (save-excursion
          (if (re-search-backward "^cd[ \t]+\\([^\t\n]+\\)" nil t)
              (setq dir (ti::remove-properties (match-string 1)))))
        (when (and dir
		   ;;  Check drive letter
                   (not (string-match "^\\(.:\\)?/" file)))
          (setq file (concat
		      ;;  Make sure there is trailing slash
		      (file-name-as-directory dir)
		      file)))
        (setq file (ti::file-name-for-correct-system file 'emacs))))
    file))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-replace-over-files-compile-buffer
  (beg end str1 str2 &optional func verb)
  "Read all files forward in buffer that is in compile buffer format.
Perform replace over the found files. Checks Out files that are
RCS controlled if necessary.

Line format:

  /DIR/DIR/FILE: matched text

Input:

  See function `tinyreplace-replace-1'
  BEG END STR1 STR2 &OPTIONAL FUNC VERB"

  (interactive
   (ti::list-merge-elements
    (tinyreplace-interactive-region-args "Compile")
    nil
    t))
  (let ((o-frame        (selected-frame))
        (w-frame        (ti::non-dedicated-frame))
        (func           (or func 'tinyreplace-replace-forward))
        (err-buffer     (ti::temp-buffer tinyreplace--err-buffer 'clear))
        (read-only      0)
        no-confirm
        buffer
        cache
        ro-cache                        ;read only file list
        file
        ch)
    (ti::verb)
    (save-excursion
      (ti::keep-lower-order beg end)
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (catch 'exit
        (while (and (not (eobp))
                    (< (point) end))
          (setq file (tinyreplace-read-compile-buffer-filename))
          ;;  See that the file is loaded only once.
          ;;  /users/jaalto/elisp/test.el:;; @(#) ...
          ;;  /users/jaalto/elisp/test.el:;; $Id: ...
          (cond
           ((and file (not (file-exists-p file)))
	    (format "TinyReplace: [press] invalid filename %s" file))
           ((and file (not (member file cache)))
            (raise-frame (select-frame w-frame))
            (push file cache)           ;Now we have dealt with it
            (cond
             ((not (file-writable-p file))
              (incf  read-only)
              (push file ro-cache))
             (t
              (save-excursion
                ;;  Also jumps to buffer if it's already in Emacs
                (setq buffer (find-file file))
                ;;  Open outline/folding before doing anything
                (ti::buffer-outline-widen)
                (ti::pmin)
                (cond
                 (no-confirm
                  ;; Automatic replace
                  (message "TinyReplace: Processing %s" file)
		  (while (search-forward str1 nil 'noerr)
		    (replace-match str2 nil t))
                  (with-current-buffer buffer
                    (save-buffer)))
                 (t
                  (funcall func str1 str2)
                  ;;  What to do after replace
                  (if (and
                       verb
                       (null no-confirm)
                       (buffer-modified-p)
                       (ti::char-in-list-case
                        (setq
                         ch
                         (ti::read-char-safe-until
                          (format
                           "\
%s: SPC s)ave n)ext k)save and kill C)ontinue all Q)uit-exit"
                           (file-name-nondirectory file))))
                        '(?s ?S  ?n ?N ?\b  ?\ ?C ?Q )))
                      (cond
                       ((ti::char-in-list-case ch '(?\ ?s ?S))
                        (with-current-buffer buffer (save-buffer)))
                       ((ti::char-in-list-case ch '(?n ?N))
                        nil)
                       ((ti::char-in-list-case ch '(?Q))
                        (throw 'exit t))
                       ((ti::char-in-list-case ch '(?C))
                        (with-current-buffer buffer (save-buffer))
                        (setq no-confirm t))
                       ((ti::char-in-list-case ch '(?k ?K))
                        ;;  Why with-current-buffer? Well I have automatic
                        ;;  select-buffer programmed to my my mouse movement,
                        ;;  so if I point some other frame, that buffer
                        ;;  gets activated.
                        ;;
                        ;;  In here we want to be sure that the right buffer
                        ;;  "the replace buffer" is touched.
                        (with-current-buffer buffer (save-buffer))
                        (kill-buffer buffer)))))))))))
          (select-frame o-frame)
          (forward-line 1))))
    (raise-frame (select-frame o-frame))
    (when verb
      (message
       "TinyReplace: Handled %s files, %s read-only: %s"
       (length cache) read-only
       (mapconcat 'concat ro-cache " ")))
    nil))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-replace-region (beg end str1 str2)
  "In region BEG END, find STR1 and replace with STR2."
  (interactive (tinyreplace-interactive-region-args "Region"))
  (tinyreplace-replace-region-1
   beg end (regexp-quote str1) str2 0 t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-replace-forward (str1 str2)
  "Find STR1 and replace with STR2 from current point forward.
See C-h f `tinyreplace-args-keymap-create' what key bindings
you can use. Normally C - l yanks, and \"\\\" key deletes line."
  (interactive (funcall tinyreplace--read-args-function))
  (tinyreplace-replace-region-1
   (point)
   (point-max)
   (regexp-quote str1) str2 0 t))

;;; ----------------------------------------------------------------------
;;; ** Not gurranteed to work interactively.
;;;
;;;###autoload
(defun tinyreplace-latex-blk-replace (str1 str2 blk &optional beg-re end-re)
  "Select latex block areas for replace.

Input:

 STR1 STR2      Find and replace with.
 BLK            Block delimiter to find
 BEG-RE END-RE  Region bound regexps."
  (interactive "sLatex equation Search: \nsReplace with: \nsBlock names: ")
  (let* ((cp        (point))            ;current point
         (block-re  (concat "\\(" blk "\\)"))
         (beg-re    (or beg-re (concat "begin{" block-re "}")))
         (end-re    (or end-re (concat "end{" block-re "}")))
         MARK
         beg end
         area-end
         move)
    (save-excursion
      (goto-char (point-max))       (setq MARK (point-marker))
      (goto-char cp)                    ;start from current point
      (while (and (if (null (re-search-forward beg-re nil t))
                      nil
                    (setq beg (point)))
                  (save-excursion
                    (if (null (re-search-forward end-re nil t))
                        nil
                      (goto-char (match-beginning 0))
                      (setq end (point))))
                  (< (point) (marker-position MARK)))
        (setq area-end                  ;leave the block end
              (tinyreplace-replace-region-1
               beg end (regexp-quote str1) str2 0 t)
              move
              (+ 2 area-end))
        (if (< move (point-max))
            (goto-char move)
          (goto-char (point-max)))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyreplace-latex-math-replace (str1 str2)
  "Find STR1 and replace with STR2 inside latex math blocks."
  (interactive "sLatex equation Search: \nsReplace with:")
  (let ((math-blocks "equation"))
    ;;   first $ .. $ blocks
    (save-excursion
      (goto-char (point-min))
      ;;   first $ .. $ blocks
      (tinyreplace-latex-blk-replace str1 str2 nil "[^\\][$]" "[^\\][$]")
      ;;  then the rest
      (tinyreplace-latex-blk-replace str1 str2 math-blocks))))

;;}}}

(provide    'tinyreplace)
(run-hooks  'tinyreplace-load-hook)

;;; tinyreplace.el ends here
