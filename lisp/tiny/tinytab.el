;;; tinytab.el --- Programmers TAB minor mode. Very flexible.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at the code with folding.el.

;; COPYIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file.
;;
;;     (require 'tinytab)
;;
;;   or use this; your .emacs loads up a bit quicker
;;
;;      (autoload 'tinytab-mode            "tinytab" "" t)
;;      (autoload 'tinytab-return-key-mode "tinytab" "" t)
;;
;;   Suggested keybindings
;;
;;      (global-set-key "\C-cT"        'tinytab-mode)
;;      (global-set-key [(shift tab)]  'tinytab-tab-del-key)
;;      (global-set-key "\C-c\C-m"     'tinytab-return-key-mode)
;;
;;  For more customisation, do this:
;;
;;       (add-hook 'tinytab-mode-define-keys-hook 'my-tinytab-keys)
;;
;;      (defun my-tinytab-keys ()
;;        "My tinytab key additions, override settings."
;;        ... code here ...)

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;; Preface, oct 1995
;;
;;      There was a post in gnu.emacs.sources (what an source of
;;      inspiration), where someone asked:
;;
;;          "Is there anyway to reset the number of spaces that TAB does?
;;           Like, I want to make it jump four spaces instead of the
;;           usual whatever.How can I set the tabs to 4?"
;;
;;      and the typical answer was:
;;
;;          "In .emacs, set the variable tab-stop-list, like so:
;;           (setq tab-stop-list (list 4 8 12 ...))"
;;
;;      Well, A regular user does not want to touch the original
;;      `tab-stop-list', because the 8 spaces per tab is the norm. But for
;;      programming the 4 tabs is norm, like for shell programming or for
;;      simple memos and text documents. The goal was to write a minor
;;      mode, which you can turn on and off, which handles _only_ tab key.
;;      This mode was supposed to be plain rigid. The tab goes where you
;;      want it, and you can control the amount of movement to either
;;      direction, back or forward.
;;
;;  Overview of features
;;
;;      o   Programmable TAB. If you set the count to to 4,
;;          you can virtually program "blindly" without any other modes.
;;      o   Selectable: 2, 4, 8 .. space indent.
;;      o   moving commands: tab-forward, tab-backward
;;      o   indent commands: tab indent forward, tab indent backward
;;      o   Simple positioning of braces { } with TAB key.
;;
;;      Extras
;;
;;      o   Special auto-indent function offered for return key.
;;          Switch it on, and you can continue your shell, awk, SQL, C++,
;;          Perl comments and more.
;;      o   C-c TAB enters interactive indentation mode where
;;          keys "qw" "as" abd "zx" control the amount of indentation.
;;
;;  What this package does?
;;
;;      Mimic `tab-stop-list' with minor mode if some analogy can be
;;      drawn. You only set one variable, that controls the amount of
;;      movement, whereas you would have to put many values inside
;;      `tab-stop-list'. The variable to control tab widths is:
;;
;;          tinytab--width-table
;;
;;      When the mode is off, the tab key behaves as the mode thinks
;;      it should behave. The tab step forward and backward keys
;;      respect `tinytab--width'. Normally the current position
;;      in the line is advanced, but if you select a region, all the
;;      lines are indented:
;;
;;          This text here
;;          More text Here
;;          And so on.
;;
;;          -- Supposing all of the above text was selected and TAB was pressed
;;
;;              This text here
;;              More text Here
;;              And so on.
;;
;;          -- Select all lines and press ESC-tab (or Shift-Tab) and lines
;;          -- are unintended
;;
;;          This text here
;;          More text Here
;;          And so on.
;;
;;          -- If you have to "shoot" carefully how much indentation is needed,
;;          -- select region and call C-c TAB
;;
;;           This text here             Use keys q - w   (un/indent by 1)
;;           More text Here                      a - s   (un/indent by 2)
;;           And so on.                          z - x   (un/indent by 4)
;;
;;      To change permanently current tab division, use function
;;      `tinytab-change-tab-width' which steps through list
;;      `tinytab--width-table'; tab factors 2, 4, and 8.
;;
;;  Major modes and this minor mode
;;
;;      When you use some programming mode, say C++ mode, it usually
;;      provides function to indent the line right with tab key.
;;      If you then turn on this mode, you loose the mode specific
;;      indenting, because turning on minor mode overrides the underlying
;;      major mode bindings. However this package co-operates with
;;      major modes so that it preserves the original indenting style in some
;;      extent. In variable `tinytab--tab-insert-hook' there is function
;;      `tinytab-tab-mode-control' which looks at variable
;;
;;          tinytab--mode-table
;;
;;      If the mode is listed in the table _and_ current point is at the
;;      *beginning* of line, then the line is handled by original major mode
;;      and not by this minor mode.
;;
;;      However, this minor mode is normally meant to be used as turn
;;      on/off basis in such programming modes that indent lines when you
;;      pressing tab key. Current compatibility function
;;      `tinytab-tab-mode-control' only allows you to get some flexibility
;;      when this mode is temporarily on. Bind this mode to some fast key
;;      which you can use to toggle this mode on/off when you need tab for
;;      a moment in programming modes.
;;
;;          (global-set-key "\C-cT" 'tinytab-mode)
;;
;;      If you don't want any support to major modes, put following
;;      into your $HOME/.emacs
;;
;;          (setq tinytab--mode-table nil)
;;
;;  Return key addition
;;
;;      This package also includes a little function
;;      `tinytab-return-key-mode' which will keep the line's indentation.
;;      You can bind it to key C-c RET:
;;
;;          (global-set-key "\C-c\C-m" 'tinytab-return-key-mode)
;;
;;      When the function is active, you can continue indenting from
;;      the current position, like this:
;;
;;          // Comment here. Call C-c C-m...and press RET
;;          // And it automatically indents here.
;;
;;      See variable
;;
;;          tinytab--auto-indent-regexp
;;
;;      what line prefixes are "copied" along with the indented spaces.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: mode

;;;###autoload (autoload 'tinytab-mode                  "tinytab" "" t)
;;;###autoload (autoload 'turn-on-tinytab-mode          "tinytab" "" t)
;;;###autoload (autoload 'turn-off-tinytab-mode         "tinytab" "" t)
;;;###autoload (autoload 'tinytab-commentary            "tinytab" "" t)

(eval-and-compile

  (autoload 'ti::macrof-minor-mode-wizard "tinylib" "" 'macro)

  (ti::macrof-minor-mode-wizard
   "tinytab-"
   " " ;; This used to be " +" to indicate "Plussed tab"
   nil
   "Tab"
   'TinyTab
   "tinytab--"                          ;parameters 1-6

   "Tab movement minor mode. Adjustable movement step.
If you're running non/windowed version, Try to figure out which key
combinations work there best, In X, you have more flexible bindings.

If region is active, the indentation  (backward or forward) is
applied to whole region.

References:

  tinytab--width

Mode description:

\\{tinytab--mode-map}

"
   "Tab indent mode"
   (progn
     (if tinytab-mode
         (tinytab-set-mode-name)))
   "Tab indent mode"
   (list                                ;arg 10
    tinytab--mode-easymenu-name
    ["Insert"                        tinytab-tab-key                     t]
    ["Delete"                        tinytab-tab-del-key                 t]
    ["Indent region forward"         tinytab-indent-by-tab-width         t]
    ["Indent region backward"        tinytab-indent-by-tab-width-back    t]
    ["Indent region dynamically"     tinytab-indent-region-dynamically   t]
    ["Forward"                       tinytab-tab-forward                 t]
    ["Backward"                      tinytab-tab-backward                t]
    ["Change step factor"            tinytab-change-tab-width            t]
    ["Return-key indent mode"        tinytab-return-key-mode             t]
    "----"
    ["Mode help"                     tinytab-mode-help                   t]
    ["Mode off"                      turn-off-tinytab-mode               t])
   (progn
     ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  non-X keys . .
     (define-key   root-map "\t"         'tinytab-tab-key)
     (define-key   root-map "\e\t"       'tinytab-tab-del-key)
     (define-key   root-map "\C-c\t"     'tinytab-indent-region-dynamically)
     (define-key   root-map "\C-c\C-m"   'tinytab-return-key-mode)
     ;; ........................................................ X-keys ...
     ;;  Standard key
     (define-key root-map (kbd "<S-tab>")        'tinytab-tab-del-key)
     ;;  Other keyboards
     (define-key root-map [(shift backtab)]      'tinytab-tab-del-key)
     (define-key root-map [(shift hpBackTab)]    'tinytab-tab-del-key) ;; XEmacs
     (define-key root-map [(shift kp-tab)]       'tinytab-tab-del-key)
     (define-key root-map [(shift iso-lefttab)]  'tinytab-tab-del-key))))

;;}}}
;;{{{ setup: hooks

(defcustom tinytab--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyTab)

(add-hook 'tinytab--load-hook 'tinytab-install-mode)

(defcustom tinytab--tab-insert-hook
  '(tinytab-tab-mode-control
    tinytab-tab-brace-control
    tinytab-tab-forward-insert
    tab-to-tab-stop)
  "*List of functions to call for inserting logical TAB.
If any of these functions return non-nil, it is assumed,
that the tab key was handled."
  :type  'hook
  :group 'TinyTab)

(defcustom tinytab--tab-delete-hook
  '(tinytab-tab-backward-del
    tinytab-bol-forward-del)
  "*List of functions to delete a logical TAB backward.
If any of these functions return non-nil, it is assumed,
that the tab handling was performed."
  :type  'hook
  :group 'TinyTab)

;;}}}
;;{{{ setup: public, user configurable

;;   Simple name is enough. Think this as "Tab +" or "extended tab" -mode
;;
(defcustom tinytab--mode-name-base " +"
  "*Minor mode's base name. Default value is ` +'."
  :type  'string
  :group 'TinyTab)

;;  If I accidentally press key I didn't meant to, I want to know
;;  about it. Like in empty line, where is no visual aids
;;
(defcustom tinytab--verbose nil
  "*Enable verbose messages."
  :type  'boolean
  :group 'TinyTab)

(defcustom tinytab--width-table '(2 4 8)
  "*After call to \\[tinytab-change-tab-width], cycle through list of tab positions.
Default values are '(2 4 8)"
  :type  '(repeat integer)
  :group 'TinyTab)

(defcustom tinytab--mode-table
  '(c++-mode cc-mode c-mode perl-mode cperl-mode java-mode)
  "*List of mode name symbols where the TAB key calls mode's TAB function.
But, only if the point is at the beginning of line."
  :type '(repeat (symbol
                  :tag "Mode name symbols"))
  :group 'TinyTab)

(defcustom tinytab--indent-region-key-message
  "Dynamic Indent <>: [qw]=1 [as]=2 [zx]=4 [Esc]=exit"
  "*Message displayed while in dynamic indent mode.
If you change this, see also `tinytab--indent-region-key-list'."
  :type  'string
  :group 'TinyTab)

(defcustom tinytab--indent-region-key-list
  '(?q ?w
       ?a ?s
       ?z ?x
       ?\e)
  "*List of keys to control dynamic indenting. Not case sensitive.
The first 6 keys go in pairs.

elt 0 1       left and right by 1
elt 2 3       left and right by 2
elt 4 5       left and right by 4
elt 6         exit key

If you chnage this variable, change also
`tinytab--indent-region-key-message'."
  :type '(list
          character character
          character character
          character character
          character)
  :group 'TinyTab)

(defcustom tinytab--auto-indent-regexp "[#!;*/]\\|REM\\|//"
  "*If previous line match this regexp, it is copied when you hit RET.
This allows e.g. continuing C++'s // comments.
See function `tinytab-return-key-mode' to turn on this auto-indent feature."
  :type  'string
  :group 'TinyTab)

;;}}}
;;{{{ setup: private

(defvar tinytab--width 4
  "Current tab division.")

;;}}}
;;{{{ extra functions

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytab-activate-region (beg end)
  "Activate region which was previously selected.")
  ;;  #todo:

;;; ----------------------------------------------------------------------
;;;
(defmacro tinytab-message (&rest body)
  "Run BODY if `tinytab--verbose' is non nil."
  `(when tinytab--verbose
     (message ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytab-region-bounds (&optional beg end)
  "Set variables BEG and END if region is active.
Otherwise use current line's end points."
  (if (and beg
           end)
      (list beg end)
    (if (and (region-active-p)
             ;;  Must be active too, otherwise may cause suprises to indent
             ;;  many lines in the buffer
             transient-mark-mode)
        (list (region-beginning)
              (region-end))
      ;; Interactive call. Single line
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytab-width ()
  "Return TAB advance."
  (if (not (integerp tinytab--width))
      (setq tinytab--width 4))
  tinytab--width)

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-indent-by-tab-width (&optional beg end back)
  "Indent region BEG END by current tab division.
Optional BACK indents backward. If BEG is nil and region is active,
determine BEG and END."
  ;; (interactive "*r")
  (let* ((width (tinytab-width))
         (div   (if back
                    (- 0 width)
                  width)))
    (multiple-value-bind (b e)
        (tinytab-region-bounds beg end)
      (if (eq b e)
          (tinytab-tab-forward-insert)
        ;;  This deactivates region, keep it on
        (indent-rigidly b e div)
        (tinytab-activate-region b e)))))

;;; ----------------------------------------------------------------------
;;; - So that you can bind this to fast key
;;;
(defun tinytab-indent-by-tab-width-back (&optional beg end)
  "Just shortcut to `tinytab-indent-by-tab-width'. Indent BEG END backward.
If BEG and END are nil, indent current line. (interactive call on current line)
If region is active, use that. (interactive + region selected)."
  (interactive)
  (multiple-value-bind (b e)
      (tinytab-region-bounds beg end)
    (tinytab-indent-by-tab-width b e 'back)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-bol-forward-del ()
  "If at beginning of line, delete `tinytab--width' spaces to the right."
  (interactive)
  (when (bolp)
    ;;  They may be \t, so convert all to spaces first and
    ;;  then we know if we can delete enough spcaes.
    (let* ((line   (buffer-substring (line-beginning-position)
                                     (line-end-position)))
           (width  (tinytab-width))
           (str    (and width
                        (with-temp-buffer
                          (insert line)
                          ;;  Only convert from the start, not whole line
                          (untabify (point-min) (min
                                                 (+ (point-min) width)
                                                 (point-max)))
                          (goto-char (point-min))
                          (when (looking-at
                                 (concat
                                  "^"
                                  (make-string width ?\  )))
                            (forward-char 4)
                            (buffer-substring (point) (point-max)))))))
      (cond
       (str
        (delete-region (point) (line-end-position))
        (save-excursion
          (insert str)))
       (tinytab--verbose
        (message "TinyTab: Cannot delete %d spaces" width))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-mode-control ()
  "If `mode-name' in in the list `tinytab--mode-table', call mode's tab key.
But only if
o  point is at the beginning of line.
o  the line is empty

This way you can partly mix e.g. C++ mode and this minor mode."
  (interactive)
  (let ((sym  (intern (format "%s-map" (symbol-name major-mode))))
	(point (point))
	map
	func)
    ;;  If we're at the beginnning of line, see if there is keymap for
    ;;  this current mode. Then try to find function for "\t" key
    ;;  and call it
    ;;
    (when (and (or (bolp)
                   (save-excursion
                     (beginning-of-line)
                     (looking-at "^[ \t]+$")))
               (memq major-mode tinytab--mode-table)
               (boundp sym)
               (keymapp (setq map (eval sym)))
               (setq func (lookup-key map "\t")))
      (call-interactively func)
      (if (eq (point) point)
          ;; Not moved? Then continue calling other functions.
          nil
        t))))

;;; ----------------------------------------------------------------------
;;; - For a little more smarter TAB key to line up { } braces
;;;   in variaous programming modes I made this. It's simple,
;;;   but suffices for most common needs.
;;; - I don't know how the C-mode or cc-mode does this, but, hey,
;;;   this is one way :-)
;;;
;;;
(defun tinytab-tab-brace-control ()
  "When hitting TAB, line up {} braces, otherwise do nothing special.
Remember that opening brace, the {, follows previous line's indentation
and } follows the \"{\".

Return:

  t             ,if TAB handled in this function.
  nil           ,nothing done."
  (interactive)
  (let (line
	rest
	indent
	pindent                        ;previous
	col
	ret                            ;flag
	handle
	equal)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... check brace ...
    (save-excursion
      (beginning-of-line)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... { . .
      (cond
       ((looking-at "^\\([ \t]*\\)\\({.*\\)")
        (setq indent (or (match-string 1) "")
              rest   (match-string 2)
              handle '{ )
        (save-excursion
          (forward-line -1)             ; peek previous line indent
          (let ((line (buffer-substring (line-beginning-position)
                                        (line-end-position))))
            (setq pindent (if (string-match "^[ \t]+" line)
                              (match-string 1 line)
                            "")))))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. } ..
       ((looking-at "^\\([ \t]*\\)\\(}.*\\)")
        (setq indent (or (match-string 1) "")
              rest   (match-string 2)
              handle '{)
        (save-excursion
          (cond
           ((re-search-backward "^\\([ \t]*\\){" nil t)
            (setq pindent (or (match-string 1) ""))))))))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... .. adjust brace ...
    (setq equal (and indent pindent (string= indent pindent))
          col   (and indent (length (subst-char-with-string indent))))
    (cond
     ((and indent
           pindent
           equal
           (memq handle '({ }))
           (< (current-column) col))
      ;;  Pressing TAB, before the {, puts cursor to brace.
      (move-to-column col)
      (setq ret t))
     ((and indent
           pindent
           (not (string= indent pindent)))
      ;;  This is reindent case: { and } didn't line up.
      (setq line (concat pindent rest)
            col  (current-column)
            ret  t)
      (setq col (current-column))
      (delete-region (line-beginning-position) (line-end-position))
      (insert line)
      ;;  If user is in the LEFT side of brace, put cursor to brace
      ;;  If user if in the RIGHT side, then move-to-column will
      ;;  preserve position.
      ;;
      (move-to-column col)
      (when (< col (length (subst-char-with-string pindent)))
        (re-search-forward "{\\|}")
        (forward-char -1))))
    ret))

;;}}}
;;{{{ code: return key

;;; ----------------------------------------------------------------------
;;; Replaces the RET key
;;; The arg is just due to: (newline &optional ARG1)
;;;
(defun tinytab-auto-indent (&optional arg)
  "Automatically indent according to previous line.
If optional ARG is given, behave exactly like 'newline' function."
  (interactive "P")
  ;;  The RE matches few common comments and empty whitespaces
  ;;  #     = shell
  ;;  ;     = lisp comments
  ;;  *     = C comments
  ;;  !     = .Xdefaults comments
  ;;  //    =  C++ comments
  ;;  REM   = oracle SQL comments
  ;;
  (let ((re  (concat "^[ \t]*\\(" tinytab--auto-indent-regexp "\\)*[ \t]*"))
        str)
    (cond
     (arg ;;  We do not do anything special if user has given arg.
      (newline arg))
     ((not (eolp)) ;; Now let's see if user wanted fresh line
      ;;  User wanted to divide a line.
      ;;  Read the line up till cursor point
      (if (> (current-column) 0)
          (setq str (buffer-substring (line-beginning-position) (point))))
      ;;  Ignore portion match --> nothing important matched
      (if (or (null str)
              (not (and (string-match re str)
                        (equal (length str)
                               ;;  The position (column in string)
                               (match-end 0)))))
          (newline)                ;something else than re, break line
        (let ((left-margin 0))       ;Can't add string right otherwise
          (newline)
          (insert str))))
     (t
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. else ...
      ;;  Let's peek current line
      (if (> (current-column) 0)
          (save-excursion
            (beginning-of-line)
            (setq str (if (looking-at re)
                          (match-string 0)))))
      (if (null str)                    ;Nothing important here
          (newline)
        (let ((left-margin 0))
          (newline)
          (insert str)))))))

;;}}}
;;{{{ code: tab

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-set-mode-name ()
  "Set mode name according to tab count in effect."
  (interactive)
  (let ((base  tinytab--mode-name-base)
	(val   (tinytab-width)))
    (setq tinytab--mode-name (format "%s%d" (or base "") val))
    (if (fboundp 'force-mode-line-update)
        (force-mode-line-update))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-change-tab-width ()
  "Toggle tab width according to `tinytab--width-table'."
  (interactive)
  (let ((verb  (interactive-p))
	(val   (tinytab-width))
	(table tinytab--width-table)
	elt)
    (cond
     ((not (integerp val))
      (setq val (car table)))           ;default value
     ((setq elt (memq val table))
      (if (eq 1 (length elt))           ;it's last item
          (setq val (car table))        ;pick first value
        (setq val (nth 1 elt))))        ;get next in the list

     (t                                 ;can't find value from table ?
      (setq val (car table))))          ;get first then.
    (setq tinytab--width val)           ;update
    (tinytab-set-mode-name)
    (if verb                            ;this does no harm....
        (message "TinyTab: Tab factor is now %d" val))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-backward-del ()
  "Move Tab backward.
Delete whitespace if all characters preceding the point are white spaces
_and_ the final position is in divide-able by current div-factor.

Eg. If you factor is 4, and there is 2 spaces before your cursor \"*\",
    This function will not delete the extra spaces, because it can't reach
    position 8.

         bar Geezy *
         12345678901234
            ^   ^     ^

In this case, calling the function is no-op.

References:

  `tinytab--tab-delete-hook'
  `tinytab--width'"
  (interactive "*")
  (let* ((div   (tinytab-width))
         (col   (current-column))
         (dest  (- col (% col div)))
         MARK
         str
         eob                            ;flag
         p                              ;points
         p2)
    (setq MARK  (save-excursion
                  (if (eobp)
                      (setq eob t)
                    (forward-char 1))   ;push marker one forward
                  (point-marker)))
    (if (= col dest )                   ; would be exact
        (setq dest (- col div )))
    (if (< dest 0)
        (setq dest 0))
    (if (= col 0)                       ;beg of line
        nil
      (move-to-column dest t)           ;converts tabs to spaces.
      ;; consider following:
      ;;    actual         seen
      ;;    12345678       123456789
      ;;    ----------------------------------
      ;;    #\thello      "#       hello"     ,suppose cursor is in "h"
      ;;    |  |
      ;;    |  point 3
      ;;    point 1
      ;;
      ;;    Now you indent back by 4, this is what happens
      ;;    12345678       12345678
      ;;    #   hello    "#   hello"
      ;;    |   |
      ;;    |  point 5                        , Geez!
      ;;    point 1
      ;;
      ;;    The tab is converted and it caused all point to be altered.
      ;;    That's why we have to use the marker, because it stays
      ;;    releative to text, in this case just _behind_ the letter "h"
      ;;
      (setq p  (if eob
                   (marker-position MARK)
                 (1- (marker-position MARK))))
      (setq p2 (point))
      (setq str (buffer-substring p2 p))
      (if (not (string-match "^[ \t]+$" str))
          (progn
            (tinytab-message "TinyTab: Can't reach previous tab position")
            (goto-char p))              ;do not move. Stay put.
        (delete-region p2 p)
        (tinytab-message "Tinytab: Deleted")))
    (setq MARK nil)))                   ;kill the marker

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-backward ()
  "Logical tab movement backward, until reach beginning of line."
  (interactive)
  (let* ((div   (tinytab-width))
         (dest  (- (current-column) div)))
    (if (< dest 0)
        (setq dest 0))
    (move-to-column dest t)
    (if (looking-at "[ \t]+$")
        (tinytab-message "TinyTab: Moved."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-forward-insert ()
  "Move tab forward, insert spaces or tabs, see variable `indent-tabs-mode'.

References:

  `tinytab--width'"
  (interactive "*")
  (let* ((col   (current-column))
         (div   (tinytab-width))
         (nbr   (- div (% col div)))
         div
         eob                            ;flag
         MARK                           ;marker
         str)
    (if (= 0 nbr)
        (setq str (make-string div ?\ ))
      (setq str (make-string nbr ?\ )))
    (insert str)
    (when indent-tabs-mode
      ;; - When we insert non-tabs, like in mode "tab 4", what happens is
      ;;   that we insert "    " + "    " ie. 4 + 4 spaces.
      ;; - but, we really like them to be like one "\t" code in text,
      ;;   So, let's fix the line every time something is inserted.
      ;; - We have to use markers again due to tabify.
      ;; - The EOB is special case
      ;;
      (setq MARK (save-excursion
                   (if (eobp)
                       (setq eob t)
                     (forward-char 1))
                   (point-marker)))
      (tabify (line-beginning-position) (point))
      (goto-char (if eob
                     (line-end-position)
                   (1- (marker-position MARK))))

      (setq MARK nil))                  ;kill it
    t))                                 ;we handled this

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-forward ()
  "Step logical tab forward. Does not insert anything. Stops at EOL.
Tabs are converted to spaces when needed; because you can't step inside
'\t' character in the line otherwise.."
  (interactive)
  (let* ((div   (tinytab-width))
         (col   (current-column))
         (nbr   (- div (% col div)))
         (ecol  (save-excursion (end-of-line) (current-column)))
         (dest  (+ col nbr)))
    (cond
     ((> dest ecol)
      (end-of-line)
      (tinytab-message "End of line."))
     (t
      (move-to-column dest t)
      (if (looking-at "[ \t]+$")
          (tinytab-message "Tinytab: Moved."))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-tab-key-insert ()
  "Run all functions in `tinytab--tab-insert-hook' until success."
  ;;  We could use this instead:
  ;;
  ;;  (run-hook-with-args-until-success 'tinytab--tab-insert-hook)
  ;;
  ;;  But then it would not be possible to debug which function gets
  ;;  called.
  (dolist (function tinytab--tab-insert-hook)
    (when (funcall function)
      (tinytab-message "TinyTab: %s" (symbol-name function))
      (return))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-tab-key (&optional beg end)
  "Run list of function to handle TAB key. See variable `tinytab--tab-insert-hook'.
If region is active and BEG and END are nil, then call function
`tinytab-indent-by-tab-width'."
  (interactive)
  (cond
   ((and (null beg)
         (region-active-p))
    (tinytab-indent-by-tab-width))
   (t
    ;;  Integrate this function with tinymail.el tab-key.
    (let* ((sym   'tinymail--complete-key-hook)
           (tinymail--complete-key-hook (if (boundp sym)
                                            (symbol-value sym))))
      ;; No-op: byte compiler silencer
      (if (null tinymail--complete-key-hook)
          (setq tinymail--complete-key-hook nil))
      (remove-hook sym 'tinymail-complete-guest-packages)
      ;; keep this at the end
      (when (memq 'tab-to-tab-stop tinytab--tab-insert-hook)
        (remove-hook 'tinytab--tab-insert-hook 'tab-to-tab-stop)
        (add-hook    'tinytab--tab-insert-hook 'tab-to-tab-stop 'append))
      (tinytab-tab-key-insert)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-tab-del-key (&optional beg end)
  "Remove indentation. See variable `tinytab--tab-delete-hook'.
If region is active, indent all lines backward."
  (interactive)
  (cond
   ((and (region-active-p)
         transient-mark-mode)
    (tinytab-indent-by-tab-width-back)
    (tinytab-activate-region beg end))
   (t
    (run-hook-with-args-until-success 'tinytab--tab-delete-hook))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinytab-return-key-mode ()
  "Turn on auto indent after RET key."
  (tinytab-return-key-mode 1 (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-off-tinytab-return-key-mode ()
  "Turn on auto indent after RET key."
  (tinytab-return-key-mode 1 (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytab-return-key-mode (&optional mode verb)
  "Toggle auto indent MODE / regular newline mode. VERB."
  (interactive)
  (let ((func 'tinytab-auto-indent)
	(now
	 (or (and
	      ;;  e.g. in fundamental-map this value is nil and
	      ;;  nil cannot be used as an keymap for lookup-key
	      ;;
	      (current-local-map)
	      (lookup-key  (current-local-map) "\C-m"))
	     (lookup-key  (current-global-map) "\C-m")))
	to)
    ;;  If we redefine return key here, user will nver get out.
    ;;  C-m is exit-minibuffer.
    (if (string-match "minibuf" (buffer-name))
        (error "TinyTab: tinytab-return-key-mode not allowed in minibuffer."))
    (setq verb (interactive-p))
    (cond
     ((or (null mode)
          (not (integerp mode)))
      (setq to (if (eq now 'tinytab-auto-indent)
                   'newline
                 func)))
     ((< mode 1)
      (setq to 'newline))
     (t
      (setq to func)))
    (local-set-key "\C-m" to)
    (if verb
        (message "TinyTab Return key auto indent %s"
                 (if (eq to func)
                     "on"
                   "off")))
    to))

;;; ----------------------------------------------------------------------
;;;
(defun tinytab-indent-region-dynamically (beg end)
  "Move region BEG END until exit key is pressed.
For ey setup, see `tinytab--indent-region-key-list'. The default keys  are:

LEFT   RIGHT
   q   w      by 1
   a   s      by 2
   z   x      by 4"
  (interactive "*r")
  (let ((count 1)
	(list    tinytab--indent-region-key-list)
	(message tinytab--indent-region-key-message)
	ch
	EXIT)
    (if (not (eq (length list) 7))
        (error "Not enough members in tinytab--indent-region-key-list."))
    (setq EXIT (nth 6 list))
    (while (not (eq EXIT (setq ch
                               (downcase
                                (read-char-exclusive message)))))
      (setq count nil)
      (cond
       ((eq ch (nth 0 list))
        (setq count -1))
       ((eq ch (nth 1 list))
        (setq count 1))
       ((eq ch (nth 2 list))
        (setq count -2))
       ((eq ch (nth 3 list))
        (setq count 2))
       ((eq ch (nth 4 list))
        (setq count -4))
       ((eq ch (nth 5 list))
        (setq count 4)))
      (if count
          (indent-rigidly (region-beginning) (region-end) count)))))

;;}}}

(add-hook 'tinytab--mode-define-keys-hook 'tinytab-mode-define-keys)

(provide   'tinytab)
(run-hooks 'tinytab--load-hook)

;;; tinytab.el ends here
