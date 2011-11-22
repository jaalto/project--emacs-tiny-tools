;;; tinycomment.el --- Smart comment setting utility

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1994-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
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

;; ........................................................ &t-install ...
;; - Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file
;;
;;      (require            'tinycomment)
;;      (global-set-key     "\M-;" 'tinycomment-indent-for-comment)
;;      (setq-default       comment-column 48)
;;
;;   Or use autoload and your .emacs starts quicker
;;
;;      (global-set-key  "\M-;" 'tinycomment-indent-for-comment)
;;      (autoload 'tinycomment-indent-for-comment "tinycomment" "" t)

;;}}}
;;{{{ Commentary

;;; Commentary:

;;}}}
;;{{{ Documentation

;;  Preface, Sep 1994
;;

;;      In 1994-10-18 Era Eriksson wrote in gnu.emacs.help that he didn't
;;      like 'modes' because they redefined his tab key strangely. What he
;;      wanted was tab 8 in _every possible case_. He wrote: "..if mode
;;      messes with tab key, I don't want it". He also wanted his comments
;;      always to be positioned at column 56 (tab #7). The problem was that
;;      how he could he add comments with tab key, when the mode occied it
;;      already. He also always used to program using `fundamental-mode';
;;      what a cool dude. As a result this package was born. The
;;      original lisp file sent  to Era was posted under name
;;      general-comment.el.
;;
;;  What's this all about, short introduction
;;
;;      Let's see...You're in C/C++ mode, and want to switch to better mode
;;      before starting to adjust comments. But wait, the new mode doesn't
;;      know about C++-comments! Or if you're editing ~/.Xdefauls, there is
;;      no mode for it (at the time of writing), no-one to know the comment
;;      syntax. Boom. Now it's time to give this packet a run. It hooks
;;      itself directly to `\M-;' replacing any previous function. The
;;      packages defines comment syntax and position on the fly when it can
;;      identify the file name. If the file isn't known then it passes
;;      control to mode to handle the commenting. This is handy in
;;      temporary buffers that do not have filename: e.g. *scratch* buffer.
;;      Repetitive calls to `M-;' shift between comment *classes*: comment
;;      is adjusted according to previous one, or move it on the line.
;;
;;  Overview of features
;;
;;      o   Replaces `M-;' comment key. Suitable for any major mode.
;;      o   Determine comment variables on the fly, no matter where you
;;          are or what mode you are using.
;;      o   There is no-list that tells not to touch this mode's commenting.
;;          This is for modes that has `comment-end' which aren't supported.
;;      o   Repetitive `M-;' converts single comment into *bigger* *classes*.
;;      o   Position new comment on empty line by looking at previous
;;          comment.
;;      o   It is possible to define column position for those comments
;;          that are not allowed to move This handy for long lines
;;          or special comments.
;;      o   If there are multiple so called comments, like $#var # comment
;;          in `perl-mode', the last # char is treated as a comment.
;;
;;  Limitations
;;
;;      This isn't designed for modes that have `comment-end', you get
;;      only '/* */' string e.g. in C-mode and no comment class shifting.
;;
;;  Examples
;;
;;      At the end of file there is simple general editing mode, which can
;;      be used for perl, shells, awk, C++ [sometimes]

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(ti::package-defgroup-tiny TinyComment tinycomment-- extensions
  "Smart comment setting utility
  Overview of features:

        o   Replaces M-; commenting key. Suitable for any major-mode
        o   Determines comment variables on the fly, no matter where you
            are or what mode you are.
        o   There is no-list that tells not to touch this mode's commenting.
            This is for modes that has comment-end. TIC can't handle those.
        o   Repetitive M-; converts single comments into 'bigger classes'
        o   Positions new comment on empty line by looking at previous
            comment.
        o   You can define column position for those comments that are not
            allowed to move This handy for long lines or special comments.
        o   If there are multiple 'comments' , like $#var # comment
            in perl-mode, TIC picks the last # char and treats
            it as a comment. Emacs commention cope similar situations.")

;;}}}
;;{{{ setup:

;;; ......................................................... &v-hooks ...

(defcustom tinycomment--load-hook nil
  "*Hook run when package has been loaded."
  :type 'hook
  :group 'TinyComment)

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinycomment--not-comment-re ".*[$]\\(#\\)[a-zA-Z]"
  "*Reject comment position according to subexpression 1.
When searching for comment position, the position found will be
rejected, if comment subexpression 1 match's position is the same as
initially found comment position. The test will be done with `looking-at' at
the beginnning of line.

      $#variable_in_csh
0123456789 123456789 12345       ;columns
       *                         ;found comment pos, reject it."
  :type 'regexp
  :group 'TinyComment)

(defcustom tinycomment--tab-call-no-alist
  '(fundamental-mode
    text-mode)
  "*List of modes which enable using TIC's own indent-for-code algorithm.

Most programming modes supply function that knows how to indent the
statement.  But in some cases mode does not supply it's own indent
function placed on variable `indent-line-function', which is called by
tab key."
  :type '(repeat (function :tag "Mode function"))
  :group 'TinyComment)

(defcustom tinycomment--adj-no-alist
  '(lisp-interaction-mode
    lisp-mode
    emacs-lisp-mode
    c-mode ;; we use use // in c++, not /* */
    pascal-mode)
  "*List of modes which disable converting comment class.

The function `tinycomment-adjust-comment' isn't suitable for all possible
modes. Eg. Lisp has it's own idea about placing comments according
to comment used.
      ;     --> comment column
      ;;    --> right to code level
      ;;;+  --> left margin.

This defines list of mode names where `tinycomment-adjust-comment' is suppressed
and the normal `indent-for-comment' is used."
  :type '(repeat (function :tag "Mode function"))
  :group 'TinyComment)

(defcustom tinycomment--comment-notify nil
  "*If non-nil allow printing notify messages.
When the comment syntax wasn't found according to file name.
The message is _not_ displayed when `buffer-name' contains '*'.

You may want to set this to 't for awhile, so that you can add all
those files that are missing from the list. When you're satisfied,
you can turn off the warning."
  :type 'boolean
  :group 'TinyComment)

(defcustom tinycomment--def-com-pos 'code
  "*Default comment position for empty lines.
Possible choices are:

  'code            code level indent [usually as previous code line]
  'cpos            normal `comment-column'

Note that 'cpos doesn't always put comment where you would expect, because
it looks back until it finds code. In spite of that, it normally works
well _inside_ functions"
  :type  '(choice
           (const code)
           (const cpos))
  :group 'TinyComment)

(defcustom tinycomment--comment-extra-arg 1
  "*See documentation of `tinycomment--comment-extra'."
  :type 'integer
  :group 'TinyComment)

(defcustom tinycomment--comment-extra-stop 63 ;TAB position 64
  "*See documentation of `tinycomment--comment-extra'.
The comment movement is not done if `current-column' > this variable."
  :type 'integer
  :group 'TinyComment)

(defcustom tinycomment--comment-extra 'tab
  "*This affects function `tinycomment-set-com'. Let's see an example:

    abcd abcd abcd abcd abcd abcd[x] abcd abcd # COMMENT

You have line, where line exeeds the comment column[x] and your
comment is at the end. This variable determines how such
line is handled when you now hit M-;

Current choices are:
    'tab      Insert tab between code and comment, so that they get
              separated. Any previous whitespace is deleted.
    'spc      Same, but insert space instead. The number or spaces inserted
              is told in variable  `tinycomment--comment-extra-arg'

None of these actions are carried out if the comment was placed in
column `tinycomment--comment-extra-stop' +1 or further. Such comment is
left untouched, because adjusting may push it out of the window edge."
  :type  '(choice
           (const tab)
           (const spc))
  :group 'TinyComment)

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-find-prev-com-col (com-start &optional not-this-col column-further)
  "Look upward to find previous comment column.

Input:

  COM-START      comment start string.
  NOT-THIS-COL   if given,  scan backward until different column
                 is found.
  COLUMN-FURTHER If number, comment searched must reside further
                 in in the line than this column.

Return:

  nil   unable to find previous comment
  col"
  (let ((loop t)
	(re   com-start)
	ret
	found)
    (save-excursion
      (while loop
        (beginning-of-line)
        (setq ret nil found nil)
        (if (setq found (re-search-backward re nil t))
            (setq ret (current-column)))
        (setq loop nil)                 ;default
        (if (or (null found)
                (null not-this-col))
            nil
          (if (not (= ret not-this-col))
              (setq loop nil)		;this will do !
            (setq loop t found nil)))	;keep searching
        (if (or (null found)
                (null column-further))
            nil
          (if (> ret column-further)	;this IS suitable !
              (setq loop nil)
            (setq loop t found nil))))) ;keep going
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-find-com-col ()
  "Look current line to find `comment-start'.

Return:

   nil
   nbr  column."
  (let ((no-com   (or tinycomment--not-comment-re   "dummy"))
        (max      (save-excursion (end-of-line) (point)))
        (clen     (length comment-start))
        (re       comment-start)
        ret found
        not-pos
        cp)
    (save-excursion
      (beginning-of-line)
      ;;  is there restrictions ?
      (setq not-pos (if (and (looking-at no-com)
                             (match-beginning 1))
                        (match-beginning 1)
                      nil))
      (while (re-search-forward re  max t) ; find last comment
        (setq found t))
      (if (null found)
          nil
        (backward-char clen)
        (setq cp (point))
        (if (eq cp not-pos)
            nil                         ;cannot use this
          (setq ret (current-column))))
      ret)))

;;}}}
;;{{{ positions

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-check-line (mode &optional arg)
  "Few commands to use to determine line data according to MODE and ARG.

Return:

  Depends heavily on the MODE"
  (let (ret
	re
	re2
	bp
	p
	p2)
    (if (null (symbolp mode)) nil       ;handle literals
      (cond
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((and (eq 'code-last-col mode) comment-start)
        (setq re comment-start)
        (save-excursion
          (beginning-of-line)
          (setq bp (point))

          (if (null (re-search-backward comment-start bp t))
              nil                       ;not moved anywhere
            (goto-char (match-beginning 0)))
          (skip-syntax-backward " " bp)
          (setq ret (current-column))))
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ;;   find *last* comment start pos
       ;;   like '// i = 1000;   // temporarily commented out'

       ((and (eq 'com-last-col mode) comment-start)
        ;;  the '.*' always matches up till last one
        (setq re (concat ".*\\(" comment-start  "\\)"  ))
        (save-excursion
          (beginning-of-line)
          (if (not (looking-at re))
              nil
            (goto-char (match-beginning 1))
            (setq ret (current-column)))))
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ;;  whole line is 'alone' comment, so that there is no
       ;;  double comments, return it's starting position
       ;;  Double comment is like the previous cond-case
       ((and (eq 'com-alone-col mode) comment-start)
        (setq re (concat "[ \t]*\\(" comment-start  "\\)"  ))
        ;;  notice COM + SPC in re2
        ;;  - user may write '// this is separator /////////////////////'
        ;;    But that's one comment
        ;;  - '// i = MaxPos; // commented'. There is SPC in second
        ;;    comment, so it has to be Double Commented line.
        (setq re2 (concat ".*\\(" comment-start  " \\)"  ))
        (save-excursion
          (beginning-of-line)
          (if (not (looking-at re))
              nil
            (setq p (match-beginning 1))
            (if (not (looking-at re2))
                (progn                  ;only single comment
                  (goto-char p)
                  (setq ret (current-column)))
              (setq p2 (match-beginning 1))
              (if (not (eq p p2))       ;Double Commented
                  nil
                (goto-char p)           ;same comment hit twice
                (setq ret (current-column)))))))
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'eolpos mode)
        (save-excursion (end-of-line) (setq ret (point))))
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'bolpos mode)
        (save-excursion (beginning-of-line) (setq ret (point))))
       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'emptyEol mode)
        ;; If the rest of line empty ?
        (setq ret (looking-at "[ \t]*")))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-find-comment-col (com-start)
  "Look upward to find possible COM-START position."
  (save-excursion
    (if (re-search-backward (regexp-quote com-start) nil t)
        (current-column))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-find-code-col (com-start)
  "Look upward to find possible code indent column. Use COM-START.
Eg.

   echo something
                     # ridiculous comment
   <now press ESC ;> # inserts comment here

The problem is that the first comment is considered as indent
for code by normal Lisp functions, when it should be the 'echo' line.
We look upward till there is code line that isn't full comment line.

NOTE:

  This doesn't work on C/C++, or any mode that has `comment-end',
  because we can't keep track of multiline comments.

Return:

  nbr           found code, proposed column returned.
  nil           unable to find proper code indent"
  (let ((re-com  (concat
		  "^[ \t]*" (regexp-quote com-start)
		  "\\|^[ \t]*$"))	;skip empty lines.
	(move t)
	point				;point mark
	ret)
    (save-excursion
      (while (and move
		  (eq ret nil))		;while RET is not set
        (re-search-backward "[^ \t]+" nil t)
        (if (save-excursion
              (beginning-of-line)
              (looking-at re-com))      ;ignore full comment lines.
            (if (eq (point) point)	;have we moved since?
                (setq move nil))
          ;; Maybe this real code line?
          (setq ret (1+ (current-column)))) ;1+ due to re-search
        (setq point (point))))
    ret))

;;}}}

;;; ######################################################### &comment ###
;;; The real engine parts to do the job

;;{{{ tinycomment-set-com

;;; ----------------------------------------------------------------------
;;; See simple.el (funcall comment-indent-function)
;;; - Funny thing is that you just can't set comment-column to NBR
;;;   and expect it to be comment place, because the indent-for-comment
;;;   might decide to move the position to another place!
;;; - This function instead, will always put comment there where
;;;   user want's it.
;;;
(defun tinycomment-set-com (&optional new)
  "Lighter version of function `indent-for-comment'.
Moves current comment tocomment-position. Doesn't signal any errors.

Features:

-  if there is multiple comments on the line, like in shell or
   perl code '#', the last one is considered as comment, *unless*
   optional artgument NEW is given. In that case, nothing is considered
   as old comments.
-  If line is too long for comment column it inserts additional SPC or TAB
   between the code and comment. See variable `tinycomment--comment-extra'

Return:

  t             position changed OR new comment added
  nil           position not allowed"
  (let* ((xtra     tinycomment--comment-extra)
         (x-spc    (make-string tinycomment--comment-extra-arg ?\ ))
         (x-spc    (if (eq 'tab xtra)
		       "\t"
		     x-spc)) ; what's the insert type ?
         (stop-col tinycomment--comment-extra-stop)
         (ep       (save-excursion
		     (end-of-line)
		     (point)))
         (skip     (or comment-start-skip
		       comment-start))
         found
         bp                             ;BEG of line point
         com-c
	 code-c
	 com-p
         ret)
    (beginning-of-line)
    (setq bp (point))

    (if (null new)                      ;did user say new vomment ?
        (while (re-search-forward skip ep t) ;last comment
          (setq found t)))
    (if (null found)
        (progn
          (end-of-line)
          (indent-to comment-column)
          (insert comment-start)
          (setq ret t))
      ;;  first set comment position
      (goto-char (match-beginning 0))
      (setq com-p (point))
      (setq com-c (current-column))     ;comment column
      ;; Now where is the code position
      ;; Give argument "nil" (no limit) for skip syntax function is line
      ;; is first line in buffer.
      (backward-char 1)
      (skip-syntax-backward " " (if (eq 1 bp)
                                    nil
                                  (1- bp)))
      (setq code-c (current-column))
      (goto-char com-p)
      (if (= comment-column com-c)
          nil                           ;nothing to do
        (setq ret t)
        (if (< code-c comment-column)
            (progn                      ;we can indent ok
              (delete-horizontal-space)
              (indent-to comment-column))
          ;;  line is too long to get position to comment-col
          (if (> com-c stop-col) nil    ;do not touch this
            (delete-horizontal-space)
            (insert x-spc)))))
    ret))

;;}}}
;;{{{ tinycomment-adj-com

;;; ----------------------------------------------------------------------
;;; Original idea in asm-mode.el by :
;;;   Martin Neitzel,  Techn. Univ. Braunschweig, W.Germany
;;;   BITNET/EARN:   neitzel@dbsinf6.bitnet    (mail via bitnet preferred)
;;
;;; - Thank you Martin! I'm Afraid the code isn't like yours any more,
;;;   but the same principle 'converting to bigger class'
;;;   is preserved.
;;; - This is self standing function.
;;;
;;; - I really should write this again some day, divide into more smaller
;;;   blocks of funcs...
;;;
(defun tinycomment-adjust-comment ()
  "Introduce a comment or convert an already existing comment to next class.
These are the known comment classes:

        1-- Left margin             ;; omitted if there is CODE on the line
        2-- indented like code
        3-- on comment column

Suggested usage: while writing your code, trigger this command repeatedly
until you are satisfied with the comment.

Comment on it's own line note:

- Following lines has two comment chars '#', I call it double commented line.
              # comment text # more text
              # set myVariable = 100;      # temporarily commented
  Because It's hard to know if the line is 'full comment' as in case 1, or
  has 'code temporarily commented out' as line 2, we always consider
  line as 'full comment' if line starts with `comment-start'.
- In this case whole line moves when converting to classes.

Code note:

-  `tinycomment-set-com' is used instead of standard `indent-for-comment'.
-  Considered adding 4th choice: indent like previous comment,
   but I decided 4th choice or 4 taps was too much...3 seemed ideal,
   so I left it out from 'full comment line'."
  (let* ((def-place tinycomment--def-com-pos)
         (tab-alist tinycomment--tab-call-no-alist)
         (ci        (current-indentation))
         (cc        (current-column))
         (com       comment-start)
         (col       comment-column)
         (clen      (length comment-start))
         ;;    handle ONLY lines that have only comment, no code.
         (re-com    (concat "^[ \t]*\\("
			    (regexp-quote com)
			    "+\\)"))
         (re-com2   (concat ".*"
			    (regexp-quote com)))
         cur-code-c
         prev-cc cur-cc                 ;various com-col points
         code-col
         class
         prev-code-ind
         cont
         tmp)
    (catch 'done
      (if (or (not (integerp col)) (not (stringp com)))
          (error "comment-[column/start] not set"))
      (when (string-match "^[ \t]*$" com) ;empty comment?
        (if (tinycomment-check-line 'emptyEol)
            (indent-to col)
          (message "tinycomment-adj: no comment-start defined.")
          (throw 'done t)))
      (setq cur-code-c (tinycomment-check-line 'code-last-col))
      (setq prev-cc (tinycomment-find-prev-com-col com col cur-code-c))
      (setq cur-cc (tinycomment-find-com-col)) ;current comment column
      ;;  - If we do NOT already have a comment, indent for a new one.
      (beginning-of-line)

      (unless (looking-at re-com)       ;comment on it's own line ?
        ;; .............................................................
        (cond
         ;;  no comment at all or not suitable comment ?
         ((or (null (setq tmp (looking-at re-com2)))
              (and tmp
                   (null (tinycomment-find-com-col)))) ;it isn't suitable
          (if (not (looking-at "[ \t]*$"))             ; CODE + no COM
              (progn
                (tinycomment-set-com 'new) ;Normal column, but it'll be changed
                (setq cur-cc (tinycomment-find-com-col))
                (setq class 3))         ;indent like prev line by DEF
            ;;  empty line
            (insert com)                ;see cont, it passes thru
            ;;  User propably want CODE level indent on empty line by DEF
            (if (eq def-place 'code )
                (setq cont t))))
         ;;   There is existing comment
         ((and cur-cc (= cur-cc col))   ;change class if possible
          (setq class 3))
         ((and cur-cc prev-cc           ;make sure these are set
               (or (null prev-cc) (= cur-cc prev-cc)))
          ;;   possibly change class to standard column
          (tinycomment-set-com)) ; no prev comment or position was same
         (t
          ;;  add New comment
          (tinycomment-set-com)))
        ;;   Determine how CODE + COM line is handled
        (when (eq class 3)
          (save-excursion
            (forward-line -1) (end-of-line)
            (setq prev-code-ind (tinycomment-find-comment-col com)))
          (if (or (null prev-code-ind)  ;No code found
                  (>= cur-code-c col)   ;cannot put to comment-col
                  (null prev-cc)   ;cannot convert to class, isn't set
                  (>= cur-code-c prev-cc) ;cannot use prev com class
                  (not (= cur-cc col)))   ;convert to com-col CLASS
              (progn
                (tinycomment-set-com))
            ;;   Convert to previous comment column class then
            (setq comment-column prev-cc) ; change temporarily
            (tinycomment-set-com)
            ;; restore value
            (setq comment-column col)))
        (if cont
            nil                         ;do we continue forward ?
          (if (not (eolp))
              (forward-char (length comment-start)))
          (throw 'done t)))
      ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
      ;;  There is a comment, convert it between classes.
      ;;  Also correct the search position due to S-FWD command
      (beginning-of-line)
      (if (re-search-forward comment-start (tinycomment-check-line 'eolpos))
          (goto-char (match-beginning 0)))
      (setq cc (current-column))        ;at front of comment
      (setq code-col (tinycomment-find-code-col com))
      ;;   First select where to convert?
      (cond
       ((= cc 0)                        ;BEG of line ?
        (setq class 1))
       ((or (eq code-col cc)
            (eq cc col))                ; in Comment column ?
        (setq class 0))
       ((and (not (eq ci 0))            ; prallel to code ?
             (not (eq cc col)))         ; this is rough guess...
        (setq class 2)))
      ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
      (cond                             ;Now the converting procedure
       ((eq class 0)                    ;BEG of line
        (delete-horizontal-space))
       ((eq class 1)                    ;code level
        (beginning-of-line) (delete-horizontal-space)
        ;; Are we allowed to indent this by ourself ?
        (if (null (memq major-mode tab-alist))
            (indent-according-to-mode)  ;let mode place the statement
          (if (null code-col)           ;no code
              (indent-to comment-column)
            (if (and (= cc 0) (= col 0)) ;if suggested POS is same
                (indent-relative))
            (indent-to col))))
       ((eq class 2)                    ;column level
        (indent-to col)))
      (forward-char clen))              ;after class change
    ;;  do we need to restore the point ? [experimental]
    ;;    (if save-excur (goto-char op))
    nil))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-status ()
  "Displays comment info."
  (interactive)
  (message
   (concat
    "cc=" (prin1-to-string comment-column) " "
    "cs=" (prin1-to-string comment-start) " "
    "ce=" (prin1-to-string comment-end) " "
    "css=" (prin1-to-string comment-start-skip) " ")))

;;; ----------------------------------------------------------------------
;;;
(defun tinycomment-set-c-vars-maybe (&optional cs ce cc css)
  "Set comment variables CS CE CC and CSS.
The names are `comment-start' `comment-end' etc. If some
comment variable is nil, it will be set to some harmless value."
  (if (null cs)                 (setq comment-start ""))
  (if (null ce)                 (setq comment-end ""))
  (if (not (integerp cc))       (setq comment-column 48))
  (if (null css)                (setq comment-start-skip "")))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycomment-indent-for-comment ()
  "Alternative to standard `indent-for-comment'.
Relies on file extension
and doesn't need specific mode to be turned on. Temporary buffers
that has no file name cannot be identified by this function, so
it passes control directly to mode. There is a chance you might not
even notice that this function is working on the background.

Verbose warnings are enabled by `tinycomment--comment-notify'
Special cases are handled by tinycomment--comment-extra* variables
Version info is on \\[tinycomment-version]."
  (interactive)
  (let* ((warn          tinycomment--comment-notify) ;; shorter name
         (no-list       tinycomment--adj-no-alist)
         (com-col       48)         ;default comment column if not set
         elt
         (mode-desc     (or (ti::id-info nil 'variable-lookup)
                            (concat
                             "code-"
                             (replace-regexp-in-string
                              "-mode" "" (symbol-name major-mode))))))
    (if mode-desc
        (setq elt (ti::id-cnv-txt2comment mode-desc)))
    (if warn
        (setq warn warn))               ;XE 19.14 ByteComp silencer
    (tinycomment-set-c-vars-maybe
     comment-start comment-end comment-column comment-start-skip)
    (cond
     ;; ........................................ mode's own commenting ...
     ((or (memq major-mode no-list)
          (null mode-desc))
      ;;   let mode handle comment classes, only IF NOT SET
      ;;   but let's correct some user mistakes first...
      (indent-for-comment))             ;mode call...)
     (t
      ;; ............................................... real engine ...
      (if (ti::nil-p comment-start)     ;they are not defined.
          (if elt                       ;we have comment info?
              (setq comment-start (car elt) comment-end (or (cdr elt) ""))))
      (tinycomment-set-c-vars-maybe
       comment-start comment-end comment-column comment-start-skip)
      ;;   if the position is NOT set use default comment position
      (if (not (integerp comment-column))
          (setq comment-column com-col))
      ;;  - The indent-for-comment WON'T work if this is nill
      ;;    See simple.el for function def.
      ;;  - We don't set _right_ value, just sufficent replacement.
      (setq comment-start-skip (concat comment-start "+"))
      (tinycomment-adjust-comment)
      (if (and warn (ti::nil-p comment-start))
          (message
           "TIC: unknown file, no comment syntax available")) ))))

;;}}}

;;{{{ example setup

;;; ......................................................... &example ...

;; (autoload 'turn-on-tinytab-mode "tinytab" "" t)
;;
;; (defun my-fundamental-mode ()
;;   "my fundamental-mode"
;;   (interactive)
;;   (fundamental-mode)
;;   (turn-on-tinytab-mode)
;;   (setq tinytab-tt-mode nil)
;;   (if (fboundp 'folding-mode)
;;       (ti::funcall 'folding-mode))
;;   (recenter)                                ;Show visible notification
;;   ;; delete possible comment settings
;;   (setq comment-start nil
;;        comment-end    nil
;;        comment-column nil
;;     ;;   very important to restore this !! See simple.el
;;         comment-indent-hook '(lambda () comment-column)))
;;
;; (add-hook 'c++-mode-hook  'my-c++-mode-hook)
;;
;; (defun my-c++-mode-hook ()
;;   (setq comment-column nil   ;; When set to nil, tinycomment.el takes over.
;;     comment-start nil
;;     comment-end nil))

;;}}}

(provide 'tinycomment)
(run-hooks 'tinycomment--load-hook)

;;; tinycomment.el ends here
