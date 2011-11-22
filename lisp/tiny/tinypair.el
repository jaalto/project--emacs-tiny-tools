;;; tinypair.el --- Self insert character (pa)irs () "" '' <>

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1995-2010 Jari Aalto
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
;;      ** MINOR MODE IS GLOBALLY ACTIVED WHEN YOU LOAD THIS FILE **
;;
;;      ;;  If you don't want global activation, use
;;      ;;  (defvar tinypair-mode nil)
;;
;;      (require 'tinypair)
;;      (tinypair-pair-type-select 'us)         ;; US `style'
;;      (tinypair-pair-type-select 'european)   ;; European 'style'
;;
;; Or use autoload and your Emacs starts faster
;;
;;      (autoload 'turn-on-tinypair-mode "tinypair")
;;      (add-hook <your-favourite-mode-hook> 'turn-on-tinypair-mode)
;;
;; If you want to turn the pairing off, use this:
;;
;;      M-x turn-off-tinypair-mode
;;
;; If you find any incorrect behavior, please immediately
;;
;;      o   Turn on debug M-x tinypair-debug-toggle
;;      o   Repeat the task
;;      o   Send bug report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, 1995
;;
;;      Pacakge paired-insert.el was posted to gnu.emacs.help group, and
;;      the code was not very well documented, The code showed lot of
;;      promises, but it lacked smart pairing, so this package was born instead.
;;
;;  Overview of features
;;
;;      o   Minor mode for paired characters.
;;      o   [] {} <> '' `' ""
;;
;;  Pairing control
;;
;;      *Remember* Always ask youself "Does this character the cursor is
;;      on, belong to _word_ class?", when you wonder why the pairing does
;;      not take in effect around the current character block.
;;
;;      The pair control is turned off for lisp mode, because it makes
;;      things worse if the pairing is on. The pairing in US style includes
;;
;;          `'
;;
;;      But European people almost never use backquote, intead they use:
;;
;;          ''
;;
;;  General pairing rules, just some of them
;;
;;      The pairing is done according to assoc lists in the following way:
;;
;;      o   if there is whitespace in front of char, then pair is inserted
;;      o   if character is over pair-end, no pairing takes effect.
;;          Like if you press opening paren when you're sitting on the
;;          closing paren:
;;
;;          ()
;;           *  <-- cursor here, pressing another ( does not pair.
;;
;;      but this behavior can be controlled through variable
;;
;;      o  if the cursor is at the beginning of the word (see syntax-table):
;;          -- if there is no pairs around the word, the whole word is paired.
;;          -- if there is pair, no pairing takes effect. The char acts as
;;          self-insert-command.
;;
;;      o   if previous character is word. then the '  doesn't pair. Reason
;;          is in english language .........................^
;;
;;      o   if character is repeated with prefix arg, the pairing isn't done,
;;          instead the character is repeated as in self-insert-command.
;;
;;  Cursor positioning
;;
;;      By default the cursor is positioned in the "middle" of the inserted
;;      pair chars. But for words, this is impossible, because there is no
;;      middle position. Please see the variables
;;
;;          tinypair--word-positioning
;;          tinypair--word-positioning-function
;;
;;      which allow you to customize cursor positioning after word pairing.
;;
;;  Word about syntax tables
;;
;;      Syntax table play a major part in pairing, especially pairing words
;;      correctly. Suppose you're writing in text mode:
;;
;;          ...txt txt... (help is the key)
;;                         *                    <-- cursor
;;
;;      If you now press " to have the word HELP paired, you don't get it,
;;      because normally text mode's syntax table says that "(" belongs
;;      to group "w" (word) too. So the actual word is seen as "(help" and
;;      the program determines that you're inside a word, thus not
;;      allowing the pairing.
;;
;;      In the other hand, if you were in any other mode, say in C++, the
;;      "(" is defined as open parenthesis syntax and it that case the
;;      seen word seen would have been "help" and the " character would have
;;      been added around the HELP string. Like this:
;;
;;          ...txt txt... ("help" is the key)
;;                          *                   <-- cursor
;;
;;      You may propably want quickly to see the syntax definition of
;;      characters; use function from my lisp libraries
;;
;;          (defalias 'syntax-info 'ti::string-syntax-info)
;;
;;      To return to this syntax problem in text mode, you could do the
;;      following, to make certain characters out of "w" class.
;;
;;          (defun my-syntax-default (table )
;;            "My syntax table settings."
;;            (modify-syntax-entry ?[ "_" table)
;;            (modify-syntax-entry ?] "_" table)
;;            (modify-syntax-entry ?{ "_" table)
;;            (modify-syntax-entry ?} "_" table)
;;            (modify-syntax-entry ?( "_" table)
;;            (modify-syntax-entry ?) "_" table)
;;            (modify-syntax-entry ?/ "." table)
;;            (modify-syntax-entry ?\' "\"" table)
;;            (modify-syntax-entry ?\" "\"" table)
;;            (modify-syntax-entry ?_ "w" table))
;;
;;      Then you just change the definitions of syntax table in hook:
;;
;;          (setq text-mode-hook 'my-text-mode-hook)
;;          (defun my-text-mode-hook ()
;;            (my-syntax-default  text-mode-syntax-table))
;;
;;      Do you wonder why I put {}()[] into "_" class and not in
;;      corresponding "(" or ")" classes? Well, my stig-paren just went
;;      beserk and started beeping the bell whenever I was nearby
;;      ")" class... The "_" shut it down, so I just chose it. You can
;;      of course put the chars into any class you like.
;;

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(ti::package-defgroup-tiny TinyPair tinypair-- extensions
  "self insert character pairs () \"\" '' <>
  Overview of features

        o   When you hit e.g. \", package will double the character. If you
            insertion point was on whitespace, the pair is inserted 'as
            is', but if point was in front of word, the word is surrounded
            with pair, provided that there we no pair already.
        o   Every pair beginning character may have it's own function
            to handle the pairing.")

;;}}}
;;{{{ setup: hook

(defcustom tinypair--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyPair)

;;}}}
;;{{{ setup: private

(defvar tinypair--us-alist
  '((?\(    ?\) nil)
    (?\[    ?\] nil)
    (?\{    ?\} nil)
    (?\<    ?\> tinypair-c-\<)
    (?\`    ?\' tinypair-c-\')
    (?\"    ?\" tinypair-c-\"))
  "Default US pairing alist.")

(defvar tinypair--european-alist
  '((?\(  ?\)   nil)
    (?\[  ?\]   nil)
    (?\{  ?\}   nil)
    (?\<  ?\>   tinypair-c-\<)
    (?\'  ?\'   tinypair-c-\')
    (?\`  ?\`   nil)             ;in perl, or shell you need backticks
    (?\"  ?\"   tinypair-c-\"))
  "Default European pairing alist.")

(defvar tinypair--alist tinypair--us-alist
  "The pairing alist '((?BEG-CHAR  ?END-CHAR FUNC-SYM) ..)
The FUNC-SYM element is optional. FUNC definition should have form,

accepted args:

  BEG-CHAR
  END-CHAR

Return values:

   t    force immediate pairing
   nil  pairing prohibited, main should insert char \"as is\"
   nbr  return control to main program.
   sym  func handled pairing, main program should terminate.

If the func element is missing, pairing is done always according to main
function's decision.")

;;; ........................................................ &v-public ...
;;; User configurable

;;  - Since not all people program with perl-mode when coding perl
;;    (I don't use it), the default function here is not always
;;    the best choice.
;;  - For detecting buffer contents in more robust way that just
;;    relying on the major-mode variable, see this
;;
;;        tinylibid.el -- Identifying buffer regardless of mode

(defcustom tinypair--all-pairing-disabled-function
  'tinypair-check-if-pairing-allowed
  "*Funtion to determine if any pairing is allowed.
Takes no args, and must return nil or non-nil.
If return value is non-nil, pairing is allowed."
  :type  'function
  :group 'TinyPair)

(defcustom tinypair--disable-mode-list
  '(message-mode
    gnus-summary-mode
    gnus-article-mode
    gnus-group-mode
    gnus-server-mode
    rmail-summary-mode
    rmail-mode
    vm-summary-mode
    vm-mode
    lisp-mode
    emacs-lisp-mode
    lisp-interaction-mode
    compilation-mode
    compilation-minor-mode
    gud-mode
    shell-mode
    comint-mode
    dired-mode
    vc-dired-mode
    cvs-mode
    rcs-mode
    Electric-buffer-menu-mode ;; std emacs ebuff-menu.el
    Buffer-menu-mode          ;; std Emacs
    bs-mode) ;; bs.el by <Olaf.Sylvester@kiel.netsurf.de>
  "*List of `major-mode' symbols, where the pairing is prohibited.
This variable is used by function `tinypair-check-if-pairing-allowed' which is
the default Manager for pairing. If you
change `tinypair--all-pairing-disabled-function', this variable is not used."
  :type  '(repeat symbol)
  :group 'TinyPair)

(defcustom tinypair--automatic-word-pairing t
  "*If non-nil, then the word pairing is allowed.
Eg when your cursor is at the beginning of word, pressing
pair-beg char will pair the whole word.

   txt          -->                (txt)"
  :type  'boolean
  :group 'TinyPair)

(defcustom tinypair--word-positioning-function
  'tinypair-word-position-function
  "*Function to position the cursor after pairing.
The value can also be a function symbol, which takes care of positioning
the cursor. Passed parameters are:

  BEG-POINT     ,point+1 where the beg-char were inserted
  BEG-CHAR      ,character

If function returns, non-nil it is assumed that function handled the
positioning. If it returns nil, then the control is returned to calling
program and the positioning is done according to variable
`tinypair--word-positioning'"
  :type  'function
  :group 'TinyPair)

(defcustom tinypair--word-positioning 'end
  "*How the cursor should be positioned after word pairing.
'beg          ,leave point after beg pair char
  'end          ,leave point after end pair char"
  :type  '(choice
           (const beg)
           (const end))
  :group 'TinyPair)

(defcustom tinypair--word-syntax-classes  '(?w ?$ ?. )
  "*List of syntax classes that are treated like WORD while pairing.
Eg if you have following text in LaTeX mode:

    $x^2+$
         *      <-- cursor here, now you want to pair it with (

You would normally get

    $x^2+()$
          *

Because the character $ is in class $. (You can check the class with
function `tinypair-syntax-info'). But when the is defined into this variable's
list, it is seen as \"word\", and the pairing is done like for word,
so that you get this:

     $x^2+($)
           *"
  :type  '(repeat character :tag "syntax class")
  :group 'TinyPair)

;;}}}
;;{{{ misc

;;; ............................................................ &mode ...

;;;###autoload (autoload 'tinypair-mode            "tinypair" "" t)
;;;###autoload (autoload 'turn-on-tinypair-mode    "tinypair" "" t)
;;;###autoload (autoload 'turn-off-tinypair-mode   "tinypair" "" t)
;;;###autoload (autoload 'tinypair-commentary      "tinypair" "" t)

(defvar tinypair-mode t
  "*Minor mode on/off flag.")

(make-variable-buffer-local 'tinypair-mode)

(ti::macrof-minor-mode-wizard
 "tinypair-" " p" nil  "Pair" 'TinyUrl "tinypair--"
 "Paired insert of characters.

Defined keys:

\\{tinypair--mode-map}"

 "Paired insert"
 nil
 ;;  The Menubar item takes space and is not useful at least not
 ;;  now, because there is no other functionality in this mode.
 nil
 nil
 (progn
   (define-key root-map "<"  'tinypair-self-insert-command)
   (define-key root-map "("  'tinypair-self-insert-command)
   (define-key root-map "{"  'tinypair-self-insert-command)
   (define-key root-map "["  'tinypair-self-insert-command)
   (define-key root-map "\"" 'tinypair-self-insert-command)
   (define-key root-map "'"  'tinypair-self-insert-command)
   (define-key root-map "`"  'tinypair-self-insert-command)
   (define-key root-map "\C-c\"" 'tinypair-pair-type-select)))

;;;### (autoload 'tinypair-debug-toggle "tinypair" t t)

(eval-and-compile (ti::macrof-debug-standard "tinypair" "--"))

(defalias 'tinypair-syntax-info 'ti::string-syntax-info)

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypair-word-class-p (class)
  "Check if CLASS of part of logical word classes."
  (memq class tinypair--word-syntax-classes))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-whitespace-p ()
  "Check that current point is sitting alone. No word next to it."
  (let ((prev (char-to-string (or (preceding-char) ?\n )))
        (next (char-to-string (or (following-char) ?\n ))))
    (and (string-match "[ \000\t\n\f\r]" prev)
         (string-match "[ \000\t\n\f\r]" next))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-word-class-skip (&optional back)
  "Skip forward all `tinypair--word-syntax-class' characters. Optionally BACK."
  (let ((ptr           tinypair--word-syntax-classes)
	(func          (if back
			   'skip-syntax-backward
			 'skip-syntax-forward))
	(point         (point)))
    (while ptr
      (funcall func (char-to-string (car ptr)))
      (if (eq (point) point)
          (pop ptr)
        ;; moved, start over.
        (setq point (point))
        (setq ptr tinypair--word-syntax-classes)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-word-beginning-paired-on-line (char-string)
  "Search backward CHAR-STRING and check if it's next to word in current line.
The point is not preserved.
See `tinypair--word-syntax-classes' for word definition."
  (interactive)
  (when (search-backward char-string (line-beginning-position) t)
    (if (tinypair-word-class-p (char-syntax (ti::buffer-read-char nil 1)))
        t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-elt-beg (elt)
  "Return begin pair from ELT."
  (nth 0  elt))

(defun tinypair-elt-end (elt)
  "Return end pair from ELT."
  (nth 1 elt))

(defun tinypair-elt-func (elt)
  "Return func from ELT."
  (if (= (length elt) 3)
      (nth 2 elt)
    nil))

;;}}}
;;{{{ pair control

;;; ----------------------------------------------------------------------
;;; "c"  refers to "checking func"
;;;
(defun tinypair-c-\' (ch1 ch2)
  "Check if tick '  character can be paired."
  (setq ch1 ch2) ;;  Byte compiler silencer
  ;;  - Check previous character. If it is a word, assume that user is
  ;;    writing regular text, like "I'm, it's, he's"
  ;;  - In fact this test is useful in old perl code too, where
  ;;    one writes "$package'variable".
  (cond
   ((tinypair-word-class-p (char-syntax (preceding-char)))
    nil)
   (t
    1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-c-\< (ch1 ch2)
  "Check if <  character can be paired. In HTML mode when there
is tag end,\"slash\", it's not desirable to have <>. Several other HTML
cases are checked too."
  (setq ch1 ch2) ;;  Byte compiler silencer
  (let ((ret 1))
    (cond
     ((memq (following-char) '(?/ ))
      (setq ret nil))
     ((eq major-mode 'shell-mode)
      (setq ret nil))
     ((and nil ;; currently disabled
           (not (tinypair-whitespace-p))
           (tinypair-word-beginning-paired-on-line "<"))
      (setq ret nil))
     ((or (looking-at "a[ \t]+href")
          (looking-at "hr[ \t]\\(size\\|wid\\)") ;1.1N <hr size=..>
          (looking-at "\\(th\\|tr\\)[ \t]align") ;1.1N tables
          (looking-at "p[ \t]+align")            ;1.1N <p align=..>
          (looking-at "\\(link\\|img\\|form\\) "))
      ;;  The word pairing isn't good in sgml/html mode.
      ;;
      ;;  If we have
      ;;     <A HREF="http://www.interplay.com">Interplay</a>
      ;;     <LINK REV="company"  HREF="http://www.interplay.com">
      ;;
      (setq ret nil)))
    ret))

;;; ----------------------------------------------------------------------
;;;
;;;  It's like you have opened ne quote
;;;   "txt txt txt
;;;       *               ,point here, and you want to end the quote..
;;;
;;;  In this case the pairing isn't desiredable
;;;
(defun tinypair-c-\" (ch1 ch2)
  "Check if \"  character can be paired. Looks backward if previous word
has starting pair.
"
  (let ((ret 1)                        ;default is main handling
	prev                           ;char
	point)
    ;;  The prev is nil if point is in BOB
    (setq  prev (char-syntax (or (ti::buffer-read-char nil -1) ?\ )))
    (if (and prev
             (tinypair-word-class-p prev))
        (save-excursion
          (setq point (point))
          ;;  "This statement has been paired"
          ;;                                 *cursor-here
          ;;
          ;;  If we find QUOTE next to WORD, then we assume that this
          ;;  is just closing QUOTE and we won't pair it
          (if (tinypair-word-beginning-paired-on-line "\"")
              (setq ret nil))
          (when ret
            (skip-syntax-backward "w")
            ;;  point must move, because the skip-syntax will skip
            ;;        "txt"
            ;;         2  1          1= before  2, after
            ;;  and reading that first " require backward char
            (when (and (not (= point (point))) ;require movement
                       (not (bobp))
                       (prog1 t (forward-char -1)) ;now we can move
;;;                (ti::d! (following-char) ch1)
                       (eq (following-char) ch1))
              ;;  disallow pairing
              (setq ret nil)))))
    ret))

;;}}}
;;{{{ other

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-check-if-pairing-allowed ()
  "Function to determine if pairing is allowed.
Returns t, when pairing is allowed for buffer."
  (not (memq major-mode tinypair--disable-mode-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-move (count)
  (cond
   ((or (not (integerp count))
        (<= count 1))
    nil)                                ;do nothing
   (t
    (backward-char (/ count 2)))))

;;; ----------------------------------------------------------------------
;;; - I used this before, may use it again...
;;;
(defun tinypair-move-logical-word (&optional count)
  "Move forward, skipping `tinypair--word-syntax-classes' COUNT times."
  (let* ((i             0)
         (count         (or count 1))
         (back          (if (< count 0)
                            'back
                          nil))
         (func          (if back 'skip-chars-backward
                          'skip-chars-forward)))
    (while (< i count)
      (funcall func " \f\t\r\n")        ;ignore whitespace
      (tinypair-word-class-skip back)
      (incf i))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-word-position-function (beg char)
  "Special cursor positioning function.
BEG is start point and CHAR is starting pair character."
  (cond
   ((char-equal char ?\( )
    ;;  Mostly in minibuffer and for lisp'ish things, put cursor
    ;;  after starting paren.
    (goto-char beg))
   ((or (char-equal char ?\' )               ;Move to next word.
        (char-equal char ?\` ))
    (let (point)
      (save-excursion
        (skip-chars-forward " \t\f")
        (unless (tinypair-whitespace-p)
          (setq point (point))))
      (goto-char point)))
   (t
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-word-pair (arg ch-beg ch-end)
  "Insert pair around word(s) ARG times using CH-BEG and CH-END."
  (let ((fid       "tinypair-word-pair: ")
	(pos-flag  tinypair--word-positioning)
	(pos-func  tinypair--word-positioning-function)
	ch1
	ch2
	read-ch
	count
	syntax-now
	syntax-prev
	tmp
	beg)
    (setq syntax-prev  (char-syntax
                        (setq ch1 (or (preceding-char) ?\ ))))
    (setq syntax-now  (char-syntax
                       (setq ch2 (or (following-char) ?\ ))))
    ;;  No-ops. XEmacs byte ocmpiler silencers
    (unless ch2
      (setq ch2 nil))
    (unless fid
      (setq fid nil))
    (tinypair-debug fid
                    "arg"
                    arg
                    "syntax now"
                    (char-to-string syntax-now)
                    "char syntax prev"
                    (char-to-string syntax-prev)
                    "Is-word-class now"
                    (tinypair-word-class-p syntax-now)
                    "Is-word-class prev"
                    (tinypair-word-class-p syntax-prev)
                    "CH1"
                    (char-to-string ch1)
                    "CH2"
                    (char-to-string ch2))
    (cond
     ((and (or (null arg)
               (integerp arg))
           (tinypair-word-class-p syntax-now)
           ;;  the $ character is consudered word in programming
           ;;  modes, so treat it specially. So is Perl's %
           ;;
           ;;      $<cursor>PATH
           ;;
           ;;  The wanted behavior is
           ;;
           ;;      ${PATH}     not ${}PATH
           ;;
           (or (null (tinypair-word-class-p syntax-prev))
               (ti::char-in-list-case ch1 '(?$ ?%))))
      (setq count (if (null arg)
                      1
                    arg))
      (if (< count 0)                   ;switch the values
          (setq tmp ch-beg   ch-beg ch-end   ch-end tmp))
      (insert ch-beg)
      (setq beg (point))
      (tinypair-move-logical-word count)
      (setq read-ch (or (ti::buffer-read-char nil 0) ?\  ))
      (tinypair-debug fid "count" count
                      "point" (point)
                      "read ch end"
                      (char-to-string read-ch)
                      (char-to-string ch-end))
      (unless (char-equal read-ch ch-end)
        (insert ch-end)))
     ((integerp arg)
      (insert (ti::string-repeat arg ch-beg)))
     (t                                 ;default case
      (tinypair-debug fid "default")
      (insert ch-beg ch-end)
      (backward-char 1)
      (setq pos-flag nil)))
    ;; ............................................ cursor positioning ...
    (setq tmp nil)                      ;"status" of call
    (and (fboundp pos-func)
         (integerp beg)
         (setq tmp (funcall pos-func beg ch-beg)))
    (tinypair-debug fid "cursor>>" beg (fboundp pos-func) tmp)
    (cond
     ((not (null tmp))                  ;function handled this.
      nil)
     ((eq 'beg pos-flag)
      (and (integerp beg)
           (goto-char beg)))
     (t
      nil))))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;;
(defun tinypair-pair-type-select (&optional arg)
  "Pairing control center.
Input:
 nil 'us 'usa    Use US pairing.
 other value     Use European pairing style."
  (interactive "P")
  (if (interactive-p)
      (message "TinyPair: Selected %s pairing style "
               (if arg "European" "US" )))
  (cond
   ((memq arg '(nil us usa))
    (setq tinypair--alist tinypair--us-alist))
   (t
    (setq tinypair--alist tinypair--european-alist))))

;;; ----------------------------------------------------------------------
;;; - Original idea in 19.29+ package paired-insert.el. Unfortunately the
;;;   package didn't satisfy my needs, so here is better pairing func.
;;;
;;; - the 'pair' variable in this function is purposively set
;;;   many times, although it is not always necessary. It is just eases
;;;   following the program flow.
;;;
(defun tinypair-self-insert-command (arg)
  "Smart pairing. ARG is repeat count of character."
  (interactive "P")
  (let*  ((fid          "tinypair-self-insert-command: ")
          (nbr          (prefix-numeric-value arg))
          (word-pair    tinypair--automatic-word-pairing)
          (ch           last-command-event)
          (elt          (assoc ch tinypair--alist))
          ;;  If TinyEf is active in minibuffer prompt, turn ourself off.
          (pair-allow
           (if (and (boundp 'tief-mode)
                    (symbol-value 'tief-mode))
               nil
             (if (fboundp tinypair--all-pairing-disabled-function)
                 (funcall tinypair--all-pairing-disabled-function)
               t)))
          (pair         nil)            ;pair control
          (status       1)           ;see user configuration CHAR-FUNC
          direction                     ;character looking at cmd
          ch-func                       ;character function
          ch-beg
          ch-end
          syntax-now
          ch-now)
    (tinypair-debug fid
                    'ARG                arg
                    'CHAR               (char-to-string ch)
                    ch
                    'POINT              (point)
                    'PAIR-ALLOW-FLAG    pair-allow
                    'MODE               major-mode
                    'ELT                elt)
    (cond
     ((null pair-allow)
      (turn-off-tinypair-mode)
      ;; This isn't exactly right, e.g. in some modes the "'" or any pairing
      ;; character is not a self-insert-command, but a keymap prefix.
      ;; We run `self-insert-command' only if buffer is NOT read-only.
      (unless buffer-read-only
        (self-insert-command nbr)))
     ((null elt)                        ;Not defined for pairing
      (self-insert-command nbr))
     (t
      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. do pairing . .
      (setq ch-beg  (tinypair-elt-beg elt))
      (setq ch-end  (tinypair-elt-end elt))
      (setq ch-func (tinypair-elt-func elt))
      (setq syntax-now (char-syntax (setq ch-now (following-char))))
      (tinypair-debug fid 'POINT (point) ch-func)
      (if (fboundp ch-func)
          (setq status (funcall ch-func ch-beg ch-end)))
      (tinypair-debug fid
                      "CH-NOW"      (char-to-string ch-now)
                      'POINT        (point)
                      "CH-END"      (char-to-string ch-end)
                      "STAT"        status
                      "CH-FUNC"     ch-func
                      "SYNTAX-NOW"  (char-to-string syntax-now))
      (cond
       ((integerp status)
        (setq direction
              (cond
               ((integerp arg)
                (if (> arg -1) nil 'back))
               (t                       ;C-u forward
                nil)))
        ;; No-ops. XEmacs byte compiler silencers
        (unless direction
          (setq direction nil))
        (unless fid
          (setq fid nil))
        (tinypair-debug  fid "direction" (or direction 'forward)
                         "WORD-PAIR" word-pair)
        (cond
         ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
         ((char-equal ch-now ch-end)         ;already pair visible
          (tinypair-debug  fid "now = End"))
         ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
         ((char-equal syntax-now ?\ )        ;whitespace at point
          (setq pair t)                 ;ok, do pairing
          (tinypair-debug  fid "Whitespace 1 1 t"))
         (word-pair
          ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  words  ..
          ;; the default case
          ;;  handle smart pairing.
          (setq pair 'word))
         (t
          (tinypair-debug  fid "default word")
          (setq arg 1 pair t)))         ;main COND
        ;; ... ... ... ... ... ... ... ... ... ... ...  insert chars ? ...
        (tinypair-debug  fid "Doing... ARG; PAIR-flag" arg pair )
        (cond
         ((eq pair 'word)
          (tinypair-word-pair arg ch-beg ch-end))
         (pair
          (tinypair-word-pair nil ch-beg ch-end))
         (t
          (insert (ti::string-repeat nbr ch-beg)))))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... other status ..
       ((eq nil status)
        (insert ch-beg))
       ((eq t status)
        (insert ch-beg ch-end)
        (backward-char 1))
       ((symbolp status)
        nil))))))

;;}}}

(add-hook 'tinypair--mode-define-keys-hook 'tinypair-mode-define-keys)

(ti::add-hooks '(minibuffer-setup-hook
                 dired-mode-hook
                 cvs-mode-hook
                 gnus-summary-mode-hook
                 gnus-group-mode-hook
                 rmail-mode-hook
                 rmail-summary-mode-hook
                 vm-mode-hook
                 vm-summary-mode-hook)
               'turn-off-tinypair-mode)

(if tinypair-mode
    (turn-on-tinypair-mode))

(provide   'tinypair)
(run-hooks 'tinypair--load-hook)

;;; tinypair.el ends here
