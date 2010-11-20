;;; tinyprocmail.el --- Emacs procmail minor mode. Lint code checker.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;; Created:         1997-09
;; Keywords:        extensions
;;
;; Look at the code with folding.el.

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
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  $HOME/.emacs startup file. This must be the very first entry before
;;  any keybindings take in effect.
;;
;;      ;;  - Tell which procmail version you're using, see procmail -v
;;      ;;  - If you do not set this, tinyprocmail.el will call shell
;;      ;;    to find out the procmail version. (slower)
;;
;;      (setq tinyprocmail--procmail-version "v3.11pre7")
;;      (add-hook 'tinyprocmail--load-hook 'tinyprocmail-install)
;;      (require 'tinyprocmail)
;;
;; You can also use the preferred way: autoload
;;
;;      (autoload 'turn-on-tinyprocmail-mode  "tinyprocmail" "" t)
;;      (autoload 'turn-off-tinyprocmail-mode "tinyprocmail" "" t)
;;      (autoload 'tinyprocmail-mode          "tinyprocmail" "" t)
;;      (add-hook 'tinyprocmail--load-hook 'tinyprocmail-install)
;;
;;      ;;  Procmail files usually end to suffix "*.rc", like pm-file.rc
;;      ;;  Some older procmail files start with "rc.*", like rc.file-name
;;
;;      (autoload 'aput "assoc")
;;      (aput 'auto-mode-alist
;;            "\\.procmailrc\\|pm-.*\\.rc$\\|\\<rc\\.[a-z]"
;;            'turn-on-tinyprocmail-mode)
;;
;;  This source file includes sample procmail test file for Lint. You
;;  can unpack it if you have `gpg' and `tar' commands in your system.
;;  When the file has been unpacked, load pm-lint.rc file into buffer,
;;  and follow instructions in the file.
;;
;;      M-x tinyprocmail-install-files

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Sep 1997
;;
;;      Procmail may revolutionize your daily email management. If you
;;      receive more than 10 spam messages per day, you may start to wonder
;;      if there were any automatic way to handle mail, so that spam never
;;      lands on a $MAIL mailbox. Procmail can be one answer. It can be
;;      used to pre-filter all incoming mailing list messages and sort them
;;      out separately without bloating the primary inbox. You may already
;;      use Gnus to read the mailing lists, but the mail splitting work is
;;      best left to procmail. Why? Because procmail is always running,
;;      while your Emacs and Gnus isn't. Procmail processes incoming
;;      messages as soon as they are received and takes care of them, like
;;      saving UBE (unsolicited bulk email) messages to
;;      separate folders. And when Gnus us fired up, you can read the
;;      sorted mailboxes immediately.
;;
;;  What is Procmail?
;;
;;      Procmail is a mail processing utility for Unix (included also
;;      in Win32/Cygwin), which can help you filter your mail;
;;      sort incoming mail according to sender, Subject line, length
;;      of message, keywords in the message, etc; implement an
;;      ftp-by-mail server, and much more.  Procmail is also a
;;      complete drop-in replacement for your MDA. (If this doesn't
;;      mean anything to you, you don't want to know.)  Learn more
;;      about procmail at <http://www.procmail.org/>.
;;
;;  Some terms
;;
;;      ._UBE_ = Unsolicited Bulk Email.
;;      ._UCE_ = (subset of UBE) Unsolicited Commercial Email.
;;
;;      _Spam_ = Spam describes a particular kind of Usenet posting (and
;;      canned spiced ham), but is now often used to describe many kinds of
;;      inappropriate activities, including some email-related events. It
;;      is technically incorrect to use "spam" to describe email abuse,
;;      although attempting to correct the practice would amount to tilting
;;      at windmills
;;
;;  Overview of features
;;
;;      o   Minor mode for writing procmail recipes (use tab for indenting)
;;      o   Linting procmail code: from a batch command line or
;;          interactively. In interactive mode, user can auto-correct recipes
;;          on the fly.
;;      o   Font-lock supported.
;;      o   files that have extension .rc or name .procmailrc trigger
;;          turning on `tinyprocmail-mode' (see `auto-mode-alist'). Please
;;          check that the first line does not have anything that would
;;          override this, like "-*- text -*-".
;;
;;      Quick reference
;;
;;      o   M-x `tinyprocmail-mode' toggles Procmail recipe write mode
;;      o           C-c ' L  to Lint whole buffer interactively
;;      o   C-u     C-c ' L  to Lint whole buffer and gathers info.
;;      o   C-u C-u C-c ' L, same as above, but be less pedantic.
;;
;;      Required packages
;;
;;      o   tinylib*.el     Emacs/XEmacs/Cygwin support libraries
;;      o   tinytab.el      General programming mode. TAB key handling
;;      o   tinycompile.el  General parser for compile output. See Lint.
;;
;;  Writing procmail code
;;
;;      The coding functions are provided by other modules. The tab key
;;      advances 4 characters at a time, and minimalistic brace alignment
;;      is supported when you press tab before the ending brace.
;;
;;          TAB     tinytab-tab-key                         tinytab.el
;;
;;      The RET autoindents, but this can be turned off by calling
;;
;;          C-c ' RET   tinytab-return-key-mode
;;
;;      Whole regions can be adjusted with commands
;;
;;          C-TAB   tinytab-indent-by-div-factor         -->
;;          A-S-TAB tinytab-indent-by-div-factor-back    <--
;;          C-c TAB tinytab-indent-region-dynamically    <-->
;;
;;  Tabs and spaces
;;
;;      When the procmail mode is active, the tab key does not produce a
;;      tab character, but sufficient amount of spaces. There is a reason
;;      for this, mostly due to Lint parser which has to move up and down
;;      columnwise when checking the code. The movements can't be done if
;;      the code includes tabs. If you need a literal tab in your regexps,
;;      you can get it with standard emacs way 'C-q` `TAB'.
;;
;;  Aligning continuation lines with backslashes
;;
;;      In procmail, you use backslashes a lot, like in the following example.
;;      The backslashes here are put after each line, but this construct is
;;      error prone, because if you later on add new `echo' commands or
;;      otherwise modify the content, you may forget to update the
;;      backslashes.
;;
;;          :0 fh
;;          * condition
;;          | (formail -rt | \
;;             cat -; \
;;             echo "Error: you requested file"; \
;;             echo "that does not exist";\
;;             ) | $SENDMAIL -t
;;
;;      To fix this code block, you can use command C-c ' \ or
;;      `tinyprocmail-fix-backslashes-paragraph'. It would have been
;;      enough to write the _first_ backslash and then call C-c ' \
;;      and the rest of the backslashes would have been added below
;;      the same cloumn.
;;
;;          :0 fh
;;          * condition
;;          | (formail -rt |                                    \
;;             cat -;                                           \
;;             echo "Error: you requested file";                \
;;             echo "that does not exist";                      \
;;             ) | $SENDMAIL -t
;;
;;  Rules on how to write Procmail recipe
;;
;;      In order to use the linting service, this package requires that
;;      you write your procmail code in following manner. These rules are
;;      needed, so that it would be possible to parse the procmail code
;;      efficiently and more easily.
;;
;;       [recipe start]
;;
;;          :   # (old way) although legal procmail, illegal here. Use `:0'
;;
;;       [flag order]
;;
;;      In order to autocorrect read flags from the buffer, the flag order
;;      must be decided: and here is a suggestion. The one presented
;;      in the procmail man page "HBDAaEehbfcwWir" is errourneous, because
;;      flags "aAeE" must be first, otherwise it causes error in procmail.
;;      The idea here is that the most important flags are
;;      put to the left, like giving priority 1 for `aAeE', which affect
;;      the receipe immedately. Priority 2 has been given to flag `f',
;;      which tells if receipe filters somthing. Also (h)eader and (b)ody
;;      should immediately follow `f', this is considered priority 3.
;;      In the middle there are other flags, and last flag is `c', which
;;      ends the receipe, or allows it to continue."
;;
;;          :0 aAeE HBD fhbwWirc: LOCKFILE
;;             |    |   |  |   |
;;             |    |   |  |   (c)ontinue or (c)opy flag last.
;;             |    |   |  (w)ait and Other flags
;;             |    |   (f)ilter flag and to filter what: (h)ead or (b)ody
;;             |    (H)eader and (B)ody match, possibly case sensitive (D)
;;             The `process' flags first. Signify (a)ad or (e)rror
;;             receipe.
;;
;;      Every recipe starts with `:0' `flags:', but if you prefer `:0flags:'
;;      more, you can use following statement. This 'flag-together (or not)
;;      format is automatically retained when everytime you call lint.
;;
;;          (setq tinyprocmail--flag-and-recipe-start-style 'flags-together)
;;
;;       [lockfile]
;;
;;      The lockfile names must be longer than two characters. Shorter
;;      lockfile name trigger an error. Also lockfile must have
;;      extension $LOCKEXT or .lock or .lck; no other non-standard
;;      extensions are allowed. The lockfile name must be within
;;      charsert [-_$.a-zA-Z0-9/] and anything else is considered as
;;      a suspicious lock file name.
;;
;;          :0 : c          # Error, should this read :0 c: instead?
;;          :0 : file       # Invalid, should read "file.$LOCKEXT"
;;          :0 : file.tmp   # Invalid, non-standard extension.
;;          :0 : file=0     # Invalid filename (odd characters in name)
;;
;;       [condition line]
;;
;;      Do not add inline commnts immediately to the right of the
;;      condition line. Although never procmail reciped mail allow
;;      the comment below, this parser does not recognize it and
;;      will flag it as an error. There another reason to avoid writing
;;      the comment into condition lines: if recipe is in another system
;;      that does not have the most recent procmail, the recpipe will break.
;;      All in all, put comments *before* recipe, just below it.
;;
;;          * H B ?? regexp # valid procmail, llegal here: write "HB"
;;
;;       [Variables]
;;
;;      The literal value on the right-hand side must be quoted with
;;      double quotes if a simple string is being assigned. If there
;;      are no double or single quotes, then Lint assumes that you forgot
;;      to add variable dollar($). Try to avoid extra spaces in the
;;      variable initialisation construct that use `:-'.
;;
;;          DUMMY  = yes        # Warning, did you mean DUMMY = $yes ?
;;          VAR    = ${VAR:-1}  # No spaces allowed: "$ {" is illegal.
;;
;;       [program names]
;;
;;      Program `sendmail' must be named sendmail, but it can also be
;;      variable $SENDMAIL. Similarly, program `formail' must be named
;;      `formail' or it can be variable $FORMAIL. Use of $MY_SENDMAIL
;;      or $MY_FORMAIL are illegal and cause missing many lint checks.
;;
;;       [commenting style]
;;
;;      In recent procmail releases you're allowed to place comments
;;      inside condition lines. Lint will issue a warning about this
;;      practise if your procmail version does not support this. But
;;      while you may place comments inside conditions, they should be
;;      indented by some amount of spaces. The default indent is 4
;;      spaces.
;;
;;          * condition1    --> * condition
;;          # comment               # comment
;;          # comment               # comment
;;          * condition2        * condition
;;
;;      This is recommended for readability (separating conditions
;;      from comments) and Lint will try to fix these comment misplacements.
;;
;;       [redirecting to a file]
;;
;;      If you print something to file, then the shell redirection
;;      tokens, like `>', must have surrounding spaces. Otherwise they
;;      are not found from the procmail recipe code. (because > can be used in
;;      regexps).
;;
;;          :0 :
;;          | echo > test.tmp       # Ok. Do not use "echo>test.tmp"
;;
;;  Linting procmail code
;;
;;      Writing procmail recipes is very demanding, because you have
;;      to watch your writing all the time. Forgetting a flag or two,
;;      or adding unnecessary flag may cause your procmail code to
;;      work improperly. The Lint interface in this module requires
;;      that
;;
;;      o   You write your procmail code in certain way. (see above)
;;      o   buffer is writable and can be modified. This is due
;;          to fact that program moves up and down to the same column
;;          as previous or next line. In order to make such movements,
;;          the tabs must be expanded when necessary.
;;
;;      To help *Linting* you procmail code, there are two functions
;;
;;          C-c ' l         tinyprocmail-lint-forward
;;          C-c ' L         tinyprocmail-lint-buffer
;;
;;      These functions check every recipe and offer corrective
;;      actions if anything suspicious is found. If you don't want to
;;      correct the recipes, you can pass prefix argument, which
;;      gathers Lint run to separate buffer. In parentheses you see
;;      the buffer that was tested and to the right you see the
;;      program and version number.  In this buffer you can press
;;      Mouse-2 or RET to jump to the line.
;;
;;          *** 1997-10-19 19:37 (pm-test.rc) tinyprocmail.el 1.10
;;          cd /users/foo/pm/
;;          pm-test.rc:02: Error, Invalid or extra flags.
;;          pm-test.rc:10: Error, Invalid or extra flags.
;;          pm-test.rc:10: info, Redundant `Wc:' because `c:' implies W.
;;          pm-test.rc:11: String `>' found, recipe should have `w' flag.
;;          pm-test.rc:15: info, flag `H' is useless, because it is default.
;;
;;      The output buffer can be sorted and you can move between blocks
;;      with commands
;;
;;          sl      tinyprocmail-output-sort-by-line
;;          se      tinyprocmail-output-sort-by-error
;;          b       tinyprocmail-output-start
;;          e       tinyprocmail-output-end
;;
;;  Lint: auto-correct
;;
;;      In many cases the Lint functions are able to autocorrect the
;;      code: answer `y' to auto-correct question at current point. If
;;      you want to correct the place yourself, abort the Linting
;;      with `C-g' and fix the indicated line.
;;
;;  Lint: directives
;;
;;      Most of the time the Lint knows what might be best, but
;;      there may be cases where you have very complex procmail code
;;      and you know exactly what you want. Here are the Lint
;;      directives that you can place immediately before the recipe
;;      start to prevent Lint from whining. The word `Lint:' can have
;;      any number of surrounding spaces as long as it is the first
;;      word after comment.
;;
;;          # Lint: <Lint flags here>
;;          :0 FLAGS
;;
;;      The comment must be in the previous line, the following is _not_
;;      recognized.
;;
;;          # Lint: <Lint flags here>
;;          #   I'm doing some odd things here and ....
;;          :0 FLAGS
;;
;;      Here is list of recognized Lint directives. each directive must have
;;      leading space.
;;
;;      o   `-w'. In pipe "|" recipe, ignore exit code. If you don't give
;;          this directive, the missing "w" flag is suggested to put there.
;;      o   `-i'. If you have recipe that, 1) has no "f"  2) has no ">"
;;          3) has "|" action, then the recipe doesn't seem to store
;;          the stdin anywhere. This may be valid case e.g. if you use
;;          MH's rcvstore. You can suppress the "-i" flag check with
;;          this directive.
;;      o   `-c'. This is used in conjunction with `-i' when you only
;;          do something as a side effect and you reaally don't want to use
;;          (c) copy flag.
;;
;;  Lint: error messages
;;
;;      The error messages should be self-explanatory, but if you
;;      don't understand the message, please refer to *pm-tips.txt*
;;      file available at Sourceforge project "pm-doc".
;;      See `pm-tips.txt' and section that talks about variable
;;      definitions.
;;
;;  Lint: batch mode from command line
;;
;;      You can also lint procmail files from command line prompt
;;      like this.
;;
;;              % emacs -batch -q -eval                            \
;;                 '(progn (load "cl")                             \
;;                  (push "~/elisp" load-path)                     \
;;                  (load "tinyprocmail" )                         \
;;                  (find-file "~/pm/pm-test.rc")                  \
;;                  (tinyprocmail-lint-buffer-batch)) '
;;
;;      Change the filename "~/pm/pm-test.rc" to targetted for
;;      linting.  The Lint results will appear in file
;;      `tinyprocmail--lint-output-file' which is ~/pm-lint.out by
;;      default. Below you see a shell script to run the above command
;;      more easily. Rip code with `ti::package-rip-magic'
;;
;;* #!/bin/sh
;;* # pm-lint.sh -- LINT A procmail batch lint with emacs tinyprocmail.el
;;* #
;;* file=$1
;;* #
;;* EMACS=emacs
;;* out=$HOME/pm-lint.lst
;;* #
;;* #  notice all these 3 lines must be concatenaed together! There must be
;;* #  no \ continuation characters. to the right.
;;* #
;;* $EMACS -batch -q -eval
;;*     '(progn (load "cl") (push "~/elisp" load-path) (load "tinyprocmail")
;;*     (find-file "'"$file"'") (tinyprocmail-lint-buffer-batch) ) '  2>&1 $out
;;* #
;;* # end of pm-lint.sh
;;
;;  Highlighting
;;
;;      Just couple of words about the chosen regexps for procmail code.
;;      Notice, that if you make a mistake, the dollar($) in front of
;;      identifier is not highlighted. This should help with spotting
;;      errors by eye better.
;;
;;          $VAR = ${VAR:-"no"}
;;          |===
;;          |Error, you must not place '$' to the left here.
;;          |
;;          This dollar($) sign is not highlighted.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(defconst tinyprocmail--version-time "2010.1120.2052"
  "*Version of last edit.")

(eval-and-compile
  (defvar tinytab-mode nil)
  (autoload 'tinycompile-parse-line-goto    "tinycompile" "" t)
  (autoload 'turn-on-tinytab-mode           "tinytab"     "" t)
  (autoload 'turn-off-tinytab-mode          "tinytab"     "" t)
  (autoload 'tinytab-mode                   "tinytab"     "" t)
  (ti::overlay-require-macro
    (message "\
  ** tinyprocmail.el: overlay-* functions missing from this Emacs.")))

(ti::package-defgroup-tiny TinyProcmail tinyprocmail-- extensions
  "Procmail log minor mode
  Overview of features

o   Minor mode for writing Procmail recipes (use tab for
    indenting)
o   Linting procmail code: From batch command line or
    interactively. In interactive mode yuser can auto-correct code
    on the fly. Linting erformance is about 160 recipes in 15 seconds.
o   Font-lock supported.
o   files that have extension .rc or name .procmailrc trigger
    turning on `tinyprocmail-mode' (By using `auto-mode-alist'). Please
    check that the first line does not have anything that would
    override this, like '-*- text -*-'")

;;}}}
;;{{{ setup: public

;;; ......................................................... &v-hooks ...

(defcustom tinyprocmail--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-before-hook nil
  "*Hook run before `tinyprocmail-lint-forward'."
  :type  'hook
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-do-hook
  '(tinyprocmail-lint-recipe-start
    tinyprocmail-lint-condition-lines)
  "List of lint functions to check the recipe at point.

Call arguments:

 FLAGS          Read flags
 STD-FLAGS      Standardized flag sequence.

Function should offer fixing recipe if `tinyprocmail--lint-fix-mode' is activated
and it should write log if `tinyprocmail--lint' is nil."
  :type  '(repeat function)
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-after-hook nil
  "*Hook run after `tinyprocmail-lint-forward'."
  :type  'hook
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-after-hook nil
  "Hook run when `tinyprocmail-lint-forward' is about to finish."
  :type  'hook
  :group 'TinyProcmail)

;;; ..................................................... &v-functions ...

(defcustom tinyprocmail--flag-format-function
  'tinyprocmail-flag-format-default
  "Function to format given flags.
This function standardizes the flag order by calling
`tinyprocmail-flag-standardize'.

It must also respect the value of `tinyprocmail--flag-and-recipe-start-style':
e.g. if given 'Afbwic' the standard function adds one
leading space so that the recipe looks like  ':0 Afbwic'"
  :type  'function
  :group 'TinyProcmail)

;;; .......................................................... &public ...

(defcustom tinyprocmail--pipe-w-warning-ignore-regexp
  ".*|[ \t]*\\(echo\\|.*vacation\\)"
  "When checking pipe recipe and missing w flags, ignore matching regexp.
Say you have the following recipe:

  :0 hi:
  | echo \"status info\" > file

Then the \"w\" is not essential at all. The default rexgexp ignores all
these `echo' pipes and doesn't complaint about missing 'w'. Be carefull,
if you set this regexp, so that you don't miss important `w' warnings."
  :type  'string
  :group 'TinyProcmail)

(defcustom tinyprocmail--flag-and-recipe-start-style  nil
  "What is the receipe start style used.
If 'flags-together, then the receipe start llike looks like

  :0flags:

If any other value, then receipe start looks like

  :0 flags:"
  :type '(choice
          (const flags-together)
          (const nil))
  :group 'TinyProcmail)

(eval-and-compile
  (defun tinyprocmail-procmail-version ()
    "Call `procmail -v' to find out the version number.
procmail v3.22 2001/09/10
procmail v3.11pre7 1997/04/28 written and created by Stephen R. van den Berg
procmail v3.11pre4 1995/10/29 written and created by Stephen R. van den Berg"
    (let ((prg (executable-find "procmail")))
      (if (null prg)
          (message "\
  ** tinyprocmail.el: Warning, couldn't auto-set `tinyprocmail--procmail-version'.")
        (ti::string-match "^procmail[ \t]+v?\\([0-9]+[^ \t\n]+\\)"
                          1 (shell-command-to-string "procmail -v"))))))

(defcustom tinyprocmail--procmail-version (tinyprocmail-procmail-version)
  "The version number returned by `procmail -v'."
  :type  'string
  :group 'TinyProcmail)

(defcustom tinyprocmail--font-lock-keywords
  (list
   ;;   Seeing embedded tabs in procmail is crucial because
   ;;   procmail doesn't know [ \t]. This regexp highlights bracketed
   ;;   regexp if it contains tab
   ;;
   ;;   [   \n]
   ;;   ^   ^^^highlighted.
   '("\\[[^]\n]*\t[^]\n]*\\]"       . font-lock-keyword-face)
   '("#.*"                          . font-lock-comment-face)
   ;;   Recipe start :0
   ;;   The regexp says: Start with `:0' or `:' followed by spaces
   ;;   and characters, but you MUST ent with non-space. This matches
   ;;   `spaced' flags:
   ;;
   ;;       :0 B fh wi
   ;;
   ;;   But also `tight' flags
   ;;
   ;;       :0Bfhwi
   '("^[\t ]*\\(:0? *[ a-zA-Z]+[^ #\n]+\\)" 1 font-lock-type-face)
   '("^[\t ]*\\(:0\\)"                      1 font-lock-type-face)
   ;;  Special condiion line
   ;;
   ;;  * ! BH ?? regexp
   '("^[ \t]*\\*.*\\<\\(HB\\|BH\\)\\>.*[?][?]"  1 font-lock-reference-face)
   '("^[ \t]*\\*.*\\<\\([BH]\\)\\>.*[?][?]"     1 font-lock-reference-face)
   ;;  Special variable assignments
   (list
    (concat
     "[^_a-zA-Z0-9]"
     "\\("
     "VERBOSE"
     "\\|DELIVERED"
     "\\|COMSAT"
     "\\|LOG"
     "\\|EXITCODE"
     "\\|LOGFILE"
     "\\|LOGABSTRACT"
     "\\|MAILDIR"
     "\\|HOST"
     "\\|FROM_DAEMON"
     "\\|FROM_MAILER"
     "\\|TO_?"
     "\\|FORMAIL"
     "\\|SENDMAIL"
     "\\)"
     "[^_a-zA-Z0-9]")
    1 'font-lock-reference-face)
   ;;  variable expansion condition or variable extrapolation
   ;;
   ;;     * $
   ;;
   ;;     ${var}
   ;;
   '("\\* *\\([$]\\)"           1 font-lock-reference-face)
   '("\\(\\$\\){"               1 font-lock-reference-face)
   '("\\<\\(\\$\\)[A-Za-z]"     1 font-lock-reference-face)
   ;;  Left hand variable assignments
   ;;  $VAR = "value"
   ;;  LOG = "value"
   '("\\([A-Z_][A-Z0-9_]+\\)[\t ]*="         1 font-lock-keyword-face)
   ;; Lonely right hand variables
   ;; $VAR = $RIGHT_HAND
   '("\\${?\\([A-Z0-9_]+\\)}?\\>"   1 font-lock-keyword-face)
   ;; Standard programs called
   (list
    (concat
     "[^_a-zA-Z0-9]\\(" ;; Do not put \\< here
     "test"
     "\\|awk"
     "\\|cat"
     "\\|cut"
     "\\|echo"
     "\\|formail"
     "\\|[ezba]*?grep"
     "\\|head"
     "\\|perl"
     "\\|python"
     "\\|sed"
     "\\|sendmail"
     "\\|tail"
     "\\)[^_a-zA-Z0-9]")
    1 'font-lock-reference-face)
   ;;  External shell calls with backquote
   '("`\\([^' \t\n]+\\)'" 1 font-lock-reference-face t))
  "*Font lock keywords."
  :type  'sexp
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-font-lock-keywords
  (list
   '("`\\([^' \t\n]+\\)'" 1 font-lock-reference-face)
   '("Error,"            0 font-lock-keyword-face)
   '("Warning,"          0 font-lock-reference-face)
   ;;  No there is no mistake here. The "i" is in lowercase because
   ;;  when the errors are sorted, the order of the sort must be like this.
   ;;
   ;;   Error
   ;;   Pedantic
   ;;   Warning
   ;;   info
   '("info,"             0 font-lock-comment-face))
  "*Font lock keywords."
  :type 'sexp
  :group 'TinyProcmail)

(defcustom tinyprocmail--auto-mode-alist
  '(("\\.rc\\'\\|^rc\\.\\|procmailrc"   . turn-on-tinyprocmail-mode))
  "Items to add to `auto-mode-alist' to call `turn-on-tinyprocmail-mode'."
  :type '(repeat
          (list
           (regexp :tag "Regexp to match filename")
           (const 'tinyprocmail-mode)))
  :group  'TinyMbx)

(defcustom tinyprocmail--lint-fix-mode 'semi
  "*The mode of fixing code.
'auto   Automatic fixing.
'semi   Ask permission to fix.
nil     no fixing."
  :type '(choice
          (const nil)
          (const auto)
          (const semi))
  :group 'TinyProcmail)

(defcustom tinyprocmail--lint-log-verbose 'pedantic
  "If nil, then do not log new features available only in latest procmail.
If 'pedantic, warn about all possible things that may not work in older
procmail releases."
  :type '(choice
          (const nil)
          (const pedantic))
  :group  'TinyProcmail)

(defcustom tinyprocmail--lint-log nil
  "*If non-nil receord lint check to `tinyprocmail--lint-output-buffer'."
  :type 'boolean
  :group 'TinyProcmail)

;;}}}
;;{{{ setup: private

;;; ......................................................... &private ...

(defvar tinyprocmail--overlay nil
  "Overlay used.")

(defvar tinyprocmail--overlay-second nil
  "Overlay used.")

(defvar tinyprocmail--lint-output-buffer "*Procmail Lint*"
  "Log buffer for Lint.")

(defvar tinyprocmail--lint-output-file "~/pm-lint.out"
  "Where `tinyprocmail-lint-buffer-batch' should save the results.")

(defvar tinyprocmail--mode-output-map nil
  "Map useed in `tinyprocmail--lint-output-buffer'.")

(defvar tinyprocmail--mode-output-easymenu  nil
  "Ooutput mode menu.")

;;}}}
;;{{{ mode

;;;###autoload (autoload 'tinyprocmail-mode             "tinyprocmail" "" t)
;;;###autoload (autoload 'turn-on-tinyprocmail-mode     "tinyprocmail" "" t)
;;;###autoload (autoload 'turn-off-tinyprocmail-mode    "tinyprocmail" "" t)
;;;###autoload (autoload 'tinyprocmail-commentary       "tinyprocmail" "" t)
;;;###autoload (autoload 'tinyprocmail-version          "tinyprocmail" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinyprocmail-" " PM" "\C-c'"  "PM" 'TinyProcmail "tinyprocmail--"
   "Procmail coding minor mode.

Code writing: `tinytab-mode' on \\[tinytab-mode]

Mode description (main mode)
\\{tinyprocmail--mode-prefix-map}"
   "Procmail recipe coding"
   (progn                              ;Some mode specific things? No?
     (cond
      (tinyprocmail-mode
       (setq comment-start       "#"
             comment-start-skip  "#+[ \t]*"
             comment-end         ""
             indent-tabs-mode    nil)
       ;;   Should we turn on the font-lock me too?
       ;;   global-font-lock-mode is available in new emacs versions.
       ;;
       ;;   Font-lock must be turned on FIRST, then set
       ;;   `font-lock-keywords'
       (font-lock-mode-maybe 1)
       (tinyprocmail-font-lock-keywords
        tinyprocmail--font-lock-keywords 'mode-font-lock)
       (put 'tinyprocmail-mode 'tit tinytab-mode) ;Save previous
       (unless tinytab-mode
         (turn-on-tinytab-mode)))
      (t
       (tinyprocmail-font-lock-keywords
        tinyprocmail--font-lock-keywords 'mode-font-lock 'restore)
       (if (and tinytab-mode (get 'tinyprocmail-mode 'tit))
           (turn-off-tinytab-mode))
       (tinyprocmail-overlay-hide))))
   "Procmail mode"
   (list                                ;arg 10
    tinyprocmail--mode-easymenu-name
    ["Forward"                   tinyprocmail-forward                    t]
    ["Backward"                  tinyprocmail-backward                   t]
    ["Forward strict"            tinyprocmail-forward-strict             t]
    ["Backward strict"           tinyprocmail-backward-strict            t]
    "----"
    ["Lint forward"                tinyprocmail-lint-forward             t]
    ["Lint buffer"                 tinyprocmail-lint-buffer              t]
    ["Lint buffer and save output" tinyprocmail-lint-buffer-batch        t]
    ["Lint output, buffer display" tinyprocmail-output-display           t]
    ["Lint output, buffer clear"   tinyprocmail-output-clear             t]
    ["Lint output, kill file"      tinyprocmail-output-file-kill         t]
    "----"
    (list
     "Comment Hiding"
     ["Hide region"              tinyprocmail-hide-comment-text-region   t]
     ["Show region"              tinyprocmail-show-comment-text-region   t]
     ["Hide recipe"              tinyprocmail-hide-comment-text-recipe   t]
     ["Show recipe"              tinyprocmail-show-comment-text-recipe   t])
    (list
     "Misc"
     ["Return key toggle"        tinytab-return-key-mode                 t]
     ["Fix backslashes at point" tinyprocmail-fix-backslashes-paragraph  t]
     ["Package version"          tinyprocmail-version                    t]
     ["Package commentary"       tinyprocmail-commentary                 t])
    "----"
    ["Mode off"                    turn-off-tinyprocmail-mode            t])
   (progn
     (define-key   map "\\"      'tinyprocmail-fix-backslashes-paragraph)
     (define-key map  "?"        'tinyprocmail-mode-help)
     (define-key map  "Hm"       'tinyprocmail-mode-help)
     (define-key map  "Hc"       'tinyprocmail-commentary)
     (define-key map  "Hv"       'tinyprocmail-version)
     (define-key   root-map "\C-n"            'tinyprocmail-forward)
     (define-key   root-map "\C-p"            'tinyprocmail-backward)
     (define-key   root-map [(end)]           'tinyprocmail-forward)
     (define-key   root-map [(home)]          'tinyprocmail-backward)
     (define-key   root-map [(control end)]   'tinyprocmail-forward-strict)
     (define-key   root-map [(control home)]  'tinyprocmail-backward-strict)
     (define-key   map  "_"      'tinyprocmail-hide-comment-text-region)
     (define-key   map  "-"      'tinyprocmail-show-comment-text-region)
     (define-key   map  ":"      'tinyprocmail-hide-comment-text-recipe)
     (define-key   map  "."      'tinyprocmail-show-comment-text-recipe)
     (define-key   map  "l"      'tinyprocmail-lint-forward)
     (define-key   map  "L"      'tinyprocmail-lint-buffer)
     (define-key   map  "r"      'tinyprocmail-standardize-recipe-start)
     (define-key   map  "\C-m"   'tinytab-return-key-mode)
     ;;  Uppercase to prevent from errors.
     (define-key   map  "B"      'tinyprocmail-lint-buffer-batch)
     (define-key   map  "K"      'tinyprocmail-output-file-kill)
     (define-key   map  "d"      'tinyprocmail-output-display)
     (define-key   map  "c"      'tinyprocmail-output-clear)
     (define-key   map  "o"      'tinyprocmail-overlay-hide)
     (define-key   map  "?"      'tinyprocmail-describe-mode))))

;;;###autoload (autoload 'tinyprocmail-output-mode          "tinyprocmail" "" t)
;;;###autoload (autoload 'turn-on-tinyprocmail-output-mode  "tinyprocmail" "" t)
;;;###autoload (autoload 'turn-off-tinyprocmail-output-mode "tinyprocmail" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinyprocmail-output-" " PM-Lint" "\C-c'"  "PM-Lint"
   'TinyProcmail "tinyprocmail-output--"

   "Browsing Procmail lint output. See \\[tinyprocmail-lint]

Mode description

\\{tinyprocmail-output--mode-prefix-map}"
   "tinyprocmail"
   (progn
     (when (and (interactive-p)        ;On when user calls us directly
                ;; Mode is Now turned on, check Lint buffer and confirm
                tinyprocmail-output-mode
                (null (ti::re-search-check (concat "^" (regexp-quote "*** "))))
                (null (y-or-n-p
                       "No Prcomail Lint output found. Are you sure? ")))
       (setq tinyprocmail-output-mode nil)
       (error "Aborted."))
     (tinyprocmail-font-lock-keywords
      tinyprocmail--lint-font-lock-keywords 'lint-font-lock))
   "Procmail Lint output mode"
   (list                                ;arg 10
    tinyprocmail-output--mode-easymenu-name
    ["Find error"                tinycompile-parse-line-goto         t]
    ["Beginning of output"       tinyprocmail-output-start           t]
    ["End of output"             tinyprocmail-output-end             t]
    ["Save output"               tinyprocmail-output-file-save       t]
    ["Clear output"              tinyprocmail-output-clear           t]
    "----"
    ["Sort by line number"       tinyprocmail-output-sort-by-line    t]
    ["Sort by error"             tinyprocmail-output-sort-by-error   t]
    "----"
    ["Mode on  for all pm buffers" turn-on-tinyprocmail-mode-all-buffers t]
    ["Mode off for all pm buffers" turn-off-tinyprocmail-mode-all-buffers t]
    ["Mode help"                 tinyprocmail-output-mode-help       t]
    ["Mode off"                  tinyprocmail-output-mode            t])
   (progn
     (if (ti::emacs-p)
         (define-key root-map [mouse-2] 'tinycompile-parse-line-goto)
       (define-key root-map [(button2)] 'tinycompile-parse-line-goto))
     ;;  Define keys like in Compile
     (define-key map  "\C-c\C-c" 'tinycompile-parse-line-goto)
     (define-key map  "\C-m"     'tinycompile-parse-line-goto)
     (define-key map  "b"        'tinyprocmail-output-start)
     (define-key map  "e"        'tinyprocmail-output-end)
     (define-key map  "sl"       'tinyprocmail-output-sort-by-line)
     (define-key map  "se"       'tinyprocmail-output-sort-by-error)
     (define-key map  "S"        'tinyprocmail-output-file-save)
     (define-key map  "C"        'tinyprocmail-output-clear)
     (define-key map  "?"        'tinyprocmail-output-mode-help))))

;;; ......................................................... &install ...

;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload 'tinyprocmail-install-files "tinyprocmail" t t)
(ti::macrof-install-pgp-tar tinyprocmail-install-files "tinyprocmail.el")

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-off-tinyprocmail-mode-all-buffers (&optional verb)
  "Call `turn-on-tinyprocmail-mode-all-buffers' with parameter `off'. VERB."
  (interactive)
  (ti::verb)
  (turn-on-tinyprocmail-mode-all-buffers 'on verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinyprocmail-mode-all-buffers (&optional off verb)
  "Turn on or OFF function `tinyprocmail-mode' for all procmail buffers. VERB.
Procmail files start with `rc.' or end to `.rc' and file content
must match `^:0'."
  (interactive "P")
  (ti::verb)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (if off tinyprocmail-mode (null tinyprocmail-mode))
                 buffer-file-name
                 (string-match
                  "^rc\\.\\|\\.rc$\\|procmailrc"
                  buffer-file-name)
                 (ti::re-search-check "^:0"))
        (if  verb (message "TinyProcmail: Mode turned %s in %s"
                           (if off "off" "on") (buffer-name)))
        (if off
            (turn-off-tinyprocmail-mode)
          (turn-on-tinyprocmail-mode))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-install-auto-mode-alist (&optional uninstall)
  "Update `auto-mode-alist' to know about procmail *.rc files."
  (cond
   (uninstall
    (ti::assoc-replace-maybe-add
     'auto-mode-alist
     tinyprocmail--auto-mode-alist
     'remove))
   (t
    (ti::assoc-replace-maybe-add
     'auto-mode-alist
     tinyprocmail--auto-mode-alist)
    (if (interactive-p)
        (message "TinyProcmail: uninstalled")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-install (&optional uninstall verb)
  "Install package, or optionally UNINSTALL. VERB."
  (interactive "P")
  (ti::verb)
  ;;  It is crucial that these two hooks are in this order
  ;;  First runs `tinyprocmail-lint-malformed-start-recipe' and only after
  ;;  that the receipes can be found.
  (ti::add-hooks 'tinyprocmail--lint-before-hook
                 '(tinyprocmail-standardize-recipe-start
                   tinyprocmail-lint-malformed-start-recipe)
                 uninstall)
  (ti::add-hooks 'tinyprocmail--lint-after-hook
                 '(tinyprocmail-lint-malformed-var-defs
                   tinyprocmail-lint-malformed-misc
                   tinyprocmail-lint-malformed-brace
                   tinyprocmail-lint-find-wrong-escape-codes
                   tinyprocmail-lint-find-2spaces
                   tinyprocmail-lint-list-lint-directives)
                 uninstall)
  (tinyprocmail-install-auto-mode-alist uninstall)
  (turn-on-tinyprocmail-mode-all-buffers uninstall verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-find-file-hook ()
  "Turn on tipm mode if you're viewing log file."
  (when (and (not tinyprocmail-mode)
             (ti::re-search-check "^:0$")) ;It's procmail ok
    (tinyprocmail-mode 1)))

;;}}}
;;{{{ macro

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyprocmail-o (&rest body)
  "Move overlay to point and protect BODY. Overlay is hiddedn after body."
  `(unwind-protect
       (progn
         (tinyprocmail-overlay (point))
         ,@body)
     (tinyprocmail-overlay-hide)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyprocmail-output-macro (&rest body)
  "Go to `tinyprocmail--lint-output-buffer' and do BODY.
If buffer does not exist, do nothing."
  `(let ((buffer (get-buffer tinyprocmail--lint-output-buffer)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinyprocmail-fix-macro 'lisp-indent-function 1)
(defmacro tinyprocmail-fix-macro (message &rest body)
  "Fix. Display MESSAGE and do BODY."
  `(when (or (eq tinyprocmail--lint-fix-mode 'auto)
             (and (eq tinyprocmail--lint-fix-mode 'semi)
                  (tinyprocmail-o (y-or-n-p ,message))))
     ,@body))

;;; ----------------------------------------------------------------------
;; fmacro = function create macro
;;;
(defmacro tinyprocmail-fmacro-move (back method)
  "Make move function using BACK and METHOD."
  (let ((sym (intern
	      (format "tinyprocmail-%s-%s"
		      (if back
			  "backward"
			"forward")
		      (symbol-name   `,method)))))
    `(defun ,sym ()
       (interactive)
       (tinyprocmail-forward (quote ,back) (quote ,method)))))

;;}}}
;;{{{ misc

;;; ............................................................. misc ...

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-comment-line-p ()
  "Check if this ine is full comment line. Use `save-excursion'."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*#")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-comment-line-pp ()
  "Check if this ine is full comment line at current point forward."
  (looking-at "^[ \t]*#"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-string-valid-p (string &optional type)
  "Check is STRING is valid variable. Find any supicious character.
Input:
  STRING    variable or read filename.
  TYPE      if 'path; then check as path."
  (cond
   ((eq type 'path)
    (string-match "^[-_a-zA-Z0-9.$\\/@]+$" string))
   (t
    (string-match "^[_a-zA-Z0-9]+$" string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-supported-p (feature)
  "Check if FEATURE is supported by `tinyprocmail--procmail-version'."
  (let ((v (or tinyprocmail--procmail-version "")))
    (cond
     ((eq feature 'condition-middle-comment)
      (string-match "3.11pre7" v))
     ((error "Invalid feature %s" feature)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-directive-1 (&optional start-point)
  "Search Lint directive line backward from current point or START-POINT.
# Ignore checking of mising -w flag in this case
# Lint: -w
:0 fh
| doSomething

Return:

  str    directive flags."
  (save-excursion
    (if start-point (goto-char start-point))
    (end-of-line)
    (when (and (re-search-backward "^[ \t]*:0" nil t)
               (forward-line -1)
               (looking-at "^[ \t]*#[ \t]*Lint:\\(.*\\)"))
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-lint-directive-p (directive directive-flags)
  "Search Lint DIRECTIVE from DIRECTIVE-FLAGS."
  (when (stringp directive-flags)
    (string-match (concat " " (regexp-quote directive)) directive-flags)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-overlay (point)
  "Move overlay to POINT."
  (when (and (fboundp 'make-overlay)
             (fboundp 'move-overlay)
             (fboundp 'overlay-put))
    (or tinyprocmail--overlay
        (setq tinyprocmail--overlay (make-overlay 1 1)))
    (or tinyprocmail--overlay-second
        (setq tinyprocmail--overlay-second (make-overlay 1 1)))
    (dolist (elt '((owner tipm)
                   (priority       1)
                   (face           highlight)
                   (before-string  ">")))
      (multiple-value-bind (property value) elt
        (overlay-put tinyprocmail--overlay property value)
        (if (eq property 'before-string)
            (overlay-put tinyprocmail--overlay-second
                         'after-string " <<"))))
    (save-excursion
      (goto-char point)
      (move-overlay tinyprocmail--overlay
                    (line-beginning-position)
                    (line-end-position)
                    (current-buffer))
      (goto-char (line-end-position))
      (move-overlay tinyprocmail--overlay
                    (line-beginning-position)
                    (line-end-position)
                    (current-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-overlay-hide ()
  "Move overlay out of sight."
  (interactive)
  (when (fboundp 'move-overlay)
    (dolist (ov '(tinyprocmail--overlay tinyprocmail--overlay-second))
      (when (and (boundp ov)
                 (setq ov (symbol-value ov)))
        (move-overlay ov 1 1)
        (overlay-put ov 'before-string "")
        (overlay-put ov 'after-string  "")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-font-lock-keywords (keywords property &optional restore)
  "Use font-lock KEYWORDS and store original to PROPERTY. RESTORE original."
  (let ((sym 'font-lock-keywords))
    (when (boundp sym)
      (if restore
          (set sym (get 'tinyprocmail-mode property))
        (put 'tinyprocmail-mode property (symbol-value sym))
        (set sym keywords)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-log (point string &optional point-min)
  "Log POINT and STRING to lint buffer if `tinyprocmail--lint' is non-nil.

Input:

  POINT         The current error point
  POINT-MIN     Where is the logical `point-min' which we use to count
                the line numbers. Defualts to (point-min) but in case
                you're using narrow, this should be `point-min' before
                narrowing to check the recipe condition."
  (when tinyprocmail--lint-log
    ;;  Some safety measures
    (when (and point (not (integerp point)))
      (error "arg POINT is not integer"))
    (when (and string (not (stringp string)))
      (error "arg STRING is not stringp"))
    (let* ((buffer (get-buffer-create tinyprocmail--lint-output-buffer))
           (name   (buffer-name))
           (LINE   (if point
                       (save-excursion
                         (goto-char point)
                         (count-lines
                          (or point-min (point-min))
                          (if (bolp)
                              (1+ (point))
                            (point)))))))
;;;      (if (eq point 4266) (ti::d! LINE point point-min (bolp) ))
      (with-current-buffer buffer
        (ti::pmax)
        (if (and point string)
            (insert
             (format
              "%s:%03d: %s\n"
              name
              LINE
              string))
          (insert string))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-log-start ()
  "Start log by adding header into list log buffer."
  (tinyprocmail-log
   nil
   (format
    (concat
     "\n\n"
     "*** %s (%s) %s tinyprocmail.el %s\n%s")
    (ti::date-standard-date 'minutes)
    (buffer-name)
    (or tinyprocmail--procmail-version "")
    tinyprocmail--version-time
    (if (buffer-file-name)
        (concat "cd " (file-name-directory (buffer-file-name)) "\n")
      ""))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-recipe-start-p (&optional line)
  "Check if current LINE is recipe start line."
  (if line
      (string-match "^[ \t]*:0" line)
    (save-excursion
      (beginning-of-line)
      (looking-at "^[ \t]*:0"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-condition-line-p (&optional line)
  "Check if current LINE is condition line."
  (if line
      (string-match "^[ \t]*\\*" line)
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^[ \t]*\\*")
          (progn
            ;;  Peek previous line
            ;;  * condition \
            ;;    line \
            ;;    line-end    << suppose point is here
            (forward-line -1)
            (looking-at "^[ \t*]+.*[\\]"))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-assignment-line-p (&optional line)
  "Check if current LINE has assignment."
  (if line
      (string-match "^[^=]+=" line)
    (save-excursion
      (beginning-of-line)
      (or (looking-at "^[^=]+=")
          (progn
            ;;  Peek previous line
            ;;  * condition \
            ;;    line \
            ;;    line-end    << suppose point is here
            (forward-line -1)
            (or (looking-at "^[^=]+=.*[\\][ \t]*$")
                (progn
                  (tinyprocmail-skip-continuation-backward)
                  (forward-line 1)
                  (looking-at "^[^=]+="))))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-flag-string ()
  "Return base flag string."
  "aAeEHBDfhbwWirc:")

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-flag-p (char-string)
  "Check if one CHAR-STRING is valid flag."
  (ti::string-match-case char-string (tinyprocmail-flag-string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyprocmail-recipe-start-require ()
  "Flag error if not at recipe start."
  (unless (tinyprocmail-recipe-start-p)
    (error "Not recipe start line")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-brace-p (&optional line)
  "Check if cursor is under brace or in brace LINE. Return 'beg, 'end or nil."
  (interactive)
  (cond
   (line
    (save-excursion
      (cond
       ((looking-at "[ \t]*{")
        'beg)
       ((looking-at "[ \t]*}")
        'beg))))
   (t
    (or (if (char-equal (following-char) ?{) 'beg)
        (if (char-equal (following-char) ?}) 'end)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-action-line-ok-p ()
  "Point must be at the end of conditions.
This checks if the actions line is ok."
  (interactive)
  (let* ((opoint (point))
	 (point  opoint))
    ;;  Suppose there is no action line at all
    ;;
    ;;  :0
    ;;  * test
    ;;
    ;;  # comment
    ;;  :0[*] point is somewhere here, previous condition end.
    (beginning-of-line)
    ;;  - Eat all newlines and comments backward
    (setq point (point))
    (if (tinyprocmail-recipe-start-p)
        (forward-line -1))
    (when (tinyprocmail-skip-comments-backward)
      (if (tinyprocmail-condition-line-p)
          (forward-line 1))
      (setq point (point)))
    (goto-char point)
    (skip-chars-forward " \t")
    ;;
    ;;  :0  :0   :0   :0    :0        :0
    ;;  !   |    {    mbox  /dev/null $mbox

    (when (not (looking-at "[|!/][^#\n]+\\|[$]?[A-Z]\\|{"))
      (point))))

;;}}}
;;{{{ move: primitives

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-next-empty-line ()
  "Return point of next empty code line."
  (let ((list (ti::re-search-point-list
	       '("^[ \t]*$" "^[ \t]*#")
	       'beginning-of-line)))
    (if list
        (apply 'min list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-skip-regexp (regexp &optional backward)
  "Skip all lines that match `looking-at' REGEXP. Optionally BACKWARD."
  (let (done)
    (while (and (not (eobp))
                (looking-at regexp))
      (setq done t)
      (if backward
          (forward-line -1)
        (forward-line 1)))
    (when done
      (if backward
          (skip-chars-backward " \t")
        ;;  Go to first char in the line
        (skip-chars-forward " \t")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-skip-comments-forward ()
  "Skip all comments and whitespace lines."
  (tinyprocmail-skip-regexp "[ \t]*#\\|[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-skip-comments-backward ()
  "Skip all comments and whitespace lines."
  (tinyprocmail-skip-regexp "[ \t]*#\\|[ \t]*$" 'back))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-skip-continuation-forward ()
  "Skip lines that end to backslash."
  (tinyprocmail-skip-regexp ".*[\\][ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-skip-continuation-backward ()
  "Skip lines that end to backslash."
  (tinyprocmail-skip-regexp ".*[\\][ \t]*$" 'back))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-move-to-next-condition-line ()
  "Move to next line in condition.
Return:
  t      Sitting on condition line after move.
  nil    Not a condition line after move"
  (let ((cont-p (looking-at ".*[\\][ \t]*$")))
    (if (or (looking-at ":0")
            (save-excursion
              (beginning-of-line)
              ;; :0
              (looking-at "^[ \t]*:")))
        (forward-line 1)
      (forward-line 1)
      (when (and (not (looking-at "[ \t]*\\*")) ;; Beginning of recipe
                 cont-p)
        (forward-line -1)               ;Start from original line
        (inline (tinyprocmail-skip-continuation-forward))
        (forward-line 1))
      ;; * condition
      ;; # comment inside condition
      ;; * another condition.
      (when (looking-at "[ \t]*#")
        (inline (tinyprocmail-skip-comments-forward))))
    (skip-chars-forward " \t")
    (looking-at "[ \t]*\\*")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-move-to-condition-end ()
  "Go to condition end. Point must inside condition or recipe start."
  (let ()  ;; FIXME: reserved for variables
    (cond
     ((tinyprocmail-brace-p)
      nil)
     ((and (tinyprocmail-recipe-start-p)
           (save-excursion
             (forward-line 1)
             (not (looking-at "^[ \t]*\\*"))))
      ;;   Handle special case
      ;;
      ;;   :0 c:
      ;;   mailbox
      (forward-line 1)
      (skip-chars-forward " \t"))
     (t
      (while (and (not (eobp))
                  (or (tinyprocmail-move-to-next-condition-line)
                      ;;  - count empty lines too. This is usually users
                      ;;    mistake and error in procmail, but
                      ;;    we must find condition end.
                      ;;  - We accept only _1_ empty line between conditions
                      ;;    Other than that must be real big error.
                      ;;
                      ;;  :0
                      ;;  * condition
                      ;;
                      ;;  *condition
                      ;;  mbox
                      (and (looking-at "^[ \t]*$")
                           (save-excursion
                             (forward-line 1)
                             ;;  Must be condition line
                             (looking-at "[ \t]*\\*")))))
        (skip-chars-forward " \t"))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-move-to-macthing-brace (&optional no-adjust)
  "Go to { or } brace when sitting on } or {.
Input:
  NO-ADJUST     If non-nil, when goind to ending brace, do not
                put cursor on brace, but to next line."
  (unless (get 'tinyprocmail--mode-name 'syntax-table)
    (let* ((otable      (syntax-table))
           (table       otable))
      (modify-syntax-entry ?{ "(" table)
      (modify-syntax-entry ?} ")" table)
      (put 'tinyprocmail--mode-name 'syntax-table table)))
  (let ((otable (syntax-table)))
    (set-syntax-table (get 'tinyprocmail--mode-name 'syntax-table))
    (prog1
        (cond
         ((char-equal (following-char) ?{)
          (forward-list 1)
          (if no-adjust
              (forward-line 1)
            (backward-char 1)))
         (t
          (forward-char 1)
          (backward-list 1)))
      (set-syntax-table otable))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-move-to-recipe-end ()
  "Go to recipe block end. Return recipe bound: (beg . end) ."
  (let (beg)
    (if (not (tinyprocmail-recipe-start-p))
        (tinyprocmail-backward-strict))
    (setq beg (point))

    (tinyprocmail-move-to-condition-end)
    (if (tinyprocmail-brace-p)
        (tinyprocmail-move-to-macthing-brace 'no-adjust)
      (or (re-search-forward "^[ \t]*$" nil t)
          (error "TinyProcmail: Can't find recipe end.")))
    (cons beg (point))))

;;}}}
;;{{{ move: interactive

;;; ............................................................ &move ...

(tinyprocmail-fmacro-move nil  strict)
(tinyprocmail-fmacro-move back strict)

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-fix-backslashes-paragraph ()
  "Fix backslashes at point"
  (interactive)
  (ti::buffer-backslash-fix-paragraph))

;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-backward (&optional method verb)
  "Find previous procmail recipe. See METHOD in `tinyprocmail-forward'. VERB."
  (interactive)
  (ti::verb)
  (tinyprocmail-forward 'back method verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-forward (&optional back method verb)
  "Find next procmail recipe.

Input:

  BACK      Search backward
  METHOD    if 'any then search commented recipe too.
            if 'strict then only left flushed recipes.
            nil searches ^WHITESPACE:
  VERB      Be verbose

Return:

   nil or non-nil if moved."
  (interactive)
  (let ((opoint (point))
	(re  (cond
	      ((eq method 'strict)
	       "^:")
	      ((eq method 'any)
	       "^[# \t]*\\(:\\)")
	      (t
	       "^[ \t]*\\(:\\)"))))
    (ti::verb)
    (cond
     (back
      (beginning-of-line)
      (if (re-search-backward re nil t)
          (goto-char (match-beginning 1))
        (if verb
            (message "TinyProcmail: No more recipes backward."))
        (goto-char opoint)
        nil))
     (t
      (end-of-line)
      (if (re-search-forward re nil t)
          (goto-char (match-beginning 1))
        (if verb
            (message "TinyProcmail: No more recipes forward."))
        (goto-char opoint)
        nil)))))

;;}}}
;;{{{ hide

;;; ............................................................ &hide ...

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-hide-comment-text-region (beg end &optional show)
  "In region BEG END, hide or SHOW comment text ."
  (interactive "r\nP")
  (ti::narrow-safe beg end
    (goto-char (min beg end))
    (setq show (not show))
    (with-buffer-modified
      (while (re-search-forward "#.*" nil t)
        (add-text-properties
         (1+ (match-beginning 0)) (match-end 0)
         (list 'invisible show))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-show-comment-text-recipe ()
  "See `tinyprocmail-hide-comment-text-recipe'."
  (interactive)
  (tinyprocmail-hide-comment-text-recipe 'show))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-hide-comment-text-recipe (&optional show)
  "Hide or SHOW comment text in current recipe. point must be in recipe."
  (interactive "P")
  (let ((region (save-excursion (tinyprocmail-move-to-recipe-end))))
    (tinyprocmail-hide-comment-text-region
     (car region) (cdr region) show)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-show-comment-text-region (beg end)
  "See `tinyprocmail-hide-comment-text-region'. Use Region BEG END."
  (interactive "r")
  (tinyprocmail-hide-comment-text-region beg end 'show))

;;}}}
;;{{{ output mode:

;;; .......................................................... &output ...

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-end ()
  "Go to endline of output block."
  (interactive)
  (if (looking-at "^[ \t]*$")
      (skip-chars-forward " \t\n"))
  (re-search-forward "^[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-start ()
  "Go to start line of output block."
  (interactive)
  (unless (re-search-backward "^\\*\\*")
    (error "Invalid buffer format, No ** found.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-line-start ()
  "Go to start of first code line."
  (tinyprocmail-output-start)
  (forward-line 1)
  (if (looking-at "cd \\([^ \t\n]+\\)")
      (forward-line 1))
  (point))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-region ()
  "Return output region block '(beg . end)."
  (save-excursion
    (let ((beg (tinyprocmail-output-line-start)))
      (tinyprocmail-output-end)
      (cons beg (point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-sort-by-error (&optional reverse)
  "Sort block by error. Optionally REVERSE."
  (interactive "P")
  (let ((region (tinyprocmail-output-region)))
    (ti::save-line-column-macro nil nil
      (sort-regexp-fields
       (if reverse -1)
       "^[^:]+:[^:]+:\\([^,]+\\).*$" "\\1"
       (car region)
       (cdr region)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-sort-by-line (&optional reverse)
  "Sort block by line number. Optionally REVERSE."
  (interactive "P")
  (let ((region (tinyprocmail-output-region)))
    (ti::save-line-column-macro nil nil
      (sort-lines reverse (car region) (cdr region)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-file-save (&optional file)
  "Write `tinyprocmail--lint-output-buffer' to `tinyprocmail--lint-file' using FILE."
  (interactive)
  (save-excursion
    (if (null (ti::set-buffer-safe tinyprocmail--lint-output-buffer))
        (error "No `%s' buffer found." tinyprocmail--lint-output-buffer)
      (write-region (point-min) (point-max)
                    (or file tinyprocmail--lint-output-file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-file-kill ()
  "Kill `tinyprocmail--lint-file' if it exists."
  (interactive)
  (if (file-exists-p tinyprocmail--lint-output-file)
      (delete-file tinyprocmail--lint-output-file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-clear  (&optional verb)
  "Clear `tinyprocmail--lint-output-buffer' buffer if it exists. VERB."
  (interactive)
  (ti::verb)
  (save-excursion
    (cond
     ((ti::set-buffer-safe tinyprocmail--lint-output-buffer)
      (erase-buffer)
      (if verb
          (message "TinyProcmail: %s cleared"
                   tinyprocmail--lint-output-buffer))))))

;;}}}

;;{{{ Lint: Flag functions

;;; ............................................................ &flag ...

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-flag-read (string)
  "Read flags; including lock char, from STRING. If no flags read, return nil."
  ;;
  ;;  We have to count space, beacuse
  ;;
  ;;  :0 E fh c:   is valid recipe.
  ;;
  (setq string (replace-regexp-in-string "[ \t]+" "" string))
  (when (or (string-match "^[ \t]*:0?\\([^0#\n:]*:\\)" string)
            (string-match "^[ \t]*:0?\\([^0#\n]+\\)" string))
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-flag-standardize (string)
  "Standardize flag order.
The STRING must not contain anything else but flags.
Call `tinyprocmail-flag-read' first.

Return:

 string   Standardized order.
 symbol   Standardized order, flags, ok, but flags were uniqueied:
          e.g. 'fhih' --> 'fhi'
 1        Error, The input string had invalid flags.
 2        Error, flag conflict, aAeE used simultaneously"
  (let* ((flags      (tinyprocmail-flag-string))
         (hash       (make-vector (length flags) nil))
         (len        (length string))
         (ret        "")
         (ret-len    0)
         (conflict1  0)
         (conflict2  0)
         case-fold-search
         ch
         pos)
    (ti::dotimes counter 0 (1- (length string))
      (setq ch (substring string counter (1+ counter)))
      (when (string-match ch flags)     ;This is case sensitive match
        ;;  Get the hash position
        ;;
        ;;  aAeEf
        ;;    |
        ;;    pos = 2 if the ch was 'f'
        (setq pos (match-beginning 0))
        ;;  Increment the logical length: The character was valid
        (incf  ret-len)                 ;OK
        ;;   filter duplicates: If the hash doen't have this character already
        ;;   then add it to new string.
        (when (null (elt hash pos))
          ;; Mark the flas as used in hash table
          (aset hash pos t)
          (if (string-match ch "aAeE")
              (incf  conflict1))
          (if (string-match ch "wW")
              (incf  conflict2)))))
    ;;  Map the hash and see what flags it set
    (ti::dotimes counter 0 (1- (length hash))
      (if (elt hash counter)
          (setq ret (concat ret (substring flags counter (1+ counter))))))
    (cond
     ((or (> conflict1 1)
          (> conflict2 1))
      ;; There can be only one: aAeE
      2)
     ((eq len (length ret))
      ;; All flags checked and they were valid
      ret)
     ((eq len ret-len)
      ;; All flags ok, but there were duplicates, which were
      ;; removed.
      (make-symbol ret))
     (t
      ;; Error: invalid flags
      1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-flag-format-default (flags)
  "Default FLAGS format: ' STANDARDIZED-ORDER'. FLAGS must not have spaces."
  (let (new)
    (setq new (tinyprocmail-flag-standardize flags))
    (if (or (stringp new)
            (and (symbolp new)
                 (setq new (symbol-name new))))
        (if (not (eq tinyprocmail--flag-and-recipe-start-style
                     'flags-together))
            (concat " " new)
          new)
      (error "Invalid flags %s --> %s " flags new))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-flag-kill (&optional replace)
  "Kill flags in the line and optionally REPLACE and STANDARDIZE with SPACE."
  (let ((eol (line-end-position))
	beg
	end
	list)
    (when (and replace tinyprocmail--flag-format-function)
      (setq replace (funcall tinyprocmail--flag-format-function replace)))
    (beginning-of-line)
    (when (re-search-forward ":0" eol t)
      (setq beg (point))
      (setq list (inline (ti::re-search-point-list '(":" "#") nil eol))
            end  (if list (apply 'min list) eol))
      (delete-region beg end)
      (goto-char beg)
      (if replace
          (insert replace)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-flag-order-lint ()
  "Lint flag order at point.  Return (FLAGS . STD-FLAGS) or nil."
  (let ((line      (ti::remove-properties (ti::read-current-line)))
	(pedantic  (eq tinyprocmail--lint-log-verbose 'pedantic))
	flags1
	flags2
	str)
    (when (prog1 (setq flags1 (tinyprocmail-flag-read line))
            (unless flags1              ;No flags in this recipe
              (setq flags1 "" flags2 "")))
      ;;  User can write this, which we just ignore if the first
      ;;  character is $
      ;;
      ;;  :0 $FLAGS
      ;;
      (if (string-match "^[$]" flags1)
          (progn
            (tinyprocmail-log
             (point)
             (format
              "info, flag variable `%s' was not checked." flags1))
            nil)                        ;RET VAL
        (setq flags2 (tinyprocmail-flag-standardize flags1))
        ;;  How would the standardization go?
        (cond
         ((symbolp flags2)
          (setq str (format "Warning, duplicate flags found: `%s'" flags1))
          (tinyprocmail-log (point) str)
          (setq flags2 (symbol-name flags2))
          (tinyprocmail-fix-macro (concat str " Correct ")
                                  (tinyprocmail-flag-kill flags2)))
         ((eq 1 flags2)
          (setq str (format "Error, invalid or extra flags: `%s'" flags1))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro (concat str "(C-g to quit)"))
          (setq flags2 nil))
         ((eq 2 flags2)
          (setq str "Error, flag conflict; some simultaneous 'aAeEwW'")
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro (concat str "(C-g to quit)"))
          (setq flags2 nil))
         ((and  (not (string= flags1 flags2)) pedantic)
          (setq
           str
           (format
            "Pedantic, flag order style is not standard `%s', was `%s'"
            flags2 flags1))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro (concat str " Correct ")
                                  (tinyprocmail-flag-kill flags2))))))
    (if (and flags1 flags2)
        (cons flags1 flags2))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-conition-comment-move-up ()
  "Move comment upward and kill it."
  (let (str
	col)
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at "^[ \t]*\\(#.*\\)")
        (goto-char (match-beginning 1))
        (setq str (match-string 1)
              col (current-column))
        (ti::buffer-kill-line))
       ((looking-at ".*\\(#.*\\)")
        (setq str (match-string 1))
        (ti::replace-match 1)
        (if (looking-at "^[ \t]+")
            (setq col (length (match-string 0))))))
      (when str
        (if (null (re-search-backward "^[ \t#]*$" nil t))
            (ti::pmin)
          (end-of-line))
        ;;   COL is the indentation of the code.
        (insert (if col (make-string col ?\ ) "") str "\n")
        t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-condition-comment-embedded (&optional point point-min)
  "Handle embedded comment inside condition line. Point must be on comment.
POINT is used for lint log. POINT-MIN is (point-min) by default."
  (let ((supp    (tinyprocmail-supported-p 'condition-middle-comment))
	(col     (current-column))
	(opoint  (point))
	(tab     "    ")
	no-move)
    (or point (setq point opoint))
    (or point-min (setq point-min (point-min)))
    (cond
     ((save-excursion
        (beginning-of-line)
        (looking-at "^[ \t]*\\*.*#"))
      (tinyprocmail-log
       point
       "Error, comments not allowed in condition line."
       point-min)
      (tinyprocmail-fix-macro
       "Comments not allowed in condition, Move upwards "
       (setq no-move
             (tinyprocmail-conition-comment-move-up))))
     ((null supp)
      (tinyprocmail-log
       point
       "Error, embedded comment. Not supported by your procmail."
       point-min)
      (tinyprocmail-fix-macro "Error, embedded comment. Move upward "
                              (tinyprocmail-conition-comment-move-up)))
     (t                                 ;supported by this procmail
      (forward-line -1)                 ;Peek previous line
      (move-to-column col t)
      (cond
       ((looking-at "\\*")
        (tinyprocmail-log
         point
         "\
info, embedded comment, please indent it by 4 spaces (readability)."
         point-min)
        (save-excursion
          (forward-line 1) (move-to-column col t)
          (tinyprocmail-fix-macro "[recommendation] Indent comment "
                                  (insert tab))))
       ((looking-at "\\([ \t]+\\)#")
        ;;
        ;;     # comment
        ;;   # comment   << here
        (tinyprocmail-log point "info, embedded comment does not line up."
                          point-min)
        (save-excursion
          (forward-line 1) (move-to-column col 'force)
          (tinyprocmail-fix-macro "Line up comment "
                                  (insert (match-string 1))))))
      ;;  Back to normal line
      (forward-line 1)))
    (if (null no-move)                  ;The comment line was killed.
        (end-of-line))))

;;; --------------------------------------------------------- &comment ---
;;;
(defun tinyprocmail-lint-condition-comments (&optional point-min)
  "Check condition area and comments.
Buffer must be narrowed to condition lines.

:0              --> :0
* condition         * condition
# comment             # comment
* condition         * condition

Input:

  POINT-MIN   This is the value of logical `point-min' before calling
              the function. If aller narrowed to recipe, it must pass
              this variable, otherwise narrowed region's `point-min'
              is used to report error lines.

Return:

 nil
 non-nil   there are embedded  comments"
  (ti::pmin)
  (while (re-search-forward "#" nil t)
    (backward-char 1)
    (tinyprocmail-condition-comment-embedded (point) point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-recipe-start (flags std-flags)
  "Lint recipe start: flags and lockfile. Use FLAGS and STD-FLAGS."
  (let* ((lock-p    (string-match ":" (or flags "")))
         (pedantic  (eq tinyprocmail--lint-log-verbose 'pedantic))
         (pipe-w-re tinyprocmail--pipe-w-warning-ignore-regexp)
         (opoint    (point))
         (point-min (point-min))
         (bol       (line-beginning-position))
         (direc     (tinyprocmail-lint-directive-1))
         (dw        (tinyprocmail-lint-directive-p "-w" direc))
         (di        (tinyprocmail-lint-directive-p "-i" direc))
         (dc        (tinyprocmail-lint-directive-p "-c" direc))
         str
         str2
         point
         lock-file
         file
         cond-end
         fix-line fix list
         dev-null-p
         formail-p
         formail-d-p
         paren-p
         sendmail-p
         condition-p
         brace-p
         pipe-p
;;;      var-or-literal-p
         mbox-many-p
         mbox-p
         assignment-p
         forward-p
         redirection-p
         invalid-action-p
         fwd-invalid-p
         size-test-p)
    (catch 'end
      (tinyprocmail-move-to-condition-end)
      (setq cond-end  (point)
            flags     (or flags "")
            pipe-p    (looking-at "[ \t]*|")
            brace-p   (tinyprocmail-brace-p)
;;;       var-or-literal-p (looking-at "[$a-z]")
            forward-p         (if (looking-at "!") (point))
            fwd-invalid-p     (if (looking-at "!.*,") (point))
            ;; Illegal construct
            ;; :0
            ;; mailbox1 mailbox2
            mbox-many-p   (if (looking-at "[^|!{:\n]+[ \t]+[^ \t\n]") (point))
            mbox-p        (if (looking-at "[^|!{:\n]+[ \t]*$") (point))
            dev-null-p    (if (looking-at "/dev/null") (point))
            ;;  The actions line must be one of the following
            ;;
            ;;  ! forward
            ;;  | pipe
            ;;  $MBOX
            ;;  MBOX
            ;;
            ;; This Moves point
            invalid-action-p  (if (tinyprocmail-action-line-ok-p) cond-end)
            ;;  VAR=| formail -zX'Subject:'
            assignment-p      (if (looking-at ".*=|")
                                  (point)))
;;;    (ti::d! mbox-p dev-null-p (ti::read-current-line))
      ;; ....................................................... mailbox ...
      (when (eobp)
        (setq str "Error, prematuere end of buffer.")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat "[cannont-fix] " str)))
      (when (and mbox-many-p
                 (null assignment-p))
        (setq str "Error, Multiple mailboxes not allowed.")
        (tinyprocmail-log cond-end str)
        (tinyprocmail-fix-macro (concat "[cannot-fix] " str)))
      (when invalid-action-p
        (save-excursion
          (goto-char invalid-action-p)
          (skip-chars-forward " \t\n")
          (cond
           ((looking-at "{")
            (setq str "Error, empty lines before brace are not allowed.")
            (tinyprocmail-log invalid-action-p str)
            (tinyprocmail-fix-macro (concat str " Delete ")
                                    (beginning-of-line)
                                    (delete-region
                                     (point)
                                     (progn
                                       (skip-chars-backward " \t\n")
                                       (forward-line 1)
                                       (point)))))
           (t
            (setq str "Error, Invalid action line; no |!$A-Z or brace found.")
            (tinyprocmail-log invalid-action-p str)
            (save-excursion
              (goto-char invalid-action-p)
              (tinyprocmail-fix-macro (concat "[cannot-fix] " str)))))))
      ;; ....................................................... forward ...
      (when fwd-invalid-p
        (setq str "Error, invalid forward line. Maybe extra colons.")
        (tinyprocmail-log fwd-invalid-p str)
        (tinyprocmail-fix-macro
	    (concat str " Remove ")
	  (beginning-of-line)
	  (while (re-search-forward "," (line-end-position) t)
	    (ti::replace-match 0))))
      (when forward-p
        (save-excursion
          (goto-char forward-p)
          (when (looking-at ".*!.*\\(\\<-t\\> *\\)")
            (setq str "Warning, forward does not need -t switch")
            (tinyprocmail-fix-macro (concat str " Remove ")
                                    (ti::replace-match 1)))
          (when (looking-at ".*!.*\\(\\<-oi\\> *\\)")
            (setq str "\
info, ! -oi, may be unnecessary. It's default in New prcomail.")
            (tinyprocmail-fix-macro (concat str " Remove ")
                                    (ti::replace-match 1)))))
      ;; ............./........................................ check >> ...
      ;;  Narrowed to condition region
      ;;
      ;;      :0
      ;;      * condition         * condition
      ;;      # comment           | pipe actions. \\
      ;;      *                     more actions \\
      ;;      {                     end
      ;;
      (goto-char cond-end)
;;;;   (ti::d! (point) brace-p  (ti::read-current-line))
      (cond
       ((tinyprocmail-recipe-start-p)
        (forward-line -1))
       ((null brace-p)
        ;;   | recipe \\
        ;;     continues
        (tinyprocmail-skip-continuation-forward)
        (forward-line 1)))
      (skip-chars-backward " \t\n")     ;The end of narrow point
;;; (ti::d! "skip" (point))
      ;; ################################################## Narrow-begin ###
      (ti::narrow-safe bol (point)
        (when (setq formail-p (ti::re-search-check "formail"))
          (setq formail-d-p  (ti::re-search-check " -[^ ]*D "))) ;; perhaps -rD
        ;;  | (formail -rA "Header: val" \
        ;;     .. rest
        ;;    ) | $SENDMAIL
        (setq paren-p (ti::re-search-check "("))
        (setq sendmail-p  (ti::re-search-check "sendmail")
              ;;  Is there any condition lines?
              condition-p (ti::re-search-check "^[ \t]*\\*")
              size-test-p (ti::re-search-check "^[ \t]*\\*[ !$]*[<>]"))
;;;      (ti::d! cond-end "pm-narrow: formail, sendmail" formail-p condition-p)
        ;;  \> is procmail word boundary.
        (when (save-excursion
                (goto-char cond-end)
                ;;  If line continues:
                ;;
                ;;  | foo \
                ;;    this > mbox
                ;;
                ;;  Accept "out >>FILE"  and "OUT >>$FILE", but single
                ;;  token must have leading space
                ;;
                ;;  If the > token is directly in the cond-end line
                ;;
                ;;  | cat > junk
                (or (looking-at ".*>") ;; on the same line
                    (re-search-forward "[^\\]> " nil t) ;; maybe continued
                    (re-search-forward ">>" nil t)))
          (setq redirection-p (point))
;;;     (ti::d! (progn (goto-char redirection-p) (ti::read-current-line)))
          nil)
        ;; ................................................. empty-lines ...
        (ti::pmin)
        (while (and (not (eobp))
                    (re-search-forward "^[ \t]*$" nil t))
          (setq point nil)
          (tinyprocmail-log
           (point) "Error, no empty lines allowed inside condition block"
           point-min)
          (tinyprocmail-fix-macro
           "Error, empty line not allowed inside condition. Remove "
           (setq point t)
           (ti::buffer-kill-line))
          (if (null point)
              (forward-line 1)))
        ;; .................................................... comments ...
        (ti::pmin)
        (when (and pedantic
                   (setq point
                         (tinyprocmail-lint-condition-comments point-min)))
          (tinyprocmail-log
           point "Pedantic, Condition lines have embedded comments."
           point-min)))
      ;; #################################################### Narrow-end ###
      (goto-char opoint)
      ;; ............................................. no-condition-line ...
      (unless condition-p
        (setq fix nil  str ""  str2 flags)
        (dolist (ch '("H" "B" "D"))
          (when (ti::string-match-case ch str2)
            (setq fix t
                  str  (concat str ch)
                  str2 (ti::replace-match 0 nil str2))))
        (when fix
          (setq str
                (format "Warning, no condition line, but flags `%s' found."
                        str))
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro (concat str " Remove ")
                                  (setq fix-line t
                                        flags    str2))))
      ;; ......................................... nested {} block start ...
      ;; Most of the flags don't make sense in the outer block level
      ;;
      ;; H, B, A, a, E, e, and D affect the conditions and thus are
      ;; meaningful when the action is to open a brace.  H, B, and D would be
      ;; meaningless, of course, on any unconditional recipe, but they should
      ;; not cause error messages.
      (when brace-p
        (setq fix nil  str ""  str2 flags)
        ;; Exclude all. Base is  aAeEfhbHBDwWirc:
        (setq list '("f" "h" "b" "i" "r" "w" "W" ":"))
        ;;  if you are using c to launch a clone, then w, W, and a
        ;;  local lockfile can be meaningful.
        (setq
         list
         (cond
          ((string-match "c" flags)
           '("f" "h" "b" "i" "r"))
          (t
           '("f" "h" "b" "i" "r" "w" "W"))))
        (setq str2 flags)
        (dolist (ch list)
          (when (ti::string-match-case ch str2)
            (setq fix t
                  str  (concat str ch)
                  str2 (ti::replace-match 0 nil str2))))
        (when fix
          (setq str
                (format "Warning, start of {} block has unnecessary flags `%s'"
                        str))
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro (concat str " Remove ")
                                  (setq fix-line t
                                        flags    str2)))
        (when (and lock-p
                   (not (string-match "c" flags)))
          (setq str "Error, start of {} block has lockfile, but no `c' flag.")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro (concat "[cannot-fix]" str))))
      ;; ...................................................... size[<>] ...
      (when size-test-p
        (let (case-fold-search)
          (when (string-match "H" flags)
            (setq str "Error, size test doesn't use `H' flag. (use H ?? <)")
            (tinyprocmail-log size-test-p str)
            (tinyprocmail-fix-macro
		(concat str " Remove ")
	      (setq fix-line  t
		    flags     (replace-regexp-in-string "H" "" flags))))
          (when (string-match "B" flags)
            (setq str "Error, size test doesn't use `B' flag. (use B ?? <)")
            (tinyprocmail-log size-test-p str)
            (tinyprocmail-fix-macro
		(concat str " Remove ")
	      (setq fix-line t
		    flags (replace-regexp-in-string "B" "" flags))))))
      ;; .................................................... assignment ...
      ;; VAR=| cat something
      (when assignment-p
        (when (and (string-match "c" flags)
                   (null dc))
          (setq str "Warning, flag `c' is useless in assignment =|")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string "c" "" flags))))
        (when (and (string-match "i" flags)
                   (null di))
          (setq str "Warning, flag `i' is not recommended in assignment =|")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string "i" "" flags))))
        (when lock-p
          (setq str "Warning, lockfile \":\" is useless in assignment =|")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string ":" "" flags))))
        (save-excursion
          (goto-char assignment-p)
          (when (looking-at ".*`")
            (setq str "Error, backquotes mess things up in assignment =|")
            (tinyprocmail-log (point) str)
            (tinyprocmail-fix-macro (concat "[cannot-fix] " str )))))
      ;; ..................................................... formail-D ...
      (when (and formail-p
                 formail-d-p
                 (null assignment-p)
                 (null brace-p))
        (when (string-match "f" flags)
          (tinyprocmail-log
           opoint "Error, formail -D used. Flag `f' is a mistake.")
          (tinyprocmail-fix-macro
	      "Formail -D used, remove `f' flag? "
	    (setq fix-line t
		  flags (replace-regexp-in-string "f" "" flags))))
        (when (not (string-match "W" flags))
          (tinyprocmail-log
           opoint "Warning, formail -D used but no `W' flag found.")
          (tinyprocmail-fix-macro
	      "Formail -D found, add `W' flag? "
	    (setq fix-line t   flags (concat "W" flags))))
        (when (not (ti::string-match-case "h" flags))
          (tinyprocmail-log opoint "Error, formail -D used. No flag `h' found.")
          (tinyprocmail-fix-macro
	      "Formail -D found, Add `h' flag? "
	    (setq fix-line t  flags (concat "h" flags))))
        (when (ti::string-match-case "b" flags)
          (tinyprocmail-log
           opoint "Error, formail -D used. SHould not have `b'.")
          (tinyprocmail-fix-macro
	      "Formail -D found, remove `b' flag? "
	    (setq fix-line t  flags (ti::string-regexp-delete "b" flags))))
        (when  (not lock-p)
          (setq str "Warning, formail -D used but no lockfile.")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Add. ")
	    (setq fix-line t  flags (concat ":" flags)))))
      ;; ............................................ Check f and h or b ...
      (setq fix nil)
      (when (and (ti::string-match-case "f" flags)
                 (null (or (ti::string-match-case "b" flags)
                           (ti::string-match-case "h" flags))))
        (tinyprocmail-log
         (point)
         "Warning, `f', but no h;b;hb found. What's up here? (readability) ")
        (when (eq tinyprocmail--lint-fix-mode 'semi)
          (if (tinyprocmail-o
               (y-or-n-p "Flag `f' requires `h' or `b' (yes=h, no=b) "))
              (setq flags (concat "h" flags))
            (setq flags (concat "b" flags)))
          (setq fix t)))
      ;; ......................................................... :0 fc ...
      (when (and (ti::string-match-case "f" flags)
                 (ti::string-match-case "c" flags))
        (setq str "info, Redundant `c' in `f' recipe.")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro
	    (concat str " Correct ")
	  (setq fix t)
	  (setq flags (replace-regexp-in-string "c" "" flags))))
      (when fix (tinyprocmail-flag-kill flags))
      ;; ..................................................... "|" and w ...
      ;;  Every "|" action should have "w" flag
      (when (and pipe-p
                 (null brace-p)
                 (null assignment-p)
                 (null (string-match "w" flags))
                 (null dw)
                 (or (null pipe-w-re)
                     (save-excursion
                       (goto-char cond-end)
                       (not (looking-at pipe-w-re)))))
        (setq str "\
Warning, recipe with \"|\" may need `w' flag. (recommended) ")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro (concat str " Add ")
                                (setq fix-line t
                                      flags    (concat "w" flags))))
      ;; ................................................. check formail ...
      (when (and formail-p
                 (null formail-d-p)
                 (null paren-p) ;; If this, then "f" is not needed
                 (null assignment-p)
                 (null brace-p)
                 ;; Don't require "f" flag in this case
                 ;;
                 ;;  | $FORMAIL -A "header" >> mbox
                 ;;
                 (null redirection-p)
                 (not (string-match "f" flags)))
        (setq str "Warning, Formail used but no `f' flag found.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro
	    (concat str " Add ")
	  (setq fix-line t   flags (concat "f" flags))))
      ;; .............................................. f and no-formail ...
      ;;  If there was MH "rcvstore" Then "i" should not be there.
      (when (and (null redirection-p)
                 pipe-p
                 (null formail-p)
                 (null sendmail-p)
                 (not (ti::string-match-case "f" flags))
                 (not (ti::string-match-case "i" flags))
                 (null di))
        (setq str "Warning, recipe with \"|\", but no \">\" may need `i' flag.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro (concat str " Add ")
                                (setq fix-line t
                                      flags    (concat "i" flags))))
      ;;  i is meaningless if nested condition follows
      ;;  :0 i
      ;;  { }
      (when (and brace-p
                 (ti::string-match-case "i" flags)
                 (null di))
        (setq str "info, flag `i' is meaningless on top of nested block.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro
	    (concat str " Remove ")
	  (setq fix-line t
		flags (replace-regexp-in-string "i" "" flags))))
      (when (and (null redirection-p)
                 pipe-p
                 (not (ti::string-match-case "f" flags))
                 (not (ti::string-match-case "c" flags))
                 (ti::string-match-case "i" flags)
                 (null dc))
        (setq str
              "\
Warning, no \">\" in \"|\" recipe 'i' kills message. May need `c'.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro
	    (concat str " Add ")
	  (setq fix-line t   flags (concat "c" flags))))
      (goto-char opoint)
      ;; ............................................. check H without B ...
      (let (case-fold-search)
        (when (and (string-match "H" flags)
                   (null (ti::string-match-case "B" flags)))
          (setq str "info, flag `H' is useless, because it is default.")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string "H" "" flags)))))
      ;; ....................................................... check W ...
      ;;  :0 c: somefile is same as :0 Wc: somefile but ONLY on nesting
      ;;  block
      (when (and flags
                 brace-p lock-p
                 (ti::string-match-case "W" flags)
                 (ti::string-match-case "c" flags))
        (setq str
              "info, redundant `Wc:', `c:' already implies W in {} block.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro
	    (concat str " Correct")
	  (setq fix-line t
		flags (replace-regexp-in-string "W" "" flags))))
      ;; ................................................. need lockfile ...
      (when (and redirection-p
                 (not brace-p)
                 (not formail-p)
                 (not forward-p)
                 (not lock-p))
        (setq str "Warning, recipe seems to store to folder, may need lock.")
        (tinyprocmail-log opoint str)
        (tinyprocmail-fix-macro
	    (concat str " Add ")
	  (setq fix-line  t
		flags    (concat flags ":"))))
      ;;  Missing lockfile, but not if /dev/null
      ;;  :0      :0
      ;;  mbox    /dev/null
      (when (and mbox-p
                 (not dev-null-p)
                 (not redirection-p)
                 (not brace-p)
                 (not formail-p)
                 (not forward-p)
                 (not lock-p))
        (setq str "Warning, message dropped to folder, it may need a lock.")
        (tinyprocmail-log mbox-p str)
        (save-excursion
          (goto-char mbox-p)
          (tinyprocmail-fix-macro
	      (concat str " Add ")
	    (setq fix-line t
		  flags    (concat flags ":")))))
      ;; .......................................................... MBOX ...
      (when mbox-p
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  dev/null . .
        (when dev-null-p
          (when lock-p
            (setq str "Warning, /dev/null doesn't need lock")
            (tinyprocmail-log mbox-p str)
            (save-excursion
              (goto-char mbox-p)
              (tinyprocmail-fix-macro
		  (concat str " Remove ")
		(setq fix-line t
		      flags (replace-regexp-in-string ":" "" flags)))))
          (unless (ti::string-match-case "h" flags)
            (setq str "Info, /dev/null may be more efficient with `h' flag")
            (tinyprocmail-log mbox-p str)
            (save-excursion
              (goto-char mbox-p)
              (tinyprocmail-fix-macro (concat str " Add ")
                                      (setq fix-line t
                                            flags    (concat flags "h"))))))
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ... mbox name . .
        ;;  Check MBOX name
        (save-excursion
          (goto-char mbox-p)
          (when (looking-at ".*LOGFILE\\|MAILDIR\\|FORMAIL\\|SENDMAIL")
            (setq str (ti::read-current-line (point)))
            (setq str (format "Warning, suspicious mbox filename `%s'" str))
            (tinyprocmail-log mbox-p str)
            (tinyprocmail-fix-macro (concat "[cannot-fix] " str))))
        (when (and flags (string-match "i" flags))
          (setq str "Warning, flag `i' is dangerous when dropping to folder.")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string "i" "" flags))))
        (when (and flags (ti::string-match-case "hb\\|bh" flags))
          (setq str "\
Warning, flag combo `hb' is useless when dropping to folder.")
          (tinyprocmail-log opoint str)
          (tinyprocmail-fix-macro
	      (concat str " Remove ")
	    (setq fix-line t
		  flags (replace-regexp-in-string "hb\\|bh" "" flags)))))
      ;; .................................................... check lock ...
      (goto-char opoint)
      (beginning-of-line)
      (skip-chars-forward "[ \t]")
      (when (looking-at "[ \t]*:0[^:\n]*\\(:\\)\\( [a-z]\\)[a-z]?[ \t\n#]+")
        (setq str (save-match-data (ti::string-remove-whitespace
                                    (match-string 2))))
        ;; change :0 HB: c  to :0 HBc:
        (tinyprocmail-log
         (point)
         (format "Possibly Flag used as lock `%s'" (ti::read-current-line)))
        (cond
         ((tinyprocmail-flag-p str)
          (tinyprocmail-fix-macro
           (format "Only One char `%s' lockfile, Move to flag section? " str)
           (ti::replace-match 2)
           (setq flags    (concat str flags)
                 fix-line t)))
         (t
          (tinyprocmail-fix-macro
           (format "[cannot-fix] Odd one char `%s' lockfile, C-g to quit."
                   str)))))
      ;; ................................................. lockfile name ...
      (when (looking-at "[ \t]*:0[^#:\n]*\\(:\\)\\([ \t]*\\)\\([^ \t\n#]+\\)")
        (setq lock-file (match-string 3))
;;;      (ti::d! "LOCK-FILE" lock-file)
        (when (save-match-data
                ;;  \ = dos styled path
                ;;  / = unix styled path

                (and
                 ;; If lock file has variable expansion, then we won't
                 ;; check it.
                 (not (string-match "[$]" lock-file))
                 (not (tinyprocmail-string-valid-p lock-file 'path))))
          (tinyprocmail-log
           (point)
           (format "Unusual characters in lockfile name `%s'" lock-file))
          (tinyprocmail-fix-macro
           "[cannot-fix] Lockfile has unusual characters. C-g to quit"))
        ;;  :0: $FILE  --> can't know if it has .lock
        (cond
         ((and (not (save-match-data (string-match "\\." lock-file)))
               (not (string-match "[$]LOCKEXT" lock-file))
               (save-match-data (string-match "^[$]" lock-file)))
          (tinyprocmail-log
           (point)
           (format
            "info, could't check extention .lock in lockfile `%s'"
            lock-file)))
         ((save-match-data
            (and (not (string-match "\\." lock-file))
                 (not (string-match "[$]LOCKEXT" lock-file))))
          (setq str (format "no $LOCKEXT extension in lockfile `%s'" lock-file))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro
	      (concat str " Add ")
	    (goto-char (match-end 0))
	    (insert "$LOCKEXT")))
         ((save-match-data
            (and
             (when (string-match "\\(.*\\)\\." lock-file)
               (setq file (match-string 1 lock-file)))
             (not (string-match "\\.lock\\|\\.lck\\|[$]LOCKEXT" lock-file))))
          (tinyprocmail-log
           (point)
           (format
            "Non-standard lockfile extension. (use $LOCKEXT) `%s'" lock-file))
          (tinyprocmail-fix-macro
           (format "Non-standard lockfile extension. Change to $LOCKEXT ")
           (setq lock-file (concat file "$LOCKEXT"))
           (ti::replace-match 3 lock-file))))) ;; When end
      (goto-char opoint)
      (when fix-line (tinyprocmail-flag-kill flags)))))

;;}}}
;;{{{ lint: other

;;; ........................................................... &other ...

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-malformed-brace ()
  "Check braces."
  (while (re-search-forward "^[ \t]*{" nil t)
    (when (and (char-equal ?\{ (preceding-char)) ; {}  or {var
               (not (looking-at "[ \t\n]")))
      (tinyprocmail-log (point) "Error, no space after  `{' .")
      (tinyprocmail-fix-macro "No space after {  Add one?"
                              (insert " ")
                              (backward-char 1)))
    (when (and (looking-at ".*\\(}\\)")
               (save-match-data
                 (not (looking-at ".*[ \t\n]+\\(}\\)"))))
      (tinyprocmail-log (point) "Error, no space before `}' .")
      (tinyprocmail-fix-macro "No space before }  Add one?"
                              (ti::replace-match 1 " }")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-malformed-misc  ()
  "Check varaious other things."
  (let (;;; (pedantic (eq tinyprocmail--lint-log-verbose 'pedantic))
	(opoint  (point))
	(re "echo\\|cat\\|tail\\|head\\|sed\\|perl\\|awk\\|perl\\|[-]")
	str)
    ;;  Detect "dummy `echo`", missing "=" or ";"
    ;;
    ;;  But following is valid.
    ;;
    ;;          LOG = "text start
    ;;          and-newline "
    (while (re-search-forward
            "^[ \t]*\\([^!|#\n=;]+\\)\\([ \t]+\\)[\"`]" nil t)
      (setq str (match-string 0))
      (when (save-match-data
              (and
               (not (string-match re (match-string 1)))
               (not (string-match "\"[ \t]*$\\|\"[ \t]*#"  str))
               (not (tinyprocmail-condition-line-p))))
        (setq
         str
         (format "Warning, After `%s', there is no \"=\" or \";\""
                 (match-string 1)))
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat str " Add = ")
                                (ti::replace-match 2 " = "))))
    (goto-char opoint)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-malformed-var-defs ()
  "Check variable definitions and assignments."
  (let (
;;;      (pedantic (eq tinyprocmail--lint-log-verbose 'pedantic))
;;;      cont-p
	var1
	var2
	str
	op)
    (while (re-search-forward "^[ \t]*[^#\n].*=" nil t)
      (beginning-of-line)
      (unless (tinyprocmail-comment-line-pp)
        ;;  Backslash at the end (continuation)
        ;;
;;;      (setq cont-p (looking-at ".*[\\][ \t]*$"))
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. date ...
        ;; It's slow to call `date' and there is already 10x faster
        ;; ways to derive the message date: See pm-jadate.rc
        ;;
        ;; MONTHNAME = `date +%y-%m`
        (when (looking-at ".*=`.*date")
          (setq str
                (concat
                 "Info, calling `date' is 10x slower"
                 " than reading From_ hdr."))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro str))
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  perl-var . .
        ;; $perl-styled-var = "value"
        (when (looking-at "^[ \t]*\\([$]\\).*=")
          (tinyprocmail-log (point) "Error, perl styled variable assignment.")
          (tinyprocmail-fix-macro "Remove extra Perl styled assignment to `$' "
                                  (ti::replace-match 1)))
        ;; ... ... ... ... ... ... ... ... ... ... ...  illegal-var-name . .
        ;; VAR-1 = 1
        (when (and (looking-at "^[ \t]*\\([^ \t\n]+\\)[ \t]*=")
                   (setq str (match-string 1))

                   ;;  Must start with alpha otherwise reject
                   (string-match "^[A-Z]" str)
                   (not (tinyprocmail-comment-line-p))
                   (not (tinyprocmail-string-valid-p str)))
          ;; This will unfortunately misclassify line,
          ;; which includes '=', like
          ;;
          ;;     SPAM_REGEXP_FILE   ="\
          ;;     filename.*=.*\.(pif|rar|zoo|arj|exe|bat)"
          (setq str
                (format
                 "Warning, odd characters in variable `%s'." str))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro str))
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . literal . .
        ;;   VAR = abc
        ;;   VER = 1.4a
        (when (looking-at "^[^=\n]+=[ \t]*\\([a-z]\\|[0-9]+[-.a-z]\\)")
          (tinyprocmail-log
           (point)
           "Warning, no right hand variable found. ([$\"`'] .. missing)")
          (tinyprocmail-fix-macro "Add missing `$' to right hand variable? "
                                  (ti::replace-match 1 (concat "$" (match-string 1)))  ))
        ;; ............................................. odd name(right) ...
        ;; var = $odd%&_name    nok
        ;; var = var-var                ok
        ;; var = $var-$var      ok
        (when (looking-at
               "^[ \t]*[a-z].*=[ \t]*[$]\\([a-z]+[^-/_a-z0-9$ \t\n#]+\\)")
          (setq str
                (format
                 "Error, Odd variable name to the right `%s'."
                 (match-string 1)))
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro str))
        ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  tilde(~) . .
        (when (looking-at "^.*=.*\\(~\\)/")
          (tinyprocmail-log
	   (point)
	   "Error, csh's tilde(~) is not supported, Use $HOME.")
          (tinyprocmail-fix-macro "Non-supported: Substitute ~ with $HOME "
                                  (ti::replace-match 1 "$HOME")))
        ;; ... ... ... ... ... ... ... ... ... ... ...  mismatch(` -- ') . .
        (when (or (looking-at "^.*=[ \t]*`.*\\('\\)[ \t]*#.*$") ;; comment
                  ;; no comment
                  (looking-at "^.*=[ \t]*`.*\\('\\)[ \t]*$"))
          (setq
           str
           "Error, assignent, starting backtick, but ends to single quote.")
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro (concat str " Kill ' ")
                                  (ti::replace-match 1 "`")))
        ;; ...................................................... var-init ...
        ;; D = ${D: ...
        ;;
        ;; Note that the colon in this case is ok.
        ;; D = {D}:/directory/file.txt
        (when (looking-at
               (concat
                "[ \t]*\\([^ \t{]+\\)[ \t]*"
                "=.*$\\({\\)[ \t]*\\([^ \t}:]*\\)\\([ \t]*\\):\\(.\\)"))
          (setq var1 (match-string 1)
                var2 (match-string 3)
                str  (match-string 4)
                op   (match-string 5))
          (when (save-match-data (not (string= "" str)))
            (tinyprocmail-log (point) "Error, space before init operator.")
            (tinyprocmail-fix-macro "Kill Space before init operator."
                                    (ti::replace-match 4)))
          (when (save-match-data (ti::nil-p var2))
            (tinyprocmail-log (point)
                              "Error, no right hand init variable found.")
            (tinyprocmail-fix-macro
             (format "Error, no right hand `%s'. Add " var1 )
             (goto-char (1+ (match-beginning 2)))
             (insert var1)
             (setq var2 var1)))
          ;;  Writing VAR = ${$VAR is a mistake. Notice 2nd $
          (when (save-match-data (and var2 (string-match "^[$]" var2)))
            (tinyprocmail-log
             (point)
             (format
              "Error, in init sequence, `%s' has extra $ ." var2))
            (tinyprocmail-fix-macro
             (format "`%s' contains extra $. Correct " var2)
             (save-match-data (setq
                               var2
                               (replace-regexp-in-string "^[$]" "" var2)))
             (ti::replace-match 3 var2)))
          (when (save-match-data
                  (or (and var1 (not (tinyprocmail-string-valid-p var1)))
                      (and var2 (not (tinyprocmail-string-valid-p var2)))))
            (setq
             str
             (format
              "Warning, in init sequence `%s' or `%s' has illegal characters."
              (or var1 "<?>") (or var2 "<?>")))
            (tinyprocmail-log (point)  str)
            (tinyprocmail-fix-macro str))
          ;;  1998-04 I used to believe this calls shell, but it doesn't.
          ;;  D = ${D:-`date`} So the following recipe is *commented out*
          (when nil
            (when (and (save-match-data (looking-at ".*[-+][ \t]*`"))
                       pedantic)
              (tinyprocmail-log
               (point)
               (concat
                "Pedantic, `` is not a recommended initialize "
                "practise (uses shell)."))))
          (when (save-match-data (not (string= var1 var2)))
            (tinyprocmail-log
	     (point)
	     "Warning, variables don't match in init sequence.")
            (tinyprocmail-fix-macro
             "[cannot-fix] Left var1 and right var2 don't match."))
          (when (save-match-data (not (string-match "[-+]" op)))
            (tinyprocmail-log
             (point)
             (format "Error, invalid init operator `%s'. Not [-+] " op))
            (tinyprocmail-fix-macro
             (format "[cannot-fix] Invalid init operator `%s' "
                     op  )))))
      (forward-line 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-malformed-start-recipe ()
  "Check ': ' or '0:' recipes."
  (let ((space  (if (eq 'flags-together
			tinyprocmail--flag-and-recipe-start-style)
		    "" " "))
	list
	str)
    (while (setq list (ti::re-search-point-list '("^[ \t]*0:" "^[ \t]*:[^0]")
                                                'beginning-of-line))
      (goto-char (apply 'min list))
      (cond
       ((looking-at "^[ \t]*\\(0:\\)")
        (tinyprocmail-log (point)
                          "Error, recipe start is invalid, should be `:0'.")
        (tinyprocmail-fix-macro "recipe error, fix to `:0' "
                                (ti::replace-match 1 ":0")))
       (t
        ;;  The :[^0]  matches \n, and that's why we use looking-at.
        (if (looking-at "[ \t]*:")
            (goto-char (match-end 0))
          (backward-char 1))            ;There was newline
        (cond
         ((looking-at "\\([ \t]*[1-9]\\|[ \t]+0\\)") ; ': 0' or
          (setq str "Warning, Suspicious recipe start, use standard `:0'.")
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro (concat str " Correct ")
                                  (ti::replace-match 1 "0")))
         (t                             ; ':'
          (setq str "Warning, Suspicious recipe start, use standard `:0'.")
          (tinyprocmail-log (point) str)
          (tinyprocmail-fix-macro str
                                  (delete-horizontal-space)
                                  (insert "0" space)
                                  (if (not (looking-at " "))
                                      (insert " ")))))))
      (end-of-line))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-condition-line-1 ()
  "Check one condition line. Point must be over start(*)."
  (let ((point         (point))
	(var-test-p    (looking-at ".*[?][?]"))
	(shell-test-p  (looking-at "\\*[! \t]+[?]"))
	tmp
	str
	match-p
	end)
    (catch 'done
      ;; ....................................................... empty ...
      (when (looking-at "\\*[ \t]*$")
        (setq str "Error, Nothing in condition line.")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat "[Cannot-fix] " str))
        (throw 'done 'nothing))
      ;; ............................................ missing caret(^) ...
      ;; * ! Headr-field:
      ;; * ! FROM_DAEMON
;;;      (ti::d! (buffer-substring (point) (line-end-position)))
      (when (or (looking-at "\\*[ \t!]*\\([-A-Z]+\\):")
                (let (case-fold-search) ;be case sensitive
                  (looking-at
                   (concat
                    "\\*[ \t!]*\\(FROM_DAEMON\\|FROM_MAILER\\|TO_?\\)"
                    "[ \t]*$"))))
        (setq
         str
         (format
          "Warning, `%s' does not have (^) in condition line."
          (match-string 1)))
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat  str " Add ")
                                (goto-char (match-beginning 1))
                                (insert "^")))
      ;; .......................................................... TO ...
      ;; * ^TOregexp  is a mistake, should use ^TOregexp\> or something
      ;; Skip ^TO$REGEXP
      (when (let (case-fold-search)
              (and (looking-at ".*\\(TO[^([\n]+\\)")
                   (save-match-data
                     (and (not (looking-at ".*[\\]>[ \t]*$"))
                          (not (looking-at ".*[$]"))

                          ;;  Next line must not a condition
                          ;;  line
                          ;;
                          ;;  * ^TOadmin
                          ;;  * more-restrictive-condition
                          ;;
                          (not (save-excursion
                                 (forward-line 1)
                                 (looking-at "[ \t]*\\*")))))
                   ;;  Try to find similar contruct from buffer.
                   ;;  If found then user needs swap the order of
                   ;;  these two.
                   ;;
                   ;;  1) TOaddr  and later) TOadd-another
                   ;;
                   (save-excursion
                     (end-of-line)
                     (re-search-forward
                      (regexp-quote (match-string 1)) nil t))))
        (setq
         str
         (format
          "Warning, `%s' is not unique, another similar TO found."
          (match-string 1)))
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat "[cannot-fix]" str)))
      ;; ....................................................... extra ...
      ;;   * regexp|regexp\
      ;;   * |regexp|regexp  << Ooops, extra (*)
      (when (and (looking-at "\\*[ \t]*|")
                 (save-excursion        ;Peek previous
                   (forward-line -1)
                   (looking-at "[\\][ \t]*$")))
        (setq str "Warning, extra \"*\" before fist regexp \"|\".")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat str " Remove `*' ")
                                (delete-char 1)
                                (throw 'done 'extra))) ;Nothing more to check
      ;; ........................................... expansion missing ...
      ;;  Very common mistake, you forgot the beginning $
      ;;
      ;;  * ! $REGEXP
      ;;
      ;;  * $$REGEXP  is ok
      ;;  * $!$REGEXP is ok
      ;;  * $(^TO$REGEXP) is ok
      ;;  * $-${VAR}^0  is ok
      (when (and (not (looking-at "\\*[ \t]*[$][-$!^(]"))
                 (looking-at "\\*[ \t!]*[$]\\([^ \t\n]+\\)[^$\n]*$")
                 (not var-test-p))
        (setq
         str
         (format "Error, No eval($) in condition found. (%s)"
                 (match-string 1)))
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat str " Add `$' ")
                                (save-excursion
                                  (forward-char 1)
                                  (insert "$" ))))
      ;;  A bit more complex
      ;;  * !^(To|Cc|Bcc):.*$LOGNAME
      ;;
      ;;  But this is ok
      ;;
      ;;  * ? echo "$ARG"
      ;;
      ;;  And $HOME variable is accepted without eval
      (when (and (not var-test-p)
                 (not shell-test-p)
                 (looking-at
                  "\\*[^$\n]+\\(.\\)[$]\\([A-Z][^ \t\n]+\\)+[^$\n]$")
                 (save-match-data
                   (not (string-match "[\"]" (match-string 1))))
                 (save-match-data
                   (not (string-match "[$]HOME" (match-string 2)))))
        (setq
         str
         (format "Error, No eval($) in complex condition found. (%s)"
                 (match-string 2)))
        (tinyprocmail-log point str)
        (tinyprocmail-fix-macro (concat str " Add `$' ")
                                (save-excursion
                                  (forward-char 1)
                                  (insert "$" ))))
      ;; ............................................. missing eval($) ...
      ;;  User forgot the interpolation in the line
      ;;
      ;;  * ! VAR ?? $eval-this-var
      ;;
      ;;  --> * ! VAR ?? $ $eval-this-var
      ;;
      ;;  It's impossible to catch this however
      ;;
      ;;  * ! VAR ?? $eval-this $eval-second
      ;;
      ;;
      ;;  But these are ok
      ;;
      ;;  * FROM??^foo@bar$
      ;;  * TO??^$
      (when (and (looking-at "\\*\\(.*\\)[?]\\([?]\\)[^$\n]+[$][^$]+$")
                 (save-match-data ;; has $
                   (not (string-match "[$]" (match-string 1))))
                 (save-match-data ;; but must not ne at the end
                   (not (string-match "[$]$" (match-string 1))))
                 (save-match-data ;; literal newline in prenthesis ($)
                   (not (string-match "($)" (match-string 1)))))
        (setq tmp (match-beginning 2))
        (setq str "Warning, missing eval($) operator in variable context.")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat str " Add ")
                                (goto-char (1+ tmp))
                                (insert " $ ")
                                (goto-char point)))
      ;; Warn about:      *$  $VAR ?? test
      ;; Not Warn about:  *$  $VAR ?? $test
      ;;
      ;; But this is ok:  *$  $SUPREME^0 ^From:(some match)
      (when (and (not (looking-at "^.+[^][0-9] "))
                 (looking-at "^.+\\([$]\\).*[$].*[?][?]"))
        (setq str "Warning, Possible $ misuse (doubled) to the left of ??")
        (tinyprocmail-log (point) str)
        (tinyprocmail-fix-macro (concat "cannot-fix]" str)))
      ;;  * VAR ?? test is legal in procmail, don't require
      ;;  *$ VAR ?? test
      ;;
;;;       (when (and (looking-at "[^$\n]+[?][?]")           ;; * VAR ?? test
;;;               (not (looking-at ".*[HB][! \t]+[?]"))  ;; * HB  ?? regexp
;;;               )
;;;      (tinyprocmail-log
;;;       (point)
;;;       "Possibly missing left hand $ in ?? variable test.")
;;;      (ti::d! (ti::read-current-line))
;;;      (tinyprocmail-fix-macro "Maybe missing left hand $. C-g to quit."
;;;        (forward-char 1)
;;;        (insert "$")
;;;        ))
      ;; .................................................... empty ?? ...
      (when (looking-at ".*[?][?][ \t\n]*$")
        (tinyprocmail-log (point) "Error, Nothing follows ?? ")
        ;; Can't autofix this one.
        ;;
        (tinyprocmail-fix-macro
         "[cannot-fix] Error, Nothing follows ??, C-g to quit. "))
      ;; ............................................... suspicious ?? ...
      ;;  VAR ?? .  is not right
      (when (looking-at ".*[?][?][ \t]*\\(\\.\\)[ \t]*$")
        (tinyprocmail-log
         (point)
         "Warning, '?? .' will fail on null variable. Prefer '(.|$)'")
        (tinyprocmail-fix-macro
         "'?? .' is not preferred, use '(.|$)' instead "
         (ti::replace-match 1 "(.|$)")))
      ;; ............................................... trailing star ...
      ;;   * regexp.*    but  * \/regexp.*  is okay.
      (save-excursion
        ;;   Skip continuation
        (while (and (not (eobp))
                    (looking-at  ".*\\[ \t]*"))
          (forward-line 1))
        (save-excursion
          (setq end (line-end-position))
          (goto-char point)
          (if (re-search-forward "\/" end t)
              (setq match-p t)))
        (when (and (null match-p) (looking-at "^[^ \t\]+\\*[ \t]*$"))
          (tinyprocmail-log
           (point)
           "info, maybe useless regexp `*' at the end condition.")
          (tinyprocmail-fix-macro "[cannot-fix] Useless `*' at the end."))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-condition-lines (&rest args)
  "Check all condition lines. Ignore ARGS. Point must be inside condition."
  (while (and (not (eobp))  (tinyprocmail-move-to-next-condition-line))
    (tinyprocmail-lint-condition-line-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-standardize-recipe-start ()
  "Check whole buffer and change recipe start to ':0 FLAGS'.
Refrences:
 `tinyprocmail--flag-and-recipe-start-style'"
  (interactive)
  (let ((style tinyprocmail--flag-and-recipe-start-style)
	(i     0)
	(found 0))
    (save-excursion
      (ti::pmin)
      (while (tinyprocmail-forward)
        (incf  found)
        ;;   We're sitting on :  go to 2 char forward
        (forward-char 2)
        (cond
         ((eq style 'flags-together)
          (when (looking-at "\\([ \t]\\)[^#]")
            (ti::replace-match 1)
            (incf  i)))
         (t
          (unless (looking-at "[ \t\n]")
            (insert " ")
            (incf  i))))
        (end-of-line)))

    (unless (zerop i)
      (message
       "TinyProcmail: standardized %d recipe start line(s). Style: %s"
       i
       (if style
           (symbol-name style)
         "Flags separated from :0")))

    (when (zerop found)
      (message "TinyProcmail: Can't find any recipes from buffer."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-list-lint-directives  ()
  "Find all recipes that have Lint directives.
This function puts the results to `tinyprocmail--lint-output-buffer'.
Function activates only of `tinyprocmail--lint-log-verbose' is 'pedantic."
  (let (options
	flags
	point)
    (when (eq tinyprocmail--lint-log-verbose 'pedantic)
      (while (re-search-forward "^[ \t]*#[ \t]*Lint:[ \t]*\\(.*\\)" nil t)
        (setq options (match-string 1)  point (match-beginning 1))
        (forward-line 1)
        (setq flags (tinyprocmail-flag-read (ti::read-current-line)))
        (tinyprocmail-log
         point
         (format "info, Lint options `%s'. recipe flags `%s'."
                 options flags))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-find-wrong-escape-codes ()
  "Find misused \\t and \\n characters."
  (let (str
	val)
    ;; (re-search-forward "\\[[^]\n\\]+\\([\\]t\\)" nil t)
    (while (re-search-forward "\\([\\][tn]\\)" nil t)
      (unless (save-match-data (tinyprocmail-comment-line-p))
        (setq val (match-string 1))
        (setq
         str
         (format "Error, escape code `%s' is not known to procmail." val))
        (tinyprocmail-log (point) str)
        (cond
         ((save-match-data (string-match "t" val))
          (tinyprocmail-fix-macro (concat str " Correct ")
                                  (ti::replace-match 1 "\t")))
         (t
          (tinyprocmail-fix-macro (concat "[cannot-fix] " str))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-find-2spaces ()
  "Find misused [  ] contructs. User meant space and TAB."
  (let (str
	set
	done
	not-comment
	not-backtics)
    (while (re-search-forward "\\[\\([^]\n]+\\)\\]" nil t)
      (setq set          (match-string 1)
            not-comment  (save-match-data
                           (not (tinyprocmail-comment-line-p)))
            not-backtics (save-excursion
                           (beginning-of-line)
                           (save-match-data (not (looking-at ".*`")))))
      ;;  Skip lines like:  dummy = `echo "[this value here]" > file`
      (when (and not-comment not-backtics)
        (save-match-data
          (when (string-match " \\( +\\)" set)
            (setq
             str
             "Warning, two spaces inside regexp [], maybe you mean TAB.")
            (tinyprocmail-log (point) str)
            (tinyprocmail-fix-macro "Warning, two spaces, change to TAB "
                                    (setq done t)
                                    (setq set (ti::replace-match 1 "\t" set)))))
        (when done
          (ti::replace-match 1 set))
        (when (and (or (tinyprocmail-condition-line-p)
                       (tinyprocmail-assignment-line-p))
                   ;; like " asd "
                   (string-match " [^ ]+ " set))
          (setq str "Warning, two spaces inside regexp [].")
          (tinyprocmail-fix-macro (concat "[cannot-fix] " str)))))))

;;}}}
;;{{{ Lint: main

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-output-display ()
  "Show `tinyprocmail--lint-output-buffer' buffer."
  (interactive)
  (cond
   ((null (get-buffer tinyprocmail--lint-output-buffer))
    (error "No `tinyprocmail--lint-output-buffer'"))
   (t
    (ti::save-excursion-macro
      (display-buffer tinyprocmail--lint-output-buffer)
      (select-window (get-buffer-window tinyprocmail--lint-output-buffer))
      (ti::pmax)
      (re-search-backward "^\\*\\*" nil t) ;; start of lint section
      (unless (eq major-mode 'tinyprocmail-output-mode)
        (turn-on-tinyprocmail-output-mode))
      (font-lock-mode-maybe 1)
      (ti::string-syntax-kill-double-quote)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-forward (&optional mode verb)
  "Lint the code forward.
Input:
  MODE  If nil, step through recipes and correct them interactively.
        If non-nil then Write log _with_ pedantic messages.
        If 2 x \\[universal-argument] then Write log without pedantic.
  VERB  Verbose messages."
  (interactive "P")
  (let* ((check-list			tinyprocmail--lint-do-hook)
         (tinyprocmail--lint-fix-mode   tinyprocmail--lint-fix-mode)
         (tinyprocmail--lint-log        tinyprocmail--lint-log)
         (tinyprocmail--lint-log-verbose tinyprocmail--lint-log-verbose)
         (opoint (point))
         (time   (current-time))
         (count  0)
         point
         secs
         ret)
    (ti::verb)
    (when mode
      (setq tinyprocmail--lint-fix-mode nil)
      (if (equal mode '(16))
          ;; suppess 'pedantic
          (setq tinyprocmail--lint-log-verbose nil)))
    (setq tinyprocmail--lint-log (if mode t nil))
    (tinyprocmail-log-start)
    (beginning-of-line)
    (run-hooks 'tinyprocmail--lint-before-hook)
    (goto-char opoint)
    (while (tinyprocmail-forward)
      (setq point (point))
      (incf  count)
      (when (and verb mode)
        (message "TinyProcmail: Linting %d %s" count (ti::read-current-line)))
      ;; There is no point of checking the recipe if the Flags are
      ;; not all right. The functions usually modify flags,
      ;; and that is imposible without proper flags.
      (setq ret (tinyprocmail-flag-order-lint))
      (when ret
        (dolist (func check-list)
          (goto-char point)
          (funcall func (car ret) (cdr ret) )))
      (goto-char point)
      (forward-line 1))
    (dolist (func tinyprocmail--lint-after-hook)
      (goto-char opoint)
      (funcall func))
    (tinyprocmail-overlay-hide)
    ;;  Sort the output buffer. The results are in random order, because
    ;;  many different list function have been run one after another.
    (when tinyprocmail--lint-log
      (tinyprocmail-output-macro (tinyprocmail-output-sort-by-line)))
    (setq secs (ti::date-time-difference (current-time) time)
          time (/ secs 60))
    (message "TinyProcmail: Lint done. recipe Count = %d time: %02d:%02d "
             count time (- secs (* time 60)))
    (when mode (tinyprocmail-output-display))
    (goto-char opoint)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-buffer (&optional mode verb)
  "Lint whole buffer. See MODE and VERB from `tinyprocmail-lint-forward'."
  (interactive "P")
  (ti::verb)
  (save-excursion
    (ti::pmin)
    (tinyprocmail-lint-forward mode verb)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyprocmail-lint-buffer-batch (&optional clear file)
  "Batch Lint buffer and write results to `tinyprocmail--lint-output-file'.
Input:

  CLEAR  If non-nil, clear output buffer
  FILE   Save results to this file"
  (interactive "P")
  (if clear
      (tinyprocmail-output-clear))
  ;;  Delete empty lines from the beginning to get line numbers
  ;;  sensible
  (ti::pmin) (skip-chars-forward " \t\n")
  (unless (eq (point) (point-min))
    (delete-region (point-min) (point)))
  (tinyprocmail-lint-buffer '(4))
  (tinyprocmail-output-file-save file))

;;}}}
;;{{{ Examples or other files
;;; ........................................................ &Examples ...
;; Here is example file for the Lint. It also demonstrated what kind of
;; errors can be trapped.

;; file pkg.tar:
;; r--r--r-- 240/222  14764 Jun 22 15:05 1998 pm-lint.rc 1.26
;;
;; -----BEGIN PGP MESSAGE-----
;; Version: 2.6.3ia
;; Comment: Base64 signed. File: pkg.tar uncompresses to approx. 20K
;;
;; owHtWUtvW0ty9kUwCMQs80A2AVo0rVdEWpIfuSOP7UiyfKXElgxbvp47lmw1z2mS
;; PToP5jxE8V57l0UGyCIIMFkEmMkmCDCL/IGs8sKsB5gECJAJ5gcMsggQZBckVdV9
;; +jx4DmmS9y6CXMKwyHO6v66urq76qup7117+0jeu3fn5f/zR3/7Nv//zJ7/2W3c+
;; +ujjP/7ofz57/6c/XfzJwQ/j/nf+5Wc//NEPfvar3/vPv2/+99/96Kd/de/7//hP
;; P/mNv/zB/V/84Y+b//oPv/zxX9z4vePv//zln/3Xj/+t+4s/v/Gbv358/Cd//exX
;; 2t8II1t61+DTd5uO9KJWYF37yj4b8Ll9+zb9vXV3Q/29dZf+wrfbd+/AO/jvFgza
;; un0Lnm/CsN+5tvHViZR+4jDiASz5Xc6dyK8e57uWy73q92ozG+bv/5HPdfa7K9dX
;; WWoGrNlkL7jbdwTbd7kVshPpDZ+5LeGwlWeBD0qQzirDwSwSYcQ60hE1jdI4tLcz
;; UOuXbLO1dZdtfvObH9/cuHtza4ttbm1v3Nm+c4cpdbP9qz5r1K4DAGOPRGgFsh9J
;; 38MnCyc9GTL4Z5ZhHT/QQvW5dcG7IhWuxZ743GYRzIGpNBqE8Jmg4dyzmcUdhz1t
;; XrFIgoiubwt6/IR20hPM8r1IeBFOH8iox/aaMQ0/p/G4pWY77nREsNxiO04kAo9H
;; 8lI4w3U29GOAB6kXQiFYzx8QIOkojLtdkF+J4PILEcJCQSAs3GbIcCWYVr6O1ste
;; j3uw1Sd+l614vidWazV6nujn6PiE9fXJKD2RPH7s2CyIvRYMRNU5jj+QXpeBvUfC
;; hZ0SSFvAY3YhHSckoUEy2Qfdua6wJQx0hkx2CDAKhgwU2hEiUTNMhwfJ0tuMHUbL
;; NrNjt497BKuoHRy/OGH32VtxJaO3Gcy3uIPWbB+2dMkDydsOqBJ+1mogBvt05/kC
;; LFT3L+oLC/jgOvMv1tkfxD5YT3bEZva1F7ttEWRfwyG4YjkzpA2GFknrIqyphRow
;; MAuzHwR+sM6eicABzQ4dUA4PQ9n1SMMZZEeCyXBHTxNqmofKVEIysO3GHGpRemlK
;; T0Yjaml8AV+2m6e1Ohq59GJh19+nilBjnxx/ghqMxFXEsh8aAdbb9MQArFPUpxzO
;; Cp/rrO/EeAtcVBHcUrImvC2iVSsT+9wGmzl/T1Nf8cADI15nYU/AdY5DYbfYyxCu
;; cnoj2YAPi/tvENLmeyMDnBudwP5VFHDWQDG24IYmpsU87ooRWRhC5A8+BE8k4BaB
;; a0KPA8r3+3DOkR+M7oSNzJbeJXekbeaw7TqrlyxbPTOzJPrNkqm//YFT83O3m7ww
;; zfNRO1sjS3x7dCiM2ySHhxOY7XvLEfi+yOqVHO8irxAvvUbrLPYuPH/gpXpa1Eg3
;; 8rcR3Jjocqf8GJubk4fSWPBg7hCdgbB6/vKIFmgn6EnPM6Np7Hl+rCthC1535f6q
;; tmvwmHsclWGLCCIAOlww3Q+94AXhTpOd6DtNp8i4CmYClO936FLV1C6S0QUB4OaR
;; L2qxo9zOMovdbxwcP92/SeEle8lz4jQ4X7qRu+WD5Koe2xB8ezzgFlzREG9aovM8
;; wv2in0D/+2Lv+Pk+THBiUZvHOyZO8v475R31eewd7O/9/nbqVjoO7zILoyq4Fogw
;; KnZsbzArsSIAsLimPWNRJKJ4foQhlXydLewETU6D5vjWhSI0I1JtfwCONkYTyJgL
;; AHjyXhfw+iVQ5wnU+VwaZ0uwc4FswhivESp5g+Q7UhZIAzaUGG7bv5o8A24YsHfU
;; CWxA7SMze5bpO9UAWo1wS5TL5x2wZrh7Dl465FbJ6eb3UAriAzMjspAXq8WepgKB
;; 6QzUZLq4n4lwXV3XARG7NvBL8l9wnRIGBocco6MFZpt4N3iLBFdJtlWp3Aly6Y1t
;; sZx+ZjUNxon8ItUtM41iGKDB5MnYivaoq9po6c8aJQgfDEIu/N1iA/nWTvM7JUj4
;; tb7YqM9p/MAIwAXa1TsE30DOVlHzkC2yZIotLIdnIvqofIvlkGSOyutom2xVAUB0
;; ugBlfNevdjyx54EZhCEH5t+MKoGaEWF9GArRgqYvx6HBa42Yh4SYUzWrM7A38b+t
;; eYMEW2oHeL2Lp8bU4yxvJz+QleiL97Xp59j+bLPYLNOYWm3MRHKJ4FwivyuASASU
;; oRr/uFHTOO/nwVCnB7TFlinzVJjzBXi2hJ6p+sa5sQNJNrg3HCYompqQnHq2Us+d
;; Jh5x2JeW9CGFocGGYRqMBqRGjw+f7BchTD3D9zH7ATmuMH9WVKylChEDcNysi0lM
;; hzuhSEiUBnohMNEJfQoEarHrCbEEOv109xjIeOPo5ZMnb/H7+1pBMD3ipi0ub3qx
;; 47xPJDS7M6/AUkSITJEsKaEgBUAzOsE59Dp+FsTlQzQEF5Mj0emA3gRtEszhvLdM
;; PKmWtYoM4NymEMrPRRnXo+dUU0q2iAH3/ECJ02Ir+POAPXzIvpXEGXagbfZbbHNj
;; YyO11w8B3s0B7+aBdyuA5+VdXXHVD0suAlU9MDqAcxCWBIYw8IPCHVhjncB339pc
;; uDr+jL0QjuA2hjGLByJaebOapD1F0EX2+Pnx07ePdvafHh9VEZEZUA9gpAiaHSkc
;; u5LgSDJMzZ6Z0s/K2mohXTIuSSVOhaXUrLWqJVC16O2Ip5/eVMlUvhqQATu9qeBa
;; WbzpDz056yb5+mbE2yWHbpRqxIsGmsYCnewJT78Iewm1DHnfUuVRQDQehj6JF/M9
;; ZwhpnbAuBJYSJujuNWNnJlyPNScj2mTQ05qWqQR9AniYR18xdTFaaHXMSuGEjRzB
;; 7UrVEkpbJKUuPc4IW5j4aVKI4KEMM/VDo3m+CkoXqSbMi/YqS+UHYwjAxhPqaAZZ
;; qyr8EZEuDNqB9BvFqplfesOlD+GTflMjZs8FEvs1llsauJPGA3lWcmBYD2Kn0aid
;; nUZna6HvCuLWa/h7Z3fvbELiU4mPWcKpUtKrw5P9F8929vax7AmvTr36dGBeibBe
;; QVhvnLDGjPN2TDam1Mhen7VGV6G0MbtQpfGiD5NdD8P16zNlxlj+T2sHHT9GVxng
;; U7pBqWXnqmasDoZB65ITIQHq5+YGJMVf6svo72e1+arfpkwy6gB17EhpjFJRlZ4z
;; FK8wA5Jgq2qWuIqEh1fKLIdhtATB/CqLUzpvT8FAy2ZnK2hMkJJ7NmSJq3nEVuT2
;; ESjxz2ZS9oymXVEvsd1APosrXJ+AllZJUjxOyWgZLLHkCiRFJamLtaw9qsIkh91C
;; qEpJK+072axRTo69kx5vLN0k8CzGjGaJzs2Wutc3pggGtAQ5iRm6nkRiJIyJqLlE
;; /h2jS0Zk8wFLmfmXDV2GvTAD9vWFhd040ikOtQM6MvELGNPawuI4C7nYA7TeMIr1
;; EcDUfB5JqNTTfICOCGSkfLPHIWkKY7A/8FAmZVpYUG6yeotTbLDFXpHsbaH8YBXo
;; lJiY8k2EHHcQPR6y+oO6Kv+ZonNmCdJ7eCH7VPcNS2pCpw/moqHa3oXbj4ZEUMpy
;; D8/PDdAFqjwRa5url4lg5vVmrfhkq/Yl3FO6qtqxlQku3LawbWFCVbhZkFAPT8Zt
;; trT3Sgk1WDtM1NWGdOTWhJFrLNsY084rTycTqFsaagakrawOJ2x9q2zrhe2P2bja
;; LXHQuA+3HB3AaEiZg05iqt2ElBa8xehBPhM2h/hhrZsgqlo1ND7ZWK+jL1/HD4jG
;; NYOoIlu5B6Sn70iLR4KAwhSj02m3273eYDKWPhPVefK9DuAZWst3yuZPMV3slwpw
;; b0r9siW4sXCKTbqfTdrrmD7aPXZEw9V1Jv+UrQKnqkqyE0iaBDmIfMJH/BCeLlu6
;; VpTsq9NrH+w+Gkh1aKOlmFnl0IKYPNmqWNbSnMt4q/lFGI+fLZea9Wa8JmzpULVF
;; qs+x0D7JSdm5Gn4+2arVRVeNULj/Qd93uCIJXpogt3LK/8yPWSiEYgXYpORdocoM
;; HTaEd0EM8YGKKjQ9P/kQF8E2D7AACncm+Qojv8/wQPUE+v/xk51PXmAit9uT9WRn
;; DXr6wbdfb+9cLrNBD8mkHfj9PhL/yMemDviUhwm0rO4JFnrPPoxh5732cqbdq4o0
;; sJEgh56At3s59HkKp2bbrdHwLYQNCaASEy9mG+lXL09YclqbIxxrnKZdZp1Kkl72
;; bhaXf8Q2tz5mjYPjp/s3LQ720nLDrrSLSI9xLx1GaWqmC41W3vtgyLn0rQJWZ4wz
;; XUcRgM9xm7elIyO8Djq+EudlvXvte71Cle6FdPtwcaLhkAyyk1TiOSanEb8wnJ8l
;; 0c72S0umwCOBlkLUBF9ox4KML9WRlZk8X9uN1HBQYnbKkdBBHWSuxLrJGSRxXswc
;; bNHhsWOiX1K0z3vOL+GSaGkHlZdkUGKaWH1p9oGu1yZP0eQqM8lkeYXJA30dsSkU
;; xt0uxZr0fLZnvBdYB4IcnhypcrbsRdynmiGEsnpW5HqhR4Vztllz8AFb+RLOQJac
;; gVQ6AYsHg0dRM4HmsGNS46RJw/t9wQM07EEgI7SnMKWHfhz1Y6xTDQcYQhIYdH9Y
;; AUtbPXQVwYko/7jOBiI5D1aX9dzhDnIaCfxuwN1W2CvsYsIZtIdMJ3iXkA0/PWCB
;; dYkBgjJMs199GLK4dDK4eO6UVi9DQLNB8MAfmjiMa6vdcJv6MkDJjAsZyKodOTNv
;; Kb8B6/Bo78nLR/vP99j9yavmFzVuVDLiFRx/UVQFlullyFlhSRO65yZcqc2S/Vd5
;; DTmL1yBLzBG0yvs2I1sk7GbSLmyVVLGSCPEKaGuz+YBZ24nuXumvX6g572vzzZ1y
;; B6nnb4Kami63Al/RXasi0hriz4E5YsXp5JgBb4UvkppkLFPS0pGW6q1qDMQf6ans
;; pQ7XJGhqFHCRFK49cQmjqC9YJL5UwgoHklqGlL36HVwQa2IDP8tcTdnmzckxLpJP
;; P4pvExEqkhS8kJnam9o7UW6sytUTvdRPjlV/oX4PKHWeaZh36W5BfHkJGikWmt4k
;; Q1NpZrDJzKlijthslLa58ys3Gs/3P9n/9rMMAygfb2pOjcU3jwPf3W6tTTt1jpmg
;; oBU9afXDZ61kp00xr9n44tOd5+/fZPsgFYngU93RaIyoafwWJ01fnBOgwRbfrJz4
;; 7/asd7uWtVqq86lNjGqBfUdcNdNqRJmJKRplGAB6ZLYPCXPe9BbZQ5byr8+vyDbQ
;; QXcD0YfQzID8Kz7Wwjah14S7GISZmDOqD5cdkfeHxVYaq9jM5Z7p5U638NPPHh0+
;; r1p5zmgHmnz4sLK6gJ6EupGYzDr+IITBI6fLGmChyYuy5FlD2X7cdkD9jQbyOHTU
;; juhElXAs44aqjS6Q3R4SO4iwlzyQvO0oOhkWrZhp1MYI7OzCwtsqWStiF+oaSJHj
;; sA6ceAtLWch1qbiaUuAF/J+9BH8P41/z5udnJAQyO0VosUIFRgypHmSdpmXMhBOK
;; VqWMrWqvQ0GVCYBEW8VMbWTP5i6zXVIjgFbZ/xR4YP60LTTzHOIkQB6pJ56dxzs5
;; foulqcYYJEjPRQAsyRMDql8Kz3L8UDU5AD2PhyM24Sn+3cpdO1ga4z91KPsuhrmo
;; FQBn+QrO/9rXn68/X3/+335UaVlx/7RrF/K+pbovES9UFinfSbIEXcfP93DRnxUj
;; xWvGztAbTw5jkO6wsM8tMRn0tKZlKkGfAB7m0VfgayS9GLaCC62OWSmcsJGjtL0B
;; a4TSFoWGrRG2MPHThGPwUIZdLzNDf/gqZWVGE+ZFe5Wl8gNXwHpKQgfNIGuVubET
;; SRWX8oN22H0Sq2Z+6Q2XPoRP+k2NmKNhq5NHsLMxjahngW8RjzVdHWSs7DQatbPT
;; 6GzN0JY1/L2zu3c2gepV4iMJOlVKenV4sv/i2c7ePraN4NWpV58OzCsR1isI640T
;; 1phx3o7JxpQa2euz1ugq1FDLLlRpvMhhZNfDMuLrM2XGSAvb3LqIpHURAlePkZsE
;; RBa9nGXbsesOQTfnwur5rA6GQeuSEyEB6ufmBuh59DL5fjZXxsGWsIRHjKk1kqu5
;; On1MhmR5WImewzjsS0v6cVicwbaZVTVLXEWC0jCzXAcyrhIE86sEBOib6AKBTMFA
;; y2ZnK6qfqWrTq3nEVuT2ESjxz2ZS9oymXVEvsd14fPhkH1e4PgFNX4EjP4MHfJoD
;; Ky2BRdQqJNX3sTAiLWuPqjBVPo5QlZJW2neyWaMcj7uFk7mxdLNlqsHzNYvQuemK
;; dmkJIelEQN4dsszQ9SQSY1kuETVXS3vH6JJRfpOUd78K6DLshRmwry8s7MaRKvwP
;; fKyWdGTiFzCmZWqQsB+JDZg4KcgvLER+V1A1diCjnjJ2alQ8QEcEMlLxlBowYQz2
;; Bx7KtKMWFpSbrN7iFBtssVckOxAk8oNVoFNiYso2EXLcQWDKXn9QX2ftmHqhHmV+
;; mSVI7+GF7Iek8kJZFvFPH2TsfeZgLtx+NCSCEpZ1OfzcAI5VnyJ3TBsxmQhmXm/W
;; ik+2al/CPW3pmh86tjLBhdsWNqTQiSsNNwsS6uHJuM2W9l4poQZrh4m6NZuO3Jow
;; co1Ga6eaOK88nUygbmmoGZC2sjqcsPWtsq0Xtj9m42q3xEHjPtxydACjIWUOOkld
;; YdVBGT3IZwJ7TdJaTxu81Cqm8cnGeh19+UztNIgqspV7QHr6jrR4JHItOMDodNrt
;; dq83mIylz4QEAbvuAJ6htXynbP4U08V+qQD3ptQvW1KN0ibdzybttYyuG70cZfqq
;; 5J9izwNvFYY8GGZUlWQnkDQJchD5hI/4ITxdtpazvVHGO732we6jgazqa80qhxbE
;; tMKsimUtzbmMt5pfhPH4GZeXrjdrR3fp0LvkjrTxHP8X
;; =D/Dv
;; -----END PGP MESSAGE-----

;;}}}

(add-hook 'tinyprocmail--mode-define-keys-hook
          'tinyprocmail-mode-define-keys)

(add-hook 'tinyprocmail-output--mode-define-keys-hook
          'tinyprocmail-output-mode-define-keys)

(provide 'tinyprocmail)
(run-hooks 'tinyprocmail--load-hook)

;;; tinyprocmail.el ends here
