;;; tinylisp.el --- Emacs lisp programming help grab-bag

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program use M-x tinylisp-version
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
;; ~/.emacs startup file.
;;
;;      ;;  Select some unused, fast prefix key.
;;      ;;  In my keyboard $ is seldomly used in Emacs Lisp programmina and
;;      ;;  it can be accessed without a Shift-key.
;;      ;;  Other alternatives: "!", "_" ":"
;;
;;      (setq tinylisp--mode-prefix-key  "$")
;;      (setq tinylisp--load-hook 'tinylisp-install)
;;      (require 'tinylisp)
;;
;;      (setq tinylisp--load-hook nil)
;;
;; Or prefer following autoload: your Emacs loads this package only
;; when you need it.
;;
;;      (autoload 'tinylisp-mode                "tinylisp" "" t)
;;      (autoload 'turn-on-tinylisp-mode        "tinylisp" "" t)
;;      (add-hook 'emacs-lisp-mode-hook         'turn-on-tinylisp-mode)
;;      (add-hook 'lisp-interaction-mode-hook   'turn-on-tinylisp-mode)
;;
;;      (setq tinylisp--load-hook 'tinylisp-install)
;;      (global-set-key "\C-ce" 'tinylisp-mode)  ; mode on/off
;;      (global-set-key "\C-cmE" 'eldoc-mode)    ; In lastest Emacs
;;
;; If you don't want to use the echo-menu, but regular keymap calls
;; instead, put following into your ~/.emacs. This must be before any
;; other TinyLisp settings. You must reload package every time if you
;; change this setting.
;;
;;      (setq tinylisp--menu-use-flag nil)
;;
;; To manually install or uninstall mode, call:
;;
;;      M-x     tinylisp-install
;;      M-x     tinylisp-uninstall

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Feb 1997
;;
;;      Private lisp help functions were scattered around files and
;;      in other private libraries. One day the author decided to write
;;      a minor mode to access all those tools that were written one by one
;;      and he I didn't want to continue stacking up `lisp-mode-hook'
;;      for all the growing features. So, if you're programming in Emacs
;;      Lisp, this minor mode may slightly increase your productivity.
;;
;;  Overview of features
;;
;;      Lisp coding help
;;
;;      o   Create list of all variables from the buffer. (or occur menu)
;;          You can use this list to update your bug report function or just
;;          to get an overview of the variables. Check names and the order how
;;          you have used them (The order is important if you use defcustom)
;;      o   Create function list (or occur menu)
;;      o   Create autoload list
;;      o   Evaluate current definition under point (re-parse function,
;;          reset defvar or even defcustom variable)
;;      o   Print variable's value under point, set variable's value
;;          under point (backs up the original value which you can restore)
;;      o   Call function under point (to test it immediately)
;;      o   Indent function/variable around point.
;;      o   FINDS LISP CODE ERROR POINT.
;;      o   In DEBUGGER *Backtrace* hit 'R' to record the content of the value
;;          to *tinylisp-record* This is great when you want to send bug report
;;          to maintainer and you can attach the state of the variables
;;          with it.
;;
;;      o   Code flow help: jump to variable of function definition even if
;;          it is located in different file. Does not use TAGS; but assumes
;;          that function is `known' to Emacs.
;;      o   Two extra echo area modes: Show underlying properties/overlays or
;;          Show characters' syntax information.
;;
;;      Edebug support
;;
;;      o   Cursor at function name and calling `tinylisp-edebug-instrument'
;;          will instrument remote function. If you used just
;;          plain edebug, then you'd have to manually load the function into
;;          current point and hit `edebug-eval-defun', for each function.
;;          (Note that "i" auto-instrument doesn't always work from
;;          edebug)
;;      o   Easily uninstrument functions: at point, in buffer
;;
;;      Elp support -- Lisp code profiling
;;
;;      o   Access elp commands from echo menu
;;      o   Profile your package or buffer's functions easily.
;;          Sit somewhere in function and un/instrument it with one command.
;;          Un/instrument all functions in the buffer with one command.
;;
;;      Elint support -- Lint your elisp code
;;
;;      o   Code by Peter liljenberg, code location unknown.
;;      o   catches misspellings and undefined variables
;;      o   function calls with the wrong number of arguments, and
;;          some typos such as (let (a (car b)) ...)
;;
;;      Checkdoc support --  Check doc strings for style requirements
;;
;;      o   ftp://ftp.ultranet.com/pub/zappo
;;      o   Easy interface to checkdoc commands.
;;      o   A tool that makes sure your package follows the guidelines
;;          presented in File: elisp, Node: Documentation Tips.
;;
;;      Find-func.el support
;;
;;      o   Use this package as backup if symbol lookup fails.
;;
;;      Remarks
;;
;;      o   Please take a look new XEmacs package bench.el (19.15 and 20.2)
;;          for bechmarking.
;;
;;  Tutorial, how do you check your package
;;
;;      o   $ f  Take a look at your function names: are they well named,
;;          so that same categories begin with same words. Below it would be
;;          a mistake to have latter as xxx-ti::erase-buffer, because then
;;          you cant find all common function with `lisp-complete-symbol'
;;          command on xxx-buffer-*. Code is not a spoken language but
;;          meant to be used by programmers (Compare function naming in
;;          XEmacs and Emacs, where XEmacs does the the right thing)
;;
;;              xxx-buffer-handling
;;              xxx-buffer-clear
;;
;;          Check also that your macros are defined first before functions.
;;          If possible, maintain this definition order in your file
;;
;;              defvar, defconst, defcustom  (on top of file)
;;              defsubst
;;              defmacro
;;              defun
;;
;;      o   C-u $ v Check variable names as the function names above,
;;          but also see that you have defined right user variables which
;;          should be using `defcustom'. The extra C-u argument will print
;;          this information.
;;      o   $ x  Check the lisp package layout: first line and footer must
;;          be in proper format and that Author etc. tags are in
;;          their places.
;;      o   Check the documentation strings with Checkdoc.
;;          To get overview of errors, do: $ c -  and $ c RET
;;          which a) turned off query b) checked whole buffer for errors.
;;          When you have got a clear look, then start correcting mistakes
;;          and do $ c a (semiautomatic correction) $ c BACKSPACE to correct
;;          full buffer.
;;
;;  Defcustom.el and evaluating an `defcustom' variable
;;
;;      If you don't know what defcustom is, or if you don't use it, you
;;      can ignore this section. The defcustom variables are evaluated
;;      pretending like they were `defconst', but because this evaluation
;;      is a bit special, pay attention to following paragraph.
;;
;;      If you got thrown to error during evaluation, pay attention now,
;;      CALL COMMAND $ Z or `M-x' `tinylisp-emergency' IMMEDIATELY. For full
;;      details, see function documentation strings in the source file for
;;      these:
;;
;;          (defadvice defconst
;;          (defun     tinylisp-eval-at-point
;;
;;  Find lisp code error position
;;
;;      The most useful functions in this package are the two error
;;      finding functions which try their best to put you on a line that
;;      generates the lisp error. You can use this feature to e.g. check
;;      your ~/.emacs startup files and find the spot where
;;      `eval-current-buffer' fails.
;;
;;      The best function, `tinylisp-error-find-1', was programmed by Mikael
;;      Djurfeldt <mdj@sanscalc.nada.kth.se> and is included here with his
;;      permission. Thanks Mikael, the function saves lot lisp debugging.
;;
;;  Following lisp code call chain
;;
;;      The traditional way to follow lisp code is to use TAGS file (See
;;      'etags' or 'ctags' shell binary and C-h a "tags") which reads bunch
;;      of *el files and builds up internal representation of all defined
;;      symbols and their locations.
;;
;;      But using tags is not very flexible if you write the code yourself,
;;      because when you add new function or new variable, the TAGS file is
;;      immediately out of date. Hm. The TAGS is general tool for many
;;      programming languages, but in Emacs lisp, we can take advantage of
;;      the fact that Emacs already knows where the symbols are defined.
;;      The information is stored to `load-history' whenever you run `load'
;;      `require' `load-file' or `load-library'.
;;
;;      In this package, there are two functions that make use of
;;      `load-history' and if the symbol is not in the history, they try to
;;      find definition from the current buffer. You see, if you do
;;      `eval-current-buffer' the definition information is _not_ stored to
;;      `load-history'. With these commands you can browse some packages
;;      without any extra TAGS file.
;;
;;        [The only assumption is that you have `loaded' the file !!]
;;
;;          $ '     tinylisp-jump-to-definition (do not record call chain)
;;          $ +     tinylisp-jump-to-definition-chain (record call chain)
;;          $ \177  tinylisp-back-to-definition (probably your backspace key)
;;                  This returns to previously saved call-chain point
;;
;;      The jump command also know following prefix arguments
;;
;;          M-0 $ ' tinylisp-jump-to-definition (empty call chain)
;;          C-u $ ' tinylisp-jump-to-definition (record call-chain)
;;
;;  Examining text properties and overlays in buffer
;;
;;      If you have ever played with text properties or overlays (called
;;      extents in XEmacs), you know how hard it is to examine buffer's
;;      characters and debug where the properties are.
;;
;;      In this package there is "constant char browsing mode" where every
;;      time you move your cursor, the face info and/or overlay info is
;;      displayed in the echo-area. If you supply `3' `C-u' arguments, the
;;      information is also recored to the separate buffer. This is the
;;      most easiest way to examine some character properties in arbitrary
;;      buffer positions. See C-h f on following function:
;;
;;          $ p     tinylisp-property-show-mode
;;
;;  Examining charcter syntax
;;
;;      Major modes define syntax tables for characters and sometimes you
;;      want to see the syntax class of a character under cursor. This mode
;;      behaves in the same manner as text property display, just turn it on
;;      and it will constantly show char info.
;;
;;          $ y     tli-syntax-show-mode
;;
;;  Snooping interesting variables
;;
;;      Has is happened to you that you're debugging package and it
;;      installs many hooks and and sets many different variables and then
;;      you suddenly realize that it went all wrong? You may even have
;;      noticed that some ill behaving package keeps preventing file
;;      writing!
;;
;;      No problem, you can define interesting variable sets to peek their
;;      contents, e.g. checking all file related hooks for problems.  And if
;;      you supply C-u prefix arg, your editing is updated to the
;;      variables. With any other non-nil arg, the contents of the
;;      variables are recorded (kinda before install -- after install
;;      snooping) See function:
;;
;;          $ s     tinylisp-snoop-variables
;;
;;      And additional prefix arguments: You can save variables states,
;;      modify them as you like, and go back to restores values.
;;
;;  Elp: notes
;;
;;        [excerpt from Barry's elp.el]
;;        ...Elp can instrument byte-compiled functions just as easily as
;;        interpreted functions, but it cannot instrument macros.  However,
;;        when you redefine a function (e.g. with eval-defun), you'll need to
;;        re-instrument it with M-x `elp-instrument-function'.  This will also
;;        reset profiling information for that function.  Elp can handle
;;        interactive functions (i.e. commands), but of course any time spent
;;        idling for user prompts will show up in the timing results.
;;
;;      To elp functions right, follow these steps. _*important*_ "(defun"
;;      must be left flushed in order the function to be found. If there is
;;      any leading spaces before the '(' or 'defun', then function won't
;;      be found and will not be (un)instrumented.
;;
;;      o   $ e A  Restore (a)ll elp'd functions
;;      o   $ -    Eval buffer containing functions (or eval single function)
;;      o   $ e I  Instrument all functions in buffer (or single function)
;;      o   $ e H  Run the harness test that calls the functions
;;
;;  Elp: Summary mode's sort capabilities
;;
;;      When you call `$' ´E' `s' to show the elp result(s), the results
;;      buffer is put into `tinylisp-elp-summary-mode' where you can sort
;;      the columns with simple keystrokes. The sort keys correspond to the
;;      column names.
;;
;;          f)unction Name  c)all Count  e)lapsed Time  a)verage Time
;;          ==============  ===========  =============  =============
;;
;;  Elp: customizations
;;
;;      You should be aware of this variable in elp; which resets the list
;;      every time you display it. You can toggle it's value from the echo
;;      menu.
;;
;;          elp-reset-after-results
;;
;;  Edebug support
;;
;;      To instrument function for edebug, you'd normally have cursor inside
;;      current function and call `C-u' `M-x' `edebug-eval-defun'. But
;;      suppose you only see function call like this:
;;
;;          (my-function arg arg arg)
;;
;;      then you'd have to a) find out where the function is defined
;;      b) load that file c) position cursor over the fuction definition
;;      d) call edebug to instrument it. That's too much of a work. Instead
;;      there are commands that do this for you. See edebug submap `C-e'
;;      for edebug commands
;;
;;          $ C-e RET   Instrument function _named_ at point
;;          $ C-e DEL   Uninstrument function _named_ at point
;;          $ C-e SPC   Instrument all functions in buffer
;;          $ C-e x     Uninstrument all functions in buffer
;;          $ C-e X     Uninstrument all functions instrumented by $ C-e RET
;;
;;  Batch processing
;;
;;      Function `tinylisp-batch-generic-command-line' can send each file
;;      to named function. Caller must arrange load-path.el or similar file
;;      that takes care of defining `load-path'. Examples:
;;
;;	    emacs -q -Q -l load-path.el -l tinylisp.el \
;;          --eval "(tinylisp-batch-generic-command-line \
;;                   (function \
;;                    tinylisp-autoload-quick-build-interactive-from-file))" \
;;          file.el ...
;;
;;      Following function are also available for command line option `-f'
;;
;;	    tinylisp-batch-autoload-quick-build-from-file
;;	    tinylisp-batch-autoload-quick-build-interactive-from-file
;;          tinylisp-batch-autoload-generate-loaddefs-file
;;          tinylisp-batch-autoload-generate-loaddefs-dir
;;
;;  Todo section
;;
;;      In standard Emacs there seems to be underused package trace.el.
;;      Add direct support for it.
;;
;;      The regress.el provides support for writing and executing
;;      regression tests for Emacs Lisp code. Could that be supported too?

;;}}}
;;{{{ history

;;; Change Log:

;;; Code:

;;{{{ require

;;; ......................................................... &require ...

(require 'tinylibm)

(ti::package-require-view) ;; TinyLisp must be first in the minor-mode-list

(eval-when-compile
  (require 'advice) ;; For apropos.el
  ;; XEmacs 21.2 NT had a problem loading the edug.el. After
  ;; debug.el was loaded first, the edebug.el load succeeded.
  ;;
  ;; In older XEmacs 20.4 edebug does not "provide", so this uses
  ;; plain old `load' method.
  (or (featurep 'debug)
      (load "debug"))
  (or (featurep 'edebug)
      (load "edebug"))
  ;;  Don't show "obsolete function warning", because we know what
  ;;  we're doing below. Emulation in handled in tinylibb.el
  (put 'frame-parameters 'byte-compile nil))

(eval-and-compile
  (autoload 'tinypath-cache-match-fullpath  "tinypath")
  (autoload 'remprop                        "cl-extra")
  (autoload 'edebug-eval-defun              "edebug" "" t)
  (autoload 'generate-file-autoloads        "autoload")
  ;; Silence bytecompiler
  (defvar edebug-all-defs)
  (defvar folding-mode)
  (defvar checkdoc-arguments-in-order-flag)
  (defvar checkdoc-verb-check-experimental-flag)
  (defvar checkdoc-spellcheck-documentation-flag)
  (defvar checkdoc-bouncy-flag)
  (defvar checkdoc-bouncy-flag)
  (defvar checkdoc-autofix-flag))

(ti::package-defgroup-tiny TinyLisp tinylisp-- tools
  "Lisp programming help module.
  Overview of features.

      Lisp coding help

      o   Create list of all variables from the buffer.
          Uou can use the list in your bug report function or just
          to get an overview of the variables: names and the order how
          you have used them (The order is important if you use defcustom)
      o   Create function list (or occur menu)
      o   Create autoload list (or occur menu)
      o   Evaluate current definition under point (reparse function,
          reset defvar or defcustom variable)
      o   Print variable's value under point, set variable's value
          under point.
      o   Call function under point (to test it immediately)
      o   Indent function/variable around point.
      o   Two tun on Modes: Show underlying  properties/overlays or
          charcter symbol information.
      o   FIND LISP CODE ERROR POINT.")

;;}}}
;;{{{ setup: mode definition

(defcustom tinylisp--menu-use-flag t
  "*Non-nil means to use echo-area facilities from tinymenu.el."
  :type  'boolean
  :group 'TinyLisp)

;;  Creating a minor mode
;;
;;      This macro creates the full minor mode and all needed variables
;;
;;  Mode name "E" for minor name
;;
;;      A general lisp helper mode; please see these too:
;;
;;      (e)lisp-mode
;;      (e)lp.el
;;      (e)ldoc.el
;;      (e)xpand.el
;;
;;  Prefix variable "$"
;;
;;      You seldom use end anchor $ in lisp. Use $$ to self insert it
;;      Another characters you could choose: "!", "_"
;;      If possible, select some character that is non-shifted
;;      for maximum accessibility of this minor mode.
;;
;;      You can change the prefix key by adding this statement before
;;      loading this package:
;;
;;          (setq tinylisp--mode-prefix-key "C-cE")

;;;###autoload (autoload 'tinylisp-commentary    "tinylisp" "" t)
;;;###autoload (autoload 'tinylisp-mode          "tinylisp" "" t)
;;;###autoload (autoload 'turn-on-tinylisp-mode  "tinylisp" "" t)
;;;###autoload (autoload 'turn-off-tinylisp-mode "tinylisp" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinylisp-" " E" "$" "E" 'TinyLisp "tinylisp--" ;1-6

   "This minor mode is used along with the lisp major modes. You can
evaluate expressions, reread functions, check your lisp packages
syntax, create autoloads and do many more things.

Defined keys:

\\{tinylisp--mode-prefix-map}"

   "Emacs Lisp extras"                  ;7

   nil                                  ;8

   "Emacs Lisp menu."                   ;9

   (list                                ;arg 10
    tinylisp--mode-easymenu-name
    ["Eval whole buffer"              tinylisp-eval-current-buffer           t]
    ["Eval whole buffer, load"        tinylisp-eval-current-buffer-from-file t]
    ["Eval whole buffer as defconst"  tinylisp-eval-current-buffer-defconst  t]
    ["Eval statement at point"        tinylisp-eval-at-point                 t]
    ["Eval reverse statement at point" tinylisp-eval-reverse                 t]
    ["Eval and edit line "            tinylisp-eval-edit                     t]
    ["Eval and print result"          tinylisp-eval-print-last-sexp          t]
    ["Macroexpand macro funcall"      tinylisp-macroexpand                   t]
    "----"
    ["Call statement at point"        tinylisp-call-at-point                 t]
    ["Set value at point"             tinylisp-set-value-at-point            t]
    ["Jump to definiton"              tinylisp-jump-to-definition            t]
    ["Jump to definiton (call-chain)" tinylisp-jump-to-definition-chain      t]
    ["Back to definiton (call-chain)" tinylisp-back-to-definition            t]
    "----"
    ["Forward  user var or func"      tinylisp-forward-user-option           t]
    ["Backward user var or func"      tinylisp-backward-user-option          t]
    "----"
    (list
     "Modes, find error, debug"
     ["Mode, property show"            tinylisp-property-show-mode           t]
     ["Mode, char syntax show"         tinylisp-syntax-show-mode             t]
     ["Find lisp error, method 1"      tinylisp-error-find-1                 t]
     ["Find lisp error, method 2"      tinylisp-error-find-2                 t]
     ["Add code debug tags"            tinylisp-error-debug-add-tags         t])

    (list
     "Lisp Library"
     ["Show symbol load path"        tinylisp-library-find-symbol-load-info  t]
     ["show loaded libraries"        tinylisp-library-info-emacs             t]
     ["Load one"                     tinylisp-library-info-emacs             t]
     ["Load by regexp"               tinylisp-library-load-by-regexp         t]
     ["Find file"                    tinylisp-library-find-file              t]
     ["Display documentation"        tinylisp-library-documentation          t])
    (list
     "Variables and Symbols"
     ["Occur"                          tinylisp-occur-verbose                t]
     ["Occur, select next"             tinylisp-occur-select-forward         t]
     ["Collect variable list"          tinylisp-find-variable-list           t]
     ["Collect variable list, occur"   tinylisp-find-variable-list-occur     t]
     ["Collect function list"          tinylisp-find-function-list           t]
     ["Collect function list, occur"   tinylisp-find-function-list-occur     t]
     "----"
     ["Info, buffer local variables"   tinylisp-find-buffer-local-variables  t]
     "----"
     ["Grep adviced functions"         tinylisp-ad-match                     t]
     ["Grep Hooks"                     tinylisp-find-match-from-hooks        t]
     ["Grep variables"                 tinylisp-find-match-from-variables    t]
     ["Grep symbols"                   ti::system-describe-symbols           t])

    (list
     "Autoload"
     ["Quick autoloads from buffer"
      tinylisp-autoload-quick-build-from-buffer           t]
     ["Quick autoloads from file"
      tinylisp-autoload-quick-build-from-file             t]
     ["Quick autoloads interactive from file"
      tinylisp-autoload-quick-build-interactive-from-file t]
     ["Quick autoloads from directory"
      tinylisp-autoload-quick-build-from-dir              t]
     ["Quick autoloads recursive"
      tinylisp-autoload-quick-autoload-build-recursive    t]
     "----"
     ["Loaddefs from file"
      tinylisp-autoload-generate-loaddefs-file      t]
     ["Loaddefs from directory"
      tinylisp-autoload-generate-loaddefs-dir       t]
     ["Loaddefs recursive"
      tinylisp-autoload-generate-loaddefs-recursive t])

    (list
     "Miscellaneous"
     ["Describe library's symbols"     tinylisp-library-symbol-information   t]
     ["Snoop variables"                tinylisp-snoop-variables              t]
     ["Emergency - defcustom"          tinylisp-emergency                    t]
     ["Indent function or variable"    tinylisp-indent-around-point          t]
     ["Narrow to function"             tinylisp-narrow-to-function           t]
     ["Widen"                          widen                                 t]
     ["Byte compile current function." tinylisp-byte-compile-sexp            t]

     ["Show call tree for file"
      tinylisp-byte-compile-display-call-tree t]

     ["Face, show font lock faces"     tinylisp-face-list-font-lock-faces    t]
     ["Face, show all faces"           tinylisp-face-list-known-faces        t]
     ["Process kill"                   tinylisp-process-kill                 t]
     ["Process list"                   list-processes                        t])

    (list
     "Package layout check"
     ["Check overall layout syntax"  tinylisp-lisp-mnt-verify                t]
     ["Check or fix layout tags in buffer"
      tinylisp-lisp-mnt-tag-check-and-fix-buffer t]
     ["Check or fix layout tags in file"
      tinylisp-lisp-mnt-tag-check-and-fix-file t]
     ["Check or fix layout tags in directory"
      tinylisp-lisp-mnt-tag-check-and-fix-dir t])

    "----"

    (list
     "Documentation check."
     ["Check forward"                tinylisp-checkdoc                       t]
     ["Check buffer, take notes"     tinylisp-checkdoc-notes                 t]
     ["Check comments"               checkdoc-comments                       t]
     ["Check comments, take notes"   tinylisp-checkdoc-comment-notes         t]
     ["Check defun, current point"   checkdoc-eval-defun                     t]
     ["Checkdoc minor mode"          checkdoc-minor-mode                     t])

    (list
     "Elint"
     ["Check buffer"                 tinylisp-elint-buffer                   t]
     ["Check defun"                  tinylisp-elint-defun                    t])

    (list
     "Edebug"
     ["Instrument   function"        tinylisp-edebug-instrument              t]
     ["Uninstrument function"        tinylisp-edebug-uninstrument            t]
     ["Instrument   buffer"          tinylisp-edebug-instrument-buffer       t]
     ["Uninstrument buffer"          tinylisp-edebug-uninstrument-buffer     t]
     ["Uninstrument everything"      tinylisp-edebug-uninstrument-everything t])

    (list
     "Elp lisp profiling menu"
     ["Instrument function"           tinylisp-elp-instrument-function    t]
     ["Instrument buffer"             tinylisp-elp-instrument-buffer      t]
     ["Instrument by regexp"          tinylisp-elp-instrument-by-regexp   t]
     ["Uninstrument function"         tinylisp-elp-restore-function       t]
     ["Uninstrument buffer"           tinylisp-elp-restore-buffer         t]
     ["Uninstrument all"              tinylisp-elp-restore-all            t]
     ["Reparse instrumentation"       tinylisp-reparse-instrumentation    t]

     "----"

     ["List instrumented functions"   tinylisp-elp-function-list-partial  t]
     ["List All instrumented functions" tinylisp-elp-function-list        t]
     ["Harness test (eval from point)" tinylisp-elp-harness               t]
     ["Master set"                    tinylisp-elp-set-master             t]
     ["Master reset"                  elp-unset-master                    t]
     ["Reset timing list"             tinylisp-elp-reset-list             t]
     ["Show  timing list"             tinylisp-elp-results                t])

    ["Keyboard menu"                  tinylisp-menu-main                     t]
    ["Mode on  for all lisp buffers"  turn-on-tinylisp-mode-all-buffers      t]
    ["Mode off for all lisp buffers"  turn-on-tinylisp-mode-all-buffers      t]
    ["Package version"                tinylisp-version                       t]
    ["Package commentary"             tinylisp-commentary                    t]
    ["Mode help"                      tinylisp-mode-help                     t]
    ["Mode off"                       tinylisp-mode                          t]

    "----")

   (progn                               ;arg 11
     (cond
      (tinylisp--menu-use-flag
       ;;  Using menu to remeber commands is easier if you don't use
       ;;  menu bar at all.
       (define-key root-map p 'tinylisp-menu-main))

      (t
       (tinylisp-install-menu)

       (define-key map "\C-m" 'tinylisp-eval-print-last-sexp)

       (define-key map "Z" 'tinylisp-emergency)

       (define-key map "-"   'tinylisp-eval-current-buffer)
       (define-key map "*"   'tinylisp-eval-current-buffer-from-file)
       (define-key map "="   'tinylisp-eval-current-buffer-defconst)
       (define-key map "."   'tinylisp-eval-at-point)
       (define-key map "\\"  'tinylisp-eval-reverse)

       (define-key map "m"   'tinylisp-macroexpand) ;; if @ is inaccessible

       (define-key map "cc"   'tinylisp-byte-compile-buffer)
       (define-key map "cs"   'tinylisp-byte-compile-sexp)
       (define-key map "ct"   'tinylisp-byte-compile-display-call-tree)

       (define-key map ","   'tinylisp-call-at-point)
       (define-key map ";"   'tinylisp-set-value-at-point)
       (define-key map "!"   'tinylisp-error-find-1)
       (define-key map "#"   'tinylisp-error-find-2)
       (define-key map "%"   'tinylisp-error-debug-add-tags)
       (define-key map "'"   'tinylisp-jump-to-definition)
       (define-key map "+"   'tinylisp-jump-to-definition)
       (define-key map "'\177"   'tinylisp-back-to-definition)

       (define-key map "{"   'tinylisp-backward-user-option)
       (define-key map "}"   'tinylisp-forward-user-option)

       (define-key map "<"   'tinylisp-indent-around-point)

       ;; FIXME
       (define-key map "ab"  'tinylisp-autoload-quick-build-from-buffer)
       (define-key map "ad"  'tinylisp-autoload-guick-build-from-dir)
       (define-key map "af"  'tinylisp-autoload-quick-build-from-file)
       (define-key map "ai"  'tinylisp-autoload-quick-build-interactive-from-file)
       (define-key map "ar"  'tinylisp-autoload-quick-autoload-build-recursive)
       (define-key map "aF"  'tinylisp-autoload-generate-loaddefs-file)
       (define-key map "aD"  'tinylisp-autoload-generate-loaddefs-dir)
       (define-key map "aR"  'tinylisp-autoload-generate-loaddefs-recursive)

       (define-key map "ia"  'tinylisp-ad-match)
       (define-key map "ie"  'tinylisp-library-info-emacs)
       (define-key map "ih"  'tinylisp-find-match-from-hooks)
       (define-key map "il"  'tinylisp-library-symbol-information)
       (define-key map "iL"  'tinylisp-find-buffer-local-variables)
       (define-key map "is"  'ti::system-describe-symbols)
       (define-key map "iv"  'tinylisp-find-match-from-variables)

       (define-key map "I"   'tinylisp-eval-edit)

       (define-key map "f"   'tinylisp-find-function-list)
       (define-key map "F"   'tinylisp-find-function-list-occur)

       (define-key map "lf"   'tinylisp-library-find-file)
       (define-key map "ll"   'tinylisp-library-load-library)
       (define-key map "lL"   'tinylisp-library-load-by-regexp)
       (define-key map "ls"   'tinylisp-library-find-symbol-load-info)
       (define-key map "ld"   'tinylisp-library-documentation)

       (define-key map "n"   'tinylisp-narrow-to-function)

       (define-key map "o"   'tinylisp-occur-verbose)
       (define-key map "+"   'tinylisp-occur-select-forward)

       (define-key map "p"   'tinylisp-property-show-mode)
       (define-key map "S"   'tinylisp-snoop-variables)

       (define-key map "v"   'tinylisp-find-variable-list)
       (define-key map "V"   'tinylisp-find-variable-list-occur)

       (define-key map "w"   'widen)
       (define-key map "x"   'tinylisp-checkdoc)
       (define-key map "y"   'tinylisp-syntax-show-mode)

       (define-key map "Xv"  'tinylisp-lisp-mnt-verify)
       (define-key map "Xt"  'tinylisp-lisp-mnt-tag-check-and-fix-buffer)
       (define-key map "Xf"  'tinylisp-lisp-mnt-tag-check-and-fix-file)
       (define-key map "Xd"  'tinylisp-lisp-mnt-tag-check-and-fix-dir)

       (define-key map "bv"  'tinylisp-b-variables)
       (define-key map "bf"  'tinylisp-b-funcs)
       (define-key map "br"  'tinylisp-b-record)
       (define-key map "bR"  'tinylisp-b-record-empty)
       (define-key map "bt"  'tinylisp-b-eval)

       (define-key map "ei"  'tinylisp-elp-instrument-function)
       (define-key map "eI"  'tinylisp-elp-instrument-buffer)
       (define-key map "eI"  'tinylisp-elp-instrument-by-regexp)
       (define-key map "eu"  'tinylisp-elp-restore-function)
       (define-key map "eU"  'tinylisp-elp-restore-buffer)
       (define-key map "eA"  'tinylisp-elp-restore-all)
       (define-key map "ee"  'tinylisp-reparse-instrumentation)
       (define-key map "eh"  'tinylisp-elp-harness)

       (define-key map "ef"  'tinylisp-elp-function-list-partial)
       (define-key map "eF"  'tinylisp-elp-function-list)
       (define-key map "er"  'tinylisp-elp-reset-list)
       (define-key map "es"  'tinylisp-elp-results)
       (define-key map "em"  'tinylisp-elp-set-master)
       (define-key map "eM"  'elp-unset-master)

       (define-key map "E\C-m" 'tinylisp-elint-buffer)
       (define-key map "E "    'tinylisp-elint-defun)

       (define-key map "\C-e\C-m" 'tinylisp-edebug-instrument)
       (define-key map "\C-e\C-h" 'tinylisp-edebug-uninstrument)
       (define-key map "\C-e "    'tinylisp-edebug-instrument-buffer)
       (define-key map "\C-ex"    'tinylisp-edebug-uninstrument-buffer)
       (define-key map "\C-eX"    'tinylisp-edebug-uninstrument-everything)

       (define-key map "1f"      'tinylisp-face-list-font-lock-faces)
       (define-key map "1F"      'tinylisp-face-list-known-faces)
       (define-key map "1p"      'tinylisp-process-kill)
       (define-key map "1P"      'list-processes))))))

;;; ................................................... &&mode-summary ...

;;;###autoload (autoload 'tinylisp-elp-summary-mode          "tinylisp" "" t)
;;;###autoload (autoload 'turn-on-tinylisp-elp-summary-mode  "tinylisp" "" t)
;;;###autoload (autoload 'turn-off-tinylisp-elp-summary-mode "tinylisp" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinylisp-elp-summary-" " Elp-sum" nil " Elp-sum" 'TinyLisp
   "tinylisp--elp-summary-"             ;1-6

   "Commands to help sorting elp summary buffer.
Defined keys:

\\{tinylisp--elp-summary-prefix-mode-map}"

   "Elp summary sort"                   ;7

   nil                                  ;8

   "Elp summary sort menu."             ;9

   (list                                ;arg 10
    tinylisp--elp-summary-mode-easymenu-name
    ["Sort by function name"     tinylisp-elp-summary-sort-column-1 t]
    ["Sort by call count"        tinylisp-elp-summary-sort-column-2 t]
    ["Sort by elapsed time"      tinylisp-elp-summary-sort-column-3 t]
    ["Sort by average time"      tinylisp-elp-summary-sort-column-4 t])
   (progn                               ;arg 11
     ;; Function Name  Call Count  Elapsed Time  Average Time
     ;; =============  ==========  ============  ============
     (define-key map "f"  'tinylisp-elp-summary-sort-column-1)
     (define-key map "c"  'tinylisp-elp-summary-sort-column-2)
     (define-key map "e"  'tinylisp-elp-summary-sort-column-3)
     (define-key map "a"  'tinylisp-elp-summary-sort-column-4))))

;;}}}
;;{{{ setup: hooks

(defcustom tinylisp--load-hook nil
  "*Hook that is run when package is loaded.
A good value could be '(turn-on-tinylisp-mode-all-buffers) to activate
the minor mode in every Emac slisp buffer."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp--find-func-list-hook 'tinylisp-highlight-default
  "*Hook run when tinylisp-find-function-list-hook has displayed the list."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp--find-var-list-hook 'tinylisp-highlight-default
  "*Hook run when `tinylisp-find-function-list' has displayed the list."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp--with-current-buffer-hook '(turn-on-tinylisp-mode)
  "*Hook run after ´tinylisp-with-current-buffer'."
  :type  'hook
  :group 'TinyLisp)

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinylisp--register ?\'
  "*An Emacs register that is used e.g. for saving point or copying text."
  :type  'character
  :group 'TinyLisp)

(defcustom tinylisp--macroexpand-function-list
  '("cl-prettyexpand" "macroexpand")
  "*Completion list of function STRINGS to expand macro call.
You can use commands `cl-prettyexpand', which sometimes does
good formatting, but does not necessarily expand to what you want to see.
The default command is `macroexpand'."
  :type '(repeat string)
  :group 'TinyLisp)

(defcustom tinylisp--table-reverse-eval-alist
  '((add-hook       . remove-hook)
    (remove-hook    . add-hook))
  "*Table of reverse commands. Format '((ORIG-FSYM . REVERSE-FSYM) ..)."
  :type  'sexp
  :group 'TinyLisp)

(defcustom tinylisp--table-snoop-variables
  '(("hook-command"
     (pre-command-hook
      post-command-hook
      post-command-idle-hook))
    ("hook-file"
     (write-file-functions
      find-file-hook
      after-save-hook))
    ("hook-mail"
     (mail-mode-hook
      mail-setup-hook
      mail-citation-hook
      mail-yank-hooks
      mail-send-hook))
    ("hook-message"
     (message-mode-hook
      message-setup-hook
      message-signature-setup-hook
      message-header-setup-hook
      message-header-hook
      message-send-hook
      message-sent-hook))
    ("hook-basic"
     (pre-command-hook
      post-command-hook
      post-command-idle-hook
      write-file-functions
      find-file-hook
      after-save-hook
      after-init-hook)))
  "*List of interesting variables printed from `tinylisp-snoop-variables'.
Non existing variables can also be listed but they are not checked.

Format:

 '((\"LIST-NAME\" (var var var ..))
   ...)"

  :type '(repeat
          (list
           (string :tag "Completion name")
           (repeat (symbol :tag "Var"))))
  :group 'TinyLisp)

;;}}}
;;{{{ setup: private variables

(defvar tinylisp--harness-flag nil
  "Described in function `tinylisp-elp-harness'.
This variable is set to t when harness is on going and set to
nil when harness test is over.")

(defvar tinylisp--call-chain nil
  "List of buffers and buffer positions. '(mark mark ..)
Whenever you call `tinylisp-jump-to-definition' the current positions
is recoded and one more element to the _beginning_ of list is added.
You can navigate back with `tinylisp-back-to-definition' and the first
element from the list is removed.")

(defvar tinylisp--buffer-elp "*tinylisp-elp*"
  "Temporary elp info buffer.")

(defvar tinylisp--buffer-autoload "*tinylisp-autoloads*"
  "Temporary buffer.")

(defvar tinylisp--buffer-variables "*tinylisp-variables*"
  "Temporary buffer.")

(defvar tinylisp--buffer-data "*tinylisp-data*"
  "Temporary buffer.")

(defvar tinylisp--buffer-library "*tinylisp-library*"
  "Temporary buffer.")

(defvar tinylisp--buffer-record "*tinylisp-record*"
  "Record variable contents to this buffer.")

(defvar tinylisp--buffer-tmp "*tinylisp-tmp*"
  "Temporary buffer.")

(defvar tinylisp--buffer-macro "*tinylisp-macroexpand*"
  "Temporary buffer.")

(defvar tinylisp--buffer-eval " *tinylisp-eval*"
  "Temporary buffer.")

(defconst tinylisp--regexp-macro-definition
  "^\\(defun\\*\\|defcustom\\|defgroup\\|defadvice\\)"
  "Regexp for commands that define macros, like `defcustom' `defgroup'.")

(defconst tinylisp--regexp-function
  (concat
   "^(\\("
   ;;  cl DEFINES defun* macro
   (regexp-opt
    '("defun"
      "defun*"
      "defsubst"
      "defmacro"
      "defalias"
      "defadvice"
      "defalias"
      "define-derived-mode"
      "define-minor-mode"
      ;; See SEMI poe.el
      "defun-maybe"
      "defmacro-maybe"
      "defalias-maybe"
      ;; see Gnus nntp.el for deffoo
      "deffoo"))
   "\\)[ \t]+\\([^ \t\n]+\\)")
  "Regexp to match functions.
This must have SUBMATCH1 and SUBMATCH2 which represent function
type and name.")

(defconst tinylisp--regexp-variable
  (concat
   "^(\\("
   (regexp-opt
    '("defvar"
      "defconst"
      "defvaralias"
      ;; Custom.el defined variables in 19.35
      "defgroup"
      "defcustom"))
   "\\)[ \t]+\\([^ \t\n]+\\)")
  "Regexp to match variables.
This must have SUBMATCH1 and SUBMATCH2 which represent
variable type and name.")

(defvar tinylisp--variable-not-charset "^][()'`\", \t\n\r"
  "When reading variable from buffer, unse this is character set.
Notice that ^ at the beginning of character set reverses it.")

(defvar tinylisp--find-error nil
  "'Find error' function's data.")

(defvar tinylisp--occur-history nil
  "History.")

(defvar tinylisp--elp-regexp-history  nil
  "History.")

(defvar tinylisp--elp-not-regexp-history  nil
  "History.")

(defvar tinylisp--elp-master-history  nil
  "History.")

;;  Too bad this is hard coded in emacs..
(defvar tinylisp--occur-buffer-name "*Occur*"
  "Emacs Occur buffer.")

(defvar tinylisp--edebug-instrument-table  nil
  "Edebug instrumentation information.

Format:

  '((function buffer-pointer buffer-file-name)
    (function buffer-pointer buffer-file-name)
    ..)")

;;}}}
;;{{{ setup: private, mode

;;; These must not be made buffer local.

(defvar tinylisp--property-show-mode nil
  "Property show mode (flag).")

(defvar tinylisp--syntax-show-mode nil
  "Property show mode (flag).")

;;}}}
;;{{{ setup: menu

(defvar tinylisp--menu-main) ;;  Just a forward declaration

(defun tinylisp-install-menu ()
  "Install `tinylisp--menu-main'."
  ;;  this is a function because if user changes prefix key and
  ;;  calls tinylisp-install, we must reflect the change here in
  ;;  self insert command.
  ;;

  (defconst tinylisp--menu-main         ;bookmark -- &menu
    (list

     ;;  All commands do not fit to echo menu, but here are at least
     ;;  the most used ones.

     '(format
       "\
%s -=*.\\rmE)val ,;'+)call wn)ar py)mode o)ccur a)uto vVfF xSdD >beEcilX C-e"
       (if current-prefix-arg
           (format "%s"  (prin1-to-string current-prefix-arg))
         "Lisp:"))
     (list
      (cons ??  'tinylisp--menu-help)
      (cons ?\C-m  (list '(tinylisp-eval-print-last-sexp)))
      (cons ?-  (list '(call-interactively 'tinylisp-eval-current-buffer)))
      (cons ?*  (list '(call-interactively
                        'tinylisp-eval-current-buffer-from-file)))
      (cons ?=  (list '(call-interactively
                        'tinylisp-eval-current-buffer-defconst)))
      (cons ?.  (list '(call-interactively 'tinylisp-eval-at-point)))
      (cons ?,  (list '(tinylisp-call-at-point current-prefix-arg)))
      (cons ?\\ (list '(call-interactively 'tinylisp-eval-reverse)))
      (cons ?\; (list '(call-interactively 'tinylisp-set-value-at-point)))
      (cons ?!  (list '(call-interactively 'tinylisp-error-find-1)))
      (cons ?#  (list '(call-interactively 'tinylisp-error-find-2)))
      (cons ?%  (list '(call-interactively 'tinylisp-error-debug-add-tags)))
      (cons ?+  (list '(call-interactively 'tinylisp-jump-to-definition-chain)))
      (cons ?'  (list '(call-interactively 'tinylisp-jump-to-definition)))
      (cons ?\177  (list '(tinylisp-back-to-definition)))
      (cons ?{  (list '(call-interactively 'tinylisp-backward-user-option)))
      (cons ?}  (list '(call-interactively 'tinylisp-forward-user-option)))
      (cons ?[  (list '(call-interactively 'tinylisp-backward-user-option)))
            (cons ?]  (list '(call-interactively 'tinylisp-forward-user-option)))
      (cons ?<  (list '(call-interactively 'tinylisp-indent-around-point)))
      (cons ?B  (list '(call-interactively 'tinylisp-byte-compile-sexp)))
      (cons ?f  (list '(call-interactively 'tinylisp-find-function-list)))
      (cons ?F  (list '(call-interactively 'tinylisp-find-function-list-occur)))
      (cons ?I  (list '(call-interactively 'tinylisp-eval-edit)))
      ;; Small "h" is reserved for echo-menu help
      (cons ?n  (list '(call-interactively 'tinylisp-narrow-to-function)))
      (cons ?m  (list '(call-interactively 'tinylisp-macroexpand)))
      (cons ?o  (list '(call-interactively  'tinylisp-occur-verbose
                                            current-prefix-arg)))
      (cons ?+  (list '(tinylisp-occur-select-forward current-prefix-arg)))
      (cons ?p  (list '(tinylisp-property-show-mode current-prefix-arg 'verb)))
      (cons ?S  (list '(let* ((i (tinylisp-snoop-variables-i-args)))
                         (tinylisp-snoop-variables
                          (nth 0 i) (nth 1 i)))))
      (cons ?v  (list '(tinylisp-find-variable-list current-prefix-arg)))
      (cons ?V  (list '(call-interactively 'tinylisp-find-variable-list-occur)))
      (cons ?w  (list '(call-interactively 'widen)))
      (cons ?y  (list '(tinylisp-syntax-show-mode current-prefix-arg 'verb)))
      (cons ?Z  (list '(call-interactively 'tinylisp-emergency)))
      (cons ?\C-c  (list '(call-interactively 'tinylisp-commentary)))
      (cons ?\C-e  'tinylisp--menu-edebug)
      (cons ?\C-v  (list '(call-interactively 'tinylisp-version)))
      (cons ?a       'tinylisp--menu-autoload)
      (cons ?i       'tinylisp--menu-info)
      (cons ?e       'tinylisp--menu-elp)
      (cons ?E       'tinylisp--menu-elint)
      (cons ?H       'tinylisp--menu-help)
      (cons ?b       'tinylisp--menu-buffers)
      (cons ?c       'tinylisp--menu-checkdoc)
      (cons ?C       'tinylisp--menu-compile)
      (cons ?l       'tinylisp--menu-lisp-library)
      (cons ?1       'tinylisp--menu-misc-1)
      (cons ?X       'tinylisp--menu-lisp-mnt)
      ;; Self insert command
      ;;     User may have defined multichararcter minor map entry
      ;;     like C-cE, we only do self insert if it is NOT
      ;;     multicharacter.
      (cons (string-to-char             ;get first char
             (substring tinylisp--mode-prefix-key 0 1))
            (list
             '(let ((key (ti::keymap-single-key-definition-p
                          tinylisp--mode-prefix-key)))
                (if (characterp key)
                    (insert tinylisp--mode-prefix-key)
                  (message "\
TinyLisp: Can't self-insert. Prefix is not one charcracter.")))))))
    "Emacs Lisp coding help menu.
Documentation of variable `tinylisp--menu-main' which is main menu
for mode function `tinylisp-mode'. You can access the mode with
\\[tinylisp-mode]. Prefix key for the minor mode is defined in
`tinylisp--mode-prefix-key'.

Menu controls:

   /     Return to previous menu (if in sub-menu)
   h     Echo-menu help. Output this screen and quit
   q     Quit.
   H     TinyLisp Help menu.

Eval commands:

    -  Eval whole buffer

    *  Reload buffer from file with load command. This has the effect that
       the function and variable definitions are recorded to load
       history and you can use \\[tinylisp-jump-to-definition] command.

    =  Treat all variables as defconst and eval buffer. (With this
       you can read the defaults if you're in package buffer)

    .  Eval current statement. If you have made changes to the function or
       variable, which can be also defvar, this command evaluates it again
       so that it gets the new definition. (defvar is treated as defconst)

    \\  Reverse command around point and eval the statement. See
       variable `tinylisp--table-reverse-eval-alist'. E.g. if you see
       `add-hook', the statement is interpreted as `remove-hook'.

    RET

        Eval statement _preceeding_ the cursor. This will output the
        returned values one by one. E.g.

            (cutrrent-buffer)RET
            --> <buffer>

    r   Reload packages to Emacs by regexp. If you have downloaded
        new packages and your Emacs session is open, this is easy
        way to refresh packages to your Emacs.

Finding errors and debugging

    m  Macroexpand a macro symbol. [See also (comma) to expand functions]

    I  Read current line, allow ed(I)ting it, then eval the statement.

    !  Find errors. Go to `point-min' and evaluate buffer portions
       until error occurs.

    #  Find Lisp error with method 2. Try this if previous failed.

    %  Insert permanent debug tags. With \\[universal-argument] remove
       debug tags. If the byte compilation gives a weird error and does not
       tell the function and keys ! or # claim that all lisp code is valid,
       you should instrument debug tags and try byte compiling again.

    Z  Emergency! If you evaled `defcustom' variable and you were thrown
       to error buffer, call this command immediately to
       restore TinyLisp. The defcustom is adviced and this fixes it.

Function and code flow

    '  Jump to a definition of variable or function.
       With \\[universal-argument], save the call-chain point.
       With non-nil prefix argument, clear the call chain. Use
       BACKSPACE or \\177 (C-h) key to go back the saved call chain.

    +  Record position to call chain before jump to the definition. This
       is shortcut to calling key \".\" with the prefix arg.

    DEL     Back to previous definition and remove mark from call chain.

    }]      Go to next user option; a star mark, or to user
            function; interactive.
    {[      Same as above, but backward.

Symbol manipulation

    ,  Call current word around point. If the word is a variable, print
       value. If word is a function, call function or show `symbol-function'

    ;  Set new value for variable at point. If the read word is not an
       existing variable, then this only prints warning messages.
       old value is saved if there is no previous backup.

       \\[universal-argument]       Restore backup'd value
       \\[universal-argument]\\[universal-argument]  Force setting backup value to current value.

    S  Snoop variables. See `tinylisp--table-snoop-variables'
       Following prefix arguments are recognized:
       1           Record snooped values to
                   to buffer `tinylisp--buffer-record'
       0           Save state
       9           Restore values from saved state.
       8           Kill saved states
       5           Set all snooped variables to nil.
       \\[universal-argument]         edit variable

Symbol find

    a  Create autoloads by reading current buffer (must have
       `buffer-file-name'). With prefix argument, ask package
       name and locate it in `load-path'.

    A  Create autoloads from directory's files matching regular epression

    d  Describe symbols. This scans whole Emacs obarray to find all
       matching symbols. --> See also [I]nfo menu for more targetted
       matching.

    D  Describe loaded package. You can rip all the documentation from
       a file by doing this 1) load file into Emacs 2) eval it and finally
       3) call this function and give file path. It collects all variable
       and function documentation to a single display.

Listing and occur commands:

    o  Run occur for full buffer and filter out comments. Prefix arg says
       _not_ to filter out full comment lines.

    +  Go to next occur line in buffer. With \\[universal-argument] backward.

    f  Find all functions from the buffer
    F  Find function and create occur menu.

    l  Show symbol Load information (file where is was defined)
    L  Library information, examine all packages in Emacs.

    v  Find all variables from buffer. Prefix args classifies variables.
    V  Find variables and create occur menu.

Modes and utilities

    p  Property show mode. Three \\[universal-argument]'s turn on recording.
    y  syntax mode, Show syntax of charcter under cursor.

    X  Check variable and function documentation strings. Do they follow
       Emacs Lisp code guidelines? File: elisp,  Node: Documentation Tip.
       (Uses package lisp-mnt.el)

Function commands:

    n  Narrow to current lisp function.

    w  Widen (\\[widen])

    <  Indent current function or variable around point.

Byte compilation

    B  Byte compile defun around point. With prefix arg DISSASSEBMLE.

    See [C]ompile menu for more options.

Additional menus

    b   Buffer menu. Jump to TinyLisp temp buffers.
    c   Checkdoc, docstring syntax checker menu.
    C   Byte (C)ompilation menu.
    e   Elp menu. Emacs lisp profiler menu.
    E   Elint menu. Emacs Lisp code syntax checker menu.
    H   Help menu.
    i   Info menu. Find adviced functions, find from hooks/variables,
        list local variables.
    l   Library menu. Load, find lisp libraries for editing.
    1   Misc menu 1: Display face settings, process kill menu.

    C-e Edebug, Emacs Lisp debugger menu"))

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-misc-1
  (list
   '(format "%sMisc 1: f)onts F)onts all p)rocess-kill P)rocess list"
            (if current-prefix-arg
                (format "%s "  (prin1-to-string current-prefix-arg))
              ""))
   (list
    (cons ?f  (list '(call-interactively 'tinylisp-face-list-font-lock-faces)))
    (cons ?F  (list '(call-interactively 'tinylisp-face-list-known-faces)))
    (cons ?p  (list '(call-interactively 'tinylisp-process-kill)))
    (cons ?P  (list '(call-interactively 'list-processes)))
    (cons ?/  'tinylisp--menu-main)))
  "*Miscellaneous interface: Processes and fonts.
/       Back to root menu
q       Quit menu
f       List font lock faces available. With prefix arg, the gory details.
F       List ALL known faces. With prefix arg, the gory details.
p       Kill running processes interactively.
P       List running processes.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-lisp-library
  (list
   '(format
     "%sLibrary: s)sym-where l)load L)oad-re f)ind pP)kg-where d)doc"
     (if current-prefix-arg
         (format "%s "  (prin1-to-string current-prefix-arg))
       ""))
   (list
    (cons ?f   (list '(call-interactively  'tinylisp-library-find-file)))
    (cons ?l   (list '(call-interactively  'tinylisp-library-load-library)))
    (cons ?L   (list '(call-interactively  'tinylisp-library-load-by-regexp)))
    (cons ?s   (list '(tinylisp-library-find-symbol-load-info)))
    (cons ?p   (list '(tinylisp-library-locate
                       (tinylisp-library-read-name)
                       current-prefix-arg)))
    (cons ?P   (list '(progn
                        (tinylisp-library-locate-by-fullpath-intercative))))
    (cons ?d   (list '(call-interactively  'tinylisp-library-documentation)))

    (cons ?/   'tinylisp--menu-main)))
  "*Lisp library interface:
/       Back to root menu
q       Quit menu

s       Try to locate file where symbol was defined. This relies on
        internal representation of symbols inside Emacs `load-history'.

l       Load one library with completion into Emacs. (evaluate)

L       Load again libraries inside Emacs matching regexp. E.g. if you want to
        reload all of present gnus, supply regexp `gnus'
        NOTE: The regexp is matches against full path, so using
        anchor '^PACKAGE-NAME' will not work.

f       `find-file' a library for editing.

p       Package search: like `locate-library' but find all occurrances
        of package. With prefix argument, insert data into buffer.

P       Package search: Search packages whose full path name matches
        regexp. In order to use this feature, package `tinypath.el'
        must be available. This command calls directly its functions.

d       Display Lisp file's documentation.
        With prefix argument insert documentation to current point.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-compile
  (list
   '(format "%sByte-Compile: c)ompile t)tree"
            (if current-prefix-arg
                (format "%s "  (prin1-to-string current-prefix-arg))
              ""))
   (list
    (cons ?c  '( (tinylisp-byte-compile-buffer)))
    (cons ?s  '( (tinylisp-byte-compile-sexp)))
    (cons ?t  '( (tinylisp-byte-compile-display-call-tree)))
    (cons ?/  'tinylisp--menu-main)))
  "*Elint interface: Check code syntax.
/       Back to root menu
q       Quit menu
RET     Lint buffer
SPC     Lint defun")

;;; ----------------------------------------------------------------------
;;;
(defmacro tinylisp-require (sym)
  "Require package SYM."
  `(unless (featurep ,sym)
     (require ,sym)))

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-elp
  '((let (val)
      (tinylisp-require 'elp)
      (format
       "%selp: iIRuUAe)instrument fF)unc rsS%s)time H)arness mM)aster%s"
       ;;  Is there functions instrumented?
       (if elp-all-instrumented-list
           (if (eq 0 (setq val (length elp-all-instrumented-list)))
               ""
             (format "%d " val))
         "")
       (if elp-reset-after-results
           ":t"
         "")
       (if elp-master
           (concat ":" (symbol-name elp-master))
         "")))
    ((?i  . ( (call-interactively 'tinylisp-elp-instrument-function)))
     (?I  . ( (call-interactively 'tinylisp-elp-instrument-buffer)))
     (?R  . ( (call-interactively 'tinylisp-elp-instrument-by-regexp)))
     (?u  . ( (call-interactively 'tinylisp-elp-restore-function)))
     (?U  . ( (call-interactively 'tinylisp-elp-restore-buffer)))
     (?A  . ( (call-interactively 'tinylisp-elp-restore-all)))
     (?H  . ( (tinylisp-elp-harness current-prefix-arg 'verb)))
     (?e  . ( (call-interactively 'tinylisp-reparse-instrumentation)))
     (?m  . ( (call-interactively 'elp-set-master)))
     (?M  . ( (call-interactively 'elp-unset-master)))
     (?f  . ( (tinylisp-elp-function-list-partial current-prefix-arg 'verb)))
     (?F  . ( (tinylisp-elp-function-list current-prefix-arg 'verb)))
     (?r  . ( (call-interactively 'tinylisp-elp-reset-list)))
     (?s  . ( (tinylisp-elp-results current-prefix-arg)))
     (?S  . (t (tinylisp-elp-reset-after-results)))
     (?/  . tinylisp--menu-main)))
  "Elp help menu.
The menu shows some status parameters in the echo area.

  '[COUNT] elp:  [:t]list'
   |               |
   |               See 'S' key when this is shown
   Count of currently instrumented functions

Basic commands:

/   Back to root menu
q   Quit menu

i   Instrument current function at point
u   Uninstrument function at point

I   Instrument all functions in buffer.
U   Uninstrument all functions in buffer.

R   Instrument by regexp mapping all Emacs functions.
    If given prefix arg, then uninstrument instead.

A   Uninstrument all functions in elp list (reastore all)

e   r(e)parse instrumentation: forget all instrumented functions,
    eval buffer to read new function definitions, and instrument those
    functions.

Misc:

H   Harness test. Eval everything 3 times from current point forward
    and record results. See `tinylisp-elp-harness' for full explanation.
    Prefix arg determines harness rounds.
m   Set master function. When functions below master are called, the timing
    infomation is gathered.
M   Unset master function.

Function information:

f   List _all_ instrumented functions . Prefix arg to display the functions
    in separate buffer.
F   Same as above, but list all only specific functions in
    `elp-function-list'.

Timing information:

s   Show timing results. With prefix arg save results to RECORD buffer.
S   rese(:t) flag, Toggle setting of variable `elp-reset-after-results'.
r   Reset timing list.")

(defconst tinylisp--menu-info
  '("info: a)d e)macs f)ile-sym o)hooks l)ocal-vars s)ym v)ar A)utoload"
    ((?A  . ( (call-interactively 'tinylisp-find-autoload-functions)))
     (?a  . ( (call-interactively 'tinylisp-ad-match)))
     (?e  . ( (call-interactively 'tinylisp-library-info-emacs)))
     (?f  . ( (call-interactively 'tinylisp-library-symbol-information)))
     (?o  . ( (call-interactively 'tinylisp-find-match-from-hooks)))
     (?l  . ( (call-interactively 'tinylisp-find-buffer-local-variables)))
     (?v  . ( (call-interactively 'tinylisp-find-match-from-variables)))
     (?s  . ( (call-interactively 'ti::system-describe-symbols)))
     (?/  . tinylisp--menu-main)))
  "Display information about lisp symbols in Emacs

/   Back to root menu
q   Quit menu

a   List all adviced functions that match advice NAME. E.g. to find all
    `my' advices.

e   Show all libraries and symbols loaded into Emacs known by `load-history'.

f   Describe file symbols. Gather all documentation from symbols in FILE.
    You have to load the file into Emacs first (eval it with \\[load-file]),
    because this function reads the documentation properties from memory.

l   Decribe library symbols. This is like `f', but you do not need to give
    the full path name, but the file will be located along `load-path'.

L   Show buffer local variables.

o   Search a match from contents of all -hook -function -functions symbols
    E.g. you can locate all hooks that have function matching 'my'.

s   Search any symbol (variable or function) from Emacs obrray with REGEXP.

v   Search all variables matching variable-REGEXP and whose value match
    VALUE-REGEXP.")

(defconst tinylisp--menu-buffers
  '("go buffer: a)utoload rR)ecord v)vars f)uncs e)val E)lp"
    ((?a  . ( (tinylisp-b-autoload)))
     (?r  . ( (tinylisp-b-record)))
     (?R  . ( (tinylisp-b-record-empty)))
     (?v  . ( (tinylisp-b-variables)))
     (?f  . ( (tinylisp-b-funcs)))
     (?e  . ( (tinylisp-b-eval)))
     (?E  . ( (tinylisp-b-elp)))
     (?/  . tinylisp--menu-main)))
  "Display TinyLisp buffers.

/   Back to root menu.
q   Quit menu
a   Display autoload buffer
r   Display the record buffer where the variable contents
    are stored when you call \\[universal-argument] `tinylisp-call-at-point'
R   Kill record buffer.
v   variables buffer
f   functions buffer
e   eval buffer")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-elint
  '("Elint: RET)buffer SPC)defun"
    (
     (?\C-m . ( (tinylisp-elint-buffer)))
     (?\    . ( (tinylisp-elint-defun)))
     (?/    . tinylisp--menu-main)))
  "Elint interface: Check code syntax.
/       Back to root menu
q       Quit menu
RET     Lint buffer
SPC     Lint defun")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-help
  '("Help: m)mode c)commentary v)ersion"
    ((?m . ( (tinylisp-mode-help)))
     (?c . ( (tinylisp-commentary)))
     (?v . ( (tinylisp-version)))
     (?/ . tinylisp--menu-main)))
  "Help menu:
/       Back to root menu
q       Quit menu
m   `tinylisp-mode' Mode description
v   `tinylisp-version'
c   `tinylisp-commentary'")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-autoload
  '("Autoloads: b)uffer fi)le d)dir r)ecur Loaddefs: F)ile D)ir R)ecursive"
    ((?b . ( (call-interactively 'tinylisp-autoload-quick-build-from-buffer)))
     (?f . ( (call-interactively 'tinylisp-autoload-quick-build-from-file)))
     (?i . ( (call-interactively
	      'tinylisp-autoload-quick-build-interactive-from-file)))
     (?d . ( (call-interactively
	      'tinylisp-autoload-guick-build-from-dir)))
     (?r . ( (call-interactively
	      'tinylisp-autoload-quick-autoload-build-recursive)))
     (?F . ( (call-interactively 'tinylisp-autoload-generate-loaddefs-file)))
     (?D . ( (call-interactively 'tinylisp-autoload-generate-loaddefs-dir)))
     (?R . ( (call-interactively
	      'tinylisp-autoload-generate-loaddefs-recursive)))
     (?/ . tinylisp--menu-main)))
  "Help menu:
/       Back to root menu
q       Quit menu

Quick autoloads: list of all macros and functions

    b   Show autoloads from buffer which defines `buffer-file-name'.
        With prefix argument, ask package

    f   Write autoloads from FILE.el.

    i   Write interactive function autoloads from FILE.el.

    d   From DIRECTORY matching REGEXP, exluding REGEXP, write all autolaods
        to each FILE-autoload.el

Real autolaods: FILE-loaddefs.el

    F   Generate FILE-loaddefs.el from FILE.el   (FIXME: not yet implemented)
    D   Same, but for whole directory.
    R   Same, but recursively for all directories.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-lisp-mnt
  '("Lisp-mnt: RET)verify SPC)fix tags f)file d)directory"
    ((?\C-m . ( (tinylisp-lisp-mnt-verify)))
     (?\    . ( (tinylisp-lisp-mnt-tag-check-and-fix-buffer 'error)))
     (?f    . ( (tinylisp-lisp-mnt-tag-check-and-fix-file   'error))) ;;#todo:
     (?d    . ( (tinylisp-lisp-mnt-tag-check-and-fix-dir    'error))) ;;#todo:
     (?/    . tinylisp--menu-main)))
  "Lisp-mnt.el interface: check package layout syntax.

/       Back to root menu
q       Quit menu
RET     Check whole buffer with `lm-verify'
SPC     Check whole buffer tags and automatically fix them
f       Check file
d       Check all files in directory")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-edebug
  '("Edebug: un/instrument DEL/RET)func x/SPC)buffer l)list e)lint xX)it"
    ((?\C-m . ( (tinylisp-edebug-instrument)))
     (?\b   . ( (tinylisp-edebug-uninstrument)))
     (?\177 . ( (tinylisp-edebug-uninstrument)))
     (?\C-h . ( (tinylisp-edebug-uninstrument)))
     (?\    . ( (tinylisp-edebug-instrument-buffer)))
     (?l    . ( (tinylisp-edebug-display-instrumented-list)))
     (?x    . ( (tinylisp-edebug-uninstrument-buffer)))
     (?X    . ( (tinylisp-edebug-uninstrument-everything)))
     (?e    . ( (tinylisp-elint-defun)))
     (?/    . tinylisp--menu-main)))
  "Edebug interface.

/       Back to root menu
q       Quit menu

RET     Instrument function call (the name) at point. E.g. if you cursor is
        on top of `my-function' symbol. this is not the same as
        instrumenting with \\[universal-argument] \\[eval-defun], which
        instruments _whole_ function at point.

DEL     Uninstrument as above. Backspace key works too.

SPC     Instrument all functions in this buffer

x       Uninstrument all functions in this buffer

X       Uninstrument everything known to TinyLisp. This requires that
        you have have had TinyLisp running before you started
        instrumenting function with \\[tinylisp-edebug-instrument] or with
        \\[universal-argument] \\[eval-defun].

e       Elint current function (code check).

l       List all known instrumented functions.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp--menu-checkdoc
  '((let (spell
          val)
      (tinylisp-require 'checkdoc)
      (setq spell checkdoc-spellcheck-documentation-flag)
      (cond
       ((not (featurep 'checkdoc))
        (error "No checkdoc available (not loaded)."))
       ((not (boundp 'checkdoc-verb-check-experimental-flag))
        (error "You have old checkdoc.el version.")))
      (format
       (concat
        "%s%s%s%s%s checkdoc: "
        "SPC)point RET)notes DEL)fwd cC)om  m)ode Flags: aA~- Bb Ss Tt")
       (cond
        ((eq checkdoc-autofix-flag nil)             "-")
        ((eq checkdoc-autofix-flag 'automatic)      "Auto")
        ((eq checkdoc-autofix-flag 'semiautomatic)  "Semi")
        ((eq checkdoc-autofix-flag 'query)          "Query")
        ((null checkdoc-autofix-flag)               "")
        (t "?"))
       (cond
        ((null checkdoc-bouncy-flag)                "")
        ((eq   checkdoc-bouncy-flag 'never)         "")
        (t "B"))
       (cond
        ((null checkdoc-arguments-in-order-flag)    "")
        (t "O"))
       (if checkdoc-verb-check-experimental-flag    "E" "")
       (cond
        ((eq spell  'defun)         "sD")
        ((eq spell  'buffer)        "sB")
        ((eq spell  'interactive)   "sI")
        ((eq spell  t)              "S")
        ((null spell)               "")
        (t                          "s?"))
       (let ((sym 'checkdoc-triple-semi-comment-check-flag))
         (if (and (boundp sym)
                  (symbol-value sym))
             "T"
           ""))))
    ((?\    . ( (checkdoc-eval-defun)))
     (?\177 . ( (tinylisp-checkdoc)))
     (?\b   . ( (tinylisp-checkdoc)))
     (?\C-m . ( (tinylisp-checkdoc-notes current-prefix-arg)))
     (?\C-j . ( (tinylisp-checkdoc-notes current-prefix-arg)))
     (?m    . ( (call-interactively 'checkdoc-minor-mode)))
     (?c    . ( (checkdoc-comments)))
     (?C    . ( (tinylisp-checkdoc-comment-notes)))
     (?a    . (t (progn (setq  checkdoc-autofix-flag 'semiautomatic))))
     (?A    . (t (progn (setq  checkdoc-autofix-flag 'automatic))))
     (?~    . (t (progn (setq  checkdoc-autofix-flag 'query))))
     (?-    . (t (progn (setq  checkdoc-autofix-flag nil))))
     (?B    . (t (progn (setq  checkdoc-bouncy-flag t))))
     (?b    . (t (progn (setq  checkdoc-bouncy-flag nil))))
     (?O    . (t (progn (setq  checkdoc-arguments-in-order-flag t))))
     (?o    . (t (progn (setq  checkdoc-arguments-in-order-flag nil))))
     (?E    . (t (progn (setq  checkdoc-verb-check-experimental-flag t))))
     (?e    . (t (progn (setq  checkdoc-verb-check-experimental-flag nil))))
     (?S    . (t (progn (setq  checkdoc-spellcheck-documentation-flag t))))
     (?s    . (t (progn (setq  checkdoc-spellcheck-documentation-flag nil))))
     (?d    . (t (progn (setq  checkdoc-spellcheck-documentation-flag
                               'defun))))
     (?r    . (t (progn (setq  checkdoc-spellcheck-documentation-flag
                               'buffer))))
     (?T    . (t (progn
                   (when (boundp 'checkdoc-triple-semi-comment-check-flag)
                     (setq  checkdoc-triple-semi-comment-check-flag t)))))
     (?t    . (t (progn
                   (when (boundp 'checkdoc-triple-semi-comment-check-flag)
                     (setq checkdoc-triple-semi-comment-check-flag
                           nil)))))))
  "According to checkdoc manual:
...The Emacs Lisp manual has a nice chapter on how to write
documentation strings.  Many stylistic suggestions are fairly
deterministic and easy to check for programatically, but also easy
to forget. The main checkdoc engine will perform the stylistic
checks needed to make sure these styles are remembered.

The echo area menu shows following status information

  [-|O|E|V|S|T] checkdoc:
   | | | | | |
   | | | | | `checkdoc-triple-semi-comment-check-flag'
   | | | | `checkdoc-spellcheck-documentation-flag'
   | | | `checkdoc-verb-check-experimental-flag'
   | | `checkdoc-arguments-in-order-flag'
   | `checkdoc-bouncy-flag' state
   `checkdoc-autofix-flag'

Commands:

/       Back to root menu.
SPC     `checkdoc-eval-defun'
DEL     Check code from current point forward.
RET     `tinylisp-checkdoc-notes' Start checking from current point forward.
        Supply prefix argument, if you want to check whole buffer.
c       `checkdoc-comments'
C       `tinylisp-checkdoc-comment-notes'

Checkdoc mode flags that can be changed:

m       Turn on minor mode which checks docstring while you write them
Aa~-    Change `checkdoc-autofix-flag'       A)uto a)semi ~)query -)never
Bb      Change `checkdoc-bouncy-flag'                     B)on b)off
Oo      Change `checkdoc-arguments-in-order-flag'         O)n  o)ff
Ee      Change `checkdoc-verb-check-experimental-flag'    E)on e)off
Tt      Change `checkdoc-triple-semi-comment-check-flag'  T)on t)off
Ssdr    Change checkdoc-spellcheck-documentation-flag'
        s)off S)interactive d)efun r)buffer

======================================================================
        Excerpts from Checkdoc 0.5
======================================================================

`checkdoc-autofix-flag'

    Non-nil means attempt auto-fixing of doc-strings.
    If this value is the symbol 'query, then the user is queried before
    any change is made. If the value is 'automatic, then all changes are
    made without asking unless the change is very-complex.  If the value
    is 'semiautomatic, or any other value, then simple fixes are made
    without asking, and complex changes are made by asking the user first.
    The value 'never is the same as nil, never ask or change anything.
    checkdoc-bouncy-flag

`checkdoc-bouncy-flag'

    Non-nil means to 'bounce' to auto-fix locations.
    Setting this to nil will silently make fixes that require no user
    interaction.  See `checkdoc-autofix-flag' for auto-fixing details.

`checkdoc-force-docstrings-flag'

    Non-nil means that all checkable definitions should have documentation.
    Style guide dictates that interactive functions MUST have documentation,
    and that its good but not required practice to make non user visible items
    have doc-strings.

`checkdoc-arguments-in-order-flag'

    Non-nil means warn if arguments appear out of order.
    Setting this to nil will mean only checking that all the arguments
    appear in the proper form in the documentation, not that they are in
    the same order as they appear in the argument list.  No mention is
    made in the style guide relating to order.

`checkdoc-verb-check-experimental-flag'

    Non-nil means to attempt to check the voice of the doc-string.
    This check keys off some words which are commonly misused.  See the
    variable `checkdoc-common-verbs-wrong-voice' if you wish to add your
    own.

`checkdoc-spellcheck-documentation-flag'

    Non-nil means run Ispell on doc-strings based on value.
    This will be automatically set to nil if Ispell does not exist on your
    system.  Possible values are:

    nil          - Don't spell-check during basic style checks.
    'defun       - Spell-check when style checking a single defun
    'buffer      - Spell-check only when style checking the whole buffer
    'interactive - Spell-check only during `checkdoc-interactive'
    t            - Always spell-check

`checkdoc-triple-semi-comment-check-flag'

    Non-nil means to check for multiple adjacent occurrences of ;;; comments.
    According to the style of Emacs code in the lisp libraries, a block
    comment can look like this:

    ;;; Title
    ;;  text
    ;;  text

    But when inside a function, code can be commented out using the ;;;
    construct for all lines.  When this variable is nil, the ;;; construct
    is ignored regardless of it's location in the code.

Auto-fixing:

    There are four classifications of style errors in terms of how
    easy they are to fix.  They are simple, complex, really complex,
    and impossible.  (Impossible really means that checkdoc does not
    have a fixing routine yet.)  Typically white-space errors are
    classified as simple, and are auto-fixed by default.  Typographic
    changes are considered complex, and the user is asked if they want
    the problem fixed before checkdoc makes the change.  These changes
    can be done without asking if `checkdoc-autofix-flag' is properly
    set.  Potentially redundant changes are considered really complex,
    and the user is always asked before a change is inserted.  The
    variable `checkdoc-autofix-flag' controls how these types of errors
    are fixed.

Spell checking doc-strings:

      The variable `checkdoc-spellcheck-documentation-flag' can be set
    to customize how spell checking is to be done.  Since spell
    checking can be quite slow, you can optimize how best you want your
    checking done.  The default is 'defun, which spell checks each time
    `checkdoc-defun' or `checkdoc-eval-defun' is used.  Setting to nil
    prevents spell checking during normal usage.
      Setting this variable to nil does not mean you cannot take
    advantage of the spell checking.  You can instead use the
    interactive functions `checkdoc-Ispell-*' to check the spelling of
    your documentation.
      There is a list of lisp-specific words which checkdoc will
    install into Ispell on the fly, but only if Ispell is not already
    running.  Use `Ispell-kill-Ispell' to make checkdoc restart it with
    these words enabled.")

;;}}}
;;{{{ macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-with-current-buffer 'lisp-indent-function 1)
(defmacro tinylisp-with-current-buffer (buffer &rest body)
  "Make BUFFER and run hook `tinylisp--with-current-buffer-hook'."
  `(with-current-buffer ,buffer
     ,@body
     (run-hooks 'tinylisp-with-current-buffer-hook)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-directory-recurse-macro 'lisp-indent-function 1)
(put 'tinylisp-directory-recurse-macro 'edebug-form-spec '(body))
(defmacro tinylisp-directory-recurse-macro (directory &rest body)
  "Start from DIRECTORY and run BODY recursively in each directory.

Following variables are set during BODY:

`dir'      Directrory name
`dir-list' All directories under `dir'."
  `(flet ((recurse
           (dir)
           (let ((dir-list (tinylisp-directory-list dir)))
             ,@body
             (when dir-list
               (dolist (elt dir-list)
                 (unless (string-match tinylisp--ignore-dir-regexp elt)
                   (recurse elt)))))))
     (recurse ,directory)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-with-file-env-macro 'lisp-indent-function 0)
(put 'tinylisp-with-file-env-macro 'edebug-form-spec '(body))
(defmacro tinylisp-with-file-env-macro (&rest body)
  "Run BODY with all the interfering hooks turned off."
  `(let (find-file-hook
	 find-file-not-found-functions
	 write-file-functions
	 font-lock-mode
	 ;; buffer-auto-save-file-name
	 auto-save-hook
	 auto-save-default
	 after-insert-file-functions
	 (auto-save-interval 0)
	 (original-backup-inhibited backup-inhibited)
	 (version-control 'never)
	 (backup-inhibited t))
     ;; Reset also global
     (setq-default backup-inhibited t)
     ;;  When each file is loaded to emacs, do not turn on lisp-mode
     ;;  or anything else => cleared file hooks. These are byte compiler
     ;;  silencers:
     (if (null find-file-hook)
         (setq find-file-hook nil))
     (if (null find-file-not-found-functions)
         (setq find-file-not-found-functions nil))
     (if (null write-file-functions)
         (setq write-file-functions nil))
     (if (null write-file-functions)
         (setq write-file-functions nil))
     (if (null font-lock-mode)
         (setq font-lock-mode nil))
     (if (null auto-save-hook)
         (setq auto-save-hook nil))
     (if (null auto-save-default)
         (setq auto-save-default nil))
     (if auto-save-interval
         (setq auto-save-interval 0))
     (if backup-inhibited
         (setq backup-inhibited t))
     (if after-insert-file-functions
	 (setq after-insert-file-functions nil))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-find-file-no-select 'lisp-indent-function 0)
(put 'tinylisp-find-file-noselect  'edebug-form-spec '(body))
(defmacro tinylisp-find-file-noselect (&rest body)
  "Disable hooks and run BODY"
  `(tinylisp-with-file-env-macro
     (find-file-noselect ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-insert-file-contents 'lisp-indent-function 0)
(put 'tinylisp-insert-file-contents 'edebug-form-spec '(body))
(defmacro tinylisp-insert-file-contents (&rest body)
  "Disable hooks and run BODY"
  `(tinylisp-with-file-env-macro
     (insert-file-contents ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-get-buffer-create (&optional buffer)
  "Call (get-buffer-create buffer) and put it in `emacs-lisp-mode'."
  (with-current-buffer (get-buffer-create buffer)
    (unless (eq major-mode 'emacs-lisp-mode)
      (emacs-lisp-mode))
    (current-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-read-word ()
  "Read word under point."
  (let ((str (or (ti::remove-properties
                  (ti::buffer-read-word tinylisp--variable-not-charset))
                 (when (bolp)
                   (ti::buffer-match
                    (concat "^[^ \t\n\r]*\\(["
                            tinylisp--variable-not-charset
                            "]+\\)+")
		    0)))))
    (when str
      ;;  Remove trainling colon
      (if (string-match "\\(.+\\):$" str)
          (match-string 1 str)
        str))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-safety-belt (function &optional msg arg)
  "If FUNCTION does not exists, signal error and refer to MSG.
Call FUNCTION with ARG if it exists."
  (unless (fboundp function)
    (error "TinyLisp: %s not exist. %s" (symbol-name function) (or msg "" )))
  (if arg
      (ti::funcall function arg)
    (ti::funcall function)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-get-symbol (string)
  "Return symbol from STRING.
If function does not exist or is string cannot be read, then return nil

  \"(function arg1\"  --> 'function
  \"(defvar xx\"      --> 'xx
   'xxx-symbol        --> 'xxx-symbol"
  (let ((re-f    (substring tinylisp--regexp-function
			    1 (length tinylisp--regexp-function)))
	(re-v    (substring tinylisp--regexp-variable
			    1 (length tinylisp--regexp-variable)))
	sym)
    (cond
     ((and (or (string-match re-f string)
               (string-match re-v string))
           (setq sym (intern-soft
                      (match-string 2 string)))))
     ;;  Read first word then
     ((setq sym (ti::string-match "[^()'\",.; \t\n\]+" 0 string))

      ;;  Delete trailing garbage "this-function:" --> "this-function"
      (if (string-match "\\(.*\\)[^a-zA-Z0-9*]$" sym)
          (setq sym (match-string 1 sym)))

      (setq sym (intern-soft sym))))
    sym))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-push-call-chain (&optional pop data verb)
  "Push current point to call chain.
Input:

  POP       flag, instead of push, do pop to last saved positions
  DATA      push DATA to chain.
  VERB      print verbose messages.

Optionally POP. VERB prints message."
  (if (null pop)
      (push data tinylisp--call-chain)
    (if (null tinylisp--call-chain)
        (error "tinylisp--call-chain is empty, nothing to pop.")
      (let ((mark (pop tinylisp--call-chain)))
        (goto-char mark)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-symbol-do-macro 'lisp-indent-function 2)
(defmacro tinylisp-symbol-do-macro (string noerr &rest body)
  "Execute body if STRING is interned.
Input:
  STRING    function or variable name.
  NOERR     If nil, then call error. if Non-nil then print message if
            STRING was not interned.
  BODY      Forms to execute."
  `(if (intern-soft ,string)
       (progn
         (setq ,string (intern-soft ,string))
         ,@body)
     (if ,noerr
         (message "TinyLisp: No symbol in obarray: %s" ,string)
       (error "TinyLisp: No symbol in obarray: %s" ,string))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-record-macro 'lisp-indent-function 1)
(defmacro tinylisp-record-macro (flag &rest body)
  "If FLAG is non-nil execute BODY in record buffer."
  `(if ,flag
       (tinylisp-with-current-buffer
	   (ti::temp-buffer tinylisp--buffer-record)
	 (ti::pmax)
	 ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-defun-macro 'lisp-indent-function 0)
(defmacro tinylisp-defun-macro (&rest body)
  "(&rest body) Determine sexp bounds and execute BODY.
Uses `end-of-defun' `forward-sexp' to determine sexp.

Bound variables in macro:

  `beg' `end'   sexp bounds.
  `str'         full line read from 'beg' point
  `buffer'      points to the current buffer

You use this macro to bounds of Lisp defun, defvar, defconst
structures."
  `(let ((buffer (current-buffer))
          str
          beg
          end)
     (if (null buffer)
         (setq buffer nil))             ;No-op, byteComp silencer
     (save-excursion
       (end-of-defun)
       (setq end (point))
       (forward-sexp -1)
       ;;  If no used, ByteComp nags -- silence it so that this macro
       ;;  can be used
       (setq beg (point))
       (if (null beg)
           (setq beg nil))
       (setq str (ti::read-current-line))
       (goto-char end)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-read-function-name-info (&optional string)
  "Return '(name . sym) After 'defxxxxx'. at point or STRING."
  (let* ((name (ti::string-match "def[a-zA-Z]+ +\\([^() \t\n\]+\\)" 1
                                 (or string (ti::read-current-line))))
         (sym  (and name (intern-soft name))))
    (if name
        (cons name sym))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-defun-sym-macro 'lisp-indent-function 0)
(defmacro tinylisp-defun-sym-macro (&rest body)
  "Run BODY when defun sym is found.
Same as `tinylisp-defun-macro' But define `name' and `sym' for function name."
  `(tinylisp-defun-macro
    (let* ((info (tinylisp-read-function-name-info str))
           (name (car-safe info))
           (sym  (cdr-safe info)))
      (if (null info)                   ;Bytecomp silencer.
          (setq info nil))
      (if (null sym)                    ;Bytecomp silencer.
          (setq sym nil))
      ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-defcustom-macro 'lisp-indent-function 0)
(defmacro tinylisp-defcustom-macro (&rest body)
  "Activate advice 'tinylisp' for `defconst' _only_ during BODY."
  `(unwind-protect
       (progn
         (ad-enable-advice 'defconst 'around 'tinylisp)
         (ad-activate 'defconst)
         ,@body)
     ;;  Make sure this is always executed.
     (tinylisp-emergency)))

;;}}}
;;{{{ Install

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-menu-main (&optional arg)
  "Show echo area menu and pass ARG to `ti::menu-menu'."
  (interactive "P")
  (unless tinylisp--menu-main
    (tinylisp-install-menu))
  (ti::menu-menu 'tinylisp--menu-main arg))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinylisp-mode-all-buffers (&optional off)
  "Turn function `tinylisp-mode' on in every Lisp buffer. Optionally turn OFF."
  (interactive "P")
  (ti::dolist-buffer-list
   (string-match "lisp\\|debugger-mode" (downcase (symbol-name major-mode)))
   'tmp-buffers-too
   nil
   (progn
     (when (eq major-mode 'debugger-mode)
       (tinylisp-debugger-setup))
     (if off
         (unless (null tinylisp-mode)
           (turn-off-tinylisp-mode))
       (unless tinylisp-mode
         (turn-on-tinylisp-mode))))))

;;; ----------------------------------------------------------------------
;;;
(defun turn-off-tinylisp-mode-all-buffers  ()
  "Call turn-on-tinylisp-mode-all-buffers' with argument off."
  (turn-on-tinylisp-mode-all-buffers 'off))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-install-hooks (&optional uninstall)
  "Install or UNINSTALL hooks that activate TinyLisp minor mode."
  (let ()
    (ti::add-hooks '(emacs-lisp-mode-hook
                     lisp-interaction-mode-hook
                     debugger-mode-hook
                     help-mode-hook
                     gnus-edit-form-mode-hook
                     Info-mode-hook)
                   'turn-on-tinylisp-mode
                   uninstall)
    (unless (boundp 'apropos-mode-hook)
      ;;  Standard Emacs does not have this hook
      (defvar apropos-mode-hook nil
        "*Hook run when mode is turned on.")
      (defadvice apropos-mode (after tinylisp act)
        "Run `apropos-mode-hook'."
        (run-hooks 'apropos-mode-hook)))
    (if (boundp 'apropos-mode-hook)
        (ti::add-hooks 'apropos-mode-hook 'turn-on-tinylisp-mode uninstall))
    (ti::add-hooks 'tinylisp--mode-define-keys-hook
                   'tinylisp-mode-define-keys uninstall)
    ;; tinylisp-elp-summary-install-mode
    (ti::add-hooks 'tinylisp--elp-summary-mode-define-keys-hook
                   'tinylisp-elp-summary-mode-define-keys
                   uninstall)
    (ti::add-hooks '(tinylisp-debugger-setup turn-on-tinylisp-mode)
		   'debugger-mode-hook
		   uninstall)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-install (&optional uninstall)
  "Install package and activate mode in every Emacs lisp buffer.
To turn on mode on by buffer basis, call `tinylisp-mode'."
  (interactive "P")
  (tinylisp-install-hooks uninstall)
  (turn-on-tinylisp-mode-all-buffers uninstall))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-uninstall ()
  "Uninstall package."
  (interactive)
  (tinylisp-install 'uninsall))

;;}}}
;;{{{ advice

;;; ----------------------------------------------------------------------
;;; byte-compile-file (filename &optional load)
(defadvice byte-compile-file (around tinylisp act)
  "Change interactive prompt and offer current buffer for compiling(.el).
With prefix arg, byte compile and load file."
  (interactive
   (list
    (read-file-name
     (if current-prefix-arg
         "TinyLisp: Byte compile and load file: "
       "TinyLisp: byte compile file: ")
     (if (and buffer-file-name
              (string-match "\\.el$" buffer-file-name))
         buffer-file-name
       (file-name-directory (or (buffer-file-name)
                                default-directory))))
    current-prefix-arg))
  ad-do-it)

;;; ----------------------------------------------------------------------
;;;
(defadvice defconst (around tinylisp (sym val &optional doc &rest args) dis)
  "This advice is only used during TinyLisp runs its own functions.
It ignores any extra arguments passed to defconst. In order to
evaluate following statement

  (defcustom my nil \"docs\" :type 'string :group my)

TinyLisp first converts it to

  (defconst my nil \"docs\" :type 'string :group my)

And turns on this advice to ignore additional :type and :group arguments.
This all is needed, because defcustom defines the variable as defvar
and it cannot be re-evaluated/reset without this trick.

After the eval has been done, this advice is turned off.
If you see this message when calling following, there is bug in TinyLisp.

  (describe-function 'defconst)"
  (ad-with-originals (defconst)
    ;;  advice prior 19.36 will not work properly with special forms
    ;;  like defconst. Hans explained is as follows to me:
    ;;
    ;;  | >   (ad-with-originals (defconst)
    ;;  | >     (defconst sym val doc)    ;; Nothing happens?
    ;;
    ;; The reason nothing happens here, is that 'sym' does not get evaluated
    ;; (since 'defconst' is a special form), instead it actually assigns the
    ;; value to the constant with the name "sym".  What you would need to do
    ;; is use `eval', e.g.,
    ;;
    ;;    (ad-with-originals (defconst)
    ;;      (eval `(defconst ,sym ,val ,doc)))
    ;;
    ;; Hans Chalupsky <hans@ISI.EDU>
    ;;
    (eval `(defconst ,sym ,val ,doc))))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-process-kill ()
  "Kill running processes with y-n-p."
  (let ((list (process-list)))
    (if (null list)
        (message "TinyLisp: no running processes to kill.")
      (list-processes)
      (dolist (proc (process-list))
        (when (y-or-n-p (format "Kill: %s " (prin1-to-string proc)))
          (delete-process proc))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-face-list-unique (face-list)
  "Return unique faces '((var face) ..) from FACE-LIST."
  (let ((getface 'get-face)
	face
	list)
    (dolist (var face-list)
      (when (and
             (not (string-match "^:" (symbol-name var)))
             (or (and (fboundp 'face-font) ;; XEmacs
                      (ignore-errors (face-font var))
                      (setq face var))
                 (if (or (and (fboundp getface) ;;  XEmacs
                              (funcall getface var))
                         ;; Only works in Emacs. Returns nil in XEmacs
                         (facep var))
                     (setq face var)))
             ;; Filter out duplicates like 'bold
             (not (member var list)))
        (push (list var face) list)))
    list))

;;; ----------------------------------------------------------------------
;;; (load-library "flyspell")
;;; (tinylisp-face-print (current-buffer) '(flyspell-incorrect-face))
;;;
(defun tinylisp-face-print (buffer face-list &optional details)
  "Insert description to BUFFER for each symbol in FACE-LIST.
If optional DETAILS is non-nil, display also 'face-defface-spec properties."
  (let ((list (tinylisp-face-list-unique face-list))
	beg
	var
	face
	plist)
    (when list
      (setq buffer (ti::temp-buffer tinylisp--buffer-tmp 'clear))
      (with-current-buffer buffer
        (dolist (elt (sort list
			   (lambda (a b)
			     (string<
			      (symbol-name (car a))
			      (symbol-name (car b))))))
          (setq var    (car elt)
                face   (nth 1 elt)
		plist  (if details
			   (format
			    " %s\n"
			    (plist-get
			     (symbol-plist face)
			     'face-defface-spec))
			 ""))
	  (insert (format "%-35s" (symbol-name var)))
          (setq beg  (point))
          (insert "abcdef12345  ")
          (set-text-properties beg (point) (list 'face face))
          (if (ti::emacs-p)
              (insert (format "fg: %-15s  bg: %s%s"
                              (face-foreground face)
                              (face-background face)
			      plist))
            (insert (format "\n  fg: %-15s\n  bg: %s%s"
                            (face-foreground face)
                            (face-background face)
			    plist)))
	  (insert "\n")))
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-face-list-font-lock-faces (&optional details)
  "List known font lock faces and colors used.
Optionally give DETAILS of each face.
See also \\[list-colors-display]."
  (interactive "P")
  (cond
   ((not (featurep 'font-lock))
    (message "tinylisp.el: font-lock.el is not loaded. No faces."))
   (t
    (let ((symbols
           (ti::system-get-symbols "^font-lock-.*face$" '(boundp sym))))
      (when symbols
        (let ((buffer (ti::temp-buffer tinylisp--buffer-tmp 'clear)))
          (tinylisp-face-print buffer symbols details)
          (display-buffer buffer)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-face-list-known-faces (&optional details)
  "List all known 'face' variables.
Optionally give DETAILS of each face.
See also \\[list-colors-display]."
  (interactive "P")
  (let ((symbols (ti::system-get-symbols
		  "face"
		  '(or (boundp sym)
		       (and (fboundp 'get-face) ;;  XEmacs
			    (get-face sym))
		       ;; Only works in Emacs. Returns nil in XEmacs
		       (facep sym))))
	(buffer  (ti::temp-buffer tinylisp--buffer-tmp 'clear)))
    (tinylisp-face-print buffer symbols details)
    (display-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-highlight-default ()
  "Highlight functions and variables, see tinylisp-*[func,var}*-hook."
  (when (ti::colors-supported-p) ;; does it make sense to show colors?
    (save-excursion (ti::text-re-search-forward "defmacro" 0 'highlight))
    (save-excursion (ti::text-re-search-forward "defsubst" 0 'bold))
    (save-excursion (ti::text-re-search-forward "defconst" 0 'highlight))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-show-register-message (&optional msg)
  "Show what to do with register and show optional MSG."
  (message
   (or msg
       (substitute-command-keys
        (format
         (concat
          "TinyLisp: Jump back to previous positon with "
          "\\[jump-to-register-compatibility-binding] %s")
         (char-to-string tinylisp--register))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-symbol-type (symbol &optional noerr)
  "Return 'var or 'func according to SYMBOL.
If NOERR is non-nil, do not call error if symbol type isn't known.
That usually means that symbol is not yet defined to obarray."
  (cond
   ((and (fboundp symbol)
         (boundp symbol))
    (if (y-or-n-p (format "select %s: Y = variable, N = Function "
                          (symbol-name symbol)))
        'var 'func))
   ((fboundp symbol)
    'func)
   ((boundp symbol)
    'var)
   (t
    (unless noerr
      (error "Don't know symbol type; not a variable or function %s"
             symbol)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-backward-opening-paren ()
  "Go backward until parenthesis found."
  (if (char-equal ?\( (following-char))
      (point)
    (re-search-backward "(" nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-read-symbol-at-point ()
  "Read function name around point.

o  Check if cursor is at the beginning of line whitespace
   and sees ' +(', then valuate next statement
o  Go backward to opening parenthesis and evaluate command.

Return:
 (point function-name-string statement)"
  (let ((opoint (point))
	(word   (save-excursion (tinylisp-read-word)))
	point
	func
	statement)
    (save-excursion
      (cond
       ((and (stringp word)
	     (intern-soft word))
        (skip-chars-backward "^( \t\r\n"))
       ((line-end-position) ;;move to opening paren in this line
        (re-search-backward "(" (line-beginning-position) t))
       (t
        ;;   if there is whitespace  '^      (autoload 'tinylisp-mode...'
        ;;   Then go to first opening paren in the line.
        ;;
        ;;   - there must be whitespace between bol and opoint
        ;;   - next we must see '(' in the current line (eol)
        (beginning-of-line)
        (if (not (and (re-search-forward "^[ \t]*" opoint t)
                      (re-search-forward "(" (line-end-position) t)))
            ;;  restore
            (goto-char opoint))))
      (when (and (tinylisp-backward-opening-paren)
                 (setq point (point))
                 (re-search-forward "[^ \t\n(]" nil t))

        (setq func (or word (tinylisp-read-word)))
        (goto-char point)
        (ignore-errors                  ;In comment; this breaks.
          (forward-sexp 1)
          (setq statement (buffer-substring point (point))))
        (if statement
            (list point func statement))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-package-prefix ()
  "Read function from the beginning of file and first word from the name.

    (defun XXX-do-it-like-this ()

Return:
  string    The XXX
  nil       can't find one."
  (save-excursion
    (ti::pmin)
    (if (re-search-forward "^(defun[ \t]+\\([^ \t]+-\\)" nil t)
        (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval (str1 str2 type &optional arg1 arg2 arg3)
  "Substitute STR1 with STR2 in string and eval all in temporary buffer..

If TYPE is nil
  Read string from buffer ARG1, position ARG2 and ARG3.

If TYPE is non-nil
  ARG1 contains string

References:
 `tinylisp--buffer-eval'"
  (tinylisp-with-current-buffer
   (ti::temp-buffer tinylisp--buffer-eval 'clear)
   (if type
       (insert arg1)
     (if (not (get-buffer arg1))
         (error "arg1 must be (existing) buffer")
       (insert-buffer-substring arg1 arg2 arg3)))
   (ti::pmin)
   (while (search-forward str1 nil 'noerr)
     (replace-match str2 nil t))
   (tinylisp-eval-fix-defconst)
   (tinylisp-eval-current-buffer)
;;;    (erase-buffer)                   ;May be big
   nil))

;;}}}
;;{{{ Internally used buffers

;;; --------------------------------------------------------- &buffers ---
;;;
(defun tinylisp-b-display (buffer point-min)
  "Display BUFFER (must be string) if it exists and go to optional POINT-MIN.
Shrink and print message if not exist."
  (let ((win           (get-buffer-window buffer))
	(frame-win     (get-buffer-window buffer t))
	(owin          (selected-window)))
    (if (not (buffer-live-p (get-buffer buffer)))
        (message "TinyLisp: Buffer does not exist, %s" buffer)
      ;;  Do nothing special if window is already visible
      (cond
       (win
        (when point-min
          (select-window win) (ti::pmin)
          (select-window owin)))
       (frame-win
        (raise-frame (window-frame frame-win))
        (select-window frame-win))
       (t
        (display-buffer buffer)
        (with-current-buffer buffer
          (shrink-window-if-larger-than-buffer)
          (if point-min (ti::pmin))))))))

;;; ----------------------------------------------------------------------
;;; (defun tinylisp-b-eval (&optional pmin)
;;;     (interactive) (tinylisp-b-display tinylisp--buffer-eval pmin))
;;;
;;; This is just byteComp forward declaration, kinda.

(defun tinylisp-b-record (&rest args)
  "Ignore ARGS."
  nil)

;; Real functions are defined here.
(dolist (x '("eval" "record" "variables" "funcs" "autoload"))
  (let ((sym (intern (format "tinylisp-b-%s" x)))
	(var (intern (format "tinylisp--buffer-%s" x)))
	def)
    (setq def
	  `(defun ,sym (&optional pmin)
	     (interactive "P")
	     (tinylisp-b-display ,var pmin)))
    (eval def)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-b-record-empty (&optional verb)
  "Empty buffer `tinylisp--buffer-record'. VERB."
  (interactive)
  (ti::verb)
  (if (buffer-live-p (get-buffer tinylisp--buffer-record))
      (ti::erase-buffer tinylisp--buffer-record))
  (if verb
      (message "TinyLisp: record buffer emptied.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-b-elp (&optional verb)
  "Go to Elp summary buffer. VERB."
  (interactive)
  (ti::verb)
  (if (buffer-live-p (get-buffer elp-results-buffer))
      (display-buffer elp-results-buffer)
    (if verb
        (message "TinyLisp: No Elp Profiling results buffer."))))

;;}}}
;;{{{ advice, elp

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-ad-match-1 (regexp)
  "Return '((function class name) ..) that are adviced matching NAME REGEXP."
  (let (list
	sym-name)
    (ad-do-advised-functions (advised-function)
      (dolist (class '(before after around))
        (dolist (info (ad-get-advice-info-field advised-function class))
          (setq sym-name (symbol-name (car info)))
          (when (string-match regexp sym-name)
            (push (list advised-function class (car info)) list)))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-ad-match  (regexp &optional verb)
  "Loop through adviced functions to find all that match REGEXP. VERB."
  (interactive "sAd name match Regexp: ")
  (if (ti::nil-p regexp)
      (error "Invalid regexp"))
  (let ((list (tinylisp-ad-match-1 regexp)))
    (ti::verb)
    (tinylisp-with-current-buffer
     (ti::temp-buffer tinylisp--buffer-data 'clear)
     (dolist (elt list)
       (insert
        (format
         "%-35s %-7s %s\n"
         (symbol-name (nth 0 elt))
         (symbol-name (nth 1 elt))
         (symbol-name (nth 2 elt))))))
    (when verb
      (pop-to-buffer tinylisp--buffer-data)
      (ti::pmin))))

;;}}}
;;{{{ elp

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-function-list-partial (&optional arg verb)
  "Call `tinylisp-elp-function-list'. See ARG and VERB parameters there."
  (interactive "P")
  (ti::verb)
  ;;  elp-all-instrumented-list. The
  ;;  `elp-function-list' is list of functions to profile
  (tinylisp-elp-function-list arg elp-function-list verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-function-list (arg &optional list verb)
  "Print list of functions that are currently being profiled.
If functions can fit in echo area they are printed there unless
prefix ARG is given.

LIST defaults to `elp-all-instrumented-list`. VERB."
  (interactive "P")
  (let (str)
    (ti::verb)
    (setq list (or list
                   elp-all-instrumented-list)
          str  (if list
                   (prin1-to-string list)))
    (if (null list)
        (progn
          (if verb
              (message "TinyLisp: No functions elp'd"))
          ;; function return code
          nil)
      (if (and (null arg)
               (< (length str) 80))
          (message str)
        (tinylisp-with-current-buffer
         (ti::temp-buffer tinylisp--buffer-elp 'clear)
         (dolist (elt list)
           (insert (symbol-name elt) "\n"))
         (sort-lines nil (point-min) (point-max))
         (pop-to-buffer (current-buffer))
         (message "TinyLisp: %d functions have been elp'd"
                  (length elp-all-instrumented-list))))
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-reset-after-results (&optional arg)
  "Toggle variable `elp-reset-after-results' according to ARG."
  (interactive "P")
  (ti::bool-toggle elp-reset-after-results))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-restore-all (&optional verb)
  "Remove all instrumented functions. VERB."
  (interactive)
  (ti::verb)
  (elp-restore-all)
  (if verb
      (message "TinyLisp: ELP, all functions restored.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-reset-list (&optional verb)
  "Reset timing list. VERB."
  (interactive)
  (ti::verb)
  (elp-reset-all)
  (if (get-buffer-window elp-results-buffer)
      (tinylisp-elp-results))           ;Clear the window
  (if verb
      (message "TinyLisp: ELP, Timing list cleared.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-results (&optional record string verb)
  "Show results, but do not change window.
if RECORD is non-nil; then copy t
iming to record buffer.
Insert STRING after the record stamp. VERB."
  (interactive)
  (let ((obuffer (current-buffer)))
    (ti::verb)
    (elp-results)
    (ti::pmin)
    (tinylisp-elp-summary-mode 1)
    (tinylisp-record-macro
	record
      (insert "\nELP: "  (ti::date-standard-date) " " (buffer-name)
	      (if string string "\n"))
      (insert-buffer-substring elp-results-buffer)
      (if verb
	  (message "TinyLisp: Results RECORDED.")))
    (pop-to-buffer obuffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-instrument-buffer-i-args (pfx-arg iact)
  "Ask args for `tinylisp-elp-instrument-buffer'.
PFX-ARG is usually `current-prefix-arg' if you know that already.
IACT signifies interactive spec."
  (let (pfx)
    (setq pfx
          (read-from-minibuffer
           (format
            "%sInstrument using package prefix [empty=examine functions]: "
            (if pfx-arg "Un)" ""))
           (or (tinylisp-find-package-prefix)
               "")))
    (if (ti::nil-p pfx)
        (list nil pfx-arg 'find iact)
      (list pfx pfx-arg nil iact))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-instrument-buffer (prefix &optional remove type verb)
  "Instrument all functions in the current buffer.

There are two possibilities when you run this in the buffer

o  Buffer contains a lisp package. Each function is prefixed
   with some unique identifier.

o  You're in scratch buffer or badly formed package where
   the names of the functions are not prefixed properly.

Interactive call note:

   The choice how to instrument functions is asked.

Input:

   PREFIX   can be nil if type is non-nil.
   REMOVE   uninstrument functions. (Interactive call's prefix arg)
   TYPE     if nil then instrument using PREFIX
            if non-nil, force finding all function names and
            instrument them. This uses `defun' keyword seach.
   VERB     verbose mode"
  (interactive
   (tinylisp-elp-instrument-buffer-i-args current-prefix-arg 'iact))

  (let ((str (if remove "un" ""))
	(count 0)
	list)
    (ti::verb)
    (cond
     (type
      (setq list (tinylisp-find-function-list 'no-show 'alternative))
      (if (null list)
          (if verb
              (message "TinyLisp: Can't find functions from buffer"))
        (let (type)
          (dolist (func list)
            (setq type (car func))
            (setq func (cdr func))      ;("defun" . "t1")
            ;;  elp can only insrument functions
            (when (string-match "defun\\|defsubst" type)
              (incf count)
              (tinylisp-symbol-do-macro func nil
		(elp-restore-function func) ;do this first
		(if (null remove)
		    (elp-instrument-function func))))))
        (if verb
            (message "TinyLisp: %sinstrumented %d functions" str count))))
     (t
      (if remove
          (elp-restore-all)
        (elp-instrument-package prefix))
      (if verb (message
                "\
TinyLisp: %sinstrumented package '%s'. Count of functions is unknown."
                str prefix))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-instrument-function ()
  "Instrument current function. Search the function name."
  (interactive)
  (let ((func (ti::buffer-defun-function-name)))
    (if (not func)
        (message "TinyLisp: Can't find function name.")
      ;;  This evaluates the function prior elp'ing it.
      (tinylisp-eval-at-point)
      (tinylisp-symbol-do-macro func nil
	(elp-instrument-function func))
      (message (format "TinyLisp: ELP instrumented [%s]" func)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-mapsym (regexp &optional not-regexp real-name)
  "Return list of function matching REGEXP NOT-REGEXP REAL-NAME.
See `tinylisp-elp-instrument-by-regexp'."
  (let (list
	name
	real)
    (mapatoms
     (function
      (lambda (sym)
        (when (fboundp sym)
          ;;  What's the real function?
          (setq real (or (ti::defalias-p sym) sym))
          (when (not (memq (car-safe (symbol-function real))
                           '(autoload macro)))
            (if real-name
                (setq sym real))      ;yes this is real function name.
            (setq name (symbol-name sym))
            (when (and (string-match regexp name)
                       (not (string-match "ad-Orig-" name))
                       ;;  Don't instrument adviced functions
                       (or (not (featurep 'advice))
                           ;;  real an sym must hnot have any advice active
                           (cond
                            ((ad-has-any-advice real)
                             (not (ad-is-active real)))
                            ((ad-has-any-advice sym)
                             (not (ad-is-active sym)))
                            (t          ;Okay, no advice
                             t)))
                       (or (not (stringp not-regexp))
                           (not (string-match not-regexp name))))
              (push sym list)))))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-instrument-by-regexp
  (regexp &optional not-regexp real-name uninstrument verb)
  "Elp all functions that match REGEXP and NOT-REGEXP.
Note, calling this function is slow, because it will map
through every single defined atom in Emacs. (there are thousands).

Note:

  Adviced functions starting with `ad-' are not instrumented.

Input:

  REGEXP        Regexp to match functon name
  NOT-REGEXP    If REGEXP matches, function must not match this. If nil,
                then only REGEXP is used.
  REAL-NAME     If non-nil then look function name behind
                defalias statements. After we get non-alias name the REGEXP
                is matched.
  UNINSTRUMENT  Flag, if non-nil. Do the opposite: Uninstrument functions.
                This is the prefix argument.
  VERB          Verbose message."
  (interactive
   (list
    (read-string
     (if current-prefix-arg
         "Elp uninstrument Regexp: "
       "Elp Regexp: ")
     nil  'tinylisp--elp-regexp-history)
    (read-string "Not Regexp: " nil  'tinylisp--elp-not-regexp-history)
    (y-or-n-p "Match against real names? (look under alias name) ")
    current-prefix-arg))
  (ti::verb)

  (if (ti::nil-p not-regexp)         ;It's "" after RET in interactive
      (setq not-regexp nil))

  (let ((list (tinylisp-elp-mapsym regexp not-regexp real-name))
	(msg  (if uninstrument
		  "un"
		"")))
    (if uninstrument
        (elp-restore-list list)
      (elp-instrument-list list))
    (if verb
        (message "TinyLisp: %d functions %sinstrumented"
                 (length list) msg))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-set-master  (function)
  "Set master FUNCTION."
  (interactive
   (list
    (intern
     (completing-read
      "Master function: "
      obarray
      'fboundp
      'match
      nil
      'tinylisp--elp-master-history))))
  (elp-set-master function))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-restore-buffer ()
  "Read functions from the buffer and cancel elp for them."
  (interactive)
  (let ((args (tinylisp-elp-instrument-buffer-i-args 'pfx 'iact)))
    (tinylisp-elp-instrument-buffer
     (nth 0 args)
     (nth 1 args)
     (nth 2 args)
     (nth 3 args))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-restore-function ()
  "Remove elp code from current function. Search the function name."
  (interactive)
  (let ((func (ti::buffer-defun-function-name)))
    (if (not func)
        (message "TinyLisp: ELP,  Can't find function name.")
      (tinylisp-symbol-do-macro func nil
	(elp-restore-function func))
      (message (format "TinyLisp: ELP, restored [%s]" func)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-reparse-instrumentation (&optional verb)
  "Uninstrument all currently instrumented functions.
Then eval current buffer (to get new function definitions) and last instrument
all found functions in the buffer.

In short: remove previous instrumentation and do new one. VERB."
  (interactive)
  (ti::verb)
  (tinylisp-elp-restore-all)
  (tinylisp-eval-current-buffer)
  (tinylisp-elp-instrument-buffer nil nil 'find verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-harness (&optional count verb)
  "Call elp multiple times to get reliable results.
Default is call count is 3, but you can supply numeric prefix COUNT. VERB.

   1. Use empty buffer
   2. Write test functions with t-*
   3. Evaluate buffer (instantiate functions) M-x eval-current-buffer
   4. Instrument test functions: $ e I t- RET
   5. Run harness: $ e I H

   ** Remember, you must have instrumented the functions before
   ** you call this harness function

This is bit exotic function and it requires that you have written
following a test setup smilar to one in the clear Lisp buffer,
like *scratch*.

  ;; Let's say we're interested if 'let*' is slower
  ;; that 'let'.

  (defun t-1 () (let* () ))
  (defun t-2 () (let  () ))
  (defun t-3 () )

  ;; The trick here is that when you instrument whole
  ;; buffer and eval all the functions ($ -),
  ;; the form below is bypassed
  ;;
  ;; The variable tinylisp--harness-flag is set to t during harness test.

  ;; -!- The cursor is blinking here

  (when tinylisp--harness-flag
    (dotimes (i 500)
      (t-1)
      (t-2)
      (t-3)))

This function evaluates everything from current point forward ARG
times. If there is word `tinylisp--harness-flag' forward in the
buffer, the `current-point' is not used but the evaluating is
started from the beginning of `tinylisp--harness-flag' line
forward.

After each evaluating round the elp results are recorded to
`tinylisp--buffer-record'.

In the above setup, this means that the test below
`tinylisp--harness-flag' is repeated 3 times to get 3 elp timing
results. Since using elp only once for small functions, doesn't
give reliable results; we have to repeat the test at least 3
times.

The `tinylisp--buffer-record' buffer is displayed after the
harness run is over."
  (interactive "P")
  (let (case-fold-search
        beg
        h-found)
    (ti::verb)
    (setq count (or count 3))
    ;;  See if there this word in the buffer
    (save-excursion
      (ti::pmin)
      (when (re-search-forward "tinylisp--harness-flag" nil t)
        (setq beg (line-beginning-position))
	(setq h-found t)))
    (or beg                             ;we already found it
        (setq beg (point)))             ;nope, use current point
    (if (null elp-all-instrumented-list)
        (error "No functions in elp list"))
    (if (and verb
             (null
              (y-or-n-p
               (format
                (if h-found
                    "tinylisp--harness-flag %s times, ok? "
                  "Harness %s times, from current point forward, ok? ")
                count))))
        (error "Abort."))
    (if (and verb
             (y-or-n-p "Do you want to clear RECORD buffer first? "))
        (tinylisp-b-record-empty))
    (unwind-protect ;; make sure tinylisp--harness-flag is set to nil
        (let ((rounds count))
          (setq tinylisp--harness-flag t)
          (dotimes (iterator count)
            (tinylisp-elp-reset-list)   ;wipe timings
            (if verb (message "TinyLisp:  Eval round %d/%d ... "
                              (1+ iterator) rounds))
            (eval-region beg (point-max))
            (tinylisp-elp-results
             'record (format " -- %d/%d\n" (1+ iterator) rounds)))
          (if verb
              (message "TinyLisp: Eval rounds done."))
          (tinylisp-b-record 'pmin))
      (setq tinylisp--harness-flag nil))))

;;}}}
;;{{{ elp results

;;; ----------------------------------------------------------------------
;;;
(dolist (x '(1 2 3 4 5 6 7 8 9)) ;; Can't use dotimes which starts from 0
  (let ((sym (intern (format "tinylisp-elp-summary-sort-column-%d" x)))
	def)
    (setq def
	  `(defun ,sym (&optional arg)
;;;              "Sort by field. ARG to reverse sort."
	     (interactive "P")
	     (tinylisp-elp-summary-sort-column ,x arg)))
    (eval def)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-summary-sort-column (nbr &optional reverse)
  "Sort column NBR or REVERSE."
  ;; Nope...
  ;; (setq nbr (if reverse (- nbr) nbr))
  (untabify (point-min) (point-max))
  (ti::save-with-marker-macro
    (ti::pmin)
    (forward-line 2)                    ;Skip header.
    (cond
     ((memq nbr '(2 3 4))
      (sort-numeric-fields nbr (point) (point-max)))
     (t
      (sort-fields nbr (point) (point-max))))))

;;}}}
;;{{{ code help: debug, find-error

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-debug-add-tags (&optional remove verb)
  "Add simple debug code before every left flushed parenthesis. REMOVE. VERB.
When you compile a file, sometimes it is very hard to find the error
position from the output; which gives no further clues:

    While compiling toplevel forms in file xxx.el:
      !! Wrong type argument ((number-or-marker-p nil))
    Done

The funny thing might be that this happens only when file is compiled. By
evaluating each piece of code with `eval-region' the error does not occur.
To help spotting the place, this function inserts random tags in the buffer
which are shown during compilation. DO NOT change the inserted tags. After
you have corrected errors, you can REMOVE the extra debug tags with prefix
argument.

DebugTag: 21-56 file.el
      !! Wrong type argument ((number-or-marker-p nil))
DebugTag: 22-56 file.el
..."
  (interactive "*P")
  (let* ((tag    ";;__LISP-DEBUG__")
         (fmt    (concat
                  "  (eval-and-compile "
                  "(message \"DebugTag: %d-%d %s\"))"))
         (re     (regexp-quote tag))
         (i      0)
         ;;  We have to randomize the tag, because suppose
         ;;  - user inserts tags. He runs debug and doesn find the spor
         ;;  - He left flushed more code
         ;;  - He inserts tags again, but because there is already tags,
         ;;    the _new_ tags must be different ==> randomized tags.
         (rand   (rand1 100))
         (name   (buffer-name)))
    (ti::verb)
    (save-excursion
      (ti::pmin)
      (if remove
          (while (re-search-forward tag nil t)
            (if verb (message "TinyLisp:  uninstrumenting tag %d" i))
            (incf  i)
            (beginning-of-line)
            (kill-line 1))
        (when (or (null (re-search-forward tag nil t))
                  (y-or-n-p
                   "TinyLisp: Debug tags already instrumented. Proceed? "))
          (setq re (concat ".*" re))
          (while (re-search-forward "^(" nil t)

            (ti::save-with-marker-macro
              (beginning-of-line)
              (unless (looking-at re)
                (insert (format fmt i rand name))
                (insert tag "\n") ))
            (forward-line 1)
            (if verb
                (message "TinyLisp:  instrumenting tag %d" i))
            (incf   i)))))
    (when (and verb (not (zerop i)))
      (if remove
          (message "TinyLisp: Debug tags removed.")
        (message "TinyLisp: %d Debug tags inserted." i)))))

;;; ----------------------------------------------------------------------
;;; Simple solution
;;;
(defun tinylisp-error-find-2 ()
  "Start from point min and Eval region at time until error occurs."
  (interactive)
  (let ((point -1)
	(opoint (point))
	last-p)
    (ti::pmin)
    (setq last-p (point))
    (while (not (eq point (point)))
      (setq point (point))
      (eval-region last-p (point))
      (setq last-p (point))
      (end-of-defun))
    ;; The while loop never finishes if there was error
    (message "TinyLisp: No lisp errors found.")
    (goto-char opoint)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-find-1 ()
  "Find code error position and put point near the error."
  (interactive)
  (let ((lower-bound 1))
    (setq tinylisp--find-error nil)
    (save-excursion
      (let (half
            (low 1)
            (high (tinylisp-error-count-sexps)))
        (if tinylisp--find-error       ;See tinylisp-error-count-sexps
            (setq lower-bound (point))
          (setq high (1+ high))
          (while (< low high)
            (if (tinylisp-error-try-parse lower-bound
                                          (tinylisp-error-sexp-position
                                           (setq half (/ (+ low high) 2))))
                (progn (setq low (1+ half))
                       (forward-sexp 2)
                       (backward-sexp)
                       (while (not (bolp))
                         (backward-sexp))
                       (setq lower-bound (point)))
              (setq high half)))
          (backward-sexp)
          (setq lower-bound (point)))))

    (if (not tinylisp--find-error)
        (message "TinyLisp: No errors found.")
      (goto-char lower-bound)
      (message "TinyLisp: %s" tinylisp--find-error))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-try-parse (from to)
  "Eval regions and try to find error in FROM TO."
  (condition-case err
      (progn (eval-region from to) t)
    (error
     (progn
       (setq tinylisp--find-error err)
       nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-count-sexps ()
  "Eval regions and try to find error."
  (goto-char (point-max))
  (condition-case err
      (let ((n 0))
        (while (not (bobp))
          (backward-sexp)
          (setq n (1+ n)))
        n)
    (error (setq tinylisp--find-error err))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-sexp-position (n)
  "Find sexp N."
  (goto-char 1)
  (forward-sexp n)
  (if (or (not (eobp))
          (save-excursion
            (goto-char 1)
            (forward-sexp (1- n))
            (skip-chars-forward " \t\n")
            (not (eobp))))
      (backward-sexp))
  (point))

;;}}}
;;{{{ code help: jump, eval

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-symbol-file-location (symbol)
  "Search SYMBOL from Emacs obarrays and try to find file location."
  (when symbol
    (or (ti::system-load-history-where-is-source symbol)
        (ti::system-doc-where-is-source symbol)
        (and (ti::autoload-p symbol)
             (let ((lib (ti::autoload-file symbol)))
               (if lib
                   (locate-library lib)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-jump-to-definition (&optional save word verb nodisplay)
  "Search function or variable definition in the same file or from outside.
This function relies on the `load-history' and if there is no such
symbol, this function can't jump to definition. If you have evaled buffer
or function inside Emacs and not used the load* commands then the
definition information is not in `load-history'.

Input:

SAVE        \\[universal-argument]: then save the point so
            that you can build call-chain and use
            \\[tinylisp-back-to-definition] to return this point.

            non-nil: then clear the call chain, save point, and jump to
            definition. This lets you start building call chain again.

WORD        String definition to search (defvar, defconst, defun ...).

VERB        Flag. Allows displaying verbose messages.

NODISPLAY   Flag. If non-nil, don't display the found point.

References:

  `tinylisp--call-chain'"

  (interactive (list current-prefix-arg
                     (tinylisp-read-word)))

  (let ((f-re
	 (concat "^(\\(defun\\*?\\|defmacro\\*?\\|defsubst\\|deffoo"
		 "\\|defun-maybe\\|defsubst-maybe"
		 "\\|define-derived-mode\\|define-minor-mode"
		 "\\|defalias\\|fset"
		 ;;  See grep.el::define-compilation-mode
		 "\\|define-[^ \t\r\n]+-mode"
		 "\\)[ \t']+%s[ \t\r\n]"))
	(v-re
	 "^(\\(defvar\\|defconst\\|defcustom\\|defvoo\\)[ \t]+%s[ \t\r\n]")
	(reg  tinylisp--register)
	(call-chain-data (point-marker))
	re
	type
	point
	file
	sym
	alias
	buffer)
    (ti::verb)
    (if (ti::nil-p word)
        (error "TinyLisp: searched WORD is nil.")
      (when (setq sym (intern-soft word))
        (when (setq alias (ti::defalias-p sym))
          (message "TinyLisp: Symbol `%s `==> alias to `%s'" sym alias)
          (setq sym alias))
        (setq type (tinylisp-symbol-type sym 'noerr)))
      ;; ..................................... Search from this buffer ...
      (save-excursion
        (ti::pmin)
        (let ((function (if alias
                            (symbol-name alias)
                          word)))
          (cond
           ((eq type 'func)
            (setq re (format f-re function)))
           ((eq type 'var)
            (setq re (format v-re function)))
           (t
            ;;  since the symbol is not defined in Emacs we can't
            ;;  know which one to search, variable or function.
            ;;  Try anything.
            (setq re (concat
                      ;;  This could also be and alias, like
                      ;;  used in many Gnus files.
                      (format f-re function)
                      "\\|"
                      (format v-re word)))))
          (when (re-search-forward re nil t)
            (setq buffer (current-buffer))
            (setq point (line-beginning-position)))))
      ;;  If the definition is not in current buffer where user is,
      ;;  Then try to search somewhere else.
      (when (and sym
                 (null point))
        (setq file (tinylisp-symbol-file-location sym)))
      ;; Still no luck? Loosen the REGEXP so that do not require the
      ;; function to be to the left "^", but allow adding spaces, like in:
      ;;
      ;; (eval-and-compile
      ;;    (defun this-here ()
      ;;      ...
      (unless (or point file)
        (setq re (concat (format (substring f-re 1) word)
                         "\\|"
                         (format (substring v-re 1) word)))
        (save-excursion
          (ti::pmin)
          (when (re-search-forward re nil t)
            (setq buffer (current-buffer))
            (setq point (line-beginning-position)))))
      (cond
       ;; ............................................... check intern ...
       ((when (and (null point)
                   (null (intern-soft word)))
          (message "TinyLisp: Can't find definition for %s (undef)" word)))
       ;; .................................................... external ...
       ((and (null point) ;; See re-search above which set the point
             (null file)
             (or alias sym)
             (ti::subrp-p (or alias sym)))
        (if (and alias
                 (not (eq alias sym)))
            (message
             "TinyLisp: alias `%s' => `%s' points to built-in function."
             word
             (symbol-name alias))
          (message
           "TinyLisp: `%s' is built-in function." word)))
       ((and (null point) ;; See re-search above which set the point
             (null file))
        ;; Can't find from this file, does load history entry say
        ;; from which file it was loaded ?
        (message
         "TinyLisp: Can't find `load-history' definition for %s" word))
       ((stringp file)
        (unless (ti::file-name-path-p file)
          (error
           "TinyLisp: Couldn't find absolute path %s %s. Contact maintainer"
           sym file))
        (when (string-match "\\(.*\\.el\\)c" file)
          (setq file (match-string 1 file))
	  (let (try)
	    (catch 'done
	      ;; Compressed lisp file
	      (dolist (ext '(".gz" ".bz2" ".lzma"))
		(setq try (format "%s%s" file ext))
		(if (file-exists-p try)
		    (throw 'done (setq file try))))))
          (unless (file-exists-p file)
            (error "TinyLisp: There is only compiled file at %s" file)))
        (when (or (find-buffer-visiting file) ;Already loaded?
                  (null verb)
                  (y-or-n-p (format "TinyLisp: Go to: %s ? " file)))
          (unless (string-match "\\.el\\($\\|\\.\\)" file)  ; .el.gz
            (setq file (concat file ".el")))
          (unless (ti::file-name-path-absolute-p file)
            (let ((path (locate-library file)))
              (if path
                  (setq file path))))
          (unless (file-exists-p file)
            (error "Tinylisp: cannot find file %s" file))
          (setq buffer (tinylisp-find-file-noselect file))
          (with-current-buffer buffer
            (setq point (point))
            (ti::pmin)
            (ti::buffer-outline-widen)
            (if (re-search-forward re nil t)
                (setq point (point))
              (goto-char point)         ;back to original position
              (setq point nil)          ;Clear flag
              (message "TinyLisp: Strange... cant't find definition: %s"
                       word)
	      ;;  Try approximation: "(WORD"  or "(setq WORD ...)"
	      (let ((re (format "(%s\\>\\|\\<%s\\>" word word)))
		(if (re-search-forward re nil t)
		    (goto-char (match-beginning 0))))
              (sit-for 2))
            (when save
              (if (and save (not (equal save '(4))))
                  (setq tinylisp--call-chain nil)
                (tinylisp-push-call-chain nil call-chain-data verb)
                (if verb
                    (message
                     "TinyLisp: Call chain %d"
                     (length tinylisp--call-chain)))))))
        (when (null file)
          ;;  No load-history so try searching all buffers in Emacs
          (setq buffer nil)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (re-search-forward re nil t)
                (setq buffer (current-buffer))
                (setq point  (line-beginning-position))
                (return))))))
       ;; ....................................................... other ...
       (point ;; point is set
        (when save
          (if (and save (not (equal save '(4))))
              (setq tinylisp--call-chain nil)
            (tinylisp-push-call-chain nil call-chain-data verb)
            (if verb
                (message "TinyLisp: Call chain %d"
                         (length tinylisp--call-chain)))))
        (point-to-register reg)
        (goto-char point)
        (when (null type)
          (message "TinyLisp: Warning, this symbol is not in obarray.")
          (sit-for 1))
        (tinylisp-show-register-message))))
    ;; ........................................... display found point ...
    (when (and buffer
               (not nodisplay)
               (not (eq buffer (current-buffer))))
      (ti::pop-to-buffer-or-window buffer point))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-back-to-definition ()
  "Jump back to last call chain point in `tinylisp--call-chain'."
  (interactive)
  (tinylisp-push-call-chain 'pop)
  (message "TinyLisp:  Call chain %d" (length tinylisp--call-chain)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-jump-to-definition-chain (&optional verb)
  "Save position to call chain and jump to definition.
See `tinylisp-jump-to-definition'. VERB."
  (interactive)
  (ti::verb)
  (tinylisp-jump-to-definition '(4) (tinylisp-read-word) verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-backward-user-option ()
  "See `tinylisp-forward-user-option'."
  (interactive)
  (tinylisp-forward-user-option 'back (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-forward-user-option (&optional back verb)
  "Search forward or BACK a user variable or user callable function. VERB."
  (interactive
   (list
    current-prefix-arg
    'interactive))
  (let ((opoint (point))
	type
	sym
	point
	beg
	end)
    (while (and (null point)
                (prog1 (setq beg (if back
                                     (tinylisp-forward-def 'back)
                                   (tinylisp-forward-def)))
                  (unless beg
                    (if verb
			(message "TinyLisp: No more user options.")))))
      (cond
       ((looking-at tinylisp--regexp-variable)
        (setq type (match-string 1)
              sym  (intern-soft (match-string 2))))
       ((looking-at "^(defun[ \t]+\\([^ \t]+\\)")
        (setq type "defun"
              sym  (intern-soft (match-string 1)))))
      ;; ..................................................... examine ...
      ;; Okay we're somewhere at the beginning of variable of
      (cond
       ((looking-at "defcustom")        ;Yes, this is user variable
        (setq point (point)))
       ((and sym                        ;Is this sym _defined_ ?
             (or
              (and (not (string-match "defun" type))
                   (boundp sym)         ;Then check is easy
                   (user-variable-p sym))
              (and (string-match "defun" type)
                   (fboundp sym)
                   (commandp sym))))
        (setq point (point)))
       (t
        ;; ................................................ not loaded ...
        ;; package is not loaded into memory, we may be looking at
        ;; varible or function. Determine var/func region first.

        (setq beg (point))
        (setq end (save-excursion
                    (beginning-of-line)
                    (forward-sexp 1) (point)))
        (beginning-of-line)
        ;;  This fails only if variable docs at flushed left, but
        ;;  then you don't follow guidelines...
        ;;
        ;;  (defvar nil
        ;;  "*docs"
        ;;
        (if (if (looking-at "^(defun")
                (re-search-forward "(interactive[) ]" end t)
              (re-search-forward "^[ \t]+\"\\*" end t)) ;It's variable
            (setq point beg))))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . continue  ..
      (if beg
          (goto-char beg))
      ;;  Reset round
      (setq sym  nil
            type nil
            beg  nil
            end  nil))
    (unless point
      (goto-char opoint)
      (if verb
          (message "TinyLisp: no more user variables or functions.")))
    point))

;;; ----------------------------------------------------------------------
;;;
;;;  You can also do this in program code like this.
;;;
;;;  (fset 'test
;;;    (byte-compile-sexp
;;;      (lambda () nil)))
;;;
(defun tinylisp-byte-compile-sexp (&optional disassemble verb)
  "Byte compile function around point.
If you give prefix argument DISASSEMBLE, then the function is also
disassembled to byte code format. VERB."
  (interactive "P")
  (let ((debug-on-error t)
	name)
    (ti::verb)
    (tinylisp-defun-macro
     (setq name (ti::string-match "def[a-zA-Z]+ +\\([^() \t\n\]+\\)" 1 str))
     (cond
      ((not (stringp name))
       (if verb
           (message "TinyLisp:No sexp to compile here...")))
      ((null (intern-soft name))
       (if verb
           (message "TinyLisp:%s is not interned symbol." name)))
      ((null (fboundp (setq name (intern name))))
       (if verb
           (message "TinyLisp:%s is not a function name." name)))
      (disassemble
       (disassemble name))
      (t
       (byte-compile name)
       (if verb
           (message "TinyLisp: byte compiled [%s]" name)))))))

;;; ----------------------------------------------------------------------
;;; #todo: how do you detect the emacs binary used ?
;;; #todo: unfinished
;;;
(defun tinylisp-byte-compile-buffer ()
  "Compile current buffer as if Emacs were newer loaded.
Since your current Emacs has already loaded packages, it's not
wise to compile using `byte-compile-file'.

Instead we cal anmother copy of Emacs to do the compilation so that
you would catch any errors with undefined variables and functions."
  (interactive)
  (let ((byte-compile-generate-call-tree nil)
         (file  (buffer-file-name)))
    (if (null file)
        (message "TinyLisp: Buffer %s is not visiting file." (buffer-name))
      (call-interactively 'byte-compile-file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-byte-compile-display-call-tree ()
  "See bytecomp.el `display-call-tree'."
  (interactive)
  (let ((byte-compile-call-tree-sort 'calls+callers)
	(byte-compile-generate-call-tree  t)
	(file (buffer-file-name)))
    (if (null file)
        (message (concat "TinyLisp: Buffer %s is not visiting file."
                         " Cannot display call tree.")
                 (buffer-name))
      (call-interactively 'byte-compile-file)
      (let ((buffer (get-buffer "*Call-Tree*")))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let (buffer-read-only)
              (save-excursion
                (ti::pmax)
                (insert "

** TinyLisp: [NOTE] 'Noninteractive functions not known to be called' usually
means that the functions were declared defsubst.\n"))
              buffer)))))))

;;; ----------------------------------------------------------------------
;;; #todo:
(defun tinylisp-byte-compile-parse-needed-packages ()
  "Byte Compile file and check what packages it needs.
With this function you can find out what other packages are needed to
run a file."
  (interactive)
  (let ((buffer (tinylisp-byte-compile-display-call-tree)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-set-value-at-point (&optional arg)
  "Read word under point and if it's variable, ask new value for it.
ARG can be
 \\[universal-argument]  'restore variable's content
 \\[universal-argument]\\[universal-argument]  'backup variable's value"
  (interactive "P")
  (let ((var  (tinylisp-read-word))
	(cmd  (cond
	       ((equal arg '(4))  'restore)
	       ((equal arg '(16)) 'bup))) ;Back it up
	val)
    (if (ti::nil-p var)
        (message "TinyLisp: Couldn't read variable at point")
      (tinylisp-symbol-do-macro var 'noerr
	(if (not (boundp var))
	    (message "TinyLisp: There is no %s variable" (symbol-name var))
	  (unless (or (eq cmd 'bup) (memq 'original (symbol-plist var)))
	    (put var 'original (symbol-value var)))
	  (cond
	   ((eq cmd 'restore)
	    (set var (get var 'original))
	    (message
	     "TinyLisp:%s restored to original value" (symbol-name var)))
	   (t
	    (setq val
		  (read-from-minibuffer
		   (format "Set %s to lisp expression: " (symbol-name var))
		   (prin1-to-string (symbol-value var))))

	    (setq val (read val)) ;Convert to lisp
	    (set var val))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-call-at-point (&optional record)
  "Call object at point.
If prefix arg RECORD is given, the content of the variable
is appended to record buffer.

- If read object is not in obarray, do nothing.
- If it is function; ask what to do
  Show symol-function, so that you can tell if it is byte compiled.
  Call it, possibly interactively
- If it's variable, eval it, possibly yielding the content."
  (interactive "P")
  (let ((str (tinylisp-read-word))
	sym
	type)
    (if (or (ti::nil-p str)
            (null (setq sym (intern-soft str))))
        (message "TinyLisp: Can't use word to eval (void?): %s "
                 (or str "<no word read>" ))
      (if (and (fboundp sym)
               (boundp sym))
          (if (y-or-n-p (format
                         "Which %s eval: Y = variable, N = function " str))
              (setq type 'var)
            (setq type 'func)))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . func type ..
      (cond
       ((or (eq type 'func)
            (and (eq type nil)
                 (fboundp sym)))
        (cond
         ((null (y-or-n-p "Y = Next choice, N = see symbol-function "))
          ;;  We can't use `message', because it would go nuts; eg if
          ;;  function would contain "%" which are formatting directives
          (pop-to-buffer (ti::temp-buffer tinylisp--buffer-macro 'clear))
          (insert (pp (symbol-function sym)))
          (ti::pmin))
         (t
          (if (and (commandp sym)
                   (y-or-n-p (format "Call interactively '%s' " str)))
              (call-interactively sym)
            (setq str (ti::function-args-p 'tinylisp-find-function-list)))
          (cond
           ((or (ti::nil-p str)
                (y-or-n-p
                 (format "Seems to need args %s; call anyway? " str)))
            (setq str (funcall sym))
            (message "TinyLisp: function returned: %s"
                     (prin1-to-string str)))))))
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . var type  ..
       ((or (eq type 'var)
            (and (eq type nil)
                 (boundp sym)))
        (setq str (prin1-to-string (eval sym)))
        (tinylisp-record-macro record
                               (insert "\n" (symbol-name sym) ":\n" str)
                               (message "TinyLisp: Content of variable recorded.")
                               (sit-for 1))
        (setq str (ti::remove-properties str))
        (if (< (length str) 73)
            (message (format "TinyLisp: %s => %s"  (symbol-name sym) str))
          (tinylisp-with-current-buffer
           (tinylisp-get-buffer-create tinylisp--buffer-macro)
           (let ((win (get-buffer-window (current-buffer)))
                 (str (pp (symbol-value sym))))
             (display-buffer (current-buffer))
             (ti::pmax)
             ;; Record this to *Message* buffer too as what we did
             ;; if the content fit the screen (size 73)
             ;; User can copy paste the results from Message bufer
             ;; if needed later
             (message (format "%s => %s" (symbol-name sym) str))
             (ti::save-with-marker-macro
               (insert "\n" (symbol-name sym) " =>\n" str))
             (set-window-point (get-buffer-window (current-buffer)) (point))
             ;; If window was not previously visible, resize the content.
             ;; If the buffer was visible, let it alone, perhaps
             ;; user wants to keep the size as it.
             (unless win
               (shrink-window-if-larger-than-buffer))))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-at-point ()
  "Evaluate variable or function around point.

Note:

  The definition must be written like this

        (defvar , (defconst , (defun ..

  And there must be no spaces after the opening parenthesis. The following
  statement is not recognised

        (  defvar

defcustom note:

  When we evaluate defcustom variable, we don't actually evaluate statement
  as is, but pretend that the defcustom is read like 'defconst'. this has the
  effect of setting new value for the variable. If you really want to
  evaluate variable as it stand there: as defcustom, you have to put cursor
  manually behind the definition and call \\[eval-last-sexp]. In this case
  defcustom treats the variable as `defvar' and only defcustom properties are
  touched.

  DANGER:

  When you evaluate `defcustom` variable with this function, be very careful
  that you have written it correctly, so that you won't get thrown out to
  error. If this happens, you're in BIG TROUBLE; repeat ; BE ON YOUR TOES
  and think carefully your next move.

  An error condition prevented restoring an advice that was enabled for
  special form 'defconst' during the evaluation of `defcustom' definition.
  The advice is still in effect and you should immediately disable it
  before you do anything else.

  Call \\[tinylisp-emergency] NOW! After that things are back to normal.
  and you can continue as usual."
  (interactive)
  (let ((debug-on-error t))            ;Make sure this is on!
    (tinylisp-defun-macro
     ;;  We handle defvar as defconst so that new value takes in
     ;;  effect.
     (cond
      ((string-match "defcustom" str)
       (tinylisp-defcustom-macro
        (tinylisp-eval "defcustom" "defconst" nil buffer beg end)))
      ((string-match "defvar" str)
       (tinylisp-eval "defvar" "defconst" nil buffer beg end))
      (t
       (eval-last-sexp nil)))
     (message (concat "TinyLisp: evaled " (or str "<nothing>"))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-fix-defconst ()
  "Fix defconst that has no argument.

    (defvar var)    ;; valid,

When converted

    (defconst var)  ;; invalid

The defconst must have initial value: we supply 'nil"
  (ti::pmin)
  (while (re-search-forward "^(defconst[ \t]+[^ \t]+\\([ \t]\\)*)" nil t)
    (backward-char 1)
    (insert " nil")
    (end-of-line)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-print-last-sexp  ()
  "Like `eval-print-last-sexp', but print --> at front."
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (eval-last-sexp t))
  (save-excursion
    (beginning-of-line)
    (insert "--> ")
    (end-of-line)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-current-buffer-defconst ()
  "Eval buffer as defconst and print message."
  (interactive)
  (let ((obuffer   (current-buffer))
	(name      (buffer-name))
	(beg       (point-min))        ;maybe narrowed?
	(end       (point-max)))
    (tinylisp-with-current-buffer
	(ti::temp-buffer tinylisp--buffer-tmp 'clear)
      (insert-buffer-substring obuffer beg end)
      (ti::pmin)
      (while (re-search-forward "^(defvar \\|^(defcustom " nil t)
	(replace-match "(defconst "))
      ;; We have to do another sweep
      (tinylisp-eval-fix-defconst)
      (tinylisp-defcustom-macro (tinylisp-eval-current-buffer)))
    (message "TinyLisp: ok, evaled buffer %s as defconst." name)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-current-buffer-from-file ()
  "Evaluate buffer by doing `load-file' from disk.
This effectively stored the function and variable definitions
to `load-history'.

If current buffer has no file, call `tinylisp-eval-current-buffer'."
  (interactive)
  (cond
   ((null buffer-file-name)
    (tinylisp-eval-current-buffer))
   (t
    (if (and (buffer-modified-p)
             (y-or-n-p "Save before loading? "))
        (save-buffer))
    ;; `load' prints message for user
    (load buffer-file-name))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-current-buffer ()
  "Eval buffer and print message."
  (interactive)
  ;;  This silences byte compiler
  (if (fboundp 'eval-buffer)
      (ti::funcall 'eval-buffer)        ;XEmacs
    (ti::funcall 'eval-current-buffer))
  (message "TinyLisp: ok, evaled buffer %s" (buffer-name)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-reverse ()
  "Search backward for opening parenthesis and Reverse the statement.
See variable `tinylisp--table-reverse-eval-alist'"
  (interactive)
  (let ((stat  (tinylisp-read-symbol-at-point))
	(table tinylisp--table-reverse-eval-alist)
	func
	str1
	str2
	statement)
    (if (or (null stat)
            (ti::nil-p (setq func (nth 1 stat))))
        (message "TinyLisp: Can't find function around point.")
      (tinylisp-symbol-do-macro
	  func 'noerr
	(setq str1 (symbol-name func))
	(if (null (setq func (cdr-safe (assq func table))))
	    (message "TinyLisp: Can't find reverse command for %s" str1)
	  (setq str2 (symbol-name func)
		statement (nth 2 stat))
	  ;; Do some special handling, e.g. add hook may have
	  ;; additional argument 'add , remove it.
	  (when (string-match "add-hook +[^ ]+ +[^ ]+\\( +[^ )]+\\))"
			      statement)
	    (setq statement (ti::replace-match 1 "" statement)))

	  (tinylisp-eval str1 str2 'string statement)
	  (message "TinyLisp: evaled as %s" str2))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-eval-edit ()
  "Read current line and allow editing the statement before evaling it."
  (interactive)
  (let ((line (ti::string-remove-whitespace (ti::read-current-line)))
	ret)
    (setq ret (eval (read (read-from-minibuffer "tinylisp-Eval: " line))))
    (message "TinyLisp: returned: %s" (prin1-to-string ret))))

;;}}}
;;{{{ code help: functions and variables

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-function-list-occur ()
  "Run occur to find functions from whole buffer."
  (interactive)
  (ti::occur-macro tinylisp--regexp-function nil
    (ti::text-re-search-forward "(defmacro")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-function-list (&optional no-show mode)
  "Find functions from buffer (macros too).

Output line format:

  [DEF][!?]   FUNCTION-NAME  INTERACTIVE-SPEC

  The DEF can defmacro, defun and defsubst.

  [!] If you see exclamation mark then it means that you have mixed
  defsubst and interactive function, which is very dangerous situation,
  because when function is in-lined the (interactive-p) tests from functions
  are in-lined too. Check that you really want to do in-lining for
  interactive functions.

  [?]Question mark means that the function does not exist in obarray
  and the possible interactive property is unknown.

Input:

  NO-SHOW   if non-nil, then the result buffer is not shown.
  MODE      if 'alternative then if there are no left flushed functions then
            try finding indented ones.

return:

 '((type-string . name) ...)"
  (interactive)
  (let ((re     tinylisp-:regexp-function)
	(buffer tinylisp-:buffer-data)
	(loop   t)
	list
	type
	var
	str
	func)
    (while loop
      (setq loop nil)
      (save-excursion
        (ti::pmin)
        (while (re-search-forward re nil t)
          (setq type (match-string 1)
                var  (match-string 2))
          (if (and type var)
              (ti::nconc list (cons type var))))
        (if (and (null list)
                 (eq mode 'alternative))
            (setq loop t                ;try again
                  ;;   remove anchor
                  re (substring re 1)))))
    (if (and list (null no-show))
        (tinylisp-with-current-buffer
	    (ti::temp-buffer buffer 'clear)
	  (dolist (var list)
	    (setq str nil) ;Clear this
	    ;;  Is it symbol? Yes; okay is there really such function?
	    ;;  Okay, read the interactive arguments the, OTW
	    ;;  it was not a function.
	    (if (setq func (intern-soft (cdr var)))
		(if (fboundp func)
		    (setq str (commandp func))
		  (setq func nil)))
	    (insert (format "%-12s%s%s %-40s %s\n"
			    (car var)
			    ;;  Interactive and defsubst? this is dangerous!
			    ;;
			    (if (and str
				     (string= "defsubst" (car var)))
				" !" "")
			    (if (null func) " ?" "")

			    (cdr var)
			    (or str ""))))
	  (pop-to-buffer (current-buffer))
	  (ti::pmin)
	  (run-hooks 'tinylisp-:find-func-list-hook)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-variable-list-occur ()
  "Run occur to find variables from whole buffer."
  (interactive)
  (ti::occur-macro tinylisp--regexp-variable nil
    (ti::text-re-search-forward "(defconst")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-variable-list (&optional show-type)
  "Get all defvars and defconst from current buffer.
you can e.g. call this function to get all variables and update them
to your M - x xxx-submit-bug-report function's variable list.

the appearing list will wave defvar's first, then defconst.

input:
  SHOW-TYPE     if non-nil, then show `user-variable-p' and
                `defcustom' information too."
  (interactive "p")
  (let ((re     tinylisp--regexp-variable)
	(buffer tinylisp--buffer-variables)
	str
	sym
	type var
	vl                             ;def(v)ar   (l)ist
	cl                             ;def(c)onst (l)ist
	list)
    (save-excursion
      (ti::pmin)
      (while (re-search-forward re nil t)
        (setq type (match-string 1)
              var  (match-string 2))
        (if (string-match "defvar\\|defcustom" type)
            (push (cons type var) vl)
          (push (cons type var) cl))))
    (if (not (or vl cl))
        (message "TinyLisp: Can't' find any variables.")
      (with-current-buffer (ti::temp-buffer buffer 'clear)
        (display-buffer (current-buffer))
        ;;  Preserve order with reverse
        (setq vl (nreverse vl)
              cl (nreverse cl))
        (setq list (list vl cl))
        (dolist (elt list)              ;loop both lists
          (dolist (var elt)
            (setq type (car var)
                  sym  (cdr var)
                  str  ";; #symbol not found")
            (tinylisp-symbol-do-macro sym 'noerr
	      (setq str "")
	      (if (user-variable-p sym)
		  (setq str "user variable"))
	      (if (string= type "defcustom")
		  (setq str (concat str " defcustom")))
	      (if (not (ti::nil-p str)) ;Add comment prefix if not empty
		  (setq str (concat ";; " str))))
            (if (null show-type)
                (insert (cdr var) "\n")
              (insert (format "%-40s%s\n" (cdr var) str))))
          (insert "\n")
          (ti::pmin)
          (run-hooks 'tinylisp--find-var-list-hook))))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-narrow-to-function ()
  "Narrow to current function around point."
  (interactive)
  (let ((re tinylisp--regexp-function)
	beg
	end)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at re))
          (re-search-backward tinylisp--regexp-function))
      ;;  find first empty line
      (re-search-backward "^[ \t]*$" nil t)
      (setq beg (point))
      (forward-sexp 1)
      (setq end (point)))
    (narrow-to-region beg end)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-symbol-information (file &optional verb)
  "Display symbol information from FILE (absolute path). VERB.
FILE must be loaded into Emacs to gather all the variable
and function definitions."
  (interactive
   (let ((name (tinylisp-library-read-name 'el)))
     (list
      (or (locate-library name)
	  (error "Can't find absolute path for %s" name))
      current-prefix-arg)))
  (or file
      (error "Argument FILE is missing"))
  (let* ((fname (file-name-nondirectory file))
	 (feature-name (intern-soft
			(file-name-sans-extension fname))))
    ;;  If the feature is not same as file name, we have no
    ;;  other choice to load the file. If feature-name was
    ;;  set, then the feature is already in Emacs (file was loaded
    ;;  previously)
    (unless feature-name
      (load file))
    (with-current-buffer (ti::system-get-file-documentation file verb)
      (if (interactive-p)
	  (display-buffer (current-buffer)))
      (turn-on-tinylisp-mode))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-info-emacs (&optional verb)
  "Examine load history and print libraries loaded into Emacs.
The summary for each library is in following format:

     [*]xxx.el NN /usr/local/Emacs/lisp tinylibm tinylib
      |        |  |                     |
      |        |  |                     |  What it `required'
      |        |  Where it is according to `load-path' order.
      |        How many symbols defined
      If star, then the load history had full path name for library

If VERB parameter is nil, then the buffer is not shown and no
messages are displayed.

Return:

 buffer     `tinylisp--buffer-data'"
  (interactive)
  (let ((max       (length load-history ))
	(buffer    (ti::temp-buffer tinylisp--buffer-library 'clear))
	(i         0)
	(unknown   "--unknown--")
	dep-list
	name
	path)
    (ti::verb)
    (tinylisp-with-current-buffer buffer
      (dolist (pkg load-history)
	(when (stringp (setq name (car pkg)))
	  (setq path (ti::system-load-history-where-is-source name)))
	;;  Go to next element, these will have dependency information
	;;  ("tinycom" (require . tinylibm) byte-compile-dynamic ...
	;;                      |
	;;                      Get these
	(pop pkg)
	(while  (ti::consp (car pkg))
	  (push (cdr (car pkg)) dep-list)
	  (pop pkg))
	;;  User has evaled the package 'in place' and not loaded it.
	(unless (stringp name)
	  (setq name unknown))
	(insert
	 (format
	  "%-30s %3d %-35s %s %s\n"
	  (concat
	   (if (string-match "^/" (or name ""))
	       "*"
	     "")
	   (file-name-nondirectory name))
	  (length pkg)
	  (if path
	      (file-name-directory path)
	    "<no path>")
	  (mapconcat
	   (function (lambda (x) (symbol-name x)))
	   dep-list
	   " ")
	  ;;  - If the package name is unknow, print some symbol
	  ;;    names that it defined so that user can use grep later
	  ;;    to find out what packagage it was
	  ;;
	  (if (not (string= name unknown))
	      ""
	    (format "%s ..." (ti::string-left (prin1-to-string pkg) 80)))))
	(if verb
	    (message "TinyLisp: lib info %d/%d %s" i max name))
	(incf  i)x
	(setq dep-list  nil
	      pkg       nil)))
    (tinylisp-with-current-buffer buffer
      (ti::pmin)
      (ti::buffer-remove-whitespace-eol)
      (sort-lines nil (point-min) (point-max)))
    (when verb
      (pop-to-buffer buffer)
      (ti::pmin)
      (message "Done."))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-read-something ()
  "Position point to over some words near point."
  (save-excursion
    (if (looking-at "[ \t\n]")          ;only spaces ahead?
        (ti::read-current-line)
      ;;  Go backward until space(word) or function call
      (unless (char-equal (following-char) ?\( )
        (re-search-backward "[( \t\n]" nil t)
        (skip-chars-forward " \t\n")))
    (buffer-substring (point) (line-end-position))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-read-name (&optional el)
  "Read lisp library name with possible completion. If EL, return with .el"
  (let* ((cache (fboundp 'tinypath-emacs-lisp-file-list))
         (list  (cond
                 (cache
                  ;;  tinyPath caches all files for fast loading
                  ;;  Use it if available
                  (ti::funcall 'tinypath-emacs-lisp-file-list 'from-cache))
                 (t
                  (ti::list-to-assoc-menu
                   (ti::system-load-history-emacs-lisp-files)))))
         (word   (ti::string-match "[a-z0-9]+[a-z0-9-.]+" 0
                                   (or (tinylisp-read-word) "" )))
         file)
    (when (setq file
                (completing-read
                 (format "%sLisp Library: "
                         (if cache
                             "(tinypath cache)"
                           "(load-history)"))
                 list
                 nil
                 nil
                 word))
      (when el
        (when (string-match "^\\(.*\\)\\.elc$" file)
          (setq file (concat (match-string 1 file))))
        (unless (string-match "\\.el" file)
          (setq file (concat file ".el")))))
    file))

;;; ----------------------------------------------------------------------
;;; The name is not a mistake although it may sound repetitive. All
;;; function in TinyLisp have prefix "tinylisp-library" if they deal with
;;; load-path libraries.
;;;
;;; The second part is `locate-library' which is standard Emacs function.
;;; If you do a C-h a  `locate-library' you will correctly find both
;;; of these implementations.
;;;
(defun tinylisp-library-locate-library-1 (file &optional extensions)
  "Like `locate-library' but find all possible occurrances of FILE.
This also finds compressed files. Path portion and file extensions
in FILE are ignored.

Extension search order is: '(\".el\" \".elc\").

Return:
  List of possible paths. Example:
  '(\"/path/to/package.el\" \"/path/to/package.elc\")
"
  (let ((compressions '("" ".gz" ".Z" ".z" ".bz2" ".zip"))
	try
	ret)
    (setq file (file-name-sans-extension
                (file-name-nondirectory file)))
    (or extensions
        (setq extensions '(".el" ".elc")))
    (dolist (path load-path)
      (setq path (file-name-as-directory (expand-file-name path)))
      (dolist (end extensions)
        (dolist (z compressions)
          (setq try (format "%s%s%s%s" path file end z))
          (if (file-exists-p try)
              (pushnew try ret :test 'string=)) )))
    ;; Preserve search order (due to push)
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-locate-by-fullpath-intercative ()
  "Call `tinylisp-library-locate-by-fullpath' interactive with a check."
  (interactive)
  (cond
   ((not (featurep 'tinylisp))
    (message "Tinylisp: [ERROR] Fullpath locate requires tinypath.el."))
   (t
    (call-interactively
     'tinylisp-library-locate-by-fullpath))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-locate-by-fullpath (regexp)
  "Find all packages whose full path name match REGEXP.
This function requires that feature 'tinypath is present.
List is outputted to message buffer."
  (interactive "sMatch package fullpath by regexp: ")
  (message "Tinylisp: Locate by FULLPATH regexp '%s' -- begin"
           regexp)
  (dolist (path (tinypath-cache-match-fullpath regexp 'names))
    (message path))
  (message "Tinylisp: Locate by FULLPATH regexp '%s' -- end"
           regexp)
  (display-buffer (ti::buffer-pointer-of-messages)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-locate (file &optional insert)
  "Like `locate-library' but find all possible occurrances of FILE.
Optionally. INSERT found filenames to point."
  (interactive (list (tinylisp-library-read-name) current-prefix-arg))
  (let ((list (tinylisp-library-locate-library-1 file)))
    (if (null list)
        (message "TinyLisp: no library found %s" file)
      (message "TinyLisp: %s" (ti::list-to-string list))
      (if insert
          (insert (ti::list-to-string list "\n"))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-documentation (file &optional insert)
  "Print the documentation of lisp FILE and possibly INSERT it to point.
This relies on the fact that documentation is at the beginning of file.

Return:

  buffer  Content of Commentary: section"
  (interactive
   (list (tinylisp-library-read-name 'el) current-prefix-arg))
  (let ((list (tinylisp-library-locate-library-1 file '(".el") ))
	str
	file
	buffer)
    (when list
      (when (interactive-p)
        (setq file (car list))
        (if (> (length list) 1)
            (setq file
                  (completing-read "TinyLisp: [Choose] "
                                   (ti::list-to-assoc-menu list)
                                   nil
                                   'match)))
        ;; Same what finder-commentary uses.
        ;; One problem: lm-commentary has a bug, which causes killing
        ;; the file from emacs after it's done. But we don't want that
        ;; if use is viewing or loaded it to emacs before us.
        ;;
        ;; Work around that bug.
        (let ((buffer (find-buffer-visiting file)))
          (setq str
                (if (null buffer)
                    (lm-commentary file)
                  (with-temp-buffer
                    (insert-buffer-substring buffer)
                    (lm-commentary)))))
        (if (not (stringp str))
            (message "TinyLisp: No commentary in %s" file)
          (with-temp-buffer
            (insert str)
            (ti::pmin) (ti::buffer-replace-regexp "^;+" 0 "")
            (ti::pmin) (ti::buffer-replace-regexp "\r" 0 "")
            (setq str (buffer-string)))
          (cond
           (insert
            (insert str)
            (setq buffer (current-buffer)))
           (t
            (setq buffer (ti::temp-buffer tinylisp--buffer-library 'clear))
            (with-current-buffer tinylisp--buffer-library
              (insert str)
              (ti::pmin) ;;#todo: how to display it at start?
              (display-buffer (current-buffer))))))))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-find-file (file)
  "`find-file' a lisp library FILE along `load-path'.
In interactive call, the FILE is completed using `load-path' libraries."
  (interactive (list (tinylisp-library-read-name 'el)))
  (let ((path (locate-library file)))
    (if (not path)
        (message "TinyLisp: file %s not along `load-path'" file)
      (find-file path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-load-library (file)
  "Like `load-library' but offer completion of lisp files."
  (interactive (list (tinylisp-library-read-name)))
  (let ((file (locate-library file)))
    (if (not file)
        (message "TinyLisp: file %s not along `load-path'" file)
      (load-library file))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-load-history-car ()
  "Return `car' elements from `load-history'."
  (let (list)
    ;; Faster than mapcar
    (dolist (elt load-history)
      (push (car elt) list))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-load-history-grep (regexp)
  "Grep load history with REGEXP."
    (ti::list-find
     (tinylisp-load-history-car)
     regexp
     (function
      (lambda (re elt)
	(and (stringp elt)
	     (string-match re elt))))
     'all-matches))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-load-by-regexp (regexp &optional no-elc verb)
  "Reload all packages (that are inside Emacs) matching REGEXP.
NO-ELC says to load non-compiled packages. VERB.

NOTE
  The REGEXP is matches against full path."
  (interactive
   (list
    (read-from-minibuffer "Reload packages matching regexp: ")
    (y-or-n-p "Load uncompiled versions ")))
  (let ((count 0)
	list
	done)
    (ti::verb)
    (when (and verb
               (string-match "el$" regexp))
      (message "Tinylisp: Reload, regexp should not need to match .el$"))
    (setq list (tinylisp-load-history-grep regexp))
    (dolist (elt list)
      (setq elt (expand-file-name elt))
      ;;  Remove extension and use .el always,
      ;;  Note, that the elt may not have extension at all
      ;;  when we do del-re
      (cond
       (no-elc
        (setq elt (replace-regexp-in-string "\\.elc?$" "" elt))
        (setq elt (concat elt ".el")))
       (t
        ;;  Remove whole extension
        (setq elt (replace-regexp-in-string "\\.elc?$" "" elt))))
      (unless (member elt done)
        ;;  In XEmacs; the packages are stored as absolute path names.
        ;;  In Emacs, just "package.el".
        ;;  Try loading absolute, if it does not work; try without.
        (push elt done)
        (cond
         ((or (and (ti::file-name-path-p elt)
                   (load elt 'noerr))
              (progn
                (setq elt (file-name-nondirectory elt))
                (load elt 'noerr)))
          (incf count))
         (t
          (message "TinyLisp: Reload failed %s" elt)))))
    (when verb
      (message "TinyLisp: %s packages reloaded" count))
    list))

;;; ----------------------------------------------------------------------
;;; See XEmacs ilisp.el :: describe-symbol-find-file
;;;
;;; (defun describe-symbol-find-file (symbol)
;;;  (loop for (file . load-data) in load-history
;;;    do (when (memq symbol load-data)
;;;      (return file))))
;;;
(defun tinylisp-library-find-symbol-load-info ()
  "Try to look up load history to determine from where functions was defined.
read current line from point forward.

displayed message format:

   [m]{AD} symbol-xxx: package.el (~/elisp/mime/)
   [m]{AD} symbol-xxx: ~/elisp/xxx.el

Description:

  The first line says that the load history entry contains only
  \"package.e\" and according to `load-path' information the package was
  found from directory ~/elisp/mime/.

  The second line: `load-history' contained full path for the package

Note:

  Additional characters at the beginning: `m' function is macro.

  The additional 'AD' String appears on the line of the function has
  any advice code attached to it. To check the advice documentation
  string, call \\[describe-function].

  BUT, this flag only tells if there is advice code, it does not tell
  whether the acvice is active or not (If you don't see advice mentioned
  after \\[describe-function], then the aadvice is instrumented, but
  latent, and not working currently)."
  (interactive)
  (let* ((str      (tinylisp-read-something))
         (sym      (tinylisp-get-symbol str))
         (alias    (or (ti::defalias-p sym) sym))
         (autoload (ti::autoload-p sym))
         (ad-info  "")
         package
         path
         msg)
    (if (null sym)
        (message "TinyLisp: \
Can't find _defined_ variable or function on the line (eval buffer first).")
      (if (memq 'ad-advice-info (symbol-plist sym))
          (setq ad-info "AD "))
      (cond
       ((null (fboundp alias))
        (setq msg "not a function"))
       ((ti::subrp-p alias)
        (setq msg "<Built-in function>"))
       (autoload
         (let* ( ;; (autoload "dired-aux" "Copy all..")
                (file (ti::string-match
                       " \"\\([^\"]+\\)" 1
                       (prin1-to-string (symbol-function autoload))))
                (name (symbol-name autoload))
                (path (locate-library file)))
           (setq msg
                 (format "[autoload] %s %s (%s)"
                         (if (not (eq autoload sym))
                             (concat "defalias->" name)
                           "")
                         (if path
                             (file-name-nondirectory path)
                           file)
                         (if path
                             (file-name-directory path)
                           "<no path found>")))))
       ;; ............................................... load-history ...
       ((setq package (car-safe
                       (ti::system-load-history-where-is-source alias)))
        (if (setq path (ti::system-load-history-where-is-source package))
            (setq msg
                  (format "%s (%s)"
                          (file-name-nondirectory path)
                          (file-name-directory path)))
          (setq msg "<no path found>")))
       ;; ...................................................... other ...
       (t
        ;;  See if we have find-func available and call it
        (if (not (and (fboundp 'find-function)
                      (ignore-errors (ti::funcall 'find-function alias))))
            (setq msg "no `load-history' entry; maybe evaled locally?"))))
      ;; ..................................................... message ...
      (message "%s%s%s: %s"
               (if (ti::defmacro-p sym) "(macro)" "")
               ad-info
               (if (and alias
                        (not (eq alias sym)))
                   (format "[%s alias --> %s]"
                           (symbol-name sym)
                           (symbol-name alias))
                 (symbol-name sym))
               msg))))

;;}}}
;;{{{ code help: misc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      AUTOLOAD UTILITIES
;;      These are admistrative utilies for package maintainer(s)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tinylisp--ignore-dir-regexp
  "\\(\\.\\(bzr\\|hg\\|git\\|svn\\|mtn\\)\\|CVS\\|RCS\\|_MTN\\|\\.\\.?\\)$"
  "Regexp to ignore directories.")

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-file-name-add-suffix (file suffix)
  "Convert FILE.EXT into FILE-SUFFIX.EXT."
  (let ((ext (file-name-extension file)))
    (format "%s-%s.%s"
	    (file-name-sans-extension file)
	    suffix
	    ext)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (not (string-match "[\\/]\\.\\.?$" elt)))
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-directory-file-list (dir &optional exclude)
  "Return list of Emacs Lisp files. Optinally EXCLUDE by regexp."
  (let (list)
    (dolist (elt (directory-files dir 'full "\\.el$"))
      (when (or (null exclude)
		(not (string-match exclude elt)))
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-directory-last (dir)
  "Return last directory name in DIR. /dir1/dir2/ -> dir2."
  (if (string-match "[/\\]\\([^/\\]+\\)[/\\]?$" dir)
      (match-string 1 dir)
    ""))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-directory-to-file-name (dir template)
  "Make file name from NAME and TEMPLATE. <template>-<last-dir>.el."
  (concat
   (file-name-as-directory dir)
   template
   (tinylisp-autoload-real-directory-last dir)
   ".el"))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-directories (list)
  "Examine LIST and return directories.
Dot-dirs and version control directories are filtered out."
  (let ((re `,(concat
	       "[/\\]\\.+$\\|CVS\\|RCS\\|_MTN"
	       "\\|\\.\\(svn\\|bzr\\|hg\\|git\\|darcs\\|mtn\\)"))
	ret)
    (dolist (elt list)
      (when (and (file-directory-p elt)
                 ;;  Drop . ..
                 (not (string-match re elt)))
        (push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-with-command-line 'lisp-indent-function 0)
(put 'tinylisp-with-command-line 'edebug-form-spec '(body))
(defmacro tinylisp-with-command-line (&rest body)
  "Loop `command-line-args-left', run BODY. Variable `item' is bound."
  `(let ((debug-on-error t))
     (dolist (item command-line-args-left)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-autoload-file-footer (file &optional end)
  "Return 'provide and optional END of the file marker."
  (concat
   (format
    "(provide '%s)\n"
    (file-name-sans-extension (file-name-nondirectory file)))
   (if end
       (format ";; End of file %s\n"
               (file-name-nondirectory (file-name-nondirectory file)))
     "")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-generate-library (library)
  "Read all defuns and construct autoloads from LIBRARY on `load-path'."
  (interactive
   (list (tinylisp-library-read-name)))
  (let ((path (if (file-name-absolute-p library)
		  library
		(or (locate-library library)
		    (error "TinyLisp: Can't locate library %s" library)))))
    ;; The name MUST end to .el, because that is the source of autoloads
    (cond
     ((string-match "\\.elc$" path)
      (setq path (replace-match ".el" nil t path))
      ;; File may also be stored in compressed format
      (let (try)
	(dolist (ext '("" ".gz" ".bz2" ".lzma"))
	  (setq try (concat path ext))
	  (if (file-exists-p try)
	      (return (setq path try))))))
     ((not (string-match "\\.el$" path))
      (setq path (concat path ".el"))))
    (ti::use-file-compression-maybe path)
    (ti::package-autoload-create-on-file
     path
     (tinylisp-get-buffer-create tinylisp--buffer-autoload))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-point ()
  "Return list of interactive functions from current point."
  (let (list)
    (while (tinylisp-forward-user-option)
      (when (looking-at "^[ \t]*([ \t]*defun[ \t]+\\([^ (\t\r\n]+\\)")
	(push (match-string 1) list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-buffer ()
  "Return list of interactive functions from buffer."
  (save-excursion
    (goto-char (point-min))
    (tinylisp-interactive-function-list-from-point)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-file (file)
  "Return list of interactive functions from file."
  (with-temp-buffer
    (tinylisp-insert-file-contents file)
    (tinylisp-interactive-function-list-from-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-interactive-gather-from-file (file)
  "Autoload definitions for interactive defuns from FILE as string."
  (let ((list (tinylisp-interactive-function-list-from-file file)))
    (when list
      (let ((re (regexp-opt list 'words)))
	(with-temp-buffer
	  (ti::package-autoload-create-on-file
	   file
	   (current-buffer)
	   'no-show)
	  (goto-char (point-min))
	  (delete-non-matching-lines re)
	  (goto-char (point-min))
	  (delete-matching-lines "^;")
	  (buffer-string))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-build-from-buffer (&optional arg)
  "Read all defuns and construct autoloads from buffer.
The autoloads cannot be generated from anonymous buffer, because the
syntax is:

   (autoload 'function \"file\" ..)
                        |
                        This is mandatory

Call arguments:

  ARG   If non-nil, ask lisp library name and locate it along `load-path'."
  (interactive "P")
  (cond
   (arg
    (tinylisp-autoload-generate-library
     (tinylisp-library-read-name)))
   ((buffer-file-name)
    (ti::package-autoload-create-on-file
     (buffer-file-name)
     (tinylisp-get-buffer-create tinylisp--buffer-autoload)))
   (t
    (error "Can't generate autoload statements. (buffer-file-name) is nil."))))

;;; ----------------------------------------------------------------------
;;; FIXME: See tinylisp-autoload-quick-autoload-build-dir
;;;###autoload
(defun tinylisp-autoload-guick-build-from-dir
  (file-or-dir &optional regexp no-desc buffer verb)
  "Generate quick autoloads from list of FILE-OR-DIR matching REGEXP.
Input:

  FILE-OR-DIR File or directory.
  REGEXP      If FILE-OR-DIR was directory, read files matching REGEXP.
  NO-DESC     Optional, if non-nil, do not include function detection comments.
              Interactively supply \\[universal-argument].
  BUFFER      Optional buffer where to gather autoloads; default
              `tinylisp--buffer-autoload'
  VERB        Optional flag, if non-nil pop to result buffer."
  (interactive "DAutoload directory: \nsFiles matching regexp: \nP")
  (let ((files (if (file-directory-p file-or-dir)
		   (ti::directory-files
		    file-or-dir
		    regexp
		    'absolute
		    '(and (not (file-directory-p arg))
			  (string-match "\\.el$" arg)))
		 (list file-or-dir))))        ;single filename
    (or buffer
        (setq buffer (tinylisp-get-buffer-create tinylisp--buffer-autoload)))
    (ti::verb)
    (dolist (file files)
      (ti::package-autoload-create-on-file
       file
       buffer
       (null verb)
       no-desc))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-write-loaddefs-file (file dest &optional verb)
  "Write ###autoload from FILE to DEST. VERB."
  (let ((generated-autoload-file dest))
    (ti::file-delete-safe dest)
 ;;;    (ti::package-autoload-loaddefs-create-maybe dest)
    (tinylisp-with-file-env-macro
      (with-current-buffer (tinylisp-find-file-noselect dest)
        (goto-char (point-max))
        ;;  line added by `ti::package-autoload-loaddefs-create-maybe'
;;;        (re-search-backward "\n.*provide")
        (let ((point (point)))
	  ;;  FIXME: This call generates ^L lines. Are they important?
          (generate-file-autoloads file)
          ;;  was something inserted?
          (cond
           ((eq (point) point)
            (if verb
                (message "No autoload definitions in %s" file)))
           (t
	    ;;  DVCS git does not EOL whitespace
	    (ti::buffer-remove-whitespace-eol)
            (let ((backup-inhibited t))
              (save-buffer))
            (kill-buffer (current-buffer)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-generate-loaddefs-file (file &optional dest verb)
  "Generate autoloads from FILE to DEST. VERB.
In interactive mode, the DEST is FILE-loaddefs.el and VERB mode is t."
  (interactive
   (let* ((path (buffer-file-name))
	  (dir  (and path
		     (file-name-directory (buffer-file-name))))
	  file
	  dest)
     (setq file (read-file-name
		 "Loaddefs from file: "
		 dir
		 (not 'default)
		 (not 'match)
		 (and path
		      (file-name-nondirectory path))))
     (if (string= "" file)
	 (error "No file name"))
     (setq dest (tinylisp-file-name-add-suffix file "loaddefs"))
     (setq dest (read-file-name
		 "Loaddefs write: "
		 (file-name-directory dest)
		 (not 'default)
		 (not 'match)
		 (file-name-nondirectory dest)))
     (if (string= "" dest)
	 (error "No file name"))
     (list
      file
      dest
      'verbose)))
  (tinylisp-autoload-write-loaddefs-file file dest verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-batch-autoload-generate-loaddefs-file (&optional suffix)
  "Call `tinylisp-autoload-write-loaddefs-file' for `command-line-args-left'.
Add optional SUFFIX to the files. Default value is 'loaddefs' resulting
a 'filename-loaddefs.el'"
  (tinylisp-with-command-line
   (tinylisp-autoload-write-loaddefs-file
    item
    (tinylisp-file-name-add-suffix item (or suffix "loaddefs"))
    'verbose)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-generate-loaddefs-file-list
  (list &optional suffix verb)
  "Generate loaddefs for LIST of files.
Add optional SUFFIX and display nmessages if VERB is non-nil."
  (let (dest)
    (dolist (file list)
      (setq dest (tinylisp-file-name-add-suffix
		  file
		  (or suffix "loaddefs")))
      (tinylisp-autoload-write-loaddefs-file file dest verb))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-generate-loaddefs-dir
  (dir &optional exclude verb)
  "Generate loaddefs from DIR, optionally EXCLUDE by regexp.
If VERB is non-nil, display verbose messages."
  (interactive "DLoaddefs from dir\nsIgnore regexp: ")
  (let ((regexp "-\\(loaddefs\\|autoload.*\\)\\.el")
	list)
    (if (and (stringp exclude)
	     (string-match "^[ \t\r\n]*$" exclude))
	;; Interactive RET
	(setq exclude regexp))
    (when (setq list (tinylisp-directory-file-list dir exclude))
      (tinylisp-autoload-generate-loaddefs-file-list
       list
       (or verb (interactive-p))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-batch-autoload-generate-loaddefs-dir (&optional exclude)
  "Call `tinylisp-autoload-generate-loaddefs-dir' for `command-line-args-left'.
Optionally EXCLUDE files by regexp."
  (tinylisp-with-command-line
   (tinylisp-autoload-generate-loaddefs-dir
    item
    exclude)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-generate-loaddefs-recursive (dir)
  "Generate ###autoload recursively starting from DIR."
  (interactive "DGenerate ###autoload recursive dir: ")
  (tinylisp-directory-recurse-macro
      dir
    (tinylisp-autoload-generate-loaddefs-dir dir)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-update-update-file-autoloads-1 (file dest)
  "Update autoload from FILE to DEST."
    (ti::package-autoload-loaddefs-create-maybe dest)
    (let ((generated-autoload-file dest))
      (tinylisp-with-file-env-macro
        (update-file-autoloads file))))

;;; ----------------------------------------------------------------------
;;; tinylisp-loaddefs-update-from-file
;;; FIXME: unused.
(defun tinylisp-autoload-real-update-file-autoloads (file)
  "Update autoload from FILE to FILE-loaddefs.el"
  (interactive "fLoaddedfs from file: ")
  (let ((dest (format "%s-loaddefs.el"
		      (file-name-sans-extension file))))
    (tinylisp-autoload-real-update-update-file-autoloads-1
     file dest)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-build-from-file-1
  (file dest &optional provide footer)
  "Generate autoload from FILE to DEST.

Input:

  FILE	    Examine FILE.
  DEST	    Write autoloads to DEST.
  PROVIDE   If non-nil, write `provide' statement.
  FOOTER    If non-nil, write 'End of ...' footer."
  (with-temp-buffer
    (tinylisp-with-file-env-macro
     (ti::package-autoload-create-on-file
      file
      (current-buffer)
      'no-show
      'no-desc
      'no-path)
     (if provide
	 (insert (tinypath-tmp-autoload-file-footer dest footer)))
     (write-region (point-min) (point-max) dest))
    dest))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-build-from-file
  (file &optional dest provide footer verb)
  "Generate autoload from FILE to DEST

Input:

  FILE	    Read FILE
  DEST	    Destination file. Default is FILE-autoloads
  PROVIDE   If non-nil, write `provide' statement.
  FOOTER    If non-nil, write 'End of ...' footer.
  VERB	    Enable verbose messages."
  (interactive
   (let* ((path  (buffer-file-name))
	  (dir   (and path
		      (file-name-directory path)))
	  file
	  dest)
     (setq file (read-file-name
		 "Autoloads from file: "
		 dir
		 (not 'default)
		 (not 'match)
		 (and path
		      (file-name-nondirectory path))))
     (if (string= "" file)
	 (error "No file name"))
     (setq dest (tinylisp-file-name-add-suffix file "autoloads"))
     (setq dest (read-file-name
		 "Autoloads write: "
		 (file-name-directory dest)
		 (not 'default)
		 (not 'match)
		 (file-name-nondirectory dest)))
     (if (string= "" dest)
	 (error "No file name"))
     (list
      file
      dest
      'verbose)))
  (unless (stringp dest)
    (setq dest (tinylisp-file-name-add-suffix file "autoloads")))
  (tinylisp-autoload-quick-build-from-file-1'
   file dest provide footer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-batch-autoload-quick-build-from-file
  (&optional suffix provide footer)
  "Call `tinylisp-autoload-quick-build-from-file-1'
Read file list from `command-line-args-left'.

Input:

  SUFFIX    Destination file suffix. default is *-autoloads
  PROVIDE   If non-nil, write `provide' statement.
  FOOTER    If non-nil, write 'End of ...' footer."
  (tinylisp-with-command-line
   (tinylisp-autoload-quick-build-from-file-1
    item
    (tinylisp-file-name-add-suffix
     item
     (or suffix "autoloads"))
    provide
    footer)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-build-interactive-from-file
  (file &optional dest provide footer verb)
"Read file list from `command-line-args-left'.
Collect interactive defuns.

Input:

  SUFFIX    Destination file suffix. default is *-autoloads
  PROVIDE   If non-nil, write `provide' statement.
  FOOTER    If non-nil, write 'End of ...' footer.
  VERB      If non-nil, write verbose messages."
  (interactive
   (let* ((path  (buffer-file-name))
	  (dir   (and path
		      (file-name-directory path)))
	  file
	  dest)
     (setq file (read-file-name
		 "I-autoloads from file: "
		 dir
		 (not 'default)
		 (not 'match)
		 (and path
		      (file-name-nondirectory path))))
     (if (string= "" file)
	 (error "No file name"))
     (setq dest (tinylisp-file-name-add-suffix
		 file
		 "autoloads-interactive"))
     (setq dest (read-file-name
		 "Autoloads write [RET = none]: "
		 (file-name-directory dest)
		 (not 'default)
		 (not 'match)
		 (file-name-nondirectory dest)))
     (if (string= "" dest)
	 (setq dest nil))
     (list
      file
      dest
      'verbose)))
  (let ((str (tinylisp-autoload-quick-interactive-gather-from-file file)))
    (cond
     ((not str)
      (if verb
	  (message "No interactive defuns found in %s" file)))
     (dest
      (with-temp-buffer
	(insert str)
	(if provide
	    (insert (tinypath-tmp-autoload-file-footer provide footer)))
	(write-region (point-min) (point-max) dest)))
     (t
      (with-current-buffer (tinylisp-get-buffer-create tinylisp--buffer-autoload)
	(goto-char (point-max))
	(insert str)
	(if verb
	    (display-buffer (current-buffer))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-batch-autoload-quick-build-interactive-from-file
  (&optional provide footer)
  "Call `tinylisp-autoload-quick-build-interactive-from-file'
for `command-line-args-left'."
  (tinylisp-with-command-line
   (tinylisp-autoload-quick-build-interactive-from-file
    item
    (tinylisp-file-name-add-suffix
     item
     "autoloads-interactive")
    provide
    footer
    'verbose)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-autoload-build-dir
  (dir &optional include exclude)
  "Generate all autoloads from DIR.
Obey optional INCLUDE and EXCLUDE regexps."
  (interactive
   "DGenerate quick autoloads in dir\nsInclude re: \nsExlude re: ")
  (let ((files (directory-files dir 'full "\\.el"))
        list)
    (dolist (file files)
      (cond
       ;; Ignore these
       ((string-match "-\\(loaddefs\\|autoload.*\\)\\.el" file))
       ((and (stringp exclude)
             (not (string-match "^[ \t]*$" exclude))
             (string-match exclude file)))
       ((or (not (stringp include))
            (string-match "^[ \t]*$" include)
            (string-match include file))
        (tinylisp-autoload-quick-build-from-file file))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-autoload-build-recursive
  (dir &optional include exclude)
  "Generate all autoloads recursively starting from DIR."
  (interactive
   "DGenerate all autoloads recursive dir: \nsInclude re: \nsExlude re: ")
  (tinylisp-directory-recurse-macro
      dir
    (tinylisp-autoload-quick-autoload-build-dir dir include exclude)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      OTHER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-forward-def (&optional back verb)
  "Go to next `def' forward or `BACK'. VERB."
  (interactive
   (list
    current-prefix-arg
    'interactive))
  (let ((opoint (point))
	ret)
    ;;  Before doing slow loop, try this. This may fail; because
    ;;  the 'defun macro' doesn't land always to right spot. Try negative
    ;;  indent inside fuction
    ;;
    ;;      (defun  ...
    ;;      (negative-indent
    ;;         ...other function code
    ;;         *point here
    ;;
    ;;  And the defu macro would go to `negative' indent position and not
    ;;  to the `defun'. That's why regexp text.
    (cond
     (back
      (beginning-of-defun)
      (cond
       ((looking-at "^(def")
        (setq ret (point)))
       ((re-search-backward "^(def" nil t)
        (setq ret (match-beginning 0)))))
     (t
      (if (looking-at "^(def")
	  (goto-char (line-end-position)))
      (if (re-search-forward "^(def" nil t)
          (setq ret (match-beginning 0)))))
    (if ret
        (goto-char ret)
      (goto-char opoint)
      (if verb
	  "No more `def' matches"))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-indent-around-point (&optional verb)
  "Indent current statement around the point. typically a function.
VERB."
  (interactive
   (list 'verb))
  (let (msg
	beg
	end)
    (and (save-excursion
           (and (setq beg (tinylisp-forward-def 'back))
                (setq msg (ti::string-left (ti::read-current-line) 60)))
           beg)
         (save-excursion
           (goto-char beg) (end-of-defun)
           (setq end (point))))
    (if (not (and beg end))
        (if verb (message "TinyLisp: can't find anything to indent here."))
      ;;   Reset the prefix or disaster occur
      (let (fill-prefix) (indent-region beg end nil))
      (if verb (message  "TinyLisp: [indented] %s" msg)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-macroexpand (&optional expand-function)
  "Expand macro call with EXPAND-FUNCTION which is string.
If point is sitting inside call to macro, expand it.
in the following example the cursor is at point [*].

  (macro-function-call arg1 * arg2 arg3)

references:
  `tinylisp--buffer-macro'
  `tinylisp--macroexpand-function-list'"

  (interactive
   (list
    (intern-soft
     (completing-read
      "Expand with function: "
      (ti::list-to-assoc-menu tinylisp--macroexpand-function-list)
      nil
      nil
      (car tinylisp--macroexpand-function-list)))))
  (let ((mac-re tinylisp--regexp-macro-definition)
	(opoint (point))
	point
	symbol
	sym
	to-buffer)
    (when (not (and (symbolp expand-function)
                    (fboundp expand-function)))
      (error "Not a function %s" expand-function))
    (tinylisp-defun-macro
      (if (setq symbol (ti::string-match "[^() \t\n\]+" 0 str))
	  (setq sym (intern-soft symbol)))
      (cond
       ((and (stringp symbol)
	     ;; These are macros
	     (not (string-match mac-re symbol))
	     ;;  Others are supposed to be function definitions
	     (string-match "^def" symbol)
	     (not (ti::defmacro-p sym)))
	(message
	 "TinyLisp: grabbed %s, but it is not a macro's call statement"
	 symbol))
       ((and (stringp symbol)
	     sym
	     (ti::defmacro-p sym))
	(setq to-buffer (ti::temp-buffer tinylisp--buffer-macro 'clear))
	(append-to-buffer to-buffer beg end)
	(goto-char opoint)               ;restore position
	(pop-to-buffer to-buffer)
	(ti::pmin)
	(emacs-lisp-mode)
	(insert "(" (symbol-name expand-function) " '\n"  )
	(ti::pmax) (insert ")")
	(setq point (point))
	(eval-last-sexp 'output)
	(delete-region (point-min) point)
	(ti::pmin))
       ((and (stringp symbol)
	     (fboundp sym))
	(message "TinyLisp: macroexpand, sexp was function: %s" symbol))
       (t
	(message "TinyLisp: macroexpand, skipped: %s"
		 (or str "<can't read>")))))))

;;}}}
;;{{{ properties display

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-post-command-config (&optional restore)
  "Disable modes that echo something to the echo-ares.
User can't see string echoed otherwise. Optionally RESTORE."
  (let ((list '(("lisp" . eldoc-mode)
		("."    . paren-message-offscreen)))
	sym
	re)
    (dolist (elt list)
      (setq re (car elt)  sym (cdr elt))
      (when (and (boundp sym)
                 (string-match re (symbol-name major-mode)))
        (put 'tinylisp-mode sym (symbol-value sym))
        (set sym (if restore t nil))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-post-command-run-p ()
  "Check if running post command is allowed."
  (and (not (eq (selected-window) (minibuffer-window)))
       (not (minibuffer-window-active-p (minibuffer-window)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-syntax-post-command ()
  "Show syntax information for current point."
  (when (tinylisp-post-command-run-p)
    (message "[TinyLisp syntax info] %s: %s"
             (char-to-string (following-char))
             (ti::string-syntax-info (following-char)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-syntax-show-mode (&optional arg verb)
  "Constantly show character syntax info, ARG behaves like mode arg. VERB."
  (interactive "p")
  (ti::verb)
  (cond
   (tinylisp--property-show-mode
    (error "Turn off property show mode first."))
   (t
    (ti::bool-toggle tinylisp--syntax-show-mode arg)
    (cond
     (tinylisp--syntax-show-mode
      (add-hook 'post-command-hook 'tinylisp-syntax-post-command nil 'local)
      (tinylisp-post-command-config))
     (t
      (remove-hook    'post-command-hook 'tinylisp-syntax-post-command)
      (tinylisp-post-command-config 'restore)))))
  (if verb
      (message
       "TinyLisp: syntax show mode is %s"
       (if tinylisp--syntax-show-mode
           "on"
         "off"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-property-info (&optional arg)
  "See `tinylisp-property-show' and ARG. Return string 'face-info ov-info'."
  (let ((count      0)
        (face-str   "")
        (ov-str     "")
        prefix-ok
        ovl)
    (if (member arg '(1 (16) (64)))
        (setq face-str
              (format
               "%s"
               (prin1-to-string (text-properties-at (point))))))
    (when (member arg '((4) (16) (64)))
      (setq ovl (ti::compat-overlays-at (point)))
      ;;  When there is only one verlay at point, the message should say
      ;;  "ov" and reserve "ov1" "ov2" for multiple overlays.
      (if (> (length ovl) 1)
          (setq prefix-ok t))
      (dolist (elt ovl)
        (incf  count)
        (setq ov-str
              (format
               "%sov%s%s "
               ov-str
               (if prefix-ok
                   (number-to-string count)
                 "")
               (prin1-to-string (ti::compat-overlay-properties elt))))))
    (concat face-str " " ov-str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-property-post-command ()
  "Display property info according to `tinylisp--property-show-mode'.
This is post command."
  (when (tinylisp-post-command-run-p)
    (let ((record (equal '(64) tinylisp--property-show-mode))
	  (ch     (char-to-string (following-char)))
	  str)
      (setq str
            (format
             "%s:%s"
             (point)
             (tinylisp-property-info tinylisp--property-show-mode)))
      (tinylisp-record-macro record (insert ch str "\n"))
      (message "TinyLisp: %s%s" (if record "r" "") str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-property-show-mode (arg &optional verb)
  "Toggle permanent text property info mode with ARG. VERB.
Utilises local `post-command-hook'.

The echo-area will show following message; definition first, then example.
It says that there is one face and two overlays in current position 12.
The little 'r' appeared at the beginning if the record mode is selected.

   [r]POINT:FACE-PROPERTIES[ovN:OVERLAY-PROPERTIES]
   12:(face highlight)ov1(face region)ov2(owner my)

Input ARG:
   nil                  toggle between 0 and '(16)
   0                    off
   1                    show face properties
   '(4)  C -u           show overlay properties.
   '(16) C -u C -u      show both text properties and overlays.
   '(64) C -u C -u C -u show both text properties and overlays AND
                        record info in buffer `tinylisp--buffer-record'.
VERB                    verbose flag"
  (interactive "P")
  (ti::verb)
  (if tinylisp--syntax-show-mode
      (error "Please turn off Syntax show mode first.")
    (cond
     ((null arg)
      (if (null tinylisp--property-show-mode)
          (setq tinylisp--property-show-mode '(16))
        (setq tinylisp--property-show-mode nil)))
     ((member arg '((4) (16) (64)))
      (setq tinylisp--property-show-mode arg)))
    (cond
     (tinylisp--property-show-mode
      (tinylisp-post-command-config)
      (when verb
        (message
         "TinyLisp: Property show mode is on %s"
         (if (equal arg '(64))
	     "(RECORDING)"
	   "")))
      (add-hook 'post-command-hook
		'tinylisp-property-post-command
		nil
		'local))
     (t
      (tinylisp-post-command-config 'restore)
      (remove-hook 'post-command-hook
		   'tinylisp-property-post-command
		   'local)
      (if verb
	  (message "TinyLisp: Property show mode is off"))))))

;;}}}
;;{{{ Snooping

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-snoop-variables-i-args ()
  "Ask arguments to `tinylisp-snoop-variables'."
  (list
   current-prefix-arg
   (nth
    1
    (assoc
     (completing-read
      "Name of variable snoop list: "
      (ti::list-to-assoc-menu
       (mapcar 'car tinylisp--table-snoop-variables))
      nil
      'match-it)
     tinylisp--table-snoop-variables))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-buffer-local-variables (&optional buffer)
  "Print buffer local variables to BUFFER."
  (interactive)
  (flet ((my-sort2
          (list)
          (sort list
                (function
                 (lambda (a b)
                   (string< (symbol-name (car a))
                            (symbol-name (car b)))))))
         (my-sort1
          (list)
          (sort list
                (function
                 (lambda (a b)
                   (string< (symbol-name a)
                            (symbol-name b)))))))
    (let (var
	  val)
      (or buffer
          (setq buffer (current-buffer)))
      (pop-to-buffer (tinylisp-get-buffer-create tinylisp--buffer-variables))
      (ti::pmax)
      (insert "\nbuffer-local-variables: " (buffer-name buffer) "\n\n" )
      (dolist (elt (my-sort2 (buffer-local-variables buffer)))
        (setq var (car elt))
        (when (and (symbolp var)        ;skip markers etc.
                   (not (memq var '(buffer-undo-list
                                    font-lock-syntax-table))))
          (insert (format "%-30s => %s\n"
                          (symbol-name var)
                          (pp (cdr elt))))))
      (insert "\nframe-parameters: " (buffer-name buffer) "\n\n" )
      (dolist (elt (my-sort2 (frame-parameters)))
        (insert (format "%-30s => %s\n"
                        (symbol-name (car elt))
                        (pp (cdr elt)))))
      (insert "\ncoding variables: " (buffer-name buffer) "\n\n" )
      (dolist (elt (my-sort1
                    (ti::system-get-symbols "coding" '(boundp sym))))
        (unless (memq elt '(coding-system-alist
                            coding-category-list
                            coding-system-list
                            set-coding-system-map))
          (setq val (symbol-value elt))
          (insert (format "%-30s => %s%s\n"
                          (if (ti::listp val) ;; Start separate line
                              "\n"
                            "")
                          (symbol-name elt)
                          (pp val))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-autoload-functions (&optional buffer)
  "Display all autoload functions."
  (interactive)
  (let ((list (ti::system-autoload-function-list))
	doc)
    (if (null list)
        (message "TinyLisp: No autoload functions found in Emacs.")
      (or buffer
          (setq buffer
                (tinylisp-get-buffer-create tinylisp--buffer-autoload)))
      (pop-to-buffer buffer)
      (erase-buffer)
      (insert "\n[TinyLisp] Autoload functions currently in Emacs:\n\n")
      (dolist (func list)
        (setq doc (documentation func))
        (cond
         ((eq doc nil)
          (setq doc "<no documentation>"))
         ((ti::nil-p doc)
          (setq doc "<empty documentation string>")))
        (insert (format "%s: %s\n%s\n\n"
                        (symbol-name func)
                        (or (ti::function-autoload-file func)
                            "<autoload file unknown>")
                        doc))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-match-from-hooks  (regexp)
  "Search all functions that match REGEXP in -hooks -function[s] symbols."
  (interactive "sSearch match from hooks: ")
  (tinylisp-with-current-buffer
   (tinylisp-get-buffer-create tinylisp--buffer-data)
   (ti::pmax))
  (pop-to-buffer (ti::system-match-in-hooks regexp tinylisp--buffer-data))
  (sort-lines nil (point-min) (point-max)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-match-from-variables (var-regexp val-regexp)
  "Search variables for VAR-REGEXP and values matching VAL-REGEXP."
  (interactive "sMatch variable name: \nsMatch content in variable: ")
  (pop-to-buffer tinylisp--buffer-data)
  (ti::pmax)
  (insert "\n")
  (mapatoms
   (function
    (lambda (sym &optional val)
      (when (and (boundp sym)
                 (string-match var-regexp (symbol-name sym))
                 (string-match val-regexp
                               (setq val (prin1-to-string
                                          (symbol-value sym)))))
        (insert (format "[%s] %s\n\n" (symbol-name sym) val)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-snoop-variables (&optional arg list)
  "Display contents of hooks. See `tinylisp--table-snoop-variables'.

ARG can be
   1           With prefix arg, variables values are recorded to
               to buffer `tinylisp--buffer-record' and

   0           Save variables values.
   9           Restore variables values from the saved copies.
   8           Kill saved variable state
   5           Set all variables to nil in list

   C -u        allows editing the variables.

LIST           list of variables.

Flags when viewing, editing echo-area:

+w   Is shown when you're actually modifying the contents.
!    is shown if the variable's state has been saved and is non-nil."
  (interactive (tinylisp-snoop-variables-i-args))
  (let ((write     (equal arg '(4)))
	(read      (eq arg nil))
	(record    (eq arg 1))
	(save      (eq arg 0))
	(restore   (eq arg 9))
	(kill      (eq arg 8))
	(reset     (eq arg 5))
	(msg       (format
		    "(%s) %s"
		    (length list)
		    (mapconcat 'symbol-name list " ")))
	(prop      'tinylisp-original)
	str
	val
	ok)
    (dolist (elt list)
      (setq ok  (boundp elt))
      (cond
       ((or read write record)
        (if ok
            (setq val (prin1-to-string (symbol-value elt)))
          (setq val "<variable does not exist>"))
        (tinylisp-record-macro record
	  (insert (format "%s %s\n" (symbol-name elt)  val)))
        ;; Using rsz-mini we can show whole content.
        (setq str (read-from-minibuffer
                   (format "%s%s%s: "
                           (if write "+w " "")
                           (if (get elt prop)
                               "! "
                             "")
                           (symbol-name elt))
                   val))
        (if write                       ;replace content?
            (set elt (read str))))
       (save
        (put elt prop (symbol-value elt)))
       (kill
        (remprop elt prop))
       (restore
        (set elt (get elt prop)))
       (reset
        (set elt nil))
       (t
        (message "TinyLisp: Unknown arg %s" (prin1-to-string arg)))))
    (cond
     (save  (message "TinyLisp: Saved %s" msg))
     (save  (message "TinyLisp: Restored %s" msg))
     (kill  (message "TinyLisp: Killed saved value copies %s" msg))
     (kill  (message "TinyLisp: Set to nil %s" msg)))))

;;}}}
;;{{{ Occur

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-occur-i-args ()
  "Ask arg1 to `tinylisp-occur'."
  (read-from-minibuffer
   "TinyLisp occur: "
   (nth 1 (tinylisp-read-symbol-at-point))
   nil
   nil
   'tinylisp--occur-history))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-occur (regexp &optional arg)
  "Run occur on REGEXP for whole buffer.
If ARG is non-nil, do not filter comment lines."
  (interactive (list (tinylisp-occur-i-args) current-prefix-arg))
  (let ((obuffer (current-buffer)))
    (ti::occur-macro regexp nil
      (ti::text-re-search-forward regexp)
      (ti::pmin)
      (unless arg
        ;;  Remove comments.
        (let (buffer-read-only)
          (while (re-search-forward "^ *+[0-9]+:\\([ \t]*;.*\\)" nil t)
            (delete-region (line-beginning-position)
                           (min (1+ (line-end-position))
                                (point-max)))))))
    ;;  Keep cursor in original buffer
    (pop-to-buffer obuffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-occur-verbose (regexp &optional arg)
  "Call `tinylisp-occur' as user would with ARG."
  (interactive (list (tinylisp-occur-i-args) current-prefix-arg))
  (when (and (stringp regexp)
             (not (string= "" regexp)))
    (tinylisp-occur regexp arg)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-occur-select-forward (&optional back)
  "Select next line from the occur buffer. You must first run `tinylisp-occur'.
Optionally goes BACK."
  (interactive "P")
  (let ((buffer (get-buffer tinylisp--occur-buffer-name))
	line
	file
	str
	go-buffer)
    (if (null buffer)
        (message "TinyLisp: No occur buffer exist.")
      (tinylisp-with-current-buffer
	  buffer
	;; This is ugly, but I don't know other way to move
	;; point permanently in the buffer. The select-window
	;; is the crucial command to make the point move.
	(save-window-excursion
	  (pop-to-buffer (current-buffer))
	  (select-window (selected-window))
	  (if back
	      (forward-line -1)
	    (forward-line 1)))
	(setq str (ti::read-current-line))
	(if (null (setq line (ti::buffer-match "^\\([0-9]+\\):" 1)))
	    (message "TinyLisp: Can't find line number from occur buffer.")
	  (setq line (string-to-number line))
	  ;;  first line in occur buffer has
	  ;;  "Lines matching "tipgpd" in buffer xxx.el"
	  (if (null (setq file
			  (ti::re-search-check "^Lines matching.* \\(.*\\).$"
					       1 nil 'matched)))
	      (message
	       "TinyLisp: Can't find file name from occur buffer."))))
      (if (and file
               (null (setq go-buffer (get-buffer file))))
          (message "TinyLisp: buffer not exist %s" file)
        (pop-to-buffer go-buffer)
        (ti::goto-line line)
        (message str)))))

;;}}}
;;{{{ debugger: std Emacs

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-debugger-setup ()
  "Define new commands to *Backtrace*."
  (defvar debugger-mode-map nil)        ;no-op ByteComp silencer
  (define-key debugger-mode-map "R" 'tinylisp-debugger-record-value))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-debugger-record-value (exp)
  "Read EXP and record it's value to `tinylisp--buffer-record' buffer."
  (interactive
   (list (read-from-minibuffer
          "Eval: "
          (ti::remove-properties (ti::buffer-read-word "^( \t\n'"))
          read-expression-map t
          'read-expression-history)))
  (let* ((buffer (ti::temp-buffer tinylisp--buffer-record))
         (standard-output buffer))
    (defvar debugger-old-buffer nil)    ;No-op ByteComp silencer.
    (save-excursion
      (if (null (buffer-name debugger-old-buffer))
          ;; old buffer deleted
          (setq debugger-old-buffer (current-buffer)))
      (princ (format "Debugger (%s): " exp))
      (princ (eval-expression exp))
      (terpri))
    (tinylisp-with-current-buffer buffer
                                  (save-excursion
                                    (forward-line -1)
                                    (message (ti::read-current-line))))))

;;}}}
;;{{{ Additional support functions

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-emergency (&optional verb)
  "Restore any dangerously advised functions.
See `tinylisp-eval-at-point'. VERB."
  (interactive)
  (ti::verb)
  (ad-disable-advice 'defconst 'around 'tinylisp)
  (ad-activate 'defconst)
  (if verb
      (message
       "TinyLisp: Function states restored; you can continue as usual.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elint-init ()
  "Prepare buffer for Elint."
  (unless (get 'tinylisp-mode 'elint)
    (tinylisp-safety-belt 'elint-initialize "See elint.el")
    (put 'tinylisp-mode 'elint 'initialized)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elint-buffer ()
  "Elint the buffer."
  (interactive)
  (tinylisp-elint-init)
  (tinylisp-safety-belt 'elint-current-buffer "See elint.el"))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elint-defun ()
  "Elint the buffer."
  (interactive)
  (tinylisp-elint-init)
  (tinylisp-safety-belt 'elint-defun "See elint.el"))

;;; ----------------------------------------------------------------------
;;;
(defadvice edebug-eval-defun (after tinylisp-record-instrumented-function act)
  "Record the function info to `tinylisp--edebug-instrument-table'.
See function `tinylisp-edebug-uninstrument-everything' for more information."
  (tinylisp-defun-sym-macro
   (when sym
     (cond
      ((ad-get-arg 0)
       (message "TinyLisp: instrumented and cached %s (Edebug advice)" name)
       (pushnew
        (list
         sym
         (current-buffer)
         (buffer-file-name))
        tinylisp--edebug-instrument-table
        :test 'equal))
      (t
       (tinylisp-edebug-table-remove-entry sym)))))
  ;; activate again
  (ti::advice-control 'eval-defun "^tinylisp"))

;;; ----------------------------------------------------------------------
;;;
(defadvice eval-last-sexp (after tinylisp-remove-instrumented-function act)
  "Remove possibly edebug instrumented function info.
See `tinylisp-edebug-table-remove-entry'"
  (save-excursion
    (ignore-errors (forward-sexp -1))
    (let ((info (tinylisp-read-function-name-info)))
      (when (cdr-safe info)
        (tinylisp-edebug-table-remove-entry (cdr-safe info))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-table-remove-entry (function)
  "Remove FUNCTION from `tinylisp--edebug-instrument-table'."
  (interactive)
  (let ((elt (assq function tinylisp--edebug-instrument-table)))
    (setq tinylisp--edebug-instrument-table
          (delete elt tinylisp--edebug-instrument-table))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-uninstrument-buffer ()
  "This is same as `eval-buffer', which cancels all edebug information."
  (tinylisp-eval-current-buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-instrument-buffer ()
  "Read whole buffer and instrument every found left flushed `defun'."
  (interactive)
  (let (edebug-all-defs)
    (save-excursion
      (ti::pmin)
      (while (re-search-forward "^(defun " nil t)
        ;;  thi makes Edebug instrument the function
        (message "TinyLisp: instrumenting %s" (ti::read-current-line))
        (eval-defun 'instrument)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-display-instrumented-list  ()
  "List all instrumented function from cache `tinylisp--edebug-instrument-table'.
Show results in `tinylisp--buffer-record'. The display shows

  FUNCTION-NAME  BUFFER-OF-EVAL  LIVE-BUFFER FILE-NAME-FOR-BUFFER"
  (interactive)
  (let ((buffer (ti::temp-buffer tinylisp--buffer-record))
	function
	name
	live-buffer
	live-name
	file)
    (display-buffer buffer)
    (tinylisp-with-current-buffer buffer
      (ti::pmax)
      (dolist (elt tinylisp--edebug-instrument-table)
	(setq function    (nth 0 elt)
	      name        (symbol-name function)
	      buffer      (nth 1 elt)
	      live-buffer (if (buffer-live-p buffer) (get-buffer buffer))
	      live-name   (if live-buffer            (buffer-name live-buffer))
;;;         key         (or live-buffer file)
	      file        (nth 2 elt))
	(insert (format "\n%-20s %-15s %-15s %s"
			name buffer live-name file))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-uninstrument-everything ()
  "Uninstrument every function instrumented via `tinylisp-edebug-instrument'.
When you Edebug you functions, you instrument function here, function
there in different packages and soon you'll find that you don't
remember any more what you have instrumented. You want to get rid of
all Edebug instrumentation when you think you no longer need them.

This function looks up `tinylisp--edebug-instrument-table' and with raw
force reloads every package again thus wiping out Edebug instrumentation."
  (interactive)
  (let (file-list
	buffer-list
	function
	name
	buffer
	live-buffer
	live-name
	file
	key
	tmp)
    (dolist (elt tinylisp--edebug-instrument-table)
      (setq function    (nth 0 elt)
            name        (symbol-name function)
            buffer      (nth 1 elt)
            live-buffer (if (buffer-live-p buffer) (get-buffer buffer))
            live-name   (if live-buffer            (buffer-name live-buffer))
            file        (nth 2 elt)
            key         (or live-buffer file))
      (cond
       ((or (and (stringp key) (member key file-list))
            (and (bufferp key) (memq   key buffer-list)))
        (message "TinyLisp: (edebug) %s %s already wiped"
                 name
                 (or file
                     live-name
                     "")))
       (live-buffer
        (with-current-buffer live-buffer
          (tinylisp-eval-current-buffer))
        (message "TinyLisp: (edebug) wiped %s by re-evaluating buffer %s"
                 name live-name)
        (push buffer buffer-list))
       ((stringp file)
        (load-file file)
        (message "TinyLisp: (edebug) wiped %s by loading file %s" name file)
        (ti::kill-buffer-safe tmp)
        (push file file-list))))
    (setq tinylisp--edebug-instrument-table nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-uninstrument  (&optional verb)
  "Uninstrument function whose _name_ is at current point. VERB.
See `tinylisp-edebug-instrument'."
  (interactive)
  (tinylisp-edebug-instrument 'restore (ti::verb)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-instrument  (&optional uninstrument verb)
  "Instrument or UNINSTRUMENT function _name_ at current point. VERB.

If there is a functon call at cursor position, instrument that function.
This is not same as edebug instrumenting \\[universal-argument]
\\[eval-defun] in `emacs-lisp-mode'. The function name at current point is
located and file is loaded to make edebug to instrument that function.

If there is no function call at point, behave like standard
`edebug-eval-defun' accessed via \\[edebug-eval-defun].

Example

    ;; If point is over the word 'my-function2', that function is
    ;; instrumented

    (defun my-function ()
      (interactive)
      (let ((buffer (buffer-name))
            (case-fold-search t))
         ;;   -!-
         (my-function2 buffer)
          ....

References:

  `tinylisp--edebug-instrument-table'"
  (interactive "P")
  (ti::verb)
  (save-excursion
    (save-window-excursion
      (cond
       ((ignore-errors
          (tinylisp-jump-to-definition
           nil
           (tinylisp-read-word)
           (not 'verb)
           (not 'nodisplay))
          (if uninstrument
              (eval-defun nil)
            (edebug-eval-defun 'instrument))
          t))
       (t
        ;;  No function at point.
        (edebug-eval-defun 'debug))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-checkdoc ()
  "Interactively check document from current point forward.
See `checkdoc-interactive'."
  (interactive)
  (tinylisp-safety-belt 'checkdoc-interactive "See checkdoc.el" (point)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-checkdoc-notes (&optional start)
  "Take notes from current point forward or START from beginning of buffer."
  (interactive "P")
  (tinylisp-require 'checkdoc)
  (let ((buffer (symbol-value 'checkdoc-diagnostic-buffer))
	(checkdoc-arguments-in-order-flag      t)
	(checkdoc-verb-check-experimental-flag t)
	(checkdoc-bouncy-flag                  t) ;; No auto fixing
	checkdoc-spellcheck-documentation-flag    ;; Don't call spell
	checkdoc-autofix-flag)
    (save-excursion
      (if start
          (ti::pmin))
      (with-current-buffer (get-buffer-create buffer)
        (ti::pmax)
        (insert (format "\n\nCheckdoc: %s *** Style check %s"
                        (symbol-value 'checkdoc-version)
                        (ti::date-standard-date 'minutes))))
      (tinylisp-safety-belt
       'checkdoc-continue
       "See checkdoc.el"
       'take-notes))
    (unless (get-buffer-window buffer)
      (display-buffer buffer))
    (with-current-buffer buffer
      (if (fboundp 'turn-on-tinyurl-mode-1)
          (turn-on-tinyurl-mode-1)))
    (when nil ;;#todo: doesn't work
      (let ((win (get-buffer-window buffer))
            point)
        (with-current-buffer buffer
          ;; Go to start of the message
          (ti::pmax)
          (when (re-search-backward "^[\r\n]" nil t)
            (setq point (point))
            (set-window-point win point)))))
    (message "TinyLisp: Checkdoc Take notes done.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-checkdoc-comment-notes ()
  "See `checkdoc'."
  (interactive)
  (tinylisp-safety-belt 'checkdoc-comments "See checkdoc.el" t))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-verify ()
  "Check package layout.
The latest Emacs distribution has improved lisp-mnt.el which has
function `lm-verify', which you should run in your package. It helps
ensuring that you have all the proper keywords in place. Here is rough
valid layout format:

    ;; XXX.el -- proper first line

    ;; Author
    ;; Maintainer
    ;; Created:
    ;; Keywords:

    ;;; Commentary:
    ;;; Change Log:
    ;;; Code:

    ;;; XXX.el ends here

See unix what(1) and GNU RCS indent(1) why you should adopt a style where
you use @(#) and $Keywords$."
  (interactive)
  (require 'lisp-mnt)
  (if (not (string= (symbol-value 'lm-history-header)
                    "Change Log\\|History"))
      (message "\
TinyLisp: your lisp-mnt.el is too old to have improved checking. Get newer.")
    (call-interactively 'lm-verify)))

;;}}}
;;{{{ lisp-mnt.el

;;#todo: Sent patch to FSF to include these in lisp-mnt.el

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-tag-check-and-fix (tag &optional on-error)
  "Correct misplaced lisp-mnt.el tag. Stop ON-ERROR.
Return:
 'missing
 'fixed
 nil         Means ok."
  (ti::pmin)
  (let ((regexp (concat "^;+[ \t]*" tag ":[ \t]*$"))
	(reference (format ";;; %s:" tag))
	status
	case-fold-search)
    (if (not (re-search-forward regexp nil t))
        (setq status 'missing)
      (unless (string= (match-string 0) reference)
        (replace-match reference)
        (setq status 'fixed))
      (forward-line -1)
      (if (looking-at "^[ \t]*$")
          (forward-line 1)
        (forward-line 1)
        (insert "\n")
        (setq status 'fixed))
      (forward-line 1)
      (unless (looking-at "^[ \t]*$")
        (insert "\n")
        (setq status 'fixed)))
    (when (and on-error
               (eq status 'missing))
      (pop-to-buffer (current-buffer))
      (error "Lisp-mnt: missing tags `;;; %s:'" tag))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-tag-check-and-fix-buffer (&optional on-error)
  "Check all Lisp commentary tags and fix as needed. Stop ON-ERROR.
Return: '((missing-tags) (fixed-tags))."
  (interactive "P")
  (let (missing
	fixed
	stat)
    (if (and (featurep 'folding)
             folding-mode)
        (folding-open-buffer))
    (dolist (tag '("Commentary" "Change Log" "Code"))
      (setq stat (tinylisp-lisp-mnt-tag-check-and-fix tag on-error))
      (cond
       ((eq stat 'missing)
        (push tag missing))
       ((eq stat 'fixed)
        (push tag fixed))))
    (if (or missing fixed)
        (list missing fixed))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-tag-check-and-fix-file (file &optional on-error)
  "Check all Lisp commentary tags on FILE and fix as needed. Stop ON-ERROR.
Return: '((missing-tags) (fixed-tags))."
  (interactive "fLisp file: \nP")
  (let ((buffer (find-buffer-visiting (expand-file-name file))))
    (unless buffer
      (setq buffer (tinylisp-find-file-noselect file)))
    (with-current-buffer buffer
      (tinylisp-lisp-mnt-tag-check-and-fix-buffer on-error))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-tag-check-and-fix-dir (dir &optional on-error)
  "Check all Lisp commentary tags and fix as needed. Stop ON-ERROR.
Return.
 '((file ((missing-tags) (fixed-tags))) ..)."
  (interactive "DDir: \nP")
  (let (stat
	list)
    (dolist (file (directory-files dir 'abs "\\.el$"))
      (setq stat (tinylisp-lisp-mnt-tag-check-and-fix-file file on-error))
      (if stat
          (push (list file stat) list)))
    list))

;; (tinylisp-lisp-mnt-tag-check-and-fix-dir "~/elisp/tiny/lisp" 'err)

;;}}}

(provide   'tinylisp)

;;  These must be set, otherwise the mode setup will not activate
;;  correctly when user calls M-x tinylisp-mode.

(add-hook 'tinylisp--mode-define-keys-hook
          'tinylisp-mode-define-keys)
(add-hook 'tinylisp--elp-summary-mode-define-keys-hook
          'tinylisp-elp-summary-mode-define-keys)

(tinylisp-install-menu)
(run-hooks 'tinylisp--load-hook)

;;; tinylisp.el ends here
