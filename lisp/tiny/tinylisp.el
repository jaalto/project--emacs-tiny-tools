;;; tinylisp.el --- Emacs lisp programming help grab-bag

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2009 Jari Aalto
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
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;      ;;  Select some unused, non-shifted, fast prefix key.
;;      ;;  My kbd accesses $ without shiff and it is seldom used
;;      ;;  in lisp. Other alternatives: "!", "_" ":"
;;
;;      (setq tinylisp-:mode-prefix-key  "$")
;;      (setq tinylisp-:load-hook 'tinylisp-install)
;;      (require 'tinylisp)
;;
;;      (setq tinylisp-:load-hook nil)
;;
;; Or prefer following autoload: your Emacs loads this package only
;; when you need it.
;;
;;      (autoload 'tinylisp-mode                "tinylisp" "" t)
;;      (autoload 'turn-on-tinylisp-mode        "tinylisp" "" t)
;;      (add-hook 'emacs-lisp-mode-hook         'turn-on-tinylisp-mode)
;;      (add-hook 'lisp-interaction-mode-hook   'turn-on-tinylisp-mode)
;;
;;      (setq tinylisp-:load-hook 'tinylisp-install)
;;      (global-set-key "\C-ce" 'tinylisp-mode)  ; mode on/off
;;      (global-set-key "\C-cmE" 'eldoc-mode)    ; In lastest Emacs
;;
;; If you don't want to use the echo-menu, but regular keymap calls
;; instead, put following into your ~/.emacs. This must be before any
;; other TinyLisp settings. You must reload package every time if you
;; change this setting.
;;
;;      (setq tinylisp-:menu-use-flag nil)
;;
;; To manually install or uninstall mode, call:
;;
;;      M-x     tinylisp-install
;;      M-x     tinylisp-uninstall
;;
;; If you have any questions, use this function
;;
;;      M-x tinylisp-submit-bug-report

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
;;      o   $ e A Restore (a)ll elp'd functions
;;      o   $ -   Eval buffer containing functions (or eval single function)
;;      o   $ e I  Instrument all functions in buffer (or single function)
;;      o   $ e h     Run the harness test that calls the functions
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
  (ti::package-use-dynamic-compilation)
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
  (defvar checkdoc-autofix-flag)
  ;;   During bute compiling it's best to see from where the
  ;;   libraries are loaded. You can also check *Messages*
  (defun tinylisp-locate-library (lib)
    "Print message if located LIB."
    (let ((loc (locate-library lib)))
      (when loc
	(message "tinyLisp.el: %s" loc)
	t)))

  (let ((count 0))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. eldoc ..
    (when (and nil ;; 2004-10-10 disabled.
	       (not (tinylisp-locate-library "eldoc")))
      (incf count)
      (message "\
  **  tinylisp.el: Hm, no eldoc.el found.
		   Emacs function parameter coding help is not available.
		   This package is included in latest Emacs versions.
		   You have to upgrade your Emacs."))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . rsz ..
    (when (and nil ;; 2004-10-10 disabled.
	       ;; XEmacs package is in different name
	       (null (or (tinylisp-locate-library "rsz-minibuf")
			 (tinylisp-locate-library "rsz-mini"))))
      (incf count)
      (message "\
  ** tinylisp.el: Hm, no rsz-mini.el or rsz-minibuf.el found.
		  This package is included in latest Emacs versions.
		  You have to upgrade your Emacs."))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . find-func ..
    (unless (or
	     ;; in XEmacs-20.3(beta) there is no
	     ;; "find-func.el", instead `find-function' is in "help.el" and so
	     ;; in fact dumped with xemacs.
	     (fboundp 'find-function)
	     ;;  In Emacs 20 it is in separate package.
	     (locate-library "find-func"))
      (incf count)
      (message "\
  ** tinylisp.el: Hm, no find-func.el found.
		  Upgrade tot latest Emacs and XEmacs."))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. elint ..
    (if (and nil ;; 2004-10-10 disabled.
	     (not (tinylisp-locate-library "elint")))
	(progn
	  (incf count)
	  (message "\
  ** tinylisp.el: Hm, no elint.el found. No code check features available.
		  Package is included in latest Emacs."))
      (autoload 'elint-initialize     "elint")
      (autoload 'elint-current-buffer "elint" "" t)
      (autoload 'elint-defun          "elint" "" t))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. checkdoc ..
    (defvar checkdoc-version)
    (if (and nil ;; 2004-10-10 disabled.
	     (not (tinylisp-locate-library "checkdoc")))
	(progn
	  (incf count)
	  (message "\
  ** tinylisp.el: Hm, no checkdoc.el found.
		  No lisp package syntax checks available.
		  Upgrade your Emacs."))
      (autoload 'checkdoc-interactive                 "checkdoc" "" t)
      (autoload 'checkdoc-eval-current-buffer         "checkdoc" nil t)
      (autoload 'checkdoc-current-buffer              "checkdoc" nil t)
      (autoload 'checkdoc                             "checkdoc" nil t)
      (autoload 'checkdoc-continue                    "checkdoc" nil t)
      (autoload 'checkdoc-comments                    "checkdoc" nil t)
      (autoload 'checkdoc-rogue-spaces                "checkdoc" nil t)
      (autoload 'checkdoc-eval-defun                  "checkdoc" nil t)
      (autoload 'checkdoc-defun                       "checkdoc" nil t)
      (autoload 'checkdoc-minor-mode                  "checkdoc" nil t)
      (autoload 'checkdoc-find-error-mouse            "checkdoc" nil t)
      (autoload 'checkdoc-find-error                  "checkdoc" nil t))
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . elp ..
    (if (and nil ;; 2004-10-10 disabled.
	     (not (tinylisp-locate-library "elp")))
	(progn
	  (incf count)
	  (message "\
  ** tinylisp.el: Hm, no elp.el found.
		  Lisp profiling functions are not available.
		  This package is included in latest Emacs and XEmacs."))
      ;;  This pretends the functions exist and avoids byte compiler errors.
      (defvar   elp-all-instrumented-list nil)
      (defvar   elp-function-list         nil)
      (defvar   elp-master                        nil)
      (defvar   elp-results-buffer                "*ELP Profiling Results*")
      (defvar   elp-reset-after-results   nil)
      (autoload 'elp-instrument-function              "elp" "" t)
      (autoload 'elp-restore-function                 "elp" "" t)
      (autoload 'elp-instrument-list                  "elp" "" t)
      (autoload 'elp-instrument-package               "elp" "" t)
      (autoload 'elp-restore-list                     "elp" "" t)
      (autoload 'elp-restore-all                      "elp" "" t)
      (autoload 'elp-reset-function                   "elp" "" t)
      (autoload 'elp-reset-list                       "elp" "" t)
      (autoload 'elp-reset-all                        "elp" "" t)
      (autoload 'elp-set-master                       "elp" "" t)
      (autoload 'elp-unset-master                     "elp" "" )
      (autoload 'elp-wrapper                          "elp" "" )
      (autoload 'elp-sort-by-call-count               "elp" "" )
      (autoload 'elp-sort-by-total-time               "elp" "" )
      (autoload 'elp-sort-by-average-time             "elp" "" )
      (autoload 'elp-output-result                    "elp" "" )
      (autoload 'elp-results                          "elp" "" t)
      (autoload 'elp-submit-bug-report                "elp" "" t))
    (unless (zerop count)
      (message "\
  ** tinylisp.el: Some files were not found. This is not fatal.
		  The package will adjust accoding to available features."))))

(ti::package-defgroup-tiny TinyLisp tinylisp-: tools
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

(defcustom tinylisp-:menu-use-flag t
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
;;          (setq tinylisp-:mode-prefix-key "C-cE")

;;;###autoload (autoload 'tinylisp-commentary    "tinylisp" "" t)
;;;###autoload (autoload 'tinylisp-mode          "tinylisp" "" t)
;;;###autoload (autoload 'turn-on-tinylisp-mode  "tinylisp" "" t)
;;;###autoload (autoload 'turn-off-tinylisp-mode "tinylisp" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinylisp-" " E" "$" "E" 'TinyLisp "tinylisp-:" ;1-6

   "This minor mode is used along with the lisp major modes. You can
evaluate expressions, reread functions, check your lisp packages
syntax, create autoloads and do many more things.

Defined keys:

\\{tinylisp-:mode-prefix-map}"

   "Emacs Lisp extras"                  ;7

   nil                                  ;8

   "Emacs Lisp menu."                   ;9

   (list                                ;arg 10
    tinylisp-:mode-easymenu-name
    ["Eval whole buffer"              tinylisp-eval-current-buffer           t]
    ["Eval whole buffer, `load'"      tinylisp-eval-current-buffer-from-file  t]
    ["Eval whole buffer as defconst"  tinylisp-eval-current-buffer-defconst t]
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
     ["Quick autoloads from buffer"  tinylisp-autoload-quick-build-from-buffer   t]
     ["Quick autoloads from file"    tinylisp-autoload-quick-build-from-file     t]
     ["Quick autoloads from directory" tinylisp-autoload-quick-build-from-dir    t]
     ["Quick autoloads recursive"    tinylisp-autoload-quick-build-dir-recursive t]
     "----"
     ["Loaddefs from file"      tinylisp-autoload-generate-loaddefs-file      t]
     ["Loaddefs from directory" tinylisp-autoload-generate-loaddefs-dir       t]
     ["Loaddefs recursive"      tinylisp-autoload-generate-loaddefs-recursive t])

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
      (tinylisp-:menu-use-flag
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
       (define-key map "ar"  'tinylisp-autoload-quick-build-dir-recursive)
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
       (define-key map "1f"      'tinylisp-face-list-known-faces)
       (define-key map "1p"      'tinylisp-process-kill)
       (define-key map "1P"      'list-processes))))))

;;; ................................................... &&mode-summary ...

;;;###autoload (autoload 'tinylisp-elp-summary-mode          "tinylisp" "" t)
;;;###autoload (autoload 'turn-on-tinylisp-elp-summary-mode  "tinylisp" "" t)
;;;###autoload (autoload 'turn-off-tinylisp-elp-summary-mode "tinylisp" "" t)

(eval-and-compile

  (ti::macrof-minor-mode-wizard
   "tinylisp-elp-summary-" " Elp-sum" nil " Elp-sum" 'TinyLisp
   "tinylisp-:elp-summary-"             ;1-6

   "Commands to help sorting elp summary buffer.
Defined keys:

\\{tinylisp-:elp-summary-prefix-mode-map}"

   "Elp summary sort"                   ;7

   nil                                  ;8

   "Elp summary sort menu."             ;9

   (list                                ;arg 10
    tinylisp-:elp-summary-mode-easymenu-name
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

(defcustom tinylisp-:load-hook nil
  "*Hook that is run when package is loaded.
A good value could be '(turn-on-tinylisp-mode-all-buffers) to activate
the minor mode in every Emac slisp buffer."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp-:find-func-list-hook 'tinylisp-highlight-default
  "*Hook run when tinylisp-find-function-list-hook has displayed the list."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp-:find-var-list-hook 'tinylisp-highlight-default
  "*Hook run when `tinylisp-find-function-list' has displayed the list."
  :type  'hook
  :group 'TinyLisp)

(defcustom tinylisp-:with-current-buffer-hook '(turn-on-tinylisp-mode)
  "*Hook run after ´tinylisp-with-current-buffer'."
  :type  'hook
  :group 'TinyLisp)

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinylisp-:register ?\'
  "*An Emacs register that is used e.g. for saving point or copying text."
  :type  'character
  :group 'TinyLisp)

(defcustom tinylisp-:macroexpand-function-list
  '("cl-prettyexpand" "macroexpand")
  "*Completion list of function STRINGS to expand macro call.
You can use commands `cl-prettyexpand', which sometimes does
good formatting, but does not necessarily expand to what you want to see.
The default command is `macroexpand'."
  :type '(repeat string)
  :group 'TinyLisp)

(defcustom tinylisp-:table-reverse-eval-alist
  '((add-hook       . remove-hook)
    (remove-hook    . add-hook))
  "*Table of reverse commands. Format '((ORIG-FSYM . REVERSE-FSYM) ..)."
  :type  'list
  :group 'TinyLisp)

(defcustom tinylisp-:table-snoop-variables
  '(("hook-command"
     (pre-command-hook
      post-command-hook
      post-command-idle-hook))
    ("hook-file"
     (write-file-hooks
      find-file-hook
      find-file-hooks
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
      write-file-hooks
      find-file-hook
      find-file-hooks
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

(defvar tinylisp-:harness-flag nil
  "Described in function `tinylisp-elp-harness'.
This variable is set to t when harness is on going and set to
nil when harness test is over.")

(defvar tinylisp-:call-chain nil
  "List of buffers and buffer positions. '(mark mark ..)
Whenever you call `tinylisp-jump-to-definition' the current positions
is recoded and one more element to the _beginning_ of list is added.
You can navigate back with `tinylisp-back-to-definition' and the first
element from the list is removed.")

(defvar tinylisp-:buffer-elp "*tinylisp-elp*"
  "Temporary elp info buffer.")

(defvar tinylisp-:buffer-autoload "*tinylisp-autoloads*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-variables "*tinylisp-variables*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-data "*tinylisp-data*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-library "*tinylisp-library*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-record "*tinylisp-record*"
  "Record variable contents to this buffer.")

(defvar tinylisp-:buffer-tmp "*tinylisp-tmp*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-macro "*tinylisp-macroexpand*"
  "Temporary buffer.")

(defvar tinylisp-:buffer-eval " *tinylisp-eval*"
  "Temporary buffer.")

(defconst tinylisp-:regexp-macro-definition
  "^\\(defun\\*\\|defcustom\\|defgroup\\|defadvice\\)"
  "Regexp for commands that define macros, like `defcustom' `defgroup'.")

(defconst tinylisp-:regexp-function
  (concat
   "^(\\("
   ;;  cl DEFINES defun* macro
   "defun\\*?\\|defsubst\\|defmacro\\|defalias"
   ;; See SEMI poe.el
   "\\|defun-maybe\\|defmacro-maybe\\|defalias-maybe"
   ;; see Gnus nntp.el for deffoo
   "\\|deffoo\\|defadv"
   "\\)[ \t]+\\([^ \t\n]+\\)")
  "Regexp to match functions.
This must have SUBMATCH1 and SUBMATCH2 which represent function
type and name.")

(defconst tinylisp-:regexp-variable
  (concat
   "^(\\("
   ;;  Normal lisp variables
   "defvar\\|defconst\\|defvaralias"
   ;; Custom.el defined variables in 19.35
   "\\|defgroup\\|defcustom"
   "\\)[ \t]+\\([^ \t\n]+\\)")
  "Regexp to match variables.
This must have SUBMATCH1 and SUBMATCH2 which represent
variable type and name.")

(defvar tinylisp-:variable-not-charset "^][()'`\", \t\n\r"
  "When reading variable from buffer, unse this is character set.
Notice that ^ at the beginning of character set reverses it.")

(defvar tinylisp-:find-error nil
  "'Find error' function's data.")

(defvar tinylisp-:occur-history nil
  "History.")

(defvar tinylisp-:elp-regexp-history  nil
  "History.")

(defvar tinylisp-:elp-not-regexp-history  nil
  "History.")

(defvar tinylisp-:elp-master-history  nil
  "History.")

;;  Too bad this is hard coded in emacs..
(defvar tinylisp-:occur-buffer-name "*Occur*"
  "Emacs Occur buffer.")

(defvar tinylisp-:edebug-instrument-table  nil
  "Edebug instrumentation information.

Format:

  '((function buffer-pointer buffer-file-name)
    (function buffer-pointer buffer-file-name)
    ..)")

;;}}}
;;{{{ setup: private, mode

;;; These must not be made buffer local.

(defvar tinylisp-:property-show-mode nil
  "Property show mode (flag).")

(defvar tinylisp-:syntax-show-mode nil
  "Property show mode (flag).")

;;}}}
;;{{{ setup: menu

(defvar tinylisp-:menu-main) ;;  Just a forward declaration

(defun tinylisp-install-menu ()
  "Install `tinylisp-:menu-main'."
  ;;  this is a function because if user changes prefix key and
  ;;  calls tinylisp-install, we must reflect the change here in
  ;;  self insert command.
  ;;

  (defconst tinylisp-:menu-main         ;bookmark -- &menu
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
      (cons ??  'tinylisp-:menu-help)
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
      (cons ?\C-e  'tinylisp-:menu-edebug)
      (cons ?\C-v  (list '(call-interactively 'tinylisp-version)))
      (cons ?a       'tinylisp-:menu-autoload)
      (cons ?i       'tinylisp-:menu-info)
      (cons ?e       'tinylisp-:menu-elp)
      (cons ?E       'tinylisp-:menu-elint)
      (cons ?H       'tinylisp-:menu-help)
      (cons ?b       'tinylisp-:menu-buffers)
      (cons ?c       'tinylisp-:menu-checkdoc)
      (cons ?C       'tinylisp-:menu-compile)
      (cons ?l       'tinylisp-:menu-lisp-library)
      (cons ?1       'tinylisp-:menu-misc-1)
      (cons ?X       'tinylisp-:menu-lisp-mnt)
      ;; Self insert command
      ;;     User may have defined multichararcter minor map entry
      ;;     like C-cE, we only do self insert if it is NOT
      ;;     multicharacter.
      (cons (string-to-char             ;get first char
	     (substring tinylisp-:mode-prefix-key 0 1))
	    (list
	     '(let ((key (ti::keymap-single-key-definition-p
			  tinylisp-:mode-prefix-key)))
		(if (characterp key)
		    (insert tinylisp-:mode-prefix-key)
		  (message "\
TinyLisp: Can't self-insert. Prefix is not one charcracter.")))))))
    "Emacs Lisp coding help menu.
Documentation of variable `tinylisp-:menu-main' which is main menu
for mode function `tinylisp-mode'. You can access the mode with
\\[tinylisp-mode]. Prefix key for the minor mode is defined in
`tinylisp-:mode-prefix-key'.

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
       variable `tinylisp-:table-reverse-eval-alist'. E.g. if you see
       `add-hook', the statement is interpreted as `remove-hook'.

    C - m (RET)

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
       debug tags.  If the byte compilation gives a weird error and does not
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

    S  Snoop variables. See `tinylisp-:table-snoop-variables'
       Following prefix arguments are recognized:
       1           Record snooped values to
		   to buffer `tinylisp-:buffer-record'
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
    c   Checkdoc, docstring syntax checker menu
    C   Byte (C)ompilation menu.
    e   Elp menu. Emacs lisp profiler menu
    E   Elint menu. Emacs Lisp code syntax checker menu
    H   Help menu.
    i   Info menu. Find adviced functions, find from hooks/variables
    l   Library menu. Load, find lisp libraries for editing.
    1   Misc menu 1: Display face settings, process kill menu

    C-e Edebug, Emacs Lisp debugger menu"))

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-misc-1
  (list
   '(format "%sMisc 1: f)onts F)onts all p)rocess-kill P)rocess list"
	    (if current-prefix-arg
		(format "%s "  (prin1-to-string current-prefix-arg))
	      ""))
   (list
    (cons ?f  (list '(tinylisp-face-list-font-lock-faces)))
    (cons ?F  (list '(tinylisp-face-list-known-faces)))
    (cons ?p  (list '(tinylisp-process-kill)))
    (cons ?P  (list '(list-processes)))
    (cons ?/  'tinylisp-:menu-main)))
  "*Miscellaneous interface: Processes and fonts.
/       Back to root menu
q       Quit menu
f       List font lock colors available.
F       List ALL known faces.
p       Kill running processes interactively.
P       List running processes.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-lisp-library
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

    (cons ?/   'tinylisp-:menu-main)))
  "*Lisp library interface:
/       Back to root menu
q       Quit menu

s       Try to loate file where symbol was defined. This relies on
	internal representation of symbols inside Emacs `load-history'.

l       Load one Lisp library with completion into Emacs. (evaluate)

L       Load again libraries inside Emacs matching regexp. E.g. if you want to
	reload all of present gnus, supply regexp `gnus'

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
(defconst tinylisp-:menu-compile
  (list
   '(format "%sByte-Compile: c)ompile t)tree for compile"
	    (if current-prefix-arg
		(format "%s "  (prin1-to-string current-prefix-arg))
	      ""))
   (list
    (cons ?c  '( (tinylisp-byte-compile-buffer)))
    (cons ?s  '( (tinylisp-byte-compile-sexp)))
    (cons ?t  '( (tinylisp-byte-compile-display-call-tree)))
    (cons ?/  'tinylisp-:menu-main)))
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
(defconst tinylisp-:menu-elp
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
     (?/  . tinylisp-:menu-main)))
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

h   Harness test. Eval everything 3 times from current point forward
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

(defconst tinylisp-:menu-info
  '("info: a)d e)macs f)ile-sym o)hooks l)ocal-vars s)ym v)ar A)utoload"
    ((?A  . ( (call-interactively 'tinylisp-find-autoload-functions)))
     (?a  . ( (call-interactively 'tinylisp-ad-match)))
     (?e  . ( (call-interactively 'tinylisp-library-info-emacs)))
     (?f  . ( (call-interactively 'tinylisp-library-symbol-information)))
     (?o  . ( (call-interactively 'tinylisp-find-match-from-hooks)))
     (?l  . ( (call-interactively 'tinylisp-find-buffer-local-variables)))
     (?v  . ( (call-interactively 'tinylisp-find-match-from-variables)))
     (?s  . ( (call-interactively 'ti::system-describe-symbols)))
     (?/  . tinylisp-:menu-main)))
  "Display information about lisp symbols in Emacs

/   Back to root menu
q   Quit menu

a   List all adviced functions that match advice NAME. E.g. to find all
    `my' advices.

e   Show all libraries and symbols loaded into Emacs known by `load-history'.

f   Describe file symbols. Gather all documentation from symbols in FILE.
    You have to load the file into Emacs first (eval it with \\[load-file]),
    because this function reads the documentation properties from memory.

h   Search a match from contents of all -hook -function -functions symbols
    E.g. you can locate all hooks that have function matching 'my'.

l   Decribe library symbols. This is like `f', but you do not need to give
    the full path name, but the file will be located along `load-path'.

L   Show buffer local variables.

s   Search any symbol (variable or function) from Emacs obrray with REGEXP.

v   Search all variables matching variable-REGEXP and whose value match
    VALUE-REGEXP.")

(defconst tinylisp-:menu-buffers
  '("go buffer: a)utoload rR)ecord v)vars f)uncs e)val E)lp"
    ((?a  . ( (tinylisp-b-autoload)))
     (?r  . ( (tinylisp-b-record)))
     (?R  . ( (tinylisp-b-record-empty)))
     (?v  . ( (tinylisp-b-variables)))
     (?f  . ( (tinylisp-b-funcs)))
     (?e  . ( (tinylisp-b-eval)))
     (?E  . ( (tinylisp-b-elp)))
     (?/  . tinylisp-:menu-main)))
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
(defconst tinylisp-:menu-elint
  '("Elint: RET)buffer SPC)defun"
    (
     (?\C-m . ( (tinylisp-elint-buffer)))
     (?\    . ( (tinylisp-elint-defun)))
     (?/    . tinylisp-:menu-main)))
  "Elint interface: Check code syntax.
/       Back to root menu
q       Quit menu
RET     Lint buffer
SPC     Lint defun")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-help
  '("Help: m)mode c)commentary v)ersion"
    ((?m . ( (tinylisp-mode-help)))
     (?c . ( (tinylisp-commentary)))
     (?v . ( (tinylisp-version)))
     (?/ . tinylisp-:menu-main)))
  "Help menu:
/       Back to root menu
q       Quit menu
m   `tinylisp-mode' Mode description
v   `tinylisp-version'
c   `tinylisp-commentary'")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-autoload
  '("Autoloads: b)uffer f)ile d)dir r)ecur Loaddefs: F)ile D)ir R)ecursive"
    ((?b . ( (call-interactively 'tinylisp-autoload-quick-build-from-buffer)))
     (?f . ( (call-interactively 'tinylisp-autoload-quick-build-from-file)))
     (?d . ( (call-interactively 'tinylisp-autoload-guick-build-from-dir)))
     (?r . ( (call-interactively 'tinylisp-autoload-quick-build-dir-recursive)))
     (?F . ( (call-interactively 'tinylisp-autoload-generate-loaddefs-file)))
     (?D . ( (call-interactively 'tinylisp-autoload-generate-loaddefs-dir)))
     (?R . ( (call-interactively 'tinylisp-autoload-generate-loaddefs-recursive)))
     (?/ . tinylisp-:menu-main)))
  "Help menu:
/       Back to root menu
q       Quit menu

Quick autoloads: list of all macros and functions

    b   Show autoloads from buffer which defines `buffer-file-name'.
	With prefix argument, ask package

    f   Write autoloads from FILE.wl to FILE-autoload.el.

    d   From DIRECTORY matching REGEXP, exluding REGEXP, write all autolaods
	to each FILE-autoload.el

Real autolaods: FILE-loaddefs.el

    F   Generate FILE-loaddefs.el from FILE.el   (FIXME: not yet implemented)
    D   Same, but for whole directory.
    R   Same, but recursively for all directories.")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-lisp-mnt
  '("Lisp-mnt: RET)verify SPC)fix tags f)file d)directory"
    ((?\C-m . ( (tinylisp-lisp-mnt-verify)))
     (?\    . ( (tinylisp-lisp-mnt-tag-check-and-fix-buffer 'error)))
     (?f    . ( (tinylisp-lisp-mnt-tag-check-and-fix-file   'error))) ;;#todo:
     (?d    . ( (tinylisp-lisp-mnt-tag-check-and-fix-dir    'error))) ;;#todo:
     (?/    . tinylisp-:menu-main)))
  "Lisp-mnt.el interface: check package layout syntax.

/       Back to root menu
q       Quit menu
RET     Check whole buffer with `lm-verify'
SPC     Check whole buffer tags and automatically fix them
f       Check file
d       Check all files in directory")

;;; ----------------------------------------------------------------------
;;;
(defconst tinylisp-:menu-edebug
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
     (?/    . tinylisp-:menu-main)))
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
(defconst tinylisp-:menu-checkdoc
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
;;{{{ version

;;; ....................................................... &v-version ...

;;;###autoload (autoload 'tinylisp-version "tinylisp" "Display commentary" t)
(eval-and-compile
  (ti::macrof-version-bug-report
   "tinylisp.el"
   "tinylisp"
   tinylisp-:version-id
   "$Id: tinylisp.el,v 2.88 2007/05/01 17:20:46 jaalto Exp $"
   '(tinylisp-:version-id
     tinylisp-:debug
     tinylisp-:load-hook
     tinylisp-:find-func-list-hook
     tinylisp-:find-var-list-hook
     tinylisp-:menu-use-flag
     tinylisp-:macroexpand-function-list
     tinylisp-:table-reverse-eval-alist
     tinylisp-:table-snoop-variables
     tinylisp-:regexp-macro-definition
     tinylisp-:regexp-function
     tinylisp-:regexp-variable)
   '(tinylisp-:debug-buffer)))

;;}}}
;;{{{ macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-with-current-buffer 'lisp-indent-function 1)
(defmacro tinylisp-with-current-buffer (buffer &rest body)
  "Make BUFFER and run hook `tinylisp-:with-current-buffer-hook'."
  `(with-current-buffer ,buffer
     ,@body
     (run-hooks 'tinylisp-with-current-buffer-hook)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinylisp-read-word ()
  "Read word under point."
  (let ((str (or (ti::remove-properties
		  (ti::buffer-read-word tinylisp-:variable-not-charset))
		 (when (bolp)
		   (ti::buffer-match
		    (concat "^[^ \t\n\r]*\\(["
			    tinylisp-:variable-not-charset
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
  (let* ((re-f    (substring tinylisp-:regexp-function
			     1 (length tinylisp-:regexp-function)))

	 (re-v    (substring tinylisp-:regexp-variable
			     1 (length tinylisp-:regexp-variable)))
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
      (push data tinylisp-:call-chain)
    (if (null tinylisp-:call-chain)
	(error "tinylisp-:call-chain is empty, nothing to pop.")
      (let* ((mark (pop tinylisp-:call-chain)))
	(goto-char mark)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-symbol-do-macro 'lisp-indent-function 2)
(defmacro tinylisp-symbol-do-macro (string noerr &rest body)
  "Execute body if string is interned.
Input:
  STRING    function or variable name
  NOERR     If nil, then call error. if Non-nil then print message if
	    STRING was not interned.
  BODY."
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
       (tinylisp-with-current-buffer (ti::temp-buffer tinylisp-:buffer-record)
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
  `(let* ((buffer  (current-buffer))
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
  (unless tinylisp-:menu-main
    (tinylisp-install-menu))
  (ti::menu-menu 'tinylisp-:menu-main arg))

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
  (let* ()
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
    (ti::add-hooks 'tinylisp-:mode-define-keys-hook
		   'tinylisp-mode-define-keys uninstall)
    ;; tinylisp-elp-summary-install-mode
    (ti::add-hooks 'tinylisp-:elp-summary-mode-define-keys-hook
		   'tinylisp-elp-summary-mode-define-keys
		   uninstall)
    (cond
     ((boundp 'debugger-mode-hook)
      (ti::add-hooks '(tinylisp-debugger-setup turn-on-tinylisp-mode)
		     'debugger-mode-hook
		     uninstall))
     (uninstall
      (ti::advice-control 'debugger-mode "^tinylisp" 'disable))
     (t
      ;;  19.x-20.2 doesn't have the debugger hook
      (defadvice debugger-mode  (after tinylisp act)
	"Run `tinylisp-debugger-setup'."
	(tinylisp-debugger-setup)
	(turn-on-tinylisp-mode))))))

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
;;;
(defadvice byte-compile-file (around tinylisp act)
  "Change interactive prompt and offer current buffer for compiling(.el)."
  ;;
  ;; byte-compile-file (filename &optional load)
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
  "This advice is only used in TinyLisp and elsewhere inactivated.
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
  (let* ((list (process-list)))
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
  (interactive)
  (let* ((getface 'get-face)
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
(defun tinylisp-face-print (buffer face-list)
  "Insert description to BUFFER for each symbol in FACE-LIST."
  (let* ((list (tinylisp-face-list-unique face-list))
	 beg
	 var
	 face)
    (when list
      (setq buffer (ti::temp-buffer tinylisp-:buffer-tmp 'clear))
      (with-current-buffer buffer
	(dolist (elt list)
	  (setq var  (car elt)
		face (nth 1 elt))
	  (insert (format "%-35s" (symbol-name var)))
	  (setq beg  (point))
	  (insert "abcdef12345  ")
	  (set-text-properties beg (point) (list 'face face))
	  (if (ti::emacs-p)
	      (insert (format " fg: %-15s  bg: %s\n"
			      (face-foreground face)
			      (face-background face)))
	    (insert (format "\n  fg: %-15s\n  bg: %s\n"
			    (face-foreground face)
			    (face-background face)))))
	(sort-lines nil (point-min) (point-max)))
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-face-list-font-lock-faces ()
  "List known font lock faces and colors used."
  (interactive)
  (cond
   ((not (featurep 'font-lock))
    (message "tinylisp.el: font-lock.el is not loaded. No faces."))
   (t
    (let ((symbols
	   (ti::system-get-symbols "^font-lock-.*face$" '(boundp sym))))
      (when symbols
	(let ((buffer (ti::temp-buffer tinylisp-:buffer-tmp 'clear)))
	  (tinylisp-face-print buffer symbols)
	  (display-buffer buffer)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-face-list-known-faces ()
  "List all known 'face' variables."
  (interactive)
  (let* ((symbols (ti::system-get-symbols
		   "face"
		   '(or (boundp sym)
			(and (fboundp 'get-face) ;;  XEmacs
			     (get-face sym))
			;; Only works in Emacs. Returns nil in XEmacs
			(facep sym))))
	 (buffer  (ti::temp-buffer tinylisp-:buffer-tmp 'clear)))
    (tinylisp-face-print buffer symbols)
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
	 (char-to-string tinylisp-:register))))))

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
  (if (char= ?\( (following-char))
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
  (let* ((opoint   (point))
	 (word     (save-excursion (tinylisp-read-word)))
	 point
	 func
	 statement)
    (save-excursion
      (cond
       ((and (stringp word) (intern-soft word))
	(skip-chars-backward "^ \t"))
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
 `tinylisp-:buffer-eval'"
  (tinylisp-with-current-buffer
   (ti::temp-buffer tinylisp-:buffer-eval 'clear)
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
  (let* ((win           (get-buffer-window buffer))
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
;;;     (interactive) (tinylisp-b-display tinylisp-:buffer-eval pmin))
;;;
;;; This is just byteComp forward declaration, kinda.

(defun tinylisp-b-record (&rest args)
  "Ignore ARGS."
  nil)

;; Real functions are defined here.

(mapcar
 (function
  (lambda (x)
    (let ((sym (intern (format "tinylisp-b-%s" x)))
	  (var (intern (format "tinylisp-:buffer-%s" x)))
	  def)
      (setq def
	    `(defun ,sym (&optional pmin)
		 (interactive "P")
		 (tinylisp-b-display ,var pmin)))
      (eval def))))
 '("eval" "record" "variables" "funcs" "autoload" ))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-b-record-empty (&optional verb)
  "Empty buffer `tinylisp-:buffer-record'. VERB."
  (interactive)
  (ti::verb)
  (if (buffer-live-p (get-buffer tinylisp-:buffer-record))
      (ti::erase-buffer tinylisp-:buffer-record))
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
  (let* (list
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
  (let* ((list (tinylisp-ad-match-1 regexp)))
    (ti::verb)
    (tinylisp-with-current-buffer
     (ti::temp-buffer tinylisp-:buffer-data 'clear)
     (dolist (elt list)
       (insert
	(format
	 "%-35s %-7s %s\n"
	 (symbol-name (nth 0 elt))
	 (symbol-name (nth 1 elt))
	 (symbol-name (nth 2 elt))))))
    (when verb
      (pop-to-buffer tinylisp-:buffer-data)
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
  (let* (str)
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
	 (ti::temp-buffer tinylisp-:buffer-elp 'clear)
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
  (let* (pfx)
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

  (let* ((str   (if remove "un" ""))
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
  (let* ((func (ti::buffer-defun-function-name)))
    (if (not func)
	(message "TinyLisp: Can't find function name.")
      ;;  This evaluates the function prior elp'ing it.
      (tinylisp-eval-at-point)
      (tinylisp-symbol-do-macro func nil
				(elp-restore-function func) ;do this first
				(elp-instrument-function func))
      (message (format "TinyLisp: ELP instrumented [%s]" func)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-mapsym (regexp &optional not-regexp real-name)
  "Return list of function matching REGEXP NOT-REGEXP REAL-NAME.
See `tinylisp-elp-instrument-by-regexp'."
  (let* (list
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
     nil  'tinylisp-:elp-regexp-history)
    (read-string "Not Regexp: " nil  'tinylisp-:elp-not-regexp-history)
    (y-or-n-p "Match against real names? (look under alias name) ")
    current-prefix-arg))
  (ti::verb)

  (if (ti::nil-p not-regexp)         ;It's "" after RET in interactive
      (setq not-regexp nil))

  (let* ((list (tinylisp-elp-mapsym regexp not-regexp real-name))
	 (msg  (if uninstrument "un" "")))
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
      'tinylisp-:elp-master-history))))
  (elp-set-master function))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-elp-restore-buffer ()
  "Read functions from the buffer and cancel elp for them."
  (interactive)
  (let* ((args (tinylisp-elp-instrument-buffer-i-args 'pfx 'iact)))
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
  (let* ((func (ti::buffer-defun-function-name)))
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
Default is call count is 3,but you can supply numeric prefix COUNT. VERB.

   ** You must have instrumented the functions before you call this function

This is bit exotic function and it requires that you have written
following test setup in the clear Lisp buffer. Let's say we're
interested if 'let*' is slower that 'let'.

	    (defun t-1 () (let* () ))
	    (defun t-2 () (let  () ))
	    (defun t-3 () )

	    [* point/cursor is before this statement]
	    ;; The trick here is that when you instrument whole
	    ;; buffer and eval all the functions with '$ -' ,
	    ;; the when forms are bypassed
	    ;;
	    ;; When you have Evaled/instrumented buffer, then change
	    ;; it to 'when t' and call the harness function.
	    ;;
	    ;; The variable tinylisp-:harness-flag is set to t when you can this
	    ;; function and set to nil when this function finishes.
	    ;;
	    (when tinylisp-:harness-flag
	      (ti::dotimes count 1 500  ; run 500 times
		(t-1)
		(t-2)
		(t-3)))

This function evals everything from current point forward ARG times.
If there is word tinylisp-:harness-flag in the buffer, the current point is not
used but the eval is started from the beginning of that line forward.

After each eval round it records the elp result to `tinylisp-:buffer-record'.
In the above setup, this means that we repeat the test setup 3 times
to get 3 elp timing results. Since using elp only once for small functions,
doesn't give reliable results; we have to repeat the test at least 3 times.

The `tinylisp-:buffer-record' buffer is displayed after the harness run is over."
  (interactive "P")
  (let* (case-fold-search
	 beg
	 h-found
	 rounds)
    (ti::verb)
    (setq count  (or count 3)
	  rounds count)
    ;;  See if there this word in the buffer
    (save-excursion
      (ti::pmin)
      (when (re-search-forward "tinylisp-:harness-flag" nil t)
	(setq beg (line-beginning-position)  h-found t)))
    (or beg                             ;we already found it
	(setq beg (point)))             ;nope, use current point
    (if (null elp-all-instrumented-list)
	(error "No functions in elp list"))
    (if (and verb
	     (null
	      (y-or-n-p
	       (format
		(if h-found
		    "tinylisp-:harness-flag %s times, ok? "
		  "Harness %s times, from current point forward, ok? ")
		count))))
	(error "Abort."))
    (if (and verb
	     (y-or-n-p "Do you want to clear RECORD buffer first? "))
	(tinylisp-b-record-empty))
    (unwind-protect ;; make sure tinylisp-:harness-flag is set to nil
	(progn
	  (setq tinylisp-:harness-flag t)
	  (ti::dotimes iterator 0 count
	    (tinylisp-elp-reset-list)   ;wipe timings
	    (if verb (message "TinyLisp:  Eval round %d/%d ... "
			      (1+ iterator) rounds))
	    (eval-region beg (point-max))
	    (tinylisp-elp-results
	     'record (format " -- %d/%d\n" (1+ iterator) rounds)))
	  (if verb
	      (message "TinyLisp: Eval rounds done."))
	  (tinylisp-b-record 'pmin))
      (setq tinylisp-:harness-flag nil))))

;;}}}
;;{{{ elp results

;;; ----------------------------------------------------------------------
;;;
(mapcar
 (function
  (lambda (x)
    (let ((sym (intern (format "tinylisp-elp-summary-sort-column-%d" x)))
	  def)
      (setq def
	    `(defun ,sym (&optional arg)
;;;              "Sort by field. ARG to reverse sort."
		 (interactive "P")
		 (tinylisp-elp-summary-sort-column ,x arg)))
      (eval def))))
 '(1 2 3 4 5 6 7 8 9))

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
  (let* ((p         -1)
	 (opoint    (point))
	 last-p)
    (ti::pmin)
    (setq last-p (point))
    (while (not (eq p (point)))
      (setq p (point))
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
    (setq tinylisp-:find-error nil)
    (save-excursion
      (let (half
	    (low 1)
	    (high (tinylisp-error-count-sexps)))
	(if tinylisp-:find-error       ;See tinylisp-error-count-sexps
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

    (if (not tinylisp-:find-error)
	(message "TinyLisp: No errors found.")
      (goto-char lower-bound)
      (message "TinyLisp: %s" tinylisp-:find-error))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-error-try-parse (from to)
  "Eval regions and try to find error in FROM TO."
  (condition-case err
      (progn (eval-region from to) t)
    (error
     (progn
       (setq tinylisp-:find-error err)
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
    (error (setq tinylisp-:find-error err))))

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

WORD        String. Symbol to search.

VERB        Flag. Allows displaying verbose messages.

NODISPLAY   Flag. If non-nil, don't display the found point.

References:

  `tinylisp-:call-chain'"

  (interactive (list current-prefix-arg
		     (tinylisp-read-word)))

  (let* ((f-re
	  (concat "^(\\(defun\\*?\\|defmacro\\*?\\|defsubst\\|deffoo"
		  "\\|defun-maybe\\|defsubst-maybe"
		  "\\|define-derived-mode"
		  "\\|defalias\\|fset"
		  ;;  See grep.el::define-compilation-mode
		  "\\|define-[^ \t\r\n]+-mode"
		  "\\)[ \t']+%s[ \t\r\n]"))
	 (v-re
	  "^(\\(defvar\\|defconst\\|defcustom\\|defvoo\\)[ \t]+%s[ \t\r\n]")
	 (reg  tinylisp-:register)
	 (call-chain-data  (point-marker))
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
	  (setq buffer (find-file-noselect file))
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
	      (sit-for 2))
	    (when save
	      (if (and save (not (equal save '(4))))
		  (setq tinylisp-:call-chain nil)
		(tinylisp-push-call-chain nil call-chain-data verb)
		(if verb
		    (message
		     "TinyLisp: Call chain %d"
		     (length tinylisp-:call-chain)))))))
	(when (null file)
	  ;;  No load-history so try searching all buffers in Emacs
	  (setq buffer nil)
	  (dolist (buf (buffer-list))
	    (save-excursion
	      (set-buffer buf)
	      (when (re-search-forward re nil t)
		(setq buffer (current-buffer))
		(setq point  (line-beginning-position))
		(return))))))
       ;; ....................................................... other ...
       (point ;; point is set
	(when save
	  (if (and save (not (equal save '(4))))
	      (setq tinylisp-:call-chain nil)
	    (tinylisp-push-call-chain nil call-chain-data verb)
	    (if verb
		(message "TinyLisp: Call chain %d"
			 (length tinylisp-:call-chain)))))
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
  "Jump back to last call chain point in `tinylisp-:call-chain'."
  (interactive)
  (tinylisp-push-call-chain 'pop)
  (message "TinyLisp:  Call chain %d" (length tinylisp-:call-chain)))

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
  (interactive)
  (let* ((opoint    (point))
	 type
	 sym
	 point
	 beg
	 end)
    (ti::verb)
    (while (and (null point)
		(prog1 (setq beg (if back
				     (tinylisp-forward-def 'back)
				   (tinylisp-forward-def)))
		  (unless beg
		    (message "TinyLisp: No more user options.")
		    ;;  If you have 'paren' package on and your cursor is
		    ;;  at  (defun
		    ;;      *
		    ;;
		    ;;  then the paren will show "Matches (((...."
		    ;; and you wouldn't ever see this message without sit-for
		    ;;
		    ;; Same goes for eldoc.el
		    (sit-for 1))))
      (cond
       ((looking-at tinylisp-:regexp-variable)
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
  (let* ((debug-on-error t)
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
  (let* ((byte-compile-generate-call-tree  nil)
	 (file  (buffer-file-name)))
    (if (null file)
	(message "TinyLisp: Buffer %s is not visiting file." (buffer-name))
      (call-interactively 'byte-compile-file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-byte-compile-display-call-tree ()
  "See bytecomp.el `display-call-tree'."
  (interactive)
  (let* ((byte-compile-generate-call-tree  t)
	 (file  (buffer-file-name)))
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
  (let* ((buffer (tinylisp-byte-compile-display-call-tree)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-set-value-at-point (&optional arg)
  "Read word under point and if it's variable, ask new value for it.
ARG can be
 \\[universal-argument]  'restore variable's content
 \\[universal-argument]\\[universal-argument]  'backup variable's value"
  (interactive "P")
  (let* ((var  (tinylisp-read-word))
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
  (let* ((str       (tinylisp-read-word))
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
	  (pop-to-buffer (ti::temp-buffer tinylisp-:buffer-macro 'clear))
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
	   (get-buffer-create tinylisp-:buffer-macro)
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
  (let* ((debug-on-error t))            ;Make sure this is on!
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
  (let* ((obuffer   (current-buffer))
	 (name      (buffer-name))
	 (beg       (point-min))        ;maybe narrowed?
	 (end       (point-max)))
    (tinylisp-with-current-buffer
     (ti::temp-buffer tinylisp-:buffer-tmp 'clear)
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
See variable `tinylisp-:table-reverse-eval-alist'"
  (interactive)
  (let* ((stat  (tinylisp-read-symbol-at-point))
	 (table tinylisp-:table-reverse-eval-alist)
	 func
	 str1
	 str2
	 statement)
    (if (or (null stat)
	    (ti::nil-p (setq func (nth 1 stat))))
	(message "TinyLisp: Can't find command around point.")

      (tinylisp-symbol-do-macro func 'noerr
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
  (let* ((line (ti::string-remove-whitespace (ti::read-current-line)))
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
  (ti::occur-macro tinylisp-:regexp-function nil
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
  (let* ((re        tinylisp-:regexp-function)
	 (buffer    tinylisp-:buffer-data)
	 (loop      t)
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
	(tinylisp-with-current-buffer (ti::temp-buffer buffer 'clear)
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
  (ti::occur-macro tinylisp-:regexp-variable nil
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
  (let* ((re        tinylisp-:regexp-variable)
	 (buffer    tinylisp-:buffer-variables)
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
	  (run-hooks 'tinylisp-:find-var-list-hook))))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-narrow-to-function ()
  "Narrow to current function around point."
  (interactive)
  (let* ((re   tinylisp-:regexp-function)
	 beg
	 end)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at re))
	  (re-search-backward tinylisp-:regexp-function))
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

 buffer     `tinylisp-:buffer-data'"
  (interactive)
  (let* ((max       (length load-history ))
	 (buffer    (ti::temp-buffer tinylisp-:buffer-library 'clear))
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
				      "%-15s %3d %-35s %s %s\n"
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
				    (incf  i)
				    (setq dep-list  nil
					  pkg       nil)))
    (tinylisp-with-current-buffer buffer
				  (ti::pmin)
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
      ;;  go backward until space(word) or function call
      (unless (char= (following-char) ?\( )
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
  (let* ((compressions '("" ".gz" ".Z" ".z" ".bz2" ".zip"))
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
  (let* ((list (tinylisp-library-locate-library-1 file '(".el") ))
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
	    (setq buffer (ti::temp-buffer tinylisp-:buffer-library 'clear))
	    (with-current-buffer tinylisp-:buffer-library
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
  (let* ((path (locate-library file)))
    (if (not path)
	(message "TinyLisp: file %s not along `load-path'" file)
      (find-file path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-load-library (file)
  "Like `load-library' but offer completion of lisp files."
  (interactive (list (tinylisp-library-read-name)))
  (let* ((file (locate-library file)))
    (if (not file)
	(message "TinyLisp: file %s not along `load-path'" file)
      (load-library file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-load-history-grep (regexp)
  "Grep load history with REGEXP."
  (ti::list-find
   (mapcar 'car load-history)
   regexp
   (function
    (lambda (arg elt)
      (string-match arg (or elt ""))))
   'all-matches))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-library-load-by-regexp (regexp &optional no-elc verb)
  "Reload all packages (that are inside Emacs) matching REGEXP.
NO-ELC says to load non-compiled packages. VERB."
  (interactive
   (list
    (read-from-minibuffer "Reload packages matching regexp: ")
    (y-or-n-p "Load uncompiled versions ")))

  (let* ((count 0)
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
	  (incf  count))
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

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-generate-library (library)
  "Read all defuns and construct autoloads from LIBRARY on `load-path'."
  (interactive
   (list (tinylisp-library-read-name)))
  (let* ((path (if (file-name-absolute-p library)
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
     path (get-buffer-create tinylisp-:buffer-autoload))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-point ()
  "Return list of interactive functions from current point."
  (let (list
	point)
    (while (and (not (eq point (point)))
		(tinylisp-forward-user-option))
      (when (looking-at ".*defun[ \t]+\\([^ (\t\r\n]+\\)")
	(push (match-string 1) list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-buffer ()
  "Return list of interactive functions from buffer."
  (save-excursion
    (goto-char (poin-min))
    (tinylisp-interactive-function-list-from-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-interactive-function-list-from-file (file)
  "Return list of interactive functions from file."
  (with-temp-buffer
    (insert-file-contents file)
    (tinylisp-interactive-function-list-from-buffer)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-build-interactive-from-file (file &optional verb)
  "Collect all interactive defuns to from FILE to autoloads."
  (interactive
   (list
    (read-file-name "Autoloads from file:" (buffer-file-name))
    'verbose))
  (let ((buffer (current-buffer))
	(list   (tinylisp-interactive-function-list-from-file file)))
    (cond
     ((not list)
      (if verb
	  (message "No interactive defuns found in %s" file)))
     (t
      (let (str)
	(with-temp-buffer
	(ti::package-autoload-create-on-file
	 file
	 (current-buffer);;; tinylibmail.el --- Library of mail related functions

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2009 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylibmail-version.
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
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;;; Install:

;; ........................................................ &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file
;;
;;     (require 'tinylibm)
;;
;; No, there is no mistake here. The 'm' lib contains all autoloads
;; to this package.

;;}}}
;;{{{ Documentation

;;; Commentary:

;;
;;      o   This is library. Package itself does nothing.
;;      o   Collection of functions to deal with Mail/News specific tasks.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: -- require

;;;  ....................................................... &v-require ...

(require 'tinylibm)
(require 'sendmail) ;; mail-header-separator

(eval-and-compile
  (defvar mail-abbrevs)                 ;Silence ByteCompiler
  (defvar mail-aliases)
  (defvar rmail-current-message nil)
  (cond
   ((ti::xemacs-p)
    (autoload 'build-mail-aliases "mail-abbrevs"))
   (t
    (autoload 'mail-abbrevs-setup "mailabbrev")
    (autoload 'build-mail-aliases "mailalias")
    (autoload 'build-mail-abbrevs "mailabbrev")))
  (autoload 'rmail-msgbeg                       "rmail")
  (autoload 'rmail-msgend                       "rmail")
  (autoload 'gnus-group-get-parameter           "gnus"))

(eval-when-compile
  (ti::package-use-dynamic-compilation))

;;}}}
;;{{{ setup: -- private

;;; ......................................................... &v-hooks ...

(defvar ti:mail-load-hook nil
  "Hook that is run when package is loaded.")

;;; ....................................................... &v-private ...

(defvar ti:mail-ret nil
  "Global return value of this package.")

(defvar ti:mail-mail-buffer " *ti::mail-mail*"
  "*Temporary mail buffer name.")

;;  Variables could be modified. defsubst makes them persistent

(defsubst ti::mail-pgp-signature-begin-line ()
  "Signature start line."
  "-----BEGIN PGP SIGNATURE-----")

(defsubst ti::mail-pgp-signature-end-line ()
  "Signature end line."
  "-----END PGP SIGNATURE-----")

;; Signed message has:
;;
;; -----BEGIN PGP SIGNED MESSAGE-----
;; -----BEGIN PGP SIGNATURE-----
;; -----END PGP SIGNATURE-----

(defsubst ti::mail-pgp-signed-begin-line ()
  "Text for start of PGP signed messages."
  "-----BEGIN PGP SIGNED MESSAGE-----")

(defsubst ti::mail-pgp-signed-end-line ()
  "Text for start of PGP signed messages."
  (ti::mail-pgp-signature-end-line))

(defsubst ti::mail-pgp-pkey-begin-line ()
  "PGP public key begin line."
  "-----BEGIN PGP PUBLIC KEY BLOCK-----")

(defsubst ti::mail-pgp-pkey-end-line ()
  "PGP public key end line."
  "-----END PGP PUBLIC KEY BLOCK-----")

(defsubst ti::mail-pgp-msg-begin-line ()
  "PGP message, typically base64 signed, begin line."
  "-----BEGIN PGP MESSAGE-----")

(defsubst ti::mail-pgp-msg-end-line ()
  "PGP message, typically base64 signed, end line."
  "-----END PGP MESSAGE-----")

(defsubst ti::mail-pgp-any-pgp-line-regexp (&optional anchor-left)
  "Return regexp that match any pgp TAG.
If ANCHOR-LEFT is non-nil; the regexp will contains left ^ anchor."
  ;;  The lines can be broken, when there is encrypted/signed message
  ;;  NOTE: there is no anchor by default; because sometimes use may have
  ;;  indented the whole PGP block (e.g. in his web page or in .doc file)

  (concat
   (if anchor-left "^" "")
   "- -----\\(BEGIN\\|END\\) PGP.*-----"
   "\\|"
   (if anchor-left "^" "")
   "-----\\(BEGIN\\|END\\) PGP.*-----"))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-raw-p (ip)
  "Check raw nnn.nnn.nnn.nnn IP."
  (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$" ip))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-top-level-domain (host)
  "Convert HOST a.b.c  => b.c domain.
If HOST is raw numeric IP, do nothing."
  (cond
   ((ti::mail-ip-raw-p host)
    host)
   ((or (string-match "\\.\\([^.]+\\.[^.]+\\)$" host)
	(string-match "^\\([^.]+\\.[^.]+\\)$" host))
    (match-string 1 host))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-3-level-domain (host)
  "Convert HOST a.b.c.d  => b.c.d domain."
  (when (string-match "\\.\\([^.]+\\.[^.]+\\.[^.]+\\)$" host)
    (match-string 1 host)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-cleanup (word)
  "Clean WORD to format 'aa.bb.cc'. Remove offending characters.
Remove all characters up till @: this@email.com => email.com
Remove all not(alphanumeric, dash, dot) charcters.

For example a word at point may include anything:

  <bb.com> \"bb.com\" this@bb.com

All of the above will become:

  bb.com"
  (and word
       (replace-regexp-in-string
	"[^a-z0-9-.]" ""
	(replace-regexp-in-string "^.*@" "" word))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-ip-at-point-1 ()
  "Read only word containing characters [-.a-zA-z0-9]."
  (let (beg
	word)
    ;;  depending where the point is, from this word different part
    ;;  is read: foo.com[1.2.3.4]
    ;;             |       |
    ;;            (1)     (2)
    (save-excursion
      (skip-chars-backward "-.a-zA-Z0-9")
      (setq beg (point))
      (skip-chars-forward "-.a-zA-Z0-9")
      (unless (eq beg (point))
	(buffer-substring beg (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-ip-at-point ()
  "Read domain ip IP name at point."
  (let* ((word (ti::mail-ip-at-point-1)))
    (when (not (ti::nil-p word))
      ;;  foo.com[1.2.3.4]
      (setq word (ti::mail-ip-cleanup word))
      (if (ti::mail-ip-raw-p word)
	  word
	(ti::mail-ip-top-level-domain word)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-news-group ()        ;ding & gnus compatible
  "Return newsgroup name if group exists."
  (if (boundp 'gnus-newsgroup-name)
      (symbol-value 'gnus-newsgroup-name)))

;;; ........................................................ &v-public ...
;;; User configurable -- but not recommended.

;;  See gnus.el or gnus-msg.el  gnus-required-headers
;;  The 'in-reply-to is for mail messages (additional)

(defconst ti:mail-required-headers
  '(from date newsgroups subject path message-id in-reply-to references)
  "*All required fields that RFC977 and RFC1036 requires.
Make sure symbol names are all in lowercase.")

(defvar ti:mail-parse-name-not-accept
  (concat
   "[A-Z][/][A-Z]"                      ;company division ABC/TMS
   "\\|^[A-Z]+\\'"                      ;all in capitals
   "\\|^[-]$"                           ;single '-' word
   "\\|[.0-9]"                          ;maybe phone number
   "\\|com\\|org\\|edu"
   "\\|Dr")                             ;Titles
  "*Regexp to exclude non-valid people names.
We can't be sure that the names are really good names when we parse the
senders From field. Let's see an example

	\"Person Someone p. nnn-nnn-nnn\"

There obviously isn't 3rd name, it's used for phone abbrev. And the last
word is the actual phone number.

This regexp tells which word matches are false name hits.
In this example it'd leave:
	\"Person Someone\"

See `ti::mail-parse-name'")

;;}}}
;;{{{ setup: -- version

(defconst tinylibmail-version (substring  "$Revision: 2.68 $" 11 16)
  "Latest version number.")

(defconst tinylibmail-version-id
  "$Id: tinylibmail.el,v 2.68 2007/05/07 10:50:08 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylibmail-version (&optional arg)
  "Show version information. ARG tell to print message in echo area only."
  (interactive "P")
  (ti::package-version-info "tinylibmail.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylibmail-submit-feedback ()
  "Submit suggestions, error corrections, impressions, anything..."
  (interactive)
  (ti::package-submit-feedback "tinylibmail.el"))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-signature-p  ()
  "Return beginning of line point if \\n-- *\\n is found.
The de facto, while not standardized by any RFC signature separator
it \\n-- \\n. The trailing whitespace is very unfortunate evolution
to separate signatures from message digests \\n--\\n.

This function accepts trailing spaces or just n\\--\\n"
  (let* ((point (point)))               ;avoid save-excursion.
    (ti::pmin)
    (prog1 (if (re-search-forward "\n-- *\n" nil t)
	       (1+ (match-beginning 0)))
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-body-empty-p ()
  "Check if there is nothing in the body or if whole buffer is empty."
  (save-excursion
    (ti::mail-text-start 'move)
    (eq (point) (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-body-clear ()
  "Delete message body."
  (ti::mail-text-start 'move)
  (delete-region (point) (point-max)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-set-region 'lisp-indent-function 1)
(put 'ti::mail-set-region 'edebug-form-spec '(body))
(defmacro  ti::mail-set-region (beg end)
  "Set BEG END to whole buffer if they don't have value."
  `(progn
     (or ,beg
	 (setq ,beg (ti::mail-text-start)))
     (or ,end
	 (setq ,end (point-max)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-point-in-header-macro 'lisp-indent-function 0)
(put 'ti::mail-point-in-header-macro 'edebug-form-spec '(body))
(defmacro ti::mail-point-in-header-macro (&rest body)
  "Run BODY only if point is inside mail header area."
  `(when (< (point) (ti::mail-text-start))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-message-length ()
  "Return message's body length, not including the headers.
The message length is measured be counting character between the
BODY begin and BODY end. Any amount of whitespaces around the body is
skipped.

Exceptions:

  The start point defaults to `point-min' if body can't be found.

  If there is PGP signed block, then the body length is the text inside
  PGP signed block, not the original message body.

  Signed headers are also skipped.

    -----BEGIN PGP SIGNED MESSAGE-----

    ##                                  < signed headers begin mark \\n##
    Subject: some subject
    Reply-to: somewhere
					< empty space ends headers
    Hi, I swanted to tell you...        < BODY BEGIN IS HERE"
  (let* ((end (ti::mail-pgp-signed-conventional-p))
	 beg)
    (save-excursion
      (cond
       ((and end
	     (null (ti::mail-pgp-signature-detached-p)))
	;; Do not count empty lines at the end of body
	(goto-char end) (skip-chars-backward " \t\n") (setq end (point))
	(ti::pmin)
	(re-search-forward (ti::mail-pgp-signed-begin-line))
	(forward-line 1)
	;;  now we're inside BODY of text, but it's not that simple yet. User
	;;  may have signed headers and they are inserterted into body
	;;  like this:
	;;
	;;  -----BEGIN PGP SIGNED MESSAGE-----
	;;
	;;  ##
	;;  Subject: See this ma!
	;;
	;;  Body text starts here.
	;;
	;;  Note, there is no spaces, becasue the body is trimmed
	(when (looking-at "\n##\n")
	  (goto-char (match-end 0))
	  (re-search-forward "^$"))
	;;  Ignore leading spaces
	(skip-chars-forward " \t\n")
	(- end (point)))
       (t
	(ti::mail-text-start 'move)
	(skip-chars-forward " \t\n") (setq beg (point))
	(goto-char (point-max))
	(if (eq beg (point))            ;Empty buffer
	    0
	  (skip-chars-backward " \t\n") (setq end (point))
	  (- (point) beg)))))))

;;; ----------------------------------------------------------------------
;;;FIXME: this is old function and should be removed.
;;;
(defun ti::mail-get-2re (re str)
  "Use RE and return submatches 1 and 2 from STR.
Return list of empty strings if no matches."
  (let ((m1 "")
	(m2 ""))
    (if (eq nil (string-match re str))
	t                               ;do nothing, not matched
      (if (match-end 1)
	  (setq m1 (substring str (match-beginning 1)
			      (match-end 1))))
      (if (match-end 2)
	  (setq m2 (substring str (match-beginning 2)
			      (match-end 2)))))
    (list m1 m2)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-required-headers ()
  "Return standard RFC header required when posting.

References:

  `ti:mail-required-headers'
  `gnus-required-headers'

Return:

  list          '(header-name-symbol .. )
  nil           gnus not loaded ?"
  (cond
   ((listp ti:mail-required-headers)
    ti:mail-required-headers)
   ((boundp 'gnus-required-headers)
    (symbol-value 'gnus-required-headers))
   (t
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-mode-p ()
  "Check if some mail MUA mode is tuned on this buffer: RMAIL, VM, MH ..."
  (string-match
   "^\\(vm-\\|rmail-\\|mh-\\|gnus-article-\\|message\\).*mode"
   (symbol-name major-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mailbox-p ()
  "Check if two first lines look like Berkeley mailbox format."
  (save-excursion
    (ti::pmin)
    ;; From foo@some.com  Wed Dec 19 19:19:41 2001
    ;; Received: from some.com ([000.22.68.000])
    (and (looking-at "^\\(From:?\\|Return-Path:\\) .*@")
	 (forward-line 1)
	 (not (eobp))
	 (looking-at "^[a-zA-Z-]+:[ \t]+[^ \r\r\n]"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-p ()
  "Check if first line contain left flushed header 'Header:'.
This is a sign that current buffer is in mail-like.
You should also check the mode name to get more reliable results."
  (or (memq major-mode '(message-mode mail-mode))
      (save-excursion
	(ti::pmin)
	(looking-at "^[-A-Za-z0-9][-A-Za-z0-9]+:"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-header-area-size ()
  "Count size of header area.
This function can only be called from a buffer that has
`mail-header-separator'. Function count the characters in the header  area.
You can use this information to determine if the headers have been
changed after the last check.

Return:

 nbr
 nil        can't find `mail-header-separator'"
  (save-excursion
    (ti::pmin)
    (when (re-search-forward (regexp-quote mail-header-separator) nil t)
      (- (point) (point-min)))))

;;; ----------------------------------------------------------------------
;;; - This is suitable for RMAIL, GNUS and for individual buffers
;;;   holding mail or news messages.
;;;
(defun ti::mail-hmax (&optional move noerr)
  "Search max point of header, optionally MOVE and NOERR.
Order is: `mail-header-separator' or find all \"Headers:\" and then
return next line after them. The header must start at `point-min'.

If no point can be found, return `point-min'."
  (let ((point (point-min)))
    (when (ti::mail-mail-p)
      (save-excursion
	(ti::pmin)
	;;  VM's "t" key shows all headers, including the
	;;  "From xxxx"foo.com" line which is not actual header, because
	;;  it has no colon. Skip ovber it if we see it.
	(if (looking-at "From")
	    (forward-line 1))
	;;  GNUS 4 sets the mail-header-separator to "" ??
	(if (and (not (ti::nil-p mail-header-separator))
		 (re-search-forward (regexp-quote mail-header-separator) nil t))
	    (setq point (match-beginning 0))
	  ;;  Header:
	  ;;    Continuing line here
	  ;;  Header2:
	  (while (and (looking-at "^[0-9a-zA-z-]+:")
		      (progn
			(end-of-line)
			(not (eobp))))
	    ;;  If this function doesn't move anuy more, then the headers
	    ;;  have ended.
	    (if (null (ti::mail-next-field-start 'move))
		(forward-line 1))
	    (setq point (point))))))
    (if (and move point)
	(goto-char point))
    point))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-text-start (&optional move)
  "Return starting point or text in BODY. Optionally MOVE to it.
If buffer is not mail-like, then return `point-min'.

References:
  `mail-header-separator'"
  (let ((re         (regexp-quote mail-header-separator))
	(point      (point-min)))
    (when (ti::mail-mail-p)
      (cond
       ((save-excursion                 ;Do we find the separator?
	  (ti::pmin)
	  (when (re-search-forward re nil t)
	    (forward-line 1)
	    (setq point (point)))))
       ((setq point (ti::mail-hmax))
	(save-excursion
	  (goto-char point)
	  (forward-line 1)
	  (setq point (point))) )
       (t
	(error "Can't find position.")))
      (if (eq point (point-min))        ;Not found
	  (error "mail-header-separator not found or headers not found.")))
    (if (and move point)
	(goto-char point))
    point))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-point-at-header-p ()
  "Return non-nil if point is at HEADER area of mail."
  (< (point) (ti::mail-text-start)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-point-at-body-p ()
  "Return non-nil if point is at BODY of mail."
  (not (ti::mail-point-at-header-p)))

;;; ----------------------------------------------------------------------
;;; - Many std emacs dist. functions work so that you have to narrow
;;;   to headers before you can call the functions.
;;;
(defun ti::mail-narrow (&optional text)
  "Narrows to the headers only. Optionally to TEXT portion."
  (if text
      (narrow-to-region (ti::mail-text-start 'move) (point-max))
    (narrow-to-region (point-min) (ti::mail-hmax))))

;;; ----------------------------------------------------------------------
;;; - This is for both GNUS and RMAIL
;;;
(defun ti::mail-mail-buffer-name ()
  "Find original mail buffer whether in GNUS or in RMAIL.

Return:

   string       buffer name
   nil          if not exist."

  ;;  It's amazing that GNUS uses pointers and RMAIL uses string ...
  (let ((buffer (if (boundp 'mail-reply-buffer)
		    (symbol-value 'mail-reply-buffer))))
    (cond                          ;what is the mail YANK buffer name?
     ((stringp buffer)
      buffer)
     ((and (not (null buffer))
	   (bufferp buffer))
      (buffer-name buffer))
     (t
      nil))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-generate-buffer-name (&rest ignore)
  "Rename the *mail* buffer to \"*mail* SENDER\". IGNORE args.
You can install this function e.g. into
`my-message-generate-new-buffers' or `mail-setup-hook'"
  (interactive "Pbuffer name: ")
  (let ((to  (if (string= (buffer-name) " *gnus article copy*") ;; See gnus-msg.el
		 (mail-fetch-field "From")
	       (mail-fetch-field "To")))
	(str "*mail*"))
    (unless (ti::nil-p to)
      (cond
       ((setq str (ti::string-match "\\([^@<]+\\)," 1 to))
	(setq str (concat str ", ...")))
       ((setq str (ti::string-match "\\([^@<]+\\)" 1 to)))
       (t
	(setq str to)))

      (setq str (replace-regexp-in-string "['\"]" "" str)) ;remove extra cruft

      (setq str
	    (concat
	     (if (ti::mail-news-buffer-p)
		 "*post* "
	       "*mail* ")
	     str)))
    str))

;;; ----------------------------------------------------------------------
;;; - The idea is to find three fields and see what they contain,
;;;   and do they exist?
;;; - What's the use of this function? Well, when you post an article
;;;   or mail it, you can call this function from some of those
;;;   posting hooks to determine what to do with the buffer.
;;;
;;; - code lines disabled now so that it buffer can be checked any time

(defun ti::mail-mail-simple-p ()
  "Check if buffer contain headers belonging to simple \\[mail].
You can call this only once, just after the buffer is initially created"
  (require 'mail-utils)
  (let* ((sub (mail-fetch-field "Subject"))
	 ;;   mail-fetch-field doesn't return nil if field is empty.
	 (to  (mail-fetch-field "to"))
	 (news (mail-fetch-field "Newsgroups")))
    ;;  When you're replying to message in NEWS, RMAIL, the SUBJ and
    ;;  TO fields are already filled.
    ;;
    ;;  That's why you can only call this function once.
    ;;  When you use C-x m, and fill the fields, there is no way
    ;;  to detect afterwards if the mail buffer was simple mail or not

    (cond
     ((or (string-match "news" (symbol-name 'major-mode))
	  news)
      nil)
     ((and (ti::nil-p sub)
	   (ti::nil-p to))
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-to-list-p ()
  "Check if message is meant to be sent to a mailing list.
In GNUS you need to add Group parameter `to-list' containing address
to mailing list or otherwise Group is not considered mailing list."
  (when (featurep 'gnus)
    (let* ((group (ti::mail-news-group)))
      (when (stringp group)
	(gnus-group-get-parameter group 'to-list) ))))

;;}}}
;;{{{ macros: VM, RMAIL, GNUS

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-vm-macro 'lisp-indent-function 0)
(put 'ti::mail-vm-macro 'edebug-form-spec '(body))
(defmacro ti::mail-vm-macro (&rest body)
  "Do BODY in VM's active buffer.
The `save-excursion' -- set buffer form is executed."
  `(let* ((BuffeR-S  (when (boundp 'vm-mail-buffer)
		       (symbol-value 'vm-mail-buffer))))
     (if (or (null BuffeR-S)
	     (not (buffer-live-p (get-buffer BuffeR-S))))
	 (error "vm-mail-buffer invalid")
       (with-current-buffer BuffeR-S
	 ,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mh-macro 'lisp-indent-function 0)
(put 'ti::mail-mh-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mh-macro (&rest body)
  "Do BODY in MH's active buffer.
The `save-excursion' -- set buffer form is executed."
  `(let* ((BuffeR-S  (when (boundp 'mh-show-buffer)
		       (symbol-value 'mh-show-buffer))))
     (if (or (null BuffeR-S)
	     (not (buffer-live-p (get-buffer BuffeR-S))))
	 (error "mh-show-buffer invalid")
       (with-current-buffer BuffeR-S
	 ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-gnus-macro 'lisp-indent-function 0)
(put 'ti::mail-gnus-macro 'edebug-form-spec '(body))
(defmacro ti::mail-gnus-macro (&rest body)
  "Do BODY in Gnus `gnus-article-buffer' if it exists.
The `save-excursion' -- set buffer form is executed."
  `(let* ((BuffeR-S  (when (boundp 'gnus-article-buffer)
		       (symbol-value 'gnus-article-buffer))))
     (if (or (null BuffeR-S)
	     (not (buffer-live-p (get-buffer BuffeR-S))))
	 (error "gnus-article-buffer invalid")
       (with-current-buffer BuffeR-S
	 ,@body))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-rmail-macro 'lisp-indent-function 0)
(put 'ti::mail-rmail-macro 'edebug-form-spec '(body))
(defmacro ti::mail-rmail-macro (&rest body)
  "Do BODY in RMAIL's active buffer. You have be in RMAIL summary."
  `(let* ((BuffeR-R
	   ;;  This variable is available in Rmail-summary
	   ;;
	   (or (if (boundp 'rmail-buffer) (symbol-value 'rmail-buffer))
	       (get-buffer "RMAIL"))))
     (if (or (null BuffeR-R)
	     (not (buffer-live-p (get-buffer BuffeR-R))))
	 (error "rmail-buffer buffer invalid")
       (with-current-buffer BuffeR-R
	 ,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-rmail-do-message-macro 'lisp-indent-function 2)
(put 'ti::mail-rmail-do-message-macro 'edebug-form-spec '(body))
(defmacro ti::mail-rmail-do-message-macro (nbr mode &rest body)
  "Go to message without showing it and execute body.
Must be in RMAIL buffer already.

Input:
 NBR    message number, like `rmail-current-message'
 MODE   if non-nil then the area narrows to full stored message
	with original headers. If nil, then area narrows to displayed
	message.
 BODY   forms to execute in are narrowed to message."
  `(let ((beg (rmail-msgbeg ,nbr))
	 (end (rmail-msgend ,nbr)))
     (save-window-excursion
       (ti::widen-safe
	 (goto-char beg)
	 (forward-line 1)
	 (if (null ,mode)
	     (search-forward "\n*** EOOH ***\n" end t))
	 (narrow-to-region (point) end)
	 (goto-char (point-min))
	 ,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-rmail-copy-message (&optional nbr separate)
  "Copy message NBR with header. Defaults to `rmail-current-message'.
Current buffer must me in RMAIL already.

Input:

  NBR       message number
  SEPARATE  if non-nil, then the headers and message body are returned
	    separately in format (hdr-string . body-string)

Return:

 string
 list       see mode."
  (interactive)
  (let* (beg
	 end
	 hdr
	 ret)
    (setq nbr  (or nbr rmail-current-message)
	  beg  (rmail-msgbeg nbr)
	  end  (rmail-msgend nbr))

    (or (integerp nbr)
	(error "NBR %s" nbr))
    ;; The BEG isn't exactly the message beginning, skip 3 lines,
    ;; also don't copy the original heades only.
    ;;
    ;; ^L
    ;; 1, answered,,
    ;; Summary-line: 23-Mar #Please Help Yourself, Help Ot...
    ;; <ORIGINAL HEADERS>
    ;;
    ;; *** EOOH ***
    ;; <HEADERS SHOWN IN RMAIL>
    ;;
    ;; <MESSAGE BODY>
    (ti::widen-safe
      (goto-char beg) (forward-line 3)
      (setq beg (point))
      (re-search-forward "^[ \t]*$")
      (setq hdr (buffer-substring beg (point)))
      ;;  Already sitting at empty line, move away.
      (forward-line 1)
      (re-search-forward "^[ \t]*$")
      (setq beg (point))
      ;;  Now make HDR + BODY of message
      (if separate
	  (setq ret (cons hdr (buffer-substring beg end)))
	(setq ret (concat hdr (buffer-substring beg end)))))
    ret))

;;}}}

;;{{{ PGP general, tests

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-v3xx-p ()
  "Check if X-Pgp v3.xx header signing is in use.
It should have VALUE = KEYWORD; statement."
  (save-excursion
    (ti::pmin)
    (when (re-search-forward "X-Pgp-signed" nil t)
      (forward-line 1)
      ;;
      ;; KEYWORD=VALUE;
      (looking-at "^[ \t]+.*=.*;"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-p ()
  "Check if buffer contain PGP. It must have left flushed regexp:
\"^-----BEGIN.*PGP +\\(SIGNATURE\\|SIGNED\\\\|MESSAGE)\", otherwise this
string may be inside quoted text.

If there is X-pgp-sig.*: header, then it's also considered PGP message."
  (let ((max (ti::mail-hmax)))          ;headers ?
    ;;  if headers was found use that.

    (setq max (if (eq (point-min) max)
		  nil
		max))
    (save-excursion
      (ti::pmin)
      (or (let (case-fold-search)
	    (re-search-forward
	     "^-----BEGIN PGP \\(SIGNATURE\\|SIGNED\\|MESSAGE\\)"
	     nil t))
	  (progn
	    ;;  The New PGP in headers standard.
	    (re-search-forward "^X-Pgp-Sig.*:" max t))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-conventional-p ()
  "Return t if message is conventionally signed."
  (save-excursion (ti::pmin) (ti::mail-pgp-re-search 'sig)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-detached-p  ()
  "Return (beg . end) if there is detached signature."
  (let* ((point  (point))
	 beg
	 end)
    (prog1 (save-excursion
	     (ti::pmin)
	     (unless (ti::mail-pgp-re-search 'msg) ;Must not exist
	       (and (setq beg (ti::mail-pgp-re-search 'sig))
		    (setq end (ti::mail-pgp-re-search 'sige))
		    (cons beg end))))
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-conventional-multi-p ()
  "Return t if message is signed conventionally multiple times."
  (save-excursion
    (ti::pmin)
    (ti::mail-pgp-re-search 'sig 'move)
    (forward-line 1)
    (ti::mail-pgp-re-search 'sig 'move)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-xpgp-p ()
  "Return t if message is X-pgp signed.
There may be X-Pgp headers, but if the message is already
verified, that removes the signature around encrypted
message  \"- -----BEGIN PGP MESSAGE-----\"
       --> \"-----BEGIN PGP MESSAGE-----\"
In this case the message is no more in signed format,
but in encrypted format."
  (and (ti::mail-pgp-headers-p)
       ;;  See documentation above
       (save-excursion
	 (ti::pmin)
	 (null (re-search-forward
		(concat "^" (ti::mail-pgp-msg-begin-line))
		nil t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-p ()
  "Return t is message is conventionally or X-pgp signed."
  (or (ti::mail-pgp-signed-xpgp-p)
      (ti::mail-pgp-signed-conventional-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-public-key-p (&optional point)
  "Find public key delimiter from current point forward or using POINT."
  (save-excursion
    (goto-char (or point (point)))
    (re-search-forward (ti::mail-pgp-pkey-begin-line) nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-remail-p ()
  "Check if This is remailer message."
  (save-excursion
    (ti::pmin)
    (re-search-forward "[:#][:#]+\nReply-To" nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-comment-file-p (&optional point)
  "You can send binary files with base64 signing.
This function checks if comment block has have words 'File: FILE '.

Example:

 -----BEGIN PGP MESSAGE-----
 Version: 2.6.3ia
 Comment: Base64 signed. File: tm.tar uncompresses to approx. 20K

Input:

  POINT     search start point

Return:

  nil
  file"
  (save-excursion
    (if point (goto-char point))
    (when (re-search-forward "^Comment:.*File:? +\\([^ \t,]+\\)" nil t)
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-encrypted-p (&optional check-pgp-dash-line)
  "Check if there is encrypted PGP message.
It must have left flushed tag. The start point of match is returned.
The following tag will tell if if the message is encrypted.

  ::
  Encrypted: PGP

Input:

  CHECK-PGP-DASH-LINE   if the tag is not found, message _could_ be signed
			if there is -----BEGIN PGP MESSAGE----- tag.
			When this flag is non-nil, it also checks this
			case. Beware: message could be base64 signed too,
			so the encrypted-p test may not be exactly right."
  (save-excursion
    (ti::pmin)
    (if (re-search-forward "::[ \t]*\nEncrypted:[ \t]*PGP" nil t)
	(match-beginning 0)
      (when check-pgp-dash-line
	(ti::pmin)
	(car-safe (ti::mail-pgp-block-area 'msg))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-normal-p (&optional point)
  "Check if there is any PGP in current buffer from POINT forward.
The beginning point of PGP is returned."
  ;; Must find at least two lines, maybe BEG and END
  (let ((re   (ti::mail-pgp-any-pgp-line-regexp 'acnhor))
	ret)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward re nil t)
	(setq ret (match-beginning 0))
	(if (null (re-search-forward re nil t))
	    (setq ret nil)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-headers-p ()
  "Return t if PGP information is in headers.
Searches string 'X-Pgp-Signed:' and return end of match or nil."
  (let ((psig           "^X-Pgp-Signed:")
	(hmax           (ti::mail-hmax)))
    (save-excursion
      (ti::pmin)
      (re-search-forward psig hmax t))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-re (str)
  "Add possible beginning anchor if STR doesn't have one."
  (if (not (char= (aref str 0) ?^))
      (concat "^" str)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-block-area-kill-forward (mode &optional move)
  "Search PGP block forward and kill it. If no block found, do nothing.

Input:
  MODE      choices are explained in `ti::mail-pgp-block-area'.
  MOVE      if non-nil, move to killed region begin point."
  (let* (reg)
    (when (setq reg (ti::mail-pgp-block-area mode))
      (delete-region (car reg) (cdr reg))
      (when move (goto-char (car reg))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-block-area (mode &optional inside max nstrict)
  "Return (beg . end) of PGP block from current point forward.

Input:
  MODE      nil   search signed start line..
	    'sig  search signature start instead.
	    'signed  search signed message area
	    'pkey search public key block start instead.
	    'msg  search for pgp base64 signed \"message\"
	    'any  go to `point-min' and search beginning of any
		  PGP line, then go to the end of buffer and search
		  backward any PGP line.  The lines must not be at
		  same position.  This gives you the whole PGP
		  region.

  INSIDE    if non-nil, beg and end are 'inside' without the PGP tags.
  MAX       max point to search
  NSTRICT   If non-nil; then the pgp bounds must not be left flushed,
	    but can contains \"- -\".
Return:
  (beg . end)
  nil"
  (let ((re1
	 (cond
	  ((null mode)      (ti::mail-pgp-signed-begin-line))
	  ((eq 'sig  mode)  (ti::mail-pgp-signature-begin-line))
	  ((eq 'pkey mode)  (ti::mail-pgp-pkey-begin-line))
	  ((eq 'msg  mode)  (ti::mail-pgp-msg-begin-line))
	  ((eq 'any  mode)  (ti::mail-pgp-any-pgp-line-regexp (not nstrict)))
	  ((eq 'signed  mode)  (ti::mail-pgp-signed-begin-line))
	  (t
	   (error "unknown mode"))))
	(re2
	 (cond
	  ((null mode)      (ti::mail-pgp-signed-end-line))
	  ((eq 'sig  mode)  (ti::mail-pgp-signature-end-line))
	  ((eq 'pkey mode)  (ti::mail-pgp-pkey-end-line))
	  ((eq 'msg  mode)  (ti::mail-pgp-msg-end-line))
	  ((eq 'signed  mode)  (ti::mail-pgp-signed-end-line))))
	ret
	beg
	end)
    (save-excursion
      (cond
       ((eq mode 'any)

	(when (re-search-forward re1 max t)
	  (setq beg (match-beginning 0))
	  (when (re-search-forward re1 max t)
	    (beginning-of-line)
	    (when (not (eq beg (point)))
	      (forward-line 1)
	      (setq ret (cons beg (point)))))))
       (t
	(if nstrict
	    (setq re1    (concat "^-? ?" re1)
		  re2    (concat "^-? ?" re2))
	  (setq re1    (concat "^" re1)
		re2    (concat "^" re2)))
	(when (re-search-forward re1 max t)
	  (if inside
	      (forward-line 1)
	    (beginning-of-line))
	  (setq beg (point))

	  (when (re-search-forward re2 max t)
	    (if inside
		(beginning-of-line)
	      (forward-line 1))

	    (setq end (point))
	    (setq ret (cons beg end)))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-re-search (&optional mode move end no-anchor)
  "Re-search-forward to find -----BEGIN.*SIGNED.

Input:
  MODE          nil     search signed start line.
		'sig    search signature start.
		'sige   search signature block end.
		'pkey   search public key block start.
		'pkeye  search public key block end.
		'msg    search for pgp base64 signed \"message\"
			This also finds conventionally crypted tag.
		'kid    search for  'Key for user ID: '
		'kpub   search for 'pub   512/47141D35 1996/06/03 ...'
			note: match level 1 matches 0x code 47141D35
  MOVE          flag    non-nil moves point to found point
  END           flag    use `match-end' instead of math-beginning.
  NO-ANCHOR     flag    non-nil disables using '^' anchor.

Return:
  point         ,beginning of line
  nil           ,if not found"
  (let ((re   (cond
	       ((null     mode)  (ti::mail-pgp-signed-begin-line))
	       ((eq 'sig  mode)  (ti::mail-pgp-signature-begin-line))
	       ((eq 'sige mode)  (ti::mail-pgp-signature-end-line))
	       ((eq 'pkey mode)  (ti::mail-pgp-pkey-begin-line))
	       ((eq 'pkeye mode) (ti::mail-pgp-pkey-end-line))
	       ((eq 'msg  mode)  (ti::mail-pgp-msg-begin-line))
	       ((eq 'kid  mode)  "Key for user ID: ")
	       ((eq 'kpub mode)
		"pub[ \t]+[0-9]+/\\([A-Z0-9]\\)+[ \t]+.*/.*/[0-9]")
	       (t
		(error "unknown mode"))))
	point)
    (when (and (null no-anchor)
	       (not (memq mode '(kid))))
      ;;  suppose encrypted and signed message
      ;;  - -----END PGP MESSAGE-----
      ;;
      (setq re (concat "^-? ?" re)))
    (save-excursion
      (if (or (looking-at re)
	      (re-search-forward re nil t))
	  (if end
	      (setq point (match-end 0))
	    (setq point (match-beginning 0)))))
    (if (and move point)
	(goto-char point))
    point))

;;}}}
;;{{{ PGP misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-exe-version-string (&optional exe-file-location)
  "Call pgp/gpg executable to find out its version number.
EXE-FILE-LOCATION defaults to \"pgp\" but can also be absolute path."
  (with-temp-buffer
    (call-process (or exe-file-location "pgp")
		  nil
		  (current-buffer)
		  nil
		  ;;  - With PGP will say "illegal option", but will print
		  ;;    the logo screen.
		  ;;  - With GPG will print logo screen.
		  "--help")
    (ti::pmin)
    (when (or (re-search-forward
	       "Pretty Good Privacy(tm) +\\([^\r\n ]+\\)" nil t)
	      (re-search-forward
	       "gpg (GnuPG) +\\([^\r\n ]+\\)" nil t))
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-type ()
  "Examine pgp data packet type by searching _forward_.
Return:
  'base64 'pgp 'conventional or nil"
  (let ((re  (ti::mail-pgp-any-pgp-line-regexp 'anchor))
	char)
    (save-excursion
      (when (and (re-search-forward re nil t)
		 (re-search-forward "^$" nil t))
	;; FIXME: Check first character. Actually we should check bit mask...
	;;
	;;  -----BEGIN PGP MESSAGE-----
	;;  Version: 2.6.2
	;;  Comment: Encrypted by xxx
	;;
	;;  hEwDYCggxO/bFq0
	(forward-line 1)
	(setq char (following-char))
	(cond
	 ((char= char ?p) 'conventional)
	 ((char= char ?h) 'pgp)
	 ((char= char ?o) 'base64))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-trim-buffer ()
  "Trim buffer: pgp blocks are left flushed and junk around them is removed."
  (let ((stat  t)
	region)
    (save-excursion
      (ti::pmin)
      (while (and stat
		  (setq region (ti::mail-pgp-block-area 'any)))

	(when (setq stat (ti::mail-pgp-chop-region (car region) (cdr region)))
	  (goto-char (cdr stat)))))))

;;; ----------------------------------------------------------------------
;;; - This is needed after finger or http call to clean up all unnecessary
;;;   tags around the PGP key.
;;;
(defun ti::mail-pgp-chop-region (beg end)
  "Delete junk around BEG END from pgp public key block.
Area BEG END that correspond to pgp begin and end
lines (call `ti::mail-pgp-block-area' with argument 'any),
then we chop the public key region so that only the pgp area
is left without additional garbage.

Return
 (beg .end)         the canonilized area of PGP block

Example:

<PRE>
<SAMP>      -----BEGIN PGP PUBLIC KEY BLOCK-----</SAMP>
<SAMP>      Version: 2.6.3ia</SAMP>
</PRE>
<PRE>
<SAMP>      mQBNAzGzQ2MAAAECAM4p2THKCpNjYXDLpsg4sLHyEiNxJwQuEYfipdTj</SAMP>
<SAMP>      p5CPHN+0LkphcmkgQWFsdG8sIEZpbmxhbmQgPGphcmkuYWFsdG9AbnRj</SAMP>
<SAMP>      LmNvbT6JAFUDBRAxs0O+wLrt1UcUHTUBAbMhAf9Qgh6EznEcY2OUOIPg</SAMP>
<SAMP>      =46gx</SAMP>
<SAMP>      -----END PGP PUBLIC KEY BLOCK-----</SAMP>

This is converted into

-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: 2.6.3ia</SAMP>

mQBNAzGzQ2MAAAECAM4p2THKCpNjYXDLpsg4sLHyEiNxJwQuEYfipdTj
p5CPHN+0LkphcmkgQWFsdG8sIEZpbmxhbmQgPGphcmkuYWFsdG9AbnRj
LmNvbT6JAFUDBRAxs0O+wLrt1UcUHTUBAbMhAf9Qgh6EznEcY2OUOIPg
=46gx
-----END PGP PUBLIC KEY BLOCK-----"
  (save-excursion
    (goto-char beg) (beginning-of-line)
    (ti::narrow-safe (point) (progn
			       (goto-char end)
			       (end-of-line)
			       (point))
      (ti::buffer-fill-region-spaces (point-min) (point-max))
      (ti::pmin)
      (re-search-forward "-----END")
      (goto-char (match-beginning 0))
      (if (> (current-column) 0)     ;Nothing to do, it's left flushed
	  (delete-rectangle (point-min) (point)))
      (ti::pmin)
      (ti::buffer-replace-regexp "<.*$" 0 "")
      ;;  Because the last line does not have newline, the
      ;;  previous regexp doesn't match. Fix the last line too.
      (goto-char (point-max))
      (beginning-of-line)
      (let (case-fold-search)           ;be sensitive
	;;  -----END PGP PUBLIC KEY BLOCK-----
	(if (and (looking-at ".*[A-Z]-----\\(.*\\)")
		 (match-end 1))
	    (ti::replace-match 1)))
      (setq beg (point-min)
	    end (point-max))))
  (cons beg end))

;;}}}
;;{{{ PGP signed headers

;;; ...................................................... &pgp-header ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-header-kill-in-body ()
  "Kill headers that are inserted into the body of message.
If there is no headers, this function does nothing.

--text follows this line--  [or empty line after headers]
##<no spaces>
Header1: content
Header2: content
<empty line>
BODY"
  (let* (beg)
    (save-excursion
      (ti::mail-text-start 'move)
      (setq beg (point))
      (when (and (looking-at "^##\n")
		 (re-search-forward "^$" nil t))
	(delete-region beg (point))))))

;;}}}
;;{{{ PGP ASCII armor

;;; ....................................................... &pgp-armor ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-char-to-int (char)
  "Process PGP ascii armor data.
Input is ASCII armor CHAR (as one string). Function return respective int."
  (let* ((table (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			"abcdefghijklmnopqrstuvwxyz0123456789+/"))
	 case-fold-search
	 str)
    (if (null (setq str (ti::string-match
			 (concat ".*" (regexp-quote char)) 0 table)))
	(error "Armor char is invalid %s " char)
      (1- (length str)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-string-to-bin-string (string)
  "Process PGP ascii armor data.
Convert quoted printable ASCII armor STRING into binary string."
  (let* ((len (length string))
	 (i    0)
	 (ret  "")
	 ch
	 int
	 bin)
    (while (< i len)
      (setq ch (substring string i (1+ i)))
      (setq int (inline (ti::mail-pgp-data-char-to-int ch)))
      (setq bin (inline (int-to-bin-string int 6)))
      (setq ret (concat ret bin))
      (incf i))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-bin-string-to-int-list(string)
  "Process PGP ascii armor data.
Convert 8bit binary byte string \"000001...\" into list of ints."
  (let* ((len (/ (length string) 8))
	 (i   0)
	 ret
	 bin
	 int)
    (while (< i len)
      (setq bin (substring string (* i 8) (+ 8 (* i 8))))
      (setq int (inline (bin-string-to-int bin)))
      (incf i)
      (push int ret))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-pgp-data-ascii-armor-convert (string)
  "Convert PGP ascii armor STRING(quoted printable) into list of ints."
  (ti::mail-pgp-data-bin-string-to-int-list
   (ti::mail-pgp-data-string-to-bin-string string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-study-ctb-byte (int)
  "From single INT, examine the PGP CTB structure.
Return
 nil    ,input was not CTB byte
 '(ctb-type length-field)
	ctb-type  is
	'enc      (pgp encrypted message)
	'signed   (signed message)
	'secring  (secret keyring)
	'pring    (public keyring)
	'base64   (base 64 signed)
	'crypt    (conventionally crypted)
	'raw      (raw literal plaintext)
	'trust    (keyring trust packet)
	'uid      (user id packet)
	'comment  (comment packet)
	'unknown  (none of the above...)

	length is
	nil       no length, unknown length
	1 2 4     byte packet length"
  (let* ((length-mask   3)     ;; 00000011b
	 (type-mask     60)    ;; 00111100b
	 (ctb-mask      128)   ;; 10000000b
	 (table
	  '((1  . enc)
	    (2  . signed)
	    (5  . secring)
	    (6  . pring)
	    (8  . comp)
	    (9  . crypt)
	    (11 . raw)
	    (12 . trust)
	    (13 . uid)
	    (14 . comment)))
	 (type 'unknown)
	 val
	 ret)
    (when (logand int ctb-mask)

      ;; shift to the right 2 bits

      (when (setq val (assq (lsh (logand int type-mask) -2) table))
	(setq type (cdr val)))

      (setq val (logand int length-mask))
      (cond
       ((eq 0 val)  (setq val 1))
       ((eq 1 val)  (setq val 2))
       ((eq 1 val)  (setq val 4))
       ((eq 3 val)  (setq val nil)))
      (setq ret (cons type val)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-pgp-stream-study-1-ver (int)
  "Return pgp version string from stream INT."
  (cond
   ((eq 2 int) "2.5")
   ((eq 3 int) "2.6")
   (t          (error "Invalid Data format."))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-pgp-stream-study-1-key-id 'lisp-indent-function 1)
(put 'ti::mail-pgp-stream-study-1-key-id 'edebug-form-spec '(body))
(defmacro ti::mail-pgp-stream-study-1-key-id (stream result)
  "Read MSB and LSB key-id from STREAM to RESULT.
STREAM will be advanced during read."
  `(let* ((i 0))
     (while (< i 4)
       (setq ,result (concat (or ,result  "")
				(format "%02x" (car ,stream)))
	     ,stream (cdr ,stream)
	     i          (1+ i)))
     (setq ,result (upcase ,result))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-1-time (stream)
  "Read TIME from STREAM to RESULT."
  (let* (val1
	 val2)
    ;;  There must be easier way to do, but right now it goes like this
    ;;  '(51 158 95 145)
    ;;  --> hex 339E 5F91
    ;;  --> int 13214  24464  which is in (current-time) format
    ;;

    (setq val1 (hexl-hex-string-to-integer
		(concat
		 (int-to-hex-string (car stream))
		 (int-to-hex-string (car (cdr stream)))))

	  stream (cdr (cdr stream))
	  val2  (hexl-hex-string-to-integer
		 (concat
		  (int-to-hex-string (car stream))
		  (int-to-hex-string (car (cdr stream))))))
    (ti::date-standard-date nil (list val1 val2))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-enc (length stream)
  "Study the 'enc packet, which has known LENGTH.
STREAM is list of ints, minimum 11 integers, 13 is the full 'enc packet.

Return:
 '(version-string
   key-msb-hex-string
   key-lsb-hex-string

   rsa-algorithm                 ;; nil if stream is not long enough
   rsa-int (encrypted integer))   ;; nil if stream is not long enough."
  (let* ((msb "")
	 (lsb "")
	 ver
	 val
	 rsa-alg
	 rsa-int)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION KEY-MSB KEY-LSB
    ;;  1byte 1-4bytes   1byte   4bytes  4bytes
    (setq stream (nthcdr (1+ length) stream))
    (setq val    (car stream)   stream (cdr stream))
    (setq ver (ti::mail-pgp-stream-study-1-ver val))
    (ti::mail-pgp-stream-study-1-key-id stream msb)
    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (setq rsa-alg (car stream)
	  rsa-int (cadr stream))
    (list ver msb lsb rsa-alg rsa-int)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-signed (length stream)
  "Study the 'sign packet, which has known LENGTH. STREAM is list of ints.

Return:

 '(version-string
   md-length
   sig-class
   timestamp

   key-msb-hex-string
   key-lsb-hex-string

   alg-rsa                  ;; nil if stream is not long enough
   alg-md5                  ;; nil if ...
   digest                   '(int int);; nil if ...

   rsa-algorithm                 ;; nil if stream is not long enough
   rsa-int (encrypted integer)   ;; nil if stream is not long enough)"
  (let* ((msb "")
	 (lsb "")
	 ver
	 md-length
	 sig-class
	 timestamp
	 alg-rsa
	 alg-md5
	 digest)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION KEY-MSB KEY-LSB
    ;;  1byte 1-4bytes   1byte   4bytes  4bytes

    (setq stream (nthcdr (1+ length) stream))
    (setq ver       (ti::mail-pgp-stream-study-1-ver (car stream))
	  stream    (cdr stream)
	  md-length (car stream)
	  stream    (cdr stream)
	  sig-class (car stream)
	  stream    (cdr stream))
    (setq timestamp
	  (ti::mail-pgp-stream-study-1-time stream))
    (setq stream (nthcdr 4 stream))
    (ti::mail-pgp-stream-study-1-key-id stream msb)
    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (setq alg-rsa (car stream)
	  stream  (cdr stream)
	  alg-md5 (car stream)
	  stream  (cdr stream)
	  digest  (list (car stream) (car (cdr stream))))
    (list ver md-length sig-class timestamp msb lsb
	  alg-rsa alg-md5 digest)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-pring (length stream)
  "Study the 'pring packet, which has known LENGTH. STREAM is list of ints.

Return:

 '(version-string
   timestamp
   key-msb-hex-string
   key-lsb-hex-string)"
  (let* ((msb "")
	 (lsb "")
	 ver
	 timestamp
	 validity)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION TIME    VALIDITY RSA-ALG
    ;;  1byte 1-4bytes   1byte   4bytes  2bytes   1byte
    (setq stream (nthcdr (1+ length) stream))
    (setq ver       (ti::mail-pgp-stream-study-1-ver (car stream))
	  stream    (cdr stream))
    (setq timestamp
	  (ti::mail-pgp-stream-study-1-time stream))
    (setq stream    (nthcdr 4 stream)
	  validity  (car stream)
	  stream    (cdr stream))
    ;;  PGP format spec is not clear enough here!
    ;;  Don't know where the User ID is...
;;;    (ti::mail-pgp-stream-study-1-key-id stream msb)
;;;    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (list ver timestamp validity msb lsb)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study (ctb stream)
  "Study PGP data.

Input:

 CTB            in format `ti::mail-pgp-data-study-ctb-byte'
 STREAM         dearmored int stream (list of ints including ctb-byte)

Return:

  LIST          depends on the ctb, see conversion functions."
  (let* ((type  (car ctb))
	 (len   (cdr ctb)))

;;;    (ti::d! type)
    (cond
     ((eq type 'enc)
      (ti::mail-pgp-stream-study-enc len stream))
     ((eq type 'signed)
      (ti::mail-pgp-stream-study-signed len stream))
     ((eq type 'base64)
      ;; #todo
      nil)
     ((eq type 'crypt)
      ;; #todo
      nil)
     ((eq type 'pring)
      (ti::mail-pgp-stream-study-pring len stream)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-xpgp ()
  "If there is X-Pgp-Signed field, goto signature stream."
  (let* ((point (ti::mail-pgp-headers-p)))
    (when point
      (goto-char point)
      ;;  must exist, this call dies if not found
      (re-search-forward "Signature[ \t]*=[ \t\n\"]+"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward (&optional any)
  "Find PGP data stream block start forward. PGP block must be left flushed.

Input:

  ANY       if non-nil, then find any stream (not necessarily left flushed)

Return:

  point     cursor is placed in front of stream
  nil       If there is no PGP stream block, do nothing."
  (let* ((beg   (ti::mail-pgp-msg-begin-line))
	 (sig   (ti::mail-pgp-signature-begin-line))
	 (pkey  (ti::mail-pgp-pkey-begin-line))
	 (re    (concat (if any "" "^") "-----BEGIN"))
	 (point (point))
	 (loop  t)
	 col
	 ret)
    ;; base64
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; owEBbACT/4kAVQMFATOb

    ;; normal signature
    ;; -----BEGIN PGP SIGNATURE-----
    ;; Version: 2.6.3ia
    ;; Charset: noconv
    ;;
    ;; iQBVAwUBM55fkcC67dVHFB01AQGnHwIAqe2OfkdcnQviGzCmy3KddnsE8uFkAeaV

    ;; conventional crypt
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; pgAAACN9WXlrJFURU5Xgi+YyN

    ;; encrypted
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; hEwDwLrt1UcUHTUBAf9
    ;;

    ;; Extracted public key
    ;; -----BEGIN PGP PUBLIC KEY BLOCK-----
    ;; Version: 2.6.3ia
    ;;
    ;; mQBNAzOW770AAAECANDkXBfEbJk0gW41o52nLiktpThcBY+BMQCY5zyGCyUIbrDp
    (while (and loop (re-search-forward re nil t))
      (goto-char (match-beginning 0))

      (when (or (looking-at beg)
		(looking-at sig)
		(looking-at pkey))
	(setq col (current-column))
	(when (re-search-forward "^[ \t]*$" nil t)
	  (setq loop nil)
	  (forward-line 1)
	  (move-to-column col)
	  (setq ret (point))))
      (if loop
	  (end-of-line)))               ;wrong match, Continue search
    (unless ret
      ;;  none found, return to original position.
      (goto-char point))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-and-study (&optional search any)
  "Find PGP data stream forward and study it.

If normal search fails, then find X-Pgp-Signed field's first
data stream.

Input:

  SEARCH    if non-nil, then search PGP starting from `point-min' if
	    forward lookup fails.
  ANY       if non-nil, find also non-left flushed stream.

Return:

  '(CTB . (INFO-LIST))  the CTB type and data for CTB
  nil                   no stream found forward."
  (interactive)
  (let* ((point (point))
	 ctb
	 line
	 list
	 data
	 ret)
    (when (or (ti::mail-pgp-stream-forward any)
	      (and search
		   (ti::pmin)
		   (ti::mail-pgp-stream-forward any))
	      (ti::mail-pgp-stream-forward-xpgp))
      ;;  Will match all base64 characters (approx.)
      (setq line (ti::buffer-match "[^ \t\n\"\']+" 0)
	    list (ti::mail-pgp-data-ascii-armor-convert line)
	    ctb  (ti::mail-pgp-data-study-ctb-byte (car list))
	    data (ti::mail-pgp-stream-study ctb list))
      (setq ret (cons (car ctb) data)))
    (unless ret (goto-char point))     ;Nothing found, return to point
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-info (&optional search any)
  "Find PGP data stream and read some information. Return string.

Input:

  SEARCH    if non-nil, then search PGP starting from `point-min' if
	    forward lookup fails.
  ANY       if non-nil, find also non-left flushed stream."
  (let* (ver
	 time key-id
	 data
	 type
	 ret)
    (when (setq data (ti::mail-pgp-stream-forward-and-study search any))
      (setq type (car data))
      (cond
       ((eq type 'signed)
	(setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
	      time      (ti::mail-pgp-stream-data-elt data 'time)
	      key-id    (ti::mail-pgp-stream-data-elt data 'key-id))
	(setq ret (format "Signed by 0x%s %s [v%s.x]" key-id time ver)))
       ((eq type 'enc)
	(setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
	      key-id    (ti::mail-pgp-stream-data-elt data 'key-id))
	(setq ret (format "Encrypted to 0x%s [v%s.x]" key-id ver)))
       ((eq type 'pring)
	(setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
	      time      (ti::mail-pgp-stream-data-elt data 'time))
	(setq ret (format "Public key %s [v%s.x]" time ver)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-data-elt (data elt)
  "Study DATA and Return packet ELT.
DATA must be in the format of `ti::mail-pgp-stream-forward-and-study'
ELT  can be 'ver 'time 'key-id"
  (let* ((type (car data))
	 (pos  (assq
		elt
		(nth
		 1
		 (assq type
		       '(
			 (signed ((ver 0) (time 3) (key-id 5)))
			 (pring  ((ver 0) (time 1) (key-id 4)))
			 (enc    ((ver 0) (key-id 2))) ;No TIME field
			 ;; #todo, not ready
			 (base64 ((ver 0) (time 3) (key-id 5)))
			 (crypt  ((ver 0) (time 3) (key-id 5)))))))))
    (if (null pos)
	(error "Wrong specification %s %s %s" type elt data)
      (nth (nth 1 pos) (cdr data)))))

;;; Test suite with live data: first ASCII armor bytes
;;
;; (setq list (ti::mail-pgp-data-ascii-armor-convert "hEwDwLrt1UcUHTUBA"))
;; (setq ctb  (ti::mail-pgp-data-study-ctb-byte (car list)))
;; (setq data (ti::mail-pgp-stream-study ctb list))

;; Sig data
;; (setq s "iQBVAwUBM55fkcC67dVHFB01AQGnHwIAqe2OfkdcnQviGzCmy3KddnsE8uFkAeaV")
;;
;; (setq list (ti::mail-pgp-data-ascii-armor-convert s))
;; (setq ctb  (ti::mail-pgp-data-study-ctb-byte (car list)))
;; (setq data (ti::mail-pgp-stream-study ctb list))

;;}}}
;;{{{ PGP key info

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-id-lines-in-region (beg end)
  "Search all lines in BEG END matching pgp -kvc and -kx lines.

Option -kvc

  pub  1024/01234567 1997/05/01 Mar Bar <Bar@bar.com>

Option -kx

  Key for user ID: Mr. Bar <bar@bar.com>
  1024-bit key, key ID 01234567, created 1997/05/01

And return list of those lines."
  (let ((l1
	 (ti::buffer-grep-lines
	  "pub[ \t:]+[0-9]+/[A-Z0-9]+[ \t:]+.*/.*/[0-9]" beg end))
	(l2 (ti::buffer-grep-lines "[0-9]-bit key, Key ID " beg end)))
    (cond
     ((and l1 l2) (ti::list-merge-elements l1 l2))
     (l1 l1)
     (l2 l2))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-id-0x-lines-in-region (beg end)
  "Call `ti::mail-pgpk-id-lines-in-region' on BEG END, return only Key HEX ids."
  (let* (ret)
    (dolist (line (ti::mail-pgpk-id-lines-in-region beg end))
      (when (stringp line)
	(push
	 (or
	  (ti::string-match "pub[ \t]+[0-9]+/\\([^ \t]+\\)" 1 line)
	  (ti::string-match "Key ID \\([0-9A-F]+\\)" 1 line))
	 ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-public-get-region (&optional beg end buffer relax)
  "Get all keys in BEG END from BUFFER and build list of key data.
The blocks searched are in following format.

  Key for user ID: Mr. Foo <foo@example.com>
  512-bit key, key ID 123456789, created 1996/06/03
  Also known as: Mr Foo <bar@bar.com>

  -----BEGIN PGP PUBLIC KEY BLOCK-----
  [...]
  -----END PGP PUBLIC KEY BLOCK-----

Note1:

  If there _no_ 'Key for user ID:' string in the buffer, this function
  can't find the public key block while it may be there. It is
  assumed that each p-key block is _preceded_ by that string.

  All anonymous p-key block are skipped.

Note2:

  If there are two sequential key-id strings, like
  Key for user ID:       <<A
  Key for user ID:       <<B
  -----BEGIN PGP PUBLIC KEY BLOCK-----

  The p-key block in the list for A will be nil.

Note3:

   If RELAX argument is non-nil, then the 'Key for user ID:'
   must not exit. Only the Public key tags are searched.

   Recommended way of informing public keys is however displaying
   full public key information and not just PK block

Return:

  '((KEY-ID-STRING PUBLIC-KEY_BLOCK)"
  (let ((opt  (if relax 'pkey 'kid))
	id
	block
	region
	ret
	max)
    (with-current-buffer (or buffer (current-buffer))
      (ti::narrow-safe (or beg (point-min)) (or end (point-max))
	(ti::pmin)
	(while (ti::mail-pgp-re-search opt 'move)
	  (setq id (ti::read-current-line))

	  ;;  If there are two
	  ;;    Key for user ID:
	  ;;    Key for user ID:
	  ;;
	  ;;  And there is no public key between these two, set the
	  ;;  search limit to stop to next Key-id line.
	  (setq max
		(save-excursion
		  (end-of-line)
		  (setq max (ti::mail-pgp-re-search 'kid))))
;;;       (ti::d! ">>" id ">>" max (ti::mail-pgp-block-area 'pkey nil max))
	  (cond
	   ((setq region (ti::mail-pgp-block-area 'pkey nil max))
	    (setq block (buffer-substring (car region) (cdr region)))
	    (goto-char (cdr region)))
	   (t
	    ;; Continue search
	    (end-of-line)))
	  (push (list id block) ret)
	  (setq id nil   block nil))))
    (nreverse ret)))

;;}}}
;;{{{ PGP signature info

;;; ................................................... &pgp-signature ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-remove (&optional add no-cnv)
  "Remove PGP signature (and headers that have included in there).
Below, only lines 7 and 8 are left in buffer.

  1   -----BEGIN PGP SIGNED MESSAGE----
  2
  3   ##                            << Header start mark
  4   Header1: content
  5   Header2: Content
  6                                 << space here
  7   test
  8
  9   -----BEGIN PGP SIGNATURE-----

With ADD flag

  The tag lines are reassembled and point sits at the beginning of line 6
  and whitespaces around (email) buffer text are deleted.
  If tag lines are found while ADD, this function does nothing.

With NO-CNV

  When removing signature, do not convert '- -' back into '-'.
  Eg. If message is encrypted and signed; it is not desirable to
  do this conversion if you just want to strip out the signature to Xpgp.
  The '- -' lines must stay there."
  (let* ((beg (save-excursion (ti::pmin) (ti::mail-pgp-re-search))))
    (cond
     ((and add (null beg))
      (ti::mail-trim-buffer)
      (ti::mail-text-start 'move)
      (insert (ti::mail-pgp-signed-begin-line) "\n\n")
      (ti::pmax)
      (insert "\n" (ti::mail-pgp-signature-begin-line) "\n"))
     ((null add)
      ;;  there is one thing to fix first, PGP converts lines that have
      ;;  double '--' at front
      ;;
      ;;                --text follows
      ;;        -->
      ;;                - --text follows
      ;;
      ;;        Let's correct those lines too.
      (when (null no-cnv)
	(save-excursion (ti::buffer-replace-regexp "^- -" 0 "-")))
      (when beg                         ;Thre is regular PGP sig
	;;  note: We don't trim BODY here, we only remove
	;;  the pgp tag lines. The receiving end should do
	;;  the trimming. (we save one function call)
	(goto-char beg)                         ;One newline at beg
	(ti::buffer-kill-line 'del 2)           ;TWO lines; important
	;;  Kill included headers
	(when (and (looking-at "##\n.*: ")
		   (re-search-forward "^$" nil t))
	  (delete-region beg (1+ (point))))

	(when (and (prog1 (setq beg (ti::mail-pgp-re-search 'sig 'move))
		     (ti::buffer-kill-line))
		   (ti::mail-pgp-re-search 'sige 'move))
	  (forward-line 1)
	  (delete-region beg (point))))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-pgp-signature-normal-do-region 'lisp-indent-function 0)
(put 'ti::mail-pgp-signature-normal-do-region 'edebug-form-spec '(body))
(defmacro ti::mail-pgp-signature-normal-do-region (&rest body)
  "Execute BODY and calculate pgp signature region.
In the macro you can use following variables:

  `limits'      (area-beg . area-end)
  `area-beg'
  `area-and'

This macro does nothing if there is no normal PGP signature."
  `(let (limits
	 area-beg
	 area-end)
     (setq limits (ti::mail-pgp-block-area 'sig))
     (when limits
       ;;   Set values
       (setq area-beg (car limits)
	     area-end (cdr limits))
       ;;  If these are no used in BODY: no-op Quiet XE ByteCompiler
       (if (null area-beg)
	   (setq area-beg nil))
       (if (null area-end)
	   (setq area-end nil))
       ,@ body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-get-article-buffer ()
  "Do `set-buffer' to *Article* if it exists. Return nil if no buffer."
  (if (boundp 'gnus-article-buffer)
      (symbol-value 'gnus-article-buffer)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-with-article-buffer 'lisp-indent-function 0)
(put 'ti::mail-with-article-buffer 'edebug-form-spec '(body))
(defmacro ti::mail-with-article-buffer (&rest body)
  "Run BODY in *Article* buffer if it exists."
  `(let* ((buffer  (ti::mail-get-article-buffer)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
	 ,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-normal-info ()
  "Return signature information from normal PGP format.
Return:
 ((beg . end) (fld fld ..) (signarure-data sig ..))"
  (let (sig-list
	info-list
	ret)
    (ti::mail-pgp-signature-normal-do-region
     (save-excursion
       (goto-char area-beg)
       (forward-line 1)
       ;;  Here are the comments and other PGP headers
       (while (looking-at "^[^ \t]+:+ .*")
	 (ti::nconc info-list (ti::read-current-line))
	 (forward-line 1))
       ;; Here is the signature itself
       (while (not (>= (point) (cdr limits)))
	 (if (looking-at "^[^ \t\n]+$")
	     (ti::nconc sig-list (match-string 0)))
	 (forward-line 1))
       (setq ret (list limits info-list sig-list))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-sig-header-info-v2xx ()
  "Return signature information from X-pgp v2.xx headers.

Reads format:

X-Pgp-Comment: Processed by TinyPgp.el 1.56
X-Pgp-Version: 2.6.3ia
X-Pgp-Charset: noconv
X-Pgp-Signed:
	iQBVAwUBMoijBMC67dVHFB01AQGf3QH/dmgc47fx1tvHYPcuKWIz0Fe7HnWXmd63
	3IBA6vhSqzbUT4nkKL2QJQX/0Z8I9dkmOahSQNKvU/7qsB9Iw8JwpQ==
	=9yu9

Return:
 ((beg . end) (fld fld ..) (signature-data sig ..))"
  (let* ((case-fold-search t)
	 (pbase         "X-Pgp-")
	 (p-re          (concat "^" pbase)) ;pgp regexp for hdrs
	 (psig          (concat p-re "Signed:"))
	 (fld-re        (concat
			 p-re
			 "\\(Version:\\|Charset:\\|Comment:\\|Signed:\\)"))
	 (hmax          (ti::mail-hmax))
	 val
	 sig-list
	 info-list
	 beg
	 end
	 ret)
    (save-excursion
      (ti::pmin)
      (while (and
	      hmax
	      (< (point) hmax)    ;those fwl-line calls may go past...
	      (re-search-forward fld-re hmax t))
	(beginning-of-line)
	(if (null beg)                  ;record it NOW
	    (setq beg (point)))
	(cond
	 ((looking-at (concat psig "[ \t]*\\([^ \t\n]*\\)"))
	  ;;  Is this the signature itself ? Special handling,
	  ;;  because spreads multiple lines.
	  (setq val (ti::remove-properties (match-string 1)))
	  (if (not (string= "" val))
	      (ti::nconc sig-list val))
	  (forward-line 1)
	  (while (looking-at "^[ \t]+\\([^ \t\n]+\\)")
	    (ti::nconc sig-list (ti::remove-properties (match-string 1)))
	    (forward-line 1)))
	 ;; Nope, some additional PGP header
	 (t
	  (ti::nconc info-list (ti::remove-properties (ti::read-current-line)))))
	;;  Because there is already one while loop that says fwd-line,
	;;  we don't want to go furher if it stopped us.
	(if (looking-at (concat p-re  "\\|^\t+"))
	    (forward-line 1)))
      (beginning-of-line)
      (setq end (point)))

    (if sig-list
	(setq ret (list (cons beg end) info-list sig-list)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-header-info-v3xx ()
  "Return signature information from X-pgp v3.xx headers.

Return:
 '((nil . nil)
   (\"Version: x.x.x\" \"Charset: xxxx\" ...)
   (signature-string sig-string ..))"
  (let ((field  (ti::remove-properties (ti::mail-get-field "X-Pgp-signed")))
	info-list
	sig-list
	elt
	list)
    (when field
      (setq list (ti::mail-mime-parse-header field 'downcase))
      ;;  Push adds to the front of list, so beware order of elements
      (if (setq elt (assoc "signature" list))
	  (setq sig-list  (cdr elt)))
      (if (setq elt (assoc "comment" list))
	  (push (concat "Comment: " (car (cdr elt))) info-list))
      (if (setq elt (assoc "charset" list))
	  (push (concat "Charset: " (car (cdr elt))) info-list))
      (if (setq elt (assoc "version" list))
	  (push  (concat "Version: " (car (cdr elt))) info-list  ))
      (if info-list
	  (list (cons nil nil) info-list sig-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-header-info ()
  "Return X-pgp header info if X-Pgp header exist."
  (if (ti::mail-pgp-v3xx-p)
      (ti::mail-pgp-signature-header-info-v3xx)
    (ti::mail-pgp-sig-header-info-v2xx)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-parse-header (header-string &optional downcase)
  "Parse Variable=value HEADER-STRING like and optionally DOWNCASE keywords.

Header-this: var1=value2; var2= val2; var3=\"starts here \"
  \" continues here\"; var4= v1,v2,v3;

The VAL returned is different for continued string. It is a list of
individual parts in the parsed. In this case the whole returned value
would be:

'((var1 . (val1))
  (var2 . (val2))
  (var3 . (\"starts here \" \" continues here\"))
  (var4 . (\" v1,v2,v3\")))

Return:
  ((var . VAL) (var . VAL) ..)"
  (let ((tag-re  "^[ \t]*\\([^ \t\n]+\\)[ \t\"]*=")
	(val-re  "[ \t\"]*\\([^\n\"]+\\)")
	(buffer  (ti::temp-buffer "*tmp*" 'clear))
	name
	val
	ret)
    (with-current-buffer buffer
      (insert header-string)
      ;; put into same line
      (ti::pmin) (ti::buffer-replace-regexp "[ \t]*;[ \t]*" 0 "\n")
      ;; Okay now it's in canonical format. First
      ;; pick up signature, then delete it and parse other fields.
      ;;  Version=2.6.3ia
      ;;  Charset=noconv
      (ti::pmin)
      (while (re-search-forward tag-re nil t)
	(setq name (match-string 1)   val nil)
	(cond
	 ((looking-at val-re)           ;VALUE at the same line
	  (ti::nconc val (match-string 1))
	  (forward-line 1))
	 (t
	  ;;  Multiline
	  (while (progn (forward-line 1)
			(looking-at val-re))
	    (ti::nconc val (match-string 1)))))
	(if downcase
	    (setq name (downcase name)))
	(push (cons name val) ret)))
    (nreverse ret)))

;;}}}
;;{{{ PGP public key

;;; ........................................................ &pgp-pkey ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-pkey-read (&optional raw kill-file)
  "Read public key block from current point forward. Point is moved.

Input:

  RAW           If non-nil, return only raw public key block.
  KILL-FILE     if non-nil, kill temporary file after statement
		'Key extracted to file ...' Once the file is killed the
		message will be removed from buffer."
  (let* (beg
	 end
	 file
	 ret)
    ;;  No temp files are left on disk
    ;;  Remove also the file message from buffer before we read the
    ;;  content.
    ;;
    ;;       Extracting from key ring: '/users/xxx/.pgp/pubring.pgp',\
    ;;       userid "xxx".
    ;;
    ;;       Key for user ID: <xxx@some.fi>
    ;;       512-bit key, key ID 8125CAAA, created 1997/06/05
    ;;
    ;;       -----BEGIN PGP PUBLIC KEY BLOCK-----
    (when (and kill-file
	       (re-search-forward "Key extracted to file.*'\\(.*\\)'" nil t))
      (setq file (match-string 1))
      (ti::buffer-kill-line)
      (ti::file-delete-safe file))
    (goto-char (point))
    (when (re-search-forward (ti::mail-pgp-pkey-begin-line) nil t)
      (re-search-backward "Key for user ID:") (beginning-of-line)
      (when raw
	(re-search-forward "^-----BEGIN")
	(beginning-of-line))
      (setq beg (point))
      (when (ti::mail-pgp-re-search 'pkeye 'move)
	(forward-line 1)
	(setq end (point)))
      (when (and beg end)
	(setq ret (buffer-substring beg end))))
    ret))

;;}}}
;;{{{ PGP remail

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-close ()
  "Close reply block by adding '**' to the end.
If there already is '**', do nothing."
  (save-excursion
    (ti::pmax)
    ;;  Remailers need "**" at the end of encrypted block
    (if (not (re-search-backward "^\\*\\*" nil t))
	(insert "\n**\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-anonymize-headers (mode &optional no-ins arg1 arg2 hash)
  "Destroy header information according to mode and move it to message body.
This function does nothing if first line is not header.

Input:

  MODE       'move-to-body moves, all headers to body
	     'move-to-body-maybe, all headers to body only if
	     there is not already hash marks.
	     arg1 is used for subject       defaults to 'dummy'
	     arg2 is used for organisation  defaults to 'dummy'

  NO-INS     Do not insert the hash headers into body, but return them
	     as list instead.

  ARG1 ARG2  used by MODE

  HASH       Use hash marks string other that \"##\"

Return:

  list"
  (let ((hlist  '("In-reply-to"
		  "Organization"
		  "Subject"))
	(empty  " dummy")
	(full-string "")
	done
	ptr
	list
	str
	ret)
    (setq hash (or hash "##")
	  arg1 (or arg1 empty)
	  arg2 (or arg2 empty))
    (save-excursion
      (when (ti::mail-mail-p)
	(cond
	 ((memq mode '(move-to-body move-to-body-maybe))
	  ;;  First check if hash mark is already there
	  ;;  If mode is "maybe" we don't add new headers.
	  ;;
	  ;;  The regexp matches to the end of line, because you may have
	  ;;  quoted the message
	  ;;
	  ;;  jerry>  ##
	  ;;  jerry>  Subject:  this here
	  (ti::pmin)
	  (unless (and (eq mode 'move-to-body-maybe)
		       (re-search-forward (concat hash "[ \t]*$") nil t))
	    (setq ptr hlist)
	    (dolist (elt ptr)
	      (setq str (ti::mail-get-field elt))
	      (when (and str (not (string= empty str)))
		(setq elt (format "%s: %s\n" elt str))
		(push elt list)
		;;  so that we can match against this later
		;;
		(setq full-string (concat full-string elt))))
	    (ti::mail-text-start 'move)
	    (when list
	      (setq ret list  done t)
	      (unless no-ins
		;;  Remailer hash mark
		(insert hash "\n"))))
	  ;;  Anonymize some headers
	  (if arg1
	      (ti::mail-kill-field "^subject"  arg1))
	  (if arg2
	      (ti::mail-kill-field "^organization" arg2))
	  (when (and done (null no-ins))
	    (dolist (elt list)
	      ;;  Copy headers inside message
	      (insert elt))))
	 (t
	  (error "Invalid mode [%s]" mode)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-reply-type (property-string)
  "Return remailer reply block type from PROPERTY-STRING.
The 'post' type is not checked, because it relates to Usenet
and can be mixed with other types."
  (if (string-match "cpunk\\|eric\\|penet" property-string)
      (match-string 0 property-string)))

;;; ----------------------------------------------------------------------
;;; used to be: cpunk   Request-Remailing-To
;;; but nowadays instructions say "Anon-To"
;;;
(defun ti::mail-pgpr-block (mode &optional type email key latent)
  "Return remailer header string defined by mode.
be sure to have <> in the email, which defaults to `user-mail-address'.

Input:

  MODE      'epgp -- return encrypted pgp tag.
	    'post -- return simple Newsgroup post block. 'email'
	    contains the address of post remailer.
	    If there is not enough
	    parameters, say for 'tk, the previous one is used: 't

  TYPE      cpunk   Anon-To
	    eric    Anon-Send-To
	    penet   X-Anon-To
	    post    Anon-Post-To Usenet

  EMAIL     Parameter for type
  KEY       Parameter for type
  LATENT    Parameter for type"
  (let* ((reply
	  (cond
	   ((string= type "cpunk")  "Anon-To")
	   ((string= type "eric")   "Anon-To")
	   ((string= type "penet")  "X-Anon-To")
	   ((string= type "post")   "Anon-Post-To")
	   ((memq mode '(epgp post)))   ;Ok; skip
	   ((error "Unknown type '%s'" type)))))
    (setq email (or email user-mail-address))
    (cond
     ((equal mode 'epgp)
      "::\nEncrypted: PGP\n\n")
     ((equal mode 'post)
      (concat
       "::\n"
       "Anon-Post-To: " (or email (error "invalid args.")) "\n"
       "Cutmarks: --\n"))
     ((and (stringp email) (stringp key) (stringp latent))
      (format "::\n%s: %s\nEncrypt-Key: %s\nLatent-Time: %s\n"
	      reply email key latent))
     ((and (stringp email) (stringp latent))
      (format "::\n%s: %s\nLatent-Time: %s\n" reply email latent))
     ((and (stringp email) (stringp key))
      (format "::\n%s: %s\nEncrypt-Key: %s\n" reply email key))
     ((or (stringp email))
      (format "::\n%s: %s\n" reply email))
     (t
      (error "Wrong args '%s' '%s'" mode type )))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-reply-block (pgp-email)
  "Return reply block header.
Should be inserted just before PGP crypted message to PGP-EMAIL."
  (format "Reply-Block:\n::\nAnon-To: %s\n\n" pgp-email))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-parse-levien-list (&optional buffer control-list)
  "Parse remailer list finger <remailer-list@kiwi.cs.berkeley.edu>.
The parsing starts from current point forward.

Input:

  BUFFER            defaults to current buffer
  CONTROL-LIST      '(remailer-alias  (prop-no-list) [(prop-add-list)])
		    This control list says 'if REGEXP matches the
		    email address, remove all properties listed in
		    prop-no-list and add all properties listed in
		    prop-add-list.

		    So, if you're sure that the levien-list has some
		    faulty entries, e.g. say remailer@replay.com doesn't
		    have feature 'ek' although levien list contains that,
		    your control-list is like this. The ek property
		    is removed even if the list says otherwise.

		    '(\"replay\" '(\"ek\"))

Return:

  '((alias remailer property_string (property property ...))
    (alias remailer property_string (p p p ..)))

  The properties are sorted: cpunk mix pgp..."
  (let ((re  (concat
	      "^[ \t]*$remailer{[\"']\\(.*\\)[\"']}.*=[ \t]*[\"']"
	      "<\\(.*\\)>[ \t]+\\(.*\\)[\"']"))
	a
	r
	p
	blocks
	ret
	elem
	list)
    ;;  The list is in Perl hash array format in case you're interested...
    (with-current-buffer (or buffer (current-buffer))
      (while (re-search-forward re nil t)
	(setq a (match-string 1)
	      r (match-string 2)
	      p (match-string 3))
	(setq blocks (split-string p))
	(setq blocks (sort blocks 'string<))
	(when (and control-list
		   (setq elem (assoc a control-list)))
	  (setq list (nth 1 elem))
	  (dolist (elt list)
	    (setq blocks (delete elt blocks)))
	  (setq list (nth 2 elem))
	  (dolist (elt list) (push elt blocks))
	  ;;  We used this now, remove from list
	  (setq control-list (delete elem control-list))
	  (setq p            (mapconcat 'concat blocks " ")))
	;; features In alphabetic order
	(setq p (mapconcat 'concat blocks " "))
	(push (list a r p blocks) ret)))
    ret))

;;}}}

;;{{{ email addresses

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-make-anti-spam-address (email)
  "Make an anti-spam address from EMAIL."
  (let* ((add [ "uce"
		"ube"
		"spam"
		"commercials"
		"advertisements"
		"ads"
		"junk"
		"garbage"
		])
	 (base  ["no"
		 "stop"
		 "die"
		 "hang"
		 "anti.address"
		 "yuck"
		 "dislike"
		 "go-away"
		 "stay-away"
		 "delete"
		 "nothanks"
		 "erase"
		 "zap-this"
		 "wipe-this"
		 "exterminate"
		 "ignore"
		 "bypass"
		 "keep-out"
		 "keep-away"
		 "none"
		 "nada"
		 "zero"
		 "not-any"
		 "zelt"
		 "no-thank-you"
		 "remove-this"
		 "rip-off-this"
		 "disregard"
		 "throw-away"
		 ])
	 (vec  (vector
		(concat
		 (elt (shuffle-vector base ) 1)
		 "-"
		 (elt (shuffle-vector add) 1))
		(concat
		 (elt (shuffle-vector add) 1)
		 "-"
		 (elt (shuffle-vector base ) 1))))
	 (this (elt (shuffle-vector vec) 0))
	 login
	 domain)
    (string-match "\\(.*\\)@\\(.*\\)"  email)
    (setq login  (match-string 1 email)
	  domain (match-string 2 email))
    (format  "%s%s%s"
	     login
	     (if (zerop (randij 0 1))
		 (concat "." this "@")
	       (concat "@" this "."))
	     domain)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-domain (string)
  "Return only the top level domain name from email STRING.
xx.yy..domain.com --> domain.com
xx.domain.co.uk   --> domain.co.uk"
  (cond
   ;;  This match tries to catch those domains that don't have 3 parts,
   ;;
   ;;      aa.bb.co.uk
   ;;            |
   ;;            We expect this part to be longer than 2 characters

   ((string-match "[^.][^.][^.]+\\.\\(..\\|...\\)$" string)
    (match-string 0 string))
   ;;  This is domain that requires 3 parts: co.uk or au jp
   ((string-match "[^.]+\\.[^.]+\\.\\(..\\|...\\)$" string)
    (match-string 0 string))
   ((string-match "[^@]+$" string)
    (match-string 0 string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-domain-canonilize (list)
  "Canonilize list of addresses to top level domain names only
Eg: '(\"aa.foo.com\" \"bb.foo.com\") --> '(\"foo.com\")"
  (let* (ret
	 domain)
    (dolist (elt list)
      (setq domain (ti::mail-email-domain elt))
      (add-to-list 'ret domain))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-find-region (&optional beg end no-dupes)
  "Find all email addresses within region BEG END (defaults to buffer).
The email addresses must contain @. Surrounding <> characters are removed.

Input:

  BEG       region start; defaults to `point-min'
  END       region end; defaults to `point-min'
  NO-DUPES  flag; if non-nil then cache only unique entries."
  (let (list
	elt)
    (save-excursion
      (setq beg (or beg (point-min))
	    end (or end (point-max)))
      (ti::keep-lower-order beg end)
      (goto-char beg)
      ;;  Intangible text property case:
      ;;  - When you do a limited search and cursor land somewhere in
      ;;    intangible char, it immediately slides to next char
      ;;    position. Like if you'd do
      ;;
      ;;    (progn (goto-char 10) (point))
      ;;    --> 20
      ;;
      ;;    This is not suprise, if point 10 had intangible text until
      ;;    19th pos. If there were no intangible text in point 10,
      ;;    the result would be expected 10.
      (while (and (<= (point) end) ;; intangible test
		  (re-search-forward
		   "[^ '\",:<\t\n(]+@[^ '\">:,\t\n)]+"
		   end
		   t))
	(setq elt (ti::remove-properties (match-string 0)))

	(if (and (stringp elt)
		 (or (or (null no-dupes)
			 (not (member elt list)))))
	    (push elt list)))
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-from-string (string)
  "Return list of email addresses from STRING.
The addresses must have @ character. Surrounding <> characters are removed.
If STRING is nil this function does nothing."
  ;; Using buffer is faster that reading string
  (when string
    (with-temp-buffer
      (insert string)
      (ti::mail-email-find-region))))

;;}}}
;;{{{ parsing

;;; ......................................................... &parsing ...

;;; ----------------------------------------------------------------------
;;;   (ti::mail-test-parse-name)
;;;
(defun ti::mail-test-parse-name ()
  "This is a test function, do not call from programs.

Because the `ti::mail-parse-name' is quite complicated,
and slightest modification may render it, this functions tests
that the old functionality is preserved in spite of changes."
  (let (list
	e1
	e2
	stat
	ptr)
    (setq list
	  '("Dr. Foo Bar <foo@bar.com>"
	    "<jdoe@examole.com> (Finland, pgp id 512/47141D35)"
	    "(Rune Juntti[FRONTEC Pajala]) <jdoe@example.se>"
	    "shahramn@wv.mentorg.com (jdoe@example.com)"
	    "(jdoe@example.com)"
	    "Jerome Santini <doe@this-example.here.com>"
	    "jdoe@example.com (Harry Halladay - EDS St. Louis)"
	    "jdoe@example.com (Ake Stenhoff TM/PMD 83442 3003)"
	    "CEO-executive this here jdoe@example.com"
	    "JDOE <\"VAX::SOME@example.com\""
	    "\"VAX::LOGIN\"@example.com"
	    "john.doe@example.com"
	    "John=Doe%aoa.rdt%OS.DC@example.com"
	    "jdoe@example.com (John Doe)"
	    "\"/G=Name/S=Surname/OU=comlab/O=oxford/PRMD=UK.AC/ADMD= /C=GB/\"@example.fi\""
	    "\"wayne (w.d.) bell\" <jdoe@example>"
	    "John doe <example@example.com>"
	    "\"Joseph B. Ottinger\" <j.doe@example.com>"
	    "\"Name Foo puh. 111 600\" <LOGIN@example.com>"
	    "\"stephane (s.) boucher\" <jdoe@example.com>"
	    "jdoe@example.com (J.D \"John\" Doe)"
	    "jd@example-com (J.D doe)"
	    "doe@example.com \(John Doe\)"
	    "jdoe@example.com \(John D. Doe\)"
	    "\"J. doe Ph.d \" jdoe@john.doe.example.com"
	    "\"John D. Doe\" <foo@example.com>"))
    (setq ptr list)
    (dolist (n ptr)
      (setq stat  (ti::mail-parse-name n))
      (setq e1 (nth 0 stat)) (setq e2 (nth 1 stat))
      (read-from-minibuffer (concat "TEST>>" e1 "," e2 "<")))))

;;; ----------------------------------------------------------------------
;;; ( ti::mail-parse-name "\"Dr. Volker Zell\" <dr.volker.zell-QHcLZuEGTsvQT0dZR+AlfA@public.gmane.org>")
(defun ti::mail-parse-name (line)
  "Try to parse various formats of 'From:' fields.
Supposes that the 'From:' keyword is removed from the LINE.

Return:
  list          '(firstname surname)
  nil           if cannot parse both"
  (let* ((re-ignore "\\(?:Dr. +\\|Mr. +\\)?")

	 (re-A          "[-a-zA-Z0-9.{|]")
	 (re-AG         (concat "\\("  re-A "+\\)"))

	 ;;  'From: Mr-CEO John Doe <jdoe@example.com'
	 (fs-re2  (concat re-ignore re-AG " +" re-AG))

	 ;;  'USER <\"CLUSTER::VAX\@site.cm\"'
	 (fs-vax  (concat "^" re-AG "[ \t<\"]+[A-Z]+::" re-AG))

	 ;;  '\"CLUSTER::LOGIN\"@example.com'
	 ;;  This is incomplete Name, it does not contain NAMES at all, but
	 ;;  we consider mail name as surname. The first group-RE is dummy.
	 (fs-vax2 (concat re-AG "::" re-AG))

	 ;;  'Job.Ganzevoort@cwi.nl', where person's name is complete
	 ;;  address
	 (fs-fse    (concat re-AG "\\." re-AG "@" ))

	 ;;  matches gateway-type addresses
	 ;;  'Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU'
	 (gtw-re1    (concat re-AG "=" re-AG "%" ))

	 (q-no-re   ti:mail-parse-name-not-accept)

	 (mail      (or (ti::mail-parse-email line) ""))
	 (account   (if (= 2 (length mail))
			(nth 0 mail)
		      "#@$@#$@#$"))     ;just some dummy

	 fn
	 sn                             ;first, surname
	 pick
	 w
	 w1
	 w2
	 D                              ;debug
	 beg
	 end
	 beg1
	 end1
	 beg2
	 end2
	 tmp
	 list)

    (if D
	(setq D D))                 ;XE 19.14 ByteComp silencer, no-op

    (catch 'found

      ;;  It's most important that the match test are made IN THIS ORDER
      ;;  - Quote test cannot precede vax name test.
      ;;  - Try most restrictive first.

      ;; ..............................................................
      ;;  VAX is identified by "::" marks

      (when (string-match "::" line)
	(setq list (ti::mail-get-2re fs-vax line))
	(when (not (string= "" (nth 0 list)))
	  (setq D "vax1")
	  (throw 'found t))
	(setq list (ti::mail-get-2re fs-vax2 line))
	(when (not (string= "" (nth 0 list)))
	  (setq D "vax2")
	  (throw 'found t)))

      ;; ............................................................
      ;; Try gateway addresses, rare, but seen in net still

      (when (string-match "%" line)
	(setq list (ti::mail-get-2re gtw-re1 line))
	(when (not (string= "" (nth 0 list)))
	  (setq D "gtw1")
	  (throw 'found t)))

      ;; X.400 address

      (when (string-match "/G=\\(.*\\)/S=\\([^/]+\\).*C=" line)
	(setq fn (match-string 1 line)
	      sn (match-string 2 line))
	(when (and fn sn)
	  (setq list (list fn sn)   D "gateX400")
	  (throw 'found t)))

      ;; .................................................................
      ;; foo.bar@example.com

      (when (string-match fs-fse line)
	(setq list (ti::mail-get-2re fs-fse line))
	(when (not (string= "" (nth 0 list)))
	  (setq D "mike.gordon")
	  (throw 'found t)))

      ;; ............................................................
      ;; And the rest , is there paren or ""  somewhere ?
      ;;

      ;;  If this is a full email string Joe@foo.com
      ;;  then get only the first part.

      (when (and (setq tmp (ti::string-match "^\\([^ \t]+\\)[@%][^ \t]+$" 1 line))
		 (setq tmp (ti::string-match re-AG 1 tmp)))
	(setq D "email")
	(setq list (list tmp ""))
	(throw 'found t))

      ;;   - if we get multiple match "stephane (s.) boucher" ,
      ;;     (L.G. \"Ted\" Stern) , pick the one that's longer.

      (if (string-match "\"\\(.*\\)\"" line)
	  (setq beg1 (match-beginning 1)  end1  (match-end 1)))

      (if (string-match "[(]\\(.*\\)[)]" line)
	  (setq beg2 (match-beginning 1)  end2  (match-end 1)))

      (cond
       ((and beg1 beg2)
	(if (> (- end1 beg1) (- end2 beg2))
	    (setq beg beg1  end end1)
	  (setq beg beg2  end end2)))
       (beg1
	(setq beg beg1  end end1))
       (beg2
	(setq beg beg2  end end2)))

      ;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

      (cond
       (beg
	;;   - Get list of words into W
	;;   - Someone wrote M. "Mack" Monroe, so the " is included
	;;     in words separate list
	;;   - The latter picks only NON-ABBREVIATED names, non-phones..
	;;     M. "Mack" Monroe --> Mack Monroe
	;;

	(setq pick (substring line beg end))
	(setq w (split-string pick "[][@%. \"]+"))

	(setq D "standard")
;;;     (ti::d! "w-1" w)

	(let ((case-fold-search nil))   ;case is important !!
	  (setq w                       ;returned word list
		(ti::list-find
		 w
		 q-no-re
		 (function
		  (lambda (arg elt)
		    (not (string-match arg elt))))
		 'all-items)))

;;;     (ti::d! "w-2" w)

	(cond
	 ((> (length w) 2)              ;too much abbrev names
	  ;;  pick first and account or last word

;;;       (setq W w AC account)

	  (setq w1 (nth 0 w)  w2 (nth (1-(length w)) w)  )

	  (setq tmp (ti::list-find
		     w account
		     (function
		      (lambda (arg elt)
			(string-match elt arg)))))

	  (if tmp                       ;account name found
	      (setq w2 tmp))

	  (setq list (list w1 w2)))

	 ((= 2 (length w))
	  (setq w1 (nth 0 w)  w2 (nth 1 w))
	  (setq list (list w1 w2)))

	 ((eq 1 (length w))
	  (setq list w))

	 (t
	  nil))

	(if list
	    (throw 'found t))))

      ;; .................................................................

      (setq list (ti::mail-get-2re fs-re2 line))
      (when (not (string= "" (nth 0 list)))
	(setq D "2.1")
	(throw 'found t))) ;; Catch end

;;;    (ti::d! "parsed" D  list)

    ;;   what should we return ?
    (if (and (string= (nth 0 list) "")
	     (string= (nth 1 list) ""))
	nil
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-email (line)
  "Try to parse various formats of 'From:' field from LINE.
Input is full LINE, possibly containing 'From' keyword.

Return:

  list          '(usrname site)
  nil           if cannot parse."
  (let* (account
	 site
	 tmp

	 ;; '.' is for firstname & surname combination
	 ;; '=' is for gateway form
	 ;; '|{' are scandinavian characters in name
	 ;; '+' Believe or not, but I just saw account name like
	 ;;     "Stephen M. Lacy" <sl31+@andrew.cmu.edu>

	 (A "[-a-zA-Z|{0-9_=.+]+")      ; alphabet
	 (As "[-a-zA-Z0-9.%]+")         ; site name

	 ;;  Note that username can have scandinavian {| marks
	 ;;  Normal site name
	 ;;  o   Simon.Marshall@mail.bar.foo.fi (Simon Marshall)
	 (re1 (concat "\\(" A "\\)@\\(" As "\\)"  ))

	 ;;  Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU
	 (re2 (concat "\\(" A "\\)\\(%" As "\\)"  ))

	 ;;  VAX address <"TNCLUS::TSYVANEN"@mailer.foo.fi>
	 (re-vax (concat "\\(\"" A "::" A "\"\\)@\\(" As "\\)"  ))
	 em                             ; email

	 ;;  "/G=Jamie/S=Lokier/OU=comlab/O=oxford/PRMD=UK.AC...
	 (re-x400
	  (concat "/G=\\([^/]+\\)/S=\\([^/]+\\)" ;fn sn
		  "/OU=\\([^/]+\\)/O=\\([^/]+\\)"
		  "/PRMD=\\([^/]+\\)")))
    (catch 'found
;;;      (setq LINE line RE re-x400)

      (if (null (string-match re-x400 line)) nil
	(setq account (concat (match-string 1 line) "." (match-string 2 line)))
	(setq site    (concat (match-string 3 line) "." (match-string 4 line)))

	;;  Now switch the last items PRMD=UK.AC --> ac.uk
	(setq tmp (match-string 5 line))
	(setq tmp (split-string tmp "[.]"))
	(setq site (downcase (concat site "." (nth 1 tmp) "." (nth 0 tmp))))
	(setq em (list account site))
	(throw 'found t))

      (setq em (ti::mail-get-2re re-x400 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re1 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re2 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re-vax line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t)))

    (if (< (length (nth 0 em)) 1)
	(setq em nil))
    em))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-regexp-list ()
  "Return list of regexps that match `Received:' header content.
The Return ed list content is

'((3  (re re re ..))
  (2  (re re re ..))
  (1  (re re re ..)))

Where the number indicated how many submatches can be read. E.g. Number
3 means, 3 submatches."
  (let* ((from  "[ \t]+from")
	 (spc   "[ \t\r\n]*")
	 (spc+  "[ \t\r\n]+")
	 (W     "[^][(){} \t\n]+")              ;;word
	 (word  (concat "\\(" W "\\)"))         ;;capturing word
	 (S     "[[({]+")                       ;;start
	 (E     "[])}]+")                       ;;end

	 ;; mail.compuserve.com (mail.compuserve.com (209.5.81.86))
	 ;; mail.msss.v.com [atl.asd.com [234.454.54]]

	 (re-word31
	  (concat from
		  spc word
		  spc S spc word
		  spc S spc word  spc
		  E))

	 ;;  Received: from [209.151.131.35] (HELO mx04.hotmail.com)
	 ;;     by elysium.ca (CommuniGate Pro SMTP 3.5)

	 (re-word32
	  (concat from
		  spc+ S word E         ;; from [209.151.131.35]
		  spc+ S W spc+ word E  ;; (HELO mx04.hotmail.com)
		  spc+ "by" spc+ word))

	 ;;  from hdn86-021.hil.compuserve.com(206.175.97.21) by

	 (re-word2a
	  (concat from
		  spc word
		  spc S spc word
		  spc E))

	 ;;   Propably faked received header?
	 ;;
	 ;;   from usinet cziegle (1Cust144.tnt1.coeur-dalene.id.da.uu.net
	 ;;       [208.254.107.144]) by ns.peace1.co.jp

	 (re-word2b
	  (concat from
		  "[^([{]+"
		  S spc word spc
		  S spc word spc
		  E))

	 ;;  Received: from usa.net - 206.133.11.158 by
	 ;;     ciudad.com.ar with Microsoft SMTPSVC; Mon, 2 Feb 1998 21:03:25

	 (re-word2c
	  (concat from
		  spc word spc+ "-"
		  spc+ word spc+ "by"))

	 ;; Received: from foo by relay1.UU.NET with SMTP
	 ;;    (peer crosschecked as: 1Cust185.tnt10.nyc3.da.uu.net
	 ;;     [153.37.131.185])

	 (re-word2d
	  (concat from
		  spc word spc "by"
		  spc word spc "with"))

	 ;;  from [206.102.180.52] by springfield.k12.il.us with ESMTP

	 (re-word2e
	  (concat from
		  spc S word E spc "by"
		  spc word spc "with"))

	 ;; Received: by SERVER02 with Internet Mail Service (5.5.2650.21)
	 ;; id <FVLHVM1Q>; Thu, 28 Feb 2002 16:26:29 -0500

	 (re-word11
	  (concat spc+ "by" spc+ W spc+ "with" spc+ W spc+ W spc+ W
		  spc+ S word E))

	 ;; from papaguena.upc.es by rita.upc.es

	 (re-word12 (concat from spc word spc "by" )))
    (list
     (list 3 (list re-word31
		   re-word32))
     (list 2 (list re-word2a
		   re-word2b
		   re-word2c
		   re-word2d
		   re-word2e))
     (list 1 (list re-word11
		   re-word12)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-line (regexp-list)
  "Parse all `Received:' IPs from current line with REGEXP-LIST.
The point must be placed just after the colon in header:

  Received:-!-

The -!- indicates the location of point."
  (let* (candidates)
    (catch 'done
      (dolist (elt regexp-list)
	(multiple-value-bind (submatch-max regexp-list)
	    elt
	  (dolist (regexp regexp-list)
	    (when (looking-at regexp)
	      (dotimes (count submatch-max) ;; starts counting from 0
		(push (match-string (1+ count)) candidates))
	      ;; Regexp-list
	      (throw 'done t))))))
    (nreverse candidates)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-string-smtp (string)
  "Parse SMTP field from 'Received:' STRING."
  ;; from 111.npgco.com (HELO NAZ-AZPIRE1) (24.121.15.77)
  (when (string-match
	 (concat
	  "\\<from[ \t\r\n]+[^ \t\r\n]+[ \t\r\n]+"
	  "(\\"           ;; BEGIN
	  "([^()]+)"      ;; First paren, required
	  "\\([ \t\r\n]+" ;; Second, optional
	  "([^()]+)\\)*"
	  "\\)") ;; END capture
	 string)
    (let* ((str  (match-string 1 string))
	   (list (list str))
	   ret)
      (if (string-match " " str)
	  (setq list (split-string str)))
      (dolist (elt list)
	(push (replace-regexp-in-string "\\[\\|\\]\\|[()\r\n]" "" elt)
	      ret))
      (nreverse ret))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-clean (string)
  "Remove () and newlines from STRING."
  (replace-regexp-in-string "[()\r\n]" "" string))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-from (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<from[ \t\r\n]+\\([^ \t\r\n]+\\)" string)
    ;;  from cm-24-121-15-77.flagstaff.az.npgco.com (HELO NAZ-AZPIRE1)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-by (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<by[ \t\r\n]+\\([^ \t\r\n]+\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-smtp-id (string)
  "Parse 'from' field from 'Received:' STRING."
  (cond
   ((string-match
     "[ \t\r\n]+id[ \t\r\n]+\\([^ ;\t\r\n]+\\)" string)
    (match-string 1 string))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-for (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<for[ \t\r\n]+\\([^ ;\t\r\n]+\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-date (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match
	 "^.+;[ \t\r\n]+\\(.+[^ \t\r\n]\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;; (ti::mail-parse-date-string "Thu, 18 Jul 1996 12:18:06 -0600")
;;; (ti::mail-parse-date-string "21 Aug 2003 20:41:15 -0000")
(defun ti::mail-parse-date-string (date)
  "Parse DATE notation.
Recognized format are:

  Thu, 18 Jul 1996 12:18:06 -0600
  21 Aug 2003 20:41:15 -0000

The timezone value is optional.

Returns alist;

   '(weekday
     dd
     mon
     mm           ;; numeric string, like \"07\" for \"Jul\"
     yyyy
     HH
     MM
     SS
     tz)"
  (cond
   ((string-match
     (concat "^[ \t\r\n]*"
	     "\\([A-Z]..\\),?[ \t\r\n]+"
	     "\\([0-9]+\\)[ \t\r\n]+"
	     "\\([A-Z]..\\)[ \t\r\n]+"
	     "\\([0-9][0-9][0-9][0-9]\\)[ \t\r\n]+"
	     "\\([0-9][0-9]\\):"
	     "\\([0-9][0-9]\\):"
	     "\\([0-9][0-9]\\)"
	     "[ \t]*\\(.*\\)")
     date)
    (list
     (match-string 1 date)
     (format "%02d" (string-to-int (match-string 2 date)))
     (match-string 3 date)
     (format "%02d"
	     (ti::month-to-number (match-string 3 date)))
     (match-string 4 date)
     (match-string 5 date)
     (match-string 6 date)
     (match-string 7 date)
     (match-string 8 date)))
   ((string-match
     (concat
      "^[ \t\r\n]*"
      "\\([0-9][0-9]?\\)[ \t\r\n]+"
      "\\([A-Z]..\\)[ \t\r\n]+"
      "\\([0-9][0-9][0-9][0-9]\\)[ \t\r\n]+"
      "\\([0-9][0-9]\\):"
      "\\([0-9][0-9]\\):"
      "\\([0-9][0-9]\\)"
      "[ \t]*\\(.*\\)")
     date)
    (list
     nil
     (match-string 1 date)
     (match-string 2 date)
     (format "%02d"
	     (ti::month-to-number (match-string 2 date)))
     (match-string 3 date)
     (match-string 4 date)
     (match-string 5 date)
     (match-string 6 date)
     (match-string 7 date)))))

;;; ----------------------------------------------------------------------
;;; (ti::mail-parse-date-string-iso8601 "Thu, 18 Jul 1996 12:18:06 -0600")
(defun ti::mail-parse-date-string-iso8601 (date &optional tz)
  "Parse DATE. See supported values in `ti::mail-parse-date-string'.
Return ISO 8601 date

    YYYY-MM-DD HH:MM:SS

If TZ is non-nil, add timezone information to the end."
  (interactive)
  (multiple-value-bind
      (dd
       mm
       yyyy
       HH
       MM
       SS
       tzone)
      (ti::mail-parse-date-string date)
    (format "%s-%s-%s %s:%s:%s%s"
	    yyyy mm dd HH MM SS (if tzone
				    (or tz "")
				  ""))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-string (string)
  "Parse 'Received:' Header STRING.
From this STRING

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

Return list:

    '((from    . HOST1)
      (smtp    . (HOST2 ...))
      (by      . HOST3)
      (smtp-id . ID)
      (for     . FOR)
      (date    . DATE))

The `cdr' of a key may be nil if no value was found.

References:

  `ti::with-mail-received-heade'."
  (list
   (cons 'from    (ti::mail-parse-received-string-from string))
   (cons 'smtp    (ti::mail-parse-received-string-smtp string))
   (cons 'by      (ti::mail-parse-received-string-by   string))
   (cons 'smtp-id (ti::mail-parse-received-string-smtp-id string))
   (cons 'for     (ti::mail-parse-received-string-for  string))
   (cons 'date    (ti::mail-parse-received-string-date string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received (&optional not-matching no-dupes)
  "Search all 'Receive:' fields and read site names followed by 'from' 'by'.
Duplicate entries are not added.

Point must be at the beginning of headers to search, and
point is advanced.

It is possible to can this function to find out from where the mail
originated and send complaint to postmasters of all those sites.

Input:

  NOT-MATCHING  string, If read entry matches this regexp it is not included in
		returned list
  NO-DUPES      flag, if non-nil then do not include duplicate addresses.

Return:

    '((IP IP IP) (IP IP) ..)   as they appear in Received fields.

Received headers explained:

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

    This Shows four pieces of useful information (reading from back to front,
    in order of decreasing reliability):

     - The host that added the Received line (host3)
     - The IP address of the incoming SMTP connection (ww.xx.yy.zz)
     - The reverse-DNS lookup of that IP address (host2)
     - The name of the sender used in the SMTP HELO command at the
       time of connection.

Real examples:

Received: from mailhost.worldnet.att.net ([206.85.117.127])
	  by mtigwc02.worldnet.att.net (post.office MTA v2.0 0613 )
	  with SMTP id AAD8244; Sun, 23 Mar 1997 23:03:10 +0000
Received: from mail.msss.v.com [atl.asd.com [234.454.54]]
	  by mediabrokers.cobracomm.com (8.8.5/8.6.5) with
	  SMTP id GAA07901 for <box17@mediabrokers.cobracomm.com>"
  (let* ((regexp-list (ti::mail-parse-received-regexp-list))
	 ret
	 candidates
	 ip-elt
	 ip-all-list)

    (while (re-search-forward "^Received:" nil t)
      (setq ip-elt nil)
      (setq candidates (ti::mail-parse-received-line regexp-list))

      (dolist (elt candidates)
	(when (and (stringp elt)
		   (string-match "\\." elt) ;;from PAPAGUENA, require dot(.)
		   ;; Is exclude in effect?
		   (or (null not-matching)
		       (not (string-match not-matching elt)))
		   (if no-dupes
		       (not (member elt ip-all-list))
		     t))
	  ;;  1) mailhost@inet.com --> inet.com
	  ;;  2) remove some garbage from string

	  (setq elt (replace-regexp-in-string ".*@" "" elt))
	  (setq elt (replace-regexp-in-string "[]()\n]" "" elt))

;;;       (ti::d! elt)

	  (if no-dupes
	      (push elt ip-all-list))   ;Needed for duplicate checking

	  (push elt ip-elt)))
      (if ip-elt
	  (push ip-elt ret)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-received-header 'edebug-form-spec '(body))
(put 'ti::with-mail-received-header 'lisp-indent-function 1)
(defmacro ti::with-mail-received-header (string &rest body)
  "With Mail 'received:' heading in STRING, run BODY.
For this STRING

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

The following access variables are available within BODY:

  received-header-data
  from              => host1
  smtp              => '(host2 ww.xx.yy.zz)
  smtp-id           => MAA04298
  by                => host3
  for               => host1
  date              => Thu, 18 Jul 1996 12:18:06 -0600

Note:

  Any of the variables may be nil, if no value found.

References:

  See functions ti::mail-parse-received-string-*
  and `ti::mail-parse-received-string'."
  `(let ((received-header-data (ti::mail-parse-received-string ,string)))
     (symbol-macrolet ((from     (cdr (assq 'from received-header-data)))
		       (smtp     (cdr (assq 'smtp received-header-data)))
		       (by       (cdr (assq 'by received-header-data)))
		       (smtp-id  (cdr (assq 'smtp-id received-header-data)))
		       (for      (cdr (assq 'for received-header-data)))
		       (date     (cdr (assq 'date received-header-data))))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-cleanup (string)
  "Remove indentation and extra whitescape from STRING."
  ;;  Remove indentation
  (ti::string-remove-whitespace
   (replace-regexp-in-string
    "[\r\n][ \t]+" "\n"
    (replace-regexp-in-string "[ \t][ \t]+" " " string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-paragraph (regexp &optional end-regexp)
  "Whois: Parse pragraph for the first REGEXP to END-REGEXP.
See `ti::mail-whois-parse'."
  (when (re-search-forward regexp nil t)
    (let ((beg (match-beginning 0)))
      (if (null end-regexp)
	  (forward-paragraph)
	(re-search-forward end-regexp)
	(beginning-of-line))
      (ti::mail-whois-parse-cleanup
       (buffer-substring beg (1- (point)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-referral ()
  "Parse referral if any. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (cond
     ((and (goto-char point)
	   (re-search-forward
	    ;; Found a referral to example.com
	    "^[ \t]*Found.*referral to \\([^ \t\r\n]+[a-z]\\)"
	    nil 'noerr))
      (match-string 1))
     ((and (goto-char point)
	   (re-search-forward
	    ;; Referral URL: http://example.com
	    "^[ \t]*referral[ \t]+URL:[ \]*\\([^ \t\r\n]+\\)"
	    nil 'noerr))
      (match-string 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-email ()
  "Whois: Parse unique email addresses from buffer.
See `ti::mail-whois-parse'."
  ;; mailto:abuse@foo.com
  ;; trouble: Spam: <mailto:abuse@foo.com>
  ;; changed: 20030912 <migration@foo.com>
  (let ((kill-regexp
	 (concat
	  "E?-?mail:[ \t]*"
	  "\\|\\(mailto\\|changed\\|updated\\):"
	  "\\|\\<[0-9]+\\>"))<
	  line
	  email
	  desc
	  seen
	  ret)
    (while (re-search-forward
	    (concat
	     "^[ \t]*.*[ ,;/\t]"
	     "\\([^/,;<> \t\r\n]+@[^/,;<> \t\r\n]+\\)")
	    nil 'noerr)
      ;; There is only one email at a line
      (setq email
	    (replace-regexp-in-string
	     "mailto:" ""
	     (match-string 1)))
      (unless (member email seen)
	(push email seen)
	(setq line (ti::buffer-read-line))
	;;  Remove that email from it
	(when (setq desc (replace-regexp-in-string
			  (regexp-quote email) "" line))
	  (setq desc
		(ti::string-remove-whitespace
		 (replace-regexp-in-string
		  "," " "
		  (replace-regexp-in-string
		   kill-regexp ""
		   (replace-regexp-in-string
		    "[ \t][ \t]+" " " desc))))))
	(if (and desc
		 (ti::nil-p desc))
	    (setq desc nil))
	(push
	 (list (if desc
		   (format "%s <%s>"
			   desc
			   email)
		 email)
	       email
	       desc)
	 ret)))
    ;; preserve order
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-whois-parse-paragraph-end-condition ()
  "Whois parse. See `ti::mail-whois-parse'."
  (concat
   "^[ \t]*\\(.+:[ \t]*[\r\n]"
   "\\|.*last update"
   "\\|.*servers in listed order\\)"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-1 ()
  "See `ti::mail-whois-parse-registrant'."
  (ti::mail-whois-parse-paragraph
   "^[ \t]*Registra\\(r\\|nt\\):.*[\r\n]+[ \t]*"
   (ti::mail-whois-parse-paragraph-end-condition)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-organization ()
  "See `ti::mail-whois-parse-registrant'."
  (ti::mail-whois-parse-paragraph
   "^[ \t]*Organi[zs]ation:[ \t]*[\r\n]+[ \t]*"
   (ti::mail-whois-parse-paragraph-end-condition)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-organization-2 ()
  "See `ti::mail-whois-parse-registrant'."
  ;; OrgName:    AT&T WorldNet Services
  ;; OrgID:      ATTW
  ;; Address:    400 Interpace Parkway
  ;; City:       Parsippany
  ;; StateProv:  NJ
  ;; PostalCode: 07054
  ;; Country:    US
  ;;
  ;;  ...
  ;;
  ;; # ARIN WHOIS database, last updated 2003-08-25 19:15
  ;; # Enter ? for additional hints on searching ARIN's WHOIS database.
  (ti::mail-whois-parse-paragraph
   "^OrgName:.*[\r\n]OrgID:"
   "^[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-domain ()
  "See `ti::mail-whois-parse-registrant'."
  ;; domain:  AHA.RU
  ;; type:    CORPORATE
  ;; descr:   Mr. Postman BBS
  ;; admin-o: ZENON-ORG-RIPN
  ;; nserver: dns1.zenon.net.
  ;; nserver: dns2.zenon.net.
  ;; created: 1996.10.01
  ;; state:   Delegated till 2003.11.01
  ;; changed: 1998.08.11
  ;; mnt-by:  ZENON-MNT-RIPN
  ;; source:  RIPN
  (ti::mail-whois-parse-paragraph
   (concat
    "^domain:[ \t]+[a-z].*\\.[a-z0-9].+[ \t\r\n]"
    ;;Licensee:
    ;;   Name:     Belgacom Skynet DnsMasters
    ;;   Company:  Belgacom Skynet SA/NV
    "\\|^Licensee:[ \t]*$")
   "^[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant ()
  "Whois: Parse registrant from buffer. See `ti::mail-whois-parse'."
  (let ((point (point))
	ret)
    (flet ((search (func)
		   (goto-char point)
		   (funcall func)))
      (dolist (func '(ti::mail-whois-parse-registrant-1
		      ti::mail-whois-parse-registrant-domain
		      ti::mail-whois-parse-registrant-organization
		      ti::mail-whois-parse-registrant-organization-2))
	(when (setq ret (search func))
	  (return ret))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-tech ()
  "Whois: Parse tech from buffer. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (or (ti::mail-whois-parse-paragraph
	 "^[ \t]*.*Technical Contact.*:"
	 (ti::mail-whois-parse-paragraph-end-condition))
	(cond
	 ((and (goto-char point)
	       (re-search-forward ":\\(.*tech.*@.*\\)" nil t))
	  (ti::mail-whois-parse-cleanup
	   (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-zone ()
  "Whois: Parse zone from buffer. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (or (ti::mail-whois-parse-paragraph
	 "^[ \t]*.*Zone Contact.*:"
	 (ti::mail-whois-parse-paragraph-end-condition))
	(cond
	 ((and (goto-char point)
	       (re-search-forward ":\\(.*zone.*@.*\\)" nil t))
	  (ti::mail-whois-parse-cleanup
	   (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
;;; It the response is like this, there is no information
;;; about the created, expires
;;;
;;;     # ARIN WHOIS database, last updated 2003-08-25 19:15
;;;     # Enter ? for additional hints on searching ARIN's WHOIS database.
;;;
(defun ti::mail-whois-parse-records ()
  "Whois: Parse records from buffer. See `ti::mail-whois-parse'.
Values examined are: expires, created and updated."
  (let* ((date-info
	  (list
	   ;;  10-Aug-1998
	   (list
	    (concat
	     "\\("
	     "\\([0-9][0-9]?\\)"
	     "-\\([A-Z][a-z][a-z]\\)"
	     "-\\([0-9][0-9][0-9][0-9]\\)"
	     "\\)")
	    ;; day month year
	    '(3 4 5))
	   ;;  10-08-1998
	   (list
	    (concat
	     "\\("
	     "\\([0-9][0-9]?\\)"
	     "-\\([0-9][0-9]?\\)"
	     "-\\([0-9][0-9][0-9][0-9]\\)"
	     "\\)")
	    '(3 4 5))
	   ;;  Mon, Aug 10, 1998
	   (list
	    (concat
	     "\\("
	     "[A-Z][a-z][a-z],[ \t]*"
	     "\\([A-Z][a-z][a-z]\\)[ \t]+" ;; Mon
	     "\\([0-9]+\\)[ \t]*,[ \t]*"   ;; day
	     "\\([0-9][0-9][0-9][0-9]\\)"  ;; year
	     "\\)")
	    '(4 3 5))
	   (list
	    (concat
	     ;; 2003-08-25 19:15
	     "\\("
	     "\\([0-9][0-9][0-9][0-9]\\)"
	     "-\\([0-9][0-9]\\)"
	     "-\\([0-9][0-9]\\)"
	     "[ \t]+[0-9][0-9]:[0-9][0-9]"
	     "\\)")
	    '(5 4 3))
	   (list
	    (concat
	     ;; 1998.08.11
	     "\\("
	     "\\([0-9][0-9][0-9][0-9]\\)"
	     "[.]\\([0-9][0-9]\\)"
	     "[.]\\([0-9][0-9]\\)"
	     "\\)")
	    '(5 4 3))
	   (list
	    (concat
	     ;; changed:  20001107 15:03:09
	     ;; changed:     registdom@tin.it 20030403
	     ;;
	     "\\(\\([0-9][0-9][0-9][0-9]\\)"
	     "\\([0-9][0-9]\\)"
	     "\\([0-9][0-9]\\)"
	     "\\)")))
	  '(5 4 3))

	 (search (list
		  (list
		   'expires
		   (concat
		    "\\("
		    "^[ \t]*Record[ \t]+expires[ \t]+on[ \t]+"
		    "\\|^[ \t]*Expires[ \t]+on"
		    "\\|^expire:[^\r\n0-9]+"
		    "\\|^[ \t]*expiration date:[ \t]+"
		    "\\)"))
		  (list
		   'created
		   (concat
		    "\\("
		    "^[ \t]*Record[ \t]+created[ \t]+on[ \t]+"
		    "\\|^[ \t]*Created[ \t]+on.*[ \t]+"
		    "\\|^created:[^\r\n0-9]+"
		    "\\|^[ \t]*creation date:[ \t]+"
		    "\\)"))
		  (list
		   'updated
		   (concat
		    "\\("
		    "^.*last.*updated?[ \t]+on[ \t]+"
		    "\\|^[ \t]*updated date:[ \t]+"
		    "\\|^changed:[^\r\n0-9]+"
		    "\\)"))))
	 (beg    (point))
	 ret)
    (dolist (elt search)
      (multiple-value-bind (type line)
	  elt
	(dolist (date-data date-info)
	  (multiple-value-bind (regexp pos-list)
	      date-data
	    (setq regexp (concat line regexp))
	    ;;  The order of the fields can be anything, start over
	    ;;  every time from the same point
	    (goto-char beg)
	    (when (re-search-forward regexp nil t)
	      (multiple-value-bind (raw day month year)
		  (list
		   (match-string 2)
		   (match-string (nth 0 pos-list))
		   (match-string (nth 1 pos-list))
		   (match-string (nth 2 pos-list)))
		(if (eq 3 (length month))
		    (setq month (ti::month-to-number
				 (capitalize month)
				 'zero)))
		(push (list
		       type
		       (list (format "%s-%s-%s" year month day)
			     raw))
		      ret))
	      (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-servers ()
  "Whois: Parse servers from buffer. See `ti::mail-whois-parse'."
  (when (re-search-forward "^[ \t]*Domain servers" nil t)
    (forward-line 1)
    (let ((beg (point))
	  (end (progn
		 (forward-paragraph)
		 (point))))
      (let (ret)
	(goto-char beg)
	;; Domain servers in listed order:
	;;
	;; NS1.GALLERYHOSTING.NET       209.19.90.117
	;; GHZ.DDAHL.COM                209.19.90.118
	;;
	(while (re-search-forward
		(concat
		 "^[ \t]+"
		 "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)"
		 "[ \t]+"
		 "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)")
		end 'noerr)
	  (push (list (downcase (match-string 1))
		      (match-string 2))
		ret))
	;; Domain servers in listed order:
	;;
	;; Name Server: ns1.dr-parkingservices.com
	;; Name Server: ns2.dr-parkingservices.com
	;;
	(unless ret
	  (goto-char beg)
	  (while (re-search-forward
		  (concat
		   "^[ \t]+Name[ \t]+Server:"
		   "[ \t]+"
		   "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)")
		  end 'noerr)
	    (push (list (downcase (match-string 1)) nil)
		  ret)))
	ret))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-admin ()
  "Whois: Parse Administrative Contact from buffer.
See `ti::mail-whois-parse'."
  (let ((point (point)))
    (cond
     ((and (goto-char point)
	   (re-search-forward "^[ \t]*Administrative Contact:" nil t))
      (forward-line 1)
      (let ((beg (point)))
	;;  Search "Technical Contact:"
	(when (re-search-forward "^[ \t]*.+:[ \t]*$" nil t)
	  (ti::mail-whois-parse-cleanup
	   (buffer-substring
	    beg (1- (line-beginning-position)))))))
     ((and (goto-char point)
	   (re-search-forward ":\\(.*admin.*@.*\\)" nil t))
      (ti::mail-whois-parse-cleanup
       (match-string 1))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-error-p (string)
  "Check if Whois call failed by examining STRING"
  (not (string-match
	(concat
	 "registra\\(nt\\|r\\):"
	 ;; domain:  AHA.RU
	 ;; type:    CORPORATE
	 ;; descr:   Mr. Postman BBS
	 ;; admin-o: ZENON-ORG-RIPN
	 ;; nserver: dns1.zenon.net.
	 ;; nserver: dns2.zenon.net.
	 ;; created: 1996.10.01
	 ;; state:   Delegated till 2003.11.01
	 ;; changed: 1998.08.11
	 ;; mnt-by:  ZENON-MNT-RIPN
	 ;; source:  RIPN
	 ;;
	 ;; domain:   siemens.at
	 ;; descr:    [organization]:Siemens AG
	 ;; descr:    [street address]:Siemensstr. 92
	 ;;
	 "\\|^domain:[ \t]+[a-z].*\\..*[\n\r]"
	 "\\(type\\|descr\\):"
	 "\\|^address:.*[^ \t\r\n]"
	 ;;
	 "\\|^# ARIN WHOIS database")
	string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse (string)
  "Parse whois output STRING.

Return:

   '((email      .  ((ADDED EMAIL REST)  ;; ADDED is \"REST <EMAIL>\"
		    ...))
     (registrant .  STRING)
     (admin      .  STRING)
     (tech       .  STRING)
     (records    .  ((expires DATE-ISO RAW-DATE)
		     (created DATE-ISO RAW-DATE)
		     (updated DATE-ISO RAW-DATE))
     (servers    .  ((host ip)
		     ...)))

Note:

  All the keys, like 'admin', are present in returned list, but any of the
  `cdr' values or their components may be nil, if no value was found.

  Do not relay in the order of these fields. They may change
  any time. Instead access the list entry with `assq'.

References:

  See functions ti::mail-whois-parse-*
  and macro `ti::with-mail-whois'."
  (with-temp-buffer
    (insert string)
    (ti::buffer-text-properties-wipe)
    (let* ((referral   (progn (ti::pmin)
			      (ti::mail-whois-parse-referral)))
	   (email      (progn (ti::pmin)
			      (ti::mail-whois-parse-email)))
	   (registrant (progn (ti::pmin)
			      (ti::mail-whois-parse-registrant)))
	   (admin      (progn (ti::pmin)
			      (ti::mail-whois-parse-admin)))
	   (tech       (progn (ti::pmin)
			      (ti::mail-whois-parse-tech)))
	   (zone       (progn (ti::pmin)
			      (ti::mail-whois-parse-zone)))
	   (records    (progn (ti::pmin)
			      (ti::mail-whois-parse-records)))
	   (servers    (progn (ti::pmin)
			      (ti::mail-whois-parse-servers))))
      (unless (and
	       registrant)
	(error "TinyLibMail: Cannot parse Whois string %s" string))
      (list
       (cons 'referral    referral)
       (cons 'email       email)
       (cons 'registrant  registrant)
       (cons 'admin       admin)
       (cons 'tech        tech)
       (cons 'zone        zone)
       (cons 'records     records)
       (cons 'servers     servers)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-whois 'edebug-form-spec '(body))
(put 'ti::with-mail-whois 'lisp-indent-function 1)
(defmacro ti::with-mail-whois (string &rest body)
  "For full ´whois' output STRING run BODY.

The following access variables are available within BODY. Any
of the values may be nil.

  email
  admin         Administrative Contact
  tech          Technical Contact
  zone          Zone Contact
  records
  servers       Domain servers

References:

  `ti::mail-whois-parse'."
  `(let ((whois-data (ti::mail-whois-parse ,string)))
     (symbol-macrolet (
		       (referral   (cdr (assq 'referral   whois-data)))
		       (registrant (cdr (assq 'registrant whois-data)))
		       (email      (cdr (assq 'email      whois-data)))
		       (admin      (cdr (assq 'admin      whois-data)))
		       (tech       (cdr (assq 'tech       whois-data)))
		       (zone       (cdr (assq 'zone       whois-data)))
		       (records    (cdr (assq 'records    whois-data)))
		       (servers    (cdr (assq 'servers    whois-data))))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
;;; Registrant:
;;; David L. Dahl (DDAHL-DOM)
;;;    PO BOX
;;;    Chicago, IL 60657
;;;    US
;;;
;;;    Domain Name: DDAHL.COM
;;;
;;;    Administrative Contact:
;;;       Dahl, David  (DD4553)              ddahl@DDAHL.COM
;;;       3450 N. Lakeshore Dr. #2605
;;;       Chicago, IL 60657
;;;       US
;;;       773-934-1738 fax: 847-746-8841
;;;    Technical Contact:
;;;       Network Solutions, Inc.(HOST-ORG) customerservice@networksolutions.com
;;;       21355 Ridgetop Circle
;;;       Dulles, VA 20166
;;;       US
;;;       1-888-642-9675 fax: 123 123 1234
;;;
;;;    Record expires on 31-Mar-2005.
;;;    Record created on 18-Sep-2002.
;;;    Database last updated on 23-Aug-2003 04:47:44 EDT.
;;;
;;;    Domain servers in listed order:
;;;
;;;    NS1.GALLERYHOSTING.NET       209.19.90.117
;;;    GHZ.DDAHL.COM                209.19.90.118
;;;    WWW.CONDOSYSTEMS.COM         64.202.114.20
;;;
;;;
(defun ti::mail-whois (site &optional options verb bin)
  "Call `whois' and return results.
Web interface is at http://www.internic.net/whois.html

Input:

  site          Top level domain. Make sure you have called
		´ti::mail-ip-top-level-domain' first.
  OPTIONS       list, additional options. E.g. -h HOST
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary."
  (let* ((path  (or bin
		    (get 'ti::mail-whois 'binary)
		    (executable-find "whois")
		    (error "No `whois' binary found.")))
	 args)
    (put 'ti::mail-whois 'binary path)
    (when (and options
	       (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (when (string-match "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" site)
      (setq args options)
      (push site args)
      (if verb
	  (message "TinylibMail: whois %s ..." site))
      (with-temp-buffer
	(apply 'call-process
	       path
	       nil       ;; input
	       '(t t)    ;; mix stdout and stderr
	       nil       ;; display
	       args)
	(if verb
	    (message "TinylibMail: whois %s ...done." site))
	(buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-nslookup-parse ()
  "Parse nslookup output in current buffer forward.

Buffer contains:

  Non-authoritative answer:
  Server:  this.server.com
  Address:  nnnn.nnn.nnn.nnn

  Name:    NAME.ANSWER.COM
  Addresses:  NNN.NNN.NNN.NNN,NNN.NNN.NNN.NNN

Return:

'(NAME.ANSWER.COM (NNN.NNN.NNN.NNN  NNN.NNN.NNN.NNN ..))."
  (let* (name
	 ip-list
	 (re            "[ \t]+\\([^ \t\r\n]+\\)")
	 (name-regexp   (concat "name:"  re))
	 (regexp1       (concat "address:"   re))
	 (regexp2       "addresses:[ \t]+\\([^\r\n]+\\)"))
    (when (re-search-forward "^[ \t]*$" nil t)
      (forward-line 1)
      (when (re-search-forward name-regexp nil t)
	(setq name (match-string 1))
	(cond
	 ((re-search-forward regexp1 nil t)
	  (setq ip-list (list (match-string 1))))
	 ((re-search-forward regexp2 nil t)
	  (let ((ip (match-string 1)))
	    (setq ip-list
		  (if (not (string-match "," ip))
		      (list ip)
		    (list (split-string ip "[ \t,]+")))))))))
    (if ip-list
	(list name ip-list))))

;;; ----------------------------------------------------------------------
;;;
;;;  % nslookup 204.253.213.3
;;;  Name Server:  example.com
;;;  Address:  131.228.134.50
;;;
;;;  Name:    librum.sourcery.com
;;;  Address:  204.253.213.3
;;;
;;;  Can also have string:
;;;
;;;  *** No address information is available for "mktg@inet.com"
;;;
;;;  NOTE: There may be "Addresses:"
;;;  =========================================================
;;;
;;;  Server:  ns3.tpo.fi
;;;  Address:  212.63.10.250
;;;
;;;  Name:    yahoo.com
;;;  Addresses:  216.115.109.6, 216.115.109.7
;;;
(defun ti::mail-nslookup (ip &optional options verb bin)
  "Run `nslookup' for IP.

Note:

  If IP address does not match 2-3 alphabetic character or max 3 digits
  at the end, then the address is not checked at all. It is immediately
  discarded.

Input:

  IP            numeric on normal site address.
  OPTIONS       list, additional options. E.g. -query=any
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary

Return:

  '(name . address)

If nslookup fails, the return value is '(ORIG-IP nil)"
  (let* ( ;;  It's faster to use absolute pathname.
	 ;;
	 (path  (or bin
		    (get 'ti::mail-nslookup 'binary)
		    (executable-find "nslookup")
		    (error "No `nslookup' binary found.")))
	 args)
    (put 'ti::mail-nslookup 'binary path)
    (when (and options
	       (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (with-temp-buffer
      (when verb
	(message "TinylibMail: nslookup %s ..." ip))
      (when (string-match
	     "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" ip)
	(setq args options)
	(push ip args)
	(apply 'call-process
	       path
	       nil     ;; input
	       '(t t)  ;; mix stdout and stderr
	       nil     ;; display
	       args))
      (when verb
	(message "TinylibMail: nslookup %s ...done." ip))
      (unless (ti::re-search-check "No address information")
	(ti::pmin)
	(ti::mail-nslookup-parse)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-nslookup 'edebug-form-spec '(body))
(put 'ti::with-mail-nslookup 'lisp-indent-function 1)
(defmacro ti::with-mail-nslookup (data &rest body)
  "with resault of `ti::mail-nslookup' DATA '(ip (ip ...)) run BODY.
The following variables are available during looping within BODY:

  ip-name  ip-found."
  `(multiple-value-bind (ip-name ip-list)
       (list
	(car ,data)
	(cdr ,data))
     (dolist (ip-found ip-list)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-dig (ip &optional options verb bin)
  "Run `dig' for IP.

Note:

  If IP address does not match 2-3 alphabetic character or max 3 digits
  at the end, then the address is not checked at all. It is immediately
  discarded.

Input:

  IP            numeric on normal site address.
  OPTIONS       list, additional options. E.g. -query=any
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary

Return:

  '(name . address)

If nslookup fails, the return value is '(ORIG-IP nil)"
  (let* ( ;;  It's faster to use absolute pathname.
	 ;;
	 (path  (or bin
		    (get 'ti::mail-dig 'binary)
		    (executable-find "dig")
		    (error "No `nslookup' binary found.")))
	 args)
    (put 'ti::mail-dig 'binary path)
    (when (and options
	       (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (with-temp-buffer
      (when verb
	(message "TinylibMail: dig %s ..." ip))
      (when (string-match
	     "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" ip)
	(setq args options)
	(push ip args)
	(apply 'call-process
	       path
	       nil     ;; input
	       '(t t)  ;; mix stdout and stderr
	       nil     ;; display
	       args))
      (when verb
	(message "TinylibMail: dig %s ...done." ip))
      (buffer-string))))

;;}}}
;;{{{ misc

;;; ............................................................ &misc ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-buffer  (&optional mode-list)
  "Return open mail buffer if one exists.
MODE-LIST is the search order precedence. It can take values
'mail-mode 'message-mode and any
other valid mail like modes.

Example:

  ;; Return some mail-mode buffer. If there is none, then
  ;; return some message-mode buffer.

  (ti::mail-get-buffer '(mail-mode message-mode))"
  (let* (list
	 buffer)
    (or mode-list
	(setq mode-list '(mail-mode message-mode mh-letter-mode)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(dolist (mode mode-list)
	  (when (eq major-mode mode)

	    ;;  We keep the separate mode in the plist
	    ;;
	    ;;  LIST: plist 'MODE1 --> '(buffer buffer ...)
	    ;;      : plist 'MODE2 --> '(buffer buffer ...)

	    (setq list (get 'list mode)) ;Read current list
	    (push (current-buffer) list) ;Add one
	    ;;  And update plist
	    (put 'list mode list)))))

    ;;  Step through mode lists and return first buffer

    (dolist (mode mode-list)
      (when (setq buffer (car-safe (get 'list mode)))
	(return)))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-signature-insert-break (&optional point)
  "Insert RFC signature break to current point or POINT if no sig break exist.
According to RFC there must be \"-- \\n\" before signature. The extra space
separates the signature from e.g. digest messages that come with \"--\\n\"

We try to find this string forward and it is not there we add one."
  (save-excursion
    (if point (goto-char point))
    (if (null (re-search-forward "^-- \n" nil t))
	(insert "-- \n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-yank (&optional prefix)
  "Yank message to current point and add optional PREFIX. GNUS/RMAIL."
  (let* (p
	 (yb (ti::mail-mail-buffer-name)) ;where is the yank buffer ?

	 ;;  See this mail is called from GNUS
	 ;;
	 ;;  - If GNUS isn't loaded, set buf name to nil

	 (gnus-buf (and (boundp 'gnus-article-buffer)
			(symbol-value 'gnus-article-buffer)))

	 ;;  Test if gnus-reply; the buffers are the same

	 (gnus-r (and gnus-buf
		      (string= gnus-buf yb))))
    (save-excursion
      (setq p (point))

      ;;  (mail-yank-original '(4))     ; mimic C-u C-c C-y == no indent
      ;;  - bypass all, see sendmail::mail-yank-original
      ;;    this is more robust, and runs no extra hooks
      ;;  - If in GNUS, the buffer will be *Article*, which is
      ;;    narrowed to headers...widen the buffer before yanking.

      (if (null gnus-r)
	  (progn                        ; normal mail
	    (mail-yank-original '(4)))
	(save-excursion (set-buffer yb) (widen))
	(insert-buffer yb))
      (ti::pmax)
      (delete-blank-lines)
      (if prefix
	  (string-rectangle p (point-max)  prefix)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-trim-buffer ()
  "Trim email message so that there are no trailing white spaces.
- at the beginning of message
- at the end of each line
- at the end of message.

If cannot find text start point, uses `point-min'. The point is not preserved.

Return:
  t         there is no text. All white spaces were removed
  nil       trimming done."
  (let ((beg  (ti::mail-text-start))
	ret)
    (goto-char beg)
    (ti::buffer-replace-regexp "[ \t]+$" 0 "") ;right hand spaces (ragged lines)
    (goto-char beg)

    ;;   Beginning of email message

    (ti::buffer-trim-blanks beg (point-max))
    (ti::buffer-delete-until-non-empty-line nil beg)

    (ti::buffer-delete-until-non-empty-line 'backward (point-max))
    (forward-line 1)

    ;;  Any text left ? Signing empty file is not sensible...

    (if (eq (point) beg)
	(setq ret t)
      ;;  Note: User may write message "123" to the body, but we must
      ;;  require final newline every time: "123\n", the trim
      ;;  command will remove any exeessive newlines.
      (ti::pmax)
      (if (not (char= (preceding-char) ?\n))
	  (insert "\n")))
    ret))

;;}}}

;;{{{ fields, headers

;;;  .......................................................... &fields ...

(defsubst ti::mail-field-space-count (field-name &optional field-value )
  "Check how many spaces is at the beginning of field.
Input:

  FIELD-NAME        If given, fetch FIELD-NAME like 'to' and check it's value.
  FIELD-VALUE       If given, use this string as field content. Argument
		    FIELD-NAME is ignored.

Return

  N                 Number of space."
  (or field-value
      (and (or (stringp field-name) (error "Missing field-name"))
	   (setq field-value (ti::mail-get-field field-name)))
      (error "No field"))
  (and field-value
       (length (ti::string-match "^[^ ]*\\( +\\)" 1 field-value))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-start (field-re &optional move max)
  "Return starting point of FIELD-RE or nil. Optionally MOVE to it.

Supposes that field has following format, the cursor -!- position
signifies returned point.

  field:-!-

Input:

  FIELD-RE      field regexp
  MOVE          should we move to found point? (beginning-of-line)
  MAX           search until MAX point"
  (let (ret)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward field-re max t)
	(beginning-of-line)
	(when (re-search-forward ":" max t)
	  (setq ret (point)))))
    (if (and move ret)
	(goto-char ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-next-field-start (&optional move back max)
  "Return starting point of next field or nil. Optionally move to field.

Note:

  If you're somewhere else than inside header area, the return value
  is not defined.

Input:

  MOVE          move to point
  BACK          move backward (field start)
  MAX           search until this point. PLEASE USE THIS TO LIMIT SEARCH

Return:

  point
  nil"
  (let ((func (if back 're-search-backward 're-search-forward))
	opoint
	point
	ret)
    (save-excursion
      (if (null back)
	  (end-of-line))

      (if (and (bobp) back)             ;first field
	  (setq ret (point))

	;;   Next line must have text, otherwise the headers have ended
	;;   alredy
	;;
	;;   Header1:
	;;   Header2:
	;;
	;;   BODY-OF-TEXT

	(cond
	 ((save-excursion
	    (forward-line 1)
	    (looking-at ".*[a-zA-Z0-9]"))
	  (setq opoint (point))

	  ;;  In the last field, the previsu regexp skips too much,
	  ;;  see where the cursor (*) is. We search backward if possible
	  ;;  to find header separator (empty line)
	  ;;
	  ;;  Header:
	  ;;    last header text
	  ;;
	  ;;  *BODY

	  (when (progn
		  (when (and (setq point (funcall func "^[^ \t]" max t))
			     (eq func  're-search-forward))
		    (goto-char opoint)  ;Try again
		    (if (re-search-forward "^$" nil t)
			;; no, it was further ahead, use previous search pos
			(if (< (point) point)
			    (setq point (point)))))
		  point)
	    (goto-char point)
	    (beginning-of-line)
	    (setq ret (point))))
	 (t
	  ;;  Hm, next line is empty line, not a field for us any more.
	  nil))))

    (if (and move ret)
	(goto-char ret))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-field-string-wrap (string)
  "Wrap i.e. delete embedded newlines in string.

X-My: one line
   two line
   three line.

=>

X-My: one line two line three line."
  (replace-regexp-in-string "[\r\n][ \t]+" " " string))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-field-string-p (string)
  "Check if string starts with Field:
Subexpression 1 contains field name and 2 contains rest."
  (string-match "^\\([A-Z][^:]+\\):\\(.*\\)" string))

;;; ----------------------------------------------------------------------
;;;FIXME:
(defun ti::mail-field-line-p ()
  "Return `field' name if the bginning of line contains 'NNNN:'."
  (let ((str (buffer-substring
	      (line-beginning-position)
	      (line-end-position))))
    (when (ti::mail-field-string-p str)
      (match-string 1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-read-line-at-point (&optional wrap)
  "Read whole header field at point. Field may continue in separate line.
Point -!- must be at the beginning line of field.

X-More: this is one line-!-
  That is wrapped to send
  and even third.

If WRAP is non-nil, call `ti::mail-field-string-wrap'."
  (let ((beg (line-beginning-position))
	(end (ti::mail-next-field-start)))
    (when (and beg end)
      (let ((line (buffer-substring beg (1- end))))
	(if wrap
	    (ti::mail-field-string-wrap line)
	  line)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-read-fuzzy (&optional wrap)
  "Read whole header field at point.
The point can be anywhere in the field.
If WRAP is non-nil, call `ti::mail-field-string-wrap'."
  (save-excursion
    (beginning-of-line)
    (unless (ti::mail-field-line-p)
      (ti::mail-next-field-start 'move 'back))
    (ti::mail-field-read-line-at-point wrap)))

;;; ----------------------------------------------------------------------
;;;FIXME:
(defun ti::mail-current-field-name  ()
  "Return name of field at current point or nil."
  (save-excursion
    (when (or (not (bolp))
	      (and (bolp)
		   ;;  Newly opened line - continuation of e.g. To: field.
		   (looking-at "^[ \t]*$")))
      (ti::mail-next-field-start 'move 'back))
    (ti::mail-field-line-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-email-send-p (&optional header-regexp)
  "Check if point is at field To, Cc or Bcc"
  (let ((field (ti::mail-current-field-name)))
    (when (and field
	       (string-match
		(or header-regexp
		    "^\\(to\\|cc\\|bcc\\)$")
		field))
      field)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-email-address-p ()
  "Check if point is at field To, Cc, Bcc, From, Sender."
  (ti::mail-field-email-send-p
   "^\\(to\\|cc\\|bcc\\|from\\|sender\\)$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field-in-body (list)
  "Kill LIST of field that are inserted into body of message."
  (ti::narrow-safe (ti::mail-text-start) (point-max)
    (dolist (header list)
      (ti::mail-kill-field header))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field (field-re &optional replace-str)
  "Delete header field. Remember to supply Anchor '^' in FIELD-RE.

Input:

  FIELD-RE      any regexp matching a line
  REPLACE-STR   replace field content with this

Return:

  t             field changed or killed
  nil           nothing done [field not exist]"
  (let ((hdr-end  (ti::mail-hmax))
	beg
	end)

    (when hdr-end
      (if replace-str
	  (setq replace-str (ti::string-verify-ends replace-str " " nil 'beg)))

      (save-excursion
	(when (and (setq beg  (ti::mail-field-start field-re 'move))
		   (setq end  (ti::mail-next-field-start))
		   (<= end hdr-end))
;;;             (setq F field-re B beg E end)
	  (if replace-str
	      (progn
		(delete-region beg end)
		(insert (concat replace-str "\n")))
	    (beginning-of-line)
	    (delete-region (point) end)
	    t))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-field-1 (field)
  "Read FIELD by finding regexp matching '^FIELD:'.
Starting searching from the beginning of buffer. You are encouraged to call
this function instead of `ti::mail-get-field' if you want to get the
field information fast e.g. in `post-command-hook'.

This function is not as reliable as `ti::mail-get-field', because
the search is not limited to header area, but for regular headers
you can use this function safely."
  (let ((re  (format "^%s:" field))
	beg
	end)
    (save-excursion
      (ti::pmin)
      (if (and (re-search-forward re nil t)
	       (setq beg (point)
		     end (ti::mail-next-field-start)))
	  (buffer-substring beg (1- end))))))

;;; ----------------------------------------------------------------------
;;; - This is almost the same as mail-utils.el/mail-fetch-field,
;;;   but offers more control. It can get citated fields too, if
;;;   ANY parameter is non-nil.
;;; - And it returns _strict_ content of the field, fetch-field strips
;;;   spaces away.
;;;
(defun ti::mail-get-field (field &optional any mode)
  "Return field content.

Input:

   FIELD         field name without anchor '^' and char ':'
   ANY           return any field. When non-nil, drops anchor ^
		 from the  ^field: criteria
   MODE          nil    read the field as is, returning all chars
			after the ':'
		 t      If field has only spaces, Return nil
		 'pure  Include header name as well as content.

Return:

   nil or contents of field."
  (let ((case-fold-search t)            ;ignore case = t
	(re (if any
		(concat field ":")      ; pick first one met
	      (concat "^" field ":")))  ; require STRICT HEADER

	(hmax (if any nil (ti::mail-text-start)))
	beg
	end
	ret)
    (save-excursion
      (when (and (setq beg (ti::mail-field-start re 'move hmax))
		 (setq end (ti::mail-next-field-start nil nil hmax)))
	(when (and (eq mode 'pure)
		   (looking-at "[\t ]*[^\n\t ]+")) ;not empty
	  (beginning-of-line)
	  (setq beg (point)))
	(setq ret (buffer-substring beg (1- end)))))
    (when (and mode
	       (stringp ret)
	       (string-match "^[ \t\n\r]*\\'" ret))
      (setq ret nil))
    ret))

;;; ----------------------------------------------------------------------
;;; - If you want simple filed adding to your mail, then have a look
;;;   at this instead:
;;;
;;;     (defconst my-mail-info-string "Emacs RMAIL in 19.28")
;;;     (setq mail-default-headers
;;;           (concat
;;;            "X-info: " my-mail-info-string "\n"))
;;;
(defun ti::mail-add-field (field text &optional look-field mode replace)
  "Add FIELD and puts TEXT into it.
If field already exist, replaces field text.
By default, field is added to the end of header.

Input:

  FIELD         string, like \"To\".
  TEXT          \\n at end is optional. _No_ colon and _no_ spaces.
  LOOK-FIELD    new field will be added after this. _No_ colon at end.
		if MODE is non-nil, field is added before this field

		If there is no LOOK-FIELD, nothing is done and nil
		is returned.

  MODE          see look-field
  REPLACE       if non-nil Any previous field is removed. You probably
		want to set this flag to non-nil if you only want unique
		field names.

Return:

  t             something done
  nil           nothing done, maybe look-field doesn't exist ?"
  (let* ((field-re (concat "^" field ":")))
    (save-excursion
      (cond
       (look-field
	(if replace
	    (ti::mail-kill-field field-re)) ;Remove
	(when (and look-field
		   (ti::mail-field-start (concat "^" look-field) 'move))
	  (unless mode                  ;use only forward
	    (ti::mail-next-field-start 'move mode))
	  (beginning-of-line)
	  (insert (concat field ": " text))
	  t))
       (t                               ;add to the end
	(if (mail-fetch-field field)
	    (ti::mail-kill-field field-re text)
	  (mail-position-on-field field)
	  (insert text))
	t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-add-to-field-string (field string &optional look-field sep )
  "Find FIELD and add STRING to the. Field is created if it does not exist.

Input:

  FIELD      string WITHOUT colon, anchor or spaces.
  STRING     added text
  LOOK-FIELD field name. If Field does not exist, add field after this field.
	     See `ti::mail-add-field'
  SEP        defaults to comma and space."
  (or sep
      (setq sep ", "))

  (save-excursion
    (ti::pmin)
    (let ((content  (mail-fetch-field field)))
      (if (ti::nil-p content)
	  (ti::mail-add-field field string look-field)
	(re-search-forward (concat "^" field ":"))
	(ti::mail-next-field-start 'move)
	(skip-chars-backward " \n\t")
	(insert sep string)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field-elt (re &optional field)
  "Kill all elts matching RE from FIELD, which defaults to cc.
Elements are supposed to be separated by commas.

Example:

   To: him@example.com
   CC: me@example.com, you@example.com

   ;;  If called with this

   (ti::mail-kill-field-elt \"me\")
   --> To: him@example.com
   --> CC: you@example.com

   ;; If called with this; all elts are matched and thus the
   ;; field is removed

   (ti::mail-kill-field-elt \".\")
   --> To: him@example.com"
  (let* (flag
	 str
	 fld)
    (setq field (or field "CC"))

    (when (setq fld    (ti::mail-get-field field))
      ;; remove spread lines

      (setq fld (replace-regexp-in-string "[\n\f\t ]+" "" fld))
      (setq fld (split-string fld "[,]+")) ; divide into items

      ;; ... ... ... ... ... ... ... ... ... ... ... ...  remove items . .

      (setq fld
	    (ti::list-find  fld re
			    (function
			     (lambda (arg elt)
			       (not (string-match arg elt))))
			    'all-items))

      ;; ... ... ... ... ... ... ... ... ... ... ... . build up string . .

      (dolist (elt fld)
	(if (null flag)
	    (setq flag  t               ;done 1st line
		  str   (concat " " elt))
	  (setq str (concat
		     str ", " elt
		     (if (> (+ (length str) (length elt))  70)
			 "\n  "  "")))))

      ;; ... ... ... ... ... ... ... ... ... ... ... ...  write new fld . .
      (if str
	  (ti::mail-kill-field (concat "^" field) str) ;replace
	;;  Remove whole field, all entries were discarded.
	;;
	(ti::mail-kill-field (concat "^" field))))))

;;; ----------------------------------------------------------------------
;;; - This is mainly for converting your mail to anon post by
;;;   removing any headers you might have added.
;;;
(defun ti::mail-kill-non-rfc-fields (&optional list)
  "Kill all non RFC fields unless LIST (HEADER-NAME-SYMBOL .. ) list is given.

References
  `ti::mail-required-headers'    ,default rfc headers"
  (let ((ptr (or list
		 (ti::mail-required-headers)
		 (error "(ti::mail-required-headers) returned nil")))
	(case-fold-search t)
	fld
	list)
    ;;  First we gather all valid headers to list
    (dolist (elt ptr)
      (setq fld (symbol-name elt))
      (when (setq elt (ti::mail-get-field fld))
	(ti::nconc list (format "%s:%s" (capitalize fld) elt))))
    ;; Now we kill all headers and yank the valid ones back.
    (ti::mail-hmax 'move)
    (delete-region (point-min) (point))
    (setq ptr list)
    (dolist (elt ptr)
      (insert elt "\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-all-email-addresses
  (&optional field-list abbrev-alist no-expand)
  "Return all email addresses from FIELD-LIST.

Input:

 FIELD-LIST     Eg. '(\"To\" \"CC\"). Default is To CC and BCC.
 ABBREV-ALIST   see function `ti::mail-abbrev-expand-mail-aliases'
 NO-EXPAND      if non-nil, Do not expand addresses on current buffer.

Return:
 '(str str ..)  notice that there may be \"\" empty strings"
  (let ((buffer  (if no-expand
		     (generate-new-buffer "*tmp*")
		   (current-buffer)))
	str
	mems
	field
	ret)
    (unwind-protect
	(progn
	  (or field-list (setq field-list '("To" "CC" "BCC")))

	  (when no-expand
	    (dolist (fld field-list)
	      (setq str
		    (concat (or str "")
			    (ti::mail-get-field fld nil 'pure)
			    "\n"))))

	  (with-current-buffer buffer
	    (if str (insert str))

	    (ti::save-with-marker-macro
	      (ti::mail-abbrev-expand-mail-aliases
	       (point-min)
	       (if str
		   (point-max)
		 (ti::mail-hmax))
	       abbrev-alist))

;;;         (pop-to-buffer (current-buffer)) (ti::d! "MT:ABB" field-list)

	    (dolist (elt field-list)
	      (when (setq field (mail-fetch-field elt))
		(setq mems (split-string field "[,\n]+")) ;members ?
;;;             (ti::d! elt field mems)
		(dolist (mem mems)
		  (unless (ti::nil-p mem)
		    (push mem ret))))))) ;; with-current + progn
      ;;  make sure temp buffer is removed.

      (if no-expand
	  (kill-buffer buffer)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-set-recipients (to-list &optional cc-list cc-flag)
  "Compose current mail message to TO-LIST and add info about CC-LIST.

Input:

  TO-LIST       List of real recipients.
  CC-LIST       List of additional recipients that are put to
		X-Cc-Info. These are not actual CC members.
  CC-FLAG       Treat CC-LIST as actual recipients. This is like combining
		TO-LIST and CC-LIST. No X-Cc-Info field is added."
  (when cc-flag
    (setq to-list (ti::list-merge-elements to-list cc-list)
	  cc-list nil))

  (ti::mail-kill-field "^To")
  (ti::mail-kill-field "^CC")
  (ti::mail-kill-field "^X-Cc-Info")

  (ti::mail-add-field "To" (pop to-list))
  (when to-list
    (ti::mail-add-field "Cc" (mapconcat 'concat to-list ",")))

  (when cc-list
    (ti::mail-add-field
     "X-Cc-Info"
     (concat "Additional recipient(s)\n  "
	     (mapconcat 'concat cc-list ",")))))

;;}}}
;;{{{ News, articles

;;; ............................................................ &News ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-news-buffer-p ()
  "Check if current buffer is news post, followup or the like."
  (interactive)
  (cond
   ((and (eq major-mode 'message-mode) (fboundp 'message-news-p))
    (ti::funcall 'message-news-p))
   ((and (eq major-mode 'message-mode) (boundp 'message-this-is-news))
    (symbol-value 'message-this-is-news))
   ((string-match "news" (symbol-name major-mode)))
   (t
    ;;  Gnus keeps it in 'message-mode', so search this header then
    (save-excursion
      (ti::pmin)
      (re-search-forward "Newsgroups\\|References:\\|Gcc:" nil t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-article-regexp-read-line (re &optional level)
  "Switch to article buffer; match RE at LEVEL and return match."
  (let (line)
    (or level
	(setq level (or level 0)))
    (ti::mail-with-article-buffer
     (ti::pmin)
     (if (re-search-forward re nil t)
	 (setq line (match-string level))))
    line))

;;; ----------------------------------------------------------------------
;;; - This is useful if you use same hook for both
;;;   regular mail posting AND for gnuis posting.
;;; - It makes it possible to decicede inside hook, which post
;;;   type this is. Eg. setting extra headers for NEWS and not
;;;   different for regular Mail
;;;
(defun ti::mail-news-reply-p ()
  "Return type of message being composed.
This function is meaningful _only_ when you use it inside
some GNUS or mail hook.  The buffer must be current mail buffer.

Return:
  'news         news
  nil"
  (let* ((mode          (symbol-name major-mode))

	 ;;             GNUS might not be loaded in this emacs
	 (gnus-buf      (if (boundp 'gnus-article-buffer)
			    ;; normally name is "Article"
			    (symbol-value 'gnus-article-buffer)
			  ""))

	 (mail-buf      (ti::mail-mail-buffer-name)) ;YANK buffer name?

	 ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GNUS
	 ;; - Detect news posting mode. The mail program uses
	 ;;   YANK from the gnus buffer "*Article*" So we can detect
	 ;;   if this is gnus post
	 ;; - gnus 'news-mail-reply-->rnewspost.el

	 (gnus          (string= gnus-buf mail-buf))

	 (news-mode     (or gnus
			    (string-match "news" mode)
			    (save-excursion ;Gnus
			      (ti::pmin)
			      (re-search-forward "^References:" nil t)))))
    (if news-mode
	'news)))

;;}}}
;;{{{ anon.penet.fi anon-nymserver.com

;;; ............................................................ &anon ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-anon-penet-p (email)
  "Check if EMAIL is penet anon address."
  (string-match "[an][an][0-9]+@.*penet.fi" email))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-anon-penet-to-p ()
  "Check if the TO: field contain anon.penet.fi address.

Return:
  nil
  email       if it's anon address."
  (let  ((to (ti::mail-get-field "to")))
    (if (and to (ti::mail-anon-penet-p to))
	to nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-nymserver-email-convert (email &optional na-mode)
  "Convert penet EMAIL address.

If NA-MODE is nil: do 'an' conversion
	anXXX@example.com   --> anXXX
	naXXX@example.com   --> anXXX
       VANITY@example.com   --> VANITY@example.com
    VANITY.an@example.com   --> VANITY@example.com
    VANITY.na@example.com   --> VANITY@example.com

If NA-MODE is non-nil:
    Then do opposite 'na' conversion"
  (cond
   (na-mode
    (if (string-match "\\(an\\)[0-9]+@\\|\\.\\(an\\)@" email)
	(setq email (ti::replace-match 1 "na" email))
      ;; the email is VANITY@example.com
      (if (string-match "\\(.*\\)\\(@.*\\)" email)
	  (setq email (concat
		       (match-string 1 email) ".na"
		       (match-string 2 email))))))
   (t
    (cond
     ((string-match "\\(na\\)[0-9]+@" email)
      (setq email (ti::replace-match 1 "an" email)))
     ((string-match "\\(\\.na\\)@" email)
      (setq email (ti::replace-match 1 "" email))))))
  email)

;;}}}
;;{{{ mime

;;; ............................................................ &mime ...

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-tm-featurep-p  ()
  "TM. Check if MIME is loaded."
  (and (featurep 'mime-setup)
       (not (featurep 'semi-setup))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-semi-featurep-p  ()
  "SEMI. Check if MIME is loaded."
  (featurep 'semi-setup))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-feature-p  ()
  "MIME. Check if TM/ or SEMI is available."
  (or (ti::mail-mime-tm-featurep-p)
      (ti::mail-mime-semi-featurep-p)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-tm-edit-p ()
  "TM. Check if mime edit is active."
  (and (boundp 'mime/editor-mode-flag)
       (symbol-value 'mime/editor-mode-flag)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-semi-edit-p ()
  "SEMI. Check if mime edit is active."
  (and (boundp 'mime-edit-mode-flag)
       (symbol-value 'mime-edit-mode-flag)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-tm-edit-mode-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-tm-edit-mode-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-tm-edit-mode-macro  (&rest body)
  "TM. Run body If mime edit mode is active in current buffer."
  `(when (and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-semi-edit-mode-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-semi-edit-mode-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-semi-edit-mode-macro  (&rest body)
  "SEMI. Run body If mime edit mode is active in current buffer."
  `(when (and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-funcall-0-macro 'lisp-indent-function 1)
(put 'ti::mail-mime-funcall-0-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-funcall-0-macro (func-tm func-semi)
  "Call function  FUNC-TM or FUNC-SEMI with no arguments."
  `(cond
    ((and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     (ti::funcall ,func-tm)
     t)
    ((and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     (ti::funcall ,func-semi))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-funcall-2-macro 'lisp-indent-function 3)
(put 'ti::mail-mime-funcall-2-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-funcall-2-macro (func-tm func-semi arg1 arg2)
  "Call function  FUNC-TM or FUNC-SEMI with ARG1 ARG2."
  `(cond
    ((and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     (ti::funcall ,func-tm ,arg1 ,arg2)
     t)
    ((and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     (ti::funcall ,func-semi ,arg1 ,arg2)
     t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-turn-on-mode ()
  "Turn on MIME mode. Do nothing if mime is not available.
Return t if mime was supported."
  (interactive)
  (cond
   ((ti::mail-mime-tm-featurep-p)
    (unless (ti::mail-mime-tm-edit-p)
      (ti::funcall 'mime/editor-mode))
    t)
   ((ti::mail-mime-semi-featurep-p)
    (unless (ti::mail-mime-semi-edit-p)
      (ti::funcall 'mime-edit-mode))
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-turn-off-mode ()
  "Turn off MIME mode. Do nothing if mime is not available.
Return t if mime was supported."
  (interactive)
  (cond
   ((ti::mail-mime-tm-featurep-p)
    (when (ti::mail-mime-tm-edit-p)
      (ti::funcall 'mime-editor/exit))
    t)
   ((ti::mail-mime-semi-featurep-p)
    (when (ti::mail-mime-semi-edit-p)
      (ti::funcall 'mime-edit-exit))
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-sign-region (&optional beg end)
  "MIME. Enclose region BEG END as signed.
Input:

BEG   Defaults to mail beginning or buffer beginning.
END   Defualts to `point-max'

Return:

nil  if mime is not available.
"
  (interactive)
  (ti::mail-set-region beg end)
  (ti::mail-mime-funcall-2-macro
   'mime-editor/enclose-signed-region
   'mime-edit-enclose-pgp-signed-region
   beg
   end))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-encrypt-region (&optional beg end)
  "MIME. Enclose region BEG END as encrypted
Input:

BEG   Defaults to mail beginning or buffer beginning.
END   Defualts to `point-max'

Return:

nil  if mime is not available.
"
  (interactive)
  (ti::mail-set-region beg end)
  (ti::mail-mime-funcall-2-macro
   'mime-editor/enclose-encrypted-region
   'mime-edit-enclose-pgp-encrypted-region
   beg
   end))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-tm-split-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-tm-split-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-tm-split-macro (&rest body)
  "TM. Define  variables `split'  `max' `parts' and run BODY if TM active.
You have to use variables `max' and `parts' otherwise you don't need this macro."
  `(when (boundp 'mime-editor/split-message)
     (let* ((split (symbol-value 'mime-editor/split-message))
	    (max   (symbol-value 'mime-editor/message-default-max-lines))
	    (lines (count-lines (point-min) (point-max)))
	    (parts (1+ (/ lines max))))
       (if (null split)
	   (setq split nil))            ; No-op Bytecomp silencer
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-maybe-p ()
  "Check if buffer possibly contain MIME sections.
if there is boundary string in header or if the TM -mime tags
'-[[' are found from buffer, then it's considered mime."
  (or (ti::mail-mime-p)
      (save-excursion
	(cond
	 ((featurep 'tm-edit)           ;TM.el
	  ;;   TM puth these markes to MIME section; Try to find one.
	  ;;   This can be only found if the mimi-edit mode is not
	  ;;   yet exited. Upon exit the message will match true
	  ;;   MIME (ti::mail-mime-p).
	  (ti::re-search-check (concat "^" (regexp-quote "--[["))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-p ()
  "Check if buffer has mime message. You probably want `ti::mail-mime-maybe-p'.
It must contain boundary field in the headers and the boundary
must be found from the message body itself.
Only the header is not enough to say it's a composed mime mail.

Content-Type: ...
  boundary=\"Multipart_Thu_Sep_19_12:46:36_1996-1\"
	   ^^^^^^^^^

This text must be found after the headers until the MIME criteria is
satisfied."
  (interactive)
  (let ((field (ti::mail-get-field "Content-Type" 'any))
	re)
    (when (and field
	       ;;  Content-Type field may not include "boundary"
	       ;;  --> it's not multipart mime.
	       (setq re (ti::string-match ".*boundary=.\\(.*\\)\"" 1 field)))
      (setq re (regexp-quote re))
      ;;   start finding the boundary text after the headers.
      (save-excursion
	(ti::pmin) (re-search-forward re) ;This is the header, ignore it
	(forward-line 1)
	(re-search-forward re nil t)))))

;;; ----------------------------------------------------------------------
;;;FIXME: not tested
;;;
(defun ti::mail-mime-qp-decode(from to)
  "Mime. Decode quoted-printable from region between FROM and TO."
  (save-excursion
    (goto-char from)
    (while (search-forward "=" to t)
      (cond ((char= (following-char) ?\n)
	     (delete-char -1)
	     (delete-char 1))
	    ((looking-at "[0-9A-F][0-9A-F]")
	     (delete-char -1)
	     (insert (hexl-hex-string-to-integer
		      (buffer-substring (point) (+ 2 (point)))))
	     (delete-char 2))
	    ((message "Malformed MIME quoted-printable message"))))))

;;; ----------------------------------------------------------------------
;;; (add-hook 'vm-select-message-hook 'ti::mail-mime-prepare-qp)
;;;
(defun ti::mail-qp-mime-prepare ()
  "Mime. Unquote quoted-printable from mail buffers.
Searches for tag:

content-transfer-encoding: quoted-printable"
  (interactive)
  (save-excursion
    (let ((case-fold-search t)
	  (type (mail-fetch-field "content-transfer-encoding"))
	  buffer-read-only)
      (cond
       ((and (stringp type)
	     (string-match "quoted-printable" type))
	(ti::pmin)

	(search-forward "\n\n" nil 'move)
	(message  "MIME Unquoting printable...")
	(ti::mail-mime-qp-decode (point) (point-max))
	(message  "MIME Unquoting printable...done"))))))

;;}}}

;;{{{ Mail sending

;;; .................................................... &mail-sending ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-plugged-p ()
  "Check if computer is on-line. This function relies on Gnus."
  (when (boundp 'gnus-plugged)
    (symbol-value 'gnus-plugged)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-sendmail-reset-send-hooks ()
  "Make `mail-send-hook' et al. buffer local and set to nil."
  (dolist (sym '(mail-send-hook
		 message-send-hook
		 mh-before-send-letter-hook))
    (when (boundp sym)
      (make-local-hook sym)
      (set sym nil))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-pure-env-macro 'lisp-indent-function 0)
(put 'ti::mail-sendmail-pure-env-macro 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-pure-env-macro (&rest body)
  "Reset all mail/message hooks/vars locally to nil and run BODY."
  `(let* (message-setup-hook
	  message-mode-hook
	  mail-mode-hook
	  mail-setup-hook
	  mail-archive-file-name
	  mail-default-headers
	  mail-default-reply-to)
     ;; byteComp silencer: "Not used variables."
     (if mail-mode-hook         (setq mail-mode-hook            nil))
     (if mail-setup-hook        (setq mail-setup-hook           nil))
     (if mail-archive-file-name (setq mail-archive-file-name    nil))
     (if mail-default-headers   (setq mail-default-headers      nil))
     (if mail-default-reply-to  (setq mail-default-reply-to     nil))
     ,@ body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-macro-1 'lisp-indent-function 3)
(put 'ti::mail-sendmail-macro-1 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-macro-1 (to subject send &rest body)
  "See `ti::mail-sendmail-macro' instead. This is low level function."
  `(progn
     (ti::mail-sendmail-pure-env-macro
      ;;   to subject in-reply-to cc replybuffer actions
      ;;
      (mail-setup ,to ,subject nil nil nil nil)
      (mail-mode)
      (ti::mail-kill-field "^fcc")
      (ti::mail-text-start 'move)
      ,@body
      (ti::pmin)
      (ti::kill-buffer-safe " sendmail temp") ;See sendmail-send-it
      (when ,send
	(mail-send-and-exit nil)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-macro 'lisp-indent-function 3)
(put 'ti::mail-sendmail-macro 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-macro (to subject send &rest body)
  "Send / construct mail according to parameters.
Use TO, SUBJECT and If SEND if non-nil, send mail after BODY finishes.

Point is at the beginning of body.

Note:

    `mail-mode-hook' `mail-setup-hook' `mail-archive-file-name'
    `mail-default-headers'

    are set to nil. If you need these, please copy them before calling this
    macro and restore their values in BODY, possibly calling
    and using them as sendmail normally would.

    The hooks are set to nil so that mail buffer is created fast and
    that nothing causes trouble when mail buffer is ready."
  `(let* ((BuffeR (ti::temp-buffer ti:mail-mail-buffer 'clear)))
     (save-window-excursion
       (with-current-buffer BuffeR
	 (ti::mail-sendmail-macro-1
	  ,to
	  ,subject
	  ,send
	  ,body)))))

;;}}}

;;{{{ Abbrevs: XEmacs and Emacs

;;; ......................................................... &abbrevs ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-abbrev-table  ()
  "XEmacs and Emacs Compatibility, Return mail abbrev hash table."
  (ti::package-require-mail-abbrevs)
  (cond
   ((ti::emacs-p)

    (if mail-abbrevs
	(ti::funcall 'mail-abbrevs-setup))

    (or mail-abbrevs
	(progn
	  (build-mail-aliases)
	  mail-abbrevs)
	mail-aliases))
   (t
    ;;  in Emacs this is a list, in XEmacs this is a HASH
    (or mail-aliases
	(progn
	  (condition-case err
	      (build-mail-aliases)
	    (error
	     ;;  See mail-abbrev.el
	     (when (get-buffer "mailrc")
	       (pop-to-buffer (get-buffer "mailrc")))
	     (error err)))
	  mail-aliases)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-abbrev-expand-mail-aliases  (beg end &optional alias-alist)
  "Expand aliases in region BEG END.
Please Cache results from `ti::mail-abbrev-get-alist' and
use the result as argument ALIAS-ALIST. Otherwise aliases are always
reuild from scratch."
  (interactive "*r")
  (let* (mb
	 me
	 word
	 exp)
    (cond
     ((and (require 'mailalias nil 'noerr) ;Emacs Feature only
	   (fboundp 'expand-mail-aliases))
      (ti::funcall 'expand-mail-aliases beg end))

     (t                                 ;Too bad, this is much slower
      (unless alias-alist
	(setq alias-alist (ti::mail-abbrev-get-alist)))

      (save-restriction
	(narrow-to-region beg end) (ti::pmin)
	(while (re-search-forward
		"^[ \t]+\\|^[ \t]*[\n,][ \t]*\\|:[ \t]*" nil t)
	  (when (setq word (ti::buffer-match"[^ \t,\n]+" 0))

	    (setq mb (match-beginning 0)
		  me (match-end 0))

	    ;;  Do not count field names, like  "CC:" words
	    (when (and (not (string-match ":$" word))
		       ;;  Is this abbrev ?
		       (setq exp (assoc word alias-alist)))
	      (setq exp (cdr exp))      ; Change alias to expansion
	      (delete-region mb me)
	      (insert exp)

	      ;;  This isn't very smart formatting, the layout
	      ;;  is so that each expansion is on it's own line,
	      ;;  no fancy lining up things -- Mail me back
	      ;;  with diff to this code if you code nicer one.

	      (when (looking-at "[ \t]*,") ;put on separate lines
		(goto-char (match-end 0))
		(when (not (looking-at "[ \t]*$"))
		  (insert "\n\t")
		  (beginning-of-line)))))))))))

;;; ----------------------------------------------------------------------
;;; See mailabbrev.el how to build your abbrevs.
;;;
(defun ti::mail-abbrev-get-alist  (&optional expand-until)
  "Return alist of all `mail-abbrevs'.
Build the abbrev table from your ~/.mailrc with command
\\[build-mail-abbrevs]. The following parameter is _not_ yet functional.

Input:
  EXPAND-UNTIL        expand until the RH elt is pure email.

Return:
  '((ABBREV-STRING . EXPANDED-STRING) (A . E) ..)"
  (let* (
	 (pre-abbrev-expand-hook        nil) ;; prevent recursion
	 (mail-abbrev-aliases-need-to-be-resolved t)
	 table
	 exp-list
	 elt)

    ;; XEmacs 19.14 no-op for ByteCompiler

    (unless mail-abbrev-aliases-need-to-be-resolved
      (setq mail-abbrev-aliases-need-to-be-resolved nil))

    (setq table (ti::mail-abbrev-table))

    (cond
     ((listp table) ;; mail-aliases is already in (A . S) form
      (setq exp-list table))
     (t                                 ;Vector
      ;;  We have to expand abbrevs by hand because XEmacs doesn't
      ;;  parse them like emacs mail-alias

      (when table
	(let ((tmp (generate-new-buffer "*ti::mail-abbrev*")))
	  (with-current-buffer tmp
	    (setq local-abbrev-table table)

	    (mapatoms
	     (function
	      (lambda (x)
		(setq elt (prin1-to-string (identity x)))
		(when (not (string= "0" elt)) ;abbrev in this slot?
		  (insert elt)
		  (end-of-line)

		  ;;  2000-09-03
		  ;;  BBDB does some voodoo with the abbrevs by
		  ;;  setting the function cell, and sometimes  calling
		  ;;  expand-abbrev by BBDB blessed abbrev gives error.
		  ;;  --> Don't bother with the error, since the
		  ;;  abbrevs is correctly expanded, but BBDB cries about
		  ;;  "wrong marker" or something.

		  (condition-case err
		      (expand-abbrev)
		    (error
		     (message
		      (concat
		       "tinylibmail: `expand-abbrev' signalled ERROR `%s'"
		       " while expanding `%s'")
		      (prin1-to-string err)
		      elt)))
		  (push (cons (symbol-name x) (ti::read-current-line)) exp-list)
		  (end-of-line)
		  (insert "\n"))))
	     table))
	  (kill-buffer tmp))))) ;; cond
    exp-list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-abbrevs-email-list  (&optional abbrev-alist)
  "Build email list of abbrevs; optionally use ABBREV-ALIST.
Only entries in this format in ~/.mailrc are returned. There must be
no \",\" chained lists in the line.

  alias[spaces]some[spaces]user@address.xx

Input:

  '((\"abbrev\" . \"expansion\") (A . E) ..)

Return:

  '(email email ...)"
  (let* (str
	 email
	 list)
    (dolist (elt
	     (or abbrev-alist (ti::mail-abbrev-get-alist)))
      (setq str (cdr elt))
      (when (null (string-match "," str)) ;filter out multiple mail lists
	(setq email (ti::string-match "\\([^< \t]+@[^> \t\n]+\\)" 0 str))
	(if email
	    (push str list))))
    ;; retain order
    (nreverse list)))

;;}}}

;;{{{ provide

(provide   'tinylibmail)
(run-hooks 'ti:mail-load-hook)

;;}}}

;;; tinylibmail.el ends here

	 (goto-char (point-min))
	 (delete-matching-lines
	  (mapconcat (lambda (x)
		       (format "\\<%s\\>" (regexp-quote x)))
		     list
		     "\\|"))
	 (setq str (buffer-string)))))
      (with-current-buffer (get-buffer-create tinylisp-:buffer-autoload)
	(goto-char (point-max))
	(insert str))))))

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
     file
     (get-buffer-create tinylisp-:buffer-autoload)))
   (t
    (error "Can't generate autoload statements. (buffer-file-name) is nil."))))

;;; ----------------------------------------------------------------------
;;;
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
	      `tinylisp-:buffer-autoload'
  VERB        Optional flag, if non-nil pop to result buffer."
  (interactive "DAutoload directory: \nsFiles matching regexp: \nP")
  (let* ((files (if (file-directory-p file-or-dir)
		    (ti::directory-files
		     file-or-dir
		     regexp
		     'absolute
		     '(and (not (file-directory-p arg))
			   (string-match "\\.el$" arg)))
		  (list file-or-dir))))        ;single filename
    (or buffer
	(setq buffer (get-buffer-create tinylisp-:buffer-autoload)))
    (ti::verb)
    (dolist (file files)
      (ti::package-autoload-create-on-file
       file
       buffer
       (null verb)
       no-desc))
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      AUTOLOAD UTILITIES
;;      These are admistrative utilies for package maintainer(s)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst tinylisp-:ignore-dir-regexp
  "\\(\\.\\(bzr\\|hg\\|git\\|svn\\|mtn\\)\\|CVS\\|RCS\\|_MTN\\|\\.\\.?\\)$"
  "Regexp to ignore directories.")

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-directory-recursive-macro 'lisp-indent-function 1)
(put 'tinylisp-directory-recursive-macro 'edebug-form-spec '(body))
(defmacro tinylisp-directory-recursive-macro (directory &rest body)
  "Start from DIRECTORY and run BODY recursively in each directory.

Following variables are set during BODY:

`dir'      Directrory name
`dir-list' All directories under `dir'."
  `(flet ((recurse
	   (dir)
	   (let* ((dir-list (tinylisp-directory-list dir)))
	     ,@body
	     (when dir-list
	       (dolist (elt dir-list)
		 (unless (string-match tinylisp-:ignore-dir-regexp elt)
		   (recurse elt)))))))
     (recurse ,directory)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinylisp-with-file-env-macro 'lisp-indent-function 0)
(put 'tinylisp-with-file-env-macro 'edebug-form-spec '(body))
(defmacro tinylisp-with-file-env-macro (&rest body)
  "Run BODY with all the interfering hooks turned off."
  `(let* (find-file-hooks
	  write-file-hooks
	  font-lock-mode
	  ;; buffer-auto-save-file-name
	  auto-save-hook
	  auto-save-default
	  (auto-save-interval 0)
	  (original-backup-inhibited backup-inhibited)
	  (backup-inhibited t))
     ;; Reset also global
     (setq-default backup-inhibited t)
     ;;  When each file is loaded to emacs, do not turn on lisp-mode
     ;;  or anything else => cleared file hooks. These are byte compiler
     ;;  silencers:
     (if (null find-file-hooks)
	 (setq find-file-hooks nil))
     (if (null write-file-hooks)
	 (setq write-file-hooks nil))
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
     ,@body))

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
(defun tinypath-tmp-autoload-file-footer (file &optional end)
  "Return 'provide and optional END of the file marker."
  (concat
   (format
    "\n\n(provide '%s)\n\n"
    (file-name-sans-extension (file-name-nondirectory file)))
   (if end
       (format ";; End of file %s\n"
	       (file-name-nondirectory (file-name-nondirectory file)))
     "")))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-directories (list)
  "Return only directories from LIST."
  (let* (ret)
    (dolist (elt list)
      (when (and (file-directory-p elt)
		 ;;  Drop . ..
		 (not (string-match
		       "[/\\]\\.+$\\|CVS\\|RCS"
		       elt)))
	(push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-write-loaddefs-file (file dest &optional verb)
  "Write ###autoload from FILE to DEST. VERB."
  (let ((generated-autoload-file dest))
    (ti::file-delete-safe dest)
;;;    (ti::package-autoload-loaddefs-create-maybe dest)
    (tinylisp-with-file-env-macro
      (with-current-buffer (find-file-noselect dest)
	(goto-char (point-max))
	;;  line added by `ti::package-autoload-loaddefs-create-maybe'
;;;        (re-search-backward "\n.*provide")
	(let ((point (point)))
	  (generate-file-autoloads file)
	  ;;  something was inserted?
	  (cond
	   ((eq (point) point)
	    (if verb
		(message "No autoload definitions in %s" file)))
	   (t
	    (let ((backup-inhibited t))
	      (save-buffer))
	    (kill-buffer (current-buffer)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-generate-loaddefs-file (file &optional dest verb)
  "Generate autoloads from FILE to DEST. VERB.
In interactive mode, the DEST is FILE-loaddefs.el and VERB mode is t."
  (interactive
   (let* ((file (read-file-name "Loaddefs from file: "))
	  (dest (format "%s-loaddefs.el"
			(file-name-sans-extension file))))
     (list file dest t)))
  (tinylisp-autoload-write-loaddefs-file file dest verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-generate-loaddefs-dir (dir &optional regexp)
  "Generate ###autoload from DIR excluding optional REGEXP."
  (interactive "DGenerate ###autoload loaddefs in dir\nsIgnore regexp: ")
  (let ((files (directory-files dir 'full "\\.el"))
	list)
    (dolist (file files)
      ;;  Ignore couple of other files as well.
      (unless (or (string-match "-\\(loaddefs\\|autoload\\)\\.el" file)
		  (and (stringp regexp)
		       (string-match "^[ \t]*$" regexp)))
	(push file list)))
    (tinylisp-autoload-generate-loaddefs-file-list list)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-generate-loaddefs-recursive (dir)
  "Generate ###autoload recursively starting from DIR."
  (interactive "DGenerate ###autoload recursive dir: ")
  (tinylisp-directory-recursive-macro
      dir
    (tinylisp-autoload-generate-loaddefs-dir dir)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-update-update-file-autoloads-1 (file dest)
  "Update ###autoload from FILE to DEST."
    (ti::package-autoload-loaddefs-create-maybe dest)
    (let ((generated-autoload-file dest))
      (tinylisp-with-file-env-macro
	(update-file-autoloads file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-real-update-file-autoloads (file)
  "Update ###autoload from FILE to FILE-loaddefs.el"
  (interactive "fUpdate ###autoload from lisp file: ")
  (let* ((dest (format "%s-loaddefs.el"
		       (file-name-sans-extension file))))
    (tinylisp-autoload-real-update-autoloads-loaddefs-1 file dest)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-build-from-file-1 (file dest)
  "Generate autoload from FILE to DEST."
  (with-temp-buffer
    (tinylisp-with-file-env-macro
     (ti::package-autoload-create-on-file
      file
      (current-buffer)
      'no-show
      'no-desc
      'no-path)
     (insert (tinypath-tmp-autoload-file-footer dest 'eof))
     (write-region (point-min) (point-max) dest))
    dest))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-build-from-file (file)
  "Generate autoload from FILE to FILE-autoload.el"
  (interactive "fGenerate quick autoload from lisp file: ")
  (let* ((dest (format "%s-autoload.el"
		       (file-name-sans-extension file))))
    (tinylisp-autoload-quick-build-from-file-1 file dest)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-quick-build-interactive-from-file (file)
  "Generate interactive function autoloads from FILE to FILE-autoload-interactive.el"
  (interactive "fGenerate quick autoload from lisp file: ")
  (let* ((dest (format "%s-autoload-interactive.el"
		       (file-name-sans-extension file))))
    (with
    ;; tinylisp-forward-user-option
    (tinylisp-autoload-quick-build-from-file-1 file dest))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-autoload-quick-build-dir-recursive (dir &optional include exclude)
  "Generate all autoloads from DIR.
Obey optional INCLUDE and EXCLUDE regexps."
  (interactive
   "DGenerate quick autoloads in dir\nsInclude re: \nsExlude re: ")
  (let ((files (directory-files dir 'full "\\.el"))
	list)
    (dolist (file files)
      ;;  Ignore couple of other files as well.
      (cond
       ;; Ignore these
       ((string-match "-\\(loaddefs\\|autoload\\)\\.el" file))
       ((and (stringp exclude)
	     (not (string-match "^[ \t]*$" exclude))
	     ((string-match exclude file))))
       ((or (not (stringp include))
	    (string-match "^[ \t]*$" include)
	    (string-match include file))
	(tinylisp-autoload-quick-build-from-file file))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinylisp-autoload-real-autoload-build-recursive
  (dir &optional include exclude)
  "Generate all autoloads recursively starting from DIR."
  (interactive
   "DGenerate all autoloads recursive dir: \nsInclude re: \nsExlude re: ")
  (tinylisp-directory-recursive-macro
      dir
    (tinylisp-autoload-quick-build-dir-recursive dir include exclude)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      OTHER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-forward-def (&optional back verb)
  "Go to next `def' forrward or `BACK'. VERB."
  (interactive "P")
  (let* ((opoint (point))
	 ret)
    (ti::verb)
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
      (end-of-defun)
      (if (re-search-forward "^(def" nil t)
	  (setq ret (match-beginning 0)))))
    (if ret
	(goto-char ret)
      (goto-char opoint)
      (if verb "No more `def' matches"))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-indent-around-point (&optional verb )
  "Indent current statement around the point. typically a function.
VERB."
  (interactive)
  (let* (msg
	 beg
	 end)
    (ti::verb)
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
  `tinylisp-:buffer-macro'
  `tinylisp-:macroexpand-function-list'"

  (interactive
   (list
    (intern-soft
     (completing-read
      "Expand with function: "
      (ti::list-to-assoc-menu tinylisp-:macroexpand-function-list)
      nil
      nil
      (car tinylisp-:macroexpand-function-list)))))
  (let* ((mac-re tinylisp-:regexp-macro-definition)
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
       (setq to-buffer (ti::temp-buffer tinylisp-:buffer-macro 'clear))
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
  (let* ((list '(
		 ("lisp" . eldoc-mode)
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
   (tinylisp-:property-show-mode
    (error "Turn off property show mode first."))
   (t
    (ti::bool-toggle tinylisp-:syntax-show-mode arg)
    (cond
     (tinylisp-:syntax-show-mode
      ;; (make-local-hook 'post-command-hook)
      (add-hook 'post-command-hook 'tinylisp-syntax-post-command nil 'local)
      (tinylisp-post-command-config))
     (t
      (remove-hook    'post-command-hook 'tinylisp-syntax-post-command)
      (tinylisp-post-command-config 'restore)))))
  (if verb
      (message
       "TinyLisp: syntax show mode is %s"
       (if tinylisp-:syntax-show-mode
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
		   (int-to-string count)
		 "")
	       (prin1-to-string (ti::compat-overlay-properties elt))))))
    (concat face-str " " ov-str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-property-post-command ()
  "Display property info according to `tinylisp-:property-show-mode'.
This is post command."
  (when (tinylisp-post-command-run-p)
    (let* ((record (equal '(64) tinylisp-:property-show-mode))
	   (ch     (char-to-string (following-char)))
	   str)
      (setq str
	    (format
	     "%s:%s"
	     (point)
	     (tinylisp-property-info tinylisp-:property-show-mode)))
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
			record info in buffer `tinylisp-:buffer-record'.
VERB                    verbose flag"
  (interactive "P")
  (ti::verb)
  (if tinylisp-:syntax-show-mode
      (error "Please turn off Syntax show mode first.")
    (cond
     ((null arg)
      (if (null tinylisp-:property-show-mode)
	  (setq tinylisp-:property-show-mode '(16))
	(setq tinylisp-:property-show-mode nil)))
     ((member arg '((4) (16) (64)))
      (setq tinylisp-:property-show-mode arg)))
    (cond
     (tinylisp-:property-show-mode
      (tinylisp-post-command-config)
      (when verb
	(message
	 "TinyLisp: Property show mode is on %s"
	 (if (equal arg '(64))
	     "(RECORDING)"
	   "")))
      ;; (make-local-hook 'post-command-hook)
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
       (mapcar 'car tinylisp-:table-snoop-variables))
      nil
      'match-it)
     tinylisp-:table-snoop-variables))))

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
    (let* (var
	   val)
      (or buffer
	  (setq buffer (current-buffer)))
      (pop-to-buffer (get-buffer-create tinylisp-:buffer-variables))
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
  (let* ((list (ti::system-autoload-function-list))
	 doc)
    (if (null list)
	(message "TinyLisp: No autoload functions found in Emacs.")
      (or buffer
	  (setq buffer
		(get-buffer-create tinylisp-:buffer-autoload)))
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
   (get-buffer-create tinylisp-:buffer-data)
   (ti::pmax))
  (pop-to-buffer (ti::system-match-in-hooks regexp tinylisp-:buffer-data))
  (sort-lines nil (point-min) (point-max)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-find-match-from-variables (var-regexp val-regexp)
  "Search variables for VAR-REGEXP and values matching VAL-REGEXP."
  (interactive "sMatch variable name: \nsMatch content in variable: ")
  (pop-to-buffer tinylisp-:buffer-data)
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
  "Display contents of hooks. See `tinylisp-:table-snoop-variables'.

ARG can be
   1           With prefix arg, variables values are recorded to
	       to buffer `tinylisp-:buffer-record' and

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
  (let* ((write     (equal arg '(4)))
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
   'tinylisp-:occur-history))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-occur (regexp &optional arg)
  "Run occur on REGEXP for whole buffer.
If ARG is non-nil, do not filter comment lines."
  (interactive (list (tinylisp-occur-i-args) current-prefix-arg))
  (let* ((obuffer (current-buffer)))
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
  (let* ((buffer (get-buffer tinylisp-:occur-buffer-name))
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
	(goto-line line)
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
  "Read EXP and record it's value to `tinylisp-:buffer-record' buffer."
  (interactive
   (list (read-from-minibuffer
	  "Eval: "
	  (ti::remove-properties (ti::buffer-read-word "^( \t\n'"))
	  read-expression-map t
	  'read-expression-history)))
  (let* ((buffer (ti::temp-buffer tinylisp-:buffer-record))
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
				    (backward-line 1)
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
  "Record the function info to `tinylisp-:edebug-instrument-table'.
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
	tinylisp-:edebug-instrument-table
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
  "Remove FUNCTION from `tinylisp-:edebug-instrument-table'."
  (interactive)
  (let* ((elt (assq function tinylisp-:edebug-instrument-table)))
    (setq tinylisp-:edebug-instrument-table
	  (delete elt tinylisp-:edebug-instrument-table))))

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
  (let* (edebug-all-defs)
    (save-excursion
      (ti::pmin)
      (while (re-search-forward "^(defun " nil t)
	;;  thi makes Edebug instrument the function
	(message "TinyLisp: instrumenting %s" (ti::read-current-line))
	(eval-defun 'instrument)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-edebug-display-instrumented-list  ()
  "List all instrumented function from cache `tinylisp-:edebug-instrument-table'.
Show results in `tinylisp-:buffer-record'. The display shows

  FUNCTION-NAME  BUFFER-OF-EVAL  LIVE-BUFFER FILE-NAME-FOR-BUFFER"
  (interactive)
  (let* ((buffer (ti::temp-buffer tinylisp-:buffer-record))
	 function
	 name
	 live-buffer
	 live-name
	 file)
    (display-buffer buffer)
    (tinylisp-with-current-buffer buffer
				  (ti::pmax)
				  (dolist (elt tinylisp-:edebug-instrument-table)
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

This function looks up `tinylisp-:edebug-instrument-table' and with raw
force reloads every package again thus wiping out Edebug instrumentation."
  (interactive)
  (let* (file-list
	 buffer-list
	 function
	 name
	 buffer
	 live-buffer
	 live-name
	 file
	 key
	 tmp)
    (dolist (elt tinylisp-:edebug-instrument-table)
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
    (setq tinylisp-:edebug-instrument-table nil)))

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

  `tinylisp-:edebug-instrument-table'"
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
  (let* ((buffer (symbol-value 'checkdoc-diagnostic-buffer))
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
  (let* ((regexp (concat "^;+[ \t]*" tag ":[ \t]*$"))
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
  (let* (missing
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
      (setq buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (tinylisp-lisp-mnt-tag-check-and-fix-buffer on-error))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylisp-lisp-mnt-tag-check-and-fix-dir (dir &optional on-error)
  "Check all Lisp commentary tags and fix as needed. Stop ON-ERROR.
Return.
 '((file ((missing-tags) (fixed-tags))) ..)."
  (interactive "DDir: \nP")
  (let* (stat
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

(add-hook 'tinylisp-:mode-define-keys-hook
	  'tinylisp-mode-define-keys)
(add-hook 'tinylisp-:elp-summary-mode-define-keys-hook
	  'tinylisp-elp-summary-mode-define-keys)

(tinylisp-install-menu)
(run-hooks 'tinylisp-:load-hook)

;;; tinylisp.el ends here
