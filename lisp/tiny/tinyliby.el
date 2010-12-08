;;; tinyliby.el --- Library of functions related to Emacs s(y)stem

;; This file is not part of Emacs

;;{{{ Id

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

;; ........................................................ &t-install ...
;; You put this file on your Emacs-Lisp `load-path', add following into your
;; .emacs startup file
;;
;;     (require 'tinyliby)
;;
;; But, normally that is not required. All these functions are autoloaded
;; from the main library, so use:
;;
;;      (require 'tinylibm)
;;
;; and it will cover everything

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Briefly
;;
;;      o    This is lisp code library. Package itself does nothing.
;;      o    Collection of Emacs s(y)stem related functions.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-and-compile
  (autoload 'adelete "assoc"))

(eval-when-compile
  (require 'advice))

;;}}}

;;{{{ setup: -- variables

(defconst tinyliby-version-time "2010.1208.0809"
  "Latest version number as last modified time.")

(defvar ti::system--describe-symbols-history nil
  "History of used regular expressions.")

(defvar ti::system--tmp-buffer "*ti::system-tmp*"
  "*Temporary buffer name.")

(defvar ti::system--desc-buffer "*desc*"
  "*Describe buffer.")

;;}}}
;;{{{ features, load list

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-package-where-is-source (package)
  "Try to locate PACKAGE as string. the one used in `load` command.
nil parameter is also accepted."
  (cond
   ((null package))                     ;Skip right away
   ((string-match "^\\([a-z]:\\)?[\\/]" package)
    package)
   ((string-match "\\.el$\\|\\.elc$" package)
    (locate-library package))
   (t
    (locate-library (ti::string-verify-ends package ".el$" ".el")))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-load-cleanup (element)
  "Remove ELEMENT from `after-load-alist' by replacing entry with nil."
  (let (forms)
    (dolist (elt after-load-alist)
      (setq forms (cdr elt))
      (dolist (frm forms)
        ;; change form to nil
        (if (equal frm element)
            (setcar forms nil))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-load-history-emacs-lisp-files ()
  "Return lisp of known Emacs lisp files in `load-history'."
  (let (list)
    (dolist (entry load-history)        ;point to functions
      (push (car entry) list))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-load-history-where-exactly (sym load-history-elt)
  "After `ti::system-load-history-where' return the elt whre entry is, check `require'.

Return:

  provide-symbol    This function returns the provide name which
                    defined the symbol.

Example of LOAD-HISTORY-ELT:

'(\"some-package.el\"
  (require . custom)
  gnus-undo-limit gnus-undo-mode gnus-undo-mode-hook ...
                  |
                  Suppose we search this SYM
  (provide . gnus-undo)  << This package provided the symbols
  ...)"
  (let ( ;; require
	provide
	item
	current
	ret)
    (dolist (elt load-history-elt)
      (cond
       ((ti::listp elt)
        (setq item (car elt))
        (cond
         ((eq item 'provide)
          (setq provide     (cdr elt))
          ;;   if RET has been; indicating that SYM was found,
          ;;   terminate on next provide that should be just after the sym list
          ;;
          ;;   (require ...)
          ;;   ...sym sym SYM sym sym
          ;;   (provide 'package)
          (when ret
            (setq ret provide)
            (return)))))
       ((symbolp elt)
        (setq current elt)))
      (when (eq sym current)
        (setq ret provide)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-load-history-where-1 (sym)
  "Look `load-history' to find SYM. The SYM may be function or variable name.

Return:

  list       feature's load history entry where variable were found.
  nil        no information in `load-history' about this variable."
  (dolist (entry load-history)          ;point to functions
    ;;  (FILE (REQUIRE) (REQ) SYM SYM SYM ...)
    (when (memq sym entry)
      (return entry))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-doc-where-is-source (sym)
  "Check documentation string of SYM to determine location of definition."
  (let* ( ;;  Defined in `textmodes/fill'.
         (sfile  (and (fboundp 'symbol-file)
                      (ti::funcall 'symbol-file sym)))
         (file   (and (stringp sfile)
                      ;;  Use Two a-z's because win32 has D:/ at front
                      (if (string-match "^[a-z][a-z].*/\\(.*\\)" sfile)
                          (match-string 1)
                        sfile))))
    (or (and file
             (or (and (ti::file-name-path-p file)
                      file)
                 (locate-library file)))

        (let ((doc (documentation-property
                    sym 'variable-documentation)))
          (when (string-match
                 `,(concat
		    ;; Emacs: run-at-time is an interactive Lisp function in `timer'.
		    "^.*Lisp[ \t]+function[ \t]+in[ \t'`]+\\([^ \n\r\f\t'`\"]+\\)"
		    ;; XEmacs:   -- loaded from "e:\usr\local\bin\emacs...
		    "\\|--[ \t]+loaded from[ \t\"]+\\([^ \n\r\f\t'`\"]+\\)")
                 (or doc "")))))))

;;; ----------------------------------------------------------------------
;;; Emacs doc string say: Defined in `frame'.
;;;
(defun ti::system-load-history-where-is-source (sym)
  "Check documentation or `load-history' to find SYM.
The SYM may be function or variable name.

Note:

  From Emacs point of view, a variable is defined at the point
  where `defconst' or similar `defcustom' or `defvar' is used.

Return:

  string     Absolute filename where the symbol was defined."
  (let (elt
	provide
	file)
    (when (setq elt (ti::system-load-history-where-1 sym))
      (setq file (car elt)		;default
            provide (ti::system-load-history-where-exactly sym elt))
      (or (and provide
               (ti::system-package-where-is-source (symbol-name provide)))
          (and (not (ti::file-name-path-p file))
               (ti::system-package-where-is-source  file))
          file))))

;;; ----------------------------------------------------------------------
;;; - Does little garbage collect...but what the heck!
;;; - lh = load-history
;;;
(defun ti::system-load-history-get (sym)
  "Return variables and functions defined by feature SYM.
The symbols are tested to be [f]boundp, so the list consists of
those elements only that actually exist in emacs.

Return:

  ((variable-list ..) (func-list ..))"
  (let* ((name (symbol-name sym))
         (list (cdr (assoc name load-history)))
         vl
         fl
         el
         ptr)
    (when list
      ;;  Search the variables' and funtions' start position in list
      (while (and list
                  (listp (car list)))
        (setq list (cdr list)))
      (setq ptr list)
      (while ptr
        (setq el (car ptr))
        (unless (listp el)
          (if (boundp el)
	      ;; FIXME: append is slow, use other
              (setq vl (append vl (list el))))
          (if (fboundp el)
              (setq fl (append fl (list el)))))
        (setq  ptr (cdr ptr))))
    (if (or vl fl)
        (list vl fl)
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-enable-disabled-options (&optional verb)
  "Map all variable symbols and enable options.
by default, Emacs comes with few presetting disabled. You
can enable those features (if you knwo what are disabled) wtih
code like:

    (put 'downcase-region 'disabled nil)

However, this function is more general and it can find
all user variables i.e. options, that might be disabled.

INPUT:

  verb   Print verbose messages."
  (interactive)
  (mapatoms
   (function
    (lambda (sym)
      (let (arg)
        (when (and (boundp 'sym)
                   (setq arg (memq 'disabled (symbol-plist sym)))
                   ;;  ARG = '(disabled t ..)
                   (nth 1 arg))
          (when verb
            (message "Tinyliby: Enabling variable `%s'" (symbol-name sym)))
          (put sym 'disabled nil)))))))

;;; ----------------------------------------------------------------------
;;;  - Be sure what your're doing if using this...
;;;
(defun ti::system-feature-kill (sym)
  "Kill feature SYM and its `load-history' information permanently."
  (let ((name (symbol-name sym)))
    ;;  Load history , dependencies remove
    (if (assoc name load-history)
        (setq load-history (adelete 'load-history name)))
    ;;  Kill the symbol from feature list
    (if (featurep sym)
        (setq features (delete sym features)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-unload-symbols (list)
  "Unload all variables and functions in LIST of symbols."
  (dolist (x list)
    (cond
     ((fboundp x)
      (fmakunbound x))
     ((boundp x)
      (makunbound x)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-unload (mode list)
  "According to MODE, unload all variables/features/functions in LIST.

MODE can be
'var        list of variables
'func       list of functions
'feature    list of features  , caution !! Be sure to get
            feature's variable and function list before you use this,
            since it'll delete all information that `unload-feature' needs.
            The `unload-feature' is not always good cmd, because it checks
            dependencies and may not allow you to delete a feature.

References:

  `ti::system-get-symbols'."
  (let (test-func
	kill-func)
    (cond
     ((eq 'var mode)
      (setq test-func 'boundp
            kill-func 'makunbound))
     ((eq 'func mode)
      (setq test-func 'fboundp
            kill-func 'fmakunbound))
     ((eq 'feature mode)
      ;;  - Emacs don't let us remove a feature if it contains some
      ;;    require statement. Be sure to get the information
      ;;    about the variables and func first before killing feature,
      ;;    since we destroy load-history information also!!
      ;;
      (setq  test-func 'featurep
             kill-func 'unload-feature))
     (t
      (error "unknown mode %s" mode)))
    (dolist (var list)
      ;; Test if exist
      (when (funcall test-func var)
        (cond
         ((eq kill-func 'unload-feature)
          ;;  Feature kill is special
          (ti::system-feature-kill var))
         ((eq kill-func 'fmakunbound)
          ;;  This is shooting with rocks, by calling advice,
          ;;  but it's safest this way.
          (ad-unadvise var)
          (funcall kill-func var))
         (t
          (funcall kill-func var)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-unload-feature (sym &optional verb)
  "Unload feature SYM, by cleaning `load-history' for all SYM symbols. VERB.
This is far more extensive wipeout than `unload-feature': All variables,
functions and ´load-history' is cleaned.

Return:
  t          If feature existed _and_ removed.
  nil        If feature does not exist."
  (interactive
   (list
    (intern-soft
     (completing-read
      "Complete feature to unload: "
      (ti::list-to-assoc-menu (mapcar 'prin1-to-string features))
      nil
      'must-match))))
  (let (list)
    (ti::verb)
    (when sym
      (when (setq list  (ti::system-load-history-get sym)) ;get (\var func\) list
        (ti::system-unload 'feature (list sym)) ;feature + load-history clean
        (ti::system-unload 'var     (nth 0 list))
        (ti::system-unload 'func    (nth 1 list)))
      (ti::system-feature-kill sym))
    (if verb
        (message "Feature now completely unloaded."))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-unload-feature-list (list)
  "Remove feature LIST, their variables and functions.
Input is list of features. Does not check any dependencies between features."
  (dolist (feature list)
    (ti::system-unload-feature feature)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::system-symbol-dolist-macro 'lisp-indent-function 1)
(defmacro ti::system-symbol-dolist-macro (symlist &rest body)
  "Map throught SYMLIST and execute BODY for each hook function.
You can refer to variables `hook' and `function' in BODY."
  `(let (hook-functions)
     (dolist (hook ,symlist)
       (when (boundp hook)
         (setq hook-functions (symbol-value hook))
         (if (and (not (ti::bool-p hook-functions))
                  (symbolp hook-functions))
             ;; single function in hook
             (setq hook-functions (list hook-functions)))
         (when (listp hook-functions)
           (dolist (function hook-functions)
             (when (and (not (eq function 'lambda)) ;skip lambda notation
                        (symbolp function))
               ,@body)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-remove-from-hooks (symlist re)
  "Look hook SYMLIST and remove all symbols matching RE.

If hook element is in form of  'lambda' instead of callable function symbol,
this element is ignored. This function cannot remove lambda functions
from hook, because match is done against `symbol-name'."
  (dolist (hook symlist)
    (when (boundp hook)			;is list element variable ?
      (cond
       ;; add-hook form
       ((listp (eval hook))           ;is hook in '(...) form ?
	(dolist (elt (eval hook))      ;step functions in list
	  (if (and (not (eq elt 'lambda)) ;skip lambda notation
		   (symbolp elt)
		   (string-match re (symbol-name elt)))
	      (remove-hook hook elt))))
       ;; setq form
       ((and (symbolp (eval hook)))
	(if (string-match re (symbol-name hook))
	    (set hook nil)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-match-in-hooks  (regexp &optional buffer)
  "Search SYMLIST for every hook functions that match REGEXP.
Write results i temporary buffer or BUFFER."
  (interactive
   (list
    (read-string "Regesp: ")))
  (or buffer
      (setq buffer (ti::temp-buffer ti::system--desc-buffer 'clear)))
  (with-current-buffer buffer
    (ti::system-symbol-dolist-macro
     (ti::system-get-symbols "-hook$\\|-functions$")
     (when (string-match regexp (symbol-name function))
       (insert (format "%-34s %s\n" (symbol-name hook)
                       (symbol-name function))))))
  (if (interactive-p)
      (pop-to-buffer buffer))
  buffer)

;;}}}
;;{{{ internal Symbols

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-get-symbols (re &optional test-form)
  "Return list of symbols that match RE.

The function 'mapatom' will return ALL symbols, no matter if they don't
even exist any more [fboundp, boundp].

You can supply your own TEST-FORM to cause it drop away certain atoms.
the current atom is stored in variable 'sym'.

Eg. test-form = '(or (fboundp sym) (boundp sym))"
  (let (list)
    (mapatoms
     (function
      (lambda (sym)
        (if (and (string-match re (symbol-name sym))
                 (or (null test-form)
                     (eval test-form)))
            (push sym list)))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-autoload-function-list ()
  "Return list of autoload function."
  (let (list)
    (mapatoms
     (function
      (lambda (sym)
        (when (ti::autoload-p sym)
          (pushnew sym list :test 'equal)))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-autoload-function-file-list (function-list)
  "Return unique filenames of autoload functions."
  (let (list
	str)
    (dolist (func function-list)
      (when (setq str (inline (ti::function-autoload-file func)))
        (pushnew (match-string 1 str)
		 list
		 :test 'string-equal)))
    list))

;;; ----------------------------------------------------------------------
;;; - There is another possibility, step through `load-history', but
;;;   since it's not in all emacs and it's buggy (at least in 19.28)
;;;   we don't use it here...
;;;
(defun ti::system-get-file-documentation (file &optional verb)
  "Gather all documentation from symbols in FILE.
You have to load the file into emacs first (eval it), because this
function reads the documentation properties from memory.

Input:

  FILE       absolute file name
  VERB       if non-nil, verbose messages are printed and
             the buffer is displayed when function finishes.

Return:

  buffer     pointer where documentation is stored."
  (interactive
   (let (file
	 feature)
     (setq file
           (call-interactively
            (function
             (lambda (f)
               (interactive "FDocs from lisp package file: ") f))))
     ;;  We must find the FILE.el name
     (or (setq feature (ti::string-match ".*/\\(.*\\)\\.el$" 1 file))
         (error "Can't read .el filename. %s " file))
     ;; there must be 'FILE feature
     (or (and (intern-soft feature)
              (setq feature (intern-soft feature)))
         (y-or-n-p (format "\
No '%s feature found, are you absolutely sure you have loaded the file? "
                           feature))
         (error "Abort."))
     (list file)))
  (let* ((tmp-buffer    (ti::temp-buffer ti::system--tmp-buffer 'clear))
         (file-buffer   (ti::find-file-literally file))
         (all-re        (concat "^[(]\\([ \t]*"
                                "defsubst\\|defvar\\|defconst"
                                "\\|defmacro\\|defun"
                                "\\|defadvice\\|deffoo\\|defvoo"
                                "\\)[ \t]*\\([^ \t\n\f()]+\\)"))
         (func-re       (concat "defsubst\\|defmacro\\|defun"
                                "\\|defadvice\\|deffoo\\|defvoo"))
         (verb          (or verb (interactive-p)))
         (count         0)
         ok-flag
         doc
         type
         sym-name
         sym
         paren)
    (unwind-protect
        (with-current-buffer file-buffer
          (ti::pmin)
          (while (re-search-forward all-re nil t)
            (setq type      (match-string 1)
                  sym-name  (match-string 2)
                  ;;  (defvar list)  --> (boundp 'list) = nil !! suprise
                  ;;
                  paren     (and (member type '("defvar" "defconst"))
                                 (looking-at "[ \t]*)"))
                  sym       (intern-soft sym-name)
                  doc       nil)
            (incf count)
            ;;  print messages for every 10th only, it's too fast to
            ;;  show every symbol...
            (if (and verb
                     (= 0 (% count 10)))
                (message (concat (number-to-string count) ": " sym-name)))
            ;; ... ... ... ... ... ... ... ... ... ... ... ... .. func ...
            (cond
             ((and (string-match "defadvice" type)
                   (or (null sym)
                       (not (fboundp sym))))
              (setq doc
                    (format
                     "tinyad: %s does nto exist yet. Can't read documentation."
                     sym-name)))
             ((string-match func-re type)
              (if (or (null sym)
                      (not (fboundp sym)))
                  (error (concat "Tinyliby: function not bound " sym-name))

                (setq doc
                      (format
                       "%-40s%s\n%s\n\n"
                       sym-name
                       "Function: "
                       (or (documentation  sym)
                           "not documented")))))
             ;; ... ... ... ... ... ... ... ... ... ... ... ... .. var  ..
             ((not paren)
              (if (or (null sym)
                      (not (boundp sym)))
                  (error (concat "Tinyliby: variable not bound " sym-name))
                (setq sym (intern-soft sym-name))
                (setq doc
                      (format "%-40s%s\n%s\n\n"
                              sym-name
                              (if (user-variable-p sym)
                                  "Option: " "Variable: ")
                              (or (documentation-property
                                   sym 'variable-documentation)
                                  "not documented"))))))

            (if doc
                (ti::append-to-buffer tmp-buffer doc)))
          (setq ok-flag t))             ;all completed
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . cleanup . .
      ;; - Recover from Ctrl-g, remove the loaded file.
      ;;
      (kill-buffer file-buffer))
    (if (and verb ok-flag)
        (pop-to-buffer tmp-buffer))     ;show contents
    (if verb
        (message ""))                   ;clear the echo area
    tmp-buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-describe-symbols-i-args (&optional arg)
  "Ask interactive arguments for `ti::system-describe-symbols'. ARG is prefix arg."
  (let (prompt
	char
	ans)
    ;;  When user calls us without arguments, offer menu to pick
    ;;  search item
    (unless arg
      (setq char (ti::read-char-safe "\
 v)ars o)options non-(O)options i)nteractive funcs f)uncs all RET)all"))
      (cond
       ((char-equal char ?v) (setq arg '(4)))
       ((char-equal char ?o) (setq arg '(16)))
       ((char-equal char ?O) (setq arg '64))
       ((char-equal char ?i) (setq arg 0))
       ((char-equal char ?f) (setq arg 9))))
    (setq prompt
          (cond
           ((equal arg '(4))
            "Describe <vars all> matching: ")
           ((equal arg '(16))
            "Describe <var options> matching: ")
           ((equal arg '(64))
            "Describe <var non-options> matching: ")
           ((equal arg 0)
            "Describe <funcs interactive> matching: ")
           ((equal arg 9)
            "Describe <funcs non-interactive> matching: ")
           ((integerp arg)
            "Describe <funcs all> matching: ")
           (t
            "Describe <all> symbols matching: ")))
    (list
     (read-from-minibuffer              ;ARG 1
      prompt nil
      nil nil
      'ti::system--describe-symbols-history)
     arg                                ;ARG 2
     ;;  Now handle exclude regexp      ;ARG 3
     (if (ti::nil-p (setq ans (read-from-minibuffer "exclude: ")))
         nil
       ans)
     (if (not (ti::listp arg))          ;ARG
         (y-or-n-p "Try to find key binding info too (takes longer)? "))
     nil)))                             ;ARG 5

;;; ----------------------------------------------------------------------
;; - This originates from the elisp manual pages somewhere,
;;   but I have made major additions and modifications to it.
;; - Actually this is massive add-on to the original one e.g.  it can look
;;   behind aliased functions  (fset, defalias) and has nice
;;   interactive interface.
;;
;; - I suggest that you add this to your .emacs, since
;;   this function is utterly useful for locating anything.
;;* (autoload 'describe-symbols  "tinyliby" t t)
;;* (if (not (fboundp 'describe-symbols))
;;*     (defalias 'describe-symbols 'ti::system-describe-symbols))
;;
;;
(defun ti::system-describe-symbols
  (pattern &optional mode exclude-re bind-info out-buffer)
  "Describe the Emacs Lisp symbols matching PATTERN.
All symbols that have PATTERN in their name are described.

MODE can be

  nil        return everything

  list 4     return variables           prefix arg \\[universal-argument]
  list 16    return only options,       prefix arg \\[universal-argument] \\[universal-argument]
  list 64    return only non-options,   prefix arg \\[universal-argument] \\[universal-argument] \\[universal-argument]

  nbr        return only functions
  nbr 0      return only interactive functions
  nbr 9      return only non-interactive functions

EXCLUDE-RE

  Excludes matches.

BIND-INFO

   If non-nil, then try to find binding info too. Note: if this flag
   is on, the time function executes decreases dramatically.

OUT-BUFFER

   Where to print the info.

References:

  `ti::system--desc-buffer'"
  (interactive (ti::system-describe-symbols-i-args current-prefix-arg))
  (let* ((buffer (or out-buffer ti::system--desc-buffer))
         subrp-test
         MF MFI MFF                     ;mode func
         MV MVO MVV                     ;mode var
         sym-list
         ALIAS
         FUNC
         (DEF "")
         tmp
         ;;  Build up the function cell into variable 'describe-func'
         (describe-func
          (function
           (lambda (s) ;; <-- symbol IN
             ;; ............................................. function ...
             ;; Print description of symbol.
             (cond
              ((and MF (fboundp s))
               ;;             (ti::d! 'function mode s MF MFI MFF)
               (setq ALIAS (ti::defalias-p s))
               (setq FUNC (or  ALIAS s))
               (cond                       ;; what is the main class ?
                ((and MFI (commandp FUNC)) ;; means interactive
                 (setq DEF "Command: "))
                ((and MFF)
                 (setq DEF "Function: ")))
               (if ALIAS
                   (setq DEF (concat DEF "Alias: " (symbol-name ALIAS))))
               (if (ti::autoload-p FUNC)
                   (setq DEF (concat DEF " Autoload: ")))
               (princ
                (format
                 "%-40s %s\n%s%s%s%s\n\n"
                 s
                 DEF
                 (or (and (setq tmp (ti::function-args-p FUNC))
                          (progn
                            ;; in xe, this doesn't print functions arguments,
                            ;; but the pacakge load information
                            ;; '(from "ange-ftp.elc")', but that's good to
                            ;; know too.
                            ;;
                            (concat tmp "\n")))
                     (and (ti::lambda-p FUNC)
                          (concat
                           (ti::string-left (prin1-to-string
                                             (symbol-function FUNC)) 75)
                           "..\n"))
                     "<Can't read func arglist>")
                 ;; .................................... function info ...
                 (when (or MF MFI MFF)
                   (concat
                    (cond
                     ((setq subrp-test (subrp (symbol-function s)))
                      "<Built-in-Lisp-primitive>\n")
                     ((ti::byte-compile-defun-compiled-p s)
                      "<Byte-compiled> ")
                     ((ti::defmacro-p s)
                      "<Macro> ")
                     (t
                      ""))

                    (if subrp-test
                        ""
                      (concat
                       "<Package: "
                       (or (car-safe (ti::system-load-history-where-is-source s))
                           "unknown")
                       ">"))))
                 (if (and
                      bind-info
                      (and (or MF MFI MFF)
                           (setq tmp (ti::keymap-function-bind-info s))))
                     (concat "\t" tmp "\n")
                   "\n")
                 (or (condition-case ()
                         (documentation  FUNC)
                       (error "<Function does not exist; not defined>"))
                     "not documented")))) ;; cond-function
              ;; ............................................. variable ...
              ((and MV (boundp s))
               ;;             (ti::d! 'variable mode s MV MVO MVV)
               (cond
                ((and MVO (user-variable-p s)) ;; option var
                 (princ
                  (format "%-40s %-9s%s\n%s\n\n"
                          s
                          "Option: "
                          (prin1-to-string (eval s))
                          (or (documentation-property
                               s 'variable-documentation)
                              "not documented"))))
                ((and MVV )
                 (princ
                  (format "%-40s %-9s%s\n%s\n\n"
                          s
                          "Variable: "
                          (prin1-to-string (eval s))
                          (or (documentation-property
                               s 'variable-documentation)
                              "not documented")))))))))))

    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let end ^^^
    (cond
     ((and (not (null mode))
           (listp mode))
      (setq MV t MVO t MVV t)
      (cond
       ((equal mode '(16))
        (setq MVV nil ))
       ((equal mode '(64))
        (setq MVO nil ))))
     ((integerp mode)
      (setq MF t MFI t MFF t)
      (cond
       ((= 0 mode)
        (setq MFF nil))
       ((= 9 mode)
        (setq MFI nil))))
     (t
      (setq MV t MVO t MVV t MF t MFI t MFF t)))
    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ options end ^^^
    ;; Build a list of symbols that match pattern.
    (mapatoms (function
               (lambda (sym)
                 (if (and (string-match pattern (symbol-name sym))
                          (or (null exclude-re)
                              (and (stringp exclude-re)
                                   (not
                                    (string-match exclude-re
                                                  (symbol-name sym))))))
                     (setq sym-list (cons sym sym-list))))))

    ;; Display the data.
    (if (null sym-list)
        (message "Describe symbols: No matches for given criterias.")
      (with-output-to-temp-buffer buffer
        (mapc describe-func (sort sym-list 'string<))
        (help-print-return-message)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::system-describe-symbol-summary (re &optional verb)
  "Make elisp script out of variables and functions that match RE. VERB.
Supposes that point is on buffer that is produced by
`ti::system-describe-symbols'

Return:

  buffer        where is ready output"
  (interactive "sRe: ")
  (let ((out-buffer    (ti::temp-buffer ti::system--tmp-buffer 'clear))
         list
         words
         var
         vlist
         flist)
  (or verb
      (setq verb (interactive-p)))
    (setq list
          (ti::buffer-grep-lines
           (concat (or re "")
                   ".*\\(command\\|variable\\|option\\|function\\):")))
    (with-current-buffer out-buffer
      (dolist (line list)
        (setq words     (split-string line)
              var       (nth 0 words))
        (cond
         ((string-match "Variable\\|option" line)
          (push var vlist))
         ((string-match "Command\\|Function" line)
          (push var flist))
         (t
          ;;  problem with line ?
          (insert (concat "#" line "\n")))))
      (lisp-mode)
      (insert "(defconst vlist\n  '(\n")
      (setq vlist (nreverse vlist))
      (dolist (elt vlist)
        (insert (concat elt "\n")))
      (insert ")\n \"Variables\")\n\n")
      (insert "(defconst flist\n  '(\n")
      (setq flist (nreverse flist))
      (dolist (elt flist)
        (insert (concat elt "\n")))
      (insert ")\n \"Functions\")\n\n")
      (indent-region (point-min) (point-max) nil))
    (if verb
        (pop-to-buffer out-buffer))
    out-buffer))

;;}}}

(provide    'tinyliby)

;;; tinyliby.el ends here
