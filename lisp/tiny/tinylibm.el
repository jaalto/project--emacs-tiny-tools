;;; tinylibm.el --- Library of s(m)all macros or functions
;; $Id: tinylibm.el,v 2.91 2007/05/07 10:50:07 jaalto Exp $

;;{{{ Id

;; Copyright (C)    1995-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylibm-version.
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
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;;; Install:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;      (require 'tinylibm)

;;}}}
;;{{{ Documentation

;;; Commentary:

;;  Preface, 1995
;;
;;      This is lisp function library, package itself does nothing.
;;      It contains small functions or macros.
;;
;;  Usage
;;
;;      You must not autoload this package; but always include
;;
;;          (require 'tinylibm)
;;
;;      You don't need any other require commands: all my other library
;;      functions get defined as well by using autoload.  Repeat: you don't
;;      have to put these in your packages:
;;
;;          (require 'tinylib)   ;; leave this out
;;          (require 'tinyliby)  ;; not needed either.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ Load forms

(require 'tinylibb)                     ;Backward compatible functions

;;{{{ function tests

;;; ----------------------------------------------------------------------
;;;
(defun ti::function-car-test (symbol test-val &optional test-func)
  "Test car of the SYMBOL against TEST-VAL with TEST-FUNC.
Function must be symbol, not a lambda form.

Return:

  symbol      yes, test succeeded
  nil         test failed"
  (if (and (not (sequencep symbol)) ;; list ?
           (symbolp symbol)         ;; chokes if not sequencep
           (fboundp symbol)

           ;;  Eg. symbol-function 'car  doesn't return list.
           ;;
           (listp (symbol-function symbol))
           (eq test-val
               (funcall (or test-func 'car)
                        (symbol-function symbol))))
      symbol
    nil))

;;; ----------------------------------------------------------------------
;;; `indirect-function' unfortunately returns the symbol-function, not
;;; the symbol name of the last function in the chain
;;;
(defun ti::defalias-p (symbol)
  "If function SYMBOL is alias, return it's truename. Otw Return nil."
  (let* (sym
         prev
         ret)

    (if (or (sequencep symbol)          ;lambda form ?
            (not (symbolp symbol))
            (not (fboundp symbol)))
        nil
      (setq sym (symbol-function symbol))
      (if (not (symbolp sym))
          nil
        (while (and (symbolp sym)   ;was alias, go into nesting levels
                    (fboundp sym)) ;must be function or user made mistake
          (setq prev sym)
          (setq sym (symbol-function sym)))
        (setq ret prev)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::subrp-p (symbol)
  "Test if function SYMBOL is built-in function.
Emacs default test (subrp 'move-to-column) returns nil, but according to
the documentation string that function is built-in. This function also
checks the documentation string."
  (when (and symbol
             (fboundp symbol))
    (or (subrp (symbol-function symbol))
        (string-match
         "built-in"
         (or (documentation-property symbol 'variable-documentation)
             "")))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::defmacro-p (symbol)
  "Test if function SYMBOL is in fact macro, created with defmacro.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  (ti::function-car-test symbol 'macro))

;;; ----------------------------------------------------------------------
;;;
(defun ti::autoload-p (symbol)
  "Test if function SYMBOL is in its autoload form.
Works with aliased symbols too.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  ;;  Get the REAL name if it is alias or use the func's SYMBOL name
  (let* ((func (or (ti::defalias-p symbol) symbol)))
    (ti::function-car-test func 'autoload)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::autoload-file (symbol)
  "Return autoload filename of function SYMBOL.
You already have to have tested the symbol with `ti::autoload-p'
or otherwise result from this function is undefined.

Return:
  string   Name of the library where symbol autolaod point to."
  ;;  Get the REAL name if it is alias or use the func's SYMBOL name
  (let* ((doc (prin1-to-string (symbol-function symbol))))
    (when (and (stringp doc)
               (string-match "autoload[ \t\"]+\\([^\"\r\n)]+\\)" doc))
      (match-string 1 doc))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::lambda-p (symbol)
  "Test if function SYMBOL was created with defsubst or is in lambda form.

Return:
  symbol     this can be truename of the function if it was aliased
  nil"
  (ti::function-car-test symbol 'lambda))

;;}}}

(defun ti::compatibility-advice-setup ()
  "Define compatibility advices for function that have changed."
  ;; Try to avoid loading advice.el.
  ;; The tests from tinylib-ad.el are duplicated here.
  (let ((msg ""))
    (if (and
         (ti::emacs-p)
         (not (ti::emacs-p "20.2")))
        (setq msg
              (concat
               msg
               "Tinylibm.el: tinylib-ad.el load reason: 1\n")))

    (if (and (fboundp 'define-key-after) ;; Emacs function
             (not
              (string-match
               "optional"
               (or (ti::function-args-p 'define-key-after) ""))))
        (setq
         msg
         (concat
          msg
          "Tinylibm.el: tinylib-ad.el load reason: define-key-after\n")))

    (if (and
         (not
          (string-match "noerr" (or (ti::function-args-p 'require) ""))))
        (setq msg
              (concat
               msg
               "Tinylibm.el: tinylib-ad.el load reason: require\n")))

    (if (and
         (ti::win32-p)
         ;;  It is unlikely that these are not in path, so this should not
         ;;  fail.
         (let ((exec-path exec-path))
           (push "c:/windows" exec-path)
           (push "c:/winnt" exec-path)
           (null (or (executable-find "command")
                     (executable-find "cmd")))))
        (setq
         msg
         (concat
          msg
          "Tinylibm.el: tinylib-ad.el load reason: executable-find\n")))

    (when (and (fboundp 'read-char-exclusive)
               (not (string-match
                     "prompt"
                     (or (ti::function-args-p 'read-char-exclusive) ""))))
      (setq
       msg
       (concat
        msg
        "Tinylibm.el: tinylib-ad.el load reason: read-char-exclusive")))

    (when (or (assoc "-debug-init" command-switch-alist)
              (assoc "--debug-init" command-switch-alist))
      (message msg))

    (when t ;; Enaled now.
      ;; 2000-01-05  If compiled this file in Win32 XEmacs 21.2.32
      ;; All the problems started. Make sure this is NOT compiled.
      (let ((path (locate-library "tinylib-ad.elc")))
        (when (and (stringp path)
                   (string-match "\\.elc$" path))
          (delete-file path)
          (message "\
  ** tinylibm.el: It is not recommend to compile tinylib-ad.el.
                  compiled file deleted %s" path))))

    ;; Backward compatible functions
    ;;
    ;; #todo: EFS does something to `require' function. Should it be loaded
    ;; first in XEmacs?
    (if (and (string-match "reason: require" msg)
             (ti::xemacs-p)
             (require 'efs))

        (unless (string= "" msg)
          (require 'tinylib-ad)))))

(ti::compatibility-advice-setup)

(eval-when-compile
  (when (and (ti::xemacs-p)
             (or (< emacs-major-version 20)
                 (and (eq emacs-major-version 20)
                      (< emacs-minor-version 3))))
    (message "\
tinylib.el: ** Ignore 'variable G3000' warnings. Corrected in XEmacs 20.3")))

;;}}}

;;{{{ variables

(defconst ti:m-debug-buffer "*ti::d!!*"
  "*Debug buffer where to write. Make a wrapper to use function ti::d!!
In your programs, like:

  (defvar my-package-:debug nil
    \"Debug. On/off.\")

  (defvar my-package-:debug-buffer \"*my-package*\"
    \"Debug record buffer.\")

  (defmacro my-package-debug (&rest args)
    \"Record debug info.\"
    (`
     (let* ( ;; write data to package private buffer.
            (ti:m-debug-buffer my-package-:debug-buffer))
       (if my-package-:debug
           (ti::d!! (,@ args))))))

  ;; this is how you use the debug capability in functions.
  ;; You must enable debug with (setq my-package-:debug t)
  ;;
  (defun my-package-some-function ()
     ;; ... code
     (my-package-debug \"here\" var1 win1ptr buffer \"\\n\" )
     ;; ... code)")

;;}}}

;;{{{ setup: version

(defconst tinylibm-version
  (substring "$Revision: 2.91 $" 11 16)
  "Latest version number.")

(defconst tinylibm-version-id
  "$Id: tinylibm.el,v 2.91 2007/05/07 10:50:07 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylibm-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylibm.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylibm-submit-bug-report ()
  "Submit bug report."
  (interactive)
  (ti::package-submit-bug-report
   "tinylibm.el"
   tinylibm-version-id
   '(tinylibm-version-id)))

;;}}}
;;{{{ code: small FORMS

;;; - To see what the'll become use for example:
;;;   (macroexpand '(decf x))

;;; ----------------------------------------------------------------------
;;;
(defmacro-maybe ti::definteractive (&rest body)
  "Define simple anonymous interactive function.
Function can take one optional argument 'arg'.
Very useful place where you can use this function is when you
want to define simple key functions

 (global-set-key
   \"\\C-cc\"
   (ti::definteractive
     (message \"You gave arg: %s\" (ti::prefix-arg-to-text arg))))"
  (` (function (lambda (&optional arg) (interactive "P") (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'nafboundp 'lisp-indent-function 2)
(defmacro ti::fboundp-check-autoload (function re &rest body)
  "Execute body if certain condition is true.

a) If not FUNCTION is not bound.

OR

a) function is bound in autoload state and
b) function's autoload definition matches regular expression RE

In short. Do BODY only if the autoload refer to file
matching RE. This is useful, if you define your own function that does
not exist in current Emacs, but may exist in newer releases. Suppose
following situation.

 (if (ti::xemacs-p)
     ;;  Make a forward declaration. Say it's in library
     (autoload 'run-at-time \"tinylibxe\"))

in file tinylibxe.el:

 (ti::fboundp-check-autoload 'run-at-time \"tinylibxe\"

  ;; XEmacs does not have this, but it somebody made it autoload.
  ;; The autoload refers to us, so we define the function.
  ;; If the autoload referred somewhere else, then this form doesn't
  ;; take in effect. Somebody else has actiated the autoload definition.
  ;;
  ...)"
  (` (cond
      ((or (and (fboundp (, function))
                (ti::autoload-p (, function))
                (string-match
                 (, re )
                 (nth 1 (symbol-function (, function)))))
           (not (fboundp (, function))))
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::narrow-safe 'lisp-indent-function 2)
(put 'ti::narrow-safe 'edebug-form-spec '(body))
(defmacro ti::narrow-safe (beg end &rest body)
  "Narrow temprarily to BEG END and do BODY.
This FORM preserves restriction and excursion with one command."
  (` (save-excursion
       (save-restriction
         (narrow-to-region (, beg) (, end))
         (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::narrow-to-paragraph 'lisp-indent-function 0)
(put 'ti::narrow-to-paragraph 'edebug-form-spec '(body))
(defmacro ti::narrow-to-paragraph (&rest body)
  "Narrow to paragraph. Point must be already inside a paragraph."
  (`
   (let* (beg)
     (when (re-search-backward "^[ \t]*$" nil t)
       (forward-line 1)
       (setq beg (point))
       (when (re-search-forward "^[ \t]*$" nil t)
         (ti::narrow-safe beg (point)
           (,@ body)))))))

;;; ----------------------------------------------------------------------
;;; Note that nconc works only if the initial
;;; list is non-empty, that's why we have to initialize it in the
;;; first time with if.
;;;
(defmacro ti::nconc (list x)
  "Add to LIST element X. Like nconc, but can also add to empty list.
Using `nconc' is faster than `append'"
  (` (setq (, list)
           (nconc (, list) (list (, x))))))

;;; ----------------------------------------------------------------------
;;;
;;; (1 2)   (cdr el) --> (2)   ,this is list
;;; (1)     (cdr el) --> nil   ,this too
;;; (1 . 2) (cdr el) --> 2     ,listp returns nil
;;;
(defsubst ti::consp (elt)
  "Test if ELT is in _really_ in format (X . X)."
  (and (consp elt)                      ;must be some '(...) form
       (null (listp  (cdr elt)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::listp (list)
  "Test if the there _really_ is elements in the LIST.
A nil is not accepted as a true list."
  (and (not (null list))
       (listp list)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::when-package 'lisp-indent-function 2)
(put 'ti::when-package 'edebug-form-spec '(body))
(defmacro ti::when-package  (feature &optional package &rest body)
  "If FEATURE is present or if PACKAGE exist along `load-path' do BODY.

  (when-package 'browse-url nil
    (autoload 'browse-url-at-mouse \"browse-url\" \"\" t))"
  (`
   (when (or (and (, feature)
                  (featurep (, feature)))
             (locate-library (or (, package)
                                 (symbol-name (, feature)))))
     (progn
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-require 'lisp-indent-function 2)
(put 'ti::with-require 'edebug-form-spec '(body))
(defmacro ti::with-require (feature &optional filename &rest body)
  "Load FEATURE from FILENAME and execute BODY if feature is present.
E.g. try loading a package and only if load succeeds, execute BODY.

  (with-feature 'browse-url nil
     ;;; Setting the variables etc)"
  (`
   (when (require (, feature) (, filename) 'noerr)
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-time-this 'lisp-indent-function 1)
(put 'ti::with-time-this 'edebug-form-spec '(body))
(defmacro ti::with-time-this (function &rest body)
  "Run FUNCTION after executing BODY and time execution.
Float time value in seconds is sent to FUNCTION.

  (ti::with-time-this '(lambda (time) (message \"Secs %f\" time))
     (sit-for 4))."
  (`
   (let* ((Time-A (current-time))
          Time-B
          Time-Diff)
     (prog1
         (progn (,@ body)))
     (setq Time-B (current-time))
     (setq Time-Diff (ti::date-time-difference Time-B Time-A 'float))
     (funcall (, function) Time-Diff))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-coding-system-raw-text 'lisp-indent-function 0)
(put 'ti::with-coding-system-raw-text 'edebug-form-spec '(body))
(defmacro ti::with-coding-system-raw-text (&rest body)
  "Bind `coding-system-for-write' to Unix style raw write during BODY."
  ;;  #todo: 'raw-text is for Emacs, is this different in XEmacs?
  (` (let* ((coding-system-for-write 'raw-text))
       (,@ body))))

;;}}}
;;{{{ small ones

;;; ----------------------------------------------------------------------
;;;   Great add to comint processess.
;;;
(defsubst ti::process-mark (&optional buffer)
  "Return process mark for current buffer or optional BUFFER.
If there is no process mark, return nil."
  (let* ((proc (get-buffer-process
                (or buffer
                    (current-buffer)))))
    (if proc
        (process-mark proc))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::verb ()
  "Setq variable 'verb'.
The variable is set If interactive flag is set or if 'verb' variable is set.
This is usually the verbosity flag that allows printing messages.

Purpose:

  The 'verb' is meant to be used in function when it decides if
  should print verbose messages. This is different that using
  simple (interactive-p) test, because (interactive-p) is only set
  if the function is really called interactively. For complete
  description why (interactive-p) est alone is not always the solution
  refer to ftp://cs.uta.fi/pub/ssjaaa/ema-code.html under heading
  that discusses about 'funtion and displaying messages'

Note:

  You have to define variable 'verb' prior calling this macro,
  preferably in function argument definition list.

Example:

  (defun my-func (arg1 arg2 &optional verb)
    (interactive
      ...do something, ask parameters)
    (ti::verb)     ;; set verbose if user calls us interactively
    (if verb
        (message 1))
    ..code
    (if verb
        (message 2)))"
  (`
   (setq verb (or verb (interactive-p)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::pmin ()
  "Go to `point-min'."
  (goto-char (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::pmax ()
  "Go to `point-max'."
  (goto-char (point-max)))

;;; ----------------------------------------------------------------------
;;;
(defmacro-maybe int-to-float (nbr)
  "Convert integer NBR to float."
  (` (read (concat (int-to-string (, nbr)) ".0"))))

;;; ----------------------------------------------------------------------
;;; see also:  (dotimes (var 5) ..
;;;
(put 'ti::dotimes 'lisp-indent-function 3)
(defmacro ti::dotimes (var beg end &rest body)
  "Loop using VAR from BEG to END and do BODY."
  (` (loop for (, var) from (, beg) to (, end)
           do
           (progn
             (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::funcall (func-sym &rest args)
  "Call FUNC-SYM with ARGS. Like funcall, but quiet byte compiler.

The full story:

  Byte Compiler isn't very smart when it comes to knowing if
  symbols exist or not. If you have following statement in your function,
  it still complaints that the function \"is not known\"

  (if (fboundp 'some-non-existing-func)
      (some-non-existing-func arg1 arg2 ...))

  instead use:

  (if (fboundp 'some-non-existing-func)
      (ti::funcall 'some-non-existing-func arg1 arg2 ...)

  to get rid of the unnecessary warning.

Warning:

  You _cannot_ use ti::funcall if the function is in autoload state, because
  `symbol-function' doesn't return a function to call. Rearrange
  code so that you do (require 'package) or (ti::autoload-p func) test before
  using ti::funcall."
  (`
   (let* ((func (, func-sym)))
     (when (fboundp (, func-sym))
       (apply func (,@ args) nil)))))
;;; Old
;;;   (apply (symbol-function (, func-sym)) (,@ args) nil)

;;; ----------------------------------------------------------------------
;;; Emacs distribution, sun-fns.el -- Jeff Peck
;;;
(defun-maybe logtest (x y)
  "Tinylibm: True if any bits set in X are also set in Y.
Just like the Common Lisp function of the same name."
  (not (zerop (logand x y))))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe bin-string-to-int (8bit-string)
  "Convert 8BIT-STRING  string to integer."
  (let* ((list  '(128 64 32 16 8 4 2 1))
         (i   0)
         (int 0))
    (while (< i 8)
      (if (not (string= "0" (substring 8bit-string i (1+ i))))
          (setq int (+ int (nth i list) )))
      (incf  i))
    int))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe int-to-bin-string (n &optional length)
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i    0)
         (len  (or length 8))
         (s    (make-string len ?0)))
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i)))
    s))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe int-to-hex-string (n &optional separator pad)
  "Convert integer N to hex string. SEPARATOR between hunks is \"\".
PAD says to padd hex string with leading zeroes."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
               (setq x (format "%X" (logand x 255)))
               (if (= 1 (length x))
                   (concat "0" x) x)))
   (list (ash n -24)
         (ash n -16)
         (ash n -8)
         n)
   separator))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe int-to-oct-string (n &optional separator)
  "Convert integer N into Octal. SEPARATOR between hunks is \"\"."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
               (setq x (format "%o" (logand x 511)))
               (if (= 1 (length x)) (concat "00" x)
                 (if (= 2 (length x)) (concat "0" x) x))))
   (list (ash n -27) (ash n -18) (ash n -9) n)
   separator))

;;; ----------------------------------------------------------------------
;;;
(defun radix (str base)
  "Convert STR according to BASE."
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz")
        (case-fold-search t)
        (n 0)
        i)
    (mapcar '(lambda (c)
               (setq i (string-match (make-string 1 c) chars))
               (if (>= (or i 65536) base)
                   (error "%c illegal in base %d" c base))
               (setq n (+ (* n base) i)))
            (append str nil))
    n))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe bin-to-int (str)
  "Convert STR into binary."
  (radix str 2))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe oct-to-int (str)
  "Convert STR into octal."
  (radix str 8))

;;; ----------------------------------------------------------------------
;;;
(defun hex-to-int (str)
  "Convert STR into hex."
  (if (string-match "\\`0x" str)
      (setq str (substring str 2)))
  (radix str 16))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe int-to-net (float)
  "Decode packed FLOAT 32 bit IP addresses."
  (format "%d.%d.%d.%d"
          (truncate (% float 256))
          (truncate (% (/ float 256.0) 256))
          (truncate (% (/ float (* 256.0 256.0)) 256))
          (truncate (% (/ float (* 256.0 256.0 256.0)) 256))))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe rmac (string)
  "Decode STRING x-mac-creator and x-mac-type numbers."
  (if (numberp string)
      (setq string (format "%X" string)))
  (let ((i 0)
        (r ""))
    (while (< i (length string))
      (setq r (concat
               r
               (make-string
                1
                ;;  EWas call to 'rhex'
                (hex-to-int (concat (make-string 1 (aref string i))
                                    (make-string 1 (aref string (1+ i)))))))
            i (+ i 2)))
    r))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe ctime (time)
  "Print a time_t TIME."
  (if (and (stringp time) (string-match "\\`[0-9]+\\'" time))
      (setq time (string-to-number (concat time ".0"))))
  (let* ((top (floor (/ time (ash 1 16))))
         ;; (bot (floor (mod time (1- (ash 1 16)))))
         (bot (floor (- time (* (ash 1 16) (float top))))))
    (current-time-string (cons top bot))))

;;; ----------------------------------------------------------------------
;;;
(defsubst rand0 (n)
  "Random number in [0 .. N]."
  (cond
   ((<= n 0)
    0)
   (t
    (abs (% (random) n)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst-maybe rand1 (n)
  "Random number [1 .. N]."
  (1+ (rand0 n)))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe randij (i j)
  "Random number [I .. J]."
  (cond
   ((< i j) (+ i (rand0 (1+ (- j i)))))
   ((= i j) i)
   ((> i j) (+ j (rand0 (1+ (- i j)))))
   (t
    (error "randij wierdness %s %s"
           (ti::string-value i)
           (ti::string-value j)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-value (x)
  "Return a string with some reasonable print-representation of X.
If X is an integer, it is interpreted as an integer rather than
a character: (ti::string-value 65) ==> \"65\" not \"A\"."
  (cond
   ((stringp x) x)
   ((symbolp x) (symbol-name x))
   ((numberp x) (int-to-string x))
   (t           (prin1-to-string x))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::prin1-mapconcat (separator &rest args)
  "Cats elements separated by single space or with SEPARATOR.
The ARGS can be anything.

Example:
   (print1cat nil buffer frame overlay list)"
  (let* ((ret  ""))
    (or separator
        (setq separator " "))

    (mapcar
     (function
      (lambda (x)
        (setq ret
              (concat
               ret

               (cond
                ((integerp x)
                 (format
                  (concat "%d" separator)
                  x))

                ((stringp x)
                 (format
                  (concat "%s" separator)
                  x))

                ((symbolp x)
                 (format
                  (concat "'%s" separator )
                  x))

                ((and (not (null x))
                      (listp x))
                 (prin1-to-string
                  (eval ;; -expression
                   (quote x))))
                (t
                 (format
                  (concat "%s" separator)
                  x)))))))
     args)
    ret))

;;; ----------------------------------------------------------------------
;;; - The world's oldest way to debug program by inserting breakpoints...
;;;
(defmacro ti::d! (&rest args)
  "Debug. Show any ARGS and wait for keypress."
  (` (save-excursion
       (save-match-data
         (read-from-minibuffer (ti::prin1-mapconcat "|" (,@ args)))))))

;;; ----------------------------------------------------------------------
;;; - This logs to buffer, when you can't display values, e.g. in loop
;;;   or while you're in minibuffer and reading input.
;;; - see tinydiff.el how to use this productively.
;;;
(defmacro ti::d!! (&rest args)
  "Stream debug. Record any information in ARGS to debug buffer.
References:
  `ti:m-debug-buffer'"
  (`
   (save-excursion
     (ti::append-to-buffer
      (get-buffer-create ti:m-debug-buffer)
      (save-match-data
        (ti::prin1-mapconcat "|" (,@ args)))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::string-left (str count)
  "Use STR and read COUNT chars from left.
If the COUNT exeeds string length or is zero, whole string is returned."
  (if (> count 0)
      (substring str 0 (min (length str) count))
    str))

;;; ----------------------------------------------------------------------
;;;  - You can do this with negative argument to substring, but if you exceed
;;;    the string len, substring will barf and quit with error.
;;;  - This one will never call 'error'.
;;;
(defsubst ti::string-right (str count)
  "Use STR and read COUNT chars from right.
If the COUNT exeeds string length or is zero, whole string is returned."
  (let* ((pos (- (length str)  count)))
    (if (> pos 0)
        (substring str (- 0 count))
      str)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::string-match-case (re str &optional case-fold start)
  "Do local case sensitive match.
Input:
  RE        See `string-match'
  STR       See `string-match'
  CASE-FOLD Value of `case-fold-search', nil means sensitive.
  START     See `string-match'"
  (let ((case-fold-search case-fold))
    (string-match re str start)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::month-list ()
  "Return LIST: month names in short format."
  (list "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::month-list-regexp (&optional cat-str)
  "Return month regexp separated by ' \\\\|' or CAT-STR.
There is intentional space, since short month name is supposed to
follow something else."
  (let* ((ret
          (mapconcat    'concat    (ti::month-list) (or cat-str " \\|"))))
    ;;  The last item must be handled separately
    (if (null cat-str)
        (concat ret " "))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::month-mm-alist ()         ;mm = month first
  "Short month names in alist form: ((\"Jan\" 1) ..)."
  '( ("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
     ("Apr" . 4) ("May" . 5) ("Jun" . 6)
     ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
     ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::month-nn-alist ()         ;nn = nbr first
  "Short month names in alist form: ((1 \"Jan\") ..)."
  '( (1 . "Jan") (2 . "Feb") (3 . "Mar")
     (4 . "Apr") (5 . "May") (6 . "Jun")
     (7 . "Jul") (8 . "Aug") (9 . "Sep")
     (10 . "Oct") (11 . "Nov") (12 . "Dec")))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::month-to-number (month &optional zero-padded)
  "Convert MONTH, 3 character initcap month name e.g. `Jan' to number."
  (let ((nbr (cdr-safe (assoc month  (ti::month-mm-alist)))))
    (if zero-padded
        (format "%02d" nbr)
      nbr)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::month-to-0number (month)
  "Convert MONTH, 3 character capitalized month name e.g. `Jan' to 01."
  (format "%02d" (cdr (assoc month  (ti::month-mm-alist)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::number-to-month (number)
  "Convert NUMBER to month, 3 character capitalized name e.g. `Jan'."
  (cdr-safe (assoc number (ti::month-nn-alist))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::date-eu-list ()
  "Return list: European date list."
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::date-us-list ()
  "Return list: US date list."
  '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::date-list-regexp (&optional cat-str)
  "Return date regexp combined with CAT-STR.
There is intentional SPACE after each date.

Input:
  CAT-STR      default is \" \\\\|\""
  (let* ((ret
          (mapconcat    'concat    (ti::date-eu-list) (or cat-str " \\|"))))
    ;;  The last item must be handled separately
    (if (null cat-str)
        (concat ret " "))))

;;; ----------------------------------------------------------------------
;;;
;;; In XEmacs20, you can't use following
;;; (memq ch '(?a ?b ?c ?d ?e ?f)), because 'eq' test against
;;; characters is wrong.
;;;
;;; Neither is this format recommended.
;;; (memq (char-int ch) (mapcar 'char-int '(?a ?b ?c ?d ?e ?f)))
;;;
;;; cl's (member* ch '(?a ?b) :test 'char=)
;;;
(defsubst ti::char-in-list-case (char list)
  "If CHAR can be found in LIST, return a pointer to it.
The match is case sensitive."
  (when char
    (let* (case-fold-search)
      (member* char list :test 'char=))))

;;; ----------------------------------------------------------------------
;;; #todo: read-char-exclusive?

(defsubst ti::read-char-safe (&optional prompt)
  "Wait for character until given and ignore all other events with PROMPT.
The `read-char' command chokes if mouse is moved while reading input.
This function returns 'ignore if the `read-char' couldn't read answer.
Otherwise it returns normal character.

Note:

  The cursor is not in the echo area when character is read. This
  may be confusing to user if you read multiple characters.

References:

  `ti::read-char-safe-until'
  `read-char-exclusive'

Return:

  ch        character
  'ignore   if read failed due to non-char event."
  (condition-case nil
      (progn
        (message (or prompt ""))        ;prevent echoing keycodes...
        (discard-input)                 ;this is a must before we read

        ;; char-int
        ;;   Emacs: this is no-op
        ;;   XEmacs19.14: char-int doesn't exist.
        ;;   XEmacs20:  read-char has changed, it does not return
        ;;          int, but a character type, and we need conversion

        (read-char))
    (error
     'ignore)))

;;; ----------------------------------------------------------------------
;;; Note: see function `read-char-exclusive' in never Emacs versions, 19.29+
;;;       Hm, It does not implement LIST of choices to accept.
;;;
(defun ti::read-char-safe-until (&optional prompt list)
  "Read character until given. Discards any events that are not characters.

Input:

  PROMPT    text displayed when asking for character
  LIST      list of character choices. The prompting won't stop until one of
            the list memebers has been selected.

Return:

  character  character type"
  (let* (ch)
    (cond
     ((null list)
      (while (symbolp (setq ch (ti::read-char-safe prompt)))))
     (list
      ;;  Check args or we're thrown on planetary ride, which never ends
      (if (or (not (ti::listp list))
              ;;   eshell-2.4.1/esh-mode.el  mistakenly defines characterp
              ;;   make sure this function is always correct.
              (prog1 nil
                (ti::compat-character-define-macro 'characterp 'integerp))
              (not (characterp (car list))))
          (error "Invalid list, must contain character in LIST %s" list))
      ;;  We don't have to do character conversion, because they are
      ;;  treated as ints
      (while (or (symbolp (setq ch (ti::read-char-safe prompt)))
                 (null ch)
                 (not (ti::char-in-list-case ch list))))))
    (message "")
    ch))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::remove-properties (string)
  "Remove properties from STRING. Modifies STRING permanently.
Return:
   string       with no properties."
  (when (stringp string)
    (set-text-properties 0 (length string) nil string)
    string))

;;; ----------------------------------------------------------------------
;;; - this is from fsf-translate-keys.el
;;;
(defmacro ti::applycar (function-form list-form)
  "Like mapcar, but does (apply FUNCTION-FORM (car LIST-FORM)).
Instead of (funcall FUNCTION (car LIST)). This is very useful for
invoking some function with many different sets of arguments.

Examples:

    (ti::applycar 'global-set-key
      '(
        ([f12]  repeat-complex-command) ; Again         L2
        ([f14]  undo)                   ; Undo          L4
        ([f16]  copy-region-as-kill)    ; Copy          L6
        ([f18]  yank)                   ; Paste         L8
        ([f20]  kill-region)))          ; Cut           L10

  -->  (nil nil nil nil nil) ;; global - set - key returns 'nil

    (ti::applycar (lambda (a b) (list b a)) ;; swaps arguments
      '((1 2)(3 4)))

  -->  ((2 1) (4 3))"
  (let ((spec-name (gensym)))
    (` (mapcar (lambda ((, spec-name))
                 (apply (, function-form) (, spec-name)) )
               (, list-form) ))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::add-command-line-arg (arg &optional func)
  "Add ARG into `command-switch-alist' if it's not already there.
This inhibits argument to be treated as filename.

Optional FUNC is called when arg is found. Default FUNC used is 'ignore."
  ;;  make sure it's not there already
  (or (assoc arg command-switch-alist)
      (setq command-switch-alist
            (cons (cons arg (or func 'ignore))
                  command-switch-alist))))

;;}}}
;;{{{ tests; small ones

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-modified-p (&optional buffer)
  "Same as `buffer-modified-p' but acceps arg BUFFER."
  (if (null buffer)
      (buffer-modified-p)
    (with-current-buffer buffer
      (buffer-modified-p))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-minibuffer-1-p ()
  "Test if current buffer is minibuffer."
  (window-minibuffer-p (selected-window)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-minibuffer-p (&optional buffer)
  "Check if BUFFER is minibuffer. Defaults to current buffer."
  (cond
   ((and buffer
         (buffer-live-p buffer))
    (with-current-buffer buffer
      (ti::buffer-minibuffer-1-p)))
   ((null buffer)
    (ti::buffer-minibuffer-1-p))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::first-line-p  ()
  "Check if cursor is at first line"
  (save-excursion
    (beginning-of-line)
    (bobp)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::last-line-p  ()
  "Check if cursor is at last line"
  (save-excursion
    (end-of-line)
    (eobp)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-narrowed-p ()
  "Check if buffer is narrowed."
  (not (eq 1 (point-min))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-empty-p (&optional buffer)
  "Check if BUFFER is empty.
Buffer is considered empty if

a) real `point-min' == `point-max'
b) or it contains only whitespace characters.

Return:

  nil           buffer contains something
  t             it is empty.
  'empty        contains only whitespace"
  (with-current-buffer (or buffer (current-buffer))
    (if (eq (point-min-marker) (point-max-marker))
        t
      (ti::pmin)
      (if (re-search-forward "[^ \n\t]" nil t)
          nil
        'empty))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::ck-maybe-activate (&optional type mode message)
  "Activate keybinding conversion if used Emacs needs it.
Call `ti::ck-advice-control' with parameter mode if key conversion needed.
This ensures that binding work in any Emacs (XEmacs and Emacs).
If you only use STRING bindings only use string notation

    (global-set-key \"\\C-c\\C-f\" ...)

then you don't need this function.

TYPE

    Informs how you have written the keybindings. The 'xemacs binding
    type is already supported by 19.33+ Emacs releases, but if you want your
    packages be backward compatible you want to call this functions prior
    bind definitions. Note: if you call this function with parameter
    'xemacs and ey definitions being bound are done in Emacs that supports
    XEmacs style bindings, this function is no-op.

                    # The Control-a binding is stylistically exploded due to
                    # checkdoc.el
                    #
    'emacs          Your bindings are like [?\\C - a] and [f10]
    'emacs-mouse    You use Emacs specific binding [mouse-1]
    'xemacs         Your bindings are like [(control ?a)] and [(f10)]
    'xemacs-mouse   You use XEmacs specific binding [(button1)]

MODE

    nil         You pass this argument bfore you start defining keys
    'disable    You pass this, when you have finished.

MESSAGE

    Message you want to display if conversion is activated.

Example:

    (ti::ck-maybe-activate 'emacs)        ;; turn conversion on in Xemacs
    (define-key [f1] 'xxx-function-call)
    <other key definitions ...>
    (ti::ck-maybe-activate 'emacs 'disable) ;; conversion off

Recommendation:

    It is recommended that you write using the 'xemacs style, which
    is also supported in later Emacs releases 19.30+. If you do so,
    then calling this function is no-op in those Emacsen that support
    XEmacs style and you save the call to tinyck.el package.

Return:

    t       conversion activated
    nil"
  (let* ((emacs-major  (ti::emacs-p))
         (common   (or (ti::xemacs-p)
                       (eq 20 emacs-major)
                       (and
                        ;; 19.34 Added XEmacs styled binding support
                        (eq 19 emacs-major)
                        (> emacs-minor-version 33)))))

    ;;  If there is mouse button bindings, then we have to use the conversion.
    ;;  Turn off "compatibility" flag between Emacs and XEmacs

    (if (memq type '(xemacs-mouse emacs-mouse))
        (setq common nil))

;;;    (eval-and-compile (ti::d! type common emacs-major message))

    (unless common
      (cond
       ((memq type '(xemacs xemacs-mouse))
        (when (ti::emacs-p)        ;XEmacs bindings and we're in Emacs
          (if message (message message))
          (ti::ck-advice-control mode)
          t))
       ((memq type '(emacs emacs-mouse))
        (when (ti::xemacs-p)       ;Emacs bindings and we're in XEmacs
          (if message (message message))
          (ti::ck-advice-control mode)
          t))
       (t
        (error "Unknown type %s" type mode))))))

;;; ----------------------------------------------------------------------
;;; See register.el::insert-register
;;;
(defsubst ti::register-live-p (char)
  "Test if register CHAR contain valid window configuration or mark."
  (let ((val (get-register char)))
    (if (or (consp val)                 ;window config
            (and (markerp val)          ;mark
                 (marker-buffer val)))  ;not killed, reverted
        t
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-dos-p ()
  "Check if there is anywhere \\r$ in the buffer."
  (save-excursion
    (ti::pmin)
    (re-search-forward "\r$" nil t)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::space-p (char)
  "Return t if character CHAR is space or tab."
  (or (char= char ?\t)
      (char= char ?\ )))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-face-p (face-symbol)
  "XEmacs ad Emacs compatibility, Check if the FACE-SYMBOL exists."
  (cond
   ((fboundp 'find-face)
    (ti::funcall 'find-face face-symbol))
   ((fboundp 'face-list)
    (memq face-symbol (ti::funcall 'face-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::color-type ()
  "Read Frame background and return `background-mode: 'dark 'light."
  ;; (frame-parameter 'display-type)
  ;; (frame-parameters (selected-frame))
  ;;  We can't read frame information when we have no visible window.
  (frame-parameter (selected-frame) 'background-mode))

;;; ----------------------------------------------------------------------
;;; Emacs 21.3+ includes this, but is it not the same as here
;;; (color-supported-p COLOR FRAME &optional BACKGROUND-P)
(defun ti::colors-supported-p ()
  "Check if colours can be used (that thay can be displayed)."
  (cond
   ((ti::emacs-p)
    (or ;; (and (fboundp 'x-display-color-p)
     ;;     (ti::funcall 'x-display-color-p))
     (ti::compat-window-system) ;; Under 21, no colors in tty
     (> emacs-major-version 20)))
   ((ti::xemacs-p)
    (or (and (fboundp 'device-class)
             ;; x-display-color-p can only be called in X, otw gives error
             (eq 'color (ti::funcall 'device-class)))
        ;; #todo:  Can I consider font-lock support for TTY as
        ;; color support? Here I assume yes.
        (> emacs-major-version 19)       ;XEmacs 20+ does tty
        (and (eq emacs-major-version 19) ;> 19.15 does too
             (> emacs-minor-version 14))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::color-lighter (color &optional percentage)
  "From base COLOR, make it integer PERCENTAGE, default 5, lighter."
  (or percentage
      (setq percentage 5))
  (let* ((components (x-color-values color))
         (new-components
          (mapcar (lambda (comp)
                    (setq comp (/ comp 256))
                    (incf comp (/ (* percentage 256) 100))
                    (when (< comp 0)
                      (setq comp 0))
                    (if (> comp 255)
                        (setq comp 255))
                    comp)
                  components)))
    (apply 'format "#%02x%02x%02x" new-components)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::overlay-supported-p ()
  "Check if overlays are supported."
  (or (ti::emacs-p)
      ;;  XEmacs has overlay emulation package, but only the 20.x
      ;;  version works right.
      (and (ti::xemacs-p "20.0" )
           (or (featurep 'overlay)
               (load "overlay" 'noerr))))) ;; will return t if load was ok

;;; ----------------------------------------------------------------------
;;;
(defun ti::idle-timer-supported-p ()
  "Check if reliable idle timers are supported."
  (and (fboundp 'run-with-idle-timer)
       (or (ti::emacs-p) ;; Idle timers work in all Emacs versions Win32/Unix
           ;;  Only work in XEmacs under 21.2+
           (ti::xemacs-p "21.2"))))

;;}}}
;;{{{ misc, matching

;;; - The functions must be here, because defsubsts must be defined
;;;   before used

(eval-and-compile

;;; ----------------------------------------------------------------------
;;; The old replace-match doesn't have support for subexpressions.
;;; 19.28: (replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING)
;;; 19.34: (replace-match NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
;;;
  (defun ti::replace-match (level &optional replace string)
    "Kill match from buffer at submatch LEVEL or replace with REPLACE.
Point sits after the replaced or killed area.

Input:

  LEVEL       Replace submatch position. 0 is full match
  REPLACE     [optional] The replce string
  STRING      [optional] If match was against string, supply the string here,
              like in (ti::replace-match 1 replace str)
Return:

  t     action taken
  nil   if match at LEVEL doesn't exist.
  str   if string was given"
    (if (null string)
        (cond
         ((match-end level)
          (delete-region (match-beginning level) (match-end level))
          ;;  I think emacs has bug, because cursor does not sit at
          ;;  match-beginning if I delete that region, instead it is off +1
          ;;  --> force it to right place
          (and replace
               (goto-char (match-beginning level))
               (insert replace))))

      (when (match-end level)           ;Handle string case
        (concat
         (substring string 0 (match-beginning level))
         (if replace replace "")
         (substring string (match-end level))))))

;;; ----------------------------------------------------------------------
;;;
  (defsubst ti::buffer-kill-control-characters ()
    "Kill all control characters from the buffer."
    (interactive)
    (save-excursion
      (ti::pmin)
      ;; Excludes tab,ff,cr,lf.
      (while (re-search-forward "[\000-\010\016-\037]+" nil t)
        (ti::replace-match 0))))

;;; ----------------------------------------------------------------------
;;;
  (defsubst ti::string-match (re level str)
    "Return RE match at LEVEL from STR. Nil if no match at level."
    (if (string-match re str)
        (match-string level str)))

;;; ----------------------------------------------------------------------
;;;
  (defsubst ti::buffer-match (re level)
    "Return string matching RE from _buffer_ at LEVEL. Use `looking-at'.
Nil if no match at level."
    (if (looking-at re)
        (match-string level)))

  ) ;; eval-and-compile

;;}}}
;;{{{ tests cont'd

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::selective-display-line-p ()
  "Check if this line is collapsed with selective display.
Note: `selective-display' variable is usually t and the line contains \\r."
  (save-excursion
    (beginning-of-line)
    (looking-at ".*\r")))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::bool-p (var)
  "Test if VAR is nil or t."
  (or (eq var nil) (eq var t)))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::print-p (ch)
  "Determines if character CH can be printed normally.
CH can be anything and this function won't choke. The \\t \\r \\n and \\f
codes are considered printable.

Return:

  t
  nil"
  (` (if (and (not (null (, ch)))       ;it must not be nil
              (or (ti::char-in-list-case (, ch) '(?\t ?\n ?\r ?\f))
                  ;;  esh-mode.el makes wrong definition of
                  ;;  `char-int'. Fix it.
                  (prog1 t
                    (ti::compat-character-define-macro 'char-int   'identity))
                  (and
                   (> (char-int (, ch)) 31)
                   (< (char-int (, ch)) 127))))
         t nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::char-case-p (char)
  "Check if character is uppercase or lowercase.

Return:
  t     uppercase
  nil   lowercase
  nbr   if character isn't in set [A-Za-z] it returns CHAR."
  (cond
   ((and (>= (char-int char)  97) (<= (char-int char) 122))
    nil)
   ((and (>= (char-int char)  65) (<= (char-int char) 90))
    t)
   (t
    char)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::nil-p (var)
  "Test if VAR is empty.
Variable with only whitespaces [ \\f\\t\\r\\n]* is considered empty too.

Example:
  (if (ti::nil-p (setq answer (read-from-minibuffer \"give dime? \")))
      (message \"No fruit juice for you then.\"))"
  (or (eq nil var)
      (and (stringp var)
           (or (string= var "")
               (not (string-match "[^ \t\f\r\n]" var))))))

;;; ----------------------------------------------------------------------
;;; #todo: XEmacs: pos-visible-in-window-p ?
(defsubst ti::window-pmin-visible-p ()
  "Check if the `point-min' is visible in current window."
  (eq (window-start) (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::window-pmax-visible-p ()
  "Check if the `point-max' is visible in current window."
  (eq (window-end) (point-max)))

;;; ----------------------------------------------------------------------
;;;  Window pmin == the area of buffer that user sees, top line
;;;
(defun ti::window-pmax-line-p ()
  "Check if cursor is on the same line as window's `point-max'."
  (let (point)
    (save-excursion
      (beginning-of-line)
      (setq point (point))
      (goto-char (window-end))
      ;;  a) if the last line DOES NOT exceed the window len; then the
      ;;     (window-end) is in next unvisible line. --> backward char
      ;;     brings it to previous line
      ;;  b) if the last line exceed the window len; then the
      ;;     (window-end) puts cursor at the last line. --> backward-char
      ;;     is no-op.
      (backward-char 1)
      (beginning-of-line)
      (eq (point) point))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::window-pmin-line-p ()
  "Check if cursor is on the same line as window's `point-min'."
  (save-excursion
    (beginning-of-line)
    ;;  The 1- is due to fact that there is NEWLINE, where C-e command
    ;;  does not ever go.
    (eq (point) (window-start))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-pmax-line-bol (&optional eol-point)
  "Return window's last line's beginnning of point or EOL-POINT."
  (save-excursion
    ;; This is past of visible window, that why we go up one line
    (goto-char (window-end))
    (backward-char 1)
    (if eol-point
        (end-of-line)
      (beginning-of-line))
    (point)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-middle-line ()
  "Computes middle line nbr in current window."
  (let* ((win-min       (count-lines (point-min) (window-start)))
         (win-max       (count-lines (point-min) (window-end)))
         (middle        (+ win-min (/ (1+ (- win-max win-min)) 2))))
    middle))

;;; ----------------------------------------------------------------------
;;; Ideas from eldoc.el
;;;
(defun ti::no-action-in-progress-p (mode)
  "Return t if there is no action currently in progress.
This means that following cases indicate that action is in progress
and it should not be interfered.

o   if cursor is in the minibuffer
o   keyboard macro is executing

Input MODE

 'timer
 This says that the function that calls us is currently run
 by an timer functin (19.34+)

 'post-command
 Same as above; but this time calling command is running in post hook.

This function is usually called from background processes that are
run by timers or post-command*hook functions when they want to print
something in the echo area."
  (and
   (not executing-kbd-macro)
   ;; Having this mode operate in an active minibuffer/echo area causes
   ;; interference with what's going on there.
   (not cursor-in-echo-area)
   ;;  Somehow this isn't quite doing what I want. If tested with C-x
   ;;  C-f open, it still goes on loading while this function should
   ;;  tell "user is in minibuffer"
   (not (eq (selected-window) (minibuffer-window)))
   ;;  This has been disabled because user may move away from the
   ;;  minibuffer but the minibuffer still stays active there.  -->
   ;;  the previous test already tells if user is really doing
   ;;  something in minibuffer
;;;   (not (minibuffer-window-active-p (minibuffer-window)))
   (sit-for 0.2)
   (cond
    ((eq mode 'timer)
     ;;  If this-command is non-nil while running via an idle
     ;;  timer, we're still in the middle of executing a command,
     ;;  e.g. a query-replace where it would be annoying to
     ;;  overwrite the echo area.
     (and (not this-command)
          (symbolp last-command)))
    ((eq mode 'post-command)
     ;;  If this-command is non-nil while running via an idle
     ;;  timer, we're still in the middle of executing a command,
     ;;  e.g. a query-replace where it would be annoying to
     ;;  overwrite the echo area.
     (and (symbolp this-command)
          (sit-for 0.3))))))

;;}}}
;;{{{ line

;;; ----------------------------------------------------------------------
;;;   Should return the same as goto-line, does it always ?
;;;
(defun ti::current-line-number (&optional pmin)
  "Return current line number from the beginning of buffer.
If ti::pmin is non-nil the `point-min' is used for starting point, this
is useful e.g. for narrowed case. Normally returns true line number.

This function counts the number of \\n chartacters, so it will
return right count even in folding/outline buffers where selective
display is used. Using command `count-lines' would return false value.

Lines are counted from 1..x"
  ;;  - always use line beginning as reference
  ;;  - The count-lines returns 0 for 1st line --> 1+
  (1+ (count-char-in-region
       (if pmin
           (point-min)
         (point-min-marker))
       (line-beginning-position)
       ?\n)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::read-current-line (&optional point)
  "Retun whole line or portion of line, starting from POINT to the eol."
  (save-excursion
    (if point
        (goto-char point))
    (buffer-substring
     (if point (point)
       (line-beginning-position))
     (line-end-position))))

;;; ----------------------------------------------------------------------
;;;
(eval-and-compile
  (defsubst ti::line-length (&optional point)
    "Length of current line. Optionally from POINT."
    (save-excursion
      (if point (goto-char point))
      (end-of-line)
      (current-column))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::line-wrap-p ()
  "Check if line wraps. ie. line is longer that current window."
  (> (ti::line-length) (nth 2 (window-edges))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::re-search-check (re &optional level start-form read)
  "Check whole buffer for regexp RE.

Input:

  RE            regexp to search
  LEVEL         which sublevel in regexp to match, default is 0
  START-FORM    form yielding starting point of search. Default is `point-min'
  READ          read the match instead of returning point

Return:

  start point of match at level.
  string
  nil)"
  (save-excursion
    (if start-form
        (goto-char (eval start-form))
      (ti::pmin))
    (when (re-search-forward re nil t)
      (if read
          (match-string (or level 0))
        (match-beginning (or level 0))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::re-search-point-list (regexp-list &optional pos-function end)
  "Return list of points that were found using REGEXP-LIST.
Input:
  REGEXP-LIST   List of regexps
  POS-FUNCTION  is used to position the point if regexp was found.
                The point used is read after POS-FUNCTION.
  END           max search point."
  (let* (list)
    (dolist (re regexp-list)
      (save-excursion
        (when (re-search-forward re end t)
          (if pos-function (funcall pos-function))
          (push (point) list))))
    list))

;;}}}

;;{{{ Special lists, assoc

;;; ----------------------------------------------------------------------
;;; Many times you want to have data structure with some KEY
;;;
(defmacro ti::assoc-append-inside (func key list add)
  "Add to the ASSOC list new ELT.
List must be in format, K = key, E = element.
  ( (K . (E E) (K . (E E)) .. )

Input:

  FUNC      'assq or 'assoc or any other to get inner list
  KEY       key
  LIST      list
  ADD       element to add

Example:

  (setq list '( (1 . (a b)) (2 . (c d))))
  (ti::assoc-append-inside 'assq 1 list 'x)

  -->
  '( (1 . (a b x)) (2 . (c d))))"
  (`
   (let* (EL-T
          LIS-T)
     (if (not (setq EL-T (funcall (, func) (, key) (, list))))
         (push (cons (, key) (list (, add))) (, list))
       (setq LIS-T (cdr EL-T))
       (push (, add) LIS-T)
       (setcdr EL-T LIS-T)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::assoc-replace-maybe-add (target-list-sym list &optional remove)
  "Set TARGET-LIST-SYM entry to LIST of pairs (STRING . CDR-ELT).
If the LIST's STRING is found, replace CDR-ELT of TARGET-LIST-SYM.
If no STRING found, add new one to the beginning of TARGET-LIST-SYM.

Input:

  TARGET-LIST-SYM   Is assoc list, e.g.
                    `auto-mode-alist' or `interpreter-mode-alist'
  LIST              Is assoc list that are used in replacing or adding.
                    Similar to target-list-sym: ((STRING . SYM) ...)
  REMOVE            Instead of adding or modifying, remove items.

Examples:

  ;; This will redefine .el and .h definitions
  ;; in `auto-mode-alist'

  (ti::assoc-replace-maybe-add
   'auto-mode-alist
   '((\"\\.el\\'\"    . lisp-mode)
     (\"\\.h\\'\"     . c++-mode)))

Return:

  nil       Nothing done
  t         Something done."
  (let* (regexp
         cdr-elt
         ret
         copy)
    ;;  1.  We try to find the regexp. This may change from emacs to emacs
    ;;  2.  If it is found (same as in previous emacs release), then change
    ;;      "in place"
    ;;  3.  Prepend new member to the list to be sure that we get the
    ;;      control over file name specification. If function is later called
    ;;      again (reloading emacs settings), then control goes to case (2)
    ;;      and we won't be prepending more cells to the list.

    (unless (ti::listp (car list))
      (error "Need LIST '( (STRING . SYM) )"))

    (cond
     (remove
      (dolist (elt (symbol-value target-list-sym))
        (setq regexp (car elt))
        (unless (assoc regexp list)
          (setq ret t)
          (push elt copy)))
      (if (and ret copy)
          (set target-list-sym (copy-alist copy))))
     (t
      (setq ret t)
      (dolist (elt list)
        ;;  The ELT is cons:  (REGEXP . CDR-ELT)
        (setq regexp (car elt)   cdr-elt (cdr elt))
        ;;  Is the regexp there already (the assoc makes the lookup)
        (cond
         ((setq elt (assoc regexp (symbol-value target-list-sym)))
          (setcdr elt cdr-elt))
         (t
          (set target-list-sym
               (cons
                (cons regexp cdr-elt)
                (symbol-value target-list-sym))))))))
    ret))

;;}}}
;;{{{ list

;;; ----------------------------------------------------------------------
;;;
(put 'ti::let-transform-nil 'edebug-form-spec '(body))
(put 'ti::let-transform-nil 'lisp-indent-function 1)
(defmacro* ti::let-transform-nil ((&rest vars) &body body)
  "Wrap list of VARS inside `let' and set all value to nil.
This macro could be used to set e.g. hook values to temporarily
nil.

  (defvar my-hook-list '(find-file-hooks write-fil-hooks))

  (defun my-test ()
    (ti::let-transform-nil my-hook-list
      ... do something, the hooks are now suppressed.
      ...))

That is efectively save as you would have written:

  (defun my-test ()
    (let (find-file-hooks
          write-fil-hooks)
      ... do something, the hooks are now suppressed.
      ...))"
  ;; If VARS is a variable, assume we wanted its value.
  ;; otherwise, we just take it as a literal list.
  ;; This means that both (ti::let-transform-nil (a b) ...)
  ;; and (ti::let-transform-nil foo ...) work (assuming foo is boundp).
  ;;
  ;; This would also work:
  ;;
  ;;    (defmacro my-let (symbols &rest body)
  ;;      `(progv ,symbols ,(make-list (length symbols) nil)
  ;;         ,@body))
  ;;
  (ignore-errors
    (setq vars (symbol-value vars)))
  `(let ,vars
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::list-make (single-or-list)
  "Converts SINGLE-OR-LIST into list.
If argument is already a list this macro is no-op."
  (if (listp single-or-list)
      single-or-list
    (list single-or-list)))

;;; ----------------------------------------------------------------------
;;; - unfortunately recursion is quite slow, but this is
;;;   exceptional example!
;;;
;;; (defun list-flatten (l)
;;;   (cond ((consp l) (append (flatten (car l)) (flatten (cdr l))))
;;;     ((null l) l)
;;;     (t (list l))))
;;;
(defun ti::list-flatten (l)
  "Flatten list L."
  (let (result stack)
    (while (or stack l)
      (if l
          (if (consp l)
              (setq stack  (cons (cdr l) stack)     l (car l))
            (setq result (cons l result)            l nil))
        (setq l     (car stack)
              stack (cdr stack))))
    (nreverse result)))

;;; ----------------------------------------------------------------------
;;; #todo : should this use prin1-to-string, before extarcting elements,
;;;         any toughts ?
;;;
(defun ti::list-join (list &optional join-str)
  "Joins string LIST with JOIN-STR, whic defaults to space."
  (let* (ret
         (ch  (or join-str " ")))
    (while list
      (setq ret (concat (or ret "") (car list)))
      (setq list (cdr list))
      (if list                          ;only if still elements
          (setq ret (concat ret ch))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::list-to-assoc-menu (list)
  "Converts string or number items in LIST into assoc menu.
Items are numbered starting from 0.

'(1 2 \"a\" \"b\")  --> '((\"1\" . 1) (\"2\" . 2) (\"a\" . 3) (\"b\" . 4))

This is useful, if you call x popup menu or completion. For example:

(completing-read \"complete number: \"
                 (ti::list-to-assoc-menu '(111 222 333 444)))"
  (let* ((i 0)
         ret)
    (dolist (elt list)
      (if (integerp elt)
          (setq elt (int-to-string elt)))
      (push (cons elt i) ret)
      (incf  i))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::list-to-cons (list)
  "Turn list to paired cons list '(1 2 3 4) --> '((1 . 2) (3 .4))."
  (let* (ret)
    (while list
      (push (cons (pop list) (pop list)) ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::list-remove-successive (list function)
  "Remove succesive same elements from LIST.

Input:

  LIST          list
  FUNCTION      accept Arg1 and Arg2 in list, should return non-nil
                if elements are the same. Arg1 and Arg2 are taken
                as 'car' in the list.

Example:

  (ti::list-remove-successive '(1 1 2 2 3) 'eq)
  --> '(1 2 3)
  (ti::list-remove-successive '(\"1\" \"1\" \"2\" \"2\" \"3\") 'string=)
  --> '(\"1\" \"2\" \"3\")"
  (let* (new-list
         prev)
    (dolist (elt list)
      (unless (funcall function prev elt)
        (setq prev elt)                 ;prev value
        (push elt new-list)))
    (nreverse new-list)))

;;}}}
;;{{{ list

;;; ----------------------------------------------------------------------
;;; This is very useful when contruction interactive calls
;;; (interactive
;;;   (ti::list-merge-elements
;;;    (region-beginning)
;;;    (region-end)
;;;    (funcall get-3-arg-list)     ;; this returns '(arg1 arg2 arg3)
;;;    ))
;;;
;;; -->  (1 100 arg1 arg2 arg3)
;;;
(defun ti::list-merge-elements (&rest args)
  "Merge single elements, ARGS, and one dimensional lists to one list.
Example:
  (ti::list-merge-elements 1 2 'some '(type here))
  -->
  '(1 2 some type here)"
  (let* (ret)
    (dolist (elt args)
      (if (ti::listp elt)
          (dolist (x elt) (push x ret))
        (push elt ret)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;; - Ever struggled with peeking the lists..?
;;; - I have, and printing the contents of auto-mode-alist into
;;;   the buffer is very easy with this.
;;; - Should be default emacs function.
;;;
(defun ti::list-print (list)
  "Insert content of LIST into current point."
  (interactive "XLisp symbol, list name: ")
  (mapcar
   (function
    (lambda (x) (insert (ti::string-value x) "\n")))
   list))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::list-to-string (list &optional separator)
  "Convert LIST into string. Optional SEPARATOR defaults to \" \".

Input:

  LIST       '(\"str\" \"str\" ...)
  separator  ' '

Return:
  str"
  (mapconcat
   (function identity)                  ;returns "as is"
   list
   (or separator " ")))

;;; ----------------------------------------------------------------------
;;; This enables you to access previous and next element easily.
;;;
(defun ti::list-elt-position (list arg &optional test-form)
  "Return position 0..x in list.

Input:

  LIST          list
  ARG           this position in list is sought
  TEST-FORM     defaults to 'equal, you can use ARG and LIST in the
                test form. Example:  '(string= (car list) arg)

Return:
  nil  ,no ARG in list"
  (let* ((i 0)
         ret)
    (while list
      (if (if test-form
              (eval test-form)
            (equal (car list) arg))
          (setq ret i  list  nil)
        (incf  i)
        (setq list (cdr list))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::list-find (table arg &optional test-function all-matches)
  "Loops through TABLE until element matching ARG is found.

Input:

  TEST-FUNCTION defaults to (string-match (caar element) arg)
                and the supposed list is assumed to be:
                '( (\"REGEXP\"  ANY_DATA)  ..)

  ALL-MATCHES   flag, if non-nil return list of matches.

You can refer to these items in the test-form

  arg           Argument as passed.
  element       current item beeing compared, also the actual element
                stored to list if match return t. Defaults to (car table)

Examples:

   (defconst my-list '((\"1\" \"a\") (\"2\" \"b\")))

   ;;  This is like using 'assoc'

   (ti::list-find my-list \"1\")
   --> (\"1\" \"a\")

   ;;  Do match against member 2

   (ti::list-find my-list \"b\" '(string-match (nth 1 element) arg))
   --> (\"2\" \"b\")

   ;;  This is little tricky, we search all '.fi' sites, and then
   ;;  remove all whitespaces around the items.

   (defconst my-list2 '(\"   foo@a.fi \" \"Bar <man@b.fi>   \" \"gee@c.uk  \"))

   (ti::list-find my-list2  \"[.]fi\"
              '(and
                 (string-match arg element)
                 (setq element (ti::string-remove-whitespace element)))
        'all-matches)

   --> (\"foo@a.fi\" \"Bar <man@b.fi>\")

Return:

  nil
  element      single element
  list         list is returned if all-items is non-nil"
  (let* (ret)
    (dolist (element table)
      (when (if test-function
                (funcall test-function arg element)
              (string-match (car element) arg))
        (if all-matches                 ;how to put results ?
            (ti::nconc ret element)
          (setq ret element)
          (return))))
    ret))

;;}}}
;;{{{ misc, window, frame, events, popup

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::non-dedicated-frame (&optional win)
  "Return some non-dedicated frame. The current frame is looked from WIN."
  (if (window-dedicated-p (selected-window))
      (car (ti::window-frame-list nil nil win))
    ;;  current frame
    (window-frame (get-buffer-window  (current-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::select-frame-non-dedicated ()
  "Move to some non dedicated frame if current frame (window) is dedicated.
E.g. you can't call `find-file', `switch-to-buffer' in dedicated frame."
  (if (window-dedicated-p (selected-window))
      (raise-frame (select-frame (car (ti::window-frame-list))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::byte-compile-defun-compiled-p (function-symbol)
  "Check if FUNCTION-SYMBOL is byte compiled."
  ;;  byte-code-function-p is marked obsolete in 19.14
  ;;  compiled-function-p is an obsolete in 19.34
  (if (ti::emacs-p)
      (` (byte-code-function-p (symbol-function (, function-symbol))))
    (` (compiled-function-p  (symbol-function (, function-symbol))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::byte-compile-defun-maybe (defun-list)
  "Byte compile `DEFUN-LIST only if not currently byte compiling.
If you have highly important functions that must be as fast as possible
no matter how the package is loaded you would do this:

  (defun function1 () ...)
  (defun function2 () ...)

  ;; At the end of file
  (ti::byte-compile-defun-maybe '(function1 function2))

Now if package is loaded in .el format, this will trigger byte compiling
those functions. If the package is currently beeing byte compiled, then
the code does nothing. Note:  loading package always causes byte compiling
the functions although they may already be byte compiled. This will not
do much harm."
  (`
   (eval-and-compile
     ;;  If not package compiltion in progress....
     ;;
     (unless (byte-compiling-files-p)
       (dolist (function (, defun-list))
         (byte-compile function) )))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::package-use-dynamic-compilation ()
  "Turn on dynamic compilation in current buffer.
Add this statement to the beginning of file:

   (eval-when-compile (ti::package-use-dynamic-compilation))"
  (`
   (progn
     (when (boundp 'byte-compile-dynamic)
       (make-local-variable 'byte-compile-dynamic)
       (defvar byte-compile-dynamic) ;; silence byte compiler
       (set 'byte-compile-dynamic t))
     (when (boundp 'byte-compile-dynamic-docstring)
       ;; In 19.34 this is t by default
       (make-local-variable 'byte-compile-dynamic-docstring)
       (defvar byte-compile-dynamic-docstring) ;; silence byte compiler
       (set 'byte-compile-dynamic-docstring t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::function-autoload-file (function)
  "Return filename where autoload FUNCTION refers to"
  (let* ((str (prin1-to-string (symbol-function function))))
    (when (and str
               (string-match "autoload[ \t\\]+\"\\([^\\\"]+\\)" str))
      (match-string 1 str))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::package-require-for-emacs (emacs xemacs &rest body)
  "EMACS and XEMACS package compatibility. Evaluate BODY.
E.g. `timer' in Emacs and 'itimer in XEmacs
Recommended usage: (eval-and-compile (ti::package-require-for-emacs ...))."
  (`
   (progn
     (if (ti::emacs-p)
         (unless (featurep (, emacs))
           (require (, emacs))
           (,@ body))
       (unless (featurep (, xemacs))
         (require (, xemacs))
         (,@ body) )))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::package-require-view ()
  "Emacs and XEmacs compatibility. Load view package."
  (`
   (if (ti::xemacs-p "20")
       (require 'view-less)
     (require 'view))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::package-package-require-timer ()
  "Emacs and XEmacs compatibility. Load view package."
  (`
   (if (ti::xemacs-p)
       (require 'itimer)
     (require 'timer))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::package-require-mail-abbrevs ()
  "Emacs and XEmacs compatibility. Load mail abbrevs package.
Recommended usage: (eval-and-compile (use-mail-abbrevs))"
  (`
   (ti::package-require-for-emacs
    'mailabbrev
    'mail-abbrevs
    (when (fboundp 'mail-abbrevs-setup) ;; Emacs
      (ti::funcall 'mail-abbrevs-setup)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::use-file-compression ()
  "Activate jka-compr.el."
  (` (cond
      ((or (featurep 'jka-compr)
           (featurep 'crypt++)))        ;That's ok then.
      ((and (featurep 'vm)
            (require 'crypt++ nil 'noerr)))
      ((featurep 'vm)
       (error "\
** Tinylibm: VM and compression was requested but no 'crypt++ feature provided.
** Tinylibm: Visit ftp://ftp.cs.umb.edu/pub/misc/.
** Tinylibm: Cannot deduce to jka-compr,
** Tinylibm: because it has been previously reported that VM is not
** Tinylibm: compatible with jka-compr. (1999-02 up till Emacs 20.3"))
      (t                                ;Last chance
       (require 'jka-compr)
       (if (fboundp 'jka-compr-install)
           (jka-compr-install)))))) ;New Emacs and XEmacs releases need this

;;; ----------------------------------------------------------------------
;;; #todo: what to do with .zip or other files?
;;;
(defun ti::use-file-compression-maybe (file)
  "Activate file compression if FILE name contains magic .gz .Z etc."
  (when (stringp file)
    (cond
     ((string-match "\\.gz$\\|\\.[Zz]$\\|\\.bz2$" file)
      (if (fboundp 'auto-compression-mode) ;; New Emacs: jka-compr.el
          (ti::funcall 'auto-compression-mode 1)
        (ti::use-file-compression))))))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::push-definition (symbol &optional func-flag)
  "Push current definition of SYMBOL to stack.
If FUNC-FLAG is non-nil, then push function definition.

Stack is at kept in property 'definition-stack"
  (if func-flag
      (push (symbol-function symbol) (get symbol 'definition-stack))
    (push (symbol-value symbol) (get symbol 'definition-stack))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::pop-definition (symbol &optional func-flag)
  "Retrieve previous definition of SYMBOL from stack.
If FUNC-FLAG is non-nil, then pop function definition.

Stack is at kept in property 'definition-stack"
  (if func-flag
      (setf (symbol-function symbol) (pop (get symbol 'definition-stack)))
    (setf (symbol-value symbol) (pop (get symbol 'definition-stack)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::use-prefix-key (map key)
  "Define to MAP a prefix KEY. If KEY is not keymap, allocate the key.
Return KEY's original binding."
  (if (not (keymapp (lookup-key map key)))
      (prog1                            ;Make it available
          (lookup-key map key)
        (define-key map key nil))))

;;; ----------------------------------------------------------------------
;;; I use this to change the BIG letter maps to `low' letter maps
;;;
(defun ti::swap-keys-if-not-keymap (sym old-key new-key)
  "In keymap SYM, swap OLD-KEY and NEW-KEY only _if_ NEW-KEY is not a keymap.

Example:

  Suppose you have Gnus map 'A' and you don't like to type
  uppercase letters. You want to change the keymap 'A' to 'a'. Here is
  the command. Notice that this executes only once, because after the
  function is called the \"a\" NEW-KEY is the keymap of 'A' now. You
  can safely use this function within hooks for that reason.

  (ti::swap-keys-if-not-keymap \"A\" \"a\")"
  (when (ti::emacs-p) ;; Keymaps in XEmacs are not lists
    (let* ((keymap  (symbol-value sym))
           (new-cdr (lookup-key keymap new-key)) ;; may be function too
           (old-cdr (lookup-key keymap old-key)))
      (when nil ;; disabled
        (ti::d!! sym
                 new-key  new-cdr (fboundp new-cdr)
                 "\n  OLD:" old-key
                 old-cdr
                 "\n  TEST"
                 (keymapp new-cdr)
                 (fboundp new-cdr)))
      (when (or (not (keymapp new-cdr)) ;Already moved
                (null new-cdr)
                (and new-cdr
                     (fboundp new-cdr)
                     (not (keymapp (symbol-function new-cdr)))))
        ;;  make the swap
        (define-key keymap new-key old-cdr)
        (define-key keymap old-key new-cdr)
        (set sym (copy-keymap keymap))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::define-buffer-local-keymap ()
  "Copy current local keymap and execute `use-local-map'.
After that your commands with `local-set-key' are buffer local."
  (use-local-map
   (copy-keymap (or (current-local-map) (make-sparse-keymap)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::define-key-if-free (map key object &optional callback)
  "Put key to map if key is not assigned already.

Key can be assigned if

o   slot is nil
o   slot has function 'ignore
o   slot has already object

Any other case generates error: the slot is already occupied.

You normally call this function from package that want's to define
e.g. function keys permanently and if there is already user definition
you can stop right there and print message.

Input:

  MAP       map where to define the key e.g. `global-map'
  KEY       key e.g. [f10]
  OBJECT    assin object to key.
  CALLBACK  on error call function CALLBACK with argument KEY and the
            result of `lookup-key'.

Example:

  (ti::define-key-if-free global-map  [f10]
    'xxx-func 'xxx-define-key-error)

  (defun xxx-define-key-error (key def)
    (error
     (format \"package xxx: key %s is already occupied with %s\"
             \"Please use manual customization.\"
             key def)))"
  (`
   (let ((def (lookup-key (, map) (, key) )))
     ;; Lookup key returns NBR if the sequence of keys exceed
     ;; the last keymap prefix
     ;; C-cck  --> C-cc  is undefined, so there is no C-c c map yet

     (if (or (eq def (, object))
             (memq def '(nil ignore))
             (integerp def))
         (define-key (, map) (, key ) (, object))
       (if (, callback)
           (funcall (, callback) (, key ) def)
         (error
          (format "Already occupied, key: %s slot content: %s "
                  (, key)
                  (prin1-to-string def))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::define-in-function-keymap (list)
  "Move key definition according to LIST '((FROM  TO) (FROM  TO) ..)
This function remap each key FROM to produce TO key instead.

Example:

  You're in terminal where tab key produces `kp-tab' and not the normal `tab'.
  You verified this by looking at the \\[view-lossage]. You want that key
  to give key code `tab' to Emacs:

  (ti::define-in-function-keymap
    '(([kp-tab]   [?\t])
      ([C-kp-tab] [C-tab])
      ([S-kp-tab] [S-tab])
      ([A-kp-tab] [A-tab])
      ([C-S-kp-tab] [C-S-tab])))

  Note: The global binging of FROM key is set to nil in order to remap
  to take effect. Do not define FROM key globally after this."
  (dolist (elt list)
    (when (and (car elt) (nth 1 elt))
      (define-key function-key-map (car elt) (nth 1 elt)) ;; Alt
      (define-key global-map (car elt) nil))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::copy-key-definition (map to-key from-key)
  "Put to MAP a TO-KEY that is bound to FROM-KEY.
You can use this function e.g. in minor modes, where when minor
mode is turned on, it moves some key definitions to somewhere
else. For example if minor mode wants to take over PgUp and PgDown
keys, but preserve their original menaing under some other key,
it could copy the function calls to sme other key like
control-PgUp and control-PgDown.

Example:

    ;;  move PgUp/Down under Control key. Preserve their original
    ;;  function that may not be simple scroll-down!

    (copy-key-function map [C-prior] [prior])
    (copy-key-function [C-next] [prior])

    ;; Now occupy  minor map definition

    (define-key [prior] 'minor-mode-function)"
  (`
   (define-key (, map) (, to-key)
     (or (and (current-local-map)
              (lookup-key (current-local-map) (, from-key)))
         (lookup-key global-map (, from-key)) ))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::beginning-of-defun-point (&optional end)
  "Search function beginning or END. Point is preserved. No errors.
Return:
 point
 nil    not found"
  (save-excursion
    (ignore-errors
      (if end
          (end-of-defun)
        (beginning-of-defun))
      (point) )))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::digit-length (arg)
  "Return number of digits in ARG which must be either number or string.
If ARG is string, the length of string is returned."
  (let ((val arg))
    (if (integerp arg)
        (setq val (int-to-string arg)))
    (length val)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::add-hook-fix ()
  "Arrange some write file hooks to correct order. Support crypt++.el"
  (let* ((crypt-w  (memq 'crypt-write-file-hook write-file-hooks)))

    (when crypt-w ;; Crypt present
      (let* ((crypt-f  (memq 'crypt-find-file-hook find-file-hooks))
             (crypt-n  (memq 'find-file-not-found-hooks
                             find-file-not-found-hooks )))
        (when (not (null (cdr crypt-w))) ;; Not in the end of the hook
          (remove-hook 'crypt-write-file-hook 'write-file-hooks)
          (add-hook    'crypt-write-file-hook 'write-file-hooks 'append))

        (when (not (null (cdr (reverse crypt-f)))) ;; Not at the beginning
          (remove-hook 'crypt-find-file-hook 'find-file-hooks)
          (add-hook    'crypt-find-file-hook 'find-file-hooks 'append))

        (when (not (null (cdr (reverse crypt-n)))) ;; Not at the beginning
          (remove-hook 'find-file-not-found-hooks 'find-file-hooks)
          (add-hook    'find-file-not-found-hooks 'find-file-hooks 'append))))))

;;; ----------------------------------------------------------------------
;;; - add-hook should accept many parameters...
;;;
(defun ti::add-hooks
  (hook-or-list single-or-list &optional remove append check)
  "Run `add-hook' to insert every element in HOOK-OR-LIST to SINGLE-OR-LIST.

Notes:

  Thic function calls `ti::add-hook-fix` if the hook in question
  is `write-file-hooks' (Crypt support)

Remember:

  `add-hook' call creates a hook variable if it doesn't exist.

Input:

  HOOK-OR-LIST  hook symbol, or list of hook symbols
  LIST          single function or list of functions
  REMOVE        flag, if non-nil run `remove-hook' instead.
  APPEND        parameter to `add-hook'
  CHECK         run ´boundp' check before trying to add to a hook.
                Only if variable exists, run `add-hook' or `remove-hook'

Example:

  ;;  Add 2 functions to 2 hooks

  (ti::add-hooks '(mode1-hook mode2-hook) '(hook1 hook2))"
  (let* ((list  (ti::list-make single-or-list))
         (hlist (ti::list-make hook-or-list)))
    (dolist (hook hlist)
      (if (eq hook 'write-file-hooks)
          ;; Arrange some write file hooks to correct order (crypt.el)
          (ti::add-hook-fix))
      (dolist (x list)
        (when (or (null check)
                  (and check
                       (boundp hook)))
          (if remove
              (remove-hook hook x)
            (add-hook hook x append)))))))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe subst-char-with-string (string &optional char to-string)
  "In STRING, converts CHAR with TO-STRING.
Default is to convert all tabs in STRING with spaces."
  (let* ((len           (length string))
         (i             0)
         elt
         ret)
    (cond
     ((not (and char to-string))
      (with-temp-buffer
        (insert string)
        (untabify (point-min) (point-max))
        (setq ret (buffer-string))))
     (t
      (while (< i len)
        (setq elt (char-to-string (aref string i)))
        (if (char= char (aref string i))
            (setq elt to-string))
        (setq ret (concat ret elt))
        (incf  i))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::prefix-arg-to-text (arg)
  "Return a string describing the current prefix argument ARG."
  (cond
   ((null     arg)    "")
   ((integerp arg)    (int-to-string arg))
   ((eq '-    arg)    "C-u - ")
   ((integerp arg)    (format "C-u %d " current-prefix-arg))
   (t
    (apply 'concat (make-list (round (log (car arg) 4)) "C-u ")))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::keep-lower-order (var1 var2)
  "Keep VAR1 < VAR2."
  (` (let ((MiN (min (, var1) (, var2)))
           (MaX (max (, var1) (, var2))))
       (setq (, var1) MiN)
       (setq (, var2) MaX))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::bool-toggle (var &optional arg)
  "Toggle VAR according to ARG like mode would do.
Usefull for for functions that use arg 0/-1 = off, 1 = on, nil = toggle.
Minor modes behave this way.

VAR is set to following values when ARG is:
  arg 0/-1  VAR -> nil
  arg nbr   VAR -> t
  arg nil   VAR -> not(var)     toggles variable"
  (`
   ;;  The `let' is mandatory. XEmacs byte compiler will not allow
   ;;  expanding the variable in numeric context. If we used
   ;;
   ;;  (and (integerp (, arg))
   ;;       (< (, arg) 1))
   ;;
   ;;  That would compile into this (when optional ARG is nil)
   ;;
   ;;  (and (integerp nil)
   ;;       (< nil 1))              ;; <= Byte compiler error
   ;;
   ;;  The message from XEmacs 21.5 would say:
   ;;  ** evaluating (< nil 1): (wrong-type-argument number-char-or-marker-p nil)
   ;;
   (let  ((toggle (, arg)))
     (setq (, var)
           (cond
            ((and (integerp toggle)
                  (< toggle 1))         ;Any negative value or 0
             nil)
            ((integerp toggle)          ;Any positive value
             t)
            ((null toggle)
             (if (null (, var))
                 t
               nil))
            (t
             nil))))))

;;}}}

;;{{{ buffers, variables

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::compat-load-user-init-file ()
  "Emacs and XEmacs compatibility."
  (cond
   ((boundp 'load-user-init-file-p)
    (intern "load-user-init-file-p"))
   ((boundp 'init-file-user)
    (intern "init-file-user"))
   (t
    (error "Unknown Emacs."))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::compat-Info-directory-list-symbol ()
  "Emacs and XEmacs compatibility. Return symbol."
  (cond
   ((boundp 'Info-directory-list) ;; XEmacs
    (intern "Info-directory-list"))
   ((boundp 'Info-default-directory-list)
    (intern "Info-default-directory-list"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::compat-Info-directory-list ()
  "Emacs and XEmacs compatibility. Return value."
  (symbol-value (ti::compat-Info-directory-list-symbol)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-pointer-of-info ()
  "Return Emacs or XEmacs *info* buffer."
  ;;  This buffer should have been defvar'd in Emacs
  (get-buffer "*info*"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-pointer-of-messages ()
  "Return Emacs or XEmacs MESSAGE buffer."
  ;;  The buffer name should be in variable and not hard coded
  ;;  Bad desing from Emacs folks...
  ;;
  ;;  The following is not used, because it's not strictly accurate:
  ;;
  ;;     (or (get-buffer "*Messages*")
  ;;          (get-buffer " *Message-Log*"))
  ;;
  ;;  An emacs type is tested because the buffer name is exactly that
  ;;
  (if (ti::emacs-p)
      (get-buffer "*Messages*")
    (get-buffer " *Message-Log*")))

;;; ----------------------------------------------------------------------
;;;
(defun ti::last-message-line ()
  "Return last line from message buffer."
  (let* ((buffer (ti::buffer-pointer-of-messages)))
    (when buffer
      (with-current-buffer buffer
        (ti::pmax)
        (re-search-backward "[^\t\n ]" nil t)
        (ti::read-current-line)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::dolist-buffer-list
  (test-form &optional temp-buf exclude-form &rest action-form)
  "Return list of buffer names matching TEST-FORM.

If optional TEMP-BUF is non-nil, every buffer is searched.
Normally following buffers are ignored.
-  Temporary buffers which start with character asterisk '*'
-  Invisible buffers which start with space ' '

Optional EXCLUDE can also be given, which excludes buffers from
matched ones.

If optional ACTION-FORM is given executes forms for every matched buffer.
At the moment of eval the `set-buffer' is already done.

Input:

  TEST-FORM     regexp or form to get matching buffers.
  TEMP-BUF      flag. Non-nil allows scanning temp buffers too
  EXCLUDE-FORM  regexp or form  -- against matched ones
  ACTION-FORM   if exist, eval this for every buffer.

Internal variables that you can refer to:

  buffer        the current buffer pointer

Return:

  list          (buffer-name buffer-name ..)

Examples:

  ;;  Get all buffers matching \"cc\"
  (ti::dolist-buffer-list \"cc\")

  ;;  Get all buffers in `dired-mode'
  (ti::dolist-buffer-list '(eq major-mode 'dired-mode))
"
  (`
   (let* (OK
          BN
          return-list)
     (dolist (buffer  (buffer-list))
       (setq BN (buffer-name buffer))
       (when (stringp BN)               ;it's killed if no name
         (with-current-buffer buffer
           (when (, test-form)
             (setq OK t)
             (when (, exclude-form)
               (setq OK nil))
             (when OK
               (if (and (null (, temp-buf))
                        (string-match "^[* ]" BN))
                   nil
                 (push BN return-list)
                 (,@ action-form)))))))
     return-list)))

;;; ----------------------------------------------------------------------
;;; Emacs erase-buffer doesn't take arguments
;;;
(defun ti::erase-buffer (&optional buffers)
  "Clear list of BUFFERS. Buffer existense is not checked."
  (setq buffers (or (ti::list-make buffers)
                    (list (current-buffer))))
  (save-current-buffer
    (dolist (elt buffers)
      (set-buffer elt)
      (erase-buffer))))

;;; ----------------------------------------------------------------------
;;; - The buffer is *not* cleared by default, only put to consistent state
;;;
(defun ti::temp-buffer (&optional buffer clear)
  "Create and reset temporary BUFFER.
Remove read-only. Buffer name is \"*tmp*\" by default.
Put buffer to `fundamental-mode' and remove any narrowing and `font-lock-mode'.
if CLEAR is non-nil, delete old buffer content.

Return:
  buffer pointer"
  (let* ((buffer
          (let (font-lock-mode   ;Handles defer-lock and fast-lock too
                lazy-lock-mode
                global-font-lock-mode)
            ;; Old Emacs doesn't have these, ByteComp silencer
            ;; This buffer doesn't need to know about font-lock.
            (if font-lock-mode (setq font-lock-mode nil))
            (if lazy-lock-mode (setq lazy-lock-mode nil))
            (if global-font-lock-mode (setq global-font-lock-mode nil))
            (get-buffer-create (or buffer "*tmp*"))))
         (sym       'font-lock-mode)
         (sym-lazy  'lazy-lock-mode))

    (with-current-buffer buffer
      (unless (eq major-mode 'fundamental-mode)
        (fundamental-mode))             ;No fancy modes here

      (setq buffer-read-only nil)

      ;;  Defconst used instead of setq due to old Emacs, where
      ;;  these variables have not been defined.
      ;;  `sym' just foold ByteCompiler again... (`set' would whine otw)

      (if (boundp sym)                  ;Exist; okay then ...
          (set sym nil))                ;Keep documentation

      (if (boundp sym-lazy)
          (set sym-lazy nil))

      ;; - This call has been commented for now, because it prints
      ;;   unecessary message every time it's beeing called.
      ;; - Besides the modified flag is not much used for "star",tmp, buffers
      ;;
      ;; (set-buffer-modified-p nil)

      ;; - We don't check the possible narrowing. Just go and widen

      (widen)
      (if clear
          (erase-buffer)))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::append-to-buffer (buffer string &optional beg-flag)
  "Append to BUFFER a STRING. If BEG-FLAG is non-nil, prepend to buffer."
  (with-current-buffer buffer
    (if beg-flag
        (ti::pmin)
      (ti::pmax))
    (insert string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::set-buffer-safe (buffer)
  "Execute `set-buffer' if BUFFER exists. Does not signal any error.
Return
  buffer pointer    if `set-buffer' executed
  nil               buffer does not exist"
  (if (buffer-live-p (get-buffer buffer))
      (set-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::kill-buffer-safe (buffer)
  "Do `kill-buffer' only if BUFFER exists. Does not signal any error.
The buffer is killed, even if modified.
Return:
  t             killed
  nil           no such buffer"
  (save-current-buffer
    (when (ti::set-buffer-safe buffer)
      (set-buffer-modified-p nil)     ;No confirmation when we kill it
      (kill-buffer buffer))))

;;}}}
;;{{{ hash table

;;; #todo: rename to `obarray' functions or get rid of these and use cl hash

;;; These are normally calld hash tables, or Emacs says they are obarrays.
;;; whatever...
;;;
;;; The idea is to store uniq ITEMS into vectors, like filenames.
;;; Then each filename can have properties, like rcs version number,
;;; locker, date of creation etc.

;;; ----------------------------------------------------------------------
;;; - just setting the hash to nil; does not kil the contents of hash.
;;;   For top security like passwords; each element must be zeroed.
;;;
(defun-maybe cl-clrhash-paranoid (hash)
  "Clear HASH by filling every item and then calling `cl-clrhash'.
This should clear memory location contents."
  (cl-maphash
   (lambda (k v)
     (fillarray v ?\0)) ;; propably faster
;;;     (loop for i from 0 to (1- (length v))
;;;           do (aset v i ?\0))
   hash)
  (cl-clrhash hash))

;;; ----------------------------------------------------------------------
;;; File: elisp,  Node: Creating Symbols
;;; - In Emacs Lisp, an obarray is actually a vector
;;; - In an empty obarray, every element is 0
;;; - lengths one less than a power of two
;;;
(defmacro ti::vector-table-init (table &optional size init-val)
  "Clears vector TABLE. Default SIZE is 128 buckets. INIT-VAL defaults to 0."
  (` (setq (, table) (make-vector (or (, size) 127) (or (, init-val) 0)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::vector-table-get (table item &optional allocate)
  "Read vector TABLE and return ITEM. ALLOCATE if ITEM does not exist."
  (` (if (, allocate)
         (intern (, item) (, table))
       (intern-soft (, item) (, table)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vector-table-property (table item prop &optional put-value force-set)
  "In vector TABLE and ITEM, get or put property PROP.

Input:

  TABLE         hash table
  ITEM          If ITEM is not allocated bucket, signal error.
  PROP          property symbol
  PUT-VALUE     value to put. If this is non-nil value is stored.
  FORCE-SET     flag, if non-nil then put anything that was in  put-value
                E.g. value nil can be stored this way."
  (let* (sym)
    (if (null (setq sym (ti::vector-table-get table item)))
        (error "No bucket found for item. [item not in table] %s" item)
      (if (or put-value force-set)
          (put sym prop put-value)
        (get sym prop)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::vector-table-clear (table)
  "Delete all values assicated to interned symbols in TABLE.
If possible, unintern all symbols."
  (` (progn
       (mapatoms
        (lambda (atom)
          (setplist atom nil)
          ;;  19.34
          (when (fboundp 'unintern)
            (ti::funcall 'unintern atom (, table))))
        (, table))
       (unless (fboundp 'unintern)      ;Old way
         (ti::vector-table-init (, table) (length (, table))))
       (, table))))

;;}}}

;;{{{ file

;;; ----------------------------------------------------------------------
;;;
(defun ti::expand-file-name-tilde-in-string (string)
  "Expand ~ referenced in string."
  ;;  #todo:  Not quite right, because XEmacs can be build under Win32/Cygwin
  ;;  and ~user would be valid.
  (unless (ti::win32-p)
    (while (string-match "\\(~[^ \n\t\\/]+\\)" string)
      (setq string
            (replace-match
             (expand-file-name (match-string 1 string))
             nil nil string))))
  string)

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-name-path-p (file)
  "Check if file looks like a pathname, which includes slash or backslash."
  (string-match "[\\/]" file))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-name-path-absolute-p (file)
  "Check if file looks like a absolute pathname."
  (or (string-match "^[a-z]:[\\/]" file)    ;; win32
      (string-match "^[/~]" file)))         ;; Unix

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-move (from to)
  "Move directory FROM TO. Relies on `mv' command. Return command results."
  (with-temp-buffer
    (let ((mv (or (executable-find "mv")
                  (error "TinyLib: `mv' command not found."))))
      (call-process mv nil (current-buffer) nil
                    (expand-file-name from)
                    (expand-file-name to)))
    (buffer-string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::write-file-with-wrapper (file)
  "Write file, possibly compressed. Crypt++ compatibility.
Bind `crypt-auto-write-buffer' to t for Crypt++."
  (let* ((crypt-auto-write-buffer t)
         (buffer (find-buffer-visiting file))
         load)
    (unless crypt-auto-write-buffer     ;Bytecomp silencer
      (setq crypt-auto-write-buffer nil))

    ;;  In XEmacs, if there is buffer visiting with the same filename,
    ;;  the user is prompted. Try to avoid it.
    ;;  If there is buffer and it is not modified, kill it
    ;;  and reload. Otherwise call normal write file.

    (when buffer
      (with-current-buffer buffer
        (if (not (buffer-modified-p))
            (setq load t)
          (pop-to-buffer buffer)
          (error "\
Tinylibm: Can't write to file. Modified buffer with the same name in Emacs."))))

    ;;  I tried to RENAME buffer-name and set buffer-file-name to
    ;;  something else, but XEmacs still thinks that the buffer
    ;;  is saved with original name and asks from user permission.
    ;;
    ;;  There is nothing left to do but kill the buffer and reload it.
    ;;  --> this unfortunately doesn't preserve markers.
    ;;  I would have wanted to use `revert-buffer' instead.
    ;;
    ;;  If someone knows how to fool XEmacs to think buffer is
    ;;  under some other name/file, let me know.

    (when load
      (kill-buffer buffer))

    (write-file file)

    (if load
        (find-file-noselect file))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::load-file-with-wrapper 'lisp-indent-function 0)
(defmacro ti::load-file-with-wrapper (file)
  "Load possibly compressed lisp file. Crypt++ support."
  (`
   (if (not (featurep 'crypt++))
       (load-file file)                 ;jka-compr handles this.
     (ti::file-eval file))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::write-file-binary-macro 'lisp-indent-function 0)
(defmacro ti::write-file-as-is-macro (&rest body)
  "Write file without any coding conversions."
  (`
   (let* ((buffer-file-coding-system 'no-conversion)) ;; #todo: XEmacs?
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (not (string-match "[\\/]\\.\\.?$" elt)))
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::directory-recursive-macro 'lisp-indent-function 1)
(put 'ti::directory-recursive-macro 'edebug-form-spec '(body))
(defmacro ti::directory-recursive-macro (directory &rest body)
  "Start from DIRECTORY and run BODY recursively in each directories.

Following variables are set during BODY:

`dir'      Directrory name
`dir-list' All directories under `dir'."
  (`
   (flet ((recurse
           (dir)
           (let* ((dir-list (ti::directory-list dir)))
             (,@ body)
             (when dir-list
               (dolist (elt dir-list)
                 (recurse elt))))))
     (recurse (, directory)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-name-remote-p (file)
  "Check if file looks like remote FILE. (ange-ftp)."
  (string-match "^[^ \t]+@[^ \t]+:" file))

;;; ----------------------------------------------------------------------
;;; (ti::file-name-backward-slashes "/cygdrive/f/test")
;;; (ti::file-name-backward-slashes "//f/test")
;;; (ti::file-name-backward-slashes "//f")
;;;
(defun ti::file-name-backward-slashes (file)
  "Convert FILE to use baskward slashes, like dos format."
  (when file
    (setq file (subst-char-in-string ?/ ?\\ file))

    ;;  handle cygwin paths as well
    ;;  //e/old-syntax             B19 and B20
    ;;  /cygdrive/e/new-syntax     V1.1+

    (while (when (string-match
                  "\\(\\([\\]cygdrive[\\]\\|[\\][\\]\\)\\([a-z]\\)\\)[\\]?.*"
                  file)
             (setq file (replace-match (concat (match-string 3 file) ":")
                                       nil nil file 1))))
    file))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-name-forward-slashes (file)
  "Convert FILE slashes to unix format."
  (if file
      (subst-char-in-string ?\\ ?/ file)))

;;; ----------------------------------------------------------------------
;;; (ti::file-name-forward-slashes-cygwin "f:/filename")
;;;
(defsubst ti::file-name-forward-slashes-cygwin (file)
  "Convert Win32 F:\\filename to /cygdrive/drive-letter/filename."
  (when file
    (setq file (ti::file-name-forward-slashes file))
    (while (when (string-match "\\(\\([a-zA-Z]\\):\\)\\([\\/].*\\)" file)
             (setq file (replace-match (concat "/cygdrive/"
                                               (downcase
                                                (match-string 2 file)))
                                       'no-alter-case
                                       nil file 1))))
    file))

;;; ----------------------------------------------------------------------
;;; The lisp primitive call isn't very descriptive. This short
;;; macro looks better in code.
;;;
(defsubst ti::file-changed-on-disk-p (&optional buffer)
  "Check if BUFFER's file has recently changed on disk."
  (not (verify-visited-file-modtime
        (or (current-buffer) buffer))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-mode-make-read-only (mode)
  "Make MODE bit user read-only."
  (logand mode 383))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-mode-make-read-only-all (mode)
  "Make MODE bit read-only to all."
  (logand mode 292))                    ;444oct

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-mode-make-writable (mode)
  "Raise MODE bit's write flag."
  (logior mode 128))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-mode-make-executable (mode)
  "Raise MODE bit's executable flag."
  (logior mode 64))                     ;oct 100

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-mode-protect (file &optional mode)
  "Set FILE modes to -rw------- or if MODE is non-nil to -r--------."
  (interactive)
  (cond
   (mode    (set-file-modes file 256))   ;; 400oct
   (t       (set-file-modes file 384)))) ;; 600oct

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-toggle-read-write (mode)
  "Toggle MODE bit's user write flag."
  (if (eq 0 (logand mode 128))          ;-r-------- , 400 oct, 256 dec
      (ti::file-mode-make-writable mode)    ;R --> W  200
    (ti::file-mode-make-read-only mode)))   ;W --> R, 577

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-owned-p (file)
  "Test if current `user-uid' [uid] owns the FILE."
  (eq (user-uid) (nth 2 (file-attributes file))))

;;; ----------------------------------------------------------------------
;;; - If you own the file, you can turn on the write flag..
;;;
(defsubst ti::file-modify-p (file)
  "Test if we can modify FILE. It must be file, not dir, owned by us."
  (and (file-exists-p file)
       (ti::file-owned-p file)))

;;; ----------------------------------------------------------------------
;;; - I do this so often that a macro is handy
;;;
(defsubst ti::file-find-file-p (file)
  "Check if FILE is loadable, like C-x C-f. Non-string args are accepted too.
The FILE is not expanded."
  (and (stringp file)
       (file-readable-p file)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-read-only-p (file)
  "Check if FILE is read only.
Only checks if there is no +w flags,other flags are not checked.

E.g. you may have permissions ---x------ which this function
reports as read-only, bcause there is no +w flags on."
  (let (modes)
    (if (not (file-exists-p file))
        (error "No file '%s'" file)
      (if (null (setq modes (file-modes file)))
          (error "File modes failed?")

        ;;  222oct is 146dec "--w--w--w" if any of these write flags
        ;;  is on, then this returns true.

        (if (eq 0 (setq modes (logand modes 146)))
            t
          nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-name-run-real-handler (caller-sym operation args)
  "You can call this function only from `file-name-handler-alist' handler.
See Info page Node: Magic File Names.

Input:

  CALLER-SYM    the caller's function symbol
  OPERATION     handler operation, see info page.
  ARGS          in &rest form"
  (let ((inhibit-file-name-handlers
         ;;  Prevent infinite loop, don't call my-handler again.
         (cons caller-sym
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

;;; ----------------------------------------------------------------------
;;; See also insert-file-contents-literally
;;;
;;; - The problem with "loading into emacs" is that all kinds of hooks
;;;   are run, e.g. folding and outline might get activated when the file is
;;;   loaded. E.g. if we do eval, it can't see the functions if they are
;;;   behind selective display.

(defun ti::find-file-literally (file &optional buffer verb)
  "Like `find-file' but disable everything which might affect loading.
No hooks are run, no other special setups.

If there existed same file, the buffer name will reflect the file name
with letters \"<2>\" or so.

Input:

  FILE          file to load
  BUFFER        optional buffer where to insert the file
  VERB          displays buffer. This is on when called interactively.

Return:

  buffer pointer"
  (interactive "fFind file: ")
  (let* ( ;;   This mode does not run any hooks.
         (default-major-mode 'fundamental-mode)
         ;;   This makes sure we truly load the file.
         ;;   If there were that file in emacs, emacs won't load it.
         (fn  (file-name-nondirectory file))
         ;;   Prohibit emacs from doing anything fancy while
         ;;   we load the file
         enable-local-eval
         ;; jka doen't use this, but crypt++ does. Prevent running mode hooks
         ;; etc.
         (find-file-hooks (if (featurep 'crypt++) '(crypt-find-file-hook)))
         tmp)
    (ti::verb)
    (or buffer
        (setq buffer (generate-new-buffer fn)))
    (if (featurep 'crypt++)
        (progn (with-current-buffer (setq tmp (find-file-noselect file))
                 (copy-to-buffer buffer (point-min) (point-max)))
               (ti::kill-buffer-safe tmp))
      (with-current-buffer buffer
        (insert-file-contents file)))
    (with-current-buffer buffer
      (if verb
          (switch-to-buffer buffer))
      (set-buffer-modified-p   nil)
      (setq buffer-file-name (expand-file-name file)))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-eval (file)
  "Like `load-file', but read FILE and eval it in temporary buffer.

The advantage over `load-file' is that physical loading also uncompresses
the file if there is proper elisp package to handle it, thus your elisp
can be in any file *form* that packages allow for loading."
  (let* (buffer)
    (setq buffer (ti::find-file-literally file))
    (with-current-buffer buffer
      (if (and (ti::xemacs-p)           ;XEmacs compatibility
               (fboundp 'eval-buffer))
          (ti::funcall 'eval-buffer)
        (ti::funcall 'eval-current-buffer)))
    (kill-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-writable-p (file-or-dir)
  "Check if FILE-OR-DIR is writable."
  (let* ((dir           (file-name-directory (expand-file-name file-or-dir)))
         (file          "#9#_%")
         (fn            (concat dir file)))
    (if (not (stringp file))
        (error "invalid arg"))
    (file-writable-p fn)))

;;; ----------------------------------------------------------------------
;;; - When removing temporary files; I don't care if they succeed or not
;;;
(defun ti::file-delete-safe (files)
  "Deletes file or list of FILES. Read only files are chmod'd to writable.
All errors are ignored."
  (let* ((list (ti::list-make files))
         mods)
    (dolist (file list)
      (ignore-errors
        (when (file-exists-p file)
          (setq mods (ti::file-mode-make-writable (file-modes file)))
          (set-file-modes file mods)
          (delete-file (car list)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::temp-directory ()
  "Return temporary directory."
  (or (getenv "TEMPDIR")
      (getenv "TMPDIR")
      (and (boundp 'temporary-file-directory) ;; Emacs var
           (let ((val (symbol-value 'temporary-file-directory)))
             (when (and (stringp val)
                        (file-directory-p val))
               val)))
      (and (file-directory-p "c:/temp")  "c:/temp")
      (and (file-directory-p "/tmp")     "/tmp")
      (and (file-directory-p "/temp")    "/temp")
      (error
       "Tinylib: Cannot suggest temporary directory. Set TEMPDIR.")))

;;; ----------------------------------------------------------------------
;;; - The buffer is *not* cleared, only put to consistent state
;;;
(defun ti::temp-file (file &optional find-temp-dir)
  "Prepare temporary FILE for use. Delete old file with the same name.
Ensure you have write permission to the file.
Aborts with error if can't prepare the conditions to use FILE.

Input:

  FILE
  FIND-TEMP-DIR     Flag, Use /tmp or system (win32) specific tmp dir
                    Any path in FILE is replaced with temp dir."
  (let (dir)
    (when find-temp-dir
      (setq dir  (ti::temp-directory))
      (setq file (ti::file-make-path dir (file-name-nondirectory file))))

    (if (file-exists-p file)
        (delete-file file)
      ;;  See if the we have permissions to dir to write this new file ?
      (if (not (file-writable-p file))
          (error "Can't write to file")))
    file))

;;; ----------------------------------------------------------------------
;;;
(defun ti::pop-to-buffer-or-window (buffer &optional point)
  "Like `pop-to-buffer' BUFFER and POINT, but find any visible window."
  (let* (win)
    (setq win (get-buffer-window buffer t))
    (if (null win)
        (pop-to-buffer buffer)
      (raise-frame   (window-frame win))
      (select-frame  (window-frame win))
      (select-window win)
      (if point
          (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::find-file-or-window (file &optional line must-exist other-win)
  "Visit FILE and LINE.
If there already is window for the file, pop to it. Otherwise
behave like `find-file'.

Input:

 FILE           filename
 LINE           line nuumber where to position point
 MUST-EXIST     Flag, if non-nil, return nil if file does not exist
                either in disk or in Emacs.
 OTHER-WIN      display in other window."
  (let* ((buffer (or (find-buffer-visiting file)
                     (get-buffer file)

                     ;; We may have mistakenly grabbed 'cd' command and
                     ;; stucked it with buffers name.
                     ;; /users/foo/*scratch*  --> *scratch*

                     (get-buffer (file-name-nondirectory file))))

         ;;  If buffer exists and is diplayed in another frame, use it.

         (win    (and buffer (get-buffer-window buffer t))))

    (unless (and buffer win)
      (when (or (file-exists-p file)
                (null must-exist))       ;Not exist, but still ok
        (ti::select-frame-non-dedicated) ;Can't do find file otherwise
        (setq buffer
              (find-file-noselect file))))

    (when buffer
      (if other-win
          (display-buffer buffer)
        (ti::pop-to-buffer-or-window buffer))
      (select-window (get-buffer-window buffer))
      (if line
          (goto-line line)))

    buffer))

;;}}}
;;{{{ mouse

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mouse-point (&optional event)
  "Return mouse's working point. Optional EVENT is a mouse click."
  (if (or mouse-yank-at-point
          (null event))
      (point)
    (if (ti::xemacs-p)
        (point)
      (ti::funcall 'posn-point (ti::funcall 'event-start event)))))

;;}}}
;;{{{ special: i-macros for interactive

;;; #todo: rethink i-macros someday. Are they necessary?

;;; You put these macros inside 'interactive'
;;;
;;; (defun test (beg end)
;;;   (interactive (tipgp-i-region-ask-macro))
;;;   ;;  code continues
;;;   )

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::i-macro-region-ask (&optional prompt)
  "Macro, usually called from 'interactive' command.
Ask to include whole buffer with PROMPT if region is not selected. If there is
no region given, signal error.

Return:
   '(beg end)"
  (cond
   ((region-active-p)
    (list (region-beginning) (region-end)))
   ((y-or-n-p
     (or
      prompt
      "Hmmm.. no region selected. Use whole buffer? "))
    (list (point-min) (point-max)))
   (t
    (error "No region."))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::i-macro-region-body 'lisp-indent-function 0)
(defmacro ti::i-macro-region-body (&rest body)
  "Macro, usually called from 'interactive' command.
Return selected region and execute BODY. Signal error if
region is not selected.

Return:
  '(beg end BODY-return-value)"
  (`
   (if (null (region-active-p))
       (error "No region selected.")
     (list
      (region-beginning)
      (region-end)
      (,@ body)))))

;;}}}
;;{{{ FORMS: special

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-unix-shell-environment 'lisp-indent-function 0)
(put 'ti::with-unix-shell-environment 'edebug-form-spec '(body))
(defmacro ti::with-unix-shell-environment  (&rest body)
  "Run BODY in Unix like shell. In Win32, this means using Cygwin.
This form does not guarrantee the environment if there isn't none.

Variable `shell-file-name' is bound locally to new value."
  (`
   (let ((shell-file-name shell-file-name))
     ;;  In cygwin, programs like zgrep and egrep are
     ;;  shell scripts, which cannot be called (they should be .exe)
     ;;  in Win32, when cmdproxy.exe is used.
     ;;
     ;;  Try to change the context if user has Cygwin.
     (when (ti::win32-p)
       (let ((cygwin (ti::win32-cygwin-p)))
         (setq shell-file-name (format "%s/bin/bash.exe" cygwin))))
     (,@ body))))

;;; ----------------------------------------------------------------------
;;; so that I can keep the URL link in one place.
;;;
(put 'ti::package-defgroup-tiny 'lisp-indent-function 3)
(defmacro ti::package-defgroup-tiny (symbol prefix group &optional doc)
  "Define defcustom.el group for tiny* files.

Input:

  SYMBOL    The package's defgroup name
  PREFIX    Package's variable prefix
  GROUP     The upper level custom group where SYMBOL belong
            (e.g. extenstions).
  DOC       Group documentation string."
  (`
   (defgroup (, symbol) nil
     (, doc)

     ;; You could also use (url-link "mailto:foo.bar@example.com")

     :link '(url-link :tag "Update site"
                      "http://tiny-tools.sourceforge.net")
     :prefix (symbol-name (quote (, prefix)))
     :group  (quote (, group))

     ;;  Now define custom contact function when you click link

     :link '(link
             :tag "Contact maintainer"
             :func-args (list
                         (symbol-name (quote (, prefix)))
                         (symbol-name (quote (, symbol))))
             :action    ti::package-tiny-defgroup-mail))))

;;; ----------------------------------------------------------------------
;;; This would actually belong to ti::package-defgroup-tiny
;;;
;;; The following autoload tells that function exists (used in function)
;;;
(eval-when-compile
  ;;  For some reason Emacs 19.30 doesn't see :func-args
  ;;  as class parameter if compiled without custom? Hm. Any ideas,
  ;;  how to tell that it is not a variable?
  ;;
  (when (and (not (fboundp 'widget-get))
             (and (ti::emacs-p)
                  (eq emacs-minor-version  30)))
    (message "\n\
tinylibm.el: ** ignore following byte compiler message if you see it
             ** 'reference to free variable :func-args'")))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-tiny-defgroup-mail (widget &rest ignore)
  "Called from defcustom/defgroup with WIDGET and IGNORE rest args.
Send mail to tiny* package maintainer. Read keyword :func-args
which should hold elements

  '(list PACKAGE-PREFIX PACKAGE-NAME)  ;; nth 0 \"list\" is ignored.

The PACKAGE-PREFIX is in format \"xxx-:\" where a contact function
name `PACKAGE-PREFIX-submit-bug-report' is derived."

  ;; Due to ti::funcall, functions must not be in autoload state.

  (require 'wid-edit)

  (let* ((args (ti::funcall 'widget-get widget ':func-args)) ;; #TODO
         (arg1 (eval (nth 1 args)))
         (arg2 (nth 2 args))

         ;;  from variable pfx "tipgp-:" --> to function prefix "tipgp-"
         (pfx   (substring arg1 0 (1- (length arg1))))
         (func  (concat pfx "submit-bug-report"))
         sym)
    (if (setq sym (intern-soft func))
        (call-interactively sym)
      (message "Can't find contact function %s. Load %s.el first."
               func (concat (downcase arg2) ".el"))
      nil)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::grep-output-parse-macro 'lisp-indent-function 1)
(put 'ti::grep-output-parse-macro 'edebug-form-spec '(body))
(defmacro ti::grep-output-parse-macro (buffer &rest body)
  "In current buffer, run BODY for every 'grep' line.
Point is set to point-min. The BODY must not change BUFFER's point.

Following variables are bound during loop (lowercase variable names):

   cd GREP-DIR
   GREP-FILE:GREP-LINE:GREP-DATA

This means that you can say this in BODY.

   (setq absolute (concat grep-dir grep-file))"
  (` (with-current-buffer (, buffer)
       (save-excursion
         (ti::pmin)
         (let ((grep-dir (and (looking-at "^cd +\\(.*\\)")
                              (match-string 1)))
               grep-file
               grep-line
               grep-data)
           (while (re-search-forward
                   "^\\([^:\r\n]+\\):\\([0-9]+\\):\\(.*\\)" nil t)
             (setq grep-file (match-string 1)
                   grep-line (match-string 2)
                   grep-data (match-string 3))

             (when grep-line
               (setq grep-line (string-to-int grep-line)))

             (beginning-of-line)
             ;;  skip over
             ;;
             ;;   cd /usr/lib/perl5/5.6.1/pods/
             ;;   grep finished (matches found) at Tue Jul 23 17:39:21
             ;;
             (unless (looking-at "^cd \\|^[^ \t\n\r]+ +finished")
               (,@ body))
             (forward-line 1)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::occur-macro 'lisp-indent-function 2)
(put 'ti::occur-macro 'edebug-form-spec '(body))
(defmacro ti::occur-macro (re &optional hook &rest body)
  "Run occur with RE starting from `point-min' and call HOOK after BODY.

Execute BODY after occur statement in occur buffer.
Run HOOK in occur buffer last; this arg can also be nil if there is no hook."
  (`
   (progn
     (save-excursion                    ;save user's active point
       (ti::pmin)
       (occur (, re)))
     (pop-to-buffer "*Occur*")
     (,@ body)
     (ti::pmin)
     (if (, hook)
         (run-hooks (quote (, hook)))))))

;;; ----------------------------------------------------------------------
;;;
(defun-maybe shell-command-to-string (command)
  "Returns shell COMMAND's ouput as string. Tinylibm."
  (with-temp-buffer
    (shell-command command (current-buffer))
    (buffer-string)))

;;; ----------------------------------------------------------------------
;;; #todo: should use help-mode ?
;;;
(put 'ti::momentary-output-macro 'lisp-indent-function 3)
(put 'ti::momentary-output-macro 'edebug-form-spec '(body))
(defmacro ti::momentary-output-macro
  (buffer &optional echo-msg win1 &rest body)
  "Momentarily execute body in buffer.
You normally use this to display messages to user.
Buffer is buried after this form finishes.

The output is accomplished using `with-output-to-temp-buffer', so
you have to use 'princ' to write output.

Input:

  BUFFER        string
  ECHO-MSG      displayed at echo area. If nil, default message is used.
  WIN1          flag, if non-nil, occupie full window
  BODY          rest of the Lisp code.

Example:

    (ti::momentary-output-macro
        \"*notes*\" \"howdy! Press some key\" nil
      (princ \"This is the message\"))"
  (`
   (save-excursion
     (save-window-excursion
       (with-output-to-temp-buffer (, buffer)
         (,@ body))
       (select-window  (get-buffer-window (, buffer)))
       (if (, win1)
           (delete-other-windows (get-buffer-window (, buffer))))
       (ti::read-char-safe-until
        (or (, echo-msg) "Press something to delete window."))
       (bury-buffer (, buffer))))))

;;; ----------------------------------------------------------------------
;;; - Sometimes you just want to switch buffer temporarily and
;;;   set point to somewhere else, like scroll a buffer
;;;
(put 'ti::save-excursion-macro 'lisp-indent-function 0)
(put 'ti::save-excursion-macro 'edebug-form-spec '(body))
(defmacro ti::save-excursion-macro (&rest body)
  "Like `save-excursion` BODY, but return to original window.
No other values are preserved. Also the `select-window'
is executed if the original buffer had `window-live-p'. (ie. it was visible)

Use this if you want to e.g. scroll some buffer."
  (`
   (let* ((oRig-Buf (current-buffer))
          (oRig-Win (get-buffer-window oRig-Buf)))
     (prog1
         (progn
           (,@ body))
       (set-buffer oRig-Buf)                    ;restore buffer.
       (when (and (windowp oRig-Win)            ;no window visible
                  (window-live-p oRig-Win))
         ;; and the visible window
         (select-window oRig-Win))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::save-with-marker-macro 'lisp-indent-function 0)
(put 'ti::save-with-marker-macro 'edebug-form-spec '(body))
(defmacro ti::save-with-marker-macro (&rest body)
  "Save the line position by using the marker and execute BODY.
Marker is assigned to current position. Caution: If you delete text where the
marker is, there is no way to set the previous point. In this case the
results are undefined.

Notes:

  Make sure you don't insert to immediate marker position, because
  markers moves along with the text!"
  (`
   (let* ((MarK (point-marker)))
     (prog1
         (progn (,@ body))
       (when (marker-position MarK)
         (goto-char (marker-position MarK)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::save-line-column-macro 'lisp-indent-function 2)
(put 'ti::save-line-column-macro 'edebug-form-spec '(body))
(defmacro ti::save-line-column-macro (fail-form col-form &rest body)
  "Save line and column position.
When you kill/add lines from buffer, you cannot normally save the current
point with `save-excursion', since the point no longer is the
same spot or it may be have been deleted.

This macro saves the position by remembering line and column position.

Call:

  (fail-form col-form &rest body)

Error conditions:

  If the line position cannot be preserved, Then FAIL-FORM is evaled: it can
  put the cursor at desired place.

  If column position cannot be preserved COL-FORM is evaled.

Example:

  ;;  1st and 2nd forms act like no-ops after erase buffer command
  (ti::save-line-column-macro nil nil (erase-buffer))

  ;;  1st: Put cursor at the be.g. of buffer when failure.
  ;;  2nd: If col is missed, put cursor at be.g. of line
  ;;  3rd: The form executed is all the rest of the lines

  (ti::save-line-column-macro
   (goto-char (point-min))
   (beginning-of-line)
   (flush-lines \"*\\.txt\"))

Return:

  Last value returned by BODY"
  (` (let* ((SLC-sLc-col  (current-column)) ;prevent variable suicide
            (SLC-sLc-line (ti::current-line-number)))
       (prog1
           (progn (,@ body))
         (goto-line SLC-sLc-line)
         (move-to-column  SLC-sLc-col)
         (cond
          ((not (eq (ti::current-line-number) SLC-sLc-line))
           (, fail-form))
          ((not (eq (current-column) SLC-sLc-col))
           (, col-form) ))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::widen-safe 'lisp-indent-function 0)
(put 'ti::widen-safe 'edebug-form-spec '(body))
(defmacro ti::widen-safe (&rest body)
  "(&rest body) Widen buffer end execute BODY.
Preserves possible narrowing when done.

The BODY is not protected against errors or surrounded by `save-excursion'

Return:

  last value of BODY"
  (` (let ((BeG         (point-min-marker))
           (EnD         (point-max-marker))
           (EnD-max     (point-max))
           EnD-wmax
           ReT)
       (unwind-protect
           (progn
             (widen)
             (setq EnD-wmax (point-max))
             (setq ReT (progn (,@ body))))
         (with-current-buffer (marker-buffer BeG)
           ;; what about after widen ? Were we in narrow mode ?
           (if (not (= EnD-wmax EnD-max))
               (narrow-to-region BeG EnD))

           (if (null ReT)       ;no-op, Silence XEmacs 19.14 ByteComp.
               (setq ReT nil))

           ReT)))))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(eval-and-compile
  (defun ti::package-config-file-directory-default ()
    "Determine default configuration file directory.
The preferred locations are ~/elisp/config ~/lisp/config
~/elisp ~/lisp ~/tmp and last ~/.

In XEmacs ~/.xemacs/config is preferred first."
    (dolist (dir (list
                  (if (ti::xemacs-p)
                      "~/.xeamcs/config"
                    nil)
                  "~/.emacs.d/config"
                  "~/elisp/config"
                  "~/lisp/config"
                  "~/tmp"
                  "~"
                  ;;   Last resort if this is Win32 Emacs and
                  ;;   HOME is not set ("~" did not expand)
                  "/cygdrive/c"
                  "c:/"))
      (when (and (stringp dir)
                 (file-directory-p dir))
        (return dir)))))

(defvar tinylib-:package-config-file-directory
  (ti::package-config-file-directory-default)
  "*Directory where to save configuration files.")

(defvar tinylib-:package-config-file-prefix "emacs-config-"
  "*Prefix to add to configuration files. Default 'emacs-config-'.")

(defun ti::package-config-file-prefix (&optional file &optional os emacs)
  "Return directory and prefix with config FILE optionally for OS and EMACS

The default value is currenly combination of
`tinylib-:package-config-file-directory' and
`tinylib-:package-config-file-prefix'

In packages, when defining a config file location, it is usually wanted
that all packages save configuration files to the same location, so that it
it not needed to configure each packages' files manually. The following
code shows how package can define the configuration files in a bad and good
manner:

  ;; Bad name. Traditional dot-something in User's root (HOME)

  (defvar xxx-config-file  \"~/.something\")

  ;; A much better way

  (defvar xxx-config-file  (package-config-file-prefix \".something\"))

Input:

  Sometimes the configuration file needs operating system
  version (OS) and Emacs version. Supply non-nil (t) values for these if you
  need exactly a specific file for Win32/Unix and for XEmacs/Emacs."
  (when tinylib-:package-config-file-directory
    (unless (file-exists-p tinylib-:package-config-file-directory)
      (error "`tinylib-:package-config-file-directory' %s does not exist."
             tinylib-:package-config-file-directory))
    (format "%s%s%s%s%s"
            (file-name-as-directory tinylib-:package-config-file-directory)
            tinylib-:package-config-file-prefix
            (if os
                (if (ti::win32-p)
                    "win32-"
                  "unix-")
              "")
            (if emacs
                (format "%s-%s-"
                        (if (ti::emacs-p) "emacs" "xemacs")
                        (ti::emacs-version-number-as-string))
              "")
            (or file ""))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::overlay-require-macro 'lisp-indent-function 0)
(put 'ti::overlay-require-macro 'edebug-form-spec '(body))
(defmacro ti::overlay-require-macro (&rest body)
  "Try to load overlay support or run BODY.
Overlays are Emacs thingies, XEmacs uses extents. In XEmacs
the overlay support is tested by loading package overlay.el and if it
fails, then BODY is run.

Example usage:

  (eval-and-compile
    (ti::overlay-require-macro
      (message \"*** package.el: Your Emacs doesn't have overlay support.\")
      (error \"Compilation aborted.\")))"
  (` (progn
       (when (and (ti::xemacs-p)
                  ;;  No overlay functions?.
                  (not (fboundp 'overlays-at)))
         (load "overlay" 'noerr)) ;; has no provide statement
       (or (fboundp 'overlays-at) ;; Did it define this function?
           (progn
             (,@ body))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::pp-variable-list (list &optional buffer def-token)
  "Print LIST of variables to BUFFER. DEF-TOKEN defaults to `defconst'."
  (let* (val)

    (or buffer
        (setq buffer (current-buffer)))

    (or def-token
        (setq def-token "defconst"))

    (dolist (sym list)
      (unless (symbolp sym)
        (error "List member is not symbol %s" sym))
      (setq val (symbol-value sym))
      (insert (format "\n\n(%s %s\n" def-token (symbol-name sym)))
      (cond
       ((numberp val)
        (insert val))
       ((stringp val)
        (insert (format "\"%s\"" val)))
       ((ti::bool-p val)
        (insert (symbol-name val)))
       ((and (symbolp val)
             (fboundp val))
        (insert "(function " (symbol-name val) ")"))
       ((symbolp val)
        (insert "'" (symbol-name val)))
       ((listp
         (insert "'" (pp val))))
       (t
        (error "unknown content of stream" sym val)))
      (insert ")"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::write-file-variable-state (file desc list &optional fast-save bup)
  "Save package state to FILE.

Input:

  FILE      filename
  DESC      One line description string for the file.
  LIST      List of variable symbols whose content to save to FILE.

  FAST-SAVE The default `pp' function used to stream out the contents
            of the listp variables is extremely slow if your variables
            contain lot of data. This flag instructs to use alternative,
            much faster, but not pretty on output, method.

  BUP       If non-nil, allow making backup. The default is no backup."
  (with-temp-buffer
    (let ((backup-inhibited (if bup nil t)))
      (insert ";; @(#) " file " -- " desc "\n"
              ";; Date: "
              (ti::date-standard-date 'short)
              "\n\n")
      (if (not fast-save)
          (ti::pp-variable-list list)
        (dolist (var list)
          (insert (format "\n\n(defconst %s\n" (symbol-name var)))
          ;;  While `pp' would have nicely formatted the value, It's
          ;;  unbearable SLOW for 3000 file cache list.
          ;;  `prin1-to-string' is 10 times faster.
          (insert "'" (prin1-to-string (symbol-value var)) ")\n")))
      (insert (format "\n\n;; end of %s\n" file))
      ;;  prohibit Crypt++ from asking confirmation
      (ti::write-file-with-wrapper file))))

;;}}}

(provide 'tinylibm)

;;; tinylibm.el ends here
