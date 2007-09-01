;;; tinylibb.el --- Library of (b)ackward compatible functions.
;; $Id: tinylibb.el,v 2.73 2007/05/01 17:20:45 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1998-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylibb-version.
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

;; ........................................................ &t-install ...
;; DO NOT LOAD THIS FILE, but load the central library "m". It loads this
;; file and autoload library "a"
;;
;;      (require 'tinylibm)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;      This is lisp function library, package itself does nothing.
;;      This library defines new [X]Emacs release functions for older
;;      [X]Emacs releases.
;;
;;  Usage
;;
;;      You must not autoload this package; but always include
;;
;;          (require 'tinylibm)
;;
;;      Yes, there is no typo, you load "m" lib. It will handle arranging
;;      everything for you. This library is included by "m" library
;;      automatically. Repeat: you DO NOT PUT any of these in your
;;      packages:
;;
;;          (require 'tinylib)
;;          (require 'tinyliba)
;;          (require 'tinylibb)
;;          (require 'tinylibo)
;;          (require 'tinyliby)
;;
;;      A single statement will arrange everything:
;;
;;          (require 'tinylibm)
;;
;;  Notes
;;
;;      2000-09-12 <ttn@revel.glug.org> in gnu.emacs.sources
;;      http://www.glug.org/people/ttn/software/ttn-pers-elisp/ reported that:
;;      New file core/veneration.el allows GNU Emacs 19 support.
;;      In this file some functions are available
;;      in GNU Emacs 20, but not in GNU Emacs 19: `compose-mail' and
;;      minimal supporting functions (see mail-n-news/compose-mail.el),
;;      `shell-command-to-string', and `char-before'. We also redefine
;;      `match-data' to handle arguments.
;;
;;      1998-10 SEMI's poe*el libraries also emulate various Emacs
;;      versions.

;;}}}

;;; Change Log:

;;; Code:

;;; .......................................................... provide ...

(require 'tinyliba)
(provide 'tinylibb)

;;{{{ code: Emacs compatibility, aliases, byteCompiler

(eval-and-compile
  (defvar temporary-file-directory)
  (autoload 'ti::replace-match "tinylibm"))

;;; ....................................................... &emulation ...

(defun-maybe force-mode-line-update  ()
  ;; XEmacs, labels this obsolete
  ;; In older Emacs it does not exist
  (set-buffer-modified-p (buffer-modified-p)))

(defun-maybe eval-after-load (arg1 form) ;; XEmacs 19.14 doesn't have this
  ;;  "A simple emulation. Eval FORM immediately."
  (load arg1)
  (eval form))

;; Some XEmacs doesn't have 'buffer-flush-undo
(defalias-maybe 'buffer-disable-undo 'buffer-flush-undo)

(defalias-maybe 'number-to-string 'int-to-string)

(defalias-maybe 'set-text-properties 'ignore)

(defalias-maybe 'string-to-number 'string-to-int)

;; Doesn't exist in Emacs
(defalias-maybe 'read-directory-name 'read-file-name)

(and (fboundp 'insert-file-contents-literally)
     ;;  Emacs includes `insert-file-literally'.
     (defalias-maybe 'insert-file-literally 'insert-file-contents-literally))

(defun-maybe make-local-hook (hook) ;; Exists in 19.30+
  ;;  "Make HOOK local to buffer."
  ;; - I need locals so many times it make sme cry, e.g. post-command-hook
  ;; - And why doesn't the add-hook accepts list by default ??
  ;;
  ;; - This aapplies to 19.29.1 and newer
  ;;       (add-hook HOOK FUNCTION &optional APPEND LOCAL)
  ;;       Do not use `make-local-variable' to make a hook
  ;;       variable buffer-local.  Use `make-local-hook'
  ;;       instead.
  ;;
  ;; the variable may be local already, but we do not do
  ;; any checkings
  (make-local-variable hook)
  ;; Copy this because add-hook modifies the list structure.
  (set hook (copy-sequence (eval hook))))

(defun-maybe find-buffer-visiting (file) ;not in XEmacs 19.14
  ;;  "Find buffer for FILE."
  ;;   file-truename  dies if there is no directory part in the name
  ;;   Check it first
  (or (and (string-match "^/" file)
           (get-file-buffer (file-truename file)))
      (get-file-buffer file)))

(defun-maybe backward-line (&optional arg)
  (forward-line (if (integerp arg)
                    (- 0 arg)
                  -1)))

(defun-maybe abs (x)
  ;;  "Absolute value of X."
  (if (< x 0)
      (- x)
    x))

(defun-maybe int-to-float (nbr)
  "Convert integer NBR to float."
  (read (concat (int-to-string nbr) ".0")))

(defun-maybe logtest (x y)
  "Tinylibm: True if any bits set in X are also set in Y.
Just like the Common Lisp function of the same name."
  (not (zerop (logand x y))))

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

(defun-maybe bin-to-int (str)
  "Convert STR into binary."
  (radix str 2))

(defun-maybe oct-to-int (str)
  "Convert STR into octal."
  (radix str 8))

(defun hex-to-int (str)
  "Convert STR into hex."
  (if (string-match "\\`0x" str)
      (setq str (substring str 2)))
  (radix str 16))

(defun-maybe int-to-net (float)
  "Decode packed FLOAT 32 bit IP addresses."
  (format "%d.%d.%d.%d"
          (truncate (% float 256))
          (truncate (% (/ float 256.0) 256))
          (truncate (% (/ float (* 256.0 256.0)) 256))
          (truncate (% (/ float (* 256.0 256.0 256.0)) 256))))

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

(defun-maybe ctime (time)
  "Print a time_t TIME."
  (if (and (stringp time) (string-match "\\`[0-9]+\\'" time))
      (setq time (string-to-number (concat time ".0"))))
  (let* ((top (floor (/ time (ash 1 16))))
         ;; (bot (floor (mod time (1- (ash 1 16)))))
         (bot (floor (- time (* (ash 1 16) (float top))))))
    (current-time-string (cons top bot))))

(defsubst rand0 (n)
  "Random number in [0 .. N]."
  (cond
   ((<= n 0)
    0)
   (t
    (abs (% (random) n)))))

(defsubst-maybe rand1 (n)
  "Random number [1 .. N]."
  (1+ (rand0 n)))

(defun-maybe randij (i j)
  "Random number [I .. J]."
  (cond
   ((< i j) (+ i (rand0 (1+ (- j i)))))
   ((= i j) i)
   ((> i j) (+ j (rand0 (1+ (- i j)))))
   (t       (error "randij wierdness %s %s"
                   (ti::string-value i)
                   (ti::string-value j)))))

;;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...  split  ..

(unless (fboundp 'split-sting)
  (eval-and-compile
    (defun ti::split-string (string &optional regexp level cont-level)
      "Do not use this function. Call `split-string' instead.
This function exists, because current Emacs did not define `split-string' and
there is now alias which emulates the new Emacs behavior.

If called with only STRING, then split on white space.

Input:

  STRING
  REGEXP        The delimiter in string, Default is '[\\f\\t\\n\\r\\v]+'
  LEVEL         The sub match in REGEXP to end reading substring.
                Default is 0
  CONT-LEVEL    The sub match end to continue reading the STRING.
                Default is 0 (REGEXP match's end point)

Example:

  (split-string \"-I/dir1 -I/dir2\" \" *-I\")
  --> '(\"/dir1\" \"/dir2\")"
      (let ((start 0)
            str
            ret)
        (or regexp
            (setq regexp "[ \f\t\n\r\v]+"))
        (or level
            (setq level 0))
        (or cont-level
            (setq cont-level 0))

        ;;  If no match, return as is '(string)

        (if (null (string-match regexp string ))
            (setq ret (list string))
          (while (string-match regexp string start)
            (setq str (substring string start (match-beginning level)))
            (setq start (match-end cont-level))
            ;; Ignore BOL matches. There is no string for us.
            (if (> (match-beginning level) 0)
                (push str ret)))
          ;;  Try with " test" --> '("test")
          (if (and (> start 0)
                   (< start (length string)))
              (push (substring string start) ret)))
        (nreverse ret)))))

(defun-maybe split-string (string &optional separators)
  ;; (split-string STRING &optional SEPARATORS)
  ;; in XEmacs 19.14 subr.el
  ;;  "Split string on whitespace."
  (ti::split-string string separators))

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. higher Emacs . .
;;:  Features found from new emacs only 20.xx

;; In simple.el, old Emacs does not have this.
(and (fboundp 'delete-indentation)
     (defalias-maybe 'join-lines 'delete-indentation))

(defun-maybe replace-char-in-string (ch1 ch2 string)
  ;;  "Search CH1, change it with CH2 in STRING."
  (nsubstitute ch1 ch2 string))

(defun-maybe string-prefix-p (s1 s2)
  ;;  "True if string S1 is a prefix of S2 (i.e. S2 starts with S1)"
  (equal 0 (string-match (regexp-quote s1) s2)))

(put 'with-temp-buffer 'lisp-indent-function 0)
(put 'with-temp-buffer 'edebug-form-spec '(body))
(defmacro-maybe with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (`
     (let (((, temp-buffer)
            (get-buffer-create (generate-new-buffer-name " *temp*"))))
       (unwind-protect
           (save-excursion
             (set-buffer (, temp-buffer))
             (,@ forms))
         (and (buffer-name (, temp-buffer))
              (kill-buffer (, temp-buffer))) )))))

(defun-maybe byte-compiling-files-p ()
  "Return t if currently byte-compiling files."
  (string= (buffer-name) " *Compiler Input*"))

;; #todo: This already exists in some XEmacs

(put 'with-output-to-string 'edebug-form-spec '(body))
(defmacro-maybe with-output-to-string (&rest body) ;XEmacs has this
  "Please use `shell-command-to-string'. Execute BODY and return string."
  (`
   (save-current-buffer
     (set-buffer (get-buffer-create " *string-output*"))
     (setq buffer-read-only nil)
     (buffer-disable-undo (current-buffer))
     (erase-buffer)
     (let ((standard-output (current-buffer)))
       (,@ body))
     (buffer-string))))

;;; ----------------------------------------------------------------------
;;;
(unless (fboundp 'with-buffer-unmodified)
  ;;  Appeared in Emacs 21.2
  (put 'with-buffer-modified 'lisp-indent-function 0)
  (put 'with-buffer-modified 'edebug-form-spec '(body))
  (defmacro with-buffer-modified (&rest body)
    "This FORM saves modified state during execution of body.
Suppose buffer is _not_ modified when you do something in the BODY,
e.g. set face properties: changing face also signifies
to Emacs that buffer has been modified. But the result is that when
BODY finishes; the original buffer modified state is restored.

This form will also make the buffer writable for the execution of body,
but at the end of form it will restore the possible read-only state as
seen my `buffer-read-only'

\(with-buffer-modified
   (set-text-properties 1 10 '(face highlight)))

"
    (` (let* ((Buffer-Modified (buffer-modified-p))
              (Buffer-Read-Only buffer-read-only))
         (prog1
             (progn
               (setq buffer-read-only nil)
               (,@ body)))
         (if Buffer-Modified
             (set-buffer-modified-p t)
           (set-buffer-modified-p nil))
         (if Buffer-Read-Only
             (setq buffer-read-only t)
           (setq buffer-read-only nil))))))

;; `save-excursion' is expensive; use `save-current-buffer' instead
(put 'save-current-buffer 'edebug-form-spec '(body))
(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
    Executes BODY just like `progn'."
  (` (save-excursion (,@ body))))

(put 'with-current-buffer 'lisp-indent-function 1)
(put 'with-current-buffer 'edebug-form-spec '(body))
(defmacro-maybe with-current-buffer (buffer &rest body)
  "tinylibm.el
Execute the forms in BODY with BUFFER as the current buffer.
    The value returned is the value of the last form in BODY.
    See also `with-current-buffer'."
  (`
   (save-current-buffer
     (set-buffer (, buffer))
     (,@ body))))

(defmacro-maybe with-output-to-file (file &rest body)
  "Open FILE and run BODY.
\(with-output-to-file \"foo\"
  (print '(bar baz)))."
  `(with-temp-file ,file
     (let ((standard-output (current-buffer)))
       ,@body)))

;; Emacs 19.30 and below don't have this

(defun-maybe match-string (level &optional string)
  ;;  "Read match from buffer at sub match LEVEL. Optionally from STRING.
  ;;Return nil, if match at LEVEL doesn't exist.
  ;;
  ;;You have to call `looking-at' etc. before using this function.
  ;;You can use use `ti::buffer-match' or `ti::string-match' directly too."
  (if (match-end level)
      (if (stringp string)
          (substring
           string
           (match-beginning level) (match-end level))
        (buffer-substring
         (match-beginning level) (match-end level)))))

;; (replace-regexp-in-string
;;   REGEXP REP STRING &optional FIXEDCASE LITERAL SUBEXP START)

;;  (string regexp rep &optional subexp count)
;;
(defun-maybe replace-regexp-in-string
  (regexp rep string &optional fixedcase literal subexp start)
  (let* ((i  0))
    (or subexp
        (setq subexp 0))
    (while (string-match regexp string)
      (if (> (incf i) 5000)
          (error "Substituted string causes circular match. Loop never ends.")
        (inline (setq string (ti::replace-match subexp rep string)))))
    string))

(defun-maybe buffer-substring-no-properties (beg end)
  (ti::remove-properties (buffer-substring beg end)))

;; Here's the pre-Emacs 20.3 definition.  Note the optional arg.

(defun-maybe match-string-no-properties (num &optional string)
  ;;   "Return string of text matched by last search, without text properties.
  ;; NUM specifies which parenthesized expression in the last regexp.
  ;;  Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
  ;; Zero means the entire text matched by the whole regexp or whole string.
  ;; STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
          (let ((result
                 (substring string (match-beginning num) (match-end num))))
            (set-text-properties 0 (length result) nil result)
            result)
        (buffer-substring-no-properties (match-beginning num)
                                        (match-end num)))))

;; This is from pcvs.el
(defun-maybe file-to-string (file &optional oneline args)
  "Read the content of FILE and return it as a string.
If ONELINE is t, only the first line (no \\n) will be returned.
If ARGS is non-nil, the file will be executed with ARGS as its
arguments.  If ARGS is not a list, no argument will be passed."
  (with-temp-buffer
    (condition-case nil
        (progn
          (if args
              (apply 'call-process
                     file nil t nil (when (listp args) args))
            (insert-file-contents file))
          (buffer-substring (point-min)
                            (if oneline
                                (progn (goto-char (point-min))
                                       (end-of-line)
                                       (point))
                              (point-max))))
      (file-error nil))))

(defun-maybe file-name-extension (filename)
  (ti::file-get-extension filename))

(defun-maybe file-name-sans-extension (filename)
  ;;  "Return FILENAME without extension."
  (replace-regexp-in-string "\\.[^.]+$" ""  filename))

;; Emacs 20.3 invented its own function names `line-beginning-position'
;; `line-end-position' while XEmacs already had had point-* function
;; names since 1996: `point-at-eol' and `point-at-bol'.

(defsubst-maybe line-beginning-position (&optional n)
  "Return begin position of line forward N."
  (save-excursion
    (if n
        (forward-line n))
    (beginning-of-line) (point)))

(defsubst-maybe line-end-position (&optional n)
  "Return end position of line forward N."
  (save-excursion
    (if n
        (forward-line n))
    (end-of-line) (point)))

(defsubst-maybe insert-file-literally (file) ;; XEmacs 21.4 does not have this
  "Insert contents of file FILENAME into buffer after point with no conversion."
  (let (find-file-hooks
        write-file-hooks
        auto-save-hook
        auto-save-default)
    (insert-file file)))

(eval-and-compile
  (if (locate-library "executable") ;; 20.4 defines this
      (autoload 'executable-find "executable")
    (defun-maybe executable-find (program-name)
      ;;  "Find PROGRAM-NAME along `exec-path'."
      (ti::file-get-load-path program-name exec-path))))

(defun-maybe executable-find-in-system (program-name) ;Handle Win32 case too.
  ;;   "Find PROGRAM-NAME along `exec-path'.
  ;; The PROGRAM-NAME should not contain system dependent prefixes; an
  ;; .exe is added automatically on PC."
  (if (ti::win32-p)
      (or (executable-find (concat program-name ".exe"))
          (executable-find (concat program-name ".com"))
          (executable-find (concat program-name ".bat"))
          (executable-find (concat program-name ".cmd")))
    (executable-find program-name)))

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. XEmacs20 char . .

(defmacro ti::compat-character-define-macro (function1 function2)
  "Define XEmacs compatible character FUNCTION2 as an alias for FUNCTION1."
  (`
   (when (or (not (fboundp (, function1)))
             (and (ti::emacs-p)
                  (fboundp (, function1))
                  (or (not (equal (symbol-function (, function1))
                                  (, function2)))
                      ;;  If the definition is 'ignore, reassign correct
                      ;;  function.
                      (equal (symbol-function (, function1))
                             'ignore))))
     (defalias (, function1) (, function2)))))

(defun ti::compat-char-int-p (ch)     ;Not in Emacs (in XEmacs20 MULE)
  (and (integerp ch)
       (> ch -1)                        ;valid range 0-255
       (< ch 255)))

(defun ti::compat-define-compatibility-defalias ()
  "Emacs and XEmacs compatibility.
Define XEmacs character functions to work in Emacs.
Function mappings are:

  int-to-char      identity
  char-equal       equal
  char-to-int      identity
  chars-in-string  length
  characterp       integerp
  char-int-p       ti::compat-char-int-p
  char-int         identity"
  ;;  - In Emacs the characters are treated as integers
  ;;  - In XEmacs charactersa are their own data type
  (dolist (elt '((int-to-char identity)
                 (char-equal  equal)
                 ;;  Not in Emacs (exist in XEmacs 20)
                 (char-to-int identity)
                 ;;  Emacs 20.2/20.3 change
                 (chars-in-string length)
                 ;;  exists only in XEmacs
                 (characterp integerp)
                 (char-int-p ti::compat-char-int-p)
                 (char-int   identity)))
    (multiple-value-bind (original alias) elt
      (ti::compat-character-define-macro original alias))))

(ti::compat-define-compatibility-defalias)

(defun-maybe char= (ch1 ch2 &optional ignored-arg) ;exists in  XEmacs 20.1
  (let* (case-fold-search)                         ;case sensitive
    (char-equal ch1 ch2)))

;;  eshell-mode.el fix
(eval-after-load "eshell-mode"
  '(progn (ti::compat-define-compatibility-defalias)))

;;  eshell-2.4.1/esh-mode.el  mistakenly defines characterp
;;  as alias to `ignore' => breaks many things
(eval-after-load "esh-mode"
  '(progn (ti::compat-define-compatibility-defalias)))

;;  Gnus MIME handling also behaves wrong
(eval-after-load "mm-decode"
  '(progn (ti::compat-define-compatibility-defalias)))

;; See cplus-md.el
(defun-maybe count-char-in-string (c s)
  "Count CHARACTER in STRING."
  (let ((count 0)
        (pos   0))
    (while (< pos (length s))
      (if (char= (aref s pos) c)
          (incf  count))
      (incf  pos))
    count))

(defun-maybe count-char-in-region  (beg end char)
  "In region BEG END, count all CHAR occurrences.
E.g. to have real line count in buffer that
is running folding.el or outline, you should not call
count-lines function , but (count-char-in-region ?\\n)"
  (interactive "r\ncChar: ")
  (let* ((i 0))
    (setq end (max beg end)
          char (char-to-string char))
    (save-excursion
      (goto-char (min beg end))
      (while (search-forward char end  t)
        (incf  i)))
    (if (interactive-p)
        (message "%d hits in region." i))
    i))

(defun-maybe char-assq (ch alist)
  "If CH can be found in ALIST, return entry. If CH is nil, do nothing."
  (let (case-fold-search
        ret)
    (while (and ch alist)
      (setq ret (car alist))
      (if (char= ch (car ret))
          (setq alist nil)
        (setq alist (cdr alist)
              ret nil) ))
    ret))

;;  XEmacs : replace-in-string
;;  Emacs 20.4
(defun-maybe subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
INPLACE is ignored."
  (let ((len   (length string))
        (ret   (copy-sequence string))) ;because 'aset' is destructive
    (while (> len 0)
      (if (char= (aref string (1- len)) fromchar)
          (aset ret (1- len) tochar))
      (decf len))
    ret))

(defun-maybe subst-char-with-string (string &optional char to-string)
  "In STRING, convert CHAR with TO-STRING.
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

(eval-and-compile
  (when (or (featurep 'xemacs)
            (boundp 'xemacs-logo))
    ;;   Just a forward declaration, because byte-compiler cannot see through
    ;;   defun-maybe. If this function already exists, this autoload
    ;;   definition is no-op.
    (autoload 'subst-char-in-string "tinylibb.el")))

;; Emacs and XEmacs differ here. Convert Emacs function --> XEmacs name

(cond
 ((and (fboundp 'exec-to-string)
       (not (fboundp 'shell-command-to-string)))
  (defalias-maybe 'shell-command-to-string 'exec-to-string))
 ((not (fboundp 'shell-command-to-string))
  (defun-maybe shell-command-to-string (command)
    "Returns shell COMMAND's ouput as string. Tinylibm."
    (with-temp-buffer
      (shell-command command (current-buffer))
      (buffer-string)))))

;;; XEmacs ilisp.el :: describe-symbol-find-file
(defun-maybe describe-symbol-find-file (symbol) ;; XEmacs
  "Find SYMBOL defined in file."
  (loop for (file . load-data) in load-history
        do (when (memq symbol load-data)
             (return file))))

;; shell.el, term.el, terminal.el

(unless (boundp 'explicit-shell-file-name)
  (defvar explicit-shell-file-name nil))

(unless (boundp 'shell-command-output-buffer)
  (defvar shell-command-output-buffer "*Shell Command Output*"))

(when (or (not (boundp 'temporary-file-directory))
          (not (stringp temporary-file-directory))
          (not (file-directory-p temporary-file-directory)))
  (let* ((temp (or (getenv "TEMP")
                   (getenv "TEMPDIR")
                   (getenv "TMPDIR"))))
    (defvar temporary-file-directory    ;Emacs 20.3
      (or temp
          (cond
           ((file-directory-p "/tmp") "/tmp")
           ((file-directory-p "~/tmp") "~/tmp")
           ((file-directory-p "C:/temp") "C:/temp")
           ;; don't know what to do, maybe this exists.
           (t "/")))
      "*Tinylib: XEmacs and Emacs compatibility.")))

;;; ........................................................... &other ...

;; Emacs 20.7 - 21.2 does not have this
(defun-maybe turn-off-font-lock ()
  "Turn off font lock."
  (font-lock-mode -1))

;; Emacs 21.3 includes `turn-on-font-lock'
(defun-maybe turn-on-font-lock-mode ()
  "Turn on font lock."
  (font-lock-mode 1))

(defun-maybe turn-on-auto-fill-mode ()
  "Turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun font-lock-mode-maybe (&optional mode check-global)
  "Pass MODE to function `font-lock-mode' only on color display.
If CHECK-GLOBAL is non-nil, the `global-font-lock-mode' flag must also
be non-nil before calling.

Usually there is no point of turning on `font-lock-mode' if Emacs
can't display colors, so this is is the umbrella function to
font-lock.el"
  (when (and (featurep 'font-lock)
             (ti::colors-supported-p)
             (or (null check-global)
                 (and (boundp 'global-font-lock-mode)
                      (symbol-value 'global-font-lock-mode))))
    (font-lock-mode mode)
    t))

(defun turn-on-font-lock-mode-maybe ()
  "Call `font-lock-mode-maybe' with argument 1."
  (font-lock-mode-maybe 1))

(defalias-maybe 'compose-mail 'mail)

(defun-maybe region-active-p ()         ;XEmacs function
  "Return `mark' if mark (region) is active."
  (cond
   ((and (ti::xemacs-p)
         (boundp 'zmacs-regions))
    (let* ((zmacs-regions t))           ;XEmacs
      (mark)))
   ((boundp 'mark-active)               ;Emacs
    (and (symbol-value 'mark-active)
         ;;  used to return (mark-marker)
         (mark 'noerr)))))

;; Newer advice "2.15" uses this call, make sure it exist.
(defalias-maybe 'byte-code-function-p 'ignore)

(defun-maybe add-to-list (list-var element)
  ;;  "Add to symbol LIST-VAR ELEMENT."
  (or (member element (symbol-value list-var)) ;; copy from 19.34
      (set list-var (cons element (symbol-value list-var)))))

(defun-maybe run-hook-with-args-until-success
  (hook-sym &optional &rest args)
  ;;   "Run all functions in HOOK-SYM. Stop when first one return non-nil.
  ;;
  ;; Input:
  ;;
  ;;   HOOK-SYM  hook symbol, or list of functions.
  ;;   ARGS           arguments to functions. if NIL, functions
  ;;             are called without arguments."
  (let* ((val  (symbol-value hook-sym))
         (list (if (listp val) val (list val))) ;Make list maybe
         ret
         func)
    (while (and (null ret) list)
      (setq func (car list)   list (cdr list))
      (setq ret (apply func args)))
    ret))

(defun-maybe buffer-live-p (buffer)
  ;;  "Check if BUFFER exist."
  (cond
   ((not (bufferp buffer))
    (error "must be pointer"))
   ((stringp buffer)
    (get-buffer buffer))
   (buffer
    (buffer-name buffer))))

(eval-when-compile
  ;;  don't show "obsolete function warning", because we know what
  ;;  we're doing below.
  (put 'frame-parameters 'byte-compile nil))

(when (not (fboundp 'frame-parameter))  ;Emacs 19.35
  (if (fboundp 'frame-property)
      (defalias 'frame-parameter 'frame-property) ; XEmacs.
    (defun frame-parameter (frame property &optional default)
      "Return FRAME's value for property PROPERTY."
      (or (cdr (assq property (frame-parameters frame)))
          default))))

(unless (and (fboundp 'find-file-binary) ;; Emacs function --> XEmacs
             (boundp 'buffer-file-coding-system))
  (defun find-file-binary (file)
    "Read FILE without conversiosn."
    (let* ((buffer-file-coding-system 'binary))
      (unless buffer-file-coding-system
        (setq buffer-file-coding-system nil)) ;Quiet Bytecompiler "unused  var".
      (find-file file))))

;;}}}
;;{{{ special

;;; ........................................... &compatibility-special ...
;;; These need emacs-p xemacs-p tests

;; not known function in 19.14

(eval-and-compile
  (autoload 'read-kbd-macro "edmacro")
  (when (ti::emacs-p)
    (or (fboundp 'kbd)                  ;Std in Emacs 20.x
        (defmacro kbd (keys)            ;(kbd "C-<delete>")
          "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros (see `insert-kbd-macro')."
          (let ((f 'read-kbd-macro))
            (funcall f keys))))))

;;}}}
;;{{{ code: function test

;;; ...................................................... &func-tests ...
;;; We define these here because they are used lated in this library
;;; "define before using"

(eval-and-compile

;;; ----------------------------------------------------------------------
;;;
  (defun-maybe functionp (obj) ;; Emacs 20.3+ XEmacs 20.x
    (or (subrp obj)
        (byte-code-function-p obj)
        (and (symbolp obj)
             (fboundp obj))
        (and (consp obj)
             (eq (car obj) 'lambda))))

;;; ----------------------------------------------------------------------
;;;
  (defun ti::function-args-p (symbol)
    "Return function SYMBOL's argument list as string or nil.
Works for byte compiled functions too.

Notes:
  if function is alias, the real function behind it is examined.
  if function is in autoload state, \"(autoload-args)\" is returned."
    (let* ((args-re-xemacs ;; arguments: (&optional BUFFER)
            "arguments: +(\\([^)]+\\))")
           (args-re ;; (buffer-size &optional BUFFER)
            "([^(]+\\([^)]+)\\)")
           sym
           sym-func
           str
           ret)
      (if (ti::autoload-p symbol)
          ;;  We can't know the args. And we don't want to find out,
          ;;  since it would load the package unnecessarily
          (setq ret "(autoload-args)")
        (if (setq sym (ti::defalias-p symbol))
            (setq symbol sym))
        (setq sym-func (symbol-function symbol))
        (if (subrp sym-func)
            (setq str (documentation sym-func))
          (setq str (prin1-to-string sym-func)))
        ;;  "$ad-doc: mouse-yank-at-click$" (interactive "e\nP")
        (when (and (string-match "ad-doc:" str)
                   (setq symbol
                         (intern-soft
                          (format "ad-Orig-%s"
                                  (symbol-name symbol)))))
          (setq str (prin1-to-string  (symbol-function symbol))))
        (cond
         ((ti::emacs-p)
          (cond
           ;; "#[(click arg)
           ((string-match "^#\\[(\\([^)]+\\)" str)
            (setq ret (match-string 1 str)))
           ((or (string-match "^(lambda[ \t]+nil" str)
                (string-match "^#\\[nil" str))
            (setq ret nil))
           ((string-match args-re str)
            (setq ret (match-string 1 str))
            ;;  Empty arg list
            (if (string= ret "")
                (setq ret nil)))))
         (t
          ;;  XEmacs has different Byte compilation format
          ;;  #<compiled-function (from "custom.elc") nil "...(7)
          (cond
           ((string-match
             (concat "compiled-function +\(from.*\) +" args-re) str)
            (setq ret (match-string 2)))
           ((string-match "^(lambda +nil" str)) ;bypass
           ((string-match args-re-xemacs str)
            (setq ret (match-string 1 str)))
           ((string-match args-re str)
            (setq ret (match-string 1 str)))))))
      ret)))

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++--  eval-and-compile --

;;}}}
;;{{{ code: Cygwin support

;;; ........................................................... cygwin ...

;;; Patch for these functions has been submitted to Emacs 21.2
;;; (w32-fns.el)

(defvar w32-cygwin-mount-table nil
  "Cygwin mount.exe mapping. See `w32-cygwin-mount-table'.")

;;; ----------------------------------------------------------------------
;;;
(put 'w32-cygwin-mount-table-dolist 'lisp-indent-function 0)
(put 'w32-cygwin-mount-table-dolist 'edebug-form-spec '(body)) ;;#todo: not working
(defmacro w32-cygwin-mount-table-dolist (&rest body)
  "Run DOLIST for Cygwin mount table.
`mount' is complete mount element (cygwin . dos).
Variables `cygwin' and `dos' are bound respectively."
  (`
   (dolist (mount w32-cygwin-mount-table)
     ;;  mount => ("/tmp" . "c:\\temp")
     (let* ((cygwin (car mount))
            (dos    (cdr mount)))
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'w32-cygwin-shell-environment 'lisp-indent-function 0)
(put 'w32-cygwin-shell-environment 'edebug-form-spec '(body))
(defmacro w32-cygwin-shell-environment  (&rest body)
  "Run BODY under Cygwin shell environment.
For example, you you want to call program ´zgrep' which is not an
.exe, but a shell program, you have to switch to the Cygwin context.

   (when (and (ti::win32-p)
              (ti::win32-cygwin-p))
      (w32-cygwin-shell-environment
           ...))

Variable ´shell-file-name' is locally bound during call."
  (`
   (let ((shell-file-name (format "%s/bin/hash.exe"
                                  (ti::win32-cygwin-p 'use-cache))))
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-parse ()
  ;; "Parse cygwin mount table from current point forward."

  ;;  Search lines with backslash
  ;;  f:\\u\\bin /usr/bin user binmode
  ;;
  ;;  Cygwin 1.3.3 changed format, it is now
  ;;
  ;;  f:\\u\\bin on /usr/bin type user (binmode)
  ;;             ==
  ;;
  ;;  \\network\path\this

  (let (list
        (regexp
         (save-excursion
           (if (re-search-forward "^\\([a-z]:\\|[\\][\\]\\).* on " nil t)
               (concat
                "^\\([a-zA-Z]:[\\][^ \t\r\n]*"
                "\\|[a-zA-Z]:"
                "\\|[\\][\\][^ \t\r\n]+"
                "\\)"
                "[ \t]+on[ \t]+"
                "\\(/[^ \t\r\n]*\\)")
             (concat
              "^\\([a-zA-Z]:[\\][^ \t\r\n]*"
              "\\|[a-zA-Z]:"
              "\\|[\\][\\][^ \t\r\n]+"
              "\\)"
              "[ \t]+"
              "\\(/[^ \t\r\n]*\\)")))))
    (while (re-search-forward regexp nil t)
      (let ((dos    (match-string 2))
            (cygwin (match-string 1)))
        (push (cons dos cygwin)
              list)))

    ;;  sort the entries so that the longest mounts come first and
    ;;  last the shortest. This makes a difference when Cygwin paths are
    ;;  converted back to dos:
    ;;
    ;;    /tmp/other       mapping must be handled before /tmp
    ;;    /tmp
    ;;    ..

    (sort list
          (function
           (lambda (a b)
             (> (length (car a))
                (length (car b))))))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-convert (path &optional flag)
  "Run `cygpath' to find out PATH.
Return:

   The default concersion is CYGWIN => DOS

   If `flag' is set, then the conversion is
   DOS => cygwin."
  (let* ((cmd     (executable-find "cygpath"))
         (option  "--windows")
         ret)
    (when cmd
      (when flag
        (setq option "--unix"))
      (with-temp-buffer
        (call-process
         cmd
         nil
         (current-buffer)
         nil
         option
         path)
        (goto-char (point-min))
        (when (looking-at "^.*") ;; Filter newlines
          (setq ret (match-string 0)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table ()
  ;; "Return Cygwin mount table '((CYGWIN . DOS) ..) using `mount' command."
  (when ;; (memq system-type '(ms-dos windows-nt))
      (ti::win32-p)
    ;; specifically request the .exe which must be along PATH
    ;; if we used only `mount', that could call user's "mount.bat" or
    ;; something.
    (let ((cmd  (executable-find "mount.exe")))
      (when cmd
        (with-temp-buffer
          (call-process cmd nil (current-buffer))
          (goto-char (point-min))

          ;;  It's a serious error if "mount" does not say where
          ;;  the ROOT "/" is. Should we do something?

          (goto-char (point-min))
          (let ((ret (w32-cygwin-mount-table-parse)))
            (unless ret
              (error "Cygwin mount.exe output parse failed:\n[%s]"
                     (buffer-string)))
            ret))))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-point-to-dos (path)
  "Convert Cygwin mount filenames like  /tmp to DOS paths."
  (let* (last-choice
         try)
    (dolist (cygwin w32-cygwin-mount-table)
      (when (string-match (concat "^"  (car cygwin) "\\(.*\\)")
                          path)
        (setq try
              ;;  expand will ensure that slashes are after glue
              ;;  to the same direction
              (expand-file-name
               (concat (file-name-as-directory (cdr cygwin) )
                       (match-string 1 path))))
        ;;  It is difficult to expand the file name correctly because
        ;;  user can make any mount points. That's what we compare which
        ;;  mount point gives the longest match and return it.
        ;;
        ;;  E.g. the root / will always match, but it is not necessarily
        ;;  the final answer given path /tmp/something where there is
        ;;  separate mount point for longer match /tmp
        ;;
        (if (null last-choice)
            (setq last-choice (cons (car cygwin) try))
          (if (length (> (car cygwin) (car last-choice)))
              (setq last-choice (cons (car cygwin) try))))))
    (if (null last-choice)
        path
      (cdr last-choice))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-set ()
  ;;   "Run mount.exe and set internal variable `w32-cygwin-mount-table'.
  ;; You should run this function after you have made a change to
  ;; cygwin mount points."
  ;;   (interactive)
  (if (ti::win32-p) ;; (memq system-type '(ms-dos windows-nt))
      (setq w32-cygwin-mount-table
            (w32-cygwin-mount-table))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-path-to-dos (path)
  "Convert PATH to dos using cygwin mount table.
You should not call this function, use `w32-cygwin-path-to-dos'."
  ;;  Convert Cygwin /usr/local to DOS path. LOCATION/usr/local.
  ;;  This relies on the fact that the longest paths are first
  ;;  in the mount table.
  (let (final-path)
    (w32-cygwin-mount-table-dolist
      ;;  mount => ("/tmp" . "c:\\temp")
      ;;  variables `cygwin' and `dos' are part of the macro
      (when (string-match (concat "^" (regexp-quote cygwin)
                                  "\\(.*\\)")
                          path)
        (unless (string= cygwin "/")
          (setq dos (concat dos (match-string 1 path))))
        ;; Convert to forward slashes
        (setq final-path (subst-char-in-string ?\\ ?/ dos))
        (return)))
    (unless final-path
      ;; None matched, so this path is under cygwin root dir.
      (let ((root (ti::win32-cygwin-p)))
        (setq final-path (concat root path))))
    final-path))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-path-to-dos (path)
  "Convert cygwin like //c/temp  or /cygdrive/c/temp path to
  dos notation c:/temp."
  ;; NOTE for cygwin and bash shell prompt
  ;; We can't require a slash after the drive letter, because
  ;; //c   and  /cygdrive/c   are all top level roots.
  ;;
  ;; The bash shell's PS1 setting \w (The current working directory)
  ;; Does not add trailing slash.
  (cond
   ((or (string-match "^//\\([a-z]\\)/?$" path)
        (string-match "^/cygdrive/\\([a-z]\\)/?$" path))
    (concat (match-string 1 path) ":/"))
   ((or (string-match "^//\\([a-z]\\)\\(/.*\\)" path)
        (string-match "^/cygdrive/\\([a-z]\\)\\(/.*\\)" path))
    (concat (match-string 1 path) ":" (match-string 2 path)))
   ((string-match "^(/cygdrive/./\\|//" path)
    ;;  if previous regexps couldn't handle it, this is severe error.
    (error "Invalid path format for cygwin %s" path))
   ((string-match "[\\]" path)
    (error "Invalid backslash path %s" path))
   ((string-match "^/" path)
    (w32-cygwin-mount-table-path-to-dos path))
   (t
    path)))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-dos-path-to-cygwin (path)
  "Convert dos PATH to cygwin path.
Be sure to call `expand-file-name' before you pass PATH to the function."
  (cond
   ((string-match "\\([a-z]\\):[\\/]\\(.*\\)" path)
    (let ((drive     (format  "/cygdrive/%s/" (match-string 1 path)))
          (rest-path (match-string 2 path)))
      (if (not rest-path)
          drive
        (w32-cygwin-mount-table-dolist
          ;;  mount => ("/tmp" . "c:\\temp")
          ;;  variables `cygwin' and `dos' are part of the macro
          (when (or (string-match (concat "^" dos "\\(.*\\)") path)
                    (string-match (concat "^"
                                          ;; Convert to / slashes
                                          (expand-file-name dos)
                                          "\\(.*\\)") path))
            (when (match-string 1 path)
              (setq path (match-string 1 path))
              (setq cygwin (concat cygwin path)))
            ;; Convert to forward slashes
            (return (subst-char-in-string ?\\ ?/ cygwin)))))))
   (t
    (error "Cannot convert to cygwin. path is not absolute %s" path))))

;;  Make it defconst, so that rereading tinylibb.el will always update
;;  the value. If Cygwin is changed, reloading this library.

(setq w32-cygwin-mount-table
      (if (ti::win32-p) ;; (memq system-type '(ms-dos windows-nt))
          (w32-cygwin-mount-table)))

(defsubst w32-expand-file-name-for-cygwin (path)
  "Expand PATH to Cygwin notation if Cygwin is present."
  (when (and (string-match "^[A-Za-z]:" path)
             (ti::win32-cygwin-p))
    (setq path (w32-cygwin-dos-path-to-cygwin path)))
  path)

(defsubst w32-expand-file-name-for-emacs (path)
  "Expand PATH to DOS Emacs notation if PATH is in Cygwin notation."
  (cond
   ((and (ti::emacs-type-win32-p)
         (string-match "^/cygdrive" path))
    (setq path (w32-cygwin-path-to-dos path)))
   ((and (ti::emacs-type-cygwin-p)
         (string-match "^[a-zA-Z]:" path))
    (setq path (w32-cygwin-dos-path-to-cygwin path))))
  path)

;;}}}

;;; ########################################################## &custom ###

;;{{{ custom

;;; 2000-03-20
;;; - This code is beginning to be obsolete now when Newest Emacs is 21.2
;;;   custom.el.
;;; - This code does nothing if custom.el is present, so let it be here.

(eval-and-compile
  (cond
   ((string-match "2[0-9]\\." (emacs-version))
    (require 'custom))                  ;Out of the box
   (t                           ;Well, this is old Emacs - lot of work
    (let* ((list  load-path)
           dir
           try
           path)
      (cond
       ;; ..................................................... no custom ...
       ;;  The reason why newest custom.el does not work in prior releases is the
       ;;  new bacquote macro syntax it uses. It needs new emacs lisp parser to
       ;;  read the macros.
       ;;
       ((or (and (ti::emacs-p)
                 (< emacs-minor-version  34))
            (and (eq 19 (ti::xemacs-p))
                 (< emacs-minor-version  15)))
        ;;  This emacs is too old for new custom. Emulate it.
        (defmacro defgroup (&rest args) nil)
        (defmacro defcustom (var value doc &rest args)
          (` (defvar (, var) (, value) (, doc)))))
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. custom maybe . .
       (t
        ;; Explanation: When I say (require 'custom) in -batch byte
        ;; compile; and the load-path HAD my private ~/elisp at front,
        ;; but it still loaded old custom.elc from XEmacs 19.14 distribution.
        ;;
        ;; Why? Don't know. That's why we load it manually here.
        (while (and (null path)         ;Where it is?
                    (setq dir (car list)))
          (setq try
                (if (string-match "/$" dir)
                    (concat dir "custom.el")
                  (concat dir "/custom.el")))
          (when (file-exists-p try)
;;;     (message (format "tinylibm: ** Using custom from [%s]" try))
            (setq path (file-name-directory try)))
          (setq list (cdr list)))
        ;; ............................................... load new custom ...
        (condition-case ()
            (progn
              ;; The new custom won't work in .el format, it must be
              ;; loaded in .elc format.
              (unless (featurep 'custom)
                (load (concat path "custom.elc"))))
          (error
           (message "tinylibm: ** Couldn't find custom.elc [compiled version]")))
        ;;  Check few things, what this custom.elc provided.
        ;;  This is internal information to debug things
        ;;
        (message "tinylibm: ** internal info: Custom [%s] declare [%s]"
                 (if (featurep 'custom)
                     "t"
                   "nil")
                 (if (fboundp 'custom-declare-variable)
                     "t"
                   "nil"))
        (cond
         ((and (featurep 'custom)
               (fboundp 'custom-declare-variable))
          ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . case 1 ..
          ;;  19.14 includes a very old custom.el, and it shouldn't be used
          ;;  any more.
          ;;
          ;;  custom-XE19.14    : custom.el::customize()
          ;;  custom-1.96       : cus-edit.el::(defun customize (symbol)
          ;;  custom-1.9956     : cus-edit.el:::customize()
          ;;                      cus-edit.el::customize-group (group)
          (cond
           ((and (null (ti::function-args-p 'customize))
                 (not (fboundp 'customize-group)))
            (message "\
tinylibm.el: ** [Ignore, Compilation is still going fine...]
             ** Hm, loading custom didn't go quite right. Reasons:
             ** a. You have too old custom.el library, because I can't
             **    see `customize' function to take ONE argument.
             **    Be sure to have newest custom.el and cus-edit.el
             ** b. Your load-path is set so that the old custom.el
             **    was loaded."))
           (t
            ;;  The new 1.9956 Custom.el produces warning for defcustom
            ;;  variables not beeing defined. This code is only for
            ;;  19.34 and won't work anywhere else.
            ;;
            (if (string-match
                 "19.2[0-9]\\|19.3[0-3]\\|19.1[0-4]"
                 (emacs-version))
                (message "\
             ** ...But you don't have [X]Emacs 19.34, 19.15, or 20+
             ** That's why you see lot of undefined variables.
             ** It's a byte compiler issue, nothing to worry about.")
              ;; This is part of bytecomp.el in 20.1:
              ;;
              (put 'custom-declare-variable 'byte-hunk-handler
                   'byte-compile-file-form-custom-declare-variable)
              (defun byte-compile-file-form-custom-declare-variable (form)
                (if (memq 'free-vars byte-compile-warnings)
                    (setq byte-compile-bound-variables
                          (cons (nth 1 (nth 1 form))
                                byte-compile-bound-variables))) form))))

          nil)
         ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . case 2 ..
         (t
          (unless (string-match "19.2[0-9]\\|19.3[0-3]\\|19.1[0-4]"
                                (emacs-version))
            (message "\
tinylibm.el: ** Too old custom.el; You should upgrade your Emacs."))
          ;; We have the old custom-library, hack around it.
          (defmacro defgroup (&rest args) nil)
          (defmacro defcustom (var value doc &rest args)
            (` (defvar (, var) (, value) (, doc))))))))))))

;;}}}

;;; ################################################### &byte-optimize ###

;;{{{ misc

(when (and nil                          ;Disabled now
           (null (get 'concat 'byte-optimizer)))
  (put  'concat 'byte-optimizer 'tinylibb-byte-optimize-concat)

  ;; Like `concat', but this macro expands to optimized form.
  ;; Many times you want to divide complex regexps on separate lines like
  ;; this
  ;;    (looking-at (concat
  ;;                  ;; Comment
  ;;                  \"regexp-1\"
  ;;                  ;; Comment
  ;;                  \"regexp-2\"
  ;;                  ))
  ;;
  ;; This is perfectly good way, but won't be optimized in any way:
  ;; The compiled version contains `concat' command and separate strings.
  ;;
  ;; This optimized `concat' macro will expand the ARGS to single string
  ;; "regexp-1regexp-2\ if they all are strings.
  ;; In other cases it expands to normal `concat' call.
  ;;
  ;;   (defmacro concat-macro (&rest args)
  ;;     (if (every 'stringp args)
  ;;         (apply 'concat args)
  ;;       (cons 'concat args)))
  ;;

  (defun tinylibb-byte-optimize-concat (form)
    (let ((args (cdr form))
          (constant t))
      (while (and args constant)
        (or (byte-compile-constp (car args))
            ;;  Stop there
            (setq constant nil))
        (setq args (cdr args)))

      (if constant
          (eval form)
        form))))

;;}}}
;;{{{ Version

;;; ......................................................... &version ...

(defconst tinylibb-version
  (substring "$Revision: 2.73 $" 11 15)
  "Latest version number.")

(defconst tinylibb-version-id
  "$Id: tinylibb.el,v 2.73 2007/05/01 17:20:45 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylibb-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylibb.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylibb-submit-bug-report ()
  "Submit bug report."
  (interactive)
  (ti::package-submit-bug-report
   "tinylibb.el"
   tinylibb-version-id
   '(tinylibb-version-id)))

;;}}}

;;; tinylibb.el ends here
