;;; tinylibb.el --- Library of (b)ackward compatible functions.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1998-2010 Jari Aalto
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
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;      This is lisp function library, package itself does nothing.
;;      This library defines some Emacs backward compatibility function.
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

;;}}}

;;; Change Log:

;;; Code:

;;; .......................................................... provide ...

(require 'tinyliba)
(provide 'tinylibb)

;;{{{ code: Emacs compatibility, aliases, byteCompiler

(eval-and-compile
  (autoload 'ti::replace-match "tinylibm"))

(defconst tinylibb-version-time "2010.1208.0755"
  "Latest version number as last modified time.")

;;; ....................................................... &emulation ...

(defun-maybe replace-char-in-string (ch1 ch2 string)
  ;;  "Search CH1, change it with CH2 in STRING."
  (nsubstitute ch1 ch2 string))

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
  (let ((case-fold-search t)
        (n 0))
    (mapc '(lambda (c &optional i)
	     (setq i (string-match
		      (make-string 1 c)
		      "0123456789abcdefghijklmnopqrstuvwxyz"))
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

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. higher Emacs . .
;;:  Features found from new emacs only 20.xx

(defun-maybe byte-compiling-files-p ()
  "Return t if currently byte-compiling files."
  (string= (buffer-name) " *Compiler Input*"))


(defmacro-maybe with-output-to-file (file &rest body)
  "Open FILE and run BODY.
\(with-output-to-file \"foo\"
  (print '(bar baz)))."
  `(with-temp-file ,file
     (let ((standard-output (current-buffer)))
       ,@body)))

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

(defun-maybe executable-find-in-system (program-name) ;Handle Win32 case too.
  ;;   "Find PROGRAM-NAME along `exec-path'.
  ;; The PROGRAM-NAME should not contain system dependent prefixes; an
  ;; .exe is added automatically."
  (if (ti::win32-p)
      (or (executable-find (concat program-name ".exe"))
          (executable-find (concat program-name ".com"))
          (executable-find (concat program-name ".bat"))
          (executable-find (concat program-name ".cmd")))
    (executable-find program-name)))

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. XEmacs20 char . .

;; See cplus-md.el
(defun-maybe count-char-in-string (c s)
  "Count CHARACTER in STRING."
  (let ((count 0)
        (pos   0))
    (while (< pos (length s))
      (if (char-equal (aref s pos) c)
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
      (if (char-equal ch (car ret))
          (setq alist nil)
        (setq alist (cdr alist)
              ret nil) ))
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
        (if (char-equal char (aref string i))
            (setq elt to-string))
        (setq ret (concat ret elt))
        (incf  i))))
    ret))

;; shell.el, term.el, terminal.el

(unless (boundp 'explicit-shell-file-name)
  (defvar explicit-shell-file-name nil))

(unless (boundp 'shell-command-output-buffer)
  (defvar shell-command-output-buffer "*Shell Command Output*"))

;;; ........................................................... &other ...

(unless (fboundp 'with-buffer-modified)
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
    (let ((modified (gensym "modified-"))
	  (read-only (gensym "read-only-")))
      `(let ((,modified (buffer-modified-p))
	     (,read-only buffer-read-only))
	 (unwind-protect
	     (progn
	       (setq buffer-read-only nil)
	       ,@body)
	   (if ,modified
	       (set-buffer-modified-p t)
	     (set-buffer-modified-p nil))
	   (if ,read-only
	       (setq buffer-read-only t)
	     (setq buffer-read-only nil)))))))

;; FIXME: Emacs 21.3 includes `turn-on-font-lock' (really?)
(defun-maybe turn-on-font-lock-mode ()
  "Turn on Font Lock mode."
  (font-lock-mode 1))

(defun-maybe turn-off-font-lock-mode ()
  "Turn off Font Lock mode."
  (font-lock-mode -1))

(defun-maybe turn-on-auto-fill-mode ()
  "Turn on Auto Fill mode."
  (auto-fill-mode 1))

(defun-maybe turn-off-auto-fill-mode ()
  "Turn off Auto Fill mode."
  (auto-fill-mode -1))

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

;;}}}
;;{{{ code: function test

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
      ret))

;;}}}
;;{{{ misc

(when (and nil                          ;Disabled now
           (null (get 'concat 'byte-optimizer)))
  (put 'concat 'byte-optimizer 'tinylibb-byte-optimize-concat)
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

;;; tinylibb.el ends here
