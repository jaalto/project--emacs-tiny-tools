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

(defconst tinylibb-version-time "2010.1119.2315"
  "Latest version number as last modified time.")

;;; ....................................................... &emulation ...

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
;;{{{ code: Cygwin support

;;; These functions has been submitted to Emacs 21.2
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
  `(dolist (mount w32-cygwin-mount-table)
     ;;  mount => ("/tmp" . "c:\\temp")
     (let* ((cygwin (car mount))
            (dos    (cdr mount)))
       ,@body)))

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

Variable `hell-file-name' is locally bound during call."
  `(let ((shell-file-name (format "%s/bin/hash.exe"
                                  (ti::win32-cygwin-p 'use-cache))))
     ,@body))

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
