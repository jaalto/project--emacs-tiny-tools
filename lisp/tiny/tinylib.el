;;; tinylib.el --- Library of general functions
;; $Id: tinylib.el,v 2.107 2007/05/07 10:50:07 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylib-version
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

;; ........................................................ &t-install ...
;; Put this file to the package that you're developing. This file is
;; is mostly for developers.
;;
;;     (require 'tinylibm)  ;; No mistake here, you load `m' library
;;
;; If you have any questions or feedback, use this function
;;
;;      M-x tinylib-submit-feedback

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1995
;;
;;      This is library, so the package itself does nothing,
;;      there may be some interactive functions.
;;      There is a issue of Emacs and XEmacs differences multiplied with
;;      different OS platforms, like Cygwin and native Win32. In order to
;;      reuse the code in modules and to keep up with the Emacs/XEmacs
;;      interface and different releases, the general function can be found
;;      from these libraries.
;;
;;  Defining a minor mode
;;
;;      This library provides Emacs/XEmacs comatible minor mode
;;      since 1995. There is one macro that defines all minor mode
;;      variables and function.
;;
;;          (eval-and-compile   ;; So that defvars and defuns are seen
;;            (ti::macrof-minor-mode-wizard
;;             ;;
;;             ;; 1. prefix for variables and functions
;;             ;; 2. Modeline name
;;             ;; 3. prefix key for mode.
;;             ;; 4. Menu bar name
;;             ;; 5. <forget this>
;;             ;;
;;             "xxx-" " xxxModeline" "\C-cx" "xxxMenubar" nil
;;
;;             "XXX minor mode. Does fancy things."  ;; mode description
;;              Defined keys:
;;              \\{xxx-mode-map}
;;              "
;;
;;              "XXX help"    ;; message displayed when user calls mode
;;              nil           ;; Forms When minor mode is called
;;
;;             ;; This is used by easy-menu.el and defines menu items.
;;             (list
;;              xxx-mode-easymenu-name
;;              ["Eval whole buffer" xxx-eval-current-buffer    t]
;;              ..)
;;
;;             ;;  this block defines keys to the mode. The mode minor map is
;;             ;;  locally bound to 'map' symbol.
;;             (progn
;;               (define-key map "-" 'xxx-eval-current-buffer)
;;               (define-key map "=" 'xxx-calculate))))
;;
;;  Defining minor mode step by step
;;
;;      If you want to take more personal control over the minor mode
;;      creation, here I explain step by step what macros you need to include
;;      in your package to get minor mode created, This takes only
;;      half an hour and you have basic minor mode ready. Put all
;;      following calls near top of your file. We suppose we're
;;      creating XXX-mode.
;;
;;      _[1]_ First, define standard variables for minor mode.
;;
;;          (ti::macrov-minor-mode "xxxModeline" "\C-cx" "xxxMenubar")
;;
;;      After that user has  following varibles for customization. (for
;;      complete list of created variables, see the macro's description)
;;
;;          ;; Don't like default key combo C-c x
;;          (setq xxx-mode-prefix-key "\C-cm")
;;
;;          ;; The default mode string was too long, use shorter.
;;          (setq xxx-mode-name "xxx")
;;
;;          ;;  When mode runs, I want to do this.
;;          (add-hook 'xxx-mode-hook 'my-xxx-settings)
;;
;;          ;;  I want to add additional keys
;;          (add-hook 'xxx-mode-define-keys-hook 'my-xxx-keys)
;;
;;      _[2]_ Next we need installation function, which installs our minor
;;      mode so that emacs is aware of it. The minor mode functions,
;;      xxx-mode, will call xxx-mode-define-keys-hook which takes care of
;;      defining keys to key maps and creating menus with easy-menu.el. The
;;      separate installation function is used, because it takes care of
;;      emacs specific things and if called with additional argument, it
;;      also knows how to remove the mode gracefully.
;;
;;          (ti::macrof-minor-mode-install
;;           xxx-install-mode
;;           xxx-mode
;;           xxx-mode-map
;;           xxx-mode-name
;;           xxx-mode-define-keys-hook)
;;
;;      _[3]_ Do we have additional files attached to the end of package?
;;      If yes, then we need pgp-tar unpack function too.
;;
;;          (ti::macrof-install-pgp-tar "xxx-install-files"  "xxx.el")
;;
;;      _[4]_ Finally the user callable minor mode function is created.
;;
;;          (ti::macrof-minor-mode
;;           xxx-mode
;;           "XXX minor mode. Does fancy things."
;;           Defined keys:
;;           \\{xxx-:mode-map}
;;           "
;;           xxx-install-mode
;;           xxx-mode
;;           xxx-mode-name
;;           xxx-mode-prefix-key
;;           xxx-mode-easymenu
;;           nil                        ;Yes, print turn on/off message
;;           "XXX help"
;;           xxx-mode-hook)
;;
;;      That's it. when you execute all these statements you have basic core
;;      for emacs minor mode. The only things missing is the actual
;;      functions that the minor mode commands uses and the function that
;;      defines keys and menus for the minor mode. You probably want to
;;      start from the function that defines keys and menus. Here is ready
;;      macro for that too.
;;
;;          (add-hook' xxx-mode-define-keys-hook 'xxx-mode-define-keys)
;;
;;          (ti::macrof-define-keys
;;           "xxx-mode-define-keys"
;;           'xxx-:mode-prefix-map
;;           'xxx-:mode-prefix-key
;;
;;           'xxx-:easymenu
;;           'xxx-:easymenu-name
;;           "Programming help menu."
;;           (list
;;            xxx-:easymenu-name
;;            ["Eval whole buffer" xxx-eval-current-buffer    t]
;;            ..)
;;           '(progn
;;              (define-key map "-" 'xxx-eval-current-buffer)
;;              (define-key map "=" 'xxx-calculate)
;;              ..))

;;}}}

;;; Change Log:

;;; Code:

;;{{{ require

;;; ......................................................... &require ...

(require 'tinylibm)                     ;macro package

(eval-when-compile
  (ti::package-use-dynamic-compilation)
  (when (and (ti::xemacs-p)
             (byte-compiling-files-p))
    (message "\n\
  ** tinylib.el: [Note] It is safe to ignore Emacs dependant ange-ftp function
                 compilation errors.")))

(eval-and-compile

  (defvar generated-autoload-file) ;; See autoload.el
  (defvar flyspell-mode)

  (autoload 'vc-name         "vc-hooks")
  (autoload 'vc-file-getprop "vc-hooks")

  ;;  Can't autoload timer, because various calls in this lib are behind
  ;;  ti::funcall --> Bytecompiler doesn't see them.

  (ti::package-package-require-timer)   ;XEmacs and Emacs differencies

  (cond
   ((ti::xemacs-p)

    ;;  Ange-ftp function used in this package won't work in XEmacs.
    ;;  The ange functions used for backgroung ftp downloads
    ;;  and low level calling calling of ange functions. Currently used in
    ;;  one pacakge: tinydired.el, which let's you donwload/upload
    ;;  files at the background.

    (require 'efs-auto nil 'noerr)
    (autoload 'read-passwd "passwd" "" t))

   ((ti::emacs-p)
    (defvar ange-ftp-process-result                 nil)
    (defvar ange-ftp-ascii-hash-mark-size           1024)
    (defvar ange-ftp-binary-hash-mark-size          1024)
    (defvar ange-ftp-process-busy                   nil)
    (autoload 'ange-ftp-process-handle-line         "ange-ftp")
    (autoload 'ange-ftp-get-process                 "ange-ftp")
    (autoload 'ange-ftp-ftp-name                    "ange-ftp")
    (autoload 'ange-ftp-real-file-name-as-directory "ange-ftp")
    (autoload 'ange-ftp-expand-dir                  "ange-ftp")
    (autoload 'ange-ftp-ftp-process-buffer          "ange-ftp")
    (autoload 'ange-ftp-set-binary-mode             "ange-ftp")
    (autoload 'ange-ftp-send-cmd                    "ange-ftp")
    (autoload 'ange-ftp-cd                          "ange-ftp")
    (autoload 'ange-ftp-raw-send-cmd                "ange-ftp"))))

;;}}}
;;{{{ setup: -- variables

;;; ....................................................... &v-private ...

(defconst ti::var-syntax-info
  '((?\  "Whitespace")
    (?-  "Whitespace")
    (?w  "Word")
    (?_  "Symbol, variables and commands")
    (?.  "Punctuation, separate symbols from one another")
    (?(  "Open parenthesis")
      (?)  "Close parenthesis")
    (?\" "String quote, string as a single token")
    (?\\ "Escape")
    (?/  "Character quote, only the character immediately following.")
    (?$  "Paired delimiter, like string quote, chars between are not suppressed")
    (?<  "Comment starter")
    (?>  "Comment ender")
    (?@  "Inherit from standard syntax table"))
  "Short syntax definition table ((CLASS . DESC) ..).")

;;; ........................................................ &v-public ...
;;; User configurable

(defvar ti::var-x-coord 170
  "*Default X menu coordinate.")

(defvar ti::var-y-coord 170
  "*Default X menu coordinate.")

;; Make this invisible by default, note leading space.
(defvar ti::var-passwd-buffer " *passwd-entries*"
  "*Contents of password file.")

;;}}}
;;{{{ setup: -- version

;;; ....................................................... &v-version ...
;;; These are not library funcs, so they have normal 'tinylib-' prefix

(defconst tinylib-version
  (substring "$Revision: 2.107 $" 11 15)
  "Latest version number.")

(defconst tinylib-version-id
  "$Id: tinylib.el,v 2.107 2007/05/07 10:50:07 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylib-version (&optional arg)
  "Show version information. ARG will instruct to print message to echo area."
  (interactive "P")
  (ti::package-version-info "tinylib.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylib-submit-feedback ()
  "Submit suggestions, error corrections, impressions, anything..."
  (interactive)
  (ti::package-submit-feedback "tinylib.el"))

;;}}}

;;; ########################################################### &funcs ###

;;{{{ defsubst

;;; ........................................................ &defsubst ...
;;; inlined functions, they must be introduced before used

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-trim-blanks (string &optional middle)
  "Strip leading, trailing and middle spaces.
Input:

  MIDDLE  if non-nil, trim blanks in the middle too and convert
          tabs to spaces."
  (when (stringp string)
    ;;  Strip leading and trailing
    (if (string-match "^[ \t\f\r\n]*\\(.+[^ \t\f\r\n]\\)" string)
        (setq string (match-string 1 string)))
    (when middle
      ;; middle spaces
      (setq string (replace-regexp-in-string "[\t\r\n]" " " string))
      (setq string (replace-regexp-in-string "  +" " " string)))
    string))

;;; ----------------------------------------------------------------------
;;;
;;; (ti::string-verify-ends "Z" "\\." "." 'beg)
;;; (ti::string-verify-ends "dir" "/")
;;;
(defun ti::string-verify-ends (str re &optional add-str beg)
  "Make sure STR match RE and add ADD-STR string to it when necessary.
if ADD-STR is not given, adds RE to the string.

Default is to check end of string, Optionally BEG of string.
The RE may not include anchors.

Examples:
   making sure directory has ending slash
   (ti::string-verify-ends \"dir\" \"/\")       --> \"dir/\"

   Making sure, time is zero based:
   (ti::string-verify-ends \"7\" \"0\" nil 'beg) --> \"07\"

   This does not give you the rsult you assume!
   because the second parameter, \"  \", is regexp that is tried.
   This function can't know that there is only \" \" space at front,
   since the regexp dind't match.

   (ti::string-verify-ends \" padd\" \"  \" nil 'beg)
   --> \"   padd\"

Return:
  str    possibly modified"
  (let* ((RE  (if beg
                  (concat "\\`" re)
                (concat re "\\'")))
         (add (or add-str re)))         ;which one to add.
    (if (string-match RE str)
        str
      (if beg
          (concat add str)
        (concat str add)))))

;;; ----------------------------------------------------------------------
;;; - Originally there was own function for this; but now
;;;   it uses general func verify...
;;; - The main purpose of this function is that when you cat words
;;;   together, you can be sure they have COUNT spaces.
;;; - kinda sprintf...
;;;
(defsubst ti::string-add-space (str &optional end count)
  "Add space to the beginning of STR if there isn't one.
Optionally adds to the END. COUNT is by default 1

If string length is 0, do nothing."
  (let* ((count  (or count 1))
         (padd   (make-string count ?\ )))
    (ti::string-verify-ends str padd padd (not end))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-remove-whitespace (string)
  "Squeezes empty spaces around beginning and end of STRING.
If STRING is not stringp, then returns STRING as is."
  (when string
    (replace-regexp-in-string
     "^[ \t\r\n]+" ""
     (replace-regexp-in-string
      "[ \t\r\n]+\\'" "" string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-mangle (string)
  "Mangle STRING ie. make STRING unreadable.
Same mangling is performed for the same STRING. Mangling can't be reversed."
  (let* ((ch-list (coerce string 'list))

         ;; (coerce list 'string) to get list of ints to string

         (abc "zaybxcwdveuftgshriqjpkolnm0918273645ZAYBXCWDVEUFTGSHRIQJPKOLNM")
         (len (length abc))
         (ret "")
         x)
    (dolist (ch ch-list)
      (setq x (% (char-to-int ch) len))
      (setq ret (concat ret (substring abc x (1+ x)))))
    ret))

;;; ----------------------------------------------------------------------
;;; #todo: Use replace-regexp-in-string
;;;
(defsubst ti::string-regexp-delete (re str &optional level)
  "Remove all that match RE from STR at subexpression LEVEL."
  (while (string-match re str)
    (setq str (ti::replace-match (or level 0) nil str)))
  str)

;;}}}
;;{{{ Conversions

;;; ##################################################### &Conversions ###

;;; ----------------------------------------------------------------------
;;; Try
;;;     (setq str "%s")
;;;     (message str)          ;; suppose you don't know what's in there
;;;
;;; and you get error... use (message (ti::string-format-percent str))
;;;
(defun ti::string-format-percent (str)
  "Convert STR to message string, doubling diffucult charactes, like % and \\."
  (let* ((len  (length str))
         (i    0)
         (ret  str)
         ch-string
         extra
         ch)
    (cond
     ((string-match "[%\\]" str)        ;only now do
      (setq ret "")
      (while (< i len)
        (setq ch        (aref str i)
              ch-string (char-to-string ch)
              extra     "")
        (if (char= ch ?%)
            (setq extra ch-string))
        (setq ret (concat ret ch-string extra))
        (incf i))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-url-to-ange-ftp (str)
  "Converts URL STR into ange ftp address.

Eg:
   ftp://some.site
   ftp://some.site/pub/users/foo/emacs/some.el
   ftp://some.site:/pub/users/foo/emacs/some.el
   ftp://ftp@some.site/pub/users/foo/emacs/some.el
   ftp://ftp@some.site:/pub/users/foo/emacs/some.el

   -->
   /ftp@some.site:/
   /ftp@some.site:/pub/users/foo/emacs/some.el

Return:
  string
  nil"
  (let* (ref
         idx
         login
         host
         dir
         ret)
    (cond
     ( ;;
      (string-match "ftp:/\\(/.*@\\)\\([^/]+:\\)\\(/.*\\)" str)
      (setq login (match-string 1 str) ;; case 3
            host  (match-string 2 str)
            dir   (match-string 3 str)
            ret   (concat login host dir)))
     ( ;;
      (and  (string-match "ftp:/\\(/.*@\\)\\(.*\\)" str)
            (setq login (match-string 1 str) ;; case 4
                  ref   (match-string 2 str)))
      (setq idx  (ti::string-index ref ?/ ))
      (setq host (or host (substring ref 0 idx)))
      (setq dir  (substring ref idx))
      (setq ret (concat (or login "/ftp@") host ":" dir)))
     ( ;; ftp://some.site/pub/users/foo/emacs/some.el
      (and (string-match "ftp://\\([^@/]+\\)\\(:?/.*\\)" str)
           (setq host (match-string 1 str)
                 dir  (match-string 2 str)))
      (setq ret (concat
                 "/ftp@" host
                 (if (ti::string-index dir ?:) "" ":") ;add colon if needed
                 dir)))

     ( ;; ftp://some.site
      (and (string-match "ftp://\\([^@:]+\\)$" str)
           (setq host (match-string 1 str)))
      (setq ret (concat "/ftp@" host ":/"))))

    ret))

;;; ----------------------------------------------------------------------
;;; #todo: there seems to be c-backslash-region
;;;
(defun ti::buffer-backslash-fix-paragraph
  (&optional target-column stop-func verb)
  "Fix \\ lines in current paragraph.
The first \\ Tells what the target column is.  If there is no \\ anywhere
in the paragraph, this function does nothing.

Input:

  TARGET-COLUMN         position backslashes to this column, if possible.
                        if \\[universal-argument] or negative number,
                        remove backslashes.
  STOP-FUNC             If this function returns non-nil, then stop adding
                        backslashes. It is called prior the line is handled.
  VERB                  Verbose messages.

Example 1:

  This is \\
  Here is another       ;; Note missing \\
  and \\                ;; Note, extra \\, should not be there

  Will be formatted as:

  This is \\            ;; Target column, when TARGET-COLUMN is nil
  Here is another \\    ;; Because the target-cool couldn't be set.
  end

Example 2:

  This is               ;; Ignored, no \\
  Here is another \\    ;; Target starts here
  And still..
  end

  Will be formatted as:

  This is
  Here is another \\
  And still..     \\    ;; Added
  end

Example 3:

All the lines in this procmail example are together, but it would be wrong
to add final \\ to the end of ')'. The STOP-FUNC can make sure about that.

{
   :0 h # this is procmail code
   * condition
   | ( formail -rt    \\
       MORE-OPTIONS | \\
       $SENDMAIL -t)
}"
  (interactive "*P")
  (let* ((point   (point))
         (cs      (or comment-start "[ \t]*"))
         (stop-re (format "^\\(%s\\)?[ \t]*$" cs)) ;Paragraph end
         (kill-it (or (ti::listp target-column)
                      (and (integerp target-column)
                           (< target-column 0))))

         indent-tabs-mode               ;No tabs allowed
         beg
         col-target
         col-now
         col-word
         ad-it)
    (ti::verb)
    ;; ............................................... paragraph start ...
    (beginning-of-line)
    (while (and (not (eobp))
                (not (looking-at stop-re)))
      (forward-line -1))
    ;; .................................... forward to first backslash ...
    ;;  Skip comment lines and emtuy line forward.
    (while (and (not (eobp))
                (looking-at stop-re))
      (forward-line 1))
    (when (eq major-mode 'makefile-mode)
      (if (looking-at ".*:") ;;  Go path the TARGET: RULE
          (forward-line 1)))
    ;; ... ... ... ... ... ... ... ... ... ... .. &starting target-col ...
    (save-excursion                     ;Find the starting \\
      (beginning-of-line)
      (while (and (not (eobp))
                  (not (looking-at ".*[\\][ \t]*$"))
                  (not (looking-at stop-re)))
        (forward-line 1))
      (setq beg (point)))
    (goto-char beg)                     ;We landed here
    (cond
     ((not (looking-at ".*[\\]"))
      (message "\
Fix backslash: Nothing to do; no \ mark at the paragraph beginning."))
     (t
      (goto-char (match-end 0))
      (backward-char 1)
      (setq col-target (or (and
                            ;; User gave this value
                            (integerp target-column)
                            (>= target-column 0)
                            target-column)
                           (current-column))) ;; use column from code them
      (when kill-it
        (delete-char 1)
        (delete-horizontal-space))
      ;;  there was old starting \\, but not in the right column. Fix it,
      ;;  but only if it was far left.
      ;;
      ;;    txt txt \     ;; this line is too far right
      ;;      T \         ;; The target column user wanted was T
      (when (and (null kill-it)
                 (not  (eq (current-column) col-target)))
        (delete-region (point) (line-end-position))
        (move-to-column col-target)
        (when (or (null stop-func)
                  (and stop-func
                       (null (funcall stop-func))))
          (insert "\\")))
      (unless (looking-at "$")          ;Remove garbage
        (delete-region (point) (line-end-position)))
      (beginning-of-line)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. loop-lines . .
      ;;  Empty line terminates
      (while (and
              (not (eobp))
              (not (looking-at stop-re))
              (or (null stop-func)
                  (and stop-func
                       (null (funcall stop-func)))))
        (save-excursion                 ;Peek next line
          (forward-line 1)
          (setq ad-it (not (looking-at stop-re))))
        ;; ... ... ... ... ... ... ... ... ... ... ... fix backslashes ...
        (cond
         (kill-it
          (when (looking-at ".*[\\]")
            (goto-char (match-end 0)) (backward-char 1)
            (delete-char 1)
            (delete-horizontal-space)))
         ((looking-at ".*[\\]")
          (goto-char (match-end 0)) (backward-char 1)
          (setq col-now (current-column))
          ;;  Where is the word start?
          (skip-chars-backward " \t\\")
          (untabify (point) (line-end-position))
          (setq col-word (current-column))
          (cond
           ((and (eq col-now col-target)
                 (null ad-it))
            ;;  remove Extra  \\
            (move-to-column col-now)
            (delete-char 1)
            (delete-horizontal-space))
           ((not (eq col-now col-target))
            ;;    This \
            ;;    GFile.here \   < This is further right
            (cond
             ((> col-word col-target))  ;Do nothing, can't "line up"
             (t
              (move-to-column (min col-target col-now))
              (delete-region (point) (line-end-position))
              (when ad-it
                (ti::buffer-move-to-col col-target)
                (insert "\\")))))))
         ;; ... ... ... ... ... ... ... ... ... ... .. no-continuation  ..
         (ad-it                   ;No previous "\" and next line exist
          (end-of-line)
          (delete-horizontal-space)     ;Clear the EOL
          ;;  Only if there is no text, T is target, but next line has
          ;;  longer line.
          ;;
          ;;       T
          ;;  This rule here \
          (if (<= (current-column) col-target)
              (ti::buffer-move-to-col col-target))
          (insert "\\")))
        (forward-line 1))))
    (goto-char point)                   ;Restore user position
    (when verb
      (cond
       (kill-it
        (message "Fix backslash: backslashes removed."))
       (col-target
        (message
         "Fix backslash: backslashes in column %d" col-target))))))

;;; ----------------------------------------------------------------------
;;; - in many C/C++ styles the variables are names so that they start
;;;   with lowercase letters and following ones are catenated + first char
;;;   in upcase.
;;; - Function names may start with uppercase.
;;;
;;;
(defun ti::buffer-upcase-words-to-variable-names (beg end &optional case-fold)
  "Does following conversion by searhing caps only words in region.

  THE_COLUMN_NAME  --> theColumnName

Input:

  BEG END       region bounds
  CASE-FOLD     the value of `case-fold-search'. nil means that  the
                upcase \"words\" are counted only. Non-nil accepts
                seearching mixed case words."
  (interactive "*r\nP")
  (let* ((case-fold-search      case-fold) ;; case is significant..
         (ptable                (syntax-table)) ;; previous
         (table                 (make-syntax-table))
         f1
         f2)
    (save-restriction
      (unwind-protect
          (progn
            (narrow-to-region beg end)
            (ti::pmin)
            ;;  let's make sure the _ is not in a word class, put it
            ;;  into some other class for now.

            (modify-syntax-entry ?_ "_" table)
            (set-syntax-table table)
            (while (re-search-forward "[A-Z][A-Z_]+" nil t)
              (setq beg (match-beginning 0)
                    end (match-end 0))
              (save-excursion
                (setq f1 (looking-at "[ \t]\\|$"))
                (goto-char (1- beg))
                (setq f2 (looking-at "[ \t]\\|$")))
              (cond
               ((and f1 f2)
                ;; make first word "lowercase only"
                (goto-char beg)
                (downcase-word 1)
                ;; handle next words, until space/eol/eob is seen
                (while (and (not (eobp))
                            (not (looking-at "[ \t]\\|$")))

                  ;; Remove that underescore
                  ;; Capit. command moves forward while doing

                  (and (looking-at "_")
                       (delete-char 1))
                  (capitalize-word 1)))))
            ;; ... ... ... ... ... ... ... ... ... ... ... .. unwind end . .
            ;;  Now, make sure the old table is restored,
            ;;  the unwind protects against Ctrl-g
            (set-syntax-table ptable))))
    ;; let-defun end
    nil))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::string-nth-from-number (nbr)
  "Return string representing NBR position: st, nd, th.

Input:
  string or number in digit form.

Return:
  \"st\", \"nd\", \"th\""
  (if (stringp nbr)
      (setq nbr (string-to-int nbr)))
  (cond
   ((eq nbr 1) "st")
   ((eq nbr 2) "nd")
   ((eq nbr 3) "rd")
   ((>  nbr 3) "th")
   (t
    (error "invalid ARG" nbr))))

;;; ----------------------------------------------------------------------
;;; #todo
;;; - Did 19.29+ change the current-time function? Oh my...say no?
;;;   --> should handle it if the format changed.
;;;
(defun ti::date-time-elements (&optional zero-form time-string)
  "Return list of elements derived from `current-time'.
This is old function, you should use newer `format-time-string'.

Input:

  ZERO-FORM     make sure numbers have preceeding zeroes. Like 7 --> 07
  TIME-STRING   user supplied time string in `current-time' format.

Return list form: \( dd mm ...\)

  0 dd     nbr,  day                  if zero-form: ti::string-value
  1 mm     nbr,  month                if zero-form: ti::string-value
  2 yy     2nbr, year
  3 tt     2nbr, hh:mm
  4 wd     3str, week day, string like 'Mon'
  5 m      str,  month, full string
  6 yyyy   4str, whole year"
  (interactive)
  (let (time m mm dd yy tt wd yyyy)
    (setq time (or time-string
                   (current-time-string))) ;"Wed Oct 14 22:21:05 1987"
    (setq wd (substring time 0 3))
    (setq m  (substring time 4 7))
    (setq mm (or (ti::date-month-to-number m) 0))
    ;;    we remove trailing space  "2 " --> 2 --> "2"
    (setq dd (string-to-int (substring time 8 10)))
    (setq tt (substring time -13 -8))
    (setq yy (substring time -2 nil))
    (setq yyyy (substring time -4 nil))
    (cond
     (zero-form                         ;convert "7" --> "07"
      (setq dd (int-to-string dd))
      (setq mm (int-to-string mm))
      (if (not (eq (length dd) 2))
          (setq dd (concat "0" dd)))
      (if (not (eq (length mm) 2))
          (setq mm (concat "0" mm)))))
    (list dd mm yy tt wd m yyyy)))

;;; ----------------------------------------------------------------------
;;; - This is mainly used, if you read the regexp from the buffer:
;;;   obviously you can't just pick it from there:
;;;
;;;             "find this\t+"
;;;                       ^^
;;;   and use it in re-search-XXX commands. See function  ti::buffer-get-re
;;;   which does the conversion automatically by calling these functions.
;;;
(defun ti::string-char-to-escape-char (item)
  "Converts ITEM to escape sequence \"t\" --> \"\\t\".

Input:

  item       integer, character, or single string

Return:

  nil        if cannot identify ITEM.
  string     escape char"
  (let* (el ret
            (table
             '(("a" . 7)
               ("b" . 8)
               ("f" . 12)
               ("n" . 10)
               ("r" . 13)
               ("t" . 9)
               ("v" . 11))))
    (if (integerp item)
        (setq item (char-to-string item)))
    (if (setq el (assoc item table))
        (setq ret (char-to-string (cdr el))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-plain-string-to-regexp (str)
  "Convert slashes in STR \\\ --> \.
If you read from buffer two some special characters, it can't be
used like that right a way for regexp. E.g. in buffer \\\\ two slashes mean
one slash actually when assigned to string to form the regexp."
  (let* ((ret           "")
         (i             0)
         (len           (length str))
         (look-ch       ?\\)
         (prev-ch       ?d)             ;just some dummy
         (count         0)
         chs
         ch)
    (while (< i len)
      (setq ch      (aref str i)
            chs     (char-to-string ch))
      (if (eq ch look-ch)               ;add counter when EQ
          (incf count))
      (cond
       ((eq count 2)                    ;two successive ?
        (if (eq prev-ch look-ch)
            (setq count 0)              ;delete second
          (setq ret (concat ret chs))
          (setq count 0)))
       ((eq count 1)
        (if (eq ch look-ch)
            ;;  Right now it was found
            (setq ret (concat ret chs))
          ;; - Count is still 9, but we aren't looking at double \\ ?
          ;;   --> there is \t sequence
          ;; - we revove last char and put our sequence instead
          (setq ret (concat
                     (substring ret 0 (1- (length ret)))
                     (ti::string-char-to-escape-char chs)))
          (setq count 0)))
       (t
        (setq ret (concat ret chs))))
      (setq prev-ch ch )
      (incf i))
    ret))

;;; ----------------------------------------------------------------------
;;; arc.mode.el  -- This is from 19.28 distrib.
;;;
(defun ti::file-access-mode-to-string (mode)
  "Turn an integer MODE, 0700 (i.e., 448) into a mode string like -rwx------."
  (let ((str (make-string 10 ?-)))
    (or (zerop (logand 16384 mode)) (aset str 0 ?d))
    (or (zerop (logand  8192 mode)) (aset str 0 ?c)) ; completeness
    (or (zerop (logand   256 mode)) (aset str 1 ?r))
    (or (zerop (logand   128 mode)) (aset str 2 ?w))
    (or (zerop (logand    64 mode)) (aset str 3 ?x))
    (or (zerop (logand    32 mode)) (aset str 4 ?r))
    (or (zerop (logand    16 mode)) (aset str 5 ?w))
    (or (zerop (logand     8 mode)) (aset str 6 ?x))
    (or (zerop (logand     4 mode)) (aset str 7 ?r))
    (or (zerop (logand     2 mode)) (aset str 8 ?w))
    (or (zerop (logand     1 mode)) (aset str 9 ?x))
    (or (zerop (logand  1024 mode)) (aset str 3 (if (zerop (logand 64 mode))
                                                    ?S ?s)))
    (or (zerop (logand  2048 mode)) (aset str 6 (if (zerop (logand  8 mode))
                                                    ?S ?s)))
    str))

;;; ----------------------------------------------------------------------
;;; See also convert-standard-filename which e.g. changes forward slashes
;;; to backward slashes under win32.
;;;
(defun ti::file-name-for-correct-system (path system)
  "Convert PATH to correct system: 'emacs, 'dos or 'cygwin.

Input:

PATH        Path name. This must already be in expanded form.
            Use Emacs function `expand-file-name' as needed.

SYSTEM      'cygwin => convert to cygwin path notation
            'dos    => convert to DOS notation.
            'emacs  => convert to notation which current Emacs uses.
                       If running Win32 native Emacs, convert to DOS.
                       If running Cygwin Emacs, convert to cygwin.

Notes:

  In native Win32 Emacs, the choice 'emacs work correctly only if package
  cygwin-mount.el is active. The cygwin path are handled by it."
  (when (stringp path)
    (when (string-match "~\\|\\.\\." path) ;; Need absolute path
      (setq path (expand-file-name path)))
    (cond
     ((eq system 'emacs)
      (setq path (w32-expand-file-name-for-emacs path))
      (let ((func 'cygwin-mount-substitute-longest-mount-name))
        (when (and (ti::emacs-type-win32-p)
                   path
                   (and (string-match "^/" path))
                   (fboundp func))
          ;;  Need to convert Cygwin => DOS path
          (setq path (funcall func path)))))
     ((eq system 'cygwin)
      (setq path (w32-expand-file-name-for-cygwin path)))
     ((eq system 'dos)
      (if (string-match "^/cygdrive/" path)
          (setq path (w32-cygwin-path-to-dos path))))))
  path)

;;}}}

;;{{{ Version control, RCS delta files

;;; ....................................................... &rcs-delta ...
;;; In general, do not use these function, but use the top-level ones
;;; that deal with filenames or buffers.

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-delta-get-revisions  (&optional buffer)
  "Parse all revision numbers from delta file BUFFER.

Return:
  '(version version ..)"
  (let* (list)
    (save-excursion
      (if buffer
          (set-buffer buffer))
      (ti::pmin)
      (while (re-search-forward "^[0-9.]+[0-9]+$" nil t)
        (push (match-string 0) list)))
    ;; preserve order
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-delta-get-file (file buffer)
  "Read delta FILE to temporary BUFFER.
The delta file is truncated to header info only.

Input:

 FILE       RCS file
 BUFFER     Existing buffer where to put delta.

Errors:

  VC Generates error if file is not vc registered.

Return:

  buffer  Possibly newly created buffer."
  (let* ((rcs-name   (vc-name file))) ;; CVS returns entries.
    (if (or rcs-name
            (error "Not an RCS file. %s" file))
        (with-current-buffer buffer
          (erase-buffer)
          (if (fboundp 'vc-insert-file) ;19.30
              (ti::funcall 'vc-insert-file rcs-name "^desc")
            (insert-file-contents rcs-name)
            (buffer-disable-undo)
            (set-buffer-modified-p nil)
            (auto-save-mode nil)
            (if (re-search-forward "^desc" nil t)
                (delete-region (point) (point-max))))))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-delta-lock-status (&optional user-name)
  "Return lock status by reading the delta buffer.
If USER-NAME is non-nil return locks only for that user.

Return:
 ((USER . (VER VER ..))  (U . (V V)) ..)
 nil"
  (let (user
        ver
        ret)
    (save-excursion
      (ti::pmin)
      ;; locks
      ;;       jaalto:1.13; strict;
      ;; comment        @; @;
      (when  (re-search-forward "^locks" nil t)
        (forward-line 1)
        (while (re-search-forward
                "^[ \t]+\\([^:]+\\):\\([^;\n\r]+\\)"
                nil t)
          (setq user (ti::remove-properties (match-string 1))
                ver  (ti::remove-properties (match-string 2)))
          (if (or (null user-name)
                  (ti::string-match-case (regexp-quote user-name) user))
              (cond
               ((assoc user ret)        ;already a user in list
                (ti::assoc-append-inside 'assoc user ret ver))
               (t
                (if (null ret)
                    (setq ret (list (cons user (list ver))))
                  (push (cons user (list ver)) ret ))))))
        (forward-line 1)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-delta-lock-status-user (user)
  "Return list of locks for USER.
This is toplevel function to `ti::vc-rcs-delta-lock-status'.
Please use it directly if you want other users information too.
If you only need *one* users information, use this function, because
it hides the lock data structure.

Return:
 (VER VER ..)    ,list of version strings.
 nil"
  ;; this always parses the buffer.
  (cdr-safe (assoc user (ti::vc-rcs-delta-lock-status))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-delta-highest-version ()
  "Return the highest version from delta buffer."
  (interactive)
  (save-excursion
    (ti::pmin)
    (if (re-search-forward "head[ \t]+\\([.0-9]+\\)" nil t)
        (match-string 1))))

;;}}}
;;{{{ Version control, general

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-dir-p (file-or-dir)
  "Check if FILE-OR-DIR looks like version controlled.
Return type: 'rcs, 'cvs, 'monotone, 'subversion 'git' 'bzr' 'hg' or 'arch.
Note, the return value is LIST."
  (let ((dir (cond
              ((file-directory-p file-or-dir)
               file-or-dir)
              ((or (file-name-directory file-or-dir)
                   (let ((buffer (or (get-buffer file-or-dir)
                                     (get-file-buffer file-or-dir)
                                     (find-buffer-visiting file-or-dir))))
                     (and buffer
                          (file-name-directory
                           (buffer-file-name buffer))))))))
        (check '(("CVS/Entries" cvs)
                 (".svn"    subversion)
                 ;; #todo: Correct these
                 (".git"   git)
                 (".hg"   hg)
                 (".bzr" bzr)
                 ("MT"   monotone)
                 ("arch" arch)))
        ret)
    (setq dir (file-name-as-directory dir))
    (dolist (elt check)
      (multiple-value-bind (try type) elt
        (setq try (concat dir try))
        (if (or (file-exists-p try)
                (file-directory-p try))
            (push type ret))))
    ret))

;;}}}
;;{{{ Version control, string, RCS information
;;; ............................................................. &rcs ...
;;; Refer to GNU RCS ident(1) how to construct valid identifiers.

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-read-val (str)
  "Cleans the RCS identifiers from the STR and return the value."
  (let* ((re ".*[$][^ \t]+: \\(.*\\) [$]"))
    (if (and (stringp str)
             (string-match re str))
        (match-string 1 str)
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-look-id (str)
  "Return the RCS identifier in STR."
  (let* ((re ".*[$]\\([^ \t]+\\): .* [$]"))
    (if (string-match re str)
        (match-string 1 str)
      nil)))

;;}}}
;;{{{ Version control, CVS

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-cvs-to-cvs-dir (file)
  "Return CVS directory for file."
  (concat (file-name-directory file) "CVS"))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-cvs-to-cvs-dir-p (file)
  "Check if there is CVS directory for file. Return CVS path if CVS exist."
  (let* ((path (ti::vc-cvs-to-cvs-dir file)))
    (when (file-directory-p path)
      path)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-cvs-to-cvs-file (file cvs-file)
  "Use FILE or directory and return CVS/CVS-FILE, like `Root'.
If CVS-FILE does not exist, return nil."
  (let* ((path (ti::vc-cvs-to-cvs-dir file))
         (root (and path (concat path "/" cvs-file))))
    (when (and root
               (file-exists-p root))
      root)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-cvs-to-cvs-file-content (file cvs-file)
  "Use FILE or directory name as base and return contents of CVS-FILE as string."
  (let* ((file (ti::vc-cvs-to-cvs-file file cvs-file)))
    (when file
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-cvs-file-exists-p (file)
  "Return cvs-entry if FILE is in VCS controlled.
Look into CVS/Entries and return line from it if file was CVS controlled."
  (let* ((cvs-dir (ti::vc-cvs-to-cvs-dir-p file))
         cvs-file)
    (when (and cvs-dir
               (file-directory-p cvs-dir)
               (setq cvs-file (concat cvs-dir "/Entries"))
               (file-exists-p cvs-file))
      (with-temp-buffer
        ;;  CVS/Entries contain information on files in repository
        (ti::find-file-literally cvs-file (current-buffer))
        ;; /tinylib.el/1.1.1.1/Thu Dec 24 04:34:10 1998//
        (if (re-search-forward
             (concat "^/" (regexp-quote (file-name-nondirectory file)))
             nil t)
            (ti::read-current-line))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-cvs-entry-split (line)
  "Split cvs /Entries LINE into pieces.
/add-log.el/1.1.1.2.2.4/Wed Jan 05 11:25:14 2000//Tb20_4
D/calendar////"
  (when line
    (split-string line "/")))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-cvs-entry-type (line)
  "Return type 'dir or 'file for cvs /Entries LINE"
  (when line
    (cond
     ((string-match "^D/" line) 'dir)
     ((string-match "^/"  line) 'file) )))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-cvs-entry-split-info (info what)
  "Request information on the CVS Entries line INFO.
Input:

  INFO  list returned by `ti::vc-cvs-entry-split'
  WHAT  list of returned values: 'file 'revision 'time 'rest."
  (let* (ret)
    (dolist (type (ti::list-make what))
      (push (cond
             ((eq type 'file)     (nth 0 info))
             ((eq type 'revision) (nth 1 info))
             ((eq type 'time)     (nth 2 info))
             ((eq type 'rest)     (nth 4 info))
             ((error "Invalid WHAT arg %s" type)))
            ret))
    ;; preserve order.
    (nreverse ret)))

;;}}}
;;{{{ Version control, RCS

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-file-p (file)
  "Return t if FILE STRING is in RCS controlled form.
That is, if FILE has ,v at the end."
  (and (> (length file) 2)
       (string= (substring file -2) ",v")))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-make-filename (file &optional vc-subdir)
  "Constructs RCS controlled FILE name. VC-SUBDIR is by default RCS/.
FILE --> PATH/vc-subdir/FILE,v"
  (let* (ret
         fn
         dir)
    (cond
     ((ti::vc-rcs-file-p file)
      (setq ret file))
     (t
      (setq dir (or (file-name-nondirectory file) "./"))
      (setq fn  (file-name-directory file))
      (setq ret (concat dir (or vc-subdir "RCS/") fn ",v"))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-file-exists-p (file)
  "Return t if equivalent RCS FILE can be found.
If the following condition is met, then such file exists:
  ~/dir1/dir2/file.cc     --> ~/dir1/dir2/RCS/file.cc,v"
  (let* ((rcs (ti::vc-rcs-make-filename file)))
    (file-exists-p rcs)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-normal-file (rcs-file)
  "Return normal file when version controlled RCS-FILE is given."
  (let* (( case-fold-search nil))
    (when (ti::vc-rcs-file-p rcs-file)
      (setq rcs-file (replace-regexp-in-string "RCS/" "" rcs-file))
      (setq rcs-file (replace-regexp-in-string ",v"  "" rcs-file)))
    rcs-file))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-sort-same-level-list (list)
  "Sort RCS revision LIST, which are at same level.
Ie. when only the last version number changes:
1.1 1.2 1.3, or 1.2.1.1 1.2.1.3 1.2.1.10"
  (let* ((max 0)
         ptr
         new-list
         len
         ret
         padd
         str)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. greatest ...
    (dolist (nbr list)                  ;find greatest. 1.xx
      (setq max (max (length nbr) max)))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. padd ...
    (setq ptr list)
    (dolist (elt ptr)                   ;padd 1.1 --> 1.01
      (setq len (length elt))
      (unless (eq len max)
        (setq padd (make-string (- max len) ?0))
        (if (not (string-match "[0-9]+$" elt))
            (setq elt nil)              ;Invalid entry
          (setq str (match-string 0 elt) )
          (setq elt (ti::replace-match 0 (concat padd str) elt))))
      (if elt
          (push elt new-list)))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... .. sort ...
    (setq new-list (sort new-list 'string<))
    ;; Check if the values are doubled, and only then fix the list.
    ;; Hmm, if this happens, then the error is not in the 'sort'
    ;; but somewhere else.
;;;    (cond
;;;     ((and new-list (string= (nth 0 new-list)
;;;                          (nth 1 new-list)))
;;;      (setq new-list (ti::list-remove-successive new-list 'string=))
;;;      ))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... remove padd ...
    (setq ptr new-list)
    (dolist (elt ptr)                   ;fix 1.01 > 1.1
      (when (string-match "\\.\\(0+\\)[1-9][0-9]*$" elt)
        (setq elt (ti::replace-match 1 "" elt)))
      (push elt ret))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-files-in-dir (&optional dir re)
  "Return all RCS controlled files in directory DIR.
It doesn't matter if the directory points to RCS itself or
one level up. Thus the two DIR parameters are identical:

    /mydir/             takes longer to execute.
    /mydir/RCS/

The DIR parameter can hold filename, but only the directory portion
is used. If no directory portion exists \"./\" will be used.

Filenames returned do not have any  \",v\" extensions or directories.

Optional RE tells to return files matching RE only.

Return:
 list           (file file ..)"
  (let* ((re (or re "."))               ;default to match all
         d
         fn
         fnn
         list
         ret)
    (if (null (file-directory-p dir))
        (error "Not a directory"))
    (setq d (or (and dir
                     (or (file-name-directory (expand-file-name dir))
                         "./"))
                "./"))
    (cond
     ((ti::string-match-case "RCS/?" d)
      (setq list (directory-files d nil re))
      (dolist (elt list)
        (set fn (replace-regexp-in-string ",v$" "" elt))
        (push fn ret)))
     (t
      (setq list (directory-files d nil re))
      (dolist (fn list)
        (setq fnn (concat d fn))        ;with directory
        (if (and (not (file-directory-p fnn))
                 (ti::vc-rcs-file-exists-p (concat d fn)))
            (push fn ret)))))
    ret))

;;; ----------------------------------------------------------------------
;;; - The vc. does not return the _version_ latest.
;;;   See vc-hook/ vc-fetch-properties
;;;
(defsubst ti::vc-rcs-head-version  (file)
  "Get latest version, the head, for FILE.
No errors generates although file is not in RCS tree.

Return:
  string    version string
  nil       not an rcs file"
  (with-temp-buffer
    ;;  May not be RCS file
    (ignore-errors (ti::vc-rcs-delta-get-file file (current-buffer)))
    (ti::vc-rcs-delta-highest-version)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-guess-buffer-version  (file &optional user)
  "Try to guess right version number for buffer visiting FILE.
If file is locked, look at delta log to find out version, otherwise call
`ti::vc-rcs-buffer-version' and consult vc if needed.

Input:

  FILE      file name
  USER      rcs user name, defaults to (user-login-name)

Return:

  string
  nil"
  (let* ((user (or user (user-login-name)))
         list
         ver)
    (when (not buffer-read-only)        ;It's Checked Out
      ;; Never trust the ID string in the buffer, always look
      ;; at delta file --> this may be checked out with -k and
      ;; then RCS strings are not updated.
      (with-temp-buffer
        (ti::vc-rcs-delta-get-file file (current-buffer))
        ;; We're interested in current user's locks only
        (setq list (ti::vc-rcs-delta-lock-status user))))
    (cond
     ((and list
           (eq 1 (length list))
           (setq list (cdr (car list)))
           (eq 1 (length list)))
      ;; Okay, only 1 version locked, then we're safe
      (setq ver (car list)))
     (t
      (setq ver
            (or (save-excursion
                  (set-buffer (get-file-buffer file))
                  (ti::vc-rcs-buffer-version))
                (vc-file-getprop file 'vc-workfile-version)
                nil))))
    ver))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-buffer-version (&optional buffer)
  "Return version number for optional BUFFER.
Supposes that RCS string 'Revision' 'Id' or 'Log' exist.
If they do not exist, then see if VC is loaded and look at the modeline.

Please use `ti::vc-rcs-guess-buffer-version' and not this function."
  (let* (rev
         tmp)
    (save-excursion
      (if buffer
          (set-buffer buffer))
      (ti::widen-safe
        (ti::pmin)
        (cond
         ((setq tmp (ti::vc-rcs-str-find "Revision"))
          (setq rev (ti::vc-rcs-read-val tmp)))
         ((ti::vc-rcs-str-find "Log" )
          (forward-line)
          (setq rev (ti::buffer-match ".*Revision +\\([0-9.]+\\)" 1)))
         ((setq tmp (ti::vc-rcs-str-find "Id" 'value))
          (setq rev (nth 1 (split-string tmp " ")))))))
    ;;  See if VC is installed and ask from it then.
    (if (and (null rev)
             (fboundp 'vc-mode-line))
        (setq rev (ti::string-match  "[^.0-9]*\\([.0-9]+\\)" 1
                                     (or (symbol-value 'vc-mode) ""))))
    rev))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-rlog-get-revisions ()
  "REad all revision numbers from rcs rlog buffer.
The line searched looks like:

   revision 1.10   locked by: loginName;
   revision 1.9

Return:

  list    revision numbers
  nil"
  (let* ((re   "^revision[ \t]+\\([.0-9]+\\)$")
         ver
         list)
    (save-excursion
      (ti::pmin)
      (while (re-search-forward re nil t)
        (if (setq ver (match-string 1))
            (push ver list))))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::vc-rcs-all-versions  (file)
  "Return string list of all version numbers for FILE."
  (with-temp-buffer
    (ti::vc-rcs-delta-get-file file (current-buffer))
    (ti::vc-rcs-delta-get-revisions)))

;;; ----------------------------------------------------------------------
;;; For big files this is real slow, since building up lists and
;;; sort the revisions is hard
;;;
(defun ti::vc-rcs-previous-version (version v-list)
  "Return previous version for FILE.
Do not call this function Often, since it may be quite time consuming.

Input:

  VERSION       ,lever as string, e.g. \"1.5\"
  V-LIST        ,all version numbers for file, order not significant.

Return:

  RCS tree      previous version
  1.5           1.4
  1.4           1.3
  1.3           1,2
    1.3.1.1     1.3
    1.3.1.2     1.3.1.1
  1.2           1.1
  1.1           nil"
  (let* (branch-list
         list
         tmp
         ret)
    (setq branch-list   (ti::vc-rcs-get-all-branches version v-list))
    (cond
     ((null branch-list)
      ;; record the error to *Message* buffer
      (message "Tinylib: [rcs] This level does not have version? %s" version))
     ;; after 1.1.1.1 we go up one level, to 1.1
     ((setq ret (ti::string-match"\\([.0-9]*\\).1.1$" 1  version)))
     (t
      (setq list branch-list    tmp nil)
      (dolist (elt list)
        (if (not (string= elt version))
            (setq tmp elt)
          (setq ret tmp)
          (return)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-get-all-branches (rev rev-list)
  "Return sorted braches, lowest first, at same revion level.

Input:

  REV           version number string
  REV-LIST      list of version numbver string

Example:

  if version is 1.2,     return all 1.x     branches
  if version is 1.2.1.1, return all 1.2.1.x branches"
  (let* (list
         val)
    (if (null val)                      ;Quiet XEmacs 19.14 ByteComp
        (setq val (ti::string-match ".*\\." 0 rev))) ;remove last number
    (setq
     list
     (ti::list-find rev-list
                    rev
                    ;;  - The count thing just makes sure we get
                    ;;    1.1  and 1.2  , not 1.1.1.1
                    ;;  - match makes sure that the start of the string is same
                    ;;    1.  --> 1.2 1.3 1.4
                    (function
                     (lambda (arg elt)
                       (and (eq (count-char-in-string ?. arg)
                                (count-char-in-string ?. elt))
                            (string-match val elt))))
                    'all-matches))
    (when list
      ;; Simple (setq list (sort list 'string<)) won't do the job,
      ;; since it claims 1.10 is before 1.9
      ;;
      ;; 1.1
      ;; 1.10           ;; see ?
      ;; 1.2
      ;; 1.9
      (setq list (ti::vc-rcs-sort-same-level-list list)))
    list))

;;}}}
;;{{{ Version control, buffer's RCS strings, other

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-version-string-p (version)
  "Test if VERSION looks like version number N.N, N.N.N etc."
  (and (stringp version)
       (string-match "^[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)*$" version)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-version-simple-p (version)
  "test if VERSION is simple N.N; N.N.N would be complex."
  (and (stringp version)
       (eq 1 (count-char-in-string ?. version))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-version-lessp (a b &optional zero-treat)
  "Return t if A is later version than B.
This function can only check only three levels; up till: NN.NN.NN.

Examples:

  2     > 1.1
  1.11  > 1.3
  1.3.1 > 1.1

Input

  A             Version string one
  B             Version string two
  ZERO-TREAT    If non-nil, consider version numbers starting with 0.NN
                never than 2.1. In this case it is assumed
                that zero based versions are latest development releases."
  (flet ((version (str regexp)
                  (if (string-match regexp str)
                      (string-to-number (match-string 1 str))
                    0)))
    (let* ((a1 (version a "^\\([0-9]+\\)"))
           (a2 (version a "^[0-9]+\\.\\([0-9]+\\)"))
           (a3 (version a "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"))
           (b1 (version b "^\\([0-9]+\\)"))
           (b2 (version b "^[0-9]+\\.\\([0-9]+\\)"))
           (b3 (version b "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)")))
      (or (and zero-treat
               (and (= a1 0)
                    (> b1 0)))
          (> a1 b1)
          (and (= a1 b1)
               (> a2 b2))
          (and (= a1 b1)
               (= a2 b2)
               (> a3 b3))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::vc-rcs-str-find (str &optional mode)
  "Try to find rcs string STR starting from the point forward.

Return:

    By default whole string is returned.
    If MODE is non-nil, the value of rcs identifier is returned."

  ;;  RCS keywords are like this:
  ;;
  ;;        $ Revision:

  (let* ((re (concat "[$]" str ":[^$]+[$]"))
         ret)
    (if (null (re-search-forward re nil t))
        nil
      (setq ret (match-string 0))
      (if (null mode)
          ret
        (ti::vc-rcs-read-val ret)))))

;;; ----------------------------------------------------------------------
;;; - In fact this should be macro, defsubst
;;;
(defsubst ti::vc-rcs-str-find-buffer (str &optional mode)
  "Try to find rcs string STR starting from `point-min'.
Return:

    By default whole string is returned.
    If MODE is non-nil, the value of rcs identifier is returned.

Example call:

  (ti::vc-rcs-str-find-buffer \"Id\" 'value)"
  (save-excursion
    (ti::widen-safe
      (ti::pmin)
      (ti::vc-rcs-str-find str mode))))

;;}}}

;;{{{ Date

;;; ............................................................ &date ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::date-standard-rfc-regexp (&optional type time)
  "Return RFC date matching regexp: Feb  9 16:50:01.
Input:

  TYPE   \"mon\"  .. \"mon-date-hh-mm-ss\" What elements to inlcude.
  TIME   if not set, use `current-time'.

Note it makes no sense to request \"mon-mm\", because the return
value si cumulated. Do not leave out directived from the middle, but
tag in order:

  mon
  mon-date
  mon-date-hh
  mon-date-hh-mm
  mon-date-hh-mm-ss."
  (or time
      (setq time (current-time)))
  (let* ((mon  (format-time-string "%b" time))
         (dd   (ti::string-trim-blanks
                (format-time-string "%e" time)))
         (hh   (format-time-string "%H" time))
         (mm   (format-time-string "%M" time))
         (ss   (format-time-string "%S" time))
         ret)
    (cond
     ((not (stringp type))
      nil)
     (t
      (when (string-match "mon" type)
        (setq ret (concat (or ret "") mon)))
      (when (string-match "date" type)
        (setq ret (concat (or ret) " +" dd)))
      (when (string-match "hh" type)
        (setq ret (concat (or ret) " +" hh)))
      (when (string-match "mm" type)
        (setq ret (concat (or ret) ":" mm)))
      (when (string-match "ss" type)
        (setq ret (concat (or ret) ":" ss)))))
    ret))

;;; ----------------------------------------------------------------------
;;; #defalias  (defalias 'time-now 'ti::date-standard-date)
;;;

(when (fboundp 'format-time-string)     ;19.29+
  (defun ti::date-standard-date (&optional type time)
    "Return time RFC 'Nov 07 1995 20:49' or in SHORT
Input:
  TYPE  return YYYY-MM-DD instead (ISO 8601).
        if 'minutes, return YYYY-MM-DD HH:MM.
  TIME-STRING   User supplied string in format `current-time-string'."
    (cond
     ((eq 'minutes type)
      (format-time-string "%Y-%m-%d %H:%M" (or  time (current-time))))
     (type
      (format-time-string "%Y-%m-%d" (or  time (current-time))))
     (t
      (format-time-string "%b %d %Y %H:%M"
                          (or  time (current-time)))))))

;;; ---
(unless (fboundp 'format-time-string)
  (defun ti::date-standard-date (&optional type time)
    "Return Time 'Nov 10th 1995 20:49'.
Input:
  TYPE  return YYYY-MM-DD ISO 8601.
        if 'minutes, return YYYY-MM-DD HH:MM.
  TIME  User supplied time in format `current-time'."
    (interactive "P")
    (let* ((list  (ti::date-time-elements nil (current-time-string
                                               (or time (current-time)))))
           nbr)
      (cond
       (type
        (setq nbr (cdr (assoc (nth 5 list) (ti::month-mm-alist))))
        (concat
         (nth 6 list) "-"
         (int-to-string nbr)
         "-"
         (int-to-string (nth 0 list))
         (if (not (eq type 'minutes))
             ""
           (concat " " (nth 3 list)))))
       (t
        (concat (nth 5 list) " "
                (int-to-string (nth 0 list))
                (ti::string-nth-from-number  (nth 0 list)) " "
                (nth 6 list) " "
                (nth 3 list)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::date-month-to-number (arg &optional mode)
  "Return month number for string or vice versa.

When MODE is nil

  Accepts Jan or January with any case     --> Return nbr or nil

When MODE is non-nil

  Accepts nbr or str-nbr                   --> return str or nil"
  ;; (interactive)
  (let ((alist
         '(("jan" . 1)    ("feb" . 2)     ("mar" . 3)     ("apr" . 4)
           ("may" . 5)     ("jun" . 6)     ("jul" . 7)     ("aug" . 8)
           ("sep" . 9)     ("oct" . 10)    ("nov" . 11)    ("dec" . 12)))
        len
        idx
        el
        ret
        str)
    (cond
     ((eq nil mode)
      (setq len (length arg))
      (if (> len 3) (setq arg (substring str 0 3))) ; cut to 3 chars
      (setq idx (downcase arg))
      (if (setq el (assoc idx alist))
          (setq ret (cdr el))))
     (t
      (if (stringp arg) (setq arg (string-to-int arg)))
      (setq idx arg)
      (if (setq el (rassq idx alist))
          (setq ret (car el)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::date-time-difference (a b &optional float)
  "Calculate difference beween times A and B optionally in FLOAT seconds.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g. if you want to calculate days; you'd do
\(/ (ti::date-time-difference a b) 86400) ;; 60sec * 60min * 24h"
  (if float
      (progn
        (multiple-value-bind (s0 s1 s2) a
          (setq a (+ (* (float (ash 1 16)) s0)
                     (float s1) (* 0.0000001 s2))))
        (multiple-value-bind (s0 s1 s2) b
          (setq b (+ (* (float (ash 1 16)) s0)
                     (float s1) (* 0.0000001 s2))))
        (- a b))
    (let ((hi (- (car a) (car b)))
          (lo (- (car (cdr a)) (car (cdr b)))))
      (+ (lsh hi 16) lo))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::date-time-diff-days  (std1 std2)
  "Return approximation of time difference in days.
STD1 and STD2 are two standard times in short format YYYY-MM-DD.
In calculation each month is supposed to have 30 days and a year 356 days."
  (let ((re  "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9]+\\)-\\([0-9]+\\)")
        y1 m1 d1
        y2 m2 d2
        ret)
    (string-match re std1)
    (setq y1 (string-to-int (match-string 1 std1))
          m1 (string-to-int (match-string 2 std1))
          d1 (string-to-int (match-string 3 std1)))
    (string-match re std2)
    (setq y2 (string-to-int (match-string 1 std2))
          m2 (string-to-int (match-string 2 std2))
          d2 (string-to-int (match-string 3 std2)))
    (if (>= (- d2 d1) 0)                ;day2 is smaller
        (setq ret (- d2 d1))
      (setq ret (- (+ 30 d2) d1))
      (decf m2))
    (incf ret (* 30  (- m2 m1)))
    (incf ret (* 356 (- y2 y1)))
    ret))

;;; ----------------------------------------------------------------------
;;; Try this:  (ti::date-parse-date "Wed, 21 Jul 93 09:26:30 EST")
;;;
(defun ti::date-parse-date (str)
  "Try to parse date field.

Return:

  list          ,(dd mm yy tt wd m yy tz)
                 \"\" in fields which weren't identified.

  list members:
  0 YYYY   year         4 numbers
  1 mm     month        number
  2 dd     day          number
  3 tt     hh:mm        nbr:nbr
  4 wd     week day     string e.g. \"Mon\"
  5 m      month        string e.g. \"Jun\"
  7 tz     time zone    e.g. [+-]nnnn, where n = number"
  (let* (wd
         dd
         mm
         m
         yyyy
         tt
         tz

         (rAaa   "\\([A-Z][a-z][a-z]\\)")
         (rd     "\\([0-9][0-9]?\\)")          ;; typical day nbr
         (rd4    "\\([0-9][0-9][0-9][0-9]\\)") ;; typical year nbr (regexp day)
         (rt     "\\([0-9]+:[0-9:]+\\)")       ;; time
         ;; UTC+2  GMT+2
         (rz     "\\([+-][0-9]+\\|[A-Z][A-Z][A-Z]+[^ \t\n]*\\)?") ;; timezone
         (re-yyyy
          (concat rd4 " +" rt)) ;; 1994 08:52:25
         (re-yy
          (concat rd " +" rt)) ;; 94 08:52:25
         (re-wd
          (concat rAaa ",? +" rd " +" rAaa)) ;; weekday: Mon, 24 Oct
         (re-dd
          (concat rd ",? +" rAaa " +")) ;;  24 Oct
         ;;  (current-time-string) Wed Oct 14 22:21:05 1987
         (re-wd-4y
          (concat re-wd " +" re-yyyy " *" rz )) ;; Mon, 24 Oct 1994 08:52:25 +0200
         (re-wd-2y
          (concat re-wd " +" re-yy " *" rz )) ;; Mon, 24 Oct 94 08:52:25 +0200
         (re-dd-yyyy                          ;
          (concat re-dd re-yyyy " *" rz)) ; 24 Oct 1994 00:28:04 GMT
         (re-dd-yy
          ;; 24 Oct 94 00:28:04 GMT
          (concat re-dd re-yy " *" rz)))
    ;; Tue, 1 Nov 1994 8:52:36 +0300 (EET)
    (cond
     ((or (string-match re-wd-4y str)
          (string-match re-wd-2y str))
      (setq wd    (match-string 1 str)
            dd    (match-string 2 str)
            m       (match-string 3 str)
            yyyy  (match-string 4 str)
            tt    (match-string 5 str)
            tz    (match-string 6 str)))
     ;;  24 Oct 1994 00:28:04 GMT
     ((or (string-match re-dd-yyyy str)
          (string-match re-dd-yy str))
      (setq dd    (match-string 1 str)
            m     (match-string 2 str)
            yyyy  (match-string 3 str)
            tt    (match-string 4 str)
            tz    (match-string 5 str))))
    (when (and yyyy (eq (length yyyy) 2))
      (setq yyyy (concat
                  (if (string-match "^[789]" yyyy) "19" "20")
                  yyyy)))
    (when m
      (setq mm (format "%02d" (ti::date-month-to-number m))))
    (when dd
      (setq dd (format "%02d" (string-to-int dd))))
    (list yyyy mm dd tt wd m tz)))

;;}}}
;;{{{ string(s), chars

;;; ########################################################## &string ###

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'string-repeat 'ti::string-repeat)
;;;
(defun ti::string-repeat (count char-or-string)
  "Repeat COUNT times CHAR-OR-STRING."
  (let* ((i 0)
         ret)
    (if (characterp char-or-string) ;; XEmacs compatibility needed
        (setq char-or-string (char-to-string char-or-string)))

    (if (integerp char-or-string)
        (setq ret (make-string count char-or-string))
      (setq ret "")
      (while (< i count)
        (setq ret (concat ret char-or-string))
        (incf i)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-syntax-info (char &optional verb)
  "Return brief syntax definition string for CHAR. VERB."
  (interactive "cShow syntax of char: ")
  (let* ((syntax (char-syntax char ))
         (elt    (assq syntax ti::var-syntax-info))
         (verb   (or verb (interactive-p)))
         ret)
    (setq ret
          (concat
           (char-to-string syntax)
           " "
           (if elt  (nth 1 elt) "")))
    (if verb
        (message ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-syntax-kill-double-quote ()
  "Kill double quote string syntax class for current buffer.
This is usually useful when you turn on `font-lock' in current
buffer where there won't be equal amount of \" and ' pairs.
Your highlighting will then work as expected after syntaxes are killed."
  (interactive)
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "_" table)
    (set-syntax-table table)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-tabify (string &optional mode)
  "Tabify STRING, or if MODE is non-nil, untabify."
  (let* ((indent-tabs-mode t))          ;makes sure tabs are used.
    (with-temp-buffer
      (insert string)
      (if (null mode)
          (tabify (point-min) (point-max))
        (untabify (point-min) (point-max)))
      (buffer-string))))

;;; ----------------------------------------------------------------------
;;; - This is slightly different than the next one. Use the one you need.
;;;
(defun ti::string-match-string-subs (level-list &optional string terminate)
  "Return matcg list according to subexpression list LEVEL-LIST.

Supposes that you have already done the matching. If STRING is not
given, the buffer will be used for reading.

If optional TERMINATE is non-nil, terminates if any of the matches return
nil. In this case the return list will be empty signifying that all matches
weren't satisfied.

Input:
  level-list    list    e.g.   '(1 0 2)
  string        str     e.g.   \"testThis\"

Return:
 ( \"str\" nil \"str\" .. )
 nil                    ,see TERMINATE"
  (let* (ret
         str)
    (dolist (level level-list)
      (setq str (match-string level string))
      (if (and terminate (null str))
          (progn
            (setq ret nil)              ;that's it then...
            (return))
        (push str ret)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-match-string-list (match-list level-list string &optional terminate)
  "Return match list list according to subexpressions.

Input:

  MATCH-LIST    list    e.g.   '(\"\\(re1\\)\" \"re2\" \"\\(cash\\(re3\\)\\)\"
  LEVEL-LIST    list    e.g.   '(1 0 2)
  STRING        str     e.g.   \"re1 re2 cashre3\"
  TERMINATE     any     e.g.   nil, 'terminate

Supposes that you have already done the matching.

If the match wasn't found in current level, it assign nil to the
corresponding position in return list

If optional TERMINATE is non-nil, terminates if any of the matches return
nil. In this case the return list will be empty signifying that all matches
weren't satisfied.

Return:
 ( \"str\" nil \"str\" .. )
 nil                    ,see TERMINATE"
  (let* (ret
         str)
    (if (not (eq (length match-list)
                 (length level-list)))
        (error "List length mismatch."))
    (while level-list
      (setq str (ti::string-match (car match-list) (car level-list) string))
      (if (and terminate (null str))
          (setq ret nil   level-list nil) ;that's it then...
        (ti::nconc ret str))
      (pop level-list)
      (pop match-list))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-case-replace (model str &optional symmetry rest-case)
  "Use MODEL and change case of characters in STR.
Preserve case if SYMMETRY is non-nil.

E.g. If your input is:

        model:          BARMAN
        str  :          Foomanager

and the symmetry is non-nil, you get

        output:         FOOMANager

If the model is too short the variable REST-CASE instructs what to do

  nil      -->  the rest of the STR will be added \"as is\"
  'follow  -->  the rest of the STR are in the same case as last
                char in MODEL
  'lower   -->  insert rest as lowercase
  'upper   -->  insert rest as uppercase"
  (let* ((i         0)
         (part      "")
         case-fold-search               ;case is important
         last
         len
         ret
         ch
         ch-model)
    (if (null symmetry)
        str                             ;don't care
      (setq len (min (length str) (length model))
            ret "")
      ;; ............................................ MODEL characters ...
      (while (< i len)
        (setq ch-model  (char-to-string (aref model i))
              ch        (char-to-string (aref str i)))
        (cond
         ((string-match "[a-z]" ch-model)
          (setq ch (downcase ch)   last 'downcase))
         ((string-match "[A-Z]" ch-model)
          (setq ch (upcase ch)   last 'upcase))
         (t
          ;; MODEL has punctuation, choose previous case
          (if (eq last 'upcase)
              (setq ch (upcase ch))
            (setq ch (downcase ch)))))
        (setq ret (concat ret ch))
        (incf i))
      ;; ............................................. REST characters ...
      ;;  if MODEL is too short, then determine what to do to the rest
      ;;  of the characters theat are left.
      (when (< (length model) (length str)) ;Need to guess REST model?
        (setq part (substring str len))
        (cond
         ((eq rest-case 'follow)
          (setq ch (char-to-string (aref model (1- len)))) ;read last char
          (cond
           ((string-match "[a-z]" ch)
            (setq part (downcase part)))
           ((string-match "[A-Z]" ch)
            (setq part (upcase part)))
           (t
            ;; kast char was punctuation, choose last type
            (if (eq last 'upcase)
                (setq part (upcase part))
              (setq part (downcase part))))))
         ((equal rest-case 'upper)
          (setq part (upcase part)))
         ((equal rest-case 'lower)
          (setq part (downcase part)))))
      (setq ret (concat ret part))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-index (str char &optional reverse)
  "Check STR and first CHAR position 0..nbr.
If REVERSE is non-nil, start searching at the end of string."
  (let ((len (length str))
        (i   -1))
    (cond
     (reverse
      (while (and (>= (decf len) 0)
                  (/= (aref str len) char))) ;check character in string
      (if (>= len 0)
          len
        nil))
     (t
      (while (and   (< (incf i) len)
                    (/= (aref str i) char)))
      (if (< i len)
          i
        nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-index-substring (str char &optional include right seek-end)
  "Return left hand substring from STR maching CHAR.

Input:

  INCLUDE   The CHAR itself is included too.
  RIGHT     Return right hand portion.
  SEEK-END  Search from the end.

Example:

    ;; To get only the file part, you'd say

    (setq string \"user@site:~/bin/myfile\")
    (ti::string-index-substring string ?: nil 'right)

    ;; To get last item, separated by |

    (setq string \"aa|bb|cc|dd\")
    (ti::string-index-substring string ?| nil 'right 'seek-end)

Input:

  str           string
  char          character to look in string
  include       flag, should char be included too?
  right         return right side of string
  seek-end      start looking the position from the end instead

Return:

  str   if ch found
  nil   no ch found, or impossible condition. Like if input STR is \":\"
        and don't want to include ?: character."

  (let (idx
        ret)
    ;;   common mistakes, prevent it immediately, because
    ;;   looking the cause in debuffer may be a bit hairy, due to
    ;;   breakout only in ti::string-index

    (if (not (and str char))
        (error "parameter error %s %s" str char))
    (if (null (setq idx (ti::string-index str char seek-end)))
        nil
      (cond
       (right
        (setq ret (substring str
                             (if include
                                 idx
                               (1+ idx)))))
       (t
;;;     (ti::d! str include idx)
        (setq ret (substring str
                             0
                             (if include ;; the second parameter
                                 (1+ idx )
                               idx))))))
    (if (ti::nil-p ret)                 ;do not return empty strings
        nil
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::string-replace-one-space (str)
  "Convers all spaces/tabs in STR into one space."
  ;; #todo: Would using a temporary buffer + untabify + replace-regexps
  ;; be faster?
  (let* ((out "")
         beg
         end)
    (while (and (> (length str) 0)
                (string-match "[ \t]+\\|$" str))
      (setq beg (match-beginning 0) end (match-end 0))
      ;;  Take only 1 space (1+ ..
      ;;
      ;;  no more spaces ? , the "$" matched ...
      (if (eq beg (length str))
          (progn
            ;;  is the rest of it spaces ?
            (if (string-match "[ \t]+$" str) nil
              (setq out (concat out str)))
            (setq str ""))              ;found empty space
        (setq out (concat out (substring str 0 (1+ beg))))
        (setq str (substring str end))))
    out))

;;; ----------------------------------------------------------------------
;;; 17 Aug 1995, gnu.emacs.help, kevinr@ihs.com (Kevin Rodgers)
;;; - Slightly modified by jaalto
;;;
(defun ti::string-listify (string &optional sep)
  "Look STRING and search SEP [whitespace] and return list of substrings."
  (let ((start 0)
        (sep (or sep "[^ \f\t\n\r\v]+"))
        list)
    (while (string-match sep string start)
      (setq list
            (cons (substring string (match-beginning 0) (match-end 0))
                  list))
      (setq start (match-end 0)))
    (nreverse list)))

;;}}}
;;{{{ buffer: line, information, dired

;;; ........................................................ &ange-ftp ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::dired-buffer (dir)
  "Return dired buffer for DIR if any."
  (setq dir (file-name-as-directory dir)) ;; Dired uses trailing slash
  (dolist (buffer (buffer-list))
    (when (with-current-buffer buffer
            (and (eq major-mode 'dired-mode)
                 (string= dired-directory dir)))
      (return buffer))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-get-ange-buffer-list (&optional regexp)
  "Return list of ange-ftp buffers matching optional REGEXP."
  (ti::dolist-buffer-list
   (and (string-match "internal.*ange" (symbol-name major-mode))
        (string-match (or regexp "^[*]ftp") (buffer-name)))
   'temp-buffers))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-find-ange-buffer (user host)
  "Find ange ftp buffer with login USER running under HOST.

Return:

  buffer"
  (car-safe                             ;may be nil list
   (ti::buffer-get-ange-buffer-list
    (concat "^[*]ftp +" user "@" host "[*]"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-find-ange-to-dired-buffer ()
  "Find associated dired buffer for current ange-ftp buffer.

Return:

 list    list of possible buffers
 nil"
  (let* ( ;;      Check that we're in ange buffer "*ftp ..."
         (name   (ti::string-match "^[*]ftp +\\(.*\\)[*]" 1 (buffer-name))))
    (when name
      (ti::dolist-buffer-list
       (and (eq major-mode 'dired-mode)
            (string-match
             name (or (symbol-value 'dired-directory) "")))))))

;;; ........................................................ &uuencode ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-uu-area (&optional data-buffer buffer)
  "Find uuencoded region forward.

Input:

 DATA-BUFFER    Where to look, defaults to `current-buffer'.
 BUFFER         If non-nil, put uuencode data here.

Return:

  (beg . end)   list, the uu data area
  nil           no uu after point found"
  (let* ((case-fold-search  nil)        ;must use case sensitive
         (beg-re            "begin[ \t]+[0-9]+[ \t]+.")
         (end-re            "end[ \t]*$")
         beg end
         bol
         leading)
    (save-excursion
      (set-buffer (or data-buffer (current-buffer)))
      (and (re-search-forward beg-re nil t)
           (setq bol (line-beginning-position))
           (setq beg (match-beginning 0))
           (re-search-forward end-re nil t)
           (setq end (line-end-position))))
    (when (and beg end buffer)
      ;;  First get the data
      (with-current-buffer buffer
        (erase-buffer)
        (insert-buffer-substring data-buffer bol end)
        ;;  Remove possible leadings so that you can extract NEWS
        ;;  citated UUdata too
        ;;
        ;;  >  begin 0 cobol.el.gz
        ;;  >  M'XL("!?:;S```V-O8F]L+F5L`*P\:W/;1I*?Q;H?,4'MK@A%8"0GL9PH&Z\B
        (if (< (- beg bol) 1)           ;no leading characters.
            nil
          (setq leading (concat "^" (make-string (- beg bol) ?.)))
          (ti::pmin)
          (ti::buffer-replace-regexp leading 0 ""))
        (ti::pmax)
        (insert "\n")))
    (if (and beg end)
        (cons beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-uu-line-p (&optional string)
  "Determines if current line is UUencoded. Optionally check STRING.
The line is considered as an uu line if it has no lowercase chars and has
length more than 50 chars. Any leading spaces and tabs are skipped to find
the UU start [applies to buffer reading only].

Return length of line if it's UU, nil if not."
  ;; (interactive)
  (let* ((case-fold-search      nil)    ;case is important
         (at-least              50)
         line
         len
         ret)
    (cond
     ((setq line (or string (ti::buffer-read-if-solid)))
      (setq len  (length line))
      (if (and (not (string-match "[a-z]" line)) ;--> not UU line
               (> len  at-least))       ;must be longer than xx chars
          (setq ret len))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-area-bounds (beg end)
  "Search area bounds delimited by _strings_ BEG and END.
First searches backward, them forward.

Return:
  (beg-point . end-point)
  nil"
  (condition-case nil
      (let (p pp)
        (save-excursion
          (search-backward beg)
          (setq p (point))
          (search-forward end)
          (setq pp (point)))
        (if (< (point) pp) (cons p pp) nil))
    (search-failed
     nil)))

;;}}}

;;; ########################################################## &Buffer ###

;;{{{ buffer: reading lines, chars

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-parse-grep-line ()
  "Parse grep(1) formatted line. FILE:LINE:<content>.
Return:
  '(file line content)."
  (let* (file
         line
         rest)
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at "^[ \t]*\\([^:\r\n]+\\):\\([0-9]+\\):\\(.*\\)")
        ;; file:nbr:<rest>
        (setq file (match-string 1)
              line (match-string 2)
              rest (match-string 3)))
       ((looking-at "^[ \t]*\\([a-zA-Z]:[^:\r\n]+\\):\\([0-9]+\\):\\(.*\\)")
        ;; d:/home/path/file.txt
        (setq file (match-string 1)
              line (match-string 2)
              rest (match-string 3))))
      (when line
        (if (string-match "^[0-9]+$" line)
            (setq line (string-to-int line))
          (setq line nil)))
      (when file
        (list file line rest)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-parse-grep-line2 ()
  "Parse 'file nbr' format. Return '(file line)."
  (save-excursion
    (beginning-of-line)
    (when
        (or (looking-at "^[ \t]*\\([^ \t\n:]+\\)[ \t]+\\([0-9]+\\)[ \t:]+")
            (looking-at (concat ".*line[ \t,\n]+\\([0-9]+\\)[ \t,\n]+"
                                "file[ \t,\n]+\\([^ \t\n:)]+\\)")))
      (list
       (match-string 1)
       (match-string 2)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-parse-line-main ()
  "Find directory from the previous 'cd' command.
Look current line first and if it has no directory part,
search backward.

Line formats recognized are:

  FILE:LINE: results
  FILE LINE results

  Or the format can be following, where tokens can span multiple lines

  line LINE, file LINE results

Note:

  You should probably call `ti::file-name-for-correct-system' to convert
  the filename to current Emacs and OS. (Like reading Cygwin paths under
  native NT Emacs)

Return:

  (file line)         information
  nil                 not valid line"
  (let* ( ;;       (drive  "\\([a-zA-Z]:\\)?")
         (cd-re1 ".*cd +\\(.*\\)")
         (cd-re2 "^cd +\\(.*\\)")
         path
         elt
         line
         ret
         file)
    ;; ................................................ grep-format ...
    (when (setq elt (or (ti::buffer-parse-grep-line)
                        (ti::buffer-parse-grep-line2)))
      (setq file (nth 0 elt)
            line (nth 1 elt))
      ;; ..................................................... Paths ...
      (cond                             ;Unix, Dos paths
       ((save-excursion
          (and (null (string-match (concat "^/\\|^[a-z]:[\\/]") file))
               (or (looking-at cd-re1)
                   (re-search-backward cd-re2 nil t)))
          (setq path (match-string 1))))
       (buffer-file-name                ;Another condition
        ;; If we loaded erorr log file from the same directory: try it
        ;;
        ;;   weblint file.html > file.err
        ;;
        ;;   --> then load file.err into emacs and start jumping to errors.
        (setq path (file-name-directory buffer-file-name))))
      ;;  ./dir/file --> dir/file
      (if (and (stringp file)
               (string-match "^\\.[/\\]" file))
          (setq file (ti::replace-match 0 nil file)))
      (setq ret (list (if path
                          (ti::file-make-path path file)
                        file)
                      line)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-join-region (beg end)
  "Join the region BEG END into a single line."
  (interactive "*r")
  (save-excursion
    (goto-char end)
    (while (> (point) beg)
      (delete-indentation)))
  (beginning-of-line))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-read-if-solid ()
  "Read from current point all the non-whitespace characters.
Ignores leading and trailing whitespace."
  (let* ((eol (line-end-position))
         beg
         ret)
    (save-excursion
      (if (looking-at "[ \t]")
          (skip-syntax-forward " " eol))
      (setq beg (point))
      (unless (eolp)
        (skip-chars-forward "^ \t" eol)
        (if (eq (point) beg)            ;not moved
            (end-of-line))              ;no trailing spaces
        (unless (eq (point) beg)
          (setq ret (buffer-substring beg (point))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-read-whitespace (&optional point)
  "Gets whitespace following the point or optional at POINT.
Return:
  ''     if no whitespace
  str    whitespace string"
  (let* ((re-w "[ \t]+")                ;whitespace
         mp                             ;maximum point, end of line
         op)
    (save-excursion
      (if (null point)
          (setq op (point))
        (setq op point)
        (goto-char point))
      (setq mp (line-end-position))
      (if (or (null  (looking-at re-w)) ;not sitting on whitespace
              (null (re-search-forward re-w mp t)))
          ""
        (buffer-substring op (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-read-paragraph ()
  "Read paragraph at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at ".*[^ \t]")
      (backward-paragraph)
      (let* ((beg (point)))
        (forward-paragraph)
        (buffer-substring beg (point))))))

;;; ----------------------------------------------------------------------
;;; - if you use outline or folding, please open the buffer first
;;;   otw lines cannot be read correcly [the \n is missing if file
;;;   has closed folds]
;;;
(defun ti::buffer-read-line (&optional len skip)
  "Read whole line from buffer.
Input:

  LEN   Only read LEN characters.
        If LEN is more than line has characters then return whole line.
  SKIP  Ignores SKIP count characters from beginning of line.
        If there is not that many to skip, return full line."
  (let* ((line     (ti::read-current-line))
         (len-full (length line)))
    (if (null skip) nil
      (cond
       ((and len (> len skip))
        (setq line (substring line skip)))
       ((eq len skip) (setq line ""))))
    (if (and len (< len len-full))
        (substring line 0 len)
      line)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-grep-lines (re &optional beg end inc-prop)
  "Greps lines matching RE from buffer.

Optionals:

  BEG           default is `point-min'
  END           default is `point-max'
  INC-PROP      do not remove properties while reading lines.

Return:

  nil or  \(str str str ..\)"
  (let* ((beg           (or beg (point-min)))   ;point begin
         (end           (or end (point-max)))   ;point end
         list
         line)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward re end t)
        (setq line (ti::read-current-line))
        (if (null inc-prop)
            (setq line (ti::remove-properties line)))
        (ti::nconc list line)
        (forward-line 1)))
    list))

;;}}}
;;{{{ buffer: matching, reading words, chars

;;; ....................................................... &b-reading ...

;;; ----------------------------------------------------------------------
;;; The bad thing is that it is impossible slow, so
;;; use it only when time is not critical (not in loops)
;;;
(defun ti::buffer-looking-back-at (re)
  "Return t if text immediately before point match RE.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them.

Note:
  Use only if you need this badly. It's impossible slow."
  (let ((beg (point))
        ret)
    (while (and (null ret)
                (re-search-backward re nil t))
      (setq ret (eq (match-end 0) beg)))
    (goto-char beg)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-read-char (&optional direction distance)
  "Read character towards the DIRECTION from current point.
nil = forward, non-nil backward. DISTANCE 0/nil means reading from
current position.

Return:

  nbr   read char value
  nil   if the position is not within `point-min-marker' and
         `point-max-marker'."
  (let* ((beg  (point-min-marker))
         (end  (point-max-marker))
         (pos  (or distance 0))
         (dest (if direction
                   (- (point) (1+ pos))
                 (+ (point) pos)))
         (read (if (or (< dest beg) (> dest end))
                   nil
                 t)))
    (if (null read)
        nil                             ;allowed to read ?
      (char-after dest))))

;;; ----------------------------------------------------------------------
;;; - You can define the "word" syntax here without changing syntax entries.
;;; - If you want to get word according to current mode's syntax table,
;;;   use following instead
;;;
;;;   (require 'thingatpt)                      ;19.29
;;;   (word-at-point)
;;;
(defun ti::buffer-read-word (&optional charset strict)
  "Return word specified by optional CHARSET after point.
If optional STRICT is non-nil, requires that point is sitting on
CHARSET before continuing. If there is no CHARSET under point,
search forward for word.

Limitations:

  Cannot read word that starts at beginning of buffer

Return:
  str         word or nil."
  (let* ((charset       (or charset "-a-zA-Z0-9_"))
         (not           (concat "^" charset)))
    (save-excursion
      (if (or (null strict)
              (and strict (looking-at charset)))
          (buffer-substring
           (progn
             (skip-chars-forward not)
             (skip-chars-backward charset)
             (point))
           (progn
             (skip-chars-forward charset)
             (point)))))))

;;; ----------------------------------------------------------------------
;;; - This is totally different from the other word reading funcs,
;;;   it gives you the word separated by spaces. For more finer control see,
;;;   CHARSET in ti::buffer-read-word
;;;
(defun ti::buffer-read-space-word ()
  "Return word separated by spaces or bol/eol.
If sitting on space or tab, read next word forward. If sitting in the
middle of word, find the word beginning until bol, and start reading from
that point. Point is moved to the beginning of word.

Return:
 str
 nil    empty line"
  (let* ((bol (line-beginning-position))
         p)                             ;point
    (cond
     ((or (bobp)
          (equal (char-syntax  (preceding-char)) ?\  ))
      ;; At the beginning of word, first char
      nil)
     ((looking-at "[^ \t\n]")
      (setq p (point))
      (skip-chars-backward "^ \t\n" bol)
      ;;      (skip-syntax-backward " " bol)
      (if (eq p (point))                 ;jump not done.
          (beginning-of-line)))          ;text starts at bol
     ((looking-at "[ \t\n]")
      (skip-chars-forward " \t\n"))
     ((save-excursion                   ;is the line end of buffer
        (end-of-line)                   ;--> e.g. in minibuffer
        (eobp))
      (beginning-of-line)))
    (ti::buffer-read-if-solid)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-read-syntax-word (syntax &optional mode)
  "Read block of characters from current point.
Blocks are separated by SYNTAX Normally the block is read
from current point forward.

Input:
 SYNTAX         class like \"w\" for words.
 MODE    'back  read backward
         'word  read full word, skip syntax forward, then backward.

Return:

 str
 nil    current point does not contain SYNTAX class char."
  (let* ((beg  (point))
         end
         ret)
    (save-excursion
      (cond
       ((eq mode 'back)
        (setq end (point))
        (skip-syntax-backward syntax)
        (setq beg (point)))
       ((eq mode 'word)
        (skip-syntax-forward syntax) (setq end (point))
        (skip-syntax-backward syntax) (setq beg (point)))
       (t
        (skip-syntax-forward syntax)
        (setq end (point)))))
    (if (not (eq beg end))
        (setq ret (buffer-substring beg end)))
    ret))

;;; ----------------------------------------------------------------------
;;; #not fully tested
;;; - Why did I do this after all ?
;;; - This won't work if cursor it at SPACE and BOL and user wants
;;;   word BACK
;;;
;;;
(defun ti::buffer-read-nth-word (&optional count mode back charset)
  "Read COUNT nth word in line.

Input:

  COUNT                 defaults to 0 ,current word according to MODE.
  MODE   nil            count from the bol/eol.
         'end           count from the bol/eol, stop at eol/bol
         'this          start counting from this position
         'thisEnd       start counting from this position, stop at eol/bol
  BACK                  read backward. Affects the mode parameter.
  CHARSET               use charset as \"word\", otw defaults to mode's
                        word syntax.

Examples:

 (ti::buffer-read-nth-word)                  ,return first word in line
 (ti::buffer-read-nth-word 5 'end)           ,return 5th word, but stop at eol

 ;; return 5th word, counting backwards stopping at bol. Read the word
 ;; with charset a-zA-z.

 (ti::buffer-read-nth-word 5 'end 'back \"a-zA-Z\")

Caveats:

  You get different results, if point is already sitting at word, or
  if it's sitting at whitespace, when using 'this modes.
  Try yourself with `forward-word' command.

  REMEMBER THAT WORD IS MODE DEPENDENT (syntax tables)

Return:

  str   word
  nil   nth word does not exist."
  (let* ((next-func     (if back 'backward-word 'forward-word))
         (prev-func     (if back 'forward-word 'backward-word))
         (next-skip     (if back 'skip-chars-backward 'skip-chars-forward))
         (cmp-func      (if back '< '>))
         (count         (or count 0))
         limit
         ret)
    (save-excursion
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  set limits ...
      (if (memq mode '(end nil))        ;starting position
          (if back (line-end-position) (line-beginning-position)))
      (if (memq mode '(end thisEnd))    ;setting the limit value
          (setq limit (if back (line-beginning-position) (line-end-position))))
      (if (eq 0 count)
          ;; Skip over spaces, stay put ...
          (if (ti::char-in-list-case (following-char) '(?\t ?\ ))
              (funcall next-skip " \t"))
        (funcall next-func count)
        (if (ti::char-in-list-case (following-char) '(?\t ?\ ))
            (funcall prev-func 1)))
      (if (and limit
               (funcall cmp-func (point) limit))
          nil                           ;limit exceeded
        (cond
         (charset
          (setq ret (ti::buffer-read-word charset)))
         (t
          (require 'thingatpt)
          ;;  silence Bytecomp.
          (setq ret (ti::funcall 'word-at-point)))))
      ret)))

;;}}}
;;{{{ buffer: replacing, modifying lines

;;; ..................................................... &b-replacing ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-replace-keywords-with-table (keys)
  "Function to replace string a with string b.
A and b are stored in a structure and b may be the result of a
computation in itself.  In other words, say we have a list of dotted
pairs like this

        ((\"$$AUTHORNAME$$\" . \"Charles R Martin\")
         (\"$$TIMESTAMP$$\"   . (current-time-string))

then the function skips through the buffer doing replace-string
$$AUTHORNAME$$ 'Charles R Martin' followed by replace-string
$$TIMESTAMP$$ (results of 'current-time-string')."
  (interactive
   (list (symbol-value
          (intern
           (completing-read "Replace keywords using table: "
                            obarray
                            (lambda (e)
                              (and (boundp e)
                                   (listp (symbol-value e)))))))))
  (mapcar (lambda (x)
            (save-excursion
              (goto-char (point-min))
              (while (search-forward (car x) nil t)
                (replace-match (eval (cdr x))))))
          keys))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-replace-region-with  (beg end string &optional keep-point)
  "Replace region BEG END with STRING.
Point is after the inserted string or if KEEP-POINT is non-nil
then point is at BEG."
  ;;  Prevent accidental delete
  (if (not (stringp string))
      (error "Input error."))
  ;;  mimic "r" tag region, do not kill that extra char.
  (delete-region beg end)
  (goto-char beg)
  (insert string)
  (if keep-point
      (goto-char beg)))

;;; ----------------------------------------------------------------------
;;; The basic code for this was borrowed from zap-to-char in simple.el
;;; (define-key esc-map "Z" 'zap-to-regexp) ; originally 'zap-to-char
;;;
(defun ti::buffer-zap-to-regexp (arg regexp)
  "Kill up to and including ARG'th occurrence of REGEXP.
Goes backward if ARG is negative; error if REGEXP not found."
  (interactive "p\nsZap to regexp: ")
  (kill-region
   (point)
   (progn
     (search-forward-regexp regexp nil nil arg)
     ;; This line makes zap-to-regexp behave like
     ;; d/ and d? in vi (ie with forward deletion
     ;; the regexp is left intact).  Is this
     ;; really the right thing?  zap-to-char
     ;; dropped this behavior.  Was there a good
     ;; reason?  I like this behavior since I use
     ;; vi frequently enough to get some benefit
     ;; from the orthogonality.
     (if (>= arg 0) (search-backward-regexp regexp 1))
     ;; p.s.  Yes I know the '=' doesn't really do
     ;; much.
     (point))))

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'leave-nth-word 'ti::buffer-leave-nth-word)
;;; - This is great function if you have some column output generated
;;;   by SQL call or shell call, and you just want THOSE words left...
;;;
;;;
(defun ti::buffer-leave-nth-word (beg end &optional nbr strict)
  "Delete all between BEG and END except nth word NBR.
Default word nbr is 1, ie. the first word in the line.
The word is considered as space separated entity.

REMEMBER that word is mode dependent !

Input:

  NBR           which word top leave on line, range 1..x
  STRICT        if non-nil then if word NBR is not found delete whole line"
  (interactive "*r\nP")
  (let* ((nbr   (or nbr 1))
         word)
    (save-restriction
      (narrow-to-region beg end) (ti::pmin)
      (while (not (eobp))
        (beginning-of-line)
        (setq word (ti::buffer-read-nth-word nbr 'end))
;;;     (ti::d! word)
        (cond
         (word
          (delete-region (line-beginning-position) (line-end-position))
          (insert word)
          (forward-line 1))
         ((and (null word) strict)
          (ti::buffer-kill-line))       ;already does fwd-line
         (t
          (forward-line 1)))))))

;;; ----------------------------------------------------------------------
;;; - Easiest would have been using zap-to-char, but
;;;   it's not same in 18.xx and 19.xx
;;; #todo: detect 19.xx and use zap, it's much quicker
;;;
;;;
(defun ti::buffer-kill-line (&optional delete count)
  "Kill line and move next line up.
If cursor is sitting at the end of buffer, nothing happens.

Input:

  DELETE    use `delete-region', which doesn't manipulate `kill-ring',
            thus the execution is faster.
  COUNT     how many lines to wipe.

Portable:

  Between any emacs versions 18.xx - 19.xx

Errors:

  Never signalled.

Return:

  t             line killed
  nil           sitting at eob, cannot kill line"
  (interactive "*P")
  (let* ((null-line-re "^$")
         (count        (or count 1))
         (i            0))

    ;;  emacs kill-line is little awkward, because if you're at the
    ;;  end of buffer it signals an error...

    (while (< i count)
      (incf i)
      (cond
       ((eobp)                          ;nothing to kill
        nil)
       ((and (null (eobp)) (looking-at null-line-re))
        (if delete
            (delete-char 1)
          (kill-line))
        t)
       (t                               ;shift line up
        (beginning-of-line)
        (if delete
            (delete-region (point) (line-end-position))
          (kill-line))
        (if (null (eobp))
            (if delete
                (delete-char 1)
              (kill-line)))
        t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-strip-control-m () ;;#todo: Emacs function?
  "Remove control-M characters from buffer."
  (with-buffer-modified
    (save-excursion
      (ti::pmin)
      (while (re-search-forward "\r+$" nil t)
        (replace-match "" t t)))))

;;; ----------------------------------------------------------------------
;;; #defalias   (defalias 'u2d 'ti::buffer-lf-to-crlf)
;;;
(defun ti::buffer-lf-to-crlf (&optional arg force)
  "Simple Unix to Dos converter. If ARG is non-nil -->  Dos to Unix.
Strips or inserts ^M (return) marker _only_ at the end of line.

If optional FORCE is given, ignores possible write protection.

Example:
  (if (ti::file-dos-p)
      (ti::buffer-lf-to-crlf 'Dos2unix 'doReadOnly))"
  (interactive "P")
  (let* ((stat   buffer-read-only))
    (cond
     ((or (not stat)
          (prog1 force (setq buffer-read-only nil))) ;turn it off
      ;;  - We use unwind, because the buffer read only status must be
      ;;    restored. User may get anxious and press C-g for large buffers...
      ;;  - I wonder if we can clear the buffer-modified flag too?
      ;;    we leave it untouched for now...
      (unwind-protect
          (save-excursion
            (goto-char (point-min))     ; start at the be.g. of file
            (if arg
                ;; ..................................... Dos --> unix ...
                (progn
                  (while (search-forward "\015\n" nil t)
                    (replace-match "\n"))
                  (ti::pmax)
                  (beginning-of-line)
                  ;; Maybe last line does not have newline?
                  (when (looking-at ".*\015$")
                    (end-of-line)
                    (delete-backward-char 1)))
              ;; ....................................... unix --> dos ...
              (end-of-line)
              (if (not (char= (preceding-char) ?\015))
                  (insert "\015"))
              (while (not (eobp))
                (forward-line)
                (end-of-line)
                (if (not (char= (preceding-char) ?\015))
                    (insert "\015")))))
        ;;  restore value
        (setq buffer-read-only stat))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-arrow-control (buffer &optional mode str pos)
  "Controls showing the arrow glyph.

Input:
  BUFFER        Where to put the arrow, must be visible.
  MODE  'show   show the arrow with optional STRING
        'hide   remove the arrow. If STR is given, change the value
                of `overlay-arrow-position'. This is usually for restoring
                the original content.
        'move   move to current bol position or to POS. STR argument is
                ignored.
        any     same as 'hide

  STR           arrow string to use, defaults to \"=>\"
  POS           any position, converted to beginning of line
                [Emacs docs say the arrow must be at bol]"
  (cond
   ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ show ^^^
   ((or (eq mode 'show)
        (eq mode 'move))
    ;;  We do not touch the arrow definition, if 'move is the mode
    (if (eq mode 'show)
        (setq overlay-arrow-string
              (if (stringp str) str "=>")))
    (or overlay-arrow-position
        (setq overlay-arrow-position (make-marker)))
    (set-marker overlay-arrow-position
                (if pos
                    (progn
                      (goto-char pos)
                      (line-beginning-position))
                  (line-beginning-position))
                buffer))
   ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ hide ^^^
   (t
    (if overlay-arrow-position          ;Kill the marker
        (set-marker overlay-arrow-position nil))
    (if (stringp str)
        (setq overlay-arrow-string str)))))
  ;; - Here should be some kind of buffer refresh, since
  ;;   the markes isn't hidden, if you're using read-char,
  ;;   instead of read-from-minibuffer. See [tinyreply.el] for hack.
  ;; - Anybody knows how to refresh the view, please MAIL ME!!
;;; Not working, I thought moving the cursor would refresh arrow state
;;;  (save-excursion
;;;    (select-window (get-buffer-window buffer))
;;;    (set-buffer buffer)
;;;    (goto-char (line-beginning-position)))

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'nl 'ti::buffer-insert-line-numbers), see unix nl(1)
;;;
;;; -- or is this better ?
;;; #defalias (defalias 'insert-lines-numbers 'ti::buffer-insert-line-numbers)
;;;
(defun ti::buffer-insert-line-numbers (beg end &optional line grow format)
  "Insert line numbers to buffer.
Mark the region where to insert the line numbers.

The default line format is '%02d:%s' for values lower that 100.
For bigger values the format is dynamical (digit len derived from
start value)

Input:

  BEG END       point  area bounds
  LINE          nbr    starting line number. 1 is default
  GROW          nbr    grow count. 1 is default
  FORMAT        str    how line is formatted, see above

Return:

  --"
  ;;  We input number as string so that user may press return
  ;;
  (interactive "*r\nsStart line[1]: \nsInterval[1]: ")
  (let* (
         ;;  convert strings to sensible value
         (count         (cond
                         ((integerp line) ;; calling lisp
                          line)
                         (t ;; interactive
                          (if (eq 0 (length line))
                              1
                            (string-to-int line)))))
         (factor        (cond
                         ((integerp grow)
                          grow)
                         (t
                          (if (eq 0 (length grow))
                              1
                            (string-to-int grow)))))
         (digits        (ti::digit-length count))
         ;;  Select "02d" when numbers < 100
         ;;  Otw, select "digits" len.
         (fmt           (or format
                            (concat
                             "%0"
                             (int-to-string
                              (if (or (= digits 1) (eq digits 2))
                                  2 digits))
                             "d:%s")))
         line)
    (save-restriction
      (narrow-to-region beg end)
      (ti::pmin)
      (while (not (eobp))
        (setq line (ti::read-current-line))
        (if (not (string-equal "" line))
            (delete-region (point) (line-end-position)))

        (insert (format fmt count line))
        (setq count (+ count factor))
;;;     (ti::d! count)
        (forward-line 1)))))

;;; ----------------------------------------------------------------------
;;; - There must be removing function too.. :-)
;;; #defalias (defalias 'remove-line-numbers 'ti::buffer-remove-line-numbers)
;;;
(defsubst ti::buffer-remove-line-numbers (beg end &optional re level)
  "Remove line numbers from region BEG END.
The Default line numbers are sticked to the left and have form

   xxx: text txt txt

where xxx represent some numbers.

You can supply optional RE and regexp LEVEL that should be
removed. E.g. in normal, above case the

  RE    = \"^[0-9]+:\"
  LEVEL = 0            ,match whole regexp"
  (interactive "*r")
  (ti::buffer-replace-regexp
   (or re "^[0-9]+:")
   (or level 0)
   ""
   nil
   beg
   end))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-randomize-lines (beg end)
  "Scramble all the lines in region BEG END.
If region contains less than 2 lines, lines are left untouched."
  (interactive "*r")
  (catch 'cancel
    (save-restriction
      (narrow-to-region beg end)
      ;;   Exit when there is not enough lines in region
      (if (< (- (point-max) (point-min)) 3)
          (throw 'cancel t))
      ;;    Prefix lines with a random number and a space
      (goto-char (point-min))
      (while (not (eobp))
        (insert (int-to-string (random 32000)) " ")
        (forward-line 1))
      ;;  Sort lines according to first field (random number)
      (sort-numeric-fields 1 (point-min) (point-max))
      (goto-char (point-min))           ;Remove the prefix fields
      (while (not (eobp))
        (delete-region (point) (progn (forward-word 1) (+ (point) 1)))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-make-dup-line (&optional count)
  "Copy the current line COUNT times (default is 1) below the current line."
  (interactive "*p")
  (setq count (or count 1))
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring
                 (point)
                 (progn (forward-line 1) (point)))))
      (while (> count 0)
        (insert line)
        (setq count (1- count))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-inc-string-nbr (re inc-val increment &optional level)
  "Search string and increment integers.

Input:

  RE        regexp to match integer. Subexpr 1 assumed in interactive call
  INC-VAL   start value.
  INCREMENT Step how much to increment every time.
  LEVEL     Subexpression in regexp to match the integer portion.

E.g. I you have just paste same variable on the lines multiple times

         tablevar10[10]
         tablevar10[10]
         tablevar10[10]
         tablevar10[10]
         tablevar10[10]

And now you want to make them unique:

         tablevar01[10]
         tablevar02[10]
         tablevar03[10]
         tablevar04[10]
         tablevar05[10]

You just give RE \"r\\([0-9]+\\)\" and start value 1, increment 1"
  (interactive "sRE: \nnstart value: \nnIncrement: ")
  (let* ((level (or level 1))
         len
         beg
         end
         fmt)
    (while (re-search-forward re nil t) ;search whole buffer
      (when (match-end level)
        (setq beg (match-beginning level)
              end (match-end level)
              len (- end beg)
              fmt (concat "%0" (int-to-string len) "d"))
        (delete-region beg end)
        (goto-char beg)
        (insert (format fmt inc-val))
        (incf inc-val increment)))))

;;; ----------------------------------------------------------------------
;;; - Here is slightly different version. this increments every number
;;;   whereas the previous would increment only SUBMATCH by STEP
;;;
;;; - E.g. copying the first line produces:
;;;
;;;     assign pi0_vld = (opc_i0 === alu0);
;;;     assign pi1_vld = (opc_i1 === alu1);
;;;
(defun ti::buffer-copy-line-and-inc-numbers (&optional increment)
  "Copy line, preserving cursor column, and INCREMENT any numbers found.
Prefix ARG is the increment value. Defaults to 1."
  (interactive "p")
  (let* ((col           (current-column))
         (line          (ti::read-current-line))
         (increment     (if (integerp increment) increment  1))
         len out
         mark
         num)
    (end-of-line)
    ;;  We have to use markers, because the line is modified.
    (setq mark (point-marker))
    (beginning-of-line)
    (while (re-search-forward "[0-9]+" (marker-position mark) 1)
      (setq len (length (match-string 0)))
      (setq num (string-to-int (match-string 0)))
      ;;  E.g. 0001 --> 0002
      (setq out (format (concat "%0" (int-to-string len) "d")
                        (+ increment num)))
      (replace-match out))
    (beginning-of-line)
    (insert line "\n")
    (move-to-column col t)
    ;; kill marker
    (setq mark nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-copy-word (n)
  "Copy N words above the current line.
If there is no words above the line, then do nothing."
  (interactive "p")
  (let ((column (current-column))
        copy)
    (save-excursion
      (beginning-of-line)
      (if (bobp)
          nil
        (forward-line -1)
        (move-to-column column t)
        (setq copy (buffer-substring
                    (point)
                    (min (save-excursion (end-of-line) (point))
                         (save-excursion (forward-word n) (point)))))))
    (if copy
        (insert copy))))

;;; ----------------------------------------------------------------------
;;; #defalias   (defalias 'double-space-region 'ti::buffer-newlines-to-region)
;;;
(defun ti::buffer-add-newlines-to-region (beg end &optional arg)
  "Insert to to the end of each line in region BEG END ARG newlines.
Default is to inser one which makes lines make double spaced."
  (interactive "*r\np")
  (save-restriction
    (narrow-to-region beg end)
    (ti::pmin)
    (while (search-forward "\n" nil t)
      (replace-match
       (concat "\n" (make-string arg ?\n))
       nil t))))

;;; ----------------------------------------------------------------------
;;; - STRICT parameter can be used from lisp call
;;; #defalias   (defalias 'remove-blank-lines 'ti::buffer-cnv-empty-lines)
;;;
(defun ti::buffer-cnv-empty-lines (beg end &optional nbr strict)
  "Convert empty lines in region BEG END to zero empty lines.
Optionally leaves NBR empty lines. If STRICT is non-nil, all lines
must have NBR amount of empty lines, no more or less.

Point is not preserved."
  (interactive "*r\nP")
  (let* ((empty-line-re  "^[ \t]+$\\|\n")
         (nbr            (or nbr 0)) ;default is to leave no empty lines
         pb pe                          ;points beg, end
         count
         do-it)
    (save-restriction
      (narrow-to-region beg end)
      (ti::pmin)
      (while (not (eobp))
        (if (null (looking-at empty-line-re))
            (forward-line 1)
          (setq pb (point))   (skip-chars-forward " \t\n")
          (beginning-of-line) (setq pe (point))
          ;;  There is a bug in count-lines, that's why we
          ;;  use line-end-position,
          ;;  not 'pe' to count the lines in region
          (setq count (count-lines pb (line-end-position)))
          (setq do-it nil)
          ;; ...................................................... cond ...
          (cond
           ((null strict)
            (if (> nbr count)
                nil                     ;not that many lines here
              (setq do-it t)))
           (t
            (setq do-it t)))
          ;; .................................................... action ...
          (cond
           ((null do-it)
            (forward-line 1))           ;skip
           ((> count 0)
            (delete-region pb pe)
            (setq count nbr)
            (while (> count 0)          ;leave that many
              (decf count) (insert "\n"))
            (if (> count 1)
                (beginning-of-line)
              ;;  nothing done, next line
              (forward-line)))))))))

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'delete-duplicate-lines 'ti::buffer-del-dup-lines)
;;;
;;;  - Letting shell to do the job is the fastest, cleanest
;;;    way. Sometimes lisp just isn't the right tool...
;;;
;;; A. Want to do it fast?
;;;    Camel book has ready code for this. Pg 228
;;;    $ perl -ne 'print unless $seen{$_}++' file.in > file.out
;;;
;;; B. How about running a shell command over the region/buffer
;;;    with command "uniq"? This filters successive lines.
;;;    C-x h , ESC-| uniq RET
;;;
;;;
(defun ti::buffer-del-dup-lines(beg end &optional len white-lines)
  "Deletes duplicate lines in buffer. Optionally compares first LEN
characters to determine line equality.

Input:

  BEG,END       area bounds
  LEN           portion of line: chars to compare
  WHITE         if non-nil, don't touch whitespace only lines.

Requirements:

  Call shell with small PERL program. Make sure PERL is along the path.
"
  (interactive "*r\nP")
  (let* (cmd)
    (save-restriction
      (narrow-to-region beg end)
      (ti::pmin)
      (setq
       cmd
       (concat
        "perl -ne '"
        (if len
            (concat "$line = substring($_,0, "
                    (int-to-string len)
                    ");")
          "$line = $_;")

        (if white-lines
            "/^\\s*$/ && do{print; next;};")
        "print unless $seen{$line}++;"
        "'"))
      (shell-command-on-region
       (point-min)
       (point-max)
       ;; replace flag
       cmd
       t))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-delete-until-non-empty-line (&optional backward point)
  "Delete all lines starting from current point.
Stop on [be]obp or non-empty line. Optionally delete BACKWARD
and start at POINT or current position.

Moves point to the beginning of non-empty line."
  (interactive "P")
  (let* (end)
    (when point
      (goto-char point))
    (beginning-of-line)
    (setq point (point))
    (cond
     (backward
      (while (and (not (bobp))
                  (looking-at "^[ \t]*$"))
        (setq end (point))
        (forward-line -1)))
     (t
      (while (and (not (eobp))
                  (looking-at "^[ \t]*$"))
        (forward-line 1)
        (setq end (point)))))
    (if end
        (delete-region point end))))

;;; ----------------------------------------------------------------------
;;; - The delete-region, according to emacs C-developers,
;;;   is _lighting_ fast way to do deletions in emacs.
;;;
(defun ti::buffer-trim-blanks (beg end)
  "Delete trailing blanks in region BEG END."
  (interactive "*r")
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (delete-horizontal-space)
        (forward-line 1))))
  nil)                                  ;for possible hook

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-replace-regexp (re level str &optional back beg end)
  "Like `replace-regexp' but for Lisp programs.
Lisp info page says in \"Node: Style Tip\", that lisp programs shouldn't
use `replace-regexp', so here is identical function that doesn't touch
the mark. The point is left after last match.

Input:

  RE        regexp
  LEVEL     subexpression
  STR       string used in replacing.
  BACK      replace backward
  BEG END   region. If both BEG and END is given, the
            BACK parameter is ignored."
  (let* ((func (if back 're-search-backward 're-search-forward))
         bp
         ep)
    (if (not (integerp level))          ;common error
        (error "Level is not integer."))
    (cond
     ((and beg end)
      (setq bp beg  ep end  func 're-search-forward))
     ((and back end)
      (setq bp (point)  ep (point-min)))
     ((and back beg)
      (setq bp beg  ep (point-min)))
     ((and (not back) beg)
      (setq bp beg ep (point-max)))
     ((and (not back) end)
      (setq bp (point) ep end))
     (t                                 ;fall thru case
      (setq bp (point)  ep (point-max))))
    (save-restriction
      (narrow-to-region bp ep)
      (ti::pmin)
      (while (and (funcall func re nil t)
                  (not (eobp)))
        (if (null (match-end level)) nil ;not matched
          (ti::replace-match level)
          ;; point is at the end of STR inserted
          (insert str))))))

;;}}}
;;{{{ buffer: misc

;;; ..................................................... &buffer-misc ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-diff-type-p ()
  "Check the diff type in buffer.
Assumes that whole buffer contains diff. Searches for traces.
Lines must be left flushed.

   *** /tmp/T.11  Fri Oct 20 12:22:51 1995
   --- /tmp/T.1   Fri Oct 20 12:24:29 1995
   ***************

Normal diff shows:

   20,21d19
   < clrFamily;
   < clrInfo;

Gnu diff -n (or --rcs, Output an RCS format diff)

    d696 1
    a696 1
            (tdi-goto-kbd 'verb)
    d704 2
    a705 2

Gnu diff -u (unified diff)

    @@ -17,6 +17,8 @@
     bAnsTime[16+1];
     clearCode;
     endChargeTime[16+1];
    +clrFamily;
    +clrInfo;
     statClrTime[16+1];
     clearPart;
     aDirNbrType;

Returns:
  cons cell
    (TYPE . POS)        ,POS is the diff start position
    nil                 ,no diff found

  TYPE can be
    'context            ,context diff -c
    'gnu-n              ,gnu diff -n
    'gnu-u              ,gnu diff -u
    'normal             ,normal diff

 POS
    character position where the first diff was found"
  (let* ((re-c1     "^[ \t]*[*][*][*] [0-9]") ;context diff regexps

         ;;   The normal diff line is following, but PGP breaks it.
         ;;   That's why we have those ? ? in thge regexp
         ;;   --- 1.2.1.1
         ;;   - --- 1.2.1.1
         ;;
         (re-c2     "^-? ?--- .")
         (re-c3     (concat  "^" (regexp-quote "***************")))
         (re-n1     "^[0-9]+[dca][0-9]+$\\|^[0-9]+,[0-9]+[dca][0-9]")
         (re-n2     "^[<>]")
         ;;  Gnu types
         (re-gn1            "^[dac][0-9]+ [0-9]+$")
         (re-gu1            "^@@ [-+][0-9]+,[0-9]+[ \t]+[-+]+")
         type
         pos
         ret)
    (save-excursion
      (ti::pmin)
      (cond
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  context ..
       ((and (re-search-forward re-c1 nil t)
             (setq pos (line-beginning-position))
             (or (save-excursion
                   (and (progn
                          (forward-line 1)
                          (looking-at re-c2))
                        (progn
                          (forward-line 1)
                          (looking-at re-c3))))
                 (save-excursion
                   (forward-line -1)
                   (looking-at re-c3))))
        (setq type 'context))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . normal ..
       ((and (re-search-forward re-n1 nil t)
             (setq pos (line-beginning-position))
             (progn
               (forward-line 1)
               (looking-at re-n2)))
        (setq type 'normal))
       ((re-search-forward re-gu1 nil t)
        ;;  There is filename information above the diff start.
        ;;  --- file.xx
        ;;  +++ file.xx
        ;;
        (forward-line -2)
        (setq pos (point))
        (setq type 'gnu-u))
       ((and (re-search-forward re-gn1 nil t) ;require two same lines
             (setq pos (line-beginning-position))
             (progn
               (forward-line 1)
               (looking-at re-gn1)))
        (setq type 'gnu-n)))
      (if (and type pos)
          (setq ret (cons type pos)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-outline-widen ()
  "Open folded/outlined buffer if some of the modes is active.
You have to call this function if you want to do something for
the whole buffer."
  (interactive)

  ;;  Unfold the buffer, so that we can see all.
  ;;  We must also preserve point

  (ti::save-with-marker-macro
    (and (boundp 'folding-mode)
         ;;  No autoloads allowed, this makes sure the fboundp
         ;;  is converted to real function. The ti::funcall command
         ;;  cannot use autoload function.
         (progn (require 'folding) t)
         (if (symbol-value 'folding-mode) ;ByteComp silencer
             (ti::save-line-column-macro nil nil
               ;; ByteComp silencer
               (ti::funcall 'folding-open-buffer))))
    (and (eq major-mode 'outline-mode)
         (fboundp 'show-all)
         (progn (require 'outline) t)
         (ti::save-line-column-macro nil nil
           (ti::funcall 'show-all)))
    (and (boundp 'outline-minor-mode)
         (fboundp 'show-all)
         (progn (require 'outline) t)
         (ti::save-line-column-macro nil nil
           (ti::funcall 'show-all)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-buffer-list-files (&optional re str)
  "Return all files loaded into Emacs.

If optional RE and STR are given, then a file name substitution
takes place:

 args           RE = \"/usr43/users/john/\"   STR = \"~/\"
 buffer file    \"/usr43/users/john/t.txt\"
 substituted    \"~/t.txt\"

Example:

 (ti::buffer-buffer-list-files \"/usr43/users/john\" \"~\")

Return:

 (filename ..)      list of filenames"
  (let* (list
         file)
    (dolist (elt (buffer-list))
      (setq file  (buffer-file-name elt))
      (when (stringp file)         ;might be nil if buffer has no file
        (if (and re str
                 (string-match re file))
            (setq file (ti::replace-match 0 str file)))
        (push file  list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-count-words (beg end)
  "Count words in region BEG END."
  (interactive "r")
  (let ((msg (count-matches "\\w*" beg end)))
    (when (and msg
               (string-match "\\([0-9]+\\)" msg))
      (string-to-int msg))))

;;; ----------------------------------------------------------------------
;;; - This is quite a handy function when you're programming e.g.
;;;   in C++ and want to know how many chars are in the string.
;;;
(defun ti::buffer-count-chars-in-delimited-area (arg &optional verb)
  "Counts characters within quotes. ARG C - u to search single quotes.
Other argument invokes asking the beginning delimiter: if you give
\"(\"  the end delimiter is automatically set to \")\".
This function is mainly for interactive use. VERB.

Return:
  nbr   count of characters
  nil   begin or end delimiter was not found"
  (interactive "P")
  (let* ((alist '(( ?\(  ?\) )
                  ( ?\{  ?\} )
                  ( ?\[  ?\] )
                  ( ?\`  ?\' )
                  ( ?\<  ?\> )))
         (verb   (or verb (interactive-p)))
         beg-ch
         end-ch
         beg-re
         end-re
         re
         elt
         point
         ret)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... . preliminaries ...
    (setq
     re   (cond
           ((equal arg nil)
            "\"")
           ((equal arg '(4))
            "'")
           (t
            (message "Begin delimiter char: ")
            (setq beg-ch (read-char))
            (setq end-ch
                  (if (setq elt (assq beg-ch alist))
                      (nth 1 elt)
                    ;;   Can't find match for it, so use same char
                    ;;   for both delimiters
                    beg-ch))
            nil)))
    (if re                              ;now, what we got?
        (setq beg-re (regexp-quote re)   end-re beg-re)
      (setq beg-re (regexp-quote (char-to-string beg-ch))
            end-re (regexp-quote (char-to-string end-ch))))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... . do it ...
    (save-excursion
      (if (null (re-search-forward end-re nil t))
          (and verb
               (message (concat "Can't find end mark: " end-re)))
        (setq point (point))
        ;;  the re-search-forward leaves point after the char,
        ;;  we have to go small step back before we change the direction.
        (forward-char -1)
        (if (null (re-search-backward beg-re nil t))
            (and verb
                 (message (concat "Can't find beginning mark: " beg-re)))
          ;; the -2 excludes the markers itself.
          ;;
          (setq ret (- (length
                        (buffer-substring point (point)))
                       2))
          (and verb
               (message (concat (int-to-string ret) " characters."))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-word-move (set &optional back)
  "Move to next word defined in SET, optionally BACK.
SET must be string, that can be turned into regexp and that can
be used with skip-chars functions.

E.g. \"-[]$%@#&*\":;{}()<>/\\ \t\n\""
  (interactive)
  (let* ((nset          (concat "^" set)) ;not-set
         (set-re        (concat "[" (regexp-quote set) "]"))
         (char          (char-to-string
                         (if back
                             (preceding-char)
                           (following-char))))
         (point (point)))
    (cond
     (back
      (if (string-match set-re char)
          (progn
            (skip-chars-backward set)
            (skip-chars-backward nset))
        ;;  If we're over word already, this moves. But if we're
        ;;  at the beginning of word this doesn't move.
        ;;
        (skip-chars-backward nset)
        (when (eq (point) point)
          (skip-chars-backward set)
          (skip-chars-backward nset))))
     (t
      (if (string-match set-re char)
          (progn
            (skip-chars-forward set)
            (skip-chars-forward nset))
        (skip-chars-forward nset)
        (skip-chars-forward set))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-find-duplicate-same-word (&optional back)
  "Find consecutive occurrences of same word, optionally search BACK."
  (interactive "P")
  (let* ((func  (if back 're-search-back 're-search-forward)))
    (if (funcall func "\\(\\<\\w*\\>\\)[ \t\n]*\\1" nil t)
        (isearch-highlight (match-beginning 0) (match-end 0))
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-move-paragraph-to-column (beg end col)
  "Move text BEG END to column position COL.

The indent is done in the following way:
o  untabify region
o  Search first non-whitespace line starting from the beginning
   of region.
o  count how much the line is indented: remove that indentation
   from all the rest of the lines.
o  Now when lines have no indet; reindent to COL

The procedure described preserves the actual paragraph style, so that
if text inside paragraph is more indented that the previous line the
relative indent is preserved.

    txt txt txt txt txt txt
    txt txt txt txt txt txt
      inner indent txt txt txt
      inner indent txt txt txt
    txt txt txt txt txt txt
    txt txt txt txt txt txt

Input:

  beg   always calculates to bol
  end   always calculates to eol"
  (interactive "*r\np")
  (let (min
        marker
        len)
    (goto-char (min beg end))           ;Setting MIN
    (setq min (line-beginning-position))
    (goto-char (max beg end))           ;setting MAX
    (end-of-line)
    (setq marker (point-marker))        ;Because untabify moves end
    (untabify min (marker-position marker))
    ;;  Is there non whitespace line?
    (goto-char min)
    (cond
     ((re-search-forward "^[^ \n]" (marker-position marker)  t)
      ;;  non whitespace line found.
      ;;  Do nothing -- indent directly
      nil)
     ((re-search-forward "^\\( +\\)[^ \n]" (marker-position marker)  t)
      ;;  Remove this indentation.
      (when (> (setq len (length (or (match-string 1) ""))) 0)
        (indent-rigidly min (marker-position marker) (- 0 len)  ))))
    ;;  Now reindent the region
    (indent-rigidly min (marker-position marker) col) ;new
    ;; Kill marker
    (setq marker nil)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-move-to-col (col)
  "Doesn't care about line length. Insert spaces to get to COL.
Convert tabs to spaces to get to exact COL."
  (interactive "Nto col: ")
  (move-to-column col t)
  (if (not (eq (current-column) col))
      (while (< (current-column) col)
        (insert " "))))

 ;;;;; Experimental
 ;;;(defun space-to-column (target)
 ;;;  "Insert spaces as necessary to move pt to TARGET column."
 ;;;  (interactive "p")
 ;;;  (let ((cur (current-column)))
 ;;;    (if (< cur target)
 ;;;        (insert (make-string (- target cur) ? )))))

;;}}}
;;{{{ buffer: selective display

;;; ................................................... &misc-packages ...

;;; ----------------------------------------------------------------------
;;; - Separating the "effective display" is easy with this...
;;;
;;;
(defun ti::buffer-selective-display-copy-to  (beg end buffer &optional verb)
  "Copy region BEG END selective display to BUFFER. VERB.
E.g. folding.el and outline based modes use selective display."
  (interactive
   (progn
     (if (not (region-active-p))
         (error "Region not selected."))
     (list
      (region-beginning)
      (region-end)
      (read-from-minibuffer "To buffer: " "*selective display*"))))
  (let* ((bp    (get-buffer-create buffer))  ;barfs if invalid...
         (bp    (ti::temp-buffer bp 'clear)) ;ok, use it
         line)
    (ti::verb)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (ti::pmin)
        (while (not (eobp))
          ;;  - Reset for normal lines.
          ;;  - Or reads until \r. I.e. the hidden part is not read
          (setq line (or (and (looking-at ".*\r")
                              (concat
                               (ti::buffer-match  "\\([^\r]+\\)+\r" 1)
                               "..."))
                         (ti::read-current-line)))

          (setq line (concat line "\n"))
          (forward-line 1)
          (ti::append-to-buffer bp line))))
    (if verb
        (pop-to-buffer bp))))

;;; ----------------------------------------------------------------------
;;; - Print folding.el and outline based buffer with this...
;;;
(defun ti::buffer-selective-display-print  (beg end)
  "Print selective display region BEG END."
  (interactive "r")
  (let* ((buffer  (generate-new-buffer "*print*")))
    (unwind-protect
        (progn
          (ti::buffer-selective-display-copy-to beg end buffer)
          (with-current-buffer buffer (print-buffer)))
      (kill-buffer buffer))))

;;}}}
;;{{{ Window, frames

;;; .......................................................... &window ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-frame-list  (&optional all exclude-current win)
  "Return only frames that are non-dedicated.
Input:
  ALL                   if non-nil, return all frames.
  EXCLUDE-CURRENT       if non-nil, exclude current active frame.
  WIN                   Use this is as a current window when searching
                        current frame."
  (let* ((oframe  (if win
                      (window-frame win)
                    (selected-frame)))
         flist
         ret)
    (if exclude-current
        (setq flist (delete oframe (frame-list)))
      (setq flist (frame-list)))
    (dolist (frame flist)
      (select-frame frame)
      (if (or all (not (window-dedicated-p (selected-window))))
          (ti::nconc ret frame)))
    (if (framep oframe)
        (select-frame oframe))          ;Return back to original
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-list (&optional buffers)
  "Gather all visible windows or BUFFERS visible in current frame."
  (let* ((s     (selected-window))      ;start window
         (loop  t)
         (w     s)                      ;current cycle
         l
         ww)

    (if buffers                         ;Start list
        (setq l (list (window-buffer s)))
      (setq l (list s)))

    (while loop
      (setq ww (next-window w))
      (setq w ww)                       ;change
      (other-window 1)                  ;move fwd
      (if (eq ww s)                     ;back to beginning ?
          (setq loop nil)

        (if buffers                     ;list of buffers instead
            (setq ww (window-buffer ww)))
        (setq l (cons ww l))))
    (nreverse l)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::window-single-p ()
  "Check if there is only one window in current frame."
  ;;  No need to run `length' when `nth' suffices.
  (let* ((win      (selected-window))
         (next     (next-window)))
    ;;  Same window?
    (eq win next)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-get-buffer-window-other-frame  (buffer)
  "Return (frame . win). If BUFFER is visible..
in some other frame window than in the current frame."
  (let* (win
         ret)
    (dolist (frame
             (delete (selected-frame) (frame-list)))
      ;;  maybe in other frame...
      (when (setq win (get-buffer-window buffer frame))
        (setq ret (cons frame win))
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;; - don't know good way how to generalize this to return either top/bottom
;;;   window. I guess we just copy this and make small changes...
;;; - Does anyone have good suggestions to do therwise?
;;;
(defun ti::window-find-bottom  (win-list)
  "Find bottom window from WIN-LIST.
Any non-visible window in list is skipped.
If there are adjacent windows, return all of them.

        -------------
        |           |   <- top window
        -------------
        |  |   |    |   < three splitted windows at the bottom
        | A| B |  C |
        -------------

Return:
  list          single or many windows. In any order."
  (let* (data
         top
         top-cmp
         bot
         bot-cmp
         win-val
         init)
    (dolist (win win-list)
      (setq data (window-edges win))
      (if (null init)                   ;init vars
          (setq init    t               ;initalized ok
                win-val (list win)      ;win comes from 'window-loop'
                top     (nth 1 data)
                bot     (nth 3 data)))

      (setq top-cmp     (nth 1 data)
            bot-cmp     (nth 3 data))
      (cond
       ((> bot-cmp bot)                 ;this is more lower
        (setq win-val (list win)
              top     top-cmp
              bot     bot-cmp))
       ((or (eq bot-cmp bot)           ;hmm, same horizontal top row..
            (eq top-cmp top))           ;split sideways...
        (push win win-val))
       ((or (eq bot-cmp bot)            ;  .........
            (> top-cmp  top))           ;  ....    .
                                        ;  .........  < pick lowest in left
        (setq win-val (list win)
              top     top-cmp
              bot     bot-cmp))))
    win-val))

;;; ----------------------------------------------------------------------
;;;
(defun ti::window-match-buffers (buffer-name-list)
  "Check all windows that match BUFFER-LIST.

Input:

  BUFFER-NAME-LIST      ,strings, list of buffer names.

Return:

  '((BUFFER-NAME WIN-PTR WIN-PTR ..)
    (BUFFER-NAME ..)
    ..)"
  (let* (alist
         buffer
         ptr
         p)
    (dolist (win (ti::window-list))
      ;;  last walue will tell the BOTTOM
      (setq buffer      (buffer-name (window-buffer win)))
      ;;  Create alist
      ;;  '((BUFFER-NAME WIN-PTR WIN-PTR ..)
      ;;    (BUFFER-NAME ..))
      (cond
       ((member buffer buffer-name-list) ;does it interest us ?
        (cond
         ((not (setq ptr (assoc buffer alist))) ;; create initial element
          (push (list buffer win) alist))
         (t                             ;; add element
          (setq p  (cdr ptr))           ;drop 1st element away
          (ti::nconc p win)             ;add new element
          ;;  replace with new list
          (setcdr ptr p))))))
    (nreverse alist)))

;;}}}
;;{{{ Key maps, translations

;;; ----------------------------------------------------------------------
;;;
(defun ti::keymap-single-key-definition-p (key-def)
  "Check if KEY-DEF is a single key definition.
E.g. If you want to check if prefix key is composed only from
one key: \"a\" \"?\\C-a\"  or even [(?a)].

  (ti::keymap-single-key-definition-p [ a ] )  --> a
  (ti::keymap-single-key-definition-p [(a)] )  --> a
  (ti::keymap-single-key-definition-p \"a\" )    --> a
  (ti::keymap-single-key-definition-p \"\\C-a\" ) --> C-a

  (ti::keymap-single-key-definition-p [(a) (b)] )  --> nil
  (ti::keymap-single-key-definition-p [(meta a)])  --> nil
  (ti::keymap-single-key-definition-p \"ab\" )       --> nil
  (ti::keymap-single-key-definition-p \"?C-ab\" )    --> nil

Return:

  If single key. Return it, either as character or symbol."
  (let* ((key (cond
               ((and (stringp key-def) ;; "\C-a" or "a"
                     (eq 1 (length key-def)))
                (string-to-char key-def))
               ((and (vectorp key-def) ;; [(ELT)] or [ELT]
                     (eq 1 (length key-def))
                     (eq 1 (length (elt key-def 0))))
                (let* ((ELT  (elt key-def 0))
                       (item (if (listp ELT) ;; was [(ELT)]
                                 (car ELT)
                               ELT)) ;; otherwise [ELT]
                       ;;  At this point; convert to string
                       (ch   (cond
                              ((symbolp item) ;; mouse-1 ot the like
                               item)
                              ((characterp item) ;; was it ?a ==> [(?a)]
                               item)
                              ((and (stringp item)
                                    (eq 1 (length item)))
                               (string-to-char item)))))
                  ch)))))
    key))

;;; ----------------------------------------------------------------------
;;;
(defun ti::keymap-define-key-backspace ()
  "Move C-h to Backspace if this is non-windowed Emacs.
Key C-x C-? replaces original C-x C-h.
Key C-c h   replaces original C-h call
"
  (interactive)
  (let* (;;;     (DELETE    "\C-h")
         (BACKSPACE "\177"))
    (unless (ti::compat-window-system)
      (defvar key-translation-map (make-sparse-keymap))
      ;;  If it's nil then something is wrong. Fix it.
      (unless key-translation-map
        (setq key-translation-map (make-sparse-keymap)))
      ;; This keymap works like `function-key-map', but comes after that,
      ;; and applies even for keys that have ordinary bindings.
      (define-key key-translation-map "\177" "\C-h")
      (define-key key-translation-map "\C-h" "\177")
      (global-set-key BACKSPACE 'backward-delete-char)
      (flet ((key-warning
              (key def)
              (message "tinylib: Warning, key already occupied: %s %s"
                       key def)))
        ;; (ti::define-key-if-free global-map
        ;;   "\C-x\C-?" 'help-for-help 'key-warning)
        (ti::define-key-if-free global-map
                                "\C-ch"    'help-command  'key-warning)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::keymap-function-bind-info  (function-sym &optional map)
  "Return binding information for FUNCTION-SYM from MAP as string or nil."
  (let* ((gm  (current-global-map))
         global-bindings
         local-bindings
         bind-info)
    (setq global-bindings (where-is-internal function-sym)
          local-bindings
          (prog2
              ;;  We have to set this to nil because where-is-internal
              ;;  searches global map too. We don't want that to happen
              ;;
              (use-global-map (make-keymap))
              (where-is-internal
               function-sym
               (or map (current-local-map)))
            (use-global-map gm)))
    (setq
     bind-info
     (if (or global-bindings local-bindings)
         (format "%s%s%s"
                 (if global-bindings
                     (format "global %s"
                             (mapcar 'key-description
                                     global-bindings))
                   "")
                 (if (and global-bindings local-bindings)
                     " and "
                   "")
                 (if local-bindings
                     (format "local to %s"
                             (mapcar 'key-description
                                     local-bindings))
                   ""))))
    bind-info))

;;; ----------------------------------------------------------------------
;;;
;;;  because of the nature of minor modes, changes in the maps
;;;  are not reflected unless, the minor mode is installed again
;;;
;;;  The following removes minor keymap, if it exists,
;;;  and reinstalls it with new added bindings.
;;;
(defun ti::keymap-reinstall-minor-mode (mode-name-symbol)
  "Reinstall minor mode MODE-NAME-SYMBOL.
This is needed if you have made changes to minor modes keymaps.
They don't take in effect until you reinstall the minor mode.

Return:
 t       minor mode found and reinstalled
 nil     no susch minor mode."
  (let* (sym
         mode-string
         elt
         map-sym
         map)
    (when (setq elt (assq mode-name-symbol minor-mode-alist))
      (setq mode-string (nth 1 elt))
      (setq elt (assq mode-name-symbol minor-mode-map-alist))

      (unless elt
        (error "No map for minor mode %s"  mode-name-symbol))
      (setq sym (concat
                 (symbol-name mode-name-symbol)
                 "-map"))
      (setq map-sym (intern-soft sym))
      (if (or (null map-sym)
              (not (keymapp (setq map (eval map-sym)))))
          (error "The keymap was not found %s" map-sym))
      (ti::keymap-add-minor-mode mode-name-symbol nil nil 'remove)
      (ti::keymap-add-minor-mode mode-name-symbol mode-string map))))

;;; ----------------------------------------------------------------------
;;; - Why doesn't emacs offer this simple interface by default ?
;;;
(defun ti::keymap-add-minor-mode
  (mode-func-sym mode-name-sym  mode-map &optional remove)
  "Add the minor mode into Emacs. If mode exists, do nothing.

Input:

  MODE-FUNC-SYM         function symbol, mode to turn on
  MODE-NAME-SYM         variable  symbol to hold mode name string
  MODE-MAP              keymap
  REMOVE                OPTIONALLY removes mode with mode-name-sym

Examples:

   ;;  to add mode
   (ti::keymap-add-minor-mode 'foo-mode 'foo-mode-name  foo-mode-map)

   ;;  to remove mode
   (ti::keymap-add-minor-mode 'foo-mode nil nil 'remove)"

  (let* (elt)
    (cond
     ((null remove)
      (or (assq mode-func-sym minor-mode-map-alist)
          (setq minor-mode-map-alist
                (cons (cons mode-func-sym  mode-map)
                      minor-mode-map-alist)))
      ;;  Update minor-mode-alist
      (or (assq  mode-func-sym minor-mode-alist)
          (setq minor-mode-alist
                (cons (list mode-func-sym mode-name-sym)
                      minor-mode-alist))))
     (t
      (and (setq elt (assq mode-func-sym minor-mode-map-alist))
           (setq minor-mode-map-alist (delq elt minor-mode-map-alist)))

      (and (setq elt (assq mode-func-sym minor-mode-alist))
           (setq minor-mode-alist (delq elt minor-mode-alist)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::keymap-bind-control (map-symbol get-set prop key)
  "Get or set the stored property binding in map.
This is a good function to use if you modify the original
bindings in the map. You can then call the original
function behind the binding in your modified function.

Input:

  MAP-SYMBOL    map name
  GET-SET       operation.
                'get  = return previous property value (key definition)
                'set  = copy definition once.
                'sett = (force) copy definition even if already copied.
                The 'set copies the key definition behind the propert
                PROP only if there is no previous value. 'sett
                replaces the content of PROPERTY.
  PROP          property name
  KEY           string -- key binding.

Examples:

  (ti::keymap-bind-control 'mail-mode-map 'set 'my \"\\C-c\\C-c\")
  --> mail-send-and-exit, saved to property 'my

  (ti::keymap-bind-control 'mail-mode-map 'set 'my \"\\C-c\\C-c\")
  --> nil, property 'my Was already set

  (ti::keymap-bind-control 'mail-mode-map 'get 'my \"\\C-c\\C-c\")
  --> mail-send-and-exit, get the saved property 'my.

Live example:

  ;; - first save original, then use our function. Use property
  ;;   'my, because The C-c C-c can already be occupied by
  ;;   some other package...
  ;; - it calls the original afterwards

  (ti::keymap-bind-control 'mail-mode-map 'set 'my \"\\C-c\\C-c\")
  (define-key      mail-mode-map \"\\C-c\\C-c\" 'my-mail-func-CcCc)

  (defun my-mail-func-CcCc (arg)
    ...
    (funcall  ;; Call the original.
      (ti::keymap-bind-control 'mail-mode-map 'get 'my \"\C-c\C-c\")
      arg)
    ;; Function ends here.)"
  (let* (map
         map-key
         sym
         val
         func)
    (unless (boundp map-symbol)
      (error "No variable bound %s" map))
    (setq map (eval map-symbol))
    (unless (keymapp map)
      (error "Not a keymap %s" map-symbol))
    (if (or (ti::nil-p key)             ;must be valid string
            (not (stringp key)))
        (error "Invalid KEY %s" key))
    (setq map-key (concat (symbol-name map-symbol) key))
    (setq func (lookup-key map key))
    (when func                          ;does function exist?
      (setq sym (intern map-key)
            val (get sym prop))
      (cond
       ((eq get-set 'get)
        val)
       ((and (eq get-set 'set)
             (null val))                ;set only if PROP not exist
        (put sym prop func))
       ((eq get-set 'sett)              ;replace value
        (put sym prop func))))))

;;; ----------------------------------------------------------------------
;;; - What is an translate table?
;;; - Well; it says "if you press this key I give you this character back"
;;; - It is used for remapping the keys, but beware! In X envinronment,
;;;   where you can paste data between emacs, the translation gives
;;;   unpleasant results. Try pasting the _normal_ \ char from other
;;;   window to emacs that uses transltion presented in example below.
;;;   --> you get the | character pasted into Emacs
;;;
(defun ti::keymap-translate-table (&optional arg)
  "Make new translate table.

Input ARG

  'use      Start using the new table if the
            `keyboard-translate-table' if nil. Otherwise does nothing.
  'use-new  replace current table with fresh one
  nil       return new, default translate table.

Examples:

    Switch these keys. Let's assume the \\ key is on top after this,
    since it is used more often in emacs.

    (ti::keymap-translate-table 'use)
    (aset keyboard-translate-table ?\\| ?\\\\ )
    (aset keyboard-translate-table ?\\\\ ?\\| )

Return:

  new translate table"
  (let ((index 0)
        (xlat-table (make-string 128 0)))
    (while (< index 128)                ;Generate the identity map.
      (aset xlat-table index index)
      (setq index (1+ index) ))
    (cond
     ((eq arg 'use-new)
      (setq keyboard-translate-table xlat-table))
     ((eq arg 'use)
      (and (null keyboard-translate-table)
           (setq keyboard-translate-table xlat-table)))
     (t))
    xlat-table))

;;; ----------------------------------------------------------------------
;;; - For preventing Emacs to beep and disabling the normal keys
;;;   (for mail, gnus, ... )
;;;
(defun ti::keymap-put-abc-map (map &optional func)
  "Put function `ignore' to abc key MAP, optionally put FUNC."
  (let* ((i             0)
         (func          (or func 'ignore))
         low
         up)
    (while (< i 27 )
      ;;  Set lowercase/upcase keys to nil
      (setq low (char-to-string (+ 65 i))
            up  (char-to-string (+ 97 i)))
      (define-key map low func)
      (define-key map  up func)
      (incf i))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::keymap-put-map (map &optional func)
  "Put function `ignore' to a0 > x <128 key MAP, optionally put FUNC."
  (let* ((i             20)
         (func          (or func 'ignore)))
    (while (< i 128 )
      (define-key map (char-to-string i) func)
      (incf i))))

;;; ----------------------------------------------------------------------
;;; - Mapping keysto functions easily.
;;;
(defun ti::keymap-mapkeys (map-key-fun args)
  "Maps MAP-KEY-FUN to list of keys in ARGS.

Example:
  (mapkeys
   'global-set-key
   '([f1] 'hilit-rehighlight-buffer
     [f2] 'eval-defun
     [f3] 'repeat-complex-command))"
  (let (key
        func
        (i 0)
        (len (length args)))
    (if (eq 0 (% len 2)) nil
      (error "args not paired"))
    (while (< i len )
      (setq key (nth i args)   func (nth (1+ i) args)   i (+ 2 i) )
      (funcall map-key-fun key func))))

;;}}}
;;{{{ (T)ext properties, faces

;;; ........................................................... &faces ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-text-properties-wipe  (&optional beg end)
  "Remove all, ie. literally all, text properten between BEG and END.
BEG AND end defaults to whole buffer.
Doesn't care about read only status of buffer."
  (interactive "r")
  (let (buffer-read-only
        (inhibit-read-only t))          ;allow read-only prop wipe out
    (set-text-properties
     (or beg  (point-min))
     (or end  (point-max))
     nil)))

;;; ----------------------------------------------------------------------
;;; - During setting a different color to a face,
;;;   the color may be occupied and emacs halts with message
;;;
;;;     (error "X server cannot allocate color" "DarkSeaGreen3")
;;;
;;; - This function allows you to give several "try" choices,
;;;
(defun ti::set-face-try-list (list face &optional attribute)
  "Try to assign color to face.
The input is list of color names which are tried one by one.
First one that succeeds is assigned. If color is occupied, tries
next one. Doesn't signal any errors.

Input:

  LIST          (\"color1\" \"color2\" ..) or single color string
  FACE          symbol. E.g. 'region
  ATTRIBUTE     symbol. Choices are 'fg and 'bg. Default is 'fg

Return:

  color         color that was assigned
  nil           all tries failed"
  (let* (status)
    (or attribute
        (setq attribute 'fg))
    (dolist (color (ti::list-make list))
      (when (condition-case nil
                (progn
                  (cond
                   ((eq attribute 'fg)
                    (set-face-foreground face color))
                   (t
                    (set-face-background face color)))
                  (setq status color)
                  t)
              (error
               ;; cannot set
               nil))
        ;; succesfull; stop the loop
        (return)))
    status))

;;}}}

;;{{{ misc: movement

;;; ############################################################ &Misc ###

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::buffer-forward-line (&optional count)
  "Move vertically lines down. If COUNT is negative, then up.

`forward-line' moves point always to the beginning
of next line, and the elisp manual says not to use `next-line' in
programs.

This function behaves exactly as `next-line'. If the next line is shorter
it moves to the end of line."
  ;; (interactive "P")
  (let* ((col (current-column)))
    (and (null count) (setq count 1))   ;No arg given
    (forward-line count)
    (move-to-column col)))

;;}}}
;;{{{ buffer: line handling , addings strings

;;; ......................................................... &m-lines ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-looking-at-one-space ()
  "Return non-nil if point is in the middle on one whitespcae.
This is a position where there is only one tab or one space or point is
followed by one newline. Similarly if point is at `point-min' and there is
only one whitepace, or at `point-max' is preceded by one whitespace."
  (let* ((char-backward (if (not (bobp))
                            (preceding-char)))
         (char-forward (if (not (eobp))
                           (following-char))))
    ;;  Point-!-Here
    (cond
     ((and (null char-backward)
           (null char-forward))
      ;;  BOBPEOBP ie. empty buffer.
      nil)
     ((and char-backward
           char-forward)
      ;; char-!-char
      (and (not (string-match "[ \t\f\r\n]"
                              (char-to-string char-backward)))
           (string-match "[ \t\f\r\n]"
                         (char-to-string char-forward))
           ;;  What is the next character?
           (save-excursion
             (forward-char 1)
             (not (string-match "[ \t\f\r\n]"
                                (char-to-string (following-char)))))))
     (t
      ;; BOBP-!-char
      ;; char-!-EOBP
      (string-match  "[ \t\f\r\n]"
                     (char-to-string
                      (if (eobp)
                          char-backward
                        char-forward)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-surround-with-char (char)
  "Insert two same CHAR around a string near point.
String is delimited by whitespace, although the function will do
the right thing at beginning or end of a line, or of the buffer.
If the char is one of a matching pair, do the right thing.
Also makes a great gift."
  (interactive "cSurround with char: ")
  ;; hmm, ought to be able to do this with syntax tables?
  (let
      ((begchar char)
       (endchar char))
    (cond
     ((or (char= char ?{) (char= char ?}))
      (setq begchar ?{)
      (setq endchar ?}))
     ((or (char= char ?\() (char= char ?\)))
      (setq begchar ?\()
      (setq endchar ?\)))
     ((or (char= char ?<) (char= char ?>))
      (setq begchar ?<)
      (setq endchar ?>))
     ((or (char= char ?`) (char= char ?'))
      (setq begchar ?`)
      (setq endchar ?'))
     ((or (char= char ?[) (char= char ?]))
      (setq begchar ?[)
            (setq endchar ?])))
    (re-search-backward "^\\|\\s-" (point-min))
    (if (not (bolp))
        (re-search-forward "\\s-")
      (if (looking-at "\\s-") (re-search-forward "\\s-")))
    (insert-char begchar 1)
    (let ((opoint (point)))
      (if (re-search-forward "\\s-\\|\n" (point-max) t)
          (forward-char -1)
        (goto-char (point-max)))
      (insert-char endchar 1)
      (if (eq (point) (+ opoint 1))
          (forward-char -1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-fill-region-spaces (beg end &optional column)
  "Fill region BEG END with spaces until COLUMN or 80.
In picture mode paste/copying rectangles,
it easiest if the area has spaces in every row up till
column \"80\".

To return to 'ragged' text, use function `ti::buffer-trim-blanks'

Input:
  BEG           beginning of area, always line beginning
  END           end of area, always line end.
  COLUMN        the fill column. Defaults to 79, because 80 would
                add annoying \\ marks at the end of line."
  (interactive "*r\nP")
  (let* ((column   (or column 79))
         (spaces   (make-string (+ 2 column) ?\ ))
         line
         len
         add)
    (save-restriction
      (narrow-to-region beg end)
      (untabify (point-min) (point-max)) ;very important !!
      (ti::pmin)
      (while (not (eobp))
        (setq line (ti::read-current-line)
              len  (length line)
              add  (- column len))
        (if (<= add 0)
            nil                         ;we can't touch this
          (end-of-line)
          (insert (substring spaces 1 add)))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------
;;; - This nice and elegant solution to get quotes around the words,
;;;   but someday it should be generalized to put ANYTHING around the word.
;;;
(defun ti::buffer-quote-words-in-region (beg end)
  "This function quotes words in selected region BEG END."
  (interactive "r")
  (goto-char beg)
  (while (< (point) end)
    (kill-word 1)
    (insert (prin1-to-string (current-kill 0)))
    (setq end (+ end 2))
    (forward-word 1)
    (forward-word -1)))

;;; ----------------------------------------------------------------------
;;; - E.g. if you want to decide "fast filling", you could check if any line
;;    is longer that fill-column.
;;;
(defun ti::buffer-find-longer-line (beg end len)
  "Check BEG END if there exist line longer than LEN.

Return:
  point    beginning of line
  nil"
  (let* (pos)
    (save-excursion
      (goto-char (min beg end))
      (while (and (null pos)
                  (not(eobp))
                  (< (point) (max beg end)))
        (end-of-line)
        (if (<= (current-column) len)
            nil
          (beginning-of-line) (setq pos (point)) )
        (forward-line 1)))
    pos))

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-scramble-region (beg end &optional char)
  "Scrables text BEG END with char so that it's not readable any more.
Preserves words by substituting every [a-zA-Z] with optional CHAR."
  (interactive "r")
  (let* ((ch (if char                   ;pick the scramble char
                 (char-to-string char)
               "o")))
    (save-excursion
      (save-restriction                 ;preserve prev narrowing
        (narrow-to-region beg end)
        (ti::pmin)
        (ti::buffer-replace-regexp "[a-zA-Z]" 0 ch)))))

;;; ----------------------------------------------------------------------
;;; - This function requires user input when RE-LOOK is given
;;; - This is aimed for lisp programs
;;;
(defun ti::buffer-add-string-region (beg end str &optional re-look)
  "Add to region BEG END STR and optionally to lines matching RE-LOOK.
You might use this as intend-region by adding more spaces to any
vertical position, but most likely this is best function for
commenting arbitrary blocks of code.

1) set mark to _exact_column_ where to add string
2) move cursor to destination line, column does not matter.

If you want to add string to specific lines only, supply
rex when you are asked for 'look for rex'. Remember that this
rex will be used from that mark column to the end of line, so whole line
is not looked. Here is one example:

      *mark here
    ;;; triple comment
    ; single comment

    ;;; another triplet
    *cursor here

    ;;#; triple comment
    ; single comment

    ;;#; another triplet
      ^^^^^^^^^^^^^^^^^^^^ --> the REX match area, note not incl. leading!

Note that the single ';' isn't matched, because the mark's column position
is further away.

References:

  Emacs 19.28 has almost similar function. Look
  `string-rectangle'. It does not overwrite existing text."
  (interactive "r\nsString to region :\nsLook for re :")
  (let* (col
         look)
    (if (ti::nil-p re-look)             ;reset
        (setq re-look nil))
    (if (ti::nil-p str)
        nil                             ;pass, nothing given
      (save-excursion
        ;;  Get true boundaries.
        ;;
        (goto-char (min beg end)) (setq col (current-column))
        (setq beg (line-beginning-position))
        (goto-char (max beg end)) (setq end (line-end-position))
        (save-restriction
          (narrow-to-region beg end) (ti::pmin)
          (while (not (eobp))
            (move-to-column col t)
            (setq look   (if (and re-look
                                  (eq (current-column) col))
                             (looking-at re-look)
                           t))
            (if look
                (insert str))
            (forward-line 1)))))))

;;}}}

;;{{{ buffer: lists handling, sorting

;;; ----------------------------------------------------------------------
;;; - The default sort-regexp-fields is too limited and awkward to use.
;;; - This one offers easy interface to 'sort'
;;;
(defun ti::buffer-sort-regexp-fields (list level re &optional numeric reverse case)
  "Sort LIST of strings at subexpression LEVEL of RE.
Sort can optionally be NUMERIC, REVERSE or CASE sensitive.

Return:
  sorted list."
  (let* ((clist (copy-list list)))      ;sort modifies it otw.
    (sort clist
          (function
           (lambda (l r &optional ret elt1 elt2)
             (cond
              ((not case)               ;not sensitive
               (setq l  (downcase l)    ;ignore case
                     r (downcase r))))
             ;;  read the maches from strings
             (setq elt1 (ti::string-match re level l)
                   elt2 (ti::string-match re level r))
             (cond
              ((not (and elt1 elt2))    ;make sure match happened
               (setq ret nil))
              (numeric
               (setq ret
                     (if reverse
                         (< (string-to-int elt2)  (string-to-int elt1))
                       (< (string-to-int elt1)  (string-to-int elt2)))))
              (t
               (setq ret
                     (if reverse
                         (string< elt2 elt1)
                       (string< elt1 elt2)))))
             ret)))
    clist))

;;}}}

;;{{{ misc: shell, exec, process

;;; ......................................................... &process ...
;;; - Current "misc" category

;;; ----------------------------------------------------------------------
;;; - This is great function to build up completions for login names...
;;; - I have 400 entries in passwd file and it's not very  fast.
;;; - You Should call this only once with regexp "." and put all the entries
;;;   into some variable. Use that variable for lookup.
;;;
(defun ti::file-passwd-grep-user-alist (re &optional not-re passwd-alist)
  "Grep all login names, where person name match RE.
The matches are gathered from `ti::var-passwd-buffer' and  if it does not
exist, error is signaled.

If optional NOT-RE is string, it will be used after the RE match is done.
It is used to exclude items.

If PASSWD-ALIST is given it will be used instead to gather needed
information. It should be alist int he form returned by function
`ti::file-passwd-build-alist'

Return:
  ((login  . user-name-entry) ..)"
  (let* ((passwd-buffer   ti::var-passwd-buffer)
         ;;  The name is 5th entry
         ;;  neva:I5KJd2C33dtMg:418:200:Max Neva,Houston Texas ...
         (passwd-re   "^\\([^:]+\\):[^:]+:[^:]+:[^:]+:\\([^:,]+\\)")
         alist
         line
         login
         person)
    (cond
     (passwd-alist
      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ list ^^^
      ;; Hm, the loops are almost identical, but what the heck...
      (while passwd-alist
        (setq line (cdr (car passwd-alist)))
        ;; It's possible, that there is no "person" field, e.g.
        ;;     "lp:*:9:7::/usr/spool/lp:/bin/sh"
        ;;              |
        ;;              empty field
        ;;
        ;;  It's quicker to test 2 at the same time, and only then decode
        ;;  the field into parts
        (when (and  (string-match re line)
                    (string-match passwd-re line))
          (setq login  (match-string 1 line))
          (setq person (match-string 2 line))
          (when (and login person)
            (if (or (not (stringp not-re))
                    (and (stringp not-re)
                         (not (string-match not-re person))))
                (push (cons login person) alist))))
        (pop passwd-alist)))
     (t
      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ buffer ^^^
      (if (null (ti::set-buffer-safe passwd-buffer))
          (error "Passwd buffer does not exist"))
      (if (eq (point-min) (point-max))
          (error "Passwd buffer is empty."))
      (ti::pmin)
      (while (not (eobp))
        (setq line (ti::read-current-line))
        (when (and (string-match  re  line)
                   (looking-at    passwd-re))
          (setq login  (match-string 1))
          (setq person (match-string 2))

          (if (null (and login person))
              nil
            (if (or (not (stringp not-re))
                    (and (stringp not-re)
                         (not (string-match not-re person))))
                (push (cons login person) alist))))
        (forward-line 1))))
    alist))

;;; ----------------------------------------------------------------------
;;; E.g. in HP-UX the command is this
;;; (my-read-passwd-entry "jaalto" "ypcat passwd")
;;;
(defun ti::file-passwd-read-entry (&optional user cmd)
  "Return USER's password entry using Shell CMD.

If the password buffer's content is not empty, the CMD isn't called, instead
the entry is searched from the buffer. This reduces overhead of calling
processes every time function is invoked.

References:
  `ti::var-passwd-buffer'"
  (let* ( ;;  Permanent buffer, since launching process is expensive
         (user      (or user (user-login-name)))
         (re        (concat "^" user ":"))
         (buffer    (get-buffer-create ti::var-passwd-buffer))
         ret)
    (unwind-protect
        (with-current-buffer buffer
          (when (eq (point-min) (point-max)) ;No entries yet
            (if (null cmd)
                (error "Need command to get the passwd file")
              (erase-buffer)
              (let ((list (split-string cmd)))
                (apply 'call-process
                       (car list)
                       nil
                       (current-buffer)
                       nil
                       (cdr list)))))
          (ti::pmin)
          (if (re-search-forward re nil t)
              (setq ret (ti::read-current-line)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-passwd-build-alist (cmd)
  "Build assoc list out of passwd table using CMD.
Please note, that building a list takes some time, so call this
only once per program. The CMD must be a command to retrieve
contents of passwd file.

Note:

    The performance of this function is not very good. Expect
    parsing 1000 users/15 seconds.

Return:

    ((login . full-passwd-entry) ..)"
  (let* ((passwd-buffer   ti::var-passwd-buffer)
         alist
         line
         login)
    ;;  force loading passwd entries
    (ti::file-passwd-read-entry "SomeUser" cmd)
    (with-current-buffer passwd-buffer
      (ti::pmin)
      (while (not (eobp))
        (beginning-of-line)
        (setq line (buffer-substring
                    (point) (progn (end-of-line) (point))))
        ;; password entry looks like this, sometimes there may be garbage
        ;; after shell command like these two grep notes.
        ;;
        ;;   grep: can't open a
        ;;   grep: can't open tty
        ;;
        ;;   lm58817:x:23193:23193:Leena M{ki|:/home3/li7/lm58817:/bin/tcsh
        (when (setq login (ti::string-match "^[^:]+" 0 line))
          (setq alist (cons (cons login line) alist)))
        (forward-line 1)))
    alist))

;;}}}
;;{{{ misc: function

;;; ----------------------------------------------------------------------
;;;
(defun ti::buffer-defun-function-name (&optional point)
  "Return possible function name.
Starts searching backward form current or optional POINT.
Be sure to be in right mode, so that right `beginning-of-defun' is used.

In Lisp, the current function can be found only if it is left flushed.

In C++, this will simply returns line portion, which it thinks
contains function name.

In Perl, it is supposed that word following \"sub\" is function name.

Input:
  point     where to look

Return:
  nil
  string"
  (let* ((name      (symbol-name major-mode))
         (lisp-re   (concat
                     "def\\(un\\|subst\\|macro\\|advice\\|var\\|const\\)"
                     "[ \t]+\\([^ \t]+\\)"))
         line
         ret)
    (setq line (ti::read-current-line))
    (save-excursion
      (ignore-errors
        ;;  Now comes fun part...Ugh!
        (cond
         ((or (setq ret (ti::string-match lisp-re 2 line))
              (string-match "lisp" name))
          ;;  This beginning-of-defun only finds only left
          ;;  flushed FORMS
          ;;
          (or ret
              (progn
                (beginning-of-defun) (setq line (ti::read-current-line))
                (setq ret (ti::string-match lisp-re 2 line)))))

         ((or (string-match "CC" name)
              (string-match "C++" name))
          (beginning-of-defun)
          ;; A nightmare...
          ;;
          ;; perAtom_c *
          ;; pMscCg_c::DecodeV7
          ;; ()
          ;; {
          ;;
          ;; perAtom_c *pMscCg_c::DecodeV7
          ;; ()
          ;; {
          ;; Try our best...
          ;;
          (search-backward "(")
          (beginning-of-line)
          (or (setq ret (ti::buffer-match "^[ \t]*\\([^ \t(]+\\)[ \t]*(" 1))
              (progn                    ;skip one line up
                (forward-line -1)
                (setq ret (ti::buffer-match "^[ \t]*\\([^\n(]+\\)" 1)))))

         ((and (string-match "perl" name)
               (re-search-backward "^[ \t]*sub[ \t]+\\([^ \t]+\\)" nil t))
          (setq ret (match-string 1)))))
      ret)))

;;}}}
;;{{{ file

;;; ############################################################ &File ###

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-days-old   (file)
  "Calculate how many days old the FILE is. This is approximation."
  (let ((now  (current-time))
        (file (nth 5 (file-attributes file))))
    (/ (ti::date-time-difference now file) 86400)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-touch (file)
  "Touch FILE by updating time stamp. FILE is created if needed.
Note: the filename is handed to the shell binary `touch'. Make sure the
filename is understood by shell and does not contain meta characters."
  (if (not (file-exists-p file))
      (with-temp-buffer (write-region (point) (point) file))
    (let* ((touch (or (get 'ti::file-touch 'touch-binary)
                      (executable-find  "touch")
                      (error "`touch' binary not found."))))
      (put 'ti::file-touch 'touch-binary touch)
      (call-process touch nil nil nil (expand-file-name file)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-ange-completed-message (&rest args)
  "Default message after file has been loaded. Ignore ARGS."
  (message "Ange-ftp bg completed"))

;;; ----------------------------------------------------------------------
;;; #todo:  Not quite what I want...
;;;
(defun ti::file-ange-status (ange-ref)
  "Return status on ANGE-REF ftp buffer.

Return:
 'no-ange        if no ange buffer exists
 (..)            some ange status values"
  (let* ((ret   'no-ange)
         ange
         buffer
         host
         user
         proc
         line
         stat
         busy)
    (require 'ange-ftp)
    (setq ange  (ange-ftp-ftp-name ange-ref) ;crack addr
          host  (nth 0 ange)
          user  (nth 1 ange))
    (cond
     ((setq buffer (ti::buffer-find-ange-buffer user host))
      (if (null buffer) (setq buffer buffer)) ;XEmacs 19.14 Bytecomp silencer
      ;;  Create a new process if needed
      (setq proc (ange-ftp-get-process host user))
      ;;  The status value is valid only when process finishes.
      (save-excursion
        (set-buffer (process-buffer proc))
        (ti::pmax)
        (setq ret   ange-ftp-process-result
              line  (ti::read-current-line)
              stat  (ange-ftp-process-handle-line line proc)
              busy  ange-ftp-process-busy)
        ;; STAT
        ;; t     = skip message
        ;; ange-ftp-process-result-line = good
        ;; fatal, deletes process.
        (setq ret (list ret stat busy)))))
    ret))

;;; ----------------------------------------------------------------------
;;; - an easy interface to ange ftp to get dingle file in bg.
;;; - this actually is a "macro" or toplevel func to the
;;;   ti::file-ange-file-handle
;;;
(defun ti::file-ange-download-file (ange-ref download-dir &optional not-bg)
  "Download single file pointed by ANGE-REF in background to the DOWNLOAD-DIR.

Input:

  ANGE-REF      /login@site:/dir/dir/file.xx
  DOWNLOAD-DIR  valid directory where to put the file.
  NOT-BG        if non-nil the ftp is done in foregroung.

Return:

  nil           if job is done in background
  status        if in fg. Nil means failure."

  (let* (ange
         host
         user
         dir
         file
         to-dir)
    (require 'ange-ftp)
    (setq ange          (ange-ftp-ftp-name ange-ref) ;crack addr
          host  (nth 0 ange)
          user  (nth 1 ange)
          dir   (file-name-directory (nth 2 ange))
          file  (file-name-nondirectory (nth 2 ange))
          to-dir (expand-file-name download-dir))
    (ti::file-ange-file-handle 'get user host dir to-dir (list file) not-bg)))

;;; ----------------------------------------------------------------------
;;; - an easy interface to ange ftp to get/put wanted files
;;; #todo: sometimes ange hangs, rarely but... should check if
;;;        process is live somehow?
;;; #todo: check that no process is going in the buffer, so that it's
;;;        not called many times (overlapping).
;;;
(defun ti::file-ange-file-handle
  (mode user host dir lcd file-list &optional not-bg msg-func)
  "Get files from remote or put files to remote site.

Important:

  All directory names must be absolute

Input:

  MODE          'put or 'get
  USER          login name when logging to site
  HOST          site name
  DIR           remote site directory
  LCD           download local dir
  FILE-LIST     files to get from/put to remote site
  NOT-BG        should we wait until ange is done?
                nil = run on bg, non-nil = wait until done.
  MSG-FUNC      function to call after download completes. Should
                contain &rest args parameter. See more in ange-ftp.el
Return:

  nil           always if NOT-BG is nil
  status        if NOT-BG is non-nil. Value nil means that session
                failed."
  (let* ((func          (or msg-func 'ti::file-ange-completed-message))
         (max-try       5)
         (try           0)
         proc
         point
         ret)
    (require 'ange-ftp)
    (cond                               ;get commands
     ((eq mode 'get)
      (setq mode "mget"))
     ((eq mode 'put)
      (setq mode "mput")
      (setq func 'ignore))         ;can't use any function for this...
     (t
      (error "What mode?")))
    (if (not (ti::listp file-list))
        (error "file-list must be LIST and _not_ empty"))
    ;;  We need absolute directory names, because the FTP process
    ;;  running does not understand anything else.
    (setq lcd (expand-file-name lcd))
    ;;  Start FTP session if it does not exist
    ;;
    (setq proc (ange-ftp-get-process host user))
;;;    (setq M mode U user H host D dir L lcd F file-list P proc)
    ;;  - Expand remote site's directory reference
    (setq dir (ange-ftp-real-file-name-as-directory
               (ange-ftp-expand-dir host user dir)))
    ;;  Without this, the next command dies. This is already called in function
    ;;  ange-ftp-get-process, but for some unknown reason it must be called
    ;;  again to be sure: the hash mark size was sometimes nil
    (with-current-buffer (ange-ftp-ftp-process-buffer host user)
      (if (null ange-ftp-ascii-hash-mark-size)
          (setq ange-ftp-ascii-hash-mark-size 1024))
      (if (null ange-ftp-binary-hash-mark-size)
          (setq ange-ftp-binary-hash-mark-size 1024)))
    (ange-ftp-set-binary-mode host user)
    ;;  - After this commands ANGE hangs quite often and never executes
    ;;    the "raw" commands
    ;;  - That's why we loop MAX-TRY times to start the
    ;;    command.
    (ange-ftp-send-cmd host user (list 'lcd lcd) "Setting lcd...")
    (message "")
    ;;  CD command dies if it the directory is wrong
    ;;
    ;;  The socond command just makes sure the command was successfull.
    ;;  I added this, because when connection was cloased and ange
    ;;  opened the connection again, the CWD command didn't succeed
    ;;  right away. We must wait here until it succeeds and only then
    ;;  send the real put or get request.
    (ange-ftp-cd host user dir)
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq try 0)
      (while
          (and (progn
                 (ti::pmax) (forward-line -1)
                 ;;  ftp> 250 CWD command successful.
                 (not (string-match "success" (ti::read-current-line))))
               (< try max-try))
        (incf try)))
    (push mode file-list)               ;command for ange
    (save-excursion
      (set-buffer (process-buffer proc))
      (ti::pmax)
      ;;  Try sending untill the point moves... => process started
      (setq point (point)   try 0)
      (while (and (eq point (point))
                  (< try max-try))
;;;     (ti::d! (eq point (point)) point (point))
        (ange-ftp-raw-send-cmd
         proc
         (ti::list-to-string file-list)
         "ftp ..."                  ;message displayed during 0%..100%
         (list func)                    ;called after completion ?
         (not not-bg))                  ;continue without wait
        (ti::pmax)
        (incf try)))
    ;;  The status value is valid only when process finishes.
    (if not-bg
        (save-excursion
          (set-buffer (process-buffer proc))
          (setq ret ange-ftp-process-result)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-chmod-w-toggle (file)
  "Toggle read-only flag for FILE.
If file does not exist, or is not owned by user this function does nothing.

Return:

  'w+    file made writable
  'w-    file made read-only.
  nil    file not processed."
  (let* ((file (expand-file-name file))
         mode)
    (when (ti::file-modify-p file)
      (setq mode (ti::file-toggle-read-write (file-modes file)))
      (set-file-modes file mode)
      ;;  return value , -r--r--r-- , 600 oct= 384 dec
      (if (= 0 (logand mode 128))
          'w-
        'w+))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-chmod-make-writable (file)
  "Make FILE writable."
  (set-file-modes file (ti::file-mode-make-writable (file-modes file))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-chmod-make-read-only (file)
  "Make FILE read only."
  (set-file-modes file (ti::file-mode-make-read-only (file-modes file))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-find-shadows (&optional path)
  "Find duplicate files along optional PATH, which defaults to `load-path'."
  (interactive)
  (or path (setq path load-path))
  (save-excursion
    (let ((true-names   (mapcar 'file-truename path))
          (reduds       0)
          files dir
          out-buffer
          curr-files
          orig-dir
          files-seen-this-dir
          file
          d1
          d2)                           ;directories
      (while path
        (if (member (car true-names) (cdr true-names))
            (setq reduds (1+ reduds))
          (setq dir (car path))
          (setq curr-files (if (file-accessible-directory-p dir)
                               (directory-files dir nil ".\\.elc?$" t)))
          (and curr-files
               (interactive-p)
               (message "Checking %d files in %s..." (length curr-files) dir))
          (setq files-seen-this-dir nil)
          (while curr-files
            (setq file (car curr-files))
            (setq file (substring
                        file 0 (if (string= (substring file -1) "c") -4 -3)))
            (unless (member file files-seen-this-dir)
              (setq files-seen-this-dir (cons file files-seen-this-dir))
              (if (not (setq orig-dir (assoc file files)))
                  (setq files (cons (list file dir) files))
                (if (null out-buffer)
                    (progn
                      (setq out-buffer (get-buffer-create "*Shadows*"))
                      (display-buffer out-buffer)
                      (set-buffer out-buffer)
                      (erase-buffer)))
                ;; Do not print if directories are the same
                ;; ++ [jari]
                (setq d1 (file-name-as-directory (car (cdr orig-dir)))
                      d2 (file-name-as-directory dir))
                (unless (string= d1 d2)
                  (insert
                   (format "%s%s shadows\n%s%s\n\n" d1 file d2 file)))))
            (setq curr-files (cdr curr-files)))) ;; if
        (setq path       (cdr path)
              true-names (cdr true-names)))
      (if (interactive-p)
          (let ((msg
                 (if out-buffer
                     (let ((n (/ (count-lines (point-min) (point-max)) 3)))
                       (format "%d shadowing%s found" n (if (eq n 1) "" "s")))
                   "No shadowings found")))
            (message "%s%s" msg
                     (if (zerop reduds) ""
                       (format " (skipped %d redundant entr%s in path)"
                               reduds (if (eq reduds 1) "y" "ies"))))))
      out-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-part-last (dir)
  "Return last portion of DIR.
Like ~/this/dir/ would return `dir'.
for `dir/' return `dir'."
  (when (or (string-match "^.*[\\/]\\([^\\/]+\\)[\\/]?$" dir)
            (string-match "^\\([^\\/]+\\)[\\/]?$" dir))
    (match-string 1 dir)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-unique-roots (path-list)
  "Return unique root directories of PATH-LIST.
Non-strings or empty strings in PATH-LIST are ignored.

For example for directories ~/elisp/packages and ~/elisp/packages/more
the unique root is ~/elisp/packages."
  (with-temp-buffer
    (dolist (path path-list)
      (when (and (stringp path)
                 (not (ti::nil-p path)))
        (insert (expand-file-name path) "\n")))
    (sort-lines nil (point-min) (point-max))
    (ti::pmin)
    (let (list
          line)
;;;      (pop-to-buffer (current-buffer)) (ti::d! 'starting)
      (while (not (eobp))
        (setq line (ti::buffer-read-line))
        (push line list)
        (beginning-of-line)
        (save-excursion
          (delete-matching-lines (concat "^" (regexp-quote line)))))
;;;      (ti::d! 'ok list)
      list)))

;;; ----------------------------------------------------------------------
;;; (tinypath-subdirectory-list "~")
;;;
(defun ti::directory-subdirectory-list (path)
  "Return all subdirectories under PATH."
  (let* (list)
    (dolist (elt (directory-files path 'absolute) )
      (when (and (not (string-match "\\.\\.?$" elt)) ;; skip . and ..
                 (file-directory-p elt)) ;; take only directories
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-recursive-do (root function)
  "Start at ROOT and call FUNCTION recursively in each ascended directory."
  (let* ((list (ti::directory-subdirectory-list root)))
    (if (null list)
        (funcall function root)
      (dolist (path list)
        (ti::directory-recursive-do path function)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-up (path)
  "Go one PATH directory level up.

Cygwin hpath handling:

  /cygdrive/            => /              May not be what you want
  /cygdrive/c/          => /cygdrive/c    Can't go no more upward
  /cygdrive/c/tmp       => /cygdrive/c

Dos path handling:

  c:/temp               => d:/            Notice, cannot return \"d:\"

Unix path handling:

  /path1/path2          => /path1
  /path1/path2/         => /path1
  /path1/path2/file.txt => /path1/path2"
  (cond
   ((string-match "^/$\\|^[a-z]:[\\/]?$" path)
    path)
   (t
    (if (string-match "[/\\]$" path)
        (setq path (ti::string-match "^\\(.*\\)[^\\/]+" 1 path)))
    ;; /cygdrive/c/  is already a root directory
    (cond
     ((string-match "^\\(/cygdrive/.\\)/?$" path)
      (match-string 1 path))
     (t
      (setq path (file-name-directory path))
      ;;  d:/temp  => d:/   ,do not return "d:"
      (if (and (string-match "[/\\].+[/\\]" path)
               (string-match "^\\([a-z]:\\)?.+[^/\\]" path))
          (match-string 0 path)
        path))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-subdirs (dir)
  "Return directories under DIR."
  (let* (list)
    (when (file-directory-p dir)
      (dolist (elt (directory-files dir 'full))
        (if (file-directory-p elt)
            (push elt list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-unix-man-path-root ()
  "Determine manual page root path. "
  (let (root)
    (dolist (try '("/opt/local/man"     ;HP-UX new
                   "/usr/share/man"     ;HP old
                   "/usr/man"))         ;Sun and Linux
      (if (ti::win32-cygwin-p)
          (setq try (w32-cygwin-path-to-dos try)))
      (when (and try
                 (file-directory-p try))
        (return try)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::directory-files (dir re &optional absolute form not-re-form)
  "Return files from DIR.

Input:

  DIR           directory name
  RE            regexp for files to match
  ABSOLUTE      flag, Return files as absolute names?
  FORM          eval form, test each file with FORM instead of RE
  NOT-RE-FORM   eval form, drop file if this evaluates to t

Example:

  ;;  Get all filenames that aren't zipped, backups or objects.
  ;;  The 'arg' will hold the filename

  (ti::directory-files dir re t nil '(string-match \"g?[Z~#o]$\" arg)))

  ;; Return only directory names

  (ti::directory-files dir \".\" 'absolute
                   '(file-directory-p arg)
                   '(string-match \"\\\\.\\\\.?$\" arg))

Return:

  list          (file file file ..)"
  (let* (ret)
    (dolist (arg
             (directory-files dir absolute re))
      (when (or (null form)             ;accept all
                (eval form))            ;accept only these
        (when (or (null not-re-form)
                  (null (eval not-re-form)))
          (push arg ret ))))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun ti::file-files-only (list &optional eval-form)
  "Return existing files. Drop directories from LIST of strings.
Note: 200 files takes about 2-3 secs. If you supply EVAL-FORM, the item
will be included if the form Return t. You can refer to current item
with symbol 'arg'.

Input:

  LIST          list of strings
  EVAL-FORM     optional eval statement

Return:
 (file ..)     list"
  (let* (ret)
    (dolist (arg list)
      (if (if eval-form
              (eval eval-form)
            (and (file-exists-p arg)
                 (not (file-directory-p arg))))
          (push arg ret)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-newer-exist (f1 f2)
  "Return file F1 or F2 which is newer. If only one of them exist, return it.

Return:
  str   file
  nil   none of them exist"
  (cond
   ((and (file-exists-p f1)
         (file-exists-p f2))
    (if  (file-newer-than-file-p f1 f2)
        f1 f2))
   ((file-exists-p f1)
    f1)
   ((file-exists-p f2)
    f1)
   (t
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-get-extension (file &optional mode)
  "Return FILE extension.
If MODE is nil, then return nil if none exist,
if MODE is non-nil, return empty string instead."
  (let* (list
         ext
         len)
;;;    (ti::d! (null file) (null (string-match "\\." file)))
    (if (or (null file)
            (null (string-match "\\." file)))
        nil
      (setq list  (split-string file "[\.]"))
      (setq len   (length list))
      (if (eq 1 len)
          (setq ext (car list))          ; first element
        (setq ext (nth (1- len) list)))) ; last element
    (if ext ext                          ;what to return?
      (if mode
          ""
        nil))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-path-and-line-info  (path)
  "Return (PATH . LINE-NBR) if path is in format PATH:NBR."
  (let* (line)
    (when (string-match ":\\([0-9]+\\):?[ \t\f]*$" path)
      (setq line (string-to-int (match-string 1 path)))
      (setq path (ti::replace-match 0 "" path))
      (cons path line))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-path-to-unix (path)
  "Convert PATH to Unix forward slash format."
  (replace-char-in-string ?/ ?\\  path))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::file-path-to-msdos (path)
  "Convert PATH to MS-DOS backward slash format."
  (replace-char-in-string ?\\ ?/  path))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-make-path  (dir &optional file)
  "Make full path by combining DIR and FILE.
In Win32, return backward slashed paths. Otherwise forward slashed
paths.

DIR will always have trailing directory separator.
You need to call this function if you pass a absolute path to
external processes. Emacs in the other hand can handle both \\ and /
internally."
  (if (ti::emacs-type-win32-p)
      (replace-char-in-string
       ?\\ ?/  (concat (file-name-as-directory dir) (or file "")))
    (replace-char-in-string
     ?/ ?\\  (concat (file-name-as-directory dir) (or file "")))))

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'which 'ti::file-get-load-path)
;;;
(defun ti::file-get-load-path (fn paths &optional all-paths verb)
  "Return full path name for FN accross the PATHS.
Input:

  FN            filename to search
  PATHS         list of path names
  ALL-PATHS     return all matches.
  VERB          verbose flag. Allows printing values in echo area

Return:

  nil           no matches
  str           first match if all-paths is nil
  list          list of matches along paths."
  (interactive
   (let* ((map (copy-keymap minibuffer-local-map))
          var1
          var2)
     (define-key map "\t"   'lisp-complete-symbol)
     (define-key map "\C-m" 'exit-minibuffer)
     (setq var1 (read-from-minibuffer "sFile: "))
     (setq var2 (read-from-minibuffer "Lisp var: " "exec-path" map))
     (list var1 (eval (intern-soft var2)))))
  (let (file found)
    (ti::verb)
    (dolist (elt paths)
      (when (stringp elt)           ;you never know what's in there...
        (setq file (ti::file-make-path elt fn))
        (when (and (file-exists-p file)
                   (not (file-directory-p file)))
          (if all-paths
              (push file found)
            (setq  found file)
            (return)))))
    (if (and found all-paths)           ;preserve order
        (setq found (nreverse found)))
    (if (and found verb)
        (message (prin1-to-string found)))
    found))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-user-home ()
  "Try to guess user's home directory.

Return:
   /PATH/PATH/USER/    users home
   nil                 not found"
  (let* ((usr       (or (getenv "USER") (getenv "LOGNAME") ))
         (home      (or (getenv "HOME") (getenv "home") ))
         (path      (expand-file-name "~")))
    (cond
     (path)
     ((> (length home) 0)               ;$HOME exist
      (setq path home))
     ((> (length usr) 0)                ;users name exist
      (with-temp-buffer
        (cond
         ((executable-find "pwd")       ;Win32 test
          ;;   Try to get via 'pwd' process then.
          (call-process "pwd" nil (current-buffer) nil)
          (ti::pmin)
          (if (re-search-forward usr nil t)
              (setq path (buffer-substring (point-min) (match-end 0)))))
         ((executable-find "ls")
          ;;  Failed ? try ls then...
          (erase-buffer)
          (call-process "ls" nil (current-buffer) nil)
          (if (re-search-forward usr nil t)
              (setq path (buffer-substring
                          (point-min) (match-end 0)))))))))
    ;;  make sure it has  trailing "/"
    (and (stringp path)
         (setq path (ti::file-make-path path)))
    path))

;;; ----------------------------------------------------------------------
;;; You can use this in interactive command to build up a completion list:
;;; like this:
;;;
;;;  (interactive
;;;     (list (completing-read
;;;             "Visit file: " (ti::file-file-list load-path "\\.el$"))))
;;;   (let ((pair (assoc emacs-file (ti::file-file-list load-path "\\.el$"))))
;;;     (if pair
;;;             (find-file (cdr pair))
;;;       (find-file (expand-file-name emacs-file "~/emacs")))))
;;;
(defun ti::file-file-list (dirs re)
  "Read DIRS and return assoc of files matching RE. (FILE FULL-PATH-FILE)."
  (let ((files nil))
    (and (stringp dirs)                 ;only one entry given ?
         (setq dirs (list dirs)))
    (while dirs
      (setq files
            (append files (directory-files (car dirs) t re)))
      (setq dirs (cdr dirs)))
    (mapcar
     (function
      (lambda (file)
        (cons (file-name-nondirectory file) file))
      files))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-complete-file-name (file-name &optional dir flist)
  "Given a FILE-NAME string return the completed file name.

Input:

  If FILE-NAME is invalid entry, signal no error and return nil
  If no DIR is not given, use FILE-NAME's directory.
  If no DIR nor FILE-NAME dir, use `default-directory'
  if non-nil flag FLIST, then return completed filename list

Nots:

  DIR must end to a slash or otherwise it is considered partial
  filename.

Return:

  str           full completion
  list          list of completions if FLIST is set.
  nil           not unique"
  (let* ((type     (cond
                    ((and (ti::win32-p)
                          (ti::emacs-type-win32-p)
                          (string-match "/cygdrive" file-name))
                     'cygwin)
                    (t
                     'emacs)))
         (file         (substitute-in-file-name file-name))
         (uncomplete   (file-name-nondirectory file))
         odir
         completed)
    (setq odir                          ;Save the original directory.
          (substring file-name 0 (- (length file-name) (length uncomplete))))
    ;;  Relative path
    (if (and (stringp odir)
             (stringp dir)
             (string-match "^\\.\\." odir))
        (setq dir (format "%s%s" (file-name-as-directory dir) odir)))
    ;;  expand-file-name dies if default-directory is nil
    (setq dir
          (expand-file-name
           (or dir
               (file-name-directory file-name)
               default-directory
               "~")))
    (setq completed
          ;;   if given impossible entry like "!@#!#"
          (ignore-errors
            (file-name-all-completions uncomplete dir)))
    ;; Only one match in the list? voila!
    (if (and completed
             (eq 1 (length completed)))
        (setq completed (ti::file-name-forward-slashes (car completed))))
    (cond
     ((and (stringp completed)
           (not (string= completed uncomplete)))
      (concat odir completed))
     ((and flist completed)
      completed))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-complete-file-name-word (&optional word no-msg)
  "Complete filename WORD at point.
`default-directory' is used if no directory part in filename.
See `ti::file-complete-file-name'.

You can use this feature easily in Lisp interactive call.
See macro `ti::file-complete-filename-minibuffer-macro' for more.

NO-MSG  if non-nil, do not flash possible choices at current point
        The `sit-for' command is used for displaying, so you can
        interrupt it by pressing any key."
  (interactive "P")
  (or word
      (setq word
            (save-excursion
              (forward-char -1)
              (ti::buffer-read-space-word))))
  (let* ((oword  word)
         (enable-recursive-minibuffers t)
         all
         tmp
         dir
         msg)
    ;;  expand-file-name dies if default-directory is nil
    (or default-directory
        (error "default-directory is nil !!"))
    (unless (ti::nil-p word)
      (setq word (ti::file-complete-file-name word nil 'list))
      (when (ti::listp word)
        (let ((alist (ti::list-to-assoc-menu word)))
          (when (stringp (setq tmp (try-completion oword alist)))
            (setq word tmp
                  ;; still completions left? Was this unique?
                  all  (all-completions word alist)))))
      (when (stringp word)
        (when (and (null no-msg)
                   ;;  This completion is not unique, so show all matches
                   (string= oword word)
                   (ti::listp all))
          (setq msg (format "%d: %s"
                            (length all)
                            (ti::list-to-string all)))
          (message msg)
          (sit-for 0.5)))
      (when (and (stringp word)
                 (not (string= word oword)))
        (skip-chars-backward "^\n\t ")
        (let ((point (point)))
          (skip-chars-forward "^\n\t ")
          (delete-region point (point))
          (insert (ti::file-name-forward-slashes word)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::file-complete-filename-minibuffer-macro 'lisp-indent-function 0)
(defmacro ti::file-complete-filename-minibuffer-macro (&rest body)
  "Complete filename in minibuffer and do BODY.
Use variable 'map' to pass map to `read-from-minibuffer' function.

Example call:

  (ti::file-complete-filename-minibuffer-macro
    (read-from-minibuffer \"test\" nil map))

Example 2:

    (defun my-example (string file-list)
      \"FILE-LIST is string. Allow completion on words\"
      (interactive
       (list
        (read-from-minibuffer \"Gimme string: \")
        (split-string
         (ti::file-complete-filename-minibuffer-macro
           (read-from-minibuffer \"Gimme file-list: \" nil map)))))
      (list string file-list))

    (setq result (call-interactively 'my-example)) \"test\" RET <files> RET
    result
    --> (\"test\" (\"~/\" \"~/bin\" \"~/exe/\"))"
  (`
   (let* ((map (copy-keymap minibuffer-local-map)))
     ;;  this event also exists for tab
     (define-key map [kp-tab]   'ti::file-complete-file-name-word)
     (define-key map [tab]      'ti::file-complete-file-name-word)
     (define-key map "\t"       'ti::file-complete-file-name-word)
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::file-read-file-list (&optional message)
  "Read file or directory list as one string, and return it as LIST.
Display optional MESSAGE, otherwise use default message.

Filesnames can be completed with tab. `default-directory' is used for
files that do not have directory part. Make sure default dir has ending
slash.

Example:

  (setq files (mapcar 'expand-file-name (ti::file-read-file-list)))

Return:

  (ELT ELT ..)          with `default-directory'
  nil                   no input"
  (let* (list
         str)
    (setq str
          (ti::file-complete-filename-minibuffer-macro
            (read-from-minibuffer
             (or message (format
                          "...%s: "
                          ;; limit the directory name
                          (ti::string-right default-directory 10)))
             nil map)))
    (unless (ti::nil-p str)             ;not empty?
      (dolist (str (split-string str " "))
        (if (not (string-match "/" str))
            (setq str (concat default-directory str)))
        (push str list)))
    (nreverse list)))

;;}}}

;;{{{ Network streams

;;; ......................................................... &network ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-finger-error (&optional buffer)
  "Read BUFFER containing a finger response after `ti::process-finger'.
If there is an error, then return possible error cause string.

Return:
 string     cause of error
 nil        no error"
  (let* (ret)
    (with-current-buffer (or buffer (current-buffer))
      (ti::pmin)
      (when (re-search-forward "unknown host:" nil t)
        (setq ret (ti::read-current-line))))
    ret))

;;; ----------------------------------------------------------------------
;;; Original function in   mc-pgp.el:mc-pgp-fetch-from-finger
;;;
(defun ti::process-finger (email &optional port timeout buffer verb)
  "Finger EMAIL on PORT with TIMEOUT.
The output is clered from possible ^M characters.

Input:

  EMAIL             email address foo@site.com
  PORT              default is 79
  TIME              default is 25
  BUFFER            where to store result, default is *finger tmp*
  VERB              print verbose messages

Return:

  string            error while doing opening network stream
  buffer-pointer"
  (interactive "sFiger email: ")
  (let (connection
        user
        host
        ret)
    (setq verb      (or verb (interactive-p))
          port      (or port 79)
          timeout   (or timeout 25))
    (if (not (string-match "^\\([^ \t]+\\)@\\([^[ \t]+\\)" email))
        (error "Need email address foo@site.com '%s'" email)
      (setq user (match-string 1 email)
            host (match-string 2 email))
      (save-excursion
        (unwind-protect
            (progn
              (if verb     (message "Fingering %s ..." email))
              (setq buffer (or buffer (ti::temp-buffer "*finger tmp*" 'clear)))
;;;           (pop-to-buffer buffer) (ti::d! "going finger....")
              (condition-case error
                  (progn
                    (setq
                     connection
                     (open-network-stream "*finger*" buffer host port))
                    (process-send-string
                     connection (concat "/W " user "\r\n"))
                    (while (and (memq  (process-status connection) '(open))
                                (accept-process-output connection timeout))))
                (file-error
                 ;; '(file-error "connection refused "connection failed" ..)
                 (setq ret (ti::list-to-string (cdr error))))
                (error
                 (setq ret (ti::list-to-string (cdr error)))))
              (if connection (delete-process connection))
              ;;  Strip Ctrl-M marks
              (with-current-buffer buffer
                (ti::buffer-lf-to-crlf 'dos2unix)))))
      (when verb
        (message "Fingering %s ...done" email))
      (if (interactive-p)
          (pop-to-buffer buffer))
      (if connection
          buffer ret))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-http-request (command &optional port timeout buffer verb)
  "Send http COMMAND i.e. URL request.
Control character C-m is removed from response.

If COMMAND includes port number, e.g.:

  http://www-swiss.ai.mit.edu:80/htbin/pks-extract-key.pl

This is actually intepreted as

  http    = www-swiss.ai.mit.edu
  port    = 80
  command = /htbin/pks-extract-key.pl

Input:

  COMMAND       http command string
  PORT          default is 80
  TIMEOUT       default is 60
  BUFFER        where to store result, default is *finger tmp*
  VERB          print verbose messages

Return:

 '(buffer-pointer  error-string)

  error-string      network stream error message.
  buffer            HTTP response."
  (interactive "sHttp request: ")
  (let (connection
        host
        ret)
    (setq verb      (or verb (interactive-p))
          port      (or port 80)
          timeout   (or timeout 60))
    (if (not (string-match "^http://\\([^/]+\\)\\(/.*\\)" command))
        (error "Must be _http_ request '%s'" command)
      (setq host    (match-string 1 command)
            command (match-string 2 command))
      (if (string-match "\\(.*\\):\\([0-9]+\\)" host)
          (setq port (string-to-int (match-string 2 host))
                host (match-string 1 host))))
;;;   (ti::d!! "\n" command "HOST" host "PORT" port "TIME" timeout buffer)
    (save-excursion
      (unwind-protect
          (progn
            (when verb
              (message "Http %s ..." host))
            (setq buffer (or buffer (ti::temp-buffer "*http tmp*" 'clear)))
;;;         (ti::d! host port command "sending http....")
            (condition-case error
                (progn
                  (setq
                   connection
                   (open-network-stream "*http*" buffer host port))
                  (process-send-string
                   connection
                   (concat "GET "
                           command
                           " HTTP/1.0\r\n\r\n"))
                  (while (and (eq 'open (process-status connection))
                              (accept-process-output connection timeout))))
              (file-error
               ;; '(file-error "connection refused "connection failed" ..)
               (setq ret (ti::list-to-string (cdr error))))
              (error
               (setq ret (ti::list-to-string (cdr error))))))
        ;; ................................................... cleanup ...
        (if connection
            (delete-process connection))
        ;;  Strip Ctrl-M marks
        (with-current-buffer buffer
          (ti::buffer-lf-to-crlf 'dos2unix))))
    (when verb
      (message "Http %s ...done" host))
    (if (interactive-p)
        (pop-to-buffer buffer))
    (list buffer ret)))

;;}}}
;;{{{ shell: zipping

;;; ....................................................... &shell-zip ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-uname ()
  "Call `uname -a'."
  (let* ((uname (executable-find "uname")))
    (when uname
      (with-temp-buffer
        (call-process uname nil (current-buffer) nil "-a")
        (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
;;; #todo
;;; #not tested
;;;
(defun ti::process-zip (zip-file files &optional zip-cmd)
  "Achive to ZIP-FILE. FILES is list (file file ..).
The ZIP-CMD defaults to \"zip -9 -q\",
Command will not return until the process has finished."
  (let* ((zcmd          (or zip-cmd "zip -9 -q "))
         (shell-buffer  (get-buffer-create "*Shell output*"))
         (flist         (ti::list-join files))
         (cmd           (concat zcmd " " zip-file " " flist)))
    (call-process cmd nil shell-buffer)
    (if (interactive-p)
        (display-buffer shell-buffer))
    shell-buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-zip-view-command (file &optional buffer nice zip-cmd verb)
  "Insert zip file listing to point.

Input:

  FILE      tar file
  BUFFER    defaults to current buffer
  NICE      if non-nil, insert file name and empty lines around listing.
  ZIP-CMD   defaults to 'unzip -v %s'
  VERB      verbose mode

Return:

  nil       no action [file not exist ...]
  nbr       shell return code"
  (interactive "fTar file: ")
  (let* ((cmd (or zip-cmd "unzip -v %s")))
    (ti::verb)
    (if (not (and (stringp file)
                  (file-exists-p file)))
        (error "Invalid file argument")
      (if nice
          (insert "file " (file-name-nondirectory file) ":\n"))
      (call-process cmd nil (or buffer (current-buffer)) nil
                    (expand-file-name file))
      (if nice
          (insert "\n")))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-tar-zip-view-maybe-command (file)
  "If FILE is zip/tar then insert listing to current point."
  (cond
   ((string-match "\\.tar$\\|\\.tar.gz$\\|\\.tgz$" file)
    (ti::process-tar-view-command file nil 'nice))
   ((string-match "\\.zip$" file)
    (ti::process-zip-view-command file nil 'nice))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::process-perl-process-environment-macro 'lisp-indent-function 1)
(put 'ti::process-perl-process-environment-macro 'edebug-form-spec '(body))
(defmacro ti::process-perl-process-environment-macro (perl-type &rest body)
  "Check PERL-TYPE and run BODY in correct Win32/Cygwin environment.
Fixe TEMP variable during the process call.

Input:

  PERL-TYPE   'perl 'win32-cygwin 'win32-activestate
  BODY        Code to run."
  (`
   (let ((process-environment process-environment) ;; Make a local copy
         new)
     (dolist (elt process-environment)
       (cond
        ((string-match "^TEMP=\\(.*\\)" elt)
         (let* ((tmp-dir (match-string 1 elt))
                (dir     (if (and (stringp tmp-dir)
                                  (file-directory-p tmp-dir))
                             (expand-file-name tmp-dir))))
           (cond
            ((and (ti::win32-shell-p)
                  ;;  c:\temp  or \\server\temp
                  (not (string-match "=[a-z]:[\\]\\|=[\\][\\][a-z]" elt)))
             (if (file-directory-p "C:/TEMP")
                 (push "TEMP=C:\\TEMP" new)
               (push "TEMP=C:\\" new)))
            ((and (string-match "[\\]\\|[a-z]:" tmp-dir) ;; Dos path
                  (not (eq perl-type 'win32-activestate)))
             ;; Path must be in Unix format
             (let* ((path (if dir
                              (w32-cygwin-dos-path-to-cygwin dir)
                            "/tmp"))
                    (env  (format "PATH=%s" path)))
               (push env new)))
            (t
             (push elt new)))))
        ((string-match "^PAGER=" elt)) ;; Delete this
        (t
         (push elt new))))
     (setq process-environment new)
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-perl-version (&optional binary)
  "Check type of perl BINARY.

Return:

  (VERSION TYPE PATH OUTPUT)

  VERSION   Version number from command line option -version
  TYPE      is 'win32-activestate 'win32-cygwin or 'perl
  PATH      Path to the BINARY or `perl'.
  OUTPUT    Whole output of -v."
  (let* ((perl  (if binary
                    (executable-find binary)
                  (executable-find "perl")))
         version
         type
         string)
    (when perl
      (with-temp-buffer
        (call-process perl
                      nil
                      (current-buffer)
                      nil
                      "-v")
        (setq string (buffer-string)))
      (setq type
            (cond
             ((string-match "cygwin" string)
              'win32-cygwin)
             ((string-match "activestate" string)
              'win32-activestate)
             ((not (ti::nil-p string))
              'perl)
             (t
              (error "Unknown perl type: %s" string))))
      ;; This is perl, v5.6.1 built for cygwin-multi
      (when (string-match
             "This[ \t]+is[ \t]+perl[ ,v\t]+\\([0-9.]+\\)"
             string)
        (setq version (match-string 1 string)))
      (list version type perl string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-java-version (&optional binary)
  "Return java BINARY type and version number.

Return:

  (VERSION TYPE PATH FULL)

  VERSION   Version number from command line option -version
  TYPE      is 'sun or 'gcc or any other known Java vendor.
  PATH      Path to the BINARY or `java'.
  FULL      Whole output of -version."

  (let* ((java (executable-find (or binary "java")))
         version
         type
         string)
    ;;  Under Debian, `call-process' will hang during
    ;;  call to /usr/bin/java, which is a symlink
    (when (and java
               (file-symlink-p java))
      (message "TinyLib: %s is symlink, cannot get version." java)
      (setq java nil))
    (when java
      ;; #todo: gcj Java version?
      (with-temp-buffer
        (call-process java
                      nil
                      (current-buffer)
                      nil
                      "-version")
        (setq string (buffer-string)))
      (when
          ;; Java HotSpot(TM) Client VM (build 1.3.0_02, mixed mode)
          (or (string-match "build[ \t]+\\([0-9_.]+\\)" string)
              ;; Debian:
              ;;
              ;; java version "1.3.1"
              ;; Java(TM) 2 Runtime Environment, Standard Edition \
              ;;   (build Blackdown-1.3.1-02b-FCS)
              (string-match "java +version +\"\\([0-9][0-9.]+\\)" string))
        (setq version (match-string 1 string)))
      (cond
       ;; Java(TM) 2 Runtime Environment, Standard Edition (build 1.3.0_02)
       ((string-match "Java(TM)[ \t]+[0-9]" string)
        (setq type 'sun))
       (t
        (setq type 'gcc)))
      (list version type java string))))

;;}}}
;;{{{ shell: tar

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-tar-view-command (file &optional buffer nice verb test)
  "Insert tar file listing to point.

Input:

  FILE      tar file
  BUFFER    default to current buffer
  NICE      if non-nil, insert file name and empty lines around listing.
  VERB      verbose mode
  TEST      Do not execute command. Print what would happen.

Return:

  nil       no action [file not exist ...]
  nbr       shell return code"
  (interactive "fTar file: ")
  (let* ((def  (cond
                ((string-match "\\.tar$" file)
                 "tar tvf %s")
                ((string-match "\\.tar\\.gz$" file)
                 "gzip -d -c %s |tar -tvf -")
                ;;  don't know this currently ...
                ((string-match "\\.tgz$" file)
                 nil)))
         cmd)

    ;; Default tar switches:
    ;; -t       ,List the name
    ;; -v       ,verbose
    ;; -f       ,next arg argument as the name of the archive (file)
    ;;
    (ti::verb)
    (when (and
           (stringp file)
           (file-exists-p file)
           (progn
             (or (file-exists-p "/hp-ux/")
                 (file-exists-p "/vol/")
                 (and verb
                      (y-or-n-p
                       (format "\
Can't guess tar command, try using default %s ? " def))))
             (setq cmd def)))
      (if nice
          (insert "file " (file-name-nondirectory file) ":\n"))
      (call-process cmd nil (or buffer (current-buffer)) nil
                    (expand-file-name file))
      (if nice (insert "\n")))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::process-tar-read-listing-forward  ()
  "Read all tar filenames from current line forward.
The point is not preserved. The tar listing looks like:

r-xr-xr-x 240/222   4269 Feb  3 09:25 1997 aa.cc
r-xr-xr-x 240/222  41515 Feb  3 09:40 1997 bb.cc
r-xr-xr-x 240/222   3013 Feb  3 09:40 1997 dd.cc

or

-r--r--r-- foo/bar 14764 1998-06-22 15:05:55 file.txt

Return:

 '((FILE SIZE PERMISSIONS) ..)"
  (let* ((re (concat
              "^\\([drwx-]+\\)[ \t]+[0-9A-Za-z_]+/[0-9A-Za-z_]+"
              "[ \t]+\\([0-9]+\\)[ \t]+.*[0-9]:[0-9]+[ \t]+"
              "\\(.*\\)"))
         list)
    (beginning-of-line)
    (when (or (looking-at re)
              (re-search-forward re nil t))
      (beginning-of-line)
      (while (and (looking-at re)
                  (not (eobp)))
        (push (list (match-string 3) (match-string 2) (match-string 1)) list)
        (forward-line 1) ))
    (nreverse list)))

;;}}}
;;{{{ Reading lines, passwords

;;; ----------------------------------------------------------------------
;;;
(defun ti::query-read-input-invisible ()
  "Read keyboard input. If user presses ESC, the asking is interrupted.
Return:
  nil
  string"
  (let* ((echo-keystrokes 0)            ;prevent showing
         str
         ch)
    (while (not (ti::char-in-list-case ch '(?\n ?\C-m ?\e)))
      (cond
       ((ti::char-in-list-case ch '(?\b ?\177))
        (if (eq 0 (length str))
            (beep)
          (setq str (substring str 0 (1- (length str)))) ))
       ((ti::print-p ch)
        (setq str (concat str (char-to-string ch))) ))
      (setq ch (ti::read-char-safe-until)))
    (if (char= ch ?\e)
        (setq str nil))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun ti::query-read-input-as-password (&optional prompt max echo-char)
  "Return read password using PROMPT, MAX chacters with ECHO-CHAR.
If user presses ESC, return nil."
  (let* (
         (prompt                 (or prompt ""))
         (cursor-in-echo-area    nil)
         (max                    (or max 80)) ;maximum string
         (bar (if echo-char
                  (make-string (+ max 2) echo-char )
                (make-string (+ max 2) ?* )))
         str
         ch
         len)
    (message prompt)
    (while (not (ti::char-in-list-case ch '(?\n ?\C-m ?\e)))
      (cond
       ((or (ti::char-in-list-case ch '(?\b ?\177)))
        (setq len (length str))
        (unless (= len 0 )
          (setq str (substring str 0 (1- len)))) )
       ((ti::print-p ch)
        (if (>= (length str) max)
            (beep)                      ;signal error
          (setq str (concat str (char-to-string ch)))
          (message (substring bar 0 (length str)))) ))
      (setq ch (ti::read-char-safe-until
                (concat prompt (substring bar 0 (length str))))))
    (message "")
    (if (char= ch ?\e)
        (setq str nil))
    str))

;;}}}

;;{{{ misc: advice control

;;; ----------------------------------------------------------------------
;;;
(defun ti::advice-control
  (single-or-list regexp &optional disable verb msg)
  "Enables/disable SINGLE-OR-LIST of adviced functions that match REGEXP.
Signals no errors, even if function in LIST is not adviced.
All advice classes ['any] are ena/disabled for REGEXP.

Input:

  SINGLE-OR-LIST        function of list of functions.
  REGEXP                advice name regexp. Should normally have ^ anchor
  DISABLE               flag, if non-nil then disable
  VERB                  enable verbose messages
  MSG                   display this message + on/off indication"
  (dolist (func (ti::list-make single-or-list))
    (ignore-errors
      (if disable
          (ad-disable-advice  func 'any regexp)
        (ad-enable-advice     func 'any regexp))
      ;;change state
      (ad-activate func)))
  (if verb
      (message
       (concat
        (or msg "advice(s): ")
        (if disable "off" "on")))))

;;}}}

;;{{{ misc: -- packaging, install, reports

;;; ..................................................... &bug-reports ...
;;; - Take a look at lisp-mnt.el if you're writing
;;;   your own packages.

;;; ----------------------------------------------------------------------
;;; #defalias (defalias 'package-feedback 'ti::package-feedback)
;;;
(defun ti::package-submit-feedback (lib)
  "Composes feedback report with lisp-mnt.el conmoncerning Lisp file LIB.
Make sure the file beeing reported is valid according to
lisp-mnt's command `lm-verify'."
  (interactive "sSend mail regarding file: ")
  (let (file
        version
        buffer)
    (cond
     ((setq file
            (or (locate-library lib)
                (progn
                  (setq lib (concat lib ".gz"))
                  (locate-library lib))))
      (require 'lisp-mnt)
      (set-buffer (setq buffer (ti::find-file-literally file)))
      (setq version (ti::vc-rcs-buffer-version))
      (lm-report-bug
       (format "%s %s Feedback"
               (or version "")
               (file-name-nondirectory file)))
      (kill-buffer buffer))
     (t
      (error (concat "No such file in load path: " lib))))))

;;; ----------------------------------------------------------------------
;;; - See package tinydiff.el and function tdi-feedback there if you
;;;   are still curious how to use this function
;;;
(defun ti::package-submit-bug-report
  (lib id var-list &optional verb elts)
  "Submit bug report with reporter.

PRECONDITIONS before using this function

1. The file must be in version control and it must have the \"\$ Id \$\" identifier
   stored into variable. Like the following:

   (defconst tinylib-version-id
     \"\$ Id: tinylib.el,v 1.18 1996/01/24 09:44:48 jaalto Exp jaalto \$\"
     \"Latest modification time and version number.\")

2. The package must be valid according to lisp-mnt.el's command
   `lm-verify' so that the \"maintainer\" information can be extracted.
   This means that you file must have header like this:

  ;; Maintainer:   Foo Bar <foo@example.com>

Input:

  LIB           filename without path. E.g. \"tinylib.el\"
  ID            the RCS Id string
  VAR-LIST      list of variables to get from package. Like '(var1 var2)
  VERB          Verbose messages and questions.
  ELTS          a) Buffer to included in report.
                b) If this is functionp, then function must return a
                   string or buffer pointer to include.
                c) if this is boundp, the value is taken as buffer
                   name string."
  (interactive)
  (let* (maintainer
         subj
         list)
    (ti::verb)
    (require 'reporter)
    (setq maintainer
          (or (car-safe (ti::package-get-header lib  "maintainer")) ""))
    (setq list (split-string id " "))
    (setq subj (concat (nth 2 list) " " (nth 1 list))) ;; name && version
    ;; ................................................... compose mail ...
    (when (or (null verb)
              (y-or-n-p "Do you really want to submit a report? "))
      (reporter-submit-bug-report
       maintainer
       (nth 1 list)
       var-list
       nil nil
       (concat "Hi,\n"))
      ;; ............................................... insert content ...
      (let (status
            str
            name
            len
            function)
        (dolist (buffer elts)
          (setq str      nil
                status   nil
                function nil)
          ;; .............................................. detect type ...
          (cond
           ((stringp buffer)
            (setq status (get-buffer buffer)))
           ((memq buffer '(nil t))) ;; Ignore
           ((and (symbolp buffer)
                 (boundp buffer))
            (setq buffer (symbol-value buffer))
            (if (stringp buffer)
                (setq status (get-buffer buffer))
              (message "TinyLib: bug report ERROR. Malformed syntax %s"
                       (prin1-to-string buffer))
              (sleep-for 3)))
           ((functionp buffer)
            (setq function buffer)
            (setq status (funcall function))
            (cond
             ((stringp status)
              (setq str status))
             ((bufferp status)
              (setq buffer status)
              (setq status t)))))
          (when buffer
            (when (and (interactive-p)
                       (null status))
              (or
               (y-or-n-p (format "Buffer `%s' missing, continue? Are you sure? "
                                 (prin1-to-string buffer)))
               (error "Abort.")))
            ;; ................................................. insert ...
            (when status
              (setq name (cond
                          ((bufferp buffer)
                           (buffer-name buffer))
                          ((stringp buffer)
                           buffer)
                          (t
                           (symbol-name function))))
              (setq len (- 70 (length name)))
              (insert "\n\n[" name "] " (make-string len ?= ) "\n\n")
              (setq len (buffer-size))
              (if str
                  (insert str)
                (insert-buffer buffer))
              ;;  `insert-buffer' does not put point after insert,
              ;;  go there manually
              (when (> (buffer-size) len)
                (forward-char (- (buffer-size) len)))))))
      ;; ............................................... position point ...
      (ti::pmin)
      (if (re-search-forward "Subject: *" nil t)
          (insert subj))
      (re-search-forward "Hi,\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-version-info (lib &optional arg)
  "Gets package information and prints it to another buffer.
The LIB is searched along 'load-path'.

Preconditions:

  The file must be valid according to lisp-mnt.el::lm-verify

Interactive call:

  You can complete the filename with TAB key

Input:

  LIB   filename with .el added
  ARG   prefix arg, print the versionin info in mode-line
        instead of creating full version buffer."
  (interactive
   (let* (file)
     (setq
      file
      (ti::file-complete-filename-minibuffer-macro
        (read-from-minibuffer
         (format "[%s] Version info for library: " default-directory)
         nil
         map)))
     (if (null file)
         (setq file file))         ;XEmacs 19.14 bytecompiler silencer
     ;; Make sure there is .el
     (list
      (ti::string-verify-ends file ".el")
      current-prefix-arg)))
  (let (out
        file
        buffer
        tmp
        lm-version
        lm-summary
        lm-maintainer
        lm-creation-date
        lm-last-modified-date
        lm-commentary
        rcs-id
        maintainer-name
        maintainer-email)
    (cond
     ((setq file
            (or (locate-library lib)
                (progn
                  (setq lib (concat lib ".gz"))
                  (locate-library lib))))
      (require 'lisp-mnt)
      (cond
       ((not (null arg))
        (set-buffer (setq buffer (ti::find-file-literally file)))
        (setq rcs-id (or (ti::vc-rcs-str-find-buffer "Id") "<no rcs id>"))
        (kill-buffer buffer)
        (ti::read-char-safe-until rcs-id))

       (t
        (setq out (ti::temp-buffer "*version*" 'clear))
        ;;  Now get the information from file with lisp-mnt.el
        (with-current-buffer (setq buffer (ti::find-file-literally file))
          (setq
           lm-version             (lm-version)
           lm-summary             (lm-summary)
           lm-maintainer          (lm-maintainer)
           lm-creation-date       (lm-creation-date)
           lm-last-modified-date  (lm-last-modified-date)
           lm-commentary          (lm-commentary)
           rcs-id                 (ti::vc-rcs-str-find-buffer "Id")))
        (when (and (stringp lm-last-modified-date)
                   (eq 3 (length (setq tmp (split-string lm-last-modified-date))))
                   (eq 3 (length (nth 1 tmp))))
          ;;  Convert "16 Feb 2000" --> to ISO 8601 Date
          (setq lm-last-modified-date
                (format "%s-%s-%s"
                        (nth 2 tmp)
                        (ti::month-to-0number (nth 1 tmp))
                        (nth 0 tmp))))
        (kill-buffer buffer)
        (setq maintainer-name
              (if  (not (null lm-maintainer))
                  (or (car-safe lm-maintainer) "<name not known>")
                "<name not known>"))
        (setq maintainer-email
              (if  (not (null lm-maintainer))
                  (or (cdr-safe lm-maintainer) "no email info")
                "no email info"))
        (switch-to-buffer-other-window out)
        (insert
         lib " -- "        (or lm-summary            "<no info>")        "\n\n"
         "Created      : " (or lm-creation-date      "<no info>")        "\n"
         "Last modified: " (or lm-last-modified-date "<no info>")        "\n"
         "Maintainer   : " maintainer-name " <" (or maintainer-email "") ">\n"
         "Version      : " (or lm-version            "<no info>")        "\n"
         "\n\n"
         (or lm-commentary "<no commentary found>"))
        (pop-to-buffer  out)
        (ti::pmin) (ti::buffer-replace-regexp "^;;;" 0 "   ")
        (ti::pmin) (ti::buffer-replace-regexp "^;;"  0 "  ")
        (ti::pmin) (ti::buffer-lf-to-crlf 'dos2unix 'force)
        (ti::pmin))))
     (t
      (error (concat "No such file in load path: " lib))))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun ti::package-get-header (lib header-list)
  "Get standard header information: e.g. maintainer, version, author.
The valid syntax of these headers is defined in lisp-mnt.el.
Make sure the file being visited can be run with  lisp-mnt's
command `lm-verify'.

Input:

  LIB           the filename of the package, including \".el\"
  HEADER-LIST   string or list of strings. E.g. '(\"maintainer\")

Return:

  list          notice that empty hits are stored: '(nil nil ..)
  nil"
  (let ((header-list  (ti::list-make header-list))
        hit elt
        file
        buffer
        ret)
    (cond
     ((setq file (locate-library lib))
      (require 'lisp-mnt)
      (unwind-protect                   ;make sure file is removed
          (progn
            (set-buffer (setq buffer (ti::find-file-literally file)))
            (mapcar
             (function
              (lambda (header)
                (setq elt (lm-header header))
                (if elt                         ;did we find any ?
                    (setq hit t))               ;raise flag
                (push elt ret)))
             header-list))
        ;; Kill the file no matter what happens.
        (kill-buffer buffer)))
     (t
      (error (concat "No such file in load path: " lib))))
    (if (null hit)                    ;if no hits, clear the ret value
        (setq ret nil))
    ret))

;;; ......................................................... &package ...
;;; - Here is some special functions. When you insert some example to
;;;   your package, you can convert functions and text directly to
;;;   "poor man's shar" format :-)
;;; - With function ti::package-make-mode-magic, you just
;;;
;;;   1. Be in lisp mode
;;;   2. Select example area to be inserted into somewhere
;;;   3. call the functions --> The result is inserted into registed
;;;   4. Go to package buffer and insert the register contents there.
;;;
;;; - Likewise the user can rip these "shar" examples with function
;;;   ti::package-rip-magic
;;;
;;;   1. Select area and call the function. --> examples in register
;;;   2. Put them into your .emacs or another favourite file.
;;;
;;; - Use similar bindings
;;;   (global-set-key   "\C-cp" 'ti::package-make-mode-magic)
;;;   (global-set-key   "\C-cP" 'ti::package-rip-magic)

;;; ----------------------------------------------------------------------
;;
(defun ti::package-install-example (lib &optional re)
  "Install example setup for you from LIB.
The LIB must be normal source file name ending in '.el'.
Function tries to find $PackageInstallRe: 'REGEXP' $
line which has the installation code chars in the surrounding
quotes. The common practise is to have '^[ \t]*;;+[*]' for Lisp.
If that regexp is followed by char '_' it means that the line is left empty.

If you supply RE, it must have match in LEVEL 1.

Return:
  buffer pointer"
  (interactive "sLibrary: ")
  (let* ((tmp  "*ti::pkg*")
         (file (locate-library lib))
         (verb (interactive-p))
         ;;    There has to be " " after the ":" otherwise it's not
         ;;    rcs ident(1) compatible. Also before the last $ ,
         ;;    there must be space.
         (re   (or re "[$]PackageInstallRe: [ \t]*'\\(.*\\)' [$]"))
         (empty-line-ch   "_")
         bp                             ;buffer pointer
         id
         comment-re)
    (if (or (null file)
            (null (file-readable-p file)))
        (error (concat "Cannot locate/read " lib " in load-path: " file))
      (setq bp (ti::temp-buffer tmp 'clear))
      (with-current-buffer bp
        (insert-file-contents file)
        (ti::pmin)
        (if (or (null (re-search-forward re nil t))
                (null (match-end 1)))
            (progn
              (pop-to-buffer bp)
              (error (concat "Cannot find install regexp: " re)))
          (setq comment-re (match-string 1)) ;read match in level 1
          (if (ti::nil-p comment-re)
              (error (concat "Level 1 mismatch_" (match-string 0) "_" re)))
          (save-excursion (setq id (ti::vc-rcs-str-find "Id" )))
          (ti::package-rip comment-re empty-line-ch (point-min) (point-max) )
          (ti::pmin)
          ;;  And final touch, add version id if it existed.
          (if (null id )
              (insert (concat ";; No rcs id found.\n\n"))
            (insert (concat ";; " id "\n\n")))
          ;;  Show contents if user called interactively.
          (when verb
            (pop-to-buffer bp)
            (message "Automatic install done.")))))
    bp))

;;; ----------------------------------------------------------------------
;;
(defun ti::package-rip (re ch &optional beg end)
  "Delete section of commented text, so that only code remains.
The installed code portion should have RE at front of each line.

RE must have anchor ^ and CH must have some magic char to
mean empty line. like RE = '^;;+[*]' and CH = '_':

    ;;*  ;;This belongs to automatic install, below is empty line code
    ;;*  _

Input:

 RE            ,regexp matching the examples
 CH             character signifying empty lines
 BEG END        area bounds

Return:

  t or nil"
  (interactive)
  (let* (ret)
    (unless (and beg end)
      (pop-to-buffer (current-buffer))
      (error "ti::package-rip: Region not defined %s" (current-buffer)))
    (save-restriction
      (narrow-to-region beg end)
      (ti::pmin)
      (when (re-search-forward re nil t)
        (ti::pmin)
        (save-excursion (delete-non-matching-lines re))
        ;; Now we have only RE lines
        (while (not (eobp))
          (when (looking-at re)
            (delete-region (match-beginning 0) (match-end 0))
            (if (looking-at ch)         ;remove that char
                (delete-char 1)))
          (forward-line)
          (setq ret t))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-rip-magic (beg end &optional verb)
  "As `ti::package-rip' BEG END, except the area is pasted to temporary buffer.
Tthe lines are prepared AND the result is inserted to register. VERB.

Make sure your are viewing the piece of code in the same mode that it is
supposed to be used. Otherwise the magic syntax isn't regognized.

Return:
  t or nil"
  (interactive "r")
  (let* ((ob      (current-buffer))
         (str     (ti::package-make-var))
         (empty   "_")
         (reg     ?p)                   ; "p" as "package"
         ret
         re)
    (ti::verb)
    (if (ti::nil-p str)
        (error "\
Couldn't set rip syntax, maybe `comment-start' is not defined.")
      (with-temp-buffer
        (insert-buffer-substring ob beg end) ;get the area
        (setq re (concat "^" (regexp-quote str)))
        (setq ret (ti::package-rip re empty (point-min) (point-max)))
        (pop-to-buffer (current-buffer))
        (cond
         (ret
          (set-register reg (buffer-string))
          (if verb
              (message "Example ripped to register `%c' " reg)))
         (t
          (when verb
            (message "could find Rip regexp `%s' from region." re))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-make-mode-magic (beg end)
  "As `ti::package-make-mode', except BEG END is pasted to temporary buffer.
The lines are prepared AND the result is inserted to register.

Return:
  t or nil according to success."
  (interactive "r")
  (let* ((source (current-buffer))       ;source buf
         (m      major-mode)             ;we must use same mode
         (verb   (interactive-p))
         (reg    ?p))
    (with-temp-buffer
      (insert-buffer-substring source beg end)
      ;;  turning mode on may have effects, since it runs hooks...
      ;;
      (funcall m)                       ;turn on same mode
      (when (ti::package-make-mode (point-min) (point-max))
        (set-register reg (buffer-string))
        (if verb
            (message "example in register `%c'" reg))))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun ti::package-make-mode (beg end)
  "Make embedded package around BEG END according to mode.
** DOES NOT WORK FOR MODES WITH `comment-end' ***

Return:
  nil or t if successfull."
  (interactive "*r")
  (let* ((str     (ti::package-make-var))
         (empty   "_")
         ret)
    (if (not (ti::nil-p comment-end))
        (message "tinylib: Comment end found, cannot proceed.")
      (ti::package-make beg end str empty)
      (setq ret t))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-make-var ()
  "Return Packaging variable 'str' according to mode.
If mode has no comment syntax default ';;* ' is used."
  (let* ((cs comment-start)
         (cs (cond                      ;set up something special
              ((memq major-mode
                     '(lisp-mode emacs-lisp-mode lisp-interaction-mode))
               (setq cs ";;"))          ;default ';' isn't enough
              (t cs)))                  ;do not change it
         (str (if (null cs)
                  ";;* "
                ;; make sure there is space
                (concat cs "* "))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-make (beg end str ch)
  "Format area for automatic install.

Input:

  BEG END       area
  STR           string to be added at front
  CH            additional character for empty lines."
  (let* ((empty (concat str
                        (cond
                         ((integerp ch)
                          (char-to-string ch))
                         (
                          ch)))))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (min beg end))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")
            (insert empty)
          (insert str))
        (forward-line 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-create-on-file
  (file &optional buffer no-show no-desc)
  "Very simple autoload function generator out of FILE.
Optionally put results to BUFFER. NO-SHOW does not show buffer.

Note:

  Doesn't recognize ###autoload tags; reads only functions.

Input:

  FILE      Lisp .el to read
  BUFFER    Where to insert autoloads.
  NO-SHOW   Do not show autoload buffer
  NO-DESC   Do not include function description comments."
  (interactive "fConstruct lisp autoloads from file: ")
  (let* ((fn     (file-name-nondirectory file))
         (regexp (concat
                  "^(\\("
                  "defun\\|defmacro\\|defsubst"
                  ;; SEMI poe.el
                  "\\|defun-maybe\\|defsubst-maybe\\|defmacro-maybe"

                  "\\)"
                  "[ \t]+\\([^ \t\n(]+\\)[ \t]*"))
         list
         args
         func
         type
         str
         iact
         point
         read-buffer
         tmp)
    (or buffer
        (setq buffer (get-buffer-create (or buffer  "*Autoloads*"))))
    ;;   We want to say (autoload 'func "pacakge" t t)
    ;;   and not        (autoload 'func "pacakge.el" t t)
    ;;   so that .elc files can be used.
    (if (string-match "\\(.*\\).el" fn)
        (setq fn (match-string 1 fn)))
    (unless (setq read-buffer (find-buffer-visiting file))
      (setq read-buffer (setq tmp (ti::find-file-literally file))))
    (with-current-buffer read-buffer
      ;; Can't use forward-sexp etc otherwise
      (unless (string-match "lisp" (symbol-name major-mode))
        (let (emacs-lisp-mode-hook) ;; Run no hooks
          (if emacs-lisp-mode-hook  ;; Quiet ByteCompiler "unused var"
              (setq emacs-lisp-mode-hook nil))
          (emacs-lisp-mode)))
      (ti::append-to-buffer
       buffer  (concat "\n;; "
                       (file-name-nondirectory file)
                       "\n"
                       ";; "
                       file
                       "\n\n"))
      (ti::pmin)
      (while (re-search-forward regexp nil t)
        (setq iact nil                  ;interactive flag
              args nil
              type (match-string 1)
              func (match-string 2))
        (when (and func
                   (progn
                     (goto-char (goto-char (match-end 0)))
                     (when (search-forward "(" nil t)
                       (setq point (point))
                       (backward-char 1)
                       (forward-sexp 1)
                       (backward-char 1)
                       (setq
                        args
                        (subst-char-in-string
                         ;;  Convert multiline args to one line.
                         ?\n ?\
                         (buffer-substring point (point)) )))))
        (if (re-search-forward
             "[ \t\n]+([ \t]*interactive"
             (save-excursion (end-of-defun) (point))
             t)
            (setq iact "t"))
        (if (null args)
            (setq args (format ";; %-36s <args not known>\n" func))
          (if (> (length args) 32)
              (setq args (format ";; %-15s %s\n" func args))
            (setq args (format ";; %-36s %s\n" func args))))
        (push args list)
        ;; (autoload FUNCTION FILE &optional DOCSTRING INTERACTIVE TYPE)
        (setq str (format "(autoload '%-36s %s \"\" %s%s)%s\n"
                          func
                          (format "\"%s\"" fn)
                          (or iact "nil")
                          (if (string-match "defmacro" type )
                              " 'macro" "")
                          (if (string= type "defsubst")
                              (format ";;%s" type) "")))
        (ti::append-to-buffer buffer str)
        (setq iact "t")))
    (unless no-desc
      (with-current-buffer buffer
        (insert "\n")                   ;list arguments for functions.
        (dolist (elt list) (insert elt)))))
  (if tmp                          ;We loaded this to Emacs, remove it
      (kill-buffer tmp))
  (unless no-show
    (pop-to-buffer buffer)
    (ti::pmin))
  buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-create-on-directory
  (dir &optional buffer no-show no-desc)
  "Create autoloads from function definitions in lisp files in DIR.
Optionally put results to BUFFER. NO-SHOW does not show buffer.

Note:

  Doesn't recognize ###autoload tags; reads only functions.

Input:

  See argument description in function `ti::package-autoload-create-on-file'."
  (let* ((files (directory-files
                 dir
                 'full
                 "\\.el$")))
    (dolist (file files)
      (ti::package-autoload-create-on-file file buffer no-show no-desc))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-loaddefs-create-maybe (file)
  "Make sure `generated-autoload-file' exists for FILE."
  (unless (file-exists-p file)
    (let* ((name1 (file-name-nondirectory file)))
      (with-temp-buffer
        (insert
         (format ";;; %s -- " name1)
         "loaddef definitions of program files\n"
         ";;  Generate date: " (format-time-string "%Y-%m-%d" (current-time))
         "\n\
;;  This file is automatically generated. Do not Change."
         "\n\n"
         (format "\n(provide '%s)\n\n"
                 (file-name-sans-extension (file-name-nondirectory name1))))
        (ti::with-coding-system-raw-text
          (write-region (point-min) (point-max) file))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-loaddefs-dir-files (dir &optional regexp)
  "Return from DIR .el files that do not matching REGEXP.
TO-FILE is excluded from autoload search."
  (let* (ret)
    (dolist (file (directory-files dir 'abs))
      (when (and (not (file-directory-p file))
                 (string-match "\.el$" file)
                 (or  (null regexp)
                      (not (string-match regexp file))))
        (push file ret )))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-loaddefs-build-dir-1 (dir &optional regexp to-file)
  "Build autoloads in DIR not matching REGEXP TO-FILE."
  (let ((files (ti::package-autoload-loaddefs-dir-files dir regexp)))
    (when files
      (let* (
             ;;  the original Emacs autload.el var does not contain "^"
             ;;  and this picks up wrong autoload definitions e.g. in
             ;;  auctex/tex-info.el which contains code
             ;;  ;;; Do not ;;;###autoload because conflicts standard texinfo.el.
             ;;  (defun texinfo-mode ()
             ;;
             ;; (generate-autoload-cookie "^;;;###autoload")
             ;;
             ;;  ...but, we cannot do that because
             ;;  generate-autoload-cookie is not a regexp, because in
             ;;  autoload.el there is statement in
             ;;  generate-file-autoloads()
             ;;
             ;;      (regexp-quote generate-autoload-cookie)
             ;;
             find-file-hooks
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
        (ti::package-autoload-loaddefs-create-maybe to-file)
        (dolist (file files)
          ;; (message "TinyLib: Updating loaddefs %s %s"
          ;; generated-autoload-file file)
          (message "TinyLib: Updated loaddefs %s => %s" dir to-file)
          (update-file-autoloads file))
        (setq-default backup-inhibited original-backup-inhibited)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-loaddefs-build-dir
  (dir to-file &optional regexp force)
  "Build autoloads in DIR TO-FILE like like `update-file-autoloads' does.

Input:

  DIR       Directory
  TO-FILE   The autoload file
  REGEXP    Ignore files matching regexp.
  FORCE     If non-nil, delete previous TO-FILE."
  (let* ((generated-autoload-file to-file) ;; See autoload.el, must be bound
         (name          (file-name-nondirectory to-file))
         (buffer        (find-buffer-visiting to-file))
         load)
    (unless generated-autoload-file ;; just byte compiler silencer.
      (setq generated-autoload-file nil))
    ;;  Exclude to-file from search.
    (if regexp
        (setq regexp (concat regexp "\\|" (regexp-quote name)))
      (setq regexp (regexp-quote name)))
    (when buffer
      (ti::kill-buffer-safe buffer)
      (setq load t))
    (when (and force
               (file-exists-p to-file))
      (ti::file-delete-safe to-file))
;;;    (dolist (file (ti::package-autoload-loaddefs-dir-files dir regexp))
;;;      (message "TinyLib: loaddefs %s %s" generated-autoload-file file)
;;;      (update-file-autoloads file))
    (ti::package-autoload-loaddefs-build-dir-1 dir regexp to-file)
    (when (setq buffer (find-buffer-visiting to-file))
      (with-current-buffer buffer
        (let (buffer-auto-save-file-name
              auto-save-default)
          (save-buffer))))
    (when load ;;  Reload, because buffer was in Emacs
      (find-file-noselect to-file))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-directories (list)
  "Return only directories from LIST, excluding version control directories."
  (let* (ret)
    (dolist (elt list)
      (when (and (file-directory-p elt)
                 ;;  Drop . ..
                 (not (string-match
                       "[/\\]\\..?$\\|CVS\\|RCS"
                       elt)))
        (push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-autoload-loaddefs-build-recursive
  (dir regexp &optional force function)
  "Build like `update-file-autoloads' recursively below DIR.
Input:

  DIR       Root directory to start searching
  REGEXP    Regexp to exclude files.
  FORCE     Recreate TO-FILE from scratch by deleting previous.
            You should do this if you have renamed any files in the directories.
  FUNCTION  Function to return autoload filename for each directory.
            Called with arg `dir'. The default file is loaddefs.el."
  (interactive "DEmacs autoload build root:\nfTo file: ")
  (unless dir
    (error "need DIR"))
  (let* ((dirs (ti::package-autoload-directories
                (directory-files
                 (expand-file-name dir)
                 'abs)))
         (to-file (or (and function
                           (funcall function dir))
                      "loaddefs.el")))
    (cond
     (dirs
      (ti::package-autoload-loaddefs-build-dir dir to-file regexp force)
      (dolist (dir dirs)
        (ti::package-autoload-loaddefs-build-recursive
         dir regexp force function)))
     (t
      (ti::package-autoload-loaddefs-build-dir dir to-file regexp force)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::package-install-pgp-tar  (dir &optional log-buffer source test)
  "Install PGP signed tar block using DIR from the end of current buffer.
The 'BEGIN PGP MESSAGE' is searched from the end of buffer backward.

The TAR block in the buffer looks like this and it is base64 pgp
signed (clearsig is off) with Author's public key.

    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; owHsWc1vG0l2n0GwwYjA3pJLgEXKlNaSDJLilySblrWWLXrMrCQrpOydzcxA02wW
    ;; ...
    ;; ...
    ;; -----END PGP MESSAGE-----

This function

o   Asks to what directory the tar files are installed.
o   shows the log buffer and echoes commads used.
o   Calls pgp to unpack the signed block
o   Calls tar to unpack the files
o   temporary files are stored to TMP, TMPDIR or /tmp

Error conditions:

o   if 'pgp' executable is not found, function aborts.
o   if 'tar' executable is not found, function aborts.
o   if previously installed files exists, function aborts.

Input:

  DIR           where to unpack the files
  LOG-BUFFER    where to print log messages.
  SOURCE        instead of using current buffer, read this file"

  (interactive "DSave programs to directory: ")
  (let* (
         (pgp     (or (and (executable-find "pgp")
                           ;;  Do not use returned absolute path
                           ;;  due to platform independency
                           "pgp")
                      (message "TinyLib: Can't find `pgp'.")))
         (gpg     (or (and (executable-find "pgp")
                           "pgp")
                      (message "TinyLib: Can't find `gpg'.")))
         (pgp-bin (or pgp gpg))
         (tar     (or (executable-find "tar")
                      (error "TinyLib: Can't find 'tar'.")))
         (tmp     (or (and (getenv "TMP")
                           (ti::file-make-path (getenv "TMP")))
                      (and  (getenv "TMPDIR")
                            (ti::file-make-path (getenv "TMPDIR")))
                      "/tmp/"))
         ;;  This may be system dependent someday..
         (tar-opt-show "tvf")
         (tar-opt-x    "xvf")
         (obuffer  (current-buffer))
         (in-file  (expand-file-name (concat tmp "t.in")))
         (out-file (expand-file-name (concat tmp "t.out")))
         cmd
         in
         buffer
         beg
         end
         file-list
         list)
    (unless pgp-bin
      (error "TinyLib: PGP or GPG is required to unpack."))
    ;; We need to expand this for shell calls
    (setq dir (expand-file-name (ti::file-make-path dir)))
    (cond
     ((and source
           (not (file-exists-p source)))
      (error "TinyLib: Can't find '%s'" source))
     ((not (file-directory-p tmp))
      (error "TinyLib: Can't use directory '%s'. Set env variable TMP." tmp))
     ((not (file-exists-p dir))
      (error "TinyLib: No such directory %s." dir)))
    (setq buffer (ti::temp-buffer
                  (or log-buffer "*tinylib::install*")
                  'clear))
    (with-current-buffer buffer
      ;; .............................................. extract base64 ...
      (buffer-disable-undo)
      (if source
          (insert-file-contents source)
        (insert-buffer obuffer))
      (ti::pmax)
      (unless (re-search-backward
               (concat "^;;+[ \t]*\\(" (ti::mail-pgp-msg-end-line) "\\)")
               nil t)
        (pop-to-buffer (current-buffer))
        (error "TinyLib: Can't find PGP end %s " source))
      (setq end (match-beginning 1))
      (unless (re-search-backward
               (concat "^;;+[ \t]*" (ti::mail-pgp-msg-begin-line))
               nil t)
        (pop-to-buffer (current-buffer))
        (error "TinyLib: Can't find PGP beginning %s " source))
      (beginning-of-line)
      ;;  remove comments
      (delete-rectangle (point) end)
      ;;  Leave only the signed region, remove rest
      (delete-region (point-min) (point))
      (buffer-enable-undo)
      ;; .................................................... call pgp ...
      (setq cmd (format "%% rm %s %s\n"  in-file out-file))
      (unless test
        (ti::file-delete-safe (list in-file out-file)))
      (write-region (point-max) (point-min) in-file)
      (unless (file-exists-p in-file)
        (error "TinyLib: Writing PGP data failed to file %s" in-file))
      ;;  Write-file may have some strange modes, be sure we can read them
      ;;  384dec = 600oct
      (set-file-modes in-file (logior (file-modes in-file) 384))
      (erase-buffer)
      ;; Start showing the log to user
      (pop-to-buffer buffer)
      (insert cmd)
      (let* ((out-file          (ti::file-name-forward-slashes out-file))
             (default-directory (file-name-directory out-file))
             (file              (file-name-nondirectory out-file)))
        (insert (format "%% cd %s ; %s -o %s %s\n"
                        default-directory
                        pgp-bin
                        file
                        (file-name-nondirectory in-file)))
        (unless test
          (call-process pgp-bin
                        nil
                        buffer
                        nil
                        "-o" file (file-name-nondirectory in-file))
          (ti::pmin)
          (unless (re-search-forward "Plaintext filename:" nil t)
            (error "TinyLib: Can't proceed, PGP didn't set filename.")))
        ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. show tar content  ..
        (ti::pmax)
        (setq cmd  (format "cd %s ; %s %s %s"
                           default-directory
                           tar
                           tar-opt-show
                           file))

        (insert "% " cmd "\n") (setq beg (point))
        (unless test
          (call-process tar
                        nil buffer nil
                        tar-opt-show
                        file)
          (goto-char beg)
          (if (null (setq file-list (ti::process-tar-read-listing-forward)))
              (error "TinyLib: Can't find tar listing."))))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. ..  previously installed?  ..
      (setq list file-list)
      (dolist (elt list)
        (setq in (concat dir (car elt)))
        (when (file-exists-p in)
          (if (y-or-n-p
               (format
                "TinyLib: Previously installed file `%s'. Overwrite ? "
                in))
              (unless test
                (delete-file in))
            (error "Abort.")) ))
      (setq cmd  (format "cd %s ; tar %s %s"
                         (expand-file-name dir)
                         tar-opt-x
                         out-file))
      (insert "% "cmd "\n")
      (unless test
        (let* ((default-directory (expand-file-name dir)))
          (call-process tar nil buffer nil
                        tar-opt-x
                        (expand-file-name out-file))))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . clean  ..
      (when (y-or-n-p "TinyLib: Clean up tmp files? ")
        (push in-file file-list)
        (push out-file file-list)
        (dolist (elt file-list)
          (insert (format "%% rm %s\n" elt))
          (unless test
            (ti::file-delete-safe elt) )))
      (message "TinyLib: installation to %s complete" dir))))

;;}}}
;;{{{ misc: XEmacs compatibility

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-installation-root ()
  "Return XEmacs installation root directory without trailing slash.
If this is queried unde Emacs, `exec-path' must contain XEmacs binary,
otherwise `load-path' is conculted."
  (let* ((xemacs   (ti::xemacs-p))
         (ver      (if xemacs
                       (ti::emacs-version-number-as-string))) ;eg "19.14"
         match
         ret)
    (dolist (path (if xemacs
                      load-path
                    exec-path))
      ;;  When we find the version from the path, ve know the root
      ;;  directory
      ;;
      ;;  /opt/local/lib/xemacs-19.14/lisp/vms -->
      ;;  /opt/local/lib/xemacs-19.14/lisp/
      (when (and (stringp path)
                 (string-match "xemacs" path)
                 (if ver
                     ;; running under XEmacs, we know what to look for.
                     (setq match (ti::string-match
                                  (concat "^.*" ver) 0 path))
                   ;; Take a guess, anything that looks like XEmacs in path
                   (setq match
                         (ti::string-match
                          ;;  XEmacs-21.2.36/ or XEmacs/21.2.36/
                          "^\\(.*xemacs[-\\/][0-9]+\\.[0-9.]*[0-9]\\)[\\/]"
                          1 path))))
        (setq ret (concat match "/lisp"))
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-overlay-some ()
  "Return some existing overlay that is used in Emacs.
Usually the primary mouse selection. You can use this function to get an
overlay that you can move in text if you don't want to create
new overlay.

Return:
  overlay symbol"
  (cond
   ((and (ti::xemacs-p)
         (boundp 'primary-selection-extent))
    'primary-selection-extent)
   ((and (ti::emacs-p)
         (boundp 'mouse-drag-overlay))
    'mouse-drag-overlay)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-overlay-properties  (overlay)
  "Return properties of OVERLAY."
  (cond
   ((ti::overlay-supported-p)
    (ti::funcall 'overlay-properties overlay))
   ((ti::xemacs-p)
    (ti::funcall 'extent-properties overlay))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-overlays-at (point)
  "Return overlays at POINT."
  (cond
   ((ti::overlay-supported-p)
    (ti::funcall 'overlays-at point))
   ((ti::xemacs-p)
    (let* (list)
      (ti::funcall
       'map-extents
       (function (lambda (ov maparg) (push ov list)))
       (current-buffer) point point)
      list))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-overlay-put (ov-sym prop val)
  "Set properties to overlay OV-SYM. Put PROP VAL pair to OV-SYM."
  (cond
   ((ti::overlay-supported-p)
    (ti::funcall 'overlay-put (symbol-value ov-sym) prop val))
   ((ti::xemacs-p)
    (ti::funcall 'set-extent-property (symbol-value ov-sym) prop val))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-overlay-move (ov-sym beg end &optional make-local face)
  "Move overlay OV-SYM to BEG END. Overlay is created if it does not exist.
MAKE-LOCAL localizes the overlay. If the overlay is created,
then FACE is assigned to it (default 'highlight)"
  (cond
   ((ti::overlay-supported-p)
    ;; ................................................ create overlay ...
    ;;  later XEmacs may have overlay emulation
    (or (symbol-value ov-sym)           ;Exist?
        (progn
          (if make-local (make-local-variable ov-sym))
          (set ov-sym
               (ti::funcall 'make-overlay (point) (point)))
          (ti::funcall 'overlay-put
                       (symbol-value ov-sym)
                       'face (or face 'highlight))))
    ;; .......................................................... move ...
    (ti::funcall 'move-overlay (symbol-value ov-sym)
                 beg end (current-buffer)))
   ((ti::xemacs-p)
    (or (symbol-value ov-sym)           ;Exist?
        (progn
          (if make-local (make-local-variable ov-sym))
          (set ov-sym
               (ti::funcall 'make-extent (point) (point)))
          (ti::funcall 'set-extent-property
                       (symbol-value ov-sym)
                       'face (or face 'highlight))))
    (ti::funcall 'set-extent-endpoints
                 (symbol-value ov-sym)
                 beg end (current-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-activate-region  (&optional off)
  "Activate region or turn the region OFF."
  (if (ti::emacs-p)
      (ti::funcall 'transient-mark-mode (if off 0 1)) ;From Simple.el
    (if off
        (ti::funcall 'zmacs-deactivate-region)
      (set 'zmacs-regions (if off nil t)) ;Avoid bute compile mesage in Emacs
      (ti::funcall 'activate-region))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-read-password  (&optional prompt)
  "Read password with PROMPT which defaults to 'Password: '."
  (let* ((var-bind  (boundp 'record-keystrokes))
         ;; If a GC occurred during that timing window, and a core dump was
         ;; forced later, the core might contain the string.
         ;;  --> use most-positive-fixnum
         (gc-cons-threshold (* 1024 1024))
         record-keystrokes)             ;XEmacs 20.4
    (setq prompt (or prompt "Password: "))
    (prog1
        (cond
         ((ti::xemacs-p)
          ;; if one follows the
          ;; - as soon as you are done with the returned string,
          ;;   destroy it with (fillarray string 0).
          ;;
          (require  'passwd)            ;utils/passwd.el
          (ti::funcall 'read-passwd prompt))
         (t
          ;;  Could also use (comint-read-noecho prompt)
          ;;  Comint won't echo anything.
          (ti::query-read-input-as-password prompt)))
      ;; ByteComp silencer; non used variable
      (if record-keystrokes
          (setq record-keystrokes nil))
      ;;  In old Emacs versions 19.35< and XEmacs 19.16< 20.3<
      ;;  you can actually read the password from lossage buffer with C-h l
      ;;
      ;;  --> We can clear it by filling it with 100 new characters.
      ;;      But this really works in XEmacs only, because Emacs
      ;;      Doesn't log events from macros.
      ;;
      (cond
       ((fboundp  'clear-lossage)
        (ti::funcall 'clear-lossage))
       ((fboundp  'clear-recent-keys)
        (ti::funcall 'clear-recent-keys))
       ((and (ti::xemacs-p)
             (not var-bind))
        (save-window-excursion
          (with-temp-buffer
            ;; force writing "1"  x 100 in this buffer
            ;;
            (switch-to-buffer (current-buffer))
            (ti::dotimes counter 1 100 (execute-kbd-macro "1")))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-key-local-map (key)
  "Return local map function for KEY"
  (let* ((prop      (text-properties-at (point)))
         (map       (and  prop
                          (nth 1 (memq 'keymap prop))))
         (function  (and  map
                          (lookup-key map key))))
    function))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-key-call-original (minor-mode-symbol key-binding)
  "Turn of MINOR-MODE-SYMBOL and execute original KEY-BINDING.
This won't work on mouse commands that examine the mouse `event'"
  (let* ((map           (or (current-local-map) global-map))
         (function      (lookup-key map key-binding))
         (this-command  (if function function this-command)))
    (when (and (not (ti::bool-p function))
               (symbolp function)
               (fboundp function))
      (unwind-protect
          (progn
            (put minor-mode-symbol 'ti::orig-value-key
                 (symbol-value minor-mode-symbol))
            (set minor-mode-symbol nil)
            ;;  This is very simplistic call. E.g. mouse event should
            ;;  be called with  (funcall function event)
            (call-interactively function)))
      ;; Make sure minor mode setting is restored
      (set minor-mode-symbol
           (get minor-mode-symbol 'ti::orig-value-key)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-mouse-position-coordinates ()
  "Return '(LINE COLUMN) where mouse pointer is currently.
If mouse is not supported, return nil."
  (when (fboundp 'mouse-position)
    (let ( ;; (frame (car (mouse-position)))
          (x  (cadr (mouse-position)))
          (y  (cddr (mouse-position))))
      ;;  window-list returns all windows starting from TOP. Count
      ;;  Lines in every window and compare that to mouse-position
      (let ((win (get-buffer-window (current-buffer)))
            (count 0))
        (save-window-excursion
          (dolist (elt (window-list))
            (when (eq elt win)
              (return))
            (select-window elt)
            ;;  Modeline is not counted as +1
            (setq count (+ count (window-height)))))
        ;; (ti::d! count x y)
        (list (1+ (- y count))
              ;;  In Emacs 21.x there is a "fringe" that mouse-position
              ;;  reports as X=0,
              (if (eq x 0)
                  ;; Consider "fringe" as column 0
                  0
                ;; Removed "fringe" count
                (1- x)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-mouse-key (event)
  "Return mouse key for EVENT."
  (cond
   ((ti::emacs-p)
    (make-vector 1 (car event)))
   ((ti::xemacs-p)
    (vector
     (append (event-modifiers event)
             (list (intern
                    (format
                     "button%d"
                     (ti::funcall 'event-button event)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-mouse-call-original-function (minor-mode-symbol &optional event)
  "Return original function behind MINOR-MODE-SYMBOL with mouse EVENT.
See. `ti::-xe-mouse-call-original'."
  (let* (ret
         flyspell-p)
    (or event
        (setq event last-input-event))
    (when (or (null minor-mode-symbol)
              (not (symbolp minor-mode-symbol))
              (not (boundp minor-mode-symbol)))
      (error "Invalid minor-mode-symbol `%s'." minor-mode-symbol))
    ;;  Turn off minor mode, so that we can see the real
    ;;  function behind it.
    (put minor-mode-symbol 'ti::orig-value (symbol-value minor-mode-symbol))
    (set minor-mode-symbol nil)
    ;; Unfortunately if flyspell is active (mouse-2 binding), ir does not look
    ;; key definition of mouse-2, but a `this-command-keys',
    ;; which is not correct.
    ;; => Turn off flyspell if there is no flyspell overlay underneath
    (when (and (boundp 'flyspell-mode)
               flyspell-mode
               (fboundp 'flyspell-overlay-p)
               (not (ti::funcall 'flyspell-overlay-p (overlays-at (point)))))
      (put minor-mode-symbol 'ti::orig-value-flyspell flyspell-mode)
      (setq flyspell-p t)
      (setq flyspell-mode nil))
    (setq ret (key-binding (ti::compat-mouse-key event))) ;Read it
    ;; Restore active modes
    (when flyspell-p
      (put minor-mode-symbol 'ti::orig-value-flyspell flyspell-mode))
    (set minor-mode-symbol (get minor-mode-symbol 'ti::orig-value))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defvar ti::-xe-mouse-call-original nil "See ti::keymap-mouse-call-original.")

(defun ti::compat-mouse-call-original (minor-mode-symbol &optional event)
  "Execute original mouse function by turning of MINOR-MODE-SYMBOL.
EVENT is mouse event. You use this function to to handle 'hot spots' in the
buffer and in other places you call the original function.

Do nothing if original function does not exist.
Does nothing when called by a function which has earlier been called
by us.

Example for some minor mode implementation:

ext-pro  (defun folding-mode-context-sensitive  (event)
    (interactive \"e\")
    ;; If test.. if test..no, then call original function
    (ti::compat-mouse-call-original 'folding-mode event))

Note:

  Works in XEmacs and Emacs

Sets global:

  `ti::-xe-mouse-call-original'"
  ;; Without the following test we could easily end up in a endless
  ;; loop in case we would call a function which would call us.
  (if ti::-xe-mouse-call-original ;; We're looping already
      nil
    (setq ti::-xe-mouse-call-original t)
    (unwind-protect
        (let* ((orig-buf (current-buffer))
               (mouse-func (ti::compat-mouse-call-original-function
                            minor-mode-symbol event))
               (local-func (ti::compat-key-local-map
                            (ti::compat-mouse-key event)))
               (orig-func  (or local-func
                               mouse-func))
               (event-p    (when orig-func
                             (string-match
                              "event"
                              (or (ti::function-args-p orig-func)
                                  "")))))
          (when orig-func
            ;;  Only if existed
            ;;  call it with the event as argument.
            ;;  We have to restore the current buffer too, because
            ;;  the minor mode is there.
            (put minor-mode-symbol 'ti::orig-value
                 (symbol-value minor-mode-symbol))
            (unwind-protect
                (if event-p
                    (funcall orig-func event)
                  ;;  Try direct call first, or pass the EVENT
                  (or (eq 'done (progn (call-interactively orig-func) 'done))
                      (eq 'done (progn (funcall orig-func event) 'done))))
              (set-buffer orig-buf)
              (set minor-mode-symbol (get minor-mode-symbol
                                          'ti::orig-value)))))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  unwind  ..
      ;; This is always executed, even if the above generates an error.
      (setq ti::-xe-mouse-call-original nil))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-popup (string-list &optional event mode title)
  "Show STRING-LIST pop up. If EVENT is nil, use default tinylib coordinates.
Works in XEmacs and Emacs.

Input:

  STRING-LIST   '(str str ..)
  EVENT         mouse-event or nil
  MODE          if non-nil, return selection NBR [0..n]. Normally
                returns the selection itself.
  TITLE         title of popup

Return:

  selection     member or nbr
  nil           nothing selected"
  (interactive "e")
  (let* ((title  (or title ""))
         (count  0)
         ;;  Allow calling from key press also.
         (event  (or event
                     (ti::compat-make-x-popup-event
                      ti::var-x-coord  ti::var-y-coord)))
         menu
         item-list
         alist
         ret)
    (when (ti::listp string-list)
      (setq alist  (ti::list-to-assoc-menu string-list))
      (cond
       ((ti::emacs-p)
        (setq item-list  alist)
        (setq menu
              (cons title
                    (list (cons title item-list))))
        (if (fboundp 'x-popup-menu)
            (setq ret (ti::funcall 'x-popup-menu  event menu)))
        (if ret
            (if (null mode)
                (setq ret (nth ret string-list)))))
       (t
        ;; Scenario: User selects item from menu-bar-menu which calls
        ;; function that should be called from mouse press --> selecting
        ;; from pull-down-menu, is not a mouse event!
        ;;
        ;; First one is real mouse call for function; the other one
        ;; is called from popup selection
        ;;
        ;;      #<buttondown-event button1>
        ;;      #<misc-user-event (call-interactively tig-index-x-popup)>
        ;;
        ;; get-popup-menu-response call breaks if EVENT is something
        ;; else than mouse-event. Check it immediately and set EVENT
        ;; to nil, because the parameter is optional.
        (if (and event (null (ti::funcall 'mouse-event-p event)))
            (setq event nil))
        ;;  Menu format is like this in XEmacs
        ;;
        ;; '("title" ["A" ("A") t] ["B" ("B") t] ["C" ("C") t]
        (setq item-list string-list)
        (setq menu
              (mapcar
               (function
                (lambda (x &optional vec)
                  (setq vec (make-vector 3 nil))
                  (aset vec 0 x)
                  (aset vec 1 (list x))
                  (aset vec 2 t)
                  vec))
               item-list))
        (setq menu (push title menu))
        ;; #todo, I don't know why there is nothing in the RET
        ;; after the selection has been done...
        ;;  See menubar.el
        ;;
        (setq ret (ti::funcall 'get-popup-menu-response  menu event ))
        (if (ti::funcall 'misc-user-event-p ret)
            (setq ret (car-safe (ti::funcall 'event-object  ret))))
        (when (and ret mode)            ;find position in list
          (dolist (arg menu)
            (when (and (vectorp arg)
                       (string= ret (elt arg 0)))
              (setq ret  (1- count))
              (return))
            (incf count))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-display-depth  ()
  "Return how many colors display can show."
  (cond
   ((ti::emacs-p)
    (ti::funcall 'x-display-planes (symbol-value 'x-display-name)))
   (t
    (ti::funcall 'device-bitplanes (ti::funcall 'default-x-device)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-read-event ()
  "Read X event."
  (cond
   ((ti::emacs-p)
    (if (fboundp 'event-to-character)
        (ti::funcall 'read-event)
      (error "Cannot read events.")))
   (t
    (ti::funcall 'next-command-event))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-executing-macro ()
  "Check if executing macro."
  (cond
   ((boundp 'executing-macro)
    (symbol-value 'executing-macro))    ;Emacs and old XEmacs
   ((boundp 'executing-kbd-macro)       ;New XEmacs
    (symbol-value 'executing-kbd-macro))))

;; briefly: events in 19.28, see subr.el
;; -------------------------------------------
;; event       :(mouse-3 (#<window 34 on *scratch*> 128 (20 . 104) -23723628))
;;
;; (setq event-start event)
;; event-start :(#<window 34 on *scratch*> 128 (20 . 104) -23723628))
;;                                         |   |          time
;;                               mouse point   coordinates
;;
;; (setq posn-col-row event-start) --> turn (20 . 104) into (col row)
;;
(defun ti::compat-make-x-popup-event (x y)
  "Make fake EVENT using X and Y coordinates.
Very handy if you call from kbd a function that requires mouse event."
  (cond
   ((ti::emacs-p)
    (list (list x  y) (selected-window)))
   (t
    ;;; (message "ti::compat-make-x-popup-event, XEmacs implementation not known.")
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-make-fake-event (x y &optional mouse-sym)
  "Make fake event using X and Y coordinates and MOUSE-SYM[mouse - 1].

Remeber: this is not full blown fake, just sufficent one, if
receiver uses any of 'posn-' function, this doesn't fool it."

  ;; (mouse-1 (#<window 42 on tinylib.el> 271088 (92 . 138) -492011))
  (cond
   ((ti::emacs-p)
    (list
     (or mouse-sym 'mouse-1 )
     (list
      (selected-window)
      1                                 ;<just some calue>
      (cons x y )
      -23723628)))
   (t
    ;; (message "ti::compat-make-fake-event, XEmacs implementation not known.")
    ;;
    ;; You can't create fake events in XEmacs.  The object data is
    ;; hidden behind an abstraction layer and there are no functions to
    ;; build or modify event objects.  You can only allocate and copy
    ;; them.
    ;;
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-modeline-update ()
  "XEmacs and Emacs Compatibility. Update modeline."
  (cond
   ((and (ti::xemacs-p)
         (fboundp 'redraw-modeline))
    ;; Xe 19.14
    ;; force-mode-line-update is an obsolete function; use redraw-modeline
    (ti::funcall 'redraw-modeline))
   ((fboundp 'force-mode-line-update)
    (ti::funcall 'force-mode-line-update))
   (t
    (set-buffer-modified-p (buffer-modified-p)))))

;;; ----------------------------------------------------------------------
;;; - Changing the frame label is same as changing the icon label
;;;
(defun ti::compat-set-frame-parameter (prop-or-list value &optional frame)
  "Use PROP-OR-LIST and VALUE to set FRAME's parameters.
When called interactively, set name of the frame.

Input:
 PROP-OR-LIST       alist of parameters or single property name
                    '((param . val) ..)
 VALUE              only used if single property given.
 FRAME              defaults to current frame."
  (interactive
   (list
    'name
    (read-from-minibuffer "frame label name: ")))
  (let* ((frame (or frame (selected-frame))))
    (cond
     ((and (ti::xemacs-p)
           (fboundp 'set-frame-properties))
      ;; #todo:  Why don't these work in XEmacs 19.14 ?
      (if (ti::listp prop-or-list)
          (ti::funcall 'set-frame-properties frame prop-or-list)
        (ti::funcall 'set-frame-property frame prop-or-list value)))
     (t
      (if (not (ti::listp prop-or-list))
          (setq prop-or-list (list (cons prop-or-list value))))
      (ti::funcall 'modify-frame-parameters frame prop-or-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-set-frame-name (string &optional frame get)
  "Change the frame display STRING in FRAME.
The implementation works differently in various emacs versions.

If GET is non-nil return frame name."
  (let* ((symbol 'name))
    (when (ti::emacs-p)
      ;; somewhere along the line the symbol was renamed to 'title
      ;; #todo: 19.31 - 33, frame, Would someone confirm this?
      (when (and (> emacs-minor-version 31)
                 (< emacs-minor-version 34))
        (setq symbol 'title)))
    (if get
        (frame-parameter frame symbol)
      (ti::compat-set-frame-parameter symbol string frame))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-frame-window-config ()
  "Return list '((FRAME WINDOW-CONFIGURATION) (F W) ..)."
  (let (ret)
    (dolist (elt
             (cdr (current-frame-configuration)))
      (push (list (nth 0 elt) (nth 2 elt))  ret))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;; XEmacs 19.14 "window-system is an obsolete variable; use (console-type)"
;;;
(defun ti::compat-window-system  ()
  "XEmacs and Emacs Compatibility, Mimic Emacs `window-system' variable.
In XEmacs the `cosole-type' returns 'tty on terminal, but this function
return nil to be in par with Emacs behavior. An 'tty is not a windowed
environment."
  (cond
   ((fboundp 'console-type)
    (let ((val (ti::funcall 'console-type)))
      (unless (eq 'tty val)
        val)))
   ((boundp 'window-system)
    (symbol-value 'window-system))))

;;; ....................................................... &xe-timers ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-timer-list-control (&optional mode)
  "Timer handling: MODE can be 'save 'restore or 'kill.

Example:

  ;; Turn off all processes for a while...

  (ti::compat-timer-list-control 'save)
  (ti::compat-timer-list-control 'kill)

  ;; ... do something

  ;;  Now restore the prosesses

  (ti::compat-timer-list-control 'restore)"

  (let* ((sym
          (cond
           ((boundp 'timer-alist)  'timer-alist)
           ((boundp 'timer-list)   'timer-list)
           ((boundp 'itimer-list)  'itimer-list))))
    ;;  We store/restore the list into the timer variable symbol
    ;;  properties.
    (cond
     ((eq 'kill mode)
      (set sym nil))
     ((eq 'save mode)
      (put sym 'ti::saved (symbol-value sym)))
     ((eq 'restore mode)
      (set sym (get sym 'ti::saved))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::compat-timer-control
  (&optional time repeat function delete verb)
  "With `run-at-time' TIME REPEAT FUNCTION keep or remove timer. VERB."
  (let* (timer)
    (ti::verb)
    (ti::compat-timer-cancel-function function)
    (cond
     (delete
      (if verb (message "TinyLib: timer process %s removed." function)))
     (t
      ;; this will also restart timer
      ;; In Emacs 19.28 - 19.30 , you could pass parameter
      ;; "now", but later emacs releases do not accept it.
      ;;
      (setq timer
            (run-at-time time repeat function))

      (if verb
          (message "TinyScroll: timer process started."))))
    timer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-timer-elt  (function)
  "Search FUNCTION and return timer elt.
You can use this function to check if some function is currently
in timer list. (ie. active)

The timer lists are searched in following order:

  `itimer-list'
  `timer-list'
  'timer-idle-list'

Return:

  '(timer-elt timer-variable)"
  (let* (pos
         list
         item
         ret)
    (flet ((get-elt (elt place)
                    (if (vectorp elt)
                        (aref elt place)
                      (nth place elt))))
      (dolist (timer '( ;; (("Mon Dec  9 10:01:47 1996-0" 10 tipgp-process nil))
                       (timer-idle-list . 5)
                       (timer-alist . 2)
                       (timer-list  . 2) ;; 19.34+
                       (itimer-list . 3)))
        (when (boundp (car timer))
          (setq list (symbol-value (car timer))
                pos  (cdr timer))
          ;;  NOTE: this is different in Xemacs. It is not a vector
          ;; timer-[idle-]list Emacs 19.34
          ;;  NOTE: this is different in Xemacs. It is not a vector

          ;; ([nil 12971 57604 0 60 display-time-event-handler nil nil])
          ;; [nil 13971 14627 646194 60
          ;;      (lambda (f) (run-at-time ...))
          ;;      (irchat-Command-keepalive) nil]
          (if (and (ti::emacs-p)
                   (vectorp (car list)))
              (setq pos 5))
          (dolist (elt list)
            (setq item (get-elt elt pos))
            (when (or (and (symbolp item)
                           (eq item function))
                      ;;  It may be lambda expression
                      (and (functionp item)
                           (string-match (regexp-quote (symbol-name function))
                                         (prin1-to-string
                                          (get-elt elt (1+ pos))))))
              (setq ret (list elt (car timer)))
              (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-timer-process-status ()
  "XEmacs and Emacs Compatibility. Return timer process status: t if active."
  (cond
   ((boundp 'timer-alist)               ;Emacs
    (symbol-value 'timer-process))
   ((boundp 'timer-list)                ;Emacs 19.34
    (ti::compat-timer-elt  'display-time-event-handler))
   ((boundp 'itimer-list)               ;
    ;; it is built in in XEmacs
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-timer-cancel  (key &optional cancel-function)
  "Delete timer KEY entry, where KEY is full element in (i)`timer-alist'.
Function `ti::compat-timer-cancel-function' may be more what you want
if you know the function in timer list."
  (let (var)
    (if (null key)
        nil                             ;Do nothing
      (when (and (null var)
                 (boundp 'timer-alist)) ;Emacs
        (setq var 'timer-alist)
        (ti::funcall 'cancel-timer key)
        (set var (delete key (symbol-value 'timer-alist))))

      (when (and (null var)
                 (boundp 'timer-list))  ;Emacs 19.34
        (setq var 'timer-list)
        ;;  Must use this command
        (ti::funcall 'cancel-timer key))
      (when (and (null var)
                 (boundp 'timer-idle-list)) ;Emacs 19.34
        (setq var 'timer-idle-list)
        ;;  Must use this command
        (ti::funcall 'cancel-timer key))
      (when (and (null var)
                 (boundp 'itimer-list)) ;XEmacs
        (setq var 'itimer-list)
        (ti::funcall 'cancel-itimer key)
        (set var (delete key (symbol-value 'itimer-list))))
      var)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-timer-cancel-function (function)
  "Delete all timer entries for FUNCTION."
  (let (key
        ret)
    (while (setq key (car-safe (ti::compat-timer-elt function)))
      (push key ret)
      (ti::compat-timer-cancel key))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::compat-set-mode-line-format  (fmt)
  "Set modeline format using FMT."
  (let* ((sym
          (if (ti::emacs-p)
              'mode-line-format
            'modeline-format)))
    ;; XEmacs 19.14 says:
    ;; ** mode-line-format is an obsolete var; use modeline-format instead.
    (set sym fmt)))

;;}}}
;;{{{ misc: create standard functions, variables

;;; .......................................................... &fmacro ...

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrov-minor-mode
  (pfx
   mode-Name
   mode-Name-prefix-key
   easymenu-Name
   custom-group

   &optional style)
  "Return standard minor mode variables.
See below how to call this function  from the top of your minor mode package.

Input:

  PFX                   string, the package prefix, usually one or two
                        words. E.g. \"xxx\" or \"xxx-mode\"
  MODE-NAME             string; which is displayed in modeline, should have
                        leading space. E.g. \" Lisp\"
  MODE-NAME-PREFIX-KEY  string, Key sequences to access the minor mode
                        functions.
  EASYMENU-NAME         string, the Menu bar name string.
  CUSTOM-GROUP          symbol, the defcustom.el group name.
  PREFIX-STYLE          string, How the characters should be named.
                        if nil then uses standard Emacs naming.

Example, when:

  PFX               is \"xxx-\"
  STYLE             is nil              ;; Standard Emacs style

  (defvar xxx-mode                      nil)
  (make-variable-buffer-local           'xxx-mode)

  (defvar xxx-mode-name                 MODE-NAME)
  (defvar xxx-mode-prefix-key           MODE-NAME-PREFIX-KEY)
  (defvar xxx-mode-map                  nil)
  (defvar xxx-mode-prefix-map           nil)
  (defvar xxx-mode-define-keys-hook     nil)
  (defvar xxx-mode-hook                 nil)
  (defvar xxx-mode-easymenu             nil)
  (defvar xxx-mode-easymenu-name        nil)

Example, when:

  PFX               is \"xxx\"
  STYLE             is 'xxx-:

  (defvar xxx-mode                      nil)
  (make-variable-buffer-local           'xxx-mode)

  (defvar xxx-:mode-name                MODE-NAME)
  (defvar xxx-:mode-prefix-key          MODE-NAME-PREFIX-KEY)
  (defvar xxx-:mode-map                 nil)
  (defvar xxx-:mode-prefix-map          nil)
  (defvar xxx-:mode-define-keys-hook    nil)
  (defvar xxx-:mode-hook                nil)
  (defvar xxx-:mode-easymenu            nil)
  (defvar xxx-:mode-easymenu-name       nil)

How to call this function:

  (ti::macrov-minor-mode \"xxx\" \" Xmode\" \"C-cx\" \"Xmenubar\" nil)"
  (` (, (ti::macrov-minor-mode-1
         pfx
         mode-Name
         mode-Name-prefix-key
         easymenu-Name
         custom-group
         style))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrov-minor-mode-1
  (pfx
   mode-Name
   mode-Name-prefix-key
   easymenu-Name
   custom-group

   &optional prefix-style)
  "Use `ti::macrov-minor-mode' and see call arguments there.
PFX MODE-NAME MODE-NAME-PREFIX-KEY
EASYMENU-NAME CUSTOM-GROUP PREFIX-STYLE"
  (let* ((x "-")
         sym
         ret)
    (if prefix-style
        (if (not (stringp prefix-style))
            (error "style must be string")
          (setq x prefix-style))
      (setq x pfx))
;;;    (push 'progn ret)
    ;; Force seeing variables at compile time
    ;;
    ;; Note 97-09-27
    ;; Thee started to appear errors from easymenu define command and
    ;; after byte compiler was forced to see the defvar definitions
    ;; of the variables during compile time, the compile was clean again.
    ;;
    ;; This was very odd.
    ;;
    ;;  (easy-menu-define
    ;;   tdi-:mode-easymenu
    ;;   tdi-:mode-map               << if not defvar seen, gives error
    ;;   "Elp summary sort menu."
    ;;   nil
    ;;   )
    (push 'eval-and-compile ret)
    (setq sym (intern (format "%smode" pfx)))
    (push (list 'defvar (` (, sym)) nil
                "mode on off variable.")
          ret)
    (push (list 'make-variable-buffer-local (` (quote (, sym)))) ret)

    (setq sym (intern (format "%smode-name" x)))
    (push (list 'defcustom (` (, sym))
                (` (, mode-Name))
                "*Minor mode name."
                ':type ''string
                ':group (` (, custom-group)))
          ret)
    (setq sym (intern (format "%smode-prefix-key" x)))
    (push (list 'defcustom (` (, sym))
                (` (, mode-Name-prefix-key))
                "*Prefix key to access mode."
                ':type ''(string :tag "Key sequence")
                ':group (` (, custom-group)))
          ret)
    (setq sym (intern (format "%smode-map" x)))
    (push (list 'eval-and-compile
                (list
                 'defvar (` (, sym))
                 nil
                 "Minor mode map."))
          ret)
    (setq sym (intern (format "%smode-prefix-map" x)))
    (push (list 'eval-and-compile
                (list
                 'defvar (` (, sym))
                 nil
                 "Prefix minor mode map."))
          ret)
    (setq sym (intern (format "%smode-easymenu" x)))
    (push (list 'defvar (` (, sym))
                nil
                "Easymenu variable.")
          ret)
    (setq sym (intern (format "%smode-easymenu-name" x)))
    (push (list 'defcustom  (` (, sym))
                (` (, easymenu-Name))
                "*Easymenu name that appears in menu-bar."
                ':type ''string
                ':group (` (, custom-group)))
          ret)
    (setq sym (intern (format "%smode-define-keys-hook" x)))
    (push (list 'defcustom (` (, sym))
                nil
                "*Hook that defines all keys and menus."
                ':type ''hook
                ':group (` (, custom-group)))
          ret)
    (setq sym (intern (format "%smode-hook" x)))
    (push (list 'defcustom (` (, sym))
                nil
                "*Hook that runs when mode function is called."
                ':type ''hook
                ':group (` (, custom-group)))
          ret)
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-minor-mode
  (func-min-sym
   doc-str

   install-func                         ;3
   mode-var
   mode-Name                            ;5
   prefix-var
   menu-var                             ;7

   no-mode-msg
   mode-desc                            ;9

   hook
   &optional body)
  "Create standard functions for minor mode.

Input:

  FUNC-MIN-SYM  symbol, the name of the function that is created.
                E.g. 'xxx-mode

  DOC-STR       string, the function documentation string

  INSTALL-FUNC  symbol, if func-min-sym isn't in `minor-mode-alist', this
                function is called to install the minor mode.

  MODE-VAR      symbol, a variable which turns minor mode on or off
  MODE-NAME     symbol, a variable, contains mode name.
  [PREFIX-VAR]  symbol, a variable, mode's prefix key. Can be nil
  [MENU-VAR]    symbol, a variable, mode's menu definitions. The menu must be
                in format of easy-menu.el so that it is Emacs and
                XEmacs compatible

  [NO-MODE-MSG] if non-nil, then default mode turn on or off message
                is not displayed. The default message is
                'MODE-DESC mode minor mode is ON. Prefix key is XXX'
  MODE-DESC     string, used in the default turn on message, see above.

  [HOOK]        symbol, hook that is run when mode is called.

  [BODY]        Lisp code to be added inside middle body. Can be nil.

Created function's arguments:

  (&optional arg verb)
  ARG           is mode on off variable. nil toggles mode.
  VERB          is set in interactive call and controlls printing mode
                turn on or off message. If nil, then no messages are
                displayed.

Example how to use this macro:

  ;;; We have to inform autoload that function exist after macro
  ;;;###autoload (autoload 'xxx-mode          \"package-file\" t t)

  (ti::macrof-minor-mode
   xxx-mode
   \"XXX minor mode. This helps you to do ....

  Defined keys:
  \\\\{xxx-mode-prefix-map}
  \"
    xxx-install-mode
    xxx-mode
    xxx-:mode-name
    xxx-:mode-prefix-key
    nil                     ;; no menu variables
    nil
    \"XXX\"
    xxx-:mode-hook
    ;; The forms
    ;;
    (progn
      (message \"Hey!\")))

Example how to call created functions:

  (xxx-mode)            ;; toggles
  (xxx-mode 1)          ;; on
  (xxx-mode 0)          ;; off, could also be -1
  (turn-on-xxx-mode)    ;; function can be put to hook
  (turn-off-xxx-mode)"
  (` (,
      (ti::macrof-minor-mode-1
       func-min-sym
       doc-str

       install-func
       mode-var
       mode-Name
       prefix-var
       menu-var

       no-mode-msg
       mode-desc

       hook
       body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-1
  (func-min-sym
   doc-str                              ;1

   install-func                         ;2
   mode-var                             ;3
   mode-Name                            ;4
   prefix-var                           ;5
   menu-var                             ;6

   no-mode-msg                          ;7
   mode-desc                            ;8

   hook                                 ;9
   &rest body)                          ;10
  "Use macro `ti::macrof-minor-mode'. And see arguments there.
FUNC-MIN-SYM DOC-STR INSTALL-FUNC MODE-VAR
MODE-NAME PREFIX-VAR MENU-VAR NO-MODE-MSG MODE-DESC
HOOK BODY"

;;;  (ti::d!! "\n\n" body)
  (let* ((sym
          (intern (symbol-name (` (, func-min-sym)))))
         (viper-sym
          (intern (concat (symbol-name (` (, func-min-sym)))
                          "-viper-attach"))))
    (`
     (defun (, sym)
       (&optional arg verb)
       (, doc-str)
       (interactive "P")
       (ti::verb)
       (if (null (assq (quote (, func-min-sym)) minor-mode-alist))
           ((, install-func)))
;;;       (let* ((val (symbol-value  (, mode-var)))
;;;              )
;;;         (setq  (, mode-var) (ti::bool-toggle val arg)))
       (ti::bool-toggle (, mode-var) arg)
       ;;  XEmacs needs this call, in emacs turning on the minor
       ;;  mode automatically adds the menu too.
       ;;
;;;       (if (symbol-value (, mode-var))
;;;           (easy-menu-add (symbol-value (, menu-var)))
;;;         (easy-menu-remove (symbol-value (, menu-var))))
       (if (and (, mode-var)
                (, menu-var))
           ;;  easy-menu-add dies if menu-var is nil
           (easy-menu-add (, menu-var))
         (easy-menu-remove (, menu-var)))
       (when (, mode-var)
         (funcall (quote (, viper-sym))))
       (,@ body)
       (ti::compat-modeline-update)
       (if (and verb (null (, no-mode-msg)))
           (message
            "%s minor mode is %s %s"
            (, mode-desc)
            (if  (, mode-var) "on." "off.")
            (if  (null (, mode-var))
                ""
              (if (, prefix-var)
                  (format "Prefix key is %s" (, prefix-var))
                ""))))
       (run-hooks (quote (, hook)))
       ;;  Return status of minor mode as last value.
       (, mode-var)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-on (mode-func-sym)
  "Create standard function to turn on the minor mode MODE-FUNC-SYM."
  (let* ((sym
          (intern (concat "turn-on-" (symbol-name (` (, mode-func-sym)))))))
    (`
     (defun (, sym) ()
       "Turn minor mode on"
       (interactive)
       ((, mode-func-sym) 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-off (mode-func-sym)
  "Create standard function to turn off the minor mode MODE-FUNC-SYM."
  (let* ((sym
          (intern (concat "turn-off-" (symbol-name (` (, mode-func-sym)))))))
    (`
     (defun (, sym) ()
       "Turn minor mode off"
       (interactive)
       ((, mode-func-sym) -1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-help (mode-func-sym)
  "Create standard function to print MODE-FUNC-SYM function's destription."
  (let* ((sym (intern (concat (symbol-name (` (, mode-func-sym))) "-help"))))
    (`
     (defun (, sym) ()
       "Mode help."
       (interactive)
       (with-output-to-temp-buffer "*help*"
         (princ (documentation (quote (, mode-func-sym)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-commentary (pfx mode-func-sym)
  "Create standard function to print PFX MODE-FUNC-SYM Commentary."
  (let* ((name pfx) ;; (symbol-name (` (, mode-func-sym))))
         (sym  (intern (concat name "commentary")))
         (file1 (substring pfx 0 (1- (length name))))
         (file2 (concat file1 ".el")))
    (`
     (defun (, sym) ()
       "Display `finder-commentary'."
       (interactive)
       ;; Same as what `finde-commentary' uses
       ;; One problem: lm-commentary has a bug, which causes killing
       ;; the file from emacs after it's done. But we don't want that
       ;; if use is viewing or loaded it to emacs before us.
       ;;
       ;; Work around that bug.
       (let ((buffer (or
                      (get-buffer (, file2))
                      (find-buffer-visiting (, file2))
                      (find-buffer-visiting (, file1)))))
         (if (not buffer)
             (finder-commentary (, file2))
           ;;  This is only a pale emulation....will do for now.
           (let (str)
             (with-current-buffer buffer
               (setq str (lm-commentary))
               (with-current-buffer (ti::temp-buffer "*Finder*" 'clear)
                 (insert str)
                 (ti::pmin) (ti::buffer-replace-regexp "^;+" 0 "")
                 (ti::pmin) (ti::buffer-replace-regexp "\r" 0 "")
                 (display-buffer (current-buffer)))))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-viper-attach (pfx mode-func-sym)
  "Create standard function PFX MODE-FUNC-SYM to attach mode to viper."
  (let* ((name pfx) ;; (symbol-name (` (, mode-func-sym))))
         (sym  (intern (concat (symbol-name (` (, mode-func-sym)))
                               "-viper-attach")))
         (file1 (substring pfx 0 (1- (length name)))))
    (`
     (defun (, sym) ()
       "Attach minor mode to viper with `viper-harness-minor-mode'."
       (if (featurep 'viper)
           (ti::funcall 'viper-harness-minor-mode (, file1)))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-minor-mode-install
  (func-ins-sym
   mode-sym
   map-sym
   prefix-map-sym
   mode-name-sym
   hook-sym
   &rest body)
  "Return standard function form.
Returned function will install and remove minor mode.

Input:

  FUNC-INS-SYM  symbol, the name of the function that is created.
                E.g. 'xxx-install-mode

  MODE-SYM      function symbol to call to run the mode e.g. 'xxx-mode

  MAP-SYM       mode's keymap symbol. E.g. 'xxx-mode-map

  MODE-NAME-SYM mode's name symbol. E.g. 'xxx-mode-name

  HOOK-SYM      hook symbol to call when mode has been installed.
                e.g. 'xxx-key-define-hook, which calls necessary
                functions to install keys and menus.

  BODY          Lisp forms executed in the beginning of function.

Created function's arguments:

  (&optional remove verb)
  REMOVE        uninstall minor mode
  VERB          is set for interactive calls: non-nil allows
                displaying messages.

How to call this function:

   (ti::macrof-minor-mode-install
    xxx-install-mode
    xxx-mode
    xxx-:mode-map
    xxx-:prefix-map-sym
    xxx-:mode-name
    xxx-:mode-define-keys-hook
    (progn
     ;; Lisp forms here
     nil))

Example how to call created function:

  M -x xxx-install-mode      ;; this calls created function and installs mode
  (xxx-install-mode)         ;; Same
  (xxx-install-mode 'remove) ;; Or prefix ARG, removes the minor mode"
  (` (, (ti::macrof-minor-mode-install-1
         func-ins-sym
         mode-sym
         map-sym
         prefix-map-sym
         mode-name-sym
         hook-sym
         body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-install-1
  (func-ins-sym
   mode-sym
   map-sym
   prefix-map-sym
   mode-name-sym
   hook-sym

   &rest body)
  "Use macro `ti::macrof-minor-mode-install'. See arguments there.
FUNC-INS-SYM MODE-SYM MAP-SYM MODE-NAME-SYM HOOK-SYM BODY"
  (let* ((sym (intern (symbol-name (` (, func-ins-sym))))))
    (`
     (defun (, sym) (&optional remove verb)
       "Install or optionally REMOVE minor mode. Calling this always
removes old mode and does reintall."
       (interactive "P")
       (ti::verb)
       (,@ body)
       (cond
        (remove
         (ti::keymap-add-minor-mode '(, mode-sym) nil nil 'remove)
         (if verb
             (message "minor mode removed")))
        (t
         (setq (,        map-sym)  (make-sparse-keymap)) ;; always refresh
         (setq (, prefix-map-sym)  (make-sparse-keymap)) ;; always refresh
         (run-hooks '(, hook-sym))
         ;;  Always do reinstall; because keymaps stored permanently and
         ;;  making a change later is impossible.
         (ti::keymap-add-minor-mode '(, mode-sym) nil nil 'remove)
         (ti::keymap-add-minor-mode '(, mode-sym)
                                    '(, mode-name-sym)
                                    (, map-sym))
         (if verb
             (message "minor mode installed"))))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-define-keys
  (minor--mode-name
   minor--mode-desc
   func-def-sym
   keymap-sym
   prefix-keymap-sym
   prefix-key-sym
   easymenu-sym
   easymenu-Name-sym
   easymenu-doc-str
   easy-menu-forms
   eval-body)
  "Return standard function form.
The returned function will install keymaps and menu-bar menu for minor mode.

Inside the function you can refer to variables

 'root-map'             refers to ROOT keymap from where the prefix map is accessed
                        This is the original keymap where the PREFIX-KEY is
                        assigned. The actual commands are put to 'map'.
 'map'                  refers to separate minor mode prefix keymap
 'p'                    holds the prefix key.

Input:

 MINOR--MODE-NAME       string
 MINOR--MODE-DESC       string
 FUNC-DEF-SYM           symbol, function name which is created
 KEYMAP-SYM             symbol, keymap where to define keys, must exist
 PREFIX-KEY-SYM         symbol, variable holding the prefix key.
 [EASYMENU-SYM]         symbol, easy menu variable or nil.
 [EASYMENU-NAME-SYM]    symbol, easy menu's menu-bar name variable or nil
 [EASYMENU-DOC-STR]     string, Describe string for menu.
 [EASY-MENU-FORMS]      forms to define menus
 EVAL-BODY              forms executed at the end of function.

Created function's arguments:

  ()

How to call this function:

   (ti::macrof-define-keys
     xxx-mode-define-keys
     xxx-:mode-prefix-map
     xxx-:mode-prefix-key
     xxx-:mode-easymenu
     xxx-:mode-easymenu-name
     (list
       xxx-:mode-easymenu-name
       [\"menu item1\"  xxx-function1 t]
       [\"menu item2\"  xxx-function2 t]
       \"----\"
       [\"menu item3\"  xxx-function3 t])
     (progn
        (define-key  map  \"a\"   'xxx-function1)
        (define-key  map  \"b\"   'xxx-function2)
        (define-key  map  \"c\"   'xxx-function3)))

Example how to call created function:

  (xxx-mode-define-keys)"
  (` (, (ti::macrof-define-keys-1
         minor--mode-name
         minor--mode-desc
         func-def-sym
         keymap-sym
         prefix-keymap-sym
         prefix-key-sym
         easymenu-sym
         easymenu-Name-sym
         easymenu-doc-str
         easy-menu-forms
         eval-body))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrov-mode-line-mode-menu (mode-symbol text)
  "Add MODE-SYMBOL to minor mode list in Emacs mode line menu."
  (let ((sym  (vector (intern (symbol-name (` (, mode-symbol)))))))
    (` (when (boundp 'mode-line-mode-menu) ;; Emacs 21.1
         (define-key mode-line-mode-menu (, sym)
           '(menu-item (, text)
                       (, mode-symbol)
                       :button (:toggle . (, mode-symbol))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-define-keys-1
  (minor--mode-name
   minor--mode-desc
   func-def-sym
   keymap-sym
   prefix-keymap-sym
   prefix-key-sym
   easymenu-sym
   easymenu-Name-sym
   easymenu-doc-str
   easy-menu-forms
   body)
  "Use macro `ti::macrof-define-keys' and see arguments there.
MODE-NAME FUNC-DEF-SYM KEYMAP-SYM PREFIX-KEYMAP-SYM PREFIX-KEY-SYM
EASYMENU-SYM EASYMENU-NAME-SYM EASYMENU-DOC-STR EASY-MENU-FORMS
BODY"
  (let* (sym)
    (setq sym (intern (symbol-name (` (, func-def-sym)))))
    (`
     (defun (, sym) ()
       (let* ((root-map  (, keymap-sym))
              (map       (, prefix-keymap-sym))
              (p         (, prefix-key-sym)))
         (when (stringp (, easymenu-doc-str)) ;This could be nil (no menus)
           (if (ti::xemacs-p)
               (easy-menu-define
                 (, easymenu-sym)
                 nil
                 (, easymenu-doc-str)
                 (, easy-menu-forms))
             (easy-menu-define
               (, easymenu-sym)
               (, keymap-sym)
               (, easymenu-doc-str)
               (, easy-menu-forms))))
         ;;  This is no-op, ByteComp silencer.
         ;;  ** variable p bound but not referenced
         (if (null p)        (setq p nil))
         (if (null map)      (setq map nil))
         (if (null root-map) (setq root-map nil))
         (ti::macrov-mode-line-mode-menu
          (, minor--mode-name) (, minor--mode-desc))
         ;; (define-key mode-map mode-prefix-key mode-prefix-map)
         (when (, prefix-key-sym)
           (define-key
             (, keymap-sym)
             (, prefix-key-sym)
             (, prefix-keymap-sym)))
         ;;  If you have selected a prefix key that is a natural ABC key;
         ;;  then define "aa" as self insert command for "a" character.
         ;;
         ;;  check also if prefix key defined is like  [{a)]] where "a"
         ;;  if a single character. The [{?\C-a)]] is nto accepted as
         ;;  repeated key: C-aC-a, only "aa"
         (let* ((char (ti::keymap-single-key-definition-p p)))
           (when (and (characterp char) (ti::print-p char))
             ;;  The prefix key is single; printable character.
             (define-key map p 'self-insert-command)))
         (, body))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-version-bug-report-1
  (filename
   prefix
   version-variable
   version-value
   bug-var-list

   &optional
   buffer-list
   bug-body)
  "Use macro `ti::macrof-version-bug-report' and see arguments there.
FILENAME PREFIX VERSION-VARIABLE VERSION-VALUE
BUG-VAR-LIST BUFFER-LIST BUG-BODY."
  (let* (sym
         ret
         elt)
    (push 'progn ret)
    (setq elt
          (list
           'defconst (` (, version-variable))
           (` (, version-value))
           "Package's version information."))
    (push elt ret)
    (setq sym (intern (format "%s-version" prefix)))
    (setq
     elt
     (`
      (defun (, sym) (&optional arg)
        "Version information."
        (interactive "P")
        (ti::package-version-info (, filename) arg))))
    (push elt ret)
    (setq sym (intern (format "%s-submit-bug-report" prefix)))
    (setq
     elt
     (`
      (defun (, sym) ()
        "Send bug report or feedback."
        (interactive)
        (ti::package-submit-bug-report
         (, filename)
         (, version-variable)
         (, bug-var-list)
         'verbose
         (, buffer-list))
        (, bug-body))))
    (push elt ret)
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-version-bug-report
  (filename
   prefix
   version-variable
   version-value
   bug-var-list
   &optional
   buffer-list
   bug-body)
  "Return standard function form.
One variable and two functions are created.

Input:

  FILENAME          string e.g. xxx.el
  PREFIX            package prefix for functions e.g. xxx
  VERSION-VARIABLE  symbol variable holding the version information.
  VERSION-VALUE     value for the variable. Should be RCS Id string or the
                    like.
  BUG-VAR-LIST      variable list to send with bug report
  BUG-BODY          Lisp forms for the bug function.

How to call this macro:

    (ti::macrof-version-bug-report
     \"xxx.el\"
     \"xxx\"
     xxx-:version-id
     \"...version Id string here, RCS controlled.\"

     '(xxx-:load-hook
       xxx-:mode-hook
       xxx-mode-define-keys-hook
       xxx-:mode-name))

Example how to call created functions:

  M - x xxx-submit-bug-report
  M - x xxx-version"
  (`(, (ti::macrof-version-bug-report-1
        filename
        prefix
        version-variable
        version-value
        bug-var-list
        buffer-list
        bug-body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-debug-1
  (prefix
   debug-function
   debug-toggle-function
   debug-buffer-show-function
   debug-variable
   debug-buffer)
  "Use macro `ti::macrof-debug' and see argument there.
PREFIX
DEBUG-FUNCTION DEBUG-TOGGLE-FUNCTION DEBUG-BUFFER-SHOW-FUNCTION
DEBUG-VARIABLE DEBUG-BUFFER."
  (let* (str
         ret
         elt)
    (push 'progn ret)

    (setq elt
          (list
           'defvar (` (, debug-variable))
           nil
           "Debug control: on or off."))
    (push elt ret)

    (setq elt
          (list
           'defvar (` (, debug-buffer))
           (format "*%s-debug*" prefix)
           "Debug output buffer."))
    (push elt ret)
    (setq str
          (concat
           "Generate debug\n"
           "Prefix ARG: nil = toggle, 0 = off, 1 = on."))
    (setq
     elt
     (`
      (defun (, debug-toggle-function) (&optional arg)
        (, str)
        (interactive "P")
        (let* ((buffer (get-buffer (, debug-buffer))))
          (ti::bool-toggle (, debug-variable) arg)
          (when (and (, debug-variable)
                     buffer
                     (y-or-n-p "Clear debug buffer?"))
            (ti::erase-buffer buffer))
          (if (interactive-p)
              (message "Debug is %s"
                       (if (, debug-variable)
                           "on"
                         "off")))))))
    (push elt ret)
    (when debug-buffer-show-function
      (setq str "Show debug buffer.")
      (setq
       elt
       (`
        (defun (, debug-buffer-show-function) (&optional arg)
          (, str)
          (interactive "P")
          (let* ((buffer (get-buffer (, debug-buffer))))
            (ti::bool-toggle (, debug-variable) arg)
            (if (null buffer)
                (message "There is no debug buffer to show.")
              (display-buffer buffer))))))
      (push elt ret))
    (setq str
          (concat "Write debug log to " ;; (` (, debug-buffer ))
                  " if "
;;;                (symbol-name (quote (` (, debug-variable)) ))
                  "is non-nil."))

    ;; We are returning a macro in next elt.
    (setq
     elt
     (`
      (defmacro (, debug-function) (&rest args)
;;;      (when (, debug-variable)
;;;        (let* ((ti:m-debug-buffer (, debug-buffer )))
        (when (, debug-variable)
          (with-current-buffer (get-buffer-create (, debug-buffer))
            (goto-char (point-max))
            (while args
              (insert (format "|%s" (eval (pop args)))))
            (insert "\n"))))))
    (push elt ret)
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-debug-lowlevel
  (prefix
   debug-function
   debug-toggle-function
   debug-buffer-show-function
   debug-variable
   debug-buffer)
  "Return standard function forms for debug interface.
One variable, one function and one macro will be created.

Input:

  PREFIX                     string, symbols' prefix.
  DEBUG-FUNCTION             symbol, function name to generate debug
  DEBUG-TOGGLE-FUNCTION      symbol, function name to turn on/off debug
  DEBUG-BUFFER-SHOW-FUNCTION symbol, fucntion to display debug buffer.
  DEBUG-VARIABLE             symbol, variable to control debug
  DEBUG-BUFFER               string, buffer name where to write debug.

How to call this macro:

  (ti::macrof-debug xxx-debug xxx-debug-toggle xxx-debug-show
   xxx-debug \"*xxx-debug*\")

Example how to call created functions:

  M - x xxx-debug-show

  M - x xxx-debug-toggle  ;; To turn on or off debug package debug
  (xxx-debug-toggle 0)    ;; off
  (xxx-debug-toggle 1)    ;; on

  ;;  To generate debug from inside code, you call:
  (xxx-debug ... anything frame-pointer buffer-pointer ...)"
  (`(, (ti::macrof-debug-1
        prefix
        debug-function
        debug-toggle-function
        debug-buffer-show-function
        debug-variable
        debug-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-debug-standard (prefix &optional var-prefix)
  "Make standard debug interface according to PREFIX and VAR-PREFIX."
  (let* ((d-func   (intern (format "%s-debug" prefix)))
         (dt-func  (intern (format "%s-debug-toggle" prefix)))
         (ds-func  (intern (format "%s-debug-show" prefix)))
         (pfx      (or var-prefix "-"))
         (d-var    (intern (format "%s%sdebug" prefix pfx)))
         (d-buffer (intern (format "%s%sdebug-buffer" prefix pfx))))
    (`(, (ti::macrof-debug-1
          prefix
          d-func
          dt-func
          ds-func
          d-var
          d-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-install-pgp-tar-1
  (func-ins-sym elisp-file &optional log-buffer)
  "Use macro `ti::macrof-install-pgp-tar' and see arguments there.
FUNC-INS-SYM ELISP-FILE LOG-BUFFER."
  (let* (sym)

    (setq sym (intern (symbol-name (` (, func-ins-sym)))))

    (`
     (defun (, sym) (dir)
       "Install additional programs from the end of package."
       (interactive "DSave programs to directory: ")
       (let* ((file    (, elisp-file))
              (source  (or (locate-library file)
                           (error "can't find %s along load-path." file))))
         (ti::package-install-pgp-tar
          dir
          (or (, log-buffer)
              "*install-log*")
          source))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-install-pgp-tar
  (func-ins-sym elisp-file &optional log-buffer)
  "Return standard pgp tar install function.
It handles installing pgp base 64 signed tar block from the end of file.

  1.   Create tar file (it sould not have directory names, but ...)
  2.   pgp base64 sign the tar file (clearsig off)
  3.   paste pgp data to to end of your lisp package

  ;; -----BEGIN PGP MESSAGE-----
  ;; Version: 2.6.3ia
  ;;
  ;; owHsWc1vG0l2n0GwwYjA3pJLgEXKlNaSDJLilySblrWWLXrMrCQrpOydzcxA02wW
  ...
  ;; -----END PGP MESSAGE-----

And nothing more is needed to get your programs untarred nicely.
The drop directory is asked from the user when he calls this function.

Input:

  FUNC-INS-SYM  symbol, the created install function name
  ELISP-FILE    your Lisp package name (with .el)
  [LOG-BUFFER]  where to print the install log. Can be nil.

Created function's arguments:

  (dir)
  DIR           Where to untar the included file, asked interactively

How to call this function:

  ;;;###autoload (autoload 'xxx-install-programs  \"package-file\" t t)

   (ti::macrof-install-pgp-tar
    xxx-install-programs
   \"xxx.el\"
   \"*xxx-install-log*\"))

Example how to call created function:

  M - x xxx-install-programs"
  (` (, (ti::macrof-install-pgp-tar-1
         func-ins-sym
         elisp-file
         log-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::macrof-minor-mode-wizard
  (pfx                                  ;1
   mode-Name                            ;
   mode-Name-prefix-key                 ;
   easymenu-Name                        ;
   custom-group                         ;
   variable-style                       ;6

   doc-str                              ;7
   mode-desc                            ;
   minor-mode-body                      ;

   easymenu-doc                         ;10
   easymenu-body                        ;
   define-key-body)                     ;12
  "Do all the necessary things to create minor mode.
Following macros are called one by one. If you want personalized
minor mode control, call each of these individually and don't use
this wizard.

    `ti::macrov-minor-mode'
    `ti::macrof-minor-mode-install'
    `ti::macrof-minor-mode'
    `ti::macrof-minor-mode-on'
    `ti::macrof-minor-mode-off'
    `ti::macrof-minor-mode-help'
    `ti::macrof-define-keys'

Input:

    PFX                     See -vmacro-
    MODE-NAME               See -vmacro-
    MODE-NAME-PREFIX-KEY    See -vmacro-
    EASYMENU-NAME           See -vmacro-
    CUSTOM-GROUP            See -vmacro-
    VARIABLE-STYLE          See -vmacro-

    DOC-STR                 See -fmacro-minor-mode
    MODE-DESC               See -fmacro-minor-mode
    MINOR-MODE-BODY         See -fmacro-minor-mode must be in format ((BOBY))

    EASYMENU-DOC            See -fmacro-define-keys must be in format ((BOBY))
    EASYMENU-BODY           See -fmacro-define-keys must be in format ((BOBY))
    DEFINE-KEY-BODY         See -fmacro-define-keys

How to call this function:

   See example tinylisp.el package which uses this function to create
   minor mode.

If you want to see what this macro produces, use

  (macroexpand '(ti::macrof-minor-mode-wizard ...))C - x C - e

Here is example how you would define the minor mode.

  (eval-and-compile   ;; So that defvars and defuns are seen
    (ti::macrof-minor-mode-wizard
     \"xxx-\"               ;; prefix for variables and functions
     \" xxxModeline\"       ;; Modeline name
     \"\\C-cx\"              ;; prefix key for mode.
     \"xxxMenubar\"         ;; Menu bar name
     nil                  ;; <forget this>

     \"XXX minor mode. Does fancy things.\"  ;; mode description

     \"XXX help\"    ;; message displayed when user calls mode

     ;; ............................................................
     (progn
      ;; Run body-of-code when minor mode is called
      nil)

     ;; ............................................................
     ;; Next id used by easy-menu.el and defines menu items.
     (list
      xxx-mode-easymenu-name
      [\"Eval whole buffer\" xxx-eval-current-buffer    t]
      ..)

     ;; ............................................................
     ;;  this block defines keys to the mode. The mode minor map is
     ;;  locally bound to 'map' symbol.
     (progn
       (define-key map \"-\" 'xxx-eval-current-buffer)
       (define-key map \"=\" 'xxx-calculate))))
"
  (` (,
      (ti::macrof-minor-mode-wizard-1
       pfx                              ;1
       mode-Name                        ;
       mode-Name-prefix-key             ;
       easymenu-Name                    ;
       custom-group                     ;
       variable-style                   ;6

       doc-str                          ;7
       mode-desc                        ;
       minor-mode-body                  ;9

       easymenu-doc                     ;10
       easymenu-body                    ;
       define-key-body))))              ;12

;;; ----------------------------------------------------------------------
;;;
(defun ti::macrof-minor-mode-wizard-1
  (pfx                                  ;1
   mode-Name                            ;2
   mode-Name-prefix-key                 ;3
   easymenu-Name                        ;4
   custom-group                         ;5
   variable-style                       ;6

   doc-str                              ;7
   mode-desc                            ;8
   minor-mode-body                      ;9

   easymenu-doc                         ;10
   easymenu-body                        ;11
   define-key-body)                     ;12
  "Use macro `ti::macrof-minor-mode-wizard' and see parameters there.
   PFX
   MODE-NAME
   MODE-NAME-PREFIX-KEY
   EASYMENU-NAME
   CUSTOM-GROUP
   VARIABLE-STYLE
   DOC-STR
   MODE-DESC
   MINOR-MODE-BODY

   EASYMENU-DOC
   EASYMENU-BODY
   DEFINE-KEY-BODY"

  (let* (sym1
         sym2
         sym3
         sym4
         sym5
         sym6
         sym7
         ret
         elt
         vs)
    (ti::nconc ret 'eval-and-compile)
    ;; ........................................... create variables ...
    (setq elt
          (ti::macrov-minor-mode-1
           pfx
           mode-Name
           mode-Name-prefix-key
           easymenu-Name
           custom-group
           variable-style))
    (setq vs  (if variable-style
                  variable-style
                pfx))
;;;    (ti::d!! "\n\n>>" elt)
    (ti::nconc ret elt)
    ;; .................................... create install function ...
    (setq sym1 (intern (concat pfx "install-mode"))
          sym2 (intern (concat pfx "mode"))
          sym3 (intern (concat vs "mode-map"))
          sym4 (intern (concat vs "mode-prefix-map"))
          sym5 (intern (concat vs "mode-name"))
          sym6 (intern (concat vs "mode-define-keys-hook")))
;;;    (ti::d!! "\n\n>>minor-mode-install" sym1 sym2 sym3 sym4 sym5  "\n")
    (setq elt (ti::macrof-minor-mode-install-1
               sym1 sym2 sym3 sym4 sym5 sym6))
    (ti::nconc ret elt)
    ;; ....................................... define keys function ...
    (setq sym1 (intern (concat pfx "mode-define-keys"))
          sym2 (intern (concat vs  "mode-map"))
          sym3 (intern (concat vs  "mode-prefix-map"))
          sym4 (intern (concat vs  "mode-prefix-key"))
          sym5 (intern (concat vs  "mode-easymenu"))
          sym6 (intern (concat vs  "mode-easymenu-name"))
          sym7 (intern (concat pfx "mode")))
;;;   (ti::d!! "\n\n>>define-keys"  sym1 sym2 sym3 sym4 sym5)
    (setq elt
          (ti::macrof-define-keys-1
           sym7
           mode-desc
           sym1
           sym2
           sym3
           sym4
           sym5
           sym6
           easymenu-doc
           easymenu-body
           define-key-body))
    (ti::nconc ret elt)
    ;; ................................. create minor mode function ...
    (setq sym1 (intern (concat pfx "mode"))
          sym2 (intern (concat pfx "install-mode"))
          sym3 (intern (concat pfx "mode"))
          sym4 (intern (concat vs  "mode-name"))
          sym5 (intern (concat vs  "mode-prefix-key"))
          sym6 (intern (concat vs  "mode-easymenu"))
          sym7 (intern (concat vs  "mode-hook")))
;;;   (ti::d!! "\n\n>>minor-mode" sym1 sym2 sym3 sym4 sym5 sym6 sym7 "\n")
    (setq elt
          (ti::macrof-minor-mode-1
           sym1 doc-str  sym2
           sym3 sym4     sym5
           sym6 nil      mode-desc
           sym7
           minor-mode-body))
    (ti::nconc ret elt)
    (setq elt (ti::macrof-minor-mode-on   sym1))
    (ti::nconc ret elt)
    (setq elt (ti::macrof-minor-mode-off  sym1))
    (ti::nconc ret elt)
    (setq elt (ti::macrof-minor-mode-help sym1))
    (ti::nconc ret elt)
    (setq elt (ti::macrof-minor-mode-commentary pfx sym1))
    (ti::nconc ret elt)
    (setq elt (ti::macrof-minor-mode-viper-attach pfx sym1))
    (ti::nconc ret elt)
    ret))

;;}}}

(provide   'tinylib)

;;; tinylib.el ends here
