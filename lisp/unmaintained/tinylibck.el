;;; tinylibck.el --- Library to (c)onvert (k)eybindings for XEmacs or Emacs

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1996-2010 Jari Aalto
;; Keywords:     tools
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; To get information on this program, call M-x ti::ck-version.
;; Look at the code with folding.el

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
;;
;; Note: 2010-11-20  This library is obsolete and no longer maintained.
;; You're free to take over the maintenance. Use at your own risk.
;;
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file. This must be the very first entry before
;;  any keybindings take in effect.
;;
;;      (require 'tinylibck)
;;
;;  You can also use the preferred way: autoload
;;
;;      (autoload 'ti::ck-advice-control "tinylibck")
;;
;;  And when you need conversion you wrap the code with calls:
;;
;;      (ti::ck-advice-control)           ;; ON
;;      <key definitions>
;;      (ti::ck-advice-control 'disable)  ;; OFF
;;
;;  Remember that you DON'T LEAVE THIS PACKAGE ON. Make sure the 'disable
;;  is the last thing you do. It disables the package and makes sure your
;;  other emacs packages work properly

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface 1996
;;
;;      This file tries to overcome differencies between Emacs and XEmacs
;;      keybinding. Package was developed at the time when there was big
;;      differences between Emacs and XEmacs key bindings. This file is in
;;      fact "library" and propably interests only lisp programmers that
;;      want to make some old package, that has Emacs specific bindings, to
;;      work in XEmacs (or vice versa).
;;
;;  Emacs 19.30+ note
;;
;;      Newer Emacs release now supports XEmacs styled bindings.
;;      You can write
;;
;;          (local-set-key [(control meta up)] 'ignore)
;;
;;      and it should work both in XEmacs and Emacs. If all your keybindins
;;      are like that and you don't use Emacs lower than 19.30, then you
;;      don't need this package.
;;
;;  Putting your key definitions to separate file
;;
;;      You should separate all you keybindings to one file,
;;      do not stuff all your emacs definitions in one huge ~/.emacs,
;;      but instead use some basic structure like this:
;;
;;          ~/.emacs            -- the main; points to ~/rc/emacs-rc.el
;;          ~/rc/emacs-kbd.el   -- All the keybindinds
;;          ~/rc/emacs-vc.el    -- vc settings and modifications
;;          ~/rc/emacs-gnus.el  -- gnus customization
;;          ..
;;
;;      You can load your other initialize files from .emacs with `load'
;;      command. Suppose you have Emacs keybinding startup file; which you
;;      want to make compatible with XEmacs too. The reason why you should
;;      use `ti::ck-maybe-activate' is that, it can determine your emacs
;;      version and decide when the converter is needed and when not.
;;
;;          ;; at the beginning of keybindings, you add these
;;
;;          (require 'tinylibm)
;;          (autoload 'ti::ck-advice-control "tinylibck")
;;
;;          (ti::ck-maybe-activate 'xemacs-mouse)
;;          (load "~/rc/emacs-rc-keys")     ;; All XEmacs styled bindings
;;          (ti::ck-maybe-activate 'xemacs-mouse 'disable)
;;
;;          ;; End of example
;;
;;  Some lowlevel explanation
;;
;;      If you're in Emacs, you use X-event bindings like this
;;
;;          (global-set-key [C-up] 'ignore)
;;
;;      Unfortunately, this does not work in XEmacs, but using the
;;      conversion function before the definition, it does.
;;
;;          (global-set-key (ti::ck-do [C-up]) 'ignore)
;;
;;      Now the current Emacs version gets the right keybinding,
;;
;;          for Emacs  it returns       --> [C-up]
;;          for XEmacs it returns       --> '(control up)
;;
;;      You can also use the XEmacs keybinding, since the conversion goes
;;      both ways. Having the following setting:
;;
;;          (global-set-key (ti::ck-do '(control up)) 'ignore)
;;
;;      it converts this to suitable form depending on the current Emacs
;;      in use.
;;
;;  About advices
;;
;;      So that you don't have to go and add that 'ti::ck-do' call for
;;      every keybinding, the key binding functions have been adviced.
;;      The conversion is done transparently and no chnages are
;;      needed in files were keys are bound.
;;
;;  About debugging
;;
;;      If you suspect any weird behavior in your emacs while
;;      this package is loaded, you should check that the `ti::ck--debug'
;;      is turned on. (`M-x' `ti::ck-debug-toggle')
;;
;;      The buffer `ti::ck--debug-buffer' constantly records any conversion
;;      actions and you can find the problems quickly. Please send the
;;      supicious/false conversion lines to the maintainer of this package
;;      and if possible, tell how the conversion should go in your opinion.
;;
;;      I'd recommend that you keep the debugging permanently on, because
;;      if problems arise afterwards and if the debug were off, there is
;;      no way to tell what went wrong in what command.
;;
;;      Important; when you have problems, increase
;;
;;          ti::ck--debug-buffer-size
;;
;;      immediately to some arbitrary big value so that you get all the
;;      conversions recorded.
;;
;;  Known limitations
;;
;;      This package tries to do its best to make the conversion, but
;;      sometimes it is just impossible. For example the following
;;      case is beyond of this package. In Emacs you can define
;;
;;          (define-key xxx-mode-map [?\C-`] 'some-function)
;;                                   ^^^^^^^
;;
;;      But when your're in XEmacs and you try to do the same, it gives
;;      error although tinylibck is currently active. The reason is that lisp
;;      intepreter never actually passes key  "?\C-`" to `define-key'
;;      but it actually evaluates the vector in place to an integer value
;;      and sends that to `define-key' function . The call actually is
;;      seen in Emacs like this:
;;
;;          (define-key xxx-mode-map [4194400] 'some-function)
;;                                   ^^^^^^^^^in HP-UX 9.05
;;
;;      And in XEmacs it is evaluates to this:
;;
;;          (define-key xxx-mode-map [0] 'xxx-tab-backward)
;;
;;      The code "0" appears, because XEmacs doesn't know Emacs "?\C-`".
;;      You should write [(control ?\`)] for XEmacs and it would work ok.
;;      Be aware of this limitation if you plan to use Emacs styled
;;      bindings. Alternatively, you can tell that you that some
;;      particular piece of code has been written by using XEmacs style.
;;      (Wouldn't you want to you use it all the time in Emacs...)
;;
;;          ;;   This is Emacs file.
;;          (require 'tinylibck)                ;Convert keys
;;          (ti::ck-advice-control)             ;turn it on
;;          ;;
;;          (define-key tinytab-mode-map [(control ?\`)]  'tinytab-tab-backward)
;;          ;; And other similar keybindings ...
;;          ;; ..
;;          (ti::ck-advice-control 'disable)    ;don't leave it on
;;
;;  Thank you
;;
;;      Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;      Presented initial idea of the conversion process.
;;      Commented how the conversion should go in XEmacs.
;;
;;      Stephen Eglen  <stephene@cogs.susx.ac.uk>
;;      Stephen had the patience to send bug reports from XEmacs 19.12 and
;;      test new versions of tinylibck.el

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(eval-when-compile
  (require 'cl)
  (require 'advice)
  (set (make-local-variable 'byte-compile-dynamic-docstrings) t)
  (set (make-local-variable 'byte-compile-dynamic) t))

(eval-and-compile
  ;;  Don't require lib package unnecessarily
  (autoload 'ti::package-version-info    "tinylib")
  (autoload 'ti::package-submit-feedback "tinylib"))

;;}}}
;;{{{ setup: -- private variables

(defconst tinylibck-version-time "2010.1120.1620"
  "Latest version number.")

(defvar ti::ck--load-hook '(ti::ck-advice-control)
  "*Hook run when file has been loaded.")

(defconst ti::ck--xemacs-flag (string-match "XEmacs" (emacs-version))
  "Non-nil means XEmacs is detected.")

(defconst ti::ck--emacs-minor
  (if (boundp 'emacs-minor-version)
      emacs-minor-version 0)
  "Emacs minor version or 0 if cannot detect one.")

(defconst ti::ck--advice-re "^ti::ck-keybind"
  "Advice REGEXP.")

(defvar ti::ck--this-command nil
  "Private. Current advice command.")

;;  To prevent buffer growing too much
;;
(defvar ti::ck--debug-buffer-size 500
  "Clear the `ti::ck--debug-buffer' if line count exceed this value.")

(defvar ti::ck--debug-buffer "*ti::ck-debug*"
  "Debug buffer for key binding commands.")

;;}}}
;;{{{ setup: -- user

;;; ........................................................ &v-public ...
;;; User configurable, but in general you don't need to touch this
;;; section.

(defvar ti::ck--debug nil
  "*Turn on/off key conversion debugging.")

(defvar ti::ck--keep-next-symbol-together
  '("kp")
  "*Keep SYMBOL and next key bind definition together.
When this string is found from key binding definition, it is
not a stand alone event name, but only part of it. After reading the next
token, the X-event has been qualified.

Eg. `kp' is a prefix for keypad X-event symbols, so we actually mean
one key when we say 'kp-tab' and not two separate events like `kp' and `tab'.

Format:
 '(STRING-SYMBOL
   STRING-SYMBOL
   ..)")

(defconst ti::ck--key-table
  '((A          . alt)
    (C          . control)
    (H          . hyper)
    (S          . shift)
    (s          . super)
    (M          . meta)
    (mouse-1    . button1)
    (mouse-2    . button2)
    (mouse-3    . button3)
    (down-mouse-1    . button1up)
    (down-mouse-2    . button2up)
    (down-mouse-3    . button3up))
  "*Key bind modifier mappings from Emacs to XEmacs.
This is a primitive table from where the complex keybindings are
derived, eg you don't put following entry to this table:

   (C-M-mouse-1       . (control meta button1))

Because it can be already contructed from the primitives.
If you have a need to change this table, please contact maintainer.

Format:
'((EMACS-MODIFIER . XEMACS-MODIFIER)
  (EMACS-MODIFIER . XEMACS-MODIFIER)
  ..)")

;;}}}
;;{{{ misc, debug

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::ck-do-p (arg)
  "Check if conversion is needed. ARG is the key definition."
  `(not (stringp ,arg)))          ;pass "" string bindings as is

;;; ----------------------------------------------------------------------
;;; - Just for load hook
;;;
(defun turn-on-ti::ck-debug ()
  "Turn on debug."
  (interactive)
  (ti::ck-debug-toggle 1))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::ck-debug-toggle (&optional arg)
  "Turn debug on or off with ARG. See buffer `ti::ck--debug-buffer'."
  (interactive)
  (cond
   ((eq 1 arg)
    (setq ti::ck--debug t))
   ((memq arg '(0 -1))
    (setq ti::ck--debug nil))
   (t
    (setq ti::ck--debug (not ti::ck--debug))))
  (if (interactive-p)
      (message (concat "Debug " (if ti::ck--debug "on" "off")))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::ck-debug-write (str)
  "Record STR to debug buffer."
  (let* ((buffer (get-buffer-create ti::ck--debug-buffer)))
    (with-current-buffer buffer
      (if (> (count-lines (point-min) (point-max))
             ti::ck--debug-buffer-size)
          (erase-buffer))
      (goto-char (point-max))
      (insert str))))

;;}}}
;;{{{ advice

;;; ----------------------------------------------------------------------
;;;
(defun ti::ck-advice-control (&optional disable verb)
  "Install advices or optionally DISABLE them. VERB."
  (interactive "P")
  (let* ((funcs '(global-set-key
                  local-set-key
                  define-key))
         (re   ti::ck--advice-re)
         (verb (or verb (interactive-p)))
         func)
    (while funcs
      (setq func (car funcs))
      (ignore-errors
        (if disable
            (ad-disable-advice  func 'any re) ;;clear flag
          (ad-enable-advice     func 'any re))
        (ad-activate func)) ;;change state
      (setq funcs (cdr funcs)))

    (if verb
        (if disable
            (message "tinylibck Advices disabled.")
          (message "tinylibck Advices activated.")))))

;;; ----------------------------------------------------------------------
;;;
(defadvice global-set-key (before ti::ck-keybind-converter  dis)
  "XEmacs and Emacs emulation. See function `ti::ck-do' for full story."
  (setq ti::ck--this-command 'global-set-key)
  (if (ti::ck-do-p (ad-get-arg 0))
      (ad-set-arg 0 (ti::ck-do (ad-get-arg 0)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice local-set-key (before ti::ck-keybind-converter  dis)
  "XEmacs and Emacs emulation. See function `ti::ck-do' for full story."
  (setq ti::ck--this-command 'local-set-key)
  (if (ti::ck-do-p (ad-get-arg 0))
      (ad-set-arg 0 (ti::ck-do (ad-get-arg 0)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice define-key (before ti::ck-keybind-converter  dis)
  "XEmacs and Emacs emulation. See function`ti::ck-do' for full story."
  (setq ti::ck--this-command 'define-key)
  (let* ((arg (ad-get-arg 1)))
    (when (ti::ck-do-p arg)
      (if (and (vectorp arg)
               (length arg)             ; "[0]"
               (eq 0 (elt arg 0)))
          (error                         ; otw user doesn't know what
           (concat                       ; going on.. barf immediately
            "define-key/tinylibck.el: "
            "Vector contains zero. Did you use Emacs styled \"[?\\C-`]\" "
            "Wich can't be converted? Use equivalent [(control ?\\`)] "
            "instead which works for both XEmacs and Emacs. "
            "See comments in tinylibck.el for more.")))
      (ad-set-arg 1 (ti::ck-do (ad-get-arg 1))))))

;;; ----------------------------------------------------------------------
;;; (ad-unadvise 'ti::ck-do)
;;;
(defadvice ti::ck-do (around ti::ck-debug act)
  "Debug filter. Record command, input/output values."
  (cond
   ((eq nil ti::ck--debug)
    ad-do-it)
   (t
    (ti::ck-debug-write
     (format
      "\n%-15s %-25s >> "
      (or (prin1-to-string ti::ck--this-command) "")
      (or (prin1-to-string (ad-get-args 0))     "")))

    ad-do-it
    (ti::ck-debug-write (concat (prin1-to-string ad-return-value))))))

;;}}}
;;{{{ conversions

;;; ----------------------------------------------------------------------
;;;
(defun ti::ck-get-key-code-string (str)
  "Convert STR A -C -k --> ?\\A -\\C -k."
  (let* ((ret   "?\\")
         (len   (length str))
         (i     0)
         case-fold-search
         ch
         next)
    (while (< i len)
      (setq ch   (aref str i)
            next (if (< (1+ i)
                        len)
                     (aref str (1+ i))))
      (setq ret
            (concat
             ret
             (if (and (eq ch ?-)
                      ;;  A-S-a  --> \A-\S-a, but
                      ;;  A-s    --> \A-s
                      (and next
                           (string-match "[A-Z]"
                                         (char-to-string next))))
                 "-\\"
               (char-to-string ch))))
      (setq i (1+ i)))
    ret))

;;; ----------------------------------------------------------------------
;;; - This is kinda faking Emacs, but since there is _no_ other way to
;;;   find the integer code for vector [?\A-a], we have to make Emacs
;;;   tell it to us.
;;;
(defun ti::ck-get-key-code (simple-key-sequence)
  "Find out the integer value for SIMPLE-KEY-SEQUENCE, like S-a."
  (let* (lisp-mode-hook                ;don't run any hooks while here
         (buffer (get-buffer-create "*tmp*"))
         (modes  '(lisp-mode
                   emacs-lisp-mode
                   lisp-interaction-mode))
         ret)
    ;; XEmacs doesn't have this variable, Quiet ByteCompiler warning.
    ;; This is no-op
    (if lisp-mode-hook
        (setq lisp-mode-hook nil))
    (setq simple-key-sequence
          (ti::ck-get-key-code-string simple-key-sequence))
    (with-current-buffer buffer
      (erase-buffer)
      ;;  Don't just always execute (lisp-mode), since
      ;;  setting up major mode may be time consuming.
      (if (not (memq major-mode modes))
          (lisp-mode))
      (insert "[" simple-key-sequence "]")
      ;;  This spits out the integer number
      (eval-last-sexp 1)
      (beginning-of-line)
      (when (looking-at ".*\\[\\([0-9]+\\)")
        (setq ret
              (string-to-number
               (buffer-substring (match-beginning 1) (match-end 1)))))

      ret)))

;;; ----------------------------------------------------------------------
;;;
;;; (ti::ck-gnu2xe-vector [C-kp-tab])
;;;
;;; [C-kp-tab]   --> [(control kp-tab)]
;;; [M-f1 C-f2]  --> [(meta f1) (control f2)]
;;; [?\e delete] --> [(meta delete)]  , we suppose meta is same as ESC.
;;;
(defun ti::ck-gnu2xe-vector (vec)
  "Convert Emacs VEC bindings to XEmacs style."
  (let* ((table     ti::ck--key-table)
         (keep-list ti::ck--keep-next-symbol-together)
         (i     0)
         len
         x
         elt
         str
         pos
         aset-pos
         list
         new-vec
         gather-flag
         gather-str)
    (setq len (length vec)  elt (elt vec 0))
    (setq i        0
          aset-pos 0)
    (setq new-vec (make-vector len nil)) ;put results here
    ;;  This is for due to ESC key in commands like:
    ;;  Emacs [?\e ?k] --> XEmacs [(meta k)]
    (cond
     ((and (eq 27 elt)                  ;first element is ?\e
           (symbolp (elt vec 1)))
      ;;  Put elements 0 and 1 together "?\e delete" --> "M-delete"
      (setq str (concat "M-" (symbol-name (elt vec 1))))
      (aset vec 1 (intern str))
      (setq i 1)                        ;start here, skip item 0
      (setq new-vec (make-vector (1- len) nil)))
     ((and (eq 27 elt)                  ;first element is ?\e
           (eq 2 len))
      (setq str (concat "M-" (char-to-string (elt vec 1))))
      (aset vec 1 (intern str))
      (setq i 1)                        ;start here, skip item 0
      (setq new-vec (make-vector (1- len) nil))))
    (while (< i len)
      (setq x (elt vec i))
      (cond
       ;;  [?\C-x mouse-1] ==> [(control x) (button1)]
       ((and (integerp x)
             (< x 27))
        (setq x
              (list
               'control
               ;; ?\C-a -- "a"
               (intern (char-to-string (+ 96 x))))))
       ((integerp x)                    ; other [?\C-z ...]
        nil)
       ((symbolp x)
        (setq str (symbol-name x)) ;; [C-up] => "C-up"
        (setq list nil)
        (while str
          (if (or (string-match "^\\(down-\\)?mouse-[1-3]" str)
                  (string-match "^[^-]+" str))
              (progn
                (setq pos (match-end 0))
                (setq elt (substring str 0 pos))

                (if (< pos (length str))
                    (setq str (substring str (1+ pos)))
                  (setq str nil)))
            ;;  No more "-" characters in string
            (setq elt str   str nil))
          ;;  There are certain X symbols that should be kept together
          ;;  [C-kp-tab]  --> (control kp_tab) and not (control kp tab)
          (cond
           ((member elt keep-list)
            (setq gather-str elt  gather-flag 0   elt nil))

           ((stringp gather-str)
            (setq gather-flag (1+ gather-flag))
            (if (eq 1 gather-flag)
                (setq elt (concat gather-str "_" elt)
                      gather-str  nil
                      gather-flag nil)
              (setq elt          gather-str
                    gather-flag  nil
                    gather-str   nil))))
          (cond
           (elt
            (setq elt (intern elt))
            (if (setq x (assq elt table))
                (setq elt (cdr x)))
            (setq list (append list (list elt))))))
        (setq x list)))

      (aset new-vec aset-pos x)
      (setq i         (1+ i)
            aset-pos  (1+ aset-pos)))
    new-vec))

;;; ----------------------------------------------------------------------
;;; [(meta f1) (control f2)] --> [M-f1 C-f2]
;;;
(defun ti::ck-xe2gnu-vector (vec)
  "Convert XEmacs VEC to Emacs."
  (let* ((i     0)
         len
         sym
         x
         new-vec)
    (setq len (length vec))
    (setq new-vec (make-vector len nil))
    (while (< i len)
      (setq x (elt vec i))
      (cond
       ((integerp x)                    ;[?\C-z ...]
        nil)                            ;as is
       ((and (symbolp x)
             (setq sym (symbol-name x))
             (eq 1 (length sym)))       ;one character
        ;; In XEmacs, it's valid to have [f1 a], where 'a' means character
        ;; a. In Emacs you'd need ?a for that.
        ;; => as char
        (setq x (string-to-char sym)))
       ((listp x)
        (setq x (ti::ck-xe2gnu-list x))))
      (aset new-vec i x)
      (setq i (1+ i)))
    new-vec))

;;; ----------------------------------------------------------------------
;;; (meta f1) --> M-f1 symbol, or '(alt a) --> 120345 some keycode integer.
;;;
(defun ti::ck-xe2gnu-list (list)
  "Convert XEmacs bind LIST to emacs."
  (let* ((table ti::ck--key-table)
         item
         elt
         str
         padd
         ret)
    (setq str "")
    (while list
      (setq elt (car list))
      (setq padd (if (cdr list)
                     "-"
                   ""))
      (cond
       ((setq item (rassq elt table))
        (setq elt (symbol-name (car item))))
       ((integerp elt)
        (setq elt (char-to-string elt)))
       ((and (stringp str)
             (symbolp elt))             ;keep it as string, see concat
        (setq elt (symbol-name elt))))

      (setq str (concat str elt padd))
      (setq list (cdr list)))
    (cond
     ((string= "" str)
      nil)
     ((and (not (string-match "mouse" str))
           ;; "A-a"  "A-C-k" "?\C-`"
           (string-match "-.$\\|^[?][\\]?" str))
      (setq ret (ti::ck-get-key-code str)))
     (t
      (setq ret (intern str))))
    ret))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;; - 20 Apr 1996, Idea by Vladimir Alexiev <vladimir@cs.ualberta.ca>
;;; - 22 Apr 1996, Reprogrammed by Jari Aalto [jari]
;;;
;;;###autoload
(defun ti::ck-do (key &optional xe)
  "Transform key binding to XEmacs or Emacs in current environment.
on current emacs. This enables you to have same key binding file
for both emacs versions. You can write key bindings either in XEmacs
or Emacs style.

    In Emacs :  (ti::ck-do '(meta up)) --> [M-up]
    In XEmacs:  (ti::ck-do [M-up])     --> '(meta up)

This function does the conversion only if it needs to, and returns
immediately if no conversion is needed. This should minimise performance
penalty.

Input:
  KEY    key sequence
  XE     flag. If this is nil, then Emacs env. is assumed. However
         `ti::ck--xemacs-flag' is obeyed if it is non-nil.
         If non-nil, then XEmacs env. is assumed and conversion to
         XEmacs like bindings are done."
  (let (
        ;;      For greater speed this is read from variable
        ;;      and not dynamically for every call.
        (xe     (or xe ti::ck--xemacs-flag))
        ret
        vec
        D)                              ;debug
    (cond
     ((and (not xe)                     ; in Emacs
           (vectorp key))               ; [C-up]
      (cond
       ((and (listp (elt key 0))
             (< ti::ck--emacs-minor 30)) ;19.30 supports [(control up)]
        (setq D "1 xe2gnu-vector")
        (setq ret (ti::ck-xe2gnu-vector key)))
       (t
        (setq D "1 as is")
        (setq ret key))))               ; return "as is"
     ((and (not xe)                     ; '(control f1) --> C-fi
           (listp key))
      (setq D "2 ti::ck-xe2gnu-list")
      (setq vec (make-vector 1 nil))
      (setq ret (ti::ck-xe2gnu-list key))
      (aset vec 0 ret)
      (setq ret vec))
     ((and xe
           (or (listp key)              ; '(control up) in XEmacs
               (symbolp key)            ; 'button2
               (and (vectorp key)       ; [(button2]) case...
                    (listp (elt key 0)))))
      (setq D "3")
      (setq ret key))                   ; return "as is"
     ((and xe
           (vectorp key))               ; [C-up] to XEmacs
      (setq D "4  gnu2xe-vector")
      (setq ret (ti::ck-gnu2xe-vector  key))))

    ;; Quiet XEmacs 19.14 ByteCompiler, This is no-op.
    (if D
        (setq D D))
    ret))

;;}}}

(provide   'tinylibck)
(run-hooks 'ti::ck--load-hook)

;;; tinylibck.el ends here
