;;; tinysearch.el --- Grab and search word under cursor
;; $Id: tinysearch.el,v 2.49 2007/05/07 10:50:14 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1994-2007 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinysearch-version.
;; Look at the code with folding.el.

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

;;; Intallation:
;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file.
;;
;;      (require 'tinysearch)
;;
;; or use autoload, and your ~/.emacs loads quicker
;;
;;      (autoload 'tinysearch-search-word-forward  "tinysearch" "" t)
;;      (autoload 'tinysearch-search-word-backward "tinysearch" "" t)
;;
;;      ;;  Install default keybindings: M-s (forward search), C-M-s
;;      ;;  (bbackward), M-Mouse-1 (forward), C-M-Mouse-1 (backward)
;;      (add-hook 'tinysearch-:load-hook 'tinysearch-install)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, 1994
;;
;;      In 7 Nov 1994 <aep@world.std.com> (Andrew E Page) posted
;;      interesting code by article name 'Script Example: Search for next
;;      word', which was interesting. The idea of the code was good, but it
;;      didn't work as expected at all. Gradually the idea was crystallized
;;      into this package.
;;
;;        "Why we need search word package, when in emacs I can do `C-s' to
;;        enter search mode: C-w C-w C-w to grap words immediately after
;;        point and finally C-s to start searching...?"
;;
;;      Well, people tend to forget, that life was out there when 19.xx
;;      wan't in hands of developers. This package was originally made for
;;      18. The advantage of this package is the variable
;;
;;          tinysearch-:word-boundary-set
;;
;;      which you can easily change whenever you need (e.g. thru
;;      functions). To do the same in emacs, you have to go and modify the
;;      syntax entries involved...then come back again when you're done. I
;;      never do that, I seldom touch the syntax entries. Besides all
;;      mode-xxx go crazy if I would do so. Now you see the advantage?
;;
;;      And of course I feel more comfortable to do just one keypress,
;;      like like `M-s' to search forward instead of cubersome C-s C-w C-w
;;      C-w [n times] and finally C-s
;;
;;  Description
;;
;;      Grab word under oint and searches fwd/back. The word is inserted
;;      into Emacs's search ring, so that you can later continue with `C-s'
;;      or with `C-r' call.
;;
;;  Why doesn't it find my C++ function class::InitClass() ??
;;
;;      User pressed the search function over the call:
;;
;;          InitClass();        << Here
;;          i = i +1;
;;
;;      Why isn't the function found? Remember that this searches
;;      'true' words, not parts of them. A word is surrounded by at
;;      least one whitespace, since it's not a word if it is concatenated
;;      together with something else.
;;
;;      The problem is, that if is you define ':' to belong to a
;;      character set in C++, [because you propably want to grab
;;      variables easily. including the scope operator
;;      'clss::variable' or '::global'], this package expects to
;;      find word a boundary:
;;
;;          nonWordWORDnonWord
;;         =======    =======
;;
;;      And as you can see, the if ':' belongs to word, it can't
;;      simultaneously belong to NonWord ! Summa summarum: Revert to
;;      emacs C-s for a moment, since the word is automatically added
;;      to the search buffer.
;;
;;  Word accept function note:
;;
;;      There is variable `tinysearch-:accept-word-function', which has
;;      default function
;;
;;          tinysearch-accept-word
;;
;;      The function's purpose is to check if the searched word is
;;      accepted and that search should be terminated. Currently there it
;;      contains some programming logic for C/C++ languages, so that
;;      certain hits are ignored. Consider following case:
;;
;;          struct *foo;   - 1
;;          foo->x;        - 2
;;          x->foo         - 3
;;
;;          int foo, x;    - 4
;;          foo = x;       - 5        * start of 'foo' and 'x' search backward
;;
;;      C/C++ mode, searching for 'foo' finds 4,2,1  -- Not 3
;;      C/C++ mode, searching for 'x'   finds 5,4,3  -- Not 2
;;      But in text-mode, you would find all occurrances.
;;
;;      The added logic to C++ ignores the struct's MEMBER matches so that
;;      you really can find the "main" variables. If you don't like
;;      this added feature, you can alwasy go to
;;
;;          M-x text-mode
;;
;;      For a while, or if want to permanently switch this feature off,
;;      you set the variable `tinysearch-:accept-word-function' to nil, which
;;      causes all hits to be accepted.
;;
;;      Needless to say, that you can use put your own checking
;;      function in that variable to control the accurrances better.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(eval-when-compile (ti::package-use-dynamic-compilation))

(ti::package-defgroup-tiny TinySearch tinysearch-: extensions
  "search word under cursor: backward, forward.")

;;}}}
;;{{{ hooks

;;; ......................................................... &v-hooks ...

(defcustom tinysearch-:before-hook nil
  "*Hook that is run at the BEG of search function.
You can set this to point to function that alters the value of
`tinysearch-:word-boundary-set' e.g. by looking at the file type."
  :type  'hook
  :group 'TinySearch)

(defcustom tinysearch-:final-hook nil
  "*Hook that is _always_ run at the END of search function.
It doesn't care about word grabbings or search failures."
  :type  'hook
  :group 'TinySearch)

(defcustom tinysearch-:load-hook nil
  "*Run when package has been loaded.
A good candidate could be `tinysearch-install-default-keybindings'."
  :type  'hook
  :group 'TinySearch)

;;}}}
;;{{{ variables

;;; ....................................................... &v-private ...

(defvar tinysearch-:direction nil
  "Tell direction of search. nil = forward.")

(defvar tinysearch-:search-status nil
  "Status of word search. t = successful.")

(defvar tinysearch-:overlay nil
  "Overlay used for highlighting.
Created and killed during program execution.")

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinysearch-:word-boundary-set "-A-Za-z0-9_"
  "*Character set to conform a single word.
You might want to set this to something else before doing search."
  :type  'hook
  :group 'TinySearch)

(defcustom tinysearch-:wrap-flag  nil
  "*Non-nil means wrap buffer if there is no more match."
  :type  'boolean
  :group 'TinySearch)

(defcustom tinysearch-:accept-word-function  'tinysearch-accept-word
  "*Function run after the search for word has been successful.
If this variable contains non-existing function (like nil), the
content of the variable is ignored.

Default function:

  'tinysearch-accept-word'

Passed args to function:

 string     word being searched

Return values of function:

  t         accept search
  nil       do not accept search, continue searching next word."
  :type  'function
  :group 'TinySearch)

;;; ....................................................... &v-version ...

(eval-and-compile
  (ti::macrof-version-bug-report
   "tinysearch.el"
   "tinysearch"
   tinysearch-:version-id
   "$Id: tinysearch.el,v 2.49 2007/05/07 10:50:14 jaalto Exp $"
   '(tinysearch-:version-id
     tinysearch-:before-hook
     tinysearch-:final-hook
     tinysearch-:load-hook
     tinysearch-:direction
     tinysearch-:search-status
     tinysearch-:overlay
     tinysearch-:word-boundary-set
     tinysearch-:wrap-flag)))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ 19.xx isearch add

;;; ----------------------------------------------------------------------
;;;
(defun tinysearch-add-to-isearch-search-ring (isearch-string)
  "Add search pattern to ISEARCH-STRING in Emacs.
This code is directly taken from function `isearch-done' By Daniel LaLiberte."
  (if (> (length isearch-string) 0)
      ;; Update the ring data.
      (if isearch-regexp
          (if (or (null regexp-search-ring)
                  (not (string= isearch-string (car regexp-search-ring))))
              (progn
                (setq regexp-search-ring
                      (cons isearch-string regexp-search-ring))
                (if (> (length regexp-search-ring) regexp-search-ring-max)
                    (setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
                            nil))))
        (if (or (null search-ring)
                (not (string= isearch-string (car search-ring))))
            (progn
              (setq search-ring (cons isearch-string search-ring))
              (if (> (length search-ring) search-ring-max)
                  (setcdr (nthcdr (1- search-ring-max) search-ring) nil)))))))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;;
(defun tinysearch-accept-word  (word)
  "Determine if we accept searched WORD."
  (let* ((type      (symbol-name major-mode))
         (ret       t)                  ;default, accept search
         space-word)
    (cond
     ((string-match "^c-\\|^cc-\\|c[+]+" type)
      ;; Check C/C++ dependent variables, where rg. 'a' is
      ;; searched
      ;; a = 1                  , begin search with 'a'
      ;; a = a + 1              , accepted hit
      ;; struct->a              , not accepted hit, continue search
      (setq space-word
            (save-excursion
              (or (ti::buffer-read-space-word)
                  "")))
      (if (string-match (concat "\\(->\\|[.]\\)" (regexp-quote word))
                        space-word)
          ;;  discard this one.
          (setq ret nil))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinysearch-grab-word (&optional charset beg end )
  "Gets word under cursor limited by CHARSET string.
Optional BEG and END gives maximum search limits.
Default boundary is line limit."
  (let* (re-word-boundary
         re-word
         ;;  We accept ':' and '-' , beasuse they are used in c++ and lisp
         (charset (or charset "-:A-Za-z0-9_"))
         (beg (or beg (line-beginning-position)))
         (end (or end (line-end-position)))
         pb
         pe
         p
         re
         ret)
    (setq re-word-boundary  (concat  "[^" charset "]"))
    (setq re-word (concat  "[" charset "]")) ;considered single word
    ;; Note:  the first search goes backwards to find the start of the
    ;;        word, which is one character in front of the character
    ;;        found by the search.  Then we go forward to the end of
    ;;        word which is one character behind the character found by the
    ;;        search.
    (save-excursion                     ;conceive original (point)
      (if (re-search-backward re-word-boundary beg t)
          (setq pb (1+ (point))))
      (if pb nil                        ;already found
        (setq p (point))
        (beginning-of-line)
        (if (eq p (point))              ;we were at the BEG
            (setq re re-word)
          (setq re (concat re-word "+")))       ;skip chars
        (if (re-search-forward re (1+ p) t)     ; word at the BEG
            (setq pb beg))))
    ;;  Then search end point
    (save-excursion
      (if (re-search-forward re-word-boundary end t)
          (setq pe (1- (point))))
      (if pe nil                               ;already found
        (if (looking-at (concat re-word "+$")) ; handle word at the END of ln
            (setq pe end))))
    (if (and pb pe)
        (setq ret (buffer-substring pb pe)))
    ;;  easier to debug this way
    ret))

;;; ----------------------------------------------------------------------
;;; - There is lot of re-search-backward/fwd commands and it is intentional,
;;;   so that the code is totally emacs version independent. Newer emacs
;;;   has nice functions that shrink this code to 10 lines :->
;;; - Never grab word function is already coded in tinylib.el
;;;
(defun tinysearch-search-word-main (&optional backward set)
  "Gets word under cursor and search next occurrence.
If BACKWARD is non-nil, the search will be headed backward, the SET
corresponds to `tinysearch-:word-boundary-set'.

Before searching is done the tinysearch-hooks is thrown. This is useful
is you want someone to dynamically change the search-word's idea of
the chars belonging to word. By setting `tinysearch-:word-boundary-set' you
can set different sets for text and Lisp.  [In Lisp the '-' is part of
word while in text it normally isn't].

NOTE:

   You cannot search 1 char words with this due to internal
   behaviour of search method and cursor positioning."
  (interactive "P")
  (let ((wrap   tinysearch-:wrap-flag)
        (loop   0)
        (accept t)
        charset
        re-charset
        word found
        re-word-boundary  re-word
        prev-point
        no-msg
        mb
        me)
    (or tinysearch-:overlay
        (setq tinysearch-:overlay (ti::compat-overlay-some)))
    ;; ................................................... set charset ...
    (setq tinysearch-:direction backward ;inform possible hook func
          charset           (or set tinysearch-:word-boundary-set)
          re-word-boundary  (concat  "[^" charset "]")
          re-word           (concat  "[" charset "]") ;considered single word
          re-charset        re-word)
    ;;   Let the user set the word criteria
    (if tinysearch-:before-hook
        (run-hooks 'tinysearch-:before-hook))
    ;; ...................................................... set word ...
    (setq word (tinysearch-grab-word charset))
    (if (null word)
        (message "TinySearch: Word not grabbed.")
      ;;   enable C-s and C-r to use the word, look isearch.el
      ;;   NOTE: this doesn't put the WORD regexp there...
      (tinysearch-add-to-isearch-search-ring word)
      ;; post a message saying what we're looking for
      (message "searching for \`%s\`" word)
      (setq no-msg (concat "TinySearch: No more words [" word "]" ))
      (setq re-word
            (concat
             "\\(^" word  "\\|"
             re-word-boundary word "\\)" re-word-boundary))
      ;; ................................................... do search ...
      (while loop
        ;;  Record the point only if the word is accepted.
        (if accept
            (setq prev-point (point)))
        (if backward                    ;choose backward
            (progn
              (setq found (re-search-backward re-word nil t))
              (if (null found)
                  (message no-msg)
                (save-match-data        ;highlight needs orig region
                  (unless (looking-at re-charset)
                    (re-search-forward re-charset) ;Goto first char
                    (backward-char 1)))))
          ;;  - This a little hard to explain: the search
          ;;    does not succeed, if the variable 'a' is at
          ;;    the beginning of line due to backward-char 2 correction
          (if (eq (current-column) 0)
              (ignore-errors (forward-char 1)))
          (setq found (re-search-forward re-word nil t))
          (if found
              (backward-char 2)))
        (if found
            ;;  - So that NEXT word will be grabbed, that's why 1 char words
            ;;    can't be found
            (setq mb (match-beginning 0)   me (match-end 0) )
          (message no-msg))
        ;; ........................................................ done ...
        (setq tinysearch-:search-status found) ;save status
        ;;  Should we continue searching ?
        (cond
         ((and (null found)
               wrap)
          (if (> loop 0)
              (setq loop nil)           ;No hits at all
            (if backward                ;start a new round
                (ti::pmax)
              (ti::pmin))))
         ((and (null found)
               (> loop 0))
          ;;  Word accept function caused loop to run again, but
          ;;  there were no more hits. Back to prev position
          (goto-char prev-point)
          (setq loop nil))
         ((or (null found)
              (not (fboundp tinysearch-:accept-word-function)))
          (setq loop nil))
         ((and found
               ;;  Is this found word accepted in the context
               ;;  surrounding the text ?
               (setq accept (funcall tinysearch-:accept-word-function word)))
          ;;  Restore previous search point
          (setq loop nil)))
        ;; .................................................... do hilit ...
        (if (and tinysearch-:overlay found (null loop))
            (ti::compat-overlay-move tinysearch-:overlay  mb me nil 'highlight))
        (when tinysearch-:overlay       ;Hide overlay
          (sit-for 1)
          (ti::compat-overlay-move tinysearch-:overlay 1 1))
        (if loop
            (incf  loop))))
    ;; ---------------------- grabbed
    (if tinysearch-:final-hook
        (run-hooks 'tinysearch-:final-hook))))

;;; ----------------------------------------------------------------------
;;;
(defun tinysearch-charset-control ()
  "Dynamic character set change according to mode. This is example function."
  (let* ((type (symbol-name major-mode))
         set)
    (cond
     ((string-match  "^c-\\|^cc-\\|c[+]+" type)
      (setq set "A-Za-z0-9_"))
     ((string-match "lisp" type)
      ;;  Add ':' , which I use in variable names.
      (setq set "-:A-Za-z0-9_"))
     ((string-match "text\\|shell\\|perl" type)
      (setq set "A-Za-z0-9_")))
    set))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinysearch-search-word-forward ()
  "Search word at point forward."
  (interactive)
  (tinysearch-search-word-main nil (tinysearch-charset-control)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinysearch-search-word-backward ()
  "Search word at point backward."
  (interactive)
  (tinysearch-search-word-main 'back (tinysearch-charset-control)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinysearch-install-default-keybindings (&optional uninstall)
  "Install default keybindings; M-s C-M-s, M-Mouse-1, C-M-Mouse-1."
  (interactive)
  (global-set-key [(meta ?s)] 'tinysearch-search-word-forward)
  (global-set-key [(control meta ?s)] 'tinysearch-search-word-backward)
  ;;  For mouse (under windowed system)
  (global-set-key [(meta control mouse-1)]
                  'tinysearch-search-word-forward)
  (global-set-key [(meta control shift mouse-1)]
                  'tinysearch-search-word-backward))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinysearch-install (&optional arg)
  "Call `tinysearch-install-default-keybindings' with ARG."
  (interactive)
  (tinysearch-install-default-keybindings arg))

;;}}}

(provide   'tinysearch)
(run-hooks 'tinysearch-:load-hook)

;;; tinysearch.el ends here
