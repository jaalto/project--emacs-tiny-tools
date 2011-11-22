;;; tinypage.el --- Handling ^L pages, select, cut, copy, head renumber.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
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
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file
;;
;;      (require 'tinypage)
;;
;;  or use this; your .emacs loads quicker. Preferred.
;;
;;      (global-set-key "\C-cmp" 'tinypage-mode)   ;; "m" for minor modes
;;      (autoload 'tinypage-mode "tinypage" "" t)
;;
;;  If you make any changes to keybindings, always run command
;;
;;      M-x tinypage-mode-install
;;
;;  to see what this mode offers, look at the mode description
;;
;;      M-x tinypage-mode
;;      C-h m

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, Jun 1996
;;
;;      I had found paged.el by Michelangelo Grigni <mic@mathcs.emory.edu>
;;      one year or so ago and had liked it very much. Unfortunately
;;      it used narrowing and didn't offer easy page select, copy, cut
;;      actions which belong to basic page editing.
;;
;;      Paged.el has one nice feature: It can renumber pages and make summary
;;      out of them. If I have time I will include those features to
;;      package too.
;;
;;  Overview of features
;;
;;      o   Copy, cut, paste, yank (after/before current page) ^L pages.
;;      o   Show  page-nbr/page-count/page-size in modeline.
;;      o   Can renumber numbered header levels, where last level is indicated
;;          with number. Eg. "A.1 A.2"  or "1.2.1.1 "1.2.1.2"
;;      o   Shows popup in X to jump to headings
;;      o   Create table of contents.
;;
;;  About making pages -- basics
;;
;;      The pages are made by adding the linefeed marker into the
;;      text. The page markers are interpreted by printed to print
;;      text on the following page. The page marker character is
;;      usually added on its own line just before the topics or
;;      headings. In Emacs, you need two key strokes to produce
;;	ASCII chacter Control-L:
;;
;;          C-q C-l  --> ^L
;;
;;	The layout of your document would look something like:
;;
;;         ^L
;;         1.0 Topic one
;;             txt txt txt txt txt txt txt txt
;;             txt txt txt txt txt txt txt txt
;;
;;         ^L
;;         1.1
;;              txt txt txt txt txt txt ..
;;
;;         ^L
;;         1.1.1.1
;;              txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt ...
;;
;;  About renumbering
;;
;;      The package offers simple renumbering features, but it has some
;;      limitations. Let's see an example of renumbering:
;;
;;          1.1
;;          1.7.1.5             (1)
;;          1.5
;;          1.5.4.1             (2)
;;          1.5.4.5
;;          1.9
;;
;;      The result is
;;
;;          1.1
;;          1.7.1.5
;;          1.2
;;          1.5.4.1
;;          1.5.4.2
;;          1.3
;;
;;      Notes:
;;
;;      .   It can't know that the 1.7.1.5 belongs under previous 1.1,
;;          because no back tracking is done.
;;
;;      .   Same goes here, it can't know that the 1.5.4.1 should actually
;;          start from 1.5.1.1
;;
;;      The thumb rule is, that you _go_ and make sure all the _first_
;;      level headings (those that end to X.X.1.1) are right before doing
;;      renumbering. In the above case, you should have done these before
;;      calling M-x tinypage-renumber-forward.
;;
;;      .   --> 1.1.1.1
;;      .   --> 1.5.1.1  _AND_ do replace M-% 1.5.4 with 1.5.1
;;
;;      Then all the renumberin would have gone just fine. Little
;;      handy work and this package helps you to number your doc
;;      easily.
;;
;;  Renumbering -- be cautious
;;
;;      If you have index section in you file, there is a little
;;      problem, because this package does not know anything about
;;      such things. If the Table of Contents section is at the
;;      beginning, go past it and use function:
;;
;;          M-x tinypage-renumber-forward
;;
;;      Using
;;
;;          M-x tinypage-renumber-buffer
;;
;;      Would be disaster. It can only be used for non-TOC buffers.
;;
;;  Creating index
;;
;;      After all headings are renumbered, the old TOC section needs
;;      update:
;;
;;          M-x tinypage-toc
;;
;;      Copy the shown buffer in place of the old TOC.
;;
;;  Limitations
;;
;;      Since the numbering is done according to regexp, there is
;;      no way to avoid the following false hit:
;;
;;          1.1 Overview
;;          This is highly technical document concerning the latest
;;          NASA ultrawave reflective shield technique. You should
;;          refer to chapter:
;;
;;              1.5
;;
;;          Where the Daddy-Cool portable sondium emission detector is
;;          described in full.
;;
;;      The number 1.5 is unfortunately renumbered to 1.2, and
;;      possibly causing headache in the NASA and in the spying
;;      countries. If you know elegant way to prevent these false
;;      hits, please drop me a mail.
;;
;;  Code Note
;;
;;      The renumbering used here uses brute force, so the execution time
;;      is O(n2). If you have more that 30-40 sections, the renumbering
;;      might take considerable time. If you improve the renumbering, plese
;;      send a patch.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)
(require 'easymenu)

(eval-and-compile
  (if (ti::xemacs-p)
      (or (load "overlay" 'noerr)
          (message "\n\
tinypage: ** you need XEmacs overlay.el library.
          ** TinyPage may not work correctly without it."))))

(ti::package-defgroup-tiny TinyPage tinypage-- tools
  "Minor mode for Handling ^L delimeted text rerions (pages).
  Overview of features
        o   Copy, cut, paste, yank (after/before current page) ^L pages.
        o   Show  page-nbr/page-count/page-size in modeline.
        o   Can renumber numbered header levels, where last level is indicated
            with number. Eg. \"A.1 A.2\"  or \"1.2.1.1\" \"1.2.1.2\"
        o   Shows popup in X to jump to headings
        o   Create table of contents.")

;;}}}
;;{{{ setup: private

(defcustom tinypage--load-hook nil
  "*Hook that is run when package is loaded."
  :type 'hook
  :group 'TinyPage)

;;}}}
;;{{{ setup: private variables

(defvar tinypage--post-command-wakeup-counter nil
  "Updated by program.")

(make-variable-buffer-local 'tinypage--post-command-wakeup-counter)

(defvar tinypage--buffer-toc "*toc*"
  "Where to create index.")

;;}}}
;;{{{ setup: public, user configurable

(defcustom tinypage--register ?p
  "*Register used for clipboard."
  :type  'character
  :group 'TinyPage)

;;; This is _not_ one char "^L", it is two chars "^" + "L"

(defcustom tinypage--mode-name-string " ^L"
  "*Minor mode name. User variable."
  :type  'string
  :group 'TinyPage)

(defcustom tinypage--post-command-wakeup-count 40
  "*How often to wake up to update modeline info.
Don't put too low value, since it slows down Emacs."
  :type ' integer
  :group 'TinyPage)

(defcustom tinypage--x-coord 170
  "*Default X menu coordinate."
  :type  'integer
  :group 'TinyPage)

(defcustom tinypage--y-coord 170
  "*Default Y menu coordinate."
  :type  'integer
  :group 'TinyPage)

(defcustom tinypage--x-popup-line-len 35
  "*Maximum line length in popup."
  :type  'integer
  :group 'TinyPage)

;;  Do not change this ! Unless you know what you do...

(defcustom tinypage--renumber-format
  '( "^[ \t]*\\([0-9.]+\\.\\)\\([0-9]+\\)"  1 2)
  "*Regexp for renumbered lines.

Format is

    (REGEXP SAME-LEVEL RENUM-LEVEL),

where the match in RENUM-LEVEL match must return a valid number.
Value SAME-LEVEL is examined only once and the _dots_ that is holds
are counted. The dots tell which mail-level was picked.
Eg, when renumbering the following, only the last number is incremented.

    1.2 Section one is here, this is picked first and examined.
    1.2.1.1 This subsection is skipped, since it's not in the same level.
    1.3 This level will be picked.

There _must_ be dots in the matched level string, because the section
level is calculated by counting the dots.  The following
section numbers won't do:

    1
    2"
  :type '(list
          (string :tag "Regexp")
          (integer :tag "Submatch in regexp")
          (integer :tag "Submatch in regexp"))
  :group 'TinyPage)

(defcustom tinypage--modeline-function 'tinypage-update-mode-line
  "*The modeline function that keep it up to date whenever called."
  :type 'integer
  :group 'TinyPage)

;;}}}
;;{{{ Minor Mode

;;;###autoload (autoload 'tinypage-mode          "tinypage" "" t)
;;;###autoload (autoload 'turn-on-tinypage-mode  "tinypage" "" t)
;;;###autoload (autoload 'turn-off-tinypage-mode "tinypage" "" t)
;;;###autoload (autoload 'tinypage-commentary    "tinypage" "" t)

(defvar tinypage--mode-name " ^L"
  "Minor mode name. Changed by program. not user variable.")

(make-variable-buffer-local 'tinypage--mode-name)

(eval-and-compile

;;; Prefix keys is "\" by default: this one
;;; was nicely non-shifted and near HP-UX return key. You can Change it
;;; prior loading the package with (setq tinypage--pref

  (ti::macrof-minor-mode-wizard
   "tinypage-" " ^L" "\\" "Tpage" 'TinyPage "tinypage--" ;1-6

   "Paged minor mode. This mode allows you to handle ^L delimited
region as page: you can e.g. cut, copy, and select it.

The page counter is _not_ updated all the time in the modeline, because
it'd be too heavy task to monitor user constantly. Please use command
\\[tinypage-modeline] if you want up to date information.

To adjust the user tracking threshold, modify value:

  `tinypage--post-command-wakeup-count'

Mode description:

  \\{tinypage--mode-prefix-map}"

   "Paged ^L mode"

   (progn
     ;; Make sure it's there...
     (or (assq 'tinypage-post-command post-command-hook)
         (add-hook 'post-command-hook 'tinypage-post-command))
     ;;  - We could leave the hook there because it's no-op if
     ;;    the mode variable is nil.
     ;;  - But i think if user looks at post-command-hook's contents
     ;;    to spot some problems, he appreciates if there is no extra
     ;;    functions in the hook -- only those that need to active
     ;;    in the current buffer/modes.
     (if (null tinypage-mode)
         (remove-hook 'post-command-hook 'tinypage-post-command)
       ;; Make sure it's there...
       (or (assq 'tinypage-post-command post-command-hook)
           (add-hook 'post-command-hook 'tinypage-post-command)))
     (tinypage-modeline)
     (ti::compat-modeline-update))
   "TinyPage menu"
   (list
    tinypage--mode-easymenu-name
    ["Cut"                     tinypage-cut                            t]
    ["Copy"                    tinypage-copy                           t]
    ["Select"                  tinypage-select                         t]
    ["Yank"                    tinypage-yank                           t]
    ["Yank before page"        tinypage-yank-before                    t]
    ["Yank after page"         tinypage-yank-after                     t]
    "----"
    ["Renumber buffer"         tinypage-renumber-buffer                t]
    ["Renumber forward"        tinypage-renumber-forward               t]
    ["Renumber Level forward"  tinypage-renumber-level-forward         t]
    "----"
    ["Index"                   tinypage-toc                            t]
    ["Index occur"             tinypage-toc-occur                      t]
    ["Index popup"             tinypage-toc-x-popup                    t]
    "----"
    ["Update modeline info"    tinypage-modeline                       t]
    ["Previous heading"        tinypage-go-previous                    t]
    ["Next heading"            tinypage-go-next                        t]
    ["Scroll down"             scroll-down                             t]
    ["Scroll up"               scroll-up                               t]
    "----"
    ["Package version"         tinypage-version                        t]
    ["Package commentary"      tinypage-commentary                     t]
    ["Mode help"               tinypage-mode-help                      t]
    ["Mode off"                tinypage-mode                           t])
   (progn
     (define-key map  "?"  'tinypage-mode-off)
     (define-key map  "Hm" 'tinypage-mode-help)
     (define-key map  "Hc" 'tinypage-commentary)
     (define-key map  "Hv" 'tinypage-version)
     ;; These are the DOS standard keys, mimic them
     ;; Alt c  = copy
     ;; Alt t  = cut
     ;; Alt p  = paste
     (define-key   map  "c" 'tinypage-copy)
     (define-key   map  "t" 'tinypage-cut)
     (define-key   map  "p" 'tinypage-yank)
     (define-key   map  "s" 'tinypage-select)
     ;; ....................................................... yanking ...
     ;; Emacs users are more familiar with this
     (define-key   map  "y"  'tinypage-yank)
     ;;  Some handy paste commands. Moving pages around
     ;;  See the keyboads: < >       which mean before , after
     ;;  I don't want to use shift...
     (define-key   map  "," 'tinypage-yank-before)
     (define-key   map  "." 'tinypage-yank-after)
     ;; ..................................................... numbering ...
     ;; key "n" for numbering.
     (define-key   map  "nl"  'tinypage-renumber-level-forward)
     (define-key   map  "nf"  'tinypage-renumber-forward)
     (define-key   map  "nb"  'tinypage-renumber-buffer)
     ;; ......................................................... index ...
     ;; key "i" for indexing
     ;;  I didn't pick "x" for X-popup because it's too far away
     ;;  from the "i" key. The "p" for "popup" is much closer.
     (define-key   map  "ii"  'tinypage-toc)
     (define-key   map  "io"  'tinypage-toc-occur)
     (define-key   map  "ip"  'tinypage-toc-x-popup-keyboard)
     ;; ...................................................... events ...
     ;; Too bad the delete key is not standard, we have to define
     ;; many symbols
     (define-key   map [(delete)]       'tinypage-cut)
     (define-key   map [(del)]          'tinypage-cut)
     (define-key   map [(deletechar)]   'tinypage-cut)
     (define-key   map [(hpDeleteChar)] 'tinypage-cut)
     (define-key   map [(backspace)]    'tinypage-copy)
     (define-key   map [(insert)]       'tinypage-yank)
     (define-key   map [(insertchar)]   'tinypage-yank)
     (define-key   map [(hpInsertChar)] 'tinypage-yank)
     (if (ti::emacs-p)
         (define-key   map [(mouse-1)]   'tinypage-toc-x-popup)
       (define-key     map [(button1up)] 'tinypage-toc-x-popup))
     ;; .................................................... go, update ...
     (define-key   map  "u" 'tinypage-modeline)
     (define-key   map [(prior)]         'tinypage-go-previous)
     (define-key   map [(next)]          'tinypage-go-next)
     (define-key   map [(control prior)] 'scroll-down)
     (define-key   map [(control next)]   'scroll-up))))

;;}}}
;;{{{ misc, engine funcs

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-modeline ()
  "Update modeline information."
  (interactive)
  (funcall tinypage--modeline-function))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-page-region (&optional verb)
  "Return region (BEG . END) of page. VERB."
  (interactive)
  (let (beg
	end
	ret)
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*\C-l")
          (setq beg (point))
        (setq beg (tinypage-go-next 'back)))
      (when (setq end (tinypage-go-next))
        (goto-char end)                 ;adjust point, do not take ^L
        (beginning-of-line)
        (setq end (point))))
    (if (and beg end)
        (setq ret (cons beg end))
      (if verb
          (message "Couldn't find region")))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-count-pages ()
  "Count page characters ^L."
  (let ((count 0))
    (save-excursion
      (ti::pmin)
      (while (re-search-forward "^[ \t]*\C-l" nil t)
        (incf  count)))
    count))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-count-lines-in-page ()
  "Count lines."
  (let* ((elt (tinypage-page-region))
         (beg (car-safe elt))
         (end (cdr-safe elt))
         ret)
    (when elt
      (setq ret (count-lines beg end)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-current-page ()
  "Current page."
  (interactive)
  (let ((re    "^[ \t]*\C-l")
	(point (point))
	(count 0))
    (save-excursion
      (ti::pmin)
      (while (re-search-forward re point t)
        (incf count)))
    (if (looking-at re)
        (incf count))
    count))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-update-mode-line ()
  "Update modeline info."
  (interactive)
  (let ((mode-string  tinypage--mode-name-string)
	pages
	now
	lines)
    (setq pages (tinypage-count-pages))
    (setq now   (tinypage-current-page))
    (setq lines (tinypage-count-lines-in-page))
    (setq tinypage--mode-name
          (format  " %s %s/%s/%s" mode-string now pages (or lines "-")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-post-command ()
  "Keep page info in modeline up to date."
  (when tinypage-mode                   ;only now!
    (if (not (integerp tinypage--post-command-wakeup-counter))
        (setq tinypage--post-command-wakeup-counter 0))
    (incf  tinypage--post-command-wakeup-counter)

    (when (eq 0 (% tinypage--post-command-wakeup-counter
                   tinypage--post-command-wakeup-count))
      (tinypage-modeline))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-overlay (act &optional beg end)
  "If ACT is 'hide, hide overlay, otherwise highlight BEG END."
  (let ((ov (ti::compat-overlay-some)))
    (cond
     ((eq act 'hide)
      (ti::compat-overlay-move ov 1 1))
     (t
      (ti::compat-overlay-move ov beg end)
      (setq ov (symbol-value ov))
      (push-mark
       (if (ti::emacs-p)
           (ti::funcall 'overlay-start ov)
         (ti::funcall 'extent-start-position ov))
       t t)
      (push-mark
       (if (ti::emacs-p)
           (ti::funcall 'overlay-end ov)
         (ti::funcall 'extent-end-position ov))
       t t)
      (setq this-command 'set-mark)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-page-mark-region (beg end &optional act maybe)
  "Mark region and do some command act.

Input:

  BEG           region beg
  END           region end
  ACT           action name, default is 'copy. Can be also 'cut 'select
  MAYBE         flag to check is BEG END are valid: if not then do
                nothing. If vallid; then select and do ACT.

References:

  `tinypage--register'

Return:

  t             if successfull
  nil"
  (let ((doit (if maybe
		  (and beg end)
		t))
	(reg tinypage--register))
    (cond
     ((null doit)
      nil)
     (t
      (tinypage-overlay 'show beg end)
      (cond
       ((eq 'cut act)
        (delete-region beg end)
        (goto-char beg))
       ((memq act '(nil copy))
        (set-register
         reg
         (ti::remove-properties (buffer-substring beg end)))))
      ;;  something done
      t))))

;;}}}
;;{{{ application functions

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-renumber-level-forward (&optional verb)
  "Renumber current level starting from current line. VERB.
Only the last level number is incremented. Put cursor line above
the level and call this function

    1.2
    *               <-- cursor here and it'll renumber level 1.2.1.x
    1.2.1.1

References:

  `tinypage--renumber-format'"
  (interactive "P")
  (let* ((data   tinypage--renumber-format)
         (re     (nth 0 data))
         (lev1   (nth 1 data))
         (lev2   (nth 2 data))
         nbr
         counter
         dots-exact
         dots
         level-string
         orig-level-string)
    (ti::verb)
    (save-excursion
      (while (re-search-forward re nil t)
        (setq level-string (match-string lev1))
        (when level-string
          (setq dots (count-char-in-string ?. level-string)))
        (when (and (null dots-exact)    ;do only once
                   dots)
          (setq dots-exact              dots
                orig-level-string       level-string))
        (when (and dots-exact
                   (eq dots-exact dots) ;only same level accepted
                   ;;  Must have same beginning "1.2.1.1" "1.2.1.x"
                   ;;
                   (string= orig-level-string level-string)
                   (setq nbr (match-string lev2)))
          (if (null counter)            ;first value ?
              (setq counter (string-to-number nbr))
            (incf  counter)
            ;;  Replace the last number with the right increment
            (ti::replace-match lev2 (number-to-string counter))))))
    (when verb
      (cond
       ((null counter)
        (message "No matches."))
       (t
        (message "Last heading was %s%s" orig-level-string counter))))
    counter))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-renumber-forward (&optional verb)
  "Renumber all found headings forward. VERB."
  (interactive "P")
  (let* ((data tinypage--renumber-format)
	 (re   (nth 0 data)))
    (ti::verb)
    ;; Well, we do lot of extra work here, because the
    ;; tinypage-renumber-level-forward goes alway to the bottom,
    ;; but what the heck... it won't take long.
    ;;
    ;; And code is much cleaner this way.
    (if verb
        (message "Renumbering..."))
    (while (re-search-forward re nil t)
      (beginning-of-line)
      (tinypage-renumber-level-forward)
      (forward-line 1))
    (if verb
        (message "Renumbering...done"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-renumber-buffer ()
  "Renumber all headings in buffer starting from `point-min'."
  (interactive)
  (save-excursion
    (ti::pmin)
    (tinypage-renumber-forward 'verb)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypage-get-index-list ()
  "Return list of strings."
  (let ((list (ti::buffer-grep-lines (nth 0 tinypage--renumber-format)))
	ret)
    (dolist (elt list)
      (push (ti::string-remove-whitespace elt) ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-toc (&optional ragged no-show)
  "Create toc to temporary buffer.
Optional argument RAGGED makes the heading to 'hang'.
With nil RAGGED, the headings are lined up.

NO-SHOW doesn't show buffer after creating table of content.

Return:
  buffer"
  (interactive "P")
  (let ((list          (ti::buffer-grep-lines
			(nth 0 tinypage--renumber-format)))
	(buffer        (ti::temp-buffer tinypage--buffer-toc 'clear))
	dots
	padd
	heading
	text)
    (with-current-buffer buffer
      (dolist (elt list)
        (setq elt (ti::string-remove-whitespace elt))
        (setq heading (ti::string-match "^[^ \t]+" 0 elt))
        (setq text    (or (ti::string-match "^[^ \t]+[ \t]+\\(.*\\)" 1 elt)
                          "<no heading found>"))
        ;; How to indent this line
        (setq dots (count-char-in-string ?. heading))
        (if (<= dots 1)
            (setq padd "")
          (setq padd (make-string (* 2 (- dots 2)) ?\  )))
        ;;  Separate 1.0  topics
        (if (string-match "0$" heading)
            (insert "\n"))
        (setq heading (concat padd heading))
        (if ragged
            (insert heading "    " text "\n")
          (insert (format "%s %s\n" heading text)))))
    (unless no-show
      ;; Display it, but do not select/go to it.
      ;;
      (display-buffer buffer)
      (ti::save-excursion-macro
        (select-window (get-buffer-window buffer))
        (shrink-window-if-larger-than-buffer)))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-toc-x-popup-keyboard ()
  "Create index. Show it in X-popup."
  (interactive)
  (tinypage-toc-x-popup
   (ti::compat-make-fake-event tinypage--x-coord tinypage--y-coord)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-toc-x-popup (event)
  "Create index. Show it in X-popup with EVENT."
  (interactive "e")
  (let ((len    tinypage--x-popup-line-len)
	(title  "Index")
	list
	val
	point)
    (cond
     ((null (ti::compat-window-system))
      (message "Sorry, Requires X to use X-popup"))
     (t
      (setq list (tinypage-get-index-list))
      (setq list (mapcar
                  (function
                   (lambda (x)
                     (ti::string-left x len)))
                  list))
      (when (setq val (ti::compat-popup list event nil title))
        ;;  See if we can find the heading...
        (ti::save-excursion-macro
          (ti::pmin)
          (if (re-search-forward (regexp-quote val) nil t)
              (setq point (line-beginning-position))
            (message "Cannot find heading..."))))
      (if point
          (goto-char point))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypage-toc-occur ()
  "Create occur buffer for jumpig to Headings easily."
  (interactive)
  (occur (nth 0 tinypage--renumber-format)))

;;}}}
;;{{{ interactive funcs

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-region-action (act &optional verb)
  "Execute action ACT. Return t or nil. VERB."
  (let* ((elt   (tinypage-page-region verb))
         (beg   (car-safe elt))
         (end   (cdr-safe elt)))
    (ti::verb)
    (tinypage-page-mark-region beg end act 'maybe)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-select (&optional verb)
  "Select page. If sitting on page Marker, use page below. VERB."
  (interactive "P")
  (ti::verb)
  (and (tinypage-region-action 'select verb)
       (if verb
           (message "Page selected."))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-copy (&optional verb)
  "Select page. If sitting on page Marker, use page below. VERB."
  (interactive "P")
  (ti::verb)
  (and (tinypage-region-action 'copy verb)
       (if verb
           (message "Page copied."))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-cut (&optional verb)
  "Select page. If sitting on page Marker, use page below. VERB."
  (interactive "P")
  (ti::verb)
  (tinypage-region-action 'cut verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-yank (&optional verb)
  "Yank page from register. VERB."
  (interactive "P")
  (insert-register tinypage--register)
  (tinypage-overlay 'hide))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-yank-before (&optional verb)
  "Yank page from register, but _before_ current page. VERB."
  (interactive)
  (ti::verb)
  (tinypage-yank-after 'before "Yanked before this page." verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-yank-after (&optional before msg verb)
  "Yank page from register, but _after_ current page.
Optionally BEFORE with MSG and VERB."
  (interactive)
  (or msg
      (setq msg "Yanked after this page."))
  (ti::verb)
  (ti::save-with-marker-macro
    (when (tinypage-go-next before verb)
      (insert-register tinypage--register)
      (tinypage-overlay 'hide)
      (if (and verb msg)
	  (message msg)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-go-previous (&optional verb)
  "Go to previous page. VERB."
  (interactive)
  (ti::verb)
  (tinypage-go-next  'back verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypage-go-next (&optional back verb)
  "Go to next page, optionally BACK. Return point if moved. VERB."
  (interactive)
  (let ((point (point))
	func
	ret)
    (ti::verb)
    (cond
     (back
      (setq func 're-search-backward)
      (beginning-of-line))
     (t
      (setq func 're-search-forward)
      (end-of-line)))
    (unless (setq ret (funcall func "\C-l" nil t))
      (goto-char point))
    (if verb
        (tinypage-modeline))
    (if (and verb (null ret))
        (message "No more page marks."))
    ret))

;;}}}

(add-hook 'tinypage--mode-define-keys-hook 'tinypage-mode-define-keys)

(provide   'tinypage)
(run-hooks 'tinypage--load-hook)

;;; tinypage.el ends here
