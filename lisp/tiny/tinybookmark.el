;;; tinybookmark.el --- Keep file in organized sections

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        tools
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
;;{{{ Installation

;;; Install:

;; ........................................................ &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file
;;
;;      (require 'tinybookmark)
;;
;; or use autoload, and Emacs starts up faster, prefered:
;;
;;      (autoload 'tinybookmark-insert          "tinybookmark" "" t)
;;      (autoload 'tinybookmark-repeat          "tinybookmark" "" t)
;;      (autoload 'tinybookmark-parse           "tinybookmark" "" t)
;;      (autoload 'tinybookmark-forward         "tinybookmark" "" t)
;;      (autoload 'tinybookmark-backward        "tinybookmark" "" t)
;;      (autoload 'tinybookmark-keyboard        "tinybookmark" "" t)
;;      (autoload 'tinybookmark-keyboard-parse  "tinybookmark" "" t)
;;
;;      (when window-system
;;        (autoload 'tinybookmark-mouse         "tinybookmark" "" t)
;;        (autoload 'tinybookmark-mouse-parse   "tinybookmark" "" t))
;;
;; Ideas for keybindings
;;
;;      M-x tinybookmark-insert  to add bookmark
;;
;;      ;;  This is for windowed Emacs. It brings up nice pop up menu
;;      ;;  In XEmacs, use different mouse event: `mouse1down'
;;
;;      (global-set-key [(?\e) (control mouse-1)]        'tinybookmark-mouse)
;;      (global-set-key [(?\e) (control shift mouse-1)]  'tinybookmark-mouse-parse)
;;
;;      ;;  To use keyboard to navigate between bookmarks
;;
;;      (global-set-key [(shift left)]  'tinybookmark-backward)
;;      (global-set-key [(shift right)] 'tinybookmark-forward)
;;
;;      ;; To navigate with completion menu
;;
;;      (global-set-key [(control shift right)] 'tinybookmark-keyboard)
;;
;; BE SURE THAT
;;
;;      Variables `comment-start' and `comment-end' are defined,
;;      otherwise the inserted text won't have proper prefix abd ending.

;;}}}
;;{{{ Documentation

;;; .................................................... &t-commentary ...

;;; Commentary:

;;  Preface, feb 1995
;;
;;      Long ago I used a little function I wrote that inserted section
;;      breaks, those that I call `book' `marks'. There was also
;;      `folding.el' to keep the code in separate sections. Findings things
;;      was easy when you just searched either book marks or jumped between
;;      folds. Next *imenu.el* was announced which provided X-pop up for
;;      book marks and adding support to it was the start of this package.
;;
;;  Overview of features
;;
;;      o   Provide 'setting book marks' functions: Add
;;          repeated characters and sequences up till end of line with
;;          named identifier.
;;      o   Automatically parse book marks from file, if it contains
;;          identifier `bookMarkRegexp' which defines book mark syntax for
;;          the file. Uses X-popup [imenu] to show those book marks and
;;          moving between them.
;;
;;  How to keep files organized
;;
;;      There are several tools to keep your code organized and they are at
;;      their best if you think how they can co-operate. There is
;;      *folding.el* and *tinybookmark.el*, which might seem to do double
;;      job, since they both divide code into more easily manageable
;;      sections. The key point is that when folding is used, one works
;;      _within_ some special section and possibly want to hide all the
;;      rest of the code. But when jumping easily back and forth on the
;;      buffer, it us *unfolded* and TinyBookmark is used. Now, to confuse
;;      you more, there is also *imenu.el* which can be used to jump inside
;;      code. It can be configured so that it will pick all function names
;;      inside list, and when you want to go to specific function, just
;;      pick one from imenu.
;;
;;      To summarize:
;;
;;      o   folding.el      - For hide unneeded code,
;;                            clear view on the structure
;;      o   tinybookmark.el - Jump between/finding  _large_ code sections
;;      o   imenu.el        - Finding specific function, more detailed control.
;;      o   tinyhotlist.el  - Add/remove files from permanent X-popup list
;;
;;  How to use this package
;;
;;      There is following function that inserts book mark on the current line
;;
;;          M-x tinybookmark-insert
;;
;;      There is also normal repeat function, that fills line with your
;;      pattern:
;;
;;          M-x tinybookmark-repeat
;;
;;      Normally the usual book mark separator is the "." <dot> , which
;;      isn't so "noisy" as continuous '-' line. Normally you add some
;;      unused ID character, like '&' at front of real book mark, like
;;      this:
;;
;;          ;;; .................................. &How-to-use ...
;;          (defun test ()
;;           (progn
;;            ..
;;            (goto-char ..
;;            ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^ sepratorInsideCode ^^^
;;
;;      The `How-to-use' is book mark, because it has `&' on it, whilst the
;;      latter isn't -- it is used inside code to make it more readable and
;;      The latter on is not included in *imenu*.
;;
;;  About the book mark identifier naming
;;
;;      When you name the breaks, keep in mind that when identifiers are
;;      sorted, the ones that start with big letters A-Z show up first, a-z
;;      come next. Allthougt it would be convenient to have all subwords in
;;      capital, it is usually better to start with lowercase letter,
;;      because it's easily unintentionally mix up/down case letters.
;;      Besides you have to reah out for shift to have uppercase.
;;
;;          ............. breakName ...         ;prefered, starting low
;;          ............. BreakName ...         ;watch out for mixed case!
;;
;;      It is also adviced that you choose some common beginning for the
;;      identifier, so that they get sorted nicely. If you define variables
;;      at the beginning of file it might be good idea to attach beginning
;;      letter like `v-' for variables before the real identifier name
;;      begins, like:
;;
;;          ............. v-globals ...
;;          ............... v-hooks ...
;;
;;      Of course, we can now use the uppercase letter trick to have them
;;      sorted first in the list, just change `v-' to `V-'. Generally
;;      you should think which ones do you use most, do you leave the
;;      variables alone when you have defined them and mostly work with new
;;      functions? Then the variables can stay at the end of list and
;;      there is no need for `V-' trick. but if you need to access
;;      variables often, then you might want to see variables first in the
;;      list. It's up to your decision how you name the variables and how
;;      you want to see them listed.
;;
;;  Breaks and sub-break naming
;;
;;      If you have very large file, you'll probably need major breaks,
;;      level one breaks and possibly level 2 breaks too. To keep the list
;;      well sorted, put the functions into bigger groups and name the
;;      sub-level breaks so that they have some common beginning in respect
;;      to the major break they belong to. Let's see an example where
;;      you're dealing with mail handling. Notice the CAPITAL letter.
;;
;;          ;; ################################# &h-Header ###
;;          ;;  this is beginning block of header handling
;;
;;          ;; ..................................... &h-cc ...
;;          ;;  Some special function here to handle CC
;;          ;;  field, killing all recipients, or only
;;          ;;  some of them
;;
;;          ;; .. .. . .. . .. . .. . .. . .. . .. . .. . .. .
;;          ;;  More detailed functions under h-cc, Not
;;          ;;  named, because there is only 2 function
;;
;;      Again there are couple of points to follow here. All the tricks are
;;      discussed already: the `Big' letter trick put's major break to the
;;      top of imenu list, common beginning keeps the subsections together.
;;
;;  Example breaks
;;
;;      Some book mark breaks are proposed here, but you can use whatever you
;;      like. Thumb of rule: be consistent, always use same convention in
;;      your files and consider the "level of noisiness" of your breaks, so that
;;      they build up nicely and the code is easy to read. Too many
;;      _different_ breaks is not good idea, because they clutter the view
;;      fast, instead use variations on a theme: same break character but
;;      varying spaces and continuous character lengths.
;;
;;      Thumb rule: select 1-3 break chars, and never change them in you
;;      files; your files look alike. Vary the spacing, not the break
;;      characters.
;;
;;      These are 'noisy breaks' , Major section separators, pick only one
;;      and use it in your files, do not use all three!
;;
;;          ##############################################################
;;          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;          ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;
;;      less noisy breaks
;;
;;          .`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`.`
;;
;;          .^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^.^
;;
;;          .:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:.:
;;          .~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~
;;
;;
;;      This is sub section break
;;
;;          ................................................................
;;
;;
;;      This is even lighter subsection break (varying spacing)
;;
;;          ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...
;;
;;      'Draw one's attention' break: something special in this section
;;
;;
;;          --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++--
;;
;;      Internal break 1, inside function, long case statement etc.
;;
;;          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;;
;;      Internal break 2, to separate long case elements etc.
;;
;;
;;          ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^
;;
;;     Book Mark Cache
;;
;;      So that imenu works fast, it is not desirable that the breaks are
;;      always parsed from scratch, because it takes time to scan the file
;;      for possible book marks. That's why the information is cached. If
;;      the break cache is empty, the breaks are gathered from buffer and
;;      stored to the cache and when you call the imenu, the cache is
;;      offered to it --> fast response time. When you add new breaks to
;;      the buffer [especially at the beginning of code development], you
;;      may want to call function `tinybookmark-parse' which will empty the
;;      cache and re-read all book marks. If you write lot of code the
;;      points that were cached do no longer represent exact points of book
;;      marks, because they have been sliding off their places. If you want
;;      *always* have updated book mark points, there is variable
;;      `tinybookmark-cache-update' which you can set to 'always, if you
;;      want the cache to be updated always prior showing X-menu. In large
;;      buffer this remarkably slows down the menu appering. See variable
;;      for more choices.
;;
;;  Automatic book mark detection
;;
;;      In order book marks to be detected in file, you may define following
;;      RCS identifier [see ident(1)] preferably at the beginning of your
;;      file:
;;
;;          $BookMarkRegexp:<space>'REGEXP'<space>$
;;
;;      Be careful so that the identifier is _exactly_ in this form: pay
;;      attention to spaces and (') around the REGEXP. The regular
;;      expression tells what line can be considered as book mark and the
;;      book mark name is indicated in subexpression 1 [\\(.*\\)] , look at
;;      this file, how it is constructed. In order to find all book marks
;;      and build up the cache, it needs to widen the buffer in case the
;;      file is narrowed with some folding or outline editor. When the
;;      cache has been built the buffer's narrowing is restored, so you
;;      shouldn't even notice this. Of course you don't want to find book
;;      marks from your RMAIL file.
;;
;;      One word about the regexp construction, let's see regexp that
;;      matches the identifier:
;;
;;          &+\\([^ ]+\\)
;;
;;      Pay attention to using exclusive regexp, not just '.*'
;;      construction. When you use folding or outline editor the '.*' form
;;      is very ill behaving, because if the line being scanned is
;;      currently folded, IT WILL MATCH WHOLE folded section --> your
;;      identifier surely isn't that one. We can't unfold the sections
;;      during scanning, because if there are subfolds, what editor is on
;;      use .. it's too complex/slow to handle such situations. But using
;;      the exclusive list [^ ] will surely match the identifier, because
;;      it stops when it can find first space. This means that you can't
;;      use _spaces_ inside the identifiers. Cat the words together.
;;
;;  If the BookMarkRegexp isn't defined in file
;;
;;      Then the programs tries to search for the default book marks.
;;      See function `tinybookmark-regexp-default' for more.
;;
;; Message: Empty cache. Building...
;;
;;      Do you wonder why you get this message displayed, while you were
;;      sure that you the buffer had cache already? Don't be surprised. This
;;      is totally normal behavior: whenever you switch mode for the
;;      buffer the new mode _kills_ all local variables, including cache
;;      information. Obviously the information must be restored when you
;;      call the hot list again. The cache could have been programmed to be
;;      buffer local, but in the present format only one cache s active at
;;      the time. This was simpler to implement and manage in the code.
;;
;;  About imenu
;;
;;      You definitely want to look at the documentation of imenu to find
;;      many more usages for it. It makes your day shine in X-display. You
;;      should also configure few variables for it, like:
;;
;;          (setq imenu-max-items 20)
;;
;;  Test run
;;
;;      Load this file and set those key bindings mentioned. Hit the mouse
;;      bindings and you're running book mark package. Since the break
;;      marks are used in commentary also, the list of book marks are not
;;      in their most informative form, I use following convention to name
;;      book marks;
;;
;;          'v-'     variable topic
;;          't-'     text topic
;;
;;  Design thoughts
;;
;;      Sooner or later someone wonders: "Can't we have sub-breaks listed
;;      nicely with indentation in front lines in X-popup?" Present answer
;;      "No", since it would require keeping track of the 'Main break' and
;;      then seeing if there exist sub-breaks. Immediately this leads to
;;      question "What is the main break?", and if we say main breaks start
;;      with "#|/%" character set we limit the use of breaks. Besides deciding
;;      what are sub-breaks, main-breaks with regexp may be too slow.
;;      The breaks are intended to to give an *overview* of the buffer.
;;      Please use imenu to find single functions if you don't feel like
;;      tapping couple of pgUp/pgDown after the point is positioned in the break
;;      section.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-and-compile
  (autoload 'imenu--mouse-menu          "imenu"    "" t)
  (autoload 'folding-show-current-entry "folding"  "" t))

(ti::package-defgroup-tiny TinyBookmark tinybookmark-- tools
  "Minor mode for writing text in 'Technical text format'.
  Overview of features

        o   Provides some 'setting book marks' functions: adding
            repeated characters and sequences up till end of line with
            named identifier. (like breaks in this file)
        o   Automatically parses book marks from file, if it contains
            RCS identifier 'book markRegexp' which defines book mark syntax for
            the file. Uses X-popup [imenu] for showing those book marks and
            moving between them.")

(defvar tinybookmark--version-time "2010.1119.1452"
  "Last modified time.")

;;}}}
;;{{{ setup: -- hooks

;;; ......................................................... &v-hooks ...

(defcustom tinybookmark--parse-before-hook nil
  "*Hook that is run just before the buffer is scanned for book marks."
  :type  'hook
  :group 'TinyBookmark)

(defcustom tinybookmark--load-hook nil
  "*Hook run when file is loaded."
  :type  'hook
  :group 'TinyBookmark)

;;}}}
;;{{{ setup: user configuration

;;; ........................................................ &v-public ...

(defcustom tinybookmark--cache-update 'threshold
  "*Method when to update cache.

nil         manual update -- you have to call `tinybookmark-parse'
'always     always update cache when menu displayed.
'threshold  update happens when buffer's total character change
            exceeds previous value of `tinybookmark--cache-threshold-val'."
  :type '(choice
          (const nil)
          (const 'always)
          (const 'threshold))
  :group 'TinyBookmark)

(defcustom tinybookmark--cache-threshold-val 100
  "*When cache is constructed, the total character count is saved.
When user adds more code, the total count changes accordingly. If the
difference between current count and last saved count gets bigger than
this value the cache is re-read."
  :type  'integer
  :group 'TinyBookmark)

(defcustom tinybookmark--re-default-chars "[-~+=*%/|#.,'`^]"
  "*Default book mark repeat chars."
  :type  'string
  :group 'TinyBookmark)

(defcustom tinybookmark--max-col '(progn  (tinybookmark-calc-max-col))
  "*Last column to extend the break.
This can be FORM which evaluates to column number"
  :type  'sexp
  :group 'TinyBookmark)

(defcustom tinybookmark--trailer-space-len 3
  "*How much space is left to the right before the book mark ID ends."
  :type  'integer
  :group 'TinyBookmark)

(defcustom tinybookmark--comment-start-func 'tinybookmark-comment-start
  "*Function that return appropriate start comment.
Must return empty string if comment not defined."
  :type  'function
  :group 'TinyBookmark)

(defcustom tinybookmark--comment-end-func 'tinybookmark-comment-end
  "*Function that return appropriate end comment.
Must return empty string if comment not defined."
  :type  'function
  :group 'TinyBookmark)

(defcustom tinybookmark--scan-filter-func 'tinybookmark-scan-filter
  "*Filter out match.
When building up the book marks from file, there may be false hits,
or you may look at special lines only. This function accepts three arguments:
- current line string
- line beginning point
- identifier found from line

If the function return nil the line is not added to the cache."
  :type  'function
  :group 'TinyBookmark)

(defcustom tinybookmark--goto-func 'tinybookmark-goto
  "*Function that handles moving to the point.
If you have folding in effect around that point you may wish
to open it in your custom function.

This function receives one argument: POINT"
  :type  'function
  :group 'TinyBookmark)

(defcustom tinybookmark--insert-strict t
  "*Controls if the book Mark insertion is strict when no argument is given.
See `tinybookmark-insert'"
  :type  'boolean
  :group 'TinyBookmark)

;;}}}
;;{{{ setup: -- private vars

;;; ....................................................... &v-private ...

(defvar tinybookmark--cache nil
  "Private.
Cache where book marks are stored in alist \(bookMarkName . point\)")
(make-variable-buffer-local 'tinybookmark--cache)

;;  We don't want cache to be wiped away when major mode changes
(put 'tinybookmark--cache           'permanent-local t)

(defvar tinybookmark--cache-char-count nil
  "Private. Totals characters in buffer.")
(make-variable-buffer-local 'tinybookmark--cache-char-count)

(defvar tinybookmark--bookmark-regexp nil
  "Private. Hold buffers book mark regexp.")
(make-variable-buffer-local 'tinybookmark--bookmark-regexp)

;;}}}
;;{{{ Macros

;;; ----------------------------------------------------------------------
;;;
(defsubst tinybookmark-regexp-read-from-buffer ()
  "Return buffer's book mark regexp.
If the local value where the regexp is stored is nil, the rescan buffer.

References:
  `tinybookmark--bookmark-regexp'"
  (or tinybookmark--bookmark-regexp     ;changing mode kill local vars
      (setq tinybookmark--bookmark-regexp
            (tinybookmark-search-bm-re))))

;;; ----------------------------------------------------------------------
;;; Default book mark syntax that is used if file does not contain
;;; it's own definition of book mark syntax.
;;;
(defsubst tinybookmark-regexp-default  ()
  "Return default book mark regexp.
References:
  `tinybookmark--re-default-chars'"
  (concat
   tinybookmark--re-default-chars
   tinybookmark--re-default-chars "+"
   " &+\\([^ \t]+\\) "
   tinybookmark--re-default-chars "+"))

;;}}}
;;{{{ movement functions

;;; ........................................................ &movement ...

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-search-regexp ()
  "Return book mark search regexp."
  (concat "^[ \t]*" (or comment-start "") "+ *"
          (tinybookmark-regexp-read-from-buffer)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-backward ()
  "Search book mark line backward."
  (interactive)
  (re-search-backward (tinybookmark-search-regexp) nil t))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-forward (&optional back)
  "Search book mark line forward or optionally BACK."
  (interactive)
  (re-search-forward (tinybookmark-search-regexp) nil t))

;;}}}

;;{{{ miscellaneous functions

;;; ............................................................ &misc ...

;;; ----------------------------------------------------------------------
;;; LISP column
;;;  - I can hear you saying: "Why 74? why not 70 or 75 ?..."
;;;  - Well, I usually add book mark section to my elisp code and while
;;;    I did them by hand I added ';;; ' comment at the beginning of
;;;    line and fed 70  continuous characters with ESC 70 '-'after
;;;    comment  --> totals 4 + 70 chars :-/
;;;
;;;  - The idea of this calculation is that when you hit separator,
;;;    like this: COMMENT-SPACE-70_CHAR_SEPARATOR, this will calculate
;;;    the column so, that when tinybookmark-insert is called, the last
;;;    char lines up with yours.
;;;
;;;    E.g. in shell mode:
;;;
;;;             # ---------------, 70 chars long sep, last col is 2 + 70
;;;             # ..............., tinybookmark-insert lines up to col 72
;;;
;;;    But in lisp
;;;
;;;             ;;; -------------, again 70 chars long sep, 4 + 70
;;;             ;;; ............., tinybookmark-insert lines up to col 74
;;;
;;;    Now you can hit 70 line separator in any mode and to be sure the
;;;    tinybookmark-insert lines up with you.
;;;
(defun tinybookmark-calc-max-col ()
  "Calculates column for mode."
  (let ((mode (symbol-name major-mode))
	(cs   (or comment-start ""))
	(len  70))            ; like "# " + 70 chars
    (cond
     ((string-match "lisp" mode)
      74)
     (t
      (if (string-match "[ \t]+" cs)  ;does it already have space "# "
          (+ len (length cs)) ;no it does not "#", add room for it.
        (1+ (+ len (length cs))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-goto (point)
  "Go to the selected POINT."
  (let ((re ".*{{{"))
    (cond
     ((and (featurep 'folding)
           (symbol-value 'folding-mode))
      (goto-char point)
      (save-excursion
        (beginning-of-line)
        (if (looking-at re)
            (folding-show-current-entry))))
     (t
      (goto-char point)))))

;;; ----------------------------------------------------------------------
;;; - include all lines
;;;
(defun tinybookmark-scan-filter (full-line pos id)
  "Return always t, so all matched lines are cached.
Ignore FULL-LINE POS ID."
  t)

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-comment-end ()
  "Return appropriate comment end, according to mode."
  (let ((str (or comment-end "")))
    (unless (equal "" str)
      (setq str (ti::string-add-space str)))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-comment-start ()
  "Return appropriate comment, according to mode."
  (let ((str (or comment-start "")))   ;comment
    ;;   Lisp is special, due to it's comment usage
    (when (memq major-mode  '(lisp-mode emacs-lisp-mode lisp-interaction-mode))
      (if (bolp)
          (setq str ";;;")
        (setq str ";;")))
    (unless (equal "" str)
      (setq str (ti::string-add-space str t)))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-cache-update ()
  "Determines when and how to update cache.
References: `tinybookmark-cache-update'"
  (let* ((mode       tinybookmark--cache-update)
         (end        (marker-position (point-max-marker)))
         (cache-end  (or tinybookmark--cache-char-count
			 end))
         (limit      tinybookmark--cache-threshold-val)
         diff)
    (cond
     ((eq mode nil)
      nil)                              ;manual
     ((eq mode 'always)
      (tinybookmark-parse))
     ((eq mode 'threshold)
      (setq diff (abs (- end cache-end)))
      (if (< diff limit) nil
        ;; Hmm, should we print a message "threshold exceeded, reparsing..?"
        ;; Let's be transparent this time: no messages.
        (tinybookmark-parse))))))

;;}}}
;;{{{ book mark line insert

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-repeat (str count &optional col strict)
  "Repeats character or string sequence STR COUNT times.

COUNT can be:

  0       repeat until position 79 or COL , or if the STR is not single
          character, until fits below COL
  \"\"    interactive insert, as long as user presses RET or SPACE.

STRICT has effect only if COL is given:

  nil     insert as long as STR fits below COL
  t       insert strictly up till COL and cut away portion
          of STR if necessary"

  (interactive "sString:\nsCount[0=eol]: ")
  (let* ((swide (or col 79))            ;screen width
         (i     0)
         (ok    t)
         ch
         c
         len
         p)
    (unless (or (not (stringp str))         ;it's not string
		(eq 0 (length str)))        ;string is empty
      (cond
       ((stringp count)
	(if (equal "" count)
	    (setq c -1)                   ;interactive
	  (setq c (string-to-number count))))
       ((numberp count)
	(setq c count))
       (t
	(error "Invalid count arg %s" count)))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
      (cond
       ((eq c -1)                        ;ask confirmation every time...
	(while ok
	  (message "insert? <spc,enter> ")
	  (setq ch (read-char))
	  (cond
	   ((or (char-equal ch ?\C-m )
		(char-equal ch ?\ ))
	    (insert str))
	   (t (setq ok nil))))
	(message ""))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq c 0)
	(setq len         (length str)
	      p           (current-column))
	;; we have to remove tabs from this line to get count right
	(untabify (line-beginning-position) (line-end-position))
	(move-to-column p)                ;restore position
	;; the added str must not move point further than COL
	(while (<= (+ len (current-column)) swide)
	  (insert str))
	;;   Check if it was multicharacter and we didn't get to last position
	;;   Insert the last string and cut after COL
	(if (null strict)
	    nil
	  (if (= (current-column) swide)
	      nil
	    (insert str)
	    (ti::buffer-move-to-col swide)
	    (delete-region (point)
			   (progn
			     (end-of-line)
			     (point))))))
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       (t                                 ;straight number !
	(while (< i c)
	  (insert str) (setq i (1+ i))))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-insert (txt sep &optional strict)
  "Add book mark until the end of line.
Normally line is filled as long as the pattern fits below max column,
but if the optional argument is given, it will be filled in full ,
truncating if necessary. To see an example, try with some long
pattern.

Input:

  TXT       book mark name
  SEP       separator string that is repeated.
  STRICT
            0       strict is nil in spite of `tinybookmark--insert-strict'
            1       strict is t   in spite of `tinybookmark--insert-strict'
            nil     use default value in `tinybookmark--insert-strict'

References:

  `tinybookmark--insert-strict'"
  (interactive "sBookmark: \nsSep: \nP")
  (let* ((strict-def  tinybookmark--insert-strict)
         (cs-func     tinybookmark--comment-start-func)
         (ce-func     tinybookmark--comment-end-func)
         (line-col    (eval tinybookmark--max-col))
         (trail-len   tinybookmark--trailer-space-len) ;how much to leave
         (bolp        (line-beginning-position))
         (cur-col     (current-column))
         (orig-point  (+ bolp cur-col))
         col
         cs
         ce)
    (cond
     ((eq nil strict)                   ;use default
      (setq strict strict-def))
     ((eq 0 strict)
      (setq strict nil))
     ((eq 1 strict)
      (setq strict t))) ;; cond end
    (if (= 0 (length sep))
        (error "No separator"))
    ;;  Add surrounding spaces if they are not included
    (unless (equal "" txt)
      (setq txt (ti::string-add-space txt)
            txt (ti::string-add-space txt t)))
    (setq cs (funcall cs-func)          ;Set comments
          ce (funcall ce-func))
    ;;  I tried to turn overwrite-mode on, but SUPRISE!
    ;;  - While it was on, and I tried to do 'insert',
    ;;    it didn't _overwrite_; emacs why can't you behave as expected ?
    ;;  - So I have to hack with delete-region instead.
    ;;  - skip lenght of comment start
    (ti::buffer-move-to-col (+ cur-col (length cs)))
    ;; We must clear the rest of line
    (unless (looking-at "$")
      (delete-region (point) (line-end-position)))
    ;;  - draw the line until max col
    (setq col line-col)                 ;maximum repeat column
    (tinybookmark-repeat sep 0 col strict) ;insert full separator
    ;;  - Now cut room for identifier
    (backward-char (+ (length txt) trail-len)) ;leave trailer space
    (delete-region (point) (+ (point) (length txt)))
    ;;  - write the identifier
    (insert txt)
    (end-of-line)
    (insert ce)                         ;comment end
    ;;  - delete spaces at front and replace it with comment start
    (goto-char orig-point)
    (delete-region (point) (+ (point) (length cs)))
    (insert cs)
    (goto-char orig-point)))

;;}}}
;;{{{ Book Mark find, caching

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-scan (re)
  "Gather all book marks from current point forward using RE.
Return list: (id . beginning-of-line-point).

References:
  `tinybookmark--scan-filter-func'"
  (let ((func tinybookmark--scan-filter-func) ;should we filter something ?
	id
	point
	list)
    (while (re-search-forward re nil t)
      (when (setq id (match-string 1))
        (setq point (line-beginning-position))
        ;;  Is this line allowed to add ?
        (if (funcall func (ti::read-current-line) id point)
            ;;  Nothing magic in this expression, just build list
            ;;  '((id point) (id .point) ..)
            (ti::nconc list (cons id point)))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-search-bm-re ()
  "Search buffer for automatic book mark identifier 'BookMarkRegexp'.
Returns regexp defined in it. if is doesn't exist returns default
book mark regexp."
  (let ((id          "BookMarkRegexp")
	(re-default  (tinybookmark-regexp-default))
	id-val
	fixed-val
	ret)
    (setq id-val (ti::vc-rcs-str-find-buffer id t))
    ;;  while reading from buffer the \ doubles, convert it back to \
    (setq fixed-val (ti::string-plain-string-to-regexp id-val))
    (if (or  (null fixed-val)
             ;;  We must find the '' marks
             (not (string-match "'\\(.+\\)'" fixed-val)))
        (setq ret re-default)
      (setq ret (match-string 1 fixed-val)))
    ret))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-parse ()
  "Build book mark list and save it to cache.

Return:

  t     cache was built.
  nil   book marks not found or error happened. Cache untouched."
  (interactive)
  (let ((op             (point))       ;user's original point
	(beg            (point-min-marker))
	(end            (point-max-marker))
	(end-pos        (marker-position (point-max-marker)))
	(end-max        (point-max))
	end-wmax
	re
	ret
	list)
    (run-hooks 'tinybookmark--parse-before-hook)
    (setq tinybookmark--cache-char-count end-pos) ;Set GLOBAL
    (if (null (setq re (tinybookmark-regexp-read-from-buffer)))
        (message "TinyBookmark: No book mark syntax Identifier found.")
      (unwind-protect                   ;handle narrowed buffers too
          (progn
            (widen)
            (setq end-wmax (point-max))
            (ti::pmin)
            (setq list (tinybookmark-scan re))
            (if (null list)
                (message "TinyBookmark: No book marks found.")
              (setq ret t)
              (setq tinybookmark--cache list)))
        (save-excursion
          (with-current-buffer (marker-buffer beg)
	    ;; what about after widen ? Were we in narrow mode ?
	    (unless (= end-wmax end-max)
	      (narrow-to-region beg end))))))
    ;; only reasonable way to return to current point
    (goto-char op)
    ret))

;;}}}
;;{{{ mouse

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-keyboard-parse ()
  "Reparse book marks."
  (tinybookmark-mouse-parse nil (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-mouse-parse (&optional event verb)
  "Reparse book mark list. This function is called from mouse binding.
Called with mouse EVENT. VERB displays message."
  (interactive "e")
  (ti::verb)
  (if (and verb (tinybookmark-parse))
      (message "TinyBookmark: Book Marks cached.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-selection (event)
  "Display cache menu. Called with mouse EVENT."
  (interactive "e")
  (let ((go-func   tinybookmark--goto-func)
	cache
	data)
    (tinybookmark-cache-update)
    (setq cache     tinybookmark--cache)
    (if (null cache)
        (message "TinyBookmark: No book marks found.")
      (cond
       ((ti::compat-window-system)
        (setq data (imenu--mouse-menu cache event)))
       (t
        (setq data (completing-read "Select: " cache))
        (setq data (assoc data cache))))
      (if data
          (funcall go-func (cdr data))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinybookmark-cache-regenerate (&optional force)
  "Regenerate cache if needed. Optional FORCE."
  (let ((cache-ok tinybookmark--cache))
    (when (or (null cache-ok)
              force)
      (message "TinyBookmark: building cache...")
      (sleep-for 1)
      (message "")
      (tinybookmark-parse))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-keyboard (bookmark &optional arg)
  "Complete and jump to bookmarks.
Optional ARG rebuilds cache."
  (interactive
   (progn
     (if current-prefix-arg
         (tinybookmark-cache-regenerate t))
     (let* ((ans (completing-read "TinyBookmark: "
                                  tinybookmark--cache nil 'match)))
       (list ans
             current-prefix-arg))))
  (unless (interactive-p)
    (tinybookmark-cache-regenerate arg))
  (let* ((elt (assoc bookmark tinybookmark--cache)))
    (if (not elt)
        (message
         (substitute-command-keys
          (concat
           "TinyBookmark: ERROR, rebuild with "
           "\\[universal-argument] \\[tinybookmark-keyboard]")))
      (goto-char (cdr elt)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinybookmark-mouse (event &optional arg)
  "Display book mark pop up menu. Use mouse EVENT.
Optional ARG rebuilds cache."
  (interactive "e\nP")
  (tinybookmark-cache-regenerate arg)
  (tinybookmark-selection event))

;;}}}

(provide   'tinybookmark)
(run-hooks 'tinybookmark--load-hook)

;;; tinybookmark.el ends here
