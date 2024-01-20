;; -*- enable-local-variables: :all;  -*-

;;; tinybookmark.el --- Maintain files in organized sections

;; This file is not part of Emacs

;; Copyright (C)    1995-2024 Jari Aalto
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
;;      M-x tinybookmark-insert  to insert a bookmark
;;
;;      ;; This is for windowed Emacs, which brings up a menu.
;;      ;; In XEmacs, instead of `mouse-1', use event `mouse1down'.
;;
;;      (global-set-key [(?\e) (control mouse-1)]        'tinybookmark-mouse)
;;      (global-set-key [(?\e) (control shift mouse-1)]  'tinybookmark-mouse-parse)
;;
;;      ;; To use the keyboard for navigating between bookmarks:
;;
;;      (global-set-key [(shift left)]  'tinybookmark-backward)
;;      (global-set-key [(shift right)] 'tinybookmark-forward)
;;
;;      ;; To navigate with menu completion:
;;
;;      (global-set-key [(control shift right)] 'tinybookmark-keyboard)
;;
;; NOTES
;;
;;      The variables comment-start' and comment-end' must be defined;
;;      otherwise, the inserted text won't have the proper prefix and
;;      ending.

;; .................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Feb 1995
;;
;;      Long ago, I used a small function I wrote to insert section
;;      breaks, what I call 'book' 'marks.' Additionally, I used
;;      `folding.el' to organize code into separate sections, making
;;      it easy to find things by searching either bookmarks or
;;      jumping between folds. Later, `imenu.el' was introduced,
;;      providing a convenient X-pop-up for bookmarks, and adding
;;      support for it marked the beginning of this package.
;;
;;  An overview of features
;;
;;      o   Provides inserting bookmarks by adding repeated characters
;;          and sequences up until the end of the line, identified by
;;          a specified name.
;;
;;      o   Automatically parses bookmarks from files containing the
;;          identifier `bookMarkRegexp.' This defines the bookmark
;;          syntax for the file and utilizes `imenu' to display and
;;          navigate between these bookmarks.
;;
;;  How to keep files organized
;;
;;      There are several tools to keep your code organized, and they
;;      work best when you consider how they can cooperate. Tools like
;;      `folding.el' and `tinybookmark.el' may seem to perform a
;;      similar task, dividing the code into more manageable sections.
;;      The crucial distinction is that when folding is used, one
;;      works within a specific section and may want to hide the rest
;;      of the code. On the other hand, when easily navigating back
;;      and forth in the buffer, it is unfolded, and TinyBookmark
;;      comes into play. To add a bit more confusion, there's also
;;      `imenu.el', which can be used to jump inside code. It can be
;;      configured to pick up all function names inside a list,
;;      allowing you to easily navigate to a specific function by
;;      selecting it from imenu.
;;
;;      To summarize:
;;
;;      o   folding.el      - For organizing sections in file.
;;                            Provides a clear view of the structure.
;;      o   tinybookmark.el - Jump between/finding  large  sections
;;      o   imenu.el        - Finding specific functions
;;      o   tinyhotlist.el  - Add or remove files from permanent list
;;
;;  How to use this package
;;
;;      The following function inserts book mark on the current line
;;
;;          M-x tinybookmark-insert
;;
;;     The following function inserts repeated pattern:
;;
;;          M-x tinybookmark-repeat
;;
;;      The default bookmark character is the dot (.), which isn't as
;;      'noisy' as a continuous '-' line. Normally, you add some
;;      unused ID character, like '&' at the front of the real
;;      bookmark, like this:
;;
;;          ;;; .................................. &How-to-use ...
;;          (defun test ()
;;           (progn
;;            ..
;;            (goto-char ..
;;            ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^ sepratorInsideCode ^^^
;;
;;      The 'How-to-use' is a bookmark because it has '&' on it, while
;;      the latter isn't. Only bookmarks with '&' are included in
;;      `imenu'.
;;
;;  About the book mark identifier naming
;;
;;      When you name the breaks, keep in mind that when identifiers
;;      are sorted, those starting with big letters `A-Z' show up first,
;;      while those starting with a-z come next. Although it might be
;;      convenient to have all subwords in capital letters, it is
;;      usually better to start with a lowercase letter to avoid
;;      unintentional mix-ups with case sensitivity. Besides, you have
;;      to reach out for the shift key to use uppercase.
;;
;;          ............. breakName ...         ;prefered, starting low
;;          ............. BreakName ...         ;watch out for mixed case!
;;
;;      It is also advised that you choose a common beginning for the
;;      identifier so that they get sorted nicely. For example, if you
;;      define variables at the beginning of a file, it might be a
;;      good idea to attach a prefix like 'v-' for variables before
;;      the real identifier name begins, like:
;;
;;          ............. v-globals ...
;;          ............... v-hooks ...
;;
;;      Of course, we can now use the uppercase letter trick to have
;;      them sorted first in the list—just change v-' to V-'.
;;      Generally, you should consider which ones you use most. Do you
;;      leave the variables alone once defined and mostly work with
;;      new functions? In that case, the variables can stay at the end
;;      of the list, and there is no need for the `V-' trick. However,
;;      if you need to access variables often, you might want to see
;;      variables listed first. It's up to your decision how you name
;;      the variables and how you want to see them listed.
;;
;;  Break and sub-break naming
;;
;;      If you have a very large file, you'll probably need major
;;      breaks, level one breaks, and possibly level two breaks too.
;;      To keep the list well-sorted, put the functions into bigger
;;      groups and name the sub-level breaks so that they have some
;;      common beginning in respect to the major break they belong to.
;;      Let's see an example where you're dealing with mail handling.
;;      Notice the CAPITAL letter.
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
;;      There are a couple of points to follow here. All the tricks
;;      have been discussed already: the 'Capital' letter trick puts
;;      major breaks at the top of the imenu list, and having a common
;;      beginning keeps the subsections together.
;;
;;  Break examples
;;
;;      Some bookmark breaks are proposed here, but you can use
;;      whatever you like. A rule of thumb: be consistent, always use
;;      the same convention in your files, and consider the 'level of
;;      noisiness' of your breaks so that they build up nicely and the
;;      code is easy to read. Having too many different breaks is not
;;      a good idea, as they clutter the view quickly. Instead, use
;;      variations on a theme—use the same break character but vary
;;      spaces and continuous character lengths.
;;
;;      Thumb rule: select 1-3 break characters and never change them
;;      in your files; this ensures your files look alike. Vary the
;;      spacing, not the break characters.
;;
;;      These are 'noisy breaks,' major section separators. Pick only
;;      one and use it in your files; do not use all three.
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
;;      This could be a sub section break
;;
;;          ................................................................
;;
;;      This is an even lighter subsection break (varying spacing)
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
;;          ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^  ^^^
;;
;;     Book Mark Cache
;;
;;      So that imenu works fast, it is not desirable for the breaks
;;      to be parsed from scratch every time, as it takes time to scan
;;      the file for possible bookmarks. That's why the information is
;;      cached. If the break cache is empty, the breaks are gathered
;;      from the buffer and stored in the cache. When you call imenu,
;;      the cache is offered to it, resulting in a fast response time.
;;      If you add new breaks to the buffer, especially at the
;;      beginning of code development, you may want to call the
;;      function `tinybookmark-parse,' which will empty the cache and
;;      re-read all bookmarks. As you write a lot of code, the points
;;      that were cached no longer represent the exact positions of
;;      bookmarks, as they have been sliding off their places. If you
;;      want to *always* have updated bookmark points, there is the
;;      variable `tinybookmark-cache-update,' which you can set to
;;      'always' if you want the cache to be updated always prior to
;;      showing the X-menu. Note that in large buffers, this
;;      noticeably slows down the appearance of the menu. See the
;;      variable for more choices.
;;
;;  Automatic book mark detection
;;
;;      In order for bookmarks to be detected in a file, you may
;;      define the following Version Control software rcs(1)'s RCS
;;      identifier. See the manual page of ident(1) for more
;;      information. The following would preferably be at the
;;      beginning of the file:
;;
;;          $BookMarkRegexp:<space>'REGEXP'<space>$
;;
;;      Be careful that the identifier is in this form; pay attention
;;      to spaces and (') around the REGEXP. The regular expression
;;      tells what line can be considered a bookmark, and the bookmark
;;      name is indicated in subexpression 1 [\(.*\)]. Look at this
;;      file to see how it is constructed. In order to find all
;;      bookmarks and build up the cache, it needs to widen the buffer
;;      in case the file is narrowed with some folding or outline
;;      editor. Once the cache has been built, the buffer's narrowing
;;      is restored, so you shouldn't even notice this. Of course, you
;;      don't want to find bookmarks from your Emacs RMAIL file.
;;
;;      A brief note about the regexp construction: let's examine the
;;      regexp that matches the identifier:
;;
;;          &+\\([^ ]+\\)
;;
;;      Pay attention to using an exclusive regexp, not just the '.'
;;      construction. When you use folding or an outline editor, the
;;      '.' form behaves poorly. If the line being scanned is
;;      currently folded, it will match the whole folded section, and
;;      your identifier surely isn't that one. We can't unfold the
;;      sections during scanning, because if there are subfolds,
;;      determining which editor is in use becomes too complex/slow to
;;      handle such situations. However, using the exclusive list [^ ]
;;      will surely match the identifier because it stops when it
;;      encounters the first space. This means that you can't use
;;      spaces inside the identifiers; instead, concatenate the words
;;      together.
;;
;;  If the BookMarkRegexp isn't defined in file
;;
;;      Program tries to search for the default book marks.
;;      See function `tinybookmark-regexp-default' for more.
;;
;; Message: Empty cache. Building...
;;
;;      Do you wonder why you get this message displayed when you were
;;      sure that the buffer had a cache already? Don't be surprised;
;;      this is totally normal behavior. Whenever you switch modes for
;;      the buffer, the new mode kills all local variables, including
;;      cache information. Obviously, the information must be restored
;;      when you call the hot list again. The cache could have been
;;      programmed to be buffer local, but in the present format, only
;;      one cache is active at a time. This was simpler to implement
;;      and manage in the code.
;;
;;  About imenu
;;
;;      Take a look at the documentation of `imenu.el' to find more
;;      usages for it. It makes your day shine on an X-display.
;;      Additionally, you should configure a few variables for it,
;;      like:
;;
;;          (setq imenu-max-items 20)
;;
;;  Test run
;;
;;      Load this file and set the mentioned key bindings. Activate
;;      the mouse bindings, and you're ready to use the bookmark
;;      package. Since the break marks are used in commentary as well,
;;      the list of bookmarks may not be in its most informative form.
;;      I use the following convention to name bookmarks:
;;
;;          'v-'     variable topic
;;          't-'     text topic
;;
;;  Design thoughts
;;
;;      Let's ponder: 'Can't we have sub-breaks listed nicely
;;      with indentation in front lines in the menu?'
;;
;;      The present answer is `No', since it would require keeping
;;      track of the main break and then checking if there exist
;;      sub-breaks. Immediately, this leads to the question 'What is
;;      the main break?' If we say main breaks start with the '#|/%'
;;      character set, we limit the use of breaks. Besides deciding
;;      what are sub-breaks, main breaks with regexp may be too slow.
;;
;;      The breaks are intended to give an overview of the buffer.
;;      Please use `imenu.el' to find single functions if you don't
;;      feel like tapping a couple of pgUp/pgDown keys after the point
;;      is positioned in the break section.

;;; Change Log:

;;; Code:

;;;; Setup: require

;; ......................................................... &require ...

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

(defvar tinybookmark--version-time "2024.0120.1306"
  "Last modified time.")

;;; Setup: hooks

;; ......................................................... &v-hooks ...

(defcustom tinybookmark--parse-before-hook nil
  "*Hook that is run just before the buffer is scanned for book marks."
  :type  'hook
  :group 'TinyBookmark)

(defcustom tinybookmark--load-hook nil
  "*Hook run when file is loaded."
  :type  'hook
  :group 'TinyBookmark)

;;; Setup: user configuration

;; ........................................................ &v-public ...

(defcustom tinybookmark--cache-update 'threshold
  "*Method when to update cache.

nil         Manual update -- you have to call `tinybookmark-parse'
\\='always     Update happens when menu is displayed.
\\='threshold  Update happens when buffer's character count
            exceeds previous value of `tinybookmark--cache-threshold-val'."
  :type '(choice
          (const nil)
          (const always)
          (const threshold))
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

;;; Setup: -- private vars

;; ....................................................... &v-private ...

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

;;; Macros

(defsubst tinybookmark-regexp-read-from-buffer ()
  "Return buffer's book mark regexp.
If the local value where the regexp is stored is nil, the rescan buffer.

References:
  `tinybookmark--bookmark-regexp'"
  (or tinybookmark--bookmark-regexp     ;changing mode kill local vars
      (setq tinybookmark--bookmark-regexp
            (tinybookmark-search-bm-re))))

;; Default book mark syntax that is used if file does not contain
;; it's own definition of book mark syntax.
(defsubst tinybookmark-regexp-default  ()
  "Return default book mark regexp.
References:
  `tinybookmark--re-default-chars'"
  (concat
   tinybookmark--re-default-chars
   tinybookmark--re-default-chars "+"
   " &+\\([^ \t]+\\) "
   tinybookmark--re-default-chars "+"))

;;; Movement functions

;; ........................................................ &movement ...

(defun tinybookmark-search-regexp ()
  "Return book mark search regexp."
  (concat "^[ \t]*" (or comment-start "") "+ *"
          (tinybookmark-regexp-read-from-buffer)))

;;;###autoload
(defun tinybookmark-backward ()
  "Search book mark line backward."
  (interactive)
  (re-search-backward (tinybookmark-search-regexp) nil t))

;;;###autoload
(defun tinybookmark-forward (&optional back)
  "Search book mark line forward or optionally BACK."
  (interactive)
  (re-search-forward (tinybookmark-search-regexp) nil t))

;;; Miscellaneous functions

;; ............................................................ &misc ...

;; LISP column
;;  - I can hear you saying: "Why 74? why not 70 or 75 ?..."
;;  - Well, I usually add book mark section to my elisp code and while
;;    I did them by hand I added ';;; ' comment at the beginning of
;;    line and fed 70  continuous characters with ESC 70 '-'after
;;    comment this becomes total of 4 + 70 characters.
;;  - The idea of this calculation is that when you hit separator,
;;    like this: COMMENT-SPACE-70_CHAR_SEPARATOR, this will calculate
;;    the column so, that when tinybookmark-insert is called, the last
;;    char lines up with yours.
;;    E.g. in shell mode:
;;             # ---------------, 70 chars long sep, last col is 2 + 70
;;             # ..............., tinybookmark-insert lines up to col 72
;;    But in lisp
;;             ;;; -------------, again 70 chars long sep, 4 + 70
;;             ;;; ............., tinybookmark-insert lines up to col 74
;;    Now you can hit 70 line separator in any mode and to be sure the
;;    tinybookmark-insert lines up with you.
(defun tinybookmark-calc-max-col ()
  "Calculates column for mode."
  (let ((mode (symbol-name major-mode))
        (cs   (or comment-start ""))
        (len  70))            ; like "# " + 70 chars
    (cond
     ((string-match "lisp" mode)
      74)
     (t
      (if (string-match "[ \t]+" cs)  ;; does it already have space "# "
          (+ len (length cs))         ;; no it does not "#", add room for it.
        (1+ (+ len (length cs))))))))

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

;; Include all lines
(defun tinybookmark-scan-filter (full-line pos id)
  "Return always t, so all matched lines are cached.
Ignore FULL-LINE POS ID."
  t)

(defun tinybookmark-comment-end ()
  "Return appropriate comment end, according to mode."
  (let ((str (or comment-end "")))
    (unless (equal "" str)
      (setq str (ti::string-add-space str)))
    str))

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

;;; Book mark line insert

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

;;; Book Mark find, caching

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

(defun tinybookmark-search-bm-re ()
  "Search buffer for automatic book mark identifier `BookMarkRegexp'.
Returns regexp defined in it. if is does not exist returns default
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

;;; Mouse

;;;###autoload
(defun tinybookmark-keyboard-parse ()
  "Reparse book marks."
  (tinybookmark-mouse-parse nil (called-interactively-p 'interactive)))

;;;###autoload
(defun tinybookmark-mouse-parse (&optional event verb)
  "Reparse book mark list. This function is called from mouse binding.
Called with mouse EVENT. VERB displays message."
  (interactive "e")
  (ti::verb)
  (if (and verb (tinybookmark-parse))
      (message "TinyBookmark: Book Marks cached.")))

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

(defun tinybookmark-cache-regenerate (&optional force)
  "Regenerate cache if needed. Optional FORCE."
  (let ((cache-ok tinybookmark--cache))
    (when (or (null cache-ok)
              force)
      (message "TinyBookmark: building cache...")
      (sleep-for 1)
      (message "")
      (tinybookmark-parse))))

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
  (unless (called-interactively-p 'interactive)
    (tinybookmark-cache-regenerate arg))
  (let* ((elt (assoc bookmark tinybookmark--cache)))
    (if (not elt)
        (message
         (substitute-command-keys
          (concat
           "TinyBookmark: ERROR, rebuild with "
           "\\[universal-argument] \\[tinybookmark-keyboard]")))
      (goto-char (cdr elt)))))

;;;###autoload
(defun tinybookmark-mouse (event &optional arg)
  "Display book mark pop up menu. Use mouse EVENT.
Optional ARG rebuilds cache."
  (interactive "e\nP")
  (tinybookmark-cache-regenerate arg)
  (tinybookmark-selection event))

;;; Provide

(provide   'tinybookmark)
(run-hooks 'tinybookmark--load-hook)

;;; tinybookmark.el ends here
