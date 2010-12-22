;;; tinytf.el --- Document layout tool for (T)echnical text (F)ormat

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Keywords:        wp
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinytf-version.
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
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;      ;; OPTIONAL: If you want fast minor mode key binding
;;      (setq tinytf--mode-prefix-key "z")   ;; faster than default C-c C-z
;;
;;      (add-hook 'tinytf--load-hook 'turn-on-tinytf-mode-all-buffers)
;;      (require 'tinytf)
;;
;; or autoload and your Emacs starts faster, preferred method:
;;
;;      (setq tinytf--mode-prefix-key "z")  ; OPTIONAL
;;
;;      (autoload 'tinytf-mode			"tinytf" "" t)
;;      (autoload 'turn-on-tinytf-mode-maybe	"tinytf" "" t)
;;      (autoload 'tinytf-untabify-buffer	"tinytf" "" t)
;;      (autoload 'turn-on-tinytf-mode-maybe	"tinytf" "" t)
;;
;; To use additional function keys, add these lines:
;;
;;      ;; Required
;;      (add-hook 'tinytf--mode-define-keys-hook 'tinytf-mode-define-keys)
;;      ;; OPTIONAL
;;      (add-hook 'tinytf--mode-define-keys-hook 'tinytf-mode-define-f-keys)
;;
;; Additional hooks to detect and format buffer (optional):
;;
;;       (add-hook 'write-file-functions 'tinytf-untabify-buffer)
;;       (add-hook 'find-file-hook 'turn-on-tinytf-mode-maybe)
;;
;; If you need to redefine some binding to suit your keyboard better,
;; add setup like this to your ~/.emacs file:
;;
;;      (setq tinytf--mode-define-keys-hook 'my-tinytf-mode-define-keys)
;;
;;      (defun my-tinytf-mode-define-keys ()
;;        (let ((map tinytf--mode-prefix-map))
;;          (tinytf-mode-define-keys)   ;; Default keys.
;;          (tinytf-mode-define-f-keys) ;; Additional F-keys
;;          (define-key map "]"  'tinytf-mark-word-sample)
;;          (define-key map "["  'ignore)
;;          (define-key map "{"  'tinytf-mark-word-emp)
;;          (define-key map "}"  'tinytf-mark-word-strong)))
;;
;; It is possible to write documentation into other files as well
;; using TF format. There is a perl program that can extract
;; documentation into text/plain by omitting comments:
;;
;;      http://cpan.perl.org/modules/by-authors/id/J/JA/JARIAALTO/
;;      ripdoc.pl
;;
;; For example this lisp file's documentation can be converted into HTML
;; with following command sequence. t2html is available at
;; <http://freshmeat.net/projects/perl-text2html>.
;;
;;      % ripdoc.pl tinytf.el | t2html.pl > tinytf.html
;;
;; If you have any questions use this function to contact maintainer
;;
;;      M-x tinytf-submit-bug-report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, Jan 1997
;;
;;      Late in the 1996 there was a need for a better text file
;;      handling than just plan `text-mode'. I was looking for a
;;      simple tool to generate HTML pages out of text-based
;;      documents. After some researching on the web, I still couldn't
;;      find anything that would have been strictly a text-based
;;      solution. There were many "languages" from which the HTML
;;      could be generated, but really, I didn't want to learn any new
;;      language just for that. I can understand people that write
;;      their documents still using LaTeX, but Win32 Word is much
;;      suitable and more known than any of those exotic formats. The
;;      project started by creating the tool that converted text into
;;      HTML (Perl *t2html.pl*
;;      <http://freshmeat.net/projects/perl-text2html>) and then
;;      writing a Emacs package to help writing the text files. It has
;;      been proven to be really nice combination where flexibility
;;      meets cross-platform demands.
;;
;;  Overview of features
;;
;;      You can use `M-x' `add-change-log-entry-other-window' (C-x 4 a) to
;;      create a standard ChangeLog record for your changes under the
;;      headings.
;;
;;      The text layout you write
;;
;;      o   You write text in rigid format called 'Technical'
;;      o   There are only two heading levels, one at column 0, and
;;          another at column 4. NO OTHER SUB-HEADINGS SUPPORTED.
;;      o   Text is written at column 8, at the first standard tab position.
;;      o   Each column has different meaning how text is interpreted
;;          into HTML with *t2html.pl* perl script.
;;      o   The full 'Technical text format' is described in the
;;          function description of `tinytf-mode'. The most recent description
;;          is always described in the perl program t2html.pl --help
;;      o   The is only a handful, natural, mark up conventions for PLAIN TEXT.
;;          The whole idea was that you do not need to learn any
;;          mark up language, but just write standard looking text, which is
;;          easily managed and edited with any editor. In addition `diff(1)',
;;          `patch(1)' and `cvs(1)' are the most effective tools to keep your
;;          "text" document project in condition and encourage others to
;;          contribute fixes to your text files.
;;
;;      This package
;;
;;      o   Can show text in outline style manner: you can open and close
;;          headings.
;;      o   Provides commands to move among headings easily.
;;      o   Capitalizes heading with one command.
;;      o   Numbers headings with one command.
;;      o   Text is untabified in regular intervals.
;;      o   On-line help in 19.30+. It assist you writing the text
;;          by displaying message in echo area: how the text is interpreted by
;;          t2html.pl program, that is, how the text would look in HTML.
;;      o   Offer functions to mark text with special text MARKERS that
;;          would produce <STRONG> or <EMP> and the like in the HTML.
;;
;;  What is Technical Format?
;;
;;      In short: it is list of text placement and formatting rules.
;;      And you're looking at it right now in this document. This
;;      package offers minor mode for text files to adjusting the
;;      correct layout.
;;
;;	You can convert text file into HTML very easily with the perl
;;      script which is availale separately at
;;      <http://freshmeat.net/projects/perl-text2html>. You do not
;;      need to know a thing about the HTML language itself. It is
;;      much easier to update text files, than deal with HTML itself.
;;      When you have text ready, you just feed it to the `t2html.pl'
;;      perl script and it gives you nicely formatted HTML page.
;;      Writing HTML home pages is different story, because you
;;      usually want to include some graphics, JavaScript, PHP or JSP
;;      in the page. But aputting some text document available in HTML
;;      format is easily made possible with this package.
;;
;;      On the other hand, while you may not be interested in HTML, you
;;      could still consider writing your documents in 'Technical format'
;;      -- with word technical I refer to the layout of the text, which
;;      is very _rigid_. In order to use facilities in this package,
;;      e.g. heading hiding/showing, the headings must be placed in
;;      columns 0 and 4 and the first word must be in *uppercase*.  The
;;      actual text you write starts at column 8.
;;
;;      If you decide write text like this, you become accustomed to the
;;      layout very quickly and it also helps keeping your documents in
;;      the same format.
;;
;;      All in all, this package was primarily designed to help writing
;;      text documents for `t2html.pl' and viewing document in *outline*
;;      styled selective display. Please refer to mode description for
;;      full details of the text layout format.
;;
;;  TF described briefly
;;
;;      Please note, that this section may be slightly out of date.
;;      You should read up to date information from the conversion
;;      program using command `t2html.pl' `--help'.
;;
;;      --//-- TF description start
;;
;;      0123456789 123456789 123456789 123456789 123456789 column numbers
;;
;;      Table of contents
;;
;;              <Do not write any text inside this heading. It will>
;;              <be generated by tinytf.el automatically with M-x tinytf-toc>
;;
;;      Heading 1 starts from left
;;
;;       emphatised text at column 1,2,3
;;
;;
;;          This is heading 2 at column 4, started with big letter
;;
;;              Standard text starts at column 8, you can
;;              *emphatize* text or make it _strong_ and show
;;              variable name like =ThisVariableSample=. notice that
;;              `ThisIsAlsoVariable' and you can even _*nest*_ the mark up.
;;              more txt in this paragraph txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt txt txt txt
;;
;;           Plain but colored text at columns 5, 6
;;
;;             EMPhatised text starts at column 7, Like heading level 3
;;
;;             "Special STRONG EMP text in column 7 starts with double quote"
;;
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;
;;               strong text at columns 9 and 11
;;
;;                Column 10 has quotation text
;;                Column 10 has quotation text
;;                Column 10 has quotation text
;;
;;                  Column 12 is reserved for code examples
;;                  Column 12 is reserved for code examples
;;                  All text here are surrounded by SAMP codes
;;
;;          Heading 2, at column 4 again
;;
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;
;;              o   Bullet 1 txt txt txt txt txt txt txt txt
;;                  ,txt txt txt txt txt txt txt txt
;;
;;                  Notice that previous paragraph ends to P-comma code,
;;                  it tells this paragraph to continue in bullet
;;                  mode, otherwise this column at 12 would be
;;                  interpreted as SAMPLE code.
;;
;;              o   Bullet 2, text starts at column 12
;;              o   Bullet 3. Bullets are advised to keep together
;;              o   Bullet 4. Bullets are advised to keep together
;;
;;              .   This is ordered list nbr 1, text starts at column 12
;;              .   This is ordered list nbr 2
;;              .   This is ordered list nbr 3
;;
;;              .This line uses BR code, notice the DOT-code at beginning
;;              .This line uses BR code
;;              .This line uses BR code
;;
;;             "This is emphatized text starting at column 7"
;;              .And this text is put after the previous line with BR code
;;             "This starts as separate line just below previous one, EM"
;;              .And continues again as usual with BR code
;;
;;              See the document #URL-BASE/document.txt, where #URL-BASE
;;              tag is substituted with -base switch contents.
;;
;;              Make this email address clickable <foo\@site.com>
;;              Do not make this email address clickable -<bar\@site.com>,
;;              because it is only an example and not a real address.
;;              Noticed the minus(-) prefix at the beginning of url?
;;
;;      Heading level 1 again at column 0
;;
;;          Sub heading, column 4
;;
;;              And regular text, column 8
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;              txt txt txt txt txt txt txt txt txt txt txt txt
;;
;;      --//-- TF description end
;;
;;  How do you write text
;;
;;      This package turns on two minor modes: `tinytab-mode', that handles
;;      your TAB key movements and `tinytf-mode', the Technical format
;;      minor mode. If you're uncertain about how the column will be
;;      treated in HTML output, call following function. If you have 19.30+
;;      this is not necessary, see note about post command above.
;;
;;        The examples here use customized 'z' keybinging for fast
;;        access. You can change that if you prefer some other key.
;;        See variable `tinytf--mode-prefix-key'
;;
;;          z RET   tinytf-column-info-display
;;
;;      Normal text you write as usual, but if you want to mark regions
;;      as "quotations" or "code examples" there is appropriate indent
;;      commands
;;
;;          z /     tinytf-indent-region-text
;;          z '     tinytf-indent-region-quote
;;          z ;     tinytf-indent-region-sample
;;          z :     tinytf-indent-region-strong
;;
;;          z t     tinytf-indent-paragraph-text
;;          z a     tinytf-indent-paragraph-text-as-is
;;          z l     tinytf-indent-paragraph-text-and-fill
;;          z q     tinytf-indent-paragraph-quote
;;          z Q     tinytf-indent-paragraph-quote-and-fill
;;          z s     tinytf-indent-paragraph-sample
;;          z 0     tinytf-indent-paragraph-zero
;;
;;      The `tinytf-indent-paragraph-text-as-is' is a bit special, because
;;      it won't fill the text while it moves the paragraph to the text
;;      position. Instead it adds symbolic <BR> codes to the front of every
;;      moved line. In HTML this ensures that the lines will be shown
;;      exactly as you see them. See also BR mark commands.
;;
;;      There is no functions for bullet creation, because you can write
;;      them easily by hand. Use `z' `b' to fill bullet text nicely.
;;      Bullets look like this
;;
;;          o   This is bullet..
;;              and cont'd line here
;;          o   Yet another bullet
;;              and cont'd line here
;;
;;      The `tinytab-mode' will advance your tab key by 4 every time, so
;;      the text in the bullets go to the right column (12). Remember also
;;      to keep the `tinytab-return-key-mode' on, because that continues
;;      lines as they were written above when you press return. See also
;;      bullet conversion command, which reformats previous text that used
;;      dashes(-) to separate bullets.
;;
;;          z b     tinytf-bullet-format
;;
;;  BR marking commands
;;
;;      In Html, the text would normally wrap according to the browser's
;;      page width. But sometimes you may wish to tell exactly that
;;      it shouldn't wrap the lines according to browser. For example
;;      if you want to include a quoted text "as is" from the Usenet
;;      posts to your page, you need o add symbolic BR code to the beginning
;;      of each line. Like including the following quotation
;;
;;          >>Jim has a good point here...
;;          >>I would have expected that the system depends on..
;;          >Yeah; but you hadn't looked at the /usr/adm/today.log
;;          >
;;
;;      In order to add this to your page "as is"; you can do this:
;;      indent it as "Sample" and it will automatically show like that.
;;      But normally you want it to show as quoted text where you refer.
;;      Then you do:
;;
;;          z Q         tinytf-indent-paragraph-quote
;;          z m b       tinytf-mark-br-paragraph
;;
;;      Which will prepend dot-code(.) to the front of every line.
;;      You can also add the *dot-code* by yourself or use following
;;      command
;;
;;          z m B       tinytf-mark-br-line
;;
;;  Heading writing and handling
;;
;;      You can only use 2 heading levels, which normally suffices.  Sorry,
;;      there is no support for more deeper headings. You start headings
;;      with big letters or number at column 0 or 4. Here is some
;;      specialized commands for headings.
;;
;;      This one converts first character of each heading to
;;      uppercase. This fixes mistakenly left lowercase letters.
;;
;;          z f h     tinytf-heading-fix
;;
;;          before command:
;;          heading
;;              heading
;;
;;          after command:
;;          Heading
;;              Heading
;;
;;      You can (re)number heading easily with following command. If
;;      there is no number in the line, one is added to the beginning of
;;      heading. And if you have added new heading somewhere in the
;;      middle of text, just call this function and it renumbers all
;;      headings again. Running command with *prefix* *argument* removes
;;      numbering.
;;
;;          z f 0     tinytf-heading-numbering
;;
;;          before command:
;;          heading
;;              heading
;;          heading
;;              heading
;;
;;          after command:
;;          1.0 heading
;;              1.1 heading
;;          2.0 heading
;;              2.1 heading
;;
;;      One note about renumbering. Some people write heading number so
;;      that they are closed with parenthesis. This style is not
;;      recommended with technical format style and when you do renumber,
;;      those parenthesis will be removed. The parenthesis style is not
;;      supported because the plain number style is more easily parsed
;;      and detected. In addition, the plain number style in headings is
;;      more widely used in the world.
;;
;;          before command:
;;          1.0) heading
;;              1.1) heading
;;
;;          after command:
;;          1.0 heading
;;              1.1 heading
;;
;;
;;  Heading fixing
;;
;;          z f a   `tinytf-fix-all'
;;                  Does lot's of things. Trim away trailing blanks
;;                  from buffer. Untabify buffer.
;;                  Renumber headings if needed. Delete extra whitespace
;;                  around headings.
;;
;;          z f c   `tinytf-heading-fix-case'
;;                  convert current heading to lowercase
;;
;;          z f C   `tinytf-heading-fix-case-all'
;;                  convert all heading to lowercase
;;
;;  About table of contents
;;
;;      When you write text, you don't write the table of contents, but
;;      the headings. Be sure to add heading "Table of contents" somewhere
;;      in the document. To generate table of contents use commands:
;;
;;          z T             tinytf-toc, Try with C-u argument too
;;          z mouse-1       tinytf-toc-mouse
;;
;;  Selective display
;;
;;      The hiding and showing of the headings and their text is done by
;;      using the outline/folding style display. There is no magic in this;
;;      but there is two interesting commands that you can use in any
;;      selective display buffer.
;;
;;          z C-p     Prints selective display (what you actually see)
;;          z C-c     Copy selective display
;;
;;  Word about key definitions when mode is turned on
;;
;;      When the minor mode is active it overrides some commonly used
;;      key bindings and moves their original function under Control key.
;;      For example:
;;
;;          original PgUp    --> Now Control-PgUp
;;          original PgDown  --> Now Control-PgDown
;;
;;          PgUp             --> Moving heading backward
;;          DownUp           --> Moving heading forward
;;
;;      If you are using X environment or your emacs recognizes mouse,
;;      then there is one handy binding that opens or closes heading
;;      levels when you click over them.
;;
;;          [mouse-3]       tinytf-mouse-context-sensitive
;;
;;      If you press this mouse button anywhere else than over the
;;      headings, it'll call original binding. This feature is similar as
;;      what is used in folding.el
;;
;;  Technical note: about the outline layout
;;
;;      Speed was my primary goal when I added the outline code. But that
;;      came with a price: I would have liked that the Heading-1 were
;;      separated by at least one empty space so that the buffer would
;;      look visually better.
;;
;;          heading1-1
;;              heading1-1
;;              heading1-2
;;                                      << empty space here
;;          heading2-1
;;              heading2-1
;;              heading2-2
;;
;;      But that would have required adding some extra code to do bound
;;      checking every time the block is collapsed/opened. Currently I'm
;;      not going to add that checking because it would reduce speed and
;;      the check would cause unnecessary complexity to current code
;;      flow.
;;
;;  Technical note: about the default prefix key z
;;
;;      The prefix key can be defined by setting `tinytf--mode-prefix-key'
;;      The default binding is `C-c` `C-z', but if you want more comfortable
;;      editing, you can set it to "z". When the key is a single character,
;;      the key "doubles"; i.e. pressing "zz" will generate the plain "z"
;;      character.
;;
;;  Technical note: HTML utilities
;;
;;      Two external utilities are searched for HTML generation:
;;      `t2html.pl' and `htmlize.el'. If these utilities are found,
;;      their locations are stored to plist of variable `tinytf-menu'.
;;      See source code of function `tinytf-utility-programs-check'
;;      for more.
;;
;;  Known bugs
;;
;;      In Emacs 20.6 the TinyTf pull down menu contains bold titles
;;      for certain sections. This gets almost always garbled with
;;      the first pull-down. Selecting the menu second time shows
;;      correct section names with bold letters.
;;
;;      [This note is valid only for old Emacs releases, prior 20.x]
;;      While you type text in the buffer, the post command activates at
;;      regular intervals to untabify the buffer. The untabify is done
;;      because it makes formatting text easier and when you print the text
;;      file, you can be sure that the output is the same regardless of
;;      the tabs.
;;
;;      If you have Emacs 19.30+ there is additional help feature available
;;      to you. When you sit still in some column position for few seconds,
;;      the column description will be shown automatically. That should
;;      help keeping you informed of the text layout.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ Libraries and compilation

(require 'tinylibm)

(eval-when-compile
  ;;  Need grep-regexp-alist
  (require 'compile))

(eval-and-compile
  (defvar tinytab-mode            nil)
  (defvar tinytab--div-factor     nil)
  ;;  Just forward declarations to shut up byte compiler.
  (defvar font-lock-keywords)
  (defvar font-lock-mode)
  (defvar global-font-lock-mode)
  (defvar grep-regexp-alist)
  (defvar add-log-current-defun-header-regexp)
  (autoload 'compile-internal "compile")
  (if (or (fboundp 'htmlize-buffer)
          (locate-library "htmlize"))
      (autoload 'htmlize-buffer "htmlize" "" t)
    (message "\
  ** tinytf.el: Hm, no htmlize.el found. [you can still use this package]
                2001-10-10 it was at http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el")))

(ti::package-defgroup-tiny TinyTf tinytf-- wp
  "Minor mode for writing text in 'Technical text format'.

      o   You write text in rigid format called 'Technical'
      o   There are only two heading levels, one at column 0, and
          another at column 4. NO OTHER SUB-HEADINGS SUPPORTED.
      o   Text is written in column 8
      o   Each column has different meaning how text is intepreted
          into html.
      o   The full 'Technical text format' is described in the
          function description of `tinytf-mode'")

;;}}}
;;{{{ setup: hooks

(defcustom tinytf--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyTf)

(defcustom tinytf--process-compile-hook
  '(tinytf-compile-mode-settings)
  "*Hook that is run when compile is called (See link check)."
  :type  'hook
  :group 'TinyTf)

(defcustom tinytf--move-paragraph-hook nil
  "*Hook run with arguments BEG END of region that was moved."
  :type  'hook
  :group 'TinyTf)

(defcustom tinytf--fix-all-hook nil
  "*Hook run when function `tinytf-fix-all' is called."
  :type  'hook
  :group 'TinyTf)

(defcustom tinytf--tinytf-mode-p-function 'tinytf-text-format-file-p
  "*Function to check if buffer is in Techical Format.
Function must return t or nil."
  :type  'function
  :group 'TinyTf)

(defcustom tinytf--t2html-link-cache-file
  (ti::package-config-file-prefix "tinytf-link-cache.txt")
  "*File to contains cached OK links for the link check feature."
  :type  'filename
  :group 'TinyTf)

(defcustom tinytf--buffer-file-name-html-source-function
  'tinytf-convert-buffer-file-name-html-source
  "*Return filename from where to read plain text.
For files this should be `buffer-file-name', but for buffer
that are not associated with file, a temporary filename shoudl be generated.

Function arguments:

  buffer pointer."
  :type  'function
  :group 'TinyTf)

(defcustom tinytf--buffer-file-name-html-destination-function
  'tinytf-convert-buffer-file-name-html-destination
  "*Return filename where the HTML is stored.

Function arguments:

  buffer pointer."
  :type  'function
  :group 'TinyTf)

(defcustom tinytf--binary-t2html
  (let ((path (ti::file-path-to-unix
               (ti::file-get-load-path "t2html.pl" exec-path))))
    (if path
        (message "tinytf.el: FOUND %s" path)
      (message
       (concat
        "tinytf.el: ** No t2html.pl along PATH. "
        "Visit http://perl-text2html.sourceforge.net/")))
    path)
  "Path to t2html.pl perl script. Do not rename t2html.pl."
  :type  'filename
  :group 'TinyTf)

;;}}}
;;{{{ setup: user config

(defcustom tinytf--heading-regexp "[A-Z0-9!]"
  "*Heading character set regexp. This charset is case sensitive.
If there is these characters immediately after indentation, then line
is a heading.

The default regexp is [A-Z0-9!], where the ! is used as a special control
code. The double !! signifies in the produced html
that there should be <hr> code before heading. Like the following.

    !! This heading has HR code in the html."
  :type  '(string :tag "Charset regexp")
  :group 'TinyTf)

(defcustom tinytf--heading-regexp-no-numbering "[!]"
  "*When numbering headings, ignore headings matching this regexp.
at the beginning of first word."
  :type  'string
  :group 'TinyTf)

(defcustom tinytf--sentence-end "[.?!][]\"')}]*[ \r\n]+"
  "*Like `sentence-end'. Used only in movement commands."
  :type 'string
  :group 'TinyTf)

(defcustom tinytf--paragraph-start "^[ \t]*$"
  "*Like `paragraph-start'. Used only in movement commands."
  :type 'string
  :group 'TinyTf)

(when (ti::emacs-p "21") ;; Variable width faces available

  (defface tinytf-quote-face
    '((((type tty pc) (class color))
       (:foreground "lightblue" :weight bold))
      (t (:height 1.0 :family "Georgia" )))
    "Face for real quotes"
    :group 'TinyTf)

  (defvar tinytf-quote-face 'tinytf-quote-face)

  (defface tinytf-quote2-face
    '((((type tty pc) (class color))
       (:foreground "lightblue" :weight bold))
      (t (:height 1.2 :family "helv" :italic t)))
    "Face for temporary highlight quote"
    :group 'TinyTf)

  (defvar tinytf-quote2-face 'tinytf-quote2-face)

  (defface tinytf-level-1-face
    '((((type tty pc) (class color)) (:foreground "blue" :weight bold))
      (t (:height 1.2 :inherit tinytf-level-2-face)))
    "Face for titles at level 1."
    :group 'TinyTf)

  (defvar tinytf-level-1-face 'tinytf-level-1-face)

  (defface tinytf-level-2-face
    '((((type tty pc) (class color)) (:foreground "lightblue" :weight bold))
      (t (:height 1.2 :inherit tinytf-level-3-face)))
    "Face for titles at level 2."
    :group 'TinyTf)

  (defvar tinytf-level-2-face 'tinytf-level-2-face)

  (defface tinytf-level-3-face
    '((((type tty pc) (class color)) (:foreground "black" :weight bold))
      (t (:height 1.2 :inherit variable-pitch)))
    "Face for titles at level 3."
    :group 'TinyTf)

  (defvar tinytf-level-3-face 'tinytf-level-3-face))

;; Order of these rexeps is very important; because there are many
;; "override" flags set to 't.
;;
;; 1999-11-23 `font-lock-other-type-face' doesn't exist in XEmacs 21.1.6
;;
;; Please COPY this variable settign to your $HOME/.emacs if you
;; want to change the colors. substitute `defcustom' with `setq'
;; and delete the variable comments at the end.

(defcustom tinytf--font-lock-keywords ;; &font
  (list

   ;; Bullet

   (list
    (concat (concat "^" (make-string 8 ?\ ) "[o.]   "))
    0 'font-lock-reference-face)

   ;; bullet continue comma

   (list
    (concat (concat "^" (make-string 12 ?\ ) "[,]"))
    0 'font-lock-reference-face)

   ;;  #REF and other user defined markers. See perl script
   ;;  for option --reference REF=value  that lets you define
   ;;  any markers.

   (list
    (concat "^"
            (make-string 8 ?\ )
            "#"
            "[^ \t\r\r\n][^ \t\r\n][^ \t\r\n]+")
    0 'font-lock-type-face)

   ;; Column 2 small bold blue

   (list
    (concat "^  \\([^ \t].*\\)$")
    1 'font-lock-builtin-face)

   ;; Column 3, emphasized

   (list
    (concat "^   \\([^ \t].*\\)$")
    1 'font-lock-constant-face)

   ;; Column 5 and 6

   (list
    (concat "^     \\([^ \t].*\\)$")
    1
    'font-lock-type-face)

   (list
    (concat "^      \\([^ \t].*\\)$")
    1
    (if (or (and (fboundp 'get-face) ;;  XEmacs
                 (get-face 'tinytf-quote-face))
            (facep 'tinytf-quote-face))
        'tinytf-quote-face
      'font-lock-comment-face))

   ;; Colum 7, leading double quote

   (list
    (concat "^" (make-string 7 ?\ ) "\\(\"[^ \t].*\\)$")
    1 'font-lock-comment-face)

   ;; Colum 7 and 9, Strong [Small level 3 headers]

   (list
    (concat "^" (make-string 7 ?\ ) "\\([^ \t].*\\)$")
    1
    (if (or (and (fboundp 'get-face) ;;  XEmacs
                 (get-face 'tinytf-level-3-face))
            ;; Only works in Emacs. Returns nil in XEmacs
            (facep 'tinytf-level-3-face))
        'tinytf-level-3-face
      'font-lock-comment-face)
    t)

   (list
    (concat "^" (make-string 9 ?\ ) "\\([^ \t].*\\)$")
    1 'font-lock-comment-face)

   ;;  Colum 10, Quotation

   (list
    (concat "^" (make-string 10 ?\ ) "\\([^ \t].*\\)$")
    1
    (if (or (and (fboundp 'get-face) ;;  XEmacs
                 (get-face 'tinytf-quote2-face))
            (facep 'tinytf-quote2-face))
        'tinytf-quote2-face
      'font-lock-type-face))

   (list
    (concat "^" (make-string 11 ?\ ) "\\([^ \t].*\\)$")
    1 'font-lock-constant-face)

   ;; ..................................................... emphasisis ...

   ;; _bold_ *italic*  and =small=

   '("_[^ \t\r\n]+_"  0 font-lock-type-face)

   '("\\*[^ \t\r\n]+\\*" 0 font-lock-variable-name-face)

   '(" =[^ '\t\r\n]+="    0 font-lock-builtin-face prepend)

   ;; ISO standard date YYYY-MM-DD HH:MM

   (list
    (concat
     "\\<[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
    0 'font-lock-keyword-face)

   ;; .................................................... references ...

   ;;  A long reference. There must be space somewhere otherwise
   ;;  this regexp would overlap with the shorter reference
   ;;  (font lock would do double job)
   ;;
   ;;       [1998-08 Phil carmody pc@foo.com in vegetarian-L]

   (list
    (let ((re "\\(\\[[^][\r\n]+ [^][\r\n]+\\]\\)"))
      (concat
       "^"
       ;; At column 8
       (make-string 8 ?\ )
       re))
    1 'font-lock-keyword-face t)

   (list
    "\\(\\[[^][\r\n]+ [^][\r\n]+\\]\\)"
    1 'font-lock-keyword-face)

   ;;  When you refer to people or to document you do it like this
   ;;
   ;;       [phil] said that [perlguts] is the document you should read..

   '("\\(\\[[^ \t\r\n]+\\]\\)" 1 font-lock-type-face t)

   ;; ........................................................ strings ...

   ;; Big letter words

   '("\\<[A-Z][-/_.A-Z0-9]+\\>"  0 font-lock-variable-name-face)

   ;; `this-string'

   '("`\\([^ '\t\r\n]+\\)'" 1 font-lock-reference-face prepend)

   ;; ........................................................... urls ...

   ;; File names or programs

   '("[ \t\r\n][\\/][^ \t\r\n]+\\>" 0 font-lock-comment-face t)

   ;;  URL highlighting, we won't highlight Whole URL, because
   ;;  then the document would look like Xmas tree if it had
   ;;  hundreads of links. (or tens of links)

   (list
    (concat "\\(http\\|ftp\\|news\\|wais\\)://"
            "\\|file:/"
            "\\|[\\][\\][^ \t\r\n]+" ;; UNC \\machine\dir

            ;;  There is no good regexp to detect Win32 idiotic
            ;;  "space in filenames"

            "\\|\\<[a-zA-Z]:[\\][^ \t\r\n]+" ;; c:\windows\file.txt
            "\\|\\<\\([a-zA-Z]:\\)?/[^ \t\r\n]+") ;; c:/windows/file.txt
    0 'font-lock-reference-face t)
   ;;   <foo@site.com> Email URLs.
   '("\\(<[^ \t\r\n]+@[^ \t\r\n]+>\\)" 1 font-lock-reference-face t)

;;;   (list
;;;    (concat "^" (make-string 12 ?\ ) "\\(.*\\)$")
;;;    '(1 font-lock-reference-face))

;;; #todo: Hmm, font-lock doesn't allow calling functions?

;;; The font-lock could also call functions that set the matched regions.
;;; However that doesn't seem to work. The code snippet below hangs,
;;; But if called directly via M-x tinytf-code-p, it works ok. Don't
;;; know what is the problem,

;;;   '(tinytf-code-p . font-lock-reference-face)

   ;; ........................................................ heading ...
   ;;  Headings 1 and 2

   (list
    "^\\([.A-Z0-9].*\\)$"
    1
    (if (or (and (fboundp 'get-face) ;;  XEmacs
                 (get-face 'tinytf-level-1-face))
            ;; Only works in Emacs. Returns nil in XEmacs
            (facep 'tinytf-level-1-face))
        'tinytf-level-1-face
      'font-lock-keyword-face)
    t)

   (list
    "^    \\([.A-Z0-9].*\\)$"
    1
    (if (or (and (fboundp 'get-face) ;;  XEmacs
                 (get-face 'tinytf-level-2-face))
            ;; Only works in Emacs. Returns nil in XEmacs
            (facep 'tinytf-level-2-face))
        'tinytf-level-2-face
      'font-lock-keyword-face)
    t))
  ;;    font-lock-reference-face    font-lock-keyword-face
  ;;    font-lock-type-face         font-lock-function-name-face
  ;;    font-lock-string-face       font-lock-comment-face
  ;;    font-lock-variable-name-face
  ;;
  ;;    font-lock-keywords

  "*Font lock keywords."
  :type   'sexp
  :group  'TinyTf)

;;}}}
;;{{{ suetup: private variables

(defvar tinytf--process-compile-html "tinytf-compile-html"
  "Name of the buffer/mode used for HTML compiling.")

(defvar tinytf--process-compile-link "tinytf-compile-link"
  "Name of the buffer/mode used for HTML link check.")

(defvar tinytf--file-last-html-generated nil
  "Filename of the last HTML generation.")

(defconst tinytf--factor 4
  "The indent factor. DO NOT CHANGE THIS. It is hard coded to 4.")

(defconst tinytf--heading-number-level 2
  "*Number of levels. Zero based. Allow values 0,1,2.")

(defvar tinytf--counter nil
  "Post command counter.")
(make-variable-buffer-local 'tinytf--counter)

(defvar tinytf--buffer-heading "*tinytf-headings*"
  "List of gatehered Headings from buffer.")

(defvar tinytf--buffer-html-process "*tinytf-t2html*"
  "Output of t2html.pl run.")

(eval-and-compile
  ;; Needed in defvar
  (defsubst tinytf-indent (&optional level)
    "Return indent space string at LEVEL. Default is 0."
    (cond
     ((eq level 2)
      (make-string (+ (* 4 1) 3) ?\  ))
     (t
      (make-string (* tinytf--factor (or level 0)) ?\ )))))

(defvar tinytf--add-log-current-defun-header-regexp
  (concat
   ;; [text]    Detect heading Levels 1 and 2 with possible numbering
   ;;
   ;;   1.0 Heading level one
   ;;
   ;;       1.1 Heading level two

   "^\\(    [0-9]+\\(\\.[0-9.]+\\)+[ \t]+[A-Z].*"
   "\\|^[0-9]+\\(\\.[0-9.]+\\)+[ \t]+[A-Z].*"
   "\\|^    [A-Z].*"
   "\\|^[A-Z].*"
   "\\)")
  "*Additional ChangeLog regepx to recognize tinytf.el headings.
This variable's locally set to `add-log-current-defun-header-regexp'
when `tinytf-mode' is turned on.")

(defvar tinytf--heading-ignore-regexp-form
  '(concat
    "Table [Oo]f [Cc]ontents"
    ;;  This is special <HR> mark, see t2html.pls
    "\\|^[ \t]*!!"
    "\\|^[0-9.]*[ \t]*End[ \t]*$"
    "\\|End[ \t]+of[ \t]+\\(file\\|document\\)[ \t]*$"
    "\\|"
    (concat "^" ;; \\(" (tinytf-indent 0) "\\|"
            "\\("  (tinytf-indent 1) "\\)?"
            tinytf--heading-regexp-no-numbering))
  "When making Table Of Contents, ignore these headings.
This variable contains Lisp form that is evaled to get the string.
If nil, then include all headings, dropping none.")

(defconst tinytf--column-table
  '(
    ;;  First the most common positions.
    (0   ((nil "Heading 0")))
    (4   ((nil "Heading 1")))
    (8   ((nil "Standard text")))
    ;; Then special positions
    (1   ((nil "Emphatised")))
    (2   ((nil "Emphatised")))
    (3   ((nil "Emphatised")))
    (5   ((nil "Normal text, colored")))
    (6   ((nil "Normal text, colored")))
    (7   (("\"" "Emphatised")
          (nil  "Heading 3, Strong")))
    (8   (("o "
           "Bullet, reguler")
          ("\. "
           "Bullet, numbered")
          ("\.[^ \t]"
           "Single line, <BR> added to the end of line")
          (",[^ \t]"
           "Line continues in next chapter. <P> code not added")))
    (9   ((nil "Strong")))
    (10  ((nil "Quotation, emphatised")))
    (11  ((nil "Normal, colored.")))

    (12  ((",[^ \t]"
           "Bullet continues in next chapter. <P> code not added")
          (nil
           "Sample"))))
  "Column positions and their properties. DO NOT TOUCH THIS.
The positions are fixed and reflect the perl program t2html.pl's HTML generation.

Format:
 '((COL ((REGEXP-OR-NIL EXPLANATION-STRING)
         (REGEXP-OR-NIL EXPLANATION-STRING)
         ..))
   (COL ((RE EXPL) (RE EXPL) ..)))")

(defvar tinytf--saved-indent-tabs-mode
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-left-margin
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-tinytab-mode
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-font-lock-keywords
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-auto-fill-function
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-auto-fill-inhibit-regexp
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-comment-start
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

(defvar tinytf--saved-comment-end
  "Buffer local variable. Holds copy of original value before `tinytf-mode'.")

;;}}}
;;{{{ Experimental

;;; ----------------------------------------------------------------------
;;; #todo: experimental
(defun tinytf-code-p (&optional limit)
  "Determine if current text is code. LIMIT parameter is passed by font-lock."
  (let ((re `,(concat "^" (make-string 12 ?\ ))))
    (and
     (save-excursion
       (beginning-of-line)
       (and (looking-at re)             ;match to return to font-lock
            (goto-char (match-end 0))
            (looking-at "\\(.*\\)$")))
     (save-excursion
       (save-match-data                 ;Now check if it was really ok
         (message (ti::read-current-line))
         (forward-line -1)
         (or (looking-at re)
             (progn
               ;;  skip empty lines
               (while (and (not (bobp))
                           (not (input-pending-p))
                           (looking-at "^[ \t]*$"))
                 (forward-line -1))
               (message (ti::read-current-line))
               (when (and
                      (not (input-pending-p))
                      ;; Same indentation still
                      (looking-at re)   )
                 ;;  Not a P-comma code in bullet?
                 (not (looking-at ".*[,o.]"))))))))))

;;; ----------------------------------------------------------------------
;;; #todo:
;;;
(defun tinytf-tmp-swallow-empty-backwards ()
  (interactive)
  (forward-line 1)
  (while
      (and
       (not (bobp))
       (looking-at "^$"))
    (forward-line 1)))

;;; ----------------------------------------------------------------------
;;; #todo:
;;;
(defun tinytf-tmp-swallow-code-backwards ()
  (interactive)
  (let ((re         `,(concat "^" (make-string 12 ?\ )))
	(b-cont-re  `,(concat "^" (make-string 12 ?\ ) ","))
	(empty      `,(concat "^$")))
    ;;(message "gulp")
    (beginning-of-line)
    (while
        (and
         (not (bobp))
         (looking-at re))
      (forward-line 1))
    (if (and
         (not (bobp))
         (looking-at empty))
        (progn
          (tinytf-tmp-swallow-empty-backwards)
          (and
           (not (looking-at b-cont-re))
           (looking-at re)
           (tinytf-tmp-swallow-code-backwards))))))

;;; ----------------------------------------------------------------------
;;; #todo:
;;;
(defun tinytf-tmp-dxc-code-p (&optional limit)
  (interactive)
  (let ((bullet    `,(concat (concat "^" (make-string 8 ?\ ) "[o.] ")))
	(re        `,(concat "^" (make-string 12 ?\ )))
	(b-cont-re `,(concat "^" (make-string 12 ?\ ) ","))
	(empty     `,(concat "^$")))
    (and
     (save-excursion
       (beginning-of-line)
       (and (looking-at re)
            (goto-char (match-end 0))
            ;; match to return to font-lock
            (looking-at "\\(.*\\)$"))))
    (save-match-data
      (save-excursion
        (beginning-of-line)
        (if (looking-at empty)
            (tinytf-tmp-swallow-empty-backwards))
        (if (not (looking-at re))
            nil
          (progn
            (tinytf-tmp-swallow-code-backwards)
            (not (or (looking-at b-cont-re)
                     (looking-at bullet)))))))))

;;; ----------------------------------------------------------------------
;;; a simple one for testing only.
;;; #todo:

(defun tinytf-tmp-dxc-2-code-p (&optional limit)
  (interactive)
  (let ((re `,(concat "^" (make-string 12 ?\ ))))
    (save-excursion
      (beginning-of-line)
      (looking-at re))))

;;}}}
;;{{{ Minor Mode definition

;;; .......................................................... &v-mode ...
;;;###autoload (autoload 'tinytf-install-mode       "tinytf" "" t)
;;;###autoload (autoload 'tinytf-mode               "tinytf" "" t)
;;;###autoload (autoload 'turn-on-tinytf-mode       "tinytf" "" t)
;;;###autoload (autoload 'turn-off-tinytf-mode      "tinytf" "" t)
;;;###autoload (autoload 'tinytf-commentary         "tinytf" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinytf-" " Tf" "\C-c\C-z" "Tf" 'TinyTf "tinytf--" ;1-6

   "Minor mode for writing and editing text in technical format (TF).
The text layout presented in this minor mode is ment to be
feed to t2html.pls perl script which generates html out of
plain text file.

The perl code is included in source file and can be unpacked with
command \\[tinytf-install-files]

To see the complete layout description and rules,
run command `tinytf-commentary'. However, this description
may be a little out of synch and you should consult perl file
t2html.pl, available at nearest Perl CPAN http://cpan.perl.org/, for
up to date description.

Mode description:

\\{tinytf--mode-prefix-map}
"

   "Technical text format"

   (progn
     ;;  reinstall is done every time, because some key definitions
     ;;  are built dynamically from current/global map
     ;;  Make C-x 4 a to detect text headings for ChangeLog
     (tinytf-install-add-log  (if tinytf-mode
                                  nil
                                'remove))
     (cond
      (tinytf-mode
       (tinytf-utility-programs-check)
       (with-buffer-modified
         (tinytf-install-mode)
         (make-local-variable 'tinytf--saved-indent-tabs-mode)
         (make-local-variable 'tinytf--saved-left-margin)
         (make-local-variable 'tinytf--saved-tinytab-mode)
         (make-local-variable 'tinytf--saved-font-lock-keywords)
         (make-local-variable 'tinytf--saved-comment-start)
         (setq tinytf--saved-indent-tabs-mode  indent-tabs-mode)
         (setq tinytf--saved-left-margin       left-margin)
         (setq tinytf--saved-tinytab-mode      tinytab-mode)
         ;;  Emacs 21.2 newcomment.el breaks if these are not set
         ;;  properly. When auto-fill-mode is on, the call chain is:
         ;;
         ;;   newline
         ;;     self-insert-command
         ;;       do-auto-fill
         ;;         comment-indent-new-line  (newcomment.el)
         ;;           comment-normalize-vars
         (setq tinytf--saved-comment-start     comment-start)
         (setq tinytf--saved-comment-end       comment-end)
         (setq comment-start "")
         (setq comment-end "")
         ;;  When auto fill is used, do not indent lines that
         ;;  contain special tags starting with "#", which may continue
         ;;  past the right side. The tags must all be in one line, not
         ;;  broken to multiple lines:
         ;;
         ;;      #PIC pic/this-picture.jpg # Explanation which is long .....  ###
         ;;
         ;;  Also, do not break long headings.
         ;;
         ;;      2.2 This long chapter ....
         (make-local-variable 'auto-fill-inhibit-regexp)
         (setq tinytf--saved-auto-fill-inhibit-regexp auto-fill-inhibit-regexp)
         (setq auto-fill-inhibit-regexp "^[ \t]+#\\|^[ \t]+[0-9]\\.[0-9] [A-Z]")
         (make-local-variable 'tinytf--saved-auto-fill-function)
         (setq tinytf--saved-auto-fill-function auto-fill-function)
         (auto-fill-mode 1)
         (setq selective-display            t
               selective-display-ellipses   t
               ;; left-margin               8  ;; for return key
               indent-tabs-mode             nil)
         (unless tinytab-mode ;;Turn on this mode
           (setq tinytf--saved-tinytab-mode nil)
           (turn-on-tinytab-mode))
         (setq tinytab--div-factor 4) ;; advance by four spaces
         ;;  Make sure RETURN key continues indent.
         (turn-on-tinytab-return-key-mode)
         ;;  Single space ends sentence. The Emacs default is some
         ;;  old relict that nobody uses, or should use any more.
         (make-local-variable 'sentence-end-double-space)
         (setq sentence-end-double-space nil)
         ;; (make-local-variable 'sentence-end)
         ;; (setq sentence-end "[.?!][]\"')}]*\\($\\|[ \t]\\)[ \t\r\n]*"
         ;;  Use our font lock keywords this time and save original
         (when (boundp 'font-lock-keywords)
           (setq tinytf--saved-font-lock-keywords font-lock-keywords)
           (tinytf-font-lock-mode))))
      (t
       (with-buffer-modified
         ;; Restore values
         (setq indent-tabs-mode         tinytf--saved-indent-tabs-mode)
         (setq auto-fill-function       tinytf--saved-auto-fill-function)
         (setq auto-fill-inhibit-regexp tinytf--saved-auto-fill-inhibit-regexp)
         (setq comment-start            tinytf--saved-comment-start)
         (when (integerp tinytf--saved-left-margin)
           (setq left-margin tinytf--saved-left-margin))
         (if tinytf--saved-tinytab-mode
             (turn-on-tinytab-mode)
           (turn-off-tinytab-mode))
         (setq selective-display nil)
         (tinytf-show-buffer)
         (when (boundp 'font-lock-keywords)
           (setq font-lock-keywords tinytf--saved-font-lock-keywords)
           (when (ti::colors-supported-p)
             (save-excursion
               ;;  force font lock to rework everything
               (set-buffer-modified-p nil)
               (set-text-properties (point-min) (point-max) nil)
               (tinytf-fontify-current-buffer-window))))))))

   "Technical text writing menu."

   (list
    tinytf--mode-easymenu-name
    "Markup"
    ["Mark word strong"       tinytf-mark-word-strong       t]
    ["Mark word sample"       tinytf-mark-word-sample       t]
    ["Mark word emphatised"   tinytf-mark-word-emp          t]
    ["Mark word small"        tinytf-mark-word-small        t]
    ["Mark word big"          tinytf-mark-word-big          t]
    ["Mark <BR> line"         tinytf-mark-br-line           t]
    ["Mark <BR> paragraph"    tinytf-mark-br-paragraph      t]
    ["Unmark word"            tinytf-unmark-word            t]
    "Indentation"
    ["Convert to bullet"      tinytf-bullet-format          t]
    ["Indent paragraph text"  tinytf-indent-paragraph-text  t]
    ["Indent paragraph text 'as is'"  tinytf-indent-paragraph-text-as-is  t]
    ["Indent paragraph text and fill"  tinytf-indent-paragraph-text-and-fill  t]
    ["Indent paragraph Quote"   tinytf-indent-paragraph-quote  t]
    ["Indent paragraph Quote and fill" tinytf-indent-paragraph-quote-and-fill t]
    ["Indent paragraph Sample"  tinytf-indent-paragraph-sample t]
    (list
     "Indent by column"
     ["Indent paragraph zero"  tinytf-indent-paragraph-zero t]
     ["Indent paragraph 2"     tinytf-indent-paragraph-2    t]
     ["Indent paragraph 3"     tinytf-indent-paragraph-3    t]
     ["Indent paragraph 5"     tinytf-indent-paragraph-5    t]
     ["Indent paragraph 6"     tinytf-indent-paragraph-6    t]
     ["Indent paragraph 11"    tinytf-indent-paragraph-11   t])
    "----"
    (list
     "HTML"
     "t2html"
     ["HTML basic" tinytf-convert-t2html-basic
      (get 'tinytf-mode  't2html)]
     ["HTML frames" tinytf-convert-t2html-frame
      (get 'tinytf-mode  't2html)]
     ["HTML as you see" tinytf-convert-t2html-as-is
      (get 'tinytf-mode  't2html)]
     ["Link check" tinytf-convert-t2html-link-check
      (get 'tinytf-mode  't2html)]
     ["Link check (with cache)" tinytf-convert-t2html-link-check-cached
      (get 'tinytf-mode  't2html)]
     "----"
     "htmlize"
     ["HTML buffer"  tinytf-convert-htmlize
      (get 'tinytf-mode  'htmlize)]
     "----"
     ["View HTML with browser"  tinytf-convert-view-default t]
     ["View HTML source"        tinytf-convert-view-html-source t]
     "----"
     ["Conversion preferences"  tinytf-convert-preference-set t]
     ["Conversion menu re-evaluate" tinytf-utility-programs-check-force t])
    (list
     "Indent Region"
     ["Indent region strong"   tinytf-indent-region-strong   t]
     ["Indent region sample"   tinytf-indent-region-sample   t]
     ["Indent region quote"    tinytf-indent-region-quote    t]
     ["Indent region text"     tinytf-indent-region-text     t])
    (list
     "Headings"
     "Heading management"
     ["Heading 1 backward"     tinytf-heading-backward-0     t]
     ["Heading 1 forward"      tinytf-heading-forward-0      t]
     ["Heading 2 backward"     tinytf-heading-backward-any   t]
     ["Heading 2 forward"      tinytf-heading-forward-any    t]
     "----"
     ["Heading numbering"      tinytf-heading-numbering      t]
     ["Heading fix 1st chars"  tinytf-heading-fix            t]
     ["Heading fix case"       tinytf-heading-fix-case       t]
     ["Heading fix case all"   tinytf-heading-fix-case-all   t]
     "----"
     ["Paragraph forward"      tinytf-forward-paragraph      t]
     ["Paragraph backward"     tinytf-backward-paragraph     t])
    (list
     "Outline"
     ["Hide buffer"            tinytf-hide-buffer            t]
     ["Show buffer"            tinytf-show-buffer            t]
     ["Hide heading"           tinytf-hide                   t]
     ["Show heading"           tinytf-show                   t]
     ["Show/hide toggle"       tinytf-show-toggle            t])
    (list
     "Misc"
     ["Toc create"             tinytf-toc                     t]
     ["Toc popup"              tinytf-toc-mouse               t]
     ["Toc Occur"              tinytf-toc-occur               t]
     "----"
     ["Selective display copy"  ti::buffer-selective-display-copy-to t]
     ["Selective display print" ti::buffer-selective-display-print   t]
     "----"
     ["Untabify buffer"        tinytf-untabify-buffer         t]
     ["Column info"            tinytf-column-info-display     t])
    "----"
    "Package functions"
    ["Package version"        tinytf-version                  t]
    ["Package commentary"     tinytf-commentary               t]
    ["Mode and menu reload"   tinytf-mode-reload              t]
    ["Mode help"              tinytf-mode-help                t]
    ["Mode exit and cleanup"  tinytf-exit                     t]
    ["Mode exit"              turn-off-tinytf-mode            t])
   (progn
     (let ()
       (define-key map  "?"  'tinytf-mode-help)
       (define-key map  "H"   nil)
       (define-key map  "Hm" 'tinytf-mode-help)
       (define-key map  "Hc" 'tinytf-commentary)
       (define-key map  "Hv" 'tinytf-version)
       (define-key map  "n"  'tinytf-heading-numbering)
       (define-key map  "u"  'tinytf-untabify-buffer)
       ;;  Toc
       ;;  mouse binding "Prefix + mouse-1"
       (define-key map  "T" 'tinytf-toc)
       (define-key map  "O" 'tinytf-toc-occur)
       (if (ti::emacs-p)
           (define-key map [(mouse-1)] 'tinytf-toc-mouse)
         (define-key map [(button1)] 'tinytf-toc-mouse))
       ;;
       ;;  Marking commands in non-shift keys
       ;;  STRONG = "."; like a heavy statement that
       ;;  ends like this: "I said that. Perioti::d!"
       ;;
       ;;  It is most likely that you "EMP" characters most of the time;
       ;;  That's why "`" is not defined as "w-". And to Undo easily
       ;;  marking, the unmark must be fast to access via " ".
       ;;
       (define-key map  " "  'tinytf-unmark-word)
       (define-key map  "-" 'tinytf-mark-word-strong)
       (define-key map  "'" 'tinytf-mark-word-sample) ;; code
       (define-key map  "*" 'tinytf-mark-word-emp)    ;; italics
       (define-key map  "+" 'tinytf-mark-word-big)
       (define-key map  "_" 'tinytf-mark-word-small)
       (define-key map  "mB" 'tinytf-mark-br-line)
       (define-key map  "mb" 'tinytf-mark-br-paragraph)
       ;;  If you're converting some document to TF format,
       ;;  therse are the commands you will use 80% of the time.
       (define-key map  "rS" 'tinytf-indent-region-strong)
       (define-key map  "rs" 'tinytf-indent-region-sample)
       (define-key map  "rq" 'tinytf-indent-region-quote)
       (define-key map  "rt" 'tinytf-indent-region-text)
       (define-key map  "c"   nil)
       (define-key map  "ct"  nil)
       (define-key map  "chb" 'tinytf-convert-t2html-basic)
       (define-key map  "chf" 'tinytf-convert-t2html-frame)
       (define-key map  "cha" 'tinytf-convert-t2html-as-is)
       (define-key map  "chl" 'tinytf-convert-t2html-link-check)
       (define-key map  "chL" 'tinytf-convert-t2html-link-check-cached)
       (define-key map  "chH" 'tinytf-convert-htmlize)
       (define-key map  "cr"  'tinytf-utility-programs-check-force)
       (define-key map  "cp"  'tinytf-convert-preference-set)
       (define-key map  "cvv" 'tinytf-convert-view-default)
       (define-key map  "cvf" 'tinytf-convert-view-html-source)
       (define-key map  "b" 'tinytf-bullet-format)
       (define-key map  "t" 'tinytf-indent-paragraph-text)
       (define-key map  "a" 'tinytf-indent-paragraph-text-as-is)
       ;; l = fil(l)
       (define-key map  "l" 'tinytf-indent-paragraph-text-and-fill)
       (define-key map  "q" 'tinytf-indent-paragraph-quote-and-fill)
       (define-key map  "Q" 'tinytf-indent-paragraph-quote)
       (define-key map  "s" 'tinytf-indent-paragraph-sample)
       (define-key map  "0" 'tinytf-indent-paragraph-zero)
       (define-key map  "1" 'tinytf-indent-paragraph-11)
       (define-key map  "2" 'tinytf-indent-paragraph-2)
       (define-key map  "3" 'tinytf-indent-paragraph-3)
       (define-key map  "5" 'tinytf-indent-paragraph-5)
       (define-key map  "6" 'tinytf-indent-paragraph-6)
       (define-key map  "f"  nil)
       (define-key map  "fa" 'tinytf-fix-all)
       (define-key map  "fh" 'tinytf-heading-fix)
       (define-key map  "fC" 'tinytf-heading-fix-case-all)
       (define-key map  "fc" 'tinytf-heading-fix-case)
       (define-key map  "fn" 'tinytf-heading-fix-newlines)
       ;;  Selective display
       (define-key map  "Sp" 'ti::buffer-selective-display-print)
       (define-key map  "Sc" 'ti::buffer-selective-display-copy-to)
       ;;  Hiding
       (define-key map  "\C-q" 'tinytf-show-toggle)
       (define-key map  "\C-x" 'tinytf-hide)
       (define-key map  "\C-s" 'tinytf-show)
       (define-key map  "\C-w" 'tinytf-hide-buffer)
       (define-key map  "\C-y" 'tinytf-show-buffer)
       ;; Info
       (define-key map  "\C-m" 'tinytf-column-info-display)
       (define-key map  "xr"   'tinytf-mode-reload)
       (define-key map  "xX"   'tinytf-exit)
       (define-key map  "xx"   'turn-off-tinytf-mode)
       ;;  Original PgUp and down keys --> move under Control key
       (ti::copy-key-definition root-map [(control prior)]  [(prior)])
       (ti::copy-key-definition root-map [(control next)]   [(next)])
       (define-key root-map [(prior)]         'tinytf-heading-backward-any)
       (define-key root-map [(next)]          'tinytf-heading-forward-any)
       (define-key root-map [(shift prior)]   'tinytf-heading-backward-0)
       (define-key root-map [(shift next)]    'tinytf-heading-forward-0)
       ;;  The Shift-prior do not always show in non-window system, so define
       ;;  these:
       (define-key map  "\C-p" 'tinytf-heading-backward-0)
       (define-key map  "\C-n" 'tinytf-heading-forward-0)
       ;;  The 'home' and 'end' keys
       (ti::copy-key-definition root-map [(control end)]      [(end)])
       (ti::copy-key-definition root-map [(control home)]     [(home)])
       (ti::copy-key-definition root-map [(control select)]   [(select)])
       (define-key root-map [(home)]          'tinytf-backward-paragraph)
       (define-key root-map [(select)]        'tinytf-forward-paragraph)
       (define-key root-map [(end)]           'tinytf-forward-paragraph)
       (if (ti::emacs-p)
           (define-key map [(mouse-3)]
             'tinytf-mouse-context-sensitive)
         (define-key map [(button3)]
           'tinytf-mouse-context-sensitive))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytf-mode-define-f-keys ()
  "Define default function key to `tinytf--mode-map'."
  (interactive)
  (let ((map tinytf--mode-map))
    ;;  more faster keys than the "w" word markup map.
    (define-key map [(f5)]  'tinytf-mark-word-emp)
    (define-key map [(f7)]  'tinytf-mark-word-strong)
    (define-key map [(f8)]  'tinytf-mark-word-sample)
    (define-key map [(f9)]  'tinytf-unmark-word)
    (define-key map [(f10)] 'tinytf-indent-paragraph-text-and-fill)
    (define-key map [(f11)] 'tinytf-indent-paragraph-quote-and-fill)
    (define-key map [(f12)] 'tinytf-indent-paragraph-sample)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-mode-reload ()
  "Reload and activate greyed menus (if new programs available).
If you have changed `exec-path' or added htmlize.el along
`load-path' the menus do not know when this has happened.

Calling this function re-eaxamines available utilities."
  (interactive)
  (tinytf-utility-programs-check-force))

;;}}}
;;{{{ mode install

;;; ----------------------------------------------------------------------
;;;
;;;###autoload (autoload 'tinytf-install-files "tinytf" "" t)
(ti::macrof-install-pgp-tar tinytf-install-files  "tinytf.el")

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-font-lock-mode ()
  "Install `font-lock' support. Activates only if `tinytf-mode' is on."
  (when (and tinytf-mode
             (boundp 'font-lock-keywords)
             (or (and (boundp 'font-lock-mode)
                      font-lock-mode)
                 (and (boundp 'global-font-lock-mode)
                      global-font-lock-mode)))
    (when (font-lock-mode-maybe 1)
      (setq font-lock-keywords tinytf--font-lock-keywords)
      ;;  if lazy-lock is in effect, it may not fontify the current window
      ;;  Do it now.
      (tinytf-fontify-current-buffer-window))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-install-eval-after-load (&optional uninstall)
  "Intall or UNINSTALL `eval-after-load' for add-log.el."
  (let ((form '(tinytf-install-add-log-all-buffers)))
    (cond
     (uninstall
      (dolist (elt after-load-alist)
        (when (and (string-match "add-log" (car elt))
                   (member form (cdr elt)))
          (setq after-load-alist (delete elt after-load-alist)))))
     (t
      (eval-after-load "add-log" form)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-install-add-log (&optional uninstall)
  "Install or UNINSTALL add-log.el support.
Calling this function makes variable
`add-log-current-defun-header-regexp' local in the current
buffer. The variable includes regexp to match heading levels so that
the ChangeLog entry is put in parentheses:

  * file.txt (This Heading): <explanation>
             ==============

References:

  `tinytf--add-log-current-defun-header-regexp'."
  (let ((sym 'add-log-current-defun-header-regexp))
    (when (boundp sym)
      ;;  See `add-log-current-defun'
      (if uninstall
          (kill-local-variable sym)
        (make-local-variable sym)
        (set sym tinytf--add-log-current-defun-header-regexp)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-install-add-log-all-buffers ()
  "Install add-log.el support for all `tinytf-mode' buffers."
  (ti::dolist-buffer-list
   (and tinytf-mode)
   nil
   nil
   (tinytf-install-add-log)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-install (&optional uninstall verb)
  "Install hook to mode or UNINSTALL. VERB allows verbose messages."
  (interactive "P")
  (ti::verb)
  (tinytf-install-eval-after-load uninstall))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-not-p ()
  "This is backup to verify after running `tinytf-text-format-p'.
Function `tinytf-text-format-p' may consider the file as TF format,
but it would be good to check few things before making decisive
conclusion."
  (let ()
    (or (ti::re-search-check
         ;;  Two consequent lines together, not good.
         "^[^ \t\r\n].*\\(\r\n\\|\r\\|\n\\)[^ \t\r\n]"
         nil nil 'read))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-ok-p-test-toc ()
  "Check if buffer content looks like technical format.
This is low level check. Use `tinytf-text-format-ok-p' instead."
  (ti::re-search-check
   ;;  1) If we see this
   "^Table [Oo]f [Cc]ontents[ \t]*$"
   nil nil 'read))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-ok-p-test-headings ()
  "Check if buffer content looks like technical format.
This is low level check. Use `tinytf-text-format-ok-p' instead."
  ;;  See if you can find two consequtive headings. The
  ;;  extra ".*" at the end of regexp is just for debugging
  ;;  purpose: what are the lines that were matched. Headings
  ;;  may be numbered.
  ;;
  ;;  Heading one            1.0 Heading one
  ;;
  ;;      Heading two           1.1 Heading two
  (ti::re-search-check
   (concat
    "^\\(\\([0-9]\\.[0-9.]*[0-9]\\) \\)?[A-Z][^ \t\f\r\n].*"
    "\\(\n\n\\|\r\n\r\n\\)" ;;  Two newlines
    "    \\(\\([0-9]\\.[0-9.]*[0-9]\\) \\)?[A-Z][^ \t\f\r\n].*")
   nil nil 'read))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-ok-p-test-heading-and-text ()
  "Check if buffer content looks like technical format.
This is low level check. Use `tinytf-text-format-ok-p' instead."
  ;;  Try to find one heading and regular text
  ;;
  ;;         Heading one
  ;;
  ;;                 And normal text at column 8
  ;;
  ;;  But take into account a special case, where the text starts
  ;;  at column 7, which causes it to be rendered as "small
  ;;  heading level 3"
  ;;
  ;;         Heading one
  ;;
  ;;                This is small heading, at column 7, offset -1
  ;;
  ;;                 And normal text at column 8
  ;;                 And normal text at column 8
  ;;
  ;;                This is small heading, at column 7, offset -1
  (ti::re-search-check
   (concat
    "^\\(    \\)?\\([0-9]\\.[0-9.]*[0-9] \\)?[A-Z][A-Za-z].*"
    "\\(\n\n\\|\r\n\r\n\\)"
    "        ? ? ?[A-Z][A-Za-z].*")
   nil nil 'read))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-ok-p ()
  "Check if buffer content looks like technical format."
  (interactive)
  (let (case-fold-search
	ret)
    ;; Exclude mail messages from any checks.
    (unless (ti::mail-mail-p)
      (setq ret
            (or (tinytf-text-format-ok-p-test-toc)
                (tinytf-text-format-ok-p-test-headings)
                (tinytf-text-format-ok-p-test-heading-and-text))))
    (if (interactive-p)
        (if (null ret)
            (message "Tinytf: No TF formattting found in this buffer.")
          (message "Tinytf: Found TF format location [%s]" ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-p ()
  "Check if buffer looks like TF format."
  (and (tinytf-text-format-ok-p)
       (not (tinytf-text-format-not-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-text-format-file-p ()
  "Test that file extension is .txt and `tinytf-text-format-p' returns t."
  (and (string-match
        "\\.txt"
        (or (buffer-file-name) ""))
       (tinytf-text-format-p)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun turn-on-tinytf-mode-maybe ()
  "If buffer looks like technical format, turn on `tinytf-mode'.
References:
  `tinytf--tinytf-mode-p-function'."
  (when (and tinytf--tinytf-mode-p-function
             (funcall tinytf--tinytf-mode-p-function))
    (turn-on-tinytf-mode)
    ;;  Hook must return nil
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinytf-mode-all-buffers ()
  "Call`tinytf-mode' on in all technical format buffers. Optionally OFF.
The buffer is detected by using function strored in variable
`tinytf--tinytf-mode-p-function'"
  (interactive)
  (when tinytf--tinytf-mode-p-function
    (ti::dolist-buffer-list
     (and (null tinytf-mode)
          (string-match "text" (downcase (symbol-name major-mode)))
          (funcall tinytf--tinytf-mode-p-function))
     nil
     nil
     (turn-on-tinytf-mode))))

;;}}}
;;{{{ macros, defsubst

;;; These macros create functions
;;;
;;; For some unknown reason the ByteCompiler doesn't see thse
;;; function in the followed macros unless the functions are wrapped
;;; inside eval-and-compile FORM.
;;;
;;; fmacro = function create macro

(eval-and-compile

;; #todo FIXME `,func
;;; ----------------------------------------------------------------------
;;;
  (defun tinytf-fmacro-indent-region-1 (func doc col msg &rest body)
    "Use `tinytf-fmacro-indent-region' with FUNC DOC COL MSG BODY."
    (let ((sym (intern (symbol-name `,func))))
      `(defun ,sym  (beg end &optional verb)
         ,doc
         (interactive "*r")
         (ti::verb)
         (let ((reg
		(tinytf-move-paragraph-to-column
		 beg end ,col
		 (if verb
		     ,msg))))
           ,@body))))

;; #todo FIME: `,func
;;; ----------------------------------------------------------------------
;;;
  (defun tinytf-fmacro-mark-word-1 (func doc char1 &optional char2)
    "Use `tinytf-fmacro-mark-word' with FUNC DOC CHAR1 CHAR2."
    (let ((sym (intern (symbol-name `,func))))
      `(defun ,sym ()
         ,doc
         (interactive "*")
         (unless (ti::space-p (preceding-char))
           (skip-chars-backward "^ ,\t\f\r\n"))
         (insert (char-to-string ,char1))
         (skip-chars-forward "^ ,\t\f\r\n")
         (insert (char-to-string (or ,char2 ,char1)))
         (skip-chars-forward " ,\t\f\r\n"))))

;;; ----------------------------------------------------------------------
;;;
  (defun tinytf-paragraph-bounds ()
    "Return (beg . end) points of paragraph."
    (let ((empty-line (ti::nil-p (ti::read-current-line)))
	  beg
	  end)
      (save-excursion
        (save-excursion
          (if (not empty-line)
              (or (re-search-backward "^[ \t]*$" nil t)
                  (ti::pmin)))
          (setq beg (point)))
        (if empty-line
            (skip-chars-forward " \t\r\n"))
        (or (re-search-forward "^[ \t]*$" nil t) (ti::pmax))
        (setq end (point)))
      (if (and beg end)
          (cons beg end))))

;; #todo FIXME `,func
;;; ----------------------------------------------------------------------
;;;
  (defun tinytf-fmacro-indent-paragraph-1 (func doc col msg &rest body)
    "Use `tinytf-fmacro-indent-paragraph'."
    (let ((sym (intern (symbol-name `,func))))
      `(defun ,sym  (&optional verb)
         ,doc
         (interactive "*")
         (let* ((region (tinytf-paragraph-bounds))
                (beg    (car-safe region))
                (end    (cdr-safe region)))
           (ti::verb)
           (if (null region)
               (if verb (message "%s: Cannot find paragraph bounds."
                                 tinytf--mode-name))
             (tinytf-move-paragraph-to-column
              beg
              end
              ,col
              (if verb
                  ,msg)
              'noask)
             ,@body)))))

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++-- - eval end --
  ) ;; eval-end

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-compile 'lisp-indent-function 3)
(defmacro tinytf-compile (cmd mode &optional regexp)
  "Run compile command based on Emacs version."
  (if (fboundp 'compilation-start) ;; Emacs
      `(compilation-start ,cmd ,mode (not 'name-function) ,regexp)
    `(compile-internal ,cmd "No more lines." ,mode (not 'parser) ,regexp)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinytf-fmacro-indent-region (func doc col msg &optional body)
  "Create indent function FUNC with DOC COL MSG BODY.
Created function arguments: (beg end &optional verb)"
  `,(tinytf-fmacro-indent-region-1
     func
     doc
     col
     msg
     body))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinytf-fmacro-mark-word (func doc char1 &optional char2)
  "Create word marking function FUNC with DOC and CHAR.
Created function arguments: ()"
  `,(tinytf-fmacro-mark-word-1
     func doc char1 char2))

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-fmacro-indent-paragraph 'edebug-form-spec '(body))
(defmacro tinytf-fmacro-indent-paragraph (func doc col msg &optional body)
  "Create word marking function FUNC with DOC and COL, MSG and BODY.
Created function arguments: ()"
  `,(tinytf-fmacro-indent-paragraph-1
     func doc col msg body))

;;; These are conventional macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-paragraph-macro 'lisp-indent-function 0)
(put 'tinytf-paragraph-macro 'edebug-form-spec '(body))
(defmacro tinytf-paragraph-macro (&rest body)
  "Set paragraph values locally while executing BODY."
  `(let ((sentence-end         tinytf--sentence-end)
	 (paragraph-start      tinytf--paragraph-start)
	 (paragraph-separate   paragraph-start))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-heading-macro  'lisp-indent-function 0)
(put 'tinytf-heading-macro 'edebug-form-spec '(body))
(defmacro tinytf-heading-macro (&rest body)
  "Map over every heading. The point sits at the beginning of heading text.
The BODY must move the point so that next heading can be found."
  `(let ((RE-search  (concat
		      (tinytf-regexp)
		      (if (> tinytf--heading-number-level 0)
			  (concat "\\|" (tinytf-regexp 1)))
		      (if (> tinytf--heading-number-level 1)
			  (concat "\\|" (tinytf-regexp 2)))))
	 (RE-no      (or (eval tinytf--heading-ignore-regexp-form)
			 "NothingMatchesLikeThis")))
     (save-excursion
       (tinytf-heading-start)
       (while (re-search-forward RE-search nil t)
         (unless (string-match RE-no (ti::read-current-line))
           ,@body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-level-macro 'lisp-indent-function 0)
(put 'tinytf-level-macro 'edebug-form-spec '(body))
(defmacro tinytf-level-macro (&rest body)
  "Search begin point of current heading level or signal error.
You can refer to variable 'level' and 'beg' and 'end' in the BODY.
The point is at start of level."
  `(let ((level (tinytf-level-number))
	 beg
	 end)
     (unless level
       (if (tinytf-heading-backward-any)
           (setq level (tinytf-level-number))
         (error "Can't find begin point")))
     (setq beg (point)
           end (tinytf-block-end))
     (goto-char beg)
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-regexp (&optional level)
  "Return indent regexp string at LEVEL."
  ;;  control character are not counted, like ^L page mark
  (concat "^" (tinytf-indent level) tinytf--heading-regexp))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-level-p (&optional level)
  "Check if line is LEVEL."
  (let (case-fold-search)              ;case sensitive match
    (save-excursion
      (beginning-of-line)
      (looking-at (tinytf-regexp level)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-level-number ()
  "Check current level on this line."
  (cond
   ((tinytf-level-p 0) 0)
   ((tinytf-level-p 1) 1)
   ((tinytf-level-p 2) 2)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-block-end ()
  "Return text block end."
  (save-excursion
    (if (null (tinytf-heading-forward-any))
        (point-max)
      (beginning-of-line)
      (1- (point)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-heading-number-regexp (&optional no-grouping)
  "Return heading number regexp: match 'N.n '  or  'N.n) '.
If NO-GROUPING is non-nil, the regexp will not have regexp group operator."
  (if no-grouping
      "[0-9]+\\.[0-9.]*[0-9])?[ \t]+"
    "\\([0-9]+\\.[0-9.]*[0-9])?[ \t]+\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-headings-numbered-p ()
  "Check if first heading is numbered."
  (save-excursion
    (ti::pmin)
    (re-search-forward (tinytf-heading-number-regexp) nil t)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-heading-string (&rest levels)
  "Return heading string. 'MAJOR.MINOR '."
  (let ((str (number-to-string (pop levels))))
    (dolist (nbr levels)
      (setq str (concat str "." (number-to-string nbr))))
    (concat str " ")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-goto-non-space ()
  "Goto first non-whitespace of bol."
  (beginning-of-line)
  (if (re-search-forward "^[ \t]+" (line-end-position) t)
      (goto-char (match-end 0))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-heading-same-p (level heading-regexp)
  "Check if heading LEVEL is identical to HEADING-REGEXP.
After the regexp there must be non whitespace, which starts the heading
name."
  (save-match-data
    (save-excursion
      (beginning-of-line)
      (looking-at (concat
                   "^"
                   (tinytf-indent level)
                   heading-regexp
                   ;;  After the spaces there must be NON-space to start
                   ;;  the heading name
                   "[^ \t]")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc-goto ()
  "Goto Table of contents and return t or to `point-min'."
  (ti::pmin)
  (when (re-search-forward "^Table [Oo]f [Cc]ontents[ \t]*$" nil t)
    (beginning-of-line)
    t))

;;}}}
;;{{{ Conversions

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-preference-set ()
  "Set HTML conversion preferences."
  (interactive)
  (message "tinytf-convert-preference-set is not yet implemented.")
  (sleep-for 2))

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-convert-view-macro 'lisp-indent-function 0)
(put 'tinytf-convert-view-macro 'edebug-form-spec '(body))
(defmacro tinytf-convert-view-macro (&rest body)
  "Check file `tinytf--file-last-html-generated' and run BODY."
  `(let ((file tinytf--file-last-html-generated))
     (when (or (not file)
               (not (file-exists-p file)))
       (error "TinyTf: Can't view HTML, file not available [%s]"
              (prin1-to-string file)))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-view-default ()
  "View last HTML with `browse-url'."
  (interactive)
  (tinytf-convert-view-macro
   ;; (browse-url file)
   (tinyurl-agent-funcall 'url file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-view-html-source ()
  "View last HTML with `find-file-other-window'."
  (interactive)
  (tinytf-convert-view-macro
   (find-file-other-window file)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-convert-file-name-html (file)
  "Make FILE.txt FILE.html"
  (concat (file-name-sans-extension file) ".html"))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-buffer-file-name-html-source (buffer)
  "Return filename from where to read plain text.
For files this should be `buffer-file-name', but for buffer
that are not associated with file, a temporary filename is
generated using `ti::temp-file'."
  (or (buffer-file-name buffer)
      (ti::temp-file "tinytf-temp.html" 'temp-dir)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-html-source (&optional buffer)
  "Return BUFFER's source file name. Default is `current-buffer'.
See `tinytf--buffer-file-name-html-source-function'"
  (or (funcall
       tinytf--buffer-file-name-html-source-function
       (or buffer
           (current-buffer)))
      (error "TinyTf: HTML source function failed.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-html-destinaton (&optional buffer)
  "Return BUFFER's destination file name. Default is `current-buffer'.
See `tinytf--buffer-file-name-html-destination-function'"
  (let ((file (funcall
	       tinytf--buffer-file-name-html-destination-function
	       (or buffer
		   (current-buffer)))))
    (unless file
      (error "TinyTf: HTML destination function failed %s."
             (prin1-to-string
              tinytf--buffer-file-name-html-destination-function)))
    (setq tinytf--file-last-html-generated file)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-file-name-html (file)
  "Make FILE.txt => FILE.html"
  (concat (file-name-sans-extension file) ".html"))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-buffer-file-name-html-destination (buffer)
  "Make buffer's FILE.txt => FILE.html"
  (tinytf-file-name-html (buffer-file-name buffer)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyperl-convert-binary-t2html ()
  "Return t2html.el full path."
  (let ((bin (get 'tinytf-mode 't2html)))
    (when (or (not bin)
              (not (file-exists-p bin)))
      (error
       (substitute-command-keys "\
No t2html.pl available. Run HTML=>Conversion menu re-evaluate\
 \\[tinytf-utility-programs-check-force]")))
    bin))

;;; ----------------------------------------------------------------------
;;;
(put 'tinytf-convert-wrapper-macro 'lisp-indent-function 1)
(put 'tinytf-convert-wrapper-macro 'edebug-form-spec '(body))
(defmacro tinytf-convert-wrapper-macro (temp &rest body)
  "Define some common variables in `let'.

Variables available:

 file-name   Buffer's file name or nil
 file        The filename (possibley generated) for HTML output.

Input:

  TEMP       if non-nil, write temporary buffers to disk
  BODY       rest of the lisp forms."
  `(let ((file-name (buffer-file-name))
	 (file      (tinytf-convert-html-source)))
     (when (and ,temp
                (null file-name))
       (let ((buffer (current-buffer)))
         (with-temp-buffer
           (insert-buffer-substring buffer)
           (write-region (point-min) (point-max) file))))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-call-process (&optional options process mode)
  "Convert buffer using Perl t2html.pl.
In order to use this function, `tinytf-utility-programs-check' must
have been called to set location of perl script.

The current buffer is used as source. Temporary buffers
are written on disk.

Input:

  OPTIONS   List of option to be passed to t2html.pl
            Following options are usually the minunum:
            '(\"--Out\" \"buffer-file-name.txt\")
            Nil values will be ignored.

  PROCESS   'compile  [this is default]
            'call-prosess, see also MODE

  MODE      This is only used for PROCESS 'call-process
            'noerr   = Do not display log on error.
            'display = Display the result buffer
            nil      = Dislpay on only if process printed something
                       (a possible HTML conversion error)

Return:

 '(html-file-name  status).   non-nil status is an error.
                              Status may also be compile buffer process."
  (interactive)
  (setq options (delq nil options))
  (tinytf-convert-wrapper-macro
      'temp-write
    (message "TinyTf: Generating HTML... (t2html) %s" file-name)
    ;;  This may take a while
    ;;  We feed the script to Perl and that works in every
    ;;  platform.
    (let* ((log    (get-buffer-create tinytf--buffer-html-process))
	   (target (tinytf-convert-html-destinaton))
	   (dir    (file-name-directory target))
	   (opt    (append options
			   (list
			    "--Out-dir"
			    dir
			    "--Out"
			    file)))
	   status)
      (with-current-buffer log
	(insert (format "\nTinyTf: t2html.pl run %s\n"
			(ti::date-standard-date 'minutes)))

	(insert (format "Source: %s\n" file)
		(format "Target: %s\n" target))
	(cond
	 ((eq process 'call-process)
	  (with-temp-buffer
	    (apply 'call-process "perl"
		   nil
		   (current-buffer)
		   nil
		   (tinyperl-convert-binary-t2html)
		   opt)
	    (message "TinyTf: Generating HTML...done. %s" target)
	    (cond
	     ((or (eq mode 'display)
		  (and (null mode)
		       (not (ti::buffer-empty-p))))
	      (setq status 'error)
	      (append-to-buffer log (point-min) (point-max))
	      (display-buffer log))
	     ((eq mode 'noerr)
	      nil))))
	 (t
	  (let ((command (concat
			  "perl "
			  (tinyperl-convert-binary-t2html)
			  " "
			  " --print-url "
			  (ti::list-to-string opt))))
	    (tinytf-compile command
		tinytf--process-compile-html)
	    ;;  Turn on URL recognizer so that lines can be clicked
	    (with-current-buffer
		(get-buffer
		 (format "*%s*"
			 tinytf--process-compile-html))
	      (run-hooks 'tinytf--process-compile-hook))
	    (message "TinyTf: Generating HTML... compile. %s"
		     target))))
	(ti::append-to-buffer
	 log
	 (format "\nEnd: %s\n"
		 (ti::date-standard-date 'minutes)))
	(list target status)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytf-convert-extra-options ()
  "If there is #t2html-* tag, return list of additional options."
  (when (ti::re-search-check "#t2html-")
    '("--Auto-detect")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-t2html-basic ()
  "Make 1-page HTML."
  (interactive)
  (tinytf-convert-call-process
   (tinytf-convert-extra-options)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-t2html-frame ()
  "Make HTML with frames using t2html.pl"
  (interactive)
  (tinytf-convert-call-process
   (append
    (tinytf-convert-extra-options)
    (list "--html-frame"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-t2html-as-is ()
  "Make HTML with frames using t2html.pl"
  (interactive)
  (tinytf-convert-call-process '("--as-is")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-t2html-link-check (&optional options)
  "Call t2html.pl to check links with OPTIONS."
  (interactive)
  (tinytf-convert-wrapper-macro 'temp-write
    (let ((command-args
	   (concat "perl "
		   (tinyperl-convert-binary-t2html)
		   " "
		   (or options
		       "--link-check --quiet")
		   " "
		   file)))
      ;;  compile-internal:
      ;;    command
      ;;    error-message
      ;;    &optional
      ;;    name-of-mode
      ;;    parser
      ;;    error-regexp-alist
      ;;    name-function
      ;;    enter-regexp-alist
      ;;    leave-regexp-alist
      ;;    file-regexp-alist
      ;;    nomessage-regexp-alist
      (tinytf-compile
	  command-args
	  tinytf--process-compile-html
	  grep-regexp-alist))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-t2html-link-check-cached ()
  "Call t2html.pl to check links by using `tinytf--t2html-link-cache-file'."
  (interactive)
  (unless (stringp tinytf--t2html-link-cache-file)
    (error "Tinytf: `tinytf--t2html-link-cache-file' must contain filename."))
  (tinytf-convert-t2html-link-check
   (format "--link-check --quiet --Link-cache %s"
           tinytf--t2html-link-cache-file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-compile-mode-settings ()
  "Install font lock and additional keybindings for Link check."
  ;; #todo: font-lock
  (let ()))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-convert-htmlize ()
  "Convert buffer using Perl htmlize.el"
  (interactive)
  (unless (fboundp 'htmlize-buffer)
    (error "TinyTf: `htmlize-buffer' is not available. You need htmlize.el"))
  (let* ((path (get 'tinytf-mode 'htmlize))
         (dir  (file-name-directory path))
         (elc  (concat dir "htmlize.elc")))
    (unless (file-exists-p elc)
      (message "TinyTf: (performance) Please compile %s" path)
      (sit-for 0.5)))
  (let ((html-buffer "*html*") ;; Thisis in htmlize.el, but hard coded
	(buffer      (current-buffer)))
    ;; Prevent multple *html* buffers.
    (if (get-buffer html-buffer)
        (kill-buffer (get-buffer html-buffer)))
    (tinytf-convert-wrapper-macro nil
      (message "TinyTf: Generating HTML... (htmlize) %s" file-name)
      (ti::funcall 'htmlize-buffer)
      (with-current-buffer html-buffer
	(write-region
	 (point-min)
	 (point-max)
	 (tinytf-convert-html-destinaton buffer)))
      (message "TinyTf: Generating HTML...done. %s"
	       (tinytf-convert-file-name-html file)))))

;;}}}
;;{{{ misc:

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-utility-programs-check (&optional force verb)
  "Set or disable found utilities (HTML converters). FORCE check.
The settings will affect the main drop-down menu. If you install
htmllize.el or t2html.pl to the system after package ais loaded,
you must run this function.

Reference:

  See (symbol-plist 'tinytf-mode)."
  (interactive (list t t))
  (let ((sym 'tinytf-mode)
	;; Affects expand-file-name to use only / in XEmacs
	(path-separator ?/))
    (when (or force
              (null (get sym 't2html-checked)))
      (let* ((path  (progn
                      ;;  Print messages because searching PATH may take
                      ;;  some time
                      (when verb
                        (message "Tinytf: searching t2html.pl..."))
                      (prog1
                          (ti::file-get-load-path "t2html.pl" exec-path)
                        (when verb
                          (message "Tinytf: searching t2html.pl... Done.")))))
             ;;  convert to use only forward slashes.
             (path2 (or tinytf--binary-t2html path))
             (bin   (and path2
                         (file-exists-p path2)
                         (expand-file-name path2))))
        (cond
         (bin
          (put sym 't2html-checked t)
          (put sym 't2html bin))
         (verb
          (message "Tinytf: Cannot find t2html.pl along `exec-path'.")))))
    (when (or force
              (null (get sym 'htmlize-checked)))
      (let ((path (locate-library "htmlize.el")))
        (cond
         (path
          (put sym 'htmlize-checked t)
          (put sym 'htmlize path))
         (verb
          (message "Tinytf: Cannot find htmlize.pl along `load-path'.")))))
    (list
     (get sym 'htmlize)
     (get sym 't2html))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-utility-programs-check-force ()
  "Re-evaluate menu and find new conversion programs.
The menu items are greyed out if the conversion programs were no
available during startup. If you later install the conversion programs
either to PATH or `load-path', run this function to enable menu
selections.

This function calls `tinytf-utility-programs-check' with 'force."
  (interactive)
  (tinytf-utility-programs-check 'force 'verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-fontify-current-buffer-window ()
  "Fontify current buffer's window."
  (let* ((buffer (current-buffer))
	 (win    (get-buffer-window buffer)))
    (when (and win
               (window-live-p win)
               (or font-lock-mode
                   global-font-lock-mode))
      (select-window win)
      (font-lock-fontify-region (window-start)
                                (min (point-max) (window-end))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-untabify-buffer ()
  "Untabify whole buffer."
  (interactive "*")
  (when tinytf-mode
    (untabify (point-min) (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-column-info ()
  "Return column intepretation."
  (interactive)
  (let* ((col (current-column))
         (elt (assq col tinytf--column-table))
         list
         re
         ret)
    (when elt
      (setq list (nth 1 elt))
      (dolist (elt list)
        (setq re (car elt))
        (cond
         ((not (stringp re))            ;Stop there
          (setq ret (nth 1 elt))
          (return))
         ((looking-at re)               ;found match ?
          (setq ret (nth 1 elt))
          ;; Yes, stop there
          (return)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-column-info-display (&optional suppress)
  "Display column info.
This function displays information about the column and the text
following it. Function does not look around the text and it will not
detect text inside bullets or inside any other complex text.
E.g.

        o   *Bullet here...
            Continues here

Cursor is at [*], which is column 12, which will indicate that the column
is used for code examples and marked with <SAMPLE>.

Input:
  SUPPRESS  If non-nil, do no display unneeded messages."
  (interactive)
  (let ((string (tinytf-column-info))
        (col    (current-column)))
    (if string
        (message "TinyTf: %s (col %d)" string col)
      (unless suppress
        (message "TinyTf: Nothing special at column %d" col)))))

;;}}}
;;{{{ headings

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-positions (&optional and-heading-names)
  "Read heading sections forward.

Input:

 AND-HEADING-NAMES  see return value.

Return:

 '(pos pos ..)
 '((heading pos) (heading pos) ..) If AND-HEADING-NAMES is non-nil
 nil"
  (let (point
	heading
	list)
    (save-excursion
      (tinytf-heading-macro
       (setq point (line-beginning-position))
       (cond
        (and-heading-names
         ;;  Dont read trailing spaces.
         (end-of-line)
         (if (re-search-backward "[^ \t]+" point t)
             ;; Search 'eats' one character include it too.
             (setq heading (buffer-substring point (1+ (point))))
           (setq heading (buffer-substring point (point))))
         (push (cons  heading (point)) list))
        (t
         (push (point) list)
         (end-of-line)))))
    ;;  Add point max there too. (due to hide region)
    (if (and list (null and-heading-names))
        (push (point-max) list))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-fix-case-all (&optional confirm verb)
  "Convert all headings to lowercase and capitalize first word.
If CONFIRM is non-nil ask permission to fix for each heading. VERB."
  (interactive "*P")
  (ti::verb)
  (tinytf-heading-macro
   (when (and (tinytf-heading-fix-case-p)
              (or (and
                   confirm
                   (y-or-n-p (format
                              "Fix? %s"
                              (ti::string-left
                               (ti::read-current-line) 70))))
                  (null confirm)))
     (downcase-region (line-beginning-position) (line-end-position))
     (capitalize-word 1))
   (end-of-line))
  (if verb
      (message "TinyTf: Case fix done.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-fix-case-p ()
  "Check if current heading need case fixing.
Caller must ensure that current line is heading. Point is moved."
  (beginning-of-line)
  (if (looking-at "[0-9. \t]+")         ;Forget heading numbering
      (re-search-forward "[0-9. \t]+" (line-end-position)) )

  (or (and (looking-at "[a-z]")         ;It alphabet
           (let (case-fold-search)     ;is it uppercase?
             (not (looking-at "[A-Z]"))))
      (save-excursion           ;Howabout rest of the line; lowercase?
        (forward-char 1)
        (let (case-fold-search)
          (looking-at ".*[A-Z]")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-fix-case ()
  "Write heading in lowercase and capitalize first word. Move heading forward.
If current line is not a heading, do nothing.

Return:

  t         if modified current and moved to next heading.
  nil"
  (interactive "*")
  (let ((re  (concat (tinytf-regexp) "\\|" (tinytf-regexp 1)))
	ret)
    (when (save-excursion
            (beginning-of-line)
            (looking-at re))
      (when (tinytf-heading-fix-case-p)
        (downcase-region (line-beginning-position) (line-end-position))
        (capitalize-word 1)
        (setq ret t)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-fix-newlines (&optional verb)
  "Search all headings and remove extra newlines. VERB.

Return:

  nil       nothing fixed
  nbr       this many fixed."
  (interactive "*")
  (let  ((mark   (point-marker))        ;Heading start point
         (fix    0)
         beg)
    (ti::verb)
    (tinytf-heading-macro
     (move-marker mark (point))
     (forward-line  -1)
     (setq beg (point))
     ;;  Delete previous newlines
     (unless (bobp)
       (cond
        ((not (looking-at "^[ \t]*$"))  ;no whitespace at all
         (end-of-line) (insert "\n") (incf  fix))
        ((not (zerop (skip-chars-backward " \t\r\n"))) ;extra whitespace
         (forward-line 1)
         (delete-region beg (point)) (incf  fix))))
     (goto-char (marker-position mark))
     (forward-line 1)
     (setq beg (point))
     (unless (eobp)
       (cond
        ((not (looking-at "^[ \t]*$"))  ;no whitespace at all
         (beginning-of-line)
         (insert "\n")
         (incf  fix))
        ((not (zerop (skip-chars-forward " \t\r\n"))) ;extra whitespace
         (beginning-of-line)

         ;;   Skip chars forward for to next line in following case,
         ;;   see the (*) cursor position. When we do beginning of
         ;;   line; the point is 1+ BEG, but we really don't want
         ;;   to delete that region!
         ;;
         ;;   Header 1
         ;;
         ;;     (*)Sub header 2
         (when (not (memq (point) (list beg (1+ beg))))
           (delete-region beg (point))
           (incf  fix))))))
    (setq mark nil)                     ;kill marker
    (if verb
        (if (> fix 0)
            (message "TinyTf: fixed %d places around headings" fix)
          (message "TinyTf: no newline fixes.")))

    (if (> fix 0)                       ;anything to return?
        fix)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-fix (&optional verb)
  "Search all headings and convert first letter to uppercase if needed. VERB.
This function will only chnage the first word in the heading, no
other case conversions are done."
  (interactive "*P")
  (let ((re  (concat                   ;Search lower letters only
	      "^" (tinytf-heading-number-regexp) "?"
	      "[a-z]"

	      "\\|^" (tinytf-indent 1)
	      (tinytf-heading-number-regexp) "?"
	      "[a-z]"))
	(count  0)
	case-fold-search)
    (ti::verb)
    (save-excursion
      (ti::pmin)
      (while (re-search-forward re nil t)
        (backward-char 1)
        (capitalize-word 1)
        (incf  count)))
    (if verb
        (message "%s: Fixed %d headings" tinytf--mode-name count))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-numbering (&optional remove verb)
  "Number heading levels. Old numbering is replaced.
Optionally REMOVE numbering. VERB."
  (interactive "*P")
  (let* ((re-no-nbr2 (if (> tinytf--heading-number-level 0)
                         (tinytf-regexp 1)))
         (re-no-nbr3 (if (> tinytf--heading-number-level 1)
                         (tinytf-regexp 2)))
         (re-nbr    (tinytf-heading-number-regexp))
         (re1       (concat "^" (tinytf-indent 0) re-nbr))
         (re2       (if (> tinytf--heading-number-level 0)
                        (concat "^" (tinytf-indent 1) re-nbr)))
         (re3       (if (> tinytf--heading-number-level 1)
                        (concat "^" (tinytf-indent 2) re-nbr)))
         (c1        0)                  ;counters
         (c2        0)
         (c3        0)
         (count     0)
         (fix       0)
         str)
    (ti::verb)
    (tinytf-heading-macro
     (incf  count)
     (beginning-of-line)
     (cond
      (remove
       (if (or (looking-at re1)
               (and re2
                    (looking-at re2))
               (and re3
                    (looking-at re3)))
           (ti::replace-match 1)))
      ((looking-at re1)                 ;heading 1
       (incf c1)
       (setq c2 0)
       (setq str (tinytf-heading-string c1 c2))
       ;;  Only change if different, this prevents buffer modify flag
       ;;  change
       (unless (tinytf-heading-same-p 0 str)
         (incf  fix)
         (ti::replace-match 1 str)))
      ((and re2 (looking-at re2))
       (incf c2)
       (setq c3 0)
       (setq str (tinytf-heading-string c1 c2))
       (unless (tinytf-heading-same-p 1 str)
         (incf  fix)
         (ti::replace-match 1 str)))
      ((and re3 (looking-at re3))
       (incf c3)
       (setq str (tinytf-heading-string c1 c2 c3))
       (unless (tinytf-heading-same-p 2 str)
         (incf  fix)
         (ti::replace-match 1 str)))
      ((and re-no-nbr2
            (looking-at re-no-nbr2))    ;Level 2
       (goto-char (match-end 0))
       (backward-char 1)
       (incf  c2)
       (insert (tinytf-heading-string c1 c2)))
      ((and re-no-nbr3
            (looking-at re-no-nbr3))    ;Level 2
       (goto-char (match-end 0))
       (backward-char 1)
       (incf  c3)
       (insert (tinytf-heading-string c1 c2 c3)))
      ((looking-at "^")                 ;must be level 1
       (setq c2 0)     (incf  c1)
       (incf  fix)
       (insert (tinytf-heading-string c1 c2))))
     (end-of-line))
    (when verb
      (message "%s: %d/%d headings numbered."
               tinytf--mode-name fix count))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-fix-all (&optional verb)
  "Fix headers, untabify buffer and do other things. VERB.

Note:

  This function does not call following functions:

  `tinytf-heading-fix-case-all', because you are in charge of the case of
  headings. Only first letter is made uppercase.

  `tinytf-toc', because the TOC in the buffer would always be substituted.
  and this would mark buffer modified although no other changes were made.
  Update this manually after heading changes.

References:

  `tinytf--fix-all-hook'   Well, you can do the misisng things here."
  (interactive "*")
  (ti::verb)
  ;;  when you fill paragraphs, the default Emacs has nasty habits to
  ;;  end the sentece to two spaces, which is some idiot way to separate
  ;;  two sentences (the history says that the typewriter fonts were so bad
  ;;  that the typists needed to add two spaces these).
  ;;
  ;;  Fix them to have only standard space.
  (if verb
      (message "TinyTf: Fixing double spaces..."))
  (save-excursion
    (ti::pmin)
    (while (re-search-forward "[a-zA-Z]+[!?.,]\\(  +\\)[a-zA-Z]+" nil t)
      (ti::replace-match 1 " ")))
  (if verb
      (message "TinyTf: Trimming extra blank lines..."))
  (ti::buffer-trim-blanks (point-min) (point-max)) ;Remove trailing blanks
  (if verb
      (message "TinyTf: Heading fix..."))
  (tinytf-heading-fix)
  (tinytf-untabify-buffer)
  (tinytf-heading-fix-newlines)
  (if verb
      (message "TinyTf: Checking heading numbering..."))
  (when (tinytf-headings-numbered-p)
    (tinytf-heading-numbering))
  (if verb
      (message "TinyTf: Running user fix hooks..."))
  (run-hooks 'tinytf--fix-all-hook)
  (if verb
      (message "TinyTf: Fixing done.")))

;;}}}
;;{{{ text: formatting with codes

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-paragraph-first-line-indent-fix (col)
  "Make sure the first line in the paragraph start at column COL.
point must be inside paragraph before calling function."
  ;;  After fill; the first line may still be ragged. fix it
  ;;
  ;;      txt txt txt txt
  ;;  txt txt txt txt txt
  ;;  txt txt txt txt txt
  (if (string-match "^[ \t]$" (ti::read-current-line))
      (error "Must be inside text."))
  (when (re-search-backward "^[ \t]*$" nil t)
    (forward-line 1)
    (skip-chars-forward " \t")
    (unless (eq col (current-column))
      (delete-region (point) (line-beginning-position))
      (insert (make-string col ?\ )))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-move-paragraph-to-column (beg end col &optional msg noask)
  "If region BEG END is big, ask confirmation for COL move with MSG NOASK.
Return
  (beg . end)    Region bounds now."
  (let ((lines (+ (count-char-in-region beg end ?\r)
		  (count-char-in-region beg end ?\n)))
	(max   40))
    ;;  We use markers, because the points have moved after the
    ;;  call to ti::buffer-move-paragraph-to-column. We must pass moved BEG end
    ;;  to hook functions.
    (ti::keep-lower-order beg end)
    (save-excursion
      (goto-char beg)
      (setq beg (point-marker))
      (goto-char end)
      (setq end (point-marker)))
    (when (or noask
              (< lines (1+ max))
              (and
               (> lines max)
               (y-or-n-p (format "%s: really move %d lines? "
                                 tinytf--mode-name lines))))
      (ti::buffer-move-paragraph-to-column
       (marker-position beg) (marker-position end)
       col)
      (if (and (null noask) (< col 12))
          (let ((fill-prefix (make-string col ?\ ))
                (left-margin 0))
            (call-interactively 'fill-paragraph)))
      (if msg
          (message "TinyTf:%s" msg))

      (run-hook-with-args 'tinytf--move-paragraph-hook
                          (marker-position beg) (marker-position end))
      (cons beg end))))

;;; ----------------------------------------------------------------------
;;;
(tinytf-fmacro-indent-region
 tinytf-indent-region-text
 "Move selected region to STRONG html code position."
 8 (format "TinyTf: Regular text, column 8"))

(tinytf-fmacro-indent-region
 tinytf-indent-region-strong
 "Move selected region to STRONG html code position."
 9 (format "TinyTf: Strong, column 9"))

(tinytf-fmacro-indent-region
 tinytf-indent-region-quote
 "Move selected region to EMPHATISED (quoted text) html code position."
 10 (format "TinyTf: Quotation, column 10"))

(tinytf-fmacro-indent-region
 tinytf-indent-region-sample
 "Move selected region to SAMPLE (example code) html code position."
 12 (format "TinyTf: Sample, column 12"))

;;; ----------------------------------------------------------------------

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-zero
 "Move paragraph to column 0."
 0 (format "TinyTf: Zero, column 0"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-2
 "Move paragraph to column 2."
 2 (format "TinyTf: column 2"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-3
 "Move paragraph to column 3."
 3 (format "TinyTf: column 3"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-5
 "Move paragraph to column 5."
 5 (format "TinyTf: column 5"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-6
 "Move paragraph to column 6."
 6 (format "TinyTf: column 6")
 (progn
   (fill-paragraph nil)
   (tinytf-forward-paragraph)))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-11
 "Move paragraph to column 11."
 11 (format "TinyTf: column 11"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-text
 "Move paragraph to text position."
 8 (format "TinyTf: text, column 8")
 (progn
   (tinytf-forward-paragraph)))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-text-as-is
 "Move paragraph to text position."
 8 (format "TinyTf: text, column 8")
 (progn
   (tinytf-forward-paragraph)))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-text-and-fill
 "Move paragraph to text position and fill."
 8 (format "TinyTf: text, column 8")
 (let ((fill-prefix
	(or (ti::string-match "^[ \t]+" 0 (ti::read-current-line))
	    ""))
       (left-margin 0))
   (fill-paragraph nil)
   (tinytf-paragraph-first-line-indent-fix 8)
   (tinytf-forward-paragraph)))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-quote
 "Move paragraph to Quotation position."
 10 (format "TinyTf: Quotation, column 10"))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-quote-and-fill
 "Move paragraph to Quotation position and fill."
 10 (format "TinyTf: Quotation, column 10")
 (let ((fill-prefix (or (ti::string-match
			 "^[ \t]+"
			 0
			 (ti::read-current-line))
			""))
       (left-margin 0))
   (fill-paragraph nil)
   (tinytf-paragraph-first-line-indent-fix 10)
   (tinytf-forward-paragraph)))

(tinytf-fmacro-indent-paragraph
 tinytf-indent-paragraph-sample
 "Move paragraph to Sample code position."
 12 (format "TinyTf: Sample, column 12")
 (progn
   (tinytf-forward-paragraph)))

;;; ----------------------------------------------------------------------
;;;
(tinytf-fmacro-mark-word
 tinytf-mark-word-sample
 "Put 'SAMP' code around word and move forward." ?` ?' )

(tinytf-fmacro-mark-word
 tinytf-mark-word-emp
 "Put 'EM' code around word and move forward." ?* )

(tinytf-fmacro-mark-word
 tinytf-mark-word-strong
 "Put 'STRONG' code around word and move forward." ?_ )

(tinytf-fmacro-mark-word
 tinytf-mark-word-big
 "Put 'BIG' code around word and move forward." ?+ )

(tinytf-fmacro-mark-word
 tinytf-mark-word-small
 "Put 'SMALL' code around word and move forward." ?= )

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-unmark-word ()
  "Remove 'STRONG' and 'EMP' mark from word and move forward."
  (interactive "*")
  (let ((word-skip "^ ,.;\n\r\t\f")
	beg
	end)
    (flet ((marker   (beg skip)
                     (save-excursion
                       (skip-chars-forward skip)
                       (list
                        (buffer-substring beg (point))
                        (point))))
           (markup-p (word)
                     (string-match "[_*='`]+\\([^_*='`]+\\)[_*='`]+$" word)))
      (unless (looking-at "[ \t\f]")
        (skip-chars-backward "^ ,\n\r\t\f")
        (setq beg (point))
        ;;  It depends how the markup has been done:
        ;;
        ;;      This _sentence._ And new sentence.
        ;;      This _sentence_. And new sentence.
        (dolist (try (list "^ \n\r\t\f" word-skip))
          (multiple-value-bind (word end)
              (marker beg try)
            (when (string-match "[_*='`]+\\([^_*='`]+\\)[_*='`]+$" word)
              (setq word (match-string 1 word))
              (delete-region beg end)
              (insert word)
              (setq end (point))
              (return))))
        (unless end
          (skip-chars-forward word-skip))
        (skip-chars-forward " (){}<>,.;:!?\"\'\n\r\t\f")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-mark-br-line (&optional unmark &optional verb)
  "Mark current line with symbolic <BR>. Optionally UNMARK. VERB."
  (interactive "*P")
  (let (point
	ch)
    (ti::verb)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at "[ \t]+[^\r\n]"))
          (if verb
              (message "TinyTf: There must be empty spaces before line."))
        (setq point (match-end 0))))
    (when point
      (goto-char (1- point))
      (setq ch (following-char))
      (if unmark
          (if (char-equal ch ?.)
              (delete-char 1))
        (if (char-equal ch ?.)
            (if verb
                (message "TinyTf: Already marked as <BR>"))
          (insert "."))))
    (forward-line 1)
    (skip-chars-forward " \t\r\n")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-mark-br-paragraph (&optional unmark)
  "Mark current paragraph with symbolic <BR> codes. Optionally UNMARK."
  (interactive "*P")
  (let (beg
	end-mark)
    (tinytf-backward-paragraph)
    (setq beg (point))
    (tinytf-forward-paragraph)
    (setq end-mark (point-marker))
    (goto-char beg)
    (while (< (point) (marker-position end-mark))
      (tinytf-mark-br-line unmark))
    ;; Kill marker
    (setq end-mark nil)))

;;}}}
;;{{{ Formatting, misc

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-bullet-format ()
  "Reformat following bullet into Technical text bullet.
Point sits somewhere in current bullet. Bullets accepted are:

    . text here
    + text here
    * text here
    - text here
    o text here

If the bullet strarts with 'o' or '.', then that is used as bullet
mark. In all other cases 'o' is used."
  (interactive "*")
  (let* ((bullet-re     "^[ \t]*\\([-*o.+]\\)[ \t]")
         (para-re       (concat bullet-re "\\|^[ \t]*$"))
         (left-margin   0)
         (char          "o")
         (str           "            ") ;; 8 + 4 spaces
         fill-prefix ;; Otherwise formatting won't work right
         beg
         end)
    (if (not (looking-at bullet-re))
        (re-search-backward para-re))
    (beginning-of-line)
    (if (not (looking-at bullet-re))
        (re-search-forward bullet-re))
    ;;   Set the bullet character
    (setq char (match-string 1))
    (if (not (member char '("o" ".")))
        (setq char "o"))
    (goto-char (match-end 0))           ;Over the bullet marker
    ;;  Delete [WHITE-SPACE]-   text
    (delete-region (point) (line-beginning-position))
    (setq beg (point))
    ;;  handle continuing line
    ;;
    ;;  -   text
    ;;      text text
    (while (and (not (looking-at bullet-re))
                (looking-at "[ \t]*[^ \t\r\n]"))
      (fixup-whitespace)
      (forward-line 1))
    (setq end (point))
    (ti::narrow-safe beg end
      ;;  Now indent the text, then fill, and finally fix the
      ;;  bullet start
      (goto-char (point-min))
      (setq fill-prefix str)
      (indent-rigidly (point-min) (point-max) (+ 8 4))
      (call-interactively 'fill-paragraph)
      (goto-char (point-min))
      (fixup-whitespace)
      (insert "        " char "   ")
      (setq end (point-max)))
    (goto-char end)
    ;;  Next paragraph
    (if (string-match "^[ \t\r\n]*$" (ti::read-current-line))
        (re-search-forward "^[ \t]*[^ \t\r\n]" nil t))))

;;}}}
;;{{{ movement

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-backward-paragraph ()
  "Like `tinytf-forward-paragraph' but go backward."
  (interactive)
  (tinytf-paragraph-macro
   (backward-paragraph)
   (backward-paragraph)
   (skip-chars-forward " \t\r\n")
   (tinytf-goto-non-space)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-forward-paragraph ()
  "Like `forward-paragraph' but keep cursor at the beginning of text."
  (interactive)
  (tinytf-paragraph-macro
   (forward-paragraph)
   (skip-chars-forward " \t\r\n")
   (tinytf-goto-non-space)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-start ()
  "Goto first heading, excluding TOC heading."
  (interactive)
  (when (tinytf-toc-goto)
    (tinytf-heading-forward-0)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-forward (level &optional back any)
  "Go to next heading. Optionally search LEVEL or BACK or ANY level.

Return:

 nbr    point if moved
 nil"
  (let ((re (if any
		(concat (tinytf-regexp) "\\|" (tinytf-regexp 1))
	      (tinytf-regexp level)))
	case-fold-search               ;case sensitive
	point)
    (if back
        (when (re-search-backward re nil t)
          (skip-chars-forward " \t") (point))
      (cond
       ((bolp)                         ;Startt seaching from next char
        (save-excursion
          (forward-char 1)
          (if (re-search-forward re nil t)
              (setq point (point))))
        (if point                       ;If search ok, then move
            (goto-char (1- point))))
       (t
        (when (re-search-forward re nil t)
          (backward-char 1) (point)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-heading-forward-0 ()
  "Forward."
  (interactive)
  (tinytf-forward 0))

(defun tinytf-heading-forward-1 ()
  "Forward."
  (interactive)
  (tinytf-forward 1))

(defun tinytf-heading-forward-any ()
  "Forward."
  (interactive)
  (tinytf-forward 1 nil 'any))

(defun tinytf-heading-backward-0 ()
  "Backward."
  (interactive)
  (tinytf-forward 0 'back))

(defun tinytf-heading-backward-1 ()
  "Backward."
  (interactive)
  (tinytf-forward 1 'back))

(defun tinytf-heading-backward-any ()
  "Backward."
  (interactive)
  (tinytf-forward 1 'back 'any))

;;}}}
;;{{{ outline control: show/hide

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-hide-region (beg end &optional show)
  "Hide region BEG END with selective display. Optionally SHOW.
Point is END after function finishes."
  (let (buffer-read-only
        (ch1   (if show ?\r ?\n))
        (ch2   (if show ?\n ?\r)))
    (subst-char-in-region beg end ch1 ch2)
    (set-buffer-modified-p nil)
    (goto-char (max beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-mouse-context-sensitive (event)
  "If `mouse-point' points indent 0 or 1 line, then hide/show level. EVENT.
In other places call original function."
  (interactive "e")

  (mouse-set-point event)
  (beginning-of-line)
  (cond
   ((or (tinytf-level-p 0)
        (tinytf-level-p 1))
    (tinytf-show-toggle))
   (t
    (ti::compat-mouse-call-original 'tinytf-mode event))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-show-buffer ()
  "Remove selective display codes from buffer."
  (interactive)
  (save-excursion
    (tinytf-hide-region (point-min) (point-max) 'show)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-hide-buffer ()
  "Hide whole buffer."
  (interactive)
  ;;  Save current point
  (let ((point (line-beginning-position)))
    (goto-char (point-min))
    (if (tinytf-level-p 0)
        (tinytf-hide))
    (while (tinytf-heading-forward-0)
      (tinytf-hide))
    ;;  But after collapsing buffer, the point is not exactly there
    ;;  any more => Go to the nearest heading which is at the beginning of
    ;;  line.
    (goto-char point)
    (goto-char (line-beginning-position))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-show-toggle ()
  "Open/close level 1. Does't touch level 0."
  (interactive)
  (let ((point (point)))
    (if (looking-at ".*\r")
        (tinytf-show)            ;level is already collapsed, open it.
      (tinytf-hide))
    (goto-char point)
    (tinytf-goto-non-space)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-show ()
  "Show current level."
  (interactive)
  (tinytf-hide 'show))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-hide (&optional show)
  "Hide current level. Optionally SHOW."
  (interactive "P")
  (let ((ok    t)
	(point (point)))
    (tinytf-level-macro
     (cond
      ((eq 1 level)
       (tinytf-hide-region (point) (tinytf-block-end) show))
      (t
       (tinytf-hide-region (point) end show)
       (setq ok (tinytf-heading-forward-any))
       (while (and ok
                   ;;  Until next level 0 ...
                   (not (eq 0 (tinytf-level-number))))
         (setq beg (point))
         (cond
          ((setq ok (tinytf-heading-forward-any))
           (beginning-of-line) (backward-char 1)
           (if show
               (subst-char-in-region beg (point) ?\r ?\n)
             (subst-char-in-region beg (point) ?\n ?\r))
           (forward-char 1))
          (t
           (tinytf-hide-region beg (point-max)))))
       (set-buffer-modified-p nil)
       (goto-char point))))))

;;}}}
;;{{{ misc: toc, exit

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc-p ()
  "Check if there is heading \"Table of contents\".
Return:
  (beg . end)    begin toc body, end of toc body
  nil"

  (let ((re  (concat                   ;May be Heading 1 or 2
	      "^\\("
	      (tinytf-indent 1)
	      "\\)?Table [Oo]f [Cc]ontents[ \t]*$"))
	case-fold-search               ;is sensitive
	beg
	end)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward re nil t)
        (forward-line 1) (setq beg (point))
        ;;  - If the above command doesn't move; assume that the Toc
        ;;    has been placed to the end of buffer.
        ;;  - maybe there is nothing more that toc or toc is at
        ;;    the end where there is no more headings
        (if (null (tinytf-heading-forward-any)) ;Not moved?
            (ti::pmax))                 ;go to end of buffer then
        (beginning-of-line)
        (setq end (point))
        (cons beg end)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc-mouse (event)
  "Create heading x-popup with mouse EVENT."
  (interactive "e")
  (tinytf-toc event))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc-occur ()
  "Generate Heading  occur menu."
  (interactive)
  (save-excursion
    (ti::pmin)
    (let ((toc  (tinytf-toc-p)))        ;Skip TOC
      (if toc
          (goto-char (cdr toc))))
    (occur "^[^ \t\r\n<#]\\|^    [^ \t\r\n]")))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc-insert-list (list)
  "Insert list of Table of Contents strings in LIST."
  (dolist (elt list)
    (setq elt (car elt))
    (if (string-match "^[ \t]+" elt)
	(insert elt "\n")       ;sub heading...
      (insert "\n" elt "\n")))
  ;; Final newline
  (insert "\n"))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-toc (&optional arg verb)
  "Create table of contents.
If there is heading level 1 whose name is \"Table of Contents\",
update that. If there is no such heading, then show toc in separate
buffer

Input ARG can be:

 nil        create toc, possibly to separate buffer
 \\[universal-argument]      create toc occur
 other      must be mouse event, create x-popup toc

VERB enables verbose messages."
  (interactive "P")
  (let ((hlist     (tinytf-heading-positions 'strings))
	(toc       (tinytf-toc-p))
	(buffer    tinytf--buffer-heading)
	elt)
    (ti::verb)
    (if (null hlist)
        (if verb
            (message "TinyTf: No headings found"))
      (cond
       ;; ..................................... generate toc to (text) ...
       ((null arg)
        (setq buffer (ti::temp-buffer buffer 'clear))
        (with-current-buffer buffer
	  (tinytf-toc-insert-list hlist)
          (ti::pmin)
          (if (stringp tinytf--heading-ignore-regexp-form)
              (flush-lines tinytf--heading-ignore-regexp-form))
          ;; Make sure there are two newlines at the end so that
          ;; inserted TOC is positioned nicely
          ;; (ti::pmax)
          ;; (when (and (looking-at "^$")
          ;;            (save-excursion
          ;;              (forward-line -1)
          ;;              (not (looking-at "^$"))))
          ;;   (insert "\n"))
          ;;  Delete leading whitespace
          ;;  1997-08 Disabled for now and now makes:
          ;;
          ;;  1.1
          ;;    1.2
          ;;  2.0
          ;;    2.1
          ;;
          (when (and toc nil)
            ;;  Convert heading 2 level to heading  1
            (ti::pmin)
	    (let ((str (tinytf-indent 1)))
	      (while (search-forward str nil 'noerr)
		(replace-match "" nil t))))
          (ti::pmin)
          (delete-region
           (point)
           (progn
             (if (zerop (skip-chars-forward " \t\r\n"))
                 (point)
               (1- (point)))))
          ;; Make indentation to text column
          (when toc
            (string-rectangle
             (point-min)
             (point-max)
             (tinytf-indent 2))) ;; with-current
	  (ti::buffer-trim-blanks (point-min) (point-max)))
        (cond
         (toc                           ;Update existing toc
          (barf-if-buffer-read-only)
          (delete-region (car toc) (cdr toc))
          (ti::save-with-marker-macro
            ;;  Leave one empty line
            (goto-char (car toc))
            (insert-buffer-substring buffer)))
         (t                             ;No previous toc
          (when verb
            (pop-to-buffer buffer)
            (ti::pmin)))))              ;end cond inner
       ;; ......................................... create toc (occur) ...
       ((equal arg '(4))
        (ti::occur-macro (concat (tinytf-regexp) "\\|" (tinytf-regexp 1))))
       ;; ......................................... create toc (mouse) ...
       (t
        ;; ARG must be mouse-event
        (if (null (ti::compat-window-system))
            (message "TinyTf: Window system required to use popup menu")
          (when (setq elt
                      (cond
                       ((< (length hlist)  20)
                        (ti::compat-popup
                         (nreverse (mapcar 'car hlist))
                         arg nil "Headings"))
                       ((fboundp 'imenu--mouse-menu)
                        ;; It's too long to be displayed in one x-widget.
                        ;; Use imenu
                        (car-safe (ti::funcall
                                   'imenu--mouse-menu hlist arg)))
                       (t
                        (message
                         "Tinytf: X-popup not available, no imenu.el")
                        nil)))
            (let* ((ret (assoc elt hlist))
                   (pos (cdr-safe ret)))
              (if pos
                  (goto-char pos)
                (ti::pmin)
                (tinytf-toc-goto)
                (tinytf-heading-forward-0)
                (re-search-forward elt))))))))
    (if verb
        (message "TinyTf: TOC generated."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytf-exit ()
  "Run `tinytf-fix-all' and exit mode."
  (interactive)
  (tinytf-fix-all)
  (tinytf-mode 0))

;;}}}

(unless tinytf--mode-define-keys-hook ;; Set default setup
  (ti::add-hooks
   'tinytf--mode-define-keys-hook
   '(tinytf-mode-define-keys tinytf-mode-define-f-keys)))

;; It's important that this is as fast as possible

(ti::byte-compile-defun-maybe '(tinytf-code-p))

(tinytf-install)

(provide   'tinytf)
(run-hooks 'tinytf--load-hook)

;;; tinytf.el ends here
