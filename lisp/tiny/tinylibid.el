;;; tinylibid.el --- Library for (Id)entifying buffer, regardless of mode

;; This file is not part of Emacs

;; Copyright (C) 1995-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
;;
;; To get information on this program, call M-x ti::id-version.
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
;;      (require 'tinylibid)
;;
;; Or use autoload. This is preferred method
;;
;;      (autolad 'ti::id-info "tinylibid" "Buffer info" t)
;;
;; Function to call to get buffer identity. You normally call this
;; from lisp code and not interactively.
;;
;;      M-x ti::id-info
;;
;; If you have any questions, always use function
;;
;;      M-x ti::id-submit-bug-report

;; .................................................... &t-commentary ...

;;; Commentary:

;; Preface, Feb 1995
;;
;;      Seems quite uninteresting package to you? I bet it does, unless
;;      you're a lisp programmer who has infite time to twiddle his
;;      c++-mode-hook + 20 others hooks and have all the time develop nice
;;      little funcs to make living in emacs easier.
;;
;;      While someone may think that all users stick to one mode e.g. when
;;      they are programming C++, that's not obvious. For example programmer
;;      may move between all kind of modes during programming and the
;;      trouble is, that while the buffer's logical content remains the same,
;;      the hooks know nothing about it. Hooks are just dummies that get
;;      called whenever you turn some mode on, try C++ mode over nroff code
;;      and you'll be surprised.
;;
;;      Now let's see one session example:
;;
;;      .   write Lisp               ;lisp-mode + folding-mode
;;      .   hmm, need center-command ;move to text-mode
;;      .   code again               ;lisp-mode
;;      .   adjust defconst var pos. ;turn on tiny tab minor mode
;;      .   code again               ;lisp-mode
;;
;;      Programmer may have bound all common modes into keys so that he can
;;      can access various modes very fast; changing modes is no
;;      problem. What is the problem, is that when you turn off the
;;      CODE-mode, all information about comment-start etc. vanish.
;;
;; Overview of features
;;
;;      o   This is LIBRARY package
;;      o   Try to identify buffer content
;;      o   Useful for checking what kind of file is in buffer and making
;;          decisions based on that. Suitable for hooks.
;;
;;  Imenu example
;;
;;      If you're using *imenu.el* to generate buffer jump points, it is
;;      very likely that the imenu command won't generate right jump points
;;      if you're in wrong mode. Let's use imenu example. Here is first
;;      try: The code sniffs around to see if we're on some mode and then
;;      configures imenu according to it.
;;
;;          (defun my-imenu-mouse (event)
;;            (interactive "e")
;;            (my-imenu))
;;
;;          (defun my-imenu (&optional arg)
;;            "Sets parameters to imenu."
;;            (let (raise)
;;              (setq imenu-max-items 20
;;                 imenu-sort-function nil)
;;              (cond
;;              ((memq major-mode
;;                '(lisp-mode emacs-lisp-mode lisp-interaction-mode))
;;                (setq imenu-create-index-function
;;                      'imenu-example--create-lisp-index
;;                      imenu-sort-function
;;                      'imenu--sort-by-name
;;                      raise t))
;;               ((memq major-mode '(c++-mode))
;;                (setq imenu-create-index-function
;;                      'imenu-example--create-c++-index
;;                      imenu-sort-function
;;                      'imenu--sort-by-name
;;                      raise t))
;;               ((memq major-mode '(c-mode))
;;                (setq imenu-create-index-function
;;                      'imenu-example--create-c-index
;;                      imenu-sort-function
;;                      'imenu--sort-by-name
;;                      raise t)))
;;              (if raise
;;                  (imenu))))
;;
;;      Here is better and more robust way. It'll let you be in any
;;      mode while retaining right imenu.
;;
;;          (require 'imenu)
;;
;;          ;;  Separate functions for keyboard and mouse.
;;          (defun my-imenu-mouse (event &optional arg)
;;            (interactive "e\nP")
;;            (my-imenu arg))
;;
;;          (defun my-imenu (&optional arg)
;;            "Sets parameters to imenu. If called with arg, the output is
;;           unsorted."
;;            (interactive "P")
;;            (let* ((sort-func (if arg nil 'imenu--sort-by-name))
;;                   (table
;;                    '((lisp-mode
;;                       imenu-example--create-lisp-index)
;;                      (emacs-lisp-mode
;;                       imenu-example--create-lisp-index)
;;                      (c++-mode
;;                       imenu-example--create-c++-index)
;;                      (c-mode
;;                       imenu-example--create-c-index)))
;;                   ;;  So, in what mode were really?
;;                   (mode (or (ti::id-info t) major-mode))
;;                   (el (assoc mode table)))
;;              (if (null el)
;;                  (message "Sorry, no imenu for this buffer.")
;;                (setq imenu-max-items         20
;;                      imenu-sort-function             sort-func
;;                      imenu-create-index-function     (nth 1 el))
;;                (imenu))))

;;; Change Log:

;;; Code:

(require 'tinylibm)

(defconst tinylibid-version-time "2010.1208.0809"
  "Latest version number.")

;;; setup: hooks

(defvar ti::id--load-hook nil
  "*Hook run when file has been loaded.")

;;; setup: private

(defvar ti::id--info  nil
  "Buffer local variable.This value is updated every time
function ti::id-info called. For faster responses, you may wan to write your
code like this:

        (setq info ti::id-info nil 'var)

Because peeking the variable is 40x times faster.")

(make-variable-buffer-local 'ti::id--info)

;; Global variables set by functions.
;; - These are heavily used. User may check these too.
;; - They are Set after the buffer is studied.

(defvar ti::id--global-buffer-name nil
  "Global: set by study func, buffer name")

(defvar ti::id--global-buffer-file-name  nil
  "Global: set by study func, buffer file name")

(defvar ti::id--global-buffer-extension nil
  "Global: set by study func, buffer fn ext.")

(defvar ti::id--global-buffer-first-line nil
  "Global: set by study func, 1st line of buffer")

;;; setup: public, user configurable

;; it is INTENTIONAL that the variables are defconst, change these
;; with ti::id--load-hook

(defvar ti::id--file-ext-re "[a-zA-Z0-9]\\(\\.\\)[a-zA-Z0-9]+$"
  "A regexp that says what files can have extension. Everything after the
DOT is considered to form extension. Files like ~/.cshrc are not
considered to have an extension.

The sub match at level 1 indicates the start of an extension.

References:

  See function `ti::id-file-extension'.")

(defconst ti::id--buffer-first-line-regexp-list
  '(("^#.*perl"          "code-perl")
    ("^#.*python"        "code-python")
    ("^#.*scm"           "code-scheme")
    ("^#.*tcl"           "code-tcl")
    ;;    Of course this is not bullet proof, but many lisp code package
    ;;    has first line describing the package.
    ("^;;[ \t]+.*\.el "  "code-lisp")
    ("^#.*awk"           "code-awk")
    ;; It's custom to start the file with ':' no-op, if it's
    ;; sh-coded, since it prevents accidental # for the first line
    ;; --> intepreted as csh code by shell if it sees # as first char
    ("^#.*\/sh\\|^[ \t]*:[ \t]*$"    "code-shell-sh")
    ("^#.*\/csh"                     "code-shell-csh")
    ("^#.*\/tcsh"                    "code-shell-tcsh")
    ("^#.*\/ksh"                     "code-shell-ksh")
    ("^#.*\/zsh"                     "code-shell-zsh")
    ("^#.*\/bash"                    "code-shell-bash")
    ;;  Fortran code uses comments beginning with "c", we assume that
    ;;  there must be at least TWO spaces after initial comment
    ("^c  +"             "code-fortran")
    ("^[ \t]*!"          "resource-x")  ;.Xinitrc ot the like comment
    ;; It's custom to start an nroff man page with a comment that
    ;; holds version control Id string. Comment is  = .\"
    ("^\.\\\""   "text-nroff")
    ;;  A "white paper" document that starts with TOC. See e.g tinytf.el
    ("^Table of contents" "text-white-paper-toc"))
  "*list of \(REGEXP str) where RE is tried upon 1 line match, normally
a bang-slash or emacs --** notation")

;;  - Remember, first one that macthes id useti::d! Put most restrictive at
;;    the beginning.
;;  - The regexp scanning should be the last resort, because its potential
;;    mishits.
;;  - There is no need to add regexp here if buffer can be identified by other
;;    means easily ie. all WWW files have universal .html extension.

(defconst ti::id--buffer-match-regexp-list
  (list
   (list
    (concat
     ;;   interface CServicePreferences;  /* Forward references */
     "interface [0-9A-Za-z]+[ \t]+[0-9A-Za-z]+[ \t]*;"
     ;; exception InvalidRequest{TString aReason;};
     "\\|exception[ \t]+[0-9A-Za-z]+{.*;"
     ;; typedef sequence<CosPropertyService::PropertyNames> PropertyNamesList;
     "\\|typedef[ \t]+[0-9A-Za-z]+<")
    "code-idl")
   ;; *FvwmIdentBack MidnightBlue
   ;; *FvwmIdentFore Yellow
   ;; Style "FvwmButtons" CirculateSkip
   '("Style[ \t]+\"FvwmButtons\"[ \t]+[A-Z]\\|^[*]Fvwm"
     "resource-code-fvwm")
   '("^@c[ \t]"
     "text-texinfo")
   ;; :0
   ;; * condition
   ;; {
   ;;    <code block>
   ;; }
   '("^:0[ \t]*[\r\n]+[ \t]*[*{]"
     "code-procmail")
   '("<\\?php"
     "code-php")
   ;;  #declare Purple_Clouds = pigment {
   ;;  #include "woodmaps.inc"
   ;;  ...
   ;;  Paraboloid_Y
   ;;  scale <30.0, 10.0, 40.0>
   ;;  rotate 180.0*z
   ;;  translate <40.0, 14.0, 50.0>
   (list
    (concat
     "^#include[ \t]+.*.inc\""
     "\\|^#declare[ \t]+[^ \t]+[ \t]*="
     "\\|\\(scale\\|translate\\)[ \t]<[ \t]*[0-9.][ \t]*,.*>")
    "code-povray")
   (list
    (concat
     "[(][ \t]*\\(defmacro\\|defvar\\|defconst\||defun\\|setq"
     "\\|add-hook\\|remove-hook\\|let[*]"
     "\\)")
    "code-lisp")
   '("entity[ \t]+[a-z_A-Z]+[ \t]+is"                  "code-vhdl")
   (list
    (concat
     ;; 01  WORK-AREA.
     ;;     02 PI               PIC S9V9(14).
     ;;
     "01[ \t]+WORK-AREA\\.\\|01[ \t]+CONSTANTS\\."
     ;;         ACUCOBOL-85
     "\\|\\(working-storage\\|portability\\)[ \t]+section\\."
     "\\|perform[ \t]+initialize-environment\\."
     ;;         display "F4 = Exit Demonstration", line 11, column 8.
     "\\|display [\"].*,[ \t]*column[ \t]+[0-9]+\\.")
    "code-cobol")
   ;;  it's "write(*,*)" , and definitely fortran
   '("write[(][*],[*][)]"                               "code-fortran")
   (list
    (concat
     "class.+\\(extends\\implements\\)"
     "\\|"
     "\\(protected\\|public\\)[ \t]+"
     "\\(synchronized[ \t]+\\)?Object")
    "code-java")
   ;;  Function definition
   ;;    def add_doc(self, document, keyword_list):
   ;;    def __init__(self):
   '("^[ \t]+def[ \t]+[a-zA-Z_]+[(].*:"                 "code-python")
   ;;  Oracle sql
   ;;  select * from p_msc_rej where measurement_time = '1995061914124675';
   '("select.*from.*where.*=.*;"                        "code-sql")
   '("order[ \t]+by[ \t]+[^ \t\n]+.*\\(asc\\|desc\\)"   "code-sql")
   '("^[.]TH"                                           "text-nroff")
   '("^Newsgroup:"                                      "text-news")
   '("^To:.*@"                                          "text-mail")
   (list
    ;;  conjug     :: Words -> Words -> String
    ;;  netails    :: [a] -> [[a]]
    ;;  replies    :: Words -> Words -> [String]
    ;;
    "conjug[ \t]+::.*->\\|netails[ \t]+::.*->\\|replies::[ \t]+.*->"
    "code-hugs")
   ;;  %HOME\file\path
   (list
    "^REM[ \t]\\|^CALL[ \t].*%[^ \t\r\n]+%\\[^ \t\r\n]+"
    "code-bat"))
  "*List of \(REGEXP str\) where RE is tried upon whole file.
First one matched is used to determine file type, so put most restrictive
REs first.")

(defconst ti::id--file-extension-alist
  '((".a"     . "code-ada")             ;Ada 83/87
    (".ada"   . "code-ada")             ;Ada 83/87
    (".ads"   . "code-ada")             ;ada 95
    (".adb"   . "code-ada")             ;Ada 95 body/implementation
    (".asp"   . "code-asp")
    (".awk"   . "code-awk")
    (".bat"   . "code-bat")
    (".bash"  . "code-shell-bash")
    (".c"     . "code-c")
    (".cbl"   . "code-cobol")         ;this is the standard hdr & strc
    (".cc"    . "code-c++")
    (".cmd"   . "code-bat")
    (".cob"   . "code-cobol")           ;non-standard unix
    (".cpp"   . "code-c++")
    (".csh"   . "code-shell-csh")
    (".cxx"   . "code-c++")
    (".C"     . "code-c++")
    (".el"    . "code-lisp")
    (".f"     . "code-fortran")
    (".F"     . "code-fortran")
    (".for"   . "code-fortran")
    (".fvwmrc". "resource-code-fvwm")
    (".h"     . "code-c-header")
    (".hh"    . "code-c++-header")
    (".hs"    . "code-hugs")
    (".i"     . "code-cobol-header")    ;non-standard unix
    (".idl"   . "code-idl")         ;CORBA idl, hassan@cs.stanford.edu
    (".html"  . "text-html")
    (".java"  . "code-java")
    (".class" . "code-java-compiled")
    (".jsp"   . "code-jsp")
    (".ksh"   . "code-shell-ksh")
    (".m"     . "code-objective-c")
    (".mod"   . "code-objective-c")
    (".md"    . "code-modula-header")   ;at least modula-2
    (".mi"    . "code-modula")          ;implementation modula-2
    (".pas"   . "code-pascal")
    (".php[34]?" . "code-php")
    (".pl"    . "code-perl-library")
    (".pls"   . "code-perl-shell")
    (".pm"    . "code-perl")
    ;;  also uses .inc and .map but I hesitate to add those extension,
    ;;  because some other may use .inc or .map too for other purposes.
    (".pov"   . "code-povray")
    (".py"    . "code-python")
    (".sh"    . "code-shell")           ;might be csh/ksh/csh
    (".sql"   . "code-sql")
    (".tex"   . "text-tex")
    (".texi"  . "text-tex-info")
    (".txt"   . "text-normal")
    (".vhd"   . "code-vhdl")
    (".vhdl"  . "code-vhdl")
    (".wml"   . "code-wml")
    (".xml"   . "code-xml")
    (".xsl"   . "code-xsl")
    (".xsp"   . "code-xsp"))
  "*List of (ASSOC-KEY STR) where KEY is tried upon
buffer-file-name's extension.")

;;  If the file cannot be identified by extension...

(defconst ti::id--file-regexp-match-list
  '(("\\.ema"        "code-lisp")       ;.emacs , .emacs.dired
    ("\/\\.[Xx]"     "resource-x")      ;.Xdefauls, .xinirc
    ("\/\\.kshrc"    "resource-code-shell-ksh")
    ("\/\\.t?cshrc"  "resource-code-shell-csh") ;alike csh = tcsh
    ("\/\\.bashrc"   "resource-code-shell-bash")
    ("\/\\.bashrc"   "resource-code-shell-sh") ;alike bash = sh
    ("\\.csh"        "shell-csh"))      ;like .cshrc or myScript.csh
  "*List of (REGEXP STR) where RE is tried upon _whole_ buffer-file-name")

;; - Buffers that do not have buffer-file-name property at all.
;; - Only put 'trusted' buffer names that are known to all here.

(defconst ti::id--buffer-name-regexp-list
  '(("[*]info"       "text-manual-info")
    ("[*]man"        "text-manual-shell")
    ("[*]shell"      "process-shell")
    ("[*]ftp"        "process-ftp")
    ("[*]Article"    "text-news")
    ("[*]Summary"    "text-news"))
  "*List of (REGEXP STR) where RE is tried upon buffer-name")

(defconst ti::id--function-list
  '( ;; This first line -*- test should represent exact mode, we trust
    ;;  to it blindly. If the content is not what this mode says, it's
    ;;  user's own mistake.
    ti::id-test-first-line-emacs-special
    ti::id-test-first-line
    ti::id-test-buffer-file-name
    ti::id-test-buffer-content-special
    ti::id-test-extension
    ti::id-test-buffer-name
    ti::id-test-buffer-search-regexp)
  "*List of unctions to call to determine buffer type.
The calling of functions stops immediately when some function
returns non-nil. Notice, that this is also the order of evaluation.")

(defconst ti::id--type2mode
  '(("ada"                 ada-mode     "--")
    ("awk"                 awk-mode     "#")
    ("code-c$"             c-mode       "/*" "*/")
    ("code-c++"            c++-mode     "//")
    ("code-cobol"          cobol-mode   )
    ("code-fortran"        fortran-mode "C")
    ("code-fvwm"           fvwm-mode    )
    ("code-hugs"           hugs-mode    "--" )
    ("code-idl"            idl-mode     )
    ("code-java"           java-mode    "/*" "*/")
    ("code-objective-c"    c-mode       )
    ("code-pascal"         pascal-mode  )
    ("html"                html-mode    "<!---" "-->")
    ("code-php"            php-mode     "//")
    ("code-python"         python-mode  )
    ("code-scheme"         scheme-mode  )
    ("code-sql"            sql-mode     "-- ")
    ("code-tcl"            tcl-mode)
    ("pascal"              pascal-mode  "{" "}")
    ("perl"                perl-mode    "#")
    ("code-povray"         povray-mode  "/*" "*/")
    ("lisp\\|emacs-lisp"   lisp-mode    ";")
    ("text-tex"            tex-mode     "%")
    ("text-texinfo"        texinfo-mode "@c")
    ("text-tex-info"       texinfo-mode "%")
    ("text-mail"           mail-mode)
    ("text-news"           mail-mode)
    ("shell-sh"            sh-mode      "#")
    ("shell-csh"           csh-mode     "#")
    ("shell-ksh"           ksh-mode     "#")
    ("shell-zsh"           zsh-mode     "#")
    ("resource-code-shell-csh" csh-mode "#")
    ("resource-code-shell-sh"   sh-mode "#")
    ("tex$"                tex-mode     "%"))
  "*List of
'((REGEXP MODE-NAME-SYMBOL [COMMENT-START COMMENT-END]) (
  (R M C C)
  ..)
where RE represent match against string that describes the buffer
contents. The comment-start and end fields are optional.")

;;; Misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-cnv-txt2mode (txt)
  "This is kinda fake function, it returns the original MODE based
on the text that represents the buffer contents. This functions purpose
is solely to return you a _symbol_ that is more commonly known to all, than
the _string_ representing a mode.

NOTE:
 Symbol returned does not necessary representy any mode you can turn on.
 Use 'fboundp' test to be sure the symbol is callable function."
  (let (ret)
    (dolist (elt ti::id--type2mode)
      (when (string-match (nth 0 elt) txt)
        (setq ret (nth 1 elt))          ;Mode name
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-cnv-txt2comment (txt)
  "Returns (COMMENT-START . COMMENT-END) variables for text representing
the buffer contents. Notice that comment-end may be nil meaning it
is not needed for mode."
  (let (com-s
        com-e
        re)
    (dolist (elt ti::id--type2mode)
      (setq re (nth 0 elt))
      (if (> (length elt) 2)
          (setq com-s (nth 2 elt)))
      (if (> (length elt) 3)
          (setq com-s (nth 3 elt)))
      (if (null (string-match re txt))
          (setq com-s nil   com-e nil)
        (return)))
    (if com-s
        (cons com-s com-e))))

;;; Id

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-file-extension (file)
  "Return file extension.

References:
  See variable `ti::id--file-ext-re' how file extension is determined."
  (let ((re ti::id--file-ext-re)
	point)
    (when (and file               ;doesn't have filename at all *temp*
               (string-match re file))
      (setq point (match-beginning 1))  ;dot position
      (substring file point))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-read-first-line ()
  "Return first ID line of the file. Empty lines are skipped."
  (let* ((comment-beg   (regexp-quote (or comment-start " ")))
         (empty-re      (concat "[ \t]*" comment-beg "[ \t]*$")))
    (save-excursion
      (ti::widen-safe
        (ti::pmin)
        (while (and (not (eobp))        ;search first sensible line
                    (looking-at empty-re))
          (forward-line 1))
        (unless (eobp)
          (ti::read-current-line))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-read-first-line-emacs-mode (str)
  "Emacs supports special first line syntax e.g. -*-Emacs-Lisp-*-,
to turn on mode when file loads. Try to find function <text>-mode
from the internal symbol list of emacs if line contains -*- marks.

Return:

  symbol    real mode function name found from emacs.
            Btw, emacs barks you automatically if functions given
            in line doesn't exist when file is loaded.
  nil."
  (let (ret
	mode
	sym)
    (cond
     ((setq mode (ti::string-match "-[*]-\\(.*\\)-[*]-" 1 str))
      ;;  let's make symbol out of it
      (setq mode (concat (downcase mode) "-mode"))
      (if (null (setq sym (intern-soft mode)))
          (progn
            ;;  too bad, such mode not loaded into emacs, well if person
            ;;  has loaded file, emacs had barfed already about this unknown
            ;;  mode: "file mode specification error, void function, <mode>"
            nil)
        (if (fboundp sym)             ;let's make sure sym is func ...
            (setq ret sym)))))          ;it's valid mode
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-match (string list)
  "Match STRING against LIST el 1, return LIST elt 2"
  (let (ret
	regexp)
    (dolist (elt list)
      (setq regexp (nth 0 elt))
      (when (string-match regexp string)
        (setq ret (nth 1 elt))
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-buffer-re-search (&optional point)
  "Search `ti::id--buffer-match-regexp-list' from buffer.
Start searching from `point-min' or from optional POINT."
  (let ((list   ti::id--buffer-match-regexp-list)
        ret)
    (or point
        (setq point (point-min)))
    (save-excursion
      (ti::widen-safe
        (goto-char point)               ;start here
        (dolist (elt list)
          (when  (re-search-forward (nth 0 elt) nil t)
            (setq ret (nth 1 elt))
            (return)))))
    ret))

;;; Study

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-global-variable-reset ()
  "Reset some globals."
  (setq ti::id--global-buffer-file-name  nil
        ti::id--global-buffer-extension  nil
        ti::id--global-buffer-first-line  nil))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-global-variable-set ()
  "Set some globals, so that they can be used by all functions.
This reduces overhead of getting these variables multiple times."
  (let* ((bp  (current-buffer))
         (bn  (buffer-name))
         (bfn (buffer-file-name bp))
         (ext (ti::id-file-extension bn))
         (id  (ti::id-read-first-line)))
    (ti::id-global-variable-reset)
    (setq ti::id--global-buffer-file-name   bfn
          ti::id--global-buffer-extension  ext
          ti::id--global-buffer-first-line   id
          ti::id--global-buffer-name    bn)
    ;; so that can be hook
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-study-buffer (type)
  "Chew buffer contents.
Be sure to run `ti::id-global-variable-set' first so that global variables get set.

Input:
  TYPE      symbol; See source code of function.

Return:
  string     type string
  symbol     if real mode found in first line -*- ..-*-
  nil"
  (let ( ;; these are already set
	(id     ti::id--global-buffer-first-line)
	(ext    ti::id--global-buffer-extension)
	(bname  ti::id--global-buffer-name)
	el
	ret)
    (cond
     ((eq type 'extension)
      (if (setq el (assoc ext ti::id--file-extension-alist))
          (setq ret (cdr el))))
     ((eq type 'buffer-file-name)       ;buffer name test
      ;;  whole file match
      (setq ret (ti::id-match bname ti::id--file-regexp-match-list)))
     ((eq type 'buffer-name)            ;buffer name test
      (setq ret (ti::id-match bname ti::id--buffer-name-regexp-list)))
     ((and (eq type '1st-emacs)         ;special -*-Emacs-Lisp-*-
           (stringp id))
      (setq ret (ti::id-read-first-line-emacs-mode id)))
     ((and (eq type '1st-regexp)
           (stringp id))
      (setq ret (ti::id-match id ti::id--buffer-first-line-regexp-list)))
     ((eq type 'buffer-regexp)          ;whole buffer is searched
      (setq ret (ti::id-buffer-re-search))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::id-test-buffer-content-special ()
  "Check special buffer content."
  (let ((text (memq major-mode '(fundamental-mode text-mode))))
    (cond
     ((and text
           (fboundp 'tinytf-text-format-p)
           (ti::funcall 'tinytf-text-format-p))
      "text-white-paper"))))

;;; ----------------------------------------------------------------------
;;; - testing/evaluation  functions

(defun ti::id-test-extension ()
  ""
  (ti::id-study-buffer 'extension))

(defun ti::id-test-buffer-file-name ()
  ""
  (ti::id-study-buffer 'buffer-file-name))

(defun ti::id-test-buffer-name ()
  ""
  (ti::id-study-buffer 'buffer-name))

(defun ti::id-test-first-line ()
  ""
  (ti::id-study-buffer '1st-regexp))

(defun ti::id-test-first-line-emacs-special
  ()
  ""
  (ti::id-study-buffer '1st-emacs))

(defun ti::id-test-buffer-search-regexp
  ()
  ""
  (ti::id-study-buffer 'buffer-regexp))

;;; ------------------------------------------------------------ &Main ---
;;;
;;;###autoload
(defun ti::id-info (&optional mode variable-lookup verb)
  "Try to identify buffer type.

Function doesn't rely on mode, because that doesn't necessarily tell what
the buffer holds. Many users still program their shell scripts in
`fundamental-mode' or so. This means that `major-mode' isn't checked,
because calling function can do it easily.

If this function returns nil, _then_ it's probably the time to check
the `major-mode'.

The normal order of evaluation is as follows:
-  First line in the file
-  Whole filename including path = `buffer-file-name'
-  File name extension
-  `buffer-name' is checked. [temporary buffer has no file name]
-  Whole buffer is searched for RE texts

Input:

  MODE              flag, controls return value format
  VARIABLE-LOOKUP   flag, read buffer type from cache. (From previous call)
  VERB              if non-nil, verbose messages allowed.

Return values:

  when optional MODE = nil
  Some appropriate _string_ that represents the content. notice that this
  string is usually generalised description, _but_ it the file has special
  1st line in form of -*-..-*- the string is direct mode name string.

  when optional MODE = non-nil
  Return possible mode name as _symbol_

  when VARIABLE is non-nil, the variable `ti::id--info' is read instead.
  If it has non-nil value, the value is returned, otherwise full buffer
  is parsed again and variable's value is updated.

References:

  `ti::id-func-alist'  order of evaluation.
  `ti::id--info'            buffer local variable updated during every call."

  (interactive)
  (let ((funcs ti::id--function-list)
	ret
	func
	doit)
    (ti::verb)
    ;; .................................................... do lookup? ...
    (setq ret ti::id--info)
    (cond
     ((null variable-lookup)
      (setq doit t))
     ((and variable-lookup (null ti::id--info)) ;no value stored
      (setq doit t))
     ((and variable-lookup              ;must same type
           (null mode)                  ;string request
           (not (stringp ti::id--info)))
      (setq doit t))
     ((and variable-lookup              ;must same type
           mode                         ;symbol request
           (not (symbolp ti::id--info)))
      (setq doit t)))
    ;; .................................................... do the job ...
    (when doit
      ;;  prepare globals to avoid overhead
      (ti::id-global-variable-set)
      (while (and (setq func (pop funcs))
                  (null (setq ret (funcall func)))))
      ;;  how the results should be returned ?
      (when ret                         ;found anything?
        (if mode
            (if (symbolp ret)           ;return symbol
                ret                     ;it's real mode name
              (setq ret (ti::id-cnv-txt2mode ret))) ;return possible mode name
          (if (symbolp ret)
              (setq ret (symbol-name ret))))
        (if verb
            (message (prin1-to-string ret))))
      ;; Update the buffer local variable
      (setq ti::id--info ret))
    ret))

(provide   'tinylibid)
(run-hooks 'ti::id--load-hook)

;;; tinylibid.el ends here
