;;; tinytag.el --- Grep tags: show C++/Java/etc. syntax call while coding

;; This file is not part of Emacs.

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at the code with folding.el.

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
;;  ~/.emacs startup file. Rip code with with tinylib.el/ti::package-rip-magic
;;
;;      (setq tinytag--database-dir "~/elisp/config")
;;      (require 'tinytag)
;;
;;  You can also use the autoload feature, which speeds up loading
;;  the ~/.emacs
;;
;;      (autoload 'tinytag-install    "tinytag" "" t)
;;      (autoload 'tinytag-main       "tinytag" "" t)
;;      (autoload 'tinytag-main-mouse "tinytag" "" t)
;;
;;  You do not have to install `tinytag-install' function to every known
;;  programming language, because the mode will be global once it
;;  is called. Here, if you program mostly with Java and C,
;;  then either one will activate tinytag for all buffers.
;;
;;      (add-hook 'c++-mode-hook      'tinytag-install)
;;      (add-hook 'c-mode-hook        'tinytag-install)
;;      (add-hook 'java-mode-hook     'tinytag-install)
;;
;;   ********************************************************************
;;
;;          YOU MAY NEED TO CHANGE VARIABLE tinytag--database-setup-table
;;          BEFORE YOU USE THIS PACKAGE.
;;
;;          It gives the instructions where are the databases located
;;          that offer the code help in echo are. Without that variable
;;          this package does nothing.
;;
;;          Read section "Installing support for your programming languages"
;;
;;   ********************************************************************
;;
;;  When you use this package for the first time, an example C/C++
;;  database is extracted fromt he end of this file. See the attached
;;  perl script if you want to generate the database from your
;;  system's manual pages by hand.
;;
;;      M-x load-library RET tinytag RET
;;      M-x tinytag-install-sample-databases
;;
;;  Keybinding suggestion (HP-UX)
;;
;;      (global-set-key [(alt control mouse-2)] 'tinytag-main-mouse)
;;      (global-set-key "\C-c\C-z" 'tinytag-main)
;;
;;  If you have any questions, use these function
;;
;;      M-x tinytag-submit-bug-report
;;
;;  You can send any other programming language database that you may
;;  use, even an different C prototypes in different platform.
;;
;;  C/C++ database help
;;
;;     Peter Simons <simons@petium.rhein.de> to get
;;     NetBSD/Linux C database
;;
;;  Java database help
;;
;;     Jari Aalto
;;     SUN Java databases  (1.2.2 - 1.4)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;  Preface, overview of features
;;
;;      o   Simple database searching, some analogue to emacs TAGS package.
;;          Databaseses are simple text files that are searched for matches.
;;      o   Flips databases easily to show the right data.
;;      o   The Language's function call syntax is shown in the echo area
;;          when cursor is over an identifiable item.
;;      o   Limitations: The function help info can only be 80 characters
;;          or as long as your minibuffer is wide. But you can keep the
;;          *tinytag-output* buffer visible in another buffer to show all
;;          the information.
;;      o   Unlimited extendability to any programming or other "lookup"
;;          languages.
;;
;;      Ready-to-use databases currently available:
;;
;;      o   HP-UX/Netbsd C/C++ function database is included in this file
;;          and perl script to rip the function definitions from Unix manual
;;          pages.
;;      o   Perl script to generate databases from any
;;          Javadoc compliant pages (E.g full database from all
;;          JDK 1.2.2 pages) The Java database is not distributed
;;          because it takes over a Meg and generating one straight
;;          from SUN Java /docs tree is trivial.
;;
;;  Story behind this package
;;
;;      The word "tag" refers to famous tags package in emacs that allows
;;      you to browse your C/C++ code easily.
;;
;;      Someone may be in the middle of c++ project at work and notice
;;      that he frequently consults the manual pages to find correct
;;      syntax for calling stdio.h functions. It's hard to remember
;;      them right every time. Time spent for reading manual pages may
;;      be considerable just to figure out what #include statements
;;      each function might require, and what type of parameters they
;;      need.
;;
;;      No more. There must be a way out of this...
;;
;;      If you have programmed in lisp, you propably know package called
;;      eldoc.el (get it fast if you haven't) by Noah Friedman
;;      <friedman@prep.ai.mit.edu>. It shows you the lisp function call
;;      arguments when your cursor is right over some function.
;;
;;      What a cool tool! You never have to go to elisp info pages
;;      just to check what the function takes, and you don't have to
;;      pop up extra buffer with c-h f <func>. It's a real time saver.
;;      Similar keyword lookup feature culd be built for any
;;      programing. Since eldoc looked the lisp args from memory
;;      (emacs obarray, symbol storage), the other programming
;;      languages must use external reference files: databases.
;;
;;      First, all C/C++ function syntaxes were extracted out of the
;;      man pages with small perl program. The final output after
;;      ripping all the 3C man pages loooked like this. The output
;;      is put under database 'c++-functions'
;;
;;          <dirent.h> dir *opendir(const char *dirname);
;;          <dirent.h> int closedir(dir *dirp);
;;          <dirent.h> int readdir_r(dir *dirp, struct dirent *result);
;;          <dirent.h> long int telldir(dir *dirp);
;;          <dirent.h> struct dirent *readdir(dir *dirp);
;;          ...
;;          <string.h><strings.h> char *index(const char *s, int c);
;;          <string.h><strings.h> char *rindex(const char *s, int c);
;;
;;      Notice how perl stuck the '#define' statements at the
;;      beginning of each function. After this 'function' database was
;;      ready, the only thing needed was lisp code to handle database
;;      lookups for the keyword under the cursor. Similar approach can
;;      be user for any programming language. Just set up the
;;      database, entries to search; one per line.  that's it.
;;
;;  Word about installation -- performance problems [19.29 or lower]
;;
;;      Skip this part if you have 19.30+
;;
;;      When you load this package, it immediately installs an _example_
;;      post-command function. It assumes that you're using the "Having a
;;      test drive" C++ database and stored it as explained.  You propably
;;      want to remove that default post-command function and use your own
;;      definition. Here is how you remove it.
;;
;;      Doing this is also recommended if you don't want post command
;;      actions, but want to use the tinytag-main[-mouse] functions
;;      directly. Call them only when you need them.
;;
;;      o   before any load command say: (setq tinytag--load-hook nil)
;;      o   If package is already loaded, say: C-u M-x tinytag-install.
;;
;;      If your databases are big, or if you're afraid of the overall emacs
;;      performance I STRONGLY ADVICE THAT YOU REMOVE THAT post-command
;;      with methods (2) or (1) You can always call the database with the
;;      supplied keyboard or mouse commands when you need the information.
;;
;;  Having a test run
;;
;;      There is sample C++ database from HP-UX 10 man 3C pages, which
;;      is unfortunately incomplete. You may consider using the BSD
;;      C-database instaed. The BSD is installed by default when you
;;      call `M-x' `tinytag-install-sample-database-c'. Rememeber that
;;      you really should replace those definitions with your own
;;      systems equivalents, because not all functions are found in
;;      all systems. Vendors are different.
;;
;;      This is how you test this package.
;;
;;      o   Go to empty buffer
;;      o   Add statement "strcat(a,b)" to the buffer
;;      o   Turn on C++ mode
;;      o   Be sure to have tinytag on (M-x load-library tinytag.el)
;;      o   Move your cursor over the word "strcat" and wait few seconds.
;;          <For very old Emacs, below 19.30: wave your cursor
;;          back and forth about 5 times.>
;;
;;      You should see the "strcat"'s function's definition displayed in
;;      the echo area. Next, you can start writing your own databases to
;;      languages you use.
;;
;;  Installing support for your programming languages
;;
;;      While you may have installed the default database for C/C++, you
;;      have to configure the variable `tinytag--database-setup-table' to
;;      include all languages where you have databases available. It is
;;      recommended that you keep all emacs related configuration,
;;      including databases, in one place, e.g.
;;
;;          ~/elisp/config/
;;
;;      First you need databases which you must write yourself.
;;      e.g. emacs-tinytag-python-function.el where you describe the
;;      function, packages and call syntax. The only thing after creating the
;;      database is to tell where it can be found. E.g for php you would
;;      add couple of you own variables:
;;
;;          (defconst my-tinytag--db-map-php
;;               '((func       "emacs-config-tinytag-php.txt"))
;;               "Java database.")
;;
;;          (defconst my-tinytag--db-re-php
;;               '(("."        (func)))  ;; See name FUNC in prev. variable
;;               "PHP database.")
;;
;;      And tell tinytag.el that the Java is now known:
;;
;;          (defconst tinytag--database-setup-table
;;            (list
;;             (list
;;              "code-php\\|php"
;;              '(my-tinytag--db-map-php
;;                my-tinytag--db-re-php))
;;            (list
;;              "c-mode....."
;;              '(..
;;                 ))))
;;
;;     C/C++ database
;;
;;      Run c-function-list.pl that comes with Tiny Tools Distribution to
;;      generate function database.
;;
;;      Alternatively copy the database from the end of this file or
;;      use M-x tinytag-install-sample-databases
;;
;;     Java database
;;
;;      Run script `java-function-list.pl' that comes with Tiny Tools
;;      distribution to generate function database from the Sun JDK's javadoc
;;      html pages.  See the manual page of the script how to run it (--help
;;      option)
;;
;;      NOTE: In SUN documentation, there is no System.out.print() or
;;      System.out.println() functions that could be extracted. Please add
;;      Those functions by hand to the database.
;;
;;      This script is also run with call tinytag-install-sample-databases,
;;      provided that you have `perl' and `java' and `java-function-list.pl'
;;      installed and located along PATH.
;;
;;     Perl database
;;
;;      <coming>
;;
;;      Run perl-function-list.pl that comes with Tiny Tools Distribution to
;;      generate function database.  See the manual page of the script how to
;;      run it (--help option)
;;
;;  Database format and display
;;
;;      There is nothing special in the database format, each entry must me
;;      in one line nad that's all. Try to find most suitable display format
;;      for your language, like the general method that is used for C/C++, Java
;;      and Perl
;;
;;          <LIBRARY> return-value function-name(function-parameters) REST-INFO
;;
;;      _Important_: When function `tinytag-search-db' searches the whole
;;      database, it gathers the lines that likely match and FIRST one that
;;      is found is displayed in the echo-area. So that you're aware of other
;;      matches, the count of matches is displayed
;;
;;          10: java.lang.System.out  void println()
;;          |
;;          Count of matches
;;
;;      If you have time, it would be sensible to move the most informational
;;      description of the function first in the list of lines, so that it
;;      get displayed. For example, you could move method this method first in
;;      the line and add [] inside function parameters to signal that the
;;      parameter is optional
;;
;;          java.lang.System.out  void print([Object obj])
;;
;;      Alternatively, you can keep the buffer `tinytag--output-buffer'
;;      visible e.g in separate frame, so that all the matched items are
;;      visible to you in case the one displayed in echo-are is not correct.
;;
;;  Differencies between 19.30+ and lower
;;
;;      The 19.30 Emacs has idle hook, which runs after you move cursor. It
;;      doesn't run if you move mouse.  19.28 on the other hand has post
;;      command hook, that runs every time you either move cursor _OR_
;;      move mouse.
;;
;;      Now, to get display fast in 19.28, you propably want to wave
;;      your mouse fast couple of times. In 19.30 you can have immediate
;;      display with just one cursor move over the word.
;;
;;  What to do if you don't see the definition displayed?
;;
;;      hem most informative is the internal debug which you turn on with:
;;
;;          M-x tinytag-debug
;;
;;      Then call this function directly over the word whose definition
;;      you want to display (e.g. strcat in C++)
;;
;;          ESC ESC : (tinytag-post-command-1)
;;          ========
;;          Press this key combination and enter text to the right.
;;
;;      After that call there is buffer *tinytag-debug* that has some
;;      information about called functions and parameters. Please
;;      investigate the call chain for possible problem. Is the database
;;      selected right? if the regexp used for search right? If you don't
;;      know how to read the debug buffer's output, just send the buffer's
;;      content to me and describe what you did and what was your current
;;      major mode.
;;
;;  Thank you
;;
;;      Peter Simons <simons@petium.rhein.de> sent me
;;      NetBSD and Linux C databases and his perl script can help you
;;      to create your own database from the man pages.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;; Under no circumstances remove the following comment line below!
;; $PackageInstallRe: '^;;+[*]' $

(require 'tinylibm)

(eval-and-compile
  (autoload 'man "man" "" t))

(ti::package-defgroup-tiny TinyTag tinytag-- programming
  "Grep database: example show C++ synatx call while coding.
Overview of features
      o   simple database searching, some analogue to emacs TAGS package.
      o   you can flip databases very easily to show the right data.
      o   example: showing c++ funcall syntax in echo area while you program.
      o   installs hp-ux or netbsd c function databases automatically.")

;;}}}
;;{{{ setup: -- variables

(defcustom tinytag--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--post-command-try-hook
  '(tinytag-try-function-show-cached-word
    tinytag-try-function-search-db)
  "*Try displaying the information.
Run these functions, until one of them return non-nil.
Put here only functions that does not need any user interaction."
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--try-hook
  '(tinytag-try-function-show-cached-word
    tinytag-try-function-search-db
    tinytag-try-function-man)
  "*Try displaying the information.
Run these functions, until one of them return non-nil.
This hook is primarily run upon request: M - x, keyboard command, or mouse
command."
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--set-database-hook
  '(tinytag-set-database)
  "*Function to set the correct database for buffer.
Run these functions, until someone return non-nil."
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--word-filter-hook
  '(tinytag-filter-default-function)
  "*Run hook until some function return non-nil.
Every function in this hook is called with

arg1:  string, word picked at current point to initiate database search

The function should return nil if the word should not be searched.
many times short words are not valid 'keys' in database: e.g. in
C/C++ code common words like 'char' 'double' 'int' can be ignored."
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--word-modify-hook  'tinytag-word-default-adjust
  "*This function formats the searched word to correct search regexp.
Regexp should match only desired hits.

Call arguments:
  string

Function must return always:
  string"
  :type  'hook
  :group 'TinyTag)

(defcustom tinytag--database-ok-hook nil
  "*Run hook database was set according to current buffer.
Called from `tinytag-set-database'.

The variables `tinytag--database-map' and `tinytag--regexp-to-databases'
have valid values when the hook is called."
  :type  'hook
  :group 'TinyTag)

;;; ....................................................... &v-private ...

(defvar tinytag--last-word-lookup  nil
  "Last lookup,  '(WORD . (DB-STRING DB-STRING)).")
(make-variable-buffer-local 'tinytag--last-word-lookup)

(defvar tinytag--noerror  nil
  "If non-nil, no error command is called.")

(defvar tinytag--post-command-hook-counter  nil
  "Counter.")

(defvar tinytag--post-command-hook-wakeup
  ;;  There is no delay in 19.30+, but for <19.30 the must be
  ;;
  (if (boundp 'post-command-idle-hook)
      0 3)
  "Wakeup threshold.
The more lower value, the more often post command hook is called
and your Emacs probably slows down. The values must be 0 in 19.30+,
because `post-command-hook' is not used there.")

(defvar tinytag--database-map nil
  "Databases available, format '((NAME-SYMBOL FILENAME) .. )
Do not put directory name here, use `tinytag--database-dir' instead.")

(defvar tinytag--regexp-to-databases nil
  "Which REGEXP on word should initiate database search?.
Format: '((REGEXP '(database1 database2 ..)) (RE  (d1 d1 ..))  ..)")

(defvar tinytag--idle-timer-elt  nil
  "If idle timer is used, this variable has the timer elt.")

;;; .................................................. &private-sample ...
;;; - These are offered as samples, see tinytag-set-database,
;;;   which uses these variable. They are not part of the tinytag.el
;;;   package (user variables). You should program your own
;;;   tinytag-set-database function to deal with different buffers.

(defconst tinytag--example-db-map-c++
  '((func       "emacs-config-tinytag-c++-functions.txt")
    (struct     "emacs-config-tinytag-c++-structs.txt")
    (types      "emacs-config-tinytag-c++-types.txt"))
  "Sample. C++ databases.")

(defconst tinytag--example-db-re-c++
  '(("_t"       (types structs))
    ("_s"       (struct types))
    ("."        (func)))
  "Sample. C++ word-to-database mappings.")

(defconst tinytag--example-db-map-java
  '((func       "emacs-config-tinytag-java-functions.txt"))
  "Sample. Java databases.")

(defconst tinytag--example-db-re-java
  '(("."        (func))) ;; All words are looked from `func' database
  "Sample. Map found word to correct Java database.")

;;;  You propably should program your own filter function for variaous
;;;  modes. This variable belongs to default filter only.
;;;
(defcustom tinytag--filter-default-c++-words
  (concat
   "^char\\|^double\\|^int$\\|^float\\|^void\\|static"
   "\\|endif\\|define\\|ifndef\\|ifdef\\|include"
   "\\|break")
  "*Filter out unwanted words from current point.
This variable is used in `tinytag-filter-default-function'."
  :type  '(string "Regexp")
  :group 'TinyTag)

;;; ........................................................ &v-public ...
;;; user configurable

(defcustom tinytag--output-buffer "*tinytag-output*"
  "*Buffer where to display all database matches for word at point.
Many times the word picked at point matches several functions and you
can keep this buffer in separate frame in Window environment to see what
is the correct match.
If this variable is nil, no buffer is created."
  :type  'string
  :group 'TinyTag)

(defcustom tinytag--database-dir
  (or
   (file-name-as-directory
    (file-name-directory (ti::package-config-file-prefix "tinytag.el")))
   (error "TinyTag: Can't set default value for `tinytag--database-dir'.
Please define the directory of database directory to `tinytag--database-dir'."))
  "*Directory of database files."
  :type  'directory
  :group 'TinyTag)

(defcustom tinytag--filter-word-table
  (list
   (list
    (concat
     "c-mode\\|cc-mode\\|c[+]+-mode"
     ;;See tinylibid.el
     "\\|code-c\\|code-c[+]+")
    '(or (< (length string) 4)          ;too short word ?
         (string-match tinytag--filter-default-c++-words string))))
  "*Format is:

'((BUFFER-TYPE-REGEXP EVAL-STATEMENT-TO-REJECT) (B E) ..)

If buffer type/mode matches REGEXP then the eval statement is evaluated
for current word that is stored into 'string'. The statement should return
t if word should be rejected. During the eval, any matches done are
case sensitive."
  :type '(repeat
          (string :tag "buffer type regexp")
          (sexp :tag "Form"))
  :group 'TinyTag)

(defcustom tinytag--database-setup-table
  (list
   (list
    (concat
     ;;  See tinylibid.el to detect buffer type
     "c-mode\\|cc-mode\\|c[+]+-mode"
     "\\|code-c\\|code-c[+]+")
    '(tinytag--example-db-map-c++
      tinytag--example-db-re-c++))
   (list
    "java"
    '(tinytag--example-db-map-java
      tinytag--example-db-re-java)))
  "*If buffer type/mode match REGEXP then set database variables.
Cariables `tinytag--database-map' and
`tinytag--regexp-to-databases' are used.

The BUFFER-TYPE-REGEXP corresponds the value returned by ti::id-info
for current buffer. The function detects various progrmaming.

Format:

'((BUFFER-TYPE-REGEXP (DATABASE-MAP-SYM DATABASE-REGEXP-SYM))
  ..)"
  :type '(repeat
          (list
           (string :tag "mode regexp")
           (list
            (symbol :tag "db map sym")
            (symbol :tag "db regexp sym"))))
  :group 'TinyTag)

(defcustom tinytag--display-function  'tinytag-display-function
  "*Function to display search results.
Should accept one ARG, which is list of matched lines from databases."
  :type  'function
  :group 'TinyTag)

;;}}}
;;{{{ code: install

;;;### (autoload 'tinytab-debug-toggle "tinytag"  t t)
(eval-and-compile (ti::macrof-debug-standard "tinytag" "--"))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-initialize  ()
  "Start package and verify that some variables exist."
  (interactive)
  (let ()
    (if (or (not (stringp tinytag--database-dir))
            (not (file-exists-p tinytag--database-dir)))
        (error "\
TinyTag: `tinytag--database-dir' is not a directory. Please configure"))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-install (&optional uninstall)
  "Install package. Optionally UNINSTALL."
  (interactive "P")
  (let ((hook  (if (boundp 'post-command-idle-hook)
		   ;; post-command-idle-hook became obsolete in 19.34
		   'post-command-idle-hook
		 'post-command-hook))
	(cmd   (if uninstall
		   'remove-hook
		 'add-hook)))
    (cond
     ((ti::idle-timer-supported-p)
      (ti::compat-timer-cancel-function 'tinytag-post-command)
      (unless uninstall
        (setq
         tinytag--idle-timer-elt
         (ti::funcall
          'run-with-idle-timer
          2
          'repeat
          'tinytag-post-command))))
     (t
      ;; We use post-command-idle-hook if defined,
      ;; otherwise put it on post-command-hook.
      ;; The idle hook appeared in Emacs 19.30.
      (funcall cmd hook 'tinytag-post-command)))
    (when (interactive-p)
      (message "TinyTag: Package %s" (if uninstall
                                         "deactivated"
                                       "activated")))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-uninstall ()
  "Uninstall package."
  (tinytag-install 'uninstall))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-install-sample-database-java-external-process (doc-dir file)
  "Call external process to examine DOC-DIR to build Java function calls.
The output is written to FILE."
  (let* ((info (ti::process-perl-version "perl"))
         (perl (nth 2 info))
         ;; (type (nth 1 info))
         (bin  "java-function-list.pl")
         (prg  (ti::file-get-load-path bin exec-path)))
    (cond
     ((null perl)
      (message
       (concat
        "Tinytag: [install] Cannot find `perl' along path. "
        "Please check your PATH or install perl. "
        (if (ti::win32-p)
            "http://www.activestate.com"
          "http://www.perl.com/"))))
     ((null prg)
      (message
       (concat
        "Tinytag: [install] Cannot find " bin
        " please check your PATH")))
     (t
      (unless (file-directory-p
               (file-name-directory file))
        (error "Tinytag: Can't find directory %s" file))
      (with-temp-buffer
        ;;  Java 1.3-1.4  documentation size, when processes, is 1.5 Meg
        ;;  It will fit into memory.
        (message
         (concat
          "Tinytag: [install] Java database... "
          "Please wait for external process to traverse %s") doc-dir)
        (call-process perl
                      nil
                      (current-buffer)
                      nil
                      prg
                      "--recurse"
                      doc-dir)
        (message "Tinytag: [install] Java database...done. Size %d"
                 (buffer-size))
        ;; perl syntax error in line NNNN
        (when (ti::re-search-check " line [0-9]")
          (error "Tinytag: Failed to call %s\n\ [Perl error] %s"
                 prg (buffer-string)))
        (when (ti::buffer-empty-p)
          (error "Tinytag: %s with %s didn't return anything."
                 prg doc-dir))
        (write-region (point-min) (point-max) file)
        (message "Tinytag: [install] Java database...done %s"
                 file))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-install-sample-database-java ()
  "Install Java database from Sub JDK documentation."
  (interactive)
  (tinytag-initialize)
  (let ((java-info  (progn
		      (message "TinyTag: Wait, checking java...")
		      (prog1 (ti::process-java-version)
			(message "TinyTag: Wait, checking java...done."))))
	(case-fold-search t))
    (cond
     ((not java-info)
      (message
       (concat
        "Tinytag: [install] Skipped. No `java' binary found along PATH."
        " Java sample database will not be installed.")))
     (t
      ;;  path to `java' binary could be something like
      ;;  i:/java/sun/jdk1.3_02/bin/java
      ;;
      ;;  The Java documentation si assumed to be under
      ;;  i:/java/sun/jdk1.3_02/docs/
      ;;
      (let* ((java (nth 2 java-info)) ;; path
             (dir  (file-name-directory java))
             (root (and dir (ti::directory-up dir)))
             (doc-dir (and root (concat (file-name-as-directory root)
                                        "docs")))
             (out-dir (file-name-as-directory tinytag--database-dir))
             (db      (concat
                       out-dir
                       (nth 1 (assq 'func tinytag--example-db-map-java)))))
        (cond
         ((and (stringp doc-dir)
               (file-directory-p doc-dir))
          (if (file-exists-p db)
              (message
               "Tinytag: [install] Skipped. Database already exists %s"
               db)
            (tinytag-install-sample-database-java-external-process
             doc-dir db)))
         (t
          (message "Tinytag: [install] Can't find java docs/ dir [%s]"
                   doc-dir))))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-install-sample-database-c ()
  "Install c/C++ database from file tintytag.el."
  (interactive)
  (tinytag-initialize)
  (let* ((case-fold-search t)
         (file   (locate-library "tinytag.el"))
         (dir    (file-name-as-directory tinytag--database-dir))
         (db     (concat
                  dir
                  (nth 1 (assq 'func  tinytag--example-db-map-c++))))
         buffer)
    (unless (stringp file)
      (error "Tinytag: [install] cannot find tinytag.el along load-path."))
    (setq buffer (ti::package-install-example "tinytag.el"))
    (with-current-buffer buffer
      (if (not (re-search-forward "Sample.*function database" nil t))
          (error "Tinytag: [install] Cannot find start of example section.")
        ;;  Remove all before the database.
        (beginning-of-line)
        (delete-region (point-min) (point))
        (cond
         ((string-match "hpux" (emacs-version))
          ;; GNU Emacs 19.28.1 (hppa1.1-hp-hpux9, X toolkit)
          ;;
          (re-search-forward "END HP-UX")
          (forward-line 1)
          (delete-region (point) (point-max)))
         (t
          ;; GNU Emacs 19.33.1 (i386-unknown-netbsd1.1, X toolkit)
          ;;
          (re-search-forward "# NetBSD Sample")
          (beginning-of-line)  (delete-region (point-min) (point))
          (re-search-forward "# END NetBSD")
          (forward-line 1)
          (delete-region (point) (point-max))))

        (if (file-exists-p db)
            (message
             "Tinytag: [install] Skipped. Database already exists %s" db)
          (message "Tinytag: [install] C/C++ database...")
          (write-region  (point-min) (point-max) db)
          (message
           "Tinytag: [install]  C/C++ database...installed %s" db))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-install-sample-databases ()
  "Install Sample databases: C/C++ and Java.
This function sets ´tinytag-install-sample-databases'
property 'done to non-nil value, when called."
  (tinytag-install-sample-database-c)
  (tinytag-install-sample-database-java)
  ;;  This is signal for other setups, that can check if functon
  ;;  has already been called (not to install databases multiple times)
  (put 'tinytag-install-sample-databases 'done t)
  (ti::kill-buffer-safe "*ti::pkg*"))

;;}}}

;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytag-word-at-point ()
  "Read word on current point."
  (if (looking-at "[.a-z0-9_]+[ \t\n\r]*(") ;is here word ?
      (ti::buffer-read-word "-_.A-Za-z0-9" )))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinytag-display (list)
  "Call display function with LIST."
  (funcall tinytag--display-function list))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-display-function  (list)
  "Display car of LIST and count of LIST.
Output matched to tinytag--output-buffer too."
  (when (stringp tinytag--output-buffer)
    (with-current-buffer (get-buffer-create tinytag--output-buffer)
      (erase-buffer)
      (dolist (line list)
        (insert line "\n"))))
  (message
   (format
    "%s: %s" (length list) (car list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-filter-default-function  (string)
  "Default filter function. Reject STRING."
  (let ((fid       "tinytag-filter-default-function: ")
	(id        (or (ti::id-info nil 'variable-lookup)
		       (symbol-name major-mode)))
	(table     tinytag--filter-word-table)
	(accept    t)
	(case-fold-search nil)         ;Case is important here
	elt)
    (when (and (setq elt (ti::list-find table id))
               (eval (nth 1 elt)))
      (setq accept nil))
    (tinytag-debug fid " ret accept" accept "elt" elt "\n")
    accept))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-word-default-adjust  (string)
  "Convert STRING to suitable regexp.
Currently supports only C/C++ and Java."
  (let ((fid        "tinytag-word-default-adjust: ")
        (id         (or (ti::id-info nil 'variable-lookup)
                        (symbol-name major-mode)))
        (ret        string))
    (cond
     ((string-match "c[+]+\\|code-c\\|c-mode\\|cc-mode" id)
      ;;
      ;;  We suppose database format
      ;;  "<include.h> function(definition...)"
      ;;
      ;;  Notice the '*' which matches functions returning a pointer
      ;;  ring.h><strings.h> char *strcat(
      ;;
      (setq ret (format "[ \t*]%s[ \t]*(" string)))
     ((string-match "java" id)
      ;;
      ;;  We suppose database format
      ;;
      ;; java.lang.System.out  void print(boolean b)
      ;; java.lang.System.out  void print(Object obj)
      ;;
      ;; --> System.out.*print\\>
      ;;
      ;; But there is a problem with variables
      ;;
      ;;     Runtime rt = Runtime.getRuntime();
      ;;     long free  = rt.freeMemory();
      ;;                   *cursor here         --> grabbed "rt.freeMemory"
      ;;
      ;; --> Search last word after dot too.
      ;;
      (let (class
            function)
        (tinytag-debug fid "java" (string-match "^(.*\\.)(.*)$" string)
                       ret "\n")
        (cond
         ((string-match "\\(.+\\)\\.\\(.*\\)$" string)
          (setq class    (match-string 1 string)
                function (match-string 2 string)
                ret      (format "%s.*%s\\>"
                                 (regexp-quote class)
                                 (regexp-quote function)))
          ;;  If the name is "System.something", the assume that the first word
          ;;  is pure java Class.
          ;;
          ;;  If the name is in lowercase, assume that it is variable and
          ;;  search for plain function name as well.
          (unless (ti::string-match-case "^[A-Z]" class)
            (setq ret (format "%s\\| %s(" ret function))))
         ((not (string-match "\\." string))
          (setq ret (format "\\<%s[ \t]*("
                            (regexp-quote string))))))))
    (tinytag-debug fid "ret" ret "\n")
    ret))

;;}}}
;;{{{ code: search engine

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-search-db (re single-or-list)
  "Search RE from databases in SINGLE-OR-LIST.

References:

  `tinytag--database-map'
  `tinytag--database-dir'
  `tinytag--noerror'
  `tinytag--word-modify-hook'

Return:

  list  '(line line ..)  matched lines or nil."
  (tinytag-initialize)
  (let ((fid       "tinytag-search-db: ")
	(table     tinytag--database-map)
	(noerr     tinytag--noerror)
	(dir       (file-name-as-directory tinytag--database-dir))
	(list      (ti::list-make single-or-list))
	buffer
	file
	ret)
    (tinytag-debug fid " input" re single-or-list "\n")
    (dolist (elt list)
      (when (setq elt (assq elt table))
        (setq file   (concat
                      (file-name-as-directory dir)
                      (nth 1 elt)))
        (setq buffer (or (get-file-buffer file)
                         (and (file-exists-p file)
                              (find-file-noselect file))))
        (tinytag-debug fid " buffer" buffer "file" file "\n")
        ;; ......................................... search or no file ...
        (cond
         ((and
           buffer
           (setq re (run-hook-with-args-until-success
                     'tinytag--word-modify-hook re)))
          (tinytag-debug fid " regexp" re "\n")
          (with-current-buffer buffer
            (ti::pmin)
            (setq ret (ti::buffer-grep-lines re))))
         ((null noerr)
          (error "No database to search %s" file))))) ;; when-dolist
    (tinytag-debug fid "RET" ret "\n")
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-do-search (string)
  "Search those databases which match predefined regexp against STRING.

References:
  `tinytag--regexp-to-databases'

Return:
  list   '(db-matched-line ..)  or nil"
  (let ((fid   "tinytag-do-search: ")
	(table tinytag--regexp-to-databases)
	e
	db
	re
	ret)
    (tinytag-debug fid "string" string "\n")
    (when (run-hook-with-args-until-success
           'tinytag--word-filter-hook string)
      (dolist (elt table)
        (setq re (car elt)
              db (nth 1 elt))
        (when (string-match re string)
          (setq ret (tinytag-search-db string db))
          (tinytag-debug fid " MATCH" "re" re "str" string "ret" ret"\n")
          (return))))
    ret))

;;}}}
;;{{{ code: try funcs

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-try-function-show-cached-word (&optional noerr)
  "Pick word at point and show info if word was same as previously looked.
NOERR ignores errors."
  (interactive)
  (let ((word          (tinytag-word-at-point))
	(prev-word     (car-safe tinytag--last-word-lookup))
	(prev-info     (cdr-safe tinytag--last-word-lookup))
	(err           (or noerr tinytag--noerror))
	(fid           "tinytag-try-function-show-cached-word: "))
    (catch 'quit
      (tinytag-debug fid
                     "word"          word
                     "previous word" prev-word
                     "previous info" prev-info
                     "error flag"    err
                     "\n")
      (when (not (stringp word))
        (if (null err)
            (message "tinytag: No word at point."))
        (throw 'quit t))

      (when (and (not (null prev-word))
                 (not (null prev-info))
                 (string= word prev-word))
        (tinytag-display prev-info)
        (throw 'quit t)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-try-function-search-db ()
  "Do lookup, pick word at point and search databases.
Show the matched word from database."
  (interactive)
  (let ((fid    "tinytag-try-function-search-db: ")
	(word   (tinytag-word-at-point))
	info)
    (tinytag-debug fid "word" word "\n")
    (when (and (stringp word)
               (run-hook-with-args-until-success 'tinytag--set-database-hook)
               (setq info (tinytag-do-search word)))
      (setq tinytag--last-word-lookup (cons word info))
      (tinytag-display info)
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-try-function-man ()
  "Suggest man page search for current word."
  (interactive)
  (let ((word (ti::buffer-read-word))
	;; only in 19.30
;;;      (syntax-elt    (fexec 'c-guess-basic-syntax))
;;;      (syntax        (car-safe syntax-elt))
	ans)
    (when
        (and word ;; (memq syntax '(statement nil))
             (y-or-n-p (concat "Run man on " word))
             (not
              (ti::nil-p
               (setq ans (read-from-minibuffer "Man cmd: " word)))))
      (man ans)
      t)))

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-main ()
  "Run `tinytag--try-hook' until some of the functions return non-nil."
  (interactive)
  (run-hook-with-args-until-success 'tinytag--try-hook))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinytag-main-mouse (&optional event)
  "See `tinytag-main'. Function is called with mouse EVENT."
  (interactive "e")
  (tinytag-main))

;;}}}
;;{{{ code: example

;;; ----------------------------------------------------------------------
;;;
(defun tinytag-post-command-1  ()
  "Do lookup."
  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... do action . .
  ;;
  (when (and
         (run-hook-with-args-until-success 'tinytag--set-database-hook)
         (run-hook-with-args-until-success 'tinytag--post-command-try-hook))
    ;;  This is needed in 19.30<, so that the
    ;;  message doesn't get wiped away.
    ;;
    ;; (unless (fboundp 'post-command-idle-hook)
    ;;   (sleep-for 1))
    ;;(discard-input)
    nil))

;;; ----------------------------------------------------------------------
;;; - The functionality is copied from Noah's <friedman@prep.ai.mit.edu>
;;;   eldoc.el: eldoc-mode-print-current-symbol-info
;;;
;;;###autoload
(defun tinytag-post-command ()
  "Activates only if `tinytag--set-database-hook' wakes up.
Show the database definition for the current word under point.

References:

  `tinytag--noerror'
  `tinytag--post-command-hook-wakeup'
  `tinytag--set-database-hook'"
  (let ((tinytag--noerror  t)
	it-is-time)
    (when (and (not (ti::compat-executing-macro))
               ;; Having this mode operate in the minibuffer
               ;; makes it impossible to
               ;; see what you're doing.
               (not (eq (selected-window) (minibuffer-window)))
               (symbolp this-command)
               (sit-for 0.50)
               ;;  Is this programming language supported?
               (run-hook-with-args-until-success
                'tinytag--set-database-hook))
      ;; ... ... ... ... ... ... ... ... ... ... ... ...  wakeup time? . .
      ;;  This is not used if we're in 19.34
      ;;
      (cond
       ((fboundp 'run-with-idle-timer)
        (setq it-is-time t))
       (t
        (if (null tinytag--post-command-hook-counter)
            (setq tinytag--post-command-hook-counter 0))
        ;;  Don't wake up all the time.. saves Emacs processing time.
        ;;
        (setq
         it-is-time
         (or (eq 0 tinytag--post-command-hook-wakeup)
             (and (not (eq 0  tinytag--post-command-hook-counter))
                  (eq 0 (% tinytag--post-command-hook-counter
                           tinytag--post-command-hook-wakeup)))))
        (incf tinytag--post-command-hook-counter)
        (if it-is-time                  ;do reset
            (setq tinytag--post-command-hook-counter 0))))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... do action . .
      ;;
      (if it-is-time
          (tinytag-post-command-1)))))

;;; ----------------------------------------------------------------------
;;; This is default function. Copy this and  you _have_to_ write your own.
;;;
(defun tinytag-set-database  ()
  "Set correct database values according to buffer content.
Return:
  non-nil or nil  was the database set according to buffer?"
  (let ((id (or (ti::id-info nil 'variable-lookup)
		(symbol-name major-mode)))
	;;    read last word, delete rest
	;;
	(table tinytag--database-setup-table)
	elt
	did-it)
    (cond
     ((and (setq elt (ti::list-find table id))
           (setq elt (nth 1 elt)))      ;Get second list
      (setq tinytag--database-map        (eval (nth 0 elt))
            tinytag--regexp-to-databases (eval (nth 1 elt))
            did-it                       (nth 0 elt)))
     (t
      ;;  Disable search. We don't know database for this buffer
      ;;
      (setq tinytag--database-map         nil
            tinytag--regexp-to-databases  nil)))

    (if did-it
        (run-hooks 'tinytag--database-ok-hook))
    did-it))

;;}}}
;;{{{ example: perl script for creating your own database

;;; ..................................................... &example-c++ ...

;;; Here sript which you can use to generate database lines from
;;; manual page files. I would have included mine, but it uses my private
;;; perl libs and it's much bigger than Peter's handy script.

;;* #! /usr/local/perl5/bin/perl
;;* #
;;* # Peter Simons <simons@petium.rhein.de>
;;* # (#) Script to make C++ function database for Emacs tinytag.el
;;* _
;;* foreach $filename (@ARGV) {
;;*     open(INFILE, $filename) || die("Can't open file $filename.");
;;*     @lines = <INFILE>;
;;*     chop(@lines);
;;*     $lines_num = @lines;
;;*     for ($isSynopsis = 0, $includes = "", $curr_line = "", $i = 0;
;;* $i < $lines_num; $i++) {
;;*     $lines[$i] =~ s/.//g;
;;*     if ($lines[$i] =~ /^SYNOPSIS$/) {
;;*         $isSynopsis = 1;
;;*         next;
;;*     }
;;*     if ($lines[$i] =~ /^DESCRIPTION$/) {
;;*         $isSynopsis = 0;
;;*         $includes = "";
;;*         $curr_line = "";
;;*         last;
;;*     }
;;*     if ($isSynopsis == 1) {
;;*         if ($lines[$i] =~ /^ *#include/) {
;;*             $lines[$i] =~ s/^.*<(.*)>$/<$1>/;
;;*             $includes = $includes . $lines[$i];
;;*         }
;;*         elsif ($lines[$i] =~ /^$/) {
;;*             if ($curr_line ne "") {
;;*                 $curr_line =~ s/ +/ /g;
;;*                 if ($includes eq "") {
;;*                     printf("<none>$curr_line\n");
;;*                 }
;;*                 else {
;;*                     printf("$includes$curr_line\n");
;;*                 }
;;*                 $curr_line = "";
;;*             }
;;*         }
;;*         else {
;;*             $curr_line = $curr_line . $lines[$i];
;;*         }
;;*     }
;;*     }
;;* }

;; }}}
;; {{{ example: HP-UX simple database

;;; Rip code with with tinylib.el/ti::package-rip-magic
;;; These databases are automatically intalled when call
;;; M-x tinytag-install-sample-database-c

;;* # HP-UX Sample C++ function database
;;* #
;;* #   HP 10, The C/C++ function call definitions from man 3C and 2.
;;* #   Not guarrantees, that are calls are here.
;;* #
;;* _
;;* <dirent.h> DIR *opendir(const char *dirname);
;;* <dirent.h> int closedir(DIR *dirp);
;;* <dirent.h> int readdir_r(DIR *dirp, struct dirent *result);
;;* <dirent.h> long int telldir(DIR *dirp);
;;* <dirent.h> struct dirent *readdir(DIR *dirp);
;;* <dirent.h> void rewinddir(DIR *dirp);
;;* <dirent.h> void seekdir(DIR *dirp, long int loc);
;;* <regexp.h> extern char *loc1, *loc2, *locs;
;;* <regexp.h> extern int circf, sed, nbra;
;;* <regexp.h> int advance(const char *string, const char *expbuf);
;;* <regexp.h> int step(const char *string, const char *expbuf);
;;* <stdio.h> FILE *fdopen(int fildes, const char *type);
;;* <stdio.h> FILE *fopen(const char *pathname, const char *type);
;;* <stdio.h> FILE *freopen(const char *pathname, const char *type, FILE *stream);
;;* <stdio.h> FILE *stream);
;;* <stdio.h> int fclose(FILE *stream);
;;* <stdio.h> int fclose_unlocked(FILE *stream);
;;* <stdio.h> int feof(FILE *stream);
;;* <stdio.h> int feof_unlocked(FILE *stream);
;;* <stdio.h> int ferror(FILE *stream);
;;* <stdio.h> int ferror_unlocked(FILE *stream);
;;* <stdio.h> int fflush(FILE *stream);
;;* <stdio.h> int fflush_unlocked(FILE *stream);
;;* <stdio.h> int fscanf(FILE *stream, const char *format, /* [pointer,] */ ...);
;;* <stdio.h> int fseek(FILE *stream, long int offset, int whence);
;;* <stdio.h> int fseek_unlocked(FILE *stream, long int offset, int whence);
;;* <stdio.h> int scanf(const char *format, /* [pointer,] */ ...);
;;* <stdio.h> int sscanf(const char *s, const char *format, /* [pointer,] */ ...);
;;* <stdio.h> long int ftell(FILE *stream);
;;* <stdio.h> long int ftell_unlocked(FILE *stream);
;;* <stdio.h> size_t fread(void *ptr, size_t size, size_t nitems, FILE *stream);
;;* <stdio.h> size_t fwrite(const void *ptr, size_t size, size_t nitems, FILE *stream);
;;* <stdio.h> void clearerr(FILE *stream);
;;* <stdio.h> void clearerr_unlocked(FILE *stream);
;;* <stdio.h> void rewind(FILE *stream);
;;* <stdio.h> void rewind_unlocked(FILE *stream);
;;* <stdlib.h> int system(const char *command);
;;* <stdlib.h> void abort(void);
;;* <string.h><strings.h> char *index(const char *s, int c);
;;* <string.h><strings.h> char *rindex(const char *s, int c);
;;* <string.h><strings.h> char *strcat(char *s1, const char *s2);
;;* <string.h><strings.h> char *strchr(const char *s, int c);
;;* <string.h><strings.h> char *strcpy(char *s1, const char *s2);
;;* <string.h><strings.h> char *strdup(const char *s);
;;* <string.h><strings.h> char *strncat(char *s1, const char *s2, size_t n);
;;* <string.h><strings.h> char *strncpy(char *s1, const char *s2, size_t n);
;;* <string.h><strings.h> char *strpbrk(const char *s1, const char *s2);
;;* <string.h><strings.h> char *strrchr(const char *s, int c);
;;* <string.h><strings.h> char *strrstr(const char *s1, const char *s2);
;;* <string.h><strings.h> char *strstr(const char *s1, const char *s2);
;;* <string.h><strings.h> char *strtok(char *s1, const char *s2);
;;* <string.h><strings.h> char *strtok_r(char *s1, const char *s2, char **last);
;;* <string.h><strings.h> int strcasecmp(const char *s1, const char *s2);
;;* <string.h><strings.h> int strcmp(const char *s1, const char *s2);
;;* <string.h><strings.h> int strcoll(const char *s1, const char *s2);
;;* <string.h><strings.h> int strncasecmp(const char *s1, const char *s2, size_t n);
;;* <string.h><strings.h> int strncmp(const char *s1, const char *s2, size_t n);
;;* <string.h><strings.h> size_t strcspn(const char *s1, const char *s2);
;;* <string.h><strings.h> size_t strlen(const char *s);
;;* <string.h><strings.h> size_t strspn(const char *s1, const char *s2);
;;* <string.h><strings.h> size_t strxfrm(char *s1, const char *s2, size_t n);
;;* <time.h> char *asctime(const struct tm *timeptr);
;;* <time.h> char *ctime(const time_t *timer);
;;* <time.h> double difftime(time_t time1, time_t time0);
;;* <time.h> extern char *tzname[2];
;;* <time.h> extern int daylight;
;;* <time.h> extern long timezone;
;;* <time.h> int asctime_r(const struct tm *timeptr, char *buffer, int buflen);
;;* <time.h> int ctime_r(const time_t *timer, char *buffer, int buflen);
;;* <time.h> int gmtime_r(const time_t *timer, struct tm *result);
;;* <time.h> int localtime_r(const time_t *timer, struct tm *result);
;;* <time.h> struct tm *gmtime(const time_t *timer);
;;* <time.h> struct tm *localtime(const time_t *timer);
;;* <time.h> time_t mktime(struct tm *timeptr);
;;* <time.h> void tzset(void);
;;* <unistd.h> char *getcwd(char *buf, size_t size);
;;* _
;;* #
;;* # HP 10 man 2 pages
;;* #
;;* <sys/stat.h> int stat(const char *path, struct stat *buf);
;;* <sys/stat.h> int lstat(const char *path, struct stat *buf);
;;* <sys/stat.h> int fstat(int fildes, struct stat *buf);
;;* <stdlib.h><unistd.h> void exit(int status);
;;* <stdlib.h><unistd.h> void _exit(int status);
;;* <unistd.h> pid_t getpid(void);
;;* <unistd.h> pid_t getpgrp(void);
;;* <unistd.h> pid_t getppid(void);
;;* <unistd.h> pid_t getpgrp2(pid_t pid);
;;* <unistd.h> int link(const char *path1, const char *path2);
;;* <time.h> time_t time(time_t *tloc);
;;* <sys/socket.h> int send(int s, const void *msg, int len, int flags);
;;* <sys/socket.h> int tolen);
;;* <sys/socket.h> int sendmsg(int s, const struct msghdr msg[], int flags);
;;* <unistd.h> extern char **environ;
;;* <unistd.h> int execv(const char *path, char * const argv[]);
;;* <unistd.h> int execve(const char *file, char * const argv[], char * const envp[]);
;;* <unistd.h> int execvp(const char *file, char * const argv[]);
;;* <unistd.h> unsigned int alarm(unsigned int sec);
;;* <sys/times.h> clock_t times(struct tms *buffer);
;;* <errno.h> extern int errno;
;;* <sys/uio.h><unistd.h> ssize_t write(int fildes, const void *buf, size_t nbyte);
;;* <sys/uio.h><unistd.h> );
;;* <unistd.h> pid_t fork(void);
;;* <unistd.h> int close(int fildes);
;;* <unistd.h> int unlink(const char *path);
;;* <unistd.h> extern char **environ;
;;* <unistd.h> int execv(const char *path, char * const argv[]);
;;* <unistd.h> int execve(const char *file, char * const argv[], char * const envp[]);
;;* <unistd.h> int execvp(const char *file, char * const argv[]);
;;* <time.h> int stime(const time_t *tp);
;;* <ulimit.h> long ulimit(int cmd, ...);
;;* <sys/wait.h> pid_t wait(int *stat_loc);
;;* <sys/wait.h> pid_t waitpid(pid_t pid, int *stat_loc, int options);
;;* <sys/wait.h> pid_t wait3(int *stat_loc, int options, int *reserved);
;;* <time.h> int getitimer(int which, struct itimerval *value);
;;* <unistd.h> int setuid(uid_t uid);
;;* <unistd.h> int setgid(gid_t gid);
;;* <unistd.h> int setresuid(uid_t ruid, uid_t euid, uid_t suid);
;;* <unistd.h> int setresgid(gid_t rgid, gid_t egid, gid_t sgid);
;;* <signal.h> int kill(pid_t pid, int sig);
;;* <signal.h> int raise(int sig);
;;* <signal.h> void (*signal(int sig, void (*action)(int)))(int);
;;*  int rmdir(const char *path);
;;* <sys/wait.h> pid_t wait(int *stat_loc);
;;* <sys/wait.h> pid_t waitpid(pid_t pid, int *stat_loc, int options);
;;* <sys/wait.h> pid_t wait3(int *stat_loc, int options, int *reserved);
;;* <unistd.h> int pause(void);
;;* <symlink.h> int symlink(const char *name1, const char *name2);
;;* <signal.h> int sigsuspend(const sigset_t *sigmask);
;;* <sys/uio.h><unistd.h> size_t read(int fildes, void *buf, size_t nbyte);
;;* <sys/uio.h><unistd.h> );
;;* <unistd.h> extern char **environ;
;;* <unistd.h> int execv(const char *path, char * const argv[]);
;;* <unistd.h> int execve(const char *file, char * const argv[], char * const envp[]);
;;* <unistd.h> int execvp(const char *file, char * const argv[]);
;;* <unistd.h> int access(char *path, int amode);
;;* <sys/stat.h> int mknod(const char *path, mode_t mode, dev_t dev);
;;* <fcntl.h> int creat(const char *path, mode_t mode);
;;* <sys/stat.h> int mkdir(const char *path, mode_t mode);
;;* <sys/uio.h><unistd.h> size_t read(int fildes, void *buf, size_t nbyte);
;;* <ustat.h> int ustat(dev_t dev, struct ustat *buf);
;;* <sys/stat.h> int chmod(const char *path, mode_t mode);
;;* <sys/stat.h> int fchmod(int fildes, mode_t mode)
;;* _
;;* # END HP-UX Sample C++ function database

;; }}}
;; {{{ example: NetBSD 1.2 database

;;* # NetBSD Sample 1.2
;;* # collected by Peter Simons <simons@rhein.de>
;;* # Thu Oct 17 16:31:10 MET DST 1996
;;* #
;;* <assert.h> assert(expression)
;;* <bitstring.h> bit_clear(bit_str name, int bit)
;;* <bitstring.h> bit_decl(bit_str name, int nbits)
;;* <bitstring.h> bit_ffc(bit_str name, int nbits, int *value)
;;* <bitstring.h> bit_ffs(bit_str name, int nbits, int *value)
;;* <bitstring.h> bit_nclear(bit_str name, int start, int stop)
;;* <bitstring.h> bit_nset(bit_str name, int start, int stop)
;;* <bitstring.h> bit_set(bit_str name, int bit)
;;* <bitstring.h> bit_test(bit_str name, int bit)
;;* <bitstring.h> bitstr_size(int nbits)
;;* <bitstring.h> bitstr_t * bit_alloc(int nbits)
;;* <ctype.h> int isalnum(int c)
;;* <ctype.h> int isalpha(int c)
;;* <ctype.h> int isascii(int c)
;;* <ctype.h> int isblank(int c)
;;* <ctype.h> int iscntrl(int c)
;;* <ctype.h> int isdigit(int c)
;;* <ctype.h> int isgraph(int c)
;;* <ctype.h> int islower(int c)
;;* <ctype.h> int isprint(int c)
;;* <ctype.h> int ispunct(int c)
;;* <ctype.h> int isspace(int c)
;;* <ctype.h> int isupper(int c)
;;* <ctype.h> int isxdigit(int c)
;;* <ctype.h> int tolower(int c)
;;* <ctype.h> int toupper(int c)
;;* <dirent.h> int getdirentries(int fd, char *buf, int nbytes, long *basep)
;;* <dlfcn.h> char * dlerror(void)
;;* <dlfcn.h> int dlclose(void *handle)
;;* <dlfcn.h> int dlctl(void *handle, int cmd, void *data)
;;* <dlfcn.h> void * dlopen(char *path, int mode)
;;* <dlfcn.h> void * dlsym(void *handle, char *symbol)
;;* <err.h> void err(int eval, const char *fmt, ...)
;;* <err.h> void errx(int eval, const char *fmt, ...)
;;* <err.h> void verr(int eval, const char *fmt, va_list args)
;;* <err.h> void verrx(int eval, const char *fmt, va_list args)
;;* <err.h> void vwarn(const char *fmt, va_list args)
;;* <err.h> void vwarnx(const char *fmt, va_list args)
;;* <err.h> void warn(const char *fmt, ...)
;;* <err.h> void warnx(const char *fmt, ...)
;;* <fcntl.h> int fcntl(int fd, int cmd, int arg)
;;* <fcntl.h> int open(const char *path, int flags, mode_t mode)
;;* <fcntl.h><kvm.h> int kvm_close(kvm_t *kd)
;;* <fcntl.h><kvm.h> kvm_t * kvm_open(const char *execfile, const char *corefile, char *swapfile, int flags, const char *errstr)
;;* <fcntl.h><kvm.h> kvm_t * kvm_openfiles(const char *execfile, const char *corefile, char *swapfile, int flags, char *errbuf)
;;* <fnmatch.h> int fnmatch(const char *pattern, const char *string, int flags)
;;* <fstab.h> int setfsent(void)
;;* <fstab.h> struct fstab * getfsent(void)
;;* <fstab.h> struct fstab * getfsfile(const char *file)
;;* <fstab.h> struct fstab * getfsspec(const char *spec)
;;* <fstab.h> void endfsent(void)
;;* <glob.h> int glob(const char *pattern, int flags, const int (*errfunc)(const char *, int), glob_t *pglob)
;;* <glob.h> void globfree(glob_t *pglob)
;;* <kvm.h> char * kvm_geterr(kvm_t *kd)
;;* <kvm.h> ssize_t kvm_read(kvm_t *kd, u_long addr, void *buf, size_t nbytes)
;;* <kvm.h> ssize_t kvm_write(kvm_t *kd, u_long addr, const void *buf, size_t nbytes)
;;* <kvm.h><nlist.h> int kvm_nlist(kvm_t *kd, struct nlist *nl)
;;* <kvm.h><sys/kinfo.h><sys/file.h> char * kvm_getfiles(kvm_t *kd, int op, int arg, int *cnt)
;;* <kvm.h><sys/sysctl.h> char ** kvm_getargv(kvm_t *kd, const struct kinfo_proc *p, int nchr)
;;* <kvm.h><sys/sysctl.h> char ** kvm_getenvv(kvm_t *kd, const struct kinfo_proc *p, int nchr)
;;* <kvm.h><sys/sysctl.h> struct kinfo_proc * kvm_getprocs(kvm_t *kd, int op, int arg, int *cnt)
;;* <limits.h><stdlib.h> int radixsort(u_char **base, int nmemb, u_char *table, u_int endbyte)
;;* <limits.h><stdlib.h> int sradixsort(u_char **base, int nmemb, u_char *table, u_int endbyte)
;;* <machine/sysarch.h> int sysarch(int number, char *args)
;;* <math.h> double acos(double x)
;;* <math.h> double acosh(double x)
;;* <math.h> double asin(double x)
;;* <math.h> double asinh(double x)
;;* <math.h> double atan(double x)
;;* <math.h> double atan2(double y, double x)
;;* <math.h> double atanh(double x)
;;* <math.h> double cabs(z)
;;* <math.h> double cbrt(double x)
;;* <math.h> double ceil(double x)
;;* <math.h> double copysign(double x, double y)
;;* <math.h> double cos(double x)
;;* <math.h> double cosh(double x)
;;* <math.h> double erf(double x)
;;* <math.h> double erfc(double x)
;;* <math.h> double exp(double x)
;;* <math.h> double expm1(double x)
;;* <math.h> double fabs(double x)
;;* <math.h> double floor(double x)
;;* <math.h> double fmod(double x, double y)
;;* <math.h> double frexp(double value, int *exp)
;;* <math.h> double hypot(double x, double y)
;;* <math.h> double j0(double x)
;;* <math.h> double j1(double x)
;;* <math.h> double jn(int n, double x)
;;* <math.h> double ldexp(double x, int exp)
;;* <math.h> double lgamma(double x)
;;* <math.h> double log(double x)
;;* <math.h> double log10(double x)
;;* <math.h> double log1p(double x)
;;* <math.h> double logb(double x)
;;* <math.h> double modf(double value, double *iptr)
;;* <math.h> double nextafter(double x, double y)
;;* <math.h> double pow(double x, double y)
;;* <math.h> double remainder(double x, double y)
;;* <math.h> double rint(double x)
;;* <math.h> double scalb(double x, double n)
;;* <math.h> double scalbn(double x, int n)
;;* <math.h> double significand(double x)
;;* <math.h> double sin(double x)
;;* <math.h> double sinh(double x)
;;* <math.h> double sqrt(double x)
;;* <math.h> double tan(double x)
;;* <math.h> double tanh(double x)
;;* <math.h> double y0(double x)
;;* <math.h> double y1(double x)
;;* <math.h> double yn(int n, double x)
;;* <math.h> erff(float x)
;;* <math.h> extern int signgam;
;;* <math.h> float acosf(float x)
;;* <math.h> float acoshf(float x)
;;* <math.h> float asinf(float x)
;;* <math.h> float asinhf(float x)
;;* <math.h> float atan2f(float y, float x)
;;* <math.h> float atanf(float x)
;;* <math.h> float atanhf(float x)
;;* <math.h> float cbrtf(float x)
;;* <math.h> float ceilf(float x)
;;* <math.h> float copysignf(float x, float y)
;;* <math.h> float cosf(float x)
;;* <math.h> float coshf(float x)
;;* <math.h> float erfcf(float x)
;;* <math.h> float expf(float x)
;;* <math.h> float expm1f(float x)
;;* <math.h> float fabsf(float x)
;;* <math.h> float floorf(float x)
;;* <math.h> float fmodf(float x, float y)
;;* <math.h> float hypotf(float x, float y)
;;* <math.h> float j0f(float x)
;;* <math.h> float j1f(float x)
;;* <math.h> float jnf(int n, float x)
;;* <math.h> float lgammaf(float x)
;;* <math.h> float log10f(float x)
;;* <math.h> float log1pf(float x)
;;* <math.h> float logbf(float x)
;;* <math.h> float logf(float x)
;;* <math.h> float nextafterf(float x, float y)
;;* <math.h> float powf(float x, float, y")
;;* <math.h> float remainderf(float x, float y)
;;* <math.h> float rintf(float x)
;;* <math.h> float scalbf(float x, float n)
;;* <math.h> float scalbnf(float x, int n)
;;* <math.h> float significand(float x)
;;* <math.h> float sinf(float x)
;;* <math.h> float sinhf(float x)
;;* <math.h> float sqrtf(float x)
;;* <math.h> float tanf(float x)
;;* <math.h> float tanhf(float x)
;;* <math.h> float y0f(float x)
;;* <math.h> float y1f(float x)
;;* <math.h> float ynf(int n, float x)
;;* <math.h> int finite(double x)
;;* <math.h> int finitef(float x)
;;* <math.h> int ilogb(double x)
;;* <math.h> int ilogbf(float x)
;;* <netdb.h> char * hstrerror(int err)
;;* <netdb.h> endnetent()
;;* <netdb.h> endprotoent()
;;* <netdb.h> setnetent(int stayopen)
;;* <netdb.h> setprotoent(int stayopen)
;;* <netdb.h> struct hostent * gethostbyaddr(const char *addr, int len, int type)
;;* <netdb.h> struct hostent * gethostbyname(const char *name)
;;* <netdb.h> struct hostent * gethostent(void)
;;* <netdb.h> struct netent * getnetbyaddr(long net, int type)
;;* <netdb.h> struct netent * getnetbyname(char *name)
;;* <netdb.h> struct netent * getnetent()
;;* <netdb.h> struct protoent * getprotobyname(char *name)
;;* <netdb.h> struct protoent * getprotobynumber(int proto)
;;* <netdb.h> struct protoent * getprotoent()
;;* <netdb.h> struct rpcent * getrpcbyname(char *name)
;;* <netdb.h> struct rpcent * getrpcbynumber(int number)
;;* <netdb.h> struct rpcent * getrpcent(void)
;;* <netdb.h> struct servent * getservbyname(char *name, char *proto)
;;* <netdb.h> struct servent * getservbyport(int port, proto)
;;* <netdb.h> struct servent * getservent()
;;* <netdb.h> void endhostent(void)
;;* <netdb.h> void endrpcent(void)
;;* <netdb.h> void endservent(void)
;;* <netdb.h> void herror(char *string)
;;* <netdb.h> void sethostent(int stayopen)
;;* <netdb.h> void setrpcent(int stayopen)
;;* <netdb.h> void setservent(int stayopen)
;;* <netinet/if_ether.h> char * ether_ntoa(struct ether_addr *e)
;;* <netinet/if_ether.h> ether_hostton(char *hostname, struct ether_addr *e)
;;* <netinet/if_ether.h> ether_line(char *l, struct ether_addr *e, char *hostname)
;;* <netinet/if_ether.h> ether_ntohost(char *hostname, struct ether_addr *e)
;;* <netinet/if_ether.h> struct ether_addr * ether_aton(char *s)
;;* <nl_types.h> char * catgets(nl_catd catd, int set_id, int msg_id, char *s)
;;* <nl_types.h> int catclose(nl_catd catd)
;;* <nl_types.h> nl_catd catopen(const char *name, int oflag)
;;* <nl_types.h><langinfo.h> char * nl_langinfo(nl_item item)
;;* <nlist.h> int nlist(const char *filename, struct nlist *nl)
;;* <none> char * getusershell(void)
;;* <none> char * lfind(const void *key, const void *base, size_t *nelp, size_t width, int (*compar)(void *, void *))
;;* <none> char * lsearch(const void *key, const void *base, size_t *nelp, size_t width, int (*compar)(void *, void *))
;;* <none> char * tgetstr(char *id, char **area)
;;* <none> char * tgoto(char *cm, destcol, destline)
;;* <none> char * timezone(int zone, int dst)
;;* <none> char *crypt(const char *key, const char *setting)
;;* <none> getloadavg(double loadavg[], int nelem)
;;* <none> getpw(uid, char *buf)
;;* <none> group_from_gid(gid_t gid, int nogroup)
;;* <none> int des_cipher(const char *in, char *out, long salt, int count)
;;* <none> int des_setkey(const char *key)
;;* <none> int encrypt(char *block, int flag)
;;* <none> int getnetgrent(char **host, char **user, char **domain)
;;* <none> int getrpcport(char *host, int prognum, int versnum, int proto)
;;* <none> int innetgr(const char *netgroup, const char *host, const char *user, )
;;* <none> int isinf(double)
;;* <none> int isnan(double)
;;* <none> int profil(char *samples, size_t size, u_long offset, u_int scale)
;;* <none> int rexec(ahost, int inport, char *user, char *passwd, char *cmd, int *fd2p)
;;* <none> int setkey(const char *key)
;;* <none> mode_t getmode(const void *set, mode_t mode)
;;* <none> moncontrol(int mode)
;;* <none> monstartup(u_long *lowpc, u_long *highpc)
;;* <none> nice(int incr)
;;* <none> tgetent(char *bp, char *name)
;;* <none> tgetflag(char *id)
;;* <none> tgetnum(char *id)
;;* <none> user_from_uid(uid_t uid, int nouser)
;;* <none> void * setmode(const char *mode_str)
;;* <none> void * shutdownhook_establish(void (*fn)(void *), void *arg)
;;* <none> void doshutdownhooks(void)
;;* <none> void endnetgrent(void)
;;* <none> void endusershell(void)
;;* <none> void inittodr(time_t base)
;;* <none> void resettodr(void)
;;* <none> void setnetgrent(const char *netgroup)
;;* <none> void setusershell(void)
;;* <none> void shutdownhook_disestablish(void *cookie)
;;* <none> void tputs(register char *cp, int affcnt, int (*outc)())
;;* <none> void tzset()
;;* <pwd.h><unistd.h> char * getpass(const char *prompt)
;;* <regexp.h> int regexec(const regexp *prog, const char *string)
;;* <regexp.h> regexp * regcomp(const char *exp)
;;* <regexp.h> void regsub(const regexp *prog, const char *source, char *dest)
;;* <search.h> struct qelem { struct qelem *q_forw; struct qelem *q_back; char q_data[]; }; void insque(struct qelem *elem, struct qelem *pred)
;;* <search.h> void remque(struct qelem *elem)
;;* <setjmp.h> int _setjmp(jmp_buf env)
;;* <setjmp.h> int setjmp(jmp_buf env)
;;* <setjmp.h> int sigsetjmp(sigjmp_buf env, int savemask)
;;* <setjmp.h> void _longjmp(jmp_buf env, int val)
;;* <setjmp.h> void longjmp(jmp_buf env, int val)
;;* <setjmp.h> void longjmperror(void)
;;* <setjmp.h> void siglongjmp(sigjmp_buf env, int val)
;;* <sgtty.h> gtty(int fd, struct sgttyb *buf)
;;* <sgtty.h> stty(int fd, struct sgttyb *buf)
;;* <signal.h> int kill(pid_t pid, int sig)
;;* <signal.h> int killpg(pid_t pgrp, int sig)
;;* <signal.h> int raise(int sig)
;;* <signal.h> int sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
;;* <signal.h> int sigaddset(sigset_t *set, int signo)
;;* <signal.h> int sigblock(int mask)
;;* <signal.h> int sigdelset(sigset_t *set, int signo)
;;* <signal.h> int sigemptyset(sigset_t *set)
;;* <signal.h> int sigfillset(sigset_t *set)
;;* <signal.h> int siginterrupt(int sig, int flag)
;;* <signal.h> int sigismember(sigset_t *set, int signo)
;;* <signal.h> int sigmask(signum)
;;* <signal.h> int sigpause(int sigmask)
;;* <signal.h> int sigpending(sigset_t *set)
;;* <signal.h> int sigprocmask(int how, const sigset_t *set, sigset_t *oset)
;;* <signal.h> int sigreturn(struct sigcontext *scp)
;;* <signal.h> int sigsetmask(int mask)
;;* <signal.h> int sigsuspend(const sigset_t *sigmask)
;;* <signal.h> sigmask(signum)
;;* <signal.h> sigvec(int sig, struct sigvec *vec, struct sigvec *ovec)
;;* <signal.h> struct sigaction { void (*sa_handler)(); sigset_t sa_mask; int sa_flags; };
;;* <signal.h> struct sigcontext { int sc_onstack; int sc_mask; int sc_sp; int sc_fp; int sc_ap; int sc_pc; int sc_ps; };
;;* <signal.h> struct sigvec { void (*sv_handler)(); sigset_t sv_mask; int sv_flags; };
;;* <signal.h> void (*signal(int sig, void (*func)()))()
;;* <stdarg.h> type va_arg(va_list ap, type)
;;* <stdarg.h> void va_end(va_list ap)
;;* <stdarg.h> void va_start(va_list ap, last)
;;* <stdio.h> FILE * fdopen(int fildes, char *mode)
;;* <stdio.h> FILE * fopen(char *path, char *mode)
;;* <stdio.h> FILE * freopen(char *path, char *mode, FILE *stream)
;;* <stdio.h> FILE * fropen(void *cookie, int (*readfn)(void *, char *, int))
;;* <stdio.h> FILE * funopen(void *cookie, int (*readfn)(void *, char *, int), int (*writefn)(void *, const char *, int), fpos_t (*seekfn)(void *, fpos_t, int), int (*closefn)(void *))
;;* <stdio.h> FILE * fwopen(void *cookie, int (*writefn)(void *, char *, int))
;;* <stdio.h> FILE * popen(const char *command, const char *type)
;;* <stdio.h> FILE * tmpfile(void)
;;* <stdio.h> FILE *stdin; FILE *stdout; FILE *stderr;
;;* <stdio.h> char * ctermid(char *buf)
;;* <stdio.h> char * cuserid(char *buf)
;;* <stdio.h> char * fgetln(FILE *stream, size_t *len)
;;* <stdio.h> char * fgets(char *str, size_t size, FILE *stream)
;;* <stdio.h> char * gets(char *str)
;;* <stdio.h> char * tempnam(const char *tmpdir, const char *prefix)
;;* <stdio.h> char * tmpnam(char *str)
;;* <stdio.h> int fclose(FILE *stream)
;;* <stdio.h> int feof(FILE *stream)
;;* <stdio.h> int ferror(FILE *stream)
;;* <stdio.h> int fflush(FILE *stream)
;;* <stdio.h> int fgetc(FILE *stream)
;;* <stdio.h> int fgetpos(FILE *stream, fpos_t *pos)
;;* <stdio.h> int fileno(FILE *stream)
;;* <stdio.h> int fprintf(FILE *stream, const char *format, ...)
;;* <stdio.h> int fpurge(FILE *stream)
;;* <stdio.h> int fputc(int c, FILE *stream)
;;* <stdio.h> int fputs(const char *str, FILE *stream)
;;* <stdio.h> int fscanf(FILE *stream, const char *format, ...)
;;* <stdio.h> int fseek(FILE *stream, long offset, int whence)
;;* <stdio.h> int fsetpos(FILE *stream, fpos_t *pos)
;;* <stdio.h> int getc(FILE *stream)
;;* <stdio.h> int getchar()
;;* <stdio.h> int getsubopt(char **optionp, char * const *tokens, char **valuep)
;;* <stdio.h> int getw(FILE *stream)
;;* <stdio.h> int pclose(FILE *stream)
;;* <stdio.h> int printf(const char *format, ...)
;;* <stdio.h> int putc(int c, FILE *stream)
;;* <stdio.h> int putchar(int c)
;;* <stdio.h> int puts(const char *str)
;;* <stdio.h> int putw(int w, FILE *stream)
;;* <stdio.h> int remove(const char *path)
;;* <stdio.h> int rename(const char *from, const char *to)
;;* <stdio.h> int scanf(const char *format, ...)
;;* <stdio.h> int setlinebuf(FILE *stream)
;;* <stdio.h> int setvbuf(FILE *stream, char *buf, int mode, size_t size)
;;* <stdio.h> int snprintf(char *str, size_t size, const char *format, ...)
;;* <stdio.h> int sprintf(char *str, const char *format, ...)
;;* <stdio.h> int sscanf(const char *str, const char *format, ...)
;;* <stdio.h> int ungetc(int c, FILE *stream)
;;* <stdio.h> long ftell(FILE *stream)
;;* <stdio.h> size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
;;* <stdio.h> size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
;;* <stdio.h> void clearerr(FILE *stream)
;;* <stdio.h> void perror(const char *string)
;;* <stdio.h> void rewind(FILE *stream)
;;* <stdio.h> void setbuf(FILE *stream, char *buf)
;;* <stdio.h> void setbuffer(FILE *stream, char *buf, size_t size)
;;* <stdio.h><stdarg.h> int vfprintf(FILE *stream, const char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vfscanf(FILE *stream, const char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vprintf(const char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vscanf(const char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vsnprintf(char *str, size_t size, const char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vsprintf(char *str, char *format, va_list ap)
;;* <stdio.h><stdarg.h> int vsscanf(const char *str, const char *format, va_list ap)
;;* <stdlib.h> char * cgetcap(char *buf, char *cap, char type)
;;* <stdlib.h> char * devname(dev_t dev, mode_t type)
;;* <stdlib.h> char * getbsize(int *headerlenp, long *blocksizep)
;;* <stdlib.h> char * getenv(const char *name)
;;* <stdlib.h> char * initstate(unsigned seed, char *state, int n)
;;* <stdlib.h> char * setstate(char *state)
;;* <stdlib.h> daemon(int nochdir, int noclose)
;;* <stdlib.h> div_t div(int num, int denom)
;;* <stdlib.h> double atof(const char *nptr)
;;* <stdlib.h> double drand48(void)
;;* <stdlib.h> double erand48(unsigned short xseed[3])
;;* <stdlib.h> double strtod(const char *nptr, char **endptr)
;;* <stdlib.h> int abs(int j)
;;* <stdlib.h> int atexit(void (*function)(void))
;;* <stdlib.h> int atoi(const char *nptr)
;;* <stdlib.h> int cgetclose(void)
;;* <stdlib.h> int cgetent(char **buf, char **db_array, char *name)
;;* <stdlib.h> int cgetfirst(char **buf, char **db_array)
;;* <stdlib.h> int cgetmatch(char *buf, char *name)
;;* <stdlib.h> int cgetnext(char **buf, char **db_array)
;;* <stdlib.h> int cgetnum(char *buf, char *cap, long *num)
;;* <stdlib.h> int cgetset(char *ent)
;;* <stdlib.h> int cgetstr(char *buf, char *cap, char **str)
;;* <stdlib.h> int cgetustr(char *buf, char *cap, char **str)
;;* <stdlib.h> int heapsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *))
;;* <stdlib.h> int mergesort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *))
;;* <stdlib.h> int putenv(const char *string)
;;* <stdlib.h> int rand(void)
;;* <stdlib.h> int setenv(const char *name, const char *value, int overwrite)
;;* <stdlib.h> int system(const char *string)
;;* <stdlib.h> ldiv_t ldiv(long num, long denom)
;;* <stdlib.h> long atol(const char *nptr)
;;* <stdlib.h> long jrand48(unsigned short xseed[3])
;;* <stdlib.h> long labs(long j)
;;* <stdlib.h> long lrand48(void)
;;* <stdlib.h> long mrand48(void)
;;* <stdlib.h> long nrand48(unsigned short xseed[3])
;;* <stdlib.h> long random(void)
;;* <stdlib.h> qdiv_t qdiv(quad_t num, quad_t denom)
;;* <stdlib.h> quad_t qabs(quad_t j)
;;* <stdlib.h> unsigned short * seed48(unsigned short xseed[3])
;;* <stdlib.h> void * alloca(size_t size)
;;* <stdlib.h> void * bsearch(const void *key, const void *base, size_t nmemb, size_t size, int (*compar) (const void *, const void *))
;;* <stdlib.h> void * calloc(size_t nelem, size_t elsize)
;;* <stdlib.h> void * calloc(size_t nmemb, size_t size)
;;* <stdlib.h> void * malloc(size_t size)
;;* <stdlib.h> void * realloc(void *ptr, size_t size)
;;* <stdlib.h> void abort(void)
;;* <stdlib.h> void exit(int status)
;;* <stdlib.h> void free(void *ptr)
;;* <stdlib.h> void lcong48(unsigned short p[7])
;;* <stdlib.h> void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *))
;;* <stdlib.h> void setproctitle(const char *fmt, ...)
;;* <stdlib.h> void srand(unsigned seed)
;;* <stdlib.h> void srand48(long seed)
;;* <stdlib.h> void srandom(unsigned seed)
;;* <stdlib.h> void unsetenv(const char *name)
;;* <stdlib.h><limits.h> long strtol(const char *nptr, char **endptr, int base)
;;* <stdlib.h><limits.h> unsigned long strtoul(const char *nptr, char **endptr, int base)
;;* <stdlib.h><limits.h><sys/types.h><stdlib.h><limits.h> quad_t strtoq(const char *nptr, char **endptr, int base)
;;* <stdlib.h><limits.h><sys/types.h><stdlib.h><limits.h> u_quad_t strtouq(const char *nptr, char **endptr, int base)
;;* <string.h> char * index(const char *s, int c)
;;* <string.h> char * rindex(const char *s, int c)
;;* <string.h> char * strcat(char *s, const char * append)
;;* <string.h> char * strcat(char *s, const char *append)
;;* <string.h> char * strchr(const char *s, int c)
;;* <string.h> char * strcpy(char *dst, const char *src)
;;* <string.h> char * strdup(const char *str)
;;* <string.h> char * strerror(int errno)
;;* <string.h> char * strerror(int errnum)
;;* <string.h> char * strncat(char *s, const char *append, size_t count)
;;* <string.h> char * strncpy(char *dst, const char *src, size_t count)
;;* <string.h> char * strncpy(char *dst, const char *src, size_t len)
;;* <string.h> char * strpbrk(const char *s, const char *charset)
;;* <string.h> char * strrchr(const char *s, int c)
;;* <string.h> char * strsep(char **stringp, char *delim)
;;* <string.h> char * strsep(char **stringp, const char *delim)
;;* <string.h> char * strsignal(int sig)
;;* <string.h> char * strstr(const char *big, const char *little)
;;* <string.h> char * strtok(char *s, const char *delim)
;;* <string.h> char * strtok(char *str, const char *sep)
;;* <string.h> int bcmp(const void *b1, const void *b2, size_t len)
;;* <string.h> int ffs(int value)
;;* <string.h> int memcmp(const void *b1, const void *b2, size_t len)
;;* <string.h> int strcasecmp(const char *s1, const char *s2)
;;* <string.h> int strcmp(const char *s1, const char *s2)
;;* <string.h> int strcoll(const char *s1, const char *s2)
;;* <string.h> int strncasecmp(const char *s1, const char *s2, size_t count)
;;* <string.h> int strncasecmp(const char *s1, const char *s2, size_t len)
;;* <string.h> int strncmp(const char *s1, const char *s2, size_t count)
;;* <string.h> int strncmp(const char *s1, const char *s2, size_t len)
;;* <string.h> size_t strcspn(const char *s, const char *charset)
;;* <string.h> size_t strlen(const char *s)
;;* <string.h> size_t strspn(const char *s, const char *charset)
;;* <string.h> size_t strxfrm(char *dst, const char *src, size_t n)
;;* <string.h> void * memccpy(void *dst, const void *src, int c, size_t len)
;;* <string.h> void * memchr(const void *b, int c, size_t len)
;;* <string.h> void * memcpy(void *dst, const void *src, size_t len)
;;* <string.h> void * memmove(void *dst, const void *src, size_t len)
;;* <string.h> void * memset(void *b, int c, size_t len)
;;* <string.h> void bcopy(const void *src, void *dst, size_t len)
;;* <string.h> void bzero(void *b, size_t len)
;;* <string.h> void strmode(mode_t mode, char *bp)
;;* <string.h> void swab(const void *src, void *dst, size_t len)
;;* <sys/disklabel.h> struct disklabel * getdiskbyname(const char *name)
;;* <sys/file.h> int flock(int fd, int operation)
;;* <sys/ioctl.h> int ioctl(int d, unsigned long request, char *argp)
;;* <sys/param.h> u_long htonl(u_long hostlong)
;;* <sys/param.h> u_long ntohl(u_long netlong)
;;* <sys/param.h> u_short htons(u_short hostshort)
;;* <sys/param.h> u_short ntohs(u_short netshort)
;;* <sys/param.h><stdlib.h> char * realpath(const char *pathname, char resolvedname[MAXPATHLEN])
;;* <sys/param.h><sys/mount.h> int fstatfs(int fd, struct statfs *buf)
;;* <sys/param.h><sys/mount.h> int mount(const char *type, const char *dir, int flags, void *data)
;;* <sys/param.h><sys/mount.h> int statfs(const char *path, struct statfs *buf)
;;* <sys/param.h><sys/mount.h> int unmount(const char *dir, int flags)
;;* <sys/param.h><sys/sysctl.h> int sysctl(int *name, u_int namelen, void *oldp, size_t *oldlenp, void *newp, size_t newlen)
;;* <sys/param.h><sys/types.h><unistd.h> int getgroups(int gidsetlen, gid_t *gidset)
;;* <sys/param.h><sys/ucred.h><sys/mount.h> int getfsstat(struct statfs *buf, long bufsize, int flags)
;;* <sys/param.h><sys/ucred.h><sys/mount.h> int getmntinfo(struct statfs **mntbufp, int flags)
;;* <sys/param.h><unistd.h> int setgroups(int ngroups, const gid_t *gidset)
;;* <sys/queue.h> CIRCLEQ_ENTRY(TYPE)
;;* <sys/queue.h> CIRCLEQ_HEAD(HEADNAME, TYPE)
;;* <sys/queue.h> CIRCLEQ_INIT(CIRCLEQ_HEAD *head)
;;* <sys/queue.h> CIRCLEQ_INSERT_AFTER(CIRCLEQ_HEAD *head, TYPE *listelm, TYPE *elm, CIRCLEQ_ENTRY NAME)
;;* <sys/queue.h> CIRCLEQ_INSERT_BEFORE(CIRCLEQ_HEAD *head, TYPE *listelm, TYPE *elm, CIRCLEQ_ENTRY NAME)
;;* <sys/queue.h> CIRCLEQ_INSERT_HEAD(CIRCLEQ_HEAD *head, TYPE *elm, CIRCLEQ_ENTRY NAME)
;;* <sys/queue.h> CIRCLEQ_INSERT_TAIL(CIRCLEQ_HEAD *head, TYPE *elm, CIRCLEQ_ENTRY NAME)
;;* <sys/queue.h> CIRCLEQ_REMOVE(CIRCLEQ_HEAD *head, TYPE *elm, CIRCLEQ_ENTRY NAME)
;;* <sys/queue.h> LIST_ENTRY(TYPE)
;;* <sys/queue.h> LIST_HEAD(HEADNAME, TYPE)
;;* <sys/queue.h> LIST_INIT(LIST_HEAD *head)
;;* <sys/queue.h> LIST_INSERT_AFTER(TYPE *listelm, TYPE *elm, LIST_ENTRY NAME)
;;* <sys/queue.h> LIST_INSERT_BEFORE(TYPE *listelm, TYPE *elm, LIST_ENTRY NAME)
;;* <sys/queue.h> LIST_INSERT_HEAD(LIST_HEAD *head, TYPE *elm, LIST_ENTRY NAME)
;;* <sys/queue.h> LIST_REMOVE(TYPE *elm, LIST_ENTRY NAME)
;;* <sys/queue.h> TAILQ_ENTRY(TYPE)
;;* <sys/queue.h> TAILQ_HEAD(HEADNAME, TYPE)
;;* <sys/queue.h> TAILQ_INIT(TAILQ_HEAD *head)
;;* <sys/queue.h> TAILQ_INSERT_AFTER(TAILQ_HEAD *head, TYPE *listelm, TYPE *elm, TAILQ_ENTRY NAME)
;;* <sys/queue.h> TAILQ_INSERT_BEFORE(TYPE *listelm, TYPE *elm, TAILQ_ENTRY NAME)
;;* <sys/queue.h> TAILQ_INSERT_HEAD(TAILQ_HEAD *head, TYPE *elm, TAILQ_ENTRY NAME)
;;* <sys/queue.h> TAILQ_INSERT_TAIL(TAILQ_HEAD *head, TYPE *elm, TAILQ_ENTRY NAME)
;;* <sys/queue.h> TAILQ_REMOVE(TAILQ_HEAD *head, TYPE *elm, TAILQ_ENTRY NAME)
;;* <sys/reboot.h> void boot(int howto)
;;* <sys/resource.h><kvm.h> int kvm_getloadavg(kvm_t *kd, double loadavg[], int nelem)
;;* <sys/signal.h> extern char *sys_siglist[]; extern char *sys_signame[];
;;* <sys/signal.h> void psignal(unsigned sig, const char *s)
;;* <sys/socket.h> int getpeername(int s, struct sockaddr *name, int *namelen)
;;* <sys/socket.h> int getsockname(int s, struct sockaddr *name, int *namelen)
;;* <sys/socket.h> int listen(int s, int backlog)
;;* <sys/socket.h> int shutdown(int s, int how)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> char * inet_ntoa(struct in_addr in)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> int inet_aton(const char *cp, struct in_addr *pin)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> struct in_addr inet_makeaddr(int net, int lna)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> unsigned long inet_addr(const char *cp)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> unsigned long inet_lnaof(struct in_addr in)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> unsigned long inet_netof(struct in_addr in)
;;* <sys/socket.h><netinet/in.h><arpa/inet.h> unsigned long inet_network(const char *cp)
;;* <sys/stat.h><unistd.h> int chflags(const char *path, u_long flags)
;;* <sys/stat.h><unistd.h> int fchflags(int fd, u_long flags)
;;* <sys/syscall.h><unistd.h> int __syscall(quad_t number, ...)
;;* <sys/syscall.h><unistd.h> int syscall(int number, ...)
;;* <sys/time.h> int adjtime(const struct timeval *delta, struct timeval *olddelta)
;;* <sys/time.h> int futimes(int fd, const struct timeval *times)
;;* <sys/time.h> int getitimer(int which, struct itimerval *value)
;;* <sys/time.h> int gettimeofday(struct timeval *tp, struct timezone *tzp)
;;* <sys/time.h> int setitimer(int which, const struct itimerval *value, struct itimerval *ovalue)
;;* <sys/time.h> int settimeofday(const struct timeval *tp, const struct timezone *tzp)
;;* <sys/time.h> int utimes(const char *file, const struct timeval *times)
;;* <sys/time.h><sys/resource.h> int getpriority(int which, int who)
;;* <sys/time.h><sys/resource.h> int getrusage(int who, struct rusage *rusage)
;;* <sys/time.h><sys/resource.h> int setpriority(int which, int who, int prio)
;;* <sys/times.h> clock_t times(struct tms *tp)
;;* <sys/types.h> char * sbrk(int incr)
;;* <sys/types.h> char *ctime(clock) const time_t *clock;
;;* <sys/types.h> double difftime(time1, time0) time_t time1; time_t time0;
;;* <sys/types.h> int bindresvport(int sd, struct sockaddr_in **sin)
;;* <sys/types.h> int brk(const char *addr)
;;* <sys/types.h> int setrgid(gid_t gid)
;;* <sys/types.h> int setruid(uid_t uid)
;;* <sys/types.h><bm.h> bm_pat * bm_comp(u_char *pattern, size_t patlen, u_char freq[256]);
;;* <sys/types.h><bm.h> u_char * bm_exec(bm_pat *pdesc, u_char *text, size_t len);
;;* <sys/types.h><bm.h> void bm_free(bm_pat *pdesc);
;;* <sys/types.h><dirent.h> DIR * opendir(const char *filename)
;;* <sys/types.h><dirent.h> int alphasort(const void *d1, const void *d2)
;;* <sys/types.h><dirent.h> int closedir(DIR *dirp)
;;* <sys/types.h><dirent.h> int dirfd(DIR *dirp)
;;* <sys/types.h><dirent.h> int scandir(const char *dirname, struct dirent ***namelist, int (*select)(struct dirent *), int (*compar)(const void *, const void *))
;;* <sys/types.h><dirent.h> long telldir(const DIR *dirp)
;;* <sys/types.h><dirent.h> struct dirent * readdir(DIR *dirp)
;;* <sys/types.h><dirent.h> void rewinddir(DIR *dirp)
;;* <sys/types.h><dirent.h> void seekdir(DIR *dirp, long loc)
;;* <sys/types.h><grp.h> int setgroupent(int stayopen)
;;* <sys/types.h><grp.h> struct group * getgrent(void)
;;* <sys/types.h><grp.h> struct group * getgrgid(gid_t gid)
;;* <sys/types.h><grp.h> struct group * getgrnam(const char *name)
;;* <sys/types.h><grp.h> void endgrent(void)
;;* <sys/types.h><grp.h> void setgrent(void)
;;* <sys/types.h><limits.h><db.h> DB * dbopen(const char *file, int flags, int mode, DBTYPE type, const void *openinfo);
;;* <sys/types.h><machine/segments.h><machine/sysarch.h> int i386_get_ldt(int start_sel, union descriptor *descs, int num_sels)
;;* <sys/types.h><machine/segments.h><machine/sysarch.h> int i386_set_ldt(int start_sel, union descriptor *descs, int num_sels)
;;* <sys/types.h><machine/sysarch.h> int i386_get_ioperm(u_long *iomap)
;;* <sys/types.h><machine/sysarch.h> int i386_iopl(int iopl)
;;* <sys/types.h><machine/sysarch.h> int i386_set_ioperm(u_long *iomap)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> dn_comp(char *exp_dn, char *comp_dn, int length, char **dnptrs, char **lastdnptr)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> dn_expand(u_char *msg, u_char *eomorig, u_char *comp_dn, u_char *exp_dn, int length)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> res_init()
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> res_mkquery(int op, char *dname, int class, int type, char *data, int datalen, struct rrec *newrr, char *buf, int buflen)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> res_query(char *dname, int class, int type, u_char *answer, int anslen)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> res_search(char *dname, int class, int type, u_char *answer, int anslen)
;;* <sys/types.h><netinet/in.h><arpa/nameser.h><resolv.h> res_send(char *msg, int msglen, char *answer, int anslen)
;;* <sys/types.h><netns/ns.h> char * ns_ntoa(struct ns_addr ns)
;;* <sys/types.h><netns/ns.h> struct ns_addr ns_addr(char *cp)
;;* <sys/types.h><pwd.h> int setpassent(int stayopen)
;;* <sys/types.h><pwd.h> struct passwd * getpwent(void)
;;* <sys/types.h><pwd.h> struct passwd * getpwnam(const char *login)
;;* <sys/types.h><pwd.h> struct passwd * getpwuid(uid_t uid)
;;* <sys/types.h><pwd.h> void endpwent(void)
;;* <sys/types.h><pwd.h> void setpwent(void)
;;* <sys/types.h><regex.h> int regcomp(regex_t *preg, const char *pattern, int cflags);
;;* <sys/types.h><regex.h> int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
;;* <sys/types.h><regex.h> size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
;;* <sys/types.h><regex.h> void regfree(regex_t *preg);
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> char * yperr_string(int incode)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_all(char *indomain, char *inmap, struct ypall_callback *incallback)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_bind(char *dom)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_first(char *indomain, char *inmap, char **outkey, int *outkeylen, char **outval, int *outvallen)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_get_default_domain(char **domp)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_master(char *indomain, char *inmap, char **outname)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_match(char *indomain, char *inmap, const char *inkey, int inkeylen, char **outval, int *outvallen)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_next(char *indomain, char *inmap, char *inkey, int inkeylen, char **outkey, int *outkeylen, char **outval, int *outvallen)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int yp_order(char *indomain, char *inmap, char *outorder)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> int ypprot_err(unsigned int incode)
;;* <sys/types.h><rpcsvc/ypclnt.h><rpcsvc/yp_prot.h> void yp_unbind(char *dom)
;;* <sys/types.h><signal.h> int sigaltstack(const struct sigaltstack *ss, struct sigaltstack *oss)
;;* <sys/types.h><signal.h><machine/segments.h><machine/sysarch.h><machine/vm86.h> int i386_vm86(struct vm86_struct *vmcp)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> struct disk * disk_find(char *)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_attach(struct disk *)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_busy(struct disk *)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_detatch(struct disk *)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_init(void)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_resetstat(struct disk *)
;;* <sys/types.h><sys/disklabel.h><sys/disk.h> void disk_unbusy(struct disk *)
;;* <sys/types.h><sys/ipc.h> key_t ftok(const char *path, char id);
;;* <sys/types.h><sys/ipc.h><sys/msg.h> int msgctl(int msqid, int cmd, struct msqid_ds *buf)
;;* <sys/types.h><sys/ipc.h><sys/msg.h> int msgget(key_t key, int msgflg)
;;* <sys/types.h><sys/ipc.h><sys/msg.h> int msgrcv(int msqid, void *msgp, size_t msgsz, long msgtyp, int msgflg)
;;* <sys/types.h><sys/ipc.h><sys/msg.h> int msgsnd(int msqid, void *msgp, size_t msgsz, int msgflg)
;;* <sys/types.h><sys/ipc.h><sys/msg.h> int shmctl(int shmid, int cmd, struct shmid_ds *buf)
;;* <sys/types.h><sys/ipc.h><sys/sem.h> int semctl(int semid, int semnum, int cmd, union semun arg)
;;* <sys/types.h><sys/ipc.h><sys/sem.h> int semget(key_t key, int nsems, int semflg)
;;* <sys/types.h><sys/ipc.h><sys/sem.h> int semop(int semid, struct sembuf *sops, int nsops)
;;* <sys/types.h><sys/ipc.h><sys/shm.h> int shmdt(void *shmaddr)
;;* <sys/types.h><sys/ipc.h><sys/shm.h> int shmget(key_t key, int size, int shmflg)
;;* <sys/types.h><sys/ipc.h><sys/shm.h> void * shmat(int shmid, void *shmaddr, int shmflg)
;;* <sys/types.h><sys/mman.h> caddr_t mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t offset)
;;* <sys/types.h><sys/mman.h> int madvise(caddr_t addr, size_t len, int behav)
;;* <sys/types.h><sys/mman.h> int mincore(caddr_t addr, size_t len, char *vec)
;;* <sys/types.h><sys/mman.h> int mlock(caddr_t addr, size_t len)
;;* <sys/types.h><sys/mman.h> int mprotect(caddr_t addr, size_t len, int prot)
;;* <sys/types.h><sys/mman.h> int msync(caddr_t addr, size_t len)
;;* <sys/types.h><sys/mman.h> int munlock(caddr_t addr, size_t len)
;;* <sys/types.h><sys/mman.h> int munmap(caddr_t addr, size_t len)
;;* <sys/types.h><sys/mount.h> int getfh(const char *path, fhandle_t *fhp)
;;* <sys/types.h><sys/ptrace.h> int ptrace(int request, pid_t pid, caddr_t addr, int data)
;;* <sys/types.h><sys/socket.h> int accept(int s, struct sockaddr *addr, int *addrlen)
;;* <sys/types.h><sys/socket.h> int bind(int s, const struct sockaddr *name, int namelen)
;;* <sys/types.h><sys/socket.h> int connect(int s, const struct sockaddr *name, int namelen)
;;* <sys/types.h><sys/socket.h> int getsockopt(int s, int level, int optname, void *optval, int *optlen)
;;* <sys/types.h><sys/socket.h> int setsockopt(int s, int level, int optname, const void *optval, int optlen)
;;* <sys/types.h><sys/socket.h> int socket(int domain, int type, int protocol)
;;* <sys/types.h><sys/socket.h> int socketpair(int d, int type, int protocol, int *sv)
;;* <sys/types.h><sys/socket.h> ssize_t recv(int s, void *buf, size_t len, int flags)
;;* <sys/types.h><sys/socket.h> ssize_t recvfrom(int s, void *buf, size_t len, int flags, struct sockaddr *from, int *fromlen)
;;* <sys/types.h><sys/socket.h> ssize_t recvmsg(int s, struct msghdr *msg, int flags)
;;* <sys/types.h><sys/socket.h> ssize_t send(int s, const void *msg, size_t len, int flags)
;;* <sys/types.h><sys/socket.h> ssize_t sendmsg(int s, const struct msghdr *msg, int flags)
;;* <sys/types.h><sys/socket.h> ssize_t sendto(int s, const void *msg, size_t len, int flags, const struct sockaddr *to, int tolen)
;;* <sys/types.h><sys/socket.h><net/if_dl.h> char * link_ntoa(const struct sockaddr_dl *sdl)
;;* <sys/types.h><sys/socket.h><net/if_dl.h> void link_addr(const char *addr, struct sockaddr_dl *sdl)
;;* <sys/types.h><sys/stat.h> int chmod(const char *path, mode_t mode)
;;* <sys/types.h><sys/stat.h> int fchmod(int fd, mode_t mode)
;;* <sys/types.h><sys/stat.h> int fstat(int fd, struct stat *sb)
;;* <sys/types.h><sys/stat.h> int lstat(const char *path, struct stat *sb)
;;* <sys/types.h><sys/stat.h> int mkdir(const char *path, mode_t mode)
;;* <sys/types.h><sys/stat.h> int mkfifo(const char *path, mode_t mode)
;;* <sys/types.h><sys/stat.h> int stat(const char *path, struct stat *sb)
;;* <sys/types.h><sys/stat.h> mode_t umask(mode_t numask)
;;* <sys/types.h><sys/stat.h><fcntl.h> int creat(const char *path, mode_t mode)
;;* <sys/types.h><sys/stat.h><fts.h> FTS * fts_open(char * const *path_argv, int options, int *compar(const FTSENT **, const FTSENT **))
;;* <sys/types.h><sys/stat.h><fts.h> FTSENT * fts_children(FTS *ftsp, int options)
;;* <sys/types.h><sys/stat.h><fts.h> FTSENT * fts_read(FTS *ftsp)
;;* <sys/types.h><sys/stat.h><fts.h> int fts_close(FTS *ftsp)
;;* <sys/types.h><sys/stat.h><fts.h> int fts_set(FTS ftsp, FTSENT *f, int options)
;;* <sys/types.h><sys/systm.h> int copyin(void *uaddr, void *kaddr, size_t len)
;;* <sys/types.h><sys/systm.h> int copyinstr(void *uaddr, void *kaddr, size_t len, size_t *done)
;;* <sys/types.h><sys/systm.h> int copyout(void *kaddr, void *uaddr, size_t len)
;;* <sys/types.h><sys/systm.h> int copyoutstr(void *kaddr, void *uaddr, size_t len, size_t *done)
;;* <sys/types.h><sys/systm.h> int copystr(void *kfaddr, void *kdaddr, size_t len, size_t *done)
;;* <sys/types.h><sys/systm.h> int fubyte(void *base)
;;* <sys/types.h><sys/systm.h> int fuswintr(void *base)
;;* <sys/types.h><sys/systm.h> int fusword(void *base)
;;* <sys/types.h><sys/systm.h> int fuword(void *base)
;;* <sys/types.h><sys/systm.h> int subyte(void *base)
;;* <sys/types.h><sys/systm.h> int suswintr(void *base)
;;* <sys/types.h><sys/systm.h> int susword(void *base)
;;* <sys/types.h><sys/systm.h> int suword(void *base)
;;* <sys/types.h><sys/time.h><sys/resource.h> int getrlimit(int resource, struct rlimit *rlp)
;;* <sys/types.h><sys/time.h><sys/resource.h> int setrlimit(int resource, const struct rlimit *rlp)
;;* <sys/types.h><sys/time.h><unistd.h> FD_CLR(fd, &fdset)
;;* <sys/types.h><sys/time.h><unistd.h> FD_ISSET(fd, &fdset)
;;* <sys/types.h><sys/time.h><unistd.h> FD_SET(fd, &fdset)
;;* <sys/types.h><sys/time.h><unistd.h> FD_ZERO(&fdset)
;;* <sys/types.h><sys/time.h><unistd.h> int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout)
;;* <sys/types.h><sys/timeb.h> int ftime(struct timeb *tp)
;;* <sys/types.h><sys/uio.h><unistd.h> ssize_t read(int d, void *buf, size_t nbytes)
;;* <sys/types.h><sys/uio.h><unistd.h> ssize_t readv(int d, const struct iovec *iov, int iovcnt)
;;* <sys/types.h><sys/uio.h><unistd.h> ssize_t write(int d, const void *buf, size_t nbytes)
;;* <sys/types.h><sys/uio.h><unistd.h> ssize_t writev(int d, const struct iovec *iov, int iovcnt)
;;* <sys/types.h><sys/wait.h> pid_t wait(int *status)
;;* <sys/types.h><sys/wait.h> pid_t waitpid(pid_t wpid, int *status, int options)
;;* <sys/types.h><sys/wait.h><sys/time.h><sys/resource.h> pid_t wait3(int *status, int options, struct rusage *rusage)
;;* <sys/types.h><sys/wait.h><sys/time.h><sys/resource.h> pid_t wait4(pid_t wpid, int *status, int options, struct rusage *rusage)
;;* <sys/types.h><time.h> char *asctime(tm) const struct tm *tm;
;;* <sys/types.h><time.h> struct tm *gmtime(clock) const time_t *clock;
;;* <sys/types.h><time.h> struct tm *localtime(clock) const time_t *clock;
;;* <sys/types.h><time.h> time_t mktime(tm) struct tm *tm;
;;* <sys/types.h><time.h> time_t posix2time(t) time_t t
;;* <sys/types.h><time.h> time_t time2posix(t) time_t t
;;* <sys/types.h><unistd.h> gid_t getegid(void)
;;* <sys/types.h><unistd.h> gid_t getgid(void)
;;* <sys/types.h><unistd.h> int chown(const char *path, uid_t owner, gid_t group)
;;* <sys/types.h><unistd.h> int fchown(int fd, uid_t owner, gid_t group)
;;* <sys/types.h><unistd.h> int setegid(gid_t egid)
;;* <sys/types.h><unistd.h> int seteuid(uid_t euid)
;;* <sys/types.h><unistd.h> int setgid(gid_t gid)
;;* <sys/types.h><unistd.h> int setuid(uid_t uid)
;;* <sys/types.h><unistd.h> int tcsetpgrp(int fd, pid_t pgrp_id)
;;* <sys/types.h><unistd.h> pid_t fork(void)
;;* <sys/types.h><unistd.h> pid_t getpid(void)
;;* <sys/types.h><unistd.h> pid_t getppid(void)
;;* <sys/types.h><unistd.h> pid_t setsid(void)
;;* <sys/types.h><unistd.h> pid_t tcgetpgrp(int fd)
;;* <sys/types.h><unistd.h> uid_t geteuid(void)
;;* <sys/types.h><unistd.h> uid_t getuid(void)
;;* <sys/types.h><utime.h> int utime(const char *file, const struct utimbuf *timep)
;;* <sys/utsname.h> int uname(struct utsname *name)
;;* <sys/vlimit.h> vlimit(resource, value)
;;* <sys/vtimes.h> vtimes(struct vtimes *par_vm, struct vtimes *ch_vm)
;;* <syslog.h><varargs.h> int setlogmask(int maskpri)
;;* <syslog.h><varargs.h> void closelog(void)
;;* <syslog.h><varargs.h> void openlog(const char *ident, int logopt, int facility)
;;* <syslog.h><varargs.h> void syslog(int priority, const char *message, ...)
;;* <syslog.h><varargs.h> void vsyslog(int priority, const char *message, va_list args)
;;* <termios.h> int cfsetispeed(struct termios *t, speed_t speed)
;;* <termios.h> int cfsetospeed(struct termios *t, speed_t speed)
;;* <termios.h> int tcdrain(int fd)
;;* <termios.h> int tcflow(int fd, int action)
;;* <termios.h> int tcflush(int fd, int action)
;;* <termios.h> int tcgetattr(int fd, struct termios *t)
;;* <termios.h> int tcsendbreak(int fd, int len)
;;* <termios.h> int tcsetattr(int fd, int action, const struct termios *t)
;;* <termios.h> speed_t cfgetispeed(const struct termios *t)
;;* <termios.h> speed_t cfgetospeed(const struct termios *t)
;;* <termios.h> void cfmakeraw(struct termios *t)
;;* <termios.h> void cfsetspeed(struct termios *t, speed_t speed)
;;* <time.h> clock_t clock(void)
;;* <time.h> size_t strftime(char *buf, size_t maxsize, const char *format, const struct tm *timeptr)
;;* <time.h> time_t time(time_t *tloc)
;;* <ttyent.h> int endttyent(void)
;;* <ttyent.h> int setttyent(void)
;;* <ttyent.h> struct ttyent * getttyent()
;;* <ttyent.h> struct ttyent * getttynam(char *name)
;;* <unistd.h> char * getcwd(char *buf, size_t size)
;;* <unistd.h> char * getlogin(void)
;;* <unistd.h> char * getwd(char *buf)
;;* <unistd.h> char * mktemp(char *template)
;;* <unistd.h> char * re_comp(const char *s)
;;* <unistd.h> char * ttyname(int fd)
;;* <unistd.h> char * valloc(unsigned size)
;;* <unistd.h> extern char **environ;
;;* <unistd.h> extern char *optarg; extern int optind; extern int optopt; extern int opterr; extern int optreset;
;;* <unistd.h> int access(const char *path, int mode)
;;* <unistd.h> int acct(const char *file)
;;* <unistd.h> int chdir(const char *path)
;;* <unistd.h> int chroot(const char *dirname)
;;* <unistd.h> int close(int d)
;;* <unistd.h> int dup(int oldd)
;;* <unistd.h> int dup2(int oldd, int newd)
;;* <unistd.h> int execl(const char *path, const char *arg, ...)
;;* <unistd.h> int execle(const char *path, const char *arg, ..., char *const envp[])
;;* <unistd.h> int execlp(const char *file, const char *arg, ...)
;;* <unistd.h> int exect(const char *path, char *const argv[], char *const envp[])
;;* <unistd.h> int execv(const char *path, char *const argv[])
;;* <unistd.h> int execve(const char *path, char *const argv[], char *const envp[])
;;* <unistd.h> int execvp(const char *file, char *const argv[])
;;* <unistd.h> int fchdir(int fd)
;;* <unistd.h> int fsync(int fd)
;;* <unistd.h> int ftruncate(int fd, off_t length)
;;* <unistd.h> int getdomainname(char *name, int namelen)
;;* <unistd.h> int getdtablesize(void)
;;* <unistd.h> int getgrouplist(const char *name, gid_t basegid, gid_t *groups, int *ngroups)
;;* <unistd.h> int gethostname(char *name, int namelen)
;;* <unistd.h> int getopt(int argc, char * const *argv, const char *optstring)
;;* <unistd.h> int getpagesize(void)
;;* <unistd.h> int initgroups(const char *name, gid_t basegid)
;;* <unistd.h> int iruserok(u_int32_t raddr, int superuser, const char *ruser, const char *luser)
;;* <unistd.h> int isatty(int fd)
;;* <unistd.h> int link(const char *name1, const char *name2)
;;* <unistd.h> int mknod(const char *path, mode_t mode, dev_t dev)
;;* <unistd.h> int mkstemp(char *template)
;;* <unistd.h> int pause(void)
;;* <unistd.h> int pipe(int *fildes)
;;* <unistd.h> int rcmd(char **ahost, int inport, const char *locuser, const char *remuser, const char *cmd, int *fd2p)
;;* <unistd.h> int re_exec(const char *s)
;;* <unistd.h> int readlink(const char *path, char *buf, int bufsiz)
;;* <unistd.h> int revoke(const char *path)
;;* <unistd.h> int rmdir(const char *path)
;;* <unistd.h> int rresvport(int *port)
;;* <unistd.h> int ruserok(const char *rhost, int superuser, const char *ruser, const char *luser)
;;* <unistd.h> int setdomainname(const char *name, int namelen)
;;* <unistd.h> int sethostid(long hostid)
;;* <unistd.h> int sethostname(const char *name, int namelen)
;;* <unistd.h> int setlogin(const char *name)
;;* <unistd.h> int setpgid(pid_t pid, pid_t pgrp)
;;* <unistd.h> int setpgrp(pid_t pid, pid_t pgrp)
;;* <unistd.h> int setregid(int rgid, int egid)
;;* <unistd.h> int setreuid(int ruid, int euid)
;;* <unistd.h> int swapon(const char *special)
;;* <unistd.h> int symlink(const char *name1, const char *name2)
;;* <unistd.h> int truncate(const char *path, off_t length)
;;* <unistd.h> int ttyslot()
;;* <unistd.h> int unlink(const char *path)
;;* <unistd.h> long fpathconf(int fd, int name)
;;* <unistd.h> long gethostid(void)
;;* <unistd.h> long pathconf(const char *path, int name)
;;* <unistd.h> long sysconf(int name)
;;* <unistd.h> off_t lseek(int fildes, off_t offset, int whence)
;;* <unistd.h> pid_t getpgrp(void)
;;* <unistd.h> pid_t vfork(void)
;;* <unistd.h> size_t confstr(int name, char *buf, size_t len)
;;* <unistd.h> u_int ualarm(u_int microseconds, u_int interval)
;;* <unistd.h> unsigned int alarm(unsigned int seconds)
;;* <unistd.h> unsigned int sleep(unsigned int seconds)
;;* <unistd.h> void _exit(int status)
;;* <unistd.h> void sync(void)
;;* <unistd.h> void usleep(u_int microseconds)
;;* <unistd.h><nfs/nfs.h> int nfssvc(int flags, void *argstructp)
;;* <unistd.h><sys/reboot.h> int reboot(int howto)
;;* <util.h> #indlude <pwd.h>
;;* <util.h> int getmaxpartitions(void)
;;* <util.h> int getrawpartition(void)
;;* <util.h> int login_tty(int fd)
;;* <util.h> int logout(const char *line)
;;* <util.h> int pw_lock(int retries)
;;* <util.h> int pw_mkdb()
;;* <util.h> int pw_scan(char *bp, struct passwd *pw, int *flags)
;;* <util.h> pid_t forkpty(int *amaster, char *name, struct termios *termp, struct winsize *winp)
;;* <util.h> void login(struct utmp *ut)
;;* <util.h> void logwtmp(const char *line, const char *name, const char *host)
;;* <util.h> void openpty(int *amaster, int *aslave, char *name, struct termios *termp, struct winsize *winp)
;;* <util.h> void pw_abort()
;;* <util.h> void pw_copy(int ffd, int tfd, struct passwd *pw)
;;* <util.h> void pw_edit(int notsetuid, const char *filename)
;;* <util.h> void pw;;* _
;;* # END NetBSD Sample C++ function database

;;}}}

(tinytag-install)

(provide   'tinytag)
(run-hooks 'tinytag--load-hook)

;;; tinytag.el ends here
