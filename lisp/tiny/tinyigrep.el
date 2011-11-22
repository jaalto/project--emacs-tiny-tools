;;; tinyigrep.el --- Top level interface to igrep.el

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
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
;;{{{ Install

;;; Install:

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file.
;;
;;     (require 'tinyigrep)
;;
;;      ** YOU NEED igrep.el before you can use this package
;;      ** See <http://groups.google.com/groups?group=gnu.emacs.sources>
;;
;; Or prefer autoload: your emacs loads this package only when you need it.
;; Put your customizations to separate file emacs-rc-tinyigrep.el and add
;; (provide 'emacs-rc-tinyigrep) to the end of the resource file.
;;
;;      (global-set-key "\C-cG" 'tinyigrep-menu)
;;      (autoload 'tinyigrep-menu "tinyigrep" "" t)
;;
;;      (defun my-tinyigrep-load-hook ()
;;        "My settings."
;;        (tinyigrep-install-default-databases)
;;        ;;  Load your additional databases from separate file
;;        (require 'emacs-rc-tinygrep))
;;
;;      (add-hook 'tinyigrep--load-hook 'my-tinyigrep-load-hook)
;;
;; If you find any incorrect behavior, please immediately
;;
;;      o   M-x tinyigrep-debug-toggle
;;      o   Clear debug buffer (kill-buffer tinyigrep--debug)
;;      o   Repeat the task
;;      o   Send a bug report

;;}}}

;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Dec 1996
;;
;;      Somewhere at summer 1996 Kevin Rodgers <kevinr@ihs.com> decided to
;;      put together all grep calls to one package named igrep.el: `agrep',
;;      `egrep', `fgrep' and `zgrep'. It also could search trees
;;      recursively.
;;
;;      The package draw attention and many people picked up the package
;;      from the gnu.emacs.sources newsgroup. The default `M-x' `grep'
;;      command that came with emacs was a pale shadow compared to
;;      `igrep.el' package's possibilities and advanced features. The birth
;;      of tinyigrep.el was the need to integrate searches to some common
;;      directories or grouped files, like news articles, info pages,
;;      project directories, lisp sources, Emacs startup files. A package
;;      that would allow so called "databases" (directories to search).
;;      igrep.el interface seemed to offer great deal of flebility if you
;;      did not have locate(1) or glimpse(1) and their indexes up to date
;;      all the time.
;;
;;  Description
;;
;;      o   Toplevel interface to `igrep.el': Easy command menu access.
;;      o   You can toggle igrep options while you're in the
;;          echo-area menu.
;;      o   Do directory searches easily: grep all your
;;          news files, your Emacs news files, your text files, your lisp
;;          files, grep all manual paths... just configure one variable.
;;          You can jump to matches from the *compile* buffer where
;;          the results will appear.
;;      o   The default installation includes many default directories
;;          to search for: Perl .pod files, perl installation .pm files,
;;          Emacs lisp tree_: cl, vm, tm, semi, mc, gnus and Emacs Info
;;          tree files, Grep all Manual pages installed in your system,
;;          grep your ~/bin ... more.
;;
;;  Do you need this package?
;;
;;      If you use Emacs "grep" then I suggest you to move to *igrep.el*
;;      and evaluate tinyigrep.el as well. It simplifies your grep tasks
;;      much more. If you have several directories where you
;;      keep some persistent data where you want to do lookup from time to
;;      time, then you propably appreciate this package. The default setup
;;      already defines several search "databases" and all you need to
;;      do is to supply SEARCH-STRING and change options for search; like
;;      case sensitivity etc.
;;
;;  Selecting igrep command from command menu
;;
;;      When you call TinyIgrep, you get prompted for a database selection,
;;      which could be "lisp-cl", "Mail", "News" anything you defined. The
;;      igrep interface menu looks like this:
;;
;;          igrep: i)grep g)uess l)ast time d)ired [c)ase r)ecur u)ser]
;;                                                 ===================
;;                                                 options on/off
;;
;;      Pess key ? to see more help on the command line interface. You can
;;      change igrep.el options listed between brackets, e.g. key `c'
;;      toggles case sensitivity of the search by adding or removing the -i
;;      option for grep, `r' can be used to toggle recursive mode on or
;;      off, and `u' toggles user switches on and off. The user options are
;;      stored to history list `tinyigrep--history-igrep-user-options' from
;;      where they can be recalled.
;;
;;  List of predefined databases
;;
;;      For your convenience, function `tinyigrep-install-default-databases'
;;      is run from `tinyigrep--load-hook', which defines several databases
;;      Here is summary of *some* default databases that are defined, if
;;      you have function `tinyigrep-install-default-databases' in
;;      variable `tinyigrep--load-hook'.
;;
;;      Specials
;;
;;      o   `.'             The dot-database: search current buffer's directory
;;
;;     Home:
;;
;;      o   *home-bin-sh*   Search ~/bin/ for *.sh *.awk
;;      o   *home-bin-pl*   Search ~/bin/ for Perl *.pl
;;
;;     Operating system:
;;
;;      o   *man*           Search manpages
;;      o   *c-usr-include* Search C header *.h files in /usr/include
;;
;;     Perl pages:
;;
;;      o   *perl-modules*  Search Perl modules *.pm in @INC
;;      o   *perl-pod*      Search Perl installation *.pod manpages
;;
;;     Emacs and Emacs lisp:
;;
;;      o   *lisp-home*       Search ~/lisp or ~/elisp for *.el
;;      o   *lisp-dot-files*  Search all .emacs* or emacs-rc- files in `load-path'
;;      o   *load-path*       Search `load-path' *.el files
;;      o   *lisp-emacs-distribution* Search Emacs Lisp installation root
;;      o   *emacs-root*      Search all of Emacs installation root
;;
;;     Seach Emacs packages: (there are many more, these are only examples)
;;
;;      o   lisp-pcl-cvs
;;      o   lisp-elib
;;      o   lisp-cl
;;      o   lisp-mc
;;      o   lisp-irchat
;;      o   lisp-bbdb
;;      o   lisp-w3
;;      o   lisp-vm
;;      o   lisp-tiny
;;
;;      In addition to the above, if you have created any of these files in
;;      the directories along the `load-path', you can search those
;;      directories recursively. Please create empty files "to mark"
;;      these directories for automatic scanning. In Unix, simply run
;;      `touch(1)' command to create a file.
;;
;;      o   *lisp-rc* database. If file `emacs-rc-flag.el' exists. Typically
;;          in ~/elisp/rc/ where you might keep all your Emacs startup
;;          settings. This directory may be under CVS, RCS version
;;          control. If you did not know, the term "rc" is historical
;;          and means "Resource file". It comes from Unix, where all
;;          startup files are referred as "Resource files".
;;          Traditionally Emacs only has one `$HOME/.emacs' but as
;;          gain experience and your Emacs configurations exlodes, it
;;          is wise to split the dot-emacs to more manageable parts.
;;
;;      o   *lisp-site-lisp* database. If file `site-lisp-flag.el' exists.
;;          Typically in /usr/local/share/site-lisp/ or under /opt
;;          hirarchy. This is the whole site wide lisp installation root
;;          directory. The search is recursive  for this "flag database".
;;
;;      o   *lisp-site-lisp-emacs* database. If file
;;          `site-lisp-emacs-flag.el' exists. Here you keep Emacs specific
;;          files that do not work with XEmacs. Typically in
;;          /usr/local/share/site-lisp/emacs/ or under /opt/.
;;
;;      o   *lisp-site-lisp-xemacs* database. If file
;;          `site-lisp-xemacs-flag.el' exists. Here you keep XEmacs
;;          specific files that do not work with Emacs. Typically in
;;          /usr/local/share/site-lisp/xemacs/ or under /opt/.
;;
;;      A Typical Emacs lisp package installation structure (site wide) might
;;      look like this. Create the appropriate dummy files as needed,
;;      like creating site-lisp-flag.el to directory /usr/share/site-lisp/
;;
;;          ROOT ( e.g. /usr/local/share/site-lisp/ )
;;          |
;;          +-common/             for Both XEmacs and Emacs
;;          | |
;;          | +-packages/         Big packages
;;          +-emacs               Emacs only packages
;;          +-net/                Net packages
;;          | |
;;          | +-cvs-packages/     CVS maintained packages from Net
;;          | +-users/            PAckages from Users around the Net
;;          +-xemacs/             XEmacs only packages
;;
;;      If you want to define a short name for any of these defaults,
;;      add additional entry e.g. for "vm". The last parameter could
;;      be '(nil) which instead if ´nil' to enable recursive search.
;;
;;          (tinyigrep-db-push-elt-lisp-package "vm" "vm.el" "grep" nil)
;;
;;      Here is small piece of lisp code which adds multiple short names
;;      (defaults are lisp-*} to the search database:
;;
;;          (require 'cl)
;;
;;          (dolist (package '("vm" "gnus" "irchat" "semi-def" "mc" "tinylib"))
;;              (tinyigrep-db-push-elt-lisp-package
;;                 package (concat package ".el") "egrep"))
;;          ;; end of code example
;;
;;  Running custom grep search
;;
;;      Sometimes there is need to search something in separate directory.
;;      and you need the full interface to the `grep'. This isavailable
;;      after the standard database menu which you can omit by hitting like
;;      this. First select  d)data menu and you see prompt:
;;
;;         TinyIgrep search database [RET=next choice]:
;;
;;      Press RET key and you see full grep interface where you can fill
;;      in the search. Say, you have downloaded a mailing list archives
;;      for project xxx and you want to know if there is any discussion
;;      about your problem with `syslog' utility. First, complete the
;;      directory or individual files in the prompt. Separate entries by
;;      a apace. We suppose that archives are in bzip2 compressed format:
;;
;;           Search file list [TAB]: ~/mailing-list/xxx/*bz2
;;           Grep program: bzgrep
;;           grep expression: syslog
;;
;;      After that, the search should start. If you want to modify this
;;      search or run it again with different regexp, start again and
;;      recall the history entries with `M-p' (or cursor up).
;;
;;  Special database for current file directory
;;
;;      Normally you add databases to variable `tinyigrep--database'.
;;      There is also a special database whose name is `.', which refers to
;;      files that are under current buffer's directory. E.g. say you
;;      are editing:
;;
;;          /user/foo/txt/file.txt
;;
;;      calling `tinyigrep-main' and selecting a special database `.' would
;;      give you a prompt to limit search for files under that directory.
;;      While at prompt, you can modify the file pattern:
;;
;;          Search: /user/foo/txt/*.txt
;;
;;      If you select `last' database, the file crieterias to search are
;;      resused and you only need to supply new search pattern (Use `M-p'
;;      and `M-n' to browse history).
;;
;;  Suggestion
;;
;;      You may find it useful to keep the igrep buffer in a special frame
;;      when working in windowed environment. See if you like this:
;;
;;          (if window-system   ;; Use variable `console-type' in XEmacs
;;             (setq special-display-buffer-names
;;               '("*compilation*" "*grep*" "*igrep*")))
;;
;;  How to define your own search databases
;;
;;      Suppose you want to search 1) emacs cl*el files 2) all your ~/Mail
;;      recursively and 3) your ~/News files. The sample database
;;      definitions would look like this:
;;
;;          (require 'tinyigrep)  ;; gives macros, see below
;;
;;          (tinyigrep-db-push-elt
;;            (tinyigrep-db-lisp-elt
;;               "cl.el"        ;; Find root based on this file
;;               "lisp-cl"      ;; the name of search "database"
;;               "egrep"        ;; Program to do the work (remember zgrep)
;;               ;;  Grep only cl libraries
;;               '(list (concat dir "cl*el"))))
;;
;;          ;; Notice '(nil) which turns on recursive search.
;;          ;;
;;          ;;                       database name
;;          ;;                       |                list of files specs
;;          ;;                       |                |           recurse flag
;;          ;;                       |                |           |
;;          (tinyigrep-db-push-elt '("Mail" ("egrep" ("~/Mail/*") (nil) )))
;;
;;          ;; This greps only ~/News/*, non-recursive
;;          ;;
;;          (tinyigrep-db-push-elt '("News" ("egrep" ("~/News/*") )))
;;

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(eval-and-compile
  ;; FIXME: Function `union' from cl package called at runtime
  (autoload 'union "cl-seq"))

(eval-when-compile
  (require 'cl))

;;  When tinyigrep.el is compiled, this strange error occurs:
;;  ** the function `igrep-read-args' is not known to be defined
;;

;;  But that function is not used anywhere? The idea to suppress the
;;  message was to tell the byte compiler beforehand where that functions
;;  is and the `eval-and-compile' trick makes the unnecessary message go
;;  away.
;;
;;  Interestingly, this error message is not displayed by XEmacs 19.14
;;  byte compiler. Maybe this is "used before defined" syndrome in
;;  igrep.el.

(eval-and-compile

  ;;  From NTEmacs 20.x FAQ "igrep 2.82 needs to have the variable
  ;;  grep-null-device defined; add the following to your startup file"

  (condition-case error
      (require 'igrep)
    (error
     (error "\
  ** tinyigrep.el: Hm, no igrep.el along `load-path'.
                   You can find it at <http://groups.google.com/groups?group=gnu.emacs.sources>
                   %s"
            (prin1-to-string error))))

  (unless (boundp 'igrep-null-device)
    (if (boundp 'null-device)
        (defvar igrep-null-device (symbol-value 'null-device))
      ;; #todo: should this be call to `error'?
      (message "\
  ** tinyigrep.el: [WARN] `igrep-null-device' defined. Check igrep.el version.")))

  ;;  If trying to load 2.82 in Xemacs 21.2 beta; it cries that
  ;;  somebody (igrep) tried to require ange-ftp. Instruct users
  ;;  to get newer version.

  (if (and (ti::xemacs-p)
           (string< igrep-version "2.83"))
      (message "  ** TinyIgrep: [XEmacs check] you must have igrep.el 2.83+."))

  (multiple-value-bind (major minor)
      (if (string-match "^\\([0-9]+\\)\+.\\([0-9]+\\)" igrep-version)
          (list (match-string 1 igrep-version)
                (match-string 2 igrep-version)))
    (if (or (< (string-to-number major) 2)
            (and (string= major "2")
                 (< (string-to-number minor) 55)))
        (error
         "TinyIgrep: [Emacs check] You must have igrep 2.56+. You have now %s"
         igrep-version)))

  (autoload 'tinyperl-install "tinyperl" "" t)

  ;;  These are used only if tinyperl.el is available.
  ;;  Just introduce variables for byte compiler.

  (defvar tinyperl--pod-path)
  (defvar tinyperl--inc-path)
  (autoload 'igrep-read-args "igrep"))

(ti::package-defgroup-tiny TinyIgrep tinyigrep-- tools
  "Top level interface to igrep.el")

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinyigrep--load-hook
  '(tinyigrep-install-default-databases)
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyIgrep)

;;}}}
;;{{{ setup: user config

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinyigrep--grep-program
  (cond
   ((boundp 'grep-program)
    grep-program)
   ((and (ti::emacs-type-win32-p)
         (cygwin-p))
    ;;  Cannot use `egrep' because itäs bash shell script and not
    ;;  a callable windows executable.
    "grep")
   (t
    "egrep"))
  "*Default grep program. Initialised from `grep-program' if available."
  :type  'string
  :group 'TinyIgrep)

(defcustom tinyigrep--grep-word-at-point nil
  "*if non-nil, Grab word at point for searching."
  :type  'boolean
  :group 'TinyIgrep)

(defcustom tinyigrep--user-level 'basic
  "*Interface level.
'basic       Suppose defautls for everything.
'advanced    Show additional features."
  :type  '(choice
           (const basic)
           (const advanced))
  :group 'TinyIgrep)

(defcustom tinyigrep--special-database "."
  "*Special database: grep files under file's directory.
If user seelcts this database, then the current search is suggested
by looking at the buffer's current directory and file extension.

Eg. if you're in list buffer /dir1/dir2/foo.el, then the suggested
files to search are

  /dir1/dir2/*el"
  :type  'string
  :group 'TinyIgrep)

(defcustom tinyigrep--debug nil
  "*If non-nil, Record program flow to debug buffer."
  :type ' boolean
  :group 'TinyIgrep)

(defcustom tinyigrep--flag-file-list
  '(("lisp-site-lisp"               "site-lisp-flag.el"              'rec)
    ("lisp-site-lisp-net"           "site-lisp-net-flag.el"          'rec)
    ("lisp-site-lisp-net-cvs"       "site-lisp-net-cvs-flag.el"      'rec)
    ("lisp-site-lisp-net-packages"  "site-lisp-net-packages-flag.el" 'rec)
    ("lisp-site-lisp-net-users"     "site-lisp-net-users-flag.el"    'rec)
    ("lisp-site-lisp-common"        "site-lisp-common-flag.el"       'rec)
    ("lisp-site-lisp-emacs"         "site-lisp-emacs-flag.el"        'rec)
    ("lisp-site-lisp-xemacs"        "site-lisp-xemacs-flag.el"       'rec)
    ("lisp-rc"                      "emacs-rc-flag.el"               'rec))
  "*List of lisp files that are searches to FLAG the directory.
When the file is found, the database for that directory is created.
The isea is that you have site-lisp structure, possibly under you
~/elisp  or /usr/share/site-lisp, where the files are grouped
according to their characteristics. Here is one possible site-lisp
organization chart:

    site-lisp
    |
    +-common        Files common to Emacs and XEmacs
    |
    +-emacs                 Fieles than only work in Emacs
    +-xemacs        Fieles than only work in XEmacs
    +-net                   Packages available directly from internet
        |
        +-cvs-packages  by CVS pserver directories (Gnus, Mailcrypt ..)
        +-packages          complete kits, like Tamp, Notes, etc (multiple files)
        +-users     by User, Emacs Lisp developers
          |
          +-firedman-noah
          +-zakharevich-llya
          ..

If the entry is

  '(\"lisp-site-lisp\" \"site-lisp-flag.el\" 'rec)

then you would create file directly to the SITE-LISP ROOT,
/usr/share/site-lisp/site-lisp-flag.el and TinyIgrep.el will flag that
directory as searchable dir, effectively searching all of your lisp.

Similarly, you can drop 'flags' to other directories, like database entry

  '(\"lisp-site-lisp-net-users\" \"site-lisp-net-users-flag.el\" 'rec)

Format:

  '((DB-NAME lisp-flag-file-name recursive-search)
    (DB-NAME lisp-flag-file-name recursive-search)
    ..)."
  :type  '(list sexp)
  :group 'TinyIgrep)

(defcustom tinyigrep--perl-pod-path  nil ;Will be set later
  "*Perl installation POD directory."
  :type  '(repeat directory)
  :group 'TinyIgrep)

(defcustom tinyigrep--perl-inc-path nil ;Will be set later
  "*Perl @INC path list."
  :type  '(repeat directory)
  :group 'TinyIgrep)

(defcustom tinyigrep--man-path-root (ti::directory-unix-man-path-root)
  "*Man path root in the system. usually /usr/man/ or /opt/local/man."
  :type  'directory
  :group 'TinyIgrep)

(defcustom tinyigrep--database nil
  "*Igrep database for group of files.
Rule: The directories _must_ have trailing slashes.

There is one special entry named `tinyigrep--special-database' which
is treated differently.

You can use following entry to tell that it should be skipped, the
DB-NAME here is string \"nil\".

  '(\"nil\")

This is useful when you build the database in a variable and you test if
certain directories exist. Like this, which builds dynamically one
entry to the `tinyigrep--database' at evaluation time.

  (list
    (if (not (file-exists-p \"/not/in/this/host\"))
       \"nil\" \"my-path-db\")
     (list
      ...go and build the correct ENTRY))

Note [igrep find fag]:

    This optional argument is very important is you grep over many
    directories and many files. It is impossible to tell in the program
    if your defined criteas generate huge listing or not.

    Defining 3rd argument as list, says that we should call `igrep-find' and
    not igrep function to prevent \"Arg list too long\" error. This
    variable reflects `igrep-find-use-xargs', and because nil is valid
    value, you must express it in list format.

    Valid values and their intepretation is presented below. You may gain
    performance benefit with xargs since it will invoke fewer grep
    processes. In the other hand the -exec choise gives you feeback for
    every found file as it seaches them. In xargs case you have to wait
    untill the whole list has been generated.

    These values are same as in `igrep-find-use-xargs', only in list format:

    '(gnu)      find is called with ...  -print0 | xargs -0 -e grep
    '(not-nil)  find is called with ...  -print  | xargs -e grep
    '(nil)      find is called with ...  -exec grep -n -e grep

Adding an entry to the database

    There is a function that handles inserting entries to the database
    for you. It will replace existing entry or add a new one. The argument
    isa same as is described in Format below.

      (tinyigrep-db-push-elt '(\"Elisp\" (\"egrep\" (\"~/elisp/*el\"))))

Format:

    '((DB-NAME
      '(GREP-PROGRAM
        (DIR DIR ..)
        [(igrep-find flag)]))
      ..)

Alternatively the database entry can contain a lisp function that defines
the actual entry to the variables. For example a call:

    (tinyigrep-db-push-lazy-define \"test-db\" 'my-db-define-test-db)

Would add a database entry in format:

    '(\"test-db\" (my-db-define-test-db))

And the function `my-db-define-test-db' is invoked when user requests
\"test-db\" search. Tee function MUST DEFINE real entry with the same
name: \"test-db\" by calling e.g. `tinyigrep-db-push-elt'. See
source code of tinyigrep.el for examples of this 'deferred until called'
lazy defining."
  :type  'sexp
  :group 'TinyIgrep)

;;}}}
;;{{{ setup: private

;;; ....................................................... &v-private ...
;;; Private variables

(defvar tinyigrep--man-path-sections
  '("cat1.Z" "cat1m.Z"
    "cat2.Z" "cat3.Z"
    "cat4.Z" "cat5.Z"
    "cat6.Z" "cat7.Z" "cat8.Z"
    "man1" "man3" "man5" "man7" "man8")
  "*Possible manual sections under `tinyigrep--man-path-root'.
You can list non existing section here; they are automatically ignored
if they do not exist.")

(defvar tinyigrep--lisp-package-file-list
  '(("elisp-apel"       "poe.el")
    ("elisp-bbdb"       "bbdb.el")
    ("elisp-edb"        "db-file-io.el")
    ("elisp-ede"        "ede.el")
    ("elisp-efs"        "efs-auto.el")
    ("elisp-eieo"       "eieo.el")
    ("elisp-elib"       "elib-node.el")
    ("elisp-erc"        "erc.el")       ; IRC client
    ("elisp-eshell"     "eshell.el")
    ("elisp-flim"       "FLIM-VERSION")
    ("elisp-gnus"       "gnus.el")
    ("elisp-irchat"     "irchat-main.el" ) ; IRC client
    ("elisp-jde"        "jde.el")
    ("elisp-liece"      "liece.el")     ; IRC client
    ("elisp-mailcrypt"  "mailcrypt.el")
    ("elisp-mel"        "MEL-CFG")
    ("elisp-notes-mode" "notes-mode.el")
    ("elisp-psgml"      "psgml-mode.el")
    ("elisp-pcl-cvs"    "pcl-cvs.el")
    ("elisp-semi"       "semi-def.el")
    ("elisp-speedbar"   "speedbar.el")
    ("elisp-tiny"       "tinylibm.el")
    ("elisp-vm"         "vm" '(nil))
    ("elisp-w3"         "w3")
    ("elisp-xslide"     "xslide.el"))
  "*Lisp databases to search.
Format:

   '((DATABASE-NAME LISP-FILE-TO-SEARCH [RECURSIVE-OPTION])
     ...)

For example, to seach Gnus files, the entry looks like:

   '((\"lisp-gnus\" \"gnus.el\") ...)

Which means, that when gnus.el if found from path, that directory is
used as a base for searches. If you supply a recursive option '(nil),
then all directories below are searched as well.

   '((\"lisp-gnus\" \"gnus.el\" '(nil)) ...)")

(defvar tinyigrep--databases-lazy-defined
  '(("lisp-cl"
     tinyigrep-install-database-setup-lisp-cl)
    ("lisp-load-path"
     tinyigrep-install-database-setup-lisp-load-path)
    ("lisp-emacs-distribution"
     tinyigrep-install-database-setup-lisp-cl)
    ("perl-pod"
     tinyigrep-install-database-setup-perl-pod)
    ("perl-modules"
     tinyigrep-install-database-setup-perl-modules))
  "Databases whose definition is deferred until used.
Format:

   '((DATABASE DEFINE-FUNCTION) ...)

In practise it means that for each element in list the
following is called:

    (tinyigrep-db-push-lazy-define DATABASE DEFINE-FUNCTION).")

(defvar tinyigrep--databases-lisp-texi-list
  '(("texi-bbdb"           "bbdb.el")
    ("texi-edb"            "db-file-io.el")
    ("texi-ede"            "ede.el")
    ("texi-eieio"          "eieio.el")
    ("texi-elib"           "elib-node.el")
    ("texi-gnus"           "gnus.el")
    ("texi-irchat"         "irchat.el")
    ("texi-mailcrypt"      "mailcrypt.el")
    ("texi-pcl-cvs"        "pcl-cvs.el")
    ("texi-psgml"          "psgml.el")
    ("texi-w3"             "w3.el"))
  "Lisp *.texi file search databases.
Many times a bigger lisp package comes with a manual under name
package.texi, which you could grep easily. This may be faster than
searching the accompanying info files (if they are even installed).

Format:

  '((DATABASE LISP-FILE-TO-SEARCH-WHERE-TEXI-COULD-BE-FOUND) ...)")

(defvar tinyigrep--igrep-previous-args nil
  "List of variables used for calling igrep.")

(defvar tinyigrep--history-igrep nil
  "History.")

(defvar tinyigrep--history-database nil
  "History of previously used databases.")

(defvar tinyigrep--history-programs nil
  "History of used programs.")

(defvar tinyigrep--history-igrep-user-options nil
  "History.")

(defvar tinyigrep--last-database nil
  "Last selected database.")

(defvar tinyigrep--debug-buffer "*tinyigrep-debug*"
  "Debug data buffer.")

;;}}}
;;{{{ code: Cygwin support

;;; ----------------------------------------------------------------------
;;; Some code does not treat Cygwin environment properly, so you should do
;;; use this macro.
;;;
(put 'ti::expand-file-name-cygwin-macro 'lisp-indent-function 1)
(put 'ti::expand-file-name-cygwin-macro 'edebug-form-spec '(body))
(defmacro ti::expand-file-name-cygwin-macro (check-form &rest body)
  "Treat Cygwin path names specially and suppress `expand-file-name'.

Input:

  CHECK-FORM    Additional check for to verify Cygwin or supply t if there
                is nothing special to check.
  BODY          Forms to run."
  `(let ((igrep-null-device igrep-null-device)
         (CHECK  ,check-form))
     (unwind-protect
         (progn
           (when (and CHECK
                      (ti::emacs-type-win32-p)
                      (ti::win32-cygwin-p))
             (setq igrep-null-device "/dev/null")
             (ti::advice-control '(expand-file-name
                                   shell-quote-argument)
                                 "^tinylib-cygwin" nil))
           ,@body)
       (when (and CHECK
                  (ti::emacs-type-win32-p)
                  (ti::win32-cygwin-p))
         (ti::advice-control '(expand-file-name
                               shell-quote-argument)
                             "^tinylib-cygwin" 'disable)))))

;;  Install only for Native Win32 Emacs + Cygwin tools

(when (and (ti::emacs-type-win32-p)
           (ti::win32-cygwin-p))

  (require 'advice)

  (defadvice shell-quote-argument (around tinylib-cygwin-fix dis)
    "Use single quotes under Cygwin, Not win32 double quotes."
    (setq ad-return-value (format "'%s'" (ad-get-arg 0))))

  (defadvice expand-file-name (around tinylib-cygwin-fix dis)
    ;;  (expand-file-name NAME &optional DEFAULT-DIRECTORY)
    ;;
    ;;  function `igrep' calls:
    ;;
    ;;        (setq files
                                        ;,          (mapcar 'expand-file-name files)))
    ;;
    ;;  Which returns incorrect DOS-filenames for Cygwin grep called from
    ;;  Win32 Emacs
    ;;
    ;;  The same happens in Emacs 21.2 filecache.el
    ;;
    ;;  (defun file-cache-add-directory-using-find (directory)
    ;;     (let ((dir (expand-file-name directory)))
    ;;     ...
    ;;     ... call `find' binary
    ;;
    "Change function during call to `igrep' under Native Win32 Emacs + Cygwin.
The path is not expanded, but returned as is."
    (let ((arg0 (ad-get-arg 0))
          (arg1 (ad-get-arg 1)))
      (if (and arg1
               ;;  Igrep calls `shell-quote-argument' which puts extra
               ;;  wuotes around text:  \"path/filename\". Remove those
               (string-match "[\"']\\(.*[^\"']\\)" arg1))
          (setq arg1 (match-string 1 arg1)))
      (setq ad-return-value
            (if arg1
                (format "%s%s" arg1 arg0)
              arg0)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-cygwin-binary-p (prg)
  "Fix `igrep' under Win32 Emacs and Cygwin."
  (and (ti::win32-p)
       (ti::emacs-type-win32-p)
       (ti::win32-cygwin-p)
       (string-match "grep\\|\\.sh$" prg)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-cygwin-fixes ()
  "Under Native Win32 Emacs, use Cygwin executables, not Windows versions."
  (when (and (ti::emacs-type-win32-p)
             (ti::win32-cygwin-p))
    ;; Depending on exec-path, the "find" may be windows version, fix
    ;; it to cygwin version - the Real Find
    (when (and (boundp 'igrep-find-program)
               (string= igrep-find-program "find"))
      (let ((bin (ti::executable-find "find" 'cygwin)))
        (when bin
          (message
           (setq igrep-find-program bin)))))
    ;;  When igrep.el loads, there are many `defvar' calls to
    ;;  `shell-quote-args' which uses double-quotes under Native Win32. But
    ;;  for Cygwin single quotes are better. Change these:
    ;;
    ;;  find -type f "!" -name "*~" "!" -name "*,v" "!" -name "s.*" -name "*el" ...
    (when (and (boundp 'igrep-find-file-clause)
               igrep-find-file-clause)
      (setq igrep-find-file-clause
            (subst-char-in-string ?\" ?' igrep-find-file-clause)))
    (when (and (boundp 'igrep-find-prune-clause)
               igrep-find-prune-clause)
      (setq igrep-find-prune-clause
            (subst-char-in-string ?\" ?' igrep-find-prune-clause)))))

;;}}}
;;{{{ code: low-level function and macros

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-expand-file-name-os (path)
  "Expand PATH to correct OS. Under Cygwin, use Cygwin paths."
  (unless (stringp path)
    (error "TinyIgrep: argument PATH is not a string"))
  (ti::file-name-for-correct-system path (if (ti::win32-cygwin-p)
                                             'cygwin)))

;;; ----------------------------------------------------------------------
;;; (tinyigrep-db-lazy-define-funcall (assoc "perl-pod" tinyigrep--database))
;;;
(defun tinyigrep-db-lazy-define-funcall (elt)
  "Examine database ELT and call embedded function to define database.
This function activates only, if ELT is in format:

   '(\"database-name\" (lisp-function))

`lisp-function' is called and it should immediately define the
real entries -- that is: it should replace \"database-name\"
in `tinyigrep--database'."
  (let ((database (car elt))
        (function (car-safe (nth 1 elt))))
    (cond
     ((functionp function)
      (message "TinyIgrep: Initializing lazy defined database %s `%s'"
               (car elt) (prin1-to-string function))
      (funcall function)
      ;;  Did the function replace the entry?
      (when (functionp
             (car-safe (nth 1 (assoc database tinyigrep--database))))
        (error "TinyIgrep: function `%s' did not define database `%s'."
               (prin1-to-string function) database)))
     ((symbolp function)
      (error "TinyIgrep: Can't define database. No callable function [%s]"
             (prin1-to-string elt))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-lazy-define (database function)
  "Add DATABASE name and FUNCTION to `tinyigrep--database'.
The FUNCTION si called when user selects DATABASE and it
should call `tinyigrep-db-push-elt' to define the real
search database entry.

In other words: The FUNCTION is used as a placeholder
and to forward declare a DATABASE which it will define
at the point of calling."
  (let ((member (assoc database tinyigrep--database)))
    (if member
        (setq tinyigrep--database (delete member tinyigrep--database)))
    (push (list database (list function)) tinyigrep--database)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt (elt)
  "Replace existing ELT in the `tinyigrep--database' or add new one.
If you want to denote a directory, make sure the last character is slash.

Examples:

   ;;  With recursion, see (nil) argument

   (tinyigrep-db-push-elt '(\"my-perl\" (\"egrep\" (\"~/bin/perl/\") (nil) )))
   (tinyigrep-db-push-elt '(\"my-bin\"  (\"egrep\" (\"~/bin/\") (nil) )))

   ;; Without recursion, filename spec mst be included: `*'

   (tinyigrep-db-push-elt '(\"news-all\"      (\"egrep\" (\"~/News/*\"))))
   (tinyigrep-db-push-elt '(\"news-Incoming\" (\"egrep\" (\"~/Mail/Inc*\"))))

   ;;  Easy and free web server http://www.xitami.com/

   (tinyigrep-db-push-elt
    '(\"Xitami-root\"
      (\"egrep\"
       (\"d:/bin/server/xitami/*cfg\"
        \"d:/bin/server/xitami/*txt\"
        \"d:/bin/server/xitami/*aut\"))))"
  (when (and elt (not (equal "nil" (car-safe elt))))
    (let ((member (assoc (car elt) tinyigrep--database)))
      (if member
          (setq tinyigrep--database (delete member tinyigrep--database)))
      (push elt tinyigrep--database))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyigrep-db-push-elt-lisp-package (name file &optional grep method)
  "Push NAME into `tinyigrep--database' if FILE found. Use GREP for search.
This means, that if FILE exists, rthe directory where it was found
is searched for *el files.

Input:

  NAME      name of the database entry.
  FILE      file to find.
  GREP      program used to find. Default is `egrep'.
  METHOD    additional recursive grep method.

Examples:

  ;;    Define shorter names. The default database names are prefixed with
  ;;    lisp- These don't need recursice search.

  (dolist (package '(\"vm\" \"irchat\" \"semi-def\" \"mc\" \"tinylib\" \"bbdb\"))
    (tinyigrep-db-push-elt-lisp-package package (concat package \".el\")))

  ;;    Recursively seached

  (tinyigrep-db-push-elt-lisp-package \"gnus\" \"gnus.el\" \"egrep\" '(nil) )"
  (tinyigrep-db-push-elt
   (tinyigrep-db-lisp-elt
    file name (or grep  "egrep")
    '(list (concat dir "*el")) method)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt-package (name package &optional recursive grep)
  "Find PACKAGE and create NAME entry to database for RECURSIVE '(nil).

Example:

  (dolist (elt '(
                 (\"lisp-bbdb\"     \"bbdb.el\")
                 (\"lisp-ede\"      \"ede.el\")
                 (\"lisp-efs\"      \"efs-auto.el\")
                 (\"lisp-eieo\"     \"eieo.el\")))
  (tinyigrep-db-push-elt-lisp-package
   (nth 0 elt)
   (nth 1 elt)
   \"egrep\"
   (nth 2 elt) ))"
  (let ((path (locate-library package)))
    (when path
      (setq path (file-name-directory path))
      (tinyigrep-db-push-elt
       (list
        name
        (list (or grep "grep")
              (list (concat path "*el"))
              (if recursive
                  '(nil))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-push-elt-package-texi
  (name package &optional recursive grep)
  "Find PACKAGE texi and create NAME entry to database for RECURSIVE '(nil).

Examples:

  (dolist (elt '((\"texi-bbdb\"     .  \"bbdb.el\")
                 (\"texi-ede\"      .  \"ede.el\")
                 (\"texi-eieio\"    .  \"eieio.el\")))
    (tinyigrep-db-push-elt-package-texi (car elt) (cdr elt) nil \"egrep\"))"
  (let ((root     (locate-library package))
        (choices  '("texi/" "tex/" "")))
    (catch 'done
      (when root
        (setq root (file-name-directory root))
        (dolist (try (list root
                           (file-name-as-directory (ti::directory-up root))))
          (dolist (dir choices)
            (setq dir (concat try dir))
            (when (and (file-directory-p dir)
                       (directory-files dir nil "\\.te?xi"))
              (tinyigrep-db-push-elt
               (list name (list grep (list (concat dir "*.texi*") ))))
              (throw 'done dir))))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinyigrep-countdown (message count &optional msg)
  "Show (format MESSAGE COUNT MSG) and decrease COUNT."
  `(progn
     (decf ,count)
     (message (format ,message ,count (or ,msg "") ))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-activate-perl-support ()
  "Add support for Perl POD manual pages.
The Perl support is consulted from package tinyperl.el.

Return:
  non-nil if `tinyigrep--perl-pod-path' and
  `tinyigrep--perl-inc-path' were defined here."
  ;;  Ask from perl what paths are in @INC. That's what we want
  ;;  to search.
  (when (or (executable-find "perl")
            (executable-find "perl5"))
    (message "TinyIgrep: Perl detected, consulting tinyperl.el...")
    (cond
     ((or (featurep 'tinyperl)
          (load "tinyperl" 'noerr))
      (unless tinyperl--pod-path
        (tinyperl-install))
      (setq tinyigrep--perl-pod-path    tinyperl--pod-path)
      (setq tinyigrep--perl-inc-path    tinyperl--inc-path))
     (t
      (message
       "TinyIgrep: Sorry, tinyperl.el not found. Can't add Perl suport.")
      nil))))

;;}}}
;;{{{ code: lazy defined databases

;;; ----------------------------------------------------------------------
;;; (assoc "perl-pod" tinyigrep--database)
;;;
(defun tinyigrep-install-database-setup-perl-pod (&optional grep)
  "Install Perl search databases."
  (or grep
      (setq grep tinyigrep--grep-program))
  (when (tinyigrep-activate-perl-support)
    (message "TinyIgrep: activating database `perl-pod'")
    (let ((path tinyigrep--perl-pod-path))
      (when path
        (tinyigrep-db-push-elt
         (list "perl-pod"
               (list grep
                     (list (format
                            "%s*pod"
                            (file-name-as-directory path))))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-setup-perl-modules (&optional grep)
  "Install Perl search databases."
  (or grep
      (setq grep tinyigrep--grep-program))
  (when (tinyigrep-activate-perl-support)
    (message "TinyIgrep: activating database `perl-modules'")
    (let ((path tinyigrep--perl-inc-path))
      (tinyigrep-db-push-elt
       (list "perl-modules"
             (list grep
                   (mapcar
                    (function
                     (lambda (x)
                       (format "%s*pm" x)))
                    path)
                   '(nil)))))))
;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-setup-lisp-rc-files (&optional grep)
  "Emacs Lisp *-rc-* file search database."
  (or grep
      (setq grep tinyigrep--grep-program))
  (let (list)
    (message "TinyIgrep: activating database `lisp-rc-files'")
    (dolist (path load-path)
      (when (and (stringp path)
                 (file-directory-p path))
        (dolist (file (directory-files path))
          (when (string-match "-rc-" file)
            (push (concat (file-name-as-directory path)
                          "*-rc-*el")
                  list)
            (return)))))
    (when list
      (tinyigrep-db-push-elt (list "lisp-rc-files" (list grep list))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-setup-lisp-cl (&optional grep)
  "Install cl*.el search databases."
  (or grep
      (setq grep tinyigrep--grep-program))
  ;;  Find the Emacs lisp root directory dynamically
  (let ((path-cl (locate-library "cl.el"))
	root)
    (when path-cl
      (setq path-cl (file-name-directory path-cl)
            root    path-cl)
      ;;  Emacs 20.7 cl.el is one directory down from root
      ;;  in emacs-20.6/lisp/emacs-lisp/cl.el, but we want the root
      (when (string-match ".*[0-9]/lisp/" path-cl)
        (setq root (match-string 0 path-cl)))
      (tinyigrep-db-push-elt
       (list
        "lisp-emacs-distribution"
        (list grep
              (list (concat root "*el"))
              '(nil))))
      (tinyigrep-db-push-elt
       (list
        "lisp-cl"
        (list grep
              (list (concat path-cl "cl*el"))
              '(nil)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-setup-lisp-load-path (&optional grep)
  "Install Emacs Lisp `load-path' search database."
  (or grep
      (setq grep tinyigrep--grep-program))
  (message "TinyIgrep: activating database `lisp-load-path'")
  (let ((root-list (ti::directory-unique-roots load-path)))
    (when root-list
      (let ((database
             (list grep
                   (delq nil
                         (mapcar
                          (function
                           (lambda (x)
                             (when (and (stringp x)
                                        (file-directory-p x))
                               (concat (file-name-as-directory x) "*.el"))))
                          root-list))
                   '(nil))))
        (tinyigrep-db-push-elt (list "lisp-load-path" database))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-lisp-packages-lazy (&optional grep)
  "Define lisp package databases. This utilizes deferred lazy loading.
References:
  `tinyigrep--lisp-package-file-list'."
  (or grep
      (setq grep tinyigrep--grep-program))
  (let* ((base  "tinyigrep-install-database-setup-lisp-package-")
         (list  tinyigrep--lisp-package-file-list)
         (count (length list))
         def
         sym)
    (dolist (elt list)
      (multiple-value-bind (db lisp-file recursive) elt
        (setq sym (intern (format "%s%s" base db)))
        (setq def
              `(defun ,sym (&optional grep)
		 "Define lisp package database"
		 (or grep
		     (setq grep tinyigrep--grep-program))
		 (tinyigrep-db-push-elt-lisp-package
		  ,db
		  ,lisp-file
		  grep
		  ,recursive )))
        (tinyigrep-countdown
         (concat
          "TinyIgrep: Wait, initialising `tinyigrep--lisp-package-file-list'"
          "lazy... %d %s")
         count
	 (format "[%s]" db) )
        ;;  Create functions on-the-fly
        (eval def)
        (tinyigrep-db-push-lazy-define db sym)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-lisp-flag-files-lazy (&optional grep)
  "Define lisp search databases. This utilizes deferred lazy loading.
References:
  `tinyigrep--flag-file-list'."
  (or grep
      (setq grep tinyigrep--grep-program))
  (let* ((base  "tinyigrep-install-database-setup-lisp-flag-")
         ;;  Create these files with touch(1) to the lisp
         ;;  root directories
         (list  tinyigrep--lisp-package-file-list)
         (count (length list))
         def
         sym)
    (dolist (elt list)
      (multiple-value-bind (db lisp-file recursive) elt
        (setq sym (intern (format "%s%s" base db)))
        (setq def
              `(defun ,sym (&optional grep)
                   "Define lisp package database"
                   (or grep
                       (setq grep tinyigrep--grep-program))
                   (tinyigrep-db-push-elt-package
                    ,db
                    ,lisp-file
                    ,recursive )))
        (tinyigrep-countdown
         (concat
          "TinyIgrep: Wait, initialising "
          "`tinyigrep--lisp-package-file-list' lazy... %d %s")
         count (format "[%s]" db) )
        ;;  Create functions on-the-fly
        (eval def)
        (tinyigrep-db-push-lazy-define db sym)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-lisp-texi-lazy (&optional grep)
  "Define lisp *.texi search databases. This utilizes deferred lazy loading.
References:
  `tinyigrep--databases-lisp-texi-list'."
  (or grep
      (setq grep tinyigrep--grep-program))
  (let* ((base  "tinyigrep-install-database-setup-lisp-texi-")
         ;;  Create these files with touch(1) to the lisp
         ;;  root directories
         (list  tinyigrep--databases-lisp-texi-list)
         (count (length list))
         def
         sym)
    (dolist (elt list)
      (multiple-value-bind (db lisp-file) ;; ... recursive)
          elt
        (setq sym (intern (format "%s%s" base db)))
        (setq def
              `(defun ,sym (&optional grep)
		 "Define lisp package database"
		 (or grep
		     (setq grep tinyigrep--grep-program))
		 (tinyigrep-db-push-elt-package-texi
		  ,db
		  ,lisp-file
		  nil
		  grep )))
        (tinyigrep-countdown
         (concat
          "TinyIgrep: Wait, initialising "
          "`tinyigrep--lisp-texi-database-list' lazy... %d %s")
         count (format "[%s]" db) )
        ;;  Create functions on-the-fly
        (eval def)
        (tinyigrep-db-push-lazy-define db sym)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-database-lazy ()
  "Install lazy defined databases in `tinyigrep--databases-lazy-defined'."
  (let* ((list tinyigrep--databases-lazy-defined)
         (count  (length list)))
    (dolist (elt list)
      (multiple-value-bind (db function) elt
        (tinyigrep-countdown
         "TinyIgrep: Wait, initialising default lazy database... %d %s"
         count
	 (format "[%s]" db))
        (tinyigrep-db-push-lazy-define db function)))))

;;}}}
;;{{{ code: default database

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-install-default-databases (&optional grep)
  "Install default Emacs, Info, Perl: Man entries to `tinyigrep--database'.
GREP is program to used for grepping. Default is `egrep'."
  (interactive)
  (let ((count 25)
	(msg "TinyIgrep: Wait, initialising default databases... %d %s"))
    (tinyigrep-install-database-lazy)
    (tinyigrep-install-database-lisp-packages-lazy)
    (tinyigrep-install-database-lisp-flag-files-lazy)
    (tinyigrep-install-database-lisp-texi-lazy)
    (or grep
        (setq grep tinyigrep--grep-program))
    (tinyigrep-countdown msg count "[start]" )
    ;; .................................................... &current-dir ...
    ;; Make sure this entry is included.
    ;; Copy this exactly like below, you may only change
    ;; the GREP program name. It greps from the current file directory,
    ;; where buffer is.
    (tinyigrep-db-push-elt
     (list tinyigrep--special-database (list grep '(nil))))
    ;; ...................................................... &man-pages ...
    (tinyigrep-countdown msg count "[man pages]" )
    (when (and tinyigrep--man-path-root
               (file-exists-p tinyigrep--man-path-root))
      (list
       "man"
       (list
        "zgrep"
        (union

         (mapcar ;; These are system's man paths
          (function
           (lambda (x)
             (setq x (concat (file-name-as-directory
                              tinyigrep--man-path-root) x))
             (when (and (stringp x)
                        (file-directory-p x))
               (concat (file-name-as-directory x) "*"))))
          tinyigrep--man-path-sections)
         (delq nil
               (mapcar
                (function
                 (lambda (x)
                   (when (and (stringp x)
                              (file-directory-p x))
                     (concat (expand-file-name x) "*" ))))
                '( ;; Add also extra man paths
                  "~/man/"
                  "/usr/local/man/"
                  "/usr/contrib/man/"))))
        ;;  Must be recursive
        '(non-nil))))
    ;; ........................................................... &home ...
    ;; shell programs
    (tinyigrep-countdown msg count "[home bin]" )
    (when (file-directory-p "~/bin")
      (tinyigrep-db-push-elt (list "home-bin-sh"
                                   (list grep
                                         (list
                                          "~/bin/*sh" "~/bin/*awk"))))
      (tinyigrep-db-push-elt (list "home-bin-perl"
                                   (list grep
                                         (list
                                          "~/bin/*.pl"
                                          "~/bin/*.pm")))))
    (tinyigrep-countdown msg count "[home Mail]" )
    (when (file-directory-p "~/Mail")
      (tinyigrep-db-push-elt
       (list "home-Mail"
             (list grep (list "~/Mail/" '(nil))))))
    (tinyigrep-countdown msg count "[home News]" )
    (when (file-directory-p "~/News")
      (tinyigrep-db-push-elt
       (list "home-News"
             (list grep (list "~/News/" '(nil))))))
    ;; .................................................... &usr-include ...
    (tinyigrep-countdown msg count "[usr include]" )
    (tinyigrep-db-push-elt
     (list
      (if (file-exists-p "/usr/include/")
          "c-usr-include"  "nil")
      (list
       grep
       (list
        "/usr/include/")
       '(nil))))
    ;; .......................................................... &elisp ...
    ;; Private home lisp directory
    (tinyigrep-countdown msg count "[lisp HOME files]" )
    (let (path-list)
      (dolist (path '("~/elisp"
                      "~/lisp"
                      "~/.xemacs"
                      "~/.emacs.d"
                      "~/.emacs"))
        (if (file-directory-p path)
            (push path path-list)))
      (when path-list
        (tinyigrep-db-push-elt
         (list "lisp-home"
               (list grep path-list '(nil)  )))))
    ;; ................................................. &elisp-rc-files ...
    ;;  find directories that contain files starting with .emacs*
    ;;  These are Emacs initialisation or setup files.
    (tinyigrep-countdown msg count "[lisp dot files]")
    (let (list)
      (dolist (path load-path)
        (when (stringp path)
          (push (concat (file-name-as-directory path)
                        ".*")
                list)))
      (tinyigrep-db-push-elt
       (list "lisp-dot-files" (list grep list))))
    ;; ............................................. &emacs-distribution ...
    (tinyigrep-countdown msg count "[emacs all current]" )
    (let ((root (ti::emacs-install-root)))
      (when root
        (tinyigrep-db-push-elt
         (list "ti::emacs-install-root-current"
               (list
                grep
                (list (concat (ti::emacs-install-root) "/*"))
                '(nil))))
        (tinyigrep-countdown msg count "[emacs all others/up dir]" )
        ;; See if thre are more Emacs version installed in the same
        ;; level and add search to install database as well
        (setq root (file-name-as-directory (ti::directory-up root)))
        (let ((dirs (ti::directory-subdirs root))
	      name)
          (dolist (path dirs)
            (when (string-match
                   "^\\([Xx]?[Ee]macs-\\)[0-9]+\\.[0-9.]+$"
                   path)
              (setq name (concat "ti::emacs-install-root-" path))
              (tinyigrep-db-push-elt
               (list name
                     (list
                      grep
                      (list (concat root path "/*"))
                      '(nil)))))))))
    ;; ........................................................ Cygwin ...
    (tinyigrep-countdown msg count "[Cygwin]" )
    (let ((root (ti::win32-cygwin-p))
          (dir  "/usr/doc"))
      (when root
        (if (ti::emacs-type-win32-p)
            (setq dir (concat root dir)))
        (when (file-directory-p dir)
          (tinyigrep-db-push-elt
           (list "cygwin-doc"
                 (list grep
                       (list
                        (concat dir "/*")
                        '(nil))))))))
    ;; ............................................................ MAIL ...
    (tinyigrep-countdown msg count "[Mail]" )
    (when (file-directory-p "~/Mail/")
      (tinyigrep-db-push-elt (list "Mail" (list grep '("~/Mail/*") '(nil)))))
    (tinyigrep-countdown msg count)
    (tinyigrep-countdown msg count "[News]" )
    (when (file-directory-p "~/News/")
      (tinyigrep-db-push-elt (list "News" (list grep '("~/News/*") '(nil)))))
    (tinyigrep-countdown msg count)
    ;; ................................................. &elisp-packages ...
    (tinyigrep-countdown msg count "[lisp ediff]" )
    (let ((path (locate-library "ediff.el")))
      (when path
        (setq path (file-name-directory path))
        (tinyigrep-db-push-elt
         (list "lisp-ediff"
               (list grep (list (concat path "ediff*el"))  nil )))))
    (tinyigrep-countdown msg count "[lisp packages]" )
    (tinyigrep-install-database-lisp-packages-lazy)
    ;;   If you have SEMI, you propably have installed it so that there is
    ;;   ROOT directory under which you have put SEMI, APEL, FLIM, CHAO
    ;;
    ;;   elisp
    ;;     semi-mime-root
    ;;     |
    ;;     --- flim
    ;;     --- apel
    ;;     --- mel
    ;;     --- semi-1.9.2
    ;; (tinyigrep-countdown msg count "[lisp SEMI]" )
    ;;
    ;; (let ((path (locate-library "semi-def.el")))
    ;;   (when path
    ;;     (setq path  (if path (ti::directory-up (file-name-directory path))))
    ;;     (tinyigrep-db-push-elt
    ;;      (list "lisp-semi-root"
    ;;            (list grep   (list (concat path "*el"))  '(nil) )))))
    ;; ..................................................... &emacs-info ...
    (tinyigrep-countdown msg count "[info elisp]" )
    (tinyigrep-db-push-elt
     (list
      "info-elisp"
      (list grep
            (mapcar
             (function
              (lambda (x)
                (when (and (stringp x)
                           (file-directory-p x))
                  (concat (file-name-as-directory x) "*elisp*"))))
             (ti::compat-Info-directory-list)))))
    (tinyigrep-countdown msg count "[info emacs]" )
    (tinyigrep-db-push-elt
     (list
      "info-emacs"
      (list grep
            (mapcar
             (function
              (lambda (x)
                (when (and (stringp x)
                           (file-directory-p x))
                  (concat (file-name-as-directory x) "*emacs*"))))
             (ti::compat-Info-directory-list)))))
    (tinyigrep-countdown msg count "[info all]" )
    (tinyigrep-db-push-elt
     (list
      "info-all"
      (list grep
            (mapcar
             (function
              (lambda (x)
                (when (and (stringp x)
                           (file-directory-p x))
                  (concat (file-name-as-directory x) "*info*"))))
             (ti::compat-Info-directory-list)))))
    (tinyigrep-countdown msg count "[info Gnus]" )
    (tinyigrep-db-push-elt
     (list
      "info-gnus"
      (list "zgrep" ;; unser linux the files are in compressed form
            (mapcar
             (function
              (lambda (x)
                (when (and (stringp x)
                           (file-directory-p x))
                  (concat (file-name-as-directory x) "*gnus*"))))
             (ti::compat-Info-directory-list)))))
    (message "TinyIgrep: Wait, initialising default databases...done")))


;;;### (autoload 'tinyigrep-debug-toggle "tinyigrep" t t)
(eval-and-compile (ti::macrof-debug-standard "tinyigrep" ":-"))

;;}}}
;;{{{ Install

(defvar tinyigrep--menu
  '((format
     "\
%s%sigrep: i)grep d)ata l)ast D)ir v)er - c)ase r)ecur u)ser [%s]"
     (if (eval (tinyigrep-recursive-var))  "R " "")
     (if igrep-options (concat igrep-options " ") "")
     (if (stringp tinyigrep--last-database)
         tinyigrep--last-database
       ""))
    ((?i  . ( (call-interactively 'igrep)))
     (?D  . ( (call-interactively 'dired-do-igrep)))
     (?d  . ( (call-interactively 'tinyigrep-main)))
     (?l  . ( (call-interactively 'tinyigrep-as-last-time)))
     (?c  . (t
             (progn
               (if (string= "-i" (or igrep-options ""))
                   (setq igrep-options nil)
                 (setq igrep-options "-i")))))
     (?u  . (t
             (let (opt)
               (setq
                opt
                (read-from-minibuffer
                 "Set igrep options: "
                 nil nil tinyigrep--history-igrep-user-options))

               ;;  Should I check something here before doing the assignment?
               (setq igrep-options opt))))
     (?r  . (t (let ((sym (tinyigrep-recursive-var)))
                 ;;  toggle value
                 (if (eval sym)
                     (set sym nil)
                   (set sym t)))))
     (?v . ( (progn (tinyigrep-version))))))
  "TinyIgrep echo area menu.
The complete package manual is available at M-x tinyigrep-version

Commands:

i = Run `igrep'. This is the standard grep interface that Emacs
    has had for ages. Gives you a command line prompt where you
    can write the command and parameters.

d = use databases. You can select from predefined
    databases that were set up at package load time. It is possible
    to define your own custom search directories and give it a
    search \"name\". See more with M-x tinyigrep-version.

    If you supply a `tinyigrep--special-database', defualt is dot(.),
    you can grep files under current buffer's file directory.

l = Use same database for searching as last time. The name of the last
    database is rightmost string that us displayd after brackets [].

D = Run `dired-do-grep' which see.

v = Run `tinyigrep-version' which will print the package's manual.

Options:

c = Toggle case sensitive option `-i' in grep searches.
u = User option. Prompt for custom grep options.
r = Toggle recursive option `igrep-find'.")

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyigrep-menu (&optional arg)
  "Igrep command menu."
  (interactive "P")
  (ti::menu-menu 'tinyigrep--menu arg))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyigrep-check-files (files)
  "Check that FILES can be grepped."
  (when files
    (dolist (elt (ti::list-make files))
      (if (ti::file-name-remote-p elt)
          (error "TinyIgrep: Remote file name is not supported: %s" elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-db-lisp-elt (file name prg list &optional method)
  "Return tigr entry if FILE was found.

Input:

  FILE          package.el or Directory. This variable is evaluated.
  NAME          the database completions name
  PRG           grep program to use
  LIST          files definitions and directories to grep
  METHOD        additional recursive grep method

You can refer to variable `dir' if path was found.

Example:

  (igr-push-elt
   (tinyigrep-db-lisp-elt
    \"bbdb.el\" \"bbdb\" \"zgrep\"
    '(list (concat dir \"*\"))))

This will dynamically find the directory where bbdb.el is stored and
assign local variable `dir' to it (which you see used here).
If bbdb.el is not found, then this return valid 'null' entry."
  (let* ((some  (eval file))
         (path  (when (stringp some)
                  (if (file-directory-p some)
                      some
                    (locate-library some))))
         (dir  (if path (file-name-directory path))))
    (if (null dir)
        (list "nil")
      (list name (list prg (eval list) method)))))

;;}}}
;;{{{ Igrep

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-fix-program-path (program)
  "Fix PROGRAM path according to environment.
Under Win32, Emacs cannot call Cygwin shell scripts.
Find out the full path for PROGRAM."
  (when (and (stringp program)
             ;; egrep.exe is in Win32. Do not check.
             (not (string-match "^grep$" program))
             ;;  There is cygwin XEmacs, howabout native xemacs? #todo:
             (ti::emacs-p)
             (ti::win32-cygwin-p 'use-cache))
    (let (bin)
      ;;  Do we have a cached value?
      (unless (setq bin (get 'tinyigrep-fix-program-path program))
        ;;  Find out path for program
        (setq bin (executable-find program))
        (unless bin
          (setq bin (ti::file-get-load-path program exec-path)))
        (when bin
          (setq program (ti::file-name-forward-slashes bin))
          ;;  SAve value, since lookups are expensive.
          (put 'tinyigrep-fix-program-path bin program)))))
  program)

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-recursive-var ()
  "Return igrep variable name."
  (if (boundp 'igrep-recursively)
      'igrep-recursively
    ;;  Newer, 2.55
    'igrep-find))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-fix-word (word)
  "Set WORD to '' if it contain only repeated chars.
Fix other things too."
  ;;  Be a bit nice to user; if he sits on repeated line like
  ;;  '------------------------' there is no point of
  ;;  offerering that as initial string.
  (if (and (> (length word) 5)
           (string=
            (make-string (length word)
                         (string-to-char (substring word 0 1)))
            word))
      (setq word ""))
  ;;    Remove grabbed parenthesis and symbol(') ticks
  (when (stringp word)
    (setq word (replace-regexp-in-string "[?!`()'\"\r\n]" "" word)))
  (ti::remove-properties word))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-igrep-call (prg pattern files &optional use-find)
  "Call igrep.el with PRG PATTERN and FILES and recursive USE-FIND."
  (let ((fid "tinyigrep-igrep-call:"))
    ;; ti::with-unix-shell-environment
    ;;  (call-process
    ;;   "h:/unix-root/u/bin/bash.exe"
    ;;   nil
    ;;   (current-buffer)
    ;;   "-c"
    ;;   "h:/unix-root/u/bin/zgrep"
    ;;   "--help"))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinyigrep-debug fid "Calling IGREP"
                     'default-directory default-directory
                     'prg      prg
                     'pattern  pattern
                     'files    files)
    (ti::expand-file-name-cygwin-macro
     (tinyigrep-cygwin-binary-p prg)
     (ti::with-unix-shell-environment
       (if (ti::listp use-find)
           (let ((igrep-find-use-xargs (car use-find)))
             (igrep-find prg pattern files))
         (igrep prg pattern files))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-as-last-time (pattern arg-list)
  "Call `igrep' like last time, with same args. Allow editing.
The word to be grepped and the passed args can be changed.
PATTERN is new search patter and ARG-LIST is original argument list."
  (interactive
   (let* ((fid     "tinyigrep-as-last-time: ")
          (args    tinyigrep--igrep-previous-args)
          (list    (mapcar 'prin1-to-string args))
          (alist   (ti::list-to-assoc-menu list))
          (word    (tinyigrep-fix-word (or (ti::buffer-read-space-word) "")))
          (level-basic   (eq tinyigrep--user-level 'basic))
          dir
          sel
          elt)
     (unless fid ;; No-op. XEmacs byte compiler silencer
       (setq fid nil))
     (tinyigrep-debug fid "interactive in:" args word)
     (if (null args)
         (error
          (concat "TinyIgrep: Sorry, no saved call arguments "
                  "in memory. Call search first.")))
     (setq word
           (read-from-minibuffer
            "Igrep pattern: "
            (ti::string-left word 40)
            nil nil
            'tinyigrep--history-igrep))

     (if level-basic
         (setq elt (car args))
       (setq sel                        ;Previous args, allow changing
             (completing-read
              "select: "
              alist
              nil
              nil
              (car list)
              'list))
       ;;  Read possibly modified entry
       (setq elt (read sel)))
     (tinyigrep-debug fid "interactive out: " word elt)
     (list word elt)))
  ;; ................................................. interactive-end ...
  (let ((default-directory     default-directory)
	(igrep-program         igrep-program) ;we may change these
	use-find
	files)
    (tinyigrep-debug fid "in: " pattern arg-list)
    (if (not (ti::listp arg-list))
        (error "Tinyigrep: No previous database call arguments saved."))
    (setq default-directory     (nth 0 arg-list)
          igrep-program         (nth 1 arg-list)
          ;;   2 is the pattern we don't care now
          files                 (nth 3 arg-list)
          use-find              (nth 4 arg-list))
    ;; (ti::d!! 'dir default-directory 'args arg-list)
    (tinyigrep-igrep-call igrep-program pattern files use-find)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-determine-grep-program (file-list)
  "Guess proper grep program for FILE-LIST."
  (when file-list
    (let ((prg "egrep"))
      (dolist (file file-list)
        (cond
         ((string-match "z2$" file)
          (setq prg "bzgrep")
          (return))
         ((string-match "gz$" file)
          (setq prg "zgrep")
          (return))))
      prg)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-main-read-args ()
  "Ask args to igrep.
If you press RETURN when this function asks for database, then you
should give directory and file list shell regexp what to match.

References:
  `tinyigrep--database'"
  (let* ((fid                   "tinyigrep-main-read-args:")
         (table                 tinyigrep--database)
         (file                  (buffer-file-name))
         (extension             (and file (file-name-extension file)))
         ;;  Remove entries named "nil", do not offer them when
         ;;  completing the DB name
         (car-list              (delete
                                 nil
                                 (delete "nil" (mapcar 'car table))))
         (table-completions     (ti::list-to-assoc-menu car-list))
         (word   (and tinyigrep--grep-word-at-point
                      (tinyigrep-fix-word
                       (or (ti::buffer-read-space-word) ""))))
         (bfn    (or (buffer-file-name) ""))
         (ext    (cond
                  ((string-match "\\.[Cch][ch]?$" bfn)
                   "*[ch]")
                  ((string-match "\\.java$" bfn)
                   "*.java")
                  ((string-match "\\.el$" bfn)
                   (concat
                    (char-to-string
                     (aref (file-name-nondirectory bfn) 0)) ;first char
                    "*el"))
                  (t
                   (or (and extension (concat "*." extension))
                       (and file (ti::list-find auto-mode-alist file))
                       "*"))))
;;;      (info   (ti::string-match "^[^-]+" 0
;;;                         (symbol-name
;;;                          (or (ti::id-info 'symbol) major-mode))))

         (program-completions
          (ti::list-to-assoc-menu
           '("bzgrep"
             "zgrep"
             "egrep"
             "grep")))
         completion-ignore-case         ; Be case sensitive
         use-find
         prg
         pattern
         files
         ans
         db-elt
         elt
         ret)
    (unless fid ;;  XEmacs byte cimpiler silencer
      (setq fid nil))
    (tinyigrep-debug fid "in:" bfn ext word car-list)
    (setq tinyigrep--last-database "nil")
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  db ...
    (or
     (progn
       (setq ans (completing-read
                  "TinyIgrep search database [RET=next choice]: "
                  table-completions
                  nil ;; PREDICATE
                  'match
                  nil
                  'tinyigrep--history-database))
       (tinyigrep-debug fid "selected DB answer" ans)
       ;; Did we get a valid database ?
       (unless (or (ti::nil-p ans)
                   (null (setq db-elt (assoc ans table))))
         (setq tinyigrep--last-database ans)

         ;; In case the element is a lisp function, let that define the
         ;; database. (Lazy-define)
         (tinyigrep-db-lazy-define-funcall db-elt)
         ;;  Now the entry has been defined.
         (setq elt (nth 1 (assoc ans tinyigrep--database)))
         (setq prg      (nth 0 elt)
               files    (delq nil (nth 1 elt))
               use-find (and (> (length elt) 2)
                             ;; If there is 3rd element, get it
                             (nth 2 elt)))
         (if (null files)
             (error "Tinyigrep: Odd? No files to search"))
         (tinyigrep-debug fid "prog db ans selected:"
                          files prg "use find" use-find)
         ;; progn ret val
         t))
     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ask ...
     (progn
       ;;  Read each directory and file to grep
       (let ((dir (abbreviate-file-name default-directory))
             grep-prg)
         (if (> (length dir) 40)
             (setq dir (concat "(...)" (ti::string-right dir 40))))
         (setq files (ti::file-read-file-list
                      (format "%s Search files or patterns [TAB]: " dir)))
         (tinyigrep-debug fid "prog asked files:" files)
         (if (null files)
             (error "Tinyigrep: No files to search"))
         (setq grep-prg (tinyigrep-determine-grep-program files))
         (setq prg
               (completing-read
                "Grep program: "
                program-completions
                nil
                nil
                grep-prg
                'tinyigrep--history-programs)))))

    ;; -----------------------------------------------------------------
    ;; What did we get from user?
    ;; -----------------------------------------------------------------
    (setq ret t)
    ;; ... ... ... ... ... ... ... ... ... ... ... ...  special / db . .
    (cond
     ((string= ans tinyigrep--special-database)
      (or (setq files
                (let ((ans (read-from-minibuffer
                            "TinyIgrep file pattern(s): ")))
                  (mapcar '(lambda (x)
                             (format "%s%s"
                                     (file-name-as-directory
                                      default-directory)
                                     x))
                          (split-string ans))))
          (error "TinyIgrep: No files for `%s'" ans))
      (setq pattern
            (read-from-minibuffer
             "grep expression: "
             word nil nil
             'tinyigrep--history-igrep))
      (tinyigrep-debug fid "cond special db files:" pattern files))
     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... rest ..
     (t
      (setq pattern (read-from-minibuffer
                     "grep expression: "
                     (and word
                          (ti::string-left word 40)) ;limit length
                     nil
                     nil
                     'tinyigrep--history-igrep))
      (setq file (delq nil files))
      (setq files (and files
                       (mapcar 'tinyigrep-expand-file-name-os files)))))
    (tinyigrep-debug fid "out:" default-directory
                     "-" prg pattern files ret use-find)
    (setq prg (tinyigrep-fix-program-path prg))
    (tinyigrep-check-files files)
    (list
     (ti::remove-properties prg)
     (ti::remove-properties pattern)
     files
     ret
     use-find)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyigrep-main (&optional prg pattern files do-it use-find)
  "Front-end to igrep.
Try to guess what directories to search according to buffer content.

If you give empty prompt (do not select any database completion),
then you can specify all arguments.

The grep is never case sensitive.

Input:

 PRG PATTERN FILES DO-IT USE-FIND"
  (interactive (tinyigrep-main-read-args))

  (tinyigrep-debug "tinyigrep-main in:"  default-directory "-"
                   prg pattern files do-it use-find)

  (let ((fid                   "tinyigrep-main")
	(default-directory     default-directory))
    (unless fid ;;  XEmacs byte cimpiler silencer
      (setq fid nil))
    (when do-it
      ;;  CD to the first directory. If it exists.
      ;;  If it doesn't, call-process in igrep will tell it to user.
      (let* ((dir (file-name-directory (car files))))
        (when (and dir
                   (setq dir (ti::file-name-for-correct-system dir 'emacs))
                   (file-directory-p dir))
          (setq default-directory dir)))
      ;;  Strip away the path from first file
      ;;  (setcar files (file-name-nondirectory (car files)))
      (push (list
             default-directory
             prg
             pattern
             files
             use-find)
            tinyigrep--igrep-previous-args)
      ;; (save-some-buffers)
      (tinyigrep-igrep-call prg pattern files use-find)
      do-it)))

;;}}}

(tinyigrep-cygwin-fixes)

(provide   'tinyigrep)
(run-hooks 'tinyigrep--load-hook)

;;; tinyigrep.el ends here
