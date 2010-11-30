;;; epackage.el --- Emacs Lisp package manager (download, build, install)

;; This file is not part of Emacs

;; Copyright (C)    2009-2011 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto

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

;; Requirements:

;;      o   Emacs 22.1+ (released 2007). Designed only for Emacs.
;;          XEmacs has its own packaging system (pui-*).
;;          http://www.gnu.org/software/emacs
;;      o   git(1) Distributed Version Control System (DVCS). Any version.
;;          http://en.wikipedia.org/wiki/Git_(software)
;;      o   Depends only on standard Emacs. Does not use cl.

;;; Install:

;;
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      ;;  M-x epackage to start installing, upgrading. removing
;;      (autoload 'epackage "tinyepkg" "" t)
;;
;;	;; One big file to boot all packages (auto generated)
;;	(load "~/.emacs.d/epackage/00link/epackage-loader" 'noerr)

;;; Commentary:

;;  Preface 2009
;;
;;      Emacs has been around for decades now. Many new version have
;;      come and gone (18.59 ... 24.x), Still there wealth of package
;;      (*.el) that enhance and add new features to Emacs. E.g.
;;      editing new programming langauges. The typical procedure to
;;      add new a feature to Emacs is:
;;
;;      o   Find a package at places like
;;          http://dir.gmane.org/gmane.emacs.sources or
;;          http://www.emacswiki.org
;;      o   Download and save the package along `load-path'
;;      o   Read the installation information. Usually embedded in comments
;;          at the beginning of *.el files.
;;      o   Add code to the Emacs startup file ~/.emacs
;;          to arrange loading the package with personal customizations.
;;
;;      That's quite a bit of work for each package; reaching
;;      thousands out there. Many Linux distributions offer package
;;      managers to download and install programs. Debian has
;;      *apt-get*, Redhat uses *rpm*, Suse uses *yum* etc. So why not
;;      make one for Emacs as well.
;;
;;      This utility is built around two concepts: 1) it borrows the
;;      Debian style of package management and applies it to Emacs
;;      packages Each new utility mush include epackge/ subdirectory
;;      than contains details how to enable the and compile the
;;      package. 2) Packages are efficiently distributed, maintained
;;      and downloaded (deltas) by using mover Distributed Version
;;      Control Software; the git(1).
;;
;;      If you're an Emacs user, you just downloaded packages that are
;;      converted in epackage format. No information about the details
;;      is necessary. If you're a developer who would like to start
;;      make your packages available in epackage format, that requires
;;      some preparations.
;;
;;      This package manager can co-exist with your standard
;;      installation as usual. You can even use ELPA at the same time.
;;      User's standard Emacs startup files, like ~/.emacs are never
;;      modified.
;;
;;  Epackage - the DVCS packaging system
;;
;;      This packaging system is called epackage, short name for
;;      "Emacs Lisp packages".
;;
;;      In this system uses the packages are available in a form of
;;      distributed[1] git[2] version control repositories. The
;;      traditional packaging methods (like ELPA[2]) have relied on
;;      archives like *.tar.gz to hold all the code. In contrast the
;;      DVCS offers important features over *.tag.gz approach:
;;
;;      o   Efficient downloads; fast, only deltas are transferred
;;      o   Local modifications; users can creaet their own customizations
;;          easily
;;      o   Helping package authors made easy; have you fixed an
;;          error? Generate diff straight from the repository
;;      o   Select any version; pick latest or
;;          downgrade to a older version with ease.
;;      o   Contains history of package in one place. No more scattered
;;          pieces around Internet.
;;
;;      To use a package in this system, it must be first converted
;;      into a Git repository and made available online. This job can
;;      be made by anyone who sets up the reposository. It doesn't
;;      need to be done by the original developer who may not be
;;      familiar with the git(1) program. For more inforamtion about
;;      the packaging see "Epackage specification" below.
;;
;;      [1] DVCS = Distributed Version Control System
;;          http://en.wikipedia.org/wiki/Distributed_revision_control
;;
;;      [2] http://git-scm.org
;;
;;      [3] http://www.emacswiki.org/emacs/ELPA
;;
;;  User commands
;;
;;      Command `M-x' `epackage' is alias for function
;;      `epackage-manager'. It builds buffer where packages can be
;;      browsed, fetched, built and installed. The view contains:
;;
;;          [mode indicators]
;;
;;          name section status v:VERSION package-description
;;          1    2       3      4         5
;;
;;      Mode indicators are:
;;
;;      o   compile - byte compile package on install phase.
;;      o   activate|enable - Auto-enable or activate on install phase
;;          See "status" for more explanation.
;;
;;      The fields are:
;;
;;      o   1 - Unique package name. No two package scan have the same name.
;;      o   2 - Package classification. M-x finder-list-keywords
;;      o   3 - status: (A)activated (E)nabled (I)installed etc.
;;      o   4 - Version number. Only known once package has been downloaded.
;;      o   5 - Short package description
;;
;;      In this view, some of the commands are (see mode help `C-h' `m'):
;;
;;      o   d, run `dired' on package installation directory.
;;      o   e, edit package's *info* file.
;;      o   E, email upstream, the package author (maintainer). You can
;;             as for new wishlist features, report bugs etc.
;;      o   g, get. Update avaiĺable package list.
;;      o   i, install package.
;;      o   l, list only installed packages.
;;      o   m, mark package (for command install or remove).
;;      o   M, send mail to person who is the maintainer of epackage
;;             for this utility. You can send requests to fix
;;             packaging or update contents of the 'info' file if some
;;             of the information in no up to date.
;;      o   n, list only new packages (not-installed).
;;      o   p, purge package; delete package physically from local disk.
;;      o   r, remove package. Synonym for uninstall action.
;;      o   s<key>, sort command. Change listing by several criterias.
;;      o   u, unmark (install, purge, remove).
;;      o   U, upgrade package to newer version.
;;      o   v<key>, view comand. E.g (a)activation file, (i)info file.
;;      o   q, quit. Run `bury-buffer'.
;;      o   x, execute (install, purge, remove).
;;
;;      The package state is shows with following status indiators:
;;
;;      o   (A)ctivated. The package has been downloaded and code to
;;          immediately activate the package has been taken into use.
;;          This setting chnages user's Emacs environment as defined
;;          by the packager. The chnages typically include modifying hook
;;          to activate the package e.g. by file extension, adding
;;          keybindings to activate the package etc. You might want
;;          to use (v)iew command to see what exactly happens
;;      o   (E)enabled. One step down from Activated state. Only interactive
;;          functions and variables are provided in latent `autoload'
;;          stae for user to call with `M-x' <function name>. User
;;          configuration is not modified in any way.If you want full
;;          control over package setup, set package to Emabled state
;;          and add further code to Emacs startup file "/.emacs to
;;          configure it
;;      o   (I)installed. This is synonym for Downloaded. Package has
;;          been fetched to local disk, but that is all. No setup
;;          whatsoever.
;;      o   (u)unmaintained. The package has been flagged as unmaintained.
;;      o   (b)uggy. The package contains remarks that it might be buggy
;;          if installed.
;;      o   (c)ompiled. Package has been byte compiled.
;;      o   (e)macs core. Package in included in latest core Emacs.
;;      o   (x)emacs core. Package is included in latest core XEmacs.
;;
;;      Building the initial list of available packages takes some time
;;      and this is done via open internet connection. Install command
;;      also requires an open internet connection.
;;
;;  Epackage system layout
;;
;;      The packages are installed under root `epackage--root-directory',
;;      which defaults to ~/.emacs.d or ~/elisp respectively. The
;;      root directory is organized as follows:
;;
;;          epackage
;;          |
;;	    +-- 00link/
;;          |   epackage-loader.el      One big boot file
;;	    |	*.el *.elc	        Symlinks to ../vc/PACKAGE/*.el
;;	    |
;;          +-- install/
;;          |   <package>-activate.el files
;;          |   <package>-install.el files
;;          |
;;          +--vc/     Packages. The Version control repositories.
;;             |
;;             +-- 00epackage/          Yellow pages: list of available packages
;;	       +-- package/		Downloaded package
;;             +-- package2/
;;             +-- ...
;;
;;  Epackage specification
;;
;;      The Git repository branches used are:
;;
;;      o   `master', required. Branched off from `upstream'. Adds directory
;;          `epackage/'. This contains everything to use the package.
;;      o   `patches', optional. Patches to `upstream' code.
;;      o   `upstream', required. The original unmodified upstream code.
;;          Releases are tagged with label
;;          "upstream/YYYY-MM-DD[--VERSION]". The YYYY-MM-DD is the
;;          date of upstream release or best guess and it is
;;          accompanied with optional "--VERSION" of the package. Not
;;          all packages include version information. The ISO 8601
;;          date is needed so that the 1) release date is immediately
;;          available e.g. for post processing and 2) the tags sort
;;          nicely by date. An example: "upstream/2009-12-31--0.3"
;;
;;      The epackage method borrows concepts from the Debian package
;;      build system where a separate control directory contains
;;      the needed information. The directory name *epackage* is not
;;      configurable. Files in pacakge/ directory include:
;;
;;          <package name>
;;          |
;;          +- .git/                    Version control branches (see above)
;;          |
;;          +-- epackage/
;;              info                    required: The package control file
;;              PACKAGE-0loaddefs.el    optional: ###autoload statements
;;              PACKAGE-autoloads.el    optional: all autoload statements (raw)
;;              PACKAGE-compile.el      optional: Code to byte compile package
;;              PACKAGE-install.el      required: Code to make package available
;;              PACKAGE-uninstall.el    optional: to remove package
;;              PACKAGE-xactivate.el    optional: Code to activate package
;;
;;      The nanes of the files have been chosen to sort
;;      alphabetically. From Emacs point of view, loading individual
;;      files is slower than loading a gigantic setup. It would be
;;      possible (due to sort order) to safely collect all together
;;      with:
;;
;;              cat PACKAGE-* | grep -v 'uninst|compile' > PACKAGE-all-in-one-loader.el
;;
;;     The *-0loaddefs.el
;;
;;      This file contains extracted ##autoload definitions. The file
;;	is suatomatically generated. The file does not modify user's
;;	environment. If PACKAGE does not contains any ###autoload
;;	definitions, the manually crafted *-install.el file works as a
;;	substitute. The "Zero" at start of the name is due to proper
;;	sorting ordering of all files.
;;
;;     The *-install.el
;;
;;      This file is manually written and it publishes user variables
;;	and interactive `M-x' functions in an autoload state. This
;;	file does not modify user's environment. This file is
;;	necessary only if PACKAGE does not contain proper ###autoload
;;	statements (see *-0loaddefs.el). The "install" in name refers
;;	to installation or availability of interactive functions, not
;;	to any modifications to the system. Mnemonic: "if you load
;;	this file, you have can start using package's features" (see
;;	*-activate.el).
;;
;;     The *-uninstall.el
;;
;;      This file does the opposite of *-install.el and *-activate.el
;;      It runs commands to remove the package as if it has never been
;;      loaded. Due to the nature of Emacs, it may not be possible to
;;      completely uninstall the package. The uninstallation covers
;;      undoing the changes to *-hook, *-functions and
;;      `auto-mode-alist' variables. The actual symbols (defined
;;      functions and variables) are not removed. Usually it is more
;;      practical to just restart Emacs than completely trying undo
;;      all the effects of a package.
;;
;;     The *-xactivate.el
;;
;;      This file makes the PACKAGE immediately active in user's
;;      environment. It can modify current environment by adding
;;      functions to hooks, adding minor or major modes or arranging
;;      keybindings so that when pressed, the feature is loaded. It
;;      may also loop through `buffer-list' to activate features
;;      immediately in running Emacs. It is adviseable that any custom
;;      settings, like variables and prefix keys, are defined *before*
;;      this file is loaded. Mnemonic: "If you load this file, the
;;      bells and whistles are turned on".
;;
;;  The info file
;;
;;      A RFC 2822 formatted file (email), which contains information
;;      about the package. The minumum required fields are presented
;;      below. The header field names are case insensitive. Continued
;;      lines must be intended; suggested indentation is one space.
;;      Required fields aer marked with "*" character.
;;
;;          *Package:
;;          *Section: <data | extensions | files | languages | mail | tools | M-x finder-list-keywords>
;;          License: <GPL-[23]+ | BSD | Apache-2.0>
;;          *Depends: emacs (>= 20)
;;          Status: [ core-emacs | unmaintained | broken |
;;            note YYYY-MM-DD the code hasn't been touched since 2006 ; ]
;;          *Email:
;;          Bugs:
;;	    Maintainer:
;;          Vcs-Type:
;;          Vcs-Url:
;;          Vcs-Browser:
;;          Homepage:
;;          Wiki: http://www.emacswiki.org/emacs/
;;          *Description: <short one line>
;;           [<Longer description>]
;;           .
;;           [<Longer description, next paragraph>]
;;           .
;;           [<Longer description, next paragraph>]
;;
;;  Details of the info file fields in alphabetical order
;;
;;     Bugs
;;
;;	URL where the bugs should be reported. This can be email address
;;	or link to a issue tracker of upstream project. Note: send
;;	problems epackage problems to address mentioned in `Maintainer'.
;;
;;     Conflicts
;;
;;      This field lists packages that must be removed before install
;;      should be done. This field follow guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;     Depends (required)
;;
;;      List of dependencies: Emacs flavor and packages required. The
;;      version information is enclosed in parentheses with comparison
;;      operators ">=" and "<=". A between range is not defined. This
;;      field follow guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;      In case program works inly in certain Emacs versions, this
;;      information should be announces in field "Status::note" (which
;;      see). Packages that are not updated to work for latest Emacs
;;      versions are candidate for removal from package archive
;;      anyway. An example:
;;
;;              Depends: emacs (>= 22.2.2) | xemacs (>= 20)
;;
;;     Description (required)
;;
;;      The first line of this field is a consise description that fits on
;;      maximum line length of 80 characters in order to display in
;;      combined format "PACKAGE -- SHORT DESCRIPTION". The longer
;;      description is explained in paragraphs that are separated from
;;      each orher with a single (.) at its own line. The paragraphs
;;      are recommended to be intended by one space.
;;
;;     Email
;;
;;      Upstream developers email address(es). Multiple developers
;;      are listed like in email: separated by commas. Teh role can
;;      be expressed in parenthesis. An example:
;;
;;              Email: John doe (Author) <jdoe@example.com>,
;;               Joe Average (Co-developer) <jave@example.com>
;;
;;     Homepage
;;
;;      URL to the project homepage. For this field it is adviseable
;;      to use project addresses that don't move; those of
;;      Freshmeat.net, Sourceforge, Launchpad, Github etc. The
;;      Freshmeat is especially good because is provides an easy
;;      on-to-tover-all hub to all other Open Source projects. Through
;;      Freshmeat users can quickly browse related software and
;;      subscribe to project announcements. Freshmeat is also easy for
;;      the upstream developer to set up because it requires no heavy
;;      project management (it's kind of "yellow pages"). In any case,
;;      the Homepage link should not directly point to a volatile
;;      personal homepage if an alternative exists.
;;
;;     License
;;
;;      If misssing, the value is automatically assumed "GPL-2+". The
;;      valid License abbreviations should follow list defined at
;;      <http://wiki.debian.org/CopyrightFormat>.
;;
;;     Maintainer
;;
;;      The packaged who maintains the utility in epackage format. If
;;      this field is missing `Email' is assumed. Best if upstream (Email)
;;	is also the Maintainer of epackage.
;;
;;     Package (required)
;;
;;      This field is the PACKAGE part from file name package.el or the
;;      canonical known name in case of bigger packages like "gnus".
;;      An example "html-helper-mode.el" => package name is
;;      "html-helper-mode". It is adviseable to always add *-mode even
;;      if file does not explicitly say so. An example "python.el" =>
;;      package name is "python-mode". Duplicate similar names cannot
;;      exists. Please contact package author in case of name clashes.
;;
;;     Recommends
;;
;;      This field lists additional packages that the current package
;;      can utilize. E.g a package A, can take advantage of package B,
;;      if it is aailable, but it is not a requirement to install B
;;      for package A to work. This field is *not* used to annouce
;;      related packages. That information can be mentioned in
;;      the end of "Description" in paragraph "SEE ALSO".
;;
;;     Section (required)
;;
;;      This field contains category for package. The valid keywords are
;;      those listed in `M-x' `finder-list-keywords'.
;;
;;     Status
;;
;;      This field lists information about the package. Each keyword
;;      has a unique mening. the allowed list:
;;
;;          keyword := 'core-emacs'
;;                     | 'core-xemacs'
;;                     | 'unmaintained'
;;                     | 'broken'
;;                     | 'note' YYYY-MM-DD [COMMENT] ';'
;;
;;      And example:
;;
;;          Status: unmaintained
;;              broken
;;              note YYYY-MM-DD Doesn't work in Emacs 23.
;;              See thread http://example.com ;
;;
;;      The `core-*' values mark the package being included (or will
;;      be) in the latest [X]Emacs. Value `unmaintained' means that
;;      the original developer has vanished or abandoned the project
;;      and is no longer available for developing the package. Value
;;      `broken' means that package is broken and does not work in
;;      some Emacs version (usually latest). The `note' keyword can be
;;      used for any kind of information. It is adviced that notes are
;;      time stamped using ISO 8601 YYYY-MM-DD format. A note ends in
;;      character `;' and can be of any length.
;;
;;     Vcs-Browser
;;
;;      The URL address to the version control browser of the repository.
;;
;;     Vcs-Type
;;
;;      Version Constrol System information. The value is the
;;      lowercase name of the version control program. A special value
;;      "http" can be used to signify direct HTTP download. An example
;;      of an Emacs package hosted directly at a web page:
;;
;;          Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/vline.el
;;
;;     Vcs-Url
;;
;;      The technical repository URL. For CVS, this is the value of
;;      CVSROOT which includes also the protocol name:
;;
;;          Vcs-Url: :pserver:anonymous@example.com/reository/foo
;;
;;     Vcs-User
;;
;;      The login name. In case the repository cannot be accessed
;;      simply by visiting the `Vcs-Url' (or in the case of CVS:
;;      pressing RETURN at login prompt), this is the login name.
;;
;;     Wiki
;;
;;      This field points to package at <http://www.emacswiki.org>. If
;;      it does not exists, consider creating one for the PACKAGE.
;;
;;     X-*
;;
;;      Any other custom fields can be inserted using `X-*' field
;;      notation:
;;
;;          X-Comment: <comment here>
;;          X-Maintainer-Homepage: <URL>
;;
;; TODO
;:
;;      - Move package list into Git repository
;;      - GUI: drop outline. If user selects DETAIL view, collect
;;        information to another buffer dynamically (info, git tags,
;;        current git branch)
;;
;;      - New file: cache. Build it dynamically from packages and
;;        combine with package information (e.g. version).
;;
;;      - Use 00link directory where to draw symlinks. This is faster
;;        than using many paths in `load-path' but it won't work in
;;        Windows.
;;        => make it configurable
;;      - Dynamically search all *.el and *elc. When byte compiled,
;;        symlink those files as well.
;;
;;      - Install action by default runs eable (*-autoloads),
;;        unless user has activated auto-activate feature (toggle)
;;      - Another toggle is auto-byte-compile feature on package install.
;;
;;      - refetch repository (destroy, re-download).
;;
;;      - Git tags, where is this information kept?
;;      - How to update package, or all packages?
;;        => Running git preocess? When update is avilable how to flag this?
;;        => What about conflits?
;;      - What about 'local', manual branch and updates?
;;      - Retrieve new yellow pages (available packages)
;;      - Rescan current information? (what is installed, what is not)
;;        => Keep cache? Or regenerate, or scan at startup every time?
;;      - What if user manually deletes directories? Refresh?
;;      - Package health check, Lint?
;;      - Edit yellow pages catalog?
;;        => Submit/update yellow pages catalog changes?
;;        => version controlled, patches? Interface to automatic email?
;;      - Yellow pages URL health check? What to do with broken links?

;;; Change Log:

;;; Code:

(defconst epackage-version-time "2010.1130.1257"
  "*Version of last edit.")

(defcustom epackage--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'Epackage)

(defcustom epackage--byte-compile-loader-file nil
  "*Non-nil measn to byte compile `epackage--loader-file'."
  :type  'boolean
  :group 'Epackage)

(defcustom epackage--sources-list-url
  "jaalto@cante.net:srv/git/emacs-lisp-dev--epackage-sources-list"
  "URL to the location of available package list. The yellow pages.
This is the Git repository that contains the canonical list of
available packages.

The included text file contains information about package names
and their repository download URLs. Empty lines and comment on
their own lines started with character '#' are ignored:

  # Comment
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  PACKAGE-NAME REPOSITORY-URL DESCRIPTION
  ...

An example:

  foo git://example.com/repository/foo.git")

(defconst epackage--sources-package-name "00epackage"
  "The name of local repository of `epackage--sources-list-url'.")

(defcustom epackage--root-directory
  (let (ret)
    (dolist (elt (list
                  (if (featurep 'xemacs)
                      "~/.xemacs.d"
                    "~/.emacs.d")
                  "~/elisp"))
      (if (and elt
               (null ret)
               (file-directory-p elt))
          (setq ret elt)))
    (cond
     (ret
      ret)
     (t
      ;; No known package installation root directory
      (message
       (concat "Epackage: [ERROR] Can't determine location of lisp packages."
               "Please define `epackage--root-directory'.")))))
  "*Location of lisp files. Typically ~/.emacs.d or ~/elisp.
Directory should not contain a trailing slash."
  :type  'directory
  :group 'Epackage)

(defvar epackage--directory-name "epackage"
  "Name of package directory under `epackage--root-directory'.
Use function `epackage-directory' for full path name.")

(defvar epackage--directory-name-vcs "vc"
  "VCS directory under `epackage--root-directory'.
Use function `epackage-file-name-vcs-compose' for full path name.")

(defvar epackage--directory-name-install "install"
  "Install directory under `epackage--root-directory'.")

(defvar epackage--directory-name-link "00link"
  "Link directory under `epackage--root-directory'.
This directory contains symlinks to all installed packages and
their *.el and *.elc files.")

(defconst epackage--directory-exclude-regexp
  (concat
   "/\\.\\.?$"
   "\\|/RCS$"
   "\\|/rcs$"
   "\\|/CVS$"
   "\\|/cvs$"
   "\\|/\\.\\(svn\\|git\\|bzr\\|hg\\|mtn\\|darcs\\)$"
   "\\|/"
   epackage--directory-name
   "$")
  "Regexp to exclude dirctory names.")

(defconst epackage--layout-mapping
  '((activate . "xactivate")
    (autoload . "autoloads")
    (enable . "install")
    (info . "info")
    (loaddefs . "loaddefs")
    (uninstall . "uninstall"))
  "File name mappings under epackage/ directory.
Format is:
  '((TYPE . FILENAME) ...)

Used in `epackage-file-name-vcs-directory-control-file'.")

(defvar epackage--initialize-flag nil
  "Set to t, when epackage has been started. do not touch.")

(defvar epackage--program-git nil
  "Location of program git(1).")

(defvar epackage--sources-file-name "epackage.lst"
  "Name of yellow pages file that lists available packages.
See variable `epackage--sources-list-url'.")

(defvar epackage--loader-file "epackage-loader.el"
  "file that contains all package enable and activate code.
See `epackage-loader-file-generate'.")

(defvar epackage--package-control-directory "epackage"
  "Name of directory inside VCS controlled package.")

(defvar epackage--process-output "*Epackage process*"
  "Output of `epackage--program-git'.")

(defsubst epackage-file-name-compose (name)
  "Return path to NAME in epackage directory."
  (format "%s/%s"
          (epackage-directory)
          name))

(defsubst epackage-file-name-loader-file ()
  "Return path to boot loader file."
  (format "%s/%s"
          (epackage-directory)
          epackage--loader-file))

(defsubst epackage-file-name-vcs-compose (package)
  "Return VCS directory for PACKAGE."
  (format "%s/%s%s"
          (epackage-directory)
          epackage--directory-name-vcs
          (if (string= "" package)
              ""
            (concat "/" package))))

(defsubst epackage-file-name-vcs-directory ()
  "Return VCS directory"
  (epackage-file-name-vcs-compose ""))

(defsubst epackage-file-name-install-directory ()
  "Return VCS directory"
  (format "%s/%s"
          (epackage-directory)
          epackage--directory-name-install))

(defsubst epackage-file-name-link-directory ()
  "Return link directory"
  (format "%s/%s"
          (epackage-directory)
          epackage--directory-name-link))

(defsubst epackage-file-name-vcs-package-control-directory (package)
  "Return control directory of PACKAGE"
  (let ((root (epackage-file-name-vcs-compose package)))
    (format "%s/%s" root epackage--package-control-directory)))

(defun epackage-file-name-vcs-directory-control-file (package type)
  "Return PACKAGE's control file of TYPE.

TYPE can be on of the following:

  'activate
  'autoload
  'enable
  'info
  'loaddefs
  'uninstall

Refer top Epackage specification for more information in
documentation of tinyepkg.el."
  (let ((dir (epackage-file-name-vcs-package-control-directory package))
        (file (cdr-safe (assq type epackage--layout-mapping))))
    (if (not file)
        (error "Epackage: [ERROR] Unknown TYPE argument '%s'" type)
      (cond
       ((eq type 'info)
         (format "%s/%s" dir file))
       (t
        (format "%s/%s-%s.el" dir package file))))))

(defsubst epackage-file-name-activated-compose (package)
  "Return path to PACKAGE under activated directory."
  (format "%s/%s%s"
          (epackage-directory)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-xactivate.el" package))))

(defsubst epackage-file-name-enabled-compose (package)
  "Return path to PACKAGE under install directory."
  (format "%s/%s%s"
          (epackage-directory)
          epackage--directory-name-install
          (if (string= "" package)
              ""
            (format "/%s-install.el" package))))

(defun epackage-package-enabled-p (package)
  "Return file if PACKAGE is enabled."
  (let ((file (epackage-file-name-enabled-compose package)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-activated-p (package)
  "Return file if PACKAGE is activated."
  (let ((file (epackage-file-name-activated-compose package)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-downloaded-p (package)
  "Check if package has been downloaded."
  (unless (stringp package)
    (error "Epackage: [ERROR arg 'package' is not a string."))
  (let ((dir (epackage-file-name-vcs-compose package)))
    (file-directory-p dir)))

(defun epackage-package-activated-p (package)
  "Check if package has been activated, return activate file."
  (unless (stringp package)
    (error "Epackage: [ERROR] arg 'package' is not a string."))
  (let ((file (epackage-file-name-activated-compose package)))
    (if (file-exists-p file)
        file)))

(defun epackage-package-enabled-p (package)
  "Check if package has been enabled, return enabled file."
  (unless (stringp package)
    (error "Epackage: [ERROR] arg 'package' is not a string."))
  (let ((file (epackage-file-name-install-compose package)))
    (if (file-exists-p file)
        file)))

(defsubst epackage-sources-list-directory ()
  "Return sources list, the yellow pages, directory."
  (epackage-file-name-vcs-compose epackage--sources-package-name))

(defsubst epackage-file-name-sources-list ()
  "Return path to `epackage--sources-file-name'."
  (format "%s/%s"
          (epackage-sources-list-directory)
          epackage--sources-file-name))

(defsubst epackage-sources-list-p ()
  "Check existence of `epackage--sources-file-name'."
  (file-exists-p (epackage-file-name-sources-list)))

(defsubst epackage-sources-list-verify ()
  "Signal error if `epackage--sources-file-name' does not exist."
  (unless (epackage-sources-list-p)
    (error "Epackage: Missing file %s. Run epackage-initialize"
           (epackage-file-name-sources-list))))

(defun epackage-program-git-verify ()
  "Verify variable `epackage--program-git'."
  (when (or (not (stringp epackage--program-git))
            (not (file-exists-p epackage--program-git)))
    (error
     (substitute-command-keys
      (format
       `,(concat
	  "Epackage: [ERROR] Invalid value in `epackage--program-git' (%s) "
	  "Run \\[epackage-initialize]")
	  epackage--program-git)))))

(defun epackage-directory ()
  "Return root directory."
  (format "%s%s"
          (expand-file-name
           (file-name-as-directory epackage--root-directory))
          (if (stringp epackage--directory-name)
              epackage--directory-name
            (error
             "Epackage: [ERROR] epackage--directory-name is not a string"))))

(put  'epackage-with-binary 'lisp-indent-function 0)
(put  'epackage-with-binary 'edebug-form-spec '(body))
(defmacro epackage-with-binary (&rest body)
  "Disable all interfering `write-file' effects and run BODY."
  `(let ((version-control 'never)
         (backup-inhibited t)
         (buffer-file-coding-system 'no-conversion)
         write-file-functions
         after-save-hook)
     ,@body))

(put  'epackage-with-sources-list 'lisp-indent-function 0)
(put  'epackage-with-sources-list 'edebug-form-spec '(body))
(defmacro epackage-with-sources-list (&rest body)
  "Run BODY in package list buffer."
  `(progn
     (epackage-sources-list-verify)
     (with-current-buffer
         (find-file-noselect (epackage-file-name-sources-list))
       ,@body)))

(defun epackage-git-command-process (&rest args)
  "Run git COMMAND with output to `epackage--process-output'."
  (epackage-program-git-verify)
  (with-current-buffer (get-buffer-create epackage--process-output)
    (unless (stringp epackage--program-git)
      (error "Epackage: [ERROR] Not a string: epackage--program-git"))
    (goto-char (point-max))
    (apply 'call-process
           epackage--program-git
           (not 'infile)
           (current-buffer)
           (not 'display)
           args)))

(defsubst epackage-git-error-handler (&optional command)
  "On Git error, show proces buffer and signal error."
  (display-buffer epackage--process-output)
  (error "Epackage: [ERROR] Git %scommand error"
         (if command
             (format "'%s' " command)
           "")))

(defsubst epackage-git-command-ok-p (status)
  "Return non-nil if command STATUS was ok."
  (zerop status))

(defun epackage-git-command-pull (dir &optional verbose)
  "Run git pull in DIR.
If VERBOSE is non-nil, display progress message."
  (let ((default-directory dir))
    (if verbose
        (message "Epackage: Running 'git pull' in %s ..." dir))
    (prog1
        (unless (epackage-git-command-ok-p
                 (epackage-git-command-process
                  "pull"))
          (epackage-git-error-handler "clone")))
    (if verbose
        (message "Epackage: Running 'git pull' in %s ...done" dir))))

(defun epackage-upgrade-package (package &optional verbose)
  "Upgrade PACKAGE in VCS directory.
If VERBOSE is non-nil, display progress message."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (error "Epackage: [ERROR] No Git URL for package '%s'" package))
    (let ((dir (epackage-file-name-vcs-compose package)))
      (epackage-git-command-pull dir))))

(defun epackage-upgrade-sources-list ()
  "Update list of available packages."
  (let ((dir (epackage-sources-list-directory)))
    (unless (file-directory-p dir)
      (error
       (substitute-command-keys
        (format
         (concat "Epackage: No such directory '%s'. "
                 "Run \\[epackage-initialize]")
         dir))))
    (epackage-git-command-pull dir)))

(defun epackage-git-command-clone (url dir &optional verbose)
  "Run git clone for PACKAGE in DIR.
If VERBOSE is non-nil, display progress message."
  (let ((default-directory (epackage-file-name-vcs-directory)))
    (if (file-directory-p dir)
        (error "Epackage: [ERROR] directory already exists: %s" dir))
    (if verbose
        (message "Epackage: Running git clone %s %s ..." url git))
    (prog1
        (unless (epackage-git-command-ok-p
                 (epackage-git-command-process
                  "clone"
                  url
                  dir))
          (epackage-git-error-handler "clone")))
    (if verbose
        (message "Epackage: Running git clone %s %s ...done" url git))))

(defun epackage-download-package (package &optional verbose)
  "Download PACKAGE to VCS directory.
If VERBOSE is non-nil, display progress message."
  (let ((url (epackage-sources-list-info-url package)))
    (unless url
      (error "Epackage: [ERROR] No Git URL for package '%s'" package))
    (let ((dir (epackage-file-name-vcs-compose package)))
      (epackage-git-command-clone url dir))))

(defun epackage-enable-package (package)
  "Enable PACKAGE."
  (let ((from (epackage-file-name-vcs-directory-control-file
               package 'enable))
        (to (epackage-file-name-install-compose package)))
    (unless (file-exists-p from)
      (error "Epackage: [ERROR] File does not exists: %s" from))
    (copy-file from to 'overwrite 'keep-time)))

(defun epackage-activate-package (package)
  "Activate PACKAGE."
  (let ((from (epackage-file-name-vcs-directory-control-file
               package 'activate))
        (to (epackage-file-name-activated-compose package)))
    (unless (file-exists-p from)
      (error "Epackage: [ERROR] file does not exists: %s" from))
    (copy-file from to 'overwrite 'keep-time)))

(defun epackage-enable-package (package)
  "Activate PACKAGE."
  (let ((from (epackage-file-name-vcs-directory-control-file
               package 'enable))
        (to (epackage-file-name-enabled-compose package)))
    (unless (file-exists-p from)
      (error "Epackage: [ERROR] file does not exists: %s" from))
    (copy-file from to 'overwrite 'keep-time)))

(defun epackage-disable-package (package)
  "Disable PACKAGE."
  (dolist (file (directory-files
                 (epackage-file-name-install-directory)
                 'full-path
                 (format "^%s-.*\\.el" package)
                 t))
    (if (file-exists-p file)
        (delete-file file))))

(defun epackage-action-package (package action)
  "Perform ACTION on PACKAGE.
ACTION can be:

  'enable
  'disable
  'activate
  'uninstall"
  ;; FIXME: Not implemented
  )

(defun epackage-directory-list (dir)
  "Return all directories under DIR."
  (let (list)
    (dolist (elt (directory-files dir 'full))
      (when (and (file-directory-p elt)
                 (not (string-match
                       epackage--directory-exclude-regexp
                       elt)))
        (setq list (cons elt list))))
    list))

(defun epackage-directory-recursive-list (dir list)
  "Return all directories under DIR recursively to LIST.
Exclude directories than contain file .nosearch
or whose name match `epackage--directory-name'."
  (let ((dirs (epackage-directory-list dir)))
    (setq list (cons dir list))
    (dolist (elt dirs)
      (cond
       ((file-exists-p (concat elt "/.nosearch")))
       (t
        (setq list (cons elt list))
        (epackage-directory-recursive-list elt list))))
    list))

(defun epackage-loader-file-insert-header ()
  "Insert header comments."
  (insert
   (format
    "\
;; Epackge boot file -- automatically generated
;;
;; Do not modify. Changes done here will be lost.
;; Add following to your ~/.emacs to use this file:
;;   (load-file \"%s\")

"
    (file-name-sans-extension
     (epackage-file-name-loader-file)))))

(defsubst epackage-loader-file-insert-footer ()
  "Insert Footer."
  (insert
   (format "\
\(provide '%s)

;; End of file
"
           (file-name-sans-extension
            (file-name-nondirectory
             (epackage-file-name-loader-file))))))

(defun epackage-loader-insert-file-path-list-by-path (path)
  "Insert `load-path' definitions to `current-buffer' from PATH."
  (let (list)
    (dolist (dir (epackage-directory-recursive-list path list))
      (insert (format
               "(add-to-list 'load-path \"%s\")\n"
               dir)))))

(defun epackage-loader-file-insert-path-list ()
  "Insert `load-path' commands to `current-buffer'."
  (let (name
        package
        list)
    (dolist (file (directory-files
                   (epackage-file-name-install-directory)
                   'full-path
                   "^.*-.*\\.el"
                   t))
      (setq name
            (file-name-sans-extension
             (file-name-nondirectory file)))
      ;; package-name-autoloads => package-name
      (setq package (replace-regexp-in-string  "-[^-]+$" "" name))
      (unless (member package list)
        (add-to-list 'list package)
        (epackage-loader-insert-file-path-list-by-path
          (epackage-file-name-vcs-compose package))))))

(defun epackage-loader-file-insert-install-code ()
  "Insert package installation code into `current-buffer'."
  ;; FIXME: Should only insert activate, not enable code if both exist
  (dolist (file (directory-files
                 (epackage-file-name-install-directory)
                 'full-path
                 "^.*-.*\\.el"
                 t))
    (goto-char (point-max))
    (insert-file-contents-literally file)))

(defsubst epackage-loader-file-insert-main ()
  "Insert Epackage loader boot commands to current point."
  (epackage-loader-file-insert-header)
  (epackage-loader-file-insert-path-list)
  (epackage-loader-file-insert-install-code)
  (epackage-loader-file-insert-footer))

(defun epackage-loader-file-byte-compile ()
  "Byte compile `epackage-file-name-loader-file'."
  (interactive)
  (let ((file (epackage-file-name-loader-file)))
    (if (file-exists-p file)
        (byte-compile-file file))))

(defsubst epackage-loader-file-byte-compile-maybe ()
  "Check `epackage--byte-compile-loader-file' and byte compile."
  (when epackage--byte-compile-loader-file
    (epackage-loader-file-byte-compile)))

(defun epackage-loader-file-generate ()
  "Generate main loader for all installed or activated packages."
  (interactive)
  (let ((file (epackage-file-name-loader-file)))
    (with-current-buffer (find-file-noselect file)
      (delete-region (point-min) (point-max))
      (epackage-loader-file-insert-main)
      (write-region (point-min)
                    (point-max)
                    (epackage-file-name-loader-file))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (epackage-loader-file-byte-compile-maybe))))

(defun epackage-sources-list-info-main (package)
  "Return '(pkg url description) for PACKAGE.
Format is described in variable `epackage--sources-list-url'."
  (epackage-with-sources-list
   (goto-char (point-min))
   (let ((re
          (format
           `,(concat "^\\(%s\\)\\>"
                     "[ \t]+\\([^ \t\r\n]+\\)"
                     "[ \t]*\\([^ \t\r\n]*\\)")
           (regexp-quote package))))
     (when (re-search-forward re nil t)
       (list
        (match-string-no-properties 1)
        (match-string-no-properties 2)
        (match-string-no-properties 3))))))

(defun epackage-sources-list-info-url (package)
  "Return URL for PACKAGE."
  (let ((info (epackage-sources-list-info-main package)))
    (when info
      (nth 1 info))))

(defun epackage-require-emacs ()
  "Require Emacs features."
  (unless (fboundp 'url-retrieve-synchronously)
    (error (concat
            "Epackage: [ERROR] this Emacs does not define "
            "`url-retrieve-synchronously' from url.el"))))

(defun epackage-require-git ()
  "Require Git program."
  (cond
   ((null epackage--program-git)
    (let ((bin (executable-find "git")))
      (unless bin
        (error "Epackage: [ERROR] program 'git' not found in PATH"))
      (setq epackage--program-git bin)))
   ((and (stringp epackage--program-git)
         (not (file-exists-p epackage--program-git)))
    (error "Epackage: [ERROR] Invalid `epackage--program-git' (%s)"
           epackage--program-git))
   ((file-executable-p epackage--program-git)) ;All ok
   (t
    (error "Epackage: [ERROR] Unknown value in `epackage--program-git' (%s)"
           epackage--program-git))))

(defun epackage-require-directories ()
  "Buid directory structure."
  (dolist (dir (list
                (epackage-directory)
                (epackage-file-name-vcs-directory)
                (epackage-file-name-install-directory)
                (epackage-file-name-link-directory)))
    (unless (file-directory-p dir)
      (message "Epackage: Making directory %s ..." dir)
      (make-directory dir))))

(defun epackage-require-main ()
  "Check requirements to run Epackage."
  (epackage-require-emacs)
  (epackage-require-git)
  (epackage-require-directories))

(defun epackage-url-http-parse-respons-error (&optional url)
  "On HTTP GET error, show reponse and signal error for optional URL."
  (let ((status (url-http-parse-response)))
    (when (or (< status 200)
              (>= status 300))
      (display-buffer (current-buffer))
      (error "[ERROR] HTTP access problem %d%s"
             status
             (if url
                 (concat " " url)
               "")))))

(defun epackage-url-retrieve-main (url file)
  "Download URL and save to a FILE."
  (let ((buffer (url-retrieve-synchronously url)))
    (unless buffer
      (error "Epackage: [ERROR] can't access url: %s" url))
    (with-current-buffer buffer
      (epackage-url-http-parse-respons-error url)
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (epackage-with-binary
        (write-region (point) (point-max) file)
        (kill-buffer (current-buffer))))))

(defun epackage-download-sources-list (&optional message)
  "Download sources list file, the yellow pages."
  (if message
      (message message))
  (if (epackage-sources-list-p)
      (error "Epackage: [ERROR] Directory already exists: %s"
             (epackage-sources-list-directory)))
  (epackage-git-command-clone epackage--sources-list-url dir))

(defun epackage-cmd-download-sources-list ()
  "Download or upgrade package list; the yellow pages of package repositories."
  (interactive)
  (if (epackage-sources-list-p)
      (epackage-upgrade-sources-list "Upgrading available package list")
    (epackage-download-sources-list "Downloading available package list")))

(defun epackage-cmd-download-package (PACKAGE)
  "Download PACKAGE, but do not install it."
  (if (not (epackage-sources-list-p))
      (message
       (substitute-command-keys
        "Epackage: No package list. Run \\[epackage-cmd-download-sources-list]"))
    (let ()
      ;; FIXME: Present list of package as completing-read
      )))

(defun epackage-initialize ()
  "Inialize package."
  (unless epackage--initialize-flag
    (epackage-require-main))
  (unless (epackage-sources-list-p)
    (epackage-cmd-download-sources-list))
  (setq epackage--initialize-flag t))

;;;###autoload (autoload 'epackage-mode          "epackage" "" t)
;;;###autoload (autoload 'turn-on-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'tun-off-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'epackage-commentary    "epackage" "" t)

;; FIXME: Unfinished, this is at a sketch / planning phase.

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "epackage-" " Epkg" "z" "Epkg" 'Epackage "epackage--"
   "Emacs package manager

Mode description:

\\{epackage--mode-prefix-map}"

   "Epackage"
   nil
   "Number conversion mode"
   (list                                ;arg 10
    epackage--mode-easymenu-name
    "----"
    ["Package version"    epackage-version        t]
    ["Package commentary" epackage-commentary     t]
    ["Mode help"   epackage-mode-help   t]
    ["Mode off"    epackage-mode        t])
   (progn
     (define-key map "v"  'epackage-version)
     (define-key map "?"  'epackage-mode-help)
     (define-key map "Hm" 'epackage-mode-help)
     (define-key map "Hc" 'epackage-commentary)
     (define-key map "Hv" 'epackage-version))))

(defsubst epackage-directory-name ()
  "Return package directory.
Refences:
  `epackage--package-root-directory'
  `epackage--directory-name'."
  (if (and (stringp epackage--root-directory)
           (stringp epackage--directory-name))
      (format "%s/%s"
              epackage--root-directory
              epackage--directory-name)
    (error (concat "Epackge: [FATAL] Invalid epackage--root-directory"
                    " or epackage--directory-name"))))

(add-hook  'epackage--mode-hook 'epackage-mode-define-keys)
(provide   'epackage)
(run-hooks 'epackage--load-hook)

;;; epackage.el ends here
