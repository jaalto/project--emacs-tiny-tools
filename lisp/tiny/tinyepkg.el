;;; epackage.el --- Emacs Lisp package manager (download, build, install)

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    2009 Jari Aalto
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

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      (require 'tinyepkg)
;;
;;  You can also use the preferred way: autoload
;;
;;       (autoload 'epackage "tinyepkg" "" t)
;;       (global-set-key "\C-cP" 'epackage)

;; ..................................................... &t-commentary ...
;;; Commentary:
;;
;;  Preface 2009
;;
;;      Emacs has been around for decades now. Many new version have
;;      come and gone (18.59, 19.x, 20.x, 21.x, 22.x, 23.x), There are
;;      many packages (*.el) that enhance and add new feature e.g. for
;;      new programming langauges. The typical procedure to add new
;;      feature to Emacs is:
;;
;;      o   Find a package at places like
;;          http://dir.gmane.org/gmane.emacs.sources or
;;          http://www.emacswiki.org
;;      o   Download and save the package along `load-path'
;;      o   Read the installation information. Usually embedded in comments
;;          inside a file.
;;      o   Add Emacs Lisp code to the startup file ~/.emacs
;;          to arrange loading the package with personal customizations.
;;
;;      That's quite a bit of work for each package; reaching 1000's
;;      out there. Many Linux distributions offer package managers to
;;      download and install programs. Debian has *apt-get*, Redhat
;;      uses *rpm*, Suse uses *yum* etc. So why not make one for Emacs
;;      as well.
;;
;;  Epackage - the DVCS packaging system
;;
;;      This packaging system is called "epackage", short name for
;;      "Emacs Lisp packages".
;;
;;      This system uses packages that available in a form of
;;      distributed[1] git[2] version control repositories. The
;;      traditional packaging methods have relied on archives like
;;      *.tar.gz which hold all the code. While it would be possible
;;      to develop packaging system using archives, the DVCS offers
;;      possiblities that could previously only be dreamt of: 1)
;;      efficient downloads; fast, only deltas are transferred 2)
;;      local modifications; users can creaet their own customizations
;;      easily 3) Helping package authors made easy; have you fixed an
;;      error? Generate diff straight from the repository 4) Select
;;      version; pick stable or unstable version of the package, or
;;      downgrade to a older version with ease.
;;
;;      Before existing Emacs Lisp code can be used, it must be first
;;      converted to a git repository and made available online. This
;;      job can be made by anyne who sets up the reposository. It
;;      doesn't need to be done by the original developer who may not
;;      be familiar with the git(1) program or version control in
;;      general. For more inforamtion about the packaging see
;;      'Epackage git repository layout' below.
;;
;;      [1] DVCS = Distributed Version Control System
;;          http://en.wikipedia.org/wiki/Distributed_revision_control
;;
;;      [2] http://git-scm.org
;;
;;  User commands
;;
;;      Command `M-x' `epackage' is alias for function
;;      `epackage-manager'. It builds outline based buffer where
;;      packages can be browsed, built and installed. Standard outline
;;      type of keys can be used to navigate in the buffer. The
;;      *Local* is "yes" when package has been downloaded to local
;;      disk. The *Status* indicates if the package activation code is
;;      found from `ROOT/enabled' directory (see below). User's
;;      standard Emacs startup files, like ~/.emacs are never
;;      modified.
;;
;;          * Section: tools
;;          ** Package: one; Local: no; Status: not-installed; Ver: 1.5 -!-
;;          <content of the 'info' file>
;;          * Section: tools
;;          ** Package: two; Local: yes; Status: installed; Ver: 1.0
;;          <content of the 'info' file>
;;          ...
;;
;;      In this view, supposing the cursor is at [-!-] or inside the
;;      package description, the commands are:
;;
;;      o   d, run `dired' on package installation directory
;;      o   e, edit package 'info'
;;      o   g, get updated view, needs internet connection.
;;      o   i, install package
;;      o   l, list only installed packages
;;      o   m, mark package (for command install or remove)
;;      o   n, list only new packages (not-installed)
;;      p   p, purge package; delete physically from local disk
;;      o   r, remove package
;;      o   q, quit; run `bury-buffer'
;;
;;      Building the initial list of available packages take some time
;;      and this is done via open internet connection. Any install
;;      command also require open internet connection.
;;
;;  Epackage system layout
;;
;;      The packages are installed under root `epackage--root-directory',
;;      which defaults to ~/.emacs.d or ~/elisp respectively. The
;;      root directory is organized as follows:
;;
;;          epackage
;;          | epackage-loader.el  ;; activate + installed as one big file.
;;          |
;;          +--activated/
;;          |  <package>-activate.el files
;;          |
;;          +--installed/
;;          |  <package>-install.el files
;;          |
;;          +--vc/     Packages. The Version control repositories.
;;             |
;;             +--<package name>/
;;             +--<package name2>/
;;             +-- ...
;;
;;  Epackage specification
;;
;;      The Git repository branches used are:
;;
;;      o   `master', required. Branched off from `upstream'. Adds directory
;;	    `epackage/'. This contains verything to use the package.
;;      o   `patches', optional. Patches to `upstream' code.
;;      o   `upstream', required. The original unmodified upstream code.
;;	    Releases are tagged with label
;;	    "upstream/YYYY-MM-DD[--VERSION]". The YYYY-MM-DD is the
;;	    date of upstream release or best guess and it is
;;	    accompanied with optional "--VERSION" of the package. Not
;;	    all packages include version information. The ISO 8601
;;	    date is needed so that the 1) release date is immediately
;;	    available e.g. for post processing and 2) the tags sort
;;	    nicely by date. An example: "upstream/2009-12-31--0.3"
;;
;;      The used method borrows concepts from the Debian package build
;;      system, where a separate control directory is reserved for
;;      packaging files. The directory name "epackage" is not
;;      configurable. Files in te epackge/ directory include:
;;
;;          <package name>
;;          |
;;          +- .giit/			Version control branches (see above)
;;          |
;;          +-- epackage/
;;		info			required: The package control file
;;		PACKAGE-autoloads.el	optional: all autoload statements (raw)
;;		PACKAGE-install.el	required: Code to make package available
;;		PACKAGE-loaddefs.el	required: ###autoload statements
;;		PACKAGE-uninstall.el	optional: to remove package
;;		PACKAGE-xactivate.el	optional: Code to activate package
;;
;;	The nanes of the files have been chosen to sort
;;	alphabetically. From Emacs point of view, loading individual
;;	files is slower than loading a gigantic setup. It would be
;;	possible (due to sort order) to safely collect all together
;;	with:
;;
;;		cat PACKAGE-* | grep -v uninstall > PACKAGE-all-in-one-loader.el
;;
;;     The *-install.el
;;
;;	This file does not modify user's environment. It publishes
;;	user variables and interactive `M-x' functions in autoload
;;	state for the package. This file is usually necessary only if
;;	PACKAGE does not contain proper ###autoload statements. See
;;	*-loaddefs alternative in that case.
;;
;;     The *-loaddefs.el
;;
;;	This file does not modify user's environment. It is
;;	automatically generated from the PACKAGE by collecting all
;;	###autoload definitions. If PACKAGE does not contains any
;;	###autoload definitions, then manually crafter *-install.el
;;	file works as a substitute for file.
;;
;;     The *-uninstall.el
;;
;;	This file does the opposite of *-install.el and *-activate.el
;;	Runs commands to remove the package as if it has never been
;;	loaded. Due to the nature of Emacs, it may not be possible to
;;	completely uninstall the package. The uninstallation usually
;;	covers undoing the changes to variables like *-hook,
;;	*-functions and `auto-mode-alist'. The actual symbols (defined
;;	functions and variables) are not removed. Usually it is more
;;	practical to just restart Emacs than completely trying undo
;;	all the effects of a package.
;;
;;     The *-xactivate.el
;;
;;	This file makes the PACKAGE immediately active in user's
;;	environment. It modifies current environment by adding
;;	functions to hooks, adding minor or major modes or arranging
;;	keybindings so that when pressed, the feature is loaded. It is
;;	adviseable that any custom settings, like variables and prefix
;;	keys, are defined *before* this file is loaded.
;;
;;  The info file
;;
;;      A RFC 2822 formatted file (email), which contains information
;;      about the package. The minumum required fields are presented
;;      below. The header field names are case insensitive. Continued
;;      lines must be intended; suggested indentation is one space.
;;      Required fields aer marked with "*" character.
;;
;;	    *Package:
;;          *Section: <data | extensions | files | languages | mail | tools | M-x finder-list-keywords>
;;          License: <GPL-[23]+ | BSD | Apache-2.0>
;;          *Depends: emacs (>= 20)
;;          Status: [ core-emacs | unmaintained | broken |
;;            note YYYY-MM-DD the code hasn't been touched since 2006 ; ]
;;          *Email:
;;          Bugs:
;;          Vcs-Type:
;;          Vcs-Url:
;;          Vcs-Browser:
;;          Homepage:
;;          Wiki: http://www.emacswiki.org/emacs/
;;          *Description: <short one line>
;;           [<Longer description>]
;;	     .
;;           [<Longer description, next paragraph>]
;;	     .
;;           [<Longer description, next paragraph>]
;;
;;  Details of the info file fields in alphabetical order
;;
;;     Conflicts
;;
;;	This field lists packages that must be removed before install
;;	should be done. This field follow guidelines of
;;	<http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;     Depends (required)
;;
;;	List of dependencies: Emacs flavor and packages required. The
;;	version information is enclosed in parentheses with comparison
;;	operators ">=" and "<=". A between range is not defined. This
;;	field follow guidelines of
;;	<http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;	In case program works inly in certain Emacs versions, this
;;	information should be announces in field "Status::note" (which
;;	see). Packages that are not updated to work for latest Emacs
;;	versions are candidate for removal from package archive
;;	anyway. An example:
;;
;;		Depends: emacs (>= 22.2.2) | xemacs (>= 20)
;;
;;     Description (required)
;;
;;	The first line of this field is a consise description that fits on
;;      maximum line length of 80 characters in order to display in
;;      combined format "PACKAGE -- SHORT DESCRIPTION". The longer
;;	description is explained in paragraphs that are separated from
;;	each orher with a single (.) at its own line. The paragraphs
;;	are recommended to be intended by one space.
;;
;;     Email
;;
;;	Upstream developers email address(es). Multiple developers
;;	are listed like in email: separated by commas. Teh role can
;;	be expressed in parenthesis. An example:
;;
;;		Email: John doe (Author) <jdoe@example.com>,
;;		 Joe Average (Co-developer) <jave@example.com>
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
;;     Package (required)
;;
;;	This field is the PACKAGE part from file name package.el or the
;;      canonical known name in case of bigger packages like "gnus".
;;      An example "html-helper-mode.el" => package name is
;;      "html-helper-mode". It is adviseable to always add *-mode even
;;	if file does not explicitly say so. An example "python.el" =>
;;	package name is "python-mode". Duplicate similar names cannot
;;      exists. Please contact package author in case of name clashes.
;;
;;     Recommends
;;
;;	This field lists additional packages that the current package
;;	can utilize. E.g a package A, can take advantage of package B,
;;	if it is aailable, but it is not a requirement to install B
;;	for package A to work. This field is *not* used to annouce
;;	related packages. That information can be mentioned in
;;	the end of "Description" in paragraph "SEE ALSO".
;;
;;     Section (required)
;;
;;	This field contains category for package. The valid keywords are
;;      those listed in `M-x' `finder-list-keywords'.
;;
;;     Status
;;
;;	This field lists information about the package. Each keyword
;;	has a unique mening. the allowed list:
;;
;;	    keyword := 'core-emacs'
;;		       | 'core-xemacs'
;;		       | 'unmaintained'
;;		       | 'broken'
;;		       | 'note' YYYY-MM-DD [COMMENT] ';'
;;
;;	And example:
;;
;;	    Status: unmaintained
;;		broken
;;		note YYYY-MM-DD Doesn't work in Emacs 23.
;;		See thread http://example.com ;
;;
;;	The `core-*' values mark the package being included (or will
;;	be) in the latest [X]Emacs. Value `unmaintained' means that
;;	the original developer has vanished or abandoned the project
;;	and is no longer available for developing the package. Value
;;	`broken' means that package is broken and does not work in
;;	some Emacs version (usually latest). The `note' keyword can be
;;	used for any kind of information. It is adviced that notes are
;;	time stamped using ISO 8601 YYYY-MM-DD format. A note ends in
;;	character `;' and can be of any length.
;;
;;     Vcs-Browser
;;
;;	The URL address to the version control browser of the repository.
;;
;;     Vcs-Type
;;
;;      Version Constrol System information. The value is the
;;      lowercase name of the version control program. A special value
;;      "http" can be used to signify direct HTTP download. An example
;;      of an Emacs package hosted directly at a web page:
;;
;;	    Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/vline.el
;;
;;     Vcs-Url
;;
;;	The technical repository URL. For CVS, this is the value of
;;	CVSROOT which includes also the protocol name:
;;
;;	    Vcs-Url: :pserver:anonymous@example.com/reository/foo
;;
;;     Vcs-User
;;
;;	The login name. In case the repository cannot be accessed
;;	simply by visiting the `Vcs-Url' (or in the case of CVS:
;;	pressing RETURN at login prompt), this is the login name.
;;
;;     Wiki
;;
;;	This field points to package at <http://www.emacswiki.org>. If
;;	it does not exists, consider creating one for the PACKAGE.
;;
;;     X-*
;;
;;      Any other custom fields can be inserted using `X-*' field
;;      notation:
;;
;;          X-Comment: <comment here>
;;          X-Maintainer-Homepage: <URL>

;;; Change Log:

;;; Code:

(defconst epackage-version-time "2010.1128.1727"
  "*Version of last edit.")

(defcustom epackage--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'Epackage)

(defcustom epackage--root-directory
  (let (ret)
    (dolist (elt (list
		  (if (feturep 'xemacs)
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
  "Name of package directory under `epackage--root-directory'.")

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
