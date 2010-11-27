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
;;  Epackage git repository layout
;;
;;      The packages are installed under root `epackage-root-directory',
;;      which defaults to ~/.emacs.d and ~/.xemacs.d respectively. The
;;      root directory is organized as follows:
;;
;;          ROOT
;;          |
;;          +--enabled/
;;          |  <package>-activate*.el files
;;          |
;;          |--disabled/
;;          |  files from enabled/ are moved here when user chooses to
;;          |  uninstall a package. Any code that is changed here is
;;          |  simply copied back when package is again "installed".
;;          |
;;          +--vc/     Version control repositories
;;          |  |
;;          |  +--<package-one>/
;;          |  +--<package-two>/
;;          |  +-- ...
;;          |
;;          +-- ...
;;
;;  Epackage version control layout
;;
;;      The Git repository branches used are: FIXME (not decided yet)
;;
;;      o   =upstream=, required. The original unmodified upstream code.
;;	    Each commit is tagged with label "upstream/YYYY-MM-DD[--VERSION]".
;;	    The YYYY-MM-DD is the date of upstream"s release or best
;;	    guess and it is accompanyed with "--VERSION" that the
;;	    upstream used. The ISO 8601 date is needed so that the 1)
;;	    release date is immediately available e.g. for post
;;	    processing and 2) the tags sort nicely by date. And
;;	    example: "upstream/2009-12-31--0.3"
;;      o   =master=, required. Branched off from "upstream". Adds directory
;;	    `epackage/'
;;      o   =patches=, optional. Patches to latest "upstream* code.
;;      o   =master=, required. Branched off from "upstream". The "ready
;;	    to use installation". This branch is never used directly.
;;	    It is a result of merges with other branches: typically
;;	    "epackage" and sometimes other branches like "patches".
;;
;;  Epackage layout specification
;;
;;      The used method borrows concepts from the Debian package build
;;      system, where a separate control directory is reserved for
;;      packaging files. The directory name "epackage" is not
;;      configurable. All the files resides under it:
;;
;;          <some package>
;;          |
;;          +-- epackage/
;;		info			required: The package control file
;;		PACKAGE-install.el	required: Code make package available
;;		PACKAGE-activate.el	optional: Code to activate package
;;		PACKAGE-autoloads.el	optional: all autoload statements (raw)
;;		PACKAGE-loaddefs.el	optional: ###autoload statements
;;
;;	The difference between 'activate' and 'install' is that
;;	activate modifies current environment by turning mode on and
;;	possibly defining keybindings etc. to make the features
;;	immediately available. The 'install' only provides interactive
;;	commands in latent `autoload' form which the user can call via
;;	`M-x'. The 'install' file never modifies the environment and
;;	is safe to load.
;;
;;  File: info
;;
;;      A RFC 2822 formatted file (email), which contains information
;;      about the package. The minumum required fields are presented
;;      below. The header field names are case insensitive. Continued
;;      lines must be intended; suggested indentation is one space
;;      for easy formatting with any editor.
;;
;;          Package: <name>
;;          Section: <name>
;;          Description: <short description max 65 chars>
;;           [<long description>]
;;
;;      The *Package* field is the PACKAGE part from file name
;;      package.el or the canonical known name in case of bigger
;;      packages like "gnus". The *Section* field is one of the
;;      `finder-list-keywords'. The first line of the *Description*
;;      field should be consise and fit on maximum line length of 80
;;      characters in order to display in combined format "PACKAGE --
;;      SHORT DESCRIPTION".
;;
;;	The rest of the fields are optional, but highly recommended:
;;
;;          License: <keywords: 'GPL[-VERSION+?]', 'BSD' ...>
;;          Depends: <[x]emacs [(>= VERSION)], package ...>
;;	    Conflicts: <like Depends field>
;;          Status: <kewords; remarks, see below>
;;          Homepage: <URL to upstream project page>
;;          Wiki: <URL to emacswiki.org page>
;;
;;      The upstream's version control information can be given
;;      in optional fields. For 'cvs', the Url is the value of CVSROOT.
;;	No specific command line options are listed in these fields.
;;
;;          Email: First Last <address@example.com>[, ...]
;;          Vcs-Type: <'http', 'git', 'bzr', 'hg', 'svn', 'cvs' ...>
;;          Vcs-Url: <URL>
;;          Vcs-Browser: <UR; http address to browseable repository>
;;          Bugs: <URL: email address(es), web page; 'M-x' FUNCTION>
;;
;;      Any other custom field can be inserted using `X-*' header
;;      notation:
;;
;;          X-Comment: <comment here>
;;          X-Maintainer-Homepage: <URL>
;;
;;      A typical example of `info' file:
;;
;;          Package: foo
;;          Section: tools
;;          License: GPL-3+
;;          Depends: emacs (>= 21)
;;          Vcs-Type: git
;;          Vcs-Url: git@github.org/project/foo/foo.git
;;          Homepage: http://example.com/project/foo
;;          Wiki: http://www.emacswiki.org/FooMode
;;          Description: <short description>
;;            [<longer description>]
;;
;;  Details of the info file fields
;;
;;      The *Depends* field announces if packaage requires particular
;;      Emacs flavor, like 'emacs (>= 22)', or if package depends on
;;      other packages. The *'Depends* and *Conflicts* follow
;;      guidelines of
;;      <http://www.debian.org/doc/debian-policy/ch-relationships.html>.
;;
;;      For the *Homepage* field, it is adviseable to use 'big' project
;;      site addresses that don't move; Freshmeat.net, Sourceforge,
;;      Launchpad, Github etc. The Freshmeat is especially good
;;      freshmeat.net, because is provides an easy hub to all other
;;      Open Source projects. User's can quickly browse related
;;      software and subscribe to project announcements. Freshmeat is
;;      also easy for the upstream developer to set up. This should
;;      not be direct link to a volatile personal homepage if an
;;      alternative exists.
;;
;;      The *License* field is automatically assumed 'GPL-2+' if the
;;      field is missing. The valid License abbreviations listed
;;      should follow list <http://wiki.debian.org/CopyrightFormat>.
;;
;;      *Vcs-Type* field is all lowercase; the name of the program. A
;;      special value 'http', which is not a version control scheme,
;;      but direct HTTP download location. An example of an Emacs package
;;      hosted directly at a web page:
;;
;;	    Vcs-Type: http
;;          Vcs-Url: http://www.emacswiki.org/emacs/download/vline.el
;;
;;	The *Status* field lists keywords that have unique mening. If
;;	Value `core-*' marks that the package has been included (or
;;	will be) in latest [X]Emacs. The field format is explained
;;	below. Value `unmaintained' means that the original developer
;;	has vanished or abandoned the project and is no longer
;;	available for developing the package. Value `broken' means
;;	that package is broken and does not work in some Emacs version
;;	(usually latest). The `note' keyword can be used for any kind
;;	of information. It is adviced that notes are time stamped
;;	using ISO 8601 YYYY-MM-DD format. A note ends in character
;;	`;' and can be of any length.
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
;;	The *Wiki* field points to package'shttp://www.emacswiki.org
;;	page. If it does not exists, consider creating one.

;;; Change Log:

;;; Code:

(defconst epackage-version-time "2010.1127.1527"
  "*Version of last edit.")

(defcustom epackage--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'Epackage)

;;;###autoload (autoload 'epackage-mode          "epackage" "" t)
;;;###autoload (autoload 'turn-on-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'tun-off-epackage-mode  "epackage" "" t)
;;;###autoload (autoload 'epackage-commentary    "epackage" "" t)

;; FIXME: Unfinished, this is at a sketch / planning phase.

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "epackage-" " Tnbr" "z" "Nbr" 'Tnbr "epackage--"
   "Simple number conversion minor mode.

Mode description:

\\{epackage--mode-prefix-map}"

   "Epackage"
   nil
   "Number conversion mode"
   (list                                ;arg 10
    epackage--mode-easymenu-name
    ["int to hex"  epackage-int-to-hex  t]
    ["int to oct"  epackage-int-to-bin  t]
    ["int to bin"  epackage-int-to-oct  t]
    "----"
    ["hex to int"  epackage-hex-to-int  t]
    ["oct to int"  epackage-oct-to-int  t]
    ["bin to int"  epackage-bin-to-int  t]
    "----"
    ["Package version"    epackage-version        t]
    ["Package commentary" epackage-commentary     t]
    ["Mode help"   epackage-mode-help   t]
    ["Mode off"    epackage-mode        t])
   (progn
     (define-key   map "X" 'epackage-hex-to-int)
     (define-key   map "B" 'epackage-bin-to-int)
     (define-key   map "O" 'epackage-oct-to-int)
     (define-key   map "x" 'epackage-int-to-hex)
     (define-key   map "b" 'epackage-int-to-bin)
     (define-key   map "o" 'epackage-int-to-oct)
     (define-key   map "v" 'epackage-version)
     (define-key map "?"  'epackage-mode-help)
     (define-key map "Hm" 'epackage-mode-help)
     (define-key map "Hc" 'epackage-commentary)
     (define-key map "Hv" 'epackage-version))))


(add-hook  'epackage--mode-hook 'epackage-mode-define-keys)
(provide   'epackage)
(run-hooks 'epackage--load-hook)

;;; epackage.el ends here
