;;; tinypath.el --- Manage Emacs startup dynamically

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1999-2010 Jari Aalto
;; Keywords:        extensions
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
;;
;;  The very fast start
;;
;;      If you want to do the reading later, follow these steps. No
;;      guarantees that this will work. If it did't, have a coffee near you
;;      and read the whole documentation.
;;
;;      o   Include perl script *emacs-util.pl* in your `PATH'. If you
;;          do not have perl, get it for Unix at http://www.perl.com/ or
;;          install http://www.cygwin.com/ to your Win32 operating system.
;;      o   If you use XEmacs, see `tinypath--core-emacs-load-path-list'
;;      o   Make sure all your personal Emacs Lisp files are under any of these
;;          directories: `$HOME/elisp', `~/.emacs.d' (new Emacs), `~/.xemacs'.
;;      o   Create directory `$HOME/elisp/config' where cache will be saved.
;;      o   Include these lines near top of startup file: `$HOME/.emacs'
;;
;;          ;; $HOME/.emacs
;;
;;          ;;  PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;;  THAT ACTIVATE IF YOU use absolute path
;;
;;          (add-to-list 'load-path "~/elisp/tiny-tools-NNNN.NNNN/lisp/tiny")
;;
;;          ;; - If you use XEmacs that ships the lisp
;;          ;;   files in separately, tell where the directories are
;;          ;; - See http://www.xemacs.org/Develop/cvsaccess.html
;;          ;;   for cvs access and easy update (2003-05-20).
;;
;;          (when (featurep 'xemacs)
;;            (setq  tinypath--core-emacs-load-path-list
;;                 '("/usr/local/share/xemacs/xemacs-packages"
;;                   "/usr/local/share/xemacs/site-packages"
;;                   "/usr/local/share/xemacs/mule-packages")))
;;
;;          (load "tinypath.el")
;;
;;          ;; <the rest of your Emacs setup below this>
;;
;;          ;; End $HOME/.emacs
;;
;;      o   After Emacs has been started, call `M-x'
;;          `tinypath-cache-problem-report'. In the generated buffer see
;;          `C-h' `m' mode help for available commands.
;;
;;  First user note
;;
;;      You may see message "TinyPath: EXT Process running ...
;;      [may take a while]" and Emacs hangs for a while when you use this
;;      package for the first time. Please wait and read the documentation
;;      about "Faster Emacs configuration" later in this file.
;;
;;      ********************************************************************
;;      It is preferred that you use the EXT method, because the TRAD(itional)
;;      lisp method has a drawback. It does not support rearranging
;;      paths to order: 1) $HOME 2) site-lisp-files 3) core-emacs-lisp-files
;;      ********************************************************************
;;
;;      The perl method guarantees, that anything you put into your
;;      private `~/elisp' will override and precede any other package
;;      found elswhere in `load-path' hierarchy.
;;
;;      At any time you can read the manual with `M-x' `tinypath-version'
;;
;;  Cache file location
;;
;;      Create a directory where the cache information is saved. The location
;;      can be set by changing `tinypath--cache-file-prefix' which should be
;;      pathname + file-prefix. The cache size depends on your
;;      installed files, with 600 directories and 8000 lisp files, the
;;      cache size is around 500k and if you use compression, it takes
;;      somewhere 200k.
;;
;;          mkdir -p ~/elisp/config     (or ~/.emacs.d/config)
;;
;;  Transparent compression
;;
;;      If space is tight, this package supports transparent
;;      compression. The files can be kept in compressed format
;;      without touching code in startup files. Calls like below are
;;      interpreted as if there were a `.el.gz' or `.el.bz2'
;;      extensions attached to the files. See
;;      `tinypath--compressed-file-extensions' for more.
;;
;;          (load "some-file")
;;          (require 'somefile)
;;
;;      This transparent support however comes with a prolonged search
;;      time, because more attempts must be made in order to find the
;;      file. If all the files are in non-compressed format and you do
;;      not plan to use the compression support, a much better
;;      performancs can be achieved by turning the support off (it's
;;      off by default). To turn it on, use:
;;
;;          (setq tinypath--compression-support 'default)
;;
;;  Contact and support
;;
;;      Call `tinypath-debug-test-run' if you think there is something
;;      odd going on. All the messages will appear in *Messages*
;;      buffer (Emacs); under XEmacs, examine " *Message-Log*"
;;      buffer. If you have any questions, contact maintainer and
;;      don't forget to send contents of the *Messages* buffer.
;;
;;      ********************************************************************
;;
;;      IT IS HIGHLY RECOMMENDED THAT YOU VALIDATE YOUR SETUP
;;      AFTER YOU HAVE LOADED THIS PACKAGE
;;
;;      Start Emacs and call report function to investigate any problems,
;;      like duplicate packages that shadow each other. See documentation
;;      below for more. The general rule is that you should delete
;;      any offending packages. key `C-d' to delete offending file permanently.
;;
;;          C-u M-x tinypath-cache-problem-report   (or without C-u argument)
;;
;;      *******************************************************************

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:
;;
;;  Preface Feb 1999 - How it all begun
;;
;;      When you have set up your Emacs installation to your liking, a day
;;      comes when you decide that it's time to seriously reconsider the
;;      directory structure of your installed lisp packages. At start, it
;;      is customary to use simple file hierarchy where all private
;;      packages are installed under:
;;
;;          ~/elisp    (in new Emacs: ~/.emacs.d)
;;
;;      Bigger packages are usually installed directly under their release
;;      directories:
;;
;;          ~/elisp/packages/bbdb-2.00.06/
;;          ~/elisp/packages/psgml-1.0.3/
;;          ~/elisp/packages/pcl-cvs-2.9.2/
;;
;;      A more sophisticated way is to use symlinks to the latest
;;      versions, so that you don't have to change `load-path' every
;;      time you install a new version. It is only matter of updating
;;      the symlink:
;;
;;          ~/elisp/packages/foo  --> ~/elisp/packages/foo-2.9.2
;;          |
;;          This path is in the `load-path'
;;
;;      In network, where Windows is coupled with Unix workstations via SAMBA,
;;      you may have mapped some disk, say H:, to you Unix _$HOME_:
;;
;;          H:  --> Unix $HOME  \\SERVER\DIRECTORY\YOUR-LOGIN-DIR
;;
;;      Now, there is a catch when Unix symlinks are used in `$HOME/elisp'
;;      and the directories are accessed from Windows. Having set PC's
;;      HOME environment variable to point to H:, Emacs can start reading
;;      Unix `$HOME/.emacs' startup file, but there appeared messages
;;      like "Can't load library xxx", which was soon followed by
;;      bigger concerns: "autoloading xxx failed". The problem was the
;;      mounted H: disk. You see, PC's network mount can't distinguish
;;      symlinked directories from real directories, so all symlinked Unix
;;      directories in `load-path' were dead. And that's why most of the
;;      files couldn't be found any more.
;;
;;     The conclusions
;;
;;      For cross platform solution it is best not to rely on symlinks,
;;      because they don't work well over a Windows mounts. Secondly,
;;      updating `load-path' should not be needed by hand after a new
;;      package installation, after a directory name change, after
;;      directory structure change, etc. A dream package would solve this
;;      all and do the hard work: "There, that is the root(s) of all Emacs
;;      lisp, go and search all the directories and update `load-path'"
;;
;;      That was what this package originally was all about. Later it
;;      evolved to contains a little more than that. The `load-path'
;;      is updated automatically without any manual work. Only the
;;      start ROOT path(s) of installed lisp hierarchies need to be
;;      known. This package is highly effective: scanning thousands of
;;      files in a matter of seconds and once the cache has been
;;      created, it takes only a snap to load it in next sessions. All
;;      `require' and `load' commands also execute faster than
;;      previously, because the information about existing files is
;;      immediately available. The speedup is helped through advised
;;      functions.
;;
;;  Overview of features
;;
;;     Automatic load-path configuration
;;
;;      o   Define list of `root' directories of your Emacs lisp and this
;;          package will recursively add directories which contain .el or
;;          .elc files to `load-path'
;;      o   A cache is utilized to remember previous scan and
;;          expired periodically. Using cache speeds up loading files
;;          considerably if you have many directories. The number of lisp
;;          directories doesn't affect the load performance.
;;          This is accomplished by using extra advice code in functions:
;;          `load', `load-library', `require', `locate-library' and
;;          `autoload'.
;;      o   When Emacs becomes idle (some 15 minutes of idle time) the
;;          cache and `load-path' is validated for erroneous entries and
;;          rebuilt as needed. This feature should auto-detect changes in
;;          directory structure and help semi auto-installing
;;          new directories (i.e. packages) for you.
;;      o   The `load-path' is optimized so, that users' files automatically
;;          take precedence first (~/elisp), next any other files found,
;;          and last the core Emacs files in the distribution.
;;
;;     Automatic Info-default-directory-list configuration
;;
;;      o   If you download packages that include Emacs info files,
;;          the `Info-default-directory-list' is updated at the same time
;;          as the `load-path', when root directories are examined.
;;      o   No more manual updating of info files. The missing
;;          `dir' entry is created or updated as needed.
;;      o   You can update all _new_ info files in your system by calling
;;          M-x `tinypath-info-scan-Info-default-directory-list'
;;
;;      If new info filesare added by hand, call function
;;      `tinypath-info-handler' to update your Emacs and update the
;;      `dir' entry. After that reset old information with `M-x'
;;      `tinypath-info-initialize'.
;;
;;      This feature was designed to be used under Windows where
;;      Cygwin installation provided many manual pages, which would
;;      have been handy to read under Win32 Native Emacs. The catch
;;      was how to mix Cygwin + Native Emacs for manual page and info
;;      page reading. Under *nix this feature is of limited usability,
;;      because info pages are installed in orderly manner by the system
;;      installation scripts.
;;
;;     Win32 automatic manpath configuration
;;
;;      o   In Unix systems the MANPATH enavironment variable contains
;;          directories where to find manual pages, but in Win32,
;;          there is no default MANPATH and `M-x' `man' does not work.
;;      o   If package *woman.el* (Included in latest Emacs
;;          versions) is along `load-path', it is automatically
;;          configured to support to read manual pages. It replaces
;;          the `M-x' `man' command.
;;
;;     Win32 Cygwin environment support
;;
;;      o   If *cygwin1.dll* (<http://www.cygwin.com/>) is in `exec-path',
;;          automatic detection tries to find the Cygwin root and scan
;;          manual pages and info pages for use with *woman.el*
;;          _Note:_ This feature is for native Win32 Emacs. Nowadays,
;;          there is also native Cygwin Emacs, which behaves just like
;;          the big brother *nix Emacs.
;;
;;     Compressed lisp file support
;;
;;      o   Overloads commands load, load-library, load-file, require
;;          and autoload to accept `jka-compr' compressed lisp .el files.
;;      o   Primarily meant to be used in low quota accounts.
;;      o   Compress or decompress lisp files. You don't have to change
;;          a thing in your Emacs startup file, all will work as usual.
;;      o   Handle aliased commands that turn out to be
;;          in `autoload' state.
;;
;;  How to set up your load path
;;
;;      The `tinypath--load-hook' should contain function
;;      `tinypath-setup' which starts examining all directories under
;;      `load-path' and `tinypath--load-path-root' which is set to
;;      reasonable defaults of site wide and personal installations.
;;      If you keep all your lisp files under *$HOME/elisp*, then you
;;      do not need to configure anything for this package to work.
;;      Your `load-path' will be updated after this code at the
;;      beginning of your *$HOME/.emacs*
;;
;;          (load "~/elisp/tiny/tinypath") ;; Or anywhere you have it installed
;;
;;      If there are _many_ separate Emacs lisp root directories, like
;;      one for *site-lisp* and one for *site-packages* and one for
;;      *personal* *lisp* files, then those directories should be
;;      added to variable `tinypath--load-path-root'. Below there is
;;      an example for PC users, where the E: partition replicates
;;      identical Unix tree structure. We suppose for a moment that
;;      Cygwin is installed also there. The following actually works for
;;      shared Unix Emacs setup file too, because non-existing
;;      directories will get ignored:
;;
;;          (setq tinypath--load-path-root
;;            '("~/elisp"  "E:/usr/share/emacs/site-lisp/common"))
;;          (load "~/elisp/tiny/tinypath")
;;
;;  Peiodic load path syncronization watchdog
;;
;;      If new lisp packages are installed and tried reularly when new
;;      development versions are tracked, then the manual need to call
;;      `M-x' `tinypath-cache-regenerate' may become tiresome. There
;;      is a built in idle timer watchdog included in the package, but
;;      it is not activated by default. Its job is to examine load path
;;      every now and them when Emacs is idle to see if the `load-path'
;;      has gone out of synch i.e. new paths have appeared, old ones removed
;;      or new packages has been added. This feature is experimental and
;;      the scanning may be quite resource intensive because bursts of disk I/O
;;      is needed to determine the status of the paths and files. To enable
;;      it, you must set the load hook before anything else:
;;
;;          (setq tinypath--load-hook
;;             '(tinypath-install tinypath-install-timer))
;;          ...
;;          (load "~/elisp/tiny/tinypath")
;;
;;  XEmacs and Emacs specific directories
;;
;;      In spite of great effort from developers to make packages
;;      compatible for both Emacs platforms, there is always some packages
;;      that only work with Emacs or XEmacs. It is assumed that the site
;;      admin has created directories like these to keep the *site-lisp*
;;      installation clean:
;;
;;          ;;   This might be also under /opt/share/site-lisp
;;          ;;   Refer to file hierarchy standard at
;;          ;;   http://www.pathname.com/fhs/
;;
;;          /usr/share/emacs/site-lisp/common/   .. XEmacs and Emacs
;;          /usr/share/emacs/site-lisp/emacs/    .. only for Emacs
;;          /usr/share/emacs/site-lisp/xemacs/   .. only for XEmacs
;;
;;      To take care of the Emacs specific `load-path' setting, use code
;;      similar to the below. If you load the setup multiple times, the
;;      `pushnew' ensures that the directories are not added multiple
;;      times.
;;
;;          (require 'cl)
;;          (dolist (path ("~/elisp"
;;                         ;;  For both Emacs and XEmacs
;;                         "/usr/share/emacs/site-lisp/common"
;;                         ;;  Select Emacs or XEmacs specific installations
;;                         (if (boundp 'xemacs-logo)
;;                             "/usr/share/xemacs/site-lisp"
;;                           "/usr/share/emacs/site-lisp/emacs")))
;;            (when (stringp path)
;;              (pushnew path tinypath--load-path-root :test 'string=)))
;;
;;          ;; PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;; THAT ACTIVATE IF YOU ADD THE PATH
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'string=)
;;          (load "tinypath.el")
;;
;;      The package will check current emacs version and make sure
;;      that only correct directories are included to the
;;      `load-path'. If you simply instructed to search the whole
;;      site-lisp root `/usr/share/site-lisp', and current emacs
;;      binary is "emacs", then all directories that contain path
;;      portion `/xemacs' would have been automatically ignored.
;;
;;     Building part of site-lisp from Internet
;;
;;      If we continue talking a bit more about site-lisp, there is utility
;;      *pwget.pl* at <http://freshmeat.net/projects/perlwebget/>. It
;;      includes an Emacs configuration settings which contain knowledge
;;      where the various lisp developers' home pages are and how to
;;      download lisp packages. If you have lot of disk space and you're
;;      interested in experimenting with more packages to go with your
;;      Emacs, follow the instruction laid out in the pwget's project page.
;;
;;      Now, the overall structure of whole site-lisp might look
;;      something like this:
;;
;;                   ROOT/  ( /usr/share/emacs or equivalent )
;;                   |
;;                   +--site-lisp/
;;                      |
;;                      +--emacs/
;;                      |  |  ...Emacs only files
;;                      |  +--packages/
;;                      |  |  +--pcl-cvs-2.9.9/
;;                      |  |  +-... and so on
;;                      |  +--win32/
;;                      |     +--gnuserv/
;;                      |     +-... and so on
;;                      +--net/
;;                      |  +--users/
;;                      |     +-LispDeveloperA
;;                      |     +-LispDeveloperB
;;                      |     +-... and so on
;;                      |  +--cvs-packages/
;;                      |     +--liece/
;;                      |     +--lookup/
;;                      |     +--ILISP/
;;                      |     +--jess-mode/
;;                      |     +--devel/
;;                      |     +--emacro/
;;                      |     +--tnt/
;;                      |     +--cc-mode/
;;                      |     +--mailcrypt/
;;                      |     +--bbdb/
;;                      |     +--gnus/
;;                      |     +-... and so on
;;                      +--common/
;;                      |     ...COMMON for both Emacs and XEmacs
;;                      |     =======================================
;;                      |     ...Packages that you find posted to the
;;                      |     ...gnu.emacs.sources and whose author's
;;                      |     ...do not have a homepage
;;
;;      For XEmacs, you would add:
;;
;;                   ROOT/  ( /usr/share/xemacs or equivalent )
;;                   |
;;                   +--site-lisp/
;;                      |
;;                      +--xemacs/
;;                         |  ...XEamcs only files
;;                         +--cvs-packages/
;;                            +--xemacs-packages/
;;
;;     XEmacs 21.2+ core packages
;;
;;      Some XEmacs versions come with only the very basic installation.
;;      Lisp packages may be distributed in separate archive
;;      *xemacs-packages* (nick named "SUMO" due to its huge size). There is
;;      also *mule-packages* and *site-packages* archives. A built-in
;;      heuristics tries to guess the location of these by looking under
;;      and near your XEmacs installation. Here is example from Win32:
;;
;;          .../XEmacs/XEmacs-NN.N/xemacs-packages
;;          .../XEmacs/xemacs-packages
;;
;;      If the archives have been installed elsewhere, you have to tell the
;;      location by defining following variable prior loading TinyPath. You
;;      can't put these to `tinypath--load-path-root' because this is
;;      special information that needs to present during the very initial
;;      boot-up to find crucial packages like *jka-compr.el*.
;;
;;          (setq tinypath--core-emacs-load-path-list
;;                '("/usr/share/site-lisp/xemacs/xemacs-packages"
;;                  "/usr/share/site-lisp/xemacs/mule-packages"
;;                  "/usr/share/site-lisp/xemacs/site-packages"))
;;
;;  Finding load-path directories
;;
;;      Supposing only default *$HOME/elisp* is used directory for files, the
;;      `tinypath--load-path-function' starts recursively searching all
;;      the directories under the root(s) `tinypath--load-path-root'. Not all
;;      directories are counted in when the search descends below the root(s).
;;      Variable `tinypath--load-path-ignore-regexp' decides if the directory
;;      should be ignored. By default:
;;
;;      o   Package's additional subdirectories like texinfo, tex, doc, etc,
;;          misc, RCS, CVS, .svn (Subversion), MT (monotone version control),
;;          .git, .hg, .darcs and 'zip' are ignored.
;;      o   Any temporary directories named .../t/ .../T/ .../tmp* .../temp*
;;          are ignored.
;;      o   Directories that do not contain any files ending to .el or .elc are
;;          ignored. (it's faster to do the above checks first).
;;
;;  Gnus and other 3rd party packages
;;
;;      _Note:_ In latest version of this utility *Gnus* is treated
;;      specially. All Gnus versions are detected along load-path and
;;      the very latest Gnus version is installed to your
;;      `load-path'. This is based on the knowledge in the
;;      `gnus-version' variable and the heuristics will pick the
;;      newest for you. You actually do not have to do anything else,
;;      but to drop latest Gnus somewhere, to be able to use it
;;      immediately.
;;
;;       Under the hood (old documentation)
;;
;;      It is important to understand how this package works: It caches
;;      every possible lisp directory it can find. Now, if you have
;;      installed private copy of Gnus, say in `~/elisp/cvs-packages/gnus',
;;      there is a problem, because Emacs distribution also includes Gnus.
;;      There is NO WAY TO TELL OR CHANGE path order when the cache is in
;;      use. This is a design decision and cannot be changed. The old trick,
;;      where a new directory was added in front of `load-path', will not
;;      work because everything goes through cache. What you need to do
;;      instead, is to tell that the "other" Gnus should be ignored during
;;      cache creation, so that it is completely unknown.
;;
;;     Solution: ignoring directories
;;
;;      There is very simple way. Put your regular expression to
;;      `tinypath--ignore-file-regexp-extra' and it will tell which
;;      directories to ignore. Define the ignore regexp before loading
;;      TinyPath:
;;
;;          (setq tinypath--load-path-ignore-regexp-extra
;;                "\\|[/\\]x?emacs[/\\0-9.]+[/\\]lisp[/\\]gnus")
;;          ;; PLEASE COPY VERBATIM. THERE ARE OPTIMIZATIONS
;;          ;; THAT ACTIVATE If YOU ADD THE PATH
;;          (require 'cl)
;;          (pushnew "~/elisp/tiny/lisp" load-path :test 'string=)
;;          (load "tinypath.el")
;;
;;      [For advanced Lisp programmers] You can add ignored gnus directory
;;      to `tinypath--load-path-ignore-regexp' via
;;      `tinypath--load-path-ignore-regexp-hook'. When the hook is run, the
;;      default value for `tinypath--load-path-ignore-regexp' is already
;;      available. In hook, append regular expression that excludes the
;;      Gnus directory. Here is an example; make sure that you don't add
;;      the regexp multiple times. The multiple invocations is protected by
;;      setting a plist property and checking it. The ugly [\\/] makes the
;;      regexp compatible with both Unix and win32 paths. System
;;      directories in Unix are typically /emacs/NN.NN/ and in win32
;;      /emacs-NN.NN/, that's why added "-".
;;
;;          (add-hook 'tinypath--load-path-ignore-regexp-hook
;;                    'my-tinypath--load-path-ignore-regexp-hook)
;;
;;          (defun my-tinypath--load-path-ignore-regexp-hook ()
;;            ;;  Do this only once
;;            (unless (get 'my-tinypath--load-path-ignore-regexp-hook 'set)
;;              ;; mark as done.
;;              (put 'my-tinypath--load-path-ignore-regexp-hook 'set t)
;;              (setq tinypath--load-path-ignore-regexp
;;                    (concat
;;                     tinypath--load-path-ignore-regexp
;;                     "[/\\]x?emacs[/\\0-9.]+[/\\]lisp[/\\]gnus"))))
;;
;;      FIXME: What about XEmacs public/private Gnus installations?
;;
;;  Updating new lisp packages
;;
;;      Suppose you have installed a new version of a package:
;;
;;          ~/elisp/gnus/foo-0.74/
;;          ~/elisp/gnus/foo-0.95/    ;; NEW
;;
;;      Both these directories end up being added to the `load-path',
;;      but that is not preferable. It is the latest version that
;;      should be in the `load-path'. The solution is to move the old
;;      versions under some name that will be ignored by default. It
;;      is recommended that a backup of previous packages are renamed
;;      to start with a word "tmp-". All directories that start with
;;      prefix *tmp* are ignored.
;;
;;          % mv ~/elisp/gnus/foo-0.74/ ~/elisp/gnus/tmp-foo-0.74/
;;                                                   ====
;;
;;      However if you update package in a site-lisp directory, there
;;      may be a distant problem that somebody needs older version of
;;      the package. If you made the backup like above, that user
;;      cannot load the old package any more, because it doesn't show
;;      up in `load-path'
;;
;;      There is no easy answer to keep old packages. Admin could
;;      announce that: "new version has been installed in DIR, the old
;;      one is in TMP-OLD-DIR" and have users manually arrange their
;;      `load-path' if needed. Following lisp command would solve
;;      their setup. The statement below adds the old directory to the
;;      *beginning* of `load-path' and thus load commands would find the
;;      old version of the package first.
;;
;;          (load "~/elisp/tiny/tinypath")
;;          ;;  Add more directories.
;;          (pushnew "TMP-OLD-OLD-DIR" load-path :test 'string=)
;;          (tinypath-cache-regenerate)
;;
;;      Remember to mention to users that they need to update cache with
;;      `tinypath-cache-regenerate' (called with prefix argument) to see
;;      the changes.
;;
;;  Duplicate files in path
;;
;;      If you have accustomed to putting your path to specific order,
;;      you have to rethink the strategy. The philosophy behind this
;;      utility was that there SHOULD BE NOT NEED TO DO MANUAL WORK TO
;;      UPDATE PATHS. This means that the order of the paths must not
;;      be significant. Now, you may face a situation where library or
;;      package contains a file, which has already been installed.
;;      Take for example, *smtpmail.el*:
;;
;;          /usr/bin/emacs-20.4/lisp/mail/smtpmail.el
;;          /usr/share/site-lisp/common/packages/semi/flim-1.12.1/smtpmail.el
;;
;;      There is a problem if FLIM's *smtpmail.el* is not compatible with
;;      the one in Emacs. If it is, then there is no problem. Either one
;;      can be loaded, and the `load-path' order does not matter. But you
;;      don't know that before you get error "function smtpmail-xxxx not
;;      defined" and you start investigating with (locate-library
;;      "smtpmail") which package is actually active.
;;
;;      Please investigate your path with [C-u] `M-x'
;;      `tinypath-cache-problem-report' and see if you find duplicate
;;      entries. Check each one and possibly move the file to another
;;      name or remove older ones. E.g. in the above situation, the
;;      cure might be moving FLIM's *smtpmail.el* under name
;;      *flim-smtpmail.el* so that it doesn't get loaded with (require
;;      'smtpmail). The BEST IS TO CONTACT THE MAINTAINER(S) and tell
;;      them about conflicts. Here is a sample of one generated
;;      problem report:
;;
;;          imenu.el
;;            323 34073 1998-05-07 16:28:08 /usr/share/site-lisp/common/other/
;;            910 37169 1999-12-04 02:47:58 /usr/share/site-lisp/common/programming/java/jde/jde-2.1.6beta13/lisp/
;;            1350 38663 1999-11-28 01:14:38 /usr/bin/emacs/gnu-emacs/emacs-20.4.1/lisp/
;;          base64.el
;;            515  9943 1999-12-11 19:15:20 /usr/share/site-lisp/common/packages/gnus-5.8.2/lisp/
;;            807  9892 1999-11-15 00:00:12 /usr/share/site-lisp/common/packages/w3-4.0pre.46/lisp/
;;
;;      _Explanation:_ Previously *imenu* was installed as a separate
;;      package. Now latest Emacs ships with one, so it is best to delete
;;      the previous one `other/imenu.el.' Keep on eye on the leftmost
;;      scores: The lower, the more close it is to the beginning of cache
;;      when the directories were searched. The package with lowest score
;;      will get loaded. Another package, *base64.el* seems to be
;;      problematic too. But because Gnus path has lowest score, it will
;;      get loaded before w3's base64.el. This is good, because Gnus
;;      contains the latest version of *base64.el*. In the buffer
;;      `tinypath-report-mode' a mode is turned on where you can manipulate
;;      reported lines. Unnecessary files can be deleted with
;;      `Control-shift-mouse-1' or `C-c' `C-d'.
;;
;;  Symlinked directories are ignored
;;
;;      It has been the tradition to use symlinks a lot in POSIX
;;      environments to arrange easy access to versioned packages. Like how
;;      to ~/elisp/gnus/ no matter what version is currently installed.
;;
;;          ln -s ~/elisp/packages/gnus-N.NN  ~/elisp/packages/gnus
;;
;;      This package however *skips* those symlinks and records the
;;      absolute path name to the `load-path'. There are couple of
;;      points: a) it is more instructive to peek the `load-path' to
;;      actually see what versions have been installed to the Emacs b)
;;      The symlinks are error prone since there may be several
;;      symlinks that lead to same directory and c) symlinks may not
;;      work well in heterogenous environments where Win32 and Linux
;;      and Unix hosts are networked together. To migrate to this
;;      package you need to examine your symlinks and remove them.
;;
;;      If you have drawn a symlink to the the current directory from
;;      *SEPARATE* directory, then that directory will never be seen:
;;
;;          ln -s ~/other-dir/elisp/artist-1.1/ ~/elisp/packages/artist-1.1
;;
;;      To solve this, instead either _a)_ move the package physically
;;      under the ~/elisp/ from the *~/some-disk/elisp/* so that the
;;      recursive search will record it or _b)_ add the separate
;;      directory *~/other-dir/elisp* to the variable
;;      `tinypath--load-path-root'.
;;
;;  Using cache
;;
;;      Now when you're freed from update burden of the directories in your
;;      disk, you can concentrate on organizing the files under sensible
;;      directories. Here is an example how the organizing could go:
;;
;;          ~/elisp/users/kevinr/       Kevin Rodger's files
;;          ~/elisp/users/ilya/         Ilya Zakharevich's files
;;          ..
;;          ~/elisp/packages/bbdb-2.00.06/  Version-ed packages
;;          ~/elisp/packages/psgml-1.0.3/
;;          ~/elisp/packages/pcl-cvs-2.9.2/
;;          ~/elisp/packages/tiny-19990215/
;;          ...
;;          ~/elisp/other/              All single add-on packages
;;
;;      All these paths in `load-path' and you can imagine how slow a
;;      standard Emacs would become: it takes even more time to find some
;;      package xxx, when Emacs sees a call (require 'xxx), because Emacs
;;      must start looking into every single directory under `load-path'
;;      until it can determine if it can or cannot load the asked package.
;;      This utility will store all lisp files in cache, and it is
;;      activated by default. The variable `tinypath--cache-expiry-days'
;;      controls the interval when it is concluded that a new tree
;;      recursion is needed. If you install new packages during those
;;      non-expiry days, it is best to call `C-u' `M-x'
;;      `tinypath-cache-regenerate' to build up to date image of your files
;;      and `load-path' directories.
;;
;;        If you want one short advice: always call `tinypath-cache-regenerate'
;;        after any lisp file or directory update.
;;
;;  Cache file and different Emacs versions
;;
;;      It is important that each Emacs loads correct cache file. The cache
;;      file's name is derived from the emacs version and emacs type, which
;;      can be "xemacs", "win32-xemacs", "emacs" or "win32-emacs".
;;
;;            tinypath--cache-file-prefix
;;          + EMACS-TYPE
;;          + HOST
;;          + EMACS-VERSION
;;          + tinypath--cache-file-postfix
;;
;;      An example:
;;
;;          ~/elisp/config/emacs-config-tinypath-cache-win32-HOST-emacs-20.4.1.el.gz
;;          ==========================================                        ======
;;          prefix                                                            postfix
;;
;;     Unix hosts and NFS mounts
;;
;;      In POSIX environment, it is also common that several hosts are
;;      NFS mounted so that the home disk is available from every
;;      server. The programs could also be NFS mounted, but many times
;;      programs are stored locally on each server's own disks. Now,
;;      there would be a problem if you logged to host *A* and started
;;      tinypath.el which had made cache in host *B*, because *A* does
;;      not have the same directories as *B* did (site-lisp). This has
;;      been taken care of by including _hostname_ part in the cache
;;      file name. For each host, a separate cache file is
;;      created. Now, suppose all the Unix hosts are same brand, say
;;      Sun OS, Linux, or HP-UX and a good administrator has separated
;;      the programs and the data in their own directory
;;      structures. Furthermore, these directories are NFS mounted and
;;      thus visible to the remote machines. In this scenario, it
;;      would not really matter to which host you log into, because
;;      you would always see the same programs and site-lisp
;;      directories and there would not be need for host specific
;;      cache files. In that case, it is possible to disable the
;;      *HOST* word by setting with:
;;
;;          (setq tinypath--cache-file-hostname-function nil)
;;
;;  Info file support
;;
;;      In addition to updating the `load-path', the recursive function
;;      has a chance to search for installed info files as well. When you
;;      keep all your site lisp under one directory, it is not uncommon
;;      that the bigger packages include documentation files in info format
;;      as well. Like:
;;
;;          /usr/share/site-lisp/emacs/pcl-cvs-2.9.9/
;;          /usr/share/site-lisp/common/packages/psgml-1.2.1/
;;
;;      One possibility is that after you download and uncompress a
;;      package, you copy the info file to some central directory
;;      where you keep all you info files. This is lot of manual work.
;;      (Never mind that in Unix you might use Makefile to install
;;      everything, in Win32 it's all manual work). This package does the
;;      same job by looking for directories that either have info files or
;;      a central info repository called `dir'. If the `dir' file
;;      has all the info files up to date, nothing is done. In other cases:
;;
;;      o   If the central `dir' in the directory does not exits,
;;          it is created.
;;      o   If `dir' does not contain entry for info file, it is added.
;;          The entry name is derived from the filename.
;;
;;      The `Info-default-directory-list' is updated to include any new
;;      directory locations and they are saved to same cache file. When you
;;      call `C-h' `i' you will see the new info entries. Easy and
;;      maintenance friendly. No need to worry about supplied info files any
;;      more, they are automatically integrated to your Emacs. If you have
;;      installed any new packages to your system, Emacs packages or Unix
;;      packages that installed something with "install -c", it is best to
;;      update your info files with `M-x'
;;      `tinypath-info-scan-Info-default-directory-list'. This is also
;;      called if you call: `C-u' `M-x' `tinypath-cache-regenerate'
;;
;;  Cygwin support (Win32 and woman.el)
;;
;;      It is common that Emacs in Win32 environment is coupled with
;;      <http://www.cygwin.com> toolkit which contains all the manual pages
;;      for the unix commands and possibly new info pages. This package
;;      will locate `cygwin1.dll' file along PATH and recurse whole cygwin
;;      installation root to find new entries that can be used inside
;;      Emacs. In theory this all should happen automatically and the only
;;      thing you have to do is to ensure that you have proper PATH
;;      settings at your OS level before this package is started. If Cygwin
;;      /bin directory in in PATH, `tinypath--extra-path-root' will get set
;;      to a correct value at boot time.
;;
;;      If you have more places where you keep Unix tools which contain
;;      more manual or info pages, like Reed Kotler (old Unix-like env)
;;      http://www.reedkotler.com/ you _must_ manually set variable
;;      `tinypath--extra-path-root' to the list of search root directories.
;;      If you set this yourself, you _must_ also include the cygwin
;;      installation root directory
;;
;;          (setq tinypath--extra-path-root
;;                '("e:/unix-root/cygwin"
;;                  "e:/unix-root/reed-kotler"
;;                  ...))
;;
;;      Package *woman.el* will be configured automatically if it is along
;;      `load-path' to handle manual page viewing with command `M-x'
;;      `man'. Please make sure that you do not destroy the pre-defined
;;      `woman-manpath' in your Emacs startup files with lisp commands or
;;      the efforts to find out new manual pages are thrown off the window.
;;      Search you startup files for anything that looks like `setq',
;;      `defvar', `defconst': (setq woman-manpath ... and change the code
;;      to _add_ to the variable instead:
;;
;;          (require 'cl)
;;          (dolist (path '("one" "two" "three"))
;;            (pushnew (expand-file-name path) woman-manpath :test 'string))
;;
;;  Faster Emacs configuration (Perl emacs-util.pl)
;;
;;      Indication of this feature at startup is a message, where
;;      EXT refers to externally launched process. It must be waited
;;      until further processing is done; i.e. Emacs is hung for a while.
;;
;;          TinyPath: EXT Process running ... [may take a while]
;;
;;      As this package evolved and more support was added to various
;;      environments, like Cygwin, which required traversing hundred of
;;      directories to find out if they contained info or manual pages,
;;      it came evident that Emacs Lisp method was too slow. An alternative
;;      method was developed using Perl language and written in *emacs-util.pl*
;;      which can traverse directory hierarchies to find relevant
;;      directories for the setup. This interface is automatically used
;;      if two conditions are met in current environment:
;;
;;      o   Binary *perl* must be along PATH. (according  `executable-find')
;;      o   perl script *emacs-util.pl* must be along PATH. Either copy
;;          the file to suitable place or include Tiny Tool's `/bin'
;;          directory to your PATH.
;;
;;      If all goes well, a `call-process' to the utility script will
;;      return the file hierarchies much faster than the Emacs Lisp ever
;;      could. The difference is that you don't see the traversing progress
;;      as you would if Emacs Lisp did the same thing. The command line
;;      arguments passed to the utility scripts can be found from the
;;      *Message* buffer and you can run the program yourself if you think
;;      that it returns incorrect listing. Print the script help with
;;      following command:
;;
;;          perl emacs-util.pl --help
;;
;;      Here are some performance statistics of the perl script in action.
;;      (Use --verbose argument to see the statistics)
;;
;;      o   Intel 400MHz, IBM GXP 80G IDE/ATA 100 disk, whole Cygwin
;;          installation scan: 3 min 46 sec, dirs: 2373, files: 35 271
;;      o   Same PC, but this time site-lisp directory, subset of Cygwin
;;          hierarchy at /usr/share/site-lisp took:
;;          0 min 13 sec, dirs: 648, files: 8750
;;
;;      Let's consider one scenario that you may encounter if you intend to
;;      use Cygwin similarly as the big brother Linux. Let's suppose that
;;      you have dedicated a disk portion where you intend to duplicate
;;      whole Linux-like directory hierarchy. You have ROOT under which you
;;      keep all the files, including anything that is Cygwin-related.
;;
;;          E:/usr/share/site-lisp Emacs lisp as outlined earlier
;;          E:/usr/share/site-perl Perl packages and scripts
;;          E:/usr/share/site-php  PHP code
;;          E:/usr/share/site-cvs  Various other external CVS C-packages
;;
;;      The default heuristics `tinypath-ti::win32-cygwin-p' should find
;;      *cygwin1.dll* installed and report that Cygwin root is *E:/*
;;      This means that `tinypath--extra-path-root' will get set for
;;      you when package loads. Suppose further that you have set
;;      variable `tinypath--load-path-root' to point out suitable
;;      locations in *E:/usr/share/site-lisp*. It would seem
;;      that this combination means that the hierarchies would be
;;      traversed multiple times, since the Cygwin root already
;;      includes all the rest:
;;
;;          E:/                             Cygwin root
;;          E:/usr/share/site-lisp/emacs    For this emacs...
;;          E:/usr/share/site-lisp/common   Emacs and XEmacs compatible tree
;;
;;      Don't worry. The Perl utility is smart enough to reduce this
;;      to search only *E:/* and discard other roots as redundant. Hm,
;;      what if other lisp files are found _outside_ of the
;;      *E:/usr/share/site-lisp/*, because it searches every dir
;;      starting from *E:/* Say:
;;
;;          E:/tmp/try/some-file.el
;;
;;      Will the directory *E:/tmp/try/* reported as lisp `load-path'
;;      candidate and added to search list? Yes and no. Yes, it will be
;;      reported, but no, it will not be added to the `load-path' because it
;;      doesn't match the initial user's idea where to look for lisp files. If
;;      you pump up the `tinypath--verbose' to level 5, you can see PATH-NOK
;;      messages labeled "candidate" to indicate those rejections. Only files
;;      that reside under `tinypath--load-path-root' directories are counted
;;      in.
;;
;;  Updating running Emacs
;;
;;      Suppose you have downloaded the latest versions of packages X, Y and Z
;;      and you want your current Emacs's paths updated, call this function:
;;
;;          M-x tinypath-cache-regenerate
;;
;;      Take a bit of skepticism: It is a fortunate event if it all
;;      worked that easily. You see, you already have several packages
;;      loaded in your Emacs and they are using the "old" code. Now
;;      you wiped the old directories away and told Emacs to look for
;;      only "new" directories.  After a while you may run into
;;      bizarre dependency problems. I recommend that after any major
;;      package update, which contains _several_ of files (like Gnus),
;;      you:
;;
;;      o    Install package and regenerate cache in current Emacs session
;;           with `M-x' `tinypach-cache-regenerate'.
;;      o    Save your current Emacs buffers (see *desktop.el*, *tinydesk.el*)
;;      o    Quit, restart Emacs and restore your working desktop.
;;
;;  Compressed lisp file support
;;
;;      In order to use the full compression support for autoload
;;      functions as well, set variable
;;      `tinypath--compression-support' to symbol `all'. The normal
;;      value for compression is 'default which support handling
;;      `require' and `load' commands. The variable must be set before
;;      package is loaded.
;;
;;     About Jka-compr package
;;
;;      jka-compr.el has native support to un/compress any file that
;;      have specific extensions. The handling is done via
;;      `file-name-handler-alist' and commands like these will load
;;      properly including any autoloads.
;;
;;          (load "my-el.gz")
;;
;;      The problem is that the load statements have to be manually
;;      changed so that they end in .gz so that jka-compr takes care
;;      of loading. What if the file is later uncompressed? Again all
;;      the load commands must be updated. This isn't very nice, since
;;      it should be able to un/compress elisp files and still have
;;      permanent load statements. Basically this is what the
;;      compression support here is all about; there is no need to
;;      worry if the file is compressed or not when advised functions
;;      are in effect. The following statement will work for both file
;;      types:
;;
;;          (load "my-el")
;;
;;     How the compressed loading works
;;
;;      o   When user request `load' FILE, try to find some compressed file
;;          that JKA knows about by adding extensions ".gz" and ".Z" and
;;          whatever user has configured JKA to handle. _LIMITATION:_
;;          only .gz .bz2 and the like that compress one file at a time
;;          is currently supported. Don't try using .zip or similar.
;;      o   If the FILE is absolute path, then look from that
;;          directory only.
;;      o   If no directory is given, find the file along the `load-path'.
;;      o   If there was somewhere a compressed file, just load it (because JKA
;;          will transparently uncompress it), eval it, and kill the buffer.
;;      o   If NO COMPRESSED file was found, just follow normal
;;          emacs rules.
;;
;;     Note: Why you should not prefer compressed .elc files
;;
;;      The purpose of compression support is to make it possible to
;;      have more useful lisp files in an account that has a limited
;;      disk space (quota). Many Unicersity student accounts have this
;;      limitation. Keeping lisp files in compressed format
;;      saves quite a much disk space.
;;
;;      o   Plain text, lisp `.el', files compress better.
;;      o   The documentation in comments is important, e.g all the
;;          instruction to use the file are there. Byte compiling
;;          strips away documentation.
;;      o   In order to debug or send bug reports you need .el files.
;;          The errors from .elc files are useless.
;;      o   The performance ratio that the .elc files offer may not
;;          be a crucial factor (many times you couldn't tell).
;;
;;     Note: advised emacs commands
;;
;;      The adviced functions can be further adviced, but
;;      if the redefined function uses `interactive-p' test, it will
;;      not indicate user call (like M-x load-library). The reason why
;;      the advised functions detect it, is that advice.el's
;;      `ad-do-it' macro cannot pass the interactive flag information
;;      to the original functions.
;;
;;  Trouble shooting
;;
;;      There is no denying it, this package is potentionally
;;      dangerous. When something goes wrong, it really goes wrong and
;;      your Emacs may be messed up completely. So, here are some
;;      trouble shooting tips, that you might want to try to rescue
;;      the situation or understand what is going on. The most usual
;;      blame is the *cache* content which does not contain the
;;      correct or up to date information.
;;
;;     Package is not found or loaded?
;;
;;      Please confirm that the file location is known and is in right
;;      directory by calling `M-x' `locate-library'. If the result is
;;      not correct, please check `tinypath--load-path-root' and
;;      `tinypath--extra-path-root'. Try to remedy the situation,
;;      regenerate cache with `C-u' `M-x' `tinypath-cache-regenerate'.
;;
;;     You don't know what particular package is causing troubles
;;
;;      Go to the *Message* buffer and clear it (`C-x' `h' followed by
;;      `C-w'). Run the path generation engine with debug `M-x'
;;      `tinypath-debug-external-helper' and study the output. It may
;;      be ignoring some files that you think should be included. Please
;;      check content of `tinypath--load-path-ignore-regexp' and
;;      `tinypath--load-path-ignore-regexp-extra'.
;;
;;     You need to see the internals
;;
;;      Call function `tinypath-cache-file-find-file' to display the current
;;      cache and use `C-s' and `C-r' to search entries in the file. Remember
;;      that you must not modify this file, because any changes you do, will
;;      get overwritten next time the cache is regenerated. The problem is
;;      somewhere else if you can see incorrect items in the cache file.
;;
;;  Code note: General
;;
;;      Because this package is among the first that is loaded from Emacs
;;      startup file, It contains copies of some functions from TinyLib
;;      libraries, to make the package independent until the point where
;;      the `load-path' has been set up and other libraries are available.
;;      In the code you may find marks "#copy:" which indicate code that
;;      has been copied/simplified to be used here. Autoload statements in
;;      this package defer loading functions until the end is reached and
;;      `load-path' is determined and the rest of the functions can be
;;      loaded from the libraries.
;;
;;  Code note: Where is that emacs package
;;
;;      If you ever need to know the location of a package that Emacs
;;      would load or has loaded, while this utility is in effect,
;;      use this call:
;;
;;          (insert (tinypath-cache-p "gnus.el"))
;;
;;      In fact the regular call yields same result, because
;;      `locate-library' is adviced:
;;
;;          (insert (locate-library "gnus.el"))
;;
;;      More easily, with *tinylisp.el*, which takes advantage of
;;      tinypath.el cache, you can load any emacs package for editing
;;      with command:
;;
;;          M-x load-library RET tinylisp RET
;;          M-x tinylisp-library-find-file
;;          (tinypath cache)Lisp Library: gnus.el RET
;;
;;      Alternatively there is mode hot-keys $ l f  and  $ l p :
;;
;;          M-x load-library RET tinylisp RET
;;          M-x tinylisp-install
;;          M-x tinylisp-mode  (in *scratch* buffer, see "E" in modeline)
;;          $ l f
;;          (tinypath cache)Lisp Library: gnus.el RET
;;
;;  Code note: Internal optimizations
;;
;;      In the installation section it is instructed that the location of the
;;      package is pushed into the `load-path' before the package is loaded:
;;
;;          (require 'cl)
;;          (pushnew "~/elisp/tiny/lisp/tiny" load-path :test 'string=)
;;          (load "tinypath.el")
;;
;;      Please follow this instruction. The reason is that program
;;      tries to use most efficient code to boot everything up and the
;;      first thing it does is to check the location where it has been
;;      saved. This package will use this information to assume that
;;      the Perl program is available somewhere near that that path
;;      (../../bin). If that fails, the Perl program is searched along
;;      `exec-path'. This is usually desirable, situation because
;;      every new installation includes newer version of Perl program
;;      and the one at `exec-path' may not be up to date. The perl
;;      code will speed up booting compared to pure Emacs Lisp
;;      implementation. In addition the Perl code section in this file
;;      (often referred as "external") has extra features included.
;;
;;  Code note: *Messages*
;;
;;      This package will print loads of messages to Emacs "*Message*" or
;;      XEmacs " *Message-Log*" buffer. This is a design decisions so that
;;      execution can be easily traced during Emacs load time. It also help
;;      reporting errors. The default `tinypath--verbose' 3 will log the most
;;      important messages.  Even if you set the level to 0 or nil, still
;;      some messages are displayed. Have a look at Message buffer if you have
;;      not much used it before. You may find interesting information to
;;      debug some of your own mis-configurations, like stale directories
;;      in `exec-path'.
;;
;;  Code note: Insinuating packages
;;
;;      Some packages can be auto-configured when the perl script
;;      reads the contents of the directories. Like package *woman.el*
;;      which needs to know the location of man path directories. For
;;      other packages there are different "installations". Gnus is
;;      one interesting example: Every Emacs and XEmacs release comes
;;      with Gnus version, which is usually outdated and many install
;;      Gnus privately. Multiple Gnus versions in the load paths is a
;;      problem and the wished situation is that there would be only
;;      the latest. Program's logic tries to find out which of the
;;      Gnus packages along `load-path' is the latest and hopefully
;;      after making the right decision (according to gnus-version-*
;;      variable) the other Gnus locations are hidden by modifying
;;      `load-path' and `tinypath--load-path-ignore-regexp'. This is a
;;      complimentary method to that suggested in this manual section's
;;      topic "3rd party packages".
;;
;;  Code note: Elp profiling results
;;
;;      The profiling results were run using method below. It must be note,
;;      that the `tinypath-external-*' is the time when the external perl
;;      program examines all the directories, so EXT time is not significant
;;      because it varies from system to system. The
;;      `tinypath-external-setup-parse-data' is the actual time spent in
;;      parsing the returned data. The functions that are called most of the
;;      time are the ones that must be kept on eye on and they seem to
;;      perform very well. Immediate below are the most important functions
;;      that perform the Parsing after the perl has returned results (these
;;      are not from the total listing, but after tweaking). The listing
;;      below represents timing results somewhere around 2001:
;;
;;          tinypath-external-output-parse                   1    4.89  4.89
;;            tinypath-external-output-parse-1               5    1.09  0.21
;;            tinypath-external-output-parse-1-cache         1    3.79  3.79
;;
;;
;;          tinypath-external-setup-parse-data               1    5.77  5.77
;;            tinypath-external-setup-1-load-path            249  0.70  0.002
;;            tinypath-external-setup-1-man-path             44   0.0   0.0
;;            tinypath-exec-path-append                      73   0.92  0.012
;;            tinypath-info-handler                          31   8.46  0.27
;;            tinypath-external-setup-cache                  1    0.0   0.0
;;
;;      These timing results was taken 2003-05-18 running Cygwin
;;      XEmacs 21.4.10, Pentium 400 Mhz. These profiling results are
;;      from the initial boot phase, before cache is loaded. It's
;;      pretty fast.
;;
;;          (setq tinypath--install-flag nil)
;;          (load "elp"
;;          (load "tinypath")
;;          (elp-instrument-package "tinypath-")
;;
;;          ;; Now run the boot phase ONLY
;;          (tinypath-load-path-initial-value
;;            tinypath--core-emacs-load-path-list)
;;
;;          Function Name                                   Count Elap   Ave
;;          =============================================== ===== =====  ===
;;          tinypath-load-path-initial-value                1     0.477  0.47
;;          tinypath-load-path-add-subdirs                  1     0.463  0.46
;;          tinypath-directory-subdirs                      1     0.451  0.45
;;          tinypath-emacs-root-directory                   1     0.008  0.00
;;          tinypath-emacs-root-by-load-path                1     0.008  0.00
;;          tinypath-emacs-core-path-p                      119   0.004  3.36
;;          tinypath-expand-file-name                       5     0.001  0.00
;;          tinypath-load-path-initial-value-xemacs         1     0.001  0.00
;;          tinypath-load-path-string-match                 1     0.001  0.00
;;          tinypath-win32-p                                5     0.0    0.0
;;          tinypath-emacs-versions                         1     0.0    0.0
;;
;;      Theses timing results was taken 2003-05-18 running Cygwin
;;      XEmacs 21.4.10, Pentium 400 Mhz. The cache with 4500
;;      directories was loaded from configuration file. In this case
;;      `tinypath--cache-file-postfix' value was '.el'. The timing
;;      information was tested and generated with:
;;
;;      o   `C-x' `C-f' tinypath.el RET  -- toad read tinypath.el to Emacs
;;      o   `M-x' `load-library' RET tinylisp.el RET
;;      o   `M-x' `turn-on-tinylisp-mode' RET
;;      o   $ -    to eval current buffer
;;      o   $ e I  to instrument everything (Wtih empty value, scan buffer)
;;      o   `M-x' `tinypath-cache-regenerate' RET
;;      o   $ e s  to show results
;;
;;
;;          Function Name                                   Count Elap   Ave
;;          =============================================== ===== =====  ===
;;          tinypath-install                                1     6.812  6.81
;;          tinypath-cache-setup-main                       1     5.76   5.76
;;          tinypath-setup                                  1     5.76   5.76
;;          tinypath-directory-list-clean                   7     3.756  0.53
;;          tinypath-cache-file-load                        1     2.552  2.55
;;          tinypath-load-path-clean                        1     2.272  2.27
;;          tinypath-cache-file-need-sync-p                 1     1.932  1.93
;;          tinypath-load-path-not-in-synch-p               1     1.932  1.93
;;          tinypath-exec-path-clean                        2     0.679  0.34
;;          tinypath-exec-path-check-verbose                2     0.597  0.298
;;          tinypath-exec-path-check                        2     0.594  0.297
;;          tinypath-load-path-merge                        1     0.364  0.364
;;          tinypath-Info-default-directory-list-clean      1     0.218  0.218
;;          tinypath-file-remove-trailing-slash             825   0.121  0.000
;;          tinypath-cache-mode                             1     0.082  0.082
;;          turn-on-tinypath-cache-mode                     1     0.082  0.082
;;          turn-on-tinypath-cache-mode-maybe               1     0.082  0.082
;;          tinypath-ti::advice-control                     1     0.081  0.081
;;          tinypath-install-timer                          1     0.007  0.007
;;          tinypath-cache-file-name                        2     0.006  0.003
;;          tinypath-exec-path-from-path                    2     0.006  0.003
;;          tinypath-ti::compat-timer-cancel-function       1     0.005  0.005
;;          tinypath-ti::compat-timer-elt                   2     0.004  0.002
;;          tinypath-cache-warn-if-not-exist                1     0.004  0.004
;;          tinypath-cache-file-old-p                       1     0.004  0.004
;;          tinypath-days-old                               1     0.002  0.002
;;          tinypath-cache-status-string                    2     0.002  0.001
;;          tinypath-cache-status-message                   1     0.002  0.002
;;          tinypath-advice-instantiate                     1     0.002  0.002
;;          tinypath-expand-file-name                       3     0.001  0.000
;;          tinypath-ti::compat-timer-cancel                1     0.001  0.001
;;          tinypath-use-compression-maybe                  2     0.001  0.0005
;;          tinypath-exec-path-append                       1     0.001  0.001
;;          tinypath-win32-p                                6     0.0    0.0
;;          tinypath-emacs-versions                         2     0.0    0.0
;;          tinypath-ti::date-time-difference               1     0.0    0.0
;;          tinypath-eval-after-load                        1     0.0    0.0
;;          tinypath-time-string                            2     0.0    0.0
;;          tinypath-file-compressed-p                      2     0.0    0.0
;;          tinypath-emacs-lisp-file-list-cache-clear       1     0.0    0.0
;;          tinypath-autoload-file-name                     1     0.0    0.0
;;          tinypath-autoload-require                       1     0.0    0.0
;;          tinypath-cache-p                                1     0.0    0.0
;;          tinypath-cache-file-hostname                    2     0.0    0.0
;;          tinypath-load-path-root-changed-p               1     0.0    0.0
;;
;;      Same timing test as above, but now using compiled cache file at
;;      Emacs startup. In this case `tinypath--cache-file-postfix' value was
;;      '.elc'. The speedup is 50%, reducing the load time to mere 3-4
;;      seconds. Notice the dramatic change in `tinypath-cache-file-load':
;;      0.5 seconds vs. 2.5 seconds non-compiled.
;;
;;          Function Name                                   Count Elap   Ave
;;          =============================================== ===== =====  ===
;;          tinypath-install                                1     3.305  3.30
;;          tinypath-cache-setup-main                       1     2.017  2.01
;;          tinypath-setup                                  1     2.017  2.01
;;          tinypath-directory-list-clean                   7     1.608  0.22
;;          tinypath-load-path-clean                        1     0.904  0.90
;;          tinypath-advice-instantiate                     1     0.784  0.78
;;          tinypath-cache-file-load                        1     0.549  0.54
;;          tinypath-exec-path-check                        2     0.506  0.25
;;          tinypath-exec-path-check-verbose                2     0.506  0.25
;;          tinypath-load-path-not-in-synch-p               1     0.368  0.36
;;          tinypath-cache-file-need-sync-p                 1     0.368  0.36
;;          tinypath-exec-path-clean                        2     0.326  0.16
;;          tinypath-exec-path-from-path                    2     0.154  0.07
;;
;;  Thoughts
;;
;;      o   In theory it would be possible to add /user@host:/path/to/dir/
;;          to `load-path', but that has never been tested.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ Require (a)

;;; ......................................................... &require ...

;;  While loading this package XEmacs garbage collects like mad.
;;  Ease it up for a while. These values are restored at the end.

(unless (get 'gc-cons-threshold 'tinypath-initial)
  (put 'gc-cons-threshold 'tinypath-initial gc-cons-threshold))

(put 'gc-cons-threshold 'tinypath gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 10))

;;  Why the provide is at the start of file?
;;  Because XEmacs does not record `load-history' entry unless it sees
;;  `provide' statement. There is a check for SELF LOCATION by looking at
;;  the `load-history' in this package

(provide   'tinypath)

(eval-when-compile
  (require 'cl))

(eval-and-compile

  (when (string-match "21.3" (emacs-version))
    ;; `dolist' is broken in Emacs 21.3 subr.el. Force loading
    ;;  it first, then wipe it with cl-macs.el. This way there
    ;;  is no chance that subr.el would be loaded ever again
    ;;  by some package
    (load "cl-macs"))

  ;;  These variables must be here in order to Byte compiler to see them
  ;;  before they are used.

  (defcustom tinypath--verbose-info-messages nil
    "*If non-nil, notify missing environment variables like USER.
This variable is meant for Win32 environment, where Unix style
USER and LOGNAME variables are not defined by default."
    :type  'boolean
    :group 'TinyPath)

  (defvar tinypath--boot-ignore-directory-regexp
    ;; #todo: /usr/share/emacs/21.3/lisp/obsolete
    "\\(CVS\\|RCS\\|info\\|texi\\|\\.svn\\|/MT\\)/?$"
    "While searching lisp boot subdirs, ignore those that match this regexp.
Popular version control directories are excluded by default.")

  ;;  #todo: Mysterious byte compile bug:
  ;;  Remove all cache files, compile tinypath, launch emacs.
  ;;  => Dies with a message of: "function member* not found".

  (unless (fboundp 'member*)
    (autoload 'member* "cl-seq"))

  (defconst tinypath--xemacs-p
    (or (boundp 'xemacs-logo)
	(featurep 'xemacs)
	(string-match "XEmacs" (emacs-version)))
    "Non-nil if running XEmacs.")

  ;;  Mostly for Win32 environment checks
  (defvar tinypath--startup-no-messages t
    "*If non-nil, do not display error message buffer at startup.
You should set this to `nil' if you begin to use this package first
time to see messages that may need attention. Alternatively, check
message buffer.")

  (defvar font-lock-mode) ;; Byte compiler silencers
  (defvar lazy-lock-mode)
  (defvar dired-directory)

  (autoload 'ti::macrof-version-bug-report  "tinylib" "" nil 'macro)

  (autoload 'pp                             "pp")
  (autoload 'assq                           "assoc")
  (autoload 'aput                           "assoc")
  (autoload 'executable-find                "executable")

  ;; Quiet byte compiler. These are checked with `boundp' in the code

  (defvar Info-default-directory-list)
  (defvar Info-dir-file-attributes)
  (defvar woman-manpath)
  (defvar Info-directory-list)

  ;; See find-file.el
  (defvar ff-search-directories)

  ;;  This is just forward declaration for byte compiler
  ;;  It it not sensible to lift `defcustom' definition apart from
  ;;  to the beginning of file due to macros and all which refer to it.
  ;;  => This is a user variable and defcustom should stay in user section.
  (defvar tinypath--verbose 3
    "*Verbosity level"))

(eval-when-compile
  (require 'advice))

;;}}}
;;{{{ Environment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      Basic Environment check and definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tinypath--win32-p
  (cond
   ((memq system-type '(ms-dos windows-nt)))  ;; Emacs
   ((fboundp 'console-type)                   ;; XEmacs
    ;; Quiet Emacs byte compiler
    (memq (funcall (symbol-function 'console-type))
	  '(win32 w32 mswindows)))
   ((boundp 'window-system)
    (memq (symbol-value 'window-system) '(win32 w32 mswindows)))
   (t
    (message "TinyPath: Internal win32-p check alert, contact maintainer.")
    nil))
  "The value is non-nil under Win32 operating system.")

(defvar tinypath--win32-cygwin-p
  (and tinypath--win32-p
       (let ((case-fold-search t))
	 (string-match "cygwin" (emacs-version))))
  "The value is non-nil if running under Win32 Cygwin Emacs.")

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-message (msg)
  "Print messages to user."
  (let ((buffer (get-buffer-create "*tinypath.el ERROR*")))
    (with-current-buffer buffer
      (goto-char (point-min))
      (insert msg) ;; Insert message first
      ;; Make a record to *Messages* buffer as well.
      (message msg)
      (unless tinypath--startup-no-messages
	(pop-to-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;; #copy from tinyliba.el
(defun tinypath-win32-p ()
  "Check if running under Win32 system."
  (cond
   ((memq system-type '(ms-dos windows-nt)))  ;; Emacs
   ((fboundp 'console-type)                   ;; XEmacs
    ;; Quiet Emacs byte compiler
    (memq (funcall (symbol-function 'console-type))
	  '(win32 w32 mswindows)))
   ((boundp 'window-system)
    (memq (symbol-value 'window-system) '(win32 w32 mswindows)))
   ((error "TinyPath: Internal win32-p check alert, contact maintainer."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment-home ()
  "Check environment: HOME."
  (when (or (not (getenv "HOME"))
	    (not (file-directory-p (getenv "HOME"))))
    (tinypath-tmp-message
     (concat
      "\
** TinyPath.el: [ERROR] HOME variable error set.

   The variable is either a) not set or b) it points to invalid directory.

   An environment variable named HOME must be set so that Emacs knows where to
   read initialization file like $HOME/.emacs. The HOME variable is crucial
   to Emacs functioning and lot of packages depend on its existence.

"
      (cond
       (tinypath--win32-p
	"")
       (t
	"\
   Hm. This error should not happen under Unix/Linux system.
   Please recheck your environment and contact your sysadm
   to determine cause of this.")
       (t
	"\
   In Windows Win95/98/NT: Add this statement to your c:\\AUTOEXEC.BAT file
   and reboot the computer.

      set HOME=C:\yourname

   The `yourname' is a directory which you must create and it should not
   contain spaces in the directory name.

   In Windows ME/2000/etc You have to use Start=> Control-Panel=> System
   icon, select `advanced' tab and button `environment' to alter the
   values. Click `apply' and `ok' to make new settings effective.\n\n")))))
  ;;  Return value from function
  (getenv "HOME"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment-user ()
  "Check environment: USER, USERNAME, LOGNAME."
  (let ((user  (getenv "USER"))
	(uname (getenv "USERNAME")) ;; W2k variable
	(log   (getenv "LOGNAME"))
	unix-fix
	win32-fix)
    ;;  In Unix, require that both LOGNAME and USER is correct
    ;;  Different shells and Unix/Linux systems do not define always
    ;;  both.
    (cond
     ((and user
	   (null log))
      ;; After this, all is ok.
      (setq unix-fix "LOGNAME")
      (setenv "LOGNAME" user))
     ((and log
	   (null user))
      (setq unix-fix "USER")
      (setenv "USER" user)))
    (when (and uname
	       (null user))
      (setq win32-fix "USER")
      (setenv "USER" user))
    ;;  Read variables again; the above may have updated something
    (setq user  (getenv "USER")
	  uname (getenv "USERNAME")
	  log   (getenv "LOGNAME"))

    (when (and unix-fix
	       tinypath--verbose-info-messages
	       (not tinypath--win32-p))
      (tinypath-tmp-message
       (format
	(concat
	 "\
** TinyPath.el: [INFO] environment variable %s was `%s'

   Hm. This error should not normally happen in Unix environment, but this
   may be a bash(1) problem, which does not define USER by default.
   Please check you environment by logging in from a fresh terminal. You
   can correct it in your shell's startup file or inform System
   Administrator of your site. Here is an example:

       $HOME/.bashrc:   export USER=$LOGNAME    # If you have $LOGNAME
       $HOME/.tcshrc:   setenv USER foo")
	unix-fix (getenv unix-fix))))
    (when (and win32-fix
	       tinypath--verbose-info-messages)
      (tinypath-tmp-message
       (format
	(concat
	 "\
** TinyPath.el: [INFO] environment variable %s set to `%s'

   In this Windows ME/NT/2000 there was variable USERNAME which was copied
   to USER. Note however, that this only sets Emacs environment, and does
   not affect outside environment, so you're adviced to define these
   variables permanetly through Start=> Control-Panel=>
   SystemIcon/Environment tab/

   If you want to set this locally to your Emacs, add following code
   to your startup file at $HOME/.emacs

      ;; \"username\" must contain no spaces. Max 8 characters
      (setenv \"USER\"  \"username\")

   In Windows Win95/98/NT: Add this statement to your c:\\AUTOEXEC.BAT file
   and reboot the computer.

      set USER=johndoe
      set LOGNAME=johndoe

   The `johndoe' is a short, usually maximum of 8 characters, which must
   not contain spaces. The value usually is the same as the HOME path's
   last directory name.

   In Windows ME/2000/etc use Start => Control-Panel => System and
   select `advanced' tab and `environment' button to alter the values.
   Fill in the values and click `ok' to activate new environment.\n\n")
	win32-fix (getenv win32-fix))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-environment ()
  "Check environment variables."
  (tinypath-install-environment-home)
  (tinypath-install-environment-user))

;;}}}

;;{{{ Load time functions and macros

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      This section must be before variable definitions.
;;      The functions must be available during the variable
;;      initializations, that's why `eval-and-compile' wrapping.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; --++-- --++-- --++-- --++-- --++-- --++-- - eval-and-compile-start --
(eval-and-compile

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-byte-compile-running-p ()
  "Return non-nil if byte compiling file."
  (or (string= (buffer-name) " *Compiler Input*")
      (string-match "batch-byte-compile"
		    (prin1-to-string command-line-args))))

;;; ----------------------------------------------------------------------
;;; Only some values are recorded as messages to the *Messages* buffer
;;; Showing the values possibly makes user think if he needs
;;; to change the defaults.
;;;
(put 'tinypath-set-default-value-macro 'lisp-indent-function 1)
(put 'tinypath-set-default-value-macro 'edebug-form-spec '(body))
(defmacro tinypath-set-default-value-macro (var &rest body)
  "Print verbose messages when activating VAR and run BODY."
  `(let (val)
     ;;  This may call several functions.
     (setq val ,@body)
     (unless (tinypath-byte-compile-running-p)
       (message "TinyPath: Default value for `%s' ... %s"
		,var
		(prin1-to-string val)))
     val))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-verbose-macro 'lisp-indent-function 1)
(defmacro tinypath-verbose-macro (level &rest body)
  "When LEVEL is =< `tinypath--verbose' run BODY."
  `(when (and (numberp tinypath--verbose)
	      (or (= ,level tinypath--verbose)
		  (< ,level tinypath--verbose)))
     ,@body
     (when (> tinypath--verbose 19)
       (tinypath-log-write))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-directory-sep-char-macro 'lisp-indent-function 0)
(defmacro tinypath-directory-sep-char-macro (&rest body)
  "Emacs and XEmacs compatibility.
In let, set `directory-sep-char' to / and run BODY."
  (let ((sym 'directory-sep-char))
    (if (and (boundp sym)
	     (or (string< emacs-version "21.1")	;; Obsolete in 21.1+
		 tinypath--xemacs-p))
	`(tinypath-with-let-variable directory-sep-char ?/
	   ,@body)
      `(progn
	 ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-expand-file-name (path)
  "Expand filenames and always use forward slashes."
  (cond
   ((and (not tinypath--win32-p)
	 ;; Nothing to do
	 (string-match "^/" path)
	 (not (string-match "\.\." path))))
   (t
    (tinypath-directory-sep-char-macro
     (setq path (expand-file-name path)))))
  (if tinypath--win32-p
      (setq path (downcase path)))
  path)

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-expand-file-name-variable-macro  'lisp-indent-function 0)
(defmacro tinypath-expand-file-name-variable-macro (var)
  "Expand list of paths stored in VAR symbol."
  `(let (list)
     (dolist (path ,var)
;;;        (push (tinypath-expand-file-name path) list))
       (setq list (cons path list)))
     (setq ,var (nreverse list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-message-bug (bug &optional die)
  "Tell how to report BUG (string) and optionally DIE."
  (let ((msg
	 (substitute-command-keys
	  (concat
	   (format
	    "TinyPath: [ERROR] report bug with name [%s]"
	    bug)
	   "See also \\[tinypath-version]"))))
    (if die
	(error msg)
      (message msg)
      (sit-for 5))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-up (dir)
  "Return precious DIR."
  (setq dir (file-name-as-directory dir)) ;; Ensure trailing slash
  (when (stringp dir)
    (file-name-directory
     ;; Delete trailing slash
     (substring dir
		0
		(1- (length dir))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-subdirs (dir)
  "Return directories under DIR."
  (let (list)
    (when (file-directory-p dir)
      (dolist (elt (directory-files dir 'full))
	(if (file-directory-p elt)
;;;            (push elt list)
	    (setq list (cons elt list)))))
    list))

;;; ----------------------------------------------------------------------
;;; #copy: tinyliba.el
;;;
(defun tinypath-ti::win32-cygwin-p (&optional use-cache)
  "Return root if path to cygwin1.dll is found from `exec-path'.
If USE-CACHE is non-nil, retrieve cached value."
  (let (ret)
    (cond
     ((and use-cache
	   (get 'tinypath-ti::win32-cygwin-p 'cache-set))
      (setq ret (get 'tinypath-ti::win32-cygwin-p 'cache-value)))
     (t
      (put 'tinypath-ti::win32-cygwin-p 'cache-set t)
      (dolist (path exec-path)
	(when (and (stringp path)
		   (file-exists-p
		    (concat
		     (file-name-as-directory path) "cygwin1.dll"))
		   (file-exists-p
		    (concat
		     (file-name-as-directory path) "cygpath.exe")))
	  ;;  The root directory is one DIR up from bin/cygwin1.dll
	  ;;
	  ;;  1) Drop the trailing slash  ../bin
	  ;;  2) Give one directory up    ..
	  ;;
	  ;;  We have to leave trailing slash, because the resulting
	  ;;  directory may be in the worst case C:/
	  ;;  (which is NOT recommended place for cygwin install)
	  ;;
	  (when (string-match "^\\(.*\\)[/\\]" path)
	    (setq path
		  (match-string 1 path))
	    (setq ret path)
	    ;;  This is native Cygwin Emacs, not a Win32 version
	    ;;  if path is empty: /bin => one up => ''
	    (when (string= ret "")
	      (setq ret "/"))
	    (put 'tinypath-ti::win32-cygwin-p 'cache-value ret)
	    (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;; Earlier XEmacs and Emacs `executable-find' functions are buggy
;;; and do not find binaries correctly, so we use our own implemantation.
;;;
(defun tinypath-executable-find (file)
  "Find FILE along path. FILE must be absolute name with possible .exe
Emacs `executable-find' tries various suffixes in Win32, but this
function just looks if FILE exists along load path."
  (let (ret name)
    (dolist (path exec-path)
      (setq name (concat (file-name-as-directory path) file))
      (when (and (not (file-directory-p name))
		 (file-exists-p name))
	(setq ret (tinypath-expand-file-name name))
	(return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-executable-find-binary (file)
  "Try finding binary: FILE or FILE.exe in win32."
  (if tinypath--win32-p
      (tinypath-executable-find (concat file ".exe"))
    (tinypath-executable-find file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-versions (&optional noerr cache)
  "Return possible version numbers for current Emacs. NOERR.
If CACHE is set, use cached value."
  (interactive)
  (if (and cache
	   (get 'tinypath-emacs-versions 'version))
      (get 'tinypath-emacs-versions 'version)
    (let* ((str (emacs-version))
	   ;;   XEmacs beta has spaces in this variable. Just take
	   ;;   the first word from it. There must be no spaces
	   ;;   in filename returned from this function
	   ;;
	   ;;   emacs-version: "21.2  (beta19) \"Shinjuku\" XEmacs Lucid"
	   (patch          (progn
			     (cond
			      ((string-match "patch \\([0-9]+\\)" str)
			       (match-string 1 str))
			      ;;  XEmacs 21.1  (beta23)
			      ((string-match "(beta\\([0-9]+\\))" str)
			       (match-string 1 str)))))
	   (major-version-x-x  (progn
				 (string-match "[0-9]+\\.[.0-9]" str)
				 (match-string 0 str)))
	   (major-version  (progn
			     (string-match "[0-9]+\\.[.0-9]+" str)
			     (match-string 0 str)))
	   (version        (concat major-version ;; 20.6.1
				   (if patch
				       (concat "." patch)
				     "")))
	   ret)
      (dolist (ver (list  version  major-version major-version-x-x))
	(when ver
	  (pushnew ver ret :test 'string=)))
      (when ret
	(put 'tinypath-emacs-versions 'version ret))
      (or ret
	  (and (null noerr)
	       (tinypath-message-bug "Can't parse `emacs-version'."))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-root-by-other-methods ()
  "Return ROOT of emacs installation directory."
  (let* ((sym  'invocation-directory)
	 ;;  Use `symbol-value' to compile cleanly in all
	 ;;  Emacs and XEmacs versions. It just hides the variable form
	 ;;  Byte compiler
	 (val  (if (and (boundp sym)
			(stringp (symbol-value sym)))
		   (symbol-value sym)))
	 (dir  (and val
		    (file-directory-p val)
		    (file-name-as-directory val))))
    (when dir
      (tinypath-directory-up dir))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-core-path-p (path &optional version)
  "Test if PATH is core Emacs path. VERSION number can be found from path."
  ;;  PATH name must contain version for this emacs and subdirectory "lisp"
  (and (if version
	   (string-match (regexp-quote version) path)
	 t)
       ;; /usr/local/share/emacs/20.7/site-lisp
       (string-match "[/\\]lisp" path)
       (string-match (concat
		      ;;  Win32 installs emacs-20.4
		      "^.*emacs-[0-9]+\\.+[0-9.-]+"
		      ;;  Unix installs emacs/20.4
		      "\\|^.*emacs[/\\][0-9]+\\.+[0-9.-]+")
		     path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-root-by-load-path ()
  "Return ROOT of emacs installation directory by reading `load-path'.
Return:

   '(matched-part original-path)."
  (let ((ver (car-safe (tinypath-emacs-versions 'noerr 'cache)))
	ret)
    (if (null ver)
	(tinypath-message-bug "root-by-load-path")
      (dolist (path load-path)
	(when (and (stringp path)
		   (tinypath-emacs-core-path-p path ver))
	  (return
	   (setq ret (list
		      (match-string 0 path)
		      path))))))
    (unless ret
      ;; User has wiped the load-path information by accident,
      ;; Try doing something about it.
      ;;
      ;; #todo: Should we restore part of the path from $EMACSLOADPATH ?
      ;; --> I'm afraid not many set the variable at all
      (let ((path (tinypath-emacs-root-by-other-methods)))
	(if path
	    (setq ret (list path path)))))
    (tinypath-verbose-macro 7
      (message "TinyPath: EMACS ROOT %s" (or (car-safe ret) "<nil>")))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-emacs-root-directory ()
  "Return Emacs installation root directory."
  (cond
   ((and invocation-directory
	 ;;  In Unix this is /usr/local/bin  which is NOT the
	 ;;  Emacs installatio place.
	 ;;
	 ;;  In Win32 this is c:/.....emacs-21.3/bin/ which
	 ;;  can be used
	 (file-directory-p (concat invocation-directory "../lisp")))
    (tinypath-expand-file-name
     (concat invocation-directory "../lisp")))
   (t
    (car-safe (tinypath-emacs-root-by-load-path)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-string-match (regexp)
  "Check if REGEXP is found form load path. Return first match."
  (dolist (path load-path)
    (when (and (stringp path)
	       (string-match regexp path))
      (return path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-add-subdirs (root &optional verbose)
  "Add all subdirectories of ROOT to `load-path' with VERBOSE message level.
ROOT can be a single directory or list of directories."
  (cond
   ((stringp root)
    (setq root (list root)))
   ((listp root)
    nil)
   (t
    (error "Incorrect ROOT parameter value: %s" root)))
  (dolist (dir root)
    (dolist (subdir (tinypath-directory-subdirs dir))
      ;;  Convert forward and backward slashes.
      (setq subdir
	    (tinypath-expand-file-name subdir))
      (unless (string-match tinypath--boot-ignore-directory-regexp subdir)
	(tinypath-verbose-macro (or verbose 8)
	  (message "TinyPath: add subdir %s" subdir))
	(pushnew subdir load-path :test 'string=)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-default-load-path-root-user ()
  "Return user's Emacs Lisp path by guessing various directories."
  (flet ((msg (m)
	      (message m)
	      (unless tinypath--startup-no-messages
		(sit-for 2))
	      nil))
    (if (null (getenv "HOME"))
	(msg "TinyPath: [ERROR] Environment variable HOME is not set.")
      (let (ret)
	(dolist (dir (list
		      (if tinypath--xemacs-p
			  "~/.xemacs.d")
		      (if tinypath--xemacs-p
			  "~/.xemacs")
		      "~/.emacs.d" ;; New Emacs
		      "~/elisp"
		      "~/lisp"
		      "~/.elisp"
		      "~/.lisp"
		      "~/.emacs"))
	  (when (and (stringp dir)
		     (file-directory-p dir))
	    (setq ret dir)))
	(unless ret
	  ;;  Try to scan all of home for lisp. Hm, Ugh.
	  ;;  Perhaps a user who starts Emacs for the first time, or
	  ;;  a Windows, where HOME is not set.
	  (tinypath-verbose-macro
	      3
	    (msg (format
		  (concat "TinyPath: [WARN] Can't determine personal "
			  "lisp package directory. $HOME/elisp was expected. "
			  "This is probably harmless; "
			  "see variable tinypath--load-path-root for more."
			  "Environment variable HOME is [%]")
		  (or (getenv "HOME")
		      "<not set>")))))
	ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-default-load-path-root-dirs ()
  "Find default directories for `tinypath--load-path-root'."
  (let (list)
    (dolist (dir
	     (list
	      (tinypath-default-load-path-root-user)

	      ;;  site wide configuration
	      ;;  #todo: where is XEmacs installed by default?
	      (if (not tinypath--xemacs-p)
		  (concat
		   "/usr/local/share/emacs/"
		   (if (string-match "[0-9]+\\.[0-9]+" emacs-version)
		       (match-string 0 emacs-version)
		     "")
		   "/lisp"))
	      ;; Cygwin
	      "/var/share/site-lisp"
	      ;; Debian
	      "/usr/local/lib/emacs/site-lisp"
	      "/usr/local/share/emacs/site-lisp"
	      "/usr/local/share/site-lisp"
	      "/opt/share/site-lisp"
	      "/opt/local/share/site-lisp"
	      "/opt/local/share/emacs/site-lisp"))
      (when (stringp dir)
	(message "TinyPath: default tinypath--load-path-root => %s %s"
		 dir
		 (if (file-directory-p dir)
		     "OK"
		   "NOT EXIST"))
	(if (file-directory-p dir)
	    (push dir list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-directory-search (dir list &optional verb bug)
  "Search DIR in the hierarchy of directories upward.

Input:

  DIR       Directory to search. This can be nil.

  LIST      List of possible search directories.
	    -- A simple string means absolute location/DIR
	    -- Directory enclosed in (dir count) means that the directory is
	       also searched `count' levels upward.
	    -- Directory enclosed in (dir 'abs) means absolute location
	       without using parameter DIR.

	    For example with value:

	    '(/dir1 (/some/more/of/dir2 2) (/this/location abs)  /dir3 ...)

	    The choices searched are:

	    /dir1/DIR
	    /some/more/of/dir2/DIR
	    /some/more/of/DIR
	    /this/location
	    /dir3/DIR

  VERB     Verbose messages.
  BUG      If set, and DIR not found, call `tinypath-message-bug'."
  (let (found)
    (flet ((check-dir
	    (try dir)
	    (setq try (tinypath-expand-file-name
		       (concat (file-name-as-directory try)
			       dir)))
	    (if verb
		(message "TinyPath: directory search ... %s" try))
	    (when (file-directory-p try)
	      (if verb
		  (message "TinyPath: directory search ... found %s" try))
	      try)))
      (or dir
	  (setq dir ""))
      (dolist (try list)
	(cond
	 ((stringp try)
	  (if (setq found (check-dir try dir))
	      (return)))
	 ((listp try)
	  (multiple-value-bind (path count) try
	    (cond
	     ((and (stringp path)
		   (eq count 'abs))
	      (if (setq found (check-dir path dir))
		  (return)))
	     ((and (stringp path)
		   (integerp count))
	      (while (and (stringp path)
			  (not (zerop count))
			  (> count 0))
		(if (setq found (check-dir path dir))
		    (return))
		(decf count)
		(setq path
		      (tinypath-directory-up path)))))))))

      (cond
       (found ;;#todo: anything to do here?
	t)
       (t
	;;  Hope people that have it in non-standard locations
	;;  will tell it to maintainer.
	(when (and verb bug)
	  (message "TinyPath: [WARNING] %s not found." dir)
	  (tinypath-message-bug
	   (format "Directory lookup fail %s" dir)))))
      found)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-initial-value-xemacs (root &optional force)
  "Add XEmacs installation lisp directories to `load-path'.

Input:

  ROOT      XEmacs installation root directory.
	    See function `tinypath-emacs-root-directory'.

  FORCE     Try to locate xemacs-packages even if that directory is
	    found from `load-path'. The force option unconditionally
	    adds all found directories to `load-path'. No duplicates
	    are added though. This option is able to fix broken
	    `load-path'."
  ;;  Latest XEmacs does not include all of its packages in the
  ;;  standard installation, but in a huge archive called "SUMO", which
  ;;  contains subdirectory "xemacs-packages".
  ;;
  ;;  We have no way of knowing where that directory has been unpacked, but
  ;;  try few guesses anyway.
  (when (and tinypath--xemacs-p
	     (boundp 'emacs-major-version)
	     ;;  The `symbol-value' is just a byte compiler silencer
	     ;;  after the above `boundp' test.
	     (> (symbol-value 'emacs-major-version) 20)
	     (or force
		 (null (tinypath-load-path-string-match
			"xemacs-packages"))))
    (message "TinyPath: load-path auto-boot [XEmacs] ...")
    (let (found
	  xemacs-packages)
      ;;  Search under standard location
      ;;  <XEmacs-root>/xemacs-packages  or
      ;;  XEmacs/XEmacs-21.2/xemacs-packages
      (dolist (lisp '("xemacs-packages"
		      "mule-packages"
		      "site-packages"))
	(setq lisp (concat lisp "/lisp"))
	(when (setq found
		    (tinypath-directory-search
		     lisp
		     (list (list root 3))
		     'verb
		     'bug))
	  (if (string= lisp "xemacs-packages/lisp")
	      (setq xemacs-packages found))
	  (tinypath-load-path-add-subdirs found)))
      ;; Still not found? Try few more alternatives. This time
      ;; we only try to find the "xemacs-packages"
      (unless xemacs-packages
	(when (setq found
		    (tinypath-directory-search
		     "xemacs-packages/lisp"
		     (list
		      ;;  The first is historical location
		      ;;  of a vanilla-configured XEmacs
		      '("/usr/local/lib/xemacs" abs)
		      ;;  Try more guesses
		      '("/usr/share/lib/xemacs" abs)
		      '("/usr/lib/xemacs" abs)
		      '("~/.xemacs-packages/lisp" abs)
		      '("~/.xemacs")
		      '("~" abs)
		      '("~/site-lisp" abs)
		      '("~/lisp")
		      '("~/elisp"))
		     'verb
		     'bug))
	  (tinypath-load-path-add-subdirs found)))
      (message "TinyPath: load-path auto-boot [XEmacs]... done."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-initial-value (&optional dir-list)
  "Add Emacs installation lisp directories to `load-path'.
This is solely used for booting up tinypath.el package, so that
`require' commands can be satisfied. Without the core packages available
in `load-path' it is not possible to use Emacs.

The DIR-LIST is location of additional directories to consider as
Emacs core-lisp installation directories."
  (let* ((root-base (tinypath-emacs-root-directory))
	 (dir-p     (and root-base
			 (file-directory-p root-base)))
	 root)
    (message "TinyPath: load-path auto-boot (Emacs install dir)... %s %s"
	     (if root-base
		 root-base
	       "[can't find Emacs install root]")
	     (if dir-p
		 "(dir nok)"
	       "(dir ok)"))
    (when (and root-base
	       dir-p)
      ;;  Why this booting is even needed? Isn't `load-path' already
      ;;  set, when Emacs starts? Not quite. Emacs does not include term/
      ;;  directory in `load-path', because it has peculiar way of
      ;;  requiring (load "term/vt100"). This boot section will ensure
      ;;  that all paths are included in `load-path'.
      ;;
      (message "TinyPath: load-path auto-boot [running]")
      (setq root-base (file-name-as-directory root-base))
      ;;
      ;;  Make ROOT/lisp directory. This is the same for all
      ;;  Emacs versions. Win32 conversion to lowercase
      ;;
      (setq root (tinypath-expand-file-name (concat root-base "lisp")))
      ;;
      ;; This is just ultimate safeguard. We did find the
      ;; root, but that doesn't mean it is included in the `load-path'
      ;; E.g. there may be directories /ROOT/lisp/something
      ;;
      ;; It is still possible that member fails, because
      ;;
      ;; - Win32 can have mixed case paths, C:/ and c:/ are
      ;;   different to pushnew
      ;; - Win32 slashes c:\ c:/ confuse pushnew.
      ;;
      ;; These will be handled in the final install phase,
      ;; see function `tinypath-load-path-clean'
      ;;
      (unless (or (member root load-path)
		  (member (file-name-as-directory root) load-path))
	(pushnew root load-path :test 'string=)
	(message "TinyPath: load-path auto-boot [%s added]." root))
      ;;
      ;;  We might have included this line inside the above `unless',
      ;;  after `pushnew' but we do not do that. It's not a guarantee
      ;;  that subdirectories are there if ROOT was there.
      ;;
      (message "TinyPath: booting standard Emacs lisp paths.")
      (tinypath-load-path-add-subdirs root 2)
      (tinypath-load-path-initial-value-xemacs root-base)
      ;;  Add user supplied additional paths.
      (when dir-list
	(message "TinyPath: booting user supplied lisp paths.")
	(tinypath-load-path-add-subdirs dir-list))
      (message "TinyPath: load-path auto-boot... done"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-find-root-home ()
  "Return suitable root user HOME directory. /home/root etc."
  (let (ret)
    (dolist (path (list
		   (if (and (not tinypath--win32-p)
			    (eq (user-uid) 0))
		       (getenv "HOME"))
		   "/home/root"
		   "/users/root"
		   "/root"
		   "/"))
      (when (and (stringp path)
		 (file-directory-p path))
	(message "TinyPath: tinypath-tmp-find-root-home [%s]" path)
	(setq ret path)
	(return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-tmp-find-writable-dir (&optional file)
  "Find writable directory and append FILE to it. Only used at startup.
This function sets initial values for variable
`tinypath--cache-file-prefix'.

User should `setq' this variable before calling tinypath.el

References:

  `tinypath--cache-file-prefix'
  `tinypath--load-path-dump-file'"
  (let ((root-home   (tinypath-tmp-find-root-home))
	(root-user-p (and (not tinypath--win32-p)
			  (eq (user-uid) 0)))
	(user        (or (getenv "USER")
			 (getenv "LOGNAME")
			 (let ((sym 'user-login-name))
			   (if (boundp sym) ;; Not in XEmacs 21.4
			       (symbol-value sym)))
			 (let ((home (expand-file-name "~")))
			   (if (string-match "\\([^/\\]+\\)$" home)
			       (match-string 1 home)))
			 ""))
	ret)
    (when (and (not (file-directory-p "~/tmp"))
	       (not (file-directory-p "c:/"))) ;; Non-Win32 system
      (message "TinyPath: [WARNING] Cannot find $HOME/tmp directory."))
    (dolist (dir '("~/.emacs.d/config/"
		   "~/elisp/config/"
		   "~/elisp/conf/"
		   "~/lisp/config/"
		   "~/lisp/conf/"
		   "~/.xemacs/config/"
		   "~/tmp/"
		   "~"
		   "/tmp/"
		   "/var/tmp/"
		   "c:/temp/"
		   "c:/tmp/"
		   "c:/"))
      ;; The ROOT user is special case. (expand-file-name "~")
      ;; may return plain "/".
      ;; check if SysAdm has created
      ;; /home/root, /users/root etc. directory.
      (cond
       ((and root-user-p
	     (string-match "~" dir))
	(setq dir
	      (if (string= root-home "/")
		  ;; ~  =>  ""
		  (replace-match "" nil nil dir)
		;; ~/tmp =>  /home/root/tmp
		(replace-match root-home nil nil dir))))
       (t
	(setq dir (file-name-as-directory
		   (expand-file-name dir)))))
      (when (and (file-directory-p dir)
		 (file-writable-p
		  (concat dir
			  (or file "###tinypath.el-test###"))))
	;; In multi-user environment, we must say /tmp/-USER-file
	(when (string= dir "/tmp/")
	  (setq dir (concat dir "-" user "-" )))
	(setq ret (concat dir (or file "")))
	(return)))
    ;;  Last thing to do. If User has set his HOME to point to
    ;;  C:/, that is not a good idea. Move cache file under C:/TEMP
    (when (and (string-match "^[Cc]:[/\\]?$" ret)
	       (file-directory-p "C:/temp"))
      (message
       "TinyPath: [WARNING] find-writable-dir Using c:/temp instead of c:/")
      (setq ret "c:/temp"))
    (if ret
	ret
      (error "TinyPath: Can't find writable directory for %s" file))))

) ;; --++-- --++-- --++-- --++-- --++-- --++-- -- eval-and-compile-end +--

;;}}}
;;{{{ variables

;;; ......................................................... &v-hooks ...

(defcustom tinypath--load-hook '(tinypath-install)
  "*Hook run when package is loaded.
Please make sure that this hook contains function `tinypath-install'
or nothing will be set up to Emacs when you load tinypath.el.

Other suggested function could be put to this hook:
  `tinypath-exec-path-check-verbose-fix'
  `tinypath-install-timer'."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath--load-path-function 'tinypath-load-path-setup
  "*Function define all additional paths to the `load-path'."
  :type  'function
  :group 'TinyPath)

(defcustom tinypath--report-mode-define-keys-hook
  '(tinypath-report-mode-default-bindings)
  "*List of functions to run which define keys to `tinydesk-mode-map'."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath--report-mode-hook nil
  "*Hook run after the `tinypath-report-mode' is turned on."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath--cache-duplicate-report-hook nil
  "*Hook run after the `tinypath-cache-duplicate-report' function.
The point is at the beginning of `tinypath--report-buffer' when
the hook is run."
  :type  'hook
  :group 'TinyPath)

(defcustom tinypath--load-path-ignore-regexp-hook  nil
  "*Hook run after the `tinypath--load-path-ignore-regexp' is defined.
You can use this to add more ignore regexps to the default value.
See Manual for the details M-x tinypath-version and \"Gnus\"."
  :type  'hook
  :group 'TinyPath)

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinypath--load-path-accept-criteria t
  "*Control which incarnation of the installed package is respected.
When Emacs is installed, it contains many packages that may be
maintained out of Emacs core. (e.g. in CVS)
You may find or install more up to date version from developer's site.

Example: cperl-mode.el

  Take for example cperl-mode.el which is avalable at
  http://cpan.perl.org/modules/by-authors/Ilya_Zakharevich/cperl-mode/

  The package is installed in Emacs kit at location:

      <root>/emacs-20.7/lisp/progmodes/cperl-mode.el

  For ystem wide installation, more up to date package could
  be found at:

      /usr/local/share/site-lisp/net/users/zakharevich-ilya/cperl-mode.el

  and private user may keep the package in

     ~/elisp/cperl-mode.el

Which package loads?

  nil           First one that is in `load-path', when the cache was built.
		See `tinypah-cache-problem-report'.

  t             Choose package under $HOME, or one at site wide or
		one in the default installation.

  function      If this is a callable function, pass LIST of paths
		to it to choose the correct package. Function must
		return string PATH or nil."
  :type '(choice (const nil)
		 (const t))
  :group 'TinyPath)

(defcustom tinypath--compression-support nil
  "*Type of compression support: 'default, 'all or 'none.

'default

    Files ending to .gz and .bz2 files are counted in when
    a load command is issued.

'all

    In addition to 'default, also autoloaded functions can be found from
    compressed files. This means that statements like these will work:

    (autoload 'jka-compr \"jka-compr\")

    The recommendation is that you set this value to 'all if you keep your lisp
    files in compressed format to save space.

nil

    Do not use compression support. Seach only .el and .elc files.
    This is the recommended setting in case there is no need for
    compressed files. It will speed searching considerably.

    Variable `tinypath--compressed-file-extensions' is not used.

'none

    Do not use cache at all. Use this if the cache is broken. In Total
    emergency, call M-x -1 `tinypath-cache-mode' to disable all advises.

This value must be set once, before package is loaded. Changing it afterwards
has no effect."
  :type '(choice (const default)
		 (const all)
		 (const none))
  :group 'TinyPath)

(when (and (boundp 'command-line-args)
	   (member "-debug-init" (symbol-value 'command-line-args)))
  (put 'tinypath--verbose 'debug-init tinypath--verbose)
  (message "tinypath: VERBOSE 10; Emacs option was -debug-init")
  (setq tinypath--verbose 10))

(defcustom tinypath--cache-expiry-days
  (tinypath-set-default-value-macro
   "tinypath--cache-expiry-days"
   14)
  "*How many days until expiring `load-path' cache and rescan paths.
If set to nil; do not use cache feature, but scan directories at startup."
  :type 'integer
  :group 'TinyPath)

(defcustom tinypath--report-mode-name "TinyPathReport"
  "*The name of the `tinypath-report-mode'."
  :type  'string
  :group 'TinyPath)

(defcustom tinypath--verbose
  (tinypath-set-default-value-macro
   "tinypath--verbose"
   3)
  "*If number, bigger than zero, let user know what's happening.
In error situations you can look old messages from *Messages* buffer.
If you want all messages, set value to 10.

If you want killer-logging, select 20. All this will also save
everything to `tinypath--log-file'."
  :type  '(integer :tag "Verbose level 0 ... 10")
  :group 'TinyPath)

(defcustom tinypath--verbose-timing
  (tinypath-set-default-value-macro
   "tinypath--verbose-timing"
   nil)
  "*If non-nil, dispaly laod time of each `load' `load-library' `require' call.
This variable is obsolete and not used."
    :type  'integer
    :group 'TinyPath)

(eval-and-compile

  (defun tinypath-cygwin-p ()
    "Return Cygwin installation root if Cygwin is along PATH."
    (let ((cygwin-p
	   (cond
	    ((locate-library "executable-find")
	     (autoload 'executable-find "executable-find")
	     ;;  Should be in /bin/cygrunsrv.exe
	     ;;  The funcall just hides this from idiot byte compiler
	     ;;  Which doesn't see autoload definition.
	     (funcall (symbol-function 'executable-find) "cygrunsrv"))
	    ((let (file)
	       (dolist (dir exec-path)
		 (setq file
		       (concat (file-name-as-directory dir)
			       "cygrunsrv.exe"))
		 (if (file-exists-p file)
		     (return file))))))))
      (when cygwin-p
	;;  X:/SOME/PREFIX/bin/cygrunsrv.exe => X:/SOME/PREFIX/
	(when (string-match "^\\(.*\\)/[^/]+/" cygwin-p)
	  (match-string 1 cygwin-p)))))

  (defun tinypath-info-default-path-list ()
    "Return default Info path candidate list."
    (let ((cygwin-p (tinypath-cygwin-p))
	  (list
	   '("/usr/info"
	     "/usr/local/info"
	     "/usr/info/"
	     "/doc/info"
	     "/usr/share/info"
	     "/usr/local/share/info"
	     "/opt/info"
	     "/opt/share/info"))
	  ret)
      ;;  Add more default info paths to search
      (when cygwin-p
	(dolist (elt '("usr/info"  "usr/local/info"))
	  (push (concat (file-name-as-directory cygwin-p)  elt) list)))
      ;;  Drop non-existing directories
      (dolist (elt list)
	(when (file-directory-p elt)
	  (push elt ret)))
      ret))

  (defcustom tinypath--Info-default-directory-list
    (tinypath-info-default-path-list)
    "*Additional INFO directories to check for inclusion.
Any new entries in these directories are checked and
fixed and added to `Info-default-directory-list'."
    :type '(list directory)
    :group 'TinyPath)) ;; eval-and-compile end

(message "TinyPath: [VAR] tinypath--Info-default-directory-list %s"
	 (prin1-to-string tinypath--Info-default-directory-list))

;;  We can't use `ti::package-config-file-prefix' NOW, because the tinylibm.el
;;  is not yet loaded - `load-path' is not yet know for sure.
;;
;;  #todo: this is hard coded location. If Emacs ever defines similar function
;;  #todo: then we can start using it to put config files to common place.

(defcustom tinypath--compressed-file-extensions
  (delq
   nil
   (cond
    (tinypath--win32-cygwin-p
     ;;  We know that Cygwin contains programs for these
     '(".gz" ".bz2"))
    (t
     (list
      ;;  The order is important. Put most likely first
      (if (tinypath-executable-find-binary "bzip2")    ".bz2")
      (if (tinypath-executable-find-binary "gzip")     ".gz")))))
  ;;  2003-05-18 commented out. the "Z" compression is way too obsolete
  ;;  it is also faster to check only 2 extensions
  ;; (if (tinypath-executable-find-binary "compress") ".Z")))
  "*List of supported compressed file extensions.
The default list is built dynamically by checking the binary in `exec-path'.
The default list is:

\(setq tinypath--compressed-file-extensions '( \".gz\" \".bz2\"))

References:
  `tinypath--compression-support'."
  :type  '(list  string)
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath--compressed-file-extensions %s"
	 (prin1-to-string tinypath--compressed-file-extensions))

(defcustom tinypath--cache-file-prefix
  ;;
  ;; Can't use `ti::package-config-file-prefix', because the library
  ;; is not loaded yet. USER MUST SETQ THIS VARIABLE
  ;;
  (tinypath-set-default-value-macro
   "tinypath--cache-file-prefix"
   (tinypath-tmp-find-writable-dir "emacs-config-tinypath-cache"))
  "*File where to store `tinypath--cache'. See `tinypath--cache-file-postfix'.
This is only a prefix for filename. The whole filename is returned by
function `tinypath-cache-file-name' which appends emacs version id after
this prefix string.

An example:  /home/some/elisp/config/tinypah-cache-"
  :type  'string
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath--cache-file-prefix %s"
	 (prin1-to-string tinypath--cache-file-prefix))

(defcustom tinypath--cache-file-hostname-function
  'tinypath-cache-file-hostname
  "*Function to return HOST for the cache file name.

You're interested on this variable only if you're running several networked
machines and 1) you always have same, ONE mounted $HOME directory 2) and
each machine has its own run-files, like site-lisp.

Use value nil to disable using hostname in cache file name:

  (setq tinypath--cache-file-hostname-function nil)

To activate the hostname portion in cache name, set variable to like this:
This makes each HOST have its own cache.

  (setq tinypath--cache-file-hostname-function 'tinypath-cache-file-hostname)

See manual \\[tinypath-version] for more information."
  :type  'function
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath--cache-file-hostname-function %s"
	 (prin1-to-string tinypath--cache-file-hostname-function))

;;  We select the compressed file to save space if we can detect gzip
;;  in this environment.

(defcustom tinypath--cache-file-postfix
  (if t
      ".elc"
    ;; 2000-01 Disabled for now
    (if (tinypath-executable-find-binary "gzip")
	".el.gz"
      ".el"))
  "*Extension for `tinypath--cache'. See also `tinypath--cache-file-prefix'.
The xtension may be compiled version \".elc\" or non-compiled \".el\".
Even with compiled version, the .el file is also retained, because it's
the only readable file and in emergencies you can fix it and load it by hand.

You could also set this to \".el.gz\" if space is crucial, but that makes
startup lot slower. This is be´cause package must arrange loading jka-compr.el
before anything else and the load time will increase with compression.

Do not st this to \".elc.gz\", it's not supported."
  :type  'string
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath--cache-file-postfix %s"
	 (prin1-to-string tinypath--cache-file-postfix))

(defcustom tinypath--load-path-dump-file
  ;;
  ;; Can't use `ti::package-config-file-prefix', because the library
  ;; is not loaded yet. USER MUST SETQ THIS VARIABLE
  ;;
  (tinypath-tmp-find-writable-dir "emacs-config-tinypath-dump.el")
  "*Where to store dumped load path. See `tinypath-load-path-dump'."
  :type  'file
  :group 'TinyPath)

(defcustom tinypath--cache-duplicate-report-ignore-functions
  '(tinypath-cache-duplicate-report-ignore-function)
  "*Functions called with FILE. Return t to ignore FILE in duplicate report.
Called from function `tinypath-cache-duplicate-report'."
  :type  'function
  :group 'TinyPath)

(message
 "TinyPath: [VAR] tinypath--cache-duplicate-report-ignore-functions %s"
 (prin1-to-string
  tinypath--cache-duplicate-report-ignore-functions))

(defcustom tinypath--ignore-file-regexp nil
  "*Prohibit loading lisp file if regexp matches absolute path.
If \"\\\\.elc\" ignore all compiled files and load only source files.

This regexp is matched against absolute filename being loaded and
if it matches, the file is ignore. An error is signaled
if there is no single choice available after exclude.

There may be reasons why you would always load only the non-compiled
version and ignore compiled versions:

--  You are developing packages or debugging packages and you
    want your Emacs to load only non-compiled versions. The *Backtrace*
    buffer output is more sensible with non-compiled functions.

    ==> Setting value to \".\" will ignore all compiled files.

--  You have share some site-lisp files with Emacs and XEmacs, but
    you primarily use GNU Emacs and the compiled files are for it.
    XEmacs must not load the compiled versions.

    ==> Set this regexp in your $HOME/.emacs when XEmacs is loaded, to
    match the directory part of file which is located in shared lisp
    directory for Emacs and Xemacs."
  :type  'regexp
  :group 'TinyPath)

(defcustom tinypath--manpath-ignore-regexp
  "terminfo"
  "*Regexp to exclude directories for MANPATH additions.
It really isn't very serious if MANPATH contains few faulty directories,
do don't worry. You can see the final results in `tinypath--extra-manpath'."
  :type  'regexp
  :group 'TinyPath)

(defcustom tinypath--exec-path-ignore-regexp nil
  "*Regexp to exclude directories for `exec-path' additions.
The automatic Perl utility will find every directory under
`tinypath--extra-path-root' which contain executable files and them to
`exec-path. Set this variable to ignore certain directories."
  :type  'regexp
  :group 'TinyPath)

(defcustom tinypath--load-path-ignore-regexp
  (concat
   "[/\\]"     ;; windows or unix dir separator start
   "\\("       ;; START grouping
   ;;   Skip Distributed help files
   "tex\\(i\\|info\\)$"
   "\\|doc[/\\]"
   ;;   Skip Other directories
   "\\|RCS[/\\]\\|CVS[/\\]\\|zip\\|\\.svn\\|/MT/"
   ;;   Skip Perl or other build directories
   "\\|\\.\\(cpan\\|build\\|s?inst\\)"
   ;;   Skip temporary directories /T/ /t/ /tmp* /temp*
   "\\|[Tt][/\\]\\|te?mp"
   ;;   Skip build directories
   "\\|\\.\\(build\\|s?inst\\)"
   (if (and (not tinypath--xemacs-p)
	    (not (string< emacs-version "21"))) ;; > 21
       "\\|psgml"
     "")
   (if (and (not tinypath--xemacs-p)
	    (not (string< emacs-version "21"))) ;; > 21
       "\\|pcl-cvs"                   ;Emacs 21.2 - under name pcvs.el
     "")
   (if (and (not tinypath--xemacs-p)
	    (not (string< emacs-version "21")))
       "\\|artist-[0-9.]+"              ;artist is in Emacs 21.2
     "")
   (if tinypath--xemacs-p               ;EFS doesn't work in Emacs
       ""
     "\\|efs")
   ;;  20.x has custom lib, so we don't want to install private
   ;;  custom.el copy that we used for 19.x Emacs
   ;; (if (> emacs-major-version 19) "\\|custom" "")
   ;;  Do not use TM in latest Emacs. Gnus and VM has MIME handling.
   ;;  SEMI might be ok.
   ;; (if (> emacs-major-version 19) "\\|tm/\\|tm-[^/]+" "")
   "\\)")
  "*Regexp to match directories which to ignore. Case sensitive.
If `tinypath--load-path-ignore-regexp-extra' is string, it is appended ONCE
to this default regexp.

This variable is case sensitive."
  :type  '(string :tag "Regexp")
  :group 'TinyPath)

(eval-and-compile
  (defvar tinypath--install-flag t
    "If non-nil, install package.
Should only be used in cases of maintenance and debug.
To start debugging the package, set this variable nil before loading. Nothing
is done until function `tinypath-install-main' is called.

    (defun my-tinypath-debug-prepare ()
      (require 'elp)
      (require 'edebug)
      (setq debug-on-error t)
      (setq debug-ignored-errors nil)
      (setq tinypath--install-flag nil)
      (setq tinypath--cache-file-postfix \".elc\")
      (setq tinypath--load-hook nil)
      (setq tinypath--verbose 5)
      (setq tinypath--load-path-root)))
	  '(
	    ;; \"~/elisp\"   ;; Commented out while debugging
	       ;;  Run statements one by one with C-x C-e
    (my-tinypath-debug-prepare)
    (load \"/path/t/tinypath\")
    ;;  <at this point, you could instrument tinypath functions using elp>
    (tinypath-load-path-initial-value
     tinypath--core-emacs-load-path-list)
    (tinypath-install-main)
    ;;  Do something and then call this:
    (tinypath-install)

The above is just an example how to prepare to debug package."))

(defvar tinypath--load-path-ignore-regexp-extra nil
  "*String to add to `tinypath--load-path-ignore-regexp'.
Remember to start the regexp with OR-statement \\\\| because the regexp
is added to existing value.

Value of this regexp is added every time the file is loaded.
See Manual for explanation: M-x tinypath-version and \"Gnus\".")

;; Append to default value. This is the easiest this way.

(when (and (stringp tinypath--load-path-ignore-regexp)
	   (stringp tinypath--load-path-ignore-regexp-extra))
  (setq tinypath--load-path-ignore-regexp
	(concat tinypath--load-path-ignore-regexp
		tinypath--load-path-ignore-regexp-extra)))

;;  Experienced users have a chance to add more regexps to the variable

(run-hooks 'tinypath--load-path-ignore-regexp-hook)

(message "TinyPath: [VAR] tinypath--ignore-file-regexp %s"
	 (prin1-to-string tinypath--ignore-file-regexp))

(eval-and-compile ;;  Needed at boot-time.
  (defcustom tinypath--core-emacs-load-path-list nil
    "*List of core Emacs lisp directories.

Setting this variable is mandatory if the initial `load-path'
in Emacs startup does not contain core lisp packages.

Emacs:

    In Emacs, this would be directory where core lisp files
    reside, typically /usr/share/emacs/NN.N/lisp.

XEmacs:

    In XEmacs, you would add the location of
    xemacs-packages, mule-packages and site-packages or in older versions
    /usr/lib/xemacs-NN.N/lisp/

   You do not need to set this variable for XEmacs, because the automatic boot
   up will find the core packages provided that packages have been
   installed at the same level as the XEmacs itself:

       XEmacs/xemacs-NN.N/
       XEmacs/site-packages/
       XEmacs/mule-packages/
       ..."
    :type  'directory
    :group 'TinyPath))

(message "TinyPath: [VAR] tinypath--core-emacs-load-path-list %s"
	 (prin1-to-string tinypath--core-emacs-load-path-list))

(defcustom tinypath--load-path-root
  (tinypath-set-default-value-macro
   "tinypath--load-path-root"
   (tinypath-default-load-path-root-dirs))
  "*List of root directories of Emacs lisp packages.
Put list all lisp package installation roots here, like

 (setq tinypath--load-path-root
   (list
    (if (not tinypath--xemacs-p)
       ;; This is for Emacs only
	\"/usr/local/share/emacs/site-lisp\")
     \"/usr/local/share/site-lisp\"
     \"/opt/share/site-lisp\"
     ;; or ~/lisp
     \"~/elisp\")

Non-existing directories do no harm, because every
element that is not a string and a valid directory is ignored."
  :type  '(list directory)
  :group 'TinyPath)

(defcustom tinypath--extra-path-root
  (tinypath-set-default-value-macro
   "tinypath--extra-path-root"
   (let ((path (tinypath-ti::win32-cygwin-p 'use-cache)))
     (when path
       (message
	(concat "TinyPath: Cygwin root is %s."
		" Consider adding all Cygwin INFO directories"
		" to variable `Info-directory-list'.")
	path))
     nil))
  "*Win32 Cygwin installation root or other search directories.
This variable contains list of directories.

In many times people working with Emacs also install http://www.cygwin.com/
Unix environment, which contains manual pages and info files for the
utilities.

Set this variable to LIST of additional search root directories
for manual pages and info files."
  :type  '(list directory)
  :group 'TinyPath)

(message "TinyPath: [VAR] tinypath--extra-path-root %s"
	 (prin1-to-string tinypath--extra-path-root))

;;; ....................................................... &v-private ...

(defvar tinypath--original-load-path load-path
  "Original load-path value before loading this package.
It is used later in \\[tinypath-cache-regenerate]. DO NOT TOUCH.")

(defvar tinypath--original-load-path-after-load nil
  "The `load-path' value after this package has been loaded.
If `load-path' changes during Emacs sesssion, then
cache is not used. This happens e.g. while value locally
bound:

    (let ((load-path  ...))
      ;; The value is no longer the global value
      ....

DO NOT TOUCH. Only function that regenerate cache are allowed
to change this.")

(defvar tinypath--log-file
  (tinypath-tmp-find-writable-dir "emacs-tinypath.el.log")
  "With `tinypath--verbose' set to 20, the message buffer
is constantly written to disk. Prepare, everything will take oodles
of time...")

(defvar tinypath--external-data-structure nil
  "Whole data structure from external tool. See `tinypath-external-setup'.
Do not touch. This is highly important for debugging purposes.")

(defvar tinypath--extra-manpath nil
  "Additional paths found. See `tinypath--extra-path-root'.")

(defvar tinypath--extra-ff-search-directories nil
  "Additional C/C++ include paths found. See `tinypath--extra-path-root'")

(defvar tinypath-report-mode-map nil
  "Keymap for buffer generated by `tinypath-cache-duplicate-report'.")

(defvar tinypath--cache nil
  "List of all lisp files along `load-path'.
\((\"file\" (POS . PATH)) .. ).")

(defvar tinypath--time-data nil
  "When each package is loaded, its load time is recoded here.
See `tinypath-time-display'. The data structure is ((package . time-sec)).")

(defvar tinypath--time-buffer "*tinypath-time-results*"
  "Buffer to put results of `tinypath-time-display'.")

(defvar tinypath--cache-level-two nil
  "Cache of tinypath--cache. It keeps the files already resolved by
consulting the cache. Its aim is to speed up the search.
\((\"file\" . \"absolute-path\") ...).")

(defvar tinypath-dumped-load-path nil
  "Load path with Disk Drive letters. See `tinypath-load-path-dump'.")

(defvar tinypath--cache-mode nil
  "State of `tinypath-cache-mode'. DO NOT CHANGE THIS VARIABLE DIRECTLY.
There is more than just changing this variable's state.
Use function `tinypath-cache-mode' which modifies everything needed.")

(defvar tinypath--report-buffer "*tinypath-report*"
  "*Buffer where to report e.g. after `tinypath-cache-duplicate-report'.")

(defvar tinypath--timer-elt nil
  "Timer process.")

(defconst tinypath--report-mode-font-lock-keywords
  (list
   ;; File size
   (list
    (concat
     "[0-9][0-9]:[0-9][0-9]:[0-9][0-9][ \t]+"
     "\\(.*\\)")
    1 'font-lock-reference-face)
   ;; Filename
   (list
    (concat
     "^[ \t]+[0-9]+[ \t]+"
     "\\([0-9]+\\)")
    1 'font-lock-variable-name-face)
   ;;  Emacs core installation
   (list
    "x?emacs[-\\/][0-9]+[0-9.]+"
    0 'font-lock-keyword-face t)
   (list
    "ERROR:"
    0 'font-lock-constant-face)
   ;; filename heading at the start of the line
   (list
    "^[^ \t\r\n]+"
    0 'font-lock-string-face)
   (list ;; mark deleted files
    "^[*].*"
    0 'font-lock-comment-face t))
  "*Font lock keywords for the `tinypath--report-buffer' buffer.")

(defvar tinypath--external-util-bin "emacs-util.pl"
  "*External utility to help finding Emacs boot up information.
DO NOR CHANGE THE NAME OF THE BINARY unless you rename the utility.
See M-x tinypath-version (the manual) for more information.")

;;}}}
;;{{{ Macros

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-with-let-variable 'lisp-indent-function 2)
(defmacro tinypath-with-let-variable (var value &rest body)
  `(let ((,var ,value))
     ,@body))

;;; ----------------------------------------------------------------------
;;; Only some values are recorded as messages to the *Messages* buffer
;;; Showing the values possibly makes user think if he needs
;;; to change the defaults.
;;;
(put 'tinypath-with-temp-buffer 'lisp-indent-function 0)
(put 'tinypath-with-temp-buffer 'edebug-form-spec '(body))
(defmacro tinypath-with-temp-buffer (&rest body)
  "Clear all hooks while running `with-temp-buffer'"
  `(let (temp-buffer-setup-hook
	 font-lock-mode
	 lazy-lock-mode)
     ;;  This is no-op, just quiets Byte Compiler (non used variable).
     (if temp-buffer-setup-hook
	 (setq temp-buffer-setup-hook nil))
     (if font-lock-mode
	 (setq font-lock-mode nil))
     (if temp-buffer-setup-hook
	 (setq temp-buffer-setup-hook nil))
     (if lazy-lock-mode
	 (setq lazy-lock-mode nil))
     (with-temp-buffer
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-Info-default-directory-list ()
  "Emacs and XEmacs compatibility."
  ;; Latest XEmacs does not use `Info-default-directory-list'
  (if tinypath--xemacs-p
      (intern "Info-directory-list")
    (intern "Info-default-directory-list")))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-Info-default-directory-list-sym ()
  "Emacs and XEmacs compatibility."
  `(if tinypath--xemacs-p
       (intern "Info-directory-list")
     (intern "Info-default-directory-list")))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-message-log-max-sym ()
  "Emacs and XEmacs compatibility."
  `(cond
    ((boundp 'log-message-max-size) ;; XEmacs
     (intern "log-message-max-size"))
    ((boundp 'message-log-max)
     (intern "message-log-max"))
    (t
     (error "tinypath-message-log-max-sym"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-message-log-max-sym-value ()
  "Emacs and XEmacs compatibility."
  (symbol-value (tinypath-message-log-max-sym)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypath-message-log-max-sym-set (value)
  "Emacs and XEmacs compatibility."
  `(set (tinypath-message-log-max-sym) ,value))

;;; ----------------------------------------------------------------------
;;; #copy: from tinyliba.el
(defmacro tinypath-ti::bool-toggle (var &optional arg)
  "Toggle VAR according to ARG like mode would do.
Useful for for functions that use arg 0/-1 = off, 1 = on, nil = toggle.
Minor modes behave this way.

VAR is set to following values when ARG is:

  arg 0/-1  VAR -> nil
  arg nbr   VAR -> t
  arg nil   VAR -> not(var)     toggles variable"
  `(setq ,var
	 (cond
	  ((and (integerp ,arg)
		(< ,arg 1))		;Any negative value or 0
	   nil)
	  ((null ,arg)
	   (not ,var))
	  (t
	   t))))

;;}}}
;;{{{ Duplicated functions

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defsubst tinypath-ti::date-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g., if you want to calculate days; you'd do

\(/ (tinypath-ti::date-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (let ((hi (- (car a) (car b)))
	(lo (- (car (cdr a)) (car (cdr b)))))
    (+ (lsh hi 16) lo)))

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defun tinypath-ti::dired-buffer (dir)
  "Return dired buffer runninr DIR."
  (setq dir (file-name-as-directory dir)) ;; Dired uses trailing slash
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eq major-mode 'dired-mode)
		 (string= dired-directory dir))
	(return buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-ti::window-single-p ()
  "Check if there is only one window in current frame."
  ;;  No need to run `length' when `nth' suffices.
  (let ((win  (selected-window))
	(next (next-window)))
    ;;  Same window?
    (eq win next)))

;;; ----------------------------------------------------------------------
;;; #copy: tinylibm.el
(defmacro tinypath-ti::funcall (func-sym &rest args)
  "Call FUNC-SYM with ARGS.
Like funcall, but secretly call function if it exists.

The full story:

  Byte Compiler isn't very smart when it comes to knowing if
  symbol exist or not. If you have following statement in your function,
  it still complaints that the function \"is not known\"

  (if (fboundp 'some-non-existing-func)
      (some-non-existing-func arg1 arg2 ...))

  instead use:

  (if (fboundp 'some-non-existing-func)
      (tinypath-ti::funcall 'some-non-existing-func arg1 arg2 ...)

  to get rid of the unnecessary warning.

Warning:

  You _cannot_ use ti::funcall if the function is in autoload state, because
  `symbol-function' doesn't return a function to call. Rearrange
  code so that you do (require 'package) test."
  `(let ((func ,func-sym))
     (when (fboundp ,func-sym)
       ;; Old
       ;;   (apply (symbol-function ,func-sym) ,@args nil)
       (apply func ,@args nil))))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylib.el
(defun tinypath-days-old (file)
  "How old FILE is in days. An approximation."
  (let* ((a  (current-time))
	 (b  (nth 5 (file-attributes file)))
	 (hi (- (car a) (car b)))
	 (lo (- (car (cdr a)) (car (cdr b)))))
    (/ (+ (lsh hi 16) lo) 86400)))

;;; ----------------------------------------------------------------------
;;; #copy from tinylibm.el
(defun tinypath-ti::replace-match (level &optional replace string)
  "Kill match from buffer at sub-match LEVEL or replace with REPLACE.
Point sits after the replaced or killed area.

Optionally you can give STRING. If level didn't match, do nothing.

Call:

  (level &optional replace string)

Return:

  t     Action taken
  nil   If match at LEVEL doesn't exist.
  str   If string was given."
  (if (null string)
      (cond
       ((match-end level)
	(delete-region (match-beginning level) (match-end level))

	;;  I think emacs has bug, because cursor does not sit at
	;;  match-beginning if I delete that region, instead it is off +1
	;;  --> force it to right place

	(and replace
	     (goto-char (match-beginning level))
	     (insert replace))))
    (when (match-end level)             ;Handle string case
      (concat
       (substring string 0 (match-beginning level))
       (if replace replace "")
       (substring string (match-end level))))))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylibb.el
(defun tinypath-replace-regexp-in-string
  (regexp rep string &optional fixedcase literal subexp start)
  (let ((i  0))
    (or subexp
	(setq subexp 0))

    (while (string-match regexp string)
      (if (> (incf i) 5000)
	  (error "Substituted string causes circular match. Loop never ends.")
	(setq string (inline (tinypath-ti::replace-match subexp rep string)))))
    string))

;;; ----------------------------------------------------------------------
;;; #copy: from tinylibm.el
(defun tinypath-ti::pp-variable-list (list &optional buffer def-token)
  "Print LIST of variables to BUFFER. DEF-TOKEN defaults to `defconst'."
  (let (val)
    (or buffer
	(setq buffer (current-buffer)))
    (or def-token
	(setq def-token "defconst"))
    (dolist (sym list)
      (unless (symbolp sym)
	(error "List member is not symbol %s" sym))
      (setq val (symbol-value sym))
      (insert (format "\n\n(%s %s\n" def-token (symbol-name sym)))
      (cond
       ((numberp val)
	(insert val))
       ((stringp val)
	(insert (format "\"%s\"" val)))
       ((memq val '(t nil))
	(insert (symbol-name val)))
       ((and (symbolp val)
	     (fboundp val))
	(insert "(function " (symbol-name val) ")"))
       ((symbolp val)
	(insert "'" (symbol-name val)))
       ((listp
	 (insert "'" (pp val))))
       (t
	(error "unknown content of stream %s %s" sym val)))
      (insert ")"))))

;;; ----------------------------------------------------------------------
;;; #copy from tinylibm.el
(defun tinypath-ti::write-file-variable-state
  (file desc list &optional fast-save bup)
  "Save package state to FILE.

Input:

  FILE      filename
  DESC      One line description string for the file.
  LIST      List of variable symbols whose content to save to FILE.

  FAST-SAVE The default `pp' function used to stream out the contents
	    of the listp variables is extremely slow if your variables
	    contain lot of data. This flag instructs to use alternative,
	    much faster, but not pretty on output, method.

  BUP       If non-nil, allow making backup. The default is no backup."
  (tinypath-with-temp-buffer
   (let ((backup-inhibited (if bup nil t))
	 ;;  prohibit Crypt++ from asking confirmation
	 (crypt-auto-write-buffer  t))
     (unless crypt-auto-write-buffer    ;Bytecomp silencer
       (setq crypt-auto-write-buffer nil))
     (insert ";; " file " -- " desc "\n"
	     ";; Date: "
	     (tinypath-time-string)
	     "\n\n")
     (if (not fast-save)
	 (tinypath-ti::pp-variable-list list)
       (dolist (var list)
	 (insert (format "\n\n(defconst %s\n" (symbol-name var)))
	 ;;  While `pp' would have nicely formatted the value, It's
	 ;;  unbearable SLOW for 3000 file cache list.
	 ;;  `prin1-to-string' is 10 times faster.
	 (insert "'" (prin1-to-string (symbol-value var)) ")\n")))
     (insert (format "\n\n;; end of %s\n" file))
     (write-region (point-min) (point-max) file))))

;;; ----------------------------------------------------------------------
;;; #copy from tinylib.el
(defun tinypath-ti::advice-control
  (list regexp &optional disable verb msg)
  "Enables/disable SINGLE-OR-LIST of advised functions that match REGEXP.
Signals no errors, even if function in LIST is not advised.
All advice classes ['any] are ena/disabled for REGEXP.

Input:

  LIST                  list of functions.
  REGEXP                advice name regexp. Should normally have ^ anchor
  DISABLE               flag, of non-nil then disable
  VERB                  enable verbose messages
  MSG                   display this message + on/off indication"
  (dolist (func list)
    (ignore-errors
      (if disable
	  (ad-disable-advice  func 'any regexp)
	(ad-enable-advice     func 'any regexp))
      (ad-activate func))) ;;change state
  (if verb
      (message
       (concat
	(or msg "advice(s): ")
	(if disable
	    "off"
	  "on")))))

;;; ----------------------------------------------------------------------
;;; #copy
(defun tinypath-ti::string-remove-whitespace (string)
  "Squeezes empty spaces around beginning and end of STRING.
If STRING is not stringp, then returns STRING as is."
  (when (stringp string)
    (if (string-match "^[ \t]+\\(.*\\)" string)
	(setq string (match-string 1 string)))

    (if (string-match "[ \t]+\\'" string)
	(setq string
	      (substring string 0  (match-beginning 0)))))
  string)

;;; ----------------------------------------------------------------------
;;; #copy: from tinylib.el
(defun tinypath-ti::vc-version-lessp (a b &optional zero-treat)
  "Return t if A is later version than B.
This function can only check only three levels, up till: NN.NN.NN.

Input

  A             Version string one
  B             Version string two
  ZERO-TREAT    If non-nil, consider version numbers starting with 0.NN
		never than 2.1. In this case it is assumed
		that zero based versions are latest development releases."
  (flet ((version (str regexp)
		  (if (string-match regexp str)
		      (string-to-number (match-string 1 str))
		    0)))
    (let* ((a1 (version a "^\\([0-9]+\\)"))
	   (a2 (version a "^[0-9]+\\.\\([0-9]+\\)"))
	   (a3 (version a "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)"))
	   (b1 (version b "^\\([0-9]+\\)"))
	   (b2 (version b "^[0-9]+\\.\\([0-9]+\\)"))
	   (b3 (version b "^[0-9]+\\.[0-9]+\\.\\([0-9]+\\)")))
      (or (and zero-treat
	       (and (= a1 0)
		    (> b1 0)))
	  (> a1 b1)
	  (and (= a1 b1)
	       (> a2 b2))
	  (and (= a1 b1)
	       (= a2 b2)
	       (> a3 b3))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-message-get-buffer ()
  "Return *Message* buffer pointer."
  (or (get-buffer "*Messages*")
      (get-buffer " *Message-Log*"))) ;; XEmacs

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-log-write ()
  "*Write log to `tinypath--log-file'."
  (let ((buffer (tinypath-message-get-buffer))
	(file   tinypath--log-file))
    (ignore-errors
      (with-current-buffer buffer
	(write-region (point-min) (point-max) file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-ti::compat-timer-elt  (function)
  "Search FUNCTION and return timer elt.
You can use this function to check if some function is currently
in timer list. (i.e. active)

The timer lists are searched in following order:

  `itimer-list'
  `timer-list'
  'timer-idle-list'

Return:

  '(timer-elt timer-variable)"
  (let (pos
	list
	item
	ret)
    (flet ((get-elt (elt place)
		    (if (vectorp elt)
			(aref elt place)
		      (nth place elt))))
      (dolist (timer '(
		       ;; (("Mon Dec  9 10:01:47 1996-0" 10
		       ;;     process nil))
		       (timer-idle-list . 5)
		       (timer-alist . 2)
		       (timer-list  . 2) ;; 19.34+
		       (itimer-list . 3)))
	(when (boundp (car timer))
	  (setq list (symbol-value (car timer))
		pos  (cdr timer))
	  ;;  NOTE: this is different in Xemacs. It is not a vector
	  ;; timer-[idle-]list Emacs 19.34
	  ;;  NOTE: this is different in Xemacs. It is not a vector

	  ;; ([nil 12971 57604 0 60 display-time-event-handler nil nil])
	  ;; [nil 13971 14627 646194 60
	  ;;      (lambda (f) (run-at-time ...))
	  ;;      (irchat-Command-keepalive) nil]
	  (if (and (not tinypath--xemacs-p)
		   (vectorp (car list)))
	      (setq pos 5))
	  (dolist (elt list)
	    (setq item (get-elt elt pos))
;;;     (d!! (functionp item) (get-elt elt (1+ pos)))
	    (when (or (and (symbolp item)
			   (eq item function))
		      ;;  It may be lambda expression
		      (and (functionp item)
			   (string-match (regexp-quote (symbol-name function))
					 (prin1-to-string
					  (get-elt elt (1+ pos))))))
	      (setq ret (list elt (car timer)))
	      (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-ti::compat-timer-cancel (key &optional cancel-function)
  "Delete timer KEY entry, where KEY is full element in (i)`timer-alist'."
  (let (var)
    (when key
      (when (and (null var)
		 (boundp 'timer-alist)) ;Emacs
	(setq var 'timer-alist)
	(tinypath-ti::funcall 'cancel-timer key)
	(set var (delete key (symbol-value 'timer-alist))))
      (when (and (null var)
		 (boundp 'timer-list))  ;Emacs 19.34
	(setq var 'timer-list)
	;;  Must use this command
	(tinypath-ti::funcall 'cancel-timer key))
      (when (and (null var)
		 (boundp 'timer-idle-list)) ;Emacs 19.34
	(setq var 'timer-idle-list)
	;;  Must use this command
	(tinypath-ti::funcall 'cancel-timer key))
      (when (and (null var)
		 (boundp 'itimer-list)) ;XEmacs
	(setq var 'itimer-list)
	(tinypath-ti::funcall 'cancel-itimer key)
	(set var (delete key (symbol-value 'itimer-list))))
      var)))

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defun tinypath-ti::compat-timer-cancel-function (function)
  "Delete all timer entries for FUNCTION."
  (let (key)
    (while (setq key (car-safe (tinypath-ti::compat-timer-elt function)))
      (tinypath-ti::compat-timer-cancel key))
    key))

;;; ----------------------------------------------------------------------
;;; #copy: tinylib.el
(defun tinypath-ti::directory-recursive-do (root function)
  "Start at ROOT and call FUNCTION recursively from each ascended directory."
  (let ((list (tinypath-subdirectory-list root)))
    (if (null list)
	(funcall function root)
      (dolist (path list)
	(tinypath-ti::directory-recursive-do path function)))))

;;}}}
;;{{{ Modes

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-default-bindings ()
  "Define default key bindings to `tinypath-report-mode-map'."
  (unless (keymapp tinypath-report-mode-map)
    (setq tinypath-report-mode-map (make-sparse-keymap))
    (cond
     (tinypath--xemacs-p
      (define-key tinypath-report-mode-map [(control shift button1)]
	'tinypath-report-mode-delete-file))
     (t
      (define-key tinypath-report-mode-map [C-S-mouse-1]
	'tinypath-report-mode-delete-file)))
    ;; ............................................. users with no mouse ...
    (define-key tinypath-report-mode-map "\C-d"
      'tinypath-report-mode-delete-file)
    (define-key tinypath-report-mode-map "\C-c\C-d"
      'tinypath-report-mode-delete-file-noconfirm)
    (define-key tinypath-report-mode-map "\C-cd"
      'tinypath-report-mode-dired)
    (define-key tinypath-report-mode-map "\C-p"
      'tinypath-report-mode-previous)
    (define-key tinypath-report-mode-map [(control up)]
      'tinypath-report-mode-previous)
    (define-key tinypath-report-mode-map "\C-n"
      'tinypath-report-mode-next)
    (define-key tinypath-report-mode-map [(control down)]
      'tinypath-report-mode-next)
    (define-key tinypath-report-mode-map "\C-cr"
      'tinypath-cache-duplicate-report)
    (define-key tinypath-report-mode-map "\C-cg"
      'tinypath-cache-regenerate)
    (define-key tinypath-report-mode-map [(return)]
      'tinypath-report-mode-find-file)
    (define-key tinypath-report-mode-map "\C-cf"
      'tinypath-report-mode-find-file)))

;;}}}
;;{{{ Debug

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-debug-wrapper-macro 'lisp-indent-function 0)
(put 'tinypath-debug-wrapper-macro 'edebug-form-spec '(body))
(defmacro tinypath-debug-wrapper-macro (&rest body)
  "Increase `tinypath--verbose' and `message-log-size'."
  `(let ((tinypath--verbose 12))
     ;;  Value t is unlimited in Emacs, but don't know about XEmacs
     ;;  Setting a high value works always.
     (set (tinypath-message-log-max-sym) 900000)
     (with-current-buffer (tinypath-message-get-buffer)
       ,@body
       (pop-to-buffer (current-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-debug-test-run (&optional clear)
  "Developer function. Test everything with full debug and CLEAR buffer."
  (interactive "P")
  (tinypath-debug-wrapper-macro
   (if clear
       (erase-buffer))
   (tinypath-cache-regenerate)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-debug-external-helper ()
  "Developer function. Test external helper program."
  (interactive)
  (tinypath-debug-wrapper-macro
   (tinypath-external-helper-call
    (current-buffer)
    (tinypath-external-setup-1-main)
    'debug)))

;;}}}
;;{{{ Misc

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-load-copy-get (&optional property)
  "Return value of `tinypath--original-load-path-after-load'.
Optionally from PROPERTY."
  (if property
      (get 'tinypath--original-load-path-after-load property)
    tinypath--original-load-path-after-load load-path))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-load-copy-now (&optional property)
  "Save `load-path' to `tinypath--original-load-path-after-load'.
Optionally save the value to PROPERTY."
  (if property
      (put 'tinypath--original-load-path-after-load
	   property
	   load-path)
    (setq tinypath--original-load-path-after-load load-path)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-load-copy-equal-p ()
  "Return non-nil if saved `load-path' copy has not changed."
  (equal tinypath--original-load-path-after-load load-path))

;;; ----------------------------------------------------------------------
;;; (tinypath-eval-after-load "woman" 'tinypath-insinuate-woman)
;;;
(defun tinypath-eval-after-load (file function)
  "Simulate `eval-after-load'. load FILE and run FUNCTION."
  (cond
   ((not (fboundp 'eval-after-load)) ;; Older Emacs versions do not have this.
    (and (load file 'noerr)
	 (funcall function)))
   (t
    ;; See after-load-alist
    ;; ... If FILE is already loaded, evaluate FORM right now.
    (eval-after-load file
      `(progn (funcall (quote ,function)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-string (&optional time)
  "Return TIME in ISO 8601 format YYYY-MM-DD HH:MM:SS"
  (format-time-string "%Y-%m-%d %H:%M:%S" (or time (current-time))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-list-display (msg list &optional insert)
  "Display MSG and LIST to *Messages* or INSERT.
The MSG should contain %s format string to write each element."
  (let ((i 0)
	(size 80000)) ;; 60k
    ;;  Without increasing the display size, all of the cached
    ;;  paths would not be seen. This could also be checked dynamically
    ;;  by computing <`length' of cache> x <approx. 120 characters display>
    (when (and (null insert)
	       (< (tinypath-message-log-max-sym-value) size))
      (tinypath-message-log-max-sym-set size))
    (dolist (elt list)
      (incf i)
      (setq elt (if (stringp elt)
		    elt
		  (prin1-to-string elt)))
      (setq elt (format (concat "%3d " msg) i elt))
      (if insert
	  (insert elt "\n")
	(message elt))))
  (unless insert
    (let ((buffer (tinypath-message-get-buffer)))
      (when buffer
	(display-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-suffixes (file)
  "Return list of try suffixes for FILE. '(\".el\" \".elc\")."
  (cond
   ((string-match "\\.elc?$" file)
    '(""))
   (t
    '(".el" ".elc"))))

;;; ----------------------------------------------------------------------
;;; We need this because we use advised `locate-library'
;;;
(defun tinypath-locate-library (file)
  "Like `locate-library' FILE, but return list of paths."
  (let (path-list
	(suffix (tinypath-suffixes file))
	path)
    (dolist (dir load-path)
      (setq dir (file-name-as-directory dir))
      (dolist (postfix suffix)
	(setq path (concat dir file postfix))
	(when (file-exists-p path)
	  (pushnew path path-list :test 'string=))))
    path-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-byte-compile-file (file)
  "Byte compile FILE is file name end to \".elc\"."
  (when (and (stringp file)
	     (string-match "\\.el$" file))
    (unless (byte-compile-file file)
      (message "TinyPath: {ERROR] Byte compile failed for %s" file)
      (delete-file (concat file "c")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-report-ignore-function (file)
  "Ignore from output in XEmacs _pkg.el and the like."
  ;; In XEmacs there are lot of these pkg files.
  (string-match
   "\\(auto-autoloads\\|_pkg\\|custom-load\\|load-path\\)\\.el"
   file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-maybe-warn-message-log-max ()
  "Print message if Message-Log size is too small.
Too small value would prevent debugging tinypath.el."
  (let ((size 20000)
	now)
    (setq now
	  (symbol-value (tinypath-message-log-max-sym)))
    (when (and (> tinypath--verbose 9)
	       ;;  Value `t' is for unlimited size.
	       (or (not (eq t now))
		   (and (integerp now)
			(and (< now size)))))
      (message
       (concat "TinyPath: Possibly can't display all logs. Increase "
	       (symbol-name
		(tinypath-message-log-max-sym))))
      (sit-for 2))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-file-compressed-p (file)
  "Check if FILE includes a comression extension."
  (string-match "\\.\\(gz\\|[Zz]\\|bz2\\)$" file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-use-compression ()
  "Load jka-compr.el safely."
  (or (featurep 'jka-compr)
      (let ((file (or (tinypath-cache-p "jka-compr")
		      (locate-library "jka-compr")
		      (error "\
TinyPath: [PANIC] Can't find Emacs core library jka-cmpr.el."))))
	(if (fboundp 'ad-Orig-load)
	    (tinypath-ti::funcall (function ad-Orig-load) file)
	  (load file))
	;; New X/Emacs releases need this
	(cond
	 ((fboundp 'auto-compression-mode) ;; New Emacs: jka-compr.el
	  ;; symbol-function suppresses Byte compiler messages
	  (funcall (symbol-function 'auto-compression-mode) 1))
	 ((fboundp 'jka-compr-install)
	  (tinypath-ti::funcall (function jka-compr-install)))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-use-compression-maybe (file)
  "Use compression if FILE includes a compressed file extension."
  (or (featurep 'jka-compr)
      (when (tinypath-file-compressed-p file)
	(tinypath-use-compression))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-warn-if-not-exist (file)
  "Print message if FILE does not exist."
  (when (stringp file)
    (tinypath-use-compression-maybe file))
  (when (null (let (ret)
		(dolist (ext '("" ".el" ".elc"))
		  (when (file-exists-p (concat file ext))
		    (setq ret t)
		    (return)))
		ret))
    (message
     (substitute-command-keys
      (format
       "TinyPath: CACHE invalid. The cached file does not exist %s \
Please run \\[tinypath-cache-regenerate]"
       file)))
    (sleep-for 1)
    t))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-compressed (&optional list)
  "Append `tinypath--compressed-file-extensions' to each element in LIST.
If `tinypath--compression-support' is nil, then do nothing and return nil."
  (let (ret)
    (dolist (elt (or list '("")))
      (when (stringp elt)
	;;  `nreverse' is due to `push' which would change the order
	(dolist (ext (reverse tinypath--compressed-file-extensions))
	  (when (stringp ext)
	    (push (concat elt ext) ret)))))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-list (package)
  "Return possible extensions to search for PACKAGE. This function is used
only once to return the search extension list to the cache function. The
list is reused internally and chhanging
`tinypath--compressed-file-extensions' afterward in running Emacs has no
effect."
  ;; See `tinypath-suffixes'
  (cond
   ((string-match "\\.elc$" package)
    (append '(".elc")
	    (tinypath-file-extension-compressed '(".elc"))))
   ((string-match "\\.el$" package)
    (append '(".el")
	    (tinypath-file-extension-compressed '(".el"))))
   ((string-match "\\(z\\|bz2\\)$" package)
    nil)
   (t
    (let (ret)
      ;;  The correct order is ELCs first then EL.
      ;;  The list is built in reverse order here.
      (setq ret (tinypath-file-extension-compressed '(".el")))
      (push ".el" ret)
      (dolist (elt (tinypath-file-extension-compressed '(".elc")))
	(push elt ret))
      (push ".elc" ret)
      ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-extension-list-choices ()
  "Return list of choices to search.
 '((el . (list)) (elc . (list)) (nil . (list)))."
  (let* (
	 ;; As a fall back, should we search .el choices if .elc
	 ;; choices fail
	 (elc (append (tinypath-file-extension-list "package.elc")
		      (tinypath-file-extension-list "package.el")))
	 (el  (tinypath-file-extension-list "package.el"))
	 (all (tinypath-file-extension-list "package")))
    (list
     elc
     el
     (cons nil all))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-file-remove-trailing-slash (path)
  "Remove trailing slashes, unless it is a Win32 root dir f:/"
  (unless (string-match "^[a-z]:[\\/]$" path)
    (if (string-match "^\\(.*\\)[\\/]$" path)
	(setq path (match-string 1 path))))
  path)

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-lisp-file-list (&optional from-cache)
  "Return only lisp file alist (file . path) from `tinypath--cache'.
With optional parameter FROM-CACHE, use the latest cached value.
Be warned, this may not be the absolute latest."
  (let ((id "tinypath-emacs-lisp-file-list")
	list
	save)
    (when from-cache
      (setq list (get 'tinypath-emacs-lisp-file-list 'cache)))

    (unless tinypath--cache
      (message "%s: [ERROR] `tinypath--cache' is nil." id))

    (unless list
      (setq save t)
      (dolist (elt tinypath--cache)
	(when (string-match "\\.el.?$" (car elt))
	  (push (cons (car elt) (cdr (nth 1 elt)))
		list))))

    (if save
	(put 'tinypath-emacs-lisp-file-list 'cache list))

    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-emacs-lisp-file-list-cache-clear ()
  "Clear cache kept by `tinypath-emacs-lisp-file-list'."
  (put 'tinypath-emacs-lisp-file-list 'cache nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-list-clean (list &optional list-name)
  "Clean LIST for anything suspicious: non-directories, non-strings.
If you have moved directories from one place to another or some program has
added entries to it, it is possible that LIST is \"fragmented\".

- Remove non-strings, possibly (nil t) values.
- Expand all directories. In Win32, `downcase' every path.
- Convert to use only forward slashes.
- Remove trailing slashes.
- Remove duplicate paths.
- Remove non existing paths

Input:

  LIST         List, List of directories
  LIST-NAME    String, The name of variable for debug."
  (let (new-path)
    (or list-name
	(setq list-name ""))
    (dolist (path list)
      (cond
       ((not (stringp path))
	(tinypath-verbose-macro 5
	  (message "TinyPath: %s cleaned, NON-STRING ENTRY %s"
		   list-name
		   (prin1-to-string path))))
       ((not (file-directory-p path))
	(tinypath-verbose-macro 5
	  (message "TinyPath: [WARN] %s cleaned, directory does not exist %s"
		   list-name path)))
       (t
	;;  This will also convert all paths to forward slashes
	;;  and downcase them in win32
	(setq path (tinypath-expand-file-name path))
	;;  Remove trailing slashes, unless it is a Win32 root dir like C:/
	(setq path (tinypath-file-remove-trailing-slash path))
	(tinypath-verbose-macro 7
	  (message "TinyPath: %s added %s" list-name path))
	(pushnew path new-path :test 'string=))))
    (nreverse new-path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-find-dir  (file dir-list)
  "Search DIR-LIST and return directory when FILE is found.
If FILE is nil, then return first existing directory in DIR-LIST.

Note: directory list passed can contain non-string entries. They are ignored."
  (let (ret)
    (dolist (dir dir-list)
      (when (stringp dir)
	(when (string-match "[/\\]$" dir) ;Remove trailing slash
	  (setq dir (substring dir 0 (1- (length dir))  )))
	(when (file-exists-p
	       (concat (file-name-as-directory dir)
		       (or file "")))
	  (setq ret (tinypath-expand-file-name dir))
	  (return))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-path-ok-this-emacs-p (path)
  "Check that /emacs path is for Emacs and /xemacs path is for XEmacs.
Return t if path is ok for current Emacs."
  (let ((no-emacs-regexp (if (inline tinypath--xemacs-p)
			     ".*[/\\]emacs"
			   ".*[/\\]xemacs"))
	(this-emacs-regexp (if (inline tinypath--xemacs-p)
			       ".*[/\\]xemacs"
			     ".*[/\\]emacs"))
	(correct-emacs   t)
	len1
	len2)
    (when (string-match no-emacs-regexp path)
      (setq len1 (length (match-string 0 path)))
      ;;  If path contains both the word Emacs and XEmacs, then it
      ;;  is hard to know if this is invalid or not
      ;;
      ;;   /usr/local/share/bin/emacs/xemacs/xemacs-21.2
      ;;   /usr/local/share/bin/emacs/emacs/emacs-20.3
      ;;
      (when (string-match this-emacs-regexp path)
	(setq len2 (length (match-string 0 path)))
	(tinypath-verbose-macro 7
	  (message "TinyPath: PATH-NOK both emacs versions in path?? %s" path)))
      (when (or (null len2)
		(< len2 len1)) ;; the correct Emacs name must be LAST
	(setq correct-emacs nil)
	(tinypath-verbose-macro 7
	  (message "TinyPath: PATH-NOK WRONG EMACS %s" path))))
    correct-emacs))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-path-ok-p (path)
  "Check if path is accepted with `tinypath--load-path-ignore-regexp'."
  (when (and (stringp path)
	     (tinypath-path-ok-this-emacs-p path))
    (cond
     ;; .................................................... directory ...
;;; Checked already in `tinypath-directory-list-clean'.
;;;     ((not (file-directory-p path))
;;;     (tinypath-verbose-macro 5
;;;       (message "TinyPath: PATH-NOK dir does not exist: %s"
;;;                path))
;;;      nil)
;;;     ;; ................................................ ignore regexp ...
     ((and (stringp tinypath--load-path-ignore-regexp)
	   (string-match "[ \t\r\n]" tinypath--load-path-ignore-regexp)
	   (let (case-fold-search)
	     (string-match tinypath--load-path-ignore-regexp path)))
      (tinypath-verbose-macro 3
	(message
	 (concat "TinyPath: PATH-NOK tinypath--load-path-ignore-regexp "
		 "matches [%s] (ignored) %s")
	 (match-string 0 path) path))
      nil)
     ;; ...................................................... symlink ...
     ((file-symlink-p path)
      (tinypath-verbose-macro 5
	(message "TinyPath: PATH-NOK symlink (ignored) %s" path))
      nil)
     ;; ................................................ non-core path ...
     ((let (ver)
	(and (setq ver (car-safe (tinypath-emacs-versions 'noerr 'cache)))
	     ;;  It looks like core path ....
	     (tinypath-emacs-core-path-p path)
	     ;;  But it's not for this emacs VERSION
	     (not (tinypath-emacs-core-path-p path ver))))
      (tinypath-verbose-macro 5
	(message "TinyPath: PATH-NOK non-core path (ignored) %s" path))
      nil)
     ;; ........................................................... ok ...
     (t
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-directory-lisp-p (path)
  "Check if directory has any files matching regexp `\\.elc?'."
  (cond
   ((not (stringp path))
    (tinypath-verbose-macro 5
      (message "TinyPath: [error] directory entry %s" (prin1-to-string path))))
   ((not (file-directory-p path))
    (tinypath-verbose-macro 5
      (message "TinyPath: [error] directory not found %s" path)))
   (t
    (dolist (elt (directory-files path))
      (when (string-match "\\.elc?" elt)
	(return t))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-subdirectory-list (path)
  "Return all subdirectories under PATH."
  (let (list)
    (dolist (elt (directory-files path 'absolute) )
      (when (and (not (string-match "\\.\\.?$" elt)) ;; skip . and ..
		 (file-directory-p elt)) ;; take only directories
	(push elt list)))
    list))

;;}}}
;;{{{ autoload and other system help functions

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-self-location-load-history ()
  "Return `load-history' entry"
  (let (file)
    (dolist (elt load-history)
      (setq file (car elt))
      (when (and (stringp file)
		 (setq file (tinypath-expand-file-name file))
		 (string-match "^\\(.+\\)[\\/]tinypath\\." file))
	(return (match-string 1 file))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-self-location ()
  "If package was loaded with absolute path, return path.
Uses `load-history' and `load-path' information."
  (let ((ret (tinypath-self-location-load-history)))
    (unless ret ;; No luck with load-history, try load-path
      (dolist (path load-path)
	(setq path (file-name-as-directory (expand-file-name path)))
	(when (or (and (file-exists-p (concat path "tinypath.el"))
		       path)
		  (and (file-exists-p (concat path "tinypath.elc"))
		       path)
		  (and (file-exists-p (concat path "tinypath.el.gz"))
		       path))
	  (return (setq ret path)))))
    (unless ret
      (message
       (concat
	"TinyPath: SELF NOTE tinypath.el was not loaded"
	"\tusing absolute path."
	"\t(load \"~/some/absolute/path/tinypath.el\")"))
      (message "TinyPath: SELF %s" (or ret "<no load-history>" )))
    ;;  tinypath-* function is XEmacs and Emacs compatible version
    ;;  and ensures that forward slashes are used.
    (and ret
	 (setq ret (tinypath-expand-file-name ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-autoload-file-name (function)
  "Load package if FUNCTION is in autoload state."
  (let ((str (prin1-to-string (symbol-function function))))
    (when (string-match "^(autoload[ \t]+\"\\([^\"]+\\)" str)
      (setq str (match-string 1 str))
      ;;  there is one problem. prin1-to-string doubles every backslash
      ;;  c:\\\\dir\\\\ ... (XEmacs problem)
      (if (string-match "/" str)
	  str
	(let ((final ""))
	  ;; It's easier and faster to do this in buffer, than
	  ;; parsing STRING
	  (tinypath-with-temp-buffer
	   (insert str)
	   (goto-char (point-min))
	   (while (re-search-forward "\\([^\\]+\\)" nil t)
	     (setq final (concat
			  final
			  (match-string 1)
			  "/"))))
	  ;; remove trailing "/"
	  (substring final 0 (1- (length final))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-autoload-require (function &optional noerr nomsg)
  "Load package if FUNCTION is in autoload state.
NOERR NOMSG are parameters to `load'."
  (let ((file (tinypath-autoload-file-name function)))
    (when file
      (load file noerr nomsg))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-admin-remove-matching (path)
  "Remove PATH from `load-path' and add to `tinypath--load-path-ignore-regexp'."
  (let ((fid  "tinypath-admin-remove-matching"))
    ;; Initially the idea was that the entries were purged fom cache too, but
    ;; looping and reconstructing it takes too much time.
    ;;
    ;; It's more efficient to disable packages by using regexps in
    ;; tinypath--load-path-ignore-regexp, although this is not as transparent.
    ;;
    ;; --> #todo: Add better functionality to perl code.

    ;;  Kill second level cache which "remembers" paths.
    (setq tinypath--cache-level-two nil)

    (setq path (regexp-quote (tinypath-expand-file-name path)))
    (tinypath-load-path-remove path)
    (tinypath-load-path-remove-cache path)

    (message "TinyPath: %s adding to tinypath--load-path-ignore-regexp [%s]"
	     fid path)

    (cond
     ((not (stringp tinypath--load-path-ignore-regexp))
      (setq tinypath--load-path-ignore-regexp path))
     ((not (string-match path tinypath--load-path-ignore-regexp))
      (setq tinypath--load-path-ignore-regexp
	    (concat tinypath--load-path-ignore-regexp
		    "\\|" path))))))

;;}}}
;;{{{ External: emacs-util.pl

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse-1-cache ()
  "Parse files in format `tinypath--cache'."
  (let ((i 0)
	(personal-count 0) ;; User files 0 .. 2000
	(other-count 2000)
	(emacs-count 5000)
	(font-lock-mode nil)
	(lazy-lock-mode nil)
	(regexp (concat "^LISP-FILE[ \t]+"
			"\\("
			"\\([^ \t\r\n]+[\\/]\\)"
			"\\([^ \t\r\n]+\\)"
			"\\)"))
	path
	dir
	file
	emacs
	other
	personal
	elt)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq path (match-string 1)
	    dir  (match-string 2)
	    file (match-string 3))
      ;; was: (tinypath-path-ok-p dir) , but now perl does
      ;; the checking
      (when t
	;; (set-text-properties 0 (length dir) nil dir)
	;; (set-text-properties 0 (length file) nil file)
	(incf i)
	(when (zerop (% i 10))
	  (tinypath-verbose-macro 2
	    (message "TinyPath: EXT Caching files... %d %s" i path)))
	;; data structure is ("file.el" (1 . "/home/foo/elisp/"))
	;;
	;;  The reason why we put paths to separate lists is that
	;;  OTHER directories must override the Core Emacs paths,
	;;  so that newest files are found. Usually you can download
	;;  newer versions than what Emacs has.
	(cond
	 ((tinypath-load-path-emacs-distribution-p path)
	  (incf emacs-count)
	  (setq elt (list file (cons emacs-count dir)))
	  (push elt emacs))
	 ((tinypath-load-path-personal-p path)
	  (incf personal-count)
	  (setq elt (list file (cons personal-count dir)))
	  (push elt personal))
	 (t
	  (incf other-count)
	  (setq elt (list file (cons other-count dir)))
	  (push elt other)))))
    (append (nreverse personal) (append other emacs))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse-1 (id)
  "Parse ID from current buffer. See `tinypath-external-helper'."
  (let ((case-fold-search t)
	(regexp (concat "^" id "[ \t]+\\([^ \t\r\n]+\\)"))
	string
	list)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (setq string (match-string 1))
      ;; (set-text-properties 0 (length string) nil string)
      (push string list))
    (unless list
      (tinypath-verbose-macro 1
	(message  "TinyPath: EXT PARSE FATAL (id %s)\n" id)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-output-parse-main ()
  "Parse current buffer. See'`tinypath-external-helper'."
  (let (list
	data
	name)
    ;;  Speedier processing
    (buffer-disable-undo)
    ;;  Clear text properties so that the data structures are clean
    ;;  and possibly faster to use
    (set-text-properties (point-min) (point-max) nil)
    (tinypath-verbose-macro 5
      (message  "TinyPath: EXT OUTPUT \n%s\n" (buffer-string)))
    ;;  This list of symbols is same as the prefix string from
    ;;  the perl script:
    ;;
    ;;  LISP-FILE filename-here
    ;;  LISP-DIR filename-here
    ;;  ...
    (dolist (id '(info
		  bin
		  man
		  lisp-dir
		  c-src-dir))
      (setq name (symbol-name id)
	    data  (tinypath-external-output-parse-1 name))
      (if (null data)
	  (tinypath-verbose-macro 3
	    (message "TinyPath: EXT PARSE ERROR [%s]" name))
	(push (cons id data) list)))
    ;;  'cache (lisp-files) handling is different. Do it now
    (let ((data (tinypath-external-output-parse-1-cache)))
      (if data
	  (push (cons 'cache data) list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-bin-location (file)
  "Return location of BINARY. Look from the installation dir.
Look up `exec-path' and the kit installation directory. See
Manual \\[tinypath-version] for more."
  (let* ((path  (tinypath-executable-find file))
	 (ret   path)
	 self)
    (when (and (null path)
	       (setq self (tinypath-self-location)))
      ;;  PATH/to/.../lisp/tiny/<tinypath.el>
      ;;            |
      ;;            |
      ;;            /bin/emacs-util.pl
      (setq self (tinypath-expand-file-name self))
      (setq self
	    (concat
	     (file-name-as-directory self)
	     ;;  PATH/to/lisp/files/<tinypath.el>
	     "../../bin/"
	     file))
      (if (file-exists-p self)
	  (setq ret self)))
    (tinypath-verbose-macro 3
      (message "TinyPath: EXT bin location %s" ret))
    (when (and ret
	       (not (file-exists-p ret)))
      (message "TinyPath: EXT FATAL, bin location is wrong %s" ret)
      (setq ret nil))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-helper-call (buffer path-list &optional debug)
  "Use external helper Perl script if available.
First, Environment must contain perl executable and second
`tinypath--external-util-bin' must be along path.

Input:

  BUFFER     Where to output.
  PATH-LIST  list of root directories to search.
  DEBUG      Request debug.

Return:

  t          If external utility was found and called."
  (let* ((file  tinypath--external-util-bin)
	 (perl  (tinypath-executable-find-binary "perl"))
	 (bin   (tinypath-external-bin-location
		 tinypath--external-util-bin))
	 (opt   (or path-list
		    (error "TinyPath: path-list is empty.")))
	 (ignore tinypath--load-path-ignore-regexp))
    (tinypath-verbose-macro 3
      (message "TinyPath: EXT perl location %s" (or perl "<not found>")))
    (tinypath-verbose-macro 3
      (message "TinyPath: EXT exec-path %s %s" file (or bin "<not found>")))
    (when debug
      (push "3" opt)
      (push "--debug" opt))
    (when (and tinypath--win32-p
	       (not tinypath--win32-cygwin-p))
      (push "no-symlinks" opt)
      (push "--scan-type" opt))
    (setq ignore
	  (concat
	   (or ignore "")
	   (if (stringp ignore)
	       "\\|" "")
	   (if tinypath--xemacs-p
	       "[/\\]emacs"
	     "[/\\]xemacs")))
    (dolist (switch (list
		     "--Info"
		     "--Man"
		     "--Bin"
		     "--Lang-lisp-file"
		     "--Lang-lisp-dir"
		     "--Lang-c-src-dir"
		     ignore
		     "--ignore-emacs-regexp"))
      ;;  These will go to the beginning, which is ok.
      (push switch opt))
    (push bin opt)
    (when debug
      ;;  If Emacs hangs, at least we know how the external command was called.
      (find-file "~/emacs-debug-tinypath.log")
      (erase-buffer)
      (insert (pp opt))
      (save-buffer))
    (if (null (and perl bin))
	(tinypath-verbose-macro 5
	  "TinyPath: EXT ERROR Can't call external utility")
      (message "TinyPath: EXT Process running... [please wait] %s"
	       (mapconcat 'identity opt " "))
      (with-current-buffer buffer
	(apply 'call-process
	       perl
	       nil
	       (current-buffer)
	       nil
	       opt)
	(tinypath-verbose-macro 9
	  (message
	   (concat "\nTinyPath: EXT OUTPUT END\n")))
	(message "TinyPath: EXT done %s" bin)
	t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-helper-main (path-list)
  "Call external helper with PATH-LIST and parse output.

Return:

  '((info . (path path ..))
    (man  . (path path ..))
    (bin  . (path path ..))
    (lisp . (path path ..))
    (cache . <FORMAT EQUALS TO TINYPATH--CACHE>))."
  (tinypath-with-temp-buffer
   (when (tinypath-external-helper-call (current-buffer) path-list)
     (tinypath-external-output-parse-main))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-1-main ()
  "Return paths to pass to external program."
  (let (list)
    (dolist (elt (list
		  ;; load-path must not be there, because it may be already
		  ;; populated from the cache file: the one that we are
		  ;; trying to build from fresh.
		  ;;
		  ;; -> do not add `load-path' to returned list
		  ;;
		  ;; But we can add the original load path which were
		  ;; saved at startup.
		  tinypath--extra-path-root
		  tinypath--original-load-path
		  tinypath--load-path-root
		  (tinypath-Info-default-directory-list)))
      (dolist (path elt)
	(when (and (stringp path)
		   (not (string-match "^[ \t]+$" path))
		   (file-directory-p path))
	  (push (tinypath-expand-file-name path) list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-cache (data)
  "Set `tinypath--cache from DATA '((cache (DATA) ..)."
  (let ((list (assq 'cache data)))
    (when list
      (setq list (cdr list))
      (setq tinypath--cache list))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-external-setup-1-load-path (path regexp)
  "Add PATH to `load-path'. Use REGEXP to check validity."
  ;; The perl program recursed ALL directories, but we only
  ;; want to find out lisp dirs that USER requested in
  ;; `load-path' and `tinypath--load-path-root'
  ;;
  ;; lisp-roots is a lookup string "PATH\\|PATH\\|PATH .."
  ;; which we can use to check if path is accepted
  ;;
  (cond
   ((not (string-match regexp path))
    (tinypath-verbose-macro 5
      (message "TinyPath: PATH-NOK not candidate %s" path)))
   ((tinypath-path-ok-p path)
    (pushnew path load-path :test 'string=))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-external-setup-1-man-path (path)
  "Add PATH to `tinypath--extra-manpath'."
  (when (or (not (stringp
		  tinypath--manpath-ignore-regexp))
	    (not (string-match
		  tinypath--manpath-ignore-regexp
		  path)))
    (pushnew path tinypath--extra-manpath :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-load-path-lookup-regexp ()
  "Return candidate `load-path' lookup regexp.
This is combination of `load-path' and `tinypath--load-path-root'."
  (let ((lisp-roots (append load-path
			    tinypath--load-path-root)))
    ;; Make lookup regexp
    (mapconcat
     (function
      (lambda (x)
	(regexp-quote
	 (tinypath-expand-file-name x))))
     lisp-roots
     "\\|")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup-parse-data (data)
  "Parse external tool's DATA structure."
  (let ((lisp-lookup (tinypath-external-load-path-lookup-regexp))
	correct-emacs
	type)
    (when data
      (dolist (elt data)
	(setq type (car elt))
	(dolist (path (cdr elt))
	  ;; 'cache element is not a string.
	  (when (stringp path)
	    (setq correct-emacs
		  (tinypath-path-ok-this-emacs-p path)))
	  (cond
	   ((equal type 'cache)
	    ;; Not handled in this loop
	    (return))
	   ((and (equal type 'lisp-dir)
		 correct-emacs)
	    (tinypath-external-setup-1-load-path path lisp-lookup))
	   ((equal type 'man)
	    (tinypath-external-setup-1-man-path path))
	   ((equal type 'c-src-dir)
	    (pushnew path
		     tinypath--extra-ff-search-directories
		     :test
		     'string=))
	   ((and (equal type 'bin)
		 correct-emacs)
	    (tinypath-exec-path-append path))
	   ((and (equal type 'info)
		 correct-emacs)
	    (tinypath-info-handler path)
	    (pushnew path
		     (tinypath-Info-default-directory-list)
		     :test
		     'string=)))))
      (tinypath-external-setup-cache data)) ;; When
    (tinypath-verbose-macro 3
      (message "TinyPath: EXT END tinypath-external-setup %s"
	       (if data
		   "[DATA OK]"
		 "[DATA NOK]")))
    data))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-external-setup ()
  "Use external tool to help setup emacs.
See `tinypath-external-helper-main'."
  (and
   (setq tinypath--external-data-structure
	 (tinypath-external-helper-main
	  (tinypath-external-setup-1-main)))
   (tinypath-external-setup-parse-data
    tinypath--external-data-structure)))

;;}}}
;;{{{ Cache

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-cache-elt-fullpath (elt)
  "Return full path t package from cache ELT."
  ;; ("sgml-mode.el" (5359 . "d:/emacs-21.3/lisp/textmodes/")
  (concat (cdr (nth 1 elt))
	  (car-safe elt)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-cache-elt-package (elt)
  "Return package name from cache ELT."
  (car-safe elt))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-initialize ()
  "Set internal extension cache."
  (put 'tinypath-cache-p-1
       'extension-cache
       (tinypath-file-extension-list-choices)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-extensions (package)
  "Return list of extensions for PACKAGE."
  (unless (get 'tinypath-cache-p-1 'extension-cache)
    (tinypath-cache-p-1-initialize))
  (if (string-match "\\.elc?$" package)
      (assoc (match-string 0 package)
	     (get 'tinypath-cache-p-1
		  'extension-cache))
    (cdr (assq nil
	       (get 'tinypath-cache-p-1
		    'extension-cache)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-new-cache-lookup
  (package choices &optional regexp)
  "Search PACKAGE and CHOICES from `tinypath--cache'.
Input:

  PACKAGE   vt100
  CHOICES   '(\"vt100\" \".el.gz\" \".el\" ...)
  REGEXP    If string, ignore files matching this regexp. E.g. '\.elc'."
  (let ((fid  "tinypath-cache-p-1-new-cache-lookup")
	(file package)
	try
	ret)
    ;; Remove extension
    (when (string-match "^\\(.*\\)\\(\\.elc?\\)$" package)
      (setq file (match-string 1 package))
      (tinypath-verbose-macro 10
	(message "%s REMOVE EXTENSION %s" fid package)))

    (dolist (elt choices)
      (tinypath-verbose-macro 10
	(message "%s trying... %s"  fid (concat file elt)))
      (setq try (concat file elt))
      (when (and (or (null regexp)
		     (not (string-match regexp try)))
		 (setq elt (assoc try tinypath--cache)))
	(tinypath-verbose-macro 10
	  (message "%s ASSOC %s" fid (prin1-to-string elt)))
	(setq ret elt)
	(return)))
    ret))

;;; ----------------------------------------------------------------------
;;; There used to be function `tinypath-cache-p-1-old' which
;;; was first implementation and the new function was developed while
;;; the "old" was trusted version.
;;;
(defun tinypath-cache-p-1-new-cache (package &optional no-special)
  "Check if PACKAGE is in tinypath--cache. Return PATH or nil.
If package contains absolute directory part, return PACKAGE.

The search order for unidentified package is:
'(\".elc\" \".elc.bz2\" \".elc.gz\" \".el\" \".el.bz2\" \".el.gz\")

Input:

  PACKAGE       file to find from cache.
  NO-SPECIAL    There is special handling for jka-compr which is never
		checked for compressed file. Non-nil bypasses special
		case handling.

Return:

  '(PATH  CACHE-ELEMENT)"
  (when tinypath--cache
    (let* ((fid  "TinyPath: tinypath-cache-p-1-new-cache ")
	   (regexp1  tinypath--ignore-file-regexp)
	   ;;  These files are banned, although they were put to
	   ;;  load-path or cache. Gnus version is one good example:
	   ;;  The original Gnus from Emacs installation is not used
	   ;;  if there is newer Gnus found.
	   (regexp2  tinypath--load-path-ignore-regexp)
	   ;; (dir      (file-name-directory package))
	   (choices  (tinypath-cache-p-1-extensions package))
	   elt
	   ret)
      (tinypath-verbose-macro 10
	(message (concat fid
			 " CHOICES "
			 (prin1-to-string choices))))
      (setq
       ret
       (catch 'done
	 (flet (                 ;; First function
		(path-name (ELT) ;; ELT = '("FILE.EL" (POS . "PATH"))
			   (when ELT
			     (concat (cdr (nth 1 ELT)) (car ELT)  )))
		;; Second function
		(throw-ignore
		 (ELT)
		 (cond
		  ((and ELT
			(or (and (stringp regexp1)
				 (string-match regexp1
					       (car ELT)))
			    (and (stringp regexp2)
				 (let (case-fold-search)
				   (string-match regexp2
						 (cdr (nth 1 ELT)))))))
		   (tinypath-verbose-macro 10
		     (message "%s`ignore-file-regexp' %s"
			      fid
			      (car ELT)))
		   nil)
		  (ELT
		   (throw 'done (path-name ELT))))))
	   (tinypath-verbose-macro 10
	     (message (concat fid " ENTRY %s %s")
		      package
		      (prin1-to-string choices)))
	   (when (setq elt (assoc package tinypath--cache))
	     (tinypath-verbose-macro 10
	       (message (concat fid "DIRECT HIT %s") package))
	     (throw-ignore elt))
	   ;; .................................................. search ...
	   (cond
	    ((and (null no-special)
		  (string-match "jka-compr" package))
	     ;; XEmacs 20.4  installs files under
	     ;; /usr/lib/xemacs-20.4/lisp and all the lisp file sources
	     ;; are in compressed format. This means, that we cannot load
	     ;; jka-compr.el.gz initially.
	     ;;
	     ;; This situation is evident if user has disabled the .elc
	     ;;  loading with tinypath--ignore-file-regexp
	     (setq regexp1 nil)
	     (tinypath-verbose-macro 10
	       (message (concat fid "SPECIAL CASE %s") package))
	     (setq elt
		   (or (and (not (string-match "\\.el$" package))
			    (assoc "jka-compr.elc" tinypath--cache))
		       (assoc "jka-compr.el" tinypath--cache)
		       (let ((cache
			      (tinypath-load-path-locate-library
			       "jka-compr.el")))
			 (when cache ;;  Make it look like CACHE entry
			   (list "jka-compr.el"
				 (cons 1 (file-name-directory
					  cache)))))))
	     (unless elt
	       (error "TinyPath: (cache-p-1) FATAL, can't find %s"
		      package))
	     (throw 'done (path-name elt)))
	    ;; .......................................... regular files ...
	    ((not (string-match "\\.\\(g?z\\|bz2\\)$" package))
	     (throw-ignore (setq elt (tinypath-cache-p-1-new-cache-lookup
				      package choices regexp1))))))))
      (tinypath-verbose-macro 9
	(message "TinyPath: cache hit: %s [%s] %s"
		 package
		 (or ret "")
		 (prin1-to-string elt)))
      (list ret elt))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-1-new (package &optional no-special)
  "Check if PACKAGE is in tinypath--cache. Return PATH or nil.
See `tinypath-cache-p-1-new-cache'.

Paths with directory component are changed to plain PACKAGE
searches. Like if searching:

   term/vt100

This is converted into search:

   vt100"
  ;; Do not search absolute paths
  (let ((fid "tinypath-cache-p-1-new "))
    (tinypath-verbose-macro 10
      (message "%s Searching for... %s" fid package))
    (cond
     ((not (stringp package))
      (list nil nil))
     ((string-match "^[/\\~]\\|^[A-Za-z]:" package)
      (list package nil))
     (t
      (when (file-name-directory package)
	(tinypath-verbose-macro 10
	  (message "%s %stry Searching plain PACKAGE.el" fid package))
	(setq package (file-name-nondirectory package)))
      (tinypath-cache-p-1-new-cache package no-special)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-cache-p-1 (package)
  "Call correct cache implementation."
  (tinypath-cache-p-1-new package))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p-2 (package)
  "Check if PACKAGE is in `tinypath--cache'. Return PATH or nil.
If PACKAGE contains a path name, return PACKAGE."
  (let  (list
	 level2
	 elt
	 elt2
	 ret)
    (cond
     ;;  Nothing to do, Linux or Win32 absolute path name
     ((string-match "^[/\\~]\\|^[A-Za-z]:" package)
      (setq ret package))
     ((file-name-directory package)
      ;;  look up "package" first, because it is most
      ;;  likely known to cache, only then "dir/package"
      (setq list
	    (list (file-name-nondirectory package)))
      ;; 2003-15-18 disabled looking term/vt100
      ;; because, it should be found from cache with
      ;; simple name "vt100".
      ;; package
      nil)
     (t
      (setq list (list package))))
    (dolist (file list)
      (setq elt    nil
	    elt2   nil
	    level2 nil)
      (cond
       ;; If level two cache exists, then check that the entry has not
       ;; been resolved before.
       ((and tinypath--cache-level-two
	     (setq elt2 (assoc file tinypath--cache-level-two))
	     (setq ret  (cdr elt)))
	(setq level2 t))
       (t
	(and (setq elt (tinypath-cache-p-1 file))
	     (setq ret (car elt)))))
      ;;  Did cache hold the information?
      (cond
       ((null ret))
       ((and (stringp ret)
	     (file-exists-p ret))
	(unless level2
	  ;; This was not in level 2, put it these
	  (push (cons package ret) tinypath--cache-level-two))
	(return))
       (ret
	;;  Invalid cache entry, file does not exist any more.
	(tinypath-verbose-macro 3
	  (tinypath-cache-warn-if-not-exist ret))
	;;  Remove from both caches
	(when elt
	  (setq tinypath--cache (delq (cadr elt) tinypath--cache)))
	(when elt2
	  (setq tinypath--cache-level-two
		(delq elt2 tinypath--cache-level-two))))))
    (when (null ret)
      ;;  Do full scan.
      (setq ret (tinypath-load-path-locate-library package))
      (when (and ret
		 (file-exists-p ret))
	;;  Mark tesese entries with "zero" position: They have
	;;  been found later on while Emacs is running.
	(push (cons package ret) tinypath--cache-level-two)
	(push (list (file-name-nondirectory ret)
		    (cons 0  (file-name-directory ret)))
	      tinypath--cache)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-p (package)
  "Check if PACKAGE is in tinypath--cache. Return PATH or nil.
If package contains absolute directory part, return PACKAGE."
  (if (string-match "^[~/\\]" package)
      ;; Any absolute load paths are ignored by CACHE and returned
      ;;  as is, so ignore references like ~/.emacs
      package
    (tinypath-cache-p-2 package)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-cache-p-for-advice (file)
  "If load-path and cache are the same, return cache lookup for FILE.
This code is used in adviced function."
  (if (tinypath-load-copy-equal-p)
      (tinypath-cache-p file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-match-package (regexp &optional flag)
  "Return cache elements whose package names match REGEXP.
If FLAG is non-nil, return package names, not cache elements."
  (let (list
	name)
    (dolist (elt tinypath--cache)
      (setq name (tinypath-cache-elt-package elt))
      (cond
       ((not (stringp name))
	(message "TinyPath: [ERROR] invalid cache entry: %s"
		 (prin1-to-string elt)))
       ((string-match regexp name)
	(push (if flag
		  name
		elt)
	      list))))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-match-fullpath (regexp &optional flag)
  "Return cache elements whose full path match REGEXP.
If FLAG is non-nil, return package names, not cache elements."
  (let (list
	name)
    (dolist (elt tinypath--cache)
      (setq name (tinypath-cache-elt-fullpath elt))
      (cond
       ((not (stringp name))
	(message "TinyPath: [ERROR] invalid cache entry: %s"
		 (prin1-to-string elt)))
       ((string-match regexp name)
	(push (if flag
		  name
		elt)
	      list))))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-hostname ()
  "Return `system-name'."
  (downcase
   (or (or (getenv "HOST")              ;Unix
	   (getenv "COMPUTERNAME"))     ;Win32
       "unknownhost")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-name ()
  "Return Emacs version specific cache file.

References:

  `tinypath--cache-file-prefix'.
  `tinypath--cache-file-postfix'"
  (let* (host
	 (type (if tinypath--xemacs-p
		   "xemacs"
		 "emacs"))
	 (list (tinypath-emacs-versions))
	 (ver  (or (nth 1 list)
		   (nth 0 list)))
	 (win32  (if tinypath--win32-p
		     "win32-"
		   ""))
	 (cygwin (if tinypath--win32-cygwin-p
		     "cygwin-"
		   ""))
	 (host-func tinypath--cache-file-hostname-function)
	 ret)
    (when (and host-func
	       (functionp host-func))
      (let (ret)
	(setq ret (funcall host-func))
	(tinypath-verbose-macro 3
	  (message "TinyPath: CACHE file host function returned %s"
		   (or ret "nil")))
	(if (stringp ret)
	    (setq host ret))))
    (setq ret
	  (concat tinypath--cache-file-prefix
		  "-"
		  win32
		  cygwin
		  (if (stringp host)
		      (concat host "-")
		    "")
		  type
		  "-"
		  ver
		  tinypath--cache-file-postfix))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-name-compiled-p (file)
  "Check if FILE matches \"\\\\.elc$\". Return non-compiled FILE."
  (when (string-match "\\(^.+\\.el\\)c$" file)
    (match-string 1 file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-name-all ()
  "Return list of cache files.
If `tinypath--cache-file-postfix' is `\.elc', then return both
compiled and non-compiled files."
  (let* ((file      (tinypath-cache-file-name))
	 (el        (tinypath-cache-file-name-compiled-p file)))
    (if el
	(list file el)
      (list file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-delete ()
  "Delete cache file(s) from disk, if they exist."
  (dolist (file (tinypath-cache-file-name-all))
    (when (file-exists-p file)
      (delete-file file)
      (tinypath-verbose-macro 5
			      (message "TinyPath: Cache deleted: %s" file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-old-p (file)
  "Return non-nil if FILE exists and is too old.
References:
  `tinypath--cache-expiry-days'."
  (when (and (file-exists-p file)
	     (integerp tinypath--cache-expiry-days))
    (let ((days (tinypath-days-old file)))
      (when (> days tinypath--cache-expiry-days)
	(tinypath-verbose-macro 2
	  (message "TinyPath: Cache is too old: %s days" days))
	t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-write (file)
  "Write state information to FILE."
;;;   (interactive "FFile to save cache: ")
  (let* ((bytecomp  (tinypath-cache-file-name-compiled-p file))
	 (write     (or bytecomp file)))
    (tinypath-verbose-macro 2
      (message "TinyPath: Saving cache to %s" write))
    (tinypath-ti::write-file-variable-state
     write
     (concat "Emacs load-path settings.\n"
	     ";; This file is automatically generated. Do not touch.\n"
	     ";; See tinypath.el and M-x tinypath-cache-regenerate.\n")
     (list
      'load-path
      'exec-path
      'tinypath--extra-manpath
      'tinypath--extra-path-root
      'tinypath--extra-ff-search-directories
      (if (boundp 'Info-directory-list) ;; XEmacs
	  'Info-directory-list
	'Info-default-directory-list)
      'tinypath--cache)
     'no-pp-print 'no-backup)
    ;;  Only if name ends to "\.elc"
    (if bytecomp
	(tinypath-byte-compile-file bytecomp))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-save ()
  "Save cache file."
  (tinypath-cache-file-write (tinypath-cache-file-name)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-load ()
  "Load cache."
  (let (stat)
    (dolist (file (tinypath-cache-file-name-all))
      (setq stat (file-exists-p file))
      (tinypath-verbose-macro 2
	(message "TinyPath: %sLoading cache file %s"
		 (if stat
		     ""
		   "[ERROR] ")
		 file))
      (when stat
	(load file)
	(return)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-find-file ()
  "Display cache by calling `find-file'."
  (interactive)
  (let ((file (tinypath-cache-file-name)))
    (tinypath-verbose-macro 2
      (message "TinyPath: find-file cache %s" file))
    (find-file file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-file-need-sync-p ()
  "Load cache. If cache needs synchronization, return non-nil."
  (let (ret
	found)
    ;;  Using a simple variable is faster than
    ;;  checking (if load-path   , because load-path may be very big
    ;;
    (if load-path
	(setq found t))
    (unless found
      (setq ret 'cache-file-content-error)
      (message "TinyPath: [ERROR] CACHE; empty load-path"))
    (unless tinypath--cache
      (setq ret 'cache-file-content-error)
      (message "TinyPath: [ERROR] CACHE; empty tinypath--cache in"))
    ;;  Make sure that read cache is in synch with
    ;;  the `load-path'. If not, force rescanning.
    (when (and found
	       (tinypath-load-path-not-in-synch-p 'fast-check))
      (setq ret 'need-sync))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-display (&optional insert)
  "Display `tinypath:-cache' and `tinypath--cache-level-two'.
Optionally INSERT."
  (interactive "P")
  (if tinypath--cache-level-two
      (tinypath-list-display "tinypath--cache-level-two %s"
			     tinypath--cache-level-two insert)
    (message "tinypath--cache-level-two is empty, nothing to display."))
  (tinypath-list-display "tinypath--cache %s"
			 tinypath--cache insert))

;;}}}
;;{{{ Info files

(defconst tinypath--info-file-basic-contents
  (concat
   "This is the file .../info/dir, which contains the\n"
   "topmost node of the Info hierarchy, called (dir)Top.\n"
   "The first time you invoke Info you start off looking at this node.\n"
   "\n"
   "File: dir  Node: Top\tThis is the top of the INFO tree\n"
   "\n"
   "  This (the Directory node) gives a menu of major topics.\n"
   "  Typing \"q\" exits, \"?\" lists all Info commands, \"d\" returns here,\n"
   "  \"h\" gives a primer for first-timers,\n"
   "  \"mEmacs<Return>\" visits the Emacs manual, etc.\n"
   "\n"
   "  In Emacs, you can click mouse button 2 on a menu item or cross reference\n"
   "  to select it.\n"
   "\n"
   "* Menu:\n\n")
  "*This variable includes a basic `dir' file for Emacs info.
Do not change.")

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-display (&optional insert)
  "Display info path contents. Optionally INSERT.
This would be `Info-directory-list' in XEmacs and
`Info-default-directory-list' in Emacs."
  (interactive "P")
  (tinypath-list-display
   (concat (if tinypath--xemacs-p
	       "Info-directory-list"
	     "Info-default-directory-list")
	   " %s")
   (tinypath-Info-default-directory-list)
   insert))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-Info-default-directory-list-clean ()
  "Clean `Info-default-directory-list'.
Remove any suspicious elements: non-directories, non-strings."
  (set (tinypath-Info-default-directory-list-sym)
       (tinypath-directory-list-clean
	(tinypath-Info-default-directory-list)
	"Info-directory-list")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-write-region (beg end file)
  "Write region BEG END to FILE and ignore errors, but print message."
  (condition-case err
      (write-region (point-min) (point-max) file)
    (error
     (tinypath-verbose-macro 3
       (message "TinyPath: [INFO] No permission to write %s %s"
		(or file "<nil>")  (prin1-to-string err))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-files-in-directory (dir)
  "Return all info files in DIR.
The list is composed of capitalized names of the found files:

    tar.info       --> Tar
    fileutils.info --> Fileutils

Returned list in the above case is '(\"Tar\" \"Fileutils\")."
  ;;  Cache this value only once and reuse as needed.
  (unless (get 'tinypath-info-files-in-directory
	       'compress-extensions)
    (put 'tinypath-info-files-in-directory
	 'compress-extensions
	 (tinypath-file-extension-compressed)))
  (let ((files      (directory-files dir))
	(extensions (cons "" (get 'tinypath-info-files-in-directory
				  'compress-extensions)))
	ret)
    (dolist (file files)
      (when (catch 'exit
	      (dolist (ext extensions)
		;;  NOTE:  Can't use \\| in here, because posix match engine
		;;  tries all possibilities and we want to stop after first
		;;  matched regexp.
		;;
		;;  File Examples:
		;;
		;;    cc-mode-1
		;;    eshell.info
		;;
		(dolist (re '("^\\(.*\\)\\.info-1"
			      "^\\(.*\\)\\.info"
			      "^\\(.*\\)-1"))
		  (setq re (concat re ext "$"))
		  (when (string-match re file)
		    (throw 'exit file)))))
	(pushnew (capitalize (downcase (match-string 1 file)))
		 ret
		 :test 'string=)))
    (sort ret 'string-lessp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-directory-entry-p (entry)
  "Search for info ENTRY."
  (let ((point (point)) ;; Faster than using save-excursion.
	ret)
    (goto-char (point-min))
    ;;  This check relies on using the same ENTRY for filename
    ;;
    ;;      * Oview: (Overview).
    ;;
    ;;  But what if user manually edit's the file and makes it read:
					;:
    ;;      * Exim Oview: (Overview).
    ;;
    ;;  Ok, handle that too, but require thet "Oview" is still there.
    (when (and (goto-char (point-min))
	       (re-search-forward
		(format "^[*]\\([ \t]+[^ \t\r\n]+\\)?[ \t]+%s:[ \t]+"
			entry)
		nil t)
	       (setq ret (point))))
    (goto-char point) ;; Restore point
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-directory-contents-update
  (file &optional verb interactive info-files)
  "Update the central `dir' with all new info files in directory.
Give the absolute path to the `dir' and the directory is scanned
for new entries which are updated to file `dir'.

Input:

  FILE         The `dir' file location
  VERB         Allow printing verbose messages
  INTERACTIVE  Leave the buffer in Emacs for editing.
  INFO-FILES   Info files in directory, like \"Eieio\"

Return:

  t   if any changes made."

  ;;  (interactive "FInfo file named `dir', location: ")

  (when (file-directory-p file)
    (error "You must give a filename"))
  (let ((buffer (find-file-noselect file))
	done
	buffer-file)
    (with-current-buffer buffer
      ;;  If we read /usr/local/info and we're not root, then
      ;;  this buffer will be read only. Make it writable. The
      ;;  save error is handled elsewhere.
      ;;
      (setq buffer-read-only nil)
      (tinypath-verbose-macro 1
	(message "TinyPath: [INFO] found %s" file))
      (let ((entries (or info-files
			 (tinypath-info-files-in-directory
			  (file-name-directory file)))))
	(dolist (entry entries)
	  (unless (tinypath-info-directory-entry-p entry)
	    (goto-char (point-max))
	    (unless (looking-at "^[\n\t ]*$")
	      (insert "\n"))
	    (insert (format "* %s: (%s).\n" entry entry))
	    (tinypath-verbose-macro 2
	      (message "TinyPath: [INFO] added entry `%s' to file %s"
		       entry file))
	    (setq done t)
	    (set-buffer-modified-p nil) ;; do not ask user  when killing buffer
	    (setq buffer-file (buffer-file-name))))) ;; let*
      (if (interactive-p)
	  (when done
	    (message "TinyPath: [INFO] Edit and verify changes at %s" file))
	(when (and done buffer-file)
	  (tinypath-write-region (point-min) (point-max) buffer-file))
	(when (buffer-live-p buffer)
	  (kill-buffer buffer)))) ;; With-current
    done))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-file-DIR (path)
  "Make `dir' file name using PATH."
  (concat (file-name-as-directory path) "dir"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-handler-DIR (dir)
  "Handle creating/updating central info file DIR `dir' to current directory."
  (let ((dir-file (tinypath-info-file-DIR dir)))
    (unless (file-exists-p dir-file)  ;No central dir, generate one...
      (tinypath-verbose-macro 3
	(message "TinyPath: [INFO] missing central `dir' generating %s"
		 dir-file))
      (tinypath-with-temp-buffer
       (insert tinypath--info-file-basic-contents)
       (insert " "
	       (tinypath-expand-file-name dir)
	       "\n")
       ;;  maybe we don't have permission to write to this directory?
       (tinypath-write-region (point-min) (point-max) dir-file)
       t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-handler (dir)
  "Check if DIR contains info files and a special `dir' file.
This function will create `dir' file if it does not exist,
update `Info-default-directory-list' and add any new INFO entries in
DIR to central `dir' file in that directory.

Please suggest to the lisp package maintainer that he
should ship with default `dir' in next release so that it
could be automatically used.

Return

  t   if any changes made."
  (interactive "fGive directory with info files: ")
  ;;  If user calls us, make sure new files are also noticed.
  ;;
  (if (interactive-p)
      (tinypath-info-initialize))
  (let ((list     (tinypath-info-files-in-directory dir))
	(dir-file (concat (file-name-as-directory dir) "dir"))
	cleanup
	done)
    (when (and (null list)
	       (interactive-p))
      (message "Tinypath: No info file candidates in %s" dir))
    (when list                          ;info files in this directory?
      (setq done (tinypath-info-handler-DIR dir))
      (tinypath-info-directory-contents-update
       dir-file
       (interactive-p)
       (interactive-p)
       list)
      (tinypath-verbose-macro 2
	(message "TinyPath: [INFO] PUSH maybe => %s"
		 dir))
      (tinypath-verbose-macro 5
	(message
	 "TinyPath: [INFO] PUSH (before) Info-default-directory-list: %s"
	 (prin1-to-string (tinypath-Info-default-directory-list))))
      ;;  Always add found directories to the list.
      ;;  Notice, that directory may contain trailing slash, that's why
      ;;  two `member' tests
      ;;
      ;;   ../info
      ;;   ../info/
      ;;
      (let* ((dir1 (file-name-as-directory dir))         ;; with slash
	     (dir2 (substring dir 0 (1- (length dir1)))) ;; without
	     (list (tinypath-Info-default-directory-list)))
	(unless (or (member dir1 list)
		    (member dir2 list))
	  (tinypath-verbose-macro 2
	    (message
	     "TinyPath: [INFO] PUSH Info-default-directory-list => %s" dir2))
	  (setq cleanup t)
	  ;;  This is efectively "(push dir2 <info-list>)"
	  (set (tinypath-Info-default-directory-list-sym)
	       (cons dir2 (tinypath-Info-default-directory-list)))
	  (tinypath-verbose-macro 5
	    (message
	     "TinyPath: [INFO] PUSH (after) Info-default-directory-list: %s"
	     (prin1-to-string (tinypath-Info-default-directory-list))))))
      ;;  Kill all previous info files from Emacs, so that next info
      ;;  C-h i will force Emacs to regenerate found new entries.
      (when (or cleanup                 ;Added new directory
		(interactive-p))
	(tinypath-info-initialize)))
    done))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-kill-buffers ()
  "Kill all *info* buffers."
  ;;  - There may be hidden buffers that Emacs uses to gather
  ;;    all 'dir' files.
  ;;  - Kill also centeal buffer *info*
  (dolist (buffer (buffer-list))
    (when (string-match "^ info\\|^\\*info" (or (buffer-name buffer) ""))
      (kill-buffer buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-initialize ()
  "Initialize info to pristine state.
After this function, the central `dir' creates all its parts from scratch
and not from cached directories."
  (interactive)
  (tinypath-Info-default-directory-list-clean)
  ;;  - This must be set to nil, because otherwise Info would not
  ;;    rescan new entries.
  (setq Info-dir-file-attributes nil)
  (tinypath-info-kill-buffers))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-info-scan-Info-default-directory-list (&optional list)
  "Examine and possibly fix LIST of dirs to `Info-default-directory-list'.
Without any arguments, checks `Info-default-directory-list'
and `tinypath--Info-default-directory-list'.

If there were any new entries or possibly new directory without
and root INFO file `dir', Emacs info cache cache is deleted and
existing *info* buffer if killed. You should run \\[info] to
regenerate all the info parts again.

Return

  t   if any changes made."
  (interactive)
  (let (seen
	done)
    (or list
	(setq list (append (tinypath-Info-default-directory-list)
			   tinypath--Info-default-directory-list)))
    (dolist (path list)
      (unless (member path seen)
	(push path seen)
	(when (file-directory-p path)
	  (when (tinypath-info-handler path)
	    (setq done t)))))
    (when (and done
	       (interactive-p))
      (tinypath-cache-file-save))
    (when done
      (tinypath-info-initialize))
    done))

;;}}}
;;{{{ Timing support

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-difference (a b)
  "Calculate difference between times A and B.
The input must be in form of '(current-time)'
The returned value is difference in seconds.
E.g., if you want to calculate days; you'd do

\(/ (tinypath-time-difference a b) 86400)  ;; 60sec * 60min * 24h"
  (let ((hi  (- (car a) (car b)))
	(lo  (- (car (cdr a)) (car (cdr b))))
	(mic (- (car (cddr a)) (car (cddr b)))))
    (+
     (+ (lsh hi 16) lo)
     (/ mic 1000000))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-results (buffer)
  "Write load time results to BUFFER. Return buffer pointer."
  (let (time
	min
	sec)
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (dolist (elt tinypath--time-data)
	(setq time (cdr elt)
	      min  (/ time 60)
	      sec  (- time (* min 60)))
	(insert
	 (format "%-20s %d  %dmin %dsec\n"
		 (car elt)
		 time
		 min
		 sec)))
      (current-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-display ()
  "Display timing information of each package loaded."
  (interactive)
  (display-buffer (tinypath-time-results tinypath--time-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-time-record (package start-time)
  "Record load time of PACKAGE, when START-TIME is known."
  (when  (stringp package)
    (let* ((stop-time (current-time))
	   (file (file-name-nondirectory package))
	   (name (if (string-match "^.*\\(.*\\)\\.elc$" file)
		     (match-string 1 file)
		   file))
	   (diff (tinypath-time-difference stop-time start-time)))
      (if tinypath--verbose-timing
	  (message "TinyPath: load time %s %dsec" name diff)
	(tinypath-verbose-macro 9
	  (message "TinyPath: load time %s %dsec" name diff)))
      (aput 'tinypath--time-data name diff))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypath-time-macro 'lisp-indent-function 1)
(put 'tinypath-time-macro 'edebug-form-spec '(body))
(defmacro tinypath-time-macro (package &rest body)
  "Record PACKAGE timing to `tinypath--time-data' and run BODY."
  `(let ((start-time (current-time)))
     (prog1
	 (progn ,@body)
       (tinypath-time-record ,package start-time))))

;;}}}
;;{{{ exec-path

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-from-path ()
  "Parse environment variable PATH."
  (let ((path   (getenv "PATH"))
	(regexp (concat "[^" path-separator "]+"))
	list)
    (when path
      (tinypath-with-temp-buffer
       (insert path)
       (goto-char (point-min))
       (while (re-search-forward regexp nil t)
	 (push (match-string 0) list))))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-append (path)
  "Add PATH to `exec-path'.
Add new PATH to the end, so that user's paths take precedence.
Ignore path if it matches `tinypath--exec-path-ignore-regexp'."
  ;;  expand - Otherwise `member' would not do much good (duplicates)
  (setq path (tinypath-expand-file-name path))
  (unless (member path exec-path)
    (if (and (stringp tinypath--exec-path-ignore-regexp)
	     (string-match
	      tinypath--exec-path-ignore-regexp
	      path))
	(tinypath-verbose-macro 3
	  (message "\
TinyPath: PATH ignored by tinypath--exec-path-ignore-regexp %s" path))
      (setq exec-path (append exec-path (list path))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-check ()
  "Check if `exec-path' lack any directory as in PATH.
Return missing paths."
  (let* ( ;; (tinypath-directory-list-clean exec-path "exec-path"))
	 (exec  exec-path)
	 (PATH  (tinypath-directory-list-clean
		 (tinypath-exec-path-from-path)
		 "PATH"))
	 missing)
    (dolist (path PATH)
      (unless (or (member path exec)
		  ;;  With trailing slash
		  (member (file-name-as-directory path) exec))
	(push path missing)))
    (nreverse missing)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-check-verbose (&optional fix)
  "Print messages if `exec-path' lacks any directory found in PATH.
Optionally FIX by adding missing directories to the end."
  (interactive)
  (let ((missing (tinypath-exec-path-check)))
    (when missing
      (dolist (path missing)
	(message "TinyPath: PATH check. `exec-path' does not include %s%s"
		 path
		 (if fix
		     " [fixed]"
		   ""))
	(when fix
	  (tinypath-exec-path-append path))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-check-verbose-fix ()
  "Call `tinypath-exec-path-check-verbose' with argument 'fix."
  (tinypath-exec-path-check-verbose 'fix))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-clean ()
  "Clean `exec-path' for anything suspicious: non-directories, non-strings."
  (tinypath-verbose-macro 5
    (message "TinyPath: tinypath-exec-path-clean."))
  (setq exec-path (tinypath-directory-list-clean exec-path "exec-path")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-exec-path-display (&optional insert)
  "Display `exec-path' by messaging' it. Optionally INSERT."
  (interactive "P")
  (tinypath-list-display "exec-path %s" exec-path insert))

;;}}}
;;{{{ load-path

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-emacs-distribution-p (path)
  "Return non-nil if PATH is from Emacs distribution."
  (string-match
   (concat
    "[/\\]x?emacs[/\\][0-9]+[0-9.]+[/\\]" ;; Unix  emacs/20.7/
    "\\|[/\\]x?emacs-[0-9]+[0-9.]+[/\\]") ;; win32 emacs-20.7/
   path))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-personal-p (path)
  "Return non-nil if PATH is under $HOME"
  (string-match
   (regexp-quote (expand-file-name "~"))
   (expand-file-name path)))

;;; ----------------------------------------------------------------------
;;; (tinypath-load-path-search "gnus.el")
;;;
(defun tinypath-load-path-search (package &optional all include-all)
  "Search `load-path' for PACKAGE and optioanlly ALL occurrances.
This is the last resort if cache fails.

INCLUDE-ALL says that tinypath--load-path-ignore-regexp'
is not used.

Return

  path          Absolute path location
  '(path ..)    If option ALL was set."
  (unless (get 'tinypath-cache-p-1 'extension-cache)
    (tinypath-cache-p-1-initialize))
  (let (case-fold-search ;; Case sensitive match.
	file
	ret)
    (tinypath-verbose-macro 5
      (message
       (concat
	"TinyPath: [WARNING] Performance problem; `%s' caused "
	"full load-path scan.")
       package))
    (dolist (dir load-path)
      (when (and (stringp dir)
		 (file-directory-p dir)
		 (or include-all
		     (null tinypath--load-path-ignore-regexp)
		     (not (string-match
			   tinypath--load-path-ignore-regexp
			   dir))))
	(let* ((try (if  (string-match "\\.elc?$" package)
			(file-name-sans-extension package)
		      package))
	       (choices (tinypath-cache-p-1-extensions package))
	       (files   (directory-files
			 dir
			 nil
			 (concat "^"
				 (regexp-quote try)
				 "\\("
				 (mapconcat
				  ;;  "\\.el\\|\\.el\\.gz\\|..."  etc.
				  (function
				   (lambda (x)
				     (regexp-quote x)))
				  choices
				  "\\|")
				 "\\)$"))))
	  (cond
	   ((eq 1 (length files))
	    (setq file (concat
			(file-name-as-directory
			 (expand-file-name dir))
			(car files)))
	    (if all
		(push file ret)
	      (return (setq ret file))))
	   (t
	    ;;  Multiple matches. Hm #todo.
	    nil)))))
    ;;  Retain order how files were encountered.
    (if (listp ret)
	(nreverse ret)
      ret)))

;;; ----------------------------------------------------------------------
;;; (tinypath-load-path-locate-library "cperl-mode")
;;;
(defun tinypath-load-path-locate-library (package)
  "Locate PACKAGE along `load-path'.

References:

  `tinypath--load-path-accept-criteria'."
  (let* ((criteria      tinypath--load-path-accept-criteria)
	 (list          (tinypath-load-path-search
			 package criteria))

	 ;;  LIST can be '(path path ...) if ALL-MATCHES is activated.
	 ;;  otherwise the returned value is absolute path name.
	 (ret  (if (listp list)
		   (car-safe list)
		 list)))
    (cond
     ((or (null ret)                    ;Not found. Do nothing
	  (stringp list)               ;Did not search all directories
	  (eq (length ret) 1)))       ;Only one match, RET already set
     ((functionp criteria)
      (setq ret (funcall criteria list)))
     (criteria
      ;;  Third party package overrides Emacs installation
      (let* ((ver  (car-safe (tinypath-emacs-versions 'noerr)))
	     (home (ignore-errors (expand-file-name "~")))
	     home-list
	     emacs-list
	     other-list)
	(dolist (path list)
	  (cond
	   ((tinypath-emacs-core-path-p path ver)
	    (push path emacs-list))
	   ((and home
		 (string-match home path))
	    (push path home-list))
	   (t
	    (push path other-list))))
	;;  Now sort out the correct package
	;;  1) User comes first
	;;  2) non-Emacs installation
	;;  3) Emacs installation
	(setq ret (or (and home-list
			   (car (nreverse home-list)))
		      (and other-list
			   (car (nreverse other-list)))
		      (and emacs-list
			   (car (nreverse emacs-list))))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-display (&optional insert)
  "Display `load-path' by messaging' it. Optionally INSERT."
  (interactive "P")
  (tinypath-list-display "load-path %s" load-path insert))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-not-in-synch-p (&optional fast)
  "Check that load-path directories exists.

Input:

  FAST   If non-nil, stop at first non-existing directory.

Return:

  List of directories that do not exist."
  (let (list)
    (dolist (path load-path)
      (when (and (stringp path)
		 (not (file-directory-p path)))
	(push path list)
	(if fast
	    (return))))
    (tinypath-verbose-macro 3
      (message "TinyPath: CACHE SYNC error status is => [%s]"
	       (prin1-to-string list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-clean (&optional extensive-test)
  "Clean `load-path' for anything suspicious: non-directories, non-strings.

If EXTENSIVE-TEST flag is non-nil, remove any paths that do not contain
lisp code. With it, the check will spend much more time."
  (tinypath-verbose-macro 3
    (message "TinyPath: CLEAN load-path"))
  (setq load-path (tinypath-directory-list-clean load-path "load-path"))
  (let (list)
    (when extensive-test
      (dolist (path load-path)
	(when (and (tinypath-path-ok-p path)
		   (tinypath-directory-lisp-p path)))
	(push path list))
      (setq load-path (nreverse list))))
  load-path)

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-reorder ()
  "Move Emacs paths to predefined order.
- User paths at the beginning (HOME dir paths)
- Next anything in any order (site-lisp)
- Last core Emacs paths."
  (let (personal
	emacs
	other)
    (dolist (path load-path)
      (cond
       ((tinypath-load-path-emacs-distribution-p path)
	(push path emacs))
       ((tinypath-load-path-personal-p path)
	(push path personal))
       (t
	(push path other))))
    (setq load-path
	  (append
	   (nreverse personal)
	   (append
	    (nreverse other)
	    (nreverse emacs))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-add-directory-one (path)
  "Add one PATH to the `load-path'. Old entry is removed."
  ;;  remove previous entry
  (if (null (tinypath-directory-lisp-p path))
      (tinypath-verbose-macro 3
	(message "TinyPath: Add ignored. No LISP files in %s" path))
    (if (member path load-path)
	(setq load-path (delete path load-path)))
    (pushnew
     (if tinypath--win32-p
	 (downcase path)
       path)
     load-path
     :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-add-directory-many (list)
  "Add to `load-path' each directory in LIST.
LIST can contains single elements or lists:
 '(single single (elt elt) single (elt elt)))"
  (dolist (elt list)
    (when elt
      (if (not (listp elt))
	  (setq elt (list elt)))
      (dolist (path elt)
	(tinypath-add-directory-one path)))))

;;; ----------------------------------------------------------------------
;;; This function is recursive
;;;
(defun tinypath-add-directory-many-below-root-dir (root)
  "Add all directories below ROOT to `load-path'."
  (if (not (stringp root))
      (tinypath-verbose-macro 5
	(message "TinPath: Cannot add below root. Not a string: %s"
		 (prin1-to-string root)))
    (if (not (and (file-exists-p root)
		  (file-directory-p root)
		  (not (file-symlink-p root))))
	(tinypath-verbose-macro 3
	  (message "TinyPath: root does NOT exist: %s" root))
      (setq root (tinypath-expand-file-name root))
      (tinypath-verbose-macro 3
	(message "TinyPath: root %s" root))
      (let ((list (tinypath-subdirectory-list root)))
	(when (tinypath-path-ok-p root)
	  (tinypath-verbose-macro 5
	    (message "TinyPath: adding        %s" root))
	  (tinypath-info-handler root)
	  (tinypath-add-directory-one root))
	(dolist (path list)
	  (tinypath-verbose-macro 3
	    (message "TinyPath: recursing => %s" path))
	  (tinypath-add-directory-many-below-root-dir path)))  )))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove-old (regexp)
  "Remove all paths matching REGEXP from `load-path'"
  (setq load-path
	(remove-if
	 (function
	  (lambda (x)
	    (string-match regexp x)))
	 load-path)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove (regexp)
  "Remove any matching REGEXP from `load-path'.
Return t if removed something."
  (let ((spare load-path)
	list
	status)
    (dolist (elt load-path)
      (if (string-match regexp elt)
	  (setq status t)
	(push elt list)))
    (cond
     ((null list)
      (setq load-path spare)
      (tinypath-verbose-macro 3
	(message "TinyPath: FATAL regexp %s cleaned whole load-path."
		 regexp)))
     (t
      (setq load-path list)))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-remove-cache (regexp)
  "Remove any matching REGEXP from `tinypath--cache'.
Return t if removed something."
  (let ((spare tinypath--cache)
	 status)
    (dolist (elt tinypath--cache)
      (when (string-match regexp
			  ;;  ELT = '("file.el" (POS . "path"))
			  (cdr (nth 1 elt)))
	(setq status t)
	(setq tinypath--cache (delete elt tinypath--cache))))
    (unless tinypath--cache
      (setq tinypath--cache spare)
      (tinypath-verbose-macro 3
	(message "TinyPath: FATAL regexp %s cleaned whole tinypath--cache."
		 regexp)))
    status))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-setup ()
  "This is default function to add paths to `load-path'.
Add all paths below `tinypath--load-path-root'. See this variable.

References:

  `tinypath--load-path-function'"
  (let ((list tinypath--load-path-root))
    (if (stringp list) ;; make one string into LIST
	(setq list (list list)))
    ;;  This message is a little premature, but it cleaner here,
    ;;  than after the dolist loop
    (message
     "TinyPath: SETUP `tinypath--load-path-root' was checked and cleaned.")
    (dolist (elt list)
      (if (not (stringp elt))
	  (message "TinyPath: `tinypath--load-path-root' ELT `%s' \
is not a string. `tinypath--load-path-root': %s "
		   (prin1-to-string elt)
		   (prin1-to-string tinypath--load-path-root)))
      (tinypath-verbose-macro 2
	(message "TinyPath: => load path root %s " elt))
      (tinypath-add-directory-many-below-root-dir elt))))

;;;}}}
;;;{{{ Cache

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-directory-files (path-list)
  "Return all files along PATH-LIST."
  (let ((count        0)
	list)
    (dolist (dir path-list)
      (when (and (stringp dir)
		 (file-directory-p dir))
	;;   make sure directory has a slash at the end
	(setq dir (file-name-as-directory dir))
	;;  TRAD means "traditional Emacs Lisp way", because
	;;  there is new method EXT for "External tool" to do similar
	;;  caching. In fact if you see these messages, something
	;;  went wrong with the EXT method.
	(tinypath-verbose-macro 1
	  (message "TinyPath: TRAD Caching files... %d %s"
		   (length list)
		   dir))
	(dolist (file (directory-files dir nil "\\.elc?$"))
	  (unless (file-directory-p (concat dir file))
	    (incf count)
	    (when (or t ) ;; (string-match "other" dir))
	      (tinypath-verbose-macro 9
		(message "TinyPath: TRAD Cached %s"
			 (concat dir file))))
	    (push (list file (cons count
				   (tinypath-expand-file-name dir)))
		  list)))))
    ;; Preserve find order.
    ;; (nreverse list)
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-merge (list)
  "Merge LIST to `load-path'."
  ;;  Merge original path to loaded path
  (dolist (path list)
    (pushnew path load-path :test 'string=)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-clear ()
  "Clear cache variables.
You should call `tinypath-cache-setup-scan' after this function."
  (setq tinypath--cache nil)
  (setq tinypath--cache-level-two nil)
  (tinypath-load-path-clean))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-scan (&optional traditional)
  "Build the cache either by using external program or Emacs Lisp."
  (let ((external (not traditional))
	;;  While loading this package Cygwin XEmacs garbage collects like mad.
	;;  Ease it up for a while. This is 30Meg
	(gc-cons-threshold (* 1024 1024 30)))
    (or (and external
	     (tinypath-external-setup))
	(progn
	  (tinypath-verbose-macro 3
	    (message
	     (concat
	      "TinyPath: "
	      "TRAD lisp method used for scanning.")))
	  (tinypath-maybe-warn-message-log-max)
	  (tinypath-info-scan-Info-default-directory-list)
	  (funcall tinypath--load-path-function)
	  (setq tinypath--cache (tinypath-load-path-directory-files
				 load-path))))
    ;;  many push and pushnew were called.
    (when (fboundp 'garbage-collect)
      (message "TinyPath: cache-setup-scan requested `garbage-collect'")
      (garbage-collect))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-status-string ()
  "Return cache statistics."
  (format "TinyPath: packages %d, load-path %d, exec-path %d, info %d"
	  (length tinypath--cache)
	  (length load-path)
	  (length exec-path)
	  (length (tinypath-Info-default-directory-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-status-message ()
  "Print cache statistics."
  (interactive)
  (message (tinypath-cache-status-string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-main (&optional force traditional)
  "Set `load-path' possibly using cache.
If `tinypath--cache-file' is recent enough load it, otherwise
rescan directories if cache file is older than
`tinypath--cache-expiry-days'. After scan save cache.

Input:

  FORCE       Rescan and save cache.
  TRADITIONAL Use traditional Emacs lisp cache scan."
  (interactive "P")
  (let* ((file       (tinypath-cache-file-name))
	 (read-cache (and (null force)
			  (stringp file)
			  (file-exists-p file)
			  (null (tinypath-cache-file-old-p file))))
	 no-save)
    ;; .............................................. compressed cache ...
    (tinypath-use-compression-maybe file)
    ;; .................................................... load cache ...
    (when read-cache
      (let ((orig load-path))
	(tinypath-cache-file-load)
	(setq force (tinypath-cache-file-need-sync-p))
	(tinypath-load-path-merge orig)))
    ;; .......................................................... scan ...
    ;;  Clean everything before scan. This has two purposes
    ;;
    ;;  - Remove invalid entries
    ;;  - Expand all paths to use absolute names and forward slashes.
    ;;    Expand is needed because all tests are done using absolute paths:
    ;;    `member', `pushnew' etc. Emacs and XEmacs Win32 differences are
    ;;    also solved with expand.
    (when (null read-cache)
      (tinypath-load-path-clean)
      ;; (tinypath-Info-default-directory-list-clean)
      (tinypath-directory-list-clean
       tinypath--extra-path-root
       "tinypath--extra-path-root"))
    ;; .............................................. regenerate cache ...
    (when (or force
	      (null (file-exists-p file))
	      (null tinypath--cache))
      (setq force t) ;; Write cache too
      ;; Remove invalid entries so that they are not saved
      (tinypath-cache-setup-clear)
      ;; READ IT
      (tinypath-cache-setup-scan traditional)
      ;; Clean invalid entries
      (tinypath-directory-list-clean
       tinypath--extra-path-root
       "tinypath--extra-path-root")
      (tinypath-directory-list-clean
       tinypath--extra-manpath
       "tinypath--extra-manpath")
      (tinypath-load-path-clean)
      (tinypath-Info-default-directory-list-clean))
    (if (> (length exec-path) 100)
	(tinypath-verbose-macro 3
	  (message
	   "TinyPath: [WARNING] exec-path length looks suspicious: %d"
	   (length exec-path))))
    (tinypath-exec-path-clean)
    (tinypath-exec-path-check-verbose-fix) ;; Missing items? (from PATH)
    (unless load-path
      (tinypath-message-bug "FATAL SCAN load-path nil")
      ;;  Try rescue as best as we can, so that User's Emacs is still usable
      (message "TinyPath: FATAL trying to boot to restore load-path.")
      (tinypath-load-path-initial-value)
      (unless load-path
	(tinypath-message-bug
	 "[FATAL] SCAN2 load-path still nil, disable tinypath.el"))
      (setq no-save t))
    (when (or force
	      (null read-cache))
      ;; Cache has changed. See where is latest gnus
      (tinypath-load-path-reorder))
    ;;  Do this always, because:
    ;;  1. At Boot phase standard emacs-NN.N/lisp/gnus path is
    ;;     added
    ;;  2. There may be newer Gnus, which we know only after the
    ;;     cache has been loaded.
    ;;  => Last thing to do is to check various Gnus versions along
    ;;     load-path.
    (tinypath-insinuate-gnus)
    ;; ................................................... write cache ...
    (tinypath-load-copy-now) ;; Save load-path.
    (when (and (null no-save)
	       (or force
		   (and tinypath--cache-expiry-days ;cache allowed
			(null read-cache))))        ;but now expired
      (tinypath-cache-file-save))
    (tinypath-cache-status-message)
    ;; Make sure that this list is cleared. It must be
    ;; regenerated as well.
    (tinypath-emacs-lisp-file-list-cache-clear)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-setup-maybe ()
  "If `load-path' or `tinypath--cache' is out of date, rebuild cache."
  (when (or (tinypath-cache-non-existing-directory-list)
	    (tinypath-cache-non-existing-file-list))
    (tinypath-verbose-macro 2
			    (message "TinyPath: Cache validate: inconsistent state, rebuilding..."))
    (tinypath-cache-setup-main 'force)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-font-lock (&optional buffer)
  "Call `font-lock' with `tinypath--report-mode-font-lock-keywords' in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (font-lock-mode 1)
    (make-local-variable 'font-lock-keywords)
    (set 'font-lock-keywords tinypath--report-mode-font-lock-keywords)
    (font-lock-fontify-buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-different-size-p (elt)
  "Called by `tinypath-cache-duplicate-report'.
Check if ELT contains different files by size."
  (let (path
	file
	stat
	size
	size-old
	ret)
    (setq file (car elt)
	  elt  (cdr elt))
    (dolist (item elt)
      (setq path  (concat (cdr item) file)
	    stat  (file-attributes path)
	    size  (nth 7 stat))
      (when (and size-old
		 (not (eq size-old size)))
	(setq ret t)
	(return))
      (setq size-old size))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-duplicate-report (&optional size-rank)
  "Report all identical lisp files in `tinypath--cache' and rank by SIZE.

Input:

  SIZE-RANK

	if given, report duplicate file only if the size is
	different. If you just have copy of the same file in the
	`load-path' that is not critical, but if the file size differs
	then you have different versions of the file and you should
	remove the old one(s) from path.

Output:

  alist.el
	    35  2971 1999-02-27 12:51:12 /usr/local/share/site-lisp/common/mime/apel-9.13/
	  1166  2971 1999-11-25 00:37:18 /home/foo/elisp/tiny/lisp/other/
	     |  |    |                   |
	     |  |    |                   location
	     |  |    ISO 8601 modification time
	     |  size
	     the order number in cache

References:

  `tinypath--cache-duplicate-report-hook'
  `tinypath-cache-problem-report'."
  (interactive "P")
  (let ((ignore-functions
	 tinypath--cache-duplicate-report-ignore-functions)
	(report-buffer tinypath--report-buffer)
	accept
	list
	stat
	size
	date
	list-tmp
	list-dup
	file
	path
	ptr
	seen)
    ;; .................................................... build list ...
    ;;  result: ( (FILE . (PATH PATH PATH ..)) (FILE . (PATH ..)) )
    (dolist (elt tinypath--cache)
      (setq file  (car elt)
	    path  (nth 1 elt))
      (when (string-match "\\.el" file)
	(when tinypath--win32-p
	  (setq file (downcase file)))
	(setq accept
	      (or (and
		   ignore-functions
		   (null
		    (let (ret)
		      (dolist (func ignore-functions)
			(when (funcall func (concat (cdr path) file))
			  (setq ret t)
			  (return)))
		      ret)))
		  (null ignore-functions)))
	(when accept
	  (if (not (setq ptr (assoc file list)))
	      (push (cons file (list path)) list)
	    (setq list-tmp (cdr ptr))
	    (push path list-tmp)
	    (setcdr ptr list-tmp)))))
    ;; .............................................. check duplicates ...
    (dolist (elt list)
      (when (> (length (cdr elt)) 1)
	(push elt list-dup)))
    ;; ................................................. print results ...
    (if (null list-dup)
	(message "TinyPath: No duplicates in `tinypath--cache'")
      (let ((sorted (sort
		     list-dup
		     (function
		      (lambda (a b)
			(setq a (car a)
			      b (car b))
			(string< a b))))))
	(setq list-dup sorted))
      (display-buffer (get-buffer-create report-buffer))
      (with-current-buffer report-buffer
	(erase-buffer)
	(tinypath-report-mode 'verbose)
	(dolist (elt list-dup)
	  (when (tinypath-cache-duplicate-different-size-p elt)
	    (setq file (car elt))
	    (insert file "\n")
	    (dolist (elt (nreverse (cdr elt)))
	      (setq path  (concat (cdr elt) file))
	      (unless (member path seen)
		(push path seen)
		(if (not (file-exists-p path))
		    (insert "\t  ERROR: file does not exist " path "\n" )
		  (setq stat  (file-attributes path)
			size  (nth 7 stat)
			date  (nth 5 stat))
		  ;; ISO 8601 date
		  (setq date (tinypath-time-string date))
		  (insert (format "\t %5d %5d %s %s\n"
				  (car elt)
				  size
				  date
				  path))))))))) ;; dolist-dolist
    (with-current-buffer report-buffer
      (goto-char (point-min))
      (run-hooks 'tinypath--cache-duplicate-report-hook))
    list-dup))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-timing-summary ()
  "Gather timing summary from *Message* buffer if `tinypath--verbose-timing'."
  (interactive)
  (let ((buffer (tinypath-message-get-buffer))
	string)
    (pop-to-buffer buffer)
    (goto-char (point-min))
    (while (re-search-forward "^TinyPath: load time.*" nil t)
      (setq string (concat (or string "") "=> " (match-string 0) "\n")))
    (message "Tinypath: [TIMING SUMMARY FROM ABOVE] %s" string)
    (goto-char (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypath-report-mode-map-activate ()
  "Use local `tinypath-report-mode-map' in current buffer.
\\{tinypath-report-mode-map}"
  (use-local-map tinypath-report-mode-map))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-previous ()
  "Go to previous file."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward "^[ \t]+[0-9].*/\\(.\\)" nil t)
      (goto-char (match-beginning 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-next ()
  "Go to next file."
  (interactive)
  (re-search-forward "^[ \t]+[0-9].*/" nil t))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-find-file ()
  "Load file in current line to Emacs."
  (interactive)
  (let ((file (tinypath-report-mode-file-name)))
    (cond
     ((null file)
      (message "TinyPath: No file in this line.")
      nil)
     (t
      (display-buffer (find-file-noselect file))))))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypath-report-mode-file-name ()
  "Read filename under point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward
	   " ..:..:..[ \t]+\\(.*\\)"
	   (save-excursion (end-of-line) (point))
	   t)
      (tinypath-ti::string-remove-whitespace (match-string 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-dired (dir)
  "Run dired on current line (reads filename)."
  (interactive
   (let* ((file (tinypath-report-mode-file-name))
	  (dir   (and file
		      (file-name-directory file))))
     (list
      (read-file-name "Dired: " dir))))
  (unless dir
    (error "TinyPath: DIR missing: `%s'" dir))
  (let ((dired (tinypath-ti::dired-buffer dir)))
    (cond
     (dired
      (pop-to-buffer dired))
     ((tinypath-ti::window-single-p)
      (split-window)
      (other-window 1)
      (dired dir))
     (t
      (other-window 1)
      (dired dir)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-delete-file (&optional force)
  "Delete file in the current line. FORCE deleting.
See also `tinypath-report-mode-delete-file-noconfirm'."
  (interactive "P")
  (let ((file (tinypath-report-mode-file-name))
	(point (point)))
    (cond
     ((null file)
      (message "TinyPath: No file in this line."))
     ((not (file-exists-p file))
      (message "TinyPath: file not found %s" file))
     ((or force
	  (y-or-n-p (format "Really delete %s " file)))
      (delete-file file)
      (message "TinyPath: deleted %s" file)
      (overwrite-mode 1)
      (beginning-of-line)
      (insert "*")
      (overwrite-mode -1)))
    (goto-char point)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-report-mode-delete-file-noconfirm ()
  "Delete file in the current line without confirmation."
  (interactive)
  (tinypath-report-mode-delete-file 'force))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypath-report-mode (&optional verb)
  "Major mode to help working with `tinypath-cache-duplicate-report'.
and `tinypath-cache-problem-report'. VERB.

\\{tinypath-report-mode-map}"
  (interactive "P")
  (tinypath-report-mode-map-activate)   ;turn on the map
  (setq  mode-name   tinypath--report-mode-name)
  (setq  major-mode 'tinypath-report-mode) ;; for C-h m
  (when verb
    (message
     (substitute-command-keys
      (concat
       "TinyPath: delete file with \\[tinydesk-report-mode-delete-file]")))
    (sleep-for 1))
  (tinypath-report-mode-font-lock)
  (run-hooks 'tinypath--report-mode-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-existing-file-list ()
  "Return list of non existing files in cache."
  (let (list
	path)
    (dolist (elt tinypath--cache)
      ;; '(("file" (POS . PATH)) .. )
      (setq path (concat (cdr (nth 1 elt))
			 (car elt) ))
      (unless (file-exists-p path)
	(push path list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-existing-directory-list ()
  "Return list of non existing directories in cache or `load-path'."
  (let (list
	path)
    (dolist (dir tinypath--cache)
      ;; ( ("file" (POS . PATH)) .. )
      (setq dir (cdr (nth 1 dir)))
      (unless (file-exists-p dir)
	(pushnew path list :test 'string=)))
    (dolist (dir load-path)
      (unless (file-exists-p dir)
	(pushnew path list :test 'string=)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-non-exist-report ()
  "Report non-existing files in cache."
  (let ((list (tinypath-cache-non-existing-file-list)))
    (if (null list)
	(message "TinyPath: No non-existing files in `tinypath--cache'")
      (display-buffer (get-buffer-create tinypath--report-buffer))
      (with-current-buffer tinypath--report-buffer
	(goto-char (point-max))
	(tinypath-report-mode-font-lock)
	(insert "\nNon Existing files:\n")
	(dolist (elt list)
	  (insert "  %s\n" elt))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-problem-report (&optional size-rank)
  "Generate problem report: non-existing files and duplicates.
See SIZE-RANK in `tinypath-cache-duplicate-report'."
  (interactive)
  (tinypath-cache-non-exist-report)
  (tinypath-cache-duplicate-report))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-regenerate (&optional delete-cache)
  "Regenerate cache. `tinypath-cache-setup-main' is called with arg t.
The DELETE-CACHE removes any previous stored cache from disk.
Use it for completely clean any previous cache conflicts."
  (interactive "P")
  (when delete-cache
    (tinypath-cache-file-delete))
  ;;  If something wicked happened, at least there is a backup
  (unless load-path
    ;;  Silence byte compiler. The function is in this file, but it
    ;;  would complain: "`tinypath-original-values' might not be defined
    ;;  at runtime."
    (let ((func 'tinypath-original-values))
      (funcall func 'restore)))
  (tinypath-info-scan-Info-default-directory-list)
  (tinypath-cache-setup-main 'regenerate))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cache-mode (mode)
  "Toggle fast package loading MODE by enabling or disabling advises.

Input:

    If MODE is positive integer, enable defadvice code to to utilize
    package (possibly compressed) lookup from `tinypath--cache'.

    If MODE is negative integer, turn support off.

Description:

    If you have many directories in your `load-path', turning this mode on
    makes packages load instantly without time consuming path lookup.

Warning:

  Regenerate cache with \\[tinypath-cache-regenerate] if you have installed new
  packages or if you have added new Lisp files to your system. Keep also
  `tinypath--cache-expiry-days' relatively small if you update often."
  (interactive "P")
  (let ((list '( ;; autoload   => see below
		locate-library
		load
		require)))
    ;; In Emacs (at least on 20.7), load-library is a wrapper for load. So,
    ;; it makes no sense advising it, because the cache is searched twice.
    ;; #todo: check this code .. and xemacs `load-library'
    (when t ;;  tinypath--xemacs-p
      (push 'load-library list))
    ;;  Activate only if user requested 'all
    (when (eq tinypath--compression-support 'all)
      (push 'autoload list))
    (tinypath-ti::bool-toggle tinypath--cache-mode mode)
    (cond
     (tinypath--cache-mode
      (tinypath-ti::advice-control list "tinypath")
      (if (interactive-p)
	  (message "TinyPath: cache advice code ACTIVATED.")))
     (t
      (tinypath-ti::advice-control list "tinypath" 'disable)
      (if (interactive-p)
	  (message "TinyPath: cache advice code DEACTIVATED."))))))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinypath-cache-mode ()
  "See `tinypath-cache-mode'."
  (interactive)
  (tinypath-cache-mode 1))

;;; ----------------------------------------------------------------------
;;;
(defun turn-off-tinypath-cache-mode ()
  "See `tinypath-cache-mode'."
  (interactive)
  (tinypath-cache-mode -1))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinypath-cache-mode-maybe ()
  "See `tinypath-cache-mode'.
Turn mode on only if `tinypath--cache-expiry-days' is non-nil,
otherwise turn mode off."
  (interactive)
  (if (integerp tinypath--cache-expiry-days)
      (turn-on-tinypath-cache-mode)
    (turn-off-tinypath-cache-mode)))

;;;}}}
;;;{{{ Advice code

;; ############################   BEGIN FUNCTION -- advice instantiate

(defun tinypath-advice-instantiate ()
  "Instantiate all advices."
  ;;  These are put into function to make them delayed and
  ;;  so that they can be called at apropriate time.

  (require 'advice)

  ;;  I don't know what EFS does, but it certainly must be loaded before we
  ;;  try to advice `require' or `load' functions. It somehow overwrites the
  ;;  the original definitions.
  ;;
  ;;  efs.el
  ;;
  ;;  (efs-overwrite-fn "efs" 'load)
  ;;  (efs-overwrite-fn "efs" 'require)
  ;;
  ;;  See also efs-ovwrt.el

  (when tinypath--xemacs-p
    (require 'efs))

;;; ----------------------------------------------------------------------
;;; (turn-on-tinypath-cache-mode)
;;; (turn-off-tinypath-cache-mode)
;;;
  (defadvice autoload (around tinypath dis)
    "Use `tinypath--cache' for fast lookup of files."
    (let* ((file        (ad-get-arg 1))
	   (path        (tinypath-cache-p-for-advice file)))
      (when path
	(ad-set-arg 1 path))
      ad-do-it))

;;; ----------------------------------------------------------------------
;;; (load FILE &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)
;;;
  (defadvice load (around tinypath dis)
    "Use `tinypath--cache' for fast lookup of files."
    (let* ((file        (ad-get-arg 0))
	   (nosuffix    (ad-get-arg 3))
	   (must-suffix (ad-get-arg 4)))
      (unless (stringp file)
	(error "Parameter FILE is not a string %s"
	       (prin1-to-string file)))
      (when (and (null nosuffix)
		 (null must-suffix))
	;; #todo: this needs better handling. Now we just
	;; ignore cache if suffix parameters are set.
	;;
	;; If optional fourth arg NOSUFFIX is non-nil, don't try adding
	;; suffixes `.elc' or `.el' to the specified name FILE. If optional
	;; fifth arg MUST-SUFFIX is non-nil, insist on the suffix `.elc' or
	;; `.el'; don't accept just FILE unless it ends in one of those
	;; suffixes or includes a directory name.
	(let ((path (tinypath-cache-p-for-advice file)))
	  (when path
	    (tinypath-verbose-macro 5
	      (message "TinyPath: (advice load) Cache hit %s" file))
	    (ad-set-arg 0 path))))
      ad-do-it))

;;; ----------------------------------------------------------------------
;;;
  (defadvice load-library (around tinypath dis)
    "Use `tinypath--cache' for fast lookup of files."
    (let* ((file  (ad-get-arg 0))
	   (path  (tinypath-cache-p-for-advice file)))
      (when path
	(tinypath-verbose-macro 5
	  (message "TinyPath: (advice load-library) Cache hit %s" file))
	(ad-set-arg 0 path))
      ad-do-it))

;;; ----------------------------------------------------------------------
;;; In Win32 XEmacs 21.2 beta; the this function calls `locate-file' which
;;; for some reason breaks if given a absolute file name. The XEmacs
;;; docs also say that `locate-file' uses hash table to speed up processing.
;;; Hm.
;;;
;;; There is problem with functions that use (interactive-p) test, because
;;; advice can't pass the information to the underlying function, so any
;;; such test inside here won't work.
;;;
;;; 21.3.1:
;;; (locate-library LIBRARY &optional NOSUFFIX PATH INTERACTIVE-CALL)
;;;
  (defadvice locate-library (around tinypath act)
    "Use `tinypath--cache' for fast lookup of files."
    (interactive
     (let ((cache (tinypath-emacs-lisp-file-list 'from-cache)))
       (list
	(completing-read
	 (format "%slocate library: "
		 (if cache
		     "(TinyPath cache)"
		   ""))
	 cache
	 nil
	 nil
	 nil))))  ;;; Default word
    (let* ((file  (ad-get-arg 0))
	   (ok    (tinypath-load-copy-equal-p))
	   (path  (if (and ok
			   file)
		      (tinypath-cache-p file)))
	   (error (and ok
		       path
		       (tinypath-cache-warn-if-not-exist path))))
      (unless (stringp file)
	(error "Parameter FILE is not a string %s"
	       (prin1-to-string file)))
      (cond
       ((and path
	     (null error))
	(tinypath-verbose-macro 5
	  (message "TinyPath: (advice locate-library) Cache hit %s => %s"
		   file path))
	(setq ad-return-value path))
       ((and ok
	     (setq path (car-safe (tinypath-locate-library file))))
	;;  (fboundp 'locate-file)  ;; Do not continue in XEmacs
	(setq ad-return-value path))
       (t
	ad-do-it))
      ;; We must simulate in the advice, this interactive behavior, because
      ;; underlying function does not know it any more, due to advice.
      (when (interactive-p)
	(if path
	    (message path)
	  (message "locate-library: %s not found."
		   (or file "<no filename>"))))))

;;; ----------------------------------------------------------------------
;;;
  (defadvice require (around tinypath dis)
    "Use `tinypath--cache' for fast lookup of files.
Property (get 'require 'tinypath-load-list) contains list
of required packages: '((feature . path)."
    (let ((feature  (ad-get-arg 0))
	  (opt      (ad-get-arg 1))    ;the optional "file" parameter
	  (alist    (get 'require 'tinypath-load-list))
	  lib
	  path)
      (unless (symbolp feature)
	(error "Parameter FEATURE is not a symbol %s"
	       (prin1-to-string feature)))
      (when (and (not (featurep feature))
		 ;;  Avoid recursive calls.
		 (not (assq feature alist)))
	(setq lib (cond
		   ((stringp opt)
		    (if (string-match "/" opt)
			(tinypath-expand-file-name opt)  opt))
		   (t
		    (symbol-name feature))))
	(when (setq path (tinypath-cache-p-for-advice lib))
	  (tinypath-verbose-macro 5
	    (message "TinyPath: (advice require) Cache hit %s" lib))
	  (tinypath-cache-warn-if-not-exist path)
	  (push (cons feature path) alist)
	  (put 'require 'tinypath-load-list alist)
	  (ad-set-arg 1 path)))
      ad-do-it))

  ) ;; ############################   END FUNCTION -- end advice instantiate

;;;}}}
;;;{{{ win32: Unix $HOME directory mounted to PC, like to H: disk

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-dump (mount-point &optional file)
  "Dump load path directories to disk.

If you have Mounted Unix disk (say H: ) which sees your Unix $HOME directory,
then keep in mind that NT Emacs does not see symlinked directories.

Call this function from _Unix_ Emacs and it converts symbolic links to
real directory names and writes output to FILE.

You can then load that file in your NT emacs and make it see all
the same directories as your Unix Emacs does.

Repeat this every time you make symbolic path links in Unix.

References:

  `tinypath--load-path-dump-file'"
  (interactive "sUnix $HOME is equivalent to: \nf")
  (let ((home      (file-truename (tinypath-expand-file-name "~")))
	(load-path load-path))
    (setq tinypath-dumped-load-path nil)
    (or file
	(setq file tinypath--load-path-dump-file))
    (dolist (path load-path)
      (if (not (string-match "[a-z]" mount-point))
	  (setq path (file-truename path))
	(setq path (tinypath-replace-regexp-in-string
		    (regexp-quote home)
		    mount-point
		    (file-truename path))))
      (push path tinypath-dumped-load-path))

    (tinypath-ti::write-file-variable-state
     file "Absolute path dump for NTEmacs to access Unix Home disk"
     '(tinypath-dumped-load-path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-setup-win32 ()
  "Load `tinypath--load-path-dump-file' in win32."
  (let ((file tinypath--load-path-dump-file))
    (when (and tinypath--win32-p
	       (load file 'noerr))
      ;; Merge these unix paths with the NT Emacs paths.
      ;; If these paths do not exist; they are not added
      (tinypath-verbose-macro 2
	(message "TinyPath: load-path merge from %s" file))
      (tinypath-add-directory-many
       (symbol-value 'tinypath-dumped-load-path)))))

;;}}}
;;{{{ Win32 support (cygwin)

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-manpage-handler (path)
  "If PATH has manual pages, add to `tinypath--extra-manpath'."
  (let (ret)
    (unless (member path tinypath--extra-manpath)
      (dolist (file (directory-files path))
	(when (string-match "\\.[0-9]$" file)
	  (tinypath-verbose-macro 9
	    (message "TinyPath: MAN %s [found %s] " path file))
	  (pushnew path tinypath--extra-manpath :test 'string=)
	  (setq ret path)
	  (return))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-extra-path-handler (path)
  "Check PATH for info files and manual pages."
  (tinypath-info-handler path)
  (tinypath-manpage-handler path))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-woman-setup ()
  "Install woman.el (if available) to read manual pages in Win32."
  (when tinypath--win32-p
    (when (or (featurep 'woman)
	      (fboundp 'woman)
	      (when (locate-library "woman.el")
		(autoload 'woman                  "woman" "" t)
		(autoload 'woman-find-file        "woman" "" t)
		(autoload 'woman-dired-find-file  "woman" "" t)

		(unless (getenv "MANPATH") ;; woman-path
		  (message
		   "TinyPath: MANPATH does not exist, set `woman-manpath'."))
		t))
      (defalias 'man 'woman)
      t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-extra-path-setup (list)
  "Look for new info and manual pages under LIST of root directories."
  (dolist (path list)
    (if (or (not (stringp path))
	    (not (file-directory-p path)))
	(tinypath-verbose-macro 5
	  (message
	   "TinyPath: invalid search ROOT %s"
	   (prin1-to-string path)))
      (tinypath-ti::directory-recursive-do
       path 'tinypath-extra-path-handler))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-cygwin-setup ()
  "If Cygwin is present add it to `tinypath--extra-path-root'."
  (let ((cygwin-path (tinypath-ti::win32-cygwin-p))) ;; has trailing slash
    (if (null cygwin-path)
	(tinypath-verbose-macro 2
	  (message "TinyPath: [Cygwin] not found from PATH."))
      (pushnew cygwin-path
	       tinypath--extra-path-root
	       :test 'string=)
      ;;  Be absolutely sure that the path is not added multiple
      ;;  times "f:/unix/cygwin" or "f:/unix/cygwin/" because
      ;;  this would cause reading the same directory twice
      ;;
      ;; (tinypath-directory-list-clean  ;; No trailing slashes after this
      ;;  tinypath--extra-path-root
      ;;  "CYGWIN tinypath--extra-path-root")
      ;;
      (tinypath-verbose-macro 2
	(message "TinyPath: [Cygwin] found from PATH: %s" cygwin-path))
      ;; (tinypath-extra-path-setup list)
      tinypath--extra-path-root)))

;;}}}
;;{{{ Install functions

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-timer (&optional uninstall)
  "Install or UNINSTALL timer to keep cache structure in synch with disk.
Reference:
  `tinypath-cache-setup-maybe'  15min, idle timer calls this periodically."
  (interactive "P")
  (let (timer
	status)
    (when (fboundp 'run-with-idle-timer)
      ;;
      ;;  I don't think this ever fails, but be bullet proof anyway
      ;;  We ,ust run `require' because `run-with-idle-timer'
      ;;  must not be in autoload state.
      ;;
      ;;  timers are different in Emacs implementations. Load correct
      ;;  package.
      ;;  XEmacs keeps this in xemacs-packages/lisp/fsf-compat/timer.el
      ;;
      (setq status
	    (cond
	     (tinypath--xemacs-p
	      (or (require 'itimer)
		  (require 'timer)))
	     (t
	      (require 'timer))))
      (if (null status)
	  (tinypath-verbose-macro 1
	    (message "TinyPath: TIMER ERROR Can't install timers to emacs."))
	(cond
	 (uninstall
	  (tinypath-ti::compat-timer-cancel-function
	   'tinypath-cache-setup-maybe)
	  (message
	   "TinyPath: `load-path' synchronization watchdog UNINSTALLED."))
	 (t
	  (tinypath-ti::compat-timer-cancel-function
	   'tinypath-cache-setup-maybe)
	  ;;  At this point, we have wiped out the autoload definitions
	  ;;  with explicit `require', because `symbol-function'
	  ;;  won't work on autoloaded definitions.
	  (tinypath-autoload-require 'run-with-idle-timer)
	  (setq timer
		(funcall
		 (symbol-function 'run-with-idle-timer)
		 (* 60 15)
		 'repeat
		 'tinypath-cache-setup-maybe))
	  (message
	   "TinyPath: `load-path' synchronization watchdog INSTALLED.")))))
    (setq tinypath--timer-elt timer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-woman ()
  "Add items in `tinypath--extra-manpath' to `woman-manpath'."
  (when (boundp 'woman-manpath)
    (dolist (path tinypath--extra-manpath)
      (when (stringp path)
	(tinypath-verbose-macro 7
	  (message "TinyPath: Adding to `woman-manpath' %s" path))
	(pushnew path woman-manpath :test 'string=)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-find-file ()
  "Add items in `tinypath--extra-manpath' to `woman-manpath'."
  (when (boundp 'ff-search-directories)
    (dolist (path tinypath--extra-ff-search-directories)
      (when (stringp path)
	(tinypath-verbose-macro 7
	  (message "TinyPath: Adding to `ff-search-directories' %s" path))
	(pushnew path ff-search-directories :test 'string=)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-load-path-list ()
  "Return Gnus locations in `load-path' by searching regexp gnus/?$"
  (let (list
	found
	previous)
    (dolist (path load-path)
      ;; cvs-packages/gnus/etc/gnus
      ;;
      ;; "../gnus/"    or at the end "../gnus"
      ;;
      (and (not (string-match "/etc/" path))
	   (string-match "\\(.+[/\\]gnus\\)\\([/\\]\\|[/\\]?$\\)"  path)
	   (setq found (match-string 1 path))
	   ;;  It's faster to "remember" the previous match and test it with
	   ;;  `equal' that all the time use `pushnew'. This reduces
	   ;;  `pushnew' calls.
	   (not (equal previous found))
	   (setq previous found)
	   (pushnew found list :test 'string=)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-versions (&optional path-list)
  "Find out gnus version numbers along `load-path' or PATH-LIST.
The PATH-LIST must conatins the root directoryies of Gnus installations.
Return ((VER . PATH) ..)."
  (let (file
	list)
    ;; There is no way we can say which Gnus version is the latest without
    ;; loading the gnus.el and looking inside the file
    (tinypath-with-temp-buffer
     (dolist (path path-list)
       ;;  XEmacs installation drop all gnus lisp files directly under:
       ;;
       ;;      xemacs-packages/lisp/gnus/
       ;;
       ;;  But the Gnus CVS tree contains directory structure
       ;;
       ;;      cvs-packages/gnus/lisp/
       ;;      cvs-packages/gnus/contrib
       ;;      cvs-packages/gnus/etc
       ;;
       (dolist (try '("gnus.el" "lisp/gnus.el"))
	 (setq file (concat
		     (tinypath-expand-file-name
		      (file-name-as-directory path))
		     try))
	 (when (file-exists-p file)
	   (erase-buffer)
	   ;;  About within 10%  of the file size the defconst can be found
	   (insert-file-contents file nil 1 10000)
	   (goto-char (point-min))
	   (when (re-search-forward
		  "defconst.*gnus-version.*\"\\([0-9.]+\\)"
		  nil t)
	     (push (cons (match-string 1) file)
		   list)))))
     (tinypath-verbose-macro 7
       (message "TinyPath: found Gnus versions %s" (prin1-to-string list)))
     list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-gnus-latest-version (path-list)
  "Return latest gnus version from PATH-LIST.
Return structure is ordered so, that the latest version is first:
'((VERSION-STRING . PATH) ..).

Development versions starting with 0.N are condired newer that
any N.N version."
  (let ((ver (tinypath-gnus-versions path-list))
	zero
	sorted)
    (when ver
      (setq sorted
	    (sort
	     ver
	     (function
	      (lambda (a b)
		(if (or (string-match "^0" (car a))
			(string-match "^0" (car b)))
		    (setq zero t))
		(setq a (car a)
		      b (car b))
		(tinypath-ti::vc-version-lessp a b)))))
      ;;  put ZERO numbers first.
      (if zero
	  (setq sorted (reverse sorted))))
    sorted))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-insinuate-gnus ()
  "Examine `load-path' and leave the latest Gnus version."
  (let ((list (tinypath-gnus-load-path-list)))
    (cond
     ((null list)
      (tinypath-verbose-macro 7
	(message "TinyPath: No newer Gnus found along `load-path'.")))
     ((eq 1 (length list))
      ;;  Make sure no old gnus is used.
      (setq tinypath--cache-level-two nil)
      (tinypath-verbose-macro 1
	(message "TinyPath: One Gnus found along `load-path' %s"
		 (car list)))
      (pushnew (car list) load-path :test 'string=)
      list)
     (t
      ;; Latest gnus version is first in the returned list, drop it out
      ;; and remove all other paths.
      ;;
      (dolist (path (cdr (tinypath-gnus-latest-version list)))
	(setq path
	      (tinypath-file-remove-trailing-slash
	       (file-name-directory (cdr path))))
	;;  some/dir/gnus/lisp/  -->  some/dir/gnus/
	(tinypath-verbose-macro 1
	  (message "TinyPath: Removing older Gnus from `load-path' %s"
		   path))
	(tinypath-admin-remove-matching path)
	path)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-setup (&optional no-cache force)
  "Add additional directories to `load-path'.
If `tinypath--cache-expiry-days' is defined, use cached `load-path'
If cache is too old, read directories under `tinypath--load-path-root'.

Input:

  NO-CACHE   If non-nil, do not use cache but read directories under
	     `tinypath--load-path-root'.
  FORCE      Regenerate cache.

References:

  `tinypath--load-path-function'"
  (interactive "P")
  (if (or no-cache
	  (null tinypath--cache-expiry-days)) ;Cache is not allowed
      (funcall tinypath--load-path-function)
    (tinypath-cache-setup-main force)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-load-path-root-changed-p ()
  "Check if `tinypath--load-path-root' has changed since last run.
The property value (get 'tinypath--load-path-root 'tinypath-last-value)
holds the last stored value."
  (let ((last (get 'tinypath--load-path-root 'tinypath-last-value)))
    (and last
	 (not (equal last tinypath--load-path-root)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install ()
  "Install package. There is no uninstall."
  (interactive)
  (let ((fid     "tinypath-install")
	(time-a  (current-time))
	time-b
	diff)
    (message "TinyPath: %s BEGIN %s" fid (tinypath-time-string))
    (message "TinyPath: [INFO] (defmacro) Info-default-directory-list: %s"
	     (prin1-to-string (tinypath-Info-default-directory-list)))
    (message "TinyPath: [INFO] Info-directory-list: %s"
	     (if (boundp 'Info-directory-list)
		 (prin1-to-string Info-directory-list)
	       "<empty>"))
    (message "TinyPath: [INFO] INFOPATH environment variable: %s"
	     (or (getenv "INFOPATH")
		 "no variable"))
    ;;  Must be before the cygwin check, where cygwin1.dll is
    ;;  searched along `exec-path'
    ;;
    ;; (tinypath-exec-path-clean)
    ;; (tinypath-exec-path-check-verbose 'fix) ;; Missing items? (from PATH)
    ;;
    ;;  This is already set in default value for `tinypath--extra-path-root'
    ;;  (when (tinypath-win32-p) (tinypath-cygwin-setup))
    ;; ................................................ examine system ...
    ;;
    ;;  Make sure all are absolute: use forward slash in all path names
    (tinypath-expand-file-name-variable-macro
     tinypath--load-path-root)
    ;;  Suppose user has changed the value since the last time
    ;;  and does M-x load-library RET tinypath.el RET
    ;;  => check if we should regenerate cache or read from disk
    (if (not (tinypath-load-path-root-changed-p))
	(tinypath-setup)
      (message
       "TinyPath: INSTALL tinypath--load-path-root changed, doing reboot.")
      ;; (tinypath-cache-regenerate)
      nil)
    ;; ........................................ cleanup and activation ...
    ;;
    ;; Delay defining advises until this point
    ;;
    (unless (eq tinypath--compression-support 'none)
      (tinypath-advice-instantiate))
    ;;
    ;;  The autoload statements must be here, because `autoload' is
    ;;  an advised function. The `fboundp' is just an extra measure,
    ;;  so that it does not even call the advised-autoload function if
    ;;  this file is loaded multiple times
    ;;
    (unless (fboundp 'ti::macrof-version-bug-report)
      (autoload 'ti::macrof-version-bug-report "tinylib" "" nil 'macro))
    (unless (fboundp 'font-lock-mode)
      (autoload 'font-lock-mode "font-lock"  "" t))
    (unless (eq tinypath--compression-support 'none)
      (turn-on-tinypath-cache-mode-maybe))
    ;; (tinypath-install-timer)       ;; Install watchdog to check load-path
    ;;  woman.el, man page viewer for Win32
    ;;  We do not load this, but define autoloads and then add the found
    ;;  paths after woman is active.
    ;;
    (when tinypath--win32-p
      (if (tinypath-woman-setup)
	  (tinypath-eval-after-load "woman" 'tinypath-insinuate-woman)
	(when tinypath--extra-manpath
	  (message "\
TinyPath: ** Hm, manual pages found, but you do not have woman.el
	     Visit http://centaur.maths.qmw.ac.uk/Emacs/
	     and you will be able to use `M-x man' in Win32 system."))))
    (tinypath-eval-after-load "find-file" 'tinypath-insinuate-find-file)
    (setq time-b (current-time))
    (setq diff   (tinypath-ti::date-time-difference time-b time-a))
    (put 'tinypath--load-path-root
	 'tinypath-last-value
	 tinypath--load-path-root)
    (tinypath-Info-default-directory-list-clean)
    (tinypath-exec-path-clean)
    (message "TinyPath: [INFO] END (defmacro) Info-default-directory-list: %s"
	     (prin1-to-string (tinypath-Info-default-directory-list)))
    (message "TinyPath: [INFO] END Info-directory-list: %s"
	     (if (boundp 'Info-directory-list)
		 (prin1-to-string Info-directory-list)
	       "<empty>"))
    (message "TinyPath: %s END %s" fid (tinypath-time-string))
    (message (concat (tinypath-cache-status-string)
		     (format " time %d sec" diff)))))

;;}}}
;;{{{ Require (b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The require statements are unconventionally put here and not to the
;;  beginning of file, because sometimes Win32
;;  XEmacs development betas do not have correct `load-path' and
;;  require `advice' and `jka-compr' would fail.
;;
;;  At this point the load-path has been partially fixed (that is: booted)
;;  and we can run `require' commands.
;;
;;  The files can be in compressed format as well.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-and-compile
  (defun tinypath-original-values (mode)
    "MODE can be 'save 'restore original `load-path' and `exec-path'.
The original value is saved under property `tinypath-saved-value'."
    (let ((savesym 'tinypath-saved-value))
      (dolist (sym '(load-path
		     exec-path))
	(cond
	 ((eq mode 'save)
	  ;;  Save can only be once.
	  (or (get sym savesym)
	      (put sym savesym (symbol-value sym))))
	 ((eq mode 'restore)
	  (set sym (get sym savesym)))))))

  (tinypath-original-values 'save)

  ;;  We MUST run this at compile time too, because in XEmacs
  ;;  it will make loading custom.elc possible. Without it, the
  ;;  defcustomed variables give errors
  (when tinypath--install-flag
    (when (and (not (tinypath-byte-compile-running-p))
	       ;;(and (tinypath-byte-compile-running-p)
	       ;;     (boundp 'xemacs-logo))
	       ;;
	       ;; If there is cache and it is valid, do not run
	       ;; BOOT.
	       (let ((file (tinypath-cache-file-name)))
		 (tinypath-cache-file-old-p file)))
      (tinypath-load-path-initial-value
       tinypath--core-emacs-load-path-list))))

(require 'info)

;;}}}
;;{{{ Install load time

;;; ----------------------------------------------------------------------
;;;
;;;####autoload (autoload 'tinypath-version "tinypath" "" t)
(defun tinypath-version (&rest args)
  "Display version and manual. ARGS are ignored."
  (interactive)
  (let ((path (locate-library "tinypath.el")))
    (cond
     ((null path)
      (message "TinyPath: [ERROR] cannot find tinypath.el to read."))
     (t
      (let* ((name   "*tinypath-version*")
	     (buffer (get-buffer name)))
	(if buffer
	    (pop-to-buffer buffer)
	  (pop-to-buffer (get-buffer-create name))
	  (insert-file-contents path)
	  (goto-char (point-min))
	  (when (re-search-forward "Change Log")
	    (forward-line 1)
	    (delete-region (point) (point-max))
	    (goto-char (point-min))
	    (while (re-search-forward "^;[;{}]+ ?" nil t)
	      (replace-match "" nil 'literal))
	    (goto-char (point-min)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-reset-variables ()
  "Restore modified values, like GC parameters."
  ;; Restore value that was saved at the beginning of file
  (setq gc-cons-threshold
	(get 'gc-cons-threshold 'tinypath))
  ;;  Restore original value for rest of the Emacs session
  (let ((val (get 'tinypath--verbose 'debug-init)))
    (when (integerp val)
      (setq tinypath--verbose val))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-pristine ()
  "Try to restore package to original Emacs settings.
This means restoring `exec-path' and `load-path' as they
were seen at Emacs startup. The cache is cimpletely rebuilt and
then saved to disk."
  (interactive)
  (let ((load (tinypath-load-copy-get 'original))
	(exec (get 'exec-path 'tinypath)))
    (if (not (and load exec))
	(error "TinyPath: No original values found.")
      (setq load-path load)
      (setq exec-path exec)
      (tinypath-cache-regenerate))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypath-install-main ()
  "The main loader. The very first setup for the package.
This function is called when package is loaded.

Runs hooks:

  `tinypath--report-mode-define-keys-hook'
  `tinypath--load-hook'."
  (run-hooks 'tinypath--report-mode-define-keys-hook)
  (eval-and-compile
    (unless (tinypath-byte-compile-running-p)
      (tinypath-install-environment)
      (run-hooks 'tinypath--load-hook)))
  ;;  This last message is here solely so that with log level 20
  ;;  the message is also saved the log file
  (tinypath-verbose-macro 3
    (tinypath-cache-status-message)))

(tinypath-load-copy-now)
(tinypath-load-copy-now 'original)
(put 'exec-path 'tinypath exec-path) ;; Save original value

(if tinypath--install-flag
    (tinypath-install-main))

(tinypath-install-reset-variables)

;;}}}

;;; tinypath.el ends here
