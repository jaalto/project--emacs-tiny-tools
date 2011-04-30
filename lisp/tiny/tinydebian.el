;;; tinydebian.el --- Debian, Ubuntu, Emacs, GNU Bug Tracking Utilities

;;{{{ Id

;; Copyright (C)    2001-2010 Jari Aalto
;; Keywords:        extensionss
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;; Created:         2001
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

;; ........................................................ &t-install ...
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   $HOME/.emacs startup file
;;
;;      (setq tinydebian--novice-mode t)  ;; or nil
;;      (add-hook 'tinydebian--load-hook 'tinydebian-install)
;;      (require 'tinydebian)
;;
;;   If you have any questions about this Emacs package:
;;
;;      M-x tinydebian-submit-bug-report    send question, feedback, bugs

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      Summary: Few Debian specific utilities and a Generic interface
;;      to many BTS's.
;;
;;      http://www.emacswiki.org/emacs/TinyDebian
;;
;;      This package contains utilities for the package authors to
;;      help reporting and managing bug reports at various Bug
;;      Tracking Systems (BTS). The Bug interface hooks up in Gnus
;;      *Summary* (http://www.gnus.org/) buffer where separate menu to
;;      send commands appear.
;;
;;      There are also minor modes that can be turned on for `M-x'
;;      `mail' (C-x m) to use BTS control commands.
;;
;;      The heuristics are based in Gnus Summary and Article buffers
;;      how to decide which bug tracking system will be used.
;;
;;      Briefly:
;;
;;      Debian utilities:
;;
;;      o   Colorize /var/log files like messages, syslog etc.
;;      o   Access important Debian developer documents (FAQ, WNPP)
;;      o   Query Debian WWW information: package page, PTS page,
;;          package bugs page
;;      o   System information: wnpp-alert(1) listing to show what
;;          packages have problems and which as suspect for removal
;;          (not maintained).
;;
;;      BTS utilities in Gnus *Summary* (and M-x mail, message-mode)
;;
;;      o   The "debbugs" mail based BTS's: full support to manipulatte
;;          bug messages for Debian, Emacs and GNU bug tracking
;;          systems.
;;      o   Other BTS: limited support for visting bugs at Sourceforge,
;;          Launchpad, Freshmeat, KDE, Gnome, MySQL, Perl CPAN, Redhat
;;          Sourcewre, Mercurial version control, Trac, Github. You can
;;          manipulate these bugs via their Web interface
;;      o   In Gnus *Summary* buffer, there is new menu "Tdeb" to
;;          administrate "debbugs" bug reports. Useful for Debian, GNU
;;          and Emacs developers.
;;      o   To send on issue (wishlist or bug) to any of the "debbugs"
;;          systems, similar to Debian's reportbug(1), use 'M-x'
;;          'tinydebian-reportbug'.
;;
;;  History
;;
;;      This package is called 'tinydebian', because it was first
;;      developed to help administering Debian GNU/Linux. Support for
;;      the Debian BTS was added afterwards. The Debian mail based BTS
;;      is called "debbugs". For more information, see
;;      http://www.debian.org/Bugs/server-control
;;
;;      Later GNU Emacs development moved also to "debbugs" and support for
;;      Emacs BTS was added. In 2010 the GNU coreutils also started using
;;      "debbugs" and support was added. For more information, see
;;      http://debbugs.gnu.org/
;;
;;  Todo
;;
;;      Ubuntu Launchpad.net BTS could also be controlled by email and
;;      it has similarities to the Debian BTS. Support for Launchpad email
;;      interface may appear in the future.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: libraries

(defconst tinydebian--version-time "2011.0305.1123"
  "Last edited time.")

(require 'tinylibm)

(autoload 'gnus-eval-in-buffer-window   "gnus-util" "" nil 'macro)
(autoload 'gnus-summary-article-number  "gnus-sum")
(autoload 'gnus-summary-display-article "gnus-sum")
(autoload 'gnus-summary-select-article   "gnus-sum")
(autoload 'url-retrieve-synchronously   "url")
(autoload 'mail-position-on-field       "sendmail")
(autoload 'mail-fetch-field             "mail-utils")
(autoload 'regexp-opt                   "regexp-opt")
(autoload 'url-retrieve-synchronously   "url")
(autoload 'mm-url-decode-entities-string "mm-url")
(autoload 'message-send-and-exit         "message")
(autoload 'message-fetch-field           "message")
(autoload 'mail-header-narrow-to-field   "mail-parse")

(eval-and-compile
  ;;  Forward declarations to quiet byte compiler.
  (defvar tinydebian--mail-mode-map)
  (defvar gnus-original-article-buffer)
  (defvar message-reply-buffer)
  (defvar gnus-newsgroup-name)
  (defvar font-lock-mode)
  (defvar font-lock-keyword-face)
  (defvar global-font-lock-mode)
  (defvar font-lock-keywords)
  (defvar font-lock-defaults)
  (defvar gnus-article-buffer))

(ti::package-defgroup-tiny TinyDebian tinydebian-- extensions
  "Debian System administrator's grabbag of utilities.")

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinydebian--novice-mode t
  "*If nil, do no hand-holding. Thi removed all extra help
messages inserted e.g. in BTS control send message templates."
  :type  'boolean
  :group 'TinyDebian)

(defcustom tinydebian--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian--find-bug-nbr-hook '(tinydebian-bug-nbr-any)
  "*Functions to return Debian bug tracking number as string.
Default value is '(tinydebian-bug-nbr-any)."
  :type  'function
  :group 'TinyDebian)

(defcustom tinydebian--find-email-hook '(tinydebian-email-any)
  "*Functions to return Email address as string.
Default value is '(tinydebian-email-any)."
  :type  'function
  :group 'TinyDebian)

(defcustom tinydebian--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian--browse-url-function
  (function tinydebian-browse-url-browse-url)
  "*Function to run for HTTP URLs. Default is `browse-url'.
To use text mode buffer inside Emacs, set value to
`tinydebian-browse-url-lynx-dump' if lynx(1) is available.

See also `browse-url-browser-function'."
  :type  'function
  :group 'TinyDebian)

;;}}}
;;{{{ setup: user config

;;; ................................................... &v-user-config ...

(defcustom tinydebian--install-buffer-file-name-regexp
  "/debian/\\(changelog\\|.*README\\)"
  "*Activate `tinydebian-bts-mode' on buffers whose file name match regexp.
This variable is used when function `tinydebian-install' is called."
  :type  'regexp
  :group 'TinyDebian)

(defcustom tinydebian--buffer-tiger "*Tinydebian tiger*"
  "*Buffer name where to generate tiger(1) mail report chmod fixes.
See function `tinydebian-command-audit-report-tiger'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--buffer-wnpp-alert "*Tinydebian wnpp-alert*"
  "*Buffer name where to generate wnpp-alert(1) report.
See function `tinydebian-command-show-wnpp-alert'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--buffer-rc-alert "*Tinydebian rc-alert*"
  "*Buffer name where to generate rc-alert(1) report.
See function `tinydebian-command-show-rc-alert'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--wnpp-alert-mode-name "TwnppAlert"
  "*Editing mode for WNPP alert buffer."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--wnpp-alert-mode-hook
  '(tinydebian-wnpp-alert-default-mode-bindings)
  "*Hook run after the `tinydebian-wnpp-alert-mode' is turned on."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian--buffer-www "*Tinydebian WWW*"
  "*Buffer name where to put WWW call results.
See `tinydebian--browse-url-function'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--bts-extra-headers nil
  "*Extra headers to insert to mail buffer.
See `tinydebian-bts-insert-headers'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--buffer-http-get "*Tinydebian WWW HTTP*"
  "*Buffer name where to put HTTP GET result.
See `tinydebian-debian-url-bug-initialize'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--buffer-bug-format "*Tinydebian bug#%s*"
  "*A `format' string for buffer, where %s is substituted with bug number.
See `tinydebian-buffer-url-bug'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian--bts-mail-type-it-nmu-message
  "This bug seems to be a candidate for NMU. I have some free time
and I am offering help. Please let me know if this bug
is already been worked on or if it's okay to NMU the package."
  "*Message to insert in `tinydebian-bts-mail-type-it-nmu'.
See also `tinydebian-bts-mail-type-it-nmu-hook'."
  :type  'string
  :group 'TinyDebian)

(defcustom tinydebian-bts-mail-type-it-nmu-hook nil
  "*Hook to run at the end of `tinydebian-bts-mail-type-it-nmu'."
  :type  'hook
  :group 'TinyDebian)

(defcustom tinydebian--install-gnus-newsgroup-name-regexp
  "debian"
  "*Newsgroup name regexp to match to activate `tinydebian-bts-mode'."
  :type  'string
  :group 'TinyDebian)

(defface tinydebian--warn-face
  '((((class color) (background light))
     (:background "green"))
    (((class color) (background dark))
     (:background "sea green"))
    (((class grayscale monochrome)
      (background light))
     (:background "black"))
    (((class grayscale monochrome)
      (background dark))
     (:background "white")))
  "Face used for warnings."
  :group 'TinyDebian)

;;; Color loading section  This is messy *Blech!*
;;
(defface tinydebian--item-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green3")))
  "Face used for noticing important items."
  :group 'TinyDebian)

(defcustom tinydebian--font-lock-mode t
  "If non-nil, allow turning on `font-lock-mode'."
  :type  'boolean
  :group 'TinyDebian)

;;}}}
;;{{{ setup: -- private

;;; ....................................................... &v-private ...

(defvar tinydebian--wnpp-alert-mode-map nil
  "Local keymap for WNPP alert listing.")

(defvar tinydebian--font-lock-keywords-adaptive-date t
  "Flag to signal that current time is used to display today's log.
For exmple in /etc/syslog today's log log entries are highlighted
differently that previous days. However this must be changed in
next day, because the day changes.

This flags says, that adaptive-date regexps are be used.")

(make-variable-buffer-local 'tinydebian--font-lock-keywords-adaptive-date)

(defvar tinydebian--font-lock-keywords-bugs-rc ;; &font
  ;; Package: [59]bookmarks (optional; [60]Tobias Toedter) [bookmarks/1.4 ; =] [[61]
  ;; add/edit comment]
  ;; [62]401275 [P        N ] Remove two sites which force the user to enter a 24 mo
  ;; nth contract
  (list
   (list
    "Package: *\\[[0-9]+\\] *\\([a-z0-9.-]+\\)"
    1 'font-lock-builtin-face)
   (list
    (concat
     "^\\[[0-9]+\\][[0-9]+ *\\(\\[[^]\r\n]+\\]\\) +"
     "\\(.+"
     ;;  Continue to second line
     "\\(?:\n *[A-Za-z<>'()].*\\)?"
     "\\)")
    '(1 'font-lock-type-face)
    '(2 'font-lock-keyword-face)))
  "Font lock keywords to set after calling `tinydebian-url-list-bugs-by-rc'.
Only used if `tinydebian--browse-url-function'is set to
`tinydebian-browse-url-lynx-dump'.")

(defvar tinydebian--font-lock-package-bugs
  (list
   (list
    "Package: *\\[[0-9]+\\] *\\([a-z0-9.-]+\\)"
    1 'font-lock-builtin-face))
  "Font lock keywords to set after calling `tinydebian-url-list-bugs-by-rc'.
Only used if `tinydebian--browse-url-function'is set to
`tinydebian-browse-url-lynx-dump'.")

(defconst tinydebian--bin-dpkg (executable-find "dpkg")
  "Location of `dpkg' binary.")

(defconst tinydebian--bin-apt-cache (executable-find "apt-cache")
  "Location of `apt-cache' binary.")

(defconst tinydebian--bin-grep-available (executable-find "grep-available")
  "Location of `grep-available' binary.")

(defvar tinydebian--grep-find-devel-docdir-list
  '("/usr/share/doc/debian-policy"
    "/usr/share/doc/debian-reference-en"
    "/usr/share/doc/debian-reference-en"
    "/usr/share/doc/developers-reference")
  "*List of directororied to search for Debian development policy etc.")

(defvar tinydebian--usertag-email-list
  (delq
   nil
   (list
    (cons user-mail-address 1)
    (let ((email (getenv "DEBEMAIL")))  ;See dch(1)
      (if email
          (cons email  2)))
    ;; Package removals. See
    ;; http://wiki.debian.org/ftpmaster_Removals
    (cons "release.debian.org@packages.debian.org" 3)))
  "Email addresses for (Debian) BTS 'usertag' comands.
An alist of email addresses for use in `completing-read':
  '((EMAIL . NUMBER) (EMAIL . NUMBER) ...).")

(defvar tinydebian--severity-list
  '(("critical"
     "Makes unrelated software on the system (or the whole system) break,
or causes serious data loss, or introduces a security hole on systems where
you install the package.")
    ("grave"
     "Makes the package in question unuseable or mostly so, or causes data
loss, or introduces a security hole allowing access to the accounts of users
who use the package.")
    ("serious"
     "Severe violation of Debian policy (that is, it violates a
\"must\" or \"required\" directive), or, in the package maintainer's
opinion, makes the package unsuitable for release.")
    ("important"
     "A bug which has a major effect on the usability of a package,
without rendering it completely unusable to everyone.")
    ("normal"
     "The default value, applicable to most bugs.")
    ("minor"
     "A problem which doesn't affect the package's usefulness, and is
presumably trivial to fix.")
    ("wishlist"
     "For any feature request, and also for any bugs that are very
difficult to fix due to major design considerations.")
    ("fixed"
     "For bugs that are fixed but should not yet be closed. This is an
exception for bugs fixed by non-maintainer uploads. Note: the "fixed"
tag should be used instead."))
  "The bug system records a severity level with each bug report.
This is set to normal by default, but can be overridden either by supplying a Severity line in the pseudo-header when the bug is submitted Severity or error.
http://www.debian.org/Bugs/Developer#severities")

(defvar tinydebian--launchpad-status-list ;FIXME add descriptions
  '(("confirmed"
     "")
    ("new"
     "")
    ("invalid"
     "")
    ("wontfix"
     "")
    ("confirmed"
     "")
    ("triaged"
     "")
    ("inprogress"
     "")
    ("incomplete"
     "")
    ("fixcommitted"
     "")
    ("fixreleased"
     ""))
  "List of status values and their explanations '((value description) ...).")

(defvar tinydebian--launchpad-importance-list ;FIXME add descriptions
  '(("wishlist"
     "")
    ("low"
     "")
    ("medium"
     "")
    ("high"
     "")
    ("critical"
     ""))
  "List of importance values and their explanations '((value description) ...).")

(defvar tinydebian--severity-selected nil
  "Function `tinydebian-severity-select-*' sets this to user selection.")

(defconst tinydebian--menu-severity
  '("\
Severity: ?h)elp c)rit g)rave s)erious i)import RET-n)orm m)inor w)ish f)ixed"
    ;; NOTE: These function are automatically created, you don't find
    ;; then with C-s. See `tinydebian-install-severity-functions'
    ((?c .      ( (call-interactively 'tinydebian-severity-select-critical)))
     (?g .      ( (call-interactively 'tinydebian-severity-select-grave)))
     (?s .      ( (call-interactively 'tinydebian-severity-select-serious)))
     (?i .      ( (call-interactively 'tinydebian-severity-select-important)))
     (?n .      ( (call-interactively 'tinydebian-severity-select-normal)))
     (?\C-m .   ( (call-interactively 'tinydebian-severity-select-normal)))
     (?m .      ( (call-interactively 'tinydebian-severity-select-minor)))
     (?w .      ( (call-interactively 'tinydebian-severity-select-wishlist)))
     (?f .      ( (call-interactively 'tinydebian-severity-select-fixed)))))
  "Severity menu.

The bug system records a severity level with each bug report. This is set
to normal by default, but can be overridden either by supplying a Severity
line in the pseudo-header when the bug is submitted (see the instructions
for reporting bugs), or by using the severity command with the control
request server.

critical
    makes unrelated software on the system (or the whole system)
    break, or causes serious data loss, or introduces a security hole
    on systems where you install the package.

grave
    makes the package in question unuseable or mostly so, or causes
    data loss, or introduces a security hole allowing access to the
    accounts of users who use the package.

serious
    is a severe violation of Debian policy (that is, it violates a
    \"must\" or \"required\" directive), or, in the package
    maintainer's opinion, makes the package unsuitable for release.

important
    a bug which has a major effect on the usability of a package,
    without rendering it completely unusable to everyone.

normal
    the default value, applicable to most bugs.

minor
    a problem which doesn't affect the package's usefulness, and is
    presumably trivial to fix.

wishlist
    for any feature request, and also for any bugs that are very
    difficult to fix due to major design considerations.

fixed
    for bugs that are fixed but should not yet be closed. This is an
    exception for bugs fixed by non-maintainer uploads. Note: the
    \"fixed\" tag should be used instead. Certain severities are
    considered release-critical, meaning the bug will have an impact
    on releasing the package with the stable release of Debian.
    Currently, these are critical, grave and serious.")

(defvar tinydebian--removal-keyword-list
  '(
    "abandoned upstream"                ; Upstream no longer developing it
    "buggy"
    "dead upstream"                     ; No upstream at all an more
    "FTBFS"                             ; Fails to build from source
    "non-free"				; License problem (DFSG)
    "old"
    "orphaned (no maintainer)"
    "other alternatives"
    "rc-buggy"
    "rquested by upstream"              ; Upstream has requested removal
    "transitional pkg"
    "unredistributable"			; Incompatible Licenses (SSL + GPL...)
    "unmaintained"
    )
  "*List of PAckage removal keywords.
See http://wiki.debian.org/ftpmaster_Removals")

(defvar tinydebian--tags-list
  '(("already-in-ubuntu"
     "Package is in Ubuntu but not yet in Debian. This is a notice to a wishlist
See <http://utnubu.alioth.debian.org/>.xm")
    ("patch"
     "A patch or some other easy procedure for fixing the bug is included
in the bug logs. If there's a patch, but it doesn't resolve the bug
adequately or causes some other problems, this tag should not be used.")
    ("wontfix"
     "This bug won't be fixed. Possibly because this is a choice between
two arbitrary ways of doing things and the maintainer and submitter prefer
different ways of doing things, possibly because changing the behaviour
will cause other, worse, problems for others, or possibly for other reasons.")
    ("moreinfo"
     "This bug can't be addressed until more information is provided by
the submitter. The bug will be closed if the submitter doesn't provide
more information in a reasonable (few months) timeframe. This is for
bugs like "It doesn't work". What doesn't work?.")
    ("unreproducible"
     "This bug can't be reproduced on the maintainer's system.
Assistance from third parties is needed in diagnosing the cause of the problem.")
    ("help"
     "The maintainer is requesting help with dealing with this bug.")
    ("pending"
     "The problem described in the bug is being actively worked on,
i.e. a solution is pending.")
    ("fixed"
     "This bug is fixed or worked around (by a non-maintainer upload,
for example), but there's still an issue that needs to be resolved.
This tag replaces the old \"fixed\" severity.")
    ("security"
     "This bug describes a security problem in a package (e.g., bad
permissions allowing access to data that shouldn't be accessible;
buffer overruns allowing people to control a system in ways they
shouldn't be able to; denial of service attacks that should be fixed, etc).
Most security bugs should also be set at critical or grave severity.")
    ("upstream"
     "This bug applies to the upstream part of the package.")
    ("confirmed"
     "The maintainer has looked at, understands, and basically agrees
with the bug, but has yet to fix it. (Use of this tag is optional; it is
intended mostly for maintainers who need to manage large numbers of open bugs.")
    ("fixed-upstream"
     "The bug has been fixed by the upstream maintainer, but not yet
in the package (for whatever reason: perhaps it is too complicated to
backport the change or too minor to be worth bothering).")
    ("ipv6"
     "This bug affects support for Internet Protocol version 6.")
    ("lfs"
     "This bug affects support for large files (over 2 gigabytes).")
    ("l10n"
     "This bug is relevant to the localisation of the package.")
;;     ("sarge"
;;      "This bug particularly applies to the sarge distribution.")
;;     ("etch"
;;      "This bug particularly applies to the etch distribution.")
;;     ("sid"
;;      "This bug particularly applies to an architecture that is
;; currently unreleased (that is, in the sid distribution).")
    ("experimental"
     "This bug particularly applies to the experimental distribution."))
  "Each bug can have zero or more of a set of given tags.
These tags are displayed in the list of bugs when you look at a
package's page, and when you look at the full bug log.
See <http://www.debian.org/Bugs/Developer#tags>.")

(defvar tinydebian--wnpp-buffer "*TinyDebian WNPP*"
  "WNPP question buffer.")

(defvar tinydebian--menu-wnpp-selected nil
  "Placeholder of selection from `tinydebian--menu-wnpp'.")

(defconst tinydebian--menu-wnpp
  (list
   '(format
     "TinyDebian:WNPP%s 1i)tp 2o)rphan 3a)dopt 4n)ew package ?)help q)uit"
     (if tinydebian--menu-wnpp-selected
         (format ";%s " (symbol-name tinydebian--menu-wnpp-selected))
       ""))
   (list
    '(?1 . ( (setq tinydebian--menu-wnpp-selected 'package)))
    '(?i . ( (setq tinydebian--menu-wnpp-selected 'package)))
    '(?I . ( (setq tinydebian--menu-wnpp-selected 'package)))
    '(?p . ( (setq tinydebian--menu-wnpp-selected 'package)))
    '(?P . ( (setq tinydebian--menu-wnpp-selected 'package)))
    '(?2 . ( (setq tinydebian--menu-wnpp-selected 'oprhan)))
    '(?o . ( (setq tinydebian--menu-wnpp-selected 'oprhan)))
    '(?O . ( (setq tinydebian--menu-wnpp-selected 'oprhan)))
    '(?3 . ( (setq tinydebian--menu-wnpp-selected 'adopt)))
    '(?a . ( (setq tinydebian--menu-wnpp-selected 'adopt)))
    '(?A . ( (setq tinydebian--menu-wnpp-selected 'adopt)))
    '(?4 . ( (setq tinydebian--menu-wnpp-selected 'new)))
    '(?n . ( (setq tinydebian--menu-wnpp-selected 'new)))
    '(?N . ( (setq tinydebian--menu-wnpp-selected 'new)))))
  ;;  This message is straight from reportbug(1)
  ;;  'apt-get install reportbug'
  "What request type? If none of these things mean anything to you, or
you are trying to report a bug in an existing package)

1 p    ITP, `Intent To Package'. Please submit a package description
       along with copyright and URL in such a report.

2 o    The package has been 'Orphaned'. It needs a new maintainer as soon as
       possible.

3 a    RFA, this is a `Request for Adoption'. Due to lack of time, resources,
       interest or something similar, the current maintainer is asking for
       someone else to maintain this package. He/she will maintain it in the
       meantime, but perhaps not in the best possible way. In short: the
       package needs a new maintainer.

4 n    RFP, this is a `Request For Package'. You have found an interesting piece of
       software and would like someone else to maintain it for Debian. Please
       submit a package description along with copyright and URL in such a
       report.

q      Quit menu.
")

(defconst tinydebian--rfp-template "\
Package: wnpp
Severity: wishlist

* Package name    : <package>
  Version         : x.y.z
  Upstream Author : Name <somebody@example.org>
* URL             : <homepage: http://www.example.org/>
* License         : <license: GPL, LGPL, BSD, MIT/X, etc.>
  Programming Lang: <C, C++, C#, Perl, Python, etc.>
  Description     : <short desc>

\(Include the long description here.)
"
  "Wnpp RFP/ITP template.
NOTE: The <TAG:> constructs must be retained.")

(defvar tinydebian--rfp-hook nil
  "Hook run after function `tinydebian-bts-mail-type-rfp'.
See also `tinydebian--rfp-template'")

(defconst tinydebian--wnpp-template-licenses-alist
  '( ;; List updated 2008-11-03
    "Apache-2.0"
    "Artistic"
    "BSD"
    "GFDL"
    "GFDL-1.2"
    "GPL"
    "GPL-2"
    "GPL-3"
    "LGPL"
    "LGPL-2"
    "LGPL-2.1"
    "LGPL-3"
    ;; This is not mentioned, but include anyway
    "MIT/X11")
  "List of licenses as recorded in Debian /usr/share/common-licenses/
See also <http://www.debian.org/legal/licenses/> and
<http://people.debian.org/~bap/dfsg-faq.html>.")

(defconst tinydebian--rfs-template "\

I'm looking for sponsor:

  Package name    : <package>
  Version         : x.y.z
  ITA/ITP URL     : <ita: http://bugs.debian.org/BugNbr>
* Package bugs URL: <bugs: http://bugs.debian.org/Package>
  URL             : <mentors: http://mentors.debian.net/debian/pool/main/p/package/*.dsc>
  License         : <license: GPL, LGPL, BSD, MIT/X, Artistic, etc.>
  Programming Lang: <C, C++, C#, Perl, Python, etc.>

\(* = remove if package is not in Debian repository.)
Description:

debian/changelog:

Other notes:
"
  "RFS message to debian.devel.mentor mailing list.
NOTE: The <TAG:> constructs must be retained and are replaced in message:

  Package name    : <package>
  Version         : x.y.z
  ITA/ITP URL     : <ita: http://bugs.debian.org/BugNbr>
* Package bugs URL: <bugs: http://bugs.debian.org/Package>
  URL             : <mentors: http://mentors.debian.net/debian/pool/main/p/package/*.dsc>
  License         : <license: GPL, LGPL, BSD, MIT/X, Artistic, etc.>
  Programming Lang: <C, C++, C#, Perl, Python, etc.>

See also `tinydebian--rfs-hook'.")

(defvar tinydebian--rfs-hook nil
  "Hook run after function `tinydebian-bts-mail-type-rfs'.
See also `tinydebian--rfs-template'")

(defvar tinydebian--debian-bts-email-address "bugs.debian.org"
  "Email address for Debian Bug Tracking System.")

;; https://help.launchpad.net/UsingMaloneEmail
(defvar tinydebian--launchpad-email-address "bugs.launchpad.net"
  "Email address for Launchpad Bug Tracking System.")

(defvar tinydebian--launchpad-email-address "bugs.launchpad.net"
  "Email address for Launchpad Bug Tracking System.
See <https://help.launchpad.net/Bugs/EmailInterface?action=show&redirect=BugTrackerEmailInterface>.")

(defconst tinydebian--launchpad-url-http-package-bugs
  "https://bugs.launchpad.net/%s/+bug/%s"
  "HTTP address of an individual bug in Launchpad.
First %s is placeholder for the PACKAGE.
Second %s is placeholder for the BUG number.")

(defconst tinydebian--gnu-savannah-url-http-package-bugs
  "http://savannah.gnu.org/bugs"
  "The savanah.gnus.org bug URL without parameter")

(defconst tinydebian--gnu-savannah-url-http-site-support-bugs
  "http://savannah.gnu.org/support"
  "The savanah.gnus.org site request URL without parameter")

(defvar tinydebian--gnu-bts-email-address "debbugs.gnu.org"
  "Email address for GNU Bug Tracking System.
See <http://debbugs.gnu.org>.")

(defvar tinydebian--gnu-bts-url-http-bugs
  ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6004
  "http://debbugs.gnu.org/%s"
  "HTTP address of an individual bug in Emacs Bug Tracking System.
The %s is placeholder for a bug number.")

(defvar tinydebian--emacs-bts-email-address "debbugs.gnu.org"
  "Email address for Emacs Bug Tracking System.")

(defvar tinydebian--emacs-bts-url-http-bugs
  "http://debbugs.gnu.org/%s"
  "HTTP address of an individual bug in Emacs Bug Tracking System.
The %s is placeholder for a bug number.")

(defvar tinydebian--emacs-bts-line-regexp
  "Emacs.*Bug#\\([0-9]+\\)"
  "Regexp to return Emacs BTS bug number from submatch 1.
E.g. in `gnus-summary-mode':

   [  16: Emacs bug Tracking Syst] Processed: Bug#2217 Change of severity / minor")

(defvar tinydebian--list-email-address "lists.debian.org"
  "Email address or Debian mailing lists.")

(defvar tinydebian--url-http-package-search
  ;; http://packages.debian.net/search?keywords=chbg&searchon=names
  "http://packages.debian.net/search?"
  "The packages Debian control URL without parameter, up to '?' token.")

(defconst tinydebian--debian-url-http-package-bugs
  "http://bugs.debian.org/%s"
  "The bugs Debian control URL without parameter, up to '/' token.")

(defvar tinydebian--debian-url-http-www
  "http://www.debian.org"
  "The main WWW page of Debian.")

(defvar tinydebian--debian-url-http-wnpp-page-main
  "http://www.debian.org/devel/wnpp"
  "The WNPP main page URL address. No trailing slash.")

(defconst tinydebian--debian-url-http-wnpp-page-alist
  '(("RFA" . "rfa_bypackage")
    ("O"   . "orphaned")
    ("RFH" . "help_request")
    ("RFP" . "requested")
    ("ITP" . "being_packaged"))
  "List of pages under `tinydebian--debian-url-http-wnpp-page-main'.")

;; FIXME: Not yet used
(defvar tinydebian--bts-compose-type
  "Dynamically bound variable which is set during BTS Control actions.
Values: nil ('debian'), 'emacs', 'launchpad'.")

(defconst tinydebian--debian-url-page-alist
  (list
   '(bts-control
     "http://www.debian.org/Bugs/server-control")
   ;; 2006-11-06 unofficial
   (list 'bugs-rc
         "http://bts.turmzimmer.net/details.php"
         tinydebian--font-lock-keywords-bugs-rc)
   '(qa-developer-status
     "http://qa.debian.org/developer.php?")
   '(qa-developer-bugs
     "http://bugs.debian.org/cgi-bin/pkgreport.cgi?")
   '(dfsg-license-faq
     "http://people.debian.org/~bap/dfsg-faq.html")
   '(base-files-faq
     "http://ftp.debian.org/doc/base-files/FAQ")
   '(debcheck-package
     "http://qa.debian.org/debcheck.php?dist=%s&package=%s")
   '(mentors
     "http://mentors.debian.net")
   '(mentors-pkg-pool
     "http://mentors.debian.net/debian/pool")
   '(pkg-search-files
     "http://packages.debian.org/cgi-bin/search_contents.pl?searchmode=searchfiles&case=insensitive")
   '(developers-reference
     "http://www.debian.org/doc/packaging-manuals/developers-reference/")
   ;;  apt-get install debian-reference-common debian-reference-en
   '(developers-reference-text
     "/usr/share/doc/Debian/reference/reference.en.txt.gz")
   '(policy
     "http://www.debian.org/doc/debian-policy/index.html")
   '(policy-text
     "/usr/share/doc/debian-policy/policy.txt.gz")
   '(newmaint-guide
     "http://www.debian.org/doc/maint-guide/")
   '(best-practises
     "http://www.debian.org/doc/packaging-manuals/developers-reference/ch-best-pkging-practices.en.html"))
  "List of Debian site pages.
Format:
 '((PAGE-TYPE  URL [FONT-LOCK-KEYWORDS])
   ...)

The FONT-LOCK-KEYWORDS is only used if the results appear in
`tinydebian--buffer-www'. See `tinydebian--browse-url-function'.")

(defvar tinydebian--debian-virtual-package-list
  ;; Updated 2008-03-08
  '(("awk" "a suitable /usr/bin/{awk,nawk} (*)")
    ("c-shell" "a suitable /bin/csh (*)")
    ("dotfile-module" "a module for the Dotfile Generator")
    ("emacsen" "the GNU emacs or a compatible editor")
    ("lzh-archiver" "an LZH archiver package")
    ("tclsh" "a /usr/bin/tclsh (*)")
    ("wish" "a /usr/bin/wish (*)")
    ("c-compiler" "a C compiler")
    ("debconf-2.0" "the debconf protocol")
    ("fortran77-compiler" "a Fortran77 compiler")
    ("kernel-headers" "kernel header files (<linux/*.h>, <asm/*.h>)")
    ("kernel-image" "kernel image (vmlinuz, System.map, modules)")
    ("kernel-source" "kernel source code")
    ("libc-dev" "header and object files of `libc'")
    ("flexmem" "anything that can access flexible memory via the")
    ("foomatic-data" "PPD printer description files")
    ("linux-kernel-log-daemon" "a daemon to facilitate logging for the Linux kernel")
    ("system-log-daemon" "a daemon that provides a logging facility for")
    ("time-daemon" "anything that serves as a time daemon")
    ("ups-monitor" "anything that is capable of controlling an UPS")
    ("dict-client" "clients for the Dictionary Server")
    ("dict-server" "the Dictionary Server")
    ("dictd-dictionary" "a dictionary for the dictd Dictionary Server")
    ("info-browser" "something that can browse GNU Info files")
    ("ispell-dictionary" "a dictionary for the ispell system")
    ("myspell-dictionary" "a dictionary for the myspell system")
    ("man-browser" "something that can read man pages")
    ("stardict-dictionary" "a dictionary for stardict")
    ("stardict" "application capable of reading stardict-dictdata")
    ("stardict-dictdata" "dictionary data which can be read from stardict")
    ("wordlist" "a /usr/share/dict/words (*)")
    ("www-browser" "something that can browse HTML files")
    ("dhcp-client" "a DHCP client")
    ("ftp-server" "a FTP server")
    ("httpd" "a HTTP server")
    ("httpd-cgi" "A CGI capable HTTP server")
    ("ident-server" "an identd daemon")
    ("inet-superserver" "an inetd server")
    ("lambdamoo-core" "a lambdamoo-compatible database package")
    ("lambdamoo-server" "anything running a moo using a lambdamoo-core")
    ("radius-server" "a RADIUS server for acct/auth")
    ("rsh-client" "an rsh client")
    ("rsh-server" "an rsh server")
    ("telnet-client" "a telnet client")
    ("telnet-server" "a telnet server")
    ("imap-client" "a mail reader capable of accessing remote mail")
    ("imap-server" "an IMAP mail server")
    ("mail-reader" "a mail user agent (e.g. Pine, Elm, mailx, &c)")
    ("mail-transport-agent" "a mail transport agent (e.g. Smail, Sendmail, &c)")
    ("news-reader" "a news reader (e.g. trn, tin, &c)")
    ("news-transport-system" "a local news system (e.g. INN, C News or B News)")
    ("pgp" "a version of PGP (International or US)")
    ("pop3-server" "a POP3 Server")
    ("x-display-manager" "an X client which manages a collection of X servers")
    ("x-session-manager" "a program which starts a desktop environment")
    ("x-terminal-emulator" "an X client which emulates a terminal with a")
    ("x-window-manager" "an X client which provides window management")
    ("xserver" "an X server that (directly or indirectly) manages")
    ("ttf-japanese-gothic" "Gothic-style Japanese font")
    ("ttf-japanese-mincho" "Mincho-style Japanese font")
    ("audio-mixer" "a utility to control the input and output levels")
    ("x-audio-mixer" "a utility to control the input and output levels")
    ("mp3-encoder" "an MP3 encoder package")
    ("mp3-decoder" "an MP3 decoder package")
    ("mpd-client" "a client that can control the Music Player Daemon")
    ("pdf-preview" "a preprocessor that creates PDF output")
    ("pdf-viewer" "anything that can display PDF files")
    ("postscript-preview" "a preprocessor that creates Postscript output")
    ("postscript-viewer" "anything that can display Postscript files")
    ("java-compiler" "a java compiler, for Java version 1")
    ("java2-compiler" "a java compiler, for Java version 2")
    ("java-virtual-machine" "a JAVA virtual machine")
    ("java1-runtime" "a Java runtime environment, Java version 1")
    ("java2-runtime" "a Java runtime environment, Java version 2")
    ("scheme-r4rs" "Scheme interpreter with the R4RS environment")
    ("scheme-r5rs" "Scheme interpreter with the R5RS environment")
    ("scheme-ieee-11878-1900" "Scheme interpreter with the IEEE-11878-1900")
    ("scheme-srfi-0" "Scheme interpreter accepting the SRFI 0 language")
    ("scheme-srfi-7" "Scheme interpreter accepting the SRFI 7 language")
    ("scheme-srfi-55" "Scheme interpreter accepting the SRFI 55 language"))
  "List of virtual packages.

To refresh:
  wget http://www.debian.org/doc/packaging-manuals/virtual-package-names-list.txt
  C-x C-f virtual-package-names-list.txt
  M-x tinydebian-virtual-package-parse-buffer")

(defun tinydebian-virtual-package-parse-buffer ()
  "Parse content of virtual package file.
This is strictly maintainer's function. Download the file
mentioned in `tinydebian--debian-virtual-package-list' and run this function
to generate updated list."
  (interactive)
  (let (beg
        end
        package
        desc
        list)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^Miscellaneous[ \t\r\n]+------" nil t)
        (error "Not content of virtual-package-names-list.txt"))
      (setq beg (point))
      (unless (re-search-forward "^Old and obsolete" nil t)
        (error "Not end marker of virtual-package-names-list.txt"))
      (setq end (line-beginning-position))
      (goto-char beg)
      (while (re-search-forward
              "^ \\([^ \t\r\n]+\\)[ \t]+\\(.+[^ \t\r\n]\\)" end t)
        (setq package (match-string 1)
              desc    (match-string 2))
        (push (list package desc) list))
      (when list
        (let* ((name "*virtual-packages*")
               (buffer (get-buffer-create name)))
          (pop-to-buffer buffer)
          (erase-buffer)
          (insert (pp (nreverse list))))))))

;;}}}
;;{{{ Install: bindings

;;; ........................................................ &bindings ...

;; #todo:
(defun tinydebian-default-bindings ()
  "Define default key bindings to `tinydebian-mode-map'.")

(eval-and-compile

;;;###autoload (autoload 'tinydebian-bts-mode          "tinydebian" "" t)
;;;###autoload (autoload 'turn-on-tinydebian-bts-mode  "tinydebian" "" t)
;;;###autoload (autoload 'turn-off-tinydebian-bts-mode "tinydebian" "" t)
;;;###autoload (defvar tinydebian--bts-mode-prefix-key "\C-c-")
  (ti::macrof-minor-mode-wizard
   "tinydebian-bts-" " Tdeb" "\C-c-" "Tdeb" 'TinyDebian "tinydebian--bts-" ;1-6

   "Debian Bug Tracking System (BTS) Minor mode. With this mode you can
jump to a bug report at or near current point (using browser), send
control messages, like turning RFS into ITP, send new RFS, send new
ITP etc.

Prefix key is:

  tinydebian--bts-mode-prefix-key

Mode description:

\\{tinydebian--bts-mode-prefix-map}"

   "TinyDebian BTS"

   nil

   "TinyDebian BTS minor mode menu."

   (list
    tinydebian--bts-mode-easymenu-name
    ["Reply to a bug"                tinydebian-bts-mail-type-reply         t]
    ["Report a new bug"              tinydebian-bug-report-generic-bts-mail t]

    "----"

;;    ["Goto URL by bug number"    tinydebian-bug-browse-url-by-bug          t]
    ["Goto bug URL"              tinydebian-bug-browse-url-main            t]
    ["Goto URL by package bugs"  tinydebian-bug-browse-url-by-package-bugs t]
    ["Goto URL by package name"  tinydebian-bug-browse-url-by-package-name t]

    "----"

    (list
     "BTS WNPP messages"
     ["Send BTS ITA: intent to adopt"      tinydebian-bts-mail-type-ita    t]
     ["Send BTS ITN: intent to NMU"        tinydebian-bts-mail-type-it-nmu t]
     ["Send BTS ITP: reponse to RFP"       tinydebian-bts-mail-type-itp    t]
     ["Send BTS RFA: request for adopt"    tinydebian-bts-mail-type-rfa    t]
     ["Send BTS RFH: request for help"     tinydebian-bts-mail-type-rfh    t]
     ["Send BTS RFP/ITP: request for packege" tinydebian-bts-mail-type-rfp    t]
     ["Send BTS RFS: request for sponsor"  tinydebian-bts-mail-type-rfs    t]
     ["Send BTS O: orphan"                 tinydebian-bts-mail-type-orphan t]
;;     ["Send BTS RM: remove"                tinydebian-bts-mail-type-remove t]
     ["WNPP control menu"                  tinydebian-package-wnpp-main    t])

    (list
     "BTS Control messages"
     ["Send BTS Ctrl affects"      tinydebian-bts-mail-ctrl-affects  t]
     ["Send BTS bug subscribe"     tinydebian-bts-mail-ctrl-bug-subscribe t]
     ["Send BTS bug unsubscribe"   tinydebian-bts-mail-ctrl-bug-unsubscribe t]
     ["Send BTS bug pkg subscribe" tinydebian-bts-mail-ctrl-bug-package-subscribe t]
     ["Send BTS bug pkg unsubscribe" tinydebian-bts-mail-ctrl-bug-package-unsubscribe t]
     ["Send BTS Ctrl clone"        tinydebian-bts-mail-ctrl-clone    t]
     ["Send BTS Ctrl close"        tinydebian-bts-mail-ctrl-close    t]
     ["Send BTS Ctrl severity"     tinydebian-bts-mail-ctrl-severity t]
     ["Send BTS Ctrl tags"         tinydebian-bts-mail-ctrl-tags     t]
     ["Send BTS Ctrl usertag"      tinydebian-bts-mail-ctrl-usertag  t]
     ["Send BTS Ctrl forward"      tinydebian-bts-mail-ctrl-forward-main  t]
     ["Send BTS Ctrl notforwarded" tinydebian-bts-mail-ctrl-notforwarded t]
     ["Send BTS Ctrl forwarded"    tinydebian-bts-mail-ctrl-forwarded-main  t]
     ["Send BTS Ctrl fixed"        tinydebian-bts-mail-ctrl-fixed    t]
     ["Send BTS Ctrl notfixed"     tinydebian-bts-mail-ctrl-notfixed t]
     ["Send BTS Ctrl owner"        tinydebian-bts-mail-ctrl-owner    t]
     ["Send BTS Ctrl noowner"      tinydebian-bts-mail-ctrl-noowner  t]
     ["Send BTS Ctrl found"        tinydebian-bts-mail-ctrl-found    t]
     ["Send BTS Ctrl notfound"     tinydebian-bts-mail-ctrl-notfound t]
     ["Send BTS Ctrl merge"        tinydebian-bts-mail-ctrl-merge    t]
     ["Send BTS Ctrl reassign"     tinydebian-bts-mail-ctrl-reassign t]
     ["Send BTS Ctrl retitle"      tinydebian-bts-mail-ctrl-retitle  t]
     ["Send BTS Ctrl reopen"       tinydebian-bts-mail-ctrl-reopen   t]
     ["Send BTS Ctrl remove ROM"   tinydebian-bts-mail-ctrl-remove-package t]
;;;     ["Send BTS Ctrl unarchive"    tinydebian-bts-mail-ctrl-command-unarchive t]
     )

    (list
     "Query information"
     ["Bug title (subject)"        tinydebian-debian-bug-info-subject-message  t]
     ["Bug all info (details)"     tinydebian-debian-bug-info-all-message  t]

     "----"

     ["List of WNPP RFP"           tinydebian-url-list-wnpp-rfp            t]
     ["List of WNPP RFH"           tinydebian-url-list-wnpp-rfh            t]
     ["List of WNPP RFA"           tinydebian-url-list-wnpp-rfa            t]
     ["List of WNPP Orphaned"      tinydebian-url-list-wnpp-orphaned       t]
     ["List of WNPP ITP"           tinydebian-url-list-wnpp-itp            t]
     ["List of RC bugs"            tinydebian-url-list-bugs-by-rc          t]
     ["List of items by usertag"   tinydebian-url-list-bugs-by-usertag     t]

     "----"

     ["Installed wnpp problems"    tinydebian-command-show-wnpp-alert      t]
     ["Installed rc problems"      tinydebian-command-show-rc-alert        t]
     ["Grep devel documentation"   tinydebian-grep-find-debian-devel       t]

     "----"

     ["QA Developer status"        tinydebian-url-list-qa-developer-status t]
     ["QA Developer bugs"          tinydebian-url-list-qa-developer-bugs   t]
     ["Package debcheck"           tinydebian-url-list-package-debcheck    t]
     ["Package search by name"     tinydebian-url-list-package-by-package-name t]
     ["Package search by filename" tinydebian-url-list-package-by-filename t]

     "----"

     ["FAQ DFSG and licenses"      tinydebian-url-list-dsfg-license-faq    t]
     ["FAQ base files"             tinydebian-url-list-base-files-faq      t])

    (list
     "Debian manuals"
     ["URL BTS Ctrl page"            tinydebian-url-bts-ctrl-page         t]
     ["URL Policy manual"            tinydebian-url-policy-manual         t]
     ["URL Newmaint guide"           tinydebian-url-policy-new-maintainer-guide  t]
     ["URL Developer's reference"    tinydebian-url-policy-developers-reference  t]
     ["URL Best practises"           tinydebian-url-policy-best-practises t]))

   (progn

     ;; Replaces tinydebian-bug-browse-url-by-bug
     (define-key map  "b"  'tinydebian-bug-browse-url-main)

     (define-key map  "B"  'tinydebian-bug-browse-url-by-package-bugs)
     (define-key map  "M"  'tinydebian-bug-report-generic-bts-mail)
     (define-key map  "p"  'tinydebian-bug-browse-url-by-package-name)
     (define-key map  "r"  'tinydebian-bts-mail-type-reply )
     (define-key map  "w"  'tinydebian-package-wnpp-main)

     (define-key map  "-a" 'tinydebian-bts-mail-type-ita)
     (define-key map  "-A" 'tinydebian-bts-mail-type-rfa)
     (define-key map  "-h" 'tinydebian-bts-mail-type-rfh)
     (define-key map  "-n" 'tinydebian-bts-mail-type-it-nmu)
     (define-key map  "-o" 'tinydebian-bts-mail-type-orphan)
     (define-key map  "-P" 'tinydebian-bts-mail-type-itp)
     (define-key map  "-p" 'tinydebian-bts-mail-type-rfp)
     (define-key map  "-r" 'tinydebian-bts-mail-type-reply)
;;     (define-key map  "-R" 'tinydebian-bts-mail-type-remove)
     (define-key map  "-s" 'tinydebian-bts-mail-type-rfs)

     (define-key map  "mi" 'tinydebian-bts-mail-message-info)

     ;;  (i)nfo (i)nstalled
     (define-key map  "ia" 'tinydebian-debian-bug-info-all-message)
     (define-key map  "ii" 'tinydebian-command-show-wnpp-alert)
     (define-key map  "ig" 'tinydebian-grep-find-debian-devel) ;grep
     ;; Release Critical
     (define-key map  "ir" 'tinydebian-command-show-rc-alert)
     (define-key map  "is" 'tinydebian-debian-bug-info-subject-message)

     ;;  (L)ist Url commands
     ;; (b)ugs
     (define-key map  "lbr"  'tinydebian-url-list-bugs-by-rc)
     (define-key map  "lbu"  'tinydebian-url-list-bugs-by-usertag)
     ;; (d)eveloper
     (define-key map  "ldb"  'tinydebian-url-list-qa-developer-bugs)
     (define-key map  "lds"  'tinydebian-url-list-qa-developer-status)
     ;; (f)aq
     (define-key map  "lfl"  'tinydebian-url-list-dsfg-license-faq)
     (define-key map  "lfb"  'tinydebian-url-list-base-files-faq)
     ;; (p)ackage
     (define-key map  "lpf"  'tinydebian-url-list-package-by-filename)
     (define-key map  "lpp"  'tinydebian-url-list-package-by-package-name)
     (define-key map  "lpc"  'tinydebian-url-list-package-debcheck)
     ;; (w)npp
     (define-key map  "lwa"  'tinydebian-url-list-wnpp-rfa)
     (define-key map  "lwh"  'tinydebian-url-list-wnpp-rfh)
     (define-key map  "lwo"  'tinydebian-url-list-wnpp-orphaned)
     (define-key map  "lwp"  'tinydebian-url-list-wnpp-rfp)
     (define-key map  "lwP"  'tinydebian-url-list-wnpp-itp)

     ;;  (C)ontrol commands alphabetically
     (define-key map  "ca"  'tinydebian-bts-mail-ctrl-affects)
     (define-key map  "cbs"  'tinydebian-bts-mail-ctrl-bug-subscribe)
     (define-key map  "cbu"  'tinydebian-bts-mail-ctrl-bug-unsubscribe)
     (define-key map  "cbp"  'tinydebian-bts-mail-ctrl-bug-package-subscribe)
     (define-key map  "cbP"  'tinydebian-bts-mail-ctrl-bug-package-unsubscribe)

     (define-key map  "cc"  'tinydebian-bts-mail-ctrl-close)
     (define-key map  "cC"  'tinydebian-bts-mail-ctrl-clone)

     (define-key map  "cff" 'tinydebian-bts-mail-ctrl-forward-main)
     (define-key map  "cfn" 'tinydebian-bts-mail-ctrl-notforwarded)
     (define-key map  "cF"  'tinydebian-bts-mail-ctrl-forwarded-main)

     (define-key map  "cm"  'tinydebian-bts-mail-ctrl-merge)

     (define-key map  "cn"  'tinydebian-bts-mail-ctrl-found)
     (define-key map  "cN"  'tinydebian-bts-mail-ctrl-notfound)
     (define-key map  "co"  'tinydebian-bts-mail-ctrl-reopen)
     (define-key map  "cr"  'tinydebian-bts-mail-ctrl-reassign)
     (define-key map  "cR"  'tinydebian-bts-mail-ctrl-retitle)
     (define-key map  "\C-r" 'tinydebian-bts-mail-ctrl-remove-package)
     (define-key map  "cs"  'tinydebian-bts-mail-ctrl-severity)
     (define-key map  "ct"  'tinydebian-bts-mail-ctrl-tags)
     (define-key map  "cT"  'tinydebian-bts-mail-ctrl-usertag)
     (define-key map  "cw"  'tinydebian-bts-mail-ctrl-owner)
     (define-key map  "cW"  'tinydebian-bts-mail-ctrl-noowner)
     (define-key map  "cx"  'tinydebian-bts-mail-ctrl-fixed)
     (define-key map  "cX"  'tinydebian-bts-mail-ctrl-notfixed)

     ;;  (U)RLs
     (define-key map  "ub"  'tinydebian-url-bts-ctrl-page)
     (define-key map  "ud"  'tinydebian-url-policy-developers-reference)
     (define-key map  "un"  'tinydebian-url-policy-new-maintainer-guide)
     (define-key map  "up"  'tinydebian-url-policy-manual)
     (define-key map  "uP"  'tinydebian-url-policy-best-practises))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mode-gnus-summary-maybe-turn-on ()
  "Activate tinydebian-bts-mode if group name contains word 'Debian'"
;;   (when (and (boundp 'gnus-newsgroup-name)
;;              (stringp gnus-newsgroup-name)
;;             (string-match "debian" gnus-newsgroup-name))
  (turn-on-tinydebian-bts-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mode-maybe-turn-on ()
  "Activate tinydebian-bts-mode in certain buffers.
Buffer should contains a word 'Debian' or 'Emacs'."
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "debian\\|emacs" nil t))
    (turn-on-tinydebian-bts-mode)))

;;}}}
;;{{{ Install: generate severity function etc.

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install-severity-functions ()
  "Generate `tinydebian-severity-select-*' user functions."
  ;; Generate functions on run-time.
  (mapcar
   (function
    (lambda (x)
      (let ((sym (intern (format "tinydebian-severity-select-%s"  x)))
            def)
        (setq def
              `(defun ,sym ()
                 "Set Severity level `tinydebian--severity-selected'."
                 (interactive)
                 (setq  tinydebian--severity-selected ,x)))
        (eval def))))
   '("critical"
     "grave"
     "serious"
     "important"
     "normal"
     "minor"
     "wishlist"
     "fixed")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-find-file-hook ()
  "Run `tinydebian-bts-mode-maybe-turn-on'.
Install `font-lock-keywords' for log files."
  (tinydebian-bts-mode-maybe-turn-on)
  (tinydebian-font-lock-keywords))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install-font-lock-keywords (&optional uninstall)
  "Install colors to all current buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (tinydebian-font-lock-keywords uninstall))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-install-in-buffers (&optional uninstall)
  "Install or UNINSTALL `tinydebiab-bts-mode' in existing buffers.
Run also `tinydebian-mail-mode-debian-default-keybindings' in all mail buffer.
Activate on Gnus summary and article modes if there is word 'Debian'.
Activate on files whose path matches
`tinydebian--install-buffer-file-name-regexp'."
  (flet ((search (regexp)
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward regexp nil t))))
    (dolist (buffer (buffer-list))
      (let (doit)
        (with-current-buffer buffer
          (when (memq major-mode '(message-mode mail-mode))
            (if uninstall
                (turn-off-tinydebian-bts-mode)
              (turn-on-tinydebian-bts-mode)))
          (cond
           ((and (stringp buffer-file-name)
                 (string-match tinydebian--install-buffer-file-name-regexp
                               buffer-file-name))
            (setq doit t))
           ((and (eq major-mode 'gnus-summary-mode)
                 (boundp 'gnus-newsgroup-name)
                 (string-match
                  tinydebian--install-gnus-newsgroup-name-regexp
                  gnus-newsgroup-name))
            (setq doit t))
           ((and (eq major-mode 'gnus-article-mode)
                 (search "debian"))
            (setq doit t))
           ((search (concat
                     "bug#[0-9][0-9][0-9][0-9][0-9][0-9]\\>"
                     "\\|Closes +#[0-9][0-9][0-9][0-9][0-9][0-9]"))
            (setq doit t)))
          (if uninstall
              (turn-off-tinydebian-bts-mode)
            (turn-on-tinydebian-bts-mode)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-install (&optional uninstall)
  "Install or UNINSTALL package."
  (interactive "P")
  ;;  This just hides from byte compiler function definition so that
  ;;  it does not remember how amny arguments it takes
  ;;
  ;;  function tinydebian-bug-report-debian-bts-mail used to take 0+
  ;;  arguments, now takes 1 function
  ;;  tinydebian-bug-report-debian-bts-mail defined multiple times in
  ;;  this file
  ;;
  (cond
   (uninstall
    (tinydebian-install-font-lock-keywords 'uninstall)
    (remove-hook 'message-mode-hook
                 'turn-on-tinydebian-mail-mode)
    (remove-hook 'mail-mode-hook
                 'turn-on-tinydebian-mail-mode)
    (remove-hook 'find-file-hook
                 'tinydebian-find-file-hook)
    (remove-hook 'gnus-summary-prepare-hook
                 'tinydebian-bts-mode-gnus-summary-maybe-turn-on)
    (remove-hook 'gnus-article-prepare-hook
                 'tinydebian-bts-mode-maybe-turn-on)
    (tinydebian-install-in-buffers 'uninstall))
   (t
    (tinydebian-install-font-lock-keywords)
    (add-hook 'message-mode-hook
              'turn-on-tinydebian-mail-mode)
    (add-hook 'mail-mode-hook
              'turn-on-tinydebian-mail-mode)
    (add-hook 'find-file-hook
              'tinydebian-find-file-hook)
    (add-hook 'gnus-summary-prepare-hook
              'tinydebian-bts-mode-gnus-summary-maybe-turn-on)
    (add-hook 'gnus-article-prepare-hook
              'tinydebian-bts-mode-maybe-turn-on)
    (tinydebian-install-in-buffers)))
  nil)

;;}}}
;;{{{ Utility functions

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-gnus-article-buffer 'lisp-indent-function 1)
(put 'tinydebian-with-gnus-article-buffer 'edebug-form-spec '(body))
(defmacro tinydebian-with-gnus-article-buffer (&optional nbr &rest body)
  "In article NBR, run BODY. Defaults to article at *Summary* buffer."
  (let ((buffer (gensym "buffer-")))
    `(save-window-excursion
       (gnus-summary-select-article 'all nil 'pseudo ,nbr)
       (let ((,buffer (get-buffer gnus-original-article-buffer)))
         (when ,buffer
           (with-current-buffer ,buffer
             ,@body))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-bug-context 'lisp-indent-function 0)
(put 'tinydebian-with-bug-context 'edebug-form-spec '(body))
(defmacro tinydebian-with-bug-context (&rest body)
  "In Gnus Group buffer change to original article and run BODY."
  `(cond
    ((eq major-mode 'gnus-summary-mode)
     (tinydebian-with-gnus-article-buffer nil
       (goto-char (point-min))
       ,@body))
    (t
     ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-at-current-point ()
  "Read bug number from current point."
  (or (tinydebian-bug-nbr-at-current-point-hash)
      (tinydebian-bug-nbr-at-current-point-number)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-any-in-string (string)
  "Read bug number NNNNNN from STRING."
  (when (stringp string)
    (if (or (string-match
	     "\\(?:[^0-9]+\\|^\\)\\([0-9][0-9][0-9][0-9][0-9]\\)$"
	     string)
	    (string-match "#\\([0-9][0-9]+\\)" string))
        (match-string 1 string))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-any-at-current-point ()
  "Read bug number NNNNNN from current point."
  (tinydebian-bug-nbr-any-in-string (current-word)))


;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-current-line-string ()
  "Return current line."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-any-at-current-line ()
  "Read bug number NNNNNN from current line."
  (tinydebian-bug-nbr-any-in-string
   (tinydebian-current-line-string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-current-line ()
  "Read bug number from current line"
  (let ((line (tinydebian-current-line-string)))
    (tinydebian-bug-nbr-string line)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-buffer-match-string (regexp &optional point)
  "Search REGEX at optional POINT and return submatch 1."
  (save-excursion
    (if point
        (goto-char point))
    (if (re-search-forward regexp nil t)
        (or (match-string 1)
            (match-string 2)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-forward (&optional regexp)
  "Read bug#NNNN from current point forward.
If optional REGEXP is set, it must take number in submatch 1."
  (tinydebian-buffer-match-string (or regexp "[Bb]ug#\\([0-9]+\\)")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-mime-clean (bug)
  "Remove 3D or other mime token from BUG number."
  (cond
   ((and (stringp bug)
         (string-match "^3D\\(.+\\)" bug))
    (setq bug (match-string 1 bug))))
  bug)

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-debian-url-forward ()
  "Read http://bugs.debian.*NNNN bug number from current point forward."
  (let ((str
         (tinydebian-buffer-match-string
          `,(concat
             "http://bugs\\.debian\\.org/\\([0-9][0-9][0-9]+\\)"
             ;; This URL contatains "mime" 3D escape
             ;; http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=3D515275
             "\\|http://bugs\\.debian\\.org/.*bug=\\([0-9][^ \t\r\n]+\\)"))))
    (tinydebian-bug-nbr-mime-clean str)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-hash-forward ()
  "Search #NNNN forward."
  (tinydebian-bug-nbr-forward "#\\([0-9]+\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-buffer (&optional regexp)
  "Read bug#NNNN or REGEXP from buffer."
  (save-excursion
    (goto-char (point-min))
    (tinydebian-bug-nbr-forward)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-cc-to-bug-nbr ()
  "Read BTS number from CC or To"
  (let ((str (mail-fetch-field "To")))
    (or (and str
             (tinydebian-bug-nbr-string str))
        (and (setq str (mail-fetch-field "Cc"))
             (tinydebian-bug-nbr-string str)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-insert-at-point ()
  "Insert bug number at point (if anything found)."
  (interactive)
  (let ((nbr (or (tinydebian-email-cc-to-bug-nbr)
                 (tinydebian-bug-nbr-any))))
    (if nbr
        (insert nbr))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-hash-buffer ()
  "Search #NNNN from buffer."
  (tinydebian-bug-nbr-buffer "#\\([0-9]+\\)"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-subject-bug-nbr ()
  "Read BTS number from Subject"
  (let ((subject (mail-fetch-field "Subject")))
    (and subject
         (tinydebian-bug-nbr-string subject))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-trim-blanks (str)
  (if (not (stringp str))
      str
    (cond
     ((or (string-match "\\([^ \t\r\n].*[^ \t\r\n]\\)" str)
          (string-match "\\([^ \t\r\n]\\)" str))
      (match-string 1 str))
     (t
      ""))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-type-debbugs-p (bts)
  "Return non-nil if BTS type is debbugs."
  (and (stringp bts)
       (string-match "debian\\|emacs\\|gnu" bts)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-sourceware-bts-url-compose (bug)
  "Return Gnome URL for BUG."
  (format "http://sourceware.org/bugzilla/show_bug.cgi?id=%s" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mysql-bts-url-compose (bug)
  "Return MySQL URL for BUG."
  (format "http://forge.mysql.com/worklog/task.php?id=%s" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-google-code-bts-url-compose (project bug)
  "Return code.google.com issue URL for PROJECT BUG."
  (format "http://code.google.com/%s/issues/detail?id=%s"
          project bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnome-bts-url-compose (bug)
  "Return Gnome URL for BUG."
  (format "https://bugzilla.gnome.org/show_bug.cgi?id=%s" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-kde-bts-url-compose (bug)
  "Return KDE URL for BUG."
  (format "https://bugs.kde.org/show_bug.cgi?id=%s" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-bts-url-compose (string)
  "Return Debian URL for STRING.
String is anything that is attached to
`tinydebian--debian-url-http-package-bugs'"
  (format tinydebian--debian-url-http-package-bugs string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-launchpad-email-compose (address)
  "Send message to ADDRESS@<launchpad>."
  (format "%s@%s" address tinydebian--launchpad-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnu-bts-email-compose (address)
  "Send message to ADDRESS@<gnu bts>."
  (format "%s@%s" address tinydebian--gnu-bts-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-emacs-bts-email-compose (address)
  "Send message to ADDRESS@<emacs bts>."
  (format "%s@%s" address tinydebian--emacs-bts-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnu-bts-bug-url-compose (bug)
  "Compose GNU BTS URL for a BUG number."
  (format tinydebian--gnu-bts-url-http-bugs
          (if (numberp bug)
              (number-to-string bug)
            bug)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-emacs-bts-bug-url-compose (bug)
  "Compose Emacs BTS URL for a BUG number."
  (format tinydebian--emacs-bts-url-http-bugs
          (if (numberp bug)
              (number-to-string bug)
            bug)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-launchpad-email-control ()
  "Compose control."
  (tinydebian-launchpad-email-compose "control"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnu-bts-email-control ()
  "Compose control."
  (tinydebian-gnu-bts-email-compose "control"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-emacs-bts-email-control ()
  "Compose control."
  (tinydebian-emacs-bts-email-compose "control"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-launchpad-email-new ()
  "Compose new."
  (tinydebian-launchpad-email-compose "new"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-list-email-compose (address)
  "Send message to ADDRESS@<debian mailing list>."
  (format "%s@%s" address tinydebian--list-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-compose-1 (address)
  "Send message to ADDRESS@<debian bts>."
  (format "%s@%s" address tinydebian--debian-bts-email-address))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-launchpad-url-package-bugs (package bug)
  "Return Launchpad URL for PACKAGE and BUG number."
  (format tinydebian--launchpad-url-http-package-bugs
          package
          (if (numberp bug)
              (number-to-string bug)
            bug)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-system-os-debian-p ()
  "Test if running on Debian OS."
  (or (string-match "by Debian" (emacs-version))
      (file-exists-p "/usr/bin/dpkg")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-send-and-exit ()
  "In email buffer, invoke send action."
  (cond
   ((eq major-mode 'mail-mode)
    (mail-send-and-exit))
   ((eq major-mode 'message-mode)
    (message-send-and-exit))
   (t
    (error "TinyDebian: Unknown mail mode. Can't send."))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-bug-number-trim (str)
  "Remove whitespace and #-hash signs.
An example: '   #12345   ' => 12345."
  (if (stringp str)
      (replace-regexp-in-string
       "^[ \t\r\n#]#+"
       ""
       (replace-regexp-in-string
        "[ \t\r\n]+$"
        ""
        str))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-mail-ask-bug-number (&optional type default)
  "Ask bug number for optional Bts TYPE (string) with DEFAULT value.
Return as '(bug) suitable for interactive"
  (tinydebian-bug-nbr-string
   (read-string
    (format "BTS %sbug number: "
            (if type
                (concat type " ")
              ""))
    (or default
        (save-excursion
          (tinydebian-bug-nbr-any))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-mail-ask-package (&optional prompt)
  "Ask package name with optional PROMPT."
  (let ((package (my-debian-bug-package-name-any)))
    ;; FIXME: write smarter selection list
    (completing-read
     (or prompt "Package: ")
     nil
     nil
     nil
     package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-bug-bts-type-p ()
  "Check if buffer is for Emacs BTS."
  (save-excursion
    (goto-char (point-min))
    (when (or (re-search-forward
               "\\(http://bugs.debian[^<> \t\r\n]+\\)" nil t)
              (re-search-forward
               (format "[0-9]+@%s" tinydebian--debian-bts-email-address) nil t)
              (re-search-forward
               (concat
                tinydebian--list-email-address
                "\\|"
                "debian\\.org\\>") nil t))
      (let ((str (or (match-string 1)
                     (match-string 0))))
        (when (and str
                   ;; Remove MIME from
                   ;; http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=3D515275
                   (string-match "\\(http.*=\\)3D\\(.+\\)" str))
          (setq str (concat (match-string 1 str)
                            (match-string 2 str))))
        str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-email-compose (address &optional bug bts-type)
  "Compare type of ADDRESS using BUG and BTS-TYPE information.
The ADDRESS can be:
  control, submit etc.

The BTS-TYPE can be:
  'emacs'   Emacs BTS
  nil       Debian BTS."
   (when (string-match "submitter\\|maintonly\\|quiet\\|close" address)
     (cond
      ((string-match "[0-9]-" address)
       t) ;; Looks like correct address
      (bug
       (setq address (format "%s-%s" bug address)))
      (t
       (error "Can't compose NNNN-ADDRESS from values `%s' `%s'"
              address bug))))
   (cond
    ((string= bts-type "emacs")
     (tinydebian-emacs-bts-email-compose address))
    ((string= bts-type "gnu")
     (tinydebian-gnu-bts-email-compose address))
    ((string= bts-type "coreutils")
     (tinydebian-gnu-bts-email-compose address))
    (t
     (tinydebian-bts-email-compose-1 address))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-url-bug-compose (bts bug &optional project)
  "Compose URL.
Input:

  BTS           String: emacs, launchpad, debian, google
  BUG           String. Nmber.
  PROJECT       String. If needed for the BTS.
                E.g. code.google.com needs project name."
  (cond
   ((string-match "gnome" bts)
    (tinydebian-gnome-bts-url-compose bug))
   ((string-match "kde" bts)
    (tinydebian-kde-bts-url-compose bug))
   ((string-match "perl-cpan" bts)
    ;; (tinydebian-perl-cpan-bts-url-compose bug) FIXME: not implemented
    )
   ((string-match "sourceware" bts)
    (tinydebian-sourceware-bts-url-compose bug))
   ((string-match "google" bts)
    (tinydebian-google-code-bts-url-compose bug project))
   ((string-match "launchpad" bts)
    (tinydebian-launchpad-email-compose bug))
   ((string-match "emacs" bts)
    (tinydebian-emacs-bts-bug-url-compose bug))
   ((string-match "gnu" bts)
    (tinydebian-gnu-bts-bug-url-compose bug))
   ((string-match "debian" bts)
    (tinydebian-debian-bts-url-compose bug))
   (t
    (error "Unknown bts `%s'" bts))))

;; FIXME Move elsewhere
;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-generic-email-compose-1 (type &optional bug buffer)
  "Compose TYPE of address according to BTS using optional BUG number.
Judging from optional BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    ;; FIXME launchpad
    (let ((bts (if (tinydebian-emacs-bug-type-p)
                   "emacs")))
      (tinydebian-bts-email-compose
       type bug bts))))


;; FIXME Move elsewhere
;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-generic-email-compose (type &optional bug buffer)
  "Compose TYPE of address according to BTS using optional BUG number.
Judging from optional BUFFER."
  (let ((bts  (tinydebian-with-bug-context
                (if (tinydebian-emacs-bug-type-p)
                    "emacs"))))
    (tinydebian-bts-generic-email-compose-1 type bug bts)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-submit ()
  "Compose submit."
  (tinydebian-bts-email-compose "submit"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-control ()
  "Compose control."
  (tinydebian-bts-email-compose "control"))

;; FIXME Move elsewhere
;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-generic-email-control (&optional buffer)
  "Compose control according to BTS, judging from optional BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (cond
     ;; FIXME launchpad
     ((tinydebian-gnu-bts-bug-type-p)
      (tinydebian-gnu-bts-email-control))
     ((tinydebian-emacs-bug-type-p)
      (tinydebian-emacs-bts-email-control))
     (t
      (tinydebian-bts-email-control)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-quiet-compose (bug)
  "Compose BUG-quiet, where BUG can be number or string."
  (if (numberp bug)
      (format "%d-quiet" bug)
    (format "%s-quiet" bug)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-quiet-remove (email)
  "Modify ADDRESS NNNN-quie@example.com to simply NNNN@example.com address"
  (when (and (stringp email)
             (string-match "\\([^ \t\r\n]+\\)-quiet\\(@.+\\)" email))
  (format "%s%s" (match-string 1 email) (match-string 2 email))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-email-quiet (bug)
  (tinydebian-bts-email-compose
   (tinydebian-bts-email-quiet-compose bug)))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinydebian-package-narrow-to-region (&rest body)
  "Search dpkg -s result from current point forward and narrow around it.
Point is put at the beginning of region.
Variable `package' contains the package name."
  `(let (beg-narrow
         package)
     (when (re-search-forward "^Package: +\\([^ \t\r\n]+\\) *$" nil t)
       (setq beg-narrow (line-beginning-position))
       (setq package (match-string 1))
       (when (re-search-forward "^[ \t]*$" nil t)
         (ti::narrow-safe beg-narrow (point)
           (ti::pmin)
           ,@body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-buffer-macro 'edebug-form-spec '(body))
(put 'tinydebian-with-buffer-macro 'lisp-indent-function 1)
(defmacro tinydebian-with-buffer-macro (buffer &rest body)
  "Create BUFFER, empty it and run BODY.
Variable `buffer' is available in this macro."
  `(let ((buffer (get-buffer-create ,buffer)))
     (with-current-buffer buffer
       (erase-buffer)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-string-p (str &optional error)
  "Check that STR contains non-empty value.
Signal optional ERROR message is STR was empty."
  (or (and (stringp str)
           (string-match "[^ \t\r\n]" str))
      (and (stringp error)
           (error "TinyDebian: %s" error))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-call-process (prg &optional buffer &rest args)
  "Call PRG with list of ARGS and print output to current buffer or BUFFER."
  (let ((default-directory (getenv "HOME")))
    (apply 'call-process
           prg
           (not 'infile)
           (or buffer (current-buffer))
           (not 'real-time-display)
           args)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-page-font-lock-keywords (page-type)
  "Return `font-lock-keywords' of PAGE-TYPE."
  (tinydebian-with-url-page-type-macro page-type (nth 2 page-type)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-packages-browse-url-compose
  (keyword &optional search-on distribution section)
  "Return URL search string.
Argument: KEYWORD
Optional: SEARCH-ON DISTRIBUTION SECTION."
  (format (concat tinydebian--url-http-package-search
                  "keywords=%s&"
                  "searchon=%s&"
                  "subword=1&"
                  "version=%s&"
                  "release=%s")
          keyword
          (or search-on    "names")
          (or distribution "all")
          (or section      "all")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-string-delete-newlines (string)
  "Delete newlines from STRING."
  (ti::string-regexp-delete "[\r\n]" string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-read-license (message)
  "Ask license with MESSAGE.
  See `tinydebian--wnpp-template-licenses-alist'."
  (completing-read
   message
   (mapcar (lambda (x)
             (cons x 1))
           tinydebian--wnpp-template-licenses-alist)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-font-lock-keywords (&optional uninstall)
  "Add color support to various log files by setting
`font-lock-keywords'."
  (interactive)
  (let ((today  (ti::date-standard-rfc-regexp "mon-date"))
        ;; (cs     (or comment-start-skip "[ \t]+"))
        (file   "")
        keywords)
    (when (stringp buffer-file-name)
      (setq file (or buffer-file-name "no-name?")))
    (setq
     keywords
     (cond
      ;; ............................................. Linux log files ...
      ;; /var/log/
      ((string-match "/log/messages$" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "restarted\\|started"
                       "\\|ignoring"
                       "\\|Linux version.*")
               0 'font-lock-comment-face))))

      ((string-match "mail\\.log\\|mail\\.info" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ ++[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              '("timed out\\|did not.*"
                0 tinydebian--warn-face)
              (list
               (concat "\\(from\\|to\\)=\\([^ ,\t\r\n]+\\)")
               2 'font-lock-comment-face))))

      ((string-match "daemon\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "connection attempt") ;See "iplogger" package
               0 'tinydebian--warn-face)
              (list
               (concat "signal +[0-9]+\\|no such user"
                       "\\|connect from .*")
               0 'font-lock-comment-face))))

      ((string-match "auth\\.log" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "opened +for +[^ \t\r\n]+")
               0 'tinydebian--warn-face)
              '( "for user \\(root\\)"
                 1 font-lock-string-face)
              '( "from \\([^ \t\r\n]+\\)"
                 1 font-lock-type-face)
              '( "for +\\([^ \t\r\n]+\\) +from"
                 1 font-lock-comment-face)
              '( "for user +\\([^ \t\r\n]+\\)"
                 1 font-lock-comment-face))))

      ((string-match "syslog" file)
       ;; font-lock-constant-face
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-keywords
             (list
              (list (concat today " +[0-9]+:+[0-9]+:+[0-9]+")
                    0 'font-lock-function-name-face)
              (list
               (concat
                "^... +[0-9]+ +[0-9]+:+[0-9]+:+[0-9]+")
               0 'font-lock-reference-face)
              (list
               (concat "Invalid.*"
                       ;; portmap[135]: cannot bind udp: Address already in use
                       "\\|cannot"
                       "\\|Connection timed out"
                       ;;  See iplogger(1)
                       "\\|connection attempt"
                       ;;  See portsentry(1)
                       "\\|attackalert:.* +to +.*port.*"
                       ;;  apm -s failed
                       "\\| failed"
                       "\\|did not .*")
               0 'tinydebian--warn-face)
              '("to=\\([^ \t\r\n]+\\)"
                1 font-lock-comment-face)
              '("(\\([^ )\t\r\n]+\\)) CMD "
                1 font-lock-comment-face)
              '("CMD .*"
                0 font-lock-constant-face)
              '("inetd"2
                0 font-lock-type-face)
              (list
               (concat
                "program exit.*\\|.*started.*"
                ;;  btpd daemon
                "\\|synchronisation lost")
               0 font-lock-keyword-face))))))
    (when keywords
      (cond
       (uninstall
        (setq font-lock-keywords nil))
       ((or font-lock-mode
            tinydebian--font-lock-mode
            global-font-lock-mode
            (font-lock-mode-maybe 1))
        (setq font-lock-keywords keywords))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-at-word (&optional string)
  "Read email address if any at current point or from STRING."
  (or string
      (setq string (thing-at-point 'url)))
  (when (and (stringp string)
             (string-match "mailto:\\(.+\\)" string))
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-at-line (&optional string)
  "Read email address if any at current line or from STRING."
  (or string
      (setq string (thing-at-point 'line)))
  (when (and (stringp string)
             (string-match "[^ <\t\r\n]+@[^ \t\r\n>]+" string))
    (match-string 0 string)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-email-gnus-summary-mode-macro 'edebug-form-spec '(body))
(put 'tinydebian-email-gnus-summary-mode-macro 'lisp-indent-function 0)
(defmacro tinydebian-email-gnus-summary-mode-macro (&rest body)
  "At current poiint, examine article and run BODY."
  `(when (eq major-mode 'gnus-summary-mode)
     (let ((article (gnus-summary-article-number))
           article-window)
       (gnus-summary-display-article article)
       (setq article-window (get-buffer-window gnus-article-buffer t))
       (gnus-eval-in-buffer-window
           gnus-article-buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnus-summary-mode-summary-line ()
  "Return Subject at current point in Gnus Summary mode."
  (save-excursion
    (goto-char (line-end-position))
    (skip-chars-backward "^][" (line-beginning-position))
    (buffer-substring (point) (line-end-position))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-gnus-summary-mode ()
  "Read mail address if point is at Gnus summary buffer."
  (tinydebian-email-gnus-summary-mode-macro
   (tinydebian-email-field-from)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-field-from ()
  "Read From: field and return email."
  (let ((str (mail-fetch-field "From")))
    (or (and str
             (tinydebian-email-at-line str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-field-to ()
  "Read To: field and return email."
  (let ((str (mail-fetch-field "To")))
    (or (and str
             (tinydebian-email-at-line str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-any (&rest args)
  "Try various methods to find email address. Ignore ARGS.
At current point, current line, headers of the mail message."
  (or (tinydebian-email-gnus-summary-mode)
      (tinydebian-email-at-word)
      (tinydebian-email-at-line)
      (tinydebian-email-field-from)
      (tinydebian-email-field-to)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-search ()
  "Call hook `tinydebian--find-email-hook' until value returned."
  (run-hook-with-args-until-success 'tinydebian--find-email-hook))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-retrieve-synchronously-macro 'edebug-form-spec '(body))
(put 'tinydebian-retrieve-synchronously-macro 'lisp-indent-function 1)
(defmacro tinydebian-retrieve-synchronously-macro (url &rest body)
  "Retrieve URL and run BODY.
Point is at the beginning. Variable `buffer' is bound.
The URL buffer is killed at exit."
  `(let ((buffer (url-retrieve-synchronously url)))
     (when buffer
       (prog1
           (with-current-buffer buffer
             (goto-char (point-min))
             ,@body)
         (kill-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-sourceforge-bug-name-to-group-id (project)
  "Visit PROJECT web page to determine group ID."
  (let ((url (format "http://sourceforge.net/projects/%s" project)))
    (tinydebian-retrieve-synchronously-macro url
      (if (re-search-forward
           (concat
            (regexp-quote "/project/showfiles.php?group_id=")
            "\\([0-9]+\\)")
           nil t)
          (match-string-no-properties 1)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-sourceforge-bug-type-parse-bug-string (str)
  "Parse STR foo-*-2040281 and return list (PROJECT BUG)."
  (if (string-match
          `,(concat
             "\\<\\([^ \t\r\n]+\\)"
             "-\\(?:Patches\\|Bugs\\|\\(?:Feature\\|Support\\) Requests\\)-"
             "\\([0-9]+\\)")
          str)
      (list (match-string-no-properties 1 str)
            (match-string-no-properties 2 str))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-sourceforge-bug-at-current-point ()
  "Read bug number NNNNNN from current point.
See `tinydebian-sourceforge-bug-type-parse-bug-string'."
  (let ((line (tinydebian-current-line-string)))
    ;; (tinydebian-bug-nbr-sourceforge-tracker line))) ;; FIXME not implemented
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-sourceforge-bug-url-1 (project bug)
  "Return URL for PROJECT name and its BUG number.
This function needs network connection."
  ;; foo-Bugs-2040281
  (let ((group-id (tinydebian-sourceforge-bug-name-to-group-id project))
        url
        atid)
    (when group-id
      (setq url
            (format
             (concat "https://sourceforge.net/tracker/?&aid=%s"
                     "&group_id=%s")
             bug group-id))
      ;;  href="/tracker/?group_id=95606&amp;atid=611982">Bugs</a></li>
      (tinydebian-retrieve-synchronously-macro url
        (if (re-search-forward
             (concat
              (regexp-quote
               "/tracker/?group_id=95606&amp;atid=")
              "\\([0-9]+\\)"
              ".*> *Bugs *</a>")
             nil t)
            (setq atid (match-string-no-properties 1))))
      (when atid
        (setq url (format "%s&atid=%s&func=detail" url atid)))
      url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-sourceforge-bug-url-main (str)
  "Return URL for STR foo-Bugs-2040281"
  (multiple-value-bind (project bug)
      (tinydebian-sourceforge-bug-type-parse-bug-string str)
    (when bug
      (tinydebian-sourceforge-bug-url-1 project bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-string-p (str)
  "Test if STR looks like Emacs BTS support request."
  ;; [  23: Emacs bug Tracking Syst] bug#2134:
  (if (string-match tinydebian--emacs-bts-line-regexp str)
      (match-string-no-properties 1 str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-re-search-email ()
  "Search from current point for `tinydebian--emacs-bts-email-address'.
Return:
  '(bug-number email)"
  (let ((email (regexp-quote tinydebian--emacs-bts-email-address))
	bug)
    (cond
     ((re-search-forward (format
                          "\\(\\([0-9]+\\)@%s\\>\\)"
                          email)
                          nil t)
      ;; Your message has been sent to the package maintainer(s):
      ;;  Emacs Bugs <email>
      ;;
      ;; If you wish to submit further information on this problem, please
      ;; send it to 2217@<address>, as before.
      ;;
      ;; Please do not send mail to owner@<address> unless you wish
      ;; to report a problem with the Bug-tracking system.
      (list (match-string-no-properties 2)
            (match-string-no-properties 1)))
     ((and (re-search-forward (format
			       "control@%s"
			       email)
			      nil t)
	   ;; To: control@debbugs.gnu.org
	   ;; Subject: Bug#7665 retitle
	   (setq bug (tinydebian-bug-nbr-any)))
      (list bug
	    (tinydebian-emacs-bts-email-compose bug)))
     ((save-excursion
        (re-search-forward (concat
                            email
                            "\\|Emacs bugs database")
                           nil t))
      ;; Processing commands for control@<address>:
      ;;
      ;; > severity NNNN minor
      ;; bug#NNN: <<subject>
      ;; Severity set to `minor' from `normal'
      ;;
      ;; > thanks
      ;; Stopping processing here.
      ;;
      ;; Please contact me if you need assistance.
      ;;
      ;; Foo Bar
      ;; (administrator, Emacs bugs database)
      (let ((bug (tinydebian-bug-nbr-forward)))
        (list bug))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-re-search-p (&optional point)
  "Search from `point-min' or optional POINT for Emacs BTS response.
Return:
  '(bug-number email)."
  (save-excursion
    (goto-char (or point (point-min)))
    (tinydebian-bug-gnu-emacs-bts-re-search-email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-buffer-p ()
  "Check if bug context is Emacs BTS."
  (multiple-value-bind (bug email)
      (tinydebian-bug-gnu-emacs-bts-re-search-p)
    (if email
        bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-gnus-summary-line-p ()
  (let* ((str (tinydebian-current-line-string))
         (bug (tinydebian-bug-gnu-emacs-bts-string-p str)))
    (cond
     (bug
      (tinydebian-emacs-bts-bug-url-compose bug))
     (t
      ;; [  46: -> submit@emacsbugs.don] [PATCH] ...
      (string-match "@emacsbug" str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-emacs-bts-gnus-article-p ()
  "Check if bug context is Emacs BTS in current article buffer."
  (tinydebian-with-gnus-article-buffer nil
    (let ((bug (tinydebian-bug-gnu-emacs-bts-buffer-p)))
      (when bug
        (tinydebian-emacs-bts-bug-url-compose bug)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-gnu-emacs-bts-gnus-summary-p ()
  "Check if bug context is Emacs BTS at Gnus summary line."
  (or (tinydebian-bug-gnu-emacs-bts-gnus-summary-line-p)
      (tinydebian-bug-gnu-emacs-bts-gnus-article-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-emacs-bug-type-p ()
  "Check if bug context is Emacs BTS."
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (tinydebian-bug-gnu-emacs-bts-gnus-summary-p))
   ((memq major-mode '(message-mode
                       mail-mode
                       gnus-original-article-mode
                       gnus-article-mode))
    (tinydebian-bug-gnu-emacs-bts-buffer-p))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-gnu-savannah-site-support-string-p (str)
  "Test if STR looks like Savannah site support request."
  (if (string-match "\\[sr +#\\([0-9]+\\)\\]" str)
      (match-string-no-properties 1 str)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-generic-string-p (str)
  "Test if STR looks like string '[bug #25611]'"
  (if (string-match "\\[bug +#\\([0-9]+\\)\\]" str)
      (match-string-no-properties 1 str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-savannah-buffer-url-p ()
  "Check if bug context is savannah.gnu.org in buffer starting at `point-min'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         "https?://savannah.\\(?:non\\)?gnu.org/[^<> \t\r\n]+[?][0-9]+" nil t)
        (match-string-no-properties 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-savannah-bug-type-bts-p ()
  "Check if bug context is Savannah site request."
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (let ((str (tinydebian-current-line-string))
          bug)
      ;; [  21: Foo Bar ] [sr #106037]
      (cond
       ((setq bug (tinydebian-bug-gnu-savannah-site-support-string-p str))
        (format "%s/?%s"
                tinydebian--gnu-savannah-url-http-site-support-bugs
                bug)))))
   (t
    (tinydebian-bug-gnu-savannah-buffer-url-p))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-sourceforge-bug-string-p (str)
  "Test if STR looks like Sourceforge bug."
  (if (string-match
       (concat
        ;; "Sourceforge.*"
        "\\([a-zA-Z]+-Bugs-"
        "[0-9][0-9][0-9]+\\)")
       str)
      (match-string-no-properties 1 str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-sourceforge-bug-type-p ()
  "Check if bug context is Sourceforge."
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (let ((str (tinydebian-current-line-string)))
      (multiple-value-bind (project bug)
          (tinydebian-sourceforge-bug-type-parse-bug-string str)
        (tinydebian-sourceforge-bug-url-1 project bug))))
   (t
    (goto-char (point-min))
    (if (re-search-forward
         "https?://sourceforge.net/tracker/[^<> \t\r\n]+" nil t)
        (match-string-no-properties 0)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-gnome-bug-type-p ()
  "Check if bug context is Gnome."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "bugzilla.gnome.org" str)
      str)
     (t
      (goto-char (point-min))
      (if (re-search-forward "https?://bugzilla.gnome.org[^<> \t\r\n]+" nil t)
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-kde-bug-type-p ()
  "Check if bug context is KDE."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "bugs.kde.org" str)
      str)
     (t
      (goto-char (point-min))
      (if (or (re-search-forward
               "https?://bugs.kde.org/[^<> \t\r\n]+[0-9]+" nil t)
              (re-search-forward "https?://bugs.kde.org" nil t))
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-freshmeat-bug-type-p ()
  "Check if bug context is Freshmeat."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "freshmeat.net" str)
      str)
     (t
      (goto-char (point-min))
      (if (re-search-forward
           ;;  http://freshmeat.net/projects/wcd#release_307159
           "https?://freshmeat.net/[^<> \t\r\n]+[0-9][0-9][0-9]+" nil t)
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-perl-cpan-bug-type-p ()
  "Check if bug context is RT.CPAN.ORG."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "rt.cpan.org" str)
      str)
     (t
      (goto-char (point-min))
      (if (or (re-search-forward
               "https?://rt.cpan.org/[^<> \t\r\n]+[0-9]" nil t)
              (re-search-forward "https?://rt.cpan.org" nil t))
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mysql-bug-type-p ()
  "Check if bug context is MySQL."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "forge.mysql.com" str)
      str)
     (t
      (goto-char (point-min))
      (if (re-search-forward
	   "https?://\\(?:forge\\|bugs\\).mysql.com[^<> \t\r\n]+" nil t)
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-sourceware-bug-type-p ()
  "Check if bug context is sourceware (GNU; under Redhat bugzilla tracker)."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ((string-match "sourceware.*bugzilla[^<> \t\r\n]+\\([0-9]+\\)" str)
      (match-string-no-properties 1))
     (t
      (goto-char (point-min))
      (if (re-search-forward "https?://.*sourceware.org/[^<> \t\r\n]+" nil t)
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-trac-bug-type-p ()
  "Check if bug context is Trac."
  ;;  Can't detect, too generic: [PROJECT] #3124: ...
  (let ((str (tinydebian-current-line-string)))
    (goto-char (point-min))
    (if (re-search-forward
         "X-Trac-Ticket-URL: *\\(http[^<> \t\r\n]+\\)" nil t)
        (match-string-no-properties 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-github-bug-type-p ()
  "Check if bug context is Github issue tracker."
  ;;  Can't detect, too generic: [PROJECT] #3124: ...
  (let ((str (tinydebian-current-line-string)))
    (goto-char (point-min))
    (if (re-search-forward
	 ;;  View Issue:  <URL>
         "*\\(https?://.*github.com/[^<> \t\r\n]+\\)" nil t)
        (match-string-no-properties 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mercurial-bug-type-p ()
  "Check if bug context is Mercurial."
  (let ((str (tinydebian-current-line-string)))
    (cond
     ;; <http://mercurial.selenic.com/bts/issue1803>
     ((string-match "http.*mercurial.*/bts/[^<> \t\r\n]+\\([0-9]+\\)" str)
      (match-string-no-properties 1))
     (t
      (goto-char (point-min))
      (if (re-search-forward "https?://.*mercurial.*/bts/[^<> \t\r\n]+" nil t)
          (match-string-no-properties 0))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-launchpad-bug-type-p ()
  "Check if bug context is Launchpad.
Return URL."
  (let ((str (tinydebian-current-line-string))
        bug
        package)
    (cond
     ((string-match
       "http.*launchpad.net/\\([^ /\t\r\n]+\\)/.bugs/\\([0-9]+\\)" str)
      (setq package (match-string-no-properties 1)
            bug     (match-string-no-properties 2)))
     (t
      (goto-char (point-min))
      ;; Reply-To: Bug 421667 <421667@bugs.launchpad.net>
      ;; * Changed in: package
      ;; Importance: Undecided => Low
      (when (re-search-forward "\\([0-9]+\\)@bugs.launchpad.net" nil t)
        (setq bug (match-string-no-properties 1))
        (goto-char (point-min))
        (cond
         ((or (re-search-forward
               "^X-Launchpad-Bug: product=\\([^;  \t\r\n]+\\)" nil t)
              (re-search-forward "[*].*Changed in: +\\([^ \t\r\n]+\\)" nil t))
          (setq package (match-string-no-properties 1)))
         (t
          (error "Cannot parse package name in a Launchpad bug context"))))))
    (when (and bug package)
      (tinydebian-launchpad-url-package-bugs package bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnus-article-buffer-p ()
  "Return bug URL from gnus article buffer if any.
This function can only be run from `gnus-summary-mode'."
  (when (and (featurep 'gnus)
             (boundp 'gnus-article-buffer)
             (eq major-mode 'gnus-summary-mode))
    (tinydebian-with-gnus-article-buffer nil
      (tinydebian-bug-gnu-savannah-buffer-url-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-url-current-buffer (bug)
  "Return correct bug URL for BUG.

NOTE: only works in standard Gnus Summary or Article buffer. This
function looks around surrounding Email text to determine what is
the proper bug destionation: Sourceforge, Ubuntu etc.
The last choice os Debian."
  (let (ret
        project
        group-id
        str)
    (cond
     ((setq str (tinydebian-emacs-bug-type-p))
      (cond
       ((string-match "http" str)
        str)
       ((string-match "^[0-9]" str)
        (tinydebian-emacs-bts-bug-url-compose str))
       (t
        (error "Unknown Emacs tracker `%s'" str))))
     ((setq str (tinydebian-savannah-bug-type-bts-p))
      (if (string-match "http" str)
          str
        (error "Unknown Savannah tracker `%s'" str)))
     ((setq str (tinydebian-sourceforge-bug-type-p))
      (if (string-match "http" str)
          str
        (tinydebian-sourceforge-bug-url-main str)))
     ((tinydebian-launchpad-bug-type-p)
      ;; FIXME: Where do we get PACKAGE?
      ;; (tinydebian-launchpad-url-package-bugs package bug)
      )
     ;;  There is nothing that can be detect from Summary line,
     ;;  we must look at article buffer.
     ;;  [  49: Foo Bar ] [bug #25611] ....
     ((tinydebian-bug-gnus-article-buffer-p))
     (bug
      (tinydebian-debian-bts-url-compose bug)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-bts-type-determine ()
  "Check what BTS type is in use.
Return: '(BTS-TYPE-STRING [BUG NUMBER | URL])."
  (tinydebian-with-bug-context
    (let (data)
      (cond
       ((setq data (tinydebian-freshmeat-bug-type-p))
        (list "freshmeat" data))
       ((setq data (tinydebian-perl-cpan-bug-type-p))
        (list "perl-cpan" data))
       ((setq data (tinydebian-savannah-bug-type-bts-p))
        (list "savannah" data))
       ((setq data (tinydebian-bug-gnu-emacs-bts-buffer-p))
        (list "emacs" data))
       ((setq data (tinydebian-bug-gnu-bts-buffer-p))
        (list "gnu" data))
       ((setq data (tinydebian-sourceforge-bug-type-p))
        (list "sourceforge" data))
       ((setq data (tinydebian-gnome-bug-type-p))
        (list "gnome" data))
       ((setq data (tinydebian-kde-bug-type-p))
        (list "kde" data))
       ((setq data (tinydebian-sourceware-bug-type-p))
        (list "sourceware" data))
       ((setq data (tinydebian-mysql-bug-type-p))
        (list "mysql" data))
       ((setq data (tinydebian-mercurial-bug-type-p))
        (list "mercurial" data))
       ((setq data (tinydebian-trac-bug-type-p))
        (list "trac" data))
       ((setq data (tinydebian-github-bug-type-p))
        (list "github" data))
       ((setq data (tinydebian-launchpad-bug-type-p))
        (list "launchpad" data))
       ((setq data (tinydebian-debian-bug-bts-type-p))
        (list "debian" data))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-url (bug)
  "Return correct bug URL for BUG.
Under Gnus, peek current article to determine bug context."
  (tinydebian-with-bug-context
   (tinydebian-bug-url-current-buffer bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-string-parse-wnpp-alert (str)
  "Parse wnpp-alert(1) like line. Return '(bug package bug-type desc)
RFA 321654 debtags -- Enables support for package tags."
  (let (case-fold-search)
    (when (string-match
           (concat
            "\\<\\(RF.\\|IT.\\|O\\) +\\([0-9]+\\) +"
            "\\([^ \t\r\n]+\\) +-- +\\(.+[^ \t\r\n]\\)")
           str)
      (list
       (match-string 2 str)
       (match-string 3 str)
       (match-string 1 str)
       (match-string 4 str)))))

;;; ----------------------------------------------------------------------
;;; (tinydebian-bug-string-parse-bts-wnpp-subject "Bug#1111: marked as done (ITA: foo -- desc")
;;; (tinydebian-bug-string-parse-bts-wnpp-subject "Bug#1111: ITA: foo -- desc")
;;; (tinydebian-bug-string-parse-bts-wnpp-subject "Processed: retitle 465897 to ITA: foo -- desc")
(defun tinydebian-bug-string-parse-bts-wnpp-subject (str)
 "Parse wnpp email Subject orphan line.  Return '(bug package bug-type desc).
Bug#NNNN: O: package -- description."
  (let ((case-fold-search t))
    (cond
     ((string-match
       (concat
        "bug#"
        "\\([0-9]+\\):[ \t]+"
        "\\(?:marked as done (\\)?"
        "\\([A-Z]+\\):[ \t]+"
        "\\([^ \t\r\n]*\\)[ \t]+-+[ \t]+"
        "\\(.+\\)")
       str)
      (list
       (match-string 1 str)
       (match-string 3 str)
       (match-string 2 str)
       (match-string 4 str)))
     ((string-match
       (concat
        "Processed:[ \t]+[a-z]+[ \t]+"
        "\\([0-9]+\\)"
        "[ \t]+to[ \t]+"
        "\\([A-Z]+\\):[ \t]+"
        "\\([^ \t\r\n]*\\)[ \t]+-+[ \t]+"
        "\\(.+\\)")
       str)
      (list
       (match-string 1 str)
       (match-string 3 str)
       (match-string 2 str)
       (match-string 4 str))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-string (str)
  "Read bug nbr from STR."
  (when (stringp str)
    (or (and (string-match "#\\([0-9]+\\)" str) ;; Bug#NNNN Debian
             (match-string 1 str))
        ;; [Bug 192841] Ubuntu
        (and (string-match "[[]Bug \\([0-9]+\\)[]]" str)
             (match-string 1 str))
        ;; [foo-Bugs-192841] Sourceforge
        (and (string-match "[[] *[a-zA-Z]+-Bugs-\\([0-9]+\\) *[]]" str)
             (match-string 1 str))
        (multiple-value-bind (bug)
            (tinydebian-bug-string-parse-wnpp-alert str)
          bug)
        ;;   NNNN@bugs.debian.org
        (and (string-match (concat "\\([0-9]+\\)\\(?:-[a-z]+\\)?@"
                                   tinydebian--debian-bts-email-address)
                           str)
             (match-string 1 str))
        ;;   BTS message lines: "owner NNNNNN"
        (and (string-match (concat "\\([0-9]+\\)\\(?:-[a-z]+\\)?@"
                                   ;; FIXME: Use variable
                                   "bugs.launchpad.net")
                           str)
             (match-string 1 str))
        ;;   BTS message lines: "owner NNNNNN"
        (and (string-match (concat "\\<\\(?:owner\\|retitle\\) "
                                   "\\([0-9][0-9][0-9][0-9][0-9][0-9]\\)\\>")
                           str)
             (match-string 1 str))
        (and (string-match          ;; Plain NUMBER
              "^[ \t\r\n]*\\([0-9]+\\)" str)
             (match-string 1 str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-at-current-point-hash ()
  "Read bug number with hash mark from current point (#NNNN)."
  (let ((table (syntax-table))
        word)
    (with-syntax-table table
      (modify-syntax-entry ?# "w" table)
      (let ((word (current-word)))
        (if (and word
                 (string-match "#" word))
            (tinydebian-bug-nbr-string word))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-at-current-point-number ()
  "Read at least 4 digit bug number from current point.
The number must be surreounded by whitespace."
  (when (looking-at "[0-9]+[ \t\r\n]")
    (save-excursion
      ;; Go to beginning
      (skip-chars-backward "^ \t\r\n")
      ;; At least 3 digits
      (when (looking-at "\\([0-9][0-9][0-9]+\\)[ \t\r\n]")
        (current-word)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-debian-control-commands ()
  "Read BTS number from DEbian control commands at the beginning."
  (let (nbr
        end
        (re
         (concat
          "^"
          (regexp-opt
           ;; http://www.debian.org/Bugs/server-control
           '("quit"
             "stop"
             "thank"
             "thanks"
             "thankyou"
             "thank you"
             "--")))))
    (goto-char (point-min))
    (when (re-search-forward mail-header-separator nil t)
      (forward-line 1)
      (when (and (save-excursion
                   (if (re-search-forward re nil t)
                       (setq end (line-beginning-position))))
                 (re-search-forward
                  ;; <command> <bug number> <rest actions>
                  "^[ \t]*[^ \r\n\t]+[ \t]+\\([0-9]+\\)\\>" end t))
        (match-string-no-properties 1)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-nbr-search ()
  "Call hook `tinydebian--find-bug-nbr-hook' until value returned."
  (run-hook-with-args-until-success 'tinydebian--find-bug-nbr-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-url-forward ()
  "Find url that looks like BTS from current point forward.
In Gnus summary buffer, look inside original article."
  (let ((buffer (current-buffer)))
    (when (memq major-mode '(gnus-summary-mode))
      (tinydebian-with-gnus-article-buffer nil
        (goto-char (point-min))
        (setq buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((point (point))
            nbr)
        (cond
         ((string-match "http://" (tinydebian-current-line-string))
          (thing-at-point 'url))
         ((re-search-forward
           `,(concat "https?://[^ \t\r\n]*"
                     "\\(bugzilla[^ \t\r\n]+[0-9]"
                     "\\|launchpad\\.[^ \t\r\n]+[0-9]"
                     "\\|\\(?:savannah\\|sv\\)\\.[^ \t\r\n]+[0-9]"
                     "\\|issue[^ \t\r\n]+[0-9]"
                     "\\|gnu\\.org[^ \t\r\n]+[0-9]"
                     "\\|forge[^ \t\r\n]+[0-9]"
                     "\\|rt\\.cpan\+.org[^ \t\r\n]+[0-9]"
                     "\\|freshmeat\\.net[^ \t\r\n]+[0-9][0-9][0-9]"
                     "\\)")
           nil t)
          (let ((str (thing-at-point 'url)))
            ;; Clean MIME, bug=3D<number>
            (replace-regexp-in-string "bug=3D" "bug=" str)))
         ((and (setq nbr (tinydebian-bug-nbr-search))
               (progn
                 (goto-char (point-min))
                 (re-search-forward "^From:.*@debian.org" nil  t)))
          (tinydebian-bug-url-current-buffer nbr)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-article-buffer ()
  "Return bug number from article buffer."
  (tinydebian-with-gnus-article-buffer nil
    (let ((str (mail-fetch-field "Subject")))
      (tinydebian-bug-nbr-any-in-string str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-nbr-any (&rest args)
  "Try various methods to find bug tracking number. Ignore ARGS.
At current point, current line, headers of the mail message
\(CC, To, Subject), forward from point, whole buffer."
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (or (tinydebian-bug-nbr-at-current-point)
        (tinydebian-bug-nbr-current-line)
        ;; !A [  40: Foo Bar ] <subject content>
        (tinydebian-bug-nbr-any-in-string
         (tinydebian-gnus-summary-mode-summary-line))
	(tinydebian-bug-nbr-article-buffer)))
   (t
    (or (tinydebian-bug-nbr-at-current-point)
        (tinydebian-bug-nbr-current-line)
        (tinydebian-email-cc-to-bug-nbr)
        (tinydebian-email-subject-bug-nbr)
        (tinydebian-bug-nbr-debian-url-forward)
        (tinydebian-email-debian-control-commands)
        (tinydebian-bug-nbr-forward)
        (tinydebian-bug-nbr-buffer)
        (tinydebian-bug-hash-forward)
        (tinydebian-bug-hash-buffer)
        (tinydebian-bug-nbr-any-at-current-point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-header-pool ()
  "Search Filename: pool/main/p/<package>."
  (tinydebian-buffer-match-string
   "^Filename: pool.*/\\([^/ \t\r\n]+\\)/"
   (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-header-package ()
  "Search Package: <package>."
  (tinydebian-buffer-match-string
   "^Package: +\\([^/ \t\r\n]+\\)/"
   (point-min)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-parse-string-with-bug (str)
  "Return '(bug type package description) for common matches."
  (let (bug
        type
        package
        desc
        case-fold-search)
    (cond
     ((string-match "\\<\\([A-Z][A-Z][A-Z]\\|O\\): *\\(.*\\)" str)
      (setq type (match-string 1 str)
            desc (match-string 2 str)
            bug  (tinydebian-bug-nbr-string str))
      (when (string-match "^\\([a-z].+\\) +--+ *\\(.*\\)" desc)
        (setq package (match-string 1 desc)
              desc    (match-string 2 desc))))
     ((string-match "Bug#\\([0-9]+\\): *\\(.*\\)" str)
      (setq bug  (match-string 1 str)
            desc (match-string 2 str))))
    (list bug type package desc)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-parse-string-with-package (str)
  "Return '(package description) for common matches."
  (let (case-fold-search)
    (cond
     ((string-match
       "RM: +\\([^ /\t\r\n]+\\).*-- *\\(?:[^ \t\r\n]+; +\\)?\\(.*\\)" str)
      (list (match-string 1 str)
            (match-string 2 str)))
     ((string-match
       "[fF]ixed in\\(?: NMU of\\)? \\([a-z][^ \t\r\n]+\\) +\\(.*\\)" str)
      (list (match-string 1 str)
            str))
     ((string-match "^\\([a-z][a-z0-9-]+\\): +\\(.*\\)" str)
      (list (match-string 1 str)
            (match-string 2 str))))))

;;; ----------------------------------------------------------------------
;;;  Test cases. Do not delete
;;;
;;; (tinydebian-bts-parse-string-1 "Bug#353353: RFP: appweb -- very ...")
;;; (tinydebian-bts-parse-string-1 "Bug#352429: marked as done (ITA: cdrdao  -- records CDs in Disk-At-Once (DAO) mode)")
;;; (tinydebian-bts-parse-string-1 "Bug#351502: fixed in nvu 1.0final-1")
;;; (tinydebian-bts-parse-string-1 "Bug#352533: Fixed in NMU of sa-exim 4.2-3")
;;; (tinydebian-bts-parse-string-1 "Bug#244582: UFO:AI is back")
;;; (tinydebian-bts-parse-string-1 "Re: RM: gcrontab/unstable -- ROM; not ported to GTK2, no upstream")
;;; (tinydebian-bts-parse-string-1 "Re: Bug#575638: glitz: not maintained and probably should be removed")
;;  (tinydebian-bts-parse-string-1 "Bug#582879: Acknowledgement (RFP: lsx -- list executables in a directory tree)")
;;; (tinydebian-bts-parse-string-1 "")
(defun tinydebian-bts-parse-string-1 (str)
  "Parse STR and return '(bug type package description)."
  (when (stringp str)
    ;;  Treat long "folded" subject like:
    ;;
    ;;  Subject: Bug#353588 acknowledged by developer (Re: Bug#353588: lintian:
    ;;     [add new rule] check debian/control::Description better ...
    ;;
    (setq str
          (replace-regexp-in-string "[\r\n]+" " " str))
    (multiple-value-bind (bug type package desc)
        (tinydebian-bts-parse-string-with-bug str)
      (when (and (not package)
                 desc)
        (multiple-value-bind (ret-pkg ret-desc)
            (tinydebian-bts-parse-string-with-package desc)
          (setq package ret-pkg
                desc    ret-desc)))
      (if (and (stringp desc)
               (string= desc ""))
          (setq desc nil))
      (if desc
          ;; Remove trailing ")" that is in return messages
          ;; Bug#NNNNN: Acknowledgement (RFP: ...)
          (setq desc
                (replace-regexp-in-string  ")$" "" desc)))
      (if (and bug desc)
          (list bug type package desc)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-parse-string-current-line ()
  (let ((str (tinydebian-current-line-string)))
    (tinydebian-bts-parse-string-1 str)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-parse-string-subject ()
  (let ((str (mail-fetch-field "Subject")))
    (when str
      (tinydebian-bts-parse-string-1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-bts-re-search-email ()
  "Search from current point for `tinydebian--emacs-bts-email-address'.
Return:
  '(bug-number email)"
  (let ((email (regexp-quote tinydebian--gnu-bts-email-address)))
    (cond
     ((re-search-forward (format
                          "\\(\\([0-9]+\\)@%s\\>\\)"
                          email)
                          nil t)
      ;; Thank you for filing a new bug report with GNU.
      ;;
      ;; This is an automatically generated reply to let you know your message
      ;; has been received.
      ;;
      ;; Your message is being forwarded to the package maintainers and other
      ;; interested parties for their attention; they will reply in due course.
      ;;
      ;; Your message has been sent to the package maintainer(s):
      ;;  bug-coreutils@gnu.org
      ;;
      ;; If you wish to submit further information on this problem, please
      ;; send it to NNNN@debbugs.gnu.org.
      ;;
      ;; Please do not send mail to help-debbugs@gnu.org unless you wish
      ;; to report a problem with the Bug-tracking system.
      ;;       (list (match-string 2)
      (list (match-string-no-properties 2)
            (match-string-no-properties 1)))
     ((and nil                          ;FIXME, not implemented (copy of emacs)
           (save-excursion
             (re-search-forward (concat
                                 email
                                 "\\|Emacs bugs database")
                                nil t)))
      ;; Processing commands for control@<address>:
      ;;
      ;; > severity NNNN minor
      ;; bug#NNN: <<subject>
      ;; Severity set to `minor' from `normal'
      ;;
      ;; > thanks
      ;; Stopping processing here.
      ;;
      ;; Please contact me if you need assistance.
      ;;
      ;; Foo Bar
      ;; (administrator, Emacs bugs database)
      (let ((bug (tinydebian-bug-nbr-forward)))
        (list bug))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-bts-re-search-p (&optional point)
  "Search from `point-min' or optional POINT for Emacs BTS response.
Return:
  '(bug-number email)."
  (save-excursion
    (goto-char (or point (point-min)))
    (tinydebian-bug-gnu-bts-re-search-email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-bts-buffer-p ()
  "Check if bug context is Emacs BTS."
  (multiple-value-bind (bug email)
      (tinydebian-bug-gnu-bts-re-search-p)
    (if email
        bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-bts-gnus-summary-line-p ()
  (let* ((str (tinydebian-current-line-string))
         (bug (tinydebian-bug-gnu-emacs-bts-string-p str)))
    (cond
     (bug
      (tinydebian-gnu-bts-bug-url-compose bug))
     (t
      ;;  [  21: GNU bug Tracking System] bug#1234: Acknowledgement (coreutils:
      (string-match "GNU bug Tracking\\|.*debbugs\\.gnu" str)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-gnu-bts-gnus-article-p ()
  "Check if bug context is GNU BTS in current article buffer."
  (tinydebian-with-gnus-article-buffer nil
    (let ((bug (tinydebian-bug-gnu-bts-buffer-p)))
      (when bug
        (tinydebian-gnu-bts-bug-url-compose bug)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-gnu-bts-gnus-summary-p ()
  "Check if bug context is GNU BTS at Gnus summary line."
  (or (tinydebian-bug-gnu-bts-gnus-summary-line-p)
      (tinydebian-bug-gnu-bts-gnus-article-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-gnu-bts-bug-type-p ()
  "Check if bug context is GNU BTS."
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (tinydebian-bug-gnu-bts-gnus-summary-p))
   ((memq major-mode '(message-mode
                       mail-mode
                       gnus-original-article-mode
                       gnus-article-mode))
    (tinydebian-bug-gnu-bts-buffer-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-current-line-wnpp-alert ()
  "Parse wnpp-alert(1) line."
  (let ((line (tinydebian-current-line-string)))
    (when line
      (multiple-value-bind (bug package)
          (tinydebian-bug-string-parse-wnpp-alert line)
        package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-package-name-current-line-wnpp-subject ()
  "PArse BTS Suject line."
  (let ((line (tinydebian-current-line-string)))
    (when line
      (multiple-value-bind (bug package)
          (tinydebian-bug-string-parse-bts-wnpp-subject line)
        package))))

;;; ----------------------------------------------------------------------
;;;
(defun my-debian-bug-package-name-any ()
  "Search package name."
  (or (tinydebian-bug-package-name-current-line-wnpp-alert)
      (tinydebian-bug-package-name-current-line-wnpp-subject)
      (tinydebian-bug-package-name-header-pool)
      (tinydebian-bug-package-name-header-package)
      (progn
        (multiple-value-bind (bug type-orig package description)
            (or (tinydebian-bts-parse-string-current-line)
                (tinydebian-bts-parse-string-subject))
          package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-gnus-summary-subject ()
  "In Gnus *Summary* buffer return current subject."
  (tinydebian-email-gnus-summary-mode-macro
   (mail-fetch-field "Subject")))

;;; ----------------------------------------------------------------------
;;;
(defun my-tinydebian-subject-any ()
  "Try to find subject for mail message."
  (or (tinydebian-gnus-summary-subject)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-email-subject-type-parse ()
  "Read BTS Subject and return '(TYPE SUBJECT)"
  (let ((subject (mail-fetch-field "Subject")))
    (when subject
      ;;  Bug#292579: marked as done (RFP: miwm -- MIcroscopic Window
      (let (type subject bug)
        (when (string-match "\\(?: (?\\)\\([a-z]+\\):\\(.*\\)" subject)
          (setq type    (match-string 1 subject)
                subject (match-string 2 subject)))
        (setq bug
              (tinydebian-bug-nbr-string subject))
        (list type subject bug)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-decode-hex (string)
  "Decode %NN hex codes in STRING."
  (with-temp-buffer
    (save-match-data
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "%\\([0-9A-F][0-9A-F]\\)" nil t)
        (replace-match
         (save-match-data
           (format "%c"
                   (radix
                    (match-string-no-properties 1)
                    16)))))
      (buffer-string))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-decode-html (string)
  "Simple HTML entity decoder."
  (tinydebian-decode-hex
   (mm-url-decode-entities-string string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-month-to-number (month &optional zero-padded)
  "Convert MONTH, 3 character initcap month name e.g. `Jan' to number."
  (cdr-safe
   (assoc
    month
    '( ("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
       ("Apr" . 4) ("May" . 5) ("Jun" . 6)
       ("Jul" . 7) ("Aug" . 8) ("Sep" . 9)
       ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))))

;;; ----------------------------------------------------------------------
;;; (tinydebian-date-to-iso8691 "Tue, 29 Sep 2009 22:01:11 UTC")
(defun tinydebian-date-to-iso8691 (string)
  "Convert RFC 'Tue, 29 Sep 2009 22:01:11 UTC' STRING to ISO 8601."
  (when (stringp string)
    (if (string-match
         `,(concat "\\([a-z][a-z][a-z]\\), *"
                   "\\([0-9]+\\) +"
                   "\\([a-z][a-z][a-z]\\) +"
                   "\\([0-9][0-9][0-9][0-9]\\) +"
                   "\\([0-9][0-9]:[0-9][0-9:]+\\) +"
                   "\\(.*\\)")
         string)
        (let ((day   (match-string-no-properties 1 string))
              (dd    (string-to-number (match-string-no-properties 2 string)))
              (month (match-string-no-properties 3 string))
              (yyyy  (match-string-no-properties 4 string))
              (time  (match-string-no-properties 5 string))
              (rest  (match-string-no-properties 6 string)))
          (format "%s-%02d-%2d %s %s %s"
                  yyyy
                  (tinydebian-month-to-number month)
                  dd
                  time
                  rest
                  day)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-parse-bts-search (field &optional delimiter)
  "Search <DELIMITER>FIELD: (.*)</DELIMITER> and return match 1."
  (or delimiter
      (setq delimiter "p"))
  (if (re-search-forward
       ;; <p><strong>Done:</strong> ...</p>
       ;; <p>Severity: <em class="severity">serious</em></p>
       (format
        `,(concat
           "<%s>\\(?:<[a-z]+>\\)?"
           "%s:\\(?:</[a-z]+>\\)? *\\(?:<[^>]+> *\\)?"
           "\\([^<]+\\)"
           "\\(?:</[a-z]+>\\)?</%s>")
        delimiter
        field
        delimiter)
       nil t)
      (tinydebian-decode-html
       (match-string-no-properties 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-parse-bts-search-tag (re &optional tag)
  "Search <TAG>(.*)</TAG> and return match 1. TAG defaults to 'p'."
  (or tag
      (setq tag "p"))
  (if (re-search-forward
       (format "<%s> *\\(?:%s\\)\\([^<]+\\)</%s>"
               tag
               re
               tag)
       nil t)
      (tinydebian-decode-html
       (match-string-no-properties 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-date ()
  "Parse buffer content of Debian BTS (HTTP result)."
  (tinydebian-debian-parse-bts-search "Date"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-date-iso ()
  "Parse buffer content of Debian BTS (HTTP result)."
  (tinydebian-date-to-iso8691
   (tinydebian-debian-parse-bts-bug-date)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-title ()
  "Parse buffer content of Debian BTS (HTTP result)."
  (if (re-search-forward "<title>\\(.+\\)</title>" nil t)
      (replace-regexp-in-string
       " *- *Debian Bug report logs *"
       ""
       (tinydebian-decode-html
        (match-string-no-properties 1)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-maintainer ()
  "Parse buffer content of Debian BTS (HTTP result)."
  (if (re-search-forward "Maintainer for .*maint=[^>]+> *\\([^<]+\\)" nil t)
      (tinydebian-decode-html (match-string-no-properties 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-package ()
  "Parse buffer content of Debian BTS (HTTP result)."
       ;; ;package=levee"></a></div>
  (if (re-search-forward "package=\\([^<\"\t\r\n]+\\)\"" nil t)
      (let ((pkg (tinydebian-decode-html (match-string-no-properties 1))))
        ;; src:<package>
        (if (string-match ":\\(.+\\)" pkg)
            (match-string 1 pkg)
          pkg))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-title-parse (string)
    "Parse STRING title.
An example:
  #548965 - levee: [PATCH] fails to build
Returns:
  '(\"548965\" \"levee: [PATCH] fails to build\")
"
    (if (and (stringp string)
             (string-match "#\\([0-9]+\\) +-+ +\\(.*\\)" string))
        (list
         (match-string-no-properties 1 string)
         (match-string-no-properties 2 string))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-field (field)
  "Parse buffer content of Debian BTS (HTTP result).
<p>FIELD: <p>"
  (tinydebian-debian-parse-bts-search field "p"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-reported-by ()
  "Parse buffer content of Debian BTS (HTTP result).
Return '(email field-conent)."
  (if (re-search-forward
       "Reported by:.*submitter=\\([^\"]+\\).>\\([^<]+\\)" nil t)
      (list
       (tinydebian-decode-html (match-string-no-properties 1))
       (tinydebian-decode-html (match-string-no-properties 2)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-parse-bts-bug-p ()
  "Check if content is parseable."
  (goto-char (point-min))
  (tinydebian-debian-parse-bts-bug-title))

;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-debian-parse-bts-bug-info-raw ()
  "Parse buffer content of Debian BTS (HTTP result).

Return assoc list with keys:
    bug package subject maintainer reported date severity tags found fixed done

Were:
    BUG         Bug number
    DATE        Report date in ISO format
    DONE        If done, the reporter information in (...)
    FIXED       if fixed, version(s) where fixed
    MAINTAINER  package maintainer
    PACKAGE     package name
    REPORTED    Reported by information
    SEVERITY    Bug severity
    SUBJECT     Bug subject
    TAGS        List of tags"
    (let (list)
      (goto-char (point-min))
      ;; This must be done in order
      (multiple-value-bind (b str)
          (tinydebian-debian-parse-bts-bug-title-parse
           (tinydebian-debian-parse-bts-bug-title))
        (push (cons "bug" b) list)
        (push (cons "subject" str) list))
      (push (cons "package" (tinydebian-debian-parse-bts-bug-package))
            list)
      (push (cons "maintainer" (tinydebian-debian-parse-bts-bug-maintainer))
            list)
      (multiple-value-bind (email rest)
          (tinydebian-debian-parse-bts-bug-reported-by)
        (push (cons "reported" rest) list))
      (let* ((str (tinydebian-debian-parse-bts-bug-date-iso))
             (date (if (and str
                            (string-match "[0-9-]+" str))
                       (match-string-no-properties 0 str))))
        (push (cons "date" date) list))
      (dolist (field '("severity"
                       "tags"))
        (push (cons field
                    (tinydebian-debian-parse-bts-bug-field field))
              list))
      (push (cons "found"
                  (tinydebian-debian-parse-bts-search-tag "Found in version +"))
            list)
      (push (cons "fixed"
                  (tinydebian-debian-parse-bts-search-tag "Fixed in version +"))
            list)
      (push (cons "done"
                    (tinydebian-debian-parse-bts-bug-field "done"))
            list)
      list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-http-get (url)
  "Return content of URL as string."
  (let ((url-http-attempt-keepalives t) ; Must be definedx
        (buffer (url-retrieve-synchronously url)))
    (if (not buffer)
        (error "TinyDebian: Failed to connect to %s" url)
      (with-current-buffer buffer
        (buffer-substring-no-properties
         (point-min) (point-max))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-debian-url-with 'lisp-indent-function 0)
(put 'tinydebian-debian-url-with 'edebug-form-spec '(body))
(defmacro tinydebian-debian-url-with (&rest body)
  "Run BODY in 'tinydebian--buffer-http-get'."
  `(with-current-buffer (get-buffer-create tinydebian--buffer-http-get)
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-url-bug-initialize-p (bug &optional bts)
  "Check if bug is on `tinydebian--buffer-http-get'.
Optionally from debbugs BTS which defaults to \"debian\"."
  (tinydebian-debian-url-with
    (goto-char (point-min))
    (cond
     ((re-search-forward "There is no record of Bug" nil t)
      nil)
     (t
      (let ((str (tinydebian-debian-parse-bts-bug-p)))
        (if (and str
                 (string-match bug str))
            str))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-url-bug-initialize (bug &optional bts)
  "HTTP GET Debian BUG to buffer `tinydebian--buffer-http-get'.
After that various tinydebian-debian-parse-bts-bug-* functions can be called.
Optionally from debbugs BTS which defaults to \"debian\"."
  (tinydebian-debian-url-with
    (erase-buffer)
    (insert
     (tinydebian-browse-url-http-get
      (cond
       ((or (null bts)
	    (string= "debian" bts))
	(tinydebian-debian-bts-url-compose bug))
       ((string= "emacs" bts)
	(tinydebian-gnu-bts-bug-url-compose bug))
       (t
	(error "Unknown debuuges BTS for http call '%s'" bts)))))
    (if (eq (point-min) (point-max))
        (error "TinyDebian: Failed to fetch Bug %s" bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-url-bug-info (bug &optional bts)
  "Fetch BUG and return `tinydebian-debian-parse-bts-bug-info-raw'.
Optionally from debbugs BTS which defaults to \"debian\"."
  (unless (tinydebian-debian-url-bug-initialize-p bug bts)
    (tinydebian-debian-url-bug-initialize bug bts))
  (tinydebian-debian-url-with
    (tinydebian-debian-parse-bts-bug-info-raw)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-browse-url (url &rest args)
  "Call `browse-url' and ignore ARGS."
  (browse-url url))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-lisp-only (url &optional bug)
  "Open HTTP connection to URL and read result.
  If BUG is set, then read specific BUG page and create buffer for it.
  If buffer already exists, do nothing."
  (ti::process-http-request url (not 'port) (not 'timeout)))

;;; ----------------------------------------------------------------------
;;; FIXME: How to convert HTML into text in lisp?
;;;
(defun tinydebian-browse-url-basic (url &optional mode)
  "Use url.el to retrive URL.
  Optional MODE is hint to activate `tinydebian-bts-mode' on text buffer"
  (message "TinyDebian: Wait, accessing %s" url)
  (let ((buffer
         (url-retrieve-synchronously url)))
    (if (not buffer)
        (error "TinyDebian: Failed to connect to %s" url)
      (tinydebian-with-buffer-macro
          tinydebian--buffer-www
        (insert-buffer-substring buffer)
        (when mode
          (turn-on-tinydebian-bts-mode)
          (let ((font (tinydebian-url-page-font-lock-keywords mode)))
            (when (and font
                       (or tinydebian--font-lock-mode
                           global-font-lock-mode))
              (setq font-lock-keywords font)
              (font-lock-mode 1))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-lynx-dump (url &optional mode)
  "Run lynx(1) with option -dump using URL.
  Optional MODE is hint to activate `tinydebian-bts-mode' on text buffer"
  ;;  For fast lookup, record the binary's full path
  (unless (get 'tinydebian-browse-url-lynx-dump 'done)
    (put 'tinydebian-browse-url-lynx-dump 'done t)
    (put 'tinydebian-browse-url-lynx-dump 'program (executable-find "lynx")))
  (let ((path (get 'tinydebian-browse-url-lynx-dump 'program)))
    (if (not path)
        (error "TinyDebian: [ERROR] `lynx' not found in PATH for %s" url)
      (tinydebian-with-buffer-macro
        tinydebian--buffer-www
        (message "TinyDebian: Wait, accessing %s" url)
        (tinydebian-call-process path nil "-dump" url)
        (when mode
          (turn-on-tinydebian-bts-mode)
          (let ((font (tinydebian-url-page-font-lock-keywords mode)))
            (when (and font
                       (or tinydebian--font-lock-mode
                           global-font-lock-mode))
              (setq font-lock-keywords font)
              (font-lock-mode 1))))
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-browse-url-1 (url &optional mode)
  "Call `tinydebian--browse-url-function' with URL.
  Optional MODE is hint to activate `tinydebian-bts-mode' on result buffer."
  (if tinydebian--browse-url-function
      (funcall tinydebian--browse-url-function url mode)
    (tinydebian-browse-url-browse-url url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-run (url &optional file)
  "Browse URL and optionally save to FILE."
  (let ((tinydebian--browse-url-function tinydebian--browse-url-function))
    ;; FIXME: the `file' is never used?
    (if file
        (setq tinydebian--browse-url-function
              (function tinydebian-browse-url-lynx-dump)))
    (when url
      ;; Display the URL, so that it gets stored in *Messages* buffer for
      ;; later copy/paste
      (message "Accessing %s" url)
      (tinydebian-browse-url-1 url)
      (if file
          (with-current-buffer (get-buffer tinydebian--buffer-www)
            (write-region (point-min) (point-max) file)
            (if (interactive-p)
                  (message "Wrote %s" file))
            file)
        tinydebian--buffer-www))))

;;; ----------------------------------------------------------------------
;;; FIXME: to be removed, obsoleted by tinydebian-bug-browse-url-main
(defun tinydebian-bug-browse-url-by-bug (bug &optional file)
  "Browse by Debian BUG number.

Optionally save bug report to FILE. A prefix argument in
Interactive mode prompts for FILE to save.

NOTE: This function is designed to work only in Gnus Summary and
Article buffers."
  (interactive
   (let* ((prev (get 'tinydebian-bug-browse-url-by-bug 'file))
          (dir  (if prev
                    (file-name-directory prev)))
          (nbr  (read-string "Browse URL by bug number: "
                             (tinydebian-bug-nbr-search)))
          (name (if current-prefix-arg
                    (read-file-name
                     (format "Save bug %s to file: " nbr)
                     dir
                     nil
                     nil
                     (format "%s.txt" nbr)))))
     (put 'tinydebian-bug-browse-url-by-bug 'file name)
     (list nbr name)))
  (setq bug (tinydebian-trim-blanks bug))
  (when (or (not (stringp bug))
            (not (string-match "^[0-9]+$" bug)))
    (error "TinyDebian: Invalid bug number format `%s'." bug))
  (let ((tinydebian--browse-url-function tinydebian--browse-url-function))
    (if file
        (setq tinydebian--browse-url-function
              (function tinydebian-browse-url-lynx-dump)))
    (tinydebian-bug-browse-url-run (tinydebian-bug-url bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-ask-bts-and-number (&optional bts nbr project)
  "Ask BTS system and bug number. Return '(BTS NBR) PROJECT.
Input notes:
  If PROJECT is \"ask\", then query project name from user.
  Otherwise the PROJECT is asked only for certain BTSs."
  ;;
  ;; (completing-read PROMPT COLLECTION &optional PREDICATE REQUIRE-MATCH
  ;;  INITIAL-INPUT HIST DEF INHERIT-INPUT-METHOD)
  ;;
  (unless bts
    (multiple-value-bind (guess-bts guess-bug)
        (tinydebian-bug-bts-type-determine)
      (or nbr
          (setq nbr guess-bug))
      (setq bts guess-bts)))

  (setq bts (completing-read
             "Select BTS (no input = debian): " ;; prompt
             '(("debian" . 1)                   ;; collection
               ("launchpad" . 1)
               ("emacs" . 1)
               ("gnu" . 1)
               ("savannah" . 1)
               ("gnome" . 1)
               ("kde" . 1)
               ("perl-cpan" . 1)
               ("sourceware" . 1)
               ("mysql" . 1)
               ("google" . 1)
               ("freshmeat" . 1))
             nil                                ;; predicate
             t                                  ;; require-match
             bts                                ;; initial-input
             nil                                ;; hist
             "debian"                           ;; def
             ))
  (if (string= bts "")
     (setq bts "debian"))
  (cond
   ((or (string-match "google\\|freshmeat\\|debian\\|emacs" bts)
        (and (stringp project)
             (string-match "ask" project)))
    (setq project (read-string
                   (format "[%s] %s name: "
                           bts
                           (if (tinydebian-bts-type-debbugs-p bts)
                               "Package"
                             "Project"))))))
  (if (and (stringp project)
           (string= project ""))
      (setq project nil))
  (setq nbr (tinydebian-bts-mail-ask-bug-number bts nbr))
  (when (or (not (stringp nbr))
            (not (string-match "[0-9]" nbr)))
    (error "TinyDebian: Error, no bug number given"))
  (list bts
        nbr
        project))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-ask-url-for-bts-and-number
  (&optional bts nbr project)
  "Ask BTS and NBR and possibly PROJECT. Return URL.
If parameters are passed, do not ask, just return URL."
  (multiple-value-bind (bts nbr project)
      (tinydebian-bug-ask-bts-and-number bts nbr project)
    (tinydebian-bts-url-bug-compose bts nbr project)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-main-interactive (&optional ask-file)
  "Interactive part of `tinydebian-bug-browse-url-main'."
  (let* ((prev (get 'tinydebian-bug-browse-url-by-bug 'file))
         (dir  (if prev
                   (file-name-directory prev)))
         (url-str (tinydebian-bug-url-forward))
         (url (multiple-value-bind (bts data)
                  (save-excursion
                    (tinydebian-bug-bts-type-determine))
                (if (and (or (null data)
                             (and (stringp data)
                                  (not (string-match "http" data))))
                         url-str
                         (string-match "http" url-str))
                    (setq data url-str))
                (cond
                 ((and (stringp data)
                       (string-match "http" data))
                  (read-string "Bug URL: " data))
                  (t
                   (or (tinydebian-bug-ask-url-for-bts-and-number
                        bts
                        (if (and (numberp data)
				 (> (string-to-number data)  1))
                            data
                          (tinydebian-bug-nbr-search)))
                       (error "TinyDebian: ERROR, No BTS information."))))))
          (file (if ask-file
                    (read-file-name
                     "Save URL content to file: "
                     dir))))
     (put 'tinydebian-bug-browse-url-by-bug 'file file)
     (list url file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-main (url &optional file)
  "Browse bug URL at point or by searching forward.
In Gnus summary buffer, the Article buffer is consulted for bug."
  (interactive
   (tinydebian-bug-browse-url-main-interactive current-prefix-arg))
  (tinydebian-bug-browse-url-run url))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bug-buffer-name (bug)
  (or bug
      (error "TinyDebian: BUG argument is empty"))
  (format tinydebian--buffer-bug-format bug))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-buffer-or-retrieve (bug)
  "Return buffer for BUG or send HTTP request to read bug.
  Return:
  buffer name"
  (or bug
      (error "TinyDebian: BUG argument is empty"))
  (let* ((name   (tinydebian-bug-buffer-name bug))
         (buffer (get-buffer name))
         (url    (tinydebian-debian-bts-url-compose bug)))
    (if buffer
        buffer
      (setq buffer (get-buffer-create name))
      (ti::process-http-request url (not 'port) (not 'timeout) buffer)
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-by-package-name (package)
  "Jump to PACKAGE description."
  (interactive
   (list (read-string "Browse desription URL by package name: "
                      (my-debian-bug-package-name-any))))
  (when (or (not (stringp package))
            (not (string-match "[a-z]" package)))
    (error "TinyDebian: Invalid package name `%s'." package))
  (tinydebian-browse-url-1
   (tinydebian-packages-browse-url-compose package)
   package))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-browse-url-by-package-bugs (package)
  "Jump to PACKAGE description."
  (interactive
   (list (read-string "Browse bugs URL by package name: "
                      (my-debian-bug-package-name-any))))
  (when (or (not (stringp package))
            (not (string-match "[a-z]" package)))
    (error "TinyDebian: Invalid package name `%s'." package))
  (tinydebian-browse-url-1
   (tinydebian-debian-bts-url-compose package)
   package))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-wnpp-alert-default-mode-bindings ()
  "Define default key bindings to `tinydebian--wnpp-alert-mode-map'."
  (define-key tinydebian--wnpp-alert-mode-map
    (kbd "RET") 'tinydebian-bug-browse-url-by-bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-wnpp-alert-mode-map-activate ()
  "Use local \\{tinydebian--wnpp-alert-mode-map} on this buffer."
  (use-local-map tinydebian--wnpp-alert-mode-map))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinydebian-wnpp-alert-mode (&rest ignore)
  "Turn on WNPP alert mode. IGNORE all arguments.

Mode description:

\\{tinydebian--wnpp-alert-mode-map}"
  (interactive)
  (setq mode-name   tinydebian--wnpp-alert-mode-name)
  (setq major-mode 'tinydebian-wnpp-alert-mode)
  (unless tinydebian--wnpp-alert-mode-map
    (setq tinydebian--wnpp-alert-mode-map (make-sparse-keymap)))
  (run-hooks 'tinydebian--wnpp-alert-mode-hook)
  (tinydebian-wnpp-alert-mode-map-activate))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-show-wnpp-alert-format ()
  "Convert lines to more readable format from current point.

  Original:

  RFH 354176 cvs -- Concurrent Versions System
  O 367169 directvnc -- VNC client using the framebuffer as display

  After formatting:

  RFH 354176 cvs       -- Concurrent Versions System
  O   367169 directvnc -- VNC client using the framebuffer as display"
  (let ((re (concat
             "\\([a-z]+\\) +\\([0-9]+\\) +\\([^ \t\r\n]+\\)"
             " +-- +\\(.*\\)")))
    (while (re-search-forward re nil t)
      (replace-match (format "%-3s %s %-12s -- %s"
                             (match-string 1)
                             (match-string 2)
                             (match-string 3)
                             (match-string 4))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-show-wnpp-alert ()
  "Check installed packages up for adoption or orphaned.
  Requires that program wnpp-alert(1) has been installed."
  (interactive)
  (let* ((bin  "wnpp-alert")
         (path (executable-find bin)))
    (cond
     ((not bin)
      (message "TinyDebian: [ERROR] program `%s' is not installed."
               bin))
     (t
      (tinydebian-with-buffer-macro
          tinydebian--buffer-wnpp-alert
        (message "TinyDebian: wait, running %s..." path)
        (tinydebian-call-process path)
        (message "TinyDebian: wait, running %s... Done." path)
        (goto-char (point-min))
        (save-excursion
          (tinydebian-command-show-wnpp-alert-format))
        (sort-lines nil (point-min) (point-max))
        (tinydebian-wnpp-alert-mode)
        (turn-on-tinydebian-bts-mode)
        (display-buffer buffer)
        buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-show-rc-alert ()
  "Check installed packages for Release Critical bugs.
  Requires that program rc-alert(1) has been installed."
  (interactive)
  (let* ((bin  "rc-alert")
         (path (executable-find bin)))
    (cond
     ((not bin)
      (message "TinyDebian: [ERROR] program `%s' is not installed."
               bin))
     (t
      (tinydebian-with-buffer-macro
          tinydebian--buffer-rc-alert
        (message "TinyDebian: wait, running %s..." path)
        (tinydebian-call-process path)
        (message "TinyDebian: wait, running %s... Done." path)
        (goto-char (point-min))
        ;; (save-excursion
        ;;   (tinydebian-command-show-wnpp-alert-format))
        ;; (sort-lines nil (point-min) (point-max))
        ;; (tinydebian-wnpp-alert-mode)
        (turn-on-tinydebian-bts-mode)
        (display-buffer buffer)
        buffer)))))

;;}}}
;;{{{ Mail: message-mode, mail-mode

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-list (header)
  "Return list of addresses in HEADER."
  (let* ((str  (mail-fetch-field header))
         (list (and str
                    (split-string str "[ \t\r\n]*,[ \t\r\n]*"))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-header-matches (header re)
  "Return list of addresses from HEADER matching RE."
  (let (list)
    (dolist (elt (tinydebian-mail-address-list header))
      (when (string-match re elt)
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-header-not-matches (header re)
  "Return list of addresses from HEADER not matching RE."
  (let (list)
    (dolist (elt (tinydebian-mail-address-list header))
      (unless (string-match re elt)
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-p (re)
  "Check of Headers To or Cc have addresses matching RE."
  (or (tinydebian-mail-header-matches "To" re)
      (tinydebian-mail-header-matches "Cc" re)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-address-match-type-p (type &optional bug)
  "Check if TYPE@ address is already set.
Optional bug checks BUG-TYPE@ address."
  (let ((re (if (not bug)
                (format "%s@" type)
              (format  "%s-%s@"
                       (if (numberp bug)
                           (number-to-string bug)
                         bug)
                       type))))
    (tinydebian-mail-address-match-p re)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-quiet-p (&optional bug)
  (tinydebian-mail-address-match-type-p "quiet" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-maintonly-p (&optional bug)
  (tinydebian-mail-address-match-type-p "maintonly" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-submitter-p (&optional bug)
  (tinydebian-mail-address-match-type-p "submitter" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-bug-p (&optional bug)
  (tinydebian-mail-address-match-type-p "%s@" bug))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-address-match-control-p (&optional bug)
  "Check if control address is already set."
  (tinydebian-mail-address-match-p "control@"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-header-set (header value)
  "Set HEADER to VALUE. if VALUE is nil, remove field."
  (save-excursion
    (save-restriction
      (mail-position-on-field header)
      (mail-header-narrow-to-field)
      (cond
       ((stringp value)
        (re-search-forward "^[^ \t\r\n]+: ?")
        (delete-region (point) (point-max))
        (insert value)
        (if (not (string-match "\n\\'" value))
            (insert "\n")))
       (t
        (delete-region (point-min) (point-max)))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-header-subject-set (value)
  "Set Subject header to VALUE."
  (tinydebian-mail-header-set "Subject" value))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-header-remove-item (header re)
  "Remove from To or Cc HEADER all items matching RE."
  (let ((field (mail-fetch-field header))
        list)
    (when (and field
               (string-match re field))
      (setq field nil)
      (if (setq list (tinydebian-mail-header-not-matches header re))
          (setq field
                (mapconcat 'concat
                           (tinydebian-mail-header-not-matches header re)
                           ", ")))
      ;; We must not remove "To: field
      (if (and (null field)
               (string-match "To" header))
          (setq field ""))
      (tinydebian-mail-header-set header field)
      t)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-header-send-remove-item-regexp (re)
  "Rmeove from headers To and CC items matching RE."
  (save-excursion
    (or (tinydebian-mail-header-remove-item "To" re)
        (tinydebian-mail-header-remove-item "Cc" re))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-add (header email)
  "Add EMAIL to Cc header."
  (unless (tinydebian-mail-address-match-p (regexp-quote email))
    (let ((cc (mail-fetch-field header)))
      (setq cc
            (if (and cc
                     (string-match "@" cc))
                ;; Append to field
                (format "%s, %s" cc email)
              ;; New field
              email))
      (tinydebian-mail-header-set header cc))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-email-add (email)
  "Add email."
  (tinydebian-mail-mode-debian-address-add "To" email))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-email-remove (email)
  "Remove email."
  (tinydebian-mail-header-send-remove-item-regexp email))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-manipulate (email &optional re)
  "Add EMAIL address. Remove addresses matching RE."
  (if re
      (tinydebian-mail-header-send-remove-item-regexp re))
  (tinydebian-mail-mode-debian-address-add
   "To"
   email))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-tag-add (bug type &optional re)
  "Remove BUG-TYPE address. Remove addresses by RE."
  (tinydebian-mail-mode-debian-address-manipulate
   (tinydebian-bts-email-compose type bug)
   re))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-add-standard-bug (bug &optional re)
  "Add BUG@ address. Remove all other addresses related to BUG.
Remove addresses by RE."
  (save-excursion
    (tinydebian-mail-mode-debian-address-manipulate
     (tinydebian-bts-email-compose bug)
     re)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-tag-toggle
  (bug type &optional remove interactive re)
  "Add BUG-TYPE address or optionally REMOVE.
In interactive call, toggle TYPE of address on and off.
Remove addresses by RE."
  ;; toggle
  (when interactive
    (when (and (null remove)
               (tinydebian-mail-address-match-type-p type bug))
      (setq remove t)))
  (if remove
      (tinydebian-mail-mode-debian-address-add-standard-bug bug re)
    (tinydebian-mail-mode-debian-address-tag-add bug type re)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-ask-bug ()
  "Return bug number from To, Cc fields etc. or ask with MSG."
  (let ((nbr (or (tinydebian-bug-nbr-at-current-point)
                 (tinydebian-bug-nbr-current-line)
                 (tinydebian-email-cc-to-bug-nbr)
                 (tinydebian-email-subject-bug-nbr)
                 (tinydebian-email-debian-control-commands)
                 (tinydebian-bug-nbr-forward)
                 (tinydebian-bug-nbr-buffer))))
    (if nbr
        nbr
      (tinydebian-bts-mail-ask-bug-number))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-ask-args (&optional msg)
  "Return bug number and value of `current-prefix-arg'.
If bug number cannot be determined from To or Cc headers, ask MSG from user."
  (list
   (tinydebian-mail-mode-debian-address-ask-bug)
   current-prefix-arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-quiet-toggle (bug &optional remove)
  "Add NNN-quiet address or optionally REMOVE.
In interactive call, toggle quiet address on and off."
  (interactive (tinydebian-mail-mode-debian-address-ask-args "Quiet bug"))
  (tinydebian-mail-mode-debian-address-tag-toggle
   bug
   "quiet"
   remove
   (interactive-p)
   (format "%s@\\|%s-\\(close\\|quiet\\)@" bug bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-close-toggle (bug &optional remove)
  "Add NNN-close address or optionally REMOVE.
In interactive call, toggle quiet address on and off."
  (interactive (tinydebian-mail-mode-debian-address-ask-args "Close bug"))
  (tinydebian-mail-mode-debian-address-tag-toggle
   bug
   "close"
   remove
   (interactive-p)
   (format "%s@\\|%s-\\(close\\|quiet\\)@" bug bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-address-type-add
  (type &optional bug remove interactive)
  "Add TYPE of BUG address (control, maintonly, submitter).
Optionally REMOVE. In interactive call, toggle TYPE of address on and off."
  ;; toggle
  (when interactive
    (when (and (null remove)
               (tinydebian-mail-address-match-type-p type))
      (setq remove t)))
  (save-excursion
    (let ((email (tinydebian-bts-generic-email-control)))
      (cond
       (remove
        (tinydebian-mail-mode-debian-address-email-remove email))
       (t
        (tinydebian-mail-mode-debian-address-email-add email))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-submitter-toggle
  (bug &optional remove)
  "Add submitter address or optionally REMOVE.
In interactive call, toggle conrol address on and off."
  (interactive (tinydebian-mail-mode-debian-address-ask-args "Submitter bug"))
  ;; toggle
  (tinydebian-mail-mode-address-type-add
   "submitter" bug remove (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-bug-p (bug)
  "Return field name if BUG address exists."
  (let ((to  (mail-fetch-field "To"))
	(cc  (mail-fetch-field "Cc"))
	(bcc (mail-fetch-field "Bcc"))
	(re  (format "%s@\\|%s-\\(close\\|quiet\\)@" bug bug))
    (cond
     ((string-match  re to)
      "To")
     ((string-match  re cc)
      "Cc")
     ((string-match  re bcc)
      "Bcc")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-bug-toggle (bug &optional remove)
  "Add BUG address or optionally REMOVE."
  (interactive
   (let ((now (or (tinydebian-bug-nbr-at-current-point)
		  (tinydebian-bug-nbr-current-line)))
	 bug)
     (list
      (or now
	  (tinydebian-mail-mode-debian-address-ask-bug))
      current-prefix-arg)))
  (let ((re (format "%s@\\|%s-\\(close\\|quiet\\)@" bug bug)))
    (when (interactive-p)
      (when (and (null remove)
                 (tinydebian-mail-address-match-p re))
        (setq remove t)))
    (if remove
        (tinydebian-mail-header-send-remove-item-regexp re)
      (tinydebian-mail-mode-debian-address-add-standard-bug
       bug
       (format re bug)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-package-toggle
  (package &optional remove)
  "Add PACKAGE address or optionally REMOVE."
  (interactive
   (let* ((re      "\\([a-z][^ \t\r\n]*\\)@packages.debian.org")
          (address (tinydebian-mail-address-match-p re))
          (pkg     (if (and address
                            (string-match re (car address)))
                       (match-string 1 (car address)))))
     (list
      (read-string "package name: " pkg)
      (if pkg
          t
        current-prefix-arg))))
  ;; toggle
  (let ((email (format "%s@packages.debian.org" package)))
    (cond
     (remove
      (tinydebian-mail-mode-debian-address-email-remove email))
     (t
      (tinydebian-mail-mode-debian-address-email-add email)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-control-toggle (&optional remove)
  "Add control address or optionally REMOVE.
In interactive call, toggle conrol address on and off."
  (interactive "P")
  ;; toggle
  (tinydebian-mail-mode-address-type-add
   "control" nil remove (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-mail-mode-debian-address-maintonly-toggle (bug &optional remove)
  "Add maintonly address or optionally REMOVE.
In interactive call, toggle conrol address on and off."
  (interactive (tinydebian-mail-mode-debian-address-ask-args "Submitter bug"))
  ;; toggle
  (tinydebian-mail-mode-address-type-add
   "maintonly" bug remove (interactive-p)))


(defvar tinydebian--bts-mail-ctrl-finalize-line-regexp
  (concat "^[ \t]*"
          (regexp-opt
           '("quit"
             "stop"
             "thank"
             "thanks"
             "thankyou"
             "thank you"))
          "[ \t]*$")
  "Regexp to indicate BTS command end.")

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-empty-line ()
  "Empty current line."
  (delete-region (line-beginning-position) (line-end-position)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-finalize-position (&optional point)
  "At `point-min' or POINT, return positon of command stop marker: thanks etc."
  (save-excursion
    (goto-char (or point (point-min)))
    (when (re-search-forward
           tinydebian--bts-mail-ctrl-finalize-line-regexp
           nil t)
      (match-beginning 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-finalize-line-p ()
  "Check current line is control command end (finalize line)."
  (string-match
   tinydebian--bts-mail-ctrl-finalize-line-regexp
   (tinydebian-current-line-string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-bts-mail-ctrl-command-finalize-add ()
  "Add 'finalize' marker."
  (insert "thanks\n"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-position (command &optional point)
  "At `point-min' or POINT, return position of command as list '(BEG END)."
  (save-excursion
    (goto-char (or point (point-min)))
    (when (re-search-forward (format "^\\([ \t]*%s[ \t]*\\)" command) nil t)
      (list (match-beginning 0) (match-end 0)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-goto (command)
  "Goto command."
  (multiple-value-bind (beg end)
      (tinydebian-bts-mail-ctrl-command-position command)
    (if end
        (goto-char end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-email-body-start-position ()
  "Return body start position in email."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\(" (regexp-quote mail-header-separator) "\\)")
           nil t)
      (match-beginning 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-goto-body-start ()
  "Goto start of email body."
  (goto-char (point-min))
  (re-search-forward            ;Signals error if not found
   (concat "^" (regexp-quote mail-header-separator)))
  (forward-line 1))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-remove (re)
  "Remove all command lines matching RE"
  (let (point)
    (tinydebian-bts-mail-ctrl-command-goto-body-start)
    (unless (setq point (tinydebian-bts-mail-ctrl-finalize-position))
      (error "No control command finalize marker 'thanks' found"))
    (save-restriction
      (narrow-to-region (point) point)
      (flush-lines re))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-insert (string)
  "Insert STRING containing control command at current position."
  (unless (string-match "\n\\'" string)
    (setq string (concat string "\n")))
  (insert string))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-add (string &optional re at-beg)
  "Add STRING containing control command.

Input:
  STRING   Commadn to add
  RE       Optional. If non-nil, remove all command lines matching regexp
  AT-BEG   Optional. If non-nil, add command to the beginning."
  (unless (tinydebian-bts-mail-ctrl-finalize-position)
    (tinydebian-bts-mail-ctrl-command-goto-body-start)
    (tinydebian-bts-mail-ctrl-command-finalize-add))
  (if re
      (tinydebian-bts-mail-ctrl-command-remove re))
  (if at-beg
      (tinydebian-bts-mail-ctrl-command-goto-body-start)
    (goto-char (tinydebian-bts-mail-ctrl-finalize-position)))
  (tinydebian-bts-mail-ctrl-command-insert string))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-mail-ctrl-command-add-macro 'edebug-form-spec '(body))
(put 'tinydebian-bts-mail-ctrl-command-add-macro 'lisp-indent-function 0)
(defmacro tinydebian-bts-mail-ctrl-command-add-macro
  (cmd bug &optional string at-beg)
  "Compose Control command.

Input:
  CMD     Command
  BUG     Bug number
  STRING  Optional. Additional parameter for CMD
  AT-BEG  Optional. If non-nil, add command to the beginning."
  `(let ((str (if ,string
                  (format "%s %s %s" ,cmd ,bug ,string)
                (format "%s %s" ,cmd ,bug)))
         (re  (format "^[ \t]*%s" ,cmd)))
     (tinydebian-bts-mail-ctrl-command-add str re ,at-beg)
     ;; Add control@ address
     (tinydebian-mail-mode-address-type-add "control")
     ;; Position point to better place.
     (forward-line -1)
     (re-search-forward (format "%s %s *" ,cmd ,bug) nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-cc (&optional list)
  "Add pseudo header and LIST of email addresses.
If LIST if nil, position point at pseudo header."
  (interactive)
  (let ((cmd (format "X-Debbugs-CC: %s" (mapconcat 'concat list " "))))
    (tinydebian-bts-mail-ctrl-command-add cmd)
    (forward-line -1)
    (re-search-forward ":" nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-clone-in-mail (bug)
  "Mark BUG notforwarded."
  (interactive
   (list
    ;; tinydebian-bts-mail-ask-bug-number
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "clone" bug)
  (insert " -1\nretitle -1 "))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-clone (bug)
  "Select correct clone command for BUG.
If in mail buffer, run
otgerwise run `tinydebian-bts-mail-ctrl-clone-new-mail'."
  (if (memq major-mode '(message-mode mail-mode))
      (tinydebian-bts-mail-ctrl-command-clone-in-mail bug)
    (tinydebian-bts-mail-ctrl-clone-new-mail bug)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-fixed (bug version)
  "Mark BUG fixed in VERSION."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Fixed in version: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "fixed" bug version))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-notfixed (bug version)
  "Mark BUG notfixed in VERSION."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Package version where bug was *not* fixed: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "notfixed" bug version))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-forwarded (bug uri)
  "Mark BUG forwarded to URI."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Forward to URI: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "forwarded" bug uri))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-found (bug version)
  "Mark BUG found in VERSION."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Package version where bug was found: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "found" bug version))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-notfound (bug version)
  "Mark BUG notfound in VERSION."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Package version where bug was *not* found: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "notfound" bug version))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-notforwarded (bug)
  "Mark BUG notforwarded."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "notforwarded" bug))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-merge (bug list)
  "Mark BUG merged with LIST of bug numbers."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (tinydebian-bts-ctrl-list-ask "Merge with bug")))
  (let ((string (mapconcat 'concat list " ")))
    (tinydebian-bts-mail-ctrl-command-add-macro "merge" bug string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-no-ack ()
  "Insert command to disable acknowledgement mail from the BTS."
  (interactive)
  (save-excursion
    (tinydebian-bts-mail-ctrl-command-add "X-Debbugs-No-Ack: enable"
                                          "^X-Debbugs-No-Ack:")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-owner (bug)
  "Add owner for the BUG."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "owner" bug "!"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-no-owner (bug)
  "Remove the owner of BUG."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "noowner" bug))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-reassign (bug package)
  "Reassign BUG to PACKAGE."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Reassign to package: ")))
  (tinydebian-bts-mail-ctrl-command-add-macro "reassign" bug package))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-reopen (bug)
  "Reopen BUG."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "reopen" bug nil 'beg))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-retitle (bug string)
  "Retitle BUG with STRING."
  (interactive
   (let ((subject (mail-fetch-field "Subject")))
     (list
      (tinydebian-mail-mode-debian-address-ask-bug)
      (read-string "New title: " subject))))
  (tinydebian-bts-mail-ctrl-command-add-macro "retitle" bug string))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-submitter (bug address)
  "Changes the originator of BUG to ADDRESS."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (read-string "Set submitter address to: " "!")))
  (tinydebian-bts-mail-ctrl-command-add-macro "submitter" bug address))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-severity (bug severity)
  "Add command for BUG SEVERITY."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (completing-read
     "Bug severity: "
     tinydebian--severity-list
     nil
     'match)))
  (tinydebian-bts-mail-ctrl-command-add-macro "severity" bug severity))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-tags (bug list)
  "Set BUG LIST to tags."
  (interactive
   (let ((bug (tinydebian-mail-mode-debian-address-ask-bug)))
     (list
      bug
      (tinydebian-bts-ctrl-tags-ask
       (format  "BTS tag #%s [RET when done]: " bug)))))
  (tinydebian-bts-mail-ctrl-command-add-macro
   "tags"
   bug
   (concat "+ " (mapconcat 'concat list " "))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-unarchive (bug)
  "Unarchive BUG to be able to respond to it.

After preset number days, the bug is archived, at which point no
more changes can be made. Trying to send mail (or merge with
it) to the bug after that will be rejected. To make any
changes, the bug must be unarchived first."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)))
  (tinydebian-bts-mail-ctrl-command-add-macro "unarchive" bug nil 'beg))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-command-usertag (bug list)
  "Set BUG LIST to usertags."
  (interactive
   (list
    (tinydebian-mail-mode-debian-address-ask-bug)
    (tinydebian-bts-ctrl-list-ask "Usertag")))
  (tinydebian-bts-mail-ctrl-command-add-macro
   "usertag"
   bug
   (concat "+ " (mapconcat 'concat list " "))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-debian-bug-info-macro 'edebug-form-spec '(body))
(put 'tinydebian-debian-bug-info-macro 'lisp-indent-function 2)
(defmacro tinydebian-debian-bug-info-macro (bug bts &rest body)
  "Get BUG to variable `info', define function `field', and run BODY.
See `tinydebian-debian-parse-bts-bug-info-raw' for INFO structure.
Optionally from debbugs BTS which defaults to \"debian\"."
  `(progn
     (if (or (not (stringp ,bug))
             (not (string-match "^[0-9][0-9]+$" ,bug)))
         (error "Invalid bug number: %s" ,bug))
     (let ((info (tinydebian-debian-url-bug-info ,bug ,bts)))
       (flet ((field (x)
                     (cdr-safe
                      (assoc x info))))
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-move-point-to-free-place ()
  "Move to place which is free (past word)."
  (unless (string-match (format "%c" (char-syntax (char-before))) " ")
    (skip-chars-forward "^ \t\r\n")
    (if (eobp)
        (insert " ")
      (forward-char 1))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-debian-bug-info-message-all-macro 'edebug-form-spec '(body))
(put 'tinydebian-debian-bug-info-message-all-macro 'lisp-indent-function 2)
(defmacro tinydebian-debian-bug-info-message-all-macro (bug bts &rest body)
  "Set `str' tor BUG information string and run BODY.
Optionally from debbugs BTS which defaults to \"debian\"."
  `(tinydebian-debian-bug-info-macro ,bug ,bts
     (let (str)
       (setq str
             (format
              "%s %s %s %s /%s/ [%s]  %s %s"
              (field "bug")
              (field "date")
              (field "package")
              (field "severity")
              (field "tags")
              (field "maintainer")
              (field "reported")
              (field "subject")))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-generic-bug-subject (bug &optional bts buffer)
  "Return BUG subject for optional BTS.
If BTS is nil, the consult BUFFER or `current-buffer' for BTS type."
  (with-current-buffer (or buffer (current-buffer))
    ;; FIXME launchpad
    (let (subject)
      (cond
       ((or (not (stringp bts))
	    (string= "debian" bts)
	    (string= "emacs" bts))
	(tinydebian-debian-bug-info-macro bug bts
	  (let ((fsub (field "subject"))
		(fpkg (field "package"))
		(pkg ""))
	    ;; Check '<package>: <message>' -- the standard bug report format
	    ;; Add one if it's not in bug report by default.
	    (unless (string-match fpkg fsub)
	      (setq pkg (format "%s: " fpkg)))
	    (setq subject
		  (format "Re: Bug#%s: %s%s"
			  bug
			  pkg
			  fsub)))))
       (t
	;; FIXME: add support for more BTS
	))
      subject)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-bug-info-all-insert (bug &optional bts)
  "Insert BUG information at point (after word).
If `inteactive-p' and `buffer-read-only', display infromation only.
Optionally from debbugs BTS which defaults to \"debian\"."
  (interactive (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-debian-bug-info-message-all-macro bug bts
    (if (and (interactive-p)
             buffer-read-only)
        (message str)
      (tinydebian-move-point-to-free-place)
      (insert str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-bug-info-all-message (bug)
  "Display BUG information."
  (interactive (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-debian-bug-info-message-all-macro bug
    (message str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-bug-info-subject-insert (bug &optional bts)
  "Insert bug subject at point (after word).
If `inteactive-p' and `buffer-read-only', display infromation only.
Optionally from BTS which defaults to \"debian\"."
  (interactive (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-debian-bug-info-macro bug bts
    (let ((subject (field "subject")))
      (if (and (interactive-p)
               buffer-read-only)
          (message subject)
        (tinydebian-move-point-to-free-place)
        (insert subject)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-debian-bug-info-subject-message (bug &optional bts)
  "Display BUG title (subject).
Optionally from BTS which defaults to \"debian\"."
  (interactive (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-debian-bug-info-macro bug bts
    (message (field "subject"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-mail-mode-map-activate ()
  "Use local \\{tinydebian--mail-mode-map} on this buffer."
  (use-local-map tinydebian--mail-mode-map))

(eval-and-compile

;;;###autoload (autoload 'tinydebian-mail-mode          "tinydebian" "" t)
;;;###autoload (autoload 'turn-on-tinydebian-mail-mode  "tinydebian" "" t)
;;;###autoload (autoload 'turn-off-tinydebian-mail-mode "tinydebian" "" t)
;;;###autoload (defvar tinydebian--mail-mode-prefix-key "\C-c-")
  (ti::macrof-minor-mode-wizard
   "tinydebian-mail-" " Tbts" "\C-c-" "Tbts" 'TinyDebian "tinydebian--mail-" ;1-6

   "Debian Bug Tracking System (BTS) Minor mode. This mode helps
composing the BTS messages in mail send buffer.

Prefix key is:

  tinydebian--mail-mode-prefix-key

Mode description:

\\{tinydebian--mail-mode-prefix-map}"

   "TinyDebian BTS mail send mode"

   nil

   "TinyDebian BTS mail send minor mode menu."

   (list
    tinydebian--mail-mode-easymenu-name
    ["Address Bug"         tinydebian-mail-mode-debian-address-bug-toggle       t]
    ["Address Control"     tinydebian-mail-mode-debian-address-control-toggle   t]
    ["Address Quiet"       tinydebian-mail-mode-debian-address-quiet-toggle     t]
    ["Address Close"       tinydebian-mail-mode-debian-address-close-toggle     t]
    ["Address Submitter"   tinydebian-mail-mode-debian-address-submitter-toggle t]
    ["Address Maintonly"   tinydebian-mail-mode-debian-address-maintonly-toggle t]
    ["Address Package maintainer" tinydebian-mail-mode-debian-address-package-toggle t]

    "----"

;;    ["Goto URL by bug number"        tinydebian-bug-browse-url-by-bug          t]
    ["Goto URL by bug number"        tinydebian-bug-browse-url-main            t]

    ["Goto URL by package bugs"      tinydebian-bug-browse-url-by-package-bugs t]
    ["Goto URL by package name"      tinydebian-bug-browse-url-by-package-name t]

    "----"

    ["Info bug all insert"     tinydebian-debian-bug-info-all-insert     t]
    ["Info bug all message"    tinydebian-debian-bug-info-all-message    t]
    ["Info bug subject insert" tinydebian-debian-bug-info-subject-insert t]

    "----"

    ["Add BTS Ctrl CC"         tinydebian-bts-mail-ctrl-command-cc       t]
;;;    ["Send BTS Ctrl close"      tinydebian-bts-mail-ctrl-command-close    t] ;; FIXME
    ["Add BTS Ctrl clone"      tinydebian-bts-mail-ctrl-clone    t]
    ["Add BTS Ctrl fixed"      tinydebian-bts-mail-ctrl-command-fixed t]
    ["Add BTS Ctrl notfixed"   tinydebian-bts-mail-ctrl-command-notfixed    t]
    ["Add BTS Ctrl forward"    tinydebian-bts-mail-ctrl-command-forwarded  t]
    ["Add BTS Ctrl notforwarded"    tinydebian-bts-mail-ctrl-command-notforwarded  t]
;;;    ["Addend BTS Ctrl forwarded"  tinydebian-bts-mail-ctrl-command-forward  t] ;; FIXME
    ["Add BTS Ctrl found"      tinydebian-bts-mail-ctrl-command-found    t]
    ["Add BTS Ctrl notfound"   tinydebian-bts-mail-ctrl-command-notfound t]
    ["Add BTS Ctrl merge"      tinydebian-bts-mail-ctrl-command-merge    t]
    ["Add BTS Ctrl No Ack"     tinydebian-bts-mail-ctrl-command-no-ack   t]
    ["Add BTS Ctrl Owner"      tinydebian-bts-mail-ctrl-command-owner    t]
    ["Add BTS Ctrl No-owner"   tinydebian-bts-mail-ctrl-command-no-owner t]
    ["Add BTS Ctrl reassign"   tinydebian-bts-mail-ctrl-command-reassign t]
    ["Add BTS Ctrl retitle"    tinydebian-bts-mail-ctrl-command-retitle  t]
    ["Add BTS Ctrl reopen"     tinydebian-bts-mail-ctrl-command-reopen   t]
    ["Add BTS Ctrl submitter"  tinydebian-bts-mail-ctrl-command-ubmitter t]
    ["Add BTS Ctrl severity"   tinydebian-bts-mail-ctrl-command-severity t]
    ["Add BTS Ctrl tags"       tinydebian-bts-mail-ctrl-command-tags     t]
    ["Add BTS Ctrl unarchive"  tinydebian-bts-mail-ctrl-command-unarchive t]
    ["Add BTS Ctrl usertag"    tinydebian-bts-mail-ctrl-command-usertag  t]
    ["Insert Bug number"       tinydebian-bug-nbr-insert-at-point t])

   (progn
     (define-key map  "ia" 'tinydebian-debian-bug-info-all-insert)
     (define-key map  "iA" 'tinydebian-debian-bug-info-all-message)
     (define-key map  "is" 'tinydebian-debian-bug-info-subject-insert)
     (define-key map  "iS" 'tinydebian-debian-bug-info-subject-message)
     (define-key map  "b"  'tinydebian-mail-mode-debian-address-bug-toggle)
     (define-key map  "m"  'tinydebian-mail-mode-debian-address-maintonly-toggle)
     (define-key map  "o"  'tinydebian-mail-mode-debian-address-close-toggle)
     (define-key map  "p"  'tinydebian-mail-mode-debian-address-package-toggle)
     (define-key map  "q"  'tinydebian-mail-mode-debian-address-quiet-toggle)
     (define-key map  "s"  'tinydebian-mail-mode-debian-address-submitter-toggle)
     (define-key map  "t"  'tinydebian-mail-mode-debian-address-control-toggle)

     (define-key map  "ub"  'tinydebian-bug-browse-url-by-bug)
     (define-key map  "uB"  'tinydebian-bug-browse-url-by-package-bugs)
     (define-key map  "up"  'tinydebian-bug-browse-url-by-package-name)

     ;;  (C)ontrol commands
;;      (define-key map  "cf"  'tinydebian-bts-mail-ctrl-command-forward)  ;; FIXME
     (define-key map  "cc"  'tinydebian-bts-mail-ctrl-command-cc)
     (define-key map  "cC"  'tinydebian-bts-mail-ctrl-clone)
     (define-key map  "cfn" 'tinydebian-bts-mail-ctrl-command-notforwarded)
     (define-key map  "cF"  'tinydebian-bts-mail-ctrl-command-forwarded)
     (define-key map  "cm"  'tinydebian-bts-mail-ctrl-command-merge)
     (define-key map  "cn"  'tinydebian-bts-mail-ctrl-command-no-ack)
     (define-key map  "cn"  'tinydebian-bts-mail-ctrl-command-found)
     (define-key map  "cN"  'tinydebian-bts-mail-ctrl-command-notfound)
     (define-key map  "co"  'tinydebian-bts-mail-ctrl-command-reopen)
     (define-key map  "cr"  'tinydebian-bts-mail-ctrl-command-reassign)
     (define-key map  "cR"  'tinydebian-bts-mail-ctrl-command-retitle)
     (define-key map  "cs"  'tinydebian-bts-mail-ctrl-command-severity)
     (define-key map  "cS"  'tinydebian-bts-mail-ctrl-command-submitter)
     (define-key map  "ct"  'tinydebian-bts-mail-ctrl-command-tags)
     (define-key map  "cT"  'tinydebian-bts-mail-ctrl-command-usertag)
     (define-key map  "cu"  'tinydebian-bts-mail-ctrl-command-unarchive)
     (define-key map  "cw"  'tinydebian-bts-mail-ctrl-command-owner)
     (define-key map  "cW"  'tinydebian-bts-mail-ctrl-command-no-owner)
     (define-key map  "cx"  'tinydebian-bts-mail-ctrl-command-fixed)
     (define-key map  "cX"  'tinydebian-bts-mail-ctrl-command-notfixed))))

;;}}}
;;{{{ BTS URL pages

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-with-url-page-type-macro 'edebug-form-spec '(body))
(put 'tinydebian-with-url-page-type-macro 'lisp-indent-function 1)
(defmacro tinydebian-with-url-page-type-macro (page-type &rest body)
  "Retrieve PAGE-TYPE from `tinydebian--debian-url-page-alist' and run BODY.
  Variable `page'is bound to the retrieved value.
  Signal error if PAGE-TYPE is not found."
  `(let ((page (assoc ,page-type tinydebian--debian-url-page-alist)))
     (unless page
       (error "TinyDebian: unknown page-typpe `%s'" ,page-type))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-page-compose (page-type)
  "Return URL location of PAGE-TYPE."
  (tinydebian-with-url-page-type-macro page-type (nth 1 page)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-debian-mentors-url (package &optional section)
  "Return PACKAGE URL to mentors.debian.net in optional SECTION (def. main)."
  (let ((first-char (substring package 0 1)))
    (format "%s/%s/%s/%s"
            (tinydebian-url-page-compose 'mentors-pkg-pool)
            (or section "main")
            first-char
            package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-debian-browse-url (page-type &optional mode)
  "Browse Debian pages.
  Optional MODE is hint to activate `tinydebian-bts-mode' on result buffer."
  (let ((url (tinydebian-url-page-compose page-type)))
    (unless url
      (error "TinyDebian: Unknown URL request `%s'." page-type))
    (cond
     ((and (tinydebian-string-p url)
           (string-match "^/" url))
      (when (and (string-match "z$" url)
                 (null auto-compression-mode))
        (auto-compression-mode 1))
      (if (file-exists-p url)
          (find-file-other-window url)
        (error "TinyDebian: need 'apt-get install ...' (not found %s)"
               url)))
     ((string-match ":" url)
      (tinydebian-browse-url-1 url mode))
     (t
      (error "TinyDebian: browse internal error `%s' `%s' `%s'"
             page-type mode url)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-bts-ctrl-page ()
  "Browse BTS control page."
  (interactive)
  (tinydebian-url-debian-browse-url 'bts-control))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-new-maintainer-guide ()
  "Browse Debian New Maintainers' Guide."
  (interactive)
  (tinydebian-url-debian-browse-url 'newmaint-guide))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-best-practises ()
  "Browse  Debian Developer's Reference Chapter 6 - Best Packaging Practices."
  (interactive)
  (tinydebian-url-debian-browse-url 'best-practices))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-developers-reference (&optional text-file)
  "Browse  Debian Developer's Reference.
  Optionally use TEXT-FILE from /usr/share/doc if found."
  (interactive "P")
  (tinydebian-url-debian-browse-url
   (if text-file
       'developers-reference-text
     'developers-reference)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-policy-manual (&optional text-file)
  "Browse policy manual page.
  Optionally use TEXT-FILE from /usr/share/doc if found."
  (interactive "P")
  (tinydebian-url-debian-browse-url
   (if text-file
       'policy-text
     'policy)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-bugs-by-rc ()
  "Browse release critical bugs."
  (interactive)
  (tinydebian-url-debian-browse-url 'bugs-rc 'bugs-rc))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-package-debcheck (package &optional distribution)
  "Check package for debcheck problems.
  Optionally from DISTRIBUTION which defaults to `testing'."
  (interactive
   (list
    (read-string "Debcheck package: ")
    (completing-read "Distribution: "
                     '(("stable" . 1)
                       ("testing" . 1)
                       ("unstable" . 1)
                       ("experimental" . 1))
                     (not 'predicate)
                     (not 'require-match))))
  (when (and (stringp package)
             (not (string= "" package)))
    (tinydebian-browse-url-1
     (format (tinydebian-url-page-compose 'debcheck-package)
             (or distribution  "testing")
             package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-qa-developer-status (email)
  "Browse QA developer status information by EMAIL address."
  (interactive
   (list (read-string "[QA status] developer's email address: "
                      (tinydebian-email-search))))
  (tinydebian-string-p
   email
   (format "[ERROR] email is missing from input [%s]" email))
  (tinydebian-browse-url-1
   (format "%slogin=%s"
           (tinydebian-url-page-compose 'qa-developer-status)
           email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-qa-developer-bugs (email)
  "Browse QA developer bugs information by EMAIL address."
  (interactive
   (list (read-string "[QA bugs] developer's email address:"
                      (tinydebian-email-search))))
  (tinydebian-string-p
   email
   (format "[ERROR] email is missing from input [%s]" email))
  (tinydebian-browse-url-1
   (format "%ssubmitter=%s"
           (tinydebian-url-page-compose 'qa-developer-bugs)
           email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-dsfg-license-faq ()
  "Browse DFSG FAQ about Licenses."
  (interactive)
  (tinydebian-browse-url-1 (tinydebian-url-page-compose 'dfsg-license-faq)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-base-files-faq ()
  "Browse base-files FAQ."
  (interactive)
  (tinydebian-browse-url-1 (tinydebian-url-page-compose 'base-files-faq)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-package-by-filename (filename &optional arch)
  "Package content search by FILENAME and optional ARCH."
  (interactive
   (let ((name (read-string "[Pkg search] filename: "))
         (arch (read-string "[Pkg search] architecture [RET=all]: ")))
     (list name arch)))
  (tinydebian-string-p
   filename
   (format "[ERROR] filename is missing from input [%s]" filename))
  ;; http://packages.debian.org/cgi-bin/search_contents.pl?word=svn_load_dirs&searchmode=searchfiles&case=insensitive&version=stable&arch=i386
  (tinydebian-browse-url-1
   (format "%s%s&word=%s"
           (tinydebian-url-page-compose 'pkg-search-files)
           (if (tinydebian-string-p arch)
               (format "&arch=%s" arch)
             "")
           filename)))

(defun tinydebian-grep-find-debian-devel (regexp grep-opt)
  "Grep REGEXP from all ddevelopment text files (policy etc.)"
  (interactive "sRegexp: \nsGrep opt (no single quotes): ")
  (let ((path-list (mapconcat
                    'concat
                    (delq nil
                          tinydebian--grep-find-devel-docdir-list)
                    " "))
        cmd)
    (setq cmd
          (format
           (concat
            "find %s -type f -name '*.txt.gz' -print0 "
            "| xargs -0 -e zgrep -n %s '%s'")
           path-list
           grep-opt
           regexp))
    (grep-find cmd)))

;;}}}
;;{{{ WNPP URLs

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-wnpp-compose (page-type)
  "Return URL to search"
  (let ((page (assoc page-type tinydebian--debian-url-http-wnpp-page-alist)))
    (unless page
      (error "TinyDebian: unknow page-typpe `%s'" page-type))
    (format "%s/%s" tinydebian--debian-url-http-wnpp-page-main (cdr page))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-usertag-compose (tag)
  "Return URL to search"
  (format "%s/usertag:%s" tinydebian--debian-url-http-www tag))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-url-wnpp-browse-url (page-type)
  "Browse WNPP PAGE-TYPE."
  (tinydebian-browse-url-1 (tinydebian-url-wnpp-compose page-type)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-bugs-by-usertag (usertag)
  "Browse by USERTAG."
  (interactive "sUsertag to search: ")
  (tinydebian-string-p
   usertag
   (format "[ERROR] usertag is missing from input [%s]" usertag))
  (tinydebian-browse-url-1 (tinydebian-url-usertag-compose usertag)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-itp ()
  "Browse WNPP ITP page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "ITP"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfp ()
  "Browse WNPP RFP page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFP"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfh ()
  "Browse WNPP RFH page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFH"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-rfa ()
  "Browse WNPP RFA page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "RFA"))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-url-list-wnpp-orphaned ()
  "Browse WNPP orphaned page."
  (interactive)
  (tinydebian-url-wnpp-browse-url "O"))

;;}}}
;;{{{ BTS functions: Debian Developer interface to bug tracking system

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-package-virtual-p (name)
  "Check if package NAME is in `tinydebian--debian-virtual-package-list'."
  (and tinydebian--debian-virtual-package-list
       (assoc name tinydebian--debian-virtual-package-list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydebian-package-info-by-key (key info)
  "Study debian/control INFO and return KEY 'Depends'."
  (cdr-safe (and info
                 (assoc key info))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-insert-headers ()
  "Insert tinydebian--bts-extra-headers' to mail buffer."
  (let ((headers tinydebian--bts-extra-headers))
    (when (stringp headers)
      (save-excursion
        (goto-char (point-min))
        (when (search-forward mail-header-separator nil t)
          (forward-line 0)
          (insert headers))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-save-window-excursion-if-send 'edebug-form-spec '(body))
(put 'tinydebian-bts-save-window-excursion-if-send 'lisp-indent-function 1)
(defmacro tinydebian-bts-save-window-excursion-if-send (send-it &rest body)
  "Run BODY withing `save-excusion' if variable `send-it' is non-nil."
  `(cond
    (,send-it
     (save-window-excursion
       ,@body))
    (t
     ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-mail-compose-macro 'edebug-form-spec '(body))
(put 'tinydebian-bts-mail-compose-macro 'lisp-indent-function 5)
(defmacro tinydebian-bts-mail-compose-macro
  (bug type package subject email &rest body)
  "Compose mail with SUBJECT and run BODY."
  (let ((name (gensym "name-"))
        (buffer (gensym "buffer-")))
    `(let ((,buffer (current-buffer))
           (,name (format "*Mail BTS %s*"
                          (cond
                           ((and ,bug ,type ,package)
                            (format "%s %s %s"
                                    ,type ,package ,bug))
                           ((and ,bug ,package)
                            (format "%s %s"
                                    ,package ,bug))
                           (t
                            (or ,bug
                                ,subject
                                ""))))))
       (pop-to-buffer (get-buffer-create ,name))
       (erase-buffer)
       (mail-setup
        (if ,email
            ,email
          (tinydebian-bts-generic-email-control ,buffer))
        ,subject
        nil
        nil
        nil
        nil)
       (cond
        ((or (featurep 'message)
             (eq mail-user-agent 'message-user-agent))
         (message-mode))
        (t
         (mail-mode)))
       (turn-on-tinydebian-mail-mode)
       (tinydebian-bts-insert-headers)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinydebian-bts-mail-type-macro 'edebug-form-spec '(body))
(put 'tinydebian-bts-mail-type-macro 'lisp-indent-function 4)
(defmacro tinydebian-bts-mail-type-macro (type pkg email subject &rest body)
  "Compose a TYPE request and run BODY.

Variables bound during macro (can all be nil):

  bugnbr
  type-orig
  package        Value of PKG is sent as an argument to macro
  description"
  (let ((subj (gensym "subject-")))
    `(multiple-value-bind (bugnbr type-orig package description)
         (or (tinydebian-bts-parse-string-current-line)
             (tinydebian-bts-parse-string-subject))
       (if (stringp ,pkg) ;; Use input argument
           (setq package ,pkg))
       (let ((,subj (or ,subject
                        (if ,type
                            (format "%s: %s%s"
                                    ,type
                                    (if package
                                        (format "%s -- " package)
                                      "")
                                    (or description ""))
                          ""))))
         (tinydebian-bts-mail-compose-macro
             bugnbr
             ,type
             package
             ,subj
             ,email
           (goto-char (point-max))
           ,@body)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-ita (bug)
  "Send an ITA request.
Optionally from BTS which defaults to \"debian\"."
  (interactive (list (tinydebian-bts-mail-ask-bug-number "ITA")))
  (tinydebian-bts-mail-type-macro
      "ITA"
      (not 'pkg)
      (not 'email)
      (not 'subject)
    (insert
     (format "\
retitle %s %s
owner %s !
thanks
"
             bug
        (let (fsub
              fpkg)
          (unless package
            (tinydebian-debian-bug-info-macro bug bts
              (setq package (field "package"))))
          (unless description
            (tinydebian-debian-bug-info-macro bug bts
              (let ((str (field "subject")))
                (setq description
                      (replace-regexp-in-string
                       "^.*-- *\\|O: *"
                       ""
                       str))
                ;; ITA: package -- Description
                (when (string-match "wnpp" package)
                  (if (string-match ": *\\([^ \t\r\n]+\\) *--" str)
                      (setq package (match-string 1 str)))))))
          (let ((string
                 (format "ITA: %s -- %s"
                         (or package "")
                         (or description ""))))
            string))
        bug))
    (when (re-search-backward "ITA:" nil t)
      (let ((string (buffer-substring (point) (line-end-position))))
        (tinydebian-mail-header-subject-set string)))
    (goto-char (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-it-nmu (bug)
  "Send an ITN (Intent to NMU) request.
Optionally from BTS which defaults to \"debian\".

In order to do a 'Non Maintainer Upload', it is polite to announce
the intent to the BTS and wait a while to see if the maintainer is
reponsive or working on the package.

See documents:
  http://dep.debian.net/deps/dep1.html (Official)
  http://www.debian.org/doc/developers-reference/pkgs.html#nmu (Official)
  http://wiki.debian.org/NmuDep (Complementary)"
  (interactive (list (tinydebian-bts-mail-ask-bug-number "ITN")))
  (tinydebian-debian-bug-info-macro bug bts
    (let ((str (field "subject")))
      ;;  package: <message string>
      (if (string-match "^[a-z][^:]+: *\\(.+\\)" str)
          (setq str (match-string 1 str)))
      (tinydebian-bts-mail-type-macro
          nil                           ;type
          nil                           ;pkg
          (tinydebian-bts-email-compose bug)
          ;;   Standard Debian BTS message format is:
          ;;   'Bug#NNNN: <action> (<description>)'
          (format "Bug#%s: Intent to NMU (%s: %s)"
                  bug
                  (field "package")
                  str)
        (if (stringp tinydebian--bts-mail-type-it-nmu-message)
            (insert tinydebian--bts-mail-type-it-nmu-message))
        (run-hooks 'tinydebian-bts-mail-type-it-nmu-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-itp (bug)
  "Repond to a RFP with an ITP request."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number "ITP response to RFP")))
  (tinydebian-bts-mail-type-macro
      "ITP" nil nil nil
    (insert
     (format "\
retitle %s %s
owner %s !
thanks
"
             bug
             (concat "ITP: "
                     (if package
                         (format "%s -- " package)
                       "")
                     (or description ""))
             bug))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-reply (bug &optional bts)
  "Reply to bug found at current point or line
Optionally from BTS which defaults to \"debian\"."
  (interactive (list (tinydebian-bts-mail-ask-bug-number "Reply to bug")))
  (let ((subject) ;; (my-tinydebian-subject-any))
        tinydebian--bts-compose-type  ;; FIXME: should be use this instead of 'bts'
	bts)
    (if (tinydebian-emacs-bug-type-p)
	(setq bts "emacs"))

    ;; (unless (and (stringp bug)
    ;; 		 (stringp subject)
    ;;              (string-match (regexp-quote bug) subject))
    ;;   ;; User gave different bug number. Read subject from BTS.
    ;;   (setq subject
    ;; 	    (tinydebian-bts-generic-bug-subject bug bts)))
    (setq subject
	  (tinydebian-bts-generic-bug-subject bug bts))
    (tinydebian-bts-mail-compose-macro
     bug
     "reply"
     "bug"
     subject
     (tinydebian-bts-email-compose bug nil bts)
     (mail-position-on-field "CC")
     (insert (tinydebian-bts-email-compose (format "%s-submitter" bug)))
     (goto-char (point-max))
     nil)))

;;; ----------------------------------------------------------------------
;;; FIXME: #TODO
(defun tinydebian-bts-mail-type-orphan (package desc)
  "Send an orphan request to PACKAGE with DESC."
  (interactive
   (let ((pkg (tinydebian-bts-mail-ask-package "Orphan package: ")))
     (list pkg (tinydebian-package-status-description-1 pkg))))
  (let ((subj-message
         (format "O: %s -- %s"
                 package
                 (or desc
                     (if tinydebian--novice-mode
                         "<description>")
                     ""))))
  (tinydebian-bts-mail-type-macro
      "O" nil (tinydebian-bts-email-submit) subj-message
    (insert
     (format "\
Package: wnpp
Severity: normal

"))
      (let ((point (point)))
        (if tinydebian--novice-mode
            (insert "<describe orphan detail>\n"))
        (goto-char point)))))

;;; ----------------------------------------------------------------------
;;; FIXME: will be removed. Obsolete function.
(defun tinydebian-bts-mail-type-remove (package type &optional desc)
  "Send a remove request.
PACKAGE   package name
TYPE      'RM' = remove, 'RoM' = request of maintainer
DESC      apt-cache show <package>; first line description."
  (interactive
   (let ((pkg (tinydebian-bts-mail-ask-package "RM package: "))
         (type
          (if (y-or-n-p "Are you the maintainer? ")
              "RoM"
            "RM")))
     (list pkg type (tinydebian-package-status-description-1 pkg))))
  (let ((subj-message
         (format "%s: %s -- %s"
                 type
                 package
                 (or desc
                     (if tinydebian--novice-mode
                         "<description>")
                     ""))))
    (tinydebian-bts-mail-type-macro
        type nil (tinydebian-bts-email-submit) subj-message
      (insert
       (format "\
Package: ftp.debian.org
Severity: wishlist

"))
      (let ((point (point)))
        (if tinydebian--novice-mode
            (insert "<describe reason for removal>\n"))
        (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-pkg-read-details-directory (directory);; FIXME: Not implemented
  "Assuming a simgle debian package is in DIRECTORY, extract details.
  The directory should contain files:
  -rw-r--r-- 1 jaalto jaalto  19885 2006-11-19 18:12 pkg_0.2.4-4.diff.gz
  -rw-r--r-- 1 jaalto jaalto    605 2006-11-19 18:12 pkg_0.2.4-4.dsc
  -rw-r--r-- 1 jaalto jaalto   1106 2006-11-19 18:12 pkg_0.2.4-4_i386.changes
  -rw-r--r-- 1 jaalto jaalto 122188 2006-11-19 18:12 pkg_0.2.4-4_i386.deb
  -rw-r--r-- 1 jaalto jaalto    339 2006-11-19 18:12 pkg_0.2.4-4_i386.upload
  -rw-r--r-- 1 jaalto jaalto    942 2006-11-19 18:12 pkg_0.2.4-4_source.changes
  -rw-r--r-- 1 jaalto jaalto 246864 2006-11-19 18:12 pkg_0.2.4.orig.tar.gz

  RETURN:
  ((pkg-name       . \"pkg\")
   (pkg-ver-major  . \"0.2.4\")
   (pkg-ver-minor  . \"4\")
   (dsc            . \"pkg_0.2.4-4.dsc\")
   (deb            . \"pkg_0.2.4-4.dsc\")
   "
  (let ()))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-rfs (package license bug desc)
  "Send an RFS request: PACKAGE name, package LICENse and BUG and DESC.
   The DESC is short one line description string use in Subject."
  (interactive
   (let* ((name    (read-string
                    "RFP package name [required; lowercase]: ")) ;
          (license (tinydebian-read-license "License [required]: "))
          (bug      (read-string
                     "ITA/ITP bug number [required]: "))
          (desc    (read-string
                    "One line description [required]: ")))
     (list name license bug desc)))
  (flet ((replace (regexp str &optional point all)
                  (when (and (stringp str)
                             (not (string= "" str)))
                    (goto-char (or point
                                   (point-min)))
                    (if all
                        (while (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))
                      (if (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))))))
    (let* ((arg-pkg package) ;; Due to macro which reserves var `package'.
           (mentors-url (tinydebian-url-debian-mentors-url package))
           (ita-url     (tinydebian-debian-bts-url-compose bug))
           (pkg-url     (tinydebian-debian-bts-url-compose package)))
      (tinydebian-bts-mail-type-macro "RFS"
          arg-pkg (tinydebian-list-email-compose "debian-mentors") nil
        (insert tinydebian--rfs-template)
        (replace "\\(<package>.*\\)"    package nil 'all)
        (replace "\\(<bugs:.*\\)"       pkg-url)
        (replace "\\(<ita:.*\\)"        ita-url)
        (replace "\\(<mentors:.*\\)"    mentors-url)
        (replace "\\(<license:.*\\)"    license)
        (mail-position-on-field "Subject")
        (beginning-of-line)
        (replace ": \\(.*\\)"
                 (format "RFS: %s -- %s" package desc)
                 (point))
        (goto-char (point-max))
        (run-hooks 'tinydebian--rfs-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-type-rfp
  (package license homepage desc &optional itp)
  "Send an RFP request.
If optional `current-prefix-arg' is set, label post as an ITP.
The default is to use RFP in Subject header."
  (interactive
   (let* ((name    (read-string
                    (format
                     "%s package name [required; lowercase]: "
                     (if current-prefix-arg
                         "ITP"
                       "RFP"))))
          (desc    (read-string
                    "Package description [required]: "))
          (license (completing-read
                    "License [required]: "
                    (mapcar (lambda (x)
                              (cons x 1))
                            tinydebian--wnpp-template-licenses-alist)))
          (url     (read-string
                    "Project homepage URL [required]: ")))
     (list name
           license
           url
           desc
           (if current-prefix-arg
               'itp))))
  (flet ((replace (regexp str &optional point all)
                  (when (and (stringp str)
                             (not (string= "" str)))
                    (goto-char (or point
                                   (point-min)))
                    (if all
                        (while (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))
                      (if (re-search-forward regexp nil t)
                          (replace-match str 'literal nil nil 1))))))
    (let ((arg-pkg package)) ;; Due to macro which reserves var `package'.
      (tinydebian-bts-mail-type-macro "ITP"
	  arg-pkg (tinydebian-bts-email-submit) nil
	(insert tinydebian--rfp-template)
	(replace "\\(<package>.*\\)"    package nil 'all)
	(replace "\\(<homepage:.*\\)"   homepage)
	(replace "\\(<license:.*\\)"    license)
	(replace "\\(<short desc>.*\\)" desc)
	(mail-position-on-field "Subject")
	(beginning-of-line)
	(replace ": \\(.*\\)"
		 (format "%s: %s -- %s"
			 (if itp
			     "ITP"
			   "RFP")
			 package desc)
		 (point))
	(goto-char (point-max))
	(run-hooks 'tinydebian--rfp-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-severity (bug severity)
  "Compose BTS control message to a BUG and change SEVERITY."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (completing-read
          "BTS severity: "
          tinydebian--severity-list
          nil
          'match)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s Change of severity / %s" bug severity)
   (insert
    (format "\
severity %s %s
thanks
"
            bug
            severity))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-usertag (bug &optional email tag-string)
  "Compose BTS control message usertag to a BUG with TAG-STRING.
Message format:
    user user@domain.tld
    usertags <#bug> [+-] <tagname>
    usertags ...

References:
    http://wiki.debian.org/bugs.debian.org/usertags"
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (completing-read
     "Usertag user (email): "
     tinydebian--usertag-email-list
     nil
     nil
     user-mail-address)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s change of usertag%s"
           bug
           (if tag-string
               (format " %s" tag-string)
             ""))
   (insert
    (format "\
%susertag %s +
thanks
"
            (if email
                (format "user %s\n" email))
            bug))
   (when (re-search-backward "[+]" nil t)
     (forward-char 1)
     (skip-chars-forward " "))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-ctrl-list-ask (&optional question)
  "Ask list of string items interactively with QUESTION."
  (let (item
        list)
    (while (or (null item)
               (not (string= "" item)))
      (setq item (read-string
                  (concat
                   (or question "Item")
                   " [RET when done]: ")))
      (unless (string= "" item)
        (push item list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-ctrl-tags-ask (&optional message)
  "Ask list of tags interactively with optional MESSAGE."
  (let (tag
        list)
    (while (or (null tag)
               (not (string= "" tag)))
      (setq tag (completing-read
                 "BTS tag [RET when done]: "
                 tinydebian--tags-list
                 nil
                 'match))
      (unless (string= "" tag)
        (push tag list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-tags (bug tag-list)
  "Compose BTS control message to a BUG with TAG-LIST."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (tinydebian-bts-ctrl-tags-ask)))
  (let ((tag-string
         (mapconcat 'concat tag-list " ")))
    (tinydebian-bts-mail-type-macro
        nil nil nil
        (format "Bug#%s change of tags / %s" bug tag-string)
      (insert
       (format "\
tags %s + %s
thanks
"
               bug
               tag-string))
      (when (re-search-backward "[+]" nil t)
        (forward-char 2)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-reassign (bug &optional package-to)
  "Compose BTS control message to a BUG amd reassign PACKAGE."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         (read-string "Reassign to package: ")))
  (tinydebian-bts-mail-type-macro
   nil                                  ;Type
   package-to
   nil                                  ;Email
   (format "Bug#%s reassign%s" bug (if package-to
                                       (format " to package %s"
                                               package-to)
                                     ""))
   (insert
    (format "\
reassign %s %s
thanks
"
            bug
            (if (and package-to
                     (not (string= "" package-to)))
                package-to
              "<to-package>")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-reopen (bug)
  "Compose BTS control message to reopen BUG."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s reopen" bug)
   (let (point)
     (insert (format "reopen %s\n" bug))
     (insert (format "owner %s !\n" bug))
     (setq point (point))
     (insert (format "# [For Closed bug] retitle %s IT[AP]: pkg -- description"
                     bug))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;; see "reportbug ftp.debian.org"
(defun tinydebian-bts-mail-ctrl-remove-package-ask-kwd ()
  "Ask removal keywords."
  (let (tag
        list)
    (while (or (null tag)
               (not (string= "" tag)))
      (setq tag (completing-read
                 "Kwd [RET when done]: "
                 tinydebian--removal-keyword-list
                 nil
                 'match))
      (unless (string= "" tag)
        (push tag list)))
    list))

;;; ----------------------------------------------------------------------
;;; see "reportbug ftp.debian.org"
(defun tinydebian-bts-mail-ctrl-remove-package-ask ()
  "Interactive part of `tinydebian-bts-mail-ctrl-remove-package'.
For removal request types, see
  http://ftp-master.debian.org/removals.txt
"
  (let ((line (tinydebian-current-line-string)) ;; Gnus summary
        package
        suite
        message
        type)
    (multiple-value-bind (bugnbr type-orig pkg description)
        (or (tinydebian-bts-parse-string-current-line)
            (tinydebian-bts-parse-string-subject))
      (unless package
        (let ((tmp (car-safe
                    (tinydebian-bts-parse-string-with-package line))))
          (if tmp
              (setq pkg tmp))))
      (setq type (completing-read
                  "Package remove request type: "
                  '(("RM" . 1)
                    ("ROM" . 2)
                    ("ROSRM" . 3)
                    ("RoQA" . 4)
                    ("ANAIS" . 5))
                  nil
                  t
                  nil))
      (setq package (read-string "Package name: " pkg))
      (setq suite (completing-read
                   "Suite: "
                   '(("experimental" . 1)
                     ("oldstable" . 2)
                     ("oldstable-proposed-updates" . 3)
                     ("stable" . 4)
                     ("stable-proposed-updates" . 5)
                     ("testing" . 6)
                     ("testing-proposed-updates" . 7)
                     ("unstable" . 8))
                   nil
                   t
                   "unstable"))
      (let ((list (tinydebian-bts-mail-ctrl-remove-package-ask-kwd)))
        (if list
            (setq message (mapconcat 'concat list "; "))))
      (list type package message suite))))

;;; ----------------------------------------------------------------------
;;; see "reportbug ftp.debian.org"
;;;
;;;  1 ANAIS   Package removal - Architecture Not Allowed In Source.
;;;  2 ICE     Package removal - Internal Compiler Error.
;;;  3 NBS     Package removal - Not Built [by] Source.
;;;  4 NPOASR  Package removal - Never Part Of A Stable Release.
;;;  5 NVIU    Package removal - Newer Version In Unstable.
;;;  6 ROM     Package removal - Request Of Maintainer.
;;;  7 ROP     Package removal - Request of Porter.
;;;  8 ROSRM   Package removal - Request of Stable Release Manager.
;;;  9 RoQA    Package removal - Requested by the QA team.
;;; 10 other   Not a package removal request, report other problems.
;;;
(defun tinydebian-bts-mail-ctrl-remove-package
  (type package message &optional suite)
  "Compose TYPE of (RM, ROM) to remove PACKAGE with MESSAGE.
MESSAGE is a short explanation that appears in Subject field.

Optional SUITE is by default 'unstable'.

TYPE is one of the uppercase strings:
RM      Package removal - Generic request
ROM     Package removal - Request Of Maintainer
ROP     Package removal - Request of Porter
ROSRM   Package removal - Request of Stable Release Manager
RoQA    Package removal - Requested by the QA team

See http://wiki.debian.org/ftpmaster_Removals"
  (interactive
   (tinydebian-bts-mail-ctrl-remove-package-ask))
  (unless (string-match "^r" type)
    (error "Unknown request type `%s'" type))
  (unless (string-match "^[a-z]" package)
    (error "Invalid package name `%s'" type))
  (when (or (not (stringp package))
            (not (string-match "^[a-z]" package)))
    (setq suite "unstable"))
  ;; Subject: RM: PKG/testing -- ROM; <reason>
  (or message
      (setq message ""))
  (let* ((testing-p
          (string-match "testing" suite))
         (email
          (if (string-match "testing" suite)
              "debian-release@lists.debian.org"
            (tinydebian-bts-email-compose "submit")))
         (subject
          (format "RM: %s%s -- %s; %s"
                  (downcase package)
                  (if (string-match "unstable" suite)
                      ""
                    (format "/%s" (downcase suite)))
                  (upcase type)
                  message)))
    (tinydebian-bts-mail-compose-macro
     nil
     nil
     (if testing-p
         package
       ;; Anything else should be reported to this pseudo package
       "ftp.debian.org")
     subject
     email
     (let (point)
       (unless testing-p
         (insert "Package: ftp.debian.org\n"
                 "Severity: normal\n\n"))
       (setq point (point))
       (if tinydebian--novice-mode
           (insert "<Describe reasons to remove package>"))
       (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-title-read ()
  "Return old TITLE from `gnus-article-buffer' buffer"
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (tinydebian-with-gnus-article-buffer nil
     (message-fetch-field "Subject")))
   ((memq major-mode '(message-mode mail-mode))
    (message-fetch-field "Subject"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-retitle (bug title &optional bts)
  "Compose BTS control message to a BUG and change TITLE.
Optionally from BTS which defaults to \"debian\"."
  (interactive
   (let ((title (tinydebian-bts-mail-title-read))
         (bug   (tinydebian-bts-mail-ask-bug-number)))
     (unless (string-match bug title)
       (tinydebian-debian-bug-info-macro bug bts
         (setq title (field "subject"))))
     (list
      bug
      (read-string "New title: " title))))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s retitle" bug)
   (let (point)
     (insert (format "retitle %s " bug))
     (setq point (point))
     (insert (format "%s\nthanks\n" title))
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-fixed (bug)
  "Compose BTS control message to mark BUG fixed."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s fixed" bug)
   (let (point)
     (insert (format "fixed %s " bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "<version>"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-notfixed (bug version)
  "Compose BTS control message to mark BUG notfixed in VERSION."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (read-string "Version where bug was *not* fixed: ")))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s status change - notfixed in version %s" bug version)
   (let (point)
     (insert (format "notfixed %s %s" bug version))
     (setq point (point))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-owner (bug &optional email)
  "Compose BTS control message to mark BUG owner EMAIL.
Default owner is the value of 'From:', that is `user-mail-address'."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s status change - owner" bug)
   (let (point)
     (insert (format "owner %s %s" bug (or email "!")))
     (setq point (point))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-noowner (bug)
  "Compose BTS control message to mark BUG noowner."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s status change - noowner" bug)
   (let (point)
     (insert (format "noowner %s" bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "<explain reason, remember also to 'retitle' as needed>"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-found (bug version)
  "Compose BTS control message to mark BUG found in VERSION."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (read-string "Package version where bug was found: ")))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s found in version %s" bug version)
   (let (point)
     (insert (format "found %s %s" bug version))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "\n# Records that #bug has been encountered"
                 "in the given version of the package"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-notfound (bug version)
  "Compose BTS control message to mark BUG notfound in VERSION."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (read-string "Package version where bug was *not* found: ")))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s status change - notfound in version %s" bug version)
   (let (point)
     (insert (format "notfound %s %s" bug version))
     (setq point (point))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-notforwarded (bug)
  "Compose BTS control message to mark BUG notforwarded."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
      nil nil nil
      (format "Bug#%s status change - notforwarded" bug)
   (let (point)
     (insert (format "notforwarded %s" bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "\n# Reason: The URL bug was forwarded does not exist"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-merge (bug)
  "Compose BTS control message a BUG and merge a duplicate."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s merge" bug)
   (let (point)
     (insert (format "merge %s " bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "<bug nbr that is duplicate of this bug>"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-clone-new-mail (bug)
  "Compose BTS control message against BUG for cloning."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s clone" bug)
   (let (point)
     (insert (format "clone %s -1" bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert " <To copy to new package: reassign -1 package>"))
     (insert "\nretitle -1")
     (if tinydebian--novice-mode
         (insert " <New bug's title>"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-affects (bug)
  "Compose BTS control message a BUG for affect."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s Affect control" bug)
   (let (point)
     (insert (format "affect %s " bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "<space separated list of packages affected by this bug>"))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-bug-package-unsubscribe
  (package-name &optional email send)
  "Compose PTS control message using PACKAGE for unsubscription.

Cancel receiving bug report notifications for PACKAGE.

Input:
    PACKAHE     Package name
    EMAIL       Optional: email address, defaults to `user-mail-address'
    SEND        Optional: if non-nil, send message. In interactive call
                this is `current-prefix-arg' and it causes bypassing question
                about EMAIL too.
Return:
   email buffer if SEND is nil."
  (interactive
   (list
    (read-string "Unsubscribe to bugs in package: ") ;; FIXME smart default value
    (if current-prefix-arg
        user-mail-address
      (read-string "Unsubscribe email: " user-mail-address))
    current-prefix-arg))
  (tinydebian-bts-save-window-excursion-if-send send
    (tinydebian-bts-mail-type-macro
        (not 'type)
        (not 'pkg)
        "pts@qa.debian.org"
        (format "%s: unsubscribe to all Debian bugs" package-name)
      (let (point)
        (insert
         (format "unsubscribe %s %s"
                 package-name
                 (or email user-mail-address)))
        (unless send
          (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-bug-package-subscribe
  (package-name &optional email send)
  "Compose PTS control message using PACKAGE for subscription.

Subscribing to a package enables receiving all bug reports. This is
useful for NMUers that upload fixes and want to follow if NMU causes
further reports.

Input:
    PACKAHE     Package name
    EMAIL       Optional: email address, defaults to `user-mail-address'
    SEND        Optional: if non-nil, send message. In interactive call
                this is `current-prefix-arg' and it causes bypassing question
                about EMAIL too.
Return:
   email buffer if SEND is nil."
  (interactive
   (list
    (read-string "Subscribe to bugs in package: ") ;; FIXME smart default value
    (if current-prefix-arg
        user-mail-address
      (read-string "Subscribe email: " user-mail-address))
    current-prefix-arg))
  (tinydebian-bts-save-window-excursion-if-send send
    (tinydebian-bts-mail-type-macro
        (not 'type)
        (not 'pkg)
        "pts@qa.debian.org"
        (format "%s: subscribe to all Debian bugs" package-name)
      (let (point)
        (insert
         (format "subscribe %s %s"
                 package-name
                 (or email user-mail-address)))
        (unless send
          (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-bug-subscribe (bug &optional email send)
  "Compose BTS control message using BUG for subscription.

Input:
    BUG         Bug number
    EMAIL       Optional: email address, defaults to `user-mail-address'
    SEND        Optional: if non-nil, send message. In interactive call
                this is `current-prefix-arg' and it causes bypassing question
                about EMAIL too.
Return:
   email buffer if SEND is nil."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (if current-prefix-arg
        user-mail-address
      (read-string "Unsubscribe email: " user-mail-address))
    current-prefix-arg))
  (tinydebian-bts-save-window-excursion-if-send send
    (tinydebian-bts-mail-type-macro
        (not 'type)
        (not 'pkg)
        (tinydebian-bts-email-compose
         (format "%s-subscribe-%s"
                 bug
                 (replace-regexp-in-string "@" "=" email)))
        (format "Bug#%s subscribe" bug)
      (let (point)
        (insert
         "# To use different address, change the \"To\" header\n"
         "# nnn-subscribe-localpart=example.com@bugs.debian.org\n")
        (insert "thanks\n")
        (unless send
          (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-bug-unsubscribe (bug &optional email send)
  "Compose BTS control message using BUG for unsubscription.

Input:
    BUG         Bug number
    EMAIL       Optional: email address, defaults to `user-mail-address'
    SEND        Optional: if non-nil, send message. In interactive call
                this is `current-prefix-arg' and it causes bypassing question
                about EMAIL too.

Return:
   email buffer if SEND is nil."
  (interactive
   (list
    (tinydebian-bts-mail-ask-bug-number)
    (if current-prefix-arg
        user-mail-address
      (read-string "Unsubscribe email: " user-mail-address))
    current-prefix-arg))
  (tinydebian-bts-save-window-excursion-if-send send
    (tinydebian-bts-mail-type-macro
        (not 'type)
        (not 'pkg)
        (tinydebian-bts-email-compose
         (format "%s-unsubscribe-%s"
                 bug
                 (replace-regexp-in-string "@" "=" email)))
        (format "Bug#%s unsubscribe" bug)
      (let (point)
        (insert
         "# To use different address, change the \"To\" header\n"
         "# nnn-unsubscribe-localpart=example.com@bugs.debian.org\n")
        (insert "thanks\n")
        (unless send
          (current-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-close (bug &optional package version bts)
  "Compose BTS control message to close BUG.
Optional PACAGE name and VERSION number can be supplied."
  (interactive
   (multiple-value-bind (bts bug project)
       (tinydebian-bug-ask-bts-and-number)
     (let (;; (bug      (tinydebian-bts-mail-ask-bug-number))
           ;;  (package  (read-string "Package name [RET=ignore]: "))
           version)
       (when (tinydebian-bts-type-debbugs-p bts)
         (if (tinydebian-string-p project)
             (setq version (read-string "Version: "))
           (setq project nil)))
     (list bug
           project
           (if (tinydebian-string-p version)
               version)
           bts))))
  (let ((email (tinydebian-bts-email-compose
                (format "%s-done" bug) bug bts))
         (pkg   package))
    (tinydebian-bts-mail-type-macro
     nil
     pkg
     email
     (format "Bug#%s Close%s"
             bug
             (if bts
                 (format " bts:%s" bts)
               ""))
     (insert
      (if (not (stringp package))
          ""
        (format "\
Package: %s
Version: %s
"
                package
                (or version "")))
      "\nReason for close:\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-upstream (bug)
  "Compose BTS control message: forward BUG report to upstream."
  (let ((email-forward (tinydebian-bts-email-compose
                        (format "%s-forwarded" bug)))
        (email-bug (tinydebian-bts-email-compose bug)))
    (tinydebian-bts-mail-type-macro
     nil nil "<upstream address>"
     (format "Debian Bug#%s -- forwarded upstream" bug)
     (mail-position-on-field "Cc")
     (insert (format "%s, %s" email-forward email-bug))
     (goto-char (point-max))
     (insert
      (format "\
\[Please keep the CC]

")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-bts (bug)
  "Compose BTS forwarded control message to BTS."
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s -- forwarded upstream" bug)
   (let (point)
     (insert (format "forwarded %s " bug))
     (setq point (point))
     (if tinydebian--novice-mode
         (insert "<http://upstream.example.com/bug-tracking/nbr>"))
     (insert "thanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forwarded-main (bug &optional control-message)
  "Compose BTS control message: forwarded BUG ADDRESS."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         current-prefix-arg))
  (tinydebian-bts-mail-type-macro
   nil nil nil
   (format "Bug#%s%s forwarded to upstream"
           bug
           (if package
               (format " to package %s"
                       package)
             ""))
   (let (point)
     (insert (format "forwarded %s " bug))
     (setq point (point))
     (if (and package
              (not (string= "" package)))
         (insert package)
       (if tinydebian--novice-mode
           (insert "<add upstream Bug tracker submission URL here>")))
     (insert "\nthanks\n")
     (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-ctrl-forward-main (bug &optional control-message)
  "Compose BTS control message: forward BUG report to upstream.
If optional CONTROL-MESSAGE is non-nil, then compose regular BTS control
message which can be used to record upstream's bug tracking system URL."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         current-prefix-arg))
  (if control-message
      (tinydebian-bts-mail-ctrl-forward-bts bug)
    (let (buffer)
      (when (eq major-mode 'gnus-summary-mode)
        (gnus-summary-select-article)
        (setq buffer "*Article*"))
      (tinydebian-bts-mail-ctrl-forward-upstream bug)
      (if buffer
          (setq message-reply-buffer (get-buffer buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bts-mail-message-info (bug &optional quiet)
  "Send more information to BUG, possibly with QUIET on.
With QUIET,  the  email will only be archived, sent to package maintainer
and not forwarded any Debian mailing lists."
  (interactive
   (list (tinydebian-bts-mail-ask-bug-number)
         current-prefix-arg))
  (let ((email (tinydebian-bts-email-compose
                (if quiet
                    (format "%s-maintonly" bug)
                  bug))))
    (tinydebian-bts-mail-type-macro
     nil nil email
     (format "Debian Bug#%s " bug))))

;;}}}
;;{{{ Dpkg, apt functions

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content-1 ()
  "Read content. Point must be positionioned at Field:-!-."
  (let ((str (if (looking-at " +\\(.*\\)")
                 (match-string-no-properties 1))))
    (while (and (not (eobp))
                (zerop (forward-line 1)) ;; Did it
                (looking-at "^\\( +.*\\)"))
      (setq str (concat (or str "") (match-string-no-properties 1))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-read-field-content (&optional field)
  "Read FIELD forward. FIELD ust be name like `Package'.
Be sure to call `tinydebian-package-narrow-to-region' first."
  (when (re-search-forward (format "^%s:" field) nil t)
    (tinydebian-package-read-field-content-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-parse-info-all ()
  "Parse all fields forward. Return '((field . info) (field . info) ..)."
  (let (name
        field
        alist)
    (while (re-search-forward "^\\([^ \t\r\n]+\\):" nil t)
      (setq name (match-string-no-properties 0))
      (setq field (match-string-no-properties 1))
      ;;  Treat "Description:" differently" and break it into two fields
      (when (string-match "description" name)
        (let (line)
          (when (looking-at " *\\([^ \t].+\\)")
            (setq line (match-string-no-properties 1))
            (push (cons (format "%s1" field) line) alist))
          (forward-line 1)))
      (push (cons field (tinydebian-package-read-field-content-1))
            alist))
    (nreverse alist)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info-from-buffer (buffer)
  "Parse dpkg -s from BUFFER. Buffer must contain nothing else."
  (with-current-buffer buffer
    (goto-char (point-min))
    (tinydebian-package-parse-info-all)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends-1 ()
  "Parse `Depends' field content from current point forward.
There must nothing else in the buffer."
  (let (name
        op
        ver
        list)
    (while (re-search-forward "\\([a-z][^ ,()\t\r\n]+\\)" nil t)
      (setq name (ti::remove-properties (match-string 1))
            op   nil
            ver  nil)
      (cond
       ((looking-at " +(\\([=><]+\\) +\\([^ ,()\t\r\n]+\\))")
        (setq op   (ti::remove-properties (match-string 1))
              ver  (ti::remove-properties (match-string 2))))
       ((looking-at " *,?")))
      (goto-char (match-end 0))
      (push (list name op ver) list))
    (nreverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-parse-depends (depends)
  "Parse `Depends' field from DEPENDS string.
Example of the DEPENDS string:

    \"libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)\"

Returned list is

   '((\"libc6\" \">=\" \"2.2.4-2\")
     (\"cron\"  \">=\" \"3.0pl1-42\"))."
  (with-temp-buffer
    (insert depends)
    (ti::pmin)
    (tinydebian-package-status-parse-depends-1)))

;;; ----------------------------------------------------------------------
;;;
;;; #todo:
(defun tinydebian-package-status-apt-file (package)
  "Use apt-file PACKAGE (must be installed separately) to find upstream."
  (let ((bin (executable-find "apt-file")))
    (cond
     ((null bin)
      (message
       "TinyDebian: no `apt-file' found along PATH (emacs `exec-path').")
      (message "TinyDebian: Please run 'apt-get install apt-file'")
      nil)
     (t
      nil))))

;;; ----------------------------------------------------------------------
;;;
;;; Package: autolog
;;; Status: install ok installed
;;; Priority: extra
;;; Section: admin
;;; Installed-Size: 45
;;; Maintainer: Nicolás Lichtmaier <nick@debian.org>
;;; Version: 0.35-10
;;; Depends: libc6 (>= 2.2.4-2), cron (>= 3.0pl1-42)
;;; Recommends: mail-transport-agent
;;; Conffiles:
;;;  /etc/autolog.conf a3fcae584ed74543a4a943e722593ff6
;;;  /etc/cron.d/autolog 805d268ea44c645299defc1c14495282
;;; Description: Terminates connections for idle users
;;;  Autolog terminates connections considered to be idle based on a large
;;;  variety of parameters.
;;;
(defun tinydebian-package-status-dpkg-s-main (package)
  "Consult dpkg -s PACKAGE"
  (let ((dpkg tinydebian--bin-dpkg))
    (cond
     ((not dpkg)
      (message "TinyDebian: no `dpkg' found along PATH (emacs `exec-path').")
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... dpkg -s %s" package)
        (tinydebian-call-process dpkg nil "-s" package)
        (ti::pmin)
        (when (re-search-forward "^Use dpkg" nil t)
          (message "TinyDebian: `dpkg`-s %s' returned error [%s]"
                   package
                   (buffer-string)))
        (tinydebian-package-parse-info-all))))))

;;; ----------------------------------------------------------------------
;;; dpkg -S dh_make
;;;
;;; debhelper: /usr/bin/dh_makeshlibs
;;; dh-make: /usr/share/debhelper/dh_make/debian/postrm.ex
;;; dh-make: /usr/share/debhelper/dh_make/native
;;; dh-make: /usr/share/debhelper/dh_make/debian/changelog
;;; dh-make: /usr/share/debhelper/dh_make/debianl/shlibs.local.ex
;;; dh-make: /usr/share/man/man1/dh_make.1.gz
;;; dh-make: /usr/bin/dh_make
;;; dh-make: /usr/share/debhelper/dh_make/debiank/README.Debian
;;; dh-make: /usr/share/debhelper/dh_make/debianm/control
;;; dh-make: /usr/share/debhelper/dh_make/debian/init.d.ex
;;; dh-make: /usr/share/debhelper/dh_make/debian/cron.d.ex
;;; dh-make: /usr/share/debhelper/dh_make/debianm/rules
;;; dh-make: /usr/share/debhelper/dh_make/licenses/lgpl
;;; dh-make: /usr/share/debhelper/dh_make/debiank/control
;;; dh-make: /usr/share/debhelper/dh_make/debians/rules
;;; dh-make: /usr/share/debhelper/dh_make/debianl/package1.dirs
;;; dh-make: /usr/share/debhelper/dh_make/native/changelog
;;; dh-make: /usr/share/debhelper/dh_make/licenses/bsd
;;; dh-make: /usr/share/debhelper/dh_make/debianm/package-doc.files
;;; dh-make: /usr/share/debhelper/dh_make/debians/watch.ex
;;; dh-make: /usr/share/debhelper/dh_make/licenses/gpl
;;; dh-make: /usr/share/debhelper/dh_make/licenses/blank
;;;
(defun tinydebian-package-status-dpkg-S-parse (package)
  "Examine dpkg -S PACKAGE listing and return package name."
  (ti::pmin)
  (when (re-search-forward (concat "^\\([^: \t\r\n]+\\):.*/"
                                   package
                                   "[ \t]*$")
                           nil t)
    (match-string 1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-dpkg-S-main (file)
  "Consult dpkg -S FILE
In this case, the package is unknown."
  (let ((dpkg  tinydebian--bin-dpkg))
    (cond
     ((not dpkg)
      (message "TinyDebian: no `dpkg' found along PATH (emacs `exec-path').")
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... dpkg -S %s (takes a while)" file)
        (apply 'tinydebian-call-process dpkg nil (list "-S" file))
        (let ((pkg (tinydebian-package-status-dpkg-S-parse file)))
          (cond
           ((null pkg)
            (message
             "TinyDebian: dpkg -S doesn't know file `%s'" file)
            nil)
           (t
            (tinydebian-package-status-dpkg-s-main pkg)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-apt-cache (package)
  "Consult apt-cache shoiw PACKAGE
In this case, the package is unknown."
  (let ((bin "/usr/bin/apt-cache"))
    (when (and (file-exists-p bin)
               (stringp package))
      (with-temp-buffer
        (message "TinyDebian: Running ... apt-cache show %s (takes a while)"
                 package)
        (apply 'tinydebian-call-process bin nil (list "show" package))
        (message "Done.")
        (unless (eq (point-max) (point-min))
          (goto-char (point-min))
          (tinydebian-package-parse-info-all))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-description-1 (package)
  "Return the first line description for PACKAGE."
  (let* ((info (tinydebian-package-status-apt-cache package))
         (desc (cdr-safe (assoc "Description1" info))))
    desc))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-grep-available (package)
  "Consult grep-available(1) for PACKAGE from 'Provides' field."
  (let ((bin tinydebian--bin-grep-available)
        (re  (format ".*[ ,]+%s([, \t]|[ \t]*$)" package)))
    (cond
     ((not bin)
      (message (concat "TinyDebian: no `grep-available' "
                       "found along PATH (emacs `exec-path')."))
      nil)
     (t
      (with-temp-buffer
        (message "TinyDebian: Running ... grep-available -e %s" package)
        (apply 'tinydebian-call-process
               bin
               nil
               (list "--field=Provides"
                     "--eregex"
                     re))
        (let ((info (tinydebian-package-info-from-buffer
                     (current-buffer))))
          (cond
           ((null info)
            (message
             "TinyDebian: grep-available doesn't know package`%s'" package)
            nil)
           (t
            info))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-wnpp-main-interactive ()
  "Ask the type of request for WNPP package.
References:
  `tinydebian--menu-wnpp'
  `tinydebian--menu-wnpp-selected'"
  (setq tinydebian--menu-wnpp-selected nil)
  (ti::menu-menu 'tinydebian--menu-wnpp)
  tinydebian--menu-wnpp-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-buffer-ask-input (message buffer &optional clear)
  "Write MESSAGE to the buffer ans ask user to type input.
The MESSAGE should contgain properly formatted text."
  (let ((buffer (ti::temp-buffer buffer clear)))))
    ;; (switch-to-buffer buffer)
    ;; #todo:

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-wnpp-main (request-type)
  "Submit REQUEST-TYPE against WNPP pseudo package.
WNPP is used for requesting to be a new Debian maintainer and
for taking maintenance of other packages. Refer to
http://www.debian.org/devel/wnpp and
http://www.debian.org/doc/packaging-manuals/developers-reference/ch-pkgs.en.html
and topic \"5.1 New Packages\"

REQUEST-TYPE can be symbol:

  'package 'orphan 'adopt or 'new.
  See http://www.debian.org/devel/wnpp for more information

References:

  `tinydebian--menu-wnpp'."
  (interactive (list (tinydebian-package-wnpp-main-interactive)))
  (cond
   ((eq request-type 'package)
    (call-interactively 'tinydebian-bts-mail-type-itp))
   ((eq request-type 'new)
    (call-interactively 'tinydebian-bts-mail-type-rfp))
   ((eq request-type 'orphan)
    (call-interactively 'tinydebian-bts-mail-type-orphan))
   ((eq request-type 'adopt)
    (call-interactively 'tinydebian-bts-mail-type-ita))
   (t
    ;;  Nothing to do
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-status-main (package)
  "Find out PACKAGE details."
  (or (tinydebian-package-status-apt-cache package)
;;; the big *-S-* already runs this
;;;      (tinydebian-package-status-dpkg-s-main package)
      (tinydebian-package-status-grep-available package)
      (tinydebian-package-status-dpkg-S-main package)
      (tinydebian-package-status-apt-file package)
      (if (string-match "^wnpp" package)
          (error (concat "TinyDebian: package WNPP is special. "
                         "Use tinydebian-package-wnpp-main instead.")))
      (error "Tinydebian: Can't find package information. `%s'" package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-package-info (&optional package prompt)
  "Get PACKAGE information. See`tinydebian-package-status'.
If PACKAGE is nil and `tinydebian--bin-dpkg' is not available,
ask with PROMPT."
  (let ((dpkg tinydebian--bin-dpkg))
    (or package
        (setq package (read-string
                       (or prompt
                           "[TinyDebian] Package name: "))))
    (or (and dpkg
             (tinydebian-package-status-main package)))))

;;}}}
;;{{{ Bug reporting interface

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-general ()
  "Return relevant system information."
  ;; FIXME: todo
  (interactive))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-depends (info &optional depend-key)
  "Return additional Dependency INFO from item `Depends'.
DEPEND-KEY can be \"Depends\" [default] or \"Pre-Depends\".

Example:

  Versions of packages autolog depends on:
  ii  cron            3.0pl1-72  management of regular background p
  ii  libc6           2.2.5-3    GNU C Library: Shared libraries an."
  (let ((depends (tinydebian-package-info-by-key
                  (or depend-key "Depends")
                  info))
         str)
    (when depends
      (setq str "")
      (dolist (dep-info
               (tinydebian-package-status-parse-depends depends))
        (multiple-value-bind (package op version)
            dep-info
          (unless (tinydebian-package-virtual-p package)
            (if op ;; Not used yet, quiet byte compiler
                (setq op op))
            (if version
                (setq version version))
            (let (info2
                  desc
                  ver)
              (setq info2
                    (tinydebian-package-info
                     package
                     (format "\
\[TinyDebian] Depend. Insert `dpkg -s %s' to *scratch* and press RET: "
                             package)))
              (setq ver (cdr-safe (assoc "Version" info2)))
              ;; cut first few characters
              (when (setq desc (cdr-safe (assoc "Description" info2)))
                (setq desc (ti::string-left desc 45)))
              (setq str
                    (concat
                     str
                     (format "%-15s %-15s %s\n" package ver desc))))))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-architecture ()
  "Read architecture. Return empty string if cannot read."
  (when tinydebian--bin-dpkg
    (with-temp-buffer
      (tinydebian-call-process
       tinydebian--bin-dpkg  nil "--print-architecture")
      (tinydebian-string-delete-newlines
       (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-cache-policy-parse-pinned ()
  "Parse output of apt-cache policy from current point.
Point is moved forward.

Return:
  '((PACKAGE VERSION) ...)

\[An example listing]
Pinned packages:
     debian-policy -> 3.8.3.0
     topgit -> 0.7-1"
  (when (re-search-forward "^Pinned packages:[ \t]*$" nil t)
    (let (list)
      (while (re-search-forward
              "^[ \t]+\\([^ \t\r\n]+\\) +-> +\\(.*[^ \t\r\n]\\)" nil t)
        (push (list (match-string 1)
                    (match-string 2))
              list))
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-cache-policy-parse-priority ()
  "Parse output of apt-cache policy from current point.
Point is moved forward.

Return:
  assoc list: '(((priority VALUE) (url VALUE) ...) ...)

\[An example listing]

Package files:
 100 /var/lib/dpkg/status
     release a=now
 500 http://deb.opera.com sid/non-free Packages
     release o=Opera Software ASA,a=unstable,n=sid,l=The Opera web browser,c=non-free
     origin deb.opera.com
   1 http://ftp.fi.debian.org experimental/non-free Packages
     release o=Debian,a=experimental,n=experimental,l=Debian,c=non-free
     origin ftp.fi.debian.org
   1 http://ftp.fi.debian.org experimental/contrib Packages
     release o=Debian,a=experimental,n=experimental,l=Debian,c=contrib
     origin ftp.fi.debian.org
   1 http://ftp.fi.debian.org experimental/main Packages
     release o=Debian,a=experimental,n=experimental,l=Debian,c=main
     origin ftp.fi.debian.org
 500 http://ftp.fi.debian.org unstable/contrib Packages
     release o=Debian,a=unstable,n=sid,l=Debian,c=contrib
     origin ftp.fi.debian.org
 990 http://ftp.se.debian.org testing/main Packages
     release o=Debian,a=testing,n=squeeze,l=Debian,c=main
Pinned packages:
     debian-policy -> 3.8.3.0
     topgit -> 0.7-1"
  (when (re-search-forward "^Package files:[ \t]*$" nil t)
    (let (list)
      (while (re-search-forward
              "^ +\\([0-9]+\\) +\\(.*\\)" nil t)
        (let ((priority (match-string 1))
              (url      (match-string 2))
              release
              origin)
          (forward-line 1)
          (when (looking-at "^[ \t]+release[ \t]+\\(.*\\)")
            (setq release (match-string 1)))
          (forward-line 1)
          (if (looking-at "^[ \t]+origin[ \t]+\\(.*\\)")
              (setq origin (match-string 1))
            (forward-line -1))
          (push (list
                 (cons 'priority priority)
                 (cons 'url url)
                 (cons 'release release)
                 (cons 'origin origin))
                list)))
      ;; Return in original order
      (reverse list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-cache-policy-parse-priority-str
  (cache)
  "Call `tinydebian-bug-system-info-apt-cache-policy-parse-priority'
with CACHE as input."
  (when cache
    (with-temp-buffer
      (insert cache)
      (goto-char (point-min))
      (tinydebian-bug-system-info-apt-cache-policy-parse-priority))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-cache-policy-query ()
  "Return result of 'apt-cache policy'."
  (when tinydebian--bin-dpkg
   (with-temp-buffer
     (tinydebian-call-process
      tinydebian--bin-apt-cache nil "policy")
     (buffer-string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-cache-policy-parse-release
  (elt field)
  "Return from ELT, the assoc 'release and its FIELD.
The DATA is a single element of
`tinydebian-bug-system-info-apt-cache-policy-parse-priority'.

An example if 'release value, where FIELD could be 'a'
    o=Debian,a=testing,n=squeeze,l=Debian,c=main"
  (let ((str (cdr-safe (assoc 'release elt))))
    (when (and (stringp str)
               (string-match (concat field "=\\([^ \t\r\n,]+\\)") str))
      (match-string 1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-prefer (parsed-cache)
  "Check APT preferences from PARSED-CACHE.
See `tinydebian-bug-system-info-apt-cache-policy-query' for CACHE."
  (when parsed-cache
    ;; The last one is highest
    (let ((elt (car (reverse parsed-cache))))
      (tinydebian-bug-system-info-apt-cache-policy-parse-release
       elt "a"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-policy
  (parsed-cache &optional match)
  "Check APT policy from PARSED-CACHE.
See `tinydebian-bug-system-info-apt-cache-policy-query' for CACHE.
If optional MATCH is given, search for MATCH from release string.

Return string. Something like:
  ((990 'testing') (500 'unstable') (1 'experimental'))"
  (let (list)
    (dolist (elt parsed-cache)
      (let* ((priority (cdr-safe (assoc 'priority elt)))
             (relassoc (cdr-safe  (assoc 'release elt)))
             (release
              (if (or (null match)
                      (string-match match relassoc))
                  (tinydebian-bug-system-info-apt-cache-policy-parse-release
                   elt "a"))))
        (when release
          (pushnew (list priority release) list :test 'equal))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-apt-policy-debian (parsed-cache)
  "Call `tinydebian-bug-system-info-apt-policy' with 'Debian'."
  (tinydebian-bug-system-info-apt-policy parsed-cache "o=Debian"))

;;; ----------------------------------------------------------------------
;;; same as in reportbug(1)
(defun tinydebian-bug-system-info-apt-policy-as-string (policy-data)
  "Convert POLICY-DATA '((PRIORITY STRING) ...) into string.
See `tinydebian-bug-system-info-apt-policy' for POLICY-DATA."
  (when policy-data
    (let (str)
      (dolist (elt policy-data)
        (multiple-value-bind (priority value) elt
          (setq str
                (if str
                    (concat str (format " (%s, %s)" priority value))
                  (format "(%s, %s)" priority value)))))
      str)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os-version ()
  "Read Debian version from /etc/debian_version file."
  (let ((file  "/etc/debian_version")
        ret)
    (when (and (file-exists-p   file)
               (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (setq ret
              (tinydebian-string-delete-newlines
               (buffer-string)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-locale ()
  "Get locale information."
  (let ((list
         '("LANG"
           "LC_ALL"
           "LC_CTYPE"))
        val
        ret)
    (dolist (var list)
      (when (setq val (getenv var))
        (setq val (format "%s=%s" var val))
        (setq ret (if (null ret)
                      val
                    (concat ret ", " val)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-system-info-os ()
  "Return OS information.
Debian Release: 3.0
Architecture: i386
Kernel: Linux terra 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US."
  (let* ((kernel       (tinydebian-string-delete-newlines
                        (ti::process-uname)))
         (architecture (or (tinydebian-bug-system-info-os-architecture) ""))
         (release      (tinydebian-bug-system-info-os-version))
         (cache
          (tinydebian-bug-system-info-apt-cache-policy-parse-priority-str
           (tinydebian-bug-system-info-apt-cache-policy-query)))
         (apt-prefer   (tinydebian-bug-system-info-apt-prefer cache))
         (apt-policy
          (tinydebian-bug-system-info-apt-policy-as-string
           (tinydebian-bug-system-info-apt-policy-debian
            cache)))
        (locale       (or (tinydebian-bug-system-info-locale) "")))
    (format "\
Debian Release: %s
  APT Prefers %s
  APT policy: %s
Architecture: %s
Kernel: %s
Locale: %s"
            release
            apt-prefer
            apt-policy
            architecture
            kernel
            locale)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-severity ()
  "Select bug severity."
  (setq tinydebian--severity-selected nil)
  (while (null tinydebian--severity-selected)
    (ti::menu-menu 'tinydebian--menu-severity)
    (unless tinydebian--severity-selected
      (message "TinyDebian: Please select severity.")
      (sit-for 1)))
  tinydebian--severity-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-bug-report-mail-insert-details (info)
  "Insert Details for apckage INFO into Mail."
  (ti::mail-text-start 'move)
  (insert "Package: " (cdr (assoc "Package" info)) "\n")
  (insert "Version: " (cdr (assoc "Version" info)) "\n")
  (insert "Severity: " (tinydebian-bug-severity)   "\n\n")
  (let ((point       (point))
        (depends     (tinydebian-bug-system-info-depends info "Depends"))
        (pre-depends (tinydebian-bug-system-info-depends info "Pre-Depends"))
        (package     (or (and info
                              (cdr (assoc "Package" info)))
                         (error "No package information."))))
    (insert "\n\n-- System Information\n"
            (tinydebian-bug-system-info-os)
            (format "\n\n-- Versions of packages `%s depends on'.\n"
                    package)
            (if pre-depends
                (concat "Pre-Depends:\n" pre-depends)
              "")
            (if depends
                (concat "Depends:\n" depends)
              ""))
    (goto-char point)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-debian-bts-mail (info)
  "Submit Debian bug report. INFO is alist of attributes for a package.
An example ´reportbug(1)' looks like

To: submit@bugs.debian.org
Subject: autolog ....
--text follows this line--
Package: autolog
Version: 0.35-10
Severity: wishlist

-- System Information
Debian Release: 3.0
  APT prefers testing
  APT policy: (990, 'testing'), (500, 'unstable'), (1, 'experimental')
Architecture: i386
Kernel: Linux foo 2.4.17 #1 Fri Feb 8 21:32:43 EET 2002 i686
Locale: LANG=en_US, LC_CTYPE=en_US

Versions of packages autolog depends on:
ii  cron                          3.0pl1-72  management of regular background p
ii  libc6                         2.2.5-3    GNU C Library: Shared libraries an"
  (interactive
   (list (tinydebian-package-info)))
  (let ((status  (or (cdr-safe (assoc "Status" info)) ""))
        (package (or (cdr-safe (assoc "Package" info)) "")))
    (cond
     ((null info)
      (message "TinyDebian: no INFO available to send a bug report."))
     ((string-match "not-installed" status)
      (message "TinyDebian: bug report skipped. ´%s' status is [%s]"
               package status))
     (t
      (let ((name (format "*mail* Debian Bug %s" package))
            buffer)
        (cond
         ((and (setq buffer (get-buffer name))
               (null (y-or-n-p
                      "Delete previous bug report? ")))
          (pop-to-buffer buffer))
         (t
          (pop-to-buffer (get-buffer-create name))
          (erase-buffer)
          (let ((subject (read-string "[Debian BTS] bug Subject: ")))
            (mail-setup
             (tinydebian-bts-email-submit) subject nil nil nil nil))
          (message-mode)
          (tinydebian-bts-insert-headers)
          (tinydebian-bug-report-mail-insert-details info))))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-gnu-bts-mail (package)
  "Submit GNU bug report to PACKAGE."
  (interactive
   (list
    (completing-read
     "[GNU BTS] package: "
     '(("emacs" . 1)
       ("coreutils" . 1))
     nil
     t)))
  (cond
   ((not (stringp package))
    (error "No package name"))
   (t
    (let ((name (format "*mail* GNU Bug %s" package))
          buffer)
      (cond
       ((and (setq buffer (get-buffer name))
             (null (y-or-n-p
                    "Delete previous bug report? ")))
        (pop-to-buffer buffer))
       (t
        (pop-to-buffer (get-buffer-create name))
        (erase-buffer)
        (let ((subject (read-string
                        (format "[GNU BTS %s] bug Subject: " package))))
          (mail-setup
            (tinydebian-gnu-bts-email-compose "submit") subject nil nil nil nil))
        (message-mode)
        (ti::mail-text-start 'move)
        (cond
         ((tinydebian-system-os-debian-p)
          (let ((info (tinydebian-package-info package)))
            (tinydebian-bug-report-mail-insert-details info)))
         (t
          (insert (format "Package: %s\n" package))
          (insert (format "Version: \n"))
          (insert (format "Severity: %s\n\n"
                          (tinydebian-bug-severity)))))))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-launchpad-mail ()
  "Submit GNU bug report to PACKAGE."
  (interactive)
  (let ((name (format "*mail* Launchpad Bug"))
        buffer)
    (cond
     ((and (setq buffer (get-buffer name))
           (null (y-or-n-p
                  "Delete previous bug report? ")))
      (pop-to-buffer buffer))
     (t
      (pop-to-buffer (get-buffer-create name))
      (erase-buffer)
      (let ((subject (read-string
                      (format "[Launcpad BTS %s] bug Subject: " ""))))
        (mail-setup
         (tinydebian-launchpad-email-compose "new") subject nil nil nil nil))
      (message-mode)
      (ti::mail-text-start 'move)
      (insert "
Body:

\<Remeber: in order to submit bugs via email you have to sign the message with a
GPG key that is registered in Launchpad.>

 affects <distribution|package|product>
")
        (let ((str (mail-fetch-field "Subject")))
          (insert " summary " str "\n"))

        (let ((str (completing-read
                    "[Launchpad BTS] Importance: "
                    tinydebian--launchpad-importance-list)))
          (unless (string= "" str)
            (insert " importance " str "\n")))

        (let ((str (completing-read
                    "[Launchpad BTS] Is this security bug: "
                    '(("yes" . 1)
                      ("no" . 1)))))
          (unless (string= "" str)
            (insert " security " str "\n")))

        (let ((str (read-string "Subscribe to bug [launchpad id|email]: ")))
          (unless (string= "" str)
            (insert " subscribe " str "\n")))

        (let ((str (read-string "Assignee [name|email|'nobody']: ")))
          (unless (string= "" str)
            (insert " assignee " str "\n")))

        ;; (let ((str (completing-read
        ;;          "[Launchpad BTS] Status: "
        ;;          tinydebian--launchpad-status-list)))
        ;;   (unless (string= "" str)
        ;;     (insert " status " str "\n")))
        (insert " done\n")))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydebian-bug-report-generic-bts-mail (bts)
  "Select BTS and submit a bug report.
This function can only be callaed interactively."
  (interactive
   (list
    (completing-read
     "Bug report BTS: "
     '(("debian" . 1)
       ("gnu-emacs" . 1)
       ("gnu-coreutils" . 1)
       ("launchpad" . 1))
     nil
     t)))
  (when (stringp bts)
    (cond
     ((string-match "debian" bts)
      (call-interactively 'tinydebian-bug-report-debian-bts-mail))
     ((string-match "emacs" bts)
      (tinydebian-bug-report-gnu-bts-mail "emacs"))
     ((string-match "coreutils" bts)
      (tinydebian-bug-report-gnu-bts-mail "coreutils"))
     ((string-match "launchpad" bts)
      (tinydebian-bug-report-launchpad-mail)))))

;;}}}
;;{{{ Admin functions: MAIL reports

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-audit-report-tiger-make-chmod (file line)
  "Make suotable chmod command for FILE according to LINE report."
  (let ((operand "+")
        group
        group-cmd
        type
        type-cmd)
    (when (string-match
           "should .*+have +\\([^ \t\r\n]+\\) +\\([^ \t\r\n.]+\\)"
           line)
      (setq group (match-string 1 line)
            type  (match-string 2 line))
      (if (string-match "should not" line)
          (setq operand "-"))
      (cond
       ((string= group "group")
        (setq group-cmd "g"))
       ((string= group "world")
        (setq group-cmd "o")))
      (cond
       ((string-match type "read")
        (setq type-cmd "r"))
       ((string-match type "write")
        (setq type-cmd "w"))
       ((string-match type "exec")
        (setq type-cmd "x")))
      (when (and operand type-cmd group-cmd)
        (format "chmod %s%s%s %s;" group-cmd operand type-cmd file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydebian-command-audit-report-tiger (beg end)
  "Process tiger(1) mail system report on region BEG END.
The body of mail looks like:

    # Performing check of system file permissions...
    OLD: --WARN-- [perm001w] /var/log/wtmp should not have group write.
    OLD: --WARN-- [perm001w] /var/run/utmp should not have group write.
    OLD: --WARN-- [perm001w] /var/log/XFree86.0.log should not have world read.

For which a corresponding command to correct the error is generated.

    chmod g-w /var/log/wtmp;
    chmod g-w /var/run/utmp;
    chmod o-r /var/log/XFree86.0.log;

You can select region and these commands to shell `sh' with command
`shell-command-on-region' which can be called with \\[shell-command-on-region]."
  (interactive "r")
  (let ((buffer (get-buffer-create tinydebian--buffer-tiger))
        done
        file
        str)
    (goto-char beg)
    (while (re-search-forward
            "--WARN-- +[^ \t\r\n]+ +\\(\\([^ \t\r\n]+\\).*\\)"
            nil end)
      (setq file (match-string 2)
            str  (match-string 1))
      (unless done                  ;Draw one empty line between calls
        (setq done t)
        (ti::append-to-buffer buffer "\n"))
      (when (setq str (tinydebian-command-audit-report-tiger-make-chmod
                       file str))
        (ti::append-to-buffer buffer (concat str "\n"))))
    (cond
     ((ti::buffer-empty-p buffer)
      (message
       "TinyDebian: Hm, region did not have --WARN-- chmod candidates."))
     (t
      (display-buffer buffer)
      (message
       (substitute-command-keys
        (concat
         "TinyDebian: [tiger] "
         "Select region and send commands to"
         " `sh' with \\[shell-command-on-region]")))))))

;;}}}

(tinydebian-install-severity-functions) ;; Auto-created functions

(add-hook 'tinydebian--bts-mode-define-keys-hook
          'tinydebian-bts-mode-define-keys)

(add-hook 'tinydebian--mail-mode-define-keys-hook
          'tinydebian-mail-mode-define-keys)

(defalias 'tinydebian-reportbug 'tinydebian-bug-report-generic-bts-mail)

(provide   'tinydebian)
(run-hooks 'tinydebian--load-hook)

;;; tinydebian.el ends here
