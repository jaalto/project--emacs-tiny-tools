;;; tinycygwin.el --- Cygwin utilities (bug reports, administrative tasks).

;;{{{ Id

;; Copyright (C)    2004-2010 Jari Aalto
;; Keywords:        extensions
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

;; ........................................................ &t-install ...
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   $HOME/.emacs startup file
;;
;;   (add-hook 'tinycygwin--load-hook 'tinycygwin-install)
;;   (autoload 'tinycygwin-reportbug "tinycygwin" nil t)
;;   (autoload 'tinycygwin-package-info-port-maintainer-list "tinycygwin" nil t)
;;
;;   To get extra cygwin bindings in `message-mode', add this
;;
;;   (add-hook 'tinycygwin--load-hook 'tinycygwin-install-message-mode)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      This package contains utilities for the Cygwin System.
;;      It will help users to submit bug reports from Emacs.
;;      Learn more about Cygwin at http://www.cygwin.org/
;;
;;      To report bug against Cygwin package:
;;
;;          M-x tinycygwin-reportbug
;;
;;      When reporting bugs, one pseudo package is available which
;;      does not actually exist. If you select package "bug-generic"
;;      a standard bug template is generated. It can be used to report e.g.
;;      a configuration problem or to send a patch proposal to a 3rd party.
;;      The template provides additional environemtn information on your
;;      current syste,
;;
;;      A bug report's Subject is set to a time based id tag to thelp
;;      tracking and monitoring the messages.
;;
;;      To display list of all packages and their maintainers:
;;
;;          M-x tinycygwin-package-info-port-maintainer-list
;;
;;      To include e.g. cygcheck results to Email buffer, call
;;
;;          M-x tinycygwin-sysinfo-insert-os-cygwin
;;
;;      Further reading:
;;
;;          http://cygwin.com/problems.html

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: libraries

;; Due to variable message-cite-prefix-regexp
(require 'message)

(eval-when-compile
  ;; Quiet bogus CL warnings
  (defvar byte-compile-warnings)
  (unless (featurep 'xemacs)
    (set (make-local-variable 'byte-compile-warnings)
	 '(not cl-functions))))

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (autoload 'delete-if "cl-seq")
  (autoload 'member*   "cl-seq"))

(eval-and-compile
  ;;  Forward declarations
  (autoload 'executable-find            "executable")
  (autoload 'mail-setup                 "sendmail")
  (autoload 'ti::menu-menu              "tinylibmenu")
  (autoload 'mml-attach-file            "mml")
  (autoload 'mml-minibuffer-read-type   "mml")
  (autoload 'base64-decode-string       "base64")
  ;;  Byte compiler silencers
  (defvar debug-ignored-errors)
  (defvar font-lock-defaults)
  (defvar font-lock-keyword-face)
  (defvar font-lock-keywords)
  (defvar font-lock-mode)
  (defvar global-font-lock-mode)
  (defvar gnus-agent-send-mail-function)
  (defvar mail-header-separator)
  (defvar message-font-lock-keywords)
  (defvar message-mode-map)
  (defvar message-send-actions)
  (defvar message-user-mail-address)
  (defvar smtpmail-debug-info)
  (defvar smtpmail-local-domain)
  (defvar stack-trace-on-error)
  (defvar tinycygwin--command-switch-email)
  (defvar tinycygwin--command-switch-expert)
  (defvar tinycygwin--command-switch-files)
  (defvar tinycygwin--command-switch-package)
  (defvar tinycygwin--command-switch-type)
  (defvar user-mail-address)
  (defvar window-system))

(defgroup dired nil
  "Cygwin System administrator's grabbag of utilities."
  :group 'TinyCygwin)

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinycygwin--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyCygwin)

(defcustom tinycygwin--bug-report-mail-hook nil
  "*Hook run after `tinycygwin-bug-report-mail-compose-interactive'."
  :type  'hook
  :group 'TinyCygwin)

;;}}}
;;{{{ setup: user config

;;; ................................................... &v-user-config ...

(defcustom tinycygwin--dummy nil
  "*"
  :type  'string
  :group 'TinyCygwin)

(defface tinycygwin--warn-face
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
  :group 'TinyCygwin)

(defface tinycygwin--item-face
  '((((class color) (background light))
     (:foreground "green4"))
    (((class color) (background dark))
     (:foreground "green3")))
  "Face used for noticing important items."
  :group 'TinyCygwin)

(defcustom tinycygwin--expert-flag nil
  "*If non-nil, ask minimum of question in expert mode.
All fancy features or Emacs settings are also disabled."
  :type  'boolean
  :group 'TinyCygwin)

(defcustom tinycygwin--debug nil
  "*If non-nil, print extra message."
  :type  'boolean
  :group 'TinyCygwin)

;;}}}
;;{{{ setup: -- private

;;; ....................................................... &v-private ...

(defvar tinycygwin--os-type
  (cond
   ;;  Win32 and Cygwin are considered equal here
   ((or (memq system-type '(cygwin ms-dos windows-nt))
        (file-directory-p "c:/"))
    'cygwin)
   ((or (memq system-type '(gnu/linux))
        (string-match "linux" (emacs-version))
        (file-directory-p "/boot/vmlinuz")
        (file-directory-p "/vmlinuz"))
    'linux)
   ((or (memq system-type '(darwin))
        (string-match "darwin" (emacs-version))) ;; Mac OS
    'darwin))
  "Recognized system type: cygwin, linux, darwin,")

(defvar tinycygwin--original-font-lock-keywords nil
  "This value holds copy of `font-lock-keywords'. Do not touch.
Variable is made buffer local in `message-mode'.")

(defvar tinycygwin--external-call-flag nil
  "Set to non-nil while bug interface is called form external shell script.
Do not in any circumstances set this variable.")

(defvar tinycygwin--external-call-flag-value nil
  "Set to non-nil while bug interface is called form external shell script.
This is buffer local version of dynamically bound
`tinycygwin--external-call-flag'.")

(defvar tinycygwin--external-email-address nil
  "Set to non-nil when bug interface is called form external shell script.
Do not in any circumstances set this variable, but
set `user-mail-address' to correct value.")

(defvar tinycygwin--email-cygwin-users-list
  "user list <cygwin@cygwin.com>"
  "Email address of Cygwiin mailing list.")

(defvar tinycygwin--email-cygwin-apps-list
  "devel list (subscriber only) <cygwin-apps@cygwin.com>"
  "Email address of Cygwin mailing list.")

(defvar tinycygwin--email-cygwin-xfree-list
  "xfree devel list <cygwin-apps@cygwin.com>"
  "Email address of Cygwiin mailing list.")

(defvar tinycygwin--email-cygbug-maintainer
  (lambda ()
    (concat
     "cygbug/tinycygwin.el maintainer - "
     (base64-decode-string "amFyaS5hYWx0b0Bwb2JveGVzLmNvbQ==")))
  "Email address of mainteiner. String or function.
The function should return email address.")

(defvar tinycygwin--root-dir "/"
  "Location of Cygwin root directory.")

(defvar tinycygwin--file-install-db
  (concat (file-name-as-directory tinycygwin--root-dir)
          "etc/setup/installed.db")
  "Location of `installed.db'.
Notice that this is the official Cygwin nstallation file that
reports packages that have been installed using Cygwin netinstaller.
This does not report any 3rd party local installation.")

(defvar tinycygwin--path-doc-root-list
  (list (concat (file-name-as-directory tinycygwin--root-dir)
                "usr/share/doc")
        (concat (file-name-as-directory tinycygwin--root-dir)
                "usr/doc")) ;; Old location
  "Location of documentation.")

(defvar tinycygwin--path-doc-cygwin-list
  (list (concat (file-name-as-directory tinycygwin--root-dir)
                "usr/share/doc/Cygwin")
        (concat (file-name-as-directory tinycygwin--root-dir)
                "usr/doc/Cygwin")) ;; Old location
  "List of directories of Cygwin package documentation.")

(defvar tinycygwin--bin-cygcheck (executable-find "cygcheck")
  "Location of `cygcheck' binary.")

(defvar tinycygwin--file-cygcheck
  (concat (file-name-as-directory tinycygwin--root-dir)
          "tmp/cygcheck-report.txt")
  "Cached result of cygcheck -c -s -d")

(defvar tinycygwin--buffer-maintainer-list "*Cygwin maintainer summary*"
  "List of Cygwin packages and their maintainers.
See \\[tinycygwin-package-info-port-maintainer-list\\]")

(defvar tinycygwin--buffer-cygcheck "*Cygwin cygcheck*"
  "Cygcheck systeinfo buffer")

(defvar tinycygwin--history-ask-program nil
  "History of quesions.
See function `tinycygwin-message-mode-attach-program-version'.")

(defvar tinycygwin--history-ask-version nil
  "History of quesions.
See function `tinycygwin-message-mode-attach-program-version'.")

(defconst tinycygwin--sysinfo-program-list
  '((devel-tools ("gcc" "make" "libtool" "automake"))
    (lang        ("perl" "python" "ruby")))
  "List of system information bundles.
Format:

  '((BUNDLE-SYMBOL (\"program\" \"program\" ...))
    ..)")

(defconst tinycygwin--sysinfo-environment-list
  '("CYGWIN")
  "List of environment variables to include in bug report.")

(defvar tinycygwin--package-maintainer-email-include nil
  "Should the Cygwin Net package maintainer's email addres be offered.
Nil is the default value. If you set this to `t' be sure that
you know what you're doing. The default policy is not to send any personal
mail, but direct bug messages to the mailing lists.

Only if you're a package developer and know some of the maintainers
personally, setting this variable to t provide concatct help.")

(defvar tinycygwin--package-upstream-email-include t
  "Should the Upstream author's email address be offered.
That, the author who is developing the package. Most likely he knows
nothing about Cygwin, so tthe Cygwin mailing lists should be the
first contact points. If you have found real bug, then it would
be good to contact the Author.")

;;  Same as in Debian
(defconst tinycygwin--severity-list
  '(("critical"
     "Makes unrelated software on the system (or the whole system) break,
or causes serious data loss, or introduces a security hole on systems where
you install the package.")
    ("grave"
     "Makes the package in question unuseable or mostly so, or causes data
loss, or introduces a security hole allowing access to the accounts of users
who use the package.")
    ("serious"
     "Severe violation of policy (that is, it violates a
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
  "The bug system may record a severity level with each bug report.
This is set to normal by default, but can be overridden either by supplying a Severity line in the pseudo-header when the bug is submitted Severity or error.")

(defvar tinycygwin--menu-severity-selected nil
  "Functions `tinycygwin-severity-select-*' set this to user selection.")

(defvar tinycygwin--menu-bug-classification-selected nil
  "Functions `tinycygwin-type-select-*' set this to user selection.")

(defconst tinycygwin--menu-bug-classification
  '("\
Type of bug: q)uit ?)help RET)standard u)pdate U)pstream"
    ;; NOTE: These function are automatically created, you don't find
    ;; See `tinycygwin-install-bug-classification-functions'.
    ((?\C-m .   ( (setq tinycygwin--menu-bug-classification-selected
                        "standard")))
     (?u .      ( (setq tinycygwin--menu-bug-classification-selected
                        "update")))
     (?U .      ( (setq tinycygwin--menu-bug-classification-selected
                        "upstream")))))
  "Bug classification menu.

standard
    Report standard package bug. The packaging is erroneous, files are
    in incorrect places, configuration files have problems, default setup
    does not work etc.

    Please do not report program's behavious problems. The package
    maintainer does not know how the program is supposed to work. He has
    only put it available in Cygwin Net Release form, so he is not the
    correct person where to report problems in the program itself (see
    bug type 'upstream' below).

update
    Request package update. Package included in Cygwin is out of date
    and there is newer one available at upstream sources. You want the
    package maintainer to be informed.

upstream
    Report problems to upstream. You are seing erratic behavior of the
    program or you think some new feature would be nice. Contact maintainer
    or author of the program.")

(defconst tinycygwin--menu-severity
  '("\
Severity: q?)help c)rit g)rave s)erious i)import RET-n)orm m)inor w)ish f)ixed"
    ;; NOTE: These function are automatically created, you don't find
    ;; them with C-h f or from this file with C-s.
    ;; See `tinycygwin-install-severity-functions'
    ((?c .      ( (call-interactively 'tinycygwin-severity-select-critical)))
     (?g .      ( (call-interactively 'tinycygwin-severity-select-grave)))
     (?s .      ( (call-interactively 'tinycygwin-severity-select-serious)))
     (?i .      ( (call-interactively 'tinycygwin-severity-select-important)))
     (?n .      ( (call-interactively 'tinycygwin-severity-select-normal)))
     (?\C-m .   ( (call-interactively 'tinycygwin-severity-select-normal)))
     (?m .      ( (call-interactively 'tinycygwin-severity-select-minor)))
     (?w .      ( (call-interactively 'tinycygwin-severity-select-wishlist)))
     (?f .      ( (call-interactively 'tinycygwin-severity-select-fixed)))))
  "Severity menu.

The bug system records a severity level with each bug report. This is set
to normal by default, but can be overridden either by supplying a Severity
line in the pseudo-header when the bug is submitted (see the instructions
for reporting bugs), or by using the severity command with the control
request server.

critical
    Makes unrelated software on the system (or the whole system) break, or
    causes serious data loss, or introduces a security hole on systems where
    you install the package.

grave
    Makes the package in question unuseable or mostly so, or causes data loss,
    or introduces a security hole allowing access to the accounts of users who
    use the package.

serious
    Is a severe violation of policy (that is, it violates a \"must\" or
    \"required\" directive), or, in the package maintainer's opinion, makes the
    package unsuitable for release.

important
    A bug which has a major effect on the usability of a package, without
    rendering it completely unusable to everyone.

normal
    The default value, applicable to most bugs.

minor
    A problem which doesn't affect the package's usefulness, and is presumably
    trivial to fix.

wishlist
    For any feature request, and also for any bugs that are very difficult to
    fix due to major design considerations.

fixed
    For bugs that are fixed but should not yet be closed. This is an exception
    for bugs fixed by non-maintainer uploads. Note: the \"fixed\" tag should be
    used instead.  Certain severities are considered release-critical, meaning
    the bug will have an impact on releasing the package with the stable
    release. Currently, these are critical, grave and serious.")

(defvar tinycygwin--tags-list
  '(("patch"
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
     "This bug applies to the upstream part of the package."))
  "Each bug can have zero or more of a set of given tags.
These tags are displayed in the list of bugs when you look at a
package's page, and when you look at the full bug log.")

(defvar tinycygwin--wnpp-buffer "*TinyCygwin WNPP*"
  "WNPP question buffer.")

(defvar tinycygwin--menu-wnpp-selected nil
  "Placeholder of selection from `tinycygwin--menu-wnpp'.")

(defconst tinycygwin--menu-wnpp
  (list
   '(format
     "\
WNPP:%s q?)help 1i)itp 2o)rphan 3a)dopt 4n)ew suggested package"
     (if tinycygwin--menu-wnpp-selected
         (format "%s; " (symbol-name tinycygwin--menu-wnpp-selected))
       ""))
   (list
    '(?1 . ( (setq tinycygwin--menu-wnpp-selected 'package)))
    '(?i . ( (setq tinycygwin--menu-wnpp-selected 'package)))
    '(?I . ( (setq tinycygwin--menu-wnpp-selected 'package)))
    '(?p . ( (setq tinycygwin--menu-wnpp-selected 'package)))
    '(?P . ( (setq tinycygwin--menu-wnpp-selected 'package)))
    '(?2 . ( (setq tinycygwin--menu-wnpp-selected 'orphan)))
    '(?o . ( (setq tinycygwin--menu-wnpp-selected 'orphan)))
    '(?O . ( (setq tinycygwin--menu-wnpp-selected 'orphan)))
    '(?3 . ( (setq tinycygwin--menu-wnpp-selected 'adopt)))
    '(?a . ( (setq tinycygwin--menu-wnpp-selected 'adopt)))
    '(?A . ( (setq tinycygwin--menu-wnpp-selected 'adopt)))
    '(?4 . ( (setq tinycygwin--menu-wnpp-selected 'new)))
    '(?n . ( (setq tinycygwin--menu-wnpp-selected 'new)))
    '(?N . ( (setq tinycygwin--menu-wnpp-selected 'new)))))
  ;;  This message is straight from reportbug(1)
  ;;  'apt-get install reportbug'
  "What request type? If none of these things mean anything to you you
should report normal bug to existing package instead.

1 i    ITP, `Intent To Package'. You want to be maintainer
       of this package. Please submit a package description
       along with copyright and URL in such a report.

2 o    The package has been `Orphaned'. Nobody is maintaining it.
       It needs a new maintainer as soon as possible.

3 a    RFA, this is a `Request for Adoption'. Due to lack of time, resources,
       interest or something similar, the current maintainer is asking for
       someone else to maintain this package. He/she will maintain it in the
       meantime, but perhaps not in the best possible way. In short: the
       package needs a new maintainer.

4 n    RFP, this is a `Request For Package'. You have found an interesting
       piece of software and would like SOMEONE ELSE to package and
       maintain it. Please submit a package description along with
       copyright and URL in such a report.

q      Quit menu.

See http://www.debian.org/devel/wnpp for more information
")

;; Emacs includes so good message.el colors, that it does not need
;; these. Do not modify.

(defvar tinycygwin--message-mode-font-lock-keywords-window-system
  (list
   (list
    "^-- +[A-Z][^ \t\r\n]+ +.*"
    (list 0 'font-lock-builtin-face t))
   (list
    "^Severity:[ \t]+\\(critical\\|grave\\|serious\\)"
    (list 1 'font-lock-warning-face t))
   (list
    "^Severity:[ \t]+\\(wishlist\\)"
    (list 1 'font-lock-string-face t))
   (list
    "^[A-Z][^ \t\r\n]+:"
    (list 0 'font-lock-doc-string-face))
   (list
    "^\\[ATTACHMENT.*"
    (list 0 'font-lock-constant-face t)))
  "Additional `message-mode' `font-lock-keywords'.
This is for XEmacs. Activated only if `tinycygwin--expert-flag' is nil.")

;;;   (list
;;;    "^\\([Tt]o:\\)\\(.*\\)"
;;;    (list 1 'message-header-name-face)
;;;    (list 2 'message-header-to-face nil t))
;;;   (list
;;;    "^\\([Cc]C\\|Reply-[Tt]o:\\)\\(.*\\)"
;;;    (list 1 'message-header-name-face)
;;;    (list 2 'message-header-cc-face nil t))
;;;   (list
;;;    "^\\(Subject:\\)\\(.*\\)"
;;;    (list 1 'message-header-name-face)
;;;    (list 2 'message-header-subject-face nil t))

(defvar tinycygwin--message-mode-font-lock-keywords-non-window-system
  ;;  In XEmacs 21.4, Cygwin, there are only two faces available.
  (list
   (list
    "^[A-Z][^ \t\r\n]+:"
    (list 0 'font-lock-type-face))
   (list
    "^-- +[A-Z][^ \t\r\n]+ +.*"
    (list 0 'font-lock-function-name-face t))
   (list
    "^Severity:[ \t]+\\(critical\\|grave\\|serious\\)"
    (list 1 'font-lock-warning-face t))
   (list
    "^Severity:[ \t]+\\(wishlist\\)"
    (list 1 'font-lock-comment-face t))
   (list
    "^\\[ATTACHMENT.*"
    (list 0 'message-header-other-face t)))
  "Additional `message-mode' `font-lock-keywords'.
This is for XEmacs. Activated only if `tinycygwin--expert-flag' is nil.")

(defvar tinycygwin--email-address-correct-list
  '((" A *T " "@")
    (" do?t " "."))
  "List of regexp to correct email addresses.
Format:
  '((SEARCH-REGEXP  REPLACE-STRING)
    (SEARCH-8REGEXP REPLACE-STRING)
     ...)")

;;}}}
;;{{{ XEmacs support

;;; ----------------------------------------------------------------------
;;;
;;; (put 'tinycygwin-defalias 'lisp-indent-function 0)
(defmacro tinycygwin-defalias (this that)
  "If there is no THIS then use THAT. Signal error if cannot make `defalias'."
  `(if (not (fboundp ,this))
       (if (fboundp ,that)
           (defalias ,this ,that)
         (error "[ERROR] function is not supported by this X/Emacs: %s"
                (symbol-name ,this)))))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (re str string)
    "TinyCygwin XEmacs support.
This is a cheap implementaion of an Emacs function and it DOES NOT
support all the capabilities. You code will break if it relies on this
to exist."
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (replace-match str))
      (buffer-string))))

(defun tinycygwin-window-system ()
  "XEmacs and Emacs Compatibility, Mimic Emacs `window-system' variable.
In XEmacs the `cosole-type' returns 'tty on terminal, but this function
return nil to be in par with Emacs behavior. An 'tty is not a windowed
environment."
  (let ((func 'console-type))
    (cond
     ((fboundp func)
      (let ((val (funcall func)))
        (unless (eq 'tty val)
          val)))
     ((boundp 'window-system)
      (symbol-value 'window-system)))))

(tinycygwin-defalias 'insert-file-contents-literally   'insert-file)
(tinycygwin-defalias 'line-beginning-position 'point-at-bol)
(tinycygwin-defalias 'line-end-position       'point-at-eol)

;;}}}
;;{{{ General

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-file-binary-p (file)
  "Check if FILE name looks like binary file (.gz etc.)."
  (string-match "\\.\\(g?z\\|bz2\\|zip\\|tar\\)" file))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-insert-file (file)
  "INsert FILE literally at point."
  (tinycygwin-clean-system-with
   (insert-file-contents-literally file)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-mail-attachment-tag (string)
  "Return attachment tag for STRING."
  (format "[ATTACHMENT: %s]"
          (if (string-match "[\\/]" string)
              (file-name-nondirectory string)
            string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-maintainer ()
  "Return maintainer."
  (let ((val tinycygwin--email-cygbug-maintainer))
    (cond
     ((functionp val)
      (funcall val))
     ((stringp val)
      val)
     ((listp val)
      (eval val)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-variable-documentation (variable-sym)
  "Return documentation of VARIABLE-SYM."
  (let ((str (documentation-property
              (if (boundp variable-sym)
                  variable-sym)
              'variable-documentation)))
    (when (stringp str)
      (replace-regexp-in-string
       "\r" ;; Remove possible extra line endings
       ""
       str))))

;;}}}
;;{{{ Install: bindings

;;; ........................................................ &bindings ...

(defun  tinycygwin-tab-to-tab-stop-4-spaces (map)
  "Define TAB key to run 4 spaces."
  ;;  Status:     `tab-stop-list' is core Emacs variable
  ;;  Info:       (Info-goto-node "(emacs)Tab Stops")
  ;;
  ;;  Make TAB key advance at 4 positions at the time. The code
  ;;  will set the tab-stop-list to value '(4 8 12 16 20 ...)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list
        (let ((i 4) list)
          (while (< i 80)
            (setq list (cons i list))
            (setq i (+ i 4)))
          (reverse list)))
  (define-key map "\t" 'tab-to-tab-stop))

;; #todo:
(defun tinycygwin-bug-report-default-bindings ()
  "Define default key bindings to `tinycygwin-mode-map'.")

;;}}}
;;{{{ Install: generate severity function etc.

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-install-menu-function-macro 'lisp-indent-function 0)
(defmacro tinycygwin-install-menu-function-macro (template value variable)
  "Generate ti::menu TEMPLATE, VALUE using VARIABLE."
  (let ((sym (intern (format template value))))
    `(defun ,sym ()
       (interactive)
       (setq  ,variable , value))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinycygwin-menu-call-with (menu-symbol variable)
  "Call MENU-SYMBOL and return content of VARIABLE."
  ` (progn
      (setq ,variable nil)
      (ti::menu-menu ,menu-symbol)
      ,variable))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-menu-function-list (variable-sym)
  "Get list of menu functions from VARIABLE-SYM.
The menu item is left flushed, lowercase word that is immediately
followed by indented two space explanation. An example:

  item
    The item is ..."
  (let ((string (tinycygwin-variable-documentation variable-sym))
	case-fold-search
	list)
    (when string
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward "^\\([a-z]+\\)[ \t]*\n[ \t]+[A-Z]" nil t)
          (push (match-string 1) list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-severity-functions ()
  "Generate `tinycygwin-severity-select-*' user functions."
  ;; Generate functions at run-time.
  (mapcar
   (lambda (x)
     (eval
      `(tinycygwin-install-menu-function-macro
        "tinycygwin-severity-select-%s"
        ,x
        tinycygwin--menu-severity-selected)))
   (tinycygwin-install-menu-function-list
    'tinycygwin--menu-severity)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-bug-classification-functions ()
  "Generate `tinycygwin-severity-select-*' user functions."
  ;; Generate functions at run-time.
  (mapcar
   (lambda (x)
     (eval
      `(tinycygwin-install-menu-function-macro
        "tinycygwin-type-select-%s"
        ,x
        tinycygwin--menu-bug-classification-selected)))
   (tinycygwin-install-menu-function-list
    'tinycygwin--menu-bug-classification)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-find-file-hook ()
  "Install `font-lock-keywords' for log files."
  (tinycygwin-font-lock-keywords))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-font-lock-keywords (&optional uninstall)
  "Install colors to all current buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (tinycygwin-font-lock-keywords uninstall))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-help ()
  "Return quick help of additional commands."
  (substitute-command-keys
   (concat
    "Insert (file) "
    "\\[tinycygwin-insert-attach-file-as-is] "
    "(Env. var) "
    "\\[tinycygwin-insert-environment-variable-content] "
    "(cygcheck) "
    "\\[tinycygwin-message-mode-attach-cygcheck]")))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-help-simple ()
  "Return quick help of additional commands."
  (concat
   "Additional Cygwin related commands at C-c C-p C-h"))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-faces ()
  "Use custom faces."
  ;;  The defaults are not readable in Cygwin white/black rxvt
  (set-face-foreground
   'message-header-name-face
   (face-foreground 'font-lock-string-face))
  (set-face-foreground
   'message-header-cc-face
   (face-foreground 'font-lock-constant-face))
  (set-face-foreground
   'message-header-to-face
   (face-foreground 'font-lock-builtin-face)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-hook ()
  "Install extra Cygwin specific keybindings to `message-mode'."
  (when (boundp 'message-mode-map)
    (tinycygwin-tab-to-tab-stop-4-spaces message-mode-map)
    (define-key message-mode-map "\C-C\C-pa"
      'tinycygwin-insert-attach-file-as-is)
    (define-key message-mode-map "\C-C\C-pc"
      'tinycygwin-message-mode-attach-cygcheck)
    (define-key message-mode-map "\C-C\C-pe"
      'tinycygwin-insert-environment-variable-content)
    (define-key message-mode-map "\C-C\C-p-"
      'font-lock-mode)
    (define-key message-mode-map "\C-C\C-p\C-r"
      'rename-uniquely)
    (define-key message-mode-map "\C-C\C-pv"
      'tinycygwin-message-mode-attach-program-version)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-message-mode (&optional uninstall)
  "Install extra Cygwin specific keybindings to `message-mode'."
  (if uninstall
      (remove-hook 'message-mode-hook 'tinycygwin-message-mode-hook)
    (add-hook 'message-mode-hook 'tinycygwin-message-mode-hook)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-install (&optional uninstall)
  "Install or optionally UNINSTALL (i.e. inactivate) this lisp package."
  (interactive "P")
  (when nil
    (cond
     (uninstall
      (tinycygwin-install-font-lock-keywords 'uninstall)
      (remove-hook 'find-file-hook 'tinycygwin-find-file-hook)
      nil)
     (t
      (tinycygwin-install-font-lock-keywords)
      (add-hook 'find-file-hook  'tinycygwin-find-file-hook)
      nil))))

;;}}}
;;{{{ Email functions

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-insert-attach-file-as-is (file)
  "Insert FILE attachment \"as is\" to the end of buffer.
This is different than a regular MIME attachment that is
inserted in `message-mode' with \\[mml-attach-file]."
  (interactive "FAttach file as is: ")
  (save-current-buffer
    (goto-char (point-max))
    (tinycygwin-bug-report-mail-attach-file file)))
;;##
;;; ----------------------------------------------------------------------
;;;
;; (defun tinycygwin-insert-environment-variable-content (var)
;;   "Inser content of environment variable VAR at point."
;;   (interactive
;;    (list
;;     (completing-read
;;      "Insert environment variable: "
;;      (mapcar
;;       (lambda (x)
;;         (if (string-match "^\\(.+\\)=\\(.*\\)" x)
;;             (cons (match-string 1 x)
;;                   (match-string 2 x))
;;           (cons "__NOT_FOUND__" . 1)))
;;       process-environment)
;;      nil
;;      'match)))
;;   (when var
;;     (let ((value (getenv var)))
;;       (insert (format "%s=%s" var (or (getenv var) ""))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-attach-cygcheck ()
  "Insert cygcheck retults to the end of buffer as a MIME attachement."
  (interactive)
  (let ((file (make-temp-file "emacs-tinycygwin-cygcheck"))
	point
	status)
    (save-current-buffer
      (goto-char (point-max))
      (message "Wait, calling cygcheck [may take a while]... ")
      (with-temp-buffer
        (tinycygwin-sysinfo-insert-os-cygwin)
        (write-region (point-min) (point-max) file))
      (message "Wait, calling cygcheck [may take a while]... Done.")
      (tinycygwin-bug-report-mail-insert-files (list file) 'mime))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-program-parse-version ()
  "Parse version information from program's version output."
  (let ((list '(("[0-9]+\\.[0-9]+\\([0-9.]+\\)?" 0)))
	version)
    (dolist (elt list)
      (goto-char (point-min))
      (multiple-value-bind (regexp subexp) elt
        (when (and (re-search-forward regexp nil t)
                   (setq version (match-string subexp)))
          (return))))
    version))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-attach-program-version
  (program &optional opt mode)
  "Insert version information of PROGRAM calling with optional OPT.

Possible values for variable MODE

  'end    Insert to the end of buffer.
  'ask    Ask user what to do.
  nil     Insert at point."
  (interactive
   (list
    (read-string "Progam name: "
                 nil 'tinycygwin--history-ask-program)
    (read-string "Version option [--version by default]: "
                 nil tinycygwin--history-ask-version)
    'ask))
  (let* ((file (make-temp-file
                (format
                 "emacs-tinycygwin-program-version-%s" program)))
         (try-opt (if (or (null opt)
                          (and (stringp opt)
                               (string= opt "")))
                      '("--version" "-V" "-v")
                    (list opt)))
         (bin     (if (not (string-match "[\\/]" program))
                      (executable-find program)
                    program))
         point
         status
         version)
    (unless bin
      (error "TinyCygwin: [ERROR] %s not found in PATH" program))
    (save-current-buffer
      (dolist (option try-opt)
        (message "Wait, calling %s %s ... "
                 program option)
        (with-temp-buffer
          (call-process bin
                        nil              ;infile
                        (current-buffer) ;buffer
                        nil              ;display
                        option)
          (when (setq version (tinycygwin-program-parse-version))
            (write-region (point-min) (point-max) file)
            (setq opt option)
            (return))))
      (cond
       ((null version)
        (message "Couldn't read version information. Please insert manually."))
       (t
        (let ((action (if (eq mode 'ask)
                          (if (y-or-n-p
                               "Insert at point or to the end of buffer? ")
                              nil
                            'end)
                        mode)))
          (cond
           ((eq action 'end)
            (goto-char (point-max))
            (tinycygwin-bug-report-mail-attach-file file))
           (t
            (insert-file-contents-literally file)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-valid-p (email)
  "Check if EMAIL address look valid."
  (and (stringp email)
       ;; foo@this.net
       ;; ||  ||  |  |
       ;; 12  34  5  6
       ;;
       ;; 1. start with non-whitespace
       ;; 2. followed by anything until @
       ;; 3. continue non-whitespace
       ;; 4. followed by anything until
       ;; 5. Must have period "."
       ;; 6. continue non-whitespace
       (string-match
        (concat
         "^"
         "[ \t]*"
         "[^ \t\r\n]+.*@[^ \t\r\n]+.*\\.[^ \t\r\n]+"
         "[ \t]*"
         "$")
        email)
       email))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-full-name-valid-p (str)
  "Check if STR includes valid 'Firstname Lastname'."
  (and (stringp str)
       (let (case-fold-search)
         (string-match "^[^ \t\r\n]+ +[^ \t\r\n]" str))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-value (&optional email)
  "Check `user-mail-address' and read environment variable EMAIL.
Return correct email address or nil."
  (dolist (try (list
                email
                tinycygwin--external-email-address
                (and (boundp 'message-user-mail-address)
                     message-user-mail-address)
                (and (boundp 'user-mail-address)
                     user-mail-address)
                (getenv "EMAIL")))
    (if (and (stringp try)
             (tinycygwin-user-mail-address-valid-p try))
        (return try))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-set ()
  "Set `user-mail-address' from possible addresses; the one that is valid.
If `user-mail-address' is already valid, do nothing. If cannot set,
call `error'."
  (unless (tinycygwin-user-mail-address-valid-p user-mail-address)
    (let ((value (tinycygwin-user-mail-address-value)))
      (unless value
        (error (concat "** [ERROR] Can't determine `user-mail-address'."
                       "Please define environemnt variable EMAIL.")))
      (setq user-mail-address value))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-full-name-set ()
  "Set `user-full-name' from environment.
The varaibles NAME and DEBFULLNAME are examine if  `user-full-name'
does not contain space separated Firstname Lastname."
  (unless (tinycygwin-user-full-name-valid-p user-full-name)
    (let ((name (or (getenv "NAME")
                    (getenv "DEBFULLNAME"))))
      (unless name
        (error "TinyCygwin: [ERROR] Can't set `user-full-name'. %s"
               "Please define environment variable NAME."))
      (setq user-full-name name))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-fix-angles ()
  "Add <> around email in current buffer."
  (goto-char (point-min))
  (when (re-search-forward "@" nil t)
    (skip-chars-backward "^ ")
    (insert "<")
    (skip-chars-forward "^ ")
    (insert ">")
    (buffer-string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-correct (str)
  "Correct words like 'A T' as @ etc."
  (with-temp-buffer
    (insert str)
    (dolist (elt tinycygwin--email-address-correct-list)
      (goto-char (point-min))
      (while (re-search-forward (car elt) nil t)
        (replace-match (nth 1 elt))))
    (buffer-string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-user-mail-address-fix (list)
  "Add missing <> around LIST of email addresses like '(me@example.com)."
  (when list
    (with-temp-buffer
      (let (ret)
        (dolist (str list)
          (when (and (stringp str)
                     (setq str (tinycygwin-user-mail-address-correct str))
                     (string-match "@" str))
            (unless (string-match "[<>]" str)
              (erase-buffer)
              (insert str)
              (setq str
                    (tinycygwin-user-mail-address-fix-angles)))
            (push str ret)))
        (reverse ret)))))

;;}}}
;;{{{ Utility functions

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-debug 'edebug-form-spec '(body))
(put 'tinycygwin-debug 'lisp-indent-function 0)
(defmacro tinycygwin-clean-system-with (&rest body)
  "Disable almost all auto-features and run BODY."
  `(let (auto-mode-alist
         find-file-hook
         interpreter-mode-alist)
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-debug 'edebug-form-spec '(body))
(put 'tinycygwin-debug 'lisp-indent-function 0)
(defmacro tinycygwin-debug (&rest body)
  "Run BODY when tinycygwin--debug is non-nil."
  `(when tinycygwin--debug
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-external-with 'edebug-form-spec '(body))
(put 'tinycygwin-external-with 'lisp-indent-function 0)
(defmacro tinycygwin-external-with (&rest body)
  "Run BODY if this is external call.
References:
  `tinycygwin--external-call-flag'
  `tinycygwin--external-call-flag-value'"
  `(when (or tinycygwin--external-call-flag
             tinycygwin--external-call-flag-value)
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-expert-with 'edebug-form-spec '(body))
(put 'tinycygwin-expert-with 'lisp-indent-function 0)
(defmacro tinycygwin-expert-with (&rest body)
  "Run BODY if `tinycygwin--expert-flag' is no-nil."
  `(when tinycygwin--expert-flag
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-non-expert-with 'edebug-form-spec '(body))
(put 'tinycygwin-non-expert-with 'lisp-indent-function 0)
(defmacro tinycygwin-non-expert-with (&rest body)
  "Run BODY if `tinycygwin--expert-flag' is nil."
  `(unless tinycygwin--expert-flag
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-not-modified-with 'edebug-form-spec '(body))
(put 'tinycygwin-not-modified-with 'lisp-indent-function 0)
(defmacro tinycygwin-not-modified-with (&rest body)
  "Mark buffer as not modified after BODY."
  `(progn
     ,@body
     (set-buffer-modified-p nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-email-choice-list (&optional package)
  "Return list of Email choices for for user with `completing-read'."
  (let ((list
         (list
          tinycygwin--email-cygwin-users-list
          tinycygwin--email-cygwin-apps-list
          (if (and package
                   (not (string-match "^x" package)))
              nil
            tinycygwin--email-cygwin-xfree-list)
          (unless package
            (tinycygwin-maintainer)))))
    (delq nil list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-email-prefix (&optional type)
  "Return Subejct's bug prefix string 'OS-TYPE#YYYYMMDDTHHMM'
The time is in UTC and similar to 'date ----iso-8601=minutes'
The TYPE is 'bug' by default, but can also be other type, like
rfa, rfp, itp, orphan, update. See `tinycygwin--menu-wnpp'."
  (format
   ;;  Cygwin-bug#NNNN Linux-Bug#NNNN
   (concat (if tinycygwin--os-type
               (format "%s-" (capitalize (symbol-name tinycygwin--os-type)))
             "")
           "%s#%s")
   (or type
       "bug")
   ;; XEmacs does not support argument UTC
   (if (featurep 'xemacs)
       (format-time-string "%Y%m%dT%H%M")
     (format-time-string "%Y%m%dT%H%M" nil 'utc))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-email-buffer-name (package &optional type)
  "Compose *mail* buffer name string using PACKAGE.
Optional TYPE is by deault \"bug\"."
  (format "*mail* Cygwin %s%s"
          (or type
              "bug")
          (if (and package
                   (not (string= "" package)))
              (format " (%s)" package)
            "")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-string-trim (string)
  "Delete leading and trailing spaces."
  (when string
    (replace-regexp-in-string "^[ \t]+" "" string)
    (replace-regexp-in-string "[ \t]+$" "" string)
    string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-include-buffer-name-p (str)
  "Check buffer name STR is Bug report include file."
  (string-match "tinycygwin include" (or str "")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-include-buffer-name (str)
  "Convert string into buffer name that would be included in Bug report."
  (unless (tinycygwin-bug-report-include-buffer-name-p str)
    (format "*tinycygwin include %s*" (buffer-name))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-include-buffer-list ()
  "Return list of Bug report include buffers."
  (let (list)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (tinycygwin-bug-report-include-buffer-name-p
                    (buffer-name)))
          (push buffer list))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-msg-exit-emacs ()
  "Return string to say how to exit Emacs."
  (substitute-command-keys
   "Exit Emacs \\[save-buffers-kill-emacs]"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-goto-mail-header-separator ()
  "Goto start of body after `mail-header-separator'.
If not found, goto `point-max'."
  (goto-char (point-min))
  (or (and (boundp 'mail-header-separator)
           (re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "\n")
            nil t))
      (re-search-forward "^--text.*\n" nil t)
      (goto-char (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-goto-body-start ()
  "Go to start of body, skipping all headers."
  (goto-char (point-min))
  (or (re-search-forward "\n\n" nil t)
      (re-search-forward "^[ \t]*$" nil t)
      (goto-char (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-pop-to-buffer (buffer)
  "Show buffer in full window."
  (pop-to-buffer buffer)
  (delete-other-windows))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-mail-mode-prepare ()
  "Prepare current buffer for bug email."
  (message "tinycygwin-bug-report-mail-mode-prepare: external %s"
           tinycygwin--external-call-flag)
  (tinycygwin-external-with
   (message "tinycygwin-bug-report-mail-mode-prepare: buffer %s"
            (buffer-name))
   (make-local-variable 'tinycygwin--external-call-flag-value)
   ;;  Save the current state permanently to this buffer
   (setq tinycygwin--external-call-flag-value
         tinycygwin--external-call-flag)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-mail-mode-buffer (name)
  "Return emty buffer with NAME and prepare it."
  (tinycygwin-user-mail-address-set)
  (tinycygwin-user-full-name-set)
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (tinycygwin-bug-report-mail-mode-prepare))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-finish-message ()
  "Show message until user starts doing something."
  (let* ((msg1   (tinycygwin-message-mode-help-simple))
         (msg2   (substitute-command-keys
                  (concat
                   "Write description and send with "
                   "\\[message-send-and-exit] "
                   "("
                   "Help \\[describe-mode] "
                   (tinycygwin-external-with
                    (tinycygwin-msg-exit-emacs))
                   ")")))
         (list (list msg1
                     msg2)))
    (while (and (sit-for 0.2)
                (not (input-pending-p))
                (message (car list))
                ;;  Rotate list of messages
                (let ((tmp (pop list)))
                  (setq list (append list (list tmp))))
                (sit-for 5)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-finish ()
  "Finish mail buffer preparations."
  (tinycygwin-bug-report-mail-mode-subject-fix)
  (set-buffer-modified-p nil)
  ;; (setq buffer-auto-save-file-name nil)
  (tinycygwin-goto-body-start)
  (tinycygwin-non-expert-with
   (tinycygwin-bug-report-mail-mode-finish-message)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-update-file-autoloads (dir)
  "Generate autoloads in DIR."
  (let ((default-directory dir)
	(generated-autoload-file
	 (concat (file-name-as-directory dir)
		 "tinycygwin-autoloads.el")))
    (unless (file-exists-p generated-autoload-file)
      (message "TinyCygwin: [WARN] %s does not exist. Creating it."
               generated-autoload-file)
      (with-temp-buffer
        (insert (format ";; Emacs autoload file. File was generated %s\n\n"
                        (format-time-string
                         "%Y-%m-%d %H:%M UTC" nil 'utc)))
        (write-region (point-min) (point-max) generated-autoload-file)))
    (dolist (file (directory-files dir nil "\\.el$" 'abs))
      (unless (string-match "loaddefs\\|autoload\\|[#~]" file)
        (update-file-autoloads file)))
    (let ((buffer (get-file-buffer generated-autoload-file)))
      (when buffer
        (with-current-buffer buffer
          (save-buffer))
        (kill-buffer buffer)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-update-file-autoloads-batch (&optional dir force)
  "Update autoloads in batch mode. Argument in command line is DIR. FORCE."
  (interactive "DAutoload dir to update: ")
  (unless dir
    (setq dir (pop command-line-args-left))
    (setq force t))
  (unless dir
    ;; Self generate error for command line ...
    (message "TinyCygwin: From what directory to generate autoloads?")
    (error 'tinycygwin-update-file-autoloads-batch))
  (message "TinyCygwin: Generating all autoloads in %s" dir)
  (tinycygwin-update-file-autoloads dir))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-smtp-available-p (&optional force)
  "Open smtå to see if mail is available. The value is cached unless FORCE."
  (when (file-directory-p "/cygdrive/c") ;; Try only in Windows
    (let ((checked (get 'tinycygwin-smtp-available-p 'checked))
          (status  (get 'tinycygwin-smtp-available-p 'status))
          proc)
      (when (or force
                (null checked))
        (message "Tinycygwin: Checking SMTP server... ")
        (setq status
              (condition-case error
                  (setq proc
                        (open-network-stream
                         "tinycygwin-smtp"
                         "*process-tinycygwin-smtp*"
                         "localhost"
                         25))
                (error
                 nil)
                (t
                 (delete-process proc)
                 t))))
      (message "Tinycygwin: Checking SMTP server... Done.")
      (put 'tinycygwin-smtp-available-p 'checked t)
      (put 'tinycygwin-smtp-available-p 'status status))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-smtp-setup-error ()
  "Check that Emacs can in theory send mail.
Call `error' if there are problems."
  (unless (or (getenv "SMTPSERVER")
              (tinycygwin-smtp-available-p))
    (read-string
     "TinyCygwin: [ERROR] No SMTPSERVER defined <press return> ")
    (pop-to-buffer (get-buffer-create "*tinycygwin.el help*"))
    (erase-buffer)
    (insert "\
\[Email configuration error]

To activate Emacs email support for ISP's mailserver, following
lines are needed in personal startup file ~/.emacs

\(setenv \"SMTPSERVER\" \"your.isp.example.net\")
\(setq smtpmail-debug-info           t)
\(setq smtpmail-local-domain         nil)
\(setq send-mail-function            'smtpmail-send-it)
\(setq message-send-mail-function    'smtpmail-send-it)
\(setq gnus-agent-send-mail-function 'smtpmail-send-it)

After you have done these changes, the setings are active next time Emacs
is started. Here are few inportant Eamcs commands to help you:

  C-x C-c   Quit
  C-g       Abort current (active) operation, like prompt input

  C-x o     Go to (o)ther visible window
  C-x C-f   Open file for editing
  C-x C-s   Save current file
  C-x C-b   Show buffer list (C-x o to it and press RET to select)

  C-k       Kill line (at the same time copies it)
  C-y       Yank, paste
")
    (when (y-or-n-p "Open ~/.emacs for editing? ")
      (pop-to-buffer (find-file-noselect "~/.emacs")))
    (message
     "Unable continue before before working email. %s"
     (tinycygwin-msg-exit-emacs))
    'error))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-buffer-name-temp (name)
  "Return temporary buffer for NAME"
  (format "*tinycygwin %s*" name))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-file-buffer (file)
  "Return buffer for FILE."
  (when file
    (let* ((name   (tinycygwin-buffer-name-temp
                    (file-name-nondirectory file)))
           (buffer (get-buffer name)))
      (unless buffer
        (with-current-buffer (setq buffer (get-buffer-create name))
          (insert-file-contents-literally file)
          (setq buffer-read-only t)))
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-first-directory (list)
  "Return Cygwin package documentation root directory"
  (dolist (dir list)
    (when (file-directory-p dir)
      (return dir))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-path-to-cygwin (path)
  "Chnage Win32 path to Cygwin path."
  (let ((root tinycygwin--root-dir))
    (when (and path
               (stringp path))
      (replace-regexp-in-string root "" path))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-path (path)
  "Convert Cygwin PATH, like /, to OS absolute patch like C:/cygwin.
 Trailing slash is stripped."
  (when path
    (let ((root tinycygwin--root-dir)
	  ret)
      (setq ret
            (cond
             ((string= root "/")
              path) ;; Native Cygwin Emacs
             ((string-match "^/cygdrive" path)
              path)
             ((string-match "^/\\(.*\\)" path)
              (format
               "%s%s"
               (file-name-as-directory root)
               (match-string 1 path)))
             (t
              path)))
      ;; Delete trailing slash.
      (if (string-match "^\\(.+\\)/$" ret)
          (match-string 1 ret)
        ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-path-doc-cygwin ()
  "Return Cygwin package documentation root directory"
  (tinycygwin-path
   (tinycygwin-first-directory tinycygwin--path-doc-cygwin-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-path-doc-root ()
  "Return Cygwin package documentation root directory"
  (tinycygwin-path
   (tinycygwin-first-directory tinycygwin--path-doc-root-list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-string-delete-newlines (string)
  "Delete newlines from STRING."
  (replace-regexp-in-string "[\r\n]" "" string))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-turn-on-emacs-debug ()
  "Activate Emacs debug."
  (interactive)
  (setq debug-on-error t)
  (if (boundp 'stack-trace-on-error) ;; XEmacs
      (setq stack-trace-on-error t))
  (if (boundp 'debug-ignored-errors)
      (setq debug-ignored-errors nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-font-lock-keywords (&optional uninstall)
  "Add color support to various log files by setting
`font-lock-keywords'."
  (let ((today   "xxx") ;; (ti::date-standard-rfc-regexp "mon-date"))
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
                0 tinycygwin--warn-face)
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
               (concat "connection attempt" );  See "iplogger" package
	       0 'tinycygwin--warn-face)
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
               0 'tinycygwin--warn-face)
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
               0 'tinycygwin--warn-face)
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
            (and (boundp 'global-font-lock-mode)
                 global-font-lock-mode)
            (font-lock-mode 1))
        (setq font-lock-keywords keywords))))))

;;}}}
;;{{{ WNPP

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-list-match (regexp list)
  "Check if REGEXP matched LIST of strings."
  (dolist (str list)
    (when (string-match regexp str)
      (return str))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-file-setup-hint-p (list)
  "Check if setup.hint is included in LIST of files."
  (tinycygwin-list-match (regexp-quote "setup.hint") list))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-call-process (binary buffer arg-list)
  "Call BINARY with list of ARGS and print output to current buffer or BUFFER."
  (apply 'call-process
         binary
         nil
         (or buffer (current-buffer))
         nil
         arg-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-wnpp-main-interactive ()
  "Ask the type of request for WNPP package.
References:
  `tinycygwin--menu-wnpp'
  `tinycygwin--menu-wnpp-selected'"
  (tinycygwin-menu-call-with
   'tinycygwin--menu-wnpp
   tinycygwin--menu-wnpp-selected))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-compose (to-list subject)
  "Compose new mail using TO-LIST and SUBJECT."
  ;;  mail-setup: (to subject in-reply-to cc replybuffer actions)
  (mail-setup (or (pop to-list) "")
              (or subject "")
              nil nil nil nil)
  (when to-list ;; More recipients
    (unless (message-fetch-field "CC")
      ;;  This creates field as well
      (message-goto-cc))
    (let (newline
          address)
      (while to-list
        (setq newline (if (cdr to-list)
                          ",\n  "
                        "")
              address (pop to-list))
        (when (stringp address)
          (insert address newline)))))
  (tinycygwin-bug-report-mail-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-wnpp-mail-generic
  (&optional prefix description info file-list)
  "Compose ITP message with optional subject PREFIX and DESCRIPTION.
If there is package information, it is in INFO."
  (interactive)
  (let* ((subject (tinycygwin-bug-report-mail-subject-compose
                   description prefix "wnpp" ))
         (name    (tinycygwin-bug-report-email-buffer-name nil prefix))
         (buffer  (tinycygwin-bug-report-mail-mode-buffer name)))
    (tinycygwin-debug
     (message
      (concat
       "TinyCygwin: WNPP generic prefix [%s] description [%s] info: %s "
       "expert: %s")
      prefix description info tinycygwin--expert-flag))
    (tinycygwin-not-modified-with
     (tinycygwin-pop-to-buffer buffer)
     (erase-buffer)
     (tinycygwin-bug-report-mail-compose
      (list tinycygwin--email-cygwin-apps-list)
      subject)
     (when info
       (tinycygwin-bug-report-mail-insert-details-package
        info))
     (tinycygwin-bug-report-mail-insert-files
      file-list
      (tinycygwin-expert-with
       'as-is))
     (tinycygwin-bug-report-mail-mode-finish))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-wnpp-main
  (request-type &optional package desc info file-list)
  "Submit REQUEST-TYPE against WNPP pseudo package.
WNPP is used for requesting to be a new maintainer and
for taking maintenance of other packages.

REQUEST-TYPE can be symbol:

  'package 'orphan 'adopt or 'new.

Optional PACKAGE in question, DESC string and package INFO.

References:
  `tinycygwin--menu-wnpp'."
  (interactive (list (tinycygwin-package-wnpp-main-interactive)))
  (let ((type (if (symbolp request-type)
                  (symbol-name request-type)
                request-type)))
    (tinycygwin-debug
     (message
      (concat
       "TinyCygwin: WNPP main type [%s] package [%s] desc [%s] info: %s"
       " files: %s")
      type package desc info file-list))
    (cond
     ((not (stringp type)))
     ((string= type "package")
      (or desc
          (tinycygwin-non-expert-with
           (setq desc (read-string "[ITP] Package name -- description: "))))
      (unless (tinycygwin-file-setup-hint-p file-list)
        (let ((file (read-file-name
                     "[ITP] setup.hint file to include: "
                     nil
                     nil
                     'match)))
          (cond
           ((string-match "setup\\.hint" file))
           ;; Ok.
           ((or (string= "" file)
                (file-directory-p file)) ;; User pressed return. No file.
            (if (y-or-n-p (format "Include file %s. Are you sure? "
                                  (file-name-nondirectory file)))
		(push file file-list))))))
      (tinycygwin-package-wnpp-mail-generic "ITP" desc nil file-list))
     ((string= type "new")
      (let ((desc  (read-string "[RFP] Package name -- description: ")))
        ;;  Check status database
        (tinycygwin-package-wnpp-mail-generic "RFP" desc)))
     ((string= type "orphan")
      (or package
          (setq package
                (tinycygwin-package-read-name
                 "[ORPHAN] package: ")))
      (or info
          (setq info (tinycygwin-package-info-main package)))
      (tinycygwin-package-wnpp-mail-generic "ORPHAN" package info))
     ((string= type "adopt\\|rfa")
      (or package
          (setq package
                (tinycygwin-package-read-name
                 "[ADOPT/RFA] package: ")))
      (or info
          (setq info (tinycygwin-package-info-main package)))
      (tinycygwin-package-wnpp-mail-generic "RFA" package info))
     (t
      ;;  Nothing to do
      nil))))

;;}}}
;;{{{ Cygcheck

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-insert-os-linux ()
  "Insert result of uname -a to buffer."
  (call-process "uname"
                nil                     ;infile
                (current-buffer)        ;buffer
                nil                     ;display
                "-a"))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-insert-os-cygwin ()
  "Insert result of cygcheck -s -v -r to buffer."
  (let ((cmd tinycygwin--bin-cygcheck))
    (when cmd
      (call-process cmd
                    nil                  ;infile
                    (current-buffer)     ;buffer
                    nil                  ;display
                    "-s"
                    "-v"
                    "-r"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-buffer (&optional force)
  "Load `tinycygwin--file-cygcheck' or FORCE (re)generating it."
  (let* ((file   tinycygwin--file-cygcheck)
         (bin    tinycygwin--bin-cygcheck)
         (name   tinycygwin--buffer-cygcheck)
         (buffer (or (get-buffer name)
                     (get-file-buffer file))))
    (cond
     ((and (null force)
           buffer)
      buffer)
     ((and (null force)
           (file-exists-p file))
      (with-current-buffer (get-buffer-create name)
        (tinycygwin-not-modified-with
         (insert-file-contents-literally file))
        (setq buffer-read-only t)
        (current-buffer)))
     (t
      (when tinycygwin--bin-cygcheck
        (message "Please wait, reading sysinfo (cygcheck)... ")
        (with-current-buffer (get-buffer-create name)
          (tinycygwin-not-modified-with
           (tinycygwin-call-process
            bin
            (current-buffer)
            (split-string "-s -v -r"))
           (write-region (point-min) (point-max) file)
           (setq buffer-read-only t)
           (setq buffer (current-buffer))))
        (message "Please wait, reading sysinfo (cygcheck)... Done.")
        buffer)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-sysinfo-with 'lisp-indent-function 0)
(defmacro tinycygwin-sysinfo-with (&rest body)
  "Run BODY at sysinfo buffer."
  `(let ((buffer (tinycygwin-sysinfo-buffer)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-version-cygcheck (program)
  "Search PROGRAM and it's version number from cygcheck listing."
  (when program
    (tinycygwin-sysinfo-with
     (goto-char (point-min))
     (if (not (re-search-forward
               "Cygwin Package Information" nil t))
         (tinycygwin-debug
          (message
           "TinyCygwin: [ERROR] tinycygwin-sysinfo-program; %s"
           "no start tag found"))
       (let (version)
         ;; Cygwin Package Information
         ;; Last downloaded files to: D:\ftp\cygwin\install\cygwin-install
         ;; Last downloaded files from: http://mirrors.sunsite.dk/cygwin
         ;;
         ;; Package                 Version
         ;;
         ;; _update-info-dir        00227-1

         (when (re-search-forward
                (format "^%s[^ \t\r\n]*[ \t]+\\([0-9][^ \t\r\n]+\\)"
                        program)
                nil t)
           (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-version-syscall-parse ()
  "Parse version number from currnt buffer."
  (let (ret)
    (goto-char (point-min))
    (when (re-search-forward "\\([0-9]\\.[0-9.]*[0-9]\\)" nil t)
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-executable-find (program)
  "Search for PROGRAM exactly from `exec-path'."
  (let ((regexp (format "^%s$" program))
        list
        ret)
    (dolist (path exec-path)
      (when (file-directory-p path)
	(setq list (directory-files path 'full regexp))
	(when (and list (eq 1 (length list)))
	  (setq ret (car list))
	  (return))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-version-syscall-call (program &optional version-arg)
  "Search PROGRAM and its version number by calling shell.
Optional VERSION-ARG defaults to --version."
  (when program
    (with-temp-buffer
      (let ((bin (executable-find program))
	    (args (list (or version-arg "--version"))))
        (unless bin
          ;;  This was not a .exe program, but a shell script or something
          ;;  like that. E.g. 'automake' is in /usr/bin/automake
          (let ((found (tinycygwin-executable-find program))
		shell)
            (when (and found
                       (setq shell (executable-find "sh")))
              (setq bin  shell
                    args (list
                          "-c"
                          (format "%s %s"
                                  (tinycygwin-path-to-cygwin found)
                                  (or version-arg "--version")))))))
        (when bin
          (tinycygwin-call-process
           bin
           (current-buffer)
           args)
          (tinycygwin-sysinfo-version-syscall-parse))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-bundle-item (item &optional call-shell)
  "Return version information of ITEM in `tinycygwin--sysinfo-program-list'.
If optional CALL-SHELL is non-nil, then query the information from
shell (more reliable, but slower).

Return:
  '((PROGRAM VERSION)
    ...)."
  (let (ver
        list)
    (dolist (bin (cadr (assq item tinycygwin--sysinfo-program-list)))
      (when (setq ver
                  (if call-shell
                      (tinycygwin-sysinfo-version-syscall-call bin)
                    (tinycygwin-sysinfo-version-cygcheck bin)))
        (push (list bin ver) list)))
    ;; Preserve order
    (reverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-kernel-version ()
  "Return OS details."
  (with-temp-buffer
    (tinycygwin-sysinfo-insert-os-linux)
    (goto-char (point-min))
    (when (re-search-forward "[0-9][.0-9]+-[-.0-9]+" nil t)
      ;; Linux host 2.6.18-1-686 #1 SMP Sat Oct 21 17:21:28 UTC 2006 i686 GNU/Linux
      (match-string 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-linux-arch ()
  "Return OS details."
  (with-temp-buffer
    (insert (tinycygwin-sysinfo-os-kernel-version))
    (goto-char (point-min))
    ;;  2.6.18-1-686
    (when (re-search-forward "[0-9]+$" nil t)
      (match-string 0))))
;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-windows ()
  "Return Windows OS details."
  ;; This is the first line in there
  ;; Windows 2000 Professional Ver 5.0 Build 2195 Service Pack 4
  (tinycygwin-sysinfo-with
   (goto-char (point-min))
   (when (re-search-forward "^Windows.*[^\r\n]" nil t)
     (match-string 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-cygwin-dll-all ()
  "Return cygwin1.dll details."
  ;;Cygwin DLL version info:
  ;;    DLL version: 1.5.7
  ;;    DLL epoch: 19
  ;;    DLL bad signal mask: 19005
  ;;    DLL old termios: 5
  ;;    DLL malloc env: 28
  ;;    API major: 0
  ;;    API minor: 109
  ;;    Shared data: 3
  ;;    DLL identifier: cygwin1
  ;;    Mount registry: 2
  ;;    Cygnus registry name: Cygnus Solutions
  ;;    Cygwin registry name: Cygwin
  ;;    Program options name: Program Options
  ;;    Cygwin mount registry name: mounts v2
  ;;    Cygdrive flags: cygdrive flags
  ;;    Cygdrive prefix: cygdrive prefix
  ;;    Cygdrive default prefix:
  ;;    Build date: Fri Jan 30 19:32:04 EST 2004
  ;;    CVS tag: cr-0x9e
  ;;    Shared id: cygwin1S3
  (tinycygwin-sysinfo-with
   (goto-char (point-min))
   (when (re-search-forward "^[ \t]*Cygwin DLL.*info:" nil t)
     (let ((beg (line-beginning-position)))
       (when (re-search-forward "^[ \t]*$" nil t)
         (buffer-substring beg (line-beginning-position)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-cygwin-dll-info ()
  "Return DLL information.

Return:

'((version \"1.5.7\")
  (api     \"0.109\")  ;; Major.Minor
  (cvs-tag     \"cr-0x9e\"))"
  (let ((str (tinycygwin-sysinfo-os-cygwin-dll-all)))
    (when str
      (let (ret)
        (when (string-match "CVS tag:[ \t]*\\(.*[^ \t\r\n]\\)" str)
          (push (list 'cvs-tag (match-string 1 str)) ret))
        (when (string-match "DLL version:[ \t]*\\(.*[^ \t\r\n]\\)" str)
          (push (list 'version (match-string 1 str)) ret))
        (when (string-match "API major:[ \t]*\\(.*[^ \t\r\n]\\)" str)
          (let ((major (match-string 1 str)))
            (when (string-match "API minor:[ \t]*\\(.*[^ \t\r\n]\\)" str)
              (push (list 'api (format "%s.%s"
                                       major
                                       (match-string 1 str)))
                    ret))))
        ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-sysinfo-os-cygwin-dll-version-string  ()
  "Make DLL version information string."
  (let ((info (tinycygwin-sysinfo-os-cygwin-dll-info)))
    (when info
      (let ((ver (nth 1 (assq 'version info)))
	    (api (nth 1 (assq 'api info)))
	    (cvs (nth 1 (assq 'cvs-tag info))))
        (concat
         (if ver
             ver
           "")
         (if api
             (concat " api " api)
           "")
         (if cvs
             (concat " cvs " cvs))
         " (cygwin1.dll)")))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-sysinfo-os-cygwin ()
  "Return Cygwin OS information."
  (tinycygwin-sysinfo-os-cygwin-dll-version-string))

;;}}}
;;{{{ Cygwin Packages

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-buffer (package &optional load)
  "Return buffer for PACKAGE. Optionally LOAD to Emacs if no buffer found."
  (tinycygwin-file-buffer
   (tinycygwin-package-info-path-doc-cygwin-package package)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-package-buffer-with 'edebug-form-spec '(body))
(put 'tinycygwin-package-buffer-with 'lisp-indent-function 1)
(defmacro tinycygwin-package-buffer-with (package &rest body)
  "In Cygwin documentation buffer for PACKAGE, run BODY."
  `(let ((buffer (tinycygwin-package-buffer package)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-package-buffer-search 'lisp-indent-function 2)
(defmacro tinycygwin-package-buffer-search (package regexp &optional subexp)
  "Search Cywin PACKAGE documentation for REGEXP and return SUBEXP or 0."
  `(progn
     (tinycygwin-package-buffer-with package
                                     (goto-char (point-min))
                                     (when (re-search-forward ,regexp nil t)
                                       (match-string (or ,subexp 0))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-wnpp-p (package)
  "Chekc if PACKAGE is the wnpp presude package."
  (and (stringp package)
       (string-match "^wnpp" package)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-pseudo-p (package)
  "Chekc if PACKAGE is the generic bug package."
  (and (stringp package)
       (string-match "^bug-generic" package)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-itp-p (package)
  "Chekc if PACKAGE is ITP, intent to package."
  (and (stringp package)
       (string-match "^wnpp" package)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-special-p (package)
  "Chekc if PACKAGE is special. I.e. does not exist, but has other meaning."
  (and (stringp package)
       (or (tinycygwin-package-pseudo-p package)
           (tinycygwin-package-wnpp-p package))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-1 (field info &optional string-p)
  "Return FIELD from INFO, optionally as empty STRING-P."
  (if string-p
      (or (nth 1 (assoc field info)) "")
    (nth 1 (assoc field info))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-cdr (field info)
  "Return cdr FIELD from INFO."
  (cdr-safe (assoc field info)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-ignore (info)
  "Return the \"Ignore-errors\' field content."
  (tinycygwin-package-info-field-1 "Ignore-errors" info))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-status (info)
  "Return the \"Status\' field content."
  (tinycygwin-package-info-field-1 "Status" info))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-package (info)
  "Return the \"Package\' field content."
  (tinycygwin-package-info-field-1 "Package" info))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-version (info)
  "Return the \"Package\' field content."
  (tinycygwin-package-info-field-1 "Version" info))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-release (info)
  "Return the \"Package\' field content."
  (tinycygwin-package-info-field-1 "Release" info))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-field-name-ok-p (string)
  "Return non-nil if STRING is valid package field name."
  (not (string-match "^ignore" string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-readme-package-file-list (&optional regexp)
  "Return ist of absolute paths to <package>.README or REGEXP files."
  (let ((dir (tinycygwin-path-doc-cygwin))
        ret)
    (when (and dir
               (file-directory-p dir))
      (directory-files
       dir
       'absolute
       regexp))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-readme-package-name-list (&optional add-list)
  "Return list of all installed packages in `tinycygwin-path-doc-cygwin'.
Optinally add ADD-LIST to the returned list."
  (let (name
        ret)
    (dolist (file (tinycygwin-readme-package-file-list "\\.README"))
      (setq name (replace-regexp-in-string
                  ;;  Not all package have version number
                  ;;  => cygserver.README
                  "\\(-[0-9].*\\)\\|\\.README.*"
                  ""
                  (file-name-nondirectory file)))
      (push name ret))
    (if add-list
        (setq ret (append add-list ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-install-database-buffer ()
  "Return `tinycygwin--file-install-db' buffer."
  (tinycygwin-file-buffer tinycygwin--file-install-db))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-install-database-buffer-with 'lisp-indent-function 0)
(defmacro tinycygwin-install-database-buffer-with (&rest body)
  "Run BODY in `tinycygwin--file-install-db' buffer."
  `(let ((buffer (tinycygwin-install-database-buffer)))
     (when buffer
       (with-current-buffer buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-database-buffer-insert ()
  "Insert `tinycygwin--file-install-db'."
  (let ((file (tinycygwin-path tinycygwin--file-install-db)))
    (if (file-exists-p file)
        (insert-file-contents file)
      (message "TinyCygwin: Not found %s" file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-database-buffer-package-info (package)
  "Return PACKAGE install.db information."
  (tinycygwin-install-database-buffer-with
   (let ((case-fold-search t)
	 (regexp (format "^%s +.+" package)))
     (goto-char (point-min))
     (when (re-search-forward regexp nil t)
       (match-string 0)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-database-buffer-package-list ()
  "Return list of installed packages"
  (tinycygwin-install-database-buffer-with
   (let (list)
     (goto-char (point-min))
     (search-forward "INSTALLED.DB" nil t) ;; Skip this
     (while (re-search-forward "^[^ \t\r\n]+" nil t)
       (push (match-string 0) list))
     list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-string-split (string)
  "Return package, version, release from STRING like foo-1.2.0-1.tar.bz2."
  (when (or (string-match
             ;; foo-1.2.0-1.tar.bz2
             "^\\([a-z-]+[0-9]?\\)-\\([0-9]+[0-9.-]*[0-9]\\)-\\(.+\\)"
             string)
            (string-match
             ;; libxxx1-1.3-2
             "^\\([a-z-]+[0-9]?\\)-\\([0-9]+[0-9.-]*[0-9]\\)\\(.*\\)"
             string)
            ;; a2ps-4.13
            (string-match
             "^\\([a-z0-9]+[0-9]?\\)-\\([0-9]+[0-9.-]*[0-9]\\)\\(.*\\)"
             string))
    (let ((name (match-string 1 string))
	  (ver  (match-string 2 string))
	  rel)
      (setq string (match-string 3 string)) ;; The rest
      ;;  Release cannot be more than 2 numbers.
      (when (string-match "^\\([0-9][0-9]?\\)\\([^0-9]+\\|$\\)" string)
        (setq rel (match-string 1 string)))
      (list name ver rel))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-string-package (string)
  "Return version from STRING."
  (nth 0 (tinycygwin-package-info-string-split string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-string-version (string)
  "Return version from STRING."
  (nth 1 (tinycygwin-package-info-string-split string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-string-release (string)
  "Return release number from STRING."
  (nth 2 (tinycygwin-package-info-string-split string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-path-doc-cygwin (string)
  "Return Cygwin documentation file path for STRING like foo-1.2.0-1.tar.bz2."
  (multiple-value-bind (package version release)
      (tinycygwin-package-info-string-split string)
    (if (not version)
        (message "TinyCygwin: Can't parse doc dir from %s" string)
      (setq release release) ;; Byte compiler silencer
      (let ((dir (tinycygwin-path-doc-root)))
        (format "%s/Cygwin/%s-%s.README" dir package version)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-path-doc-cygwin-package (package)
  "Return Cygwin documentation file path for PACKAGE like 'foo'."
  (let ((dir (tinycygwin-path-doc-cygwin)))
    (when (and dir
               (file-directory-p dir))
      (let ((list (directory-files
                   dir
                   'absolute
                   (format "^%s-.*README" package))))
        (when (eq (length list) 1)
          (car list))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-name (package)
  "Return PACKAGE name from `tinycygwin--path-doc-cygwin-list'.
This is ismilar function to `tinycygwin-database-buffer-package-info'."
  (let ((file (tinycygwin-package-info-path-doc-cygwin-package package)))
    (when file
      (replace-regexp-in-string
       "\\.README"
       ""
       (file-name-nondirectory file)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-port-maintainer-1 ()
  "Search current buffer for maintainer."
  (goto-char (point-min))
  (or (and (re-search-forward
            "Cygwin port.*maintained.*:[ \t]*\\(.+[^ \t\r\n]\\)" nil t)
           (match-string 1))
      (progn
        (goto-char (point-max))
        (when (search-backward "@" nil t)
          (cond
           ((search-backward ":" (line-beginning-position) t)
            ;; Maintainer: ...
            (re-search-forward "[: \t]+" nil t)
            (buffer-substring (point) (line-end-position)))
           (t
            (goto-char (line-beginning-position))
            (if (looking-at "[ \t]*\\(.+@.+[^ \t\r\n]\\)")
                (match-string 1))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-port-maintainer (package)
  "Return Cygwin port maintainer for PACKAGE."
  (tinycygwin-package-buffer-with package
                                  (tinycygwin-package-info-port-maintainer-1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-bug-report (package)
  "Return bug report address for PACKAGE"
  (let* ((upstream-info
          (tinycygwin-package-info-upstream-contacts package))
         (str (if upstream-info
                  (nth 1 (assq 'bugs upstream-info)))))
    str))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-maintainer (package)
  "Return author or maintainer of the PACKAGE."
  (let* ((upstream-info
          (tinycygwin-package-info-upstream-contacts package))
         (str (if upstream-info
                  (nth 1 (or (assq 'maintainer upstream-info)
                             (assq 'author upstream-info))))))
    str))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycygwin-package-info-macro 'lisp-indent-function 2)
(defmacro tinycygwin-package-info-macro (package check-variable &rest body)
  "PACKAGE. If CHECK-VARIABLE is set, then allow running BODY."
  `(let ((special (tinycygwin-package-special-p package)))
     (when (and
            (not special)
            ,check-variable)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-port-maintainer-maybe (package)
  "Only in certain conditions return package mailtainer's email aadress.
PACKAGE is not special and
`tinycygwin--package-maintainer-email-include' is set."
  (tinycygwin-package-info-macro
   package tinycygwin--package-maintainer-email-include
   (tinycygwin-package-info-port-maintainer package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-maintainer-maybe (package)
  "Only in certain conditions return package mailtainer's email aadress.
PACKAGE is not special and
`tinycygwin--package-maintainer-email-include' is set."
  (tinycygwin-package-info-macro
   package tinycygwin--package-upstream-email-include
   (tinycygwin-package-info-port-maintainer package)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-package-info-port-maintainer-list (&optional display)
  "Generate list of all packages and their maintainers. Optionally DISPLAY."
  (interactive (list t))
  (let* ((buffer (get-buffer-create
                  tinycygwin--buffer-maintainer-list))
         (dir    (tinycygwin-path-doc-cygwin))
         package
         maintainer)
    (with-current-buffer buffer
      (erase-buffer))
    (with-temp-buffer
      (dolist (file (directory-files dir 'abs "\\.README$"))
        (erase-buffer)
        (insert-file-contents-literally file)
        (setq maintainer
              (or (tinycygwin-package-info-port-maintainer-1)
                  "ERROR, not found; file syntax unknown"))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert (format "%-30s %s\n"
                          (replace-regexp-in-string
                           "\\.README"
                           ""
                           (file-name-nondirectory file))
                          maintainer)))))
    (if display
        (pop-to-buffer buffer))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-homepage (package)
  "Return homepage of PACKAGE."
  (tinycygwin-package-buffer-search
   package
   ".*homepage:[ \t\r\n]*\\(.+[^ \t\r\n]\\)"
   1))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-info-heading-block (package heading)
  "Return Heading: block for Cygwin PACKAGE documentation."
  (tinycygwin-package-buffer-search
   package
   ;;  Grab all indented lines after HEADING
   (format  "%s.*\\(\r?\n[ \t]+.*\\)+" heading)
   0))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-heading-value
  (package heading header &optional subexp)
  "Read PACKAGE and position to HEADING regexp and read HEADER SUBEXP
Like if HEADER were 'Upstream contact' and HEADING were
'Author: +\(.+\)' from text:

    Upstream contact:
      Author: Foo Bar <foo@example.com>
      Bugs: Foo Bar <foo@example.com>"
  (let ((str (tinycygwin-package-info-heading-block
              package
              heading)))
    (when str
      (with-temp-buffer
        (insert str)
        (goto-char (point-min))
        (goto-char (line-end-position)) ;; Past heading
        (when (re-search-forward header nil t)
          (match-string (or subexp 0)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-first-email (package)
  "Return first email address from PACKAGE."
  (tinycygwin-package-buffer-search
   package
   "[^: \t\r\n][^:@\r\n]+@.+[^ \t\r\n]"
   0))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-upstream-contacts (package)
  "Return upstream contact addresses.

Return:

  '((author address)
    (bugs   address))

Notice that the values may be missing if no such fields were found."
  (let ((fields
         '((bugs       "Bugs:[ \t]*\\(.+\\)")
           (maintainer "Maintainer:[ \t]*\\(.+\\)")
           (author     "Author:[ \t]*\\(.+\\)")))
        ret
        val)
    (dolist (elt fields)
      (multiple-value-bind (tag regexp) elt
        (when (setq val
                    (tinycygwin-package-info-heading-value
                     package
                     "^upstream"
                     regexp
                     1))
          (push (list tag val) ret))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-status-cygwin (package)
  "Return Cygwin PACKAGE details.
'((\"Package\" \"foo\")
   (\"Status\"  \"installed\")
   (\"Version\" \"1.13\")
   (\"Release\" \"1\")
   ...)"
  (let* ((db     (tinycygwin-database-buffer-package-info package))
         (cygdoc (unless db
                   (tinycygwin-package-info-name package)))
         version
         release
         ret)
    (flet ((push-ret (tag value function)
                     (when (and value
                                (setq value (funcall function value)))
                       (push (list tag value) ret))))
      (cond
       ((not (or db cygdoc))
        (setq ret
              (list
               (list "Package" package)
               (list "Status" "not-installed"))))
       (db
        ;; keychain keychain-1.9-1.tar.bz2 0
        (multiple-value-bind (name package dummy)
            (split-string db)
          (setq ret
                (list (list "Package" name)
                      (list "Name"    package)
                      (list "Status"  "installed")))
          (push-ret "Version"
                    package
                    'tinycygwin-package-info-string-version)
          (push-ret "Release"
                    package
                    'tinycygwin-package-info-string-release)))
       (cygdoc
        (setq ret
              (list
               (list "Package" (replace-regexp-in-string
                                "-[0-9].*"
                                ""
                                package))
               (list "Name"    cygdoc)
               (list "Status" "installed-3rd-party")))
        (push-ret "Version"
                  cygdoc
                  'tinycygwin-package-info-string-version)
        (push-ret "Release"
                  cygdoc
                  'tinycygwin-package-info-string-release)))
      (tinycygwin-debug
       (message "TinyCygwin: [DEBUG] pkg-status-cygwin '%s'"
                (prin1-to-string ret)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-status-bug-generic ()
  "Return Generic bug package status values."
  '(("Package" "")
    ("Status" "")
    ("Ignore-errors" "files email")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-package-status-wnpp ()
  "Return WNPP package status values."
  '(("Package" "wnpp")
    ("Ignore-errors" "files email")))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-status-main (package)
  "Return PACKAGE details.

One PACKAGE name is special: \"bug-generic\".

This package does not exist, but informas, that you want to fill in a
genereic bug report concerning issues in Cygwin. If you're porting
software to Cygwin, but can't get it cmpiled, you may want to talk
o the author of the code. Creating a generic bug report, help you and the
author to keep track of discussion."
  (cond
   ((tinycygwin-package-pseudo-p package)
    (tinycygwin-package-status-bug-generic))
   ((tinycygwin-package-wnpp-p package)
    (tinycygwin-package-status-wnpp))
   (t
    (tinycygwin-package-status-cygwin package))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-read-name (&optional prompt list add-list)
  "Read installed package name with optional PROMPT.
The optional LIST is full ask lisk. ADD-LIST is added to the default
package ask list."
  (message "Wait, building package list...")
  (completing-read
   (or prompt
       "Cygwin package name (TAB to complete): ")
   (mapcar (lambda (x)
             (cons x 'dummy))
           (or list
               ;; tinycygwin-database-buffer-package-list
               (tinycygwin-readme-package-name-list add-list)))
   nil
   'match))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-package-readme-find-file (package)
  "Open PACKAGE*.README."
  (interactive
   (list (tinycygwin-package-read-name
          "Find File (Cygwin package README): ")))
  (when package
    (let ((file (tinycygwin-package-info-path-doc-cygwin-package
                 package)))
      (unless file
        (error "TinyCygwin: [ERROR] Cannot find %s*.README"
               package))
      (find-file file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-package-info-main (package)
  "Get PACKAGE information. See`tinycygwin-package-status'."
  (when (stringp package)
    (tinycygwin-string-trim package)
    (when (string-match "[^ \t\r\n]" package)
      (tinycygwin-package-status-main package))))

;;}}}
;;{{{ Bug reporting interface

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-type-standard-p (type)
  "Check if bug TYPE is standard bug."
  (or (null type)
      (and (stringp type)
           (string= "standard" type))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-system-info-os-architecture ()
  "Read architecture."
  (cond
   ((eq tinycygwin--os-type 'cygwin)
    (tinycygwin-sysinfo-os-cygwin))
   ((eq tinycygwin--os-type 'linux)
    (tinycygwin-sysinfo-os-linux-arch))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-system-info-os-version ()
  "Read Cygwin version number."
  (cond
   ((eq tinycygwin--os-type 'cygwin)
    (tinycygwin-sysinfo-os-windows))
   ((eq tinycygwin--os-type 'linux)
    (tinycygwin-sysinfo-os-kernel-version))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-system-info-locale ()
  "Get locale information."
  (let ((list
	 '("LC_ALL"
	   "LANG"
	   "LC_MESSAGES"
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
(defun tinycygwin-bug-system-info-os-main ()
  "Return OS information. Something like below.
Release: 1.5.7
Architecture: i386
Kernel:
Locale: LANG=en_US, LC_CTYPE=en_US."
  (let ((kernel   (tinycygwin-bug-system-info-os-architecture))
        (release  (tinycygwin-bug-system-info-os-version))
        (locale   (tinycygwin-bug-system-info-locale)))
    (format "\
Release: %s
Kernel: %s
Locale: %s
"
            (or release "")
            (or kernel  "")
            (or locale  ""))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-severity ()
  "Select bug severity."
  (setq tinycygwin--menu-severity-selected nil)
  (while (null tinycygwin--menu-severity-selected)
    (ti::menu-menu 'tinycygwin--menu-severity)
    (unless tinycygwin--menu-severity-selected
      (message "TinyCygwin: Please select severity.")
      (sit-for 1)))
  tinycygwin--menu-severity-selected)

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-exit ()
  "Ask to exit Emacs unless `tinycygwin--expert-flag' is non-nil."
  (tinycygwin-non-expert-with
   (tinycygwin-external-with
    (when (y-or-n-p "Exit Emacs now? ")
      (kill-emacs)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-attach-file (file)
  "Attach file \"as is\" to current point."
  (unless (bolp)
    (beginning-of-line))
  (unless (looking-at "^[ \t]*$")
    (insert "\n"))
  (insert (format "\n%s\n" (tinycygwin-mail-attachment-tag file)))
  (tinycygwin-not-modified-with
   (insert-file-contents-literally file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-files (list &optional type)
  "Attach LIST of file to the end of current buffer.

Optional TYPE

  'as-is        Add as plain text.
  'mime         Add as a mime attachment.
  nil           Ask user wat to do with files that are not binaries."
  (when list
    (unless (eq major-mode 'message-mode)
      (error
       "TinyCygwin: Can't add attachments. Not in `message-mode'"))
    (let (mml-type
          description)
      (goto-char (point-max))
      (tinycygwin-not-modified-with
       (insert "\n"))
      (dolist (file list)
        (goto-char (point-max))
        (cond
         ((not (file-exists-p file))
          (let ((msg (format "[ERROR] Not exists. Can't attach file %s "
                             file)))
            (message msg)
            (tinycygwin-non-expert-with
             (sit-for 2))))
         ((tinycygwin-file-binary-p file)
          (mml-attach-file file type description))
         ((or (eq type 'mime)
              (and (null type)
                   (y-or-n-p
                    (format "Insert as a MIME attachment/as is [%s]? "
                            (file-name-nondirectory file)))))
          (tinycygwin-expert-with
           (setq mml-type (mml-minibuffer-read-type file)))
          (mml-attach-file file mml-type description))
         (t
          (tinycygwin-bug-report-mail-attach-file file)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-details-bundle ()
  "Include `tinycygwin-sysinfo-bundle-item' details."
  (let (done
        info)
    (dolist (bundle '(devel-tools lang))
      (when (setq info
                  (mapconcat (lambda (x)
                               (multiple-value-bind (bin ver) x
                                 (format "%s %s" bin ver)))
                             (tinycygwin-sysinfo-bundle-item
                              bundle (not (eq tinycygwin--os-type 'cygwin)))
                             ", "))
        (unless done
          (insert "\n-- Other package information\n")
          (setq done t))
        (insert (format "Info-Pkg-%s: %s\n"
                        (symbol-name bundle)
                        info))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-environment ()
  "Insert details from `tinycygwin--sysinfo-environment-list'"
  (let (done
        info)
    (dolist (var tinycygwin--sysinfo-environment-list)
      (when (setq info (getenv var))
	(unless done
	  (insert "\n-- Environment information\n")
	  (setq done t))
        (insert (format "%s: %s\n" var info))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-details-upstream (package)
  "Insert PACKAGE upstream information at point."
  (when package
    (let ((upstream-info
           (tinycygwin-package-info-upstream-contacts package)))
      (dolist (info upstream-info)
        (multiple-value-bind (type email) info
          (insert (format "Upstream-%s: %s"
                          (symbol-name type)
                          email)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-details-sysinfo ()
  "Insert system information at point."
  (insert "\n\n-- System Information\n"
          (tinycygwin-bug-system-info-os-main))
  (tinycygwin-bug-report-mail-insert-details-bundle))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-details-package
  (info &optional severity)
  "Insert package INFO details with optional bug SEVERITY level."
  (goto-char (point-min))
  (tinycygwin-goto-mail-header-separator)
  (let ((status  (tinycygwin-package-info-field-status  info))
	(package (tinycygwin-package-info-field-package info))
	(version (tinycygwin-package-info-field-version info))
	(release (tinycygwin-package-info-field-release info)))
    (dolist (elt (list
                  (list "Package"   package)
                  (list "Version"   (concat
                                     version
                                     (if release
                                         (concat "-" release)
                                       "")))
                  (list "Status"    status)
                  (list "Severity" (or severity "normal"))))
      (multiple-value-bind (field value) elt
        (insert (format "%s: %s\n"   field (or value "")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-insert-details-main
  (info &optional severity)
  "Insert Details for package INFO with optional bug SEVERITY level."
  (tinycygwin-bug-report-mail-insert-details-package info severity)
  (tinycygwin-bug-report-mail-insert-details-sysinfo)
  (tinycygwin-bug-report-mail-insert-environment)
  (tinycygwin-bug-report-mail-insert-details-upstream
   (cadr (assoc "Package" info))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-report-message-mark-external ()
  "Define `tinycygwin--external-call-flag' local to buffer."
  (when tinycygwin--external-call-flag
    (set (make-local-variable 'tinycygwin--external-call-flag)
         tinycygwin--external-call-flag)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-message-send-actions ()
  "Arrange `message-send-actions'."
  ;;  Will be buffer local. See message.el
  (when (boundp 'message-send-actions)
    (push '(progn
             (message "Bug report sent. %s"
                      (or (tinycygwin-external-with
                           (tinycygwin-msg-exit-emacs))
                          "")))
          message-send-actions)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-message-mode-font-lock-keywords ()
  "Return correct `font-lock-keywords'."
  (if (tinycygwin-window-system)
      tinycygwin--message-mode-font-lock-keywords-window-system
    tinycygwin--message-mode-font-lock-keywords-non-window-system))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-font-lock ()
  "Activate `font-lock-mode' with custom settings."
  (let ((keys (tinycygwin-message-mode-font-lock-keywords)))
    (when (fboundp 'font-lock-mode)
      (when (and (boundp 'message-font-lock-keywords)
                 (null (get 'message-mode
                            'tinycygwin-font-lock-keywords)))
        (require 'font-lock) ;; force to define variables
        (tinycygwin-message-mode-faces)
        (unless tinycygwin--original-font-lock-keywords
          (set (make-local-variable 'tinycygwin--original-font-lock-keywords)
               font-lock-keywords))
        (make-local-variable 'message-font-lock-keywords)
        (setq message-font-lock-keywords
              (append message-font-lock-keywords keys))
        ;;  Delete "Catch all" header regexp whic overrides all other
        ;;  faces.
        ;;  ("^\\([A-Z][^: \n      ]+:\\)..."
        ;;    (1 'message-header-name-face)
        ;;    (2 'message-header-other-face nil t))
        (setq
         message-font-lock-keywords
         (delete-if (lambda (x)
                      (let* ((str (prin1-to-string x))
                             (status
                              (string-match
                               "other-face\\|cited-text" str)))
                        status))
                    message-font-lock-keywords))
        ;; (put 'message-mode
        ;;      'font-lock-defaults
        ;;      '(message-font-lock-keywords t))
        (put 'message-mode
             'tinycygwin-font-lock-keywords
             keys))
      (font-lock-mode 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode ()
  "Turn on mail mode for current buffer."
  (set (make-local-variable 'message-cite-prefix-regexp)
       "^\\([|>] *\\)+")
  (message-mode)
  (tinycygwin-non-expert-with
   (tinycygwin-bug-report-mail-mode-font-lock))
  (tinycygwin-external-with
   (tinycygwin-bug-report-message-send-actions))
  (auto-save-mode -1)
  (when buffer-file-name
    ;;  Under windows, file #*message*# is invalid, change it.
    (setq buffer-file-name
          (replace-regexp-in-string "[*]" "" buffer-file-name))
    ;;  Check for working Emacs/Gnus, if not, then cancel save file name
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-directory-p dir)
        (message-disassociate-draft)
        (setq buffer-file-name nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-body-header-value (header)
  "Return HEADER value from body of email."
  (save-current-buffer
    (tinycygwin-goto-mail-header-separator)
    (when (re-search-forward
           (format
            "^%s:[ \t]*\\([^ \t\r\n]+\\)" header)
           nil t)
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-high-priority-p (severity)
  "Check SEVERITY is high priority (Severity > important)."
  (and (stringp severity)
       (string-match "critical\\|grave\\|serious" severity)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycygwin-bug-mail-attached-patch-p ()
  "Check if Patch has been attached."
  (save-current-buffer
    (tinycygwin-goto-mail-header-separator)
    (re-search-forward
     "\\(attachment:\\|filename=\\).*.\\(diff\\|patch\\)"
     nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-subject-tags ()
  "Add subject tags [patch] etc. if needed"
  (let (value
	tag)
    (save-current-buffer
      (when (and (setq value (tinycygwin-bug-report-mail-body-header-value
                              "Severity"))
                 (tinycygwin-bug-high-priority-p value))
        (setq tag (format "[%s]" value)))
      (when (tinycygwin-bug-mail-attached-patch-p)
        (setq tag (concat (or tag "")  "[patch]"))))
    tag))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-subject-split (str)
  "Split subject STR on ':' or if it does not exist return BUG ID."
  (when (stringp str)
    (let (prefix
	  rest
	  list)
      (cond
       ((and (string-match ":" str)
             (setq list (split-string str ":")))
        (setq prefix (pop list)
              rest   (mapconcat 'identity list " ")))
       ((string-match "^\\([^#\r\n]+#[0-9T]+\\)\\(.*\\)" str)
        (setq prefix (match-string 1 str)
              rest   (match-string 2 str))))
      (if rest
          (list prefix rest)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-set-header (header value)
  "Replace HEADER with value."
  (save-current-buffer
    (goto-char (point-min))
    (let ((end "\n"))
      (when (re-search-forward
             (concat "^" header ":")
             nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (setq end ""))
      (insert (format "%s: %s%s" header value end)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-mode-subject-fix ()
  "Add tags to subject."
  (let ((tag (tinycygwin-bug-report-mail-mode-subject-tags)))
    (when tag
      ;; message-field-value
      (let ((subject (message-fetch-field "Subject")))
        (when subject
          (multiple-value-bind (prefix rest)
              (tinycygwin-bug-report-mail-mode-subject-split subject)
            (when (and rest
                       ;; Does not have tags already?
                       (not (string-match "\\[" rest)))
              (tinycygwin-bug-report-mail-mode-set-header
               "Subject"
               (format "%s: %s%s" prefix tag rest)))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-subject-compose
  (&optional subject package type)
  "Compose bug SUBJECT and optionally include PACKAGE name with tYPE."
  (format (if package
              "%s%s%s"
            "%s%s%s")
          (tinycygwin-bug-report-email-prefix type)
          (if (and (stringp package)
                   (not (string= "" package)))
              (format " %s: " package)
            ": ")
          (or subject "")))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-subject-interactive (&optional package)
  "Compose Bug subject. Optional argument PACKAGE is added to Subject."
  (let ((subject (or (tinycygwin-non-expert-with
                      (read-string "Cygwin bug subject: "))
                     "")))
    (tinycygwin-bug-report-mail-subject-compose subject package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-compose-email (&optional address-list)
  "Compose list of email addresses with optional ADDRESS-LIST."
  (let* ((choices       (append
                         (tinycygwin-email-choice-list)
                         address-list))
         (choice-alist  (mapcar (lambda (x)
                                  (cons x "dummy"))
                                choices))
         list
         email)
    (while (string-match
            "@"
            (setq email
                  (completing-read
                   (format
                    "[%d]Bug email (TAB choices; empty to quit): "
                    (length list))
                   choice-alist)))
      (pushnew email list :test 'string=))
    (reverse list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-compose-interactive
  (buffer to-list package-name info &optional file-list)
  "Compose bug report interactively and display BUFFER.
Send mail to TO-LIST with PACKAGE-NAME INFO.
Attach FILE-LIST."
  (tinycygwin-pop-to-buffer buffer)
  ;;  For inspection in `tinycygwin-bug-report-message-send-actions'.
  (tinycygwin-bug-report-message-mark-external)
  (erase-buffer)
  (let ((subject (tinycygwin-bug-report-mail-subject-interactive
                  package-name)))
    (tinycygwin-not-modified-with
     (tinycygwin-bug-report-mail-compose
      to-list
      subject)
;;;     (when info
       (message "Please wait, reading sysinfo...")
       (goto-char (point-max))
       (tinycygwin-bug-report-mail-insert-details-main
        info
        (tinycygwin-non-expert-with
         (tinycygwin-bug-severity)))
       (message "Please wait, reading sysinfo... Done.")
;;;       )
     (tinycygwin-bug-report-mail-insert-files file-list)
     (tinycygwin-bug-report-mail-mode-finish)))
  (run-hooks 'tinycygwin--bug-report-mail-hook))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-main-new-bug
  (buffer package info &optional email-list file-list)
  "Generate new bug report for PACKAGE and with INFO.
Optionally to EMAIL-LIST."
  (let ((ignore (tinycygwin-package-info-field-ignore info)))
    (setq email-list
          (cond
           (tinycygwin--expert-flag
            (append email-list
                    (tinycygwin-email-choice-list package)))
           ((null ignore)
            (tinycygwin-bug-report-mail-compose-email
             email-list))))
    (tinycygwin-bug-report-mail-compose-interactive
     buffer
     email-list
     package
     info
     file-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-main-2 (info &optional file-list)
  "See `tinycygwin-bug-report-mail-main' for INFO FILE-LIST."
  (let* ((status        (tinycygwin-package-info-field-status info))
         (ignore        (tinycygwin-package-info-field-ignore info))
         (package       (tinycygwin-package-info-field-package info))
         (special       (tinycygwin-package-special-p package))
         (maintainer    (tinycygwin-package-info-port-maintainer-maybe
                         package))
         (author        (tinycygwin-package-info-maintainer package))
         (name          (tinycygwin-bug-report-email-buffer-name
                         package))
         email-list)
    (tinycygwin-debug
     (message "TinyCygwin: [DEBUG] bug-1 pkg %s maintainer %s maint %s"
              package maintainer author))
    ;; ............................................. no maintainer ...
    (when (and package
               tinycygwin--package-maintainer-email-include
               (not special)
               (not maintainer))
      (read-string
       (format
        (concat
         "No maintainer email in %s*.README. "
         "Consider reporting this bug as well <RET to continue>.")
        package)))
    (cond
     ;; ...................................... previous bug report ...
     ((and (get-buffer name)
           (null (y-or-n-p
                  "Delete previously composed bug report? ")))
      (tinycygwin-pop-to-buffer (get-buffer name)))
     ;; ........................................... new bug report ...
     (t
      (when author
        (push (format
               "maintainer of the package %s - %s"
               (or package "")
               (car (tinycygwin-user-mail-address-fix
                     (list author))))
              email-list))
      (when maintainer
        (push (format
               "Cygwin port maintainer %s - %s"
               (or package "")
               (car (tinycygwin-user-mail-address-fix
                     (list maintainer))))
              email-list))
      (tinycygwin-bug-report-mail-main-new-bug
       (tinycygwin-bug-report-mail-mode-buffer name)
       package
       info
       email-list
       file-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type-standard (info &optional file-list)
  "INFO is alist of package's attributes.
Optional TYPE is bug type.
FILE-LIST are files to attach.

An example bug report would look something like:

    To: <Cygwin package maintainer>
    Subject: Cygwin-bug#20040121T1030 foo: <subject of the bug>
    --text follows this line--
    Package: foo
    Version: 0.35-10
    Status: installed-3rd-party
    Severity: wishlist

    <bug report body described here>

    -- System Information
    Release: 1.5.7 api 0.109 cvs cr-0x9e
    Kernel: Windows 2000 Professional Ver 5.0 Build 2195 Sp4
    Locale: LC_ALL=en_US

    -- Other package information
    Info-Pkg-devel-tools: gcc 3.3.1-3, make 3.80-1, libtool 1.5b-1
    Info-Pkg-lang: perl 5.8.2-1, python 2.3.3-1, ruby 1.8.1-1

    -- Environment information
    CYGWIN: tty ntsec binmode smbntsec

For lisp calls, The INFO variables is like:

    '((\"Status\" ...)
      (\"Package\" ...)
      ...)."
  (let* ((status        (tinycygwin-package-info-field-status info))
         (package       (tinycygwin-package-info-field-package info))
         (special       (tinycygwin-package-special-p package)))
    (tinycygwin-debug
     (message "TinyCygwin: [DEBUG] bug-1 info %s\n"
              (prin1-to-string info))
     (message "TinyCygwin: [DEBUG] bug-1 package '%s'"
              (prin1-to-string package))
     (message "TinyCygwin: [DEBUG] bug-1 file-list '%s'"
              (prin1-to-string file-list)))
    (cond
     ((null info)
      (message
       (format "No package INFO available to send a bug report. %s"
               (if (tinycygwin-external-with
                    (tinycygwin-msg-exit-emacs))
                   ""))))
     ((and (not special)
           (string-match "not-installed" (or status "")))
      (cond
       ((y-or-n-p
         (format "Packege [%s] is not installed. Select other package? "
                 (or package "")))
        (let ((package (tinycygwin-package-read-name)))
          (when (string-match "[^ \t\r\n]" (or package ""))
            ;; Phew, this is recursive call to back to us.
            (tinycygwin-bug-report-mail-package package file-list))))
       (t
        (or (tinycygwin-bug-report-exit)
            (tinycygwin-external-with
             (message (tinycygwin-msg-exit-emacs)))))))
     (t
      (tinycygwin-bug-report-mail-main-2 info file-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type-update-xxx-todo (info)
  "Request update of package whose INFO is old."
  (let ((status (assoc "Status" info)))
    (when status
      (setq info (delete status info)))
    (push '("Status" "old") info)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type-wnpp (type &optional info file-list)
  "WNPP type request with INFO and FILE-LIST."
  (tinycygwin-package-wnpp-main
   type
   nil
   nil
   nil
   file-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type-upstream (info &optional file-list)
  "Send message to upstream maintainer with package INFO and FILE-LIST."
  (let* ((package (tinycygwin-package-info-field-package info))
         (name    (tinycygwin-bug-report-email-buffer-name package))
         (buffer  (tinycygwin-bug-report-mail-mode-buffer name)))
    (message "Sending mail to UPSTREAM has not been implemented yet.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type-update (info &optional file-list)
  "Update request."
  (let* ((package (tinycygwin-package-info-field-package info))
         (name    (tinycygwin-bug-report-email-buffer-name package))
         (buffer  (tinycygwin-bug-report-mail-mode-buffer name)))
    (if buffer
        (pop-to-buffer buffer)
      (tinycygwin-pop-to-buffer (get-buffer-create name))
      (let ((subject (tinycygwin-bug-report-mail-subject-compose
                      "[UPDATE] Newer package available" package))
            (to-list (list
                      tinycygwin--email-cygwin-apps-list
                      (tinycygwin-package-info-port-maintainer-maybe
                       package))))
        (tinycygwin-not-modified-with
         (tinycygwin-bug-report-mail-compose
          to-list
          subject)
         (tinycygwin-bug-report-mail-insert-files file-list)
         (tinycygwin-bug-report-mail-mode-finish)))
      (run-hooks 'tinycygwin--bug-report-mail-hook))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-type (&optional type info file-list)
  "Determine correct TYPE of message and act accordingly. This the main bug
report handling semaphore, which delegates the task to correct
function. INFO is alist of package's attributes. FILE-LIST contains
files to attach."
  (let ((package (tinycygwin-package-info-field-package info)))
    (tinycygwin-debug
     (message "TinyCygwin: Mail-type type [%s] info: %s files: %s"
              type info file-list))
    (cond
     ((and (stringp package)
           (string= package "wnpp"))
      (tinycygwin-bug-report-mail-type-wnpp type info file-list))
     ((and info
           (tinycygwin-bug-type-standard-p type))
      (tinycygwin-bug-report-mail-type-standard info file-list))
     ((and info
           (stringp type)
           (string= type "update"))
      (tinycygwin-bug-report-mail-type-update info file-list))
     ((and info
           (stringp type)
           (string= type "upstream"))
      (tinycygwin-bug-report-mail-type-upstream info file-list))
     (t
      (error
       (concat "TinyCygwin: [ERROR] Insufficient information [%s, %s, %s, %s] "
               "Perhaps you meant `wnpp'?")
       type package info file-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-ask-type ()
  "Ask type of bug.
References:
  `tinycygwin--menu-bug-classification'
  `tinycygwin--menu-bug-classification-selected'"
  (tinycygwin-menu-call-with
   'tinycygwin--menu-bug-classification
   tinycygwin--menu-bug-classification-selected))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-ask-package (&optional add-list)
  "Return package infor by asking and completing name with ADD-LIST."
  (let ((package (tinycygwin-package-read-name
                  "[TinyCygwin] Report bug to package: "
                  nil
                  add-list)))
    (if (or (not (stringp package))
            (string= "" package))
        (setq package "bug-generic"))
    (tinycygwin-package-info-main package)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-main (&optional info type file-list)
  "Report bug interactive by mail. TYPE is bug type.
INFO is alist of package's attributes. FILE-LIST are files to attach."
  (interactive
   (if (tinycygwin-smtp-setup-error)
       (list nil)
     (let ((info (tinycygwin-bug-report-ask-package
                  '("bug-generic" "wnpp"))))
       (list
        info
        (if (tinycygwin-package-wnpp-p
             (tinycygwin-package-info-field-package info))
            (tinycygwin-package-wnpp-main-interactive)
          (tinycygwin-bug-report-ask-type))))))
  (tinycygwin-debug
   (message "TinyCygwin: mail-mail info %s" (prin1-to-string info)))
  (let ((error (unless (interactive-p)
                 (tinycygwin-smtp-setup-error))))
    (unless error
      (tinycygwin-debug
       (message "TinyCygwin: mail-main %s" (prin1-to-string info)))
      (tinycygwin-bug-report-mail-type type info file-list))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycygwin-reportbug ()
  "Fully interactive Cygwin bug reporting entrance.
See function `tinycygwin-bug-report-mail-main' which contains more
detailled description."
  (interactive)
  (call-interactively 'tinycygwin-bug-report-mail-main))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-mail-package (package &optional type file-list)
  "Interface to `tinycygwin-bug-report-mail-main' when PACKAGE is known.
Optional TYPE of bug and possibly attach FILE-LIST."
  (tinycygwin-bug-report-mail-main
   (tinycygwin-package-status-main package)
   type
   file-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-batch-include-tag-buffers ()
  "Tag all Emacs files as include files to bug report
This function must not be called by any other than function
`tinycygwin-bug-report-mail-batch'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((file (buffer-file-name))
            (name (buffer-name)))
        (when file
          (unless (tinycygwin-bug-report-include-buffer-name-p name)
            (setq buffer-read-only t)
            (rename-buffer
             (tinycygwin-bug-report-include-buffer-name name))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-batch-setup-general ()
  "Define Emacs settings for batch bug reporting.
This function is not called if `tinycygwin--expert-flag' is non-nil.
the setting include e.g.

  (setq sentence-end-double-space nil)
  (setq colon-double-space        nil)
  (setq mouse-yank-at-point       t)
  (setq use-dialog-box            nil)
  (setq-default fill-column       75)
  (setq isearch-lazy-highlight    t)
  (setq query-replace-highlight   t)
  (setq search-highlight          t)
  (setq track-eol                 t)
  ..."
  (modify-syntax-entry ?-  "w")         ; part of word
  (modify-syntax-entry ?\t " ")         ; Treat TABs as spaces.
  (setq sentence-end-double-space nil)
  (setq colon-double-space        nil)
  (setq smtpmail-debug-info       t)
  (setq mouse-yank-at-point       t)
  (setq use-dialog-box            nil)
  (setq-default fill-column       75)
  (setq isearch-lazy-highlight    t)
  (setq query-replace-highlight   t)
  (setq search-highlight          t)
  (setq track-eol                 t)
  (setq-default indent-tabs-mode  nil) ;; Always spaces, more secure in email
  (add-hook 'debugger-mode-hook 'toggle-truncate-lines)
  (when (fboundp 'minibuffer-electric-default-mode)
    (minibuffer-electric-default-mode  1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinycygwin-bug-report-batch-setup-smtp ()
  "If SMTPSERVER is set, arrange `smtpmail-send-it' to send mail."
  (let ((server (getenv "SMTPSERVER")))
    (when server
      (setq smtpmail-debug-info           t)
      (setq smtpmail-local-domain         nil)
      (setq send-mail-function            'smtpmail-send-it)
      (setq message-send-mail-function    'smtpmail-send-it)
      (setq gnus-agent-send-mail-function 'smtpmail-send-it))))

;;; ----------------------------------------------------------------------
;;; This function is called from external program 'cygbug' which see.
;;;
;;;###autoload
(defun tinycygwin-bug-report-batch (&optional package)
  "This function is called from external script. DO NOT USE.
Do not call this from lisp in any circumstances or it will cause
Emacs to exit."
  (let ((tinycygwin--external-call-flag  t)
        (tinycygwin--debug  tinycygwin--debug) ;; Make local copy
        (tinycygwin--expert-flag
         (if (boundp 'tinycygwin--command-switch-expert)
             tinycygwin--command-switch-expert))
        (tinycygwin--external-email-address
         (if (boundp 'tinycygwin--command-switch-email)
             tinycygwin--command-switch-email))
        (file-list
         (if (boundp 'tinycygwin--command-switch-files)
             tinycygwin--command-switch-files))
        (type
         (if (boundp 'tinycygwin--command-switch-type)
             tinycygwin--command-switch-type)))
    ;;  Enable these commands
    (put 'narrow-to-region 'disabled nil)
    (put 'eval-expression  'disabled nil)
    (put 'downcase-region  'disabled nil)
    (put 'upcase-region    'disabled nil)
    ;;  Make answering questions easier, like "Really exit
    ;;  "Emacs" when message is being composed still.
    (defalias 'yes-or-no-p 'y-or-n-p)
    (or package
        (setq package
              (when (boundp 'tinycygwin--command-switch-package)
                ;;  Because this is called from external script,
                ;;  be cautious and activate debug to pinpoint
                ;;  possible errors.
                (setq tinycygwin--debug t)
                tinycygwin--command-switch-package)))
    (unless package
      (error "** [ERROR] Need Cygwin PACKAGE name in order to report bug."))
    (when (and (null type)
               (tinycygwin-package-wnpp-p package))
      (let ((loop t)
            selection)
        (while loop
          (cond
           ((setq type (tinycygwin-package-wnpp-main-interactive))
            (setq loop nil))
           ((when (y-or-n-p
                   "Select other package bug/WNPP again (C-g to abort)? ")
              (setq selection
                    (tinycygwin-package-read-name
                     "[TinyCygwin] Report bug to package: "
                     nil
                     '("bug-generic")))
              (cond
               ((string-match "^[ \t\r\n]*$" selection)
                (message "Hm, nothing selected. Trying agian...")
                (sit-for 1))
               (t
                (setq package selection
                      loop    nil)))))))))
    (tinycygwin-bug-report-batch-setup-smtp)
    (tinycygwin-non-expert-with
     (tinycygwin-bug-report-batch-setup-general)
     (when nil ;; disabled for now
       ;;  Print clear message to Emacs novices.
       (message
        (substitute-command-keys
         (concat
          "[INFO] Exit \\[save-buffers-kill-emacs]  "
          "Abort \\[keyboard-quit]  "
          "")))
       (sit-for 1.9)))
    (tinycygwin-bug-report-batch-include-tag-buffers)
    (tinycygwin-bug-report-mail-package package type file-list)))

;;}}}

;; Auto-created functions
(tinycygwin-install-severity-functions)
(tinycygwin-install-bug-classification-functions)
(tinycygwin-install-message-mode)

(provide   'tinycygwin)
(run-hooks 'tinycygwin--load-hook)

;;; tinycygwin.el ends here
