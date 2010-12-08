;;; tinygnus.el --- Gnus Plug-in. Additional functions. UBE fight etc.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1997-2010 Jari Aalto
;; Keywords:        extensions
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

;;; Install:

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.gnus startup file. This file should be loaded only after gnus
;;  startup.
;;
;;      (require 'tinygnus)
;;
;;  Alternatively you can add this autoload code to integrate the package
;;  with Gnus startup:
;;
;;      (add-hook 'gnus-startup-hook '(lambda () (require 'tinygnus)))

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;
;;  Preface, Sep 1997
;;
;;      I haven't have a chance to try the new Gnus for a long time
;;      because the envinronment didn't have Emacs 19.34. And when the
;;      sysadm installed it, I started slowly moving from my dear RMAIL
;;      (which I had configured to work very well) to the bold and beatiful
;;      Gnus. I had also started using procmail and subscribed to many
;;      mailing lists, so the only choice to manage all my mail was
;;      Gnus. Here you find some functions that I found useful.
;;
;;  Overview of features
;;
;;      o   Automatic reload of files when entring group with SPACE
;;          (mimic  Newsgroup behavior)
;;      o   You can have compresses .eld file. If you compress .gnus to
;;          .gnus.gz then the .eld files will be compressed too to .eld.gz
;;          This saves you disk space in low quota account.
;;
;;      o   Fast read group by showing only unread (newly arrived)
;;          articles. Speeds up reading your mail groups.
;;      o   Show immediately dormants in non-nntp groups. Some people
;;          use dormant mark ? in their private mail groups as `todo'
;;          and to be able to see those todo articles immediately saves
;;          you 5x time, when you don't have to separately limit to
;;          dormants.
;;
;;      o   Ready %uX user function that you can use in the *-line-format
;;          strings.
;;      o   Group User format function: "expiry", Tell the expiry value
;;          for the group and varaious other values.
;;      o   Group User format function: "comment", Tell the group comment.
;;      o   Group User format function: "tick",    Tell if group has ticks.
;;
;;      o   Send UBE complaint to all postmasters in Received headers.
;;          The ip addresses of postmasters are nslookup verified. You
;;          can select either individual article or process mark multiple
;;          articles.
;;
;;  Url pointers
;;
;;      o   Procmail information can be found at
;;          http://www.procmail.org/ and http://pm-doc.sourceforge.net/
;;      o   Gnus can be found at http://www.gnus.org/
;;
;;  Fighting against UBE messages
;;
;;      Please visit http://spam.abuse.net/ for up to date information.
;;      Other good sites: http://spamcop.net/ and http://www.spamcop.com/
;;
;;      [2000-11] Automatically generated Gnus blacklist by Brian Edmonds
;;      is at http://www.gweep.bc.ca/~edmonds/usenet/index.html
;;
;;      Many of us receive UBE (Unsolicited Bulk Email) and if we don't do
;;      anything to stop them, then the practice comes approved de facto
;;      internet convention. It is important that you complaint about every
;       piece of UBE you may receive, your vote counts and it will also
;;      give you satisfaction to know that most of the postmasters kick off
;;      the idiot in the other end of the wire. There are two functions
;;      in this module:
;;
;;          tinygnus-article-ube-send-to-postmasters    U      UBE
;;          tinygnus-summary-ube-send-to-postmasters    C-c'u  send UBE
;;
;;      The first function is fully interactive and it reads the current
;;      active article and composes `forward' message to all postmasters
;;      mentioned in the `received' header chain. Before sending you have
;;      a chance to reformat the article anyway you like.
;;
;;      The latter function is useful to batch send complaints: you
;;      process mark(#) articles in summary buffer, Hit C-c'u, and each
;;      article is processes and complaint is sent to postmasters. Before
;;      sending message, the function asks confirmation. You can suppress
;;      the confirmation with `C-u' prefix argument. _Note_: It may take some
;;      time to compose all complaints if you have marked many articles,
;;      because parsing *Received:* headers and checking them with `nslookup'
;;      may be slow. If you use `procmail' or Gnus split methods to flter
;;      your UBE mail to one single newsgroup, say `junk.ube', Then you can
;;      mark all messages in the newsgroup and handle all the UBE you have
;;      received in a whip.
;;
;;      Why is the complaint message sent to *postmaster* address, while
;;      recent sites have set up an *abuse* addresses as well? That's
;;      simply because RFC822 requires that each site must have postmaster
;;      account and you should be able to count on delivery to that address.
;;
;;        [RFC822] (...) standard specifies a single, reserved mailbox address
;;        (local-part) which is to be valid at each site. Mail sent to that
;;        address is to be routed to a person responsible for the site's
;;        mail system or to a person with responsibility for general site
;;        operation. The name of the reserved local-part address is:
;;        Postmaster
;;
;;      From the standard, "postmaster@domain" is required to be valid.
;;      Some domains have opened specific addresses where you can send
;;      these complains, e.g. abuse@aol.com, fraud@uu.net. If you know a
;;      specific address where to send the complaint, update
;;      `tinygnus--ube-abuse-account-table'
;;
;;  Gathering information from articles (e.g. URLs)
;;
;;      If you read group that has very high traffic, and don't have to
;;      time to read all articles, but you're are still interested in
;;      seeing if there are any good urls mentioned, you can use function
;;      below. It will not record duplicate urls, only unique ones.
;;
;;          C-c ' g u       tinygnus-summary-gather-urls
;;
;;      Function steps through all marked articles (Mark command in summary
;;      buffer is in M P submap), examines each message and puts the urls
;;      in `tinygnus--output-buffer'. You can clear and display with
;;      commands:
;;
;;          C-c ' g d       tinygnus-summary-gather-display
;;          C-c ' g c       tinygnus-summary-gather-clear
;;
;;  Configuring the user format functions
;;
;;      Before you load this file, it might be good to configure variable
;;      `tinygnus--uff-table' so that it won't clash the definitions of
;;      your own `gnus-user-format-function-X'. If you load this file
;;      without modifying the table, it will replace all existing functions
;;      according to that table. In case you don't know what this is all
;;      about, go to Emacs info pages `C-h' `i', go to Gnus node, press
;;      's' to search for 'Summary Buffer Lines' RET. Look at the specifier
;;      %uX, where X is anything.
;;
;;  Miscellaneous commands
;;
;;      `tinygnus-make-group-from-dir-nndoc' can be used to generate all nndoc
;;      groups fast from bunch of mailboxes that you dropped to some
;;      directory. You might have downloaded archives of mailing lists
;;      sorted by month and year and you want to genrate Gnus groups for
;;      them. This is it.
;;
;;  Nnml handling commands
;;
;;      TinyGnus is mainly designed for nnml backend. Gnus can be easily
;;      used for mailing lists; Gnus customisations; moving groups from one
;;      place to another. In TinyGnus there are some exotic functions that
;;      may prove handy when you have the same need. See below.
;;
;;      `tinygnus-make-group-nnml-from-dir'. If you have nnml
;;      groups in ~/Mail; this function can create the equivalent nnml
;;      groups to your gnus easily. Give a REGEXP to match directories to
;;      include for group creation (E.g. "list\." for all
;;      mailing list list.* directories)
;;
;;      `tinygnus-make-group-from-dir-nnml-procmail-spool'. A procmail
;;      (never mind if you don't know what that is); is a tool to deliver
;;      each incoming mail to correct mailbox as it arrives and it is very
;;      efective for filtering mailing lists. Procmail users have to
;;      reserve separate directory for these mailboxes; usually
;;      ~/Mail/spool/ and all files end to *.spool ( list.ding.spool,
;;      work.lab.spool ... ). Keeping Gnus aware of the mailboxes in the
;;      ~/Mail/spool would normally be manual work, but this function can
;;      create the nnml groups for you for each found spool file from the
;;      directory.
;;
;;      `tinygnus-group-parameter-mailing-list'. Use this to read the last
;;      nnml mail from the directory and suggest an email address from
;;      From, To, Cc, Reply-To to be inserted into the group parameter
;;      `to-list'. When you start a fresh Gnus and create nnml groups, which
;;      are mailing lists (e.g. from old mail); the tedious part is to
;;      recover the "list status" of the group and insert correct `to-list'
;;      field into each group. With This function; just mark the groups
;;      where you want to add the parameter and you're set in few minutes.
;;
;;  Enhanced Gnus functions
;;
;;       Enter group in Topic mode with SPC
;;
;;      Function `gnus-topic-read-group' is enhanced to maximize speed of
;;      reading new articles. Normally when you enter Group, gnus shows
;;      unread and ticked articles, but if you have any previously ticked
;;      articles in group, making the summary buffer is slow. If we ignore
;;      the ticked articles and display only the newly arrived, unread,
;;      articles, the time to generate Summary buffer is far less. If you
;;      have many private mail,work, mailing list groups, this saves you
;;      from lot of time to be able to track new messages.
;;
;;       Show dormants immediately in non-nntp groups
;;
;;      Function `gnus-summary-limit-children' is enhanced so that it will
;;      include dormant articles in Summary creation in non-nntp groups.
;;      Some people found out that the dormant mark ? is handy in mail
;;      groups to mean `todo' or `see this later' or `urgent'. Normally
;;      gnus treats all groups the same: nntp or private mail makes no
;;      difference. However the dormant mark can be used to mean different
;;      meaning in nntp group and non-nntp groups and this enchancement
;;      does just that. You get fast Summary with dormants now and you
;;      don't need to separately limit the buffer to show the dormants. To
;;      turn off this feature, set `tinygnus--show-dormants' to nil.
;;
;;  Compressed Gnus newsrc files
;;
;;      Having a unix account that has unlimited disk space is very rare
;;      and for that reason being able to keep files in compressed format s
;;      preferrable to avoid going over Quota with message "Quota limit
;;      exceed, remove nnnK withing N days...".
;;
;;      Gnus has compression support for Group files, but not for the
;;      bloating .newsrc or .eld files. Gawk. They consume your disk real
;;      fast because they become big in no time.
;;
;;      For that reason there is included adviced Gnus code that
;;      automatically starts using compressed startup files if your
;;      `gnus-init-file' has extension `.gz'. Changing from normal init
;;      file to compressed one is easy:
;;
;;      .   gzip your .newsrc and .eld files
;;      .   (setq tinygnus--z ".gz")
;;      .   M-x load-library RET tinygnus RET
;;
;;      If you later want to restore this settings: Unzip, do (setq
;;      tinygnus--z nil), and reload the package. But if you're low
;;      of quota, you propably do the reverse operation.
;;
;;       Gnus version note
;;
;;      This file installs only features to Gnus 5.8.2 (Emacs 20.5) and
;;      if you're using newer gnus version the advice code is not activated.
;;      Using this package should be safe with any existing Gnus version
;;      later than 5.8.2
;;
;;  Line format example for *Group* buffer
;;
;;      The personal Group buffer line can be configured as follows. If you
;;      try this with very old Gnus, drop away that fancy ~(cut 6) and use
;;      plain %d.
;;
;;          (setq gnus-topic-line-format "%i%(%{%n%}%) %A -- %g %v\n")
;;
;;          (add-hook 'gnus-select-group-hook   'gnus-group-set-timestamp)
;;
;;          (setq gnus-group-line-format
;;              "%M%S%p%3uZ[%L]%-4uE%uT %5y: %-40,40g %7,7~(cut 6)d %uC\n")
;;
;;      Which looks like the following in the buffer, notice that the topic
;;      mode is on.
;;
;;          Procmail 34 -- 9
;;            [2]3.g       0: nnml:list.ding        30T1810
;;            [2]3.        0: nnml:list.ntemacs     30T1819
;;          * [2]3.  !     0: nnml:list.procmail    30T1850
;;            [2]2.t      33: nnml:list.flamenco    30T1849
;;             | |   |                              %d
;;             | |   %uT
;;             | The whole "2.t" comes from %uE
;;             %L
;;
;;      There you can see the benefit of the user functions. The [2] tells
;;      the group level, "2.t" says "2" day total Expiry, "." means that the
;;      period is explicitely defined as a group parameter and "t" means
;;      that total expiry in the group parameter list is on. Do you
;;      see the extra `g' at the top line? It tells that the `gcc-self'
;;      group parameter is activated in group parameter list. If group has
;;      ticked articles, the %uT will show it. The %ud says "Day 30 in the
;;      month, Time 18:10" when you read the group.
;;
;;      All these additional functions that display these status informations
;;      can be found from this package.
;;
;;  Displaying the group parameter info
;;
;;      As you saw above, the %uE function, or more precisely,
;;      `tinygnus-uff-group-expiry' controls what information is returned by
;;      looking at `tinygnus--uff-table'. Please configure it to display
;;      whatever you want from group parameters.
;;
;;  Article wash functions
;;
;;      If you are interested, you can add following function(s) to the
;;      `gnus-article-display-hook'
;;
;;      o   `tinygnus-article-fix-msword-quotes'
;;
;;  Debuging Gnus: can't select group
;;
;;      If something is wrong with the Gnus and you can't enter the group
;;      for a reason or another, something has happened to your setup.
;;      There is *experimental* funtions in this package that may shed some
;;      help. The first thing to try is calling
;;      `tinygnus-gnus-debug-investigate-problem' Which asks for a group
;;      name, give fully qualifies name like "nnml:list.ding". This
;;      function is geared towards debugging nnml groups, so you may
;;      not benefit a lot for other backends.
;;
;;      Thre is no detailled instructions how to fix the situation after
;;      the function has run, but the printed results in
;;      `tinygnus--debug-buffer' should at least give better clues. LOOK
;;      CLOSELY THE RESULTS. And supply them to gnus newsgroup or mailing
;;      list. Maybe someone can by looking at the values what's the
;;      problem.
;;
;;      o   It's mostly trial and error; after you get used to reading
;;          what values are important and what to do with it.
;;      o   The `tinygnus-gnus-debug-investigate-problem' is EXPERIMENTAL
;;          and it is not guarranteed to work with any Gnus version.
;;          It was created to debug setup problems with 5.8.2 1999-12-24.
;;
;;  Gnus summary minor mode
;;
;;      `tinygnus-summary-mode' is turned on when summary buffer gets
;;      created. There are some keybindings that you may wish to
;;      relocate for faster access, e.g. the search functions that
;;      repeat the last search. In Gnus, pressing Esc-s to search again
;;      would require a confirmation of the search string each time,
;;      while using ``tinygnus-gnus-summary-search-article-forward' uses
;;      the supplied string immediatedly. To relocate keys, use this code:
;;
;;          (defun my-tinygnus-summary-mode-hook ()
;;            "Define new keybindings."
;;            (let ((map tinygnus--summary-mode-map))
;;              (define-key map [(alt ?<)]
;;                'tinygnus-gnus-summary-search-article-forward)
;;              (define-key map [(control ?<)]
;;                'tinygnus-gnus-summary-search-article-backward)))
;;
;;          (add-hook 'tinygnus-summary-mode-hook
;;                    'my-tinygnus-summary-mode-hook)

;;}}}

;;; Change Log:

;;; Code:

;;; Code:

;;{{{ require: basic

;;; ......................................................... &require ...

(eval-and-compile
  (message (locate-library "gnus")) ;; Leave location to compile output
  ;; 2000-01 When compiling CVS gnus with XEmacs ....
  (condition-case err
      (require 'gnus)
    (error
     (message "  ** tinygnus.el: Wow, (require 'gnus) dies on error %s"
              (prin1-to-string err)))))

(require 'timezone)
(require 'pp)
(require 'tinylibm)

(autoload 'time-stamp-string                  "time-stamp")
(autoload 'gnus-summary-mark-article          "gnus-sum")
(autoload 'gnus-summary-select-article        "gnus-sum")
(autoload 'gnus-summary-work-articles         "gnus-sum")
(autoload 'gnus-summary-move-article          "gnus-sum")
(autoload 'gnus-summary-show-all-threads      "gnus-sum")
(autoload 'gnus-summary-first-subject         "gnus-sum")
(autoload 'gnus-summary-mark-article-as-read  "gnus-sum")
(autoload 'gnus-summary-find-next             "gnus-sum")
(autoload 'gnus-summary-mark-as-read-forward  "gnus-sum")
(autoload 'gnus-summary-mark-as-expirable     "gnus-sum")
(autoload 'gnus-summary-search-article-forward "gnus-sum")
(autoload 'gnus-read-move-group-name          "gnus-sum")
(autoload 'gnus-set-global-variables          "gnus-sum")
(autoload 'gnus-set-mode-line                 "gnus-sum")
(autoload 'nnfolder-group-pathname            "nnfolder")

(eval-when-compile
  (require 'advice))

(eval-and-compile
  ;;  Yes, this variable is purposively put to "tinypath" package.
  ;;  See that package for better explanation.
  ;;
  (defconst tinypath--gnus-load-path
    (locate-library "gnus"))
  (message "tinygnus.el: Gnus path %s"
           (or tinypath--gnus-load-path "<path not found>"))
  (defvar bbdb/gnus-summary-show-bbdb-names)
  (defvar bbdb/gnus-summary-prefer-bbdb-data)
  (defvar bbdb/gnus-summary-prefer-real-names)
  (defvar bbdb/gnus-summary-mark-known-posters)
  (defvar bbdb-message-marker-field)
  (defvar bbdb/gnus-summary-known-poster-mark)
  (defvar bbdb-canonicalize-net-hook)
  (defvar gnus-last-search-regexp)
  (defvar gnus-expirable-mark)
  (defvar gnus-init-inhibit)
  (defvar mail-send-hook)
  (defvar tinyurl-mode)
  (defvar gnus-version)
  (if (not (locate-library "bbdb"))
      (message "\
tinymail.el: ** No bbdb.el along load-path. Please do not compile this file.
                http://bbdb.sourceforge.net/")
    (autoload 'bbdb-search-simple        "bbdb")
    (autoload 'bbdb-canonicalize-address "bbdb")
    (autoload 'bbdb-record-net           "bbdb")
    (autoload 'bbdb-record-getprop       "bbdb")
    (autoload 'bbdb-record-name          "bbdb")))

;;}}}
;;{{{ trquire: advanced

(eval-and-compile

  ;;  (autoload 'mail-header-extra "nnheader.el" "" nil 'macro) ;; 2000-01 Gnus

  ;; ................................................... version check ...

  (defun tinygnus-check-gnus-installation-libraries ()
    "Verify that new enough Gnus version is installed to the Emacs."
    (let ((i 0))
      (flet ((load-it
              (lib)
              (let* ((name   (if (stringp lib)
                                 lib
                               (prin1-to-string lib)))
                     (path   (locate-library name))
                     (status (ignore-errors
                               (if (symbolp lib)
                                   (require lib)
                                 (load path 'noerr)))))
                (unless status
                  (message "TinyGnus: ** [ERROR] couldn't load %s %s. "
                           name
                           (or path
                               (concat
                                "Load error or package not along `load-path'."
                                " Please check Gnus path>")))
                  (incf  i)))))
        (dolist (lib '(gnus-group
                       message
                       nnml
                       nnfolder
                       nnheader
                       gnus-agent
                       ;;  mm-util defined mm-char-int, which is used
                       ;;  in gnus.el::gnus-continuum-version
                       ;;
                       ;;  => continuum fails, if mm-char-int is not defined.
                       mm-util))
          (load-it lib))
        i)))

  (defun tinygnus-check-gnus-installation-gnus ()
    "Verify that new enough Gnus version is installed to the Emacs."
    ;;  Standard Gnus than comes with Old Emacs versions
    ;;  is not accepted. User must be running development
    ;;  version of Gnus or the latest Emacs

    (unless (or (ti::emacs-p "21.1")
                (ti::xemacs-p "21.4"))
      (message (emacs-version))
      (string-match
       (concat
        ;;  Win32 installs to emacs-20.6
        "emacs-[0-9]+\\.[0-9]+"
        ;; Unix Emacs installs to /usr/share/emacs/20.6/lisp/
        "\\|/emacs/[0-9]+\\.[0-9]+/")
       (or tinypath--gnus-load-path
           (locate-library "gnus")
           ""))))

  (defun tinygnus-check-gnus-installation-emacs ()
    "Verify that new enough Gnus version is installed to the Emacs."
    (cond
     ((not (fboundp 'mail-header-extra))
      "nnheader.el::mail-header-extra was not defined.")
     ((tinygnus-check-gnus-installation-gnus))))

  (defun tinygnus-check-gnus-installation ()
    "Verify that new enough Gnus version is installed to the Emacs."
    (let ((i (tinygnus-check-gnus-installation-libraries))
	  emacs-gnus
	  error)
      (when (string-match "rest" (ti::function-args-p 'mm-char-int))
        ;; Hm, the function is alias to `ignore', fix it.
        (defalias 'mm-char-int 'identity))

      (setq error
            (if (not (zerop i))
                (format "%d load errors happened" i)
              (tinygnus-check-gnus-installation-emacs)))
      (when error
        (message
         "\
  ** tinygnus.el: [Error: %s]
                  %s.
                  Emacs version is %s. %s
                  This file works and compiles only with the very
                  latest development gnus.
                  http://www.gnus.org/dist/ => gnus.tar.gz (see time stamps)
                  Be sure to include latest Gnus along the `load-path'
                  when you compile this file.
                  If you do not plan to use Gnus, ignore this message.
                  -- You will now see load aborted message --"
         error
         (if (boundp 'gnus-version)
             gnus-version
           "<error loading gnus.el>")
         emacs-version
         (if emacs-gnus
             "\n  ** tinygnus.el: [Gnus from Emacs installation - no good]"
           ""))
        (error "Load aborted. See *Messages* buffer"))))

  (tinygnus-check-gnus-installation))

;;}}}
;;{{{ setup: variables

(ti::package-defgroup-tiny TinyGnus tinygnus-- extensions
  "Gnus utilities grabbag.")

(defcustom tinygnus--load-hook nil
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'TinyGnus)

(defcustom tinygnus--summary-ube-send-to-postmasters-hook nil
  "Hook run after each UBE message has been forwarded to postmasters."
  :type  'hook
  :group 'TinyGnus)

(defcustom tinygnus--article-ube-send-to-postmasters-hook nil
  "Hook run after the UBE forward has been composed.
References:
  `tinygnus-article-ube-send-to-postmasters'
  `tinygnus--use-postmaster-addresses'"
  :type  'hook
  :group 'TinyGnus)

;;  but it was not a good idea to reduce to top level domain.
;;  for example
;;
;;      nslookup sdn-ts-037txfwoRP08.dialsprint.net OK
;;      nslookup                     dialsprint.net NOK
;;
;;  So the top level domain addresses are not necessarily reliable.
;;  Hm. Too bad. This could have been general function, but now it seems that
;;  you have to use some regexp based function
;;
;;      xx.aol.com  --> aol.com
;;      yy.aol.com

(defcustom tinygnus--canonilize-ip-functions
  '(tinygnus-domain tinygnus-article-received-top-level-domain-maybe)
  "List of function to change host address.
Function should top level domain for passed HOST.
Eg: '(\"aa.foo.com\" \"bb.foo.com\") --> '(\"foo.com\")
References:
  `tinygnus--domain-table'"
  :type  '(list function)
  :group 'TinyGnus)

(defcustom tinygnus--ube-forward-mail-addresses
  ;; "uce@ftc.gov" no more active
  '()
  "*Addresses of archives where to send UBE messages."
  :type  '(list string)
  :group 'TinyGnus)

(defcustom tinygnus--show-dormants  t
  "*If non-nil, show dormants immediately when entering non-nntp group.
Some people like to use dormant mark ? as `important todo' in their
private mail groups, while gnus usually reserves dormant mark to
articles that do not need to show up if there is no replies."
  :type  'boolean
  :group 'TinyGnus)

(defcustom tinygnus--save-mail-notify-regexp (user-login-name)
  "Regexp to match To field when mail is saved.
A message is printed in the echo area when the regexp matches.
References:
 `tinygnus-save-mail-notify'"
  :type  'regexp
  :group 'TinyGnus)

(defcustom tinygnus--nslookup-file
  (ti::package-config-file-prefix "tinygnus.el")
  "File where to store `tinygnus--nslookup-table' cache.
This speeds up processing the UBE messages so that nslookup hosts can
be found from cache instead of calling expensive `nslookup'"
  :type  'file
  :group 'TinyGnus)

(defcustom tinygnus--z nil ;; ".gz"
  "*Extension to use in .newsrc and .eld files.
If you set this to `.gz' then compressed files are in use.
You have to reload the package every time you change this settings."
  :type 'string
  :group  'TinyGnus)

(defcustom tinygnus--gnus-version-for-advice "."
  "Which version of gnus should have compressed .eld.gz support."
  :type   'regexp
  :group  'TinyGnus)

(defcustom tinygnus--ube-exclude-ip-regexp nil
  "Regexp to matc IP domains that are not included in SPAM complain.
When function `tinygnus-ube-send-to-postmasters' is called, all the IP
addresses in Received headers are gathered and a message to all
ostmasters are composed. This regexp filter out the read IP addresses.

A good value would be to filter out your local domain."
  :type  'regexp
  :group 'TinyGnus)

(defcustom tinygnus--ube-abuse-account-table
  '(("aol\\|globecomm\\|nortel\\.net\\|\\<usa\\.net"
     . "abuse")
    ("mindspring"
     . "abuse")
    ("PRSERV.NET$"
     . "postmaster@attglobal.net")
    ("prodigy"
     . "abuse")
    ("\\<uu\\.net"
     . "fraud"))
  "The account address where to send complaint.
Many domains have opened `abuse' address in addition to RFC `postmaster'.

1) If regexp matches the domain, the complaint is directed to `ACCOUNT@DOMAIN'
2) If the ACCOUNT contains `@', then the ACCOUNT is supposed to have complete
email address where to send complaint

Format:
  '((REGEXP . ACCOUNT)
    (REGEXP . ACCOUNT)
    ..)"
  :type   '(repeat (list regexp (string :tag "Account")))
  :group  'TinyGnus)

(defcustom tinygnus--domain-table
  '(("aol\\."           . "aol.com")
    ("soon\\.fi"        . "soon.fi")
    ("yahoo"            . "yahoo.com")
    ("wanadoo"          . "wanadoo.fr")
    ("compuserve"       . "compuserve.com")
    ("dialsprint"       . "dialsprint.net")
    ("\\<uu\\.net\\>"   . "uu.net"))
  "If REGEXP match address, use DOMAIN-ADDRESS.
This table will efectively filter out duplicate addresses, e.g.
xx.foo.com yy.foo.com are same as foo.com

Table format:
  '((REGEXP . DOMAIN-ADDRESS)
    (REGEXP . DOMAIN-ADDRESS)
    ..)"
  :type   '(repeat (list regexp (string :tag "Domain")))
  :group  'TinyGnus)

(defcustom tinygnus--uff-table
  '(
    ;;  *Group* buffer format functions in big letter

    (?C tinygnus-uff-group-comment)
    (?E tinygnus-uff-group-expiry)
    (?F tinygnus-uff-summary-line-bbdb)
    (?N tinygnus-uff-message-count)
    (?T tinygnus-uff-group-tick)
    (?Z tinygnus-uff-group-file-size)

    ;;  *Summary* buffer

    (?d tinygnus-uff-summary-date))
  "The gnus-user-format-function map table.

Format:

 '((CH FUNCTION)
   (CH FUNCTION)
   ..)

The CH is the `X' character is used to run gnus-user-format-fnction-X
where the FUNCTION will be mapped. For example if you want to
run expiry function through %uE modified the elt in the pable is

  (?E tinygnus-uff-group-expiry)"
  :type '(repeat
          (char   :tag "gnus-user-format-fnction-")
          (symbol :tag "Used TinyGnus function"))
  :group  'TinyGnus)

(defcustom tinygnus--uff-summary-date
  '(format "%02d-%02d" (string-to-number date-mon)  date-day)
  "This variable contain Lisp FORM to return summary line date string.
If you want to customize this variable you have to look at the source
code of `tinygnus-uff-summary-date' and use the dynamically bound variables.

The default value is

   '(format \"%02d-%02d\" (string-to-number date-mon)  date-day)

Which returns ISO date parts YY-MM. It is good to selects as brief
date string as possible because the summary line is quite crowded place.

Here is value for YY-MM-DD:

   '(format \"%s-%02d-%02d\"
             (ti::string-right date-yyyy 2)
             (string-to-number date-mon)
             date-day)"
  :type  'sexp
  :group 'TinyGnus)

(defcustom tinygnus--expiry-in-group-string "."
  "Character to add to the end of expiry count if value is defined in group.
When `tinygnus-uff-group-expiry' is called the number of days is returned.
But if the expiry-wait is defined in group parameters, this string
is added to the number."
  :type  'string
  :group 'TinyGnus)

(defcustom tinygnus--additional-group-info
  '((gcc-self     t eq "g")
    (total-expire t eq "t"))
  "*What additional grup parameter `tinygnus-uff-group-expiry' would return.
When GROUP-PARAM run agains TEST is equal to VALUE then
return RETURNED-STRING. You should return only one character in
string to save space.

For example the following entry

  (gcc-self t eq \"c\")

Will cause following test, the GCC-SELF-VALUE is read from group.

 (if (eq GCC-SELF-VALUE t) ..return \"c\")

Format:
  '((GROUP-PARAM VALUE TEST RETURNED-STRING)
    ...)"
  :type '(repeat
          (symbol   :tag "Group param")
          (sexp     :tag "wanted value")
          (function :tag "test function")
          (string   :tag "returned val"))
  :group 'TinyGnus)

(defcustom tinygnus--get-news-symbolic-levels
  '(("primary Mail"     . 1)
    ("secondary Mail"   . 2)
    ("mailing lists"    . 3)
    ("mail, some"       . '(1 2 3))
    ("some news"        . 5)
    ("News, all"        . 'gnus-group-get-new-news)
    ("Mail, all"        . 'gnus-group-get-new-mail))
  "*Symbolic `gnus-get-new-news' levels.
Format:

  '((COMPETION-STRING . NUMBER-OR-FUNCTION-OR-LIST)
    ..)

COMPETION-STRING

  The completion name is offered when you call
  `tinygnus-gnus-group-get-news-symbolic' and all news at level NUMBER is
  read.

NUMBER-OR-FUNCTION-OR-LIST

  If the parameter is number, News in that Group level is read.

  If the cdr parameter is function, then the function is called
  interactively.

  If the parmeter is list of numbers like '(1 2) then all news on
  those group levels are read."
  :type  '(repeat
           string
           sexp)
  :group 'TinyGnus)

;;}}}
;;{{{ setup: private

(defvar tinygnus--use-postmaster-addresses nil
  "Variable contains postmaster address used to compose UBE response.
You can use this in `tinygnus--article-ube-send-to-postmasters-hook'
This variable also has following properties: 'ip-list 'ns-list (nslookup)")

(defvar tinygnus--output-buffer "*tinygnus-buffer*"
  "Temporary buffer to store miscellaneous user selected information.")

;; Reloading file will reset this; which is good.
;; By sitting on the Group Line in *Group* Try
;;
;;      (get 'tinygnus--gnus-group-info (make-symbol (gnus-group-group-name)))
;;      (symbol-plist 'tinygnus-group-info)

(defconst tinygnus--gnus-group-info nil
  "Miscellaneous group information kept in property list.
Keyed by full prefixed group name.")

(defvar tinygnus--nslookup-table nil
  "List of nslookup's.")

;;}}}
;;{{{ Debug

;;; ........................................................... &debug ...

;;;###autoload (autoload 'tinygnus-debug-toggle "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-debug-show   "tinygnus" "" t)

(eval-and-compile (ti::macrof-debug-standard  "tinygnus" "--"))

;;}}}

;;{{{ minor mode

;;;###autoload (autoload 'tinygnus-summary-install-mode  "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-summary-mode          "tinygnus" "" t)
;;;###autoload (autoload 'turn-on-tinygnus-summary-mode  "tinygnus" "" t)
;;;###autoload (autoload 'turn-off-tinygnus-summary-mode "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-summary-commentary    "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-summary-version       "tinygnus" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinygnus-summary-" " Tg" "\C-c'"  "Tgnus" 'TinyGnus "tinygnus--summary-"

   "Gnus utilities.
This minor mode defines some additional commands to Gnus Group buffer.
See also `tinygnus-summary-mode'

Mode description:

Prefix key to access the minor mode is defined in
`tinygnus--summary-mode-prefix-key' which is by deafult C - c '

\\{tinygnus--summary-mode-prefix-map}"

   "Gnus summary mode extras"
   (progn                               ;Some mode specific things
     (when (and tinygnus-summary-mode
                (not (eq major-mode 'gnus-summary-mode)))
       (setq tinygnus-summary-mode nil)
       (error
        "TinyGnus mode can only be used in summary buffer. Mode is now `%' "
        (symbol-name major-mode))))
   "TinyGnus summary mode"
   (list                                ;arg 10
    tinygnus--summary-mode-easymenu-name

    ["Repeat search forward"  tinygnus-gnus-summary-search-article-forward   t]
    ["Repeat search backward" tinygnus-gnus-summary-search-article-backward  t]
    "----"
    ["Send UBE complaint"     tinygnus-summary-ube-send-to-postmasters       t]
    ["Catchup, expire"    tinygnus-gnus-summary-catchup-with-expire-all      t]
    ["Catchup this user"      tinygnus-summary-expunge-all-from-user         t]
    ["Catchup, expire non-replied"
     tinygnus-gnus-summary-catchup-with-expire-not-replied                   t]
    ["Catchup, read"    tinygnus-gnus-summary-catchup-with-read-all          t]
;;;   ["Toggle original"        tinygnus-summary-toggle-original            t]
    "----"
    ["Gather headers"            tinygnus-summary-gather-headers             t]
    ["Gather URLs"               tinygnus-summary-gather-urls                t]
    ["Gather, display buffer"    tinygnus-summary-gather-display             t]
    ["Gather, clear buffer"      tinygnus-summary-gather-clear               t]
    "----"
    ["Toggle original article"   tinygnus-summary-toggle-original            t]
    ["Reload Gnus init file"     tinygnus-gnus-group-read-init-file          t]
    "----"
    ["Debug show"                tinygnus-debug-show                         t]
    ["Debug TinyGnus"            tinygnus-debug-toggle                       t]
    "----"
    ;; ["Keyboard menu"          tinygnus-menu-main                          t]
    ["Package version"           tinygnus-summary-version                    t]
    ["Package commentary"        tinygnus-summary-commentary                 t]
    ["Mode help"                 tinygnus-summary-mode-help                  t]
    ["Mode off"                  turn-off-tinygnus-summary-mode              t])
   (progn
     (define-key map "M"      'tinygnus-summary-move-article)
     ;;   And the X-window keys, Unfortunately these may be
     ;;   be under the ESC key in some keyboards.
     ;;   see also `w32-alt-is-meta'
     (define-key root-map [(alt s)]
       'tinygnus-gnus-summary-search-article-forward)
     (define-key root-map [(alt r)]
       'tinygnus-gnus-summary-search-article-backward)
     (define-key map  "u" 'tinygnus-summary-ube-send-to-postmasters)
     (define-key map  "e" 'tinygnus-gnus-summary-catchup-with-expire-all)
     (define-key map  "E" 'tinygnus-gnus-summary-catchup-with-expire-not-replied)
     (define-key map  "C" 'tinygnus-summary-expunge-all-from-user)
     (define-key map  "d" 'tinygnus-gnus-summary-catchup-all-with-delete)
     (define-key map  "r" 'tinygnus-gnus-group-read-init-file)
     (define-key map  "t" 'tinygnus-summary-compose-current-mail-as-template)
     (define-key map  " " 'tinygnus-summary-toggle-original)
     (define-key map  "gh" 'tinygnus-summary-gather-headers)
     (define-key map  "gu" 'tinygnus-summary-gather-urls)
     (define-key map  "gd" 'tinygnus-summary-gather-display)
     (define-key map  "gc" 'tinygnus-summary-gather-clear)
     (define-key map  "Ds" 'tinygnus-debug-show)
     (define-key map  "Ds" 'tinygnus-debug-toggle)
     (define-key map  "?"  'tinygnus-summary-mode-help)
     (define-key map  "Hm" 'tinygnus-summary-mode-help)
     (define-key map  "Hc" 'tinygnus-summary-commentary)
     (define-key map  "Hv" 'tinygnus-summary-version)
     (define-key map  "x"  'turn-off-tinygnus-summary-mode))))

;;; ----------------------------------------------------------------------

;;;###autoload (autoload 'tinygnus-group-install-mode   "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-group-mode           "tinygnus" "" t)
;;;###autoload (autoload 'turn-on-tinygnus-group-mode   "tinygnus" "" t)
;;;###autoload (autoload 'turn-off-tinygnus-group-mode  "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-group-commentary     "tinygnus" "" t)
;;;###autoload (autoload 'tinygnus-group-version        "tinygnus" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinygnus-group-" " Tg" "\C-c'"  "Tgnus" 'TinyGnus "tinygnus--group-"

   "Gnus utilities.

Mode description:

Prefix key to access the minor mode is defined in `tinygnus--group-mode-prefix-key'

\\{tinygnus--group-mode-prefix-map}"
   "TinyGnus"
   (progn                               ;Some mode specific things
     (when (and tinygnus-group-mode
                (not (eq major-mode 'gnus-group-mode)))
       (setq tinygnus-group-mode nil)
       (error "Mode Can only be used in Gnus Group buffer.")))
   "Gnus group mode extras"
   (list                                ;arg 10
    tinygnus--group-mode-easymenu-name
    ["Read news with symbolic levels" tinygnus-gnus-group-get-news-symbolic  t]
    ["Crash box delete"               tinygnus-crash-box-delete              t]
    ["Crash box find-file"            tinygnus-crash-box-find-file           t]
;;;   ["Make group from file"        tinygnus-make-group-from-file          t]
    "----"
    ["Set Group level in region" tinygnus-group-set-current-level-region     t]
    ["Add to-list mailing list parameter" tinygnus-group-parameter-mailing-list t]
    ["nndoc  Create groups from directory" tinygnus-make-group-from-dir-nndoc t]
    ["nnml Read procmail spool and make groups"
     tinygnus-make-group-from-dir-nnml-procmail-spool                         t]
    ["nnml Recreate marked groups" tinygnus-make-group-nnml                  t]
    ["nnml Create groups from directory" tinygnus-make-group-nnml-from-dir   t]
    "----"
    ["Debug show"                    tinygnus-debug-show                     t]
    ["Debug TinyGnus"                tinygnus-debug-toggle                   t]
    ["Debug Gnus group"              tinygnus-gnus-debug-investigate-problem t]
    "----"
    ["Reload Gnus init file"         tinygnus-gnus-group-read-init-file      t]
    ["Package version"               tinygnus-group-version                  t]
    ["Package commentary"            tinygnus-group-commentary               t]
    ["Mode help"                     tinygnus-group-mode-help                t]
    ["Mode off"                      turn-off-tinygnus-group-mode            t])
   (progn
     ;;   The ' prefix is usually free
     ;;   "c"  map for CrashBox
     (define-key map  "g"  'tinygnus-gnus-group-get-news-symbolic)
     (define-key map  "cd" 'tinygnus-crash-box-delete)
     (define-key map  "cf" 'tinygnus-crash-box-find-file)
     (define-key map  "mn"  'tinygnus-make-group-nnml)
     (define-key map  "mN"  'tinygnus-make-group-nnml-from-dir)
     (define-key map  "mf"  'tinygnus-make-group-from-file)
     (define-key map  "md"  'tinygnus-make-group-from-dir-nndoc)
     (define-key map  "mp"  'tinygnus-make-group-from-dir-nnml-procmail-spool)
     (define-key map  "ds"  'tinygnus-debug-show)
     (define-key map  "dd"  'tinygnus-debug-toggle)
     (define-key map  "dgo" 'tinygnus-gnus-debug-on)
     (define-key map  "dgf" 'tinygnus-gnus-debug-off)
     (define-key map  "dgi" 'tinygnus-gnus-debug-investigate-problem)
     (define-key map  "pm"  'tinygnus-group-parameter-mailing-list)
     (define-key map  "lr"  'tinygnus-group-set-current-level-region)
     (define-key map  "N"   'tinygnus-move-group-to-native-nnml)
     (define-key map  "?"  'tinygnus-group-mode-help)
     (define-key map  "Hm" 'tinygnus-group-mode-help)
     (define-key map  "Hc" 'tinygnus-group-commentary)
     (define-key map  "Hv" 'tinygnus-group-version)
     (define-key map  "r"  'tinygnus-gnus-group-read-init-file)
     (define-key map  "x"  'turn-off-tinygnus-group-mode))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-help ()
  "Mode Help."
  (interactive)
  (describe-function 'tinygnus-group-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-group-read-init-file ()
  "Read Gnus init file always. sets `init-file-user' to t."
  (interactive)
  ;;
  ;;  Without these Gnus won't read the init file
  ;; ´letf' is needed, because you cannot have macro expansion
  ;;  inside special form `let'. `letf' is just like let, but
  ;;  all values must be in (var value) format.
  ;;
  (letf ((gnus-init-inhibit nil)
         ((ti::compat-load-user-init-file) t))
    (gnus-group-read-init-file)))

;;}}}
;;{{{ Install

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinygnus-install (&optional uninstall)
  "Install package. Optionally UNINSTALL."
  (interactive "P")
  (let ((list  '((gnus-group-mode
		  gnus-group-mode-hook
		  (turn-on-tinygnus-group-mode))
		 (gnus-summary-mode
		  gnus-summary-mode-hook
		  (turn-on-tinygnus-summary-mode))
		 (gnus-article-mode
		  gnus-article-mode-hook
		  (tinygnus-article-mode-keys))))
	hook
	hook-list)
    (tinygnus-uff-table-install)
    (ti::add-hooks  'tinygnus--summary-ube-send-to-postmasters-hook
                    'tinygnus-mark-deleted
                    uninstall)
    (ti::add-hooks  'tinygnus--summary-mode-define-keys-hook
                    'tinygnus-summary-mode-define-keys
                    uninstall)
    (ti::add-hooks  'tinygnus--group-mode-define-keys-hook
                    'tinygnus-group-mode-define-keys
                    uninstall)
    (ti::add-hooks 'tinygnus--article-ube-send-to-postmasters-hook
                   '(tinygnus-ube-cc-spam-archive
                     tinygnus-ube-postmaster-inform)
                   uninstall)
    ;;  Run the hook functions immediately if GNUS is already present.
    (ti::dolist-buffer-list
     (memq major-mode (mapcar 'car list))
     'temp-buffers
     (not 'exclude)
     (progn
       (dolist (func (nth 2 (assq major-mode list)))
         (funcall func))))
    (dolist (elt list)
      (setq hook      (nth 1 elt)
            hook-list (nth 2 elt))
      (ti::add-hooks hook hook-list uninstall))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-mode-keys ()
  "Install default keybindings to GNUS map."
  (define-key gnus-article-mode-map "U"
    'tinygnus-article-ube-send-to-postmasters))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-table-install ()
  "Install `tinygnus--uff-table'. Previous Gnus user functions will be wiped."
  (interactive)
  (let (func
	gnus-func)
    (dolist (elt tinygnus--uff-table)
      (unless (fboundp (setq func  (nth 1 elt)))
        (error "Internal error. tinygnus--uff-table, No func %s" func))
      (setq gnus-func
            (intern (format "gnus-user-format-function-%s"
                            (char-to-string (car elt)))))
      (defalias gnus-func func))))

;;}}}
;;{{{ Final install

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-compile  ()
  "Compile all that is needed to get peak performance."
  (interactive)
  (tinygnus-gnus-compile-1
   (mapcar (function (lambda (x) (car x)))
           tinygnus--uff-table)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-compile-1 (char-list)
  "Compile the line formats and their user functions: CHAR-LIST."
  (interactive)
  (let ((fmt  "gnus-user-format-function-%s")
	sym
	func)
    (message "TinyGnus: Compiling relevant parts...")
    (save-window-excursion ;; Gnus and Compile changes the windowcfg

      ;; File: gnus,  Node: Compilation
      ;;
      ;; format specification variables ... `M-x' `gnus-compile' after you've
      ;; This will result in the new specs being byte-compiled, and you'll get
      ;; top speed again.
      ;;
      ;; ...user-generated function %uX are not compiled though
      ;;  See also M-x `gnus-update-format'

      (dolist (ch char-list)
        (setq func (format fmt (char-to-string ch)))
        (setq sym (intern-soft func))
;;;     (ti::d! func (fboundp sym))
        (if (not (fboundp sym))
            (error "Not exist: %s" func)
          (byte-compile sym)))
      ;;  see if we can find this
      (when (not (fboundp 'gnus-update-format-specifications))
        (load "gnus-spec" 'noerr))
      ;;  Update all formats in all Gnus buffer.
      ;;  Node: Formatting Variables
      ;;  Currently Gnus uses the following formatting variables:
      (cond
       ((fboundp 'gnus-update-format-specifications)
        (ti::funcall 'gnus-update-format-specifications 'force))
       ((fboundp 'gnus-update-format)   ;19.34
        (dolist (var '("gnus-group-line-format"
                       "gnus-group-mode-line-format"
                       "gnus-summary-line-format"
                       "gnus-summary-mode-line-format"
                       ;;  Don't compile these because would require
                       ;;  unnecessary packages
                       ;;  "gnus-topic-line-format"
                       ;;  "gnus-server-mode-line-format"
                       ;;  "gnus-server-line-format"
                       "gnus-article-mode-line-format"))
          ;; Use caution, I have several Gnus versions around.
          ;; Define only those that exist.
          (when (and (intern-soft var) (boundp (intern-soft var)))
            (gnus-update-format var)))))
      (if (get-buffer "*Gnus Format*")  ;Where did this come from?
          (kill-buffer "*Gnus Format*")))
    ;;  Too bad that this command gives compilation errors because
    ;;  the variables are dynamically bound in each user function
    (when (fboundp 'gnus-compile)       ;New Gnus  only
      (gnus-compile))))

;;}}}
;;{{{ General Misc

;;; ----------------------------------------------------------------------
;;;
(defmacro tinygnus-set-group ()
  "Set variable `group'."
  `(or group
       (setq group (symbol-value 'gnus-newsgroup-name))
       (error "Can't know the group")))

;;; ----------------------------------------------------------------------
;;;
(put 'tinygnus-summary-map-articles-macro 'lisp-indent-function 0)
(defmacro tinygnus-summary-map-articles-macro (&rest body)
  "Map through marked mesaes in Summary buffer and execute BODY.
The variable `nbr' has the current article number. Use command
 (return) to stop the loop."
  `(let ((articles (gnus-summary-work-articles nil))
	 gnus-article-display-hook     ;Do not run this
	 gnus-article-prepare-hook
	 gnus-select-article-hook
	 gnus-article-mode-hook
	 gnus-visual-mark-article-hook)
     ;; ByteComp silencer, unused variables
     (if gnus-article-display-hook (setq gnus-article-display-hook t))
     (if gnus-article-prepare-hook (setq gnus-article-prepare-hook t))
     (if gnus-select-article-hook (setq gnus-select-article-hook t))
     (if gnus-article-mode-hook (setq gnus-article-mode-hook t))
     (if gnus-visual-mark-article-hook (setq gnus-visual-mark-article-hook t))
     ;; (gnus-summary-save-process-mark)
     (dolist (nbr articles)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinygnus-summary-map-article-body-macro 'lisp-indent-function 0)
(defmacro tinygnus-summary-map-article-body-macro (&rest body)
  "Run BODY inside articles that are marked.
Variable `out' contains the output buffer and `buffer' points
to the article buffer."
  `(let ((out (get-buffer-create tinygnus--output-buffer))
	 buffer)
     (tinygnus-summary-map-articles-macro
      (gnus-summary-select-article 'all nil 'pseudo nbr)
      (setq buffer (get-buffer gnus-original-article-buffer))
      (when buffer
        (with-current-buffer buffer
          (ti::pmin)
          ,@body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinygnus-output-buffer-macro 'lisp-indent-function 0)
(defmacro tinygnus-output-buffer-macro (&rest body)
  "Run BODY if `tinygnus--output-buffer' exists. Signal error otherwise."
  `(let ((buffer (get-buffer tinygnus--output-buffer)))
     (if buffer
         (progn ,@body)
       (error "TinyGnus: buffer %s does not exist." tinygnus--output-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defmacro  tinygnus-files-from-dir-macro (dir &rest body)
  "Read all files from DIR and do BODY.
You can refer to `file' when processing the files. Stop loop with
command (return)."
  `(let ((files (tinygnus-read-files-from-dir ,dir)))
     (when (or (not (interactive-p))
               (and (interactive-p)
                    (y-or-n-p
                     (format
                      "Found %d files, Proceed " (length files)))))
       (dolist (file files)
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinygnus-summary-map-lines 'lisp-indent-function 0)
(defmacro tinygnus-summary-map-line-macro (&rest body)
  "Map line by line and run BODY in Summary buffer."
  `(save-excursion
     (ti::pmin)
     (while (not (eobp))
       ,@body
       (forward-line 1))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinygnus-mark-deleted ()
  "Mark current article expirable(mail) or deleted(news)."
  (interactive)
  (cond
   ((string-match "nntp" gnus-newsgroup-name )
    (gnus-summary-mark-article nil))
   (t
    (gnus-summary-mark-article gnus-expirable-mark))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-compose-return-address (address)
  "Check that ADDRESS is usable. Discard er 0.0.0.
Returns [N.N.N] for pure ip addresses."
  (cond
   ;; Drop addresses ^000.*  or .0.0
   ((string-match "^0+\\.\\|\\.0+\\.0|^127" address))
   ((string-match "^[0-9.]+$" address)
    (format "[%s]" address))
   (address)))                          ;Return as is

;;; ----------------------------------------------------------------------
;;; #todo: Actually how can we tell when the address is same in the domain?
;;;
;;; postmaster@hub6.compuserve.com is same as postmaster@compuserve.com
;;;
;;; And we don't want to send duplicates, ehm?
;;;
;;;(defun tinygnus-address-uniquefy (list)
;;;  "Leave only shortest domain name: like DOMAIN.com over some.DOMAIN.com"
;;;  (let (array ret domain)
;;;    (dolist (elt list)
;;;      (setq array  (split-string elt "[.]")
;;;         domain (nth 1 (nreverse array))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-ube-cc-spam-archive ()
  "Send copy of message to SPam archives.
1998-06:
  http://www.spam-archive.org/         --> spam-list@toby.han.de
  http://www.ftc.gov/os/9806/email.htm --> uce@ftc.gov"
  (dolist (address tinygnus--ube-forward-mail-addresses)
    (ti::mail-add-to-field-string "CC" address  "To")))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-ube-postmaster-inform ()
  "Add a short Preface chapter to postmasters about UBE."
  (ti::mail-text-start 'move)
  (insert
   "
To postmasters: Please investigate this UBE (Unsolicited Bulk Email)
message and take the necessary actions to prevent delivering similar
messages in the future. You may have an open SMTP Relay or there
is a person that is abusing your accounts.

Thank you beforehand for your co-operation to stop UBE in the net.\n"))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinygnus-nslookup-save (&optional read)
  "READ or save `tinygnus--nslookup-table' to `tinygnus--nslookup-file'.
See function `tinygnus-article-ube-send-to-postmasters'."
  (interactive "P")
  (let ((fid  "tinygnus-nslookup-save")
	(file tinygnus--nslookup-file))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (when (and (stringp file)
               ;;  1) If we're saving, then go ahead
               ;;  2) If we're reading, check that file exists
               (or (null read)
                   (file-exists-p file)))
      (if (string-match "\\.gz$" file)
          (ti::use-file-compression))
      (tinygnus-debug fid (if read "read") file)
      (cond
       (read
        (load file)
        (put 'tinygnus--nslookup-table 'pos (length tinygnus--nslookup-table))
        (if (interactive-p)
            "TinyGnus: nslookup loaded."))
       (t
        (ti::write-file-variable-state
         file
         "TinyGnus.el nslookup cache file"
         '(tinygnus--nslookup-table)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nslookup-maybe-save ()
  "Save every 5th new nslookup."
  (let ((fid   "tinygnus-nslookup-maybe-save")
	(count (get   'tinygnus--nslookup-table 'pos))
	(len   (length tinygnus--nslookup-table)))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (when (or (not (integerp count))
              (> (- len count) 4))
      (tinygnus-debug fid "Calling save" len)
      (tinygnus-nslookup-save)
      (put 'tinygnus--nslookup-table 'pos len))))

;;}}}
;;{{{ Article functions

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-expunge-all-from-user ()
  "Expunge all posts and followups from the current author"
  (interactive)
  (save-window-excursion
    (gnus-summary-show-article)
    (gnus-summary-select-article-buffer)
    (let ((author (gnus-fetch-field "From")))
      (gnus-summary-score-entry
       "from" author 'substring -500000
       (+ (date-to-day (time-stamp-string)) gnus-score-expiry-days))
      (gnus-summary-score-entry
       "followup" author 'substring -500000
       (+ (date-to-day (time-stamp-string)) gnus-score-expiry-days)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-compose-current-mail-as-template ()
  "Use current article as template and compose new mail."
  (interactive)
  (let ((article (gnus-summary-article-number)))
    (gnus-setup-message
	'reply-yank
      (gnus-summary-select-article t)
      (set-buffer gnus-original-article-buffer)
      ;; see message.el - message-supersede
      (let ( ;; (sender  (message-fetch-field "sender"))
	    ;; (from    (message-fetch-field "from"))
	    (buffer  (current-buffer)))
	;; Get a normal message buffer.
	(message-pop-to-buffer (message-buffer-name "mail from template"))
	(insert-buffer-substring buffer)
	(message-narrow-to-head)
	;; Remove unwanted headers.
	(message-remove-header "Message-ID")
	(message-remove-header "Content-Type")
	(when message-ignored-supersedes-headers
	  (message-remove-header message-ignored-supersedes-headers t))
	;; insert mail-header-separator if needed
	(if (re-search-backward
	     (concat "\n" mail-header-separator "\n") nil t)
	    (goto-char (point-max))
	  (insert mail-header-separator))
	(widen)
	(forward-line 1))
      (push
       `((lambda ()
	   (when (gnus-buffer-exists-p ,gnus-summary-buffer)
	     (save-excursion
	       (set-buffer ,gnus-summary-buffer)
	       (gnus-cache-possibly-remove-article ,article nil nil nil t)
	       (gnus-summary-mark-as-read ,article gnus-canceled-mark)))))
       message-send-actions))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-toggle-original  ()
  "Toggle showing original article and *Article*."
  (interactive)
  (let ((wlist (ti::window-list))
	buffer
	disp-win
	disp-buffer
	name)
    ;;  Is there any "article" buffer in this
    (dolist (win wlist)
      (setq name  (buffer-name (setq disp-buffer (window-buffer win))))
      (when (string-match "article" name)
        (setq disp-win win)
        (return)))
    (cond
     ((eq disp-buffer (get-buffer gnus-article-buffer))
      (if (null (setq buffer (get-buffer gnus-original-article-buffer)))
          (message "Can't find: %s" gnus-original-article-buffer)
        ;;  If we didn't found the window; user occupied the full
        ;;  *Summary* buffer
        (if (null disp-win)
            (pop-to-buffer buffer)
          (select-window disp-win)
          (switch-to-buffer buffer))
        (ti::pmin)))
     (t
      (gnus-summary-select-article)
      (pop-to-buffer gnus-article-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-ube-send-to-postmasters (&optional no-confirm)
  "Process all marked articles and send coplaint to postmasters.
If NO-CONFIRM is non-nil, then the messages are enst directly without
confirmations."
  (interactive "P")
  (let ((fid   "tinygnus-summary-ube-send-to-postmasters")
	(count 0)
	kill-flag)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;; (gnus-summary-save-process-mark)
    (tinygnus-summary-map-articles-macro
     (tinygnus-debug fid nbr)
     (gnus-summary-select-article 'all nil 'pseudo nbr)
     (message "TinyGnus: UBE processing article %d" nbr)
     (tinygnus-article-ube-send-to-postmasters
      'send (not no-confirm)
      kill-flag)
     (run-hooks 'tinygnus--summary-ube-send-to-postmasters-hook)
     (setq kill-flag t)
     (incf  count))
    (if (interactive-p)
        (message "TinyGnus: Mapped %d ube messgaes" count))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-domain (address)
  "Change ADDRESS xx.domain.com --> domain.com using `tinygnus--domain-table'."
  (let ((ret address))
    (when tinygnus--domain-table
      (dolist (elt tinygnus--domain-table)
        (when (string-match (car elt) address)
          (setq ret (cdr elt))
          (if (not (stringp ret))
              (error "Invalid format in tinygnus--domain-table: %s" elt))
          (return)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-ube-identify ()
  "Examine all headers in Post and try to identify UBE source.
This function will run `traceroute' to the found address and from
the output, the upstream provider is usually the ISP where you can send
complaint if the destination address won't handle your notes.

The upstream provider in yraceroute output is the second/third last rows
in the listing."
  (interactive)
  (let ()
    ;; #todo:
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-ip-top-level-domain (host)
  "Convert HOST a.b.c  => b.c domain."
  (when (string-match "\\.\\([^.]+\\.[^.]+\\)$" host)
    (match-string 1 host)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-received-top-level-domain-maybe (host)
  "If HOST looks suspicious, return HOST x.y.z => y.z.
For example:

   sdn-ap-002watacoP1727.foo.net => foo.net."
  (when (and (stringp host)
             ;;  Skip 123.123.123.123
             (not (ti::mail-ip-raw-p host)))
    (let ((name (if (or (string-match "^\\([^.]+\\)\\....+\\..+$" host)
                        (string-match "^\\([^.]+\\)\\....+\\..+$" host))
                    (match-string 1 host))))
      (when (and (stringp name)
                 (string-match "[0-9-]" name))
        (setq host (ti::mail-ip-top-level-domain  host)))))
  host)

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-received-list-handle (received)
  "Treat 3 sequence list differently.
The first address(X) in Received header may be forged

    Received: from X ( Y [Z] ) by

From which we get addresses

    '(X Y Z)

The X May look like:

    adsl-156-62-239.asm.foo.net

Shorten the address to 2 significant parts only

    foo.net."
  (when (eq 3 (length received))
    (let ((first (car received))
          (rest  (cdr received)))
      (when (string-match "\\.\\([^.]+\\.[^.]+\\)$" first)
        (setq first (match-string 1 first))
        (push first rest)
        (setq received rest))))
  received)

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-host-canonilize (host)
  "Send HOST to `tinygnus--canonilize-ip-functions'."
  (let ((fid   "tinygnus-host-canonilize")
        ret)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (dolist (function tinygnus--canonilize-ip-functions)
      (when (setq ret (funcall function host))
        (tinygnus-debug
         (format "%s: %s (%s => %s)" fid function host ret))
        (setq host ret)))
    host))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nslookup-filter (list)
  "Filter out duplicates.

Input:

  list            '(ip ip ...)

Return:

  ns-lookup-list   Need nslookup.
  ns-list          known addresses.

References:

  `tinygnus--nslookup-table' contains previous nslookup address."
  (let ((fid  "tinygnus-nslookup-filter")
        elt
        ns-lookup-list
        ns-list)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (dolist (ip list)
      (when (stringp ip)
        ;;    Filter out dupliates
        ;;    xx.aaa.com --> aaa.com
        ;;    yy.aaa.com
        (setq ip (tinygnus-host-canonilize ip))
        (if (or (null tinygnus--nslookup-table)
                (and (null (setq elt
                                 (assoc ip tinygnus--nslookup-table)))
                     (null (setq elt
                                 (rassoc ip tinygnus--nslookup-table)))))
            ;; Not known, put into ask list
            ;; Sometimes we get address 8.8.5/8.7.3, which is actually
            ;; a sendmail version. Filter out false hits
            (if (not (string-match "/" ip))
                (push ip ns-lookup-list))
          ;;  This is from cache. We have done the lookup already.
          (push (list ip elt) ns-list))))
    (tinygnus-debug fid "NS-LOOKUP-LIST" (nreverse ns-lookup-list))
    (tinygnus-debug fid "NS-LIST" (nreverse ns-list))
    (list ns-lookup-list
          ns-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nslookup-do (list)
  "Run nslookup for LIST.
Failed addresses are returned in ERR-LIST. Good address
are added to `tinygnus--nslookup-table'.

Return:

 '(err-list ok-list)."
  (let ((fid  "tinygnus-nslookup-do")   ; Function id
        err-list)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (error "Sorry, this is disabled for now. New Spam mode is in sketch table.")
    ;; #todo: ti::mail-nslookup function has changed.
    (dolist (elt (ti::mail-nslookup list nil 'verb))
      (tinygnus-debug fid elt)
      (if (nth 1 elt) ;; Add new members to the cache.
          (add-to-list 'tinygnus--nslookup-table (nth 1 elt))
        (push (car elt) err-list)))
    (tinygnus-debug fid "ERR-LIST" (mapcar 'car err-list))
    (list err-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nslookup-examine-ip-top-level (ip-list)
  "Examine IP-LIST by converting x.y.z => y.z."
  (let (list)
    (dolist (ip ip-list)
      ;;  Treat only DNS names, not raw ip's: a.b.c.d   => c.d
      (unless (ti::mail-ip-raw-p ip)
        (setq ip
              (ti::mail-ip-top-level-domain ip))
        (multiple-value-bind (nok)
            (tinygnus-nslookup-do ip)
          (unless nok ;; Succeeded, top level was ok
            (pushnew ip list :test 'string=)))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nslookup-examine-ip-list (ip-list)
  "Examine `Received:' header IP-LIST.
Return:

  '(ns-err-list ns-list)."
  (let (ns-err-list
        ns-list)
    (dolist (received ip-list)          ; '((IP IP IP) ..)
      (setq received (tinygnus-article-received-list-handle received))
      (multiple-value-bind (need-lookup ok)
          (tinygnus-nslookup-filter received)
        (if ok
            (setq ns-list (append ok ns-list)))
        ;;  Now run nslookup for ip's that are not known and
        ;;  add them to total list.
        (multiple-value-bind (nok)
            (tinygnus-nslookup-do need-lookup)
          (if nok
              (setq ns-err-list (append nok ns-err-list))))))
    ;;  In case non of the IPs succeeded, do rigorous search.
    ;;  Maybe top level domans are ok
    (unless ns-list
      (message
       "TinyGnus: complete  nslookup failure. Next: top-level search.")
      (setq ns-list (tinygnus-nslookup-examine-ip-top-level ns-err-list)))
    (list ns-err-list
          ns-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-ube-address-compose (ns-list)
  "Compose UBE return addresses from NS-LIST."
  (let ((fid  "tinygnus-ube-address-compose")
	str
	done
	tmp-list
	addr-list
	ip)
    (unless fid ;; No-op. XEmacs byte compiler silencer.
      (setq fid nil))
    ;;   ns-list:  '(IP (name . addr))
    ;;                |  |
    ;;                |  the nslookup results
    ;;                Ip in the message
    (dolist (elt ns-list)
      (setq str  (car-safe (nth 1 elt))
            ip   (cdr-safe (nth 1 elt)))
      ;;   The reverse lookup:
      ;;     nslookup mail.eic.com.mx   : 200.23.239.146
      ;;     nslookup  mty.eic.com.mx   : 200.23.239.146
      ;;
      ;;   Ie. the IP numeric addresses are the same, thus we don't send
      ;;   double copies to different symbolic addresses.
      ;;
      ;;   The tmp-list will hold numeric ip addresses '((IP . t) (IP .t) ..)
      ;;   and if the ip is already there, the message to that site has
      ;;   already been composed,
      (setq done nil)
      (if (and ip (assoc ip tmp-list))
          (setq done t)
        (if (stringp ip)
            (push (cons ip str) tmp-list)))
;;;     (ti::d! done str ip tmp-list)
      (cond
       (done)                           ;do nothing
       ((stringp str)
        (let  ((abuse-list tinygnus--ube-abuse-account-table)
               tmp
               login
               email)
          (setq str (tinygnus-host-canonilize str))
          (setq tmp (ti::list-find abuse-list str))
          (cond
           ((and (stringp tmp)
                 (string-match "@" tmp))
            (setq email tmp))
           (t
            (if (stringp tmp)
                (setq login (concat (cdr tmp) "@"))
              (setq login "postmaster@"))
            (when (setq str (tinygnus-compose-return-address str))
              (setq email (concat login str)))))
          (when email
            (add-to-list 'addr-list email))))
       (t
        ;;  There is no point to send complaint to address where nslookup
        ;;  failed.
        (message "TinyGnus: %s nslookup failed" (car str))
        (setq str (car elt))
        (when (stringp (tinygnus-compose-return-address str))
          (add-to-list 'ns-err-list str)))))
    ;;  Save the values so that hook functions can use them.
    (setq tinygnus--use-postmaster-addresses addr-list)
    (put  'tinygnus--use-postmaster-addresses 'ns-list ns-list)
    (tinygnus-debug fid "ADDR-LIST" addr-list)
    addr-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-ube-send-to-postmasters
  (&optional send confirm kill)
  "Parse all Received-headers and complain about UBE aka Spam.
This function runs nslookup for each Received-header, so it may take
some time to get all valid postmaster addresses. The found unique numeric
and symbolic IP addresses are used when composing message to postmasters.

We do not use any mail arresses that are in the message, because mail
addresses cannot be checked and are usually forged in UBE message.

Input:

  SEND      Flag, If prefix arg given, send the message.
  CONFIRM   Flag, If SEND is non-nil, should the sending be confirmed.
  KILL      Flag, if non-nil, kill possible mail that was being composed.

References:

  `tinygnus--ube-exclude-ip-regexp'
  `tinygnus--use-postmaster-addresses'
  `tinygnus--nslookup-table'
  `tinygnus--nslookup-file'"
  (interactive "P")
  (let ((message-included-forward-headers ".")
	(fid           'tinygnus-article-ube-send-to-postmasters)
	;; Add-on package to message.el that generates keywords.
	;; DO NOT be intercative.
	(message-keyword-interactive  nil)
	;; Disable PGP auto signing.
	tinypgp-mode
	;; Make copy, we modify this in correct buffer
	(mail-send-hook mail-send-hook)
	ip-list
	ns-list
	ns-err-list
	addr-list
	subject
	buffer)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (unless message-included-forward-headers ;; Byte Compiler silencer
      (setq message-included-forward-headers nil))
    ;; ................................................. byte-compiler ...
    ;; Quiet Byte Compiler, unused variable.
    (if tinypgp-mode
        (setq tinypgp-mode nil))
    (unless mail-send-hook
      (setq mail-send-hook nil))
    (if (and (boundp 'message-keyword-interactive)
             message-keyword-interactive)
        (setq message-keyword-interactive t))
    ;; ......................................................... check ...
    (unless (get-buffer gnus-original-article-buffer)
      (error "TinyGnus: panic, no gnus-original-article-buffer exist."))
    ;;   Get the cache table if not set
    (or tinygnus--nslookup-table
        (tinygnus-nslookup-save 'read))
    (with-current-buffer gnus-original-article-buffer
      (setq subject (mail-fetch-field "Subject"))
      (ti::pmin)
      ;; ............................................ received-headers ...
      ;;   We have to do nslookup for each ip to find out if
      ;;   it is alive and filter out duplicates
      (setq ip-list (ti::mail-parse-received tinygnus--ube-exclude-ip-regexp))
      (tinygnus-debug fid "IP-LIST" ip-list)
      (put  'tinygnus--use-postmaster-addresses 'ip-list ip-list)
      ;; ........................................ &check-need-nslookup ...
      ;;  Check if we have done nslookup for this already.
      (multiple-value-bind (nok ok)
          (tinygnus-nslookup-examine-ip-list ip-list)
        (setq ns-err-list nok
              ns-list     ok))
      (setq addr-list (tinygnus-ube-address-compose ns-list))
      (cond
       ((null addr-list)
        (message
         "'%s' Could not read ip addresses. Check ti::mail-parse-received."
         subject))
       (t
        ;;  The list is in order of appearence: Reference headers top-down,
        ;;  but the originating address is at the end. We reverse the list
        ;;  so that we get originator, next and the 2nd next ...
        (setq addr-list (nreverse addr-list))
        (when (and kill
                   (setq buffer (get-buffer (message-buffer-name "mail"))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (message-forward nil)
        (ti::mail-kill-field "^Subject" (format "ABUSE (Was: %s)" subject))
        (ti::mail-kill-field "^To" (car addr-list))
        (when (setq addr-list (cdr addr-list))
          (ti::mail-add-field "CC" (ti::list-join addr-list ", ")
                              "To" nil 'replace))
        (tinygnus-nslookup-maybe-save)
        ;; disable few settings, like TinyPgp
        (setq tinypgp-mode      nil
              mail-send-hook (delq 'tinypgp-auto-action mail-send-hook))
        (run-hooks 'tinygnus--article-ube-send-to-postmasters-hook)
        (when ns-err-list
          (ti::mail-text-start 'move)
          (insert "\nReceived header IP addresses that failed nslookup,\n"
                  "possibly forged:\n")
          (dolist (elt ns-err-list)
            (insert "    " elt "\n")))
        (when send
          (if (or (null confirm)
                  (and confirm
                       (progn
                         (ti::pmin)
                         (y-or-n-p "Send to postmasters? "))))
              (message-send-and-exit nil))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-article-fix-msword-quotes ()
  "Fixes MsWord style `smart quotes' back to normal ascii ones."
  (interactive)
  (with-current-buffer (symbol-value 'gnus-article-buffer)
    (let ((buffer-read-only nil)
          (inhibit-read-only t))
      (subst-char-in-region (point-min) (point-max) ?\221 ?`)
      (subst-char-in-region (point-min) (point-max) ?\222 ?')
      (subst-char-in-region (point-min) (point-max) ?\223 ?\")
      (subst-char-in-region (point-min) (point-max) ?\224 ?\"))))

;;}}}

;;{{{ user Format functions

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-group-tick (params)
  "Return `gnus-ticked-mark' if there are ticked articles in this group.
Otherwise return empty ` '. PARAMS is passed by gnus."
  (if (cdr (assq 'tick (symbol-value 'gnus-tmp-marked)))
      (char-to-string (symbol-value 'gnus-ticked-mark))
    " "))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-group-comment (params)
  "Return the comment field of a group. PARAMS is passed by gnus."
  (if (not (boundp 'gnus-tmp-group))
      ""
    (let* ((comment1 (gnus-group-get-parameter
                      (symbol-value 'gnus-tmp-group )
                      'comment))
           (comment2 (if (consp comment1)
                         (car comment1)
                       comment1)))
      (if (null comment2)
          ""
        (concat "(" comment2 ")")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-message-count (params)
  "Return nubmber of message in file backend. Ignore PARAMS."
  (if (not (boundp 'gnus-tmp-group))
      ""
    (let* ((group   (symbol-value 'gnus-tmp-group))
           (path    (tinygnus-group-pathname group)))
      (cond
       ((not (stringp path))
        "")                             ;Error!
       ((string-match "^/.*@" path)
        "@")                            ;Skip ange-ftp
       ((file-directory-p path)         ;nnml
        ;;  Don't count "." ".." and ".overview"
        ;;
        (- (length (directory-files path)) 3))
       ((file-exists-p path)
        ;; #todo: unfinished
        ;;  It's tougher with One file backends
        nil)))))

;;; ----------------------------------------------------------------------
;;; #todo: 1999-02 This function is not tested. Inserted as is
;;;
(defun tinygnus-uff-summary-line-bbdb (&optional header)
  "Display To: fields in summary buffers (g To From Newsgroups)
This is a copy of bbdb/gnus-summary-get-author, where FROM is replaced
with TO.

replace %f, %n or %uB in `gnus-summary-line-format' by this user function.
in groups where you want to use it."
  (let* ((to     (cdr-safe (assoc 'To (mail-header-extra header))))
         (data   (and bbdb/gnus-summary-show-bbdb-names
                      (ignore-errors (mail-extract-address-components to))))
         (name   (car data))
         (net    (car (cdr data)))
         (record (and data
                      (bbdb-search-simple
                       name
                       (if (and net bbdb-canonicalize-net-hook)
                           (bbdb-canonicalize-address net)
                         net)))))
    (if (and record name (member (downcase name) (bbdb-record-net record)))
        ;; bogon!
        (setq record nil))
    (setq name
          (or (and bbdb/gnus-summary-prefer-bbdb-data
                   (or (and bbdb/gnus-summary-prefer-real-names
                            (and record (bbdb-record-name record)))
                       (and record (bbdb-record-net record)
                            (nth 0 (bbdb-record-net record)))))
              (and bbdb/gnus-summary-prefer-real-names
                   (or (and (equal bbdb/gnus-summary-prefer-real-names 'bbdb)
                            net)
                       name))
              net to "**UNKNOWN**"))
    ;; Return answer
    (format "->%s%s"
            (or (and record bbdb/gnus-summary-mark-known-posters
                     (or (bbdb-record-getprop
                          record bbdb-message-marker-field)
                         bbdb/gnus-summary-known-poster-mark))
                " ")
            name)))

;;; ----------------------------------------------------------------------
;;; By Gary Lawrence Murphy (garym@sos.on.ca) in
;;; http://www.lebel.org/gnus/garym.gnus.el
;;;
;;; Used by permission 1997-09-29
;;;
(defun tinygnus-uff-summary-date (header)
  "Return a date string from the Article HEADER.
The format of date string is defined in `tinygnus--uff-summary-date'"
  (let* ((header-lines  (mail-header-lines header))
         (header-date   (mail-header-date header))
         (date-vector   (ignore-errors (timezone-parse-date header-date))))
    ;;  If value is nil, then `header-date' contained something that couldn't
    ;;  be parsed by `timezone-parse-date'
    (if (null date-vector)
        ""
      (let* ((date-yyyy (aref date-vector 0))
             (date-mon  (aref date-vector 1))
             (date-day  (string-to-number (aref date-vector 2)))
             (string-lines      (if (> header-lines 9999)
                                    "????"
                                  (number-to-string header-lines)))
             (string-mon        (or (capitalize
                                     (car (nth
                                           (1- (string-to-number date-mon))
                                           timezone-months-assoc)))
                                    "???"))
             (string-day        (format "%d" (or date-day "?"))))
        ;;  No-ops. Bytecomp silencers. User can use these dynamically bound
        ;;  variables in tinygnus--uff-summary-date, but ByteCompiler can't
        ;;  kow that and it would say: variable bound but not referenced.
        ;;
        ;;  Using "IF no-op-test NOTHING" statements silence byte compiler
        (if (null string-day)   (setq string-day t))
        (if (null string-mon)   (setq string-mon t))
        (if (null string-lines) (setq string-lines t))
        (if (null date-yyyy)    (setq date-yyyy t))
        (if tinygnus--uff-summary-date
            (eval tinygnus--uff-summary-date)
          "")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-group-expiry (params)
  "Return the Expiry value for the group.
Note: This function assumes that `nnmail-expiry-wait-function' is _not_
used. Instead you should use `gnus-auto-expirable-newsgroups'
and group parameter `nnmail-expiry-wait' combination.
PARAMS is passed by gnus.

Return:

  empty string

  or following where N is expiry number in days

  char   If the expiration value is symbol, the first character from it
         is returned. Eg 'i' for 'immediate.

   N     Global `nnmail-expiry-wait' used

   N.    Value was defined in Group parameter. See
         `tinygnus--expiry-in-group-string'

   ?    Something is wrong

References:

  `tinygnus--additional-group-info' Additional chacters added"
  (if (not (boundp 'gnus-tmp-group))
      ""
    (let ((group       (symbol-value 'gnus-tmp-group))
;;;        (re          gnus-auto-expirable-newsgroups)
	  (group-char  tinygnus--expiry-in-group-string)
	  (fmt         "%s")           ; I used to have e:%s
	  (ret         "")
	  arg
	  param func str val
	  stat)
      ;; ................................................... file test ...
      ;; Looking the expiry value makes sense only for groups that
      ;; have associated file. Do not check e.g. nntp
      (when  (tinygnus-group-pathname group)
        (setq
         arg   (or
                (gnus-group-get-parameter group 'expiry-wait)
                (gnus-group-get-parameter group 'nnmail-expiry-wait)))
        ;; .................................................. expiry get ...
        (setq
         ret
         (cond
          (arg
           (cond
            ((integerp arg)
             (format fmt (concat (number-to-string arg) group-char )))
            ((symbolp arg)
             (substring (symbol-name arg) 0 1))
            (t
             "?.")))
          ((gnus-group-auto-expirable-p group)
           (cond
            ((numberp nnmail-expiry-wait)
             (format fmt (number-to-string nnmail-expiry-wait)))
            ((symbolp nnmail-expiry-wait)
             (substring (symbol-name arg) 0 1))
            (t
             "?")))
          (t
           ;; This group isn't defined as expirable.
           ""))))
      ;; ......................................................... other ...
      (dolist (elt tinygnus--additional-group-info)
        (setq param (nth 0 elt)
              val   (nth 1 elt)
              func  (nth 2 elt)
              str   (nth 3 elt))
        (setq
         stat
         (if (eq param 'total-expire)
             ;; Ask from gnus directly.
             (gnus-group-total-expirable-p group)
           (funcall func arg val)))
        (when stat
          (setq ret (concat ret str))))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-uff-group-file-size (arg)
  "Return File size if the group has attached file.
ARG is passed by gnus.

Returned strings:

 @   Group has ange-ftp like path.
 ?   The path does not exist.
 N   Filesize in kilos (1000byte count) filesize lower that 1000 is
     not returned."
  ;;   ARG is nil usually when us is called.
  ;;
  (if (not (and (boundp 'gnus-tmp-group) ;current group name
                (string-match "nnfolder" (symbol-value 'gnus-tmp-group))))
      ""
    (let* ((group   (symbol-value 'gnus-tmp-group))
           (path    (tinygnus-group-pathname group))
           size)
      (cond
       ((not (stringp path))
        "")                             ;Error!
       ((string-match "^/.*@" path)
        "@")                            ;Error!
       ((not (file-exists-p path))
        ;;  Ugh; file does not exist? Make a warning to group buffer
        "?")
       (t
        ;;  Display file size in kilos, if size is < 1000, do not
        ;;  display 0 kilos.
        ;;
        (setq size (nth 7 (file-attributes path)))
        (setq size (/ size 1000))
        (if (zerop size)
            ""
          (number-to-string size)))))))

;;}}}
;;{{{ Summary: misc functions

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-move-article (&optional n)
  "Move articles N to another mail group.
See `tinygnus--summary-move-article-table'"
  (interactive "P")
  (let* ((group    gnus-newsgroup-name)
         (articles (gnus-summary-work-articles n))
         (prefix   (gnus-group-real-prefix group))
         (action   'move)
         (pfx      (or (ti::string-match ".*:\\([^.]+.\\)" 1 group) ""))
         select-method                  ;Make it nil
         to-newsgroup)
    (setq to-newsgroup
          (gnus-read-move-group-name
           "Move"
           (concat (symbol-value
                    (intern (format "gnus-current-%s-group" action)))
                   pfx)
           articles prefix))
    (gnus-summary-move-article
     n
     to-newsgroup select-method action)))

;;; ----------------------------------------------------------------------
;;; See gnus-sum.el::gnus-summary-catchup-all
;;;  (&optional all quietly to-here not-mark)
;;;
(defun tinygnus-gnus-summary-catchup-all-with-mark
  (&optional all to-here not-mark mark-char)
  "Mark rest of the articles with marker char.
Input:
  ALL
  TO-HERE
  NOT-MARK
  MARK-CHAR"
  (gnus-set-global-variables)
  (gnus-summary-show-all-threads)
  (when (gnus-summary-first-subject (not all))
    (while (and
            (if to-here (< (point) to-here) t)
            (gnus-summary-mark-article-as-read mark-char)
            (gnus-summary-find-next (not all)))))
  (gnus-set-mode-line 'summary))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-summary-catchup-with-expire-all (&optional all)
  "Mark rest or ALL articles expriable."
  (interactive "P")
  (tinygnus-gnus-summary-catchup-all-with-mark
   all
   nil
   nil
   gnus-expirable-mark))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-summary-catchup-with-read-all (&optional all)
  "Mark rest or ALL articles expriable."
  (interactive "P")
  (tinygnus-gnus-summary-catchup-all-with-mark
   all
   nil
   nil
   gnus-del-mark))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-summary-search-article-backward ()
  "Repeat last search backward."
  (interactive)
  (tinygnus-gnus-summary-search-article-forward t))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-summary-search-article-forward (&optional backward)
  "Repeat last search forward or BACKWARD."
  (interactive)
  (when (stringp gnus-last-search-regexp)
    (gnus-summary-search-article-forward
     gnus-last-search-regexp backward)
    (message "Searched: %s" gnus-last-search-regexp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-gather-headers  ()
  "Read marked messages and gather all headers to `tinygnus--output-buffer'.
When you see some suspicious messages, the headers are all you need to spot
the problem. This function makes it easy to collect such messages."
  (interactive)
  (tinygnus-summary-map-article-body-macro
   (cond
    ((re-search-forward "^[ \t]*$" nil t)
     (forward-line 1)
     (append-to-buffer out (point-min) (point)))
    (t
     (message "TinyGnus: Problem with article number %d" nbr)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-gather-urls  (&optional arg verb)
  "Gathel all urls from marked messages. Duplicate ulrs are not gathered.

Input:
  ARG   If non-nil, then include `group:atricle-nbr:' prefix to the
        beginning of each gathered url.
  VERB  Verbose messages."
  (interactive "P")
  (ti::verb)
  (let (subject-field
	(total 0)
	count
	url)
    (tinygnus-summary-map-article-body-macro
     (setq ;;; from-field    (mail-fetch-field "From")
      subject-field (mail-fetch-field "Subject"))
     (setq count 0)
     (while (re-search-forward "\\(http\\|ftp\\|telnet\\|wais\\):/" nil t)
       (incf  count)
       (setq url (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
       (with-current-buffer out
         (ti::pmin)
         (unless (re-search-forward (regexp-quote url) nil t)
           (ti::pmax)
           (if arg
               (insert (format "%s:%d: %s\n" gnus-newsgroup-name nbr url))
             (insert url "\n")))))
     (incf total count)
     (when verb
       (message "TinyGnus: msg %d, %d (%d urls) %s"
                nbr count total subject-field)))
    ;;  Turn on th URL jump mode.
    (with-current-buffer tinygnus--output-buffer
      (when (and (fboundp 'turn-on-tinyurl-mode-1)
                 (boundp 'tinyurl-mode)
                 (null (symbol-value 'tinyurl-mode)))
        (turn-on-tinyurl-mode-1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-gather-display  ()
  "Display `tinygnus--output-buffer'."
  (interactive)
  (tinygnus-output-buffer-macro (pop-to-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-gather-clear  ()
  "Clear `tinygnus--output-buffer'."
  (interactive)
  (let ((buffer (get-buffer-create tinygnus--output-buffer)))
    (ti::erase-buffer buffer)
    (if (interactive-p)
        (message "TinyGnus: %s cleared" tinygnus--output-buffer))))

;;}}}

;;{{{ Summary: exist, enter

;;; ............................................... &summary-functions ...
;;; Decriptions
;;;
;;;     It is annoying that gnus won't re-read the file groups automatically
;;;     if the file underneath has changed. Eg if you have appended to a file
;;;     that is known to gnus, you should press "g" to rescan the file
;;;
;;;     This piece of code saves the file attributes when you exit the Group
;;;     and when you re-enter it it checks if the file size is still
;;;     the same. If not, then it performs automatig "g" to re-read the file.
;;;
;;;     So, you only have to hit SPACE to read the group and leave the
;;;     details to the rest of the code.

(add-hook 'gnus-summary-prepare-exit-hook 'tinygnus-summary-prepare-exit-hook)
(add-hook 'gnus-select-group-hook         'tinygnus-select-group-hook)

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-summary-prepare-exit-hook ()
  "Save the group data before exit."
  (tinygnus-group-params-set))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-file-p (group)
  "Test if GROUP is file group."
  (string-match "nnfolder\\|nndoc\\|archive" group))

;;; ----------------------------------------------------------------------
;;; gnus-valid-select-methods (("nntp" post address prompt-address) ...
;;; gnus-server-alist
;;;   ( ("cache" nnspool "cache")
;;;     ("mbox" nnfolder "mbox"
;;;         (nnfolder-directory "~/Mail/mbox")
;;;         (nnfolder-active-file "~/Mail/mbox/active")
;;;         (nnfolder-get-new-mail nil)
;;;         (nnfolder-inhibit-expiry t)
;;;         )
;;;     ...
;;;
;;;  gnus-group-real-prefix (group)
;;;
;;;  gnus-server-to-method  (server)
;;;  gnus-server-get-method (group method)
;;;  gnus-group-prefixed-name (group method)
;;;  ...whole name from GROUP and METHOD.
;;;
(defun tinygnus-group-pathname (&optional group)
  "Return path of the GROUP."
  (tinygnus-set-group)
  (let* ((method (gnus-group-method group))
         ;;      (pfx    (gnus-group-prefixed-name group method))
         ;;      (server1 (assoc server gnus-server-alist))
         (group1 (ignore-errors (gnus-group-real-name group))))
    (when (and group1
               (ti::listp method))
      (cond
       ((eq (car method) 'nnfolder)
        ;; (setq dir  (memq 'nnfolder-directory method))
        (or (ignore-errors (nnfolder-group-pathname  group1))
            ""))
       ((eq (car method) 'nnml)
        (or (ignore-errors (nnmail-group-pathname
                            group1 (symbol-value 'nnml-directory)))
            ""))
       ((eq (car method) 'nnmh)
        (or (ignore-errors (nnmail-group-pathname
                            group1 (symbol-value 'nnmh-directory)))
            ""))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-params-set (&optional group)
  "Save extra GROUP information to group symbol plist."
  (tinygnus-set-group)
  (let ((path      (tinygnus-group-pathname group))
	(sym       'tinygnus--gnus-group-info)
	attr
	list)
    (when (and path
               (file-exists-p path))
      (cond
       ((file-directory-p path)         ;nnml
        ;; If you use nnml, then it's not that important to
        ;; know the filesize. It would be too slow to map over all files
        ;; and sum up the total size for nnml files.
        ;;
        ;; The nnfolder and others use single file, so getting the filesize
        ;; is much simpler and faster.
        nil)
       (t
        (setq attr (file-attributes path))
        (setq list                   ;Make date list ((ATTR . VAL) ..)
              (list
               (cons 'file path)
               (cons 'file path)
               (cons 'file-attr attr)
               (cons 'file-size (nth 7 attr))
               (cons 'file-mod-time (nth 5 attr))))
        (put sym (make-symbol gnus-newsgroup-name) list))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-select-group-hook (&optional group)
  "Actions when GROUP is entered.
If this is file group, check if the underlying file has changed and
read it. Otherwise do nothing. This is like doing 'g' before entering
the group."
  (tinygnus-set-group)
  (let ((sym   'tinygnus--gnus-group-info)
	(path (tinygnus-group-pathname))
	info
	attr-now
	attr-was
	s1
	s2)
    ;; Warn about missing .overview file
    (when path
      (setq s1 (ti::file-make-path path ".overview"))
      (when (and (string= "nnml" (or (car (gnus-group-method group)) ""))
                 (not (file-exists-p s1)))
        (message
         "TinyGnus: .overview missing, Run nnml-generate-nov-databases")
        (sit-for 2)))
    (when (and (setq path (tinygnus-group-pathname))
               (setq info (get sym (make-symbol group))))
      ;;  If we enter gruop for the first time the EXIT INFO is not
      ;;  yet available. When this is second time the info is there.
      (cond
       ((null (setq path (cdr (assq 'file-path info))))
        (message "TinyGnus: invalid INFO for group."))
       ((null (file-exists-p path))
        (message "TinyGnus: File does not exist any more, %s" path))
       (t
        (setq attr-now (file-attributes path)
              attr-was (assq 'file-attr info))
        (when (not (eq (setq s1 (nth 7 attr-now))
                       (setq s2 (nth 7 attr-was))))
          (message
           "My Gnus: File sizes differ, rereading... %s (%d/%d) "
           path s1 s2)
          (gnus-group-get-new-news-this-group)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-summary-catchup-with-expire-not-replied  ()
  "Mark all not replied messages as read (nntp) or expired (other backends)."
  (interactive)
  (tinygnus-summary-map-line-macro
   (when (and (looking-at "^ .*")
              (not (looking-at "^.*Re:")))
     (save-excursion
       (if (string-match "nntp" (or gnus-newsgroup-name "nntp"))
           (gnus-summary-mark-as-read-forward 1)
         (gnus-summary-mark-as-expirable 1))))))

;;}}}
;;{{{ Group: e.g. Symbolic get levels

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-mail-extract-address-components (field)
  "Extract addresses from current buffer matching FIELD."
  (when (setq field (mail-fetch-field field))
    (setq field (nth 1 (mail-extract-address-components field)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-parameter-mailing-list (n)
  "Set `to-list' group parameter to one that is found from mails. This
function is handy if you just dropped a nnml directory under gnus
containing mailing list messages and you want to add the `to-list'
defiitions easily to group parameters. (usually recovering the mailing list
properties for Group.)

The list of email address choices is gathered from last article in the group
by looking at To, From, Reply-To, CC. Answer empty string \"\" if none
match the mailing list address and fix the `to-list' Group parameter by hand
with G p."
  (interactive "P")
  (dolist (group (gnus-group-process-prefix n))
    ;;   (setq group (gnus-group-group-name)))
    (let (
;;;        (name    (gnus-group-real-name group))
;;;        (method  (gnus-find-method-for-group group))
;;;        (type    (nth 0 method))     ;; 'nnml
;;;        (server  (or (nth 1 (assoc 'nnml-address method))
;;;                     (nth 1 method)))
;;;        (dir       (nth 1 (assoc 'nnml-directory method)))
;;;        (new-name  (concat server "." name))
	  (to-list   (gnus-group-get-parameter group 'to-list))
	  address
	  list)
      (gnus-group-remove-mark group)
      (if to-list
          (message "TinyGnus: %s `to-list' already set to %s" group to-list)
        (with-temp-buffer
          (tinygnus-nnml-find-file (current-buffer) group)
          (dolist (field '("From" "To" "Cc" "Reply-To"))
            (when (setq address
                        (tinygnus-mail-extract-address-components field))
              (push address list)))
          (setq to-list
                (completing-read
                 (format "TinyGnus: SET %s to-list? " group)
                 (ti::list-to-assoc-menu list)))
          (unless (ti::nil-p to-list)
            (message "TinyGnus: %s `to-list' set to %s" group to-list)
            (gnus-group-set-parameter group 'to-list to-list)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-set-current-level-region  (beg end level)
  "Map over region BEG and END and set groups to LEVEL."
  (interactive "r\nnTinyGnus set level to region: ")
  (let ((lines (count-lines beg end)))
    (goto-char (min beg end))
    (gnus-group-set-current-level lines level)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-read-files-from-dir (dir)
  "Return files from DIR in sorted order."
  (let ((files
	 (ti::directory-files
	  dir "."
	  'absolute
	  '(not (file-directory-p arg)))))
    (sort files 'string<)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-move-group-to-native-nnml (n)
  "Move nnml+SOME:name under nnml:SOME.name."
  (interactive "P")
  (let* ((nnml-server (assoc "nnml" gnus-server-alist))
         (nnml-dir    (or (assoc 'nnml-directory nnml-server)
                          "~/Mail/")))
    (dolist (group (gnus-group-process-prefix n))
      ;;   (setq group (gnus-group-group-name)))
      (let* ((level   (gnus-group-group-level))
             (name    (gnus-group-real-name group))
             (method  (gnus-find-method-for-group group))
;;;          (type    (nth 0 method))   ;; 'nnml
             (server  (or (nth 1 (assoc 'nnml-address method))
                          (nth 1 method)))
             (dir       (nth 1 (assoc 'nnml-directory method)))
             (new-name  (concat server "." name))
             from
             new-dir
             status)
        (cond
         ((not (and nnml-dir dir))
          (message "TinyGnus: move nmml doesn't know directory for %s" group))
         ((not (file-directory-p dir))
          (message "TinyGnus: move nmml directory not exist %s %s" group dir))
         (t
          (setq from      (nnheader-concat dir       name))
          (setq new-dir   (nnheader-concat nnml-dir  new-name))
;;;       (ti::d! nnml-dir new-name from new-dir)
          (setq status (ti::directory-move from new-dir))
          (if (not (ti::nil-p status))
              (message "TinGnus: ERROR while move %s %s %s" from new-dir status)
            (message "Tinygnus: Moving %s --> %s" group new-dir)
            (gnus-group-goto-group group)
            (gnus-group-kill-group)
            (gnus-group-make-group new-name '(nnml ""))
            (gnus-group-set-current-level 1 level))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-make-group-nnml-from-dir (dir regexp)
  "Create nnml groups from DIR matching REGEXP."
  (interactive "DTinyGnus Nnml from directory: \nsRegexp: ")
  (let ((files (directory-files
		(expand-file-name dir )
		nil regexp)))
    (dolist (group files)
      (when (file-directory-p (nnheader-concat dir group))
;;;     (if (tinygnus-nnml-group-alist-p group)
;;;         (message "TinyGnus: Alredy in Gnus %s. Ignored." group)
        (ignore-errors (gnus-group-make-group group '(nnml "")))
        (message "Tinygnus: Created nnml group %s" group)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-make-group-nnml (n)
  "Kill marked nnml groups and recreate them."
  (interactive "P")
  (let (nnml-list)
    (dolist (group (gnus-group-process-prefix n))
      ;;   (setq group (gnus-group-group-name)))
      (gnus-group-remove-mark group)
      (if (not (string-match "nnml" group))
          (message "TinyGnus: Recreate skipping non-nnml group %s" group)
        ;; (gnus-delete-line)
        (push (cons group (gnus-group-group-level))
              nnml-list)))
    (dolist (elt nnml-list)
      (let* ((group   (car elt))
             (level   (cdr elt))
             (name    (gnus-group-real-name group))
             (method  (gnus-find-method-for-group group))
             (type    (nth 0 method)) ;; 'nnml
             (server  (or (nth 1 (assoc 'nnml-address method))
                          (nth 1 method)))
             nnml-list)
        (if nnml-list ;; Byte Compiler silencer
            (setq nnml-list t))
        (cond
         ((not (and method type server))
          (message "TinyGnus: Recreating failure. NIL method for %s" group))
         (t
          (setq type (format "%s:"  (symbol-name type)))
          (gnus-group-goto-group group)
          (gnus-group-kill-group)
          (gnus-group-make-group name type)
          (gnus-group-set-current-level 1 level)
          (message "Tinygnus: Recreating group %s with level %d"
                   group level)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-make-group-from-file (method)
  "Make nndoc group from FILE with METHOD."
  (interactive
   (let (file)
     (setq file (read-file-name "Make group from file: " nil nil t))
     (if (or (file-directory-p file)
             (not (file-readable-p file)))
         (error "invalid file: %s" file))
     (setq method
           (completing-read
            "Method: "
            (ti::list-to-assoc-menu '("nndoc" "nnfolder" "nnmbox" "nnspool"))
            nil
            t
            "nndoc"))
     (list (list (make-symbol method) file)))) ;; interactive
  (gnus-group-make-group
   (file-name-nondirectory (nth 1 method))
   method))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-make-group-from-dir-nndoc (dir)
  "Read DIR and make all files as nndoc groups."
  (interactive "Ddirectory: ")
  (tinygnus-files-from-dir-macro
   dir
   (ignore-errors (gnus-group-make-doc-group file nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-make-group-from-dir-nnml-procmail-spool ()
  "This function is for old Gnus only that has `nnmail-procmail-directory'.
Read and convert them to nnml backends.
Say you have these files in in directory:

    junk.daemon.spool         list.java-linux.spool     mail.emacs.spool
    junk.dupli.spool          list.java.spool           mail.default.spool
    junk.null.spool           list.jcvs.spool

then each of these spool files would become a nnml backend folder, so that
`nnmail-procmail-suffix' is removed from the end filenames.

This function is primarily meant for promail users that create spool file
categories on the fly eg for new mailing lists. Alternatively, if you
have to start Gnus from scratch, it is nice to have function to create
nnml backends with one call."
  (interactive)
  (if (not (boundp 'nnmail-procmail-directory))
      (error "sorry, this Gnus doen no longer have nnmail-procmail-directory.")
    (when (y-or-n-p "Create many nnml backend folders from spool? ")
      (tinygnus-files-from-dir-macro
       (symbol-value 'nnmail-procmail-directory)
       (ignore-errors
         (let ((name (replace-regexp-in-string
		      (symbol-value 'nnmail-procmail-suffix)
		      ""
		      (file-name-nondirectory file))))
           (gnus-group-make-group name (quote (nnml "")))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-get-crash-box ()
  "Return Gnus crash box."
  (cond
   ((boundp 'mail-source-crash-box)
    (symbol-value 'mail-source-crash-box))
   ((boundp 'nnmail-crash-box)
    (symbol-value 'nnmail-crash-box))
   (t
    (error "TinyGnus: Can't find crash box for Gnus any more.\
Contact maintainer."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-crash-box-delete ()
  "Delete `nnmail-crash-box'."
  (interactive)
  (let ((box (tinygnus-get-crash-box)))
    (cond
     ((not (file-exists-p box))
      (message "TinyGnus: File not found: %s" box))
     ((and (file-exists-p box)
           (y-or-n-p
            (format
             "TinyGnus: Really delete crashbox %s"
             box)))
      (delete-file box)))
    (ti::kill-buffer-safe box)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-crash-box-find-file ()
  "Find-file Gnus crash-box."
  (interactive)
  (let ((box (tinygnus-get-crash-box)))
    (cond
     ((not (file-exists-p box))
      (message "TinyGnus: File not found: %s" box))
     (t
      (find-file-other-window box)))))

;;; ----------------------------------------------------------------------
;;; Note: if yo hit just "3 g"; GNUS will read all level up till 3,
;;; so you would actually read levels 1,2,3  and not just 3 :-)
;;;
(defun tinygnus-gnus-group-get-news-symbolic (elt)
  "Ask for symbolic name which represents level where to get news.
f ELT is nil then gel news for all groups.

References:
  `tinygnus--get-news-symbolic-levels'"
  (interactive
   (let ((table tinygnus--get-news-symbolic-levels)
	 ans)
     (setq
      ans
      (completing-read
       "Get new nwes [empty=all levels]: "
       tinygnus--get-news-symbolic-levels nil nil))
     (list (assoc ans table) )))
  (let ((cdr-elt (if elt
		     (cdr elt))))
    (if (integerp cdr-elt)              ; 1-- > '(1)
        (setq cdr-elt (ti::list-make cdr-elt)))
    (cond
     ((null elt)
      (message "Reading all cdr-elts.")
      (sit-for 1)
      (call-interactively 'gnus-group-get-new-news))
     ((fboundp cdr-elt)
      (call-interactively cdr-elt))
     ((ti::listp cdr-elt)
      (dolist (n cdr-elt)
        (message (format "Reading level %d" n)) (sit-for 0.5)
        (gnus-group-get-new-news n))))))

;;}}}
;;{{{ Debugging Gnus

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-debug-insert-line (key value &optional id)
  "Insert KEY and VALUE into buffer. Optionally prefix with function ID."
  (with-current-buffer (get-buffer-create tinygnus--debug-buffer)
    (ti::pmax)
    (insert (format "  %s%-30s: %s\n"
                    (if (null id)
                        ""
                      (format "  [%s] " (ti::string-value id)))
                    (if (stringp key) key (prin1-to-string key))
                    (ti::string-value value)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinygnus-debug-gnus-macro 'lisp-indent-function 1)
(put 'tinygnus-debug-gnus-macro 'edebug-form-spec '(body))
(defmacro tinygnus-debug-gnus-macro (func &rest body)
  "Instantiate `pr' function to print debug information about FUNC."
  `(flet ((pr (x y)
	      (tinygnus-gnus-debug-insert-line x y ,func)))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nnml-group-alist-p (group)
  "Check if GROUP is in `nnml-group-alist'."
  (assoc group nnml-group-alist))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-newsrc-alist (function)
  "Return elts from `gnus-newsrc-alist' according to FUNCTION."
  (let (list
        ;;      method
        ;;      backend
        ;;      server
        group)
    ;; (("dummy.group" 0 nil) ("comp.security.ssh" 3 nil nil nil) ...
    (dolist (elt gnus-newsrc-alist)
      (setq
;;;         method  (gnus-find-method-for-group group)
;;;         backend (car method)
;;;         server  (cdr method)
       group   (car elt))
      (when (funcall function group)
        (push elt list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nnml-file-range (dir)
  "Find the article range in directory (FIRST . LAST)."
  (let* ((files (directory-files dir nil "^[0-9]+$"))
         (list  (sort
                 files
                 (lambda (a b)
                   (< (string-to-number a) (string-to-number b))))))
    (when list
      (cons (string-to-number (car list))
            (string-to-number (car (nreverse list))) ))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-group-directory (group)
  "Return directory for GROUP."
  (let* ((method  (gnus-find-method-for-group group))
         (type    (nth 0 method)) ;; 'nnml
;;;      (server  (or (nth 1 (assoc 'nnml-address method))
;;;                   (nth 1 method)))
         base)
    (cond
     ((string-match (symbol-name type) "nnml")
      (setq base
            (or (nth 1 (assoc 'nnml-directory method))
                nnml-directory))
      (nnheader-concat base (gnus-group-real-name group)))
     (t
      (error "TinyGnus: Non-nnm;l backends not implemented.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-nnml-find-file (buffer group &optional nbr)
  "Find to BUFFER a nnml GROUP article NBR or last article."
  (let* ((dir   (tinygnus-group-directory group))
         (file  (or nbr
                    (cdr (tinygnus-nnml-file-range dir))))
         (path  (concat (file-name-as-directory dir)
                        (number-to-string file))))
    (with-current-buffer (get-buffer-create buffer)
      (ti::pmax)
      (insert-file-contents-literally path)
      (current-buffer))))

;;; ----------------------------------------------------------------------
;;; ("sfnet.atk.laitteet.pc" (85772 . 90896))
;;;
(defun tinygnus-gnus-debug-update-nnml-group-alist (group dir &optional replace)
  "Update `nnml-group-alist' to have the GROUP with DIR.
Possibly REPLCE existing entry."
  (let ((exist-p (tinygnus-nnml-group-alist-p group))
        range)
    (if (not (file-directory-p dir))
        (message "TinyLisp: No directory %s %s" group dir)
      (when (or replace
                (null exist-p))
        (if (not (setq range (tinygnus-nnml-file-range dir)))
            (message "TinyLisp: No files %s %s" group dir)
          (message "TinyGnus: Adding to `nnml-group-alist' %s" dir)
          (if replace
              (aput 'nnml-group-alist group range))
          (push (list group range) nnml-group-alist))))))

;;; 5.8.2
(defadvice gnus-open-server (around tinygnus-debug dis)
  ;; (gnus-command-method)
  ;; "Open a connection to GNUS-COMMAND-METHOD."
  (flet ((pr (x y)
             (tinygnus-gnus-debug-insert-line x y 'gnus-open-server )))
    (pr '(CALL-ARGS gnus-command-method)
        (list gnus-command-method))
    (when (stringp gnus-command-method)
      (setq gnus-command-method (gnus-server-to-method gnus-command-method)))
    (let ((elem (assoc gnus-command-method gnus-opened-servers)))
      ;; If this method was previously denied, we just return nil.
      (pr 'gnus-opened-servers  elem)
      (setq
       ad-return-value
       (if (eq (nth 1 elem) 'denied)
           (progn
             (gnus-message 1 "Denied server")
             nil)
         ;; Open the server.
         (pr '(gnus-get-function gnus-command-method 'open-server)
             (gnus-get-function gnus-command-method 'open-server))
         (let ((result
                (funcall (gnus-get-function gnus-command-method 'open-server)
                         (nth 1 gnus-command-method)
                         (nthcdr 2 gnus-command-method))))
           ;; If this hasn't been opened before, we add it to the list.
           (unless elem
             (setq elem (list gnus-command-method nil)
                   gnus-opened-servers (cons elem gnus-opened-servers)))
           ;; Set the status of this server.
           (setcar (cdr elem) (if result 'ok 'denied))
           ;; Return the result from the "open" call.
           result)))
      (pr 'RETURN-VALUE ad-return-value))))

;;; 5.8.2
(defadvice gnus-summary-read-group-1 (around t-tinygnus-debug dis)
  "Output trace to tinygnus--debug-buffer"
  ;; (group show-all no-article kill-buffer no-display &optional select-articles)
  (flet ((pr (x y)
             (tinygnus-gnus-debug-insert-line x y 'gnus-summary-read-group-1 )))
    ;; Killed foreign groups can't be entered.
    (when (and (not (gnus-group-native-p group))
               (not (gnus-gethash group gnus-newsrc-hashtb)))
      (error "Dead non-native groups can't be entered"))
    (gnus-message 5 "Retrieving newsgroup: %s..." group)
    (let* ((new-group (gnus-summary-setup-buffer group))
           (quit-config (gnus-group-quit-config group))
           (did-select (and new-group (gnus-select-newsgroup
                                       group show-all select-articles))))
      (pr 'my-gnus-summary-read-group-1::new-group   new-group)
      (pr 'my-gnus-summary-read-group-1::quit-config quit-config)
      (pr 'my-gnus-summary-read-group-1::did-select  did-select)
      (cond
       ;; This summary buffer exists already, so we just select it.
       ((not new-group)
        (gnus-set-global-variables)
        (when kill-buffer
          (gnus-kill-or-deaden-summary kill-buffer))
        (gnus-configure-windows 'summary 'force)
        (gnus-set-mode-line 'summary)
        (gnus-summary-position-point)
        (message "")
        t)
       ;; We couldn't select this group.
       ((null did-select)
        (when (and (eq major-mode 'gnus-summary-mode)
                   (not (equal (current-buffer) kill-buffer)))
          (kill-buffer (current-buffer))
          (if (not quit-config)
              (progn
                ;; Update the info -- marks might need to be removed,
                ;; for instance.
                (gnus-summary-update-info)
                (set-buffer gnus-group-buffer)
                (gnus-group-jump-to-group group)
                (gnus-group-next-unread-group 1))
            (gnus-handle-ephemeral-exit quit-config)))
        (gnus-message 3 "Can't select group")
        nil)
       ;; The user did a `C-g' while prompting for number of articles,
       ;; so we exit this group.
       ((eq did-select 'quit)
        (and (eq major-mode 'gnus-summary-mode)
             (not (equal (current-buffer) kill-buffer))
             (kill-buffer (current-buffer)))
        (when kill-buffer
          (gnus-kill-or-deaden-summary kill-buffer))
        (if (not quit-config)
            (progn
              (set-buffer gnus-group-buffer)
              (gnus-group-jump-to-group group)
              (gnus-group-next-unread-group 1)
              (gnus-configure-windows 'group 'force))
          (gnus-handle-ephemeral-exit quit-config))
        ;; Finally signal the quit.
        (signal 'quit nil))
       ;; The group was successfully selected.
       (t
        (gnus-set-global-variables)
        ;; Save the active value in effect when the group was entered.
        (setq gnus-newsgroup-active
              (gnus-copy-sequence
               (gnus-active gnus-newsgroup-name)))
        ;; You can change the summary buffer in some way with this hook.
        (gnus-run-hooks 'gnus-select-group-hook)
        ;; Set any local variables in the group parameters.
        (gnus-summary-set-local-parameters gnus-newsgroup-name)
        (gnus-update-format-specifications
         nil 'summary 'summary-mode 'summary-dummy)
        (gnus-update-summary-mark-positions)
        ;; Do score processing.
        (when gnus-use-scoring
          (gnus-possibly-score-headers))
        ;; Check whether to fill in the gaps in the threads.
        (when gnus-build-sparse-threads
          (gnus-build-sparse-threads))
        ;; Find the initial limit.
        (if gnus-show-threads
            (if show-all
                (let ((gnus-newsgroup-dormant nil))
                  (gnus-summary-initial-limit show-all))
              (gnus-summary-initial-limit show-all))
          ;; When untreaded, all articles are always shown.
          (setq gnus-newsgroup-limit
                (mapcar
                 (lambda (header) (mail-header-number header))
                 gnus-newsgroup-headers)))
        ;; Generate the summary buffer.
        (unless no-display
          (gnus-summary-prepare))
        (when gnus-use-trees
          (gnus-tree-open group)
          (setq gnus-summary-highlight-line-function
                'gnus-tree-highlight-article))
        ;; If the summary buffer is empty, but there are some low-scored
        ;; articles or some excluded dormants, we include these in the
        ;; buffer.
        (when (and (zerop (buffer-size))
                   (not no-display))
          (cond (gnus-newsgroup-dormant
                 (gnus-summary-limit-include-dormant))
                ((and gnus-newsgroup-scored show-all)
                 (gnus-summary-limit-include-expunged t))))
        ;; Function `gnus-apply-kill-file' must be called in this hook.
        (gnus-run-hooks 'gnus-apply-kill-hook)
        (if (and (zerop (buffer-size))
                 (not no-display))
            (progn
              ;; This newsgroup is empty.
              (gnus-summary-catchup-and-exit nil t)
              (gnus-message 6 "No unread news")
              (when kill-buffer
                (gnus-kill-or-deaden-summary kill-buffer))
              ;; Return nil from this function.
              nil)
          ;; Hide conversation thread subtrees.  We cannot do this in
          ;; gnus-summary-prepare-hook since kill processing may not
          ;; work with hidden articles.
          (and gnus-show-threads
               gnus-thread-hide-subtree
               (gnus-summary-hide-all-threads))
          (when kill-buffer
            (gnus-kill-or-deaden-summary kill-buffer))
          ;; Show first unread article if requested.
          (if (and (not no-article)
                   (not no-display)
                   gnus-newsgroup-unreads
                   gnus-auto-select-first)
              (progn
                (gnus-configure-windows 'summary)
                (cond
                 ((eq gnus-auto-select-first 'best)
                  (gnus-summary-best-unread-article))
                 ((eq gnus-auto-select-first t)
                  (gnus-summary-first-unread-article))
                 ((gnus-functionp gnus-auto-select-first)
                  (funcall gnus-auto-select-first))))
            ;; Don't select any articles, just move point to the first
            ;; article in the group.
            (goto-char (point-min))
            (gnus-summary-position-point)
            (gnus-configure-windows 'summary 'force)
            (gnus-set-mode-line 'summary))
          (when (get-buffer-window gnus-group-buffer t)
            ;; Gotta use windows, because recenter does weird stuff if
            ;; the current buffer ain't the displayed window.
            (let ((owin (selected-window)))
              (select-window (get-buffer-window gnus-group-buffer t))
              (when (gnus-group-goto-group group)
                (recenter))
              (select-window owin)))
          ;; Mark this buffer as "prepared".
          (setq gnus-newsgroup-prepared t)
          (gnus-run-hooks 'gnus-summary-prepared-hook)
          (setq ad-return-value t)))))))

;; 5.8.2
(defadvice gnus-select-newsgroup (around tinygnus-debug dis)
  ;; (group &optional read-all select-articles)
  "Output trace to tinygnus--debug-buffer"
  ;; (group &optional read-all select-articles)
  ;;  "Select newsgroup GROUP.
  ;;If READ-ALL is non-nil, all articles in the group are selected.
  ;; If SELECT-ARTICLES, only select those articles from GROUP."
  (flet ((pr (x y)
             (tinygnus-gnus-debug-insert-line x y 'gnus-select-newsgroup)))
    (pr '(CALL-ARGS group read-all select-articles)
        (list group read-all select-articles))

    (let* ((entry (gnus-gethash group gnus-newsrc-hashtb))
           ;;!!! Dirty hack; should be removed.
           (gnus-summary-ignore-duplicates
            (if (eq (car (gnus-find-method-for-group group)) 'nnvirtual)
                t
              gnus-summary-ignore-duplicates))
           (info (nth 2 entry))
           articles fetched-articles cached)
      (pr 'gnus-current-select-method gnus-current-select-method)
      (pr '(gnus-find-method-for-group group) (gnus-find-method-for-group group))
      (unless (gnus-check-server
               (setq gnus-current-select-method
                     (gnus-find-method-for-group group)))
        (error "Couldn't open server"))
      (or (and entry (not (eq (car entry) t))) ; Either it's active...
          (gnus-activate-group group)   ; Or we can activate it...
          (progn                        ; Or we bug out.
            (when (equal major-mode 'gnus-summary-mode)
              (kill-buffer (current-buffer)))
            (error "Couldn't request group %s: %s"
                   group (gnus-status-message group))))
      (unless (gnus-request-group group t)
        (when (equal major-mode 'gnus-summary-mode)
          (kill-buffer (current-buffer)))
        (error "Couldn't request group %s: %s"
               group (gnus-status-message group)))
      (setq gnus-newsgroup-name group)
      (setq gnus-newsgroup-unselected nil)
      (setq gnus-newsgroup-unreads (gnus-list-of-unread-articles group))
      (gnus-summary-setup-default-charset)
      ;; Adjust and set lists of article marks.
      (when info
        (gnus-adjust-marked-articles info))
      ;; Kludge to avoid having cached articles nixed out in virtual groups.
      (when (gnus-virtual-group-p group)
        (setq cached gnus-newsgroup-cached))
      (setq gnus-newsgroup-unreads
            (gnus-set-difference
             (gnus-set-difference gnus-newsgroup-unreads gnus-newsgroup-marked)
             gnus-newsgroup-dormant))
      (setq gnus-newsgroup-processable nil)
      (gnus-update-read-articles group gnus-newsgroup-unreads)
      (if (setq articles select-articles)
          (setq gnus-newsgroup-unselected
                (gnus-sorted-intersection
                 gnus-newsgroup-unreads
                 (gnus-sorted-complement gnus-newsgroup-unreads articles)))
        (setq articles (gnus-articles-to-read group read-all)))

      (setq
       ad-return-value
       (cond
        ((null articles)
         ;;(gnus-message 3 "Couldn't select newsgroup -- no articles to display")
         'quit)
        ((eq articles 0) nil)
        (t
         ;; Init the dependencies hash table.
         (setq gnus-newsgroup-dependencies
               (gnus-make-hashtable (length articles)))
         (gnus-set-global-variables)
         ;; Retrieve the headers and read them in.
         (gnus-message 5 "Fetching headers for %s..." gnus-newsgroup-name)
         (setq gnus-newsgroup-headers
               (if (eq 'nov
                       (setq gnus-headers-retrieved-by
                             (gnus-retrieve-headers
                              articles gnus-newsgroup-name
                              ;; We might want to fetch old headers, but
                              ;; not if there is only 1 article.
                              (and (or (and
                                        (not (eq gnus-fetch-old-headers 'some))
                                        (not (numberp gnus-fetch-old-headers)))
                                       (> (length articles) 1))
                                   gnus-fetch-old-headers))))
                   (gnus-get-newsgroup-headers-xover
                    articles nil nil gnus-newsgroup-name t)
                 (gnus-get-newsgroup-headers)))
         (gnus-message 5 "Fetching headers for %s...done" gnus-newsgroup-name)
         ;; Kludge to avoid having cached articles nixed out in virtual groups.
         (when cached
           (setq gnus-newsgroup-cached cached))
         ;; Suppress duplicates?
         (when gnus-suppress-duplicates
           (gnus-dup-suppress-articles))
         ;; Set the initial limit.
         (setq gnus-newsgroup-limit (copy-sequence articles))
         ;; Remove canceled articles from the list of unread articles.
         (setq gnus-newsgroup-unreads
               (gnus-set-sorted-intersection
                gnus-newsgroup-unreads
                (setq fetched-articles
                      (mapcar (lambda (headers) (mail-header-number headers))
                              gnus-newsgroup-headers))))
         ;; Removed marked articles that do not exist.
         (gnus-update-missing-marks
          (gnus-sorted-complement fetched-articles articles))
         ;; We might want to build some more threads first.
         (when (and gnus-fetch-old-headers
                    (eq gnus-headers-retrieved-by 'nov))
           (if (eq gnus-fetch-old-headers 'invisible)
               (gnus-build-all-threads)
             (gnus-build-old-threads)))
         ;; Let the Gnus agent mark articles as read.
         (when gnus-agent
           (gnus-agent-get-undownloaded-list))
         ;; Remove list identifiers from subject
         (when gnus-list-identifiers
           (gnus-summary-remove-list-identifiers))
         ;; Check whether auto-expire is to be done in this group.
         (setq gnus-newsgroup-auto-expire
               (gnus-group-auto-expirable-p group))
         ;; Set up the article buffer now, if necessary.
         (unless gnus-single-article-buffer
           (gnus-article-setup-buffer))
         ;; First and last article in this newsgroup.
         (when gnus-newsgroup-headers
           (setq gnus-newsgroup-begin
                 (mail-header-number (car gnus-newsgroup-headers))
                 gnus-newsgroup-end
                 (mail-header-number
                  (gnus-last-element gnus-newsgroup-headers))))
         ;; GROUP is successfully selected.
         (or gnus-newsgroup-headers t))))
      (pr 'RETURN-VALUE ad-return-value))))

;;; 5.8.2
(defadvice gnus-summary-read-group-1 (around tinygnus-debug dis)
  ;; (group show-all no-article kill-buffer no-display &optional select-articles)
  ;; Killed foreign groups can't be entered.
  (tinygnus-debug-gnus-macro 'gnus-summary-read-group-1
    (pr 'CALL-ARGS
	(list group show-all no-article kill-buffer no-display select-articles))
    (when (and (not (gnus-group-native-p group))
	       (not (gnus-gethash group gnus-newsrc-hashtb)))
      (error "Dead non-native groups can't be entered"))
    (gnus-message 5 "Retrieving newsgroup: %s..." group)
    (let* ((new-group (gnus-summary-setup-buffer group))
	   (quit-config (gnus-group-quit-config group))
	   (did-select (and new-group (gnus-select-newsgroup
				       group show-all select-articles))))
      (cond
       ;; This summary buffer exists already, so we just select it.
       ((not new-group)
	(gnus-set-global-variables)
	(when kill-buffer
	  (gnus-kill-or-deaden-summary kill-buffer))
	(gnus-configure-windows 'summary 'force)
	(gnus-set-mode-line 'summary)
	(gnus-summary-position-point)
	(message "")
	t)
       ;; We couldn't select this group.
       ((null did-select)
	(when (and (eq major-mode 'gnus-summary-mode)
		   (not (equal (current-buffer) kill-buffer)))
	  (kill-buffer (current-buffer))
	  (if (not quit-config)
	      (progn
		;; Update the info -- marks might need to be removed,
		;; for instance.
		(gnus-summary-update-info)
		(set-buffer gnus-group-buffer)
		(gnus-group-jump-to-group group)
		(gnus-group-next-unread-group 1))
	    (gnus-handle-ephemeral-exit quit-config)))
	(gnus-message 3 "Can't select group")
	nil)
       ;; The user did a `C-g' while prompting for number of articles,
       ;; so we exit this group.
       ((eq did-select 'quit)
	(and (eq major-mode 'gnus-summary-mode)
	     (not (equal (current-buffer) kill-buffer))
	     (kill-buffer (current-buffer)))
	(when kill-buffer
	  (gnus-kill-or-deaden-summary kill-buffer))
	(if (not quit-config)
	    (progn
	      (set-buffer gnus-group-buffer)
	      (gnus-group-jump-to-group group)
	      (gnus-group-next-unread-group 1)
	      (gnus-configure-windows 'group 'force))
	  (gnus-handle-ephemeral-exit quit-config))
	;; Finally signal the quit.
	(signal 'quit nil))
       ;; The group was successfully selected.
       (t
	(gnus-set-global-variables)
	;; Save the active value in effect when the group was entered.
	(setq gnus-newsgroup-active
	      (gnus-copy-sequence
	       (gnus-active gnus-newsgroup-name)))
	;; You can change the summary buffer in some way with this hook.
	(gnus-run-hooks 'gnus-select-group-hook)
	;; Set any local variables in the group parameters.
	(gnus-summary-set-local-parameters gnus-newsgroup-name)
	(gnus-update-format-specifications
	 nil 'summary 'summary-mode 'summary-dummy)
	(gnus-update-summary-mark-positions)
	;; Do score processing.
	(when gnus-use-scoring
	  (gnus-possibly-score-headers))
	;; Check whether to fill in the gaps in the threads.
	(when gnus-build-sparse-threads
	  (gnus-build-sparse-threads))
	;; Find the initial limit.
	(if gnus-show-threads
	    (if show-all
		(let ((gnus-newsgroup-dormant nil))
		  (gnus-summary-initial-limit show-all))
	      (gnus-summary-initial-limit show-all))
	  ;; When untreaded, all articles are always shown.
	  (setq gnus-newsgroup-limit
		(mapcar
		 (lambda (header) (mail-header-number header))
		 gnus-newsgroup-headers)))
	;; Generate the summary buffer.
	(unless no-display
	  (gnus-summary-prepare))
	(when gnus-use-trees
	  (gnus-tree-open group)
	  (setq gnus-summary-highlight-line-function
		'gnus-tree-highlight-article))
	;; If the summary buffer is empty, but there are some low-scored
	;; articles or some excluded dormants, we include these in the
	;; buffer.
	(when (and (zerop (buffer-size))
		   (not no-display))
	  (cond (gnus-newsgroup-dormant
		 (gnus-summary-limit-include-dormant))
		((and gnus-newsgroup-scored show-all)
		 (gnus-summary-limit-include-expunged t))))
	;; Function `gnus-apply-kill-file' must be called in this hook.
	(gnus-run-hooks 'gnus-apply-kill-hook)
	(if (and (zerop (buffer-size))
		 (not no-display))
	    (progn
	      ;; This newsgroup is empty.
	      (gnus-summary-catchup-and-exit nil t)
	      (gnus-message 6 "No unread news")
	      (when kill-buffer
		(gnus-kill-or-deaden-summary kill-buffer))
	      ;; Return nil from this function.
	      nil)
	  ;; Hide conversation thread subtrees.  We cannot do this in
	  ;; gnus-summary-prepare-hook since kill processing may not
	  ;; work with hidden articles.
	  (and gnus-show-threads
	       gnus-thread-hide-subtree
	       (gnus-summary-hide-all-threads))
	  (when kill-buffer
	    (gnus-kill-or-deaden-summary kill-buffer))
	  ;; Show first unread article if requested.
	  (if (and (not no-article)
		   (not no-display)
		   gnus-newsgroup-unreads
		   gnus-auto-select-first)
	      (progn
		(gnus-configure-windows 'summary)
		(cond
		 ((eq gnus-auto-select-first 'best)
		  (gnus-summary-best-unread-article))
		 ((eq gnus-auto-select-first t)
		  (gnus-summary-first-unread-article))
		 ((gnus-functionp gnus-auto-select-first)
		  (funcall gnus-auto-select-first))))
	    ;; Don't select any articles, just move point to the first
	    ;; article in the group.
	    (goto-char (point-min))
	    (gnus-summary-position-point)
	    (gnus-configure-windows 'summary 'force)
	    (gnus-set-mode-line 'summary))
	  (when (get-buffer-window gnus-group-buffer t)
	    ;; Gotta use windows, because recenter does weird stuff if
	    ;; the current buffer ain't the displayed window.
	    (let ((owin (selected-window)))
	      (select-window (get-buffer-window gnus-group-buffer t))
	      (when (gnus-group-goto-group group)
		(recenter))
	      (select-window owin)))
	  ;; Mark this buffer as "prepared".
	  (setq gnus-newsgroup-prepared t)
	  (gnus-run-hooks 'gnus-summary-prepared-hook)
	  (setq ad-return-value t)))))))

;;; 5.8.2
(defadvice gnus-activate-group (around tinygnus-debug dis)
  "Output trace to tinygnus--debug-buffer"
  ;; (group &optional scan dont-check method)
  ;; Check whether a group has been activated or not.
  ;; If SCAN, request a scan of that group as well.
  (tinygnus-debug-gnus-macro 'gnus-activate-group
    (pr '(CALL-ARGS group &optional scan dont-check method)
	(list group scan dont-check method))
    (let ((method (or method (inline (gnus-find-method-for-group group))))
	  active)
      (pr 'method method)
      (setq
       ad-return-value
       (and (inline (gnus-check-server method))
	    ;; We escape all bugs and quit here to make it posxsible to
	    ;; continue if a group is so out-there that it reports bugs
	    ;; and stuff.
	    (progn
	      (and scan
		   (gnus-check-backend-function 'request-scan (car method))
		   (gnus-request-scan group method))
	      t)
	    (condition-case ()
		(inline (gnus-request-group group dont-check method))
                                        ;(error nil)
	      (quit nil))
	    (setq active (gnus-parse-active))
	    (unless active
	      (pr "(parse-active)NNTP buffer conatins no data"
		  nntp-server-buffer)
	      (pr 'gnus-parse-active active))
	    ;; If there are no articles in the group, the GROUP
	    ;; command may have responded with the `(0 . 0)'.  We
	    ;; ignore this if we already have an active entry
	    ;; for the group.
	    (if (and (zerop (car active))
		     (zerop (cdr active))
		     (gnus-active group))
		(gnus-active group)
	      (gnus-set-active group active)
	      ;; Return the new active info.
	      active))))))

;;;  --> nnagent-request-scan calls this too
;;;
;;; 5.8.2
(defadvice nnml-request-group (around tinygnus-debug dis)
  ;; (group &optional server dont-check)
  "Output trace to tinygnus--debug-buffer"
  (tinygnus-debug-gnus-macro 'nnml-request-group
    (pr '(CALL-ARGS group dont-check gnus-command-method)
	(list group dont-check gnus-command-method))
    (setq
     ad-return-value
     (let ((pathname-coding-system 'binary))
       (cond ((not (nnml-possibly-change-directory group server))
	      (nnheader-report 'nnml "Invalid group (no such directory)"))
	     ((not (file-exists-p nnml-current-directory))
	      (nnheader-report 'nnml
			       "Directory %s does not exist"
			       nnml-current-directory))
	     ((not (file-directory-p nnml-current-directory))
	      (nnheader-report 'nnml
			       "%s is not a directory"
			       nnml-current-directory))
	     (dont-check (nnheader-report 'nnml "Group %s selected" group)
			 t)
	     (t (nnheader-re-read-dir nnml-current-directory)
		(nnmail-activate 'nnml)
		(let ((active (nth 1 (assoc group nnml-group-alist))))
		  (if (not active)
		      (nnheader-report 'nnml "No such group: %s" group)
		    (nnheader-report 'nnml "Selected group %s" group)
		    (nnheader-insert "211 %d %d %d %s
" (max (1+ (- (cdr active) (car active))) 0) (car active) (cdr active) group)))))))))

(defadvice nnml-possibly-change-directory (around tinygnus-debug dis)
  ;; (group &optional server)
  "Output trace to tinygnus--debug-buffer"
  (tinygnus-debug-gnus-macro 'nnml-possibly-change-directory
    (pr '(CALL-ARGS group server) (list group server))
    (when (and server
	       (not (nnml-server-opened server)))
      (nnml-open-server server))
    (setq
     ad-return-value
     (if (not group)
	 t
       (let ((pathname (nnmail-group-pathname group nnml-directory))
	     (pathname-coding-system 'binary))
	 (pr 'nnmail-group-pathname pathname)
	 (pr 'nnml-current-directory nnml-current-directory)
	 (when (not (equal pathname nnml-current-directory))
	   (setq nnml-current-directory pathname
		 nnml-current-group group
		 nnml-article-file-alist nil))
	 (file-exists-p nnml-current-directory))))
    (pr 'RETURN-VALUE ad-return-value)))

;;; 5.8.2
(defadvice gnus-request-group (around tinygnus-debug dis)
  ;; (defun my-gnus-request-group (group &optional dont-check gnus-command-method)
  ;; (group &optional dont-check gnus-command-method)
  ;; "Request GROUP.  If DONT-CHECK, no information is required."
  "Output trace to tinygnus--debug-buffer"
  (tinygnus-debug-gnus-macro 'gnus-request-group
    (pr '(CALL-ARGS group &optional dont-check gnus-command-method)
	(list group dont-check gnus-command-method))
    (let ((gnus-command-method
	   (or gnus-command-method (inline (gnus-find-method-for-group group)))))
      (when (stringp gnus-command-method)
	(setq gnus-command-method
	      (inline (gnus-server-to-method gnus-command-method)))
	(pr 'gnus-command-method gnus-command-method))
      (let* ((function (inline (gnus-get-function gnus-command-method
						  'request-group)))
	     (group    (gnus-group-real-name group))
	     (server   (nth 1 gnus-command-method))
	     ret
	     stat
	     dir)
	(pr 'FUNCALL    function)
	(pr 'FUNCALL-SYMBOL-FUNC (symbol-function function))
	(pr 'GROUP      group)
	(pr 'SERVER     server)
	(pr 'DONT-CHECK dont-check)
	(when (string-match "nnml-request-group"
			    (prin1-to-string (symbol-function function)))
	  (pr '(nnml-server-opened server) (nnml-server-opened server))
	  ;; FIXME: nnml-directory may be in server parameters
	  (setq dir (nnmail-group-pathname group nnml-directory))
	  (pr '(nnmail-group-pathname group nnml-directory) dir)
	  (pr 'nnml-directory nnml-directory)
	  (pr 'nnml-current-directory nnml-current-directory)
	  (setq stat (assoc group nnml-group-alist))
	  (pr  '(assoc group nnml-group-alist) stat)
	  (unless stat
	    (pr  "ERROR: Gnus doesn't know about ACTIVE file" "")
	    (pr  'nnml-group-alist nnml-group-alist)

	    (when (and (file-directory-p dir)
		       (or (file-exists-p (concat dir "active"))
			   (file-exists-p (concat dir ".agentview")))
		       (y-or-n-p "Group not in `nnml-group-alist'. Update? "))
	      (tinygnus-gnus-debug-update-nnml-group-alist group dir)))
	  (unless (string-match "nnml" group)
	    (pr "ERROR: group name doesn not contain NNML?"
		"Gnus can't read group!!!")))
	(setq ret
	      (funcall function group server dont-check))
	(pr 'nnml-status-string  nnml-status-string)
	(pr 'RETURN-VALUE ret)
	(setq ad-return-value ret)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-debug-investigate-problem (group)
  "Debug why you can't select NNML/Agent NNTP group."
  (interactive
   (list
    (completing-read
     "TinyGnus group to debug: "
     (ti::list-to-assoc-menu (list (gnus-group-group-name))))))
  (let* ((method  (gnus-find-method-for-group group))
         (server  (nth 1 method))
         (buffer  (get-buffer-create tinygnus--debug-buffer))
         info
         info-method
         elt
         tmp1
         tmp2)
    ;;  make shorter function name
    (flet ((pr (x y)
               (tinygnus-gnus-debug-insert-line x y)))
      (with-current-buffer buffer
        (tinygnus-gnus-debug-on)
        (ti::pmax)
        (insert (format "\nGNUS DEBUG SESSION (group: %s) %s\n\n"
                        group
                        (ti::date-standard-date 'minutes)))
        (pr 'nnml-directory nnml-directory)
        (pr 'gnus-agent-directory gnus-agent-directory)
        (pr 'gnus-find-method-for-group             method)
        (pr 'request-group (gnus-get-function method 'request-group))
        (pr 'gnus-group-real-name   (gnus-group-real-name group))
        (pr 'gnus-group-method      (gnus-group-method group))
        (pr 'gnus-group-real-prefix (gnus-group-real-prefix group))
        (pr 'gnus-server-status (gnus-server-status method))
        (pr 'gnus-server-opened (gnus-server-opened method))
        (pr '(gnus-group-find-parameter nnml-directory)
            (gnus-group-find-parameter nnml-directory))
        (pr '(gnus-info-params (gnus-get-info group))
            (gnus-info-params (gnus-get-info group)))
        (pr '(assoc server gnus-server-alist)
            (assoc server gnus-server-alist))
        (pr 'gnus-group-name-to-method (gnus-group-name-to-method group))
        (pr 'gnus-server-to-method (gnus-server-to-method gnus-command-method))
        (pr '(gnus-active group) (gnus-active group))
        (pr '(gnus-check-server method) (gnus-check-server method))
        (pr 'gnus-agent-covered-methods gnus-agent-covered-methods)
        (pr '(gnus-methods-using 'respool) (gnus-methods-using 'respool))
        ;; ....................................... Active article list ...
        (setq tmp1 (gnus-active group))
        (setq tmp2 (gnus-activate-group group))
        (pr '(gnus-active group) tmp1)
        (pr '(gnus-activate-group group) tmp2)
        (unless (or tmp1 tmp2)
          (pr "ERROR: Gnus DOES NOT HAVE INFO ABOUT FILE RANGE. (active)"
              ""))
        ;; .................................................... server ...
        (setq elt (assoc method gnus-opened-servers))
        (pr 'gnus-opened-servers elt)
        (pr '(gnus-get-function method 'open-server)
            (gnus-get-function method 'open-server))
        (pr '(gnus-request-group group nil method)
            (gnus-request-group group nil method))
        (setq info (gnus-get-info group))
        (pr 'gnus-get-info info)
        (pr 'nnml-directory nnml-directory)
        (pr 'nnml-current-directory nnml-current-directory)
        (setq info-method (gnus-info-method info))
        (pr "(gnus-info-method info)" info-method)
        (pr 'gnus-server-extend-method
            (gnus-server-extend-method group info-method))
        (pr 'gnus-group-entry (gnus-group-entry group))
        ;; (gnus-activate-group group)
        (tinygnus-gnus-debug-off)
        (display-buffer (current-buffer))
        (message "TinyGnus: Investigation ready. Check results from %s"
                 tinygnus--debug-buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-fix-nnml-groups ()
  "Step throught every nnml group and make sure they have
proper files created and Gnus knows about them via `nnml-group-alist'.

Agent groups are also NNML groups, so this will also step through nntp
backends when Gnus is unplugged."
  (interactive)
  (let ((list (tinygnus-gnus-newsrc-alist
	       (function
		(lambda (group)
		  (or (string-match "nnml" group)
		      (and (null gnus-plugged)
			   (eq (car (gnus-find-method-for-group group))
			       'nntp)))))))
	method
	server
	function
	open-server
	status
	status2
	real-name
	dir
	group)
    (message "TinyGnus: NNML-DIRECTORY is %s" nnml-directory)
    (message "TinyGnus: GNUS-AGENT-DIRECTORY is %s" gnus-agent-directory)
    (dolist (elt list)
      (setq group      (car elt)
            method     (gnus-find-method-for-group group)
            server     (nth 1 method)
            function   (gnus-get-function method 'request-group)
            real-name  (gnus-group-real-name group))
      (setq elt         (assoc method gnus-opened-servers))
      (setq open-server (gnus-get-function method 'open-server))
      (when (eq (nth 1 elt) 'denied)
        (message "TinyGnus: Group has denied server %s Trying to open %s ..."
                 group (prin1-to-string open-server))
        (setq status  (nth 1 method)
              status2 (nthcdr 2 method))
        (unless status
          (message "TinyGnus: Open Server didn't succeed"))
        (unless status2
          (message "TinyGnus: status 2 error %s" (prin1-to-string status2))))
      ;; see gnus-int.el gnus-request-group
      (unless (funcall function real-name (nth 1 method) nil)
        (message "TinyGnus: Group %s problem [%s] Trying to fix..."
                 group nnml-status-string)
        (nnml-possibly-change-directory group server)
        (setq dir (nnmail-group-pathname group nnml-directory))
        (ti::d! group nnml-current-directory dir)
        (if (not (file-directory-p dir))
            (message "TinyGnus: Unable to fix %s, no directory %s" group dir)
          (tinygnus-gnus-debug-update-nnml-group-alist group dir))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-debug-on (&optional verb)
  "Turn on Gnus debug. See `tinygnus--debug-buffer'. VERB.
If you experience a problem during entering a group

  cannot select group
  couldn't open server

Call this function and it will record the state of several Gnus functions
and call parameters and gateher them to `tinygnus--debug-buffer'. Examining
the results may reveal where the problem is."
  (interactive)
  (ti::verb)
  (let ((re "^tinygnus-debug"))
    (if verb
        (message
         (substitute-command-keys
          (concat
           "TinyGnus: Gnus debug is now on (advices on). "
           "Show debug \\[tinygnus-debug-show]."))))
    (ad-enable-regexp   re)
    ;; (ad-update-regexp   re)
    (ad-activate-regexp re)))

;;; ----------------------------------------------------------------------
;;;
(defun tinygnus-gnus-debug-off (&optional verb)
  "Turn off Gnus debug. See `tinygnus--debug-buffer'. VERB."
  (interactive)
  (ti::verb)
  (let ((re "^tinygnus-debug"))
    (if verb
        (message
         (substitute-command-keys
          (concat
           "TinyGnus: Gnus debug is now off (advices off). "
           "Show debug \\[tinygnus-debug-show]."))))
    (ad-disable-regexp re)
    (ad-update-regexp  re)))

;;}}}
;;{{{ Advice

;;; .......................................................... &advice ...

;;; ----------------------------------------------------------------------
;;;  Dormant handling is hard coded in gnus, and the fastest way to
;;;  show them is include them in summary generation phase.
;;;  Called by gnus-sum.el::gnus-summary-initial-limit
;;;
;;;  #Todo: 2000-01 puts gnus to infinite loop. Fix this.
;;;
(defadvice gnus-summary-limit-children (around tinygnus-show-dormants dis)
  "Replace function if `tinygnus--show-dormants' is t.
Make dormants immediately visible in non-nntp groups."
  (if (null tinygnus--show-dormants)
      ad-do-it
    ;; Return 1 if this subthread is visible and 0 if it is not
    (when (ad-get-arg 0)                ;thread flag
      (cond
       ;;   This part is copied from gnus-sum.el
       ((and (not (string-match "nntp" gnus-newsgroup-name))
             (let ((children
		    (if (cdr (ad-get-arg 0))
			(apply '+ (mapcar 'gnus-summary-limit-children
					  (cdr (ad-get-arg 0))))
		      0))
		   (number (mail-header-number (car (ad-get-arg 0)))))
               ;;  In original gnus this test would suppress dormants.
               (when (and (memq number gnus-newsgroup-dormant)
                          (zerop children))
                 (push number gnus-newsgroup-limit)
                 (setq ad-return-value 1)))))
       (t
        ad-do-it)))))

;;; ----------------------------------------------------------------------
;;;
(defadvice gnus-topic-read-group
  (around tinygnus-fast-read-unread-articles act)
  "Read only unread/newly arrived articles. If no new articles, read as usual.
If given prefix arg 2 x \\[universal-argument] (NO-THREADS) then all threads
with unread articles will be displayed.

To put it simply: When you see new articles in Group, entering the
group only shows those new articles. This makes reading group faster."
  (let* ((fid   "gnus-topic-read-group")
         (arg  (ad-get-arg 0))
         ;; See also (gnus-group-group-name)
         (groups (gnus-group-process-prefix nil))
         (group  (car groups))
         unread-arts)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;; Parameter GROUP is not defined if you hit SPC on TOPIC
    ;; to collapse or open it.
    (cond
     ((and group
           (or (null arg)  (equal arg '16))
           (eq 1 (length groups))
           (string-match "nnml" group)
           (not  (memq (car-safe  (gnus-group-method group)) '(nntp)))
           (let ((gnus-fetch-old-headers (if arg t nil)))
             (setq unread-arts
                   (gnus-list-of-unread-articles group))))
      (message "TinyGnus Advice: reading NEW articles.")
      (sit-for 0.2)
      (gnus-group-read-group nil t nil unread-arts))
     (t
      (message "TinyGnus Advice: Normal reading...")
      ;;  As usual, no new articles.
      ad-do-it))))

;;}}}
;;{{{ 19.34 compressed .eld support

;;; ..................................................... &compression ...
;;; - sometimes I have _very_ limited quota and I woul wish gnus would allow
;;;   using compresses files, but it doesn't by default.
;;; - These advices make Gnus use compressed startup files.
;;; - The functins are copied directly from Gnus kit and needed modifications
;;;   have been made.
;;;
;;; See also
;;;
;;; gnus.el::gnus    Find the current startup file name.
;;; (setq gnus-current-startup-file (gnus-make-newsrc-file gnus-startup-file))

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --+ &advice-start --
(when (and nil
           (string-match  tinygnus--gnus-version-for-advice gnus-version)
           (stringp tinygnus--z))

  (defadvice gnus-check-first-time-used (around tinygnus  act)
    "Replace function."
    (if (or (> (length gnus-newsrc-alist) 1)
            (file-exists-p (concat gnus-startup-file (concat ".eld" tinygnus--z)))
            (file-exists-p gnus-startup-file)
            (file-exists-p (concat gnus-startup-file ".el"))
            (file-exists-p (concat gnus-startup-file ".eld")))
        nil
      (gnus-message 6 "First time user; subscribing you to default groups")
      (unless (gnus-read-active-file-p)
        (gnus-read-active-file))
      (setq gnus-newsrc-last-checked-date (current-time-string))
      (let ((groups gnus-default-subscribed-newsgroups)
            group)
        (if (eq groups t)
            nil
          (setq groups (or groups gnus-backup-default-subscribed-newsgroups))
          (mapatoms
           (lambda (sym)
             (if (null (setq group (symbol-name sym)))
                 ()
               (let ((do-sub (gnus-matches-options-n group)))
                 (cond
                  ((eq do-sub 'subscribe)
                   (gnus-sethash group group gnus-killed-hashtb)
                   (funcall gnus-subscribe-options-newsgroup-method group))
                  ((eq do-sub 'ignore)
                   nil)
                  (t
                   (setq gnus-killed-list (cons group gnus-killed-list)))))))
           gnus-active-hashtb)
          (while groups
            (if (gnus-active (car groups))
                (gnus-group-change-level
                 (car groups) gnus-level-default-subscribed gnus-level-killed))
            (setq groups (cdr groups)))
          (gnus-group-make-help-group)
          (and gnus-novice-user
               (gnus-message 7 "`A k' to list killed groups"))))))

  (defun gnus-read-newsrc-file (&optional force)
    "Replace function. Optionally FORCE."
    (interactive)
    ;;Make sure this is defined
    (setq gnus-current-startup-file (gnus-make-newsrc-file gnus-startup-file))
    (let ((variables gnus-variable-list))
      (while variables
        (set (car variables) nil)
        (setq variables (cdr variables))))
    (let* ((newsrc-file gnus-current-startup-file)
           (quick-file (concat newsrc-file ".el")))
      (save-excursion
        ;; We always load the .newsrc.eld file.  If always contains
        ;; much information that can not be gotten from the .newsrc
        ;; file (ticked articles, killed groups, foreign methods, etc.)
        (gnus-read-newsrc-el-file quick-file)
        (if (and (file-exists-p gnus-current-startup-file)
                 (or force
                     (and (file-newer-than-file-p newsrc-file quick-file)
                          (file-newer-than-file-p
                           newsrc-file (concat quick-file "d" tinygnus--z)))
                     (not gnus-newsrc-alist)))
            ;; We read the .newsrc file.  Note that if there if a
            ;; .newsrc.eld file exists, it has already been read, and
            ;; the `gnus-newsrc-hashtb' has been created.  While reading
            ;; the .newsrc file, Gnus will only use the information it
            ;; can find there for changing the data already read -
            ;; ie. reading the .newsrc file will not trash the data
            ;; already read (except for read articles).
            (save-excursion
              (gnus-message 5 "Reading %s..." newsrc-file)
              (set-buffer (find-file-noselect newsrc-file))
              (buffer-disable-undo (current-buffer))
              (gnus-newsrc-to-gnus-format)
              (kill-buffer (current-buffer))
              (gnus-message 5 "Reading %s...done" newsrc-file)))
        ;; Read any slave files.
        (unless gnus-slave
          (gnus-master-read-slave-newsrc))
        ;; Convert old to new.
        (gnus-convert-old-newsrc))))

  (defadvice gnus-read-newsrc-el-file (around tinygnus act)
    "Replace function."
    (let ((ding-file (concat file "d" tinygnus--z)))
      ;; We always, always read the .eld file.
      (gnus-message 5 "Reading %s..." ding-file)
      (let (gnus-newsrc-assoc)
        (condition-case nil
            (load ding-file t t t)
          (error
           (gnus-error 1 "Error in %s" ding-file)))
        (when gnus-newsrc-assoc
          (setq gnus-newsrc-alist gnus-newsrc-assoc)))
      (gnus-make-hashtable-from-newsrc-alist)
      (when (file-newer-than-file-p file ding-file)
        ;; Old format quick file
        (gnus-message 5 "Reading %s..." file)
        ;; The .el file is newer than the .eld file, so we read that one
        ;; as well.
        (gnus-read-old-newsrc-el-file file))))

  (defadvice gnus-make-newsrc-file (around tinygnus act)
    "Replace function."
    (setq
     ad-return-value
     (let* ((file (expand-file-name file nil))
            (real-file (concat file "-" (nth 1 gnus-select-method))))
       (cond
        ((file-exists-p (concat real-file ".el" tinygnus--z))
         (concat real-file  ".el" tinygnus--z))
        ((file-exists-p (concat file tinygnus--z))
         (concat file tinygnus--z))
        ((or (file-exists-p real-file)
             (file-exists-p (concat real-file ".el"))
             (file-exists-p (concat real-file ".eld")))
         real-file)
        (t
         file)))))

  (defadvice gnus-save-newsrc-file (around tinygnus act)
    "Add compressed file support."
    ;; Note: We cannot save .newsrc file if all newsgroups are removed
    ;; from the variable gnus-newsrc-alist.
    (when (and (or gnus-newsrc-alist gnus-killed-list)
               gnus-current-startup-file)
      (save-excursion
        (if (and (or gnus-use-dribble-file gnus-slave)
                 (not force)
                 (or (not gnus-dribble-buffer)
                     (not (buffer-name gnus-dribble-buffer))
                     (zerop (save-excursion
                              (set-buffer gnus-dribble-buffer)
                              (buffer-size)))))
            (gnus-message 4 "(No changes need to be saved)")
          (run-hooks 'gnus-save-newsrc-hook)
          (if gnus-slave
              (gnus-slave-save-newsrc)
            ;; Save .newsrc.
            (when gnus-save-newsrc-file
              (gnus-message 5 "Saving %s..." gnus-current-startup-file)
              (gnus-gnus-to-newsrc-format)
              (gnus-message 5 "Saving %s...done" gnus-current-startup-file))
            ;; Save .newsrc.eld.
            (set-buffer (get-buffer-create " *Gnus-newsrc*"))
            (make-local-variable 'version-control)
            (setq version-control 'never)
            (setq buffer-file-name
                  (concat gnus-current-startup-file ".eld" tinygnus--z))
            (setq default-directory (file-name-directory buffer-file-name))
            (gnus-add-current-to-buffer-list)
            (buffer-disable-undo (current-buffer))
            (erase-buffer)
            (gnus-message 5 "Saving %s.eld..." gnus-current-startup-file)
            (gnus-gnus-to-quick-newsrc-format)
            (run-hooks 'gnus-save-quick-newsrc-hook)
            (save-buffer)
            (kill-buffer (current-buffer))
            (gnus-message
             5 "Saving %s.eld...done" gnus-current-startup-file))
          (gnus-dribble-delete-file)
          (gnus-group-set-mode-line)))))

;;; --++-- --++-- --++-- --++-- --++-- --++-- --++-- --++- &advice-end --
  ) ;; advice-end

;;}}}

(provide   'tinygnus)

(tinygnus-install)
(run-hooks 'tinygnus--load-hook)

;;; tinygnus.el ends here
