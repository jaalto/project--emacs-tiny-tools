;;; tinymail.el --- Mail add-ons. Report incoming mail, passwd, BBDB complete.

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

;; ....................................................... &t-install ...
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file:
;;
;;      (require 'tinymail-install)
;;      (require 'tinymail-install-extras)  ;; optional
;;
;;  Other setting you may wish to add:
;;
;;      ;;  Activate nice citation
;;      (add-hook 'tinymail--load-hook 'tinymail-install-citation)
;;
;;      ;;  If you want nice TAB to indent for your messages,
;;      ;;  add this. You TAB advances 4 spaces in the body of message.
;;      (autoload 'turn-on-tinytab-mode  "tinytab" "" t)
;;      (add-hook 'tinymail--mode-hook   'turn-on-tinytab-mode)
;;
;;      ;;  If you use NIS, use "ypcat passwd"
;;      (setq tinymail--password-cat-cmd "cat /etc/passwd")
;;
;;      ;;  Protect plain text email addresses in the body
;;      (add-hook 'mail-send-hook    'tinymail-buffer-email-address-scramble)
;;      (add-hook 'message-send-hook 'tinymail-buffer-email-address-scramble)
;;
;;   If you have any questions, use 'submit' function. In case of error
;;   or misbehavior, turn on the debug and send the debug result and
;;   describe what you did and what went wrong.
;;
;;      .. do what you did in mail buffer ..
;;      M-x tinymail-debug-toggle            Make sure debug is on
;;      C-u M-x tinymail-process-1           Run this if you got error signall
;;      M-x tinymail-submit-bug-report       And compose bug report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      o   Generate sendmail PLUS address: login@domain (Mr Foo+info)
;;          Works like real sendmail PLUS addressing:
;;          login+info@domain (Mr Foo)
;;      o   Generate anti-ube addresses to prevent UBE/Spam from arriving
;;          to your mailbox.
;;      o   Changes Fcc dynamically according to header content.
;;      o   Very easy TAB completion: two modes, alias and definition string.
;;          Also completes password file entries if your .mailrc doesn't
;;          contain a match.
;;      o   Easy interface for completing any field with TAB. E.g. complete
;;          `Followup-To:', `Gcc', `Newsgroups' and any user defined fields,
;;          like `Class' or `Priority'.
;;      o   Fcc/Gcc folder can have compression extension .gz or .Z
;;      o   When you reply, tour address is removed from CC to prevent
;;          duplicates.
;;      o   `mail-mode], Gnus `message-mode' and VM compatible.
;;      o   MIME support: turns on Multi part sending if buffer size is
;;          bigger than 50K.
;;
;;     BBDB supported
;;
;;      Search BBDB for partial matches when you complete *To* and *Cc*
;;      fields in header. E.g. if you remember person's address, "site" or
;;      something, hit just TAB and all found BBDB's `net' field completions
;;      are offered.
;;
;;      Notice that you have to _manually_ add full
;;      user name, phone number, whatever to the Net Field on order to
;;      complete to those items. The default `:' command adds only
;;      this:
;;
;;          Foo Bar
;;               net: abc@example.com
;;
;;      An in order to make that useful for completion purposes, you need to
;;      modify the `net' field with `C-o'
;;
;;          Foo Bar
;;               net: Foo Bar - Head of Skyscraper inc. <abc@example.com>
;;
;;      Now you can complete to any word found in the `net' line.
;;      If you want case sensitive completions, set this:
;;
;;          (setq tinymail--complete-bbdb-case-fold-search nil)
;;
;;  Installation note
;;
;;      This package installs itself to `mail-setup-hook' and you should
;;      know why if you try to get the package running for some other
;;      mail agent than Emacs mail, RMAIL, Gnus and VM where this package
;;      has been tested.
;;
;;      The `mail-setup-hook' is called *after* the basic headers, like
;;      `To' and `Subject', which are already in the buffer. Function
;;      `tinymail-mail' needs to read the contents of `To' in order to
;;      determine how it starts. It puts 1 or 2 spaces at the beginning of
;;      `To' field at the initial start, so that the packages `Cc' control
;;      is started correctly. When you use simple mail, `C-x' `m', the auto
;;      Cc feature addds 1 space (on) and when you hit reply, the auto Cc
;;      feature adds 2 spaces (off).
;;      So, if your mail agent doesn't call `mail-setup-hook', find similar
;;      hook that runs after the headers are in the buffer and install
;;      to that hook `tinymail-mail'.
;;
;;  Completion: Guess Completion feature
;;
;;      There are two basic completion modes: 'alias and 'string, which is
;;      selected via `tinymail--complete-mode'. They refer to your
;;      ~/.mailrc definitions: When you hit the completions key (TAB in
;;      headers) the current word is picked at point and searched from
;;      either of these two definition lists.
;;
;;          alias test "Mister Foo, Skyscraper Doing co. <foo@company.com>"
;;                |             |
;;                alias mode    string match mode
;;
;;      The caces 1-4 below present words that you can type into the `To' field
;;      before you hit the completion key, TAB.
;;
;;          1  To: company
;;          2  To: Foo
;;          3  To: sky
;;          4  To: mister
;;
;;      It doesn't matter what you type initially; it can be anything you
;;      remember from the person's definition string. TheTAB calls function
;;      `tinymail-complete-guess' and any of those lines, 1-4, will be
;;      replaced with
;;
;;          To: Mister Foo, Skyscraper Doing co. <foo@doing.com>
;;
;;      If there are more than one match, a completion list is displayed.
;;
;;     Completion and BBDB integration
;;
;;      The completion is integrated to BBDB, but you have to have
;;      BBDB present with appropriate (require 'bbdb). The fields
;;      in NET and NAME are searched by default, but you can make the
;;      completion feature to try ANY-FIELD if you change the value
;;      of `tinymail--complete-bbdb-fuzzy-method'. See variable documentation
;;      for complete description.
;;
;;     Accepting the found match from .mailrc
;;
;;      TinyMail supports running several completion functions so that
;;      the right match is inserted into the buffer. In order to
;;      discard the found match from .mailrc file, you can set a trigger
;;      to `tinymail--confirm-mailrc-regexp'. Suppose, you want to confirm
;;      completion whenever you are sending mail to your colleagues that
;;      work in "disney.com". You'd set:
;;
;;          (setq tinymail--confirm-mailrc-regexp "disney.com")
;;
;;      And if the match was picked from mailrc, you have a chance to
;;      reject the string and move on with other completion functions.
;;
;;          To: world
;;
;;      When you hit tab here, a string "info@disneyword.com" was found from
;;      the mailrc, but may not be what you want to insert. Because
;;      you had set `tinymail--confirm-mailrc-regexp', you get confirmation:
;;
;;          TinyMail: Use? info@disneyword.com
;;
;;      Where you can answer "n". The completion is canceled and all
;;      other completion function have a chance to find more
;;      suitable choice. (See Shared Shared Tab key explanation later)
;;
;;  Completion: Password table
;;
;;      In addition to .mailrc completion, there is support for completing
;;      entries found from *passwd* file. If the guess complete above fails
;;      the password file is examined if the mode is turned on. See
;;      variable
;;
;;          tinymail--password-mode
;;
;;      Which is set to t by default. When you complete password entries
;;      for the first time, building all necessary variables will take some
;;      time. After the password file completions have been parsed, the
;;      content is written to cache file
;;
;;          tinymail--password-file
;;
;;      Next time you need password completions, if this file exists,
;;      it will be read instead of heavy /etc/passwd file parsing. If you
;;      want to force reading the /etc/passwd again, just delete
;;      `tinymail--password-file' and it will be recreated next time
;;      password completion is used.
;;
;;  Completion: Custom completion of any header
;;
;;      You can complete any field by setting variable
;;      `tinymail--table-header-complete' For example to complete "Class" header,
;;      you would set the variable like this. See variable documentation
;;      for more information.
;;
;;          (require 'assoc)
;;          (aput
;;           'tinymail--table-header-complete
;;           "Class"                ;; Add new header for use with TAB
;;           (list
;;            '("confidential"       ;; completion list
;;              "private"
;;              "for internal use only"
;;              "personal private"
;;              "personal another")))
;;          ;; end example
;;
;;  CC field tracking
;;
;;      You can delete any elements from `Cc' field if you set variable
;;      `tinymail--cc-kill-regexp'. This feature can be used to delete
;;      e.g. your email addresses from the list of `Cc' recipients to
;;      to avoid getting duplicate copies of the mail when you reply.
;;
;;      At any time you can add two spaces in front of `Cc' field to
;;      disable this "kill" feature. This is desirable if you WANT to
;;      add a CC to your other email addresses. An example:
;;
;;        (setq tinymail--cc-kill-regexp \"me@here.at\")
;;
;;        To: some@example.com
;;        CC: me@here.at      << will be removed
;;        CC:  me@here.at     << NOT removed, because field has two spaces.
;;
;;  RMAIL Fcc field tracking
;;
;;      By default the `Fcc' is not added in your mail message, thus this
;;      package's automatic Fcc tracking isn't activated. Add following entry
;;      into your $HOME/.emacs to record your outgoing mail messages
;;
;;          (setq mail-archive-file-name "~/.RMAIL.out")
;;
;;      When Emacs sees that you have set this, it adds the Fcc field to
;;      your mail message. Alternatively you can press keys
;;
;;          C-c C-f C-f    ;; mail-fcc
;;
;;      in *mail* buffer and it asks you to insert the Fcc field.  Only
;;      now, when the `Fcc' is in the message, the automatic Fcc handling
;;      starts snooping around your headers and changing it if it finds a
;;      match from variable
;;
;;          tinymail--table-fcc
;;
;;      If you want to disable Fcc changing (and edit it by hand),
;;      put two spaces at front of the Fcc. like this:
;;
;;          FCC:  ~/.RMAIL.secondary
;;              ^^
;;
;;     Fcc and saving outgoing copy in compressed format
;;
;;      If you have an count that has quota limits, you want to save space
;;      as much as possible. You can save your outgoing mail copy in
;;      compressed format if you prepend the filename with ".gz" or
;;      ".Z". TinyMail will automatically load jka-compr if it sees any of
;;      those extensions. The fcc folder definition looks like this.
;;
;;          (defconst tinymail--table-fcc
;;             (list
;;              (list "elisp-archive"    " ~/.mail.elisp-post.gz")
;;              (list "bug-gnu"          " ~/.mail.bug.gz")
;;              (list "."                " ~/.mail.out.gz"))) ;; general
;;
;;      You _have_ to add (require 'jka-compr) is you want to use compresses
;;      RMAIL file.
;;
;;          ;;  first one is defined in paths.el
;;          (setq rmail-file-name        "~/RMAIL.gz")
;;          (setq mail-archive-file-name "~/.RMAIL.out.gz")
;;
;;  Gnus Gcc archiving
;;
;;      `Gcc' feature is similar to Fcc, but the Gcc is special to Gnus.
;;      All instruction you read above for Fcc are same for
;;      Gcc tracking feature. The table you have to configure is
;;
;;          tinymail--table-gcc
;;
;;      Before you start defining Gnus folders, you must create them from
;;      Gnus *Group* buffer with command `G' `m'. E.g. you may have
;;      created following Gnus folders for newsgroup posting
;;
;;          nnfolder+archive:post-pgp
;;          nnfolder+archive:post-emacs
;;          nnfolder+archive:post-gen
;;
;;      The code below sets the `Gcc' folder only once when you start
;;      composing message, probably a followup and there is a `Newsgroups'
;;      header in the buffer. But if you hit `R' or `r' to reply directly to
;;      person (or use message-mode for mailing), there is `To' header in
;;      the buffer. Only now this package changes the Gcc field according to `To'
;;      field contents. The code below is for Newsgroup posting.
;;
;;          (setq gnus-message-archive-group 'my-gnus-archive)
;;
;;          (defun my-gnus-archive  (group)
;;            "Archive outgoing mail to right group: Create the group by G m"
;;            (interactive)
;;            (let* ()
;;              (or (stringp group)                 ;No accidents...
;;                  (setq group ""))
;;              (cond
;;               ((string-match "pgp\\|anon\\|privacy" group)
;;                "nnfolder+archive:post-pgp")
;;               ((string-match "emacs\\|gnu" group)
;;                "nnfolder+archive:post-emacs")
;;               (t
;;                "nnfolder+archive:post-gen"))))
;;
;;  Feature: Sending message to mailing list
;;
;;      In Gnus you may have defined mailing lists like this
;;
;;          list.linux-announce
;;          list.ding
;;          list.java
;;
;;      And your personal work and mail groups with
;;
;;          mail.private
;;          mail.misc
;;
;;          work.documents
;;          work.fault
;;          work.customer
;;
;;      Daemon messages to junk.daemon, Spam to junk.spam and so on.
;;      Now suppose you are reading group `list.xxx' and you hit `f'
;;      to send followup to an article. Your composed message looks like this:
;;
;;          To: answer-to-person <foo@bar.com>
;;          Cc: <someone@list.com>, <list-foo@bar.com>
;;
;;      The Message goes to two people in the list and gets CC'd to
;;      list. Not what you want. You want simple:
;;
;;          To: <list-foo@bar.com>
;;
;;      And this is what this package does for you. All you need to do it to
;;      make sure the current group has Group parameter `to-list'.
;;      defined. You add one with `G' `p] From *Group* and typing
;;
;;          ((to-list . "The List FOO <list-foo@bar.com>"))
;;
;;      This feature is controlled by `tinymail--feature-hook' which contains
;;      function `tinymail-mail-send-to-list'. If you remove the function from
;;      the hook, this feature is disabled
;;
;;  Feature: Reporting incoming mail in local mail spool
;;
;;      Function to control mail reporting:
;;
;;          turn-on-tinymail-report-mail
;;          turn-off-tinymail-report-mail
;;
;;      When you load this package the Report Mail feature is activated.
;;      If you're running windowed Emacs, the X-drag bar (top of the frame)
;;      is used to display the last incoming mail and count of pending
;;      unread mail. Here the last message was from Mr. foo and the
;;      pending mail count in spool is six.
;;
;;          "foo@bar.com 6" ;; See variable `tinymail--report-format-string'
;;
;;      In non-windowed Emacs this same information is displayed in echo
;;      are instead. If you would like to have it always displayed in
;;      echo area, even in X environment, then set variable
;;      `tinymail--report-window-system' to nil before loading this package.
;;
;;      If you would like see more information about the arrived mail, you
;;      can adjust `tinymail--report-spool-buffer-control' e.g. to keep
;;      permanent record of incoming mail. Value 'keep says that the report
;;      mail buffer is kept when mail is queried, so you can glance it from
;;      time to to for full information about arrived messages.
;;
;;      If the only feature you want is the mail reporting functionality,
;;      you can activate it and disable all other settings with:
;;
;;          ;; Don't activate tinymail-mode
;;          (setq tinymail--enter-mail-hook-list nil)
;;          (require 'tinymail)
;;
;;     Setting up report mail notify program
;;
;;      The `tinymail--report-mail-notify-program' fetches the Berkeley Mailbox
;;      formatted information from mailboxes. The default program used
;;      is `from(1)', but in case you don't have it, a equivalent command
;;      "grep '^From ' $MAIL" is used. See also `frm(1)'
;;      and `nfrm(1)' `newmail(1)' and `mailfrom(1)' if you can
;;      find those in your system.
;;
;;      If you use Gnus and separate spool files, like you would do with
;;      Procmail, then you need to gather mail arrival information from all
;;      the spool files. Let's suppose you don't want to get notified on
;;      mailing list messages, but only messages saved to your private and
;;      work spool files:
;;
;;          ~/Mail/spool        or `nnmail-procmail-directory'
;;          mail.misc
;;          mail.private
;;          mail.programming
;;          mail.emacs
;;          mail.java
;;          ...
;;
;;          work.meetings
;;          work.docs
;;          work.customer
;;          ...
;;
;;      In that case you have to install custom mail notify program. A
;;      simple multiple mailbox grep will work here. Note, we also grep
;;      default $MAIL:
;;
;;          (setq tinymail--report-mail-notify-program
;;            (format
;;              "grep '^From ' %s %s %s "
;;              (or (getenv "MAIL") (error "No $MAIL defined"))
;;              (concat (expand-file-name "~/Mail/spool/") "mail.*")
;;              (concat (expand-file-name "~/Mail/spool/") "work.*")))
;;
;;      Take a look at variable `tinymail--report-spool-buffer-control'
;;      which has default value 'keep where the
;;      `tinymail--report-mail-notify-program' results are gathered. You
;;      may find it useful to keep the `tinymail--report-spool-buffer'
;;      *tinymail-mail-spool* visible in some frame to act like `biff(1)'.
;;      From there you can find more detailed information of incoming
;;      message queue, than the simple message count in echo-area or x-drag
;;      bar.
;;
;;      _Note_: XEmacs has package `reportmail.el'. In case that package
;;      is loaded, the report mail feature here is not installed.
;;
;;  Feature: Saving unused mail buffers on Emacs exit
;;
;;      This file installs one function to `kill-emacs-hook' that loops
;;      through all mail buffers and appends the buffer content to
;;
;;          tinymail--dead-mail-file
;;
;;      If you had some unfinished messages that you didn't yet send, you
;;      can restore the copy from this file when you restart emacs again.
;;      In Gnus `message-mode', you can use following to trash sent mail:
;;
;;          (setq message-kill-buffer-on-exit t)
;;
;;      If you don't want to use this feature, add following code to your
;;      $HOME/.emacs
;;
;;          (add-hook 'tinymail--load-hook 'my-tinymail--load-hook)
;;          (defun my-tinymail--load-hook  ()
;;            (remove-hook 'kill-emacs-hook 'tinymail-save-dead-mail))
;;
;;      _Note_: VM and Gnus can keep the sent mail buffer around. This
;;      package won't install `tinymail-save-dead-mail-maybe' the dead mail
;;      collector under
;;      Gnus and VM.
;;
;;  Feature: anti-ube email addresses
;;
;;     Philosophy
;;
;;      Changing the email address so that is is not pointing to your
;;      natural address is usually referred as "address munging". There are
;;      two schools that take firm position to express their views in this
;;      matter. Those who say that it is "plain wrong to munge address" and
;;      those who say "RFC does not require you to use REAL, returnable,
;;      address". It can be argued that the email address is property of
;;      an individual who can take measures to protect himself from getting
;;      into the email harvester's "2 billion email address on a CD for
;;      $100"
;;
;;      Here is an opinion whether it is right to munge the
;;      address according to RFC by Marty Fouts
;;      1997-11-05 in newsgroup gnus.emacs.gnus:
;;
;;      o   The real implementation of news software doesn't care if the from
;;          field is munged or not
;;      o   No RFC forces the address of the poster to be a *reachable* addr.
;;          It only requires such addresses to be syntactically correct.
;;      o   RFC 1036 _specifically_ states that it is not an Internet
;;          standard.
;;      o   News is a *public* forum. Mail is a *private*
;;          communication medium. Posting in a _public_ forum does not
;;          require that you give you access to _private_ address, just as
;;          speaking at a public meeting does not require that I give you give
;;          unlisted phone number.
;;
;;     Why to munge From address
;;
;;      o   Email address is one's own property. The reasons to munge are
;;          one's own. In perfect world you wouldn't need lock to your
;;          doors, but you do have them in houses. The world has changed
;;          in respect to email too.
;;      o   Filter solution is no-road. It's an arms race; some UBE always
;;          sneaks through and it will never stop the actual UBE.
;;      o   Not all people have access to filtering tools (some amy
;;          require certain Operating System e.g. Unix Procmail).
;;      o   POP users download their post and each UBE byte costs in transfer
;;          time.
;;      o   Nothing works as well as *not* giving the real address in the
;;          first place.
;;
;;      This package can activate the address munging very easily for
;;      selected newsgroups and make those email harverters gathering job
;;      more difficult. Humans that want to contact the person can still
;;      decode the address.
;;
;;      To activate address munging for newsgroups matching regexp, set
;;      variable `tinymail--from-anti-ube-regexp'. Your `user-mail-address'
;;      is be hashed and different address is generated for each post.
;;
;;          me@here.com   --> me.ads-hang@here.com, me.hate-ube@here.com ...
;;
;;  Feature: Sendmail Plus Addressing (introduction)
;;
;;        [excerpted from http://pm-doc.sourceforge.net/ for background]
;;        Recall from [rfc1036] that the preferred Usenet email address
;;        formats are following
;;
;;              From: login@example.com
;;              From: login@example.com (First Surname)
;;              From: First Surname <login@example.com>
;;
;;        A new sendmail supports plus addressing, where the address is
;;        treated like <login@example.com> and the extra "plus-info" is
;;        available eg to procmail or other LDAs. See Eli'd faq for more
;;        information at http://www.faqs.org/faqs/mail/addressing/ A typical
;;        sendmail enabled plus address looks like:
;;
;;            login+plus-info@domain
;;
;;        We can simulate plus addressing with pure RFC compliant address.
;;        We exploit RFC comment syntax, where comment is any text inside
;;        parentheses. According to Eli's paper, comments should be
;;        preserved during transit. They may not appear in the exact place
;;        where originally put, but that shouldn't be a problem. So, we
;;        send out message with following `From' or `Reply-To' line:
;;
;;            first.surname@domain (First Surname+mail.default)
;;
;;        Now, when someone replies to you, the MUA usually copies that
;;        address as is and you can read in the receiving end the PLUS
;;        information and drop the mail to appropriate folder: `mail.default'.
;;
;;        [About subscribing to mailing lists with RFC comment-plus addess]
;;
;;        It's very unfortunate that when you subscribe to lists, the comment
;;        is not preserved when you're added to the list database. Only the
;;        address part is preserved. I even put the comment inside angles to
;;        fool program to pick up everything between angles.
;;
;;            first.surname(+list.linux)@example.com
;;
;;        But I had no luck. They have too good RFC parsers, which throw away
;;        and clean comments like this. E.g. procmail based mailing lists, the
;;        famous `Smartlist', use `formail' to derive the return address and
;;        `formail' does not preserve comments. The above gets truncated to
;;
;;            first.surname@example.com
;;
;;        You can put anything inside RFC comment and do whatever you want
;;        with these plus addresses. _NOTE_: there are no guarantees that
;;        the RFC comment is preserved every time. Well, the standard RFC822
;;        says is must be passed untouched, but I'd say it is 90% of the
;;        cases where mail is delivered from one server to another, it is
;;        kept.
;;
;;        Example: if you discuss in usenet groups, you could use address
;;
;;            first.surname@example.com (First Surname+usenet.default)
;;            first.surname@example.com (First Surname+usenet.games)
;;            first.surname@example.com (First Surname+usenet.emacs)
;;            first.surname@example.com (First Surname+usenet.linux)
;;
;;  Feature: Sendmail Plus Addressing in this package
;;
;;      The idea of setting PLUS information is that you "tag" you messages
;;      and when messages are returned to you, you can file the messages to
;;      proper folders. Unix users can set up a procmail receipe to trap
;;      the plus information. Alternatively Emacs Gnus can be configured
;;      to use fancy splitting methods for IMAP, POP and regular
;;      mailbox.
;;
;;      The sender field generation is disabled in `message-mode-hook' by
;;      function `tinymail-message-disable-sender', so that *From* field
;;      gets a trusted status. If you still want to generate the *Sender*
;;      field, then add this after package has been loaded.
;;
;;          (remove-hook 'tinymail-message-disable-sender 'message-mode-hook)
;;
;;     Non-Newsgroup posting
;;
;;      Use your custom function to decide what address to use and what
;;      plus information to use by setting function to
;;      `tinymail--from-info-function'. Non-Newsroup posting means, that
;;      you're not inside a Gnus Newsgroup from where you initiate
;;      a "post". A typical invocation to non-Newsroup posting is `C-x' `m'.
;;
;;     Newsgroup posting
;;
;;      You might want to set `tinymail--from-info-function' return
;;      different email address for Usenet newsgroup posts. Set up an free
;;      email account somewhere and use only that for Usenet discussions.
;;      That way you can reserve your normal address to your private email
;;      communication.
;;
;;      The settings you need to enable the address generation is simple.
;;      Table `tinymail--from-table-prefix' sets the left part of
;;      the plus address component and `tinymail--from-table-postfix' can
;;      set the right part after period.
;;
;;          tinymail--from-table-prefix + tinymail--from-table-postfix
;;
;;      This makes the the *+left.right* information which is added after
;;      your `user-full-name' part. If `tinymail--from-table-prefix'
;;      returns nothing, the `tinymail--from-table-postfix' is used as is.
;;      Here is example setup. Pay attention to the "work.misc" which is
;;      the return value for all addresses matching "my-work-site".
;;
;;          (setq tinymail--from-table-prefix
;;            '(("emacs\\|perl" . "mail")
;;              ("."            . "usenet"))
;;
;;          (setq tinymail--from-table-postfix
;;            '(("games"                                 . "games")
;;              ("emacs\\|[a-z]+\\.el\\>\\|(def\\|(setq" . "emacs")
;;              ("perl\\|\\.pl\\>"                       . "perl")
;;              ("my-work-site\\>"                       . "work.misc"))
;;
;;     Gnus support
;;
;;      If you use Gnus news reader, then you get some bonus. For Gnus
;;      users the default plus information is generated based on the group
;;      you're posting from. In general the plus address generated is
;;      directly the group's name. That's quite convenient. To make this
;;      this effective for mailing lists too, do this:
;;
;;      o   Rename all your mailing lists to start with *list.NAME*
;;          like list.ding, list.linux, list.procmail, list.dance ...
;;      o   Edit each mailing lists group parameter with `G' `p'
;;          from *Group* buffer and add mailing list destination address:
;;
;;              ((to-list . "Mailing List Name <address@example.com>"))
;;
;;      Now when the `to-list' property is set, The Gnus group is labeled
;;      as "mailing list". If the `to-list' property is not set, the group
;;      is not considered as mailing list.
;;
;;  Feature: Toggle plugged state
;;
;;      With dial up connections, it is customary to swap between on-line
;;      and off-line mode. If you use Gnus as your mail reader, TinyMail
;;      can show the plugged status in the `mode-line'. If you see "tm!"
;;      you're plugged (on-line). The key to change the Gnus plugged status
;;      is bound to `C-c' `t' `j' in TinyMail controlled mail buffer.
;;
;;  Configuration: Highlighting color settings
;;
;;      The default highlighting is only provided to your convenience. If
;;      you use `font-lock' the internal highlighting is *automatically*
;;      suppressed.
;;
;;  Configuration: Default citation header
;;
;;      This feature is mainly designed for Gnus `message-mode'. Use it
;;      like this:
;;
;;          (setq mail-yank-prefix  "| ")   ;; less noisy, than "> "
;;          (setq mail-user-agent   'message-user-agent)
;;
;;      There is function `tinymail-citation-generate' which generates
;;      citation that uses international ISO 8601 date format, user name
;;      and the Gnus mailing group from where the reply started:
;;
;;          * Tue YYYY-MM-DD John Doe <johnd@example.com> mail.emacs
;;          | ...said something
;;
;;      To activate this citation reference function with your Mail User
;;      Agent (Gnus, RMAIL ..), call:
;;
;;          (add-hook 'tinymail--load-hook 'tinymail-install-citation)
;;
;;      For supercite, install this function to the handlers and select
;;      it with index 0:
;;
;;          (require 'sc)
;;          (push (list tinymail-citation-generate) sc-rewrite-header-list)
;;          (setq sc-preferred-header-style 0)
;;
;;  Code Note: shared TAB key
;;
;;      When TinyMail is active in the mail buffer, it takes ower the tab
;;      key. The default function `tinymail-complete-guess-in-headers' is
;;      electric, meaning that it behaves like ordinary tab if the point is
;;      not in completing headers. E.g. If the point is in `Cc' or in `To',
;;      then the completion feature is activated. If you have plans to use
;;      the tab key to do some other special things in other headers,
;;      you're free to to do so. All you have to do is to add your own
;;      custom function into
;;
;;          `tinymail--complete-key-hook'
;;
;;      The custom function must return `t' if it did something. See also
;;      `tinymail--table-header-complete' where it is possible to define
;;      custom headers and the completions easily.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)
(require 'tinylibmail)

;;  We must have these minor modes loaded beforehand. That's
;;  because we can't override that TAB key unless we "became" minor
;;  mode after these packages.

(require 'tinytab    nil 'noerr)
(require 'tinyindent nil 'noerr)

(autoload 'message-mail-p    "message")
(autoload 'bbdb-hashtable    "bbdb" "" nil 'macro)
(autoload 'bbdb-gethash      "bbdb")
(autoload 'bbdb-record-net   "bbdb")
(autoload 'bbdb-record-name  "bbdb")
(autoload 'bbdb-record-notes "bbdb")

(autoload 'mail-position-on-field              "sendmail")
(autoload 'mml-secure-message-sign-pgpmime     "mml")
(autoload 'mml-secure-message-encrypt-pgpmime  "mml")

(eval-and-compile
  (ti::package-require-mail-abbrevs)

  ;;  forward declarations for byte compiler
  (defvar message-citation-line-function)
  (defvar message-reply-headers)
  (defvar bbdb-file)
  (defvar tinytab--tab-insert-hook)
  (defvar tinytab-mode)

  (unless (locate-library "bbdb")
    (message "\
  ** tinymail.el: No bbdb.el along `load-path'. http://bbdb.sourceforge.net/
                  You can still use the package if you do not byte compile it.
                  Package will adapt to missing BBDB features."))

  (autoload 'message-tab                "message"  "" t)
  (autoload 'message-narrow-to-headers  "message")

  (let ((loc (locate-library "nnheader")))
    (unless loc
      (message "\
  ** tinymail.el: You have too old Gnus, visit http://www.gnus.org/
                  Old Gnus version found at %s" loc)))

  (autoload 'mail-header-from   "nnheader" "" nil 'macro)
  (autoload 'mail-header-date   "nnheader" "" nil 'macro))

(ti::package-defgroup-tiny TinyMail tinymail-- mail
  "Some mail additions: dynamic Fcc, Cc
        Overview of features

        o  Some handy additions to mail sending interface.
        o  Adds automatically Cc field when you type the To: address.
        o  Changes Fcc dynamically according to header content.
        o  Very easy TAB completion: two modes, alias and definition string.
           or password file entry completion.
        o  if Fcc folder has .gz or .Z name it automatically triggers
           loading jka-compr.")

;; Without fully qualified domain name,  smtpmail.el
;; can't send messages. Make sure the email is in format user@domain.com

(when (or (not (stringp user-mail-address))
          (not (string-match ".+@.*\\..+"
                             (or user-mail-address
                                 ""))))
  (message
   (concat "Tinymail: [ERROR] Please set `user-mail-address' "
           "to \"user@somewhere.net\". Was %s")
   (prin1-to-string user-mail-address)))

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...
;;; hooks and functions

(defcustom tinymail--load-hook nil
  "*Hook run when package has been loaded."
  :type  'hook
  :group 'TinyMail)

;;  Add more dynamic change functions to this hook

(defcustom tinymail--process-hook nil
  "*Hook run when `tinymail--awake-time' is up. This hook is always run."
  :type  'hook
  :group 'TinyMail)

(defcustom tinymail--feature-hook '(tinymail-mail-send-to-list)
  "*Hook run when idle time is up. Optional features to run.
Eg If you're using Gnus for mailing lists. Please define `to-list'
Group parameter for each group."
  :type  'hook
  :group 'TinyMail)

(defcustom tinymail--complete-key-hook
  '(tinymail-complete-everything
    ;; tinymail-complete-bbdb-fuzzy ;honor BBDB first

    ;;  tinymail-complete-bbdb <NO GOOD> because displays
    ;;  BBDB record.
    tinymail-complete-simple
    tinymail-complete-guess-in-headers  ; then passwd

    ;;  tinymail-complete-bbdb <NO GOOD> because displays
    ;;  BBDB record. fuzzy is better

    ;; tinymail-complete-bbdb-fuzzy

    tinymail-complete-headers-nothing-found

    ;; tinymail-complete-guest-packages
    ;; tinymail-complete-abbrevs
    ;; tinymail-complete-headers-move-to-next-field

    tinymail-complete-everything)
  "*Run each function with argument nil until success.
This function contains default try funcions
to completes email addresses in the Cc and To headers. It is strongly
suggested that you don't add new functions to this hook with `add-hook',
but that that you set the value manually. The order of the tried functions
is important.

Default value for this hook is as follows. These are preset at startup
by calling function `tinymail-install-hooks' at package load time.

  '(tinymail-complete-everything
    tinymail-complete-simple
    tinymail-complete-headers-nothing-found
    tinymail-complete-abbrevs
    ;; put your own completion functions here. Befor call to guest packages
    tinymail-complete-guest-packages)

Function call arguments:

  info  This variable holds the string part at current point
  '(BEG END STRING)

Function should return:

  nil               Did nothing; pass control to next function in hook.
  non-nil           Handled the Tab at point"
  :type  'hook
  :group 'TinyMail)

(defcustom tinymail--complete-body-hook
  '(tinymail-complete-bbdb-fuzzy-at-point
    tinymail-complete-guess)
  "*Run each function with argument nil until completion success.
This is similar variable like `tinymail--complete-key-hook' but run in the
message body. E.g. When user want to add a BBDB net entry to the current point."
  :type  'hook
  :group 'TinyMail)

(defcustom tinymail--send-mail-hook-list
  '(mail-send-hook ;; VM runs this too
    message-send-hook
    mh-before-send-letter-hook)
  "*List of mail sending hooks."
  :type  '(repeat (symbol :tag "Hook"))
  :group 'TinyMail)

(defcustom tinymail--citation-message-id-function 'tinymail-message-id
  "Return message-id line that is added above the citation header."
  :type  'function
  :group 'TinyMail)

;;}}}
;;{{{ setup: config public

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinymail--protect-email-addresses t
  "*If non-nil, then scrable all words that look like an email address.
E.g. this@example.com is made something like this <AT> example.com.

Note: If you are sending other content with mail, like patches, make sure
you protect those with base64 encoding to prevent changing the content."
  :type '(repeat (symbol :tag "Keymap variable"))
  :group 'TinyMail)

(defcustom tinymail--table-keymap-list
  '(mail-mode-map
    message-mode-map
    mh-letter-mode-map)
  "*List of keymaps where to install default bindings."
  :type '(repeat (symbol :tag "Keymap variable"))
  :group 'TinyMail)

(defcustom tinymail--enter-mail-hook-list
  '( ;; gnus-message-setup-hook
    message-header-setup-hook

    ;; mail-send-hook
    mail-setup-hook

    mh-letter-mode-hook             ;; MH
    vm-mail-mode-hook)              ;; VM
  "*List of hooks where to install `tinymail-mail'."
  :type '(repeat (symbol :tag "Hook"))
  :group 'TinyMail)

(defcustom tinymail--dead-mail-file
  (ti::package-config-file-prefix "tinymail-dead-mail")
  "*Append all mail buffers to this fAile on Emacs exit."
  :type  'file
  :group 'TinyMail)

(defcustom tinymail--awake-time
  (if (ti::xemacs-p)
      10                                ;XEmacs needs lower value
    15)
  "*Sleep time of `post-command-hook' before activation."
  :type  '(integer :tag "Movement Cycles")
  :group 'TinyMail)

(defcustom tinymail--confirm-mailrc-regexp  "."
  "*If matches, confirm picked completions from .mailrc file.
When the completion is found from the .mailrc it is matched against
this regexp. If `tinymail--confirm-mailrc-regexp' matches, then
you're asked if you accept the match. If you discard it the other
completion functions get a chance to run."
  :type  'regexp
  :group 'TinyMail)

(defcustom tinymail--cc-kill-regexp (and (stringp user-mail-address)
                                         (regexp-quote user-mail-address))
  "*Kill all CC field elements matching regexp.
The usual value is you possibel Email addresses that you
wish to remove from CC fields to avoid duplicate copies when
you already use Means of Fcc, Gcc etc.

At any time you can add two spaces in front of CC field to
disable this \"kill\" feature. This is desirable if you WANT to
add a CC to your other email addresses. An example:

  (setq tinymail--cc-kill-regexp \"me@here.at\")

  To: some@example.com
  CC: me@here.at             << will be removed
  CC:  me@here.at            << NOT removed, because field has two spaces."
  :type  'regexp
  :group 'TinyMail)

(defcustom tinymail--password-mode t
  "*Should we try to complete passwd entries?.
if normal .mailrc completion fails then  non-nil enables completion.

If you're running slow machine and huge amount of users, and
you can't afford to use `tinymail--password-file' due to disk quota
reasons, set this variable to nil and no passwords entries are
completed. It's faster to defne .mailrc aliases that you need.

This variable can be toggled with \\[tinymail-complete-password-mode]."
  :type  'boolen
  :group 'TinyMail)

(defcustom tinymail--password-file
  (ti::package-config-file-prefix "tinypath-passwd.el")
  "*Preparsed password file completions.
If this file does not exist it will be created when passwd
completion is needed. You _can_ keep this file in compressed format by
adding extension .gz to filename.

If this file is nil, then no file is read or written to."
  :type  'file
  :group 'TinyMail)

(defcustom tinymail--password-cat-cmd
  (cond
   ((ti::os-check-hpux-p)
    "ypcat passwd")
   ((ti::os-check-sunos-p)
    "cat /etc/passwd")
   ((string-match "irix" (emacs-version))
    "ypcat passwd")
   ((ti::os-check-linux-like-p)
    "cat /etc/passwd")
   ((ti::win32-p)
    nil) ;; No password file here
   (t
    (error
     (substitute-command-keys
      (concat
       "TinyMail: No tinymail--password-cat-cmd. Please share your know-how"
       " with \\[tinymail-submit-bug-report]")))))
  "*Shell command that print the contents of standard UNIX passwd file.
If your systems shell command isn't seen here, contact maintainer
immedately and report right shell command, so that it is set automatically
right."
  :type  '(string :tag "Command")
  :group 'TinyMail)

(defcustom tinymail--complete-bbdb-fuzzy-method
  '( ;; Can't funcall macros, so wrap them inside lambda's
    (lambda (record) (bbdb-record-net        record))
    (lambda (record) (bbdb-record-aka        record))
    (lambda (record) (bbdb-record-name       record))
    (lambda (record) (bbdb-record-raw-notes  record)))
  "*Which fields to check against the completion string.
The value must be list of functions to return a string or list of strings
to match when passed and BBDB RECORD.

The value must be callable by `funcall', e.g. macros are not callable."
  :type  '(list sexp)
  :group 'TinyMail)

(defcustom tinymail--complete-bbdb-case-fold-search case-fold-search
  "*Should completing against BBDB record be case sensitive."
  :type  'boolean
  :group 'TinyMail)

(defcustom tinymail--complete-mode 'string
  "*Control how completion is done.

'alias

    Means that we should complete alias names and
    that the alias expansion is shown in echo-area.

'string

    Means that _string_, which may include any character including white
    spaces, is searched from the full expansion list of aliases. This way
    you can remember anything from the person itself and it will be
    searched. Found expansion is inserted in place of typed
    string.

    Your ~/.mailrc can have entries like this:

    alias mark   \"Mark Eggert -- Project engineer <meg@twenix.com>\"
    alias mike   \"Michael Lowell  -- SkyTrax consulting <ml@sky.com>\"

    The right hand strings are searched with picked _string_ and
    if there is only one match for the string, the expansion (rh element)
    is inserted into buffer."
  :type '(choice
          (const alias)
          (const expansion))
  :group 'TinyMail)

(defconst tinymail--idle-timer-seconds 1
  "*Seconds after Emacs is idle to check the mail contant in buffer.")

;;; ........................................................ &v-tables ...

(defcustom tinymail--table-fcc nil
  "*Replace Fcc content with FCC-FIELD-STRING when headers match REGEXP.
If there is _two_ spaces in the Fcc field, the Fcc header is not touched.
Format is '((REGEXP FCC-FIELD-STRING) ..)"
  :type  '(repeat
           (list
            (string :tag "To Regexp")
            (sexp   :tag "Fcc field string")))
  :group 'TinyMail)

(defcustom tinymail--table-gcc nil
  "*Replace Gcc content with GCC-FIELD-STRING when headers match REGEXP.
If there is _two_ spaces in the Gcc field, the Gcc header is not touched.
Format is '((REGEXP GCC-FIELD-STRING) ..)"
  :type  '(repeat
           (list
            (string :tag "To Regexp")
            (sexp   :tag "Gcc field string")))
  :group 'TinyMail)

;; (all-completions "nnml" gnus-active-hashtb 'gnus-valid-move-group-p)

(defcustom tinymail--table-header-complete nil
  "*List of field names and their copletion values.
If after HEADER-FIELD the value is not a string, the rest value is evaluated.
HEADER-FIELD must not contain colon.

Format:
 '((HEADER-FIELD (COMPLETION-STRING COMPLETION-STRING ..)
   (HEADER-FIELD EVAL-FORM)
   ..)

Notes

  The EVAL-FORM must set `tinymail--complete-key-return-value' to non-nil
  if it does not return a list of completions, but otherwise handles
  the completions itself. This stops running other completion functions.

Example:

If you want to to complete header `Class' with values Urgent, Note, Memo,
FYI, Announce.

In addition completing the Gnus Gcc and Newsgroup header is easy. Some notes
about the EVAL-FORM used: the form is called in function
`tinymail-complete-simple', so all variables used there are visible in
EVAL-FORM. The `string' is the read word from current point, which you
should use when searching completions.

 (setq tinymail--table-header-complete
   '((\"Class\"
      (\"Urgent\" \"Note\" \"Memo\" \"FYI\" \"Announce\"))

     (\"Gcc\"
      (when (and (featurep 'gnus) (stringp string))
        (all-completions
         string
         gnus-active-hashtb 'gnus-valid-move-group-p)))

    (\"Newsgroups\"
      (when (and (featurep 'gnus) (stringp string))
        (all-completions
         string
         gnus-active-hashtb
         (gnus-read-active-file-p))))))"
  :type  '(repeat
           (list
            (string :tag "Field")
            (repeat (string :tag "value"))))
  :group  'TinyMail)

;;}}}
;;{{{ setup: Sendmail like PLUS Address configuration

(defcustom tinymail--from-field-plus-separator "+"
  "The string to separate `user-full-name' from plus information.
Note: some MTAs may not accept '+' character. An alternative
could be '--'.

  login+plus-information@example.com

  login@example.com (First Surname+plus-information)"
  :type  'string
  :group 'TinyMail)

(defcustom tinymail--from-field-enable-flag t
  "*Non-nil means that From: field generation is allowd.
The function to generate the from field is `tinymail-from-set-field'."
  :type  'boolean
  :group 'TinyMail)

(defcustom tinymail--from-anti-ube-regexp "games\\|ibm"
  "*If Regexp match Newsgroups header, generate anti-ube email From address.
Generated address is based un `user-mail-address' with hashed words in
the address. The generated email is made with `ti::mail-email-make-anti-spam-address'.
Returned value is different each time.

   me@here.com   --> me.ads-hang@here.com, me.hate-ube@here.com ...

References:

  For complete email address control, you want to use
  `tinymail--from-info-function'."
  :type  'regexp
  :group 'TinyMail)

(defcustom tinymail--from-info-function  nil
  "*Functon to return the suitable `user-mail-address' for message.

Return value:

 '(email-address [plus-string] [Filername Surname])

    If if function wants to change only the email-address for the message,
    the return value is in format:

     '(\"foo@bar.com\")

    And if the Plus info and Another user-id FirstName and Surname is
    wanted, then return value is:

     '(\"foo@bar.com\" \"mail.priv\" \"Mr. Foo\")

    If the return value is nil, the `user-mail-address' is used.

Notes

    Value returned from this function overrides

        `tinymail--from-table-prefix'
        `tinymail--from-table-postfix'
        `tinymail--from-anti-ube-regexp'

    If you want to protect yourself from UBE (Unsolicited bulk Email), you
    can use function `ti::mail-email-make-anti-spam-address' which uses hash table
    to construct human, but not easily machine decodable address.

Example one:

    Suppose you have some public domain email address, like hotmail
    and you want to use that in your Usenet postings instead of your normal
    email address. Here is code to do that:

    (setq tinymail--from-info-function 'my-tinymail-address)

    (defun my-tinymail-address ()
      (when (mail-fetch-field \"Newsgroups\")
        (list \"my-virtual@hotmail.com\")))

Example two:

    Suippose you want to make email harverter's work harder and use non-spam
    address in the high traffic Usenet groups. Here the ibm and games groups
    get \"protected\" address, which human can decode if they wish to contact
    you personally. Other usenet groups use your normal virtual aaddress.
    All other mail use your default `user-mail-address'.

    (setq tinymail--from-info-function 'my-tinymail-address)

    (defun my-tinymail-address ()
      let ((group (or (mail-fetch-field \"Newsgroups\") \"\" ))
           (addr  \"my-virtual@hotmail.com\"))
      (cond
        ((string-match \"games\\\\|ibm\" group)
         (list (ti::mail-email-make-anti-spam-address addr)))   ;; grumbled address
        ((string= \"\" group)
         (list addr))))                              ;; use normal virtual address

    This differ's from `tinymail--from-anti-ube-regexp' so that you have full
    control what address is used to generate the anti-ube address."
  :type  'string
  :group 'TinyMail)

(defcustom tinymail--from-table-prefix nil
  "*If `Newsgroup' header, match regexp, return plus address prefix.

Format:

  '((REGEXP . STRING)
    (REGEXP . STRING)
    ..)

Example

  '((\"emacs\\\\|perl\" . \"mail\")
    (\".\"   \"usenet\"))"
  :type  '(repeat (cons regexp string))
  :group 'TinyMail)

(defcustom tinymail--from-table-postfix nil
  "*Rules for constructing COMMENT PLUS part of the From address.

Match the Newsgroup header:

    If there is `Newsgroup' header, match regexp AND combine
    result of `tinymail--from-table-prefix' with `tinymail--from-table-postfix'
    match

In normal mail

    o   Go to the beginning of body, after headers and search body
        for regexp and return STRING from `tinymail--from-table-postfix'.
        `tinymail--from-table-prefix' IS NOT USED.
    o   If not found, search Gcc Gnus header and use it
    o   Otherwise use no postfix

Format:

    The left hand element can also be FUNCTION, which is called. It must
    return STRING like in the cdr element.

   '((REGEXP . STRING)
     (REGEXP . STRING)
     (FUNCTION)
     ..)

Example:

    From: foo@bar.com (Foo Bar+mail.emacs)
    To: somebody@else.com
    Subject: Re: Emacs keybindings
    Gcc: nnml:mail.reply
    --text follows this line--

        ...See function global-set-key and frieds in your Emacs.

Suppose we have above example mail in the buffer. The From line contains
string +mail.emacs added inside the comment (), because word 'emacs' were
found from the body of text according to the following varible contents:

  (setq tinymail--from-table-postfix
    '(
      ;;  Restrictive regexp first. These are searched from body
      ;;  in normal mail

      (\"[a-z]+\\\\.el\\\\>\\\\|(def\\\\|setq\"   . \"mail.emacs\")
      (\"\\.pl\\\\>\"                        . \"mail.perl\")

      (\"games\"                . \"mail.games\")
      (\"emacs\"                . \"mail.emacs\")
      (\"perl\"                 . \"mail.perl\")))"
  :type '(repeat (cons
                  (choice regexp function)
                  string))
  :group 'TinyMail)

;;}}}
;;{{{ setup: Reportmail

(defcustom tinymail--report-window-system (ti::compat-window-system)
  "*If non-nil; then never try to use X dragbar to announce mail.
Display the mail message in echo area instead."
  :type  'boolean
  :group 'TinyMail)

(defconst tinymail--report-asychronous-timeout 3
  "If non-nil, SECONDS to wait for `tinymail--report-mail-notify-program' finish.
If you are in a system where mailbox is over NFS and there are lot of
periodic NFS mount or access problems (automount failure, hardware
problem or whatever); then set this variable to number of seconds to timeout
`tinymail--report-mail-notify-program'.

If you don't set the timeout in NFS problematic environment, then the
call to repor tmail is blocked until answer has been received. This may freeze
your whole Emacs for several minutes.")

(defcustom tinymail--display-time t
  "*If non-nil, display the current time, load, and mail flag."
  :type  'boolean
  :group 'TinyMail)

(defvar tinymail--report-spool-buffer "*tinymail-mail-spool*"
  "*Buffer where to write mail spool information.
If this value is initially set to nil, no mail reporting is done.
See `tinymail--report-spool-buffer-control'.")

(defcustom tinymail--report-spool-buffer-control 'keep
  "*How to treat the `tinymail--report-spool-buffer'.
Accepted values are:

  'kill     Query mail spool and kill the buffer
  'keep     Query mail spool but do not kill after query
  'raise    If there is mail and mail count has changed since the last
            query; show the buffer in current working frame."
  :type '(choice
          (const kill)
          (const keep)
          (const raise))

  :group 'TinyMail)

(eval-and-compile
  (defun tinymail-default-report-mail-command ()
    "Construct default report mail shell call."
    (let ((mail (getenv "MAIL"))
          cmd)
      (setq
       cmd
       (or (and (file-exists-p "~/.procmailrc") ;; [1]
                (message "\
TinyMail: [WARNING] autosetup aborted. $HOME/.procmailrc found. Please set
manually `tinymail--report-mail-notify-program' to cover incoming mail
spool folders.")
                'procmail-error)
           (executable-find "from")     ;; [2a]
           (executable-find "mailfrom") ;; [2b]
           (and mail                    ;; [3]
                (or (file-exists-p mail)
                    ;;  In pristine system, user may not have received
                    ;;  mail yet, but if the leading directory is there,
                    ;;  then it's good enough
                    ;;
                    ;;  /var/spool/mail/LOGIN => /var/spool/mail/
                    ;;
                    (file-directory-p
                     (file-name-directory mail))
                    (message "TinyMail: [ERROR] Environment variable MAIL is invalid: %s "
                             mail))
                (executable-find "grep")
                (format "%s \"^From \"  %s"
                        (executable-find "grep")
                        mail))
           ;;  Okay, we give up. This is the fall-through case
           (let ((function (if (ti::win32-p)
                               'message
                             'error)))
             (funcall function "\                                   ;; [4]
TinyMail: [WARNING] Can't guess `tinymail--report-mail-notify-program'. Set manually.")
             nil)))
      (when cmd
        (cond
         ((and (ti::win32-p)
               (stringp cmd)
               (string-match "\\<bin\\>" (or shell-file-name "")))
          ;; This system is using Cygwin bash
          (ti::file-name-forward-slashes-cygwin cmd))
         ((and (stringp cmd)
               (ti::emacs-type-unix-like-p)) ;Unix, return as is
          cmd)
         ((stringp cmd)
          (ti::file-name-backward-slashes cmd)))))))

(defcustom tinymail--report-mail-notify-program
  (let ((cmd (tinymail-default-report-mail-command)))
    (when (stringp cmd)
      cmd))
  "*A shell call to return entries in the mail spool(s).
Set to nil if tou have lo local mail folders to scan.

Warning:

   If you're mixing Cygwin32 and DOS shell buffers in your Emacs,
   you MUST SET THIS variable and not rely on the automatic detection
   of Cygwin, which is determined by examining `shell-file-name'.

   The call must reflect you `shell-file-name', where paths must be
   Unix or Win32 styled accordingly.

Program must return entries in following format, which is the Berkeley mailbox
format or commonly known as Unix MBOX format:

   From login@site.xx Mon Feb 26 14:41:50 EET 1996

See if you can use from(1), mailfrom(1) or equivalent: \"man -k from\".
Make sure the binary is on your path, possibly located at /usr/ucb/ or
/usr/bin/. If you use absolute path, this program executes faster.

The `tinymail--report-mail-notify-program' value can be:

STRING  A shell program is called to return the lines
SYMBOL  an Emacs Lisp function is called to return the lines. Lisp function
        must return list of string or nil. There is default function
        `tinymail-report-mail-info-spool' which searches all messages in
        `tinymail--report-mail-spool-dir'"
  :type '(choice
          (string   :tag "Shell program")
          (function :tag "Lisp function"))
  :group 'TinyMail)

(defcustom tinymail--report-mail-kill-line-regexp
  (concat
   "Command.*finished"
   "\\|no mail"
   "\\|can't open")
  "Kill lines matching this regexp from report mail buffer.
When `tinymail--report-mail-notify-program' has finished printing the addresses,
it may print some garbage into the buffer like: 'command finished'
'No mail'. With this regexp you can kill these unwanted lines, otherwise
the line count would have been equal to the pending mail count.
Below the actual count is (1) and the message should display the
last message, not the 'Command finished'.

   From login@site2.xx Mon Feb 26 14:41:50 EET 1996
   From login@site1.xx Mon Feb 26 14:41:50 EET 1996
   Command finished"
  :type  'string
  :group 'TinyMail)

(defcustom tinymail--report-keep-intact-list
  '("VM")
  "*A list of frame names not to change."
  :type  '(repeat string)
  :group 'TinyMail)

(defcustom tinymail--report-no-mail-string
  (if tinymail--report-window-system
      " ----"
    ;;  This is better for echo area in non-window emacs
    "-- No Mail --")
  "*String to be printed to dragbar when no mail is pending.
If this string is nil, then nothing is displayed in
the echo area if Emacs is running in non-windowed envinronment."
  :type  'string
  :group 'TinyMail)

(defcustom tinymail--report-format-string
  '(concat
    tinymail--report-old-frame-string
    " "
    ;;  Use (display-time) in you ~/.Emacs to define display-time-string
    (if (and (boundp 'display-time-string)      ;may not exist ?
             (stringp display-time-string))     ;XEmacs has vector
        display-time-string
      "")
    tinymail--report-mail-info-string)
  "*Customize your display string layout here."
  :type  'sexp
  :group 'TinyMail)

(defcustom tinymail--report-mail-info-shorten-regexp nil
  "*Regexp to match local site address.
When you're in local host and receive mail internally, you
propably want to display user's account name only instead of full
email name. This is REGEXP that is tried upon arrived email address,
if it matches, the email address is truncated to account name."
  :type  '(string :tag "Regexp")
  :group 'TinyMail)

;;}}}
;;{{{ Setup: private

(defvar tinymail--report-old-frame-string     nil
  "Private.")

(defvar tinymail--report-old-mail-info-string nil
  "Private.")

(defvar tinymail--report-timer-object nil
  "Private. When package is activated this hold the timer object ativated.")

(defvar tinymail--report-mail-info-string nil
  "Private. Mail message information string.
This variable has one leading and trailingspace around the message.")

(defvar tinymail--timer-elt nil
  "Timer element is stored here.")

(defvar tinymail--y-or-n-p  nil
  "Andwered key from `tinymail-y-or-n-p'")

(defvar tinymail--tm-mode-name ""
  "TM MIME message split indicator.")

(defvar tinymail--message-type nil
  "Private flag. The initial message type in mail buffer.
When tinymail is first turned on, it checks if the message
is composed with \\[mail] or if you have replied to someone
else's message with 'r' from some mail mode. This initial
message type determines how \\[tinymail-mail] call behaves in the buffer.")

(put 'tinymail--message-type 'permanen-local t)
(make-variable-buffer-local 'tinymail--message-type)
(setq-default tinymail--message-type nil)

(defvar tinymail--last-to-field nil
  "Private. Last to: field value.")
(make-variable-buffer-local 'tinymail--last-to-field)

(defvar tinymail--mail-aliases-alist nil
  "Private. Cached aliases.
Run function `tinymail-update-mail-abbrevs' if you change your
~/.mailrc so that this variable gets updated.

Format: ((\"ALIAS\" . \"EXPANDED\") ..)")

(defvar tinymail--temp-buffer " *tinymail-tmp*"
  "Temporary buffer.")

(defvar tinymail--password-alist nil
  "Private. Password file in assoc form: '((LOGNAME . PASSWD-ENTRY)).")

(defvar tinymail--password-completion-alist nil
  "Private. Completion table of login names.")

(defvar tinymail--user-mail-address nil
  "This is made local to mail buffer.
Only ised if `tinymail-from-anti-ube-maybe' is in effect.")

;;}}}
;;{{{ setup: private

(defvar tinymail--complete-key-return-value  nil
  "Value set to non-nil in `tinymail--table-header-complete' EVAL-FORM.")

;;}}}

;;;### (autoload 'tinymail-debug-toggle "tinymail" "" t)
;;;### (autoload 'tinymail-debug-show   "tinymail" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinymail" "--"))

;;}}}
;;{{{ code: install

;;;###autoload (autoload 'tinymail-mode          "tinymail" "" t)
;;;###autoload (autoload 'turn-on-tinymail-mode  "tinymail" "" t)
;;;###autoload (autoload 'turn-off-tinymail-mode "tinymail" "" t)
;;;###autoload (autoload 'tinymail-commentary    "tinymail" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinymail-" " tm" "\C-ct" "tm" 'TinyMail "tinymail--" ;1-6

   "Mail enchancements.
For Documentation, run \\[tinymail-version]

Defined keys:

Prefix key to access the minor mode is defined in `tinymail--mode-prefix-key'

\\{tinymail--mode-prefix-map}"

   "Tinymail"
   (progn
     (cond
      (tinymail-mode
       (if buffer-read-only
           (error "TinyMail: Buffer is read-only, cannot turn on mode")
         (tinymail-mail)))
      (t
       (tinymail-mail 'disable))))
   "Mail enchancement mode"
   (list
    tinymail--mode-easymenu-name
    ["TO field tracking on/off" tinymail-on-off-toggle            t]
    ["Complete by guessing"     tinymail-complete-guess           t]
    ["Complete in body"         tinymail-complete-guess-in-body   t]
    ["Complete passwords mode"  tinymail-complete-password-mode   t]
    ["Abbrev expand at point"   expand-abbrev                     t]
    ["Abbrev rebuild (.mailrc)" tinymail-update-mail-abbrevs      t]
    ["Deactivate and set address"
     tinymail-deactivate-and-send-to-you                  t]
    ["Toggle Gnus plugged state"
     tinymail-gnus-agent-toggle-plugged                   t]
    "----"
    ["Debug toggle"           tinymail-debug-toggle       t]
    ["Debug show"             tinymail-debug-show         t]
    "----"
    ;; ["Keyboard menu"          tinymail-menu-main       t]
    ["Package version"        tinymail-version            t]
    ["Package commentary"     tinymail-commentary         t]
    ["Mode help"              tinymail-mode-help          t]
    ["Mode off (exit)"        turn-off-tinymail-mode      t])
   (progn
     (define-key root-map "\t"  'tinymail-complete-key-interactive)
;;;    (define-key root-map " "   'tinymail-expand-abbrev)
     (define-key map  "dd" 'tinymail-debug-toggle)
     (define-key map  "ds" 'tinymail-debug-show)
     (define-key map  "j"  'tinymail-gnus-agent-toggle-plugged)
     (define-key map  "p"  'tinymail-complete-password-mode)
     (define-key map  "u"  'tinymail-update-mail-abbrevs)
     (define-key map  "t"  'tinymail-on-off-toggle)
     (define-key map  "\t" 'tinymail-complete-guess-in-body)
     (define-key map  "x"  'turn-off-tinymail-mode)
     (define-key map  "?"  'tinymail-mode-help)
     (define-key map  "Hm" 'tinymail-mode-help)
     (define-key map  "Hc" 'tinymail-commentary)
     (define-key map  "Hv" 'tinymail-version))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-modeline-update (&rest plugged-status)
  "Udate `tinymail--mode-name' to show ! in plugged state."
  (let ((status (if (not (zerop (length plugged-status)))
		    (car plugged-status)
		  (ti::mail-plugged-p))))
    (if status
        (unless (string-match "!" tinymail--mode-name)
          (setq tinymail--mode-name (concat tinymail--mode-name "!")))
      (when (string-match "^\\([^!]+\\)!" tinymail--mode-name )
        (setq tinymail--mode-name (match-string 1 tinymail--mode-name ))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-gnus-agent-toggle-plugged (&optional mode)
  "Toggle Gnus plugged state if Gnus has been loaded."
  (interactive  "P")
  (if (not (fboundp 'gnus-agent-toggle-plugged))
      (message "Can't change plugged staus. Gnus Agent is not loaded.")
    (let ((status (ti::mail-plugged-p)))
      (ti::bool-toggle status mode)
      (if status
          (ti::funcall 'gnus-agent-toggle-plugged t)
        (ti::funcall 'gnus-agent-toggle-plugged nil))
      (tinymail-modeline-update (ti::mail-plugged-p)))))

;;; ----------------------------------------------------------------------
;;; #todo: is this really needed
;;;
(defun tinymail-expand-abbrev (&optional arg)
  "Call `abbrev-expand' if cursor is inside header or `self-insert-command'.
If Prefix argument is given, call `self-insert-command' with ARG.
This function should be bound to SPACE key."
  (interactive "P")
  (if (or (not (null arg))
          (not (and (< (point) (ti::mail-hmax))
                    (fboundp 'expand-abbrev)
                    (expand-abbrev))))
      (self-insert-command (prefix-numeric-value arg))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-install-hooks (&optional remove verb)
  "Install needed hooks, optionally REMOVE. VERB."
  (let ((list '(
		;; tinymail-complete-everything
		;; tinymail-complete-bbdb-fuzzy ;honor BBDB first
		;;  tinymail-complete-bbdb <NO GOOD> because displays
		;;  BBDB record.
		tinymail-complete-simple
		tinymail-complete-guess-in-headers ; then passwd
		;;  tinymail-complete-bbdb <NO GOOD> because displays
		;;  BBDB record. fuzzy is better
		;; tinymail-complete-bbdb-fuzzy
		tinymail-complete-headers-nothing-found)))
    ;; tinymail-complete-guest-packages
    ;; tinymail-complete-abbrevs
    ;; tinymail-complete-headers-move-to-next-field
    (ti::add-hooks 'tinymail--process-hook
                   '(tinymail-modeline-update)
                   remove)
    (ti::add-hooks 'kill-emacs-hook 'tinymail-save-dead-mail-maybe remove)
    ;; Install the default functions only if this hook is initially nil
    (cond
     (remove
      (ti::add-hooks 'tinymail--complete-key-hook list 'remove))
     ((null tinymail--complete-key-hook)
      ;;  The TAB key handler. First remove the hooks, and then add, so that
      ;;  they will be in this order. The order is _very_ important.
      (ti::add-hooks 'tinymail--complete-key-hook list 'remove)
      ;;  The first function that runs must be at the end of list
      (ti::add-hooks 'tinymail--complete-key-hook (reverse list))))
    ;;  List of hooks where to install us
    (ti::add-hooks tinymail--enter-mail-hook-list
                   'turn-on-tinymail-mode  remove)
    (ti::add-hooks 'write-file-functions
                   'tinymail-write-file-hook remove)
    (ti::add-hooks 'tinymail--mode-define-keys-hook
                   'tinymail-mode-define-keys remove)
    ;;  If user has allowed message-mode to run tinymail, then also install
    ;;  this function, which prevents Sender field genearation. (We generate
    ;;  the From field).
    (when (memq 'message-header-setup-hook  tinymail--enter-mail-hook-list)
      (ti::add-hooks 'message-mode-hook
                     'tinymail-message-disable-sender remove))
    (when verb
      (if remove
          (message "TinyMail: hooks removed.")
        (message "TinyMail: hooks installed")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-install-table-header-complete-gnus ()
  "Add Gnus Followup-To, Gcc, Newsgroups to `tinymail--table-header-complete'."
  ;;  Debian bug report header
  ;;  http://www.debian.org/Bugs/Reporting
  ;;  See http://www.debian.org/Bugs/Developer#severities
  (aput 'tinymail--table-header-complete
        "Severity"
        (list
         '("critical"    ;; Whole system break, serious data loss
           "grave"       ;; unuseable, data loss, security hole
           "serious" ;; violation of Debian policy, unsuitable for release.
           "important" ;; major effect withour  completely unusable.
           "normal"    ;; the default value, applicable to most bugs.
           "minor"     ;; doesn't affect the package's usefulness
           "wishlist"  ;; feature request
           "fixed")))  ;; fixed but should not yet be closed.
  ;;  Debian bug report header
  (aput 'tinymail--table-header-complete
        "Tags"
        (list
         '("patch"       ;;
           "wontfix"     ;;  change will cause other, worse, problems
           "moreinfo" ;;  more information must be provided by the submitter
           "unreproducible" ;; can't be reproduced on the maintainer's system
           "fixed" ;;  bug is fixed or worked around, needs to be resolved
           "security"                ;:  security problem in a package
           "potato"                  ;;  potato release
           "woody"                   ;;  woody distribution
           "s1id"))) ;;  architecture that is currently unreleased
  (aput 'tinymail--table-header-complete
        "Followup-To"
        (list
         '(when (eq major-mode 'message-mode)
            (call-interactively 'message-tab)
            ;;   We must stop the other completion function from running
            (setq tinymail--complete-key-return-value t)
            nil)))
  (aput 'tinymail--table-header-complete
        "Gcc"
        (list
         '(if (not (featurep 'gnus))
              (prog1 nil (message "TinyMail: Gcc completion needs Gnus..."))
            (when (stringp string))
            (all-completions
             string
             gnus-active-hashtb 'gnus-valid-move-group-p))))
  (aput 'tinymail--table-header-complete
        "Newsgroups"
        (list
         '(if (not (featurep 'gnus))
              (prog1 nil
                (message "TinyMail: Newsgroups completion needs Gnus..."))
            (when (stringp string))
            (all-completions
             string
             gnus-active-hashtb
             (gnus-read-active-file-p))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-read-version (sym)
  "Read version number from variable SYM if variable exists.
Otherwise return ''."
  (or (ti::string-match "[0-9][0-9.]+" 0
                        (if (boundp sym) (symbol-value sym) ""))
      ""))

;;; ----------------------------------------------------------------------
;;; - This is the main controller "install" that calls all other
;;;   functions.
;;;
(defun tinymail-install-to-buffers (&optional remove verb)
  "Activate or REMOVE tinyamil from mail buffers."
  (dolist (buffer (buffer-list)) ;;  Activate in current Emacs
    (with-current-buffer buffer
      (when (and (memq major-mode '(message-mode mail-mode))
                 ;; #todo:  vm send mode?
                 (null buffer-read-only))
        (message "TinyMail: mode %s in buffer %s"
                 (if remove "deactivated" "activated")
                 (buffer-name))
        (tinymail-mode (if remove -1 1) )))))

;;; ----------------------------------------------------------------------
;;; - This is the main controller "install" that calls all other
;;;   functions.
;;;
(defun tinymail-install (&optional remove)
  "Install or REMOVE package."
  (interactive "P")
  (let ((idle-p (ti::idle-timer-supported-p)))
    (if (null idle-p)
        (message "\
TinyMail: This Emacs does not support idle timers. Using regular timers."))
    (ti::compat-timer-cancel-function 'tinymail-process)
    (tinymail-report-mail-install-maybe remove)
    (tinymail-install-hooks remove)
    ;;  If the idle timer is available, use it. Otherwise we would have
    ;;  no other option but occupy post command hook
    (unless remove
;;; 2007-05-18 disabled. FIXME: needed? Too much CPU?
;;;      (if idle-p
;;;          (setq tinymail--timer-elt
;;;                (ti::funcall
;;;                 'run-with-idle-timer
;;;                 tinymail--idle-timer-seconds
;;;                 t
;;;                 'tinymail-process))
;;;        ;;  Can't respect tinymail--idle-timer-seconds,
;;;        ;;  so use 20 seconds repeat time.
;;;        (setq tinymail--timer-elt
;;;              (run-at-time "20 sec" 20 'tinymail-process)))
      (when (ti::nil-p (user-full-name))
        (message
         (concat
          "TinyMail: [ERROR] please set variable `user-full-name'."
          "Was [%s]")
         (prin1-to-string (user-full-name))))
      (tinymail-install-table-header-complete-gnus)
      (tinymail-install-to-buffers)
      (message
       "TinyMail: Installed. Read documentation with M-x tinymail-version"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-install-citation ()
  "Install First line citation function for Mail user agents."
  (setq message-citation-line-function
        'tinymail-message-citation-line-function))

;;}}}
;;{{{ code: misc

;;; ----------------------------------------------------------------------
;;;
(defsubst tinymail-y-or-n-p-abort-p ()
  "Check if `tinymail-y-or-n-p' was abort."
  (ti::char-in-list-case tinymail--y-or-n-p '(?q ?Q)))

;;; ------------------------------------------------------------ &misc ---
;;;
(defsubst tinymail-field-off-p (header-name &optional header-value)
  "Check status of HEADER-NAME field which has optional HEADER-VALUE.
If there is 2 or more leading spaces, then the field is considered 'off'."
  (when (and (stringp header-name)
             (stringp header-value))
    (not (memq (ti::mail-field-space-count header-name header-value)
               '(0 1)))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinymail-mail-aliases ()
  "Return `tinymail--mail-aliases-alist' or build it if it is empty."
  `(or tinymail--mail-aliases-alist
       (tinymail-update-mail-abbrevs)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-y-or-n-p (message)
  "Ask confirmation for MESSAGE. Accept TAB as yes.
Setq global variable `tinymail-y-or-n-p' to the result."
  (setq tinymail--y-or-n-p
        (ti::read-char-safe-until
         (concat message  " (tab/n or q)")
         '(?\t
           ?\ ?y ?Y ?n
           ;;  NO keys
           ?\b ?\177 ?\C-h ?\127
           ?N ?q ?Q
           ?\e ?\n ?\r
           ;;  These keys are usually above the TAB key, so you can answer
           ;;  NO with your left hand.
           ?\247
           ?\`
           ?\~)))
  ;;  YES answers.
  (ti::char-in-list-case tinymail--y-or-n-p '(?y ?Y ?\ ?\t ?\n ?\r)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-write-file-hook ()
  "Rebuild aliases everytime .mailrc is saved."
  (tinymail-debug "tinymail-write-file-hook" "MODE" major-mode (buffer-name))
  (when (string-match "\\.mailrc" (or (buffer-file-name) ""))
    (message "TinyMail: Updating mail aliases and abbrevs...")
    (build-mail-aliases)
    (when (fboundp 'build-mail-abbrevs) ;update abbrevs too
      (ti::funcall 'build-mail-abbrevs))
    (tinymail-update-mail-abbrevs 'force)
    (message "TinyMail: Updating mail aliases and abbrevs...done")
    ;;  Hook return value
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-update-mail-abbrevs (&optional force)
  "Build up all aliases to `tinymail--mail-aliases-alist' cache and return it.
Optional FORCE builds the list in any case.
You need to run this function if you change your ~/.mailrc."
  (interactive)
  (tinymail-debug "tinymail-update-mail-abbrevs")
  (when (and (fboundp 'build-mail-abbrevs) ;update abbrevs too
             (or force (interactive-p)))
    (ti::funcall 'build-mail-abbrevs))
  (setq tinymail--mail-aliases-alist (ti::mail-abbrev-get-alist)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymail-deactivate-and-send-to-you ()
  "Deactivate TinyMail and change To field to point to your address.
This function is normally used when you use mailing lists. See
documentation in the tinymail.el or call \\[tinymail-version]."
  (interactive)
  (tinymail-debug "tinymail-deactivate-and-send-to-you")
  (if (ti::nil-p user-mail-address)
      (error "TinyMail: Please set variable `user-mail-address'")
    (ti::mail-kill-field  "^to:" user-mail-address)
    (tinymail-field-to-off)
    (if (interactive-p)
        (message "Address changed to point to you. TinyMail signs off."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-buffer-email-address-scramble-area ()
  "Return area of email that can be scrambled.
Exclude patches and attachments."
  (let ((list
         (list
          "^RCS[ \t]+file:.*,v\\|^[ \t]*diff[ \t]+.*[-/]"
          "^---[ \t]"
          "^***[ \t]"
          "^\+\+\+[ \t]"
          "[<]#part" ;; Gnus attachment
          "\\[Attachment:"))
        (point-list (list (point-max)))
        beg
        end)
    (save-excursion
      (ti::pmin)
      (when (and (stringp mail-header-separator)
                 (search-forward mail-header-separator nil t))
        (setq beg (1+ (line-end-position)))
        (dolist (re list)
          (when (re-search-forward re nil t)
            (push (line-beginning-position) point-list)))
        (setq end (apply 'min point-list))))
    (when beg
      (list beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-buffer-email-address-scramble-1 ()
  "Spam protect email address words.
Scramble Email addresses do that spammers cannot use them.
The end position is before text that looks like a patch or `point-max'"
  (multiple-value-bind (beg end)
      (tinymail-buffer-email-address-scramble-area)
    (when beg
      (save-excursion
        (goto-char beg)
        (let ( ;;  If there is patch in this buffer, limit changes before it.
              (regexp
               (concat
                ;; Must be separated by space or "<".
                ;; this email@example.com or <email@example.com>
                ;; But not http://user@site.com/
                "\\(^\\|[ \t]\\)"
                "\\([^ /\t\r\r]+\\)@\\([^ /\t\r\r]+\\.[^ /\t\r\r]+\\)"
                "\\(^\\|[ \t]\\)")))
          (while (re-search-forward regexp end t)
            (replace-match "\\1\\2 AT \\3\\4")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-buffer-email-address-scramble ()
  "If `tinymail--protect-email-addresses' is non-nil, scrable addresses."
  (if tinymail--protect-email-addresses
      (tinymail-buffer-email-address-scramble-1))
  ;; Hook function. Return nil
  nil)

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-active-p ()
  "Check if TinyMail is active in current buffer."
  tinymail-mode)

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymail-mail (&optional disable verb)
  "Prepare mail mode.
Add or changes Cc, FF, X-Sender-Info fields on the fly while you're
composing the message.

Input:

  DISABLE       Disables package.
  VERB          print verbose message.

References:

  `tinymail--feature-hook'."
  (let ((fid "tinymail-mail")
	to-list)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug
     fid "in:"
     "dis-flag"     disable
     "MODE"         major-mode)
    (when (featurep 'tinytab)
      ;;  - make TinyTab.el work with TinyMail so that they share
      ;;    common TAB key.
      ;;  - Remove and add make sure the function is at the beginning and
      ;;    runs first.
      (remove-hook 'tinytab--tab-insert-hook
                   'tinymail-complete-key-interactive)
      (add-hook    'tinytab--tab-insert-hook
                   'tinymail-complete-key-interactive))
    (unless disable
      ;;  If you're replying to someone else's message, the TO field
      ;;  must have two spaces to turn off TinyMail so that the remaining
      ;;  Cc fields are not modified.
      ;;
      ;;  R and r keys don't add Cc field, so we put there only one space.
      ;;
      ;;  For simple C-x m TO field will be initially empty.
      (tinymail-debug
       fid
       "MAIN STATUS (BEFORE)"
       "point"      (point)
       "msg type"   tinymail--message-type
       "to"         (ti::mail-get-field-1 "to")
       "cc"         (ti::mail-get-field-1 "cc")
       "Subject"    (ti::mail-get-field-1 "subject")
       "MODE"       major-mode
       "\n["
       (buffer-substring (point-min) (point-max))
       "]\n")
      (run-hooks 'tinymail--feature-hook)
      (cond
       (tinymail--message-type
        ;;  User calls us again
        (if (ti::nil-p (ti::mail-get-field-1 "subject"))
            (tinymail-field-to-off)))
       (t
        ;;  We're called from some mail setup hook. See what is the initial
        ;;  state of the buffer...
        (cond
         ((setq to-list (ti::mail-to-list-p))
          (setq tinymail--message-type 'to-list)
          (tinymail-field-to-on))
         ((ti::nil-p (ti::mail-get-field-1 "subject"))
          ;;  simple mail: there is no subject field filled
          (setq tinymail--message-type 'simple))
         (t                             ;R or r; No cc field
          (setq tinymail--message-type 'reply)
          (tinymail-field-to-on))))))
    (tinymail-field-to-move-maybe)
    (unless to-list ;; No-op. XEmacs byte compiler silencer
      (setq to-list nil))
    (tinymail-debug
     fid
     "MAIN AFTER"
     "point"        (point)
     "disable"      disable
     "msg type"     tinymail--message-type
     "to-list"      to-list
     "MODE"         major-mode
     "to"           (ti::mail-get-field-1 "to")
     "Subject"      (ti::mail-get-field-1 "subject")
     "\n["
     (buffer-substring (point-min) (point-max))
     "]\n")))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-mail-send-to-list ()
  "Check if message is being sent to mailing list and Fix CC/To.
This function makes the To to point to mailing list and delete
any CC. Set Gnus group parameter to take use of this feature: (G p
in *Group* buffer):

   '(...
     (to-list . \"discussion-list@list.com\")
     ...)"
  (when (eq major-mode 'message-mode)
    (let* ((fid     "tinymail-mail-send-to-list:")
           (tofield (mail-fetch-field  "To"))
           (ccfield (mail-fetch-field  "Cc"))
           (to      (ti::mail-to-list-p))
           (email   (if to
                        (car-safe (ti::mail-email-from-string to)))))
      (unless fid ;; No-op. XEmacs byte compiler silencer
        (setq fid nil))
      (tinymail-debug fid "to" to "email" email)
      ;;  If TO is not in the headers; then this is private reply with
      ;;  "r". A followup will include TO in To or Ccc field.
      (when (and (string-match "^[ \t]*$" (or tofield ""))
                 (string-match "^[ \t]*$" (or ccfield ""))
                 to
                 email
                 ;; (save-restriction
                 ;;  (message-narrow-to-headers)
                 ;;  (not (ti::re-search-check email)))
                 (not (tinymail-field-off-p "To" to)))
        (tinymail-debug fid "TO-LIST SET, killed To/Cc")
        (ti::mail-kill-field "^To" to)
        (ti::mail-kill-field "^CC")
        t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-resolve-abbrevs (list)
  "Resolves LIST of mail abbrevs in format '(\"abbrav\" \"abbrev\" ..)

Return:

  ((\"alias\" . \"expansion\") (A . E) .. )
  alias        = the alias definition
  expansion    = expanded alias"
  (let* ((abbrevs   (tinymail-mail-aliases))
         abbrev-expand-functions ;; prevent recursion
         exp-list
         hit)
    (dolist (elt list)
      (tinymail-debug "tinymail-resolve-abbrevs" elt)
      ;;  Returns (ABBR . ABBR-EXPANDED)
      (if (not (setq hit (assoc elt abbrevs)))
          (message "TinyMail: Can't find abbrev '%s', is it in ~/.mailrc ?" elt)
        (if (not (member hit exp-list))
            (push hit exp-list))))
    exp-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-password-save (&optional load verb)
  "Save passwd completions to file `tinymail--password-file'. Optionally LOAD.
If that variable is nil, then do nothing. VERB."
  (let ((file  tinymail--password-file)
;;;     (list  tinymail--password-completion-alist)
	(list2 tinymail--password-alist))
    (ti::verb)
    (tinymail-debug "tinymail-password-save" file load verb)
    (when (stringp file)
      (if (string-match "\\.gz$\\|\\.Z$" file)
          (ti::use-file-compression))
      (cond
       (load
        (if (not (file-exists-p file))  ;Create file then
            (tinymail-password-define-variables 'force))
        (ti::load-file-with-wrapper file)
        (if verb (message "TinyMail: passwd completions loaded.")))
       (t
        (if (null list2)
            (message "\
TinyMail: `tinymail--password-alist' is empty, nothing to save.
Call `tinymail-password-define-variables' with argument FORCE.")
          (ti::write-file-variable-state
           file "TinyMail.el password completions"
           '(tinymail--password-completion-alist tinymail--password-alist))
          (if verb
              (message "TinyMail: passwd completions saved."))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-password-define-variables (&optional force no-save)
  "Defines passwd variables.
Read definitions from  `tinymail--password-file' if FORCE is nil.

Input:

  FORCE     flag, if non-nil, read passwd table and reset all variables.
  NO-SAVE   flag, if non-nil, do not save passwd completions to file.

Return:

  non-nil   if password completion can be used.

References:

  `tinymail--password-mode'"

  (tinymail-debug "tinymail-password-define-variables"
                  'force force
                  'no-save no-save
                  'passwd-file tinymail--password-file)
  (when (and tinymail--password-mode
             tinymail--password-cat-cmd)
    (cond
     ;; .................................................... cond-save ...
     ((or force
          (and tinymail--password-file
               (not (file-exists-p tinymail--password-file)))
          (and (null tinymail--password-file)
               (null tinymail--password-alist)))
      (message "TinyMail: Buildig password completions...")
      (setq tinymail--password-alist
            (ti::file-passwd-build-alist tinymail--password-cat-cmd))
      (message "TinyMail: Buildig password completions...done"))
     ;; .................................................... cond-load ...
     ((and (file-exists-p tinymail--password-file)
           (null tinymail--password-alist))
      (tinymail-password-save 'load)))
    ;; ......................................................... build ...
    (when (or force
              (null tinymail--password-completion-alist))
      (setq tinymail--password-completion-alist
            (mapcar (function
                     (lambda (x) (cons (car x) 1)))
                    tinymail--password-alist))
      (if (null no-save)
          (tinymail-password-save)))
    tinymail--password-completion-alist))

;;}}}
;;{{{ Completion

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-password-mode (&optional mode)
  "Toggle `tinymail--password-mode'  on or off."
  (interactive "P")
  (ti::bool-toggle tinymail--password-mode mode)
  (when (interactive-p)
    (message "TinyMail: Password complete mode is now %s"
             (if tinymail--password-mode "on" "off"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-string-read ()
  "Return completion string from current point or nil.
The String must be delimited by comma as in mail header are.

Return:
 (beg-marker end-marker string)"
  (let ((fid    "tinymail-complete-string-read")
	(point  (point))
	(heder-p (ti::mail-point-at-header-p))
	string
	beg-marker
	end-marker)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (when (and (not (bolp))             ;Nothing to read
               (not (char-equal (char-syntax (preceding-char)) ?\ )))
      (ti::narrow-safe (line-beginning-position) (line-end-position)
        (goto-char point)
        ;;  First, go away from whitespace so that match-end gets
        ;;  length in next case statement
        (skip-chars-forward " \t")
        (cond
         ((or (if (not heder-p)
                  (skip-chars-backward "^ \t")
                (or (re-search-backward ",[ \t]*" nil t)
                    (and (re-search-backward "^[^:]+:" nil t)
                         (goto-char (match-end 0)))
                    ;; continued line
                    ;;
                    ;;  Cc: this,
                    ;;
                    ;;
                    ;;
                    ;;      here_is_point
                    ;;
                    (re-search-backward "[:, ][ \t]*"  nil t))))
          (skip-chars-forward " ,\t")   ;Goto word
;;;       (ti::d! 1 (buffer-substring (point) (line-end-position)))
          nil)
         ((re-search-backward "^[ \t]*" nil t)
;;;       (ti::d! 2 (buffer-substring (point) (line-end-position)))
          (goto-char (1+ (point)))))
        (setq beg-marker (point-marker))
;;;      (ti::d! beg (looking-at "[^\n\t ,:]+") (buffer-substring beg (line-end-position)))
        ;;  There must be somthing, not just empty lines
        (when (looking-at "[^\n\t ,:]+")
          (cond
           ((re-search-forward " *,"  nil t)
            (setq end-marker (make-marker))
            (move-marker end-marker (match-beginning 0)))
           ((re-search-forward "[ \t]*,\\|[ \t]*$"  nil t)
            (setq end-marker (make-marker))
            (move-marker end-marker (match-beginning 0)))))

        (if (and beg-marker end-marker)
            (setq string (buffer-substring-no-properties
                          (marker-position beg-marker)
                          (marker-position end-marker)))))
      (tinymail-debug fid "RET" string beg-marker end-marker)
      (if (null string)
          (setq beg-marker nil ;; Kill possible markers
                end-marker nil)
        (list
         beg-marker
         end-marker
         string)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-list-mail-aliases (&optional mode data)
  "Return '(match match ...) from mail aliases.

Input MODE:

  The default match is made against all the alias expansion ('string mode).
  With 'alias, only the alias names are matched."
  (let ((fid    "tinymail-complete-guess-2-choices: ")
	(list   (tinymail-mail-aliases))
	(mail   (ti::mail-mail-p))
	elt
	beg
	end
	str)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug fid "in" mode 'mail-p mail)
    (when mail
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. get word ...
      (cond
       ;; ... ... ... ... ... ... ... ... ... ... ... ...  second mode ..
       ((eq mode 'alias)
        (save-excursion
          (when (< (skip-chars-backward "^ \t\n") 0)
            (setq beg (point))
            (when (> (skip-chars-forward "^ \n\t") 0)
              (setq end (point)))))
        (if (and beg end)
            (setq str (buffer-substring beg end))
          (setq str nil)))
       (t
        (or data
            (setq data   (tinymail-complete-string-read)))
        (if data
            (setq beg (nth 0 data)
                  end (nth 1 data)
                  str (regexp-quote (nth 2 data))))))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. find matches ...
      ;;#todo: this code must be rewritten, ti::list-find and `function'
      ;;is flow combination.

      (when (and (not (ti::nil-p str))
                 (setq elt
                       (ti::list-find
                        list str
                        (function
                         (lambda (arg elt)
                           (if (eq mode 'string)
                               (string-match arg (cdr elt))
                             (string-match (concat "^" arg)
                                           (car elt)))))
                        'all-matches)))
        (tinymail-debug fid "after type" beg end str)
        (mapcar 'cdr elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-guess-1 (&optional mode verb)
  "Try to expand using underlying characters.
Look completion from `mail-aliases'. If there is more than 1 match,
ask which one to use.

If MODE is 'string, then text read from buffer must be separated by

    LEFT-COLON:          txt [COMMA,WHITESPACE]
    LEFT-ALL-WHITESPACE  txt [COMMA,WHITESPACE]
    COMMA                txt [COMMA,WHITESPACE]

If MODE is 'alias then text is read direcly under point separated
by spaces. This function does nothing if the first line doesn't contain

    KEYWORD:

Indicating a mail like mode. VERB prints verbose messages.

Return:

  t         completed
  nil"
  (let ((fid    "tinymail-complete-guess-2: ")
	(list   (tinymail-mail-aliases))
	(mail   (ti::mail-mail-p))
	(check-regexp  tinymail--confirm-mailrc-regexp)
	user-selected-p
	data
	elt
	beg
	end
	str
	done
	ret)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug fid "in" mode verb mail)

    (when mail
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. get word ...
      (cond
       ((eq mode 'string)
        (setq data   (tinymail-complete-string-read))
        (if data
            (setq beg (nth 0 data)
                  end (nth 1 data)
                  str (regexp-quote (nth 2 data)))))
       ;; ... ... ... ... ... ... ... ... ... ... ... ...  second mode ..
       ((eq mode 'alias)
        (save-excursion
          (when (< (skip-chars-backward "^ \t\n") 0)
            (setq beg (make-marker))
            (when (> (skip-chars-forward "^ \n\t") 0)
              (setq end (make-marker)))))
        (if (and beg end)
            (setq str (buffer-substring
                       (marker-position beg)
                       (marker-position end)))
          (setq str nil))))
      (tinymail-debug fid "after type" beg end str)
      ;; ... ... ... ... ... ... ... ... ... ... ... ... .. find matches ...
      (when (and (not (ti::nil-p str))
                 (setq elt
                       (ti::list-find
                        list str
                        (function
                         (lambda (arg elt)
                           (if (eq mode 'string)
                               (string-match arg (cdr elt))
                             (string-match (concat "^" arg)
                                           (car elt)))))
                        'all-matches)))
        (setq str nil)
        (tinymail-debug fid "ELT matches" (length elt) (cdr (car elt)) elt)
        ;; ............................................... any matches ...
        ;;  How many matches?
        (cond
         ((eq 1 (length elt))
          (setq elt (car elt)))    ; '( (alias . string) ) --> (a . s)
         (elt
          (let (completion-ignore-case)
            (setq str
                  (completing-read
                   (format "%d Choose: " (length elt))
                   (ti::list-to-assoc-menu (mapcar 'cdr elt))
                   nil ;; predicate
                   (not 'match-it))))

          (if (ti::nil-p str)
              (setq elt nil)            ;User didn't select anything
            (setq  user-selected-p t
                   elt (rassoc str elt)
                   ret t))))
        (tinymail-debug fid "ELT" elt)
        ;; .............................................. select match ...
        ;;  Now we have a MATCH unless user cancelled the choices
        (when elt
          (if (eq mode 'string)
              (setq str (cdr elt))
            (setq str (car elt))))
        (tinymail-debug fid "SELECTION" mode str elt)
        (unless (ti::nil-p str)
          ;;  For some strings, ask confirmation.
          ;;  Ie. Give a chance to discard this completions and move on...

          (tinymail-debug fid
                          "CHECK" check-regexp
                          (string-match check-regexp str))
          ;;  *) If user already did selected this match from several
          ;;     choices, then go ahead
          ;;  *) If we found only one match, then confirm that match

          (when (or user-selected-p
                    (not (stringp check-regexp))
                    (or (null (string-match check-regexp str))
                        (and (string-match check-regexp str)
                             (tinymail-y-or-n-p (concat "TinyMail: " str)))))
            (goto-char (marker-position beg))
            (delete-region (marker-position beg) (marker-position end))
            (setq  beg nil  end nil) ;; Kill markers
            (insert str)
            (setq done t  ret t)))) ;; when-nil-var
      (cond
       ((and verb (null done) str)
        (message (format "TinyMail: no completion match on '%s'" str)))
       ((and verb str done (eq mode 'alias))
        (message (cdr (car elt)))))
      (tinymail-debug fid "RET" ret)
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-passwd (&optional force verb)
  "Complete names in passwd in header area, otw do nothing.

Input:

  FORCE     Complete anyway
  VERB      enable verbose messages.

Return:

  t     completed
  nil   nothing done"
  (interactive "P")
  (let ((fid      "tinymail-complete-passwd")
	(header-p (< (point) (ti::mail-hmax) ))
	ret
	table
	word
	str
	completions)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (ti::verb)
    (save-excursion
      (forward-char -1)                 ;move over some char
      (setq word (ti::buffer-read-word "[-_+a-zA-Z0-9]" 'strict)))
    (when (and (or force header-p)
               (not (ti::nil-p word))
               (tinymail-password-define-variables))
      (setq table       tinymail--password-completion-alist)
      (setq completions (all-completions word table))
      (tinymail-debug fid "COMPLETIONS" word completions)
      (cond
       ((eq 1 (length completions))
        (setq str (car completions))
        (if (null (tinymail-y-or-n-p (format "Accept Passwd match: %s " str)))
            (tinymail-y-or-n-p-abort-p)
          ;;  We only insert the missing part to the buffer.
          ;;  abcDEF
          ;;     * if tab was pressed after abc
          (insert  (substring (car completions) (length word)))
          (setq ret t)))
       ((setq completions (tinymail-password-grep word 'verb))
        (tinymail-display-list completions)
        ;;  Show the matched entries from passwd table, sometimes
        ;;  User doens't want to use them but continue calling other
        ;;  functions. Ask what's up.
        (setq ret
              (not
               (tinymail-y-or-n-p
                "TinyMail: Continue calling more completion functions?"))))))
    (tinymail-debug fid "RET" ret word)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-guess (&optional verb)
  "Complete using .mailrc and passwd.
Optional VERB allows displaying messages.

References:

  The completion type is determined by variable `tinymail--complete-mode',
  which can be 'alias or 'string

  This function is part of the other completion possibilities run by
  `tinymail-complete-key'  and installed in `tinymail--complete-key-hook'.

Return:

 non-nil    Completion handled
 nil        Not completed"
  (interactive "*")
  (let ((mode  tinymail--complete-mode)
        (pmode tinymail--password-mode)
        ret)
    (ti::verb)
    (cond
     ((eq  mode 'alias)
      (and (null (setq ret (tinymail-complete-guess-1 'alias  verb)))
           pmode
           (setq ret (tinymail-complete-passwd nil verb))))
     ((eq  mode 'string)
      (and (null (setq ret (tinymail-complete-guess-1 'string  verb)))
           pmode
           (tinymail-field-in-to-cc-p)
           (setq ret (tinymail-complete-passwd nil verb))))
     (t
      (error "TinyMail: Unknown mode %s" mode)))
    (tinymail-debug "tinymail-complete-guess" mode "PASS-MODE" pmode "RET" ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-everything (&optional verb)
  "Gather list of possible completions and let user choose."
  (interactive)
  (let ((data (tinymail-complete-string-read)))
    (when (and data
               ;;  It doesn't make sense to search items that already
               ;;  look like email,  this@here.com
               (not (string-match "@" (nth 2 data))))
      (let ((fid           "tinymail-complete-everything:")
	    (check-regexp  tinymail--confirm-mailrc-regexp)
	    (mode          tinymail--complete-mode)
	    matches
	    (beg (nth 0 data))
	    (end (nth 1 data))
	    choice
	    done
	    ret
	    user-selected-p)
        (unless fid ;; No-op. XEmacs byte compiler silencer
          (setq fid nil))
        ;; .......................................... clean duplicates ...
        (dolist (results (list
                          (tinymail-complete-list-mail-aliases mode data)
                          (tinymail-complete-list-bbdb mode data)
                          (tinymail-complete-list-passwd mode data)))
          (dolist (address results)
            (pushnew address matches :test 'string=)))
        ;; ............................................... any matches ...
        ;;  How many matches?
        (cond
         ((eq 1 (length matches))
          (setq choice (car matches)))
         (matches
          (setq choice
                (completing-read
                 (format "%d Choose: " (length matches))
                 (ti::list-to-assoc-menu matches)
                 nil
                 (not 'must-match)))
          (if (ti::nil-p choice)
              (setq choice nil)
            (setq  user-selected-p  t
                   ret              t))))
        (tinymail-debug fid "CHOICE" choice)
        ;; .............................................. select match ...
        ;;  Now we have a MATCH unless user cancelled the choices
        (when choice
          (tinymail-debug fid 'mode mode "CHOICE" choice)
          ;;  For some strings, ask confirmation.
          ;;  Ie. Give a chance to discard this completions and move on...
          (tinymail-debug fid
                          "CHECK" check-regexp
                          (string-match check-regexp choice))
          ;;  *) If user already did selected this match from several
          ;;     choices, then go ahead
          ;;  *) If we found only one match, then confirm that match
          (when (or user-selected-p
                    (not (stringp check-regexp))
                    (or (null (string-match check-regexp choice))
                        (and (string-match check-regexp choice)
                             (tinymail-y-or-n-p (concat "TinyMail: " choice)))))
            (goto-char (marker-position beg))
            (delete-region (marker-position beg) (marker-position end))
            (setq  beg nil  end nil) ;; Kill markers.
            (insert choice)
            (setq done t  ret t)))
        (if (and verb (null done) choice)
            (message (format "TinyMail: no completion match on '%s'" choice)))
        (tinymail-debug fid "RET" ret)
        ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-guess-in-headers (&optional arg)
  "Like `tinymail-complete-guess', but complete only in headers. Ignore ARG."
  (interactive)
  (ti::mail-point-in-header-macro
   (when (ti::mail-field-email-address-p)
     (tinymail-debug 'tinymail-complete-guess-in-headers
                     'ARG arg 'MODE major-mode 'POINT (point))
     (tinymail-complete-everything))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-guess-in-body (&optional arg)
  "Like `tinymail-complete-guess', but complete only in body. Ignore ARG."
  (interactive)
  (when (>(point) (ti::mail-hmax))
    (let ((fid    "tinymail-complete-guess-in-body")
	  (hook   tinymail--complete-body-hook)
	  (data  (tinymail-complete-string-read))
	  ret)
      (unless fid ;; No-op. XEmacs byte compiler silencer
        (setq fid nil))
      (tinymail-debug 'tinymail-complete-guess-in-body
                      'ARG     arg
                      'MODE    major-mode
                      'POINT   (point)
                      'data    data
                      'hook    hook)
      (dolist (func hook)
        (tinymail-debug fid 'FUNC func)
        (when (cond
               ((not (fboundp func))
                (tinymail-debug fid 'FUNC func "not exist")
                nil)
               (t
                (funcall func data)))
          (setq ret t)
          (return)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-bbdb  (&rest args)
  "Call bbdb-complete-name' if is bbdb loaded and ignore ARGS."
  (when (and (fboundp 'bbdb-complete-name)
             (tinymail-field-in-to-cc-p))
    (let ((point (point)))
      (call-interactively 'bbdb-complete-name)
      (if (eq (point) point)
          nil                          ;Point not moved, not completed
        ;; point moved, completed
        t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-bbdb-parse-to-string ()
  "Parse BBDB to a fast search format."
  (let ((str "")
        record
        tmp)
    (mapatoms
     (function
      (lambda (sym &optional symbol val name notes)
        (setq symbol (symbol-name sym))
        (setq record (bbdb-gethash symbol))
        (if (and (listp record)
                 (vectorp (setq tmp (car-safe record))))
            (setq record tmp))
        (when record
          (setq name  (bbdb-record-name record))

          (setq str (concat str (format "\C-m%s\C-j%s"
                                        name
                                        (prin1-to-string record)))))))
     (bbdb-hashtable))
    str))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinymail-bbdb-record-fix (record)
  "Fix BBDB RECORD to pure vector.
Upgrading from v3 to v5 BBDB database, the
entries are returned as ([ ... ]) by
bbdb-gethash, but this format is not suitable for
calling (bbdb-record-net record)

The code below removes the extra () and only
leaves RECORD [ .. ]."
  (let (tmp)
    (if (and (listp record)
             (vectorp (setq tmp (car-safe record))))
        tmp
      record)))

;;; ----------------------------------------------------------------------
;;;
(defun  tinymail-bbdb-data-read ()
  "Read user information based on current line in `bbdb-file'."
  (let ((fid   "tinymail-bbdb-data-read:")
	(point (point))
	(case-fold-search
	 tinymail--complete-bbdb-case-fold-search)
	one
	two
	key
	record)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (beginning-of-line)
    ;;  ["Jack E." "Den" nil nil nil
    ;;    |         |
    ;;    one       two
    (when (looking-at "^.\"\\([^\n\r\"]+\\)[ \t\"]+\\([^ \t\"]+\\)")
      (setq one (match-string 1)
            two (match-string 2))
      (if (string= one "nil")
          (setq one nil))
      (if (string= two "nil")
          (setq two nil))
      (cond
       ((and one two)
        (setq key (format "%s %s" one two)))
       (one
        (setq key one))
       (two
        (setq key two)))
      (setq record (bbdb-gethash (downcase key))))
    (goto-char point) ;; faster than save-excursion
    (if tinymail--debug
        (tinymail-debug fid one two 'key key '=> record))
    record))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-bbdb-record-net-completions (record)
  "Construct email completions for RECORD."
  (let ((fid "tinymail-bbdb-record-net-completions:")
	completion
	tmp
	name
	list)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (dolist (net (bbdb-record-net record))
      (when (and (stringp net)
                 (string-match "@" net))
        (setq completion
              ;;  If user has given a custom name to a NET,
              ;;  Like: Customer Support -- Phone Number <@>
              ;;  Then use that. Otherwise
              ;;  combine name and plain address
              (if (string-match "[<>]" net)
                  net
                (setq name  (bbdb-record-name record))
                ;; DO NOT ADD name "John doe" if address already
                ;; has those in john.doe@some.com because it looks
                ;; funny to read "john doe john.doeEMAIL" multiple times
                (if tinymail--debug
                    (tinymail-debug fid
                                    "\n\t" 'SPLIT
                                    (split-string name) net))
                (if (and (setq tmp (split-string name))
                         (> (length tmp) 1)
                         (string-match (regexp-quote (nth 0 tmp)) net)
                         (string-match (regexp-quote (nth 1 tmp)) net))
                    net
                  (format "%s <%s>" name net))))
        (push completion list)))
    (if tinymail--debug
        (tinymail-debug fid "\n\t" 'RET list))
    list))

;;; ----------------------------------------------------------------------
;;; Switched to another implementation (2). Read the matches
;;; directly from the BBDB data buffer, because it is faster than reading
;;; with `mapatoms' => obarray.
;;;
(defun tinymail-complete-list-bbdb-2 (regexp &optional check)
  "Return list of strings that match REGEXP in BBDB hash table.

Input:

  REGEXP  Regexp to match for mail fields
  CHECK  See `tinymail--complete-bbdb-fuzzy-method'."
  (let ((fid  "tinymail-complete-list-bbdb-2: ")
        buffer
        list
        str
        record)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;; The BBDB intrface code is filled with condition statements:
    ;;
    ;; (if tinymail--debug
    ;;     (tinymail-debug
    ;;
    ;; This prevents function call to happen, so that
    ;; the BBDB interface is as fast as possible.
    (when (and (featurep 'bbdb)
               (setq buffer (find-buffer-visiting bbdb-file)))
      (with-current-buffer buffer
        ;;  Don't want to see text properties in this buffer.
        (if (and (boundp 'font-lock-mode)
                 (symbol-value 'font-lock-mode))
            (font-lock-mode -1))
        (ti::pmin)
        (while (re-search-forward regexp nil t)
          (if tinymail--debug
              (tinymail-debug fid
                              'found (ti::buffer-read-space-word)
                              (ti::read-current-line)
                              "\n"))
          (when (setq record (tinymail-bbdb-data-read))
            (setq record (tinymail-bbdb-record-fix record))
            ;; ......................................... field match ...
            (if (null check)
                nil ;; (setq ok t)
              (dolist (func check)
                (when (and (functionp func)
                           (setq str (funcall func record))
                           (cond
                            ((stringp str)
                             (string-match regexp str))
                            ((and (listp str)
                                  ;; '((field . "str") ..)
                                  (ti::consp (car-safe str)))
                             (dolist (elt str)
                               (setq elt (cdr elt))
                               (when (and (stringp elt)
                                          (string-match regexp elt))
                                 (return t))))
                            ((and (listp str)
                                  (stringp (car-safe str)))
                             (dolist (s str)
                               (when (string-match regexp s)
                                 (return t))))))
                  (if tinymail--debug
                      (tinymail-debug fid 'MATCH regexp func str))
                  (return))))
            ;; .................................... make completions ...
            (dolist (elt (inline
                           (tinymail-bbdb-record-net-completions
                            record)))
              ;;  Previously used `pushnew' to to remove duplicates.
              ;;  push is faster. See `tinymail-complete-everything'
              ;;
              ;;  (pushnew elt list :test 'string=)
              (push elt list)))
          (forward-line 1))))
    (if tinymail--debug
        (tinymail-debug fid 'RET list))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-list-bbdb-1 (regexp &optional fields)
  "Return list of strings that match REGEXP and @ in BBDB hash table.

Input:

  REGEXP        Regexp to match for mail fields
  FIELDS        See `tinymail--complete-bbdb-fuzzy-method'."
  (let ((fid  "tinymail-complete-list-bbdb-1: ")
        list
        record
        completion
        tmp)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (when (featurep 'bbdb)
      (mapatoms
       (function
        (lambda (sym &optional symbol val name notes)
          (setq symbol (symbol-name sym))
          ;;  Look at all atoms in BBDB and try to find email addresses
          ;;  that have string that would match.
          (setq record (bbdb-gethash symbol))
          ;;  NOTE: upgrading from v3 to v5 BBDB database, the
          ;;  entries are returned as ([ ... ]) by
          ;;  bbdb-gethash, but this format is not suitable for
          ;;  calling (bbdb-record-net record)
          ;;
          ;;  The code below removes the extra () and only
          ;;  leaves RECORD [ .. ]
          (if (and (listp record)
                   (vectorp (setq tmp (car-safe record))))
              (setq record tmp))
          (when record
            (setq name  (bbdb-record-name record)
                  notes (bbdb-record-notes record)))
          ;; .......................................... select record ...
          (when fields
            ;;  If ANYTHING has been set:
            ;;  -- Compare element in BBDB if it is string
            ;;  -- Require at least 3 characters to compare
            ;;     (it makes no sense to complete one character "a")
            ;;  -- Match the element
            ;; ["John" "Doe" nil nil nil nil ("jdoe@example.com")
            ;;   ((creation-date . "2000-09-09") (timestamp . "2000-09-09")
            ;;   (notes . "that.el"))
            ;;   ["John Doe" nil #<marker at 114932 in bbdb-data.el> nil]])
            (when (and fields record)
              ;;  #todo: Is there function to `dolist' over vector list?
              (let ((i   0)
		    (len (if (integerp fields)
			     fields
			   3))
		    (max (1- (length record)))
		    elt)
                (when (>= (length regexp) len)
                  (while (< i max)
                    (setq elt (aref record i))
                    (incf i)
                    (if (not (listp elt))
                        (setq elt (list elt)))
                    (dolist (item elt)
                      ;;   Try CDR: (notes . "value")
                      ;;   or  CAR: ("string")
                      (if (listp item)
                          (setq item (or (cdr-safe item)
                                         (car-safe item))))
                      (when (and (stringp item)
                                 (string-match  regexp item))
                        (tinymail-debug fid 'ANYTHING regexp elt)
                        (setq max (1+ max))
                        (return))))))))
          ;; ..................................... make completions ...
          (when (and record
                     name
                     (prog1 t (tinymail-debug fid 'BBDB-SCAN record))

                     ;;  If there is case sensitive search in effect, check that,
                     ;;  before adding to completion list

                     (let ((case-fold-search
                            tinymail--complete-bbdb-case-fold-search))
                       (or (string-match regexp symbol)
                           (string-match regexp name)
                           (string-match regexp (or notes ""))))
                     (setq val (bbdb-record-net record)))
            (tinymail-debug fid
                            'MATCHED-OK
                            "REGEXP"  regexp
                            "SYMBOL"  symbol
                            "ATOM "   val
                            "RECORD"  record
                            "NET"     val
                            "NOTES"   notes)
            (dolist (net val)
              (when (and (stringp net)
                         (string-match "@" net))
                (setq completion
                      ;;  If user has given a custom name to a NET,
                      ;;  Like: Customer Support -- Phone Number <@>
                      ;;  Then use that. Otherwise
                      ;;  combine name and plain address
                      (if (string-match "[<>]" net)
                          net
                        ;; DO NOT ADD name "John doe" if address already
                        ;; has those in john.doe@some.com because it looks
                        ;; funny to read NAME EMAIL multiple times
                        (tinymail-debug fid
                                        "\n\t" 'SPLIT
                                        (split-string name) net)
                        (if (and (setq tmp (split-string name))
                                 (> (length tmp) 1)
                                 (string-match (regexp-quote (nth 0 tmp)) net)
                                 (string-match (regexp-quote (nth 1 tmp)) net))
                            net
                          (format "%s <%s>" name net))))
                (pushnew completion list :test 'string=)))) ;; When-end
          (setq record nil)))
       (bbdb-hashtable))

      (tinymail-debug fid 'RETURN-COMPLETIONS list)
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-list-bbdb (mode data)
  "Return list of matches from BBDB.

Input:

  MODE is the value of  `tinymail--complete-mode'.
  DATA can contain values returned from `tinymail-complete-string-read'."
  (when (or data
            (setq data (tinymail-complete-string-read)))
    (setq data (regexp-quote (nth 2 data)))
    (tinymail-complete-list-bbdb-2
     data
     tinymail--complete-bbdb-fuzzy-method)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-bbdb-fuzzy (&optional info &optional force)
  "Scan through BBDB 'net for partial matches and offer completion list.

Input:

  INFO   '(beg end STRING)  of the completion word
  FORCE  Normally this function completes only in Header To/Cc fields< but if
         this is non-nil, complete at point."
  (when (and (featurep 'bbdb)
             (or force (tinymail-field-in-to-cc-p))
             (eq tinymail--complete-mode 'string))
    (tinymail-debug 'tinymail-complete-bbdb-fuzzy info 'FORCE force)
    (let* ((fid    "tinymail-complete-bbdb-fuzzy:")
           (string (nth 2 info))
           (list   (and string
                        (tinymail-complete-list-bbdb-2
                         (regexp-quote string)))))
      (unless fid ;; No-op. XEmacs byte compiler silencer
        (setq fid nil))
      (tinymail-debug fid 'LIST list 'STRING string)

      (when list
        (cond
         ((eq 1 (length list))
          (if (tinymail-y-or-n-p (concat "TinyMail bbdb accept: " (car list)))
              (tinymail-complete-insert-completion (car list) info)
            (tinymail-y-or-n-p-abort-p)))
         (list
          (setq string (completing-read
                        (format "TinyMail bbdb fuzzy %d (empty to cancel): "
                                (length list))
                        (ti::list-to-assoc-menu list)))
          (unless (ti::nil-p string)
            (tinymail-complete-insert-completion string info)
            t)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-bbdb-fuzzy-at-point (info)
  "Call tinymail-complete-bbdb-fuzzy with INFO and `FORCE' argument."
  (tinymail-complete-bbdb-fuzzy info 'force))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-insert-completion (string info)
  "Replace the content with STRING by using the INFO.
INFO contains list '(begin-point end-point text-between-points)."
  (interactive)
  (delete-region (nth 0 info) (nth 1 info))
  (insert string)
  (skip-chars-backward " ")
  t)

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-header-complete-choices (field)
  "Return completion choices for HEADER-FIELD."
  (let ((fid "tinymail-header-complete-choices:")
	(ret (nth 1 (assoc field tinymail--table-header-complete))))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug fid 'CHOICES-RAW ret)
    ;;  If the first element is string, then suppose list of strings
    ;;  If not, evaluate `choices' to get list of strings.
    (when ret
      (if (not (stringp (car ret)))
          (setq ret (eval ret))))
    (tinymail-debug fid 'CHOICES-FINAL ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-simple (&optional info)
  "Complete according to `tinymail--table-header-complete'.
INFO is '(string beg end) of the completion word"
  (interactive)
  (ti::mail-point-in-header-macro
   (let* ((fid        "tinymail-complete-simple: ")
          (field-1    (ti::remove-properties (ti::mail-current-field-name)))
          (field      (and field-1
                           (capitalize field-1))) ;; gcc -> Gcc
          (field-info (or info
                          (tinymail-complete-string-read)))
          multi-word
          complete-list
          tmp
          choices
          string
          ret)
     (unless fid ;; No-op. XEmacs byte compiler silencer
       (setq fid nil))
     ;;  The EVAL-FORM may set this if it does not return `choices'
     (setq tinymail--complete-key-return-value nil)
     ;;  The STRING is dynamically bound and visible for EVAL CHOICES
     (when (stringp (setq string (nth 2 field-info)))
       (setq choices (tinymail-header-complete-choices field)))
     (tinymail-debug fid
                     'INFO    info
                     'FIELD   field
                     'STRING  string
                     'CHOICES choices)
     ;; ............................................... check choices ...
     (when choices
       (cond
        ((null string) ;; Empty field, user expects all completions
         (setq string (completing-read
                       (concat field ": ")
                       (ti::list-to-assoc-menu choices)))
         (unless (ti::nil-p string)
           (insert string)
           (setq ret t)))
        (t
         (setq choices (ti::list-to-assoc-menu choices))
         ;;  Forget choices that are multiwords "val val"
         (unless (string-match " " string)
           (setq complete-list (all-completions string choices))
           ;;  This is the common string at the beginning
           (setq tmp (try-completion string choices))
           (tinymail-debug fid
                           'COMPLETE-LIST complete-list
                           'TRY tmp
                           'str string)
           (dolist (completion complete-list)
             (when (string-match " " completion)
               (setq multi-word t)
               (return)))
           ;; ....................................... completion-list ...
           (cond
            ((null complete-list)
             (message "TinyMail: no simple completions matching `%s'" string))
            ((or (and (eq 1 (length complete-list))      ;; ONE found
                      (setq string (car complete-list))) ;; that's it
                 (and tmp
                      ;;  Don't accept partial match from "Multi Word"
                      ;;  completion strings.
                      multi-word
                      (not (ti::nil-p
                            (setq string
                                  (completing-read
                                   "Complete: "
                                   (ti::list-to-assoc-menu complete-list)
                                   nil
                                   nil
                                   ;; initial value
                                   tmp))))))
             (tinymail-complete-insert-completion string info)
             (setq ret t))

            ((and tmp (not (string= tmp string)))
             ;;  there was common denominator, complete further
             (tinymail-complete-insert-completion tmp info)
             (message "Tinymail complete:  %s"
                      (ti::list-to-string complete-list ", " ))
             (setq ret t))
            (complete-list
             (let (ret)
               (setq ret (completing-read
                          (concat field ": ")
                          (ti::list-to-assoc-menu complete-list)
                          nil
                          nil
                          string))
               (unless (ti::nil-p ret)
                 (tinymail-complete-insert-completion ret info)))
             ;; More than 1, stop and return t
             (setq ret t)))))))
     (tinymail-debug fid
                     "RET"    ret
                     "GLOBAL COMPLETE VALUE"
                     tinymail--complete-key-return-value)
     ;;  Return status if we did something in this function
     (or ret
         tinymail--complete-key-return-value))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-guest-packages (&optional arg)
  "Support minor modes like tinytab and tinyindent which also use TAB key.
Ignore ARG."
  (interactive "P")
  (let ((fid "tinymail-complete-guest-packages:")
	(ch  last-command-event))
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug fid 'ARG arg 'MODE major-mode 'POINT (point))
    ;;  The TinyTab minor mode overrides tab, return nil
    ;;  so that it can proceed
    (cond
     ((and (featurep 'tinytab)
           (symbol-value 'tinytab-mode)
           (fboundp 'tinytab-tab-key))
      (tinymail-debug fid 'tinytab-tab-key tinytab--tab-insert-hook)
      (ti::funcall 'tinytab-tab-key))
     ((and (featurep 'tinyindent)
           (symbol-value 'tinyindent-mode)
           (fboundp 'tinyindent-tab-key))
      (tinymail-debug fid 'tinyindent-tab-key)
      (ti::funcall 'tinyindent-tab-key))
     (t
      (when ch
        (self-insert-command 1)
        t)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-list-passwd (&optional mode data force)
  "Return list of matches from password file.

Input:

  MODE is the value of  `tinymail--complete-mode'.
  DATA can contain values returned from `tinymail-complete-string-read'."
  (let ((fid "tinymail-complete-list-passwd")
	str
	table
	completions)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (when (or data
              (setq data (tinymail-complete-string-read)))
      (setq str (regexp-quote (nth 2 data))))
    (if (or force
            (null tinymail--password-completion-alist))
        (tinymail-password-define-variables))
    (setq table       tinymail--password-completion-alist)
    (setq completions (all-completions str table))
    (tinymail-debug fid str "COMPLETIONS" completions)
    completions))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-display-list (list &optional flash)
  "Display LIST or alist in `tinymail--temp-buffer' or FLASH in echo area."
  (when list
    (if flash
        (message (ti::list-to-string (mapcar 'car list)))
      (let ((buffer (ti::temp-buffer tinymail--temp-buffer 'clear)))
        (with-current-buffer buffer
          (dolist (elt list)
            (insert (format "%-10s %s\n"  (car elt) (or (cdr elt) "")))))
        (display-buffer buffer)
        (ti::save-excursion-macro ;; Go and make displayed buffer small
          (select-window (get-buffer-window buffer))
          (shrink-window-if-larger-than-buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-password-grep (match &optional verb)
  "Grep USER from passwd.

Input:

  MATCH     String, to grep
  DISPLAY   flag, display results in separate buffer.
  VERB      flag, Verbose messages"
  (interactive "sUser regexp: ")
  (let ((fid      "tinymail-password-grep")
        alist)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (ti::verb)
    (tinymail-debug fid match verb)
    (if (null tinymail--password-alist)
        (tinymail-password-define-variables))
    ;;   Force loading it if not exist
    (if verb (message "Grepping passwd contents..."))
    (setq alist (ti::file-passwd-grep-user-alist
                 match nil tinymail--password-alist))
    (if verb (message "Grepping...done"))
    alist))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-headers-move-to-next-field (&rest ignore)
  "Move to next field if cursor is at the end of field in header."
  (interactive)
  (ti::mail-point-in-header-macro
   (let ((str (buffer-substring (line-beginning-position) (point)))
	 (max (ti::mail-text-start)))
     (when (and (not (ti::nil-p str))
                (eolp))
       (when (re-search-forward ":." max t)
         (end-of-line))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-headers-nothing-found (&rest ignore)
  "Display 'No completions found' in header and return t. IGNORE arguments.
Advance by 4 spaces if there is only spaces to the left."
  (interactive)
  (ti::mail-point-in-header-macro
   ;; User started a continuing line. Point is at mark (!)
   ;;
   ;;   CC: some@example.com
   ;;   !
   ;;   To: him@there.at
   ;;
   (cond
    ((or (ti::nil-p (buffer-substring (line-beginning-position) (point)))
         (char-equal (char-syntax (preceding-char)) ?\ ))
     (insert "    "))
    ((ti::mail-point-at-header-p)
     ;;  this message is displayed only when cursor is next to character
     ;; (forward-word 1)
     (message "TinyMail: No completions found.")
     t)
    (nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-abbrevs (&optional info)
  "Complete using abbrevs. INFO."
  ;; Actually we don't need this because SPACE already expands abbrevs
  ;; if abbrev mode is on.
  ;; (expand-abbrev)
  nil)

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-key-remove-itself ()
  "Remove calls from `tinytab--tab-insert-hook'. See
Source code of `tinymail-complete-key' why. "
  ;;  In calling function this variable is `let' bound, so the
  ;;  change is temporary.
  (let (clean-hook)
    (when (boundp 'tinytab--tab-insert-hook)
      (dolist (function tinytab--tab-insert-hook)
        (if (not (string-match "tinymail"
                               (or (symbol-name function) "")))
            (push function clean-hook)))
      (setq tinytab--tab-insert-hook (nreverse clean-hook)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-tab-to-tab-stop (&rest args)
  "Ignore ARGS and call `tab-to-tab-stop'."
  (tab-to-tab-stop))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-key (&optional header-check)
  "Run functions in `tinymail--complete-key-hook'.
Te first function that return non-nil terminates calling the rest of the
functions. Each function is passed the word info at point: '(BEG END STRING)."
  (interactive)
  (tinymail-debug 'tinymail-complete-key
                  'BEGIN
                  tinymail--complete-key-hook)
  ;; It makes no use to call this function anywhere elase than in Mail
  ;; buffer. (this prevent's double call from tinytab.el too)
  (when (ti::mail-mail-p)
    (tinymail-debug 'tinymail-complete-key
                    'HEADER-CHECK header-check
                    (save-excursion
                      (concat "\n************ START **********\n"
                              (buffer-substring
                               (progn (forward-line -2) (point))
                               (progn (forward-line 2) (point)))
                              "\n************ END **********\n")))
    (let ((fid "tinymail-complete-key:")
	  ;; Make copies of these
	  (tinytab--tab-insert-hook
	   (if (boundp 'tinytab--tab-insert-hook)
	       (symbol-value 'tinytab--tab-insert-hook)))
	  (tinymail--complete-key-hook tinymail--complete-key-hook)
	  string
	  ret)
      (unless fid ;; No-op. XEmacs byte compiler silencer
        (setq fid nil))
      ;; Avoid resursive calls by removing all tinymail entries
      (tinymail-complete-key-remove-itself)
      ;;  It doesn't make sense to run mail completions inside BODY,
      ;;  remove unnecessary hooks
      (cond
       ((ti::mail-point-at-body-p)
        (tinymail-debug 'tinymail-complete-key 'POINT-INSIDE-BODY)
        ;; Leaves
        ;;
        ;; tinymail-complete-abbrevs
        ;; tinymail-complete-guest-packages
        (ti::add-hooks 'tinymail--complete-key-hook
                       '(tinymail-complete-everything
                         tinymail-complete-simple
                         tinymail-complete-guess-in-headers
                         tinymail-complete-headers-nothing-found
                         tinymail-complete-headers-move-to-next-field)
                       'remove)
        (cond
         ((and (boundp 'tinytab-mode)
               tinytab-mode
               (fboundp 'tinytab-indent-by-tab-width))
          (add-hook 'tinymail--complete-key-hook
                    'tinytab-indent-by-tab-width))
         (t
          (add-hook 'tinymail--complete-key-hook
                    'tinymail-tab-to-tab-stop)))
        (tinymail-debug fid 'BODY-AREA tinymail--complete-key-hook))
       (t
        (tinymail-debug 'tinymail-complete-key 'POINT-INSIDE-HEADER)
        (setq string (tinymail-complete-string-read))
        ;;  Complete only in CC, Bcc, To .. fields. If not there,
        ;;  remove function
        (when (and header-check
                   (not (ti::mail-field-email-address-p)))
          (tinymail-debug 'tinymail-complete-key 'NOT-IN-TO-CC-BCC)
          (tinymail-debug fid 'header-check 'REMOVED
                          'tinymail-complete-everything)
          (remove-hook 'tinymail--complete-key-hook
                       'tinymail-complete-everything))))
      ;; .................................................... cond-end ...
      (tinymail-debug fid 'LOOPING-LIST tinymail--complete-key-hook)
      (dolist (func tinymail--complete-key-hook)
        (tinymail-debug fid 'FUNC func string)
        (when (cond
               ((not (fboundp func))
                (tinymail-debug fid 'FUNC func "not exist")
                nil)
               (t
                (funcall func string)))
          (setq ret t)
          (return)))
      (tinymail-debug fid fid 'RET ret)
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-complete-key-interactive ()
  "See `tinymail-complete-key'. Comlete only in header."
  (interactive)
  (tinymail-complete-key 'only-complete-in-headers))

;;}}}
;;{{{ advice

;;; .......................................................... &advice ...

;;  Old message has autosave name "*message*", but that does not work in
;;  Win32 platform (C-x m  M-x message-mode and  C-x s and Emacs
;;  dies on error)

(when (ti::win32-p)
  (require 'message)
  (if (string-match
       "[*]message"
       (prin1-to-string
        (symbol-function 'message-set-auto-save-file-name)))
      (defadvice message-set-auto-save-file-name (around tinymail act)
        "\
Replace function. Change the autosave name from *message* to #message# due to Win32"
        (when message-auto-save-directory
          (if (gnus-alive-p)
              (setq message-draft-article
                    (nndraft-request-associate-buffer "drafts"))
            (setq buffer-file-name
                  (expand-file-name "#message#"
                                    message-auto-save-directory))
            (setq buffer-auto-save-file-name (make-auto-save-file-name)))
          (clear-visited-file-modtime)
          (setq buffer-file-coding-system message-draft-coding-system)))))

;;}}}
;;{{{ Extra

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-save-dead-mail-maybe ()
  "Call `tinymail-save-dead-mail' only if RMAIL is used as MUA.
All other Agents have some sort of 'todo' message save feature."
  ;;  VM after sending, keeps the corresponding mail
  ;;  buffer which implies that the dead letter facility
  ;;
  ;;  Gnus has also Gcc feature; but we can't know if User uses it for mail?
  ;;  User may only read News.
  (when (featurep 'rmail)
    (tinymail-save-dead-mail)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-save-dead-mail ()
  "Save mail buffers to `tinymail--dead-mail-file' on Emacs exit."
  (ti::dolist-buffer-list
   (memq major-mode '(mail-mode
                      message-mode))
   'loop-temp-buffers
   nil
   (progn
     ;;  In message.el is possible to "save a draft" in normal manner:
     ;;  C-x C-s. If the mail buffer has already been saved, we ignore
     ;;  that buffer.
     (when (buffer-modified-p)
       (set-buffer-modified-p nil)      ;"no changes in this buffer"
       (append-to-file
        (point-min)
        (point-max)
        tinymail--dead-mail-file)))))

;;}}}

;;{{{ Email notification (old Dragbar Time package)

;;; ...................................................... &reportmail ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-get-email-word (str)
  "Return first word, separated by space from STR."
  (let ((word str))                    ;set default
    (when (string-match "From \\([^ ]+\\) " str)
      (setq word (substring str (match-beginning 1)  (match-end 1))))
    word))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-break-email (str)
  "Break email STR into two words.
Return:
   (ACCOUNT SITE)  or nil"
  (let (w1
	w2
	ret)
    (when (string-match "[@!]" str)
      (setq w1 (substring str 0 (match-beginning 0))
            w2 (substring str (1+ (match-beginning 0))))
      ;;   Some sites has "from" command that sends the info in format:
      ;;     "From site.com!login Mon Feb 26 15:50:18 1996"
      ;;
      ;;   And not in traditional format
      ;;     "From login@site.com Mon Feb 26 15:50:18 1996"
      ;;
      ;;   We have to swap the order
      (cond
       ((string-match "!" str)
        ;; then swap order, since word1 = site, w2 = account
        (setq ret (list w2 w1)))
       (t
        (setq ret (list w1 w2)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-mail-info-1 (shell-call)
  "Run SHELL-CALL to get information about arrived mail.

Input:

  SHELL-CALL    If string, run `shell-command'.

                If function, call function with no arguments.

                Otherwise eval it.

                The SHELL-CALL must return Mailbox From information
                to current empty buffer. Oldest entries first, newest last.

Return:

   list       (line line ..)  Berkeley MBOX 'From ' lines. Oldest first.
   nil        No new mail"
  (let ((default-directory  default-directory)
	(buffer             (get-buffer-create tinymail--report-spool-buffer))
	(kill-p             (eq tinymail--report-spool-buffer-control 'kill))
;;; #todo: not yet used
;;;      (timeout            tinymail--report-asychronous-timeout)
	(tmp-dir            "/tmp/")
	(kill-re            tinymail--report-mail-kill-line-regexp)
	ret)
    (unwind-protect
        (with-current-buffer buffer
          (erase-buffer)
          ;; - launch up the process and restore the directory setting
          ;; - The output is like:
          ;;
          ;;   From aa@zig.com Thu May 11 19:05:36 EET 1995
          ;;   From bb@zag.com.edu Thu May 11 18:55:59 EET 1995
          (setq default-directory tmp-dir)
          (cond
           ((stringp shell-call)
            (shell-command shell-call buffer))
           ((fboundp shell-call)
            (funcall shell-call))
           (t
            (eval shell-call)))
          (when (stringp kill-re)
            (ti::pmin)
            (flush-lines kill-re))
          (unless (eq (point-min) (point-max)) ;No output ?
            ;;  - Now read persons email delimited by spaces
            ;;  - Read the last line to get newest mail arrival
            (ti::pmin)
            (while (not (eobp))
              (push (ti::read-current-line) ret)
              (forward-line 1))))
      ;; Unwind
      (when (and kill-p (buffer-live-p (setq buffer (get-buffer buffer))))
        (with-current-buffer buffer
          (set-buffer-modified-p nil))  ;No confirmations
        (kill-buffer buffer)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-mail-info ()
  "Run `tinymail--report-mail-notify-program'."
  (and tinymail--report-mail-notify-program
       (tinymail-report-mail-info-1 tinymail--report-mail-notify-program)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-get-mail-info-string ()
  "Return mail string: last sender and mail count."
  (let ((list          (tinymail-report-mail-info))
	(re            tinymail--report-mail-info-shorten-regexp)
	(ret           tinymail--report-no-mail-string)
	last-line
	email
	count)
    (tinymail-debug 'tinymail-report-get-mail-info-string list)
    (cond
     ((ti::listp list)
      (setq count       (length list)
            last-line   (car (nreverse list))
            email       (tinymail-report-get-email-word last-line))
      ;;  does user want shortened version ?
      (when (and (stringp re)           ;no regexp
                 (string-match re email)
                 (setq list (tinymail-report-break-email email)))
        (setq email (nth 0 list)))
      (setq ret (concat " " email " " (number-to-string count))))
     ((not (null list))
      (message "TinyMail: *** tinymail-report-mail-info didn't return list")))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinymail-report-mail (&optional verb)
  "Call `tinymail-report-mail-install-maybe'."
  (ti::verb)
  (tinymail-report-mail-install-maybe verb))

;;; ----------------------------------------------------------------------
;;;
(defun turn-off-tinymail-report-mail (&optional verb)
  "Call `tinymail-report-mail-install' with prefix argument."
  (interactive)
  (ti::verb)
  (tinymail-report-mail-install 'uninstall verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-update (&rest args)
  "Update mail status information.
Update the frame's status line, or in non-X show the message in echo area.
ARGS are ignored."
  (let* ((buffer  (and (stringp tinymail--report-spool-buffer)
                       (get-buffer-create tinymail--report-spool-buffer)))
         (raise   (and buffer
                       (eq tinymail--report-spool-buffer-control 'raise)))
         str
         mail-info
         display-string)
    (when buffer
      ;;  Save the contents of frame name, e.g. host name only once
      (if (and tinymail--report-window-system
               (null tinymail--report-old-frame-string))
          (setq tinymail--report-old-frame-string
                (ti::compat-set-frame-name nil nil 'get)))
      (when (stringp (setq mail-info (tinymail-report-get-mail-info-string)))
        (setq tinymail--report-mail-info-string (format " %s " mail-info))
        (setq display-string (eval tinymail--report-format-string)))
      (cond
       ;; ..................................................... windowed ...
       (tinymail--report-window-system
        (dolist (elt (frame-list)) ;Update frames that are not exluded
          (if (not (member
                    (ti::compat-set-frame-name nil nil 'get)
                    tinymail--report-keep-intact-list))
              (ti::compat-set-frame-name display-string elt))))
       ;; ...................................................... non-win ...
       ;; Do nothing if this is nil, user doesn't want to see evel "No mail"
       ;; message.
       ((null mail-info))
       ;; ................................................. non-windowed ...
       (t
        (if (and (stringp tinymail--report-no-mail-string)
                 (not (string= mail-info tinymail--report-no-mail-string)))
            (setq str "Mail: "))
        (cond
         ((and (not (ti::compat-executing-macro))
               ;; printing message while user is in minibuffer
               ;; makes it impossible to see what he's doing.
               (not (eq (selected-window) (minibuffer-window)))
               (sit-for 0.50))
          (message "%s%s" (or str "") display-string)
          ;;  make sure user sees it
          (sleep-for 1)))))
      ;; .......................................................... beep ...
      ;; Notify about new mail ?
      (unless (stringp tinymail--report-old-mail-info-string)
        (setq tinymail--report-old-mail-info-string
              tinymail--report-no-mail-string))
;;;    (ti::d!! mail-info tinymail--report-no-mail-string tinymail--report-old-mail-info-string "\n")
      (when (and (stringp mail-info)
                 (stringp tinymail--report-no-mail-string)
                 (not (string= mail-info
                               tinymail--report-no-mail-string))
                 (not (string= mail-info
                               tinymail--report-old-mail-info-string)))
        (setq tinymail--report-old-mail-info-string mail-info)
        (beep)
        (sit-for 0.15)
        (beep)
        (if raise
            (display-buffer buffer)))
      (setq tinymail--report-old-mail-info-string mail-info))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-mail-install (&optional uninstall verb)
  "Install or UNINSTALL mail watchdog (report mail).
References:
  `tinymail--report-window-system'"
  (interactive "P")
  (ti::verb)
  (if (featurep 'reportmail)
      (message "\
TinyMail: tinymail-report-mail-install: 'reportmail feature found, install ignored.")
    (let (process-connection-type)     ;Nicer process communication
      (if tinymail--display-time
          (display-time))) ;; time.el
    ;; In XEmacs the frame must be configured by hand
    (when (and tinymail--report-window-system (ti::xemacs-p))
      ;;  make sure it's list
      (setq frame-title-format (ti::list-make frame-title-format))
      (if uninstall
          (delete 'tinymail--report-mail-info-string frame-title-format)
        (pushnew 'tinymail--report-mail-info-string
                 frame-title-format
                 :test 'equal)))
    ;; Delete old timer
    (ti::compat-timer-cancel-function 'tinymail-report-update)
    (setq tinymail--report-timer-object nil)
    (unless uninstall
      (setq tinymail--report-timer-object
            (run-at-time "1 min" (* 60 10) 'tinymail-report-update)))
    (when verb
      (message "TinyMail: Report mail feature is %s"
               (if uninstall
                   "OFF"
                 "ON")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-report-mail-install-maybe (&optional uninstall verb)
  "Don't call `tinymail-report-mail-install' if there already exists reporter.
E.g. in XEmacs you can use package reportmail.el."
  (interactive "P")
  (ti::verb)
  (cond
   ;;  #todo: Any other report features we should check?
   ((featurep 'reportmail)
    (message "TinyMail: reportmail.el present, not installing."))
   (t
    (tinymail-report-mail-install uninstall verb))))

;;}}}
;;{{{ From-address generator (sendmail PLUS emulation)

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-from-anti-ube-maybe ()
  "Return anti-ube address if `newsgroups' match `tinymail--from-anti-ube-regexp'"
  (when (and (stringp tinymail--from-anti-ube-regexp)
             (stringp user-mail-address)
             user-mail-address)
    (let ((group (mail-fetch-field "Newsgroups")))
      (when (string-match tinymail--from-anti-ube-regexp
                          (or group ""))
        ;;  - Because the anti-ube returns different email every
        ;;    time it is called, cache the first value.
        ;;  - The changing value would otherwise cause indication
        ;;    "Headers have changed".
        (make-local-variable 'tinymail--user-mail-address)
        (let ((addr (or tinymail--user-mail-address
                        (ti::mail-email-make-anti-spam-address
                         user-mail-address))))
          (setq tinymail--user-mail-address addr)
          addr)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-from-field-value-plus ()
  "Return special plus address emulation (RFC Comment)."
  (let* ((fid      "tinymail-from-field-value-plus:")
         (news     (mail-fetch-field "newsgroups"))
         (prefixes  tinymail--from-table-prefix)
         (postfixes tinymail--from-table-postfix)
         ;;  Posting from Gnus, so get the Group name
         ;;
         ;;   backend:mail.xxx --> mail.xxx
         (grp      (ti::mail-news-group))
         (group    (and (stringp grp)
                        (eq major-mode 'message-mode)
                        (or (ti::string-match ":\\(.*\\)" 1 grp)
                            grp)))
         prefix
         postfix
         ret
         msg-postfix
         condition)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (cond
     ;; ............................................... news followup ...
     (news
      ;;  Direct some mesages to my "mail" group, others to
      ;;  general usenet group.
      (setq prefix  (cdr-safe (ti::list-find prefixes news )))
      (setq postfix "")) ;; (cdr-safe (ti::list-find prefixes news )))
     (group
      ;; If posting from inside Group, add Group based PLUS address
      ;;
      ;;  list.xxx --> '("list" "xxx")
      ;;  For Imap folders; INBOX.list.foo => list.foo
      (when (string-match "INBOX\\.\\(.+\\)" group)
        (setq group (match-string 1 group)))
      (setq prefix  (ti::string-match "^\\([^.]+\\)" 1 group)
            postfix (ti::string-match "\\.\\(.+\\)"  1 group))))
    ;; ................................ according to message content ...
    (setq msg-postfix
          (save-excursion
            (dolist (elt postfixes)
              (ti::mail-text-start 'move)
              (setq condition (car elt))
              (when (if (stringp condition)
                        (re-search-forward condition nil t)
                      (setq ret (funcall condition)))
                (unless ret
                  (tinymail-debug fid 'point (point) 'LOOP-SELECT elt )
                  (setq ret (cdr elt)))
                (return)))
            ret))
    ;; ............................................... guess mail type ...
    ;; If not yet set, look at message and decide right postfix
    (setq ret
          (cond
           (msg-postfix                 ;Always obey this
            msg-postfix)
           ((and (ti::nil-p prefix) postfix)
            postfix)
           ((and (ti::nil-p postfix) prefix)
            prefix)
           ((and prefix postfix)
            (concat prefix "." postfix))))
    (tinymail-debug fid 'ret ret
                    'NEWS           news
                    'GROUP          group
                    'msg-postfix    msg-postfix
                    'prefix prefix 'postfix postfix )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-from-field-value ()
  "Make From Address.

References:

  `user-full-name'
  `user-mail-address'
  `tinymail--from-info-function'
  `tinymail--from-field-plus-separator'"
  (interactive)
  (let* ((fid      "tinymail-from-field-value:")
         (separator tinymail--from-field-plus-separator)
         (info     (and (fboundp tinymail--from-info-function)
                        (funcall tinymail--from-info-function)))
         (address   (or (and (listp info)
                             (nth 0 info))
                        (tinymail-from-anti-ube-maybe)
                        (or (stringp user-mail-address)
                            (error
                             "TinyMail: Please set `user-mail-address'."))))
         (name      (or (user-full-name)
                        (error "TinyMail: Please set `user-full-name'")))
         (plus      (or (and (listp info)
                             (nth 1 info))
                        (tinymail-from-field-value-plus)))
         localpart
         domain
         ret)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;;   With procmail you can have plus addresses:
    ;;
    ;;      login+additional-info@site.com
    ;;
    ;;   But you can accomplish the same with RFC comment syntax
    ;;
    ;;     login@site.com (Foo Bar+additional-info)
    ;;
    ;;   The extra "+" is just added there to mark that this is
    ;;   PLUS addess.
    (cond
     ((and (not (ti::nil-p plus))
           (not (ti::nil-p name)))
      (setq plus (format " (%s%s%s)" name separator plus )))
     ((not (ti::nil-p name))      ;If no plus info, use normal address
      (setq plus (format " (%s)" name))))
    (when (stringp address)
      (setq localpart (or (ti::string-match "^[^@]+" 0 address) "")
            domain    (or (ti::string-match "@.*"    0 address) ""))
      ;;  RFC 1036/2.1.1 Says that following address formats are preferred in
      ;;  USENET posts
      ;;
      ;;  From: mark@cbosgd.ATT.COM
      ;;  From: mark@cbosgd.ATT.COM (Mark Horton)
      ;;  From: Mark Horton <mark@cbosgd.ATT.COM>
      (setq ret (format "%s%s%s" localpart  domain  (or plus "") )))
    (tinymail-debug fid
                    'info-function tinymail--from-info-function
                    'info info
                    'localpart localpart
                    'domain domain
                    'name
                    'RETURN ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-message-disable-sender ()
  "Disable Sender field generation permanently."
  ;; Gnus message-mode
  ;; Don't generate Sender address, but trust From address
  (interactive)
  (when (boundp 'message-syntax-checks)
    (let* ((syntaxes  (and (boundp 'message-syntax-checks)
                           (symbol-value 'message-syntax-checks)))
           ;; Gnus Group / Agent J S  comamdn set this to
           ;; value 'dont-check-for-anything-just-trust-me
           ;; => skip any checks
           (list-p    (ti::listp syntaxes))
           (pointer   (and list-p
                           (assq 'sender syntaxes))))
      (when list-p
        (if pointer
            (setcdr pointer 'disabled)
          (add-to-list 'message-syntax-checks '(sender . disabled)))))))

;;}}}

;;{{{ code: Cc, X-Sender-Info

;;; ............................................................. &fld ...

(defun tinymail-field-cc-kill-by-regexp ()
  "Kill entry from CC field that match `my--email-regexp'"
  ;;  don't touch CC field if user has put two spaces in front.
  (when (and (mail-fetch-field "CC")
             (not (tinymail-field-off-p "CC"))
             (stringp tinymail--cc-kill-regexp))
    (let* ((cc      (mail-fetch-field "cc"))
           (cc-list (and cc (split-string cc ",[ \t\n]*")))
           (count   0)
           ccl)
      (when cc-list
        (dolist (elt cc-list)
          (unless (string-match tinymail--cc-kill-regexp elt)
            (incf count)
            (setq ccl (format "%s\n  %s," (or ccl "")  elt))))

        (when (and cc-list  (not (stringp ccl)))
          (ti::mail-kill-field "^CC")) ;All CC memebers killed. Wipe field

        (when (and (stringp ccl)
                   (not (eq count (length cc-list)))) ;items removed
          ;; delete leading \n and trailing comma
          (setq ccl (substring ccl 1 (1- (length ccl))))
          (ti::mail-kill-field "^CC" ccl)
          ;;  we did something
          t)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-in-to-cc-p ()
  "Check if point is at field To, Bcc, Cc."
  (and (< (point) (ti::mail-hmax))
       (save-excursion
         (and (ti::mail-next-field-start 'move 'back)
              (looking-at "CC\\|BCC\\|To")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-to-move-maybe ()
  "Move cursor to the end of TO field if it is empty."
  (when (save-excursion (beginning-of-line) (looking-at "To: *$"))
    (end-of-line)
    (tinymail-debug "TO-move:" (ti::read-current-line) (point))))

;;}}}
;;{{{ code: Fcc handling

;;; ......................................................... &fld-fcc ...

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-fcc-determine (&optional type hsize)
  "Look if default folder must be changed.
Tries to find RE given in `tinymail--table-fcc' by looking at header area.

Input:

  TYPE      nil: Find Fcc folder. 'gcc: Find Gcc folder.
  HSIZE     The header size precalculated.

Return:

   string   suggested folder
   nil"
  (let* ((fid   "tinymail-field-fcc-determine: ")
         (ptr   (if type
                    tinymail--table-gcc
                  tinymail--table-fcc))
         (sym    (if type 'gcc 'fcc))
         (get-hsize (get 'tinymail--table-fcc 'hsize))
         (get-sym   (get 'tinymail--table-fcc sym))
         hmax
         ret
         re
         folder)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (or hsize
        (setq hsize  (ti::mail-header-area-size)))
    (tinymail-debug fid "IN"
                    "SYM"        sym
                    "HSIZE"      hsize
                    "GET HSIZE"  get-hsize
                    "GET SYM"    get-sym)
    (when (not (and (eq hsize get-hsize)
                    ;;   Previous folder value
                    (setq ret get-sym)))
      ;; Header are has changed; calculate new field and update values
      (put 'tinymail--table-fcc 'hsize hsize)
      (put 'tinymail--table-fcc sym nil)
      (when (setq hmax (ti::mail-hmax)) ;header end must be found
        (save-excursion
          (ti::pmin)
          (dolist (elt ptr)
            (setq re     (nth 0 elt)
                  folder (nth 1 elt))
            (when (re-search-forward re hmax t)
              (setq ret folder)
              (return)))))
      (if (and (stringp ret)
               (string-match "gz$\\|Z$" ret))
          (ti::use-file-compression))
      (put 'tinymail--table-fcc sym ret)
      (tinymail-debug  fid "SET hmax" hmax "ret" ret  "re" re))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-fcc (&optional type hsize)
  "Set right [GF]cc folder if there is match in `tinymail--table-[gf]cc'.

Input:

  TYPE      nil: Find Fcc folder. 'gcc: Find Gcc folder.
  HSIZE     The header size precalculated."
  (let ((fid    "tinymail-field-fcc: ")
	fld
	sym
	str
	folder
	prev)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    (tinymail-debug fid "in TYPE" type "HSIZE"  hsize)
    (or hsize
        (setq hsize (ti::mail-header-area-size)))
    (cond
     (type (setq fld "GCC"  sym 'gcc-old))
     (t    (setq fld "FCC"  sym 'fcc-old)))
    (setq prev   (get 'tinymail--table-fcc sym))
    (setq folder (tinymail-field-fcc-determine type hsize))
    (tinymail-debug fid fld type hsize
                    "PREV-FLD"  prev
                    "FLD"       folder
                    "CHECK"     (if folder
                                    (ti::re-search-check (regexp-quote folder)))
                    "MODE"      major-mode)

    (when (and (stringp folder)
               (or (not (stringp prev))
                   (not (ti::re-search-check (regexp-quote folder)))
                   (not (string-match (regexp-quote prev) folder)))
               (setq str (mail-fetch-field fld))
               (not (tinymail-field-off-p nil str)))
      (put 'tinymail--table-fcc sym folder)
      (ti::save-line-column-macro nil nil
        (tinymail-debug fid "SET" folder (current-buffer))
        (ti::mail-kill-field (concat "^" fld) folder)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-to-off (&optional count field)
  "Disable TinyMail by space COUNT for FIELD.
2  spaces disables Cc tracking
3  spaces disables both Cc and other tracking."
  (let (str)
    (setq str (make-string (or count 2) ?\ ))
    (setq field (or field "to"))
    (tinymail-debug "tinymail-field-to-off, count, field" count field)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward
             (concat "^" field ":\\([ \t]*\\)")
             (ti::mail-hmax)
             t)
        (if (match-beginning 1)
            (ti::replace-match 1 str)   ;There is spaces
          (insert str))                 ;There is no spaces
        t))
    (tinymail-field-to-move-maybe)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-field-to-on ()
  "Keep activated by making sure the To: field has only one space."
  (tinymail-debug "tinymail-field-to-on")
  (tinymail-field-to-off 1))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinymail-on-off-toggle (&optional arg)
  "Toggle TinyMail mode on and off by Changing spacing of To field.
This affects automatic Cc and X-Sender-Info tracking.
ARG behaves  like mode argument.

Without arg, this toggless Cc tracking, with prefix argument,
it toggless both Cc and X-Sender-Info tracking."
  (interactive "P")
  (if arg
      (setq arg 3))                     ;3 spaces turn off completely.
  (if (null arg)
      (cond
       ((save-excursion
          (ti::pmin)
          (re-search-forward "^to:  " nil t))
        (tinymail-field-to-on)
        (message "TinyMail: mail field tracking mode on."))
       (t
        (tinymail-field-to-off arg)
        (message "TinyMail: mail field tracking mode off.")))
    (tinymail-field-to-off arg)
    (message "TinyMail: mail field tracking mode off.")))

;;}}}
;;{{{ code: citation

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-iso8601-date-value ()
  "Read Date field and return ISO 8601 date: WEEKDAY YYYY-MM-DD."
  (let ((date "")
	yyyy
	mm
	dd
	week-day)
    (when (setq date (or (mail-fetch-field "date")
                         (and (featurep 'message)
                              message-reply-headers
                              (mail-header-date message-reply-headers))))
      (setq date (ti::date-parse-date date))

      (setq yyyy     (nth 0 date)
            mm       (nth 1 date)
            dd       (nth 2 date)
            week-day (nth 4 date))

      (setq date
            (format "%s%s%s%s"
                    (or week-day "")
                    (if yyyy             (concat (if week-day " " "") yyyy))
                    (if (and yyyy mm)    (concat "-" mm) "")
                    (if (and yyyy mm dd) (concat "-" dd) ""))))
    date))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-citation-who-said (str)
  "Formats sender line reference. Input is From/To field.

Return:

  str  formatted line, without 'From:'
  nil  if cannot format"
  (let ((limit (- 75 13))              ; line-lenght - date  ==> limit
        ;;  Get the group name only when posting from GNUS
        ;;  gnus-group-real-name
        (grp        (replace-regexp-in-string
                     ".*:" "" (or (ti::mail-news-group) "")))
        list
        fn                              ; first name
        sn                              ; surname
        email
        ret)
    (setq ret str)
    (when ret
      ;;  Remove quotes: "Mr. this" <email@example.com>
      (setq ret (replace-regexp-in-string "['\"]+" "" ret)))
    ;;   If the line is exessive long, say;
    ;;   "Mr. Foo the most spectacular..." <foo@camel.com>
    ;;   Then we make it smaller.
    (when (> (length ret) limit)
      ;;  Get only the email, and drop all others
      (setq list  (ti::mail-parse-name str))
      (setq email (or (car-safe (ti::mail-email-from-string str)) ""))
      (when list
        (setq fn (nth 0 list)   sn (nth 1 list)) ;; first/surname
        ;;   this should suffice
        (setq ret (concat fn " " sn " <" email ">"))))
    ;;  Does the group name fit in too ?
    (if (and grp
             (< (+ (length ret) (length grp) 1) limit))
        (setq ret (concat ret " " grp)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-message-id-value ()
  "Return Google group url."
  (let ((id (mail-fetch-field "References")))
    (and id
         ;;  There are several Message-Id's in a thread. Pick latest.
         (setq id  (car (nreverse (split-string id))))
         (ti::string-match "<\\([^ \t\n>]+\\)>" 1 id))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-url-reference-google-group ()
  "Return Google group url."
  (let* ((id     (tinymail-message-id-value))
         (group  (and id
                      (mail-fetch-field "Newsgroups"))))
    (when (and (stringp group)
               ;;  See http://groups.google.com/
               (string-match
                group
                (concat
                 "^\\(alt\\|biz\\|comp\\|humanities"
                 "\\|misc\\|news\\|rec\\|sci\\|soc\\|talk")))
      (concat
       ;; Old format was
       ;;  http://search.dejanews.com/msgid.xp
       ;;  ?MID=%3C3cgd8m0w.fsf@blue.sea.net%3E&format=threaded
       ;;
       "<http://groups.google.com/groups?oi=djq"
       "&as_umsgid=%3C"
       id
       ">"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-url-reference-mailing-list ()
  "Return maling list URL refence."
  (when (or (ti::mail-to-list-p)
            (string-match
             "^gmane"
             (or (mail-fetch-field "Newsgroups") "")))
    (let ((id (tinymail-message-id-value)))
      (concat "Message-Id: " id))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-message-id ()
  "Return message id or empty.
This function works best with Gnus:

- Mailing lists replies contain Message-Id reference.
  The mailing list status is indicated by Gnus group property `to-list'.
- Newsgroup replies contain URL reference.
- Private mail does _not_ include any extra references."
  (let ((url  (or (tinymail-url-reference-google-group)
		  (tinymail-url-reference-mailing-list))))
    (when (stringp url)
      (concat "* " url "\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-message-citation-line-function ()
  "Generate citation line.

    * Tue YYYY-MM-DD John Doe <johnd@example.com> mail.emacs
    * Message-Id: <......>
    | Thankyou for helping me...
    | ...

It is important to include the Message-Id reference because then it is
possible to retrieve whole News thread e.g. from GOOGLE group. Message-Id is
a handly way to refer to past articles."
  (let* ((hdrs (if (boundp 'message-reply-headers)
                   (symbol-value 'message-reply-headers)))
         (from (and hdrs (tinymail-citation-who-said
                          (mail-header-from hdrs))))
         (date (tinymail-iso8601-date-value)))
    (delete-horizontal-space)
    (insert "* " date
	    (if from
		(format " %s" from)
	      "")
	    "\n")
    (let ((id (funcall tinymail--citation-message-id-function)))
      (when (stringp id)
        (insert id)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-citation-generate ()
  "Write reference line."
  (if (eq major-mode 'message-mode)
      (tinymail-message-citation-line-function)
    (tinymail-citation-who-said (mail-fetch-field "From"))))

;;}}}
;;{{{ GPG + BBDB

(defun tinymail-gpg-recipient ()
  "Check BBDB field gnus-pgp for 'sign' and 'encrypt'."
  (when (and (eq major-mode 'message-mode)
	     (featurep 'bbdb))
    (when (and (boundp 'message-has-gpg)
	       (not message-has-gpg)
               (message-mail-p))
      (let* ((to-field      (mail-fetch-field "to"))
             (components    (mail-extract-address-components to-field t))
             recipient)
        (when (= (length components) 1)
          ;; Only a single recipient
          (setq recipient (nth 1 (car components)))
          (let ((record (bbdb-search-simple nil recipient))
		gpg)
            (when record
              (setq gpg (bbdb-get-field record 'gnus-gpg)))
            (when (> (length gpg) 0)
              (cond
               ((string= gpg "sign")
                (mml-secure-message-sign-pgpmime))
               ((string= gpg "encrypt")
                (mml-secure-message-encrypt-pgpmime))))))))))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-from-set-field (&optional from-field)
  "Check FROM-FIELD and set From: unless it has two spaces in front."
  (save-excursion
    (let ((from (or from-field
                    (ti::mail-get-field-1 "From")))
          str)
      (tinymail-debug fid 'initial-from from-field)
      (cond
       ;;  The field was there, if there in NO two spaces, replace
       ;;  the content with new dynamic value
       ;;
       ;;  If used puts two spaces at from, he want to modify
       ;;  the field himself
       ((and (stringp from)
             (string-match "^  " from-field))
        (tinymail-debug fid 'from-disabled-space))
       ((stringp (setq str (tinymail-from-field-value)))
        ;;  Will create if not exists.
        (mail-position-on-field "From")
        (ti::mail-kill-field "^From" str))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-process-1 (&optional force)
  "See `tinymail-process'. If FORCE is non-nil, run immediately.
This function should be called interactive only when debugging errors:
C-u M-x tinymail-process-1."
  (interactive "P")
  (let ((fid           "tinymail-process: ")
	(last-to       tinymail--last-to-field)
	(alias-alist   (tinymail-mail-aliases))
	to
	from
	hsize
	ohsize)
    (unless fid ;; No-op. XEmacs byte compiler silencer
      (setq fid nil))
    ;;  - If "To:" field content has two spaces at front, this is signal
    ;;    to stay away.
    ;;
    ;;  - If To address has remained the same, we do nothing.
    ;;    If we would aways go into expanding and killing the Cc,Fcc
    ;;    fields blindly, user would notice that while he was
    ;;    writing his message. Avoid that as much as posisble

    (setq to    (ti::remove-properties
                 (or (ti::mail-get-field-1 "To")
                     (ti::mail-get-field-1 "newsgroups")))
          from        (ti::remove-properties (ti::mail-get-field-1 "From"))
          hsize (ti::remove-properties (ti::mail-header-area-size))
          ohsize        (get 'tinymail--process-hook 'old-hsize))
    ;;  Record the values now so that they aren't calculated any more
    (put 'tinymail--process-hook 'from from)
    (put 'tinymail--process-hook 'to to)
    (put 'tinymail--process-hook 'new-hsize hsize)
    (tinymail-debug fid "TO"    to
                    "LAST-TO"   last-to
                    "POST-hook" tinymail--process-hook
                    "hsize"     hsize ohsize
                    "MODE"      major-mode)
    (cond
     ((or (not (stringp to))
          (tinymail-field-off-p nil to))
      nil)                              ;flag DISABLED
     ((and (not (string= to last-to))   ;not same as previously ?
           (not (ti::nil-p to)))
      (save-excursion
        (ti::mail-abbrev-expand-mail-aliases
         (point-min) (ti::mail-hmax) alias-alist))
      ;;  what was expanded
      (setq to (ti::mail-get-field "To"))))
    (if tinymail--from-field-enable-flag
        (tinymail-from-set-field from))
    ;; ............................................ header changed ...
    (tinymail-field-cc-kill-by-regexp)
    (when (not (eq hsize ohsize)) ;;  Handle dynamic save to folders
      (tinymail-field-fcc nil hsize)
      (tinymail-field-fcc 'gcc hsize)
      (tinymail-debug
       fid
       "Running post hook"      tinymail--process-hook
       "MODE"                   major-mode)
      (setq tinymail--last-to-field to) ;update
      (tinymail-debug fid "END" "MODE" major-mode))
    ;;  User's things now
    (run-hooks 'tinymail--process-hook)))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-process-run-p ()
  "Return t if `tinymail-process' is allowed to run."
  (and (get-buffer-window (current-buffer))
       ;;  If buffer is not displayed, do nothing.
       (not buffer-read-only)
       (ti::mail-mail-p)
       tinymail-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinymail-process (&optional force)
  "Expand mail aliases and inserts additional info.

optional FORCE argument causes running post hook now.

If you take advantage of the `tinymail--process-hook', please remember
following

- Your hook must run as fast as possible so that it won't disturb
  writing the text.
- You can peek contents of the precaculated values instead of reading
  then again in the buffer

        (get 'tinymail--process-hook 'to to)      ;; To field content
        (get 'tinymail--process-hook 'new-hsize)  ;; Header size now
        (get 'tinymail--process-hook 'old-hsize)  ;; old header size"
  (when (or force
            (and (tinymail-process-run-p)
                 ;; If this doesn't look like mail, don't bother
                 (ti::mail-mail-p)))
    (condition-case error
        (tinymail-process-1 force)
      (error
       (message "TinyMail: post-command error: %s
Spot the error by turning on Emacs debug and calling
(tinymail-process-1 'force) or C-u M-x tinymail-process-1"
                (prin1-to-string error))
       (ding)
       (sit-for 5)                      ;Make sure user notices this ;
       (message "TinyMail: (error watch) Please Check *Messages* buffer.")))))

;;}}}

(provide   'tinymail)
(run-hooks 'tinymail--load-hook)

;;; tinymail.el ends here
