;;; tinyirc.el --- IRC related utilities for Emacs

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    2003-2010 Jari Aalto
;; Keywords:        tools
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto

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
;;
;; Requirements:
;;
;; o    Only supports <freshmeat.net/projects/pastebot> services.
;; o    Perl must have been installed
;; o    External program pbotutil.pl must have been installed
;;      See `tinyirc--pastebot-program-url'.
;;
;; Put this file on your Emacs-Lisp `load-path', add following into
;; ~/.emacs startup file.
;;
;;      (require 'tinyirc)
;;      (global-set-key "\C-cps" 'tinyirc-pastebot-send-region)
;;      (global-set-key "\C-cpr" 'tinyirc-pastebot-receive-url)
;;
;;      ** NOTE: Read "Pastebot Preliminary settings" before using
;;      ** NOTE: Win32 NTEmacs users, read "Pastebot Win32 notes"
;;
;; Or prefer autoload: your emacs loads this package only when you need it.
;;
;;      (autoload 'tinyirc-pastebot-send-region "tinyirc" "" t)
;;      (autoload 'tinyirc-pastebot-receive-url "tinyirc" "" t)
;;
;;      (global-set-key "\C-cps" 'tinyirc-pastebot-send-region)
;;      (global-set-key "\C-cpr" 'tinyirc-pastebot-receive-url)
;;
;; Following commads install default setup and the needed Perl program
;;
;;      M-x load-library RET tinyirc.el RET
;;      M-x tinyirc-pastebot-install-perl-util-pastebot RET
;;      M-x tinyirc-pastebot-install-example-servers RET
;;
;; If you have any questions, have suggestions or bug reports send mail
;; to maintainer.

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Aug 2003
;;
;;      IRC is very poplar method of getting together with all sorts of
;;      activities. For programmers, IRC is like 'all united' where
;;      you get invaluable help from people that happen to be online. No need
;;      to scratch your head alone; let's scratch together in a friendly
;;      programming channel.
;;
;;      Most of the channels do not permit flooding - that is -
;;      copy/pasting many lines (of code) at once. If person does that, the
;;      (ro)bot watching the activities of the channel will kick person out
;;      of the channel faster than he can blink his eye. So don't try
;;      pasting long material to the channel. Usually the channel's topic,
;;      which is displayed on entering the channel, includes the etiquette
;;      how to present your problem to the audiance. in previous times it
;;      has been a custom to use separate #flood channel (which you must
;;      join):
;;
;;          /join #flood
;;
;;      and then announce to people "Hey, I've posted the code to #flood,
;;      go and check". But someone may not be watching the channel's
;;      messages at the time of announcement and when he finally joins the
;;      #flood, he's too late. He cannot see the code. The catch is that
;;      every interested person has be be in the channel #flood *first*
;;      before anyone pastes a message there. Participants cannot see old
;;      messages but only the lines after his joined to the #flood channel.
;;
;;      An then someone came with a nifty idea: use PasteBot services for
;;      permanet storage (with line numbers). It would be nice if the
;;      the PasteBot messages could be managed directly from Emacs with
;;      couple of key bindings. So, this package was born. The basic idea
;;      to exchange information (like examples, bug reports) is:
;;
;;          You  => send message  =>    +------------------------+
;;               <= message URL         | PasteBot server        |
;;                                      | stores the message and |
;;                                      | assigns a unique ID    |
;;                                      | to it. It returns the  |
;;                                      | storage URL back       |
;;                                      +------------------------+
;;
;;      Now, you publish the URL (to anyone interested), e.g in a IRC
;;      channel. Interested people can go and check it, at any time:
;;
;;          Rush of Crowd => See it!    +------------------------+
;;                                      | PasteBot displays the  |
;;                                      | stored message         |
;;                                      |                        |
;;                                      | It won't fade away,    |
;;                                      | like text in IRC       |
;;                                      |                        |
;;                                      | SEMI-PERMANENT STORAGE |
;;                                      +------------------------+
;;
;;      _NOTE;_ Although the PasteBot services are mostly used in
;;      IRC channels to exchange information, they can also be used
;;      to exchange e.g. debug information with any parties involved.
;;
;;  Description
;;
;;      o   Send text region to PasteBot servers.
;;      o   Receive messages from PasteBot servers using URL or
;;          message ID.
;;
;;  Sending Pastebot messages
;;
;;      There is simple interface: draw region and call `M-x'
;;      `tinyirc-pastebot-send-region', which you should assign to a key
;;      for easier access. But you can't use that quite yet, because you
;;      have to configure your environment first. Read "preliminary
;;      settings" topic and test your interface before calling that function.
;;
;;          ;; If this is occupied, select other free key
;;          (global-set-key "\C-cps" 'tinyirc-pastebot-send-region)
;;
;;      The response from the send command is recorded into separate buffer
;;      *IRC* *pastebot* *sent* and the lines in the buffer look like:
;;
;;          2003-08-07 16:06 test foo <URL> <MESSAGE>
;;          2003-08-07 17:21 test foo <URL> <MESSAGE>
;;                           |    |   |     |
;;                           |    |   |     Summary line (Subject/Errors)
;;                           |    |   Where message can be read
;;                           |    Your id used for sending the message
;;                           The service (channel) where message was sent
;;
;;      For adavanced users: few variables are available in case you make
;;      changes to the perl script. See `tinyirc--pastebot-program' and
;;      `tinyirc--pastebot-config-directory'.
;;
;;  Sending to channels that are not supported
;;
;;      The pastebot servers do not support all IRC channels per se.
;;      E.g. they mey limit posts to #perl, #sendmail etc. In case you
;;      participate in other channels, you can still use the pastebot
;;      service. Just use some free channel like "flood" or "test" for
;;      all your posts. Announce the returned URL to the channel
;;      your're being joined in. Use IRC command `/me' to submit your
;;      IRC message:
;;
;;           /me [pastebot] <MESSAGE> htpp//....
;;
;;  Receiving pastebot messages
;;
;;      Receiving messages announced in the IRC channel is even easier
;;      than sending (less typing into prompts). Call `M-x'
;;      `tinyirc-pastebot-receive-url' and copy the URL announced in the
;;      channel. A possible key binding for this could be:
;;
;;          ;; If this is occupied, select a any free key
;;          (global-set-key "\C-cpr" 'tinyirc-pastebot-receive-url)
;;
;;      The receive buffer output looks something like this when used
;;      couple of times. Notice the added line numbers (001:) in the
;;      first message, which can be toggled on or off with `mouse-3'.
;;
;;          2003-08-07 18:04 http://dragon.cbi.tamucc.edu:8080/72
;;          001: test message
;;          002: another line
;;          003: more lines
;;          004: and more
;;          2003-08-07 18:14 http://dragon.cbi.tamucc.edu:8080/72
;;          warning: error fetching http://dragon.cbi.tamucc.edu:8080/72: 500 Can't connect to dragon.cbi.tamucc.edu:8080 (connect: timeout)
;;          2003-08-07 18:32 http://dragon.cbi.tamucc.edu:8080/74
;;          #/!usr/bin/perl
;;          use strict;
;;          use English;
;;          ...
;;          2003-08-08 15:29 http://dragon.cbi.tamucc.edu:8080/74
;;          #/!usr/bin/perl
;;          use strict;
;;          use English;
;;          ...
;;
;;  Pastebot mode (PBot)
;;
;;      The buffers *pastebot* *received* and *pastebot*
;;      *sent* are put into `tinyirc-pastebot-mode', whose mode name is
;;      derived from variable `tinyirc--mode-name'. Within these buffer it
;;      is possible to receive or view the received messages easily.
;;      Following default key bindings are set, when
;;      `tinyirc--pastebot-mode-define-keys-hook' is run if
;;      `tinyirc-pastebot-mode-map' is nil when mode is turned on for the
;;      first time:
;;
;;          mouse-3         tinyirc-pastebot-mode-command-receive
;;          Control mouse-3 tinyirc-pastebot-mode-command-line-number-toggle
;;          C-p             tinyirc-pastebot-message-timestamp-backward
;;          C-n             tinyirc-pastebot-message-timestamp-forward
;;          C-c C-r         tinyirc-pastebot-mode-command-receive
;;          C-c C-w         tinyirc-pastebot-mode-command-write-file
;;          C-c C-l         tinyirc-pastebot-mode-command-line-numbers-toggle
;;
;;      The line number toggle function is handy when checking at references
;;      "see line NN", or "Try what variable that code prints at line NN".
;;      The mode calls hook `tinyirc--pastebot-mode-hook' where user
;;      can add more customizations.
;;
;;  Pastebot preliminary settings
;;
;;      You can use the PasteBot services directly from their web
;;      pages. There should be a form to where to submit a message.
;;      Couple of services at the time of writing existed:
;;
;;          http://www.pastebot.net/
;;          http://sial.org/pbot/
;;          http://pastebin.ca/
;;          http://paste.lisp.org/
;;          http://nopaste.snit.ch/
;;          http://channels.debian.net/paste/
;;
;;      At the page, there is a button [channel] which defines what
;;      channels support pastebot announcements. In order to use the
;;      service from Emacs, you need a command line program that
;;      handles the send request. The Perl client `pbotutil' can be
;;      found from address `tinyirc--pastebot-program-url'. A
;;      configuration file must include the details about available
;;      pastebot servers and their supported channels. _Note:_ There
;;      is command `M-x' `tinyirc-pastebot-install-example-servers'
;;      which writes the example file.
;;
;;      Use command line prompt first to test that the interface works
;;      by sending message simple file to *none* service, which
;;      can be used for testing purposes:
;;
;;          $ pbotutil.pl -s none -u $USER -m test put test.txt
;;                        |
;;                        Select a service from configuration file
;;
;;          $ pbotutil.pl get <URL>
;;
;;      If everything worked ok, you should see program print an URL where
;;      the message was stored. Visit the page to verify that message is
;;      available. When these preliminary tests indicate that the interface
;;      works, you're ready to use the Emacs interface.
;;
;;     Making your PasteBot send log available
;;
;;      If you can run a web server and want to publish your posts, you
;;      could configure it to show ´tinyirc--pastebot-buffer-file-name' all
;;      the time (it's nil by default). For Apache the needed line in
;;      /etc/apache/httpd.conf would look something line:
;;
;;          Alias /pastebot-log /home/you/tmp/pastebot
;;
;;      and Emacs setting:
;;
;;          ;;  Browsers can view ".txt" files directly.
;;          (setq tinyirc--pastebot-buffer-file-name-sent
;;                "~/tmp/pastebot/pastebot.txt")
;;
;;      After these (and `apachectl' `restart') all your posts could be
;;      recalled by looking at the list from address:
;;
;;          http://www.example.com/pastebot-log
;;
;;      If you want more challenges, it is possible to start up your own
;;      PasteBot service. For more information see "Related software" later.
;;
;;  Pastebot Win32 notes
;;
;;     NTEmacs and Cygwin Emacs - two different choices
;;
;;      Emacs and XEmacs have two release flavors under Windows platform.
;;      For GNU Emacs, you can download and run "Native Win32 NTEmacs" or
;;      use "Cygwin Win32 Emacs" which is included in Cygwin available
;;      at <http://www.cygwin.com>. Usually these two are simply referred
;;      to as `NTEmacs' and `Cygwin' `Emacs'. Most of the Win32 users use
;;      the NTEmacs version. People from Unix/Linux background usually
;;      prefer the Cygwin Emacs, becaue it's more like the "real thing" and
;;      integrates better to features that are not available to NTEmacs.
;;      Usually things work better under Cygwin Emacs than NTEmacs,
;;      because real Emacs supposes many of the Unix utilities to be
;;      around. In Cygwin there are `diff' program, `find' program etc.
;;
;;     NTEmacs
;;
;;        There is command `M-x'
;;        `tinyirc-pastebot-install-example-servers' which you can try. If
;;        it fails, follow these steps described below.
;;
;;      If you use NTEmcs, read this section carefully. The client
;;      *pbotutil.pl* is a _perl_ program, so Perl must be installed.
;;      See Native Win32 Perl at <http://www.activestate.com> or
;;      better, install Cygwin, which includes all. Client
;;      *pbotutil.pl* is an application which expects to see
;;      configuration directory named `$HOME/.pbotutil'. Yes, the directory
;;      indeed starts with a dot.
;;
;;      Windows does not define environment variable named `HOME', so
;;      it must be created to system registry. In W2k you create the
;;      variable from Start => Setings => Control Panel => System icon
;;      Advanced [tab] + Environment variable [button] => System variable.
;;      The `HOME' is location that is considered to be the work directory.
;;      If you have never heard of `$HOME' (the "variable"), refer to
;;      NTEmacs FAQ at
;;      <http://www.gnu.org/software/emacs/windows/ntemacs.html>. At
;;      this page, see link pointing to *Installing* *Emacs*. There
;;      you can find more information on setting the `$HOME' variable.
;;
;;      Next, create the dot-directory `$HOME/.pbotutil' which
;;      _cannot_ be made using the standard Windows file manager (explorer).
;;      It is not possible to request from menu *File* => *New* =>
;;      *Folder* with a name that starts with a dot. That's a Windows
;;      bug. But it is possible to create dot-directories directly from Emacs.
;;      Start *dired* and point it to your $HOME:
;;
;;          C-x d $HOME         or this is the same:  C-x d ~
;;
;;      From dired buffer, press command `+' to create directory *.pbotutil*
;;      and it should soon appear in the *dired* buffer. You're now ready
;;      to use perl script *pbotutil.pl* (See above "Preliminary setup")
;;      which searches its configuration file from that location.
;;
;;     Cygwin symlink notes (= Windows shotrcuts)
;;
;;      _NOTE:_ Never edit any file which is a Windows shortcut or a Cygwin
;;      symbolic link under NTEmacs. NTEmcs (as of writing; 21.3) cannot by
;;      default follow any windows shortcuts or Cygwin's symbolic links.
;;      Just use Cygwin Emacs for Cygwin's symbolic link files.
;;
;;      There exist packages for NTEmacs to help it to understand links,
;;      but those packages are recommended only for advanced Emacs users.
;;      If you're interested, contact these people for additional packages:
;;
;;          w32-symlinks    by F.J.Wright@qmul.ac.uk
;;          follow-lnk.el   by christoph.conrad@gmx.de
;;
;;  Related software
;;
;;      o   Perl client: http://bboett.free.fr/webPaste.html
;;      o   The server software is available at
;;          <http://freshmeat.net/projects/pastebot/>

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(eval-when-compile
  (require 'cl))

(eval-and-compile
  ;; predeclare - Byte compiler silencer.
  (defvar font-lock-keywords))

;;}}}
;;{{{ setup: hooks

;;; ..................................................... &v-variables ...

(defcustom tinyirc--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-hook-sent
  '(tinyirc-pastebot-font-lock-mode-sent)
  "*Hook that is run at the end of `tinyirc-pastebot-message-record'"
  :type  'hook
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-hook-received
  '(tinyirc-pastebot-font-lock-mode-received)
  "*Hook that is run at the end of `tinyirc-pastebot-receive-message'."
  :type  'hook
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-mode-hook nil
  "*Hook run after the `tinydesk-receive-mode' is turned on.
This happend when message log is written either to buffer
`tinyirc--pastebot-buffer-name-received' or
`tinyirc--pastebot-buffer-name-sent'."
  :type  'hook
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-mode-define-keys-hook
  '(tinyirc-pastebot-default-mode-bindings)
  "*Hook run only if `tinyirc-pastebot-mode-map' is nil. This is checked at
package load and when `tinyirc-pastebot-mode' is called."
  :type  'hook
  :group 'TinyIrc)

;;}}}
;;{{{ setup: User variables

(defcustom tinyirc--pastebot-program nil
  "*Perl program to send messages to PasteBot servers."
  :type  'filename
  :group 'TinyIrc)

;; Try to configure the variable
(unless tinyirc--pastebot-program
  (setq tinyirc--pastebot-program
        (let* ((name "pbotutil.pl")
               (bin  (executable-find name)))
          (if bin
              bin
            (message
             (concat
              "TinyIrc: [ERROR] Please configure "
              "`tinyirc--pastebot-program '. Not in PATH `%s'")
             name)
            nil))))

(defcustom tinyirc--pastebot-send-file
  (let ((file "pastebot-submit.txt")
        dir)
    (dolist (d '("~/tmp/"
                 "~/"
                 "c:/"))
      (when (file-directory-p d)
        (setq dir d)
        (return)))
    (unless dir
      (error (concat "TinyIrc: Can't find suitable directory. "
                     "Set `tinyirc--pastebot-send-file'.")))
    (format "%s%s" dir file))
  "*Perl program to send messages to PasteBot servers."
  :type  'filename
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-config-directory
  (let ((dir "~/.pbotutil"))
    (unless (file-directory-p dir)
      (message
       (concat
        "TinyIrc: [ERROR] Please configure "
        "`tinyirc--pastebot-config-directory'. No directory `%s'")
       dir))
    dir)
  "*Configuration directory of `tinyirc--pastebot-program'. If you change
this variable, you need to change the Perl program itseld too."
  :type  'directory
  :group 'TinyIrc)

;; "~/tmp/pastebot.txt"
(defcustom tinyirc--pastebot-buffer-file-name-sent nil
  "Name of file buffer where the results are saved after each send. If nil,
no file is saved. Refer to manual \\[finder-commentary] `tinyirc' for more
information"
  :type  'filename
  :group 'TinyIrc)

(defcustom tinyirc--pastebot-font-lock-keywords-sent
  (list
   ;; Service name used to send the message
   (list
    "^.*:[0-9][0-9][ \t]+\\([^ \t]+\\)"
    1 'font-lock-reference-face)
   ;; The Message
   (list
    "http:/[^ \t]+[ \t]+\\(.+\\)"
    1 'font-lock-type-face))
  "*Font lock keywords."
  :type   'sexp
  :group  'TinyIrc)

(defcustom tinyirc--pastebot-font-lock-keywords-received
  (list
   ;; The id line (receive time)
   (list
    "^[0-9][0-9].*:[0-9][0-9].*http://.*"
    0 'font-lock-reference-face)
   ;; Errors
   (list
    "Can't connect"
    0 'font-lock-type-face))
  "*Font lock keywords."
  :type   'sexp
  :group  'TinyIrc)

(defcustom tinyirc--mode-name "PBot"
  "*Name of major mode `tinyirc-pastebot-mode'."
  :type  'string
  :group 'TinyIrc)

;;}}}
;;{{{ setup: private variables

;;; ....................................................... &v-private ...
;;; Private variables

(defvar tinyirc--pastebot-program-url
  "http://sial.org/code/perl/scripts/pbotutil.pl"
  "Download location of the pastebot perl interface.
See also manual <http://sial.org/code/perl/scripts/pbotutil.pl.html>.")

(defvar tinyirc--pastebot-message-format-function
  'tinyirc-pastebot-message-format
  "Function to format the message arguments: SERVICE USER MSG and URL.")

(defvar tinyirc--pastebot-buffer-name-sent "*pastebot sent*"
  "Log buffer of sent pastebot messages. If nil, no log is recorded.")

(defvar tinyirc--pastebot-buffer-name-received "*pastebot received*"
  "log buffer of received pastebot messages.")

(defvar tinyirc--pastebot-history-user nil
  "History of used pastebot user names.")

(defvar tinyirc--pastebot-history-services nil
  "History of used pastebot services.")

(defvar tinyirc--pastebot-service-list nil
  "List of available services according to `servers' file.
If this variable is not set, it is populated from
`tinyirc--pastebot-config-directory' and file `servers'.

The content of the `servers' file is read only once, so if it
modified, function `tinyirc-pastebot-service-list-set'.")

(defvar tinyirc--pastebot-service-list-time-stamp nil
  "Time of reading from `tinyirc--pastebot-config-directory'.")

(defvar tinyirc-pastebot-mode-map nil
  "Local keymap for STATE files loaded by edit.")

(defvar tinyirc--error-buffer "*TinyIrc error*"
  "Error buffer.")

(defvar tinyirc--http-buffer "*TinyIrc http*"
  "Error buffer.")

(defvar tinyirc--pastebot-config-default-content
  (format "\
# %s configuration file for SERVERS
# for program %s

# irc.freenode.net
name debian
url http://oaste.debian.net/
channel #debian

# irc.freenode.net
name flood
url http://oaste.debian.net/
channel #flood

# irc.freenode.net
name perl
url http://nopaste.snit.ch/
channel #perl

# irc.freenode.net (backup)
name perl2
url http://nopaste.snit.ch/
channel #perl

# Services for channels that do not have particular support for
# PasteBot. Simply announce the url in the #channel with command:
#
#    /me [pastebot] <URL>

name none
url http://nopaste.snit.ch/
channel #none

# FIXME; Interface problems?
# name pastebot
# url http://www.pastebot.net/
# channel #none

# FIXME; Not a pastebot server
# name pastebin-ca
# url http://pastebin.ca/
# channel ''

# End of file
"
        tinyirc--pastebot-config-directory
        (or tinyirc--pastebot-program
	    "pbotutil.pl"))
  "Default content for `tinyirc-pastebot-install-example-servers'.
See also `tinyirc--pastebot-config-directory'.")

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ General functions

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-pastebot-program-name ()
  "Verify that `tinyirc--pastebot-program' is string."
  (if (stringp tinyirc--pastebot-program)
      tinyirc--pastebot-program
    (error
     "TinyIrc: [ERROR] `tinyirc--pastebot-program' not defined.")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-time-string ()
  "Return ISO 8601 time YYYY-MM-DD HH:MM."
  (format-time-string "%Y-%m-%d %H:%M"))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-word-at-point ()
  "Return word separated by whitespace."
  (save-excursion
    (unless (string-match
             "[ \t\r\n]" (char-to-string (following-char)))
      (skip-chars-backward "^ \t\r\n")
      (let ((point (point)))
        (skip-chars-forward "^ \t\r\n")
        (buffer-substring point (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-append-to-buffer (string)
  "Add STRING to the end of current buffer."
  ;;  Make room for new message if point is ar wrong place.
  (goto-char (point-max))
  (beginning-of-line)
  (unless (or (eobp)
              (looking-at "^[\r\n]"))
    (open-line 1)
    (forward-line 1))
  (insert string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-line-number-p ()
  "Return non-nil if line contains a line number.
Match 1 contains line numer, 2 contains rest of the line."
  (save-excursion
    (beginning-of-line)
    (looking-at "^\\([0-9][0-9][0-9]: \\)\\(.*\\)")))

;;; ----------------------------------------------------------------------
;;;
(defun tinyird-line-number-add-region (beg end)
  "Add line numbers to region BEG END. Point is moved."
  (let ((i 1))
    (goto-char beg)
    (beginning-of-line)
    (catch 'stop
      (while (and (not (eobp))
                  (< (point) end))
        ;;  Abort if there is already a line number
        (when (tinyirc-line-number-p)
          (throw 'stop 'abort))
        (insert (format "%03d: " i))
        (forward-line 1)
        (incf i)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyird-line-number-delete-region (beg end)
  "Delete line numbers to region BEG END. Point is moved."
  (goto-char beg)
  (beginning-of-line)
  (let (line)
    (while (and (not (eobp))
                (< (point) end))
      (when (tinyirc-line-number-p)
        (setq line (match-string 2))
        (delete-region (line-beginning-position)
                       (line-end-position))
        (insert line))
      (forward-line 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-path (path)
  "Return path using forward slashes and without using trailing slash."
  (setq path (file-name-as-directory
              (replace-regexp-in-string "[\\]" "/" path)))
  (if (string-match "^.+[^\\/]" path)
      (match-string 0 path)
    path))

;;}}}
;;{{{ Pastebot: Library

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-font-lock-mode-select (mode &optional off)
  "MODE is 'sent or 'received. Turn on or OFF font lock."
  (let ((kwds (if (eq mode 'sent)
                  tinyirc--pastebot-font-lock-keywords-sent
                tinyirc--pastebot-font-lock-keywords-received)))
    (cond
     (off
      (setq font-lock-keywords nil)
      (font-lock-mode -11))
     (t
      (font-lock-mode 1)
      (setq font-lock-keywords kwds)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-font-lock-mode-sent (&optional off)
  "Turn on or OFF font lock."
  (tinyirc-pastebot-font-lock-mode-select 'sent off))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-font-lock-mode-received (&optional off)
  "Turn on or OFF font lock."
  (tinyirc-pastebot-font-lock-mode-select 'received off))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-message-record (msg)
  "Record sent MSG to `tinyirc--pastebot-buffer-name-sent'.
Buffer is saved if `tinyirc--pastebot-buffer-file-name' is set.

References:
  `tinyirc--pastebot-hook-sent'."
  (let* ((buffer    tinyirc--pastebot-buffer-name-sent)
         (save-file tinyirc--pastebot-buffer-file-name-sent)
         (save-dir  (and save-file
                         (file-name-directory save-file))))
    (when buffer
      (get-buffer-create buffer)
      (display-buffer buffer)
      (with-current-buffer buffer
        (when (and (not buffer-file-name)
                   save-file)
          (if (file-directory-p save-dir)
              (setq buffer-file-name save-file)
            (message
             (concat "TinyIrc: [ERROR] "
                     "tinyirc--pastebot-buffer-file-name-sent' "
                     "no such dir ``%s'")
             save-file)))
        ;;  Make room for new message if point is ar wrong place.
        (tinyirc-append-to-buffer msg)
;;;     (shrink-window-if-larger-than-buffer
;;;      (get-buffer-window buffer))
        (when buffer-file-name
          (save-buffer))
        (run-hooks 'tinyirc--pastebot-hook-sent)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-message-format (service user msg url)
  "Format message using SERVICE USER MSG URL with timestamp."
  (let ((time (tinyirc-time-string))
	(eol  (if (string-match "\n$" msg)
		  ""
		"\n")))
    (format "%s %s %s %s %s%s" time service user url msg eol)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-program-1 ()
  "Return location of `tinyirc--pastebot-program'."
  (let*  ((prg        (tinyirc-pastebot-program-name))
          (saved-abs  (get 'tinyirc-pastebot-program 'absolute))
          (saved-orig (get 'tinyirc-pastebot-program 'original))
          (path       (cond
		       ((and saved-abs
			     (file-exists-p saved-abs)
			     ;;  Program has not changed since
			     (string= prg saved-orig))
			;;  `executable-find' is heavy; use cached file.
			saved-abs)
		       ((and prg
			     (file-exists-p prg))
			(expand-file-name prg))
		       (t
			(executable-find prg)))))
    (put 'tinyirc-pastebot-program 'original prg)
    (put 'tinyirc-pastebot-program 'absolute path)
    path))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-program ()
  "Return location of `tinyirc--pastebot-program' or signal an error."
  (let ((path (tinyirc-pastebot-program-1)))
    (unless path
      (error
       (format
        (concat "TinyIrc: `tinyirc--pastebot-program' %s not found in PATH. "
                "Download it at %s")
        tinyirc--pastebot-program
        tinyirc--pastebot-program-url)))
    (unless (file-exists-p path)
      (error "TinyIrc: `tinyirc--pastebot-program' %s does not exist."
             tinyirc--pastebot-program))
    path))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-service-file-name ()
  "Return configuration filename."
  (let* ((dir  tinyirc--pastebot-config-directory)
         (file  (concat (file-name-as-directory dir)
                        "servers")))
    (unless (file-directory-p dir)
      (error "Cannot read `tinyirc--pastebot-config-directory' %s"
             tinyirc--pastebot-config-directory))
    file))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-service-file-name-changed-p ()
  "Check if configuration file has chnages since last reading."
  (let ((time tinyirc--pastebot-service-list-time-stamp))
    (when time
      (let* ((file    (tinyirc-pastebot-service-file-name))
             (modtime (format-time-string
                       "%Y-%m-%d %H:%M"
                       (nth 5 (file-attributes file)))))
        (string< time modtime)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-service-list-from-file ()
  "Read `tinyirc--pastebot-config-directory' and parse `servers' file."
  (let ((file (tinyirc-pastebot-service-file-name))
	list)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]*name[ \t]+\\([^ \t\r\n\f]+\\)"
              nil t)
        (push (match-string 1) list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-service-list-set ()
  "Set `tinyirc--pastebot-service-list' from file.
See `tinyirc--pastebot-config-directory'."
  (setq tinyirc--pastebot-service-list-time-stamp
        (format-time-string "%Y-%m-%d %H:%M")
        tinyirc--pastebot-service-list
        (tinyirc-pastebot-service-list-from-file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-service-list ()
  "Return `tinyirc--pastebot-service-list' or read configuration."
  (if (tinyirc-pastebot-service-file-name-changed-p)
      ;;  Need update. User has chnages it on disk while we had read
      ;;  and cached it earlier.
      (tinyirc-pastebot-service-list-set)
    (or tinyirc--pastebot-service-list
        (tinyirc-pastebot-service-list-set))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-receive-call-process-id (service id)
  "Receive message from pastebot SERVICE by ID number. Return content.
Valid SERVICE is one that is defined in dire$ctory
`tinyirc--pastebot-config-directory' and file `servers'."
  (let ((prg (tinyirc-pastebot-program)))
    (if (integerp id)
        (setq id (number-to-string id)))
    (with-temp-buffer
      (message "TinyIrc: pastebot receiving ID %s from %s ..." id service)
      (call-process "perl"
                    nil
                    (current-buffer)
                    nil
                    prg
                    "-s"
                    service
                    "get"
                    id)
      (message "TinyIrc: pastebot receiving ID %s from %s ...done." id service)
      ;;  Drop trailing newline from URL.
      (buffer-string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-receive-call-process-url (url)
  "Receive message from pastebot by URL . Return content."
  (let ((prg (tinyirc-pastebot-program)))
    (with-temp-buffer
      (message "TinyIrc: pastebot receiving URL %s ..." url)
      (call-process "perl"
                    nil
                    (current-buffer)
                    nil
                    prg
                    "get"
                    url)
      (message "TinyIrc: pastebot receiving URL %s ...done." url)
      ;;  Drop trailing newline from URL.
      (buffer-string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-send-call-process (file service user msg)
  "Call `tinyirc--pastebot-program' with perl and send argumens.
See program for definition of FILE SERVICE USER MSG.

Return program's message without trailing newline. If command succeed,
the return value is URL where message is available. In case of error, the
return value is program's error message."
  (let ((prg (tinyirc-pastebot-program)))
    (with-temp-buffer
      (setq file (expand-file-name file))
      (message "TinyIrc: pastebot sending %s ..." file)
      (call-process "perl"
                    nil
                    (current-buffer)
                    nil
                    prg
                    "-s"
                    service
                    "-u"
                    user
                    "-m"
                    msg
                    "put"
                    file)
      (message "TinyIrc: pastebot sending %s ...done." file)
      ;;  Drop trailing newline from URL.
      (buffer-substring (point-min)
                        (max (point-min)
                             (1- (point-max)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-send-main (file service &optional msg user)
  "Send FILE to SERVICE using optional MSG and USER.
USER defaults to variable `user-login-name', environment variable USER
or string `anon'."
  (let ((function tinyirc--pastebot-message-format-function)
	url)
    (or user
        (setq user (or (and (boundp 'user-login-name) ;; Emacs only variable
                            (symbol-value 'user-login-name))
                       (getenv "USER")
                       "anon")))
    (or msg
        (setq msg "No message"))
    (setq url (tinyirc-pastebot-send-call-process
               file service user msg))
    (setq msg
          (if function
              (funcall function service user msg url)
            (tinyirc-pastebot-message-format service user msg url)))
    (tinyirc-pastebot-message-record msg)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-receive-message (url msg)
  "Write URL's MSG to `tinyirc--pastebot-buffer-name-received'."
  (let ((time   (tinyirc-time-string))
	(buffer (get-buffer-create
		 tinyirc--pastebot-buffer-name-received)))
    ;;  Make sure there is final newline
    (unless (string-match "\n$" msg)
      (setq msg (concat msg "\n")))
    (display-buffer buffer)
    (with-current-buffer buffer
      (tinyirc-append-to-buffer
       (format "%s %s\n%s" time url msg))
      (run-hooks 'tinyirc--pastebot-hook-received))))

;;}}}
;;{{{ Pastebot: Mode

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-pastebot-message-timestamp-regexp ()
  "Return timestamp regexp."
  "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9] +[0-9][0-9]:[0-9][0-9] +")

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-pastebot-message-timestamp-p ()
  "Return t if line contains a timestamp."
  (string-match
   (tinyirc-pastebot-message-timestamp-regexp)
   (buffer-substring (line-beginning-position)
                     (line-end-position))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-pastebot-message-timestamp-backward ()
  "Move to previous timestamp.  Return nin-nil if moved.
Point is after timestamp."
  (re-search-backward
   (tinyirc-pastebot-message-timestamp-regexp) nil t))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-pastebot-message-timestamp-forward ()
  "Move to previous timestamp.  Return nin-nil if moved.
Point is at the beginning of line."
  (re-search-forward
   (tinyirc-pastebot-message-timestamp-regexp) nil t))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-message-timestamp-move-to-url ()
  "At timestap line, go to URL at line. Return non-nl if moved."
  (let (point)
    (save-excursion
      (beginning-of-line)
      (when (looking-at (tinyirc-pastebot-message-timestamp-regexp))
        (setq point (match-end 0))))
    (when point
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-url-at-point ()
  "Return HTTP url at point if any."
  (let ((word (tinyirc-word-at-point)))
    (when (and word
               (string-match "http://" word))
      word)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-message-region ()
  "Determine retrieved message's region. Return list '(beg end).
The region searched starts with a time stamp and ends in another timestamp
or `eobp'."
  (let (point
        ok)
    (save-excursion
      (cond
       ((tinyirc-pastebot-message-timestamp-p)
        (forward-line 1)
        (setq ok t))
       ((tinyirc-pastebot-message-timestamp-forward)
        (setq ok t)))
      (when (and ok
                 (not (or (tinyirc-pastebot-message-timestamp-p)
                          (eobp))))
        (setq point (point))
        (cond
         ((tinyirc-pastebot-message-timestamp-forward)
          (beginning-of-line))
         (t
          (goto-char (point-max))))
        (list point (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-message-string ()
  "Return received message at point."
  (multiple-value-bind (beg end)
      (tinyirc-pastebot-message-region)
    (when (and beg end)
      (buffer-substring beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-mode-command-write-file (beg end file)
  "Write message at BEG END to a FILE."
  (interactive
   (let (file)
     (multiple-value-bind (beg end)
         (tinyirc-pastebot-message-region)
       (unless beg
         (error "TinyIrc: Can't find timestamp at point %d"
                (point)))
       (setq file
             (read-file-name "Save message to file: "))
       (unless (string-match "[^ \t\r\n\f]" file)
         (setq file "--abort-this"))
       (list
        beg
        end
        file))))
  (when (and file
             (not (string-match "--abort-this" file)))
    (write-region beg end file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-mode-command-receive (url &optional arg)
  "Receive messages.
In buffer tinyirc--pastebot-buffer-name-sent', receive
message using current line's URL. With Prefix argument, receive
arbitrary user supplied URL.

In buffer `tinyirc--pastebot-buffer-name-received' this function
automatically asks what URL to receive."
  (interactive
   (let (url)
     (cond
      ((string= (buffer-name)
                tinyirc--pastebot-buffer-name-sent)
       (cond
        (current-prefix-arg
         (setq url
               (read-string "Pastebot receive URL: "
                            (thing-at-point 'url))))
        (t
         (save-excursion
           (when (tinyirc-pastebot-message-timestamp-move-to-url)
             (setq url (thing-at-point 'url))
             (when (and url
                        (not (y-or-n-p
                              (format
                               "Receive %s "
                               url))))
               (setq url nil)))))))
      ((string= (buffer-name)
                tinyirc--pastebot-buffer-name-received)
       (setq url
             (read-string "Pastebot receive URL: "
                          (tinyirc-pastebot-url-at-point)))))
     (list url
           current-prefix-arg)))
  (when url
    (tinyirc-pastebot-receive-url url)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-mode-command-line-number-toggle ()
  "Add or remove line numbers to the message at point (or forward)."
  (interactive)
  (if (not (tinyirc-pastebot-message-timestamp-p))
      (message "TinyIrc: Move to a timestamp first.")
    (multiple-value-bind (beg end)
        (tinyirc-pastebot-message-region)
      (if (not (and beg end))
          (message "TinyIrc:  Cannot find message's region.")
        (let ((number-p (save-excursion
			  (goto-char beg)
			  (tinyirc-line-number-p))))
          (save-excursion
            (if number-p
                (tinyird-line-number-delete-region beg end)
              (tinyird-line-number-add-region beg end))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-pastebot-default-mode-bindings ()
  "Define default key bindings to `tinyirc-pastebot-mode-map'."

  (cond
   ((string-match "XEmacs" (emacs-version))
    (define-key tinyirc-pastebot-mode-map [(mouse3up)]
      'tinyirc-pastebot-mode-command-receive)
    (define-key tinyirc-pastebot-mode-map [(control mouse3up)]
      'tinyirc-pastebot-mode-command-line-number-toggle))
   (t ;; Emacs
    (define-key tinyirc-pastebot-mode-map [(mouse-3)]
      'tinyirc-pastebot-mode-command-receive)
    (define-key tinyirc-pastebot-mode-map [(control mouse-3)]
      'tinyirc-pastebot-mode-command-line-number-toggle)))

  (define-key tinyirc-pastebot-mode-map "\C-p"
    'tinyirc-pastebot-message-timestamp-backward)

  (define-key tinyirc-pastebot-mode-map "\C-n"
    'tinyirc-pastebot-message-timestamp-forward)

  (define-key tinyirc-pastebot-mode-map "\C-c\C-r"
    'tinyirc-pastebot-mode-command-receive)

  (define-key tinyirc-pastebot-mode-map "\C-c\C-w"
    'tinyirc-pastebot-mode-command-write-file)

  (define-key tinyirc-pastebot-mode-map "\C-c\C-l"
    'tinyirc-pastebot-mode-command-line-numbers-toggle))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-mode-map-activate ()
  "Use local \\{tinyirc-pastebot-mode-map} on this buffer."
  (use-local-map tinyirc-pastebot-mode-map))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinyirc-mode-map-define-keys ()
  "Run `tinyirc--pastebot-mode-define-keys-hook'.
But only if `tinyirc-pastebot-mode-map' is nil."
  (unless tinyirc-pastebot-mode-map
    (setq tinyirc-pastebot-mode-map (make-sparse-keymap))
    (run-hooks 'tinyirc--pastebot-mode-define-keys-hook)))

;;; ----------------------------------------------------------------------
;;;
;;;###autolaod
(defun tinyirc-pastebot-mode ()
  "Major mode for handlling PasteBot server messages: sending, receiving and
formatting. For more information run \\[finder-commentary] RET tinyirc.el
RET.

Mode description:

\\{tinyirc-pastebot-mode-map}"
  (interactive)
  (tinyirc-mode-map-define-keys)
  (tinyirc-mode-map-activate)           ;turn on the map
  (setq  mode-name   tinyirc--mode-name)
  (setq  major-mode 'tinyirc-pastebot-mode) ;; for C-h m
  (when (interactive-p)
    (message
     (substitute-command-keys
      (concat
       "Receive URL at line \\[tinyirc-pastebot-mode-command-receive] "
       "Line num \\[tinyirc-pastebot-mode-command-line-numbers-toggle] ")))
    (sleep-for 1))
  (run-hooks 'tinyirc--pastebot-mode-hook))

;;}}}
;;{{{ Pastebot: User functions

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-http-get (url buffer &optional verbose timeout)
  "Send URL and output result to BUFFER with VERBOSE optional TIMEOUT."
  (let ((port    80)
        connection
        path
        host)
    (or timeout
        (setq timeout 60))
    (cond
     ((stringp buffer)
      (setq buffer (or (get-buffer buffer)
                       (get-buffer-create buffer))))
     ((and (not (null buffer))
           (bufferp buffer)))
     (t
      (error "BUFFER arg [%s] is incorrect" buffer)))
    (if (not (string-match "^http://\\([^/]+\\)\\(/.*\\)" url))
        (error "Must be _http_ request '%s'" url)
      (setq host (match-string 1 url)
            path (match-string 2 url)))
    (with-current-buffer buffer
      (erase-buffer))
    (condition-case error
        (progn
          (if verbose
              (message "TinyIrc: opening %s:%s" host port))
          (setq
           connection
           (open-network-stream "*http*" buffer host port))
          (if verbose
              (message "TinyIrc: sending %s:%s + %s" host port path))
          (process-send-string
           connection
           (concat "GET "
                   path
                   " HTTP/1.0\r\n\r\n"))
          (while (and (eq 'open (process-status connection))
                      (accept-process-output connection timeout))))
      (error
       (error (cdr error))))
    (if verbose
        (message "TinyIrc: clossing %s:%s" host port))
    (if connection
        (delete-process connection))
    buffer))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyirc-pastebot-install-perl-util-pastebot ()
  "Install `tinyirc--pastebot-program-url'."
  (interactive)
  (let ((perl        (executable-find "perl"))
        (http-buffer tinyirc--http-buffer)
        (name        tinyirc--error-buffer)
        (url         tinyirc--pastebot-program-url)
        (filename    (file-name-nondirectory tinyirc--pastebot-program-url))
        (program     tinyirc--pastebot-program)
        buffer)
    (unless perl
      (with-current-buffer (or buffer
                               (get-buffer-create name))
        (insert (format "\
INSTALL PROBLEM: Perl

  The `tinyirc--pastebot-program' [%s] need Perl command interpreter to run.
  Perl doesn't seem to be installed. Please update environment variable
  PATH if you do have Perl, but it's merely missing from there."
                        tinyirc--pastebot-program))
        (if (file-directory-p "c:/")
            (insert "\

  If you haven't installed Perl language yet, get it from
  <http://cygwin.com> by downloading the setup.exe from top right
  and running the install program. Make sure you mark the checkbox
  for Perl from list of installable program list."))))
    (setq program nil)
    (when (not (stringp program))
      (let ((buffer (tinyirc-http-get
                     url
                     (get-buffer-create http-buffer)
                     'verbose)))
        (with-current-buffer buffer
          (goto-char (point-min))
	  (while (re-search-forward "\r" nil t)
	    (replace-match "" nil nil))
          (re-search-forward "^\n")
          (let (dir
                saveto)
            (setq dir (completing-read
                       (format
                        "Save %s to dir (press TAB; along PATH): " filename)
                       (mapcar (function
                                (lambda (x)
                                  (cons x 1)))
                               (split-string (getenv "PATH") path-separator))
                       nil
                       'match))
            (setq saveto
                  (expand-file-name
                   (concat (file-name-as-directory dir) filename)))
            (write-region (point) (point-max) saveto)
	    ;;  Make it executable
	    (let* ((x-bits 73)  ;; Oct 111
		   (r-bits 292) ;; Oct 444
		   (mode   (file-modes saveto))
		   (run-mode  (logior (logior mode x-bits)
				      r-bits)))
	      (set-file-modes saveto run-mode))
            (message "TinyIrc: saved %s" saveto)))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyirc-pastebot-install-example-servers ()
  "Install the Pastebot `servers' example configuration file. This function
tries to veriry the setup and suggest corrective actions how to get the
PasteBot interface working. Once the installation look valid, this function
should report an success status.

References:
  `tinyirc--pastebot-config-default-content'
  `tinyirc--pastebot-program'
  `tinyirc--pastebot-program-url'"
  (interactive)
  (let ((config-default-content tinyirc--pastebot-config-default-content))
    (let* ( ;; (win32  (file-directory-p "c:/"))
           ;;  (cygwin (string-match "cygwin" "emacs-version"))
           (dir    (tinyirc-path tinyirc--pastebot-config-directory))
           ;;  Watch out for Cygwin made symlink under Native
           ;;  Win32 NTEmacs. We must not
           (link   (concat dir ".lnk"))
           (config (format "%s/servers" dir)))
      (when (and (file-directory-p "c:/")
                 (file-exists-p link))
        (error "TinyIrc: [install] Cygwin conflict. File [%s] exists. \
That file might be a Windows shortcut, a symlink, made under Cygwin.
In that case, you cannot use PasteBot interfcase
both from NTEmacs and Cygwin Emacs, because NTEmacs
cannot by default follow Cygwin symlinks.

In case you aren't using Cygwin, please remove that Windows shortcut link
and create real directory instead."
               link))
      (if (file-directory-p dir)
          (message "TinyIrc: [install] Good, you have %s" dir)
        (message "TinyIrc: [install] Making directory %s" dir)
        (make-directory dir))
      (if (file-exists-p config)
          (message "TinyIrc: [install] Good, you have %s" config)
        (message "TinyIrc: [install] Writing configuration file %s ..."
                 config)
        (with-temp-buffer
          (insert config-default-content)
          (write-region (point-min) (point-max) config))
        (message "TinyIrc: [install] Writing configuration file %s ...done."
                 config))
      (let ((prg (tinyirc-pastebot-program-1)))
        (if prg
            (message "TinyIrc: [install] Good, you have program %s" prg)
          (error (concat
                  "TinyIrc: [install] FATAL you do not have program %s, "
                  "visit %s and install it along PATH")
                 tinyirc--pastebot-program
                 tinyirc--pastebot-program-url)))
      (message
       (concat
        "TinyIrc: [install] Check passed. "
        "Your PasteBot interface should be functonal provided that "
        "configuration file %s 1) contains needed entries and "
        "2) it has no syntax errors. "
        "This function did not check its content. ")
       config))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyirc-pastebot-receive-url (url)
  "Retrieve URL from PasteBot service."
  (interactive
   (list
    (read-string "Pastebot receive URL: "
                 (tinyirc-pastebot-url-at-point))))
  (when (or (not (stringp url))
            (not (string-match "http://" url)))
    (error "TinyIrc: invalid URL %s" url))
  (tinyirc-pastebot-receive-message
   url
   (tinyirc-pastebot-receive-call-process-url url)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyirc-pastebot-send-region (service user msg beg end)
  "Send code to SERVICE using Perl script pastebot.pl.
identify as USER with MSG. Send text in region BEG and END.

See http://sial.org/code/perl/scripts/pbotutil.pl.html
You also have to define databases for SERVICE selections, see script's
manual page for details.

References:
  `tinyirc--pastebot-send-file'."
  (interactive
   (let ((list (mapcar (function ;;  Make assoc list
			(lambda (x)
			  (cons x 1)))
		       (tinyirc-pastebot-service-list)))
	 (file tinyirc--pastebot-send-file))
     (tinyirc-pastebot-program-name)	;signal error if no program
     (unless list
       (error (concat "Tinyirc: Cannot get completions."
                      "Check pastebot `servers' file.")))
     (list
      (completing-read "Send to PasteBot service: "
                       list
                       nil
                       ;;  Because user may have updated the
                       ;;  configuration file and we don't know about it
                       (not 'requir-match)
                       (if tinyirc--pastebot-history-services
                           (car tinyirc--pastebot-history-services))
                       'tinyirc--pastebot-history-services)
      (read-string "Pastebot user: "
                   (if tinyirc--pastebot-history-user
                       (car tinyirc--pastebot-history-user)
                     (or user-login-name
                         (getenv "USER")))
                   'tinyirc--pastebot-history-user)
      (read-string "Pastebot message: ")
      (region-beginning)
      (region-end))))
  (let ((file (or tinyirc--pastebot-send-file
                  (error "TinyIrc: `tinyirc--pastebot-send-file' not set."))))
    (unless (and beg end)
      (error "Pastebot: region not defined"))
    (write-region beg end file)
    (tinyirc-pastebot-send-main file service msg user)))

;;}}}

;;; ----------------------------------------------------------------------
;;;
(defun tinyirc-install (&optional uninstall)
  "Install or UNINSTALL package."
  ;; (interactive "p")
  (tinyirc-mode-map-define-keys))

(tinyirc-install)
(provide 'tinyirc)
(run-hooks 'tinyirc--load-hook)

;;; End of tinyirc.el
