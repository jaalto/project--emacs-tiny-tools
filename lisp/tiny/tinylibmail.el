;;; tinylibmail.el --- Library of mail related functions

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1995-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinylibmail-version.
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

;; ........................................................ &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file
;;
;;     (require 'tinylibm)
;;
;; No, there is no mistake here. The 'm' lib contains all autoloads
;; to this package.

;;}}}
;;{{{ Documentation

;;; Commentary:

;;
;;      o   This is library. Package itself does nothing.
;;      o   Collection of functions to deal with Mail/News specific tasks.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: -- require

;;;  ....................................................... &v-require ...

(require 'tinylibm)
(require 'sendmail) ;; mail-header-separator

(eval-and-compile
  (cond
   ((ti::xemacs-p)
    (autoload 'build-mail-aliases "mail-abbrevs"))
   (t
    (autoload 'mail-abbrevs-setup "mailabbrev")
    (autoload 'build-mail-aliases "mailalias")
    (autoload 'build-mail-abbrevs "mailabbrev")))
  (autoload 'mail-fetch-field		"mail-utils")
  (autoload 'rmail-msgbeg		"rmail")
  (autoload 'rmail-msgend		"rmail")
  (autoload 'gnus-group-get-parameter	"gnus"))

(eval-when-compile
  (defvar mail-abbrevs)                 ;Silence ByteCompiler
  (defvar mail-aliases)
  (defvar rmail-current-message nil))

;;}}}
;;{{{ setup: -- private

;;; ......................................................... &v-hooks ...

(defvar ti:mail-load-hook nil
  "Hook that is run when package is loaded.")

;;; ....................................................... &v-private ...

(defvar ti:mail-ret nil
  "Global return value of this package.")

(defvar ti:mail-mail-buffer " *ti::mail-mail*"
  "*Temporary mail buffer name.")

;;  Variables could be modified. defsubst makes them persistent

(defsubst ti::mail-pgp-signature-begin-line ()
  "Signature start line."
  "-----BEGIN PGP SIGNATURE-----")

(defsubst ti::mail-pgp-signature-end-line ()
  "Signature end line."
  "-----END PGP SIGNATURE-----")

;; Signed message has:
;;
;; -----BEGIN PGP SIGNED MESSAGE-----
;; -----BEGIN PGP SIGNATURE-----
;; -----END PGP SIGNATURE-----

(defsubst ti::mail-pgp-signed-begin-line ()
  "Text for start of PGP signed messages."
  "-----BEGIN PGP SIGNED MESSAGE-----")

(defsubst ti::mail-pgp-signed-end-line ()
  "Text for start of PGP signed messages."
  (ti::mail-pgp-signature-end-line))

(defsubst ti::mail-pgp-pkey-begin-line ()
  "PGP public key begin line."
  "-----BEGIN PGP PUBLIC KEY BLOCK-----")

(defsubst ti::mail-pgp-pkey-end-line ()
  "PGP public key end line."
  "-----END PGP PUBLIC KEY BLOCK-----")

(defsubst ti::mail-pgp-msg-begin-line ()
  "PGP message, typically base64 signed, begin line."
  "-----BEGIN PGP MESSAGE-----")

(defsubst ti::mail-pgp-msg-end-line ()
  "PGP message, typically base64 signed, end line."
  "-----END PGP MESSAGE-----")

(defsubst ti::mail-pgp-any-pgp-line-regexp (&optional anchor-left)
  "Return regexp that match any pgp TAG.
If ANCHOR-LEFT is non-nil; the regexp will contains left ^ anchor."
  ;;  The lines can be broken, when there is encrypted/signed message
  ;;  NOTE: there is no anchor by default; because sometimes use may have
  ;;  indented the whole PGP block (e.g. in his web page or in .doc file)

  (concat
   (if anchor-left "^" "")
   "- -----\\(BEGIN\\|END\\) PGP.*-----"
   "\\|"
   (if anchor-left "^" "")
   "-----\\(BEGIN\\|END\\) PGP.*-----"))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-raw-p (ip)
  "Check raw nnn.nnn.nnn.nnn IP."
  (string-match "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$" ip))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-top-level-domain (host)
  "Convert HOST a.b.c  => b.c domain.
If HOST is raw numeric IP, do nothing."
  (cond
   ((ti::mail-ip-raw-p host)
    host)
   ((or (string-match "\\.\\([^.]+\\.[^.]+\\)$" host)
        (string-match "^\\([^.]+\\.[^.]+\\)$" host))
    (match-string 1 host))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-3-level-domain (host)
  "Convert HOST a.b.c.d  => b.c.d domain."
  (when (string-match "\\.\\([^.]+\\.[^.]+\\.[^.]+\\)$" host)
    (match-string 1 host)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-ip-cleanup (word)
  "Clean WORD to format 'aa.bb.cc'. Remove offending characters.
Remove all characters up till @: this@email.com => email.com
Remove all not(alphanumeric, dash, dot) charcters.

For example a word at point may include anything:

  <bb.com> \"bb.com\" this@bb.com

All of the above will become:

  bb.com"
  (and word
       (replace-regexp-in-string
        "[^a-z0-9-.]" ""
        (replace-regexp-in-string "^.*@" "" word))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-ip-at-point-1 ()
  "Read only word containing characters [-.a-zA-z0-9]."
  (let (beg
        word)
    ;;  depending where the point is, from this word different part
    ;;  is read: foo.com[1.2.3.4]
    ;;             |       |
    ;;            (1)     (2)
    (save-excursion
      (skip-chars-backward "-.a-zA-Z0-9")
      (setq beg (point))
      (skip-chars-forward "-.a-zA-Z0-9")
      (unless (eq beg (point))
        (buffer-substring beg (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-ip-at-point ()
  "Read domain ip IP name at point."
  (let* ((word (ti::mail-ip-at-point-1)))
    (when (not (ti::nil-p word))
      ;;  foo.com[1.2.3.4]
      (setq word (ti::mail-ip-cleanup word))
      (if (ti::mail-ip-raw-p word)
          word
        (ti::mail-ip-top-level-domain word)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-news-group ()        ;ding & gnus compatible
  "Return newsgroup name if group exists."
  (if (boundp 'gnus-newsgroup-name)
      (symbol-value 'gnus-newsgroup-name)))

;;; ........................................................ &v-public ...
;;; User configurable -- but not recommended.

;;  See gnus.el or gnus-msg.el  gnus-required-headers
;;  The 'in-reply-to is for mail messages (additional)

(defconst ti:mail-required-headers
  '(from date newsgroups subject path message-id in-reply-to references)
  "*All required fields that RFC977 and RFC1036 requires.
Make sure symbol names are all in lowercase.")

(defvar ti:mail-parse-name-not-accept
  (concat
   "[A-Z][/][A-Z]"                      ;company division ABC/TMS
   "\\|^[A-Z]+\\'"                      ;all in capitals
   "\\|^[-]$"                           ;single '-' word
   "\\|[.0-9]"                          ;maybe phone number
   "\\|com\\|org\\|edu"
   "\\|Dr")				;Titles
  "*Regexp to exclude non-valid people names.
We can't be sure that the names are really good names when we parse the
senders From field. Let's see an example

        \"Person Someone p. nnn-nnn-nnn\"

There obviously isn't 3rd name, it's used for phone abbrev. And the last
word is the actual phone number.

This regexp tells which word matches are false name hits.
In this example it'd leave:
        \"Person Someone\"

See `ti::mail-parse-name'")

;;}}}
;;{{{ setup: -- version

(defconst tinylibmail-version (substring  "$Revision: 2.68 $" 11 16)
  "Latest version number.")

(defconst tinylibmail-version-id
  "$Id: tinylibmail.el,v 2.68 2007/05/07 10:50:08 jaalto Exp $"
  "Latest modification time and version number.")

;;; ----------------------------------------------------------------------
;;;
(defun tinylibmail-version (&optional arg)
  "Show version information. ARG tell to print message in echo area only."
  (interactive "P")
  (ti::package-version-info "tinylibmail.el" arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinylibmail-submit-feedback ()
  "Submit suggestions, error corrections, impressions, anything..."
  (interactive)
  (ti::package-submit-feedback "tinylibmail.el"))

;;}}}
;;{{{ misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-signature-p  ()
  "Return beginning of line point if \\n-- *\\n is found.
The de facto, while not standardized by any RFC signature separator
it \\n-- \\n. The trailing whitespace is very unfortunate evolution
to separate signatures from message digests \\n--\\n.

This function accepts trailing spaces or just n\\--\\n"
  (let* ((point (point)))               ;avoid save-excursion.
    (ti::pmin)
    (prog1 (if (re-search-forward "\n-- *\n" nil t)
               (1+ (match-beginning 0)))
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-body-empty-p ()
  "Check if there is nothing in the body or if whole buffer is empty."
  (save-excursion
    (ti::mail-text-start 'move)
    (eq (point) (point-max))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-body-clear ()
  "Delete message body."
  (ti::mail-text-start 'move)
  (delete-region (point) (point-max)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-set-region 'lisp-indent-function 1)
(put 'ti::mail-set-region 'edebug-form-spec '(body))
(defmacro  ti::mail-set-region (beg end)
  "Set BEG END to whole buffer if they don't have value."
  `(progn
     (or ,beg
         (setq ,beg (ti::mail-text-start)))
     (or ,end
         (setq , end (point-max)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-point-in-header-macro 'lisp-indent-function 0)
(put 'ti::mail-point-in-header-macro 'edebug-form-spec '(body))
(defmacro ti::mail-point-in-header-macro (&rest body)
  "Run BODY only if point is inside mail header area."
  `(when (< (point) (ti::mail-text-start))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-message-length ()
  "Return message's body length, not including the headers.
The message length is measured be counting character between the
BODY begin and BODY end. Any amount of whitespaces around the body is
skipped.

Exceptions:

  The start point defaults to `point-min' if body can't be found.

  If there is PGP signed block, then the body length is the text inside
  PGP signed block, not the original message body.

  Signed headers are also skipped.

    -----BEGIN PGP SIGNED MESSAGE-----

    ##                                  < signed headers begin mark \\n##
    Subject: some subject
    Reply-to: somewhere
                                        < empty space ends headers
    Hi, I swanted to tell you...        < BODY BEGIN IS HERE"
  (let* ((end (ti::mail-pgp-signed-conventional-p))
         beg)
    (save-excursion
      (cond
       ((and end
             (null (ti::mail-pgp-signature-detached-p)))
        ;; Do not count empty lines at the end of body
        (goto-char end) (skip-chars-backward " \t\n") (setq end (point))
        (ti::pmin)
        (re-search-forward (ti::mail-pgp-signed-begin-line))
        (forward-line 1)
        ;;  now we're inside BODY of text, but it's not that simple yet. User
        ;;  may have signed headers and they are inserterted into body
        ;;  like this:
        ;;
        ;;  -----BEGIN PGP SIGNED MESSAGE-----
        ;;
        ;;  ##
        ;;  Subject: See this ma!
        ;;
        ;;  Body text starts here.
        ;;
        ;;  Note, there is no spaces, becasue the body is trimmed
        (when (looking-at "\n##\n")
          (goto-char (match-end 0))
          (re-search-forward "^$"))
        ;;  Ignore leading spaces
        (skip-chars-forward " \t\n")
        (- end (point)))
       (t
        (ti::mail-text-start 'move)
        (skip-chars-forward " \t\n") (setq beg (point))
        (goto-char (point-max))
        (if (eq beg (point))            ;Empty buffer
            0
          (skip-chars-backward " \t\n") (setq end (point))
          (- (point) beg)))))))

;;; ----------------------------------------------------------------------
;;;FIXME: this is old function and should be removed.
;;;
(defun ti::mail-get-2re (re str)
  "Use RE and return submatches 1 and 2 from STR.
Return list of empty strings if no matches."
  (let ((m1 "")
        (m2 ""))
    (if (eq nil (string-match re str))
        t                               ;do nothing, not matched
      (if (match-end 1)
          (setq m1 (substring str (match-beginning 1)
                              (match-end 1))))
      (if (match-end 2)
          (setq m2 (substring str (match-beginning 2)
                              (match-end 2)))))
    (list m1 m2)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-required-headers ()
  "Return standard RFC header required when posting.

References:

  `ti:mail-required-headers'
  `gnus-required-headers'

Return:

  list          '(header-name-symbol .. )
  nil           gnus not loaded ?"
  (cond
   ((listp ti:mail-required-headers)
    ti:mail-required-headers)
   ((boundp 'gnus-required-headers)
    (symbol-value 'gnus-required-headers))
   (t
    nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-mode-p ()
  "Check if some mail MUA mode is tuned on this buffer: RMAIL, VM, MH ..."
  (string-match
   "^\\(vm-\\|rmail-\\|mh-\\|gnus-article-\\|message\\).*mode"
   (symbol-name major-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mailbox-p ()
  "Check if two first lines look like Berkeley mailbox format."
  (save-excursion
    (ti::pmin)
    ;; From foo@some.com  Wed Dec 19 19:19:41 2001
    ;; Received: from some.com ([000.22.68.000])
    (and (looking-at "^\\(From:?\\|Return-Path:\\) .*@")
         (forward-line 1)
         (not (eobp))
         (looking-at "^[a-zA-Z-]+:[ \t]+[^ \r\r\n]"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-p ()
  "Check if first line contain left flushed header 'Header:'.
This is a sign that current buffer is in mail-like.
You should also check the mode name to get more reliable results."
  (or (memq major-mode '(message-mode mail-mode))
      (save-excursion
        (ti::pmin)
        (looking-at "^[-A-Za-z0-9][-A-Za-z0-9]+:"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-header-area-size ()
  "Count size of header area.
This function can only be called from a buffer that has
`mail-header-separator'. Function count the characters in the header  area.
You can use this information to determine if the headers have been
changed after the last check.

Return:

 nbr
 nil        can't find `mail-header-separator'"
  (save-excursion
    (ti::pmin)
    (when (re-search-forward (regexp-quote mail-header-separator) nil t)
      (- (point) (point-min)))))

;;; ----------------------------------------------------------------------
;;; - This is suitable for RMAIL, GNUS and for individual buffers
;;;   holding mail or news messages.
;;;
(defun ti::mail-hmax (&optional move noerr)
  "Search max point of header, optionally MOVE and NOERR.
Order is: `mail-header-separator' or find all \"Headers:\" and then
return next line after them. The header must start at `point-min'.

If no point can be found, return `point-min'."
  (let ((point (point-min)))
    (when (ti::mail-mail-p)
      (save-excursion
        (ti::pmin)
        ;;  VM's "t" key shows all headers, including the
        ;;  "From xxxx"foo.com" line which is not actual header, because
        ;;  it has no colon. Skip ovber it if we see it.
        (if (looking-at "From")
            (forward-line 1))
        ;;  GNUS 4 sets the mail-header-separator to "" ??
        (if (and (not (ti::nil-p mail-header-separator))
                 (re-search-forward (regexp-quote mail-header-separator) nil t))
            (setq point (match-beginning 0))
          ;;  Header:
          ;;    Continuing line here
          ;;  Header2:
          (while (and (looking-at "^[0-9a-zA-z-]+:")
                      (progn
                        (end-of-line)
                        (not (eobp))))
            ;;  If this function doesn't move anuy more, then the headers
            ;;  have ended.
            (if (null (ti::mail-next-field-start 'move))
                (forward-line 1))
            (setq point (point))))))
    (if (and move point)
        (goto-char point))
    point))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-text-start (&optional move)
  "Return starting point or text in BODY. Optionally MOVE to it.
If buffer is not mail-like, then return `point-min'.

References:
  `mail-header-separator'"
  (let ((re         (regexp-quote mail-header-separator))
        (point      (point-min)))
    (when (ti::mail-mail-p)
      (cond
       ((save-excursion                 ;Do we find the separator?
          (ti::pmin)
          (when (re-search-forward re nil t)
            (forward-line 1)
            (setq point (point)))))
       ((setq point (ti::mail-hmax))
        (save-excursion
          (goto-char point)
          (forward-line 1)
          (setq point (point))) )
       (t
        (error "Can't find position.")))
      (if (eq point (point-min))        ;Not found
          (error "mail-header-separator not found or headers not found.")))
    (if (and move point)
        (goto-char point))
    point))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-point-at-header-p ()
  "Return non-nil if point is at HEADER area of mail."
  (< (point) (ti::mail-text-start)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-point-at-body-p ()
  "Return non-nil if point is at BODY of mail."
  (not (ti::mail-point-at-header-p)))

;;; ----------------------------------------------------------------------
;;; - Many std emacs dist. functions work so that you have to narrow
;;;   to headers before you can call the functions.
;;;
(defun ti::mail-narrow (&optional text)
  "Narrows to the headers only. Optionally to TEXT portion."
  (if text
      (narrow-to-region (ti::mail-text-start 'move) (point-max))
    (narrow-to-region (point-min) (ti::mail-hmax))))

;;; ----------------------------------------------------------------------
;;; - This is for both GNUS and RMAIL
;;;
(defun ti::mail-mail-buffer-name ()
  "Find original mail buffer whether in GNUS or in RMAIL.

Return:

   string       buffer name
   nil          if not exist."

  ;;  It's amazing that GNUS uses pointers and RMAIL uses string ...
  (let ((buffer (if (boundp 'mail-reply-buffer)
                    (symbol-value 'mail-reply-buffer))))
    (cond                          ;what is the mail YANK buffer name?
     ((stringp buffer)
      buffer)
     ((and (not (null buffer))
           (bufferp buffer))
      (buffer-name buffer))
     (t
      nil))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-generate-buffer-name (&rest ignore)
  "Rename the *mail* buffer to \"*mail* SENDER\". IGNORE args.
You can install this function e.g. into
`my-message-generate-new-buffers' or `mail-setup-hook'"
  (interactive "Pbuffer name: ")
  (let ((to  (if (string= (buffer-name) " *gnus article copy*") ;; See gnus-msg.el
                 (mail-fetch-field "From")
               (mail-fetch-field "To")))
        (str "*mail*"))
    (unless (ti::nil-p to)
      (cond
       ((setq str (ti::string-match "\\([^@<]+\\)," 1 to))
        (setq str (concat str ", ...")))
       ((setq str (ti::string-match "\\([^@<]+\\)" 1 to)))
       (t
        (setq str to)))
      (setq str (replace-regexp-in-string "['\"]" "" str)) ;remove extra cruft
      (setq str
            (concat
             (if (ti::mail-news-buffer-p)
                 "*post* "
               "*mail* ")
             str)))
    str))

;;; ----------------------------------------------------------------------
;;; - The idea is to find three fields and see what they contain,
;;;   and do they exist?
;;; - What's the use of this function? Well, when you post an article
;;;   or mail it, you can call this function from some of those
;;;   posting hooks to determine what to do with the buffer.
;;;
;;; - code lines disabled now so that it buffer can be checked any time

(defun ti::mail-mail-simple-p ()
  "Check if buffer contain headers belonging to simple \\[mail].
You can call this only once, just after the buffer is initially created"
  (let ((sub (mail-fetch-field "Subject"))
	;;   mail-fetch-field doesn't return nil if field is empty.
	(to  (mail-fetch-field "to"))
	(news (mail-fetch-field "Newsgroups")))
    ;;  When you're replying to message in NEWS, RMAIL, the SUBJ and
    ;;  TO fields are already filled.
    ;;
    ;;  That's why you can only call this function once.
    ;;  When you use C-x m, and fill the fields, there is no way
    ;;  to detect afterwards if the mail buffer was simple mail or not
    (cond
     ((or (string-match "news" (symbol-name 'major-mode))
          news)
      nil)
     ((and (ti::nil-p sub)
           (ti::nil-p to))
      t))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-to-list-p ()
  "Check if message is meant to be sent to a mailing list.
In GNUS you need to add Group parameter `to-list' containing address
to mailing list or otherwise Group is not considered mailing list."
  (when (featurep 'gnus)
    (let* ((group (ti::mail-news-group)))
      (when (stringp group)
        (gnus-group-get-parameter group 'to-list) ))))

;;}}}
;;{{{ PGP general, tests

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-v3xx-p ()
  "Check if X-Pgp v3.xx header signing is in use.
It should have VALUE = KEYWORD; statement."
  (save-excursion
    (ti::pmin)
    (when (re-search-forward "X-Pgp-signed" nil t)
      (forward-line 1)
      ;;
      ;; KEYWORD=VALUE;
      (looking-at "^[ \t]+.*=.*;"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-p ()
  "Check if buffer contain PGP. It must have left flushed regexp:
\"^-----BEGIN.*PGP +\\(SIGNATURE\\|SIGNED\\\\|MESSAGE)\", otherwise this
string may be inside quoted text.

If there is X-pgp-sig.*: header, then it's also considered PGP message."
  (let ((max (ti::mail-hmax)))          ;headers ?
    ;;  if headers was found use that.

    (setq max (if (eq (point-min) max)
                  nil
                max))
    (save-excursion
      (ti::pmin)
      (or (let (case-fold-search)
            (re-search-forward
             "^-----BEGIN PGP \\(SIGNATURE\\|SIGNED\\|MESSAGE\\)"
             nil t))
          (progn
            ;;  The New PGP in headers standard.
            (re-search-forward "^X-Pgp-Sig.*:" max t))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-conventional-p ()
  "Return t if message is conventionally signed."
  (save-excursion (ti::pmin) (ti::mail-pgp-re-search 'sig)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-detached-p  ()
  "Return (beg . end) if there is detached signature."
  (let* ((point  (point))
         beg
         end)
    (prog1 (save-excursion
             (ti::pmin)
             (unless (ti::mail-pgp-re-search 'msg) ;Must not exist
               (and (setq beg (ti::mail-pgp-re-search 'sig))
                    (setq end (ti::mail-pgp-re-search 'sige))
                    (cons beg end))))
      (goto-char point))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-conventional-multi-p ()
  "Return t if message is signed conventionally multiple times."
  (save-excursion
    (ti::pmin)
    (ti::mail-pgp-re-search 'sig 'move)
    (forward-line 1)
    (ti::mail-pgp-re-search 'sig 'move)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-xpgp-p ()
  "Return t if message is X-pgp signed.
There may be X-Pgp headers, but if the message is already
verified, that removes the signature around encrypted
message  \"- -----BEGIN PGP MESSAGE-----\"
       --> \"-----BEGIN PGP MESSAGE-----\"
In this case the message is no more in signed format,
but in encrypted format."
  (and (ti::mail-pgp-headers-p)
       ;;  See documentation above
       (save-excursion
         (ti::pmin)
         (null (re-search-forward
                (concat "^" (ti::mail-pgp-msg-begin-line))
                nil t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signed-p ()
  "Return t is message is conventionally or X-pgp signed."
  (or (ti::mail-pgp-signed-xpgp-p)
      (ti::mail-pgp-signed-conventional-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-public-key-p (&optional point)
  "Find public key delimiter from current point forward or using POINT."
  (save-excursion
    (goto-char (or point (point)))
    (re-search-forward (ti::mail-pgp-pkey-begin-line) nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-remail-p ()
  "Check if This is remailer message."
  (save-excursion
    (ti::pmin)
    (re-search-forward "[:#][:#]+\nReply-To" nil t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-comment-file-p (&optional point)
  "You can send binary files with base64 signing.
This function checks if comment block has have words 'File: FILE '.

Example:

 -----BEGIN PGP MESSAGE-----
 Version: 2.6.3ia
 Comment: Base64 signed. File: tm.tar uncompresses to approx. 20K

Input:

  POINT     search start point

Return:

  nil
  file"
  (save-excursion
    (if point (goto-char point))
    (when (re-search-forward "^Comment:.*File:? +\\([^ \t,]+\\)" nil t)
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-encrypted-p (&optional check-pgp-dash-line)
  "Check if there is encrypted PGP message.
It must have left flushed tag. The start point of match is returned.
The following tag will tell if if the message is encrypted.

  ::
  Encrypted: PGP

Input:

  CHECK-PGP-DASH-LINE   if the tag is not found, message _could_ be signed
                        if there is -----BEGIN PGP MESSAGE----- tag.
                        When this flag is non-nil, it also checks this
                        case. Beware: message could be base64 signed too,
                        so the encrypted-p test may not be exactly right."
  (save-excursion
    (ti::pmin)
    (if (re-search-forward "::[ \t]*\nEncrypted:[ \t]*PGP" nil t)
        (match-beginning 0)
      (when check-pgp-dash-line
        (ti::pmin)
        (car-safe (ti::mail-pgp-block-area 'msg))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-normal-p (&optional point)
  "Check if there is any PGP in current buffer from POINT forward.
The beginning point of PGP is returned."
  ;; Must find at least two lines, maybe BEG and END
  (let ((re   (ti::mail-pgp-any-pgp-line-regexp 'acnhor))
        ret)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward re nil t)
        (setq ret (match-beginning 0))
        (if (null (re-search-forward re nil t))
            (setq ret nil)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-headers-p ()
  "Return t if PGP information is in headers.
Searches string 'X-Pgp-Signed:' and return end of match or nil."
  (let ((psig           "^X-Pgp-Signed:")
        (hmax           (ti::mail-hmax)))
    (save-excursion
      (ti::pmin)
      (re-search-forward psig hmax t))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-re (str)
  "Add possible beginning anchor if STR doesn't have one."
  (if (not (char-equal (aref str 0) ?^))
      (concat "^" str)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-block-area-kill-forward (mode &optional move)
  "Search PGP block forward and kill it. If no block found, do nothing.

Input:
  MODE      choices are explained in `ti::mail-pgp-block-area'.
  MOVE      if non-nil, move to killed region begin point."
  (let* (reg)
    (when (setq reg (ti::mail-pgp-block-area mode))
      (delete-region (car reg) (cdr reg))
      (when move (goto-char (car reg))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-block-area (mode &optional inside max nstrict)
  "Return (beg . end) of PGP block from current point forward.

Input:
  MODE      nil   search signed start line..
            'sig  search signature start instead.
            'signed  search signed message area
            'pkey search public key block start instead.
            'msg  search for pgp base64 signed \"message\"
            'any  go to `point-min' and search beginning of any
                  PGP line, then go to the end of buffer and search
                  backward any PGP line.  The lines must not be at
                  same position.  This gives you the whole PGP
                  region.

  INSIDE    if non-nil, beg and end are 'inside' without the PGP tags.
  MAX       max point to search
  NSTRICT   If non-nil; then the pgp bounds must not be left flushed,
            but can contains \"- -\".
Return:
  (beg . end)
  nil"
  (let ((re1
         (cond
          ((null mode)      (ti::mail-pgp-signed-begin-line))
          ((eq 'sig  mode)  (ti::mail-pgp-signature-begin-line))
          ((eq 'pkey mode)  (ti::mail-pgp-pkey-begin-line))
          ((eq 'msg  mode)  (ti::mail-pgp-msg-begin-line))
          ((eq 'any  mode)  (ti::mail-pgp-any-pgp-line-regexp (not nstrict)))
          ((eq 'signed  mode)  (ti::mail-pgp-signed-begin-line))
          (t
           (error "unknown mode"))))
        (re2
         (cond
          ((null mode)      (ti::mail-pgp-signed-end-line))
          ((eq 'sig  mode)  (ti::mail-pgp-signature-end-line))
          ((eq 'pkey mode)  (ti::mail-pgp-pkey-end-line))
          ((eq 'msg  mode)  (ti::mail-pgp-msg-end-line))
          ((eq 'signed  mode)  (ti::mail-pgp-signed-end-line))))
        ret
        beg
        end)
    (save-excursion
      (cond
       ((eq mode 'any)

        (when (re-search-forward re1 max t)
          (setq beg (match-beginning 0))
          (when (re-search-forward re1 max t)
            (beginning-of-line)
            (when (not (eq beg (point)))
              (forward-line 1)
              (setq ret (cons beg (point)))))))
       (t
        (if nstrict
            (setq re1    (concat "^-? ?" re1)
                  re2    (concat "^-? ?" re2))
          (setq re1    (concat "^" re1)
                re2    (concat "^" re2)))
        (when (re-search-forward re1 max t)
          (if inside
              (forward-line 1)
            (beginning-of-line))
          (setq beg (point))

          (when (re-search-forward re2 max t)
            (if inside
                (beginning-of-line)
              (forward-line 1))

            (setq end (point))
            (setq ret (cons beg end)))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-re-search (&optional mode move end no-anchor)
  "Re-search-forward to find -----BEGIN.*SIGNED.

Input:
  MODE          nil     search signed start line.
                'sig    search signature start.
                'sige   search signature block end.
                'pkey   search public key block start.
                'pkeye  search public key block end.
                'msg    search for pgp base64 signed \"message\"
                        This also finds conventionally crypted tag.
                'kid    search for  'Key for user ID: '
                'kpub   search for 'pub   512/47141D35 1996/06/03 ...'
                        note: match level 1 matches 0x code 47141D35
  MOVE          flag    non-nil moves point to found point
  END           flag    use `match-end' instead of math-beginning.
  NO-ANCHOR     flag    non-nil disables using '^' anchor.

Return:
  point         ,beginning of line
  nil           ,if not found"
  (let ((re   (cond
               ((null     mode)  (ti::mail-pgp-signed-begin-line))
               ((eq 'sig  mode)  (ti::mail-pgp-signature-begin-line))
               ((eq 'sige mode)  (ti::mail-pgp-signature-end-line))
               ((eq 'pkey mode)  (ti::mail-pgp-pkey-begin-line))
               ((eq 'pkeye mode) (ti::mail-pgp-pkey-end-line))
               ((eq 'msg  mode)  (ti::mail-pgp-msg-begin-line))
               ((eq 'kid  mode)  "Key for user ID: ")
               ((eq 'kpub mode)
                "pub[ \t]+[0-9]+/\\([A-Z0-9]\\)+[ \t]+.*/.*/[0-9]")
               (t
                (error "unknown mode"))))
        point)
    (when (and (null no-anchor)
               (not (memq mode '(kid))))
      ;;  suppose encrypted and signed message
      ;;  - -----END PGP MESSAGE-----
      ;;
      (setq re (concat "^-? ?" re)))
    (save-excursion
      (if (or (looking-at re)
              (re-search-forward re nil t))
          (if end
              (setq point (match-end 0))
            (setq point (match-beginning 0)))))
    (if (and move point)
        (goto-char point))
    point))

;;}}}
;;{{{ PGP misc

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-exe-version-string (&optional exe-file-location)
  "Call pgp/gpg executable to find out its version number.
EXE-FILE-LOCATION defaults to \"pgp\" but can also be absolute path."
  (with-temp-buffer
    (call-process (or exe-file-location "pgp")
                  nil
                  (current-buffer)
                  nil
                  ;;  - With PGP will say "illegal option", but will print
                  ;;    the logo screen.
                  ;;  - With GPG will print logo screen.
                  "--help")
    (ti::pmin)
    (when (or (re-search-forward
               "Pretty Good Privacy(tm) +\\([^\r\n ]+\\)" nil t)
              (re-search-forward
               "gpg (GnuPG) +\\([^\r\n ]+\\)" nil t))
      (match-string 1))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-type ()
  "Examine pgp data packet type by searching _forward_.
Return:
  'base64 'pgp 'conventional or nil"
  (let ((re  (ti::mail-pgp-any-pgp-line-regexp 'anchor))
        char)
    (save-excursion
      (when (and (re-search-forward re nil t)
                 (re-search-forward "^$" nil t))
        ;; FIXME: Check first character. Actually we should check bit mask...
        ;;
        ;;  -----BEGIN PGP MESSAGE-----
        ;;  Version: 2.6.2
        ;;  Comment: Encrypted by xxx
        ;;
        ;;  hEwDYCggxO/bFq0
        (forward-line 1)
        (setq char (following-char))
        (cond
         ((char-equal char ?p) 'conventional)
         ((char-equal char ?h) 'pgp)
         ((char-equal char ?o) 'base64))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-trim-buffer ()
  "Trim buffer: pgp blocks are left flushed and junk around them is removed."
  (let ((stat  t)
        region)
    (save-excursion
      (ti::pmin)
      (while (and stat
                  (setq region (ti::mail-pgp-block-area 'any)))

        (when (setq stat (ti::mail-pgp-chop-region (car region) (cdr region)))
          (goto-char (cdr stat)))))))

;;; ----------------------------------------------------------------------
;;; - This is needed after finger or http call to clean up all unnecessary
;;;   tags around the PGP key.
;;;
(defun ti::mail-pgp-chop-region (beg end)
  "Delete junk around BEG END from pgp public key block.
Area BEG END that correspond to pgp begin and end
lines (call `ti::mail-pgp-block-area' with argument 'any),
then we chop the public key region so that only the pgp area
is left without additional garbage.

Return
 (beg .end)         the canonilized area of PGP block

Example:

<PRE>
<SAMP>      -----BEGIN PGP PUBLIC KEY BLOCK-----</SAMP>
<SAMP>      Version: 2.6.3ia</SAMP>
</PRE>
<PRE>
<SAMP>      mQBNAzGzQ2MAAAECAM4p2THKCpNjYXDLpsg4sLHyEiNxJwQuEYfipdTj</SAMP>
<SAMP>      p5CPHN+0LkphcmkgQWFsdG8sIEZpbmxhbmQgPGphcmkuYWFsdG9AbnRj</SAMP>
<SAMP>      LmNvbT6JAFUDBRAxs0O+wLrt1UcUHTUBAbMhAf9Qgh6EznEcY2OUOIPg</SAMP>
<SAMP>      =46gx</SAMP>
<SAMP>      -----END PGP PUBLIC KEY BLOCK-----</SAMP>

This is converted into

-----BEGIN PGP PUBLIC KEY BLOCK-----
Version: 2.6.3ia</SAMP>

mQBNAzGzQ2MAAAECAM4p2THKCpNjYXDLpsg4sLHyEiNxJwQuEYfipdTj
p5CPHN+0LkphcmkgQWFsdG8sIEZpbmxhbmQgPGphcmkuYWFsdG9AbnRj
LmNvbT6JAFUDBRAxs0O+wLrt1UcUHTUBAbMhAf9Qgh6EznEcY2OUOIPg
=46gx
-----END PGP PUBLIC KEY BLOCK-----"
  (save-excursion
    (goto-char beg) (beginning-of-line)
    (ti::narrow-safe (point) (progn
                               (goto-char end)
                               (end-of-line)
                               (point))
      (ti::buffer-fill-region-spaces (point-min) (point-max))
      (ti::pmin)
      (re-search-forward "-----END")
      (goto-char (match-beginning 0))
      (if (> (current-column) 0)     ;Nothing to do, it's left flushed
          (delete-rectangle (point-min) (point)))
      (ti::pmin)
      (ti::buffer-replace-regexp "<.*$" 0 "")
      ;;  Because the last line does not have newline, the
      ;;  previous regexp doesn't match. Fix the last line too.
      (goto-char (point-max))
      (beginning-of-line)
      (let (case-fold-search)           ;be sensitive
        ;;  -----END PGP PUBLIC KEY BLOCK-----
        (if (and (looking-at ".*[A-Z]-----\\(.*\\)")
                 (match-end 1))
            (ti::replace-match 1)))
      (setq beg (point-min)
            end (point-max))))
  (cons beg end))

;;}}}
;;{{{ PGP signed headers

;;; ...................................................... &pgp-header ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-header-kill-in-body ()
  "Kill headers that are inserted into the body of message.
If there is no headers, this function does nothing.

--text follows this line--  [or empty line after headers]
##<no spaces>
Header1: content
Header2: content
<empty line>
BODY"
  (let* (beg)
    (save-excursion
      (ti::mail-text-start 'move)
      (setq beg (point))
      (when (and (looking-at "^##\n")
                 (re-search-forward "^$" nil t))
        (delete-region beg (point))))))

;;}}}
;;{{{ PGP ASCII armor

;;; ....................................................... &pgp-armor ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-char-to-int (char)
  "Process PGP ascii armor data.
Input is ASCII armor CHAR (as one string). Function return respective int."
  (let* ((table (concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "abcdefghijklmnopqrstuvwxyz0123456789+/"))
         case-fold-search
         str)
    (if (null (setq str (ti::string-match
                         (concat ".*" (regexp-quote char)) 0 table)))
        (error "Armor char is invalid %s " char)
      (1- (length str)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-string-to-bin-string (string)
  "Process PGP ascii armor data.
Convert quoted printable ASCII armor STRING into binary string."
  (let* ((len (length string))
         (i    0)
         (ret  "")
         ch
         int
         bin)
    (while (< i len)
      (setq ch (substring string i (1+ i)))
      (setq int (inline (ti::mail-pgp-data-char-to-int ch)))
      (setq bin (inline (int-to-bin-string int 6)))
      (setq ret (concat ret bin))
      (incf i))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-bin-string-to-int-list(string)
  "Process PGP ascii armor data.
Convert 8bit binary byte string \"000001...\" into list of ints."
  (let* ((len (/ (length string) 8))
         (i   0)
         ret
         bin
         int)
    (while (< i len)
      (setq bin (substring string (* i 8) (+ 8 (* i 8))))
      (setq int (inline (bin-string-to-int bin)))
      (incf i)
      (push int ret))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-pgp-data-ascii-armor-convert (string)
  "Convert PGP ascii armor STRING(quoted printable) into list of ints."
  (ti::mail-pgp-data-bin-string-to-int-list
   (ti::mail-pgp-data-string-to-bin-string string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-data-study-ctb-byte (int)
  "From single INT, examine the PGP CTB structure.
Return
 nil    ,input was not CTB byte
 '(ctb-type length-field)
        ctb-type  is
        'enc      (pgp encrypted message)
        'signed   (signed message)
        'secring  (secret keyring)
        'pring    (public keyring)
        'base64   (base 64 signed)
        'crypt    (conventionally crypted)
        'raw      (raw literal plaintext)
        'trust    (keyring trust packet)
        'uid      (user id packet)
        'comment  (comment packet)
        'unknown  (none of the above...)

        length is
        nil       no length, unknown length
        1 2 4     byte packet length"
  (let* ((length-mask   3)     ;; 00000011b
         (type-mask     60)    ;; 00111100b
         (ctb-mask      128)   ;; 10000000b
         (table
          '((1  . enc)
            (2  . signed)
            (5  . secring)
            (6  . pring)
            (8  . comp)
            (9  . crypt)
            (11 . raw)
            (12 . trust)
            (13 . uid)
            (14 . comment)))
         (type 'unknown)
         val
         ret)
    (when (logand int ctb-mask)

      ;; shift to the right 2 bits

      (when (setq val (assq (lsh (logand int type-mask) -2) table))
        (setq type (cdr val)))

      (setq val (logand int length-mask))
      (cond
       ((eq 0 val)  (setq val 1))
       ((eq 1 val)  (setq val 2))
       ((eq 1 val)  (setq val 4))
       ((eq 3 val)  (setq val nil)))
      (setq ret (cons type val)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-pgp-stream-study-1-ver (int)
  "Return pgp version string from stream INT."
  (cond
   ((eq 2 int) "2.5")
   ((eq 3 int) "2.6")
   (t          (error "Invalid Data format."))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-pgp-stream-study-1-key-id 'lisp-indent-function 1)
(put 'ti::mail-pgp-stream-study-1-key-id 'edebug-form-spec '(body))
(defmacro ti::mail-pgp-stream-study-1-key-id (stream result)
  "Read MSB and LSB key-id from STREAM to RESULT.
STREAM will be advanced during read."
  `(let* ((i 0))
     (while (< i 4)
       (setq ,result (concat (or ,result  "")
                                (format "%02x" (car ,stream)))
             ,stream (cdr ,stream)
             i          (1+ i)))
     (setq ,result (upcase ,result))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-1-time (stream)
  "Read TIME from STREAM to RESULT."
  (let* (val1
         val2)
    ;;  There must be easier way to do, but right now it goes like this
    ;;  '(51 158 95 145)
    ;;  --> hex 339E 5F91
    ;;  --> int 13214  24464  which is in (current-time) format
    ;;

    (setq val1 (hexl-hex-string-to-integer
                (concat
                 (int-to-hex-string (car stream))
                 (int-to-hex-string (car (cdr stream)))))

          stream (cdr (cdr stream))
          val2  (hexl-hex-string-to-integer
                 (concat
                  (int-to-hex-string (car stream))
                  (int-to-hex-string (car (cdr stream))))))
    (ti::date-standard-date nil (list val1 val2))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-enc (length stream)
  "Study the 'enc packet, which has known LENGTH.
STREAM is list of ints, minimum 11 integers, 13 is the full 'enc packet.

Return:
 '(version-string
   key-msb-hex-string
   key-lsb-hex-string

   rsa-algorithm                 ;; nil if stream is not long enough
   rsa-int (encrypted integer))   ;; nil if stream is not long enough."
  (let* ((msb "")
         (lsb "")
         ver
         val
         rsa-alg
         rsa-int)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION KEY-MSB KEY-LSB
    ;;  1byte 1-4bytes   1byte   4bytes  4bytes
    (setq stream (nthcdr (1+ length) stream))
    (setq val    (car stream)   stream (cdr stream))
    (setq ver (ti::mail-pgp-stream-study-1-ver val))
    (ti::mail-pgp-stream-study-1-key-id stream msb)
    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (setq rsa-alg (car stream)
          rsa-int (cadr stream))
    (list ver msb lsb rsa-alg rsa-int)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-signed (length stream)
  "Study the 'sign packet, which has known LENGTH. STREAM is list of ints.

Return:

 '(version-string
   md-length
   sig-class
   timestamp

   key-msb-hex-string
   key-lsb-hex-string

   alg-rsa                  ;; nil if stream is not long enough
   alg-md5                  ;; nil if ...
   digest                   '(int int);; nil if ...

   rsa-algorithm                 ;; nil if stream is not long enough
   rsa-int (encrypted integer)   ;; nil if stream is not long enough)"
  (let* ((msb "")
         (lsb "")
         ver
         md-length
         sig-class
         timestamp
         alg-rsa
         alg-md5
         digest)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION KEY-MSB KEY-LSB
    ;;  1byte 1-4bytes   1byte   4bytes  4bytes

    (setq stream (nthcdr (1+ length) stream))
    (setq ver       (ti::mail-pgp-stream-study-1-ver (car stream))
          stream    (cdr stream)
          md-length (car stream)
          stream    (cdr stream)
          sig-class (car stream)
          stream    (cdr stream))
    (setq timestamp
          (ti::mail-pgp-stream-study-1-time stream))
    (setq stream (nthcdr 4 stream))
    (ti::mail-pgp-stream-study-1-key-id stream msb)
    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (setq alg-rsa (car stream)
          stream  (cdr stream)
          alg-md5 (car stream)
          stream  (cdr stream)
          digest  (list (car stream) (car (cdr stream))))
    (list ver md-length sig-class timestamp msb lsb
          alg-rsa alg-md5 digest)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study-pring (length stream)
  "Study the 'pring packet, which has known LENGTH. STREAM is list of ints.

Return:

 '(version-string
   timestamp
   key-msb-hex-string
   key-lsb-hex-string)"
  (let* ((msb "")
         (lsb "")
         ver
         timestamp
         validity)
    ;;  Skip to begin of real data
    ;;  CTB   LENGTH     VERSION TIME    VALIDITY RSA-ALG
    ;;  1byte 1-4bytes   1byte   4bytes  2bytes   1byte
    (setq stream (nthcdr (1+ length) stream))
    (setq ver       (ti::mail-pgp-stream-study-1-ver (car stream))
          stream    (cdr stream))
    (setq timestamp
          (ti::mail-pgp-stream-study-1-time stream))
    (setq stream    (nthcdr 4 stream)
          validity  (car stream)
          stream    (cdr stream))
    ;;  PGP format spec is not clear enough here!
    ;;  Don't know where the User ID is...
;;;    (ti::mail-pgp-stream-study-1-key-id stream msb)
;;;    (ti::mail-pgp-stream-study-1-key-id stream lsb)
    (list ver timestamp validity msb lsb)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-study (ctb stream)
  "Study PGP data.

Input:

 CTB            in format `ti::mail-pgp-data-study-ctb-byte'
 STREAM         dearmored int stream (list of ints including ctb-byte)

Return:

  LIST          depends on the ctb, see conversion functions."
  (let* ((type  (car ctb))
         (len   (cdr ctb)))

;;;    (ti::d! type)
    (cond
     ((eq type 'enc)
      (ti::mail-pgp-stream-study-enc len stream))
     ((eq type 'signed)
      (ti::mail-pgp-stream-study-signed len stream))
     ((eq type 'base64)
      ;; #todo
      nil)
     ((eq type 'crypt)
      ;; #todo
      nil)
     ((eq type 'pring)
      (ti::mail-pgp-stream-study-pring len stream)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-xpgp ()
  "If there is X-Pgp-Signed field, goto signature stream."
  (let* ((point (ti::mail-pgp-headers-p)))
    (when point
      (goto-char point)
      ;;  must exist, this call dies if not found
      (re-search-forward "Signature[ \t]*=[ \t\n\"]+"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward (&optional any)
  "Find PGP data stream block start forward. PGP block must be left flushed.

Input:

  ANY       if non-nil, then find any stream (not necessarily left flushed)

Return:

  point     cursor is placed in front of stream
  nil       If there is no PGP stream block, do nothing."
  (let* ((beg   (ti::mail-pgp-msg-begin-line))
         (sig   (ti::mail-pgp-signature-begin-line))
         (pkey  (ti::mail-pgp-pkey-begin-line))
         (re    (concat (if any "" "^") "-----BEGIN"))
         (point (point))
         (loop  t)
         col
         ret)
    ;; base64
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; owEBbACT/4kAVQMFATOb

    ;; normal signature
    ;; -----BEGIN PGP SIGNATURE-----
    ;; Version: 2.6.3ia
    ;; Charset: noconv
    ;;
    ;; iQBVAwUBM55fkcC67dVHFB01AQGnHwIAqe2OfkdcnQviGzCmy3KddnsE8uFkAeaV

    ;; conventional crypt
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; pgAAACN9WXlrJFURU5Xgi+YyN

    ;; encrypted
    ;; -----BEGIN PGP MESSAGE-----
    ;; Version: 2.6.3ia
    ;;
    ;; hEwDwLrt1UcUHTUBAf9
    ;;

    ;; Extracted public key
    ;; -----BEGIN PGP PUBLIC KEY BLOCK-----
    ;; Version: 2.6.3ia
    ;;
    ;; mQBNAzOW770AAAECANDkXBfEbJk0gW41o52nLiktpThcBY+BMQCY5zyGCyUIbrDp
    (while (and loop (re-search-forward re nil t))
      (goto-char (match-beginning 0))

      (when (or (looking-at beg)
                (looking-at sig)
                (looking-at pkey))
        (setq col (current-column))
        (when (re-search-forward "^[ \t]*$" nil t)
          (setq loop nil)
          (forward-line 1)
          (move-to-column col)
          (setq ret (point))))
      (if loop
          (end-of-line)))               ;wrong match, Continue search
    (unless ret
      ;;  none found, return to original position.
      (goto-char point))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-and-study (&optional search any)
  "Find PGP data stream forward and study it.

If normal search fails, then find X-Pgp-Signed field's first
data stream.

Input:

  SEARCH    if non-nil, then search PGP starting from `point-min' if
            forward lookup fails.
  ANY       if non-nil, find also non-left flushed stream.

Return:

  '(CTB . (INFO-LIST))  the CTB type and data for CTB
  nil                   no stream found forward."
  (interactive)
  (let* ((point (point))
         ctb
         line
         list
         data
         ret)
    (when (or (ti::mail-pgp-stream-forward any)
              (and search
                   (ti::pmin)
                   (ti::mail-pgp-stream-forward any))
              (ti::mail-pgp-stream-forward-xpgp))
      ;;  Will match all base64 characters (approx.)
      (setq line (ti::buffer-match "[^ \t\n\"\']+" 0)
            list (ti::mail-pgp-data-ascii-armor-convert line)
            ctb  (ti::mail-pgp-data-study-ctb-byte (car list))
            data (ti::mail-pgp-stream-study ctb list))
      (setq ret (cons (car ctb) data)))
    (unless ret (goto-char point))     ;Nothing found, return to point
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-forward-info (&optional search any)
  "Find PGP data stream and read some information. Return string.

Input:

  SEARCH    if non-nil, then search PGP starting from `point-min' if
            forward lookup fails.
  ANY       if non-nil, find also non-left flushed stream."
  (let* (ver
         time key-id
         data
         type
         ret)
    (when (setq data (ti::mail-pgp-stream-forward-and-study search any))
      (setq type (car data))
      (cond
       ((eq type 'signed)
        (setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
              time      (ti::mail-pgp-stream-data-elt data 'time)
              key-id    (ti::mail-pgp-stream-data-elt data 'key-id))
        (setq ret (format "Signed by 0x%s %s [v%s.x]" key-id time ver)))
       ((eq type 'enc)
        (setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
              key-id    (ti::mail-pgp-stream-data-elt data 'key-id))
        (setq ret (format "Encrypted to 0x%s [v%s.x]" key-id ver)))
       ((eq type 'pring)
        (setq ver       (ti::mail-pgp-stream-data-elt data 'ver)
              time      (ti::mail-pgp-stream-data-elt data 'time))
        (setq ret (format "Public key %s [v%s.x]" time ver)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-stream-data-elt (data elt)
  "Study DATA and Return packet ELT.
DATA must be in the format of `ti::mail-pgp-stream-forward-and-study'
ELT  can be 'ver 'time 'key-id"
  (let* ((type (car data))
         (pos  (assq
                elt
                (nth
                 1
                 (assq type
                       '(
                         (signed ((ver 0) (time 3) (key-id 5)))
                         (pring  ((ver 0) (time 1) (key-id 4)))
                         (enc    ((ver 0) (key-id 2))) ;No TIME field
                         ;; #todo, not ready
                         (base64 ((ver 0) (time 3) (key-id 5)))
                         (crypt  ((ver 0) (time 3) (key-id 5)))))))))
    (if (null pos)
        (error "Wrong specification %s %s %s" type elt data)
      (nth (nth 1 pos) (cdr data)))))

;;; Test suite with live data: first ASCII armor bytes
;;
;; (setq list (ti::mail-pgp-data-ascii-armor-convert "hEwDwLrt1UcUHTUBA"))
;; (setq ctb  (ti::mail-pgp-data-study-ctb-byte (car list)))
;; (setq data (ti::mail-pgp-stream-study ctb list))

;; Sig data
;; (setq s "iQBVAwUBM55fkcC67dVHFB01AQGnHwIAqe2OfkdcnQviGzCmy3KddnsE8uFkAeaV")
;;
;; (setq list (ti::mail-pgp-data-ascii-armor-convert s))
;; (setq ctb  (ti::mail-pgp-data-study-ctb-byte (car list)))
;; (setq data (ti::mail-pgp-stream-study ctb list))

;;}}}
;;{{{ PGP key info

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-id-lines-in-region (beg end)
  "Search all lines in BEG END matching pgp -kvc and -kx lines.

Option -kvc

  pub  1024/01234567 1997/05/01 Mar Bar <Bar@bar.com>

Option -kx

  Key for user ID: Mr. Bar <bar@bar.com>
  1024-bit key, key ID 01234567, created 1997/05/01

And return list of those lines."
  (let ((l1
         (ti::buffer-grep-lines
          "pub[ \t:]+[0-9]+/[A-Z0-9]+[ \t:]+.*/.*/[0-9]" beg end))
        (l2 (ti::buffer-grep-lines "[0-9]-bit key, Key ID " beg end)))
    (cond
     ((and l1 l2) (ti::list-merge-elements l1 l2))
     (l1 l1)
     (l2 l2))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-id-0x-lines-in-region (beg end)
  "Call `ti::mail-pgpk-id-lines-in-region' on BEG END, return only Key HEX ids."
  (let* (ret)
    (dolist (line (ti::mail-pgpk-id-lines-in-region beg end))
      (when (stringp line)
        (push
         (or
          (ti::string-match "pub[ \t]+[0-9]+/\\([^ \t]+\\)" 1 line)
          (ti::string-match "Key ID \\([0-9A-F]+\\)" 1 line))
         ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpk-public-get-region (&optional beg end buffer relax)
  "Get all keys in BEG END from BUFFER and build list of key data.
The blocks searched are in following format.

  Key for user ID: Mr. Foo <foo@example.com>
  512-bit key, key ID 123456789, created 1996/06/03
  Also known as: Mr Foo <bar@bar.com>

  -----BEGIN PGP PUBLIC KEY BLOCK-----
  [...]
  -----END PGP PUBLIC KEY BLOCK-----

Note1:

  If there _no_ 'Key for user ID:' string in the buffer, this function
  can't find the public key block while it may be there. It is
  assumed that each p-key block is _preceded_ by that string.

  All anonymous p-key block are skipped.

Note2:

  If there are two sequential key-id strings, like
  Key for user ID:       <<A
  Key for user ID:       <<B
  -----BEGIN PGP PUBLIC KEY BLOCK-----

  The p-key block in the list for A will be nil.

Note3:

   If RELAX argument is non-nil, then the 'Key for user ID:'
   must not exit. Only the Public key tags are searched.

   Recommended way of informing public keys is however displaying
   full public key information and not just PK block

Return:

  '((KEY-ID-STRING PUBLIC-KEY_BLOCK)"
  (let ((opt  (if relax 'pkey 'kid))
        id
        block
        region
        ret
        max)
    (with-current-buffer (or buffer (current-buffer))
      (ti::narrow-safe (or beg (point-min)) (or end (point-max))
        (ti::pmin)
        (while (ti::mail-pgp-re-search opt 'move)
          (setq id (ti::read-current-line))

          ;;  If there are two
          ;;    Key for user ID:
          ;;    Key for user ID:
          ;;
          ;;  And there is no public key between these two, set the
          ;;  search limit to stop to next Key-id line.
          (setq max
                (save-excursion
                  (end-of-line)
                  (setq max (ti::mail-pgp-re-search 'kid))))
;;;       (ti::d! ">>" id ">>" max (ti::mail-pgp-block-area 'pkey nil max))
          (cond
           ((setq region (ti::mail-pgp-block-area 'pkey nil max))
            (setq block (buffer-substring (car region) (cdr region)))
            (goto-char (cdr region)))
           (t
            ;; Continue search
            (end-of-line)))
          (push (list id block) ret)
          (setq id nil   block nil))))
    (nreverse ret)))

;;}}}
;;{{{ PGP signature info

;;; ................................................... &pgp-signature ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-remove (&optional add no-cnv)
  "Remove PGP signature (and headers that have included in there).
Below, only lines 7 and 8 are left in buffer.

  1   -----BEGIN PGP SIGNED MESSAGE----
  2
  3   ##                            << Header start mark
  4   Header1: content
  5   Header2: Content
  6                                 << space here
  7   test
  8
  9   -----BEGIN PGP SIGNATURE-----

With ADD flag

  The tag lines are reassembled and point sits at the beginning of line 6
  and whitespaces around (email) buffer text are deleted.
  If tag lines are found while ADD, this function does nothing.

With NO-CNV

  When removing signature, do not convert '- -' back into '-'.
  Eg. If message is encrypted and signed; it is not desirable to
  do this conversion if you just want to strip out the signature to Xpgp.
  The '- -' lines must stay there."
  (let* ((beg (save-excursion (ti::pmin) (ti::mail-pgp-re-search))))
    (cond
     ((and add (null beg))
      (ti::mail-trim-buffer)
      (ti::mail-text-start 'move)
      (insert (ti::mail-pgp-signed-begin-line) "\n\n")
      (ti::pmax)
      (insert "\n" (ti::mail-pgp-signature-begin-line) "\n"))
     ((null add)
      ;;  there is one thing to fix first, PGP converts lines that have
      ;;  double '--' at front
      ;;
      ;;                --text follows
      ;;        -->
      ;;                - --text follows
      ;;
      ;;        Let's correct those lines too.
      (when (null no-cnv)
        (save-excursion (ti::buffer-replace-regexp "^- -" 0 "-")))
      (when beg                         ;Thre is regular PGP sig
        ;;  note: We don't trim BODY here, we only remove
        ;;  the pgp tag lines. The receiving end should do
        ;;  the trimming. (we save one function call)
        (goto-char beg)                         ;One newline at beg
        (ti::buffer-kill-line 'del 2)           ;TWO lines; important
        ;;  Kill included headers
        (when (and (looking-at "##\n.*: ")
                   (re-search-forward "^$" nil t))
          (delete-region beg (1+ (point))))

        (when (and (prog1 (setq beg (ti::mail-pgp-re-search 'sig 'move))
                     (ti::buffer-kill-line))
                   (ti::mail-pgp-re-search 'sige 'move))
          (forward-line 1)
          (delete-region beg (point))))))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-pgp-signature-normal-do-region 'lisp-indent-function 0)
(put 'ti::mail-pgp-signature-normal-do-region 'edebug-form-spec '(body))
(defmacro ti::mail-pgp-signature-normal-do-region (&rest body)
  "Execute BODY and calculate pgp signature region.
In the macro you can use following variables:

  `limits'      (area-beg . area-end)
  `area-beg'
  `area-and'

This macro does nothing if there is no normal PGP signature."
  `(let (limits
         area-beg
         area-end)
     (setq limits (ti::mail-pgp-block-area 'sig))
     (when limits
       ;;   Set values
       (setq area-beg (car limits)
             area-end (cdr limits))
       ;;  If these are no used in BODY: no-op Quiet XE ByteCompiler
       (if (null area-beg)  (setq area-beg nil))
       (if (null area-end)  (setq area-end nil))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-get-article-buffer ()
  "Do `set-buffer' to *Article* if it exists. Return nil if no buffer."
  (if (boundp 'gnus-article-buffer)
      (symbol-value 'gnus-article-buffer)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-with-article-buffer 'lisp-indent-function 0)
(put 'ti::mail-with-article-buffer 'edebug-form-spec '(body))
(defmacro ti::mail-with-article-buffer (&rest body)
  "Run BODY in *Article* buffer if it exists."
  `(let* ((buffer  (ti::mail-get-article-buffer)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-normal-info ()
  "Return signature information from normal PGP format.
Return:
 ((beg . end) (fld fld ..) (signarure-data sig ..))"
  (let (sig-list
        info-list
        ret)
    (ti::mail-pgp-signature-normal-do-region
     (save-excursion
       (goto-char area-beg)
       (forward-line 1)
       ;;  Here are the comments and other PGP headers
       (while (looking-at "^[^ \t]+:+ .*")
         (ti::nconc info-list (ti::read-current-line))
         (forward-line 1))
       ;; Here is the signature itself
       (while (not (>= (point) (cdr limits)))
         (if (looking-at "^[^ \t\n]+$")
             (ti::nconc sig-list (match-string 0)))
         (forward-line 1))
       (setq ret (list limits info-list sig-list))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-sig-header-info-v2xx ()
  "Return signature information from X-pgp v2.xx headers.

Reads format:

X-Pgp-Comment: Processed by TinyPgp.el 1.56
X-Pgp-Version: 2.6.3ia
X-Pgp-Charset: noconv
X-Pgp-Signed:
        iQBVAwUBMoijBMC67dVHFB01AQGf3QH/dmgc47fx1tvHYPcuKWIz0Fe7HnWXmd63
        3IBA6vhSqzbUT4nkKL2QJQX/0Z8I9dkmOahSQNKvU/7qsB9Iw8JwpQ==
        =9yu9

Return:
 ((beg . end) (fld fld ..) (signature-data sig ..))"
  (let* ((case-fold-search t)
         (pbase         "X-Pgp-")
         (p-re          (concat "^" pbase)) ;pgp regexp for hdrs
         (psig          (concat p-re "Signed:"))
         (fld-re        (concat
                         p-re
                         "\\(Version:\\|Charset:\\|Comment:\\|Signed:\\)"))
         (hmax          (ti::mail-hmax))
         val
         sig-list
         info-list
         beg
         end
         ret)
    (save-excursion
      (ti::pmin)
      (while (and
              hmax
              (< (point) hmax)    ;those fwl-line calls may go past...
              (re-search-forward fld-re hmax t))
        (beginning-of-line)
        (if (null beg)                  ;record it NOW
            (setq beg (point)))
        (cond
         ((looking-at (concat psig "[ \t]*\\([^ \t\n]*\\)"))
          ;;  Is this the signature itself ? Special handling,
          ;;  because spreads multiple lines.
          (setq val (ti::remove-properties (match-string 1)))
          (if (not (string= "" val))
              (ti::nconc sig-list val))
          (forward-line 1)
          (while (looking-at "^[ \t]+\\([^ \t\n]+\\)")
            (ti::nconc sig-list (ti::remove-properties (match-string 1)))
            (forward-line 1)))
         ;; Nope, some additional PGP header
         (t
          (ti::nconc info-list (ti::remove-properties (ti::read-current-line)))))
        ;;  Because there is already one while loop that says fwd-line,
        ;;  we don't want to go furher if it stopped us.
        (if (looking-at (concat p-re  "\\|^\t+"))
            (forward-line 1)))
      (beginning-of-line)
      (setq end (point)))

    (if sig-list
        (setq ret (list (cons beg end) info-list sig-list)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-header-info-v3xx ()
  "Return signature information from X-pgp v3.xx headers.

Return:
 '((nil . nil)
   (\"Version: x.x.x\" \"Charset: xxxx\" ...)
   (signature-string sig-string ..))"
  (let ((field  (ti::remove-properties (ti::mail-get-field "X-Pgp-signed")))
        info-list
        sig-list
        elt
        list)
    (when field
      (setq list (ti::mail-mime-parse-header field 'downcase))
      ;;  Push adds to the front of list, so beware order of elements
      (if (setq elt (assoc "signature" list))
          (setq sig-list  (cdr elt)))
      (if (setq elt (assoc "comment" list))
          (push (concat "Comment: " (car (cdr elt))) info-list))
      (if (setq elt (assoc "charset" list))
          (push (concat "Charset: " (car (cdr elt))) info-list))
      (if (setq elt (assoc "version" list))
          (push  (concat "Version: " (car (cdr elt))) info-list  ))
      (if info-list
          (list (cons nil nil) info-list sig-list)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-signature-header-info ()
  "Return X-pgp header info if X-Pgp header exist."
  (if (ti::mail-pgp-v3xx-p)
      (ti::mail-pgp-signature-header-info-v3xx)
    (ti::mail-pgp-sig-header-info-v2xx)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-parse-header (header-string &optional downcase)
  "Parse Variable=value HEADER-STRING like and optionally DOWNCASE keywords.

Header-this: var1=value2; var2= val2; var3=\"starts here \"
  \" continues here\"; var4= v1,v2,v3;

The VAL returned is different for continued string. It is a list of
individual parts in the parsed. In this case the whole returned value
would be:

'((var1 . (val1))
  (var2 . (val2))
  (var3 . (\"starts here \" \" continues here\"))
  (var4 . (\" v1,v2,v3\")))

Return:
  ((var . VAL) (var . VAL) ..)"
  (let ((tag-re  "^[ \t]*\\([^ \t\n]+\\)[ \t\"]*=")
        (val-re  "[ \t\"]*\\([^\n\"]+\\)")
        (buffer  (ti::temp-buffer "*tmp*" 'clear))
        name
        val
        ret)
    (with-current-buffer buffer
      (insert header-string)
      ;; put into same line
      (ti::pmin) (ti::buffer-replace-regexp "[ \t]*;[ \t]*" 0 "\n")
      ;; Okay now it's in canonical format. First
      ;; pick up signature, then delete it and parse other fields.
      ;;  Version=2.6.3ia
      ;;  Charset=noconv
      (ti::pmin)
      (while (re-search-forward tag-re nil t)
        (setq name (match-string 1)   val nil)
        (cond
         ((looking-at val-re)           ;VALUE at the same line
          (ti::nconc val (match-string 1))
          (forward-line 1))
         (t
          ;;  Multiline
          (while (progn (forward-line 1)
                        (looking-at val-re))
            (ti::nconc val (match-string 1)))))
        (if downcase
            (setq name (downcase name)))
        (push (cons name val) ret)))
    (nreverse ret)))

;;}}}
;;{{{ PGP public key

;;; ........................................................ &pgp-pkey ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgp-pkey-read (&optional raw kill-file)
  "Read public key block from current point forward. Point is moved.

Input:

  RAW           If non-nil, return only raw public key block.
  KILL-FILE     if non-nil, kill temporary file after statement
                'Key extracted to file ...' Once the file is killed the
                message will be removed from buffer."
  (let* (beg
         end
         file
         ret)
    ;;  No temp files are left on disk
    ;;  Remove also the file message from buffer before we read the
    ;;  content.
    ;;
    ;;       Extracting from key ring: '/users/xxx/.pgp/pubring.pgp',\
    ;;       userid "xxx".
    ;;
    ;;       Key for user ID: <xxx@some.fi>
    ;;       512-bit key, key ID 8125CAAA, created 1997/06/05
    ;;
    ;;       -----BEGIN PGP PUBLIC KEY BLOCK-----
    (when (and kill-file
               (re-search-forward "Key extracted to file.*'\\(.*\\)'" nil t))
      (setq file (match-string 1))
      (ti::buffer-kill-line)
      (ti::file-delete-safe file))
    (goto-char (point))
    (when (re-search-forward (ti::mail-pgp-pkey-begin-line) nil t)
      (re-search-backward "Key for user ID:") (beginning-of-line)
      (when raw
        (re-search-forward "^-----BEGIN")
        (beginning-of-line))
      (setq beg (point))
      (when (ti::mail-pgp-re-search 'pkeye 'move)
        (forward-line 1)
        (setq end (point)))
      (when (and beg end)
        (setq ret (buffer-substring beg end))))
    ret))

;;}}}
;;{{{ PGP remail

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-close ()
  "Close reply block by adding '**' to the end.
If there already is '**', do nothing."
  (save-excursion
    (ti::pmax)
    ;;  Remailers need "**" at the end of encrypted block
    (if (not (re-search-backward "^\\*\\*" nil t))
        (insert "\n**\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-anonymize-headers (mode &optional no-ins arg1 arg2 hash)
  "Destroy header information according to mode and move it to message body.
This function does nothing if first line is not header.

Input:

  MODE       'move-to-body moves, all headers to body
             'move-to-body-maybe, all headers to body only if
             there is not already hash marks.
             arg1 is used for subject       defaults to 'dummy'
             arg2 is used for organisation  defaults to 'dummy'

  NO-INS     Do not insert the hash headers into body, but return them
             as list instead.

  ARG1 ARG2  used by MODE

  HASH       Use hash marks string other that \"##\"

Return:

  list"
  (let ((hlist  '("In-reply-to"
                  "Organization"
                  "Subject"))
        (empty  " dummy")
        (full-string "")
        done
        ptr
        list
        str
        ret)
    (setq hash (or hash "##")
          arg1 (or arg1 empty)
          arg2 (or arg2 empty))
    (save-excursion
      (when (ti::mail-mail-p)
        (cond
         ((memq mode '(move-to-body move-to-body-maybe))
          ;;  First check if hash mark is already there
          ;;  If mode is "maybe" we don't add new headers.
          ;;
          ;;  The regexp matches to the end of line, because you may have
          ;;  quoted the message
          ;;
          ;;  jerry>  ##
          ;;  jerry>  Subject:  this here
          (ti::pmin)
          (unless (and (eq mode 'move-to-body-maybe)
                       (re-search-forward (concat hash "[ \t]*$") nil t))
            (setq ptr hlist)
            (dolist (elt ptr)
              (setq str (ti::mail-get-field elt))
              (when (and str (not (string= empty str)))
                (setq elt (format "%s: %s\n" elt str))
                (push elt list)
                ;;  so that we can match against this later
                ;;
                (setq full-string (concat full-string elt))))
            (ti::mail-text-start 'move)
            (when list
              (setq ret list  done t)
              (unless no-ins
                ;;  Remailer hash mark
                (insert hash "\n"))))
          ;;  Anonymize some headers
          (if arg1
              (ti::mail-kill-field "^subject"  arg1))
          (if arg2
              (ti::mail-kill-field "^organization" arg2))
          (when (and done (null no-ins))
            (dolist (elt list)
              ;;  Copy headers inside message
              (insert elt))))
         (t
          (error "Invalid mode [%s]" mode)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-reply-type (property-string)
  "Return remailer reply block type from PROPERTY-STRING.
The 'post' type is not checked, because it relates to Usenet
and can be mixed with other types."
  (if (string-match "cpunk\\|eric\\|penet" property-string)
      (match-string 0 property-string)))

;;; ----------------------------------------------------------------------
;;; used to be: cpunk   Request-Remailing-To
;;; but nowadays instructions say "Anon-To"
;;;
(defun ti::mail-pgpr-block (mode &optional type email key latent)
  "Return remailer header string defined by mode.
be sure to have <> in the email, which defaults to `user-mail-address'.

Input:

  MODE      'epgp -- return encrypted pgp tag.
            'post -- return simple Newsgroup post block. 'email'
            contains the address of post remailer.
            If there is not enough
            parameters, say for 'tk, the previous one is used: 't

  TYPE      cpunk   Anon-To
            eric    Anon-Send-To
            penet   X-Anon-To
            post    Anon-Post-To Usenet

  EMAIL     Parameter for type
  KEY       Parameter for type
  LATENT    Parameter for type"
  (let* ((reply
          (cond
           ((string= type "cpunk")  "Anon-To")
           ((string= type "eric")   "Anon-To")
           ((string= type "penet")  "X-Anon-To")
           ((string= type "post")   "Anon-Post-To")
           ((memq mode '(epgp post)))   ;Ok; skip
           ((error "Unknown type '%s'" type)))))
    (setq email (or email user-mail-address))
    (cond
     ((equal mode 'epgp)
      "::\nEncrypted: PGP\n\n")
     ((equal mode 'post)
      (concat
       "::\n"
       "Anon-Post-To: " (or email (error "invalid args.")) "\n"
       "Cutmarks: --\n"))
     ((and (stringp email) (stringp key) (stringp latent))
      (format "::\n%s: %s\nEncrypt-Key: %s\nLatent-Time: %s\n"
              reply email key latent))
     ((and (stringp email) (stringp latent))
      (format "::\n%s: %s\nLatent-Time: %s\n" reply email latent))
     ((and (stringp email) (stringp key))
      (format "::\n%s: %s\nEncrypt-Key: %s\n" reply email key))
     ((or (stringp email))
      (format "::\n%s: %s\n" reply email))
     (t
      (error "Wrong args '%s' '%s'" mode type )))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-reply-block (pgp-email)
  "Return reply block header.
Should be inserted just before PGP crypted message to PGP-EMAIL."
  (format "Reply-Block:\n::\nAnon-To: %s\n\n" pgp-email))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-pgpr-parse-levien-list (&optional buffer control-list)
  "Parse remailer list finger <remailer-list@kiwi.cs.berkeley.edu>.
The parsing starts from current point forward.

Input:

  BUFFER            defaults to current buffer
  CONTROL-LIST      '(remailer-alias  (prop-no-list) [(prop-add-list)])
                    This control list says 'if REGEXP matches the
                    email address, remove all properties listed in
                    prop-no-list and add all properties listed in
                    prop-add-list.

                    So, if you're sure that the levien-list has some
                    faulty entries, e.g. say remailer@replay.com doesn't
                    have feature 'ek' although levien list contains that,
                    your control-list is like this. The ek property
                    is removed even if the list says otherwise.

                    '(\"replay\" '(\"ek\"))

Return:

  '((alias remailer property_string (property property ...))
    (alias remailer property_string (p p p ..)))

  The properties are sorted: cpunk mix pgp..."
  (let ((re  (concat
              "^[ \t]*$remailer{[\"']\\(.*\\)[\"']}.*=[ \t]*[\"']"
              "<\\(.*\\)>[ \t]+\\(.*\\)[\"']"))
        a
        r
        p
        blocks
        ret
        elem
        list)
    ;;  The list is in Perl hash array format in case you're interested...
    (with-current-buffer (or buffer (current-buffer))
      (while (re-search-forward re nil t)
        (setq a (match-string 1)
              r (match-string 2)
              p (match-string 3))
        (setq blocks (split-string p))
        (setq blocks (sort blocks 'string<))
        (when (and control-list
                   (setq elem (assoc a control-list)))
          (setq list (nth 1 elem))
          (dolist (elt list)
            (setq blocks (delete elt blocks)))
          (setq list (nth 2 elem))
          (dolist (elt list) (push elt blocks))
          ;;  We used this now, remove from list
          (setq control-list (delete elem control-list))
          (setq p            (mapconcat 'concat blocks " ")))
        ;; features In alphabetic order
        (setq p (mapconcat 'concat blocks " "))
        (push (list a r p blocks) ret)))
    ret))

;;}}}

;;{{{ email addresses

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-make-anti-spam-address (email)
  "Make an anti-spam address from EMAIL."
  (let* ((add [ "uce"
                "ube"
                "spam"
                "commercials"
                "advertisements"
                "ads"
                "junk"
                "garbage"
                ])
         (base  ["no"
                 "stop"
                 "die"
                 "hang"
                 "anti.address"
                 "yuck"
                 "dislike"
                 "go-away"
                 "stay-away"
                 "delete"
                 "nothanks"
                 "erase"
                 "zap-this"
                 "wipe-this"
                 "exterminate"
                 "ignore"
                 "bypass"
                 "keep-out"
                 "keep-away"
                 "none"
                 "nada"
                 "zero"
                 "not-any"
                 "zelt"
                 "no-thank-you"
                 "remove-this"
                 "rip-off-this"
                 "disregard"
                 "throw-away"
                 ])
         (vec  (vector
                (concat
                 (elt (shuffle-vector base ) 1)
                 "-"
                 (elt (shuffle-vector add) 1))
                (concat
                 (elt (shuffle-vector add) 1)
                 "-"
                 (elt (shuffle-vector base ) 1))))
         (this (elt (shuffle-vector vec) 0))
         login
         domain)
    (string-match "\\(.*\\)@\\(.*\\)"  email)
    (setq login  (match-string 1 email)
          domain (match-string 2 email))
    (format  "%s%s%s"
             login
             (if (zerop (randij 0 1))
                 (concat "." this "@")
               (concat "@" this "."))
             domain)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-domain (string)
  "Return only the top level domain name from email STRING.
xx.yy..domain.com --> domain.com
xx.domain.co.uk   --> domain.co.uk"
  (cond
   ;;  This match tries to catch those domains that don't have 3 parts,
   ;;
   ;;      aa.bb.co.uk
   ;;            |
   ;;            We expect this part to be longer than 2 characters

   ((string-match "[^.][^.][^.]+\\.\\(..\\|...\\)$" string)
    (match-string 0 string))
   ;;  This is domain that requires 3 parts: co.uk or au jp
   ((string-match "[^.]+\\.[^.]+\\.\\(..\\|...\\)$" string)
    (match-string 0 string))
   ((string-match "[^@]+$" string)
    (match-string 0 string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-domain-canonilize (list)
  "Canonilize list of addresses to top level domain names only
Eg: '(\"aa.foo.com\" \"bb.foo.com\") --> '(\"foo.com\")"
  (let* (ret
         domain)
    (dolist (elt list)
      (setq domain (ti::mail-email-domain elt))
      (add-to-list 'ret domain))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-find-region (&optional beg end no-dupes)
  "Find all email addresses within region BEG END (defaults to buffer).
The email addresses must contain @. Surrounding <> characters are removed.

Input:

  BEG       region start; defaults to `point-min'
  END       region end; defaults to `point-min'
  NO-DUPES  flag; if non-nil then cache only unique entries."
  (let (list
        elt)
    (save-excursion
      (setq beg (or beg (point-min))
            end (or end (point-max)))
      (ti::keep-lower-order beg end)
      (goto-char beg)
      ;;  Intangible text property case:
      ;;  - When you do a limited search and cursor land somewhere in
      ;;    intangible char, it immediately slides to next char
      ;;    position. Like if you'd do
      ;;
      ;;    (progn (goto-char 10) (point))
      ;;    --> 20
      ;;
      ;;    This is not suprise, if point 10 had intangible text until
      ;;    19th pos. If there were no intangible text in point 10,
      ;;    the result would be expected 10.
      (while (and (<= (point) end) ;; intangible test
                  (re-search-forward
                   "[^ '\",:<\t\n(]+@[^ '\">:,\t\n)]+"
                   end
                   t))
        (setq elt (ti::remove-properties (match-string 0)))

        (if (and (stringp elt)
                 (or (or (null no-dupes)
                         (not (member elt list)))))
            (push elt list)))
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-email-from-string (string)
  "Return list of email addresses from STRING.
The addresses must have @ character. Surrounding <> characters are removed.
If STRING is nil this function does nothing."
  ;; Using buffer is faster that reading string
  (when string
    (with-temp-buffer
      (insert string)
      (ti::mail-email-find-region))))

;;}}}
;;{{{ parsing

;;; ......................................................... &parsing ...

;;; ----------------------------------------------------------------------
;;;   (ti::mail-test-parse-name)
;;;
(defun ti::mail-test-parse-name ()
  "This is a test function, do not call from programs.

Because the `ti::mail-parse-name' is quite complicated,
and slightest modification may render it, this functions tests
that the old functionality is preserved in spite of changes."
  (let (list
        e1
        e2
        stat
        ptr)
    (setq list
          '("Dr. Foo Bar <foo@bar.com>"
	    "<jdoe@examole.com> (Finland, pgp id 512/47141D35)"
            "(Rune Juntti[FRONTEC Pajala]) <jdoe@example.se>"
            "shahramn@wv.mentorg.com (jdoe@example.com)"
            "(jdoe@example.com)"
            "Jerome Santini <doe@this-example.here.com>"
            "jdoe@example.com (Harry Halladay - EDS St. Louis)"
            "jdoe@example.com (Ake Stenhoff TM/PMD 83442 3003)"
            "CEO-executive this here jdoe@example.com"
            "JDOE <\"VAX::SOME@example.com\""
            "\"VAX::LOGIN\"@example.com"
            "john.doe@example.com"
            "John=Doe%aoa.rdt%OS.DC@example.com"
            "jdoe@example.com (John Doe)"
            "\"/G=Name/S=Surname/OU=comlab/O=oxford/PRMD=UK.AC/ADMD= /C=GB/\"@example.fi\""
            "\"wayne (w.d.) bell\" <jdoe@example>"
            "John doe <example@example.com>"
            "\"Joseph B. Ottinger\" <j.doe@example.com>"
            "\"Name Foo puh. 111 600\" <LOGIN@example.com>"
            "\"stephane (s.) boucher\" <jdoe@example.com>"
            "jdoe@example.com (J.D \"John\" Doe)"
            "jd@example-com (J.D doe)"
            "doe@example.com \(John Doe\)"
            "jdoe@example.com \(John D. Doe\)"
            "\"J. doe Ph.d \" jdoe@john.doe.example.com"
            "\"John D. Doe\" <foo@example.com>"))
    (setq ptr list)
    (dolist (n ptr)
      (setq stat  (ti::mail-parse-name n))
      (setq e1 (nth 0 stat)) (setq e2 (nth 1 stat))
      (read-from-minibuffer (concat "TEST>>" e1 "," e2 "<")))))

;;; ----------------------------------------------------------------------
;;; ( ti::mail-parse-name "\"Dr. Volker Zell\" <dr.volker.zell-QHcLZuEGTsvQT0dZR+AlfA@public.gmane.org>")
(defun ti::mail-parse-name (line)
  "Try to parse various formats of 'From:' fields.
Supposes that the 'From:' keyword is removed from the LINE.

Return:
  list          '(firstname surname)
  nil           if cannot parse both"
  (let* ((re-ignore "\\(?:Dr. +\\|Mr. +\\)?")

	 (re-A          "[-a-zA-Z0-9.{|]")
         (re-AG         (concat "\\("  re-A "+\\)"))

         ;;  'From: Mr-CEO John Doe <jdoe@example.com'
         (fs-re2  (concat re-ignore re-AG " +" re-AG))

         ;;  'USER <\"CLUSTER::VAX\@site.cm\"'
         (fs-vax  (concat "^" re-AG "[ \t<\"]+[A-Z]+::" re-AG))

         ;;  '\"CLUSTER::LOGIN\"@example.com'
         ;;  This is incomplete Name, it does not contain NAMES at all, but
         ;;  we consider mail name as surname. The first group-RE is dummy.
         (fs-vax2 (concat re-AG "::" re-AG))

         ;;  'Job.Ganzevoort@cwi.nl', where person's name is complete
         ;;  address
         (fs-fse    (concat re-AG "\\." re-AG "@" ))

         ;;  matches gateway-type addresses
         ;;  'Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU'
         (gtw-re1    (concat re-AG "=" re-AG "%" ))

         (q-no-re   ti:mail-parse-name-not-accept)

         (mail      (or (ti::mail-parse-email line) ""))
         (account   (if (= 2 (length mail))
                        (nth 0 mail)
                      "#@$@#$@#$"))     ;just some dummy

         fn
         sn                             ;first, surname
         pick
         w
         w1
         w2
         D                              ;debug
         beg
         end
         beg1
         end1
         beg2
         end2
         tmp
         list)

    (if D
        (setq D D))                 ;XE 19.14 ByteComp silencer, no-op

    (catch 'found

      ;;  It's most important that the match test are made IN THIS ORDER
      ;;  - Quote test cannot precede vax name test.
      ;;  - Try most restrictive first.

      ;; ..............................................................
      ;;  VAX is identified by "::" marks

      (when (string-match "::" line)
        (setq list (ti::mail-get-2re fs-vax line))
        (when (not (string= "" (nth 0 list)))
          (setq D "vax1")
          (throw 'found t))
        (setq list (ti::mail-get-2re fs-vax2 line))
        (when (not (string= "" (nth 0 list)))
          (setq D "vax2")
          (throw 'found t)))

      ;; ............................................................
      ;; Try gateway addresses, rare, but seen in net still

      (when (string-match "%" line)
        (setq list (ti::mail-get-2re gtw-re1 line))
        (when (not (string= "" (nth 0 list)))
          (setq D "gtw1")
          (throw 'found t)))

      ;; X.400 address

      (when (string-match "/G=\\(.*\\)/S=\\([^/]+\\).*C=" line)
        (setq fn (match-string 1 line)
              sn (match-string 2 line))
        (when (and fn sn)
          (setq list (list fn sn)   D "gateX400")
          (throw 'found t)))

      ;; .................................................................
      ;; foo.bar@example.com

      (when (string-match fs-fse line)
        (setq list (ti::mail-get-2re fs-fse line))
        (when (not (string= "" (nth 0 list)))
          (setq D "mike.gordon")
          (throw 'found t)))

      ;; ............................................................
      ;; And the rest , is there paren or ""  somewhere ?
      ;;

      ;;  If this is a full email string Joe@foo.com
      ;;  then get only the first part.

      (when (and (setq tmp (ti::string-match "^\\([^ \t]+\\)[@%][^ \t]+$" 1 line))
                 (setq tmp (ti::string-match re-AG 1 tmp)))
        (setq D "email")
        (setq list (list tmp ""))
        (throw 'found t))

      ;;   - if we get multiple match "stephane (s.) boucher" ,
      ;;     (L.G. \"Ted\" Stern) , pick the one that's longer.

      (if (string-match "\"\\(.*\\)\"" line)
          (setq beg1 (match-beginning 1)  end1  (match-end 1)))

      (if (string-match "[(]\\(.*\\)[)]" line)
          (setq beg2 (match-beginning 1)  end2  (match-end 1)))

      (cond
       ((and beg1 beg2)
        (if (> (- end1 beg1) (- end2 beg2))
            (setq beg beg1  end end1)
          (setq beg beg2  end end2)))
       (beg1
        (setq beg beg1  end end1))
       (beg2
        (setq beg beg2  end end2)))

      ;; ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...  ...

      (cond
       (beg
        ;;   - Get list of words into W
        ;;   - Someone wrote M. "Mack" Monroe, so the " is included
        ;;     in words separate list
        ;;   - The latter picks only NON-ABBREVIATED names, non-phones..
        ;;     M. "Mack" Monroe --> Mack Monroe
        ;;

        (setq pick (substring line beg end))
        (setq w (split-string pick "[][@%. \"]+"))

        (setq D "standard")
;;;     (ti::d! "w-1" w)

        (let ((case-fold-search nil))   ;case is important !!
          (setq w                       ;returned word list
                (ti::list-find
                 w
                 q-no-re
                 (function
                  (lambda (arg elt)
                    (not (string-match arg elt))))
                 'all-items)))

;;;     (ti::d! "w-2" w)

        (cond
         ((> (length w) 2)              ;too much abbrev names
          ;;  pick first and account or last word

;;;       (setq W w AC account)

          (setq w1 (nth 0 w)  w2 (nth (1-(length w)) w)  )

          (setq tmp (ti::list-find
                     w account
                     (function
                      (lambda (arg elt)
                        (string-match elt arg)))))

          (if tmp                       ;account name found
              (setq w2 tmp))

          (setq list (list w1 w2)))

         ((= 2 (length w))
          (setq w1 (nth 0 w)  w2 (nth 1 w))
          (setq list (list w1 w2)))

         ((eq 1 (length w))
          (setq list w))

         (t
          nil))

        (if list
            (throw 'found t))))

      ;; .................................................................

      (setq list (ti::mail-get-2re fs-re2 line))
      (when (not (string= "" (nth 0 list)))
        (setq D "2.1")
        (throw 'found t))) ;; Catch end

;;;    (ti::d! "parsed" D  list)

    ;;   what should we return ?
    (if (and (string= (nth 0 list) "")
             (string= (nth 1 list) ""))
        nil
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-email (line)
  "Try to parse various formats of 'From:' field from LINE.
Input is full LINE, possibly containing 'From' keyword.

Return:

  list          '(usrname site)
  nil           if cannot parse."
  (let* (account
         site
         tmp

         ;; '.' is for firstname & surname combination
         ;; '=' is for gateway form
         ;; '|{' are scandinavian characters in name
         ;; '+' Believe or not, but I just saw account name like
         ;;     "Stephen M. Lacy" <sl31+@andrew.cmu.edu>

         (A "[-a-zA-Z|{0-9_=.+]+")      ; alphabet
         (As "[-a-zA-Z0-9.%]+")         ; site name

         ;;  Note that username can have scandinavian {| marks
         ;;  Normal site name
         ;;  o   Simon.Marshall@mail.bar.foo.fi (Simon Marshall)
         (re1 (concat "\\(" A "\\)@\\(" As "\\)"  ))

         ;;  Marla=Bush%aoa.rdt%OS.DC@Ban-Gate.AoA.DHHS.EDU
         (re2 (concat "\\(" A "\\)\\(%" As "\\)"  ))

         ;;  VAX address <"TNCLUS::TSYVANEN"@mailer.foo.fi>
         (re-vax (concat "\\(\"" A "::" A "\"\\)@\\(" As "\\)"  ))
         em                             ; email

         ;;  "/G=Jamie/S=Lokier/OU=comlab/O=oxford/PRMD=UK.AC...
         (re-x400
          (concat "/G=\\([^/]+\\)/S=\\([^/]+\\)" ;fn sn
                  "/OU=\\([^/]+\\)/O=\\([^/]+\\)"
                  "/PRMD=\\([^/]+\\)")))
    (catch 'found
;;;      (setq LINE line RE re-x400)

      (if (null (string-match re-x400 line)) nil
        (setq account (concat (match-string 1 line) "." (match-string 2 line)))
        (setq site    (concat (match-string 3 line) "." (match-string 4 line)))

        ;;  Now switch the last items PRMD=UK.AC --> ac.uk
        (setq tmp (match-string 5 line))
        (setq tmp (split-string tmp "[.]"))
        (setq site (downcase (concat site "." (nth 1 tmp) "." (nth 0 tmp))))
        (setq em (list account site))
        (throw 'found t))

      (setq em (ti::mail-get-2re re-x400 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re1 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re2 line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t))

      (setq em (ti::mail-get-2re re-vax line))
      (if (not (string= "" (nth 0 em)))           (throw 'found t)))

    (if (< (length (nth 0 em)) 1)
        (setq em nil))
    em))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-regexp-list ()
  "Return list of regexps that match `Received:' header content.
The Return ed list content is

'((3  (re re re ..))
  (2  (re re re ..))
  (1  (re re re ..)))

Where the number indicated how many submatches can be read. E.g. Number
3 means, 3 submatches."
  (let* ((from  "[ \t]+from")
         (spc   "[ \t\r\n]*")
         (spc+  "[ \t\r\n]+")
         (W     "[^][(){} \t\n]+")              ;;word
         (word  (concat "\\(" W "\\)"))         ;;capturing word
         (S     "[[({]+")                       ;;start
         (E     "[])}]+")                       ;;end

         ;; mail.compuserve.com (mail.compuserve.com (209.5.81.86))
         ;; mail.msss.v.com [atl.asd.com [234.454.54]]

         (re-word31
          (concat from
                  spc word
                  spc S spc word
                  spc S spc word  spc
                  E))

         ;;  Received: from [209.151.131.35] (HELO mx04.hotmail.com)
         ;;     by elysium.ca (CommuniGate Pro SMTP 3.5)

         (re-word32
          (concat from
                  spc+ S word E         ;; from [209.151.131.35]
                  spc+ S W spc+ word E  ;; (HELO mx04.hotmail.com)
                  spc+ "by" spc+ word))

         ;;  from hdn86-021.hil.compuserve.com(206.175.97.21) by

         (re-word2a
          (concat from
                  spc word
                  spc S spc word
                  spc E))

         ;;   Propably faked received header?
         ;;
         ;;   from usinet cziegle (1Cust144.tnt1.coeur-dalene.id.da.uu.net
         ;;       [208.254.107.144]) by ns.peace1.co.jp

         (re-word2b
          (concat from
                  "[^([{]+"
                  S spc word spc
                  S spc word spc
                  E))

         ;;  Received: from usa.net - 206.133.11.158 by
         ;;     ciudad.com.ar with Microsoft SMTPSVC; Mon, 2 Feb 1998 21:03:25

         (re-word2c
          (concat from
                  spc word spc+ "-"
                  spc+ word spc+ "by"))

         ;; Received: from foo by relay1.UU.NET with SMTP
         ;;    (peer crosschecked as: 1Cust185.tnt10.nyc3.da.uu.net
         ;;     [153.37.131.185])

         (re-word2d
          (concat from
                  spc word spc "by"
                  spc word spc "with"))

         ;;  from [206.102.180.52] by springfield.k12.il.us with ESMTP

         (re-word2e
          (concat from
                  spc S word E spc "by"
                  spc word spc "with"))

         ;; Received: by SERVER02 with Internet Mail Service (5.5.2650.21)
         ;; id <FVLHVM1Q>; Thu, 28 Feb 2002 16:26:29 -0500

         (re-word11
          (concat spc+ "by" spc+ W spc+ "with" spc+ W spc+ W spc+ W
                  spc+ S word E))

         ;; from papaguena.upc.es by rita.upc.es

         (re-word12 (concat from spc word spc "by" )))
    (list
     (list 3 (list re-word31
                   re-word32))
     (list 2 (list re-word2a
                   re-word2b
                   re-word2c
                   re-word2d
                   re-word2e))
     (list 1 (list re-word11
                   re-word12)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-line (regexp-list)
  "Parse all `Received:' IPs from current line with REGEXP-LIST.
The point must be placed just after the colon in header:

  Received:-!-

The -!- indicates the location of point."
  (let* (candidates)
    (catch 'done
      (dolist (elt regexp-list)
        (multiple-value-bind (submatch-max regexp-list)
            elt
          (dolist (regexp regexp-list)
            (when (looking-at regexp)
              (dotimes (count submatch-max) ;; starts counting from 0
                (push (match-string (1+ count)) candidates))
              ;; Regexp-list
              (throw 'done t))))))
    (nreverse candidates)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-string-smtp (string)
  "Parse SMTP field from 'Received:' STRING."
  ;; from 111.npgco.com (HELO NAZ-AZPIRE1) (24.121.15.77)
  (when (string-match
         (concat
          "\\<from[ \t\r\n]+[^ \t\r\n]+[ \t\r\n]+"
          "(\\"           ;; BEGIN
          "([^()]+)"      ;; First paren, required
          "\\([ \t\r\n]+" ;; Second, optional
          "([^()]+)\\)*"
          "\\)") ;; END capture
         string)
    (let* ((str  (match-string 1 string))
           (list (list str))
           ret)
      (if (string-match " " str)
          (setq list (split-string str)))
      (dolist (elt list)
        (push (replace-regexp-in-string "\\[\\|\\]\\|[()\r\n]" "" elt)
              ret))
      (nreverse ret))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-clean (string)
  "Remove () and newlines from STRING."
  (replace-regexp-in-string "[()\r\n]" "" string))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-from (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<from[ \t\r\n]+\\([^ \t\r\n]+\\)" string)
    ;;  from cm-24-121-15-77.flagstaff.az.npgco.com (HELO NAZ-AZPIRE1)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-by (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<by[ \t\r\n]+\\([^ \t\r\n]+\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-smtp-id (string)
  "Parse 'from' field from 'Received:' STRING."
  (cond
   ((string-match
     "[ \t\r\n]+id[ \t\r\n]+\\([^ ;\t\r\n]+\\)" string)
    (match-string 1 string))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-for (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match "\\<for[ \t\r\n]+\\([^ ;\t\r\n]+\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-parse-received-string-date (string)
  "Parse 'from' field from 'Received:' STRING."
  (when (string-match
         "^.+;[ \t\r\n]+\\(.+[^ \t\r\n]\\)" string)
    (match-string 1 string)))

;;; ----------------------------------------------------------------------
;;; (ti::mail-parse-date-string "Thu, 18 Jul 1996 12:18:06 -0600")
;;; (ti::mail-parse-date-string "21 Aug 2003 20:41:15 -0000")
(defun ti::mail-parse-date-string (date)
  "Parse DATE notation.
Recognized format are:

  Thu, 18 Jul 1996 12:18:06 -0600
  21 Aug 2003 20:41:15 -0000

The timezone value is optional.

Returns alist;

   '(weekday
     dd
     mon
     mm           ;; numeric string, like \"07\" for \"Jul\"
     yyyy
     HH
     MM
     SS
     tz)"
  (cond
   ((string-match
     (concat "^[ \t\r\n]*"
             "\\([A-Z]..\\),?[ \t\r\n]+"
             "\\([0-9]+\\)[ \t\r\n]+"
             "\\([A-Z]..\\)[ \t\r\n]+"
             "\\([0-9][0-9][0-9][0-9]\\)[ \t\r\n]+"
             "\\([0-9][0-9]\\):"
             "\\([0-9][0-9]\\):"
             "\\([0-9][0-9]\\)"
             "[ \t]*\\(.*\\)")
     date)
    (list
     (match-string 1 date)
     (format "%02d" (string-to-number (match-string 2 date)))
     (match-string 3 date)
     (format "%02d"
             (ti::month-to-number (match-string 3 date)))
     (match-string 4 date)
     (match-string 5 date)
     (match-string 6 date)
     (match-string 7 date)
     (match-string 8 date)))
   ((string-match
     (concat
      "^[ \t\r\n]*"
      "\\([0-9][0-9]?\\)[ \t\r\n]+"
      "\\([A-Z]..\\)[ \t\r\n]+"
      "\\([0-9][0-9][0-9][0-9]\\)[ \t\r\n]+"
      "\\([0-9][0-9]\\):"
      "\\([0-9][0-9]\\):"
      "\\([0-9][0-9]\\)"
      "[ \t]*\\(.*\\)")
     date)
    (list
     nil
     (match-string 1 date)
     (match-string 2 date)
     (format "%02d"
             (ti::month-to-number (match-string 2 date)))
     (match-string 3 date)
     (match-string 4 date)
     (match-string 5 date)
     (match-string 6 date)
     (match-string 7 date)))))

;;; ----------------------------------------------------------------------
;;; (ti::mail-parse-date-string-iso8601 "Thu, 18 Jul 1996 12:18:06 -0600")
(defun ti::mail-parse-date-string-iso8601 (date &optional tz)
  "Parse DATE. See supported values in `ti::mail-parse-date-string'.
Return ISO 8601 date

    YYYY-MM-DD HH:MM:SS

If TZ is non-nil, add timezone information to the end."
  (interactive)
  (multiple-value-bind
      (dd
       mm
       yyyy
       HH
       MM
       SS
       tzone)
      (ti::mail-parse-date-string date)
    (format "%s-%s-%s %s:%s:%s%s"
            yyyy mm dd HH MM SS (if tzone
                                    (or tz "")
                                  ""))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received-string (string)
  "Parse 'Received:' Header STRING.
From this STRING

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

Return list:

    '((from    . HOST1)
      (smtp    . (HOST2 ...))
      (by      . HOST3)
      (smtp-id . ID)
      (for     . FOR)
      (date    . DATE))

The `cdr' of a key may be nil if no value was found.

References:

  `ti::with-mail-received-heade'."
  (list
   (cons 'from    (ti::mail-parse-received-string-from string))
   (cons 'smtp    (ti::mail-parse-received-string-smtp string))
   (cons 'by      (ti::mail-parse-received-string-by   string))
   (cons 'smtp-id (ti::mail-parse-received-string-smtp-id string))
   (cons 'for     (ti::mail-parse-received-string-for  string))
   (cons 'date    (ti::mail-parse-received-string-date string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-parse-received (&optional not-matching no-dupes)
  "Search all 'Receive:' fields and read site names followed by 'from' 'by'.
Duplicate entries are not added.

Point must be at the beginning of headers to search, and
point is advanced.

It is possible to can this function to find out from where the mail
originated and send complaint to postmasters of all those sites.

Input:

  NOT-MATCHING  string, If read entry matches this regexp it is not included in
                returned list
  NO-DUPES      flag, if non-nil then do not include duplicate addresses.

Return:

    '((IP IP IP) (IP IP) ..)   as they appear in Received fields.

Received headers explained:

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

    This Shows four pieces of useful information (reading from back to front,
    in order of decreasing reliability):

     - The host that added the Received line (host3)
     - The IP address of the incoming SMTP connection (ww.xx.yy.zz)
     - The reverse-DNS lookup of that IP address (host2)
     - The name of the sender used in the SMTP HELO command at the
       time of connection.

Real examples:

Received: from mailhost.worldnet.att.net ([206.85.117.127])
          by mtigwc02.worldnet.att.net (post.office MTA v2.0 0613 )
          with SMTP id AAD8244; Sun, 23 Mar 1997 23:03:10 +0000
Received: from mail.msss.v.com [atl.asd.com [234.454.54]]
          by mediabrokers.cobracomm.com (8.8.5/8.6.5) with
          SMTP id GAA07901 for <box17@mediabrokers.cobracomm.com>"
  (let* ((regexp-list (ti::mail-parse-received-regexp-list))
         ret
         candidates
         ip-elt
         ip-all-list)

    (while (re-search-forward "^Received:" nil t)
      (setq ip-elt nil)
      (setq candidates (ti::mail-parse-received-line regexp-list))

      (dolist (elt candidates)
        (when (and (stringp elt)
                   (string-match "\\." elt) ;;from PAPAGUENA, require dot(.)
                   ;; Is exclude in effect?
                   (or (null not-matching)
                       (not (string-match not-matching elt)))
                   (if no-dupes
                       (not (member elt ip-all-list))
                     t))
          ;;  1) mailhost@inet.com --> inet.com
          ;;  2) remove some garbage from string

          (setq elt (replace-regexp-in-string ".*@" "" elt))
          (setq elt (replace-regexp-in-string "[]()\n]" "" elt))

;;;       (ti::d! elt)

          (if no-dupes
              (push elt ip-all-list))   ;Needed for duplicate checking

          (push elt ip-elt)))
      (if ip-elt
          (push ip-elt ret)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-received-header 'edebug-form-spec '(body))
(put 'ti::with-mail-received-header 'lisp-indent-function 1)
(defmacro ti::with-mail-received-header (string &rest body)
  "With Mail 'received:' heading in STRING, run BODY.
For this STRING

    Received: from host1 (host2 [ww.xx.yy.zz]) by host3
     (8.7.5/8.7.3) with SMTP id MAA04298; Thu, 18 Jul 1996 12:18:06 -0600

The following access variables are available within BODY:

  received-header-data
  from              => host1
  smtp              => '(host2 ww.xx.yy.zz)
  smtp-id           => MAA04298
  by                => host3
  for               => host1
  date              => Thu, 18 Jul 1996 12:18:06 -0600

Note:

  Any of the variables may be nil, if no value found.

References:

  See functions ti::mail-parse-received-string-*
  and `ti::mail-parse-received-string'."
  `(let ((received-header-data (ti::mail-parse-received-string ,string)))
     (symbol-macrolet ((from     (cdr (assq 'from received-header-data)))
                       (smtp     (cdr (assq 'smtp received-header-data)))
                       (by       (cdr (assq 'by received-header-data)))
                       (smtp-id  (cdr (assq 'smtp-id received-header-data)))
                       (for      (cdr (assq 'for received-header-data)))
                       (date     (cdr (assq 'date received-header-data))))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-cleanup (string)
  "Remove indentation and extra whitescape from STRING."
  ;;  Remove indentation
  (ti::string-remove-whitespace
   (replace-regexp-in-string
    "[\r\n][ \t]+" "\n"
    (replace-regexp-in-string "[ \t][ \t]+" " " string))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-paragraph (regexp &optional end-regexp)
  "Whois: Parse pragraph for the first REGEXP to END-REGEXP.
See `ti::mail-whois-parse'."
  (when (re-search-forward regexp nil t)
    (let ((beg (match-beginning 0)))
      (if (null end-regexp)
          (forward-paragraph)
        (re-search-forward end-regexp)
        (beginning-of-line))
      (ti::mail-whois-parse-cleanup
       (buffer-substring beg (1- (point)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-referral ()
  "Parse referral if any. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (cond
     ((and (goto-char point)
           (re-search-forward
            ;; Found a referral to example.com
            "^[ \t]*Found.*referral to \\([^ \t\r\n]+[a-z]\\)"
            nil 'noerr))
      (match-string 1))
     ((and (goto-char point)
           (re-search-forward
            ;; Referral URL: http://example.com
            "^[ \t]*referral[ \t]+URL:[ \]*\\([^ \t\r\n]+\\)"
            nil 'noerr))
      (match-string 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-email ()
  "Whois: Parse unique email addresses from buffer.
See `ti::mail-whois-parse'."
  ;; mailto:abuse@foo.com
  ;; trouble: Spam: <mailto:abuse@foo.com>
  ;; changed: 20030912 <migration@foo.com>
  (let ((kill-regexp
         (concat
          "E?-?mail:[ \t]*"
          "\\|\\(mailto\\|changed\\|updated\\):"
          "\\|\\<[0-9]+\\>"))<
          line
          email
          desc
          seen
          ret)
    (while (re-search-forward
            (concat
             "^[ \t]*.*[ ,;/\t]"
             "\\([^/,;<> \t\r\n]+@[^/,;<> \t\r\n]+\\)")
            nil 'noerr)
      ;; There is only one email at a line
      (setq email
            (replace-regexp-in-string
             "mailto:" ""
             (match-string 1)))
      (unless (member email seen)
        (push email seen)
        (setq line (ti::buffer-read-line))
        ;;  Remove that email from it
        (when (setq desc (replace-regexp-in-string
                          (regexp-quote email) "" line))
          (setq desc
                (ti::string-remove-whitespace
                 (replace-regexp-in-string
                  "," " "
                  (replace-regexp-in-string
                   kill-regexp ""
                   (replace-regexp-in-string
                    "[ \t][ \t]+" " " desc))))))
        (if (and desc
                 (ti::nil-p desc))
            (setq desc nil))
        (push
         (list (if desc
                   (format "%s <%s>"
                           desc
                           email)
                 email)
               email
               desc)
         ret)))
    ;; preserve order
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-whois-parse-paragraph-end-condition ()
  "Whois parse. See `ti::mail-whois-parse'."
  (concat
   "^[ \t]*\\(.+:[ \t]*[\r\n]"
   "\\|.*last update"
   "\\|.*servers in listed order\\)"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-1 ()
  "See `ti::mail-whois-parse-registrant'."
  (ti::mail-whois-parse-paragraph
   "^[ \t]*Registra\\(r\\|nt\\):.*[\r\n]+[ \t]*"
   (ti::mail-whois-parse-paragraph-end-condition)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-organization ()
  "See `ti::mail-whois-parse-registrant'."
  (ti::mail-whois-parse-paragraph
   "^[ \t]*Organi[zs]ation:[ \t]*[\r\n]+[ \t]*"
   (ti::mail-whois-parse-paragraph-end-condition)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-organization-2 ()
  "See `ti::mail-whois-parse-registrant'."
  ;; OrgName:    AT&T WorldNet Services
  ;; OrgID:      ATTW
  ;; Address:    400 Interpace Parkway
  ;; City:       Parsippany
  ;; StateProv:  NJ
  ;; PostalCode: 07054
  ;; Country:    US
  ;;
  ;;  ...
  ;;
  ;; # ARIN WHOIS database, last updated 2003-08-25 19:15
  ;; # Enter ? for additional hints on searching ARIN's WHOIS database.
  (ti::mail-whois-parse-paragraph
   "^OrgName:.*[\r\n]OrgID:"
   "^[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant-domain ()
  "See `ti::mail-whois-parse-registrant'."
  ;; domain:  AHA.RU
  ;; type:    CORPORATE
  ;; descr:   Mr. Postman BBS
  ;; admin-o: ZENON-ORG-RIPN
  ;; nserver: dns1.zenon.net.
  ;; nserver: dns2.zenon.net.
  ;; created: 1996.10.01
  ;; state:   Delegated till 2003.11.01
  ;; changed: 1998.08.11
  ;; mnt-by:  ZENON-MNT-RIPN
  ;; source:  RIPN
  (ti::mail-whois-parse-paragraph
   (concat
    "^domain:[ \t]+[a-z].*\\.[a-z0-9].+[ \t\r\n]"
    ;;Licensee:
    ;;   Name:     Belgacom Skynet DnsMasters
    ;;   Company:  Belgacom Skynet SA/NV
    "\\|^Licensee:[ \t]*$")
   "^[ \t]*$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-registrant ()
  "Whois: Parse registrant from buffer. See `ti::mail-whois-parse'."
  (let ((point (point))
        ret)
    (flet ((search (func)
                   (goto-char point)
                   (funcall func)))
      (dolist (func '(ti::mail-whois-parse-registrant-1
                      ti::mail-whois-parse-registrant-domain
                      ti::mail-whois-parse-registrant-organization
                      ti::mail-whois-parse-registrant-organization-2))
        (when (setq ret (search func))
          (return ret))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-tech ()
  "Whois: Parse tech from buffer. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (or (ti::mail-whois-parse-paragraph
         "^[ \t]*.*Technical Contact.*:"
         (ti::mail-whois-parse-paragraph-end-condition))
        (cond
         ((and (goto-char point)
               (re-search-forward ":\\(.*tech.*@.*\\)" nil t))
          (ti::mail-whois-parse-cleanup
           (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-zone ()
  "Whois: Parse zone from buffer. See `ti::mail-whois-parse'."
  (let ((point (point)))
    (or (ti::mail-whois-parse-paragraph
         "^[ \t]*.*Zone Contact.*:"
         (ti::mail-whois-parse-paragraph-end-condition))
        (cond
         ((and (goto-char point)
               (re-search-forward ":\\(.*zone.*@.*\\)" nil t))
          (ti::mail-whois-parse-cleanup
           (match-string 1)))))))

;;; ----------------------------------------------------------------------
;;;
;;; It the response is like this, there is no information
;;; about the created, expires
;;;
;;;     # ARIN WHOIS database, last updated 2003-08-25 19:15
;;;     # Enter ? for additional hints on searching ARIN's WHOIS database.
;;;
(defun ti::mail-whois-parse-records ()
  "Whois: Parse records from buffer. See `ti::mail-whois-parse'.
Values examined are: expires, created and updated."
  (let* ((date-info
	  (list
	   ;;  10-Aug-1998
	   (list
	    `,(concat
	       "\\("
	       "\\([0-9][0-9]?\\)"
	       "-\\([A-Z][a-z][a-z]\\)"
	       "-\\([0-9][0-9][0-9][0-9]\\)"
	       "\\)")
	    ;; day month year
	    '(3 4 5))
	   ;;  10-08-1998
	   (list
	    `,(concat
	       "\\("
	       "\\([0-9][0-9]?\\)"
	       "-\\([0-9][0-9]?\\)"
	       "-\\([0-9][0-9][0-9][0-9]\\)"
	       "\\)")
	    '(3 4 5))
	   ;;  Mon, Aug 10, 1998
	   (list
	    `,(concat
	       "\\("
	       "[A-Z][a-z][a-z],[ \t]*"
	       "\\([A-Z][a-z][a-z]\\)[ \t]+" ;; Mon
	       "\\([0-9]+\\)[ \t]*,[ \t]*"   ;; day
	       "\\([0-9][0-9][0-9][0-9]\\)"  ;; year
	       "\\)")
	    '(4 3 5))
	   (list
	    `,(concat
	       ;; 2003-08-25 19:15
	       "\\("
	       "\\([0-9][0-9][0-9][0-9]\\)"
	       "-\\([0-9][0-9]\\)"
	       "-\\([0-9][0-9]\\)"
	       "[ \t]+[0-9][0-9]:[0-9][0-9]"
	       "\\)")
	    '(5 4 3))
	   (list
	    `,(concat
	       ;; 1998.08.11
	       "\\("
	       "\\([0-9][0-9][0-9][0-9]\\)"
	       "[.]\\([0-9][0-9]\\)"
	       "[.]\\([0-9][0-9]\\)"
	       "\\)")
	    '(5 4 3))
	   (list
	    `,(concat
	       ;; changed:  20001107 15:03:09
	       ;; changed:     registdom@tin.it 20030403
	       ;;
	       "\\(\\([0-9][0-9][0-9][0-9]\\)"
	       "\\([0-9][0-9]\\)"
	       "\\([0-9][0-9]\\)"
	       "\\)")
	    '(5 4 3))))
	 (search (list
		  (list
		   'expires
		   `,(concat
		      "\\("
		      "^[ \t]*Record[ \t]+expires[ \t]+on[ \t]+"
		      "\\|^[ \t]*Expires[ \t]+on"
		      "\\|^expire:[^\r\n0-9]+"
		      "\\|^[ \t]*expiration date:[ \t]+"
		      "\\)"))
		  (list
		   'created
		   `,(concat
		      "\\("
		      "^[ \t]*Record[ \t]+created[ \t]+on[ \t]+"
		      "\\|^[ \t]*Created[ \t]+on.*[ \t]+"
		      "\\|^created:[^\r\n0-9]+"
		      "\\|^[ \t]*creation date:[ \t]+"
		      "\\)"))
		  (list
		   'updated
		   `,(concat
		      "\\("
		      "^.*last.*updated?[ \t]+on[ \t]+"
		      "\\|^[ \t]*updated date:[ \t]+"
		      "\\|^changed:[^\r\n0-9]+"
		      "\\)"))))
	 (beg (point))
	 ret)
    (dolist (elt search)
      (multiple-value-bind (type line)
	  elt
	(dolist (date-data date-info)
	  (multiple-value-bind (regexp pos-list)
	      date-data
	    (setq regexp (concat line regexp))
	    ;;  The order of the fields can be anything, start over
	    ;;  every time from the same point
	    (goto-char beg)
	    (when (re-search-forward regexp nil t)
	      (multiple-value-bind (raw day month year)
		  (list
		   (match-string 2)
		   (match-string (nth 0 pos-list))
		   (match-string (nth 1 pos-list))
		   (match-string (nth 2 pos-list)))
		(if (eq 3 (length month))
		    (setq month (ti::month-to-number
				 (capitalize month)
				 'zero)))
		(push (list
		       type
		       (list (format "%s-%s-%s" year month day)
			     raw))
		      ret))
	      (return))))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-servers ()
  "Whois: Parse servers from buffer. See `ti::mail-whois-parse'."
  (when (re-search-forward "^[ \t]*Domain servers" nil t)
    (forward-line 1)
    (let ((beg (point))
          (end (progn
                 (forward-paragraph)
                 (point))))
      (let (ret)
        (goto-char beg)
        ;; Domain servers in listed order:
        ;;
        ;; NS1.GALLERYHOSTING.NET       209.19.90.117
        ;; GHZ.DDAHL.COM                209.19.90.118
        ;;
        (while (re-search-forward
                (concat
                 "^[ \t]+"
                 "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)"
                 "[ \t]+"
                 "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)")
                end 'noerr)
          (push (list (downcase (match-string 1))
                      (match-string 2))
                ret))
        ;; Domain servers in listed order:
        ;;
        ;; Name Server: ns1.dr-parkingservices.com
        ;; Name Server: ns2.dr-parkingservices.com
        ;;
        (unless ret
          (goto-char beg)
          (while (re-search-forward
                  (concat
                   "^[ \t]+Name[ \t]+Server:"
                   "[ \t]+"
                   "\\([^ \t\r\n]+\\.[^ \t\r\n]+\\)")
                  end 'noerr)
            (push (list (downcase (match-string 1)) nil)
                  ret)))
        ret))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse-admin ()
  "Whois: Parse Administrative Contact from buffer.
See `ti::mail-whois-parse'."
  (let ((point (point)))
    (cond
     ((and (goto-char point)
           (re-search-forward "^[ \t]*Administrative Contact:" nil t))
      (forward-line 1)
      (let ((beg (point)))
        ;;  Search "Technical Contact:"
        (when (re-search-forward "^[ \t]*.+:[ \t]*$" nil t)
          (ti::mail-whois-parse-cleanup
           (buffer-substring
            beg (1- (line-beginning-position)))))))
     ((and (goto-char point)
           (re-search-forward ":\\(.*admin.*@.*\\)" nil t))
      (ti::mail-whois-parse-cleanup
       (match-string 1))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-error-p (string)
  "Check if Whois call failed by examining STRING"
  (not (string-match
        (concat
         "registra\\(nt\\|r\\):"
         ;; domain:  AHA.RU
         ;; type:    CORPORATE
         ;; descr:   Mr. Postman BBS
         ;; admin-o: ZENON-ORG-RIPN
         ;; nserver: dns1.zenon.net.
         ;; nserver: dns2.zenon.net.
         ;; created: 1996.10.01
         ;; state:   Delegated till 2003.11.01
         ;; changed: 1998.08.11
         ;; mnt-by:  ZENON-MNT-RIPN
         ;; source:  RIPN
         ;;
         ;; domain:   siemens.at
         ;; descr:    [organization]:Siemens AG
         ;; descr:    [street address]:Siemensstr. 92
         ;;
         "\\|^domain:[ \t]+[a-z].*\\..*[\n\r]"
         "\\(type\\|descr\\):"
         "\\|^address:.*[^ \t\r\n]"
         ;;
         "\\|^# ARIN WHOIS database")
        string)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-whois-parse (string)
  "Parse whois output STRING.

Return:

   '((email      .  ((ADDED EMAIL REST)  ;; ADDED is \"REST <EMAIL>\"
                    ...))
     (registrant .  STRING)
     (admin      .  STRING)
     (tech       .  STRING)
     (records    .  ((expires DATE-ISO RAW-DATE)
                     (created DATE-ISO RAW-DATE)
                     (updated DATE-ISO RAW-DATE))
     (servers    .  ((host ip)
                     ...)))

Note:

  All the keys, like 'admin', are present in returned list, but any of the
  `cdr' values or their components may be nil, if no value was found.

  Do not relay in the order of these fields. They may change
  any time. Instead access the list entry with `assq'.

References:

  See functions ti::mail-whois-parse-*
  and macro `ti::with-mail-whois'."
  (with-temp-buffer
    (insert string)
    (ti::buffer-text-properties-wipe)
    (let* ((referral   (progn (ti::pmin)
                              (ti::mail-whois-parse-referral)))
           (email      (progn (ti::pmin)
                              (ti::mail-whois-parse-email)))
           (registrant (progn (ti::pmin)
                              (ti::mail-whois-parse-registrant)))
           (admin      (progn (ti::pmin)
                              (ti::mail-whois-parse-admin)))
           (tech       (progn (ti::pmin)
                              (ti::mail-whois-parse-tech)))
           (zone       (progn (ti::pmin)
                              (ti::mail-whois-parse-zone)))
           (records    (progn (ti::pmin)
                              (ti::mail-whois-parse-records)))
           (servers    (progn (ti::pmin)
                              (ti::mail-whois-parse-servers))))
      (unless (and
               registrant)
        (error "TinyLibMail: Cannot parse Whois string %s" string))
      (list
       (cons 'referral    referral)
       (cons 'email       email)
       (cons 'registrant  registrant)
       (cons 'admin       admin)
       (cons 'tech        tech)
       (cons 'zone        zone)
       (cons 'records     records)
       (cons 'servers     servers)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-whois 'edebug-form-spec '(body))
(put 'ti::with-mail-whois 'lisp-indent-function 1)
(defmacro ti::with-mail-whois (string &rest body)
  "For full ´whois' output STRING run BODY.

The following access variables are available within BODY. Any
of the values may be nil.

  email
  admin         Administrative Contact
  tech          Technical Contact
  zone          Zone Contact
  records
  servers       Domain servers

References:

  `ti::mail-whois-parse'."
  `(let ((whois-data (ti::mail-whois-parse ,string)))
     (symbol-macrolet (
                       (referral   (cdr (assq 'referral   whois-data)))
                       (registrant (cdr (assq 'registrant whois-data)))
                       (email      (cdr (assq 'email      whois-data)))
                       (admin      (cdr (assq 'admin      whois-data)))
                       (tech       (cdr (assq 'tech       whois-data)))
                       (zone       (cdr (assq 'zone       whois-data)))
                       (records    (cdr (assq 'records    whois-data)))
                       (servers    (cdr (assq 'servers    whois-data))))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
;;; Registrant:
;;; David L. Dahl (DDAHL-DOM)
;;;    PO BOX
;;;    Chicago, IL 60657
;;;    US
;;;
;;;    Domain Name: DDAHL.COM
;;;
;;;    Administrative Contact:
;;;       Dahl, David  (DD4553)              ddahl@DDAHL.COM
;;;       3450 N. Lakeshore Dr. #2605
;;;       Chicago, IL 60657
;;;       US
;;;       773-934-1738 fax: 847-746-8841
;;;    Technical Contact:
;;;       Network Solutions, Inc.(HOST-ORG) customerservice@networksolutions.com
;;;       21355 Ridgetop Circle
;;;       Dulles, VA 20166
;;;       US
;;;       1-888-642-9675 fax: 123 123 1234
;;;
;;;    Record expires on 31-Mar-2005.
;;;    Record created on 18-Sep-2002.
;;;    Database last updated on 23-Aug-2003 04:47:44 EDT.
;;;
;;;    Domain servers in listed order:
;;;
;;;    NS1.GALLERYHOSTING.NET       209.19.90.117
;;;    GHZ.DDAHL.COM                209.19.90.118
;;;    WWW.CONDOSYSTEMS.COM         64.202.114.20
;;;
;;;
(defun ti::mail-whois (site &optional options verb bin)
  "Call `whois' and return results.
Web interface is at http://www.internic.net/whois.html

Input:

  site          Top level domain. Make sure you have called
                ´ti::mail-ip-top-level-domain' first.
  OPTIONS       list, additional options. E.g. -h HOST
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary."
  (let* ((path  (or bin
                    (get 'ti::mail-whois 'binary)
                    (executable-find "whois")
                    (error "No `whois' binary found.")))
         args)
    (put 'ti::mail-whois 'binary path)
    (when (and options
               (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (when (string-match "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" site)
      (setq args options)
      (push site args)
      (if verb
          (message "TinylibMail: whois %s ..." site))
      (with-temp-buffer
        (apply 'call-process
               path
               nil       ;; input
               '(t t)    ;; mix stdout and stderr
               nil       ;; display
               args)
        (if verb
            (message "TinylibMail: whois %s ...done." site))
        (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-nslookup-parse ()
  "Parse nslookup output in current buffer forward.

Buffer contains:

  Non-authoritative answer:
  Server:  this.server.com
  Address:  nnnn.nnn.nnn.nnn

  Name:    NAME.ANSWER.COM
  Addresses:  NNN.NNN.NNN.NNN,NNN.NNN.NNN.NNN

Return:

'(NAME.ANSWER.COM (NNN.NNN.NNN.NNN  NNN.NNN.NNN.NNN ..))."
  (let* (name
         ip-list
         (re            "[ \t]+\\([^ \t\r\n]+\\)")
         (name-regexp   (concat "name:"  re))
         (regexp1       (concat "address:"   re))
         (regexp2       "addresses:[ \t]+\\([^\r\n]+\\)"))
    (when (re-search-forward "^[ \t]*$" nil t)
      (forward-line 1)
      (when (re-search-forward name-regexp nil t)
        (setq name (match-string 1))
        (cond
         ((re-search-forward regexp1 nil t)
          (setq ip-list (list (match-string 1))))
         ((re-search-forward regexp2 nil t)
          (let ((ip (match-string 1)))
            (setq ip-list
                  (if (not (string-match "," ip))
                      (list ip)
                    (list (split-string ip "[ \t,]+")))))))))
    (if ip-list
        (list name ip-list))))

;;; ----------------------------------------------------------------------
;;;
;;;  % nslookup 204.253.213.3
;;;  Name Server:  example.com
;;;  Address:  131.228.134.50
;;;
;;;  Name:    librum.sourcery.com
;;;  Address:  204.253.213.3
;;;
;;;  Can also have string:
;;;
;;;  *** No address information is available for "mktg@inet.com"
;;;
;;;  NOTE: There may be "Addresses:"
;;;  =========================================================
;;;
;;;  Server:  ns3.tpo.fi
;;;  Address:  212.63.10.250
;;;
;;;  Name:    yahoo.com
;;;  Addresses:  216.115.109.6, 216.115.109.7
;;;
(defun ti::mail-nslookup (ip &optional options verb bin)
  "Run `nslookup' for IP.

Note:

  If IP address does not match 2-3 alphabetic character or max 3 digits
  at the end, then the address is not checked at all. It is immediately
  discarded.

Input:

  IP            numeric on normal site address.
  OPTIONS       list, additional options. E.g. -query=any
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary

Return:

  '(name . address)

If nslookup fails, the return value is '(ORIG-IP nil)"
  (let* ( ;;  It's faster to use absolute pathname.
         ;;
         (path  (or bin
                    (get 'ti::mail-nslookup 'binary)
                    (executable-find "nslookup")
                    (error "No `nslookup' binary found.")))
         args)
    (put 'ti::mail-nslookup 'binary path)
    (when (and options
               (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (with-temp-buffer
      (when verb
        (message "TinylibMail: nslookup %s ..." ip))
      (when (string-match
             "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" ip)
        (setq args options)
        (push ip args)
        (apply 'call-process
               path
               nil     ;; input
               '(t t)  ;; mix stdout and stderr
               nil     ;; display
               args))
      (when verb
        (message "TinylibMail: nslookup %s ...done." ip))
      (unless (ti::re-search-check "No address information")
        (ti::pmin)
        (ti::mail-nslookup-parse)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::with-mail-nslookup 'edebug-form-spec '(body))
(put 'ti::with-mail-nslookup 'lisp-indent-function 1)
(defmacro ti::with-mail-nslookup (data &rest body)
  "with resault of `ti::mail-nslookup' DATA '(ip (ip ...)) run BODY.
The following variables are available during looping within BODY:

  ip-name  ip-found."
  `(multiple-value-bind (ip-name ip-list)
       (list
        (car ,data)
        (cdr ,data))
     (dolist (ip-found ip-list)
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-dig (ip &optional options verb bin)
  "Run `dig' for IP.

Note:

  If IP address does not match 2-3 alphabetic character or max 3 digits
  at the end, then the address is not checked at all. It is immediately
  discarded.

Input:

  IP            numeric on normal site address.
  OPTIONS       list, additional options. E.g. -query=any
  VERB          flag, if non-nil print verbose messages. (Recommended)
  BIN           Location of the binary

Return:

  '(name . address)

If nslookup fails, the return value is '(ORIG-IP nil)"
  (let* ( ;;  It's faster to use absolute pathname.
         ;;
         (path  (or bin
                    (get 'ti::mail-dig 'binary)
                    (executable-find "dig")
                    (error "No `nslookup' binary found.")))
         args)
    (put 'ti::mail-dig 'binary path)
    (when (and options
               (not (ti::listp options)))
      (error "OPTIONS must be a list."))
    (with-temp-buffer
      (when verb
        (message "TinylibMail: dig %s ..." ip))
      (when (string-match
             "\\.[0-9][0-9]?[0-9]?$\\|\\.[a-z][a-z][a-z]*$" ip)
        (setq args options)
        (push ip args)
        (apply 'call-process
               path
               nil     ;; input
               '(t t)  ;; mix stdout and stderr
               nil     ;; display
               args))
      (when verb
        (message "TinylibMail: dig %s ...done." ip))
      (buffer-string))))

;;}}}
;;{{{ misc

;;; ............................................................ &misc ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-buffer  (&optional mode-list)
  "Return open mail buffer if one exists.
MODE-LIST is the search order precedence. It can take values
'mail-mode 'message-mode and any
other valid mail like modes.

Example:

  ;; Return some mail-mode buffer. If there is none, then
  ;; return some message-mode buffer.

  (ti::mail-get-buffer '(mail-mode message-mode))"
  (let* (list
         buffer)
    (or mode-list
        (setq mode-list '(mail-mode message-mode mh-letter-mode)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (dolist (mode mode-list)
          (when (eq major-mode mode)

            ;;  We keep the separate mode in the plist
            ;;
            ;;  LIST: plist 'MODE1 --> '(buffer buffer ...)
            ;;      : plist 'MODE2 --> '(buffer buffer ...)

            (setq list (get 'list mode)) ;Read current list
            (push (current-buffer) list) ;Add one
            ;;  And update plist
            (put 'list mode list)))))

    ;;  Step through mode lists and return first buffer

    (dolist (mode mode-list)
      (when (setq buffer (car-safe (get 'list mode)))
        (return)))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-signature-insert-break (&optional point)
  "Insert RFC signature break to current point or POINT if no sig break exist.
According to RFC there must be \"-- \\n\" before signature. The extra space
separates the signature from e.g. digest messages that come with \"--\\n\"

We try to find this string forward and it is not there we add one."
  (save-excursion
    (if point (goto-char point))
    (if (null (re-search-forward "^-- \n" nil t))
        (insert "-- \n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-yank (&optional prefix)
  "Yank message to current point and add optional PREFIX. GNUS/RMAIL."
  (let* (p
         (yb (ti::mail-mail-buffer-name)) ;where is the yank buffer ?
         ;;  See this mail is called from GNUS
         ;;
         ;;  - If GNUS isn't loaded, set buf name to nil
         (gnus-buf (and (boundp 'gnus-article-buffer)
                        (symbol-value 'gnus-article-buffer)))
         ;;  Test if gnus-reply; the buffers are the same
         (gnus-r (and gnus-buf
                      (string= gnus-buf yb))))
    (save-excursion
      (setq p (point))
      ;;  (mail-yank-original '(4))     ; mimic C-u C-c C-y == no indent
      ;;  - bypass all, see sendmail::mail-yank-original
      ;;    this is more robust, and runs no extra hooks
      ;;  - If in GNUS, the buffer will be *Article*, which is
      ;;    narrowed to headers...widen the buffer before yanking.
      (if (null gnus-r)
          (progn                        ; normal mail
            (mail-yank-original '(4)))
        (with-current-buffer yb
	  (widen))
        (insert-buffer-substring yb))
      (ti::pmax)
      (delete-blank-lines)
      (if prefix
          (string-rectangle p (point-max)  prefix)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-trim-buffer ()
  "Trim email message so that there are no trailing white spaces.
- at the beginning of message
- at the end of each line
- at the end of message.

If cannot find text start point, uses `point-min'. The point is not preserved.

Return:
  t         there is no text. All white spaces were removed
  nil       trimming done."
  (let ((beg  (ti::mail-text-start))
        ret)
    (goto-char beg)
    (ti::buffer-replace-regexp "[ \t]+$" 0 "") ;right hand spaces (ragged lines)
    (goto-char beg)

    ;;   Beginning of email message

    (ti::buffer-trim-blanks beg (point-max))
    (ti::buffer-delete-until-non-empty-line nil beg)

    (ti::buffer-delete-until-non-empty-line 'backward (point-max))
    (forward-line 1)

    ;;  Any text left ? Signing empty file is not sensible...

    (if (eq (point) beg)
        (setq ret t)
      ;;  Note: User may write message "123" to the body, but we must
      ;;  require final newline every time: "123\n", the trim
      ;;  command will remove any exeessive newlines.
      (ti::pmax)
      (if (not (char-equal (preceding-char) ?\n))
          (insert "\n")))
    ret))

;;}}}

;;{{{ fields, headers

;;;  .......................................................... &fields ...

(defsubst ti::mail-field-space-count (field-name &optional field-value )
  "Check how many spaces is at the beginning of field.
Input:

  FIELD-NAME        If given, fetch FIELD-NAME like 'to' and check it's value.
  FIELD-VALUE       If given, use this string as field content. Argument
                    FIELD-NAME is ignored.

Return

  N                 Number of space."
  (or field-value
      (and (or (stringp field-name) (error "Missing field-name"))
           (setq field-value (ti::mail-get-field field-name)))
      (error "No field"))
  (and field-value
       (length (ti::string-match "^[^ ]*\\( +\\)" 1 field-value))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-start (field-re &optional move max)
  "Return starting point of FIELD-RE or nil. Optionally MOVE to it.

Supposes that field has following format, the cursor -!- position
signifies returned point.

  field:-!-

Input:

  FIELD-RE      field regexp
  MOVE          should we move to found point? (beginning-of-line)
  MAX           search until MAX point"
  (let (ret)
    (save-excursion
      (ti::pmin)
      (when (re-search-forward field-re max t)
        (beginning-of-line)
        (when (re-search-forward ":" max t)
          (setq ret (point)))))
    (if (and move ret)
        (goto-char ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-next-field-start (&optional move back max)
  "Return starting point of next field or nil. Optionally move to field.

Note:

  If you're somewhere else than inside header area, the return value
  is not defined.

Input:

  MOVE          move to point
  BACK          move backward (field start)
  MAX           search until this point. PLEASE USE THIS TO LIMIT SEARCH

Return:

  point
  nil"
  (let ((func (if back 're-search-backward 're-search-forward))
        opoint
        point
        ret)
    (save-excursion
      (if (null back)
          (end-of-line))

      (if (and (bobp) back)             ;first field
          (setq ret (point))

        ;;   Next line must have text, otherwise the headers have ended
        ;;   alredy
        ;;
        ;;   Header1:
        ;;   Header2:
        ;;
        ;;   BODY-OF-TEXT

        (cond
         ((save-excursion
            (forward-line 1)
            (looking-at ".*[a-zA-Z0-9]"))
          (setq opoint (point))

          ;;  In the last field, the previsu regexp skips too much,
          ;;  see where the cursor (*) is. We search backward if possible
          ;;  to find header separator (empty line)
          ;;
          ;;  Header:
          ;;    last header text
          ;;
          ;;  *BODY

          (when (progn
                  (when (and (setq point (funcall func "^[^ \t]" max t))
                             (eq func  're-search-forward))
                    (goto-char opoint)  ;Try again
                    (if (re-search-forward "^$" nil t)
                        ;; no, it was further ahead, use previous search pos
                        (if (< (point) point)
                            (setq point (point)))))
                  point)
            (goto-char point)
            (beginning-of-line)
            (setq ret (point))))
         (t
          ;;  Hm, next line is empty line, not a field for us any more.
          nil))))

    (if (and move ret)
        (goto-char ret))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-field-string-wrap (string)
  "Wrap i.e. delete embedded newlines in string.

X-My: one line
   two line
   three line.

=>

X-My: one line two line three line."
  (replace-regexp-in-string "[\r\n][ \t]+" " " string))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-field-string-p (string)
  "Check if string starts with Field:
Subexpression 1 contains field name and 2 contains rest."
  (string-match "^\\([A-Z][^:]+\\):\\(.*\\)" string))

;;; ----------------------------------------------------------------------
;;;FIXME:
(defun ti::mail-field-line-p ()
  "Return `field' name if the bginning of line contains 'NNNN:'."
  (let ((str (buffer-substring
              (line-beginning-position)
              (line-end-position))))
    (when (ti::mail-field-string-p str)
      (match-string 1 str))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-read-line-at-point (&optional wrap)
  "Read whole header field at point. Field may continue in separate line.
Point -!- must be at the beginning line of field.

X-More: this is one line-!-
  That is wrapped to send
  and even third.

If WRAP is non-nil, call `ti::mail-field-string-wrap'."
  (let ((beg (line-beginning-position))
        (end (ti::mail-next-field-start)))
    (when (and beg end)
      (let ((line (buffer-substring beg (1- end))))
        (if wrap
            (ti::mail-field-string-wrap line)
          line)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-read-fuzzy (&optional wrap)
  "Read whole header field at point.
The point can be anywhere in the field.
If WRAP is non-nil, call `ti::mail-field-string-wrap'."
  (save-excursion
    (beginning-of-line)
    (unless (ti::mail-field-line-p)
      (ti::mail-next-field-start 'move 'back))
    (ti::mail-field-read-line-at-point wrap)))

;;; ----------------------------------------------------------------------
;;;FIXME:
(defun ti::mail-current-field-name  ()
  "Return name of field at current point or nil."
  (save-excursion
    (when (or (not (bolp))
              (and (bolp)
                   ;;  Newly opened line - continuation of e.g. To: field.
                   (looking-at "^[ \t]*$")))
      (ti::mail-next-field-start 'move 'back))
    (ti::mail-field-line-p)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-email-send-p (&optional header-regexp)
  "Check if point is at field To, Cc or Bcc"
  (let ((field (ti::mail-current-field-name)))
    (when (and field
               (string-match
                (or header-regexp
                    "^\\(to\\|cc\\|bcc\\)$")
                field))
      field)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-field-email-address-p ()
  "Check if point is at field To, Cc, Bcc, From, Sender."
  (ti::mail-field-email-send-p
   "^\\(to\\|cc\\|bcc\\|from\\|sender\\)$"))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field-in-body (list)
  "Kill LIST of field that are inserted into body of message."
  (ti::narrow-safe (ti::mail-text-start) (point-max)
    (dolist (header list)
      (ti::mail-kill-field header))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field (field-re &optional replace-str)
  "Delete header field. Remember to supply Anchor '^' in FIELD-RE.

Input:

  FIELD-RE      any regexp matching a line
  REPLACE-STR   replace field content with this

Return:

  t             field changed or killed
  nil           nothing done [field not exist]"
  (let ((hdr-end  (ti::mail-hmax))
        beg
        end)

    (when hdr-end
      (if replace-str
          (setq replace-str (ti::string-verify-ends replace-str " " nil 'beg)))

      (save-excursion
        (when (and (setq beg  (ti::mail-field-start field-re 'move))
                   (setq end  (ti::mail-next-field-start))
                   (<= end hdr-end))
;;;             (setq F field-re B beg E end)
          (if replace-str
              (progn
                (delete-region beg end)
                (insert (concat replace-str "\n")))
            (beginning-of-line)
            (delete-region (point) end)
            t))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-field-1 (field)
  "Read FIELD by finding regexp matching '^FIELD:'.
Starting searching from the beginning of buffer. You are encouraged to call
this function instead of `ti::mail-get-field' if you want to get the
field information fast e.g. in `post-command-hook'.

This function is not as reliable as `ti::mail-get-field', because
the search is not limited to header area, but for regular headers
you can use this function safely."
  (let ((re  (format "^%s:" field))
        beg
        end)
    (save-excursion
      (ti::pmin)
      (if (and (re-search-forward re nil t)
               (setq beg (point)
                     end (ti::mail-next-field-start)))
          (buffer-substring beg (1- end))))))

;;; ----------------------------------------------------------------------
;;; - This is almost the same as mail-utils.el/mail-fetch-field,
;;;   but offers more control. It can get citated fields too, if
;;;   ANY parameter is non-nil.
;;; - And it returns _strict_ content of the field, fetch-field strips
;;;   spaces away.
;;;
(defun ti::mail-get-field (field &optional any mode)
  "Return field content.

Input:

   FIELD         field name without anchor '^' and char ':'
   ANY           return any field. When non-nil, drops anchor ^
                 from the  ^field: criteria
   MODE          nil    read the field as is, returning all chars
                        after the ':'
                 t      If field has only spaces, Return nil
                 'pure  Include header name as well as content.

Return:

   nil or contents of field."
  (let ((case-fold-search t)            ;ignore case = t
        (re (if any
                (concat field ":")      ; pick first one met
              (concat "^" field ":")))  ; require STRICT HEADER

        (hmax (if any nil (ti::mail-text-start)))
        beg
        end
        ret)
    (save-excursion
      (when (and (setq beg (ti::mail-field-start re 'move hmax))
                 (setq end (ti::mail-next-field-start nil nil hmax)))
        (when (and (eq mode 'pure)
                   (looking-at "[\t ]*[^\n\t ]+")) ;not empty
          (beginning-of-line)
          (setq beg (point)))
        (setq ret (buffer-substring beg (1- end)))))
    (when (and mode
               (stringp ret)
               (string-match "^[ \t\n\r]*\\'" ret))
      (setq ret nil))
    ret))

;;; ----------------------------------------------------------------------
;;; - If you want simple filed adding to your mail, then have a look
;;;   at this instead:
;;;
;;;     (defconst my-mail-info-string "Emacs RMAIL in 19.28")
;;;     (setq mail-default-headers
;;;           (concat
;;;            "X-info: " my-mail-info-string "\n"))
;;;
(defun ti::mail-add-field (field text &optional look-field mode replace)
  "Add FIELD and puts TEXT into it.
If field already exist, replaces field text.
By default, field is added to the end of header.

Input:

  FIELD         string, like \"To\".
  TEXT          \\n at end is optional. _No_ colon and _no_ spaces.
  LOOK-FIELD    new field will be added after this. _No_ colon at end.
                if MODE is non-nil, field is added before this field

                If there is no LOOK-FIELD, nothing is done and nil
                is returned.

  MODE          see look-field
  REPLACE       if non-nil Any previous field is removed. You probably
                want to set this flag to non-nil if you only want unique
                field names.

Return:

  t             something done
  nil           nothing done, maybe look-field doesn't exist ?"
  (let* ((field-re (concat "^" field ":")))
    (save-excursion
      (cond
       (look-field
        (if replace
            (ti::mail-kill-field field-re)) ;Remove
        (when (and look-field
                   (ti::mail-field-start (concat "^" look-field) 'move))
          (unless mode                  ;use only forward
            (ti::mail-next-field-start 'move mode))
          (beginning-of-line)
          (insert (concat field ": " text))
          t))
       (t                               ;add to the end
        (if (mail-fetch-field field)
            (ti::mail-kill-field field-re text)
          (mail-position-on-field field)
          (insert text))
        t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-add-to-field-string (field string &optional look-field sep )
  "Find FIELD and add STRING to the. Field is created if it does not exist.

Input:

  FIELD      string WITHOUT colon, anchor or spaces.
  STRING     added text
  LOOK-FIELD field name. If Field does not exist, add field after this field.
             See `ti::mail-add-field'
  SEP        defaults to comma and space."
  (or sep
      (setq sep ", "))

  (save-excursion
    (ti::pmin)
    (let ((content  (mail-fetch-field field)))
      (if (ti::nil-p content)
          (ti::mail-add-field field string look-field)
        (re-search-forward (concat "^" field ":"))
        (ti::mail-next-field-start 'move)
        (skip-chars-backward " \n\t")
        (insert sep string)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-kill-field-elt (re &optional field)
  "Kill all elts matching RE from FIELD, which defaults to cc.
Elements are supposed to be separated by commas.

Example:

   To: him@example.com
   CC: me@example.com, you@example.com

   ;;  If called with this

   (ti::mail-kill-field-elt \"me\")
   --> To: him@example.com
   --> CC: you@example.com

   ;; If called with this; all elts are matched and thus the
   ;; field is removed

   (ti::mail-kill-field-elt \".\")
   --> To: him@example.com"
  (let* (flag
         str
         fld)
    (setq field (or field "CC"))

    (when (setq fld    (ti::mail-get-field field))
      ;; remove spread lines

      (setq fld (replace-regexp-in-string "[\n\f\t ]+" "" fld))
      (setq fld (split-string fld "[,]+")) ; divide into items

      ;; ... ... ... ... ... ... ... ... ... ... ... ...  remove items . .

      (setq fld
            (ti::list-find  fld re
                            (function
                             (lambda (arg elt)
                               (not (string-match arg elt))))
                            'all-items))

      ;; ... ... ... ... ... ... ... ... ... ... ... . build up string . .

      (dolist (elt fld)
        (if (null flag)
            (setq flag  t               ;done 1st line
                  str   (concat " " elt))
          (setq str (concat
                     str ", " elt
                     (if (> (+ (length str) (length elt))  70)
                         "\n  "  "")))))

      ;; ... ... ... ... ... ... ... ... ... ... ... ...  write new fld . .
      (if str
          (ti::mail-kill-field (concat "^" field) str) ;replace
        ;;  Remove whole field, all entries were discarded.
        ;;
        (ti::mail-kill-field (concat "^" field))))))

;;; ----------------------------------------------------------------------
;;; - This is mainly for converting your mail to anon post by
;;;   removing any headers you might have added.
;;;
(defun ti::mail-kill-non-rfc-fields (&optional list)
  "Kill all non RFC fields unless LIST (HEADER-NAME-SYMBOL .. ) list is given.

References
  `ti::mail-required-headers'    ,default rfc headers"
  (let ((ptr (or list
                 (ti::mail-required-headers)
                 (error "(ti::mail-required-headers) returned nil")))
        (case-fold-search t)
        fld
        list)
    ;;  First we gather all valid headers to list
    (dolist (elt ptr)
      (setq fld (symbol-name elt))
      (when (setq elt (ti::mail-get-field fld))
        (ti::nconc list (format "%s:%s" (capitalize fld) elt))))
    ;; Now we kill all headers and yank the valid ones back.
    (ti::mail-hmax 'move)
    (delete-region (point-min) (point))
    (setq ptr list)
    (dolist (elt ptr)
      (insert elt "\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-get-all-email-addresses
  (&optional field-list abbrev-alist no-expand)
  "Return all email addresses from FIELD-LIST.

Input:

 FIELD-LIST     Eg. '(\"To\" \"CC\"). Default is To CC and BCC.
 ABBREV-ALIST   see function `ti::mail-abbrev-expand-mail-aliases'
 NO-EXPAND      if non-nil, Do not expand addresses on current buffer.

Return:
 '(str str ..)  notice that there may be \"\" empty strings"
  (let ((buffer  (if no-expand
                     (generate-new-buffer "*tmp*")
                   (current-buffer)))
        str
        mems
        field
        ret)
    (unwind-protect
        (progn
          (or field-list (setq field-list '("To" "CC" "BCC")))

          (when no-expand
            (dolist (fld field-list)
              (setq str
                    (concat (or str "")
                            (ti::mail-get-field fld nil 'pure)
                            "\n"))))

          (with-current-buffer buffer
            (if str (insert str))

            (ti::save-with-marker-macro
              (ti::mail-abbrev-expand-mail-aliases
               (point-min)
               (if str
                   (point-max)
                 (ti::mail-hmax))
               abbrev-alist))

;;;         (pop-to-buffer (current-buffer)) (ti::d! "MT:ABB" field-list)

            (dolist (elt field-list)
              (when (setq field (mail-fetch-field elt))
                (setq mems (split-string field "[,\n]+")) ;members ?
;;;             (ti::d! elt field mems)
                (dolist (mem mems)
                  (unless (ti::nil-p mem)
                    (push mem ret))))))) ;; with-current + progn
      ;;  make sure temp buffer is removed.

      (if no-expand
          (kill-buffer buffer)))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-set-recipients (to-list &optional cc-list cc-flag)
  "Compose current mail message to TO-LIST and add info about CC-LIST.

Input:

  TO-LIST       List of real recipients.
  CC-LIST       List of additional recipients that are put to
                X-Cc-Info. These are not actual CC members.
  CC-FLAG       Treat CC-LIST as actual recipients. This is like combining
                TO-LIST and CC-LIST. No X-Cc-Info field is added."
  (when cc-flag
    (setq to-list (ti::list-merge-elements to-list cc-list)
          cc-list nil))

  (ti::mail-kill-field "^To")
  (ti::mail-kill-field "^CC")
  (ti::mail-kill-field "^X-Cc-Info")

  (ti::mail-add-field "To" (pop to-list))
  (when to-list
    (ti::mail-add-field "Cc" (mapconcat 'concat to-list ",")))

  (when cc-list
    (ti::mail-add-field
     "X-Cc-Info"
     (concat "Additional recipient(s)\n  "
             (mapconcat 'concat cc-list ",")))))

;;}}}
;;{{{ News, articles

;;; ............................................................ &News ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-news-buffer-p ()
  "Check if current buffer is news post, followup or the like."
  (interactive)
  (cond
   ((and (eq major-mode 'message-mode) (fboundp 'message-news-p))
    (ti::funcall 'message-news-p))
   ((and (eq major-mode 'message-mode) (boundp 'message-this-is-news))
    (symbol-value 'message-this-is-news))
   ((string-match "news" (symbol-name major-mode)))
   (t
    ;;  Gnus keeps it in 'message-mode', so search this header then
    (save-excursion
      (ti::pmin)
      (re-search-forward "Newsgroups\\|References:\\|Gcc:" nil t)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-article-regexp-read-line (re &optional level)
  "Switch to article buffer; match RE at LEVEL and return match."
  (let (line)
    (or level
        (setq level (or level 0)))
    (ti::mail-with-article-buffer
     (ti::pmin)
     (if (re-search-forward re nil t)
         (setq line (match-string level))))
    line))

;;; ----------------------------------------------------------------------
;;; - This is useful if you use same hook for both
;;;   regular mail posting AND for gnuis posting.
;;; - It makes it possible to decicede inside hook, which post
;;;   type this is. Eg. setting extra headers for NEWS and not
;;;   different for regular Mail
;;;
(defun ti::mail-news-reply-p ()
  "Return type of message being composed.
This function is meaningful _only_ when you use it inside
some GNUS or mail hook.  The buffer must be current mail buffer.

Return:
  'news         news
  nil"
  (let* ((mode          (symbol-name major-mode))

         ;;             GNUS might not be loaded in this emacs
         (gnus-buf      (if (boundp 'gnus-article-buffer)
                            ;; normally name is "Article"
                            (symbol-value 'gnus-article-buffer)
                          ""))

         (mail-buf      (ti::mail-mail-buffer-name)) ;YANK buffer name?

         ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ GNUS
         ;; - Detect news posting mode. The mail program uses
         ;;   YANK from the gnus buffer "*Article*" So we can detect
         ;;   if this is gnus post
         ;; - gnus 'news-mail-reply-->rnewspost.el

         (gnus          (string= gnus-buf mail-buf))

         (news-mode     (or gnus
                            (string-match "news" mode)
                            (save-excursion ;Gnus
                              (ti::pmin)
                              (re-search-forward "^References:" nil t)))))
    (if news-mode
        'news)))

;;}}}
;;{{{ anon.penet.fi anon-nymserver.com

;;; ............................................................ &anon ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-anon-penet-p (email)
  "Check if EMAIL is penet anon address."
  (string-match "[an][an][0-9]+@.*penet.fi" email))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-anon-penet-to-p ()
  "Check if the TO: field contain anon.penet.fi address.

Return:
  nil
  email       if it's anon address."
  (let  ((to (ti::mail-get-field "to")))
    (if (and to (ti::mail-anon-penet-p to))
        to nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-nymserver-email-convert (email &optional na-mode)
  "Convert penet EMAIL address.

If NA-MODE is nil: do 'an' conversion
        anXXX@example.com   --> anXXX
        naXXX@example.com   --> anXXX
       VANITY@example.com   --> VANITY@example.com
    VANITY.an@example.com   --> VANITY@example.com
    VANITY.na@example.com   --> VANITY@example.com

If NA-MODE is non-nil:
    Then do opposite 'na' conversion"
  (cond
   (na-mode
    (if (string-match "\\(an\\)[0-9]+@\\|\\.\\(an\\)@" email)
        (setq email (ti::replace-match 1 "na" email))
      ;; the email is VANITY@example.com
      (if (string-match "\\(.*\\)\\(@.*\\)" email)
          (setq email (concat
                       (match-string 1 email) ".na"
                       (match-string 2 email))))))
   (t
    (cond
     ((string-match "\\(na\\)[0-9]+@" email)
      (setq email (ti::replace-match 1 "an" email)))
     ((string-match "\\(\\.na\\)@" email)
      (setq email (ti::replace-match 1 "" email))))))
  email)

;;}}}
;;{{{ mime

;;; ............................................................ &mime ...

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-tm-featurep-p  ()
  "TM. Check if MIME is loaded."
  (and (featurep 'mime-setup)
       (not (featurep 'semi-setup))))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-semi-featurep-p  ()
  "SEMI. Check if MIME is loaded."
  (featurep 'semi-setup))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-feature-p  ()
  "MIME. Check if TM/ or SEMI is available."
  (or (ti::mail-mime-tm-featurep-p)
      (ti::mail-mime-semi-featurep-p)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-tm-edit-p ()
  "TM. Check if mime edit is active."
  (and (boundp 'mime/editor-mode-flag)
       (symbol-value 'mime/editor-mode-flag)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::mail-mime-semi-edit-p ()
  "SEMI. Check if mime edit is active."
  (and (boundp 'mime-edit-mode-flag)
       (symbol-value 'mime-edit-mode-flag)))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-tm-edit-mode-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-tm-edit-mode-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-tm-edit-mode-macro  (&rest body)
  "TM. Run body If mime edit mode is active in current buffer."
  `(when (and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-semi-edit-mode-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-semi-edit-mode-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-semi-edit-mode-macro  (&rest body)
  "SEMI. Run body If mime edit mode is active in current buffer."
  `(when (and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-funcall-0-macro 'lisp-indent-function 1)
(put 'ti::mail-mime-funcall-0-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-funcall-0-macro (func-tm func-semi)
  "Call function  FUNC-TM or FUNC-SEMI with no arguments."
  `(cond
    ((and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     (ti::funcall ,func-tm)
     t)
    ((and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     (ti::funcall ,func-semi))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-funcall-2-macro 'lisp-indent-function 3)
(put 'ti::mail-mime-funcall-2-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-funcall-2-macro (func-tm func-semi arg1 arg2)
  "Call function  FUNC-TM or FUNC-SEMI with ARG1 ARG2."
  `(cond
    ((and (ti::mail-mime-tm-featurep-p) (ti::mail-mime-tm-edit-p))
     (ti::funcall ,func-tm ,arg1 ,arg2)
     t)
    ((and (ti::mail-mime-semi-featurep-p) (ti::mail-mime-semi-edit-p))
     (ti::funcall ,func-semi ,arg1 ,arg2)
     t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-turn-on-mode ()
  "Turn on MIME mode. Do nothing if mime is not available.
Return t if mime was supported."
  (interactive)
  (cond
   ((ti::mail-mime-tm-featurep-p)
    (unless (ti::mail-mime-tm-edit-p)
      (ti::funcall 'mime/editor-mode))
    t)
   ((ti::mail-mime-semi-featurep-p)
    (unless (ti::mail-mime-semi-edit-p)
      (ti::funcall 'mime-edit-mode))
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-turn-off-mode ()
  "Turn off MIME mode. Do nothing if mime is not available.
Return t if mime was supported."
  (interactive)
  (cond
   ((ti::mail-mime-tm-featurep-p)
    (when (ti::mail-mime-tm-edit-p)
      (ti::funcall 'mime-editor/exit))
    t)
   ((ti::mail-mime-semi-featurep-p)
    (when (ti::mail-mime-semi-edit-p)
      (ti::funcall 'mime-edit-exit))
    t)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-sign-region (&optional beg end)
  "MIME. Enclose region BEG END as signed.
Input:

BEG   Defaults to mail beginning or buffer beginning.
END   Defualts to `point-max'

Return:

nil  if mime is not available.
"
  (interactive)
  (ti::mail-set-region beg end)
  (ti::mail-mime-funcall-2-macro
   'mime-editor/enclose-signed-region
   'mime-edit-enclose-pgp-signed-region
   beg
   end))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-encrypt-region (&optional beg end)
  "MIME. Enclose region BEG END as encrypted
Input:

BEG   Defaults to mail beginning or buffer beginning.
END   Defualts to `point-max'

Return:

nil  if mime is not available.
"
  (interactive)
  (ti::mail-set-region beg end)
  (ti::mail-mime-funcall-2-macro
   'mime-editor/enclose-encrypted-region
   'mime-edit-enclose-pgp-encrypted-region
   beg
   end))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-mime-tm-split-macro 'lisp-indent-function 0)
(put 'ti::mail-mime-tm-split-macro 'edebug-form-spec '(body))
(defmacro ti::mail-mime-tm-split-macro (&rest body)
  "TM. Define  variables `split'  `max' `parts' and run BODY if TM active.
You have to use variables `max' and `parts' otherwise you don't need this macro."
  `(when (boundp 'mime-editor/split-message)
     (let* ((split (symbol-value 'mime-editor/split-message))
            (max   (symbol-value 'mime-editor/message-default-max-lines))
            (lines (count-lines (point-min) (point-max)))
            (parts (1+ (/ lines max))))
       (if (null split)
           (setq split nil))            ; No-op Bytecomp silencer
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-maybe-p ()
  "Check if buffer possibly contain MIME sections.
if there is boundary string in header or if the TM -mime tags
'-[[' are found from buffer, then it's considered mime."
  (or (ti::mail-mime-p)
      (save-excursion
        (cond
         ((featurep 'tm-edit)           ;TM.el
          ;;   TM puth these markes to MIME section; Try to find one.
          ;;   This can be only found if the mimi-edit mode is not
          ;;   yet exited. Upon exit the message will match true
          ;;   MIME (ti::mail-mime-p).
          (ti::re-search-check (concat "^" (regexp-quote "--[["))))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mime-p ()
  "Check if buffer has mime message. You probably want `ti::mail-mime-maybe-p'.
It must contain boundary field in the headers and the boundary
must be found from the message body itself.
Only the header is not enough to say it's a composed mime mail.

Content-Type: ...
  boundary=\"Multipart_Thu_Sep_19_12:46:36_1996-1\"
           ^^^^^^^^^

This text must be found after the headers until the MIME criteria is
satisfied."
  (interactive)
  (let ((field (ti::mail-get-field "Content-Type" 'any))
        re)
    (when (and field
               ;;  Content-Type field may not include "boundary"
               ;;  --> it's not multipart mime.
               (setq re (ti::string-match ".*boundary=.\\(.*\\)\"" 1 field)))
      (setq re (regexp-quote re))
      ;;   start finding the boundary text after the headers.
      (save-excursion
        (ti::pmin) (re-search-forward re) ;This is the header, ignore it
        (forward-line 1)
        (re-search-forward re nil t)))))

;;; ----------------------------------------------------------------------
;;;FIXME: not tested
;;;
(defun ti::mail-mime-qp-decode(from to)
  "Mime. Decode quoted-printable from region between FROM and TO."
  (save-excursion
    (goto-char from)
    (while (search-forward "=" to t)
      (cond ((char-equal (following-char) ?\n)
             (delete-char -1)
             (delete-char 1))
            ((looking-at "[0-9A-F][0-9A-F]")
             (delete-char -1)
             (insert (hexl-hex-string-to-integer
                      (buffer-substring (point) (+ 2 (point)))))
             (delete-char 2))
            ((message "Malformed MIME quoted-printable message"))))))

;;; ----------------------------------------------------------------------
;;; (add-hook 'vm-select-message-hook 'ti::mail-mime-prepare-qp)
;;;
(defun ti::mail-qp-mime-prepare ()
  "Mime. Unquote quoted-printable from mail buffers.
Searches for tag:

content-transfer-encoding: quoted-printable"
  (interactive)
  (save-excursion
    (let ((case-fold-search t)
          (type (mail-fetch-field "content-transfer-encoding"))
          buffer-read-only)
      (cond
       ((and (stringp type)
             (string-match "quoted-printable" type))
        (ti::pmin)

        (search-forward "\n\n" nil 'move)
        (message  "MIME Unquoting printable...")
        (ti::mail-mime-qp-decode (point) (point-max))
        (message  "MIME Unquoting printable...done"))))))

;;}}}

;;{{{ Mail sending

;;; .................................................... &mail-sending ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-plugged-p ()
  "Check if computer is on-line. This function relies on Gnus."
  (when (boundp 'gnus-plugged)
    (symbol-value 'gnus-plugged)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-sendmail-reset-send-hooks ()
  "Make `mail-send-hook' et al. buffer local and set to nil."
  (dolist (sym '(mail-send-hook
                 message-send-hook
                 mh-before-send-letter-hook))
    (when (boundp sym)
      (add-hook sym 'ignore nil 'local)
      (set sym nil))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-pure-env-macro 'lisp-indent-function 0)
(put 'ti::mail-sendmail-pure-env-macro 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-pure-env-macro (&rest body)
  "Reset all mail/message hooks/vars locally to nil and run BODY."
  `(let* (message-setup-hook
          message-mode-hook
          mail-mode-hook
          mail-setup-hook
          mail-archive-file-name
          mail-default-headers
          mail-default-reply-to)
     ;; byteComp silencer: "Not used variables."
     (if mail-mode-hook         (setq mail-mode-hook            nil))
     (if mail-setup-hook        (setq mail-setup-hook           nil))
     (if mail-archive-file-name (setq mail-archive-file-name    nil))
     (if mail-default-headers   (setq mail-default-headers      nil))
     (if mail-default-reply-to  (setq mail-default-reply-to     nil))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-macro-1 'lisp-indent-function 3)
(put 'ti::mail-sendmail-macro-1 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-macro-1 (to subject send &rest body)
  "See `ti::mail-sendmail-macro' instead. This is low level function."
  `(progn
     (ti::mail-sendmail-pure-env-macro
      ;;   to subject in-reply-to cc replybuffer actions
      ;;
      (mail-setup ,to ,subject nil nil nil nil)
      (mail-mode)
      (ti::mail-kill-field "^fcc")
      (ti::mail-text-start 'move)
      ,@body
      (ti::pmin)
      (ti::kill-buffer-safe " sendmail temp") ;See sendmail-send-it
      (when ,send
        (mail-send-and-exit nil)))))

;;; ----------------------------------------------------------------------
;;;
(put 'ti::mail-sendmail-macro 'lisp-indent-function 3)
(put 'ti::mail-sendmail-macro 'edebug-form-spec '(body))
(defmacro ti::mail-sendmail-macro (to subject send &rest body)
  "Send / construct mail according to parameters.
Use TO, SUBJECT and If SEND if non-nil, send mail after BODY finishes.

Point is at the beginning of body.

Note:

    `mail-mode-hook' `mail-setup-hook' `mail-archive-file-name'
    `mail-default-headers'

    are set to nil. If you need these, please copy them before calling this
    macro and restore their values in BODY, possibly calling
    and using them as sendmail normally would.

    The hooks are set to nil so that mail buffer is created fast and
    that nothing causes trouble when mail buffer is ready."
  `(let* ((BuffeR (ti::temp-buffer ti:mail-mail-buffer 'clear)))
     (save-window-excursion
       (with-current-buffer BuffeR
         (ti::mail-sendmail-macro-1
          ,to
          ,subject
          ,send
          ,@body)))))

;;}}}

;;{{{ Abbrevs: XEmacs and Emacs

;;; ......................................................... &abbrevs ...

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-abbrev-table  ()
  "XEmacs and Emacs Compatibility, Return mail abbrev hash table."
  (ti::package-require-mail-abbrevs)
  (cond
   ((ti::emacs-p)
    (if mail-abbrevs
        (ti::funcall 'mail-abbrevs-setup))
    (or mail-abbrevs
        (progn
          (build-mail-aliases)
          mail-abbrevs)
        mail-aliases))
   (t
    ;;  in Emacs this is a list, in XEmacs this is a HASH
    (or mail-aliases
        (progn
          (condition-case err
              (build-mail-aliases)
            (error
             ;;  See mail-abbrev.el
             (when (get-buffer "mailrc")
               (pop-to-buffer (get-buffer "mailrc")))
             (error err)))
          mail-aliases)))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-abbrev-expand-mail-aliases  (beg end &optional alias-alist)
  "Expand aliases in region BEG END.
Please Cache results from `ti::mail-abbrev-get-alist' and
use the result as argument ALIAS-ALIST. Otherwise aliases are always
reuild from scratch."
  (interactive "*r")
  (let* (mb
         me
         word
         exp)
    (cond
     ((and (require 'mailalias nil 'noerr) ;Emacs Feature only
           (fboundp 'expand-mail-aliases))
      (ti::funcall 'expand-mail-aliases beg end))

     (t                                 ;Too bad, this is much slower
      (unless alias-alist
        (setq alias-alist (ti::mail-abbrev-get-alist)))
      (save-restriction
        (narrow-to-region beg end) (ti::pmin)
        (while (re-search-forward
                "^[ \t]+\\|^[ \t]*[\n,][ \t]*\\|:[ \t]*" nil t)
          (when (setq word (ti::buffer-match"[^ \t,\n]+" 0))
            (setq mb (match-beginning 0)
                  me (match-end 0))
            ;;  Do not count field names, like  "CC:" words
            (when (and (not (string-match ":$" word))
                       ;;  Is this abbrev ?
                       (setq exp (assoc word alias-alist)))
              (setq exp (cdr exp))      ; Change alias to expansion
              (delete-region mb me)
              (insert exp)
              ;;  This isn't very smart formatting, the layout
              ;;  is so that each expansion is on it's own line,
              ;;  no fancy lining up things -- Mail me back
              ;;  with diff to this code if you code nicer one.
              (when (looking-at "[ \t]*,") ;put on separate lines
                (goto-char (match-end 0))
                (when (not (looking-at "[ \t]*$"))
                  (insert "\n\t")
                  (beginning-of-line)))))))))))

;;; ----------------------------------------------------------------------
;;; See mailabbrev.el how to build your abbrevs.
;;;
(defun ti::mail-abbrev-get-alist  (&optional expand-until)
  "Return alist of all `mail-abbrevs'.
Build the abbrev table from your ~/.mailrc with command
\\[build-mail-abbrevs]. The following parameter is _not_ yet functional.

Input:
  EXPAND-UNTIL        expand until the RH elt is pure email.

Return:
  '((ABBREV-STRING . EXPANDED-STRING) (A . E) ..)"
  (let ((abbrev-expand-functions nil) ;; prevent recursion
	(mail-abbrev-aliases-need-to-be-resolved t)
	table
	exp-list
	elt)
    ;; XEmacs 19.14 no-op for ByteCompiler
    (unless mail-abbrev-aliases-need-to-be-resolved
      (setq mail-abbrev-aliases-need-to-be-resolved nil))
    (setq table (ti::mail-abbrev-table))
    (cond
     ((listp table) ;; mail-aliases is already in (A . S) form
      (setq exp-list table))
     (t                                 ;Vector
      ;;  We have to expand abbrevs by hand because XEmacs doesn't
      ;;  parse them like emacs mail-alias
      (when table
        (let ((tmp (generate-new-buffer "*ti::mail-abbrev*")))
          (with-current-buffer tmp
            (setq local-abbrev-table table)

            (mapatoms
             (function
              (lambda (x)
                (setq elt (prin1-to-string (identity x)))
                (when (not (string= "0" elt)) ;abbrev in this slot?
                  (insert elt)
                  (end-of-line)
                  ;;  2000-09-03
                  ;;  BBDB does some voodoo with the abbrevs by
                  ;;  setting the function cell, and sometimes  calling
                  ;;  expand-abbrev by BBDB blessed abbrev gives error.
                  ;;  --> Don't bother with the error, since the
                  ;;  abbrevs is correctly expanded, but BBDB cries about
                  ;;  "wrong marker" or something.
                  (condition-case err
                      (expand-abbrev)
                    (error
                     (message
                      (concat
                       "tinylibmail: `expand-abbrev' signalled ERROR `%s'"
                       " while expanding `%s'")
                      (prin1-to-string err)
                      elt)))
                  (push (cons (symbol-name x) (ti::read-current-line)) exp-list)
                  (end-of-line)
                  (insert "\n"))))
             table))
          (kill-buffer tmp))))) ;; cond
    exp-list))

;;; ----------------------------------------------------------------------
;;;
(defun ti::mail-mail-abbrevs-email-list  (&optional abbrev-alist)
  "Build email list of abbrevs; optionally use ABBREV-ALIST.
Only entries in this format in ~/.mailrc are returned. There must be
no \",\" chained lists in the line.

  alias[spaces]some[spaces]user@address.xx

Input:

  '((\"abbrev\" . \"expansion\") (A . E) ..)

Return:

  '(email email ...)"
  (let* (str
         email
         list)
    (dolist (elt
             (or abbrev-alist (ti::mail-abbrev-get-alist)))
      (setq str (cdr elt))
      (when (null (string-match "," str)) ;filter out multiple mail lists
        (setq email (ti::string-match "\\([^< \t]+@[^> \t\n]+\\)" 0 str))
        (if email
            (push str list))))
    ;; retain order
    (nreverse list)))

;;}}}

;;{{{ provide

(provide   'tinylibmail)
(run-hooks 'ti:mail-load-hook)

;;}}}

;;; tinylibmail.el ends here
