;;; tinypgp.el --- PGP minor mode, remailing, keyring management

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinypgp-version.
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
;; This file is not part of Emacs

;;}}}
;;{{{ Install

;;; Install:

;; ....................................................... &t-install ...
;;
;;  THIS FILE IS UNMAINTAINED - AND NOT WORKING IN ANY WAY
;;
;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      (require 'tinypgp)
;;
;;  or use this; your ~/.emacs loads quicker
;;
;;      (autoload 'tinypgp-mode                 "tinypgp" "" t)
;;      (autoload 'turn-on-tinypgp-mode         "tinypgp" "" t)
;;      (autoload 'turn-off-tinypgp-mode        "tinypgp" "" t)
;;      (autoload 'tinypgp-install              "tinypgp" "" t)
;;
;;      (add-hook 'message-mode-hook      'turn-on-tinypgp-mode)
;;      (add-hook 'mail-mode-hook         'turn-on-tinypgp-mode)
;;      (add-hook 'rmail-mode-hook        'turn-on-tinypgp-mode)
;;      (add-hook 'vm-mode-hook           'turn-on-tinypgp-mode)
;;      (add-hook 'gnus-startup-hook      'tinypgp-install)
;;      (add-hook 'gnus-article-edit-mode 'turn-on-tinypgp-mode)
;;
;;  Put your customizations to separate file and add this.
;;
;;   (setq tinypgp-:load-hook
;;     '(lambda () (require 'rc-tinypgp  "~/elisp/rc/emacs-rc-tinypgp")))
;;
;;  to automatically sign all your outgoing mail, add this to your .emacs
;;  For more personal signing, see manual
;;
;;      (add-hook 'mail-send-hook    'tinypgp-sign-mail-auto-mode-on)
;;      (add-hook 'message-send-hook 'tinypgp-sign-mail-auto-mode-on)
;;
;;  Suggested mode binding, "m" prefix for all minor mode toggles.
;;  If these are occupied, then choose some other bindings.
;;
;;      ;; note, Mailcrypt's prefix key is C-c / which is also
;;      ;; this package's prefix key unless you use the setq below.
;;      ;;
;;      ;; Personally I like the "-" because it's easier to reach than "/"
;;      ;; in my keyboard.
;;      ;;
;;      (setq tinypgp-:mode-prefix-key    "\C-c-")
;;      (global-set-key "\C-cm-"        'tinypgp-mode)
;;      (global-set-key "\C-cm'"        'tinypgp-key-mode)
;;
;;  See the end of file for additional examples.
;;  If you want to contact maintainer, always use this function
;;
;;      M-x tinypgp-submit-bug-report       -- send feedback or bug report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;;; Commentary:

;;}}}

;;; History:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylib)
(require 'tinylibmail)
(require 'mail-utils)

(eval-when-compile
  (ti::package-use-dynamic-compilation)
  (require 'advice))

(eval-and-compile

  (message "\
  ** tinypgp.el: Notice dated 2000-02-10
     THIS FILE IS NOT CURRENTLY MAINTAINED. You can expect that the pgp
     interface is non-functional and compiling this file gives errors.")

  (when (and (ti::win32-p)
             (ti::nil-p (getenv "PGPPATH")))
    (error "TinyPgp: environment variable PGPPATH not set for secring.*"))

  (ti::package-package-require-timer)

  (autoload 'rmail-edit-current-message         "rmailedit" t t)
  (autoload 'rmail-cease-edit                   "rmailedit" t t)
  (autoload 'rmail-add-label                    "rmailkwd")
  (autoload 'rmail-kill-label                   "rmailkwd")

  (defvar vm-frame-per-edit t)          ;See vm-vars.el
  (autoload 'vm-edit-message                    "vm-edit" t t)
  (autoload 'vm-edit-message-end                "vm-edit" t t)
  (autoload 'vm-delete-message-labels           "vm-undo" t t)
  (autoload 'vm-add-message-labels              "vm-undo" t t)
  (autoload 'vm-update-summary-and-mode-line    "vm" t t)

  (defvar mail-send-hook nil)
  (defvar mail-mode-hook nil)

  (defvar message-mode-hook nil)
  (autoload 'message-send-and-exit              "message")

  (autoload 'mail-send-and-exit                 "sendmail")
  (autoload 'mail-setup                         "sendmail")
  (autoload 'mail-do-fcc                        "sendmail")

  (autoload 'adelete                            "assoc")

  ;; TM mime available at
  ;; ftp://ftp.jaist.ac.jp:/pub/GNU/elisp/mime/

  (defvar   mime/editor-mode-flag nil)
  (autoload 'mime-editor/exit                       "tm-edit")
  (autoload 'mime-editor/enclose-signed-region      "tm-edit")
  (autoload 'mime-editor/enclose-encrypted-region   "tm-edit")
  (autoload 'mime-viewer/quit                       "tm-view")

  (autoload 'timi-mail                              "tinymail")

  (autoload 'bbdb-search-simple                     "bbdb")
  (autoload 'bbdb-record-getprop                    "bbdb")

  (autoload 'gnus-inews-do-gcc                      "gnus-msg")
  (autoload 'gnus-summary-edit-article              "gnus-sum" t t)
  (autoload 'gnus-article-edit-done                 "gnus-art" t t)

  ;;  The expect code is needed only in Pgp 5.x
  ;;  Only if that backend is used the expect.el is loaded.

  (autoload 'expect-make-info                     "expect" nil nil)
  (autoload 'expect-info-process                  "expect" nil nil 'macro)
  (autoload 'expect-info-message                  "expect" nil nil 'macro)
  (autoload 'expect-info-point                    "expect" nil nil 'macro)
  (autoload 'expect-info-set-point                "expect" nil nil 'macro)
  (autoload 'expect-info-sentinels                "expect" nil nil 'macro)
  (autoload 'expect-info-set-sentinels            "expect" nil nil 'macro)
  (autoload 'expect-info-timer                    "expect" nil nil 'macro)
  (autoload 'expect-info-set-timer                "expect" nil nil 'macro)
  (autoload 'expect-info-queries                  "expect" nil nil 'macro)
  (autoload 'expect-info-set-queries              "expect" nil nil 'macro)
  (autoload 'expect-find-info                     "expect" nil nil 'macro)
  (autoload 'with-expect                          "expect" nil nil 'macro)
  (autoload 'expect-start-process                 "expect" nil nil)
  (autoload 'with-expect-asynchronous             "expect" nil nil 'macro)
  (autoload 'expect                               "expect" nil nil 'macro)
  (autoload 'expect-cond                          "expect" nil nil 'macro)
  (autoload 'expect-exit                          "expect" nil nil 'macro)
  (autoload 'expect-send                          "expect" nil nil 'macro)
  (autoload 'expect-setup                         "expect" nil nil)
  (autoload 'expect-shutdown                      "expect" nil nil)
  (autoload 'expect-kill                          "expect" nil nil)
  (autoload 'expect-wait                          "expect" nil nil)
  (autoload 'expect-1                             "expect" nil nil)
  (autoload 'expect-exit-1                        "expect" nil nil)
  (autoload 'expect-filter                        "expect" nil nil)
  (autoload 'expect-sentinel                      "expect" nil nil)
  (autoload 'expect-find-event                    "expect" nil nil)
  (autoload 'expect-setup-timer                   "expect" nil nil)
  (autoload 'expect-cancel-timer                  "expect" nil nil)

  ;;  When file is byte compiled, the expand-file-name might eventually
  ;;  call this function, so let emacs know where it is.

  (autoload 'ange-ftp-real-expand-file-name         "ange-ftp" t t))

;;; ......................................................... &v-group ...

(defgroup TinyPgp nil
  "Emacs PGP and Remailer interface.

        TinyPgp is intended to be a 2nd generation Emacs PGP interface
        and it supports all major pgp commands from inside
        emacs. Remailing and anonymous account handling in different
        servers is included."

  :link '(url-link
          :tag "Keyserver home"
          "http://geronimo.uit.no/cc/tjenester/PGP/servruit.eng.html")

  :link '(url-link
          :tag "Pgp mailing list"
          "http://pgp.rivertown.net/")

  :link '(url-link
          :tag "Norway's keyserver"
          "http://www.ifi.uio.no/pgp/")

  :link '(url-link
          :tag "Remailer Faq (Galactus)"
          "http://www.stack.urc.tue.nl/~galactus/remailers/")

  :link '(url-link
          :tag "PGP faq alt.security.pgp"
          "ftp://ftp.prairienet.org/pub/providers/pgp/pgpfaq.txt")

  :link '(url-link
          :tag "X-Pgp header specififacion"
          "ftp://cs.uta.fi/pub/ssjaaa/pgp-xhd.html")

  :link '(url-link
          :tag "TinyPgp Manu page"
          "ftp://cs.uta.fi/pub/ssjaaa/tinypgp.html")

  :prefix "tinypgp-:"
  :group 'extensions)

;;; .................................................... &v-group-mode ...

(defgroup tinypgp-mode-definitions nil
  "Mode names, menu names and prefix key settings."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-mode nil
  "Options that directly address basic PGP commands in minor modes."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-header nil
  "Options that deal with Email message headers."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-file nil
  "Files used when communicating with PGP. You shouldn't rename these.
Do not add any extension to files, because PGP itself may append extension
.asc or .pgp or .bak. change only directory location.

When you load the package first time the directory name is initialized
from `tinypgp-:file-directory' or if it is nil a wild guess will be taken
See function documentation `tinypgp-path' for details.
"
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-hook nil
  "Variables where you can put your own functions."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-function nil
  "Variables where you can put your own functions."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-pgp nil
  "Options that relate to PGP executable and shell envinronment."
  :prefix "tinypgp-:"
  :group  'TinyPgp)

;;; .................................................... &v-group-misc ...

(defgroup tinypgp-interface nil
  "Variables to configure connections to outside world (ftp, http, email)"
  :prefix "tinypgp-:"
  :group  'TinyPgp)

(defgroup tinypgp-remail nil
  "Remailer interface settings."
  :prefix "tinypgp-:r"
  :group  'TinyPgp)

(defgroup tinypgp-remail-hook nil
  "Remailer interface hooks."
  :prefix "tinypgp-:r"
  :group  'tinypgp-remail)

(defgroup tinypgp-nymserver nil
  "Anonymous service (paid) anon.nymserver.com settings.
Similar to anon.penet.fi, which has been closed permanently."

  :link '(url-link
          :tag "Nymserver main page"
          "http://www.nymserver.com")

  :link '(url-link
          :tag "Nymserver html doc (a bit old)"
          "ftp://cs.uta.fi/pub/ssjaaa/nymserv.html")

  :prefix "tinypgp-:r"
  :group  'TinyPgp)

(defgroup tinypgp-newnym nil
  "Anonymous PGP service newnym type remailers."

  :link '(url-link
          :tag "Nym help page"
          "http://www.stack.nl/~galactus/remailers/nym.html")

  :prefix "tinypgp-:r"
  :group  'TinyPgp)

;;}}}
;;{{{ setup: predefined functions

(defcustom tinypgp-:file-directory nil
  "*Directory where to store temporary files. Must not be public; like /tmp/.
You should store files under your private directory. If this variable
is nil; then `tinypgp-path' guesses the right location for you. See function
documentations for more."
  :type  'directory
  :group 'tinypgp-file)

;;; ----------------------------------------------------------------------
;;; Define this function becore it is used in variables.
;;;

(eval-and-compile

  (defun tinypgp-expand-file-name (file &optional type)
    "Expand file under correct OS. TYPE overrides: 'unix 'win32."
    (cond
     ((and (ti::win32-p)
           (eq type 'unix))
      (save-match-data
        (ti::file-name-forward-slashes-cygwin (expand-file-name file))))
     ((or (ti::win32-p)
          (eq type 'win32))
      (save-match-data
        (ti::file-name-backward-slashes (expand-file-name file))))
     (t
      (expand-file-name file))))

  (defun tinypgp-path (file &optional try-paths)
    "Add path to FILE with TRY-PATHS. See also `tinypgp-:file-directory'.
Search list is:

    `tinypgp-:file-directory'
    PGPPATH
    ~/.pgp/
    ~/

If FILE already includes path, do nothing."
    (let* (path)
      (or (stringp file)
          (error "FILE is missing."))

      (if (string-match "[~/]" (substring file 0 1))
          (tinypgp-expand-file-name file) ;Already had path
        (dolist (try (or try-paths
                         (list tinypgp-:file-directory
                               (getenv "PGPPATH")
                               "~/.pgp/"
                               "~/")))
          (when try
            (setq try (ti::string-verify-ends try "/"))
            (when (file-directory-p try)
              (setq path try)
              (return))))
        (if (not (file-exists-p path))
            (error "Can't find path %s" path))
        (tinypgp-expand-file-name (concat path file))))))

;;; ----------------------------------------------------------------------
;;;

(eval-and-compile

  (defun tinypgp-binary-get-version (&optional ret-type call-shell)
    "Return version number of current pgp.

Input:

  RET-TYPE      How the information is returned.
                If this is nil then return STRING.
                If this is non-nil then return 'us (2.6.2) or
                'international (2.6.3i)

  CALL-SHELL    if nil, then look variable `tinypgp-:pgp-binary'.
                If there is no variable or it is not string, then
                call shell to find out pgp exe's version number

Return:

  string        See RET-TYPE
  symbol"
    (let (ret)
      (if (and (null call-shell)
               (boundp 'tinypgp-:pgp-binary)
               (setq ret (get 'tinypgp-:pgp-binary 'version)))
          nil                           ;ret already set
        (setq ret (ti::mail-pgp-exe-version-string)))

      (if (and ret-type (stringp ret))
          (if (string-match "i" ret)
              (setq ret 'international)
            (setq ret 'us)))
      ret)))

;;}}}
;;{{{ setup: version

;;; ...................................................... &vp-version ...
;;; the version information is needed in the variable definitions later.

(eval-and-compile
  (defconst tinypgp-:version-id
    "$Id: tinypgp.el,v 2.57 2007/05/07 10:50:10 jaalto Exp $"
    "Latest modification time and version number.")
  (defun tinypgp-version-number ()
    "Return version number as string."
    (ti::string-match "\\([0-9]+\\.[0-9]+\\)" 1 tinypgp-:version-id)))

;;; ----------------------------------------------------------------------
;;;

(defun tinypgp-version (&optional arg)
  "Show version information. ARG instruct to print message in echo area only."
  (interactive "P")
  (ti::package-version-info "tinypgp.el" arg))

;;; ----------------------------------------------------------------------
;;;

(defun tinypgp-version-message ()
  "Display version."
  (interactive)
  (message tinypgp-:version-id))

;;}}}
;;{{{ setup: hooks

;; ......................................................... &v-hooks ...

(defcustom tinypgp-:load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:mode-hook nil
  "*Hook run when minor mode is turned on."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:key-mode-hook nil
  "*Hook run when minor mode is turned on."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:summary-mode-hook nil
  "*Hook run when minor mode is turned on."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:mail-send-hook-list
  '(mail-send-hook
    message-send-hook
    mh-before-send-letter-hook)
  "*List of hooks that are called by Mail agents before sending mail."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:turn-on-hook-list
  '(mail-mode-hook
    rmail-mode-hook
    vm-mode-hook
    message-mode-hook
    gnus-article-mode-hook              ; When selecting the article.
    gnus-article-edit-mode-hook
    news-reply-mode-hook                ; 'f' key reply, GNUS 4 only
    mh-letter-mode-hook
    mh-show-mode-hook)
  "*List of hooks where to install pgp mode.
Call `add-hook' only inside `tinypgp-:load-hook', because the defvar
installs many default hooks."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:sig-from-header-hook nil
  "*Hook run at the end of `tinypgp-pgp-move-sig-from-header' function.
If there is no PGP header, hook is not called."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:sig-to-header-hook nil
  "*Hook run at the end of `tinypgp-signature-move-to-header function' function.
If there is no PGP header, hook is not called."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:sign-loose-info-hook nil
  "*Hook run when the `tinypgp-sign-loose-info' function has completed."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:define-keys-hook nil
  "*List of functions to define all keys and menus."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:key-mode-define-keys-hook nil
  "*List of functions to define all keys and menus."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:summary-mode-define-keys-hook nil
  "*List of functions to define all keys and menus."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:newnym-mode-define-keys-hook nil
  "*List of functions to define all keys and menus."
  :type  'hook
  :group 'tinypgp-hook)

(defvar tinypgp-:do-command-region-before-hook nil
  "Hook run in tmp buffer where containing data for pgp.
The PGP shell command is at this point stored into variable
`tinypgp-:last-pgp-exe-command'.

Note:

  If function in this hook returns non-nil, the rest of the functions are
  not called.

Call arguments:

  cmd msg res-str")

(defvar tinypgp-:do-command-region-after-hook nil
  "Hook run in tmp buffer after the PGP shell command has completed.

Note:

  If function in this hook returns non-nil, the rest of the functions are
  not called.

Call arguments:
  cmd msg res-str")

(defvar tinypgp-:cmd-macro-before-hook nil
  "Hook which run before the pgp sequence initiates in current buffer.
See function `tinypgp-cmd-macro' for arguments.

Note:

  First function that returns non-nil terminates running the
  rest of the functions. User functions must be at the end of hook
  (use add-hook's 3rd parameter)

Call arguments:

  cmd user msg string")

(defvar tinypgp-:cmd-macro-after-hook nil
  "Hook which run after the pgp sequence has been completed.
See function `tinypgp-cmd-macro' for arguments.

Note:

  First function that returns non-nil terminates running the
  rest of the functions. User functions must be at the end of hook
  (use add-hook's 3rd 'append parameter)

Call arguments:

  cmd user msg string

Call Note:

  if the CMD is 'cancel, then the function in this hook must not
  do any modification, but only restore any state that may have
  been opened in the *before* hook. (Eg. closing rmail-edit-mode).
  The 'cancel indicates that `error' command is about to be called soon.")

(defcustom tinypgp-:verify-before-hook nil
  "*Hook run before verify function is called.
Every function in the hook is called with 2 args: region-beg region-end
The function should return non-nil if it doesn't want to allow other
functions in the hook to continue."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:verify-after-hook nil
  "*Hook run when verify function is done.

Note:

  First function that returns non-nil terminates running the
  rest of the functions.

Call arguments:

  region-beg region-end verify-string-ret-val."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:read-email-after-hook '(tinypgp-email-substitution-default)
  "*This hook is called after email address list has been read.
The list is used e.g. for decrypting the message to multiple
users.

It may be desiradble to change some email address to something else;
supposes that you're sending encrypted message to foo2@site.com, but
you have key from him only that refers to email foo1@site.com. If you
try to encrypt according to \"To: foo2@site.com\" you get PGP error,
because there is no such key in the active keyring. That's why
you modify list and change the foo2@site.com to foo1@site.com.

Call arguments:

  email-list or string

Function should return:

  list                  ,original list if no changes.
  modified list"
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:insert-file-sign-base64-hook
  'ti::process-tar-zip-view-maybe-command
  "*Hook to run before the file is inserted to current point.
There is default function to this hook for tar/zip files, which inserts
the file listing into the buffer.

Note:

  If some function return non-nil the rest of the functions are not run.
  The buffer is temporary buffer for inserted data where hook is run,
  point sits at `point-min' and buffer holds the base64 signed file.

Call arguments:

  string : filename"
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:auto-action-before-hook nil
  "*Hook run before `tinypgp-auto-action' processes anything."
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:auto-action-defeat-hook '(tinypgp-auto-action-defeat-p)
  "*If any the functions return non-nil, the auto action is defeated.
Called from `tinypgp-auto-action'. The default function
`tinypgp-auto-action-defeat-p' inhibits processing MIME messages."
  :type  'hook
  :group 'tinypgp-hook)

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. others . .

(defcustom tinypgp-:finger-discard-email-hook
  '(tinypgp-finger-discard-by-regexp)
  "*This hook is called before finger spawned to fetch public key.
You should discard any email addresses that refer to your account.

Default function:
  `tinypgp-finger-discard-by-regexp'
  --> uses variable `tinypgp-:finger-discard-by-regexp'

Function call arguments:

  string or list of strings '(email email ..)

Function should return:

  modified list or string
  nil                       ,do not finger anything.

Example code, which is also the idea of default function:

  (add-hook 'tinypgp-:finger-discard-email-hook
            'my-tinypgp-finger-discard-email)

  (setq my-:tinypgp-me \"me.surname@\\|myOtherAccount@foo\\|3rd@bix.com\")

  (defun my-tinypgp-finger-discard-email (string-or-list)
    ;;  Discard addresses that point to me
    (require 'tinylibm)
    (let (ret)
      (mapcar
       (function
        (lambda (x)
          (if (not (string-match my-:tinypgp-me x))
              (push x ret))))
       ;; convert string to list if needed.
       (ti::list-make string-or-list))
      ret))"
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:find-by-guess-hook nil
  "*Functions called to find public key and keyring.

Notes:

  First function that return non-nil terminates calling other
  function in the hook

  PLEASE THINK CAREFULLY WHICH IS THE FIND ORDER, if you
  use your own function; use add-hook's 3rd parameter to
  add your methods last in the hook.

Function call arguments:

  string        usually email address(key id)

Function should return:

  string        (filename) keyring where the key is available
  nil"
  :type  'hook
  :group 'tinypgp-hook)

(defcustom tinypgp-:auto-action-encrypt-ok-hook  nil
  "*Hook to determine if sending auto encrypted mail is ok.
This hook is called only if some recipent matches
`tinypgp-:auto-action-encrypt-regexp'

Function call arguments:

  flag
  list          list of To and Cc recipients.

Function should return:

  boolean       non-nil says that encrypting is ok"
  :type  'hook
  :group 'tinypgp-hook)

;;; ...................................................... &v-function ...
;;; These are not in defcustom. Experts user know what to look
;;; for from source code if they need to change these.

(defcustom tinypgp-:encrypt-after-function  'tinypgp-encrypt-add-remailer-tag
  "Function run after the buffer is encrypted.
The default function `tinypgp-encrypt-add-remailer-tag' adds the
'Encrypted: PGP' tag to the beginning of encryopted block. It is needed
when the message is sent to remailer.

If you put your function inside this, be sure that you supply that tag
if you're writing message to remailer.

  ::
  Encrypted: PGP

  -----BEGIN PGP MESSAGE-----
  ...

Function call arguments:
  none

Function should return:

  none

Function call point

  at the beginnning of message"
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:filter-email-function 'tinypgp-mail-abbrevs-filter
  "*Function to filter out unwanted mailabbrevs.
When making the completion list of email address out of the
mail-abbrev table, the obarray may contain some _old_, unwanted, or
invalid email addresses. Perhaps you just don't want to have all
email addresses for PGP encryption: that's what this filter
function is for.

Call arguments:

  list      list of email addresses.

Return value:

  list      list of valid email addresses.")
  :type  'function
  :group 'tinypgp-pgp

(defcustom tinypgp-:verify-message-function nil
  "*Function called to print the verify status.
This function is called with one argument: STRING, when verify status
is displayed. For example; sometimes PGP could display

    Good signature from user \"0f00bc000\".

Which isn't quite enlightling. By supplying your own function you
can check cases like this and convert the message into something
more meaningful.

Example:

    (setq tinypgp-:verify-message-function 'my-tinypgp-verify-message)

    (defun my-tinypgp-verify-message  (str)
      \"Display more meaningful message\"
      (let* ((pfx \"Good signature from: \"))
        (cond
         ((string-match \"0f00bc095\" str)
          (setq str (concat pfx \"Foo Bar\"))))
        (message str)))"
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:pgp-encrypted-p-function 'tinypgp-pgp-encrypted-p-default
  "*Function to return PGP data type for message.
When you call `tinypgp-decrypt-mail' interactively, the
PGP type is asked. However, you can automate the type checking if you
know the type of PGP data.

The default function `tinypgp-pgp-encrypted-p-default' check the CTB bits
and return correct type.

Function arguments:

  none

Function should return:

  string        'pgp', 'base64', 'conventional' or nil"
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:pgp-decrypt-arg-function
  'tinypgp-decrypt-arg-function
  "*How to Honour variable `tinypgp-:decrypt-arg-interpretation'.
The default function `tinypgp-decrypt-arg-function' treats writable and read
only buffers differently."
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:pgp-command-compose-function nil
  "*Hook to run after pgp executable command has been composed.
If this hook doesn't modify the command, it should return CMD untouched.

function args:

  cmd      string

Should return:

  cmd      string"
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:secring-crypt-function 'tinypgp-crypt-do-with-pgp
  "*Function to crypt the secring.

Default values available:

  'tinypgp-crypt-do-with-pgp
  'tinypgp-crypt-do-with-crypt   ;; not recommended

Function args:

  from          source file
  to            destination file
  password      TO crypted by using this

Notes

  Function should detect by looking FROM file if it is already
  in encrypted format and convert it to back to regular file.
  Kinda flip-flop. It should also signal error and terminate if
  wrong password were used for opening the file (that is, if it
  is possible to determine that condition)"
  :type  'function
  :group 'tinypgp-pgp)

(defcustom tinypgp-:encrypt-with-function nil
  "*When message is encrypted, this function return additional keyIds.

For example if you want to encrypt all messages to yourself but only
when they are not sent to remailers, then you could use this setup.
All messages would be then readable by you also.

  (setq tinypgp-:encrypt-with-function  'my-tinypgp-encrypt-with)

  (defun my-tinypgp-encrypt-with ()
     (unless (ti::re-search-check \"remail\")
        ;;  Or your explicit PGP keyID if the name is not unique enough
        (list tinypgp-:user-primary)))

Function args:

 none

Should return:

  list of additional keyIds (strings) used in encryption or nil."
  :type  'function
  :group 'tinypgp-pgp)

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  remailer . .

(defcustom tinypgp-:r-post-before-hook nil
  "*Hook run before post is converted into Anon format."
  :type  'hook
  :group 'tinypgp-remail-hook)

(defcustom tinypgp-:r-post-after-hook nil
  "*Hook run after post is converted into Anon format."
  :type 'hook
  :group 'tinypgp-remail-hook)

(defcustom tinypgp-:nymserver-post-hook nil
  "*Hook run after `tinypgp-nymserver-post' function finishes."
  :type  'hook
  :group 'tinypgp-nymserver)

(defcustom tinypgp-:r-init-hook nil
  "*Hook run after the remailer support has been initialised.
See `tinypgp-r-init'."
  :type  'hook
  :group 'tinypgp-remail-hook)

(defcustom tinypgp-:r-get-list-hook nil
  "*Hook run after the Levien list file is inserted into temporary buffer.
This is your chance to check and modify the Remailer Levien list.
See `tinypgp-r-get-list'."
  :type  'hook
  :group 'tinypgp-remail-hook)

;;}}}
;;{{{ setup: mode variables

;;; .......................................................... &v-mode ...

(defvar tinypgp-mode nil
  "Minor mode variable.")

(make-variable-buffer-local 'tinypgp-mode)

(defvar tinypgp-:mode-name nil
  "Minor mode name.
This is not a user variable because the string is modified dynamically.")
(make-variable-buffer-local 'tinypgp-:mode-name)

(defcustom tinypgp-:mode-menu-name "TPgp"
  "*Menu name for pgp mode."
  :type 'string
  :group 'tinypgp-mode-definitions)

(defvar tinypgp-:mode-map nil
  "Minor mode map.")

(defvar tinypgp-:mode-menu nil
  "Menu for mode.")

(defcustom tinypgp-:mode-prefix-key "\C-c/"
  "*Key map prefix."
  :type  '(string :tag "Key sequence")
  :group 'tinypgp-mode-definitions)

;;; ................................................ &v-key-management ...

(defvar tinypgp-key-mode nil
  "Minor mode variable.")
(make-variable-buffer-local 'tinypgp-key-mode)

(defconst tinypgp-:key-mode-name nil
  "Minor mode name.
This is not a user variable because the string is modified dynamically.")
(make-variable-buffer-local 'tinypgp-:key-mode-name)

(defvar tinypgp-:key-mode-map nil
  "Minor mode map.")

(defvar tinypgp-:key-mode-menu nil
  "Menu for mode.")

(defcustom tinypgp-:key-mode-menu-name "TPk"
  "*Menu name for pgp key mode."
  :type  'string
  :group 'tinypgp-mode-definitions)

(defcustom tinypgp-:key-mode-prefix-key "\C-c'"
  "*Key map prefix."
  :type  'string
  :group 'tinypgp-mode-definitions)

;;; .................................................. &v-summary-mode ...

(defvar tinypgp-summary-mode nil
  "Minor mode variable.")
(make-variable-buffer-local 'tinypgp-summary-mode)

(defconst tinypgp-:summary-mode-name nil
  "Minor mode name. Changed dynamically.")
(make-variable-buffer-local 'tinypgp-:summary-mode-name)

(defvar tinypgp-:summary-mode-map nil
  "Minor mode map. \\[tinypgp-:summary-mode-map].")

(defvar tinypgp-:summary-mode-menu nil
  "Menu for mode.")

(defcustom tinypgp-:summary-mode-menu-name "TPsum"
  "*Menu name for mode."
  :type  'string
  :group 'tinypgp-mode-definitions)

(defcustom tinypgp-:summary-mode-prefix-key tinypgp-:mode-prefix-key
  "*Key map prefix."
  :type  'string
  :group 'tinypgp-mode-definitions)

;;; ................................................... &v-newnym-mode ...

(defvar tinypgp-newnym-mode nil
  "Minor mode variable.")
(make-variable-buffer-local 'tinypgp-newnym-mode)

(defvar tinypgp-:newnym-mode-name " Nym"
  "Minor mode name.")
(make-variable-buffer-local 'tinypgp-:newnym-mode-name)

(defvar tinypgp-:newnym-mode-map nil
  "Minor mode map. \\[tinypgp-:newnym-mode-map].")

(defvar tinypgp-:newnym-mode-menu nil
  "Menu for mode.")

(defcustom tinypgp-:newnym-mode-menu-name "TPnym"
  "*Menu name for mode."
  :type  'string
  :group 'tinypgp-mode-definitions)

;;  escreen.el uses same prefix; so change this if you use that package.
;;  Also the (enable-flow-control) takes over C-\ key.
;;
(defcustom tinypgp-:newnym-mode-prefix-key "\C-\\"
  "*Key map prefix."
  :type  'string
  :group 'tinypgp-mode-definitions)

;;; ................................................. &v-mode-remailer ...

;;  In HP the keys "/." are next to each other on the lower right
;;  near RET key
;;
(defcustom tinypgp-:mode-prefix-key-remailer "\C-c/.r"
  "*Key map prefix for remailer commands."
  :type  'string
  :group 'tinypgp-mode-definitions)

(defcustom tinypgp-:mode-prefix-key-newnym "\C-c/.n"
  "*Keymap prefix for newnym type anon server commands.
The default prefix key is C - c / . n; where p refers to (n)ewnym
account; similar to famous nym.alias.net"
  :type  'string
  :group 'tinypgp-mode-definitions)

(defcustom tinypgp-:mode-prefix-key-nymserver "\C-c/.y"
  "*Keymap prefix for nymserver type anon server commands.
The default prefix key is C - c / . y; where p refers to n(y)mserver
account; similar to ex-anon.penet.fi.

As of writing this,  the only active remailer that resembles 'penet' is
anon.nymserver.com"
  :type  'string
  :group 'tinypgp-mode-definitions)

;;}}}
;;{{{ setup: user config

;;; ........................................................ &v-config ...
;;; PGP executable settings

(defvar tinypgp-:pgp-binary-interactive-option
  (if (ti::win32-p)
      nil
    '(format "+comment=\"Processed by Emacs TinyPgp %s\""
             (tinypgp-version-number)))
  "*Extra encrypt option passed to PGP; use only \"+comment=\\\"\\\"\".
This variable is EVALUATED; so it can contain lisp FORM.
Must be nil in PCP platform.")

;;   See tinypgp-binary-header-field-fix

(put 'tinypgp-:pgp-binary-interactive-option
     'comment
     (format "Processed by %sEmacs TinyPgp %s"
             (if (ti::win32-p) "WinNT " "")
             (tinypgp-version-number)))

(put 'tinypgp-:pgp-binary-interactive-option 'original
     tinypgp-:pgp-binary-interactive-option)

;;; ....................................................... &v-pgp-exe ...

;;  This variable also has property
;;  'crypt          The absolute path for 'crypt'.
;;
;;  In variable tinypgp-:hash you find following properties
;;  Note: this is not in the hash table itself, but in symbol's plist,
;;  because hash table is resetted in regular intervals.
;;
;;  'secring-passwd
;;  'secring        The whole secring.pgp
;;
(defvar tinypgp-:pgp-binary nil
  "Property list of PGP executables.
The value of variable is always nil (not used). Property list values are:

  'version      string, PGP version number

  'ppg2         symbol 'ok if found
  'ppg2-type    symbol 'unix or 'win32

  'pgp5         symbol 'ok if found
  'ppg5-type    symbol 'unix or 'win32

  'gpg
  'gpg-type     symbol 'unix or 'win32

  'pgp          string, pgp 2.6.x executable path

  'pgp{koves}   string, pgp 5 executable paths

  'pgp-now      symbol 'pgp2, 'pgp5, `gpg;
                What pgp version is used currently
  'pgp-now-type 'unix 'win32
                What kind of pgp version is in use: Unix/Cygwin or Win32")

(defconst tinypgp-:pgp-binary-support-table
  '( ;; will generate a file with the specified filename, containing <nnn>
    ;; random bytes, to allow other programs to benefit from PGP's
    ;; strong random-number generator.

    (random  "+makerandom=")

    ;;  This prints trust parameters

    (trust   "-km"))
  "Support table of undocumented commands for your PGP binary.
These commands are usually available in 2.6.3, but they just aren't
include in PGP documentation.")

(defcustom tinypgp-:pgp-binary-charset "noconv"
  "*See PGP documentation.
If you change this value, you have to reload tinypgp.el.

Possible choices according to Pgp 2.6.3ia manual:

  noconv        No conversion  [prefer this]
  latin1        ISO 8859-1
  koi8          Eastern countries e.g. Russia
  cp850         ms-dos users in Europe"

  :type '(choice
          (const "noconv")
          (const "latin1")
          (const "koi8")
          (const "cp850"))
  :group 'tinypgp-pgp)

(defvar tinypgp-:pgp-sh-exe
  (let (path-win32
        path-unix)
    (when (ti::win32-p)
      (or (setq path-win32 (executable-find "cmdproxy.exe"))
          (error "\
TinyPgp: `tinypgp-:pgp-sh-exe' - cmdproxy.exe not in exec-path?")))

    (when (and (null (setq path-unix (executable-find "sh")))
               (not (ti::win32-p)))
      (error "\
TinyPgp: `tinypgp-:pgp-sh-exe' - /bin/sh not in exec-path?"))
    (list
     (list 'unix  (or path-unix "/bin/sh")
           (list 'win32 (or path-win32 "cmdproxy.exe")))))

  "*Shell executables. Use absolute path names for greater speed.

'((win32  \"cmdproxy.exe\")
  (unix   \"/bin/sh\"))")

;;; ......................................................... &v-files ...

;;; Please do not add exension to these files!
;;; --> PGP itself adds extension if it needs to create any additional files.
;;;
(defcustom tinypgp-:file-source
  (if (ti::win32-p)
      "c:/pgp-src"
    (tinypgp-path "pgp-src"))
  "*Source file fed to PGP. Region is written to this file.
Must reside in C:/ root directory in PC platform due to total command
length restrictions."
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-output
  (if (ti::win32-p)
      "c:/pgp-out"
    (tinypgp-path "pgp-out"))
  "*Output file produced by PGP when it gets `tinypgp-:file-source'.
Must reside in C:/ root directory in PC platform."
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-password
  (if (ti::win32-p)
      "c:/pgp-pwd"
    (tinypgp-path "pgp-pwd"))
  "*File where to save the password only during calling PGP.
The file is immediately deleted after PGP has finished.
Must reside in C:/ root directory in PC platform due to total command
length restrictions."
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-user-list
  (if (ti::win32-p)
      "c:/pgp-lst"
    (tinypgp-path "pgp-lst"))
  "*File where to store user list. (e.g. when encrypting).
Must reside in C:/ root directory in PC platform due to total command
length restrictions."
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-key-cache (tinypgp-path "tinypgp-cache")
  "*File where to store key cache."
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-secring
  (list
   (cons 'pgp2 (tinypgp-path "secring.pgp"))
   (cons 'pgp5 (tinypgp-path "secring.skr"))
   (cons 'gpg  (tinypgp-path "secring.gpg"
                             (list
                              (getenv "GNUPGHOME")
                              "~/.gnupg"))))
  "*Secring path. If you change this you must reload TinyPgp.
Format:
  '((pgp2 . \"/absolute/path/secring.pgp\")
    (pgp5 . \"/absolute/path/secring.skr\"))"
  :type  'file
  :group 'tinypgp-file)

(defcustom tinypgp-:file-secring-encrypted (tinypgp-path "secring.enc")
  "*Where to store the encrypted secring."
  :type  'file
  :group 'tinypgp-file)

;;; .......................................................... &v-user ...

(defcustom tinypgp-:user-primary
  (or (car-safe (ti::mail-email-from-string user-mail-address))
      (error
       "\
TinyPgp: tinypgp-:user-primary, Set user-mail-address to foo@site.com: '%s'"
       user-mail-address))
  "*Variable is used when you decrypt mail in buffer.

o  whatever your logical user id may
   be currently, it is changed to this

After decrypt has finished, the previous user identity is restored.
This should provide smooth processing of incoming encrypted messages,
while you may be doing something else."
  :type  'string
  :group 'tinypgp-mode)

(defcustom tinypgp-:user-identity-table nil
  "*When decrypting, this table is consulted for right active pgp user.

Format:

  '((\"key-hex-id\"  \"key-id\")
    ...)

The encrypted PGP ascii armor is examined and if the found
hex key-id match, then switch to key-i (usually mode descriptive
email string) as a current PGP user.

Example:

 (setq tinypgp-:user-identity-table
   '(
     ;;  My known public keyid firtsname.surname@site.com
     (\"12345670\"  \"firsname.surname\")

     ;;  If I receive pgp message from nymserver, then use my
     ;;  nymserver user id

     (\"12345678\"  \"an12345@anon.nymserver\")))"
  :type '(repeat
          (list
           (string :tag "Key-id (8 hex)"
                   (string :tag "Clear text User id"))))
  :group 'tinypgp-mode)

;;; .......................................................... &v-misc ...

(defcustom tinypgp-:register ?/
  "*Register used to store the contents of PGP output."
  :type  'character
  :group 'tinypgp-mode)

(defcustom tinypgp-:password-protection
  (if (ti::win32-p)
      nil
    t)
  "*If this variable is non-nil, use extra caution to protect the password.
Set this to t only if you're in UNIX system where
the processes commands can be seen by running 'ps'. This variable
prohibits using PGP's -z flag and forces using file descriptors
which cannot be snooped so easily.

Set the variable to t only if your PGP understands env variable PGPPASSFD
and that it can use many file descriptors.

In default WinNT this variable must be nil."
  :type  'boolean
  :group 'tinypgp-pgp)

(defcustom tinypgp-:password-keep-time (* 15 60) ;; 15 minutes default
  "*How many seconds to keep password in memory before forgetting it.
Set to nil, if you want to be asked password every time when you sign
a message."
  :type  'integer
  :group 'tinypgp-mode)

(defcustom tinypgp-:decrypt-arg-interpretation nil
  "*How to interpret prefix argument to `tinypgp-decrypt-mail' (interacive only).
This variable is used _only_ if function is called interactively.

If non-nil
  then meaning of the prefix arg passed to function
  `tinypgp-decrypt-mail' is reversed.

If 'preview
  As in non-nil but also the the content of the decrypted message is
  displayed in a separate buffer"
  :type  'boolean
  :group 'tinypgp-mode)

(defcustom tinypgp-:finger-discard-by-regexp (user-login-name)
  "*When fingering email addresses, discard those that match regexp.
Please look at variable `tinypgp-:finger-discard-email-hook' for more."
  :type  'regexp
  :group 'tinypgp-mode)

;;; ........................................................ &v-labels ...

(defcustom tinypgp-:label-table
  '((v      ("v+" "v-"))
    (s      "s")
    (d      "d")
    (e      "e")
    (pgp    "pgp"))
  "*Labels to attach to messages.
There are two ways to use labels in your mail agent. Here is one style,
where a general label is attached first and then the short flags. The advantage
of this is that you can summarise a) all 'pgp' labels b) summarise
'pgp' labels _and_ decrypted messages 'd'. See TinyRmail.el That adds
new command to RMAIL to do this kind of label _and_ operation.

   pgp,v+       PGP message, verified
   pgp,v+,d     PGP message, verified and decrypted
   pgp,v-,d     PGP message, verify failed and decrypted

Or you could leave out the general label out and mark each pgp actions with

  pgp+v
  pgp+v, pgp-d
  pgp-v, pgp-d

Choose your style, but remember that shortest labels are the best.

Format:
 '((v   (OK-VERIFY-LABEL NOK-VERIFY-LABEL)
   (s   SIGN-LABEL)
   (d   DECRYPT-LABEL
   (e   ENCRYPT-LABEL)
   (pgp PGP-GENERAL-LABEL)   << can be empty string or nil
   ))"
  :type '(list
          (list (const v :tag "verify") (list string string))
          (list (const s :tag "sign")    string)
          (list (const d :tag "decrypt") string)
          (list (const e :tag "encrypt") string)
          (list (const pgp :tag "pgp")   string))
  :group 'TinyPgp)

;;; ................................................... &v-tables-misc ...

(defcustom tinypgp-:pubring-table
  (let* ((file2  (tinypgp-path "pubring.pgp"))
         (file5  (tinypgp-path "pubring.pkr"))
         (gpg    (tinypgp-path "pubring.gpg"
                               (list
                                (getenv "GNUPGHOME")
                                "~/.gnupg"))))
    (if (and (not (file-exists-p file2))
             (not (file-exists-p file5))
             (not (file-exists-p gpg)))
        (error "\
TinyPgp: tinypgp-:pubring-table, Please configure, cannot auto-install.
File pubring.pgp or pubring.pkr couldn't be found. Check PGPPATH."))

    (list
     (list 'pgp2 (if file2
                     (list (list "default" file2 "-"))))
     (list 'pgp5 (if file5
                     (list (list "default" file5 "-"))))
     (list 'gpg (if gpg
                    (list (list "default" gpg "-"))))))
  "*Pubrings, alias names and mode line indicators.

Description:

  Possible pubrings user can select. Make sure your primary
  pubring is first in the list and that the others come
  in order of importance. The last one is least unimportant
  keyring. When searching for key, this is the search order.

To remember:

  Your primary pubring must be first.

  Your merged keyring must be last. When you're encrypting to
  multiple people, pgp needs one big pubring which contains
  all keys for those people that your encrypting the message. When
  program sees that you have multiple CC, BCC or To recipients,
  it automatically sets the active pubring to the last one in this
  list.

Tip:

  Please use some common convention when creating new pubring, e.g.
  use name \"pr-\" to denote pubring and add the descriptor after it.
  pr-elisp.pgp            ;; my pgp keys for my elisp mates
  pr-news.pgp             ;; occasional users from newsgroups
  pr-pgpnews.pgp          ;; the pgp newsgroup people pubring
  ...
  pr-temp.pgp             ;; temporary storage that I may discard any time
  pr-all.pgp              ;; Merged, pubring for all. maybe keyserver ring.

Format:

  '((BACKEND
     (COMPLETION-STRING PUBRING-FILE MODE-STRING) (COMP PUB-F MODE-S)
      ..)
    (BACKEND
     (COMPLETION-STRING PUBRING-FILE MODE-STRING) (COMP PUB-F MODE-S)
      ..))

  BACKEND is either 'pgp2 or 'pgp5

  COMPLETION-STRING is 'nice name' for the pubring.

  PUBRING-FILE is the absolute filename where pubring resides.

  MODE-STRING is displayed in the mode line to show which pubring
  you have active. Please choose some non-word character to give
  you enough visible hint which pubring you use currently. Eg.

    -      default
    =      secondary
    *      special, whole keyserver pubring."

  :type '(repeat
          (list
           (string :tag "Pubring completion name")
           (file   :tag "Pubring filename")
           (string :tag "String, One character modeline indicator")))
  :group 'tinypgp-mode)

(defcustom tinypgp-:header-sign-table
  ;;  Always use these fields
  '(("@" ("subject" "reply-to" )))
  "*List of headers that should be signed along with the message.

Format:

 '((REGEXP '(HEADER-NAME-STRING HEADER-NAME-STRING ..) [NO-XPGP-MODE])
   ...)

Example:

Definition of fields:

REGEXP        If matches To-field or Newsgroup-field, the HEADER-LIST is used.
              You should not sign reply-to field if the destination
              address changes the field contents. Many times mailing
              list do this.
HEADER-LIST   If it is empty, no headers are included in signing.
NO-XPGP-MODE  This field is optional. If it is non-nil, then when you
              do signing, this flag is consulted. If the to-field matches
              the no X-Pgp signing is done, no matter what the
              `tinypgp-:xpgp-signing-mode' says currently. In some cases
              you can't send X-pgp signed messages to the destination
              address.

References:

  `tinypgp-:xpgp-signing-mode'"
  :type '(repeat
          (list
           (regexp :tag "Regexp matching To/Newsgroups")
           (repeat (string :tag "Header field to be signed"))))
  :group 'tinypgp-header)

(defcustom tinypgp-:keyserver-mail-table
  '(
    ;; official
    ("pgp"                  "pgp-public-keys@keys.pgp.net")

    ;; 1998-03 http://www.prairienet.org/~jalicqui/pgpfaq.txt
    ("uk"  "pgp-public-keys@keys.uk.pgp.net")
    ("de"  "pgp-public-keys@keys.de.pgp.net")
    ("no"  "pgp-public-keys@keys.no.pgp.net")
    ("us"  "pgp-public-keys@keys.us.pgp.net")
    ("nl"  "pgp-public-keys@keys.nl.pgp.net")
    ("fi"  "pgp-public-keys@keys.fi.pgp.net")
    ("es"  "pgp-public-keys@keys.es.pgp.net")
    ("hr"  "pgp-public-keys@keys.hr.pgp.net")
    ("tw"  "pgp-public-keys@keys.tw.pgp.net")
    ("pl"  "pgp-public-keys@keys.pl.pgp.net")
    ("au"  "pgp-public-keys@keys.au.pgp.net"))
  "*List of available Email keyservers.
See PGP faq \"8.2. What public key servers...\" for updated list.
http://www.pgp.net/mail-help/email-help-en.html

Format:
 '((COMPLETION-NAME EMAIL-ADDRESS))
   (COMP-N EMAIL-A)
   ..)"
  :link '(url-link :tag "PGP keyservers"
                   "http://www.pgp.net/mail-help/email-help-en.html")
  :type '(repeat (list string string))
  :group 'tinypgp-interface)

;;  http://geronimo.uit.no/pgp/servruit.eng.html
;;  http://www-swiss.ai.mit.edu/~bal/bal-home.html
;;
(defcustom tinypgp-:keyserver-http-table
  '(

    ;; Maintainer: <grobi@uni-paderborn.de>
    ;; http://math-www.uni-paderborn.de/pgp/

    ("wwwkeys.pgp.net:11371"
     "/pks/lookup?op=get&search=%s")

    ;;  Hm, this is nowadays PGP 5 keyserver

    ("pgp.ai.mit.edu"
     "/htbin/pks-extract-key.pl?op=get&search=%s")

    ("goliat.upc.es:1137" ;; <marc@mit.edu>
     "/pks/lookup?op=index&search=%s"))
  "*List of available http keyservers.
Be sure that you put your nearest/fastest keyserver first in the list.
It is offered as default connection.

Format:

  '((KEYSERVER COMMAND)
    (KEYSERVER COMMAND)
    ...)"
  :type  '(repeat (list string string))
  :group 'tinypgp-interface)

;;; ................................................... &v-auto-action ...

(defcustom tinypgp-:sign-mail-p-function nil
  "*Function to decide if message should be signed.
Auto signing mode is active when function is called.
See `tinypgp-sign-mail-auto-mode'.

Function return values:

  t     Yes, sign this mail
  nil   Ignore signing for this message

Example:

  ;;  Do not sign messages that are sent to my fellow
  ;;  workers at domain 'foo'. Ie. sign messages to the outside
  ;;  world.

  (setq tinypgp-:sign-mail-p-function
    '(lambda ()
       (not (string-match \"foo\" (or (mail-fetch-field \"to\") \"\")))))"
  :type  'sexp
  :group 'tinypgp-mode)

(defcustom tinypgp-:auto-action-encrypt-regexp nil
  "*Bulk encryption regexp to match all members in To, CC, BCC.
This is special auto action variable and it is used only if
there is more than _one_ address where you're sending a message.
Typical situation: You want to send encrypted mail to Cc'd
members who also have pgp.

The regexp s matched individually against each member in To, Cc and Bcc
fields. If regexp didn't match for each member, then the auto encryption
is not engaged.

You must know for sure who have pgp and those people's keys must
be stored in big pubring 'all'. (see `tinypgp-:pubring-table')"
  :type  'regexp
  :group 'tinypgp-mode)

(defcustom tinypgp-:auto-action-table nil
  "*Automatic encrypt and sign control table.
When there is only _one_ email destination (no CC, BCC and one entry in To)

Alternative way, see also:
  `tinypgp-:bbdb-field'
  Note: `tinypgp-:auto-action-table' overrides BBDB

Format:

 '((EVAL-OR-REGEXP  [SIGN-KEY-ID] [ENCRYPT] [MIME] [XPGP] [KEYRING])
   ..)

Example:

 ;;  To automatically send PGP/MIME encrypted messages to
 ;;  foo and bar, signed by you:

 '((\"foo@bar.com\" 'my-pgp-key-id@site.com 'encrypt 'mime)
   (\"bar@bar.com\" 'my-pgp-key-id@site.com 'encrypt 'mime))

Definition of fields:

  EVAL-OR-REGEXP: string or lisp list
  Regexp means matching on To field contents.
  *Note* To field must have address including @ character otherwise no
  comparison is done.

  If you use EVAL, then you can refer to variable 'to-field' in the
  form that builds up eval. You can also search the buffer for specific
  strings before determining if the actions defined should be
  engaged.

  SIGN-KEY-ID: string or symbol
  If this contains string key-id, that is used to sign the message. If
  value is SYMBOL instead of string _and_ the ENCRYPT is non-nil, then the
  result is 'one pass' encrypt and sign and not a separate encrypt + sign.

  ENCRYPT: boolean
  If non-nil, the message is encrypted according to TO field content.
  See also `tinypgp-:email-substitution-table' if you want to encrypt
  using some other key.

  MIME: symbol
  nil    use Regular pgp
  'mime  use PGP/MIME interface with TM or SEMI if mime interface is present.

  XPGP: boolean
  If non-nil means signing by using X-Pgp headers;
  if this is nil, then use regular signing. This overrides any existing mode.

  KEYRING: string; absolute filename
  tells which file to use as pubring when doing the encryption/signing.
  It defaults to current keyring in use.

  The following example demonstrates EVAL-use of this variable, there are
  three entries in this list.

  o   If newsgroup field is found from the message and
      it matches to pgp groups, then sign every mail.
  o   if to-field matches person foo@site.com, then the mail
      is encrypted and signed. (in this order)
  o   If message is sent somewhere else than my current domain,
      sign it.

  (setq tinypgp-:auto-action-table
    '(
      ;;  elt 1
      ((let ((grp  (mail-fetch-field \"Newsgroups\")))
                    ((string-match \"pgp\" (or grp \"\"))))
        \"me@foo\")

      ;; elt 2
      (\"foo@site.com\" \"me@foo\" 'enc)

      ;; elt 3
      ((not (string-match \"@mysite.com\" to-field))
       \"me@foo\")))

Note 1:

  If message _already_ contains some Pgp data (signed; encrypted)
  this variable is not used, because it's supposed that the user is
  controlling the layout of PGP message.

Note 2:

  This variable is used only if you send message to _one_
  destination. If any CC or BCC is found from the message or if To:
  field contains comma, then this variable is not used.

Note 3:

  All mode settings are overridden. Toggling modes on/off do not
  affect auto-action command.

Note 4:

  The order of regexp elements is important: first one matched is used
  and the rest of the list is forgotten."

  :type  '(repeat
           (list
            (sexp       :tag "To field regexp")
            (boolean    :tag "Sign flag")
            (boolean    :tag "X-pgp flag")
            (file       :tag "Keyring file used:")))
  :group 'tinypgp-mode)

;;}}}
;;{{{ setup: header

;;; ........................................................ &v-header ...

(defcustom tinypgp-:xpgp-signing-mode nil
  "*Non-nil if X-Pgp signing is used.

References:
  `tinypgp-:header-sign-table'   ,this overrides `tinypgp-:xpgp-signing-mode'

See \\[tinypgp-xpgp-header-mode-toggle]"
  :type  'boolean
  :group 'tinypgp-header)

(defcustom tinypgp-:xpgp-user-info
  '(format "Comment= \"Processed by Emacs TinyPgp.el %s\""
           (tinypgp-version-number))
  "*Additional information added to X-Pgp header.
Set this variable to STRING-OR-EVAL-FORM that you wish to include
in X-Pgp. The correct keywords are defined in X-Pgp standard.
ftp://cs.uta.fi/pub/ssjaaa/pgp-xhd.html#additional_keywords:_telling_how_to

Notes:

  DO NOT put newline code at the end of string.
  Put 2 spaces before each statement line (except first line).
  Every keyword must end to semicolon!
  Enclose strings in double quotes.

Full Example:

  (setq tinypgp-:xpgp-user-info
   '(concat
     \"  Fingerprint=\\\"12 92 9C E4 60 DF 62 CD FC AD 18 47 9A 74 E7 D1\\\";\\n\"
     \"  Length=1024; Id=0x17D57681;\"
     \"  Access-type=Finger; Address=foo@site.com;\\n\"
     (format \"  Comment=\\\"Processed by Emacs TinyPgp.el %s\\\";\"
            (tinypgp-version-number))))

Recommended Example (only essential keywords):

  (setq tinypgp-:xpgp-user-info
   '(concat
     \" Id=0x17D57681; Access-type=Finger; Address=foo@site.com;\\n\"
     (format \"  Comment=\\\"Processed by Emacs TinyPgp.el %s\\\";\"
            (tinypgp-version-number))))"
  :type  '(sexp :tag "String of Form")
  :group 'tinypgp-header)

;;}}}
;;{{{ setup: remail private

;;; ....................................................... &vp-remail ...
;;; Private

(defvar tinypgp-:r-levien-table nil
  "Updated by program. List of remailers and their properties.")

(defvar tinypgp-:r-host-table nil
  "Updated by program. List of accepted remailers and their properties.")

(defvar tinypgp-:r-history nil
  "History.")

;; Raph's list is not always right. This variable is for experts only
;; and you should not touch it if you don't what you're doing.
;;
(defconst tinypgp-:r-control-list nil
;;;  '(("replay"   ("ek"))                      ;does no support this
;;;    ("dustbin"  nil ("post"))                ;supports this
;;;    ("haystack" nil ("post"))                ;supports this
;;;    )
  "List of remailers and additional property control.

Format:

  '((REMAILER (REMOVE-PLIST) (ADD-PLIST))
    (REMAILER ..))

  For each remailer a property is either removed or added.

Example:

  (setq tinypgp-:r-control-list
         ;; dustbin supported one day the property post.
        '((\"dustbin\" nil (\"post\"))))")

(defvar tinypgp-:r-mode-indication-flag nil
  "Non-nil means that current message should be treated with caution.
Eg. if you encrypt the message, there will be no extra
PGP 'Comment' keywords included that may reveal your identity.")

(make-variable-buffer-local 'tinypgp-:r-mode-indication-flag)
(put 'tinypgp-:r-mode-indication-flag 'permanent-local t)

;;}}}
;;{{{ setup: remail user config

;;; ................................................... &v-remail-hook ...

(defcustom tinypgp-:r-post-before-hook '(tinypgp-r-post-before-default)
  "*Things to do before converting message to anonymous format.
Turn off/exit all minor modes that may interfere the process."
  :type  'hook
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-reply-block-basic-hook nil
  "*Hook that is run after reply block is added."
  :type  'hook
  :group 'tinypgp-remail-hook)

;;; ........................................................ &v-remail ...
;;; User config

(defcustom tinypgp-:r-list-file
  (let ((file "~/.remailer.lst"))

    ;;  Suppose we have low quota account; use .gz file if it exists.
    ;;  the regular file is not checked here: it is checked when
    ;;  user uses the remail functions.

    (if (file-exists-p (concat file ".gz"))
        (concat file ".gz")
      file))

  "*Remailer list file. See `tinypgp-r-update-remailer-list'."
  :type  'file
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-user-mail-address user-mail-address
  "*Email address of your reply block.
This account may be different from your regular email address."
  :type  'string
  :group 'tinypgp-remail)

;;  1998-01  #finger also rlist@anon.lcs.mit.edu
(defcustom tinypgp-:r-list-finger "remailer-list@kiwi.cs.berkeley.edu"
  "*Finger address where to get updated remailer list."
  :type  'string
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-mail2news-remailer "replay"
  "*Remailer alias through which you want to send you Usenet posts.
Variable is not a email address, but the remailer alias name according
to Levien remailer list. This variable can contain lisp FORM.

Must support properties: POST PGP HASH CUTMARKS."
  :type  'string
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-chain nil
  "*Remailer chain table. List of remailer.
Only remailers that at least have properties PGP HASH EK are allowed.

Format:

 '((COMPLETION-NAME
    [vector                 or (lisp-form-to-evaluate; must return vector)
     (REMAILER
      latent-time           this is optional
      encrypt-key)          this is optional
     (REMAILER
      ...)
     ])
    ...)

Examples:

  (defconst tinypgp-:r-chain
    '((\"1-way\"     [(\"replay\" \"+0:05r\" \"ZepHyR1x\")])
      ;;  Select random path
      (\"hide\"      (progn (shuffle-vector [(\"replay\") (\"dustbin\")])))
      ;;  Use some reliable remailer, but hide identity better
      (\"milkyway\"  [\"replay\" \"replay\" \"replay\"])))"
  :type  'sexp
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-subject-table
  '(" dummy"
    " This is a test"
    " ignore this message"
    " Regarding your previous message"
    " As for the www and html..."
    " Re: about the last subject..."
    " Re: Programming langueges.. ")
  "*List of dummy Subject sentences that are used in your Remailer message.
The subject should be such that it doesn't draw your sysadm's attention."
  :type '(repeat (list string))
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-reply-block-table nil
  "*Correct reply block for each remailer.
Suggested filename could be ~/.r-dustbin for dustbin remailer.
You can place anything after the last -----END PGP MESSAGE-----
because the reply block is only read from `point-min' to this
tag line and rest of the file is ignored.

File format:

    ::
    Request-Remailing-To: remailer@replay.com
    Latent-Time: +0:00

    ::
    Encrypted: PGP

    -----BEGIN PGP MESSAGE-----
    Version: 2.6.3ia

    hIkDPRWysueuweUBA+jLifdDpkCxcUYA
    ...
    -----END PGP MESSAGE-----

    #
    # this is comment
    # this is another comment
    # end of file

Variable format:

'((REMAILER-ALIAS FILE)
  (R F)
  ..
  )"
  :type '(repeat
          (list
           (string :tag "Remailer alias")
           (file   :tag "Reply block File")))
  :group 'tinypgp-remail)

(defcustom tinypgp-:r-header-keep-list '("Gcc" "Fcc")
  "*In addition to strict RFC headers; keep these headers too.
When you compose anon post, all the unnecessary headers will be
killed so that your identity is not revealed by accident.
This is list f headers that are preserved in addition to RFC headers.
Please do not include colon or spaces.

Format:
  '(\"hdr1\" \"hdr2\" ..)"
  :type '(repeat string)
  :group 'tinypgp-remail)

;;; ........................................................ &v-newnym ...

(defcustom tinypgp-:r-newnym-stamp-file-prefix
  (tinypgp-path "~/.emacs.tinypgp-stamp.")
  "*Newnym type accounts expire in 120 days.
This file is touched every time user sends a newnym account
request or remail. It is compared to current sate and a warning
is issued after 100 days if user hasn't used the account.

The filenames are manfgled to protect reading the Newnym server and
account information from them.

User must send `request' message to the account to keep it alive."
  :type  'file
  :group 'tinypgp-newnym)

(defcustom tinypgp-:r-newnym-default-account-table nil
  "*List of newnym servers and accounts you have.
The active default server and login information are stored
into properties 'default-server and 'default-account. If these properties
are nil, then no default values are set.

Format:
  '((COMPLETION-NAME  NYM-SERVER NYM-ACCOUNT MODELINE-CHAR)
    (.. .. ..))
   Important: NYM-ACCOUNT must not have @site.suffix.com; only the account name

Example:
  '((\"weasel\" \"weasel\"  \"my-weasel-login-name\" \"W\")
    (\"nym\"    \"nym\"     \"my-nym-login-name\"    \"N\")
    (\"nym2\"   \"nym\"     \"my-nym-login-name2\"   \"N2\")
    (\"efga\"   \"efga\"    \"my-efga-login-name\"   \"E\"))"
  :type  '(repeat (list string string string))
  :group 'tinypgp-newnym)

(defcustom tinypgp-:r-newnym-mail2news-address
  "mail2news_nospam@anon.lcs.mit.edu"
  "*Email address through which the newsgroup posts are sent.
This variable is evaled to get the email address.

Aug 13 1997 there was a list of gateways available at
http://students.cs.byu.edu/~don/mail2news.html and the list below
is copied from there.

Note:

  The default value is mail2news_nospam@anon.lcs.mit.edu which creates
  headers like this:

       From: Bogus Name <Use-Author-Address-Header@[127.1]>
       Author-Address: Name <AT> nym <DOT> alias <DOT> net

  If you would use regular mail2news_nospam@anon.lcs.mit.edu; then your
  headers were as they would. But expect to get UCE mail through your newnym
  account as soon as you post to usenet.

       From: Sam Bogus <name@nym.alias.net>  ????

Sites that scan headers:

  mail2news@anon.lcs.mit.edu CONFIRMED Jun97
  mail2news@news.wsnet.com NOT FUNCTIONAL

Sites that parse the email address:

  group.name.usenet@alpha.jpunix.com DO NOT USE
  m2n-YYYYMMDD-group.name+group.name@alpha.jpunix.com CONFIRMED Aug97
  post-group.name@newspost.zippo.com CONFIRMED Mar97
  group.name@news.cs.dal.ca PROBABLY NOT FUNCTIONAL
  no.group.name@news.uninett.no
    (uninett only reported to carry norwegian news) CONFIRMED Jul96
  group.name@news.uni-stuttgart.de CONFIRMED Mar97
  mail2news-YYYYMMDD-group.name+group.name@anon.lcs.mit.edu CONFIRMED Jul97
  group.name@myriad.alias.net CONFIRMED Jun97"
  :type  'string
  :group 'tinypgp-newnym)

(defcustom tinypgp-:r-newnym-help-file nil
  "*Remailer 'newnym' help file."
  :type  'file
  :group 'tinypgp-newnym)

(defconst tinypgp-:newnym-cmd-table
  '(("acksend"
     "per-message: automatic acknowledgment of successfully remailed message."
     "Default: -acksend")
    ("signsend"
     "per-message: automatic PGP signing of any outgoing mail."
     "Default: -signsend")
    ("cryptrecv"
     "automatic encryption with your nym's public key."
     "Default: +cryptrecv")
    ("fixedsize"
     "all messages padded to exactly the same size (roughly 10K)"
     "Default: -fixedsize")
    ("disable"
     "4 Megabytes per day disables account, notified if this happens."
     "Default: -disable. Re-enable account with -disable.")
    ("fingerkey"
     "Allow people to finger <yournym@weasel.owl.de> for you PGP key."
     "Default: -fingerkey")
    ("name"
     "\
Describe text of nym >> From: YOUR-NAME-DESC-HERE <yournym@weasel.owl.de>"
     "Default: name=\"\". Example: name=\"Your Alias Name\"")
    ("create"
     "\
Create fails if a nym exists. Use Create? for updating nym. (sign message)."
     "Example: create/create?")
    ("delete"
     "Deletes your alias and wipes your reply block. Acknowledged."
     "<no other options>")
    ("nobcc"
     "\
Counce bcc, only  To, Cc, Resent-To, or Resent-Cc accepted. (SPAM protect)"
     "Default: -nobcc"))
  "Newnym command table.
Format:
 '((COMMAND DESC cmd-example-or-default-value)
   (COMMAND DESC cmd-example-or-default-value)
   ..)")

;;; ....................................................... &vp-remail ...
;;; Private variables.

(defvar tinypgp-:r-reply-block-cache nil
  "Reply block cache.

Format:
 '((BUFFER PGP-BEG PGP-END)
   (B P-B P-E)
   )")

;;}}}
;;{{{ setup: Nymserver

;;; ....................................................... &nymserver ...

(defcustom tinypgp-:nymserver-request-encrypt nil
  "*To send every command to 'nymserver' account in encrypted format.
NOTE: You must have inserted the Server's PGP key into the keyring."
  :type  'boolean
  :group 'tinypgp-nymserver)

;;;  Currently this is not user variable
;;;  There is only one nymserver type remailer currently active.
;;;
(defconst tinypgp-:nymserver-table
  '(("nymserver"
     tinypgp-nymserver-create-1 "request@anon.nymserver.com"
     "anon@anon.nymserver.com"
     3))
  "Table of 'nymserver' type services.
Format:

  '((SERVER-ALIAS-STRING
     ACCOUNT-CREATE-FUNCTION
     ACCOUNT-CREATE-EMAIL-ADDRESS
     SERVER-EMAIL-POST-TO
     NEWSGROUP-POST-COUNT-LIMIT)
         (S A A S N )
    ..)")

;;;  When you receive account creation confirmation; update
;;;  this variable immediately.
;;;
(defcustom tinypgp-:nymserver-account-table nil
  "*Your nymserver account information table.

'((SERVER-ALIAS-STRING
   ACCOUNT-EMAIL
   ACCOUNT-PASSWORD
   [ACCOUNT-NICKNAME-STRING | nil ]
   [FROM-ADDRESS            | nil ]
   [HELP-FILE               | nil ]))

You get the slots ACCOUNT-EMAIL, ACCOUNT-PASSWORD when you order an
account from the server. The ACCOUNT-NICKNAME-STRING can be nil,
because nymserver also controls your Nickname. This overrides
the server's value.

FROM-ADDRESS

    is important. When you ordered account from nymserver, it
    allocates only your current address and handles only messages sent from
    that address.

       aa@a.com  --> you ordered anon account here.
       bb@b.com  you have another normal account here
       cc@c.com  you have yet another normal account here

    Suppose you want to post from account bb@b.com as anon. Can't
    do that because nymserver expects you to be only in aa@a.com, in the
    site where you initially ordered the anon account.

    Now, if you set FROM-ADDRESS to aa@a.com, then the From-field is inserted
    into the message pretending that the mail is coming from aa@a.com and
    now you can use your Anon account from different sites.

HELP-FILE

    If the E-mail message that contained the server manual which explains
    all its features. Store the mail to this file;

Example:

 (defconst tinypgp-:nymserver-account-table
  '((\"nymserver\"
     \"an1111@anon.nymserver.com\"
     \"qF8asdd\"
     \"\"
     \"my.name@address.com\"
     \"~/txt/nymserver.hlp\"
     )))"
  :type '(list
          (const "nymserver" :tag "Server")
          (string :tag "Account email")
          (string :tag "Account ppassword")

          ;; optional
          ;;
          (string :tag "nickname")
          (string :tag "From address")
          (file   :tag "Help file"))
  :group 'tinypgp-nymserver)

;;}}}

;;{{{ setup: private

;;; ...................................................... &vp-private ...

(defvar tinypgp-:timer-elt nil
  "Timer process that e.g. expires passwords.")

(defvar tinypgp-:key-cache nil
  "Cache: '((key-id, pubring, public-key) (...)).")

(defvar tinypgp-:key-cache-last nil
  "Last accessed 'get element in cache. See function `tinypgp-key-cache'.
  (ORIGINAL-EMAIL (CACHE-KEY PUBRING ..))")

(defvar tinypgp-:return-value nil
  "Common return value between functions.
This variable is used as a signal to TinyPgp when it has called
some user function or hook. The usage is explained in
the functions that use it. It will contain properties too.")

(defvar tinypgp-:buffer-tmp-shell "*tinypgp-shell-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-finger " *tinypgp-finger-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-copy " *tinypgp-copy-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-article " *tinypgp-article*"
  "Temporary buffer.
If user doesn't want to replace the contents of the
buffer in mail-like modes, then the content is copied to
this buffers first, so that any text properties or overlays can beremoved
without invoking edit mode.")

(defvar tinypgp-:buffer-tmp-http " *tinypgp-http-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-kring " *tinypgp-kring-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-show " *tinypgp-show-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-tmp-mail " *tinypgp-mail-tmp*"
  "Temporary mail buffer.")

(defvar tinypgp-:buffer-tmp " *tinypgp-tmp*"
  "Temporary buffer.")

(defvar tinypgp-:buffer-newnym "*mail-newnym*"
  "Newnym remailer mail buffer.")

(defvar tinypgp-:buffer-comint "tinypgp-comint"
  "Interactive comint buffer to talk with PGP.
This buffer name will automatically have stars over the name.")

(defvar tinypgp-:buffer-view "*tinypgp-view*"
  "Interactive comint buffer to talk with PGP.
This buffer name will automatically have stars over the name.")

(defvar tinypgp-:original-buffer nil
  "Original buffer storage. Set in macro `tinypgp-run-in-tmp-buffer'.")

(defvar tinypgp-:pgp-email-list nil
  "List of email addresses in ~/.emailrc.")

(defvar tinypgp-:pgp-email-abbrev-list nil
  "List of abbrevs and their expansions: '((\"abb\" . \"expa\") ..).")

(defvar tinypgp-:pgp-email-list-completions nil
  "Email assoc menu for completion. '((\"a@b.com\" . 1) ..)
This variable is initialised to the contents of your
~/.mailrc file.")

(defvar tinypgp-:sign-data nil
  "Stored sign information for current message.
Used for checking message tampering afterwards.

Format:
  number   ,message body length in characters.")
(make-variable-buffer-local 'tinypgp-:sign-data)
(put                        'tinypgp-:sign-data 'permanent-local t)

;;; ....................................................... &vp-colors ...

(defvar tinypgp-:face-mark 'highlight
  "The face for text marking.")

(defvar tinypgp-:face-error 'bold
  "The face for pointing out errors.")

;;; ...................................................... &vp-history ...

(defvar tinypgp-:history-key-info nil
  "History of used key info strings.")

(defvar tinypgp-:history-email nil
  "User email history.")

(defvar tinypgp-:history-newnym-account nil
  "Nym account name history.")

(defvar tinypgp-:history-r-chain nil
  "Remailer chain selection history.")

(defvar tinypgp-:history-r-chain nil
  "Remailer chain selection history.")

(defvar tinypgp-:history-http-keyserver nil
  "History of used key servers.")

(defvar tinypgp-:history-http-keyserver-string nil
  "History of used key server search strings.")

;;; ..................................................... &vp-commands ...

(defconst tinypgp-:pgp-command-options
  (let* ((charset (ti::string-remove-whitespace
                   (if (ti::nil-p tinypgp-:pgp-binary-charset)
                       "noconv"
                     tinypgp-:pgp-binary-charset)))
         (secring (cdr (assq 'pgp2 tinypgp-:file-secring))))
    (concat

     ;;  These options are best to left out from the commands in WinNT.
     ;;  The maximum command line parameter length is 255 or was it 180 ?
     ;;
     ;;  The PGP compress ratio is like 1,6M text --> 600k

     (if (ti::win32-p)
         ""
       (concat
        (if secring
            (concat " +secring=" secring)
          "")
        " "
        " +nomanual"
        " +showpass=off"                ;There is no cmd line arg
        " +encrypttoself=off"

        " +verbose=1"
        " +language=en"                 ;don't use language modules
        " +armorlines=0"                ;No separate UU chunks
        " +charset=" charset
        " "))
     ""))
  "Default 2.6.x options for every pgp command.
Notice that in PC platform there may be restrictions.")

;; #todo: tinypgp-:pgp-command-options5 for Unix?

(defconst tinypgp-:pgp-command-options5
  (let* ((charset (ti::string-remove-whitespace
                   (if (ti::nil-p tinypgp-:pgp-binary-charset)
                       "noconv"
                     tinypgp-:pgp-binary-charset))))
;;;      (secring (cdr (assq 'pgp5 tinypgp-:file-secring)))

    (if (ti::win32-p)
        ""
      (concat
       " +headers"
       " +encrypttoself=off "
       " +compress=on "
       " +language=en "                 ;don't use language modules
       " +armorlines=0 "                ;No separate UU chunks

       ;; " WarnOnMixRSADiffieHellman=on "
       ;; " WarnOnRSARecipAndNonRSASigner=on "

       ;; batchmode; You must not add this to the switches, because
       ;; then PGP 5.x won't ask for pass phrase, but expects to get
       ;; it from PGPPASSFD. We don't use PGPASSFD in Unix, but the expect.el
       ;; will feed the pass phrase to the prompt.
       ;;
       ;; " +batchmode=1"
       ;;
       ;; " -v "   ;; Verbose mode

       " +charset=" charset
       " ")))
  "Default 5.x options for every pgp command.
Notice that in PC platform there may be restrictions.")

(defconst tinypgp-:gpg-command-table
  (let* ((common2 tinypgp-:pgp-command-options) ;Without batch mode
         (common
          (concat
           common2
           " --batch "
           " "))
         (passwd-scheme
          (concat
           " echo #password #bin "
           common
           " --passphrase-fd 0 ")))

    (list
     ;; Encrypt and output ascii #todo
     (list
      'encrypt-info
      (concat common " -f -u xx_test -z xx_test #SOURCE-FILE"))

     (list
      'encrypt
      ;; Multiple users
      (concat passwd-scheme
              " #OUT-FILE -e -a #MUSER #SOURCE-FILE  "))

     (list
      'encrypt-sign
      (concat common
              " --textmode --sign -e -a #MUSER #PGP-USER #password"))

     (list
      'decrypt
      (concat passwd-scheme " #OUT-FILE --decrypt #USER #SOURCE-FILE "))

     (list
      'decrypt-base64
      (concat (concat common " -f  ")))

     (list
      'crypt
      (concat common2 " -a -c "))

     (list
      'sign
      (concat
       passwd-scheme
       " #OUT-FILE --textmode --clearsign  -a #USER #SOURCE-FILE "))

     (list
      'sign-detach
      (concat common " -bsatf #USER #password "))

     (list
      'verify
      (concat common " --verify "))

     (list
      'key-get
      (concat common " -kaf "))

     (list
      'key-info
      (concat common "  -kvc "))

     (list
      'key-add
      (concat common "  -fka "))

     (list
      'key-extract
      (concat common "  -fkxa "))

     (list
      'key-generate
      (concat common2 "  -kg "))

     (list
      'key-delete
      (concat common " +force  -kr "))

     (list
      'key-remove
      (concat common " +force  -kr "))

     (list
      'key-sign-a
      (concat common " +force  #USER -ks "))

     (list
      'key-sign-b
      (concat common " +force  #USER -krs "))))
  "GPG 1.0.4 command table.")

(defconst tinypgp-:pgp-command-table
  (let* ((common2 tinypgp-:pgp-command-options) ;Without batch mode
         (common
          (concat
           common2
           " +batchmode "
           " ")))

    ;;  To use a Unix-style filter  mode,  reading  from  standard
    ;;  input and writing to standard output, use -f option

    ;;  converted to recipient's local text
    ;;  line conventions,  add  the  -t  (text)

    (list
     ;; Encrypt and output ascii ascii
     (list
      'encrypt-info
      (concat common " #PUBRING -f -u xx_test -z xx_test #SOURCE-FILE"))

     (list
      'encrypt
      ;; Multiple users
      (concat common " #PUBRING -eatf #SOURCE-FILE #MUSER "))

     (list
      'encrypt-sign
      (concat common " #PUBRING -eatfs #MUSER #PGP-USER #password"))

     (list
      'decrypt
      (concat common " #PUBRING -f #password "))

     (list
      'decrypt-base64
      (concat common " -f  "))

     (list
      'crypt
      (concat common2 " -a -c "))

     (list
      'sign
      (concat common " #PUBRING -satf #USER #password "))

     (list
      'sign-detach
      (concat common " #PUBRING -bsatf #USER #password "))

     (list
      'verify
      (concat common " #PUBRING -f "))

     (list
      'key-get
      (concat common " #PUBRING -kaf "))

     (list
      'key-info
      (concat common " #PUBRING -kvc "))

     (list
      'key-add
      (concat common " #PUBRING -fka "))

     (list
      'key-extract
      (concat common " #PUBRING -fkxa "))

     (list
      'key-generate
      (concat common2 " #PUBRING -kg "))

     (list
      'key-delete
      (concat common " +force #PUBRING -kr "))

     (list
      'key-remove
      (concat common " +force #PUBRING -kr "))

     (list
      'key-sign-a
      (concat common " +force #PUBRING #USER -ks "))

     (list
      'key-sign-b
      (concat common " +force #PUBRING #USER -krs "))))
  "PGP 2.6.x command table.")

;; #todo: #PUBRING is not in the switches.
;; #todo: I have no idea if these work in Unix

(defconst tinypgp-:pgp-command-table5
  (let* ((common tinypgp-:pgp-command-options5))
    (list
     ;; Encrypt and output ascii ascii
     (list
      'encrypt-info
      (concat common " -f -u xx_test -z xx_test #SOURCE-FILE"))

     (list
      'encrypt
      (concat common " -atf #OUT-FILE  #MUSER #SOURCE-FILE"))

     (list
      'encrypt-sign
      (concat
       common
       " -atf -s #OUT-FILE #USER #MUSER #password #SOURCE-FILE "))

     (list
      'decrypt
      (concat common " -f  #OUT-FILE  #password #SOURCE-FILE"))

     (list
      'decrypt-base64
      (concat common " -f #OUT-FILE #SOURCE-FILE"))

     (list
      'crypt
      (concat common " -a -c  #SOURCE-FILE"))

     (list
      'sign
      (concat
       common
       " -atv #USER #password #OUT-FILE #SOURCE-FILE "))

     (list
      'sign-detach
      (concat
       common
       " -b -atv #USER #password #OUT-FILE #SOURCE-FILE "))

     (list
      'verify
      ;;  option -z requires pass phrase argument.
      ;;
      (concat common "  #OUT-FILE #SOURCE-FILE "))

     (list
      'key-get
      (concat common " -kaf "))

     (list
      'key-info
      (concat common " -ll "))

     (list
      'key-add
      (concat common " -a "))

     (list
      'key-extract
      (concat common " -xa "))

     (list
      'key-generate
      (concat common " -g "))

     (list
      'key-delete
      (concat common " -r "))

     (list
      'key-remove
      (concat common " -kr "))

     (list
      'key-sign-a
      (concat common " #USER -ks "))

     (list
      'key-sign-b
      (concat common "#USER -krs "))))
  "PGP 5.0.x command table.")

(defconst tinypgp-:pgp-binary-exit-code-table
  '((pgp2 .
          (
           ;; Possible error exit codes - not all of these are used.  Note that
           ;; we don't use the ANSI EXIT_SUCCESS and EXIT_FAILURE.  To make
           ;; things easier for compilers which don't support enum we use
           ;; #defines

           (0  'EXIT_OK "JumBoJumboMamboBaile")
           (1  'INVALID_FILE_ERROR)
           (2  'FILE_NOT_FOUND_ERROR)
           (3  'UNKNOWN_FILE_ERROR)
           (4  'NO_BATCH)
           (5  'BAD_ARG_ERROR)
           (6  'INTERRUPT)
           (7  'OUT_OF_MEM)
           ;; /* Keyring errors: Base value = 10 */
           (10 'KEYGEN_ERROR )
           (11 'NONEXIST_KEY_ERROR)
           (12 'KEYRING_ADD_ERROR)
           (13 'KEYRING_EXTRACT_ERROR)
           (14 'KEYRING_EDIT_ERROR)
           (15 'KEYRING_VIEW_ERROR)
           (16 'KEYRING_REMOVE_ERROR)
           (17 'KEYRING_CHECK_ERROR)
           (18 'KEY_SIGNATURE_ERROR)
           (19 'KEYSIG_REMOVE_ERROR)
           ;; /* Encode errors: Base value = 20 */
           (20 'SIGNATURE_ERROR)
           (21 'RSA_ENCR_ERROR)
           (22 'ENCR_ERROR)
           (23 'COMPRESS_ERROR)
           ;; /* Decode errors: Base value = 30
           (30 'SIGNATURE_CHECK_ERROR)
           (31 'RSA_DECR_ERROR)
           (32 'DECR_ERROR)
           (33 'DECOMPRESS_ERROR))))
  "Error codes of PGP versions.
Format:
 '((PGP-VERSION-REGEXP .((EXIT-CODE ERROR-SYMBOL [ERROR-REGEXP] ..)))
   (P-V-R . ((EX ES ER) (EX ES ER) ..))))

If ERROR-REGEXP is not specified, then ERROR-SYMBO should be used to
show the error to user.")

(defconst tinypgp-:pgp-binary-error-regexp
  (concat
   "Bad pass phrase"
   "\\|user ID is required"
   "\\|Unable to get terminal"
   "\\|Transport armor stripping failed"
   "\\|Encryption error"
   "\\|No such file or directory"
   "\\|Cannot find the public key"
   "\\|Output file.*already exists"
   "\\|You do not have the secret key needed to decrypt this file\\."
   "\\|We need to generate.*bits" ;; Can I handle this in the prg?

   ;;  If you encrypt with multiple keys, then missing key is flagged

   "\\|This user will not be able to decrypt this message"

   "\\|Key matching userid.*not found in file"
   "\\|Key matching.*not found in file"

;;; Signature validation....
;;;   "\\|Key matching expected Key ID.*not found in file"

   "\\|Keyring extract error\\."

   ;;  When removing keys...

   "\\|Do you also want to remove it from.*[?]"

   ;; When you try to verify detached sig file and say that some file
   ;; XXX holds sig (when it doesn't)

   "\\|Error:.*is not a ciphertext, signature, or key file."

   ;; Eg. From conventional crypt error

   "\\|You need a pass phrase to decrypt this file"

   ;;  PGP 5.x Error!  Unable to load string PRIVATE_KEY_MISSING.

   "\\|Error!.*Unable to load string.")
  "All error messages from PGP executable.
These are case sensitive sentences.")

(defconst tinypgp-:pgp-binary-error-regexp-quiet
  (concat
   "Bad pass phrase"
   "\\|Cannot find the public key"
   "\\|Key matching userid.*not found in file"
   "\\|Key matching.*not found in file")
  "List of errors that does not bring up the Shell Error buffer.
The buffer contain the last PGP executable call.
Consider these errors so familiar that you don't have to
examine the shell error message better.")

(defvar tinypgp-:error nil
  "Last error message.")

(defvar tinypgp-:last-pgp-exe-command nil
  "Last command sent to PGP exe.")

;;; .................................................... &vp-pass-hash ...

(defvar tinypgp-:hash-password (make-vector 127 0)
  "Stored passwords, expired periodically.")

;;  Some variables must be stored locally, but some variables must
;;  be globally visible; becfause on error conditions the
;;  current buffer may have changed and in order to restore
;;  situation, we must do lookup from GLOBAL array, because we don't
;;  know any more what was the starting buffer.
;;
(defvar tinypgp-:hash nil
  "General _local_  hash storage.")
(make-variable-buffer-local 'tinypgp-:hash)

(defvar tinypgp-:hash-global nil
  "General _global_ hash storage.")

(defvar tinypgp-:secring-crypt-mode nil
  "If Non-nil, use encrypted secring.
This is NOT A USER VARIABLE. Use \\[tinypgp-secring-crypt-mode-toggle]
Variable's value should not be trusted at all; but instead set it
by calling function `tinypgp-secring-crypt-mode-detect' and only
then trusting the value.")

;;; ......................................................... &vp-misc ...

(defvar tinypgp-:header-sign-smf-info nil
  "The header SMF data that was constructed is stored here.")

(defvar tinypgp-:pubring-now nil
  "Current pubring in use.
This will be initialised in `tinypgp-backend-select'")

(defvar tinypgp-:user-now
  (let* ((em user-mail-address))
    (cond
     ((not (stringp em))
      (error "\
TinyPgp: tinypgp-:user-now, user-mail-address is not str like foo@site.com"))

     ;;  If you have <> in user-mail-address that messes up From
     ;;  field.

     ((string-match "<.*@.*>" em)
      (error "\
TinyPgp: tinypgp-:user-now, please remove <> from user-mail-address")))
    (car (ti::mail-email-from-string em)))
  "Current user.")

(defvar tinypgp-:last-network-error nil
  "Last finger call error text.")

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ Bug report

;;; ........................................................... &debug ...

(defvar tinypgp-:debug-buffer-size 100000
  "The buffer size after which the debug buffer is emptied.
If you don't see all the information, increase size.")

(defvar tinypgp-:debug t
  "*Debug flag.")

(defvar tinypgp-:debug-buffer "*tinypgp-debug*"
  "*Debug buffer.")

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypgpd (&rest args)
  "Generate debug if debug is on and output ARGS."
  (`
   (when tinypgp-:debug
     (let* ( ;; write to package's private buffer.
            (ti:m-debug-buffer tinypgp-:debug-buffer))
       (save-match-data
         (ti::d!! (,@ args) "\n")
         ;;  don't let it grow without limit....
         (with-current-buffer ti:m-debug-buffer
           (if (and (integerp tinypgp-:debug-buffer-size)
                    (> (buffer-size) tinypgp-:debug-buffer-size))
               (erase-buffer))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-debug-buffer-clear ()
  "Clear the debug buffer."
  (interactive)
  (ti::temp-buffer tinypgp-:debug-buffer 'clear)
  (if (interactive-p)
      (message "TinyPgp: Debug buffer cleared.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-password-wipe-buffer (&optional force)
  "Wipe password strings from buffer. This may not succeed if cache is empty.
if passwords are not in cache any more this function is no-op.

If FORCE is non-nil ask interactively.
If force is nil, then get the passwords from cache and

Normally getting passwords from cache is performed in `mail-send-hook'"
  (interactive (list (interactive-p)))
  (let ((fid  "tinypgp-password-wipe-buffer:")
        passwd
        serv-passwd)

    (cond
     (force
      (setq passwd (tinypgp-password-set "\
I need pass phrase to wipe out all references to it: "))

      (setq
       serv-passwd (tinypgp-nymserver-password
                    (tinypgp-nymserver-ask
                     "Nymserver server you have used: "))))
     (t
      ;; is it there?
      (if (ti::vector-table-get tinypgp-:hash-password tinypgp-:user-now)
          (setq passwd (ti::vector-table-property
                        tinypgp-:hash-password tinypgp-:user-now 'password)))
      ;;
      ;;  Actually there may be multiple passwords if user has several PGP
      ;;  keys (common, if you use remailers)
      ;;
      ;; #todo: We don't know nymserver password, because it is not in hash

      (mapatoms
       (function
        (lambda (x)
          (when x)))
       ;;  (ti::d! x)
       tinypgp-:hash-password)))

    ;; finally, scramble any pass pharases, so that they are not sent
    ;; to Maintainer!

    (ti::save-line-column-macro nil nil
      (when (stringp passwd)
        (ti::mail-hmax 'move)
        (replace-string passwd "#PASSWD-WAS-HERE"))

      (when (stringp serv-passwd)
        (ti::mail-hmax 'move)
        (replace-string serv-passwd "#PASSWD-WAS-HERE-ANON")))
    (tinypgpd fid "out:" (current-buffer))
    ;;  Clean return value
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-submit-bug-report ()
  "Submit bug report or feedback.
When you call this function it automatically includes all needed
buffers. Please leave the *Backtrace* buffer before you call this function
and it will be copied too.

If this is feedback call, then do not include any extra buffers.
\[Answer 'n' when to insert questions]"
  (interactive)
  (ti::package-submit-bug-report
   "tinypgp.el"
   tinypgp-:version-id
   '(tinypgp-:version-id

     message-send-hook
     mail-send-hook
     message-send-hook

     mail-mode-hook
     rmail-mode-hook
     vm-mode-hook
     vm-version
     message-mode-hook
     gnus-article-mode-hook
     gnus-version
     news-reply-mode-hook
     mh-show-mode-hook
     mh-letter-mode-hook
     mh-before-send-letter-hook
     mh-show-hook
     mh-e-version

     tinypgp-:load-hook
     tinypgp-:mode-hook
     tinypgp-:turn-on-hook-list
     tinypgp-:sig-from-header-hook
     tinypgp-:sig-to-header-hook
     tinypgp-:sign-loose-info-hook
     tinypgp-:key-mode-define-keys-hook
     tinypgp-:do-command-region-before-hook
     tinypgp-:do-command-region-after-hook
     tinypgp-:cmd-macro-before-hook
     tinypgp-:cmd-macro-after-hook
     tinypgp-:verify-before-hook
     tinypgp-:verify-after-hook
     tinypgp-:read-email-after-hook
     tinypgp-:find-by-guess-hook
     tinypgp-:finger-discard-email-hook
     tinypgp-:r-post-before-hook
     tinypgp-:r-post-after-hook
     tinypgp-:nymserver-post-hook
     tinypgp-:r-post-before-hook
     tinypgp-:r-reply-block-basic-hook
     tinypgp-:define-keys-hook

     tinypgp-:pgp-encrypted-p-function
     tinypgp-:decrypt-arg-interpretation
     tinypgp-:pgp-decrypt-arg-function
     tinypgp-:pgp-command-compose-function
     tinypgp-mode
     tinypgp-:mode-name
;;;       tinypgp-:mode-menu-name
;;;       tinypgp-:mode-map
     tinypgp-:mode-prefix-key
     tinypgp-:mode-prefix-key-remailer
     tinypgp-:mode-prefix-key-nymserver
     tinypgp-key-mode
;;;       tinypgp-:key-mode-map
;;;       tinypgp-:key-mode-menu
     tinypgp-:key-mode-menu-name
     tinypgp-:key-mode-prefix-key
     tinypgp-:xpgp-user-info
     tinypgp-:pgp-binary-charset
     tinypgp-:pgp-sh-exe
     tinypgp-:pgp-binary
     tinypgp-:file-source
     tinypgp-:file-output
     tinypgp-:file-password
     tinypgp-:file-user-list
     tinypgp-:file-key-cache
     tinypgp-:file-secring-encrypted
     tinypgp-:face-mark
     tinypgp-:face-error
     tinypgp-:register
     tinypgp-:finger-discard-by-regexp
     tinypgp-:password-protection
     tinypgp-:password-keep-time
     tinypgp-:user-primary
     tinypgp-:filter-email-function
     tinypgp-:sign-mail-p-function

;;; Do not send this to maintainer!
;;;       tinypgp-:user-identity-table
     tinypgp-:header-sign-table
;;;       tinypgp-:keyserver-mail-table
     tinypgp-:auto-action-table
     tinypgp-:pubring-table
     tinypgp-:r-levien-table
     tinypgp-:r-host-table
     tinypgp-:r-history
     tinypgp-:r-mode-indication-flag
     tinypgp-:r-list-file
     tinypgp-:r-user-mail-address
     tinypgp-:r-list-finger
     tinypgp-:r-newnym-help-file
     tinypgp-:r-mail2news-remailer
     tinypgp-:r-chain
     tinypgp-:r-reply-block-table
     tinypgp-:r-reply-block-cache
     tinypgp-:nymserver-request-encrypt
     tinypgp-:nymserver-account-table
     tinypgp-:debug
     tinypgp-:debug-buffer-size
;;;     tinypgp-:key-cache
     tinypgp-:key-cache-last
     tinypgp-:return-value
     tinypgp-:buffer-tmp-shell
     tinypgp-:buffer-tmp-finger
     tinypgp-:buffer-tmp-copy
     tinypgp-:buffer-tmp-http
     tinypgp-:buffer-tmp-kring
     tinypgp-:buffer-tmp-show
     tinypgp-:buffer-tmp-mail
     tinypgp-:buffer-tmp
     tinypgp-:buffer-comint
     tinypgp-:buffer-view
     tinypgp-:original-buffer
     tinypgp-:xpgp-signing-mode
     tinypgp-:history-email
;;;       tinypgp-:pgp-email-list
;;;       tinypgp-:pgp-email-abbrev-list
;;;       tinypgp-:pgp-email-list-completions
     tinypgp-:sign-data
     tinypgp-:history-key-info
     tinypgp-:error
     tinypgp-:last-pgp-exe-command
;;;       tinypgp-:hash-password
;;;       tinypgp-:hash
     tinypgp-:header-sign-smf-info
     tinypgp-:pubring-now
     tinypgp-:user-now
     tinypgp-:last-network-error
     tinypgp-:nymserver-echo-menu-use-p

     tinypgp-:key-mode-name
     tinypgp-:pgp-binary-interactive-option
     tinypgp-:pgp-binary-support-table
;;;       tinypgp-:keyserver-http-table
     tinypgp-:r-control-list
;;;       tinypgp-:r-subject-table
     tinypgp-:nymserver-table))
;;;       tinypgp-:pgp-command-table
;;;       tinypgp-:pgp-binary-error-regexp
;;;       tinypgp-:nymserver-echo-menu

  (save-excursion

    (ti::pmax)

    (when (get-buffer "*Backtrace*")
      (insert "\n\n#BACKTRACE BEGIN-------------\n")
      (insert-buffer (get-buffer "*Backtrace*")) (ti::pmax)
      (insert "\n\n#BACKTRACE END-------------\n"))

    (when (and (get-buffer tinypgp-:buffer-tmp-shell)
               (y-or-n-p "Insert PGP shell buffer contents? "))
      (insert "\n\n#SHELL BEGIN-------------\n")
      (insert-buffer (get-buffer tinypgp-:buffer-tmp-shell)) (ti::pmax)
      (insert "\n\#SHELL END-------------\n"))

    (cond
     ((or
       (and
        (null (get-buffer tinypgp-:debug-buffer))
        (y-or-n-p
         "No debug buffer: Are you sure maintainer doesn't need it? "))
       (y-or-n-p "Insert the debug buffer contents too? "))
      (insert "\n\n#DEBUG BEGIN-------------\n")
      (insert-buffer tinypgp-:debug-buffer) (ti::pmax)
      (insert "\n\#DEBUG END-------------\n")))

    (tinypgp-password-wipe-buffer 'force))
  (ti::read-char-safe-until
   "[press]Please check that your pass phrase wasn't included..."))

;;}}}
;;{{{ macros: test-p

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-now ()
  "Return 'gpg 'pgp2 or 'pgp5"
  (get 'tinypgp-:pgp-binary 'pgp-now))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-type (&optional backend)
  "Return BACKEND type: 'unix or 'win32."
  (let* ((prop (intern (concat (symbol-name
                                (or backend
                                    (tinypgp-backend-now)))
                               "-type"))))
    (get 'tinypgp-:pgp-binary prop)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-file (file)
  (concat file  "."  (symbol-name (tinypgp-backend-now))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-pgp2-p  ()
  "Return non-nil is if pgp 2.6.x is in use."
  (eq (tinypgp-backend-now) 'pgp2))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-gpg-p  ()
  "Return non-nil is if gpg is in use."
  (eq (tinypgp-backend-now) 'gpg))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-list ()
  "Return available backends: 'pgp2 'pgp5"
  (get 'tinypgp-:pgp-binary 'pgp-backends))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-exist-pgp2 ()
  "Return non-nil if pgp2 is available"
  (memq 'pgp2 (tinypgp-backend-list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-backend-exist-pgp5 ()
  "Return non-nil if pgp5 is aailable"
  (memq 'pgp5 (tinypgp-backend-list)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-sign-data-same-p ()
  "Compare previous signing info against current buffer content.
If this function returns non-nil, the buffer has been changed and
it should be resigned."
  (eq (ti::mail-message-length) tinypgp-:sign-data))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-sign-data-set ()
  "Store sign information."
  (if (ti::mail-mail-p)
      (setq tinypgp-:sign-data (ti::mail-message-length))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-sign-mail-auto-mode-on-p ()
  "Check if auto sign is active."
  (memq 'tinypgp-sign-mail-func  mail-send-hook))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-mail-buffer-p (&optional msg-flag)
  "Check if buffer look like mail message.
Non-nil MSG-FLAG displays message if test is nil."
  ;;  Gnus uses message-mode
  ;;
  (if (ti::mail-mail-p)
      t
    (tinypgpd "tinypgp-mail-buffer-p")
    (when msg-flag
      (message "This PGP action is available only in mail, news")
      (sit-for 1))
    nil))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-hidden-p ()
  "Check if the PGP BLOCK is hidden.
Return:
  nil
  (point . invisible-property-value)"
  (let* ((point (point-min))              ;Before widen
         (pmax  (+ (point-max) (* 80 6))) ;lookahead about 6 full lines
         pos
         prop)
    ;;  first find our property. Then see if it's invisible
    ;;
    (when (and
           ;;  In RMAIL buffer this widens a lot!
           (ti::widen-safe
             (setq pos (text-property-any
                        point
                        ;; Select lookahead or point-max.
                        ;; In RMAIL the pmax is selected.
                        ;;
                        (min pmax (point-max))
                        'owner 'tinypgp)))
           (setq prop (get-text-property pos 'invisible)))
      (cons pos prop))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-user-list  (&optional list)
  "Add to LIST users from `tinypgp-:encrypt-with-function'."
  (let* ((add (if tinypgp-:encrypt-with-function
                  (funcall tinypgp-:encrypt-with-function))))
    (if add
        (ti::list-merge-elements list add)
      list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-user-find-current ()
  "Find current user.

If buffer is read-only (supposing RMAIL, VM):

  look at the PGP stream in buffer and consult `tinypgp-:user-identity-table'.

If buffer is writable:

  Do nothing special."
  (let ((fid    "tinypgp-user-find-current:")
        (type   (tinypgp-hash 'action 'get 'now nil 'global))
        key-id
        elt)

    (tinypgpd fid "TYPE" type "READ-ONLY" buffer-read-only (buffer-name)
              "remail" tinypgp-:r-mode-indication-flag)

    (cond
     ((or (and (not (member (buffer-name) '("RMAIL" "INBOX")))
               (not buffer-read-only))
          tinypgp-:r-mode-indication-flag)
      nil)
     (t
      (setq type (save-excursion
                   (ti::pmin)
                   (ti::mail-pgp-stream-forward-and-study)))
      (tinypgpd fid type)
      (when (and (eq (car type) 'enc)
                 (setq key-id (nth 3 type))
                 (inline
                   (setq elt
                         (ti::list-find
                          tinypgp-:user-identity-table key-id))))
        (nth 1 elt))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-interactive-enable 'lisp-indent-function 2)
(defmacro tinypgp-interactive-enable (type)
  "Check TYPE condition and display MSG if function usage is prohibited."
  (`
   (cond
    ((eq (, type) 'remail)
     (unless (tinypgp-install-menu-bar-remail)
       (message "You haven't configured TinyPgp to use remailers yet.")
       (sit-for 1)
       (error "See TinyPgp Manual and 'tinypgp-:r-levien-table'")))

    ((eq (, type) 'newnym)
     (unless (tinypgp-install-menu-bar-newnym)
       (message "\
You haven't ordered newnym account or configured TinyPgp to use it.")
       (sit-for 1)
       (error "See TinyPgp Manual and 'tinypgp-:r-levien-table'")))

    ((eq (, type) 'nymserver)
     (unless (tinypgp-install-menu-bar-nymserver)
       (message "\
You haven't ordered nymserver account or configured TinyPgp to use it.")
       (sit-for 1)
       (error "See TinyPgp Manual and `tinypgp-:nymserver-account-table'")))
    (t
     (error "Not know type. %s" (, type))))))

(defsubst tinypgp-r-i-enable ()
  "Interactive check."
  (tinypgp-interactive-enable 'remail))

(defsubst tinypgp-newnym-i-enable ()
  "Interactive check."
  (tinypgp-interactive-enable 'newnym))

(defsubst tinypgp-nymserver-i-enable ()
  "Interactive check."
  (tinypgp-interactive-enable 'nymserver))

;;}}}
;;{{{ macros: misc and inline defsubst

;;; .......................................................... &macros ...
;;; Macros must be defined before used --> keep them at the top of file

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-email-or-string (string)
  "Return email address from STRING or STRING itself."
  (or (ti::string-match "[^< \t]+@[^ >\t]+" 0 string)
      string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-comint-buffer ()
  "Return comint buffer name."
  (concat "*" tinypgp-:buffer-comint "*"))

;;; ----------------------------------------------------------------------
;;; - This "stringifies" a regexp :-)
;;;
(defsubst tinypgp-cnv (string)
  "Remove possible anchor tag or other RE tags from STRING."
  (replace-regexp-in-string "[\n\r?$^]+" "" string))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-table ()
  "Return backend's pubring table."
  (or (nth 1 (assq (tinypgp-backend-now) tinypgp-:pubring-table))
      (error "tinypgp-:pubring-table is corrupt. No backend %s: %s"
             (tinypgp-backend-now)
             tinypgp-:pubring-table)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-set-big ()
  "Set `tinypgp-:pubring-now' to point to big pubring."
  (setq tinypgp-:pubring-now
        (nth 1 (car (reverse (tinypgp-pubring-table))))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypgp-do-shell-env (&rest body)
  "Execute BODY in specific shell environment."
  (`
   (let* ((pgp-type (tinypgp-backend-type))
          (shell    (nth 1 (assq pgp-type tinypgp-:pgp-sh-exe)))
          (explicit-shell-file-name  (or shell
                                         explicit-shell-file-name
                                         shell-file-name))
          (shell-file-name           (or shell shell-file-name)))
     (if (null explicit-shell-file-name) ;; nop-op Quiet XE ByteCompiler
         (setq explicit-shell-file-name nil))
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-save-state-macro 'lisp-indent-function 0)
(defmacro tinypgp-save-state-macro (&rest body)
  "Save key values of program and execute BODY."
  (`
   (let ((TINYPGP-user  tinypgp-:user-now) ;Mixed case: Prevent variable suicide
         (TINYPGP-userp tinypgp-:user-primary)
         (TINYPGP-pring tinypgp-:pubring-now)
         (TINYPGP-h-s-t tinypgp-:header-sign-table)
         (TINYPGP-x-s-m tinypgp-:xpgp-signing-mode))
     (prog1 (progn (,@ body))
       (setq tinypgp-:user-now           TINYPGP-user
             tinypgp-:user-primary               TINYPGP-userp
             tinypgp-:pubring-now                TINYPGP-pring
             tinypgp-:header-sign-table  TINYPGP-h-s-t
             tinypgp-:xpgp-signing-mode  TINYPGP-x-s-m)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-clone-buffer ()
  "Copy content of current buffer to `tinypgp-:buffer-tmp-article'."
  (tinypgp-copy-to-buffer (tinypgp-ti::temp-buffer 'article)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-copy-to-buffer (buffer)
  "Copy content of current buffer to BUFFER and remove all properties."
  (let ((data-buffer (current-buffer)))
    (tinypgpd "tinypgp-copy-to-buffer" buffer)
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert-buffer data-buffer)

      ;; SIG may be hidden; Gnus hides headers with properties

      (ti::buffer-text-properties-wipe (point-min) (point-max))
      (ti::overlay-remove-region (point-min) (point-max))
      buffer)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-run-in-tmp-buffer  'lisp-indent-function 1)
(defmacro tinypgp-run-in-tmp-buffer (buffer &rest body)
  "Use BUFFER, which is copy of current buffer, and do BODY.
All text properties in the copy are removed. If BUFFER is nil,
then use internal temporary buffer.

Note:
  The `set-buffer' command leaves pointer to copy buffer.

References:
  `tinypgp-:original-buffer' is set to buffer from where the text was copied."
  (`
   (let ((Data-buffeR           (current-buffer))
         BuffeR)
     (setq BuffeR (or (, buffer) (tinypgp-ti::temp-buffer 'copy)))
     (tinypgpd "tinypgp-run-in-tmp-buffer" BuffeR)

     (setq tinypgp-:original-buffer Data-buffeR) ;save position
     (tinypgp-copy-to-buffer BuffeR)

     (with-current-buffer BuffeR
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-set-pgp-env-macro 'lisp-indent-function 2)
(defmacro tinypgp-set-pgp-env-macro (user-list &optional verb &rest body)
  "Set environment. Find correct keyring and switch to it temporarily.
But only if USER-LIST length is 1; if list is longer, use big pubring
that holds all keys. The VERB parameter must also be set. Do BODY.

Error is signalled if we can't find keyring."
  (`
   (tinypgp-save-state-macro
    ;;  Let's be a little user friendly and try finding the key
    ;;
    (let ((user   (cond
                   ((stringp (, user-list))
                    (, user-list))

                   ((and (ti::listp (, user-list))
                         (eq 1 (length (, user-list))))
                    (car (, user-list)))))
          kring)

      (when (, verb)

        (cond
         (user
          (if (not (setq kring (tinypgp-key-find-by-guess user)))
              (error "Sorry, can't set keyring '%s'. Fetch key first." user)
            (tinypgpd "tinypgp-set-pgp-env-macro" (, user-list) kring )
            (setq tinypgp-:pubring-now kring)))

         ((ti::listp (, user-list))
          ;;  Multiple users, set pubring to point to BIG RING
          ;;
          (tinypgpd "tinypgp-set-pgp-env-macro: LAST KRING")
          (tinypgp-pubring-set-big))))
      (tinypgpd "tinypgp-set-pgp-env-macro: BODY " verb (, user-list)
                tinypgp-:pubring-now)

      (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-user-change-macro  'lisp-indent-function 0)
(defmacro tinypgp-user-change-macro (&rest body)
  "Change pgp user if From field address match `tinypgp-:user-identity-table'.
If there is no From field or match this macro does nothing to BODY."
  (`
   (let (UseR)
     (if (setq UseR (tinypgp-user-find-current))
         (setq tinypgp-:user-now UseR))
     (tinypgpd "tinypgp-user-change-macro: " tinypgp-:user-now)
     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-name2alias (str table)
  "Return Nth 0 when NTH 1 STR is given from TABLE."
  (let* (ret)
    (dolist (elt table)
      (when (string= str (nth 1 elt))
        (setq ret elt)
        (return)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-alias2name (str table)
  "Return Nth 1 when NTH 0 STR is given from TABLE."
  (nth 1 (assoc str table)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-x-headers-deinstall ()
  "Move X-pgp signature to normal format (if X-pgp exist)."
  (tinypgpd "tinypgp-x-headers-deinstall")
  (if (ti::mail-pgp-headers-p)
      ;;   Move X-pgp headers to their normal places
      ;;
      (tinypgp-signature-from-header)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-complete (&optional prompt init)
  "Read the pubring name with PROMPT and INIT. Return nil or selected string."
  (let ((ans
         (completing-read
          (or prompt "Select pubring: ")
          (ti::list-to-assoc-menu (mapcar 'car (tinypgp-pubring-table)))
          nil
          'require-match
          init)))
    (if (ti::nil-p ans)
        nil
      ans)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-alias2file (name)
  "Find real pubring behind completion NAME."
  (if name
      (tinypgp-expand-file-name
       (nth 1 (assoc name (tinypgp-pubring-table))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-file2alias (name)
  "Find alias for real pubring NAME. Return nil if no match."
  (let* (ret)
    (setq name (tinypgp-expand-file-name name))
    (dolist (elt (tinypgp-pubring-table))
      (when (string= name (tinypgp-expand-file-name (nth 1 elt)))
        (setq ret (car elt))
        (return)))
    (or ret
        (error "Can't find alias for: %s"))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-list ()
  "Return all pubrings known to program."
  (let (list)
    (dolist (elt (tinypgp-pubring-table))
      (push (tinypgp-expand-file-name (nth 1 elt)) list))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-many-p ()
  "Return non nil if there are many pubrings."
  (> (length (tinypgp-pubring-table)) 1))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-default ()
  "Return first pubring< which is supposed to be default."
  (nth 1 (car (tinypgp-pubring-table))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-pubring-change-to-current ()
  "Change to pubring relative to current user.
Calling function should possibly save the `tinypgp-:pubring-now'."
  (setq tinypgp-:pubring-now
        (or (tinypgp-key-find-by-cache tinypgp-:user-now)
            tinypgp-:pubring-now)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-user-change-to-primary ()
  "Change current variable settings to reflect primary user.
The calling function should copy the key values of TinyPgp
before calling this function.

This also changes the pubring.

Reference:
  `tinypgp-save-state-macro'"
  (tinypgpd "tinypgp-user-change-to-primary" tinypgp-:user-primary )
  (setq tinypgp-:user-now    tinypgp-:user-primary)
  (setq tinypgp-:pubring-now
        (tinypgp-expand-file-name
         (if (tinypgp-key-find-by-cache
              tinypgp-:user-now)
             (nth 1 (car (tinypgp-pubring-table)))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-randseed-file ()
  "Return randseed filename."
  (or (getenv "RANDSEED")
      (format "%s/%s"
              (tinypgp-expand-file-name (or (getenv "PGPPATH") "~/.pgp"))
              "randseed.bin")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-finger-email-filter (list)
  "Filter out unwanted entries from email LIST."
  (when list
    (setq  list (tinypgp-email-discard-default list))
    (when tinypgp-:finger-discard-email-hook
      (setq list (run-hook-with-args-until-success
                  'tinypgp-:finger-discard-email-hook list)))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-find-region (beg end)
  "Read all email addressed from BEG END and filter out unwanted ones.
See. `tinypgp-:finger-discard-email-hook'."
  (tinypgp-finger-email-filter
   (ti::mail-email-find-region beg end 'no-dupes)))

;;}}}

;;{{{ misc: messages, error; hash; whatever...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-error (message)
  "Generate error using MESSAGE and show buffer `tinypgp-:buffer-tmp-shell'.
If the error is in list `tinypgp-:pgp-binary-error-regexp-quiet' then the
shell buffer is not shown."

  (tinypgpd "tinypgp-error" message tinypgp-:cmd-macro-after-hook)

  (if (not (string-match tinypgp-:pgp-binary-error-regexp-quiet message))
      (ti::pop-to-buffer-or-window  tinypgp-:buffer-tmp-shell))

  ;;  We must close the EDIT-RMAIL etc. before calling error.

  (run-hook-with-args-until-success 'tinypgp-:cmd-macro-after-hook 'cancel)
  (tinypgp-password-expire-now 'keep-tmp-files)

  (when (eq '1pass (tinypgp-hash 'action 'get 'detail 'global))
    (setq
     message
     (concat
      message
      "[possible cause: you don't have all the keys in this keyring.]")))
  (error "[PGP executable signalled error] %s" message))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-unfinished-function ()
  "Signal error."
  (if  (not (string= (getenv "USER") "jaalto"))
      (error "\
Function you tried to call is not yet ready; it's on todo list.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-initial-message ()
  "
Release note

  1999-12-10 Development of this package has been stalled and there is no
  guarrantees that it will continue to work in new Emacs versions. The last
  update was more than year ago and since then I've been busy elswhere.
  I do appreciate bug reports, even if I can't adress any of the defects
  raised by the reports --The Maintainer.

  Emacs debug and TinyPgp debug is now ON."
  (interactive)
  (let* ((win  (selected-window)))
    (tinypgp-version)
    (ti::pmin)
    (insert (documentation 'tinypgp-initial-message) "\n\n")
    (ti::pmin)
    (select-window win)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-hash (var-sym mode &optional property value global)
  "Set or get data from obarray.
This function is used for internal data handling for current buffer.

Input:

  VAR-SYM       variable name as symbol
  MODE          'put or 'get and 'def checks if defined
  PROPERTY      property name
  VALUE         value for property
  GLOBAL        Instead of using buffer local hash, use global hash table

References:
  `tinypgp-:hash'
  `tinypgp-:hash-global'"

  ;; Make sure these two are initialized.
  (unless (vectorp tinypgp-:hash)
    (ti::vector-table-init  tinypgp-:hash))
;;;    (tinypgpd "HASH INIT" tinypgp-:hash)

;;;  (tinypgpd "HASH" var-sym mode property tinypgp-:hash)

  (or (vectorp tinypgp-:hash-global)
      (ti::vector-table-init  tinypgp-:hash-global))

  (let* ((hash (if global
                   tinypgp-:hash-global
                 tinypgp-:hash)))

    (if (symbolp var-sym)
        (setq var-sym (symbol-name var-sym))
      (error "TinyPgp: Must give a symbol '%s' " var-sym))

    (cond
     ((eq mode 'def)
      (let* ((sym (ti::vector-table-get hash var-sym)))
        (if (null property)             ;Check only if variable exist.
            (ti::vector-table-get hash var-sym)
          (when sym                     ;Check property list
            (memq property (symbol-plist sym))))))

     ((eq mode 'get)
      (if (ti::vector-table-get  hash var-sym) ;Exist ?
          (ti::vector-table-property hash var-sym property)))

     ((eq mode 'put)
      (ti::vector-table-get  hash var-sym 'allocate)
      (ti::vector-table-property hash var-sym property value 'set))
     (t
      (error "TinyPgp: No such mode '%s' ." mode)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-update-modeline ()
  "Set correct mode name."
  (let* ((fid   "tinypgp-update-modeline:")
         (str   (cond
                 ((tinypgp-backend-pgp2-p)
                  " pgp")
                 ((tinypgp-backend-gpg-p)
                  " gpg")
                 (t
                  " pgp5")))
         elt
         D)                             ;_Extra_ debug

    (if elt
        (setq elt nil))                 ;No-op, byteComp silencer

    ;;  This makes sense only if mode is on.

    (when tinypgp-mode
      (tinypgpd fid  "BEGIN" (point))

      ;; ................................................ update hooks ...
      ;;  Keep the hooks in proper order, Call function only
      ;;  periodically every 20th time. (it's too heavy operation to
      ;;  run all the time)

      (inline (tinypgp-install-menu-bar))

      (unless (setq elt (tinypgp-hash 'vital-hook 'get 'counter nil 'global))
        (setq elt 1)
        (tinypgp-hash 'vital-hook 'put 'counter 0 'global))

      (when (zerop (% (incf  elt) 20))
        (tinypgp-install-hooks-vital)
        (setq elt 1))

      (tinypgp-hash 'vital-hook 'put 'counter elt 'global)

      (if D (tinypgpd fid  "1" (point)))

      ;; ............................................... install check ...
      ;; Confirm proper installation. If we see any new packages since last
      ;; modeline update, these trigger auto installation.

      (if (and (featurep 'gnus) (null (get 'tinypgp-:hash 'gnus-check)))
          (tinypgp-install-gnus))

      (inline (tinypgp-install-mime-pgp))

      (if (and (featurep 'vm) (null (get 'tinypgp-:hash 'vm-check)))
          (tinypgp-install-vm))

      ;; .............................................. update pubring ...
      (setq elt (tinypgp-pubring-elt))

      (setq str (concat                 ;Set pubring indicator
                 str
                 (or (nth 2 elt)
                     (error "\
Internal error tinypgp-:pubring-table tinypgp-:pubring-now conflict"))))

      (if D (tinypgpd fid  "2" (point)))

      ;; ..................................................... secring ...

      (inline (tinypgp-secring-crypt-mode-detect))

      (when tinypgp-:secring-crypt-mode
        (setq str (concat str "c")))

      ;; ...................................................... remail ...

      (if tinypgp-:r-mode-indication-flag
          (setq str (concat str "r")))

      (when tinypgp-:read-email-after-hook
        (if (tinypgp-key-id-conversion-check)
            (setq str (concat str "E"))
          (setq str (concat str "e"))))

      ;;  Hmm, Should I call (tinypgp-header-sign-active-list)
      ;;  Which tells if this message will have headers?
      ;;
      ;;  Right now I just show the mode.

      (if tinypgp-:header-sign-table
          (setq str (concat str "h")))

      (if tinypgp-:xpgp-signing-mode
          (setq str (concat str "x")))

      (when (tinypgp-sign-mail-auto-mode-on-p)
        (if (inline (tinypgp-sign-mail-auto-p))
            (setq str (concat str "A"))
          (setq str (concat str "a"))))

      (if D (tinypgpd fid  "3" (point)))

;;;      (if (tinypgp-nymserver-mail-p)
;;;       (setq str (concat str "n")))

      (when (setq elt (get 'tinypgp-:r-newnym-default-account-table
                           'default-completion))
        (setq str
              (concat
               str
               (or (nth 3 (assoc elt tinypgp-:r-newnym-default-account-table))
                   "N")))

      (if D (tinypgpd fid  "3.5" (point)))

      (cond
       ((progn
          (if D (tinypgpd fid  "3.510" (point)))
          (tinypgp-auto-action-multiple-addresses-p))
        (if D (tinypgpd fid  "3.511" (point)))
        (setq str (concat str "$"))
        (unless (tinypgp-hash 'auto-action 'get 'user-mode)
          (setq str (concat str "-"))))

       ((progn
          (if D (tinypgpd fid  "3.520" (point)))
          (tinypgp-auto-action-on-p))
        (if D (tinypgpd fid  "3.521" (point)))
        (setq str (concat str "!"))
        (unless (tinypgp-hash 'auto-action 'get 'user-mode)
          (setq str (concat str "-")))))

      (if D (tinypgpd fid  "4" (point)))

)      ;;  check if we know this person: is the
      ;;  public key pubring info in cache?

      (when (and (null buffer-read-only) ;skip RMAIL
                 (inline (ti::mail-mail-p))
                 (setq elt (car-safe (ti::mail-email-from-string
                                      (mail-fetch-field  "to"))))

                 ;;  Call the conversion if it is activated,
                 ;;  save possibly one function call

                 (or (and tinypgp-:read-email-after-hook
                          (setq elt (car-safe (tinypgp-key-id-conversion elt))))
                     t)
                 (inline (tinypgp-key-find-by-cache elt "modeline")))
        ;;  Yes, key is known
        (setq str (concat str "k")))
      (setq tinypgp-:mode-name str)
      (tinypgpd fid  "END" (point))
      (ti::compat-modeline-update))

    ;;  These modes may have dynamic mode name later
    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  key mode . .

    (when  tinypgp-key-mode
      (setq tinypgp-:key-mode-name " pgpK")
      (ti::compat-modeline-update))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. summary mode . .

    (when  tinypgp-summary-mode
      (setq tinypgp-:summary-mode-name " pgp-sum")
      (ti::compat-modeline-update))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-highlight
  (regexp &optional level point face ov-type arg1 arg2 arg3)
  "Mark text forward.
If Emacs does not support highlight, this function does nothing.

Input:

  REGEXP        string to search
                 This can also be symbol with special meaning.
                 Additional arguments are passed in other variables.
                 'delet-all      remove all _tinypgp_ overlays
                 'wipe-all       delete ALL overlays and faces
                 'match          mark matched text

  LEVEL         which level in string to match, defaults to 0
  POINT         from which point forward, defaults to `point-min'
  FACE          defaults to `tinypgp-:face-mark'
  OV-TYPE       overlay type information. Defaults to 'mark
  ARG1          additional arguments to 'match
  ARG2
  ARG3"
  (when (ti::colors-supported-p)

    (let* ((fid "tinypgp-highlight: ")
           plist)

      (setq face    (or face  tinypgp-:face-mark)
            level   (or level 0)
            ov-type (or ov-type 'mark)) ;used to be overlay type

      (setq plist                       ;property list
            (list 'owner    'tinypgp
                  'type     ov-type
                  'face     face))

      (tinypgpd fid "r" regexp "l" level "point" point face ov-type
                (current-buffer))

      (save-excursion
        (cond
         ((stringp regexp)
          (goto-char (or point (point-min)))
          (ti::text-re-search regexp nil level nil plist))

         ((eq regexp 'match)
          (tinypgpd fid "level" level arg1 arg2)
          (ti::text-match-level level plist arg1 arg2))

         ((eq regexp 'delete-all)
          (ti::text-clear-region-properties
           (point) (point-max) '(owner tinypgp) ))

         ((eq regexp 'wipe-all)
          (set-text-properties (point) (point-max) nil))

         (t
          (error "TinyPgp: No such action as '%s'" regexp)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-set-session-parameters (action)
  "Set program flags according to ACTION.
In some cases the program's parameters must be in certain state
before ACTION 'sign 'encrypt 'decrypt 'verify is carried out.

Here is one reason to do so:

  When you sign create command to 'newnym' account: the X-Pgp
  signing must not be used, No headers must be signed.

This function should be inside wrapper macro that saves the previous
state of session. Use `tinypgp-save-state-macro'.

Return:
 t          if state changed
 nil        nothing done"
  (when (ti::mail-mail-p)
    (let* ((to    (or (mail-fetch-field  "to") ""))
           ret)
      (cond
       ((string-match
         "@weasel\\|@squirrel\\|efga\\|nym.alias" to) ;Newnym remailers
        (setq tinypgp-:header-sign-table nil
              tinypgp-:xpgp-signing-mode nil
              ret t)))
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-add-signature-if-signing ()
  "Insert `mail-signature-file' in mail. Do nothing in `message-mode'."
  (when (and (boundp 'mail-signature-file)
             (memq major-mode
                   '(mail-mode
                     news-reply-mode)))
    ;; message-mode , Gnus

    (let* ((file  (symbol-value 'mail-signature-file))
           (sig   (and file

                       ;; Gnus composes messages in message-mode,
                       ;; we don't touch
                       ;; that buffer because Gnus 5 can add signature when
                       ;; you compose the mail.

                       (file-exists-p file)
                       (null (ti::mail-signature-p))

                       ;;  If we're signing whole mail  buffer, then ask if
                       ;; signature should be added before signing.

                       (y-or-n-p
                        "Tinypgp: Add .signature before sign? "))))
      (when sig
        (save-excursion
          (ti::pmax)
          (insert-file-contents file)
          ;;  According to RFC there must be "-- \n" before signature.
          (ti::mail-signature-insert-break))
        nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-after-pgp-command (&optional cmd &rest args)
  "Example function: run after you have executed and some PGP command.
If buffer contains diff (after decrypting) and you have tinydiff.el
loaded, call function `tinydiff-patch' to apply that diff.

Input:
  CMD       ,'encrypt 'sign 'verify ...
  ARGS      ,ignored"
  (if (and (fboundp 'tinydiff-patch)
           ;;  We suppose that we're in incoming RMAIL or VM buffer

           (memq major-mode '(rmail-mode vm-mode))
           (memq cmd '(verify decrypt))
           (ti::buffer-diff-type-p))    ;Is there diff
      (call-interactively 'tinydiff-patch))
  nil)

;;}}}
;;{{{ misc: file control; abbrevs

;;; ........................................................ &pgp-misc ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-show-last-finger-error ()
  "Show last finger error message in echo area."
  (interactive)
  (if (stringp tinypgp-:last-network-error)
      (message tinypgp-:last-network-error)
    (message "No Finger error information.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-view-register (&optional noerr)
  "View content of register.
Do not signal error if the register `tinypgp-:register' is empty and
NOERR is non nil. NOERR is automatically t if function is called
interactively."
  (interactive)
  (let* ((reg   tinypgp-:register)
         (msg   (format "TinyPgp: register '%c' doesn't contain data yet."
                        tinypgp-:register))
         win)
    (if (not (stringp (get-register reg)))
        (unless noerr
          (if (interactive-p)
              (message msg)
            (error msg)))
      (setq win (get-buffer-window tinypgp-:buffer-view t))

      (if (null win)
          (pop-to-buffer (ti::temp-buffer tinypgp-:buffer-view 'clear))
        (raise-frame (window-frame win))
        (select-window win)
        (erase-buffer))

      (insert-register tinypgp-:register)
      (ti::pmin)
      (when (interactive-p)
        (message "Content of register '%c'" tinypgp-:register)
        (sleep-for 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-file-control (mode &optional arg)
  "Do file operation according to MODE and ARG.

Input:
  MODE  'all-kill
        'password-write
        'password-kill
        'password-kill
        'source-kill
        'source-write
        'users-write
  ARG"
  (let* (buffer)
    (tinypgpd "file-control in:" mode arg)

    (cond
     ((eq 'all-kill mode)
      (dolist (file
               (list
                tinypgp-:file-source
                tinypgp-:file-output
                tinypgp-:file-password
                tinypgp-:file-user-list))
        (if (file-exists-p file)
            (delete-file file))))

     ((eq 'password-write mode)
      (ti::file-delete-safe tinypgp-:file-password)

      (with-current-buffer (tinypgp-ti::temp-buffer)
        (buffer-disable-undo (current-buffer))

        (insert
         (or arg
             (ti::vector-table-property
              tinypgp-:hash-password tinypgp-:user-now 'password)))

        (set-buffer-modified-p nil)
        (write-region (point-min) (point-max) tinypgp-:file-password)

        ;;  Don't leave password traces in the buffer

        (if (fboundp 'passwd-erase-buffer)
            (ti::funcall 'passwd-erase-buffer) ;passwd.el
          (let ((s (* (buffer-size) 3))) ;Code copied from passwd.el
            (erase-buffer)
            (while (> s 0)
              (insert ?\000)
              (setq s (1- s)))
            (erase-buffer)))

        (ti::file-mode-protect tinypgp-:file-password)))

     ((eq 'password-kill mode)
      (if (file-exists-p tinypgp-:file-password)
          (delete-file tinypgp-:file-password)))

     ((eq 'source-kill mode)
      (if (file-exists-p tinypgp-:file-source)
          (delete-file tinypgp-:file-source)))

     ((eq 'source-write mode)
      ;;  When wring the file out, it must be exactly
      ;;  as it appears in buffer

      (let* ((require-final-newline nil))
        (ti::file-delete-safe
         (list tinypgp-:file-source
               (concat tinypgp-:file-source ".asc")))

        ;;  I don't think this is good for Multibyte Chars

;;;     (if (fboundp 'as-binary-process)
;;;         (as-binary-process
;;;          (write-region (point) (point-max) tinypgp-:file-source))

        (write-region (point) (point-max) tinypgp-:file-source)

        (ti::file-mode-protect tinypgp-:file-source)))

     ((eq 'users-write mode)
      (ti::file-delete-safe tinypgp-:file-user-list)
      (setq buffer (tinypgp-ti::temp-buffer))
      (unless arg
        (error "No USER LIST"))

      (with-current-buffer buffer

        (dolist (elt (ti::list-make arg))
          (unless (stringp elt)
            (error "Users corrupt. Check tinypgp-:encrypt-with-function"))
          (insert (ti::string-remove-whitespace elt) "\n"))

        (ti::file-delete-safe tinypgp-:file-user-list)
        (write-region (point-min) (point-max) tinypgp-:file-user-list)
        (ti::file-mode-protect tinypgp-:file-user-list)))

     (t
      (error "Unknown mode")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mail-abbrevs-filter (email-list)
  "Filter invalid entries out form EMAIL-LIST.
Every entry must have .xx or .xxx extension, which refers to country
name or organisation form."
  (let* (ret)
    (dolist (elt email-list)
      (if (string-match "\\....?$" (car (ti::mail-email-from-string elt)))
          (push elt ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-update-mail-abbrevs ()
  "Update mail abbrevs.
You need to do this is you have modified ~/.mailrc.
Call mail abbrev.el first to read the file."
  (interactive)
  (let* ((sym 'timi-:mail-aliases-alist)
         list)
    (tinypgpd "update-mail-abbrevs 1:")

    ;;   since the tinymail.el and tinypgp.el use the same
    ;;   abbrevs list, it isn't worth to build 2 separate lists,
    ;;   because creating alist is slow!
    ;;
    ;;   Now we share the same list and the abbrevs are built by
    ;;   tinymail, which we copy here.

    (if (and (featurep 'tinymail)
             (boundp 'timi-:mail-aliases-alist))
        (setq  tinypgp-:pgp-email-abbrev-list (symbol-value sym))
      (setq  tinypgp-:pgp-email-abbrev-list (ti::mail-abbrev-get-alist)))

    (tinypgpd "update-mail-abbrevs 2:")
    (setq  tinypgp-:pgp-email-list
           (ti::mail-mail-abbrevs-email-list tinypgp-:pgp-email-abbrev-list))

    (tinypgpd "update-mail-abbrevs 3:")

    ;;  maybe not all are valid in the obarray...

    (setq list (funcall tinypgp-:filter-email-function
                        tinypgp-:pgp-email-list))

    (tinypgpd "update-mail-abbrevs 4:")
    (setq tinypgp-:pgp-email-list-completions
          (ti::list-to-assoc-menu list))

    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-update-mail-abbrevs-hook ()
  "Reparse the ~/.mailrc file when it is saved.
This function is installed into `write-file-hooks'."
  (when (string-match "\\.mailrc" (or buffer-file-name "#noName"))
    (message "Updating mail abbrevs for TinyPgp...")
    (tinypgpd "update-mail-abbrevs-hook in:")
    (tinypgp-update-mail-abbrevs)
    (message "Updating mail abbrevs for TinyPgp...done")
    nil))                               ;Hook return value

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-invisible-region (beg end &optional show)
  "Make BEG END invisible. Optionally SHOW it."
  ;;  We also say that these properties belong to "tinypgp"
  (let* (buffer-read-only)              ;allow writing
    (with-buffer-modified
      (if (null show)
          (set-text-properties beg end '(invisible t owner tinypgp))
        (set-text-properties beg end '(invisible nil owner tinypgp))))))

;;}}}
;;{{{ misc: test-p, or or primitives

;;; ........................................................... &tests ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pgp-encrypted-p-default ()
  (let (stat)
    ;; this function returns symbol, convert it to string
    (save-excursion
      (ti::pmin)
      (if (setq stat (ti::mail-pgp-data-type))
          (symbol-name stat)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-password-time-valid-p ()
  "Return non-nil, if it's not yet time to forget password.
The returned value is number of seconds left."
  (let* (secs-was
         secs-now
         diff
         val)
    (cond
     ((not (integerp tinypgp-:password-keep-time))
      nil)
     (t
      (if (null (tinypgp-hash 'password-time 'get 'tick nil 'global))
          (tinypgp-hash 'password-time 'put 'tick (current-time) 'global))

      (setq val         (tinypgp-hash 'password-time 'get 'tick nil 'global)
            secs-was    (nth 1  val)
            secs-now    (nth 1  (current-time))
            diff        (- secs-now secs-was)
            diff        (- tinypgp-:password-keep-time diff))

;;;      (ti::d! diff)

      (if (> diff 0)
          ;; How much is left, counts down...
          diff)))))

;;}}}
;;{{{ misc: email and substitutions

;;; ........................................................... &email ...

(defvar tinypgp-:email-substitution-table nil
  "Where this variable is used:

    Change email addresses if needed to get right public key.

    Say, the PGP key-id shows <foo@site.com> as email, but the person also
    has mailing address <foo@x-site.com>. If we receive mail from
    foo@x-site.com, PGP wouldn't find it from the database if we used
    that. Instead we must immediately tell 'hey, this person is known as
    <foo@site.com>' which is listed in his key-id field.

How this variable is used:

    List of email substitution. When REGEXP is matches then SUBST is used.
    SUBST is should match unique key entry in your keyrings. Best if
    SUBST is 0xFFFF key id, but many times it more descriptive to use
    alternative email address.

Where this variable is used

    In function `tinypgp-email-substitution-default' which is installed
    to `tinypgp-:read-email-after-hook'

Example:

    WE CHANGE THIS VARIABLE WITH FUNCTION `tinypgp-email-substitution-add'

    ;;  List of email addresses that are not in the person's pgp-key id
    ;;  Use the right Hand key when left hand matches.

    (defconst my-:tinypgp-email-substitution-table
      (list
       (cons \"xxx@.*lycaeum\"   \"yyy@lycaeum.org\")
       (cons \"xxx.*jena.de\"    \"zzz.foo@Jena.Thur.De\")

       ;; This one has multiple keys and we want to use one particular.
       ;; The 0xFFFF is unique way to tell which key to use

       (cons \"valkyr\"     \"0xA73B5E6D\"))
      \"*My email substitutions that will be added to
    `tinypgp-:email-substitution-table'\")

    ;;  Now add my substitutions

    (tinypgp-email-substitution-add my-:tinypgp-email-substitution-table)

Format:
  '((REGEXP  SUBST) (R S) ..)")

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-make-choices (email)
  "Make new choices from EMAIL.
If you try to encrypt with EMAIL and it fails; it may be
the case that the email address is not added to user's PGP key-id field.

This function examines EMAIL and constructs some suitable
choices that may match better when doing new lookup.

Return:
  nil
  (\"string\"
   ..)"
  (let* (list
         str
         s1
         s2)

    ;;  firstname.surname@site.com --> "Firstname Surname"

    (when (string-match "^\\(.*\\)\\.\\(.*\\)@" email)
      (setq s1 (capitalize (match-string 1 email))
            s2 (capitalize (match-string 2 email)))

      ;;  Because the firsh name may be shortened
      ;;  "Rich" is actually "Richard", we want to add the surname
      ;;  by it self to the list too

      (push  (concat s1 " " s2) list)
      (push s2 list))

    ;;   many times the 'server' is local and is not
    ;;   included in the key id
    ;;
    ;;   @server.domain.here.com -->  "domain.here.com"

    (if (setq str (ti::string-match "@[^.]+\\.\\(.*\\..*\\)" 1 email))
        (push str list))

    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-discard-default (list)
  "Toss away addresses from LIST that are not finger sites.

In-Reply-To: <199611101605.LAA18736@site.com> from Foo Bar at..
X-Face: >>@YIrj6h"
  (let (ret)
    (tinypgpd "tinypgp-email-discard-default in: " list )

    (when list
      (dolist (elt (ti::list-make list))
        (when (and (not (string-match
                         (concat
                          "\\(19[89][0-9]\\|200[0-9]\\)[0-9][0-9]"
                          "\\|^foo\\|^ba[zr]@\\|@site.com"
                          "\\|[^-_0-9a-zA-Z+]@")
                         elt))
                   ;;  leave only real email addresses
                   (string-match "@" elt))
          (push elt ret))))
    (tinypgpd "tinypgp-email-discard-default out: " ret )
    (if ret
        (nreverse ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-substitution-add-1 (cons-cell &optional remove)
  "Add new CONS-CELL (RE . SUBST) to `tinypgp-:email-substitution-table'.
IF REMOVE is non-nil, search for SUBST and delete the entry
from the table.

Return:
  killed entry
  added entry
  nil           ;already exist(add) or not exist(remove)"
  (let* (elt
         ret)
    (setq elt (rassoc (cdr cons-cell) tinypgp-:email-substitution-table))
    (cond
     (remove
      (when elt
        (setq tinypgp-:email-substitution-table
              (delete elt  tinypgp-:email-substitution-table))
        (setq ret elt)))

     ((null elt)                        ;Add new element if not there.
      (setq ret cons-cell)
      (push cons-cell tinypgp-:email-substitution-table)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-substitution-add (cons-list &optional remove)
  "Add CONS-LIST or REMOVE it from list of email substitutions.
The CONS-LIST must be in format:

'((RE . SUBST) (R . S) ..)"

  (mapcar
   (function
    (lambda (x)
      (tinypgp-email-substitution-add-1 x remove)))
   cons-list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-substitution-default (list)
  "Check LIST of email addresses and subtitute them with suitable pgp-ids.
`tinypgp-:email-substitution-table' takes precedence over BBDB record `pgp-id'.

References:
  `tinypgp-:email-substitution-table'
  `tinypgp-:read-email-after-hook'."
  (let* (re
         subst
         bbdb-pgp-id
         ret)
    (dolist (email (ti::list-make list))

      (dolist (elt tinypgp-:email-substitution-table)
        (setq re (car elt)    subst (cdr elt))
        (cond
         ((string-match re email)
          (setq email subst) ;;  substitute and stop loop
          (return))
         ((setq bbdb-pgp-id (tinypgp-bbdb-id email))
          (setq email bbdb-pgp-id)
          (return))))

      (push email ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-email-substitution-toggle (&optional mode)
  "Toggle email substitution.
It is possible that you have coded an email substitution function
and installed it into `tinypgp-:read-email-after-hook'.

If there is such a function; it probably converts some email addresses
to some relevant PGP key ids. However sometimes you may want to turn
off this feature completely to be sure that when reading the
email address eg from TO: field, it will also be used when calling
for encryption.

This functions toggles email substitution functions on/off by
clearing/restoring the `tinypgp-:read-email-after-hook'

MODE can be
 nil        toggle
 0 -1       off
 other      on"
  (interactive)
  (let* ((sym 'tinypgp-:read-email-after-hook))

    ;; Not recorded; record original value

    (if (null (get sym 'original))
        (put sym 'original (symbol-value sym)))

    (cond
     ((or (memq mode '(0 -1))
          (symbol-value sym))
      (set sym nil)
      (message "Email substitution off."))
     (t
      (set sym (get sym 'original))
      (message "Email substitution restored to original.")))

    (tinypgp-update-modeline)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-id-conversion (single-or-list)
  "Modify SINGLE-OR-LIST and return possibly modified list.
Function is used to convert any email address in the list to a suitable pgp
key-id that can be used in place of the 'email' string.

This function stores the list to hash table and reads the
conversion from there if it exist in symbol 'key-id property
'conversion.

References:

  `tinypgp-:read-email-after-hook'"
  (let* ((fid     "tinypgp-key-id-conversion: ")
         ;;  Make hash access key property
         (prop    (when single-or-list
                    (make-symbol
                     (mapconcat
                      'concat
                      (ti::list-make single-or-list)
                      ""))))
         val)

    ;;  Because you use the key-id conversion in the program all the
    ;;  time (called multiple times) and the conversion will
    ;;  always be same, we save the converted list into hash table
    ;;  for later use.
    ;;
    ;;  1. the hash-key is all list strings concatenated
    ;;     together "me@foo.siteyou@bar.site"
    ;;
    ;;  2. If that hash entry is not found, then we call conversion
    ;;     function and store the result to hash
    ;;
    ;;  3. Next time the conversion is already available for us
    ;;     from quick cache.
    ;;
    ;;  This should result faster response, becuse calling hook
    ;;  functions is real slow.

    (tinypgpd fid 'KEY prop 'LIST single-or-list)

    (when single-or-list
      (cond
       ((tinypgp-hash 'key-id-conversion 'def prop)
        (when (setq val (tinypgp-hash 'key-id-conversion 'get prop))
          (setq single-or-list val))
        (tinypgpd fid 'HASH single-or-list))
       (t
        (tinypgpd fid 'HOOK tinypgp-:read-email-after-hook)
        (dolist (func (ti::list-make tinypgp-:read-email-after-hook))
          (setq single-or-list  (funcall func single-or-list)))

        (tinypgp-hash 'key-id-conversion 'put prop single-or-list)
        (tinypgpd fid 'OUT single-or-list))))

    (when single-or-list
      (ti::list-make single-or-list))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-id-conversion-check ()
  "Return non-nil if the the conversion happens on TO field.
To field must contain only one address."
  (let* (elt)
    (when (ti::mail-mail-p)
      ;;   Will conversion happen?
      ;;   - To field must have something
      ;;   - there must be only one email
      ;;   - the conversion has changed email.
      (cond
       ((and (not (ti::nil-p (setq elt (mail-fetch-field "To"))))
             (not (string-match "," elt))
             (not (string= elt (or (car-safe (tinypgp-key-id-conversion elt))
                                   ""))))
        (or (car-safe (tinypgp-key-id-conversion elt))
            ""))
       (t
        nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-id-conversion-check-verbose ()
  "Check if email address conversion is about to happen in To field."
  (interactive)
  (let* (stat)
    (cond
     ((null (ti::mail-mail-p))
      (message "Email conversion: not a mail buffer, can't read To field."))
     (t
      (setq stat (tinypgp-key-id-conversion-check))
      (cond
       ((null tinypgp-:read-email-after-hook)
        (message "You have turned off Email conversion mode. %s"
                 (if stat (format "[cnv: %s" stat))))
       (t
        (if stat
            (message "Conversion to: %s" stat)
          (message "No Email conversion trigges"))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-id-find ()
  "Try to find 'Id' 0x12345678 from current buffer. X-Pgp is searched first."
  (let* ((list (tinypgp-xpgp-get-info))
         elt
         ret)
    (cond
     ((and list                         ; Id=0xF72ED579;
           (setq elt (assoc "id" list)))
      (setq ret (nth 1 elt)))
     (t                                 ;No other methods yet.
      nil))
    ret))

;;}}}

;;{{{ buffer: generate, show

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ti::temp-buffer (&optional choice arg1 arg2 arg3)
  "Create tmp buffer for TinyPgp.el. CHOICE ARG1 ARG2 ARG3 are internal."
  (let ((fid "tinypgp-ti::temp-buffer:")
        mail-setup-hook                 ;No hooks now (slow) !
        mail-mode-hook
        message-mode-hook
        buffer)
    (tinypgpd fid choice arg1 arg2 arg3)

    ;;  ByteComp silencer, this is no-op
    (if mail-setup-hook (setq mail-setup-hook nil))
    (if mail-mode-hook  (setq mail-setup-hook nil))

    (setq
     buffer
     (cond
      ((eq choice 'shell)
       (ti::temp-buffer tinypgp-:buffer-tmp-shell 'clear))

      ((eq choice 'copy)
       (ti::temp-buffer tinypgp-:buffer-tmp-copy  'clear))

      ((eq choice 'article)
       (ti::temp-buffer tinypgp-:buffer-tmp-article  'clear))

      ((eq choice 'finger)
       (ti::temp-buffer tinypgp-:buffer-tmp-finger 'clear))

      ((eq choice 'http)
       (ti::temp-buffer tinypgp-:buffer-tmp-http 'clear))

      ((eq choice 'kring)
       (ti::temp-buffer tinypgp-:buffer-tmp-kring 'clear))

      ((eq choice 'show)
       (ti::temp-buffer tinypgp-:buffer-tmp-show 'clear))

      ((eq choice 'mail)
       (ti::kill-buffer-safe tinypgp-:buffer-tmp-mail)
       (setq buffer (ti::temp-buffer tinypgp-:buffer-tmp-mail 'clear))
       (with-current-buffer buffer
         (setq tinypgp-:hash nil)       ;Clear hash array
         (mail-mode)
         ;;   to subject in-reply-to cc replybuffer actions
         ;;
         (mail-setup arg1 arg2 nil arg3 nil nil))
       (tinypgpd fid "MAIL OUT")
       buffer)

      ((null choice)
       (ti::temp-buffer tinypgp-:buffer-tmp 'clear))
      (t
       (error "TinyPgp: No such mode '%s'" choice))))

    (with-current-buffer buffer
      (defconst font-lock-mode nil)
      (defconst lazy-lock-mode nil)
      ;;  one time scratch buffer
      (buffer-disable-undo (current-buffer)))

    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-show-buffer-general (type)
  "Pop to buffer TYPE."
  (let ((buffer
         (cond
          ((eq type 'comint)  (tinypgp-comint-buffer))
          ((eq type 'debug)   tinypgp-:debug-buffer)
          ((eq type 'finger)  tinypgp-:buffer-tmp-finger)
          ((eq type 'http)    tinypgp-:buffer-tmp-http)
          ((eq type 'shell)   tinypgp-:buffer-tmp-shell)
          ((eq type 'tmp)     tinypgp-:buffer-tmp))))
    (cond
     ((null buffer)
      (error "TinyPgp: Wrong type '%s' " type))
     ((get-buffer buffer)
      (pop-to-buffer buffer))
     (t
      (message "Buffer does not exist: '%s'" buffer)))))

(defun tinypgp-show-buffer-comint ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'comint))

(defun tinypgp-show-buffer-debug ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'debug))

(defun tinypgp-show-buffer-finger ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'finger))

(defun tinypgp-show-buffer-http ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'http))

(defun tinypgp-show-buffer-shell ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'shell))

(defun tinypgp-show-buffer-tmp ()
  "Show buffer."
  (interactive) (tinypgp-show-buffer-general 'tmp))

;;}}}
;;{{{ pubring: misc

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-elt ()
  "Return active pubring ELT."
  (let* ((ring  (tinypgp-expand-file-name tinypgp-:pubring-now))
         kring
         ret)
    (dolist (elt (tinypgp-pubring-table))
      (setq kring (nth 1 elt))
      (cond
       ((stringp kring)
        ;; Second element must be filename string
        (when (string= ring (tinypgp-expand-file-name kring))
          (setq ret elt)
          (return)))
       (t
        (error "Invalid format: tinypgp-:pubring-table, please check."))))

    (unless ret
      (error "tinypgp-:pubring-table, can't find tinypgp-:pubring-now?"))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-ask (&optional msg)
  "Ask pubring with MSG and offer 'alias' completion.

Return:
  nil
  pubring file"
  (let (ret)
    (setq ret
          (tinypgp-pubring-complete
           (if msg
               msg
             (format
              "Ok to use pubring '%s' [ret=yes]? "
              (or (tinypgp-pubring-file2alias tinypgp-:pubring-now)
                  "<unknown>")))))

    (if (not (ti::nil-p ret))
        (setq ret (tinypgp-pubring-alias2file ret))
      (setq ret nil))

    (tinypgpd "tinypgp-pubring-ask out: " ret )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-in-use-confirm ()
  "Change pubring if it is not the first entry in `tinypgp-pubring-table'.
Ask confirmation for the change. The calling
function should bound variable `tinypgp-pubring-table' locally,
because it may be changed here.

References:
  `tinypgp-save-state-macro'"
  (let ((first  (tinypgp-expand-file-name
                 (nth 1 (car (tinypgp-pubring-table)))))
        (now    (tinypgp-expand-file-name tinypgp-:pubring-now)))
    (when (not (string= first now))
      (setq now (tinypgp-pubring-ask))
      (when now
        (setq tinypgp-:pubring-now now)
        (tinypgpd "tinypgp-pubring-in-use-confirm out: "
                  tinypgp-:pubring-now)))))

;;}}}
;;{{{ pubring: interactive

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-display ()
  "Show current pubring in use."
  (interactive)
  (message "Current pubring: %s" tinypgp-:pubring-now)
  (sit-for 1)) ;; If drawn from menu, the mouse move wipes it away..

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pubring-set-current (alias)
  "Set active pubring using ALIAS and update mode line."
  (interactive (list (tinypgp-pubring-complete "Set active pubring to: ")))
  (when alias
    (setq tinypgp-:pubring-now
          (tinypgp-expand-file-name (nth 1 (assoc alias
                                                  (tinypgp-pubring-table)))))

    (if (not (file-exists-p tinypgp-:pubring-now))
        (error "No pubring file %s" tinypgp-:pubring-now))

    (tinypgpd "tinypgp-pubring-set-current out: " alias tinypgp-:pubring-now)

    (tinypgp-update-modeline)
    (if (interactive-p)
        (tinypgp-pubring-display))))

;;}}}
;;{{{ user: general, interactive

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-user-in-use-confirm (&optional msg)
  "Change user if Primary user is not active ask confirmation with MSG."
  (let (ans)
    (setq msg
          (or
           msg
           "Not primary, change user id to [empty = no change]: "))
    (if (and (not (string-match
                   (regexp-quote tinypgp-:user-primary) tinypgp-:user-now))
             (not
              (ti::nil-p
               (setq ans (read-from-minibuffer msg tinypgp-:user-now)))))
        (setq tinypgp-:user-now ans))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-user-display ()
  "Show active user."
  (interactive)
  (ti::read-char-safe-until (concat "Current user: " tinypgp-:user-now)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-user-set-current (user)
  "Set active USER."
  (interactive
   (list
    (completing-read
     (format "[%s] Set pgp user to: " tinypgp-:user-now)
     tinypgp-:pgp-email-list-completions
     nil nil nil
     'tinypgp-:history-email)))

  (if (ti::nil-p user)
      (error "Invalid input."))

  (setq tinypgp-:user-now user)
  (if (interactive-p)
      (tinypgp-user-display)))

;;}}}
;;{{{ key: handling

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-cache (mode &optional data1 data2 data3)
  "Function to control caching of key-id and.
The cache hook tells if the entry should be cached when MODE is 'put

When inserting new keys into cache, every 3rd key triggers saving
the cache to disk.

References:

  `tinypgp-:key-cache'
  `tinypgp-:key-cache-last'

Input MODE:

  'get  look for data1 from cache and return cache entry or nil
  'put  cache entries data1, data2, data3
  'del  remove named entry from cache. Do nothing if no such entry.

Data arguments:

  DATA1 DATA2 DATA3

Return:

  nil
  cache entry"
  (let ((last           tinypgp-:key-cache-last)
        (data1-orig     data1)          ;Email may be changed
        (debug          nil)            ;developer's manual debug flag
        (fid            "tinypgp-key-cache: ")
        ret)

    ;;  The cache is used only if user has multiple pubrings
    (when (tinypgp-pubring-many-p)

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. fast case . .
      ;;  Remember that modeline calls us many times

      (cond                             ;Is the entry in QUICK cache?
       ((and last                     ;bypass everything if we find it
             (eq mode 'get)       ;many time we call 'get successively
             (string= (car last) data1))
        (setq ret (nth 1 (nth 1 last)))

        (if debug
            (tinypgpd fid "fast get" data1 ret )))

       (t
        ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . normal case  ..

;;;    (ti::d! "C in: " mode data1 data2)

        (setq data1 (ti::remove-properties data1))

        (if data2
            (setq data2 (ti::remove-properties data2)))

        (if debug
            (tinypgpd fid "in:" 'MODE  mode 'DATA1 data1 'DATA2 data2))

;;;    (ti::d! "Cache name>>" data1 data2)

        ;;  Note: I used elp.el to check if the obarray method
        ;;  would be faster, but it seems that at least for single entry
        ;;  the list implementation is faster? I was suprised..

        (if (not (listp tinypgp-:key-cache)) ;make sure this is a list
            (setq tinypgp-:key-cache nil))

        (cond

         ((eq mode 'get)
          (when (setq ret (assoc data1 tinypgp-:key-cache))
            (setq tinypgp-:key-cache-last  (list data1-orig ret))
            (setq ret (nth 1 ret))))

         ((eq mode 'del)
          (if (setq ret (assoc data1 tinypgp-:key-cache))
              (adelete 'tinypgp-:key-cache (car ret))))

         (t
          (if (and (null (assoc data1 tinypgp-:key-cache)) ;;  Already there ?
                   (< (length  tinypgp-:key-cache) 300))   ;Hard limit
              (push (list data1 data2 data3) tinypgp-:key-cache ))

          ;;  Save every 3rd new entry.

          (if (eq (% (length tinypgp-:key-cache) 3) 0)
              (tinypgp-key-cache-save)))))))

    (if debug
        (tinypgpd fid "out: RET" ret))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-cache-save (&optional load)
  "Save or LOAD the key cache file.
If the underlying cache file has recent copy; the SAVE is not
performed, but the newer copy reloaded and evaluated.

Signal no erro if LOAD cannot find cache file."
  (let* ((fid   "tinypgp-key-cache-save:")
         (file  (or (tinypgp-backend-file tinypgp-:file-key-cache)
                    (error "TinyPgp: Internal cache error")))
         (list  tinypgp-:key-cache)
         (len   (length list))
         (olen  (tinypgp-hash 'cache 'get 'len nil 'global))
         buffer
         done)
    (tinypgpd fid "in: FILE" file 'LEN len 'OLEN olen 'LOAD-FLAG load)

    (cond
     (load
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (if (fboundp 'eval-buffer)    ;XE 19.14
              (ti::funcall 'eval-buffer)
            (ti::funcall 'eval-current-buffer))
          (setq done t))))

     ;; ......................................................... save ...
     (t
      ;;   There may be several emacsen running, and they may have saved the
      ;;   cache too. Reload the file if it is newer that the buffer
      ;;   in this emacs (it has been saved by some other emacs)

      (when (and (buffer-live-p (setq buffer (find-buffer-visiting file)))
                 (with-current-buffer buffer (ti::file-changed-on-disk-p)))
        (with-current-buffer buffer
          (revert-buffer t t)           ;No confirmations
          (if (fboundp 'eval-buffer)    ;XE 19.14
              (ti::funcall 'eval-buffer)
            (ti::funcall 'eval-current-buffer)
            (setq done t))))

      (when (and (null done)
                 ;; Something to save? Has Length changed
                 (or (not (eq len olen))
                     ;;  Not yet saved?
                     (not (file-exists-p file))))
        (tinypgp-hash 'cache 'put 'len len 'global)
        (with-current-buffer (find-file-noselect file)
          (erase-buffer)
          (insert ";;\n;;\tEmacs TinyPgp.el: key cache file\n;;\n\n")
          (insert "(defconst tinypgp-:key-cache\n  '(\n")
          (dolist (elt list) (insert "    " (prin1-to-string elt) "\n"))
          (insert "    ))\n\n;; End of file\n")
          (save-buffer)))))
    done))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-cache-display (&optional verb)
  "Print contents of cache. VERB."
  (interactive)
  (tinypgp-key-cache-save) ;;  Save latest first
  (let* ((fid    "tinypgp-key-cache-display:")
         (file   (tinypgp-backend-file tinypgp-:file-key-cache))
         (buffer (or (find-buffer-visiting file)
                     (and (file-exists-p file)
                          (find-file-noselect file)))))
    (tinypgpd fid file buffer)
    (if (null buffer)
        (error "Can't display %s" file)
      (display-buffer buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-cache-remove-entry-last ()
  "Clear last fast cache entry."
  (interactive)
  (setq tinypgp-:key-cache-last nil)
  (if (interactive-p)
      (message "Cleared last cache entry.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-cache-remove-entry (string &optional raw-entry)
  "Read email addresses from string and remove it from cache.

Input:
  STRING     String ==> email address is picked from it
  RAW-ENTRY  if nono-nil, then bypass call to
             `tinypgp-:read-email-after-hook' which may change the string"
  (let* ((fid "tinypgp-key-cache-remove-entry: ")
         list)
    (tinypgpd fid "in:" raw-entry string )

    (when (tinypgp-pubring-many-p)

      (cond
       (raw-entry
        (tinypgp-key-cache 'del string))

       (t
        (or (setq list (ti::mail-email-from-string string))
            (setq list (list string)))
        (dolist (elt list)
          (when elt ;; Should we change the keyId that is read from field?
            (setq elt (car (tinypgp-key-id-conversion elt))))
          (tinypgp-key-cache 'del elt))))
      ;; Clear fast cache
      (tinypgp-key-cache-remove-entry-last))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-generate
  (key-bit-choice user-id pass-phrase &optional verb)
  "Generate new key. Only default key sizes are supported.

Input:
  KEY-BIT-CHOICE        1,2 or 3
  USER-ID
  PASS-PHRASE
  VERB"
  (interactive
   (let* ((key-list
           '(("512"   1)
             ("768"   2)
             ("1024"  3)))
          key
          user
          pass
          ans)

     (setq key (completing-read "Key size: " key-list nil 'match-it "768"))
     (if (null (setq key (tinypgp-alias2name key key-list)))
         (error "No key choice found."))

     (setq user (read-from-minibuffer "User id for your public key: "))
     (if (ti::nil-p user)
         (error "Empty user id."))

     (setq pass (ti::compat-read-password "Pass phrase: "))
     (if (ti::nil-p pass)
         (error "Empty pass phrase"))

     (list key user pass)))

  ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...  body . .

  (if (not (and (memq key-bit-choice '(1 2 3))
                (stringp user-id)
                (stringp pass-phrase)))
      (error "Arg error."))

  (let* ( ;; (BCMD  (tinypgp-binary-get-cmd 'key-generate))
         ;; (cmd   (tinypgp-cmd-compose BCMD user-id pass-phrase))
         ret)
    (ti::verb)
    (tinypgp-unfinished-function)

    (tinypgp-save-state-macro
     (if verb  (tinypgp-pubring-in-use-confirm)))

    (if verb
        (message "Generating new user-id...done."))

    ret))

;;}}}

;;{{{ misc: auto-action

;;; ..................................................... &auto-action ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-on-modeline-p ()
  "Check if mode line string say that auto action in 'on'."
  (and (stringp tinypgp-:mode-name)
       (string-match "!$\\|!k" tinypgp-:mode-name)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-on-p ()
  "Check is user has allowed action and if action exists."
  ;;  There is no auto action for read only buffer like RMAIL
  (tinypgpd "tinypgp-auto-action-on-p: "
            tinypgp-mode
            (tinypgp-hash 'auto-action 'def 'user-mode)
            (tinypgp-hash 'auto-action 'get 'user-mode))

  (when (and (null buffer-read-only)
             tinypgp-mode)
    (unless (tinypgp-hash 'auto-action 'def 'user-mode)
      ;;  Not defined, initialize
      (tinypgpd "tinypgp-auto-action-on-p: SET DEFAULT")
      (tinypgp-hash 'auto-action 'put 'user-mode t))
    (tinypgp-auto-action-p 'read-hash)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-defeat-p ()
  "Check if auto action should be cancelled."
  ;;  Forget mime multiparts/PGP signed.
  (ti::mail-mime-maybe-p))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-verbose ()
  "Show auto-action entry to user.
If auto action is found it is also available from `tinypgp-:register'."
  (interactive)
  (let* (elt)
    (tinypgpd "tinypgp-auto-action-verbose in:")

    (cond
     ((ti::mail-mime-maybe-p)
      (message "TinyPgp;  Looks like MIME message, no auto action allowed"))

     ((tinypgp-auto-action-multiple-addresses-p 'force)
      (message "TinyPgp; encryption to multiple recipients pending."))

     ((setq elt (tinypgp-auto-action-p))
      (message "TinyPgp; Auto-action triggers: %s" (prin1-to-string elt))
      (set-register tinypgp-:register (prin1-to-string elt)))

     (t
      (message "TinyPgp; There is no auto action that would activate.")))
    (tinypgp-update-modeline)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-update-modeline ()
  "Do auto action check and update mode line."
  (tinypgp-auto-action-p)
  (tinypgp-update-modeline))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-p (&optional read-hash)
  "Check if auto-action entry is defined for current (email) buffer.
If buffer is read only, this does nothing.
Any MIME message in buffer suppresses auto-action.

Note:

  Multiple recipients are not checked, Only To address.
  See `tinypgp-auto-action-multiple-addresses-p' for that.

References:

  `tinypgp-:auto-action-table'

Input:

  READ-HASH     non-nil instructs to read the value
                from storage, if the TO: address hasn't changed.
                This is faster than evaluating the list every time.

Return:

  elt           entry from action table"
  (let ( ;; (EVAL-OR-STRING  SIGN-FLAG [ENCRYPT-FLAG] [KEYRING])
        (fid    "tinypgp-auto-action-p: ")
        (tbl    tinypgp-:auto-action-table)

        ;;   We don't enable this because timer calls us
        ;;   Only when we debug the function

        (debug  t)

        ;;  These tags must be broken in this file so that TM won't get upset
        ;;  seeing them
        ;;
        ;; -- } - <<signed>>
        ;; -- } - <<encrypted>>

        (mime-p  (ti::re-search-check "--[}]-<<"))

        user-mode
        to-field
        val
        ret)

    ;;  This function is called from a timer process to update the
    ;;  modeline, that's why we can't afford to rescan the auto-action
    ;;  list all the time: it takes too much time.
    ;;
    ;;  Instead, we store the found ACTION to hash table and read the
    ;;  hash entry. The drawback is that if user goes and changes
    ;;  the auto action table, we can't tell about it in the modeline.
    ;;
    ;;  Used local hash properties on variable 'auto-action
    ;;  'user-mode      bool    t = ok, nil = defeated by user
    ;;  'to-field       string  to field contents
    ;;  'elt            lisp    stored auto action.

    ;;  If not yet defined, set the auto action to 't'
    ;;  User may defeat the action manually.

    (if (tinypgp-hash 'auto-action 'def 'user-mode)
        (setq user-mode (tinypgp-hash 'auto-action 'get 'user-mode))
      (tinypgp-hash 'auto-action 'put 'user-mode t)
      (setq user-mode t))

    ;;  Should always be a string otherwise lot of code breaks.

    (unless (stringp tinypgp-:user-now)
      (message "\
Tinypgp: Warning, tinypgp-:user-now is not a string. Fixing...")
      (sit-for 1)
      (setq tinypgp-:user-now (user-login-name)))

    ;;  TO FIELD: see what we have in the hash table

    (setq val (tinypgp-hash 'auto-action 'get 'to-field))

    (when debug
      (tinypgpd fid
                "read-only"       buffer-read-only
                "USER-MODE"       user-mode
                "mail"            (ti::mail-mail-p)
                "pgp"             (ti::mail-pgp-p)
                "MIME"            (ti::mail-mime-maybe-p) mime-p
                "remail"          tinypgp-:r-mode-indication-flag
                "READ HASH"       read-hash
                "to-field hash"   val
                "to-field"        (mail-fetch-field "to")))

    (when (and (ti::mail-mail-p)
               (null buffer-read-only)
               (cond
                ((or (ti::mail-mime-maybe-p) mime-p)
                 ;;  MIME found, defeat auto action immediately.
                 ;;
                 (tinypgp-hash 'auto-action 'put 'elt nil)
                 nil)
                (t t))
               ;;
               ;;  only if there is no previous PGP,
               ;;  If there is PGP, let go through is there is
               ;;  remailer message Eg. newnym account create where
               ;;  you send you PGP key in buffer.
               ;;
               (if (ti::mail-pgp-p)
                   (if tinypgp-:r-mode-indication-flag
                       t nil)
                 t)

               (not (ti::nil-p (setq to-field (mail-fetch-field  "to"))))
               (not (string-match "," to-field)) ;skip multiple addresses
               (ti::nil-p (mail-fetch-field "cc")))

      ;; .................................................. hash check ...

      (cond
       ((and read-hash
             val                        ;previous TO field in HASH ?
             (string= to-field val)) ;compare previous with current TO
        (setq val   (tinypgp-hash 'auto-action 'get 'elt))
        (when debug (tinypgpd fid "hash ret"))
        (setq ret val))

       ;; .................................................. raw check ...

       (t

        ;; Empty field with spaces does not come here
        ;; To field has changed, we must calculate new entry
        ;; OR the hash-get wasn't set.

        (tinypgp-hash 'auto-action 'put 'to-field to-field)
        (tinypgp-hash 'auto-action 'put 'elt nil)
        (when debug (tinypgpd fid "evaluate"))

        ;; First check BBDB entry

        (setq ret (tinypgp-bbdb-entry))
        (when debug (tinypgpd fid to-field "BBDB" ret))

        ;; And this table overrrides bbdb

        (dolist (elt tbl)
          (setq val  (nth 0  elt))
          (when debug (tinypgpd fid "action tbl" val))
          (when (or                     ;Try to match
                 (and (stringp val)
                      (string-match val to-field))
                 (and (symbolp val) (not (ti::bool-p val))
                      (eval val)))
            (setq ret elt)
            (return)))

        (if ret
            (tinypgp-hash 'auto-action 'put 'elt ret))))) ;Save it!

    (if debug
        (tinypgpd fid "RET" ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action ()
  "Determine right auto action for mail message.
If auto-action has been disabled or if `tinypgp-mode' is off,  do nothing.

References:

  `tinypgp-:header-sign-table'
  `tinypgp-:auto-action-table'
  `tinypgp-:auto-action-defeat-hook'"
  (tinypgpd "tinypgp-auto-action: function entry")
  (let ((fid            "tinypgp-auto-action: ")
        (umode          (tinypgp-hash 'auto-action 'get 'user-mode))
        (multi-flag     (tinypgp-auto-action-multiple-addresses-p 'force))
        (pgp-p          (ti::mail-pgp-p))
        to-field
        sign enc mime-mua xpgp keyr
        email
        len
        elt)

    (tinypgpd fid 'user-mode umode 'multi-flag multi-flag 'pgp-p pgp-p)

    (when tinypgp-mode
      (run-hooks 'tinypgp-:auto-action-before-hook)

      (cond
       ;; ............................................ user defeat ...

       ((or pgp-p
            (run-hook-with-args-until-success
             'tinypgp-:auto-action-defeat-hook))
        (tinypgpd fid "defeated")
        nil)

       ;; ........................................... nymserver-cc ...

       ((and (tinypgp-nymserver-mail-p)
             (tinypgp-nymserver-send))  ;Maybe no multi-CC ?
        (tinypgpd fid "Nymserver"))

       ;; ............................................... defeated ...

       ((not umode)                     ;User has defeated the action
        (tinypgpd fid "Umode")
        nil)

       ;; ............................................ encrypt-to-many ...

       (multi-flag
        (tinypgpd fid "Multi")
        (tinypgp-auto-action-multiple-addresses))

       ;; ............................................ auto-action ...

       ((and (not (ti::nil-p (setq to-field  (mail-fetch-field   "to"))))

             ;;  The Addresses must be expanded so that they have @

             (string-match "@" to-field)

             ;; Force reading real action. If user has made changes
             ;; in his rc file; this guarrantees that we see them.

             (setq elt (tinypgp-auto-action-p))

             ;;  returns a list of email strings

             (setq email (ti::mail-email-from-string to-field)))

        (tinypgpd fid "--Action--" 'TO to-field 'EMAIL email 'ELT elt)

        (setq len  (length elt)
              sign (nth 1  elt)
              keyr tinypgp-:pubring-now)

        ;;  Should we change the key-id that is read from field?

        (setq email (car-safe (tinypgp-key-id-conversion email)))

        ;;  optional fields

        (setq enc        (if (> len 2) (nth 2 elt))
              mime-mua   (if (> len 3) (nth 3 elt))
              xpgp       (if (> len 4) (nth 4 elt))
              keyr       (tinypgp-expand-file-name
                          (cond
                           ((> len 5)
                            (nth 5 elt))
                           ((tinypgp-key-find-by-keyrings email))
                           (t
                            tinypgp-:pubring-now))))

        ;;  XE byteCompiler 19.14 has bug here, it reports that
        ;;  variable 'xpgp bound but not referenced, allthoug
        ;;  it is used in 'let' stement underneath! The following
        ;;  silences byteCompiler.

        (if (null xpgp) (setq xpgp nil))

        (tinypgpd fid "addr" email "ENC" enc "SIGN" sign "XP" xpgp
                  "KEY" keyr "MUA" mime-mua elt)

        (when (and mime-mua
                   (null (ti::mail-mime-tm-featurep-p))
                   (null (ti::mail-mime-semi-featurep-p)))
          (setq mime-mua nil)
          (message "\
Auto-action: PGP/MIME requested but no TM/SEMI mime support present.")
          (sit-for 2))

        (cond
         (mime-mua

          ;;  These only add the TAGS into the buffer. SEMI/TM
          ;;  hook handles the actual work of turning then to PGP/MIME
          ;;  --> It calls TinyPgp to do it.

          (if sign (ti::mail-mime-sign-region))
          (if enc  (ti::mail-mime-encrypt-region)))
         (t
          (tinypgp-save-state-macro
           (if sign   (setq tinypgp-:user-now
                            (if (and (not (ti::bool-p sign))
                                     (symbolp sign)) ;One pass encrypt/sign
                                (symbol-name sign)
                              sign)))
           (if keyr   (setq tinypgp-:pubring-now keyr))
           (setq tinypgp-:xpgp-signing-mode xpgp)

           (tinypgpd fid "SIGN" sign "KEY" keyr tinypgp-:user-now "KRING" keyr)

           ;; ............................................ do encrypt ...

           (when enc
             (when (and (not (ti::bool-p sign)) (symbolp sign))
               (tinypgp-password-set
                (format "[%s] Auto-action sign password: "
                        tinypgp-:user-now)))
             (tinypgp-encrypt-mail
              email
              (not 'register-insert)
              (if (and (not (ti::bool-p sign)) (symbolp sign))
                  '1pass)
              nil
              'verb))

           ;; ......................................... possibly sign ...

           (when (and sign (stringp sign))
             (tinypgp-password-set
              (format "Auto-action, Sign pass phrase %s: " tinypgp-:user-now))

             ;; The previous function call may have changed the user,
             ;; keep the pubring also in sync

             (tinypgp-pubring-change-to-current)
             (call-interactively 'tinypgp-sign-mail))))))

       ;; ........................................... auto-encrypt ...
       ;; If there is no auto action, we check if we have previously
       ;; encrypted to that person.

       ((and (null (ti::mail-pgp-p))    ;No previsou pgp
             (not (ti::nil-p (setq to-field  (mail-fetch-field   "to"))))
             (setq elt
                   (tinypgp-key-find-by-cache
                    (car-safe (ti::mail-email-from-string to-field)))))
        (tinypgpd fid "encrypt guess" to-field elt)))

      ;;  We actually do nothing here...but the code is ready
      ;;  (tinypgp-encrypt-mail email)

      (tinypgpd fid "out:" (current-buffer))

      ;; ..................................................... restore ...

      ;; If this was nym create request, restore pgp user
      ;; - If there are these buffer local variables and PGP msg found
      ;; - If saver "now" is "now"; ie. user hasn't changed active user
      ;;   after the create request was started.
      ;; - THEN restore the original pgp user

      (when (and (boundp 'tinypgp-pgp-user-original)
                 (boundp 'tinypgp-pgp-user-now)
                 (ti::mail-pgp-p))
        (let* ((orig  (symbol-value 'tinypgp-pgp-user-original))
               (now   (symbol-value 'tinypgp-pgp-user-now)))
          (if (string= now tinypgp-:user-now)
              (setq tinypgp-:user-now orig))))

;;;    (ti::d! "AUTO-ACT done" email)
      elt)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-multiple-addresses-p (&optional force)
  "Check multiple address auto-action. Optionally FORCE raw check."
  (and (null (tinypgp-nymserver-mail-p))
       (tinypgp-auto-action-multiple-addresses 'check force)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-multiple-addresses (&optional mode force)
  "Determine if multiple address encryption will be done.
Function does nothing if buffer is read only

Input:

  MODE      If 'check, then return nil or t if auto-action
            is in progress. Any other value starts auto-encryption
            if the conditions are met.

  FORCE     Force re-evaluating the buffer check (normally read result
            from stored value in hash table)

Return:

  non-nil       auto action in progress. All recipientsents have PGP
  LIST          '(email email ..) There were many recipients
                but not all members members have PGP. This is list of email
                addresses that had PGP.
  nil

References:

  `tinypgp-:auto-action-encrypt-regexp'
  `tinypgp-:auto-action-encrypt-ok-hook'"
  (let* ((re    tinypgp-:auto-action-encrypt-regexp)
         (fid   "tinypgp-auto-action-multiple-addresses: ")
         (debug nil)                    ;func is Called by timer...
         hsize-prev
         hsize
         list
         len
         ret
         pgp-ok-list
         pgp-nok-list)

    ;;  Because this function is called from timer process, the
    ;;  'check must be very quick in order not to decrease
    ;;  emacs performance
    ;;
    ;;  'many-addr-hsize
    ;;    We count the length of the header area and put that value
    ;;    into property. If the size has changed, we reread
    ;;    the To,CC,BCC headers again and do the checking
    ;;
    ;;    If the headers have not changed, then we don't do time
    ;;    consuming parse, but assume thet 'many-addr-hsize value
    ;;    is valid (No changes compared to last parse)
    ;;
    ;;  'many-addr-stat
    ;;    Holds value t or nil if auto action should be engaged.

    (if debug  (tinypgpd fid
                         "in: mode" mode
                         'force force
                         'mail  (ti::mail-mail-p)
                         'point (point) ))

    (when (and (ti::mail-mail-p)        ;Only do in mail buffers
               (null buffer-read-only))
      (setq hsize       (ti::mail-header-area-size)
            hsize-prev  (tinypgp-hash 'auto-action 'get 'many-addr-hsize))

      (when debug
        (tinypgpd fid
                  'hsize hsize
                  'prev hsize-prev
                  (ti::mail-get-all-email-addresses
                   nil tinypgp-:pgp-email-abbrev-list)
                  "point"
                  (point)))

      (if force (setq hsize nil hsize-prev 1)) ;Re-evaluate.

      (cond
       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .
       ((and (eq hsize hsize-prev)
             mode)
        ;; return the precalculated status
        ;;
        (setq ret (tinypgp-hash 'auto-action 'get 'many-addr-stat))
        (if debug (tinypgpd fid 'cond1-hash ret (point))))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .

       ((eq hsize hsize-prev)           ;Not check mode, do action
        (if debug (tinypgpd fid 'cond2-enc ret (point)))
        (if (and (tinypgp-hash 'auto-action 'get 'many-addr-stat)
                 (not (ti::mail-pgp-p))) ;No previous pgp
            (tinypgp-encrypt-mail-verbose)))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  recalculate . .
       ;; *) The size has changed, so update it immediately.
       ;; *) put initial value into the property, because we may not enter
       ;;    the case at all if list is empty
       ;;
       ;; The rest of the 'and' are real tests

       ((and (prog1 t
               (tinypgp-hash 'auto-action 'put 'many-addr-hsize hsize)
               (tinypgp-hash 'auto-action 'put 'many-addr-stat nil))
             (setq list (ti::mail-get-all-email-addresses
                         nil tinypgp-:pgp-email-abbrev-list))
             (> (setq len  (length list)) 1))
        (if debug  (tinypgpd fid 'cond3 len list (point)))
;;;     (ti::d! "HZ" hsize (tinypgp-hash 'auto-action 'get 'many-addr-hsize))

        ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. do checking ..
        ;;  Is there regexp defined in the table ?

        (when (stringp re)
          ;;  See if there is hit for all recipinets,
          ;;  then we want t'1o encrypt this mail.
          ;;
          ;;  Other times; this may be just regular CC mail

          (dolist (elt list)

            (when debug
              (tinypgpd fid 'dolist-match
                        (string-match re elt)
                        (if (string-match re elt)
                            (match-string 0 elt))
                        elt))

            (if (string-match re elt)
                (push elt pgp-ok-list)
              (push elt pgp-nok-list)))

          (if debug  (tinypgpd fid (ti::mail-pgp-p)))

          (tinypgp-hash 'auto-action 'put 'many-addr-ok-list  pgp-ok-list)
          (tinypgp-hash 'auto-action 'put 'many-addr-nok-list pgp-nok-list)

          ;;  There must not be no PGP already in the buffer!

          (if (ti::mail-pgp-p)
              (tinypgp-hash 'auto-action 'put 'many-addr-stat nil)
            (tinypgp-hash 'auto-action 'put 'many-addr-stat pgp-ok-list)))

        (setq ret (tinypgp-hash 'auto-action 'get 'many-addr-stat))

        ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  return action ..
        (when (and ret (null mode))
          (setq pgp-ok-list  (tinypgp-hash 'auto-action 'get 'many-addr-ok-list)
                pgp-nok-list (tinypgp-hash 'auto-action 'get 'many-addr-nok-list))
          (if (null pgp-nok-list)
              (tinypgp-encrypt-mail-verbose)

            ;; #todo: send each message separately: those who have PGP
            ;; #todo: and those that don't? Hmm.. this function
            ;; is run from `mail-send-hook' so we have to send non-pgp first.

            (message "TinyPgp: auto-action info, not all recipients have pgp")
            (sleep-for 2)

            (if (y-or-n-p "\
Would you like to send separate PGP and plain mail messages?")
                (let* ((orig   (current-buffer))
                       (buffer (tinypgp-ti::temp-buffer 'mail "" "")))
                  (with-current-buffer buffer
                    (erase-buffer)
                    (insert-buffer orig)
                    (ti::mail-set-recipients pgp-nok-list pgp-ok-list 'cc-all)
;;;               (pop-to-buffer buffer)
                    (mail-send-and-exit nil))
                  ;; Now it's time to encrypt this message for recipients that
                  ;; do have pgp.

                  (ti::mail-set-recipients pgp-ok-list pgp-nok-list)
                  (tinypgp-encrypt-mail (tinypgp-key-id-conversion pgp-ok-list)))))))
       )) ;; if-let

    (if debug  (tinypgpd fid 'RET ret 'pgp-p (ti::mail-pgp-p) 'point (point) "\n"))

    ;;  If we decided it was okay to send multiple encrypted message,
    ;;  let user say final word

    (if (and ret tinypgp-:auto-action-encrypt-ok-hook)
        (setq ret (run-hook-with-args
                   tinypgp-:auto-action-encrypt-ok-hook
                   list)))

    (if debug (tinypgpd fid 'after-user-hook ret))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-auto-action-toggle (&optional mode verb)
  "If the auto action is detected for this buffer, toggle MODE on/off.
Otherwise if no auto action is present, do nothing. VERB."
  (interactive)
  (let* ((act   (tinypgp-auto-action-p))
         val)
    (ti::verb)
    (if (null act)
        (if verb (message "TinyPgp: no action entry found for this buffer."))
      (setq val (tinypgp-hash 'auto-action 'get 'user-mode))

      (ti::bool-toggle val mode)
      (tinypgp-hash 'auto-action 'put 'user-mode val)
      (tinypgp-update-modeline)

      (if verb
          (message (format "TinyPgp auto action: %s"
                           (if (not val) "pending" "defeated")))))))

;;}}}
;;{{{ misc: functions

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-require-final-newline ()
  "Make sure there is empty line at the end."
  (save-excursion
    (ti::pmax)
    (if (not (looking-at "^[ \t]*$"))
        (insert "\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-xpgp-get-info ()
  "Return X-pgp info '((MIME-KEY  DATA) (MIME-K  DATA) ..) or nil.
This function caches the read X-Pgp information so that the parsing
doesn't take effect in every call. The cache will be expired if the buffer
size has changed and the new data parsing will be done."
  (let* ((fid   "tinypgp-xpgp-get-info:")
         (size  (- (point-max) (point-min)))
         field
         list)
    (cond
     ((and (eq  size  (tinypgp-hash 'xpgp-info 'get 'size))
           (setq list (tinypgp-hash 'xpgp-info 'get 'data)))
      (tinypgpd fid "cache"))
     ((setq field  (mail-fetch-field "X-Pgp-signed"))
      (setq list   (ti::mail-mime-parse-header field 'downcase))
      (tinypgp-hash 'xpgp-info 'put 'size size)
      (tinypgp-hash 'xpgp-info 'put 'data list)))
    (tinypgpd fid (current-buffer) list)
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-xpgp-key-address (type &optional message)
  "Return TYPE (Finger or http) URL if can be found from X-Pgp header.
Print optional MESSAGE if there is no such information.

TYPE can be
 'finger
 'http

Return:
 string
 nil"
  (let* ((elt (tinypgp-xpgp-get-info))
         ret)
    (when elt
      (cond
       ((and (eq type 'finger)
             (setq elt (assoc "address" elt)))
        (setq ret (nth 1 elt)))
       ((and (eq type 'http)
             (setq elt (assoc "url" elt)))
        (setq ret (nth 1 elt)))))
    (if message
        (message message))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-sign-mode-toggle (&optional mode)
  "Toggle signing of selected headers `tinypgp-:header-sign-table' with MODE.
When the mode if OFF, the `tinypgp-:header-sign-table' is ignored."
  (interactive)
  (let* ((sym 'tinypgp-:header-sign-table))
    ;; Not recorded; record original value
    ;;
    (if (null (get sym 'original))
        (put sym 'original (symbol-value sym)))

    (cond
     ((or (memq mode '(0 -1))
          (symbol-value sym))
      (set sym nil)
      (message "Headers are not signed: tinypgp-:header-sign-table is ignored."))
     (t
      (set sym (get sym 'original))
      (message "Header list tinypgp-:header-sign-table is used.")))
    (tinypgp-update-modeline)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-list-show ()
  "See what headers will be signed for this message."
  (interactive)
  (tinypgp-header-sign-active-list 'display))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-sign-active-list (&optional display)
  "See what headers we should sign. Optionally DISPLAY to user.
Subject is the only safe field to sign when you for example
send a message to some mailing list that may alter all other fields.

References:
  `tinypgp-:header-sign-table'"
  (let* ((list   tinypgp-:header-sign-table)
         to
         elt)
    (and list
         (setq to (or (mail-fetch-field  "To")
                      (mail-fetch-field  "Newsgroups")))
         (not (ti::nil-p to))
         (setq elt (ti::list-find list to)))
    (when display
      (cond
       ((ti::nil-p to)
        (message "Header sign info: Can't find field To or Newsgroups."))
       ((null elt)
        (message "Header sign info: To or Newsgroup header does not trigger."))
       (t
        (message "Header sign info: %s" (ti::list-to-string (nth 1 elt))))))

    elt))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-move-to-body (&optional opt1 opt2)
  "Move headers into body and anonymize them. See source for OPT1 and OPT2"
  (interactive)
  (ti::mail-pgpr-anonymize-headers
   (or opt1 'move-to-body-maybe) opt2 "message" "dummy"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-kill (&optional add-list)
  "Kill all but the most crucial headers.
ADD-LIST is additional headers to keep."
  (let* ((hlist (ti::list-merge-elements
                 (mapcar
                  (function
                   (lambda (x)
                     (make-symbol
                      (downcase x))))
                  tinypgp-:r-header-keep-list)
                 (ti::mail-required-headers))))
    (ti::mail-kill-non-rfc-fields hlist)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-header-sign-make-smf (&optional read-xpgp &optional header-list)
  "Construct header SMF (a stripped message format).
Read header field names and their contents from the message. If some
header does not exist or is empty in message, then that header is ignored.

References:

  `tinypgp-:header-sign-table'      Read from
  `tinypgp-:header-sign-smf-info'           Written to

Input:

  READ-XPGP    ,The headers that were signed are told in X-Pgp.
                If cannot read all headers, signal error.
  HEADER-LIST  ,list of headers names

Return:
  (string (hdr hdr ..))        ,SMF'd header-string and headers included
  nil"
  (interactive)
  (let* ((fid   "tinypgp-header-sign-make-smf:")
         elt
         (list (or header-list
                   (if (setq elt (tinypgp-header-sign-active-list))
                       (nth 1 elt))))
         hdr-name
         flag
         fld
         hlist
         str
         buffer
         ret)

    (tinypgpd fid "in" read-xpgp header-list "list" elt)
    ;;  Clear this global
    ;;
    (setq tinypgp-:header-sign-smf-info nil)

    (when read-xpgp
      (setq fld (mail-fetch-field        "X-Pgp-signed"))
      (when (setq fld (ti::string-match "SignedHeaders=\\([^;]+\\);" 1 fld))
        ;;  Remove newlines, because the field may continue
        ;;
        (setq fld  (subst-char-with-string fld ?\n " "))
        (setq list (split-string fld "[ ,]+"))))

    (when (ti::listp list)
      (setq buffer (tinypgp-ti::temp-buffer))
      ;;  Get the fields
      ;;
      (save-restriction
        (dolist (elt list)

          (when (setq str (ti::mail-get-field elt nil 'pure))

            ;;  this code is inside loop, because outside loop
            ;;  we don't know if we got any headers
            ;;
            (unless  flag               ;Do only once
              (setq flag t)             ;Remailer type header hash ##
              (ti::append-to-buffer buffer "##\n"))

            ;;  We want to store the real header name, not the "list"
            ;;  names that can be "reply-to", where real header name is like
            ;;
            ;;  REPLY-to:  .....
            ;;
            (setq hdr-name (ti::string-match "^\\([^:]+\\):" 1 str))
            (tinypgpd fid "READ" elt "NAME" hdr-name str)
            (ti::nconc hlist hdr-name)

            (ti::append-to-buffer
             buffer
             (format "%s\n" (ti::string-remove-whitespace str) )))))

      (when hlist
        ;;   Add final newline after the headers.
        ;;
        (ti::append-to-buffer buffer "\n")
        (with-current-buffer buffer
          (setq ret (buffer-substring (point-min) (point-max))))))

    (tinypgpd fid "ret" ret hlist)

    (when ret
      (setq tinypgp-:header-sign-smf-info (list ret hlist)))))

;;}}}
;;{{{ timer control

;;; .......................................................... &timers ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-timer-process ()
  "PGP timer process. Expires stored password and update mode line."
  ;;  Run only if some visible windows has tinypgp-mode on.
  ;;
  (let (do-it)
    (dolist (win (ti::window-list))
      (with-current-buffer (window-buffer win)
        (when tinypgp-mode (setq do-it t   wlist nil))))

    (if do-it (tinypgp-update-modeline))
    (if (not (tinypgp-password-time-valid-p))
        (tinypgp-password-expire-now))
    do-it))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-timer-control (&optional remove verb)
  "Keep the password expiration timer alive. Optionally REMOVE it. VERB."
  (interactive "P")
  (let* ((fid   "tinypgp-timer-control: ")
         (timer tinypgp-:timer-elt))
    (ti::verb)

    (tinypgpd fid "in:" timer)

    (ti::compat-timer-cancel-function 'tinypgp-timer-process)

    (unless remove
      (setq tinypgp-:timer-elt (run-at-time "10 sec" 10 'tinypgp-timer-process)))

    (when verb
      (if remove
          (message "TinyPgp timer process installed")
        (message "TinyPgp timer process removed.")))))

;;}}}
;;{{{ password control

;;; ........................................................ &password ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-password-expire-now (&optional no-file-kill verb)
  "Expire all PGP passwords including used files.
Input:

  NO-FILE-KILL  if non-nil, then temporary files are not removed.
  VERB          Display verbose message."
  (interactive "P")
  (ti::verb)
  (tinypgpd "tinypgp-password-expire-now" no-file-kill verb)

  ;;  Do not leave traces to memory (gc)
  ;;
  (let* ((gc-cons-threshold (* 1024 1024)))
    (ti::vector-table-clear tinypgp-:hash-password))

  ;;  Create new
  ;;
  (ti::vector-table-init tinypgp-:hash-password)

  ;;  This command also may contains the password, wipe it
  ;;
  (setq tinypgp-:last-pgp-exe-command nil)
  (tinypgp-hash 'password-time 'put 'tick nil 'global)

  (if (null no-file-kill)
      (tinypgp-file-control 'all-kill))

  (when (or verb (interactive-p))
    ;;  If user called us; expire also secring password
    ;;
    (tinypgp-secring-crypt-expire-password)
    (message "TinyPgp: all pass phrases and files expired.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-password-get  ()
  "Get password."
  (let* ((sym tinypgp-:user-now)
         (type (tinypgp-hash 'action 'get 'now nil 'global))
         ret)
    (tinypgpd "tinypgp-password-get:" sym type)

    ;;  This may be "pgp" decrypt or "conventional". pick right
    ;;  password from hash.

    (when (string= "conventional" type)
      (setq sym "conventional"))

    (tinypgp-password-set
     "Password: "
     (if (string= "conventional" type)
         'conventional))

    (unless (setq ret
                  (ti::vector-table-property
                   tinypgp-:hash-password sym 'password))
      (error "Internal error. Password hash corrupt."))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-password-set (&optional prompt type)
  "Set pass phrase for `tinypgp-:user-now' or ask again with PROMPT (expired).
Eg. if last PGP command terminated to error, that had expired all
pass phrases.

Input:

  prompt    string, Prompt to user
  TYPE      symbol, if 'conventional, set conventional password.
            If 'e-s, set one pass encrypt&sign password

Return:
 t     if password available"
  (let* ((fid "tinypgp-password-set:")
         (sym (if (and (not (ti::bool-p type))
                       (symbolp type))
                  (symbol-name type)
                tinypgp-:user-now))
         ret
         pass)
    (or prompt
        (setq
         prompt
         (cond
          ((equal type 'conventional)
           "Conventional decrypt password: ")
          ((equal type 'e-s)
           (format "[%s] One pass encrypt&Sign password: " tinypgp-:user-now))
          (t            (format "[%s] Pass phrase:  " tinypgp-:user-now)))))

    (tinypgpd fid "in:" tinypgp-:user-now prompt type sym)

    (if (and (ti::vector-table-get tinypgp-:hash-password sym)
             (ti::vector-table-property tinypgp-:hash-password sym 'password)
             (tinypgp-password-time-valid-p)
             (null tinypgp-:error))
        (setq ret t)                    ;Ok, was in hash

      ;;  unwind: Makes sure 'pass' is wiped away

      (unwind-protect
          (progn
            (setq pass (ti::compat-read-password prompt))
            (when tinypgp-:password-keep-time

              ;;  Create new user to hash table

              (intern sym  tinypgp-:hash-password)

              ;;  Set user's password in the hash

              (ti::vector-table-property tinypgp-:hash-password sym 'password pass 'force)
              (tinypgp-hash 'password-time 'put 'tick nil 'global)
              (setq tinypgp-:error nil)
              (setq ret t)))))
;;; Hmm; this also wipes the password from hash; why?
;;;     (if pass (fillarray pass 0))

    (tinypgpd fid "out:" tinypgp-:user-now prompt type ret)

    ret))

;;}}}

;;{{{ installation funcs

;;; ----------------------------------------------------------------------
;;; We can't initialize the substitution table in defvar, because
;;; it may be possible that some user sats (setq ...) and then these
;;; definitions aren't there any more.
;;;
(defun tinypgp-install-default-substitutions (&optional remove)
  "Add default email substitutions or REMOVE."
  (let* ((nymserver-re
          (concat
           "\\("
           (mapconcat
            'concat
            '("anon" "finger" "ping" "remove" "help"
              "nick"
              "newpassword" "newalias" "newpgp" "newaddress"
              "vacation" "noarchive" "setnon" "paranoid"
              "pgpencrypt" "pgpsign" "sendmix"
              "abuse")
            "\\|")
           "\\)@anon.nymserver.com"))

         (weasel-re "@weasel.owl.de\\|@squirrel.owl.de"))

    (tinypgp-email-substitution-add
     (list
      ;; the 2nd entry is found from PGP key id.
      (cons nymserver-re "Nymserver at anon.nymserver.com")

      ;;  You can get the Weasel 'newnym' PGP key from
      ;;     <info@weasel.owl.de>
      ;;     Johannes Kroeger <jkroeger@squirrel.owl.de>
      ;;
      ;;  Squirrel.owl.de and weasel.owl.de offer the following mail services:
      ;;  1.  The Squirrel Remailer, a Mixmaster/Ghio remailer combination:
      ;;
      ;;  The capabilities of the Ghio remailer are: $remailer{"squirrel"} =
      ;;  "<mix@squirrel.owl.de> cpunk mix pgp pgponly hash latent cut ek" The
      ;;  abbrevs are explained in http://www.publius.net/rlist.html
      ;;
      ;;  It accepts only PGP messages encrypted

      (cons weasel-re "config@weasel.owl.de"))
     remove)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-send-mail-hook (&optional remove)
  "Install right hook order to `' or REMOVE hooks."
  (let* (hook
         func)

    ;;   Hook chain is this:
    ;;
    ;;     tinypgp-password-wipe-buffer
    ;;     tinypgp-sign-modify-check
    ;;     tinypgp-auto-action
    ;;     --> rest of the user hooks.
    ;;
    ;;   The REST user hooks that do something TO BUFFER before sending
    ;;   message, should be in tinypgp-cmd-
    ;;   or to `tinypgp-:auto-action-before-hook'.
    ;;

    (setq hook tinypgp-:mail-send-hook-list
          func '(tinypgp-auto-action
                 tinypgp-sign-modify-check
                 tinypgp-password-wipe-buffer))

    ;;  First remove then add --> puts hooks to the beginning.
    ;;  IMPORTANT:
    ;;
    ;;      tinypgp-auto-action         --> add SEMI tags
    ;;      mime-edit-maybe-translate   --> translate tags and make PGP/MIME
    ;;
    ;;  So, TM/SEMI hook must be after TinyPgp hooks.

    (ti::add-hooks hook func 'remove)

    ;; Add the hooks in right order

    (unless remove
      (ti::add-hooks hook func))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-hooks-vital (&optional remove)
  "Install and keep vital functions in right order. Optionally REMOVE."
  (interactive "P")
  (let* (func
         list)

    ;; .............................................. kring find hooks ...

    (setq list
          '(tinypgp-key-find-by-cache
            tinypgp-key-find-by-keyrings-verbose
            tinypgp-key-find-by-finger-verbose
            tinypgp-key-find-by-http-url-verbose
            tinypgp-key-find-by-http-keyserver-verbose))

    (ti::add-hooks 'tinypgp-:find-by-guess-hook list 'remove)
    (unless remove (ti::add-hooks 'tinypgp-:find-by-guess-hook (nreverse list)))

    ;; ................................................. control hooks ...

    (remove-hook 'tinypgp-:cmd-macro-after-hook
                 'tinypgp-mode-specific-control-after)
    (unless remove
      (add-hook 'tinypgp-:cmd-macro-after-hook
                'tinypgp-mode-specific-control-after 'append))

    ;; .......................................................... mail ...

    (tinypgp-install-send-mail-hook remove)

    ;; ...................................................... external ...
    ;; It is essential that mime translate hooks is after TinyPgp
    ;; or otherwise eg when you send patch:
    ;;
    ;;  o   content is made quoted printble (=3D ...)
    ;;  o   auto action triggers encrypting
    ;;  --> receiving end doesn't get clean patch

    (setq func 'mime-editor/maybe-translate ;TM.el
          list '(mail-send-hook
                 message-send-hook))

    (dolist (hook list)
      (when (and (boundp hook)
                 (memq func (symbol-value hook)))
        (remove-hook hook func)
        ;; Make sure it is last
        (add-hook hook func 'append)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-hooks (&optional remove)
  "Install package hooks. Optionally REMOVE installation.
Can't restore changes to key maps."
  (interactive "P")

  (ti::add-hooks 'find-file-hooks 'turn-on-tinypgp-mode-maybe remove)

  (ti::add-hooks tinypgp-:turn-on-hook-list 'turn-on-tinypgp-mode remove)

  (ti::add-hooks 'tinypgp-:define-keys-hook ;; just to make sure they are there.
                 '(tinypgp-mode-define-menu
                   tinypgp-mode-define-keys))

  (ti::add-hooks 'tinypgp-:key-mode-define-keys-hook
                 '(tinypgp-key-mode-define-menu
                   tinypgp-key-mode-define-keys))

  (ti::add-hooks 'tinypgp-:summary-mode-define-keys-hook
                 '(tinypgp-summary-mode-define-menu
                   tinypgp-summary-mode-define-keys))

  (ti::add-hooks 'tinypgp-:newnym-mode-define-keys-hook
                 '(tinypgp-newnym-mode-define-menu
                   tinypgp-newnym-mode-define-keys))

  (ti::add-hooks '(rmail-show-message-hook
                   vm-display-buffer-hook
                   mh-show-hook)
                 'tinypgp-hide
                 remove)

  (ti::add-hooks '( ;; RMAIL summary is handled elswhere
                   vm-summary-mode-hook
                   gnus-summary-mode-hook
                   mh-show-mode-hook)
                 'turn-on-tinypgp-summary-mode
                 remove)

  (ti::add-hooks 'gnus-select-article-hook 'tinypgp-hide-gnus remove) ;Gnus 4

  (tinypgp-install-hooks-vital remove)

  ;;  This must be after the mode specific hook has finished.

  (unless remove
    (add-hook 'tinypgp-:cmd-macro-after-hook
              'tinypgp-after-pgp-command 'append))

  (ti::add-hooks 'tinypgp-:verify-before-hook
                 'tinypgp-mode-specific-control-before
                 remove)

  (ti::add-hooks 'tinypgp-:verify-after-hook
                 'tinypgp-mode-specific-control-after
                 remove)

  (ti::add-hooks 'write-file-hooks      ; ~/.mailrc parsing
                 'tinypgp-update-mail-abbrevs-hook
                 remove)

  (ti::add-hooks 'tinypgp-:r-reply-block-basic-hook
                 'tinypgp-r-mail-mode-init
                 remove))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-menu-bar-remail ()
  "Disable or enable items from menubar."
  (if (tinypgp-hash 'remail 'get 'init nil 'global) ;If initialised
      (put 'tinypgp-:mode-menu 'remail t)
    (put 'tinypgp-:mode-menu 'remail nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-menu-bar-newnym ()
  "Disable or enable items from menubar."
  ;;  Hmm. Let me think of some test here later; Now it is enabled always.
  ;;
  (if (tinypgp-hash 'remail 'get 'init nil 'global)
      (put 'tinypgp-:mode-menu 'newnym t)
    (put 'tinypgp-:mode-menu 'newnym nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-menu-bar-nymserver ()
  "Disable or enable items from menubar."
  ;;  Enable only if user has ordered Nymserver account
  (if (ti::listp tinypgp-:nymserver-account-table)
      (put 'tinypgp-:mode-menu 'nymserver t)
    (put 'tinypgp-:mode-menu 'nymserver nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-menu-bar ()
  "Disable or enable items from menubar."
  (tinypgp-install-menu-bar-remail)
  (tinypgp-install-menu-bar-newnym)
  (tinypgp-install-menu-bar-nymserver))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-check-environment  ()
  "Check basic environment variabler or die on error.
PGP uses TMP for temporary files, make sure directory is accessible."
  (let* ((dir  (getenv "TMP"))
         file)

    ;; PGP 2.6.x uses TMP env variable. See pgp.doc

    (unless dir
      (message "TinyPgp: WARNING, environment variable TMP is not set.")
      (sleep-for 2)
      (dolist (directory '("/tmp" "/temp"))
        (when (file-directory-p directory)
          (setenv "TMP" directory)
          (setq dir directory)
          (message "TinyPgp: Setenv TMP ==> %s" directory)
          (return))))

    (when (file-directory-p dir)
      (setq file (ti::file-make-path dir "tinypgp.tmp")))

    (cond
     ((null dir)
      (error "TinyPgp: environment variable TMP is not set."))

     ((not (file-directory-p dir))
      (error "TinyPgp: environment variable TMP is not pointing to directory"))

     ((not (file-writable-p file))
      (error "TinyPgp: Can't write to TMP dir: %s" dir))

     ;; Actually try to write, one day I got weir error from my TMP
     ;; file system. This neede fcsk run because disk had inode broken.
     ;;
     ;;   echo test > test.txt
     ;;   test.txt: No such device or address.

     ((with-temp-buffer
        (insert "test\n")
        (write-region (point-min) (point-max) file) ;Breaks if not ok
        ;;  Breaks if not ok
        (delete-file file))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install (&optional remove)
  "Install whole package or REMOVE installation.
This is main installation controller."
  (interactive)
  (tinypgpd "tinypgp-install in:" remove)
  (tinypgp-install-check-environment)

  (tinypgp-binary-path-set)

  ;;   Set the backenmd if thsi is firt time when program loads

  (unless (get 'tinypgp-:pgp-binary 'pgp-now)
    (tinypgp-backend-select-auto))

  (tinypgp-secring-crypt-mode-detect)
  (tinypgp-install-default-substitutions)

  (tinypgp-install-hooks            remove)
  (tinypgp-timer-control            remove)

;;; this is run from 'update modeline' Do not call here; because
;;; we're in wrong buffer and TP mode is not on.
;;;
;;;   (tinypgp-install-menu-bar)

  (unless remove
    (tinypgp-key-cache-save 'load))
  (tinypgpd "tinypgp-install out:"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-to-current-emacs ()
  "Examine every emacs buffer and turn on PGP minor mode when needed."
  (interactive)

  ;;  Forced install. Clear these

  (put 'tinypgp-:hash 'vm-check nil)   (tinypgp-install-vm)
  (put 'tinypgp-:hash 'gnus-check nil) (tinypgp-install-gnus)

  (put 'tinypgp-:hash 'mime-backend-in-use nil)
  (put 'tinypgp-:hash 'mime-backend-in-use nil)
  (tinypgp-install-mime-pgp)

  ;; If user loads TinyPgp, it should immediately install itself to
  ;; appropriate buffers. Otherwise user has to call manually
  ;; `tinypgp-mode' for every mail buffer and that is not very nice.

  (save-excursion
    (dolist (elt (buffer-list))
      (set-buffer elt)
      (cond
       ((memq major-mode '(vm-mode
                           rmail-mode
                           rmail-edit-mode
                           mail-mode
                           message-mode
                           gnus-article-mode
                           gnus-article-edit-mode
                           mime/viewer-mode)) ;TM
        (unless tinypgp-mode (tinypgp-mode 1)))

       ((memq major-mode '(vm-summary-mode
                           rmail-summary-mode
                           gnus-summary-mode))
        (unless tinypgp-summary-mode
          (tinypgp-summary-mode 1)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-gnus-do ()
  "Add Headers to GNUS."
  (let* ((h    "X-pgp-signed")
         (hdr  "\\|X-pgp-signed:")
         (h2   "^X-pgp-signed:")
         sym
         val)

    ;;  Bytecomp silencer with symbols

    (dolist (sym '(gnus-saved-headers gnus-visible-headers))
      (setq val  (symbol-value sym))
      (if (not (stringp val))
          (error "Install problem1: See manual for GNUS installation.")
        (unless (string-match h val)
          (set sym (concat val hdr)))))

    (setq sym 'gnus-sorted-header-list   val (symbol-value sym))

    (if (not (ti::listp val))
        (error "Install problem2: See manual for GNUS installation.")
      (unless (member h2 val)
        ;;  Add to the end
        (set sym (append val (list h2)) )))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-gnus (&optional force)
  "Check that GNUS is configured right. Optionally FORCE."
  (when (and (featurep 'gnus)
             (or (boundp 'gnus-saved-headers) ;Gnus check
                 ;; Not bound, this is old gnus. Do not install
                 ;;
                 (prog1 nil (put 'tinypgp-:hash 'gnus-check t)))
             (null   (get 'tinypgp-:hash 'gnus-check)))
    (tinypgp-install-gnus-do)
    ;;  Done, do not repeat
    (put 'tinypgp-:hash 'gnus-check t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-mime-tm-do ()
  "Install package to TM."
  (unless (featurep 'tm-tinypgp-setup)
    (or (load "tm-tinypgp-setup" 'noerr)
        (progn
          (message
           "tm-tinypgp-setup.el not found. Couldn't auto-install to TM")
          (sleep-for 5)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-mime-semi-do ()
  "Install package to SEMI."
  (unless (featurep 'mime-tinypgp-setup)
    (or (load "mime-tinypgp-setup" 'noerr)
        (progn
          (message
           "mime-tinypgp-setup.el not found. Couldn't auto-install to SEMI")
          (sleep-for 5)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-mime-tm (&optional force)
  "Check that GNUS is configured right. Optionally FORCE."
  (when (and (null (get 'tinypgp-:hash 'mime-backend-in-use))
             (ti::mail-mime-tm-featurep-p))
    (when (ti::mail-mime-semi-featurep-p)
      (error "\
TinyPgp: Conflict; Trying to use TM while SEMI is present. Restart Emacs."))
    (tinypgp-install-mime-tm-do)
    ;;  Done, do not repeat
    (put 'tinypgp-:hash 'mime-backend-in-use 'tm)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-mime-semi (&optional force)
  "Check that GNUS is configured right. Optionally FORCE."
  (when (and (null (get 'tinypgp-:hash 'mime-backend-in-use))
             (ti::mail-mime-semi-featurep-p))
    (when (ti::mail-mime-tm-featurep-p)
      (error "\
TinyPgp: Conflict; Trying to use SEMI while TM is present. Restart Emacs."))
    (tinypgp-install-mime-semi-do)
    (put 'tinypgp-:hash 'mime-backend-in-use 'semi)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-mime-pgp (&optional force)
  "Install PGP/MIME support or possible FORCE install. Need TM or SEMI."
  (interactive "P")
  (when (or force
            (null (get 'tinypgp-:hash 'mime-backend-in-use)))
    (cond
     ((ti::mail-mime-tm-featurep-p)
      (tinypgp-install-mime-tm)
      (tinypgp-install-hooks-vital))    ;Arrange TM look last

     ((ti::mail-mime-semi-featurep-p)
      (tinypgp-install-mime-semi)
      ;;  Arrange SEMI hook last
      (tinypgp-install-hooks-vital)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-vm ()
  "Install minor mode indication to VM summary buffer."
  ;;  1. User loads TinyPgp and VM is not loaded yet
  ;;     --> this function does nothing
  ;;  2. When user uses commands afterwards in VM, this function
  ;;     is called to chek the situation.
  ;;
  (when (and (featurep 'vm)
             (null (get 'tinypgp-:hash 'vm-check)))
    (let* ((sym     'vm-mode-line-format)
           (val     (symbol-value sym))
           (hdr     "X-Pgp-Signed:"))

      ;;  The modeline format is defined in vm-vars.el::vm-mode-line-format,
      ;;  but it does not have variable minor-mode-alist. That's why TPsum
      ;;  mode is not shown in summary buffer.
      ;;
      (save-excursion
        (dolist (buffer (buffer-list))
          (set-buffer buffer)
          (when (and (eq major-mode 'vm-summary-mode)
                     (not (memq 'minor-mode-alist val)))
            ;;  Add this and update modeline
            (ti::nconc val 'minor-mode-alist)
            (ti::compat-set-mode-line-format val)
            (vm-update-summary-and-mode-line))))

      (setq sym 'vm-visible-headers  val (symbol-value sym))
      (tinypgpd "tinypgp-install-vm:" sym val)

      (if (not (ti::listp val))
          (error "Install problem: See manual for VM installation.")
        (unless (member hdr val)
          ;;  Add to the end
          (set sym (append val (list hdr))) ))

      (put 'tinypgp-:hash 'vm-check t))))

;;}}}
;;{{{ install: modes, keys

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-install-modes (&optional remove)
  "Install or REMOVE minor modes.
Calling this always removes old mode and does reinstall."
  (interactive "P")
  (cond
   (remove
    (ti::keymap-add-minor-mode 'tinypgp-mode     nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-key-mode nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-summary-mode nil nil 'remove))

   (t
    (setq tinypgp-:mode-map  (make-sparse-keymap)) ;; always refresh
    (run-hooks 'tinypgp-:define-keys-hook)
    (ti::keymap-add-minor-mode 'tinypgp-mode nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-mode
                               'tinypgp-:mode-name
                               tinypgp-:mode-map)

    (setq tinypgp-:key-mode-map  (make-sparse-keymap)) ;; always refresh
    (run-hooks 'tinypgp-:key-mode-define-keys-hook)
    (ti::keymap-add-minor-mode 'tinypgp-key-mode nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-key-mode
                               'tinypgp-:key-mode-name
                               tinypgp-:key-mode-map)

    (setq tinypgp-:summary-mode-map  (make-sparse-keymap)) ;; always refresh
    (run-hooks 'tinypgp-:summary-mode-define-keys-hook)
    (ti::keymap-add-minor-mode 'tinypgp-summary-mode nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-summary-mode
                               'tinypgp-:summary-mode-name
                               tinypgp-:summary-mode-map)

    (setq tinypgp-:newnym-mode-map  (make-sparse-keymap)) ;; always refresh
    (run-hooks 'tinypgp-:newnym-mode-define-keys-hook)
    (ti::keymap-add-minor-mode 'tinypgp-newnym-mode nil nil 'remove)
    (ti::keymap-add-minor-mode 'tinypgp-newnym-mode
                               'tinypgp-:newnym-mode-name
                               tinypgp-:newnym-mode-map))))

;;}}}

;;{{{ menu: main

(put 'tinypgp-:mode-menu 'nymserver nil)

;;; ------------------------------------------------------------ &menu ---
;;;
(defun tinypgp-mode-define-menu ()
  "Define menus."
  (easy-menu-define
    tinypgp-:mode-menu
    (if (ti::xemacs-p) nil (list tinypgp-:mode-map))
    "TinyPgp menu"
    (list
     tinypgp-:mode-menu-name
     ["Next action"                     tinypgp-next-action-mail                    t]
     ["Sign"                            tinypgp-sign-mail                           t]
     ["Sign, base64"                    tinypgp-sign-mail-base64                    t]
     ["Sign, detached"                  tinypgp-sign-mail-detached          t]
     ["Sign, PGP/MIME"                  tinypgp-sign-mail-mime              t]

     ["Encrypt"                         tinypgp-encrypt-mail                t]
     ["Encrypt and sign (one pass)"     tinypgp-encrypt-mail-sign                   t]
     ["Encrypt PGP/MIME"                tinypgp-encrypt-mail-mime                   t]

     ["Decrypt"                         tinypgp-decrypt-mail                t]
     ["Verify"                          tinypgp-verify-mail                 t]
     ["Verify detached signature on file"   tinypgp-verify-detached-signature t]
     ["Conventional crypt"              tinypgp-crypt-mail                  t]
     ["Insert file, base64 signed"      tinypgp-sign-base64-insert-file     t]

     "----"

     (list
      "Region PGP"
      ["Sign"                           tinypgp-sign-region                 t]
      ["Sign, base64"                   tinypgp-sign-region-base64          t]
      ["Sign, detached"                 tinypgp-sign-region-detached        t]
      ["Encrypt"                        tinypgp-encrypt-region              t]
      ["Encrypt and sign (one pass)"    tinypgp-encrypt-region-sign         t]
      ["Decrypt"                        tinypgp-decrypt-region              t]
      ["Verify"                         tinypgp-verify-region               t])

     (list
      "Key handling"
      ["Fetch by finger"                tinypgp-key-find-by-finger          t]
      ["Fetch by http [keyserver]"      tinypgp-key-find-by-http-guess      t]
      ["Fetch by email keysrv request"  tinypgp-key-find-by-email           t]
      ["Fetch by guess"                 tinypgp-key-find-by-guess           t]
      "----"
      ["Insert with batch to pubring"   tinypgp-key-add-region-batch        t]
      ["Insert with ask to pubring"     tinypgp-key-add-region-interactive  t]
      ["Extract to point"               tinypgp-key-extract-to-point        t]

;;;#todo menu
;;;      ["Generate new key"            tinypgp-key-generate                t]
      ["Remove from keyring"            tinypgp-key-delete-region           t]
      "----"
      ["Info insert matches"            tinypgp-key-info-insert             t]
      ["Info show matches"              tinypgp-key-info-at-point-show      t])

     (list
      "Pubring and user control"
      ["Pubring show"                   tinypgp-pubring-display      t]
      ["Pubring change"                 tinypgp-pubring-set-current  t]
      "----"
      ["User show"                      tinypgp-user-display         t]
      ["User change"                    tinypgp-user-set-current     t])

     (list
      "Modes and toggles"
      ["Flip x-pgp header/regular pgp"  tinypgp-xpgp-header-toggle    t]
      ["Flip Signature hide/show"       tinypgp-hide-show-toggle      t]
      "----"
      ["Mode Auto action on/off"        tinypgp-auto-action-toggle          t]
      ["Mode Auto signing on/off"       tinypgp-sign-mail-auto-mode         t]
      ["Mode Header sign on/off"        tinypgp-header-sign-mode-toggle     t]
      ["Mode x-pgp on/off"              tinypgp-xpgp-header-mode-toggle     t]
      "----"
      ["Mode Secring crypt on/off"      tinypgp-secring-crypt-mode-toggle   t]
      ["Mode Email substitution on/off" tinypgp-email-substitution-toggle   t])

;;;     "----"

     (list
      "Extra commands"
      ["Info Show Encrypt keys used"        tinypgp-encrypt-info          t]
      ["Info Show auto action entry"        tinypgp-auto-action-verbose   t]
      ["Info Show email conversion"  tinypgp-key-id-conversion-check-verbose t]
      ["Info Show header signing fields"    tinypgp-header-list-show      t]
      ["Info Show last finger error"     tinypgp-show-last-finger-error   t]
      "----"
      ["Info Describe mode"                 tinypgp-mode-describe         t]
      ["Info View pgp register"             tinypgp-view-register         t]
      ["Info Sudy PGP stream forward." tinypgp-pgp-stream-forward-study   t]
      "----"
      ["Wash Anonymize headers"             tinypgp-header-move-to-body   t]
      ["Wash expire pass phrases/files"     tinypgp-password-expire-now   t]
      ["Wash expire secring password"
       tinypgp-secring-crypt-expire-password                             t]
      ["Wash loose signing information"     tinypgp-sign-loose-info       t]
      ["Wash wipe passwords from buffer"    tinypgp-password-wipe-buffer  t]
      ["Wash delete running PGP processes"  tinypgp-delete-processes      t]
      "----"
      ["Send email: .plan has no PGP key"   tinypgp-sendmail-key-not-in-plan t]
      ["Send email: keyserver cmd"          tinypgp-keysrv-send-email-command t])
     "----"

     (list
      "Remailer service"
      ["Post as Anon "                  tinypgp-r-post
       (get 'tinypgp-:mode-menu 'remail)]
      ["Encrypt-Remail message once"    tinypgp-r-chain-1
       (get 'tinypgp-:mode-menu 'remail)]
      ["Encrypt-Remail message using chain"  tinypgp-r-chain
       (get 'tinypgp-:mode-menu 'remail)]
      "----"
      ["Initialize remailer support"    tinypgp-r-init              t]
      ["Update remailer list"           tinypgp-r-update-remailer-list t]
      (list
       "Reply block"
       ["Make basic reply block"         tinypgp-r-reply-block-basic
        (get 'tinypgp-:mode-menu 'remail)]
       ["Construct remailer reply block" tinypgp-r-reply-block-insert
        (get 'tinypgp-:mode-menu 'remail)]
       ["Test defined reply blocks"      tinypgp-r-reply-block-test
        (get 'tinypgp-:mode-menu 'remail)])

     (list
      "Newnym service"
      ["Show or get account help"       tinypgp-newnym-help
       (get 'tinypgp-:mode-menu 'newnym)]
      ["Default account in use/not in use" tinypgp-newnym-default-toggle
       tinypgp-:r-newnym-default-account-table]
      ["Default account select"         tinypgp-newnym-default-set
       tinypgp-:r-newnym-default-account-table]
      ["Post as Anon "                  tinypgp-newnym-post
       (get 'tinypgp-:mode-menu 'newnym)]
      "----"
      (list
       "Requests"
       ["acksend"               tinypgp-newnym-req-acksend
        (get 'tinypgp-:mode-menu 'newnym)]
       ["cryptrecv"             tinypgp-newnym-req-cryptrecv
        (get 'tinypgp-:mode-menu 'newnym)]
       ["disable"               tinypgp-newnym-req-disable
        (get 'tinypgp-:mode-menu 'newnym)]
       ["fingerkey"             tinypgp-newnym-req-fingerkey
        (get 'tinypgp-:mode-menu 'newnym)]
       ["fixedsize"             tinypgp-newnym-req-fixedsize
        (get 'tinypgp-:mode-menu 'newnym)]
       ["sigsend"               tinypgp-newnym-req-sigsend
        (get 'tinypgp-:mode-menu 'newnym)])
      (list
       "Configuration and misc"
       ["Account expiry status"  tinypgp-newnym-account-expiry-warnings t]
       ["Configuration template" tinypgp-newnym-config-sendmail-template
        (get 'tinypgp-:mode-menu 'newnym)]
       ["Create new account"            tinypgp-newnym-create
        (get 'tinypgp-:mode-menu 'newnym)]
       ["Delete account"                tinypgp-newnym-delete
        (get 'tinypgp-:mode-menu 'newnym)]
       "----"
       ["Get used account list"         tinypgp-newnym-get-used-list
        (get 'tinypgp-:mode-menu 'newnym)]
       ["Get server's PGP key"          tinypgp-newnym-get-pgp-key
        (get 'tinypgp-:mode-menu 'newnym)])

     (list
      "Nymserver service"
      ["Post as anon"                   tinypgp-nymserver-post
       (get 'tinypgp-:mode-menu 'nymserver)]
      ["Finger, account status"         tinypgp-nymserver-finger
));;; Yes; you can finger an anon address ok: anNNN@anon-nymserver.com
;;;     (get 'tinypgp-:mode-menu 'nymserver)
       t]
      ["Ping, your account status"      tinypgp-nymserver-ping
       (get 'tinypgp-:mode-menu 'nymserver)]
      ["Help, read file"                tinypgp-nymserver-help
       (get 'tinypgp-:mode-menu 'nymserver)]
      "----"
      (list
       "Change account properties"
       ["Change account alias"          tinypgp-nymserver-newalias
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["Change nickname"               tinypgp-nymserver-nickname
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["Change .plan"                  tinypgp-nymserver-newplan
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["Change .signature"             tinypgp-nymserver-newsig
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["Change to new address"         tinypgp-nymserver-newaddress
        (get 'tinypgp-:mode-menu 'nymserver)])

      (list
       "Flags and pgp key"
       ["flag, paranoid"                tinypgp-nymserver-paranoid
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["flag, vacation"                tinypgp-nymserver-vacation
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["flag, no archive"              tinypgp-nymserver-noarchive
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["flag, anNNN/naNNN"             tinypgp-nymserver-setnon
        (get 'tinypgp-:mode-menu 'nymserver)]
       "----"
       ["PGP key upload "               tinypgp-nymserver-pgp-upload
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["PGP flag, encrypt"             tinypgp-nymserver-pgp-encrypt
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["PGP flag, sign"                tinypgp-nymserver-pgp-sign
        (get 'tinypgp-:mode-menu 'nymserver)]
       ["PGP flag, mixmaster"           tinypgp-nymserver-pgp-sendmix
        (get 'tinypgp-:mode-menu 'nymserver)])

      (list
       "Create"
       ["Account create"                tinypgp-nymserver-create            t]
       ["Account remove"                tinypgp-nymserver-remove
        (get 'tinypgp-:mode-menu 'nymserver)]))
     "----"

     (list
      "Cache service"
      ["Remove last entry"              tinypgp-key-cache-remove-entry-last  t]
      ["Display"                        tinypgp-key-cache-display           t])

     (list
      "Report and backend service"
      ["Select PGP backend"             tinypgp-backend-select    t]
      ["Select PGP backend 2.6.x"       tinypgp-backend-select-pgp2   t]
      ["Select PGP backend 5.x"         tinypgp-backend-select-pgp5   t]
      ["Show TinyPgp version"           tinypgp-version-message     t]
      ["Show TinyPgp initial message"   tinypgp-initial-message     t]
      ["Submit bug report"              tinypgp-submit-bug-report     t]
      "----"
      ["Debug on/off"                   tinypgp-debug-toggle          t]
      ["Debug buffer clear"             tinypgp-debug-buffer-clear    t]
      "----"
      ["Display comint"                 tinypgp-show-buffer-comint    t]
      ["Display debug"                  tinypgp-show-buffer-debug           t]
      ["Display finger"                 tinypgp-show-buffer-finger    t]
      ["Display http"                   tinypgp-show-buffer-http            t]
      ["Display shell"                  tinypgp-show-buffer-shell           t]
      ["Display tmp"                    tinypgp-show-buffer-tmp     t]))))

;;; I don't know if average user realizes what this command does...
;;;     ["Generate randseed.bin"  t]

;;}}}
;;{{{ menu: echo, newnym

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-newnym (map n)
  ;;  Seldom used command in big letter to prevent accidents.
  ;;
  (define-key map (concat n "?") 'tinypgp-newnym-help)
  (define-key map (concat n "a") 'tinypgp-newnym-req-acksend)
  (define-key map (concat n "b") 'tinypgp-newnym-req-nobcc)
  (define-key map (concat n "C") 'tinypgp-newnym-create)

  (define-key map (concat n "c")
    'tinypgp-newnym-config-sendmail-template)

  (define-key map (concat n "D") 'tinypgp-newnym-delete)
  (define-key map (concat n "e") 'tinypgp-newnym-req-disable)
  (define-key map (concat n "f") 'tinypgp-newnym-req-fingerkey)
  (define-key map (concat n "K") 'tinypgp-newnym-get-pgp-key)
  (define-key map (concat n "u") 'tinypgp-newnym-get-used-list)
  (define-key map (concat n "p") 'tinypgp-newnym-post)
  (define-key map (concat n "r") 'tinypgp-newnym-req-cryptrecv)
  (define-key map (concat n "s") 'tinypgp-newnym-req-sigsend)
  (define-key map (concat n "\t") 'tinypgp-newnym-default-set)
  (define-key map (concat n "t") 'tinypgp-newnym-default-toggle)
  (define-key map (concat n "x") 'tinypgp-newnym-account-expiry-warnings)
  (define-key map (concat n "z") 'tinypgp-newnym-req-fixedsize))

;;; ----------------------------------------------------------------------
;;;
(defcustom tinypgp-:newnym-echo-menu-use-p t
  "*Should the 'newnym' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp-nymserver)

;; Change this mane in the load-hook is need to.
;;
(defconst tinypgp-:newnym-echo-menu
  '(
    (let* ((srv (get 'tinypgp-:r-newnym-default-account-table 'default-server))
           (pfx (if current-prefix-arg "+" ""))
           (def (format "%s[%s]" pfx (or srv "Newnym") )))
      (tinypgp-backend-set-for-action 'newnym)
      (format
       "\
%s p)ost c)fg t/ab)oggle h)lp req:a)ck b)cc e)nab f)ing s)ig si(z)e [utx CDK]"
       def))
    ((?a  . ( (call-interactively 'tinypgp-newnym-req-acksend)))
     (?C  . ( (call-interactively 'tinypgp-newnym-create)))
     (?b  . ( (call-interactively 'tinypgp-newnym-req-nobcc)))
     (?c  . ( (call-interactively 'tinypgp-newnym-config-sendmail-template)))
     (?D  . ( (call-interactively 'tinypgp-newnym-delete)))
     (?e  . ( (call-interactively 'tinypgp-newnym-req-disable)))
     (?f  . ( (call-interactively 'tinypgp-newnym-req-fingerkey)))
     (?h  . ( (tinypgp-newnym-help-verbose current-prefix-arg)))
     (?K  . ( (call-interactively 'tinypgp-newnym-get-pgp-key)))
     (?u  . ( (call-interactively 'tinypgp-newnym-get-used-list)))
     (?p  . ( (call-interactively 'tinypgp-newnym-post)))
     (?r  . ( (call-interactively 'tinypgp-newnym-req-cryptrecv)))
     (?s  . ( (call-interactively 'tinypgp-newnym-req-sigsend)))
     (?t  . (t (call-interactively 'tinypgp-newnym-default-toggle)))
     (?\t . ( (call-interactively 'tinypgp-newnym-default-set)))
     (?x  . ( (tinypgp-newnym-account-expiry-warnings)))
     (?z  . ( (call-interactively 'tinypgp-newnym-req-fixedsize)))))
  "Nym account menu.
Esc or q to exit menu without choosing. Less used commands are in uppercase.

Basic Nym commands

  h   = Show help file (prefix arg orders help file by mail)
  p   = convert current message to anon (p)ost

Nym account requests

  All these commands send the minus(-) request and request action is
  explained to the right. Supply prefix argument if you want to send plus(+)
  request.

  a   = (a)cksend    disable automatic acknowledgement
  b   = no(b)cc      receive bcc carbon copies. Needed if you
                     subscribe to mailing lists.
  r   = c(r)yptrecv  disable encryption to you.
  e   = disable      re-(e)nable account
  f   = (f)ingerkey  disallow people to get your PGP key.
  z   = fixedsi(z)e  do not padd messages to 10K
  s   = (s)igsend    disable automatic pgp signing

Nym account management

  t   = (t)oggle using default account.
  tab = set default server and account
  c   = prepare (c)onfigure template and enter 'Nym' mode.
        You can manage you account in details. See tab key in this mode.
  C   = (C)reate account
  D   = (D)elete account

Other

  x   = Display count of days to account e(x)piration.
  u   = Get account list ie. (u)sed nym names
  K   = Get server's PGP (k)ey.")

;;}}}
;;{{{ menu: echo, nymserver

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-nymserver (map y)
  ;; Normal keybindings then. No menu in echo area used.

  (define-key map (concat y "p") 'tinypgp-nymserver-post)
  (define-key map (concat y "f") 'tinypgp-nymserver-finger)
  (define-key map (concat y "i") 'tinypgp-nymserver-ping)

  (define-key map (concat y "a") 'tinypgp-nymserver-newalias)
  (define-key map (concat y "n") 'tinypgp-nymserver-nickname)
  (define-key map (concat y "w") 'tinypgp-nymserver-newpassword)

  (define-key map (concat y "o") 'tinypgp-nymserver-paranoid)
  (define-key map (concat y "v") 'tinypgp-nymserver-vacation)
  (define-key map (concat y "d") 'tinypgp-nymserver-newaddress)
  (define-key map (concat y "r") 'tinypgp-nymserver-noarchive)
  (define-key map (concat y "l") 'tinypgp-nymserver-newplan)
  (define-key map (concat y "g") 'tinypgp-nymserver-newsig)
  (define-key map (concat y "t") 'tinypgp-nymserver-setnon)

  (define-key map (concat y "k") 'tinypgp-nymserver-pgp-upload)
  (define-key map (concat y "e") 'tinypgp-nymserver-pgp-encrypt)
  (define-key map (concat y "s") 'tinypgp-nymserver-pgp-sign)
  (define-key map (concat y "x") 'tinypgp-nymserver-pgp-sendmix)

  (define-key map (concat y "C") 'tinypgp-nymserver-create)
  (define-key map (concat y "D") 'tinypgp-nymserver-remove)
  (define-key map (concat y "A") 'tinypgp-nymserver-abuse)

  (define-key map (concat y "h") 'tinypgp-nymserver-help))

(defcustom tinypgp-:nymserver-echo-menu-use-p t
  "*Should the 'nymserver' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp-nymserver)

;; Change this mane in the load-hook is need to.
;;
(defconst tinypgp-:nymserver-echo-menu
  '(
    (progn
      (tinypgp-backend-set-for-action 'nymserv)
      "Nymserv p)ost f)ing p(i)ng n)ick si(g) p(l)an PGP.kesx req.drtovwa [hACR]")
    ((?p  . ( (call-interactively 'tinypgp-nymserver-post)))
     (?f  . ( (call-interactively 'tinypgp-nymserver-finger)))
     (?i  . ( (call-interactively 'tinypgp-nymserver-ping)))

     (?n  . ( (call-interactively 'tinypgp-nymserver-nickname)))
     (?v  . ( (call-interactively 'tinypgp-nymserver-vacation)))
     (?a  . ( (call-interactively 'tinypgp-nymserver-newalias)))

     (?l  . ( (call-interactively 'tinypgp-nymserver-newplan)))
     (?g  . ( (call-interactively 'tinypgp-nymserver-newsig)))
     (?r  . ( (call-interactively 'tinypgp-nymserver-noarchive)))

     (?y  . ( (call-interactively 'tinypgp-nymserver-setnon)))
     (?o  . ( (call-interactively 'tinypgp-nymserver-paranoid)))
     (?d  . ( (call-interactively 'tinypgp-nymserver-newaddress)))
     (?w  . ( (call-interactively 'tinypgp-nymserver-newpassword)))

     (?k  . ( (call-interactively 'tinypgp-nymserver-pgp-upload)))
     (?e  . ( (call-interactively 'tinypgp-nymserver-pgp-encrypt)))
     (?s  . ( (call-interactively 'tinypgp-nymserver-pgp-sign)))
     (?x  . ( (call-interactively 'tinypgp-nymserver-pgp-sendmix)))

     (?A  . ( (call-interactively 'tinypgp-nymserver-abuse)))
     (?C  . ( (call-interactively 'tinypgp-nymserver-create)))
     (?D  . ( (call-interactively 'tinypgp-nymserver-remove)))
     (?h  . ( (tinypgp-nymserver-help-verbose current-prefix-arg)))))
  "anon.nymserver.com menu.
Esc or q to exit menu without choosing.

Basic commands

  p   = convert current message to anon (p)ost
  f   = (f)inger account for configuration information.

Common commands

  n   = (n)ickname change request
  g   = upload new .(s)ignature file
  l   = upload new .p(l)an file

PGP related requests

  k   = upload PGP (k)ey to your account
  e   = (e)ncrypt request
  s   = (s)igning request
  x   = mi(x)master request

Requests

  d   = newa(dd)ress request
  r   = noa(r)chive request
  t   = se(t)non request
  o   = paran(o)id request
  v   = (v)acation request
  w   = ne(w)password request
  a   = new(a)lias request. This changes your anNNN to vanity alias.

Other

  h   = show (h)elp file, With Prefix arg send help request email.
  A   = Send (a)buse mail
  C   = (C)reate new account. This command can be sent only once.
  D   = (D)elete account. This is opposite of create.")

;;}}}
;;{{{ menu: echo, remail

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-remail (map p)
  (define-key map (concat p "b")  'tinypgp-r-reply-block-basic)
  (define-key map (concat p "r")  'tinypgp-r-reply-block-insert)
  (define-key map (concat p "i")  'tinypgp-r-init)
  (define-key map (concat p "u")  'tinypgp-r-update-remailer-list)
  (define-key map (concat p "p")  'tinypgp-r-post)
  (define-key map (concat p "t")  'tinypgp-r-reply-block-test)
  (define-key map (concat p "C")  'tinypgp-r-chain-1)
  (define-key map (concat p "c")  'tinypgp-r-chain))

(defcustom tinypgp-:remail-echo-menu-use-p t
  "*Should the 'remail' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:remail-echo-menu
  '(
    (progn
      (tinypgp-backend-set-for-action 'remail)
      "remail: p)ost cC)hain b)asic-rb t)est-rb r)b-insert   u)pdate i)nit")
    ((?b . ( (call-interactively 'tinypgp-r-reply-block-basic)))
     (?r . ( (call-interactively 'tinypgp-r-reply-block-insert)))
     (?i . ( (call-interactively 'tinypgp-r-init)))
     (?u . ( (call-interactively 'tinypgp-r-update-remailer-list)))
     (?p . ( (call-interactively 'tinypgp-r-post)))
     (?t . ( (call-interactively 'tinypgp-r-reply-block-test)))
     (?C . ( (call-interactively 'tinypgp-r-chain-1)))
     (?c . ( (call-interactively 'tinypgp-r-chain)))))
  "Remail management menu

p  convert message to remailer post
c  Chain message using predefined paths. Use (p) first
C  Chain once manually. Use (p) first

b  construct basic reply block
r  Insert reply block

i  Initialise remailer support
u  update remailer list
t  test reply blocks")

;;}}}

;;{{{ menu: echo, buffer

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-buffer (map p)
  "Define buffer handling keys. Use P prefix key and assign to MAP."
  ;;  Buffer management in prefix "b"
  ;;
  (define-key map (concat p "c")  'tinypgp-show-buffer-comint)
  (define-key map (concat p "d")  'tinypgp-show-buffer-debug)
  (define-key map (concat p "f")  'tinypgp-show-buffer-finger)
  (define-key map (concat p "h")  'tinypgp-show-buffer-http)
  (define-key map (concat p "s")  'tinypgp-show-buffer-shell)
  (define-key map (concat p "t")  'tinypgp-show-buffer-tmp)

  (define-key map (concat p "\b")   'tinypgp-debug-buffer-clear)
  (define-key map (concat p "\177") 'tinypgp-debug-buffer-clear)
  (define-key map (concat p "\C-m") 'tinypgp-show-buffer-debug))

;;; ----------------------------------------------------------------------
;;;
(defcustom tinypgp-:show-buffer-echo-menu-use-p t
  "*Should the 'show-buffer' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:show-buffer-echo-menu
  '(
    "buffer: c)ache d)ebug,RET f)ing h)ttp s)hell t)emp DEL)debug clear "
    ((?c     . ( (tinypgp-key-cache-display)))
     (?d     . ( (tinypgp-show-buffer-debug)))
     (?f     . ( (tinypgp-show-buffer-finger)))
     (?h     . ( (tinypgp-show-buffer-http)))
     (?s     . ( (tinypgp-show-buffer-shell)))
     (?t     . ( (tinypgp-show-buffer-tmp)))
     (?\b    . ( (tinypgp-debug-buffer-clear)))
     (?\177  . ( (tinypgp-debug-buffer-clear)))
     (?\C-m  . ( (tinypgp-show-buffer-debug)))))
  "buffer show menu

c   Show key cache buffer
d   Show debug buffer
f   Show finger buffer
h   Show http buffer
s   Show shell buffer
t   Show temp buffer

RET Show debug buffer
DEL Clear debug buffer")

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-user (map p)
  "Define user keys. Use P prefix key and assign to MAP."
  (define-key map (concat p "s")  'tinypgp-user-display)
  (define-key map (concat p "\t") 'tinypgp-user-set-current))

(defcustom tinypgp-:user-echo-menu-use-p t
  "*Should the 'user' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:user-echo-menu
  '(
    "user: s)how tab)change"
    ((?s  . ( (call-interactively 'tinypgp-user-display)))
     (?\t . ( (call-interactively 'tinypgp-user-set-current)))))
  "User handling menu

s   Show current pgp user
tab Change current pgp user")

;;}}}
;;{{{ menu: echo, key

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-pubring (map p)
  (define-key map (concat p "s")  'tinypgp-pubring-display)
  ;;  This is little faster key
  (define-key map (concat p "\t") 'tinypgp-pubring-set-current))

(defcustom tinypgp-:pubring-echo-menu-use-p t
  "*Should the 'pubring' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:pubring-echo-menu
  '(
    "pubring: s)how tab)change"
    ((?s  . ( (call-interactively 'tinypgp-pubring-display)))
     (?\t . ( (call-interactively 'tinypgp-pubring-set-current)))))
  "User handling menu

s   Show current pubring in use
tab Change current pubring")

;;}}}
;;{{{ menu: echo, key

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-cache (map p)
  (define-key map (concat p "r")
    'tinypgp-key-cache-remove-entry-last)
  (define-key map (concat p "s") 'tinypgp-key-cache-display))

(defcustom tinypgp-:cache-echo-menu-use-p t
  "*Should the 'pubring' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:cache-echo-menu
  '(
    "pubring: s)how tab)change"
    ((?r . ( (call-interactively 'tinypgp-key-cache-remove-entry-last)))
     (?s . ( (call-interactively 'tinypgp-key-cache-display)))))
  "Cache menu

r   remove entry from cache.
s   Show cache")

;;}}}

;;{{{ menu: echo, debug

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-debug (map p)
  (define-key map (concat p "d")  'tinypgp-debug-toggle)
  (define-key map (concat p "c")  'tinypgp-debug-buffer-clear)
  (define-key map (concat p "s")  'tinypgp-submit-bug-report)
  (define-key map (concat p "v")  'tinypgp-version-message)
  (define-key map (concat p "i")  'tinypgp-initial-message)
  (define-key map (concat p "\e")  'tinypgp-submit-bug-report))

(defcustom tinypgp-:debug-echo-menu-use-p t
  "*Should the 'debug' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:debug-echo-menu
  '(
    "debug: d)toggle c)lear s)submit report   v)ersion msg i)nit msg"
    ((?d . ( (call-interactively 'tinypgp-debug-toggle)))
     (?c . ( (call-interactively 'tinypgp-debug-buffer-clear)))
     (?s . ( (call-interactively 'tinypgp-submit-bug-report)))
     (?v . ( (call-interactively 'tinypgp-version-message)))
     (?i . ( (call-interactively 'tinypgp-initial-message)))))
  "Debug menu
d  Toggle debug
c  Clear debug buffer
s  Submit bug report

v  Show version message
i  Show initial startup message")

;;}}}
;;{{{ menu: echo, region

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-region (map p)
  (define-key map (concat p "s") 'tinypgp-sign-region)
  (define-key map (concat p "S") 'tinypgp-sign-region-base64)
  (define-key map (concat p "D") 'tinypgp-sign-region-detached)
  (define-key map (concat p "e") 'tinypgp-encrypt-region)
  (define-key map (concat p "t") 'tinypgp-encrypt-region-sign)
  (define-key map (concat p "d") 'tinypgp-decrypt-region)
  (define-key map (concat p "v") 'tinypgp-verify-region)
  (define-key map (concat p "c") 'tinypgp-crypt-region))

(defcustom tinypgp-:region-echo-menu-use-p t
  "*Should the 'region' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:region-echo-menu
  '(
    "region: sSD)sign,base64,detach e)ncrypt t)1pass d)ecrypt v)erify c)rypt "
    ((?s  . ( (call-interactively 'tinypgp-sign-region)))
     (?S  . ( (call-interactively 'tinypgp-sign-region-base64)))
     (?D  . ( (call-interactively 'tinypgp-sign-region-detached)))
     (?e  . ( (call-interactively 'tinypgp-encrypt-region)))
     (?t  . ( (call-interactively 'tinypgp-encrypt-region-sign)))
     (?d  . ( (call-interactively 'tinypgp-decrypt-region)))
     (?v  . ( (call-interactively 'tinypgp-verify-region)))
     (?c  . ( (call-interactively 'tinypgp-crypt-region)))))
  "Region menu

s   Sign
S   Sign with base64 armor
D   Detach sign
e   encrypt
t   encrypt and sign on 1pass
d   decrypt
v   verify
c   crypt
")

;;}}}
;;{{{ menu: echo, keyring

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-key (map p)
  ;; #todo key generate
  ;; A  (define-key map (concat p "g") 'tinypgp-key-generate)
  ;;  - "ki" is closer to keyboard than default pgp "kv".

  (define-key map (concat p "i") 'tinypgp-key-info-at-point-show)
  (define-key map (concat p "I") 'tinypgp-key-info-insert)
  (define-key map (concat p "v") 'tinypgp-key-info-insert)

  (define-key map (concat p "a") 'tinypgp-key-add-region-batch)
  (define-key map (concat p "A")
    'tinypgp-key-add-region-interactive)
  (define-key map (concat p "x") 'tinypgp-key-extract-to-point)
  (define-key map (concat p "r") 'tinypgp-key-delete-region))

(defcustom tinypgp-:key-echo-menu-use-p t
  "*Should the 'key' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:key-echo-menu
  '(
    "key: i)nfo show vI)nsert a)dd batch A)add interactive x)tract r)emove"
    ((?i  . ( (call-interactively 'tinypgp-key-info-at-point-show)))
     (?I  . ( (call-interactively 'tinypgp-key-info-insert)))
     (?v  . ( (call-interactively 'tinypgp-key-info-insert)))
     (?a  . ( (call-interactively 'tinypgp-key-add-region-batch)))
     (?A  . ( (call-interactively 'tinypgp-key-add-region-interactive)))
     (?x  . ( (call-interactively 'tinypgp-key-extract-to-point)))
     (?r  . ( (call-interactively 'tinypgp-key-delete-region)))))
  "Key management menu

i  Show keys matching string at point
I  Insert key info mathing string to point
v  ...same... (synonym)
a  add keys in region to pubring
A  add keys in region to pubring (interactive)
x  Extract key from keyring to point
r  removed selected keys in region from keyring
")

;;}}}
;;{{{ menu: echo, modes

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-mode (map p)
  (define-key map (concat p "!") 'tinypgp-auto-action-toggle)
  (define-key map (concat p "c") 'tinypgp-secring-crypt-mode-toggle)

  (define-key map (concat p "e")
    'tinypgp-email-substitution-toggle)

  (define-key map (concat p "h") 'tinypgp-xpgp-header-mode-toggle)
  (define-key map (concat p "H")
    'tinypgp-header-sign-mode-toggle)

  (define-key map (concat p "s") 'tinypgp-sign-mail-auto-mode))

(defcustom tinypgp-:mode-echo-menu-use-p t
  "*Should the 'mode' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:mode-echo-menu
  '(
    "mode: !)action c)rypt secring e)mail h)x-pgp H)eader sign s)ign"
    ((?! . ( (call-interactively 'tinypgp-auto-action-toggle)))
     (?c . ( (call-interactively 'tinypgp-secring-crypt-mode-toggle)))
     (?e . ( (call-interactively 'tinypgp-email-substitution-toggle)))
     (?h . ( (call-interactively 'tinypgp-xpgp-header-mode-toggle)))
     (?H . ( (call-interactively 'tinypgp-header-sign-mode-toggle)))
     (?s . ( (call-interactively 'tinypgp-sign-mail-auto-mode)))))
  "Mode handling menu

!  Toggle auto action: enable, disable
c  Toggle secring crypt mode
e  Toggle email subtitution mode
f  Toggle fcc encrypt mode
h  Toggle header based x-pgp signing mode
H  Toggle including part of the headers for signing
s  Toggle auto signing mode of outgoing mail")

;;}}}
;;{{{ menu: echo, key

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys-extra (map p)

  (define-key map (concat p "a") 'tinypgp-auto-action-verbose)

  (define-key map (concat p "b") 'tinypgp-backend-select)
  (define-key map (concat p "B") 'tinypgp-secring-backup)

  (define-key map (concat p "D") 'tinypgp-delete-processes)

  (define-key map (concat p "E")
    'tinypgp-key-id-conversion-check-verbose)

  (define-key map (concat p "e") 'tinypgp-encrypt-info)

  (define-key map (concat p "h") 'tinypgp-header-list-show)
  (define-key map (concat p "i")
    'tinypgp-pgp-stream-forward-study)

  (define-key map (concat p "f") 'tinypgp-show-last-finger-error)

  (define-key map (concat p "k")
    'tinypgp-keysrv-send-email-command)

  (define-key map (concat p "l") 'tinypgp-sign-loose-info)

  (define-key map (concat p "p")
    'tinypgp-sendmail-key-not-in-plan)

  (define-key map (concat p "w") 'tinypgp-password-wipe-buffer)

  (define-key map (concat p "x") 'tinypgp-password-expire-now)
  (define-key map (concat p "X") 'tinypgp-secring-crypt-expire-password))

(defcustom tinypgp-:extra-echo-menu-use-p t
  "*Should the 'extra' commands be accessible from echo-area menu?.
You can set this only once; otherwise you have to reload package."
  :type  'boolean
  :group 'tinypgp)

(defconst tinypgp-:extra-echo-menu
  (list
   "\
extra: aeh)info f)ing iE)pgp kp)email l)oose b)backend B)up wDxX)pire >dC "
   (list
    '(?a . ( (call-interactively 'tinypgp-auto-action-verbose)))
    '(?b . ( (call-interactively 'tinypgp-backend-select)))
    '(?B . ( (call-interactively 'tinypgp-secring-backup)))
    '(?D . ( (call-interactively 'tinypgp-delete-processes)))
    '(?e . ( (call-interactively 'tinypgp-key-id-conversion-check-verbose)))
    '(?E . ( (call-interactively 'tinypgp-encrypt-info)))
    '(?h . ( (call-interactively 'tinypgp-header-list-show)))
    '(?i . ( (call-interactively 'tinypgp-pgp-stream-forward-study)))
    '(?f . ( (call-interactively 'tinypgp-show-last-finger-error)))
    '(?k . ( (call-interactively 'tinypgp-keysrv-send-email-command)))
    '(?l . ( (call-interactively 'tinypgp-sign-loose-info)))
    '(?p . ( (call-interactively 'tinypgp-sendmail-key-not-in-plan)))
    '(?w . ( (call-interactively 'tinypgp-password-wipe-buffer)))
    '(?x . ( (call-interactively 'tinypgp-password-expire-now)))
    '(?X . ( (call-interactively 'tinypgp-secring-crypt-expire-password)))

    (cons ?d 'tinypgp-:debug-echo-menu)
    (cons ?C 'tinypgp-:cache-echo-menu)))
  "Extra menu

Information

    a  Show auto action that would trigger this mail
    e  Show what email conversion would apply to To address
    h  Show what headers would be signed
    f  Show last finger error in echo area

  Pgp block

    E  Study encrypted message and show whom it's encrypted to
    i  Study pgp stream forward and show info (type pgp version etc.)

Email

    k  Send command to keyserver
    p  Send notice that user's key was not in .plan when fingered.

Miscellaneous

    l  Loose signing information
    b  Select backend> pgp 2.6.x or pgp 5.x
    B  Backup secring in encrypted format

Wipe

    d  Delete all running PGP processes. Eg. Pgp 5.x may be hung in your
       emacs. Use this command to get rip of those zombies. See process
       list with command \\[list-processes]
    w  wipe passwords from buffer
    x  Expire pass phrases
    X  Expire encrypted secring password.")

;;}}}

;;{{{ menu: define keys

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-define-keys ()
  "Define keys."
  (let* ((map  tinypgp-:mode-map)
         (p    tinypgp-:mode-prefix-key)
         (r    tinypgp-:mode-prefix-key-remailer)
         (y    tinypgp-:mode-prefix-key-nymserver)
         (n    tinypgp-:mode-prefix-key-newnym))

    (if tinypgp-:region-echo-menu-use-p
        (define-key map (concat p "r")
          (ti::definteractive (ti::menu-menu 'tinypgp-:region-echo-menu arg)))
      (tinypgp-mode-define-keys-region map (concat p "r")))

    ;; ................................................. user, pubring ...

    (if tinypgp-:user-echo-menu-use-p
        (define-key map (concat p "u")
          (ti::definteractive (ti::menu-menu 'tinypgp-:user-echo-menu arg)))
      (tinypgp-mode-define-keys-user map (concat p "u")))

    (if tinypgp-:pubring-echo-menu-use-p
        (define-key map (concat p "p")
          (ti::definteractive (ti::menu-menu 'tinypgp-:pubring-echo-menu arg)))
      (tinypgp-mode-define-keys-pubring map (concat p "p")))

    ;; ....................................................... keyring ...

    (if tinypgp-:key-echo-menu-use-p
        (define-key map (concat p "k")
          (ti::definteractive (ti::menu-menu 'tinypgp-:key-echo-menu arg)))
      (tinypgp-mode-define-keys-key map (concat p "k")))

    ;; ........................................................ buffer ...

    (if tinypgp-:show-buffer-echo-menu-use-p
        (define-key map (concat p "b")
          (ti::definteractive (ti::menu-menu 'tinypgp-:show-buffer-echo-menu arg)))
      (tinypgp-mode-define-keys-buffer map (concat p "b")))

    ;; ......................................................... extra ...

    (if tinypgp-:extra-echo-menu-use-p
        (define-key map (concat p "x")
          (ti::definteractive (ti::menu-menu 'tinypgp-:extra-echo-menu arg)))
      (tinypgp-mode-define-keys-extra map (concat p "x")))

    (unless tinypgp-:debug-echo-menu-use-p
      (tinypgp-mode-define-keys-debug map (concat p "xd")))

    (unless tinypgp-:debug-echo-menu-use-p
      (tinypgp-mode-define-keys-cache map (concat p "xC")))

    ;; .......................................................... mode ...

    (if tinypgp-:mode-echo-menu-use-p
        (define-key map (concat p "m")
          (ti::definteractive (ti::menu-menu 'tinypgp-:mode-echo-menu arg)))
      (tinypgp-mode-define-keys-mode map (concat p "m")))

    ;; ...................................................... remailer ...

    (if tinypgp-:remail-echo-menu-use-p
        (define-key map r
          (ti::definteractive (ti::menu-menu 'tinypgp-:remail-echo-menu arg)))
      (tinypgp-mode-define-keys-remail map r))

    ;; ..................................................... nymserver ...

    (if tinypgp-:nymserver-echo-menu-use-p
        (define-key map y
          (ti::definteractive
           (ti::menu-menu 'tinypgp-:nymserver-echo-menu arg)))
      (tinypgp-mode-define-keys-nymserver map y))

    ;; ........................................................ newnym ...

    (if tinypgp-:newnym-echo-menu-use-p
        (define-key map n
          (ti::definteractive (ti::menu-menu 'tinypgp-:newnym-echo-menu arg)))
      (tinypgp-mode-define-keys-nymserver map n))

    ;; ....................................................... regular ...

    (define-key map
      (concat p (ti::string-right p 1))    'tinypgp-next-action-mail)

    (define-key map (concat p "?")  'tinypgp-mode-describe)

    (define-key map (concat p "a")  'tinypgp-header-move-to-body)
    (define-key map (concat p "s")  'tinypgp-sign-mail)
    (define-key map (concat p "S")  'tinypgp-sign-mail-base64)
    (define-key map (concat p "D")  'tinypgp-sign-mail-detached)

    (define-key map (concat p "e")  'tinypgp-encrypt-mail)
    (define-key map (concat p "t")  'tinypgp-encrypt-mail-sign)

    ;;  There no particular reason why "q" for mime.
    ;;  I chose it because, Q char is obscure enough to
    ;;  remind that in 1998-03 the PGP/MIME is still new.

    (define-key map (concat p "q")  'tinypgp-sign-mail-mime)
    (define-key map (concat p "Q")  'tinypgp-encrypt-mail-mime)

    (define-key map (concat p "d")  'tinypgp-decrypt-mail)
    (define-key map (concat p "v")  'tinypgp-verify-mail)
    (define-key map (concat p "V")
      'tinypgp-verify-detached-signature)

    (define-key map (concat p "c")  'tinypgp-crypt-mail)
    (define-key map (concat p "i")  'tinypgp-sign-base64-insert-file)

    (define-key map (concat p "h")  'tinypgp-xpgp-header-toggle)
    (define-key map (concat p "g")  'tinypgp-hide-show-toggle)

    (define-key map (concat p "R")  'tinypgp-view-register)
    (define-key map (concat p "F")  'tinypgp-key-find-by-finger)

    (define-key map (concat p "G")
      'tinypgp-key-find-by-guess)

    (define-key map (concat p "E")  'tinypgp-key-find-by-email)

    (define-key map (concat p "K")
      'tinypgp-key-find-by-http-guess)

    (define-key map (concat p "2")  'tinypgp-backend-select-pgp2)
    (define-key map (concat p "5")  'tinypgp-backend-select-pgp5)

    (define-key map (concat p "\C-m") 'tinypgp-key-find-by-guess)))

;;}}}
;;{{{ mode: key mode

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-mode-define-menu ()
  "Define menus."
  (easy-menu-define
    tinypgp-:key-mode-menu (if (ti::xemacs-p) nil tinypgp-:key-mode-map)
    "TinyPgp Key management menu"
    (list
     tinypgp-:key-mode-menu-name)))
;;;    ["Mail Sign"                     tinypgp-sign-mail                   t]

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-mode-define-keys ()
  "Define keys."
  (let* ((p    tinypgp-:key-mode-prefix-key)
         (map  tinypgp-:key-mode-map))
    (define-key map (concat p "a") 'tinypgp-key-add-region-batch)))

;;}}}
;;{{{ mode: summary mode

;;; --------------------------------------------------------- &summary ---
;;;
(defun tinypgp-summary-mode-define-menu ()
  "Define menus."
  (easy-menu-define
    tinypgp-:summary-mode-menu (if (ti::xemacs-p) nil tinypgp-:summary-mode-map)
    "TinyPgp Mail Summary management menu"
    (list
     tinypgp-:summary-mode-menu-name
     ["Verify"                   tinypgp-summary-mode-verify       t]
     ["Decrypt"                  tinypgp-summary-mode-decrypt      t]
     ["Next action"              tinypgp-summary-mode-next-action  t]
     ["Describe mode"            tinypgp-summary-mode-describe     t]

     "----"
     (list
      "Extra commands"
      ["Wash expire pass phrases/files"  tinypgp-password-expire-now         t]
      ["Wash expire secring password"
       tinypgp-secring-crypt-expire-password        t]
      "----"
      ["Info Display last finger error"  tinypgp-show-last-finger-error    t]
      ["Info View pgp register"          tinypgp-view-register               t]
      "----"
      ["Send email: .plan has no PGP key" tinypgp-sendmail-key-not-in-plan  t]
      ["Send email: keyserver cmd"        tinypgp-keysrv-send-email-command t])

     (list
      "Cache service"
      ["Remove last entry" tinypgp-key-cache-remove-entry-last   t]
      ["Display"           tinypgp-key-cache-display                 t])

     (list
      "Report service"
      ["Submit bug report"               tinypgp-submit-bug-report     t]
      ["Debug on/off"                    tinypgp-debug-toggle          t]
      ["Debug buffer clear"              tinypgp-debug-buffer-clear    t]
      "----"
      ["Display comint"                  tinypgp-show-buffer-comint    t]
      ["Display debug"                   tinypgp-show-buffer-debug           t]
      ["Display finger"                  tinypgp-show-buffer-finger    t]
      ["Display http"                    tinypgp-show-buffer-http            t]
      ["Display shell"                   tinypgp-show-buffer-shell           t]
      ["Display tmp"                     tinypgp-show-buffer-tmp     t]))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-summary-mode-define-keys ()
  "Define keys."
  (let* ((p    tinypgp-:summary-mode-prefix-key)
         (map  tinypgp-:summary-mode-map))

    (tinypgp-mode-define-keys-buffer map p)
    (tinypgp-mode-define-keys-user   map p)

    (define-key map
      (concat p (ti::string-right p 1)) 'tinypgp-summary-mode-next-action)

    (define-key map (concat p "?")      'tinypgp-summary-mode-describe)
    (define-key map (concat p "d")      'tinypgp-summary-mode-decrypt)
    (define-key map (concat p "v")      'tinypgp-summary-mode-verify)))

;;}}}
;;{{{ code: Mode functions

(eval (ti::macrof-minor-mode-viper-attach "tinypgp-mode-" 'tinypgp-mode))

;;; ----------------------------------------------------------------------
;;;
;;;
(ti::macrof-minor-mode
 tinypgp-mode                           ;1
 "PGP minor mode.
Mode description:
\\{tinypgp-:mode-map}
"
 tinypgp-install-modes                  ;3
 tinypgp-mode                           ;4
 tinypgp-:mode-name

 tinypgp-:mode-prefix-key               ;5
 tinypgp-:mode-menu                     ;6

 nil                                    ;7
 "TinyPgp"                              ;8
 tinypgp-:mode-hook

 (progn
   (if (null tinypgp-:pubring-now)
       (setq tinypgp-:pubring-now
             (tinypgp-expand-file-name
              (nth 1 (car (tinypgp-pubring-table))))))

   (if (not (file-exists-p tinypgp-:pubring-now))
       (error "TinyPgp: Can't init mode, pubring not found '%s'"
              tinypgp-:pubring-now))

   (if (not (stringp tinypgp-:user-now))
       (error "TinyPgp: Can't init mode, user is not defined '%s'"
              tinypgp-:user-now))
   (tinypgpd "tinypgp-mode" arg)
   (tinypgp-update-modeline)))

(defun turn-on-tinypgp-mode  ()
  "Pgp mode on."
  (tinypgp-mode 1))

(defun turn-off-tinypgp-mode ()
  "Pgp mode off."
  (tinypgp-mode 0))

;;; ----------------------------------------------------------------------
;;;
(defun turn-on-tinypgp-mode-maybe ()
  "Turn on `tinypgp-mode' only if PGP tags are found from buffer.
This function is by default installed into `find-file-hooks'."
  (when (and (null tinypgp-mode)
             (ti::mail-pgp-p))
    (turn-on-tinypgp-mode)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-describe ()
  "Describe mode."
  (interactive)
  (describe-function 'tinypgp-mode))

;;; .................................................... &pgp-key-mode ...

;;; ----------------------------------------------------------------------
;;;
(ti::macrof-minor-mode
 tinypgp-key-mode
 "PGP key handling minor mode. You should extract the the key information
to some buffer first before turning on this mode.
Eg. with \\[tinypgp-key-info-at-point-show]

Mode description:
\\{tinypgp-:key-mode-map}
"
 tinypgp-install-modes
 tinypgp-key-mode
 tinypgp-:key-mode-name                 ;5

 tinypgp-:key-mode-prefix-key
 tinypgp-:key-mode-menu                 ;7

 nil
 "TinyPgp Key handling"
 tinypgp-:key-mode-hook                 ;10

 (progn
   (if tinypgp-key-mode
       (tinypgp-update-modeline))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-mode-describe ()
  "Describe mode."
  (interactive)
  (describe-function 'tinypgp-key-mode))

;;; .................................................... &pgp-sum-mode ...

;;; ----------------------------------------------------------------------
;;;
(ti::macrof-minor-mode
 tinypgp-summary-mode
 "PGP summary minor mode. This function can only be turned on in VM
RMAIL and GNUS summary buffer.  Any PGP action called there is reflected
on the current message selected.

Mode description:
\\{tinypgp-:summary-mode-map}
"
 tinypgp-install-modes
 tinypgp-summary-mode
 tinypgp-:summary-mode-name

 tinypgp-:summary-mode-prefix-key
 tinypgp-:summary-mode-menu

 nil
 "TinyPgp Mail Summary"
 tinypgp-:summary-mode-hook

 (progn
   (when tinypgp-summary-mode
     (unless (memq major-mode
                   '(rmail-summary-mode
                     vm-summary-mode
                     gnus-summary-mode
                     mh-show-mode))
       (setq tinypgp-summary-mode nil)
       (error "You can use this mode only in Mail summary buffers."))

     ;; This modeline update is a problem only in RMAIL-summary buffer.
     ;; We cannot use rmail-summary-mode-hook, because turning mode on
     ;; there does no good (summary buffer is not shown yet)
     ;;
     ;; See advised function rmail-new-summary which calls us.
     ;;
     (tinypgp-update-modeline))))

(defun turn-on-tinypgp-summary-mode ()
  "Summary mode." (tinypgp-summary-mode 1))

(defun turn-off-tinypgp-summary-mode ()
  "Summary mode." (tinypgp-summary-mode 0))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-summary-mode-describe ()
  "Describe mode."
  (interactive)
  (describe-function 'tinypgp-summary-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-summary-mode-verify (&optional arg)
  "Verify current article with ARG.

Note:

  In GNUS Summary buffer, where the *Article* is guessed to be a newsgroup
  post, the prefix arg meaning has been reversed. When you verify
  newsgroup article, the content of the article is not replaced, as it
  would anywhere else."
  (interactive "P") (tinypgp-summary-action 'verify arg 'verb))

(defun tinypgp-summary-mode-decrypt (&optional arg)
  "Decrypt current article with ARG."
  (interactive "P") (tinypgp-summary-action 'decrypt arg 'verb))

(defun tinypgp-summary-mode-next-action (&optional arg)
  "Guess next action and pass ARG."
  (interactive "P") (tinypgp-summary-action 'next-action arg 'verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-summary-action-1 (action func arg verb)
  "See source code for `tinypgp-summary-action' for ACTION FUNC ARG VERB."
  (save-excursion
    (pop-to-buffer (current-buffer))
    (cond
     ((eq action 'verify)       (funcall func arg 'verb))
     ((eq action 'next-action)  (call-interactively func))
     ((eq action 'decrypt)      (tinypgp-decrypt-mail-verbose (quote arg))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-summary-action (action-sym &optional arg verb)
  "Do ACTION-SYM in summary buffer. ARG is passed to called function. VERB."
  (let* ((fid   "tinypgp-summary-action: ")
         (win   (get-buffer-window (current-buffer)))
         (list  '(verify decrypt next-action))
         str
         func)
    (if (not (memq action-sym list))
        (error "TinyPgp: Unregognized/Not supported summary action."))

    (setq str (format "tinypgp-%s-mail" (symbol-name action-sym)))

    (if (null (setq func (intern-soft str)))
        (error "TinyPgp: Function not found %s" str))

    (tinypgpd fid major-mode action-sym func)

    (cond
     ((eq major-mode 'rmail-summary-mode)
      (ti::mail-rmail-macro
       (tinypgp-summary-action-1 action-sym func arg verb)))

     ((eq major-mode 'vm-summary-mode)
      (ti::mail-vm-macro
       (tinypgp-summary-action-1 action-sym func arg verb)))

     ((eq major-mode 'gnus-summary-mode)
      (ti::mail-gnus-macro
       ;;  In newsgroup post the user doesn't want to "open"
       ;;  the message when he verifies it. Reverse the ARG meaning
       (when (and (eq action-sym 'verify) (ti::mail-news-buffer-p))
         (ti::bool-toggle arg))
       (tinypgpd fid action-sym "ARG" arg "NEWS" (ti::mail-news-buffer-p))
       (tinypgp-summary-action-1 action-sym func arg verb)))

     ((eq major-mode 'mh-show-mode)
      (ti::mail-mh-macro
       (tinypgp-summary-action-1 action-sym func arg verb)))

     (t
      (error "TinyPgp: I Can't do anything in this major mode.")))
    (if (window-live-p win)             ;Back to summary
        (select-window win))))

;;}}}

;;{{{ code: defadvice

;;; ....................................................... &defadvice ...

(defadvice rmail-new-summary (after tinypgp act)
  "Update mode line.
For some reason this couldn't be done from 19.28's`rmail-summary-mode-hook'."
  (tinypgp-summary-mode))

;;; (ad-unadvise 'vm-edit-message)
;;;
(defadvice vm-edit-message (after tinypgp dis)
  "If Edit is called interactively, call `turn-on-tinypgp-mode'.
We can't do this in `vm-edit-message-hoo' because the hook function
doesn't know if the function were called interactively or not."
  (if (and (interactive-p)
           (null tinypgp-mode))
      (turn-on-tinypgp-mode)))

;;}}}

;;{{{ Special: sending email

;;; ........................................................ &sendmail ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sendmail-key-not-in-plan (email)
  "Send small mail to EMAIL and ask him to add his PGP key to ~/.plan.

When you finger someone for his pgp key, consider this before you send
notice to person. (check the content of finger buffer)

o  Finger finds the .plan file and the contents seems valid, there is login
   name and directory information and soon. This would indicate that
   it is ok to send notice to person.
o  Finger result"
  (interactive
   (let* (ret)
     (and (y-or-n-p
           ;;  someone may think this harrashement
           ;;
           "Are you sure you want to send .plan notice? Think twice..." )
          (y-or-n-p
           "You did check the content of the finger results: was it ok otw? ")
          (setq ret
                (read-from-minibuffer
                 "Mail to: "
                 (ignore-errors (ti::mail-get-field "to" nil 'nil-mode)))))
     (if (not (string-match "@"))
         (error "Abort."))
     (list ret)))
  (tinypgp-sendmail email 'pk-finger-none)
  (if (interactive-p)
      (message "Email sent to: %s" email)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sendmail (email mode &optional arg1 arg2 arg3)
  "Send email notice to EMAIL address according to MODE and ARG1 ARG2 ARG3."
  (let* ((id
          (format
           "\nThis is message from TinyPgp.el %s\n\n"
           (tinypgp-version-number)))

         (subject       (format
                         " %s, Notification concerning your PGP."
                         email))
         msg)
    (cond
     ((eq mode 'pk-no-full-format)
      (setq msg
            (format
             (concat
              "\tWe fingered address %s to get your public key\n\t"
              "but it was not presented in full format of pgp -fakx.\n\t"
              "Would you please insert all test starting from\n\t"
              "'Key for user ID:' line from the -fakx output. \n\t"
              "That would offer access to your other information\n\t"
              "that may be needed. \nt"
              "\n"
              "Please note that the -kv format is not the same as -fakx\n"
              "\n"
              "\tThank you.\n")
             email)))
     ((eq mode 'pk-finger-none)
      (setq msg
            (format
             (concat
              "\tHi!\n\t"
              "We could finger address %s but there was no PGP \n\t"
              "Public key available. Would you kindly run \n\t"
              "'pgp -fakx' on you keyId and put all\n\t"
              "lines after 'Key for user ID:' to you $HOME/.plan file.\n\n\t"
              "Thank you.  Please excuse this message if you don't have\n"
              "or use PGP\n")
             email)))
     (t
      (error "TinyPgp: Unknown mode")))

    (ti::mail-sendmail-macro email subject 'send
                             (insert id msg))))

;;}}}
;;{{{ BBDB

;;; ............................................................ &bbdb ...

(defcustom tinypgp-:bbdb-field 'pgp-mail
  "*Field to use in BBDB to store PGP preferences.
Entry in table `tinypgp-:auto-action-table' overrides BBDB definition.

Field can have values:

  'sign'        Sign message
  'sign-Keyid'  Sign with KeyId
  'xpgp'        Use X-pgp when signing.
  'enccrypt'    Encrypt message by looking at To field. If you want to encrypt
                using some other value, like 0xFFFFFF hex key id, see
                variable `tinypgp-:email-substitution-table'
  'mime-tm'     use PGP/MIME with package TM
  '1pass'       Use 1 pass encrypt and sign. The message is signed with
                active pgp user's key
  '1pass-keyId' ..same but sign by using KeyId

You can't use `sign' and `encrypt' with `1pass', which has highest
precedence.

Examples:

  pgp-mail: sign                ;; Sign by pgp user
  pgp-mail: sign mime-tm        ;; PGP/MIME sign with TM package
  pgp-mail: encrypt
  pgp-mail: 1pass               ;; encryt and sign"
  :type   'symbol
  :group 'TinyPgp)

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-bbdb-1 (name address field)
  "Look up user NAME and ADDRESS in BBDB and return FIELD"
  (let* ((record (bbdb-search-simple name address)))
    (when record
      (bbdb-record-getprop record field))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-bbdb-id (&optional email)
  "Return BBDB `pgp-id' field matching EMAIL or To-field address."
  (interactive)
  (when (featurep 'bbdb)
    (let* ((fid      "tinypgp-bbdb-id:")
           (key      'pgp-id)
           ret
           address)
      (setq ret
            (if email
                (tinypgp-bbdb-1 "" email key)
              (setq address  (mail-extract-address-components
                              (or (mail-fetch-field "To" nil t) "")))
              (when (nth 1 address)
                (tinypgp-bbdb-1 (or (nth 0 address) "") (nth 1 address) key))))
      (tinypgpd fid 'ARG email 'address address 'RET ret)
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-bbdb-entry ()
  "Return bbdb auto action entry in format `tinypgp-:auto-action-table'."
  (when (featurep 'bbdb)
    (let* ((fid      "tinypgp-bbdb-entry:")
           (to-field (mail-fetch-field "To" nil t))
           (address  (mail-extract-address-components (or to-field "")))
           elt sign enc mime-mua xpgp)
      (tinypgpd fid to-field address)

      (when (and (nth 1 address)
                 (setq elt (tinypgp-bbdb-1 (car address) (nth 1 address)
                                           tinypgp-:bbdb-field)))

        (if (string-match "mime" elt)
            (setq mime-mua 'mime))

        (cond
         ((string-match "sign-\\([^ \t]+\\)" elt)
          (setq sign (match-string 1 elt)))
         ((string-match "sign" elt)
          (setq sign tinypgp-:user-now)))

        (if (string-match "xpgp" elt)
            (setq xpgp t))

        (setq enc (string-match "encrypt" elt))

        (when (string-match "1pass" elt)
          (setq sign (make-symbol tinypgp-:user-now))
          (setq enc  t))

        (if (and mime-mua sign)
            (setq sign (make-symbol sign)))

        ;; '(EVAL-OR-REGEXP  [SIGN-KEY-ID] [ENCRYPT]
        ;;   [MIME-MUA] [XPGP] [KEYRING])

        (list
         (nth 1 address)
         sign enc mime-mua xpgp)))))
;;}}}

;;{{{ special: Mode specific actions

;;; ................................................... &mode-specific ...

;;; ----------------------------------------------------------------------
;;; #todo: tinypgp-mail-do-fcc breaks in VM
;;;
(defun tinypgp-mail-do-fcc (&optional cmd user msg string)
  "Do FCC before the message is encrypted and remove FCC field.
You don't want sendmail.el to FCC message which was encrypted
with the other user's public key.

This function Supports MUAs:

  Sendmail Fcc      -- mail-mode
  Gnus Gcc          -- message-mode

Input:
  CMD
  USER
  MSG
  STRING"
  (let ((fid "tinypgp-mail-do-fcc: ")
        field-fcc field-gcc
        hmax)

    ;;#todo VM FCC must be handled differently ?
    ;;#todo Gnus 5 mail fcc ?

    (tinypgpd fid "in: " cmd user msg string major-mode)

    (setq field-fcc (mail-fetch-field "fcc")
          field-gcc (mail-fetch-field "gcc"))

    (when (and (memq major-mode '(mail-mode message-mode))
               (memq cmd        '(encrypt encrypt-sign)))

      (setq hmax (ti::mail-hmax))

      (tinypgpd fid 'fcc field-fcc 'gcc field-gcc
                'buffer (current-buffer)
                'header-max  hmax
                'point-max   (point-max))

      (when (and field-gcc (featurep 'gnus))
        (gnus-inews-do-gcc))

      (cond
       ((ti::xemacs-p)                  ;needs MARKER
        (save-excursion
          (goto-char hmax)
          (setq hmax (point-marker)))
        (mail-do-fcc hmax)              ;Header end
        (setq hmax nil))                ;kill marker

       (t                               ;XE19.14 and Emacs needs POINT
        (mail-do-fcc (ti::mail-hmax))))

      (tinypgp-hash 'fcc 'put 'fcc field-fcc)
      (tinypgp-hash 'gcc 'put 'gcc field-gcc)

      ;; Message saving happened in another buffer, remove these
      ;; fields from this original buffer.

      (while (not (ti::nil-p (mail-fetch-field "fcc")))
        (ti::mail-kill-field "^FCC"))

      (while (not (ti::nil-p (mail-fetch-field "gcc")))
        (ti::mail-kill-field "^GCC")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-specific-control-before
  (cmd &optional user msg string)
  "Turn on possible edit mode while we do some PGP action.
This function is called prior the PGP action takes effect in current
region or buffer. Eg. in RMAIL we have to turn on the edit mode in
order to modify the message content.

Input:

  The content of these function call parameter depends on the
  calling CMD which can be 'sign 'encrypt 'decrypt 'verify.
  You have to look at the source code to see what is passed in each case.
  CMD OPTIONAL USER MSG STRING

Used hash entries:

  The buffer pointer is recorded to hash table under property
  'mode-specific and value 'buffer. This is name of the current buffer
  where the original message is.

  When edit mode is turned on, the buffer may now be different and
  the buffer pointer is recorded under property 'mode-specific and value
  'buffer-edit.

References:

  `tinypgp-:verify-before-hook'         ;; contain this function
  `tinypgp-cmd-macro'                   ;; calls this function
  `tinypgp-mode-specific-control-after'   ;; 'the other side of the coin'"
  (let ((fid "tinypgp-mode-specific-control-before: "))

    (tinypgpd fid "CMD" cmd user msg  "BUFFER" (current-buffer) major-mode)

    (tinypgp-mail-do-fcc cmd user msg string)

    ;; When "after" function runs it checks if this flag is non-nil
    ;; and strores the contents of the "clone" buffer there.

    (tinypgp-hash 'mode-specific 'put 'register  nil    'global)

    ;;  We have to record the initial buffer, so that the AFTER
    ;;  hook can restore the state in correct buffer. The package
    ;;  may die anywhere in the code and the buffer pointer certainly
    ;;  isn't pointing to the right place any moreon error.

    (tinypgp-hash 'mode-specific 'put 'buffer      (buffer-name)  'global)
    (tinypgp-hash 'mode-specific 'put 'major-mode  major-mode     'global)
    (tinypgp-hash 'vm          'put 'control     nil            'global)

    ;; - For some unknown reason the VM window configurations
    ;;   is mixed when we open edit mode and close it afterwards,
    ;;   we must save window configuration now.
    ;; - We save this every time, but we only use it in VM

    (tinypgp-hash 'mode-specific 'put 'wcfg
                  (current-window-configuration)
                  'global)

    (tinypgp-hash 'mode-specific 'put 'frame    (selected-frame)    'global)
    (tinypgp-hash 'mode-specific 'put 'window   (selected-window)   'global)
    (tinypgp-hash 'mode-specific 'put 'read-only buffer-read-only    'global)

    (when (featurep 'vm)
      (tinypgp-hash 'vm 'put 'vm-frame-per-edit vm-frame-per-edit 'global))

    (tinypgp-hash 'mode-specific 'put 'buffer      (buffer-name)  'global)

    ;; ....................................................... secring ...
    (when (and tinypgp-:secring-crypt-mode
               (not (memq cmd '(verify))))
      (tinypgp-secring-use))

    ;; ......................................................... modes ...

    ;;  We have to quit TM so that underlying mode underneath is
    ;;  exposed.
    ;;
    ;;  But in Gnus, this wouldn't have any effect, because TM is permanently
    ;;  on. See ESC-t which runs `gnus-summary-toggle-mime'.

    (when (eq major-mode 'mime/viewer-mode) ;TM preview buffer
      (cond
       ((and (featurep 'gnus)
             (string= (buffer-name)
                      (symbol-value 'gnus-article-buffer)))
        nil)                        ;Entering article edit quits mime.
       (t
;;;     (setq buffer-read-only nil)
        ;;  in RMAIL this works
        (mime-viewer/quit))))

    (cond
     ((and (featurep 'gnus)
           (or (string= (buffer-name) (symbol-value 'gnus-article-buffer))
               (eq major-mode 'gnus-article-mode)))
      (when (and buffer-read-only
                 (not (eq 'ok (ignore-errors
                                (gnus-summary-edit-article) 'ok))))
        ;;  Eg. NNTP backend doesn't allow editing buffers.
        (message (substitute-command-keys "\
Gnus backend doesn't support edit. Use \\[tinypgp-view-register]"))
        (tinypgp-hash 'mode-specific 'put 'register t 'global)
        (set-buffer (tinypgp-clone-buffer)))

      ;;  Old Gnus versions have a bug, they give *Article* buffer
      ;;  for editing, which is not good. The buffer may have been
      ;;  formatted so that there is gnus buttons in the middle of the PGP
      ;;
      ;;  noRr110XVahfo/3MaLL2PGlJ/h8rOdZkJCPCQ1OO8BKcXg3NQWTb+RpqSbSRnbEq
      ;;  [...]
      ;;  win0apLYccO+tqhhzK3CIiDbgBGfQLNU9ju+nMOOm1VUfF2A/phMoQg6ucYrXFxk
      ;;
      ;;  We must edit the `gnus-original-article-buffer', which contains
      ;;  the message "as is".

      ;;  I submitted gnus bug report on this, but Lars didn't consider
      ;;  it as a bug: User is expect to do C-u g to view raw article.

      (if (not (buffer-live-p
                (get-buffer (symbol-value 'gnus-original-article-buffer))))
          ;;  If the original article weren't found, try anyway in
          ;;  this *Article* buffer. It may even succeed if there
          ;;  is no gnus buttons in the PGP block.

          (message "TinyPgp: Wish me luck, I couldn't find original article")

        ;;  Ok, found buffer, so play safe

        (delete-region (point-min) (point-max))
        (insert-buffer (symbol-value 'gnus-original-article-buffer))))

     ((memq major-mode '(rmail-mode))
      (rmail-edit-current-message))

     ((memq major-mode '(vm-mode))

      ;; - VM opens another frame immediately if you
      ;;   put message in edit mode (that happend when you decrypt mail)
      ;; - We don't want it to do that; Set this locally to nil

      (setq vm-frame-per-edit nil)
      (vm-edit-message)
      (tinypgp-hash 'vm 'put 'control 'edit 'global)))

    ;;  Expose any hidden text

    (set-text-properties (point-min) (point-max) nil)
    (ti::overlay-remove-region (point-min) (point-max))

    (tinypgp-hash 'mode-specific 'put 'buffer-edit (buffer-name) 'global)
    (tinypgpd fid major-mode "BUFFER-EDIT" (current-buffer))
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-specific-label (cmd &optional buffer)
  "Add mail-agent labels according to CMD. Work buffer is BUFFER."
  (let* ((fid "tinypgp-mode-specific-label")

         (tbl   tinypgp-:label-table)
         (v+    (nth 0 (nth 1 (assq 'v tbl))))
         (v-    (nth 1 (nth 1 (assq 'v tbl))))
         (si    (nth 1 (assq 's tbl)))
         (en    (nth 1 (assq 'e tbl)))
         (de    (nth 1 (assq 'd tbl)))
         (pgp   (nth 1 (assq 'pgp tbl)))
         stat)
    (with-current-buffer (or buffer (current-buffer))
      (cond

       ;; ...................................................... rmail ...

       ((memq major-mode '(rmail-mode rmail-edit-mode))

        (cond
         ((eq cmd 'sign)
          (rmail-kill-label v+)
          (rmail-kill-label v-)
          (when (not (ti::nil-p pgp)) (rmail-add-label pgp))
          (when (not (ti::nil-p si)) (rmail-add-label si)))

         ((eq cmd 'decrypt)
          (rmail-kill-label en)
          (when (not (ti::nil-p pgp)) (rmail-add-label pgp))
          (rmail-add-label de)

          ;;  The message may have beeen encrypted and signed (one pass),
          ;;  force checking verify too.

          (setq cmd 'verify))

         ((eq cmd 'encrypt)
          (rmail-kill-label de)
          (when (not (ti::nil-p pgp)) (rmail-add-label pgp))
          (rmail-add-label en))

         ((eq cmd 'encrypt-sign)
          (rmail-kill-label de)
          (when (not (ti::nil-p pgp)) (rmail-add-label pgp))
          (rmail-add-label en)
          (rmail-add-label si)))

        (when (eq cmd 'verify)
          ;;  This is special, the parameter call order is 'beg end RET'
          ;;
          (rmail-add-label "pgp")
          (setq stat (or (tinypgp-binary-get-result-verify-status) ""))
          (cond
           ((string-match "good.*signature" stat)
            (rmail-kill-label si)
            (rmail-kill-label v-)
            (rmail-add-label  v+))
           ((string-match "bad.*signature" stat)
            (rmail-kill-label si)
            (rmail-kill-label v+)
            (rmail-add-label  v-)))))

       ;; ......................................................... vm ...

       ((or (memq major-mode '(vm-mode vm-edit-mode))
            (string-match "edit.*note " (buffer-name)))
        (tinypgpd fid "LABELING" cmd (current-buffer))

        (cond
         ((eq cmd 'sign)
          (vm-delete-message-labels v+ 1)
          (vm-delete-message-labels v- 1)
          (vm-add-message-labels    si 1)
          (when (not (ti::nil-p pgp)) (vm-add-message-labels    pgp 1)))

         ((eq cmd 'decrypt)
          (vm-delete-message-labels en 1)
          (vm-add-message-labels    de 1)
          (when (not (ti::nil-p pgp)) (vm-add-message-labels    pgp 1))
          (setq cmd 'verify))

         ((eq cmd 'encrypt)
          (vm-delete-message-labels de 1)
          (vm-add-message-labels    en 1)
          (when (not (ti::nil-p pgp)) (vm-add-message-labels    pgp 1)))

         ((eq cmd 'encrypt-sign)
          (vm-delete-message-labels de 1)
          (vm-add-message-labels    si 1)
          (vm-add-message-labels    en 1)
          (when (not (ti::nil-p pgp)) (vm-add-message-labels    pgp 1))))

        (when (eq cmd 'verify)
          (when (not (ti::nil-p pgp)) (vm-add-message-labels pgp 1))
          (setq stat (or (tinypgp-binary-get-result-verify-status) ""))
          (cond
           ((string-match "good.*signature" stat)
            (vm-delete-message-labels si 1)
            (vm-delete-message-labels v- 1)
            (vm-add-message-labels    v+ 1))
           ((string-match "bad.*signature" stat)
            (vm-delete-message-labels si 1)
            (vm-delete-message-labels v+ 1)
            (vm-add-message-labels    v- 1)
            (when (not (ti::nil-p pgp))
              (vm-add-message-labels    pgp 1))))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-mode-specific-control-after
  (cmd &optional user msg string)
  "See `tinypgp-mode-specific-control-before' for CMD USER  MSG STRING."
  (let* ((fid  "tinypgp-mode-specific-control-after: ")

         ;;  We have to set this to nil; otherwise TM goes nuts
         ;;  when it calls tm-rmail/preview-message
         ;;  #todo: investigate

         rmail-show-message-hook

         (buffer
          (tinypgp-hash 'mode-specific 'get 'buffer nil 'global))
         (buffer-edit
          (tinypgp-hash 'mode-specific 'get 'buffer-edit nil 'global))

         restore-cfg)

    (when (tinypgp-hash 'mode-specific 'get 'register nil 'global)
      (with-current-buffer tinypgp-:buffer-tmp-article
        (ti::mail-hmax 'move)
        (set-register
         tinypgp-:register
         (buffer-substring (point) (point-max)))))

    (if rmail-show-message-hook ;;  ByteComp silencer; no-op
        (setq rmail-show-message-hook nil))

    (tinypgpd fid cmd user "BUFFER" buffer "B-edit" buffer-edit major-mode
              msg string (buffer-name))

    ;; ....................................................... secring ...

    (when tinypgp-:secring-crypt-mode
      (tinypgp-secring-kill-maybe))

    ;; .......................................................... mode ...
    ;;  The "before" hook must have been called otherwise, there must be
    ;;  some error somewhere or exist some situation I haven't thought of.

    (with-current-buffer (or buffer-edit
                             (prog1 nil
                               (tinypgpd fid "**CONFLICT; no buffer")
                               (ti::read-char-safe-until
                                "\
Internal error in AFTER HOOK; send bug report + debug immediately."))
                             (current-buffer))
      (cond
       ((memq major-mode '(rmail-mode rmail-edit-mode))
        ;; ..................................................... rmail ...

        (if (eq major-mode 'rmail-edit-mode)
            (rmail-cease-edit))
        (tinypgp-mode-specific-label cmd))

       ;; ....................................................... gnus ...
       ((eq major-mode 'gnus-article-edit-mode)
        (gnus-article-edit-done))

       ((memq major-mode '(gnus-article-mode
                           mime/viewer-mode))
        (setq buffer-read-only          ;Restore this value
              (tinypgp-hash 'mode-specific 'get 'read-only nil 'global)))

       ;; ......................................................... vm ...
       ((or (memq major-mode '(vm-mode vm-edit-mode))
            ;;  XEmacs 19.14 sources say...
            ;;
            ;;  In vm-edit.el :: vm-edit-message
            ;;  It says (funcall (or vm-edit-message-mode 'text-mode)),
            ;;  where vm-vars.el:1506:(defvar vm-edit-message-mode 'text-mode
            ;;
            ;;  --> VM does editing in text mode? Glup; that makes hard
            ;;      to detect its edit buffer.
            ;;
            (string-match "edit.*note " (buffer-name)))

        (tinypgpd fid "VM ENTRY" major-mode (current-buffer) (buffer-name))

        (setq restore-cfg 'vm)        ;Yes; we need to restore Win cfg

        (setq
         vm-frame-per-edit
         (tinypgp-hash 'vm 'put 'vm-frame-per-edit vm-frame-per-edit 'global))

        ;;  Only close edit mode if we opened it. If user was inside
        ;;  edit buffer, we don't close it here.

        (when (and (tinypgp-hash 'vm 'get 'control  nil 'global)
                   (or (eq major-mode 'vm-edit-mode)
                       (string-match "edit.*note " (buffer-name))))
          (vm-edit-message-end))
        (tinypgp-mode-specific-label cmd))))

    (when restore-cfg
      (let* ((wcfg
              (tinypgp-hash 'mode-specific 'get 'wcfg nil 'global))
             (frame
              (tinypgp-hash 'mode-specific 'get 'frame nil 'global))
             (window
              (tinypgp-hash 'mode-specific 'get 'window nil 'global)))
        (set-window-configuration wcfg)
        (select-frame frame)
        (select-window window)))

    (tinypgp-hash 'mode-specific 'put 'buffer nil) ;Clear this
    ;; hook's return value
    nil))

;;}}}

;;{{{ remail: misc

;;; .......................................................... &remail ...
;;; -r-  refers to remailing
;;; -r-h refers to remailer headers

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-subject-cookie ()
  "Return random subject cookie."
  (nth (1- (rand1 (length tinypgp-:r-subject-table)))
       tinypgp-:r-subject-table))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-elt-email2elt (email)
  "Return remailer entry when EMAIL is known."
  (ti::list-find tinypgp-:r-levien-table email
                 (function
                  (lambda (arg elt)
                    (string= arg (nth 1 elt))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-elt-remailer (remailer)
  "Return remailer elt when REMAILER is known."
  ;;  We have the alias name, find the real email address
  (or (assoc remailer tinypgp-:r-host-table)
      (error "No such remailer %s" remailer)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-type (alias &optional email)
  "Return post type for remailer. ALIAS and EMAIL are mutually exclusive."
  (if email
      (ti::mail-pgpr-reply-type (nth 2 (tinypgp-r-elt-email2elt email))))
  (ti::mail-pgpr-reply-type (nth 2 (assoc alias tinypgp-:r-levien-table))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-r-server-macro 'lisp-indent-function 2)
(defmacro tinypgp-r-server-macro (server account &rest body)
  "Find SERVER and do body or signal error.

Input:

  SERVER    remailer server (alias name)
  ACCOUNT   remailer ACCOUNT@some.remailer.net
            Can also be nil, in that case the `email' is not constructed.
            (gains little speed)
  BODY      lisp form to do if server exists.

Defined variables inside BODY

  `info'    Full Levien list entry for server
  `email'   Constructed according to ACCOUNT."
  (`
   (let* ((info  (or (assoc (, server) tinypgp-:r-levien-table)
                     (error "Server is unknown %s" (, server))))
          email)
     (if (, account)
         (setq email (tinypgp-r-format-email-address (, account) info)))

     ;;  If these varibles are not used in the macro BODY,
     ;;  then byteCompiler nags. Make it quiet.

     (if (null email) (setq email nil))
     (if (null info)  (setq info  nil))

     (,@ body))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-post-p (elt)
  "Check if this remailer ELT can be used for posting."
  (or (string-match "cut.* hash.* pgp.* post" (nth 2 elt))
      (error "\
TinyPgp: not enough properties %s '%s'" (nth 0 elt) (nth 2 elt))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-init-maybe ()
  "Call initialise function is needed."
  (tinypgp-backend-set-for-action 'remail)
  (or (tinypgp-hash 'remail 'get 'init nil 'global)
      (tinypgp-r-init)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-init (&optional force)
  "Initialise remailer support. Set up all necessary variables etc.
If `tinypgp-:r-levien-table' is non-nil, then this function does nothing.

FORCE tells to discard old values and build all from scratch.
You usually do this if you have updated your remailer list.
FORCE is set to t if you call this function interactively.

References:
  `tinypgp-:r-init-hook'  is run after initialise sequences have been completed."
  (interactive (list 'force))      ;inteactive call always forces init

  (let ((file  tinypgp-:r-list-file)
        (clist tinypgp-:r-control-list)
        val)
    (tinypgp-backend-set-for-action 'remail)

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... . check ...

    (if (not (stringp tinypgp-:r-mail2news-remailer)) ;has default
        (error "TinyPgp: Please set Usenet post remailer tinypgp-:r-mail2news-remailer."))

    (if (not (stringp tinypgp-:r-user-mail-address)) ;has default
        (error "TinyPgp: Please set tinypgp-:r-user-mail-address"))

    (when (not (and (stringp tinypgp-:r-list-file)
                    (file-exists-p tinypgp-:r-list-file)))
      (error "TinyPgp: Hm, no tinypgp-:r-list-file please see manual.")

      ;; 1997-08-30
      ;; - not a good idea. Person may not have access to ftp or
      ;;   the ftp location does not exist any more.

      (message "TinyPgp: Hm, no tinypgp-:r-list-file; fetching it by finger..")
      (sit-for 1)
      ;;  Notice the 'no-init parameter. It would otw loop back to us.
      (tinypgp-r-update-remailer-list 'verb 'no-init))

    ;; It is important that you have new remailer file, print
    ;; warning regularly if the file is old

    (when (and tinypgp-:r-list-file
               (progn
                 (setq val (tinypgp-hash 'remail 'get 'file-warning))
                 (if (not (integerp val))
                     (setq val 0))
                 (incf  val)
                 (tinypgp-hash 'remail 'put 'file-warning val)
                 (eq 0 (% val 5))))     ;every 5th call
      (tinypgp-hash 'remail 'put 'file-warning 0)
      (tinypgp-r-file-old-warning))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... do init ...

    ;;  If this doesn't exist, init all

    (when (or force (null tinypgp-:r-levien-table))
      (when (or (not (stringp file))
                (not (file-exists-p file)))
        (error
         (substitute-command-keys
          (concat
           "TinyPgp: Please set variable tinypgp-:r-list-file and call"
           "\\[tinypgp-r-update-remailer-list]"))))

      ;;  Full RAPH's list

      (setq tinypgp-:r-levien-table (tinypgp-r-get-list "." nil file clist))
      (tinypgpd "tinypgp-r-init: " tinypgp-:r-levien-table)

      (setq tinypgp-:r-host-table      ;only cpunk and some properties
            (tinypgp-r-get-list nil tinypgp-:r-levien-table))

      (if (null tinypgp-:r-host-table)
          (error
           "TinyPgp: Can't find good remailers from '%s'. Consult maintainer."
           tinypgp-:r-list-file))

      (setq tinypgp-:r-reply-block-cache nil) ;Build from scratch

      (tinypgp-hash 'remail 'put 'init (or force 'done) 'global)
      (if tinypgp-:r-init-hook (run-hooks 'tinypgp-:r-init-hook))

      (if (interactive-p)
          (message "TinyPgp: Remailer support initialised.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-file-old-warning (&optional file days-old fmt)
  "Print warning if file is too old.
Input:

  FILE      Defaults to `tinypgp-:r-list-file'
  DAYS-OLD  Defaults to 24 (3 weeks).
  FMT       Message  format. First arg is %s file and second %d how old file."
  (interactive)
  (let* (days)
    (or file
        (setq file tinypgp-:r-list-file)
        (error "No tinypgp-:r-list-file set."))

    (or days-old
        (setq days-old (* 3 7)))
    (setq days (ti::file-days-old file))

    ;;  over 3 weeks old remailer list...too old
    ;;
    (when (> days days-old)
      (save-excursion
        (message
         (format
          (or fmt "'%s' is approx %d days old, which is too much.")
          file days))
        (sit-for 1)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-get-list (&optional re list file control-list)
  "Get remailer list matching RE.

Input:
  RE        what properties to grep. The properties are sorted
            and defaults to \"cpunk.* hash.* pgp\". These features are
            considered minimum features from remailer.
  LIST      prepared list
  FILE      file from where to read the Levien list. LIST must be nil.
  CONTROL-LIST  See `ti::mail-pgpr-parse-levien-list'.

References:
  `tinypgp-:r-get-list-hook'  is run after the Levien file is read into
                            temporary buffer."
  (let* ((fid  "tinypgp-r-get-list:")
         ret)
    (setq re (or re "cpunk.* hash.* pgp"))

    (unless list

      ;;  Read remailer list from file and parse it

      (if (or (null file)
              (not (file-exists-p file)))
          (error "TinyPgp: Can't read remailer file file '%s'" file))

      (with-current-buffer (tinypgp-ti::temp-buffer)
        (insert-file-contents file)
        (run-hooks 'tinypgp-:r-get-list-hook)
        (tinypgp-r-file-old-warning)

        (ti::pmin)
        (unless (setq list (ti::mail-pgpr-parse-levien-list
                            nil control-list))
          (tinypgpd fid (buffer-string))
          (pop-to-buffer (current-buffer))
          (error "\
TinyPgp: Cannot parse this buffer: not in levien format. %s " file))))

    (dolist (elt list)
      (if (string-match re (nth 2 elt))
          (push elt ret)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-mail-mode-init ()
  "Turn off all interfering minor modes from remailer mail buffer."
  (let (s)
    (setq s 'post-command-hook)
    (make-local-hook s)                 ;19.30+
    (remove-hook s 'timi-post-command)  ;disable tinymail.el

    ;;  What should we remove from this hook ?

    (setq s 'post-command-idle-hook)
    (when (boundp s) (make-local-hook s))

    (setq s 'mime/editor-mode-flag)     ;tm.el
    (when (boundp s)  (set s nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-post-before-default ()
  "Disable/exit known minor modes/features."
  (ti::mail-mime-turn-off-mode)
  (if (featurep 'tinymail)
      (timi-mail 'disable)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-latent-time-random (remailer str)
  "Add 'r' to the time if REMAILER supports it. If STR is nil, do nothing."
  (when (stringp str)
    ;;
    ;; The remailers are not checked yet.
    ;;
    (if (not (char= (aref str (1- (length str))) ?r))
        (setq str (concat str "r"))))
  str)

;;}}}
;;{{{ remail: reply block

;;; ................................................... &r-reply-block ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-reply-block-read (remailer)
  "Read reply block for the REMAILER.
Don't use this function, use `tinypgp-r-reply-block-cache' instead.

Return:
  '(buffer-pointer
    pgp-beg          ,points
    pgp-end
    )"
  (let ((elt (assoc remailer tinypgp-:r-reply-block-table))
        file
        buffer
        reg
        beg
        end)
    (unless elt
      (error
       (format
        (concat
         "TinyPgp: No Reply block defined for remailer '%s' "
         "in tinypgp-:r-reply-block-table")
        remailer)))

    (setq file   (nth 1 elt)
          buffer (or (find-buffer-visiting file)
                     (if (file-exists-p file)
                         ;;
                         ;;  pure find avoigs calling hooks/modes
                         ;;  when file is loaded.
                         ;;
                         (ti::find-file-literally file)
                       (error "TinyPgp: No reply block file %s" file))))
    (with-current-buffer buffer
      (ti::pmin)

      ;;  Make sure it will not be modified.

      (setq buffer-read-only t)
;;;     (rename-buffer (concat " " file))

      (unless (setq reg (ti::mail-pgp-block-area 'any))
        (pop-to-buffer buffer)
        (error "TinyPgp: Can't find reply block region?"))
      (setq beg (point-min) end (cdr reg)))
    (if buffer
        (list buffer beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-reply-block-cache (mode &optional arg1)
  "Reply block cache management according to MODE and ARG1.

MODE:

  'get     ARG1 = remailer alias; return reply block.
  'put     ARG1 = `tinypgp-:r-reply-block-cache' element
  'del     ARG1 = remailer alias"
  (let ((table   tinypgp-:r-reply-block-table)
        buffer
        elt
        old)
    (cond
     ((eq mode 'get)
      (or (setq elt (assoc arg1 table))
          (error "TinyPgp: %s not defined in tinypgp-:r-reply-block-table '%s'"
                 arg1 mode))

      ;;  Have we read it already?
      (setq buffer (find-buffer-visiting (nth 1 elt)))
      (or (setq elt  (assq buffer tinypgp-:r-reply-block-cache))
          ;;  No, load it from file then
          ;;
          (and (setq elt (tinypgp-r-reply-block-read arg1))
               (setq old (assq (car elt) tinypgp-:r-reply-block-cache))
               (setq tinypgp-:r-reply-block-cache
                     (delq old tinypgp-:r-reply-block-cache)))
          (push elt tinypgp-:r-reply-block-cache))

      ;;  Remove non-existing buffers -- keep the list up to date

      (dolist (elt tinypgp-:r-reply-block-cache)
        (if (buffer-live-p (get-buffer (car elt)))
            (setq tinypgp-:r-reply-block-cache
                  (delq elt tinypgp-:r-reply-block-cache))))

      elt)

     ((eq mode 'del)
      (and (setq elt    (assoc arg1 table))
           (setq buffer (get-buffer
                         (file-name-nondirectory
                          (nth 1 elt))))
           (setq elt    (assq buffer tinypgp-:r-reply-block-cache))
           (setq tinypgp-:r-reply-block-cache
                 (delq elt tinypgp-:r-reply-block-cache)))
      tinypgp-:r-reply-block-cache)

     ((eq mode 'put)
      (push arg1 tinypgp-:r-reply-block-cache)
      arg1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-reply-block-insert (remailer)
  "Insert REMAILER's reply block."
  (interactive (list (tinypgp-ask-reply-block-remailer)))
  (let* ((elt (tinypgp-r-reply-block-cache 'get remailer)))
    (if (null elt)
        (error "TinyPgp: Invalid return value.")
      (insert-buffer-substring (nth 0 elt) (nth 1 elt) (nth 2 elt)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-reply-block-header (remailer latent key anon-to)
  "Return reply block headers of remailer.

Input:

  REMAILER  string or symbol or list, The remailer used.
            If list, then the REMAILER is remailer-elt from
            `tinypgp-:r-levien-table'
  LATENT    latent time specification. This is not be used if
            remailer does not support it.
  KEY       The conventional crypt password
  ANON-TO   Where to send the reply block (return address)."
  (if (symbolp remailer) (setq remailer (symbol-name remailer)))
  (let* ((properties   (cond
                        ((ti::listp remailer)  remailer)
                        ((tinypgp-r-elt-remailer remailer))))
;;;      (email         (nth 1 properties))
         (rtype         (ti::mail-pgpr-reply-type        (nth 2 properties)))
         ;;  What kind of reply block: With/out latent ?
         ;;
         (btype         (nth 2 properties)))
    (when latent
      (if (null (string-match "latent" btype))
          (setq latent nil)             ;Not supported
        (setq latent (tinypgp-r-latent-time-random remailer latent))))

    (when key
      (if (null (string-match "ek" btype))
          (setq key nil)))
    ;;  The Reply string type "cpunk, eric..."
    (ti::mail-pgpr-block nil rtype anon-to key latent)))

;;}}}
;;{{{ remail: reply-block: interactive

;;; ................................................... &reply-block-i ...
;;;
(defun tinypgp-r-reply-block-test (&optional no-confirm)
  "Send every reply block listed in `tinypgp-:r-reply-block-table'.
NO-CONFIRM bypasses asking.
If you don't receive mail back, there are two possibilities:
o  remailer is down
o  your reply block was not constructed correctly."
  (interactive "P")
  (tinypgp-r-init-maybe)
  (let* ((fid      "tinypgp-r-reply-block-test: ")
         (i       0)
         remailer
         email)
    (dolist (elt tinypgp-:r-reply-block-table)
      (setq remailer (nth 0 elt))
      (setq email    (nth 1 (assoc remailer tinypgp-:r-levien-table)))

      (tinypgpd fid remailer email)

      (cond
       ((null email)
        (ti::read-char-safe-until
         (format
          "[%s] Does not exist any more, delete reply block. [press]"
          remailer)))
       (t
        (when (or no-confirm
                  (y-or-n-p (format "Send reply block to %s " remailer)))
          (incf  i)
          (ti::mail-sendmail-macro email remailer 'send
                                   (ti::mail-kill-field
                                    "^Subject"
                                    (format
                                     "r-test %s"
                                     (ti::date-standard-date)))
;;;         (pop-to-buffer (current-buffer))
                                   ;;          (insert (ti::mail-pgpr-block 'epgp "cpunk" ) "\n")
                                   (tinypgp-r-reply-block-insert remailer)))))
;;;         (ti::pmin) (ti::d! "testing-rblock")

      (if (interactive-p)
          (message "Sent %d test reply block%s."
                   i (if (eq i 1) "" "s"))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-reply-block-basic
  (remailer-elt &optional latent key anon-to final verb)
  "Contruct most basic reply block.

The created encrypted reply block will contain following

  ::
  Request-Remailing-To: <`tinypgp-:r-user-mail-address'>
  Encrypt-Key: <key you gave>
  Latent-Time: <latent time you gave>

Important:

  You must be in empty [mail] buffer. When this function finishes, you
  should _encrypt_ the mail body.

Input:

  REMAILER-ELT      Remailer table entry
  LATENT            \"+1:00\"
  KEY               crypt key, no spaces
  ANON-TO           send-to@some.com
  FINAL             flag, if this is final block, include
                    \"**\" to the end. (See remailer faqs)
                    And kill any extra headers.

  VERB              Verbose messages.

Interactive call note:

  LATENT can be passed by prefix arg. Each \\[universal-argument] adds 30 minutes, so
  3 times \\[universal-argument] is same as +1:30.
  Numeric argument gives straigh hours, so M - x 2 means +2:00. Latent time
  is not always supported by selected remailer and it is ignored if remailer
  can't use it.

  ANON-TO  is `tinypgp-:r-user-mail-address'.

  FINAL is always set to t"
  (interactive
   (progn

     (unless (ti::mail-body-empty-p)
       (if (y-or-n-p "Fresh buffer needed, empty this buffer? ")
           (progn
             (ti::mail-text-start 'move) (delete-region (point) (point-max)))
         (error "TinyPgp: Buffer must be emptied first")))

     (ti::list-merge-elements
      (tinypgp-ask-remail-args)
      tinypgp-:r-user-mail-address
      t)))
  ;; ... ... ... ... ... ... ... ... ... ... ... ... . interactive end . .

  (let* ()
    (tinypgp-r-init-maybe)
    (ti::verb)
    (unless (ti::mail-body-empty-p)
      (error "TinyPgp: Buffer must be emptied first"))

    (tinypgp-r-chain-1 remailer-elt latent key anon-to final)

    (if tinypgp-:r-reply-block-basic-hook
        (run-hooks 'tinypgp-:r-reply-block-basic-hook))

;;;    (if verb
;;;     (message "If you encrypt this, you should leave '**' outside."))

    nil))

;;}}}
;;{{{ remail: interactive

;;; ................................................... &r-interactive ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-chain-1 (remailer-elt &optional latent key anon-to final)
  "Encrypt mail to next remailer.
Input:

  REMAILER-ELT      remailer elt from `tinypgp-:r-levien-table'
  LATENT            latent time e.g. 0:00r  (not used if...)
  KEY               Encrypt key (not used if remailer does not support it)
  ANON-TO           send-to@somewhere.com
  FINAL             flag, prefix arg, if this is final block, include
                    \"**\" to the end. (See remailer faqs)

Return:
  email             remailer address"
  (interactive
   (ti::list-merge-elements
    (tinypgp-ask-remail-args)
    (read-from-minibuffer
     "Anon to: "
     (mail-fetch-field "To"))))
  (tinypgp-r-init-maybe)
  (let* (tinypgp-:xpgp-signing-mode     ;Do not use X-Pgp
         (properties    remailer-elt)
         (email         (nth 1 properties))

         ;;  The Reply string type "cpunk, eric..."

         (rtype         (ti::mail-pgpr-reply-type        (nth 2 properties)))

         (mail          (ti::mail-mail-p))
         str)
;;;      str2
    (or tinypgp-:r-mode-indication-flag
        (setq tinypgp-:r-mode-indication-flag 'basic-1))

    (if (and key (string-match "[ \t\n]" key))
        (error "TinyPgp: Key may not contains spaces '%s'" key))

    ;; ........................................... destination address ...

    (ti::mail-text-start 'move)

    (setq str (tinypgp-r-reply-block-header remailer-elt latent key anon-to))

    (insert str "\n")
    (tinypgp-encrypt-mail email)

    ;; ................................................ remail address ...

    (ti::mail-text-start 'move)

    ;;  The outer block encrypt key is disabled for now because
    ;;  it causes double encryptinn. When you receive the mail,
    ;;  then you have to decrypt it twice...not convenient.
    ;;
    ;;  The Reply Blocks EK should be enough. User can add the
    ;;  extra Field if he wants it.

    (setq str (ti::mail-pgpr-block nil rtype email nil latent))
    (insert str "\n")

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... . final ...

    (when final
      (ti::mail-pgpr-close)

      (when mail
        (ti::mail-kill-field "^To" (concat "  " email) )

        ;;  sysadm in your site probably isn't interested in subjects
        ;;  like this one. We don't want to draw his attention

        (ti::mail-kill-field "^Subject" (tinypgp-r-subject-cookie))
        (ti::mail-kill-field "^Fcc")
        (ti::mail-kill-field "^Gcc")    ;GNUS 5
        (ti::mail-kill-field "^Reply-to")))

    email))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-chain (chain &optional verb)
  "Decrypt current message according to remailer CHAIN. VERB.
Important, before you call this function:
1. You have called \\[tinypgp-r-post] or \\[tinypgp-newnym-post] to convert
   the message into remail post format first.
2. You must have encrypted the message.

Only after these, the additonal chain layers are feasible."
  (interactive
   (progn
     (or tinypgp-:r-chain
         (error "TinyPgp: tinypgp-:r-chain is empty"))
     (list
      (completing-read
       "Select remailer chain: "
       tinypgp-:r-chain nil 'match nil 'tinypgp-:history-r-chain))))
  (let* (to
         list
         remailer
         remailer-elt
         latent key anon-to
         final)
    (ti::verb)
    (or (setq chain (assoc chain tinypgp-:r-chain))
        (error "TinyPgp: No such choice in tinypgp-:r-chain"))

    (setq chain (nth 1 chain)   list chain)

    (unless (or (vectorp chain)
                (vectorp (setq list (eval chain))))
      (error "TinyPgp: %s evaluated to %s, which is not vector." chain list))

    (or (setq list (append list nil))   ;Convert to list :-)
        (error "TinyPgp: Vector list was empty?"))

    ;;  I can't do automatic encryption, because I have no of
    ;;  knowing if user had called C-c / . p to convert the message
    ;;  to post format. It would be disaster to encrypt non-post message

    (or (ti::mail-pgp-encrypted-p)
        (error "\
TinyPgp: The message must have been encrypted to mail2news gateway."))

    (dolist (elt list) ;; #todo: Can't use dolist beacause tests FINAL
      (setq remailer (nth 0 elt)
            latent   (nth 1 elt)
            key      (nth 2 elt)
            remailer-elt  (tinypgp-r-elt-remailer remailer)
            final    (null (cdr list))  ;No more remailers
            anon-to  (mail-fetch-field "to"))
      (or (ti::nil-p to)
          (error "TinyPgp: To address is empty, can't use Anon-to"))
      (tinypgp-r-chain-1 remailer-elt latent key anon-to final))

    ;;  Let's do fast check and turn off auto-action

    (when (and verb (tinypgp-auto-action-on-modeline-p))
      (tinypgp-hash 'auto-action 'put 'user-mode nil))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-update-remailer-list (&optional verb no-auto-init)
  "Finger remailer list maintainer and get updated list.
VERB allows verbose messages. NO-AUTO-INIT suppresses call to
`tinypgp-r-init' after file update."
  (interactive)
  (let ((file   tinypgp-:r-list-file)
        (email  tinypgp-:r-list-finger)
        (buffer (tinypgp-ti::temp-buffer))
        ret)
    (ti::verb)

    (setq ret (ti::process-finger email nil nil buffer verb))
    (cond
     ((not (bufferp ret))
      (setq tinypgp-:last-network-error ret)
      (error "TinyPgp: finger Failed: %s" ret))
     (t
      (ti::file-delete-safe file)
      (with-current-buffer ret (write-region (point-min) (point-max) file))
      (if verb
          (message "TinyPgp: remailer list [%s] updated." file))
      (call-interactively 'tinypgp-r-init)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-post (&optional type)
  "Anonymize message. See TYPE from `tinypgp-r-post-usenet'."
  (interactive)
  (let* ()
    (or type
        (setq type 'remail))
    (tinypgp-r-init-maybe)
    (run-hooks 'tinypgp-:r-post-before-hook)
    (if (ti::mail-news-buffer-p)
        (tinypgp-r-post-usenet type)
      (call-interactively 'tinypgp-r-post-regular))
    (run-hooks 'tinypgp-:r-post-after-hook)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-post-regular
  (remailer &optional insert-reply-block remailer-elt)
  "Normal mail to: send as anonymous post. Bulk mail is not permitted.
This means that any BCC or FCC field generates error.

The Prefix arg inserts to the message a reply block, so that person
can answer to the mail if he sends the message back to remailer.

Notes:

  `post-command-hook' and possible `post-command-idle-hook' are
  bound locally to current buffer and set to nil,
  so that nothing special happens when you compose and send this mail.

Input:

  REMAILER              remailer alias name
  INSERT-REPLY-BLOCK    prefix arg, if non-nil, insert remailer reply block
  REMAILER-ELT          the remailer entry from table `tinypgp-:r-host-table'"
  (interactive
   (let (remailer
         remailer-elt)
     (setq remailer (tinypgp-ask-remailer))
     (setq remailer-elt (tinypgp-r-elt-remailer remailer))
     (list remailer current-prefix-arg remailer-elt)))
  (let* (tinypgp-:xpgp-signing-mode     ;Do not use X-Pgp
         (var-list    '(post-command-hook
                        post-command-idle-hook))
         (hlist       (delete 'newsgroups (ti::mail-required-headers)))
         (hlist       (push  'to hlist))
         (to          (ti::mail-get-field "TO" nil 'nil-mode))

         (reply-msg   "To reply to this message, send it to some remailer.")
         (properties   (or remailer-elt
                           (assoc remailer tinypgp-:r-levien-table)
                           (error "TinyPgp: No remailer [%s]" remailer)))
         (email         (nth 1 properties))

         ;;  The Reply string type "cpunk, eric..."
         ;;
         (rtype         (ti::mail-pgpr-reply-type        (nth 2 properties)))

         hash-headers
         header-block
         message
         point
         str)

    (if (or (mail-fetch-field    "CC")
            (mail-fetch-field    "BCC"))
        (error "TinyPgp: sorry, bulk CC or BCC mail is not permitted."))

    (if (null to)
        (error "TinyPgp: No TO field filled."))

    (tinypgp-r-init-maybe)
    (setq str (ti::mail-pgpr-block nil rtype to))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . headers . .
    ;;  Get rid of headers that may reveal your identity

    (ti::mail-kill-non-rfc-fields hlist)

    (if (setq hash-headers (tinypgp-header-move-to-body 'move-to-body 'no-ins))
        (setq header-block (mapconcat 'concat hash-headers ""))
      (setq header-block ""))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... . doing message ...

    (ti::mail-kill-field "to" email)
    (setq point   (ti::mail-text-start))
    (setq message (buffer-substring point (point-max)))
    (delete-region point (point-max))

    (ti::pmax)
    (insert
     str
     "Cutmarks: --\n\n"
     "##\n" header-block "\n")

    (when insert-reply-block
      (tinypgp-r-reply-block-insert remailer))

    (when (and insert-reply-block reply-msg)
      (insert reply-msg "\n" ))
    (insert message "\n--\n")

    ;;  Make sure there is nothing that interferes sending.
    ;;  make them first local; then set them to nil

    (dolist (sym var-list)
      (when (boundp sym)
        (make-local-hook sym)
        (set sym nil)))))

;;; ----------------------------------------------------------------------
;;;
;;;
(defun tinypgp-r-post-usenet-body-convert
  (groups email &optional rb rtype rblk)
  "Convert body text into Remail post.
Supposes that you have already reformatted the buffer.

Input:

  GROUPS    list of newsgroups where to post.
  EMAIL     the mail2news gateway email address
  RB        if string, insert reply block of remailer given.
  RTYPE     Remailer reply type, e.g. 'cpunk'
  RBLK      Header block"
  (interactive)
  (let* ((reply-msg   "To reply to this message, send it to some remailer.")
         point
         message
         block
         str)

    ;; ... ... ... ... ... ... ... ... ... ... ... ...  doing blocks . .

    (setq point   (ti::mail-text-start)
          message (buffer-substring point (point-max)))
    (delete-region point (point-max))

    (dolist (grp groups)
      (setq str   (ti::mail-pgpr-block nil rtype grp)
            block (concat str "\n##\n" rblk  "\n"))
      (ti::pmax)
      (insert block)
      (when rb
        (tinypgp-r-reply-block-insert rb)
        (insert "**\n")
        (insert reply-msg "\n" ))
      (insert message "\n--\n"))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-r-post-usenet (type &optional rb)
  "Usenet message: Convert current message into anonymous remailer post.
Call error if buffer is not a newsgroup post.

Input:

  TYPE  type of conversion: 'newnym or 'remail
  RB    Insert reply block of remailer RB, so that user can aswer to you
        directly by using this replay block.

Return:

 '(remailer-email-addr  (newsgroup newsgroup ..))

References:

 `tinypgp-:r-mail2news-remailer'"
  (let* (post-command-hook
         mail-setup-hook
         mail-mode-hook
         message-mode-hook

         hash-headers
         header-block
         group-fld
         group-list
         phost                          ;posting host
         phost-elt
         phost-prop
         phost-email
         rtype
         sym)
    (setq tinypgp-:r-mode-indication-flag 'post)

    ;;  Why these if statemnts? Because the byteCompiler sees that
    ;;  I have introduced hooks in let*, but I never use them!
    ;;  This fools bytecompiler to believe they are used and it
    ;;  doesn't give any warnings any more

    (if post-command-hook   (setq post-command-hook nil)) ;ByteComp silencer
    (if mail-setup-hook     (setq mail-setup-hook nil))
    (if mail-mode-hook      (setq mail-mode-hook nil))

    ;;  This does exist between 19.30 - 19.33; but then it was made obsolete
    ;;  This trick gives clean byteCompilation and no warnings

    (setq sym 'post-command-idle-hook)

    (when (fboundp sym)
      (make-local-hook sym)
      (set sym  nil))

    (setq group-fld   (mail-fetch-field  "Newsgroups"))

    (when (and t                        ;Enabled now
               (ti::nil-p group-fld))
      (error "TinyPgp: No newsgroups? Buffer must contain a news message."))

    (run-hooks 'tinypgp-:r-post-before-hook)

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... variables . .
    ;;  Read needed variables

    (if (or (null (setq phost     (eval tinypgp-:r-mail2news-remailer)))
            (null (setq phost-elt (assoc phost tinypgp-:r-levien-table))))
        (error "TinyPgp: tinypgp-:r-mail2news-remailer '%s' %s"
               tinypgp-:r-mail2news-remailer phost))

    (tinypgp-r-post-p phost-elt)    ;Calls error is not capable enough
    (setq phost-email (nth 1 phost-elt)
          phost-prop  (nth 2 phost-elt)
          rtype       (ti::mail-pgpr-reply-type phost-prop))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... . headers . .
    ;;  Get rid of headers that may reveal your identity
    ;;  Don't kill in-reply to because it is used in newsgroup postings.

    (tinypgp-header-kill)

    ;;  Save all headers because they are inserted into body

    (if (setq hash-headers (tinypgp-header-move-to-body 'move-to-body 'no-ins))
        (setq header-block (mapconcat 'concat hash-headers ""))
      (setq header-block ""))

    ;;  The remaier doesn't need this field

    (ti::mail-kill-field "in-reply-to")
    (ti::mail-kill-field "newsgroups")

    (if (string-match "," group-fld)
        (setq group-list (split-string group-fld "[,\t\n ]+"))
      (setq group-list (list group-fld)))

    ;; ... ... ... ... ... ... ... ... ... ... ... ...  doing blocks . .

;;;      (setq point   (ti::mail-text-start)
;;;         message (buffer-substring point (point-max)))
;;;      (delete-region point (point-max))

    (cond
     ((eq type 'newnym))
     ((eq type 'remail)
      (tinypgp-r-post-usenet-body-convert
       group-list phost rb rtype header-block)))

    (ti::pmin) (insert "To: " phost-email "\n") ;Set destination

    (mail-mode)                  ;This is not a news message any more.
    (unless tinypgp-mode (tinypgp-mode 1))

    (list phost-email group-list)))

;;}}}

;;{{{ newnym: misc

;;; ........................................................ &r-newnym ...

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-newnym-read-word ()
  "Read newnym configuration command word."
  (let* (word)
    (save-excursion
      (when (char= (char-syntax (following-char)) ?\ ) ;Sitting on whitespace
        (backward-char 1))
      (when (setq word (ti::buffer-read-space-word))
        (ti::string-match "[^-+=]+" 0 word)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-newnym-list (&optional mode force)
  "Return ELTS for all 'newnym'.
Normally once the list has been generated; it is stored to hash.

Input:

  MODE      nil    = return full configuration list
            'alias = return alias name list
  FORCE     reread Levien table content and update hash.

References:
  `tinypgp-:r-levien-table'"
  (let* ((list  (tinypgp-hash 'remail 'get 'newnym)))
    (tinypgp-r-init-maybe)
    (when (or force (null list))
      (or tinypgp-:r-levien-table
          (error "TinyPgp: Levien list is nil."))

      (setq list (tinypgp-r-get-list "newnym" tinypgp-:r-levien-table))
      (tinypgp-hash 'remail 'put 'newnym list)
      (tinypgp-hash 'remail 'put 'newnym-alias (mapcar 'car list)))
    (cond                               ;No other choices yet
     ((eq mode 'alias)
      (setq list (tinypgp-hash 'remail 'get 'newnym-alias))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-format-email-address (account remailer-entry)
  "Return address that has ACCOUNT@site.com derived from REMAILER-ENTRY.
The remailer-entry is one of the entries in `tinypgp-:r-levien-table'"

  (let* ((email (nth 1 remailer-entry)))
    (if (null (string-match "^[^@]+" email))
        (error "TinyPgp Internal error. Call \\[tinypgp-r-init] or maintainer."))
    ;;  Set address to "help@..."
    ;;
    (ti::replace-match 0 account email)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-newnym-ask-server (&optional prompt)
  "Ask newnym server name with completion and PROMPT."
  (tinypgp-r-init-maybe)
  (or (get 'tinypgp-:r-newnym-default-account-table 'default-server)
      (completing-read
       (or "Newnym account domain: " prompt)
       (ti::list-to-assoc-menu (tinypgp-newnym-list 'alias))
       nil
       'match)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-file-stamp-name (server account)
  "Return Stamp file name according to SERVER and ACCOUNT."
  (concat tinypgp-:r-newnym-stamp-file-prefix
          (ti::string-mangle (concat server account))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-file-stamp (server account)
  "Stamp Newnym file with with SERVER and ACCOUNT."
  (let* ((file (tinypgp-newnym-file-stamp-name server account)))
    (if (and (file-exists-p file)
             (not (file-writable-p file)))
        (set-file-modes file (ti::file-mode-make-writable (file-modes file))))
    (ti::file-touch file)
    (ti::file-mode-protect file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-account-expiry-warnings ()
  "Print possible account expiry warnings.
References:
 `tinypgp-:r-newnym-default-account-table'"
  (interactive)
  (let* ((limit 100)                ;It's actually 120, but we use 100
         server account
         days
         file
         ret)
    (dolist (elt tinypgp-:r-newnym-default-account-table)

      (setq server   (nth 1 elt)
            account  (nth 2 elt)
            file     (tinypgp-newnym-file-stamp-name server account))

;;;      (setq F file S server A account)
;;;      (ti::d! (file-exists-p file) account server file)

      (cond
       ((null (file-exists-p file))
        (message "TinyPgp Warning: No stamp file for %s %s, Creating..."
                 server
                 account)
        (tinypgp-newnym-file-stamp server account))
       (t
        (setq days (ti::file-days-old file)
              ret  (format "%s %s: %d" ret account (- limit days)))
        (when (> days limit)
          (message
           "Tinypgp Newnym stamp is %d days old, account may expire: %s %s"
           days
           server
           account)
          (sit-for 3)))))
    (when ret
      (message ret))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-newnym-ask-account ()
  "Ask newnym Account name."
  (tinypgp-r-init-maybe)
  (let* (nym)
    (setq
     nym
     (or (get 'tinypgp-:r-newnym-default-account-table 'default-account)
         (if (ti::nil-p
              (setq nym
                    (read-from-minibuffer
                     "Nym account login name: "
                     nil nil nil)))
             'tinypgp-:history-newnym-account
           (error "TinyPgp: Empty not accepted.")
           nym)))
    nym))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-ask-srv-acc (&optional confirm-msg)
  "Ask '(server account) with optional CONFIRM-MSG."
  (tinypgp-r-init-maybe)
  (let* (srv
         acc)
    (if confirm-msg
        (or (y-or-n-p confirm-msg)
            (error "Abort")))
    (setq srv (tinypgp-newnym-ask-server))
    (setq acc (tinypgp-newnym-ask-account))
    (list srv acc)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-ask-srv-acc-arg (&optional confirm-msg)
  "Ask '(server account prefix_arg) with CONFIRM-MSG."
  (tinypgp-r-init-maybe)
  (if confirm-msg
      (or (y-or-n-p confirm-msg)
          (error "Abort")))
  (list
   (tinypgp-newnym-ask-server)
   (tinypgp-newnym-ask-account)
   current-prefix-arg))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-r-sendmail-create-buffer (name &optional subject)
  "Create mail buffer. The old buffer is killed.
Input:
  NAME     buffre name. Kill eny existing buffer with NAME without query.
  SUBJECT  Message subject."
  (ti::kill-buffer-safe name)
  (with-current-buffer (tinypgp-ti::temp-buffer 'mail "NONE" (or subject ""))
    (rename-buffer name)
    (buffer-enable-undo)
    (setq tinypgp-:r-mode-indication-flag 'newnym)
    (tinypgpd "tinypgp-r-sendmail-create-buffer" name (current-buffer))
    (current-buffer)))

;;; ----------------------------------------------------------------------
;;;
(eval-and-compile
  (defun tinypgp-newnym-sendmail-fmacro-1 (func doc account subject msg)
    "Use `tinypgp-newnym-sendmail-fmacro' instead.
See FUNC DOC ACCOUNT SUBJECT MSG there."
    (let* ((sym (intern (symbol-name (` (, func))))))
      (`
       (defun (, sym)  (alias &optional verb)
         (, doc)
         (interactive (list (tinypgp-newnym-ask-server)))
         (ti::verb)
         (tinypgp-r-init-maybe)
         (tinypgp-r-server-macro alias (, account)
                                 (ti::mail-sendmail-macro email (, subject) 'send (insert "empty"))
                                 (if verb
                                     (message "'%s' request sent to %s, wait for answer."
                                              (, msg) email))))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-newnym-sendmail-fmacro 'lisp-indent-function 0)
(defmacro tinypgp-newnym-sendmail-fmacro (func doc account subject msg)
  "Create interactive function that sends mail to remailer.
Input:

  FUNC          Created function name
  DOC           Function's doc string
  ACCOUNT       the account name where to send email request
  SUBJECT       Subject for email
  MSG           Notification message to interactive user e.g. 'create'."
  (` (, (tinypgp-newnym-sendmail-fmacro-1
         func doc account subject msg ))))

;;}}}
;;{{{ newnym: keys; menus

;;; ................................................... &newnym-mode ...

(defun tinypgp-newnym-mode-define-menu ()
  "Define menus."
  (easy-menu-define
    tinypgp-:newnym-mode-menu (if (ti::xemacs-p)
                                  nil
                                tinypgp-:newnym-mode-map)
    "TinyPgp Newnym management menu"
    (list
     tinypgp-:newnym-mode-menu-name
     ["Nym-Commands: Electric tab"   tinypgp-newnym-mode-electric-tab        t]
     ["Nym-Commands: Go to."         tinypgp-newnym-mode-nym-commands-goto           t]
     ["Reply-Block:  Add"            tinypgp-newnym-mode-reply-block         t]
     ["Reply-Block:  Kill"           tinypgp-newnym-mode-reply-block-kill            t]
     ["Public-key:   Add"            tinypgp-newnym-mode-public-key          t]
     ["Public-key:   Kill"           tinypgp-newnym-mode-public-key-kill     t]
     ["Mode description"             tinypgp-newnym-mode-describe                    t]
     "----")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-define-keys ()
  "Define keys."
  (let* ((p    tinypgp-:newnym-mode-prefix-key)
         (map  tinypgp-:newnym-mode-map))
    (define-key map "\t"                'tinypgp-newnym-mode-electric-tab)
    (define-key map (concat p "\t")     'tinypgp-newnym-mode-nym-commands-goto)
    (define-key map (concat p "p")      'tinypgp-newnym-mode-public-key)
    (define-key map (concat p "P")      'tinypgp-newnym-mode-public-key-kill)
    (define-key map (concat p "r")      'tinypgp-newnym-mode-reply-block)
    (define-key map (concat p "R")      'tinypgp-newnym-mode-reply-block-kill)
    (define-key map (concat p "?")      'tinypgp-newnym-mode-describe)))

;;}}}
;;{{{ newnym: Mode functions

;;; ----------------------------------------------------------------------
;;;
;;;
(ti::macrof-minor-mode
 tinypgp-newnym-mode                    ;1
 "Newnym account management help mode.
You turn this mode on in mail buffer and it helps you to
compose message to nym account. The most interesting command probably
is `tinypgp-newnym-mode-electric-tab'; which works as follows

  If cursor is anywhere else that at the line Nym-Commands:, then the
  original tab function is called.

Nym-Commands: create +acksend +fin  +
|             |     |             |  |
|             |     |             |  complete all commands
|             |     |             complete command 'fin'
|             |     Show default setting or example (previous word)
|             |     *
|             Show command help and advance to '*'.
If the cursor is over word Nym-Commands:, then advance forward to first
command word.

In hooks you should use functions

  `turn-on-tinypgp-newnym-mode'
  `turn-off-tinypgp-newnym-mode'

Mode description:
\\{tinypgp-:newnym-mode-map}
"
 tinypgp-install-modes                  ;3
 tinypgp-newnym-mode                    ;4
 tinypgp-:newnym-mode-name

 tinypgp-:newnym-mode-prefix-key        ;5
 tinypgp-:newnym-mode-menu              ;6

 nil                                    ;7
 "Newnym acocunt handling"              ;8
 tinypgp-:newnym-mode-hook              ;

 (progn
   (tinypgp-update-modeline)))

(defun turn-on-tinypgp-newnym-mode ()
  "Newnym mode on."
  (tinypgp-newnym-mode 1))

(defun turn-off-tinypgp-newnym-mode ()
  "Newnym mode off."
  (tinypgp-newnym-mode 0))

;;; .............................................. &newnym-interactive ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-describe ()
  "Describe mode."
  (interactive)
  (describe-function 'tinypgp-newnym-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-electric-tab ()
  "Compose newnym commands if cursor is on field Nym-Commands.
Otherwise call original mode's tab key. See description of this command
from `tinypgp-newnym-mode'."
  (interactive)
  (let* ((tbl tinypgp-:newnym-cmd-table)
         elt
         word)
    (cond
     ((null
       (save-excursion (beginning-of-line) (looking-at "Nym-Commands:")))
      ;;  Turn mode off and call original tab key.
      ;;
      (let* (tinypgp-newnym-mode)
        (call-interactively (key-binding "\t"))))
     (t
      (setq word (tinypgp-newnym-read-word))

      (cond
       ;; ................................................... beg line ...
       ((and (not (ti::nil-p word))
             ;;   the "-" terminates word; because it is [+-] option,
             ;;   that's why we have to test separate words.
             (member word '("Nym" "Commands:")))
        (skip-chars-forward "^ \t\n")
        (skip-chars-forward " \t"))
       ;; ............................................... complete all ...
       ((and (ti::nil-p word)   ;User wrote [+-] and wants to complete
             (ti::char-in-list-case (preceding-char) '(?+ ?-)))
        (setq word (completing-read "Insert Command: " tbl))
        (if (not (ti::nil-p word))
            (insert word)))

       ;; ............................................... example show ...
       ((and (not (ti::nil-p word))
             (setq elt (assoc word tbl)) ;Full match
             ;; after word that is full match; on whitespace
             (ti::char-in-list-case (following-char) '(?\ ?\t ?\n)))
        (message (nth 2 elt)))

       ;; ........................................... full match; help ...
       ((and (not (ti::nil-p word))
             (setq elt (assoc word tbl))) ;Full match
        (message (nth 1 elt))
        (skip-chars-forward "^ \t\n"))

       ;; ........................................... partial complete ...
       ((and (not (ti::nil-p word))     ;Partial
             (setq elt (all-completions word tbl)))
        (cond
         ((eq 1 (length elt))           ;one match
          (skip-chars-forward "^ \t\n")
          (delete-backward-char (length word))
          (insert (car elt)))
         (t                             ;many completions
          (message (ti::list-to-string elt)))))

       ;; .............................................. nothing works ...
       (t
        ;; User is sitting on whitespace and nothing is nearby
        ;; "Nym-Commands:  "
        (message "Write [+-] before options. Complete with TAB.")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-nym-commands-goto ()
  "Goto Nym-Commands: forward or add that field if it does not exist."
  (interactive)
  (let* ((fld   "Nym-Commands: ")
         (point (if (re-search-forward fld nil t)
                    (match-end 0)
                  (save-excursion       ;Wrap
                    (ti::pmin)
                    (if (re-search-forward fld nil t)
                        (match-end 0))))))
    (if point
        (goto-char point)
      ;; No such field; add one. Put after From field.
      ;;
      ;; Config:
      ;; From:
      ;; Nym-Commands:
      ;;
      ;;
      (ti::mail-text-start 'move)
      (cond
       ((re-search-forward "From:")     (forward-line 1))
       ((re-search-forward "Config:")   (forward-line 1)))
      (insert fld "\n")
      (backward-char 1))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-public-key-kill ()
  "Kill Public-Key field."
  (interactive)
  (tinypgp-newnym-mode-public-key nil 'kill))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-public-key (key-id &optional kill)
  "Insert Public-Key field and PGP key block to the end.
If there already exist Public-Key tag, then insert pgp key block after it
by possibly deleting old pgp key block.

Input:
  KEY-ID    key-id matching public key
  KILL      if non-nil prefix arg, kill the public key block"
  (interactive
   (let* ((default (save-excursion
                     (ti::mail-text-start 'move)
                     ;;  find the From command field and suggest
                     ;;  inserting pgp key-id amtching it
                     ;;
                     (ti::mail-get-field "From" 'any)))
          ret)
     (unless current-prefix-arg         ;Don't ask if arg given
       (setq
        ret
        (read-from-minibuffer "Insert pgp key matching key-id: "
                              (if (not (ti::nil-p default))
                                  (ti::string-remove-whitespace default))))
       (if (ti::nil-p ret)
           (error "TinyPgp: Empty not accepted.")))

     (list ret current-prefix-arg)))
  (let* ((fld "Public-Key:")
         stat)
    (ti::save-with-marker-macro
      (ti::mail-text-start 'move)
      (setq stat (re-search-forward fld nil t))

      (cond
       ((and kill stat)
        (ti::buffer-kill-line))

       ((null kill)
        (if stat
            (forward-line 1)
          (ti::pmax)
          (insert fld "\n"))))

      (ti::mail-pgp-block-area-kill-forward 'pkey 'move)

      (when (null kill)
        (tinypgp-key-extract-to-point key-id 'raw)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-reply-block-kill (&optional insert remailer)
  "Kill Reply-Block or INSERT (or replace with) matching REMAILER."
  (interactive)
  (let* ((fld "Reply-Block:")
         stat)
    (ti::save-with-marker-macro
      (ti::mail-text-start 'move)
      (setq stat (re-search-forward fld nil t))

      (cond
       ((and insert (null stat))
        (ti::pmax)
        (insert fld "\n"))
       ((and insert
             stat
             (save-excursion            ;Previous reply block?
               (forward-line 1)         ;Peek next line
               (looking-at "::\n")))
        (forward-line 1)
        (delete-region (point) (point-max)))
       ((and (null insert) stat)
        (ti::buffer-kill-line)
        (if (looking-at "::\n")
            (delete-region (point) (point-max)))))

      (when insert
        (tinypgp-r-reply-block-insert  remailer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-mode-reply-block (remailer &optional verb)
  "Insert Reply-Block field and REMAILER block to the end.
If there already exist Reply-Block tag, then insert block after it
by possibly deleting old block.

Input:
  REMAILER  The reply block must have been created beforehand and
            it must be included in `tinypgp-:r-reply-block-table'
  VERB      Verbose messages."
  (interactive (list (tinypgp-ask-reply-block-remailer)))
  (ti::verb)
  (tinypgp-newnym-mode-reply-block-kill 'insert remailer)
  (if verb
      (message "Tinypgp: '%s' reply block inserted" remailer)))

;;}}}
;;{{{ newnym: misc, interactive(delete; create; toggle)

;;; ............................................ &r-newnym-interactive ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-default-set (completion-name)
  "Set default newnym server and account according to COMPLETION-NAME.
The name must be found from table `tinypgp-:r-newnym-default-account-table'."
  (interactive
   (list
    (if (null tinypgp-:r-newnym-default-account-table)
        (error "TinyPgp: tinypgp-:r-newnym-default-account-table not defined.")
      (completing-read
       "Default Newnym selection: "
       tinypgp-:r-newnym-default-account-table
       nil
       'match))))
  (let* ((sym 'tinypgp-:r-newnym-default-account-table)
         (elt (assoc completion-name (symbol-value sym))))
    (when elt
      (put sym 'default-completion  completion-name)
      (put sym 'default-server   (nth 1 elt))
      (put sym 'original-server  (nth 1 elt))
      (put sym 'default-account  (nth 2 elt))
      (put sym 'original-account (nth 2 elt))
      (if (interactive-p)
          (message "TinyPgp: Default newnym server and account now: %s %s"
                   (nth 1 elt) (nth 2 elt)  ))
      (tinypgp-update-modeline)
      elt)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-default-toggle (&optional arg verb)
  "Toggle setting and resetting default newnym account.
Set and restore variable's `tinypgp-:r-newnym-default-account-table' properties
'default-server and 'default-account.

ARG behaves like mode arg.

  nil  toggle
  0    set values to nil
  1    restore values.
  9    Force re-reading values now. You have to call this if you chnage the
       contents of the values during session manually.

VERB allows verbose messages."
  (interactive "P")

  (if (null tinypgp-:r-newnym-default-account-table)
      (error "TinyPgp: tinypgp-:r-newnym-default-account-table not defined."))

  (let* ((sym   'tinypgp-:r-newnym-default-account-table)
         (srv   (get sym 'default-server))
         (acc   (get sym 'default-account))
         (force (eq arg 9))
         msg)
    (ti::verb)
    ;; Not recorded? Record original value
    ;;
    (when (or force (null (get sym 'original-server)))
      (put sym 'original-server srv))

    (when (or force (null (get sym 'original-account)))
      (put sym 'original-account acc))

    (cond
     ((memq arg '(9))
      (setq msg (format "Default newnym parameters updated: %s %s"
                        srv acc)))

     ((memq arg '(0 -1))
      (put sym 'default-server  nil)
      (put sym 'default-account nil)
      (setq msg (format "Default newnym parameters off.")))

     (t                                 ;Toggle
      (cond
       (srv
        (put sym 'default-server  nil)
        (put sym 'default-account nil))
       (t
        (put sym 'default-server  (get sym 'original-server))
        (put sym 'default-account (get sym 'original-account))))
      (setq msg (format "Default newnym server and account now: %s %s"
                        (or (get sym 'original-server)  "nil")
                        (or (get sym 'original-account) "nil")))))

    (tinypgp-update-modeline)
    (if verb
        (message msg))
    msg))

;;; ----------------------------------------------------------------------
;;;
(tinypgp-newnym-sendmail-fmacro
 tinypgp-newnym-get-pgp-key
 "Get PGP key via email from remailer."
 "remailer-key" "Send PGP key"  "PGP key get")

(tinypgp-newnym-sendmail-fmacro
 tinypgp-newnym-get-used-list
 "Get list of used 'newnym' account names."
 "list" "Send used account list" "Used")

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-help-verbose (&optional arg)
  "Call `tinypgp-nymserver-help' as interactive would with ARG."
  (let* ((a (tinypgp-newnym-help-i-args arg)))
    (tinypgp-newnym-help (nth 0 a) (nth 1 a))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-help-i-args (&optional arg)
  "Ask arrgs for `tinypgp-newnym-help'. ARG is prefix arg."
  (let* ((list   (tinypgp-newnym-list)))
    (when (or (not (stringp tinypgp-:r-newnym-help-file))
              (null (file-exists-p tinypgp-:r-newnym-help-file))
              current-prefix-arg)
      (if (null list)
          (error "\
TinyPgp: No 'newnym' type remailers in `tinypgp-:r-levien-table'."))

      (list
       'mail-req
       (tinypgp-newnym-ask-server "Send help request to newnym: ")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-help (&optional mail-req nym-alias-name verb)
  "Print newnym remailer help or send the help request via mail.

Input:
  MAIL-REQ          send mail request [current-prefix-arg]
  NYM-ALIAS-NAME    from where to ask the help file.
  VERB              verbose messages"
  (interactive (tinypgp-newnym-help-i-args current-prefix-arg))
  (let* ((file  tinypgp-:r-newnym-help-file)
         (elt   (if nym-alias-name
                    (assoc nym-alias-name tinypgp-:r-levien-table)))
         email)
    (ti::verb)
    (cond
     (mail-req
      (if (null elt)
          (error "TinyPgp: Cannot find ELT for '%s'" nym-alias-name))

      ;; Set address to "help@..."
      (setq email (nth 1 elt))

      (if (null (string-match "^[^@]+" email))
          (error "TinyPgp Internal error. Call \\[tinypgp-r-init]"))

      (setq email (ti::replace-match 0 "help" email))

      (ti::mail-sendmail-macro email "help" 'send (insert "help\n"))
      (if verb
          (message
           "Email request sent to '%s'.%s"
           email
           (if file ""
             "Update tinypgp-:r-newnym-help-file when you get answer."))))

     ((and file
           (file-exists-p file))
      (pop-to-buffer (find-file-noselect file)))

     (t
      (error "TinyPgp: Don't know what to do. %s %s "
             mail-req nym-alias-name)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-config-insert
  (server nym-name &optional command pgp-key remailer)
  "Insert Config request to mail buffer. Mail body is supposed to be empty.

Input:

  SERVER        newnym server alias name, like 'weasel'
  NYM-NAME      account name i the newnym server
  COMMAND       commands to send
  PGP-KEY       PGP key block.
                if 'string' insert as is
                if buffer pointer, insert buffer contents
                if symbol; call pgp to find key from keyrings matching symbol.

  REMAILER      Reply block
                if 'string', then insert as is
                if buffer pointer, then insert buffer content.
                if symbol, it must be remailer alias name to use for
                reply block. The remailer reply block is then
                inserted from file pointed by `tinypgp-:r-reply-block-table'."
  (interactive
   (list
    (tinypgp-newnym-ask-server)
    (read-from-minibuffer "Nym account name: ")))
  (tinypgp-r-init-maybe)
  (let* ((fid   "tinypgp-newnym-config-insert:")
         list)

    (tinypgpd fid "in:" server nym-name  command pgp-key remailer)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... compose request ...
    (tinypgp-r-server-macro server "config"

                            (ti::mail-kill-field "^To:" email)
                            (ti::mail-text-start 'move)

                            (insert
                             "Config:\n"
                             "From: " (or nym-name "") "\n"
                             "Nym-Commands: "  (or command "") "\n")

                            (when pgp-key
                              (insert "Public-Key:\n")
                              (cond
                               ((bufferp pgp-key) (insert-buffer pgp-key))
                               ((stringp pgp-key) (insert pgp-key))

                               ((symbolp pgp-key)
                                (tinypgp-key-extract-to-point (symbol-name pgp-key) 'raw)
                                ;; check that PGP public key definition contains <> email
                                ;; to this host.
                                ;;
                                (with-current-buffer tinypgp-:buffer-tmp-shell
                                  (setq list (ti::mail-email-find-region))
                                  (when (or (null list)
                                            (not (string-match
                                                  (replace-regexp-in-string ".*@" "" email 0)
                                                  ;;  Take first email from key-id
                                                  (or (car list)
                                                      (progn
                                                        (pop-to-buffer (current-buffer))
                                                        (error "\
TinyPgp: no email found from pgp key?"))))))
                                    (pop-to-buffer (current-buffer))
                                    (error "TinyPgp: PGP user ID '%s' does not refer to domain '%s'"
                                           pgp-key email))))

                               ((error "TinyPgp: Oops, wrong argument..."))))

                            (when remailer
                              (insert "Reply-Block:\n")
                              (cond
                               ((bufferp remailer) (insert-buffer remailer))
                               ((stringp remailer) (insert remailer))
                               ((symbolp remailer)
                                (tinypgp-r-reply-block-insert (symbol-name remailer)))
                               ((error "TinyPgp: Oops, wrong argument...")))
                              (insert "\n**\n")
                              (ti::pmax)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-config-sendmail-template (server account &optional verb)
  "Create mail buffer and inset newnym' configuration template.
Input:

  SERVER    newnym server
  ACCOUNT   login account
  VERB      verbose, show buffer. Interactive call sets this.

Return:
  buffer pointer"
  (interactive
   (list
    (tinypgp-newnym-ask-server)
    (tinypgp-newnym-ask-account)))
  (tinypgp-r-init-maybe)
  (let* (buffer)
    (ti::verb)
    (with-current-buffer (setq buffer
                               (tinypgp-r-sendmail-create-buffer
                                tinypgp-:buffer-newnym
                                "Config request"))
      (tinypgp-newnym-config-insert server account)
      (turn-on-tinypgp-mode)
      (turn-on-tinypgp-newnym-mode)
      (tinypgp-newnym-mode-nym-commands-goto))
    (when verb
      (switch-to-buffer buffer))
    buffer))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgpg-newnym-account-request
  (server account cmd &optional pgp-key remailer send)
  "Set up all necessary things to send command to newnym server account.

Input:

  SERVER    newnym server alias
  ACCOUNT   newnym account name
  CMD       Nym-Comands's field content

  PGP-KEY   If t, then inser pgp-key matching ACCOUNT

            It symbol but not t, Email address string which
            matches the key-id from PGP key -- that key is sent to newnym.

  REMAILER  Use this remailer's reply block. You must have created this
            beforehand with `tinypgp-r-reply-block-basic' and stored
            it to file pointed by `tinypgp-:r-reply-block-table'.
  SEND      if non-nil, encrypt and send the message.

Return:
  mail buffer pointer if SEND is nil"
  (let* ((fid   "tinypgpg-newnym-account-request: ")
         buffer
         to)
    (tinypgpd fid "ARGS" server account (current-buffer))
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... mail buffer ...
    (tinypgp-r-server-macro server account
                            (ti::mail-sendmail-pure-env-macro
                             (setq buffer (tinypgp-r-sendmail-create-buffer
                                           tinypgp-:buffer-newnym
                                           "Account request"))
                             ;;  The window excursion is needed so that nothing fancy happens
                             ;;  when we send mail. User doesn't want his windows changed
                             ;;
                             (save-window-excursion
                               (save-excursion
                                 (set-buffer buffer)
                                 (tinypgpd fid server account email info)
                                 (if (eq t pgp-key)
                                     (setq pgp-key (make-symbol email)))

                                 (tinypgp-newnym-config-insert server account cmd pgp-key remailer)

                                 (if (null send)
                                     buffer
                                   (make-local-variable 'tinypgp-:auto-action-table)
                                   (setq tinypgp-:auto-action-table nil)
                                   (ti::mail-sendmail-reset-send-hooks)

                                   (tinypgp-save-state-macro
                                    (setq tinypgp-:user-now email)
                                    (tinypgp-password-set (format "Newnym Encrypt password: "))
                                    (setq to  (mail-fetch-field "to"))
                                    (tinypgp-encrypt-mail (ti::string-remove-whitespace to) nil))
                                   (mail-send-and-exit nil))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-delete (server account &optional verb)
  "Send to newbyn SERVER a ACCOUNT delete request. VERB."
  (interactive
   (tinypgp-newnym-ask-srv-acc
    "Are you sure you want to send DELETE request? "))
  (ti::verb)
  (tinypgp-r-init-maybe)
  (pop-to-buffer
   (tinypgpg-newnym-account-request server account "delete" nil nil))
  (if verb (message "Newnym Delete request sent.")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-create-i-args ()
  "Ask arguments to `tinypgp-newnym-create'."
  (let* ((site (get 'tinypgp-:r-newnym-default-account-table 'default-server))
         desc
         remailer
         srv-account
         login)
    (tinypgp-r-init-maybe)
    (message "You should check free Nym login names first...ok?")
    (sit-for 2)

    (setq srv-account (tinypgp-newnym-ask-srv-acc))

    (if (ti::nil-p
         (setq
          login
          (read-from-minibuffer
           (format
            "[%s] Create Nym Login: "
            site))))
        (error "Abort."))

    (if (ti::nil-p
         (setq
          desc
          (read-from-minibuffer
           (format
            "[%s] Describe Nym login name: "
            site))))
        (error "TinyPgp: Empty not accepted."))

    (setq remailer
          (tinypgp-ask-reply-block-remailer
           (format
            "[%s] Select Reply block of remailer: "
            site)))

    (list (nth 0 srv-account)
          login
          desc
          remailer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-create (server account desc remailer &optional verb)
  "Send to newbyn SERVER a ACCOUNT delete request.

Note:

  Before you call this function, make sure you have created new key
  with 'pgp -kg' and that its key-id line contain email address
  <yournym@remail.domain.com>

Input:

  SERVER    newnym server (alias) name
  ACCOUNT   account name in newnym
  DESC      account description
  REMAILER  remailer's reply block to submit to newnym server.
  VERB      Verbose messages. Shows the buffer and turns on
            `tinypgp-mode' and `tinypgp-newnym-mode`."
  (interactive
   (progn
     (tinypgpd "tinypgp-newnym-create: INTERACTIVE")

     (or
      (y-or-n-p
       "\
Do have created the ncessary PGP keys for newnym account? (see manual)")
      ;; Umph; again some impatient user selected this choice without readin
      ;; the newnym documentation....
      (error "TinyPgp: Please read the newnym remailer manual first."))
     (tinypgp-newnym-create-i-args)))
  (tinypgpd "tinypgp-newnym-create: in" server account desc remailer verb)
  (tinypgp-r-init-maybe)
  (let* ()
    (ti::verb)
    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... mail buffer ...
    (with-current-buffer (tinypgpg-newnym-account-request
                          server account
                          (format "create +acksend +fingerkey name=\"%s\"" desc)
                          t
                          (make-symbol remailer))

      (when verb
        (turn-on-tinypgp-mode)
        (switch-to-buffer (current-buffer)) ;Now visible to user
        (ti::mail-text-start 'move)

        (turn-on-tinypgp-newnym-mode)
        (ti::mail-mime-turn-off-mode)
        (tinypgp-email-substitution-toggle 0) ;; Config request
;;;     (tinypgp-auto-action-toggle 0)       ;; No auto action here by default

        ;;  We havae to "account", bwecause 1pass needs to be signed
        ;;  with the "account" key. Store the active pgp user information
        ;;  to local variables, so that we can restore the user in mail send
        ;;  hook
        ;;

        (make-local-variable 'tinypgp-pgp-user-original)
        (make-local-variable 'tinypgp-pgp-user-now)

        (defconst tinypgp-pgp-user-original   tinypgp-:user-now)
        (defconst tinypgp-pgp-user-now        account)
        (setq     tinypgp-:user-now           account)

        ;;  Warn about this change, because user may kill the buffer
        ;;  and the active pgp user still stays "nym" login.
        ;;
        (message "Active PGP user changed to: %s" account) (sleep-for 1.5)

        (ti::read-char-safe-until
         (substitute-command-keys
          (concat
           "Check all; do 1pass Encrypt-Sign with NymKey: "
           "\\[tinypgp-encrypt-mail-sign] [press to continue]")))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-newnym-post (&optional server account verb)
  "Convert current message into Nym post ueing SERVER and ACCOUNT. VERB.
The message can be newsgroup post or regular email."
  (interactive
   (progn
     (tinypgp-r-init-maybe)
     (list
      (tinypgp-newnym-ask-server)
      (tinypgp-newnym-ask-account))))
  (tinypgp-r-init-maybe)
  (let* ((subj      (mail-fetch-field    "Subject"))
         (news      (ti::mail-news-buffer-p))
         (hdr-blk   "")
         hash-headers to
         hdr
         ret)
    (ti::verb)

    (tinypgp-r-server-macro server "send"
                            (cond
                             (news
                              (setq ret (tinypgp-r-post-usenet 'newnym))
                              (ti::mail-kill-field "To")
                              (ti::mail-kill-field "Subject")
                              (ti::mail-kill-field "From")
                              ;;  Rest of the headers without "To" field.
                              ;;
                              (setq hdr (buffer-substring (point-min) (ti::mail-hmax)))

                              ;;  Now send to newnym server
                              ;;
                              (ti::pmin)
                              (insert "To: " email "\n"
                                      "Subject: message\n")

                              (ti::mail-text-start 'move)
                              (insert "From: " account "\n"
                                      "To: " (or (eval tinypgp-:r-newnym-mail2news-address)
                                                 (error "TinyPgp: no newnym mail2news gateway?"))
                                      "\n"
                                      "subject: " subj "\n")

                              (insert hdr)
                              (insert "Newsgroups: " (ti::list-to-string (nth 1 ret)) "\n\n")
                              (ti::mail-kill-field-in-body '("fcc" "gcc")))
                             (t
                              (setq to (mail-fetch-field       "to"))
                              (tinypgp-header-kill)
                              ;;  Save all headers because they are inserted into body
                              ;;
                              (if (setq hash-headers
                                        (tinypgp-header-move-to-body 'move-to-body 'no-ins))
                                  (setq hdr-blk      (mapconcat 'concat hash-headers "")))
                              (ti::mail-kill-field "To" email)
                              (ti::pmin) (insert "To: " email "\n")

                              (ti::mail-text-start 'move)
                              (insert "From: " account "\n"
                                      "To: " to "\n"
                                      hdr-blk))))
    (when verb
      (unless tinypgp-newnym-mode (turn-on-tinypgp-newnym-mode))
      ;; (tinypgp-auto-action-update-modeline)
      (message
       (substitute-command-keys
        (concat
         "Nym-Commands can be set per message basis, press "
         "\\[tinypgp-newnym-mode-nym-commands-goto] and "
         "\\[tinypgp-newnym-mode-electric-tab]"))))))

;;}}}
;;{{{ newnym: interactive requests

;;; ------------------------------------------------------ &newnym-req ---
;;;
(eval-and-compile
  (defun tinypgp-newnym-req-fmacro-1 (func req)
    "Use `tinypgp-newnym-req-fmacro' instead. See FUNC REQ there."
    (let* ((sym (intern (symbol-name (` (, func))))))
      (`
       (defun (, sym)  (server account &optional plus verb)
         "Send to newnym SERVER ACCOUNT an minus(default) or PLUS request. VERB."
         (interactive (tinypgp-newnym-ask-srv-acc-arg))
         (ti::verb)
         (setq plus (concat (if plus "+" "-") (, req)))
         (tinypgpg-newnym-account-request
          server account plus  nil nil 'send)
         (tinypgp-newnym-file-stamp server account)
         (when verb
           (message "[%s] Newnym request sent: %s" server plus)
           ;; If mouse pressed, don't wipe message immediately
           (sleep-for 1)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-newnym-req-fmacro 'lisp-indent-function 0)
(defmacro tinypgp-newnym-req-fmacro (func req)
  "Create interactive function that sends newnym request.
Input:

  FUNC  Created function name
  REQ   request to send; without +- option at front."
  (` (, (tinypgp-newnym-req-fmacro-1 func req))))

;;; ----------------------------------------------------------------------
;;; We have to tell the autoloads by hand; because the functions are
;;; created by separate macro.
;;;
;;;###autoload (autoload 'tinypgp-newnym-req-acksend    "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-sigsend    "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-cryptrecv  "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-fixedsize  "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-disable    "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-fingerkey  "tinypgp" "" t)
;;;###autoload (autoload 'tinypgp-newnym-req-nobcc      "tinypgp" "" t)

(tinypgp-newnym-req-fmacro tinypgp-newnym-req-acksend   "acksend")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-sigsend   "sigsend")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-cryptrecv "cryptrecv")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-fixedsize "fixedsize")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-disable   "disable")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-fingerkey "fingerkey")
(tinypgp-newnym-req-fmacro tinypgp-newnym-req-nobcc     "nobcc")

;;}}}

;;{{{ Nymserver: misc

;;; .................................................. &nymserver-misc ...
;;; anon.nymserver.com successor of anon.penet.fi

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-mail-p (&optional alias)
  "Check if there is Anon X-headers in the buffers. ALIAS."
  (setq alias (car (car tinypgp-:nymserver-table)))
  (and
   (ti::re-search-check "^X-Anon-Password\\|^X-Anon-To")
   (ti::re-search-check (format "^To:.*%s" alias))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-nymserver-service-elt (alias)
  "Return service entries or call error is no such ALIAS."
  (or (assoc alias tinypgp-:nymserver-table)
      (error "TinyPgp: No server alias '%s'" alias)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-nymserver-mailto (alias)
  "Return address where user can send mail so that it gets anynymized. ALIAS."
  (or (nth 3 (assoc alias tinypgp-:nymserver-table))
      (error "TinyPgp: No post email address")))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-nymserver-address (string alias)
  "Return nymserver email address prepended with STRING as account name. ALIAS.

Return:
  STRING@NYMSERVER-ADDRESS"
  (concat
   string
   (ti::string-match
    "\\(@.*\\)" 1
    (nth 2 (assoc alias tinypgp-:nymserver-table)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-ask (&optional msg)
  "Ask server alias name with MSG."
  (if nil                               ;disabled now
      (completing-read
       (or msg "Use pent server: ")
       (ti::list-to-assoc-menu (mapcar 'car tinypgp-:nymserver-table))
       nil
       'match)
    ;; 1997-02-13 Jari aalto
    ;; - We don't support other nymserver accounts currently
    ;;
    (car (car tinypgp-:nymserver-table))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-password (alias)
  "Return password or nil for ALIAS."
  (let* ((elt      (assoc alias tinypgp-:nymserver-account-table))
         (pass     (nth 2 elt)))
    pass))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-sendmail (action alias &optional verb arg1 arg2)
  "Send ACTION mail to nymserver ALIAS.
Mail will be encrypted if `tinypgp-:nymserver-request-encrypt' is non-nil.
See variables documentation for more detailed usage.

Input:
  ACTION ALIAS VERB ARG1 ARG2

Note:
  following variables are bound to nil to prevent any interference when
  sending mail commands.

  `mail-archive-file-name'
  `mail-default-headers'
  `mail-mode-hook'
  `mail-setup-hook'"
  (let* ((elt       (assoc alias tinypgp-:nymserver-account-table))
         (account   (or (nth 1 elt)
                        (error "TinyPgp: No account")))
         (pass      (or (nth 2 elt)
                        (error "TinyPgp: No account password")))
         (my-from   (nth 4 elt))

         (fld1      "X-Anon-Password: ")
         (fld2      "X-Anon-Subject: ")
         (encrypt   tinypgp-:nymserver-request-encrypt)

         ;; Make sure email substitution mode is on when we send
         ;; mail to anon server. User may have forgotten it off

         (tinypgp-:read-email-after-hook
          (or (get 'tinypgp-:read-email-after-hook 'original)

              ;; if the above fails, that means that the 'original
              ;; property is not used yet and not available.

              tinypgp-:read-email-after-hook))

         (email     (tinypgp-nymserver-address (symbol-name action) alias))
         (enc-key   (car (tinypgp-key-id-conversion email)))
         subject
         buffer)

    (tinypgpd "tinypgp-nymserver-sendmail in: " action alias verb arg1 arg2)

    (save-window-excursion
      (cond
       ((memq action '(finger ping remove help abuse))
        (ti::mail-sendmail-macro  email "None" 'send
                                  (insert fld2 (or arg1 "No subject data") "\n")

                                  ;; This field will confuse Nymserver server. Remove it
                                  ;;
                                  (ti::mail-kill-field "Reply-To")
                                  (if my-from (ti::mail-add-field "From"  my-from "To"))

                                  (if encrypt (tinypgp-encrypt-mail-find-keyring enc-key))))
;;;       (pop-to-buffer (current-buffer)) (ti::d! 101)

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  upload pgp key . .
       ((eq action 'newpgp)
        (unless arg1                    ;Nor a remove request?
          (with-current-buffer (setq buffer (tinypgp-ti::temp-buffer 'finger))
            (erase-buffer)
            (tinypgp-key-extract-to-point account)

            (ti::pmin)
            (if (re-search-forward "matching keys found" nil t)
                (error "TinyPgp: [%s' didn't match exactly." arg1))))

        (ti::mail-sendmail-macro  email "No subject" 'send
                                  (if my-from (ti::mail-add-field "From"  my-from "To"))

                                  (insert fld1 pass    "\n")

;;;       (pop-to-buffer (current-buffer)) (ti::d! 10)

                                  (if (string= "remove" arg1)
                                      (insert fld2 "remove" "\n")
                                    (insert-buffer buffer))

                                  (if encrypt
                                      (tinypgp-encrypt-mail-find-keyring enc-key))))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. plan and sig . .
       ((memq action '(newplan newsig))
        (ti::mail-sendmail-macro  email "No subject" 'send
                                  (insert fld1 pass "\n")

                                  (ti::mail-kill-field "Reply-To")
                                  (if my-from (ti::mail-add-field "From"  my-from "To"))

                                  (if (string= "remove" arg1)
                                      (insert fld2 "remove" "\n")
                                    (insert-file arg1))

                                  (if encrypt
                                      (tinypgp-encrypt-mail-find-keyring enc-key))))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  flags . .
       ((memq action '(paranoid newalias nick
                                newpassword vacation noarchive
                                newaddress setnon
                                pgpencrypt pgpsign sendmix))
        (setq subject (or arg1 "No subject")) ;this is the new alias name
        (ti::mail-sendmail-macro  email "No subject" 'send
                                  (insert fld1 pass "\n"
                                          fld2 subject "\n")

                                  (ti::mail-kill-field "Reply-To")
                                  (if my-from (ti::mail-add-field "From"  my-from "To"))

;;;       (pop-to-buffer (current-buffer)) (ti::d! 10)
                                  (if encrypt (tinypgp-encrypt-mail-find-keyring enc-key))))
;;;       (pop-to-buffer (current-buffer)) (ti::d! 10)
       (t
        (error "TinyPgp: unknown action '%s'" action)))
      (if verb (message "Nymserver: %s request sent."
                        (capitalize (symbol-name action)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-create-1 (email)
  "Send EMAIL to create account."
  (interactive)
  (ti::read-char-safe-until
   "[press] Store received account info into tinypgp-:nymserver-account-table.")
  (ti::mail-sendmail-macro email "No subject" 'send))

;;}}}
;;{{{ Nymserver: interactive

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-post (alias &optional verb)
  "Convert message so that it can be posted to through nymserver. ALIAS VERB."
  (interactive
   (list
    (progn
      (tinypgp-nymserver-i-enable)
      (tinypgp-nymserver-ask))))

  (let* ((srv    (tinypgp-nymserver-service-elt alias))
         (mailto (or (nth 3 srv)
                     (error "TinyPgp: No server mailto address.")))
         (grp-limit (nth 4 srv))

         (elt   (or (assoc alias tinypgp-:nymserver-account-table)
                    (error "TinyPgp: Unknown server %s" alias)))
;;;      (email (or (nth 1 elt)
;;;                 (error "No account email")))

         (pass  (or (nth 2 elt)
                    (error "TinyPgp: No account password")))
         (name      (nth 3 elt))
         (my-from   (nth 4 elt))

         (fld1      "X-Anon-Password: ")
         (fld2      "X-Anon-To: ")
         (fld3      "X-Anon-Name: ")
         (fld4      "X-Anon-Subject: ")
;;;      (fld-ref   "X-Anon-references: ") references
         to subject
         hlist
         grp)

    (ti::verb)

    (unless (ti::mail-mail-p)
      (error "TinyPgp: This is not email buffer."))

    (setq to        (mail-fetch-field    "to")
          subject   (mail-fetch-field    "subject")
          hlist     (delete 'newsgroups (ti::mail-required-headers)))

    (when   (ti::nil-p subject)
      (error "TinyPgp: No subject. Aborted"))

    (ti::save-with-marker-macro
      (ti::mail-text-start 'move)
      (if (looking-at "X-Anon")
          (if verb
              (message "Already in anon post format."))

        (cond
         ((not (ti::nil-p to))          ;regular email message

          ;;  tinymail.el / we have to add 2 spaces to the beginning of field
          ;;  so that CC tracking goes off.
          ;;
          (ti::mail-kill-field "to" (concat "  " mailto))
          (if my-from (ti::mail-add-field "From"  my-from "To"))

          (insert fld1 pass "\n"
                  fld2 to   "\n"
                  (if name (concat fld3 name "\n") "")
                  fld4
                  subject "\n")
          (tinypgp-update-modeline))

         ((not (ti::nil-p (setq grp (mail-fetch-field "newsgroups"))))
          (if (and grp-limit
                   (> (count-char-in-string ?, grp) grp-limit))
              (error "\
TinyPgp: Too many newsgroups, only %d allowed" grp-limit))

;;;       (setq references (mail-fetch-field "references"))
          (push 'in-reply-to  hlist )
          (ti::mail-kill-non-rfc-fields hlist)

          (ti::mail-add-field "To"  mailto)
          (when my-from
            ;; it may be possible that this field is there already,
            ;; kill it first
            ;;
            (ti::mail-kill-field "^From:")
            (ti::mail-add-field "From"  my-from  "To"))

          (ti::mail-text-start 'move)
          (insert fld1 pass "\n"
                  fld2 grp  "\n"
                  (if name (concat fld3 name "\n") "")
                  fld4
                  subject "\n")
          (tinypgp-update-modeline))

         (t
          (if verb
              (error "\
TinyPgp: Don't know what to do: To or Newsgroup field empty."))))
        (ti::mail-kill-field "subject" " None")

        ;;  Add 'cutmarks' so that all the rest of the text are
        ;;  ripped.
        ;;
        (ti::pmax)
        (if (bolp)
            (insert "--")
          (insert "\n--"))))

    (run-hooks 'tinypgp-:nymserver-post-hook)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-send ()
  "Handle sending mail addressed to Nymserver.
This function is called after C -c C -c to sned the mail.
If there are no multiple recipients, this function does nothing

Return:
  nil
  t"
  (let* ((fid      "tinypgp-nymserver-send: ")
         (email    "anon@anon.nymserver.com")
         (to        (mail-fetch-field    "to"))
         (cc        (mail-fetch-field    "cc"))
         (fcc       (mail-fetch-field    "fcc"))
         (subject   (mail-fetch-field    "subject"))
         (elist     (delete
                     email
                     (append
                      (ti::mail-email-from-string to)
                      (if cc (ti::mail-email-from-string cc)))))

         (enc-key   (car (tinypgp-key-id-conversion email)))
         (encrypt   tinypgp-:nymserver-request-encrypt)

         (len       (length elist))
;;;      (i         0)
         (send-flag t)

         message-body
         ret)

    ;;  - Nymserver doesn't accept CC or many addresses in To field,
    ;;    it can only have one X-anon-To destination.
    ;;  - What we do here is, that we copy the message and send it
    ;;    individually to each destination
    ;;  - We need confirmation for this

    (when (or (> len 1)
              ;;  If there is CC, then automatically suppose multiple
              ;;  recipients. The To field is already in X-Anon-To
              ;;  So this CC makes at least 2 recipients.
              ;;
              cc)
      (tinypgpd fid subject to cc fcc elist)

      ;;  The X-Anon-To is inside PGP envelope, we can't use this message
      ;;  body to CC it to others.
      ;;
      (if (ti::mail-pgp-encrypted-p 'double-check)
          (error "\
TinyPgp: You have CC in Nymserver mail. Can't process encrypted message."))

      (if (null
           (y-or-n-p
            (format
             "CC %d: You have multiple anon recipients, are you sure? "
             len)))
          (error "Abort.")
        (setq message-body
              (buffer-substring (ti::mail-text-start) (point-max)))
        (dolist (elt elist)
          (ti::mail-sendmail-macro email "None" send-flag
                                   (insert message-body)
                                   (pop-to-buffer (current-buffer))
                                   (ti::pmin)
                                   (re-search-forward "X-Anon-To:\\(.*\\)")
                                   (ti::replace-match 1 (concat " " elt))
                                   (pop-to-buffer (current-buffer))
;;;         (incf  i) (message "Sending-quick %d/%d %s" i len elt)
                                   (if encrypt
                                       (tinypgp-encrypt-mail-find-keyring enc-key))))
        (ti::read-char-safe-until
         "[press]Anon CC copies sent, now sending this mail buffer.")
        (ti::mail-kill-field "cc")
        (if encrypt
            (tinypgp-encrypt-mail-find-keyring enc-key))
        (setq ret t)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-create (alias)
  "Send account create request. ALIAS."
  (interactive (list (tinypgp-nymserver-ask)))
  (let ((srv (tinypgp-nymserver-service-elt alias)))
    (if (yes-or-no-p
         "Are you absolutely sure you want to send 'create' request ")
        (funcall (nth 1 srv) (nth 1 srv)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-remove (alias)
  "Remove your anonymous account. ALIAS."
  (interactive
   (list
    (progn
      (tinypgp-nymserver-i-enable)
      (tinypgp-nymserver-ask))))
  (if (yes-or-no-p
       "Are you absolutely sure you want to terminate anonymous account ")
      (tinypgp-nymserver-sendmail 'remove alias (interactive-p))))

;;; ----------------------------------------------------------------------
;;; Hm. This function does not have paramaeter 'alias'.
;;; So it's not general purpose for other accounts
;;; #todo: Should rethink it sometime.
;;;
(defun tinypgp-nymserver-finger (account)
  "Finger account's email address for its configuration.
If ACCOUNT is in format vanity.an@site or vanity.na@site.com, it is converted
into vanity@site.com before sending finger request."
  (interactive
   (progn
;;;     (tinypgp-nymserver-i-enable)
     (let* ((elt            (assoc (tinypgp-nymserver-ask)
                                   tinypgp-:nymserver-account-table))
            (from           (car-safe
                             (ti::mail-email-from-string
                              (or (mail-fetch-field "from") ""))))
            (account        (nth 1 elt))
            (list           (if account
                                (ti::list-to-assoc-menu (list account)))))
       ;;  If user has reveived mail from anNNN@anon.nymserver.com
       ;;  Then we offer to finger that account too
       ;;
       (when (and from (string-match "an[0-9]@\\|\\.[an][na]@" from))
         (setq list (ti::list-to-assoc-menu
                     (if account
                         (list from account)
                       (list from))))
         (setq account from))

       (list
        (completing-read
         "Finger nymserver account [give email address]: "
         list
         nil
         nil
         account)))))

  ;;  Use may press <empty> RET in completing-read

  (if (not (string-match "@" account))
      (error "TinyPgp: Need email address."))

  ;; silent converion to 'an' format

  (setq account (ti::mail-nymserver-email-convert account))

  (tinypgp-nymserver-sendmail
   'finger (tinypgp-nymserver-ask) (interactive-p) account))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-abuse (alias)
  "Send ABUSE request. ALIAS."
  (interactive (list (tinypgp-nymserver-ask)))
  (let* ((buffer "*mail-nymserver-abuse*"))
    (ti::kill-buffer-safe buffer)
    (when (y-or-n-p "Nymserver: Are you sure you want to report ABUSE? ")
      (ti::mail-sendmail-macro
       (tinypgp-nymserver-address "abuse" alias)
       "ABUSE"
       nil
       (rename-buffer buffer)
       (pop-to-buffer (current-buffer))
       (message "Write message and possibly encrypt it.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-ping (alias)
  "Send Ping request. ALIAS.
In order to send ping, you have to be sending
mail FROM AN ACCOUNT WHERE YOU SENT the create command. You can't send ping
from any other location."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'ping alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-paranoid (alias)
  "Toggle paranoid setting. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'paranoid alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-vacation (alias)
  "Toggle vacation setting. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'vacation alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-noarchive (alias)
  "Toggle USENET achive setting. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'noarchive alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-setnon (alias)
  "Toggle anNNN/naNNN mode when you get private mail. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'setnon alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-newplan (alias file)
  "ALIAS. Upload plan FILE. If file is 'remove' then remove plan."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (if (y-or-n-p "y = upload .plan, n = remove plan" )
          (call-interactively
           '(lambda (arg) (interactive "fNymserver plan file: ") arg))
        "remove"))))
  (tinypgp-nymserver-sendmail 'newplan alias (interactive-p) file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-newsig (alias file)
  "ALIAS. Upload signature FILE. If file is 'remove' then remove signature."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (if (y-or-n-p "y = upload .signature, n = remove plan" )
          (call-interactively
           '(lambda (arg) (interactive "fNymserver signature file: ") arg))
        "remove"))))
  (tinypgp-nymserver-sendmail 'newsig alias (interactive-p) file))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-newaddress (alias new)
  "ALIAS. Change your mailbox address.
You must be mailing from the NEW ADDRESS currently."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (read-from-minibuffer
       "[You must be in NEW site now] Your old address: "))))
  (if (ti::nil-p new)                  ;User may have pressed ENTER...
      (error "TinyPgp: No address."))
  (tinypgp-nymserver-sendmail 'newaddress alias (interactive-p) new))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-newalias (alias name)
  "ALIAS NAME. Change you anNNN@ account to NEWALIAS@."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (read-from-minibuffer
       "newalias request; vanity alias [word]: "))))
  (if (or (< (length name) 3)
          (> (length name) 15))
      (error "TinyPgp: Invalid string size [3-15]; %s has %d characters."
             name (length name)))
  (tinypgp-nymserver-sendmail 'newalias alias (interactive-p) name))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-nickname (alias name)
  "ALIAS. Change you nick NAME that appears in anon post From field."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (read-from-minibuffer "Nickname [string or word 'remove']: "))))
  (tinypgp-nymserver-sendmail 'nick alias (interactive-p) name))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-newpassword (alias password)
  "ALIAS. Change your PASSWORD."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (ti::compat-read-password "New nymserver password: "))))
  (tinypgp-nymserver-sendmail 'newpassword alias nil password)
  (ti::read-char-safe-until
   "Update your password _now_ to tinypgp-:nymserver-account-table"))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-pgp-upload (alias &optional remove)
  "ALIAS. Upload or REMOVE pgp key. Before you call this commaand note:

o You must have created the PGP public key for your Nymserver account.
o You must have defined the `tinypgp-:nymserver-account-table'; the key
  uploaded must have the email address.
o If you change your vanity name, remember to start all over(New key,
  and update table)"
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list
      (tinypgp-nymserver-ask)
      (not (y-or-n-p
            "Y = upload your PGP key to Anon account [N = remove] ")))))
  (tinypgp-nymserver-sendmail 'newpgp alias (interactive-p) remove))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-pgp-encrypt (alias)
  "Toggle receiving PGP encryped mail. ALIAS.
You have to upload PGP key first with \\[tinypgp-nymserver-pgp-upload]"
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'pgpencrypt alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-pgp-sign (alias)
  "Turn on/off PGP siging. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'pgpsign alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-pgp-sendmix (alias)
  "Turn on/off Mixmaster support. ALIAS."
  (interactive
   (progn
     (tinypgp-nymserver-i-enable)
     (list (tinypgp-nymserver-ask))))
  (tinypgp-nymserver-sendmail 'sendmix alias (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-help-i-args (arg)
  "Ask args for `tinypgp-nymserver-help' using ARG."
  (list
   (tinypgp-nymserver-ask)
   arg))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-help-verbose (&optional arg)
  "Call `tinypgp-nymserver-help' as interactive would with ARG."
  (let* ((a (tinypgp-nymserver-help-i-args arg)))
    (tinypgp-nymserver-help (nth 0 a) (nth 1 a))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-nymserver-help (alias &optional mail-req verb)
  "Print help or send the help request via mail.

Input:
  ALIAS             ,from where to ask the help file.
  MAIL-REQ          ,send mail request [current-prefix-arg]
  VERB              ,verbose messages"
  (interactive  (tinypgp-nymserver-help-i-args current-prefix-arg))
  (let* ((elt   (assoc alias tinypgp-:nymserver-account-table))
         (file  (or (nth 5 elt) "_#_#")))
    (ti::verb)
    (cond
     (mail-req
      (tinypgp-nymserver-sendmail 'help alias verb))
     (t
      (cond
       ((file-exists-p file)
        (pop-to-buffer (find-file-noselect file)))

       ((not (file-exists-p file))
        (error "TinyPgp: File not exists %s" file))

       (t
        (message "No HELP file defied in tinypgp-:nymserver-account-table")
        (sit-for 2)
        (message " You get the help file, when you create account.")))))))

;;}}}

;;{{{ misc: ask

;;; ........................................................... &r-ask ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-reply-block-remailer (&optional msg)
  "Ask which remailer's reply block to use. Return remailer.
References:
   `tinypgp-:r-reply-block-tab.le'"
  (or tinypgp-:r-reply-block-table
      (error "TinyPgp tinypgp-:r-reply-block-table is empty."))
  (completing-read
   (or msg "Select Reply block of remailer: ")
   (ti::list-to-assoc-menu (mapcar 'car tinypgp-:r-reply-block-table))
   nil
   'match))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-remailer (&optional msg)
  "Select REMAILER with optional MSG."
  (let* (list)
    (tinypgp-r-init-maybe)
    (unless (setq list
                  (ti::list-to-assoc-menu
                   (mapcar 'car tinypgp-:r-host-table)))
      (error "TinyPgp Internal error, tinypgp-:r-host-table is nil."))
    (completing-read
     (or msg "Select remailer: ")
     list
     nil 'match
     nil
     'tinypgp-:r-history)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-email-keyserver (&optional msg)
  "Ask which email keyserver to use using MSG."
  (tinypgp-alias2name
   (completing-read
    (or msg "Email key server: ")
    (ti::list-to-assoc-menu (mapcar 'car tinypgp-:keyserver-mail-table))
    nil
    'match-it
    (car (car tinypgp-:keyserver-mail-table)))
   tinypgp-:keyserver-mail-table))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-http-keyserver ()
  "Ask which http keyserver to use. Return keyserver elt."
  (let* (elt)
    (setq
     elt
     (assoc
      (completing-read
       "Key server: "
       (ti::list-to-assoc-menu (mapcar 'car tinypgp-:keyserver-http-table))
       nil 'match
       (or (tinypgp-hash 'keyserver 'get 'used nil 'global) ;; last used
           (car (car tinypgp-:keyserver-http-table))) ;; or first in list
       'tinypgp-:history-http-keyserver)
      tinypgp-:keyserver-http-table))

    ;;  Remember the last used keyserver
    ;;
    (tinypgp-hash 'keyserver 'put 'used (car-safe elt) 'global)
    (tinypgp-hash 'keyserver 'put 'elt elt 'global)
    elt))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-remail-args (&optional msg)
  "Ask remail arguments for REMAILER with crypt key ask MSG.
Return:
 '(remailer-elt latent key)"
  (let (remailer
        remailer-elt
        latent
        key)

    (setq remailer (tinypgp-ask-remailer))
    (setq remailer-elt (tinypgp-r-elt-remailer remailer))

    (if (string-match "ek" (nth 2 remailer-elt)) ;Supports this ?
        (if (ti::nil-p
             (setq
              key (read-from-minibuffer
                   "Use crypt key: ")))
            (setq key nil)))

    (if (string-match "latent" (nth 2 remailer-elt))
        (cond
         ((ti::nil-p
           (setq
            latent (read-from-minibuffer
                    "Latent time e.g. +0:00r [empty = no latent]: ")))
          (setq latent nil))
         (t                             ;Some checkings
          (setq latent (ti::string-remove-whitespace latent))
          (or (string-match "^\\+[0-9]:[0-9][0-9]r?$" latent)
              (error "TinyPgp: Invalid latent time format '%s'" latent)))))
    (list remailer-elt  latent key)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-i-args-decrypt ()
  "Ask suitable decrypt password and return decrypt type.
This function tries to determine if it should ask conventional password of
pgp password by looking at the pgp stream.

Return:
  string            decrypt-type"
  (let* ((fid       "tinypgp-i-args-decrypt: ")
         (c-point   (ti::mail-pgp-encrypted-p))
         (tlist     (ti::list-to-assoc-menu '("pgp" "base64" "conventional")))
         (type      "pgp")
         var-sym)
    (tinypgpd fid c-point)

    (unless c-point
      ;;  couldn't find "Encrypted: PGP" tag, ask type then
      ;;
      (setq type
            (and tinypgp-:pgp-encrypted-p-function
                 (funcall tinypgp-:pgp-encrypted-p-function)))

      ;;  See if the type was set to sensible value. Ask from
      ;;  user if it wasn't
      ;;
      (if (or (not (stringp type))
              (not (assoc type tlist)))
          (setq type (completing-read
                      "Decrypt type: " tlist nil 'match "pgp"))))

    (cond
     ((string= type "conventional")
      (setq var-sym type)
      (ti::vector-table-get  tinypgp-:hash-password var-sym 'allocate)
      (ti::vector-table-property
       tinypgp-:hash-password var-sym 'password nil 'force)
      (tinypgp-password-set nil 'conventional))

     ((string= type "pgp")
      (tinypgp-save-state-macro
       (tinypgp-user-change-macro
        ;;  Now We are right user to ask the PGP pass phrase
        ;;
        (tinypgp-ask-pass-phrase-decrypt)))))
    type))

;;}}}
;;{{{ PGP entry i-macros

;;; ........................................................ &i-macros ...
;;; functions that are normally used in (interactive) spec.
;;;

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-i-args-read-email
  (&optional barf-if-not-email-buffer prompt history-sym)
  "Read email addresses from buffer or ask from use with completion.

Input:

 BARF-IF-NOT-EMAIL-BUFFER   as name says
 PROMPT                     display this string
 HISTORY-SYM                use this history"
  (let* ((fid  "tinypgp-i-args-read-email:")
         to-field
         ret
         init
         tmp)

    (tinypgpd fid "in: " barf-if-not-email-buffer prompt)

    (if (and barf-if-not-email-buffer
             (not (ti::mail-mail-p)))
        (error "TinyPgp: This is not an mail buffer."))

    (or tinypgp-:pgp-email-list-completions ;make sure this exist
        (tinypgp-update-mail-abbrevs))

    (cond
     ((and (string-match "news\\|message\\|mail" (symbol-name  major-mode))
           (not (ti::nil-p
                 (mail-fetch-field      "To")))) ;Just check this

      (setq to-field (ti::mail-get-all-email-addresses
                      nil
                      tinypgp-:pgp-email-abbrev-list))

      (tinypgpd fid "cond1: to-field" to-field)

      ;;  Slim down "Mr. ABC <abc@com>" --> "abc@com"
      ;;
      (setq to-field
            (mapcar
             (function
              (lambda (x)
                (ti::string-remove-whitespace
                 (ti::remove-properties (tinypgp-email-or-string x)))))
             to-field))

      (setq ret to-field)

      ;;   Confirm only if there is multiple recipients
      ;;   07.03.97 I have disbled the confimation with 'and'.
      ;;
      (if (> (length to-field) 1)
          (and
           nil
           (ti::read-char-safe-until
            (format "%d email recipients found. Press to continue."
                    (length to-field))))
        ;;  See if point in on line that has email
        ;;
        (when
            (setq
             tmp
             (car-safe (ti::mail-email-from-string
                        (ti::remove-properties (ti::read-current-line)))))
          (push tmp to-field)
          (setq tmp
                (completing-read
                 "You were on email line, use it? [empty=skip]: "
                 (ti::list-to-assoc-menu to-field) nil nil
                 tmp))

          (if (not (ti::nil-p tmp))
              (setq ret tmp)))))

     (t
      (setq init
            (ti::string-remove-whitespace
             (or (ti::mail-get-field "Request-Remailing-To" 'any)
                 (ti::mail-get-field "Anon-To" 'any)
                 tinypgp-:user-now)))

      (tinypgpd fid "cond t: ")
      (setq
       ret
       (completing-read
        (or prompt "User: ")
        tinypgp-:pgp-email-list-completions
        nil nil
        init
        (or history-sym
            'tinypgp-:history-email)))
      (setq ret (tinypgp-email-or-string ret))
      (if ret
          (setq ret (ti::string-remove-whitespace ret)))))

    (tinypgpd fid "hook call: " ret)

    (setq ret (tinypgp-key-id-conversion ret))

    (tinypgpd fid "RET: " ret)

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-i-args-pass-phrase (&optional msg)
  "The MSG defaults to asking signing pass phrase."
  (tinypgp-password-set
   (format "[%s] %s"
           (or tinypgp-:user-now
               (error "TinyPgp Internal error: current pgp user unknown."))
           (or msg
               "Sign pass phrase: "))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-pass-phrase-decrypt ()
  "See `tinypgp-i-args-pass-phrase'."
  (tinypgp-i-args-pass-phrase "Decrypt pass phrase: "))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-i-args-reg-email (&optional prompt barf-not-mail-buffer)
  "Read region + String. PROMPT BARF-NOT-MAIL-BUFFER."
  (ti::i-macro-region-body
    (tinypgp-i-args-read-email barf-not-mail-buffer)))

;;}}}
;;{{{ PGP entry command macros, email,exe

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-cmd-macro-email 'lisp-indent-function 1)
(defmacro tinypgp-cmd-macro-email (message &rest body)
  "(MESSAGE &rest BODY). Select email body or whole buffer.

You must locally define variable `beg' `end' in let statement
before using this macro."
  (`
   (cond
    ((or (ti::mail-text-start)
         (progn
           ;;  The region is defined beforehand, now.
           ;;
           (setq beg (point-min)  end (point-max))
           (y-or-n-p
            (format "Not a mail buffer, %s whole buffer? "
                    (or (, message) "Use")))))
     (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-cmd-macro 'lisp-indent-function 3)
(defmacro tinypgp-cmd-macro
  (cmd user password &optional msg reg options mode-specific &rest body)
  "Common command macro for all PGP commands.
Macro, used to contruct user command. CMD and USER must be
variables. You must bound 'beg' and 'end' variables before calling this
macro.

Args:

  (cmd user password &optional msg reg options &rest body mode-specific)

Input:

  CMD USER PASSWORD     parameters. CMD is symbol for logical command

  MSG                   message shown to user before initiating command

  REG                   non-nil = put results to register instead
                        of replacing the region with pgp output.

  OPTIONS               extra switched that are added to the pgp command.

  MODE-SPECIFIC         If non-nil, Do not run mode specific actions.

  BODY                  code to execute when real pgp command is known.
                        If there is no body, then execute the command
                        that is found from table.

                        Body must assign the result of command to
                        macro variable 'ReS'

                        The default command executed in macro is, where
                        Rcmd is the real shell command. However the
                        command can still contains macros that start
                        like #MACRONAME.

                        (tinypgp-binary-do-command-region Rcmd beg end msg (, reg))

Hooks:
  `tinypgp-before-do-cmd-region-hook'
  `tinypgp-after-do-cmd-region-hook'"
  (`
   (let* ((FiD   "tinypgp-cmd-macro: ")
          (Rcmd  (tinypgp-binary-get-cmd (, cmd) (, options))) ;Real command
          (enter-buffer (current-buffer))
          (msg   (if (or verb (interactive-p))
                     (, msg)))
          edit-buffer
          ReS
          beg-mark
          end-mark)

     ;;  VM: edit mode changes the current buffer
     ;;  Gnus: sometimes we must clone the buffer (nntp doesn't allow edit)

     (unless (, mode-specific)
       (tinypgp-mode-specific-control-before
        (, cmd) (, user)  msg (, reg)))

     (tinypgpd FiD "in:" enter-buffer
               beg end
               "CMD" (, cmd)
               "USER" (, user)
               "pass" (, password)
               msg
               "register" (, reg)
               "MODE-SPEC" (, mode-specific))

     (setq edit-buffer (current-buffer))
     (tinypgpd FiD "EDIT-BUFFER" major-mode edit-buffer)

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . setting markers ..
     (cond
      ((eq (, cmd) 'decrypt)
       (setq ReS (save-excursion (ti::pmin) (ti::mail-pgp-block-area 'msg)))
       (goto-char (or
                   ;;  This checks "encrypted: PGP" tag.
                   (ti::mail-pgp-encrypted-p)
                   ;;  Nope, there was none, use this.
                   ;;
                   (car ReS)))

       (setq beg-mark (point-marker))
       (goto-char (cdr ReS)) (setq end-mark (point-marker))
       (tinypgpd FiD "DECRYPT marks" beg-mark end-mark)
       (setq ReS nil))
      (t
       ;;   If user hasn't set END variable, we suppose
       ;;   rest of the buffer. It is important that END variable
       ;;   gets set here when MAIL message is handled, because
       ;;   only now the message is trimmed and whitespaces
       ;;   removed
       ;;
       (if (null end)
           (setq end (point-max)))

       (if (null beg)
           (setq beg (if (ti::mail-mail-p)
                         (ti::mail-text-start)
                       (point-min))))

       ;;  We use markers, because hook is called and it
       ;;  may change the buffer content. The area must still be
       ;;  available for us after changes.
       ;;
       (save-excursion
         (goto-char beg) (setq beg-mark (point-marker))
         (goto-char end) (setq end-mark (point-marker)))))

     (tinypgpd FiD "BEG END" beg end "MARKER-BEGIN" beg-mark end-mark
               (current-buffer))
     (tinypgpd FiD (buffer-substring beg-mark end-mark))

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . user funcall ..

     (if tinypgp-:cmd-macro-before-hook
         (run-hook-with-args-until-success 'tinypgp-:cmd-macro-before-hook
                                           (, cmd) (, user) msg (, reg)))

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  checking markers ..

     (if (or (null (setq beg (marker-position beg-mark)))
             (null (setq end (marker-position end-mark)))
             (eq beg end))              ;This is error too.
         (error "\
TinyPgp: tinypgp-:cmd-macro-before-hook modified text too much."))

     (setq beg-mark nil end-mark nil)   ;kill the markers

     (if ReS (setq ReS nil))            ;NoOp XE ByteComp silencer

     (if (null (, user))
         (setq (, user) (user-login-name)))

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . make command ..

     (setq Rcmd (tinypgp-cmd-compose Rcmd (, user) ))

     (tinypgpd FiD "vars:" "USER" (, user)
               "CUR-BUF" (current-buffer) beg end
               "CMD" Rcmd
               "BODY-NIL" (equal 'nil (quote (, body))))

;;;     (ti::d! "Doing COMMAND" beg end (current-buffer))

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . DO ACTION ..
     ;;     Check if BODY is omitted

     (cond
      ((equal 'nil (quote (, body)))
       (setq
        ReS
        (if (or (tinypgp-backend-pgp2-p)
                (tinypgp-backend-gpg-p))
            (tinypgp-binary-do-command-region
             Rcmd
             beg
             end
             (tinypgp-hash 'mode-specific 'get 'buffer-edit nil 'global)
             msg
             (, reg))
          (tinypgp-binary-do-command-region-with-expect
           Rcmd
           beg end
           (tinypgp-hash 'mode-specific 'get 'buffer-edit nil 'global)
           msg
           (, reg)))))

      (t
       (,@ body)))

     (tinypgp-binary-header-field-fix (, cmd) 'force)

     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  results ..
;;;     (setq PTR ReS)

     (when (, reg)                      ;Save results
       (set-register
        tinypgp-:register
        (tinypgp-binary-get-result-as-string ReS)))

     (tinypgpd FiD "cmd-macro done. calling mode specific...")

     (if tinypgp-:cmd-macro-after-hook
         (run-hook-with-args-until-success 'tinypgp-:cmd-macro-after-hook
                                           (, cmd) (, user) msg (, reg)))

     (tinypgpd "cmd-macro out:")
     ReS)))

;;}}}
;;{{{ PGP exe command compose

;;; ................................................. &command-compose ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-cmd-compose (cmd user &optional password args)
  "Compose PGP command.

Input:

  CMD       list of strings which may contain #TAGS
            '(binary base-command-set options)
  USER
  PASSWORD
  ARGS"
  (let* ((cat  (if (eq (tinypgp-backend-type) 'win32)
                   "type "
                 "cat "))
         (binary-type (tinypgp-backend-type))
         (binary      (car cmd))
         tmp)

    (setq cmd (format "%s %s" (nth 1 cmd) (or (nth 2 cmd) "")))

    ;;  Decide where to put the binary itself. Is there a token #bin
    ;;  where to put it?

    (cond
     ((string-match "#bin" cmd)
      (setq cmd (ti::replace-match 0 binary cmd)))
     (t
      (setq cmd (concat binary " " cmd))))

    (if tinypgp-:pgp-command-compose-function
        (setq cmd (funcall tinypgp-:pgp-command-compose-function cmd)))

    (tinypgpd "[cmd-compose] in: USER"
              tinypgp-:user-now "PRING" tinypgp-:pubring-now)

    (tinypgpd "[cmd-compose] in: cmd"
              cmd "USER" user "PASS" password )

    (unless (stringp tinypgp-:pubring-now)
      (error "TinyPgp: no current pubring? tinypgp-:pubring-now"))

    (unless (file-exists-p tinypgp-:pubring-now)
      (error "TinyPgp: %s (tinypgp-:pubring-now) does not exist."))

    (tinypgpd "[cmd-compose] in2: global user, pring"
              tinypgp-:user-now tinypgp-:pubring-now )

    (unless (stringp tinypgp-:user-now) ;;  make sure this variable exists
      (error "TinyPgp: user is unknown."))

    (when (string-match "#PUBRING" cmd)
      (setq cmd (ti::replace-match
                 0
                 (concat "+pubring="
                         (tinypgp-expand-file-name
                          tinypgp-:pubring-now binary-type)
                         " ")
                 cmd)))

    (when (string-match "#PGP-USER" cmd)
      (setq cmd (ti::replace-match
                 0 (concat
                    "-u \""
                    ;; Always treat this as list
                    (ti::list-to-string (ti::list-make tinypgp-:user-now))
                    "\" ")
                 cmd)))

    (when (and user
               (string-match "#USER" cmd))

      ;;  With PGP 2: -u "user"
      ;;  With pgp 5: -u user

      (setq tmp (if (or (tinypgp-backend-pgp2-p)
                        (tinypgp-backend-gpg-p))
                    "\""
                  ""))
      (setq cmd (ti::replace-match
                 0 (concat "-u " tmp
                           ;; Always treat this as list
                           (ti::list-to-string (ti::list-make user ))
                           tmp " ")
                 cmd)))

    (when (string-match "#OUT-FILE" cmd)
      (setq cmd (ti::replace-match
                 0
                 (concat "-o "
                         (tinypgp-expand-file-name
                          tinypgp-:file-output binary-type)
                         " ")
                 cmd)))

    (when (and user
               (string-match "#MUSER" cmd))
      (cond
       ((tinypgp-backend-pgp2-p)
        (let ((type (save-match-data (tinypgp-binary-get-version 'symbol))))
          (cond
           ((eq type 'international)
            (setq cmd
                  (ti::replace-match
                   0
                   (concat
                    "-@"
                    (tinypgp-expand-file-name
                     tinypgp-:file-user-list binary-type)
                    " ")
                   cmd)))
           (t                           ;doesn't know -@ switch
            (setq cmd
                  (ti::replace-match
                   0
                   (format "`%s %s`"
                           cat
                           (tinypgp-expand-file-name
                            tinypgp-:file-user-list binary-type))
                   cmd))))
          (tinypgp-file-control 'users-write user)))
       (t ;; pgp 5.x
        (setq tmp "")
        (dolist (elt user)
          (setq tmp (concat tmp " -r " elt)))

        (setq cmd (ti::replace-match 0 tmp cmd))
        (tinypgpd "[cmd-compose] #MUSER" user cmd))))

    ;; ........................................................... other ...
    ;; These are called from tinypgp-binary-do-command-region when parameters
    ;; are better known.

    (when (and args (string-match "#PIPE" cmd))
      ;; REST ARGS 1 = pipe file
      ;;
      (setq cmd (ti::replace-match
                 0
                 (concat cat
                         (tinypgp-expand-file-name
                          (or (nth 0 args)
                              tinypgp-:file-source)
                          binary-type)
                         " | ")
                 cmd)))

    (when (string-match "#SOURCE-FILE" cmd)
      (let ((file (or (nth 0 args)
                      tinypgp-:file-source)))
        ;; ARGS = filename
        ;;
        (setq cmd (ti::replace-match
                   0
                   (concat " "
                           (tinypgp-expand-file-name file binary-type)
                           " ")
                   cmd))))

    ;; .......................................................... password ...

    (when (string-match "#password" cmd)
      (when (ti::nil-p password)
        (setq password (tinypgp-password-get)))

      (when (null password)
        (error
         "TinyPgp Internal error: Command composing failed. No passwd."))

      (when (tinypgp-backend-gpg-p)
        (setq cmd (ti::replace-match
                   0
                   (concat "\""  password "\" | ") cmd)))

      (when (tinypgp-backend-pgp2-p)
        (if (or nil                     ;Enabled now!
                (null tinypgp-:password-protection))
            (setq cmd (ti::replace-match
                       0
                       (concat "-z\""  password "\" ") cmd))

          (setq cmd (ti::replace-match 0 nil cmd))
          (tinypgp-file-control 'password-write password)

          ;;  PGP gets the password from file descriptor 3. This way
          ;;  'ps' listing doesn't show the password like it does
          ;;  with -z option
          ;;
          (setq cmd (format (concat "PGPPASSFD=3; export PGPPASSFD; "
                                    " #PIPE %s  3< %s ")
                            cmd
                            (tinypgp-expand-file-name
                             tinypgp-:file-password binary-type))))))

    (tinypgpd "[cmd-compose] out: "
              cmd  tinypgp-:pgp-command-compose-function )

    cmd))

;;}}}
;;{{{ PGP exe result, general, macros, error

;;; ........................................................ &pgp-core ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-header-field-set (field value)
  "Set FIELD with VALUE in PGP Signature header."
  (ti::pmax)
  (when (or (re-search-backward (ti::mail-pgp-signature-begin-line) nil t)
            (re-search-backward (ti::mail-pgp-msg-begin-line) nil t))
    (tinypgpd "tinypgp-binary-header-field-set: " field value)
    (cond
     ((re-search-forward field nil t)
      (delete-region (point) (line-end-position))
      (insert " " value)
      (forward-line 1))
     ((re-search-forward "^[ \t]*$")    ;Must exist
      (insert field " " value "\n")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-header-field-fix (command &optional force)
  "Change PGP headers for COMMAND. Optionally FORCE in spite of backend.
In Windows NT not all the command line options cannot passed
with the call, so we patch resulte manually."
  (when (and (or force
                 (ti::win32-p)
                 (not (tinypgp-backend-pgp2-p)))
             (ti::re-search-check (ti::mail-pgp-signature-begin-line)))
    (save-excursion
      (let* ((comment
              (get 'tinypgp-:pgp-binary-interactive-option 'comment)))
        (when (eq command 'sign)
          ;; (tinypgp-binary-header-field-set "Charset:" tinypgp-:pgp-binary-charset)
          (if comment
              (tinypgp-binary-header-field-set "Comment:" comment)))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-excute-in-tmp 'lisp-indent-function 2)
(defmacro tinypgp-excute-in-tmp (beg end &rest body)
  "Copy region BEG END from current buffer and execute BODY.
Uses buffer `tinypgp-:buffer-tmp-shell'."
  (`
   (let* ((ob  (current-buffer))
          (tmp (tinypgp-ti::temp-buffer 'shell)))
     (with-current-buffer tmp
       (insert-buffer-substring ob (, beg) (, end))
       (tinypgp-x-headers-deinstall)
       (,@ body)))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-binary1-command-table (cmd)
  "Return right command table"
  (cond
   ((eq 'pgp2 (tinypgp-backend-now))
    (assq cmd tinypgp-:pgp-command-table))
   ((eq 'gpg (tinypgp-backend-now))
    (assq cmd tinypgp-:gpg-command-table))
   (t
    (assq cmd tinypgp-:pgp-command-table5))))

;;; ----------------------------------------------------------------------
;;; http://www.pgpi.org/products/pgp/versions/freeware/
;;; => Unix => PGP 2.6.3i => Download PGP 2.6.3i
;;; => Download PGP 2.6.3i source code
;;;
;;; Win32/Cygwin compile command:
;;;
;;; cd /tmp
;;; gzip -dc pgp263is.tar.gz | tar -xvf
;;; tar -xvf pgp263ii.tar
;;; cd src/
;;; make -f makefile CFLAGS='-DUNIX -DPORTABLE' CC=gcc linux
;;;
(defun tinypgp-binary-path-set (&optional verb)
  "Define backend properties in variable `tinypgp-:pgp-binary'.

This function stores the executable paths in variable
`tinypgp-:pgp-binary'."
  (interactive)
  (let  ((fid   "tinypgp-binary-path-set: ")
         (list  '("pgpk" "pgpv" "pgpe" "pgps"))
         (ext    (if (ti::win32-p)
                     ".exe"
                   ""))
         (cygwin-root (ti::win32-cygwin-p))
         (search (delete "." exec-path))
         (count  0)
         exe
         str
         path)

    (ti::verb)

    ;; Clear all first

    (dolist (sym '(pgp-set
                   pgp
                   pgp2-type
                   pgp5-type
                   gpg-type
                   gpg
                   pgpk pgpv pgpo pgpe))
      (put 'tinypgp-:pgp-binary sym nil))

    ;; ......................................................... 2.6.x ...

    (cond
     ((setq path (ti::file-get-load-path (concat "pgp" ext) search 'all))
      (dolist (bin (ti::list-make path))

        ;;  Is this really 2.6.x? The PGP 5.x kit may contain binary
        ;;  "pgp" too

        (setq str (ti::mail-pgp-exe-version-string bin))
        (tinypgpd fid "Verifying 2.6" bin  str)

        (when (stringp str)
          (cond
           ((string-match "2\\.6" str)
            (put 'tinypgp-:pgp-binary 'pgp bin)
            (put 'tinypgp-:pgp-binary 'pgp-backends '(pgp2))

            ;;  It is impossible to say if the pgp.exe is Cygwin
            ;;  compiled or pure DOS version, because "pgp -h" gives
            ;;  identical message.
            ;;
            ;;  The cygwin status is needed, because it affects
            ;;  how file names are passed.
            ;;
            ;;  It is supposed that "cygwin version" WILL reside under
            ;;  Cygwin hierarchy. This test fails if user uses
            ;;  mount points that refer to external disks

            (let* ((cygwin-p (and cygwin-root
                                  (string-match
                                   (ti::file-path-to-unix cygwin-root)
                                   (ti::file-path-to-unix bin))))
                   (type  (if (and (ti::win32-p)
                                   (not cygwin-p))
                              'win32
                            'unix)))
              (put 'tinypgp-:pgp-binary 'pgp2-type type))
            (return))
           (t
            (message "TinyPgp: `pgp' found but that's not 2.6 version"))))))

     (verb
      (message "Tinypgp: Hm, no pgp 2.x binary found.")
      (sit-for 1)))

    ;; ........................................................... GPG ...

    (cond
     ((setq path (ti::file-get-load-path (concat "gpg" ext) search 'all))
      (dolist (bin (ti::list-make path))

        (setq str (ti::mail-pgp-exe-version-string bin))
        (tinypgpd fid "Verifying GPG 1.x" bin  str)

        (when (stringp str)
          (cond
           ((string-match "1\\." str)
            (put 'tinypgp-:pgp-binary 'gpg bin)
            (put 'tinypgp-:pgp-binary
                 'pgp-backends
                 (append '(gpg) (tinypgp-backend-list)))

            ;;  It is impossible to say if the gpg.exe is Cygwin
            ;;  compiled or pure DOS version, because "pgp -h" gives
            ;;  identical message.

            (let* ((cygwin-p (and cygwin-root
                                  (string-match
                                   (ti::file-path-to-unix cygwin-root)
                                   (ti::file-path-to-unix bin))))
                   (type  (if (and (ti::win32-p)
                                   (not cygwin-p))
                              'win32
                            'unix)))
              (put 'tinypgp-:pgp-binary 'gpg-type type))
            (return))
           (t
            (message "TinyPgp: `gpg' found but that's not 1.x version"))))))

     (verb
      (message "Tinypgp: Hm, no gpg 1.x binary found.")
      (sit-for 1)))

    ;; ........................................................... 5.x ...

    (dolist (bin list)
      (setq exe  (concat bin ext)
            path (ti::file-get-load-path exe search 'all))
      (cond
       ((null path)
        (when verb
          (message "TinyPgp: Can't find PGP[56] executable %s:%s" exe search))
        (tinypgpd fid "Verifying 5.x FAILED" exe path))
       (path
        (dolist (binary (ti::list-make path))

          ;;  #todo: what should be done to multiple occurrances of BIN?

          (tinypgpd fid "Verifying 5.x" binary)
          (incf count)
          (put 'tinypgp-:pgp-binary (intern bin) binary)))))

    ;;  if all pgp 5.x executables were found; then installation went okay

    (when (eq count 4)
      (setq    list (tinypgp-backend-list))
      (add-to-list 'list 'pgp5)
      (put     'tinypgp-:pgp-binary 'pgp-backends list))

    (tinypgpd
     fid
     "count"     count
     "extension" ext
     "pgp-set"   (tinypgp-backend-list)
     "pgp"       (get 'tinypgp-:pgp-binary 'pgp)
     "pgpk"      (get 'tinypgp-:pgp-binary 'pgpk))

    (if verb
        (message "Tinypgp: found %s"
                 (or
                  (and (tinypgp-backend-list)
                       (mapconcat
                        (function (lambda (elt) (symbol-name elt)))
                        (tinypgp-backend-list)
                        " "))
                  "(nothing)")))

    (tinypgp-backend-list)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-select (backend &optional verb)
  "Select BACKEND 'pgp2 or 'pgp5 executables for use. VERB."
  (interactive
   (let* ((list (mapcar
                 (function
                  (lambda (elt)
                    (cons
                     (symbol-name elt)
                     elt)))
                 (tinypgp-backend-list)))
          ret)
     (setq ret (completing-read "Select pgp: " list nil 'match))
     (list (cdr (assoc ret list)))))

  (let* ((fid "tinypgp-backend-select: ")
         secring
         pubring)

    (ti::verb)

    ;;  Check that arg is part of known list
    (unless (member backend (tinypgp-backend-list))
      (error
       "\
TinyPgp: Feature %s is not configured or available: Call tinypgp-binary-path-set"
       backend))

    (put 'tinypgp-:pgp-binary 'pgp-now backend)

    (setq secring (tinypgp-secring-file))
    (unless (file-exists-p secring)
      (error "\
TinyPgp: Secring %s does not exist. See tinypgp-:file-secring %s" secring))

    (setq pubring (tinypgp-pubring-default))
    (unless (file-exists-p pubring)
      (error
       "\
TinyPgp: Can't find pubring %s. Check tinypgp-:pubring-table for backend %s"
       pubring
       backend))

    (setq tinypgp-:pubring-now pubring)

    ;; Each time backend is changed, the cache must be updated and

    (tinypgp-key-cache-remove-entry-last)
    (setq tinypgp-:key-cache nil)
    (tinypgp-key-cache-save 'load)

    (tinypgpd fid backend pubring secring)
    (tinypgp-update-modeline)

    (if verb
        (message "Tinypgp: backend %s" (symbol-name backend)))

    secring))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-variable-state-control (&optional restore)
  "Save or RESTORE variables. Used when changing backends."
  (let* ((opt (get 'tinypgp-:pgp-binary-interactive-option 'original)))
    ;;  PGP 5.x doesn't know +comment option.

    (cond
     (restore
      (setq tinypgp-:pgp-binary-interactive-option opt))
     (t
      (put 'tinypgp-:pgp-binary-interactive-option 'original
           tinypgp-:pgp-binary-interactive-option)
      (setq tinypgp-:pgp-binary-interactive-option nil)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-select-pgp2 ()
  "Select pgp 2.6.x backend"
  (interactive)
  (tinypgp-variable-state-control 'restore)
  (tinypgp-backend-select 'pgp2 (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-select-pgp5  ()
  "Select pgp 5.x backend"
  (interactive)
  (tinypgp-variable-state-control)
  (tinypgp-backend-select 'pgp5 (interactive-p)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-select-auto ()
  "Select pgp 2 if it exists else use pgp 5. Otherwise flag error."
  (let* ((list (get 'tinypgp-:pgp-binary 'pgp-backends)))
    (cond
     ((memq 'pgp2 list)
      (tinypgp-backend-select-pgp2))
     ((memq 'pgp5 list)
      (tinypgp-backend-select-pgp5))
     (t
      (error "\
Check PATH for pgp executable(s): maybe tinypgp-binary-path-set failed.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-backend-set-for-action  (action &rest args)
  "Select right backend for ACTION.
Action may be 'remail 'newnym  'nymserv or 'pgp
Die if can't select right backend."
  (when (memq action '(remail newnym nymserv))
    (unless (tinypgp-backend-pgp2-p)
      (unless (tinypgp-backend-exist-pgp2)
        (error "Pgp 2 not available for Action %s" action))
      (tinypgp-backend-select-pgp2))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary1 (cmd)
  "Return right pgp executable for COMMAND type 'encrypt ...."
  (interactive)
  (let* (ret)
    (setq
     ret
     (cond
      ((tinypgp-backend-pgp2-p)
       (get 'tinypgp-:pgp-binary 'pgp))

      ((tinypgp-backend-gpg-p)
       (get 'tinypgp-:pgp-binary 'gpg))

      ((eq 'pgp5 (tinypgp-backend-now))
       (cond
        ((memq cmd '(sign
                     sign-detach))
         (get 'tinypgp-:pgp-binary 'pgps))

        ((memq cmd '(encrypt
                     encrypt-sign
                     encrypt-info
                     crypt))
         (get 'tinypgp-:pgp-binary 'pgpe))

        ((memq cmd '(decrypt
                     decrypt-base64))
         (get 'tinypgp-:pgp-binary 'pgpv))

        ((eq cmd 'verify)
         (get 'tinypgp-:pgp-binary 'pgpv))

        ((string-match "key" (symbol-name cmd))
         (get 'tinypgp-:pgp-binary 'pgpk))))))

    (if (or (not (stringp ret))
            (not (file-exists-p ret)))
        (error "Install failure: Please run tinypgp-binary-path-set (%s)" cmd))

    ;;  In WinNT the maximum command length is 255, so we can't
    ;;  afford to use absolute path here. (It would have been faster)
    ;;

    (if (ti::win32-p)
        (file-name-nondirectory ret)
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-cmd (cmd &optional options)
  "Return pgp shell command according to logical CMD with appended OPTIONS."
  (let* ((exe (tinypgp-binary1 cmd))
         (elt (tinypgp-binary1-command-table cmd)))
    (if (null elt)
        (error "PGP exe command error: No logical command in table '%s'" cmd)
      (list
       exe
       (nth 1 elt)
       options))))

;;; ----------------------------------------------------------------------
;;;
(defmacro tinypgp-binary-result-data-win32 (beg end)
  "Set result of PGP2 in WindowsNt shell buffer.
In unix the output is printed so that 1)stderr 2)results
but in Windows NT it could be printed in reverse order.

We check here if the data is put to the beginning of the buffer,
before the PGP logo.

Variables BEG and END are modified if data starts from `point-min'."
  (`
   (progn
     ;;  1) If variables are both nil
     ;;  2) they are equal
     ;;
     (when (or (not (and (, beg) (, end)))
               (eq (, beg) (, end)))
       (save-excursion
         (ti::pmin)
         ;;   No configuration file found.
         ;;   Pretty Good Privacy(tm) 2.6.3ia -
         ;;
         (when (and (re-search-forward
                     (concat
                      "^config.txt: \\|"
                      "^No configuration file found.$\\|"
                      "\C-g?Pretty Good Privacy(tm)")
                     nil t)
                    (prog1 t (beginning-of-line))
                    (not (eq (point) (point-min))))
           (setq beg (point-min) end (point))))))))

;;; ----------------------------------------------------------------------
;;;
(put 'tinypgp-binary-get-result-re1-macro 'lisp-indent-function 1)
(defmacro tinypgp-binary-get-result-re1-macro (options &rest body)
  "If case-sensitive REGEXP match, execute BODY.
The OPTIONS is a list containing an alist of options:

'((regexp  REGEXP)            - Search REGEXP
  (loop    [t|nil]))          - if LOOP is t, run while loop for REGEXP"
  (`
   (with-current-buffer tinypgp-:buffer-tmp-shell
     (let (case-fold-search             ;Case sensitive matching
           (re   (nth 1 (assq 'regexp (, options))))
           (loop (nth 1 (assq 'loop (, options)))))
       (ti::pmin)
       (when (re-search-forward re nil t)
         (if loop
             (while (re-search-forward re nil t)))
         (tinypgpd "exe-get-result-re1-macro:" (match-string 0) )
         (,@ body))))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-binary-insert-command-log (&optional point cmd)
  "Insert last command log into POINT[current point] or insert CMD."
  (if point (goto-char point))

  (setq cmd
        (if cmd
            (prin1-to-string cmd)
          (prin1-to-string tinypgp-:last-pgp-exe-command)))

  (insert "\n\nTinyPgp report, last command and parameters:\n\n"

          "explicit-shell-file-name: "
          (or explicit-shell-file-name "<>") "\n"

          "shell-file-name         : " (or shell-file-name "<>") "\n"
          "command length          : " (int-to-string (length cmd)) "\n\n"

          cmd

          "\n"))

;;}}}
;;{{{ PGP exe result get,check

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-check-error (&optional ignore-output-error cmd buffer)
  "Return non-nil, if the PGP output is not valid.

Input:

  IGNORE-OUTPUT-ERROR   this skips checking the output: --- TAGS
  CMD                   command used
  BUFFER                Where the pgp output is

References:
  `tinypgp-:error'      stored error message"
  (let ( ;;  the re-ok does not produce re-block, but it's still valid
        ;;  pgp answer, not an error condition.

        (fid                   "tinypgp-binary-check-error:" )
        (re-ok                 (concat
                                "Good signature \\(from\\|made\\)"
                                "\\|Bad signature from"
                                "\\|Pass phrase +\\(is\\|appears\\) +good"
                                "\\|WARNING: +"))
        (re-block              "-----BEGIN.*PGP")
        (re                    tinypgp-:pgp-binary-error-regexp)
        case-fold-search) ;; Case is important here !!

    (or buffer
        (setq buffer tinypgp-:buffer-tmp-shell))

    (setq tinypgp-:error nil)

    ;;  - See if buffer DOES not contain ok sign, then GO AND
    ;;    check error. Once I have message where
    ;;    "You do not have the secret" was written in message body.
    ;;    and that was not an error condition.

    (with-current-buffer buffer
      (unless (and (ti::re-search-check
                    "^Pass phrase is good.  Just a moment[.][.]+")

                   ;;  Funny; The previous message is ouputted, but
                   ;;  if one pass encryption&sign fails; this is message
                   ;;  will be seen. Make3 sure we don't see it.
                   ;;
                   ;; Including "pgp-lst"...
                   ;; Pass phrase is good.  Just a moment....
                   ;; ^GKey matching userid 'a@b.if' not found
                   ;; in file '/aa7bb/ring-all.pgp'
                   ;;
                   ;; ^GCannot find the public key matching userid 'a@b.if'
                   ;; This user will not be able to decrypt this message.
                   ;; ^GEncryption error

                   (null (ti::re-search-check "Encryption error$")))
        (tinypgp-binary-get-result-re1-macro (list (list 'regexp re))
                                             (tinypgpd fid "MB" (match-beginning 0) "ME" (match-end 0)
                                                       (current-buffer))
                                             (tinypgp-highlight 'match 0 nil tinypgp-:face-error nil
                                                                (match-beginning 0)
                                                                (match-end 0))
                                             (setq tinypgp-:error (ti::remove-properties (ti::read-current-line)))
                                             (tinypgp-binary-insert-command-log (point-max) cmd))))

    (unless ignore-output-error
      (with-current-buffer buffer
        (when (and (null tinypgp-:error) ;Not already set?
                   (not (ti::re-search-check re-ok 0 '(point-min)))
                   (not (ti::re-search-check re-block 0 '(point-min))))
          (setq tinypgp-:error "Internal error. No output from PGP.")
          (tinypgp-binary-insert-command-log (point-max) cmd))))

    ;;  If this was encryption and it failed, then remove entry from
    ;;  cache.

    (if tinypgp-:error
        (tinypgp-key-cache-remove-entry tinypgp-:error))

    (tinypgpd fid "RET" tinypgp-:error )

    tinypgp-:error))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result (&optional buffer)
  "Return the result of PGP output from BUFFER or `tinypgp-:buffer-tmp-shell'.
Look for markers -----BEGIN PGP, -----END PGP.
Return:
  '(buffer beg end)"
  (let* ((re1 "[.]*\\(-----BEGIN.*PGP\\)")
         (re2 "^-----END.*PGP")
         beg
         ret)
    (with-current-buffer (or buffer tinypgp-:buffer-tmp-shell)
      (ti::pmin)
      (when (re-search-forward re1 nil t)
        (setq beg (match-beginning 1))
        (ti::pmax)
        (when (re-search-backward re2 nil t)
          (setq ret (list (current-buffer) beg (line-end-position))))))
    (tinypgpd "exe-get-result ret: " ret )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-decrypt (&optional buffer)
  "Read BUFFER after decrypt and sign (international version).

Return position of result in buffer.
  '(buffer beg end)"
  (let* (ret
         tmp)
    ;; Note how international version spits string "pass phrase",
    ;; and US version doesn't
    ;; +++++++++++++++++++++++++++
    ;;
    ;; International version - not for use in the USA. Does not useRSAREF.
    ;; Current time: 1997/05/19 12:10 GMT
    ;; Pass phrase is good.  Just a moment....-----BEGIN PGP SIGNED
    ;;
    ;;
    ;; Export of this software may be restricted by the U.S. government.
    ;; Current time: 1997/05/16 20:40 GMT
    ;; Pass phrase is good.
    ;; Key for user ID: xxxxk
    ;; 768-bit key, Key ID xxxx
    ;; Also known as:
    ;; Also known as:
    ;; Just a moment....-----BEGIN PGP SIGNED MESSAGE-----

    ;; note: When you call command -seatf; encrypt and sign in one pass,
    ;; the output is bit different.
    ;; +++++++++++++++++++++++++++
    ;;
    ;;  International version - not for use in the USA. Does not use RSAREF.
    ;;  Current time: 1997/06/26 20:29 GMT
    ;;
    ;;  Including "/users/jaalto/.pgp/pgp-lst"...
    ;;  Pass phrase is good.  Just a moment....
    ;;  Key for user ID: Foo <foo@example.com>
    ;;  512-bit key, key ID 47141D35, created 1996/06/03
    ;;  Also known as: Jari Aalto, Finland <ssjaaa@uta.fi>
    ;;  .-----BEGIN PGP MESSAGE-----
    ;;  Version: 2.6.3ia
    ;;  Comment: Processed by Emacs TinyPgp.el 1.222
    ;;
    ;;  hEwDwLrt1UcUHTUBAgCFBDvkHJ7dEffIGiqyPi2WtdOPwWQ+Duw6/be/7FjJYEUV

    (tinypgp-binary-get-result-re1-macro ; -seatf
     '((regexp "Pass phrase is good.  Just a moment[.]+"))
     (when (and (save-excursion
                  (forward-line 1)
                  (looking-at  ".*Key for user ID:"))
                (re-search-forward (ti::mail-pgp-msg-begin-line) nil t))
       (setq
        tmp "-seatf[1]"
        ret (list (current-buffer) (match-beginning 0) (point-max)))))

    (unless ret
      (tinypgp-binary-get-result-re1-macro
       (list
        (list
         'regexp
         (concat
          "Pass phrase is good.  Just a moment[.]+"
          ;; #todo: warning handling in decrypting
          ;;
          "\\|WARNING: Can't find.*can't check signature integrity.*\n")))
       (setq
        tmp "[2]"
        ret (list (current-buffer) (point) (point-max)))))

    (unless ret
      ;;  This is from conventional decrypt
      (tinypgp-binary-get-result-re1-macro
       '((regexp "Pass phrase appears good\\. \\."))
       (setq
        tmp "[3]"
        ret (list (current-buffer) (point) (point-max)))))

    ;; gpg: encrypted with 1024-bit ELG-E key, ID E7114155, created 2002-01-15
    ;;    "foo <foo@some.com>"
    ;; <THE MESSAGE FOLLOWS>

    (unless ret
      ;;  This is from conventional decrypt
      (tinypgp-binary-get-result-re1-macro
       '((regexp "^gpg: encrypted with.*[\r\n\]+.*[\r\n\][\r\n\]?"))
       (setq
        tmp "[gpg]"
        ret (list (current-buffer) (point) (point-max)))))

    ;;  GPG is different. It will not give any indication if
    ;;  Pass phrase was good. It simply decrypted the message and
    ;;  possibly gave warnings:
    ;;  gpg: Please note that you don't have secure memory on this system
    ;;  gpg: Warning: unsafe permissions on file "~/.gnupg/options"
    ;;  gpg: Warning: unsafe permissions on file "~/.gnupg/random_seed"
    ;;  gpg: Warning: unsafe permissions on file "~/.gnupg/secring.gpg"
    ;;  gpg: Warning: unsafe permissions on file "~/.gnupg/pubring.gpg"
    ;;  <THE MESSAGE FOLLOWS>

    (unless ret
      ;;  This is from conventional decrypt
      (tinypgp-binary-get-result-re1-macro
       '((regexp "^gpg: Warning:.*[\r\n]")
         (loop   t))
       (setq
        tmp "[gpg]"
        ret (list (current-buffer) (point) (point-max)))))

    (tinypgpd "exe-get-result-decrypt ret: " tmp ret )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-encrypt-info (&optional buffer)
  "Return pointer to block 'This message can only be read by:'"
  (with-current-buffer (or buffer tinypgp-:buffer-tmp-shell)
    (ti::pmin)
    (when (re-search-forward
           "This message can only be read by:" nil t)
      (beginning-of-line)
      (let* ((beg (point)))
        (or (re-search-forward "^[ \t]*$" nil t) (ti::pmax))
        (beginning-of-line)
        (list (current-buffer) beg (point))))))

;;; ----------------------------------------------------------------------
;;;
;;; This message can only be read by:
;;;   keyID: EFDB16AD
;;;   foo <foo@some.com>
;;;
(defun tinypgp-binary-get-result-encrypt-info-list (&optional pointer)
  "Return list of users in 'This message can only be read by:'.
POINTER is region where to read the results: (buffer beg end)"
  (interactive)
  (let* (list)
    (or pointer
        (setq pointer (tinypgp-binary-get-result-encrypt-info)))
    (when pointer
      (with-current-buffer (car pointer)
        (goto-char (nth 1 pointer))
        (forward-line 1)
        (while (or (looking-at ".*keyID: +\\(.*\\)")
                   (looking-at "^ +\\(.*\\)"))
          (push (ti::remove-properties (match-string 1)) list)
          (forward-line 1))))
    list))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-verify-status (&optional buffer)
  "Return result STRING after verify from BUFFER."
  (tinypgp-binary-get-result-re1-macro
   (list
    (list
     'regexp
     (concat "Good signature \\(from\\|made\\)"

             ;;  This warning is preceeded by lines:
             ;;  File has signature.  Public key is required to check...
             ;;  Key matching expected Key ID 1CEB1F55 not found
             ;;
             "\\|WARNING: Can't find the right public"
             "\\|Bad signature from"
             "\\|Key matching.*not found")))
   (if (or (tinypgp-backend-pgp2-p)
           (tinypgp-backend-gpg-p))
       (ti::read-current-line)
     (forward-line 1)

     (let* ((case-fold-search t)
            (id  (ti::buffer-match ".*key +id +\\([0-9A-Z]+\\)" 1))
            list)
       (forward-line 1)
       (setq list
             (ti::mail-email-find-region
              (point)
              (progn (forward-line 5) (point))))
       (format "Good signature from %s%s"
               (if id (format " %s " id)  "")
               (if list
                   (ti::list-to-string list)
                 "<unknown>"))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-using-function (function &optional buffer)
  "Call FUNCTION with arg BUFFER and return result in string format.
Function is the one that returns `pointer' object, like
`tinypgp-binary-get-result-verify'"
  (let* ((pointer (funcall function buffer)))
    (when pointer
      (inline (tinypgp-binary-get-result-as-string pointer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-insert-pointer-data (pointer &optional beg)
  "Read POINTER '(buffer beg end) and insert data to point.
Input:
  pointer   '(BUFFER BEG END)
  beg       flag, keep poin in beginnning instead of end of inserted data.
"
  (if (not (eq 3 (length pointer)))
      (error "Invalid pointer")
    (let ((point (point)))
      (insert-buffer-substring (car pointer) (nth 1 pointer) (nth 2 pointer))
      (if beg
          (goto-char point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-as-string (pointer)
  "Read string from POINTER '(buffer beg end)."
  (with-current-buffer (car pointer)
    (buffer-substring (nth 1 pointer) (nth 2 pointer))))

;;; ----------------------------------------------------------------------
;;; File has signature.  Public key is required to check signature.
;;; .
;;; Good signature from user "XXX xxx <xxx@example.com>
;;; Signature made 1998/03/04 08:22 GMT using 512-bit key, key ID 47141D35
;;; PGP-DATA-FOLLOWS
;;;
(defun tinypgp-binary-get-result-verify (&optional buffer)
  "Return result after verify from BUFFER. '(buffer beg end)."
  (let ((fid   "tinypgp-binary-get-result-verify")
        ret
        beg
        end)
    (tinypgp-binary-get-result-re1-macro
     (list
      (list
       'regexp
       (concat "Good signature \\(from\\|made\\)"
               "\\|Bad signature"
               ;;  This warning is preceeded by lines:
               ;;  File has signature.  Public key is required to check...
               ;;  Key matching expected Key ID 1CEB1F55 not found
               ;;
               "\\|WARNING: Can't find the right public")))
     (re-search-forward "Signature made" nil t)

     (if (or (tinypgp-backend-pgp2-p)
             (tinypgp-backend-gpg-p)
             (forward-line 1)
             (goto-char (tinypgp-hash 'expect 'get 'point nil 'global)))

         (setq beg (point)
               end (point-max))

       (if (or (tinypgp-backend-pgp2-p)
               (tinypgp-backend-gpg-p))
           (tinypgp-binary-result-data-win32 beg end))

       ;; Sometimes PGP says this:
       ;;
       ;; Looking for next packet in '/users/jaalto/junk/pgptemp.$00'...
       ;;
       ;; File has signature.  Public key is required to check signature.
       ;;
       ;; File '/users/jaalto/junk/pgptemp.$01' has signature, but with no text.

       (when (re-search-forward "Looking for next packet in '" nil t)
         (beginning-of-line)
         (setq end (point)))

       (setq ret (list (current-buffer) beg end)))

     (tinypgpd fid  "POINTER" ret)
     ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-base64 (&optional buffer)
  "Get contents after the 'Signature made 1996/11 ...' from BUFFER.
Return:
  pointer   '(buffer beg end)"
  (let (ret)
    (tinypgp-binary-get-result-re1-macro
     '((regexp "^Good signature from"))
     ;; Good signature from user
     ;; Signature made 1996/11/07
     ;; DATA-HERE
     ;;
     (forward-line 2)
     (setq ret (list (current-buffer) (point) (point-max))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-key-add (&optional buffer)
  "Return result of key adding from BUFFER."
  (interactive)
  (let (ret
        list)
    (cond
     ((tinypgp-binary-get-result-re1-macro
       (list
        (list
         'regexp
         (concat
          "you need a newer version of PGP"
          "\\|Bad ASCII armor"
          "\\|^No +keys found\\|.*added.*\\|ERROR: Bkad ASCII armor.*"
          "\\|.*error\\|No new keys or signatures")))
       (setq ret (ti::read-current-line))))
     ((with-current-buffer tinypgp-:buffer-tmp-shell
        (ti::pmin)
        (setq list (ti::buffer-grep-lines "new key(s)")))
      (setq ret (format "%d New keys added." (length list)))))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-key-sign (&optional buffer)
  "Return result of key signing from BUFFER."
  (interactive)
  (let (ret)
    (cond
     ((tinypgp-binary-get-result-re1-macro
       (list
        (list
         'regexp
         (concat
          "^No +keys found\\|ERROR\\|.*error"
          "\\|Key is already signed by")))
       (setq ret (ti::read-current-line)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-get-result-key-remove (&optional buffer)
  "Return result of key remove from BUFFER."
  (let (ret)
    (cond
     ((tinypgp-binary-get-result-re1-macro
       (list
        (list
         'regexp
         (concat
          ;;  PGP can't remove key if it asks this
          ;;
          ;;  Key has more than one user ID.
          ;;  Do you want to remove the whole key (y/N)? << WAITS HERE
          ;;
          "^Key has more than one user ID"
          "\\|Keyring remove error")))
       (setq ret (ti::read-current-line)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-exit-code-ok-p (number)
  "Check if exit code NUMBER is ok."
  (if (and (tinypgp-backend-pgp2-p)
           (memq number '(0 1)))
      t
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-exit-status-entry (number)
  "Check PGP's exit code NUMBER and return appropriate error message."
  (let* ((table  tinypgp-:pgp-binary-exit-code-table)
         elt)
    (cond
     ((tinypgp-backend-pgp2-p)
      (setq elt  (cdr (assq 'pgp2 table)))))
    ;; (unless elt (error "Unknown PGP executable."))
    (assq number elt)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-handle-result (&optional status)
  "Show `tinypgp-:buffer-tmp-shell' buffer if error, otherwise return result.
STATUS is Shell processes exit code.

Return
 '(buffer beg end)   or call error"
  (tinypgpd "tinypgp-binary-handle-result: in" status)
  (let* ((fid    "tinypgp-binary-handle-result")
         (action (tinypgp-hash 'action 'get 'now nil 'global))
         (elt    (if status (tinypgp-binary-exit-status-entry status)))
;;;      (sym    (if elt (nth 1 elt)))
;;;      (re     (if elt (nth 2 elt)))
         (ok     (if status (tinypgp-binary-exit-code-ok-p status)))
         error
         ret)

    (tinypgpd fid "Status" status "OK"  ok )

    ;;  There is one case where pgp return 0 status(ok): encrypt with
    ;;  multiple keys, but some key is not found from keyring.
    ;; --> I'd say this is fatal error
    ;;
    ;;  That's why we always check the verbal results in spite of STATUS

    (setq error (tinypgp-binary-check-error))

    (unless error
      (setq ret (or
                 ;;  verifying the message also unpacks
                 ;;  encrypted message if sig was good
                 ;;
                 (tinypgp-binary-get-result-verify)

                 (tinypgp-binary-get-result)
                 (and (string-match "decrypt" (symbol-name action))
                      (tinypgp-binary-get-result-decrypt)))))

    (tinypgpd fid
              "STATUS" status
              "elt" elt
              "error" error
              "POINTER" ret
              "ACTION" action)

    (cond
     ((and (null error)
           (not (null ret)))
      ret)
     (t
      (tinypgp-error (or error "No PGP output or error; huh?"))))))

;;}}}
;;{{{ PGP exe

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-binary-command-region-fix (cmd pointer)
  "If the CMD failed when PGP asked random bits, fix it. POINTER is PGP data."
  (tinypgpd "tinypgp-binary-command-region-fix in:" pointer )
  (with-current-buffer (car pointer)
    (when (ti::re-search-check
           "We need to generate \\([0-9]+\\)"  0 '(point-min))
      (tinypgpd "tinypgp-binary-command-region-fix done:" pointer "\n")
      (tinypgp-error "randseed.bin must be generated."))))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-send  (string)
  "Send STRING to open expect process."
  (expect-send (concat string (if (ti::win32-p) "\r" "\n"))))

;;; ---------------------------------------------------------- &engine ---
;;;
(defun tinypgp-binary-do-command-region-with-expect
  (cmd beg end o-buffer &optional msg ret-ptr)
  "Execute shell CMD on region BEG END with USER.

Input:

  CMD           str, full PGP command.
  BEG           int, region beg to feed to PGP
  END           int, region end to feed to PGP
  O-BUFFER      bfr, original buffer where BEG END are
  MSG           str, message
  RET-PTR       flag, instead of replacing previous content return pointer

Return:

  REGION END REPLACED   point at beg, if ret-ptr = nil
  POINTER               '(buffer beg end) if ret-ptr = non-nil"
  (let* ((fid           "tinypgp-binary-do-command-region-with-expect: ")
         (binary-process-input t)
         (out-p         (string-match "-o\\|#OUT" cmd))
         (orig-buffer   (current-buffer))
         pgp-error
         split
         bin-name
         args
         expect-start
         process pass
         out-buffer
         point

         ret
         pointer)

    (if (null binary-process-input)     ;quiet ByteCompiler
        (setq binary-process-input nil))

    (tinypgp-hash 'expect 'put 'process nil 'global)

    (tinypgp-do-shell-env
     (tinypgp-excute-in-tmp beg end     ;results in temp buffer

                            (if msg
                                (message msg))

                            (tinypgp-file-control 'all-kill)

                            (ti::pmin)
                            (tinypgp-file-control 'source-write)

                            (cond
                             ((string-match "#PIPE" cmd)
                              (setq cmd (tinypgp-cmd-compose cmd nil nil '(nil))))
                             ((string-match "#SOURCE-FILE" cmd)
                              (setq cmd (tinypgp-cmd-compose cmd nil nil '(nil)))))

                            ;;  If this command requires password, it contains marker #password
                            ;;  --> get the password from cache or ask from user.

                            (when (string-match "#password" cmd)
                              (setq cmd   (ti::replace-match 0 nil cmd) ;; Delete tag from command
                                    pass  (tinypgp-password-get)))

                            ;; The command is given as plain string. Explode it to individual
                            ;; arguments "pgp -s +batchmode=1" --> '("pgpg" "-s" "+batchmode=1")

                            (setq split         (split-string cmd "[ ]+")
                                  bin-name      (nth 0 split)
                                  args          (cdr split)
                                  out-buffer    (current-buffer))

                            (setq tinypgp-:last-pgp-exe-command cmd)

                            (tinypgpd fid "in:"
                                      "COMMAND"  cmd
                                      "CURRENT"  (current-buffer)
                                      "ORIG"     orig-buffer
                                      beg end
                                      "min-max" (point-min) (point-max)
                                      "MSG"     msg
                                      "RET-PTR" ret-ptr
                                      "BIN"     bin-name
                                      "ARGS"    args
                                      "CMD"     cmd)

                            (erase-buffer)

                            (setq expect-start (point-max)
                                  process (apply
                                           'start-process
                                           "PGP"
                                           out-buffer
                                           bin-name
                                           args))

                            (unwind-protect
                                (with-expect process

                                             (unless (ti::win32-p) ;; Unix is slower than NT, add delay
                                               (sit-for 0.3))

                                             (expect-cond

                                              ;; Error!  Unable to load string ENTER_PASSPHRASE

                                              ("Enter pass phrase:\\|ENTER_PASSPHRASE"
                                               (tinypgpd "Expect: triggered password prompt, sending it...")
                                               (unless (stringp pass)
                                                 (delete-process process)
                                                 (tinypgp-error "Internal error. No pass phrase available."))
                                               (sit-for 0.3) ;Small delay so that PGP is ready
                                               (tinypgp-send pass)

                                               (expect-cond
                                                ("Error: Bad pass phrase."
                                                 (interrupt-process process)
                                                 (setq pgp-error 'bad-pass-phrase))

                                                ("Enter pass phrase:"
                                                 (interrupt-process process)
                                                 (setq pgp-error 'bad-pass-ohrase))

                                                ("Cannot decrypt message.  It can only be decrypted by:"
                                                 (interrupt-process process)
                                                 (setq pgp-error 'cannot-decrypt)))))

                                             ;;  WRN:  WARNING: The above key is not trusted to belong to:
                                             ;;  WRN:  Mr. Foo <foo.site.com>
                                             ;;  QRY:  Do you want to use the key with this name? [y/N]

                                             (unless pgp-error
                                               (expect-cond
                                                ("Do you want to use the key with this name"
                                                 (tinypgpd "Expect: Use this kay ok...")
                                                 (tinypgp-send "y"))))

                                             (unless pgp-error
                                               (expect-cond
                                                (exit
                                                 (delete-process process))

                                                (timeout
                                                 (tinypgpd "Expect: timeout")
                                                 (delete-process process)
                                                 (error
                                                  (substitute-command-keys
                                                   (concat
                                                    "Expect: timeout occurred: send bug report "
                                                    "\\[tinypgp-submit-bug-report]"))))

                                                )) ;; expect-cond

                                             ;;  Killing killed process won't hurt. Make sure the
                                             ;;  Expect-cond didn't fall through.

                                             (delete-process process)

                                             ;; ......................................... read results ...

                                             (cond
                                              (pgp-error
                                               (tinypgpd "Expect: Terminated on error" pgp-error)
                                               (tinypgp-error
                                                (format "Expect error %s" (symbol-name pgp-error))))

                                              ((null out-p)
                                               (insert "\n")
                                               (setq point (point)))

                                              (t
                                               (tinypgpd "Expect: reading input"
                                                         (current-buffer)
                                                         out-buffer
                                                         tinypgp-:file-output)

                                               ;;  Expect may move us out of the buffer

                                               (unless (eq (current-buffer) out-buffer)
                                                 (if (buffer-live-p (get-buffer out-buffer))
                                                     (set-buffer out-buffer)
                                                   (error "Expect: Can't insert data: buffer has changed")))

                                               ;;  point is nil if there was no output file in
                                               ;;  this command, so the eq test will work in those
                                               ;;  cases too.

                                               (if (eq (point) point)
                                                   (tinypgp-error "Expect: no output from PGP"))

                                               (insert "\n")
                                               (setq point (point))

                                               (if (file-exists-p tinypgp-:file-output)
                                                   (insert-file-contents tinypgp-:file-output)
                                                 (if (buffer-live-p (get-buffer out-buffer))
                                                     (pop-to-buffer out-buffer)
                                                   (error "No expected output-file %s "
                                                          tinypgp-:file-output))))) ;; end of cond

                                             ;; ...................................... handle results ...

                                             (tinypgp-hash 'expect 'put 'point point 'global)

                                             ;;   Remove possible ^M chars

                                             (ti::buffer-lf-to-crlf 'dos2Unix 'doReadOnly)
                                             (setq pointer (list (current-buffer) point (point-max)))

                                             (tinypgpd "Expect: pointer" pointer)

                                             (when (eq point (point-max))
                                               (tinypgp-error "No output from pgp"))

                                             (tinypgp-file-control 'source-kill)))))

    (unless (eq (current-buffer) orig-buffer) ;Restore buffer we left
      (set-buffer orig-buffer))

    (cond
     (ret-ptr
      (setq ret pointer))
     (t
      (goto-char beg)
      (delete-region beg end)
      (insert-buffer-substring
       (car pointer) (nth 1 pointer) (nth 2 pointer))
      (goto-char beg)))

    ret))

;;; ---------------------------------------------------------- &engine ---
;;;
(defun tinypgp-binary-do-command-region
  (cmd beg end o-buffer &optional msg ret-ptr)
  "Execute shell CMD on region BEG END with USER.

Input:

  CMD           string,  full PGP command.
  BEG           integer, region beg to feed to PGP
  END           integer, region end to feed to PGP
  O-BUFFER      buffer,  original buffer where BEG END are
  MSG           string,  message
  RET-PTR       flag,    instead of replacing previous content return pointer

Return:

  REGION END REPLACED   point at beg, if ret-ptr = nil
  POINTER               '(buffer beg end) if ret-ptr = non-nil

References:

  `tinypgp-:pgp-sh-exe'
  `tinypgp-:last-pgp-exe-command'
  `tinypgp-:file-output'
  `tinypgp-:file-source'"
  (let* ((fid           "tinypgp-binary-do-command-region: ")
         (action        (tinypgp-hash 'action 'get 'now nil 'global))
         (loop          t)
         (final-newline "\n")
         (binary-process-input t)
         status
         ret pointer pointer-orig)

    (if (null binary-process-input)     ;quiet ByteCompiler
        (setq binary-process-input nil))

    (tinypgp-do-shell-env

     (if msg
         (message msg))

     (tinypgpd fid "in:" (current-buffer)
               beg end  "min-max" (point-min) (point-max)
               "MSG" msg
               "ACTION" action
               ret-ptr)

     ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  loop start ...
     (while loop                        ;If we should repeat the task?
       (setq loop nil)
       (tinypgpd fid "loop-beg" loop (current-buffer) cmd "\n")

       (tinypgp-excute-in-tmp beg end   ;results in temp buffer

;;;      (pop-to-buffer (current-buffer)) (ti::d! "DOING PGP")

                              (setq pointer-orig (list (current-buffer) (point-min) (point-max)))

                              (ti::pmin)
                              (tinypgp-file-control 'source-write)

                              ;;  PGP: Cannot use INPUT file as a parameter to pgp, but
                              ;;  we must feed the file through pipe to pgp. Fix some
                              ;;  commands.

                              (when (tinypgp-backend-pgp2-p)
                                (cond
                                 ((string-match "#PIPE" cmd)
                                  (setq cmd (tinypgp-cmd-compose cmd nil nil '(nil))))
                                 ((string-match "#SOURCE-FILE" cmd)
                                  (setq cmd  (ti::replace-match 0 nil cmd))
                                  (setq cmd  (concat " #PIPE " cmd))
                                  (setq cmd (tinypgp-cmd-compose cmd nil nil '(nil))))
                                 (t
                                  (setq cmd (concat " #PIPE " cmd))
                                  (setq cmd (tinypgp-cmd-compose cmd nil nil '(nil))))))

                              (tinypgpd fid "last-cmd:" (current-buffer) cmd )

                              (setq tinypgp-:last-pgp-exe-command cmd)

                              (if tinypgp-:do-command-region-before-hook
                                  (run-hook-with-args-until-success
                                   'tinypgp-:do-command-region-before-hook
                                   cmd msg ret-ptr))

                              (erase-buffer)

                              ;; ............................................. save command ...

                              (when nil ;;  only t if development version
                                (with-temp-buffer
                                  (let ((file "~/.tinypgp-cmd"))
                                    (insert cmd "\n")
                                    (write-region (point-min) (point-max) file)
                                    (ti::file-mode-protect file))))

                              ;; .............................................. run command ...

                              (ti::file-delete-safe tinypgp-:file-output)

                              (setq status
                                    (shell-command cmd (current-buffer)))

                              ;;  If there is output file (which was not sent stdout),
                              ;;  then read it. This happens with GPG, which is unable to send
                              ;;  to stdout, if stdin is used for password.

                              (when (file-exists-p tinypgp-:file-output)
                                (ti::pmax)
                                (tinypgpd fid "READING OUTPUT FILE" tinypgp-:file-output)
                                (insert-file-contents-literally tinypgp-:file-output))

                              (tinypgpd fid "SHELL-STATUS" status)

                              (if tinypgp-:do-command-region-after-hook
                                  (run-hook-with-args-until-success
                                   'tinypgp-:do-command-region-after-hook
                                   cmd msg ret-ptr))

                              ;;   sometimes PGP need new randseed file, this generates it
                              ;;   and runs the command again.
                              ;;
                              ;;   WinNT: If PGP tries to ask for ranadseed, it hangs whole emacs.

                              (when (and t ;enable for now..
                                         (not (ti::win32-p))
                                         (tinypgp-binary-command-region-fix cmd pointer-orig))
                                (setq loop t))

                              ;;  Arggh, When decrypting message in WinNT with 2.6.x The output
                              ;;  is not correct: there is extra "..." at the end of DATA.
                              ;;
                              ;;     Pass phase is good. Just a moment...DATA-DATA
                              ;;     ...

                              (when (ti::win32-p)
                                (ti::pmax)
                                (if (not (eq 0 (skip-chars-backward ".")))
                                    (delete-region (point) (line-end-position))))

                              ;;   Remove possible ^M chars
                              (ti::buffer-lf-to-crlf 'dos2Unix 'force)))

     ;; ... ... ... ... ... ... ... ... ... ... ... ... ... .. loop end ...

;;;    (tinypgp-binary-insert-command-log)
;;;     (pop-to-buffer (car pointer-orig)) (ti::d! 1234)

     (setq pointer (tinypgp-binary-handle-result status))

     ;;     We kill these only after the results have been examined,
     ;;     because user may want to check the contents if error happend.

     (tinypgp-file-control 'password-kill) ;Remove password file
     ;;     (tinypgp-file-control 'source-kill)

     ;;  For some reason PGP does not output final newline
     ;;  after its TAGS. Check this and add it, otherwise replacing
     ;;  the buffer content doesn't go right.

     (when pointer
       (with-current-buffer (car pointer)
         (when (string= "---"
                        (buffer-substring
                         (nth 1 pointer) (+  3 (nth 1 pointer))))
           (goto-char (nth 2 pointer))
           (insert final-newline)
           (setq pointer
                 (list (current-buffer)
                       (nth 1 pointer)
                       (1+ (nth 2 pointer)))))))
     (cond
      (ret-ptr
       (setq ret pointer))
      (t
       (goto-char beg)
       (delete-region beg end)
       (insert-buffer-substring
        (car pointer) (nth 1 pointer) (nth 2 pointer))
       (goto-char beg)))
     ret)))

;;}}}

;;{{{ PGP public key 'find by'

;;; ..................................................... &pgp-key-get ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-finger-discard-by-regexp (string-or-list)
  "Discards some email addresses from STRING-OR-LIST.
See variable `tinypgp-:finger-discard-email-hook'"
  (let (ret)
    (tinypgpd "[tinypgp-:finger-discard-email-hook] in:"
              tinypgp-:finger-discard-by-regexp "#" string-or-list )

    (when string-or-list
      (if (not (stringp tinypgp-:finger-discard-by-regexp))
          (setq ret string-or-list)
        (dolist (x (ti::list-make string-or-list))
          (if (string-match tinypgp-:finger-discard-by-regexp x)
              (tinypgpd "[tinypgp-:finger-discard-email-hook doing]:" x )
            (push x ret)))))
    (nreverse ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-network-spawn (mode &optional arg1 arg2 verb)
  "Get key by fingering EMAIL.
Examine the returned information and ask user help if there is more than
one public key.

Input:

  MODE          'finger
  ARG1 ARG2     if 'finger then arg1 is email
                if 'http   then arg1 is host, arg2 is command
  VERB          flag, verbose messages.

Return:

 string         if only one public key
 (string)       internal finger error string

References:

 `tinypgp-:buffer-tmp-shell'     ,results of finger
 `tinypgp-:last-network-error'   ,if error happened."
  (let ((fid      "tinypgp-key-network-spawn:")
        (buffer   (tinypgp-ti::temp-buffer 'finger))
        (email    arg1)                 ;if command is finger
        stat
        data
        data2
        len
        ret)

    (tinypgpd fid "in:" mode arg1 arg2 verb)

    (setq tinypgp-:last-network-error nil)

    (cond
     ((eq mode 'finger)
      (setq stat (ti::process-finger email nil nil buffer verb)))
     (t
      (error "Wrong mode '%s' " mode)))

    (cond
     ((stringp stat)
      (setq tinypgp-:last-network-error stat)
      (setq ret (list stat)))

     ((bufferp stat)
      (ti::mail-pgp-trim-buffer)        ;Remove garbage around keys.

      (or (setq data
                (ti::mail-pgpk-public-get-region  nil nil buffer))
          (setq data2
                (ti::mail-pgpk-public-get-region  nil nil buffer 'relax)))

      (when data2
        (setq data data2))

      ;; I don't think people undertand this mail very well,
      ;; they only know how to do -kxa and -kv, not -fkxa
      ;;
;;;     (if (y-or-n-p
;;;          (concat
;;;           "Public key found, but not in full -fakx format "
;;;           "Send email notice? "))
;;;         (tinypgp-sendmail email 'pk-no-full-format))

      (setq len (length data))

      (cond
       ((and (eq  1 len)                ;only 1 public key found
             (not
              (null
               ;;  P-key block must not me empty
               (setq ret (nth 1 (car data))))))
        ret)

       ((null data)
        (ti::read-char-safe-until
         "finger ok, but no Public key in his ~/.plan file.[press]")
        (setq ret nil))

       (t
        ;; #todo
;;;     (ti::d! "FSTAT" stat  (length data))
        (error "Multiple keys not implemented yet.")))))

    (tinypgpd fid email ret )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-finger-guess-email ()
  "Check Whole buffer for PGP email address.
Return:
  nil                   Nothing cound
  email"
  (let* ((set           "[^ \t\n<=\"';:]+")
         (email-re      (concat "\\(" set "@" set "\\)"))
         ;;  finger ssjaaa@uta.fi | pgp -fka for pgp key
         ;;
         (kaf-re        "[ \t]*|[ \t]*pgp[ \t]+-\\(fka\\|kaf\\|afk\\|fak\\)")
         email
         line
         list)

    (save-excursion
      (cond

       ((and
         buffer-read-only               ;Incoming message RMAIL
         (ti::pmin)
         ;; X-Pgp-Signed:
         ;;     access-type=Finger; Address=foo@site.com;
         ;;
         (setq list   (tinypgp-xpgp-get-info))
         (setq email (assoc "address" list))))

       ((and
         (ti::pmin)
         ;;  If's faster first look for simple regexp, and match
         ;;  it against complex regexp
         ;;
         (re-search-forward kaf-re nil t)
         (setq line (ti::read-current-line))
         (string-match (concat email-re kaf-re) line)
         (setq email (ti::remove-properties (match-string 1 line)))))

       ((and
         (ti::pmin)
         (re-search-forward
          (concat
           "public.*key.*@\\|@.*public.*key"

           ;; |Boudewijn Visser|E-mail:visser@ph.tn.tudelft.nl |finger for |
           ;; |University of Technology                        |PGP-key    |
           ;;
           "\\|@.*finger\\|finger.*@"

           ;;  steve*windsong.demon.co.uk (for which PGP is preferred)
           ;;
           "\\|@.*pgp.*prefered\\|pgp.*prefered.*@")
          nil t)
         (setq line (ti::read-current-line))
         (string-match email-re line)
         (setq email (ti::remove-properties (match-string 1 line)))))))

    (tinypgpd "tinypgp-key-finger-guess-email out:" email )

    email))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-finger-add (email &optional no-ask)
  "Ask where to store the public key for EMAIL; optionally NO-ASK.

Return
  non-nil     if added
  nil"
  (let ((finger-buffer      tinypgp-:buffer-tmp-finger)
        ans)
    (cond
     (no-ask
      ;;  Put into temporary keyring ... #todo
      (error "Not supported no-ask"))
     (email
      (setq ans
            (tinypgp-pubring-alias2file
             (tinypgp-pubring-complete
              (format
               "%s: Store the public key to pubring[empty=cancel]: "
               (or (car-safe (ti::mail-email-from-string email))
                   (ti::string-left email 20))))))
      (if (ti::nil-p ans)
          (setq ans nil)
        ;; #todo, should add the key to keyring.
        ;;
        (with-current-buffer finger-buffer
          ;;  (tinypgp-key-add-region-interactive)
          (tinypgp-key-add-region-batch (point-min) (point-max)))
;;;     (ti::d! "Cacheing finger>>" ans)
        ;;  email, keyring
        (tinypgp-key-cache 'put email ans))))
    ans))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-finger-verbose (email-list)
  "EMAIL-LIST. See `tinypgp-key-find-by-finger'."
  (tinypgp-key-find-by-finger email-list nil 'verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-finger (&optional email-list no-ask  verb)
  "Find a PGP key using finger.

The exact references searched are like:

  finger foo@site.com for pgp public key
  finger foo@site.com | pgp -fka
  ...

If finger fails then user is offered a list of all email
addresses and each one selected is fingered.

Input:
  EMAIL-LIST        ,if this is given, then do not search
                     current buffer for email addresses.
                     All entries that do not contain @ are filtered out.
                     This can be string list or single string
  NO-ASK            ,store all fingered keys without asking
                     to current keyring.
  VERB              ,enable verbose messages

Return:
  string    ,pgp publick key block
  nil"
  (interactive)
  (tinypgpd "tinypgp-key-find-by-finger in:")
  (let* (email
         stat
         ans
         list
         ret)

    (ti::verb)

    ;; ... ... ... ... ... ... ... ... ... ... ...  exact match search ...

    (if email-list
        (setq list (ti::list-make email-list))

      ;; ... ... ... ... ... ... ... ... ... ... ... .. list not given . .
      (setq email (tinypgp-key-finger-guess-email)
            email (tinypgp-email-discard-default list)
            email (tinypgp-finger-email-filter email))

      (if (ti::listp email)
          (setq email (car email)))

      (when email                       ;Try adding the exact match
        (setq ret (tinypgp-key-network-spawn 'finger email nil verb)))

      (if (stringp ret)
          (setq ret (tinypgp-key-finger-add email))
        (setq list
              (tinypgp-email-find-region
               (point-min)
               ;;  For large buffers, look only the start
               ;;  of buffer. The point-min offset is
               ;;  needed because buffer may be narrowed (RMAIL)
               ;;
               (if (> (point-max) (+ (point-min) 1000))
                   (+ (point-min) 1000)
                 (point-max))))))

    (if (and list tinypgp-:finger-discard-email-hook)
        (setq list (run-hook-with-args-until-success
                    'tinypgp-:finger-discard-email-hook list)))

    (when list
      (setq list (tinypgp-email-discard-default list))
      (setq email (car-safe list)))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... . loop-finger . .
    (while (and list
                (not (stringp ret))
                (not (ti::nil-p email)))

      (when (null no-ask)
        (setq
         ans
         (completing-read
          (format "%sFinger [e(x)it, !, empty=skip]: "
                  (if (> (length list) 1)
                      (format "%d: " (length list)) ""))
          (ti::list-to-assoc-menu list) nil nil email))

        (cond
         ((string= "!" ans)
          (setq no-ask t))

         ((string= "x" ans)
          (setq list  nil
                email nil))

         ((ti::nil-p ans)
          (setq email nil))

         (t
          (setq email ans))))

      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... results ...

      (when (not (ti::nil-p email))
        (setq stat (tinypgp-key-network-spawn 'finger email nil t))

        (cond
         ((and (ti::listp stat)  verb)
          (message (format "[press]Finger internal error: %s" (car stat)))
          (sit-for 3)
          (discard-input))

         ((stringp stat)
          (setq ret stat)
          (if verb
              (message "Fingered PGP key found."))))
        ;;  Used, remove
        (setq list (delete email list)))

      ;; ................................................ go to next ...
      (unless ret                       ;not found yet?
        (if email
            (setq list (delete email list)))
        (setq  email (car list)
               list  (cdr list))))

    (if ret
        (tinypgp-key-finger-add email no-ask))

    (tinypgpd "tinypgp-key-find-by-finger out:" ret )

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-http-study-buffer (&optional buffer)
  "Search public key from HTTP keyserver request result BUFFER."
  (let* ()
    (tinypgpd "tinypgp-key-http-study-buffer in: " buffer (current-buffer))
    (with-current-buffer buffer
      (ti::mail-pgp-trim-buffer))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-url-verbose (&rest args) ;Called from hook
  "Call `tinypgp-key-find-by-http-url' interactively."
  (call-interactively 'tinypgp-key-find-by-http-url))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-url (url &optional verb)
  "Send http request and try to read key from URL page. VERB.

Interactive call note:

  This function searches only X-Pgp field for possible key location
  pointer in format Access-type=URL; URL=http://me.org/~me/pgp.html"
  (interactive
   (list (tinypgp-xpgp-key-address
          'http
          "(http) X-Pgp information is not present.")))

  (let* ((fid       "tinypgp-key-find-by-http-url: ")
         (buffer    (tinypgp-ti::temp-buffer 'http))
         (obuffer   (current-buffer))
         (win-count (length (ti::window-list)))
         stat
         ret)

    (ti::verb)
    (tinypgpd fid  "URL" url "VERB" verb)

    (when (stringp url)
      (setq stat (ti::process-http-request url nil nil buffer verb)))

    (tinypgpd fid "STAT" stat buffer)

    (cond
     ((and (nth 1 stat)
           verb)
      (message "Http internal error: %s" stat)
      (sit-for 2)
      (discard-input))

     ((bufferp (setq stat (car stat)))
      (pop-to-buffer stat)
      (ti::pmin)
      (if (setq
           stat
           (cond
            ((ti::mail-pgp-public-key-p (point-min))
             (if verb
                 (call-interactively 'tinypgp-key-add-region-batch)
               (tinypgp-key-add-region-batch (point-min) (point-max))))))
          ;;
          ;; See tinypgp-key-add-region-batch documentation
          ;;
          (setq ret tinypgp-:return-value)

        ;;  If user had only 1 window visible, make this 'new' buffer
        ;;  small. But if he had more windows, don't shrink the
        ;;  just shown buffer (it shocks if your window settings are
        ;;  modified !)
        ;;
        (when (eq win-count 1)
          (shrink-window-if-larger-than-buffer))

        ;; Keep cursor in the original buffer
        ;;
        (pop-to-buffer obuffer)

        (if verb
            (message "Http request didn't find public key."))))) ;cond end
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-keyserver-i-args (&optional string)
  "Ask args for function `tinypgp-key-find-by-http-keyserver'.
If STRING is already know then do not ask for it.

Return:
  (srv cmd str)"
  (let* ((fid       "tinypgp-key-find-by-http-keyserver: ")
         (dummy     (tinypgpd fid "in: "))

         (to-field  (if buffer-read-only ;; RMAIL VM
                        (mail-fetch-field        "from")
                      (mail-fetch-field  "to"))) ;; mail buffer

         (line-end-position      (or (ti::mail-hmax) (point-max)))

         (elist     (tinypgp-email-find-region
                     (point-min)

                     ;;  Search up till character limit 3000
                     (if (> line-end-position (+ (point-min) 3000))
                         (+ (point-min)3000) line-end-position)))
         (key-id     (tinypgp-key-id-find))
         elt
         srv
         cmd)

    (if dummy (setq dummy t))           ;No-op, byte-comp silencer.

    (tinypgpd fid to-field key-id elist
              (current-buffer) (point-min) line-end-position)

    (if to-field
        (setq to-field (car (ti::mail-email-from-string to-field))))

    (if (setq elt (tinypgp-ask-http-keyserver))
        (setq srv (nth 0 elt)
              cmd (nth 1 elt))
      (error "Internal."))              ;should not happen

    (if key-id                        ;Add this to completion list too
        (push key-id elist))

    (or string
        (setq string
              (completing-read
               "Search string, no spaces: "
               (ti::list-to-assoc-menu elist) nil nil
               (if key-id
                   key-id
                 (ti::remove-properties
                  (or to-field (ti::buffer-read-space-word))))
               'tinypgp-:history-http-keyserver-string)))
    (list srv cmd string)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-keyserver-verbose (string)
  "See `tinypgp-key-find-by-http-keyserver'. STRING."
  (let ((a (tinypgp-key-find-by-http-keyserver-i-args string)))
    (tinypgp-key-find-by-http-keyserver (nth 0 a) (nth 1 a) string 'verb)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-keyserver
  (server command string &optional verb)
  "Send http request to keyserver to get a key.

Interactive note:

  All email addresses are read from `point-min' to end of
  current line where your cursor sits. The default search string
  offered is read from the To field but you can delete the prompt
  and enter other found email addresses via Tab completion.

Functional note:

  Please understand that waiting for a HTTP response may be painfully
  slow many times. If you can, prefer the finger and instruct
  poeople to include their publick key information in the
  $HOME/.plan file in full -fkax format.

Input:

  SERVER        www.xx.com
  COMMAND       command to run in server
  STRING        the search string without spaces. If this parameter is nil
                or contains spaces, thi function returns immediately.
  VERB          Verbose messages.

Return:

  keyring       If htttp call succeeded and key was inserted to some keyring
  nil           no keys added or found"
  (interactive (tinypgp-key-find-by-http-keyserver-i-args))
  (tinypgpd "tinypgp-key-find-by-http-keyserver in: " string )

  (unless (ti::nil-p string)
    (let* ((cmd      (format (concat "http://%s" command) server string)))
      (ti::verb)
      (tinypgpd "tinypgp-key-find-by-http-keyserver cmd: " cmd)
      (tinypgp-key-find-by-http-url cmd verb))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-http-guess ()
  "Select X-pgp URL if it exists or suggest keyserver search.
This function is stricly for interactive use."
  (interactive)
  (let* ((url (tinypgp-xpgp-key-address 'http))
         tried
         ret)
    (if (and url
             (setq tried (y-or-n-p "X-Pgp key url found; obey it ")))
        (setq ret (tinypgp-key-find-by-http-url url 'verb)))

    (unless ret
      (cond
       (tried
        (message "No luck, Inform person about possible defective X-URL")
        (sit-for 1.5))
       (t
        (setq ret (call-interactively 'tinypgp-key-find-by-http-keyserver)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-email (email-srv string)
  "Send email to nearest Public key mail service to get the Key.
Notice that this sends _mail_ and doesn't return any values.

This function should not be put into any key find nook, but
called by user with clear intention to find key as last resort.

Input:
  EMAIL-SRV     full string placed in To: field where to send the
                request.
  STRING        what to request from the server normally
                \"FirstName Surname\""
  (interactive
   (let (srv
         string)
     (setq srv (tinypgp-ask-email-keyserver))
     (setq                              ;ARG 2
      string
      (read-from-minibuffer "Search string [firstname surname]: "))
     (list srv string)))

  ;; ... ... ... ... ... ... ... ... ... ... ... ... .. function start . .
  (let* (cmd)

    (if (or (ti::nil-p email-srv)
            (ti::nil-p string)
            (not (string-match "@" email-srv)))
        (error "Invalid arguments."))

    (setq cmd (format "GET %s" string))

    (ti::mail-sendmail-macro  email-srv cmd 'send
                              (insert cmd "\n"))))

;;; ----------------------------------------------------------------------
;;; - We don't make this a macro! It could be installed into hooks...
;;;
(defun tinypgp-key-find-by-cache (string &optional who)
  "Check cache for STRING.

Input:
  STRING   string to find
  WHO      who calls this function (for debug purposes)

Return:
  pubring
  nil"
  (tinypgpd "tinypgp-key-find-by-cache: " string who)
  (if (stringp string)
      (tinypgp-key-cache 'get string)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-key-cache-update (&optional user)
  "Update cache with current USER/pubring parameters."
  (setq user (or user tinypgp-:user-now))

  (tinypgpd "tinypgp-key-cache-update: " user tinypgp-:pubring-now )

  ;;  The USER must at least contain 3 character, it's no use to
  ;;  cache 2 character user, because that may be a bug in program
  ;;
  (if (> (length user) 2)
      (tinypgp-key-cache 'put user tinypgp-:pubring-now)
    (error "TinyPgp: cache update internal error %s" user)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-keyrings-1 (string-or-list)
  "Search all keyrings and cache.

Input:
  STRING-OR-LIST    string or list of search strings, first one found
                    is used.

Return:
 (string . keyring) STRING is the one in string-or-list that was found
                    first.
 nil"
  (let* ((tinypgp-:pubring-now  tinypgp-:pubring-now)
         (fid                   "tinypgp-key-find-by-keyrings-1:")
         list
         kring
         ret
         search-string)
    (tinypgpd fid "in:" string-or-list)
    (setq string-or-list (ti::list-make string-or-list))

    (dolist (search-string string-or-list) ;; #todo: Can't use dolist/2 loop
      (setq list (tinypgp-pubring-list))

      (tinypgp-save-state-macro
       (with-current-buffer (tinypgp-ti::temp-buffer 'shell)
         (dolist (kring list) ;; #todo: Can't use dolist/2 loop
           (if (not (file-exists-p kring))
               (error "\
Check tinypgp-:pubring-table/Config error, no exist '%s'" kring))

           (setq tinypgp-:pubring-now kring) ;Search this

           (when (tinypgp-key-info-insert search-string)
             ;;  That's it, stop the loop by setting list to nil
             (setq ret  (cons search-string kring)
                   string-or-list  nil   ;Stop loop 1
                   list            nil)  ;Stop loop 2
             (tinypgp-key-cache 'put search-string kring))))))
    (tinypgpd fid "out:" search-string kring ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-keyrings-verbose (string)
  "See `tinypgp-key-find-by-keyrings'. STRING."
  (tinypgp-key-find-by-keyrings string 'verb))

(defun tinypgp-key-find-by-keyrings (string &optional verb)
  "Try all available keyrings and try to find the public key.
If pubring file searched does not exist, signal error.

Input:

  STRING    ,search string
  VERB      ,if non-nil, then ask for search string if STRING search fails.

Note:

  This function caches the pubring and string information
  The cache is always looked first, before doing any outside search.

Sets global

  `tinypgp-:return-value' and property 'find-by-keyrings

  If you call this function with argument VERB
  user can change the search STRING. if the user's string is found
  from the keyrings then the original STRING is changed. The
  property has value nil if STRING is original or
  it has the user's input value if that match was found.

  You need the information if you try to encrypt with key
  xxx@foo.site.com and user changes it to 'doodle'. Then if
  'doodle' is found, you should use that for encryption and not
  the original xxx@foo.site.com

Return:

  string    public keyring
  nil"
  (let ((fid  "tinypgp-key-find-by-keyrings:")
        (loop t)
        ret)

    (tinypgpd "tinypgp-key-find-by-keyrings in:" string )
    (put 'tinypgp-:return-value 'find-by-keyrings nil)

    (while (and loop (null ret))
      (setq loop nil)                   ;User sets this 't' if retry

      (or (setq ret (tinypgp-key-find-by-keyrings-1 string))
          (and verb
               (ti::mail-mail-p)
               (progn
                 (message "\
Hm, Consider using tinypgp-email-substitution-add in tinypgp rc file: TO hdr")
                 (sit-for 5)

                 (setq
                  string
                  (completing-read
                   (format
                    "[%s] No keyring, try another string? : "
                    string)
                   (ti::list-to-assoc-menu (tinypgp-email-make-choices string))))
                 (if (ti::nil-p string) ;RET pressed --> ""
                     nil
                   (setq loop t)
                   (tinypgpd fid "RETRY" string)
                   (setq ret (tinypgp-key-find-by-keyrings-1 string)))))))

    ;;  tinypgp-key-find-by-keyrings-1 return cons cell
    ;;
    (when (ti::listp ret)
      (put 'tinypgp-:return-value 'find-by-keyrings (car ret))
      (setq ret (cdr ret)))

;;;    (ti::d! "fbk" (get 'tinypgp-:return-value 'find-by-keyrings))
    (tinypgpd "tinypgp-key-find-by-keyrings out:" string ret )
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-find-by-guess (string &optional verb)
  "Try to determine where to get the Public key-id STRING.
For best results, the STRING should be in 0xFFFFF format to
uniquely match single person. Second best choice is full email address.
VERB activates verbose messages.

The order of search depends on the variable:

    `tinypgp-:find-by-guess-hook'

Which is list of functions."
  (interactive
   (list
    (ti::string-remove-whitespace
     (read-from-minibuffer
      "Search string: "
      (ti::string-remove-whitespace
       (or (ti::mail-get-field "from" nil 'null-mode)
           (ti::mail-get-field "to"   nil 'null-mode)))))))
  (let* ((fid "tinypgp-key-find-by-guess:")
         ret)
    (ti::verb)
    (if (ti::nil-p string) (error "Invalid arg"))

    ;;  Is there substitution for this ?
    ;;
    (setq ret (car-safe (tinypgp-key-id-conversion string)))
    (tinypgpd fid "in: STRING" string "key-subst" ret verb)

    (if ret (setq string ret))

    (tinypgpd fid "RUN HOOKS" tinypgp-:find-by-guess-hook)

    (setq ret (run-hook-with-args-until-success
               'tinypgp-:find-by-guess-hook string))

    (if ret (tinypgp-key-cache 'put string ret))

    (when verb
      (cond
       (ret
        ;;  maybe the previous call cached they KEY whose indicator "k"
        ;;  is not shown in modeline. Show "k" now
        ;;
        (tinypgp-update-modeline)
        (message "TinyPgp Guess found: [%s] keyring %s"
                 string
                 (file-name-nondirectory ret)))
       (t
        (message "TinyPgp Guess failure: (maybe converted) %s" string))))
    ret))

;;}}}
;;{{{ PGP key management

;;; ......................................................... &pgp-key ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-ring-at-point (&optional alias)
  "See if there is keyring by looking backward.
First empty line terminates search. Eg.

Key ring: '/users/jaalto/.pgp/pubring.pgp', looking for
user ID \"foo@site.com\".
Type Bits/KeyID    Date       User ID
pub  1024/20378F71 1995/08/19 Mr. foo  <foo@site.com>

Input:
 ALIAS   ,flag, return keyring alias name"
  (let (ret)
    (save-excursion

      ;;  move away from empty line
      ;;
      (if (looking-at "^[ \t]*$")
          (forward-line -1))

      (while (and (not (bobp))
                  (not (looking-at "^[ \t]*$"))
                  (null ret))
        (if (looking-at ".*Key ring:[ \t]+'\\([^']+\\)")
            (setq ret (match-string 1)))
        (forward-line -1)))

    (if alias
        (setq ret (tinypgp-pubring-file2alias ret)))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinypgp-key-trust-ask (&optional id)
  "Ask trust parameter. If user gives empty line, 'undefined' is returned.
ID is user-id."
  (let ((ans
         (completing-read
          (format "%s%strust parameter? " (or id "") (if id " " ""))
          (ti::list-to-assoc-menu
           '("undefined" "untrusted" "marginal" "complete"))
          nil 'match-it
          "undefined")))
    (if (ti::nil-p ans)
        "undefined"
      ans)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-info-insert-current-user ()
  "Insert current user's key information to point.
The current pubring is set temporarily to first
entry in `tinypgp-pubring-table'."
  (tinypgp-save-state-macro
   (setq tinypgp-:pubring-now (nth 1 (car (tinypgp-pubring-table))))
   (tinypgp-key-info-insert tinypgp-:user-now 'verb)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-info-insert (string &optional verb)
  "Run pgp -kvc to get key information matching the STRING.
Insert the content to current point. VERB allows verbose messages.

References:
  `tinypgp-:buffer-tmp-shell'

Return:
  t         something inserted
  nil       error condition"
  (interactive
   (list
    (read-from-minibuffer
     (format "insert key matching [pubring: '%s']: "
             (or (tinypgp-pubring-file2alias tinypgp-:pubring-now)
                 "<unknown>")))))

  (barf-if-buffer-read-only)
  (tinypgpd "tinypgp-key-info-insert in: " string verb )

  (let* ((bcmd  (tinypgp-binary-get-cmd 'key-info)) ;;base command
         (cmd   (tinypgp-cmd-compose bcmd nil nil '(nil)))
         (fid   "tinypgp-key-info-insert: ")
         shell-cmd
         buffer
         ret)

    (ti::verb)

    (cond
     ((and (string-match "[ \t]" string) (ti::win32-p))
      (error "STRING must not contain whitespace in WInNT"))
     (t
      (setq string (format "\"%s\"" string))))

    (tinypgp-do-shell-env
     (with-current-buffer (setq buffer (tinypgp-ti::temp-buffer 'shell))
       (setq shell-cmd (format "%s %s" cmd string))
       (tinypgpd fid "run: " shell-cmd)
       (shell-command shell-cmd buffer)
       ;;       (pop-to-buffer (current-buffer)) (ti::d! "::key" string)

       (ti::pmin)
       (cond
        ((re-search-forward "0 matching keys found\\." nil t)
         (if verb
             (message "0 matching keys found.")))
        (t
         (setq ret t)))))

    (if ret
        (insert-buffer buffer))

    (tinypgpd fid "out: " ret )

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-info-at-point-show (string &optional pubring-list)
  "Find match using STRING from current keyring or PUBRING-LIST.
When called interactively, read space-separated-word
under point and find matches from current keyring and
show them in temporary buffer."
  (interactive
   (let (str)
     (setq str
           (read-from-minibuffer
            "Display key info matching: "
            (if (null (setq str (ti::buffer-read-word "-0-9a-zA-Z@.")))
                ""
              ;; If  underlying word is Key-id 12345678, then
              ;; prepend 0x to it because that is only valid search string
              ;;
              (if (and (eq (length str) 8)
                       (string-match "^[0-9A-Z]+$" str))
                  (concat "0x" str)
                (ti::string-left str 35)))
            nil
            nil
            'tinypgp-:history-key-info))
     (list str (tinypgp-pubring-list))))

  (let ((tmp   (tinypgp-ti::temp-buffer 'show)))
    (if (not (stringp string))
        (error "Arg error")

      (with-current-buffer tmp
        (tinypgp-save-state-macro
         (dolist (elt pubring-list)
           (setq tinypgp-:pubring-now elt)

           (insert "\n" elt ":")

           (beginning-of-line)
           (if (looking-at "^.*/\\(.*:\\)")
               (tinypgp-highlight 'match 1))
           (end-of-line)  (insert "\n")

           (tinypgp-key-info-insert string)
           (ti::pmax))))
      (pop-to-buffer tmp)
      (ti::pmin))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-add-region-batch (beg end &optional noerr verb)
  "Add all public keys in region to active keyring.
The region is handled by PGP directly. No checkings are done here.

Input:

  BEG END   region
  NOERR     if nil, then signal error if PGP reports error.
  VERB      allow verbose messages

Interactive call:

  The region is cecked for public key. If none exist offer using
  whole buffer.

Sets global:
  `tinypgp-:return-value'   pubring where the key was inserted

Return:

  string
  nil           no keys were added"

  (interactive
   (progn
     (ti::compat-activate-region)       ;Make sure user sees region
     ;; Check this before going further
     (if (null (ti::mail-pgp-public-key-p (point-min)))
         (error "No public key area in buffer"))
     (ti::i-macro-region-ask
      "No region selected, use whole buffer for key insert? ")))

  (tinypgpd "tinypgp-key-add-region-batch in: pring"
            tinypgp-:pubring-now (current-buffer) )

  (let* ((tinypgp-:pubring-now tinypgp-:pubring-now) ;make local copy
         (logical-cmd 'key-add)
         (bcmd      (tinypgp-binary-get-cmd logical-cmd)) ;;base command
         (copy      (tinypgp-ti::temp-buffer))
         (buffer    (tinypgp-ti::temp-buffer 'shell))
         (i         0)
         cmd
         pring
         ret)

    (ti::verb)
    (tinypgpd "tinypgp-key-add-region-batch in:"
              (current-buffer) beg end bcmd cmd )

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . verbose part ..
    ;;  Many times there may be old region active and user doesn't
    ;;  realize that,. Do little check first...

    (when verb
      (when (and (not (ti::narrow-safe beg end
                        (ti::mail-pgp-public-key-p (point-min))))
                 (y-or-n-p
                  (concat
                   "Can't find public key block in region.. "
                   "Use full buffer [C-g to abort]")))
        (setq beg (point-min)  end (point-max)))

      (if (setq pring (tinypgp-pubring-ask))
          (setq tinypgp-:pubring-now pring)))

    ;; Only now can we compose the command: pubring is known or
    ;; set by user.
    ;;
    (setq tinypgp-:return-value tinypgp-:pubring-now)
    (setq cmd (tinypgp-cmd-compose bcmd nil nil '(nil)))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. do it ..
    (tinypgp-do-shell-env
     (save-window-excursion
       (append-to-buffer copy beg end)
       (append-to-buffer buffer beg end)
       (with-current-buffer buffer
;;;      (pop-to-buffer (current-buffer)) (ti::d! 12345)
         ;;   Remove spaces: "intended PGP key", but only if there is
         ;;   only one key. Ignore "chop" if there is multiple keys.
         ;;
         (ti::pmin)
         (while (re-search-forward (ti::mail-pgp-pkey-begin-line) nil t) (incf  i))
         (if (eq 1 i) (ti::mail-pgp-chop-region (point-min) (point-max)))

         ;;  If there is error situation, the "after" hook runs.
         (tinypgp-mode-specific-control-before logical-cmd)

         (shell-command-on-region       ;This shows the buffer, gawk!
          (point-min) (point-max) (format "%s " cmd) buffer))))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  results ..
    (setq ret (tinypgp-binary-get-result-key-add))
    (when (and verb ret)
      (message "Key add note: %s" ret))

    (when (and (stringp ret)
               (string-match "error" ret))
      (if noerr
          (setq ret nil)
        (tinypgp-error ret)))

    (when (and (stringp ret)
               (string-match "No keys found" ret))
      (setq ret nil))

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-add-region-interactive (beg end)
  "Parse BEG END and ask if key should be added to the active keyring."
  (interactive (ti::i-macro-region-ask))
  (tinypgpd "tinypgp-key-add-region-interactive in: pring"
            tinypgp-:pubring-now  (current-buffer) beg end )
  (let ((data   (ti::mail-pgpk-public-get-region
                 nil nil tinypgp-:buffer-tmp-shell))
        (verb  (interactive-p))
        id
        pkey)
;;;     trust

    (tinypgp-unfinished-function)
    (cond
     ((null data)
      (if verb
          (message
           "'Key for user ID:' tags not found to signify public key blocks.")))
     (t
      (dolist (elt data)
        (setq id (nth 0 elt)   pkey (nth 1 elt))
        (cond
         ((null pkey)
          (ti::read-char-safe
           (format "Public key empty: %s" (or id "<id not known>"))))

         ((y-or-n-p (format"Add: %s" id))
;;;       (setq trust (tinypgp-key-trust-ask id))
          (with-current-buffer (tinypgp-ti::temp-buffer)
            (insert pkey)
            (tinypgp-key-add-region-batch (point-min) (point-max))
            (error "#todo trust not set."))))))

)));;; ----------------------------------------------------------------------
;;; Called by TM.el
;;;
(defun tinypgp-key-extract-to-point-current-user ()
  "Extract `tinypgp-:user-now' key to current point."
  (tinypgp-key-extract-to-point  tinypgp-:user-now))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-extract-to-point (string &optional raw noerr)
  "Insert public key matching STRING to current point.

Note:

 If Pgp extracts file to some temporary file, that file will be deleted
 automatically, because the key will be available from emacs buffer.
 This prevents temporary files accumulating in your tmp directory.
 Also the line that containbs sentence

    Key extracted to file '/users/xxx/junk/pgptemp.$07'.

 is removed from the shell output buffer before yanking.

References:

 `tinypgp-:buffer-tmp-shell'

Input:

 STRING     string to search
 RAW        only insert the PGP block
 NOERR      if non-nil no error is signalled is string is not found,
            also the output is _not_ inserted to the current point,
            but returned.

Return:

 string
 nil"
  (interactive
   (list
    (read-from-minibuffer "Insert public key matching: " tinypgp-:user-now)
    current-prefix-arg))

  (barf-if-buffer-read-only)
  (tinypgpd "tinypgp-key-extract-to-point in: pring" tinypgp-:pubring-now )
  (let* ((fid   "tinypgp-key-extract-to-point:")
         (bcmd  (tinypgp-binary-get-cmd 'key-extract))
         (out   (tinypgp-ti::temp-buffer 'shell))

         cmd
         kring
         ret)

    (tinypgpd fid "in:" (current-buffer) string noerr )

    (unless (setq kring  (tinypgp-key-find-by-keyrings string))
      (error "No PGP key for '%s'" string))

    (tinypgpd fid "cmd,out,kring" cmd out kring )

    (tinypgp-save-state-macro
     (setq tinypgp-:pubring-now kring)
     (setq cmd (tinypgp-cmd-compose bcmd nil nil '(nil)))
     (setq cmd (format "%s '%s'" cmd string)))

    (tinypgp-do-shell-env (shell-command cmd out))

    (with-current-buffer out
      (ti::pmin)
      (when (and (null (setq ret (ti::mail-pgp-pkey-read raw 'kill-file)))
                 (null noerr))
        ;;  Remove cache entry, maybe user has moved the key
        ;;  to another keyring?
        ;;
        (tinypgp-key-cache-remove-entry string)
        (pop-to-buffer out)
        (error "\
PGP error; Maybe cache has old keyring information? Check cache.")))

    (when (and ret
               (null noerr))
      (insert ret))

    (tinypgpd fid "out:" ret )

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-delete-region (beg end &optional mode plist verb)
  "Remove all keys from keyring that are found from region.
The picked key items are:
o   all email addresses
o   All regular pgp key lines \"pub   512/47141D35 1996/06/03 ...\"

In interactive or verb mode, all removed KeyId's used are marked
with overlays after command completes. Overlays have property '(owner tinypgp)

Input:
 BEG END
 MODE       nil         = key id 0x based deletion
            1 or 'email = key id email based deletion
            2 or 'any   = both methods used.
 PLIST      list of pubring filenames to touch.
            Interactive call:
              all public keyring are stepped through;
              permission to use the pubring is asked from user.
            Lisp call:
              If this list oi nil, active pubring is used
 VERB       Enable verbose asking/message mode.

If VERB is non-nil, error is generated if it happens. if VERB is nil,
then the possible error string is returned."
  (interactive
   (let* ((plist  (list tinypgp-:pubring-now))
          (kring  (or (tinypgp-key-ring-at-point 'alias)
                      (tinypgp-pubring-file2alias tinypgp-:pubring-now)))
          ans
          reg)
     (if (not (region-active-p))
         (error "Region not selected.")
       (setq reg (ti::i-macro-region-ask)))

     (setq
      ans
      (tinypgp-pubring-complete
       (format
        (concat
         "%sDel keys from all prings or one ring? "
         "[empty=all] ")
        (cond
         ((eq 1   current-prefix-arg) "@: ")
         ((eq 2   current-prefix-arg) "0x@: ")
         ((eq nil current-prefix-arg) "0x: ")
         (t
          (error "No such prefix arg mode"))))
       kring))

     (if (not (ti::nil-p ans))
         (setq plist (list (tinypgp-pubring-alias2file ans)))
       (setq plist (tinypgp-pubring-list)))

     (list
      (nth 0 reg) (nth 1 reg)
      current-prefix-arg
      plist)))

  (tinypgpd "tinypgp-key-delete-region in: " beg end mode plist verb )
  (let* ((buffer-orig   (current-buffer))
         (BCMD          (tinypgp-binary-get-cmd 'key-delete)) ;base command
         (delete-count  0)
         buffer
         buffer-shell
         bcmd
         cmd
         list1 list2  email-list keyid-list
         elt elt2
         err err1 err2
         permission)

    (ti::verb)
    ;; #todo: use comint to delete keys ?
    ;;
;;;    (error "PGP can't use batch mode...needs new implementation.")

    (tinypgpd "tinypgp-key-delete-region in: BCMD " BCMD)
    (if (null plist)                    ;Set default value
        (setq plist tinypgp-:pubring-now))

    (setq plist (ti::list-make plist))  ;make sure it is list

    (if (and verb
             (not (y-or-n-p
                   "TinyPgp: are you sure about this (region right)? ")))
        (error "Aborted."))

    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ...  narrow ...
    ;;  We narrow so that highlighting finds right matches
    ;;
    (ti::narrow-safe beg end
      (tinypgp-run-in-tmp-buffer nil
                                 (cond  ;Gather key-ids first
                                  ((memq mode '(nil 2 any))
                                   (setq keyid-list
                                         (ti::mail-pgpk-id-0x-lines-in-region (point-min) (point-max))))
                                  ((memq mode '(1 email))
                                   (setq email-list (tinypgp-email-find-region (point-min) (point-max))))
                                  (t
                                   (error "Unknown mode %s" mode))))

      (and verb (tinypgp-highlight 'delete-all))

      (tinypgp-do-shell-env
       (tinypgp-save-state-macro
        (setq  buffer          (tinypgp-ti::temp-buffer)
               buffer-shell    (tinypgp-ti::temp-buffer 'shell))
        (with-current-buffer buffer
          (dolist (pring plist)

            (setq tinypgp-:pubring-now pring)
            (setq list1 email-list  list2 keyid-list)

            (tinypgpd "email-list"  list1  "keyid-list" list2)

            ;; ... ... ... ... ... ... ... ... ... ... ... . user-ask  ..
            (when (and verb
                       (not (string= "!" (or permission ""))))
              (setq permission
                    (read-from-minibuffer
                     (format
                      (concat
                       "Keyring %s "
                       "[ret=ok, !=all, s=skip]: ")
                      (file-name-nondirectory pring)))))

            ;; ... ... ... ... ... ... ... ... ... ...  user-response  ..
            (while (and
                    (member permission '("!" ""))
                    (null err)
                    (or list1 list2))
              (setq elt nil elt2 nil)
              (setq bcmd  (tinypgp-cmd-compose BCMD nil nil '(nil)))
              ;; ... ... ... ... ... ... ... ... ... ... ... .. email ..
              (when list1
                (setq elt (pop list1))
                (when elt
                  (setq cmd (format "%s '%s'" bcmd elt))
                  (shell-command cmd buffer-shell)
                  (tinypgpd "tinypgp-key-delete-region shell: " cmd )
                  (incf  delete-count)
                  ;;
                  ;;  "Key not found in keyring"
                  ;;  But that's no error and we don't report it.
                  ;;
                  (setq err1 (tinypgp-binary-check-error 'ignore-output cmd))
                  (when (and verb err1)  (tinypgp-error err1))))

              ;; ... ... ... ... ... ... ... ... ... ... ... ... . 0x ..
              (when list2
                (setq elt2 (pop list2))
                (when (stringp elt2)
                  (setq cmd (format "%s '0x%s'" bcmd elt2))
                  (shell-command cmd buffer-shell)
                  (incf  delete-count)
                  (tinypgpd "tinypgp-key-delete-region shell: " cmd )
                  (setq err1 (tinypgp-binary-check-error 'ignore-output cmd))
                  (when (and verb err2)  (tinypgp-error err2))))

              (tinypgpd "tinypgp-key-delete-region do: " pring
                        elt elt2 err1 err2 )

              ;;  Highlight the line so that user sees it was processed.
              ;;
              (if (and verb  (or elt elt2))
                  (with-current-buffer buffer-orig
                    (if elt  (tinypgp-highlight elt))
                    (if elt2 (tinypgp-highlight elt2 nil nil 'region))))))))

))    ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... message ...
    (when (and verb (null err))
      (cond
       ((zerop delete-count)
        (message "TinyPgp: Hm. It appears that no keys were found to delete."))
       (t
        (message "TinyPgp: Deleted keys have been marked with color. [%d]"
                 delete-count))))

    (when (and verb
               (setq err (tinypgp-binary-get-result-key-remove buffer-shell)))
      (pop-to-buffer buffer-shell)
      (error "TinyPgp: Key remove problem; remove manually "))

    err))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-sign-1 (your-keyid her-keyid &optional noerr)
  "Sign key-id to current pubring.

Input:
 YOUR-KEYID HER-KEYID NOERR"
  (tinypgpd "tinypgp-key-sign-1 in: " your-keyid her-keyid noerr )
  (let* ((BCMD          (tinypgp-binary-get-cmd 'key-sign)) ;base command
         (bcmd          (tinypgp-cmd-compose BCMD nil nil '(nil)))
         (buffer-shell  (tinypgp-ti::temp-buffer 'shell))
         stat
         cmd)
    (tinypgp-unfinished-function)
    (setq cmd (format "%s %s" bcmd your-keyid her-keyid))
    (error "#todo") (ti::d! (ti::string-right cmd 50))
    (shell-command cmd buffer-shell)

    ;; #todo: check results of signing
    ;;
    (if (setq stat (tinypgp-binary-get-result-key-sign))
        stat stat)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-sign-0x-forward (&optional verb)
  "Sign current 0x key forward. VERB.
The lines must contain valid key info entry extracted from pubring."
  (interactive)
  (let (keyid
        line)
    (error "#todo")
    (tinypgpd "tinypgp-key-sign-0x-forward in:")
    (ti::verb)

    (when (and (ti::mail-pgp-re-search 'kpub)
               (setq keyid (match-string 1)))
      (setq line (buffer-substring (match-end 0) (line-end-position)))

      (if (or (null verb)
              (and verb
                   (y-or-n-p
                    (format "Sign key %s , %s: " line keyid ))))
          (tinypgp-save-state-macro
           (tinypgp-user-in-use-confirm
            (tinypgp-key-sign-1 tinypgp-:user-now keyid)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-key-mode-set-trust (mode)
  "Set the trust MODE on current key/email in the line or point."
  (interactive "*r")
  (let* ()
    (cond
     ((eq mode 'undefined))
     ((eq mode 'untrusted))
     ((eq mode 'marginal))
     ((eq mode 'complete)))))

;;}}}
;;{{{ PGP main code

;;; ............................................................. &pgp ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-delete-processes (&optional verb)
  "Kill all PGP processes found from `process-list'. VERB."
  (interactive)
  (let* ((count 0))
    (ti::verb)
    (dolist (elt (process-list))
      (when (string-match "pgp" (prin1-to-string elt))
        (incf count)
        (delete-process elt)))
    (if verb
        (message "TinyPgp: %d processes deleted" count))

    ;; Return t if processes were deleted.
    (not (eq count 0))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-signature-user-info ()
  "Return User's X-Pgp information.

References:
  `tinypgp-:xpgp-user-info'

Return:
  nil
  string"
  (let ((ret (if (stringp  tinypgp-:xpgp-user-info)
                 tinypgp-:xpgp-user-info
               (eval tinypgp-:xpgp-user-info))))
    (if (ti::nil-p ret)
        nil
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-signature-move-to-header (&optional just-delete no-cnv)
  "Move Normal PGP signature to email headers.
If there is no PGP signature info, do nothing.
Works for read-only buffers too.

You can call this function only after you have composed the
message and attached the normal PGP signature.

Input:
  JUST-DELETE   delete Normal PGP signature: do not move.
  NO-CNV        Do not convert '- -' to '-' when deleting old signature."
  (tinypgpd "tinypgp-signature-move-to-header in:" just-delete)
  (let* (buffer-read-only
         (fid           "tinypgp-signature-move-to-header:")
         (hlist         tinypgp-:header-sign-smf-info)
         (psig          "X-Pgp-Signed")

         data
         hdr-smf
         user-info
         sig-fld

         sig-list
         info-list
         fld
         str)

    (when (and (null just-delete)
               (setq data               ;only if there is PGP
                     (save-excursion
                       (ti::pmin) (ti::mail-pgp-signature-normal-info))))
      (setq hdr-smf (if hlist
                        (concat
                         "SignedHeaders="
                         (mapconcat
                          'concat
                          (nth 1 tinypgp-:header-sign-smf-info)
                          ", ")
                         ";")))

      (tinypgpd fid "DATA" data
                "HDR-SMF" hdr-smf tinypgp-:header-sign-smf-info)
      ;;  moving signature in buffer is not really a modification
      ;;
      (with-buffer-modified
        (ti::save-with-marker-macro
          (setq info-list (nth 1 data)
                sig-list  (nth 2 data))

;;;     (setq I info-list S sig-list)
;;;     (ti::d! I B E)

          (tinypgpd  fid "INFO-LIST" info-list "SIG-LIST" sig-list)

          (unless just-delete
            (setq user-info (tinypgp-signature-user-info))

            (dolist (elt info-list)
              ;; For each PGP id, we just use that ID as
              ;; additional header name.
              ;;
              (when (string-match "\\(.*\\):[ \t]+\\(.*\\)" elt)
                (setq fld (match-string 1 elt)
                      str (match-string 2 elt))

                (if (string-match "Version\\|Charset" fld)
                    (setq sig-fld (format "%s%s=%s; "
                                          (or sig-fld "")
                                          fld str)))))

            (setq sig-fld (format "%sSignature=\n" sig-fld))

            (dolist (elt sig-list)
              ;; Last one Must have terminating colon
              ;;
              (if (null (cdr sig-list))
                  (setq sig-fld (format "%s  \"%s\";\n" sig-fld elt))
                (setq sig-fld (format "%s  \"%s\"\n" sig-fld elt))))

            (tinypgpd  fid "SIG-FLD" sig-fld)

            (setq str
                  (concat (if user-info (concat user-info "\n  " ) " ")
                          (if hdr-smf
                              ;; Fit in the same line?
                              ;;
                              (cond
                               ((< (+ (length hdr-smf) (length sig-fld))
                                   ;;  If there is no user info, then
                                   ;;  these fields go directly after
                                   ;;  X-Pgp-Signed:  (value 60)
                                   ;;
                                   (if user-info 77 60))
                                (concat hdr-smf sig-fld "\n  "))

                               ((< (length hdr-smf) 40)
                                (concat hdr-smf " \n  " sig-fld))

                               (t
                                (concat "\n  " hdr-smf "\n  " sig-fld)))
                            sig-fld)))
            (ti::mail-add-field psig str)))))

    ;;  We can do this without knowing if there is PGP sig,
    ;;  The previous statements already got rid of it
    ;;  Remove the traditional signature.
    ;;
    (ti::mail-pgp-signature-remove nil no-cnv)

    (tinypgpd fid "out: hooks" tinypgp-:sig-to-header-hook)

    (if tinypgp-:sig-to-header-hook
        (run-hook-with-args-until-success 'tinypgp-:sig-to-header-hook))))

;;; ----------------------------------------------------------------------
;;; The parameter 'delete' is optional, because
;;; - we may want to convert Headers to INFO block
;;; - do something when the block is there
;;;   remove that blocka.
;;;
;;; And we don't have no Moving back to headers.
;;;
(defun tinypgp-signature-from-header (&optional just-delete)
  "Convert X-Pgp signature to regular PGP signature.

Input:
  JUST-DELETE      do not move but delete header signature info."
  (let* ((fid           "tinypgp-signature-from-header:" )
         (pbase         "X-Pgp-")
         (sig-b-line    (ti::mail-pgp-re  (ti::mail-pgp-signature-begin-line)))
         (sig-e-line    (tinypgp-cnv (ti::mail-pgp-signed-end-line)))
         buffer-read-only

         data
         hdr-smf
         sig-list
         info-list
         beg
         end)

    (tinypgpd fid "in:" "DEL FLAG" just-delete )

    (cond
     (just-delete
      ;;  Old v2.xx x-pgp standard
      ;;
      (setq sig-list '("^X-Pgp-Charset" "^X-Pgp-Version"
                       "^X-Pgp-Signed"  "^X-Pgp-Comment"))
      (dolist (elt sig-list) (ti::mail-kill-field elt))
      (ti::mail-pgp-signature-remove))

     ((setq data (ti::mail-pgp-signature-header-info))
      (tinypgpd fid "X-pgp" data)
      (ti::save-with-marker-macro
        (ti::mail-pgp-signature-remove 'add)
        (setq beg       (car (nth 0 data)) ;headers are here
              end       (cdr (nth 0 data))
              info-list (nth 1 data)
              sig-list  (nth 2 data))

        (ti::pmin)
        (when (setq hdr-smf (tinypgp-header-sign-make-smf 'xpgp))
          (ti::mail-text-start 'move)
          (forward-line 2)
          (insert (car hdr-smf)))
        (re-search-forward sig-b-line)

        (forward-line 1)
        ;; There must be absolutely nothing after it.
        ;;
        (delete-region (point) (point-max))

        (unless just-delete
;;;       (insert sig-b-line "\n")
          (dolist (elt info-list)
            (setq elt (replace-regexp-in-string (concat "^" pbase) "" elt))
            (insert elt "\n"))

          (insert "\n")                 ;blank line
          (dolist (elt sig-list) (insert elt "\n"))

          (insert sig-e-line "\n"))

;;;     (ti::d! "DEL" beg end delete)

        (if (and beg end)
            (delete-region beg end)
          ;;  v3.xx has only one heder field
          (ti::mail-kill-field "^X-Pgp-signed"))

        (run-hooks 'tinypgp-:sig-from-header-hook))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-signature-move-to-header-maybe (&optional verb)
  "If current mode is mail or news then move signature to header.
But only if this is not a remailer message.

Input:
  VERB      Verbose messages.

References:
  `tinypgp-:xpgp-signing-mode'
  `tinypgp-:header-sign-table'       ,overrides all"
  (tinypgpd "tinypgp-signature-move-to-header-maybe in:")

  (let* ((fid   "tinypgp-signature-move-to-header-maybe:")
         elt
         (tinypgp-:xpgp-signing-mode    ;take local copy
          tinypgp-:xpgp-signing-mode)
         (allowed  (tinypgp-mail-buffer-p))
         (remail   (or (ti::mail-pgp-remail-p)
                       tinypgp-:r-mode-indication-flag))

         (do-it   tinypgp-:xpgp-signing-mode)
         (count   (tinypgp-hash 'sign 'get 'sign-remind-counter nil 'global)))
    (tinypgpd fid "ALLOWED" allowed "DO" do-it "REMAIL" remail)

    (when (and
           allowed do-it
           (null remail)
           (if (setq elt (tinypgp-header-sign-active-list))
               (null (nth 2 elt))   ;if this entry is NIL then proceed
             t))

      (tinypgp-signature-move-to-header nil 'no-cnv)

      (when verb
        (unless (integerp count)
          (setq count 0)
          (tinypgp-hash 'sign 'put 'sign-remind-counter 0 'global))

        (incf  count)
        ;;
        ;;  Display message every 5th time
        ;;
        (when (eq 0 (% count 5))
          (setq count 0)
          (message
           (concat
            "Do not modify buffer, otherwise "
            "PGP signature must be generated again.")))
        (tinypgp-hash 'sign 'put 'sign-remind-counter count 'global)))
    (tinypgpd fid "out:")))

;;}}}
;;{{{ secring management

;;; ......................................................... &secring ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-file ()
  "Return current backends secring."
  (or (cdr (assq (tinypgp-backend-now) tinypgp-:file-secring ))
      (error "No secring in tinypgp-:file-secring")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-crypt-command-get (from to password)
  "Return 'crypt' command for files FROM TO using PASSWORD."
  ;;  Store to property so that we don't have to ask it again
  ;;
  (let* ((fid   "tinypgp-crypt-command")
         (sym   'tinypgp-:pgp-binary)
         crypt)
    (unless (setq crypt (get sym 'crypt))
      (setq crypt (or (executable-find "crypt")
                      (error "Can't find 'crypt' on `exec-path'.")))
      (put sym 'crypt crypt))
    (unless (and (stringp from) (stringp to) (stringp password))
      (error "Invalid crypt command parameters."))

    (tinypgpd fid crypt from to)
    (format "%s %s < %s > %s" crypt password from to)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-crypt-do-with-pgp (from to password &optional comment)
  "Use PGP to conventionally crypt file.

Input:

  FROM          source file
  TO            destination file
  PASSWORD      crypt password
  COMMENT       the +comment string. Default is
                'FILE is conventionally encrypted.'
                Set to \"\" if you don't want comment.

Note:

  If the FROM file is pgp armored, it will be assumed that it is already
  encrypted conventionally and that it should be restored. If the file
  has no ascii armor, then it will be crypted.

  So, depending on input file, the file is either locked or unlocked.
  You don't get double conventional encryption if you specify FROM
  as already crypted file."
  (let* ((fid       "tinypgp-crypt-do-with-pgp")
         (buffer    (tinypgp-ti::temp-buffer 'shell))
         (opt       tinypgp-:pgp-command-options)
         (pgp-exe   (tinypgp-binary1 'crypt))

         ;;  in case there is error these hooks are called to
         ;;  restore buffer. But because this function deals with
         ;;  files; no emacs buffer is involved. Prevent
         ;;  calling these functions.
         ;;
         tinypgp-:cmd-macro-after-hook
         encrypted-p
         cmd
         err)

    (tinypgpd fid "in:" from to "comment:" comment)

    (if (not (file-exists-p from))
        (error "no FROM file"))

    (if (file-exists-p to)
        (delete-file to))

    (or (stringp comment)
        (setq comment
              (format "Conventionally crypted %s" from)))

    ;;  We have to know if the file is already crypted to select right
    ;;  command. We only read part of the file to determine if it has ascii
    ;;  armor

    (with-current-buffer buffer
      (insert-file-contents from nil 0 300)
      (setq encrypted-p (ti::mail-pgp-re-search 'msg))
      (erase-buffer))

    (tinypgpd fid "ENCRYPTED stat" encrypted-p)

    ;; cat T     | pgp +comment="Crypted secring.pgp" -caf -z foo > T.asc
    ;; cat T.asc | pgp -f -z foo > T

    (cond
     (encrypted-p
      (setq cmd (format
                 "%s %s | %s -f -z %s %s +batch > %s "
                 (if (ti::win32-p) "type " "cat ")
                 from
                 pgp-exe
                 password
                 opt
                 to))
      (tinypgpd fid "CRYPT --> regular ."))
     (t
      (setq cmd (format
                 "%s %s | %s -caf -z %s %s +batch %s > %s"
                 (if (ti::win32-p) "type " "cat ")
                 from

                 pgp-exe
                 password
                 opt
                 (if (not (ti::nil-p comment))
                     (format "+comment=\"%s\"" comment)
                   "")
                 to))
      (tinypgpd fid "REGULAR --> crypt" cmd)))

    (setq tinypgp-:last-pgp-exe-command cmd)
    (shell-command cmd buffer)
    (when (setq err (tinypgp-binary-check-error 'ignore-output))
      (tinypgp-error err)
      err)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-crypt-do-with-crypt (from to password)
  "Crypt FROM source TO destination using PASSWORD using 'crypt'."
  ;;
  ;;  Maybe I add something here later.
  ;;  It's too bad that we can't check if the (de)crypting was done
  ;;  with right password. The 'crypt' command won't tell success or
  ;;  failure so be _sure_ you type it right in the prompt.
  ;;
  (tinypgpd "tinypgp-crypt-do-with-crypt" from to)
  (shell-command (tinypgp-crypt-command-get from to password)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-crypt-do (from to password)
  "Crypt FROM source TO destination using PASSWORD."
  (funcall tinypgp-:secring-crypt-function from to password))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-ask-secring-password (&optional force)
  "Ask secring password. Return old or FORCE asking again."
  (let* ((sym   'tinypgp-:hash)
         (ret   (get sym 'secring-passwd)))
    (when (or force (null ret))
      (setq ret (ti::compat-read-password "Password for secring: "))
      (put sym 'secring-passwd ret))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring (&optional set)
  "Return secring from memory or set secring from current buffer.
SET can be
  'kill    Empty secring from memory
  non-nil  read buffer content into memory as secring
  nil      return secring from memory."

  (cond
   ((null set)
    (get 'tinypgp-:hash 'secring))
   ((eq set 'kill)
    (put 'tinypgp-:hash 'secring nil))
   (t
    (put 'tinypgp-:hash 'secring (buffer-string)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-use ()
  "Make sure we have secring available."
  (let* ((fid     "tinypgp-secring-use")
         (secring (tinypgp-secring-file))
         (enc     tinypgp-:file-secring-encrypted)
         pass)
    (tinypgpd fid)
    (when (not (file-exists-p secring)) ;Ahem, it's encrypted...

      (if (not (file-exists-p enc)) ;Nope, something is very wrong here
          (error "Panic, no secring! Pull out your backup..."))

      (cond
       ((and (file-exists-p enc)        ;In memory
             (tinypgp-secring))
        (tinypgpd fid "Write")
        (tinypgp-secring-crypt-read nil 'write))
       ((and (file-exists-p enc)
             (null (tinypgp-secring)))  ;Not in Memory
        (tinypgpd fid "read & rrite")
        (setq pass (tinypgp-ask-secring-password))
        (tinypgp-secring-crypt-read pass)
        (tinypgp-secring-crypt-read nil 'write))))))

;;; ----------------------------------------------------------------------
;;; Why I dind't use PGP? because I can't control to what file it
;;; produces the output. It always writes to .pgp or .asc (-a) and
;;; that not very friendly.
;;;
(defun tinypgp-secring-crypt (password &optional decrypt)
  "Conventionally encrypt secrig with PASSWORD secring.
This function doesn't use PGP, but calls external 'crypt' command.
If DECRYPT is non-nil, move encrypted secring back.

Caution: Make backup first. This fuction deletes or modifies the
secring.pgp !!

References:
  `tinypgp-:file-secring-encrypted'"
  (interactive
   (list
    (ti::compat-read-password
     (format "[%s] Secring password: "
             (if current-prefix-arg "decrypt" "encrypt")))
    current-prefix-arg))
  (let* ((fid    "tinypgp-secring-crypt")
         (from   (if decrypt
                     tinypgp-:file-secring-encrypted
                   (tinypgp-secring-file)))
         (to    (if decrypt
                    (tinypgp-secring-file)
                  tinypgp-:file-secring-encrypted)))
    (tinypgpd fid "in:" decrypt from to)
    (if (not (file-exists-p from))
        (error "Fatal condition, no file: %s" from))

    ;;  If this fails; then we can't execute crypt command that
    ;;  overwrites file.
    ;;
    (if (file-exists-p to)
        (delete-file to))

    (tinypgp-crypt-do from to password)

    (if (interactive-p)
        (message "Secring %s"
                 (if decrypt
                     "decrypted"
                   "encrypted")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-crypt-read (&optional password write force)
  "Read encrypted secring, open it, and put to to memory.
If file already exists in memory, do nothing.

Input:

  PASSWORD  password string
  WRITE     write secring from memory to (tinypgp-secring-file)
  FORCE     If non-nil force reading encrypted secring to memory"
  (let* ((fid       "tinypgp-secring-crypt-read")
         (secring   (tinypgp-secring-file))
         (from      tinypgp-:file-secring-encrypted)
         (to        tinypgp-:file-source))
    (tinypgpd fid (if write "WRITE" "READ") force)
    (cond
     (write
      (if (null (setq from (tinypgp-secring)))
          (error "Read secring first to memory."))
      (with-temp-buffer
        (erase-buffer)
        (insert from)
        (tinypgp-secring 'read-to-memory)
        (write-region (point-min) (point-max) secring)))
     (t
      ;; ........................................................ read ...
      (when (or (not (tinypgp-secring))
                (null force))
        (unless (file-exists-p from)
          (error "There is no encrypted secring."))

        (if (file-exists-p to)
            (delete-file to))

        (tinypgp-crypt-do from to password)

        (unwind-protect
            (progn
              (with-temp-buffer
                (erase-buffer)
                (insert-file-contents to)
                (if (ti::buffer-empty-p)
                    (error "No results after opening encrypted secring?"))
                (tinypgp-secring 'read-to-memory)))
          (tinypgp-file-control 'source-kill)))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-crypt-maybe ()
  "Make encrypted secring if it doesn't exist already."
  (unless (file-exists-p (tinypgp-secring-file))
    (call-interactively 'tinypgp-secring-crypt)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-kill-maybe ()
  "Kill secring.pgp if there is encrypted keyring.
`tinypgp-:secring-crypt-mode' must be non-nil too."
  (when (and tinypgp-:secring-crypt-mode
             (file-exists-p (tinypgp-secring-file)))
    (delete-file (tinypgp-secring-file))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-restore-maybe ()
  "Restore (tinypgp-secring-file) if there is encrypted keyring.
If there already is (tinypgp-secring-file) then do nothing."
  (unless (file-exists-p (tinypgp-secring-file))
    ;;  - Be sure that there exists encrypted secring in the disk
    ;;  - We may have the secring in the memory, but nevertheless
    ;;    I must require that is also in disk.
    ;;
    (when (or (file-exists-p tinypgp-:file-secring-encrypted)
              (error "Can't find encrypted scring?"))
      (tinypgp-secring-crypt (tinypgp-ask-secring-password) 'restore))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-backup (file password &optional verb)
  "Backup (tinypgp-secring-file) in crypted format to FILE with PASSWORD.
Previous FILE is deleted. VERB."
  (interactive
   (progn
     (let* ((default-directory (concat (tinypgp-path ".") "/")))
       (list
        (read-file-name "Backup secring to: ")
        (ti::compat-read-password "Backup password: ")))))
  (let* ((from  (tinypgp-secring-file)))
    (ti::verb)
    (unless (file-exists-p from)
      (error "There is no secring to be backed up."))
    (if (file-exists-p file) (delete-file file))

    (prog1
        (tinypgp-crypt-do from file password)
      (if (not (file-exists-p file))
          (error "Couldn't make backup."))
      (if verb
          (message "TinyPgp: secring backup done.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-crypt-mode-detect ()
  "Set correct `tinypgp-:secring-crypt-mode'."
  (if (and tinypgp-:secring-crypt-mode
           (not (file-exists-p tinypgp-:file-secring-encrypted)))
      (setq tinypgp-:secring-crypt-mode nil))

  ;; If mode is off; then this condition must be true
  ;; - there must be secring.pgp
  ;; - there must not be secring.enc

  (if (null tinypgp-:secring-crypt-mode)
      (cond
       ((file-exists-p tinypgp-:file-secring-encrypted)
        (setq tinypgp-:secring-crypt-mode t))
       ((or (not (file-exists-p (tinypgp-secring-file) )) ;; .pgp missing
            (file-exists-p tinypgp-:file-secring-encrypted)) ;; .enc found
        (error "Fatal, no secring.pgp or secring.enc found."))))
  tinypgp-:secring-crypt-mode)

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-crypt-expire-password (&optional verb)
  "Reset the secring password so that you can change it. VERB.
The password is set once when you turn on crypt mode
with `tinypgp-secring-crypt-mode-toggle' and it never chages during the
lifetime of program.

However if you want to change the password; you must
o  turn off the crypt mode
o  call this function
o  turn on the crypt mode"
  (ti::verb)
;;;  (or (tinypgp-secring-crypt-mode-detect)
;;;      (error
;;;       (substitute-command-keys
;;;        "\
;;;Can't expire secring password: Use \\[tinypgp-secring-crypt-mode-toggle]")))
;;;

  (let* ((pass (get 'tinypgp-:hash 'secring-passwd)))
    (if (stringp pass) (fillarray pass ?\0))
    (put 'tinypgp-:hash 'secring-passwd nil)
    (when verb
      (message "TinyPgp: Secring Password expired."))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-secring-crypt-mode-toggle (arg &optional verb)
  "Toggle using crypted secring.

Input:

   ARG      Mode arg. nil = toggle, 0 = off, 1 = on.
   VERB     If non-nil, print verbose messages.

Caution

   Before turning on this mode make backup of your keyring removable media.
   Prefer ancrypting that backup too, otherwise you have defeated the
   purpose of this mode by letting people to access your secring
   in some other readable file. See command \\[tinypgp-secring-backup]

   See also \\[tinypgp-secring-crypt-expire-password]

Description

    When this mode is enabled. You secring is immediately moved to
    conventionally encrypted format if it already isn't crypted.
    The ecrypted secring is located at `tinypgp-:file-secring-encrypted'
    and (tinypgp-secring-file), `Secring' , is deleted.

    When programs needs the secring it temporarily opens the encrypted
    secring and write to `Secring'. When the PGP operation
    that needed secring is over, the `Secring' is removed.

    If you are in multi-user environment, be aware that all your files
    are propable taped due to regular backups in the file system.
    Thus your PGP keys are available to the sysadm.

    And even if your're in single user environment, somebody may sit
    down to your computer console and copy the secring.pgp withing
    seconds.

    If you're paranoid at all, you keep this mode permanently on by
    setting `tinypgp-:secring-crypt-mode' to t.

Files

    When you turn on the mode the `Secring' is deleted and
    encrypted `tinypgp-:file-secring-encrypted'. When you turn off this
    mode reverse happens and `Secring' is restored.

Note

    Turning on or off this mode causes a slight delay because
    the command to encyprt or decypt the password is called.

    While the mode is active, you cannot use all pgp commands
    from the shell command prompt because there is no secring.pgp
    directly available. Eg. if you want to generate new key, which
    modifies secring; you should turn off this mode to temprarily
    reveal secring.pgp.

Return:
   value of `tinypgp-:secring-crypt-mode'"
  (interactive "P")
  (let* ((fid  "tinypgp-secring-crypt-mode-toggle")
         old-mode)

    (ti::verb)
    (tinypgpd fid "in:" arg)
    (setq old-mode (tinypgp-secring-crypt-mode-detect))
    (ti::bool-toggle tinypgp-:secring-crypt-mode arg)
    (tinypgpd fid "ARG" arg "MODE" old-mode tinypgp-:secring-crypt-mode verb)

    ;;  If MODE was ON; and we were called with parameter 1,
    ;;  then do nothing; because mode hasn't changed.

    (when (not (eq old-mode tinypgp-:secring-crypt-mode))
      ;;   When mode is turned off
      ;;   o   Remove secring from memory, because user may now change it
      ;;       on disk.
      ;;   When mode is on
      ;;   o   Read it from disk to memory. Secring is nor in encrypted
      ;;       format on disk.

      (cond
       (tinypgp-:secring-crypt-mode
        ;;  Display messages so that user doesn't get nervous. This
        ;;  may take 1-3 seconds.
        ;;
        (when verb (message "Secring conversion in progress...2"))
        (tinypgp-secring-crypt (tinypgp-ask-secring-password))
        (tinypgp-secring-kill-maybe)
        (when verb (message "Secring conversion in progress...1"))
        (tinypgp-secring-crypt-read (tinypgp-ask-secring-password)))
       (t
        (when verb (message "Secring conversion in progress..."))
        (tinypgp-secring-restore-maybe)
        ;;  We must delete this
        ;;  o  In many places program checks if this exist; but because
        ;;     mode is off it should not be used. Safest is to destroy it.
        ;;  o  If user starts adding new secret keys; he turns this mode off
        ;;     --> he should see just regular secring.pgp and not get confused
        ;;     by secring.enc
        ;;
        (delete-file tinypgp-:file-secring-encrypted)
        (tinypgp-secring 'kill))))

    (tinypgp-secring-crypt-mode-detect)

    (when verb
      (message
       (concat "TinyPgp: SECRING encrypt mode: "
               (if tinypgp-:secring-crypt-mode
                   "on" "off")
               (if (null tinypgp-:secring-crypt-mode)
                   (concat ". "
                           (file-name-nondirectory
                            (tinypgp-secring-file))
                           " restored")))))

    (tinypgp-update-modeline)
    tinypgp-:secring-crypt-mode))

;;}}}
;;{{{ interactive, guess next action

;;; .................................................... &guess-action ...

;;; ----------------------------------------------------------------------
;;; - If you have used vc.el, then you know why this function ....
;;;
(defun tinypgp-next-action-mail (&optional arg)
  "Try to guess next action. ARG is passed to called function.
If buffer has auto action active or if function cannot guess what
to do, this command does nothing.

In mail buffer,

o  If buffer is read-only, try to decrypt it. We suppose that the
   buffer is used by some mail reader.
o  -- Check if there is only one email in TO field. If the
      user is cached (you have previously encrypted message to him),
      then ask permission to encrypt the message.
   -- Sign the message

In some other buffer:

o  If not signed, sign.
o  If signed, verify and if that reveals inner folders, open them all.
o  If encrypted, decrypt. (Mail buffers are ignored, because you can't
   decrypt other users encrypted message.)

Prefix argument:

o  Is passed to decrypt command"
  (interactive "P")
  (tinypgpd "tinypgp-next-action-mail in:" arg)
  (let ((auto-action-pending (and (not buffer-read-only)
                                  (tinypgp-auto-action-p)
                                  (tinypgp-hash 'auto-action 'get 'user-mode)))
        (fid        "tinypgp-next-action-mail: ")
        pring
        to
        type)

    (tinypgpd fid "auto action:" auto-action-pending )

    (cond
     (auto-action-pending
      (message "TinyPgp.el ...note, auto action is pending."))
     (t
      (cond
       ;; ... ... ... ... ... ... ... ... ... ... ... ... ...  regular ..
       ((not (ti::mail-mail-p))         ;not a mail buffer
        (tinypgpd fid "non-mail buffer start:")

        (cond
         ((not (ti::mail-pgp-signed-p))
          (tinypgpd fid "regular not pgp-signed:")
          (tinypgp-sign-region (point-min) (point-max)))

         ((ti::mail-pgp-signed-p)

          (tinypgpd fid "regular signed:")
          (while (or (if (ti::mail-pgp-signed-p) (setq type 'sign))
                     (if (ti::mail-pgp-p)        (setq type 'other)))
            (tinypgpd fid "regular; envelope" type)

            (cond
             ((eq type 'sign)
              (tinypgp-verify-region (point-min) (point-max)))
             ((eq type 'other)

              ;;  When we verify message...
              ;;  a)  an encrypted message envelope surfaces
              ;;  b)  it was base64 signed -> regular text
              ;;  c)  signed

              (tinypgp-decrypt-region
               (point-min) (point-max)
               (car (tinypgp-i-args-decrypt)))))))))

       ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... mail ..

       ((ti::mail-mail-p)
        (setq to (mail-fetch-field "to"))

        (tinypgpd fid "mail: to" to
                  "see pgp? " (ti::mail-pgp-p)
                  "multi ,? " (count-char-in-string ?, (or to "")))

        (cond
         ((and (not buffer-read-only)
               (not (ti::nil-p to))           ;; Must have TO email
               (null (ti::mail-pgp-p))        ;; No PGP yet, okay...
               (eq (count-char-in-string ?, to) 0) ;; only one email?
               (setq to (car-safe (ti::mail-email-from-string to)))
               ;;  Have we sent encrypted mail to him?
               ;;
               (setq pring (tinypgp-key-find-by-cache to)))
          (tinypgpd fid "mail: 1 encrypt")
          (tinypgp-save-state-macro
           (setq tinypgp-:pubring-now pring)
           (call-interactively 'tinypgp-encrypt-mail)))

         ((and (ti::mail-pgp-signed-p)
               (ti::mail-pgp-encrypted-p 'double-check))
          (tinypgpd fid "mail: verify/decrypt")
          (call-interactively 'tinypgp-verify-mail)
          (sit-for 1.7)              ;let user see "Good signature..."
          (tinypgp-decrypt-mail-verbose (quote arg)))

         ((and (null buffer-read-only)
               (not (ti::mail-pgp-headers-p))
               (not (ti::mail-pgp-signed-p)))
          (tinypgpd fid "mail: sign")
          (call-interactively 'tinypgp-sign-mail))

         ((and (ti::mail-pgp-signed-p)
               (not (ti::mail-pgp-encrypted-p 'message-tag-too)))
          (tinypgpd fid "mail: 3")
          (call-interactively 'tinypgp-verify-mail))) ;Cond end

        ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..
        ;;
        ;;  Still pgp ? Maybe we should decrypt it ?
        ;;  But what if this is mail that is meant to be sent to
        ;;  someone else --> we assume that non-read-only buffer
        ;;  is mail to someone else
        ;;
        ;;  This is looped two times because:
        ;;  - Nym account sends use [ENCRYPT [conventional CRYPT]] envelope
        ;;  - Loop1 open CRYPT envelope and the second loop checks
        ;;    is there was still real encrypted message (by your nym key)
        ;;
        ;;  but I don't dare...2 envelopes should suffice.

        (while (and buffer-read-only
                    (not (ti::mail-pgp-signed-p))
                    (ti::mail-pgp-p))
          (tinypgpd fid "mail: still pgp")
          (tinypgp-decrypt-mail-verbose (quote arg)))

        (goto-char (ti::mail-text-start))))))))
      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... mail end ..

;;}}}
;;{{{ signing

;;; ......................................................... &signing ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-modify-check ()
  "Detect if signed message is modified.
References:
  `tinypgp-:sign-data'"
  (let* ((mail  (ti::mail-mail-p))
         (mime  (ti::mail-mime-maybe-p)))
    (when (and mail
               (not mime)
               (ti::mail-pgp-headers-p)
               (tinypgp-sign-data-same-p))
      (message "TinyPgp: Body changed, signature invalid; resigning...")
      (sit-for 0.7)
      (tinypgp-sign-loose-info)
      (call-interactively 'tinypgp-sign-mail))

    (tinypgpd "sign-modify-check:" (current-buffer) "MAIL" mail "MIME"
              (ti::mail-message-length)
              tinypgp-:sign-data)
    ;;  hook return value
    nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-mail-auto-p ()
  "Check if auto signing would happen."
  (and tinypgp-mode
       (not (ti::mail-pgp-signed-p))
       (not (ti::mail-mime-maybe-p))
       (null tinypgp-:r-mode-indication-flag)
       (or (null tinypgp-:sign-mail-p-function)
           (funcall tinypgp-:sign-mail-p-function))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-mail-func ()
  "Maybe sign current buffer. This function is called from hooks.
If buffer is already signed or remailer action is in progress,
do nothing.

References:
  `tinypgp-:sign-mail-p-function'"
  (if (inline (tinypgp-sign-mail-auto-p))
      (call-interactively 'tinypgp-sign-mail)))

;;; ----------------------------------------------------------------------
;;; on/off function can be used in hooks
;;;
(defun tinypgp-sign-mail-auto-mode-on ()
  "Turn on automatic signing."
  (tinypgp-sign-mail-auto-mode 1))

(defun tinypgp-sign-mail-auto-mode-off ()
  "Turn off automatic signing."
  (tinypgp-sign-mail-auto-mode 0))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-sign-mail-auto-mode (&optional arg)
  "Toggle autosigning mode according to ARG.

Input:
  0, -1     off
  nil       toggle
  t, 1      on

  'push-on  Record previous value and turn on  auto signing.
  'push-off Record previous value and turn off auto signing.
  'pop      pop previous autosign value.

Return:
  nil       autosigning off
  non-nil   autosigning on"
  (interactive)
  (let* ((fid           "tinypgp-sign-mail-auto-mode")
         (stack         (get 'tinypgp-sign-mail-auto-mode 'stack))
         (func          'tinypgp-sign-mail-func)
         (hooks         tinypgp-:mail-send-hook-list)
         (now-on-p      (tinypgp-sign-mail-auto-mode-on-p))
         remove)

    (tinypgpd fid arg)

    ;; ......................................................... stack ...
    (when (and (not (null arg))
               (symbolp arg))
      (cond
       ((eq arg 'push-on)
        (push now-on-p stack)
        (setq arg 1))

       ((eq arg 'push-off)
        (push now-on-p stack)
        (setq arg 0))

       ((eq arg 'pop)
        (if (not (ti::listp stack))
            (error "Nothing to pop from stack.")
          (setq arg (car stack))
          (setq stack (cdr stack))))
       (t
        (error "Not known arg")))
      (put 'tinypgp-sign-mail-auto-mode 'stack stack))

    (tinypgpd fid arg "STACK" stack)

    ;; ...................................................... mode arg ...
    (cond
     ((null arg)
      (if now-on-p
          (setq remove t)))

     ((memq arg '(0 -1))
      (setq remove t)))

    (ti::add-hooks hooks func remove)
    (tinypgp-update-modeline)

    (if (interactive-p)
        (message
         (format
          "TinyPgp: mail auto signing mode %s"
          (if remove "off" "on"))))

    remove))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-sign-loose-info (&optional verb)
  "Loose signature info.
Input:
  VERB          Verbose messages."
  (interactive)
  (let* (tinypgp-:sig-to-header-hook    ;must be disabled for now
         (allow   (tinypgp-mail-buffer-p)))
    (ti::verb)
    (when (and allow
               (ti::mail-pgp-headers-p))
      (ti::mail-kill-field "X-Pgp-Signed"))
    (ti::save-with-marker-macro
      (tinypgp-signature-from-header 'just-delete))

    (when (and verb (null allow))
      (message "PGP action maybe partially completed...") (sit-for 2))

    (run-hooks 'tinypgp-:sign-loose-info-hook)
    (if verb
        (message "PGP signing information deleted."))
    t))

;;; ----------------------------------------------------------------------
;;; - parameters BEG and END _must_ be nil
;;;
;;;###autoload
(defun tinypgp-sign-mail (&optional register user options verb noxpgp)
  "Sign message in mail buffer.

Input:

  REGISTER      flag, if non-bil store the signature to register.
                This is the prefix arg user passes to program.
                This will automatically turn off X-pgp.
  USER          key-id
  VERB          allow verbose messages
  NOXPGP        Prohibit X-Pgp

Notes:

  if VERB is non-nil (set in interactive call), the pubring is
  changed if it the information is on the cache."
  (interactive
   (progn
     (tinypgp-hash 'action 'put 'now    'sign 'global)
     (tinypgp-hash 'action 'put 'detail 'mail 'global)

     (tinypgpd "tinypgp-sign-mail: interactive")

     (tinypgp-user-change-macro
      (list
       current-prefix-arg
       tinypgp-:user-now
       (eval tinypgp-:pgp-binary-interactive-option)
       t
       current-prefix-arg))))

  (barf-if-buffer-read-only)
  (tinypgp-i-args-pass-phrase)

  (let* ((fid               "tinypgp-sign-mail: ")
         (tinypgp-:pubring-now tinypgp-:pubring-now)
         (signed-p           (ti::mail-pgp-signed-conventional-p))
         (signed-xpgp-p      (ti::mail-pgp-signed-xpgp-p))
         (signed-multi-p     (ti::mail-pgp-signed-conventional-multi-p))
         (mail-p             (ti::mail-mail-p))
         (write-point        (point))
         beg
         write-mark
         write-line
         write-col
         sign-user
         buffer
         hdr-smf
         pring
         end)

    (ti::verb)
    (tinypgp-hash 'action 'put 'now 'sign 'global)
    (tinypgp-hash 'action 'put 'detail 'mail 'global)

    (tinypgpd fid "signed" signed-p signed-xpgp-p signed-multi-p
              "mail-p" mail-p
              "User:" user verb
              (current-buffer)
              (buffer-name))

    ;;  Note: It sis very unfortunate that the signature separator
    ;;  is "-- ". Thazt extra space will be gone below, because we trim
    ;;  the message before signing it.
    ;;
    ;;  To my opinion it's more important to trim the message than
    ;;  preserve trailing spaces at the end of lines.

    (tinypgp-add-signature-if-signing)
    (ti::mail-trim-buffer)

    (ti::mail-pgp-header-kill-in-body)

    ;;  Actually; someone else could have signed using X-pgp,
    ;;  and when we sign the message, the Right Thing would be
    ;;  - check if X-pgp is ours --> remove it. If not, then convert
    ;;    it to regular pgp signature.
    ;;  - add out signing (if there is regular signature, then
    ;;    do ot use X-pgp)

    (when signed-xpgp-p (tinypgp-sign-loose-info))

    (save-excursion
      (goto-char (if mail-p
                     (ti::mail-text-start)
                   (point-min)))
      (setq beg (point)))

    (if (or (eq beg (point-max))
            (and mail-p                 ;Check only mail buffer
                 (save-excursion
                   (goto-char (or beg (point-min)))
                   ;; there must be text, not just emptly lines
                   ;;
                   (null (re-search-forward "[^ \t\n]" nil t)))))
        (error "Nothing to do, no text found."))

    (tinypgp-save-state-macro
     ;; Turn this off if buffer is not mail or if there already is signature
     (when (or (not mail-p)
               signed-p)
       (setq tinypgp-:xpgp-signing-mode nil))

     (when verb
       (if (null (setq pring (tinypgp-key-find-by-cache tinypgp-:user-now)))
           (tinypgp-pubring-in-use-confirm)
         (setq tinypgp-:pubring-now pring)))

     (tinypgpd fid "PRING NOW" tinypgp-:pubring-now pring)

     ;; ... ... ... ... ... ... ... ... ... ... ... ...  tmp buffer ...

     (tinypgp-mode-specific-control-before 'sign tinypgp-:user-now)

     (tinypgp-run-in-tmp-buffer nil
                                (tinypgp-user-change-macro
                                 (tinypgp-set-session-parameters 'sign)

                                 (goto-char write-point)
                                 (setq buffer     (current-buffer)
                                       sign-user  tinypgp-:user-now ;Save this value
                                       write-mark (point-marker)
                                       write-line (ti::read-current-line)
                                       write-col  (current-column))
                                 ;; ............................................ do signing ...
;;;       (ti::d! "::sign do" tinypgp-:pubring-now)
                                 (tinypgp-cmd-macro-email "sign"
                                                          (cond
                                                           ((and (ti::mail-mail-p)
                                                                 (setq hdr-smf (tinypgp-header-sign-make-smf)))
                                                            (goto-char beg)
                                                            (insert (car hdr-smf))))

                                                          (tinypgp-cmd-macro 'sign user nil
                                                                             "Signing..." register options 'no-mode-funcs)

                                                          ;;        (pop-to-buffer (current-buffer))  (ti::d! "::sign done")

                                                          (tinypgp-signature-move-to-header-maybe verb)
                                                          (setq write-point (marker-position write-mark)
                                                                ;; kill marker
                                                                write-mark  nil))))
     ;; ........................................... signing end ...

;;;      (tinypgp-mode-specific-control-after 'sign tinypgp-:user-now nil nil nil)

     ;;  - Copy signed data to original buffer.
     ;;  - restore original write position: this is tricky because the
     ;;    buffer has changed: Search line string and goto column OR
     ;;    got to marker position.
     ;;
     (unless register
       (erase-buffer)
       (insert-buffer buffer)
       (ti::pmin)
       (if (and (not (ti::nil-p write-line)) ; can't search empty line
                (search-forward write-line nil 'noerr))
           (move-to-column write-col)
         ;;  This doesn't necessarily succeed to preserve position,
         ;;  but it's better than nothing.
         ;;
         (goto-char write-point)
         ;; If this changes, signing is not valid
         (tinypgp-sign-data-set)))

     ;; ............................................. verbose message ...
     (when verb
       (message "%sSigned with key: %s"
                (if register
                    (format "[Result in register %c] " tinypgp-:register)
                  "")
                sign-user)
       (sit-for 1))

     t)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-mail-base64 (&optional register )
  "Uuencode message with pgp.
Store output to `tinypgp-:register' if REGISTER is non-nil.
This function turns off clearsig, so that mail is signed, compressed,
and uuencoded in base64."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list
      current-prefix-arg)))
  (tinypgp-i-args-pass-phrase "[Base64] Sign pass phrase: ")
  (tinypgp-hash 'action 'put 'now 'sign 'global)
  (let* ((beg  (or (ti::mail-text-start) (point-min)))
         (end  (point-max)))
    (if (eq beg end)
        (error "TinyPgp: sign mase64, There is no text in message body."))
    (tinypgp-sign-region-base64 beg end register nil (interactive-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-base64-insert-file (file &optional options)
  "Insert uuencoded FILE into point using OPTIONS.
This function inserts the given file into point and turns
off clearsig, so that the file is signed, compressed, and uuencoded
in base64.

It is encouraged that insert big files with this function
to the buffer when you're going to send them via email."
  (interactive "*f[base64 sign] Insert file: ")
  (let ((buffer (tinypgp-ti::temp-buffer 'finger)) ;This is free for us.
        comment
        size)
    (barf-if-buffer-read-only)
    (tinypgp-hash 'action 'put 'now 'sign 'global)
    ;;  Hm. This should be interactive part...
    ;;
    (tinypgp-i-args-pass-phrase)

    ;; Insert file leaves point before the file, but we need to know
    ;; where it ends...
    ;;
    (with-current-buffer buffer
      (insert-file-contents file)

      (unless options
        (setq size (/ (buffer-size) 1000)) ;in kilos
        (setq
         comment
         (format
          "Base64 signed. File: %s uncompresses to approx. %s"
          (file-name-nondirectory file)

          (if (eq 0 size)               ;Hm. very small file
              (format "%dbytex" (buffer-size))
            (format "%dK" size)))))

      ;;  In Unix we pass the option directly to pgp.
      ;;  This way UNDO can undo whole PGP response at once
      ;;
      (when (and comment (not (ti::win32-p)))
        (setq options (format "+comment=\"%s\"" comment)))

      (tinypgp-sign-region-base64 (point-min) (point-max) nil options)

      ;;  But in Windows we have to manually patch the genrated output.
      ;;  You have to ress twice UNDO to get original text

      (when (and comment
                 (or (not (tinypgp-backend-pgp2-p))
                     (ti::win32-p)))
        (tinypgp-binary-header-field-set "Comment:" comment))

      (ti::pmin)
      (run-hook-with-args-until-success
       'tinypgp-:insert-file-sign-base64-hook
       file))
    (insert-buffer buffer)
    ;; It may be big file, don't leave into emacs
    (ti::erase-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-region-base64
  (beg end &optional register options verb )
  "Sign as base64 (uuencode).

Input:

 BEG END
 REGISTER       if non-nil; then store contents to `tinypgp-:register'
 OPTIONS        option string passed to pgp.
 VERB           Verbose messages.

This function turns off clearsig, so that region is signed,
compressed, and uuencoded in base64."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (list
      (region-beginning) (region-end)
      current-prefix-arg)))
  (ti::verb)
  (barf-if-buffer-read-only)
  (tinypgp-i-args-pass-phrase "Region Sign base64 pass phrase:" )

  (tinypgp-hash 'action 'put 'now    'sign 'global)
  (tinypgp-hash 'action 'put 'detail 'base64 'global)

  (let* ((orig-opt options)
         (comment
          (concat
           "base64 signed. "
           "run signature verify to to dearmor to clear text. ")))

    (when (and (null orig-opt) (not (ti::win32-p)))
      (setq options (format "+comment=\"%s\"" comment)))

    ;; Add user options to the end
    ;;
    (setq options (concat "+clearsig=off " options))
    (tinypgp-sign-region beg end verb options nil register )

    (when (and (null orig-opt) (ti::win32-p))
      (tinypgp-binary-header-field-set "Comment:" comment)))

  (if (and verb register)
      (message
       (substitute-command-keys
        (format
         (concat
          "Results in register `%c'. View it with "
          "\\[tinypgp-view-register]")
         tinypgp-:register)))))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypgp-sign-mail-mime  ()
  "Sign buffer as PGP/MIME using SEMI or TM.
Function activates mime mode if needed."
  (interactive)

  (unless (ti::re-search-check mail-header-separator)
    (error "Tinypgp: MPGP/MIME Must have mail buffer."))

  (tinypgpd "tinypgp-sign-mail-mime: MIME-P" (ti::mail-mime-feature-p))

  (when (ti::mail-mime-feature-p)
    (ti::mail-mime-turn-on-mode))

  (unless (ti::mail-mime-sign-region)
    (error "Can't sign PGP/MIME. TM or SEMI is not active."))

  (ti::mail-mime-turn-off-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-mail-detached ()
  "Create detached signature to register `tinypgp-:register' using PASSWORD."
  (interactive)
  (tinypgp-i-args-pass-phrase "Detach sign password: ")
  (let* ((beg  (ti::mail-text-start))
         (end  (point-max)))
    (if (eq beg end)
        (error "TinyPgp: sign detached, There is no text in message body."))
    (tinypgp-sign-region-detached beg end (interactive-p))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-sign-region-detached
  (beg end &optional verb options noerr)
  "Put detached signature to register `tinypgp-:register'.

Input:

  BEG END   region
  VERB      verbose messages
  OPTIONS   additional option string for PGP
  NOERR     do not call error

Note:
   If verb is non-nil, correct keyring containing the key is
   first set according to `tinypgp-:user-now' before signing."
  (interactive
   (progn
     (if (null (region-active-p))
         (error "region not active"))
     (list
      (region-beginning)
      (region-end)
      t
      nil)))

  (let* ((fid "tinypgp-sign-region-detached:"))
    (ti::verb)
    (tinypgp-i-args-pass-phrase "Region detach sign pass phrase: ")
    (tinypgpd fid "in:" beg end verb options noerr)

    (tinypgp-hash 'action 'put 'now    'sign   'global)
    (tinypgp-hash 'action 'put 'detail 'detach 'global)

    ;;  This is an ugly hack, but the previous SIGN options are replaced
    ;;  with the new ones. User options are added before -bfast.

    (setq options (format "%s %s" (or (eval options) "")
                          (if (tinypgp-backend-pgp2-p)
                              " -bfast "
                            " -b -atv ")))

    (set-register tinypgp-:register nil) ;Clear it

    (tinypgp-sign-region beg end verb options nil 'register 'as-is)

    (if verb
        (message "Detached signature in register '%s'"
                 (char-to-string tinypgp-:register)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-sign-region
  (beg end &optional verb options noerr register as-is)
  "Sign region.

Input:

  BEG END       ints, region
  VERB          flag, verbose messages
  OPTIONS       string, flags to add to the real pgp command.
  NOERR         flag, return nil or t only
  REGISTER      flag, save results to register
  AS-IS         flag, if non-nil. no buffer modification is done.
                Normally would delete whitespaces at the end of lines.
"
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (if (null (region-active-p))
         (error "region not active"))
     (list
      (region-beginning)
      (region-end)
      t
      nil
      nil
      current-prefix-arg)))

  (let ((fid  "tinypgp-sign-region:")
        ret)
    (barf-if-buffer-read-only)
    (tinypgp-i-args-pass-phrase "Sign region pass phrase:")

    (tinypgp-hash 'action 'put 'now 'sign 'global)
    (tinypgp-hash 'action 'put 'detail 'region 'global)

    (unless as-is
      (ti::buffer-trim-blanks beg end)) ;EOL whitespace strip

    (tinypgpd fid "in:" beg end verb options)

    (tinypgp-save-state-macro
     (tinypgp-user-change-macro
      (cond
       ((null noerr)
        (tinypgp-set-pgp-env-macro tinypgp-:user-now 'verb
                                   (tinypgp-cmd-macro 'sign tinypgp-:user-now nil
                                                      "Signing..." register options))
        (setq ret t))

       (t
        (ignore-errors
          (tinypgp-set-pgp-env-macro tinypgp-:user-now 'verb
                                     (tinypgp-cmd-macro 'sign tinypgp-:user-now nil
                                                        "Signing..." register options))
          (setq ret t))))))

    (when ret
      (tinypgp-key-cache-update)
      (tinypgp-sign-data-set))

    ret))

;;}}}
;;{{{ interactive, verifying

;;; ....................................................... &verifying ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-verify-maybe-fetch-key (status-string)
  "If verify fails, asks if we should try to fetch key.

Input:
  STATUS-STRING     ,the result of verify

Return:
  t                 ,if key fetch tried.
  nil"
  (let* ((fid  "tinypgp-verify-maybe-fetch-key:")
         (tinypgp-:find-by-guess-hook (copy-list tinypgp-:find-by-guess-hook))
         key-id)
    ;;  We already tried these methods, there is finger
    ;;  and http left
    (setq tinypgp-:find-by-guess-hook
          (delq
           'tinypgp-key-find-by-cache
           (delq
            'tinypgp-key-find-by-keyrings-verbose
            tinypgp-:find-by-guess-hook)))

    (when (setq key-id
                (ti::string-match "ID \\([^ \t]+\\) not found"
                                  1 status-string))

      ;; Key matching expected Key ID C4AF0331 not found in file
      ;; '/home/xxx/.pgp/pubring.pgp'.

      (tinypgpd fid "status" status-string key-id)

      (when (y-or-n-p
             (format "Can't verify: fetch key for %s ? "
                     key-id))
        (tinypgp-key-find-by-guess key-id)
        t))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-verify-region (beg end &optional no-replace verb)
  "Verify message in region.

If signature is good and there is some PGP message inside it,
say encrypted to you, then message is replaced
with the output of PGP. In short: message is unpacked.

If there is no PGP, this function does nothing.

Input:

  BEG END       region which is feed to PGP. If both are nil, then whole
                buffer is used.

  NO-REPLACE    If non-nil prefix argument, the result is put into
                register instead. RBEG and REND are replace position beg
                and end points. They default to BEG and END. These
                arguments are useful if you feed whole buffer to PGP but
                want the replace tho happen only in cerating region.
  VERB          Verbose messages

References:
  `tinypgp-:verify-before-hook'
  `tinypgp-:verify-after-hook'"
  (interactive "r")
  (tinypgpd "tinypgp-verify-region in:" no-replace verb)

  (let* ((cmd       (tinypgp-binary-get-cmd 'verify))
         (fid       "tinypgp-verify-region: ")
         (sig-holder (ignore-errors
                       (or (car-safe
                            (ti::mail-email-from-string
                             (mail-fetch-field "from")))
                           ;;  Maybe this is mail message that user has
                           ;;  just signed and he want to varify it himself
                           tinypgp-:user-now)))
         (do-it        t)
         stat
         region
         pring
         msg
         ret
;;;      stat
         info)

    (ti::verb)

    (tinypgp-hash 'action 'put 'now      'verify    'global)
    (tinypgp-hash 'action 'put 'no-replace no-replace 'global)

    (setq msg (if verb
                  "Verifying signature..."
                nil))

    (if (null (ti::mail-pgp-p))
        (if verb (message "No PGP tags found."))

      (run-hook-with-args-until-success
       'tinypgp-:verify-before-hook 'verify beg end)

      ;;    Because we have our own hooks, we can use the
      ;;    command macro, because if we'd call it see what happens:
      ;;
      ;;    V-B-hook
      ;;      macro (macro-B-hook macro-E-hook)
      ;;    V-E-hook
      ;;
      ;;    The macro-E-hook would e.g. cease rmail-edit-mode
      ;;    already. That's why we don't use the macro here at all.

      (tinypgpd fid "verb sig-holder"  verb sig-holder "BEG" beg end )

      (tinypgp-save-state-macro
       (when (and verb sig-holder)
         (if (or (setq pring (tinypgp-key-find-by-keyrings
                              (tinypgp-key-id-conversion sig-holder)))
                 ;;   Hmmm, User's email weren't found, find HEX key-id
                 ;;   from base64 signature them. This is slower way
                 ;;
                 (and (setq info (ti::mail-pgp-stream-forward-and-study t))
                      (setq sig-holder
                            (concat "0x"
                                    (ti::mail-pgp-stream-data-elt
                                     info 'key-id)))
                      (setq pring
                            (tinypgp-key-find-by-keyrings
                             (tinypgp-key-id-conversion sig-holder)))
                      (message "\
Need From addr -- key-id conversion: use `tinypgp-email-substitution-add'")
                      (sit-for 5)))
             (setq tinypgp-:pubring-now pring)
           (tinypgpd fid "--Can't find key-id from keyrings")
           (if (null
                (setq
                 do-it
                 (y-or-n-p
                  (format "Can't find %s from keyrings, call pgp anyway?"
                          sig-holder))))
               (setq stat (format "ID %s not found" sig-holder)))))

       ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . action ..
       (when do-it
         (tinypgpd fid "DO-IT" cmd beg end (current-buffer))
         (setq cmd (tinypgp-cmd-compose cmd nil))
         (if (or (tinypgp-backend-pgp2-p)
                 (tinypgp-backend-gpg-p))
             (setq ret (tinypgp-binary-do-command-region
                        cmd beg end (current-buffer) msg 'string))
           (setq ret (tinypgp-binary-do-command-region-with-expect
                      cmd beg end (current-buffer) msg 'string))))

       ) ;;save-state

      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. result . .

      ;;  Was it encrypted + signed message? The result removes
      ;;  the signature around the region
      ;;
      (cond
       (no-replace
        (if verb
            (setq ret "Good signature. Results unpacked to register.")))
       (t
        (cond
         ((save-excursion               ;Normal PGP signing
            (ti::mail-hmax 'move)
            (setq region
                  (or
                   (ti::mail-pgp-block-area 'signed)
                   ;;  Base64 signed then
                   (ti::mail-pgp-block-area 'msg)))))
         ((and (ti::mail-pgp-headers-p) ;X-Pgp signed message?
               (save-excursion
                 (ti::mail-text-start 'move)

                 ;;  Message is not yet verified if this is found
                 ;;
                 (not (re-search-forward "^--+BEGIN.*PGP" nil t))))
          (setq region (cons (ti::mail-text-start) (point-max))))
         (t
          (error "\
Cannot find PGP signature. Already verified or signature hidden?")))

        (tinypgpd "REGION" region (current-buffer))

        (delete-region (car region) (cdr region))
        (goto-char (car region) )
        (tinypgp-binary-insert-pointer-data ret 'beg)

        (setq ret
              (tinypgp-binary-get-result-using-function
               'tinypgp-binary-get-result-verify))

;;;     (setq stat (or (tinypgp-binary-get-result-verify-status) ""))
        (when (and (null no-replace) (ti::mail-pgp-headers-p))
          ;;  We must remove the X-Pgp too.
          ;;
          (tinypgp-signature-from-header 'just-remove-all))))

      (run-hook-with-args-until-success
       'tinypgp-:verify-after-hook 'verify beg end ret)

      (setq stat (or stat
                     (tinypgp-binary-get-result-verify-status)
                     ""))
      (cond
       ((and verb (tinypgp-verify-maybe-fetch-key stat))
        nil)                            ;Nothing more to do
       (verb
        (setq msg (or (tinypgp-binary-get-result-verify-status)
                      "<unknown verify status>"))
        (if (fboundp tinypgp-:verify-message-function)
            (funcall tinypgp-:verify-message-function msg)
          (message msg)))))
    (tinypgpd fid "out: " ret "stat" stat)

    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-verify-detached-signature (file key-id &optional pring verb)
  "Verify detached signature in current buffer against file on disk.

Input:

  FILE
  KEY-ID    If this string has '@' e.g.  'mister foo <qf@site.com>'
            the key-id is automatically shortened to 'qf@site.com'.
  PRING     If nil; all pubrings are searched to contain key-id and if none
            found, funtion calls error. The found keyring is used to call
            pgp with option +pubring
  VERB      Verbose messages.

Return:

  nbr       PGP error code
  nil       verify successfull.

References:
  `tinypgp-:buffer-tmp-shell'   pgp response"

  (interactive
   (let* ((data (ti::mail-pgp-stream-forward-and-study 'search 'any))
          (type (car data))
          (key-id  (and (eq type  'signed)
                        (ti::mail-pgp-stream-data-elt data 'key-id))))
     (if (not (stringp key-id))
         (error "Can't find key id from PGP stream?")
       (setq key-id (concat "0x" key-id)))

     (list
      (read-file-name "Verify detach signed file: " nil nil t)
      key-id)))
  (let* ((fid "tinypgp-verify-detached-signature:")
         out
         status
         email)
    (ti::verb)
    (tinypgp-hash 'action 'put 'now 'verify 'global)
    (if (and (string-match "@" key-id)
             (setq email (car-safe (ti::mail-email-from-string key-id))))
        (setq key-id email))

    (tinypgpd fid "in:" file key-id "OPT" pring verb)

    (setq file (tinypgp-expand-file-name file))
    ;;  First we have to know in what pubring the key is in, because
    ;;  PGP needs pubring when it checks the key.
    ;;
    (or pring
        (or (setq pring (tinypgp-key-find-by-cache key-id))
            (setq pring (tinypgp-key-find-by-keyrings key-id)))
        (error "Can't find '%s' from any pubring." key-id))
    (tinypgpd fid "pring" pring)

    (setq out    (tinypgp-ti::temp-buffer 'shell))
    (save-excursion (ti::pmin) (tinypgp-file-control 'source-write))

    ;;  call-process-region
    ;;              START END PROGRAM
    ;;    &optional DELETE DESTINATION DISPLAY
    ;;    &rest     ARGS
    ;;
    ;;  % pgp sig-file original-file
    ;;
    (setq
     status
     (call-process-region (point-min) (point-max) "pgp"

                          (not 'text-delete)
                          out
                          (not 'constant-display)

                          tinypgp-:file-source
                          file
                          (format "+pubring=%s" pring)))
    (tinypgp-file-control 'source-kill)

;;;    (pop-to-buffer (current-buffer)) (ti::d!  orig-file pring)
;;;    (pop-to-buffer out)

    (if verb
        (message (or (tinypgp-binary-get-result-verify-status)
                     (and (pop-to-buffer out)
                          "<unknown verify results>"))))

    ;;  Convert 0(pgp ok) to nil(lisp ok) return code
    ;;
    (if (eq 0 status)
        nil
      status)))

;;; ----------------------------------------------------------------------
;;; - parameters BEG and end _must_ be nil
;;;
(defun tinypgp-verify-mail (&optional no-replace verb)
  "Verify message in mail buffer. See `tinypgp-verify-region' for more details.

Input:
  NO-REPLACE    flag, store results to `tinypgp-:register'
  VERB          flag, display verbose messages"
  (interactive "P")
  (let ((fid   "tinypgp-verify-mail:")
        hidden
        stat)

    (ti::verb)
    (tinypgp-hash 'action 'put 'now      'verify    'global)
    (tinypgp-hash 'action 'put 'no-replace no-replace 'global)

    (tinypgpd fid "in:" no-replace verb)

    (when (tinypgp-hidden-p)
      (tinypgp-hide 'show)
      (setq hidden t))

    (tinypgp-verify-region
     nil nil                     ;it is not a good idea to pass region
     no-replace
     verb)

    (setq stat (or (tinypgp-binary-get-result-verify-status)
                   ""))
    (when (and (null no-replace)
               (ti::mail-pgp-headers-p)
               (not (string-match "bad\\|not found" stat)))
      ;;  We must remove the X-Pgp signed fields, if the status was ok
      ;;
      (tinypgp-signature-from-header 'just-delete))

    (if hidden
        (tinypgp-hide))))

;;}}}
;;{{{ interactive, encrypting

;;; .......................................................... &encypt ...

(defun tinypgp-encrypt-add-remailer-tag ()
  "Add' Encrypted: PGP' remailer tag to the point in mail mode buffers."
  (if (ti::mail-mail-mode-p)
      (insert "::\nEncrypted: PGP\n\n")))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-allowed-check ()
  "In certains situations it is not allowed to encrypt the mail message.
Check those cases and call error."
  (let* ()
    (cond
     ((and (tinypgp-nymserver-mail-p)
           (or (string-match "," (or (mail-fetch-field  "To") ""))
               (mail-fetch-field        "CC")))
      (error "\
Impossible to encrypt Nymserver mail to multiple recipients.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-by-cache (string func &rest args)
  "If the STRING is found from cache, encrypt with FUNC and ARGS.

Return:
  t
  nil"
  (tinypgpd "tinypgp-encrypt-by-cache: " string func args )
  (let* ((pring (tinypgp-key-find-by-cache string)))
    ;; # todo: not tested
    (when pring
      (tinypgp-save-state-macro
       (setq tinypgp-:pubring-now pring)
       (apply func args)
       t))))

;;; ----------------------------------------------------------------------
;;;
(defun  tinypgp-encrypt-mail-mime  ()
  "Sign buffer as PGP/MIME using SEMI or TM."
  (interactive)

  (unless (ti::re-search-check mail-header-separator)
    (error "Tinypgp: PGP/MIME needs mail buffer."))

  (tinypgpd "tinypgp-encrypt-mail-mime: MIME-P" (ti::mail-mime-feature-p))

  (when (ti::mail-mime-feature-p)
    (ti::mail-mime-turn-on-mode))

  (unless (or (not (ti::mail-mime-feature-p))
              (ti::mail-mime-encrypt-region))
    (error "Can't encrypt PGP/MIME. TM or SEMI is not active."))
  (ti::mail-mime-turn-off-mode))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-mail-verbose (&optional arg)
  "Call `tinypgp-encrypt-mail' like user would with ARG."
  (eval
   (`
    (tinypgp-encrypt-mail
     (,@ (tinypgp-encrypt-mail-i-args arg nil 'bquote))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-mail-find-keyring (single &optional sign-pwd)
  "Find keyring for SINGLE key-id and encrypt and optionally use SIGN-PWD."
  (tinypgp-encrypt-mail single nil sign-pwd nil 'verb))

;;; ----------------------------------------------------------------------
;;; - parameters BEG and end _must_ be nil
;;;
;;;###autoload
(defun tinypgp-encrypt-mail-sign
  (single-or-list &optional no-replace sign-pwd options verb noerr)
  "See `tinypgp-encrypt-mail'. Raise parameter 'sign'.
SINGLE-OR-LIST NO-REPLACE SIGN-PWD OPTIONS VERB NOERR."
  (interactive (tinypgp-encrypt-mail-i-args
                current-prefix-arg
                'pwd))
  (ti::verb)
  (tinypgp-encrypt-mail
   single-or-list no-replace sign-pwd options verb noerr))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-mail-i-args (&optional arg pwd bquote)
  "Read args for `tinypgp-encrypt-mail'.
Input:
  ARG        prefix arg
  PWD        If non-nil, ask password
  BQUOTE     If you call this function in macro which uses ,@ you must
             set this flag to non-nil"
  (tinypgpd "tinypgp-encrypt-mail-i-args")

  (list
   (if bquote
       ;;  We need to protect this list or else Backquote
       ;;  tries to call first element as a function
       ;;
       (quote (tinypgp-i-args-read-email nil "Encrypt to: "))
     (tinypgp-i-args-read-email nil "Encrypt to: "))
   (or arg current-prefix-arg)
   pwd
   (if (null tinypgp-:r-mode-indication-flag)
       (eval tinypgp-:pgp-binary-interactive-option))))

;;; ----------------------------------------------------------------------
;;; - parameters BEG and end _must_ be nil
;;;
;;;###autoload
(defun tinypgp-encrypt-mail
  (single-or-list &optional no-replace sign-flag options verb noerr)
  "Encrypt mail buffer.

Input:

  SINGLE-OR-LIST    List of key-ids. Interactive call reads To,CC,BCC.
  NO-REPLACE        prefix arg, store result to `tinypgp-:register'.
  SIGN-FLAG         if non-nil, sign at the same time as you encrypt.
  OPTIONS           Additional pgp option string.
  VERB              If non-nil, verbose messages.
  NOERR             If non-nil, do not call error.

Function call note:

  [interactive]
  In case the EMAIL address you're sending doesn't have entry in your
  keyring, but you know that person has a PGP public key, then
  please remove the email address prior calling this function and
  it will prompt you a string to match for USER.

  If this function is called interactively, it tries to set right
  pubring by querying cache and other keyrings (user prompted)
  Also the `tinypgp-:pgp-binary-interactive-option' is suppressed if
  `tinypgp-:r-mode-indication-flag' is non-nil

  Normally the To field's address is read and used for encryption.
  However, if you are _on_ line that has email address in format
  <foo@site.com> then your are asked if you want to use this email
  instead. You can complete between this and To address.

  [when called as lisp function]
  Be sure to take precaution when passing OPTIONS if the message is
  sent to remailer. Any extra keyword, like 'Comment:'
  may reveal your identity.

  SINGLE-OR-LIST is not processed with `tinypgp-key-id-conversion'.
  You should call it manually if you want to respect user's
  substitution definitions.

  [Genenal note]
  If there are multiple recipiens in the To, CC, BCC field the
  last keyring in the `tinypgp-pubring-table' is used when doing the
  encryption.

Input:

  single-or-list    list of email addresses or KEY ID's
  no-replace        flag, do not replace area with encryption
  options           string, extra options passed to pgp exe
  verb              flag, allow printing messages."
  (interactive (tinypgp-encrypt-mail-i-args current-prefix-arg))
  (let* ((fid       "tinypgp-encrypt-mail:")
         (beg-text  (ti::mail-text-start))
         beg
         end
         elt)

    (ti::verb)
    (tinypgp-hash 'action 'put 'now      'encrypt   'global)
    (tinypgp-hash 'action 'put 'detail   nil        'global)
    (tinypgp-hash 'action 'put 'no-replace no-replace 'global)

    (tinypgp-encrypt-allowed-check)

    (put 'tinypgp-:return-value 'find-by-keyrings nil) ;reset

    (tinypgpd "tinypgp-encrypt-mail in: "
              single-or-list
              "no-rep"  no-replace
              "1pass"   sign-flag
              "options" options
              "verb"    verb
              "BEG" beg (point-max))

    (unless single-or-list
      (error "single-or-list is empty"))

    (if (eq beg-text (point-max))
        (error "Nothing to do, no text found."))

    (setq single-or-list (tinypgp-user-list single-or-list))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  do it . .
    (setq single-or-list (ti::list-make single-or-list))

    (tinypgp-cmd-macro-email "Encrypt"
                             (tinypgp-set-pgp-env-macro single-or-list 'verb

                                                        ;;  See the tinypgp-key-find-by-keyrings function.
                                                        ;;  Effective encrypt key may have changed

                                                        (when (setq elt (get 'tinypgp-:return-value 'find-by-keyrings))
                                                          (tinypgpd "tinypgp-encrypt-mail: KEY CHANGED " elt )
                                                          (setq single-or-list (ti::list-make elt)))

                                                        ;;  Beacuse the Encrypt and signing is done
                                                        ;;  in 'One pass' both keys must be in same pubring.

                                                        (when sign-flag
                                                          (tinypgpd fid "1pass: PUBRING CHANGED TO BIG")
                                                          (tinypgp-hash 'action 'put '1pass nil 'global)
                                                          (tinypgp-pubring-set-big))

                                                        ;; single-or-list will be changed if it is nil.
                                                        ;; --> user login name

                                                        (tinypgp-cmd-macro
                                                         (if sign-flag 'encrypt-sign 'encrypt)
                                                         single-or-list
                                                         nil
                                                         "Encrypting...." no-replace options)

                                                        ;; If all went ok, then we update cache, user XXX in in pubring YYY
                                                        ;; All users must be in same pubring otherwise the previous command
                                                        ;; didn't succeed.

                                                        (dolist (elt single-or-list)
                                                          (when (stringp elt)
                                                            (tinypgp-key-cache 'put elt tinypgp-:pubring-now)))))

    ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . results . .

    (when (and (null no-replace)
               (null buffer-read-only)
               tinypgp-:encrypt-after-function)
      (if (ti::mail-mail-p)
          (goto-char (ti::mail-text-start)) ;ignore other buffers
        (ti::pmin))

      (ti::save-with-marker-macro
        (funcall tinypgp-:encrypt-after-function)))

    (when verb
      (message
       "%d: Encrypted to %s %s"
       (length single-or-list)
       (ti::list-to-string single-or-list)
       (if (null sign-flag) ""
         (format "and signed [%s]" tinypgp-:user-now)))
      ;;  Make sure this is seen
      (sleep-for 2))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-region-i-args (&optional pwd)
  "Ask arguments for `tinypgp-encrypt-region-i-args' possibly also asking PWD."
  (ti::list-merge-elements
   (tinypgp-i-args-reg-email "Encrypt to: ")
   current-prefix-arg
   (if pwd (tinypgp-password-set
            (format "[%s] Sign password: " tinypgp-:user-now)))
   (if (null tinypgp-:r-mode-indication-flag)
       (eval tinypgp-:pgp-binary-interactive-option))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-encrypt-region-sign
  (beg end user &optional no-replace sign-pwd options verb)
  "Same as `tinypgp-encrypt-region' but raise 'sign' parameter.
BEG END USER NO-REPLACE SIGN-PWD OPTIONS VERB"
  (interactive (tinypgp-encrypt-region-i-args 'pwd))
  (ti::verb)
  (tinypgp-encrypt-region
   beg end user no-replace sign-pwd options verb))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-encrypt-region
  (beg end user &optional no-replace sign-pwd options verb)
  "Encrypt region.

Input:

  BEG END       region
  USER          key-id (possibly email) or list of keyIds.
  NO-REPLACE    prefix arg, store results to `tinypgp-:register'
  SIGN-PWD      if non-nil string, Sign at the same time as you encrypt.
  OPTIONS       Additional option string for PGP.
  VERB          If non-nil, Verbose messages."
  (interactive (tinypgp-encrypt-region-i-args))
  (tinypgpd "tinypgp-encrypt-region in:"
            beg end user "replace" no-replace options verb)

  (ti::verb)
  (tinypgp-encrypt-allowed-check)
  (tinypgp-password-set (format "[%s] Encrypt password: " tinypgp-:user-now))

  (tinypgp-hash 'action 'put 'now               'encrypt    'global)
  (tinypgp-hash 'action 'put 'no-replace        no-replace  'global)

  (setq user (tinypgp-user-list user))

  (tinypgp-set-pgp-env-macro user 'verb
                             (tinypgp-cmd-macro
                              (if sign-pwd 'encrypt-sign 'encrypt )
                              user
                              sign-pwd
                              "Encrypting...." no-replace options))

  ;; If all went ok, then we update cache, use XXX in in pubring YYY
  (tinypgp-key-cache-update (car (ti::list-make user))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-encrypt-info  (&optional register verb)
  "Check encrypted message and who can read it.
If Flag REGISTER is non-nil store results to `tinypgp-:register'. VERB."
  (interactive)
  (let* ((user tinypgp-:user-primary)
         ptr
         list
         str
         beg
         end)
    (ti::verb)
    (ignore-errors                      ;We know this generates error.
      (tinypgp-set-pgp-env-macro user 'verb
                                 (tinypgp-cmd-macro
                                  'encrypt-info
                                  user
                                  nil
                                  "Checking encrypt users...." 'no-replace (not 'options))))
    (setq ptr (tinypgp-binary-get-result-encrypt-info))

    (cond
     ((null ptr)
      (message
       "TinyPgp: Can't find list of encrypt users. Maybe not encrypted."))
     (register
      (with-current-buffer (car ptr)
        (set-register tinypgp-:register
                      (buffer-substring (nth 1 ptr) (nth 2 ptr))))
      (if verb
          (message "Encrypt info in register '%s'"
                   (char-to-string tinypgp-:register))))
     (t
      (setq list (tinypgp-binary-get-result-encrypt-info-list ptr)
            str  (ti::list-to-string list ","))

      (if (< (length str) 75)
          (message "Encrypt: %s" str)
        ;;  Hm, Doesn't fit in echo area, so display in another window
        (tinypgp-ti::temp-buffer 'show)
        (display-buffer tinypgp-:buffer-tmp-show)
        (with-current-buffer (car ptr)
          (append-to-buffer tinypgp-:buffer-tmp-show
                            (nth 1 ptr) (nth 2 ptr))))))))

;;}}}
;;{{{ interactive, decrypting

;;; .......................................................... &decypt ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-decrypt-signed-base64
  (beg end user &optional no-replace verb)
  "Decrypt conventinally signed but base64 coded text.

Input:

  BEG END       region
  USER          key-id string (possibly email)
  NO-REPLACE    store results to `tinypgp-:register'
  VERB          Verbose messages."
  (let* ((fid  "tinypgp-decrypt-signed-base64: ")
         pointer
         file-out
         file-write)

    (ti::verb)
    (tinypgp-hash 'action 'put 'now 'decrypt 'global)
    (setq file-out (ti::mail-pgp-comment-file-p beg))

    (tinypgpd fid "in:" beg end user no-replace verb)

    (when file-out
      (setq
       file-write
       (read-file-name
        "Base64 block save contents to file: "
        nil (concat default-directory file-out) nil file-out))
      (cond
       ((ti::nil-p file-write)
        (setq file-write nil))

       ((not (file-exists-p (file-name-directory file-write)))
        (error "No such directory %s" file-write))

       ((file-exists-p file-write)
        (if (y-or-n-p "File exists, overwrite?")
            (delete-file file-write)
          (error "Abort.")))))

    (if file-write
        (setq no-replace t))

    (tinypgp-cmd-macro 'decrypt-base64 user nil "Decrypting..." no-replace)

    ;; The result of PGP is not delimited by any
    ;; --- TAG, so we cannot request replace now, but read the contents
    ;; by hand first
    ;;
    (with-current-buffer tinypgp-:buffer-tmp-shell
      (setq   pointer (tinypgp-binary-get-result-base64))
      (unless pointer (tinypgp-error "No output from PGP.")))

    (cond
     (file-write
      (with-current-buffer (tinypgp-ti::temp-buffer)
        (tinypgp-binary-insert-pointer-data pointer)
        (write-region (point-min) (point-max) file-write)
        (erase-buffer))
      (message "Wrote %s" file-write))
     (no-replace
      (set-register tinypgp-:register
                    (tinypgp-binary-get-result-as-string pointer)))
     (t
      (delete-region beg end)
      (tinypgp-binary-insert-pointer-data pointer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-decrypt-arg-function (arg)
  "See how we should interpret the passed prefix ARG.
If buffer is read-only, then assume, that it may be MAIL buffer
or the like and honor the variable `tinypgp-:decrypt-arg-interpretation'

If buffer is not read-only. return ARG as is."
  (if (not buffer-read-only)            ;regular buffer
      arg

    ;; This may be MAIL buffer, because it is read only,
    ;; see how user want the arg to be intepreted.

    (if (null tinypgp-:decrypt-arg-interpretation)
        arg                             ;as is
      (if arg                           ;reverse sense
          nil
        tinypgp-:decrypt-arg-interpretation))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-decrypt-mail-verbose (&optional prefix-arg)
  "Call `tinypgp-decrypt-mail' like user would with PREFIX-ARG."
  ;;  Loonks cryptic? Not really, because i-args returns a
  ;;  list and tinypgp-decrypt-mail needs individual args,
  ;;  we use eval + backquote to construct command that
  ;;  turns list into individual args before
  ;;  it calls tinypgp-decrypt-mail.
  ;;
  ;;  Got it? No? Then you must learn backquote syntax first.
  ;;
  (eval
   (` (tinypgp-decrypt-mail
       (,@ (tinypgp-decrypt-mail-i-args prefix-arg))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-decrypt-mail-i-args (&optional arg)
  "Ask args to function `tinypgp-decrypt-mail'.
ARG passed can be `current-prefix-arg' if that is known."
  (tinypgpd "tinypgp-decrypt-mail-i-args: ")
  (tinypgp-hash 'action 'put 'now 'decrypt 'global)

  (if (null (ti::mail-pgp-p))
      (error "Nothing to do. No pgp found."))

  (list
   (funcall tinypgp-:pgp-decrypt-arg-function arg)
   (tinypgp-i-args-decrypt)
   ;; c-point
   nil))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-decrypt-mail (&optional no-replace type verb)
  "Decrypt mail buffer.
The PGP data in the buffer is detected by reading the CTB bits:
see pgpformat.doc in pgp documentation.

Input:

  NO-REPLACE    flag, prefix arg instructs to show the cotent in
                separate buffer. See refrerence note too.
                If this is 'preview and verb argument is nil-nil,
                then automatically show content is different buffer.

  TYPE          nil or \"pgp\" --> PGP encrypted
                \"base64\" --> base64 signed and
                \"conventional\" --> encrypted with conventional key.

  VERB          Verbose mode.

References:

  `tinypgp-:pgp-encrypted-p-function'
  `tinypgp-:decrypt-arg-interpretation'     for interactive calls
  `tinypgp-:pgp-decrypt-arg-function'     for interactive calls
  `tinypgp-:user-identity-table'"

  (interactive (tinypgp-decrypt-mail-i-args current-prefix-arg))
  (tinypgpd "tinypgp-decrypt-mail in:" no-replace type verb)

  (let* ((fid       "tinypgp-decrypt-mail:")
         (region    (save-excursion (ti::pmin) (ti::mail-pgp-block-area 'msg)))
         (beg       (car-safe region))
         (end       (cdr-safe region))
         (buffer    (current-buffer))
         stat)

    (ti::verb)

    (tinypgp-hash 'action 'put 'now      'decrypt   'global)
    (tinypgp-hash 'action 'put 'type     type       'global)
    (tinypgp-hash 'action 'put 'no-replace no-replace 'global)

    (if (null region)
        (error "No PGP encrypt block found."))

    (tinypgp-save-state-macro
     (tinypgpd fid "user" tinypgp-:user-now)

     ;; ... ... ... ... ... ... ... ... ... ... ...  normally encrypted ...

     (cond
      ((member type '("conventional" "pgp"))
       (tinypgp-save-state-macro
        (tinypgp-user-change-macro
         (tinypgp-cmd-macro-email "Decrypt"
                                  (tinypgp-decrypt-region beg end no-replace type verb)))))

      ;; ... ... ... ... ... ... ... ... ... ... ... ... ... ... base64  ..

      ((member type '("base64"))

       (when no-replace
         (setq buffer (tinypgp-ti::temp-buffer))
         (append-to-buffer buffer beg end))

       (with-current-buffer buffer

         ;; There may be several blocks, open them all.
         ;; This is the first one.

         (tinypgp-decrypt-signed-base64 beg end nil no-replace)
         (while (and (setq region
                           (save-excursion
                             (goto-char end)
                             (ti::mail-pgp-block-area 'msg)))
                     (setq beg (car region) end (cdr region)))
           (tinypgp-decrypt-signed-base64 beg end nil no-replace))))
      (t
       (error "Unkown decrypt type '%s'" type))))

    (goto-char (ti::mail-text-start))

    ;;    The message may have been encrypted and signed (one pass).
    ;;    Check it too.

    (when (and verb
               (setq stat (tinypgp-binary-get-result-verify-status)))
      (message "[was signed] %s" stat))

    (tinypgp-hash 'action 'put 'type nil 'global) ;Clear this
    (tinypgpd "tinypgp-decrypt-mail out: user" tinypgp-:user-now)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-decrypt-region (beg end &optional no-replace type verb)
  "Decrypt region. Signal error is there is no decrypt message.

Input:

  BEG END       int, region
  NO-REPLACE    flag, store contents to `tinypgp-:register'.
                If values is 'review and verb is non-nil, also display
                content in separate buffer. Calls `tinypgp-view-register'
  TYPE          string, Decrypt type: conventional, base64 or pgp
  VERB          flag, verbose messages"
  (interactive
   (progn
     (tinypgpd "tinypgp-decrypt-region interactive")
     (ti::list-merge-elements
      (ti::i-macro-region-body)
      current-prefix-arg
      'iact
      (tinypgp-i-args-decrypt))))

  (let* ((fid  "tinypgp-decrypt-region")
         user)                          ;Must be defined due to macro

    (tinypgpd fid "in:" beg end no-replace type verb (current-buffer))

    (tinypgp-hash 'action 'put 'now     'decrypt    'global)
    (tinypgp-hash 'action 'put 'type     type       'global)
    (tinypgp-hash 'action 'put 'no-replace no-replace 'global)

    (if (null (save-excursion (ti::pmin) (ti::mail-pgp-block-area 'msg)))
        (error "No PGP encrypt block found."))

    (tinypgpd fid "in:" beg end no-replace verb)
    (tinypgp-cmd-macro 'decrypt user  nil "Decrypting..." no-replace)

    (when (and no-replace verb)
      (or (get-buffer-window tinypgp-:buffer-view t) ;already visible
          (eq no-replace 'preview)
          (y-or-n-p "View content in temp buffer? "))
      (tinypgp-view-register))

    (tinypgp-hash 'action 'put 'type nil 'global)))

;;}}}
;;{{{ interactive: regular crypting

;;; ............................................................ &cypt ...

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-crypt-mail (password &optional no-replace comment verb)
  "Crypt mail buffer.

Input:

  PASSWORD      pass phrase
  NO-REPLACE    store contents to `tinypgp-:register'.
  COMMENT       Additional comment added
  VERB          verbose messages"
  (interactive
   (list
    (ti::compat-read-password "Crypt password: ")
    current-prefix-arg))
  (let* ((beg  (ti::mail-text-start))
         (end  (point-max))
         (verb (or verb (interactive-p))))
    (tinypgp-hash 'action 'put 'now 'crypt 'global)
    (tinypgp-crypt-region beg end password no-replace comment verb)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-crypt-region
  (beg end password &optional no-replace comment verb)
  "Crypt region.

Input:

  BEG END       region
  PASSWORD      pass phrase
  NO-REPLACE    store contents to `tinypgp-:register'.
  COMMENT       The comment string.
  VERB          verbose messages"
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (ti::i-macro-region-body
       (read-from-minibuffer "Crypt password: ")
       current-prefix-arg
       "")))

  (let* ((obuffer (current-buffer)))
    (tinypgp-hash 'action 'put 'now 'crypt 'global)

    (with-current-buffer (tinypgp-ti::temp-buffer)
      (insert-buffer-substring obuffer beg end)
      (ti::pmin) (tinypgp-file-control 'source-write)

      (tinypgp-crypt-do-with-pgp
       tinypgp-:file-source tinypgp-:file-output password (or comment ""))

      (cond
       (no-replace
        (erase-buffer)
        (insert-file-contents tinypgp-:file-output)
        (set-register tinypgp-:register (buffer-string)))
       (t
        (with-current-buffer obuffer
          (delete-region beg end) (goto-char beg)
          (insert-file-contents tinypgp-:file-output)))))
    (tinypgp-file-control 'source-kill)))

;;}}}
;;{{{ interactive, extra, header toggle

;;; ..................................................... &interactive ...

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-xpgp-header-mode-toggle (&optional arg)
  "Toggle X-pgp header mode with ARG.

References
  `tinypgp-:header-sign-table'     ,this variable overrides the signing mode."
  (interactive "P")
  (ti::bool-toggle tinypgp-:xpgp-signing-mode arg)
  (if (interactive-p)
      (message
       (concat "TinyPgp: X-Pgp header mode: "
               (if tinypgp-:xpgp-signing-mode
                   "on" "off"))))

  (tinypgp-update-modeline)
  tinypgp-:xpgp-signing-mode)           ;return the changed value

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-xpgp-header-toggle ()
  "Togle moving signature FROM/TO headers."
  (interactive)
  (cond
   ((null (tinypgp-mail-buffer-p 'message)))
   (t
    (ti::save-line-column-macro nil nil ;preserve user's position
      (with-buffer-modified
        (cond
         ((ti::mail-pgp-headers-p)
          (tinypgp-signature-from-header))
         ((ti::mail-pgp-normal-p)
          (tinypgp-signature-move-to-header nil 'no-cnv))
         (t
          (message "No PGP signature found..."))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-hide-gnus (&optional unhide)
  "Hide or UNHIDE pgp signature in GNUS."
  (let* ((buffer (if (boundp 'gnus-article-buffer)
                     ;; Silence byteComp.
                     (symbol-value 'gnus-article-buffer))))
    (when (stringp buffer)
      (with-current-buffer buffer
        (tinypgp-hide)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-hide (&optional unhide)
  "Hide PGP signatures, optionally UNHIDE."
  (ti::mail-pgp-signature-normal-do-region
   (tinypgp-invisible-region area-beg area-end  unhide)
   ;; return value on success
   t))

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-show ()
  "Show PGP signature."
  (tinypgp-hide 'show))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinypgp-hide-show-toggle ()
  "Togle hiding and showing the PGP signature."
  (interactive)
  (let* (ret)
    (setq ret
          (if (tinypgp-hidden-p)
              (tinypgp-show)
            (tinypgp-hide)))
    (if (and (interactive-p)
             (null ret))
        (message "No signature found to un/hide"))))

;;}}}
;;{{{ interactive, keyserver submit

;;; ................................................ &keyserver-submit ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-keysrv-send-email-command (email command &optional arg)
  "Send to EMAIL address a keyserver COMMAND with ARG.
The COMMAND is placed in the subject line. If command is 'add' then
the current buffer is sent to keyserver.

See keyserver documentation for more up to date command definitions:

Command                Message body contains
--------------------------------------------------------------------
ADD            Your PGP public key (key to add is body of msg) (-ka)
INDEX          List all PGP keys the server knows about (-kv)
VERBOSE INDEX  List all PGP keys, verbose format (-kvv)
GET            Get the whole public key ring (-kxa *)
GET <userid>   Get just that one key (-kxa <userid>)
MGET <userid>  Get all keys which match <userid>
LAST <n>       Get all keys uploaded during last <n> days
--------------------------------------------------------------------"
  (interactive
   (let* ((obuffer  (current-buffer))
          arg1
          arg2
          arg3
          elt)
     (setq arg1 (tinypgp-ask-email-keyserver))
     (setq arg2 (completing-read
                 "Send command: "
                 (ti::list-to-assoc-menu
                  '("help" "add" "index" "verbose index" "get"
                    "mget" "last"))))

     (if (setq elt
               (assoc
                arg2
                '(("get" . "<userid>")
                  ("mget" . "<userid>")
                  ("last" . "<nbr of days>"))))
         (setq
          arg3
          (read-from-minibuffer
           (format "%s, possible additional parameter %s: "
                   arg2 (cdr elt)))))

     (list arg1 arg2 arg3)))

  ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . interactive end ..
  (let ((obuffer  (current-buffer))
        insert-flag)

    (if (ti::nil-p email)    (error "email is invalid."))
    (if (ti::nil-p command)  (error "command is invalid."))

    (cond
     ((string= "index" command)
      (if (null (y-or-n-p "\
Really List all PGP keys the server knows about (-kv)? "))
          (error "Abort.")))

     ((string= "verbose index" command)
      (if (null (y-or-n-p "\
Really  List all PGP keys, verbose format (-kvv) "))
          (error "Abort.")))

     ((string= "get" command)
      (if (null (y-or-n-p "\
Really Get the whole public key ring (-kxa *) "))
          (error "Abort.")))

     ((string= "mget" command)
      (if (null (y-or-n-p (format "\
Really Get all keys which match <userid %s> " arg)))
          (error "Abort.")))

     ((and (string= "add" command)
           (save-excursion
             (ti::pmin)
             (unless (ti::mail-pgp-public-key-p)
               (error "I can't send this buffer, no public key found."))
             t))
      (setq insert-flag t))

     ((member command '("help" "last"))
      nil)

     (t
      (error "unsupported command %s to %s" command email)))

    (ti::mail-sendmail-macro email command 'send
;;;       (pop-to-buffer (current-buffer)) (ti::d! "__ksrv")
                             (if insert-flag
                                 (insert-buffer obuffer)))))

;;}}}
;;{{{ interactive: misc

;;; ........................................................... &imisc ...

;;; ----------------------------------------------------------------------
;;;
(defun tinypgp-pgp-stream-forward-study (&optional verb)
  "Find PGP stream and display information from it. VERB.
The information is stored to `tinypgp-:register'.

Interactive call note:

  If can't find stream forward, then go to `point-min' and try searching
  again."
  (interactive)
  (let* (info)
    (ti::verb)
    (if (setq info (ti::mail-pgp-stream-forward-info 'search 'any))
        (set-register tinypgp-:register info)
      (setq info "Can't intepret/find PGP stream."))
    (message info)))

;;}}}
;;{{{ examples

;;; ........................................................ &examples ...
;;; - Rip code with tinylib.el/ti::package-rip-magic
;;; - Here is how I control PGP message sending: For company mail,
;;;   I don't use PGP, but for outside wordl I use quite often.
;;; - Do not use autosigning if you decide to use this kind of control.

;;* (add-hook 'mail-send-hook   'my-tinypgp-ask-if-send-pgp-mail)

;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-tinypgp-ask-if-send-pgp-mail  ()
;;*   "See if we should ask to sign the mail with PGP.
;;* - If there is already PGP blocks, do nothing.
;;* - If these are local host email addresses, do not ask PGP signing.
;;* "
;;*   (require 'tinylibmail)
;;*   (save-excursion
;;*     (let* ((to              (or (mail-fetch-field "to") ""))
;;*        (subject     (or (mail-fetch-field "subject") ""))
;;*        ;;  Exclude my local host addresses, Anon and remail posts
;;*
;;*        (skip-address-p
;;*         (or (string-match (concat
;;*                            "ntc\\|nokia\\|tne[0-9]\\|[an][na][0-9]"
;;*                            "\\|remail\\|@anon"
;;*                            )
;;*                           to)
;;*             ;;    local mail addresses do not have @ --> skip PGP
;;*             ;;    TO field does not exist in news article
;;*
;;*             (not (string-match "@" to))
;;*             ))
;;*        (mime        (ti::re-search-check
;;*                      "^--[[]\\|^--+Multi\\|--pgp-"
;;*                      0 '(ti::pmin)))
;;*        (diff        (ti::re-search-check
;;*                      "diff[ \t]+-[ucr]\\|^--- .*199[0-9]"))
;;*        pgp-ask-no
;;*        start
;;*        )
;;*       (defvar my-:pgp-previous-mail-subject nil)
;;* _
;;* _
;;*       ;; .............................................. untabify maybe ...
;;*       ;; Remove TABS; so that receiver can see the text as written
;;*
;;*       (when (and (null diff)                ;Skip diff message
;;*              (not (ti::mail-pgp-encrypted-p))       ;already encrypted
;;*              (not (ti::mail-pgp-p)) ;or other pgp
;;*              )
;;*     (untabify (ti::mail-text-start) (point-max))
;;*     )
;;* _
;;*       ;; ........................................ should we sign this? ...
;;*       ;; Raise flag if NO.
;;*       ;;
;;*       (setq pgp-ask-no
;;*         (or (not (featurep 'tinypgp))
;;*             mime
;;*             diff
;;*             skip-address-p
;;*             ;;  In news this function is called twice, prevent asking
;;*             ;;  in the second time.
;;*             ;;
;;*             (string= (or my-:pgp-previous-mail-subject "") subject)
;;*             ))
;;* _
;;*       ;; ............................................ do signing maybe ...
;;*       (when (and (null (ti::mail-pgp-p))            ;no previous pgp
;;*              (null pgp-ask-no)              ;Not a special message
;;*              (y-or-n-p "PGP sign message? ")
;;*              )
;;*     (call-interactively 'tinypgp-sign-mail))
;;* _
;;*       ;;  - Well, I just want to have confirmation after C-c C-c
;;*       ;;  - Many times I have changed my mind, or missed something I
;;*       ;;    should have added. At this point there is a short break to
;;*       ;;    have a glimpse on the message
;;*       ;;  - I want to see the "Subject", because I may have auto-replied
;;*       ;;    and started talking about whole different things -->
;;*       ;;    I should have chnaged the subject. This way I don't
;;*       ;;    forgot to change it.
;;*
;;*       (if (null (y-or-n-p (concat "Sending msg: " subject  " ")))
;;*       (error "Abort"))
;;* _
;;*       (setq my-:pgp-previous-mail-subject subject)
;;*       nil                           ;hook return value
;;*       )))

;;}}}
;;{{{ final install

(setq tinypgp-:debug t)
(when (null debug-on-error)
  (setq debug-on-error t))

(tinypgp-install)
(tinypgp-install-modes) ;;  Do this every time when package is loaded
(tinypgp-install-to-current-emacs)

;;  Until this package is labelled Alpha

(unless (featurep 'tinypgp)
  (setq debug-on-error t)
  (tinypgp-initial-message))

(tinypgp-newnym-account-expiry-warnings) ;when Newnym defined

(provide   'tinypgp)
(run-hooks 'tinypgp-:load-hook)

(error "TinyPgpg is no longer maintained. It will be removed in newar future.")

;;}}}

;;; tinypgp.el ends here
