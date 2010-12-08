;;; tinydiff.el --- Diff and patch minor mode. Browsing, patching.

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
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file
;;
;;   Here is the very basic setup. You don't necessarily need to set
;;   these variables, because they are determined at startup.
;;
;;      (setq tinydiff--diff-program  "gdiff")      ;; GNU diff
;;      (setq tinydiff--patch-program "gpatch -t")  ;; GNU patch
;;
;;      (require 'tinydiff)
;;
;;   OR use this; your .emacs will load much  quicker
;;
;;      (autoload 'tinydiff-mode              "tinydiff" "" t)
;;      (autoload 'turn-on-tinydiff-mode      "tinydiff" "" t)
;;      (autoload 'tinydiff-diff              "tinydiff" "" t)
;;      (autoload 'tinydiff-diff-show         "tinydiff" "" t)
;;      (autoload 'tinydiff-diff-show-noask   "tinydiff" "" t)
;;
;;   Here are some suggested key bindings. Reserve The "d" for diff map.
;;
;;      (global-set-key "\C-cmd" nil)     ;; Initialize prefix key
;;      (global-set-key "\C-cmdd"         'tinydiff-mode)
;;      (global-set-key "\C-cmdd"         'tinydiff-diff-show)
;;      (global-set-key "\C-cmdp"         'tinydiff-patch)
;;
;;   To debug problems:
;;
;;      M-x tinydiff-debug-toggle         turn on debug
;;      <... do as you did ...>

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, Jan 1996
;;
;;      Long ago there was set of simple functions lying around to generate
;;      instant diffs for the file that was being edited, before it was
;;      checked in with RCS. At the time *vc.el* was not in the Emacs
;;      distribution. Looking at diffs and using "goto-line" command in
;;      other buffer gave an idea to make a separate diff mode. The project
;;      turned out to be a bit bigger than just taking simple diff. You may
;;      wonder, why would you use this utility over ediff.el? If you like
;;      working with "command line" diff interface, then you may want to
;;      use this utility over *ediff.el*. There is a command prompt when
;;      various diff options can be manipulated with key bindings. Lik:
;;      Change rcsdiff to diff command, copy previous argument etc.
;;
;;  Overview of features
;;
;;      Taking diff
;;
;;      o   Buffer based simple diff/rcsdiff/patch package.
;;      o   You can diff current buffer against last saved backup copy.
;;      o   Give and manipulate diff command in echo-area: file name
;;          completion; switching between rcsdiff/diff, guess rcs version
;;          for buffer, various diff switches...
;;
;;      Browsing diff
;;
;;      o   Supported: normal diff, context diff, gnu diff -u, gnu diff -n
;;      o   When you have diff output in buffer, turning on `tinydiff-mode'
;;          allows you to browse source buffer by jumping diff blocks fwd/back
;;          and showing the source location.
;;      o   You can kill single diff block with `tinydiff-block-kill'
;;      o   You can apply only the current diff block (patch) with
;;          `tinydiff-block-apply-patch'
;;
;;      Sending or saving diff
;;
;;      o   In diff buffer, you can save the diff to a file with "W"; write
;;      o   In diff buffer, you can attach the content as MIME diff to
;;          the open mail buffer. Or if you don't have MIME-edit active,
;;          the diff is added without MIME tags. Command "M" for Mime.
;;
;;      Patch
;;
;;      o   Easy patching. Finds file to patch (along user defined paths)
;;          and applies the diff. You can receive patches by email
;;          and apply them with one or two keystrokes.
;;      o   Finds the file to patch through pre defined paths.
;;      o   Can also patch compresses .gz file.
;;      o   loads patch rejection file, if patch didn't succeed 100%
;;      o   Re-evaluates patched lisp file if the file was used by Emacs
;;      o   If you don't want to apply whole diff, use
;;          `tinydiff-block-apply-patch' for individual sections.
;;
;;  Genrating diff -- parsing diff
;;
;;      Be in buffer where you have diff file and just turn on the
;;
;;          M-x tinydiff-mode
;;
;;      Then take a look at the bindings you have available:
;;
;;          C-x b
;;
;;      If you want to generate diff against current buffer and file on
;;      disk, call function
;;
;;          M-x tinydiff-diff-show
;;
;;      In windowed system an those Emacs versions that have
;;      highlighting capabitities can enjoy more about this mode,
;;      because it marks line numbers in buffer with `mouse-face'. You
;;      can just click the point to jump to diff position.
;;
;;  Taking diffs
;;
;;      The main purpose of this package is to help taking "diff
;;      shots". This means that the file must be loaded into emacs and
;;      the cursor must be in the buffers, before you execute
;;
;;          M-x tinydiff-diff-show
;;
;;      o   if the file is not version controlled, a regular diff is offered
;;      o   if file is version controlled, your're offered vc prompt
;;      o   if the buffer has changed, you're offered to diff against
;;          last saved file to see recent changes you have done since you
;;          saved the buffer.
;;
;;  Command prompt
;;
;;      The help key is on `?', press it to get summary of command while
;;      you're in minibuffer prompt. The command prompt in minibuffer looks
;;      like this for RCS version controlled file.
;;
;;          > cd /users/foo/dir1/dir2; rcsdiff -c -r1.21 test.txt
;;
;;      You can edit this command as much as you like, but please leave `cd'
;;      `XXX' alone because the minibuffer commands expect it it be
;;      present. The hotkey command won't work without it.
;;
;;  Command prompt: rcsdiff and diff toggle
;;
;;      To conveniently construct diff command against another file, say
;;      test2.txt, you can hit key `C-z' to chage the prompt immediately to
;;
;;          > cd /users/foo/dir1/dir2; diff -c test.txt
;;
;;      And add the `test2.txt' to the end of line. If you want to restore
;;      the previous rcsdiff form, just hit `C-z' again. This `C-z'
;;      facility works only if the initial command was rcsdiff. There is no
;;      point of converting initial diff command to rcsdiff command though.
;;
;;  Command prompt: tab completes file name
;;
;;      While your're editing the command you can also use the TAB key to
;;      complete filename in the 'cd' command directory. If you specify any
;;      directories for the file, the directory's files are completed.
;;      That feature should allow you to get filenames into the prompt
;;      easily.
;;
;;  Command prompt: diffing between two RCS revisions
;;
;;      There is also more commands, like `C-r' which changes
;;
;;          > cd /users/foo/dir1/dir2; rcsdiff -c -r1.21 test.txt
;;
;;      prompt so that it has now two -r commands. You can take diffs
;;      between two versions easily with it. The `C-r' key is a toggle.
;;
;;          > cd /users/foo/dir1/dir2; rcsdiff -c -r1.21 -r1.21 test.txt
;;
;;       Case study:
;;
;;      You see a package on the net. You download it and notice that
;;      it needs some fixes. You put the original version to your
;;      private rcstree with the same version number as what the
;;      package had; say 2.2. Then you CheckOut the original, make
;;      changes, and put it back to tree with version 2.2.1.1. You
;;      dont't put it back with 2.3, because that's not your file. You
;;      made the correction to 2.2, so you must make a branch.
;;
;;      Okay. You have the original 2.2 and you have the fixed version
;;      2.2.1.1 and you want to send the diff to the author. Here is how
;;      you do it
;;
;;      o   Be on the file buffer 2.2.1.1 and call `M-x' `tinydiff-dif'
;;      o   Hit `C-r' toggle second revision (previous) and edit the line
;;          to look "-r2.2 -r2.2.1.1". You are usually comparing _old_ and
;;          new_ versions.
;;      o   Hit `C-s' to toggle between `-u' or `-c'. You normally want
;;          to send `-u' gnu unified diff, because it is more readable.
;;          Provided that the receiver has gnu patch to understand it.
;;      o   Hit `C-f' to add option `tinydiff--cl-user-option' which by
;;          default is `-kk'. From the rcsdiff(1) man pages you will
;;          see that it roughly means: "When you take diff between versions,
;;          ignore the rcs tag differencies". Confused? It means that
;;          the keywords that changed, like version, author, log ..
;;          when you deposited 2.2 and 2.2.1.1 are ignored.
;;
;;      And hit enter. Then you get clean diff that you can send to author.
;;      And when he responds back or sends you new version, say 2.5,
;;      you repeat the whole process again if you intend to make more
;;      changes put original 2.5 on ice and make branch 2.5.1.1 for your
;;      changes)
;;
;;  Command prompt: autosave and backup file diff
;;
;;      Other helpful commands insert he #autosaved# and backup~ filenames
;;      into the current point. Remember to put the # or ~ file to the left
;;      and not to the right. you do want to diff current file against the
;;      saved one; right? The first one is original prompt. That second is
;;      after `C-r' and latter after `C-v'
;;
;;          > cd /users/foo/dir1/dir2; diff -c test.txt
;;                                            * point here
;;
;;          > cd /users/foo/dir1/dir2; diff -c #test.txt# test.txt
;;          > cd /users/foo/dir1/dir2; diff -c ~/backup/test.txt~ test.txt
;;
;;      Notice that your backup file may not reside int he same directory.
;;      The backupfilename is returned by function `make-backup-file'.
;;
;;  Generated diff: the Prereq tag
;;
;;      It is important that when you send diff, it is diff between two
;;      rcs versions if possible (if you're author of program). In those
;;      cases where revision information can be found, the diff data
;;      is preceeded with this line:
;;
;;          Prereq: N.NN        e.g. Prereq: 1.76
;;
;;      If the receiving end has GNU patch, the patch program first checks
;;      if the version that person has is exactly N.NN and aborts if
;;      he had some other version. This prevent applying diffs that
;;      are meant to other versions. Regular Unix *patch* program
;;      does not read the *Prereq:* tag.
;;
;;  Patching
;;
;;      There is also included little patching function.
;;
;;          M-x tinydiff-patch          non verbose
;;          C-u M-x tinydiff-patch      verbose
;;
;;      For elisp (.el) files the `load-path' is automatically searched
;;      for possible destination of the patch. You can set variable
;;
;;          tinydiff--patch-list
;;
;;      To match files and their associated patch directories if you
;;      receive patches for other files regularly. This function is most
;;      useful for RCS diffs, because they can be easily detected and the
;;      file information is also included in the diff.
;;
;;  Patch: general notes
;;
;;      Note: normally when `patch' program is called it always makes
;;      backup with the suffix .orig. So if you have applied a patch,
;;      then there is two file in the directory.
;;
;;          FILE.txt        -- patched file
;;          FILE.txt.orig   -- original file, before the patch
;;
;;      It also creates rejections file if all dind't go as planned.
;;
;;          FILE.txt.rej
;;
;;  Patch: success or failure
;;
;;      When the patch has been applied, This package checks if all went
;;      well. If rejection file was created; then the patch process's
;;      output is shown and the rejection file is loaded so that you can
;;      see what possibly went wrong and if you should be concerned.
;;
;;      If you get this rejection file, then there propably is potential
;;      trouble. Please contact the sender of patch immediately and tell
;;      about your troubles. There are few common reasons why patch failure
;;      happened.
;;
;;      o   Sender forgot `-kk' switch when he run rcsdiff to the file
;;          that was not owned by him (See RCS for details about `-kk')
;;      o   Too few context, Sender should try increasing context with
;;          `-C' switch (like `-C7')
;;      o   The patch were modified during email transit. Ask
;;          to send the patch with some encoded format: uuencode, base64,
;;          PGP encrypted or PGP base64 signed (clearsig off) format.
;;
;;  Patch: what happens after success
;;
;;      When the patch succeeds, there is a bit special handling for Emacs
;;      elisp packages. Say we recieve correction to the following module
;;      and you have it loaded in emacs: (feature 'foo) returns true.
;;
;;          foo.el
;;
;;      After patch is applied, you're asked if you want to reload the
;;      new release of *foo* module (just patched). You should answer
;;      `Yes' to get the newest one running in your Emacs immediately.
;;
;;  Patch: after success, returning to original version
;;
;;      If the patched version, which is usually new version of the progrmam
;;      doesn't work as it is supposed to, you can go back to the original
;;      version by appluing the same patch again. You should report what
;;      problems you had to the maintainer and inform that you wnet back
;;      to previous version.
;;
;;      *IMPORTANT* If you did get the rejection file, you can't use that
;;      patch to go back to original!! See next chapter how to go to
;;      original version in that case
;;
;;  Patch: rejection file created -- what to do?
;;
;;      If you want to go back to original version, apply the same diff
;;      again; this reverses just applied patch. Just call `M-x'
;;      `tinydiff-patch' in the buffer where you have the diff.
;;
;;      When you do that, the function detects that there is already a
;;      .orig file and prompts you to choose an appropriate action.
;;      Here is the explanation what they do and what should you choose
;;
;;       Command _o_
;;
;;      Go back to (o)riginal. This copies the FILE.txt.orig over the
;;      FILE.txt and deletes FILE.txt.orig and doesn't do _anything_
;;      else (stops the patching process). You're back to starting
;;      point as if you never patched anything.
;;
;;       Command _r_
;;
;;      (R)etry means that the FILE.txt.orig is copied over FILE.txt and the
;;      pach is tried again for FILE.txt. You may have asked the author to
;;      send more context with using the -C10 switch and after you received
;;      this new patch you want to try if it now goes ok. The FILE.txt.orig
;;      still remains as a backup
;;
;;       Command _g_
;;
;;      (G)o says that we should apply the diff again to FILE.txt. Do this
;;      only if you did not get rejections last time. The intention
;;      is that you apply the patch again, and this reverses the situation.
;;      I mean 1) you patch; you get new version 2) you patch again: you
;;      degrade to the version before patch (original file before patch)
;;
;;  Development note
;;
;;      There is `ediff.el', which is much more complete package than
;;      this is. The aim was to develop a simple but handy package for
;;      everyday diff'ing and easy package patching.
;;
;;  Bugs
;;
;;      The "f" key, which shows the function identifier in diff browse
;;      mode `tinydiff-mode', can handle buffers which are narrowed, but if the
;;      buffer is using folding.el or similar package where goto-line does
;;      not work properly, the returned message shown to user is not
;;      correct.
;;
;;      Please unfold the buffer and you get the correct result.
;;
;;  Example
;;
;;      This hook setup turns on the view mode for easy scrolling
;;      of buffer.
;;
;;          (add-hook 'tinydiff--diff-hook  'my-tinydiff-diff-hook)
;;
;;          (defun my-tinydiff-diff-hook ()
;;            "Turn on view-mode in diff buffer."
;;            ;; See tinydiff--diff-buffer.
;;            (view-mode 1))
;;
;;  Sending good bug reports
;;
;;      If you find anything funny happening in the command line prompt
;;      while you use the tdi minibuffer commands. Immediately do
;;      following.
;;
;;      o   Turn on debug: `M-x' `tinydiff-debug-toggle'
;;      o   Turn on emacs debug: (setq debug-on-error t)
;;      o   Clear the debug buffer *tinydiff-debug* if it exists
;;      o   Start from original situation
;;      o   Do what you did and when the weird condition is met
;;          immediately go to *tinydiff-debug* buffer and save the
;;          content and send it to the maintainer.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

;;  make sure this is loaded so that the `tinydiff-mode' map can redefine
;;  keys "n" and "p"

(require 'tinylibm)

(eval-and-compile
  (ti::package-require-view)
  (defvar diff-command)        ;; Byte compiler silencer
  (defvar ediff-diff-program)  ;; Byte compiler silencer
  (autoload 'dired-get-filename   "dired"  "" t))

(ti::package-defgroup-tiny TinyDiff tinydiff-- tools
  "Take buffer diffs easily, browse diff and apply patch.
  Overview of features.

      o   Buffer based simple diff/rcsdiff/patch package.
      o   You cab diff current buffer against last saved backup copy.
      o   Give and manipulate diff command in echo-area: file name
            completion; switching between rcsdiff/diff, guess rcs version
            for buffer ...
      o   When you have diff output in buffer, turning on `tinydiff-mode'
          allows you to browse source buffer by jumping diff blocks fwd/back
          and showing the source location.
      o   Supported: normal diff, context diff, gnu diff -u, gnu diff -n
      o   Easy patching. Finds file to patch (along user defined paths)
          and applies the diff. You can receive patches by email
          and apply them with one or two keystrokes.
      o   loads patch rejection file, if patch didn't succeed 100%")

;;}}}
;;{{{ setup: hooks

;;; ......................................................... &v-hooks ...

(defcustom tinydiff--load-hook nil
  "*Hook that is run when package is loaded."
  :type  'hook
  :group 'TinyDiff)

(defcustom tinydiff--diff-hook  nil
  "*Hooks that run after successful diff run."
  :type  'hook
  :group 'TinyDiff)

(defcustom tinydiff--mode-define-keys-minibuffer-hook
  'tinydiff-mode-define-keys-minibuffer-default
  "*Function to define the keys for the minibuffer."
  :type  'hook
  :group 'TinyDiff)

(defcustom tinydiff--parse-buffer-hook  nil
  "Function called when diff buffer has been parsed. (highight)."
  :type  'hook
  :group 'TinyDiff)

;;}}}
;;{{{ setup: variables

;;; ........................................................ &v-public ...
;;; User configurable

(defcustom tinydiff--auto-mode-alist
  '(("\\.diff\\'"    . turn-on-tinydiff-mode)
    ("\\.patch\\'"   . turn-on-tinydiff-mode))
  "Items to add to `auto-mode-alist' to activate `turn-on-tinydiff-mode'."
  :type '(repeat
          (list
           (string :tag "File Regexp")
           (const turn-on-tinydiff-mode)))
  :group  'TinyDiff)

;;  We need this function in defvar; so instantiate it for compiler
;;  to use it.
;;
(eval-and-compile

  (defun tinydiff-find-program (program-list default opt seek-option)
    "Try to use GNU program. Return program name.
Input:

  PROGRAM-LIST      list of program binaries to try (gnu)
  DEFAULT           if no gnu binary found, use DEFAULT binary
  OPT               additional option for binary
  SEEK-OPTION       When GNU product, it must know this option."
    (let ((exec-path exec-path)
	  (ret       default)
	  gnu
	  path)
      ;;  Give precedence to GNU diff programs
      (dolist (path '("/opt/local/bin/" "/usr/local/bin/"))
        (if (file-directory-p path)
            (push path exec-path )))
      (if path
          (setq path t))               ;XEmacs ByteCom silencer, no-op
      (with-temp-buffer
        ;;   Select gnu if possible
        (dolist (prg program-list)
          (message "TinyDiff: Please wait. Searching for binary `%s'" prg)
          (when (setq path (executable-find prg))
            (call-process prg
                          nil
                          (current-buffer)
                          nil
                          seek-option)
            (when (ti::re-search-check seek-option)
              (setq gnu path)
              (return))))
        (if gnu
            (setq ret gnu)
          (message "TinyDiff: Hm, no GNU %s, but using it anyway" default)
          (sit-for 1))
        (set-buffer-modified-p nil)
        ret)))) ;; eval-end

;;  Different users may want to set the keys differently.
;;  You could say
;;
;;  (setq tinydiff--diff-program (progn (my-diff-program-select)))
;;
;;  Which would return appropriate diff program: maybe you want to use
;;  GNU diff for some files and normal diff other times.
;;  GNU diff offers line exlude options that you may want
;;  to set for RCS files.

(defcustom tinydiff--diff-program
  (cond
   ((and (boundp 'diff-command)
         diff-command)
    diff-command)
   ((and (boundp 'ediff-diff-program)
         ediff-diff-program)
    ediff-diff-program)
   ((ti::os-check-gnu-support-p)
    "diff")
   (t
    (or (tinydiff-find-program '("gdiff" "diff") "diff" nil "--help")
        "diff")))
  "*Program to generate diff.
It should print 'filename: FILE.XXX' tag which is read by function
`tinydiff-get-buffer-name'. The variable can contain a Lisp expression
which returns program name."
  :type  '(string :tag "Shell command")
  :group 'TinyDiff)

(defcustom tinydiff--rcsdiff-program "rcsdiff"
  "*Shell program to print RCS diff.
It should output the 'filename: FILE' which is read by `tinydiff-get-buffer-name'.
This variable is evaled to get the program name."
  :type  '(string :tag "Shell program")
  :group 'TinyDiff)

(defcustom tinydiff--cvsdiff-program "cvs diff"
  "*Shell command to print CVS diff."
  :type  '(string :tag "Command")
  :group 'TinyDiff)

;; The diff-switches is defined at least in vc.el

(defcustom tinydiff--diff-option
  '(or (and (boundp 'diff-switches)
            (stringp diff-switches)
            diff-switches)
       (and (ti::os-check-gnu-support-p)
            "-u")
       "-c")
  "*Diff options as STRING.
This variable is evaled to get the options, so it can contain Lisp
FORM that returns option string."

  :type  '(string :tag "Options")
  :group 'TinyDiff)

(defcustom tinydiff--cl-user-option  "-kk"
  "The option inserted or removed when user presses C - s in command line.
The default option -kk is only meaningful on rcsdiff command where
it excludes the rcs tags from diff."
  :type  'string
  :group 'TinyDiff)

(defcustom tinydiff--diff-tmp-file
  (or (let ((temp (or (getenv "TEMPDIR")
                      (getenv "TMP")))
            (file "tinydiff.diff"))
        (dolist (dir (list
                      "~/tmp"
                      "~/temp"
                      "/tmp"
                      temp ;; this may be nil
                      "c:/tmp"
                      "c:/temp"
                      "c:/winnt/tmp"
                      "c:/windows/temp"))
          (when (and (stringp dir)
                     (file-directory-p dir))
            (return (concat (file-name-as-directory dir) file)))))
      (error "TinyDiff: Please set tinydiff--diff-tmp-file"))
  "*Temporary file where the diff is stored for patching."
  :type  'file
  :group 'TinyDiff)

(defcustom tinydiff--patch-program
  (if (ti::os-check-gnu-support-p)
      ;;  We Know this is GNU patch, do not search alternatives
      ;; gnu patch : -t, --batch
      ;; similar to -f, in that it suppresses questions,
      ;; skip patches for which a file to patch can't be found
      ;;
      ;; Note: OLD patch command doesn not know -t switch!
      "patch -t -N -F 3"
    (tinydiff-find-program
     '("gpatch" "patch")
     "patch"
     "-t -N -F 3"
     "--help"))
  "*Patch command and its options.
This variable is evaluated to get the program name and switches."
  :type  '(string :tag "shell command")
  :group 'TinyDiff)

(defcustom tinydiff--patch-list
  '(( "[.]el$"  load-path)
    ( "."       '("~/txt" "~/elisp")))
  "*List of item that control how patching is applied.
The list form is:

   '((REGEXP EVAL-FORM) (REGEXP EVAL-FORM) ..)

Where REGEXP is tried against the filename that is found from the patch
itself. EVAL-FORM can be any form that return list of pathnames that can
be searched for the filename. The first file that is found from the path
is used."
  :type  '(repeat
           (list
            (string :tag "Regexp")
            directory))
  :group 'TinyDiff)

(defcustom tinydiff--font-lock-keywords
  '(

    ;; RCS diff

    ("RCS file: +\\(.*\\)"           1 'highlight)
    ("retrieving revision +\\(.*\\)" 1 'highlight)

    ;;  Lisp: "defun NAME" etc.

    ("(def[^(\n]+"                  0 font-lock-reference-face)
    ("(interactive.*)"              0 font-lock-type-face)

    ;;  Lisp: some tokens

    ("(\\(let\\|cond\\|when\\|unless\\|save-.*\\|with-.*\\)"
     1 font-lock-keyword-face))
  "Font lock keywords."
  :type 'sexp
  :group  'TinyDiff)

;;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . misc . .

(defcustom tinydiff--register-function-name  ?f
  "*Register used to store the function name.
Only used if `tinydiff--function-name-handle-function' is set to
'tinydiff-function-name-store."
  :type  'character
  :group 'TinyDiff)

;;  The diff data is inserted into register automatically, because
;;  many time the diff data is needed elswhere.

(defcustom tinydiff--register-diff ?d
  "*Register name where diff is inserted.
If this variable is nil, then no data is inserted into register.

Variable is evaled to get the register name."
  :type  'character
  :group 'TinyDiff)

;;; ......................................................... &v-funcs ...

(defcustom tinydiff--mail-buffer-function  'ti::mail-get-buffer
  "Return some mail buffer for function `tinydiff-mime-compose'.
Default value is `ti::mail-get-buffer'."
  :type 'function
  :group 'TinyDiff)

(defcustom tinydiff--find-ref-function 'beginning-of-defun
  "*Elisp Function to find underlying code's function name around point.
The cursor is positioned in the source buffer and on the referenced
line before calling with no arguments. Function should move the point
in the line below where the associated reference is located.

If no reference is found, function _must_ call 'error'."
  :type 'function
  :group 'TinyDiff)

;;  - It's great to paste the function name into buffer.
;;  - C-x g REG put the name into current buffer...

(defcustom tinydiff--function-name-handle-function
    'tinydiff-function-name-store
  "*Function which find the code's function name string.

Input args to function:

  POINT where the `tinydiff--find-ref-function' positioned the defun.
  This function should store the function name into register
  `tinydiff--register-function-name'."
  :type  'function
  :group 'TinyDiff)

(defcustom tinydiff--source-buffer-function 'tinydiff-get-buffer-name
  "*Function which return filename for the diff buffer.
You shouldn't touch this function unless you're coping with very
strange diff format. Default function is `tinydiff-get-buffer-name'."
  :type  'function
  :group 'TinyDiff)

;;}}}
;;{{{ setup: private

;;; ....................................................... &v-private ...

(defvar tinydiff--patch-global-option nil
  "Path options in effect. See `tinydiff-patch-set-option'.")

(defvar tinydiff--patch-reject-buffer "*tinydiff-patch-rejects"
  "Buffer where to display rejected patch parts.")

(defvar tinydiff--diff-source-buffer nil
  "Private. Source buffer for diff. See `tinydiff--source-buffer-function'.")
(make-variable-buffer-local 'tinydiff--diff-source-buffer)

(defvar tinydiff--last-data nil
  "Private. Data storage eg in `tinydiff-minibuffer--change-diff-command'.")

(defvar tinydiff--version-list  nil
  "All Version for file. Updated when diff command is being run.")

(defvar tinydiff--version-branch-list  nil
  "Branches. Updated when diff command is being run.")

(defvar tinydiff--diff-tmp-buffer "*tinydiff-tmp*"
  "Temporary work buffer, patch shell results.")

(defvar tinydiff--patch-tmp-buffer " *tinydiff-tmp-patch*"
  "Temporary work buffer, patch.")

(defvar tinydiff--package-exist-tinymy  (locate-library  "tinymy.el")
  "Private. Has load path for package tinymy.el if it exist.
It has some functions we may use from there.")

(defvar tinydiff--diff-buffer "*diff*"
  "Buffer where diff is inserted.")

(defvar tinydiff--patch-to-file nil
  "Name of the file to patch.
This variable is made local to current patch/diff buffer.")

(defvar tinydiff--patch-hunk-count nil
  "Counter how many patch hunks hvae been applied
This variable is made local to current patch/diff buffer.")

;;}}}
;;{{{ code: minor mode definition

;;; ............................................................ &mode ...

(defvar tinydiff--minibuffer-map nil
  "Minibuffer key map when asked for the right diff command.")

;;;###autoload (autoload 'tinydiff-mode          "tinydiff" "" t)
;;;###autoload (autoload 'turn-on-tinydiff-mode  "tinydiff" "" t)
;;;###autoload (autoload 'turn-off-tinydiff-mode "tinydiff" "" t)
;;;###autoload (autoload 'tinydiff-commentary    "tinydiff" "" t)

(eval-and-compile
  (ti::macrof-minor-mode-wizard
   "tinydiff-" " Tdi" nil " Tdiff" 'TinyDiff "tinydiff--" ;1-6

   "Diff browsing minor mode.

Defined keys:

\\{tinydiff--mode-map}"

   "Diff minor mode"

   nil

   "Diff browsing mode"

   (list
    tinydiff--mode-easymenu-name
    ["goto current point"                tinydiff-goto-kbd               t]
    ["block forward"                     tinydiff-goto-next              t]
    ["block forward, no update"          tinydiff-goto-next-no-update    t]
    ["block backward"                    tinydiff-goto-prev              t]
    ["block backward, no update"         tinydiff-goto-prev-no-update    t]
    ["Set patch option"                  tinydiff-patch-set-option       t]
    "----"
    ["Parse buffer"                      tinydiff-parse-buffer           t]
    ["Set source buffer for diff"        tinydiff-set-source-buffer      t]
    ["Show function name"                tinydiff-show-function-name     t]
    "----"
    ;; ["Keyboard menu"                     tinydiff-menu-main           t]
    ["Package version"                   tinydiff-version                t]
    ["Package commentary"                tinydiff-commentary             t]
    ["Mode help"                         tinydiff-mode-help              t]
    ["Mode off"                          tinydiff-mode                   t])

   (progn

     ;;  I first thought to put goto line into " " or "\C-m"
     ;;  That is: SPACE or RETURN, but later realized that they were
     ;;  used in view-mode to scroll buffer up-down.
     ;;
     ;;  Eg. in my setup whenever I turn on the read-only
     ;;  with C-x C-q it also automatically turns on view-mode for
     ;;  easy scrolling...
     ;;

     ;;  This happens to be unsifted in HP-UX, a top-leftmost button.
     ;;  Select something that suit you more...

     (define-key   root-map "\C-m"     'tinydiff-goto-kbd)

     (define-key   root-map "e"        'tinydiff-parse-buffer)
     (define-key   root-map "!"        'tinydiff-set-source-buffer)
     (define-key   root-map "n"        'tinydiff-goto-next)
     (define-key   root-map "p"        'tinydiff-goto-prev)

     ;;  These are borrewd from unix more(1) and less(1)

     (define-key   root-map "y"        'tinydiff-goto-prev-no-update)
     (define-key   root-map "b"        'tinydiff-goto-next-no-update)

     ;;  But perhaps user feels more comfortable with these.

     (define-key   root-map "P"        'tinydiff-goto-prev-no-update)
     (define-key   root-map "B"        'tinydiff-goto-next-no-update)

     (define-key   root-map "f"        'tinydiff-show-function-name)

     (define-key   root-map "W"        'tinydiff-write-file)
     (define-key   root-map "M"        'tinydiff-mime-compose)
     (define-key   root-map "O"        'tinydiff-patch-set-option)

     (define-key   root-map "-k"       'tinydiff-block-kill)
     (define-key   root-map "-\C-?"    'tinydiff-block-kill) ;; backspace
     (define-key   root-map "--"       'tinydiff-block-apply-patch)

     (define-key map  "?"  'tinydiff-mode-help)
     (define-key map  "Hm" 'tinydiff-mode-help)
     (define-key map  "Hc" 'tinydiff-commentary)
     (define-key map  "Hv" 'tinydiff-version)

     (if (ti::emacs-p)
         (define-key   root-map [mouse-2]  'tinydiff-goto-mouse)
       (define-key   root-map [(button2)]  'tinydiff-goto-mouse)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-install (&optional uninstall)
  "Install TinyDiff package, or optionally UNINSTALL.
A .diff or .patch file invokes `tinydiff-mode' in `automode-alist'."
  (interactive "P")
  (cond
   (uninstall
    (ti::assoc-replace-maybe-add
     'auto-mode-alist tinydiff--auto-mode-alist 'remove))
   (t
    (ti::assoc-replace-maybe-add
     'auto-mode-alist tinydiff--auto-mode-alist)
    (if (interactive-p)
        (message "TinyDiff installed")))))

;;}}}
;;{{{ code: keymap

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-mode-define-keys-minibuffer-default ()
  "This function defines some extra bindings to minibuffer.
Eg. TAB that completes current filename."
  (setq tinydiff--minibuffer-map (copy-keymap minibuffer-local-map))
  ;;
  ;;  Here we define nice tab filename completion inside minibuffer
  ;;  This may be superflous, but what the heck :-)
  ;;
  (define-key tinydiff--minibuffer-map "\t"
    'tinydiff-minibuffer--complete-filename)
  (define-key tinydiff--minibuffer-map [(kp-tab)]
    'tinydiff-minibuffer--complete-filename)
  ;;
  ;;  More command line handling
  ;;
  ;;  There is no logic in naming the key settings other than
  ;;  simple rule: they must me as lower left as possible
  ;;  to be reached quickly.
  ;;
  ;;  You won't need any of these in echo area prompt
  ;;
  (define-key tinydiff--minibuffer-map "\C-z"
    'tinydiff-minibuffer--change-diff-command)
  (define-key tinydiff--minibuffer-map "\C-r"
    'tinydiff-minibuffer--rev-add-command)
  (define-key tinydiff--minibuffer-map "\C-c"
    'tinydiff-minibuffer--insert-file-autosave)
  (define-key tinydiff--minibuffer-map "\C-v"
    'tinydiff-minibuffer--insert-file-backup)
  (define-key tinydiff--minibuffer-map "\C-s"
    'tinydiff-minibuffer--toggle-diff-type)
  (define-key tinydiff--minibuffer-map "\C-f"
    'tinydiff-minibuffer--user-option)
  (define-key tinydiff--minibuffer-map "\C-p"
    'tinydiff-minibuffer--insert-previous-word)
  (define-key tinydiff--minibuffer-map "?"
    'tinydiff-minibuffer--minibuffer-help))

;;}}}
;;{{{ code: misc

;;; ............................................................ &misc ...

;;;###autoload (autoload 'tinydiff-debug-toggle "tinydiff" "" t)

(eval-and-compile (ti::macrof-debug-standard "tinydiff" "--"))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydiff-kill-revision-list ()
  "Deletes private version lists."
  (setq tinydiff--version-list nil
        tinydiff--version-branch-list nil))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-splice-command  (string)
  "Splice off directory from string and return '(DIR CMD REST)"
  (when (string-match
         (concat
          "cd[ \t]+\\([^;]+\\);"    ;; dir
          "[ \t]*\\([^ \t\r\n]+\\)" ;; cmd
          "[ \t]+\\(.+\\)")         ;; rest
         string)
    (list
     ;;  Delete trailing whitespace
     (replace-regexp-in-string "[ \t\r\n]+$" ""
                               (match-string 1 string))
     (match-string 2 string)
     (match-string 3 string))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-shell-command (cmd buffer)
  "Run CMD and output to BUFFER.
The passed CMD must be in the format:

   cd DIRECTORY; BINARY option option option ...."
  (let ((fid "tinydiff-shell-command"))
    (multiple-value-bind (dir cmd rest)
        (tinydiff-splice-command cmd)
      (when (or (not dir)
                (not (file-directory-p dir)))
        (error "Tinydiff: No directory found `%s'" dir))
      (let ((default-directory (file-name-as-directory dir)))
        (tinydiff-debug fid
                        'default-directory default-directory
                        'cmd    cmd
                        'rest   rest
                        'buffer buffer)
        (setq buffer (get-buffer-create buffer))
        (let* (args
               (rargs (reverse (split-string rest)))
               (file2 (expand-file-name (pop rargs)))
               (file1 (expand-file-name (pop rargs))))
          (push file1 rargs)
          (push file2 rargs)
          (setq args (reverse rargs))
          (tinydiff-debug fid 'CMD cmd 'FILE1 file1 'FILE2 file2)
          (apply 'call-process
                 cmd
                 nil
                 buffer
                 nil
                 args))
        (when (or (null buffer)
                  (null (get-buffer buffer))
                  (not (buffer-live-p buffer)))
          (error "TinyDiff: Shell dind't return results [ %s ]" cmd))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-update-revision-list (file &optional version)
  "Read all revision numbers for FILE starting from VERSION."
  (let* ((fid   "tinydiff-update-revision-list: ")
         (ver   (or version "1.1"))
         (dots  (count-char-in-string ?. ver)))
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (when (null tinydiff--version-list)
      (tinydiff-debug fid ver dots)
      (setq tinydiff--version-list  (ti::vc-rcs-all-versions file)))
    (when (or (null tinydiff--version-branch-list)
              ;; Chck that there is right brach list
              ;; "1.1"  -- "1.1.1.1" ?
              (not (eq dots
                       (count-char-in-string
                        ?.
                        (car tinydiff--version-branch-list)))))
      (setq tinydiff--version-branch-list
            (ti::vc-rcs-get-all-branches
             ver tinydiff--version-list))
      (tinydiff-debug fid "ver" version))
    tinydiff--version-list))

;;; ----------------------------------------------------------------------
;;; - Doing the version getting is SLOW with lisp. Use some external
;;;   shell program to do the job for you. It is MUCH quicker.
;;;
(defun tinydiff-rcs-diff-between-versions (dir file)
  "Return VC diff command that diffs current version and previous version.
DIR and FILE is passed to function."
  (interactive)
  (let ((ver (ti::vc-rcs-buffer-version))
	v-list
	ret
	prev)
    (if (null ver)
        (error "TinyDiff: Cannot find version number."))
    (setq v-list   (tinydiff-update-revision-list (concat dir file)))
    (setq prev     (ti::vc-rcs-previous-version ver v-list))
    (if (null prev)
        (error "TinyDiff: Cannot find previous version number."))
    (setq ret
          (format "cd %s; %s -r%s -r%s %s %s "
                  dir
                  (if (ti::vc-rcs-file-exists-p file)
                      tinydiff--rcsdiff-program
                    tinydiff--cvsdiff-program)
                  prev ver
                  (or (eval tinydiff--diff-option)
                      "")
                  file))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydiff-source ()
  "Return source buffer of diff."
  (if (and tinydiff--diff-source-buffer
           (buffer-live-p (get-buffer tinydiff--diff-source-buffer)))
      tinydiff--diff-source-buffer
    (setq tinydiff--diff-source-buffer
          (funcall tinydiff--source-buffer-function))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-set-source-buffer (buffer)
  "Set source BUFFER for current diff."
  (interactive "bSource buffer: ")
  (if (not (get-buffer buffer))
      (error "TinyDiff: Buffer does not exist")
    (setq tinydiff--diff-source-buffer buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-function-name-store (line)
  "Store found function name from LINE into `tinydiff--register-function-name'.
Currently works well only Lisp functions."
  (let ((reg  tinydiff--register-function-name)
	(mode (symbol-name major-mode))
	txt)
    (when (string-match "lisp" mode)
      ;;  try to extract symbol-name
      ;;  see tinydiff-get-function-name, because point sits on DEFUN already
      ;;  and the ti::buffer-defun-function-name searches it again..
      (forward-line 1))
    (setq txt (ti::buffer-defun-function-name))
    (when reg
      (if txt
          (set-register reg txt)
        ;; empty it
        (set-register reg "")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-get-function-name (buffer line)
  "Return function or variable name at current point.
Switches to BUFFER and go to LINE and calls `beginning-of-defun'"
  (let ((func          tinydiff--function-name-handle-function)
	(find-func     tinydiff--find-ref-function)
	(max-leap      100)            ;lines
	point
	ret)
    (with-current-buffer buffer
      (ti::widen-safe
        (ti::goto-line line)
        (ignore-errors
          (setq point (point))
          (funcall find-func)           ;exit , if error generated
          ;;  Let's be little intelligent
          (if (< (count-lines (point) point) max-leap)
              ;;  Okay, we believe that function was found
              (setq ret (ti::read-current-line))
            ;; Can't be that far away... reset the pointer
            (goto-char point))
          (if func
              (setq ret (funcall func (point))))) ;; ignore-errors
        ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-show-function-name (&rest args)
  "Show possible function name in mode line on the current diff point. ARGS."
  (interactive)
  (let ((line   (tinydiff-get-line-number))
	(buffer (tinydiff-source))
	desc)
    (cond
     ((not (and line buffer))
      (message "Tinydiff: Sorry, No line and buffer info."))
     (t
      (setq desc  (tinydiff-get-function-name buffer line))
      (if (stringp desc)
          ;;  Moving mouse wipes this away...
          (message desc)
        (message "Tinydiff: Can't find reference."))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-turn-on-view-mode ()
  "Turn on view mode and read-only status."
  (view-mode 1)
  (unless buffer-read-only
    (setq buffer-read-only t)))

;;}}}
;;{{{ code: command line

;;; ..................................................... &commandLine ...

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--replace-text (beg end text)
  "Command line. Replace region between BEG and END with TEXT."
  (ti::save-line-column-macro
      nil nil
    (delete-region beg end)
    (goto-char beg)
    (insert text)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--read-revision ()
  "Command line. Read revision.

Return:
 (nbr . pos)    ,where pos is point at line.
 nil"
  (interactive)
  (let ((fid "tinydiff-minibuffer--read-revision:")
	nbr
	pos)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (save-excursion
      ;;  a) in the top of REV number
      ;;  b) pick REV forward
      (or (eq 0 (skip-chars-backward "0-9."))
          (eq 0 (skip-chars-forward "^0-9.")))
      ;; cd /users/foo/elisp/; rcsdiff -c -r1.25 -r1.8 tinydiff.el
      (cond
       ((setq nbr (ti::buffer-match "[0-9.]+" 0))
        (setq pos (match-beginning 0)))
       (t
        (beginning-of-line)
        (setq nbr (ti::buffer-match ".*-[rul]\\([0-9.]+\\)" 1))
        (setq pos (match-beginning 1)))))
    (tinydiff-debug fid "col"(current-column) "nbr" nbr "pos" pos )
    (if nbr
        (cons nbr pos))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--directory  ()
  "Return directory name from 'cd' command."
  (save-excursion
    (beginning-of-line)
    (ti::buffer-match "cd +\\([^;]+\\)" 1)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--read-rcs-file-name (&optional line or-diff-name)
  "Read RCS filename from LINE.
If OR-DIFF-NAME is non-nil, look for 'diff' command instead."
  (let* ((fid       "tinydiff-minibuffer--read-rcs-file-name:")
         (line      (or line
			(ti::read-current-line)))
         (rcsdiff   tinydiff--rcsdiff-program)
         (diff      tinydiff--diff-program)
         (re        (concat
                     "cd[ \t]+\\([^;]+\\);[ \t]*"
                     (if or-diff-name
			 diff
		       rcsdiff)
                     ".* \\([^ \t]+\\)"))
         dir
         file
         ret)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    ;;   The directory name is gotten after 'cd' command
    (tinydiff-debug fid line re)
    (when (and (string-match re line)
               (setq dir  (match-string 1 line))
               (setq file (match-string 2 line)))
      (setq ret (concat dir file)))
    ret))

;;; ----------------------------------------------------------------------
;;; Don't ask: This function should be rewritten someday, someday...
;;;
(defun tinydiff-minibuffer--rev-add-command  ()
  "Adding two -rX.x string to the command line.
This is only done if there is rcsdiff command and less the 2 -rX.x
switches

Eg.
        rcsdiff -r1.3 file.cc
-->     rcsdiff -r1.2 -r1.3 file.cc

        rcsdiff -r1.1 -r1.1 file.cc
-->     Do nothing, since there is already two -r switches."
  (interactive)
  (let ((fid       "tinydiff-minibuffer--rev-add-command:")
	(line      (ti::remove-properties (ti::read-current-line)))
	(re        (concat "^.*;[ \t]*"
			   "\\("
			   tinydiff--rcsdiff-program
			   "\\|cvs[ \t]+diff"
			   "\\|"
			   tinydiff--cvsdiff-program
			   "\\)"
			   "\\(.*\\)"))
	(i 0)
	prev-list elt
	r-list
	args
	list
	revision
	nbr
	tmp
	copy)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (tinydiff-debug "IN:\n\n" fid line re)
    (when (string-match re line)
      (setq args (match-string 1 line)
            list (split-string line)
            copy (copy-list list))
      (tinydiff-debug fid "ARGS" args "LIST" list)
      (setq args list)
      (while (setq elt (pop copy))
        (if (not (string-match "^-r" elt)) ;Find this
            (ti::nconc prev-list elt)
          ;;   Here we make list
          ;;   '((NTH-POS CAR-LIST CDR-LIST) (N CA CD) ..)
          ;;   -- all-the-elements before
          ;;   -- and the rest
          (push (list i
                      (copy-list prev-list)
                      (nthcdr i list))
                r-list)
          (tinydiff-debug fid "R-LIST>>" r-list))
        (incf  i))
      (tinydiff-debug fid "r-list NOW:" (length r-list)  r-list)
      (cond
       ((eq (length r-list) 0)          ;no -r --> add it
        ;;  looks complicated? The idea is to
        ;;  -- get last element, but only if it's NOT an option
        ;;  -- get all, but not the last element.
        (setq r-list (nreverse args))
        (setq elt (car r-list))         ;This is the added -rN.N
        (tinydiff-debug fid "ELT" elt r-list)
        (if (not (string-match "^-" elt))
            (setq args (reverse (cdr (reverse args))))
          (setq elt nil))               ;it was an option
        (ti::nconc args "-r" )
        (if elt                         ;file name set ?
            (ti::nconc args elt )))
       ((eq (length r-list) 2)          ;-r -r  remove first
        (setq elt  (car r-list))
        (setq args (nth 2 elt))         ;Revision and rest of the args
        (setq args                      ;car-list to the beginning
              (reverse (union (reverse (nth 1 elt)) args))))
       ((eq (length r-list) 1)          ;Only one -r
        (setq elt (car r-list))
        (setq args (nth 2 elt))         ;Revision and rest of the args
        ;; elt = (POS LIST-UNTIL-R LIST-INClUDING-R-AND-AFTER)
        (setq tmp (car (nth 2 elt))              ;copy the -rN.N
              i   (count-char-in-string ?. tmp)) ;; How many dots?
        (tinydiff-debug fid "1>" elt "ARGS" args "I=" i "TMP" tmp)
        ;; ........................................... Change revision ...
        (when (string-match "\\.\\([0-9]+\\)$" tmp)
          (setq revision (string-to-number (match-string 1 tmp))
                nbr      (string-to-number
                          (ti::string-right (match-string 1 tmp) 1)))
          (tinydiff-debug fid "1>REV"   revision "last NBR"  nbr)
          (cond
           ((or (eq i 1)                ; Like 2.2, one dot
                (and (> i 1)          ; Can't make 1.1.1.1 --> 1.1.1.0
                     (> nbr 1)))
            ;;  2.2 --> 2.1
            (setq revision (1- revision))
            (setq tmp (ti::replace-match 1 (number-to-string revision) tmp))
            (tinydiff-debug fid "1>CHANGED" revision tmp))
           ((and (> i 1)
                 (= nbr 1))
            ;;   Remove last 1.1.N.N --> 1.1
            (if (string-match "\\(.*\\)\\.[0-9]+\\.[0-9]+$" tmp)
                (setq tmp (match-string 1 tmp)))
            (tinydiff-debug fid "2>CHANGED" tmp))))
        (push tmp args)                 ;--> '(-rN.N -rN.N FILE)
        (setq args                      ;car-list to the beginning
              (reverse (union (reverse (nth 1 elt)) args)))))
      (setq args (ti::list-to-string args))
      (tinydiff-debug fid "args" args  )
      (delete-region (line-beginning-position) (line-end-position))
      (insert args))
    (end-of-line)
    ;;  Go after last revision number, so that user can change it easily
    (beginning-of-line)
    (if (re-search-forward "-r" nil t)
        (skip-chars-forward "^ \t")
      (end-of-line))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--toggle-diff-type  ()
  "Toggle -c context or -u unified diff option in command line."
  (interactive)
  (let ((line (ti::remove-properties (ti::read-current-line)))
	ret)
    (cond
     ((string-match "^\\(.+ -[^ \t]*\\)c\\(.+\\)" line)
      (setq ret (format "%su%s"
                        (match-string 1 line)
                        (match-string 2 line))))
     ((string-match "^\\(.+ -[^ \t]*\\)u\\(.+\\)" line)
      (setq ret (format "%sc%s"
                        (match-string 1 line)
                        (match-string 2 line)))))
    (when ret
      (beginning-of-line)
      (kill-line)
      (insert ret)
      ;; Preserve approx point.
      (goto-char (min (point-max) (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--user-option  ()
  "Add or remove tinydiff--cl-user-option from the line."
  (interactive)
  (let ((line      (ti::remove-properties (ti::read-current-line)))
	(rcsdiff   tinydiff--rcsdiff-program)
	(diff      tinydiff--diff-program)
	(opt       tinydiff--cl-user-option)
	ret)
    (when (not (ti::nil-p opt))
      (cond
       ((string-match (regexp-quote opt) line)
        (setq ret (ti::replace-match 0 "" line)))

       ((or (string-match (concat rcsdiff "\\( +\\)") line)
            (string-match (concat diff "\\( +\\)") line))
        (setq ret (ti::replace-match 1 (concat " " opt " ") line)))))
    (when ret
      (beginning-of-line) (kill-line)
      (insert ret))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--change-diff-command  ()
  "Change diff command in minibuffer."
  (interactive)
  (let ((fid      "tinydiff-minibuffer--change-diff-command: ")
	(rcsdiff  tinydiff--rcsdiff-program)
	(diff     tinydiff--diff-program)
	word
	line
	list
	done)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (tinydiff-debug fid "in")
    (save-excursion
      (beginning-of-line)
      (cond
       ;;  Make sure it's our command.
       ((and (re-search-forward "; *" nil t)
             (setq word (ti::buffer-read-word)))
        (setq line (buffer-substring-no-properties
                    (point) (line-end-position)))
        (setq list (split-string line))
        (tinydiff-debug fid
                        "\n\nLINE " line "\n"
                        "LIST " list "\n"
                        "1 string= word rcsdiff " word rcsdiff "\n"
                        "2 string= word diff "    word diff "\n"
                        "2 tinydiff--last-data " tinydiff--last-data "\n"
                        "3 " (and (string-match word diff)
                                  list
                                  (not (stringp tinydiff--last-data))))
        (cond
         ;; ................................................... case-1 ...
         ((or (string= word rcsdiff)
              (string= word "cvs"))
          (tinydiff-debug fid "rcsdiff INPUT:" list)
          (setq tinydiff--last-data line)
          ;;  Remove some elements from list
          (setq list (ti::list-find
                      list
                      (concat "^-r\\|" rcsdiff "\\|cvs\\|diff" )
                      (function
                       (lambda (arg elt)
                         (not (string-match arg elt))))
                      'all))
          (tinydiff-debug fid "1 FILTERED:" list)
          (kill-line)
          (insert  diff " " (ti::list-to-string list))
          (setq done t))
         ;; ................................................... case-2 ...
         ;; Did the last command have the same filename ? --> if not
         ;; then we cannot use C-z
         ((and (string-match word diff)
               list
               (stringp tinydiff--last-data)
               (ti::string-match-case
                (regexp-quote (nth (1- (length list)) list) )
                tinydiff--last-data))
          (kill-line)
          (insert tinydiff--last-data)
          (setq done t))
         ;; ................................................... case-3 ...
         ;; (3) diff --> rcs diff thing
         ((and (string-match word diff)
               list
               (not (stringp tinydiff--last-data)))
          (kill-line)
          (insert (concat
                   rcsdiff
                   " -r "
                   (ti::list-to-string (cdr list))))
          (setq done t))))))
    (when done
      ;;  Move after the 'cd' and 'diff commands to add/change options easily
      (beginning-of-line)
      (re-search-forward "; *" nil t)
      (forward-word 2))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--sweep-unix () ;; win32
  "Change all backslashes to forward slashes."
  (let ((line (ti::remove-properties (ti::read-current-line))))
    (when (string-match "[\\]" line)
      (setq line (ti::file-name-forward-slashes line))
      (beginning-of-line) (kill-line)
      (insert line)
      ;; Preserve approx point.
      (goto-char (min (point-max) (point))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--complete-filename ()
  "Complete filename."
  (interactive)
  (tinydiff-minibuffer--sweep-unix)
  (let ((fid   "tinydiff-minibuffer--complete-filename")
	(word  (save-excursion
		 (forward-char -1)
		 (ti::buffer-read-space-word)))
	(dir   default-directory))
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (tinydiff-debug fid "BEGIN" word dir)
    (unless (ti::nil-p word)
      ;;   The directory name we get from the 'CD' prompt if the
      ;;   filename lacks dir part.
      (let ((cddir (tinydiff-minibuffer--directory)))
        (cond
         ((not (string-match "/" word))
          (setq dir cddir))
         ((string-match "^\\.\\." word) ;; relative path
          (setq dir (expand-file-name (concat
                                       (file-name-as-directory cddir)
                                       (file-name-directory word)))
                word (file-name-nondirectory word)))))
      (tinydiff-debug fid "END" word dir)
      (let ((default-directory (or dir
                                   default-directory)))
        (ti::file-complete-file-name-word word)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--insert-file-autosave  (&optional backup)
  "Insert auto-save filename into current point if it exists.
Prefix arg says to insert BACKUP filename instead."
  (interactive)
  (let ((line      (ti::read-current-line))
	(rcsdiff   tinydiff--rcsdiff-program)
	(fid       "tinydiff-minibuffer--insert-file-autosave: ") ;function id
	file
	file2
	stat)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (setq file (tinydiff-minibuffer--read-rcs-file-name line 'diff))
    (tinydiff-debug fid "arg" backup "file" file)
    (cond
     ((string-match rcsdiff line)
      (message
       (substitute-command-keys
        (concat
         "Tinydiff: Don't use rcsdiff command; Change command with "
         "\\[tinydiff-minibuffer--change-diff-command] "))))
     ((not file)
      (message "Tinydiff: Can't read file from prompt. Include 'cd'"))
     ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. . process file ..
     (t
      (cond
       (backup
        (setq file2 (make-backup-file-name file))
        (setq stat (file-exists-p file2)))
       (t
        ;;  The autosave fiel must be surrounded with '' because
        ;;  # is shell comment
        (setq file2 (format
                     "#%s#"
                     (file-name-nondirectory file)))
        (setq stat
              (file-exists-p (concat (file-name-directory file) file2)))))
      (if (null stat)
          (message "Tinydiff: Not found %s " file2 )
        (setq file2 (concat "'" file2 "'"))
        ;; put spaces around filename if needed
        (setq file2
              (concat
               (if (ti::space-p (preceding-char))
                   ""
                 " ")
               file2
               (if (ti::space-p (following-char))
                   ""
                 " ")))
        (insert file2))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--insert-file-backup  ()
  "Command line. Inset backup filename if it exists."
  (interactive)
  (tinydiff-minibuffer--insert-file-autosave 'backup))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--minibuffer-help ()
  "Show brief help."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ
     (substitute-command-keys
      "TinyDiff minibuffer command line keys\n\n \\{tinydiff--minibuffer-map}")))
  (with-current-buffer "*Help*"
    (ti::pmin)
    (forward-line 4)
    (delete-non-matching-lines "tinydiff-minibuffer-")))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-minibuffer--insert-previous-word ()
  "Insert previous word: like filename or rcs switch."
  (interactive)
  (let ((word (save-excursion
		(when (re-search-backward "[^ \t]" nil t)
		  (ti::buffer-read-space-word)))))
    (when word
      (insert word))))

;;}}}

;;{{{ code: diff type, patch

;;; ........................................................... &patch ...

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydiff-patch-get-dir-from-cmd  (cmd)
  "Return FILE from CMD."
  (when cmd
    (setq cmd (ti::string-match "cd +\\([^ \t;]+\\)" 1 cmd))
    (ti::file-make-path cmd)))

;;; ----------------------------------------------------------------------
;;;
(defsubst tinydiff-patch-get-file-from-cmd  (cmd)
  "Return FILE from CMD."
  (when cmd
    (setq cmd (ti::string-match
               (format "cd .*%s +\\([^ \t;]+\\)" tinydiff--patch-program)
               1
               cmd))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-minibuffer-cleanup ()
  "Check if we can use the patch as is, PGP change must be restored."
  (save-excursion
    ;; PGP breaks the dashed lines:
    ;; - --- 1.2.1.1  1996/06/11 11:36:03
    ;; - --- 212,219 ----
    ;;
    ;; Correct the lines back.
    (let (done)
      (ti::pmin)
      (while (re-search-forward "^\\(- \\)\\(--- [0-9]+.*\\)" nil t)
        (message "Tinydiff: Correcting patch: PGP's broken lines...")
        (setq done t)
        (ti::replace-match 1 (match-string 2)))
      (ti::pmin)
      (when (re-search-forward "^--+BEGIN +PGP +SIGNATURE" nil t)
        (message "Tinydiff: Correcting patch: removing PGP tags...")
        (setq done t)
        (delete-region (line-beginning-position) (point-max)))
      (when done
        (message "Tinydiff: Correcting patch: done.")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-get-file-name (&optional arg)
  "Try to get file name for the diff.
Optionally read line \"RCS file: xxx.el\"

ARG says which to look:

  1 pick line *** file.xx
  2 pick line --- file.xx
  3 pick line +++ file.xx      GNU unified diff.

Return list:
  '(file (type pos))
  nil"
  (let* ((stat (ti::buffer-diff-type-p))
         (re1   "^[ \t]*[*][*][*] +\\([^ \t\n]+\\)")
         (re2   "^[ \t]*--- +\\([^ \t\n]+\\)")
         (re3   "^[ \t]*[+][+][+] +\\([^ \t\n]+\\)")
         (re    re1)                    ;default
         list
         file)
    (when stat                          ;only if diff found
      (if arg                           ;set search regexp
          (cond
           ((eq 1 arg) (setq re re1))
           ((eq 2 arg) (setq re re2))
           ((eq 3 arg) (setq re re3))))

      (save-excursion
        (goto-char (cdr stat))
        (cond
         ((save-excursion
            ;;  Ignore path
            (and (or (re-search-backward
                      "RCS +file: +\\([^/]+\\),v" nil t)
                     (re-search-backward
                      "RCS +file: +.*/\\\(.*\\),v" nil t))
                 (setq file (match-string 1)))))
         ((save-excursion
            (when (re-search-backward "diff -c.*" nil t)
              (setq list (split-string (ti::read-current-line)))
              ;; get last element
              (setq file (nth (length list) list)))))
         ((save-excursion
            (when (or (re-search-backward re nil t)
                      (re-search-forward  re nil t))
              (setq file (match-string 1))))))))

    (if file                            ;pick the return values
        (list (ti::remove-properties file) stat)
      nil)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-check-if-load  (file buffer &optional flag)
  "See if we want to load FILE by looking results in BUFFER.
If file was RCS controlled and not in Emacs, ask to load it.
If file is active in Emacs ad to do `load-file' to refresh current Emacs.

Input:

  FILE      File name
  BUFFER    The patch(1) command output buffer
  FLAG      If 'hunk, this is only partial diff."
  (let* (case-fold-search               ;case sensitive matches
         (fid       "tinydiff-patch-check-if-load")
         (fbuffer   (find-buffer-visiting file))
         (modified  (if fbuffer  (ti::buffer-modified-p fbuffer)))
         (was-rcs   (ti::re-search-check "RCS/"))
         ;; Is package active in Emacs? Was patched file .el?
         el-file
         sym
         feature
         no-ask)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (and file
         (setq el-file  (ti::string-match "\\(.*\\)\\.el$" 1 file))
         (setq sym      (intern-soft (file-name-nondirectory el-file)))
         (setq feature  (featurep sym)))

    (tinydiff-debug fid "in:" file buffer el-file sym feature
                    "FBUFFER" fbuffer "MODIFIED" modified
                    "WAS-RCS" was-rcs)
    ;;  Be sure about success
    (when (with-current-buffer buffer (ti::re-search-check "[Hh]unk.*succeed"))
      (cond
       ((and fbuffer
             (not modified)
             ;;  If autorevert.el is turned on with
             ;;  M-x global-auto-revert-mode, do not ask from user
             (or (not (boundp 'global-auto-revert-mode))
                 (null (symbol-value 'global-auto-revert-mode)))
             (or (y-or-n-p "Buffer for the file exist, revert? ")
                 (progn
                   (setq no-ask t)
                   ;; Set flag and Stop case
                   nil)))
        (pop-to-buffer fbuffer)
        (revert-buffer))
       ((and fbuffer modified)
        (display-buffer buffer)
        (message "Tinydiff: file's buffer in Emacs is modified.."))
       ((and was-rcs
             (null no-ask)              ;Maybe set in 1st cond case
             (y-or-n-p (format "RCS file patched, find-file %s?" file)))
        (find-file file))
       ((and feature
             (not (eq flag 'hunk))
             (y-or-n-p (format "%s is running in your Emacs, reload it?  "
                               file)))
        (load file)
        (message
         "Tinydiff: %s reloaded. Your Emacs is now running the latest patch."
         file))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-check-failure (&optional buffer)
  "Check patch failure messages from BUFFER.
Return:
  filename      rejection file if failure happened.
  '(\"\")       some unknown failure happened; rejection file not available
                String holds matched failure condition.
  nil           Patch succeeded ok."
  (interactive)
  (let ((fid "tinydiff-patch-check-failure")
	(re  "failed.*saving +rejects +to +\\([^ \t\n]+\\)")
	(case-fold-search t)
	file)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (with-current-buffer (or buffer (current-buffer))
      (or
       ;;   1 out of 2 hunks failed--saving rejects to test.el.rej
       (setq file (ti::re-search-check re 1 nil 'get-matched))
       ;;  patch: **** this file doesn't appear to be the 1.7 version--aborting
       (and (setq file
                  (ti::re-search-check
                   "^patch:.*--aborting" 1 nil 'get-matched-text))
            (setq file (list file)))
       ;;  In SunOS it simply prints the following on success.
       ;;
       ;;       Looks like a new-style context diff.
       ;;       done
       (and (save-excursion             ;Check second line first
              (ti::pmin)             ;If we care to dig deeper then...
              (forward-line 2)
              (looking-at ".*done\\.?"))
            (save-excursion
              (let (stat1 stat2 stat3 str)
                (ti::pmin)
                (setq stat1 (looking-at ".*Looks like.*"))
                (forward-line 1)
                (setq stat2 (looking-at ".*done\\.?"))
                (forward-line 1)
                (setq str (buffer-substring-no-properties
                           (point) (point-max)))
                (setq stat3 (string-match "^[\n\r]*\\'"  str))
                (or (and stat1 stat2 stat3)
                    (setq file
                          (list
                           "TinyDiff: Hm. Can't tell if patch succeeded."))))))
       ;;  Maybe shell error has terminated the command
       ;;  "Unrecognized switch: -t"
       (and (not (ti::re-search-check "hunk.*succeed"))
            (prog1 nil
              (setq file
                    (list
                     "TinyDiff: Hm. No 'hunk succeed' message found."))))))
    (tinydiff-debug fid file)
    file))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-check-rejections  (cmd buffer)
  "After CMD, check rejections from BUFFER.
If the the patch command says in this buffer:

  1 out of 1 hunks failed--saving rejects to file.rej

Then loading the rejection file.

CMD is the original patch command used.

References:

  `tinydiff--patch-reject-buffer'"
  (let ((fid      "tinydiff-patch-check-rejections: ")
	file
	stat
	tmp
	dir
	file-load)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (tinydiff-debug fid "in:" cmd buffer)
    (setq stat (tinydiff-patch-check-failure buffer))
;;;    (pop-to-buffer (current-buffer)) (ti::d! "rejections" stat cmd)
    (cond
     ((ti::listp stat)
      (message (car stat)))
     ((stringp stat)
      (setq file stat)
      (if cmd
          (setq dir (tinydiff-patch-get-dir-from-cmd cmd))
        (setq dir default-directory))
      (setq file-load (concat dir file))
      (tinydiff-debug fid file-load)
      (cond
       ((file-exists-p file-load)
        (setq tmp (ti::temp-buffer tinydiff--patch-reject-buffer 'clear))
        (with-current-buffer tmp (insert-file-contents file-load))
        (display-buffer tmp)
        t)
       (t
        (message "TinyDiff: Can't find DIR for file...") (sit-for 1)
        (call-interactively 'find-file)
        t))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-with-diff-1 (file beg end &optional interactive type)
  "Apply diff to file i.e. patch a FILE.

Input:

  FILE          absolute file name where to store diff.
  BEG           diff start point in buffer
  END           diff end point
  INTERACTIVE   User interaction, allow editing the patch command etc.
  TYPE          Type of patch: 'hunk means partial diff."
  (let* ((fid           "tinydiff-patch-with-diff-1")
         (file          (expand-file-name file))
         (opt-global    (or tinydiff--patch-global-option
                            ""))
         (diff-tmp      (if (ti::win32-shell-p)
                            ;;  Must use DOS paths
                            (expand-file-name tinydiff--diff-tmp-file)
                          ;;  Otherwise take it as it is
                          tinydiff--diff-tmp-file))
         (prg           (eval tinydiff--patch-program))
         (map           tinydiff--minibuffer-map)
         (source-buffer (current-buffer))
         (file-buffer   (find-buffer-visiting file))
         buffer                         ;shell messages
         data-buffer
         ff
         dir
         cmd)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (tinydiff-debug fid "in:" file beg end interactive)
    (tinydiff-debug fid "vars:" diff-tmp prg source-buffer file-buffer)
    (when (and
           file-buffer
           (ti::buffer-modified-p file-buffer)
           interactive
           (not (y-or-n-p
                 (format
                  "TinyDiff: buffer %s not saved, continue ?" file-buffer))))
      (error "TinyDiff: Aborted."))
    (if (not (file-exists-p file))
        (error "Tinydiff: file not found: %s" file ))
    (setq dir         (file-name-directory file)
          ff          (file-name-nondirectory file)
          buffer      (ti::temp-buffer tinydiff--diff-tmp-buffer  'clear)
          data-buffer (ti::temp-buffer tinydiff--patch-tmp-buffer 'clear))
    ;;  Sometimes user's UMASK is not ok. PErhaps the file is
    ;;  not readable after the write.
    (when (and (file-exists-p diff-tmp)
               (not (file-writable-p diff-tmp)))
      (error "TinyDiff: [ERROR] Not writable. Check UMASK or permissions %s"
             diff-tmp))
    (ti::write-file-as-is-macro
     (write-region (point-min) (point-max) diff-tmp))
    (unless (file-readable-p diff-tmp)
      (set-file-modes diff-tmp 384)) ;; -rw-------
    (with-current-buffer data-buffer
      (insert-buffer-substring source-buffer beg end)
      (tinydiff-patch-minibuffer-cleanup)
;;;      (pop-to-buffer (current-buffer))      (ti::d! "ok")
      (setq cmd (format "cd %s ; %s %s %s %s" dir prg opt-global ff diff-tmp))
      (tinydiff-debug fid "cmd:" cmd)
      (when interactive
        (let (tinyef-mode              ;Electric file mode OFF
	      tinycompile-mode)        ;Make sure this is off too
          (if tinyef-mode
              (setq tinyef-mode nil))   ;No-op, bytecomp silencer
          (if tinycompile-mode
              (setq tinycompile-mode nil)) ;No-op
          (setq cmd (ti::remove-properties
                     (read-from-minibuffer "> " cmd map)))
          ;;  Record command line prompt to *Messages* buffer
          (message (concat "TinyDiff: RUN " cmd))))
      (if (ti::nil-p cmd)               ;user cleared the line ?
          (message "Tinydiff: Patching cancelled.")
        (tinydiff-shell-command cmd buffer)
        (when interactive
          (display-buffer buffer)
          (or (tinydiff-patch-check-rejections cmd buffer)
              (and (stringp file)
                   (tinydiff-patch-check-if-load
                    file
                    buffer
                    type))))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-file-to-patch ()
  "Suggest possible filename to patch.

References:

  `tinydiff--patch-list'

Return:

  string     suggested file to patch
  list       list of filenames read from diff."
  (let ((fid   "tinydiff-get-file-name-list")
	(list  tinydiff--patch-list)
	stat
	file
	file-list
	re
	search-path
	dest-file)
    (unless fid ;; XEmacs byte compiler silencer
      (setq fid nil))
    (ti::dotimes counter 1 4
      (setq stat (tinydiff-get-file-name counter))
      (when stat
        (setq file (nth 0 stat)
              stat (nth 1 stat))
        ;;  Try to find also gzipped files
        (push file file-list)
        (pushnew (concat file ".gz") file-list :test 'string=)
        (if (string-match "/" file)
            (push (file-name-nondirectory file)  file-list))))
    ;;  Preserve the order. Try to find the original file first.
    (setq file-list (nreverse file-list))
    (tinydiff-debug fid "file-list:" file-list)
    (when file-list
      ;; Normally the directory part cannot be used, because people have
      ;; files in different places, search it anyway in case the
      ;; file structure is the same..
      ;;
      ;; 1. "/dir/dir/file.el"
      ;; 2. "file.el"
      ;;
      ;; ClearCase diff:
      ;; *** /work/jackr/Emacs/folding.el@@/main/5  Wed Mar 13 09:39:36 1996
      ;; --- /work/jackr/Emacs/folding.el           Wed Mar 13 13:50:26 1996
      (catch 'done
        (dolist (file file-list)
          (cond
           ((file-exists-p file)
            (setq dest-file file)
            (throw 'done t))
           ((stringp file)
            (setq file (file-name-nondirectory file))
            (dolist (elt list) ;; User '((REGEXP DIR) ..)
              ;;  Get next element from patch table
              (setq re              (nth 0 elt)
                    search-path     (eval  (nth 1 elt)))
              (when (and (string-match re file)
                         (or (setq dest-file
                                   (ti::file-get-load-path file search-path))
                             (and (string-match "\\.el$" file)
                                  (setq
                                   dest-file
                                   (ti::file-get-load-path
                                    file
                                    search-path)))))
                (throw 'done t))))))))
    (if dest-file
        dest-file)))

;;; ----------------------------------------------------------- &patch ---
;;;
(defun tinydiff-patch
  (arg &optional beg end dest-file verb type orig-buffer)
  "Try to guess diff type and region in the buffer.
If automatic detection fails user must select diff region by hand.

Input:

  PREFIX ARG    lets user to edit the diff command before executing.
                This is enabled by default for interactive calls.
  BEG END       diff region; defaults to whole buffer if nil.
  DEST-FILE     file to patch
  VERB          Be verbose.
  TYPE          Type of diff: 'hunk or nil (whole diff)
  ORIG-BUFFER   Original buffer where the whole patch is."
  (interactive
   (progn
;;; This is not a good idea if you get many patches; to
;;; ask every time...
;;;
;;;     (if (and tinydiff--package-exist-tinymy
;;;           (y-or-n-p "Do you want to make a safe copy? "))
;;;      (call-interactively 'tinymy-copy-file))
     (cond
      ((region-active-p)
       (list
        current-prefix-arg
        (region-beginning)              ;avoids region active check
        (region-end)
        (tinydiff-file-to-patch)))
      (t
       (list
        current-prefix-arg
        nil
        nil
        (tinydiff-file-to-patch))))))
  (let ((fid           "tinydiff-patch")
	(go-status     t)              ;should we proceed patching?
	buffer
	rej-flag
	char
	ZIP)
    (or orig-buffer
        (setq orig-buffer (current-buffer)))
    (let* ((patch-buffer-file-name
            (with-current-buffer orig-buffer
              buffer-file-name))
           (guess-dir  (if patch-buffer-file-name
                           (file-name-directory
                            patch-buffer-file-name)))
           (guess-file (if patch-buffer-file-name
                           (file-name-nondirectory
                            patch-buffer-file-name)))
           (guess-path patch-buffer-file-name))
      (if (and guess-file
               (string-match "\\.rej$" guess-file))
          (setq guess-file (file-name-sans-extension guess-file)
                guess-path (concat guess-dir guess-file)))
      (unless fid ;; XEmacs byte compiler silencer
        (setq fid nil))
      (setq verb (or arg verb (interactive-p)))
      (tinydiff-debug fid "in:" arg beg end)
      (or orig-buffer
          (setq orig-buffer (current-buffer)))
      (or dest-file
          (with-current-buffer orig-buffer
            (setq dest-file tinydiff--patch-to-file)))
      (tinydiff-debug fid "dest-file:" dest-file)
      ;; ......................................................... patch ...
      ;;
      (unless (stringp dest-file)
        (message
         "Tinydiff: Can't detect file along paths in tinydiff--patch-list.")
        (sit-for 0.7)
        (let ((default-directory default-directory))
          ;;  The ask prompt will start from HOME in that case.
          (unless patch-buffer-file-name
            (setq default-directory "~"))
          (setq dest-file (read-file-name
                           (format "Apply diff %sto: "
				   (if (eq type 'hunk)
				       "hunk "
				     ""))
                           guess-dir
                           nil
                           t
                           guess-file))))
      (if (or (not (stringp dest-file))
              (not (file-exists-p dest-file)))
          (error "TinyDiff: Cannot patch non-existing file. Aborted."))
      (with-current-buffer orig-buffer
        (set (make-local-variable 'tinydiff--patch-to-file) dest-file))
      ;; ........................................................... zip ...
      ;;
      (when (string-match "\\(.*\\)\\.gz$" dest-file)
        (setq dest-file (match-string 1 dest-file))
        (setq ZIP t)
        (message "TinyDiff: Uncompressing gzip file...")
        (ti::temp-buffer tinydiff--diff-tmp-buffer 'clear)
        (call-process
         "gzip"
         nil                        ;; Input
         tinydiff--patch-tmp-buffer ;; Output buffer
         nil                        ;; display
         "-d"
         (format "%s.gz" dest-file))
        (message "TinyDiff: Uncompressing %s file...done." dest-file))
      ;; ....................................................... rejects ...
      ;;
      (when (file-exists-p (concat dest-file ".rej"))
        (setq rej-flag t))
      (when (and (not (or (null tinydiff--patch-hunk-count)
                          (eq 0 tinydiff--patch-hunk-count))))
        (if (not
             (and
              (file-exists-p (concat dest-file ".orig"))
              (prog1 t
                (setq
                 char
                 (ti::read-char-safe-until
                  (format "%s\
.orig found: r = retry patch, o = back to .orig, g = go and patch"
                          (if rej-flag
                              "[.rej]"
                            ""))
                  '(?o ?r ?g    ?\e ?q ?\b ?\C-g))))))
            (when rej-flag
              (message "Tinydiff: Hm... rejection file found.")
              (sit-for 1))
          ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .orig exist ..
          (cond
           ((char-equal char ?r)             ;Retry patch to original
            (delete-file dest-file)     ;copy-file won't work otw
            (copy-file  (concat dest-file ".orig") dest-file))
           ((char-equal char ?g)             ;Go ahead
            nil)
           ((char-equal char ?o)
            (delete-file dest-file)
            (copy-file  (concat dest-file ".orig") dest-file)
            (delete-file (concat dest-file ".orig"))
            (setq go-status nil)
            (message
             "Tinydiff: Original file restored: .orig copied over %s"
             dest-file))
           (t
            (setq go-status nil)
            (message "Tinydiff: Cancelled patching.")))))
      (when go-status
        (with-current-buffer orig-buffer
          (set (make-local-variable 'tinydiff--patch-hunk-count)
               (1+ (or tinydiff--patch-hunk-count 0))))
        (tinydiff-patch-with-diff-1
         dest-file (point-min) (point-max) verb type))
      (when ZIP
        (message "TinyDiff: Compressing gzip file...")
        (call-process "gzip"
                      nil
                      nil               ; (current-buffer)
                      nil
                      "-9"
                      (format "%s.gz" dest-file))
        (message "TinyDiff: Compressing %s file...done" dest-file)))))

;;}}}
;;{{{ code: command generate

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-diff-command-generate  (&optional no-ask)
  "Return diff command as string; optionally NO-ASK."
  (if (null tinydiff--minibuffer-map)
      (run-hooks 'tinydiff--mode-define-keys-minibuffer-hook))
  (let* ((map           tinydiff--minibuffer-map)
         (tmp-file      (expand-file-name
                         tinydiff--diff-tmp-file))
         (diff-prg      (or (eval tinydiff--diff-program)
                            (error "TinyDiff: tinydiff--diff-program is nil")))
         (rcsdiff-prg   (eval tinydiff--rcsdiff-program))
         (cvsdiff-prg   (eval tinydiff--cvsdiff-program))
         (options       (or (eval tinydiff--diff-option) ""))
         (ange          (and (stringp default-directory)
                             (string-match "@" default-directory)))
         (dired         (and (null ange)
                             (string-match "dired" (symbol-name major-mode))))
         (bf            (cond
                         (dired

                          ;;  If point is out of file listing dired flags
                          ;;  error, don't mind it.

                          (ignore-errors (dired-get-filename)))
                         ((buffer-file-name))))
         (file          "")
         (file2         "")
         (dir           "")
         rev
         prompt
         ans
         cvs-info)
    ;;  This flag is used to to signal that buffer should not be
    ;;  saved, because the underlying file has changed.
    (put 'tinydiff-diff-command-generate 'buffer-not-modified nil)
    (tinydiff-debug default-directory bf "DIRED-ANG" dired ange "OPT" options)
    (tinydiff-kill-revision-list)
    (when bf
      (setq dir  (file-name-directory bf)
            file (file-name-nondirectory bf)))
    (cond
     ;; .............................................. buffer change ...
     ;; - The buffer has changed. Or file has changed.
     ((and bf
           (null dired)
           (file-exists-p bf)
           (or (buffer-modified-p)
               ;;  someone else edited or changed the same file
               (null (verify-visited-file-modtime (current-buffer))))
           (y-or-n-p "Diff between buffer and file on disk? "))
      (put 'tinydiff-diff-command-generate 'buffer-not-modified bf)
      (ti::widen-safe                 ;make sure whole buffer is saved
        (write-region (point-min) (point-max) tmp-file))
      (setq file2 tmp-file))
     ;; ........................................................ rcs ...
     ;; - Buffer is RCS controlled.
     ((and bf
           (null dired)
           (ti::vc-rcs-file-exists-p bf))
      ;; - if the revision information cannot be found, then the
      ;;   '-rX.x' switch is not used.
      (setq rev (ti::vc-rcs-buffer-version))
      (unless rev
        (message "Tinydiff: RCS Revision not detected.") (sit-for 1))
      (setq options
            (format "%s %s"
                    options
                    (if rev
                        (concat "-r" rev " ")
                      "")))
      (setq diff-prg
            (or rcsdiff-prg
                (error "TinyDiff: tinydiff--rcsdiff-program is nil"))))
     ;; .......................................................... cvs ...
     ((and bf
           (null dired)
           (or (boundp 'tinydiff-cvs-flagged)
               (setq cvs-info (ti::vc-cvs-file-exists-p bf))))
      ;; It's too expensive to call `ti::vc-cvs-file-exists-p' every time,
      ;; so we create intermediate variable to flag this buffer as CVS
      ;; controlled.
      (make-local-variable 'tinydiff-cvs-flagged)
      ;; (setq rev (ti::vc-rcs-buffer-version))
      (setq rev (car (ti::vc-cvs-entry-split-info
                      (ti::vc-cvs-entry-split cvs-info)
                      'revision)))
      (unless rev
        (message "Tinydiff: CVS Revision not detected.") (sit-for 1))
      (setq options
            (format "%s %s"
                    options
                    (if rev
                        (concat "-r" rev " ")
                      "")))
      (setq diff-prg
            (or cvsdiff-prg
                (error "TinyDiff: tinydiff--cvsdiff-program is nil"))))
     ;; .................................................... default ...
     ;; General diff prompt
     ((null ange)
      (setq dir default-directory))
     (t
      (setq dir (expand-file-name "~"))))
    ;; ... ... ... ... ... ... ... ... ... ... command generated . .
    (setq prompt (format
                  "cd %s; %s %s %s %s "
                  dir
                  (or diff-prg
                      (error
                       "TinyDiff: No diff program available. Check PATH."))
                  options
                  file
                  file2))
    (if no-ask
        (setq ans prompt)
      (let (tinyef--mode)              ;Electric file mode OFF
	(if tinyef--mode
	    (setq tinyef--mode nil))       ;No-op, bytecompier silencer
	;;  Record command line prompt to *Messages* buffer
	(message (concat "TinyDiff: " prompt))
	(setq ans (read-from-minibuffer ">" prompt map))))
    (if (ti::nil-p ans)
        (setq ans nil))
    (ti::remove-properties ans)))

;;}}}
;;{{{ code: generating, parsing diff

;;; .................................................... &diff-parsing ...

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-parse-buffer (&optional verb)
  "Prepare diff buffer for `tinydiff-mode'.  VERB.
Mark diff lines for special handling."
  (interactive)
  (let ((diff-type (car-safe (ti::buffer-diff-type-p)))
	;;  In GNU diff , there is option --initial-tab
	;;  which adds tab before each diff line to make the
	;;  text look "as it was originally"
	;;
	;;  That's why allowed whitespace at the beginning.
	(re-c2         "^[ \t]*[*][*][*] \\([0-9]+\\)")
	(re-c3         "^[ \t]*[-][-][-] \\([0-9]+\\)")
	(re-normal     "^\\([0-9]+\\)\\(,[0-9]+\\)?+[acd][0-9]")
	(re-gnu-u      "^@@[ \t]+[-+][0-9]+,[0-9]+[ \t]+[-+]+\\([0-9]+\\)")
	(re-gnu-n      "^[dac]\\([0-9]+\\) [0-9]+$")
	(prop-list     '(mouse-face    highlight
			 owner         tinydiff)))
    (ti::verb)
    (let ((sym 'font-lock-keywords))
      (set sym tinydiff--font-lock-keywords))
    (ti::text-clear-region-properties
     (point-min) (point-max) '(owner tinydiff))
    (save-excursion
      (ti::pmin)
      (cond
       ((eq diff-type 'context)
        (ti::text-re-search re-c2 nil 1 nil prop-list)
        (ti::pmin)
        (ti::text-re-search re-c3 nil 1 nil prop-list))
       ((eq diff-type 'normal)
        (ti::text-re-search re-normal nil 1 nil prop-list))
       ((eq diff-type 'gnu-u)
        (ti::text-re-search re-gnu-u nil 1 nil prop-list))
       ((eq diff-type 'gnu-n)
        (ti::text-re-search re-gnu-n nil 1 nil prop-list))))
    (if diff-type
        (if verb
            (message (concat "Tinydiff: Diff parsed, type: "
                             (prin1-to-string diff-type))))
      (if verb
          (message "Tinydiff: Diff not recognized.")))
    (run-hooks 'tinydiff--parse-buffer-hook)))

;;; ......................................................... &diff-do ...

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydiff-diff-show (cmd)
  "Generate diff CMD for the buffer and show it in the other window.
Lets user to edit option in the command line."
  (interactive
   (progn
     (when (and
            (buffer-file-name)
            (file-exists-p
             (format
              "%s#%s#"
              (file-name-directory (buffer-file-name))
              (file-name-nondirectory (buffer-file-name)))))
       (message
        "Tinydiff: There is autosave file, use minibuffer %s binding"
        (ti::keymap-function-bind-info
         'tinydiff-minibuffer--insert-file-autosave
         tinydiff--minibuffer-map))
       (sit-for 1))
     (list
      (tinydiff-diff-command-generate))))
  (if (and (stringp cmd)
           (buffer-modified-p)
           (buffer-file-name)
           ;;  If this is in the commend, we're diffing buffer against
           ;;  file on disk
           (not (or (string-match (regexp-quote tinydiff--diff-tmp-file) cmd)
                    (string-match
                     (regexp-quote (expand-file-name tinydiff--diff-tmp-file))
                     cmd)))
           (y-or-n-p "Save buffer before running diff? "))
      (save-buffer))
  (when (stringp cmd)
    (tinydiff-diff cmd 'show)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinydiff-diff-show-noask (cmd)
  "Generate diff CMD for the buffer. Guess all parameters."
  (interactive
   (list
    ;;  - The first one runs fine for rcsdiff (ie. buffer is in RCS)
    ;;    but if it fails, we have to ask paramters from user.
    (or (tinydiff-diff-command-generate 'no-ask)
        (tinydiff-diff-command-generate))))
  ;;  the `get' is set in `tinydiff-diff-command-generate'
  ;;  to indicate that buffer and file  content are no in synch, user
  ;;  does not want to save modified buffer, but check against the copy
  ;;  at disk.
  (if (and (null (get 'tinydiff-diff-command-generate 'buffer-not-modified))
           (buffer-modified-p)
           (y-or-n-p "Save buffer before running diff? "))
      (save-buffer))
  (if (not (ti::nil-p cmd))
      (tinydiff-diff cmd 'show)))

;;; ------------------------------------------------------------- &cmd ---
;;; - The diff data is inserted into register automatically, because
;;;   many time the diff data is pasted to somewhere else. Eg. by sending it
;;;   via mail to someone else in projects.
;;;
;;;
;;;###autoload
(defun tinydiff-diff (cmd &optional show verb)
  "Run diff on buffer, possibly using rcsdiff if file is version controlled.
Inserts contents into register.

The version control is determined by searching RCS strings 'Id' or 'Log'

Input:

  CMD           diff command
  SHOW          show the results
  NO-ASK        run diff without asking any questions.
  VERB          enable verbose messages

References:

  `tinydiff--extra-diff-program'
  `tinydiff--diff-buffer'
  `tinydiff--diff-options'

Return:

 nil            ,the no-ask parameter could not determine right diff.
 buffer         ,the shell output buffer. Note, that the diff may have
                 failed, in that case the buffer does not hold valid output."
  (let ((fid      "tinydiff-diff")
	(bf       (buffer-file-name))
	(buffer   (ti::temp-buffer tinydiff--diff-buffer 'clear))
	(reg      (eval tinydiff--register-diff)) ;where to put the diff content
	prereq
	tmp)
    (tinydiff-debug fid 'cmd cmd 'show show 'verb verb)
    (ti::verb)
    ;;  It the command is rcsdiff, then we must include
    ;;  Prereq: tag to the beginning of diff. Thhat tells patch
    ;;  that when applying the diff, it must find that string
    ;;  from the file before it can prosess applying the patch.
    ;;
    ;;  It prevents patching wrong versions.
    ;;
    ;;      Prereq: 1.10
    ;;
    ;;  Or we use this which is more stricter. It supposes you have
    ;;  rcs 'id' string in a file.
    ;;
    ;;      Prereq: tinylib.el,v 1.10
    ;;
    ;;  Nov 8 1996: Hm the latter isn't supported, if picks only
    ;;  "tinylib.el,v" and not whole string in the linew
    ;;  I'm going to request imprevement to GNU patch....
    (cond
     ((setq tmp (ti::string-match "-r\\([0-9.]+\\)" 1 cmd))
      ;; Add file name too
      (if (or t
              (null (tinydiff-minibuffer--read-rcs-file-name cmd)))
          (setq prereq (format "Prereq: %s\n" tmp))
;;; disabled now.
;;;     (setq tmp2    (file-name-nondirectory tmp2))
;;;     (setq prereq  (format "Prereq: %s,v %s"  tmp2 tmp))
        nil)))
    (if (and (null cmd)
             (null bf))
        (message "Tinydiff: Sorry, this is not a file buffer.")
      (if (null cmd)
          ;;  if the NO-ASK parameter is set, then we can't ask anything
          ;;  from the user. What if the file is not RCS file? Then what we
          ;;  diff against? --> give up and return nil pointer
          (setq buffer nil)
        ;; ... ... ... ... ... ... ... ... ... ... ... ... shell command . .
        ;; ELSE
        (tinydiff-shell-command cmd buffer)
        (with-current-buffer buffer
          (ti::pmin)
          (when prereq
            (insert prereq "\n"))
          (when show
            (setq tmp (current-buffer))
            ;;  Unless it's already visible in some frame.
            (if (setq tmp (get-buffer-window buffer t))
                (raise-frame (window-frame tmp))
              (display-buffer buffer)))
          (when reg
            (set-register reg (buffer-string))
            (if verb
                (message
                 (concat "Tinydiff: Diff stored in register ..."
                         (char-to-string reg)))))
          (run-hooks 'tinydiff--diff-hook))))
    buffer))

;;}}}
;;{{{ code: Misc; mime write

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-compose-diff-filename ()
  "Compose filename by reading the original filename from diff buffer.
Filename is composed like this: ~/tmp + FILE + .diff suffix."
  (let ((dir
	 (cond
	  ((file-directory-p "~/tmp/") "~/tmp/")
	  ((file-directory-p "/tmp/") "/tmp/")
	  (t
	   default-directory)))
	file
	stat
	ret)
    (ti::dotimes counter 1 4 ;;  #todo: remove ti::dotimes
      (setq stat (tinydiff-get-file-name counter))
      (when stat
        (setq file (nth 0 stat)
              stat (nth 1 stat))
        ;;   If the name is not .diff or .patch, then it will do
        (when (not (string-match "\\.diff\\|\\.patch" file))
          (setq ret file  counter 6))))
    (when ret
      (ti::file-make-path
       (if (ti::win32-shell-p)
           (expand-file-name dir)
         dir)
       (concat file ".patch")))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-write-file (file)
  "Write current diff to temporary file.
This is purely an interactive function.
The suggested to be written is named like this: ~/tmp + FILE + .diff suffix."
  (interactive
   (let* ((file-name            (tinydiff-compose-diff-filename))
          (file                 (and file-name
                                     (file-name-nondirectory file-name)))
          (default-directory    (if file-name
                                    (file-name-as-directory
                                     (file-name-directory file-name))
                                  default-directory)))
     (list (read-file-name "Write diff to: " nil nil nil file))))
  (unless (ti::nil-p file)
    (write-region (point-min) (point-max) file)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-mime-compose  (&optional insert-to-mail verb)
  "Read current buffer and make TM MIME attachement.
Save attachement to `tinydiff--register-diff' or to a mail buffer,
which must have MIME-edit mode active.

The TM MIME spec is used: 7bit, type=patch.
If you need other specifications, insert diff via TM's insert file.

Input:

  INSERT-TO-MAIL    Flag, if non-nil, add MIME block to the end of
                    buffer pointed by `tinydiff--mail-buffer-function'.
  VERB              Be verbose."
  (interactive "P")
  (let* ((file  (file-name-nondirectory (tinydiff-compose-diff-filename)))
         (mime-tag
          (format
           (concat
            "--[[application/octet-stream; type=patch\n"
            "Content-Disposition: attachment; "
            "filename=\"%s\"][7bit]]\n")
           file))
         (obuffer  (current-buffer))
         (mime-p   t)
         mail-buffer)
    (ti::verb)
    (cond
     (insert-to-mail
      (unless (setq mail-buffer (funcall tinydiff--mail-buffer-function))
        (error "Tinydiff: Can't find mail buffer where to insert to"))
      (with-current-buffer mail-buffer
        (when (not (or (ti::mail-mime-tm-edit-p)
                       (ti::mail-mime-semi-edit-p)))
          (setq mime-p nil)))
      ;;  Do not add the mime tag, if there is noTM mime edit mode
      (if mime-p
          (ti::append-to-buffer mail-buffer mime-tag))
      (append-to-buffer mail-buffer (point-min) (point-max))
      (when verb
        (message "Tinydiff: %sdiff appended to buffer: %s"
                 (if mime-p "Mime " "")
                 (buffer-name mail-buffer))))
     (t
      (with-temp-buffer
        (insert mime-tag)
        (insert-buffer-substring obuffer)
        (set-register tinydiff--register-diff (buffer-string))
        (when verb
          (message "TinyDiff: MIME diff in register `%c'"
                   tinydiff--register-diff)))))))

;;}}}

;;{{{ code: Line functions

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-get-buffer-name ()
  "Return buffer name of the current diff."
  (let* (file
         ret
         list)
    (save-excursion
      (cond
       ((progn (ti::pmin)
;;;                    (ti::d! "RCS file")
               (re-search-forward "^RCS file: .*/\\(.*\\),v" nil t))

        ;; RCS file: RCS/folding.el,v
        ;; retrieving revision 1.18
        ;; retrieving revision 1.19
        (setq file (match-string 1)))
       ((progn (ti::pmin)
;;;           (ti::d! "---")
               (re-search-forward "^---[ \t]+\\([^\t ]+\\)" nil t))
        ;;  --- copy/tinydiff.el Wed Jan 24 16:08:05 1996
        ;;  +++ tinydiff.el      Wed Jan 24 16:29:07 1996
        ;;  @@ -1,6 +1,6 @@
        (setq file (match-string 1))
        (or (or (setq ret (and (string-match "/" file)
                               (find-buffer-visiting file)))
                (setq ret (get-buffer (file-name-nondirectory file))))
            ;;  Hmm; the "---" file was not found; try "***" file
            (and
             (progn
               (ti::pmin)
;;;           (ti::d! "***")
               (re-search-forward "^\\*\\*\\*[ \t]+\\([^\t ]+\\)" nil t)
               ;;  *** copy/tinydiff.el Wed Jan 24 16:08:05 1996
               ;;  --- /tmp/tdi.diff    Wed Jan 24 16:29:07 1996
               (setq file (match-string 1)))
             (or (setq ret (and (string-match "/" file)
                                (find-buffer-visiting file)))
                 (setq ret (get-buffer (file-name-nondirectory file)))))))
       ((progn (ti::pmin)
               (re-search-forward "^filename: \\([^\t ]+\\)$" nil t))
        ;;  User tag, e.g. output from shell script that generates the
        ;;  'filename' tag + runs the diff program.
        (setq file (match-string 1)))
       (t
        ;;  other diff formats .. Still open
        nil))
      ;; ............................................. examine results ...
      (when (and (not ret)
                 file)
        (if file                        ;remove directory part
            (setq file                  ;; returns nil if no "/" found
                  (file-name-nondirectory file)))
        (cond
         ((get-buffer file)
          (setq ret file)) ;;  - buffer name == file name
         ((setq list (ti::dolist-buffer-list (string-match file (buffer-name))))
          ;;  - Eg name tinydiff.el may be in name tinydiff.el<2>
          ;;  - just pick the first from list
          (setq ret (car list)))))

      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-get-line-number ()
  "Return diff line number if line has one."
  (let ((diff-type     (car-safe (ti::buffer-diff-type-p)))
	(re-c2         "^[*][*][*] \\([0-9]+\\)")
	(re-c3         "^[-][-][-] \\([0-9]+\\)")
	(re-normal     "^\\([0-9]+\\)\\(,[0-9]+\\)?+[acd][0-9]")
	;; Wrong: this returned left hand number
;;;         (re-gnu-u      "^@@ [-+]\\([0-9]+\\),[0-9]+[ \t]+[-+]+")
	(re-gnu-u      "^@@ [-+][0-9]+,[0-9]+[ \t]+[-+]+\\([0-9]+\\)")
	(re-gnu-n      "^[dac]\\([0-9]+\\) [0-9]+$")
	ret)
    (save-excursion
      (beginning-of-line)
      (cond
       ((eq diff-type 'context)
        (or (setq ret (ti::buffer-match re-c2 1))
            (setq ret (ti::buffer-match re-c3 1))))
       ((eq diff-type 'normal)
        (setq ret (ti::buffer-match re-normal 1)))
       ((eq diff-type 'gnu-n)
        (setq ret (ti::buffer-match re-gnu-n 1)))
       ((eq diff-type 'gnu-u)
        (setq ret (ti::buffer-match re-gnu-u 1)))))
;;;    (ti::d! (match-string 0) diff-type)
    (when ret
      (setq ret (string-to-number ret)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto (buffer line)
  "Show BUFFER and put cursor at LINE in other window."
  (let ((ob     (current-buffer))      ;original buffer
	(delay  0.1))
    (switch-to-buffer-other-window buffer)
    (ti::goto-line line)
    ;; Flash the cursor and go back to diff buffer
    (and delay
         (sit-for delay))
    (pop-to-buffer ob)))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-next (&optional back verb no-update)
  "Search next position, or  BACKWARD.

Input:

  BACK          if non-nil then search backward
  VERB          enable verbose messages
  NO-UPDATE     do not update diff source buffer

Return:

  t             if successful
  nil           no more hits"
  (interactive)
  (let* ((diff-type     (car-safe (ti::buffer-diff-type-p)))
         (re-c2         "^[*][*][*] \\([0-9]+\\)")
;;;         (re-c3         "^[-][-][-] \\([0-9]+\\)")
         (re-normal     "^[0-9]+\\(,[0-9]+\\)?+[acd][0-9]")

;;;         (re-gnu-u      "^@@ [-+]\\([0-9]+\\),[0-9]+[ \t]+[-+]+")
         (re-gnu-u      "^@@ [-+][0-9]+,[0-9]+[ \t]+[-+]+\\([0-9]+\\)")
         (re-gnu-n      "^[dac]\\([0-9]+\\) [0-9]+$")
         (type-list
          (list
           (cons 'context
                 re-c2)
           (cons 'normal
                 re-normal)
           (cons 'gnu-u
                 re-gnu-u)
           (cons 'gnu-n
                 re-gnu-n)))
         (func          (if back 're-search-backward 're-search-forward))
         buffer
         ret
         re)
    (ti::verb)
    (setq re (cdr-safe (assoc diff-type type-list)))
    (ignore-errors
      (when (setq buffer (funcall tinydiff--source-buffer-function))
        (setq tinydiff--diff-source-buffer buffer) ))
    (when (stringp re)
      (if back
          (beginning-of-line)
        (end-of-line))
      (if (null (funcall func re nil t))
          (if verb (message "Tinydiff: No more hits."))
        ;;   Adjust the display to middle of the screen
        (unless no-update
          (goto-char (match-beginning 0))
          (recenter '(4))               ;show it
          (tinydiff-goto-kbd 'verb))
        (setq ret t)))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-prev ()
  "Search diff position backward."
  (interactive)
  (beginning-of-line)
  (tinydiff-goto-next 'back 'verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-prev-no-update ()
  "Search diff position backward."
  (interactive)
  (beginning-of-line)
  (tinydiff-goto-next 'back 'verb 'no-update))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-next-no-update ()
  "Search next position."
  (interactive)
  (end-of-line)
  (tinydiff-goto-next nil 'verb 'no-update))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-kbd (&optional verb)
  "Show the diff source in another window. VERB."
  (interactive)
  (let ((line   (tinydiff-get-line-number))
	(buffer (tinydiff-source)))
    (ti::verb)
    (if (null line)
        ;; nothing to do, not valid line
        (if verb (message
                  "Tinydiff: Can't find source line reference."))
      (if (null buffer)
          (if verb (message "Tinydiff: Cannot find buffer name for diff."))
        (tinydiff-goto buffer line)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-goto-mouse (event)
  "Show current line in other window. Use mouse EVENT.
Activate only if point underneath has 'mouse-property."
  (interactive "e")
  (let ((buffer (tinydiff-source))
	line)
    (when (ti::text-get-mouse-property)
      (setq line (ti::remove-properties (ti::buffer-read-word "[0-9]+")))
      (if (and buffer
               (not (ti::nil-p line)))
          (tinydiff-goto buffer (string-to-number line))
        (message "Tinydiff: Sorry, missing Line Number or filenname.")))))

;;}}}
;;{{{ code: patch block handling

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-header ()
  "Return the diff header."
  (save-excursion
    (ti::pmin)
    ;; --- foo-2.4.orig/config.guess
    ;; +++ foo-2.4/config.guess
    (when (looking-at "^--- ")
      (forward-line 2)
      (buffer-substring (point-min) (point)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-block-region  ()
  "Return (beg . end) of diff block around current point or nil."
  (let (beg
         end)
    (end-of-line)
    (setq end (point))
    (or (tinydiff-goto-next nil nil 'no-update) ;; *** 2720,2727 ****
        (goto-char (point-max)))
    ;; If the point moved to somewhere.
    (if (eq end (point))
        (setq end nil)
      ;;  We need the previous line too
      ;;
      ;;  ***************
      ;;  *** 4687,4692 ****
      ;;
      (forward-line -1)
      (setq end (line-beginning-position)))
    (setq beg (point))
    (tinydiff-goto-next 'back nil 'no-update)
    (if (eq beg (point))
        (setq beg nil)
      ;;  Take the whole hunk
      (if (looking-at "^[*][*][*] ")
          (forward-line -1))
      (setq beg (line-beginning-position)))
    (if (and beg end)
        (cons beg end))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-block-kill  ()
  "Kill Diff block around point."
  (interactive)
  (let ((region (tinydiff-block-region))
	buffer-read-only)
    (if (and (interactive-p)
             (null region))
        (message "TinyDiff: can't determine diff block bounds.")
      (delete-region (car region) (cdr region)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-block-apply-patch  ()
  "Apply diff hunk around point.
References:
  `tinydiff-patch-set-option'."
  (interactive)
  (let ((add-opt tinydiff--patch-global-option)
	(region  (save-excursion
		   (or (tinydiff-block-region)
		       (error "TinyDiff: Hunk region not found."))))
	(header  (or (tinydiff-header)
		     (progn
		       (message "TinyDiff: [WARN] Patch header not found.")
		       "")))
	(buffer  (current-buffer))
	file)
    (if (and (interactive-p)
             (null region))
        (message "TinyDiff: can't determine diff hunk bounds.")
      (setq file (tinydiff-file-to-patch))
      (with-temp-buffer
        (insert header)
        (insert-buffer-substring buffer (car region) (cdr region))
        (tinydiff-patch nil nil nil  file 'verb 'hunk buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinydiff-patch-set-option (opt-string)
  "Set `tinydiff--patch-global-option' to OPT-STRING.
E.g. to apply revese diff, you may want to set the option to: -R"
  (interactive
   (list
    (read-string
     (format
      "Tinydiff patch option%s: "
      (if (and (stringp tinydiff--patch-global-option)
               (not (string-match "^[ \t]*$"
                                  tinydiff--patch-global-option)))
          (format " [%s]"
                  tinydiff--patch-global-option)
        "")))))
  (setq tinydiff--patch-global-option opt-string))

;;}}}
;;{{{ setup: Install

(defadvice cvs-mode-diff (after tinydiff-turn-on-diff-mode act)
  "Call `turn-on-tinydiff-mode'."
  (when (boundp 'cvs-diff-buffer-name)
    (with-current-buffer (symbol-value 'cvs-diff-buffer-name)
      (unless (fboundp 'diff-mode)
        (turn-on-tinydiff-mode)))))

(add-hook 'tinydiff--mode-define-keys-hook  'tinydiff-mode-define-keys)
(add-hook 'tinydiff--parse-buffer-hook      'turn-on-font-lock-mode)

;; These have to be here, because when someone says
;;   (add-hook 'tinydiff--diff-hook 'my-tinydiff--diff-hook)
;;
;; The variable gets defined immediately. --> following does nothing...
;;   (defvar tinydiff--diff-hook '(tinydiff-parse-buffer tinydiff-mode))

(ti::add-hooks 'tinydiff--diff-hook
               '(tinydiff-parse-buffer
                 turn-on-tinydiff-mode
                 tinydiff-turn-on-view-mode))

(tinydiff-install)

;;}}}

(provide   'tinydiff)
(run-hooks 'tinydiff--load-hook)

;;; tinydiff.el ends here
