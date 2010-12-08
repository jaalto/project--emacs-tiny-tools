;;; tinylpr.el --- Easy Emacs lpr command handling, pop-up, completions

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1996-2010 Jari Aalto
;; Keywords:     extensions
;; Author:       Jari Aalto
;; Maintainer:   Jari Aalto
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

;;; Install:

;; ....................................................... &t-install ...
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   ~/.emacs startup file
;;
;;      (require 'tinylpr)
;;
;;  Suggested key binding. The commands are available via echo-menu, but
;;  you can bind each command individually too. The echo menu:
;;
;;      (ti::use-prefix-key "\C-z")          ;; Free C-z for us.
;;      (global-set-key "\C-zp" (ti::definteractive (ti::menu-menu 'tinylpr--menu)))
;;
;;   Notice, that loading this file changes your `lpr-command' immediately
;;   to "sh". This is essential and if you want to use this package,
;;   leave it there or choose some compatible shell that accepts "-c"
;;   switch.
;;
;;   CHANGE THE VARIABLES !
;;
;;   You must copy the user variables and put your own definitions
;;   there. The ones that ship with this module are only examples

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      o   Managing printers or print styles easily
;;      o   Queue information
;;      o   Has ready X-popup example to select print styles etc.
;;      o   Echo menu provided to select printing advice:
;;
;;          TinyLpr: 01c2 r)egion b)uffer l)ine numbers d)printer
;;                   Q)ueue s)tyle >P
;;
;;      o   Ps print support in another `P' echo-menu:
;;
;;          TinyLpr: 01c2(ps) rR)egion bB)uffer sS)Spool d)espool
;;
;;  Introduction
;;
;;      Unix environment offers numerous utilizes to format printing
;;      the user wants, not to mention the numerous printers that can be set.
;;      you may find these command in your system:
;;
;;          mmpage       multi-page 1-8, like sunOS enscript
;;          lp
;;          lpr
;;          a2ps
;;          squeeze.awk  my own empty line squeezer. Ever run CPP on C/C++ ?
;;          groff        I make some nroff files...
;;          banner       big letters
;;          lpstat
;;          col -bx      remove ctrl codes from man pages
;;          pps          pretty printer for PostScript -- jau@tut.fi
;;          pr           format files
;;          fold         fold long lines for finite width output device
;;          adjust       for filling, centering, ..justifying
;;
;;      If you want to print the file in some other format, i.e. combining
;;      some of the commands above, you have change `lpr-switches'
;;      every time. This is tedious. Instead this package offers a pop up menu
;;      where you can select lpstatus, select print command, cancel print
;;      job etc...
;;
;;  Example
;;
;;          (defun my-x-menu (event)
;;            "Pop up an X window of user defined commands. "
;;            (interactive "e")
;;            (let* ((pstat
;;                 (replace-regexp-in-string           ;remove directory name
;;                  ".*/"
;;                  ""
;;                  (or (my-print-status) "")))
;;                item)
;;              (setq
;;               item
;;               (x-popup-menu
;;                event
;;                (list
;;                 "Command Menu"
;;                 (list
;;               "Printer: "
;;               ;;   This first one is header, not selection
;;               (cons (concat ":: " pstat) 'ignore)
;;               ;; these are selections
;;               '("* Print region"     . print-region)
;;               '("* Print buffer"     . print-buffer)
;;               '("Destination"        . tinylpr-select-printer)
;;               '("Print style"        . tinylpr-print-style-select)
;;               '("Queue status"       . tinylpr-queue)))))
;;              (cond
;;               (item                                  ;direct command
;;                (call-interactively item)))))

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)
(require 'lpr)

(eval-and-compile

  (autoload 'ps-print-buffer            "ps-print" nil t)
  (autoload 'ps-print-buffer-with-faces "ps-print" nil t)
  (autoload 'ps-print-region            "ps-print" nil t)
  (autoload 'ps-print-region-with-faces "ps-print" nil t)
  (autoload 'ps-spool-buffer            "ps-print" nil t)
  (autoload 'ps-spool-buffer-with-faces "ps-print" nil t)
  (autoload 'ps-spool-region            "ps-print" nil t)
  (autoload 'ps-spool-region-with-faces "ps-print" nil t)

  (defvar ps-lpr-switches)              ;to quiet ByteCompiler
  (defvar lpr-switches)
  (defvar lpr-command))

(ti::package-defgroup-tiny TinyLpr tinylpr-- extensions
  "Easy Emacs lpr command handling, popup, completions
        o   Managing printers or print styles easily
        o   Queue information
        o   Has ready X-popup example to select print styles etc.")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom tinylpr--load-hook nil
  "Hook run when file is loaded."
  :type  'hook
  :group 'TinyLpr)

;;; .......................................................... &v-vars ...
;;; *** important ***
;;;
;;;     These are just examples. Copy the variables into your ~/.emacs
;;;     and make changes to reflect your system.
;;;
;;;

(defcustom tinylpr--set-ps-lpr-switches  t
  "If non-nil, set also ps-lpr-switches from ps-print.el when
changing printer."
  :type  'boolean
  :group 'TinyLpr)

(defcustom tinylpr--queue-cmd
  (or (executable-find "lpstat")
      (let ((function (if (ti::win32-p)
                          'message
                        'error)))
        (funcall function
                 "TinyLpr: can't use default [lpstat] for tinylpr--queue-cmd")))
  "*Shell Command to return queue status"
  :type  '(string :tag "Shell Command")
  :group 'TinyLpr)

(eval-and-compile
  (defcustom tinylpr--printer-list
    (delq nil
          (list
           (getenv "PRINTER")
           (if (ti::win32-p) "lpt1:")
           (if (ti::win32-p) "prn:")))
    "*List of available printers, like  '(\"PRINTER1\" \"PRINTER2\")."
    :type  '(repeat (string :tag "printer"))
    :group 'TinyLpr)

  (defcustom tinylpr--print-style-list
    (let ((mp   (executable-find "mpage")) ;HP-UX multipage
	  (lp   (executable-find "lp"))
	  (lpr  (executable-find "lpr"))
	  (nl   (executable-find "nl"))
	  (ens  (executable-find "enscript"))
	  (gs   (executable-find "gs"))
	  (gs32 (executable-find "gs386"))) ;; Ghostscript in Win32
      (delq
       nil                              ;Remove empty entries
       (list
        (if lp
            (list
             ;;  Select the first string so, that it's easy to complete.
             "lp, straight lp"    (concat lp " -d#")))
        (if lpr
            (list
             "lpr straight"    (concat lpr " -d#")))
        (if (and nl lp)
            (list
             "nl, numbered lp"    "nl | lp -d#"))
        (if mp
            (list
             "2 mpage"            (concat  mp " -A -2 -P#")))
        (if mp
            (list
             "4 mpage"            (concat  mp " -A -4 -P#")))
        (if mp
            (list
             "8 mpage"            (concat  mp " -A -8 -P#")))
        (if mp
            (list
             "2l mpage landscape" (concat  mp " -A -l -2 -P#")))
        (if ens
            (list
             "enscript"           (concat  ens " -d#")))
        (if ens
            (list
             "et enscript TOC"    (concat  ens " --toc -d#")))
        (if ens
            (list
             "2l enscript landscape" (concat  ens " -r -2 -d#")))
        (if gs
            (list
             "ghostscript a4"
             (concat gs "-q -dNOPAUSE -sDEVICE=SomeDevice"
                     "-r600 -sPAPERSIZE=a4 "
                     "-sOutputFile=#"
                     "-Ic:/gs -"))))))
    "*Available print styles.
The # char tells where to install printer in command.

Format:

  '((COMPLETION-STRING PRINTER-COMMAND-STRING) ..)

Example

  '((\"2 pages\"  \"mpage -A -2 -P#\"))"
    :type '(repeat
            (list (string :tag "Completion name")
                  (string :tag "Shell Command.")))

    :group 'TinyLpr)

  ) ;; eval-and-compile

;;; ....................................................... &v-private ...

(defvar tinylpr--current-printer (car-safe tinylpr--printer-list)
  "Private. Current printer.")

(defvar tinylpr--current-print-style (car-safe (car-safe tinylpr--print-style-list))
  "Private. Current print style.")

(defvar tinylpr--printer-list-history nil
  "Private. History list for `tinylpr--printer-list'.")

(defvar tinylpr--print-style-history nil
  "Private. History list for tinylpr-print-style-completions.")

(defvar tinylpr--menu
  '((format
     "TinyLpr: %s r)egion b)uffer l)ine numbers d)printer Q)ueue s)tyle >P"
     tinylpr--current-printer)
    ((?d  . (t (call-interactively 'tinylpr-select-printer)))
     (?Q  . (  (call-interactively 'tinylpr-queue)))
     (?s  . (t (call-interactively 'tinylpr-print-style-select)))
     (?r  . (  (call-interactively 'print-region)))
     (?b  . (  (call-interactively 'print-buffer)))
     (?l  . (  (call-interactively 'tinylpr-print-with-line-numbers)))
     (?P  . tinylpr--ps-print-menu)))
  "*Echo menu to access printer commands. Select `P' for ps-print.el commands.")

(defvar tinylpr--ps-print-menu
  '((format "\
TinyLpr: %s(ps) rR)egion bB)uffer sS)Spool d)espool "
            tinylpr--current-printer)
    ((?r  . (  (call-interactively 'ps-print-region)))
     (?R  . (  (call-interactively 'ps-print-region-with-faces)))
     (?b  . (  (call-interactively 'ps-print-buffer)))
     (?B  . (  (call-interactively 'ps-print-buffer-with-faces)))
     (?s  . (  (call-interactively 'ps-spool-buffer)))
     (?S  . (  (call-interactively 'ps-spool-buffer-with-faces)))
     (?w  . (  (call-interactively 'ps-spool-region)))
     (?W  . (  (call-interactively 'ps-spool-region-with-faces)))
     (?d  . (  (call-interactively 'ps-despool)))))
  "*Echo menu to access ps-print commands.

  r     Print region.
  R     Print region with faces.
  b     Print buffer.
  B     rint buffer with faces.

  s     Spool buffer.
  S     Spool buffer with faces.
  w     Spool region.
  W     Spool region with faces.

  d     Despool (send spooled items)")

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ code: funcs

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-install-lpr-command ()
  "Set correct shell for `lpr-command'."
  (interactive)
  (let (sh)
    (unless (string-match "sh\\|bash\\|cmd.exe\\|command.exe"
                          (or lpr-command ""))
      ;; NT Cygnus users get served too by putting sh,bash test first.
      (cond
       ((setq sh (or (executable-find "sh")
                     (executable-find "bash")))
        (setq lpr-command sh))
       ((and (ti::win32-p)
             (setq sh (or (executable-find "cmd")
                          (executable-find "command"))))
        (setq lpr-command sh))
       (t
        (error "\
TinyLpr: sh, bash or cmd.exe not available. Can't set lpr-command." ))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-set-command (template printer)
  "Substitutes possible # n TEMPLATE with PRINTER name in."
  (if (string-match "\\(#\\)" template)
      (setq template (ti::replace-match 1 printer template)))

  ;;  We know the lpr-command is "sh", so just put the "-c" as
  ;;  first option.

  (cond
   ((string-match "\\(sh\\|bash\\)$" lpr-command)
    (setq lpr-switches (list "-c" template)))
   ((string-match "\\(command\\|cmd\\)\\.exe$" lpr-command) ;Win32
    (setq lpr-switches (list "/c" template)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-print-with-line-numbers  ()
  "Adds line numbers to buffer and prints it. After printing,
removes line numbers."
  (interactive)
  (let (buffer-read-only
	fmt
	len)
    (with-buffer-modified
      (save-excursion
        (ti::pmax)
        ;;  Set dynamic format according to biggest line number
        (setq len  (ti::digit-length (ti::current-line-number))
              fmt  (concat "%0" len "d: %s"))
        (unwind-protect
            (progn
              (ti::buffer-insert-line-numbers (point-min) (point-max) 1 1 fmt)
              (print-buffer))
          (ti::buffer-remove-line-numbers
           (point-min)
           (point-max)
           "^[0-9]+: " 0))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-print-style-completions ()
  "Build up the completion array."
  (let ((i 0)
	completions)
    (dolist (x tinylpr--print-style-list)
      (setq i (1+ i))
      (setq completions (cons  (cons (car x) i) completions)))
    completions))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-setting-status ()
  "Return current settings."
  (interactive)
  (let ((stat (nth 1 lpr-switches)))
    (if (interactive-p)
        (message stat))
    stat))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-queue ()
  "Return queue status."
  (interactive)
  (let ((cmd       tinylpr--queue-cmd)
	(buffer    (ti::temp-buffer "*tmp*" 'clear)))
    (display-buffer buffer)
    (shell-command cmd buffer)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-select-printer (printer)
  "Select PRINTER printer."
  (interactive
   (list
    (completing-read
     (concat "Printer [" tinylpr--current-printer "]: ")
     (ti::list-to-assoc-menu tinylpr--printer-list)
     nil t
     nil
     'tinylpr--printer-list-history)))
  (when (not (ti::nil-p printer))
    (setq tinylpr--current-printer printer)
    (if tinylpr--set-ps-lpr-switches
        (setq ps-lpr-switches (list (concat "-P" printer))))
    (tinylpr-print-style-select tinylpr--current-print-style)))

;;; ----------------------------------------------------------------------
;;;
(defun tinylpr-print-style-select (arg)
  "Select print output style with ARG."
  (interactive
   (list
    (completing-read
     (format "Print style [%s: %s ]: "
             (or tinylpr--current-print-style "<style unknown>")
             (tinylpr-setting-status))
     (tinylpr-print-style-completions)
     nil
     t)))

  (let ((printer (or tinylpr--current-printer ""))
	elt
	args)
    ;;  Try to find the style in assoc array
    (if (not (and arg (setq elt (assoc arg tinylpr--print-style-list))))
        (message "No such style")
      ;;  replace # with printer name
      (setq tinylpr--current-print-style arg)
      (setq args (nth 1 elt))
      (tinylpr-set-command args printer)
      (message "Print <%s> on %s" arg (tinylpr-setting-status)))))

;;}}}
;;{{{ code: install

;;; .................................................... &auto-install ...

;; Install package, reset lpr variables

(tinylpr-install-lpr-command)

(let* ((template (nth 1 (car  tinylpr--print-style-list))))
  (if (and template tinylpr--current-printer)
      (tinylpr-set-command template tinylpr--current-printer)
    (message "\
TinyLpr: ** Auto setup failure, please define tinylpr--print-style-list and
TinyLpr: ** tinylpr--current-printer")))

;;}}}

(provide 'tinylpr)
(run-hooks 'tinylpr--load-hook)

;;; tinylpr.el ends here
