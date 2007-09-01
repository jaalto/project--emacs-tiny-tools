;;; tinyliba.el --- Library for (a)utoload definitions
;; $Id: tinyliba.el,v 2.93 2007/05/06 23:06:11 jaalto Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1998-2007 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; To get information on this program, call M-x tinyliba-version.
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
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;; DO NOT LOAD THIS FILE, but load the central library "m". It loads this
;; file and backward compatible library "b"
;;
;;      (require 'tinylibm)
;;
;; See also Sourceforge project `apel'

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Preface, 1998
;;
;;      This is lisp function library, package itself does nothing.
;;      This library defines autoload functions and few emacs version
;;      detection functions.
;;
;;      The autoloads are automatically generated and YOU SHOULD NOT
;;      FIX THEM BY HAND. To add or update autoloads from a package,
;;      do it like this:
;;
;;      o   Generate autoloads to separate buffer with
;;          command
;;;         C-u M-x tinylisp-autoload-generate-library RET file.el RET
;;      o   At the end of buffer *tinylisp-autoloads* cut'n paste
;;          the definititions to this file.
;;
;;      NOTE: If function already exists in Emacs/XEmacs, an autoload
;;      definition here does nothing. Like `describe-symbol-find-file',
;;      which is already defined in XEmacs.

;;}}}

;;; Change Log:

;;; Code:

;;{{{ code: Init

(provide 'tinyliba)

;; Older byte compiler doesn't allow putting these inside
;; `eval-and-compile'. The message was:
;;
;;   ** The compiler ignores `autoload' except at top level.  You should
;;      probably put the autoload of the macro `with-timeout' at top-level.

(autoload 'with-timeout      "timer"        "" nil 'macro)
(autoload 'easy-menu-define  "easymenu"     "" nil 'macro)
(autoload 'executable-find   "executable")

(eval-and-compile ;; function must be visible at load time
  (defun ti::tmp-cl-library-check ()
    "Check that cl.el library is correct and force loading if not.
This function is run only once at tinynyliba.el load."
    (let* ((pkg   (featurep 'tinypath))
           (mode  (if (and pkg
                           (boundp 'tinypath-:cache-mode))
                      ;; Quiet Byte Compiler
                      (symbol-value 'tinypath-:cache-mode))))
      ;; Turn off advices and cache only IF the package is active
      (if (and pkg mode)
          (let ((func 'tinypath-cache-mode))
            (if (fboundp func)
                (funcall func -1))))
      (unless (fboundp 'return)
        ;;  cl.el version 3.0 does not define macro `return'. cl
        ;;  2.02(19.34) is ok. This was noticed by Sami Khoury
        ;;  <skhoury@cse.dnd.ca>
        (let ((location (locate-library "cl")))
          (error "\
** tinyliba.el: Core library `cl' [%s] is dysfunctional.
                (require 'cl) dind't provide standard CL statement
                `return'. This may be a problem in `load-path' order.
                Do you need to re-arrange it? The package `cl' is in [%s]"
                 location)))
      ;;  But, even if there is `return', the `dolist' macro may be broken.
      ;;  In Emacs 21.3 the dolist was moved to subr.el but with a
      ;;  broken implementation.
      (condition-case err
          (dolist (elt '(1))
            (return elt))
        (error
         (message "\
** tinyliba.el [ERROR] Broken `dolist' implementation!
                A simple `dolist' call with `return' statement failed
                with error [%s]. Trying to fix this by loading
                `cl-macs.el' explicitly." err)
         (load-library "cl-macs.el")))
      ;;  Do post-check if everything is ok.
      (condition-case nil
          (dolist (elt '(1))
            (return elt))
        (error
         (message "\
** tinyliba.el [ERROR] Still broken `dolist' implementation!
                It's impossible to know why this happened,
                Try searching all cl*.el files along path and checking
                if any of them define dysfunctional `dolist'")))
      ;;  Restore caching feature
      (if (and pkg mode)
          (let ((func 'tinypath-cache-mode))
            (if (fboundp func)
                (funcall func 1)))))))

(eval-and-compile
  (require 'cl)
  (ti::tmp-cl-library-check)
  ;; defvar silences Byte Compiler
  (defvar byte-compile-dynamic nil "") ;; Introduced in Emacs 19.29
  (make-local-variable 'byte-compile-dynamic)
  (setq byte-compile-dynamic t))

;;}}}

(eval-and-compile

  ;; XEmacs and Emacs differ here

  ;; (if (locate-library "rsz-mini")
  ;;     (autoload 'resize-minibuffer-mode "rsz-mini")
  ;;   (autoload 'resize-minibuffer-mode "rsz-minibuf"))
  ;;

  ;;{{{ code: Autoload easymenu.el

  ;;  These are from XEmacs 19.14, they should suffice

  (autoload 'easy-menu-do-define                  "easymenu" "" nil)
  (autoload 'easy-menu-add                        "easymenu" "" nil)
  (autoload 'easy-menu-remove                     "easymenu" "" nil)

;;; ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  .. Emacs 19.30  ..

  ;; (autoload 'easy-menu-define                     "easymenu" "" nil 'macro)
  ;; (autoload 'easy-menu-do-define                  "easymenu" "" t)
  ;; (autoload 'easy-menu-create-keymaps             "easymenu" "" nil)
  ;; (autoload 'easy-menu-change                     "easymenu" "" nil)
  ;; (autoload 'easy-menu-remove                     "easymenu" "" nil)
  ;; (autoload 'easy-menu-add                        "easymenu" "" nil)

  ;;}}}
  ;;{{{ code: Autoload skeleton.el

  (autoload 'define-skeleton                      "skeleton" "" t 'macro)
  (autoload 'skeleton-proxy-new                   "skeleton" "" t)
  (autoload 'skeleton-proxy                       "skeleton" "" t)
  (autoload 'skeleton-abbrev-cleanup              "skeleton" "" nil)
  (autoload 'skeleton-insert                      "skeleton" "" nil)
  (autoload 'skeleton-read                        "skeleton" "" nil)
  (autoload 'skeleton-internal-list               "skeleton" "" nil)
  (autoload 'skeleton-internal-1                  "skeleton" "" nil)
  (autoload 'skeleton-pair-insert-maybe           "skeleton" "" t)

  ;;}}}
  ;;{{{ code: Autoload cl

  ;; cl-compat.el Emacs 19.34

  (autoload 'defkeyword                           "cl-compat" "" nil 'macro)
  (autoload 'keywordp                             "cl-compat" "" nil)
  (autoload 'keyword-of                           "cl-compat" "" nil)
  (autoload 'values                               "cl-compat" "" nil)
  (autoload 'values-list                          "cl-compat" "" nil)
  (autoload 'multiple-value-list                  "cl-compat" "" nil 'macro)
  (autoload 'multiple-value-call                  "cl-compat" "" nil 'macro)
  (autoload 'multiple-value-bind                  "cl-compat" "" nil 'macro)
  (autoload 'multiple-value-setq                  "cl-compat" "" nil 'macro)
  (autoload 'multiple-value-prog1                 "cl-compat" "" nil 'macro)
  (autoload 'build-klist                          "cl-compat" "" nil)
  (autoload 'extract-from-klist                   "cl-compat" "" nil)
  (autoload 'keyword-argument-supplied-p          "cl-compat" "" nil)
  (autoload 'elt-satisfies-test-p                 "cl-compat" "" nil)
  (autoload 'cl-floor                             "cl-compat" "" nil)
  (autoload 'cl-ceiling                           "cl-compat" "" nil)
  (autoload 'cl-round                             "cl-compat" "" nil)
  (autoload 'cl-truncate                          "cl-compat" "" nil)
  (autoload 'safe-idiv                            "cl-compat" "" nil)
  (autoload 'pair-with-newsyms                    "cl-compat" "" nil)
  (autoload 'zip-lists                            "cl-compat" "" nil)
  (autoload 'unzip-lists                          "cl-compat" "" nil)
  (autoload 'reassemble-argslists                 "cl-compat" "" nil)
  (autoload 'duplicate-symbols-p                  "cl-compat" "" nil)
  (autoload 'setnth                               "cl-compat" "" nil)
  (autoload 'setnthcdr                            "cl-compat" "" nil)
  (autoload 'setelt                               "cl-compat" "" nil)

  ;; cl-extra.el 19.34

  ;; (autoload 'cl-push                              "cl-extra" "" nil 'macro)
  ;; (autoload 'cl-pop                               "cl-extra" "" nil 'macro)
  (autoload 'coerce                               "cl-extra" "" nil)
  (autoload 'equalp                               "cl-extra" "" nil)
  (autoload 'cl-mapcar-many                       "cl-extra" "" nil)
  (autoload 'map                                  "cl-extra" "" nil)
  (autoload 'maplist                              "cl-extra" "" nil)
  (autoload 'mapc                                 "cl-extra" "" nil)
  (autoload 'mapl                                 "cl-extra" "" nil)
  (autoload 'mapcan                               "cl-extra" "" nil)
  (autoload 'mapcon                               "cl-extra" "" nil)
  (autoload 'some                                 "cl-extra" "" nil)
  (autoload 'every                                "cl-extra" "" nil)
  (autoload 'notany                               "cl-extra" "" nil)
  (autoload 'notevery                             "cl-extra" "" nil)
  (autoload 'cl-map-keymap                        "cl-extra" "" nil)
  (autoload 'cl-map-keymap-recursively            "cl-extra" "" nil)
  (autoload 'cl-map-intervals                     "cl-extra" "" nil)
  (autoload 'cl-map-overlays                      "cl-extra" "" nil)
  (autoload 'cl-set-frame-visible-p               "cl-extra" "" nil)
  (autoload 'cl-progv-before                      "cl-extra" "" nil)
  (autoload 'cl-progv-after                       "cl-extra" "" nil)
  (autoload 'gcd                                  "cl-extra" "" nil)
  (autoload 'lcm                                  "cl-extra" "" nil)
  (autoload 'isqrt                                "cl-extra" "" nil)
  (autoload 'cl-expt                              "cl-extra" "" nil)
  (autoload 'floor*                               "cl-extra" "" nil)
  (autoload 'ceiling*                             "cl-extra" "" nil)
  (autoload 'truncate*                            "cl-extra" "" nil)
  (autoload 'round*                               "cl-extra" "" nil)
  (autoload 'mod*                                 "cl-extra" "" nil)
  (autoload 'rem*                                 "cl-extra" "" nil)
  (autoload 'signum                               "cl-extra" "" nil)
  (autoload 'random*                              "cl-extra" "" nil)
  (autoload 'make-random-state                    "cl-extra" "" nil)
  (autoload 'random-state-p                       "cl-extra" "" nil)
  (autoload 'cl-finite-do                         "cl-extra" "" nil)
  (autoload 'cl-float-limits                      "cl-extra" "" nil)
  (autoload 'subseq                               "cl-extra" "" nil)
  (autoload 'concatenate                          "cl-extra" "" nil)
  (autoload 'revappend                            "cl-extra" "" nil)
  (autoload 'nreconc                              "cl-extra" "" nil)
  (autoload 'list-length                          "cl-extra" "" nil)
  (autoload 'tailp                                "cl-extra" "" nil)
  (autoload 'cl-copy-tree                         "cl-extra" "" nil)
  (autoload 'get*                                 "cl-extra" "" nil)
  (autoload 'getf                                 "cl-extra" "" nil)
  (autoload 'cl-set-getf                          "cl-extra" "" nil)
  (autoload 'cl-do-remf                           "cl-extra" "" nil)
  (autoload 'cl-remprop                           "cl-extra" "" nil)
  (autoload 'make-hash-table                      "cl-extra" "" nil)
  (autoload 'hash-table-p                         "cl-extra" "" nil)
  (autoload 'cl-not-hash-table                    "cl-extra" "" nil)
  (autoload 'cl-hash-lookup                       "cl-extra" "" nil)
  (autoload 'cl-gethash                           "cl-extra" "" nil)
  (autoload 'cl-puthash                           "cl-extra" "" nil)
  (autoload 'cl-remhash                           "cl-extra" "" nil)
  (autoload 'cl-clrhash                           "cl-extra" "" nil)
  (autoload 'cl-maphash                           "cl-extra" "" nil)
  (autoload 'hash-table-count                     "cl-extra" "" nil)
  (autoload 'cl-prettyprint                       "cl-extra" "" nil)
  (autoload 'cl-do-prettyprint                    "cl-extra" "" nil)
  (autoload 'cl-macroexpand-all                   "cl-extra" "" nil)
  (autoload 'cl-macroexpand-body                  "cl-extra" "" nil)
  (autoload 'cl-prettyexpand                      "cl-extra" "" nil)

  ;; cl-seq.el 19.34
  ;; Hm. Sometimemes you find this message:
  ;;    "Tried to load `cl-seq' before `cl'!"
  ;;
  ;; These are commented for now

  (when nil

    (autoload 'cl-push                              "cl-seq" "" nil 'macro)
    (autoload 'cl-pop                               "cl-seq" "" nil 'macro)
    (autoload 'cl-parsing-keywords                  "cl-seq" "" nil 'macro)
    (autoload 'cl-check-key                         "cl-seq" "" nil 'macro)
    (autoload 'cl-check-test-nokey                  "cl-seq" "" nil 'macro)
    (autoload 'cl-check-test                        "cl-seq" "" nil 'macro)
    (autoload 'cl-check-match                       "cl-seq" "" nil 'macro)
    (autoload 'reduce                               "cl-seq" "" nil)
    (autoload 'fill                                 "cl-seq" "" nil)
    (autoload 'replace                              "cl-seq" "" nil)
    (autoload 'remove*                              "cl-seq" "" nil)
    (autoload 'remove-if                            "cl-seq" "" nil)
    (autoload 'remove-if-not                        "cl-seq" "" nil)
    (autoload 'delete*                              "cl-seq" "" nil)
    (autoload 'delete-if                            "cl-seq" "" nil)
    (autoload 'delete-if-not                        "cl-seq" "" nil)
    (autoload 'remove                               "cl-seq" "" nil)
    (autoload 'remq                                 "cl-seq" "" nil)
    (autoload 'remove-duplicates                    "cl-seq" "" nil)
    (autoload 'delete-duplicates                    "cl-seq" "" nil)
    (autoload 'cl-delete-duplicates                 "cl-seq" "" nil)
    (autoload 'substitute                           "cl-seq" "" nil)
    (autoload 'substitute-if                        "cl-seq" "" nil)
    (autoload 'substitute-if-not                    "cl-seq" "" nil)
    (autoload 'nsubstitute                          "cl-seq" "" nil)
    (autoload 'nsubstitute-if                       "cl-seq" "" nil)
    (autoload 'nsubstitute-if-not                   "cl-seq" "" nil)
    (autoload 'find                                 "cl-seq" "" nil)
    (autoload 'find-if                              "cl-seq" "" nil)
    (autoload 'find-if-not                          "cl-seq" "" nil)
    (autoload 'position                             "cl-seq" "" nil)
    (autoload 'cl-position                          "cl-seq" "" nil)
    (autoload 'position-if                          "cl-seq" "" nil)
    (autoload 'position-if-not                      "cl-seq" "" nil)
    (autoload 'count                                "cl-seq" "" nil)
    (autoload 'count-if                             "cl-seq" "" nil)
    (autoload 'count-if-not                         "cl-seq" "" nil)
    (autoload 'mismatch                             "cl-seq" "" nil)
    (autoload 'search                               "cl-seq" "" nil)
    (autoload 'sort*                                "cl-seq" "" nil)
    (autoload 'stable-sort                          "cl-seq" "" nil)
    (autoload 'merge                                "cl-seq" "" nil)
    (autoload 'member*                              "cl-seq" "" nil)
    (autoload 'member-if                            "cl-seq" "" nil)
    (autoload 'member-if-not                        "cl-seq" "" nil)
    (autoload 'cl-adjoin                            "cl-seq" "" nil)
    (autoload 'assoc*                               "cl-seq" "" nil)
    (autoload 'assoc-if                             "cl-seq" "" nil)
    (autoload 'assoc-if-not                         "cl-seq" "" nil)
    (autoload 'rassoc*                              "cl-seq" "" nil)
    (autoload 'rassoc-if                            "cl-seq" "" nil)
    (autoload 'rassoc-if-not                        "cl-seq" "" nil)
    (autoload 'union                                "cl-seq" "" nil)
    (autoload 'nunion                               "cl-seq" "" nil)
    (autoload 'intersection                         "cl-seq" "" nil)
    (autoload 'nintersection                        "cl-seq" "" nil)
    (autoload 'set-difference                       "cl-seq" "" nil)
    (autoload 'nset-difference                      "cl-seq" "" nil)
    (autoload 'set-exclusive-or                     "cl-seq" "" nil)
    (autoload 'nset-exclusive-or                    "cl-seq" "" nil)
    (autoload 'subsetp                              "cl-seq" "" nil)
    (autoload 'subst-if                             "cl-seq" "" nil)
    (autoload 'subst-if-not                         "cl-seq" "" nil)
    (autoload 'nsubst                               "cl-seq" "" nil)
    (autoload 'nsubst-if                            "cl-seq" "" nil)
    (autoload 'nsubst-if-not                        "cl-seq" "" nil)
    (autoload 'sublis                               "cl-seq" "" nil)
    (autoload 'cl-sublis-rec                        "cl-seq" "" nil)
    (autoload 'nsublis                              "cl-seq" "" nil)
    (autoload 'cl-nsublis-rec                       "cl-seq" "" nil)
    (autoload 'tree-equal                           "cl-seq" "" nil)
    (autoload 'cl-tree-equal-rec                    "cl-seq" "" nil)

    ;; cl-indent.el 19.34

    (autoload 'common-lisp-indent-function          "cl-indent" "" nil)
    (autoload 'lisp-indent-report-bad-format        "cl-indent" "" nil)
    (autoload 'lisp-indent-259                      "cl-indent" "" nil)
    (autoload 'lisp-indent-tagbody                  "cl-indent" "" nil)
    (autoload 'lisp-indent-do                       "cl-indent" "" nil)
    (autoload 'lisp-indent-function-lambda-hack     "cl-indent" "" nil)

    ) ;; when-nil

  ;; assoc.el 20.4

  (autoload 'asort                                "assoc" "" nil)
  (autoload 'aelement                             "assoc" "" nil)
  (autoload 'aheadsym                             "assoc" "" nil)
  (autoload 'anot-head-p                          "assoc" "" nil)
  (autoload 'aput                                 "assoc" "" nil)
  (autoload 'adelete                              "assoc" "" nil)
  (autoload 'aget                                 "assoc" "" nil)
  (autoload 'amake                                "assoc" "" nil)

  ;;}}}
  ;;{{{ Backward compatible lib: tinylibb.el

  (autoload 'shell-command-to-string              "tinylibm" "" nil)
  (autoload 'describe-symbol-find-file            "tinylibb" "" nil)
  (autoload 'subst-char-with-string               "tinylibb" "" nil)
  (autoload 'subst-char-in-string                 "tinylibb" "" nil)
  (autoload 'font-lock-mode-maybe                 "tinylibb" "" nil)
  (autoload 'turn-on-font-lock-mode               "tinylibb" "" nil)
  (autoload 'turn-on-font-lock-mode-maybe         "tinylibb" "" nil)
  (autoload 'int-to-float                         "tinylibb" "" nil)
  (autoload 'logtest                              "tinylibb" "" nil)
  (autoload 'bin-string-to-int                    "tinylibb" "" nil)
  (autoload 'int-to-bin-string                    "tinylibb" "" nil)
  (autoload 'int-to-hex-string                    "tinylibb" "" nil)
  (autoload 'int-to-oct-string                    "tinylibb" "" nil)
  (autoload 'radix                                "tinylibb" "" nil)
  (autoload 'bin-to-int                           "tinylibb" "" nil)
  (autoload 'oct-to-int                           "tinylibb" "" nil)
  (autoload 'hex-to-int                           "tinylibb" "" nil)
  (autoload 'int-to-net                           "tinylibb" "" nil)
  (autoload 'rmac                                 "tinylibb" "" nil)
  (autoload 'ctime                                "tinylibb" "" nil)
  (autoload 'rand0                                "tinylibb" "" nil) ;;defsubst
  (autoload 'rand1                                "tinylibb" "" nil)
  (autoload 'randij                               "tinylibb" "" nil)
  (autoload 'byte-compiling-files-p               "tinylibb" "" nil 'macro)

  ;;}}}
  ;;{{{ code: Autoload 'env' lib -- Emacs and XEmacs environment checks

  (autoload 'defalias-maybe                       "tinylibenv" "" nil 'macro)
  (autoload 'defconst-maybe                       "tinylibenv" "" nil 'macro)
  (autoload 'defmacro-maybe                       "tinylibenv" "" nil 'macro)
  (autoload 'defsubst-maybe                       "tinylibenv" "" nil 'macro)
  (autoload 'defun-maybe                          "tinylibenv" "" nil 'macro)
  (autoload 'ti::emacs-debug-mode                 "tinylibenv" "" t)
  (autoload 'ti::emacs-install-root               "tinylibenv" "" nil)
  (autoload 'ti::emacs-install-root-emacsen       "tinylibenv" "" nil)
  (autoload 'ti::emacs-p                          "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::xemacs-p                         "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::emacs-type-cygwin-p              "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::emacs-type-unix-like-p           "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::emacs-type-win32-p               "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::emacs-version-number-as-string   "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::emacs-version-number-as-string-major "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::executable-find                  "tinylibenv" "" nil)
  (autoload 'ti::file-version                     "tinylibenv" "" nil)
  (autoload 'ti::os-check-gnu-support-p           "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::os-check-hpux-p                  "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::os-check-linux-like-p            "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::os-check-linux-p                 "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::os-check-sunos-p                 "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::turn-off-emacs-debug             "tinylibenv" "" t)
  (autoload 'ti::turn-on-emacs-debug              "tinylibenv" "" t)
  (autoload 'ti::win32-9x-p                       "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::win32-cygwin-binary-p            "tinylibenv" "" nil)
  (autoload 'ti::win32-cygwin-p                   "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::win32-cygwin-p-1                 "tinylibenv" "" nil)
  (autoload 'ti::win32-nt-p                       "tinylibenv" "" nil) ;;defsubst
  (autoload 'ti::win32-p                          "tinylibenv" "" nil)
  (autoload 'ti::win32-shell-p                    "tinylibenv" "" nil)

  ;;{{{ code: Autoload

  (autoload 'ti::function-car-test                "tinylibm" "" nil)
  (autoload 'ti::defalias-p                       "tinylibm" "" nil)
  (autoload 'ti::subrp-p                          "tinylibm" "" nil)
  (autoload 'ti::defmacro-p                       "tinylibm" "" nil)
  (autoload 'ti::autoload-p                       "tinylibm" "" nil)
  (autoload 'ti::autoload-file                    "tinylibm" "" nil)
  (autoload 'ti::lambda-p                         "tinylibm" "" nil)
  (autoload 'ti::compatibility-advice-setup       "tinylibm" "" nil)
  (autoload 'tinylibm-version                     "tinylibm" "" t)
  (autoload 'tinylibm-submit-bug-report           "tinylibm" "" t)
  (autoload 'ti::definteractive                   "tinylibm" "" t 'macro)
  (autoload 'ti::fboundp-check-autoload           "tinylibm" "" nil 'macro)
  (autoload 'ti::narrow-safe                      "tinylibm" "" nil 'macro)
  (autoload 'ti::narrow-to-paragraph              "tinylibm" "" nil 'macro)
  (autoload 'ti::nconc                            "tinylibm" "" nil 'macro)
  (autoload 'ti::consp                            "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::listp                            "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::when-package                     "tinylibm" "" nil 'macro)
  (autoload 'ti::with-require                     "tinylibm" "" nil 'macro)
  (autoload 'ti::with-time-this                   "tinylibm" "" nil 'macro)
  (autoload 'ti::with-coding-system-raw-text      "tinylibm" "" nil 'macro)
  (autoload 'ti::process-mark                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::verb                             "tinylibm" "" t 'macro)
  (autoload 'ti::pmin                             "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::pmax                             "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::dotimes                          "tinylibm" "" nil 'macro)
  (autoload 'ti::funcall                          "tinylibm" "" nil 'macro)
  (autoload 'ti::string-value                     "tinylibm" "" nil)
  (autoload 'ti::prin1-mapconcat                  "tinylibm" "" nil)
  (autoload 'ti::d!                               "tinylibm" "" nil 'macro)
  (autoload 'ti::d!!                              "tinylibm" "" nil 'macro)
  (autoload 'ti::string-left                      "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::string-right                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::string-match-case                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::month-list                       "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::month-list-regexp                "tinylibm" "" nil)
  (autoload 'ti::month-mm-alist                   "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::month-nn-alist                   "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::month-to-number                  "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::month-to-0number                 "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::number-to-month                  "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::date-eu-list                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::date-us-list                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::date-list-regexp                 "tinylibm" "" nil)
  (autoload 'ti::read-char-safe                   "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::read-char-safe-until             "tinylibm" "" nil)
  (autoload 'ti::remove-properties                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::applycar                             "tinylibm" "" nil 'macro)
  (autoload 'ti::add-command-line-arg             "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::buffer-modified-p                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::first-line-p                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::last-line-p                      "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::buffer-narrowed-p                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::buffer-empty-p                   "tinylibm" "" nil)
  (autoload 'ti::ck-maybe-activate                "tinylibm" "" nil)
  (autoload 'ti::register-live-p                  "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-dos-p                       "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::space-p                          "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::compat-face-p                    "tinylibm" "" nil)
  (autoload 'ti::color-type                       "tinylibm" "" nil)
  (autoload 'ti::colors-supported-p               "tinylibm" "" nil)
  (autoload 'ti::overlay-supported-p              "tinylibm" "" nil)
  (autoload 'ti::idle-timer-supported-p           "tinylibm" "" nil)
  (autoload 'ti::replace-match                    "tinylibm" "" nil)
  (autoload 'ti::buffer-kill-control-characters   "tinylibm" "" t) ;;defsubst
  (autoload 'ti::string-match                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::buffer-match                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::selective-display-line-p         "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::bool-p                           "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::print-p                          "tinylibm" "" nil 'macro)
  (autoload 'ti::char-case-p                      "tinylibm" "" nil)
  (autoload 'ti::nil-p                            "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::window-pmin-visible-p            "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::window-pmax-visible-p            "tinylibm" "" nil 'macro)
  (autoload 'ti::window-pmax-line-p               "tinylibm" "" nil)
  (autoload 'ti::window-pmin-line-p               "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::window-pmax-line-bol             "tinylibm" "" nil)
  (autoload 'ti::window-middle-line               "tinylibm" "" nil)
  (autoload 'ti::no-action-in-progress-p          "tinylibm" "" nil)
  (autoload 'ti::current-line-number              "tinylibm" "" nil)
  (autoload 'ti::read-current-line                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::line-length                      "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::line-wrap-p                      "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::re-search-check                  "tinylibm" "" nil)
  (autoload 'ti::re-search-point-list             "tinylibm" "" nil)
  (autoload 'ti::assoc-append-inside              "tinylibm" "" nil 'macro)
  (autoload 'ti::assoc-replace-maybe-add          "tinylibm" "" nil)
  (autoload 'ti::char-in-list-case                "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::list-make                        "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::list-flatten                     "tinylibm" "" nil)
  (autoload 'ti::list-join                        "tinylibm" "" nil)
  (autoload 'ti::list-to-assoc-menu               "tinylibm" "" nil)
  (autoload 'ti::list-to-cons                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::list-remove-successive           "tinylibm" "" nil)
  (autoload 'ti::list-merge-elements              "tinylibm" "" nil)
  (autoload 'ti::list-print                       "tinylibm" "" t)
  (autoload 'ti::list-to-string                   "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::list-elt-position                "tinylibm" "" nil)
  (autoload 'ti::list-find                        "tinylibm" "" nil)
  (autoload 'ti::non-dedicated-frame              "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::select-frame-non-dedicated       "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::byte-compile-defun-compiled-p    "tinylibm" "" nil 'macro)
  (autoload 'ti::byte-compile-defun-maybe         "tinylibm" "" nil 'macro)
  (autoload 'ti::package-use-dynamic-compilation  "tinylibm" "" nil 'macro)
  (autoload 'ti::function-autoload-file           "tinylibm" "" nil)
  (autoload 'ti::package-require-for-emacs        "tinylibm" "" nil 'macro)
  (autoload 'ti::package-require-view             "tinylibm" "" nil 'macro)
  (autoload 'ti::package-package-require-timer    "tinylibm" "" nil 'macro)
  (autoload 'ti::package-require-mail-abbrevs     "tinylibm" "" nil 'macro)
  (autoload 'ti::use-file-compression             "tinylibm" "" nil 'macro)
  (autoload 'ti::use-file-compression-maybe       "tinylibm" "" nil)
  (autoload 'ti::push-definition                  "tinylibm" "" nil)
  (autoload 'ti::pop-definition                   "tinylibm" "" nil)
  (autoload 'ti::use-prefix-key                   "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::swap-keys-if-not-keymap          "tinylibm" "" nil)
  (autoload 'ti::define-buffer-local-keymap       "tinylibm" "" nil 'macro)
  (autoload 'ti::define-key-if-free               "tinylibm" "" nil 'macro)
  (autoload 'ti::define-in-function-keymap        "tinylibm" "" nil)
  (autoload 'ti::copy-key-definition              "tinylibm" "" nil 'macro)
  (autoload 'ti::beginning-of-defun-point         "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::digit-length                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::add-hook-fix                     "tinylibm" "" nil)
  (autoload 'ti::add-hooks                        "tinylibm" "" nil)
  (autoload 'ti::prefix-arg-to-text               "tinylibm" "" nil)
  (autoload 'ti::keep-lower-order                 "tinylibm" "" nil 'macro)
  (autoload 'ti::bool-toggle                      "tinylibm" "" nil 'macro)
  (autoload 'ti::compat-load-user-init-file       "tinylibm" "" nil 'macro)
  (autoload 'ti::compat-Info-directory-list-symbol "tinylibm" "" nil) ;; defsubst
  (autoload 'ti::compat-Info-directory-list       "tinylibm" "" nil) ;; defsubst
  (autoload 'ti::buffer-pointer-of-info           "tinylibm" "" nil)
  (autoload 'ti::buffer-pointer-of-messages       "tinylibm" "" nil)
  (autoload 'ti::last-message-line                "tinylibm" "" nil)
  (autoload 'ti::dolist-buffer-list               "tinylibm" "" nil 'macro)
  (autoload 'ti::erase-buffer                     "tinylibm" "" nil)
  (autoload 'ti::temp-buffer                      "tinylibm" "" nil)
  (autoload 'ti::append-to-buffer                 "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::set-buffer-safe                  "tinylibm" "" nil)
  (autoload 'ti::kill-buffer-safe                 "tinylibm" "" nil)
  (autoload 'cl-clrhash-paranoid                  "tinylibm" "" nil)
  (autoload 'ti::vector-table-init                "tinylibm" "" nil 'macro)
  (autoload 'ti::vector-table-get                 "tinylibm" "" nil 'macro)
  (autoload 'ti::vector-table-property            "tinylibm" "" nil)
  (autoload 'ti::vector-table-clear               "tinylibm" "" nil 'macro)
  (autoload 'ti::expand-file-name-tilde-in-string "tinylibm" "" nil)
  (autoload 'ti::file-name-path-p                 "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-name-path-absolute-p        "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::directory-move                   "tinylibm" "" nil)
  (autoload 'ti::write-file-with-wrapper          "tinylibm" "" nil)
  (autoload 'ti::load-file-with-wrapper           "tinylibm" "" nil 'macro)
  (autoload 'ti::write-file-as-is-macro           "tinylibm" "" nil 'macro)
  (autoload 'ti::directory-list                   "tinylibm" "" nil)
  (autoload 'ti::directory-recursive-macro        "tinylibm" "" nil 'macro)
  (autoload 'ti::file-name-remote-p               "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-name-backward-slashes       "tinylibm" "" nil)
  (autoload 'ti::file-name-forward-slashes        "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-name-forward-slashes-cygwin "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-changed-on-disk-p           "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-mode-make-read-only         "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-mode-make-read-only-all     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-mode-make-writable          "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-mode-make-executable        "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-mode-protect                "tinylibm" "" t) ;;defsubst
  (autoload 'ti::file-toggle-read-write           "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-owned-p                     "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-modify-p                    "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-find-file-p                 "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-read-only-p                 "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::file-name-run-real-handler       "tinylibm" "" nil)
  (autoload 'ti::find-file-literally              "tinylibm" "" t)
  (autoload 'ti::file-eval                        "tinylibm" "" nil)
  (autoload 'ti::directory-writable-p             "tinylibm" "" nil)
  (autoload 'ti::file-delete-safe                 "tinylibm" "" nil)
  (autoload 'ti::temp-directory                   "tinylibm" "" nil)
  (autoload 'ti::temp-file                        "tinylibm" "" nil)
  (autoload 'ti::pop-to-buffer-or-window          "tinylibm" "" nil)
  (autoload 'ti::find-file-or-window              "tinylibm" "" nil)
  (autoload 'ti::mouse-point                      "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::i-macro-region-ask               "tinylibm" "" nil) ;;defsubst
  (autoload 'ti::i-macro-region-body              "tinylibm" "" nil 'macro)
  (autoload 'ti::with-unix-shell-environment      "tinylibm" "" nil 'macro)
  (autoload 'ti::package-defgroup-tiny            "tinylibm" "" nil 'macro)
  (autoload 'ti::package-tiny-defgroup-mail       "tinylibm" "" nil)
  (autoload 'ti::grep-output-parse-macro          "tinylibm" "" nil 'macro)
  (autoload 'ti::occur-macro                      "tinylibm" "" nil 'macro)
  (autoload 'ti::momentary-output-macro           "tinylibm" "" nil 'macro)
  (autoload 'ti::save-excursion-macro             "tinylibm" "" nil 'macro)
  (autoload 'ti::save-with-marker-macro           "tinylibm" "" nil 'macro)
  (autoload 'ti::save-line-column-macro           "tinylibm" "" nil 'macro)
  (autoload 'ti::widen-safe                       "tinylibm" "" nil 'macro)
  (autoload 'ti::package-config-file-directory-default "tinylibm" "" nil)
  (autoload 'ti::package-config-file-prefix       "tinylibm" "" nil)
  (autoload 'ti::overlay-require-macro            "tinylibm" "" nil 'macro)
  (autoload 'ti::pp-variable-list                 "tinylibm" "" nil)
  (autoload 'ti::write-file-variable-state        "tinylibm" "" nil)

;;; tinylib.el

  (autoload 'tinylib-version                      "tinylib" "" t)
  (autoload 'tinylib-submit-feedback              "tinylib" "" t)
  (autoload 'ti::string-trim-blanks               "tinylib" "" nil)
  (autoload 'ti::string-verify-ends               "tinylib" "" nil)
  (autoload 'ti::string-add-space                 "tinylib" "" nil) ;;defsubst
  (autoload 'ti::string-remove-whitespace         "tinylib" "" nil)
  (autoload 'ti::string-mangle                    "tinylib" "" nil)
  (autoload 'ti::string-regexp-delete             "tinylib" "" nil) ;;defsubst
  (autoload 'ti::string-format-percent            "tinylib" "" nil)
  (autoload 'ti::string-url-to-ange-ftp           "tinylib" "" nil)
  (autoload 'ti::buffer-backslash-fix-paragraph   "tinylib" "" t)
  (autoload 'ti::buffer-upcase-words-to-variable-names "tinylib" "" t)
  (autoload 'ti::string-nth-from-number           "tinylib" "" nil) ;;defsubst
  (autoload 'ti::date-time-elements               "tinylib" "" t)
  (autoload 'ti::string-char-to-escape-char       "tinylib" "" nil)
  (autoload 'ti::string-plain-string-to-regexp    "tinylib" "" nil)
  (autoload 'ti::file-access-mode-to-string       "tinylib" "" nil)
  (autoload 'ti::file-name-for-correct-system     "tinylib" "" nil)
  (autoload 'ti::vc-rcs-delta-get-revisions       "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-delta-get-file            "tinylib" "" nil)
  (autoload 'ti::vc-rcs-delta-lock-status         "tinylib" "" nil)
  (autoload 'ti::vc-rcs-delta-lock-status-user    "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-delta-highest-version     "tinylib" "" t) ;;defsubst
  (autoload 'ti::vc-rcs-read-val                  "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-look-id                   "tinylib" "" nil)
  (autoload 'ti::vc-cvs-to-cvs-dir                "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-cvs-to-cvs-dir-p              "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-cvs-to-cvs-file               "tinylib" "" nil)
  (autoload 'ti::vc-cvs-to-cvs-file-content       "tinylib" "" nil)
  (autoload 'ti::vc-cvs-file-exists-p             "tinylib" "" nil)
  (autoload 'ti::vc-cvs-entry-split               "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-cvs-entry-type                "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-cvs-entry-split-info          "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-file-p                    "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-make-filename             "tinylib" "" nil)
  (autoload 'ti::vc-rcs-file-exists-p             "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-normal-file               "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-sort-same-level-list      "tinylib" "" nil)
  (autoload 'ti::vc-rcs-files-in-dir              "tinylib" "" nil)
  (autoload 'ti::vc-rcs-head-version              "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-guess-buffer-version      "tinylib" "" nil)
  (autoload 'ti::vc-rcs-buffer-version            "tinylib" "" nil)
  (autoload 'ti::vc-rcs-rlog-get-revisions        "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-all-versions              "tinylib" "" nil) ;;defsubst
  (autoload 'ti::vc-rcs-previous-version          "tinylib" "" nil)
  (autoload 'ti::vc-rcs-get-all-branches          "tinylib" "" nil)
  (autoload 'ti::vc-version-string-p              "tinylib" "" nil)
  (autoload 'ti::vc-version-simple-p              "tinylib" "" nil)
  (autoload 'ti::vc-version-lessp                 "tinylib" "" nil)
  (autoload 'ti::vc-rcs-str-find                  "tinylib" "" nil)
  (autoload 'ti::vc-rcs-str-find-buffer           "tinylib" "" nil) ;;defsubst
  (autoload 'ti::date-standard-rfc-regexp         "tinylib" "" nil)
  (autoload 'ti::date-standard-date               "tinylib" "" t)
  (autoload 'ti::date-month-to-number             "tinylib" "" t)
  (autoload 'ti::date-time-difference             "tinylib" "" nil)
  (autoload 'ti::date-time-diff-days              "tinylib" "" nil)
  (autoload 'ti::date-parse-date                  "tinylib" "" nil)
  (autoload 'ti::string-repeat                    "tinylib" "" nil)
  (autoload 'ti::string-syntax-info               "tinylib" "" t)
  (autoload 'ti::string-syntax-kill-double-quote  "tinylib" "" t)
  (autoload 'ti::string-tabify                    "tinylib" "" nil)
  (autoload 'ti::string-match-string-subs         "tinylib" "" nil)
  (autoload 'ti::string-match-string-list         "tinylib" "" nil)
  (autoload 'ti::string-case-replace              "tinylib" "" nil)
  (autoload 'ti::string-index                     "tinylib" "" nil)
  (autoload 'ti::string-index-substring           "tinylib" "" nil)
  (autoload 'ti::string-replace-one-space                   "tinylib" "" nil)
  (autoload 'ti::string-listify                   "tinylib" "" nil)
  (autoload 'ti::dired-buffer                     "tinylib" "" nil)
  (autoload 'ti::buffer-get-ange-buffer-list      "tinylib" "" nil) ;;defsubst
  (autoload 'ti::buffer-find-ange-buffer          "tinylib" "" nil)
  (autoload 'ti::buffer-find-ange-to-dired-buffer "tinylib" "" nil)
  (autoload 'ti::buffer-uu-area                   "tinylib" "" nil)
  (autoload 'ti::buffer-uu-line-p                 "tinylib" "" t)
  (autoload 'ti::buffer-area-bounds               "tinylib" "" nil)
  (autoload 'ti::buffer-parse-grep-line           "tinylib" "" nil)
  (autoload 'ti::buffer-parse-grep-line2          "tinylib" "" nil)
  (autoload 'ti::buffer-parse-line-main           "tinylib" "" nil)
  (autoload 'ti::buffer-join-region               "tinylib" "" t)
  (autoload 'ti::buffer-read-if-solid             "tinylib" "" nil)
  (autoload 'ti::buffer-read-whitespace           "tinylib" "" nil)
  (autoload 'ti::buffer-read-line                 "tinylib" "" nil)
  (autoload 'ti::buffer-grep-lines                "tinylib" "" nil)
  (autoload 'ti::buffer-looking-back-at           "tinylib" "" nil)
  (autoload 'ti::buffer-read-char                 "tinylib" "" nil)
  (autoload 'ti::buffer-read-word                 "tinylib" "" nil)
  (autoload 'ti::buffer-read-space-word           "tinylib" "" nil)
  (autoload 'ti::buffer-read-syntax-word          "tinylib" "" nil)
  (autoload 'ti::buffer-read-nth-word             "tinylib" "" nil)
  (autoload 'ti::buffer-replace-keywords-with-table "tinylib" "" t)
  (autoload 'ti::buffer-replace-region-with       "tinylib" "" nil) ;;defsubst
  (autoload 'ti::buffer-zap-to-regexp             "tinylib" "" t)
  (autoload 'ti::buffer-leave-nth-word            "tinylib" "" t)
  (autoload 'ti::buffer-kill-line                 "tinylib" "" t)
  (autoload 'ti::buffer-strip-control-m           "tinylib" "" nil)
  (autoload 'ti::buffer-lf-to-crlf                "tinylib" "" t)
  (autoload 'ti::buffer-arrow-control             "tinylib" "" nil)
  (autoload 'ti::buffer-insert-line-numbers       "tinylib" "" t)
  (autoload 'ti::buffer-remove-line-numbers       "tinylib" "" t) ;;defsubst
  (autoload 'ti::buffer-randomize-lines           "tinylib" "" t)
  (autoload 'ti::buffer-make-dup-line             "tinylib" "" t)
  (autoload 'ti::buffer-inc-string-nbr            "tinylib" "" t)
  (autoload 'ti::buffer-copy-line-and-inc-numbers "tinylib" "" t)
  (autoload 'ti::buffer-copy-word                 "tinylib" "" t)
  (autoload 'ti::buffer-add-newlines-to-region    "tinylib" "" t)
  (autoload 'ti::buffer-cnv-empty-lines           "tinylib" "" t)
  (autoload 'ti::buffer-del-dup-lines             "tinylib" "" t)
  (autoload 'ti::buffer-delete-until-non-empty-line "tinylib" "" t)
  (autoload 'ti::buffer-trim-blanks               "tinylib" "" t)
  (autoload 'ti::buffer-replace-regexp            "tinylib" "" nil)
  (autoload 'ti::buffer-diff-type-p               "tinylib" "" nil)
  (autoload 'ti::buffer-outline-widen         "tinylib" "" t)
  (autoload 'ti::buffer-buffer-list-files         "tinylib" "" nil)
  (autoload 'ti::buffer-count-words               "tinylib" "" t)
  (autoload 'ti::buffer-count-chars-in-delimited-area "tinylib" "" t)
  (autoload 'ti::buffer-word-move                 "tinylib" "" t)
  (autoload 'ti::buffer-find-duplicate-same-word  "tinylib" "" t)
  (autoload 'ti::buffer-move-paragraph-to-column  "tinylib" "" t)
  (autoload 'ti::buffer-move-to-col               "tinylib" "" t) ;;defsubst
  (autoload 'ti::buffer-selective-display-copy-to "tinylib" "" t)
  (autoload 'ti::buffer-selective-display-print   "tinylib" "" t)
  (autoload 'ti::window-frame-list                "tinylib" "" nil)
  (autoload 'ti::window-list                      "tinylib" "" nil)
  (autoload 'ti::window-single-p                  "tinylib" "" nil) ;;defsubst
  (autoload 'ti::window-get-buffer-window-other-frame "tinylib" "" nil)
  (autoload 'ti::window-find-bottom               "tinylib" "" nil)
  (autoload 'ti::window-match-buffers             "tinylib" "" nil)
  (autoload 'ti::keymap-single-key-definition-p   "tinylib" "" nil)
  (autoload 'ti::keymap-define-key-backspace      "tinylib" "" t)
  (autoload 'ti::keymap-function-bind-info        "tinylib" "" nil)
  (autoload 'ti::keymap-reinstall-minor-mode      "tinylib" "" nil)
  (autoload 'ti::keymap-add-minor-mode            "tinylib" "" nil)
  (autoload 'ti::keymap-bind-control              "tinylib" "" nil)
  (autoload 'ti::keymap-translate-table                "tinylib" "" nil)
  (autoload 'ti::keymap-put-abc-map               "tinylib" "" nil)
  (autoload 'ti::keymap-put-map                   "tinylib" "" nil)
  (autoload 'ti::keymap-mapkeys                   "tinylib" "" nil)
  (autoload 'ti::buffer-text-properties-wipe      "tinylib" "" t)
  (autoload 'ti::set-face-try-list                "tinylib" "" nil)
  (autoload 'ti::buffer-forward-line              "tinylib" "" t) ;;defsubst
  (autoload 'ti::buffer-surround-with-char        "tinylib" "" t)
  (autoload 'ti::buffer-fill-region-spaces        "tinylib" "" t)
  (autoload 'ti::buffer-quote-words-in-region     "tinylib" "" t)
  (autoload 'ti::buffer-find-longer-line          "tinylib" "" nil)
  (autoload 'ti::buffer-scramble-region           "tinylib" "" t)
  (autoload 'ti::buffer-add-string-region         "tinylib" "" t)
  (autoload 'ti::buffer-sort-regexp-fields        "tinylib" "" nil)
  (autoload 'ti::file-passwd-grep-user-alist      "tinylib" "" nil)
  (autoload 'ti::file-passwd-build-alist          "tinylib" "" nil)
  (autoload 'ti::file-passwd-read-entry           "tinylib" "" nil)
  (autoload 'ti::buffer-defun-function-name       "tinylib" "" nil)
  (autoload 'ti::file-days-old                    "tinylib" "" nil) ;;defsubst
  (autoload 'ti::file-touch                       "tinylib" "" nil)
  (autoload 'ti::file-ange-completed-message      "tinylib" "" nil)
  (autoload 'ti::file-ange-status                 "tinylib" "" nil)
  (autoload 'ti::file-ange-download-file          "tinylib" "" nil)
  (autoload 'ti::file-ange-file-handle            "tinylib" "" nil)
  (autoload 'ti::file-chmod-w-toggle              "tinylib" "" nil)
  (autoload 'ti::file-chmod-make-writable         "tinylib" "" nil)
  (autoload 'ti::file-chmod-make-read-only        "tinylib" "" nil)
  (autoload 'ti::file-find-shadows                "tinylib" "" t)
  (autoload 'ti::directory-part-last              "tinylib" "" nil)
  (autoload 'ti::directory-unique-roots           "tinylib" "" nil)
  (autoload 'ti::directory-subdirectory-list      "tinylib" "" nil)
  (autoload 'ti::directory-recursive-do           "tinylib" "" nil)
  (autoload 'ti::directory-up                     "tinylib" "" nil)
  (autoload 'ti::directory-subdirs                "tinylib" "" nil)
  (autoload 'ti::directory-unix-man-path-root     "tinylib" "" nil)
  (autoload 'ti::directory-files                  "tinylib" "" nil)
  (autoload 'ti::file-files-only                  "tinylib" "" nil)
  (autoload 'ti::file-newer-exist                 "tinylib" "" nil)
  (autoload 'ti::file-get-extension               "tinylib" "" nil)
  (autoload 'ti::file-path-and-line-info          "tinylib" "" nil)
  (autoload 'ti::file-path-to-unix                "tinylib" "" nil)
  (autoload 'ti::file-path-to-msdos               "tinylib" "" nil)
  (autoload 'ti::file-make-path                   "tinylib" "" nil)
  (autoload 'ti::file-get-load-path               "tinylib" "" t)
  (autoload 'ti::file-user-home                   "tinylib" "" nil)
  (autoload 'ti::file-file-list                   "tinylib" "" nil)
  (autoload 'ti::file-complete-file-name          "tinylib" "" nil)
  (autoload 'ti::file-complete-file-name-word     "tinylib" "" t)
  (autoload 'ti::file-complete-filename-minibuffer-macro "tinylib" "" t 'macro)
  (autoload 'ti::file-read-file-list              "tinylib" "" nil)
  (autoload 'ti::process-finger-error             "tinylib" "" nil)
  (autoload 'ti::process-finger                   "tinylib" "" t)
  (autoload 'ti::process-http-request             "tinylib" "" t)
  (autoload 'ti::process-uname                    "tinylib" "" nil)
  (autoload 'ti::process-zip                      "tinylib" "" t)
  (autoload 'ti::process-zip-view-command         "tinylib" "" t)
  (autoload 'ti::process-tar-zip-view-maybe-command "tinylib" "" nil)
  (autoload 'ti::process-perl-process-environment-macro "tinylib" "" nil 'macro)
  (autoload 'ti::process-perl-version             "tinylib" "" nil)
  (autoload 'ti::process-java-version             "tinylib" "" nil)
  (autoload 'ti::process-tar-view-command         "tinylib" "" t)
  (autoload 'ti::process-tar-read-listing-forward "tinylib" "" nil)
  (autoload 'ti::query-read-input-invisible       "tinylib" "" nil)
  (autoload 'ti::query-read-input-as-password     "tinylib" "" nil)
  (autoload 'ti::advice-control                   "tinylib" "" nil)
  (autoload 'ti::package-submit-feedback          "tinylib" "" t)
  (autoload 'ti::package-submit-bug-report        "tinylib" "" t)
  (autoload 'ti::package-version-info             "tinylib" "" t)
  (autoload 'ti::package-get-header               "tinylib" "" nil)
  (autoload 'ti::package-install-example          "tinylib" "" t)
  (autoload 'ti::package-rip                      "tinylib" "" t)
  (autoload 'ti::package-rip-magic                "tinylib" "" t)
  (autoload 'ti::package-make-mode-magic          "tinylib" "" t)
  (autoload 'ti::package-make-mode                "tinylib" "" t)
  (autoload 'ti::package-make-var                 "tinylib" "" nil)
  (autoload 'ti::package-make                     "tinylib" "" nil)
  (autoload 'ti::package-autoload-create-on-file  "tinylib" "" t)
  (autoload 'ti::package-autoload-create-on-directory "tinylib" "" nil)
  (autoload 'ti::package-autoload-loaddefs-create-maybe "tinylib" "" nil)
  (autoload 'ti::package-autoload-loaddefs-dir-files "tinylib" "" nil)
  (autoload 'ti::package-autoload-loaddefs-build-dir-1 "tinylib" "" nil)
  (autoload 'ti::package-autoload-loaddefs-build-dir "tinylib" "" nil)
  (autoload 'ti::package-autoload-directories     "tinylib" "" nil)
  (autoload 'ti::package-autoload-loaddefs-build-recursive "tinylib" "" t)
  (autoload 'ti::package-install-pgp-tar          "tinylib" "" t)
  (autoload 'ti::compat-installation-root         "tinylib" "" nil)
  (autoload 'ti::compat-overlay-some              "tinylib" "" nil)
  (autoload 'ti::compat-overlay-properties        "tinylib" "" nil)
  (autoload 'ti::compat-overlays-at               "tinylib" "" nil)
  (autoload 'ti::compat-overlay-put               "tinylib" "" nil)
  (autoload 'ti::compat-overlay-move              "tinylib" "" nil)
  (autoload 'ti::compat-activate-region           "tinylib" "" nil)
  (autoload 'ti::compat-read-password             "tinylib" "" nil)
  (autoload 'ti::compat-key-local-map             "tinylib" "" nil)
  (autoload 'ti::compat-key-call-original         "tinylib" "" nil)
  (autoload 'ti::compat-mouse-position-coordinates "tinylib" "" nil)
  (autoload 'ti::compat-mouse-key                 "tinylib" "" nil)
  (autoload 'ti::compat-mouse-call-original-function "tinylib" "" nil)
  (autoload 'ti::compat-mouse-call-original       "tinylib" "" t)
  (autoload 'ti::compat-popup                     "tinylib" "" t)
  (autoload 'ti::compat-display-depth             "tinylib" "" nil)
  (autoload 'ti::compat-read-event                "tinylib" "" nil)
  (autoload 'ti::compat-executing-macro           "tinylib" "" nil)
  (autoload 'ti::compat-make-x-popup-event        "tinylib" "" nil)
  (autoload 'ti::compat-make-fake-event           "tinylib" "" nil)
  (autoload 'ti::compat-modeline-update           "tinylib" "" nil)
  (autoload 'ti::compat-set-frame-parameter       "tinylib" "" t)
  (autoload 'ti::compat-set-frame-name            "tinylib" "" nil)
  (autoload 'ti::compat-frame-window-config       "tinylib" "" nil)
  (autoload 'ti::compat-window-system             "tinylib" "" nil)
  (autoload 'ti::compat-timer-list-control        "tinylib" "" nil)
  (autoload 'ti::compat-timer-control             "tinylib" "" nil)
  (autoload 'ti::compat-timer-elt                 "tinylib" "" nil)
  (autoload 'ti::compat-timer-process-status      "tinylib" "" nil)
  (autoload 'ti::compat-timer-cancel              "tinylib" "" nil)
  (autoload 'ti::compat-timer-cancel-function     "tinylib" "" nil)
  (autoload 'ti::compat-set-mode-line-format      "tinylib" "" nil)
  (autoload 'ti::macrov-minor-mode                "tinylib" "" nil 'macro)
  (autoload 'ti::macrov-minor-mode-1              "tinylib" "" nil)
  (autoload 'ti::macrof-minor-mode                "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-minor-mode-1              "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-on             "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-off            "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-help           "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-commentary     "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-viper-attach   "tinylib" "" t)
  (autoload 'ti::macrof-minor-mode-install        "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-minor-mode-install-1      "tinylib" "" t)
  (autoload 'ti::macrof-define-keys               "tinylib" "" nil 'macro)
  (autoload 'ti::macrov-mode-line-mode-menu       "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-define-keys-1             "tinylib" "" nil)
  (autoload 'ti::macrof-version-bug-report-1      "tinylib" "" t)
  (autoload 'ti::macrof-version-bug-report        "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-debug-1                   "tinylib" "" t)
  (autoload 'ti::macrof-debug-lowlevel            "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-debug-standard            "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-install-pgp-tar-1         "tinylib" "" t)
  (autoload 'ti::macrof-install-pgp-tar           "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-minor-mode-wizard         "tinylib" "" nil 'macro)
  (autoload 'ti::macrof-minor-mode-wizard-1       "tinylib" "" nil)

  ;;}}}
  ;;{{{ code: Autoload 'mt' lib -- mail tools

  (autoload 'ti::mail-pgp-signature-begin-line    "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-signature-end-line      "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-signed-begin-line       "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-signed-end-line         "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-pkey-begin-line         "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-pkey-end-line           "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-msg-begin-line          "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-msg-end-line            "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-any-pgp-line-regexp     "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-ip-raw-p                    "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-ip-top-level-domain         "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-ip-3-level-domain           "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-ip-cleanup                  "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-ip-at-point-1               "tinylibmail" "" nil)
  (autoload 'ti::mail-ip-at-point                 "tinylibmail" "" nil)
  (autoload 'ti::mail-news-group                  "tinylibmail" "" nil) ;;defsubst
  (autoload 'tinylibmail-version                  "tinylibmail" "" t)
  (autoload 'tinylibmail-submit-feedback          "tinylibmail" "" t)
  (autoload 'ti::mail-signature-p                 "tinylibmail" "" nil)
  (autoload 'ti::mail-body-empty-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-body-clear                  "tinylibmail" "" nil)
  (autoload 'ti::mail-set-region                  "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-point-in-header-macro       "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-message-length              "tinylibmail" "" nil)
  (autoload 'ti::mail-get-2re                     "tinylibmail" "" nil)
  (autoload 'ti::mail-required-headers            "tinylibmail" "" nil)
  (autoload 'ti::mail-mail-mode-p                 "tinylibmail" "" nil)
  (autoload 'ti::mail-mailbox-p                   "tinylibmail" "" nil)
  (autoload 'ti::mail-mail-p                      "tinylibmail" "" nil)
  (autoload 'ti::mail-header-area-size            "tinylibmail" "" nil)
  (autoload 'ti::mail-hmax                        "tinylibmail" "" nil)
  (autoload 'ti::mail-text-start                  "tinylibmail" "" nil)
  (autoload 'ti::mail-point-at-header-p           "tinylibmail" "" nil)
  (autoload 'ti::mail-point-at-body-p             "tinylibmail" "" nil)
  (autoload 'ti::mail-narrow                      "tinylibmail" "" nil)
  (autoload 'ti::mail-mail-buffer-name            "tinylibmail" "" nil)
  (autoload 'ti::mail-generate-buffer-name        "tinylibmail" "" t)
  (autoload 'ti::mail-mail-simple-p               "tinylibmail" "" nil)
  (autoload 'ti::mail-to-list-p                   "tinylibmail" "" nil)
  (autoload 'ti::mail-vm-macro                    "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mh-macro                    "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-gnus-macro                  "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-rmail-macro                 "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-rmail-do-message-macro      "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-rmail-copy-message          "tinylibmail" "" t)
  (autoload 'ti::mail-pgp-v3xx-p                  "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-p                       "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signed-conventional-p   "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signature-detached-p    "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signed-conventional-multi-p "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signed-xpgp-p           "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signed-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-public-key-p            "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-remail-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-comment-file-p          "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-encrypted-p             "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-normal-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-headers-p               "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-re                      "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-block-area-kill-forward "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-block-area              "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-re-search               "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-exe-version-string      "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-data-type               "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-trim-buffer             "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-chop-region             "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-header-kill-in-body     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-data-char-to-int        "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-data-string-to-bin-string "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-data-bin-string-to-int-list "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-data-ascii-armor-convert "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-data-study-ctb-byte     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-study-1-ver      "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-pgp-stream-study-1-key-id   "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-pgp-stream-study-1-time     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-study-enc        "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-study-signed     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-study-pring      "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-study            "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-forward-xpgp     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-forward          "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-forward-and-study "tinylibmail" "" t)
  (autoload 'ti::mail-pgp-stream-forward-info     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-stream-data-elt         "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpk-id-lines-in-region     "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpk-id-0x-lines-in-region  "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpk-public-get-region      "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signature-remove        "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signature-normal-do-region "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-get-article-buffer          "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-with-article-buffer         "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-pgp-signature-normal-info   "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-sig-header-info-v2xx    "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signature-header-info-v3xx "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-signature-header-info   "tinylibmail" "" nil)
  (autoload 'ti::mail-mime-parse-header           "tinylibmail" "" nil)
  (autoload 'ti::mail-pgp-pkey-read               "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-close                  "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-anonymize-headers      "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-reply-type             "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-block                  "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-reply-block            "tinylibmail" "" nil)
  (autoload 'ti::mail-pgpr-parse-levien-list      "tinylibmail" "" nil)
  (autoload 'ti::mail-email-make-anti-spam-address "tinylibmail" "" nil)
  (autoload 'ti::mail-email-domain                "tinylibmail" "" nil)
  (autoload 'ti::mail-email-domain-canonilize     "tinylibmail" "" nil)
  (autoload 'ti::mail-email-find-region           "tinylibmail" "" nil)
  (autoload 'ti::mail-email-from-string           "tinylibmail" "" nil)
  (autoload 'ti::mail-test-parse-name             "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-name                  "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-email                 "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-received-regexp-list  "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-received-line         "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-received-string-smtp  "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-received-string-clean "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-received-string-from  "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-received-string-by    "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-received-string-smtp-id "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-received-string-for   "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-received-string-date  "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-parse-date-string           "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-date-string-iso8601   "tinylibmail" "" t)
  (autoload 'ti::mail-parse-received-string       "tinylibmail" "" nil)
  (autoload 'ti::mail-parse-received              "tinylibmail" "" nil)
  (autoload 'ti::with-mail-received-header        "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-whois-parse-cleanup         "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-paragraph       "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-referral        "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-email           "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-paragraph-end-condition "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-whois-parse-registrant-1    "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-registrant-organization "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-registrant-organization-2 "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-registrant-domain "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-registrant      "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-tech            "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-zone            "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-records         "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-servers         "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse-admin           "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-error-p               "tinylibmail" "" nil)
  (autoload 'ti::mail-whois-parse                 "tinylibmail" "" nil)
  (autoload 'ti::with-mail-whois                  "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-whois                       "tinylibmail" "" nil)
  (autoload 'ti::mail-nslookup-parse              "tinylibmail" "" nil)
  (autoload 'ti::mail-nslookup                    "tinylibmail" "" nil)
  (autoload 'ti::with-mail-nslookup               "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-dig                         "tinylibmail" "" nil)
  (autoload 'ti::mail-get-buffer                  "tinylibmail" "" nil)
  (autoload 'ti::mail-signature-insert-break      "tinylibmail" "" nil)
  (autoload 'ti::mail-yank                        "tinylibmail" "" nil)
  (autoload 'ti::mail-trim-buffer                 "tinylibmail" "" nil)
  (autoload 'ti::mail-field-space-count           "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-field-start                 "tinylibmail" "" nil)
  (autoload 'ti::mail-next-field-start            "tinylibmail" "" nil)
  (autoload 'ti::mail-field-string-wrap           "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-field-string-p              "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-field-line-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-field-read-line-at-point    "tinylibmail" "" nil)
  (autoload 'ti::mail-field-read-fuzzy            "tinylibmail" "" nil)
  (autoload 'ti::mail-current-field-name          "tinylibmail" "" nil)
  (autoload 'ti::mail-field-email-send-p          "tinylibmail" "" nil)
  (autoload 'ti::mail-field-email-address-p       "tinylibmail" "" nil)
  (autoload 'ti::mail-kill-field-in-body          "tinylibmail" "" nil)
  (autoload 'ti::mail-kill-field                  "tinylibmail" "" nil)
  (autoload 'ti::mail-get-field-1                 "tinylibmail" "" nil)
  (autoload 'ti::mail-get-field                   "tinylibmail" "" nil)
  (autoload 'ti::mail-add-field                   "tinylibmail" "" nil)
  (autoload 'ti::mail-add-to-field-string         "tinylibmail" "" nil)
  (autoload 'ti::mail-kill-field-elt              "tinylibmail" "" nil)
  (autoload 'ti::mail-kill-non-rfc-fields         "tinylibmail" "" nil)
  (autoload 'ti::mail-get-all-email-addresses     "tinylibmail" "" nil)
  (autoload 'ti::mail-set-recipients              "tinylibmail" "" nil)
  (autoload 'ti::mail-news-buffer-p               "tinylibmail" "" t)
  (autoload 'ti::mail-article-regexp-read-line    "tinylibmail" "" nil)
  (autoload 'ti::mail-news-reply-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-anon-penet-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-anon-penet-to-p             "tinylibmail" "" nil)
  (autoload 'ti::mail-nymserver-email-convert     "tinylibmail" "" nil)
  (autoload 'ti::mail-mime-tm-featurep-p          "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-mime-semi-featurep-p        "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-mime-feature-p              "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-mime-tm-edit-p              "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-mime-semi-edit-p            "tinylibmail" "" nil) ;;defsubst
  (autoload 'ti::mail-mime-tm-edit-mode-macro     "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mime-semi-edit-mode-macro   "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mime-funcall-0-macro        "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mime-funcall-2-macro        "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mime-turn-on-mode           "tinylibmail" "" t)
  (autoload 'ti::mail-mime-turn-off-mode          "tinylibmail" "" t)
  (autoload 'ti::mail-mime-sign-region            "tinylibmail" "" t)
  (autoload 'ti::mail-mime-encrypt-region         "tinylibmail" "" t)
  (autoload 'ti::mail-mime-tm-split-macro         "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-mime-maybe-p                "tinylibmail" "" nil)
  (autoload 'ti::mail-mime-p                      "tinylibmail" "" t)
  (autoload 'ti::mail-mime-qp-decode              "tinylibmail" "" nil)
  (autoload 'ti::mail-qp-mime-prepare             "tinylibmail" "" t)
  (autoload 'ti::mail-plugged-p                   "tinylibmail" "" nil)
  (autoload 'ti::mail-sendmail-reset-send-hooks   "tinylibmail" "" nil)
  (autoload 'ti::mail-sendmail-pure-env-macro     "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-sendmail-macro-1            "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-sendmail-macro              "tinylibmail" "" nil 'macro)
  (autoload 'ti::mail-abbrev-table                "tinylibmail" "" nil)
  (autoload 'ti::mail-abbrev-expand-mail-aliases  "tinylibmail" "" t)
  (autoload 'ti::mail-abbrev-get-alist            "tinylibmail" "" nil)
  (autoload 'ti::mail-mail-abbrevs-email-list     "tinylibmail" "" nil)

  ;;}}}
  ;;{{{ code: Autoload 'y' lib -- system

  (autoload 'tinyliby-version                     "tinyliby" "" t)
  (autoload 'tinyliby-submit-feedback             "tinyliby" "" t)
  (autoload 'ti::system-package-where-is-source   "tinyliby" "" nil)
  (autoload 'ti::system-load-cleanup              "tinyliby" "" nil)
  (autoload 'ti::system-load-history-emacs-lisp-files "tinyliby" "" nil)
  (autoload 'ti::system-load-history-where-exactly "tinyliby" "" nil)
  (autoload 'ti::system-load-history-where-1      "tinyliby" "" nil)
  (autoload 'ti::system-doc-where-is-source       "tinyliby" "" nil)
  (autoload 'ti::system-load-history-where-is-source "tinyliby" "" nil)
  (autoload 'ti::system-load-history-get          "tinyliby" "" nil)
  (autoload 'ti::system-enable-disabled-options   "tinyliby" "" t)
  (autoload 'ti::system-feature-kill              "tinyliby" "" nil)
  (autoload 'ti::system-unload-symbols            "tinyliby" "" nil)
  (autoload 'ti::system-unload                    "tinyliby" "" nil)
  (autoload 'ti::system-unload-feature            "tinyliby" "" t)
  (autoload 'ti::system-unload-feature-list       "tinyliby" "" nil)
  (autoload 'ti::system-symbol-dolist-macro       "tinyliby" "" nil 'macro)
  (autoload 'ti::system-remove-from-hooks         "tinyliby" "" nil)
  (autoload 'ti::system-match-in-hooks            "tinyliby" "" t)
  (autoload 'ti::system-get-symbols               "tinyliby" "" nil)
  (autoload 'ti::system-autoload-function-list    "tinyliby" "" nil)
  (autoload 'ti::system-autoload-function-file-list "tinyliby" "" nil)
  (autoload 'ti::system-get-file-documentation    "tinyliby" "" t)
  (autoload 'ti::system-describe-symbols-i-args   "tinyliby" "" nil)
  (autoload 'ti::system-describe-symbols          "tinyliby" "" t)
  (autoload 'ti::system-describe-symbol-summary   "tinyliby" "" t)

  ;;}}}
  ;;{{{ code: Autoload 'o' lib -- overlays

  (autoload 'tinylibo-version                     "tinylibo" "" t)
  (autoload 'tinylibo-feedback                    "tinylibo" "" t)
  (autoload 'ti::overlay-make                           "tinylibo" "" nil) ;;defsubst
  (autoload 'ti::overlay-makec                          "tinylibo" "" nil) ;;defsubst
  (autoload 'ti::overlay-make-match                     "tinylibo" "" nil)
  (autoload 'ti::overlay-buffer-substring               "tinylibo" "" nil) ;;defsubst
  (autoload 'ti::overlay-mouse-on-p                     "tinylibo" "" nil)
  (autoload 'ti::overlay-get-mouse                      "tinylibo" "" nil)
  (autoload 'ti::overlay-get-prop                       "tinylibo" "" nil)
  (autoload 'ti::overlay-get-prop-val                   "tinylibo" "" nil)
  (autoload 'ti::overlay-re-search                      "tinylibo" "" nil)
  (autoload 'ti::overlay-re-search-move                 "tinylibo" "" nil)
  (autoload 'ti::overlay-get-within-area                "tinylibo" "" nil)
  (autoload 'ti::overlay-remove-region                  "tinylibo" "" t)

  ;;}}}
  ;;{{{ code: Autoload Text property library

  (autoload 'ti::text-search-face-reset              "tinylibt" "" nil 'macro)
  (autoload 'ti::text-search-face-set                "tinylibt" "" nil 'macro)
  (autoload 'ti::text-face                           "tinylibt" "" nil 'macro)
  (autoload 'ti::text-stack-clear                    "tinylibt" "" nil) ;;defsubst
  (autoload 'ti::text-stack-length                   "tinylibt" "" nil) ;;defsubst
  (autoload 'ti::text-stack-full-p                   "tinylibt" "" nil) ;;defsubst
  (autoload 'ti::text-stack-p                        "tinylibt" "" nil) ;;defsubst
  (autoload 'ti::text-save-data                      "tinylibt" "" nil)
  (autoload 'ti::text-undo                           "tinylibt" "" t)
  (autoload 'ti::text-clear-buffer-properties        "tinylibt" "" t)
  (autoload 'ti::text-clear-region-properties        "tinylibt" "" t)
  (autoload 'ti::text-get-mouse-property             "tinylibt" "" nil)
  (autoload 'ti::text-match-level                    "tinylibt" "" nil)
  (autoload 'ti::text-re-search                      "tinylibt" "" t)
  (autoload 'ti::text-property-search-and-modify     "tinylibt" "" nil)
  (autoload 'ti::text-read-regexp                    "tinylibt" "" nil)
  (autoload 'ti::text-looking-at                     "tinylibt" "" t)
  (autoload 'ti::text-buffer                         "tinylibt" "" t)
  (autoload 'ti::text-re-search-forward              "tinylibt" "" t)
  (autoload 'ti::text-re-search-backward             "tinylibt" "" t)
  (autoload 'ti::text-mouse-mark-region              "tinylibt" "" t)
  (autoload 'ti::text-mouse-unmark-region            "tinylibt" "" t)
  (autoload 'ti::text-unmark-region                  "tinylibt" "" t)
  (autoload 'ti::text-mark-region                    "tinylibt" "" t)

  ;;}}}

  ;;{{{ code: Autoload other 'tiny tools'

  (autoload 'ti::ck-advice-control                "tinylibck")

  (autoload 'ti::id-info                          "tinylibid")
  (autoload 'ti::id-cnv-txt2comment               "tinylibid")

  (autoload 'ti::menu-help                        "tinylibmenu")
  (autoload 'ti::menu-menu                        "tinylibmenu")

  (autoload 'tinytab-mode                         "tinytab" "" t)
  (autoload 'turn-on-tinytab-mode                 "tinytab" "" t)
  (autoload 'turn-off-tinytab-mode                "tinytab" "" t)
  (autoload 'turn-on-tinytab-return-key-mode      "tinytab" "" t)

  (autoload 'turn-on-tinyurl-mode-maybe           "tinyurl" "" nil)
  (autoload 'turn-on-tinyurl-mode-mail            "tinyurl" "" nil)
  (autoload 'turn-on-tinyurl-mode-1               "tinyurl" "" t)
  (autoload 'turn-off-tinyurl-mode-1              "tinyurl" "" t)
  (autoload 'tinyurl-mode-1                       "tinyurl" "" t)
  (autoload 'turn-on-tinyurl-mode                 "tinyurl" "" t)
  (autoload 'turn-off-tinyurl-mode                "tinyurl" "" t)
  (autoload 'tinyurl-mode                         "tinyurl" "" t)
  (autoload 'tinyurl-mode-action                  "tinyurl" "" nil)
  (autoload 'tinyurl-install                      "tinyurl" "" t)
  (autoload 'tinyurl-mark-line                    "tinyurl")
  (autoload 'tinyurl-overlay-get                  "tinyurl")
  (autoload 'tinyurl-dispatcher                   "tinyurl")
  (autoload 'tinyurl-agent-funcall                "tinyurl")

  ;;}}}
  ;;{{{ code: autoload other

  (autoload 'byte-compile                     "bytecomp")
  (autoload 'occur                            "replace" "" t)

  (autoload 'folding-open-buffer              "folding" "" t)

  (autoload 'mail-yank-original               "sendmail")
  (autoload 'mail-send-and-exit               "sendmail")
  (autoload 'mail-setup                       "sendmail")
  (autoload 'mail-mode                        "sendmail")
  (autoload 'mail-position-on-field           "sendmail")

  (autoload 'mail-fetch-field                 "mail-utils")

  (autoload 'hexl-hex-string-to-integer       "hexl")

  (autoload 'browse-url                       "browse-url")
  (autoload 'browse-url-w3                    "browse-url")
  (autoload 'browse-url-netscape              "browse-url")
  (autoload 'browse-url-lynx-emacs            "browse-url")

  (autoload 'display-time                     "time")
  (autoload 'shuffle-vector                   "cookie1")
  (autoload 'name-last-kbd-macro              "macros")
  (autoload 'mail-extract-address-components  "mail-extr")

  ;;  This is special case. if there is Igrep package available, it
  ;;  will define autoload to "grep" and we must reflect the
  ;;  situation accordingly. See `igrep-insinuate'

  (unless (fboundp 'grep)
    (if (locate-library "igrep")
        (autoload 'grep "igrep" "" t)
      (autoload 'grep "grep" "" t)))

  (autoload 'compile           "compile" "" t)
  (autoload 'compile-internal  "compile")

  ;; Emacs 20.6 sort.el

  (autoload 'sort-subr                            "sort" "" nil)
  (autoload 'sort-build-lists                     "sort" "" nil)
  (autoload 'sort-reorder-buffer                  "sort" "" nil)
  (autoload 'sort-lines                           "sort" "" t)
  (autoload 'sort-paragraphs                      "sort" "" t)
  (autoload 'sort-pages                           "sort" "" t)
  (autoload 'sort-numeric-fields                  "sort" "" t)
  (autoload 'sort-fields                          "sort" "" t)
  (autoload 'sort-fields-1                        "sort" "" nil)
  (autoload 'sort-skip-fields                     "sort" "" nil)
  (autoload 'sort-regexp-fields-next-record       "sort" "" nil)
  (autoload 'sort-regexp-fields                   "sort" "" t)
  (autoload 'sort-columns                         "sort" "" t)
  (autoload 'reverse-region                       "sort" "" t)

  ;; tabify.el

  (autoload 'tabify                           "tabify" "" t)
  (autoload 'untabify                         "tabify" "" t)

  ;; pp.el

  (autoload 'pp-to-string                         "pp" "" nil)
  (autoload 'pp                                   "pp" "" nil)
  (autoload 'pp-eval-expression                   "pp" "" t)
  (autoload 'pp-eval-last-sexp                    "pp" "" t)

  ;; thingatpt.el

  (autoload 'forward-thing                        "thingatpt" "" nil)
  (autoload 'bounds-of-thing-at-point             "thingatpt" "" nil)
  (autoload 'thing-at-point                       "thingatpt" "" nil)
  (autoload 'beginning-of-thing                   "thingatpt" "" nil)
  (autoload 'end-of-thing                         "thingatpt" "" nil)
  (autoload 'in-string-p                          "thingatpt" "" nil)
  (autoload 'end-of-sexp                          "thingatpt" "" nil)
  (autoload 'forward-whitespace                   "thingatpt" "" t)
  (autoload 'forward-symbol                       "thingatpt" "" t)
  (autoload 'forward-same-syntax                  "thingatpt" "" t)
  (autoload 'word-at-point                        "thingatpt" "" nil)
  (autoload 'sentence-at-point                    "thingatpt" "" nil)
  (autoload 'read-from-whole-string               "thingatpt" "" nil)
  (autoload 'form-at-point                        "thingatpt" "" nil)
  (autoload 'sexp-at-point                        "thingatpt" "" nil)
  (autoload 'symbol-at-point                      "thingatpt" "" nil)
  (autoload 'number-at-point                      "thingatpt" "" nil)
  (autoload 'list-at-point                        "thingatpt" "" nil)

  ;; rect.el

  (autoload 'operate-on-rectangle                 "rect" "" nil)
  (autoload 'delete-rectangle-line                "rect" "" nil)
  (autoload 'delete-extract-rectangle-line        "rect" "" nil)
  (autoload 'extract-rectangle-line               "rect" "" nil)
  (autoload 'spaces-string                        "rect" "" nil)
  (autoload 'delete-rectangle                     "rect" "" t)
  (autoload 'delete-extract-rectangle             "rect" "" nil)
  (autoload 'extract-rectangle                    "rect" "" nil)
  (autoload 'kill-rectangle                       "rect" "" t)
  (autoload 'yank-rectangle                       "rect" "" t)
  (autoload 'insert-rectangle                     "rect" "" nil)
  (autoload 'open-rectangle                       "rect" "" t)
  (autoload 'open-rectangle-line                  "rect" "" nil)
  (autoload 'string-rectangle                     "rect" "" t)
  (autoload 'string-rectangle-line                "rect" "" nil)
  (autoload 'clear-rectangle                      "rect" "" t)
  (autoload 'clear-rectangle-line                 "rect" "" nil)

  ;; jka-compr.el

  (autoload 'jka-compr-info-regexp                "jka-compr"   "" nil)
  (autoload 'jka-compr-info-compress-message      "jka-compr"   "" nil)
  (autoload 'jka-compr-info-compress-program      "jka-compr"   "" nil)
  (autoload 'jka-compr-info-compress-args         "jka-compr"   "" nil)
  (autoload 'jka-compr-info-uncompress-message    "jka-compr"   "" nil)
  (autoload 'jka-compr-info-uncompress-program    "jka-compr"   "" nil)
  (autoload 'jka-compr-info-uncompress-args       "jka-compr"   "" nil)
  (autoload 'jka-compr-info-can-append            "jka-compr"   "" nil)
  (autoload 'jka-compr-info-strip-extension       "jka-compr"   "" nil)
  (autoload 'jka-compr-get-compression-info       "jka-compr"   "" nil)
  (autoload 'jka-compr-error                      "jka-compr"   "" nil)
  (autoload 'jka-compr-partial-uncompress         "jka-compr"   "" nil)
  (autoload 'jka-compr-call-process               "jka-compr"   "" nil)
  (autoload 'jka-compr-make-temp-name             "jka-compr"   "" nil)
  (autoload 'jka-compr-delete-temp-file           "jka-compr"   "" nil)
  (autoload 'jka-compr-write-region               "jka-compr"   "" nil)
  (autoload 'jka-compr-insert-file-contents       "jka-compr"   "" nil)
  (autoload 'jka-compr-file-local-copy            "jka-compr"   "" nil)
  (autoload 'jka-compr-load                       "jka-compr"   "" nil)
  (autoload 'jka-compr-byte-compiler-base-file-name "jka-compr" "" nil)
  (autoload 'jka-compr-handler                    "jka-compr"   "" nil)
  (autoload 'jka-compr-run-real-handler           "jka-compr"   "" nil)
  (autoload 'toggle-auto-compression              "jka-compr"   "" t)
  (autoload 'jka-compr-build-file-regexp          "jka-compr"   "" nil)
  (autoload 'jka-compr-install                    "jka-compr"   "" nil)
  (autoload 'jka-compr-uninstall                  "jka-compr"   "" nil)
  (autoload 'jka-compr-installed-p                "jka-compr"   "" nil)

  ;; Advice.el (partial autoloads only)

  (autoload 'ad-disable-advice                    "advice")
  (autoload 'ad-enable-advice                     "advice")
  (autoload 'ad-activate                          "advice")

  ;; finder.el

  (autoload 'finder-compile-keywords              "finder" "" nil)
  (autoload 'finder-compile-keywords-make-dist    "finder" "" nil)
  (autoload 'finder-insert-at-column              "finder" "" nil)
  (autoload 'finder-mouse-face-on-line            "finder" "" nil)
  (autoload 'finder-list-keywords                 "finder" "" t)
  (autoload 'finder-list-matches                  "finder" "" nil)
  (autoload 'finder-find-library                  "finder" "" nil)
  (autoload 'finder-commentary                    "finder" "" t)
  (autoload 'finder-current-item                  "finder" "" nil)
  (autoload 'finder-select                        "finder" "" t)
  (autoload 'finder-mouse-select                  "finder" "" t)
  (autoload 'finder-by-keyword                    "finder" "" t)
  (autoload 'finder-mode                          "finder" "" t)
  (autoload 'finder-summary                       "finder" "" t)
  (autoload 'finder-exit                          "finder" "" t)

  ;; lisp-mnt.el

  (autoload 'lm-get-header-re                     "lisp-mnt" "" nil) ;;defsubst
  (autoload 'lm-get-package-name                  "lisp-mnt" "" nil) ;;defsubst
  (autoload 'lm-section-mark                      "lisp-mnt" "" nil)
  (autoload 'lm-code-mark                         "lisp-mnt" "" nil) ;;defsubst
  (autoload 'lm-commentary-mark                   "lisp-mnt" "" nil) ;;defsubst
  (autoload 'lm-history-mark                      "lisp-mnt" "" nil) ;;defsubst
  (autoload 'lm-header                            "lisp-mnt" "" nil)
  (autoload 'lm-header-multiline                  "lisp-mnt" "" nil)
  (autoload 'lm-summary                           "lisp-mnt" "" nil)
  (autoload 'lm-crack-address                     "lisp-mnt" "" nil)
  (autoload 'lm-authors                           "lisp-mnt" "" nil)
  (autoload 'lm-maintainer                        "lisp-mnt" "" nil)
  (autoload 'lm-creation-date                     "lisp-mnt" "" nil)
  (autoload 'lm-last-modified-date                "lisp-mnt" "" nil)
  (autoload 'lm-version                           "lisp-mnt" "" nil)
  (autoload 'lm-keywords                          "lisp-mnt" "" nil)
  (autoload 'lm-adapted-by                        "lisp-mnt" "" nil)
  (autoload 'lm-commentary                        "lisp-mnt" "" nil)
  (autoload 'lm-insert-at-column                  "lisp-mnt" "" nil)
  (autoload 'lm-verify                            "lisp-mnt" "" t)
  (autoload 'lm-synopsis                          "lisp-mnt" "" t)
  (autoload 'lm-report-bug                        "lisp-mnt" "" t)

  ;; dired.el

  (defvar dired-directory nil)

  ;; reporter.el

  (autoload 'reporter-update-status               "reporter" "" nil)
  (autoload 'reporter-beautify-list               "reporter" "" nil)
  (autoload 'reporter-lisp-indent                 "reporter" "" nil)
  (autoload 'reporter-dump-variable               "reporter" "" nil)
  (autoload 'reporter-dump-state                  "reporter" "" nil)
  (autoload 'reporter-calculate-separator         "reporter" "" nil)
  (autoload 'reporter-mail                        "reporter" "" nil)
  (autoload 'reporter-compose-outgoing            "reporter" "" nil)
  (autoload 'reporter-submit-bug-report           "reporter" "" nil)
  (autoload 'reporter-bug-hook                    "reporter" "" nil)
  (autoload 'define-mail-user-agent               "reporter" "" nil)

  ;; vc-hooks.el
  ;; /usr/share/emacs/21.2/lisp/vc-hooks.el

  (autoload 'vc-mistrust-permissions              "vc-hooks" "" nil)
  (autoload 'vc-mode                              "vc-hooks" "" nil)
  (autoload 'vc-error-occurred                    "vc-hooks" "" nil 'macro)
  (autoload 'vc-file-setprop                      "vc-hooks" "" nil)
  (autoload 'vc-file-getprop                      "vc-hooks" "" nil)
  (autoload 'vc-file-clearprops                   "vc-hooks" "" nil)
  (autoload 'vc-make-backend-sym                  "vc-hooks" "" nil)
  (autoload 'vc-find-backend-function             "vc-hooks" "" nil)
  (autoload 'vc-call-backend                      "vc-hooks" "" nil)
  (autoload 'vc-call                              "vc-hooks" "" nil 'macro)
  (autoload 'vc-parse-buffer                      "vc-hooks" "" nil) ;;defsubst
  (autoload 'vc-insert-file                       "vc-hooks" "" nil)
  (autoload 'vc-registered                        "vc-hooks" "" nil)
  (autoload 'vc-backend                           "vc-hooks" "" nil)
  (autoload 'vc-backend-subdirectory-name         "vc-hooks" "" nil)
  (autoload 'vc-name                              "vc-hooks" "" nil)
  (autoload 'vc-checkout-model                    "vc-hooks" "" nil)
  (autoload 'vc-user-login-name                   "vc-hooks" "" nil)
  (autoload 'vc-state                             "vc-hooks" "" nil)
  (autoload 'vc-up-to-date-p                      "vc-hooks" "" nil) ;;defsubst
  (autoload 'vc-default-state-heuristic           "vc-hooks" "" nil)
  (autoload 'vc-workfile-version                  "vc-hooks" "" nil)
  (autoload 'vc-default-registered                "vc-hooks" "" nil)
  (autoload 'vc-possible-master                   "vc-hooks" "" nil)
  (autoload 'vc-check-master-templates            "vc-hooks" "" nil)
  (autoload 'vc-toggle-read-only                  "vc-hooks" "" t)
  (autoload 'vc-default-make-version-backups-p    "vc-hooks" "" nil)
  (autoload 'vc-version-backup-file-name          "vc-hooks" "" nil)
  (autoload 'vc-delete-automatic-version-backups  "vc-hooks" "" nil)
  (autoload 'vc-make-version-backup               "vc-hooks" "" nil)
  (autoload 'vc-before-save                       "vc-hooks" "" nil)
  (autoload 'vc-after-save                        "vc-hooks" "" nil)
  (autoload 'vc-mode-line                         "vc-hooks" "" t)
  (autoload 'vc-default-mode-line-string          "vc-hooks" "" nil)
  (autoload 'vc-follow-link                       "vc-hooks" "" nil)
  (autoload 'vc-find-file-hook                    "vc-hooks" "" nil)
  (autoload 'vc-file-not-found-hook               "vc-hooks" "" nil)
  (autoload 'vc-kill-buffer-hook                  "vc-hooks" "" nil)

  ;; font-lock from Emacs 20.6

  (defvar font-lock-mode nil)

  (autoload 'font-lock-mode                       "font-lock" "" t)
  (autoload 'turn-on-font-lock                    "font-lock" "" nil)

  ;; Not necessarily in XEmacs font-lock.el
  ;; (autoload 'global-font-lock-mode                "font-lock" "" t)
  ;; (autoload 'font-lock-add-keywords               "font-lock" "" nil)

  (autoload 'font-lock-change-major-mode          "font-lock" "" nil)
  (autoload 'turn-on-font-lock-mode-if-enabled    "font-lock" "" nil)
  (autoload 'font-lock-turn-on-thing-lock         "font-lock" "" nil)
  (autoload 'font-lock-turn-off-thing-lock        "font-lock" "" nil)
  (autoload 'font-lock-after-fontify-buffer       "font-lock" "" nil)
  (autoload 'font-lock-after-unfontify-buffer     "font-lock" "" nil)
  (autoload 'font-lock-fontify-buffer             "font-lock" "" t)
  (autoload 'font-lock-unfontify-buffer           "font-lock" "" nil)
  (autoload 'font-lock-fontify-region             "font-lock" "" nil)
  (autoload 'font-lock-unfontify-region           "font-lock" "" nil)
  (autoload 'font-lock-default-fontify-buffer     "font-lock" "" nil)
  (autoload 'font-lock-default-unfontify-buffer   "font-lock" "" nil)
  (autoload 'font-lock-default-fontify-region     "font-lock" "" nil)
  (autoload 'font-lock-default-unfontify-region   "font-lock" "" nil)
  (autoload 'font-lock-after-change-function      "font-lock" "" nil)
  (autoload 'font-lock-fontify-block              "font-lock" "" t)
  (autoload 'font-lock-prepend-text-property      "font-lock" "" nil)
  (autoload 'font-lock-append-text-property       "font-lock" "" nil)
  (autoload 'font-lock-fillin-text-property       "font-lock" "" nil)
  (autoload 'font-lock-apply-syntactic-highlight  "font-lock" "" nil)
  (autoload 'font-lock-fontify-syntactic-anchored-keywords "font-lock" "" nil)
  (autoload 'font-lock-fontify-syntactic-keywords-region "font-lock" "" nil)
  (autoload 'font-lock-fontify-syntactically-region "font-lock" "" nil)
  (autoload 'font-lock-apply-highlight            "font-lock" "" nil) ;;defsubst
  (autoload 'font-lock-fontify-anchored-keywords  "font-lock" "" nil) ;;defsubst
  (autoload 'font-lock-fontify-keywords-region    "font-lock" "" nil)
  (autoload 'font-lock-compile-keywords           "font-lock" "" nil)
  (autoload 'font-lock-compile-keyword            "font-lock" "" nil)
  (autoload 'font-lock-eval-keywords              "font-lock" "" nil)
  (autoload 'font-lock-value-in-major-mode        "font-lock" "" nil)
  (autoload 'font-lock-choose-keywords            "font-lock" "" nil)
  (autoload 'font-lock-set-defaults               "font-lock" "" nil)
  (autoload 'font-lock-unset-defaults             "font-lock" "" nil)
  (autoload 'font-lock-match-c-style-declaration-item-and-skip-to-next "font-lock" "" nil)
  (autoload 'font-lock-match-c++-style-declaration-item-and-skip-to-next "font-lock" "" nil)

  ;; imenu.el in Emacs 20.6, Not in XEmacs.

  (when (locate-library "imenu")
    (autoload 'imenu--subalist-p                    "imenu" "" nil)
    ;; ** The compiler ignores `autoload' except at top level.
    ;; (autoload 'imenu-progress-message               "imenu" "" nil 'macro)
    (autoload 'imenu-example--name-and-position     "imenu" "" nil)
    (autoload 'imenu-example--lisp-extract-index-name "imenu" "" nil)
    (autoload 'imenu-example--create-lisp-index     "imenu" "" nil)
    (autoload 'imenu-example--create-c-index        "imenu" "" nil)
    (autoload 'imenu--sort-by-name                  "imenu" "" nil)
    (autoload 'imenu--sort-by-position              "imenu" "" nil)
    (autoload 'imenu--relative-position             "imenu" "" nil)
    (autoload 'imenu--split                         "imenu" "" nil)
    (autoload 'imenu--split-menu                    "imenu" "" nil)
    (autoload 'imenu--split-submenus                "imenu" "" nil)
    (autoload 'imenu--truncate-items                "imenu" "" nil)
    (autoload 'imenu--make-index-alist              "imenu" "" nil)
    (autoload 'imenu--cleanup                       "imenu" "" nil)
    (autoload 'imenu--create-keymap-2               "imenu" "" t)
    (autoload 'imenu--create-keymap-1               "imenu" "" nil)
    (autoload 'imenu--in-alist                      "imenu" "" nil)
    (autoload 'imenu-default-create-index-function  "imenu" "" nil)
    (autoload 'imenu--replace-spaces                "imenu" "" nil)
    (autoload 'imenu--generic-function              "imenu" "" nil)
    (autoload 'imenu--completion-buffer             "imenu" "" nil)
    (autoload 'imenu--mouse-menu                    "imenu" "" nil)
    (autoload 'imenu-choose-buffer-index            "imenu" "" nil)
    (autoload 'imenu-add-to-menubar                 "imenu" "" t)
    (autoload 'imenu-add-menubar-index              "imenu" "" t)
    (autoload 'imenu-update-menubar                 "imenu" "" nil)
    (autoload 'imenu--menubar-select                "imenu" "" nil)
    (autoload 'imenu-default-goto-function          "imenu" "" nil)
    (autoload 'imenu                                "imenu" "" t))

  ;;}}}

  ;;{{{ code: XEmacs emulation.

  (when t ;; (locate-library "timer")
    ;; XEmacs provides xemacs-packages\lisp\fsf-compat\timer.el
    ;;
    ;; These functions are the "common denominator" of XEmacs 21.2
    ;; And Emacs 20.4
    ;;
    (autoload 'cancel-function-timers               "timer" "" t)
    (autoload 'cancel-timer                         "timer" "" nil)
    (autoload 'run-at-time                          "timer" "" t)
    (autoload 'run-with-idle-timer                  "timer" "" t)
    (autoload 'run-with-timer                       "timer" "" t)
    (autoload 'timer-activate                       "timer" "" nil)
    (autoload 'timer-activate-when-idle             "timer" "" nil)
    (autoload 'timer-duration                       "timer" "" nil)
    (autoload 'timer-inc-time                       "timer" "" nil)
    (autoload 'timer-relative-time                  "timer" "" nil)
    (autoload 'timer-set-function                   "timer" "" nil)
    (autoload 'timer-set-idle-time                  "timer" "" nil)
    (autoload 'timer-set-time                       "timer" "" nil)
    (autoload 'timer-set-time-with-usecs            "timer" "" nil)
    (autoload 'with-timeout-handler                 "timer" "" nil)
    (autoload 'y-or-n-p-with-timeout                "timer" "" nil))

  (when (featurep 'xemacs)

    (autoload 'set-cursor-color                   "tinylibxe" "" t)
    (autoload 'set-foreground-color               "tinylibxe" "" t)
    (autoload 'set-background-color               "tinylibxe" "" t)
    (autoload 'transient-mark-mode                "tinylibxe" "" t)

    (unless (fboundp 'run-at-time)
      (autoload 'run-at-time                      "tinylibxe"))

    (unless (fboundp 'cancel-timer)
      (autoload 'cancel-timer                     "tinylibxe"))

    (autoload 'posn-window                        "tinylibxe")
    (autoload 'posn-point                         "tinylibxe")
    (autoload 'posn-timestamp                     "tinylibxe")
    (autoload 'window-edges                       "tinylibxe")

    (autoload 'event-start                        "tinylibxe")
    (autoload 'event-x                            "tinylibxe")
    (autoload 'event-y                            "tinylibxe")
    (autoload 'posn-x-y                           "tinylibxe")

    (autoload 'frame-parameters                   "tinylibxe")

    (eval-when-compile
      ;;  emulation in xe library
      (put 'frame-parameters 'byte-obsolete-variable nil))

    (autoload 'dired-unmark                       "tinylibxe")
    (autoload 'dired-mark                         "tinylibxe")
    (autoload 'dired-get-marked-files             "tinylibxe")
    (autoload 'dired-map-over-marks               "tinylibxe"))

  ;;}}}
  ;;{{{ code: XEmacs and Emacs autoloads

  (defvar view-mode nil)

  (cond
   ;; XEmacs 21.x changed package name
   ((and (featurep 'xemacs)
         (locate-library "view-less"))
    (autoload 'view-exit  "view-less" "" t)
    (autoload 'view-mode  "view-less" "" t))
   (t
    (autoload 'view-exit "view" "" t)
    (autoload 'view-mode "view" "" t)))

  (when t ;; (locate-library "overlay")   ;; XEmacs includes emulation lib
    ;; overlay.el
    ;; xemacs-packages/lisp/fsf-compat/overlay.el
    (autoload 'overlayp                             "overlay" "" nil)
    (autoload 'make-overlay                         "overlay" "" nil)
    (autoload 'move-overlay                         "overlay" "" nil)
    (autoload 'delete-overlay                       "overlay" "" nil)
    (autoload 'overlay-start                        "overlay" "" nil)
    (autoload 'overlay-end                          "overlay" "" nil)
    (autoload 'overlay-buffer                       "overlay" "" nil)
    (autoload 'overlay-properties                   "overlay" "" nil)
    (autoload 'overlays-at                          "overlay" "" nil)
    (autoload 'overlays-in                          "overlay" "" nil)
    (autoload 'next-overlay-change                  "overlay" "" nil)
    (autoload 'previous-overlay-change              "overlay" "" nil)
    (autoload 'overlay-lists                        "overlay" "" nil)
    (autoload 'overlay-recenter                     "overlay" "" nil)
    (autoload 'overlay-get                          "overlay" "" nil)
    (autoload 'overlay-put                          "overlay" "" nil))

  ;;}}}

  ) ;; eval-and-compile

;;; tinyliba.el ends here
