;;; tinycache.el --- Maintain a cache of visited files [compile,dired]

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)    1996-2010 Jari Aalto
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; Look at the code with folding.el

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

;;   Put this file on your Emacs-Lisp `load-path', add following into
;;   ~/.emacs startup file:
;;
;;       (add-hook tinycache--load-hook 'tinycache-install)
;;       (add-hook tinycache--load-hook 'tinycache-install-msb)
;;       (require 'tinycache)
;;
;;   Or use quicker autoload:
;;
;;       (add-hook tinycache--load-hook 'tinycache-install-msb)
;;       (eval-after-load "compile" '(progn (require 'tinycache)))
;;       (eval-after-load "dired"   '(progn (require 'tinycache)))
;;
;;   If you use *gnuserv.el*, be sure that to load the packages in order:
;;   gnuserv, tinycache.
;;
;;   To disable this package:
;;
;;      M-x tinycache-uninstall
;;
;;   If you have any questions, use this function
;;
;;      M-x tinycache-submit-bug-report

;;}}}
;;{{{ Documentation

;;; Commentary:

;;  Preface, overview of features
;;
;;      This package is meant to be used with `dired' and compilation
;;      buffers. When you load file from either one, the file is
;;      "remembered". This way you can browse bunch of files easily and
;;      when you have finished you can flush the cache and get rid of all
;;      vieved files.
;;
;;  Dired description
;;
;;      When you load a file from dired with `dired-view-file', the
;;      file is remembered. You can load several files for viewing and when you
;;      have finished, call `tinycache-flush' (Defaults to `C-c' `k' in
;;      dired) to remove all the remembered (cached) files from emacs.
;;
;;      This way you don't end up having files that you're not interested
;;      in any more. Using the cache makes browsing bunch of files very
;;      easy. Each dired buffer has it's own cache. The cache is also
;;      flushed if you kill the dired buffer.
;;
;;  Compilation cache description
;;
;;      Maintain also a cache of buffers visiting files via the
;;      `next-error' and `compile-goto-error' commands; each compile/grep
;;      buffer has its own cache.  To kill the cached buffers manually, use
;;      `C-c' `C-d' (compile-flush-cache) in the compile/grep buffer;
;;      deleting the compile/grep buffer automatically kills the cached
;;      buffers.  To disable the cache, set `compilation-find-file-cache'
;;      to a non-list value (e.g. 'disable).
;;
;;      After loading this file, every file that is loaded by calling some
;;      compile function, i.e. `compile-goto-error', is cached if it is not
;;      in emacs already. I.e. when you fix some small errors in other
;;      files, you may not want to keep those files in emacs after you've
;;      done; remember, those got loaded during the calls to
;;      compile-goto-error. The easiest way to get rid of these extra
;;      files, that were not originally in emacs, is to:
;;
;;          A. kill compilation buffer, C-x k *compilation*
;;          B. Call M-x tinycache-flush directly
;;
;;      See *igrep.el* also how you can browse (grep) files easily and when
;;      you've done, you can call this package top get rid of those browsed
;;      files.
;;
;;  Cache minor mode indication --  Controlling the cache flag
;;
;;      Mode line indication shows for loaded buffer
;;
;;          "+C"    if file is loaded as cached.
;;          "+c"    If you have manually turned off the cache
;;
;;      And for root buffer where the file were loaded, normally
;;      compilation or dired buffer, the mode line shows
;;
;;          "+CN"  where N is number of files currently in the cache
;;
;;      Sometimes you want to keep some file that belongs to the cache
;;      and you don't want to loose it when you execute `M-x' `tinycache-flush'
;;      or when you kill the root buffer.
;;
;;      For that purpose there is function `tinycache-mode' to turn
;;      off the cache for current buffer. When the cache mark says
;;      "+c" in the mode line, it tells you that the file will not be
;;      killed when you `tinycache-flush' is called.
;;
;;      Note: the root buffer's xx count is not updated when you kill
;;      buffer that was cached. So if the count says 10, and you kill 3
;;      files that, the count will still still say 10. The count is
;;      updated only when you load some *new* file from the root buffer.
;;      At that time all the buffers cached are checked and the ones that
;;      do not exist any more are removed.
;;
;;  Buffer list commands
;;
;;      There are some additional commands added to buffer list which
;;      helps you to keep track of the cached files better. The "c"
;;      prefix is chosen for (c)ache related commands.
;;
;;          C-c c m     mark all cached files
;;          C-c c d     mark as deleted
;;          C-c c u     unmark cached files.
;;
;;  Dired mode commands
;;
;;      Similar to buffer list, there is some dired commands too
;;
;;          C-c c k     tinycache-flush, remove all cached files from this dired
;;          C-c c m     tinycache-dired-mark
;;          C-c c u     tinycache-dired-unmark
;;
;;  Thanks
;;
;;      Kevin Rodgers, his *igrep.el* gave me an idea for this. The
;;      original cache code from where this package evolved was
;;      written by Kevin under name *compile-cache.el*

;;; Change Log:

;;}}}
;;{{{ setup: require, variables

;;; Code:

(eval-when-compile (require 'advice))
(require 'tinylibm)

(eval-and-compile
  (autoload 'compilation-find-buffer    "compile"   "" t)
  (autoload 'dired-view-file            "dired"     "" t)
  (autoload 'dired-get-filename         "dired")
  (autoload 'dired-mark                 "dired"     "" t)
  (autoload 'dired-unmark               "dired"     "" t))

(ti::package-defgroup-tiny TinyCache tinycache-- extensions
  "Maintain a cache of visited files [compile, dired].
    overview of features

        This package is meant to be used with dired and compilation
        buffers. When you load file from either one, the file is
        cached. This way you can view files easily and when you
        have finished you can flush the cache and get rid of all
        viewed files.")

;;; ............................................. &emacs-compatibility ...
;;;  We must install this for older emacs versions ( 19.30< )

(unless (boundp 'kill-buffer-hook)
  ;; gnuserv.el
  ;; - We have to do this now, because if user loads files in this order:
  ;;
  ;;        tinycache
  ;;        gnuserv
  ;;
  ;;   The gnuserv won't see the real function, because tinycache
  ;;   put advice around the function. And using the adviced function
  ;;   as "real" causes infinite loop.
  ;;
  ;;   If user doesn't use gnuserv, this just defines one extra function,
  ;;   which does no harm.
  ;;
  (or (fboundp 'server-real-kill-buffer)
      (fset 'server-real-kill-buffer (symbol-function 'kill-buffer)))

  (defvar kill-buffer-hook nil
    "Defined in tinycache.el package. Hook run just before buffer
     is killed.")

  (defadvice kill-buffer (around tinycache act)
    "Run kill-buffer-hook before buffer iss killed."
    (run-hooks 'kill-buffer-hook)
    ;; prevent accidents by setting this hook to nil
    (let ((kill-buffer-hook nil))
      ad-do-it)))

;;; ........................................................ &v-public ...

(defcustom tinycache--mode-on-string " +C"
  "*Cache property on indicator. File can be flushed."
  :type  'string
  :group 'TinyCache)

(defcustom tinycache--mode-off-string " +c"
  "*Cache property off indicator. File will not be flushed."
  :type  'string
  :group 'TinyCache)

(defcustom tinycache--load-hook nil
  "*Hook run when file has been loaded.
Suggested functions:
  `tinycache-install'
  `tinycache-install-msb'."
  :type  'hook
  :group 'TinyCache)

(defcustom tinycache--find-file-buffer-hook 'tinycache-maybe-view-mode
  "*Hook run inside buffer which is loaded from compile output."
  :type 'hook
  :group 'TinyCache)

;;; ....................................................... &v-private ...

(defvar tinycache--mode-name tinycache--mode-on-string
  "String in the mode line to mark if that file is part the cache or not.
This is changed by program and not a user variable.")

(defvar tinycache-mode nil
  "Mode on/off variable.")
(make-variable-buffer-local 'tinycache-mode)
(put 'tinycache-mode  'permanent-local t)

(defvar tinycache--info nil
  "Variable to keep information about the cache.
Contains the compile buffer pointer from where the file was loaded.")
(make-variable-buffer-local 'tinycache--info)
(put 'tinycache--info 'permanent-local t)

(defvar tinycache--mode-user-flag nil
  "If non-nil, user has touched the `tinycache-mode' flag in this buffer.")
(make-variable-buffer-local 'tinycache--mode-user-flag)
(put 'tinycache--mode-user-flag   'permanent-local t)

(defvar tinycache--cache nil
  "List of buffers created by `compilation-find-file'. Local to each buffer.
Format: (BUFFER-POINTER BP ..)")

;; If user starts new compilation in *grep* buffer, it wipes the results
;; but the cache must remain there, since he may have loaded files from
;; previous compilation.

(put 'tinycache--cache            'permanent-local t)

;;}}}
;;{{{ code: misc functions

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-uninstall ()
  "Deactivate package."
  (tinycache-advice-control 'disable))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-advice-control (&optional disable)
  "Turn advices on. Optionally DISABLE."
  (interactive)
  (ti::advice-control
   '(compile-internal compilation-find-file) "^tinycache"  disable 'verb))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-install-mode ()
  "Install `tinycache-mode'."
  ;;  Make sure we can display string in mode line
  (if (null (assq 'tinycache-mode minor-mode-alist))
      (ti::keymap-add-minor-mode 'tinycache-mode
                                 'tinycache--mode-name
                                 ;; No key map
                                 (make-sparse-keymap))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-install (&optional uninstall)
  "Install or UNINSTALL cache."
  (interactive)
  (cond
   (uninstall
    (remove-hook 'buffer-menu-mode-hook 'tinycache-buffer-list-bindings-on)
    (remove-hook 'compilation-mode-hook 'tinycache-define-default-keys)
    (remove-hook 'dired-mode-hook       'tinycache-add-local-hook)
    (remove-hook 'dired-mode-hook       'tinycache-define-default-keys))
   (t
    (add-hook 'buffer-menu-mode-hook 'tinycache-buffer-list-bindings-on)
    (add-hook 'compilation-mode-hook 'tinycache-define-default-keys)
    (add-hook 'dired-mode-hook       'tinycache-add-local-hook)
    (add-hook 'dired-mode-hook       'tinycache-define-default-keys))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-install-msb ()
  "Install new cache menu to msb.el if it is loaded."
  (let ((elt
	 '((and (boundp 'tinycache-mode)
		tinycache-mode)
	   1005
	   "Cached compile files (%d)"))
	(sym  'msb-menu-cond)
	menu)
    (when (and (featurep 'msb)
               ;;  Install only once.
               ;;  symbol-value just silences byteComp
               (setq menu (symbol-value sym))
               (not (member menu elt)))
      (push elt menu)
      (set sym menu))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-define-default-keys ()
  "Define keys to `compilation-minor-mode-map'."
  (interactive)
  (let (map)
    ;;  Avoid byte compilation warnings this way....
    (when (and (boundp  'compilation-minor-mode-map)
               (setq map (symbol-value 'compilation-minor-mode-map))
               (keymapp map))
      (ti::use-prefix-key map "\C-cc")
      (ti::define-key-if-free map "\C-cck" 'tinycache-flush))
    (when (and (boundp  'compilation-minor-mode-map)
               (setq map (symbol-value 'compilation-minor-mode-map))
               (keymapp  map))
      (ti::use-prefix-key map "\C-cc")
      (ti::define-key-if-free map "\C-cck" 'tinycache-flush))
    (when (and (boundp  'dired-mode-map)
               (setq map (symbol-value 'dired-mode-map))
               (keymapp  map))
      (ti::use-prefix-key map "\C-cc")
      (ti::define-key-if-free map "\C-cck" 'tinycache-flush)
      (ti::define-key-if-free map "\C-ccm" 'tinycache-dired-mark)
      (ti::define-key-if-free map "\C-ccu" 'tinycache-dired-unmark))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-buffer-list-bindings-on ()
  "Add default bindings to buffer list, \\[list-buffers]."
  ;;  Choose "c" for cache commands
  (ti::use-prefix-key Buffer-menu-mode-map "\C-cc")
  ;;  With these you can see the cachec files
  (ti::define-key-if-free Buffer-menu-mode-map "\C-ccd"
                          'tinycache-buffer-list-mark-deleted)
  (ti::define-key-if-free Buffer-menu-mode-map "\C-ccm"
                          'tinycache-buffer-list-mark)
  (ti::define-key-if-free Buffer-menu-mode-map "\C-ccu"
                          'tinycache-buffer-list-unmark)
  (ti::define-key-if-free Buffer-menu-mode-map "\C-ccf"
                          'tinycache-buffer-list-mark-cache-off)
  ;;  Bye, bye. No need to call us again. Installation already done.
  (remove-hook 'buffer-menu-mode-hook 'tinycache-define-default-keys))

;;; ----------------------------------------------------------------------
;;;  - The following says that "Load view-(minor)mode in Emacs 19.30, but
;;;    for other (X)Emacs load mview-mode if it exists."
;;;  - The view-mode is minor mode in new Emacs releases, older
;;;    versions are encouraged to get mview.el minor mode
;;;
;;;            mview.el
;;;            Mike Williams
;;;
(defun tinycache-maybe-view-mode ()
  "Turen on view (minor) mode if needed."
  (interactive)
  (let ((zip-re ;; arc-mode's zip file prompt
	 ;; M Filemode      Length  Date         Time      File
	 ;; - ----------  --------  -----------  --------  ---------
	 ;; -rw-r--r--      9695  10-Sep-1993  17:53:46  Makefile
	 ;; -rw-r--r--      7441   8-Sep-1993  14:21:20  README
	 ;;
	 ".*Filemode[ \t]+Length[ \t]+Date[ \t]+Time[ \t]+File[ \t]*$")
	func)
    ;;  Now; if you load from dired a .zip or .tar file; you don't
    ;;  want view mode on, check buffer file name.
    (cond
     ((and (not (ti::emacs-p "21"))
           (ti::emacs-p)
           (< emacs-minor-version 30)
           (require 'mview nil 'noerr)
           ;; Not loaded from dired-view-file, but from compile buffer?
           (not (eq major-mode 'view-mode)))
      (when (fboundp 'view-mode)        ;Turn off this mode.
        (ti::funcall 'view-mode 0)) ;in 19.28 won't work, but try anyway
      ;; Use minor mode instead
      (setq func 'mview-mode))
     ((fboundp 'view-mode)
      (setq func 'view-mode)))
    (cond
     ((save-excursion
        (ti::pmin)
        (and (looking-at zip-re)
             (fboundp 'archive-mode)))
      (ti::funcall 'archive-mode))
     (func
      (funcall func)))))

;;}}}
;;{{{ code: Buffer list control

;;; ----------------------------------------------------------------------
;;;
(defsubst tinycache--mode-on-string-p ()
  "Check if `tinycache-mode' is on."
  (memq tinycache-mode '(t on)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycache-map-over-buffers 'lisp-indent-function 1)
(defmacro tinycache-map-over-buffers (off &rest body)
  "Map over all cached buffers.
If OFF is non-nil, maps over buffers whose `tinycache--cache' is off
and do BODY.

In macro you can refer to these variables. The names are mangled
so that they don't clash with the toplevel definitions.

    'BuffeR'    as current buffer
    'NamE'      buffers's name
    'LisT'      current list of buffers to loop over.

Setting 'list' to nil terminates this macro."
  `(let (NamE)
     (dolist (BuffeR (tinycache-cached-file-list ,off) )
       (setq NamE (buffer-name BuffeR))
       (if (null NamE)
           (setq NamE nil))             ; No-op, silence byte compiler
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycache-buffer-list-map-over-buffers 'lisp-indent-function 0)
(defmacro tinycache-buffer-list-map-over-buffers (&rest body)
  "Special Buffer list macro to execute BODY at found buffer line."
  `(tinycache-map-over-buffers nil
                               (setq NamE (regexp-quote NamE))
                               (ti::pmin)
                               (when (re-search-forward NamE nil t)
                                 (beginning-of-line)
                                 ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'tinycache-buffer-list-map-over-off-buffers 'lisp-indent-function 0)
(defmacro tinycache-buffer-list-map-over-off-buffers (&rest body)
  "Special Buffer list macro to execute BODY at found buffer line."
  `(tinycache-map-over-buffers 'off
                               (setq NamE (regexp-quote NamE))
                               (ti::pmin)
                               (when (re-search-forward NamE nil t)
                                 (beginning-of-line)
                                 ,@body)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-buffer-list-mark ()
  "Mark Cached files in buffer list."
  (interactive)
  (tinycache-buffer-list-map-over-buffers (Buffer-menu-mark)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-buffer-list-unmark ()
  "Mark Cached files in buffer list."
  (interactive)
  (tinycache-buffer-list-map-over-buffers (Buffer-menu-unmark)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-buffer-list-mark-deleted ()
  "Mark Cached files in buffer list."
  (interactive)
  (tinycache-buffer-list-map-over-buffers (Buffer-menu-delete)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-buffer-list-mark-cache-off ()
  "Mark files whose cache property has been turned off."
  (interactive)
  (tinycache-buffer-list-map-over-off-buffers (Buffer-menu-mark)))

;;}}}
;;{{{ Dired buffer

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-dired-mark (&optional unmark verb)
  "Mark cached files. Optionally UNMARK. VERB."
  (interactive)
  (let ((i 0)
         file)
    (ti::verb)
    (ti::save-line-column-macro nil nil
      (dolist (elt tinycache--cache)
        (setq file (buffer-name elt))
        (ti::pmin)
        (when (re-search-forward (format "%s$" file))
          (cond
           (unmark
            (beginning-of-line)
            (when (eq (following-char)
                      (symbol-value 'dired-marker-char))
              (incf  i)
              (dired-unmark 1)))
           (t
            (incf  i)
            (dired-mark 1))))))
    (if verb
        (message "%d cached files %smarked" i
                 (if unmark
		     "un"
		   "") ))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-dired-unmark (&optional verb)
  "Unmark cached files. VERB."
  (interactive)
  (ti::verb)
  (tinycache-dired-mark 'unmark verb))

;;}}}
;;{{{ code: cache control

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-add-local-hook ()
  "Make `kill-buffer-hook' local to this buffer. And add `tinycache-flush' to it.
When you kill the dired buffer, cached buffers loaded from this
buffer are also killed."
  (let (buffer)
    ;; Make sure this hook does not contain tinycache-flush outside
    ;; the compilation buffer
    (remove-hook 'kill-buffer-hook 'tinycache-flush)
    (save-excursion
      ;;   Select right buffer where to localize the hook
      (cond
       ((eq major-mode 'dired-mode))
       ((and (boundp 'compilation-last-buffer)
             (setq buffer (symbol-value 'compilation-last-buffer))
             (buffer-live-p (get-buffer buffer)))
        (set-buffer buffer))
       (t
        (error "improper use of tinycache-add-local-hook: no dired/compilation")))
      ;;  force the hook local to buffer
      (add-hook 'kill-buffer-hook 'tinycache-flush nil 'local)
      ;;  Make sure there are no constants in the hook.
      (setq kill-buffer-hook (delq nil (delq t kill-buffer-hook))))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-activate (this-buffer buffer)
  "THIS-BUFFER is the root buffer (dired,compile) and put BUFFER to cache."
  (interactive)
  (let (new-list)
    ;;  update cache list
    (with-current-buffer this-buffer
      (make-local-variable 'tinycache--cache)
      (dolist (elt tinycache--cache) ;Remove buffers that do not exist
        (if (buffer-live-p (get-buffer elt))
            (push elt new-list)))
      (setq tinycache--cache new-list)
      (if (not (memq buffer tinycache--cache))
          (push buffer tinycache--cache))
      (tinycache-mode-root-buffer))
    ;;  Turn on cache mode
    (with-current-buffer buffer
      (setq tinycache--info this-buffer)
      ;;  - We don't touch the tinycache-mode if user has taken control
      ;;  - If the flag shows untouched buffer, mark the buffer
      ;;    to cache.
      (unless tinycache--mode-user-flag
        (setq tinycache-mode 'on)
        (setq tinycache--mode-name tinycache--mode-on-string)
        (ti::compat-modeline-update))
      (run-hooks 'tinycache--find-file-buffer-hook))))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-cached-file-list (&optional off)
  "Return all cached files by looking at every compilation buffer.
This excludes files that user has manually instructed not to be in
cache (tinycache-mode is off for buffer)

if optional OFF is non-nil, then return buffers whose `tinycache--cache'
has been turned manually off."
  (let ((modes '(compilation-mode compilation-minor-mode dired-mode))
	buffers
	blist2)
    (dolist (elt (buffer-list))
      (with-current-buffer elt
        (when (and (memq major-mode modes)
                   tinycache--cache)
          ;;  Read the value of cache and loop over it
          (setq blist2 tinycache--cache)
          ;;  Check the `tinycache-mode' for every cached file.
          (dolist (elt2 blist2)
            (with-current-buffer elt2
              (cond
               ((null off)
                (if (tinycache--mode-on-string-p)
                    (push elt2 buffers)))
               (t
                (if (null (tinycache--mode-on-string-p))
                    (push elt2 buffers))))))  ))) ;; dolist1 - 2
    buffers))

;;; ----------------------------------------------------------------------
;;;
(defun tinycache-mode-root-buffer (&optional remove)
  "Update root buffer's `tinycache-mode' flag. Optionally REMOVE from cache.
This is the buffer where `tinycache--cache' resides."
  ;; (make-variable-buffer-local 'tinycache--mode-name)
  (make-local-variable 'tinycache--mode-name)
  (cond
   (remove
    (setq tinycache-mode nil))
   (t
    (setq tinycache-mode 'on)
    (setq tinycache--mode-name
          (format "%s%d" tinycache--mode-on-string (length tinycache--cache))))))
;;; ----------------------------------------------------------------------
;;; - This function must be called by user only!
;;;
;;;###autoload
(defun tinycache-mode (&optional arg)
  "Toggle cache flag for this buffer with ARG.
If the file does not belong to compile cache, calling this function
does nothing. If the file is in the cache, the mode line displays mode name.

Removing the file from cache means that the file is not killed when
the cache is flushed with \\[tinycache-flush]."
  (interactive)
  (unless tinycache--info
    (message "Can't find tinycache--info, Buffer is not part of the cache?")
    (sleep-for 1))
  (if (null tinycache-mode)
      (message "This buffer is not in cache controlled.")
    (setq tinycache--mode-user-flag t)
    (cond
     ((memq arg '(0 -1))
      (setq tinycache-mode 'off))
     ((eq arg nil)
      (if (tinycache--mode-on-string-p)
          (setq tinycache-mode 'off)
        (setq tinycache-mode 'on)))
     (t
      (setq tinycache-mode 'on)))
    (message
     (format "Buffer is %s in cache control"
             (if (tinycache--mode-on-string-p)
                 (progn
                   (setq tinycache--mode-name tinycache--mode-on-string)
                   "now")
               (setq tinycache--mode-name tinycache--mode-off-string)
               "no more")))
    (ti::compat-modeline-update)))

;;}}}
;;{{{ code: flush

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-flush-all-compilation (&optional verb)
  "Kill all cached files by stepping through all compilation buffers. VERB."
  (interactive)
  (let ((count  0)
	(verb   (or verb (interactive-p))))
    (tinycache-map-over-buffers nil
                                (when (buffer-live-p  (get-buffer BuffeR))
                                  (kill-buffer BuffeR)
                                  (incf  count)))
    (if verb
        (message (format "Flushed %d buffers." count)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinycache-flush (&optional verb)
  "Kill buffers listed in `tinycache--cache'. VERB.
You must be in the Compilation/Dired buffer to execute this command.

If you're not in dired buffer, function tries to find compilation
buffer and kill compilation cached files."
  (interactive)
  (let ((cache-buffer  (current-buffer))
	count
	do-it)
    (ti::verb)
    (unless (eq major-mode 'dired-mode)
      ;;  This calls error if no compile buffer found...
      (setq cache-buffer (ignore-errors (compilation-find-buffer))))
    (when cache-buffer                  ;if there is buffer for us
      (with-current-buffer cache-buffer
        (tinycache-mode-root-buffer 'remove)
        ;;  This variable is local to buffer
        (setq count   (length tinycache--cache))
        (dolist (buffer tinycache--cache)
          (setq do-it nil)
          (if (not (buffer-live-p  (get-buffer buffer)))
              (setq do-it t)
            (with-current-buffer buffer
              ;;  kill-buffer-hook is local to each buffer.
              ;;  prevent alias loop
              (let ((kill-buffer-hook kill-buffer-hook))
                (remove-hook 'kill-buffer-hook 'tinycache-flush)
                (when (tinycache--mode-on-string-p) ;it's allowed to kill
                  (kill-buffer buffer)
                  (setq do-it t)))))
          (if do-it
              (setq tinycache--cache (delq buffer tinycache--cache))))))
    (if verb
        (message (format "Cache flushed [%s] files. " count)))))

;;}}}
;;{{{ code: advice

;;; ----------------------------------------------------------------------
;;;
(defadvice compilation-find-file (around tinycache activate)
  "Cache newly visited files in `tinycache--cache';
use `\\[tinycache-flush]' in compilation buffer,
to kill these loaded files."
  (let ((this-buffer    (current-buffer))
        (file           filename)       ;ADVICE variable
        buffer)
    (tinycache-install-mode)
    ;;  Already in emacs ?
    (setq buffer (get-file-buffer file))
    (prog1
        ad-do-it
      (unless buffer                    ;Loaded new file
        (if (setq buffer ad-return-value)
            (tinycache-activate this-buffer buffer))))))

;;; ----------------------------------------------------------------------
;;;
(defadvice compile-internal (after tinycache activate)
  "Automatically kill the buffers listed in `tinycache--cache'.
`kill-buffer-hook' when the `compile' buffer is killed."
  (tinycache-add-local-hook))

;;; ----------------------------------------------------------------------
;;;
(defadvice dired-view-file (around tinycache act)
  "Cache newly visited files in `tinycache--cache';
Kill the dired buffer to kill cached files."
  (let ((this-buffer    (current-buffer))
        (file           (dired-get-filename))
        buffer)
    (tinycache-install-mode)
    (cond
     ((file-directory-p file)
      (setq buffer t))                  ;skip this one
     ((get-file-buffer file)            ;Already in emacs ?
      (setq buffer t))
     (t
      (setq buffer (get-file-buffer file))))
    (prog1
        ad-do-it
      (when (and (null buffer)          ;Loaded new file
                 (setq buffer (get-file-buffer file)))
        (tinycache-activate this-buffer buffer)))))

;;}}}

(provide   'tinycache)
(run-hooks 'tinycache--load-hook)

;; tinycache.el ends here
