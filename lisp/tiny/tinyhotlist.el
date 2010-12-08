;;; tinyhotlist.el --- Hot-list of important buffers, files(ange-ftp), dired

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) 1995-2010 Jari Aalto
;; Keywords:     tools
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

;; ........................................................ &t-install ...
;; Put this file on your Emacs-Lisp `load-path', add following into your
;; ~/.emacs startup file. Rip code with tinylib.el/ti::package-rip-magic
;;
;;      (add-hook 'tinyhotlist--load-hook 'tinyhotlist-load-hotlist)
;;      (require 'tinyhotlist)
;;
;; or use autoload, preferred because your emacs starts up faster
;;
;;      (autoload 'tinyhotlist-control          "tinyhotlist" "" t)
;;      (autoload 'tinyhotlist-load-hotlist     "tinyhotlist" "" t)
;;      (autoload 'tinyhotlist-save-hotlist     "tinyhotlist" "" t)
;;
;; Suggested key bindings
;;
;;      ;;  for windowed system. In XEmacs, use event `button3'.
;;      (global-set-key [(control shift mouse-3)] 'tinyhotlist-control)
;;
;;      ;;  for non-windowed, close to C-x b , switch-to-buffer
;;      (global-set-key "\C-cb" 'tinyhotlist-control-kbd)
;;
;; Before you can use hot list, read the documentation of function
;; `tinyhotlist-control'. Example setup is at the end of file.

;;}}}
;;{{{ Briefly

;;; .................................................... &t-commentary ...

;;; Commentary:

;;}}}
;;{{{ Documentation

;;  Preface, may 1995
;;
;;      There is excellent utility 'msb.el', but when it comes to having
;;      most important files at hand, It needs some companion with it. An
;;      emacs session can easily have 20 C++ files, user may start news
;;      while the compile it going on and try some lisp code found from the
;;      gnu.emacs.help articles, load couple of emacs configuration files
;;      for editing and then realize that there is mail coming, because
;;      some biff utility tells so. User switches to mail reader and starts
;;      reading the latest messages... within short period of time emacs is
;;      full of buffers and to use MSB to navigate through them all may be
;;      get one frustrated: "Where was that buffer again, do I need to step
;;      3 panes before I can see that file...?"
;;
;;      The navigation is especially problem if user is working only with
;;      handful of source files actively, while he may still have 40+ files
;;      loaded.
;;
;;      What would help the situation? A simple hot list for my most used
;;      files, where one can put and remove items very easily. No more
;;      searching like in msb.el.
;;
;;      This package does not intend to replace `msb', it's superb in class
;;      of its own, but there may be also need for a hot list, because the
;;      most used files page in `msb' changes dynamically whenever buffers
;;      are changed. Hot list in the other hand stays the same from session
;;      to session.
;;
;;  Overview of features
;;
;;      o   Provides pop-up menu where you can add and remove current buffer:
;;          "most important work file list". In non-windowed system,
;;          standard completion feature is used instead of pop-up menu.
;;      o   Any persistent files can be kept in hot list, even ange-ftp files or
;;          dired buffers.
;;      o   Hot list can be saved and read on startup.
;;      o   This is not "last visited files" list, but persistent list of
;;          files. When you select item from hot list, the file is displayed
;;          (if it is in Emacs) or loaded (by using ange-ftp if necessary).
;;
;;  How to use the hotlist
;;
;;      When you load this package, it defines hot list cache to store the
;;      items. The list will be empty at first, but after you
;;      have added an entry to it, you can display the hot list. To add
;;      or remove entries from hot list, is explained in function:
;;
;;          C-h f tinyhotlist-control
;;
;;      If you use add and remove commands often, it might be useful to
;;      to define some custom key bindings. The alternative way is to use
;;      prefix arguments to functions `tinyhotlist-control'
;;
;;          (global-set-key [(shift f3)]   'tinyhotlist-add)
;;          (global-set-key [(control f3)] 'tinyhotlist-remove)
;;
;;      In non-windowed environment hot list is is managed through completion menu.
;;      Admittedly, this is not as nice as the pop-up menu.,
;;      To use keyboard, use function:
;;
;;          tinyhotlist-control-kbd
;;
;;      Here is an example of the displayed hot list in pop-up. The second
;;      string to the right is abbreviation name of the directory, e.g. `~ftp1'
;;      is a short name for /user@site.com:~user/project/this/. The `txt' is
;;      short name for $HOME/doc/txt/
;;
;;          +-------------------+
;;          |hotlist            |
;;          |===================|
;;          |*Backtrace*        |
;;          |*VC-log*           |
;;          |.emacs             |
;;          |.procmailrc        |
;;          |ChangeLog          |
;;          |RMAIL              |
;;          |file.txt     txt   |
;;          |other.txt    txt   |
;;          |remote.cc    ~ftp1 |
;;          |remote.cc    ~ftp2 |
;;          +-------------------+
;;
;;      Note about the pop-up display: The font used in pop-up may not be
;;      proportionally spaced, like Courier, so the entries may therefore
;;      show as ragged. That is, the directory names are not nicely lined
;;      up.
;;
;;  Shortening long filenames
;;
;;      The typical menu item is quite long, because there is buffer name
;;      and filename part. The default rule shortens the home directory
;;      names to "" but if your file is elsewhere, you have to modify the
;;      `tinyhotlist--abbreviate-file-name-table'. There is examples how to use it
;;      at the end of source file. Like:
;;
;;          /user@site.com:~user/project/this/  --> ~ftp1
;;
;;      If you make changes to this variable after the hot list has been
;;      made, the new abbreviations will take effect on at creation of new
;;      items. To rebuild everything from defaults (this destroys you
;;      previous content), call function `tinyhotlist-build-default-hotlist'.
;;
;;  Hooks: saving hot list after each cache update
;;
;;      The buffers are stored into variable `tinyhotlist--cache' and there
;;      is two hooks that run after the entry is deleted or added to the
;;      cache. The hooks are `tinyhotlist--add-hook' and
;;      `tinyhotlist--remove-hook'. They contain default value
;;      `tinyhotlist-save-hotlist' which updates the cache on disk after
;;      each change. You can set these hooks to nil if you want to manually
;;      control when to save cache. (Maybe you load BASE cache every time
;;      and modify it during Emacs session, but you don't want to save
;;      this "session" hot list).
;;
;;          (add-hook 'tinyhotlist--load-hook 'my-tinyhotlist-load-hook)
;;
;;          (defun my-tinyhotlist-load-hook ()
;;            "My hotlist settings"
;;            (setq tinyhotlist-save-hotlist nil)
;;            (setq tinyhotlist--remove-hook nil))
;;
;;  Saving and restoring the hot list
;;
;;      When you're satisfied with the hot list, save it to file with command:
;;
;;            M-x tinyhotlist-save-hotlist
;;
;;      To automatically restore the hot list when package loads:
;;
;;          (add-hook 'tinyhotlist--load-hook 'tinyhotlist-load-hotlist)
;;
;;      To save the _current_ hot list automatically when Emacs exists:
;;
;;          (add-hook 'kill-emacs-hook 'tinyhotlist-save-hotlist)
;;
;;  An example
;;
;;      Here is complete example setup how you could configure this package.
;;
;;          (autoload  'tinyhotlist-control "tinyhotlist" "" t)
;;          (ti::add-hooks 'tinyhotlist--load-hook
;;               '(tinyhotlist-load-hotlist my-tinyhotlist-init))
;;
;;          (defun my-tinyhotlist-init ()
;;            "Sets defaults for hotlist"
;;            (setq tinyhotlist--default-function       'my-tinyhotlist-defaults)
;;            (global-set-key [(control shift mouse-3)] 'tinyhotlist-control))
;;
;;            (defconst tinyhotlist--abbreviate-file-name-table
;;              (list
;;               ;;   Remember: the substitution order must be _BIGGEST_
;;               ;;   substitution first.
;;               ;;
;;               ;;  Shorten ange ftp references
;;               (list
;;               "/foo@example.com:/home/foo"
;;               "~foo")
;;
;;               (list txt    "~t")
;;               (list wtxt   "~wt")
;;               (list elisp  "")   ;; and wont show this either
;;               (list h        ""))))   ;; don't display the home
;;
;;          (defconst tinyhotlist--default-regexp
;;            (concat
;;             "^RMAIL$\\|scratc\\|diff\\|buffer menu\\|diff\\|Messages"
;;
;;             ;; Procmail
;;             "\\|procmailrc\\|pm-.*\\(hdr\\|log\\|rc\\|txt\\)"
;;
;;             ;; text
;;             "\\|elisp.txt\\|ssjaaa.txt"
;;
;;             ;; perl
;;             "\\|\\.pls"
;;
;;             "\\|.mak"
;;
;;             ;; emacs project files
;;             "\\|emacrs\\|funcs.ja.el\\|tinylibm.el\\|tinylib.el"
;;
;;             ;;  C++ project files
;;             "\\|wmpmea.*cc"
;;
;;             ;; Gnus
;;             "\\|article\\|newsgroup\\|Summary\\|MIME-out"))
;;
;;            ;; ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ window-system ^ ^
;;            )

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

(require 'tinylibm)

(ti::package-defgroup-tiny TinyHotlist tinyhotlist-- tools
  "Hotlist of important buffers and files, easy add, easy remove")

;;}}}
;;{{{ setup: private
;;; .......................................................... &v-bind ...

(defvar tinyhotlist--history-keymap nil
  "Keymap for history.")

;; completion keymap unused currently, #todo someday

(if tinyhotlist--history-keymap
    nil
  (setq tinyhotlist--history-keymap (make-sparse-keymap))
  (define-key  tinyhotlist--history-keymap      [(up)]   'ignore)
  (define-key  tinyhotlist--history-keymap      [(down)] 'ignore))

;;; ......................................................... &v-hooks ...

(defcustom tinyhotlist--load-hook '(tinyhotlist-load-hotlist)
  "*Hook run when file is loaded."
  :type  'hook
  :group 'TinyHotlist)

(defcustom tinyhotlist--add-hook '(tinyhotlist-save-hotlist)
  "*Hook run when new buffer is added with `tinyhotlist-add-internal'.
Functions in hook are called with two arguments:

  BUFFER ADD-STATUS

Default value is `tinyhotlist-save-hotlist' which saves cache after every change."
  :type  'hook
  :group 'TinyHotlist)

(defcustom tinyhotlist--remove-hook '(tinyhotlist-save-hotlist)
  "*Hook run when new buffer is added with `tinyhotlist-remove-internal'.
Functions in hook are called with two arguments:

  BUFFER REMOVE-STATUS

Default value is `tinyhotlist-save-hotlist' which saves cache after every change."
  :type  'hook
  :group 'TinyHotlist)

;;; ....................................................... &v-private ...

(defvar tinyhotlist--cache nil
  "Hotlist cache.
Format:
  '((\"BUFFER-NAME[ DIRECTORY]\" . [FILE-NAME])
    ...)

The BUFFER-NAME is the actual name of the buffer. It may contains
<2> in the name too indicatin second buffer with the same name.
The DIRECTORY part is only included if buffer is readlly connected to file.
the DIRECTORY contains leading space if the directory part is included

  'buffer'     -- single entry
  'buffer2 ~/' -- buffer and filename.")

(defvar tinyhotlist--history nil
  "History for completion.")

(defcustom tinyhotlist--hotlist-file
  (ti::package-config-file-prefix "tinyhotlist.el")
  "*Default hotlist configuration file. You can edit as you want.
If you edit the order of this file, set `tinyhotlist--cache-sort-flag' to nil."
  :type  'file
  :group 'TinyHotlist)

(defcustom tinyhotlist--cache-sort-flag t
  "Non-nil means Sort the entries in hotlist after adding a buffer to it.
If you want to edit by hand the order of the hotlist file
`tinyhotlist--hotlist-file', then set this variable to nil, and the raw
order is preserved."
  :type  'boolean
  :group 'TinyHotlist)

;;}}}
;;{{{ setup: user config

;;; ........................................................ &v-public ...

(defcustom tinyhotlist--list-max 40
  "*Maximum members in hotlist."
  :type  'integer
  :group 'TinyHotlist)

(defcustom tinyhotlist--title "     hotlist     "
  "*Title of menu."
  :type  'string
  :group 'TinyHotlist)

;;  handy if you want to call from non-mouse, e.g. pressing key.
;;  --> set event parameter to nil when calling func  tinyhotlist-control

(defcustom tinyhotlist--x-coord 170
  "*Default menu coordinate."
  :type  'integer
  :group 'TinyHotlist)

(defcustom tinyhotlist--y-coord 170
  "*Default menu coordinate."
  :type  'integer
  :group 'TinyHotlist)

(defcustom tinyhotlist--use-x-popup t
  "*If non-nil, don't use popups.
If you prefer not to use popup-like dialog box for hotlist items,
then set ths variable to nil. This variable is valid only if you're
running in X-windowed system."
  :type  'boolean
  :group 'TinyHotlist)

(defcustom tinyhotlist--default-regexp nil
  "*Regexp to match buffers when initialising hotlist menu.
See `tinyhotlist-control'."
  :type  'string
  :group 'TinyHotlist)

(defcustom tinyhotlist--abbreviate-file-name-table
  (list
   (list
    (or (and (getenv "HOME")
             ;;   The path names are seen as lowercase in Emacs in Win32
             (if (ti::win32-p)
                 (downcase (getenv "HOME"))
               (getenv "HOME")))
        (error "TinyHotlist: no HOME env variable"))
    "~"))
  "How to substitute absolute path names. The PATH value is case sensitive.
Changes in this variable will only affect the new buffers added to the
hotlist. If you want to rebuild the whole hotlist using the
`tinyhotlist--default-regexp', call `tinyhotlist-build-default-hotlist'

Format:
 '((PATH  SUBSTITUTE) (P S) ..)

Example:

  ;;  Remember to put _longest_ substitutionmatches first.
  ;;
  '((\"/etc/daemons/interalarms/\" \"ALARM\")
    (\"/users/foo/\" \"\"))   ;; Don't show my home at all

Please look at the many examples that are in the end of tinyhotlist.el"
  :type '(repeat
          (list
           (string :tag "path")
           (string :tag "alias")))

  :group 'TinyHotlist)

;;}}}
;;{{{ menu handle

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-find-buffer (item &optional no-confirm)
  "Find buffer for corresponding menu ITEM.
The buffer is loaded from disk if it does not exist in Emacs.
NO-CONFIRM suppresses confirm of loading ange-ftp files."
  (let (buffer
	file
	elt
	ptr)
    (setq elt (assoc item tinyhotlist--cache))
    (setq buffer (car elt)
          file   (cdr elt))
    (cond
     (file
      ;; Find ange-ftp dired buffer
      (when (string-match "@.*:" file)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and (eq major-mode 'dired-mode)
                       (string=
                        file
                        (symbol-value 'dired-directory)))
              (setq ptr (current-buffer))
              (return)))))
      (setq ptr
            (or ptr
                (get-file-buffer file)

                ;;  This would call file-attributes, which will call
                ;;  ange ftp for remote buffers.
                ;;
                ;;  ange-ftp-hook-function(file-attributes ...

                (and (not (string-match "@.*:" file))
                     (find-buffer-visiting file))))
      (if ptr
          (switch-to-buffer ptr)
        (cond
         ((and (string-match "@.*:" file)
               (y-or-n-p (format "Load %s " file)))
          (find-file file))
         ((not (file-exists-p file))
          (message "TinyHotlist: file not found [%s]." (or file buffer))
          (sleep-for 2))
         (t
          (find-file file)))))
     ((setq ptr (get-buffer buffer))
      ;;   Temporary buffer, which is not a file, like  *Messages*
      (switch-to-buffer ptr))
     (t
      (message "TinyHotlist: Can't find buffer [%s]" buffer)))))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-abbreviate-file-name (file &optional restore)
  "Abbreviate FILE by looking at `tinyhotlist--abbreviate-file-name-table'.
If RESTORE is passed, the convert abbreviated FILE into absolute path
using `tinyhotlist--abbreviate-file-name-table'."
  (let (case-fold-search
	str
	substitute
	match
	replace)
    (dolist (elt tinyhotlist--abbreviate-file-name-table)
      (setq str (nth 0 elt)  substitute (nth 1 elt))
      (setq match (if restore substitute str)
            replace
            (if restore
                (file-name-as-directory str)
              substitute))

      (when (string-match (concat "^" (regexp-quote match)) file)
        (setq file (ti::replace-match 0 replace file))
        (return)))
    file))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-add-internal (buffer)
  "Add BUFFER or list of buffers to hotlist. Arg must be STRING [list].

Returns:
   t or nil if added."
  (let (buffer-file
	ptr
	exist
	ret)
    (dolist (buffer (ti::list-make buffer))
      ;;  We have to check if it exists already...
      ;;  this is a  bit inefficent way to check list, but because
      ;;  list is small, this is the shortest way.
      (setq buffer-file
            (and (setq ptr (get-buffer buffer))
                 (with-current-buffer ptr
                   (or (buffer-file-name) ;; 1) regular file
                       ;; 2) User may be in dired
                       ;;    VC renames dired mode, so we can't just 'memq
                       ;;    `major-mode'
                       (if (string-match "dired" (symbol-name major-mode))
                           (symbol-value 'dired-directory))))))
      ;; ................................................ check buffer ...
      ;;  - If buffer has filename check the CDR of cache
      ;;  - if buffer has no filename, then check CAR of the cache.
      (cond
       (buffer-file
        (setq exist
              (member buffer-file (mapcar (function cdr) tinyhotlist--cache))))
       (t
        (setq exist (ti::list-find tinyhotlist--cache (regexp-quote buffer)))))
      ;; ............................................. push to hotlist ...
      (unless exist
        (when buffer-file ;;  Get the directory name
          (let* ((abbrev (abbreviate-file-name
                          (tinyhotlist-abbreviate-file-name
                           (file-name-directory buffer-file))))
                 (total  (+ (length buffer)
                            (length abbrev)))
                 elt)
            (if (< total 80)
                (setq elt (format "%-25s %s" buffer abbrev))
              (setq elt (concat buffer " " abbrev)))
            (push (cons elt (abbreviate-file-name buffer-file))
                  tinyhotlist--cache)))))
    ;;  Keep it in sorted order.
    (when tinyhotlist--cache-sort-flag
      (setq
       tinyhotlist--cache
       (sort
        tinyhotlist--cache
        (function
         (lambda (a b)
           (string-lessp (car b) (car a)))))))
    (setq ret (not exist)) ;; buffer was added if didn't exist
    (run-hook-with-args 'tinyhotlist--add-hook buffer ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-remove-internal (arg type)
  "Remove according to ARG and MODE a item from `tinyhotlist--cache'.

Input:

  ARG    Depends on mode.
  TYPE   what type the arg is: 'menu-item   'buffer  'file

Return

 nil t   if removed."
  (let (list
	func
	ret)
    (cond
     ((eq type 'menu-item)
      (when (and (stringp arg)
                 (setq arg (assoc arg tinyhotlist--cache)))
        (setq ret t)
        (setq tinyhotlist--cache (delete arg tinyhotlist--cache))))
     ((eq type 'file)
      (setq func 'cdr))
     ((eq type 'buffer)
      (cond
       ((get-buffer arg)                ;Buffer is filename
        (setq func 'cdr))
       (t
        (setq func 'car)))))
    (when func
      (dolist (elt tinyhotlist--cache)
        (if (string-match (regexp-quote arg) (or (funcall func elt) "" ))
            (setq ret t)
          (push elt list))
        (setq tinyhotlist--cache list))
      (setq tinyhotlist--cache list))
    (run-hook-with-args 'tinyhotlist--remove-hook arg ret)
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-add-by-regexp (regexp &optional temp-buf)
  "Add all buffers matchig REGEXP to hotlist.
If optional TEMP-BUF prefix arg is non-nil the mach is made
against temporary buffers too. Otherwise they are not counted."
  (interactive "sAdd buffers matching: \nP")
  (tinyhotlist-add-internal
   (ti::dolist-buffer-list
    (string-match regexp (buffer-name))
    temp-buf)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-kill (&optional default)
  "Kill hotlist or initialise with defaults if DEFAULT flag is non-nil.
References:
   `tinyhotlist--default-regexp'."
  (interactive)
  (setq tinyhotlist--cache nil)
  (if (and default (stringp tinyhotlist--default-regexp))
      (tinyhotlist-add-by-regexp tinyhotlist--default-regexp)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-set-defaults ()
  "Initialise hotlist according to `tinyhotlist--default-regexp'."
  (tinyhotlist-kill 'init))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-build-default-hotlist ()
  "Delete existing hotlist and build with `tinyhotlist--default-regexp'.
See variable `tinyhotlist--abbreviate-file-name-table'."
  (interactive)
  (setq tinyhotlist--cache nil)
  (tinyhotlist-set-defaults))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-save-hotlist (&rest ARGS)
  "Call `tinyhotlist-load-hotlist' with arg to save hotlist. ARGS are ignored."
  (interactive)
  (tinyhotlist-load-hotlist 'save))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-load-hotlist (&optional save)
  "Load or SAVE hotlist configuration from `tinyhotlist--hotlist-file'.
When the hotlist file is loaded, only valid entries from there
are selected to `tinyhotlist--cache': If File does not exist, it is dropped.

Return:

 nil t"
  (interactive "P")
  (let ((file tinyhotlist--hotlist-file)
	buffer
	list)
    (cond
     ;; ......................................................... load ...
     ((null save)
      (when (file-exists-p file)
        (load-file file)
        (dolist (elt tinyhotlist--cache)
          (setq buffer (car elt)
                file   (cdr elt))
          ;;  Drop away non-existing files.
          ;;  The Temp buffers *scratch* may not be in emacs, but
          ;;  they can be in hotlist.

          (when (or (null file)
                    (and file
                         (or
                          ;;  Let ange-ftp fies go through
                          (string-match "@" file)
                          ;;  Check regular files.
                          (file-exists-p file))))
            (push (cons buffer file) list)))
        ;;  Reverse must be used due to push.
        (setq tinyhotlist--cache (nreverse list))
        t))
     ;; ......................................................... save ...
     (t
      (let* ((file tinyhotlist--hotlist-file)
             (dir  (file-name-directory file)))
        (if (not (stringp dir))
            (error (concat "TinyHotlist: `tinyhotlist--hotlist-file'"
                           " must be absolute path [%s]")
                   file)
          ;;  Make sure that file can be saved to a directory
          (or (file-directory-p dir)
              (and (y-or-n-p (format "TinyHotlist: [SAVE] Create %s? " dir))
                   (make-directory dir t)))
          (ti::write-file-variable-state
           file
           "Emacs TinyHotlist.el cache file."
           '(tinyhotlist--cache))))))))

;;}}}
;;{{{ X menu

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-complete (list)
  "Show LIST in completion menu.
Return:
 buffer or nil"
  (let ((menu (ti::list-to-assoc-menu list))
	(def  (car-safe tinyhotlist--history))
	ret)
    (setq ret (completing-read "hot item: "
			       menu
			       nil
			       t
			       def
			       'tinyhotlist--history))
    (if (ti::nil-p ret)                 ;really selected ?
        nil
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun tinyhotlist-show-menu (event &optional title)
  "Pop the menu and select the buffer.
If EVENT is nil, use default coordinates to display the menu and TITLE.

Return:
  menu item or nil."
  (interactive "e")
  (let* ((list   (mapcar (function car) tinyhotlist--cache))
         (title  (or title tinyhotlist--title))
         (x      (cond
                  ((and tinyhotlist--use-x-popup ;; permits use of popup
                        (ti::compat-window-system))
                   t)
                  (t
                   nil)))) ;; no X available...
    (if x
        (ti::compat-popup list event nil title)
      (tinyhotlist-complete list))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-control-kbd (&optional arg)
  "Same as `tinyhotlist-control' with ARG, but you can call this from keyboard."
  (interactive "P")
  (tinyhotlist-control
   (ti::compat-make-fake-event tinyhotlist--x-coord tinyhotlist--y-coord) arg))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-control (event &optional arg)
  "Control center of hotlist. Use mouse EVENT to position popup.

Optional ARG can be:

  nil           show the hotlist
  0             kill all members from hotlist.
  9             kill all, but initalize with defaults.
  nbr           any number, add current active buffer to hotlist
  -             negative number, remove item from hotlist.
                E.g. \\[universal-argument] -
  1 x \\[universal-argument]       remove current buffer from hotlist
  2 x \\[universal-argument]       Save hotlist
  3 x \\[universal-argument]       load hotlist."
  (interactive "e\nP")
  (let ((buffer (buffer-name))
	(menu   (or tinyhotlist--cache
		    ;;  See if there is any buffers matching user's
		    ;;  regexp to make the initial hotlist.
		    (and tinyhotlist--default-regexp
			 (tinyhotlist-set-defaults)
			 tinyhotlist--cache)))
	ret)
    (cond
     ;; ...................................................... display ...
     ((null arg)
      (cond
       ((null menu)
        (message  "TinyHotlist: Empty hotlist.")
        (sleep-for 1))
       (t
        (when (setq ret (tinyhotlist-show-menu event))
          (tinyhotlist-find-buffer ret)))))
     ;; ................................................... remove/add ...
     ((or (integerp arg)
          (memq arg '(-)))
      (cond
       ((eq 0 arg)
        (tinyhotlist-kill)
        (message "TinyHotlist: Hotlist killed.")
        (sleep-for 1))
       ;;  "Why number 9??" --   Because it's next to number 0
       ((eq 9 arg)
        (tinyhotlist-set-defaults)
        (message "TinyHotlist: Hotlist killed/initalized.")
        (sleep-for 1))
       ((and (integerp arg)
             (> arg 0))
        (cond
         ((tinyhotlist-add-internal buffer)
          (message "TinyHotlist: Added to hotlist [%s]" buffer)
          (sleep-for 1))
         (t
          (message "TinyHotlist: Already in hotlist."))))

       (t                               ;Negative
        (if (null menu)
            (message "TinyHotlist: Empty hotlist.")
          (when (setq ret (tinyhotlist-show-menu event "--Remove item--"))
            (tinyhotlist-remove-internal ret 'menu-item)
            (message "TinyHotlist: Removed. [%s]" ret)
            (sleep-for 1))))))
     ;; ............................................... remove current ...
     ((equal '(4) arg)
      (if (if (buffer-file-name)
              (tinyhotlist-remove-internal (buffer-file-name) 'file )
            (tinyhotlist-remove-internal buffer 'buffer))
          (message "TinyHotlist: Removed [%s]" buffer)
        (message "TinyHotlist: Nothing to remove, [%s] wasn't in hotlist."
                 buffer)))
     ((equal '(16) arg)
      (tinyhotlist-save-hotlist)
      (message "TinyHotlist: saved")
      (sit-for 1.5))
     ((equal '(64) arg)
      (if (tinyhotlist-load-hotlist)
          (message "TinyHotlist: loaded")
        (message "TinyHotlist: Can't load %s" tinyhotlist--hotlist-file))
      (sleep-for 2)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-add ()
  "Add current buffer to hotlist."
  (interactive)
  (tinyhotlist-control nil 1))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tinyhotlist-remove ()
  "Remove current buffer from hotlist."
  (interactive)
  (tinyhotlist-control nil -1))

;;}}}

(provide   'tinyhotlist)
(run-hooks 'tinyhotlist--load-hook)

;;; tinyhotlist.el ends here
