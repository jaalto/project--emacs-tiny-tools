;;; complete-menu.el --- show completions in X-popup menu

;;{{{ Id

;; This file is not part of Emacs

;; Copyright (C) 1993 Alon Albert
;; Copyright (C) 1997-2010 Jari Aalto
;; Author:       Alon Albert <alon@milcse.rtsg.mot.com>
;; Maintainer:   Jari Aalto
;; Created:      1993-12-07

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
;;; install:

;;   Put this file in your load-path and insert the following in .emacs
;;
;;      (require 'cl)
;;      (when window-system
;;	  (require 'complete-menu))
;;
;;   Or use autoload, your emacs starts up faster (then maybe not, because
;;   there is so much code)
;;
;;   (when window-system
;;     (setq cm-load-hook 'cm-install-default)  ;; Need advices too
;;     (autoload 'cm-minibuffer-completion-help "complete-menu")
;;     (define-key minibuffer-local-completion-map [C-tab]
;;       'cm-minibuffer-completion-help)
;;     (define-key minibuffer-local-must-match-map [C-tab]
;;       'cm-minibuffer-completion-help)
;;     (substitute-key-definition 'minibuffer-completion-help
;; 			       'cm-minibuffer-completion-help
;; 			       minibuffer-local-completion-map)
;;     (substitute-key-definition 'minibuffer-completion-help
;; 			       'cm-minibuffer-completion-help
;; 			       minibuffer-local-must-match-map)
;;     (substitute-key-definition 'PC-completion-help
;; 			       'cm-minibuffer-completion-help
;; 			       minibuffer-local-completion-map)
;;     (substitute-key-definition 'PC-completion-help
;; 			       'cm-minibuffer-completion-help
;; 			       minibuffer-local-must-match-map))
;;
;;
;;   The X-popup appears if "?" is pressed in minibuffer.

;;}}}
;;{{{ Documentation
;;; Commentary:

;;  Press "?" while in minibuffer to get the X-popup
;;  Also supports unix like wildcards so:
;;
;;        find file: comp*.el* <?>
;;
;;  This utility may be useful for `describe-function' and `describe-variable'.
;;  typing C-h v *word* pops a menu with all variables with the word "word"
;;  in them. (something like apropos)

;;}}}
;;{{{ History

;;; History:

;; v1.10  2001-03-05 [jari]				Released
;;       - Added Autoload statements.
;;
;; v1.8-1.9 May  24 1997 [jari]				Released
;;      - Added byte compilation stop for XEmacs, thank to note from
;;        Rick Flower <flower@ms31.sp.trw.com>
;;      - Added defcustom support. Checkdoc 1.29 clear.
;;
;; v1.7 Dec  5  1996 [jari]				Released
;;	- I was reordering my emacs startup files to make
;;	  maximum use of autoloads and delete all unnecessary
;;	  require commands, then I noticed that this file didn't have
;;	  autoload choice.
;;	- updated the installation instructions, so that you this
;;	  package is loaded only in demand.
;;
;; v1.6 Jun  3  1996 [jari]				Released
;;	- Noticed bug. I I pressed "*" and tried to find all temporary
;;	  buffers, it showed me all. Too bad...
;;	- Now I can hit "*m" to show me all "*mail* foo" "*mail* quux"
;;	  and other pending mail buffers.
;;	- Advertise: If you want to have user name attached to mail
;;	  buffer, like above, get my lisp libs and do this:
;;
;;	     (require 'tinylibmail)
;;	     (add-hook 'mail-setup-hook 'ti::mail-rename-buffer)
;;
;; v1.5 Jun  3  1996 [jari]				Released
;;      - Error in installation, now there is cm-install-2
;;
;; v1.4 Jun  3  1996 [jari]				NotReleased
;;      - Small corrections
;;
;; v1.3 Sep  21 1995 [jari]				NotReleased
;;	- Bryan M Kramer <kramer@cs.toronto.edu> popped up in g.e.help
;;	  asking why this package didn't work in XEmacs any more.
;;	  It turned out that this package wasn't archived anywhere, nor
;;	  in the OHIO nor did any archie could find it. So I received
;;	  copy of this code and packaged whole file in suitable form.
;;	- added separate installation, added final load hook, replaced
;;	  right copyright info. added advice, lots of small stuff..
;;      - Corrected bug in cm-minibuffer-completion-help: if user didn't
;;        choose anything, it cleared the minibuffer entry. Now the original
;;        entry is preserved.
;;
;; v1.2 Aug  22 1993 [Alon]
;;	- a few minor fixes
;;	- a new chooser from *completions* buffer that allows completion to
;;        be yanked into any buffer (not just the minibuffer)
;;
;; v1.1 July 18 1993 [Alon]
;;	- Cleaner faster version.
;;	- Special thanks to Kevin Rodgers <kevin@traffic.den.mmc.com>
;;        for an intriguing discussion about list manipulation in emacs lisp
;;
;; v1.0 July 7 1993: [Alon]
;;	- First release.

;;}}}

;;; Code:

;;; ......................................................... &require ...

(require 'advice)

(eval-and-compile
  (when (boundp 'xemacs-logo)
    (message "\n\
  ** complete-menu.el: This package works only in Emacs, because the popup\n\
                       function is not compatible between Emacs versions.\n\
                       Ignore `Aborted' error command if you're\n\
                       byte compiling this file in XEmacs.")
    (error "Aborted.")))

(defgroup complete-menu nil
  "Provides X-popup list where you can select completions items. See ? key."
  :prefix "complete-menu-"
  :group  'extensions)

;;{{{ setup: variables

;;; ....................................................... &variables ...

;;; or if you dont't want to substitute the originals, use
;;; 'cm-install-2
;;;
;;;###autoload
(defcustom cm-load-hook '(cm-install-default)
  "*Hook run when file has been loaded."
  :type  'hook
  :group 'complete-menu)

;;  handy if you want to call from non-mouse, eg. pressing key.
;;
(defcustom cm-x-coord 170
  "*Default X menu coordinate."
  :type  'integer
  :group 'complete-menu)

(defcustom cm-y-coord 170
  "*Default Y menu coordinate."
  :type  'integer
  :group 'complete-menu)

(defcustom cm-max-entries-in-menu 45
  "*Maximum lines to display in a single menu pane"
  :type  'integer
  :group 'complete-menu)

(defcustom cm-store-cut-buffer t
  "If not nill then store selection in mouse cut buffer"
  :type 'boolean
  :group 'complete-menu)

(defcustom cm-execute-on-completion t
  "If not nil then exucute command after completion"
  :type  'boolean
  :group 'complete-menu)

(defvar cm-wildcard-to-regexp
  '((?* . ".*")
    (?. . "\\.")
    (?? . "."))
  "Translation table from wildcard format to regexp format")

;;}}}
;;{{{ code: funcs

;;; ....................................................... &Functions ...

;;; ----------------------------------------------------------------------
;;;
(defun cm-make-regexp (wildcard)
  "Make a regexp out of unix like WILDCARD."
  (let* ((char-list (append wildcard)))
    (mapconcat (function
                (lambda (elt)
                  (let ((regexp (cdr (assoc elt cm-wildcard-to-regexp))))
                    (if regexp
                        regexp
                      (char-to-string elt)))))
               char-list "")))

;;; ----------------------------------------------------------------------
;;;
(defun cm-old-zap-to-char (arg char)
  "Kill up to (but not including) ARG'th occurrence of CHAR.
Goes backward if ARG is negative; goes to end of buffer if CHAR not found."
;;;  (interactive "*p\ncZap to char: ")
  (kill-region (point) (if (search-forward (char-to-string char) nil t arg)
                           (progn (goto-char
                                   (if (> arg 0) (1- (point)) (1+ (point))))
                                  (point))
                         (if (> arg 0) (point-max) (point-min)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun cm-minibuffer-completion-help ()
  "List completions in a menu and copy selction into minibuffer"
  (interactive)
  (message "Making completion list...")
  (let* ((complete (buffer-string))
         (mouse-pos (mouse-position))
         (mouse-pos (if (nth 1 mouse-pos)
                        mouse-pos
                      (set-mouse-position (car mouse-pos)
                                          (/ (frame-width) 2) 2)
                      (unfocus-frame)
                      (mouse-position)))
         (pos (list (list (car (cdr mouse-pos))
                          (1+ (cdr (cdr mouse-pos))))
                    (car mouse-pos)))
         (match nil)
         (panes nil)
         (pane nil)
         (i 0)

         completion-list
         name
         menu
         elt)

    (if (string-match "?" complete)
        (setq match (format "^%s$" (cm-make-regexp
                                    (file-name-nondirectory complete)))
              complete (substring complete 0 (match-beginning 0))))

    (setq completion-list
          (sort (all-completions
                 complete
                 minibuffer-completion-table
                 minibuffer-completion-predicate)
                'string<))

    (message "Making completion list... Done")
    (message "Creating menu...")

    (while (setq elt (car completion-list))
      (if (or (null match)
              (string-match match elt))
          (setq pane (cons elt pane)
                i (1+ i)))
      (setq completion-list (cdr completion-list))
      (if (= i cm-max-entries-in-menu)
          (setq panes (cons pane (nreverse panes))
                pane nil
                i 0)))

    (if pane (setq panes  (cons pane (nreverse panes))))

    (setq menu (cons "Completions"
                     (mapcar (function
                              (lambda (elt)
                                (cons (car elt)
                                      (mapcar (function
                                               (lambda (elt)
                                                 (cons elt elt)))
                                              elt))))
                             panes)))

    (message "Creating menu... Done")
    (if (not (car (cdr menu)))
        (beep)
      (setq name (x-popup-menu pos menu))
      (cm-old-zap-to-char -1 ?/)
      (if (null name)
          ;;  User didn't select anything
          (insert complete)		;put previous back.
        (insert name)
        (if cm-store-cut-buffer
            (kill-new name))
        (if cm-execute-on-completion
            (exit-minibuffer))))))

;;; ----------------------------------------------------------------------
;;;
(defun cm-delete-max-match (string)
  "Return maximum match for STRING."
  (let* ((len (min (length string) (1- (point))))
         (string (substring string 0 len)))
    (goto-char (- (point) len))
    (while (and (> len 0) (null (looking-at string)))
      (setq string (substring string 0 -1)
            len (1- len))
      (forward-char 1))
    (delete-char len)))

;;; ----------------------------------------------------------------------
;;;
(defun cm-choose-completion (event)
  "Display completion menu. EVENT is x popup event."
  (interactive "e")
  (let ((buffer (window-buffer))
        choice)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-start event))))
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (skip-chars-backward "^ \t\n")
        (let ((beg (point)))
          (skip-chars-forward "^ \t\n")
          (setq choice (buffer-substring beg (point))))))
    (set-buffer buffer)
    (cm-delete-max-match choice)
    (insert choice)
    (and (equal buffer (window-buffer (minibuffer-window)))
         cm-execute-on-completion (exit-minibuffer))))

;;; ----------------------------------------------------------------------
;;;  Not activated until user wants it, this overrides ? keys
;;;
(defadvice minibuffer-completion-help (around cm-x-complete dis)
  "Replaces function and calls cm-minibuffer-completion-help.
Displays completions in X-menu instead of separate buffer."
  (cm-minibuffer-completion-help))

;;; ----------------------------------------------------------------------
;;;
(defadvice PC-completion-help (around cm-x-complete dis)
  "Replaces function and calls cm-minibuffer-completion-help.
Displays completions in X-menu instead of separate buffer."
  (cm-minibuffer-completion-help))

;;; ----------------------------------------------------------------------
;;;  You may also want to bind it to "more closer key", [left hand
;;;  pops the X, and right controls the mouse]:
;;;
(defun cm-install-2 ()
  "Install the X-menuing feature to separate C-tab key."
  (define-key minibuffer-local-completion-map [C-tab]
    'cm-minibuffer-completion-help)
  (define-key minibuffer-local-must-match-map [C-tab]
    'cm-minibuffer-completion-help))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun cm-install-default (&optional arg verb)
  "Install the X-menuing feature. With ARG, remove X-menuing. VERB.
Note: installation is only possible in X envinronment."
  (interactive "P")
  (let* ((map1  'minibuffer-local-completion-map)
         (map2  'minibuffer-local-must-match-map)
         (ofun1 'minibuffer-completion-help)
         (ofun2 'PC-completion-help)
         (nfun  'cm-minibuffer-completion-help))

    (or verb
        (setq verb (interactive-p)))

    (if (null window-system)
        (if verb
            (message "No window system detected. Cannot do nothing."))
      (cond
       (arg
        (substitute-key-definition nfun ofun1 (eval map1))
        (substitute-key-definition nfun ofun1 (eval map2))

        (substitute-key-definition nfun ofun2 (eval map1))
        (substitute-key-definition nfun ofun2 (eval map2))

        (ad-disable-advice ofun1 'around 'cm-x-complete)
        (ad-disable-advice ofun2 'around 'cm-x-complete)
        (if verb
            (message "X-menu completion off")))
       (t
        (substitute-key-definition ofun1 nfun (eval map1))
        (substitute-key-definition ofun1 nfun (eval map2))

        (substitute-key-definition ofun2 nfun (eval map1))
        (substitute-key-definition ofun2 nfun (eval map2))

        (ad-enable-advice ofun1 'around 'cm-x-complete)
        (ad-enable-advice ofun2 'around 'cm-x-complete)
        (if verb
            (message "X-menu completion on"))))
      (ad-activate ofun1)
      (ad-activate ofun2))))

;;}}}

(provide   'complete-menu)
(run-hooks 'cm-load-hook)

;;; complete-menu.el ends here
