;;; c-comment-edit2.el --- C Comment Edit
;;; $Id: c-comment-edit2.el,v 2.11 2007/05/07 10:50:05 jaalto Exp $

;; This file is not part of Emacs

;; Copyright (C) 1987-2007 Kyle Jones
;; Author:       Kyle Jones
;; Maintainer:   Jari Aalto
;; Keywords:     extensions

;;{{{ id

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
;;{{{ Docs

;;  .................................................. &t-documentation ...

;;; Commentary:

;;
;;	Date: 12 Jan 89 17:36:19 GMT
;;
;;	Attached is an enhanced version of the `c-comment-edit' package, last
;;	posted sometime in 1987.
;;
;;	c-comment-edit is a command that copies a C comment into a
;;	temporary buffer for editing under a more suitable major mode
;;	(usually text-mode).  Once the comment is edited,
;;	c-comment-edit-end (normally bound to C-c ESC) replaces the old
;;	comment with the edited version, adding comment delimiters and
;;	leaders as necessary.  c-comment-edit is ideal for large comments
;;	of these styles:
;;
;; 	/*	/*	/*
;; 	  ...	 * ...	** ...
;; 	  ...	 * ...	** ...
;; 	*/	 */	*/
;;
;;	Features added:
;;
;;	o c-comment-edit no longer uses a recursive-edit so multiple
;;	  c-comment-edit's  be running simultaneously.
;;	o c-comment-edit will now search forward from point for a comment if
;;	  point is not within a comment.
;;	o c-comment-edit-hook is provided.
;;	o Bill Carpenter's c-comment-leader-regexp fixed was incorporated.
;;
;;	Kyle Jones

;;}}}
;;{{{ history

;; ......................................................... &t-history ...
;;; Change Log:
;;
;; Mar	03	2001	[jari]		20.7	v1.18		Released
;; - Added ###autoload items.
;;
;; Mar	23	1998	[jari]		19.34	v1.12		Released
;; - Byte compile errors fixed.
;;
;; Aug	11	1997	[jari]		19.28	v1.11		Released
;; - The example setup at the end worked fine, execept if used inside
;;   the function. The last line was not adjusted to
;;
;;   ***********/   but to   *********
;;                           */
;;
;;   That is; the last */ was not shifted to the end of line.
;; - corrected the example.
;;
;; Feb	24	1997	[jari]		19.28	v1.10		Released
;; - Rick Flower <flower@ms31.sp.trw.com> reported that c-indent-command
;;   wasn't seen by 19.15 byte compiler. Now requires cc-mode.
;;
;; Feb	26	1997	[jari]		19.28	v1.9		Released
;; - Small error in window configuration restore corrected.
;;
;; Dec	11	1996    [jari]          19.28   v1.6-1.8	Released
;; - Some minor byte compilation corrections. Window configuration
;;   bug corrected. c-comment-edit-at-point corrected in C++ mode.
;;
;; May	27	1996    [jari]          19.28   v1.5		Released
;; - Added saving/restoring the frame configuration. Previously the
;;   window layout was destroyed when the c-comment-edit-end was
;;   finished. Now windows are restored as they were.
;; - Made this package 18.xx compatible again.
;;
;; Apr	30	1996    [jari]          19.28   v1.4		Released
;; - Jerome Santini <santini@chambord.univ-orleans.fr> reported problems
;;   in 19.30 with the font-lock code. Now the eval-when-compile
;;   forms take in effect only if font-lock is not present. This was
;;   serious bug.
;; - Renamed all the rest functions to have prefix "c-comment". This
;;   follows the GNU package guidelines.
;; - Added defontifying a comment in separate buffer to get clear look.
;;
;; Oct	14	1995    [jari]          19.28   v1.3		Released
;; - Corrected the LCD entry, that was mistakenly modified.
;;
;; Sep	8	1995    [jari]          19.28   v1.2		NotReleased
;; - Cleaned up the Newsereader headers and presedved only 'date:'
;;   of original introduction
;;   within-c-comment-p		:!  renamed to comment-area, more general
;; - Added new function c-comment-edit-at-point.  If point is inside of a
;;   comment, the comment is edited.  Otherwise, a new comment is created
;;   at point. [jcolman]
;; - Cleared byte-compiler errors. Added optional parameters to main function
;;   c-comment-edit. Main also now returns more gracefully, if comment
;;   syntax isn't defined. This really should be made more general..
;;
;; Sep	4	1995    [jari]          19.28   v1.1		NotReleased
;; - Jake Colman <jcolman@j51.com> sent mail asking to crrect some
;;   things. Great that I get user feedback!
;; - kill-all-local-variables call removed
;; - renamed  c-comment-edit-after-hook --> c-comment-edit-exit-hook
;;   and moved it to the end of defun.
;; - The fill-column setting has been adjusted to include "/*" (-2 from the
;;   original setting).
;; - When edited comment is inserted back, the colors are gone. This has been
;;   corrected to re-fontify the comment if font-lock-mode is on.
;; - Added c-comment-edit-load-hook
;;
;; Apr	28	1995    [jari]          19.28   v0.2		NotReleased
;; - After talking with Kyle directly, he said that I should load
;;   this .el  with some other name, because he didn't plan to have
;;   any enhancement support. He said that I had been among the very
;;   few that had ever asked any changes to this module.
;; - On my behalf, since this is almost identical copy of the
;;   original, I have no objections that someone else modifies this
;;   .el with the "c-comment-edit2.el" -- I do plan to update this if it has
;;   errors that are caused by me of course, but right now I don't
;;   think it misses anything crucial. Just drop me a note, I love to hear
;;   about new improvements.
;;
;; Feb	22	1995    [jari]          19.28   v0.1		NotReleased
;; - I had used this intensively when 18.57 was still around in our
;;   envinronment (about 4 moths ago), but when I moved to 19.28
;;   I found some incompatibilities in keybindings and I dislike the
;;   comment syntax which left first line empty. Now there is variable
;;   to configure 1st line layout.
;; - Now ESC ESC terminates in 19 and ESC in 18.
;; - Added few confortable variables
;;   *  c-comment-edit-bname , c-comment-edit-empty-1-line
;;   *  c-comment-edit-other-buffer , c-comment-edit-C-buf
;;   *  c-comment-edit-[bc,ec]
;; - Added hooks
;;   *  c-comment-edit-end-hook
;;   *  c-comment-edit-after-hook
;;
;; - touched original code:
;;   *  c-comment-edit
;;      - there was one problem with this, the hook was too early
;;        run compared to kill-local-var, so if you turned on some minor
;;        mode, it was effectively lost. --> now the hook runs last.
;;   *  c-comment-edit-end
;;      - Added the new functionality here, many changes, sets global
;;        variables + runs hooks
;;
;; - There is an EXAMPLE section at the end of this file which is one of
;;   my favourite C/C++ function header. And cleanup function to
;;   retain the format after comment has been added too.

;;}}}

;;{{{ setup: bind, hooks

;;; Code:

(require 'cc-mode)
(load "c-mode" 'noerr)			;Hm, XEmacs 19.13 lacks this?
(eval-when-compile (require 'cl))

;;; .......................................................... &v-bind ...

(defvar c-com-mode-map nil  "C comment edit map")

(if c-com-mode-map
    nil
  (setq c-com-mode-map (make-sparse-keymap))
  ;; keys;
  (if (string< emacs-version "19")
      (progn
        (define-key c-com-mode-map "\C-c\C-c" 'c-comment-edit-end)
        (define-key c-com-mode-map "\e" 'c-comment-edit-abort))
    (define-key c-com-mode-map "\C-c\C-c" 'c-comment-edit-end)
    (define-key c-com-mode-map "\e\e" 'c-comment-edit-abort)))

;;;  ......................................................... &v-hooks ...

(defvar c-comment-edit-hook nil
  "*Function to call whenever `c-comment-edit' is used.
The function is called just before the `c-comment-edit' function allows you to
begin editing the comment.")

(defvar c-comment-edit-exit-hook nil
  "*Enables you to do some cleanup after edit is done, not called
if user aborted the action. Buffer is already inserted back when this
hook is called.")

(defvar c-comment-edit-end-hook nil
  "*When user has pressed C-c or ESC to complete editing, the
Comment prefix lines are drawn. After it has completed drawing,
and the buffer is in ready to be inserted back, this hook will be called. ")

(defvar c-comment-edit-load-hook nil
  "*Run when file has been loaded.")

;;}}}
;;{{{ setup: read-only vars

;;; ....................................................... &v-private ...
;;; These are set by funcs, user can check the values in hooks.

(defconst c-comment-edit-beg-c nil
  "After the comment is edited, this variable contains
begin MARK of comment.")

(defconst c-comment-edit-end-c nil
  "After the comment is edited, this variable contains
end MARK of comment.")

(defconst c-comment-edit-bufc nil
  "After the comment is edited, this variable contains C-code buffer name,
where comment edited belonged.")

;;}}}
;;{{{ setup: -- user config

;;; .......................................................... &v-conf ...

(defvar c-comment-window-register  ?w
  "*Which register to use to save window configuration.")

;;;###autoload
(defvar c-comment-leader " *"
  "*Leader used when rebuilding edited C comments.  The value of this variable
should be a two-character string.  Values of \"  \", \" *\" and \"**\"
produce the comment styles:
        /*	/*	/*
          ...	 * ...	** ...
          ...	 * ...	** ...
        */	 */	*/
respectively.")

(defconst c-comment-leader-regexp "^[ 	]*\\(\\*\\*\\|\\*\\)?[ ]?"
  "Regexp used to match C comment leaders.")

(defvar c-comment-edit-mode 'text-mode
  "*Major mode used by `c-comment-edit' when editing C comments.")

(defvar c-comment-edit-buffer-alist nil
  "Assoc list of C buffers and their associated comment buffers.
Elements are of the form (C-BUFFER COMMENT-BUFFER COMMENT-START COMMENT-END)
COMMENT-START and COMMENT-END are markers in the C-BUFFER.")

(defvar c-comment-edit-bname " *C Comment Edit*"
  "*buffer name to edit the comment")

(defvar c-comment-edit-empty-1-line nil
  "*This determines if the first comment line will be left empty

/*
 * comment begin, when value is t
 */
")

(defvar  c-comment-edit-other-buffer t
  "*Set to nil if you want to edit in full buffer")

;;}}}
;;{{{ code: macros

;;;  ########################################################## &Macros ###

(defmacro c-comment-save-point (&rest body)
  "Save value of point, evalutes FORMS and restore value of point.
If the saved value of point is no longer valid go to (point-max).
The variable `save-point' is lambda-bound to the value of point for
the duration of this call."
  (list 'let '((save-point (point)))
        (list 'unwind-protect
              (cons 'progn body)
              '(goto-char (min (point-max) save-point)))))

(defmacro c-comment-marker (pos &optional buffer)
  (list 'set-marker '(make-marker) pos buffer))

;;}}}

;;; ########################################################### &Funcs ###

;;{{{ code: general

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-save-state  ()
  "Save window configuration."
  (if (fboundp 'point-to-register-compatibility-binding)
      (funcall
       (symbol-function 'point-to-register-compatibility-binding)
       c-comment-window-register
       'window-config)))

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-restore-state  ()
  "Save window configuration."
  (if (and c-comment-window-register
           (get-register c-comment-window-register)
           (fboundp 'jump-to-register-compatibility-binding))
      (funcall
       (symbol-function 'jump-to-register-compatibility-binding)
       c-comment-window-register)))

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-area (beg end)
  "Searches area bounds delimited by strings BEG and END.
First searches backward, them forward.

Returns:
  (beg-point . end-point)
  nil."
  (condition-case nil
      (let (p pp)
        (c-comment-save-point
         (search-backward beg)
         (setq p (point))
         (search-forward end)
         (setq pp (point)))
        (if (< (point) pp) (cons p pp) nil))
    (search-failed
     nil)))

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-find-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
        (if (eq (nth 1 (car list)) buffer)
            (throw 'return-value (car list))
          (setq list (cdr list)))))))

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-find-c-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
        (if (eq (car (car list)) buffer)
            (throw 'return-value (car list))
          (setq list (cdr list)))))))

;;}}}

;;{{{ code: edit

;;; ----------------------------------------------------------------------
;;; 1995-09-07 Jake Colman <jcolman@j51.com> supplied basic code
;;;
;;;###autoload
(defun c-comment-edit-at-point ()
  "Edit C comment at point.
If point is inside of a comment, the comment is edited.  Otherwise, a new
comment is created at point.
"
  (interactive)
  (let* ((cs	   comment-start)
         (ce	   comment-end)
         comment)
    (when (memq major-mode '(c-mode c++-mode cc-mode))
      ;;    override the comment settings, because C++ has
      ;;    "//" and "" by default and that's not what we want
      (setq   cs "/*"   ce "*/"))

    (if (c-comment-area cs ce)
        (c-comment-edit nil)
      ;;  The catch is, that we first create a comment and then
      ;;  pass control to the main.
      (setq comment  (concat cs " "    ce))
      (insert comment)
      (goto-char (- (point) (length comment))) ;go inside it
      (c-indent-command)
      (c-comment-edit nil))))

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-edit-end ()
  "End c-comment-edit.
C comment is replaced by its edited counterpart in the appropriate C buffer.
Indentation will be the same as the original."
  (interactive)
  (let ((tuple		(c-comment-find-buffer))
        (line1-empty	c-comment-edit-empty-1-line)
        (i		0)
        edited
        char-count)

    (if (null tuple)
        (error "Not a c-comment-edit buffer."))

    (let ((inhibit-quit		t)
          (c-comment-c-buffer	(car tuple))
          (c-comment-buffer	(nth 1 tuple))
          (c-comment-start	(nth 2 tuple))
          (c-comment-end	(nth 3 tuple)))

      (cond
       ((buffer-modified-p)
        ;; rebuild the comment
        (goto-char (point-min))

        (if (null line1-empty)
            (insert "/*")
          (insert "/*\n"))

        (if (string= c-comment-leader "  ")
            (while (not (eobp))
              (setq i (1+ i))
              (if (eq 1 i)
                  (insert " ")
                (if (not (eolp)) (insert c-comment-leader " ")))
              (forward-line))
          (setq i 0)
          (while (not (eobp))
            (setq i (1+ i))
            (if (and (eq 1 i) (null line1-empty))
                (insert " ")
              (insert c-comment-leader (if (eolp) "" " ")))
            (forward-line)))

        (if (not (char-equal (preceding-char) ?\n))
            (insert "\n"))

        (insert (if (string= c-comment-leader " *") " */" "*/"))

        ;; indent if necessary
        (let ((indention
               (save-excursion
                 (set-buffer c-comment-c-buffer)
                 (goto-char c-comment-start)
                 (current-column))))
          (goto-char (point-min))
          (unless (zerop indention)
            ;; first line is already indented
            ;; in the C buffer
            (forward-line)
            (while (not (eobp))
              (indent-to indention)
              (forward-line))))

        (setq edited t)			;Raise the Flag
        (run-hooks 'c-comment-edit-end-hook)

        ;; replace the old comment with the new

        (save-excursion
          (setq char-count (- (point-max) (point-min)) )
          (set-buffer c-comment-c-buffer)

          (delete-region c-comment-start c-comment-end)
          (goto-char c-comment-start)

          (insert-buffer c-comment-buffer)

          ;;  save values for possible hook function

          (setq c-comment-edit-beg-c (marker-position c-comment-start)
                c-comment-edit-end-c (+ c-comment-edit-beg-c char-count)
                c-comment-edit-bufc  c-comment-c-buffer)

          ;;  The colors vanished, when we deleted that region and inserted
          ;;  new comment into buffer, lets get them back

          (if (and (featurep 'font-lock)
                   (symbol-value 'font-lock-mode))
              (funcall
               (symbol-function 'font-lock-fontify-region)
               c-comment-edit-beg-c c-comment-edit-end-c))))

       (t
        (message "No change.")))

      ;; .................................................. cond

      (c-comment-restore-state)

      ;; switch to the C buffer

      (if (get-buffer-window c-comment-c-buffer)
          (select-window (get-buffer-window c-comment-c-buffer))
        (switch-to-buffer c-comment-c-buffer))

      ;; delete the window viewing the comment buffer

      (and (get-buffer-window c-comment-buffer)
           (delete-window (get-buffer-window c-comment-buffer)))

      ;; unlink the tuple from c-comment-edit-buffer-alist

      (setq c-comment-edit-buffer-alist
            (delq tuple c-comment-edit-buffer-alist))

      ;; let Emacs reclaim various resources

      (save-excursion
        (set-buffer		c-comment-buffer)
        (set-buffer-modified-p	nil)
        (kill-buffer		c-comment-buffer))

      ;;  Now kill the markers so that they don't consume resources

      (set-marker c-comment-start	nil)
      (set-marker c-comment-end		nil))

    (if edited				;only if touched the contents
        (run-hooks 'c-comment-edit-exit-hook))))

;;}}}
;;{{{ code: abort

;;; ----------------------------------------------------------------------
;;;
(defun c-comment-edit-abort ()
  "Abort a c-comment-edit with no change."
  (interactive)
  (let* ((tuple (c-comment-find-buffer))
         (c-comment-c-buffer (car tuple))
         (c-comment-buffer (nth 1 tuple))
         (c-comment-start (nth 2 tuple))
         (c-comment-end (nth 3 tuple)))

    (if (null tuple)
        (error "Not a c-comment-edit buffer."))

    ;; switch to the C buffer

    (if (get-buffer-window c-comment-c-buffer)
        (select-window (get-buffer-window c-comment-c-buffer))
      (switch-to-buffer c-comment-c-buffer))

    (let ((inhibit-quit t))
      (save-excursion
        (set-buffer c-comment-buffer)
        (set-buffer-modified-p nil)
        (if c-comment-edit-other-buffer
            (delete-window))
        (kill-buffer c-comment-buffer))
      ;; unlink the tuple from c-comment-edit-buffer-alist
      (setq c-comment-edit-buffer-alist
            (delq tuple c-comment-edit-buffer-alist))
      (set-marker c-comment-start nil)
      (set-marker c-comment-end nil)
      (message "Aborted with no change.")
      (c-comment-restore-state))))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun c-comment-edit (&optional search-prefix com-start com-end)
  "Edit multi-line C comments.
This command allows the easy editing of a multi-line C comment like this:
   /*
    * ...
    * ...
    */
The comment may be indented or flush with the left margin.

If point is within a comment, that comment is used.  Otherwise the
comment to be edited is found by searching forward from point.

With one \\[universal-argument] searching starts after moving back one
  paragraph.
With two \\[universal-argument]'s searching starts at the beginning of the
  current or proceeding C function.
With three \\[universal-argument]'s searching starts at the beginning of the
  current page.
With four \\[universal-argument]'s searching starts at the beginning of the
  current buffer (clipping restrictions apply).

Once located, the comment is copied into a temporary buffer, the comment
leaders and delimiters are stripped away and the resulting buffer is
selected for editing.  The major mode of this buffer is controlled by
the variable `c-comment-edit-mode'.

Use \\[c-comment-edit-end] when you have finished editing the comment.  The
comment will be inserted into the original buffer with the appropriate
delimiters and indention, replacing the old version of the comment.  If
you don't want your edited version of the comment to replace the
original, use \\[c-comment-edit-abort]."
  (interactive "*P")
  (catch 'out
    (let ((inhibit-quit	t)
          (bname		c-comment-edit-bname)
          (other-bedit	c-comment-edit-other-buffer)
          marker
          cs ce
          tem
          c-comment-fill-column
          c-comment-buffer
          c-comment-start
          c-comment-end)

      (cond
       ((and com-start com-end)         ;don't touch they are set.
        nil)

       ((memq major-mode '(c-mode c++-mode cc-mode))
        ;;    override the comment settings, because C++ has
        ;;    "//" and "" by default and that's not what we want
        (setq   cs "/*"   ce "*/"))

       (t
        (setq cs (or comment-start
                     "cbAnything##"))
        (setq ce (or comment-end
                     "ceAnything##"))))

      ;;  What was the prefix arg ?

      (cond ((equal search-prefix '(4))
             (backward-paragraph))
            ((equal search-prefix '(16))
             (end-of-defun)
             (beginning-of-defun)
             (backward-paragraph))
            ((equal search-prefix '(64))
             (backward-page))
            ((equal search-prefix '(256))
             (goto-char (point-min))))

      (if (and (null search-prefix)
               (setq tem (c-comment-area cs ce)))
          (setq c-comment-start (c-comment-marker (car tem))
                c-comment-end   (c-comment-marker (cdr tem)))
        (let (start end)
          (condition-case nil
              (c-comment-save-point
               (search-forward cs)
               (setq start (- (point) (length cs)))
               (search-forward ce)
               (setq end (point)))
            (search-failed
             (message
              (concat "No C comment found. Check comment-start: "
                      cs "|" ce))
             (throw 'out t)))
          (setq c-comment-start (c-comment-marker start))
          (setq c-comment-end   (c-comment-marker end))))

      ;; calculate the correct fill-column for the comment

      (setq c-comment-fill-column
            (- fill-column
               (save-excursion
                 (goto-char c-comment-start)
                 (+ (length comment-start) (current-column)))))

      ;; create the comment buffer

      (setq c-comment-buffer
            (generate-new-buffer (concat (buffer-name) bname)))

      ;; link into the c-comment-edit-buffer-alist

      (setq c-comment-edit-buffer-alist
            (cons (list (current-buffer) c-comment-buffer
                        c-comment-start c-comment-end)
                  c-comment-edit-buffer-alist))

      ;; copy to the comment to the comment-edit buffer

      (copy-to-buffer c-comment-buffer
                      (+ c-comment-start 2) (- c-comment-end 2))

      ;; mark the position of point, relative to the beginning of the
      ;; comment, in the comment buffer.  (if point is within a comment.)

      (or search-prefix (< (point) c-comment-start)
          (setq marker (c-comment-marker (+ (- (point) c-comment-start 2) 1)
                                         c-comment-buffer)))
      ;; ...............................................................
      ;; select the comment buffer for editing

      (c-comment-save-state)

      (if (null other-bedit)
          (switch-to-buffer c-comment-buffer)
        (switch-to-buffer-other-window c-comment-buffer))

      ;; remove the comment leaders and delimiters

      (goto-char (point-min))

      (while (not (eobp))
        (and (re-search-forward c-comment-leader-regexp nil t)
             (replace-match "" nil t))
        (forward-line))

      ;; run appropriate major mode

      (funcall (or c-comment-edit-mode 'fundamental-mode))

      ;; override user's default fill-column here since it will lose if
      ;; the comment is indented in the C buffer.

      (setq fill-column c-comment-fill-column)

      ;; delete one leading whitespace char

      (goto-char (point-min))

      (if (looking-at "[ \n\t]")
          (delete-char 1))

      ;; restore cursor if possible

      (goto-char (or marker (point-min)))

      ;; defontify to get a clear look at text

      (put-text-property (point-min) (point-max) 'face 'default)

      (set-buffer-modified-p nil))
    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ let end ^^^

    (use-local-map  c-com-mode-map)
    (run-hooks 'c-comment-edit-hook)

    (message
     (substitute-command-keys
      (concat "Type \\[c-comment-edit-end] to end edit, "
              "\\[c-comment-edit-abort] to abort with no change.")))))

    ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch end ^^^

;;}}}

;;{{{ Example
;;; ......................................................... &example ...

;; - Here is ready setup, which you could use right away.
;; - I am used to program all my C/C++ function like this, where the
;;   header is just before each function:
;;
;; /*************************************************************************
;;  * <PUBLIC> FUNCTION: MyFunc
;;  *************************************************************************
;;  * DESCRIPTION
;;  * - This is function is the main entry point to class myClass.
;;  *   it handles reading the oracle database....
;;  *
;;  * SPECIAL
;;  * - Note, that the oracle connection must be verified before your're
;;  *   using this function....
;;  *
;;  * RETURNS
;;  * - Creates object errorAtom which hold data about the promlem occurred.
;;  *   other
;;  *************************************************************************/
;; errorAtom_c *myClass_c::Execute(char * ptr)
;; {
;;
;; }
;;
;;
;; - In order to maintain the '*****' breaks correctly you have to use some
;;   cleanup function like one below. It detects if the Comment has
;;   '****' in it and does nothing if it's regular comment.
;;
;; - Remember that when the comment has been edited, the comment style you
;;   choosed, affects the function. This supposes you have use the 'one star'
;;   style.

;;  Setting proper hooks.
;;  I seldom need M-c (capitalize word) in C/C++
;;  If you use many hooks, use command add-hook instead.
;;
;;  (setq c++-mode-hook  'c++-my-hook)
;;  (defun c++-my-hook ()
;;    (local-set-key "\M-c" 'c-comment-edit))
;;
;; (setq c-mode-hook  'c-my-hook)
;; (defun c-my-hook ()
;;   (local-set-key "\M-c" 'c-comment-edit))

;; (defun my-com-end ()
;;   "C- comment edit cleanup."
;;   (let* ((sep (make-string 70 ?* ))	;what separator you want to use
;;  	 (fix-re "[-=*] [-=*][-=*]" )	;the gap " " is in buffer
;;  	 (back-step 3)			;depends on the fix-re
;;  	 (break-re " +[-=*][-=*][-=*]*") ;at least two continuous chars
;;  	 )
;;
;;     ;;  To preserve indentation. Remember that C-comment markers are
;;     ;;  added to the beginning
;;
;;     (untabify (point-min) (point-max))
;;
;;
;;     ;;  - We are in comment buffer now, so we can move freely with goto-char
;;     ;;  - fix all break-marked lines to certain length
;;
;;     (goto-char (point-min))
;;     (while (re-search-forward fix-re nil t)
;;       (backward-char back-step)  (kill-line) (insert sep))
;;
;;     ;;  - Check if the last line has separator == it is function header
;;     ;;  - The last line holds "*/", so look at the previous one.
;;
;;     (goto-char (point-max))    (forward-line -1)
;;
;;     (cond
;;      ((looking-at break-re)
;;       ;; Remove that lonely "*/" and shift it one line up
;;       ;;
;;       (goto-char (point-max)) (beginning-of-line)
;;       (kill-line)
;;       (backward-delete-char 1)
;;       (insert "/")))			;terminate C comment
;;     nil))					;hook must return this

;;}}}

(provide   'c-comment-edit)
(run-hooks 'c-comment-edit-load-hook)

;;; c-comment-edit2.el ends here
