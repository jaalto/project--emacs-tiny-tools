;;; dired-sort.el --- Sort by by size, date, field, name and type

;; This file is not part of Emacs

;; {{{ Id

;; Maintainer:      Jari Aalto
;; Created:         1989-03
;; Keywords:        extensions
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
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;; }}}
;; {{{ Install

;;; Install:

;;  Put this file on your Emacs-Lisp `load-path', add following into your
;;  ~/.emacs startup file.
;;
;;      (add-hook 'dired-mode-hook '(lambda () (require 'dired-sort)))
;;
;;  NOTE: ls-lisp.el, which dired.el uses, by default inserts month names
;;  in national format. If the setting is anything other than English,
;;  this module *cannot* parse the dired lines. Please add this setting
;;  to your Emacs (21.4+) to make dired listing use ISO 8601 date stamps:
;;
;;   (setq ls-lisp-format-time-list
;;       '("%Y-%m-%d %H:%M"
;;         "%Y-%m-%d      "))

;; }}}
;; {{{

;;; Commentary:

;;
;;  Preface, Nov 1997
;;
;;      Not much to say. I have had this package lying in my lisp directory
;;      since Emacs 18.xx days. When I noticed that this package doesn't
;;      exist in the OHIO archive, neither did the ftpsearch locate it,
;;      I decided to clen it up and put publically available.
;;
;;      The original author is unknown And the only thing that was in the
;;      original documentation was this line:
;;
;;      $Header: /tmp_mnt/am/p7/utility/gmacs/f2/RCS/dired-resort.el,v
;;      1.1 88/11/03 13:22:08 fad Exp $
;;
;;  New bindings in dired
;;
;;      When you load this file, function `dired-sort-default-keys' is called.
;;      The following bindings to dired mode are defined.
;;
;;          S" "    dired-sort-resort    (that's an "S" + SPACE)
;;          Ss      dired-sort-by-size
;;          Sd      dired-sort-by-date
;;          Sf      dired-sort-by-field
;;          Sn      dired-sort-by-name
;;          St      dired-sort-by-type
;;
;; }}}

;;; Change Log:

;;; Code:

;;(require 'date-parse)

(eval-and-compile
  ;;  Silence Byte compiler
  (defvar revert-buffer-function)
  (defvar current-prefix-arg) ;; Elint.el
  ;; Not exported from sort.el
  (autoload 'sort-subr                "sort")
  (autoload 'sort-fields-1            "sort")
  (autoload 'dired-revert             "dired")
  (autoload 'dired-get-filename       "dired")
  (autoload 'dired-move-to-filename   "dired")
  (autoload 'sort-skip-fields         "sort")
  (autoload 'parse-date               "date-parse")
  (autoload 'date-compare-key         "date-parse"))

;;; ....................................................... &variables ...

(defvar dired-sort-load-hook '(dired-sort-default-keys)
  "Hook run when file is loaded.")

;; File property caching mechanism for dired

(defvar dired-sort-line-property-table nil
  "Buffer local obarray:
Each symbol is a file name whose plist caches file properties,
accessed by #'dired-line-property")
(make-variable-buffer-local 'dired-sort-line-property-table)

(defvar dired-sort-resort-last-kind '(date)
  "What the last sort did to the buffer.")
(make-variable-buffer-local 'dired-sort-resort-last-kind)

(defvar dired-sort-resort-alist
  '(("name" dired-sort-by-name nil "ascending order")
    (nil dired-sort-by-name t "descending order")
    ("date" dired-sort-by-date nil "most recent first")
    (nil dired-sort-by-date t "oldest first")
    ("size" dired-sort-by-size nil "biggest first")
    (nil dired-sort-by-size t "smallest first")
    ("type" dired-sort-by-type t "alphabetically")
    ("modes" dired-sort-by-field 1 "file modes")
    ("links" dired-sort-by-field 2 "number of links")
    ("owner" dired-sort-by-field 3 "file owner")
    ("field" dired-sort-by-field (1) "textual field")))

;;; ............................................................ &code ...

(defvar dired-sort-last-sort  nil
  "Last sort indication.")

(defun dired-sort-revert-and-decache (&optional arg noconfirm)
  "Revert buffer using `dired-revert' ARG and NOCONFIRM."
  (if dired-sort-line-property-table
      (mapatoms (function (lambda (file) (setplist file nil)))
                dired-sort-line-property-table))
  (dired-revert arg noconfirm))

(defun dired-sort-line-property (func)
  "Call FUNC with one argument:  The (absolute) file name of this dired line.
Cache the result, and return it the next time without calling FUNC.
   The caches are cleared when the buffer is reverted.
   See dired-sort-line-property-table."
  (or dired-sort-line-property-table
      (progn
        (if (eq revert-buffer-function 'dired-revert)
            (setq revert-buffer-function 'dired-sort-revert-and-decache))
        (setq dired-sort-line-property-table (make-vector 31 0))))
  (let ((file (intern (dired-get-filename t) dired-sort-line-property-table)))
    (or (get file func)
        (put file func
             (funcall func (symbol-name file))))))

(defun dired-sort-move-word-backward ()
  "move one space dlimited word backward. Must already be on word."
  (skip-chars-backward "^ \t" (line-beginning-position))
  (skip-chars-backward " \t" (line-beginning-position))
  (skip-chars-backward "^ \t" (line-beginning-position)))

;;   FIXME: It is unreliable to read words from dired buffer,
;;   because the Month name can be in national format.
;;   => There is no eas way, doing file stat() would be too
;;   expensive to find out the month name?
;;   => It is bets to configure Emacs to always use
;;   ISO dates only.

(defun dired-sort-move-to-date (&optional and-extract)
  "Details depend on the `dired-extract-size' AND-EXTRACT."
  ;;  Go two words backward
  ;;                         4694 Month 16   19:44 file
  ;; -rw-rw----   1 foo foo  2082 2004-10-14 17:23 .
  ;;                                               |
  ;;                                      start here
  (when (dired-move-to-filename)
    (let ((end (point)))
      (dired-sort-move-word-backward)
      (dired-sort-move-word-backward)
      ;;  Now, should we still take on leap due to Month name?
      (unless (looking-at "[0-9][0-9][0-9][0-9]-")
        (dired-sort-move-word-backward))
      (if and-extract
          (parse-date (buffer-substring (point) end) t)
        (point)))))

(defun dired-sort-extract-date ()
  "Call `dired-sort-move-to-date'."
  (dired-sort-move-to-date t))

(defun dired-sort-extract-size ()
  "Read size with regular expression."
  (let ((ret -1))
    (when (dired-sort-move-to-date)
      (skip-chars-backward " " (line-beginning-position))
      (skip-chars-backward "0-9" (line-beginning-position))
      (if (looking-at "[0-9]+ ")
          (setq ret (read (current-buffer)))))
    ret))

(defun dired-sort-header-line-p ()
  "Check `dired-sort-extract-size'."
  (save-excursion
    (minusp (dired-sort-extract-size))))

(defun dired-sort-first-file ()
  "Goto first file."
  (interactive)
  (goto-char (point-min))
  (while (and (dired-sort-header-line-p)
              (not (eobp)))
    (forward-line 1))
  (dired-move-to-filename))

(defun dired-sort-extract-date-key (&optional ignore)
  "Extract key with IGNORE."
  (let ((date (dired-sort-extract-date)))
    (if date
        (date-compare-key date 'integer))))

(defun dired-sort-by-size-key (&optional ignore)
  "Sort by zise or IGNORE."
  (dired-sort-by-size-key-1 nil))

(defun dired-sort-by-size-increasing-key (&optional ignore)
  "Sort by zise or IGNORE."
  (dired-sort-by-size-key-1 t))

(defun dired-sort-resort-menu-options ()
  "See `dired-sort-resort-alist'."
  (list "Help"
        (cons "Sort Dired listing by:"
              (mapcar
               (function(lambda (elt)
                          (cons
                           (format "%5s (%s)"
                                   (capitalize (or (nth 0 elt) " '' "))
                                   (nth 3 elt))
                           elt)))
               dired-sort-resort-alist))))

(defun dired-sort-by-size-key-1 (incr-p)
  "Sort possibly with INCR-P."
  (let ((size (dired-sort-extract-size))
        (char (save-excursion
                (forward-line 0)
                (skip-chars-forward " ")
                (following-char))))
    (setq char (downcase char))
    (cond
     ((not incr-p))
     ((= char ?-) (setq char ?~))
     ((>= char ?a) (setq char (- (+ ?a ?z) char))))
    (format "%c%09d" char size)))

(defun dired-sort-read-resort-args (&optional res)
  "Produce a 1- or 2- list.
Suitable for non-interactive calling of dired-sort-resort.
Optional RES is a line from dired-sort-resort-alist."
  (or res
      (setq res
            (completing-read
             (format "Sort by: [%s] " (car dired-sort-resort-last-kind))
             dired-sort-resort-alist
             nil t)))
  (if (zerop (length res))
      dired-sort-resort-last-kind
    (if (atom res)
        (setq res (or (assoc res dired-sort-resort-alist)
                      (error "reading resort"))))
    (let ((type (nth 0 res))
          (func (nth 1 res))
          (arg (nth 2 res))
          (what (nth 3 res)))
      (let ((ptr dired-sort-resort-alist) elt)
        (while (and ptr (null type))
          (setq elt (car ptr) ptr (cdr ptr))
          (if (eq func (nth 1 elt))
              (setq type (nth 0 elt)))))
      (setq type (intern type))
      (cond
       ((atom arg))
       (current-prefix-arg
        (setq arg
              (if (integerp (car arg))
                  (prefix-numeric-value current-prefix-arg)
                (and current-prefix-arg t))))
       ((integerp (car arg))
        (setq arg (read-string (format "What %s? " what))))
       (t (setq arg (y-or-n-p (format "%s? " what)))))
      (if (null arg)
          type
        (list type arg)))))

;;; ..................................................... &interactive ...

(defun dired-sort-last-file ()
  "Go to last file."
  (interactive)
  (goto-char (point-max))
  (while (and (dired-sort-header-line-p)
              (not (bobp)))
    (forward-line -1))
  (dired-move-to-filename))

(defun dired-sort-narrow-to-files ()
  "Narrow to visible files."
  (interactive)
  (narrow-to-region
   (save-excursion
     (dired-sort-first-file)
     (forward-line 0)
     (point))
   (save-excursion
     (dired-sort-last-file)
     (forward-line 1)
     (point))))

;;;###autoload
(defun dired-sort-by-date (&optional arg)
  "In dired, sort the lines by date, newest first.
With ARG, sorts oldest first."
  (interactive "P")
  (save-restriction
    (dired-sort-narrow-to-files)
    (let (buffer-read-only)
      (goto-char (point-min))
      (sort-subr
       (not arg) 'forward-line 'end-of-line
       (function
        (lambda ()
          (or (dired-sort-line-property 'dired-sort-extract-date-key)))))))
  ;; (throw key 'nil)
  (setq dired-sort-last-sort (if arg
                                 'oldest
                               'newest))
  (message "Dired-sort: Now sorted by date, %s first."
           (if arg
               "oldest"
             "newest")))

(defun dired-sort-by-name (&optional arg skip-to sort-by)
  "In dired, sort the lines by file name.
With ARG, sorts in reverse order. SKIP-TO SORT-BY."
  (interactive "P")
  (or sort-by (setq sort-by 'name))
  (save-restriction
    (dired-sort-narrow-to-files)
    (let ((buffer-read-only nil)
          (reverse-sort-p arg))
      (goto-char (point-min))
      (sort-subr
       reverse-sort-p 'forward-line 'end-of-line
       (function(lambda ()
                  (dired-move-to-filename)
                  (cond
                   ((null skip-to))
                   (reverse-sort-p
                    (let ((here (point)))
                      (end-of-line)
                      (re-search-backward
                       skip-to here 'move)))
                   ((re-search-forward
                     skip-to
                     (save-excursion (end-of-line) (point))
                     'move)
                    (goto-char (match-beginning 0))))
                  nil)))))
  (setq dired-sort-last-sort sort-by)
  (message "Dired-sort: Now sorted by %s%s." sort-by
           (if arg
               ", in reverse order"
             "")))

;;;###autoload
(defun dired-sort-by-type (&optional arg)
  "Sort by type, ARG means reverse."
  (interactive "P")
  (dired-sort-by-name
   arg (if arg
           "[.#~]"
         "[.~0-9#]+")
   'type))

;;;###autoload
(defun dired-sort-by-field (field)
  "In dired, sort the lines by FIELD (defaults to the mode field)."
  (interactive "p")
  (save-restriction
    (dired-sort-narrow-to-files)
    (let ((buffer-read-only nil))
      (goto-char (point-min))
      (sort-fields-1
       field (point-min) (point-max)
       (function(lambda ()
                  (sort-skip-fields (1- field))
                  (skip-chars-backward " ")
                  nil))
       nil)))
  (setq dired-sort-last-sort 'fields)
  (message "Dired-sort: Now sorted by %s."
           (cond ((= field 1) "file mode")
                 ((= field 2) "number of links")
                 ((= field 3) "file owner")
                 (t (format "field #%d" field)))))

;;;###autoload
(defun dired-sort-by-size (&optional arg)
  "In dired, sort the lines by file size, largest first.
With ARG, sorts in the reverse order (smallest first).
All directories are grouped together at the head of the buffer,
and other file types are also grouped."
  (interactive "P")
  (let ((buffer-read-only nil)
        (incr-p arg))
    (save-restriction
      (dired-sort-narrow-to-files)
      (goto-char (point-min))
      (sort-subr
       (not incr-p) 'forward-line 'end-of-line
       (if incr-p
           (function (lambda () (dired-sort-line-property
                                 'dired-sort-by-size-increasing-key)))
         (function (lambda () (dired-sort-line-property
                               'dired-sort-by-size-key))))))
    (setq dired-sort-last-sort (if incr-p
                                   'smallest
                                 'largest))
    (message "Dired-sort: Now sorted by type and size, %s first."
             (if incr-p
                 "smallest"
               "largest"))))

;;;###autoload
(defun dired-sort-resort (kind &optional args)
  "In dired, change the sorting of lines. Prompt for the KIND of sorting.
Non-interactively, takes a sort-kind, and an optional argument for
the associated function. To get a list of such arguments interactively,
call dired-sort-read-resort-args. ARGS are passed to sort."
  (interactive (list (dired-sort-read-resort-args)))
  (if (null kind)
      (setq kind dired-sort-resort-last-kind))
  (if (consp kind)
      (setq args (cdr kind) kind (car kind)))
  (if (symbolp kind) (setq kind (symbol-name kind)))
  (apply
   (or (nth 1 (assoc kind dired-sort-resort-alist))
       (error "No such sorting method: %s" kind))
   args)
  (setq dired-sort-resort-last-kind (cons kind args)))

;;;###autoload
(defun dired-sort-default-keys-dired-mode-map ()
  "Define default bindings to dired map."
  (interactive)
  (let* ((map (symbol-value 'dired-mode-map)))
    (unless map
      (error "dired-sort.el: [ERROR] dired is not yet loaded."))
    (define-key map  "\C-cs"  nil)
    (define-key map  "\C-cs " 'dired-sort-resort)
    (define-key map  "\C-css" 'dired-sort-by-size)
    (define-key map  "\C-csd" 'dired-sort-by-date)
    (define-key map  "\C-csf" 'dired-sort-by-field)
    (define-key map  "\C-csn" 'dired-sort-by-name)
    (define-key map  "\C-cst" 'dired-sort-by-type)))

;;;###autoload
(defun dired-sort-default-keys ()
  "Define default bindings to dired map."
  (eval-after-load "dired"
    '(progn (dired-sort-default-keys-dired-mode-map))))

;;;###autoload
(add-hook  'dired-mode-hook 'dired-sort-default-keys 'end)

(run-hooks 'dired-sort-load-hook)
(provide   'dired-sort)

;;; dired-sort.el ends here
