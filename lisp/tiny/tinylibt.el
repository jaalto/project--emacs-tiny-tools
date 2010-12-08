;;; tinylibt.el --- Library for handling text properties.

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C)   1995-2010 Jari Aalto
;; Keywords:       extensions
;; Author:         Jari Aalto
;; Maintainer:     Jari Aalto
;;
;; To get information on this program, call M-x tinylibt-version.
;; Look at the code with folding.el

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even tqhe implied warranty of MERCHANTABILITY
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
;; Put this file on your Emacs-Lisp `load-path', add following into
;; ~/.emacs startup file.
;;
;;  (require 'tinylibt)
;;
;; No autoload is suggested, because almost every function would have
;; to be in autoload state. It's easier to use require. Here are
;; suggested keybings for interactive use.
;;
;;  (global-unset-key "\C-z")
;;  (global-set-key "\C-ztm" 'ti::text-mark-region)   ;; e.g. permanent 'mark'
;;  (global-set-key "\C-ztu" 'ti::text-unmark-region) ;; remove 'mark'
;;  (global-set-key "\C-ztc" 'ti::text-clear-buffer-properties)
;;  (global-set-key "\C-ztb" 'ti::text-buffer)
;;  (global-set-key "\C-ztU" 'ti::text-undo)
;;
;; If you have any questions or feedback, use this function
;;
;;      M-x ti::text-submit-bug-report

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      o   This package is primary aimed for programmers, but
;;          interactive users will also find handy functions.
;;      o   Show matched text with color in the buffer.
;;      o   This is *NOTHING* like `font-lock' or `lazy-lock'
;;          which are demand driven packages intended for certain major modes.
;;          Use this package to "manually" mark interesting things in
;;          any buffer.
;;      o   Examples: highlighting on/off tabs, Verifying PGP
;;          fingerprints against trusted key server list
;;      o   UNDO: adjustable stack size. Stack is cleared if
;;          stack limit reached (stack 'wraps')
;;
;;  User functions
;;
;;      Mostly this package is designed for programmers, who add
;;      some highlight commands in hooks. For quick text highlighting,
;;      you can use these interactive functions:
;;
;;          ti::text-looking-at
;;          ti::text-buffer              ;; Highlight in whole buffer area
;;          ti::text-re-search-forward
;;          ti::text-re-search-backward
;;          ti::text-undo
;;
;;          ti::text-clear-buffer-properties
;;          ti::text-clear-region-properties
;;
;;          ti::text-mark-region
;;          ti::text-unmark-region
;;
;;          ti::text-mouse-mark-region
;;          ti::text-mouse-unmark-region
;;
;;  Setting different face (programming)
;;
;;      If you want permanetly change the face, when marking text
;;      use commands
;;
;;          ti::text-search-face-set   ;to set
;;          ti::text-search-face-reset ;to get default color back
;;
;;      If you want temporarily use some face, supply direct FACE parameter
;;      when you call search functions, like:
;;
;;          ti::text-re-search-forward (re &optional level face)
;;
;;  Note
;;
;;      This is for simple text highlighting only. Like finding certain items
;;      or marking something quickly and temporarily (great for text files)
;;
;;      You can mix font-lock/hilit19 and TIMA package, but remember that
;;      these packages have different goals. Use TIMA only for finding
;;      things in buffer, or marking certain articles in gnus...
;;
;;      Be carefull: if you use `ti::text-clear-buffer-properties', you will
;;      wipe out all text properties.
;;
;;  Example: highlighting tabs
;;
;;          (global-set-key "\C-ct" 'my-tabs-highligh-in-buffer)
;;
;;          (defun my-tabs-highligh-in-buffer (&optional arg)
;;            "Toggless hilit/dehiliting tabs in buffer.
;;          If ARG is integer, force highlighting. If ARG is C-u, then
;;          force dehighlighting."
;;            (interactive "P")
;;            (let (prop)
;;              (save-excursion
;;                (ti::pmin)
;;                (when (re-search-forward "\t" nil t)
;;               ;; is the tab marked?
;;               (setq prop (get-text-property (1- (point)) 'face))
;;               (cond
;;                ((or (integerp arg)             ;; Do highlighting
;;                     (or (eq prop nil)
;;                         (eq prop 'default)))
;;                 (beginning-of-line)
;;                 (ti::text-re-search-forward "\t+"))
;;                (t
;;                 (beginning-of-line)
;;                 ;; Remove
;;                 (ti::text-re-search-forward "\t+" 0 'default )))))))
;;
;;  Example: finding PGP key matches
;;
;;          (defun my-pgp-fp-certify  ()
;;            "To certify keys, E.g. get list of remailers
;;             from http://www.uit.no/
;;          - Display in window1 the UIT.NO result file
;;          - Put received key fingerprints in other window (pgp -ka
;;            will tell you)
;;
;;          Call this function in the Received keys buffer, and it'll
;;          highlight keys that match Fingerprint in uit.no window."
;;            (interactive)
;;            (let* ((blist  (ti::window-list 'buffers))
;;                   (buffer (car (delq (current-buffer) blist)))
;;                   A
;;                   elt
;;                   ok)
;;              (ti::pmin)
;;              (while (re-search-forward
;;                       "Key fingerprint.*= +\\(.*\\)" nil t)
;;                (setq elt (match-string 1)  ok nil)
;;                (setq A elt)
;;                (save-excursion
;;               (set-buffer buffer)
;;               (ti::pmin)
;;               (setq ok (ti::text-re-search-forward elt)))
;;                (when ok
;;               (beginning-of-line)
;;               (ti::text-looking-at ".*"))
;;                (end-of-line))))
;;

;;}}}

;;; Change Log:

;;; Code:

;;{{{ setup: require

;;; ......................................................... &require ...

(require 'tinylibm)

(ti::package-defgroup-tiny TinylibText ti::text-: extensions
  "Mark matched text in buffer with face.
  Overview of features

        o   This package is primary aimed for elisp programmers, but
            interactive users will also find handy functions.
        o   Shows matched text with color in the buffer.
        o   This is *NOTHING* like font-lock, lazy-lock or hilit19,
            which are demand driven packages intended for certain major modes.
            Use this package to manually mark interesting things in
            any buffer.
        o   Examples: highlighting on/off tabs, Verifying PGP
            fingerprints against trusted list like http://www.uit.no/
        o   UNDO: adjustable stack size. Stack is cleared if
            stack limit reached (stack 'wraps')")

;;}}}
;;{{{ setup: variables

;;; ......................................................... &v-hooks ...

(defcustom ti::text-:load-hook nil
  "*Hook run when file has been loaded."
  :type 'hook
  :group 'TinylibText)

(defcustom ti::text-:stack-size 1000
  "*How big undo history."
  :type 'number
  :group 'TinylibText)

;;; ....................................................... &v-private ...

(defvar ti::text-:stack-count nil
  "Incremented after every search. Do not touch.")

(defvar ti::text-:stack-push-flag nil
  "Non-nil  means ('undo-func) do not record match data to stack.
If this variable has value 'undo-func then the next calls to
`ti::text-re-search' won't record data to stack.")

(defvar ti::text-:stack nil
  "Private. Contain last search data.
This is actually property list stack so that undo can be done.

Format:

  '(start-point
    last-func
    last-re
    las-beg-point
    last-level
    last-mode)")

;;; ........................................................ &v-public ...
;;; user configurable

(defcustom ti::text-:face-tab-search-default 'highlight
  "*Default face used when marking searched text."
  :type '(symbol :tag "Face symbol")
  :group 'TinylibText)

;;; For now, only search face is used, but maybe in the future the others..
;;;
(defcustom ti::text-:face-table
  (list
   (cons 'search 'highlight)
   (cons 'warn   (if (ti::emacs-p) 'region 'bold)) ;XE doesn't have 'region face
   (cons 'head    'bold))
  "*Faces used for marking text."
  :type '(repeat
          (list
           (symbol :tag "symbolic face name"
                   (symbol :tag "Face name"))))
  :group 'TinylibText)

;;}}}
;;{{{ version

(eval-and-compile
  (ti::macrof-version-bug-report
   "tinylibt.el"
   "tinulibt"
   ti::text-:version-id
   "$Id: tinylibt.el,v 2.46 2007/05/06 23:15:20 jaalto Exp $"
   '(ti::text-:version-id
     ti::text-:load-hook
     ti::text-:stack-size
     ti::text-:stack-count
     ti::text-:stack-push-flag
     ti::text-:stack
     ti::text-:face-search-default
     ti::text-:face-table)))

;;}}}
;;{{{ code: misc funcs

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::text-search-face-reset ()
  "Reset used face to the default value.
If you use many colours to highlight text. Remember to call this
when you're finished."
  (list
   'setcdr (list 'assq ''search 'ti::text-:face-table)
   'ti::text-:face-search-default))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::text-search-face-set (face)
  "Change search colour to FACE."
  (list 'setcdr (list 'assq ''search 'ti::text-:face-table) face))

;;; ----------------------------------------------------------------------
;;;
(defmacro ti::text-face (face)
  "Return real face when logical FACE is given."
  ;;  This way the global variable does not float around the file
  (list 'cdr (list 'assoc face 'ti::text-:face-table)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::text-stack-clear ()
  "Clear undo stack."
  (put 'ti::text-:stack 'definition-stack nil)
  (setq ti::text-:stack-count  0
        ti::text-:stack        nil))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::text-stack-length ()
  "Return undo stack length."
  (length (get 'ti::text-:stack 'definition-stack)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::text-stack-full-p ()
  "Check if stack is full."
  (eq (ti::text-stack-length) (1+ ti::text-:stack-size)))

;;; ----------------------------------------------------------------------
;;;
(defsubst ti::text-stack-p ()
  "Check if there is data in undo stack. nil means that stack is empty."
  (or (get 'ti::text-:stack 'definition-stack)
      ;;  Make sure this is also zero because there is no data
      (progn (setq ti::text-:stack-count 0) nil)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-save-data (re level func mode beg)
  "Save search values RE LEVEL FUNC MODE BEG for undo.
If the stack is full, then Clear the stack before pushing to it."
  (or (integerp ti::text-:stack-count)
      (setq ti::text-:stack-count (ti::text-stack-length) ))
  (when (and (stringp re)
             (not (eq ti::text-:stack-push-flag 'undo-func)))

    ;; The last entry in the stack is always nil, that's why
    ;; 1+.
    ;;
    ;; inital:      nil
    ;; 1st:         '(mil)              pushed last data; size 1
    ;; 2nd          '((..) nil)         pushed next, size 2
    ;;
    ;; As you can see there actually is only one real data;
    ;; the 1st push reads the current calues of ti::text-:stack
    ;; which is nil; because it was the last element that was poped
    (when (>= ti::text-:stack-count
              (1+ ti::text-:stack-size))
      (ti::text-stack-clear)
      (setq ti::text-:stack-count 0))
    (ti::push-definition 'ti::text-:stack)
    (setq ti::text-:stack
          (list
           func
           re
           beg
           level
           mode))
    (incf  ti::text-:stack-count)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-undo ()
  "Undo last highlighting.
`ti::text-:stack-push-flag' is set to 'undo-func while this function runs."
  (interactive)
  (let* ((ti::text-:stack-push-flag  'undo-func)
         (prev   ti::text-:stack)
         func
         beg
         re
         level
         mode)
    ;; - Hm The stack values are empty now, try to pop last saved values
    ;;   from stack.
    ;; - Actually there should be something in the variable is the stack is
    ;;   not empty and it is an erro condition is variable IS empty AND
    ;;   there is values in the stack! ... We'll we don't nag about that
    ;;   here. I just thought you should know about it.
    (unless  prev
      (decf ti::text-:stack-count)
      (ti::pop-definition 'ti::text-:stack)
      (setq prev ti::text-:stack)) ;Maybe this time there is something
    (if (not (and (ti::listp prev)
                  (nth 0 prev)))
        (progn
          (ti::text-stack-clear)
          (error "tinylibt: No more undo information in stack."))
      (setq func    (nth 0 prev)
            re      (nth 1 prev)
            beg     (nth 2 prev)
            level   (nth 3 prev)
            mode    (nth 4 prev))
      (decf ti::text-:stack-count)
      (save-excursion
        ;;  - What if user has narrowed the buffer
        ;;  - Or he has deleted text
        (or (ignore-errors (goto-char beg))
            (error "\
There is no such search point in the buffer any more? %s" beg))
        (cond
         ((eq func 'looking-at)
          (ti::text-looking-at re level 'default))
         (t
          (ti::text-re-search
           re
           (if (eq func 're-search-backward)
               'back nil)
           level
           nil
           'default
           mode))))
      ;;  UNDO done; now get next undo information
      (ti::pop-definition 'ti::text-:stack))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-clear-buffer-properties (&optional  propl)
  "Remove all properties from buffer that match property list PROPL.

Input:
  See function `ti::text-clear-region-properties'"
  (interactive)
  (ti::text-clear-region-properties (point-min) (point-max) propl)
  (when (interactive-p)
    (redraw-display)
    (message "Properties cleared")))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-clear-region-properties (beg end &optional propl)
  "Remove properties from BEG END. Affects read only buffers too.

Input:

  PROPL  (PROP VAL PROP VAL ..) Property list to match.
         If nil, then remove all properties."
  (interactive "r")
  (let* (buffer-read-only               ;allow writing
         point
         prop
         val
         list
         read-list
         elt
         rprop                          ;read property name
         rval
         delete)                        ;flag
    (with-buffer-modified
      (if (null propl)
          (set-text-properties beg end nil)
        (ti::keep-lower-order beg end)
        (setq prop (nth 0 propl)  val (nth 1 propl))
        (while (and (> (point-max) beg)
                    ;;  The beg is advanced in loop
                    ;;
                    (<= beg end)
                    (setq point (text-property-any beg end prop val)))
          (setq read-list (text-properties-at point)
                list      propl
                delete    t)
          (while list
            (setq rprop (nth 0 list)
                  rval  (nth 1 list)
                  list  (cdr (cdr list))) ;go 2 forward
            ;;  The memq return the rest of list
            ;;
            ;;      '(owner me face highlight  do-it nil)
            ;;      (memq 'face)  --> '(face highlight  do-it nil)
            ;;
            ;;  So the (nth 1) gives the value 'highlight which we
            ;;  test.
            (if (or (null (setq elt (memq rprop read-list)))
                    (not (eq (nth 1 elt) rval)))
                ;;  This doesn'tmatch, stop and cancel delete
                (setq list nil  delete nil)))
          ;;  Character by character, this is bit slow...
          (when delete
            (set-text-properties point (1+ point) nil))
          ;;  Search again
          (setq beg (1+ point)))))))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-get-mouse-property ()
  "Check if the point has 'mouse-face text property.
notice that if value read from point is nil,
that means same as no `mouse-face' property exists.

Return:

  nil          no property at point found
  prop         `mouse-face' property value"
  (let* ((prop  (text-properties-at (point))))
    (if (setq prop (memq 'mouse-face prop))
        (cdr prop))))                   ;return value, may be nil

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-match-level (&optional level face-or-propl beg end)
  "Add to match LEVEL a FACE-OR-PROPL in region BEG END.
If no match in that level, do nothing. Property `rear-nonsticky' is
added to the end of match unless FACE-OR-PROPL contains it.

Input:

  LEVEL             Defaults to 0
  FACE-OR-PROPL     Defaults to '(face highlight)
                    If symbol, must be face symbol.
                    Can also be property list '(PROP VAL PROP VAL ..))

  BEG END           If given, then these are the are matched."
  (let ((add-flag   t))
    (setq beg (or beg (match-beginning (or level 0)))
          end (or end (match-end       (or level 0))))
    (when (and (and beg end)
               (not (eq beg end)))      ;Nothing to do
      (cond
       ((null face-or-propl)
        (add-text-properties beg end '(face highlight)))
       ((symbolp face-or-propl)
        (add-text-properties beg end (list 'face face-or-propl)))
       (t
        (setq add-flag (null (memq 'rear-nonsticky face-or-propl)))
        (add-text-properties beg end face-or-propl)))
      (when add-flag
        (if (eq end 1) (setq end 2))    ;(1- 1) = 0, invalid charpos
        (add-text-properties (1- end) end '(rear-nonsticky t))))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-re-search
  (re &optional direction level maxp face mode save-undo)
  "Highlight found text with search face.

Note:

    The beginning of match and end of match will have
    property 'rear-nonsticky t, so that adding characters before of after
    text, won't inherit the face.

Input:

  RE            str  regexp
  DIRECTION     bool non-nil means backward
  LEVEL         nbr  which subexpression in re to highlight, default is 0
  MAXP          nbr  last search point [default until bob/eob]

  FACE          sym  face symbol
                     if symbol is 'null then set face to value nil
                     or if this is list; then it must be properly list
                     of format '(PROP PROP-VAL  PROP PROP-VAL ..)

  MODE          nbr  signify that function should highlight all matches
                     that occur within LEVEL..NBR
                     if you have lot's of xx(match)yy|zz(match)tt|
                     the subexpression are counted from left to to
                     right: 1,2 ...
  SAVE-UNDO     flag non-nil means that the highlight information is
                     recorded for undo. This flag in on if function is
                     called interactively. NOTE: the undo information is
                     saved only if something was matched.

Return:

 nil            No match
 nbr            start of match at LEVEL."
  (let* ((func          (if direction
                            're-search-backward
                          're-search-forward))
         (start-point   (point))
         buffer-read-only
         max-level
         count
         bp ep                          ;beg/end points
         ret
         prop
         val
         list)
    (with-buffer-modified
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. set defaults . .
      (or level
          (setq level 0))
      (or maxp
          (setq maxp
                (if direction
                    (point-min)
                  (point-max))))
      (cond
       ((equal 'null face)
        (setq face nil))
       ((null face)
        (setq face (ti::text-face 'search))))
      ;;  Otherwise face is non-nil
      (setq max-level (1+ (or mode level)))
      ;; Make sure the property list has paired values if supplied
      (if (and (ti::listp face)
               (not (eq 0 (% (length face) 2))))
          (error "face properties are not paired: prop val"))
      ;; .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ..  do matching . .
      (while (funcall func re maxp t)
        (setq count level)
        (while (< count max-level)
          (setq  bp (match-beginning count)
                 ep (match-end count))
          (if (and bp (null ret))       ;do only once
              (setq ret bp))
          (when (and bp (not (eq bp ep))) ;empty string
            (cond
             ((or (symbolp face)
                  (null face))
              (put-text-property bp ep 'face face))
             ((listp face)
              (setq list face)
              (while list
                ;;  Read two values at time
                ;;
                (setq prop (car list) list (cdr list)
                      val  (car list) list (cdr list))
                (put-text-property bp ep prop val))))
            ;;  #todo: something is wrong here, investigate..
            ;;
            ;;  If a character's `rear-nonsticky'
            ;;  property is `t', then none of its properties are rear-sticky.
            ;;
            ;;  Hmm, doesn't affect 19.28; is there bug in this emacs?
            ;;  The highlight is still extended If one adds chars after
            ;;  the matched text.
            ;;
            ;;  The stickiness must be activated ONE before the character.
            (let (beg)
              (if (eq bp (1- ep))
                  (setq beg (1- bp))
                (setq beg (1- ep)))
              (if (zerop beg)
                  (setq beg 1))
              (add-text-properties beg ep '(rear-nonsticky t))))
          (setq count (1+ count))))
      ;;   Saving the search values for possible undo.
      (if (and save-undo ret)
          (ti::text-save-data re level func mode start-point))
      ;; Return success status
      ret)))

;;; ----------------------------------------------------------------------
;;;
(defun ti::text-property-search-and-modify
  (match-plist set-plist &optional beg end)
  "Search all characters forward, whose text properties match MATCH-PLIST.
Set properties to SET-PLIST. The point moves along the search.

Input:

 MATCH-PLIST    property list '(prop val prop val ..)
 SET-PLIST      property list '(prop val prop val ..)
 BEG            start point of search; defaults to `point-min'
 END            end point of search; defaults to `point-max'"
  (let* ((sprop (car match-plist))      ;serach property
         (sval  (car (cdr match-plist)))
         point
         plist mlist
         elt
         ok)
    (if (null match-plist)
        (error "Invalid match-plist"))
    (or beg (setq beg (point-min)))
    (or end (setq end (point-max)))
    (ti::keep-lower-order beg end)
    (goto-char beg)
    (setq point (1- (point)))
    (while (and (not (eobp))
                (<= (point) end)
                (setq point (text-property-any (1+ point) end sprop sval)))
      (goto-char point)
      (setq plist (text-properties-at (point))
            mlist match-plist
            ok t)
      (while (and ok mlist)
        ;; Select 1str PROP fro match-list and see if it is in read PLIST
        ;; Continue until all MLIST properties are found from read PLIST
        (setq elt (memq (car mlist) plist)
              ;;   first non-match terminates loop immediately
              ok    (and elt (eq (nth 1 elt) (nth 1 mlist)))
              mlist (cdr (cdr mlist))))
      (if ok
          (set-text-properties (point) (1+ (point)) set-plist)))))

;;}}}
;;{{{ code: interactive

;;; ----------------------------------------------------------------------
;;; Mon, 12 Feb 1996,  Tom Fontaine <fontaine@esd.ray.com>
;;; Sent this piece of code.  Thanks Tom!
;;;
(defun ti::text-read-regexp ()
  "Read regexp using `regexp-history'."
  (let*
      ((default (car regexp-history))
       (input
        (read-from-minibuffer
         (if default
             (format "Highlight matching regexp (default `%s'): " default)
           "Highlight matching regexp: ")
         nil nil nil
         'regexp-history)))
    (if (> (length input) 0)            ;the return value
        input
      (setcar regexp-history default))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-looking-at (re &optional level face-or-plist )
  "Highlight found RE at LEVEL with FACE-OR-PLIST.
The LEVEL is subexpression to highlight. PLIST means property list."
  (interactive "slook at: ")
  (let (buffer-read-only)               ;allow writing
    (with-buffer-modified
      (setq level               (or level 0)
            face-or-plist (or face-or-plist (ti::text-face 'search)))

      (when (and (looking-at re)
                 (match-end level))
        (ti::text-save-data re level 'looking-at nil (point))
        (ti::text-match-level level face-or-plist)))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-buffer (re &optional level face mode save-undo)
  "Highlight RE and sub LEVEL in whole buffer, starting from `point-min'.
Preserve point.

See `ti::text-re-search' for descriptions of FACE MODE and SAVE-UNDO."
  (interactive (list (ti::text-read-regexp)  current-prefix-arg))
  (save-excursion
    (if (interactive-p)
        (setq save-undo t))
    (goto-char (point-min))
    (ti::text-re-search re nil level nil face mode save-undo)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-re-search-forward (re &optional level face mode save-undo)
  "Search RE and highlight forward until `point-max'.
Optional prefix arg tells which subexpression LEVEL to match that
function should highlight. point is preserved during call.

See `ti::text-re-search' for descriptions of FACE MODE SAVE-UNDO."
  (interactive (list (ti::text-read-regexp)  current-prefix-arg))
  (save-excursion
    (if (interactive-p)
        (setq save-undo t))
    (ti::text-re-search re nil level nil face mode save-undo)))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-re-search-backward (re &optional level face mode save-undo)
  "Search RE and highlight backward until `point-min'.
Optional prefix arg tells which subexpression LEVEL to match that
function should highlight. point is preserved during call.

See `ti::text-re-search' for descriptions of FACE MODE SAVE-UNDO."
  (interactive (list (ti::text-read-regexp)  current-prefix-arg))
  (save-excursion
    (if (interactive-p)
        (setq save-undo t))
    (ti::text-re-search re 'back level nil face mode save-undo)))

;;; ----------------------------------------------------------------------
;;; - These are handy when you want to "mark" ceratin texts for quick ref.
;;;
;;;###autoload
(defun ti::text-mouse-mark-region (beg end event)
  "Highlight region BEG END. EVENT is mouse event."
  (interactive "r\ne")
  (ti::text-mark-region beg end))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-mouse-unmark-region (beg end event)
  "Remove highlight from region BEG END. EVENT is mouse event."
  (interactive "r\ne")
  (ti::text-mark-region beg end 'remove))

;;; - This is for keyboard users
;;;
;;;###autoload
(defun ti::text-unmark-region (beg end)
  "Remove highlight from region BEG END."
  (interactive "r")
  (ti::text-mark-region beg end 'remove))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ti::text-mark-region (beg end &optional remove face)
  "Highlight BEG END. With optional prefix arg REMOVE all matching FACE."
  (interactive "r\nP")
  (let* (buffer-read-only               ;set this to nil
         (face      (if remove
                        'default
                      (or face (ti::text-face 'search)))))
    (with-buffer-modified
      (put-text-property beg end 'face face))))

;;}}}

(provide     'tinylibt)
(run-hooks   'ti::text-:load-hook)

;;; tinylibt.el ends here
