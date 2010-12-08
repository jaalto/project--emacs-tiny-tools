;;; tinylibw.el --- Library for (w)indows Cygwin OS specific functions

;; This file is not part of Emacs

;; Copyright (C)    2010-2011 Jari Aalto
;; Keywords:        extensions
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
;;
;;; Install:
;;
;; DO NOT LOAD THIS FILE, but load the central library "m". It will handle arranging
;; everything for you.
;;
;;      (require 'tinylibm)
;;
;;; Commentary:
;;
;;  Preface, 1998 (rearranged 2010)
;;
;;      This is lisp function library, package itself does nothing.
;;      This library defines some Emacs backward compatibility function.
;;      In 2010 these function were moved from tinylibb.el here.

;;; Change Log:

;;; Code:

(require 'tinyliba)
(provide 'tinylibw)

(defconst tinylibw-version-time "2010.1208.0757"
  "Latest version number as last modified time.")

;;; These functions has been submitted to Emacs 21.2
;;; (w32-fns.el)

(defvar w32-cygwin-mount-table nil
  "Cygwin mount.exe mapping. See `w32-cygwin-mount-table'.")

;;; ----------------------------------------------------------------------
;;;
(put 'w32-cygwin-mount-table-dolist 'lisp-indent-function 0)
(put 'w32-cygwin-mount-table-dolist 'edebug-form-spec '(body)) ;;#todo: not working
(defmacro w32-cygwin-mount-table-dolist (&rest body)
  "Run DOLIST for Cygwin mount table.
`mount' is complete mount element (cygwin . dos).
Variables `cygwin' and `dos' are bound respectively."
  `(dolist (mount w32-cygwin-mount-table)
     ;;  mount => ("/tmp" . "c:\\temp")
     (let* ((cygwin (car mount))
            (dos    (cdr mount)))
       ,@body)))

;;; ----------------------------------------------------------------------
;;;
(put 'w32-cygwin-shell-environment 'lisp-indent-function 0)
(put 'w32-cygwin-shell-environment 'edebug-form-spec '(body))
(defmacro w32-cygwin-shell-environment  (&rest body)
  "Run BODY under Cygwin shell environment.
For example, you you want to call program ´zgrep' which is not an
.exe, but a shell program, you have to switch to the Cygwin context.

   (when (and (ti::win32-p)
              (ti::win32-cygwin-p))
      (w32-cygwin-shell-environment
           ...))

Variable `shell-file-name' is locally bound during call."
  `(let ((shell-file-name (format "%s/bin/hash.exe"
                                  (ti::win32-cygwin-p 'use-cache))))
     ,@body))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-parse ()
  "Parse cygwin mount table from current point forward."
  ;;  Search lines with backslash
  ;;  f:\\u\\bin /usr/bin user binmode
  ;;
  ;;  Cygwin 1.3.3 changed format, it is now
  ;;
  ;;  f:\\u\\bin on /usr/bin type user (binmode)
  ;;             ==
  ;;
  ;;  \\network\path\this
  (let (list
        (regexp
         (save-excursion
           (if (re-search-forward "^\\([a-z]:\\|[\\][\\]\\).* on " nil t)
               (concat
                "^\\([a-zA-Z]:[\\][^ \t\r\n]*"
                "\\|[a-zA-Z]:"
                "\\|[\\][\\][^ \t\r\n]+"
                "\\)"
                "[ \t]+on[ \t]+"
                "\\(/[^ \t\r\n]*\\)")
             (concat
              "^\\([a-zA-Z]:[\\][^ \t\r\n]*"
              "\\|[a-zA-Z]:"
              "\\|[\\][\\][^ \t\r\n]+"
              "\\)"
              "[ \t]+"
              "\\(/[^ \t\r\n]*\\)")))))
    (while (re-search-forward regexp nil t)
      (let ((dos    (match-string 2))
            (cygwin (match-string 1)))
        (push (cons dos cygwin)
              list)))
    ;;  sort the entries so that the longest mounts come first and
    ;;  last the shortest. This makes a difference when Cygwin paths are
    ;;  converted back to dos:
    ;;
    ;;    /tmp/other       mapping must be handled before /tmp
    ;;    /tmp
    ;;    ..
    (sort list
          (function
           (lambda (a b)
             (> (length (car a))
                (length (car b))))))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-convert (path &optional flag)
  "Run `cygpath' to find out PATH.
Return:

   The default concersion is CYGWIN => DOS

   If `flag' is set, then the conversion is
   DOS => cygwin."
  (let ((cmd     (executable-find "cygpath"))
	(option  "--windows")
	ret)
    (when cmd
      (when flag
        (setq option "--unix"))
      (with-temp-buffer
        (call-process
         cmd
         nil
         (current-buffer)
         nil
         option
         path)
        (goto-char (point-min))
        (when (looking-at "^.*") ;; Filter newlines
          (setq ret (match-string 0)))))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table ()
  "Return Cygwin mount table '((CYGWIN . DOS) ..) using `mount' command."
  (when ;; (memq system-type '(ms-dos windows-nt))
      (ti::win32-p)
    ;; specifically request the .exe which must be along PATH
    ;; if we used only `mount', that could call user's "mount.bat" or
    ;; something.
    (let ((cmd  (executable-find "mount.exe")))
      (when cmd
        (with-temp-buffer
          (call-process cmd nil (current-buffer))
          (goto-char (point-min))
          ;;  It's a serious error if "mount" does not say where
          ;;  the ROOT "/" is. Should we do something?
          (goto-char (point-min))
          (let ((ret (w32-cygwin-mount-table-parse)))
            (unless ret
              (error "Cygwin mount.exe output parse failed:\n[%s]"
                     (buffer-string)))
            ret))))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-point-to-dos (path)
  "Convert Cygwin mount filenames like  /tmp to DOS paths."
  (let* (last-choice
         try)
    (dolist (cygwin w32-cygwin-mount-table)
      (when (string-match (concat "^"  (car cygwin) "\\(.*\\)")
                          path)
        (setq try
              ;;  expand will ensure that slashes are after glue
              ;;  to the same direction
              (expand-file-name
               (concat (file-name-as-directory (cdr cygwin) )
                       (match-string 1 path))))
        ;;  It is difficult to expand the file name correctly because
        ;;  user can make any mount points. That's what we compare which
        ;;  mount point gives the longest match and return it.
        ;;
        ;;  E.g. the root / will always match, but it is not necessarily
        ;;  the final answer given path /tmp/something where there is
        ;;  separate mount point for longer match /tmp
        ;;
        (if (null last-choice)
            (setq last-choice (cons (car cygwin) try))
          (if (length (> (car cygwin) (car last-choice)))
              (setq last-choice (cons (car cygwin) try))))))
    (if (null last-choice)
        path
      (cdr last-choice))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-set ()
  "Run mount.exe and set internal variable `w32-cygwin-mount-table'.
You should run this function after you have made a change to
Cygwin mount points."
  ;;   (interactive)
  (if (ti::win32-p) ;; (memq system-type '(ms-dos windows-nt))
      (setq w32-cygwin-mount-table
            (w32-cygwin-mount-table))))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-mount-table-path-to-dos (path)
  "Convert PATH to dos using cygwin mount table.
You should not call this function, use `w32-cygwin-path-to-dos'."
  ;;  Convert Cygwin /usr/local to DOS path. LOCATION/usr/local.
  ;;  This relies on the fact that the longest paths are first
  ;;  in the mount table.
  (let (final-path)
    (w32-cygwin-mount-table-dolist
      ;;  mount => ("/tmp" . "c:\\temp")
      ;;  variables `cygwin' and `dos' are part of the macro
      (when (string-match (concat "^" (regexp-quote cygwin)
                                  "\\(.*\\)")
                          path)
        (unless (string= cygwin "/")
          (setq dos (concat dos (match-string 1 path))))
        ;; Convert to forward slashes
        (setq final-path (subst-char-in-string ?\\ ?/ dos))
        (return)))
    (unless final-path
      ;; None matched, so this path is under cygwin root dir.
      (let ((root (ti::win32-cygwin-p)))
        (setq final-path (concat root path))))
    final-path))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-path-to-dos (path)
  "Convert cygwin like //c/temp  or /cygdrive/c/temp path to
DOS notation c:/temp."
  ;; NOTE for cygwin and bash shell prompt
  ;; We can't require a slash after the drive letter, because
  ;; //c   and  /cygdrive/c   are all top level roots.
  ;;
  ;; The bash shell's PS1 setting \w (The current working directory)
  ;; Does not add trailing slash.
  (cond
   ((or (string-match "^//\\([a-z]\\)/?$" path)
        (string-match "^/cygdrive/\\([a-z]\\)/?$" path))
    (concat (match-string 1 path) ":/"))
   ((or (string-match "^//\\([a-z]\\)\\(/.*\\)" path)
        (string-match "^/cygdrive/\\([a-z]\\)\\(/.*\\)" path))
    (concat (match-string 1 path) ":" (match-string 2 path)))
   ((string-match "^(/cygdrive/./\\|//" path)
    ;;  if previous regexps couldn't handle it, this is severe error.
    (error "Invalid path format for cygwin %s" path))
   ((string-match "[\\]" path)
    (error "Invalid backslash path %s" path))
   ((string-match "^/" path)
    (w32-cygwin-mount-table-path-to-dos path))
   (t
    path)))

;;; ----------------------------------------------------------------------
;;;
(defun w32-cygwin-dos-path-to-cygwin (path)
  "Convert dos PATH to cygwin path.
Be sure to call `expand-file-name' before you pass PATH to the function."
  (cond
   ((string-match "\\([a-z]\\):[\\/]\\(.*\\)" path)
    (let ((drive     (format  "/cygdrive/%s/" (match-string 1 path)))
          (rest-path (match-string 2 path)))
      (if (not rest-path)
          drive
        (w32-cygwin-mount-table-dolist
          ;;  mount => ("/tmp" . "c:\\temp")
          ;;  variables `cygwin' and `dos' are part of the macro
          (when (or (string-match (concat "^" dos "\\(.*\\)") path)
                    (string-match (concat "^"
                                          ;; Convert to / slashes
                                          (expand-file-name dos)
                                          "\\(.*\\)") path))
            (when (match-string 1 path)
              (setq path (match-string 1 path))
              (setq cygwin (concat cygwin path)))
            ;; Convert to forward slashes
            (return (subst-char-in-string ?\\ ?/ cygwin)))))))
   (t
    (error "Cannot convert to cygwin. path is not absolute %s" path))))

(defsubst w32-expand-file-name-for-cygwin (path)
  "Expand PATH to Cygwin notation if Cygwin is present."
  (when (and (string-match "^[A-Za-z]:" path)
             (ti::win32-cygwin-p))
    (setq path (w32-cygwin-dos-path-to-cygwin path)))
  path)

(defsubst w32-expand-file-name-for-emacs (path)
  "Expand PATH to DOS Emacs notation if PATH is in Cygwin notation."
  (cond
   ((and (ti::emacs-type-win32-p)
         (string-match "^/cygdrive" path))
    (setq path (w32-cygwin-path-to-dos path)))
   ((and (ti::emacs-type-cygwin-p)
         (string-match "^[a-zA-Z]:" path))
    (setq path (w32-cygwin-dos-path-to-cygwin path))))
  path)

(setq w32-cygwin-mount-table
      (if (memq system-type '(ms-dos windows-nt))
          (w32-cygwin-mount-table)))

;; End of file
