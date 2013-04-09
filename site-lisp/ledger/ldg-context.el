;;; ldg-context.el --- Helper code for use with the "ledger" command-line tool

;; Copyright (C) 2003-2013 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.


;;; Commentary:
;;  Provide facilities for reflection in ledger buffers

;;; Code:

(eval-when-compile
  (require 'cl))


(defconst ledger-line-config
  '((xact
     (("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*?\\)[ \t]*;\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee comment))
      ("^\\(\\([0-9][0-9][0-9][0-9]/\\)?[01]?[0-9]/[0123]?[0-9]\\)[ \t]+\\(\\([!*]\\)[ \t]\\)?[ \t]*\\((\\(.*\\))\\)?[ \t]*\\(.*\\)[ \t]*$"
       (date nil status nil nil code payee))))
    (acct-transaction
     (("^\\([ \t]+;\\|;\\)\\s-?\\(.*\\)"
       (indent comment))
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)\\s-\\s-[ \t]+\\([$€£]\\s-?\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)[ \t]*;[ \t]*\\(.*\\)[ \t]*$"
       (indent status account commodity amount nil comment)) ;checked 2013-04-06 
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)\\s-\\s-[ \t]+\\([$€£]\\s-?\\)\\(-?[0-9]*\\(\\.[0-9]*\\)?\\)$"
       (indent status account commodity amount))  ;checked 2013-04-06 
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)\\s-\\s-[ \t]+\\(-?[0-9]+\\.[0-9]*\\)[ \t]+\\(.*?\\)[ \t]*\\(;[ \t]*\\(.*?\\)[ \t]*$\\|@+\\)"
       (indent status account amount nil commodity comment)) ;checked 2013-04-06 
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)\\s-\\s-[ \t]+\\(-?[0-9]+\\.[0-9]*\\)[ \t]+\\(.*\\)"
       (indent status account amount nil commodity))  ;checked 2013-04-06 
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)[ \t]*;[ \t]*\\(.*?\\)[ \t]*$"
       (indent status account comment))
      ("\\(^[ \t]+\\)\\([*! ]?\\)\\(.*?\\)[ \t]*$"
       (indent status account))))))

(defun ledger-extract-context-info (line-type pos)
  "Get context info for current line with LINE-TYPE.

Assumes point is at beginning of line, and the POS argument specifies
where the \"users\" point was."
  (let ((linfo (assoc line-type ledger-line-config))
        found field fields)
    (dolist (re-info (nth 1 linfo))
      (let ((re (nth 0 re-info))
            (names (nth 1 re-info)))
        (unless found
          (when (looking-at re)
            (setq found t)
            (dotimes (i (length names))
              (when (nth i names)
                (setq fields (append fields
                                     (list
                                      (list (nth i names)
                                            (match-string-no-properties (1+ i))
                                            (match-beginning (1+ i))))))))
            (dolist (f fields)
              (and (nth 1 f)
                   (>= pos (nth 2 f))
                   (setq field (nth 0 f))))))))
    (list line-type field fields)))

(defun ledger-thing-at-point ()
  "Describe thing at points.  Return 'transaction, 'posting, or nil.
Leave point at the beginning of the thing under point"
  (let ((here (point)))
    (goto-char (line-beginning-position))
    (cond ((looking-at "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.+?)\\)?\\s-+")
           (goto-char (match-end 0))
           'transaction)
          ((looking-at "^\\s-+\\([*!]\\s-+\\)?[[(]?\\(.\\)")
           (goto-char (match-beginning 2))
           'posting)
          ((looking-at "^\\(sun\\|mon\\|tue\\|wed\\|thu\\|fri\\|sat\\)\\s-+")
           (goto-char (match-end 0))
           'day)
          (t
           (ignore (goto-char here))))))

(defun ledger-context-at-point ()
  "Return a list describing the context around point.

The contents of the list are the line type, the name of the field
containing point, and for selected line types, the content of
the fields in the line in a association list."
  (let ((pos (point)))
    (save-excursion
      (beginning-of-line)
      (let ((first-char (char-after)))
        (cond ((equal (point) (line-end-position))
               '(empty-line nil nil))
              ((memq first-char '(?\ ?\t))
               (ledger-extract-context-info 'acct-transaction pos))
              ((memq first-char '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
               (ledger-extract-context-info 'xact pos))
              ((equal first-char ?\=)
               '(automated-xact nil nil))
              ((equal first-char ?\~)
               '(period-xact nil nil))
              ((equal first-char ?\!)
               '(command-directive))
              ((equal first-char ?\;)
               '(comment nil nil))
              ((equal first-char ?Y)
               '(default-year nil nil))
              ((equal first-char ?P)
               '(commodity-price nil nil))
              ((equal first-char ?N)
               '(price-ignored-commodity nil nil))
              ((equal first-char ?D)
               '(default-commodity nil nil))
              ((equal first-char ?C)
               '(commodity-conversion nil nil))
              ((equal first-char ?i)
               '(timeclock-i nil nil))
              ((equal first-char ?o)
               '(timeclock-o nil nil))
              ((equal first-char ?b)
               '(timeclock-b nil nil))
              ((equal first-char ?h)
               '(timeclock-h  nil nil))
              (t
               '(unknown nil nil)))))))

(defun ledger-context-other-line (offset)
  "Return a list describing context of line OFFSET from existing position.

Offset can be positive or negative.  If run out of buffer before reaching
specified line, returns nil."
  (save-excursion
    (let ((left (forward-line offset)))
      (if (not (equal left 0))
          nil
	  (ledger-context-at-point)))))

(defun ledger-context-line-type (context-info)
  (nth 0 context-info))

(defun ledger-context-current-field (context-info)
  (nth 1 context-info))

(defun ledger-context-field-info (context-info field-name)
  (assoc field-name (nth 2 context-info)))

(defun ledger-context-field-present-p (context-info field-name)
  (not (null (ledger-context-field-info context-info field-name))))

(defun ledger-context-field-value (context-info field-name)
  (nth 1 (ledger-context-field-info context-info field-name)))

(defun ledger-context-field-position (context-info field-name)
  (nth 2 (ledger-context-field-info context-info field-name)))

(defun ledger-context-field-end-position (context-info field-name)
  (+ (ledger-context-field-position context-info field-name)
     (length (ledger-context-field-value context-info field-name))))

(defun ledger-context-goto-field-start (context-info field-name)
  (goto-char (ledger-context-field-position context-info field-name)))

(defun ledger-context-goto-field-end (context-info field-name)
  (goto-char (ledger-context-field-end-position context-info field-name)))

(provide 'ldg-context)

;;; ldg-report.el ends here
