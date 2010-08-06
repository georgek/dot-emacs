;;; magit-svn.el --- git-svn plug-in for Magit

;; Copyright (C) 2008, 2009  Marius Vollmer
;; Copyright (C) 2008  Linh Dang
;; Copyright (C) 2008  Alex Ott
;; Copyright (C) 2008  Marcin Bachry
;; Copyright (C) 2009  Alexey Voinov
;; Copyright (C) 2009  John Wiegley
;; Copyright (C) 2010  Yann Hodique
;;
;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides git-svn functionality as a separate component of Magit

;;; Code:

(require 'magit)

;; git svn commands

(defun magit-svn-find-rev (rev &optional branch)
  (interactive
   (list (read-string "SVN revision: ")
         (if current-prefix-arg
             (read-string "In branch: "))))
  (let* ((sha (apply 'magit-git-string
                     `("svn"
                       "find-rev"
                       ,(concat "r" rev)
                       ,@(when branch (list branch))))))
    (if sha
        (magit-show-commit
         (magit-with-section sha 'commit
           (magit-set-section-info sha)
           sha))
      (error "Revision %s could not be mapped to a commit" rev))))

(defun magit-svn-rebase ()
  (interactive)
  (magit-run-git-async "svn" "rebase"))

(defun magit-svn-dcommit ()
  (interactive)
  (magit-run-git-async "svn" "dcommit"))

(defun magit-svn-enabled ()
  (not (null (magit-svn-get-ref-info))))

(defun magit-svn-get-local-ref (url)
  (let ((branches (cons (magit-get "svn-remote" "svn" "fetch")
                        (magit-get-all "svn-remote" "svn" "branches")))
        (base-url (magit-get "svn-remote" "svn" "url"))
        (result nil))
    (while branches
      (let* ((pats (split-string (pop branches) ":"))
             (src (replace-regexp-in-string "\\*" "\\\\(.*\\\\)" (car pats)))
             (dst (replace-regexp-in-string "\\*" "\\\\1" (cadr pats)))
             (base-url (replace-regexp-in-string "\\+" "\\\\+" base-url))
             (pat1 (concat "^" src "$"))
             (pat2 (cond ((equal src "") (concat "^" base-url "$"))
                         (t (concat "^" base-url "/" src "$")))))
        (cond ((string-match pat1 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil))
              ((string-match pat2 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil)))))
    result))

(defvar magit-svn-get-ref-info-cache nil
  "A cache for svn-ref-info.
As `magit-get-svn-ref-info' might be considered a quite
expensive operation a cache is taken so that `magit-status'
doesn't repeatedly call it.")

(defun magit-svn-get-ref-info (&optional use-cache)
  "Gather details about the current git-svn repository.
Return nil if there isn't one.  Keys of the alist are ref-path,
trunk-ref-name and local-ref-name.
If USE-CACHE is non-nil then return the value of `magit-get-svn-ref-info-cache'."
  (if use-cache
      magit-svn-get-ref-info-cache
    (let* ((fetch (magit-get "svn-remote" "svn" "fetch"))
           (url)
           (revision))
      (when fetch
        (let* ((ref (cadr (split-string fetch ":")))
               (ref-path (file-name-directory ref))
               (trunk-ref-name (file-name-nondirectory ref)))
          (setq magit-svn-get-ref-info-cache
                (list
                 (cons 'ref-path ref-path)
                 (cons 'trunk-ref-name trunk-ref-name)
                 ;; get the local ref from the log. This is actually
                 ;; the way that git-svn does it.
                 (cons 'local-ref
                       (with-temp-buffer
                         (insert (or (magit-git-string "log" "--first-parent")
                                     ""))
                         (goto-char (point-min))
                         (cond ((re-search-forward "git-svn-id: \\(.+/.+?\\)@\\([0-9]+\\)" nil t)
                                (setq url (match-string 1)
                                      revision (match-string 2))
                                (magit-svn-get-local-ref url))
                               (t
                                (setq url (magit-get "svn-remote" "svn" "url"))
                                nil))))
                 (cons 'revision revision)
                 (cons 'url url))))))))

(defun magit-svn-get-ref (&optional use-cache)
  "Get the best guess remote ref for the current git-svn based branch.
If USE-CACHE is non nil, use the cached information."
  (let ((info (magit-svn-get-ref-info use-cache)))
    (cdr (assoc 'local-ref info))))

(magit-define-inserter svn-unpulled (&optional use-cache)
  (when (magit-svn-get-ref-info)
    (magit-git-section 'svn-unpulled
                       "Unpulled commits (SVN):" 'magit-wash-log
                       "log" "--pretty=format:* %H %s"
                       (format "HEAD..%s" (magit-svn-get-ref use-cache)))))

(magit-define-inserter svn-unpushed (&optional use-cache)
  (when (magit-svn-get-ref-info)
    (magit-git-section 'svn-unpushed
                       "Unpushed commits (SVN):" 'magit-wash-log
                       "log" "--pretty=format:* %H %s"
                       (format "%s..HEAD" (magit-svn-get-ref use-cache)))))

(magit-define-section-jumper svn-unpushed  "Unpushed commits (SVN)")

(defun magit-svn-remote-string ()
  (let ((svn-info (magit-svn-get-ref-info)))
    (when svn-info
      (concat (cdr (assoc 'url svn-info))
              " @ "
              (cdr (assoc 'revision svn-info))))))

(defun magit-svn-remote-update ()
  (interactive)
  (when (magit-svn-enabled)
    (magit-run-git-async "svn" "fetch")))

(defvar magit-svn-extension-keys
  `((,(kbd "N r") . magit-svn-rebase)
    (,(kbd "N c") . magit-svn-dcommit)
    (,(kbd "N f") . magit-svn-remote-update)
    (,(kbd "N s") . magit-svn-find-rev)))

(easy-menu-define magit-svn-extension-menu
  nil
  "Git SVN extension menu"
  '("Git SVN"
    ["Rebase" magit-svn-rebase (magit-svn-enabled)]
    ["Fetch" magit-svn-remote-update (magit-svn-enabled)]
    ["Commit" magit-svn-dcommit (magit-svn-enabled)]))

(defvar magit-svn-extension-inserters
  '((:after unpulled-commits (lambda () (magit-insert-svn-unpulled t)))
    (:after unpushed-commits (lambda () (magit-insert-svn-unpushed t)))))

(defvar magit-svn-extension
  (make-magit-extension :keys magit-svn-extension-keys
                        :menu magit-svn-extension-menu
                        :insert magit-svn-extension-inserters
                        :remote-string 'magit-svn-remote-string))

(magit-install-extension magit-svn-extension)

(provide 'magit-svn)
;;; magit-svn.el ends here
