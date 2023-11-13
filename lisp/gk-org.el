;;; gk-org.el --- misc org stuff I use  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Just some miscallenous org stuff I've written or found
;;; Code:

(require 'org)

;;; found at https://emacs.stackexchange.com/questions/41438/rename-tags-in-org-agenda-files
(defun gk-org-switch-tags (old new)
  "Switch tag OLD for tag NEW."
  (when (member old (org-get-tags))
    (org-toggle-tag new 'on)
    (org-toggle-tag old 'off)))

(defun gk-org-rename-tag (old new)
  "Rename a tag OLD to NEW in the current org buffer."
  (interactive "scurrent name: \nsnew name: ")
  (org-map-entries
   (lambda () (gk-org-switch-tags old new))
   (format "+%s" old)
   nil))

(provide 'gk-org)
;;; gk-org.el ends here
