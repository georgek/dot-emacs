;;; gk-extra.el - misc stuff I use

;;; Code:

;;;###autoload
(defun gk-select-current-line ()
  "Sets the region to the current line"
  (interactive)
  (beginning-of-line)
  (push-mark nil 1 1)
  (end-of-line))

;;;###autoload
(defun gk-comment-current-line ()
  "Toggles comment on the current line"
  (interactive)
  (save-excursion
    (gk-select-current-line)
    (comment-dwim nil)))

;;;###autoload
(defun gk-c-c++-header ()
  "Decides whether a .h file is C or C++ based on existence of a
corresponding .c file. If no such file is found the mode is set
to C++."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

;;;###autoload
(defun gk-c-c++-toggle ()
  "Toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

(provide 'gk-extra)
