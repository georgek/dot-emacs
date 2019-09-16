;;; gk-extra.el - misc stuff I use

;;; Code:

;;;###autoload
(defun gk-select-current-line ()
  "Set the region to the current line."
  (interactive)
  (beginning-of-line)
  (push-mark nil 1 1)
  (end-of-line))

;;;###autoload
(defun gk-comment-current-line ()
  "Toggle comment on the current line."
  (interactive)
  (save-excursion
    (gk-select-current-line)
    (comment-dwim nil)))

;;;###autoload
(defun gk-c-c++-header ()
  "Decide whether a .h file is C or C++ header.

The decision is based on existence of a corresponding .c file.
If no such file is found the mode is set to C++."
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

;;;###autoload
(defun gk-c-c++-toggle ()
  "Toggle between `c-mode' and `c++-mode'."
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

;;;###autoload
(defun gk-kill-client-or-daemon ()
  "Kill Emacs client or daemon.

If we are running as daemon, the daemon is killed when the last
frame is killed."
  (interactive)
  (if (and (boundp 'server-clients)
           (> (length server-clients) 0))
      ;; daemon
      (if (<= (length (frame-list)) 2)
          (when (y-or-n-p "Last frame; kill daemon? ")
            (save-some-buffers)
            (delete-frame)
            (kill-emacs))
        ;; not the last frame so just delete it
        (delete-frame))
      ;; not daemon
    (save-buffers-kill-emacs)))

;;; stuff from https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive (list
                (read-file-name
                 "New filename: "
                 nil (buffer-file-name) nil)))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

;;;###autoload
(defun move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive (list
                (read-directory-name
                 "New directory: "
                 (file-name-directory (buffer-file-name)))))
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

(provide 'gk-extra)
;;; gk-extra.el ends here
