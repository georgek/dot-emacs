;;; gk-other-window-repeat.el

;;; Code:

;;;###autoload
(defun gk-other-window-repeat (COUNT &optional no-repeat)
  "Calls other-window.  If a multiple key sequence was used to
  call this then the last key can be used on its own to repeat
  this, like kmacro-call-macro."
  (interactive "p")
  (let ((repeat-key (and (null no-repeat)
                         (> (length (this-single-command-keys)) 1)
                         last-input-event))
        repeat-key-str
	prev-window
        (nxt t))
    ;; save current window
    (setq prev-window (selected-window))
    (other-window COUNT)
    (when repeat-key
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil)))
    (while repeat-key
      (unless (current-message)
        (message "(Type %s to keep cycling)"
                 repeat-key-str))
      (if (equal repeat-key (read-event))
          (progn
            (clear-this-command-keys t)
            (other-window COUNT)
            ;; if we cycle all the way to the same window, set prev-window to
            ;; next window, then if we continue to cycle set it back again
            (when (eq prev-window (selected-window))
              (setq prev-window (if nxt
                                    (next-window)
                                  (previous-window)))
              (setq nxt (not nxt)))
            (setq last-input-event nil))
        (setq repeat-key nil)))
    (when last-input-event
      (clear-this-command-keys t)
      (setq unread-command-events (list last-input-event)))))

(provide 'gk-other-window-repeat)
