;;; gk-electric.el - electric return stuff

;;; Code:

(defvar gk-electrify-return-match
  "[\]}\)]\\|\"\"\"\\|/\\|<"
  "This regexp must match to do an \"electric\" return.")

(defun gk-electrify-return-if-match (&optional arg)
  "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
  (interactive "P")
  (let ((case-fold-search nil))
    (if (looking-at gk-electrify-return-match)
        (save-excursion (newline-and-indent)))
    (delete-horizontal-space t)
    (newline arg)
    (indent-according-to-mode)))

(provide 'gk-electric)
;;; gk-electric.el ends here
