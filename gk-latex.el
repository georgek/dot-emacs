;;; utilities for LaTeX

(defun gk-pgfplots-add-table (filename &optional legend)
  (interactive
   (list (read-file-name "File: ")
         (read-string "Legend: ")))
  (let* ((table-string (file-to-string filename))
         (table-header (car (split-string table-string "\n" t)))
         (x (completing-read "x: " (split-string table-header " " t)))
         (y (completing-read "y: " (split-string table-header " " t))))
    (insert (format "\\addplot table[x=%s,y=%s] {\n" x y)
            table-string
            "};\n")
    (unless (string= legend "")
      (insert (format "\\addlegendentry{%s}\n" legend)))))

