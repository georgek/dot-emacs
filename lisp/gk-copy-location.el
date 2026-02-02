;;; gk-copy-location.el - copy locations in source code

;;; Code:

(defun gk-treesit-get-context-path ()
  "Get a hierarchical path of treesitter nodes at point.
Returns something like 'Class.method' or 'namespace.Class.method'."
  (when (treesit-parser-list)
    (let* ((node (treesit-node-at (point)))
           (path-parts '()))
      (while node
        (let ((type (treesit-node-type node)))
          ;; Adjust these node types based on your language
          ;; Common ones for many languages:
          (when (member type '("class_definition" "class_declaration"
                               "method_definition" "function_definition"
                               "function_declaration" "method_declaration"
                               "namespace_definition" "module_definition"
                               "interface_declaration" "struct_declaration"))
            (when-let ((name-node (or (treesit-node-child-by-field-name node "name")
                                      (treesit-search-subtree node "identifier" nil nil 1))))
              (push (treesit-node-text name-node t) path-parts))))
        (setq node (treesit-node-parent node)))
      (when path-parts
        (string-join path-parts ".")))))


(defun gk-copy-location-with-treesit-context ()
  "Copy current location as '/path/to/file:Context.path'.
If treesitter is available, includes class/method context.
Otherwise, just copies the file path with line number."
  (interactive)
  (let* ((file-path (or (buffer-file-name) default-directory))
         (context (gk-treesit-get-context-path))
         (location (if context
                       (format "%s:%s" file-path context)
                     (format "%s:%d" file-path (line-number-at-pos)))))
    (kill-new location)
    (message "Copied: %s" location)
    location
    ))

(provide 'gk-copy-location)
;;; gk-copy-location.el ends here
