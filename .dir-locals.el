(
 (auto-mode-alist
  ("\\.\\(borgconfig\\|gitremotes\\)\\'" . gitconfig-mode))
 ("lib/undo-tree"
  (emacs-lisp-mode
   (outline-regexp . ";;;\\(;* [^=\t\n]\\|###autoload\\)\\|(")))
 (git-commit-mode
  (git-commit-major-mode . git-commit-elisp-text-mode))
 )
