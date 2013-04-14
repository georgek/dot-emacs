;;; here are my settings for a diminished modeline
(defmacro gk-dim-maj (mode diminished-name)
  (let ((hook-name (concat (symbol-name mode) "-hook"))
        (dim-name (concat diminished-name " ")))
    `(add-hook ',(intern-soft hook-name)
               (lambda ()
                 (setq mode-name ,dim-name)))))

(defmacro gk-dim-min (name mode diminished-name)
  `(eval-after-load ,name
     '(diminish ',mode ,diminished-name)))

;;; emacs-lisp-mode
(gk-dim-maj emacs-lisp-mode "Elisp")

(gk-dim-min "elisp-slime-nav" elisp-slime-nav-mode "S")
(gk-dim-min "yasnippet" yas-minor-mode "Y")
(gk-dim-min "eldoc" eldoc-mode "E")
(gk-dim-min "paredit" paredit-mode "P")
(gk-dim-min "macrostep" macrostep-mode "M")
