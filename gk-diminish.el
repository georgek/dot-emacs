;;; here are my settings for a diminished modeline
(defmacro gk-dim-maj (mode diminished-name)
  (let ((hook-name (concat (symbol-name mode) "-hook")))
    `(add-hook ',(intern-soft hook-name)
               (lambda ()
                 (setq mode-name ,diminished-name)))))

(defmacro gk-dim-min (name mode diminished-name)
  `(eval-after-load ,name
     '(diminish ',mode ,diminished-name)))

(setf (caadr (nth 4 mode-line-modes)) " ")

(gk-dim-maj emacs-lisp-mode "Elisp")
(gk-dim-maj org-mode "Org")
(gk-dim-maj ielm-mode "IELM")

;;; all
(gk-dim-min "yasnippet" yas-minor-mode "Y")
(gk-dim-min "flyspell" flyspell-mode "F")
(gk-dim-min "abbrev" abbrev-mode "A")

;;; lisp
(gk-dim-min "paredit" paredit-mode "P")
(gk-dim-min "eldoc" eldoc-mode "E")

;;; elisp
(gk-dim-min "elisp-slime-nav" elisp-slime-nav-mode "S")
(gk-dim-min "macrostep" macrostep-mode "M")

;;; common lisp
(gk-dim-min "slime" slime-mode "S")
