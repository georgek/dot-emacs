;;;; my .emacs file

(setq inhibit-splash-screen t)

;; set window title, turn toolbars and stuff off
(when window-system
  (setq frame-title-format '("%b" " - GNU Emacs"))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (blink-cursor-mode 1))
(mouse-avoidance-mode 'banish)

;; start server
(server-start)

;; add path
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; turn off tabs
(setq-default indent-tabs-mode nil)

;; set some default styles
(setq c-default-style '((java-mode . "java") (awk-mode
      . "awk") (c-mode . "k&r") (c++-mode . "stroustrup") (other
      . "gnu")))

;; column and line number
(column-number-mode 1)
(line-number-mode 1)

;; set some default modes
;; lex
(add-to-list 'auto-mode-alist '("\\.l\\'" . c-mode))
;; matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
;; XML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;; stuff for SLIME
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/site-lisp/slime/")  ; your SLIME directory
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner))

;; org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)
(setq org-log-done 'time)
(setq org-blank-before-new-entry 
      '((heading . t) (plain-list-item . nil)))
(setq org-todo-keywords (quote ((sequence "TODO" "IN PROGRESS" "DONE"))))
;; stuff for remember
(org-remember-insinuate)
(setq org-directory "~/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
;; make org table mode come on for some modes
(add-hook 'LaTeX-mode-hook 'turn-on-orgtbl)

;; makefile mode make key
(add-hook 'makefile-mode-hook
          (function (lambda ()
                      (define-key makefile-mode-map "\C-c\C-c" 'compile))))

;; auto new line and hungry delete modes
(setq c-auto-newline 1)
(setq c-hungry-delete-key 1)

;; turn on flyspell for text and org-mode
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(setq flyspell-issue-message-flag -1)

;; enable parens matching
(show-paren-mode t)

;; uniquify changes conflicting buffer names from file<2> etc
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; binds hippie-expand to M-/
(global-set-key (kbd "M-/") 'hippie-expand)

;; midnight mode kills unused buffers at midnight
(require 'midnight)
(setq midnight-mode t)

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ; set PDF mode by default
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(setq TeX-electric-sub-and-superscript t)
;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)        ;AUCTeX-RefTeX interface

;; aligns the current block of code
(global-set-key (kbd "C-|") 'align-current)

;; enable Winner Mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; magit for using git
(require 'magit)
(global-set-key (kbd "\C-ci") 'magit-status)

;; key for opening a shell
(global-set-key (kbd "\C-cs") 'shell)

;; ido mode
(ido-mode t)
;; disable annoying buffer list
(global-set-key (kbd "\C-x\C-b") 'ido-switch-buffer)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/snippets/")

;; keep backup files neatly out of the way in .~/
(setq backup-directory-alist '(("." . ".~")))

;; scroll compilation buffer
(setq compilation-scroll-output t)

;; change yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; selects current line
(defun select-current-line ()
  "sets the region to the current line"
  (interactive)
  (beginning-of-line)
  (push-mark nil 1 1)
  (end-of-line))
(global-set-key (kbd "C-;") 'select-current-line)

;; comment or uncomment current line
(defun comment-current-line ()
  "comments out the current line"
  (interactive)
  (select-current-line)
  (comment-dwim nil))
(global-set-key (kbd "C-M-;") 'comment-current-line)

;; function decides whether .h file is C or C++ header, sets C++ by
;; default because there's more chance of there being a .h without a
;; .cc than a .h without a .c (ie. for C++ template files)
(defun c-c++-header ()
  "sets either  c-mode or c++-mode, whichever  is appropriate for
  header"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; and if that doesn't work, a function to toggle between c-mode and
;; c++-mode
(defun c-c++-toggle ()
  "toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

;; cc mode key bindings - applies to all CC modes (C, C++ etc.)
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-c\C-h" 'c-c++-toggle)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)
;; enable windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(setq windmove-wrap-around t)
