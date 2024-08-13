;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;             startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (when (< emacs-major-version 27)
    (setq package-enable-at-startup nil)
    ;; (package-initialize)
    (load-file (expand-file-name "early-init.el" user-emacs-directory)))
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "gk")
  (setq inhibit-compacting-font-caches t)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq frame-title-format
        '("%b - GNU Emacs [" (:eval (frame-parameter (selected-frame) 'window-id)) "]"))
  (tooltip-mode 0)
  (blink-cursor-mode 0)
  (global-unset-key (kbd "C-z"))
  ;; disable as it ruins keyboard macros
  (setq line-move-visual nil)
  ;; prefer to split windows vertically even on tall monitor
  (setq split-height-threshold 160)
  (setq split-width-threshold 160)
  (setq mouse-wheel-progressive-speed nil)
  (setq focus-follows-mouse t)
  (setq mouse-autoselect-window t)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode +1)
  ;; tab completion
  (setq-default indent-tabs-mode nil)
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
  (menu-bar-mode 0)
  (pixel-scroll-precision-mode)
  (setq native-comp-async-report-warnings-errors 'silent)
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
  (defmacro orgdr (&optional filename)
    (if filename
        `(concat (file-name-as-directory org-directory) ,filename)
      org-directory))
  (defmacro makehookedfun (hook &rest body)
  "Defines a function using BODY that is hooked to HOOK."
  (declare (indent 1))
  (let ((function (intern (concat (symbol-name hook) "-function"))))
    `(progn
       (defun ,function ()
         ,@body)
       (add-hook ',hook #',function)))))

(eval-and-compile ; `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize)
  (setq borg-compile-function 'borg-byte+native-compile))

(eval-and-compile ; `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

;;; WM integration
(use-package i3
  :unless (string-empty-p (shell-command-to-string "pgrep -x i3"))
  :config
  (require 'i3-integration)
  (i3-advise-visible-frame-list-on)
  (setq avy-frame-list-function #'visible-frame-list))

(use-package sway
  :unless (string-empty-p (shell-command-to-string "pgrep -x sway"))
  :config
  (defun sway-visible-frame-list ()
    "List of visible frames according to sway."
    (mapcar #'car (sway-list-frames (sway-tree) t)))
  (setq avy-frame-list-function #'sway-visible-frame-list))

;;; Theme
(progn                                  ;     fonts
  (defconst my-font "UbuntuMono Nerd Font-9")
  (set-frame-font my-font nil t)
  (add-to-list 'default-frame-alist `(font . ,my-font))
  ;; for modes that set the face explicitly
  (dolist (face '(default fixed-pitch fixed-pitch-serif))
    (set-face-attribute face nil :height 90 :family "UbuntuMono Nerd Font"))
  (dolist (face '(variable-pitch))
    (set-face-attribute face nil :height 90 :family "Ubuntu")))

(use-package doom-modeline
  :init (doom-modeline-mode +1)
  :config
  (setq doom-modeline-height 16)
  (setq doom-modeline-time nil)
  ;; this simple function works better while using direnv
  (setq doom-modeline-env-python-command (lambda () '("python" "--version"))))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package solaire-mode
  :hook (magit-blob-mode . turn-on-solaire-mode)
  :config
  (solaire-global-mode +1)
  ;; treemacs fix: https://github.com/hlissner/emacs-solaire-mode/issues/51
  (push '(treemacs-window-background-face . solaire-default-face)
        solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face)
        solaire-mode-remap-alist))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package zenburn-theme
  :init
  (setq zenburn-use-variable-pitch nil)
  (setq zenburn-scale-org-headlines nil)
  (setq zenburn-scale-outline-headlines nil)
  :config
  (load-theme 'zenburn t))

;;; Global
(progn ;     advice
  (defun kill-line--autoreindent (&optional arg)
    "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra
indent whitespace in front of the next line."
    (when (and (eolp) (not (bolp)))
      (save-excursion
        (forward-char 1)
        (delete-horizontal-space))))
  (advice-add 'kill-line :before #'kill-line--autoreindent))

(progn ;    `isearch'
  (setq isearch-allow-scroll t
        isearch-regexp-lax-whitespace t
        search-whitespace-regexp "[ \t\r\n]+"))

(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package auto-compile
  :config
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t))

(use-package autorevert
  :config
 (setq auto-revert-verbose nil)
 (setq auto-revert-remote-files nil))

(use-package avy
  :bind (("C-'" . #'avy-goto-char-timer)
         ("C-#" . #'avy-resume)
         :map isearch-mode-map
         ("C-'" . #'avy-isearch))
  :config
  (setq avy-all-windows 'all-frames
        avy-background t))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package dash
  :config (global-dash-fontify-mode))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-hl-flydiff
  :config (diff-hl-flydiff-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package eieio)

(use-package eldoc
  :when (version< "25" emacs-version)
  :config
  (global-eldoc-mode)
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package gk-extra
  :demand
  :bind (("C-;" . gk-select-current-line)
         ("C-M-;" . gk-comment-current-line)
         ("C-x C-c" . gk-kill-client-or-daemon)
         ("C-c d" . gk-insert-date)
         ("C-c t" . gk-insert-time)
         ("C-c T" . gk-insert-time-iso))
  :mode ("\\.h\\'" . gk-c-c++-header)
  :config
  (advice-add 'create-image :filter-args
              #'gk-create-image-with-background-color))

(use-package gk-other-window-repeat
  :bind (("C-x o" . gk-other-window-repeat)
         ("M-'" . other-window)))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

(use-package hl-todo
  :defer 2
  :config (global-hl-todo-mode))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences))

(use-package midnight
  :demand t
  :config
  (setq midnight-mode t)
  ;; some buffers that shouldn't be killed
  (setq clean-buffer-list-kill-never-buffer-names
        (append clean-buffer-list-kill-never-buffer-names
                '("*slime-repl sbcl*"
                  "*R*"
                  "init.el"))))

(use-package multiple-cursors
  :bind (("C->" . #'mc/mark-next-like-this)
         ("C-M->" . #'mc/unmark-next-like-this)
         ("C-<" . #'mc/mark-previous-like-this)
         ("C-M-<" . #'mc/unmark-previous-like-this)
         ("C-c C-<" . #'mc/mark-all-like-this))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))

(use-package no-littering
  :config
  (use-package recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (no-littering-theme-backups))

(use-package project
  :config
  (setq project-switch-commands #'project-find-file))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?x?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package server
  :commands (server-running-p)
  :config (or (server-running-p) (server-mode)))


(use-package shackle
  :commands (shackle-mode shackle-trace-functions shackle-untrace-functions)
  :config
  (setq shackle-rules
        '(("*org-roam*" :align t :select t)
          ("*xref*" :align t :select t)
          (("*Help*" helpful-mode) :align t :select t)
          (sql-interactive-mode :ignore t :noselect t)
          (compilation-mode :noselect t)
          (magit-revision-mode :noselect t)
          (treemacs-mode :align left))
        shackle-default-rule nil
        shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-inhibit-window-quit-on-same-windows t)
  (shackle-mode 1))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package tramp-sh
  :defer t
  :config (cl-pushnew 'tramp-own-remote-path tramp-remote-path))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-min-dir-content 0))

(use-package volatile-highlights
  :defer 2
  :config (volatile-highlights-mode t))

(use-package vundo
  :bind (("C-x u" . vundo)
         ("C-/" . undo-only)
         ("C-?" . undo-redo))
  :config
  (setq vundo-glyph-alist vundo-ascii-symbols))

(use-package which-func
  :config
  (setq which-func-modes
        '(python-mode
          python-ts-mode
          elisp-mode
          org-mode))
  (which-function-mode)
  (setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-function-mode mode-line-misc-info)))

(use-package which-key
  :config
  (which-key-mode))

(use-package whitespace
  :config
  (defun nice-whitespace-on ()
    (setq whitespace-style '(face tabs tab-mark trailing lines-tail))
    ;; highlight lines with more than `fill-column' characters
    (setq whitespace-line-column nil)
    (whitespace-mode 1))
  :hook (prog-mode . nice-whitespace-on))

(use-package xref
  :config
  (setq xref-search-program 'ripgrep))

;;; Completion
;; (use-package company
;;   :demand
;;   :init
;;   (setq tab-always-indent 'complete)
;;   :config
;;   (keymap-global-set "TAB" #'company-indent-or-complete-common)
;;   (keymap-set company-active-map "<return>" nil)
;;   (keymap-set company-active-map "TAB" #'company-complete-common)
;;   (keymap-set company-active-map "RET" nil)
;;   (keymap-set company-active-map "M-n" #'company-select-next-or-abort)
;;   (keymap-set company-active-map "M-p" #'company-select-previous-or-abort)
;;   (keymap-set company-active-map "C-SPC" #'company-complete-selection)
;;   (keymap-set company-active-map "M-SPC" #'company-complete-selection)
;;   (keymap-unset company-active-map "C-n")
;;   (keymap-unset company-active-map "C-p")
;;   (setq company-backends
;;         '((company-files
;;            company-keywords
;;            company-capf
;;            company-yasnippet)
;;           (company-abbrev company-dabbrev)))
;;   (setq company-idle-delay 0.2)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-frontends '(company-pseudo-tooltip-frontend
;;                             company-echo-metadata-frontend))
;;   ;; override company completion style
;;   (defun company-completion-styles (capf-fn &rest args)
;;     (let ((completion-styles '(basic partial-completion)))
;;       (apply capf-fn args)))
;;   (advice-add 'company-capf :around #'company-completion-styles)
;;   (global-company-mode))

(use-package corfu
  :init
  (global-corfu-mode)
  :bind (:map corfu-map
              ("RET" . nil)
              ("TAB" . #'corfu-insert)
              ("M-SPC" . #'corfu-insert)
              ("C-SPC" . #'corfu-insert-separator))
  :config
  (use-package corfu-echo
    :init
    (corfu-echo-mode))
  (use-package corfu-popupinfo
    :init
    (corfu-popupinfo-mode)
    :config
    (setq corfu-popupinfo-delay '(0.5 . 0.25)))
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        corfu-auto t
        corfu-quit-no-match 'separator
        corfu-auto-delay 0.2
        corfu-auto-prefix 1
        corfu-min-width 40
        corfu-preselect 'prompt)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package flymake
  :hook
  (emacs-lisp-mode . flymake-mode)
  :config
  (setq flymake-mode-line-lighter "!")
  :bind (:map flymake-mode-map
              ("C-c C-n" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error)
              ("C-c C-l" . flymake-show-buffer-diagnostics)))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'right
        marginalia-field-width 40))

(use-package orderless
  :init
  (setq completion-styles '(basic partial-completion orderless)
        completion-category-defaults nil
        completion-category-overrides '((project-file (styles orderless))
                                        (buffer (styles orderless))
                                        (command (styles orderless)))
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        orderless-smart-case nil))

(use-package vertico
  :demand
  :bind (("C-x C-b" . switch-to-buffer)
         :map vertico-map
         ("C-<return>" . vertico-exit-input)
         ("<tab>" . vertico-insert))
  :config
  (vertico-mode)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; org-mode fixes
  (defun org-enforce-basic-completion (&rest args)
    (minibuffer-with-setup-hook
        (:append
         (lambda ()
           (let ((map (make-sparse-keymap)))
             (keymap-set map "<tab>" #'minibuffer-complete)
             (use-local-map (make-composed-keymap (list map) (current-local-map))))
           (setq-local completion-styles (cons 'basic completion-styles)
                       vertico-preselect 'prompt)))
      (apply args)))
  (advice-add #'org-make-tags-matcher :around #'org-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'org-enforce-basic-completion))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode +1))

;;; General editing
(use-package eglot
  :hook ((dockerfile-mode
          js2-mode
          typescript-mode
          python-ts-mode
          rust-mode
          sh-mode
          yaml-mode)
         . eglot-ensure)
  :bind (("C-c C-r" . #'eglot-rename)
         ("C-c r" . #'eglot-rename)
         ("C-c C-a" . #'eglot-code-actions)
         ("C-c C-q" . #'eglot-code-action-quickfix))
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.1
        eglot-events-buffer-size 0
        eglot-report-progress nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider))
  ;; use flake8 by default
  (setq-default
   eglot-workspace-configuration
   '(:pylsp (:plugins (:pycodestyle (:enabled :json-false)
                       :mccabe (:enabled :json-false)
                       :pyflakes (:enabled :json-false)
                       :flake8 (:enabled t)
                       :rope_autoimport (:enabled :json-false)
                       :pylsp_mypy (:enabled t))
             :configurationSources ["flake8"]))))

(use-package flyspell
  :config (setq flyspell-issue-message-flag -1)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package highlight-indentation
  :hook ((python-mode python-ts-mode yaml-mode) . highlight-indentation-mode))

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

(use-package mic-paren
  :config
  (setq paren-sexp-mode 'mismatch)
  (paren-activate))

(use-package paredit
  :bind (:map paredit-mode-map
              ("M-?" . nil))
  :config
  (require 'gk-electric)
  (defun paredit-with-electric-return ()
    (paredit-mode +1)
    (local-set-key (kbd "RET") 'gk-electrify-return-if-match))
  ;; use with eldoc
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (defun nice-paredit-on ()
    (turn-off-smartparens-mode)
    (paredit-mode t)
    (turn-on-eldoc-mode)
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)
    (local-set-key (kbd "RET") 'gk-electrify-return-if-match)
    (eldoc-add-command 'gk-electrify-return-if-match)
    (show-paren-mode t)))

(use-package paren-face
  :config
  (global-paren-face-mode))

(use-package smartparens
  :demand
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-escape-quotes-after-insert nil) ; disable for c-mode
  (setq sp-ignore-modes-list
        '(web-mode git-commit-elisp-text-mode emacs-lisp-mode))
  (smartparens-global-mode)
  :bind (:map smartparens-mode-map
              ("C-)" . sp-forward-slurp-sexp)
              ("C-(" . sp-backward-slurp-sexp)))

(use-package subword
  :hook ((python-mode
          python-ts-mode
          yaml-mode
          go-mode
          clojure-mode
          cider-repl-mode
          js2-mode
          rjsx-mode
          typescript-mode)
         . subword-mode))

;;; Special modes

(use-package compile
  :config
  (setq compilation-scroll-output 'first-error))

(use-package deadgrep
  :commands (deadgrep)
  :bind (("C-z" . deadgrep)))

(use-package diff-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'diff-refine-changed nil :extend t)
    (set-face-attribute 'diff-refine-removed nil :extend t)
    (set-face-attribute 'diff-refine-added   nil :extend t)))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eshell
  :bind (("C-c s" . eshell)))

(use-package forge
  :after magit)

(use-package git-commit
  :defer t
  :config
  (setq git-commit-usage-message nil)
  (remove-hook 'git-commit-setup-hook 'git-commit-setup-changelog-support)
  (remove-hook 'git-commit-setup-hook 'git-commit-propertize-diff)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell t))

(use-package git-rebase
  :defer t
  :config
  (setq git-rebase-confirm-cancel nil)
  (setq git-rebase-show-instructions t))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

(use-package ielm
  :init
  (defun ielm-switch-to-buffer ()
    (interactive)
    (let ((ielm-buffer (get-buffer "*ielm*")))
      (if ielm-buffer
          (pop-to-buffer ielm-buffer)
        (ielm))))
  (defalias 'p #'princ)
  :config
  (makehookedfun ielm-mode-hook
    (nice-paredit-on))
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-z" . ielm-switch-to-buffer)
        ("C-c z" . ielm-switch-to-buffer)
        :map ielm-map
        ("C-<return>" . ielm-send-input)))
(use-package epkg
  :defer t
  :init
  (setq epkg-repository
        (expand-file-name "var/epkgs/" user-emacs-directory))
  (setq epkg-database-connector
        (if (>= emacs-major-version 29) 'sqlite-builtin 'sqlite-module)))

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  ;;
  ;; Key bindings
  :bind (("C-c i" . magit-status)
         ("C-c b" . magit-blame)
         ("C-c g" . magit-dispatch)
         ("C-c f" . magit-file-dispatch))
  ;;
  ;; Global settings
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-to-list 'magit-repository-directories (cons "~/.emacs.d/" 0))
  (add-to-list 'magit-repository-directories (cons "~/.emacs.d/lib/" 1))
  (setq magit-auto-revert-mode t
        magit-auto-revert-immediately t)
  ;;
  ;; Window management
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-v1
        magit-save-repository-buffers 'dontask)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-revision-buffer)
  (add-hook 'magit-section-movement-hook 'magit-status-maybe-update-blob-buffer)
  (add-hook 'magit-section-movement-hook 'magit-log-maybe-update-blob-buffer)
  ;;
  ;; Status buffer settings
  (setq magit-section-visibility-indicator nil)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-pushremote
                          'magit-insert-unpushed-to-upstream)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  (setq magit-module-sections-hook
        '(magit-insert-modules-overview
          magit-insert-modules-unpulled-from-upstream
          magit-insert-modules-unpushed-to-upstream))
  ;;
  ;; Branch settings
  (setq magit-branch-prefer-remote-upstream '("main" "master" "develop"))
  (setq magit-branch-adjust-remote-upstream-alist
        '(("origin/main" . ("main" "master" "next" "release" "maint"))
          ("origin/master" . ("main" "master" "next" "release" "maint"))))
  ;;
  ;; Tag settings
  (setq magit-release-tag-regexp "\\`\
\\(?1:\\(?:v\\(?:ersion\\)?\\|r\\(?:elease\\)?\\)?[-_]?\\)?\
\\(?2:[0-9]+\\(?:\\.[0-9]+\\)*\\)\\(\\(a\\|b\\|rc\\)[0-9]+\\)?\\'")
  ;;
  ;; Diff settings
  (setq magit-diff-refine-hunk 'all)
  ;;
  ;; Misc settings
  (setq magit-list-refs-sortby "-creatordate")
  (setq magit-refs-pad-commit-counts t))

(use-package magit-wip
  :after magit
  :config (magit-wip-mode))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package transient
  :config
  (setq transient-display-buffer-action '(display-buffer-below-selected)))

(use-package treemacs
  :bind ("C-x t" . #'treemacs-add-and-display-current-project-exclusively)
  :config
  (setq treemacs-is-never-other-window t
        treemacs-follow-after-init t
        treemacs-default-visit-action #'treemacs-visit-node-close-treemacs)
  (treemacs-fringe-indicator-mode 'only-when-focused))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package vterm)

;;; Editing modes
(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package asm-mode
  :bind (:map asm-mode-map
         ("C-c C-c" . compile)))

(use-package cc-mode
  :config
  (setq c-auto-newline 1)
  (setq c-hungry-delete-key 1)
  :bind (:map c-mode-base-map
         ("C-c C-c" . compile)
         ("C-m" . c-context-line-break)
         ("C-c C-h" . gk-c-c++-toggle)
         ("RET" . gk-electrify-return-if-match)))

(use-package cider
  :mode (("\\.clj$" . clojure-mode))
  :hook ((clojure-mode . nice-paredit-on)
         (cider-repl-mode . nice-paredit-on))
  :bind (:map clojure-mode-map
         ("C-c z" . cider-switch-to-repl-buffer)
         ("C-c C-z" . cider-switch-to-repl-buffer)))

(use-package copy-as-format
  :bind (("C-c w g" . copy-as-format-github)
         ("C-c w h" . copy-as-format-hipchat)
         ("C-c w j" . copy-as-format-jira)
         ("C-c w m" . copy-as-format-markdown)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w s" . copy-as-format-slack)))

(use-package css-mode
  :mode ("\\.css\\'"
         "\\.mss\\'")
  :bind (:map css-mode-map
              ("RET" . gk-electrify-return-if-match)))

(use-package dbt-mode)

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package ebuild-mode
  :mode ("\\.ebuild\\'"
         "\\.eclass\\'"))

(use-package ess
  :init (require 'ess-site)
  :config
  (setq ess-r-package-auto-activate nil)
  (defun ess-eval-defun-key ()
    (interactive)
    (ess-eval-function-or-paragraph t))
  (defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))
  :bind (:map ess-mode-map
         ("C-c z" . ess-switch-to-inferior-or-script-buffer)
         ("C-c C-c" . ess-eval-defun-key)
         ("C-c C-k" . ess-eval-buffer)
         ("C-c C-b" . ess-force-buffer-current)
         :map inferior-ess-mode-map
         ("C-c M-o" . clear-shell)))

(use-package geiser
  :commands (geiser-mode)
  :load-path "lib/geiser/elisp"
  :init
  (setq geiser-active-implementations '(racket guile)))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . (lambda () (setq tab-width 4)))
  :config
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (defun go-mode-compile ()
    (interactive)
    (compile "go install"))
  (defun go-mode-test ()
    (interactive)
    (compile "go test -v && go vet && golint"))
  :bind (:map go-mode-map
         ("C-c C-c" . go-mode-compile)
         ("C-c C-t" . go-mode-test)
         ("M-." . godef-jump)
         ("RET" . gk-electrify-return-if-match)))

(use-package jinja2-mode
  :mode ("\\.j2\\'"))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default js2-basic-offset 2)
  :config
  (require 'smartparens-javascript)
  :bind (:map js2-mode-map
              ("RET" . gk-electrify-return-if-match)
              ("M-." . xref-find-definitions)))

(use-package latex
  :defer t
  :config
  (use-package preview)
  (add-to-list 'TeX-view-program-selection '(output-pdf "xdg-open"))
  :hook ((LaTeX-mode . (lambda ()
                         (TeX-PDF-mode)
                         (auto-fill-mode)
                         (turn-on-orgtbl)
                         (turn-on-reftex)))))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :bind (:map ledger-mode-map
              ("C-c C-c" . ledger-report))
  :config
  (setq ledger-default-date-format ledger-iso-date-format))

(use-package lisp-mode
  :mode "\\.lisp\\'"
  :hook ((lisp-mode . nice-paredit-on))
  :config
  (use-package hyperspec)
  (use-package slime-company
    :commands (slime-company)
    :config (setq slime-company-completion 'fuzzy))
  (use-package slime-complete-locals
    :commands (slime-complete-locals))
  (use-package slime
    :demand
    :commands (slime)
    :config
    (setq inferior-lisp-program "sbcl")
    (slime-setup '(slime-fancy
                   slime-banner
                   slime-asdf
                   slime-company
                   slime-complete-locals)))
  :bind
  (:map lisp-mode-map
   ("C-c C-z" . slime-switch-to-output-buffer)
   ("C-c z" . slime-switch-to-output-buffer)
   ("C-c e" . macrostep-expand)))

(use-package lisp-mode                  ; emacs-lisp-mode
  :config
  (defun eval-buffer-key ()
    (interactive)
    (message "Evaluating buffer...")
    (eval-buffer)
    (message "Buffer evaluated."))
  (defun eval-defun-key (edebug-it)
    (interactive "P")
    (let (beg ol)
      (save-excursion
        (end-of-defun)
        (beginning-of-defun)
        (setq beg (point))
        (end-of-defun)
        (setq ol (make-overlay beg (point))))
      (overlay-put ol 'face 'highlight)
      (unwind-protect
          (progn
            (eval-defun edebug-it)
            (sit-for 0.1))
        (delete-overlay ol))))
  (makehookedfun emacs-lisp-mode-hook
    (outline-minor-mode)
    (reveal-mode)
    (nice-paredit-on))
  (makehookedfun lisp-data-mode-hook
    (outline-minor-mode)
    (reveal-mode)
    (nice-paredit-on))
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (makehookedfun lisp-interaction-mode-hook
    (indent-spaces-mode))
  :bind
  (:map emacs-lisp-mode-map
   ("C-c C-k" . eval-buffer-key)
   ("C-c C-c" . eval-defun-key)
   ("C-l" . paredit-recentre-on-sexp)
   ("C-c d" . toggle-debug-on-error)
   ("M-p" . outline-move-subtree-up)
   ("M-n" . outline-move-subtree-down)
   ("<backtab>". outline-cycle)
   ("M-S-<iso-lefttab>". outline-cycle-buffer)
   :map lisp-interaction-mode-map
   ("C-<return>" . #'eval-print-last-sexp)))

(use-package make-mode
  :defer t
  :bind
  (:map makefile-mode-map
   ("C-c C-c" . compile)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (require 'smartparens-markdown)
  (setq markdown-asymmetric-header t))

(use-package nginx-mode
  :mode "nginx-mode")

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c C-x C-j" . org-clock-goto)
   ("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-x" . org-clock-in-last)
   :map org-mode-map
   ("M-p" . org-metaup)
   ("M-n" . org-metadown)
   ("C-M->" . org-metaright)
   ("C-M-<" . org-metaleft)
   ("M-." . #'org-open-at-point)
   ("M-," . #'org-mark-ring-goto)
   ("C-M-SPC" . #'org-mark-subtree)
   :map org-agenda-mode-map
   ("M-n" . #'org-agenda-forward-block)
   ("M-p" . #'org-agenda-backward-block))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . org-bullets-mode)
         (org-mode . flyspell-mode)
         (org-mode . reveal-mode)
         (org-mode . (lambda () (yas-minor-mode -1)))
         (org-babel-after-execute . org-redisplay-inline-images)
         (org-agenda-finalize . (lambda () (setq-local cursor-type nil))))
  :config
  (use-package gk-org)
  (use-package org-bullets)
  (use-package org-tempo)
  (use-package org-habit
    :config
    (setq org-habit-preceding-days 28
          org-habit-following-days 10
          org-habit-graph-column 44))
  (require 'smartparens-org)
  (add-to-list 'org-modules 'org-habit)
  (setq org-startup-indented t
        org-indent-indentation-per-level 1
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-return-follows-link nil
        org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               (file . find-file)
                               (wl . wl-other-frame))
        org-archive-location "~/org/archive/%s::")
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (sqlite . t)
     (R . t)
     (shell . t)
     (python . t)
     (plantuml . t)))
  (setq org-plantuml-exec-mode 'plantuml
        org-confirm-babel-evaluate nil)
  ;; exporting
  (use-package htmlize)
  (use-package org-contrib
    :config
    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines)))
  (use-package ox-hugo)
  (use-package ox-latex)
  (use-package ox-md)
  (use-package ox-reveal)
  (use-package ox-twbs)
  (use-package ox-jira)
  (setq org-latex-to-pdf-process '("latexmk -pdf %f"))
  (setq org-export-latex-listings 'minted)
  (defun orgtbl-to-latex-booktabs (table params)
    "Convert the Orgtbl mode TABLE to LaTeX using booktabs package."
    (let* ((alignment (mapconcat (lambda (x) (if x "r" "l"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :tstart "\\toprule"
             :tend "\\bottomrule\n"
             :lstart "" :lend " \\\\" :sep " & "
             :efmt "%s\\,(%s)" :hline "\\midrule")))
      (orgtbl-to-generic table (org-combine-plists params2 params))))
  (eval-after-load "org-table"
    '(progn
       (setq orgtbl-radio-table-templates
             (cl-delete-if (lambda (x) (equal (car x) 'latex-mode))
                        orgtbl-radio-table-templates))
       (add-to-list 'orgtbl-radio-table-templates
                    '(latex-mode "% BEGIN RECEIVE ORGTBL %n\n"
                                 "% END RECEIVE ORGTBL %n\n"
                                 "\\begin{comment}\n#+ORGTBL: SEND %n "
                                 "orgtbl-to-latex-booktabs :splice nil "
                                 ":skip 0 :no-escape t\n"
                                 "| | |\n\\end{comment}\n"))))
  ;; export with CSS classes instead of explicit colours
  (setq org-html-htmlize-output-type 'css)
  (setq org-html-htmlize-font-prefix "org-")
  (setq org-twbs-htmlize-output-type 'css)
  (setq org-twbs-htmlize-font-prefix "org-")
  ;; capture
  (setq org-default-notes-file (orgdr "notes.org"))
  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,(orgdr "todo.org") "Misc (Captured)")
           "* TODO %?\n%U\n%a")
          ("d" "Diary" entry (file+headline ,(orgdr "diary.org") "Captured"))
          ("j" "Journal" entry (file+olp+datetree ,(orgdr "journal.org"))
           "* %? %^g\nEntered on %U\n%i")
          ("i" "Idea" entry (file ,(orgdr "ideas.org"))
           "* %?\n%U\n%a")))
  ;; agenda
  (setq org-agenda-custom-commands
        '(("h" "Home" tags-todo "-@work")
          ("w" "Work" tags-todo "-@home")
          ("u" "Untagged" tags-todo "-{.*}")
          ("c" "Today" ((agenda "" ((org-agenda-span 3)
                                    (org-agenda-show-log t)
                                    (org-agenda-log-mode-items '(clock state))
                                    (org-agenda-skip-deadline-if-done t)
                                    (org-agenda-skip-scheduled-if-done t)
                                    (org-agenda-include-inactive-timestamps t)
                                    (org-agenda-skip-deadline-prewarning-if-scheduled t)))
                        (tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "Top priority")))
                        (todo "STARTED" ((org-agenda-overriding-header "Started")
                                         (org-agenda-todo-list-sublevels t)))
                        (todo "NEXT" ((org-agenda-overriding-header "Next")
                                      (org-agenda-todo-list-sublevels t)))
                        (todo "WAITING" ((org-agenda-overriding-header "Waiting")
                                         (org-agenda-todo-list-sublevels t)))
                        (todo "ETERNAL" ((org-agenda-overriding-header "Eternal")
                                         (org-agenda-todo-list-sublevels t)))))
          ("y" "Yesterday" ((agenda "" ((org-agenda-start-day "-1d")
                                        (org-agenda-include-deadlines nil)
                                        (org-agenda-span 1)
                                        (org-agenda-show-log t)
                                        (org-agenda-log-mode-items '(closed state))
                                        (org-agenda-clockreport-mode t)
                                        (org-agenda-clockreport-parameter-plist
                                         '(:maxlevel 4 :link t :fileskip0 t :formula %))
                                        (org-agenda-skip-scheduled-if-done t)
                                        (org-agenda-include-inactive-timestamps t)))
                            (todo "STARTED" ((org-agenda-todo-list-sublevels t)))
                            (todo "WAITING" ((org-agenda-todo-list-sublevels t)))))
          ("T" "Timesheet" ((agenda "" ((org-agenda-start-on-weekday 1)
                                        (org-agenda-include-deadlines nil)
                                        (org-agenda-span 7)
                                        (org-agenda-show-log t)
                                        (org-agenda-log-mode-items '(clock))
                                        (org-agenda-clockreport-mode t)
                                        (org-agenda-clockreport-parameter-plist
                                         '(:maxlevel 3
                                           :tcolumns 1
                                           :link t
                                           :tags t
                                           :fileskip0 t
                                           :formula "$5='(* 37.5 (/ (string-to-number (replace-regexp-in-string \"[\*h]\" \"\" $4)) (string-to-number (replace-regexp-in-string \"[\*h]\" \"\" @2$4))));%.2f"))
                                        (org-duration-format '(("h" . t) (special . 2)))))))))
  (setq org-agenda-files (directory-files (orgdr) t "\\.org$" t))
  (setq org-agenda-include-diary nil)
  (setq org-agenda-todo-list-sublevels nil)
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-prefix-format '((agenda . " %-2i %-12:c%?-12t% s")
                                   (todo .   " %-2i %-12:c")
                                   (tags .   " %-2i %-12:c")
                                   (search . " %-2i %-12:c")))
  (setq org-agenda-current-time-string "󰳝┄┄┄┄┄┄┄┄┄┄┄┄┄┄ 󰔠 Now")
  (setq org-time-stamp-custom-formats
        '("<%A, %e %B %Y>" . "<%A, %e %B %Y %H:%M>"))
  (setq org-log-done 'time)
  (setq org-blank-before-new-entry
        '((heading . t) (plain-list-item . nil)))

  (defun gk-nerd-agenda-icons (fun prefix alist)
    "Makes an org agenda alist"
    (mapcar (pcase-lambda (`(,category . ,icon))
              `(,category
                (,(funcall fun (concat prefix icon) :height 1.2))))
            alist))

  (setq org-agenda-category-icon-alist
        (append
         (gk-nerd-agenda-icons #'nerd-icons-mdicon "nf-md-"
                               '(("Birthday" . "cake_variant")
                                 ("Diary" . "book_clock")
                                 ("Holiday" . "umbrella_beach")
                                 ("Chore" . "broom")
                                 ("Regular" . "autorenew")
                                 ("Sprint" . "run_fast")
                                 ("Maint" . "tools")
                                 ("ELT" . "pipe")
                                 ("Devops" . "gitlab")
                                 ("Blog" . "fountain_pen_tip")
                                 ("FOSS" . "code_braces")
                                 ("Tool" . "toolbox")
                                 ("Todo" . "list_status")))
         (gk-nerd-agenda-icons #'nerd-icons-sucicon "nf-custom-"
                               '(("Project" . "folder_git_branch")
                                 ("Emacs" . "emacs")
                                 ("Org" . "orgmode")))
         '(("" '(space . (:width (11)))))))

  ;; todo
  (setq org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE"))
        org-agenda-todo-ignore-scheduled 'future
        org-agenda-tags-todo-honor-ignore-options t)
  ;; clocking
  (require 'org-clock)
  (require 'org-timer)
  (setq org-timer-default-timer 25)
  (setq org-clock-clocked-in-display 'frame-title)
  (setq org-timer-display 'frame-title)
  (setq org-clock-frame-title-format
        (append frame-title-format
                '(" - Org clocked in: " org-mode-line-string)))
  ;; refile
  (setq org-refile-targets
        '((nil :level . 1)
          (nil :todo . "STARTED")
          (nil :tag . "sprint")))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  ;; use log drawer
  (setq org-log-into-drawer t)
  (defun diary-limited-cyclic (recurrences interval y m d)
    "For use in emacs diary. Cyclic item with limited number of recurrences.
Occurs every INTERVAL days, starting on YYYY-MM-DD, for a total of
RECURRENCES occasions."
    (let ((startdate (calendar-absolute-from-gregorian (list m d y)))
          (today (calendar-absolute-from-gregorian date)))
      (and (not (minusp (- today startdate)))
           (zerop (% (- today startdate) interval))
           (< (floor (- today startdate) interval) recurrences))))
  ;; this code removes time grid lines that are within an appointment
  (defun org-time-to-minutes (time)
    "Convert an HHMM time to minutes"
    (+ (* (/ time 100) 60) (% time 100)))
  (defun org-time-from-minutes (minutes)
    "Convert a number of minutes to an HHMM time"
    (+ (* (/ minutes 60) 100) (% minutes 60)))
  ;; setup default file readers
  (eval-after-load "org"
    '(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

(use-package org-download
  :hook ((dired-mode . org-download-enable)))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-directory "~/org/roam"
        org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

(use-package org-roam-dailies
  :bind (("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?\n%U"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :config
  (setq plantuml-executable-path "plantuml"
        plantuml-default-exec-mode 'executable))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package python
  :mode ("\\.py\\'" . python-ts-mode)
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
  (setq python-prettify-symbols-alist
        '(("lambda" . 955)))
  (require 'smartparens-python)
  :bind
  (:map python-mode-map
        ("RET" . gk-electrify-return-if-match))
  (:map python-ts-mode-map
        ("RET" . gk-electrify-return-if-match)))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :init
  (setq-default js2-basic-offset 2)
  :bind (:map rjsx-mode-map
              ("RET" . gk-electrify-return-if-match)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
         ("RET" . gk-electrify-return-if-match))
  :config
  (require 'smartparens-rust)
  (setq rust-mode-tree-sitter-derive t))

(use-package scheme
  :config
  (makehookedfun scheme-mode-hook
    (nice-paredit-on)
    (geiser-mode)))

(use-package smerge-mode
  :defer t
  :config
  (when (>= emacs-major-version 27)
    (set-face-attribute 'smerge-refined-removed nil :extend t)
    (set-face-attribute 'smerge-refined-added   nil :extend t)))

(use-package sql)

(use-package sql-indent
  :hook ((sql-mode . sqlind-minor-mode)))

(use-package sqlup-mode
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(use-package tex
  :defer t
  :hook ((TeX-mode . (lambda ()
                       (TeX-PDF-mode)
                       (auto-fill-mode)
                       (setq tab-stop-list (number-sequence 3 45 3))))))

(use-package tex-site                   ; auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  (setq reftex-plug-into-AUCTeX t)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-newline-function 'newline-and-indent)
  (setq reftex-default-bibliography '("bibliography"))
  (require 'smartparens-latex))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :bind (:map web-mode-map
              ("C-c C-c" . browse-url-of-file))
  :config (require 'smartparens-html))

(use-package yaml-mode
  :mode "\\.\\(ya?\\|m\\)ml\\'")

(use-package yaml-pro
  :hook ((yaml-mode . yaml-pro-mode)
         (yaml-ts-mode . yaml-pro-ts-mode)))

(progn                                  ; misc settings
  ;; set some default styles
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (c-mode . "k&r")
          (c++-mode . "stroustrup")
          (other . "gnu")))

  ;; column and line number
  (column-number-mode 1)
  (line-number-mode 1)

  ;; enable auto fill mode globally
  (setq auto-fill-mode 1)
  ;; default fill length
  (setq-default fill-column 88)

  (defun unfill-paragraph ()
    "Unfill paragraph at or after point."
    (interactive "*")
    (let ((fill-column most-positive-fixnum))
      (fill-paragraph nil (region-active-p))))

  ;; set some default modes
  ;; lex
  (add-to-list 'auto-mode-alist '("\\.l\\'" . c-mode))
  ;; matlab
  (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
  ;; XML
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . sgml-mode))

  ;; ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)

  (global-set-key (kbd "C-M-'") 'other-frame)
  (global-set-key (kbd "M-SPC")
                  (lambda ()
                    (interactive)
                    (insert " ")
                    (backward-char)))

  ;; don't make lockfiles (.#)
  (setq create-lockfiles nil)

  ;; keep backup files neatly out of the way in .~/
  (setq backup-directory-alist '(("." . ".~")))

  ;; use character folding in search (a matches á etc)
  (setq search-default-mode 'char-fold-to-regexp)

  ;; rat yank doesn't move cursor
  (setq mouse-yank-at-point t))

;;; Tree-sitter stuff

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;;; Tequila worms

(progn                                  ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(when load-file-name
  (find-file (concat (file-name-sans-extension load-file-name)
                     ".el")))

;;; Calendar
(use-package uk-holidays
  :config
  (setq holiday-general-holidays nil
        holiday-christian-holidays nil
        holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-bahai-holidays nil
        holiday-oriental-holidays nil)
  (setq calendar-holidays holiday-uk-holidays))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
