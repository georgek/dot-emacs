;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  ;; increase temporarily during startup
  (setq gc-cons-threshold (* 256 1024 1024))
  ;; disable temporarily during startup
  (defvar file-name-handler-alist-old file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "gk")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (setq frame-title-format '("%b - GNU Emacs"))
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
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
  (setq-default indent-tabs-mode nil))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

;;; i3 integration
(use-package i3
  :config
  (require 'i3-integration)
  (i3-advise-visible-frame-list-on))

;; theme
(defvar my-font "Ubuntu Mono-9")
(dolist (face '(default fixed-pitch fixed-pitch-serif))
 (set-face-attribute face nil :height 90 :family "Ubuntu Mono"))
(dolist (face '(variable-pitch))
 (set-face-attribute face nil :height 90 :family "Ubuntu"))
(set-frame-font my-font)
(add-to-list 'default-frame-alist `(font . ,my-font))
(use-package zenburn-theme
  :init
  (setq zenburn-use-variable-pitch nil)
  (setq zenburn-scale-org-headlines t)
  (setq zenburn-scale-outline-headlines t)
  :config
  (load-theme 'zenburn t))

(use-package rich-minority
  :config
  (setq rm-blacklist
        (format
         "^ \\(%s\\)$"
         (mapconcat #'identity
                    '("Fly" "Projectile.*" "Reveal"
                      "Counsel" "Ivy" "company" "yas"
                      "ElDoc" "||" "WK" "VHl" "," "ws")
                    "\\|"))))

(use-package smart-mode-line
  :config
  (sml/setup)
  (setq sml/name-width 50)
  (setq sml/replacer-regexp-list
        '(("^~/\\.emacs\\.d/" ":ed:")
          ("^/sudo:.*:" ":su:"))))

(use-package volatile-highlights
  :defer 2
  :config (volatile-highlights-mode t))

(use-package hl-todo
  :defer 2
  :config (global-hl-todo-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package whitespace
  :config
  (defun nice-whitespace-on ()
    (setq whitespace-style '(face tabs tab-mark trailing lines-tail))
    ;; highlight lines with more than `fill-column' characters
    (setq whitespace-line-column nil)
    (whitespace-mode 1))
  :hook (prog-mode . nice-whitespace-on))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package gk-extra
  :demand
  :bind (("C-;" . gk-select-current-line)
         ("C-M-;" . gk-comment-current-line)
         ("C-x C-c" . gk-kill-client-or-daemon)
         ("C-c d" . gk-insert-date)
         ("C-c t" . gk-insert-time))
  :mode ("\\.h\\'" . gk-c-c++-header))

(use-package no-littering
  :config
  (use-package recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer               nil)
  (setq auto-compile-mode-line-counter            t)
  (setq auto-compile-source-recreate-deletes-dest t)
  (setq auto-compile-toggle-deletes-nonlib-dest   t)
  (setq auto-compile-update-autoloads             t)
  (add-hook 'auto-compile-inhibit-compile-hook
            'auto-compile-inhibit-compile-detached-git-head))

(use-package epkg
  :defer t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))

(use-package custom
  :no-require t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(defmacro makehookedfun (hook &rest body)
  "Defines a function using BODY that is hooked to HOOK."
  (declare (indent 1))
  (let ((function (intern (concat (symbol-name hook) "-function"))))
    `(progn
       (defun ,function ()
         ,@body)
       (add-hook ',hook #',function))))

;;; Long tail

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (add-to-list 'projectile-globally-ignored-file-suffixes "~")
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-global-mode))

(use-package company
  :config
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-SPC") #'company-complete-selection)
  (define-key company-active-map (kbd "M-SPC") #'company-complete-selection)
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev)))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (global-company-mode))

(use-package lsp-mode
  :hook ((js2-mode rjsx-mode typescript-mode) . lsp)
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil)
  (require 'company-lsp)
  (setq company-lsp-cache-candidates 'auto)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-lsp))))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently))

(use-package paredit
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

(use-package smartparens
  :demand
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-escape-quotes-after-insert nil)  ; disable for c-mode
  (setq sp-ignore-modes-list '(web-mode))
  (smartparens-global-mode)
  :bind (:map smartparens-mode-map
         ("C-)" . sp-forward-slurp-sexp)
         ("C-(" . sp-backward-slurp-sexp)))

(use-package subword
  :hook ((python-mode
          yaml-mode
          go-mode
          clojure-mode
          cider-repl-mode
          js2-mode
          rjsx-mode
          typescript-mode)
         . subword-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package lisp-mode
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

  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))

  (makehookedfun lisp-interaction-mode-hook
    (indent-spaces-mode))

  :bind
  (:map emacs-lisp-mode-map
   ("C-c C-k" . eval-buffer-key)
   ("C-c C-c" . eval-defun-key)
   ("C-c C-l" . paredit-recentre-on-sexp)
   ("C-c d" . toggle-debug-on-error)))

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
   ("C-c e" . macrostep-expand)))

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

(use-package scheme
  :config
  (makehookedfun scheme-mode-hook
    (nice-paredit-on)
    (geiser-mode)))

(use-package geiser
  :commands (geiser-mode)
  :load-path "lib/geiser/elisp"
  :init
  (setq geiser-active-implementations '(racket guile)))

(use-package cider
  :mode (("\\.clj$" . clojure-mode))
  :hook ((clojure-mode . nice-paredit-on)
         (cider-repl-mode . nice-paredit-on))
  :bind (:map clojure-mode-map
         ("C-c z" . cider-switch-to-repl-buffer)
         ("C-c C-z" . cider-switch-to-repl-buffer)))

(use-package selectrum
  :demand
  :bind (("C-x C-b" . switch-to-buffer))
  :config
  (selectrum-mode +1))

(use-package prescient
  :config
  (selectrum-prescient-mode +1)
  (company-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package transient
  :config
  (setq transient-display-buffer-action '(display-buffer-below-selected)))

(use-package magit
  :defer t
  :bind (("C-c i"   . magit-status)
         ("C-c b"   . magit-blame))
  :config
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
  (setq magit-branch-prefer-remote-upstream '("master" "develop"))
  (setq magit-branch-adjust-remote-upstream-alist
        '(("origin/master" "master" "release")))
  (setq magit-release-tag-regexp "\\`\
\\(?1:\\(?:v\\(?:ersion\\)?\\|r\\(?:elease\\)?\\)?[-_]?\\)?\
\\(?2:[0-9]+\\(?:\\.[0-9]+\\)*\\)\\(\\(a\\|b\\|rc\\)[0-9]+\\)?\\'")
  (setq magit-section-visibility-indicator nil)
  (setq magit-list-refs-sortby "-creatordate")
  (setq magit-diff-refine-hunk 'all)
  (setq magit-refs-pad-commit-counts t))

(use-package forge
  :after magit)

(use-package org-presie)

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-~" . (lambda () (interactive) (org-agenda nil "n")))
   ("C-c C-x C-j" . org-clock-goto)
   ("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-x" . org-clock-in-last)
   :map org-mode-map
   ("M-p" . org-metaup)
   ("M-n" . org-metadown)
   ("C-M->" . org-metaright)
   ("C-M-<" . org-metaleft))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . org-bullets-mode)
         (org-mode . flyspell-mode)
         (org-mode . reveal-mode))
  :config
  (use-package org-bullets)
  (require 'smartparens-org)
  (setq org-startup-indented t)

  (defmacro orgdr (&optional filename)
    (if filename
        `(concat (file-name-as-directory org-directory) ,filename)
      org-directory))
  (setq org-return-follows-link t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)
     (sqlite . t)
     (R . t)
     (shell . t)
     (python . t)))

  ;; a named source block
  (add-to-list 'org-structure-template-alist
               '("S" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC")
               '("N" "#+NAME: ?"))

  ;; exporting
  (use-package htmlize)
  (use-package ox-md)
  (use-package ox-pandoc)
  (use-package ox-twbs)
  (use-package ox-latex)
  (use-package ox-reveal)
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
             (delete-if (lambda (x) (equal (car x) 'latex-mode))
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
           "* TODO %?\n %U\n %a")
          ("d" "Diary" entry (file+headline ,(orgdr "diary.org") "Captured"))
          ("j" "Journal" entry (file+olp+datetree ,(orgdr "journal.org"))
           "* %? %^g\nEntered on %U\n %i")
          ("i" "Idea" entry (file ,(orgdr "ideas.org"))
           "* %?\n %U\n %a")))

  ;; agenda stuff
  (custom-set-variables
   '(org-agenda-custom-commands
     '(("n" "Agenda and all TODOs"
        ((agenda "")
         (alltodo ""))
        ((org-agenda-tag-filter '("-NOAGENDA")))))))
  (setq org-agenda-files (directory-files-recursively (orgdr) "\\.org$" nil))
  (setq org-agenda-include-diary t)
  (setq org-agenda-span 14)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo .   " %i %-12:c")
                                   (tags .   " %i %-12:c")
                                   (search . " %i %-12:c")))

  (setq org-time-stamp-custom-formats
        '("<%A, %e %B %Y>" . "<%A, %e %B %Y %H:%M>"))
  (setq org-log-done 'time)
  (setq org-blank-before-new-entry
        '((heading . t) (plain-list-item . nil)))
  (setq org-todo-keywords (quote((sequence "TODO" "WAITING" "|" "DONE"))))

  (require 'org-clock)
  (require 'org-timer)
  (setq org-timer-default-timer 25)

  (setq org-clock-clocked-in-display 'frame-title)
  (setq org-timer-display 'frame-title)
  (setq org-clock-frame-title-format
        (append frame-title-format
                '(" - Org clocked in: " org-mode-line-string)))

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

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (require 'smartparens-markdown)
  (setq markdown-asymmetric-header t))

(use-package yaml-mode
  :mode "\\.\\(ya?\\|m\\)ml\\'")

(use-package make-mode
  :defer t
  :bind
  (:map makefile-mode-map
   ("C-c C-c" . compile)))

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package groovy-mode
  :mode "Jenkinsfile")

(use-package nginx-mode
  :mode "nginx-mode")

(use-package compile
  :config
  (setq compilation-scroll-output 'first-error))

(use-package cc-mode
  :config
  (setq c-auto-newline 1)
  (setq c-hungry-delete-key 1)
  :bind (:map c-mode-base-map
         ("C-c C-c" . compile)
         ("C-m" . c-context-line-break)
         ("C-c C-h" . gk-c-c++-toggle)
         ("RET" . gk-electrify-return-if-match)))

(use-package asm-mode
  :bind (:map asm-mode-map
         ("C-c C-c" . compile)))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences))

(use-package flycheck
  :demand t
  :config
  (setq flycheck-global-modes
        '(not
          org-mode
          markdown-mode))
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
                '(c/c++-clang))
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8rc "setup.cfg")
  (flycheck-add-next-checker 'python-pycompile 'python-flake8)
  :bind (:map
         flycheck-mode-map
         ("C-c C-n" . flycheck-next-error)
         ("C-c C-p" . flycheck-previous-error)
         ("C-c C-l" . flycheck-list-errors)))

(use-package flyspell
  :config (setq flyspell-issue-message-flag -1)
  :hook ((text-mode . flyspell-mode)))

(use-package uniquify
  :demand t
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
  (setq uniquify-min-dir-content 0))

(use-package hippie-expand
  :bind (("M-/" . hippie-expand)))

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

(use-package tex
  :defer t
  :hook ((TeX-mode . (lambda ()
                       (TeX-PDF-mode)
                       (auto-fill-mode)
                       (setq tab-stop-list (number-sequence 3 45 3))))))

(use-package eshell
  :bind (("C-c s" . eshell)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq python-prettify-symbols-alist
        '(("lambda" . 955)))
  (require 'smartparens-python)
  :bind (:map python-mode-map
         ("RET" . gk-electrify-return-if-match)))

(use-package elpy
  :after python
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-backend "jedi")
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq-default elpy-django-test-runner-args
                '("test" "--noinput" "--keepdb" "--parallel" "--failfast"))
  (unbind-key "C-c C-n" elpy-mode-map)
  (unbind-key "C-c C-p" elpy-mode-map)
  :bind (:map elpy-mode-map
         ("C->" . #'elpy-nav-indent-shift-right)
         ("C-<" . #'elpy-nav-indent-shift-left)))

(use-package pylint
  :after python
  :commands (pylint)
  :hook ((python-mode . pylint-add-menu-items)
         (python-mode . pylint-add-key-bindings)))

(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("bash-fc" . sh-mode)))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren-face
  :config
  (global-paren-face-mode))

(use-package mic-paren
  :config
  (setq paren-sexp-mode 'mismatch)
  (paren-activate))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :when (version< "25" emacs-version)
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil))
  ;; disable remote version control
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package avy
  :config
  (setq avy-background t)
  :bind
  (("C-:" . avy-goto-char)))

(use-package ace-window
  :config
  (setq aw-scope 'frame)
  :bind
  (("M-'" . ace-window)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

(use-package which-key
  :defer 5
  :config (which-key-mode 1))

(use-package deadgrep
  :commands (deadgrep)
  :bind (("C-z" . deadgrep)))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . (lambda () (setq tab-width 4)))
  :config
  (require 'go-eldoc)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (require 'company-go)
  (add-hook 'go-mode-hook (lambda () (set (make-local-variable
                                      'company-backends)
                                     '(company-go))))
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

(use-package ess
  :init (require 'ess-site)
  :config
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

(use-package dna-mode
  :mode "\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'")

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

(use-package jinja2-mode
  :mode ("\\.j2\\'"))

(use-package css-mode
  :mode ("\\.css\\'"
         "\\.mss\\'")
  :bind (:map css-mode-map
              ("RET" . gk-electrify-return-if-match)))

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default js2-basic-offset 2)
  :config
  (require 'smartparens-javascript)
  :bind (:map js2-mode-map
              ("RET" . gk-electrify-return-if-match)
              ("M-." . xref-find-definitions)))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx\\'"
  :init
  (setq-default js2-basic-offset 2)
  :bind (:map rjsx-mode-map
              ("RET" . gk-electrify-return-if-match)))

(use-package add-node-modules-path
  :hook (js2-mode typescript-mode rjsx-mode))

(use-package ebuild-mode
  :mode ("\\.ebuild\\'"
         "\\.eclass\\'"))

(use-package ledger-mode
  :mode ("\\.ledger\\'")
  :bind (:map ledger-mode-map
              ("C-c C-c" . ledger-report)))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package copy-as-format
  :bind (("C-c w g" . copy-as-format-github)
         ("C-c w h" . copy-as-format-hipchat)
         ("C-c w j" . copy-as-format-jira)
         ("C-c w m" . copy-as-format-markdown)
         ("C-c w o" . copy-as-format-org-mode)
         ("C-c w s" . copy-as-format-slack)))

(use-package gk-other-window-repeat
  :bind (("C-x o" . gk-other-window-repeat)))

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

 ;; enable auto revert globally
 (global-auto-revert-mode 1)
 (setq auto-revert-check-vc-info t)
 (setq auto-revert-verbose nil)
 (setq auto-revert-remote-files t)

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

 ;; keep backup files neatly out of the way in .~/
 (setq backup-directory-alist '(("." . ".~")))

 ;; use character folding in search (a matches á etc)
 (setq search-default-mode 'char-fold-to-regexp)

 ;; rat yank doesn't move cursor
 (setq mouse-yank-at-point t))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "WORKON_HOME")
  (exec-path-from-shell-copy-env "PROJECT_HOME"))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t)
  ;; restore after startup
  (setq file-name-handler-alist file-name-handler-alist-old)
  (setq gc-cons-threshold (* 40 1024 1024)))

(when load-file-name
  (find-file (concat (file-name-sans-extension load-file-name)
                     ".el")))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
