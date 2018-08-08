;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  ;; (package-initialize)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "locutus")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (setq frame-title-format '("%b - GNU Emacs - NEW CONFIG"))
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (blink-cursor-mode -1)
  (global-unset-key (kbd "C-z"))
  ;; disable as it ruins keyboard macros
  (setq line-move-visual nil)
  ;; prefer to split windows vertically even on tall monitor
  (setq split-height-threshold 160)
  (setq split-width-threshold 160)
  (setq mouse-wheel-progressive-speed nil))

(progn ;    `borg'
  (add-to-list 'load-path (expand-file-name "lib/borg" user-emacs-directory))
  (require  'borg)
  (borg-initialize))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t))

;; theme
(set-frame-font "DejaVu Sans Mono-9")
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

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
  (declare (indent 1))
  (let ((function (intern (concat (symbol-name hook) "-function"))))
    `(progn
       (defun ,function ()
         ,@body)
       (add-hook ',hook #',function))))

;;; Long tail

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

(use-package company
  :config
  (setq company-backends
        '((company-files
           company-keywords
           company-capf
           company-yasnippet)
          (company-abbrev company-dabbrev)))
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (global-company-mode))

(use-package paredit
  :config
  ;; electric return stuff
  (defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

  (defun electrify-return-if-match (&optional arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (delete-horizontal-space t)
      (newline arg)
      (indent-according-to-mode)))

  (defun paredit-with-electric-return ()
    (paredit-mode +1)
    (local-set-key (kbd "RET") 'electrify-return-if-match))

  ;; use with eldoc
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)

  (defun nice-paredit-on ()
    (paredit-mode t)

    (turn-on-eldoc-mode)
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)

    (local-set-key (kbd "RET") 'electrify-return-if-match)
    (eldoc-add-command 'electrify-return-if-match)

    (show-paren-mode t)))

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

  (defun elisp-magic-tab ()
    (interactive)
    (if (member (char-before) '(?\s ?\t ?\n))
        (indent-for-tab-command)
      (company-complete)))

  (makehookedfun emacs-lisp-mode-hook
    (outline-minor-mode)
    (reveal-mode)
    (nice-paredit-on)
    (add-to-list (make-local-variable 'company-backends)
                 'company-elisp))

  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))

  (makehookedfun lisp-interaction-mode-hook
    (indent-spaces-mode))

  :bind
  (:map emacs-lisp-mode-map
   ("TAB" . elisp-magic-tab)
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

(use-package scheme
  :config
  (makehookedfun scheme-mode-hook
    (nice-paredit-on)
    (geiser-mode)))

(use-package geiser
  :commands (geiser-mode)
  :load-path "lib/geiser/elisp"
  :init
  (setq geiser-default-implementation 'mit)
  ;; Get around geiser bug not associating repls with buffers.
  (setq geiser-active-implementations '(mit)))

(use-package ivy
  :demand
  :bind (("C-x C-b" . ivy-switch-buffer))
  :config
  (ivy-mode)
  (counsel-mode)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-count-format "(%d/%d) "))

(use-package magit
  :defer t
  :bind (("C-c i"   . magit-status)
         ("C-c M-i" . magit-dispatch-popup))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-~" . (lambda () (interactive) (org-agenda nil "n")))
   :map org-mode-map
   ("M-p" . org-metaup)
   ("M-n" . org-metadown)
   ("C-M->" . org-metaright)
   ("C-M-<" . org-metaleft))
  :hook ((org-mode . auto-fill-mode)
         (org-mode . org-bullets-mode)
         (org-mode . flyspell-mode))
  :config
  (use-package org-bullets)

  (defmacro orgdr (&optional filename)
    (if filename
        `(concat org-directory ,filename)
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
  (setq org-agenda-files (list (orgdr)))
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

(use-package make-mode
  :defer t
  :bind
  (:map makefile-mode-map
   ("C-c C-c" . compile)))

(use-package man
  :defer t
  :config (setq Man-width 80))

(use-package paren
  :config (show-paren-mode))

(use-package paren-face
  :config
  (global-paren-face-mode))

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
               (list (regexp-quote (system-name)) nil nil)))

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
 (setq-default fill-column 78)

 (defun unfill-paragraph ()
   "Unfill paragraph at or after point."
   (interactive "*")
   (let ((fill-column most-positive-fixnum))
     (fill-paragraph nil (region-active-p))))

 ;; enable auto revert globally
 (global-auto-revert-mode 1)

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
 (setq backup-directory-alist '(("." . ".~"))))

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
            t))

(progn ;     personalize
  (let ((file (expand-file-name (concat (user-real-login-name) ".el")
                                user-emacs-directory)))
    (when (file-exists-p file)
      (load file))))

(when load-file-name
  (find-file (concat (file-name-sans-extension load-file-name)
                     ".el")))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
