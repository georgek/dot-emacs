;;;; my .emacs file

;;; init path stuff
(or (boundp 'init-path)
    (setq init-path (file-name-directory load-file-name)))
(defmacro load-init (filename &optional noerror)
  `(load (expand-file-name ,filename init-path) ,noerror))
(defmacro add-to-path-init (path directory)
  `(add-to-list ',path (expand-file-name ,directory init-path)))
(defun add-subdirs-to-load-path (directory init-path)
  (let* ((default-directory (expand-file-name directory init-path))
         (files (directory-files default-directory)))
    (dolist (file files)
      (when (and (file-directory-p file)
                 (string-match "\\`[[:alnum:]]" file))
        (add-to-list 'load-path (expand-file-name file default-directory))))))

(setq inhibit-splash-screen t)
(setq inhibit-default-init t)
;; ignore case in completion
(setq completion-ignore-case t)
(setq pcomplete-ignore-case t)

(global-font-lock-mode 1)

;; set window title, turn toolbars and stuff off
(setq frame-title-format '("%b - GNU Emacs"))
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(mouse-avoidance-mode 'banish)
(if (or (display-graphic-p)
        (daemonp))
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(global-unset-key (kbd "C-z"))
;; disable this as it ruins keyboard macros
(setq line-move-visual nil)
;; prefer to split windows vertically even on tall monitor
(setq split-height-threshold 160)
(setq split-width-threshold 160)

(require 'cl-lib)

;; remove old org from load path
(setq load-path (cl-remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; add paths
;; (add-to-path-init load-path ".")
(add-to-path-init load-path "site-lisp")
(add-subdirs-to-load-path "site-lisp" init-path)
(add-to-path-init load-path "lisp")
;; this adds stuff that I'm currently working on
(load-init "working" t)
(load-init "gk-utils")

;; org development version
;; (add-to-list 'load-path "~/src/org-mode/lisp/")

;; load stuff in other files
;; (load-library "gk-gtags")

;;; Zenburn
(add-to-path-init custom-theme-load-path "themes")
(load-theme 'zenburn t)
(load-init "themes/zenburn-mods")

;;; desktop save
;; (defun save-desktop ()
;;   (desktop-save "~")
;;   (message "Desktop saved."))
;; (setq desktop-timer
;;  (run-with-timer 0 (* 10 60) #'save-desktop))

;; global keys
(global-set-key (kbd "C-M-/") 'indent-region)

;; ido mode
(require 'ido)
(require 'ido-hacks)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
(ido-hacks-mode t)
;; disable annoying buffer list
(global-set-key (kbd "\C-x\C-b") 'ido-switch-buffer)
;; some buffers to ignore
(setq ido-ignore-buffers '("\\` " "*Group*" "*Article*" "*Messages*"
                           "\\`*magit" "*Completions*" "*Help*"
                           ".newsrc-dribble" "\\`*trace"))

;; deletes the current frame, unless this is the last frame in which case it
;; kills emacs
(defun kill-client-or-daemon ()
  "Kills emacs.  If running as daemon, the daemon is killed when
  the last frame is killed."
  (interactive)
  (if (and (boundp 'server-clients)
           (> (length server-clients) 0))
      ;; daemon
      (if (<= (length (frame-list)) 2)
          ;; this is the last frame
          (progn
            (save-some-buffers)
            (delete-frame)
            (kill-emacs))
        ;; not the last frame so just delete it
        (delete-frame))
      ;; not daemon
    (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'kill-client-or-daemon)

;; turn off tabs
(setq-default indent-tabs-mode nil)

;; set some default styles
(setq c-default-style '((java-mode . "java") (awk-mode
      . "awk") (c-mode . "k&r") (c++-mode . "stroustrup") (other
      . "gnu")))

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
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

;;; info
(require 'info)
(require 'info-look)

(if Info-directory-list
    (add-to-path-init Info-directory-list "info/ansicl")
  (add-to-path-init Info-default-directory-list "info/ansicl"))

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(info-lookup-add-help
 :mode 'slime-repl-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;; avy
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)

;;; ace window mode
(require 'ace-window)
(global-set-key (kbd "M-'") 'ace-window)

;;; *** paredit ***

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

(require 'paredit)

;; use with eldoc
(require 'eldoc) ; if not already loaded
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

  (show-paren-mode t))

;;; *** emacs lisp ***
(require 'elisp-slime-nav)
(require 'macrostep)

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
    (lisp-complete-symbol)))

(makehookedfun emacs-lisp-mode-hook
  (nice-paredit-on)
  ;; (smartparens-strict-mode)
  (yas/minor-mode -1)
  (local-set-key (kbd "TAB") #'elisp-magic-tab)
  (local-set-key (kbd "C-c C-k") #'eval-buffer-key)
  (local-set-key (kbd "C-c C-c") #'eval-defun-key)
  (local-set-key (kbd "C-c C-z") #'ielm-switch-to-buffer)
  (local-set-key (kbd "C-c z") #'ielm-switch-to-buffer)
  (local-set-key (kbd "C-c C-l") #'paredit-recentre-on-sexp)
  (local-set-key (kbd "C-c e") #'macrostep-expand)
  (local-set-key (kbd "C-c d") #'toggle-debug-on-error))

;;; *** ielm ***
(defun ielm-switch-to-buffer ()
  (interactive)
  (let ((ielm-buffer (get-buffer "*ielm*")))
    (if ielm-buffer
        (pop-to-buffer ielm-buffer)
      (ielm))))

(defalias 'p #'princ)

(makehookedfun ielm-mode-hook
  (nice-paredit-on)
  ;; (smartparens-strict-mode)
  (local-set-key (kbd "C-<return>")
                 'ielm-send-input))

;;; *** scheme ***
(makehookedfun scheme-mode-hook
  (nice-paredit-on)
  ;; (smartparens-strict-mode)
  )

;;; *** SLIME ***
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner slime-asdf))

(setf slime-complete-symbol-function #'slime-simple-complete-symbol)
(defun slime-magic-tab ()
  (interactive)
  (if (member (char-before) '(?\s ?\t ?\n))
      (indent-for-tab-command)
    (slime-complete-symbol)))

(makehookedfun lisp-mode-hook
  (nice-paredit-on)
  ;; (smartparens-strict-mode)
  (yas/minor-mode -1)
  (local-set-key (kbd "TAB") #'slime-magic-tab)
  (local-set-key (kbd "C-c z") #'slime-switch-to-output-buffer)
  (local-set-key (kbd "C-c e") #'slime-macroexpand-1))

(makehookedfun slime-load-hook
  (define-key slime-macroexpansion-minor-mode-map
   (kbd "e") #'slime-expand-1-inplace)
 (define-key slime-macroexpansion-minor-mode-map
   (kbd "n") #'next-line)
 (define-key slime-macroexpansion-minor-mode-map
   (kbd "p") #'previous-line))

;;; REPL
(makehookedfun slime-repl-mode-hook
  (nice-paredit-on)
  ;; (smartparens-strict-mode)
  ;; Stop SLIME's REPL from grabbing DEL,
  ;; which is annoying when backspacing over a '('
  ;; (define-key slime-repl-mode-map
  ;;   (read-kbd-macro paredit-backward-delete-key) nil)
  
  )

;;; Lisp pretty things
;;; dim parens
(require 'parenface)

;; CIDER
(add-to-path-init load-path "site-lisp/clojure-mode")
(add-to-path-init load-path "site-lisp/cider")
(require 'cider)
(require 'cider-macroexpansion)
(makehookedfun clojure-mode-hook
  (nice-paredit-on)
  (local-set-key (kbd "C-c z") #'cider-switch-to-repl-buffer))
(makehookedfun cider-repl-mode-hook
  (paredit-mode))
(setq cider-repl-display-help-banner nil)

;;; *** org-mode settings ***
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(makehookedfun org-mode-hook
  (auto-fill-mode)
  (org-bullets-mode 1)
  (flyspell-mode 1)
  (local-set-key (kbd "M-p") #'org-metaup)
  (local-set-key (kbd "M-n") #'org-metadown))

(setq org-directory "~/org/")
(defmacro orgdr (&optional filename)
  (if filename
   `(concat org-directory ,filename)
   org-directory))
(setq org-return-follows-link t)

(require 'org-bullets)

;; src
(setq org-src-fontify-natively t)
;; add lisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (sqlite . t)
   (R . t)
   (shell . t)
   (python . t)))

;;; export
(setq org-export-allow-bind-keywords t)
(require 'htmlize)
;;; org-reveal
(require 'ox-reveal)
(setq org-reveal-root ".")
(setq org-reveal-init-script "zoomKey: 'shift'")

;;; twbs
(require 'ox-twbs)
;;; latex
(require 'ox-latex)
(setq org-latex-to-pdf-process '("latexmk -pdf %f")) ;use latexmk to do pdfs
(setq org-export-latex-listings 'minted)
;(add-to-list 'org-export-latex-default-packages-alist '("" "minted"))

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

;;; export with CSS classes instead of explicit colours
(setq org-html-htmlize-output-type 'css)
(setq org-html-htmlize-font-prefix "org-")

(setq org-twbs-htmlize-output-type 'css)
(setq org-twbs-htmlize-font-prefix "org-")

;; capture
(setq org-default-notes-file (orgdr "notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (orgdr "todo.org") "Misc (Captured)")
         "* TODO %?\n %U\n %a")
        ("d" "Diary" entry (file+headline (orgdr "diary.org") "Captured"))
        ("j" "Journal" entry (file+datetree (orgdr "journal.org"))
         "* %? %^g\nEntered on %U\n %i")
        ("i" "Idea" entry (file (orgdr "ideas.org"))
         "* %?\n %U\n %a")))

;; agenda stuff
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-~") (lambda () (interactive) (org-agenda nil "n")))
(setq org-agenda-files (list (orgdr) (orgdr "personal/")))
(setq org-agenda-include-diary t)
(setq org-agenda-span 14)
;; (setq org-agenda-ndays 14)              ;old version of span
(setq org-agenda-time-grid '((daily today remove-match)
                             ""
                             (0800 1000 1200 1400 1600 1800 2000)))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)

(setq org-time-stamp-custom-formats
      '("<%A, %e %B %Y>" . "<%A, %e %B %Y %H:%M>"))
(setq org-log-done 'time)
(setq org-blank-before-new-entry 
      '((heading . t) (plain-list-item . nil)))
(setq org-todo-keywords (quote((sequence "TODO" "WAITING" "|" "DONE"))))
;; make org table mode come on for some modes

;; (add-to-list 'org-modules 'org-timer)
;; (add-to-list 'org-modules 'org-clock)
(require 'org-clock)
(require 'org-timer)
(setq org-timer-default-timer 25)
(add-hook 'org-clock-in-hook '(lambda () 
                                (if (not org-timer-current-timer) 
                                    (org-timer-set-timer '(16)))))
(add-hook 'org-clock-cancel-hook 'org-timer-cancel-timer)

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

;;; setup default file readers
(eval-after-load "org"
  '(setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s"))

;; *** end org-mode stuff ***

;; makefile mode make key
(makehookedfun makefile-mode-hook
  (define-key makefile-mode-map (kbd "C-c C-c") 'compile))

;; auto new line and hungry delete modes
(setq c-auto-newline 1)
(setq c-hungry-delete-key 1)

;; turn on flyspell for some modes
(dolist (hook '(text-mode-hook))
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
(setq uniquify-min-dir-content 0)

;; binds hippie-expand to M-/
(global-set-key (kbd "M-/") 'hippie-expand)

;; midnight mode kills unused buffers at midnight
(require 'midnight)
(setq midnight-mode t)
;; some buffers that shouldn't be killed
(setq clean-buffer-list-kill-never-buffer-names
      (append clean-buffer-list-kill-never-buffer-names
              '("*slime-repl sbcl*"
                "*R*"
                "init.el")))

;; AUCTeX
(load "auctex.el" t t t)
(load "preview-latex.el" t t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)
;; (add-to-list 'LaTeX-verbatim-environments "Verbatim")
;; (add-to-list 'LaTeX-verbatim-environments "lstlisting")
;; RefTeX
(require 'reftex)
(setq reftex-plug-into-AUCTeX t)        ;AUCTeX-RefTeX interface
(setq reftex-default-bibliography '("bibliography"))

(makehookedfun LaTeX-mode-hook
  (TeX-PDF-mode)
  (auto-fill-mode)
  (turn-on-orgtbl)
  (turn-on-reftex))
(makehookedfun TeX-mode-hook
  (TeX-PDF-mode)
  (auto-fill-mode)
  (setq tab-stop-list (number-sequence 3 45 3)))

;; aligns the current block of code
(global-set-key (kbd "C-|") 'align-current)

;; enable Winner Mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;;; git stuff
;; (require 'gitconfig-mode)
;; (require 'gitignore-mode)
;; (require 'gitattributes-mode)

;; magit for using git
(require 'magit)
(global-set-key (kbd "C-c i") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame)

;; smerge-mode
(setq smerge-command-prefix (kbd "C-c v"))

;; key for opening a shell
(global-set-key (kbd "C-c s") 'eshell)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      (expand-file-name "site-lisp/yasnippet/snippets" init-path))
(yas-global-mode 1)
(setq yas-prompt-functions
      '(yas-dropdown-prompt yas-completing-prompt yas-ido-prompt))

;; keep backup files neatly out of the way in .~/
(setq backup-directory-alist '(("." . ".~")))

;; scroll compilation buffer
(setq compilation-scroll-output 'first-error)

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

(defun my-c-electric-brace (arg)
  (interactive "P")
  ;; (delete-horizontal-space t)
  ;; (insert " ")
  (c-electric-brace arg))

;; cc mode key bindings - applies to all CC modes (C, C++ etc.)
(makehookedfun c-initialization-hook
  (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
  (define-key c-mode-base-map (kbd "C-c C-h") 'c-c++-toggle)
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
  (define-key c-mode-base-map (kbd "{") 'my-c-electric-brace)
  (abbrev-mode -1))

;; use tags with C modes
;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (gtags-mode t)))

;; stuff for debugging with gdb
;; sr-speedbar runs speedbar in the same frame
;; (require 'sr-speedbar)
;; (global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; enable windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(setq windmove-wrap-around t)

;; assembly
(require 'gas-mode)
(setq gas-comment-char ?\@)
(add-to-list 'auto-mode-alist '("\\.s\\'" . asm-mode))
(add-hook 'asm-mode-set-comment-hook
          (lambda () (setq asm-comment-char ?\#)))
(makehookedfun asm-mode-hook
  (define-key asm-mode-map (kbd "C-c C-c") 'compile))
(makehookedfun gas-mode-hook
  (define-key gas-mode-map (kbd "C-c C-c") 'compile))

;; prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.m$" . mercury-mode))
                              auto-mode-alist))

;;; perl
(setq auto-mode-alist (append '(("\\.pl$" . perl-mode))
                              auto-mode-alist))

;; for inserting greeked text
(require 'lorem-ipsum)

;;; *** ledger stuff ***
(require 'ledger)

;; modified ledger-accounts puts names in list rather than tree
(defun ledger-find-all-accounts ()
  "Returns list of all account names in file."
  (let ((origin (point))
        (accounts (list))
        (prefix ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ledger-account-any-status-regex
              nil t)
        (dolist (s (split-string (match-string-no-properties 2) ":" t))
          (add-to-list 'accounts (concat prefix s))
          (setq prefix (concat prefix s ":")))
        (setq prefix "")))
    (cons " " accounts)))

;; account specifier with completion
(defun report-account-format-specifier ()
  (let ((accounts (ledger-find-all-accounts)))
    (completing-read "Account: " accounts)))

(defun report-payee-format-specifier ()
  (read-from-minibuffer "Payee: "))

;; interactive add
(defun ledger-add-entry (date title in out)
  (interactive
   (let (date title (in nil) (out nil) numin numout count curr
              (accounts (ledger-find-all-accounts)) last-date)
     ;; get the last date in the buffer to be used as default for get date
     (save-excursion
       (if (re-search-backward
            "^\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" nil t)
           (setq last-date
                 (encode-time 0 0 0
                              (string-to-number
                               (match-string-no-properties 3))
                              (string-to-number
                               (match-string-no-properties 2))
                              (string-to-number
                               (match-string-no-properties 1))))))
     
     (setq date (org-read-date nil nil nil nil last-date))
     (setq title (read-string "Payee: "))
     (setq numin (string-to-number
                  (read-string 
                   "How many accounts is money going to? (1): "
                   nil nil "1")))
     ;; read in accounts
     (setq count 1)
     (while (<= count numin)
       (setq curr
             (cons
              (completing-read
               (concat "(" (number-to-string count) ") Which account? ")
               accounts)
              (read-string
               (concat "(" (number-to-string count) ") How much? ") "£")))
       (setq in (cons curr in))
       (setq count (1+ count)))
     (setq in (nreverse in))

     (setq numout (string-to-number
                   (read-string 
                    "How many accounts is money coming from? (1): "
                    nil nil "1")))
     ;; read out accounts
     (if (> numout 1)
         (progn
           (setq count 1)
           (while (<= count numout)
             (setq curr
                   (cons
                    (completing-read
                     (concat "(" (number-to-string count) ") Which account? ")
                     accounts)
                    (if (= numout count)
                        nil
                      (read-string
                       (concat
                        "(" (number-to-string count) ") How much? ") "£"))))
             (setq out (cons curr out))
             (setq count (1+ count)))
           (setq out (nreverse out)))
       (setq out (completing-read
                  "Where did the money come from? " accounts)))
     (list date title in out)))
  (insert date " " title)
  (newline)
  ;; print ins
  (while in
    (indent-to 4)
    (insert (caar in) "  " (cdar in))
    (newline)
    (setq in (cdr in)))
  ;; print outs
  (if (stringp out)
      (progn
        (indent-to 4)
        (insert out)
        (newline))
    (while out
      (indent-to 4)
      (if (null (cdar out))
          (insert (caar out))
        (insert (caar out) "  -" (cdar out))) ; negative inserted
      (newline)
      (setq out (cdr out))))
  (newline))

(defun ledger-add-loads (in out year month)
  (interactive
   (let (in out year month (accounts (ledger-find-all-accounts)))
     (setq in (completing-read "Account to: " accounts))
     (setq out (completing-read "Account from: " accounts))
     (setq year (string-to-number (read-from-minibuffer "Year: ")))
     (setq month (string-to-number (read-from-minibuffer "Month: ")))
     (list in out year month)))
  (let ((default-date
         (encode-time 0 0 0 1 month year))
        date title amount)
    (while t
      (setq day (string-to-number (read-from-minibuffer "Day: ")))
      (setq title (read-string "Payee: "))
      (setq amount (read-string "Amount: " "£"))
      (ledger-add-entry (format "%04d-%02d-%02d" year month day)
                        title
                        (list (cons in amount)) (list (cons out nil))))))

(defun ledger-indent-and-pcomplete (&optional interactively)
  (interactive "P")
  (save-excursion
    (back-to-indentation)
    (when (> (current-column) 4)
      (kill-line 0))
    (indent-to 4))
  (when (< (current-column) 4)
    (back-to-indentation))
  (when (eq (point) (line-end-position))
   (pcomplete interactively)))

(defun insert-bitcoin-symbol ()
  (interactive)
  (insert ?\฿))

(makehookedfun ledger-mode-hook
  (flyspell-mode -1)
  (yas-minor-mode -1)
  (local-set-key (kbd "C-c C-c") 'ledger-report)
  (local-set-key (kbd "C-c C-q") 'ledger-toggle-current)
  ;; (local-set-key (kbd "<tab>") 'ledger-indent-and-pcomplete)
  (local-set-key (kbd "C-c C-a") 'ledger-add-entry))

;;; *** ERC ***
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)

(defvar prev-window (selected-window)
  "Holds the previous window which was selected before a switch
  using other-window-repeat.")

;; a better command to be bound to C-x o
(defun other-window-repeat (COUNT &optional no-repeat)
  "Calls other-window.  If a multiple key sequence was used to
  call this then the last key can be used on its own to repeat
  this, like kmacro-call-macro."
  (interactive "p")
  (let ((repeat-key (and (null no-repeat)
                         (> (length (this-single-command-keys)) 1)
                         last-input-event))
        repeat-key-str
        (nxt t))
    ;; save current window
    (setq prev-window (selected-window))
    (other-window COUNT)
    (when repeat-key
      (setq repeat-key-str (format-kbd-macro (vector repeat-key) nil)))
    (while repeat-key
      (unless (current-message)
        (message "(Type %s to keep cycling)"
                 repeat-key-str))
      (if (equal repeat-key (read-event))
          (progn
            (clear-this-command-keys t)
            (other-window COUNT)
            ;; if we cycle all the way to the same window, set prev-window to
            ;; next window, then if we continue to cycle set it back again
            (when (eq prev-window (selected-window))
              (setq prev-window (if nxt
                                    (next-window)
                                  (previous-window)))
              (setq nxt (not nxt)))
            (setq last-input-event nil))
        (setq repeat-key nil)))
    (when last-input-event
      (clear-this-command-keys t)
      (setq unread-command-events (list last-input-event)))))
(global-set-key (kbd "C-x o") 'other-window-repeat)

(defun switch-prev-window ()
  "Switchs back to previous window that was selected before last
call to other-window-repeat or switch-prev-window."
  (interactive)
  (when (or (eq prev-window (selected-window))
            (not (window-live-p prev-window)))
    ;; previous window is best guess
    (setq prev-window (previous-window)))
  (let ((wind prev-window))
    (setq prev-window (selected-window))
    (select-window wind)))
(global-set-key (kbd "M-'") 'switch-prev-window)
(global-set-key (kbd "C-M-'") 'other-frame)

;; ido imenu from emacswiki
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "M-i") 'ido-goto-symbol)

;;; Go
(require 'go-mode-load)

(setq go-mode-gopath "~/code/go")
(defun go-mode-electric-return (&optional arg)
  (interactive "P")
  (newline arg)
  (go-mode-indent-line))
(defun go-mode-electric-brace (&optional arg)
  (interactive "P")
  (insert-char ?{ arg)
  (go-mode-electric-return))
(defun go-mode-compile ()
  (interactive)
  (let ((str (concat "export GOPATH=" go-mode-gopath "; go install")))
    (compile str)))

(makehookedfun go-mode-hook
  (local-set-key (kbd "C-c C-c") #'go-mode-compile)
  (local-set-key (kbd "RET") #'go-mode-electric-return)
  (local-set-key (kbd "{") #'go-mode-electric-brace))

;;; ESS (R)
(add-to-path-init load-path "site-lisp/ess/lisp")
(require 'ess-site)
(defun ess-eval-defun-key ()
  (interactive)
  (ess-eval-function-or-paragraph t))
(makehookedfun ess-mode-hook
  (local-set-key (kbd "C-c z") #'ess-switch-to-inferior-or-script-buffer)
  (local-set-key (kbd "C-c C-c") #'ess-eval-defun-key)
  (local-set-key (kbd "C-c C-k") #'ess-eval-buffer)
  (local-set-key (kbd "C-c C-b") #'ess-force-buffer-current))
;;; iESS
(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))
(makehookedfun inferior-ess-mode-hook
  (local-set-key (kbd "C-c M-o") #'clear-shell))

;;; DNA
(require 'dna-mode)
(add-to-list
 'auto-mode-alist
 '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))

;;; Python
(require 'python)
(setq
  python-shell-interpreter "ipython3"
  python-shell-interpreter-args ""
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
  "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
  "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(defun python-eval-defun-key (arg)
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
             (python-shell-send-defun arg)
             (sit-for 0.1))
         (delete-overlay ol))))

(makehookedfun python-mode-hook
  (local-set-key (kbd "C-c C-c") #'python-eval-defun-key)
  (local-set-key (kbd "C-c C-k") #'python-shell-send-buffer)
  (local-set-key (kbd "C-c C-z") #'python-shell-switch-to-shell)
  (local-set-key (kbd "C-c z") #'python-shell-switch-to-shell))

;;; mail stuff

;;; ~/.gnus.el tells gnus how to get messages
;; (setq imap-shell-program "~/libexec/dovecot/imap -u user")
;; (setq gnus-select-method
;;       '(nnimap "Mail"
;;                (nnimap-stream shell)))

;; (setq user-mail-address "addr")

;;; .mail.el tells emacs how to send messages
;; (setq user-mail-address "addr"
;;       user-full-name "name")
;; (setq smtpmail-smtp-server "smtp.")

;; message mode
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'message-smtpmail-send-it)
;; report problems with the smtp server
(setq smtpmail-debug-info t)

;; gnus alias
(require 'gnus-alias)
(gnus-alias-init)

(add-hook 'message-load-hook (lambda () (gnus-alias-init)))
(define-key message-mode-map (kbd "C-c C-p") 'gnus-alias-select-identity)

;; message-x for auto-completion (using bbdb) in message-mode
(require 'message-x)

;; gnus
;; all mails should be always displayed in the mailbox
(setq gnus-permanently-visible-groups ".*INBOX")

;; default Pine ordered header list when displaying mail
(setq gnus-sorted-header-list
      '("^Date:" "^From:" "^To:" "^Followup-To:"
        "^Cc:" "Bcc:" "^Newsgroups:" "Fcc:" "^Subject:"))

(setq gnus-visual t)
(setq gnus-use-full-window nil)

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '((not gnus-thread-sort-by-most-recent-date))
 gnus-sum-thread-tree-false-root ""
 gnus-sum-thread-tree-indent " "
 gnus-sum-thread-tree-leaf-with-other "├► "
 gnus-sum-thread-tree-root ""
 gnus-sum-thread-tree-single-leaf "╰► "
 gnus-sum-thread-tree-vertical "│")

(setq message-citation-line-function 'message-insert-formatted-citation-line)

(global-set-key (kbd "C-x g") 'gnus)

(defvar offlineimap-buffer "*offlineimap*")

;; runs the oimaptime script which runs offlineimap and prints the sync times
;; with arg it does it synchronously
(defun run-offlineimap (&optional synchronous)
  (interactive)
  (let ((buf-name offlineimap-buffer))
    (set-buffer (get-buffer-create buf-name))
    (erase-buffer)
    (if synchronous
        (call-process (expand-file-name "oimaptime" init-path) nil buf-name nil)
        (start-process-shell-command "offlineimap" buf-name
                                     (expand-file-name "oimaptime"
                                                       init-path)))))
(defvar offlineimap-timer nil)

;; run offlineimap when we start gnus, then do it on a timer while gnus is
;; running
(add-hook 'gnus-before-startup-hook
          (lambda ()
            (when offlineimap-timer
              (cancel-timer offlineimap-timer))
            (run-offlineimap)
            (switch-to-buffer-other-window offlineimap-buffer)
            (setq offlineimap-timer
                  (run-with-timer 600 600 'run-offlineimap))))
(add-hook 'gnus-after-exiting-gnus-hook
          (lambda ()
            (when offlineimap-timer
              (cancel-timer offlineimap-timer)
              (setq offlineimap-timer nil))
            (run-offlineimap)
            (switch-to-buffer-other-window offlineimap-buffer)))

;; insidious big brother database
(when (require 'bbdb nil t)
  (bbdb-initialize 'gnus)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (setq bbdb-use-pop-up nil)
  (setq bbdb-completion-display-record nil))

;; this file sets up the mail accounts
(load-init "mail" t)


;;; diminish modeline
(require 'diminish)
(load-init "gk-diminish" t)
;; *** visit this file at startup for convenience ***
(when load-file-name
  (find-file load-file-name))


;;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(makehookedfun web-mode-hook
  (local-set-key (kbd "C-c C-c") #'browse-url-of-file))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
