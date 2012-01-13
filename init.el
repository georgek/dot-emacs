;;;; my .emacs file

(setq inhibit-splash-screen t)
;; ignore case in completion
(setq completion-ignore-case t)

;; set window title, turn toolbars and stuff off
(setq frame-title-format '("%b - GNU Emacs"))
(tooltip-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(tool-bar-mode -1)
(mouse-avoidance-mode 'banish)

;; add paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(progn (cd "~/.emacs.d/site-lisp/")
       (normal-top-level-add-subdirs-to-load-path))

;; load stuff in other files
;; (load-library "gk-gtags")

(require 'color-theme-zenburn)
(color-theme-zenburn)

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

;; set some default modes
;; lex
(add-to-list 'auto-mode-alist '("\\.l\\'" . c-mode))
;; matlab
(add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode))
;; XML
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))

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

;;; *** emacs lisp ***
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)

            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)

            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)

            (show-paren-mode t)))

;;; *** SLIME ***
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner slime-asdf))
(add-hook 'lisp-mode-hook
          (lambda ()
            (paredit-mode t)

            (turn-on-eldoc-mode)
            (eldoc-add-command
             'paredit-backward-delete
             'paredit-close-round)

            (local-set-key (kbd "RET") 'electrify-return-if-match)
            (eldoc-add-command 'electrify-return-if-match)

            (show-paren-mode t)))

;;; *** org-mode settings ***
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-agenda-files (list "~/org/"))
(setq org-agenda-include-diary t)
(setq org-agenda-span 14)
;; (setq org-agenda-ndays 14)              ;old version of span
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
;; some buffers that shouldn't be killed
(add-to-list 'clean-buffer-list-kill-never-buffer-names
             '("*slime-repl sbcl*"
               "*R*"))

;; AUCTeX
(load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-newline-function 'newline-and-indent)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ; set PDF mode by default
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
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
(require 'magit-svn)
(require 'magit-topgit)
(global-set-key (kbd "\C-ci") 'magit-status)

;; key for opening a shell
(global-set-key (kbd "\C-cs") 'eshell)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
;; disable annoying buffer list
(global-set-key (kbd "\C-x\C-b") 'ido-switch-buffer)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/site-lisp/yasnippet/snippets/")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/dropdown-prompt))

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

;; cc mode key bindings - applies to all CC modes (C, C++ etc.)
(defun my-c-initialization-hook ()
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-c\C-h" 'c-c++-toggle)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)

;; use tags with C modes
(add-hook 'c-mode-common-hook
  (lambda ()
    (gtags-mode t)))

;; stuff for debugging with gdb
;; sr-speedbar runs speedbar in the same frame
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; enable windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(setq windmove-wrap-around t)

;; assembly
;; (require 'gas-mode)
(add-to-list 'auto-mode-alist '("\\.S\\'" . asm-mode))
(add-hook 'asm-mode-set-comment-hook
          (lambda () (setq asm-comment-char ?#)))
(add-hook 'asm-mode-hook
          (function (lambda ()
                      (define-key asm-mode-map "\C-c\C-c" 'compile))))

;; prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

;; for inserting greeked text
(require 'lorem-ipsum)

;;; *** ledger stuff ***
(require 'ledger)
(add-hook 'ledger-mode-hook
          (lambda () (flyspell-mode -1)))

;; modified ledger-accounts puts names in list rather than tree
(defun find-all-ledger-accounts ()
  (let ((origin (point)) accounts)
    (save-excursion
      (setq ledger-account-tree (list t))
      (goto-char (point-min))
      (while (re-search-forward
              "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\(.+?\\)\\(\t\\|\n\\| [ \t]\\)" nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq accounts (cons (match-string-no-properties 2) accounts)))))
    accounts))

;; account specifier with completion
(defun report-account-format-specifier ()
  (let ((accounts (find-all-ledger-accounts)))
    (completing-read "Account: " accounts)))

;; interactive add
(defun ledger-add-entry (title in out)
  (interactive
   (let (title (in nil) (out nil) numin numout count curr
               (accounts (find-all-ledger-accounts)))
     (setq title
           (read-string
            "Entry: "
            (format-time-string "%Y/%m/%d " (current-time))))

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
                       (concat "(" (number-to-string count) ") How much? ") "£"))))
             (setq out (cons curr out))
             (setq count (1+ count)))
           (setq out (nreverse out)))
       (setq out (completing-read "Where did the money come from? " accounts)))
     (list title in out)))
  (insert title)
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
  (newline)
  (ledger-align-amounts))

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
                         last-input-event)))
    ;; save current window
    (setq prev-window (selected-window))
    (other-window COUNT)
    (while repeat-key
      (if (equal repeat-key (read-event))
          (progn
            (clear-this-command-keys t)
            (other-window COUNT)
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
  (let ((wind prev-window))
    (setq prev-window (selected-window))
    (select-window wind)))
(global-set-key (kbd "M-'") 'switch-prev-window)
