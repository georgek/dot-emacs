;;;; my .emacs file

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
(scroll-bar-mode -1)
(blink-cursor-mode 1)
(tool-bar-mode -1)
(mouse-avoidance-mode 'banish)

;; remove old org from load path
(require 'cl)
;; (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))

;; add paths
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(progn (cd "~/.emacs.d/site-lisp/")
       (normal-top-level-add-subdirs-to-load-path))
;; this adds stuff that I'm currently working on
(load "working.el" t)
(load "gk-utils.el")

;; org development version
;; (add-to-list 'load-path "~/src/org-mode/lisp/")

;; load stuff in other files
;; (load-library "gk-gtags")

;; (require 'color-theme-zenburn)
;; (color-theme-zenburn)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)
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

;; files ignored when saving all buffers
(defvar save-ignored-files
  '(".newsrc-dribble"))
(defun save-all-considered ()
  (not (member (buffer-name) save-ignored-files)))
(global-set-key (kbd "C-x s") (lambda () (interactive)
                                (save-some-buffers nil 'save-all-considered)))

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
    (add-to-list 'Info-directory-list "~/.emacs.d/info/ansicl/")
  (add-to-list 'Info-default-directory-list "~/.emacs.d/info/ansicl/"))

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

;;; ace jump mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(ace-jump-mode-enable-mark-sync)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

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
(defun elisp-init ()
  (nice-paredit-on)
  (local-set-key (kbd "C-c C-c") #'eval-defun)
  (local-set-key (kbd "C-c C-z") #'ielm-switch-to-buffer)
  (local-set-key (kbd "C-c e") #'macrostep-expand))
(add-hook 'emacs-lisp-mode-hook #'elisp-init)

;;; *** ielm ***
(defun ielm-switch-to-buffer ()
  (interactive)
  (let ((ielm-buffer (get-buffer "*ielm*")))
    (if ielm-buffer
        (pop-to-buffer ielm-buffer)
      (ielm))))

(defun ielm-init ()
  (nice-paredit-on)
  (local-set-key (kbd "C-<return>")
                 'ielm-send-input))

(add-hook 'ielm-mode-hook #'ielm-init)

;;; *** scheme ***
(add-hook 'scheme-mode-hook 'nice-paredit-on)

;;; *** SLIME ***
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime-autoloads)
(slime-setup '(slime-fancy slime-banner slime-asdf))

(defun lisp-init ()
  (nice-paredit-on)
  (local-set-key (kbd "C-c z") 'slime-switch-to-output-buffer))

(add-hook 'lisp-mode-hook #'lisp-init)

;;; REPL
(defun slime-repl-init ()
  (paredit-mode +1))

(add-hook 'slime-repl-mode-hook #'slime-repl-init)
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;;; Lisp pretty things
;;; dim parens
(require 'parenface)

;;; *** org-mode settings ***
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory "~/org/")
(defmacro orgdr (&optional filename)
  (if filename
   `(concat org-directory ,filename)
   org-directory))
(setq org-return-follows-link t)

;; capture
(setq org-default-notes-file (orgdr "notes.org"))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (orgdr "todo.org") "Misc (Captured)")
         "* TODO %?\n %U\n %a")
        ("j" "Journal" entry (file+datetree (orgdr "journal.org"))
         "* %?\nEntered on %U\n %i")
        ("i" "Idea" entry (file (orgdr "ideas.org"))
         "* %?\n %U\n %a")))

;; agenda stuff
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-~") 'org-agenda-list)
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
(setq org-todo-keywords (quote ((sequence "TODO" "DONE"))))
;; make org table mode come on for some modes
(add-hook 'LaTeX-mode-hook 'turn-on-orgtbl)

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

;; (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
;;                                                   (list ndays todayp))
;;   (if (member 'remove-match (car org-agenda-time-grid))
;;       (cl-flet ((extract-window
;;               (line)
;;               (let ((start (get-text-property 1 'time-of-day line))
;;                     (dur (get-text-property 1 'duration line)))
;;                 (cond
;;                  ((and start dur)
;;                   (cons start
;;                         (org-time-from-minutes
;;                          (+ dur (org-time-to-minutes start)))))
;;                  (start start)
;;                  (t nil)))))
;;         (let* ((windows (delq nil (mapcar 'extract-window list)))
;;                (org-agenda-time-grid
;;                 (list (car org-agenda-time-grid)
;;                       (cadr org-agenda-time-grid)
;;                       (remove-if
;;                        (lambda (time)
;;                          (find-if (lambda (w)
;;                                     (if (numberp w)
;;                                         (equal w time)
;;                                       (and (>= time (car w))
;;                                            (< time (cdr w)))))
;;                                   windows))
;;                        (caddr org-agenda-time-grid)))))
;;           ad-do-it))
;;     ad-do-it))
;; (ad-activate 'org-agenda-add-time-grid-maybe)

;; *** end org-mode stuff ***

;; makefile mode make key
(defun makefile-init ()
  (define-key makefile-mode-map "\C-c\C-c" 'compile))

(add-hook 'makefile-mode-hook #'makefile-init)

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
               "*R*"
               "init.el"))

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
(require 'magit-blame)
(global-set-key (kbd "\C-ci") 'magit-status)
(global-set-key (kbd "\C-cb") 'magit-blame-mode)

;; key for opening a shell
(global-set-key (kbd "\C-cs") 'eshell)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/site-lisp/yasnippet/snippets/")
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

;; cc mode key bindings - applies to all CC modes (C, C++ etc.)
(defun c-init ()
  (define-key c-mode-base-map "\C-c\C-c" 'compile)
  (define-key c-mode-base-map "\C-c\C-h" 'c-c++-toggle)
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (abbrev-mode -1))
(add-hook 'c-initialization-hook 'c-init)

;; use tags with C modes
;; (add-hook 'c-mode-common-hook
;;   (lambda ()
;;     (gtags-mode t)))

;; stuff for debugging with gdb
;; sr-speedbar runs speedbar in the same frame
(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

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
(add-hook 'asm-mode-hook
          (function (lambda ()
                      (define-key asm-mode-map "\C-c\C-c" 'compile))))
(add-hook 'gas-mode-hook
          (function (lambda ()
                      (define-key gas-mode-map "\C-c\C-c" 'compile))))

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

;; modified ledger-accounts puts names in list rather than tree
(defun find-all-ledger-accounts ()
  (let ((origin (point)) accounts)
    (save-excursion
      (setq ledger-account-tree (list t))
      (goto-char (point-min))
      (while (re-search-forward
             "^[ \t]+\\([*!]\\s-+\\)?[[(]?\\([^ ;]+?\\)\\(\t\\|\n\\| [ \t]\\)"
             nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq accounts (cons (match-string-no-properties 2) accounts)))))
    accounts))

;; account specifier with completion
(defun report-account-format-specifier ()
  (let ((accounts (find-all-ledger-accounts)))
    (completing-read "Account: " accounts)))

(defun report-payee-format-specifier ()
  (read-from-minibuffer "Payee: "))

;; interactive add
(defun ledger-add-entry (date title in out)
  (interactive
   (let (date title (in nil) (out nil) numin numout count curr
              (accounts (find-all-ledger-accounts)) last-date)
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
  (newline)
  (ledger-align-amounts))

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
   (pcomplete interactively))
  
  )

(defun insert-bitcoin-symbol ()
  (interactive)
  (insert ?\฿))

(defun ledger-init ()
  (flyspell-mode -1)
  (yas-minor-mode -1)
  (local-set-key (kbd "C-c C-c") 'ledger-report)
  (local-set-key (kbd "C-c C-q") 'ledger-toggle-current)
  ;; (local-set-key (kbd "<tab>") 'ledger-indent-and-pcomplete)
  (local-set-key (kbd "C-c C-a") 'ledger-add-entry))

(add-hook 'ledger-mode-hook #'ledger-init)

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

(defun go-mode-keys ()
  (local-set-key (kbd "C-c C-c") #'go-mode-compile)
  (local-set-key (kbd "RET") #'go-mode-electric-return)
  (local-set-key (kbd "{") #'go-mode-electric-brace))

(add-hook 'go-mode-hook #'go-mode-keys)

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
        (call-process "~/.emacs.d/oimaptime" nil buf-name nil)
        (start-process-shell-command "offlineimap" buf-name
                                     "~/.emacs.d/oimaptime"))))
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
(load "mail.el" t)


;;; diminish modeline
(require 'diminish)
(load "gk-diminish.el" t)
;; *** visit this file at startup for convenience ***
(find-file load-file-name)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
