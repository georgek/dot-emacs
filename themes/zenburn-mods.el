;;; my modifications to zenburn
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(font-lock-builtin-face ((t (:foreground ,zenburn-cyan))))
   `(paren-face ((t (:foreground "#989888"))))
   ;; ledger
   `(ledger-font-payee-uncleared-face
     ((t (:foreground ,zenburn-red :weight bold))))
   `(ledger-font-payee-cleared-face
     ((t (:foreground ,zenburn-blue-2 :weight normal))))
   `(ledger-font-xact-highlight-face
     ((t (:background ,zenburn-bg+1))))
   `(ledger-font-pending-face
     ((t (:foreground ,zenburn-red :weight normal))))
   `(ledger-font-other-face
     ((t (:foreground ,zenburn-yellow))))
   `(ledger-font-posting-account-face
     ((t (:foreground ,zenburn-orange))))
   `(ledger-font-posting-account-cleared-face
     ((t (:foreground ,zenburn-yellow))))
   `(ledger-font-posting-account-pending-face
     ((t (:foreground ,zenburn-red))))
   `(ledger-font-posting-amount-face
     ((t (:foreground ,zenburn-green+2))))
   `(ledger-font-posting-negative-amount-face
     ((t (:foreground ,zenburn-red+1))))
   `(ledger-occur-narrowed-face
     ((t (:foreground ,zenburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face
     ((t (:background ,zenburn-bg+1))))
   `(ledger-font-comment-face
     ((t (:foreground ,zenburn-green-1 :slant italic))))
   `(ledger-font-reconciler-uncleared-face
     ((t (:foreground ,zenburn-red :weight bold))))
   `(ledger-font-reconciler-cleared-face
     ((t (:foreground ,zenburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face
     ((t (:foreground ,zenburn-red :weight normal))))
   `(ledger-font-report-clickable-face
     ((t (:foreground ,zenburn-red :weight normal))))
   ;; macrostep
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   ;; eldoc
   `(eldoc-highlight-function-argument ((t (:weight bold))))))
