;;;; stuff for using GNU Global
(require 'gtags)

;; http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))
      ;; tagfile doesn't exist?
      (let ((olddir default-directory)
            (topdir (read-directory-name  
                     "gtags: top of source tree:" default-directory)))
        (cd topdir)
        (message "Creating tags table...")
        (shell-command "gtags")
        (message "Creating tags table... Done")
        (cd olddir)) ;; restore   
    ;;  tagfile already exists; update it
    (message "Updating tags table...")
    (shell-command "global -u")
    (message "Updating tags table... Done")))

;; goto tag's point - modified version which returns t if a match was
;; found or nil otherwise
(defun gk-gtags-goto-tag (tagname flag)
  (let (option context save prefix buffer lines)
    (setq save (current-buffer))
    ;; Use always ctags-x format.
    (setq option "-x")
    (if (equal flag "C")
        (setq context
              (concat "--from-here="
                      (number-to-string (gtags-current-lineno))
                      ":"
                      buffer-file-name))
      (setq option (concat option flag)))
    (cond
     ((equal flag "C")
      (setq prefix "(CONTEXT)"))
     ((equal flag "P")
      (setq prefix "(P)"))
     ((equal flag "g")
      (setq prefix "(GREP)"))
     ((equal flag "I")
      (setq prefix "(IDUTILS)"))
     ((equal flag "s")
      (setq prefix "(S)"))
     ((equal flag "r")
      (setq prefix "(R)"))
     (t (setq prefix "(D)")))
    ;; load tag
    (setq buffer
          (generate-new-buffer
           (generate-new-buffer-name
            (concat "*GTAGS SELECT* " prefix tagname))))
    (set-buffer buffer)
    (cond
     ((equal gtags-path-style 'absolute)
      (setq option (concat option "a")))
     ((equal gtags-path-style 'root)
      (let (rootdir)
        (if gtags-rootdir
            (setq rootdir gtags-rootdir)
          (setq rootdir (gtags-get-rootpath)))
        (if rootdir (cd rootdir)))))
    (message "Searching %s ..." tagname)
    (if (not (= 0 (if (equal flag "C")
                      (call-process "global" nil t nil option context tagname)
                    (call-process "global" nil t nil option tagname))))
	(progn (message (buffer-substring (point-min)(1- (point-max))))
               (gtags-pop-context)
               nil)
      (goto-char (point-min))
      (setq lines (count-lines (point-min) (point-max)))
      (cond
       ((= 0 lines)
        (cond
         ((equal flag "P")
          (message "%s: path not found" tagname))
         ((equal flag "g")
          (message "%s: pattern not found" tagname))
         ((equal flag "I")
          (message "%s: token not found" tagname))
         ((equal flag "s")
          (message "%s: symbol not found" tagname))
         (t
          (message "%s: tag not found" tagname)))
	(gtags-pop-context)
	(kill-buffer buffer)
	(set-buffer save)
        nil)
       ((= 1 lines)
	(message "Searching %s ... Done" tagname)
	(gtags-select-it t)
        t)
       (t
	(switch-to-buffer buffer)
	(gtags-select-mode)
        t)))))

;; modified version which updates the tags file and tries again if the
;; tag was not found
(defun gk-gtags-find-tag ()
  "Input tag name and move to the definition."
  (interactive)
  (let (tagname prompt input)
    (setq tagname (gtags-current-token))
    (if tagname
        (setq prompt (concat "Find tag: (default " tagname ") "))
      (setq prompt "Find tag: "))
    (setq input (completing-read prompt 'gtags-completing-gtags
                                 nil nil nil gtags-history-list))
    (if (not (equal "" input))
        (setq tagname input))
    (gtags-push-context)
    (if (not (gk-gtags-goto-tag tagname ""))
        (progn
          (djcb-gtags-create-or-update)
          (gk-gtags-goto-tag tagname "")))))

(add-hook 'gtags-mode-hook 
          (lambda()
            (define-key gtags-mode-map (kbd "M-.") 'gk-gtags-find-tag)
            (define-key gtags-mode-map (kbd "C-M-.") 'gtags-find-rtag)
            (define-key gtags-mode-map (kbd "C-M-,") 'gtags-find-symbol)))
