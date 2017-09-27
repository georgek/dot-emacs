;;; message-x-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "message-x" "message-x.el" (22987 62321 265518
;;;;;;  257000))
;;; Generated autoloads from message-x.el

(autoload 'message-x-tab "message-x" "\
Smart completion or indentation in message buffers.

Looks at the position of point to decide what to do.  If point is in
one of the headers specified by `message-x-completion-alist', then the
completion function specified there is executed.  If point is in
another header (not mentioned there), then the function specified by
`message-x-unknown-header-function' is executed.  If point is in the
body, the function specified by `message-x-body-function' is executed.

Completion is magic: after the completion function is executed, checks
are performed to see if the completion function has actually done
something.  If it has not done anything,
`message-x-unknown-header-function' is executed.  See the function
`message-x-call-completion-function' for details on how to check
whether the completion function has done something.

A non-nil optional arg SKIP-COMPLETION (prefix arg if invoked
interactively) means to not attempt completion.  Instead,
`message-x-unknown-header-function' function is called in all headers,
known or unknown.

\(fn &optional SKIP-COMPLETION)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; message-x-autoloads.el ends here
