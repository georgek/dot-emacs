(defmacro time (&rest body)
  `(let ((t-bef (float-time)))
     ,@body
     (- (float-time) t-bef)))

(defmacro ml (&rest strings)
  "Makes a multiline string."
  (mapconcat #'identity strings "\n"))

