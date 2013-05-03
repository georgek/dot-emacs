(defmacro time (&rest body)
  `(let ((t-bef (float-time)))
     ,@body
     (- (float-time) t-bef)))
