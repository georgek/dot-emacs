(defmacro time (&rest body)
  `(let ((t-bef (float-time)))
     ,@body
     (- (float-time) t-bef)))

(defmacro ml (&rest strings)
  "Makes a multiline string."
  (mapconcat #'identity strings "\n"))

(defmacro hash-table (test &rest contents)
  (let ((tbl-sym (cl-gensym "tbl"))
        (conts (mapcar
                #'(lambda (c)
                    (cons (eval (car c)) (eval (cadr c))))
                contents)))
   `(let ((,tbl-sym (make-hash-table :test ,test)))
      (dolist (pair ',conts)
        (puthash (car pair) (cdr pair) ,tbl-sym))
      ,tbl-sym)))

(defun file-to-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defmacro makehookedfun (hook &rest body)
  (declare (indent 1))
  (let ((function (intern (concat (symbol-name hook) "-function"))))
    `(progn
       (defun ,function ()
         ,@body)
       (add-hook ',hook #',function))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))
