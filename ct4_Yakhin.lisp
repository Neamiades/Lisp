(defmacro defanaph (name fn)
    (let ((expand (gensym)))
	`(labels ((,expand (args syms)
		 (if args
		     (let ((sym (gensym)))
			 `(let* ((,sym ,(car args))
				 (it ,sym))
			      ,(,expand (cdr args)
					 (append syms
						 (list sym)))))
		     `(,',fn ,@syms))))
	(defmacro ,name (&rest args)
	     (,expand args nil)))))

;;(defanaph a+ +)

(defmacro with-gensyms (args &body body)
    `(let ,(mapcar (lambda (x) `(,x (gensym))) args) ,@body))


(defmacro our-let* (bindings &body body)
    (let-expand (mapcar #'cadr bindings) nil)
    )
(defun let-expand (args syms)
    (if args
	(our-let ((sym (gensym)))
	    `(our-let ((,sym ,(car args)))
		 ,(let-expand (cdr args)
			    (append syms
				    (list sym)))))
	`((lambda ,(mapcar #'car bindings) ,@body) ,@syms)))
