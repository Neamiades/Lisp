
(defun group-by (fn lst)
	(reduce (labmda (acc x)
	(let ((key (funcall fn x)))
		(if (assoc key acc)
			(progn (push x (cdr (assoc key acc))) acc)
			(cons (cons key (list x)) acc))
		))
		lst
		:init_value nil))


(defun flatten (lst)
	(mapcan (lambda (x)
		(if (atom x)
			(list x)
			(flatten x)))
			lst))
