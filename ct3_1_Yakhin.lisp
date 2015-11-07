(defmacro aif (test foo bar)
    `(let ( (it ,test) ) (if it ,foo ,bar)))

(defmacro awhen (test &body body)
   `(let ( (it ,test) ) (if it (progn ,@body))))


(defmacro cut (&rest rest)
    (labels (
	     (flatten (lst)
		 (mapcan (lambda (x)
			     (if (atom x)
				 (list x)
				 (flatten x)))
			 lst))
	     (count_ (lst)
		 (count '_ (flatten lst)))
	     (repl (seq1 seq2)
		 (if seq2
		     (if (listp (car seq2))
			 (cons (repl seq1 (car seq2)) (repl (subseq seq1 (count_ (car seq2))) (cdr seq2)))
			 (if (eql (car seq2) '_)
			     (cons (pop seq1) (repl seq1 (cdr seq2)))
			     (cons (car seq2) (repl seq1 (cdr seq2)))))))
	     (crt-vars (n)
		 (if (/= 0 n)
		     (cons (read-from-string (concatenate 'string "variable" (write-to-string n)))  (crt-vars (- n 1))))))
	(let* ((nlst (crt-vars (count_ rest))) (nrest (repl nlst rest)))
	    `(lambda ,nlst (,@nrest)))))








