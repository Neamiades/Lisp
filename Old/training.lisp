;;Charpter 1

(defun xl (lst)
    (if (listp (car lst))
        t
        (and (cdr lst) (xl (cdr lst)))
    )
)

(defun dot (n)
    (if (= n 0)
        nil
        (or
            (format t ".")
            (dot (- n 1))
        )
    )
)

(defun col (lst)
    (if (atom lst)
        0
        (+ (if (eq 'a (car lst))
                1
                0
            )
            (col (cdr lst))
        )
    )
)
(defun summit (lst)
    (apply #’+ (remove nil lst))
)

(defun summit (lst)
    (let ((x (car lst)) (y (cdr lst)))
        (if (atom y)
            0
           (if (null x)
                (summit y)
                (+ x (summit y))
            )
        )
    )
)
;;Charpter 2
(defun new-union (lst1 lst2)
    (if (or (null lst1) (null lst2))
        (append lst1 lst2)
        (append lst1
            (del lst1 lst2)
        )
    )
)

(defun del (lst1 lst2)
    (if (null lst1)
        lst2
        (if (find (car lst1) lst2)
            (del (cdr lst1) (remove (car lst1) lst2))
            (del (cdr lst1) lst2)
        )
    )
)


(defun occurrences (lst)
    (if (null lst)
        nil
        (let ((x (car lst)))
            (append (list (cons x (count x lst))) (occurrences (remove x lst)))
        )
    )
)

(sort tester #'> :key #'cdr)




































