Tasks:
1) Add predicate for sort;(bubble,insert,choice)
2) complement;
3) member/find/find­if/find­if­not
4) mapcar/mapcan/reduce;
5) count/position;
6)remove/remove­if/
remove­if­not/remove­duplicates;
;;
(defun my-complement (func)
    (lambda (x) (not (funcall func x)))
)

(funcall (my-complement #'zerop) 1) ;;test for my-complement

(defun my-member (item lst)
    (if (atom lst)
    nil
        (if (eql (car lst) item)
        lst
            (my-member item (cdr lst))
        )
    )
)

(my-member 2 '(1 2 3)) =>  (2 3)   ;;test for my-member

(defun my-member-if (func lst)
    (if (atom lst)
    nil
        (if (funcall func (car lst))
        lst
            (my-member func (cdr lst))
        )
    )
);; don't work

(defun myfind (item lst)
    (if (atom lst)
    nil
        (if (eql item (car lst))
        item
            (myfind item (cdr lst)))
            )
)

(defun myfind-if (func lst)
    (if (atom lst)
    nil
        (if (funcall func (car lst))
        (car lst)
            (myfind func (cdr lst)))
            )
)

(myfind-if #'oddp '(1 2 3 4 5))

(defun mymapcar (func lst)
    (if (atom lst)
    (funcall func lst)
        (cons (funcall func (car lst)) (mymapcar func (cdr lst)) )
    )
)

(mymapcar #'car '((1 a) (2 b) (3 c)))

(defun myreduce(func lst)
    (if (atom (cdr lst))
    (car lst)
        (funcall func (car lst) (myreduce func (cdr lst))))
)

