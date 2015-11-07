#|
Tasks:
1) Add predicate for sort;(bubble,insert,choice)
2) complement;+
3) member/find/find­if/find­if­not+
4) mapcar/mapcan/reduce;
5) count/position;
6)remove/remove­if/
remove­if­not/remove­duplicates;
|#

(defun my-complement (func)
    (lambda (x) (not (funcall func x)))
)



(defun my-member (item lst)
    (if (atom lst)
        nil
        (if (eql (car lst) item)
            lst
            (my-member item (cdr lst))
        )
    )
)



(defun my-find (item lst)
    (if (atom lst)
        nil
        (if (eql item (car lst))
            item
            (my-find item (cdr lst)))
    )
)

(defun myfind-if (func lst)
    (if (atom lst)
        nil
        (if (funcall func (car lst))
            (car lst)
            (myfind-if func (cdr lst))
        )
    )
)

(defun myfind-if-not (func lst)
    (if (atom lst)
        nil
        (if (not (funcall func (car lst)))
            (car lst)
            (myfind-if func (cdr lst))
        )
    )
)

(defun my-mapcar (func lst)
    (if (null lst)
        nil
        (cons (funcall func (car lst)) (my-mapcar func (cdr lst)) )
    )
)

(defun my-mapcan (func lst)
    (if (null lst)
        nil
        (append (funcall func (car lst)) (my-mapcan func (cdr lst)) )
    )
)

(defun my-reduce(func lst)
    (if (atom (cdr lst))
        (car lst)
        (funcall func (car lst) (my-reduce func (cdr lst)))
    )
)



(defun my-count (item seq)
    (if (= 0 (length seq))
        0
        (if (eql (elt seq 0) item)
            (+ 1 (my-count item (subseq seq 1)))
            (my-count item (subseq seq 1))
        )
    )
)


(defun my-position (item seq)
    (if (= 0 (length seq))
        nil
        (if (eql (elt seq 0) item)
            0
            (let ((res (my-position item (subseq seq 1))))
                (if (null res)
                    nil
                    (+ 1 res)
                )
            )

        )
    )
)


 (defun my-remove (item lst) ;don't know how to do this with seq :( , maybe with one more if, TYPE-OF and concatenate but i'm not shure that is right decision
    (if (atom lst)
        nil
        (if (eql (car lst) item)
            (my-remove item (cdr lst))
            (cons (car lst) (my-remove item (cdr lst)))
        )
    )
 )

(defun my-remove-if (pred lst) ;don't know how to do this with seq :(
    (if (atom lst)
        nil
        (if (funcall pred (car lst))
            (my-remove-if pred (cdr lst))
            (cons (car lst) (my-remove-if pred (cdr lst)))
        )
    )
 )

 (defun my-remove-if-not (pred lst) ;don't know how to do this with seq :(
    (if (atom lst)
        nil
        (if (not (funcall pred (car lst)))
            (my-remove-if pred (cdr lst))
            (cons (car lst) (my-remove-if pred (cdr lst)))
        )
    )
 )

#|(defun my-remove-duplicates (lst)
    (if (atom lst)
        nil
        (if (> (my-count (car lst) lst) 1)
           (cons (car lst) (remove-duplicates (my-remove (car lst) (cdr lst))))
           (cons (car lst) (remove-duplicates (cdr lst)))
        )
    )
)
|#
(defun my-remove-duplicates (lst);doesn't work with my own functions, on debug i can't understand where is the problem :(
    (if (atom lst)
        nil
        (if (> (my-count (car lst) lst) 1)
           (cons (car lst) (remove-duplicates (my-remove (car lst) (cdr lst))))
           (cons (car lst) (remove-duplicates (cdr lst)))
        )
    )
)

