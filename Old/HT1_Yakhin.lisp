;;1)bubble sort
;;2)choice sort
;;3)insert sort

;;;Non-functional type
;;;I think help-functions too big to put them in the labels, mb it is my mistake, but ^^

(defun bubble1 (orig_lst pred)
    (let ( (org_size (length orig_lst)) (lst (copy-list orig_lst)))
    (do ((flag t) (size (- org_size 1) (- size 1)))
        ((not flag))
        (setf flag nil)
        (do ((i 0 (+ i 1)) )
            ((= i size) )
            (if (funcall pred (nth i lst) (nth (+ i 1) lst))
                (progn
                    (setf flag t)
                    (psetf (nth i lst) (nth (+ i 1) lst) (nth (+ i 1) lst) (nth i lst))
               ))))
               lst))

(defun insert1 (lst pred)
    (do ( (flag t t) (size 1 (+ size 1)) (lst1 (list (car lst))) (lst2 (cdr lst) (cdr lst2)))
        ((null lst2) lst1)
            (do ( (i 0 (+ i 1)) )
                ((= i size))
                (if (and (funcall pred (nth i lst1) (car lst2)) flag)
                    (progn
                        (setf lst1 (append (subseq lst1 0 i) (list (car lst2)) (subseq lst1 i)))
                        (setf flag nil))
                )
            )
            (if flag
                (setf lst1 (append lst1 (list (car lst2)))))
    )
)

(defun select1 (lst pred)
     (labels (( get-el(pred lst)
       (if (null (cdr lst))
          (car lst)
          (if (funcall pred (car lst) (cadr lst))
              (get-el pred (cons (car lst) (cdr (cdr lst))))
              (get-el pred (cdr lst))))))
      (do ((lst1) (lst2 (copy-list lst)))
        ((null lst2) lst1)
        (let* ( (x (get-el pred lst2)) (pos (position x lst2)))
            (push x lst1)
            (setf lst2 (remove x lst2 :start pos :end (+ pos 1)))))))


;;;Non-functional type END

;;;Functional
;;bubble
(defun bubble (lst pred)
    (if (is-not-sorted lst pred)
	(bubble (bsort lst pred) pred)
	lst)
    )
(defun is-not-sorted (lst pred)
    (if (cdr lst)
	(if (funcall pred (car lst) (cadr lst))
	    t
	    (is-not-sorted (cdr lst) pred))
	nil)
    )
(defun bsort (lst pred)
    (if (cdr lst)
	(if (funcall pred (car lst) (cadr lst))
	    (cons (cadr lst)  (bsort (cons (car lst) (cddr lst)) pred))
	    (cons (car lst) (bsort (cdr lst) pred)))
	lst))
;;bubble

;;insert
(defun insert-sort(lst pred &optional new-lst)
    (if lst
	(insert-sort (cdr lst) pred (insert new-lst (car lst) pred))
	new-lst))

(defun insert (lst item pred)
    (if lst
	(if (funcall pred item (car lst))
	    (append (list (car lst)) (insert (cdr lst) item pred))
	    (cons item lst))
	(list item)))
;;insert

(defun select-sort (lst pred) ;; need use predicate like max and min!!!
    (if lst
	(let* ((val (reduce pred lst)) (pos (position val lst)) (n-lst (append (subseq lst 0 pos) (subseq lst (+ 1 pos)))))
	    (append (select-sort n-lst pred) (list val)))))



