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
            (my-member item (cdr lst)))))

(defun my-find1 (item seq)
    (if (= 0 (length seq))
        nil
        (if (eql (elt seq 0) item)
            item
            (my-find1 item (subseq seq 1)))))

(defun my-find (item seq &key from-end (start 0) end key)
    (if (or (= 0 (length seq)) (eql end 0))
        nil
        (let ((work_seq seq) (n_end (if end (if (> start 0) (- end start) (- end 1)) )))
        (progn
            (if from-end
               (setf work_seq (reverse work_seq)))
            (if key
                (if (eql (funcall key (elt work_seq start)) item)
                    (elt work_seq start)
                    (my-find item (subseq work_seq (+ start 1)) :end n_end :key key)) ;end key doesn't work

                (if (eql (elt work_seq start) item)
                    item
                    (my-find item (subseq work_seq (+ start 1)) :end n_end)))))))


(defun my-find-if1 (func lst)
    (if (atom lst)
        nil
        (if (funcall func (car lst))
            (car lst)
            (my-find-if1 func (cdr lst)))))

(defun my-find-if (func seq &key from-end (start 0) end key)
    (if (or (= 0 (length seq)) (eql end 0))
        nil
        (let ((work_seq seq) (n_end (if end (if (> start 0) (- end start) (- end 1)) )))
        (progn
            (if from-end
               (setf work_seq (reverse work_seq)))
            (if key
                (if (funcall func (funcall key (elt work_seq start)))
                    (elt work_seq start)
                    (my-find-if func (subseq work_seq (+ start 1)) :end n_end :key key))

                (if (funcall func (elt work_seq start))
                    (elt work_seq start)
                    (my-find-if func (subseq work_seq (+ start 1)) :end n_end)))))))

(defun myfind-if-not (func lst)
    (if (atom lst)
        nil
        (if (not (funcall func (car lst)))
            (car lst)
            (myfind-if-not func (cdr lst)))))

(defun my-mapcar (func lst)
    (if (null lst)
        nil
        (cons (funcall func (car lst)) (my-mapcar func (cdr lst)) )))

(defun my-mapcan (func lst)
    (if (null lst)
        nil
        (append (funcall func (car lst)) (my-mapcan func (cdr lst)) )))

(defun my-reduce1 (func lst)
    (if (atom (cdr lst))
        (car lst)
        (funcall func (car lst) (my-reduce1 func (cdr lst)))))
#|
(defun my-reduce (func lst &key key from-end (start 0) end initial-value)
    (let ( (w_lst lst) (flag t))
        (progn
            (if from-end (setf w_lst (reverse w_lst)))
            (if (atom (cdr w_lst))
                (car w_lst)
                (funcall func (car w_lst) (my-reduce func (cdr w_lst)))
            )
        )
    )
)
|#

(defun my-count (item seq &key from-end (start 0) end key (test #'eql))
    (if (or (= 0 (length seq)) (eql end 0))
        0
        (let ( (w_seq seq) (w_end (if end (if (> start 0) (- end start) (- end 1)) )))
            (progn
                (if from-end (setf w_seq (reverse w_seq)))
                (if w_end (setf w_end (- w_end 1)))
                (if key
                    (if (funcall test (funcall key (elt seq start)) item)
                        (+ 1 (my-count item (subseq w_seq (+ start 1)) :end w_end :key key :test test))
                        (my-count item (subseq w_seq (+ start 1)) :end w_end :key key :test test)
                    )
                    (if (funcall test (elt seq start) item)
                        (+ 1 (my-count item (subseq w_seq (+ start 1)) :end w_end :test test))
                        (my-count item (subseq w_seq (+ start 1)) :end w_end :test test)
                    ))))))


(defun my-position (item seq &key from-end (start 0) end key (test #'eql))
    (if (or (= 0 (length seq)) (eql end 0))
        nil
        (let ( (w_seq seq) (w_end (if end (if (> start 0) (- end start) (- end 1)) )))
            (progn
                (if from-end (setf w_seq (reverse w_seq)))
                (if key
                    (if (funcall test (funcall key (elt w_seq start)) item)
                        0
                        (let ((res (my-position item (subseq w_seq (+ start 1)) :end w_end :key key :test test)))
                            (if (null res)
                                nil
                                (+ 1 res))))
                    (if (funcall test (elt w_seq start) item)
                        0
                        (let ((res (my-position item (subseq w_seq (+ start 1)) :end w_end :test test)))
                            (if (null res)
                                nil
                                (+ 1 res)))))))))

 (defun my-remove (item lst)
    (if (atom lst)
        nil
        (if (eql (car lst) item)
            (my-remove item (cdr lst))
            (cons (car lst) (my-remove item (cdr lst)))
        )
    )
 )

 (defun my-remove (item lst &key from-end (start 0) end key (test #'eql))
    (if (or (= 0 (length lst)) (eql end 0))
        (subseq lst start)
        (let ( (w_lst lst) (w_end (if end (if (> start 0) (- end start) (- end 1)) )))
            (progn

                (if from-end (setf w_lst (reverse w_lst)))
                (if key
                    (if (funcall test (funcall key (elt lst start)) item)
                            (append (subseq lst 0 start) (my-remove item (cdr (subseq lst start)) :end w_end :key key :test test))
                            (append (subseq lst 0 start) (list (elt lst start)) (my-remove item (cdr (subseq lst start)) :end w_end :key key :test test))

                    )
                    (if (funcall test (elt lst start) item)
                        (append (subseq lst 0 start) (my-remove item (cdr (subseq lst start)):end w_end :test test))
                        (append (subseq lst 0 start) (list (elt lst start)) (my-remove item (cdr (subseq lst start)):end w_end :test test))
                        ))))))

(defun my-remove-if (pred lst)
    (if (null lst)
        nil
        (if (funcall pred (car lst))
            (my-remove-if pred (cdr lst))
            (cons (car lst) (my-remove-if pred (cdr lst))))))

 (defun my-remove-if-not (pred lst)
    (if (atom lst)
        nil
        (if (not (funcall pred (car lst)))
            (my-remove-if pred (cdr lst))
            (cons (car lst) (my-remove-if pred (cdr lst))))))

(defun my-remove-duplicates (lst)
    (if (atom lst)
        nil
        (if (> (my-count (car lst) lst) 1)
           (cons (car lst) (my-remove-duplicates (my-remove (car lst) (cdr lst))))
           (cons (car lst) (my-remove-duplicates (cdr lst))))))

;(load "/run/media/neamiades/Files/Study/KPI/Lisp/Programs/HT2_Yakhin.lisp")
