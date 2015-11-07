(defstruct s1 ID1 NAME SURNAME AGE)
(defstruct s2 ID2 AUTO COLOR YEAR)
(defstruct s3 ID3 ID1 ID2)
(defstruct s4 ID4 ID1 NAME SURNAME AGE ID2 AUTO COLOR YEAR)
;(make-select "/run/media/neamiades/Files/Study/KPI/Lisp/Table1.txt" "/run/media/neamiades/Files/Study/KPI/Lisp/Table2.txt" "/run/media/neamiades/Files/Study/KPI/Lisp/Table3.txt")
;(lambda (res-file &key name surname age auto color year))
(defun make-select (t1 t2 t3)
    (let ( (table1) (table2) (table3))
        (setf table1 (fill_struct1 (crt_lst (fill_lst t1))))
        (setf table2 (fill_struct2 (crt_lst (fill_lst t2))))
        (setf table3 (fill_struct3 (crt_lst (fill_lst t3))))
        (lambda (res-file &key name surname age auto color year)
            (cond ((and name surname age auto color year)
                    )
                  ((and name surname age auto color) )
                  ((and name surname age auto) )
                  ((and name surname age) (items-by-param (items-by-param (items-by-param table1 name "s1-NAME") surname "s1-SURNAME") age "s1-AGE"))
                  ((and name surname) (items-by-param (items-by-param table1 name "s1-NAME") surname "s1-SURNAME"))
                  ((and name)
                    (give-id (items-by-param table1 name "s1-NAME"))
                    (items-by-param table3 "ID1" "s3-ID1"))
                  (t ))
        )

    )
)
(defun make-res-list (t1 t2 t3)

)

(defun give-id (table param)
    (when table
        (let* ( (rw (car table))
            (name ((read-from-string param) rw)))
            (append (list name) (give-id (cdr table) param))
        ))
)

(defun items-by-param (table item param)
    (when table
        (let* ( (rw (car table))
            (name ((read-from-string param) rw)))
            (if (eql name item)
                (append (list rw) (items-by-name (cdr table) item param))
                (items-by-name (cdr table) item param)
            )
        )))

(defun fill_struct4 (lst_val1 lst_val2)
    (let ( (size (length lst_val1)) (table) )
        (do ((i 0 (+ i 1)) (lst1 lst_val1 (cdr lst1)) (lst2 lst_val2 (cdr lst2)))
            ((= i size) table)
            (push (make-s4 :ID4 i
                          :ID1 (elt (car lst1) 0)
                          :NAME (elt (car lst1) 1)
                          :SURNAME (elt (car lst1) 2)
                          :AGE (elt (car lst1) 3)
                          :ID2 (elt (car lst2) 0)
                          :AUTO (elt (car lst2) 1)
                          :COLOR (elt (car lst2) 2)
                          :YEAR (elt (car lst2) 3)
            ) table)

        )
    )
)

(defun elem_lst (seq)
    (let ( (size (count #\, seq)) (lst))
        (do ((i 0 (+ i 1)) (start 0))
            ((> i size) (reverse lst))
            (push (subseq seq start (position #\, seq :start start)) lst)
            (if (/= i size) (setf start (+ 1 (position #\, seq :start start))))

        )
    )
)

(defun crt_lst (lst_seq)
    (if (null lst_seq)
        nil
        (cons (elem_lst (car lst_seq)) (crt_lst (cdr lst_seq)))

    )

)

(defun fill_file (file_name lst)
     (let ( (size (length lst)) )
        (with-open-file (stream file_name
            :direction :output :if-exists :supersede)
            (do* ((i 0 (+ i 1)) (w_lst lst (cdr w_lst)) (l (car w_lst) (car w_lst)))
                ((= i size))
                (format stream "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
                    (s4-ID4 l) (s4-ID1 l) (s4-NAME l) (s4-SURNAME l) (s4-AGE l) (s4-ID2 l) (s4-AUTO l) (s4-COLOR l) (s4-YEAR l)
                ))))
)

(defun fill_lst (file_name)
    (let ((lst))
        (with-open-file (s file_name)
                (do ((l (read-line s) (read-line s nil 'eof)))
                    ((eq l 'eof) (cdr lst))
                    (push l lst)
                )
    ))
)

(defun fill_struct1 (lst_val)
    (let ( (size (length lst_val)) (table) )
        (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
            ((= i size) table)
            (push (make-s1 :ID1 (elt (car lst) 0)
                          :NAME (elt (car lst) 1)
                          :SURNAME (elt (car lst) 2)
                          :AGE (elt (car lst) 3)
            ) table)

        )
    )
)

(defun fill_struct2 (lst_val)
    (let ( (size (length lst_val)) (table) )
        (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
            ((= i size) table)
            (push (make-s2 :ID2 (elt (car lst) 0)
                          :AUTO (elt (car lst) 1)
                          :COLOR (elt (car lst) 2)
                          :YEAR (elt (car lst) 3)
            ) table)
            ;(setf lst (cdr lst))
        )
    )
)

(defun fill_struct3 (lst_val)
    (let ( (size (length lst_val)) (table) )
        (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
            ((= i size) table)
            (push (make-s3 :ID3 (elt (car lst) 0)
                          :ID1 (elt (car lst) 1)
                          :ID2 (elt (car lst) 2)
            ) table)

        )
    )
)



#|
(defun elem_lst(seq)
    (let ( (size (count #\, seq)) (lst))
        (do ((i 0 (+ i 1)) (start 0))
            ((> i size) lst)
            (push (subseq seq start (position #\, seq :start start)) lst)
            (if (/= i size) (setf start (+ 1 (position #\, seq :start start))))

        )
    )
)
|#

#|
        (eval (read-from-string (pop ls1)))
        (eval (read-from-string (pop ls2)))
        (eval (read-from-string (pop ls3)))
|#
