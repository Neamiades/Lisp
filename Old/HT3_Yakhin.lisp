(defstruct s1 ID1 NAME SURNAME AGE)
(defstruct s2 ID2 AUTO COLOR YEAR)
(defstruct s3 ID3 ID1 NAME SURNAME AGE ID2 AUTO COLOR YEAR)
;(make-select "/run/media/neamiades/Files/Study/KPI/Lisp/Table1.txt" "/run/media/neamiades/Files/Study/KPI/Lisp/Table2.txt" "/run/media/neamiades/Files/Study/KPI/Lisp/Table3.txt")
;(lambda (res-file &key name surname age auto color year))



(defun make-select (t1 t2 t3)
    (labels
	    ((crt_lst (lst_seq)
             (labels (
                  (elem_lst (seq)
                      (let ( (size (count #\, seq)) (lst))
                      (do ((i 0 (+ i 1)) (start 0))
                          ((> i size) (reverse lst))
                          (push (subseq seq start (position #\, seq :start start)) lst)
                          (if (/= i size) (setf start (+ 1 (position #\, seq :start start))))))))
                 (if (null lst_seq)
                 nil
                 (cons (elem_lst (car lst_seq)) (crt_lst (cdr lst_seq))))))
	     (fill_lst (file_name)
             (let ((lst))
                 (with-open-file (s file_name)
                 (do ((l (read-line s) (read-line s nil 'eof)))
                     ((eq l 'eof) (cdr lst))
                     (push l lst)))))
            (items-by-param (table item param)
                    (if item
                        (when table
                            (let* ( (rw (car table))
                                (name (eval `(,(read-from-string param) ,rw))))
                                (if (equal name item)
                                    (append (list rw) (items-by-param (cdr table) item param))
                                    (items-by-param (cdr table) item param))))
                        table))
	     (fill_struct1 (lst_val)
            (let ( (size (length lst_val)) (table) )
                (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
                    ((= i size) table)
                    (push (make-s1 :ID1 (elt (car lst) 0)
                                  :NAME (elt (car lst) 1)
                                  :SURNAME (elt (car lst) 2)
                                  :AGE (elt (car lst) 3)
                    ) table))))
            (fill_struct2 (lst_val)
                (let ( (size (length lst_val)) (table) )
                    (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
                        ((= i size) table)
                        (push (make-s2 :ID2 (elt (car lst) 0)
                                      :AUTO (elt (car lst) 1)
                                      :COLOR (elt (car lst) 2)
                                      :YEAR (elt (car lst) 3)
                        ) table))))
                (fill_struct3 (lst_val1 tab1 tab2)
                    (let ( (size (length lst_val1)) (table) )
                        (do ((i 0 (+ i 1)) (lst1 lst_val1 (cdr lst1)) (lst2 lst_val2 (cdr lst2)))
                            ((= i size) table)
                            (let ((t1 (items-by-param tab1 (elt (car lst1) 1) "s1-ID1"))
                                (t2 (items-by-param tab2 (elt (car lst1) 2) "s2-ID2")))
                            (push (make-s3 :ID3 i
                                          :ID1 (elt (car lst1) 1)
                                          :NAME  (s1-NAME t1)
                                          :SURNAME (s1-SURNAME t1)
                                          :AGE (s1-AGE t1)
                                          :ID2 (elt (car lst1) 2)
                                          :AUTO (s2-AUTO t2)
                                          :COLOR (s2-COLOR t2)
                                          :YEAR (s2-YEAR t2)
                            ) table)))))

                (fill_file (file_name lst)
                 (let ( (size (length lst)) )
                    (with-open-file (stream file_name
                        :direction :output :if-exists :supersede)
                        (do* ((i 0 (+ i 1)) (w_lst lst (cdr w_lst)) (l (car w_lst) (car w_lst)))
                            ((= i size))
                            (format stream "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
                                (s3-ID3 l) (s3-ID1 l) (s3-NAME l) (s3-SURNAME l) (s3-AGE l) (s3-ID2 l) (s3-AUTO l) (s3-COLOR l) (s3-YEAR l)
                            ))))))
        (let ( (table1) (table2) (table3))
            (setf table1 (fill_struct1 (crt_lst (fill_lst t1))))
            (setf table2 (fill_struct2 (crt_lst (fill_lst t2))))
            (setf table3 (fill_struct3 (crt_lst (fill_lst t3)) table1 table2))
            (lambda (res-file &key name surname age auto color year)
               (fill_file res-file (items-by-param (items-by-param (items-by-param (items-by-param (items-by-param (items-by-param
                    table3 name "s3-NAME")
                        surname "s3-SURNAME")
                            age "s3-AGE")
                                auto "s3-AUTO")
                                    color "s3-COLOR")
                                        year "s3-YEAR"))
                ))))



(defun items-by-param (table item param)
    (if item
        (when table
            (let* ( (rw (car table))
                (name (eval `(,(read-from-string param) ,rw))))
                (if (equal name item)
                    (append (list rw) (items-by-param (cdr table) item param))
                    (items-by-param (cdr table) item param))))
        table))

(defun fill_struct1 (lst_val)
    (let ( (size (length lst_val)) (table) )
        (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
            ((= i size) table)
            (push (make-s1 :ID1 (elt (car lst) 0)
                          :NAME (elt (car lst) 1)
                          :SURNAME (elt (car lst) 2)
                          :AGE (elt (car lst) 3)
            ) table))))

(defun fill_struct2 (lst_val)
    (let ( (size (length lst_val)) (table) )
        (do ((i 0 (+ i 1)) (lst lst_val (cdr lst)))
            ((= i size) table)
            (push (make-s2 :ID2 (elt (car lst) 0)
                          :AUTO (elt (car lst) 1)
                          :COLOR (elt (car lst) 2)
                          :YEAR (elt (car lst) 3)
            ) table))))



(defun fill_struct3 (lst_val1 tab1 tab2)
    (let ( (size (length lst_val1)) (table) )
        (do ((i 0 (+ i 1)) (lst1 lst_val1 (cdr lst1)) (lst2 lst_val2 (cdr lst2)))
            ((= i size) table)
            (let ((t1 (items-by-param tab1 (elt (car lst1) 1) "s1-ID1"))
                (t2 (items-by-param tab2 (elt (car lst1) 2) "s2-ID2")))
            (push (make-s3 :ID3 i
                          :ID1 (elt (car lst1) 1)
                          :NAME  (s1-NAME t1)
                          :SURNAME (s1-SURNAME t1)
                          :AGE (s1-AGE t1)
                          :ID2 (elt (car lst1) 2)
                          :AUTO (s2-AUTO t2)
                          :COLOR (s2-COLOR t2)
                          :YEAR (s2-YEAR t2)
            ) table)))))

(defun fill_file (file_name lst)
     (let ( (size (length lst)) )
        (with-open-file (stream file_name
            :direction :output :if-exists :supersede)
            (do* ((i 0 (+ i 1)) (w_lst lst (cdr w_lst)) (l (car w_lst) (car w_lst)))
                ((= i size))
                (format stream "~A,~A,~A,~A,~A,~A,~A,~A,~A~%"
                    (s3-ID3 l) (s3-ID1 l) (s3-NAME l) (s3-SURNAME l) (s3-AGE l) (s3-ID2 l) (s3-AUTO l) (s3-COLOR l) (s3-YEAR l)
                )))))




