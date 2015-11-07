#|`(a b c)

`(a ,b c ,d) is equal to (list 'a b 'c d)

(setq a 1 b 2 c 3)
`(a b ,c (',(+ a b c)) (+ a b) 'c '((,a ,b))) ==> (A B 3 ('6) (+ A B) 'C '((1 2)))
|#
(defmacro nil! (var)
    `(setq ,var nil))

(defmacro do-list ((var lst &optional result) &body body)
    `(progn (mapc #'(lambda (,var) ,@body) ,lst)
    ,result))

(defmacro nif (expr pos zero neg)
    `(cond
       ((< ,expr 0) ,neg)
       ((> ,expr 0) ,pos)
       ((= ,expr 0) ,zero))
   )

