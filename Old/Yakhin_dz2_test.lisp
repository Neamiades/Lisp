(funcall (my-complement #'zerop) 1) ;;test for my-complement
(my-member 2 '(1 2 3)) =>  (2 3)   ;;test for my-member
(myfind-if #'oddp '(1 2 3 4 5))
(my-mapcar #'car '((1 a) (2 b) (3 c)))
(my-mapcan #'(lambda (x) (and (numberp x) (list x)))
          '(a 1 b c 3 4 d 5))
(my-count #\a "how many A's are there in here?")
(my-count 2 '(1 3 4))

(my-position #\t "baobab")
(my-position 1 '(2 3 4))
