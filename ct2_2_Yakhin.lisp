(defstruct Ntree Nnode Nleaf)


(defun mkTree (lfunc)
        (and (not (null lfunc))
            (if (atom lfunc)
                lfunc
                (make-Ntree :Nnode (car lfunc) :Nleaf (mapcar #'mkTree (cdr lfunc)))
            )
        )
)

(defun lTree (tree)
    (if (Ntree-p tree)
        (cons (Ntree-Nnode tree) (mapcar #'ltree (Ntree-Nleaf tree)))
        tree
    )
)



(defun preTree (tree)
    (if (Ntree-p tree)
            (progn
                (format t "~A " (Ntree-Nnode tree))
                (mapcar #'preTree (Ntree-Nleaf tree))
            )
            (format t "~A " tree)
    )
    (values)
)

#|
(defun flatten (lst)
	(mapcan (lambda (x)
		(if (atom x)
			(list x)
			(flatten x)))
			lst)
)

(defun preTree3 (tree)
    (if (Ntree-p tree)
        (cons (Ntree-Nnode tree) (flatten (mapcar #'preTree3 (Ntree-Nleaf tree))))
        tree
    )
)


(defun preTree2 (tree)
    (if (Ntree-p tree)
        (format nil "~A ~A" (Ntree-Nnode tree) (mapcar #'preTree2 (Ntree-Nleaf tree)))
        (format nil "~A" tree)
    )
)

(defun preTree3 (tree)
    (if (Ntree-p tree)
        (cons (Ntree-Nnode tree) (mapcar #'preTree3 (Ntree-Nleaf tree)))
        tree
    )
)
|#

(defun postTree (tree)
    (if (Ntree-p tree)
            (progn
                (mapcar #'postTree (Ntree-Nleaf tree))
                (format t "~A " (Ntree-Nnode tree))

            )
            (format t "~A " tree)
    )
    (values)
)

(defun breTree (tree &key (node_name t))
    (labels ( (show (lfs)
                (cond
                    ( (Ntree-p lfs) (format t "~A " (Ntree-Nnode lfs)))
                    ( (null lfs) (values))
                    (t (format t "~A " lfs))
                )
                ))
    (if (Ntree-p tree)
            (progn
                (if node_name
                    (format t "~A " (Ntree-Nnode tree)))
                (mapcar #'show (Ntree-Nleaf tree))
                (mapcar #'breTree (Ntree-Nleaf tree) nil)
            )
    )
    )
    (values)
)

