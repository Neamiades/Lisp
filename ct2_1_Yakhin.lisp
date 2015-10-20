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
        (cons (Ntree-Nnode tree) (ltree Ntree-Nleaf tree))
        tree)
)


