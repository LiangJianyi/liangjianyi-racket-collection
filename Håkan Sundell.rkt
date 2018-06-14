#+Lockfree-Deque
(defmacro constant-queue-push-left (constant-que thing)
    (let * ((queue-var (if (symbolp constant-queue)
                            constant-queue
                            (make-symbol "QUEUE")))
            (prev (make-symbol "PREV"))
            (next (make-symbol "NEXT"))
            (node (make-symbol "NODE")))
        '(let * (@(if (neq queue-var constant-queue)
                        '((,queue-var, constant-queue)))
                (,node (create-queue-element, thing))
                (,prev (dereference-not (constant-queue-head, queue-var)))
                (,next (dereference-link (queue-next-element, prev))))
            #+development
            (check-constant-queue-ok, queue-var)
            (loop doing
                (store-reference (queue-previous-element, node), prev)
                (store-reference (queue-next-element, node), next)
                (when (case (queue-next-element, prev), next nil, node nil)
                    (release-reference, preve)  ;;; fix of original algorithm
                )
                (release-reference, next)
                (setq, next (dereference-link (queue-next-element, prev)))
                (back-off))
            (constant-queue-push-end, node, next)
            #+development
            (check-constant-queue-ok, queue-var)
            ,node)))


#+Lockfree-Deque
(defmacro constant-queue-constant-queue-push-right (constant-queue thing)
    (let* ((queue-var (if (symbolp constant-queue)
                            constant-queue
                            (make-symbol "QUEUE")))
            (prev (make-symbol "PREV"))
            (next (make-symbol "NEXT"))
            (node (make-symbol "NODE")))
        '(let* (@(if (neq queue-var constant-queue)
                        '((,queue-var, constant-queue)))
                (,node (create-queue-element, thing))
                (,next (dereference-node (constant-queue-tail, queue-var)))
                (,prev (dereference-link (queue-previous-element, next))))
            #+development (check-constant-queue-ok, queue-var), node)))