#+Lockfree-Deque
(defmacro constant-queue-pop-left (constant-queue &optional (default-value nil))
    (let ((queue-var (if (symbolp constant-queue)
                                    constant-queue
                                    (make-symbol "QUEUE")))
            (prev (make-symbol "PREV"))
            (next (make-symbol "NEXT"))
            (node (make-symbol "NODE"))
            (mark (make-symbol "MARK"))
            (value (make-symbol "VALUE")))
        '(let* (,@(if (neq queue-var constant-queue)
                        '((,queue-var, constant-queue)))
                (,prev (dereference-node (constant-queue-head, queue-var))))
            #+development
            (check-constant-queue-ok, queue-var)
            (loop doing
                (let ((,node (dereference-link (queue-next-element, prev))))
                    (when (eq, node (constant-queue-tail, queue-var))
                        (release-reference, node, prev)
                        default-value))
                    (multiple-value-bind (,next, mark)
                        (dereference-link (queue-next-element, node))
                        (if ,mark
                            (progn
                                (set-reference-mark (queue-previous-element, node))
                                (cas-reference (queue-next-element, prev) ,node nil, next nil)
                                (release-reference, next, node))
                            (progn
                                (when (cas-reference (queue-next-element, node) ,next nil, next t)
                                    (setq, prev (correct-previous, prev, next))
                                    (release-reference, prev, next)
                                    (let ((value (queue-datum node)))
                                        (release-reference node)
                                        #+development
                                        (check-constant-queue-ok queue-var)
                                        value)))
                                (release-reference next node)
                                (back-off)))))))