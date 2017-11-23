#lang racket
(provide make-binarytree)
(provide append-binarytree)
(provide iterator-binarytree)
(provide clone-binarytree)
(provide reverse-binarytree)

(define (make-binarytree deep [fill null])
  (if (= deep 0)
      null
      (letrec ([f (lambda (incre [tree null])
                    (if (> incre deep)
                        tree
                        (begin
                          (set! tree (append-binarytree tree (mcons null null)))
                          (f (+ incre 1) tree))))])
        (f 0))))

(define (mpair-iterator-stop? link) (or (not (mpair? link)) (null? link)))

(define (append-binarytree tree node)
  (if (null? tree)
      node
      (mcons (append-binarytree (mcar tree) node)
             (append-binarytree (mcdr tree) node))))

(define (iterator-binarytree tree proc)
  (proc tree)
  (when (mpair? tree)
    (iterator-binarytree (mcar tree) proc)
    (iterator-binarytree (mcdr tree) proc)))


(define (clone-binarytree tree)
  (if (mpair? tree)
      (mcons (clone-binarytree (mcar tree)) (clone-binarytree (mcdr tree)))
      tree))


(define (reverse-binarytree tree)
  (when (mpair? tree)
    (if (and [mpair? [mcar tree]] [mpair? [mcdr tree]])
        (mcons [reverse-binarytree (mcdr tree)] [reverse-binarytree (mcar tree)])
        (mcons [mcdr tree] [mcar tree]))))
