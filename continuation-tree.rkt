#lang racket
(define t1 '(a (b (d h)) (c e (f i) g)))
(define t2 '(1 (2 (3 6 7) 4 5)))

(define (dft tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (write tree))
        (else (dft (car tree))
              (dft (cdr tree)))))

(define *saved* null)

(define (dft-node tree)
  (cond ((null? tree) (restart))
        ((not (pair? tree)) tree)
        (else (call-with-current-continuation
               (lambda (cc)
                 (set! *saved*
                       (cons (lambda ()
                               (cc (dft-node (cdr tree))))
                             *saved*))
                 (dft-node (car tree)))))))

(define (restart)
  (if (null? *saved*)
      'done
      (let ((cont (car *saved*)))
        (set! *saved* (cdr *saved*))
        (cont))))

(define (dft2 tree)
  (set! *saved* null)
  (let ((node (dft-node tree)))
    (cond ((eq? node 'done) null)
          (else (write node)
                (restart)))))
