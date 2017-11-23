#lang racket
(provide append-linkedlist)
(provide prepend-linkedlist)
(provide make-linkedlist)
(provide string->linkedlist)
(provide iterator-linkedlist)
(provide linkedlist-length)
(provide linkedlist-ref)
(provide find-node?)
(provide linkedlist-reverse)
(provide set-mcar-by-ref!)
(provide set-mcar-by-value!)
(provide remove-node-by-ref)
(provide remove-node-by-value)
(provide list->linkedlist)
(provide linkedlist->vector)
(provide linkedlist->string)

(define (mpair-iterator-stop? link) (or (not (mpair? link)) (null? link)))

(define (append-linkedlist list1 list2)
  (if (null? list1)
      list2
      (mcons (mcar list1) (append-linkedlist (mcdr list1) list2))))

(define (prepend-linkedlist list1 list2)
  (if (null? list2)
      list1
      (mcons (mcar list2) (prepend-linkedlist list1 (mcdr list2)))))

(define (make-linkedlist length . fill)
  (letrec ((f (lambda (incre [lik null])
                (if (> incre length)
                    lik
                    (if (= incre 0)
                        (f (+ incre 1))
                        (if (null? fill)
                            (f (+ incre 1) (append-linkedlist lik (mcons null null)))
                            (f (+ incre 1) (append-linkedlist lik (mcons (car fill) null)))))))))
    (f 0)))

(define (string->linkedlist s)
  (letrec ((s-length (string-length s))
           (linkedlist null)
           (conver (lambda (i)
                     (if (= i s-length)
                         linkedlist
                         (begin
                           (set! linkedlist (append-linkedlist linkedlist (mcons (string-ref s i) null)))
                           (conver (+ i 1)))
                         ))))
    (conver 0)))

(define (iterator-linkedtree tree proc)
  (if (mpair? tree)
      (begin
        (iterator-linkedtree (mcar tree) proc)
        (iterator-linkedtree (mcdr tree) proc))
      (proc tree)))

(define (iterator-linkedlist lik proc)
  (cond [[mpair? lik] (proc lik) (iterator-linkedlist (mcdr lik) proc)]))

(define (linkedlist-length lik)
  (letrec ((length 0)
           (f (lambda (linkedlist)
                (cond [[mpair? linkedlist] (set! length (+ length 1)) (f (mcdr linkedlist))]
                      [else length]))))
    (f lik)))

(define (linkedlist-ref lik k)
  (letrec ((f (lambda (linkedlist i)
                (if (= i k)
                    (mcar linkedlist)
                    (f (mcdr linkedlist) (+ i 1))))))
    (f lik 0)))

(define (find-node? lik arg)
  (if (equal? lik arg)
      #t
      (if (mpair-iterator-stop? arg)
          #f
          (find-node? (mcdr lik) arg))))

(define (linkedlist-reverse lik)
  (if (null? [mcdr lik])
      (mcons [mcar lik] null)
      (prepend-linkedlist (mcons [mcar lik] null) (linkedlist-reverse [mcdr lik]))))


(define (set-mcar-by-ref! lik ref node)
  (if (>= ref [linkedlist-length lik])
      (error "ref: out of range exception.")
      (letrec ((n (linkedlist-length lik))
               (temp null)
               (f (lambda (i k aux)
                    (if (= i ref)
                        (begin
                          (set! temp [append-linkedlist aux (mcons node [mcdr k])])
                          (set-mcar! lik (mcar temp))
                          (set-mcdr! lik (mcdr temp)))
                        (f (+ i 1) (mcdr k) (append-linkedlist aux (mcons (mcar k) null)))))))
        (f 0 lik null))))


(define (set-mcar-by-value! lik value node)
  (letrec ([f (lambda (k aux)
                (if (mpair-iterator-stop? k)
                    (begin
                      (set-mcar! lik [mcar aux])
                      (set-mcdr! lik [mcdr aux]))
                    (if (equal? value (mcar k))
                        (f [mcdr k] [append-linkedlist aux (mcons node null)])
                        (f [mcdr k] [append-linkedlist aux (mcons (mcar k) null)]))))])
    (f lik null)))


(define (remove-node-by-ref lik ref)
  (letrec ([f (lambda (i k aux)
                (if (mpair-iterator-stop? k)
                    aux
                    (if (= i ref)
                        (f [+ i 1] [mcdr k] aux)
                        (f [+ i 1] [mcdr k] [append-linkedlist aux (mcons [mcar k] null)]))))])
    (f 0 lik null)))


(define (remove-node-by-value lik value)
  (letrec ([f (lambda (k aux)
                (if (mpair-iterator-stop? k)
                    aux
                    (if (equal? [mcar k] value)
                        (f [mcdr k] aux)
                        (f [mcdr k] [append-linkedlist aux (mcons [mcar k] null)]))))])
    (f lik null)))


(define (list->linkedlist list)
  (letrec ((f (lambda (list lik)
                (if (null? list)
                    lik
                    (if (or (null? lik) (mpair? lik))
                        (f (cdr list) (append-linkedlist lik (mcons (car list) null)))
                        (f (cdr list) (append-linkedlist (mcons lik null) (mcons (car list) null))))))))
    (f list null)))

(define (linkedlist->vector lik)
  (letrec ((vec (make-vector (linkedlist-length lik)))
           (f (lambda (lik i) [cond [[not (null? lik)]
                                     (vector-set! vec i (mcar lik))
                                     (f (mcdr lik) (+ i 1))]])))
    (f lik 0)))

(define (linkedlist->string lik)
  (letrec (;(string->char (lambda (s) ()))
           (f (lambda (lik str)
                (if (null? lik)
                    str
                    (begin
                      [cond [[char? (mcar lik)] (f (mcdr lik) (string-append str (make-string 1 (mcar lik))))]
                            [[string? (mcar lik)] (f (mcdr lik) (string-append str (mcar lik)))]
                            [[number? (mcar lik)] (f (mcdr lik) (string-append str (number->string (mcar lik))))]
                            [[symbol? (mcar lik)](f (mcdr lik) (string-append str (symbol->string (mcar lik))))]]
                      )))))
    (f lik "")))