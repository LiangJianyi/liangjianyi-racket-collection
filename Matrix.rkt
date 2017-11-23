#lang racket

(define (make-matrix row column . init)
  (define matrix (make-vector column))
  (let ([init-index 0])
    (do ([column-index 0 (+ column-index 1)])
      ([= column-index column] matrix)
      (do ([row-index 0 (+ row-index 1)])
        ([= row-index row])
        (if [> (length init) 0]
            (begin
              (vector-set! matrix column-index (vector-append
                                                (vector-ref matrix column-index)
                                                (list-ref init init-index)))
              (set! init-index [+ init-index 1]))
            (vector-set! matrix column-index (vector-append 
                                              (vector-ref matrix column-index)
                                              0)))))))