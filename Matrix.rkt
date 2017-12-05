#lang racket

(provide make-matrix)
(provide print-matrix)
(provide matrix-row-count)
(provide matrix-column-count)
(provide matrix-element)
(provide matrix-multiplication)

(define (make-matrix row column . init)
  (define matrix (make-vector column))
  (let ([init-index 0])
    (do ([column-index 0 (+ column-index 1)])
      ([= column-index column] matrix)
      (vector-set! matrix column-index
                   (do ([row-index 0 (+ row-index 1)]
                        [row-vector (make-vector row)])
                     ([= row-index row] row-vector)
                     (if [= init-index (length init)]
                         (vector-set! row-vector row-index 0)
                         (begin
                           (vector-set! row-vector row-index [vector-ref init init-index])
                           (+ init-index 1))))))))

(define (print-matrix matrix)
  (display "{")
  (newline)
  (do ([row-index 0 (+ row-index 1)])
    ([= row-index (matrix-row-count matrix)]
     ;(newline)
     (display "}")
     (newline))
    (display "  [ ")
    (do ([column-index 0 (+ column-index 1)])
      ([= column-index (matrix-column-count matrix)]
       (display "]")
       (newline))
      (display (vector-ref [vector-ref matrix column-index] row-index))
      (display #\space))))

(define (matrix-row-count matrix)
  (vector-length (vector-ref matrix 0)))

(define (matrix-column-count matrix)
  (vector-length matrix))

(define (matrix-element matrix row-index column-index)
  (vector-ref [vector-ref matrix column-index] row-index))

(define (matrix? matrix)
  (if [and (vector? matrix)
           (vector? (vector-ref matrix 0))]
      #t
      #f))

(define (matrix-multipli-with-vector matrix vec)
  (let ([result (make-vector (matrix-row-count matrix))]
        [element 0])
    (do (;[result-index 0 (+ result-index 1)]
         [row-index 0 (+ row-index 1)])
      ([= row-index (matrix-row-count matrix)] result)
      (do ([column-index 0 (+ column-index 1)])
        ([= column-index (matrix-column-count matrix)])
        (set! element (+ element
                         (* (matrix-element matrix row-index column-index)
                            (vector-ref vec column-index))))))))

(define (matrix-multiplication obj1 obj2)
  (cond [(matrix? obj1)
         (cond [(vector? obj2)
                (matrix-multipli-with-vector obj1 obj2)]
               [(matrix? obj2)
                (let ([result (make-vector (matrix-column-count obj2))])
                  (do ([column-index 0 (+ column-index 1)])
                    ([= column-index (matrix-column-count obj2)] result)
                    (vector-set! result column-index
                                 (matrix-multipli-with-vector obj1 [vector-ref obj2 column-index]))))]
               [else "obj2 is not matrix or vector."])]
        ;[]
        [else (error "obj1 is not matrix or vector.")]))