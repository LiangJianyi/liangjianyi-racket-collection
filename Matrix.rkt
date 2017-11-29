#lang racket

;[b11...b12...b13]   [a11...a12...a13]
;[b21...b22...b23]   [a21...a22...a23]
;[b31...b32...b33]   [a31...a32...a33]
;[b41...b42...b43]
;[b51...b52...b53]
;
;

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