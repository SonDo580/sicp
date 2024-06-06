;; Define a procedure that generate accumulators,
;; each maintaining an independent sum

(define (make-accumulator initial-value)
  (lambda (value)
          (begin (set! initial-value (+ initial-value value))
                 initial-value)))

(define A (make-accumulator 5))
(A 10) ; 15
(A 10) ; 25