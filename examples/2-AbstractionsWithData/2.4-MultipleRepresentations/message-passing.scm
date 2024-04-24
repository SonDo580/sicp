;; data-directed-programming.scm:
;; - use a operation and type table

;; tagged-data.scm
;; - use dispatching on type: each operation takes care of its own dispatching
;; -> decompose the table into rows

;; Message passing: 
;; - an alternative strategy is to decompose the table into columns
;; - data object is represented as a procedure that takes as input
;;   the operation name and performs the operation indicated

;; Example

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magniture) (sqrt (+ square x) (+ square y)))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;; Apply operation to an argument: feed the operation name to the data object
(define (apply-generic op arg) (arg op))

;; Limitation:
;; - only permits generic procedures of one argument