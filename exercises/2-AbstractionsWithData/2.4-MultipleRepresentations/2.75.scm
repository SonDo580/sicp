;; Implement the constructor make-from-mag-ang in message-passing style

;; Helper
(define (apply-generic op arg) (arg op))

;; Main
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)
