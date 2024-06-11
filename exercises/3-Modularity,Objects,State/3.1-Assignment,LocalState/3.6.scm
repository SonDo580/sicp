;; Design a new 'rand' procedure:
;; (rand 'generate) produces a new random number; 
;; ((rand 'reset) ⟨new-value⟩) resets the internal state variable to the designated ⟨new-value⟩

;; Reference: Example 3.1/monte-carlo

;; 'rand-update':
;; - start with x1 
;; x2 = (rand-update x1)
;; x3 = (rand-update x2)
;; ...
;; -> a sequence of numbers with statistical properties of uniform distribution

;; (old) 'rand':
;; (define rand (let ((x random-init))
;;                (lambda ()
;;                        (set! x (rand-update x))
;;                        x)))

(define (rand arg)
  (let ((x random-init)) ; random-init could be any number
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value))
    (define (dispatch)
      (cond ((eq? arg 'generate) (generate))
            ((eq? arg 'reset) reset)
            (else (error "Operation not supported" arg))))
    dispatch))