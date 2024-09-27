(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; SQRT machine
(controller
   (assign x (op read))
   (assign guess (const 1.0))
 square-iter
   (test (op good-enough?) (reg guess) (reg x))
   (branch (label square-iter-done))
   (assign guess (op improve) (reg guess) (reg x))
   (goto (label square-iter))
 square-iter-done)

;; SQRT machine - expand good-enough? and improve
(controller
   (assign x (op read))
   (assign guess (const 1.0))
 square-iter
 ;; good-enough? expansion
   (assign temp (op *) (reg guess) (reg guess))
   (assign temp (op -) (reg x) (reg temp))
   (test (op >) (reg temp) (const 0))
   (branch (label abs-not-flip))
   (assign temp (op -) (reg temp))
 abs-not-flip
   (test (op <) (reg temp) (const 0.001))
 ;; end good-enough?
   (branch (label square-iter-done))
 ;; improve expansion
   (assign temp (op /) (reg x) (reg guess))
   (assign temp (op +) (reg guess) (reg temp))
   (assign guess (op /) (reg temp) (const 2))
 ;; end improve
   (goto (label square-iter))
 square-iter-done)  