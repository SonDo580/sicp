(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;; Factorial machine           
(controller
   (assign n (op read))
   (assign product (const 1))
   (assign counter (const 1))
 test-counter
   (test (op >) (reg counter) (reg n))
   (branch (label fact-done))
   (assign counter (op +) (reg counter) (const 1))
   (assign product (op *) (reg counter) (reg product))
   (goto (label test-counter))
 fact-done)   