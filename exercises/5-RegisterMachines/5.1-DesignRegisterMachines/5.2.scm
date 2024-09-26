(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;; Factorial machine           
(controller
 (assign product (const 1))
 (assign counter (const 1))
 test-counter
 (test (op >) (reg counter) (reg n))
 (branch (label fact-done))
 (assign counter (op inc) (reg counter))
 (assign product (op mul) (reg counter) (reg product))
 (goto (label test-counter))
 fact-done)   