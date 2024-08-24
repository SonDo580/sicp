;; describe the behavior of the procedure:
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; - take 2 numbers: a and b
;; - return the sum of a and absolute value of b
;; - use an if expression to select the operation to apply

