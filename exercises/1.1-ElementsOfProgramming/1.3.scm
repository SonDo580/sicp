;; Define a procedure that takes 3 numbers as arguments
;; and returns the sum of the squares of the 2 larger numbers.

(define (square x)
  (* x x))

(define (sumSquares x y)
  (+ (square x) (square y)))

(define (sumTwoLargestSquares x y z)
  (cond ((isSmallest x y z) (sumSquares y z))
        ((isSmallest y x z) (sumSquares x z))
        (else (sumSquares x y))))

(define (isSmallest x y z)
  (and (< x y) (< x z)))

;; Usage
(sumTwoLargestSquares 1 2 3)
(sumTwoLargestSquares 5 2 4)
