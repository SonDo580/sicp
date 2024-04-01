;; Define a better version of make-rat that handles both 
;; positive and negative arguments. make-rat should
;; normalize the sign so that if the rational number is positive,
;; both the numerator and denominator are positive, and if
;; the rational number is negative, only the numerator is negative.

(define (negative-rat? n d)
  (or (and (< n 0) (> d 0))
      (and (> n 0) (< d 0))))

(define (make-rat n d)
  (let ((g (gcd n d))
        (processed-n (if (negative-rat? n d)
              (- (abs n))
              (abs n)))
        (processed-d (abs d)))
    (cons (/ processed-n g) (/ processed-d g))))

;; Helpers
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Usages
(define rat1 (make-rat 1 3))
(define rat2 (make-rat -1 3))
(define rat3 (make-rat 1 -3))
(define rat4 (make-rat -1 -3))

(print-rat rat1)
(print-rat rat2)
(print-rat rat3)
(print-rat rat4)
