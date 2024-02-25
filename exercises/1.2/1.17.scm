;; Assume our language can only add, not multiply
;; We can perform multiplication by repeated addition
;; The algorithm takes a number of steps that is linear in b
(define (slow-mult a b)
  (if (or ( = a 0) (= b 0))
    0
    (+ a (slow-mult a (- b 1)))))

;; Together with addition, we have 'double' and 'halve'
;; (double integer) => 2 * integer
;; (halve even-integer) => even-integer / 2  

;; Design a multiplication procedure that uses a logarithmic number of steps
;; When b is odd, use the formula
;; a * b = (2*a) * ((b - 1 + 1) / 2) =  (2*a) * (b-1)/2) + a
(define (fast-mult a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((= a 1) b)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (recursive-mult (double a) (halve (- b 1)))))))

(define (even? n)
  (= 0 (remainder n 2)))

;; Helpers: double 
(define (double n)
  (* n 2))

;; Helpers: halve (only accept even number)  
(define (halve n)
  (/ n 2))