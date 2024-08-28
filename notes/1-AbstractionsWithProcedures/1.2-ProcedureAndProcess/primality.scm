;; 1. SEARCHING FOR DIVISOR

;; - if n is not prime it must have a divisor less than or equal to sqrt(n)
;;   (if d is a divisor of n, then so is n/d. but d and n/d cannot both
;;    be greater than sqrt(n)) 
;; -> number of steps will have order of growth O(sqrt(n)) 

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; 2. FERMAT TEST

;; Fermat's Little Theorem:
;; - If n is a prime number and a is any positive integer less than n, 
;;   then a raised to the nth power is congruent to a modulo n
;;   (2 numbers are said to be congruent modulo n if they both have 
;;    the same remainder when divided by n)

;; => Fermat test:
;; -  Given a number n, pick a random number a < n
;; -  Compute the remainder of a^n mod n. 
;;    + If the result is not equal to a, then n is certainly not prime. 
;;    + If it is a, then chances are good that n is prime. 
;; - Repeat the test a number of times to be certain.

;; compute base^exp mod m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base 
                                    (/ exp 2) 
                                    m))
                    m))
        (else
         (remainder (* base
                       (expmod base
                               (- exp 1)
                               m))
                    m))))
;; - this uses successive squaring, similar to the fast-expt procedure
;;   (see note 1.2/exponentiation)
;; - it is based on the fact: 
;;   x.y mod m = (x mod m).(y mod m)
;; 
;; Proof: 
;; x mod m = rx -> x = kx.m + rx
;; y mod m = ry -> y = ky.m + ry
;; x.y = (kx.m + rx).(ky.m + ry) = (kx.ky.m + ky.rx + kx.ry).m + rx.ry
;; -> x.y mod m = rx.ry = (x mod m).(y mod m)
;; -> b^e mod m = (b^(e/2) mod m)^2     (when e is even)

;; - assume that we have the primitive procedure 'random', that returns
;;   a non-negative integer less than its integer input

;; Ferma test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

;; Run the test a given number of times.
;; Returns true if the test succeeds every time, and false other wise.
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; PROBABILISTIC METHODS

;; - In Fermat test, the answer obtained is only probably correct.
;; - If n fails the Fermat test, we can be certain that n is not prime.
;;   But n passing the test is not a guarantee that n is prime.
;; - There are numbers that fool the Ferma test (Carmichael numbers). 
;;   a is not prime, a^n is congruent to a modulo n for all integers a < n
;;   (561, 1105, 1729, 2465, 2821, 6601,...)
;; - These numbers are extremely rare, so the chance of stumbling upon 
;;   them in Fermat test is very low.