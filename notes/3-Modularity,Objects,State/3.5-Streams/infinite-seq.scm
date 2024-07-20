;; Positive integers
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) 
                 integers))

(stream-ref no-sevens 100) ; 117

;; Fibonacci numbers
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fib (fibgen 0 1))

;; Sieve of Eratosthenes:
;; - Start with the integers stream from 2
;; - Filter multiples of 2 from the rest of the stream
;; - This leaves a stream beginning with 3 (the next prime)
;; - Now we filter multiples of 3 from the rest of the stream
;; - And so on...

(define (sieve stream)
  (cons-stream 
   (stream-car stream)
   (sieve (stream-filter (lambda (x)
                                 (not (divisible? x 
                                                  (stream-car stream))))
                         (stream-cdr stream)))))

(define primes (sieve (integers-from 2))) 
(stream-ref primes 50) ; 233

;; Define streams implicitly
(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers 
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0 
               (cons-stream 1 (add-streams (stream-cdr fibs)
                                           fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define power-of-twos 
  (cons-stream 1 (scale-stream power-of-twos 2)))



(define primes
  (cons-stream 2
               (stream-filter prime?
                              (integers-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n)
           true)
          ((divisible? n (stream-car ps))
           false)
          (else (iter (stream-cdr ps)))))
  (iter primes))