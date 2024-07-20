;; HELPER
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; EXERCISE
(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
                 (add-streams sums (stream-cdr stream))))
  sums)

;; TEST
(define integers (integers-from 1))
(define int-sums (partial-sums integers))

(stream-ref int-sums 0) ; 1
(stream-ref int-sums 1) ; 3
(stream-ref int-sums 2) ; 6
(stream-ref int-sums 3) ; 10
(stream-ref int-sums 4) ; 15