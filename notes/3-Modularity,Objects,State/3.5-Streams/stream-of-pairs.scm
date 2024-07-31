;; produce the stream of pairs of all integers
;; (i, j) with i <= j such that i + j is prime.

;; combine 2 streams with append
(define (stream-append s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (stream-append (stream-cdr s1) s2))))
;; - This is not suitable for infinite stream
;; - We need to alternate between items of the 2 streams

;; the appropriate way: combine 2 streams with interleave
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

;; procedure to produce the needed stream of pairs
(define (pairs s t)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (interleave (stream-map (lambda (x)
                                               (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s)
                                  (stream-cdr t)))))

;; integers sequence - From note 3.5/infinite-seq
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))

;; pairs of all integers(i, j) with i <= j
(define int-pairs (pairs integers integers))

;; needed sequence
(define prime-sum-pairs
  (stream-filter (lambda (pair)
                         (prime? (+ (car pair) (cadr pair))))
                 int-pairs))