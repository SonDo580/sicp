;; HELPER
(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

;; EXERCISE
(define (mul-stream s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-stream factorials 
                             (integers-from 2))))

;; TEST
(stream-ref factorials 0) ; 1
(stream-ref factorials 1) ; 2
(stream-ref factorials 2) ; 6
(stream-ref factorials 3) ; 24
(stream-ref factorials 4) ; 120
