;; Helpers
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
                 (add-streams sums (stream-cdr stream))))
  sums)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

;; Generate sequence of better and better square root guesses 
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                                     (sqrt-improve guess x))
                             guesses)))
  guesses)

;; (display-stream (sqrt-stream 2))

;; Generate an approximation to pi
;; pi/4 = 1 - 1/3 + 1/5 - 1/7 + ...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1))
                4))

;; (display-stream pi-stream)

;; Sequence accelarator:
;; - converts a sequence of approximation to a new sequence 
;;   that converges to the same value as the original but faster.

;; Euler's technique:
;; (works well with partial sums of alternating series)
;; - If Sn is the nth term of the original sum sequence,
;;   then the accelarated sequence has terms:
;;   S(n+1) - (S(n+1) - Sn)^2 / (S(n-1) - 2Sn + S(n+1))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S(n-1)
        (s1 (stream-ref s 1))  ; Sn
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; (display-stream (euler-transform pi-stream))

;; We can continue accelerating the accelarated sequences
;; -> Create a stream of streams (tableau) in which
;;    each stream is the transform of the preceding one
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

;; Form a sequence by taking the first term in each row of the tableau
(define (accelarated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; (display-stream (accelarated-sequence euler-transform pi-stream))