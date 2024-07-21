;; REFERENCE
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (define sums
    (cons-stream (stream-car stream)
                 (add-streams sums (stream-cdr stream))))
  sums)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))  ; S(n-1)
        (s1 (stream-ref s 1))  ; Sn
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelarated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; EXERCISE
;; Generate 3 sequences of approximation to ln2
;; ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream-0
  (partial-sums (ln2-summands 1)))

(define ln2-stream-1
  (euler-transform ln2-stream-0))

(define ln2-stream-2
  (accelarated-sequence euler-transform ln2-stream-0))
