;; Write a procedure triples that takes 3 infinite streams S, T, U
;; and produces the stream of triples (Si, Tj, Uk) such that i ≤ j ≤ k. 
;; Use triples to generate the stream of all Pythagorean triples 
;; of positive integers, i.e., the triples (i, j, k) such that 
;; i ≤ j and i^2 + j^2 = k^2

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream (list (stream-car s)
                     (stream-car t))
               (interleave (stream-map (lambda (x)
                                               (list (stream-car s) x))
                                       (stream-cdr t))
                           (pairs (stream-cdr s)
                                  (stream-cdr t)))))

(define (triples s1 s2 s3)
  (cons-stream (list (stream-car s1)
                     (stream-car s2)
                     (stream-car s3))
               (interleave (stream-map (lambda (pair)
                                               (cons (stream-car s1) pair))
                                       (stream-cdr (pairs s2 s3)))
                           (triples (stream-cdr s1)
                                    (stream-cdr s2)
                                    (stream-cdr s3)))))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))
(define integers (integers-from 1))

(define int-triples (triples integers integers integers))

(define (is-pythagorean-triple? triple)
  (let ((i (car triple))
        (j (cadr triple))
        (k (caddr triple)))
    (= (+ (square i) (square j))
       (square k))))

(define pythagorean-triples
  (stream-filter (lambda (triple)
                         (is-pythagorean-triple? triple))
                 int-triples))

;; TEST
;; (3 4 5)
(stream-car pythagorean-triples)

;; (6 8 10)
(stream-car (stream-cdr pythagorean-triples))
