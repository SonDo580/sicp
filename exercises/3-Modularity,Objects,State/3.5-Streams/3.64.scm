;; REFERENCE:
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
  
;; EXERCISE:
;; stream-limit examines the stream until it finds 
;; 2 successive elements that differ by less than the tolerance,
;; and returns the second element.
(define (acceptable? first second tolerance)
  (< (abs (- first second))
     tolerance))

(define (stream-limit stream tolerance)
  (let ((first (stream-car stream))
        (second (stream-car (stream-cdr stream))))
    (if (acceptable? first second tolerance)
      second
      (stream-limit (stream-cdr stream)
                    tolerance))))
  
;; TEST:
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.001)
(sqrt 3 0.001)
(sqrt 5 0.001)