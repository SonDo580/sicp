;; Original version: note 3.5/iteration-with-stream
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                                     (sqrt-improve guess x))
                             guesses)))
  guesses)

;; Louis Reasoner's version
(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                            (sqrt-improve guess x))
                    (sqrt-stream x))))
;; Analysis:
;; - this version is less efficient because it doesn't leverage memoization
;; - it improves the first guess to get the second guess, 
;;   but then does it again when improving the second guess. 
;;   (sqrt-improve is called multiple times to produce the same guess)

;; If delay is implemented without memoization,
;; the efficiency of both implementations are the same.
