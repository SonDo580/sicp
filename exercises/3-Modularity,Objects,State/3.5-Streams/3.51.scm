;; HELPERS:
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (display-line x) (newline) (display x))

(define (show x) 
  (display-line x)
  x)

;; QUESTION:
;; What does the interpreter print in response to 
;; evaluating each expression in the following sequence

(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
;; 0 
;; (because the first element is not delayed)

(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5

(stream-ref x 7)

;; If delay is a memoized procedure:
;; 6
;; 7
;; 
;; Else:
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7