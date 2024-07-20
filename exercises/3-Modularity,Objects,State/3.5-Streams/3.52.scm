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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred
                             (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;; QUESTION:
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(stream-ref y 7)
(display-stream z)

;; 1) What is the value of sum after each of 
;;    the above expressions is evaluated? 

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
;; sum: 0

(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
;; sum: 0 + 1 = 1
;; - accum is applied to only the first element
;; - the rest of the stream hasn't been evaluated

(define y (stream-filter even? seq))
;; sum: 1 + 2 = 3
;; - the first element is odd 
;; -> evaluate (stream-filter pred (stream-cdr seq))
;; - accum is applied to only the second element

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
;; sum: 3 + 4 + 5 = 12
;; - similar reason as y (apply accum util x is divided by 5)

(stream-ref y 7)
;; sum: 12 + 6 + 7 = 25

(display-stream z)
;; sum: 25 + 8 + 9 + ... + 19 + 20 = (1 + 20)*10 = 210

;; 2) What is the printed response to evaluating 
;;    the stream-ref and display-stream expressions? 
(stream-ref y 7)
;; 1 + 2 + 3 = 6
;; 6 + 4 = 10
;; 10 + 5 + 6 + 7 = 28
;; 28 + 8 = 36
;; 36 + 9 = 45
;; 45 + 10 = 55
;; 55 + 11 = 66
;; 66 + 12 = 78 -> answer

(display-stream z)
;; 1 + 2 + 3 + 4 + 5 = 15
;; 15 + 6 + 7 + 8 + 9 + 10 = 55
;; 55 + 11 + 12 + 13 + 14 + 15 = 120
;; 120 + 16 + 17 + 18 + 19 + 20 = 210

;; 3) Would these responses differ if we had 
;;    implemented delay without using the optimization?
;; => Yes, because accum is called multiple times for the same element 
