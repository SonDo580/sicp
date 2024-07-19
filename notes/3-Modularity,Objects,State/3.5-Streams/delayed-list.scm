(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
           (iter (+ count 1) (+ accum count)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime?
                      (enumeration-interval a b))))

;; The first program doesn't need large intermediate storage.
;; In the second program:
;; - enumeration-interval must construct a complete list.
;; - filter generate another list.

;; Using stream:
;; - Construct the stream only partially, and pass the partial construction
;;   to a program that consumes the stream.
;; - If the consumer attempts to access part of the stream that has not been
;;   constructed, the stream will construct enough more of itself.

(stream-car (cons-stream x y)) ; => x
(stream-cdr (cons-stream x y)) ; => y

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;; - The cdr of a stream is evaluated when it is accessed by stream-cdr
;;   rather than when the stream is constructed by cons-stream
;; 
;; - Stream implementation is based on:
;; . delay: does not evaluate the expression but returns a delayed object.
;; . force: takes a delayed object and performs the evaluation.

(cons-stream ⟨a⟩ ⟨b⟩) <=> (cons ⟨a⟩ (delay ⟨b⟩))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; Stream implementation in action
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low
                 (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred
                             (stream-cdr stream)))))

;; Implement delay and force
(delay ⟨exp⟩) <=> (lambda () ⟨exp⟩)
(define (force delayed-object) (delayed-object))

;; Optimization: implement delay as a memoized procedure
(define (memo-proc proc)
  (let ((already-run? false)
        (result false))
    (lambda ()
            (if (not already-run?)
              (begin (set! result (proc))
                     (set! already-run? true)
                     result)
              result))))

(delay ⟨exp⟩) <=> (memo-proc (lambda () ⟨exp⟩))