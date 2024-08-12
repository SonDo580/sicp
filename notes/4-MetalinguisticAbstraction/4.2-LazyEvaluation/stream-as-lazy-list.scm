;; Section 3.5/Streams:
;; - implement streams as delayed lists
;; - use special forms: delay, cons-stream
;; 
;; problems:
;; - a special form is not a first-class object like a procedure,
;;   so we cannot use it with higher-order procedures
;; - the stream is a similar data object but not identical to lists,
;;   so we have to re-implement many list operations (map, append, ...)

;; - With lazy evaluation, streams and lists can be identical.
;;   We only need to make 'cons' non-strict

;; Method 1: extends the lazy evaluator to implement cons as non-strict

;; Method 2 (easier): represent pairs as procedures
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

;; - List operations will work with infinite lists (streams) 
;; as well as finite ones
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))

;; Note:
;; - lazy lists are even lazier than the streams in Section 3
;; - the car and cdr of the list is delayed
;; - accessing the car or cdr of a lazy pair need not force the value
;; - the value will be forced only when it is really needed:
;;   + for use as argument of a primitive procedure
;;   + to be printed as an answer
