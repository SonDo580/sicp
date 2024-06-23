;; From exercise 3.27

;; - A memoized procedure maintains a table in which values
;; of previous calls are stored using as keys the arguments
;; that produced the values. 
;; 
;; - When the memoized procedure is asked to compute a value, 
;; it first checks the table to see if the value is already there 
;; and, if so, just returns that value.
;; 
;; - Otherwise, it computes the new value in the ordinary way
;; and stores this in the table.

;; EXAMPLE: calculate fibonacci numbers

;; Helpers to manipulate 1-dimensional table
(define (make-table) (list '*table*))

(define (lookup key table)
  (let ((record 
         (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (insert! key value table)
  (let ((record
         (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table))))))

;; Memoizer
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
            (let ((previously-computed-result
                   (lookup x table)))
              (or previously-computed-result
                  (let ((result (f x)))
                    (insert! x result table)
                    result))))))

;; Memoized procedure
(define memo-fib
  (memoize
   (lambda (n)
           (cond ((= n 0) 0)
                 ((= n 1) 1)
                 (else (+ (memo-fib (- n 1))
                          (memo-fib (- n 2))))))))