;; Design a table constructor make-table 
;; that takes as an argument a same-key? procedure 
;; that will be used to test “equality” of keys

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records))
           (car records))
          (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup key1 key2)
      (let ((subtable
             (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record
                 (assoc key2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))

    (define (insert! key1 key2 value)
      (let ((subtable
             (assoc key1 (cdr local-table))))
        (if subtable
          (let ((record
                 (assoc key2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key1
                                (cons key2 value))
                          (cdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    
    dispatch))

;; TEST
(define tolerance 0.001)

(define (same-key? x y)
  (if (and (number? x) (number? y))
    (<= (abs (- x y)) tolerance)
    (equal? x y)))

(define my-table (make-table same-key?))

((my-table 'insert!) 'number-key 1 5)

;; should return 5
((my-table 'lookup) 'number-key 1.001) 

;; should return false
((my-table 'lookup) 'number-key 1.05) 
