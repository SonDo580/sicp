;; Find the first record that has the given key as its car
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records))
         (car records))
        (else (assoc key (cdr records)))))

;; Create a table
(define (make-table) (list '*table*))

;; ONE-DIMENSIONAL TABLE
;; Look up a value in table by key
(define (lookup key table)
  (let ((record 
         (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

;; Insert/Update a value under a key
(define (insert! key value table)
  (let ((record
         (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table))))))

;; TWO-DIMENSIONAL TABLE
(define (lookup key1 key2 table)
  (let ((subtable
         (assoc key1 (cdr table))))
    (if subtable
      (let ((record
             assoc key2 (cdr subtable)))
        (if record
          (cdr record)
          false))
      false)))

(define (insert! key1 key2 value table)
  (let ((subtable
         (assoc key1 (cdr table))))
    (if subtable
      (let ((record
             (assoc key2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key1
                            (cons key2 value))
                      (cdr table)))))
  'ok)

;; LOCAL TABLE (represent table as an object)
(define (make-table)
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
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    
    dispatch))

;; Implement get/put (See note 2.4/data-directed-programming)
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; TEST
(put 'letters 'a 97)

;; should return 97
(get 'letters 'a)

;; should return false
(get 'letters 'b)