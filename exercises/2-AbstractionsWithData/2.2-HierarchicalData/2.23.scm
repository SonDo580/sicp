;; Implement for-each procedure
;; (apply a procedure to each item of the list)

(define (for-each proc items)
  (define (action remaining)
    (proc (car remaining))
    (for-each proc (cdr items)))
  (if (null? items)
    true
    (action items)))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))