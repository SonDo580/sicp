;; Define a procedure reverse that takes a list as argument 
;; and returns a list of the same elements in reverse order

(define (reverse items)
  (cond ((or (null? items) 
             (null? (cdr items))) 
         items)
        (else (append (reverse (cdr items))
                    (list (car items))))))

(reverse (list))
(reverse (list 1))
(reverse (list 1 4 9 16 25))