;; Write a program to detect cycle in a list
;; Requirement: Use constant space

;; => Solution: Use 2 pointers to trarverse the list at different speeds.

(define (detect-cycle lst)
  (define (move-slow pointer)
    (if (null? pointer)
      pointer
      (cdr pointer)))
  
  (define (move-fast pointer)
    (move-slow (move-slow pointer)))
  
  (define (iter slow-pointer fast-pointer)
    (cond ((or (null? slow-pointer)
               (null? fast-pointer))
           false)
          ((eq? slow-pointer fast-pointer)
           true)
          (else
           (iter (move-slow slow-pointer)
                 (move-fast fast-pointer)))))
  
  (iter lst (move-slow lst)))

;; HELPER
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;; TEST
(define z1 (make-cycle (list 'a 'b 'c)))
(define z2 (list 'a 'b 'c))
(define z3 '())
(define z4 (cons 'a z1))

; should be true
(detect-cycle z1)

; should be false
(detect-cycle z2)

; should be false
(detect-cycle z3)

; should be true
(detect-cycle z4)