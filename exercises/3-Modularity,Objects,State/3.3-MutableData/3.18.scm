;; Write a program to detect cycle in a list

(define (detect-cycle lst)
  (define (encountered? current encountered)
    (cond ((null? encountered) 
           false)
          ((eq? current (car encountered)) 
           true)
          (else 
           (encountered? current (cdr encountered)))))
  (define (iter current encountered)
    (cond ((null? current) 
           false)
          ((encountered? current encountered) 
           true)
          (else (iter (cdr current) (cons current encountered)))))
  (if (null? lst)
    false
    (iter lst '())))

;; Test
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

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
