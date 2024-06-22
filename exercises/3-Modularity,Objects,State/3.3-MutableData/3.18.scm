;; Write a program to detect cycle in a list

;; Functional style
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
  (iter lst '()))

;; With mutation
(define (detect-cycle-2 lst)
  (let ((encountered '()))
    (define (encountered? current encountered-lst)
      (cond ((null? encountered-lst)
             false)
            ((eq? current (car encountered-lst))
             true)
            (else
             (encountered? current (cdr encountered-lst)))))
    (define (iter current)
      (cond ((null? current)
             false)
            ((encountered? current encountered)
             true)
            (else
             (begin (set! encountered (cons current encountered))
                    (iter (cdr current))))))
    (iter lst)))

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
(detect-cycle-2 z1) 

; should be false
(detect-cycle z2) 
(detect-cycle-2 z2) 

; should be false
(detect-cycle z3) 
(detect-cycle-2 z3) 

; should be true
(detect-cycle z4) 
(detect-cycle-2 z4) 
