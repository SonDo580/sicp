;; We can get by without numbers
;; (insofar as non-negative integers are concerned) 
;; by implementing 0 and the operation of adding 1 as
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Exercise: 
;; - Define one and two directly (not in terms of zero and add1)
;; - Give a direct definition of the addition procedure + 
;; (not in terms of repeated application of add-1)

;; Evaluate 1
;; (add-1 zero)
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f ((zero f) x))))
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f (((lambda (f) 
;;                              (lambda (x) x)) f) 
;;                     x))))
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f ((lambda (x) x)) x)))
;; (lambda (f) (lambda (x) (f x)))

;; Evaluate 2
;; (add-1 one)
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f ((one f) x))))
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f (((lambda (f) 
;;                              (lambda (x) (f x))) f) 
;;                     x))))
;; (lambda (f) 
;;         (lambda (x) 
;;                 (f ((lambda (x) (f x))) x)))
;; (lambda (f) (lambda (x) (f (f x))))

;; => Definitions of 'one' and 'two'
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; TODO:
;; Definition of addition procedure