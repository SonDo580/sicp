(define (make-simplified-withdraw balance)
  (lambda (amount)
          (set! balance (- balance amount))
          balance))

(define W (make-simplified-withdraw 25))
(W 20) ; 5
(W 10) ; -5

;; Without set!
(define (make-decrementer balance)
  (lambda (amount)
          (- balance amount)))

(define D (make-decrementer 25))
(D 20) ; 5
(D 10) ; 15


;; Sameness and change

(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
;; D1 and D2 have the same computational behavior

(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
;; W1 and W2 are not the same because they can have distinct effects

;; 'make-account' procedure from example 3.1/bank-account
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" message))))
  dispatch)

(define peter-acc (make-account 100))
(define paul-acc (make-account 100))
;; => 2 distinct accounts

(define peter-acc (make-account 100))
(define paul-acc peter-acc)
;; => 2 names refer to the same account (aliasing)

;; Factorial calculation 
;; functional style
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* product counter)
            (+ counter 1))))
  (iter 1 1))

;; imperative style
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin (set! product (* counter product))
               (set! counter (+ counter 1))
               (iter))))
    (iter)))
;; => the order of assignments matters
