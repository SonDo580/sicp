(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

;; Encapsulate variable 'balance' in 'new-withdraw' procedure  
(define (new-withdraw amount)
  (let (balance 100)
    (lambda (amount)
            (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds"))))

;; Create 'withdraw processor'
(define (make-withdraw balance)
  (lambda (amount)
          (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50) ; 50
(W2 70) ; 30

;; Represent a bank account object
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

(define account (make-account 100))
((account 'withdraw) 50) ; 50
((account 'withdraw) 60) ; Insufficient funds
((account 'deposit) 40) ; 90