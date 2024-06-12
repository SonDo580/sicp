;; Consider the bank account objects created by make-account, 
;; with the password modification described in Exercise 3.3. 

;; Suppose that our banking system requires the ability 
;; to make joint accounts. 

;; Define a procedure make-joint that create an additional access 
;; to the original account using the new password. 

;; make-joint should take 3 arguments.
;; - a password-protected account.
;; - the password with which the account was defined.
;; - a new password.

;; From exercise 3.3
(define (make-account balance creation-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password message)
    (if (eq? password creation-password)
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT" message)))
      (error "Incorrect password")))
  dispatch)


(define (make-joint account creation-password new-password)
  (define withdraw (account creation-password 'withdraw))
  (define deposit (account creation-password 'deposit))
  (define (dispatch password message)
     (if (eq? password new-password)
      (cond ((eq? message 'withdraw) withdraw)
            ((eq? message 'deposit) deposit)
            (else (error "Unknown request: MAKE-JOINT" message)))
      (error "Incorrect password")))
  dispatch)

;; TEST
(define peter-acc (make-account 100 'peter))

(define paul-acc (make-joint peter-acc 'wrong 'paul)) ; "Incorrect password"
(define paul-acc (make-joint peter-acc 'peter 'paul))

((paul-acc 'wrong 'deposit) 50) ; "Incorrect password"
((paul-acc 'peter 'deposit) 50) ; "Incorrect password"
((paul-acc 'paul 'deposit) 50) ; 150
((paul-acc 'paul 'withdraw) 100) ; 50
((peter-acc 'peter 'withdraw) 20) ; 30


