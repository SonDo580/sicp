;; Modify the 'make-account' procedure so that
;; it creates password-protected accounts
;; The resulting account object should process a request only
;; if it is accompanied by the password with which the account was created

;; Reference: Example 3.1/bank-account
;; (define (make-account balance)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;       (begin (set! balance (- balance amount))
;;              balance)
;;       "Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (define (dispatch message)
;;     (cond ((eq? message 'withdraw) withdraw)
;;           ((eq? message 'deposit) deposit)
;;           (else (error "Unknown request: MAKE-ACCOUNT" message))))
;;   dispatch)

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

;; Usage
(define account (make-account 100 'secret-password))

((account 'secret-password 'withdraw) 40) ; 60
((account 'other-password 'deposit) 50) ; Incorrect password