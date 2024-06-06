;; Modify the 'make-account' procedure of Exercise 3.3 
;; by adding another local state variable so that, 
;; if an account is accessed more than seven consecutive times
;; with an incorrect password, it invokes the procedure 'call-the-cops'

(define (make-account balance creation-password)
  (define consecutive-wrong-password-count 0)
  (define (call-the-cops)
    (error "You're arrested"))
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
      (begin (set! consecutive-wrong-password-count 0)
             (cond ((eq? message 'withdraw) withdraw)
                   ((eq? message 'deposit) deposit)
                   (else (error "Unknown request: MAKE-ACCOUNT" message))))
      (begin (set! consecutive-wrong-password-count
                   (+ consecutive-wrong-password-count 1))
             (if (>= consecutive-wrong-password-count 7)
               (call-the-cops)
               ((error "Incorrect password"))))))
  dispatch)

(define account (make-account 100 'secret-password))

((account 'secret-password 'withdraw) 10) ; 90
((account 'secret-password 'deposit) 20) ; 110

;; Use correct password should reset the counter 
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'secret-password 'withdraw) 10) ; 100

;; Arrested by the police after 7 consecutive incorrect passwords
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; Incorrect password
((account 'wrong-password 'withdraw) 10) ; You're arrested