;; From note 3.4/multiple-shared-resources
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; Even with serialized-exchange, there still be a problem
;; 
;; Scenario:
;; - A attempts to exchange a1 with a2
;; - B attempts to exchange a2 with a1
;; - A's process enters a serialized procedure protecting a1, just after that,
;; B's process enters a serialized procedure protecting a2
;; - Now A cannot proceed to enter a serialized procedure protecting a2,
;; until B exits the serialized procedure protecting a2
;; - B's situation is similar.
;; => DEADLOCK

;; Solution:
;; - Give each account a unique identification number and rewrite
;; serialized-exchange so that a process with always attempt to
;; enter a procedure protecting the lower-numbered account first.
;; - There are other situations that require more sophisticated 
;; deadlock-avoidance techniques.