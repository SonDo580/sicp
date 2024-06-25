;; Suppose we have a procedure called parallel-execute:
;; (parallel-execute ⟨p1⟩ ⟨p2⟩ . . . ⟨pk⟩)
;; 
;; - each ⟨p⟩ is a procedure with no arguments.
;; - parallel-execute creates a separate process for each ⟨p⟩, 
;;   which applies ⟨p⟩.
;; - these processes all run concurrently.

;; Usage example
(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))

;; After the execution is complete, x will be in 5 possible values,
;; depends on the interleaving of P1 and P2.
;; 
;; 101: P1 sets x to 100, then P2 increments x to 101.
;; 121: P2 increments x to 11, then P1 sets x to (* x x)
;; 110: P2 changes x from 10 to 11 between the 2 times that
;;      P1 accesses x during the evaluation of (* x x)
;;  11: P2 accesses x, then P1 sets x to 100, set P2 sets x
;; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x

;; => Solution: use serializer 
;; - takes a procedure as argument and returns a serialized procedure
;;   that behaves like the original procedure.
;; - all calls to a given serializer return serialized procedures 
;;   in the same set 
;; - if some procedure in the set is being executed, 
;;   then a process that attempts to execute any procedure in the set 
;;   will be forced to wait until the first execution has finished
 
(define x 10)
(define s (make-serializer))
(parallel-execute
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (+ x 1)))))
;; This can produce only 2 possible values of x, 101 or 121.
;; (because P1 and P2 can not be interleaved)

;; make-account with serialized withdraw and deposit
;; (See note 3.1/bank-account)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;; => 2 processes can not be withdrawing or depositing 
;;    into a single account concurrently.
;; => Each account has its own serializer, so deposits and withdrawals
;;    for different accounts can proceed concurrently.