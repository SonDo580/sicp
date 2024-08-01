;; OOP style
(define (make-simplified-withdraw balance)
  (lambda (amount)
          (set! balance (- balance amount))
          balance))

;; Stream style
(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (- balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))

;; - From the user perspective, the stream process has the same behavior
;; as the object created by make-simplified-withdraw.
;; - There is no assignment, no local state variable,... 
;; Yet the system has state

;; - The stream model still suffers from time-related problems
;; - For example, if the system permits join bank accounts,
;; we have to 'merge' multiple amount-streams.
