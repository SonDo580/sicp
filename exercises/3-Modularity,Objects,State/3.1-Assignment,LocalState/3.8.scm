;; When we introduce assignment, the order in which the arguments to a
;; procedure are evaluated can make a difference to the result.

;; Define a simple procedure f such that evaluating (+ (f 0) (f 1))
;; will return 0 if the arguments to + are evaluated from left to right 
;; but will return 1 if the arguments are evaluated from right to left.

(define f
  (let ((state -1))
    (lambda (val)
            (if (= state -1)
              (begin (set! state val)
                     val)
              state))))