;; Recursive factorial
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

;; - To use the same machine for computing factorial(n-1), 
;;   we need to save current value of n, to restore it later.
;; - We also need to know where to continue when a call returns.
;; => use a stack (save & restore instructions)

;; Recursive factorial machine
(controller
   (assign continue (label fact-done)) ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ; set up for the recursive call by saving n and continue
   (save continue)
   (save n)
   (assign continue (label after-fact)) ; computation will continue at after-fact when the subroutine returns
   (assign n (op -) (reg n) (const 1))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val)) ; val now contains n*(n-1)!
   (goto (reg continue)) ; return to caller
 base-case
   (assign val (const 1)) ; 1! = 1
   (goto (reg continue)) ; return to caller
 fact-done)

;; DOUBLE RECURSION

;; Recursive Fibonacci
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1) (- n 2)))))

;; Recursive Fibonacci machine
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label base-case))
   ; set up to compute fib(n-1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)
   (assign (op -) (reg n) (const 1))
   (goto (label fib-loop)) ; perform recursive call
 afterfib-n-1 ; now val contains fib(n-1)
   (restore n)
   (restore continue)
   ; set up to compute fib(n-2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val) ; save fib(n-1)
   (goto (label fib-loop))
 afterfib-n-2 ; now val contains fib(n-2)
   (assign n (reg val)) ; now n contains fib(n-2)
   (restore val) ; now val contains fib(n-1)
   (restore continue)
   (assign val (op +) (reg val) (reg n)) ; fib(n) = fib(n-1) + fib(n-2)
   (goto (reg continue)) ; return to caller, answer is in val
 base-case
   (assign val (reg n)) ; fib(n) = n if n < 2
   (goto (reg continue)) ; return to caller
 fib-done)