;; The Fibonacci machine’s controller sequence 
;; has an extra save and an extra restore, 
;; which can be removed to make a faster machine

;; Recursive Fibonacci machine (note 5.1/recursion)
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label base-case))
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)
   (assign (op -) (reg n) (const 1))
   (goto (label fib-loop))
 afterfib-n-1
   (restore n)
   (restore continue) ; remove this line
   (assign n (op -) (reg n) (const 2))
   (save continue) ; remove this line
   (assign continue (label afterfib-n-2))
   (save val)
   (goto (label fib-loop))
 afterfib-n-2 
   (assign n (reg val))
   (restore val)
   (restore continue)
   (assign val (op +) (reg val) (reg n)) 
   (goto (reg continue))
 base-case
   (assign val (reg n))
   (goto (reg continue))
 fib-done)