;; Extend Scheme to support nonderterminism

;; - The expression (amb <e1> ... <en>) returns the value of one of 
;;   the n expressions "ambiguously"
;; - Example:
;; (list (amb 1 2 3) (amb 'a 'b)) can have 6 posible values
;; (1 a) (1 b) (2 a) (2 b) (3 a) (3 b)
;; - amb with no choices (amb) cause the evaluation to "fail"
;; - amb represents a nondeterministic choice point

;; require that a predicate expression must be true
(define (require p) (if (not p) (amb)))

;; check if an item is in the list
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; return any integer greater than or equal to n
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; If we have a machine with a sufficient number of processors 
;; that cound be dynamically allocated: 
;; - execution proceed as in a sequential machine
;; - when an amb expression is encountered, more processors are 
;;   allocated to continue all the parallel executions
;; - each processor would proceed sequentially until it encounters
;;   a failure, furthur subdivides, or finishes

;; If we have a machine that can execute only 1 process, 
;; or a few concurrent processes, 
;; - we must consider the alternatives sequentially
;;   (systematically search all possible execution paths)
;; 
;; depth-first search (chronological backtracking):
;; - always try the first alternative
;; - if a choice results in a failure, the evaluator backtracks to 
;;   the most recent choice point and try the next alternative
;; - if it runs out of alternatives at any choice point, backup to
;;   the previous choice point and resume from there

;; DRIVER LOOP of amb evaluator
;; - reads an expression and return the value of the first 
;;   non-failing execution
;; - try-again 
;;   -> ask the interpreter to backtrack and attempt
;;      to generate a second non-failing execution
;; - if any expresion except try-again is given 
;;   -> start a new problem and discard the unexplored alternatives 
;;      in the previous problem
