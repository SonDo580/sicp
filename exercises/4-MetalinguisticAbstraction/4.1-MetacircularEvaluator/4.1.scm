;; REFERENCE: note 4/metacircular-evaluator
(define (list-of-values exps env)
  (if (no-operand? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; EXERCISE:
;; Write a version of list-of-values that evaluates operands
;; from left to right regardless of the order of evaluation 
;; in the underlying Lisp.
;; Also write a version of list-of-values that 
;; evaluates operands from right to left.

;; left-to-right
(define (list-of-values exps env)
  (if (no-operand? exps)
    '()
    (let (first-value 
          (eval (first-operand exps) env))
      (cons first-value
            (list-of-values (rest-operands exps) env)))))

;; right-to-left
(define (list-of-values exps env)
  (if (no-operand? exps)
    '()
    (let (rest-values 
          (list-of-values (rest-operands exps) env))
      (cons (eval (first-operand exps) env)
            rest-values))))