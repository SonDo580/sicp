;; REFERENCE: note 4/metacircular-evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; EXERCISE:
;; Louis Reasoner plans to reorder the cond clauses
;; in eval so that the clause for procedure applications 
;; appears before the clause for assignments. 
;; He argues that this  will make the interpreter more efficient: 
;; Since programsusually contain more applications than 
;; assignments, definitions, and so on, his modified eval will 
;; usually check fewer clauses than the original eval before 
;; identifying the type of an expression

;; a. What is wrong with Louis’s plan?
;; => With Louis’s implementation, all special forms 
;; (assignment, definition, if, lambda, begin, cond) will be
;; evaluated as procedure applications

;; b. Make Louis's evaluator recognize procedure applications 
;; before it checks for most other kinds of expressions.
;; Change the syntax of the evaluated language so that
;; procedure applications start with 'call'

;; Re-order cond clauses of eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else (error "Unknown expression type: EVAL" exp))))

;; Helper: tagged-list
(define (tagged-list exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;; New procedure-application syntax: 
;; (call <operator> <...operands>)
(define (application? exp) (tagged-list exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

;; same
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))