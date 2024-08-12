;; - When applying a procedure, the interpreter must determine which 
;; arguments are to be evaluated and which are to be delayed
;; - The delayed arguments are not evaluated, they are transformed into objects 
;; called 'thunks'
;; - The thunk must contain the information required to produce the value of 
;; the argument when it is needed (argument expression + environment)
;; - The process of evaluating the expression in a thunk is called 'forcing'

;; REFERENCE: notes 4/metacircular-evaluator

;; modified 'eval' (application? ...)
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
        ((application? exp) (apply (actual-value (operator exp) env)
                                   (operands exp) 
                                   env))
        (else (error "Unknown expression type: EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

;; modified 'apply':
;; - primitive procedure: evaluate all arguments before applying
;; - compound procedure: delay all arguments before applying
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure 
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (list-of-delayed-args arguments env) ; changed
          (procedure-environment procedure)))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps)
                        env)
          (list-of-arg-values (rest-operands exps)
                              env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps)
                    env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

;; modified if handling:
;; - use actual-value instead of eval to get the value of the predicate
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;; modified the driver loop:
;; - use actual value instead of eval, so if a delayed value is propagated
;; back to the read-eval-print loop, it will be forced before being printed
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; REPRESENTING THUNKS
(define (force-it obj)
  (if (thunk? obj)
    (actual-value (thunk-exp obj) (thunk-env obj))
    obj))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; With memoization
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '()) ; forget unneeded env 
           result)) 
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))