;; Metacircular evaluator: 
;; an evaluator that is written in the same language that it evaluates

;; 1) CORE

;; Eval
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

;; Apply
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence 
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; Procedure arguments
(define (list-of-values exps env)
  (if (no-operand? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

;; Conditional
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; Assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! 
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; 2) REPRESENTING EXPRESSIONS

;; Self-evaluating: numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; Variable: represented by symbols
(define (variable? exp) (symbol? exp))

;; Quotation: '<text> <-> (quote <text>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;; Assignment: (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; Definition: 2 forms
;; 1. (define <var> <value>)
;; 2. (define (<var> <...params>) 
;;         <body>)
;; 
;; form 2 is syntactic sugar for:
;; (define <var>
;;         (lambda (<...params>)
;;                 <body>))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp) ; form 1
    (caadr exp))) ; form 2
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp) ; form 1
    (make-lambda (cdadr exp) ; parameters
                 (cddr exp)))) ; body

;; Lambda expression
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; lambda expression constructor (used by definition-value)
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; If: (if <predicate> <consequent> <alternative>?)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

;; if expression constructor (used by cond->if)
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; Begin: (begin <...expressions>)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; constructor sequence->exp (used by cond->if)
;; transform a sequence into a single expression
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; Procedure applications: (<operator> <...operands>)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; DERIVED EXPRESSIONS

;; Cond: (cond <...clauses>)
;; clause format: (<predicate>/else <...expresions>)
;; => transformed to nested if
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false ; no else clause
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF" clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-action first))
                 (expand-clauses rest))))))

;; 3) DATA STRUCTURES

;; Testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; Compound procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on environment

;; Represent an environment as a list of frame
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Represent each frame of an environment as a pair of lists
;; - a list of the variables bound in that frame
;; - a list of the associated values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) 
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) 
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; 4) RUNNING THE EVALUATOR AS A PROGRAM

;; Setup global environment
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

;; Primitive procedure: ('primitive <underlying-Lisp-procedure>)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?))) 
        ;; define more primitives
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitives-procedure-objects)
  (map (lambda (proc)
               (list 'primitive (cadr proc)))
       primitive-procedures))

;; apply-in-underlying-scheme: the underlying apply
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme 
   (primitive-implementation proc) args))

;; Driver loop: model the read-eval-print loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (annouce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (annouce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (display object)))
;; (driver-loop)

;; 5) DATA AS PROGRAMS
;; 6) INTERNAL DEFINITIONS

;; 7) SEPARATE SYNTACTIC ANALYSIS
;; - split eval into 2 parts: analysis and execution
;; - analyze performs the syntactic analysis and return an execution procedure
;; - the execution procedure takes an environment and completes the evaluation
;; - analyze will be called only once on an expression
;; - the execution procedure may be called multiple times

(define (eval exp env) ((analyze exp) env))

;; Analyze: 
;; - use the same cases as the old eval
;; - just perform analysis, not full evaluation
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lamda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

;; looking up variables is done in the execution phase
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;; defer setting the variable until the execution
;; however assignment/definition-value can be analyzed only once
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
            (set-variable-value! var (vproc env) env)
            'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
            (define-variable! var (vproc env) env)
            'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
            (if (true? (pproc env))
              (cproc env)
              (aproc env)))))

;; major gain: lambda body can be analyzed only once
;; (the resulting procedure can be applied multiple times)
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
            (make-procedure vars bproc env)))) 

;; - each expressions is analyzed, yielding an execution procedure
;; - the procedures are combined to produce an execution procedure
;;   that sequentially calls each individual procedure 
(define (analyze-sequece exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop 
       (sequentially first-proc (car rest-procs))
        (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
            (execute-application
             (fproc env)
             (map (lambda (aproc)
                          (aproc env))
                  aprocs)))))

;; execute-application is similar to apply in Section 1 (CORE)
;; but the procedure body for a compound procedure has already been analyzed
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else "Unknown procedure type: EXECUTE-APPLICATION" proc)))
