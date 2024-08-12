;; REFERENCE: note 4.1/metacircular-evaluator

;; SEPARATE SYNTACTIC ANALYSIS:
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
        ((lambda? exp) (analyze-lambda exp))
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
(define (analyze-sequence exps)
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