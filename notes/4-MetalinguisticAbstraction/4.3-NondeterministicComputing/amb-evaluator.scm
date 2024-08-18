;; Construct the amb evaluator by modifying the analyzing evaluator
;; REFERENCE: 4.1/analyze-evaluator

;; EXECUTION PROCEDURE & CONTINUATION
;; - The execution procedures of the ordinary evaluator takes 1 argument:
;;   the environment of execution
;; - The execution procedures of the amb evaluator take 3 arguments:
;;   the environment, and 2 continuation procedures
;; - The evaluation of an expression will finish by calling 1 of 
;;   these 2 continuations
;;   + if the evaluation results in a value, the success continuation
;;     is called with that value
;;   + if the evaluation results in a dead end, the failure continuation
;;     is called
;; - The success continuation receives a value and proceed with the computation.
;;   Along with the value, it is passed another failure continuation to 
;;   be called if the use of that value leads to a dead end
;; - When a failure is triggered during evaluation, the failure continuation
;;   in hand at that point will cause the most recent choice point to
;;   choose another alternative.
;;   If there are no more alternatives at that point, a failure at an 
;;   earlier choice point is triggered, and so on.
;;   Failure continuation are also invoked by the driver loop in response
;;   to a try-again request.
;;   A side-effect operation needs to produce a failure continuation
;;   that undoes the side effect.

;; FAILURE CONTINUATIONS SUMMARY
;; 1. Failure continuations are constructed by:
;; - amb expressions: to provide a mechanism to make alternative choices
;;   if the current choice leads to a dead end
;; - top-level driver: to provide a mechanism to report failure when
;;   the choices are exhausted
;; - assignments: to intercept failures and undo assignments
;; 
;; 2. A dead end is encountered when:
;; - the program executes (amb)
;; - the user types 'try-again' at the top-level driver
;; 
;; 3. Failure continuations are also called during processing of a failure
;; - When the failure continuation created by an assignment finishes
;;   undoing a side effect, it calls the failure continuation it 
;;   intercepted to propagate the failure back to the choice point
;;   that led to this assignment or to the top level
;; - When the failure continuation for an amb runs out of choices,
;;   it calls the failure continuation that was given to the amb
;;   to propagate the failure back to the previous choice point 
;;   or to the top level

;; STRUCTURE OF THE AMB EVALUATOR

;; amb
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; (modified) analyze
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
        ((amb? exp) (analyze amb exp)) ; added
        (else (error "Unknown expression type: ANALYZE" exp))))

;; ambeval (similar to eval)
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; The general form of an execution procedure is
;; (lambda (env succeed fail)
;;         succeed is (lambda (value fail) ...)
;;         fail is (lambda () ...)
;;         ...)

;; SIMPLE EXPRESSION

;; The resulting execution procedures simply succeed with the value 
;; of the expression, passing along the failure continuation that
;; was passed to them

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
          (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
            (succeed exp fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
          (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env) fail))))

;; CONDITIONALS & SEQUENCES

;; - fail2 maybe the same fail continuation
;; - but if the predicate involves an amb expression, 
;;   it will generate a new failure continuation
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lamda (env succeed fail)
           (pproc env
                  ;; success continuation for evaluating the predicate
                  (lambda (pred-value fail2)
                          (if (true? pred-value)
                            (cproc env succeed fail2)
                            (aproc env succeed fail2)))
                  ;; failure continuation for evaluating the predicate
                  fail))))
;; Example: 
;; > (if (amb true false)
;;     (amb 1)
;;     (amb 2))
;; 1
;; > try-again
;; 2
;; 
;; Explanation:
;; - the first amb return true, so 'if' evaluates the consequent clause,
;;   and that second amb returns 1.
;; - when the user 'try-again', there are no more value for that amb 
;;   to return, so it fails. 
;;   the first amb is re-evaluated, this time it returns false, so 'if'
;;   evaluates the alternative clause.
;; - fail2 is the contination that re-evaluates the predicate amb
;;   (analyze-amb shows how it is constructed).

;; To sequentially execute a and then b, 
;; we call a with a success continuation that calls b
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
            (a env
               ;; success continuation for calling a
               (lambda (a-value fail2)
                       (b env succeed fail2))
               ;; failure continuation for calling a
               fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc
                          (car rest-procs))
        (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE")
      (loop (car procs) (cdr procs)))))

;; DEFINITIONS & ASSIGNMENTS

;; If the execution of vproc succeeds, 
;; the variable is defined and the success is propagated 
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                           (define-variable! var val env)
                           (succeed 'ok fail2))
                   fail))))

;; - give vproc a success continuation that saves the old value
;;   of the variable before assigning the new value
;; - the failure continuation that is passed along with the value
;;   of the assignment restores that old value of the variable
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                           (let ((old-value 
                                  (lookup-variable-value var env)))
                             (set-variable-value! var val env)
                             (succeed 'ok
                                      (lambda ()
                                              (set-variable-value!
                                               var
                                               old-value
                                               env)
                                              (fail2)))))
                   fail))))

;; PROCEDURE APPLICATIONS

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
            (fproc env
                   (lambda (proc fail2)
                           (get-args 
                            aprocs
                            env
                            (lambda (args fail3)
                                    (execute-application
                                     proc args succeed fail3))
                            fail2))
                   fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs)
     env
     ;; success continuation for this aproc
     (lambda (arg fail2)
             (get-args
              (cdr aprocs)
              env
              ;; success continuation for recursive call to get-args
              (lambda (args fail3)
                      (succeed (cons arg args) fail3))
              fail2))
     fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc) 
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else "Unknown procedure type: EXECUTE-APPLICATION" proc)))

;; EVALUATE AMB EXPRESSIONS
;; - try-next cycles through the execution procedures 
;;   for all possible values of the amb expression.
;; - each execution procedure is called with a 
;;   failure continuation that try the next one.
;; - when there are no more alternatives to try,
;;   the entire amb expression fails.
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
            (define (try-next choices)
              (if (null? choices)
                (fail)
                ((car choices)
                 env
                 succeed
                 (lambda () (try-next (cdr choices))))))
            (try-next cprocs))))

;; DRIVER LOOP

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval output:")
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin (newline) (display ";;; Starting a new problem")
               (ambeval input
                        the-global-environment
                        ;; ambeval success
                        (lambda (val next-alternative)
                                (announce-output output-prompt)
                                (user-print val)
                                (internal-loop next-alternative))
                        ;; ambeval failure
                        (lambda ()
                                (announce-output output-prompt)
                                ";;; There are no more values of"
                                (user-print input)
                                (driver-loop)))))))
  (internal-loop 
   (lambda ()
           (newline) (display ";;; There is no current problem")
           (driver-loop))))
