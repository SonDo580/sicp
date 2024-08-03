;; REFERENCE: note 4/metacircular-evaluator
(define (tagged-list exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; EXERCISE: support and / or
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
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

;; And: 
;; - return false if any value is false
;; - return true if there are no expressions
;;   or all values are true
(define (and? exp) (tagged-list exp 'and))
(define (eval-and exp env)
  (define (iter exps)
    (cond ((null? exps) 'true)
          ((not (eval (car exps) env)) 'false)
          (else (iter (cdr exps)))))
  (iter (cdr exp)))

;; Or: 
;; - return true if any value is true
;; - return false if there are no expressions
;;   or all values are false
(define (or? exp) (tagged-list exp 'or))
(define (eval-or exp env)
  (define (iter exps)
    (cond ((null? exps) 'false)
          ((eval (car exps) env) 'true)
          (else (iter (cdr exps)))))
  (iter (cdr exp)))

;; Define and / or as derived expressions
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
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type: EVAL" exp))))

(define (and->if exp)
  (if (null? (cdr exp))
    'true
    (make-if (not (cadr exp))
             'false
             (and->if (cons 'and (cddr exp))))))

(define (or->if exp)
  (if (null? (cdr exp))
    'false
    (make-if (cadr exp)
             'true
             (or->if (cons 'or (cddr exp))))))