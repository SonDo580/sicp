;; Primitive predicates such as 'symbol?' and 'number?' 
;; determine whether data objects have particular types

;; Modify the definitions of 'type-tag', 'contents', 'attach-tag' 
;; so that our generic system takes advantage of 
;; Scheme’s internal type system

;; => ordinary numbers should be represented simply as Scheme numbers 
;; rather than as pairs whose 'car' is the symbol 'scheme-number'.

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
         ((number? datum) datum)
         (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

;; Handle ordinary numbers
(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (/ x y)))
  'done)