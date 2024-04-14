;; 2 lists are said to be equal? 
;; if they contain equal elements arranged in the same order

;; To be more precise: a and b are equal? 
;; if they are both symbols and the symbols are eq?
;; or if they are both lists such that 
;; (car a) is equal? to (car b) and (cdr a) is equal? to (cdr b) .

(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) 
         (eq? a b))
        ((and (pair? a) (pair? b)) 
         (and (eq? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ()))


(equal? '(this is a list) '(this is a list))
;; true

(equal? '(this is a list) '(this (is a) list))
;; false