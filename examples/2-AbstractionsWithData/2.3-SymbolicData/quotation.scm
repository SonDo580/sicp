

;; We can distinguish between symbols and their values
(define a 1)
(define b 2)

(list a b)
;; (1 2)
(list 'a 'b)
;; (a b)
(list 'a b)
;; (a 2)

;; Quotation with compound object
(car '(a b c))
;; a
(cdr '(a b c))
;; (b c)

;; Primitive eq?: test if 2 symbols are the same

;; Procedure memq
;; - take 2 arguments: a symbol and a list
;; - if the symbol is not in the list, return false
;; - otherwise, return the sublist begin with the symbol

(define (memq item seq)
  (cond ((null? seq) false)
        ((eq? item (car seq)) seq)
        (else (memq item (cdr seq)))))

(memq 'apple '(pear banana prune))
;; false

(memq 'apple '(x (apple sauce) y apple pear))
;; (apple pear)