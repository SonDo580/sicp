;; PUZZLE:
;; Baker, Cooper, Fletcher, Miller, and Smith live on different 
;; floors of an apartment house that contains only 5 floors. 
;; - Baker does not live on the top floor. 
;; - Cooper does not live on the bottom floor. 
;; - Fletcher does not live on either the top or the bottom floor. 
;; - Miller lives on a higher floor than does Cooper. 
;; - Smith does not live on a floor adjacent to Fletcher’s. 
;; - Fletcher does not live on a floor adjacent to Cooper’s. 
;; Where does everyone live?

;; STRATEGY: 
;; - enumerate all possibilities 
;; - impose the given restrictions
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; HELPERS:
;; from note 4.3/amb-and search
(define (require p) (if (not p) (amb)))

;; determine if the elements of a list are distinct
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

;; EVALUATE:
(multiple-dwelling)
;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))