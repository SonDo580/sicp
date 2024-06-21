;; “The number of pairs in any structure is the
;; number in the 'car' plus the number in the 'cdr' 
;; plus one more to count the current pair.”
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;; The procedure is INCORRECT.
;; Because the pairs can point to each other.

;; Examples - list structure with 3 pairs:

;; [*|*] -> [*|*] -> [*|X]
;;  |        |        |
;;  a        a        a
;; 
;; => count-pair returns the correct value (3)
(define three3 (list 'a 'a 'a))
(count-pairs three3)

;; [*|*] -> [*|*] -> [*|X]
;;  |        |    ^   |      
;;  |        a    |   a     
;;  |_____________|
;; 
;; => count-pair returns 4
(define a (cons 'a '()))
(define three4 (cons a (cons 'a a)))
(count-pairs three4)

;; [*|*] -> [*|X]
;;  |    |   |        
;;  |____|   a          
;; 
;; => count-pair returns 3
(define two3 (cons a a))
(count-pairs two3)

;; [*|*]
;;  |_|
;;  V
;; [*|*] -> [*|X]
;;  |    |   |        
;;  |____|   a          
;; 
;; => count-pair returns 7
(define three7 (cons two3 two3))
(count-pairs three7)

;; -> [*|*] -> [*|*] -> [*|*] ->
;; ^   |        |        |      |
;; |   a        a        a      |
;; |____________________________|
;; 
;; => count-pair never returns
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define three-never (make-cycle (list 'a 'a 'a)))
