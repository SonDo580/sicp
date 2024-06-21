;; Devise a correct version of the count-pairs
;; procedure of Exercise 3.16 that returns 
;; the number of distinct pairs in any structure

(define (count-pairs x)
  (let ((counted-lst '()))
    (define (counted? y lst)
      (cond ((null? lst) false)
            ((eq? y (car lst)) true)
            (else (counted? y (cdr lst)))))
    (define (iter y)
      (if (or (not (pair? y))
              (counted? y counted-lst))
        0
        (begin
         (set! counted-lst (cons y counted-lst))
         (+ (iter (car y))
            (iter (cdr y))
            1)
         )))
    (iter x)))

;; TEST
(count-pairs (list 'a 'a 'a)) ; should return 3
;; [*|*] -> [*|*] -> [*|X]
;;  |        |        |
;;  a        a        a

(define one (cons 'a '()))
(count-pairs (cons one (cons 'a one))) ; should return 3
;; [*|*] -> [*|*] -> [*|X]
;;  |        |    ^   |      
;;  |        a    |   a     
;;  |_____________|

(define two (cons one one))
(count-pairs two) ; should return 2
;; [*|*] -> [*|X]
;;  |    ^   |
;;  |____|   a

(count-pairs (cons two two)) ; should return 3
;; [*|*]
;;  |_|
;;  |
;;  V
;; [*|*] -> [*|X]
;;  |    ^   |        
;;  |____|   a          

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define with-cycle (make-cycle (list 'a 'a 'a)))
(count-pairs with-cycle) ; should return 3
;; -> [*|*] -> [*|*] -> [*|*] ->
;; ^   |        |        |      |
;; |   a        a        a      |
;; |____________________________|