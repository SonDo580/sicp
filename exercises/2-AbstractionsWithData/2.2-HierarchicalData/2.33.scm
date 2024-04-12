;; Helper
(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

;; Define some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y)
                      (cons (p x) y)) 
              (list) 
              sequence))

(map square (list 1 2 3 4))
;; (1 4 9 16)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5))
;; (1 2 3 4 5)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 
              0 
              sequence))

(length (list 1 2 3))
;; 3