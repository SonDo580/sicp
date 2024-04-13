;; 'accumulate-n' is similar to 'accumulate'
;; but the 3rd argument is a sequence of sequences
;; which have the same number of elements

;; 'accumulate-n' applies the accumulation procedure to combine 
;; all the first elements of the sequences, 
;; all the second elements of the sequences, ...
;; and returns a sequence of results

;; Helper
(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

;; Main
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    (list)
    (cons (accumulate op 
                      init 
                      (map (lambda (s) (car s)) 
                           seqs))
          (accumulate-n op
                        init
                        (map (lambda (s) (cdr s))
                              seqs)))))

;; Usage
(define s (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))
(accumulate-n + 0 s) 
;; (22 26 30)