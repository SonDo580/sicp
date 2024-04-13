;; 'reverse' definition (exercise 2.18) 
;; (define (reverse items)
;;   (cond ((null? items) items)
;;         (else (append (reverse (cdr items))
;;                       (list (car items))))))

;; Redefine 'reverse' in terms of 'fold-right'
;; (define (fold-right operation initial sequence)
;;   (if (null? sequence)
;;     initial
;;     (operation (car sequence)
;;                (fold-right operation initial (cdr sequence)))))

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) (append y (list x))) 
;;               (list) 
;;               sequence))

;; Redefine 'reverse' in terms of 'fold-left'
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) 
             (list) 
             sequence))

;; Test
(reverse (list 1 4 9 16))
;; (16 9 4 1)
(reverse (list))
;; ()
(reverse (list 1))
;; (1)