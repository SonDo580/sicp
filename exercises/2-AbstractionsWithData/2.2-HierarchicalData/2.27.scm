;; 'reverse' procedure - from exercse 2.18
(define (reverse items)
  (cond ((null? items) items)
        (else (append (reverse (cdr items))
                      (list (car items))))))

;; Define a 'deep-reverse' procedure that also reverse sub-lists
(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items)) (append (deep-reverse (cdr items))
                                     (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items))
                      (list (car items))))))

;; Usage
(define x (list (list 1 2) (list 3 4)))
;; ((1 2) (3 4))

(reverse x)
;; ((3 4) (1 2))

(deep-reverse x)
;; ((4 3) (2 1))

(deep-reverse (list))
;; ()

(deep-reverse (list 3))
;; (3)
