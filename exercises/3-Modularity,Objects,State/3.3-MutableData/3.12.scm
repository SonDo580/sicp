;; Form a new list from x and y
(define (append x y)
  (if (null? x)
    y
    (cons (car x)
          (append (cdr x) y))))

;; Mutator: modify the final pair of x so that its cdr is y
(define (append! x y)
  (set-cdr! (last-pair x) y))

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

;; Interaction
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z ; (a b c d)

(cdr x) ; (b)
;; x -> [*|*] -> [*|X]
;;       |        |
;;       a        b

(define w (append! x y))
w ; (a b c d)

(cdr x) ; (b c d)
;; w ->
;; x -> [*|*] -> [*|*] -> [*|*] -> [*|X]
;;       |        |        |        |
;;       a        b        c        d 