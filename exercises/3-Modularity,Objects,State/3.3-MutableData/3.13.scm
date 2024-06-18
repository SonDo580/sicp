(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
;; z -> [*|*] -> [*|*] -> [*|*] ->.
;;   |   |        |        |      |
;;   |   a        b        c      |
;;   |____________________________|

;; If we try to compute (last-pair z):
;; we will be stuck in an infinite recursion because z is not null-terminated
