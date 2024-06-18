;; Reference: note 3.3/mutable-list-structure
(define (set-to-wow! x) 
  (set-car! (car x) 'wow) 
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

z1 ; ((a b) a b)
;; z1 -> [*|*]
;;       _|_|
;;      |
;; x -> [*|*] -> [*|X]
;;       |        |
;;       a        b

(set-to-wow! z1) ; ((wow b) wow b)
;; z1 -> [*|*]
;;       _|_|
;;      |
;; x -> [*|*] -> [*|X]
;;       |        |
;;      wow       b

x ; (wow b)

z2 ; ((a b) a b)
;; z2 -> [*|*]
;;       _| |________
;;      |            |
;; [*|*] -> [*|X]   [*|*] -> [*|X]
;;  |        |       |        |
;;  a        b       a        b

(set-to-wow! z2) ; ((wow b) a b)
;; z2 -> [*|*]
;;       _| |________
;;      |            |
;; [*|*] -> [*|X]   [*|*] -> [*|X]
;;  |        |       |        |
;; wow       b       a        b