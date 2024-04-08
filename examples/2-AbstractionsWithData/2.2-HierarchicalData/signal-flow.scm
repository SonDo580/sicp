;; Helper: get the n-th Fibonacci number
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; Mapping: use the 'map' procedure
(map square (list 1 2 3 4 5))
;; (1 4 9 16 25)

;; Filtering
(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5))
;; (1 3 5)

;; Accumulation
(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5))
;; 15
(accumulate * 1 (list 1 2 3 4 5))
;; 120
(accumulate cons (list) (list 1 2 3 4 5))
;; (1 2 3 4 5)

;; Enumerating
(define (enumerate-interval low high)
  (if (> low high)
    (list)
    (cons low
          (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)
;; (2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;; (1 2 3 4 5)

;; Reformulate sum-odd-squares and even-fibs

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))
;; enumerate: tree-leaves
;; filter: odd?
;; map: square
;; accumulate: +, 0

(define (even-fibs n)
  (accumulate cons
              (list)
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
;; enumerate: integers
;; map: fib
;; filter: even?
;; accumulate: cons, ()

;; Constructs a list of squares of the first n + 1 Fibonacci numbers:
(define (list-fib-squares n)
  (accumulate cons
              (list)
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
;; enumerate: integers
;; map: fib
;; map: square
;; accumulate: cons, ()

(list-fib-squares 10)
;; (0 1 1 4 9 25 64 169 441 1156 3025)

;; Compute product of squares of odd integers
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))
;; filter: odd
;; map: square
;; accumulate: *, 1

(product-of-squares-of-odd-elements (list 1 2 3 4 5))
;; 225

;; Find the salary of the highest-paid programmer in a a sequence of personnel records. 
;; Assume that we have
;; - a selector 'salary' that returns the salary of a record
;; - a predicate 'programmer?' that tests if a record is for a programmer
(define (salary-of-highest-paid-programmer records)
  (accumulate max 
              0 
              (map salary 
                   (filter programmer? records))))
