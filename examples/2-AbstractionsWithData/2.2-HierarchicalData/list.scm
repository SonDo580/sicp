;; Represent list

;; Nested cons
(cons 1
      (cons 2
            (cons 3
                  cons 4 nil)))

;; Primitive list
(list 1 2 3 4)

(define one-through-four
  (list 1 2 3 4))

one-through-four
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))
(cadr one-through-four)
(cons 5 one-through-four)

;; Get the n-th element of the list
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16))
(list-ref squares 3)

;; Get number of items in the list
(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7 9))
(length odds)

;; Compute length in iterative style
(define (length items)
  (define (iter remaining count)
    (if (null? remaining)
      count
      (iter (cdr remaining) (+ count 1))))
  (iter items 0))

;; Combine lists
(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append (cdr list1) list2))))

(append squares odds)
(append odds squares)