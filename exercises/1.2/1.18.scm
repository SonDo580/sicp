;; Devise a procedure that generates an iterative process 
;; for multiplying two integers in terms of adding, doubling, and halving
;; and uses a logarithmic number of steps

;; For reference: recursive process (exercise 1.17)
(define (recursive-mult a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((= b 1) a)
        ((= a 1) b)
        ((even? b) (recursive-mult (double a) (halve b)))
        (else (+ a (recursive-mult (double a) (halve (- b 1)))))))

;; Iterative process
(define (iterative-mult a b)
  (define (iter A B sum) 
     (cond ((= B 0) sum)
           ((even? B) (iter (double A) (halve B) sum))
           (else (iter A (- B 1) (A + sum)))))
  (iter A B 0))

;; ANALYZE:
;; b is even: a * b = (2*a) * (b/2)
;; b is odd: a * b = (2*a) * ((b - 1 + 1) / 2) = (2*a) * (b-1)/2 + a
;; accumulate A to current 'sum' every time B becomes odd
;; example
;; > 5 * 10
;; > 10 * 5
;; > 10 + 10 * 4
;; > 10 + 20 * 2
;; > 10 + 40 * 1
;; > 10 + 40



;; HELPERS
(define (even? n)
  (= 0 (remainder n 2)))

(define (double n)
  (* n 2))

(define (halve n) ;; only accept even number
  (/ n 2))