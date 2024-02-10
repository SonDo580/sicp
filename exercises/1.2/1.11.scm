;; f(n) = n if n < 3
;; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

;; Compute by recursive process
(define (f n)
  (cond ((< n 3) n)
        (else (+ (f (- n 1))
                 (* 2 (f (- n 2)))
                 (* 3 (f (- n 3)))))))

;; Compute by iterative process
(define (f n)
  (define (f-iter a b c count)
    (cond ((< n 3) n)
          ((< count 1) a)
          (else (f-iter (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (- count 1)))))
  (f-iter 2 1 0 (- n 2)))
