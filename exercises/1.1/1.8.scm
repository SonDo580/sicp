(define (cube-root x)
  (define (cube-root-iter guess)
    (if (good-enough? guess)
      guess
      (cube-root-iter (improve guess))))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (good-enough? guess)
    (define next-guess (improve guess))
    (define delta (abs (- next-guess guess)))
    (< (/ delta guess) 0.001))

  (define initial-guess 1.0)

  (cube-root-iter initial-guess))

(cube-root 27)
(cube-root 8)
(cube-root 3)
(cube-root 0.1)
(cube-root 0.002)