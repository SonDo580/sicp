;; procedure 'rand-update':
;; - start with x1 
;; x2 = (rand-update x1)
;; x3 = (rand-update x2)
;; -> a sequence of numbers with statistical properties of uniform distribution

(define rand (let ((x random-init))
               (lambda ()
                       (set! x (rand-update x))
                       x)))

;; Monte Carlo method to estimate pi
;; 6 / pi^2 is the probability that the GCD of 2 integers chosen at random is 1
;; - to obtains the approximation of pi, perform a large number of experiments
;; - in each experiment, choose 2 integers at random and perform the test
;; - the fraction of times that the test is passed gives the estimate of 6 / pi^2

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials))
          ((experiment) 
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else 
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

;; Without assignment (in rand)
(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0) 
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1) 
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else 
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))