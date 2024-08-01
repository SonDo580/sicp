;; Reimplement the Monte Carlo estimation of pi (see note 3.1/monte-carlo)
;; from a stream-processing point of view

;; procedure 'rand-update':
;; - start with x1 
;; x2 = (rand-update x1)
;; x3 = (rand-update x2)
;; -> a sequence of numbers with statistical properties of uniform distribution

(define random-numbers
  (cons-stream
   random-init
   (stream-map rand-update random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream (f (stream-car s)
                  (stream-car (stream-cdr s)))
               (map-successive-pairs f 
                                     (stream-cdr (stream-cdr s)))))

;; stream of experiment outcomes (true/false)
(define ceraso-stream
  (map-successive-pairs (lambda (r1 r2)
                                (= (gcd r1 r2) 1))
                        random-numbers))

;; stream of estimated probabilities
(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream (/ passed (+ passed failed))
                 (monte-carlo (stream-cdr experiment-stream)
                              passed
                              failed)))
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

;; pi-stream
(define pi-stream
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo ceraso-stream 0 0)))

