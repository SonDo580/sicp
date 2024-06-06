; Write procedure 'make-monitored' 
; It takes a procedure 'f' (which also takes one input) as input.
; It returns a new procedure 'mf' that:
; - Keeps track of the number of times it is called by maintaining an internal counter.
; - When given the input 'how-many-calls?, returns the current value of the counter.
; - When given the input 'reset-count, resets the counter to zero.
; - For any other input, calls f with that input, increments the counter, and returns the result.

(define (make-monitored f)
  (define count 0)
  (define (mf input)
    (cond ((eq? input 'how-many-calls?) count)
          ((eq? input 'reset-count) (set! count 0) true)
          (else (set! count (inc count))
                (f input))))
  mf)

;; Usage
(define s (make-monitored sqrt))

(s 100) ; 10
(s 'how-many-calls?) ; 1
(s 'reset-count) ; true
(s 'how-many-calls?) ; 0

