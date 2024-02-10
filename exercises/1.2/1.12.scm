;; Pascal's triangle
;; Compute the element at row r and column c with a recursive process
(define (pascal r c)
  (cond ((or (= c 1) (= c r)) 1)
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))

;; Note: assume r and c are valid
