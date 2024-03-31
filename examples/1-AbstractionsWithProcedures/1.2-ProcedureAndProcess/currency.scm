
;; Return number of ways to make change of 'amount' with 5 kinds of coins
(define (count-change amount) (count-change-recursive amount 5))

;; Return the denomination of the first kind of coin (largest value)
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)    ; pennie
        ((= kinds-of-coins 2) 5)    ; nickel
        ((= kinds-of-coins 3) 10)   ; dime
        ((= kinds-of-coins 4) 25)   ; quarter
        ((= kinds-of-coins 5) 50))) ; half-dollar

;; Special case:
;; amount = 0 => 1 way to make change
;; amount < 0 or kinds-of-counts = 0 => 0 way to make change

;; Normal case:
;; the ways to make change can be divided into 2 groups:
;; those that use the first kind of coin, and those that do not

;; Recursive procedure => tree-recursive process
(define (count-change-recursive amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (count-change-recursive 
                  amount 
                  (- kinds-of-coins 1))  ; don't use the first kind of coin
                 (count-change-recursive 
                  (- amount (first-denomination kinds-of-coins)) 
                  kinds-of-coins)))))    ; keep using 1 coin of the first kind

;; Number of ways to make change of $1.00
(count-change 100)