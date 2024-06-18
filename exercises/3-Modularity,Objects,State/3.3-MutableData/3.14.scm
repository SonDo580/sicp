(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

;; Explain what mystery does in general
;; - It reverses the list x

(define v (list 'a 'b 'c 'd))
; v->[*|*]->[*|*]->[*|*]->[*|X]
;     |      |      |      |
;     a      b      c      d

v ; (a b c d)

(define w (mystery v))
; v->[*|X]<-[*|*]<-[*|*]<-[*|*]<-w
;     |      |      |      |
;     a      b      c      d

w ; (d b c a)
v ; (a)