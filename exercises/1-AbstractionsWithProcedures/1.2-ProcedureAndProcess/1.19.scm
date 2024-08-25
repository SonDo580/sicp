;; REFERENCE: note 1.2/fibonacci
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))
(define (fib n)
  (fib-iter 1 0 n))

;; - Call the transformation of a and b T
;;   a <- a + b
;;   b <- a
;; - Applying T n times, starting with (a, b) = (1, 0)
;;   produce (Fib(n+1), Fib(n))
;; - Consider T to be the special case p = 0 and q = 1
;;   in a family of transformation Tpq, where:
;;   Tpq(a, b) = (bq + aq + ap, bp + aq)

;; PROBLEM:
;; - Show that applying Tpq twice is the same as 
;;   using a single Tp'q', and compute p' and q'.
;; - Use this the complete the faster version of
;;   fib-iter.

;; SOLUTION:

;; Tp'q'(a, b)
;; = Tpq(Tpq(a, b))
;; = Tpq[bq + aq + ap, bp + aq]
;; = [(bp + aq)q + (bq + aq + ap)q + (bq + aq +ap)p,
;;    (bp + aq)p + (bq + aq + ap)q]
;; = [bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2,
;;    bp^2 + apq + bq^2 + aq^2 + apq]
;; = [b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2),
;;    b(p^2 + q^2) + a(q^2 + 2pq)]
;; 
;; -> p' = p^2 + q^2
;; -> q' = q^2 + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
