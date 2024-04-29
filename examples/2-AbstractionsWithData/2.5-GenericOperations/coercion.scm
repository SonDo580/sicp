;; Note: The code extends generic-artithmetic.scm

;; Assume that there are put-coercion and get-coercion procedures
;; for manipulating  a special coercion table

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; Install the coercion procedure
(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

;; Modify apply-generic procedure
;; - If the procedure is defined for the argument types, 
;;   dispatch the procedure found in operation-and-type table
;; - Otherwise, try coercion

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args) ))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2
                     (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                     (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types" (list op type-tags)))))))
