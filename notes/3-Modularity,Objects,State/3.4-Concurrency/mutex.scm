;; We can implement serializers with mutex

;; - A mutex is an object that supports 2 operations: acquire and release
;; - Once a mutex has been acquired, no other acquire operations on that
;; mutex may proceed until the mutex is released

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
            (define (serialized-p . args)
              (mutex 'acquire)
              (let ((val (apply p args)))
                (mutex 'release)
                val))
            serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release)
             (clear! cell))))
    the-mutex))

(define (clear! cell) 
  (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
    true
    (begin (set-car! cell true)
           false)))

;; - This implementation of test-and-set! does not suffice
;; - We must guarantee that once a process has tested the cell and
;; found it to be false, the cell contents will be set to true before
;; any other process can test the cell.