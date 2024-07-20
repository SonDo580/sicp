;; HELPERS
(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; QUESTION:
;; Describe the elements of the stream defined by
(define s (cons-stream 1 (add-streams s s)))

;; => They are powers of 2