;; Classify words: the first item designates the type 
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

;; GRAMMAR:
;; sentence -> noun-phrase verb-phrase
;; noun-phrase -> simple-noun-phrase | (noun-phrase prepositional-phrase)
;; simple-noun-phrase -> article noun
;; verb-phrase -> verb | (verb-phrase prepositional-phrase)
;; prepositional-phrase -> preposition noun-phrase

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) (found-word))))

;; START PARSING:
;; - set *unparsed* to be the entire input
;; - try to parse a sentence
;; - check that nothing is left over
(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; NOTE: a given input may have more than one legal parse

(parse '(the professor lectures to the student with the cat))
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase
;;    (verb lectures)
;;    (prep-phrase
;;     (prep to)
;;     (simple-noun-phrase (article the) (noun student))))
;;   (prep-phrase
;;    (prep with)
;;    (simple-noun-phrase (article the) (noun cat)))))

;; try-again
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;  (verb lectures)
;;  (prep-phrase
;;  (prep to)
;;  (noun-phrase
;;      (simple-noun-phrase (article the) (noun student))
;;      (prep-phrase
;;       (prep with)
;;       (simple-noun-phrase (article the) (noun cat)))))))