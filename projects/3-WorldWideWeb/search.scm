;;; SEARCH.SCM
;;; MIT 6.001                               Spring, 2005
;;; PROJECT 3

(define *search-debug* #t)         ; flag that shows search progress

;;; Searching and Indexing the World Wide Web.
;;;
;;; This file contains three major components, all of which are
;;; *not* web specific.  They are general purpose abstractions
;;; that we will then use to represent, search, and index the web.
;;;
;;;  1. Graph Abstraction -- directed graph with labeled nodes,
;;;                          node children (outgoing edges), and
;;;                          node contents
;;;
;;;  2. Search and        -- system to search a graph network looking
;;;     Search Strategy      for some goal
;;;
;;;  3. Index             -- an index associating a key with
;;;                          one or more values

;;;------------------------------------------------------------
;;; Graph Abstraction
;;;
;;;   Graph                     a collection of Graph-Elements
;;;   Graph-Element               a node, outgoing children from the
;;;                               node, and contents for the node
;;;   Node = symbol             a symbol label or name for the node
;;;   Contents = anytype        the contents for the node

;;---------------
;; Graph-Element

; make-graph-element: Node,list<Node>,Contents -> Element
(define (make-graph-element node children contents)
  (list 'graph-element node children contents))

(define (graph-element? element)            ; anytype -> boolean
  (and (pair? element) (eq? 'graph-element (car element))))

; Get the node (the name) from the Graph-Element
(define (graph-element->node element)       ; Graph-Element -> Node
  (if (not (graph-element? element))
      (error "object not element: " element)
      (first (cdr element))))

; Get the children (a list of outgoing node names)
; from the Graph-Element
(define (graph-element->children element)   ; Graph-Element -> list<Node>
  (if (not (graph-element? element))
      (error "object not element: " element)
      (second (cdr element))))

; Get the contents from the Graph-Element
(define (graph-element->contents element)   ; Graph-Element -> Contents
  (if (not (graph-element? element))
      (error "object not element: " element)
      (third (cdr element))))

;;---------------
;; Graph

(define (make-graph elements)            ; list<Element> -> Graph
  (cons 'graph elements))

(define (graph? graph)                  ; anytype -> boolean
  (and (pair? graph) (eq? 'graph (car graph))))

(define (graph-elements graph)           ; Graph -> list<Graph-Element>
  (if (not (graph? graph))
      (error "object not a graph: " graph)
      (cdr graph)))

(define (graph-root graph)		; Graph -> Node|null
  (let ((elements (graph-elements graph)))
    (if (null? elements)
	#f
	(graph-element->node (car elements)))))

; Find the specified node in the graph
(define (find-graph-element graph node)   ; Graph,Node -> Graph-Element|null
  (define (find elements)
    (cond ((null? elements) '())
          ((eq? (graph-element->node (car elements)) node)
           (car elements))
          (else (find (cdr elements)))))
  (find (graph-elements graph)))

; Find the children of the specified node in the graph
(define (find-node-children graph node)        ; Graph,Node -> list<Node>|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->children element)
        '())))

; Find the contents of the specified node in the graph
(define (find-node-contents graph node)         ; Graph,Node -> Contents|null
  (let ((element (find-graph-element graph node)))
    (if (not (null? element))
        (graph-element->contents element)
        '())))

;; Testing...

(define test-graph
  (make-graph (list
   (make-graph-element 'a '(b i m) '(some words))
   (make-graph-element 'b '(c d e h) '(more words))
   (make-graph-element 'c '() '(at c node some words))
   (make-graph-element 'd '() '())
   (make-graph-element 'e '(f g) '(and even more words))
   (make-graph-element 'f '() '())
   (make-graph-element 'g '() '())
   (make-graph-element 'h '() '())
   (make-graph-element 'i '(j k l) '(more words yet))
   (make-graph-element 'j '() '())
   (make-graph-element 'k '() '())
   (make-graph-element 'l '() '()))))

(define test-cycle
  (make-graph (list
   (make-graph-element 'a '(b c) '(words for node a))
   (make-graph-element 'b '(c) '(words for node b))
   (make-graph-element 'c '(a) '(words for node c)))))

; (find-graph-element test-graph 'b)
; (find-graph-element test-graph 'z)
; (find-node-children test-graph 'b)
; (find-node-children test-graph 'z)
; (find-node-contents test-graph 'b)
; (find-node-contents test-graph 'z)


;;;------------------------------------------------------------
;;; Searching a network
;;;
;;; We define below a standard search procedure that walks
;;; over a graph in an effort to find a desired node.
;;; This version does not handle cycles in the graph

;; search: Node, (Node->Boolean), (Graph, Node -> List<Node>)
;;         (List<Node>, List<Node> -> List<Node>), Graph
;;           --> Boolean 

(define (search initial-state goal? successors merge graph)
  ;; initial-state is the start state of the search
  ;;
  ;; goal? is the predicate that determines whether we have
  ;; reached the goal
  ;;
  ;; successors computes from the current state all successor states
  ;;
  ;; merge combines new states with the set of states still to explore
  (define (search-inner still-to-do)
    (if (null? still-to-do)
	#f
	(let ((current (car still-to-do)))
	  (if *search-debug*
	      (write-line (list 'now-at current)))
	  (if (goal? current)
	      #t
	      (search-inner
	       (merge (successors graph current) (cdr still-to-do)))))))
  (search-inner (list initial-state)))

(define (DFS-simple start goal? graph)
  (search start
	  goal?
	  find-node-children
	  (lambda (new old) (append new old)) ; stack
	  graph))

;; (DFS-simple 'a
;;             (lambda (node) (eq? node 'l))
;;             test-graph)

;; Exercise 1: BFS-simple
;; Note: this will still fail if the graph contains cycles  
(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new)) ; queue
          graph))

;; (BFS-simple 'a
;;             (lambda (node) (eq? node 'l))
;;             test-graph)

;; Helper: check if a list contains an element
(define (include? items elem)
  (cond ((null? items) false)
        ((eq? (car items) elem) true)
        (else (include? (cdr items) elem))))

;; Exercise 2: Marking visited nodes
(define (search-with-cycles initial-state goal? successors merge graph)
  (let ((visited '()))
    (define (visited? node)
      (include? visited node))
    (define (search-inner still-to-do)
      (if (null? still-to-do)
        #f
         (let ((current (car still-to-do)))
           (if (visited? current)
             (search-inner (cdr still-to-do))
             (begin
              (set! visited (cons current visited))
              (if *search-debug*
                (write-line (list 'now-at current)))
              (if (goal? current)
                #t
                 (search-inner
                  (merge (successors graph current) (cdr still-to-do)))))))))
    (search-inner (list initial-state))))

(define (DFS start goal? graph)
  (search-with-cycles 
   start
   goal?
   find-node-children
   (lambda (new old) (append new old))
   graph))

(define (BFS start goal? graph)
  (search-with-cycles
   start
   goal?
   find-node-children
   (lambda (new old) (append old new))
   graph))

;; (DFS 'a
;;      (lambda (node) (eq? node 'l))
;;      test-cycle)
;; 
;; (now-at a)
;; (now-at b)
;; (now-at c)
;; ;Value: #f

;; (BFS 'a
;;      (lambda (node) (eq? node 'l))
;;      test-cycle)
;; 
;; (now-at a)
;; (now-at b)
;; (now-at c)
;; ;Value: #f

;; (DFS 'http://sicp.csail.mit.edu/
;;      (lambda (node) (eq? node 'http://sicp.csail.mit.edu/not-exist))
;;      the-web)
;; 
;; (now-at http://sicp.csail.mit.edu/)
;; (now-at http://sicp.csail.mit.edu/schemeimplementations)
;; (now-at http://sicp.csail.mit.edu/getting-help.html)
;; (now-at http://sicp.csail.mit.edu/lab-use.html)
;; (now-at *the-goal*)
;; (now-at http://sicp.csail.mit.edu/psets)
;; ;Value: #f

;; (BFS 'http://sicp.csail.mit.edu/
;;      (lambda (node) (eq? node 'http://sicp.csail.mit.edu/not-exist))
;;      the-web)
;;
;; (now-at http://sicp.csail.mit.edu/)
;; (now-at http://sicp.csail.mit.edu/schemeimplementations)
;; (now-at http://sicp.csail.mit.edu/psets)
;; (now-at http://sicp.csail.mit.edu/getting-help.html)
;; (now-at http://sicp.csail.mit.edu/lab-use.html)
;; (now-at *the-goal*)
;; ;Value: #f

;;;------------------------------------------------------------
;;; Index Abstraction
;;;
;;;   An Index enables us to associate values with keys, and
;;; to retrieve those values later on given the key.
;;;
;;; Key = symbol
;;; Val = symbol

;; Index Implementation
;;
;;   An Index will be a tagged data object that holds a 
;; list of Index-Entries.  Each Index-Entry associates
;; a key with a list of values for that key, i.e.
;;   Index = Pair<Index-Tag, list<Index-Entry>>
;;   Index-Entry = list<Key, list<Val>>
;; 

(define (make-index)            ; void -> Index
  (list 'index))

(define (index? index)          ; antype -> boolean
  (and (pair? index) (eq? 'index (car index))))

; An index can be reset to empty.
(define (reset-index! index)    ; Index -> Index
  (cond ((not (index? index))
         (error "object not an index: " index))
        (else (set-cdr! index '())
              index)))
      
; This is an internal helper procedure not to
; be used externally.
(define (find-entry-in-index index key)
  (if (not (index? index))
      (error "object not an index: " index)
      (let ((entry (assv key (cdr index))))
        (if entry entry '()))))


; returns a list of values associated with key
(define (find-in-index index key)       ; Index,Key -> list<Val>
  (let ((index-entry (find-entry-in-index index key)))
    (if (not (null? index-entry))
        (cadr index-entry)
        '())))

;; Warm up 1
(define (add-to-index! index key value) ; Index,Key,Val -> Index
 (let ((index-entry (find-entry-in-index index key)))
   (if (null? index-entry)
     ;; no entry -- create and insert a new one...
	   (set-cdr! index (cons (list key (list value)) (cdr index)))
     ;; entry exists -- insert value if not already there...
     (if (not (include? (cadr index-entry) value))
       (set-car! (cdr index-entry) 
                 (cons value (cadr index-entry))))))
 index)

;; Box diagram of index:
;; [ | ]---[ | ]-----------[ |X]
;;  |       |               |
;; index   [ | ]---[ |X]   [ | ]---[ |X]
;;          |       |       |       |
;;        key2     val2    key1    [ | ]---[ |X]
;;                                  |       |
;;                                 val1`   val1

;; Testing / Exercise 3

;; (define test-index (make-index))

;; (add-to-index! test-index 'key1 'value1)
;; (add-to-index! test-index 'key1 'value1)
;; (add-to-index! test-index 'key1 'value1)
;; ;Value: (index (key1 (value1)))

;; (add-to-index! test-index 'key2 'value2)
;; ;Value: (index (key2 (value2)) (key1 (value1)))

;; (add-to-index! test-index 'key1 'another-value1)
;; ;Value: (index (key2 (value2)) (key1 (another-value1 value1)))

;; (find-in-index test-index 'key1)
;; ;Value: (another-value1 value1)

;; (find-in-index test-index 'key2)
;; ;Value: (value2)

;;------------------------------------------------------------
;; Finally, the Web!

;;--------------------
;; Web representation 
;;
;; We'll represent a "Web" as a graph.  Each Node in
;; the graph will be a URL; the node contents is the
;; Text inside the URL, and the node children is the
;; list of URL links inside the URL:
;;
;; Web = Graph
;; URL = Node
;; Text = list<Word>
;; Word = symbol      

; Procedures to get web links and web page contents:

(define (find-URL-links web url)
  (find-node-children web url))

(define (find-URL-text web url)
  (find-node-contents web url))


;; The real definition of THE-WEB we'll use is in another file, 
;; including all of the words in the documents.

;;(define the-web
;;  (list
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/
;;    '(http://sicp.csail.mit.edu/SchemeImplementations/
;;      http://sicp.csail.mit.edu/projects/)
;;    '(... words extracted from http://sicp.csail.mit.edu/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/projects/
;;    '(http://sicp.csail.mit.edu/collaborative-work.html
;;      http://sicp.csail.mit.edu/getting-help.html)
;;    '(... words extracted from http://sicp.csail.mit.edu/SchemeImplementations/ ...))
;;   (make-graph-element
;;    'http://sicp.csail.mit.edu/getting-help.html
;;    '(http://sicp.csail.mit.edu/
;;      http://sicp.csail.mit.edu/SchemeImplementations/)
;;    '(... words extracted from http://sicp.csail.mit.edu/getting-help.html))
;;   ...))

;; Warm up 2
;; - DFS-simple will fail on the-web graph because it contains cycle
;;   / -> /projects -> /getting-help.html
;;   |__________________________________|

;;--------------------
;; Searching the Web

;; you need to write expressions to search the web using different search
;; strategies


;;--------------------
;; Indexing the Web
;;
;;   Our purpose in creating an index of a web is to
;; later support the ability to find any pages that contain
;; a given word.  Thus, a Key in our index will be a Word,
;; and the values in the index will be the URLs of pages
;; that contain that word.

;; A procedure to help  with indexing web pages
;; using the Index abstraction.  The idea is to
;; get the text associated with the URL from the
;; web, and then key each of the words in the
;; text into the index.

;; Exercise 4: Web index
;; add-document-to-index!: Index, Web, URL
(define (add-document-to-index! index web url)
  (define (add-words-to-index! words)
    (if (not (null? words))
      (begin
       (add-to-index! index (car words) url)
       (add-words-to-index! (cdr words)))))
  (add-words-to-index! (find-URL-text web url))
  index)

;; Example use
;; 
;; (define the-web-index (make-index))

;; (add-document-to-index! the-web-index
;;                         the-web 
;;                         'http://sicp.csail.mit.edu/)

;; (find-in-index the-web-index 'help)
;; ;; ;Value: (http://sicp.csail.mit.edu/)

;; (find-in-index the-web-index '*magic*)
;; ;; ;Value: ()

;;------------------------------------------------------------
;; Excersise 5: Crawling the Web to build an index

;; traverse: 
;; - similar to search 
;; - allow an action to take place at each node
(define (traverse initial-state goal? successors merge graph action)
  (let ((visited '()))
    (define (visited? node)
      (include? visited node))
    (define (traverse-inner still-to-do)
      (if (null? still-to-do)
        #f
         (let ((current (car still-to-do)))
           (if (visited? current)
             (traverse-inner (cdr still-to-do))
             (begin
              (action current)
              (set! visited (cons current visited))
              (if *search-debug*
                (write-line (list 'now-at current)))
              (if (goal? current)
                #t
                 (traverse-inner
                  (merge (successors graph current) (cdr still-to-do)))))))))
    (traverse-inner (list initial-state))))

(define (BF-traverse start goal? graph action)
  (traverse
   start
   goal?
   find-node-children
   (lambda (new old) (append old new))
   graph
   action))

;; make-web-index
;; - creates a new index
;; - find all URLs that can be reached from a given web and intial URL
;;   and indexes them
;; - returns a procedure to look up all the URLs of documents containing 
;;   a word
(define (make-web-index web initial-url)
  (let ((web-index (make-index)))
    (BF-traverse initial-url
                 (lambda (node) false)
                 web
                 (lambda (url)
                         (add-document-to-index! web-index web url)))
    (lambda (word)
            (find-in-index web-index word))))

;; Test:
;; (define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))
;; (find-documents 'collaborative)
;Value: (http://sicp.csail.mit.edu/psets http://sicp.csail.mit.edu/)

;;------------------------------------------------------------
;; Excersise 6: Dynamic web search

;; find:
;; - similar to search-with-cycle
;; - any = false -> find all nodes
;; - any = true -> find any node and stop
(define (find initial-state goal? successors merge graph any)
  (let ((visited '())
        (found '()))
    (define (visited? node)
      (include? visited node))
    (define (find-inner still-to-do)
      (if (null? still-to-do)
         found
         (let ((current (car still-to-do)))
           (if (visited? current)
             (find-inner (cdr still-to-do))
             (begin
              (set! visited (cons current visited))
              (if *search-debug*
                (write-line (list 'now-at current)))
              (if (goal? current)
                 (begin
                  (set! found (cons current found))
                  (if any 
                    found
                    (find-inner
                     (merge (successors graph current) (cdr still-to-do)))))
                 (find-inner
                  (merge (successors graph current) (cdr still-to-do)))))))))
    (find-inner (list initial-state))))

(define (BF-find start goal? graph any)
  (find
   start
   goal?
   find-node-children
   (lambda (new old) (append old new))
   graph
   any))

;; search-any:
;; - search the indicated web with breadth-first strategy
;; - return the first document that contains the given word
(define (search-any web start_node word)
  (BF-find 
   start_node
   (lambda (node) (include? (find-node-contents web node) word))
   web
   true))

;; search-all:
;; - search the indicated web with breadth-first strategy
;; - return all the documents that contain the given word
(define (search-all web start_node word)
  (BF-find
   start_node
   (lambda (node) (include? (find-node-contents web node) word))
   web
   false))

;; Test:

;; (search-any the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;; ;Value: (http://sicp.csail.mit.edu/)

;; (search-all the-web 'http://sicp.csail.mit.edu/ 'collaborative)
;; ;Value: (http://sicp.csail.mit.edu/psets http://sicp.csail.mit.edu/)

;;------------------------------------------------------------
;; Exercise 7: Comparison - Web Index vs. Dynamic Search

;; (define web1 (generate-random-web 30))
;; (define web2 (generate-random-web 60))
;; (define web3 (generate-random-web 90))
;; (define web4 (generate-random-web 120))
;; (define web5 (generate-random-web 150))
;; (define web6 (generate-random-web 180))

;; (timed search-any web1 '*start* 'help) ; 9.999999999999787e-3
;; (timed search-any web2 '*start* 'help) ; 1.0000000000000231e-2
;; (timed search-any web3 '*start* 'help) ; 9.999999999999787e-3
;; (timed search-any web4 '*start* 'help) ; 4.0000000000000036e-2
;; (timed search-any web5 '*start* 'help) ; 1.0000000000000231e-2
;; (timed search-any web6 '*start* 'help) ; 9.999999999999787e-3

;; (timed search-any web1 '*start* 'Susan-hockfield) ; .11000000000000032
;; (timed search-any web2 '*start* 'Susan-hockfield) ; .13999999999999968
;; (timed search-any web3 '*start* 'Susan-hockfield) ; .2200000000000002
;; (timed search-any web4 '*start* 'Susan-hockfield) ; .23999999999999977
;; (timed search-any web5 '*start* 'Susan-hockfield) ; .41999999999999993
;; (timed search-any web6 '*start* 'Susan-hockfield) ; .4299999999999997

;; (timed search-all web1 '*start* 'help) ; .11000000000000032
;; (timed search-all web2 '*start* 'help) ; .11000000000000032
;; (timed search-all web3 '*start* 'help) ; .23999999999999932
;; (timed search-all web4 '*start* 'help) ; .2999999999999998
;; (timed search-all web5 '*start* 'help) ; .3899999999999997
;; (timed search-all web6 '*start* 'help) ; .5

;; (timed make-web-index web1 '*start*) ; .27000000000000046
;; (timed make-web-index web2 '*start*) ; .4900000000000002
;; (timed make-web-index web3 '*start*) ; .9100000000000001
;; (timed make-web-index web4 '*start*) ; 1.3099999999999987
;; (timed make-web-index web5 '*start*) ; 1.6900000000000013
;; (timed make-web-index web6 '*start*) ; 2.3599999999999994

;; (define find-documents-1 (make-web-index web1 '*start*))
;; (define find-documents-2 (make-web-index web2 '*start*))
;; (define find-documents-3 (make-web-index web3 '*start*))
;; (define find-documents-4 (make-web-index web4 '*start*))
;; (define find-documents-5 (make-web-index web5 '*start*))
;; (define find-documents-6 (make-web-index web6 '*start*))

;; (timed find-documents-1 'help) ; 0.
;; (timed find-documents-2 'help) ; 0.
;; (timed find-documents-3 'help) ; 0.
;; (timed find-documents-4 'help) ; 0.
;; (timed find-documents-5 'help) ; 0.
;; (timed find-documents-6 'help) ; 0.

;; (timed find-documents-1 'Susan-hockfield) ; 0.
;; (timed find-documents-2 'Susan-hockfield) ; 0.
;; (timed find-documents-3 'Susan-hockfield) ; 0.
;; (timed find-documents-4 'Susan-hockfield) ; .00999999999999801
;; (timed find-documents-5 'Susan-hockfield) ; 0.
;; (timed find-documents-6 'Susan-hockfield) ; 1.0000000000001563e-2

;; => Observation:
;; - search-any a word that is in the web is faster than search-any 
;;   a word that is not in the web
;; - search-any a word that is not in the web takes approximately 
;;   the same amount of time as seach-all, and the time is propotional 
;;   to the number of nodes
;; - building an index takes longer than searching directly, and the time
;;   is proportional to the number nodes
;; - once we have an index, searching is almost instantaneous

;; => Factors to consider when building a search engine
;; - Size of dataset
;; - Search / Update frequency
;; - User expectation

;;------------------------------------------------------------
;; utility for timing procedure calls.
;; returns the time in seconds

(define (timed f . args)
  (let ((start (runtime)))
    (let ((val (apply f args)))
      (newline)
      (display "time expended: ")
      (display (- (runtime) start))
      val)))

