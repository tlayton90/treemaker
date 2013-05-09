;;; Some example applications of the system

(define (no-consume! symbol is-terminal?) #!unspecific)
(define ((make-alist-is-terminal? rules) symbol) (not (assq symbol rules)))
(define ((make-alist-get-rules rules) symbol) (filter (lambda (e) (eq? (car e) symbol)) rules))
(define (tree-format symbol children) `(,symbol ,@children))


(define (rule in-symbol out-symbols)
  (list in-symbol (make-rule-handler in-symbol out-symbols))
)

(define (rule-set . rules)
  (make-symbol-handler
    no-consume!
	(make-alist-is-terminal? rules)
	(make-alist-get-rules rules)
	tree-format
  )
)

(define cfg (rule-set
  (rule 'S '(NP VP))
  (rule 'NP '(Det N))
  (rule 'Det '("the"))
  (rule 'N '("man"))
  (rule 'N '("bear"))
  (rule 'VP '(V))
  (rule 'V '("ran"))
  (rule 'V '("ate"))
))

(define (string-rule-set . rules)
  (make-symbol-handler
    no-consume!
	(make-alist-is-terminal? rules)
	(make-alist-get-rules rules)
	(lambda (symbol children) (apply string-append children))
  )
)

(define (rule-set-rule in-symbol rule-set)
  (list in-symbol (lambda (consume! is-terminal? get-rules format) (rule-set in-symbol)))
)

(define number-cfg (rule-set
  (rule 'S '(NP VP))
  (rule 'NP '(Number N))
  (rule-set-rule 'Number (string-rule-set
    (rule 'Number '(Digit))
    (rule 'Number '(Tens "-" Digit))
    (rule 'Number '(Hundreds " " Digit))
    (rule 'Number '(Hundreds " " Tens "-" Digit))
    (rule 'Digit '("two"))
    (rule 'Digit '("three"))
    (rule 'Tens '("twenty"))
    (rule 'Tens '("thirty"))
    (rule 'Hundreds '(Digit "hundred"))
  ))
  (rule 'N '("men"))
  (rule 'VP '(V))
  (rule 'V '("ran"))
))

(define  ((make-cfg-parser root . rules) input)
  (let ((result
          ((make-symbol-handler
            (lambda (symbol is-terminal?)
              (if (is-terminal? symbol)   
                  (if (string-prefix? symbol input)
                    (amb-set! input (string-tail input (string-length symbol)))
                    (amb))))
            (lambda (symbol) (not (assq symbol rules)))
            (lambda (symbol) (filter (lambda (e) (eq? (car e) symbol)) rules))
            (lambda (symbol children) `(,symbol ,@children)))
            root)))
    (if (not (string-null? input))
        (amb)
        result))
)

(define cfg-parser (make-cfg-parser 'S 
                                    (rule 'S '(NP))
                                    (rule 'S '(NP VP))
                                    (rule 'NP '(Det N))
                                    (rule 'Det '("the"))
                                    (rule 'N '("man"))
                                    (rule 'N '("bear"))
                                    (rule 'VP '(V))
                                    (rule 'V '("ran"))
                                    (rule 'V '("ate"))))

(define (rule in-symbol out-symbols prob)
  (list in-symbol (make-rule-handler in-symbol out-symbols prob) prob)
)

(define (randomize r)
  (let ((total-prob (reduce + 0 (map caddr r))))
    (if (= total-prob 0)
      '()
	  (let ((random-prob (random total-prob)))
	    (let loop ((left-r '()) (right-r r) (excess-prob random-prob))
	      (if (< excess-prob (caddar right-r))
		    (cons (car right-r) (randomize (append (reverse left-r) (cdr right-r))))
		    (loop (cons (car right-r) left-r) (cdr right-r) (- excess-prob (caddar right-r)))
		  )
		)
	  )
	)
  )
)

(define (rule-set . rules)
  (make-symbol-handler
    no-consume!
	(make-alist-is-terminal? rules)
	(lambda (symbol) (randomize ((make-alist-get-rules rules) symbol)))
	(lambda (symbol children prob)
	  `(
	    ,symbol
		,@children
		,(fold-left * prob (map (lambda (n) (if (list? n) (list-ref n (- (length n) 1)) 1)) children))
	  )
	)
  )
)


(define pcfg (rule-set
  (rule 'S '(NP VP) 1)
  (rule 'NP '(Det N) 1)
  (rule 'Det '("the") 1)
  (rule 'N '("man") .75)
  (rule 'N '("bear") .25)
  (rule 'VP '(V) 1)
  (rule 'V '("ran") .75)
  (rule 'V '("ate") .25)
))

#|
(define pcfg (rule-set
  (rule '1 '("1" 2 "1") 3)
  (rule '2 '("2" 1 "2") 3)
  (rule '1 '() 1)
  (rule '2 '() 1)
))
|#

(define (edge in-symbol out-symbol)
  (list in-symbol (make-rule-handler in-symbol (list out-symbol))))

(define ((make-path-finder . edges) start goal)
  (define visited '())
  (with-breadth-first-schedule
    (lambda ()
      ((make-symbol-handler
        (lambda (symbol is-terminal?)
          (if (memq symbol visited)
            (amb)
            (set! visited (cons symbol visited))))
        (lambda (symbol)  (eq? symbol goal))
        (lambda (symbol) (filter (lambda (e) (eq? (car e) symbol)) edges))
        (lambda (symbol children) 
          (cons symbol
                (apply append 
                      (map (lambda (child) 
                              (if (list? child) 
                                  child
                                  (list child))) 
                            children)))))
        start))
  )
)

(define graph-searcher (make-path-finder
                                    (edge 'A 'B)
                                    (edge 'A 'C)
                                    (edge 'B 'A)
                                    (edge 'B 'D)
                                    (edge 'B 'E)
                                    (edge 'C 'F)
                                    (edge 'C 'G)
                                    (edge 'E 'H)))
                                    
									
									
									
;;; Not Working ;;;     
(define (edge-list in-symbol out-symbols)
  (list in-symbol (make-rule-handler in-symbol out-symbols))
)
                         
(define ((make-graph-explorer . edges) start)
  (define visited '())
  (with-breadth-first-schedule
    (lambda ()
      ((make-symbol-handler
        (lambda (symbol is-terminal?)
          (if (not (is-terminal? symbol))
            (set! visited (cons symbol visited))))
        (lambda (symbol) (or (memq symbol (cdr visited)) ((make-alist-is-terminal? edges) symbol)))
        (make-alist-get-rules edges)
		tree-format
		)
       start))))


(define graph-explorer (make-graph-explorer
  (edge-list 'A '(B C))
  (edge-list 'B '(D E))
  (edge-list 'C '(F G))
  (edge-list 'E '(H A))
))