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





#| some example application ideas
a CFG parser
	get-rule returns an alternation of all matching rules
	consume! checks terminal symbols against the next symbol in the input sequence, and fails if it doesn't
a PCFG generator
	get-rule uses rule probabilities to return a weighted-random-order alternation of all matching rules
various graph operations
|#