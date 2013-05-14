;;; Some example applications of the system

;;; Some default implementations of specification functions and helper functions ;;;
(define (no-consume! symbol is-terminal?) #!unspecific)
(define ((make-alist-is-terminal? rules) symbol) (not (assoc symbol rules)))
(define ((make-alist-get-rules rules) symbol) (filter (lambda (e) (equal? (car e) symbol)) rules))
(define (tree-format symbol children) `(,symbol ,@children))
(define (make-parse-stack input)
    (list (lambda (symbol is-terminal?)
            (if (is-terminal? symbol)   
              (if (string-prefix? symbol input)
                (amb-set! input (string-tail input (string-length symbol)))
                (amb))))
          (lambda (result)
            (if (not (string-null? input))
              (amb)
              result))))
(define (get-parse-stack-consume parse-stack) (car parse-stack))
(define (get-parse-stack-final-check parse-stack) (cadr parse-stack))

;;; Default rule implementation ;;;
(define (rule in-symbol out-symbols)
  (list in-symbol (make-rule-handler in-symbol out-symbols)))

;;; Basic CFG Generator ;;;
(define (rule-set . rules)
  (make-symbol-handler
    no-consume!
    (make-alist-is-terminal? rules)
    (make-alist-get-rules rules)
    tree-format))

;;; CFG Generator example ;;;
#|
  (define cfg (rule-set
    (rule 'S '(NP VP))
    (rule 'NP '(Det N))
    (rule 'Det '("the"))
    (rule 'N '("man"))
    (rule 'N '("bear"))
    (rule 'VP '(V))
    (rule 'V '("ran"))
    (rule 'V '("ate"))))

  ]=> (cfg 'S)
  ;Value 11: (S (NP (Det "the") (N "man")) (VP (V "ran")))
  ]=> (amb)
  ;Value 12: (S (NP (Det "the") (N "man")) (VP (V "ate")))
  ]=> (amb)
  ;Value 13: (S (NP (Det "the") (N "bear")) (VP (V "ran")))
  ]=> (amb)
  ;Value 14: (S (NP (Det "the") (N "bear")) (VP (V "ate")))
  ]=> (amb)
  ;No more alternatives
|#

;;; New rule that delegates symbol handling to its own CFG Generator ;;;
(define (rule-set-rule in-symbol rule-set)
  (list in-symbol (lambda (consume! is-terminal? get-rules format) (rule-set in-symbol))))

;;; CFG Generator that outputs concatenated string instead of tree ;;;
(define (string-rule-set . rules)
  (make-symbol-handler
    no-consume!
    (make-alist-is-terminal? rules)
    (make-alist-get-rules rules)
    (lambda (symbol children) (apply string-append children))))

;;; Nested rule example ;;;
#|
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
      (rule 'Hundreds '(Digit "hundred"))))
    (rule 'N '("men"))
    (rule 'VP '(V))
    (rule 'V '("ran"))))

  ]=> (number-cfg 'S)
  ;Value 11: (S (NP "two" (N "men")) (VP (V "ran")))
  ]=> (amb)
  ;Value 12: (S (NP "three" (N "men")) (VP (V "ran")))
  ]=> (amb)
  ;Value 13: (S (NP "twenty-two" (N "men")) (VP (V "ran")))
|#

;;; CFG Parser ;;;
(define  ((make-cfg-parser root . rules) input)
  (let* ((parse-stack (make-parse-stack input))
         (result
            ((make-symbol-handler
              (get-parse-stack-consume parse-stack)
              (make-alist-is-terminal? rules)
              (make-alist-get-rules rules)
              tree-format)
            root)))
    ((get-parse-stack-final-check parse-stack) result)))
    
;;; CFG Parser Example ;;;
#|
  (define cfg-parser (make-cfg-parser 'S 
                                      (rule 'S '(NP " " VP "."))
                                      (rule 'NP '(Det " " N))
                                      (rule 'Det '("the"))
                                      (rule 'N '("man"))
                                      (rule 'N '("bear"))
                                      (rule 'VP '(V))
                                      (rule 'V '("ran"))
                                      (rule 'V '("ate"))))

  ]=> (cfg-parser "the man ran.")
  ;Value 11:
    (S (NP (Det "the") " " (N "man")) " " (VP (V "ran")) ".")
|#

;;; New rule that carries a probabilistic weight ;;;
(define (p-rule in-symbol out-symbols prob)
  (list in-symbol (make-rule-handler in-symbol out-symbols prob) prob))

;;; Randomize the order of rules according to probabilistic weights ;;;
(define (randomize r)
  (let ((total-prob (reduce + 0 (map caddr r))))
    (if (= total-prob 0)
      '()
      (let ((random-prob (random total-prob)))
        (let loop ((left-r '()) (right-r r) (excess-prob random-prob))
          (if (< excess-prob (caddar right-r))
            (cons (car right-r) (randomize (append (reverse left-r) (cdr right-r))))
            (loop (cons (car right-r) left-r) (cdr right-r) (- excess-prob (caddar right-r)))))))))

;;; Probabilistic CFG Generator ;;;
(define (p-rule-set . rules)
  (make-symbol-handler
    no-consume!
    (make-alist-is-terminal? rules)
    (lambda (symbol) (randomize ((make-alist-get-rules rules) symbol)))
    (lambda (symbol children prob)
      `(,symbol
        ,@children
        ,(fold-left * prob (map (lambda (n)
                                  (if (list? n) 
                                      (list-ref n (- (length n) 1))
                                      1))
                                children))))))

;;; Probabilistic CFG Generator Examples ;;;
#|
  (define pcfg (p-rule-set
    (p-rule 'S '(NP VP) 1)
    (p-rule 'NP '(Det N) 1)
    (p-rule 'Det '("the") 1)
    (p-rule 'N '("man") .75)
    (p-rule 'N '("bear") .25)
    (p-rule 'VP '(V) 1)
    (p-rule 'V '("ran") .75)
    (p-rule 'V '("ate") .25)))
  
  ]=> (pcfg 'S)
  ;Value 11: (S (NP (Det "the" 1) (N "man" .75) .75) (VP (V "ran" .75) .75) .5675)
  ]=> (amb)
  ;Value 12: (S (NP (Det "the") 1) (N "man" .75) .75) (VP (V "ate" .25) .25) .1875)

  (define pcfg-palindrome (p-rule-set
    (p-rule '1 '("1" 2 "1") 0.75)
    (p-rule '2 '("2" 1 "2") 0.75)
    (p-rule '1 '() 0.25)
    (p-rule '2 '() 0.25)))
  
   ]=> (pcfg-palindrome '1)
   ;Value 43: (1 .25)
   ]=> (amb)
   ;Value 44: (1 "1" (2 "2" (1 "1" (2 .25) "1" .1875) "2" .140625) "1" .10546875)
|#

;;; Edge(rule analogue) ;;;
(define (edge in-symbol out-symbol)
  (list in-symbol (make-rule-handler in-symbol (list out-symbol))))

;;; Graph Searcher ;;;
(define ((make-path-finder . edges) start goal)
  (define visited '())
  (with-breadth-first-schedule
    (lambda ()
      ((make-symbol-handler
        (lambda (symbol is-terminal?)
          (if (memq symbol visited)
            (amb)
            (set! visited (cons symbol visited))))
        (lambda (symbol)  (equal? symbol goal))
        (make-alist-get-rules edges)
        (lambda (symbol children) 
          (cons symbol
                (apply append 
                      (map (lambda (child) 
                              (if (list? child) 
                                  child
                                  (list child))) 
                            children)))))
        start))))

;;; Graph Searcher Example ;;;
#|
  (define graph-searcher (make-path-finder
                                      (edge 'A 'B)
                                      (edge 'A 'C)
                                      (edge 'B 'A)
                                      (edge 'B 'D)
                                      (edge 'B 'E)
                                      (edge 'C 'F)
                                      (edge 'C 'G)
                                      (edge 'E 'H)))
                                      
  ]=> (graph-searcher 'A 'H)
  ;Value 12: (a b e h)
  ]=> (graph-searcher 'H 'A)
  ;Value: #f
|#
