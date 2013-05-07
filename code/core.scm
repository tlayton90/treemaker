(load "ambsch")

(define ((make-symbol-handler consume! is-terminal? get-rules format) symbol)
  (consume! symbol is-terminal?)
  (if (is-terminal? symbol)
    symbol
	((cadr (amb-element (get-rules symbol))) consume! is-terminal? get-rules format)
  )
)

(define ((make-rule-handler in-symbol out-symbols . args) consume! is-terminal? get-rules format)
  (apply format `(
    ,in-symbol
	,(map (make-symbol-handler consume! is-terminal? get-rules format) out-symbols)
	,@args
  ))
)