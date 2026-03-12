;; OptionSpec ([short : char?]
;              [long : string?]
;              [help : string?]
;              [flag? : bool?]
;              [arg-name : string? | #f]
;              [default: string? | bool?])
(struct OptionSpec (short long help flag? arg-name default) #:transparent)

;; PositionalSpec ([arg-name : string?]
;                  [optional? : bool?])
(struct PositionalSpec (arg-name optional?) #:transparent)

;; ParsedArgs ([ positionals : (listof string?) ]
;              [ options : hash? ]
;              [ specs : (listof ArgSpec?) ])
(struct ParsedArgs (positionals options specs) #:transparent)

(define-syntax argparse
  (syntax-rules (o: p:)
    [(command-line 
       (o: short long help flag? arg-name default) ...
       (p: pos-name optional?) ...)
     (make-arg-parser 
       #:options (list (OptionSpec short long help flag? arg-name default) ...)
       #:positionals (list (PositionalSpec pos-name optional?) ...))]))

(define (make-arg-parser #:options opt-specs #:positionals pos-specs)
  (let ([args (current-command-line-arguments)]
        [results (make-hash)]
        [positionals '()])
    (for-each (lambda (spec) 
                (hash-insert! results (OptionSpec-long spec) (OptionSpec-default spec)))
              opt-specs)
    (ParsedArgs (reverse positionals) results (append opt-specs pos-specs))))
