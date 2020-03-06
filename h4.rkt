#lang plait

(module+ test
  (print-only-errors #t))

;
; abstract syntax for Truing language
;

; TruExpr
(define-type TruExpr
  (tru-value [val : Boolean]) ; value
  (tru-not   [expr : TruExpr]) ; NOT
  (tru-and   [lhs : TruExpr] [rhs : TruExpr]) ; AND
  (tru-or    [lhs : TruExpr] [rhs : TruExpr]) ; OR
  (tru-nand  [lhs : TruExpr] [rhs : TruExpr]) ; NAND
  (tru-nor   [lhs : TruExpr] [rhs : TruExpr]) ; NOR
  (tru-xor   [lhs : TruExpr] [rhs : TruExpr]) ; XOR
  (tru-xnor  [lhs : TruExpr] [rhs : TruExpr]) ; XNOR
  (tru-imply [lhs : TruExpr] [rhs : TruExpr]) ; IMPLY
  (tru-eq    [lhs : TruExpr] [rhs : TruExpr]) ; ==
  (tru-maj   [a   : TruExpr] [b   : TruExpr] [c : TruExpr]) ; MAJ
  (tru-id    [name  : Symbol]) ; x
  (tru-call0  [fname : Symbol]) ; {my-false}
  (tru-call1  [fname : Symbol] [arg  : TruExpr]) ; {my-not false}
  (tru-call2  [fname : Symbol] [arg1 : TruExpr] [arg2 : TruExpr]) ; {my-xor b1 b2}
  (tru-call3  [fname : Symbol] [arg1 : TruExpr] [arg2 : TruExpr] [arg3 : TruExpr])) ; {my-majority a b c}




; TruDefinition
(define-type TruDefinition
  (tru-function0 [name : Symbol]
                 [body : TruExpr])
  (tru-function1 [name : Symbol]
                 [arg  : Symbol]
                 [body : TruExpr])
  (tru-function2 [name : Symbol]
                 [arg1 : Symbol]
                 [arg2 : Symbol]
                 [body : TruExpr])
  (tru-function3 [name : Symbol]
                 [arg1 : Symbol]
                 [arg2 : Symbol]
                 [arg3 : Symbol]
                 [body : TruExpr]))




; HELPER tru-lookup
(define (tru-lookup [s : Symbol] [defs : (Listof TruDefinition)]) : TruDefinition
  (type-case (Listof TruDefinition) defs
    [empty (error 'tru-lookup "undefined function")]
    [(cons def rst-defs)
     (type-case TruDefinition def
       [(tru-function0 name body)
        (if (symbol=? s name )
            def
            (tru-lookup s rst-defs))]
       [(tru-function1 name arg body)
        (if (symbol=? s name )
            def
            (tru-lookup s rst-defs))]
       [(tru-function2 name arg1 arg2 body)
        (if (symbol=? s name )
            def
            (tru-lookup s rst-defs))]
       [(tru-function3 name arg1 arg2 arg3 body)
        (if (symbol=? s name )
            def
            (tru-lookup s rst-defs))])]))


; HELPER tru-substitute
(define (tru-substitute [what : TruExpr] [for : Symbol] [in : TruExpr]) : TruExpr
  (type-case TruExpr in
    [(tru-value val) in]
    [(tru-not   e)   (tru-not   (tru-substitute what for e))]
    [(tru-and   l r) (tru-and   (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-or    l r) (tru-or    (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-nand  l r) (tru-nand  (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-nor   l r) (tru-nor   (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-xor   l r) (tru-xor   (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-xnor  l r) (tru-xnor  (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-imply l r) (tru-imply (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-eq    l r) (tru-eq    (tru-substitute what for l)
                                (tru-substitute what for r))]
    [(tru-maj a b c) (tru-maj   (tru-substitute what for a)
                                (tru-substitute what for b)
                                (tru-substitute what for c))]
    [(tru-id s) (if (equal? s for)
                    what
                    in)]
    [(tru-call0 f) (tru-call0 f)]
    [(tru-call1 f arg) (tru-call1 f (tru-substitute what for arg))]
    [(tru-call2 f arg1 arg2) (tru-call2 f
                                        (tru-substitute what for arg1)
                                        (tru-substitute what for arg2))]
    [(tru-call3 f arg1 arg2 arg3) (tru-call3 f
                                             (tru-substitute what for arg1)
                                             (tru-substitute what for arg2)
                                             (tru-substitute what for arg3))]))

; TESTS tru-substitute
(module+ test
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `false))
        (tru-value #f))
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `x))
        (tru-value #t))
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `y))
        (tru-id 'y))
  (test (tru-substitute (tru-parse-expression `false) 'x (tru-parse-expression `{not x}))
        (tru-parse-expression `{not false}))
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `{and x y}))
        (tru-parse-expression `{and true y}))
  (test (tru-substitute (tru-parse-expression `false) 'y (tru-parse-expression `{or x y}))
        (tru-parse-expression `{or x false}))
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `{nand x y}))
        (tru-parse-expression `{nand true y}))
  (test (tru-substitute (tru-parse-expression `true)  'y (tru-parse-expression `{nor x y}))
        (tru-nor  (tru-id 'x) (tru-value #t)))
  (test (tru-substitute (tru-parse-expression `false) 'x (tru-parse-expression `{xor x y}))
        (tru-parse-expression `{xor false y}))
  (test (tru-substitute (tru-parse-expression `false) 'y (tru-parse-expression `{xnor x y}))
        (tru-xnor (tru-id 'x) (tru-value #f)))
  (test (tru-substitute (tru-parse-expression `false) 'x (tru-parse-expression `{implies x y}))
        (tru-parse-expression `{implies false y}))
  (test (tru-substitute (tru-parse-expression `true)  'y (tru-parse-expression `{equals x y}))
        (tru-eq (tru-id 'x) (tru-value #t)))
  (test (tru-substitute (tru-parse-expression `false) 'z (tru-parse-expression `{majority x y z}))
        (tru-parse-expression `{majority x y false}))
  (test (tru-substitute (tru-parse-expression `true)  'x (tru-parse-expression `{my-false}))
        (tru-parse-expression `{my-false}))
  (test (tru-substitute (tru-parse-expression `false) 'x (tru-parse-expression `{my-not x}))
        (tru-call1 'my-not (tru-value #f)))
  (test (tru-substitute (tru-parse-expression `true)  'y (tru-parse-expression `{my-xor x y}))
        (tru-parse-expression `{my-xor x true}))
  (test (tru-substitute (tru-parse-expression `false) 'z (tru-parse-expression `{my-majority x y z}))
        (tru-call3 'my-majority (tru-id 'x) (tru-id 'y) (tru-value #f))))

; VARIABLES TruExpr
(define sample-tru-value1 (tru-value #f))
(define sample-tru-value2 (tru-value #t))
(define sample-tru-not1   (tru-not   (tru-value #t)))
(define sample-tru-not2   (tru-not   (tru-value #f)))
(define sample-tru-and1   (tru-and   (tru-value #f) (tru-value #f)))
(define sample-tru-and2   (tru-and   (tru-value #t) (tru-value #f)))
(define sample-tru-and3   (tru-and   (tru-value #t) (tru-value #t)))
(define sample-tru-and4   (tru-and   (tru-and (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-or1    (tru-or    (tru-value #f) (tru-value #f)))
(define sample-tru-or2    (tru-or    (tru-value #t) (tru-value #f)))
(define sample-tru-or3    (tru-or    (tru-value #t) (tru-value #t)))
(define sample-tru-or4    (tru-or    (tru-or  (tru-value #f) (tru-value #t)) (tru-value #f)))
(define sample-tru-nand1  (tru-nand  (tru-value #f) (tru-value #f)))
(define sample-tru-nand2  (tru-nand  (tru-value #t) (tru-value #f)))
(define sample-tru-nand3  (tru-nand  (tru-value #t) (tru-value #t)))
(define sample-tru-nand4  (tru-nand  (tru-nand (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-nor1   (tru-nor   (tru-value #f) (tru-value #f)))
(define sample-tru-nor2   (tru-nor   (tru-value #t) (tru-value #f)))
(define sample-tru-nor3   (tru-nor   (tru-value #t) (tru-value #t)))
(define sample-tru-nor4   (tru-nor   (tru-nor (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-xor1   (tru-xor   (tru-value #f) (tru-value #f)))
(define sample-tru-xor2   (tru-xor   (tru-value #t) (tru-value #f)))
(define sample-tru-xor3   (tru-xor   (tru-value #t) (tru-value #t)))
(define sample-tru-xor4   (tru-xor   (tru-xor (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-xnor1  (tru-xnor  (tru-value #f) (tru-value #f)))
(define sample-tru-xnor2  (tru-xnor  (tru-value #t) (tru-value #f)))
(define sample-tru-xnor3  (tru-xnor  (tru-value #t) (tru-value #t)))
(define sample-tru-xnor4  (tru-xnor  (tru-xnor (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-imply1 (tru-imply (tru-value #f) (tru-value #f)))
(define sample-tru-imply2 (tru-imply (tru-value #t) (tru-value #f)))
(define sample-tru-imply3 (tru-imply (tru-value #t) (tru-value #t)))
(define sample-tru-imply4 (tru-imply (tru-imply (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-eq1    (tru-eq  (tru-value #f) (tru-value #f)))
(define sample-tru-eq2    (tru-eq  (tru-value #t) (tru-value #f)))
(define sample-tru-eq3    (tru-eq  (tru-value #t) (tru-value #t)))
(define sample-tru-eq4    (tru-eq  (tru-xnor (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-maj1   (tru-maj (tru-value #f) (tru-value #f) (tru-value #f)))
(define sample-tru-maj2   (tru-maj (tru-value #t) (tru-value #t) (tru-value #t)))
(define sample-tru-maj3   (tru-maj (tru-value #f) (tru-value #f) (tru-value #t)))
(define sample-tru-maj4   (tru-maj (tru-maj  (tru-value #f) (tru-value #t) (tru-value #t))
                                   (tru-value #t)
                                   (tru-value #f)))
(define sample-tru-id     (tru-id 'x))
(define sample-tru-call0  (tru-call0 'my-false))
(define sample-tru-call1  (tru-call1 'my-not      (tru-value #f)))
(define sample-tru-call2  (tru-call2 'my-xor      (tru-value #t) (tru-value #f)))
(define sample-tru-call3  (tru-call3 'my-majority (tru-value #t) (tru-value #t) (tru-value #f)))
(define sample-tru-nested (tru-maj (tru-eq  (tru-imply (tru-value #f) (tru-value #f))
                                            (tru-xnor  (tru-value #t) (tru-value #f)))
                                   (tru-xor (tru-nor   (tru-value #t) (tru-value #t))
                                            (tru-nand  (tru-nand (tru-value #f) (tru-value #t)) (tru-value #t)))
                                   (tru-or  (tru-and   (tru-value #f) (tru-value #f))
                                            (tru-not   (tru-value #f)))))


; VARIABLES TruDefinition
(module+ test
  (define sample-tru-def0 (tru-function0 'my-false
                                         (tru-value #f))) ; 0 arguments
  (define sample-tru-def1 (tru-function1 'my-not 'false   ; 1 argument
                                         (tru-not (tru-value #f)))) 
  (define sample-tru-def2 (tru-function2 'my-xor 'a 'b    ; 2 arguments
                                         (tru-parse-expression `{and {or a b} {not {and a b}}})))
  (define sample-tru-def3 (tru-function3 'my-majority 'a 'b 'c ; 3 arguments
                                         (tru-parse-expression `{or {or
                                                                     {and a b}
                                                                     {and a c}}
                                                                    {and  b c}}))))

; tru-interpret
(define (tru-interpret [expr : TruExpr] [defs : (Listof TruDefinition)]) : Boolean
  (type-case TruExpr expr
    [(tru-value val) val]
    [(tru-not   expr)    (not  (tru-interpret expr defs))]
    [(tru-and   lhs rhs) (and  (tru-interpret lhs  defs) (tru-interpret rhs defs))]
    [(tru-or    lhs rhs) (or   (tru-interpret lhs  defs) (tru-interpret rhs defs))]
    [(tru-nand  lhs rhs) (not  (tru-interpret (tru-and  lhs rhs) defs))]
    [(tru-nor   lhs rhs) (not  (tru-interpret (tru-or   lhs rhs) defs))]
    [(tru-xor   lhs rhs) (and  (tru-interpret (tru-or   lhs rhs) defs)
                               (tru-interpret (tru-nand lhs rhs) defs))]
    [(tru-xnor  lhs rhs) (tru-interpret (tru-nand (tru-or   lhs rhs)
                                                  (tru-nand lhs rhs))
                                        defs)]
    [(tru-imply lhs rhs) (tru-interpret (tru-or (tru-not  lhs) rhs) defs)]
    [(tru-eq    lhs rhs) (equal? (tru-interpret lhs defs) (tru-interpret rhs defs))]
    [(tru-maj   a b c)   (tru-interpret (tru-or (tru-or
                                                 (tru-and a b)
                                                 (tru-and a c))
                                                (tru-and  b c))
                                        defs)]
    [(tru-id id) (error 'tru-interpret "unbound name")]
    [(tru-call0  f)
     (local [(define def (tru-lookup f defs))]
       (tru-interpret (tru-function0-body def)
                      defs))]
    [(tru-call1  f arg)
     (local [(define def (tru-lookup f defs))]
       (tru-interpret (tru-substitute (tru-value (tru-interpret arg defs))
                                      (tru-function1-arg def)
                                      (tru-function1-body def))
                      defs))]
    [(tru-call2  f arg1 arg2)
     (local [(define def (tru-lookup f defs))]
       (tru-interpret
        (tru-substitute (tru-value (tru-interpret arg2 defs))
                        (tru-function2-arg2 def)
                        (tru-substitute (tru-value (tru-interpret arg1 defs))
                                        (tru-function2-arg1 def)
                                        (tru-function2-body def)))
        defs))]
    [(tru-call3  f arg1 arg2 arg3)
     (local [(define def (tru-lookup f defs))]
       (tru-interpret
        (tru-substitute (tru-value (tru-interpret arg3 defs))
                       (tru-function3-arg3 def)
                       (tru-substitute (tru-value (tru-interpret arg2 defs))
                                       (tru-function3-arg2 def)
                                       (tru-substitute (tru-value (tru-interpret arg1 defs))
                                                       (tru-function3-arg1 def)
                                                       (tru-function3-body def))))
        defs))]))
                         

; TESTS tru-interpret
(module+ test
  (test (tru-interpret sample-tru-value1 empty) #f)
  (test (tru-interpret sample-tru-value2 empty) #t)
  (test (tru-interpret sample-tru-not1   empty) #f)
  (test (tru-interpret sample-tru-not2   empty) #t)
  (test (tru-interpret sample-tru-and1   empty) #f)
  (test (tru-interpret sample-tru-and2   empty) #f)
  (test (tru-interpret sample-tru-and3   empty) #t)
  (test (tru-interpret sample-tru-and4   empty) #f)
  (test (tru-interpret sample-tru-or1    empty) #f)
  (test (tru-interpret sample-tru-or2    empty) #t)
  (test (tru-interpret sample-tru-or3    empty) #t)
  (test (tru-interpret sample-tru-or4    empty) #t)
  (test (tru-interpret sample-tru-nand1  empty) #t)
  (test (tru-interpret sample-tru-nand2  empty) #t)
  (test (tru-interpret sample-tru-nand3  empty) #f)
  (test (tru-interpret sample-tru-nand4  empty) #f)
  (test (tru-interpret sample-tru-nor1   empty) #t)
  (test (tru-interpret sample-tru-nor2   empty) #f)
  (test (tru-interpret sample-tru-nor3   empty) #f)
  (test (tru-interpret sample-tru-nor4   empty) #f)
  (test (tru-interpret sample-tru-xor1   empty) #f)
  (test (tru-interpret sample-tru-xor2   empty) #t)
  (test (tru-interpret sample-tru-xor3   empty) #f)
  (test (tru-interpret sample-tru-xor4   empty) #f)
  (test (tru-interpret sample-tru-xnor1  empty) #t)
  (test (tru-interpret sample-tru-xnor2  empty) #f)
  (test (tru-interpret sample-tru-xnor3  empty) #t)
  (test (tru-interpret sample-tru-xnor4  empty) #f)
  (test (tru-interpret sample-tru-imply1 empty) #t)
  (test (tru-interpret sample-tru-imply2 empty) #f)
  (test (tru-interpret sample-tru-imply3 empty) #t)
  (test (tru-interpret sample-tru-imply4 empty) #t)
  (test (tru-interpret sample-tru-eq1    empty) #t)
  (test (tru-interpret sample-tru-eq2    empty) #f)
  (test (tru-interpret sample-tru-eq3    empty) #t)
  (test (tru-interpret sample-tru-eq4    empty) #f)
  (test (tru-interpret sample-tru-maj1   empty) #f)
  (test (tru-interpret sample-tru-maj2   empty) #t)
  (test (tru-interpret sample-tru-maj3   empty) #f)
  (test (tru-interpret sample-tru-maj4   empty) #t)
  (test/exn (tru-interpret sample-tru-id empty) "tru-interpret: unbound name")
  (test (tru-interpret sample-tru-call0  (list sample-tru-def0)) #f)
  (test (tru-interpret sample-tru-call1  (list sample-tru-def1)) #t)
  (test (tru-interpret sample-tru-call2  (list sample-tru-def2)) #t)
  (test (tru-interpret sample-tru-call3  (list sample-tru-def3)) #t))


; concrete syntax for Truing language
; true | false
; {not  true}
; {and  true  false}
; {or   false false}
; {nand true  false}
; {nor  false false}
; {xor  true  true}
; {xnor false true}
; {implies    true  false}
; {equals     false false}
; {majority   true  true false}
; {and  {or   true  false} false}
; {not  {and  true  {or false true}}}
; {nand {nor  false true}  true}
; {xor  {xnor true  false} true}
; {implies  {equals false  true} false}
; {majority {not  {and false true}} {nor true true} {implies false false}}
; x
; {my-not false}
; (E)BNF (Extended) Backus-Naur Form
; <expression> ::= <value> | <not> | <and> | <or> | <nand> |
;                  <nor> | <xor> | <xnor> | <implies> |
;                  <equals> | <majority> | <id> | <call>
; <value>      ::= true | false
; <and>        ::= "{" "and"  <expression> <expression> "}"
; <or>         ::= "{" "or"   <expression> <expression> "}"
; <not>        ::= "{" "not"  <expression> "}"
; <nand>       ::= "{" "nand" <expression> <expression> "}"
; <nor>        ::= "{" "nor"  <expression> <expression> "}"
; <xor>        ::= "{" "xor"  <expression> <expression> "}"
; <xnor>       ::= "{" "xnor" <expression> <expression> "}"
; <implies>    ::= "{" "implies"  <expression> <expression> "}"
; <equals>     ::= "{" "equals"   <expression> <expression> "}"
; <majority>   ::= "{" "majority" <expression> <expression> <expression> "}"
; <id> ::= <character>+
; <call> ::= "{" <id> <expression> "}"
; <definition> ::= "{" "define" "{" <id> <id> "}" <expression> "}"

; tru-parse-expression
(define (tru-parse-expression [s : S-Exp]) : TruExpr
  (cond
    [(s-exp-match? `true  s) (tru-value #t)]
    [(s-exp-match? `false s) (tru-value #f)]
    [(s-exp-match? `{not  ANY}     s) (tru-not
                                       (tru-parse-expression (second (s-exp->list s))))]
    [(s-exp-match? `{and  ANY ANY} s) (tru-and
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{or   ANY ANY} s) (tru-or
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{nand ANY ANY} s) (tru-nand
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{nor  ANY ANY} s) (tru-nor
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{xor  ANY ANY} s) (tru-xor
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{xnor ANY ANY} s) (tru-xnor
                                       (tru-parse-expression (second (s-exp->list s)))
                                       (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{implies  ANY ANY} s) (tru-imply
                                           (tru-parse-expression (second (s-exp->list s)))
                                           (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{equals   ANY ANY} s) (tru-eq
                                           (tru-parse-expression (second (s-exp->list s)))
                                           (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{majority ANY ANY ANY} s) (tru-maj
                                               (tru-parse-expression (second (s-exp->list s)))
                                               (tru-parse-expression (third  (s-exp->list s)))
                                               (tru-parse-expression (fourth  (s-exp->list s))))]
    [(s-exp-match? `SYMBOL s)       (tru-id   (s-exp->symbol s))]
    [(s-exp-match? `{SYMBOL} s) (tru-call0    (s-exp->symbol (first (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (tru-call1 (s-exp->symbol (first (s-exp->list s)))
                (tru-parse-expression (second (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY} s)
     (tru-call2 (s-exp->symbol (first (s-exp->list s)))
                (tru-parse-expression (second (s-exp->list s)))
                (tru-parse-expression (third  (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY ANY ANY} s)
     (tru-call3 (s-exp->symbol (first (s-exp->list s)))
                (tru-parse-expression (second (s-exp->list s)))
                (tru-parse-expression (third  (s-exp->list s)))
                (tru-parse-expression (fourth (s-exp->list s))))]))
    

; TESTS tru-parse-expression
(module+ test
  (test (tru-parse-expression `true)         (tru-value #t))
  (test (tru-parse-expression `false)        (tru-value #f))
  (test (tru-parse-expression `{not  true})  (tru-not  (tru-value #t)))
  (test (tru-parse-expression `{not  false}) (tru-not  (tru-value #f)))
  (test (tru-parse-expression `{and  false false}) (tru-and  (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{and  true  false}) (tru-and  (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{and  true  true})  (tru-and  (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{or   false false}) (tru-or   (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{or   true  false}) (tru-or   (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{or   true  true})  (tru-or   (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{nand false false}) (tru-nand (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{nand true  false}) (tru-nand (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{nand true  true})  (tru-nand (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{nor  false false}) (tru-nor  (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{nor  true  false}) (tru-nor  (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{nor  true  true})  (tru-nor  (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{xor  false false}) (tru-xor  (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{xor  true  false}) (tru-xor  (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{xor  true  true})  (tru-xor  (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{xnor false false}) (tru-xnor (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{xnor true  false}) (tru-xnor (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{xnor true true})   (tru-xnor (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{implies  false false}) (tru-imply (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{implies  true  false}) (tru-imply (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{implies  true  true})  (tru-imply (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{equals   false false}) (tru-eq  (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{equals   true  false}) (tru-eq  (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{equals   true  true})  (tru-eq  (tru-value #t) (tru-value #t)))
  (test (tru-parse-expression `{majority false false false}) (tru-maj (tru-value #f) (tru-value #f) (tru-value #f)))
  (test (tru-parse-expression `{majority true  true  false}) (tru-maj (tru-value #t) (tru-value #t) (tru-value #f)))
  (test (tru-parse-expression `{majority true  true  true})  (tru-maj (tru-value #t) (tru-value #t) (tru-value #t))))


; tru-parse-definition
(define (tru-parse-definition [s : S-Exp]) : TruDefinition
  (cond
    [(s-exp-match? `{define {SYMBOL} ANY} s)
     (let* ([def-list (s-exp->list s)] ; (Listof S-Exp) (list `define `{my-false} `false)
            [header (map s-exp->symbol (s-exp->list (second def-list)))] ; (Listof Symbol) (list 'my-false 'false)
            [fname  (first header)]
            [body   (third def-list)]) ; S-Exp `false
       (tru-function0 fname
                     (tru-parse-expression body)))]
    
    [(s-exp-match? `{define {SYMBOL SYMBOL} ANY} s)
     (let* ([def-list (s-exp->list s)] ; (Listof S-Exp) (list `define `{my-not value} `{nand true value})
            [header (map s-exp->symbol (s-exp->list (second def-list)))] ; (Listof Symbol) (list my-not 'value)
            [fname  (first header)]
            [arg    (second header)]
            [body   (third def-list)]) ; S-Exp `{and x {not x}}
       (tru-function1 fname arg
                     (tru-parse-expression body)))]
    
    [(s-exp-match? `{define {SYMBOL SYMBOL SYMBOL} ANY} s)
     (let* ([def-list (s-exp->list s)] ; (Listof S-Exp) (list `define `{repeat x} `x)
            [header (map s-exp->symbol (s-exp->list (second def-list)))] ; (Listof Symbol) (list 'repeat 'x)
            [fname  (first header)]
            [arg1   (second header)]
            [arg2   (third header)]
            [body   (third def-list)]) ; S-Exp `{and x {not x}}
       (tru-function2 fname arg1 arg2
                     (tru-parse-expression body)))]
 
    [(s-exp-match? `{define {SYMBOL SYMBOL SYMBOL SYMBOL} ANY} s)
     (let* ([def-list (s-exp->list s)] ; (Listof S-Exp) (list `define `{repeat x} `x)
            [header (map s-exp->symbol (s-exp->list (second def-list)))] ; (Listof Symbol) (list 'repeat 'x)
            [fname  (first header)]
            [arg1   (second header)]
            [arg2   (third header)]
            [arg3   (fourth header)]
            [body   (third def-list)]) ; S-Exp `{and x {not x}}
       (tru-function3 fname arg1 arg2 arg3
                           (tru-parse-expression body)))]
   
    [else (error 'tru-parse-definition "Invalid function definition")]))

; VARIABLES tru-parse-definition
(define falsey
  (tru-parse-definition `{define {my-false} false}))
(define notty
  (tru-parse-definition `{define {my-not value} {nand true value}}))
(define xorry
  (tru-parse-definition `{define {my-xor b1 b2} {and {or b1 b2}
                                                     {not {and b1 b2}}}}))
(define majy
  (tru-parse-definition `{define {my-majority a b c} {or {or {and a b} {and a c}} {and b c}}}))

; TESTS tru-lookup
(module+ test
  (test (tru-lookup 'my-false (list falsey)) falsey)
  (test (tru-lookup 'my-false (list xorry notty majy falsey)) falsey)
  (test (tru-lookup 'my-not   (list notty)) notty)
  (test (tru-lookup 'my-not   (list notty falsey)) notty)
  (test (tru-lookup 'my-not   (list xorry notty)) notty)
  (test (tru-lookup 'my-xor   (list xorry notty)) xorry)
  (test (tru-lookup 'my-majority   (list majy)) majy)
  (test (tru-lookup 'my-majority   (list falsey xorry notty majy)) majy)
  (test/exn (tru-lookup 'my-not empty) "undefined function"))

; TESTS tru-parse-definition
(module+ test
  (test falsey
        (tru-function0 'my-false (tru-value #f)))
  (test notty
        (tru-function1 'my-not 'value
                      (tru-parse-expression `{nand true value})))
  (test xorry
        (tru-function2 'my-xor 'b1 'b2
                      (tru-parse-expression `{and {or b1 b2}
                                                  {not {and b1 b2}}})))
  (test majy
        (tru-function3 'my-majority 'a 'b 'c
                      (tru-parse-expression `{or {or {and a b}
                                                     {and a c}}
                                                 {and b c}})))
  (test/exn (tru-parse-definition `{define {majority w x y z} {major w x y}})
            "tru-parse-definition: Invalid function definition"))
