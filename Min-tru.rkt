#lang plait

(module+ test
  (print-only-errors #t))

; abstract syntax for Truing language
(define-type TruExpr
  (tru-value [val : Boolean]) ; value
  (tru-and   [lhs : TruExpr] [rhs : TruExpr]) ; AND
  (tru-or    [lhs : TruExpr] [rhs : TruExpr]) ; OR
  (tru-not   [expr : TruExpr])) ; NOT

; tru-interpret
(define (tru-interpret [expr : TruExpr]) : Boolean
  (type-case TruExpr expr
    [(tru-value val) val]
    [(tru-and lhs rhs) (and (tru-interpret lhs) (tru-interpret rhs))]
    [(tru-or lhs rhs)  (or  (tru-interpret lhs) (tru-interpret rhs))]
    [(tru-not expr)    (not (tru-interpret expr))]))


; VARIABLES TruExpr
(define sample-tru-value1 (tru-value #f))
(define sample-tru-value2 (tru-value #t))
(define sample-tru-and1   (tru-and (tru-value #f) (tru-value #f)))
(define sample-tru-and2   (tru-and (tru-value #t) (tru-value #f)))
(define sample-tru-and3   (tru-and (tru-value #t) (tru-value #t)))
(define sample-tru-and4   (tru-and (tru-and (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-or1    (tru-or  (tru-value #f) (tru-value #f)))
(define sample-tru-or2    (tru-or  (tru-value #t) (tru-value #f)))
(define sample-tru-or3    (tru-or  (tru-value #t) (tru-value #t)))
(define sample-tru-or4    (tru-or  (tru-or  (tru-value #f) (tru-value #t)) (tru-value #f)))
(define sample-tru-not1   (tru-not (tru-value #t)))
(define sample-tru-not2   (tru-not (tru-value #f)))                         
(define sample-tru-not3   (tru-not
                          (tru-or  (tru-or  (tru-value #f) (tru-value #t))
                                   (tru-and (tru-value #f) (tru-value #f)))))

; TESTS tru-interpret
(module+ test
  (test (tru-interpret sample-tru-value1) #f)
  (test (tru-interpret sample-tru-value2) #t)
  (test (tru-interpret sample-tru-and1)   #f)
  (test (tru-interpret sample-tru-and2)   #f)
  (test (tru-interpret sample-tru-and3)   #t)
  (test (tru-interpret sample-tru-and4)   #f)
  (test (tru-interpret sample-tru-or1)    #f)
  (test (tru-interpret sample-tru-or2)    #t)
  (test (tru-interpret sample-tru-or3)    #t)
  (test (tru-interpret sample-tru-or4)    #t)
  (test (tru-interpret sample-tru-not1)   #f)
  (test (tru-interpret sample-tru-not2)   #t)
  (test (tru-interpret sample-tru-not3)   #f))


; concrete syntax for Truing language
; #t | #f
; {and #t #f}
; {or  #f #t}
; {not #t}
; {and {or  #t #f}  #f}
; {not {and #t {or  #f #t}}}
; (E)BNF (Extended) Backus-Naur Form
; <expression> ::= <value> | <and> | <or> | <not>
; <value>      ::= #t | #f
; <and>        ::= "{" "and" <expression> <expression> "}"
; <or>         ::= "{" "or"  <expression> <expression> "}"
; <not>        ::= "{" "not" <expression> "}"


; tru-parse
(define (tru-parse [s : S-Exp]) : TruExpr
  (cond
    [(s-exp-match? `{and ANY ANY} s) (tru-and   (tru-parse (second (s-exp->list s)))
                                                (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match? `{or  ANY ANY} s) (tru-or    (tru-parse (second (s-exp->list s)))
                                                (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match? `{not ANY}     s) (tru-not   (tru-parse (second (s-exp->list s))))]
    [(s-exp-match? `ANY           s) (tru-value (s-exp->boolean s))]))
    

; TESTS tru-parse
(module+ test
  (test (tru-parse `#t)           (tru-value #t))
  (test (tru-parse `#f)           (tru-value #f))
  (test (tru-parse `{and #f #f})  (tru-and (tru-value #f) (tru-value #f)))
  (test (tru-parse `{and #t #f})  (tru-and (tru-value #t) (tru-value #f)))
  (test (tru-parse `{and #t #t})  (tru-and (tru-value #t) (tru-value #t)))
  (test (tru-parse `{or #f #f})   (tru-or  (tru-value #f) (tru-value #f)))
  (test (tru-parse `{or #t #f})   (tru-or  (tru-value #t) (tru-value #f)))
  (test (tru-parse `{or #t #t})   (tru-or  (tru-value #t) (tru-value #t)))
  (test (tru-parse `{not  #t})    (tru-not (tru-value #t)))
  (test (tru-parse `{not  #f})    (tru-not (tru-value #f)))
  (test (tru-parse `{not {or {or #f #t} {and #f #t}}})
        (tru-not (tru-or
                  (tru-or  (tru-value #f) (tru-value #t))
                  (tru-and (tru-value #f) (tru-value #t))))))


; unparse
(define (unparse [tr : TruExpr]) : S-Exp
  (type-case TruExpr tr
    [(tru-value val)   (boolean->s-exp val)]
    [(tru-and lhs rhs) (list->s-exp (list `and (unparse lhs) (unparse rhs)))]
    [(tru-or lhs rhs)  (list->s-exp (list `or  (unparse lhs) (unparse rhs)))]
    [(tru-not expr)    (list->s-exp (list `not (unparse expr)))]))


; TESTS unparse
(module+ test
  (test `#f (unparse (tru-value #f)))
  (test `#t (unparse (tru-parse `#t)))
  (test `{and #f #f} (unparse (tru-and (tru-value #f) (tru-value #f))))
  (test `{or  #f #f} (unparse (tru-or  (tru-value #f) (tru-value #f))))
  (test `{not #f}    (unparse (tru-not (tru-value #f))))
  (test `{or  #f #t} (unparse (tru-parse `{or #f #t})))
  (test `{not {or {or #t #t} {and #f #t}}}
        (unparse (tru-not (tru-or (tru-or  (tru-value #t) (tru-value #t))
                                  (tru-and (tru-value #f) (tru-value #t))))))
  (test `{not {or {or #f #t} {and #t #t}}}
        (unparse (tru-parse `{not {or {or #f #t} {and #t #t}}}))))



;
; H2 Sugar
;
; abstract syntax for Min Truing language

(define-type MinTruExpr
  (min-tru-false)
  (min-tru-nand  [lhs : MinTruExpr] [rhs : MinTruExpr]))


; VARIABLES MinTruExpr
(define sample-min-tru-nand1 (min-tru-nand
                              (min-tru-false)
                              (min-tru-false)))   ; nand #f #f
(define sample-min-tru-nand2 (min-tru-nand
                              (min-tru-false)
                              (min-tru-nand
                               (min-tru-false)
                               (min-tru-false)))) ; nand #f #t
(define sample-min-tru-nand3 (min-tru-nand
                              (min-tru-nand
                               (min-tru-false)
                               (min-tru-false))
                              (min-tru-nand
                               (min-tru-false)
                               (min-tru-false)))) ; nand #t #t


; tru-desugar
(define (tru-desugar [tr : TruExpr]) : MinTruExpr
  (type-case TruExpr tr
    [(tru-value val)   (if
                        (equal? val #f)
                        (min-tru-false)
                        (min-tru-nand (min-tru-false)    (min-tru-false)))]
    [(tru-and lhs rhs) (min-tru-nand
                        (min-tru-nand (tru-desugar lhs)  (tru-desugar rhs))
                        (min-tru-nand (tru-desugar lhs)  (tru-desugar rhs)))]
    [(tru-or  lhs rhs) (min-tru-nand
                        (min-tru-nand (tru-desugar lhs)  (tru-desugar lhs))
                        (min-tru-nand (tru-desugar rhs)  (tru-desugar rhs)))]
    [(tru-not expr)    (min-tru-nand
                        (tru-desugar expr)
                        (tru-desugar expr))]))


; TESTS tru-desugar
(module+ test
  (test (tru-desugar sample-tru-value1) (min-tru-false))
  (test (tru-desugar sample-tru-value2) (min-tru-nand (min-tru-false) (min-tru-false)))
  (test (tru-desugar sample-tru-and1) (min-tru-nand
                                       (min-tru-nand  (min-tru-false) (min-tru-false))
                                       (min-tru-nand  (min-tru-false) (min-tru-false))))
  (test (tru-desugar sample-tru-and2) (min-tru-nand
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-false))
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-false))))
  (test (tru-desugar sample-tru-and3) (min-tru-nand
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-nand (min-tru-false) (min-tru-false)))
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-nand (min-tru-false) (min-tru-false)))))
  (test (tru-desugar sample-tru-and4) (min-tru-nand
                                       (min-tru-nand
                                        (min-tru-nand
                                         (min-tru-nand
                                          (min-tru-false)
                                          (min-tru-nand (min-tru-false) (min-tru-false)))
                                         (min-tru-nand
                                          (min-tru-false)
                                          (min-tru-nand (min-tru-false) (min-tru-false))))
                                        (min-tru-nand (min-tru-false) (min-tru-false)))
                                       (min-tru-nand
                                        (min-tru-nand
                                         (min-tru-nand
                                          (min-tru-false)
                                          (min-tru-nand (min-tru-false) (min-tru-false)))
                                         (min-tru-nand
                                          (min-tru-false)
                                          (min-tru-nand (min-tru-false) (min-tru-false))))
                                        (min-tru-nand (min-tru-false) (min-tru-false)))))
  (test (tru-desugar sample-tru-or1) (min-tru-nand
                                       (min-tru-nand (min-tru-false) (min-tru-false))
                                       (min-tru-nand (min-tru-false) (min-tru-false))))
  (test (tru-desugar sample-tru-or2) (min-tru-nand (min-tru-nand
                                                    (min-tru-nand (min-tru-false) (min-tru-false))
                                                    (min-tru-nand (min-tru-false) (min-tru-false)))
                                                   (min-tru-nand (min-tru-false) (min-tru-false))))
  (test (tru-desugar sample-tru-or3) (min-tru-nand
                                      (min-tru-nand
                                       (min-tru-nand (min-tru-false) (min-tru-false))
                                       (min-tru-nand (min-tru-false) (min-tru-false)))
                                      (min-tru-nand
                                       (min-tru-nand (min-tru-false) (min-tru-false))
                                       (min-tru-nand (min-tru-false) (min-tru-false)))))
  (test (tru-desugar sample-tru-or4) (min-tru-nand
                                      (min-tru-nand
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-nand
                                         (min-tru-nand (min-tru-false) (min-tru-false))
                                         (min-tru-nand (min-tru-false) (min-tru-false))))
                                       (min-tru-nand
                                        (min-tru-nand (min-tru-false) (min-tru-false))
                                        (min-tru-nand
                                         (min-tru-nand (min-tru-false) (min-tru-false))
                                         (min-tru-nand (min-tru-false) (min-tru-false)))))
                                      (min-tru-nand (min-tru-false) (min-tru-false))))
  (test (tru-desugar sample-tru-not1) (min-tru-nand
                                       (min-tru-nand (min-tru-false) (min-tru-false))
                                       (min-tru-nand (min-tru-false) (min-tru-false))))
  (test (tru-desugar sample-tru-not2) (min-tru-nand
                                       (min-tru-false)
                                       (min-tru-false))))


; min-tru-interpret
(define (min-tru-interpret [expr : MinTruExpr]) : Boolean
  (type-case MinTruExpr expr
    [(min-tru-false) #f]
    [(min-tru-nand  lhs rhs) (not
                              (and
                               (min-tru-interpret lhs)
                               (min-tru-interpret rhs)))]))


; TESTS min-tru-interpret
(module+ test
  (test (min-tru-interpret (min-tru-false)) #f)
  (test (min-tru-interpret sample-min-tru-nand1) #t)
  (test (min-tru-interpret sample-min-tru-nand2) #t)
  (test (min-tru-interpret sample-min-tru-nand3) #f)
  (test (min-tru-interpret (tru-desugar sample-tru-value1)) #f)
  (test (min-tru-interpret (tru-desugar sample-tru-value2)) #t)
  (test (min-tru-interpret (tru-desugar sample-tru-and1))   #f)
  (test (min-tru-interpret (tru-desugar sample-tru-and2))   #f)
  (test (min-tru-interpret (tru-desugar sample-tru-and3))   #t)
  (test (min-tru-interpret (tru-desugar sample-tru-and4))   #f)
  (test (min-tru-interpret (tru-desugar sample-tru-or1))    #f)
  (test (min-tru-interpret (tru-desugar sample-tru-or2))    #t)
  (test (min-tru-interpret (tru-desugar sample-tru-or3))    #t)
  (test (min-tru-interpret (tru-desugar sample-tru-or4))    #t)
  (test (min-tru-interpret (tru-desugar sample-tru-not1))   #f)
  (test (min-tru-interpret (tru-desugar sample-tru-not2))   #t)
  (test (min-tru-interpret (tru-desugar sample-tru-not3))   #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `#t)))   #t)
  (test (min-tru-interpret (tru-desugar (tru-parse `#f)))   #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `{and #f #f}))) #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `{and #t #f}))) #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `{and #t #t}))) #t)
  (test (min-tru-interpret (tru-desugar (tru-parse `{or  #f #f}))) #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `{or  #t #f}))) #t)
  (test (min-tru-interpret (tru-desugar (tru-parse `{or  #t #t}))) #t)
  (test (min-tru-interpret (tru-desugar (tru-parse `{not #t})))    #f)
  (test (min-tru-interpret (tru-desugar (tru-parse `{not #f})))    #t))