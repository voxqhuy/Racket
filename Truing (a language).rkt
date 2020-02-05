#lang plait

(module+ test
  (print-only-errors #t))

; abstract syntax for Truing language

(define-type TruExpr
  (tru-value [val : Boolean]) ; value
  (tru-and   [lhs : TruExpr] [rhs : TruExpr]) ; AND
  (tru-or    [lhs : TruExpr] [rhs : TruExpr]) ; OR
  (tru-not   [expr : TruExpr])) ; NOT

(define (tru-interpret [expr : TruExpr]) : Boolean
  (type-case TruExpr expr
    [(tru-value val) val]
    [(tru-and lhs rhs) (and (tru-interpret lhs) (tru-interpret rhs))]
    [(tru-or lhs rhs)  (or  (tru-interpret lhs) (tru-interpret rhs))]
    [(tru-not expr)    (not (tru-interpret expr))]))


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

; TESTS
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


(define (tru-parse [s : S-Exp]) : TruExpr
  (cond
    [(s-exp-match? `{and ANY ANY} s) (tru-and   (tru-parse (second (s-exp->list s)))
                                                (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match? `{or  ANY ANY} s) (tru-or    (tru-parse (second (s-exp->list s)))
                                                (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match? `{not ANY}     s) (tru-not   (tru-parse (second (s-exp->list s))))]
    [(s-exp-match? `ANY           s) (tru-value (s-exp->boolean s))]))
    

; TESTS
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
  (test (tru-parse `{not  #f})    (tru-not (tru-value #f))))


(define (unparse [tr : TruExpr]) : S-Exp
  (type-case TruExpr tr
    [(tru-value val)   (boolean->s-exp val)]
    [(tru-and lhs rhs) (list->s-exp (list `and (unparse lhs) (unparse rhs)))]
    [(tru-or lhs rhs)  (list->s-exp (list `or  (unparse lhs) (unparse rhs)))]
    [(tru-not expr)    (list->s-exp (list `not (unparse expr)))]))

