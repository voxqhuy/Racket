#lang plait

(print-only-errors #t)

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

(define sample-tru-value (tru-value #f))
(define sample-tru-and   (tru-and (tru-and (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-or    (tru-or  (tru-or  (tru-value #f) (tru-value #t)) (tru-value #f)))
(define sample-tru-not   (tru-not
                          (tru-or (tru-or  (tru-value #f) (tru-value #t))
                                  (tru-and (tru-value #f) (tru-value #f)))))

(test (tru-interpret sample-tru-value) #f)
(test (tru-interpret sample-tru-and)   #f)
(test (tru-interpret sample-tru-or)    #t)
(test (tru-interpret sample-tru-not)   #f)

; concrete syntax for Truing language
; #t | #f
; {& #t #f}
; {||  #f #t}
; {! #t}
; {& {||  #t #f} #f}
; {! {& #t {||  #f #t}}}
; (E)BNF (Extended) Backus-Naur Form
; <expression> ::= <value> | <and> | <or> | <not>
; <value> ::= "#t" | "#f"
; <and> ::= "{" "&" <expression> <expression> "}"
; <or> ::= "{" "||" <expression> <expression> "}"
; <not> ::= "{" "!" <expression> "}"

(define (tru-parse [s : S-Exp]) : TruExpr
  (cond
    [(s-exp-match? `Boolean      s) (tru-value (s-exp->boolean s))]
    [(s-exp-match? `{& ANY ANY}  s) (tru-and   (tru-parse (second (s-exp->list s)))
                                               (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match? `{|| ANY ANY} s) (tru-or    (tru-parse (second (s-exp->list s)))
                                               (tru-parse (third  (s-exp->list s))))]
    [(s-exp-match?  `{! ANY}      s) (tru-not   (tru-parse (second (s-exp->list s))))]))





(define (truing-parse [s : S-Exp]) : TruingExpr
  (cond
    [(s-exp-match? `String s)      (truing-literal (s-exp->string s))]
    [(s-exp-match? `{+ ANY ANY} s) (truing-append (truing-parse (second (s-exp->list s)))
                                                  (truing-parse (third (s-exp->list s))))]
    [(s-exp-match? `{& ANY} s)       (truing-reverse (truing-parse (second (s-exp->list s))))]))

(test (truing-parse `"hello") (truing-literal "hello"))
(test (truing-parse `"") (truing-literal ""))
(test (truing-parse `{+ "a" "b"})
      (truing-append (truing-literal "a")
                     (truing-literal "b")))