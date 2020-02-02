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
    [(tru-or lhs rhs) (or (tru-interpret lhs) (tru-interpret rhs))]
    [(tru-not expr) (not (tru-interpret expr))]))

(define sample-tru-value (tru-value #f))
(define sample-tru-and (tru-and (tru-and (tru-value #f) (tru-value #t)) (tru-value #t)))
(define sample-tru-or (tru-or (tru-or (tru-value #f) (tru-value #t)) (tru-value #f)))
(define sample-tru-not (tru-not
                        (tru-or (tru-or (tru-value #f) (tru-value #t))
                                (tru-and (tru-value #f) (tru-value #f)))))

(test (tru-interpret sample-tru-value) #f)
(test (tru-interpret sample-tru-and) #f)
(test (tru-interpret sample-tru-or) #t)
(test (tru-interpret sample-tru-not) #f)








(define-type TruingExpr
  (truing-literal [data : String]) ; literal
  (truing-append [lhs : TruingExpr] [rhs : TruingExpr]) ; (string-append a b)
  (truing-reverse [expr : TruingExpr])) ; (rope-reverse s)

(define sample-truing-literal (truing-literal "hello"))
(define sample-truing-append (truing-append (truing-literal "Hello ") (truing-literal "world!")))
(define sample-truing-reverse (truing-reverse (truing-literal "radar")))
(define sample-truing-nested (truing-append (truing-reverse (truing-literal "olleh"))
                                        (truing-literal " there"))) ; AST

(define (truing-interpret [expr : TruingExpr]) : String
  (type-case TruingExpr expr
    [(truing-literal str) str]
    [(truing-append lsh rsh) (string-append (truing-interpret lsh) (truing-interpret rsh))]
    [(truing-reverse e) (list->string (reverse (string->list (truing-interpret e)))) ]))

(test (truing-interpret sample-truing-literal) "hello")
(test (truing-interpret sample-truing-append) "Hello world!")
(test (truing-interpret sample-truing-reverse) "radar")
(test (truing-interpret sample-truing-nested) "hello there")

; concrete syntax for Truing language
; "hello"
; {+ "hello" "there"}
; {& "racecar"}
; {& {+ "ab" "cd"}}
; {+ {& "ab"} "ck"}
; (E)BNF (Extended) Backus-Naur Form
; <expression> ::= <literal> | <append> | <reverse>
; <character> ::= "A" | "B" | "C" ... "a" | "b" | "c" ...
; <literal> ::= "\"" <character>* "\""
; <append> ::= "{" "+" <expression> <expression> "}"
; <reverse> ::= "{" "&" <expression> "}"

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