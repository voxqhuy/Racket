#lang plait

(print-only-errors #t)

; abstract syntax for Truing language

(define-type TruingExpr
  (truing-literal [data : String]) ; literal
  (truing-append [lhs : TruingExpr] [rhs : TruingExpr]) ; (string-append a b)
  (truing-reverse [expr : TruingExpr])) ; (rope-reverse s)

