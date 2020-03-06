#lang plait

(module+ test
  (print-only-errors #t))

; Abstract syntax for Curly language
(define-type Value
  (numV  [n : Number])
  (closV [arg  : (Listof Symbol)]
         [body : Exp]
         [env  : Env]))

(define-type Exp
  (numE  [n : Number])
  (idE   [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE  [n : Symbol] 
         [rhs : Exp]
         [body : Exp])
  (lamE  [n : (Listof Symbol)]
         [body : Exp])
  (appE  [fun : Exp]
         [arg : Exp]))

(define-type Binding
  (bind [name : (Listof Symbol)]
        [val  : Value]))

(define-type-alias Env (Listof Binding))

(define mt-env empty)
(define extend-env cons)


;; helpers ----------------------------------------

; (Listof S-Exp) -> (Listof Symbol)
; convert a list of S-Exps to a list of Symbols
; given: (list `x)        expected: (list 'x)
; given: (list `x `y `z)  expected: (list 'x 'y 'z)
(define (s-exp-list->symbol-list [expressions : (Listof S-Exp)]) : (Listof Symbol)
  (type-case (Listof S-Exp) expressions
    [empty empty]
    [(cons exp lst) (cons (s-exp->symbol exp) (s-exp-list->symbol-list lst))]))

; TESTS for s-exp-list->symbol-list
(module+ test
  (test (s-exp-list->symbol-list (list `x)) (list 'x))
  (test (s-exp-list->symbol-list (list `x `y)) (list 'x 'y))
  (test (s-exp-list->symbol-list (list `x `y `z)) (list 'x 'y 'z)))

; Symbol (Listof Symbol) -> Boolean
; check if a list contains a specific symbol
; given: 'a (list 'x 'y)     expected: false
; given: 'x (list 'x 'y 'z)  expected: true
(define (contains? [s : Symbol] [lst : (Listof Symbol)]) : Boolean
  (cond
    [(empty? lst) #f]
    [(cons?  lst) (or (equal?    s (first lst))
                      (contains? s (rest  lst)))]))

; TESTS for contains?
(module+ test
  (test (contains? 'a empty)           #f)
  (test (contains? 'a (list 'x))       #f)
  (test (contains? 'x (list 'x 'y))    #t)
  (test (contains? 'z (list 'x 'y 'z)) #t))


;; parse ----------------------------------------
; S-Exp -> Curly
; parse an S-Exp into a variant of Curly language
; given: `5,                  expected: (numE 5)
; given: `{* 2 3},            expected: (multE (numE 2) (numE 3))
; given: `{let .......}
; given: `{lambda {x y z} 9}, expected: (lamE (list 'x 'y 'z) (numE 9)
(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letE (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL} ANY} s)
     (lamE (s-exp-list->symbol-list (s-exp->list (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY} s)
     (appE (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{let {[x {+ 1 2}]}
                  y})
        (letE 'x (plusE (numE 1) (numE 2))
              (idE 'y)))
  (test (parse `{lambda {x} 9})
        (lamE (list 'x) (numE 9)))
  (test (parse `{double 9})
        (appE (idE 'double) (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s) (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(letE n rhs body) (interp body
                               (extend-env
                                (bind (list n) (interp rhs env))
                                env))]
    [(lamE n body) (closV n body env)]
    [(appE fun arg) (type-case Value (interp fun env)
                      [(closV n body c-env)
                       (interp body
                               (extend-env
                                (bind n
                                      (interp arg env))
                                c-env))]
                      [else (error 'interp "not a function")])]))

(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind (list 'x) (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse `{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse `{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse `{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse `{lambda {x} {+ x x}})
                mt-env)
        (closV (list 'x) (plusE (idE 'x) (idE 'x)) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse `{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse `{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse `{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse `{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (Number Number -> Number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : Symbol] [env : Env]) : Value
  (type-case (Listof Binding) env
   [empty (error 'lookup "free variable")]
   [(cons b rst-env) (cond
                       [(contains? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind (list 'x) (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind (list 'x) (numV 9))
                    (extend-env (bind (list 'x) (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind (list 'x) (numV 9))
                    (extend-env (bind (list 'y) (numV 8)) mt-env)))
        (numV 8)))