#lang plait

(module+ test
  (print-only-errors #t))

; Abstract syntax for Curly language
(define-type Value
  (numV  [n : Number])
  (closV [params : (Listof Symbol)]
         [body : Exp]
         [env  : Env]))

(define-type Exp
  (numE  [n : Number])
  (idE   [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (letE  [ids  : (Listof Symbol)] 
         [exps : (Listof Exp)]
         [body : Exp])
  (letE* [ids  : (Listof Symbol)] 
         [exps : (Listof Exp)]
         [body : Exp])
  (lamE  [params   : (Listof Symbol)]
         [body : Exp])
  (appE  [fun  : Exp]
         [args : (Listof Exp)]))

(define-type Binding
  (bind [name : Symbol]
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


; concrete syntax for Curly language
; 5
; x
; {+ 2 3}
; {* 2 3}
; {let {[x {+ 1 2}]} y}
; {let* {[y 3]} x}
; {add-all 2 4 5 7}
; {lambda {x} 9}
; {let* {[x 5]} {let {[y 6]} x}}
; {let {[add-x {lambda {y} {+ x y}}] [x 2] {add-x 3})
; (E)BNF (Extended) Backus-Naur Form
; <expression> ::= <number> | <id> | <plus> | <multiply>
;                  <let> | <let*> | <lambda> | <call>
; <number>     ::= 0 | 1 | 2 ...
; <character>  ::= "A" | "B" | "C" ... "a" | "b" | "c" ...
; <id>         ::= <character>+
; <plus>       ::= "{" "+" <expression> <expression> "}"
; <multiply>   ::= "{" "*" <expression> <expression> "}"
; <binding>    ::= "[" <id> <expression> "]"
; <let>        ::= "{" "let" "{" <binding>* "}" <expression> "}"
; <let*>       ::= "{" "let*" "{" <binding>* "}" <expression> "}"
; <lambda>     ::= "{" "lambda" "{" <id>* "}" <expression> "}"
; <call>       ::= "{" <id> <expression> "}"

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
            (parse (third  (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third  (s-exp->list s))))]
    [(s-exp-match? `{let {[SYMBOL ANY] ...} ANY} s)
     (let ([bindings (s-exp->list (second (s-exp->list s)))])
       (letE (map s-exp->symbol (map first (map s-exp->list bindings)))
             (map parse (map second (map s-exp->list bindings)))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{let* {[SYMBOL ANY] ...} ANY} s)
     (let ([bindings (s-exp->list (second (s-exp->list s)))])
       (letE* (map s-exp->symbol (map first (map s-exp->list bindings)))
              (map parse (map second (map s-exp->list bindings)))
              (parse (third (s-exp->list s)))))]
    [(s-exp-match? `{lambda {SYMBOL ...} ANY} s)
     (lamE (s-exp-list->symbol-list (s-exp->list (second (s-exp->list s))))
           (parse (third  (s-exp->list s))))]
    [(s-exp-match? `{ANY ANY ...} s)
     (appE (parse (first  (s-exp->list s)))
           (let ([arg-list (rest (s-exp->list s))])
             (cond
               [(empty? arg-list) empty]
               [(cons?  arg-list) (map parse arg-list)])))]
    [else (error 'parse "invalid input")]))

;; TESTS parse
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
  
  (test (parse `{let {} 
                  y}) ; 0 bindings
        (letE empty empty
              (idE 'y)))
  (test (parse `{let {[x {+ 1 2}]} 
                  y}) ; 1 binding
        (letE (list 'x) (list (plusE (numE 1) (numE 2)))
              (idE 'y)))
  
  (test (parse `{let* {[z {+ 1 2}] [x {* 2 1}]}
                  y}) ; multiple bindings
        (letE* (list 'z 'x)
               (list (plusE (numE 1) (numE 2)) (multE (numE 2) (numE 1)))
               (idE 'y)))
  
  (test (parse `{lambda {} 9})      ; 0 arguments
        (lamE empty (numE 9)))
  (test (parse `{lambda {x} {* 3 x}})     ; 1 argument
        (lamE (list 'x) (multE (numE 3) (idE 'x))))
  (test (parse `{lambda {x y z} 9}) ; multiple arguments
        (lamE (list 'x 'y 'z) (numE 9)))
  
  (test (parse `{double})           ; 0 arguments
        (appE (idE 'double) empty))
  (test (parse `{double 9})         ; 1 argument
        (appE (idE 'double) (list (numE 9))))
  (test (parse `{double 9 10 11})   ; multiple arguments
        (appE (idE 'double) (list (numE 9) (numE 10) (numE 11))))
  (test/exn (parse `{})
            "invalid input")
  ; intergration
  (test (parse `{let {[x 1]}
                  {let {[add-x {lambda {y} {+ x y}}]
                        [x 2]}
                    {add-x 3}}})
        (letE (list 'x)
              (list (numE 1))
              (letE (list 'add-x 'x)
                    (list (lamE (list 'y) (plusE (idE 'x) (idE 'y))) (numE 2))
                    (appE (idE 'add-x) (list (numE 3))))))
  (test (parse `{let {[x 1]}
                  {let* {[x 2]
                         [time-x {lambda {y} {* x y}}]}
                    {time-x 3}}})
        (letE (list 'x)
              (list (numE 1))
              (letE* (list 'x 'time-x)
                     (list (numE 2) (lamE (list 'y) (multE (idE 'x) (idE 'y))))
                     (appE (idE 'time-x) (list (numE 3)))))))

; map1+ : just like "map" but takes one extra argument for convenience
(define (map1+ [function : (Exp Env -> Value)]
               [exps     : (Listof Exp)]
               [env      : Env]) : (Listof Value)
  (type-case (Listof Exp) exps
    [empty empty]
    [(cons exp rst-exps) (cons (function exp env) (map1+ function rst-exps env))]))

; map1* : just like "map1+" but returns a list of lists of values
; given: some-func '(1 2 3) some-env, expected: '('(list 1 2 3) '(2 3) '(3))
(define (map1* [function : (Exp Env -> Value)]
               [exps     : (Listof Exp)]
               [env      : Env]) : (Listof (Listof Value))
  (cond
    [(empty? exps) empty]
    [(cons? exps) (append (list (map1+ function exps env)) (map1* function (rest exps) env))]))

; map2* : just like "map2" but takes an element and a list
; apply the function on the element and each element of the list
; return a new list
(define (bind-list [id : Symbol] [values : (Listof Value)]) : (Listof Binding)
  (type-case (Listof Value) values
    [empty empty]
    [(cons val rst-vals) (cons (bind id val) (bind-list id rst-vals))]))

; flat-map : flatten a list
; given: '('(1) '(2 3)), expected: '(1 2 3)
(define (flat-map [a : (Listof (Listof Binding))]) : (Listof Binding)
  (type-case (Listof (Listof Binding)) a
    [empty empty]
    [(cons bs rst-bs) (append bs (flat-map rst-bs))]))
  

;; interp ----------------------------------------
(define (interp [a : Exp] [env : Env]) : Value
  (type-case Exp a
    [(numE n) (numV n)]
    [(idE s)  (lookup s env)]
    [(plusE l r) (num+ (interp l env) (interp r env))]
    [(multE l r) (num* (interp l env) (interp r env))]
    [(letE  ids exps body) (interp body
                                   (append
                                    (map2 bind ids (map1+ interp exps env))
                                    env))]
    [(letE* ids exps body) (interp body
                                   (append
                                    (flat-map (map2 bind-list ids (map1* interp exps env)))
                                    env))]
    [(lamE args body) (closV args body env)]
    [(appE fun argsE) (type-case Value (interp fun env)
                        [(closV argsV body c-env)
                         (interp body
                                 (append
                                  (map2 bind argsV
                                        (list (interp (first argsE) env))) ; TODO how to (interp exps env) foldl only takes <element> <list>
                                  c-env))]
                        [else (error 'interp "not a function")])]))

; TESTS interp
(module+ test
  (test (interp (parse `2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
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
  (test (interp (parse `{lambda {x y z} {* x {+ y z}}})
                mt-env)
        (closV (list 'x 'y 'z) (multE (idE 'x) (plusE (idE 'y) (idE 'z))) mt-env))
  (test (interp (parse `{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse `{let* {[x 2]
                               [time-x {lambda {y} {* 3 y}}]}
                          {time-x 3}})
                mt-env)
        (numV 9))
  (test (interp (parse `{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse `{let {[x 1]}
                          {let* {[x 2]
                                 [add-x {lambda {y} {+ x y}}]}
                            {add-x 3}}})
                mt-env)
        (numV 4))
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

; TESTS num+ and num*
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
                       [(symbol=? n (bind-name b))
                        (bind-val b)]
                       [else (lookup n rst-env)])]))

; TESTS lookup
(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))