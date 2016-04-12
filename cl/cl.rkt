#lang racket/base
(require racket/contract/base)

;; Libraries
(require racket/pretty)
(define-syntax-rule (xxx v ...)
  (error 'xxx "~a" (pretty-format (list (cons 'v v) ...))))

(module define-type racket/base
  (require (for-syntax racket/base
                       syntax/parse
                       racket/list
                       racket/syntax)
           racket/contract/base
           racket/match)

  (begin-for-syntax
    (define-syntax-class variant
      #:attributes (? def v)
      (pattern v:id
               #:with v? (format-id #'v "~a?" #'v)
               #:with vv (generate-temporary #'v)
               #:with a-v (generate-temporary #'v)
               #:attr ? #'v?
               #:attr def
               (syntax/loc #'v
                 (begin (struct a-v ()
                          #:transparent
                          #:reflection-name 'v)
                        (define-match-expander v
                          (λ (stx)
                            (syntax-parse stx
                              [(_) (syntax/loc stx (a-v))]))
                          (make-rename-transformer #'vv))
                        (define vv (a-v))
                        (define (v? x) (eq? v x)))))
      (pattern (v:id [f:id ctc:expr] ...)
               #:with v? (format-id #'v "~a?" #'v)
               #:with a-v (generate-temporary #'v)
               #:with cv (generate-temporary #'v)
               #:with a-v? (format-id #'a-v "~a?" #'a-v)
               #:attr ? #'v?
               #:attr def
               (syntax/loc #'v
                 (begin (struct a-v (f ...)
                          #:transparent
                          #:reflection-name 'v)
                        (define v? a-v?)
                        (define-match-expander v
                          (λ (stx)
                            (syntax-parse stx
                              [(_ . x) (syntax/loc stx (a-v . x))]))
                          (make-rename-transformer #'cv))
                        (define (cv f ...)
                          (a-v (contract ctc f 'pos 'neg) ...)))))))

  (define-syntax define-type
    (λ (stx)
      (syntax-parse stx
        [(_ n:id v:variant ...)
         (with-syntax ([n? (format-id #'n "~a?" #'n)])
           (syntax/loc stx
             (begin
               (define (n? x)
                 (or (v.? x) ...))
               v.def ...
               (define-syntax n
                 (type-record (list #'v.v ...))))))])))

  (begin-for-syntax
    (struct type-record (vs))

    (define-syntax-class clause
      #:attributes (v)
      (pattern [(v:id . more) . body])))

  (define-syntax match-type
    (λ (stx)
      (syntax-parse stx
        [(_ ty v:id c:clause ...)
         #:declare ty (static type-record? "define-type type")
         (define ty-vs (type-record-vs (attribute ty.value)))
         (define the-vs (syntax->list #'(c.v ...)))
         (define missing
           (for/list ([req-v (in-list ty-vs)]
                      #:unless (member req-v the-vs free-identifier=?))
             req-v))
         (unless (empty? missing)
           (raise-syntax-error 'match-type
                               (format "missing clauses for ~a variants: ~a"
                                       (syntax-e #'ty)
                                       (map syntax-e missing))
                               stx))
         (syntax/loc stx
           (match v c ...))])))

  (provide define-type
           match-type))

(require (submod "." define-type))

;; Core Language
(define unsigned? exact-nonnegative-integer?)
(define signed? exact-integer?)
(define float? real?)

(define Field? symbol?)
(define Seal-Id? symbol?)
(define Var? symbol?)
(define CName? string?)

(define-type CPP
  (CHeader [cflags (listof string?)]
           [ldflags? (listof string?)]
           [pre (listof string?)]
           [litc string?]
           [post (listof string?)]))

(define <stdint.h>
  (CHeader '() '() '() "<stdint.h>" '()))

(define (Lval? x)
  (or ($aref? x)
      ($pref? x)
      ($vref? x)
      ($dref? x)
      ($sref? x)
      ($uref? x)))

(define-type Type
  ;; From C
  Size Char
  UI8 UI16 UI32 UI64
  SI8 SI16 SI32 SI64
  F32 F64 F128
  Void
  ;; xxx intptr_t?
  (Record [fields (listof (cons/c Field? Type?))])
  (Ptr [ty Type?])
  (Arr [ty Type?] [k unsigned?])
  (Union [tys (listof (cons/c Field? Type?))])
  (Fun [dom (listof (cons/c Var? Type?))] [rng Type?])
  ;; Extensions
  (Extern [h CHeader?] [n CName?])
  (Delay [-ty (-> Type?)])
  (Seal [n Seal-Id?] [ty Type?]))

(define (Union-ty ec u f)
  (match-define (Union tys) u)
  (match (assq f tys)
    [(cons _ t) t]
    [#f (ec-fail! ec (format "~e not a variant of union ~e" f u))]))
(define (Record-field-ty ec r f)
  (match-define (Record fs) r)
  (match (assq f fs)
    [(cons _ t) t]
    [#f (ec-fail! ec (format "~e not a field of record ~e" f r))]))

(define String (Ptr Char))
(define Bool (Seal 'Bool UI8))

(define-type Op1
  $! $neg $bneg)

(define (ec-check-op1-ty! ec o at)
  (xxx 'ec-check-op1-ty! ec o at))

(define-type Op2
  $+ $- $* $/ $%
  $== $!= $> $< $>= $<=
  $and $or
  $band $bior $bxor $bshl $bshr)

(define Integer-Types
  (list Size Char
        UI8 UI16 UI32 UI64
        SI8 SI16 SI32 SI64))
(define Numeric-Types
  (list* F32 F64 F128 Integer-Types))

(define (ec-check-op2-ty! ec at o bt)
  (cond
    [(memq o (list $+ $- $* $/ $% $== $!= $> $< $>= $<= $and $or))
     ;; These operations require equal types
     (ec-check-ty! ec at bt)
     (cond
       ;; These consumes and produce Bools
       [(memq o (list $and $or))
        (ec-check-ty! ec bt Bool)]
       ;; These consume numbers and produce Bools
       [(memq o (list $> $< $>= $<=))
        (ec-check-ty-in! ec bt Numeric-Types)
        Bool]
       ;; These consume numbers and pointers and produce Bools
       [(memq o (list $== $!=))
        (ec-check-ty-in-or-?! ec bt Numeric-Types Ptr?)
        Bool]
       ;; These consume integers and produce integers
       [(memq o (list $%))
        (ec-check-ty-in! ec bt Integer-Types)]
       ;; These consume numbers and produce the kind of number that went in
       [(memq o (list $+ $- $* $/))
        (ec-check-ty-in! ec bt Numeric-Types)]
       [else
        (xxx 'ec-check-op2-ty! ec at o bt)])]
    [else
     (xxx 'ec-check-op2-ty! ec at o bt)]))

(define-type Literal
  $NULL
  $zero-init
  ($struct [f->v (hash/c Field? Val?)])
  ($union [f->v (hash/c Field? Val?)])
  ($array [vs (vectorof Val?)]))

(define Val?
  (or/c boolean? unsigned? signed? float? char? string? Literal?))

(define ((unsigned/bits? bits) x)
  (and (exact-integer? x)
       (not (negative? x))
       (<= (integer-length x) bits)))
(define ((signed/bits? bits) x)
  (and (exact-integer? x)
       (<= (integer-length x) (sub1 bits))))

(define (val-? ec ct)
  (match ct
    [(Size) (val-? UI64)]
    [(Char) char?]
    [(UI8) (unsigned/bits? 8)]
    [(UI16) (unsigned/bits? 16)]
    [(UI32) (unsigned/bits? 32)]
    [(UI64) (unsigned/bits? 64)]
    [(SI8) (signed/bits? 8)]
    [(SI16) (signed/bits? 16)]
    [(SI32) (signed/bits? 32)]
    [(SI64) (signed/bits? 64)]
    [(F32) single-flonum?]
    [(F64) double-flonum?]
    [(F128) double-flonum?]
    [(Ptr (Char)) bytes?]
    [(Seal 'Bool (UI8)) boolean?]
    [else
     (ec-fail! ec (format "invalid type for value: ~e" ct))]))

(define (ec-check-val-ty! ec v ct)
  (cond
    [(Literal? ct)
     (xxx 'ec-check-val-ty! ec v ct)]
    [else
     (define ? (val-? ec ct))
     (unless (? v)
       (ec-fail! ec (format "~e should match ~e" v ?)))
     ct]))

(define-type Expr
  ($sizeof [ty Type?])
  ($offsetof [ty Record?] [f Field?])
  ($op1 [op Op1?] [arg Expr?])
  ($op2 [lhs Expr?] [op Op2?] [rhs Expr?])
  ($val [t Type?] [v Val?])
  ($app [rator Expr?] [rands (listof Expr?)])
  ($aref [lhs Expr?] [rhs Expr?])
  ($addr [arg Expr?])
  ($pref [arg Expr?])
  ($vref [v Var?])
  ($dref [d Decl?])
  ($ddref [-d (-> Decl?)])
  ($sref [obj Expr?] [f Field?])
  ($uref [obj Expr?] [f Field?])
  ($ife [test Expr?] [if1 Expr?] [if0 Expr?])
  ($seal [s Seal-Id?] [e Expr?])
  ($unseal [s Seal-Id?] [e Expr?]))

(define-type Stmt
  ($nop)
  ($seq [fst Stmt?] [snd Stmt?])
  ($do [e Expr?])
  ($if [test Expr?] [if1 Stmt?] [if0 Stmt?])
  ($while [test Expr?] [body Stmt?])
  ($let1 [ty Type?] [v Var?] [body Stmt?])
  ($set! [lhs Lval?] [rhs Expr?])
  ($ret [val Expr?])
  ($return))

(define-type Decl
  ($extern [h CHeader?] [n CName?])
  ($typedef [hn symbol?] [ty Type?])
  ($proc [hn symbol?] [ty Fun?] [body Stmt?])
  ($var [hn symbol?] [ty (and/c Type? (not/c Fun?))] [val Expr?]))

(define-type Unit
  ($cflags [flags (listof string?)] [u Unit?])
  ($ldflags [flags (listof string?)] [u Unit?])  
  ($exe [main $proc?])
  ($lib [ds (hash/c CName? Decl?)]))

;; Compiler
(require (prefix-in pp: pprint)
         racket/match
         racket/set)

(define NEST 2)

(define (gencsym [s 'c])
  (symbol->string (gensym (regexp-replace* #rx"[^A-Za-z_0-9]" (format "_~a" s) "_"))))

(struct emit-ctxt (cflags ldflags is ds n->d d->n d->ty v->vn v->ty))
(define (make-emit-ctxt)
  (emit-ctxt (mutable-set) (mutable-set)
             (mutable-seteq <stdint.h>) (mutable-seteq)
             (make-hash) (make-hasheq) (make-hasheq)
             (hasheq) (hasheq)))
(define (ec-add-cfs! ec a-cfs)
  (define cfs (emit-ctxt-cflags ec))
  (for ([e (in-list a-cfs)])
    (set-add! cfs e)))
(define (ec-add-lfs! ec a-lfs)
  (define lfs (emit-ctxt-ldflags ec))
  (for ([e (in-list a-lfs)])
    (set-add! lfs e)))
(define (ec-add-decl! ec d)
  (set-add! (emit-ctxt-ds ec) d))
(define (ec-add-named! ec n d)
  (define n->d (emit-ctxt-n->d ec))
  (define d->n (emit-ctxt-d->n ec))
  (if (hash-has-key? n->d n)
      (error 'ec-add-named! "~a is already used" n)
      (begin (hash-set! n->d n d)
             (hash-set! d->n d n))))
(define (ec-name! ec d)
  (define n->d (emit-ctxt-n->d ec))
  (define d->n (emit-ctxt-d->n ec))
  (define hn
    (match-type
     Decl d
     [($extern _ n)
      (hash-set! n->d n d)
      (hash-set! d->n d n)
      n]
     [($typedef hn _) hn]
     [($proc hn _ _) hn]
     [($var hn _ _) hn]))
  (define n (hash-ref! d->n d (λ () (gencsym hn))))
  (values (hash-has-key? n->d n)
          n))
(define (ec-extend-ns ec v*t)
  (match-define (cons v t) v*t)
  (define vn (gencsym v))
  (struct-copy emit-ctxt ec
               [v->vn (hash-set (emit-ctxt-v->vn ec) v vn)]
               [v->ty (hash-set (emit-ctxt-v->ty ec) v t)]))
(define (ec-extend-ns* ec ds)
  (for/fold ([ec ec])
            ([d (in-list ds)])
    (ec-extend-ns ec d)))
(define (ec-var-name ec v)
  (hash-ref (emit-ctxt-v->vn ec) v
            (λ ()
              (ec-fail! ec (format "Unbound identifier: ~e" v)))))
(define (ec-var-ty ec v)
  (hash-ref (emit-ctxt-v->ty ec) v
            (λ ()
              (ec-fail! ec (format "Unbound identifier: ~e" v)))))

(define (ec-fail! ec msg)
  (error 'emit msg))
(define (ec-check-ty?! ec t ?)
  ;; xxx deal with delayed type
  (unless (? t)
    (ec-fail! ec (format "~e should match ~e" t ?))))
(define (ec-check-eq?! ec x y)
  (xxx 'ec-check-eq?! ec x y))
(define (ec-check-ty! ec x y)
  (cond
    [y
     (cond
       [(eq? x y)
        y]
       [else (xxx 'ec-check-ty! ec x y)])]
    [else
     x]))
(define (ec-check-ty-in! ec t ts)
  (xxx 'ec-check-ty-in! ec t ts))
(define (ec-check-ty-in-or-?! ec t ts ?)
  (xxx 'ec-check-ty-in-or-?! ec t ts ?))

(define pp:xxx (pp:text "xxx"))

(define (pp:include ec i)
  (match-type
   CPP i
   [(CHeader _ _ pre litc post)
    (pp:v-append (apply pp:v-append (map pp:text pre))
                 (pp:hs-append (pp:text "#include") (pp:text litc))
                 (apply pp:v-append (map pp:text post)))]))

(define (pp:val v)
  (match v
    [(or (? unsigned?)
         (? signed?)
         (? float?))
     (pp:text (number->string v))]
    [(? char?)
     (pp:text (format "'~a'" v))]
    [(? string?)
     (pp:text (format "~v" v))]
    [x
     (error 'pp:val "Illegal value to emit: ~e" v)]))

(define (pp:field f)
  (pp:text f))

(define (pp:var ec v)
  (pp:text (ec-var-name ec v)))

(define (pp:dref ec d)
  (define-values (global? n) (ec-name! ec d))
  (pp:text n))

(define (pp:ty ec t
               #:name [n #f]
               #:ptrs [p 0])
  (define (pp:ty-name t)
    (define tp
      (if (zero? p)
          t
          (format "~a ~a" t (make-string p #\*))))
    (if n
        (pp:hs-append (pp:text tp) (pp:text n))
        (pp:text tp)))
  (match-type
   Type t
   [(Size) (pp:ty-name "size_t")] [(Char) (pp:ty-name "char")]
   [(UI8) (pp:ty-name "uint8_t")] [(UI16) (pp:ty-name "uint16_t")]
   [(UI32) (pp:ty-name "uint32_t")] [(UI64) (pp:ty-name "uint64_t")]
   [(SI8) (pp:ty-name "int8_t")] [(SI16) (pp:ty-name "int16_t")]
   [(SI32) (pp:ty-name "int32_t")] [(SI64) (pp:ty-name "int64_t")]
   [(F32) (pp:ty-name "float")] [(F64) (pp:ty-name "double")]
   [(F128) (pp:ty-name "long double")]
   [(Void) (pp:ty-name "void")]
   [(Record fs) (xxx 'record)]
   [(Ptr t)
    (pp:ty ec t #:name n #:ptrs (add1 p))]
   [(Arr t k)
    (pp:h-append (pp:ty ec t #:name n #:ptrs p) pp:lbracket k pp:rbracket)]
   [(Union ts)
    (xxx 'union)]
   [(Fun dom rng)
    (xxx 'fun)]
   [(Extern h ln)
    (pp:ty-name ln)]
   [(Delay -t)
    (pp:ty ec (-t) #:name n #:ptrs p)]
   [(Seal n t)
    (pp:ty ec t #:name n #:ptrs p)]))

(define pp:op1-table
  (hasheq $! "!"
          $neg "-"
          $bneg "~"))
(define (pp:op1 o)
  (pp:text
   (hash-ref pp:op1-table o
             (λ () (error 'pp:op1 "Unknown op1: ~e" o)))))

(define pp:op2-table
  (hasheq $+ "+" $- "-" $* "*" $/ "/" $% "%"
          $== "==" $!= "!=" $> ">" $< "<" $>= ">=" $<= "<="
          $and "&&" $or "||"
          $band "&" $bior "|" $bxor "^" $bshl "<<" $bshr ">>"))
(define (pp:op2 o)
  (pp:text
   (hash-ref pp:op2-table o
             (λ () (error 'pp:op2 "Unknown op1: ~e" o)))))

(define (pp:expr ec e)
  (match-type
   Expr e
   [($sizeof t)
    (pp:h-append pp:lparen
                 (pp:text "sizeof") pp:lparen (pp:ty ec t) pp:rparen
                 pp:rparen)]
   [($offsetof t f)
    (pp:h-append pp:lparen
                 (pp:text "offsetof") pp:lparen (pp:ty ec t)
                 pp:comma pp:space (pp:field f) pp:rparen
                 pp:rparen)]
   [($op1 o a)
    (pp:h-append pp:lparen (pp:op1 o) (pp:expr ec a) pp:rparen)]
   [($op2 a o b)
    (pp:h-append pp:lparen (pp:expr ec a) pp:space
                 (pp:op2 o) pp:space
                 (pp:expr ec b) pp:rparen)]
   [($val t v)
    ;; XXX put in cast?
    (pp:val v)]
   [($app r rs)
    (pp:h-append (pp:expr ec r) pp:lparen
                 (apply pp:hs-append
                        (pp:apply-infix pp:comma
                                        (map (λ (r) (pp:expr ec r)) rs)))
                 pp:rparen)]
   [($aref a b)
    (pp:h-append (pp:expr ec a) pp:lbracket (pp:expr ec b) pp:rbracket)]
   [($addr a)
    (pp:h-append pp:lparen (pp:char #\&) (pp:expr ec a) pp:rparen)]
   [($pref a)
    (pp:h-append pp:lparen (pp:char #\*) (pp:expr ec a) pp:rparen)]
   [($vref v)
    (pp:var ec v)]
   [($dref d)
    (pp:dref ec d)]
   [($ddref -d)
    (pp:dref ec (-d))]
   [($sref a f)
    (pp:h-append pp:lparen (pp:expr ec a) pp:rparen (pp:char #\*) (pp:field f))]
   [($uref a f)
    (pp:expr ec ($sref a f))]
   [($ife a b c)
    (pp:hs-append pp:lparen (pp:expr ec a) (pp:char #\?)
                  (pp:expr ec b) (pp:char #\:)
                  (pp:expr ec c))]
   [($seal _ e)
    (pp:expr ec e)]
   [($unseal _ e)
    (pp:expr ec e)]))

(define (pp:stmt ec st)
  (match-type
   Stmt st
   [($nop)
    (pp:text ";")]
   [($seq a b)
    (pp:v-append (pp:stmt ec a) (pp:stmt ec b))]
   [($do e)
    (pp:h-append (pp:text "(void)") (pp:expr ec e) pp:semi)]
   [($if e s1 s2)
    (pp:h-append (pp:hs-append (pp:text "if")
                               pp:lparen (pp:expr ec e) pp:rparen
                               pp:lbrace)
                 (pp:nest NEST
                          (pp:h-append
                           pp:line (pp:stmt ec s1)))
                 pp:line
                 (pp:nest NEST
                          (pp:h-append
                           (pp:hs-append pp:rbrace (pp:text "else") pp:lbrace) pp:line
                           (pp:stmt ec s2)))
                 pp:line
                 pp:rbrace)]
   [($while e s1)
    (pp:h-append (pp:hs-append (pp:text "while")
                               pp:lparen (pp:expr ec e) pp:rparen
                               pp:lbrace )
                 (pp:nest NEST
                          (pp:h-append
                           pp:line
                           (pp:stmt ec s1)))
                 pp:line
                 pp:rbrace)]
   [($let1 t v b)
    (define ec-p (ec-extend-ns ec (cons v t)))
    (define vn (ec-var-name ec v))
    (pp:h-append pp:lbrace pp:space (pp:ty ec t #:name vn) pp:semi
                 (pp:nest NEST (pp:h-append pp:line (pp:stmt ec-p b))) pp:rbrace)]
   [($set! l r)
    (pp:h-append (pp:expr ec l) pp:space (pp:char #\=) pp:space (pp:expr ec r) pp:semi)]
   [($ret v)
    (pp:h-append (pp:text "return") pp:space (pp:expr ec v) pp:semi)]
   [($return)
    (pp:h-append (pp:text "return") pp:semi)]))

(define (pp:decl ec d #:proto-only? [proto-only? #f])
  (define-values (global? n) (ec-name! ec d))
  (define maybe-static
    (if global? pp:empty (pp:h-append (pp:text "static") pp:space)))
  (match-type
   Decl d
   [($extern _ _) pp:empty]
   [($typedef hn t)
    (if proto-only?
        (pp:h-append (pp:text "typedef") pp:space (pp:ty ec t #:name n) pp:semi)
        pp:empty)]
   [($proc hn (and t (Fun dom rng)) b)
    (define ec-p (ec-extend-ns* ec dom))
    (pp:h-append
     maybe-static
     (pp:h-append
      (pp:hs-append (pp:ty ec rng) (pp:text n)
                    pp:lparen
                    (apply pp:hs-append
                           (pp:apply-infix
                            pp:comma
                            (for/list ([v*t (in-list dom)])
                              (match-define (cons v t) v*t)
                              (define vn (ec-var-name ec v))
                              (if proto-only?
                                  (pp:ty ec t)
                                  (pp:ty ec t #:name vn)))))
                    pp:rparen)
      (if proto-only?
          pp:semi
          (pp:h-append
           pp:space
           (pp:nest NEST
                    (pp:h-append
                     pp:lbrace pp:line
                     (pp:stmt ec-p b)))
           pp:line
           pp:rbrace))))]
   [($var hn t v)
    (pp:h-append
     maybe-static
     (pp:ty ec t #:name n)
     (if proto-only?
         pp:semi
         (pp:hs-append (pp:text "=")
                       (pp:expr ec v)
                       pp:semi)))]))

(define (walk-h! ec h)
  (match-type
   CPP h
   [(CHeader cfs lfs _ _ _)
    (ec-add-cfs! ec cfs)
    (ec-add-lfs! ec lfs)])
  (define is (emit-ctxt-is ec))
  (set-add! is h))

(define (walk-ty! ec t)
  (match-type
   Type t
   [(Size) (void)] [(Char) (void)]
   [(UI8) (void)] [(UI16) (void)] [(UI32) (void)] [(UI64) (void)]
   [(SI8) (void)] [(SI16) (void)] [(SI32) (void)] [(SI64) (void)]
   [(F32) (void)] [(F64) (void)] [(F128) (void)]
   [(Void) (void)]
   [(Record fs)
    (for ([f*t (in-list fs)])
      (walk-ty! ec (cdr f*t)))]
   [(Ptr t)
    (walk-ty! ec t)]
   [(Arr t k)
    (walk-ty! ec t)]
   [(Union ts)
    (for-each (λ (t) (walk-ty! ec t)) ts)]
   [(Fun dom rng)
    (for ([v*t (in-list dom)])
      (walk-ty! ec (cdr v*t)))
    (walk-ty! ec rng)]
   [(Extern h n)
    (walk-h! ec h)]
   [(Delay -t)
    (void)]
   [(Seal n t)
    (walk-ty! ec t)]))

(define (walk-expr! ec e #:check [ct #f])
  (match-type
   Expr e
   [($sizeof t)
    (walk-ty! ec t)
    (ec-check-ty! ec Size ct)]
   [($offsetof t f)
    (walk-ty! ec t)
    (define ft (Record-field-ty ec t f))
    (ec-check-ty! ec Size ct)]
   [($op1 o a)
    (define at (walk-expr! ec a))
    (define rt (ec-check-op1-ty! ec o at))
    (ec-check-ty! ec rt ct)]
   [($op2 a o b)
    (define at (walk-expr! ec a))
    (define bt (walk-expr! ec b))
    (define rt (ec-check-op2-ty! ec at o bt))
    (ec-check-ty! ec rt ct)]
   [($val t v)
    (walk-ty! ec t)
    (ec-check-val-ty! ec v (or t ct))
    (ec-check-ty! ec t ct)]
   [($app r rs)
    (define rt (walk-expr! ec r))
    (ec-check-ty?! ec rt Fun?)
    (match-define (Fun dom rng) rt)
    (for ([r (in-list rs)]
          [v*rt (in-list dom)])
      (walk-expr! ec r #:check (cdr v*rt)))
    (ec-check-ty! ec rng ct)]
   [($aref a b)
    (define at (walk-expr! ec a))
    (ec-check-ty?! ec at Arr?)
    (match-define (Arr et _) at)
    (walk-expr! ec b #:check Size)
    (ec-check-ty! ec et ct)]
   [($addr a)
    (define at (walk-expr! ec a))
    (ec-check-ty! ec (Ptr at) ct)]
   [($pref a)
    (define at (walk-expr! ec a))
    (ec-check-ty?! ec at Ptr?)
    (match-define (Ptr et) at)
    (ec-check-ty! ec et ct)]
   [($vref v)
    (ec-check-ty! ec (ec-var-ty ec v) ct)]
   [($dref d)
    (define dt (walk-decl! ec d))
    (ec-check-ty! ec dt ct)]
   [($ddref -d)
    (walk-expr! ec ($dref (-d)) #:check ct)]
   [($sref a f)
    (define at (walk-expr! ec a))
    (ec-check-ty?! ec at Record?)
    (define ft (Record-field-ty ec at f))
    (ec-check-ty! ec ft ct)]
   [($uref a f)
    (define at (walk-expr! ec a))
    (ec-check-ty?! ec at Union?)
    (define ft (Union-ty ec at f))
    (ec-check-ty! ec ft ct)]
   [($ife a b c)
    (walk-expr! ec a #:check Bool)
    (walk-expr! ec b #:check ct)
    (walk-expr! ec c #:check ct)]
   [($seal s e)
    (define et (walk-expr! ec e))
    (ec-check-ty! ec (Seal s et) ct)]
   [($unseal s e)
    (define dt (walk-expr! ec e))
    (ec-check-ty?! ec dt Seal?)
    (match-define (Seal n et) dt)
    (ec-check-eq?! ec n s)
    (ec-check-ty! ec et ct)]))

(define (ec-check-ret! ec ret?)
  (xxx 'ec-check-ret! ec ret?))

(define (walk-stmt! ec st
                    #:ret? ret? 
                    #:check rt)
  (match-type
   Stmt st
   [($nop)
    (ec-check-ret! ec ret?)]
   [($seq a b)
    (walk-stmt! ec a #:check rt #:ret? #f)
    (walk-stmt! ec b #:check rt #:ret? ret?)]
   [($do e)
    (walk-expr! ec e #:check Void)
    (ec-check-ret! ec ret?)]
   [($if e s1 s2)
    (walk-expr! ec e #:check Bool)
    (walk-stmt! ec s1 #:check rt #:ret? ret?)
    (walk-stmt! ec s2 #:check rt #:ret? ret?)]
   [($while e s1)
    (walk-expr! ec e #:check Bool)
    (walk-stmt! ec s1 #:check rt #:ret? ret?)]
   [($let1 t v b)
    (define ec-p (ec-extend-ns ec (cons v t)))
    (walk-stmt! ec-p b #:check rt #:ret? ret?)]
   [($set! l r)
    (define t (walk-expr! ec l))
    (walk-expr! ec r #:check t)
    (ec-check-ret! ec ret?)]
   [($ret v)
    (walk-expr! ec v #:check rt)]
   [($return)
    (ec-check-ty! ec Void rt)]))

(define (walk-decl! ec d)
  (define d->ty (emit-ctxt-d->ty ec))
  (cond
    [(hash-ref d->ty d #f)
     => (λ (dt) dt)]
    [else
     (ec-add-decl! ec d)
     (match-type
      Decl d
      [($extern h n)
       (define t (Extern h n))
       (hash-set! d->ty d t)
       (walk-h! ec h)]
      [($typedef hn t)
       (hash-set! d->ty d t)
       (walk-ty! ec t)]
      [($proc hn (and t (Fun dom rng)) b)
       (hash-set! d->ty d t)
       (walk-ty! ec t)
       (define ec-p (ec-extend-ns* ec dom))
       (walk-stmt! ec-p b #:check rng #:ret? #t)
       t]
      [($var hn t v)
       (hash-set! d->ty d t)
       (walk-ty! ec t)
       (walk-expr! ec v #:check t)])]))

(define (pp:ec ec)
  (define is (emit-ctxt-is ec))
  (define ds (emit-ctxt-ds ec))
  (for ([d (in-list (set->list ds))])
    (walk-decl! ec d))
  (pp:v-append (apply pp:v-append
                      (for/list ([i (in-set is)])
                        (pp:include ec i)))
               pp:line
               (apply pp:v-append
                      (for/list ([i (in-set ds)])
                        (pp:decl ec i #:proto-only? #t)))
               pp:line
               (apply pp:v-append
                      (for/list ([i (in-set ds)])
                        (pp:h-append (pp:decl ec i) pp:line)))))

(define (pp:unit ec u)
  (match-type
   Unit u
   [($cflags cfs u)
    (ec-add-cfs! ec cfs)
    (pp:unit ec u)]
   [($ldflags lfs u)
    (ec-add-lfs! ec lfs)
    (pp:unit ec u)]
   [($exe m)
    (pp:unit ec ($lib (hash "main" m)))]
   [($lib ds)
    (for ([(n d) (in-hash ds)])
      (ec-add-decl! ec d)
      (ec-add-named! ec n d))
    (pp:ec ec)]))

(struct emit-result (cflags ldflags doc))

(define (emit u)
  (define ec (make-emit-ctxt))
  (define d (pp:unit ec u))
  (emit-result (set->list (emit-ctxt-cflags ec))
               (set->list (emit-ctxt-ldflags ec))
               d))

(define (er-print! er)
  (pp:pretty-print (emit-result-doc er)))

(define (emit! u)
  (er-print! (emit u)))

(require racket/file
         racket/system)
(define (delete-file* p)
  (when (file-exists? p)
    (delete-file p)))

(define (call-with-temporary pt t)
  (define p (make-temporary-file pt))
  (dynamic-wind
    void
    (λ () (t p))
    (λ ()
      (delete-file* p))))

(define (run u)
  (define er (emit u))
  (call-with-temporary
   "~a.c"
   (λ (c)
     (with-output-to-file c #:exists 'replace (λ () (er-print! er)))
     (call-with-temporary
      "~a.o"
      (λ (o)
        (define cc-pth (find-executable-path "cc"))
        (apply system* cc-pth (append (emit-result-cflags er) (list c "-c" "-o" o)))
        (call-with-temporary
         "~a.bin"
         (λ (b)
           (apply system* cc-pth
                  (append (list o) (emit-result-ldflags er) (list "-o" b)))
           (system* b))))))))

;; Convenience
(define arg cons)

(define ($default-flags u)
  ($cflags '("-Wall" "-Wextra" "-Weverything" "-Wpedantic" "-Wshadow"
             "-Wstrict-overflow" "-fno-strict-aliasing"
             "-Wno-unused-parameter" "-Wno-unused-function"
             "-Werror" "-pedantic" "-std=c99" "-O3" "-march=native"
             "-fno-stack-protector" "-ffunction-sections" "-fdata-sections"
             "-fno-unwind-tables" "-fno-asynchronous-unwind-tables" "-fno-math-errno"
             "-fmerge-all-constants" "-fno-ident" "-fPIE" "-fPIC")
           ($ldflags '("-dead_strip") u)))

(define ($let* ty n e b)
  ($let1 ty n ($seq ($set! ($vref n) e) b)))
(define ($for ty n ie te ss b)
  ($let* ty n ie
         ($while te
                 ($seq b ss))))
(define ($begin . ss)
  (match ss
    [(list)
     ($nop)]
    [(list s)
     s]
    [(cons s ss)
     ($seq s (apply $begin ss))]))

(define ($when e s)
  ($if e s ($nop)))
(define ($unless e s)
  ($when ($op1 $! e) s))

(define $v $val)

;; XXX exhaustive enum + datatypes
;; XXX testing & contracts
;; XXX closures

;; Tests
(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define stdio:printf ($extern <stdio.h> "printf"))

(module* ex:fac #f
  ;; XXX add macros for $proc and $let that use racket-level binding
  ;; XXX alias ops to $op1/2-less forms
  (define fac-rec
    ($proc 'fac-rec (Fun (list (arg 'n UI64)) UI64)
           ($if ($op2 ($vref 'n) $<= ($v UI64 0))
                ($ret ($v UI64 1))
                ($ret ($op2 ($vref 'n) $*
                            ($app ($ddref (λ () fac-rec))
                                  (list ($op2 ($vref 'n) $- ($v UI64 1)))))))))

  (define fac
    ($proc 'fac (Fun (list (arg 'n UI64)) UI64)
           ($let* UI64 'acc ($v UI64 1)
                  ($begin
                   ($while ($op2 ($vref 'n) $!= ($v UI64 0))
                           ($begin
                            ($set! ($vref 'acc) ($op2 ($vref 'acc) $* ($vref 'n)))
                            ($set! ($vref 'n) ($op2 ($vref 'n) $- ($v UI64 1)))))
                   ($ret ($vref 'acc))))))

  (define main
    ($proc 'main (Fun (list (arg 'argc SI32) (arg 'argv (Ptr String))) SI32)
           (let ()
             (define (test-fac which fac)
               ($let* UI64 'r ($v SI32 0)
                      ($begin
                       ($for UI32 'i ($v UI32 0)
                             ($op2 ($vref 'i) $<= ($v UI32 10000))
                             ($set! ($vref 'i) ($op2 ($vref 'i) $+ ($v UI32 1)))
                             ($set! ($vref 'r) ($app ($dref fac) (list ($v UI64 12)))))
                       ($do ($app ($dref stdio:printf)
                                  (list ($v String (format "~a r = %llu\n" which))
                                        ($vref 'r)))))))
             ($begin
              ($unless ($op2 ($vref 'argc) $== ($v SI32 1))
                       ($ret ($v SI32 1)))
              ($do ($vref 'argv))
              (test-fac "iter" fac)
              (test-fac " rec" fac-rec)
              ($ret ($v SI32 0))))))

  (define this
    ($default-flags ($exe main)))

  (module+ test
    (emit! this)
    (run this)))

(module+ test
  (require (submod ".." ex:fac test)))
