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
(define Val?
  (or/c unsigned? signed? float? char? string?))

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
      ($fref? x)))

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
  (Union [tys (listof Type?)])
  (Fun [dom (listof (cons/c Var? Type?))] [rng Type?])
  ;; Extensions
  (Literal [h CHeader?] [n CName?])
  (Delay [-ty (-> Type?)])
  (Seal [n Seal-Id?] [ty Type?]))

(define String (Ptr Char))

(define-type Op1
  $sizeof ;; XXX actually special, because references a ty
  ;; XXX add offset of
  $! $neg $bneg)

(define-type Op2
  $+ $- $* $/ $%
  $== $!= $> $< $>= $<=
  $and $or
  $band $bior $bxor $bshl $bshr)

(define-type Expr
  ($op1 [op Op1?] [arg Expr?])
  ($op2 [lhs Expr?] [op Op2?] [rhs Expr?])
  ;; xxx array/union/struct literals
  ($val [v Val?])
  ($app [rator Expr?] [rands (listof Expr?)])
  ($aref [lhs Expr?] [rhs Expr?])
  ($addr [arg Expr?])
  ($pref [arg Expr?])
  ($vref [v Var?])
  ($dref [d Decl?])
  ($ddref [-d (-> Decl?)])
  ($fref [obj Expr?] [f Field?])
  ($ife [test Expr?] [if1 Expr?] [if0 Expr?])
  ;; xxx add seal/unseal
  )

(define-type Stmt
  ($seq [fst Stmt?] [snd Stmt?])
  ($do [e Expr?])
  ($if [test Expr?] [if1 Stmt?] [if0 Stmt?])
  ($while [test Expr?] [body Stmt?])
  ($let1 [ty Type?] [v Var?] [body Stmt?])
  ($set! [lhs Lval?] [rhs Expr?])
  ($ret [val Expr?])
  ($return))

(define-type Decl
  ;; XXX allow naming of types for libraries
  ($extern [h CHeader?] [n CName?])
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

(struct emit-ctxt (cflags ldflags is ds n->d d->n v->vn))
(define (make-emit-ctxt)
  (emit-ctxt (mutable-set) (mutable-set)
             (mutable-seteq <stdint.h>) (mutable-seteq)
             (make-hash) (make-hash)
             (hasheq)))
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
     [($proc hn _ _) hn]
     [($var hn _ _) hn]))
  (define n (hash-ref! d->n d (λ () (gencsym hn))))
  (values (hash-has-key? n->d n)
          n))
(define (ec-extend-ns ec v)
  (define vn (gencsym v))
  (values vn
          (struct-copy emit-ctxt ec
                       [v->vn (hash-set (emit-ctxt-v->vn ec) v vn)])))

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
  (pp:text
   (hash-ref (emit-ctxt-v->vn ec) v
             (λ ()
               (error 'pp:var "Unbound identifier: ~e" v)))))

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
   [(Literal h ln)
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
   [($op1 o a)
    (pp:h-append pp:lparen (pp:op1 o) (pp:expr ec a) pp:rparen)]
   [($op2 a o b)
    (pp:h-append pp:lparen (pp:expr ec a) pp:space
                 (pp:op2 o) pp:space
                 (pp:expr ec b) pp:rparen)]
   [($val v)
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
   [($fref a f)
    (pp:h-append pp:lparen (pp:expr ec a) pp:rparen (pp:char #\*) (pp:field f))]
   [($ife a b c)
    (pp:hs-append pp:lparen (pp:expr ec a) (pp:char #\?)
                  (pp:expr ec b) (pp:char #\:)
                  (pp:expr ec c))]))

(define (pp:stmt ec st)
  (match-type
   Stmt st
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
    (define-values (vn ec-p) (ec-extend-ns ec v))
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
   [($proc hn (and t (Fun dom rng)) b)
    (define-values (vns ec-p)
      (let loop ([rvns '()]
                 [ec ec]
                 [dom dom])
        (match dom
          ['()
           (values (reverse rvns) ec)]
          [(cons (cons v t) dom)
           (define-values (vn ec-p) (ec-extend-ns ec v))
           (loop (cons vn rvns) ec-p dom)])))
    (pp:h-append
     maybe-static
     (pp:h-append
      (pp:hs-append (pp:ty ec rng) (pp:text n)
                    pp:lparen
                    (apply pp:hs-append
                           (pp:apply-infix
                            pp:comma
                            (for/list ([v*t (in-list dom)]
                                       [vn (in-list vns)])
                              (match-define (cons v t) v*t)
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
   [(Literal h n)
    (walk-h! ec h)]
   [(Delay -t)
    (void)]
   [(Seal n t)
    (walk-ty! ec t)]))

(define (walk-expr! ec e)
  (match-type
   Expr e
   [($op1 o a)
    (walk-expr! ec a)]
   [($op2 a o b)
    (walk-expr! ec a)
    (walk-expr! ec b)]
   [($val v)
    (void)]
   [($app r rs)
    (walk-expr! ec r)
    (for-each (λ (r) (walk-expr! ec r)) rs)]
   [($aref a b)
    (walk-expr! ec a)
    (walk-expr! ec b)]
   [($addr a)
    (walk-expr! ec a)]
   [($pref a)
    (walk-expr! ec a)]
   [($vref v)
    (void)]
   [($dref d)
    (walk-decl! ec d)]
   [($ddref -d)
    (void)]
   [($fref a f)
    (walk-expr! ec a)]
   [($ife a b c)
    (walk-expr! ec a)
    (walk-expr! ec b)
    (walk-expr! ec c)]))

(define (walk-stmt! ec st)
  (match-type
   Stmt st
   [($seq a b)
    (walk-stmt! ec a)
    (walk-stmt! ec b)]
   [($do e)
    (walk-expr! ec e)]
   [($if e s1 s2)
    (walk-expr! ec e)
    (walk-stmt! ec s1)
    (walk-stmt! ec s2)]
   [($while e s1)
    (walk-expr! ec e)
    (walk-stmt! ec s1)]
   [($let1 t v b)
    (walk-stmt! ec b)]
   [($set! l r)
    (walk-expr! ec r)]
   [($ret v)
    (walk-expr! ec v)]
   [($return)
    (void)]))

(define (walk-decl! ec d)
  (ec-add-decl! ec d)
  (match-type
   Decl d
   [($extern h n)
    (walk-h! ec h)]
   [($proc hn ty b)
    (walk-ty! ec ty)
    (walk-stmt! ec b)]
   [($var hn ty v)
    (walk-ty! ec ty)
    (walk-expr! ec v)]))

(define (pp:ec ec)
  (define is (emit-ctxt-is ec))
  (define ds (emit-ctxt-ds ec))
  ;; XXX type-check during walk?
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
     ($return)]
    [(list s)
     s]
    [(cons s ss)
     ($seq s (apply $begin ss))]))

;; xxx when, unless

;; Tests
(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define stdio:printf ($extern <stdio.h> "printf"))

(module* ex:fac #f
  ;; XXX add macros for $proc and $let that use racket-level binding
  ;; XXX alias $val to $v or just $
  ;; XXX alias ops to $op1/2-less forms
  (define fac-rec
    ($proc 'fac-rec (Fun (list (arg 'n UI64)) UI64)
           ($if ($op2 ($vref 'n) $<= ($val 0))
                ($ret ($val 1))
                ($ret ($op2 ($vref 'n) $*
                            ($app ($ddref (λ () fac-rec))
                                  (list ($op2 ($vref 'n) $- ($val 1)))))))))

  (define fac
    ($proc 'fac (Fun (list (arg 'n UI64)) UI64)
           ($let* UI64 'acc ($val 1)
                  ($begin
                   ($while ($op2 ($vref 'n) $!= ($val 0))
                           ($begin
                            ($set! ($vref 'acc) ($op2 ($vref 'acc) $* ($vref 'n)))
                            ($set! ($vref 'n) ($op2 ($vref 'n) $- ($val 1)))))
                   ($ret ($vref 'acc))))))

  (define main
    ($proc 'main (Fun (list (arg 'argc SI32) (arg 'argv (Ptr String))) SI32)
           (let ()
             (define (test-fac which fac)
               ($let* UI64 'r ($val 0)
                      ($begin
                       ($for UI32 'i ($val 0)
                             ($op2 ($vref 'i) $<= ($val 10000))
                             ($set! ($vref 'i) ($op2 ($vref 'i) $+ ($val 1)))
                             ($set! ($vref 'r) ($app ($dref fac) (list ($val 12)))))
                       ($do ($app ($dref stdio:printf)
                                  (list ($val (format "~a r = %llu\n" which))
                                        ($vref 'r)))))))
             ($begin
              ($do ($vref 'argc))
              ($do ($vref 'argv))
              (test-fac "iter" fac)
              (test-fac " rec" fac-rec)
              ($ret ($val 0))))))

  (define this
    ($default-flags ($exe main)))

  (module+ test
    (emit! this)
    (run this)))

(module+ test
  (require (submod ".." ex:fac test)))
