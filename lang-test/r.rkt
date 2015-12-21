#lang at-exp racket/base
(require racket/format
         racket/set
         racket/contract/base
         (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define
         racket/list
         racket/match
         racket/string
         racket/generic)

(define dsp list)
(define semi ";")

(define (gencsym s)
  (symbol->string (gensym (regexp-replace* #rx"[^A-Za-z_0-9]" (~a "_" s) "_"))))

(define-generics r-ty
  [r-ty-write r-ty]
  [r-ty-write-as str r-ty]
  [r-ty-walk! r-ty])
(define gr-ty-write r-ty-write)
(define gr-ty-walk! r-ty-walk!)

(define-generics r-expr
  [r-e-write r-expr]
  [r-e-walk! r-expr]
  #:fast-defaults
  ([integer?
    (define (r-e-write t) (number->string t))
    (define r-e-walk! void)]
   [inexact-real?
    (define (r-e-write t) (number->string t))
    (define r-e-walk! void)]
   [string?
    (define (r-e-write t) (~v t))
    (define r-e-walk! void)]))
(define gr-e-write r-e-write)
(define gr-e-walk! r-e-walk!)

(define-generics r-statement
  [r-s-write r-statement]
  [r-s-walk! r-statement]
  #:defaults
  ([r-expr?
    (define (r-s-write t) @dsp{@(r-e-write t)@semi})
    (define (r-s-walk! t) (r-e-walk! t))]))
(define gr-s-write r-s-write)
(define gr-s-walk! r-s-walk!)

(struct r-ty-verbatim+include (in-c inc)
  #:methods gen:r-ty
  [(define (r-ty-write t)
     (match-define (r-ty-verbatim+include ic inc) t)
     ic)
   (define (r-ty-write-as s t)
     @dsp{@(r-ty-write t) @s})
   (define (r-ty-walk! t)
     (match-define (r-ty-verbatim+include ic inc) t)
     (when inc
       (r-include-walk! inc)))])

(define (r-ty-verbatim in-c)
  (r-ty-verbatim+include in-c #f))

(define r-char (r-ty-verbatim "char"))
(define r-int (r-ty-verbatim "int"))
(define r-void (r-ty-verbatim "void"))
(define f32 (r-ty-verbatim "float"))
(define f64 (r-ty-verbatim "double"))
(define f128 (r-ty-verbatim "long double"))

(struct r-ptr (i)
  #:methods gen:r-ty
  [(define (r-ty-write t)
     @dsp{@(gr-ty-write (r-ptr-i t)) *})
   (define (r-ty-write-as s t)
     @dsp{@(r-ty-write t) @s})
   (define (r-ty-walk! t) (gr-ty-walk! (r-ptr-i t)))])

(struct r-var (ty id-base)
  #:methods gen:r-expr
  [(define (r-e-write t)
     (r-var-mapping-ref t))
   (define (r-e-walk! t)
     (r-ty-walk! (r-var-ty t)))])
(define (r-var-id v)
  (gencsym (r-var-id-base v)))
(define (r-var-walk! v)
  (r-ty-walk! (r-var-ty v)))

(struct *r-var-extern (id inc)
  #:methods gen:r-expr
  [(define (r-e-write t)
     (match-define (*r-var-extern id inc) t)
     id)
   (define (r-e-walk! t)
     (match-define (*r-var-extern id inc) t)
     (r-include-walk! inc))])

(struct *r-rec (pr)
  #:methods gen:r-expr
  [(define (r-e-write t)
     (gr-e-write ((*r-rec-pr t))))
   (define (r-e-walk! t)
     (gr-e-walk! ((*r-rec-pr t))))])
(define-syntax-rule (r-rec e)
  (*r-rec (λ () e)))

(define (r-e-write-bin-expr l op r)
  @dsp{((@gr-e-write[l]) @op (@gr-e-write[r]))})
(define-syntax-rule (define-binary-r-expr id op)
  (begin
    (struct id (lhs rhs)
    #:methods gen:r-expr
    [(define (r-e-write t)
       (match-define (id l r) t)
       (r-e-write-bin-expr l op r))
     (define (r-e-walk! t)
       (match-define (id l r) t)
       (gr-e-walk! l)
       (gr-e-walk! r))])
    (provide (contract-out [id (-> r-expr? r-expr? r-expr?)]))))
(define-binary-r-expr r!= "!=")
(define-binary-r-expr r<= "<=")
(define-binary-r-expr r+ "+")
(define-binary-r-expr r* "*")
(define-binary-r-expr r- "-")

(define (r-e-write-un-expr op r)
  @dsp{(@op (@gr-e-write[r]))})
(define-syntax-rule (define-unary-r-expr id op)
  (begin
    (struct id (rhs)
    #:methods gen:r-expr
    [(define (r-e-write t)
       (match-define (id r) t)
       (r-e-write-un-expr op r))
     (define (r-e-walk! t)
       (match-define (id r) t)
       (gr-e-walk! r))])
    (provide (contract-out [id (-> r-expr? r-expr?)]))))
(define-unary-r-expr r! "!")

(struct r-app (fe args)
  #:methods gen:r-expr
  [(define (r-e-write t)
     (match-define (r-app fe args) t)
     @dsp{(@gr-e-write[fe])(@(add-between (map gr-e-write args) ", "))})
   (define (r-e-walk! t)
     (match-define (r-app fe args) t)
     (gr-e-walk! fe)
     (for-each gr-e-walk! args))])

(define current-r-var-mapping (make-parameter (hasheq)))
(define-simple-macro (with-r-var-mapping v . body)
  (parameterize ([current-r-var-mapping (r-var-mapping-add v)])
    . body))
(define (r-var-mapping-add v)
  (define h (current-r-var-mapping))
  (define (i v h) (hash-set h v (r-var-id v)))
  (cond [(r-var? v) (i v h)]
        [else (foldr i h v)]))
(define (r-var-mapping-ref v)
  (define m (current-r-var-mapping))
  (unless m
    (error 'r-var-mapping-ref "No r-var-mapping-ref"))
  (hash-ref m v
            (λ () (error 'r-var-mapping-ref "unbound var: ~e" (r-var-id-base v)))))
(define (r-write-var-def v)
  (r-ty-write-as (r-var-mapping-ref v) (r-var-ty v)))

(define current-r-global-mapping (make-parameter #f))
(define (r-global-mapping-add! v s)
  (hash-set! (current-r-global-mapping) v s))
(define (r-global-mapping-try-add! v)
  (define m (current-r-global-mapping))
  (and (not (hash-has-key? m v))
       (hash-set! m v (gencsym (*r-fun-id v)))))
(define (r-global-mapping-ref v)
  (define m (current-r-global-mapping))
  (unless m
    (error 'r-global-mapping-ref "no current-r-global-mapping"))
  (hash-ref m v
            (λ () (error 'r-global-mapping-ref "unbound fun(~e) in ~e"
                         (*r-fun-id v)
                         m))))

(define current-indent (make-parameter 0))
(define-syntax-rule (indent . b)
  (parameterize ([current-indent (add1 (current-indent))])
    (list . b)))
(define (indentnl)
  (cons "\n"
        (for/list ([i (in-range (current-indent))])
          " ")))

(struct r-begin (stmts)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (r-begin ss) t)
     @dsp{{@indent{@(add-between (map gr-s-write ss) (indentnl))}}})
   (define (r-s-walk! t)
     (match-define (r-begin ss) t)
     (for-each gr-s-walk! ss))])
(struct *r-let1 (v e body)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (*r-let1 v e b) t)
     (with-r-var-mapping v
       @dsp{{@r-write-var-def[v] = @gr-e-write[e]@|semi|@(indentnl)@gr-s-write[b]}}))
   (define (r-s-walk! t)
     (match-define (*r-let1 v e b) t)
     (r-var-walk! v)
     (r-e-walk! e)
     (gr-s-walk! b))])
(define-syntax (r-let* stx)
  (syntax-parse stx
    [(r-let () . body)
     (syntax/loc stx
       (r-begin (list . body)))]
    [(r-let ([v-ty v:id v-e] . more) . body)
     (syntax/loc stx
       (let ([v (r-var v-ty 'v)])
         (*r-let1 v v-e
                  (r-let more . body))))]))

(struct *r-while (cond body)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (*r-while c b) t)
     @dsp{while (@gr-e-write[c]) {@(indentnl)@indent{@gr-s-write[b]}}})
   (define (r-s-walk! t)
     (match-define (*r-while c b) t)
     (gr-e-walk! c)
     (gr-s-walk! b))])
(define (r-while test . body)
  (*r-while test (r-begin body)))

(struct r-if (cond true false)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (r-if c tr fa) t)
     @dsp{if (@gr-e-write[c]) {@(indentnl)@indent{@gr-s-write[tr]}@(indentnl)} else {@(indentnl)@indent{@gr-s-write[fa]}}})
   (define (r-s-walk! t)
     (match-define (r-if c tr fa) t)
     (gr-e-walk! c)
     (gr-s-walk! tr)
     (gr-s-walk! fa))])

(struct *r-for (cond step body)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (*r-for c s b) t)
     @dsp{for (@semi @gr-e-write[c]@semi @gr-e-write[s]) {@(indentnl)@indent{@gr-s-write[b]}}})
   (define (r-s-walk! t)
     (match-define (*r-for c s b) t)
     (r-e-walk! c)
     (gr-s-walk! s)
     (gr-s-walk! b))])
(define-simple-macro (r-for [v-ty v v-e] v-cond v-step . body)
  (r-let* ([v-ty v v-e])
          (*r-for v-cond v-step (r-begin (list . body)))))

(define r-lvalue?
  (or/c r-var?))
(struct r-set! (lv rhs)
  #:methods gen:r-expr
  [(define (r-e-write t)
     (match-define (r-set! l r) t)
     @dsp{@gr-e-write[l] = @gr-e-write[r]})
   (define (r-e-walk! t)
     (match-define (r-set! l r) t)
     (gr-e-walk! l)
     (gr-e-walk! r))])
(struct *r-ret (v)
  #:methods gen:r-statement
  [(define (r-s-write t)
     (match-define (*r-ret v) t)
     (if v
         @dsp{return @gr-e-write[v]@semi}
         @dsp{return@semi}))
   (define (r-s-walk! t)
     (match-define (*r-ret v) t)
     (gr-s-walk! v))])
(define (r-ret [v #f])
  (*r-ret v))

(define-generics r-fun)

(struct *r-fun-extern (sym inc)
  #:property prop:procedure
  (λ (t . args)
    (r-app t args))
  #:methods gen:r-fun
  []
  #:methods gen:r-expr
  [(define (r-e-write t)
     (match-define (*r-fun-extern s i) t)
     s)
   (define (r-e-walk! t)
     (match-define (*r-fun-extern s i) t)
     (r-include-walk! i))])

(struct *r-fun (id args ret body)
  #:property prop:procedure
  (λ (t . args)
    (r-app t args))
  #:methods gen:r-fun
  []
  #:methods gen:r-expr
  [(define (r-e-write t)
     (r-global-mapping-ref t))
   (define (r-e-walk! t)
     (r-fun-walk! t))])
(define (r-fun-walk! t #:global? [global? #f])
  (define walk? global?)
  (when (r-global-mapping-try-add! t)
    (set-add! (current-defns) (r-private-fun t))
    (set! walk? #t))
  (when walk?  
    (match-define (*r-fun id args ret body) t)
    (for-each r-var-walk! args)
    (r-ty-walk! ret)
    (r-s-walk! body)))

(define (r-fun-write-defn t
                          #:name sym
                          #:visibility [vis ""]
                          #:proto-only? [proto-only? #f])
  (match-define (*r-fun id args ret body) t)
  (with-r-var-mapping args
    @dsp{@|vis|@r-ty-write[ret] @sym (@(add-between (map r-write-var-def args) ", "))@(if proto-only? ";" @indent{{@(indentnl)@gr-s-write[body]}})@(if proto-only? "\n" "\n\n")}))
(define-syntax (r-fun stx)
  (syntax-parse stx
    [(_ ([v-ty v:id] ...) (~datum :) r-ty body ...)
     (quasisyntax/loc stx
       (let ([v (r-var v-ty 'v)]
             ...)
         (*r-fun '#,(syntax-local-name) (list v ...) r-ty
                 (r-begin (list body ...)))))]))

(struct *r-include (pre litc post))
(define (r-include-walk! i)
  (set-add! (current-includes)
            i))
(define (r-include-write i)
  (match-define (*r-include pre lc post) i)
  @dsp{@(add-between pre "\n") @"\n"
       #include @lc @"\n"
       @(add-between post "\n") @"\n"})
(define (r-include #:pre-options [pre '()]
                   litc 
                   #:post-options [post '()])
  (*r-include pre litc post))

(define-generics r-decl
  [r-decl-walk! r-decl #:deep? bool])
(define-generics r-defn
  [r-defn-write-proto r-defn]
  [r-defn-write r-defn])

(struct r-public-fun (symbol f)
  #:methods gen:r-decl
  [(define (r-decl-walk! t #:deep? deep?)
     (match-define (r-public-fun s f) t)
     (set-add! (current-defns) t)
     (r-global-mapping-add! f s)
     (when deep?
       (r-fun-walk! f #:global? #t)))]
  #:methods gen:r-defn
  [(define (r-defn-write t)
     (match-define (r-public-fun s f) t)
     (r-fun-write-defn f #:name s))
   (define (r-defn-write-proto t)
     (match-define (r-public-fun s f) t)
     (r-fun-write-defn f #:name s #:proto-only? #t))])
(struct r-private-fun (f)
  #:methods gen:r-defn
  [(define (r-defn-write t)
     (match-define (r-private-fun f) t)
     (r-fun-write-defn f
                       #:name (r-global-mapping-ref f)
                       #:visibility "static "))
   (define (r-defn-write-proto t)
     (match-define (r-private-fun f) t)
     (r-fun-write-defn f
                       #:name (r-global-mapping-ref f)
                       #:visibility "static "
                       #:proto-only? #t))])

(define current-includes (make-parameter #f))
(define current-defns (make-parameter #f))

(struct *r-exe (decls))
(define (r-exe-write t)
  (match-define (*r-exe ds) t)
  (define include-set (mutable-seteq))
  (define defn-set (mutable-seteq))
  (define global-mapping (make-hasheq))
  (parameterize ([current-includes include-set]
                 [current-defns defn-set]
                 [current-r-global-mapping global-mapping])
    (for-each (λ (d) (r-decl-walk! d #:deep? #f)) ds)
    (for-each (λ (d) (r-decl-walk! d #:deep? #t)) ds)
    (dsp
     (for/list ([i (in-set include-set)])
       (r-include-write i))
     "\n"
     (for/list ([d (in-set defn-set)])
       (r-defn-write-proto d))
     "\n"
     (for/list ([d (in-set defn-set)])
       (r-defn-write d))
     "\n")))
(define (r-exe . decls)
  (*r-exe decls))
(define r-exe? *r-exe?)

(define (r-emit e)
  (l-display (r-exe-write e))
  (newline))
(define (l-display l)
  (cond
    [(string? l) (display l)]
    [(cons? l)
     (l-display (car l))
     (l-display (cdr l))]))

(define <stdint.h> (r-include "<stdint.h>"))

(define ui64 (r-ty-verbatim+include "uint64_t" <stdint.h>))
(define ui32 (r-ty-verbatim+include "uint32_t" <stdint.h>))
(define ui16 (r-ty-verbatim+include "uint16_t" <stdint.h>))
(define  ui8 (r-ty-verbatim+include  "uint8_t" <stdint.h>))
(define si64 (r-ty-verbatim+include  "int64_t" <stdint.h>))
(define si32 (r-ty-verbatim+include  "int32_t" <stdint.h>))
(define si16 (r-ty-verbatim+include  "int16_t" <stdint.h>))
(define  si8 (r-ty-verbatim+include   "int8_t" <stdint.h>))

(provide
 r-fun
 r-let*
 r-for
 r-rec
 (contract-out
  [r-char r-ty?] [r-int r-ty?] [r-void r-ty?]
  [ui8 r-ty?] [ui16 r-ty?] [ui32 r-ty?] [ui64 r-ty?]
  [si8 r-ty?] [si16 r-ty?] [si32 r-ty?] [si64 r-ty?]
  [f32 r-ty?] [f64 r-ty?] [f128 r-ty?]
  [r-ptr (-> r-ty? r-ty?)]
  [r-include (->* (string?) (#:pre-options (listof string?) #:post-options (listof string?)) *r-include?)]
  [r-var (-> r-ty? symbol? r-expr?)]
  [r-app (-> r-expr? (listof r-expr?) r-expr?)]
  [*r-rec (-> (-> r-expr?) r-expr?)]
  [*r-let1 (-> r-var? r-expr? r-statement? r-statement?)]
  [r-while (->* (r-expr?) () #:rest (listof r-statement?) r-statement?)]
  [*r-while (-> r-expr? r-statement? r-statement?)]
  [*r-for (-> r-expr? r-statement? r-statement? r-statement?)]
  [r-if (-> r-expr? r-statement? r-statement? r-statement?)]
  [r-set! (-> r-lvalue? r-expr? r-expr?)]
  [r-ret (->* () (r-expr?) r-statement?)]
  [r-begin (-> (listof r-statement?) r-statement?)]
  [*r-fun (-> symbol?
              (listof r-var?)
              r-ty?
              (listof r-statement?)
              r-fun?)]
  [r-public-fun (-> string? r-fun? r-decl?)]
  [r-exe (->* () () #:rest (listof r-decl?) r-exe?)]
  [r-emit (-> r-exe? void?)]))

(define (r-unless cond body)
  (r-when (r! cond) body))
(define (r-when cond body)
  (r-if cond body (r-begin '())))

(provide
 (contract-out
  [r-unless (-> r-expr? r-statement? r-statement?)]
  [r-when (-> r-expr? r-statement? r-statement?)]))

(define-simple-macro (define-r-fn fn:id #:from inc:id)
  (define fn (*r-fun-extern (symbol->string 'fn) inc)))

(define-simple-macro (define-r-fns #:from inc:id fn:id ...)
  (begin (define-r-fn fn #:from inc) ...))

(define-simple-macro (define-r-val val:id #:from inc:id)
  (define val (*r-var-extern (symbol->string 'val) inc)))

(define-simple-macro (define-r-vals #:from inc:id fn:id ...)
  (begin (define-r-val fn #:from inc) ...))

(provide define-r-fn
         define-r-fns
         define-r-val
         define-r-vals)

(define <stdio.h> (r-include "<stdio.h>"))
(define stdio:printf (*r-fun-extern "printf" <stdio.h>))
(provide <stdio.h> stdio:printf)

(define <stdlib.h> (r-include "<stdlib.h>"))
(define-r-vals #:from <stdlib.h> EXIT_FAILURE)
(define stdlib:exit (*r-fun-extern "exit" <stdlib.h>))
(provide <stdlib.h> stdlib:exit EXIT_FAILURE)
