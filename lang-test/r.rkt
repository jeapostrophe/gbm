#lang at-exp racket/base
(require racket/format
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

(define-generics ty-writer
  [ty-write ty-writer]
  [ty-write-as str ty-writer])
(define gty-write ty-write)

(define-generics e-writer
  [e-write e-writer]
  #:fast-defaults
  ([number?
    (define (e-write t) (number->string t))]
   [string?
    (define (e-write t) (~v t))]))
(define ge-write e-write)

(define-generics s-writer
  [s-write s-writer]
  #:defaults
  ([e-writer?
    (define (s-write t) @dsp{@(e-write t)@semi})]))
(define gs-write s-write)

(struct r-ty ())
(struct r-ty-verbatim r-ty (in-c)
  #:methods gen:ty-writer
  [(define (ty-write t)
     (match-define (r-ty-verbatim ic) t)
     ic)
   (define (ty-write-as s t)
     @dsp{@(ty-write t) @s})])
(define r-char (r-ty-verbatim "char"))
(define r-int (r-ty-verbatim "int"))
(define r-void (r-ty-verbatim "void"))
(define f32 (r-ty-verbatim "float"))
(define f64 (r-ty-verbatim "double"))
(define f128 (r-ty-verbatim "long double"))

(struct r-ptr r-ty (i)
  #:methods gen:ty-writer
  [(define (ty-write t)
     @dsp{@(gty-write (r-ptr-i t)) *})
   (define (ty-write-as s t)
     @dsp{@(ty-write t) @s})])

(struct *r-expr ())
(struct r-var *r-expr (ty id-base)
  #:methods gen:e-writer
  [(define (e-write t)
     (r-var-mapping-ref t))])
(define (r-var-id v)
  (gencsym (r-var-id-base v)))

(define (e-write-bin-expr l op r)
  @dsp{((@ge-write[l]) @op (@ge-write[r]))})
(define-syntax-rule (define-binary-r-expr id op)
  (begin
    (struct id *r-expr (lhs rhs)
    #:methods gen:e-writer
    [(define (e-write t)
       (match-define (id l r) t)
       (e-write-bin-expr l op r))])
    (provide (contract-out [id (-> r-expr? r-expr? r-expr?)]))))
(define-binary-r-expr r!= "!=")
(define-binary-r-expr r<= "<=")
(define-binary-r-expr r+ "+")
(define-binary-r-expr r* "*")
(define-binary-r-expr r- "-")

(struct r-app *r-expr (fe args)
  #:methods gen:e-writer
  [(define (e-write t)
     (match-define (r-app fe args) t)
     @dsp{(@ge-write[fe])(@(add-between (map ge-write args) ", "))})])

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
  (hash-ref (current-r-var-mapping) v
            (λ () (error 'r-var-mapping-ref "unbound var: ~e" (r-var-id-base v)))))
(define (r-write-var-def v)
  (ty-write-as (r-var-mapping-ref v) (r-var-ty v)))

(define current-r-global-mapping (make-parameter #f))
(define (r-global-mapping-add! v)
  (hash-ref! (current-r-global-mapping) v (gencsym (*r-fun-id v))))
(define (r-global-mapping-ref v)
  (define m (current-r-global-mapping))
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

(struct *r-statement ())
(struct r-begin *r-statement (stmts)
  #:methods gen:s-writer
  [(define (s-write t)
     (match-define (r-begin ss) t)
     @dsp{{@indent{@(add-between (map gs-write ss) (indentnl))}}})])
(struct *r-let1 *r-statement (v e body)
  #:methods gen:s-writer
  [(define (s-write t)
     (match-define (*r-let1 v e b) t)
     (with-r-var-mapping v
       @dsp{{@r-write-var-def[v] = @ge-write[e]@|semi|@(indentnl)@gs-write[b]}}))])
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

(struct *r-while *r-statement (cond body)
  #:methods gen:s-writer
  [(define (s-write t)
     (match-define (*r-while c b) t)
     @dsp{while (@ge-write[c]) {@(indentnl)@indent{@gs-write[b]}}})])
(define (r-while test . body)
  (*r-while test (r-begin body)))

(struct *r-for *r-statement (init cond step body)
  #:methods gen:s-writer
  [(define (s-write t)
     (match-define (*r-for i c s b) t)
     @dsp{for (@semi @ge-write[c]@semi @ge-write[s]) {@(indentnl)@indent{@gs-write[b]}}})])
(define-simple-macro (r-for [v-ty v v-e] v-cond v-step . body)
  (r-let* ([v-ty v v-e])
          (*r-for v v-cond v-step (r-begin (list . body)))))

(define r-lvalue?
  (or/c r-var?))
(struct r-set! *r-expr (lv rhs)
  #:methods gen:e-writer
  [(define (e-write t)
     (match-define (r-set! l r) t)
     @dsp{@ge-write[l] = @ge-write[r]})])
(struct *r-ret *r-statement (v)
  #:methods gen:s-writer
  [(define (s-write t)
     (match-define (*r-ret v) t)
     (if v
         @dsp{return @gs-write[v]@semi}
         @dsp{return@semi}))])
(define (r-ret [v #f])
  (*r-ret v))

(struct r-def ())
(struct *r-fun-extern (sym)
  #:methods gen:e-writer
  [(define (e-write t)
     (match-define (*r-fun-extern s) t)
     s)])

(struct *r-fun r-def (id args ret body)
  #:methods gen:e-writer
  [(define (e-write t)
     (r-global-mapping-ref t))])
(define (r-fun-write-decl t
                          #:name sym
                          #:visibility [vis ""]
                          #:proto-only? [proto-only? #f])
  (match-define (*r-fun id args ret body) t)
  (with-r-var-mapping args
    @dsp{@|vis|@ty-write[ret] @sym (@(add-between (map r-write-var-def args) ", "))@(if proto-only? ";" @indent{{@(indentnl)@gs-write[body]}})}))
(define-syntax (r-fun stx)
  (syntax-parse stx
    [(_ ([v-ty v:id] ...) (~datum :) r-ty body ...)
     (quasisyntax/loc stx
       (let ([v (r-var v-ty 'v)]
             ...)
         (*r-fun '#,(syntax-local-name) (list v ...) r-ty
                 (r-begin (list body ...)))))]))

(define r-fun?
  (or/c *r-fun-extern? *r-fun?))
(define r-expr?
  (or/c *r-expr? r-fun? integer? inexact-real? string?))
(define r-statement?
  (or/c *r-statement? r-expr?))

(define-generics r-decl-writer
  [r-decl-write-proto r-decl-writer]
  [r-decl-write r-decl-writer])

(struct r-decl ())
(struct r-public-fun r-decl (symbol f)
  #:methods gen:r-decl-writer
  [(define (r-decl-write t)
     (match-define (r-public-fun s f) t)
     (r-fun-write-decl f #:name s))
   (define (r-decl-write-proto t)
     (match-define (r-public-fun s f) t)
     (r-fun-write-decl f #:name s #:proto-only? #t))])
(struct r-private-fun r-decl (f)
  #:methods gen:r-decl-writer
  [(define (r-decl-write t)
     (match-define (r-private-fun f) t)
     (define s (r-global-mapping-add! f))
     (r-fun-write-decl f #:name s #:visibility "static "))
   (define (r-decl-write-proto t)
     (match-define (r-private-fun f) t)
     (define s (r-global-mapping-add! f))
     (r-fun-write-decl f #:name s #:visibility "static " #:proto-only? #t))])

(struct *r-exe (decls))
(define (r-exe-write t)
  (match-define (*r-exe ds) t)
  (parameterize ([current-r-global-mapping (make-hasheq)])
    ;; XXX discover these includes
    @dsp{#include <stdio.h>
         #include <stdint.h>
         @(map (λ (d) (cons (r-decl-write-proto d) "\n")) ds)
         @(map (λ (d) (cons (r-decl-write d) "\n\n")) ds)}))
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

(define ui64 (r-ty-verbatim "uint64_t"))
(define ui32 (r-ty-verbatim "uint32_t"))
(define ui16 (r-ty-verbatim "uint16_t"))
(define ui8 (r-ty-verbatim "uint8_t"))
(define si64 (r-ty-verbatim "int64_t"))
(define si32 (r-ty-verbatim "int32_t"))
(define si16 (r-ty-verbatim "int16_t"))
(define si8 (r-ty-verbatim "int8_t"))
(define stdio-printf (*r-fun-extern "printf"))

(provide
 r-fun
 r-let*
 r-for
 (contract-out
  [r-char r-ty?] [r-int r-ty?] [r-void r-ty?]
  [ui8 r-ty?] [ui16 r-ty?] [ui32 r-ty?] [ui64 r-ty?]
  [si8 r-ty?] [si16 r-ty?] [si32 r-ty?] [si64 r-ty?]
  [f32 r-ty?] [f64 r-ty?] [f128 r-ty?]
  [r-ptr (-> r-ty? r-ty?)]
  [stdio-printf r-fun?]
  [r-var (-> r-ty? symbol? r-expr?)]
  [r-app (-> r-expr? (listof r-expr?) r-expr?)]
  [*r-let1 (-> r-var? r-expr? r-statement? r-statement?)]
  [r-while (->* (r-expr?) () #:rest (listof r-statement?) r-statement?)]
  [*r-while (-> r-expr? r-statement? r-statement?)]
  [*r-for (-> r-expr? r-expr? r-statement? r-statement? r-statement?)]
  [r-set! (-> r-lvalue? r-expr? r-expr?)]
  [r-ret (->* () (r-expr?) r-statement?)]
  [*r-fun (-> symbol?
              (listof r-var?)
              r-ty?
              (listof r-statement?)
              r-fun?)]
  [r-public-fun (-> string? r-fun? r-decl?)]
  [r-private-fun (-> r-fun? r-decl?)]
  [r-exe (->* () () #:rest (listof r-decl?) r-exe?)]
  [r-emit (-> r-exe? void?)]))
