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

(define-generics c-writer
  [c-write c-writer]
  #:fast-defaults
  ([number?
    (define (c-write t) (number->string t))]))
(define-generics c-write-asr
  [c-write-as str c-write-asr])
(define gc-write c-write)
(define gc-write-as c-write-as)

(struct r-ty ())
(struct r-ty-verbatim r-ty (in-c)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r-ty-verbatim ic) t)
     ic)]
  #:methods gen:c-write-asr
  [(define (c-write-as s t)
     (match-define (r-ty-verbatim ic) t)
     @dsp{@ic @s})])
(define r-int (r-ty-verbatim "int"))
(define r-void (r-ty-verbatim "void"))
(define ui64 (r-ty-verbatim "uint64_t"))
(define ui32 (r-ty-verbatim "uint32_t"))
(define ui16 (r-ty-verbatim "uint16_t"))
(define ui8 (r-ty-verbatim "uint8_t"))
(define si64 (r-ty-verbatim "int64_t"))
(define si32 (r-ty-verbatim "int32_t"))
(define si16 (r-ty-verbatim "int16_t"))
(define si8 (r-ty-verbatim "int8_t"))
(define f32 (r-ty-verbatim "float"))
(define f64 (r-ty-verbatim "double"))
(define f128 (r-ty-verbatim "long double"))

(struct *r-expr ())
(struct r-var *r-expr (ty id-base)
  #:methods gen:c-writer
  [(define (c-write t)
     (r-var-mapping-ref t))])
(define (r-var-id v)
  ;; XXX incorporate id-base in a safe way
  (symbol->string (gensym)))

(define (c-write-bin-expr l op r)
  @dsp{((@gc-write[l]) @op (@gc-write[r]))})
(struct r!= *r-expr (lhs rhs)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r!= l r) t)
     (c-write-bin-expr l "!=" r))])
(struct r* *r-expr (lhs rhs)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r* l r) t)
     (c-write-bin-expr l "*" r))])
(struct r- *r-expr (lhs rhs)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r- l r) t)
     (c-write-bin-expr l "-" r))])
(define r-expr?
  (or/c *r-expr? integer? inexact-real?))

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
  (hash-ref (current-r-var-mapping) v))
(define (c-write-var-def v)
  (c-write-as (r-var-mapping-ref v) (r-var-ty v)))

(define current-indent (make-parameter 0))
(define-syntax-rule (indent . b)
  (parameterize ([current-indent (add1 (current-indent))])
    (list . b)))
(define (indentnl)
  (cons "\n"
        (for/list ([i (in-range (current-indent))])
          " ")))

(struct r-statement ())
(struct r-begin r-statement (stmts)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r-begin ss) t)
     @dsp{{@indent{@(add-between (map gc-write ss) (indentnl))}}})])
(struct *r-let1 r-statement (v e body)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (*r-let1 v e b) t)
     (with-r-var-mapping v
       @dsp{{@c-write-var-def[v] = @gc-write[e]@|semi|@(indentnl)@gc-write[b]}}))])
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
(struct *r-while r-statement (cond body)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (*r-while c b) t)
     @dsp{while (@gc-write[c]) {@(indentnl)@indent{@gc-write[b]}}})])
(define (r-while test . body)
  (*r-while test (r-begin body)))
(define r-lvalue?
  (or/c r-var?))
(struct r-set! r-statement (lv rhs)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (r-set! l r) t)
     @dsp{@gc-write[l] = @gc-write[r]@semi})])
(struct *r-ret r-statement (v)
  #:methods gen:c-writer
  [(define (c-write t)
     (match-define (*r-ret v) t)
     (if v
         @dsp{return @gc-write[v]@semi}
         @dsp{return@semi}))])
(define (r-ret [v #f])
  (*r-ret v))

(struct r-def ())
(struct *r-fun r-def (args ret body))
(define (r-fun-write t #:name sym
                     #:visibility [vis ""]
                     #:proto-only? [proto-only? #f])
  (match-define (*r-fun args ret body) t)
  (with-r-var-mapping args
    @dsp{@|vis|@gc-write[ret] @sym (@(add-between (map c-write-var-def args) ", "))@(if proto-only? ";" @indent{{@(indentnl)@gc-write[body]}})}))
(define r-fun? *r-fun?)
(define-simple-macro (r-fun ([v-ty v:id] ...) (~datum :) r-ty body ...)
  (let ([v (r-var v-ty 'v)]
        ...)
    (*r-fun (list v ...) r-ty
            (r-begin (list body ...)))))

(define-generics r-decl-writer
  [r-decl-write-proto r-decl-writer]
  [r-decl-write r-decl-writer])

(struct r-decl ())
(struct r-decl-fun r-decl (symbol f))
(struct r-public-fun r-decl-fun ()
  #:methods gen:r-decl-writer
  [(define (r-decl-write t)
     (match-define (r-decl-fun s f) t)
     (r-fun-write f #:name s))
   (define (r-decl-write-proto t)
     (match-define (r-decl-fun s f) t)
     (r-fun-write f #:name s #:proto-only? #t))])
(struct r-private-fun r-decl-fun ()
  #:methods gen:r-decl-writer
  [(define (r-decl-write t)
     (match-define (r-decl-fun s f) t)
     (r-fun-write f #:name s #:visibility "static "))
   (define (r-decl-write-proto t)
     (match-define (r-decl-fun s f) t)
     (r-fun-write f #:name s #:visibility "static " #:proto-only? #t))])

(struct *r-exe (decls))
(define (r-exe-write t)
  (match-define (*r-exe ds) t)
  @dsp{#include <stdint.h>
                
       @(map (λ (d) (cons (r-decl-write-proto d) "\n")) ds)
       @(map (λ (d) (cons (r-decl-write d) "\n\n")) ds)})
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

(provide
 r-fun
 r-let*
 (contract-out
  [r-int r-ty?] [r-void r-ty?]
  [ui8 r-ty?] [ui16 r-ty?] [ui32 r-ty?] [ui64 r-ty?]
  [si8 r-ty?] [si16 r-ty?] [si32 r-ty?] [si64 r-ty?]
  [f32 r-ty?] [f64 r-ty?] [f128 r-ty?]
  [r-var (-> r-ty? symbol? r-expr?)]
  [r!= (-> r-expr? r-expr? r-expr?)]
  [r* (-> r-expr? r-expr? r-expr?)]
  [r- (-> r-expr? r-expr? r-expr?)]
  [*r-let1 (-> r-var? r-expr? r-statement? r-statement?)]
  [r-while (->* (r-expr?) () #:rest (listof r-statement?) r-statement?)]
  [*r-while (-> r-expr? r-statement? r-statement?)]
  [r-set! (-> r-lvalue? r-expr? r-statement?)]
  [r-ret (->* () (r-expr?) r-statement?)]
  [*r-fun (-> (listof r-var?)
              r-ty?
              (listof r-statement?)
              r-fun?)]
  [r-public-fun (-> string? r-fun? r-decl?)]
  [r-private-fun (-> string? r-fun? r-decl?)]
  [r-exe (->* () () #:rest (listof r-decl?) r-exe?)]
  [r-emit (-> r-exe? void?)]))
