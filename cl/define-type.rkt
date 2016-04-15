#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     racket/syntax)
         racket/contract/base
         racket/match
         syntax/quote)

;; XXX include stx information in structs implicitly to track them?
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
    (pattern (v:id [f:id ctc:expr] ...
                   (~optional (~seq #:procedure proc:id)))
             #:with v? (format-id #'v "~a?" #'v)
             #:with a-v (generate-temporary #'v)
             #:with cv (generate-temporary #'v)
             #:with a-v? (format-id #'a-v "~a?" #'a-v)
             #:with maybe-prop-proc
             (if (attribute proc)
                 #'(#:property prop:procedure
                    (λ (me . args)
                      (proc me args)))
                 #'())
             #:with (cvf ...) #'(f ...)
             #:attr ? #'v?
             #:attr def
             (syntax/loc #'v
               (begin (struct a-v (f ...)
                        #:transparent
                        #:reflection-name 'v
                        . maybe-prop-proc)
                      (define v? a-v?)
                      (define-match-expander v
                        (λ (stx)
                          (syntax-parse stx
                            [(_ . x) (syntax/loc stx (a-v . x))]))
                        (make-rename-transformer #'cv))
                      (define-syntax (cv stx)
                        (syntax-parse stx
                          [(_ cvf ...)
                           (with-syntax
                             ([pos (syntax-source stx)]
                              [neg (syntax-source #'v)])
                             (quasisyntax/loc stx
                               (let ([srcloc (quote-syntax/keep-srcloc #,stx)])
                                 (a-v (contract ctc cvf 'pos 'neg 'f srcloc)
                                      ...))))])))))))

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
      [(_ ty ve:expr c:clause ...)
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
       (quasisyntax/loc stx
         (let ([v ve])
           (match v c ... [_ (error (or '#,(syntax-local-name) 'ty)
                                    "Given non-~a value: ~e" 'ty v)])))])))

(provide define-type
         match-type)
