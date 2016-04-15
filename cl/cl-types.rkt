#lang racket/base
(require racket/contract/base
         (for-syntax racket/base
                     syntax/parse)
         syntax/parse/define
         "define-type.rkt")

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

(define (Lval? x)
  (or ($aref? x)
      ($pref? x)
      ($vref? x)
      ($dref? x)
      ($sref? x)
      ($uref? x)))

;; XXX const

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
  Any
  (Opaque [n CName?])
  (Extern [h CHeader?] [t Type?])
  (Delay [-ty (-> Type?)])
  (Seal [n Seal-Id?] [ty Type?]))

(define String (Ptr Char))
(define Bool (Seal 'Bool UI8))

(define-simple-macro
  (define-Op1 x [ov iv] ...)
  (begin (define-type x iv ...)
         (define-simple-macro (ov a)
           ($op1 iv a))
         ...))

(define-Op1 Op1
  [$! $%!]
  [$neg $%neg]
  [$bneg $%bneg])


(define-simple-macro
  (define-Op2 x [ov iv] ...)
  (begin (define-type x iv ...)
         (define-syntax (ov stx)
           (syntax-parse stx
             [(_ a b)
              (syntax/loc stx
                ($op2 a iv b))]))
         ...))

(define-Op2 Op2
  [$+ $%+] [$- $%-] [$* $%*] [$/ $%/] [$% $%%]
  [$== $%==] [$!= $%!=] [$> $%>] [$< $%<] [$>= $%>=] [$<= $%<=]
  [$and $%and] [$or $%or]
  [$band $%band] [$bior $%bior] [$bxor $%bxor] [$bshl $%bshl] [$bshr $%bshr])

(define Integer-Types
  (list Size Char
        UI8 UI16 UI32 UI64
        SI8 SI16 SI32 SI64))
(define Numeric-Types
  (list* F32 F64 F128 Integer-Types))

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

(define-type Expr
  ($sizeof [ty Type?])
  ($offsetof [ty Record?] [f Field?])
  ($op1 [op Op1?] [arg Expr?])
  ($op2 [lhs Expr?] [op Op2?] [rhs Expr?])
  ($val [t Type?] [v Val?])
  ($%app [rator Expr?] [rands (listof Expr?)])
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
  ($%while [test Expr?] [body Stmt?])
  ($%let1 [ty Type?] [v Var?] [body Stmt?])
  ($set! [lhs Lval?] [rhs Expr?])
  ($ret [val Expr?])
  ($return))

(define-type Decl
  ($extern [h CHeader?] [n CName?] [ty Type?])
  ($typedef [hn symbol?] [ty Type?])
  ($%proc [hn symbol?] [ty Fun?] [body Stmt?])
  ($%var [hn symbol?] [ty (and/c Type? (not/c Fun?))] [val Expr?]))

(define-type Unit
  ($cflags [flags (listof string?)] [u Unit?])
  ($ldflags [flags (listof string?)] [u Unit?])
  ($exe [main $%proc?])
  ($lib [ds (hash/c CName? Decl?)]))

(provide (all-defined-out))
