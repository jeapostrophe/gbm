#lang racket/base
(require "../cl.rkt")

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define $stderr ($extern <stdio.h> "stderr" Any))
(define $printf ($extern <stdio.h> "printf" Any))
(define $fprintf ($extern <stdio.h> "fprintf" Any))

(define <stdlib.h> (CHeader '() '() '() "<stdlib.h>" '()))
(define $exit ($extern <stdlib.h> "exit" (Fun (list (cons 'status SI32)) Void)))

(define <math.h> (CHeader '() '("-lm") '() "<math.h>" '()))
(define $sinf ($extern <math.h> "sinf" (Fun (list (arg 'x F32)) F32)))

(define (check-zero e #:m [mf (λ (err) (list "non-zero return code: %d\n" err))])
  ;; XXX i wish i could inspect the type of e
  ($let1 ([SI32 err e])
         ($unless ($== ($v SI32 0) err)
                  (apply $fprintf $stderr (mf err))
                  ($exit 1))))

(define (check-pos e #:m [mf (λ (err) (list "non-pos return code: %d\n" err))])
  ;; XXX i wish i could inspect the type of e
  ($let1 ([SI32 err e])
         ($unless ($< ($v SI32 0) err)
                  (apply $fprintf $stderr (mf err))
                  ($exit 1))))

(define (check-null e #:m [m "null pointer!\n"])
  ;; XXX i wish i could inspect the type of e
  ($when ($== e ($v (Ptr Any) $NULL))
         ($fprintf $stderr m)
         ($exit 1)))

(provide (all-defined-out))
