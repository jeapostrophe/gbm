#lang racket/base
(require "../cl.rkt")

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
;; XXX need to have typed versions
(define $stderr ($extern <stdio.h> "stderr"))
(define $printf ($extern <stdio.h> "printf"))
(define $fprintf ($extern <stdio.h> "fprintf"))

(define <stdlib.h> (CHeader '() '() '() "<stdlib.h>" '()))
(define $exit ($extern <stdlib.h> "exit"))

(define <math.h> (CHeader '() '("-lm") '() "<math.h>" '()))
;; XXX add $?
(define sinf ($extern <math.h> "sinf"))

(define (check-zero e #:m [mf (λ (err) (list "non-zero return code: %d\n" err))])
  ($let1 ([SI32 err e])
         ($unless ($== ($v SI32 0) err)
                  (apply $fprintf $stderr (mf err))
                  ($exit 1))))

(define (check-pos e #:m [mf (λ (err) (list "non-pos return code: %d\n" err))])
  ($let1 ([SI32 err e])
         ($unless ($< ($v SI32 0) err)
                  (apply $fprintf $stderr (mf err))
                  ($exit 1))))

(define (check-null e #:m [m "null pointer!\n"])
  ($when ($== e ($v (Ptr Any) $NULL))
         ($fprintf $stderr m)
         ($exit 1)))

(provide (all-defined-out))
