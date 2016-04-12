#lang racket/base
(require "../cl.rkt")

(define <stdio.h> (CHeader '() '() '() "<stdio.h>" '()))
(define $stderr ($extern <stdio.h> "stderr"))
(define $printf ($extern <stdio.h> "printf"))
(define $fprintf ($extern <stdio.h> "fprintf"))

(define <stdlib.h> (CHeader '() '() '() "<stdlib.h>" '()))
(define $exit ($extern <stdlib.h> "exit"))

(define (check-zero e #:m [mf (λ (err) (list "non-zero return code: %d\n" err))])
  ($let1 ([SI32 err e])
         ($unless ($== ($v SI32 0) err)
                  (apply $fprintf $stderr (mf err))
                  ($exit 1))))

(provide (all-defined-out))
