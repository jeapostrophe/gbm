#lang racket/base
(require "r.rkt")

(define <stdlib.h> (r-include "<stdlib.h>"))
(define-r-vals #:from <stdlib.h> EXIT_FAILURE)
(define-r-fns #:from <stdlib.h> exit)

(provide (all-defined-out))
