#lang racket/base
(require "r.rkt")

(define <stdio.h> (r-include "<stdio.h>"))
(define-r-fns #:from <stdio.h> printf)

(provide (all-defined-out))
