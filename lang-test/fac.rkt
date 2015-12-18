#lang racket/base
(require "r.rkt")

(define fac
  (r-fun ([ui64 n]) : ui64
         (r-let* ([ui64 acc 1])
                 (r-while (r!= n 0)
                          (r-set! acc (r* acc n))
                          (r-set! n (r- n 1)))
                 (r-ret acc))))

(define main
  (r-fun () : r-int
         ;; XXX how to make a call? Should we record deps? (given that
         ;; functions don't get their names until later and we don't
         ;; want to force them all to be listed in the exe?)
         (r-ret 0)))

(define exe
  (r-exe (r-public-fun "fac" fac)
         (r-private-fun "sfac" fac)
         (r-public-fun "main" main)))

(module+ test)

(module+ main
  (r-emit exe))
