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
  (r-fun ([r-int argc] [(r-ptr (r-ptr r-char)) argv]) : r-int
         (r-let* ([ui64 r 0])
                 (r-for [ui32 i 0]
                        (r<= i 10000)
                        (r-set! i (r+ i 1))
                        (r-set! r (r-app fac (list 12))))
                 (r-app stdio-printf
                        (list "r = %llu\n" r)))
         (r-ret 0)))

(define exe
  (r-exe
   (r-public-fun "main" main)))

(module+ test)

(module+ main
  (r-emit exe))
