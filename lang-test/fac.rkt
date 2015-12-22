#lang racket/base
(require "r.rkt"
         (prefix-in stdio: "stdio.rkt"))

(define fac
  (r-fun ([ui64 n]) : ui64
         (r-let* ([ui64 acc 1])
                 (r-while (r!= n 0)
                          (r-set! acc (r* acc n))
                          (r-set! n (r- n 1)))
                 (r-ret acc))))

(define fac-rec
  (r-fun ([ui64 n]) : ui64
         (r-if (r<= n 0)
               (r-ret 1)
               (r-ret (r* n (r-app (r-rec fac-rec) (list (r- n 1))))))))

(define main
  (r-fun ([r-int argc] [(r-ptr (r-ptr r-char)) argv]) : r-int
         (let ()
           (define (test-fac which fac)
             (r-let* ([ui64 r 0])
                     (r-for [ui32 i 0]
                            (r<= i 10000)
                            (r-set! i (r+ i 1))
                            (r-set! r (r-app fac (list 12))))
                     (r-app stdio:printf
                            (list (format "~a r = %llu\n" which) r))))
           (r-begin (list (test-fac "iter" fac)
                          (test-fac " rec" fac-rec))))
         (r-ret 0)))

(define exe
  (r-exe
   (r-public-fun "main" main)))

(module+ test)

(module+ main
  (r-emit exe))
