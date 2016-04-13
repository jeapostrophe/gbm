#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt"
         racket/list)

(define fac-rec
  ($proc (Fun ([UI64 n]) UI64)
         ($if ($<= n ($v UI64 0))
              ($ret ($v UI64 1))
              ($ret ($* n
                        ($app ($ddref (λ () fac-rec))
                              ($- n ($v UI64 1))))))))

(define fac
  ($proc (Fun ([UI64 n]) UI64)
         ($let1 ([UI64 acc ($v UI64 1)])
                ($while ($!= n ($v UI64 0))
                        ($set! acc ($* acc n))
                        ($set! n ($- n ($v UI64 1))))
                ($ret acc))))

(define main
  ($proc (Fun () SI32)
         (let ()
           (define (test-fac which fac)
             ($let1 ([UI64 r ($v UI64 0)])
                    ($for ([UI32 i ($in-range ($v UI32 10000))])
                          ($set! r ($app ($dref fac) ($v UI64 12))))
                    ($do ($app ($dref $printf)
                               ($v (format "~a r = %llu\n" which))
                               r))))
           ($begin
            (test-fac "iter" fac)
            (test-fac " rec" fac-rec)
            ($ret ($v SI32 0))))))

(define this
  ($default-flags ($exe main)))

(module+ test
  (emit! this)
  (run this))
