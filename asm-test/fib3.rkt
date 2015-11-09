#lang racket/base
(define (fac i)
  (fac-inner i 1))
(define (fac-inner i acc)
  (if (zero? i)
      acc
      (fac-inner (sub1 i) (* acc i))))
(define before (current-inexact-milliseconds))
(define r (fac 12))
(define after (current-inexact-milliseconds))
(define span (- after before))
(printf "result is ~a (~ams)\n" r span)
