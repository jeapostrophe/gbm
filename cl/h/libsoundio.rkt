#lang racket/base
(require racket/match
         "../cl.rkt"
         "../h/libc.rkt")

(define <soundio.h>
  (CHeader (match (system-type 'os)
             ['macosx '("-L/usr/local/include")]
             [_ '()])
           (append
            (match (system-type 'os)
              ['macosx '("-L/usr/local/lib")]
              [_ '()])
            '("-lsoundio"))
           '() "<soundio/soundio.h>" '()))

(define soundio_strerror ($extern <soundio.h> "soundio_strerror"))

(define (check-sio-zero e m)
  (check-zero e
              #:m (λ (err)
                    (list (format "~a: %s\n" m)
                          (soundio_strerror err)))))

