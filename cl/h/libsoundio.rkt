#lang racket/base
(require racket/match
         "../cl.rkt"
         "../h/libc.rkt"
         syntax/parse/define)

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

(define-simple-macro (define-sio-f f)
  (define f ($extern <soundio.h> (symbol->string 'f))))
(define-simple-macro (define-sio-fs f ...)
  (begin (define-sio-f f) ...))

(define-sio-fs
  soundio_create
  soundio_connect
  soundio_flush_events
  soundio_default_output_device_index
  soundio_get_output_device
  soundio_outstream_create
  soundio_outstream_open
  soundio_outstream_start
  soundio_wait_events
  soundio_outstream_destroy
  soundio_device_unref
  soundio_destroy
  soundio_strerror
  soundio_outstream_begin_write
  soundio_outstream_end_write
  SoundIoFormatFloat32NE)

(define SoundIo (Extern <soundio.h> "struct SoundIo"))
(define SoundIoDevice (Extern <soundio.h> "struct SoundIoDevice"))
(define SoundIoOutStream (Extern <soundio.h> "struct SoundIoOutStream"))
(define SoundIoChannelLayout (Extern <soundio.h> "struct SoundIoChannelLayout"))
(define SoundIoChannelArea (Extern <soundio.h> "struct SoundIoChannelArea"))

(define (check-sio-zero e m)
  (check-zero e
              #:m (λ (err)
                    (list (format "~a: %s\n" m)
                          (soundio_strerror err)))))

(provide (all-defined-out))
