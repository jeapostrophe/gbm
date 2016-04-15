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

(define-simple-macro (define-sio-f f t)
  (define f ($extern <soundio.h> (symbol->string 'f) t)))
(define-simple-macro (define-sio-fs [f t] ...)
  (begin (define-sio-f f t) ...))

(define SoundIo (Extern <soundio.h> (Opaque "struct SoundIo")))
(define SoundIoDevice (Extern <soundio.h> (Opaque "struct SoundIoDevice")))
(define SoundIoOutStream (Extern <soundio.h> (Opaque "struct SoundIoOutStream")))
(define SoundIoChannelLayout (Extern <soundio.h> (Opaque "struct SoundIoChannelLayout")))
(define SoundIoChannelArea (Extern <soundio.h> (Opaque "struct SoundIoChannelArea")))

(define-sio-fs
  [soundio_create Any]
  [soundio_connect Any]
  [soundio_flush_events Any]
  [soundio_default_output_device_index Any]
  [soundio_get_output_device Any]
  [soundio_outstream_create Any]
  [soundio_outstream_open Any]
  [soundio_outstream_start Any]
  [soundio_wait_events Any]
  [soundio_outstream_destroy Any]
  [soundio_device_unref Any]
  [soundio_destroy Any]
  [soundio_strerror Any]
  [soundio_outstream_begin_write Any]
  [soundio_outstream_end_write Any]
  [SoundIoFormatFloat32NE Any])

(define (check-sio-zero e m)
  (check-zero e
              #:m (λ (err)
                    (list (format "~a: %s\n" m)
                          (soundio_strerror err)))))

(provide (all-defined-out))
