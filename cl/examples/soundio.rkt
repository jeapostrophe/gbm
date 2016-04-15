#lang racket/base
(require "../cl.rkt"
         "../h/libc.rkt"
         "../h/libsoundio.rkt")

(define PI
  ($var F32 ($val F32 3.1415926535)))

(define seconds-offset
  ($var F32 ($val F32 0.0)))

(define write-callback
  ($proc
   (Fun ([SoundIoOutStream outs]
         [SI32 frame-count-min]
         [SI32 frame-count-max])
        Void)
   ($let*
    ([(Ptr SoundIoChannelLayout) layout ($addr ($-> outs 'layout))]
     [F32 float-sample-rate ($-> outs 'sample_rate)]
     [F32 seconds-per-frame ($/ ($v F32 1.0) float-sample-rate)]
     [SoundIoChannelArea areas ($v SoundIoChannelArea $NULL)]
     [SI32 frames-left frame-count-max]
     [Bool stop? ($v #f)])
    ($while
     ($or stop? ($> frames-left ($val SI32 0)))
     ($let1
      ([SI32 frame-count frames-left])
      (check-sio-zero
       (soundio_outstream_begin_write outs
                                      ($addr areas)
                                      ($addr frame-count))
       "cannot begin write")
      ($cond
       [($== ($v SI32 0) frame-count)
        ($set! stop? ($v #t))]
       [#:else
        ($let*
         ([F32 pitch ($v F32 440.0)]
          [F32 radians-per-second
               ($* pitch ($* ($v F32 2.0) PI))])
         ($for ([SI32 frame ($in-range frame-count)])
               ($let1
                ([F32 sample
                      (sinf
                       ($* ($+ seconds-offset
                               ($* frame seconds-per-frame))
                           radians-per-second))])
                ($for ([SI32 ch ($in-range ($-> layout 'channel_count))])
                      ($set! ($pref
                              ($+ ($sref ($aref areas ch) 'ptr)
                                  ($* ($sref ($aref areas ch) 'step) frame)))
                             sample))))
         ($set! seconds-offset
                ($+ seconds-offset
                    ($* seconds-per-frame frame-count)))
         (check-sio-zero (soundio_outstream_end_write outs)
                         "cannot end write")
         ($set! frames-left
                ($- frames-left
                    frame-count)))]))))
   ($return)))

(define main
  ($proc
   (Fun () SI32)
   ($let1
    ([SoundIo sio (soundio_create)])
    (check-null sio)
    (check-sio-zero (soundio_connect sio)
                    "error connecting")
    ($do (soundio_flush_events sio))
    ($let1
     ([SI32 default-out (soundio_default_output_device_index sio)])
     (check-pos default-out "no output device found: %d\n")
     ($let1
      ([SoundIoDevice
        dev
        (soundio_get_output_device sio default-out)])
      (check-null dev)
      ($let1
       ([SoundIoOutStream
         outs
         (soundio_outstream_create dev)])
       ($set! ($-> outs 'format)
              SoundIoFormatFloat32NE)
       ($set! ($-> outs 'write_callback)
              write-callback)
       (check-sio-zero (soundio_outstream_open outs)
                       "unable to open device")
       (check-sio-zero ($-> outs 'layout_error)
                       "unable to set channel layout")
       (check-sio-zero (soundio_outstream_start outs)
                       "unable to start device")

       ($while ($v #t)
               ($do (soundio_wait_events sio)))

       ($do (soundio_outstream_destroy outs)))
      ($do (soundio_device_unref dev))))
    ($do (soundio_destroy sio)))
   ($ret ($v SI32 0))))

(define this
  ($default-flags ($exe main)))

(module+ test
  (emit! this)
  (run this))
