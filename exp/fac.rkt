#lang racket/base
(require racket/file
         racket/pretty
         "bytecode.rkt")

(module+ main
  (with-output-to-file
    "fac.bin"
    #:exists 'replace
    (λ ()
      (bytecode-write
       `((load-imm r0 0) ;; 0
         (load-imm r1 12) ;; 1
         (load-imm r2 1) ;; 2
         (load-imm r3 1) ;; 3
         (cmp i-eq r0 r1) ;; 4
         (control-imm conditional jump ,(+ 256 (- 9 5))) ;; 5
         (mul int r1 r2 r2) ;; 6
         (sub int r1 r3 r1) ;; 7
         (control-imm always jump ,(+ 256 (- 4 8))) ;; 8
         (halt) ;; 9
         ))))
  (pretty-print
   (bytecode-read
    (file->bytes "fac.bin"))))
