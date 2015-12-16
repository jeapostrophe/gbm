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
       `((load-immx r4 0) ;; 0
         10000 ;; 1
         
         (load-imm r0 0) ;; 2
         (load-imm r1 12) ;; 3
         (load-imm r2 1) ;; 4
         (load-imm r3 1) ;; 5         
         (cmp i-eq r0 r1) ;; 6
         (control-imm conditional jump ,(+ 256 (- 11 7))) ;; 7
         (mul int r1 r2 r2) ;; 8
         (sub int r1 r3 r1) ;; 9
         (control-imm always jump ,(+ 256 (- 6 10))) ;; 10

         (sub int r4 r3 r4) ;; 11
         (cmp i-ne r0 r4) ;; 12
         (control-imm conditional jump ,(+ 256 (- 2 13))) ;; 13
         
         (halt) ;; 14
         ))))
  (pretty-print
   (bytecode-read
    (file->bytes "fac.bin"))))
