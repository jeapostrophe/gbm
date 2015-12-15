#lang racket/base
(require racket/format
         racket/string
         racket/list)

;; XXX Compile to C structure definition and Racket code to construct
;; & emit

(struct bits (count))
(define (bits-for x)
  (cond
    [(bits? x)
     (bits-count x)]
    [(list? x)
     (integer-length (sub1 (length x)))]))
(define (disp-for x)
  (cond
    [(bits? x)
     (expt 2 (bits-count x))]
    [(list? x)
     (length x)]))

(define (show-bits-for id vals)
  (displayln
   (~a "// " (~a #:min-width 13 id) " = " (~a #:min-width 4 (disp-for vals))
       " (" (~a #:min-width 2 (bits-for vals)) " bits)")))

(define (Cify . args)
  (string-append*
   (add-between
    (map (λ (x)
           (regexp-replace* #rx"-"
                            (regexp-replace* #rx"->"
                                             (string-upcase (~a x))
                                             "_TO_")
                            "_"))
         args)
    "_")))

(define (constant-set-dynamic id vals)
  (show-bits-for id vals)
  (displayln
   (~a "typedef enum { "
       (string-append*
        (add-between
         (map (λ (val) (Cify id val)) vals)
         ", "))
       " } "
       (Cify id)
       ";\n")))

(define-syntax-rule (define-constant-set id (val ...))
  (begin (define id '(val ...))
         (module+ main
           (constant-set-dynamic 'id id))))
(define-syntax-rule (define-constant id #:bits bits-c)
  (begin (define id (bits bits-c))
         (module+ main
           (show-bits-for 'id id)
           (displayln (~a)))))

(define op '())
(define op-printer '())
(define-syntax-rule (define-op (id [kind f] ...))
  (begin (set! op (append op '(id)))
         (set! op-printer
               (append
                op-printer
                (list
                 (λ (opcode-bits)
                   (displayln
                    (~a "// " (~a #:min-width 12 'id)
                        " = "
                        (~a #:min-width 2
                            (+ opcode-bits
                               (bits-for kind)
                               ...))
                        " bits"))
                   (displayln
                    (~a "typedef struct {\n"
                        "  unsigned code:" opcode-bits "; // = " (Cify 'op 'id) "\n"
                        (~a "  unsigned " 'f ":" (bits-for kind) ";\n")
                        ...
                        "} "
                        (Cify 'op 'id 't)
                        ";\n"))))))))

;; There are 8 registers
;; Each register is 32 bits
(define-constant-set sreg (r0 r1 r2 r3 r4 r5 r6 r7))

;; Arith
(define-constant-set arith-mode (int float))
(define-constant-set div-mode (uint sint float))
(define-op (add [arith-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-op (sub [arith-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-op (mul [arith-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-op (div [div-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-op (rem [div-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-constant-set fprim-mode
  (sqrt sin cos exp exp2 log log10 log2 abs
        floor ceil trunc
        rint nearbyint round))
(define-op (fprim [fprim-mode mode] [sreg src] [sreg dst]))

;; Bits
(define-constant-set shift-mode (left right-logical right-arithmetic))
(define-op (shift [shift-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))
(define-constant-set bit-mode (and ior xor nand))
(define-op (bitwise [bit-mode mode] [sreg lhs] [sreg rhs] [sreg dst]))

;; Conversion
;; XXX I don't know if this is enough stuff or if they are all
;; necessary
(define-constant-set conv-ty (u8->u16
                              u8->u32
                              u8->float
                              s8->s16
                              s8->s32
                              s8->float

                              u16->u8
                              u16->u32
                              u16->float
                              s16->s8
                              s16->s32
                              s16->float

                              u32->u8
                              u32->u16
                              u32->float
                              s32->s8
                              s32->s16
                              s32->float

                              float->s8
                              float->s16
                              float->s32))
(define-op (conv [sreg src] [conv-ty ty] [sreg dst]))

;; Comparison
(define-constant-set cmp-code
  (i-false i-eq i-ne i-ugt i-uge i-ult i-ule i-sgt i-sge i-slt i-sle i-true
           f-false f-oeq f-ogt f-oge f-olt f-ole f-one f-ord f-ueq f-ugt f-uge f-ult f-ule f-une f-uno f-true))
(define-op (cmp [cmp-code mode] [sreg lhs] [sreg rhs]))

;; Control
(define-constant-set control-mode (call jump))
(define-op (control [control-mode mode] [sreg addr]))
(define-op (return))
(define-op (halt))

;; Immediate Control
;; You can jump within an (+/-) 11-bit range of the PC (the
;; bottom bit is always assumed to be 0, because instructions are 16-bit
;; aligned)
(define-constant control-imm
  #:bits 11)
(define-op (control-imm [control-imm offset]))

;; You can call in an "extended" way with:
;; 0000 0<11 bits> <16 bits>
;; where the bottom 16 bits are from the next instruction
(define-op (control-immx [control-imm offset]))

;; Move
(define-constant load-imm
  #:bits 8)
(define-op (load-imm [sreg dst] [load-imm val]))

;; You can do an "extended" immediate which loads the destination with
;; 0000 0000 <8 bits> <16 bits>
;; where the bottom 16 bits are from the next instruction
(define-op (load-immx [sreg dst] [load-imm hi]))
(define-op (select [sreg x] [sreg y] [sreg dst]))

;; Memory

;; Memory is accessed via 4 "buffers" of 32-bytes each
(define-constant-set breg (b0 b1 b2 b3))

;; There are distinct regions that can be accessed: the ROM, RAM,
;; STACK, "system" (which is for input/output), and "debug" (which is
;; lost)
(define-constant-set load-space (rom ram stack sys))
(define-constant-set store-space (debug ram stack sys))

(define-constant-set buf-mem-mode (read write))
(define-constant-set buf-mem-sync (sync async))
(define-op (buf-mem [buf-mem-mode mode] [buf-mem-sync sync]
                    [load-space src] [sreg addr] [breg dst]))
(define-op (mem-fence))

;; Once a buffer is filled with memory, you can access its elements in
;; either 1, 2, or 4 bytes chunks. There are 32 1-byte chunks, 16
;; 2-byte chunks, and 8 4-byte chunks.
;;
;; 00_ aaa = Access 4-byte chunk a
;; 01a aaa = Access 2-byte chunk a
;; 1aa aaa = Access 1-byte chunk a
(define-constant buf-idx
  #:bits 6)

(define-op (buf-set [sreg src] [buf-idx idx] [breg dst]))
(define-op (buf-read [sreg dst] [buf-idx idx] [breg src]))

;; You can simultaneously store to/from all registers (useful for
;; saving everything before a function call)
(define-constant-set buf-all-mode (read write))
(define-op (buf-all [buf-all-mode mode] [breg dst]))

;; Stack
(define-constant-set stack-dir (read write))
(define-op (stack-mov [stack-dir dir] [sreg dst]))

;;;;;;;;;
(module+ main
  (constant-set-dynamic 'op op)
  (for ([i (in-list op-printer)])
    (i (bits-for op)))
  (displayln
   (~a "typedef struct {\n"
       "  unsigned code:" (bits-for op) ";\n"
       "  unsigned rest:" (- 16 (bits-for op)) ";\n"
       "} OP_T;\n"))
  (displayln
   (~a "void bytecode_switch_template ( OP_T op ) {\n"
       "  switch ( op.code ) {\n"
       (string-append*
        (for/list ([o (in-list op)])
          (~a "    case " (Cify 'op o) ":\n"
              "      break;\n")))
       "  }\n"
       "}\n")))

;;;;;;;;;
