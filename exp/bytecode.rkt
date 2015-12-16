#lang racket/base
(require racket/format
         racket/match
         racket/string
         racket/list)

(define ENCODER (make-hasheq))
(define KIND-ENCODER (make-hasheq))

(define (make-constant-set-encoder vs)
  (define bs (bits-for vs))
  (λ (val)
    (or
     (for/or ([v (in-list vs)]
              [i (in-naturals)])
       (and (eq? val v)
            (cons i bs)))
     (error 'bytecode-write "Unknown value: ~v" val))))
(define (make-constant-encoder bs)
  (λ (i)
    (cons i (bits-count bs))))

(module+ main
  (displayln "#include \"bitfields.h\""))

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
  (define ty (type-for (bits-for vals)))
  (displayln
   (~a "typedef " ty " " (Cify id) ";\n"
       (string-append*
        (for/list ([v (in-list vals)]
                   [i (in-naturals)])
          (~a "#define " (Cify id v) " " i "\n"))))))

(define-syntax-rule (define-constant-set id (val ...))
  (begin (define id '(val ...))
         (hash-set! KIND-ENCODER 'id (make-constant-set-encoder id))
         (module+ main
           (constant-set-dynamic 'id id))))
(define-syntax-rule (define-constant id #:bits bits-c)
  (begin (define id (bits bits-c))
         (hash-set! KIND-ENCODER 'id (make-constant-encoder id))
         (module+ main
           (show-bits-for 'id id)
           (displayln (~a "typedef " (type-for (bits-for id)) " " (Cify 'id) ";\n")))))

(define (type-for bits)
  (cond
    [(<= bits 8) "uint8_t"]
    [(<= bits 16) "uint16_t"]
    [else "uint32_t"]))

(define (display-c-bitfield-accessors big prefix fields)
  (displayln
   (~a "typedef uint16_t " big ";\n"
       (string-append*
        (let ()
          (define start
            (- 16
               (apply + (map third fields))))
          (for/list ([f (in-list (reverse fields))])
            (match-define (list* kind name bits maybe-default) f)
            (begin0
                (~a "static inline " kind " " (Cify prefix name) "(uint32_t y) {"
                    " return bf_get(y, " start ", " bits "); "
                    "}\n")
              (set! start (+ start bits)))))))))

(define op '())
(define op-printer '())
(define op-definer '())
(define-syntax-rule (define-op (id [kind f] ...))
  (let ([my-id (length op)])
    (set! op (append op '(id)))
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
              (display-c-bitfield-accessors
               (Cify 'op 'id 't) (Cify 'op 'id)
               (list (list (Cify 'op) 'code opcode-bits (Cify 'op 'id))
                     (list (Cify 'kind) 'f (bits-for kind))
                     ...))))))
    (set! op-definer
          (cons (λ (opcode-bits)
                  (hash-set! ENCODER 'id
                             (make-encoder
                              (vector my-id
                                      opcode-bits
                                      (list 'kind ...)))))
                op-definer))))

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
(define-constant-set control-cond (always conditional))
(define-constant-set control-mode (call jump))
(define-op (control [control-cond cond] [control-mode mode] [sreg addr]))
(define-op (return))
(define-op (halt))

;; Immediate Control
;; You can jump within an (+/-) 9-bit range of the PC (the
;; bottom bit is always assumed to be 0, because instructions are 16-bit
;; aligned)
(define-constant control-imm
  #:bits 9)
(define-op (control-imm [control-cond cond] [control-mode mode] [control-imm offset]))

;; You can call in an "extended" way with:
;; 0000 000<9 bits> <16 bits>
;; where the bottom 16 bits are from the next instruction
(define-op (control-immx [control-cond cond] [control-mode mode] [control-imm offset]))

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
  (display-c-bitfield-accessors
   (Cify 'op 't) (Cify 'op)
   (list (list (Cify 'op) 'code (bits-for op))
         (list "uint16_t" 'rest (- 16 (bits-for op))))))

;;;;;;;;;

(define (combine-bits l-of-v*bc)
  (define-values (b tbc)
    (for/fold ([b 0] [tbc 0])
              ([v*bc (in-list l-of-v*bc)])
      (match-define (cons v bc) v*bc)
      (values (+ (arithmetic-shift b bc) v)
              (+ tbc bc))))
  (arithmetic-shift b (- 16 tbc)))

(define (make-encoder v)
  (match-define (vector opcode opcode-bits fields) v)
  (λ (i)
    (combine-bits
     (cons
      (cons opcode opcode-bits)
      (for/list ([f (in-list fields)]
                 [i (in-list (rest i))])
        ((hash-ref KIND-ENCODER f) i))))))

(for ([i (in-list op-definer)])
  (i (bits-for op)))

(define (instruction->uint16 i)
  (match i
    [(? number? n) n]
    [(cons instr _)
     ((hash-ref ENCODER instr) i)]))

(define (bytecode-write is)
  (for ([i (in-list is)])
    (define n (instruction->uint16 i))
    (eprintf
     (~a (~r n #:min-width 5) " = " (~r n #:base 2 #:min-width 16 #:pad-string "0")
         " = " i "\n"))
    (display (integer->integer-bytes n 2 #f))))

(define (bytecode-read bs)
  '(XXX))

(provide bytecode-write
         bytecode-read)
