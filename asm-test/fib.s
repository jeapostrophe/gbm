.text
.globl _main
_main:
    movq $15, %r8
Lfac:
    movq $1, %r9
    movq $0, %r10
Lfac_loop:   
    cmpq %r8, %r10
    je Lfac_done
    imulq %r8, %r9
    subq $1, %r8
    jmp Lfac_loop
Lfac_done:   
    movq %r9, %rax
    ret
