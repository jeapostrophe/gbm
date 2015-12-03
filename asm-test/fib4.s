BITS 64
DEFAULT REL
%use altreg

;; System V AMD64 ABI
%define ria0 rdi
%define ria1 rsi
%define ria2 rdx
%define ria3 rcx
%define ria4 r8
%define ria5 r9
%define rfa0 xmm0
%define rfa1 xmm1
%define rfa2 xmm2
%define rfa3 xmm3
%define rfa4 xmm4
%define rfa5 xmm5
%define rfa6 xmm6
%define rfa7 xmm7
    
fac:
    mov r9, 1
ifac:
    cmp ria0, 0
    je fac_done
    imul r9, ria0
    sub ria0, 1
    jmp ifac
fac_done:
    mov r0, r9
    ret
section ROM
;;align 4096
c1: dw 55
section .bss
;;align 4096
v1:
align 64
v2:
