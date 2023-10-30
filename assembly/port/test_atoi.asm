format ELF64 executable

include "atoi.asm"
include "lib.inc"

;; MAIN
segment readable executable
entry main
main:
        fn2 atoi, msg, msg_len
        mov rcx, rax
        exit rcx

;; DATA
segment readable writable
msg db "8080"
msg_len = $ - msg
