format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS = 0

segment readable executable
entry main
main:
    ;; Raw asm
    mov [buf], 0x21
    mov [buf+1], 0x0a

    write STDOUT, buf, 2
    
    exit SUCCESS

segment readable writeable

buf rb 2

msg db "Hello, World!", 10
msg_len = $ - msg
