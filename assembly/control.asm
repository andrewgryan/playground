format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS = 0

segment readable executable
entry main
main:
    ;; Raw asm
    mov [buf], 0x21

    write STDOUT, buf, 1
    
    exit SUCCESS

segment readable writeable

buf rb 1

msg db "Hello, World!", 10
msg_len = $ - msg
