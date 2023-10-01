format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS = 0

segment readable executable
entry main
main:
    write STDOUT, msg, msg_len
    exit SUCCESS

segment readable writeable
msg db "Hello, World!", 10
msg_len = $ - msg
