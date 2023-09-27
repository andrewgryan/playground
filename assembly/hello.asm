format ELF64 executable

segment readable executable
entry main
main:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, 14
    syscall

    mov rax, 60
    mov rdi, 0
    syscall

segment readable writeable
msg db "Hello, World!", 10
