format ELF64 executable

macro write fd, buf, count
{
    mov rax, 1
    mov rdi, fd
    mov rsi, buf
    mov rdx, count
    syscall
}

segment readable executable
entry main
main:
    write 1, msg, msg_len

    mov rax, 60
    mov rdi, 0
    syscall

segment readable writeable
msg db "Foo, Bar!", 10
msg_len = $ - msg
