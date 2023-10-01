;; System call x86_64 codes
SYS_read = 0
SYS_write = 1
SYS_exit = 60
SYS_socket = 41
SYS_accept = 43
SYS_bind = 49
SYS_listen = 50
SYS_close = 3

;; I/O
STDOUT = 1
STDERR = 2

;; System calls
macro syscall1 number, a {
    mov rax, number
    mov rdi, a
    syscall
}

macro syscall3 number, a, b, c {
    mov rax, number
    mov rdi, a
    mov rsi, b
    mov rdx, c
    syscall
}

macro exit code {
    syscall1 SYS_exit, code
}

macro write fd, buf, count {
    syscall3 SYS_write, fd, buf, count
}
