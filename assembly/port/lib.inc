;; Library code
SYS_EXIT = 60
SYS_WRITE = 1
STDOUT = 1

;; System calls
macro syscall1 number, a {
    mov rax, number
    mov rdi, a
    syscall
}

macro syscall2 number, a, b {
    mov rax, number
    mov rdi, a
    mov rsi, b
    syscall
}
macro syscall3 number, a, b, c {
    mov rax, number
    mov rdi, a
    mov rsi, b
    mov rdx, c
    syscall
}

;; Commands
macro exit code {
    syscall1 SYS_EXIT, code
}

macro print buf, count {
  syscall3 SYS_WRITE, STDOUT, buf, count
}
