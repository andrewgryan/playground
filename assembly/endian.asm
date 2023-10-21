format ELF64 executable

include "strlen.inc"

;; System call exit
SYS_write = 1
SYS_exit = 60
PORT = 8080 ;; How many bytes does this have? 8 bytes?

STDOUT = 1

;; MACROS
macro print buf {
    ;; string length
    mov rdi, buf
    call strlen
    mov rdx, rax

    ;; syscall print
    mov rax, SYS_write
    mov rdi, STDOUT
    mov rsi, buf
    syscall
}

;; MAIN
segment readable executable
entry main
main:
    ;; Big endian (Network endian)
    mov rbx, PORT
    mov rcx, PORT
    shr rbx, 8
    shl rcx, 7 * 8
    shr rcx, 6 * 8
    add rbx, rcx

    ;; Convert number to string
    ;; mov y, x
    ;; div x, 10
    ;; mul x, 10
    ;; sub y, x
    ;; add y, 48


    ;; Print a number
    print age

    ;; Exit with code
    mov rax, SYS_exit
    mov rdi, rbx
    syscall

;; DATA
segment readable writeable

age db 48, 48, 48, 48, 48, 10, 0 ;; ASCII encoding
