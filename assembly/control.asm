format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS = 0

segment readable executable
entry main
main:
    ;; calculate string length dynamically
    mov rdi, msg
    call strlen
    mov rdx, rax
    write STDOUT, msg, rdx
    
    exit SUCCESS

strlen:
    ;; push registers
    push rdi
    xor rax, rax ;; zero rax register

.next_char:
    mov al, byte [rdi]  ;; move 1 byte into the lower part of the rax register
    cmp rax, 0 ;; check byte is zero
    je .done

    inc rdi ;; Increment pointer
    jmp .next_char

.done:
    ;; 0-byte encountered
    pop rsi
    sub rdi, rsi
    mov rax, rdi
    ret


segment readable writeable

msg db "Hello, World!", 10, 0
msg_len = $ - msg
