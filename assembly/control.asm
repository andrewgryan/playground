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

    ;; Reverse string
    mov rdi, msg
    mov rsi, rdx
    call reverse

    ;; Write result
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

;; Reverse
reverse:
    ;; Save argument registers
    push rdi
    push rsi
    xor rax, rax

    ;; Correct for newline and 0-index
    dec rsi
    dec rsi

    ;; Swap two chars
    mov al, byte [rdi]
    mov bl, byte [rdi + rsi]
    mov byte [rdi + rsi], al
    mov byte [rdi], bl

.next:
    ;; Swap two chars
    inc rdi
    dec rsi
    dec rsi

    cmp rsi, 0
    jle .done

    mov al, byte [rdi]
    mov bl, byte [rdi + rsi]
    mov byte [rdi + rsi], al
    mov byte [rdi], bl
    jmp .next

.done:
    ;; Remove data from stack
    pop rsi
    pop rdi
    
    ret

segment readable writeable

msg db "Hello, World! This is a longer sentence.", 10, 0
msg_len = $ - msg
