format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS equ 0
FAILURE equ 1

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

    ;; Reverse twice
    mov rdi, msg
    call reverse

    ;; Write result
    write STDOUT, msg, rdx

    ;; startswith "GET "
    mov rdi, http_message
    mov rsi, http_get
    call startswith
    cmp rax, 0
    je get

    mov rdi, http_message
    mov rsi, http_post
    call startswith
    cmp rax, 0
    je post

    jmp error
    
    exit SUCCESS

get:
    write STDOUT, get_msg, get_msg_len
    exit SUCCESS

post:
    write STDOUT, post_msg, post_msg_len
    exit SUCCESS

error:
    write STDOUT, error_msg, error_msg_len
    exit FAILURE

startswith:
    xor rax, rax

    ;; Algorithm
    mov al, byte [rdi]
    mov bl, byte [rsi]

    sub al, bl  ;; updates lower part of rax register
    ret

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
    sub rsi, 2

    ;; Swap two chars
    mov al, byte [rdi]
    mov bl, byte [rdi + rsi]
    mov byte [rdi + rsi], al
    mov byte [rdi], bl

.next:
    ;; Swap two chars
    inc rdi
    sub rsi, 2

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

get_msg db "GET detected!", 10, 0
get_msg_len = $ - get_msg
post_msg db "POST detected!", 10, 0
post_msg_len = $ - post_msg
error_msg db "Error detected!", 10, 0
error_msg_len = $ - error_msg

;; Example HTTP message
http_message db "POST /favicon.ico", 10, 0
http_get db "GET ", 0
http_post db "POST ", 0
