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

    ;; rdi register should not change between calls
    mov rdi, http_message

    ;; startswith "GET "
    call is_get
    cmp rax, 0
    je get

    ;; startswith "POST "
    call is_post
    cmp rax, 0
    je post

    ;; startswith "DELETE "
    call is_delete
    cmp rax, 0
    je delete

    jmp error
    
    exit SUCCESS

get:
    write STDOUT, get_msg, get_msg_len
    exit SUCCESS

post:
    write STDOUT, post_msg, post_msg_len
    exit SUCCESS

delete:
    write STDOUT, delete_msg, delete_msg_len
    exit SUCCESS

error:
    write STDOUT, error_msg, error_msg_len
    exit FAILURE


;; Check HTTP method is GET
is_get:
    mov rsi, http_get
    call startswith
    ret

;; Check HTTP method is POST
is_post:
    mov rsi, http_post
    call startswith
    ret

;; Check HTTP method is DELETE
is_delete:
    mov rsi, http_delete
    call startswith
    ret

;; Check NULL-terminated string starts with NULL-terminated string
;;
;; rdi - void *text
;; rsi - void *text
;;
;; returns rax
startswith:
    xor rax, rax  ;; Zero rax register
    xor rbx, rbx  ;; Zero rbx register
    push rdi
    push rsi

.step:
    ;; Load next pair of bytes
    mov al, byte [rdi]
    mov bl, byte [rsi]

    ;; If prefix terminated exit with succeed
    cmp bl, 0
    je .succeed

    ;; Compare bytes
    cmp al, bl  ;; updates lower part of rax register
    jne .fail

    ;; Update indices
    inc rdi
    inc rsi

    ;; Loop while bytes equal
    jmp .step

.fail:
    mov rax, 1  ;; False
    jmp .done

.succeed:
    mov rax, 0  ;; True
    jmp .done

.done:
    pop rsi
    pop rdi
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
delete_msg db "DELETE detected!", 10, 0
delete_msg_len = $ - delete_msg
error_msg db "Error detected!", 10, 0
error_msg_len = $ - error_msg

;; Example HTTP message
http_message db "DELETE /favicon.ico", 10, 0
http_get db "GET ", 0
http_get_len = $ - http_get
http_post db "POST ", 0
http_post_len = $ - http_post
http_delete db "DELETE ", 0
http_delete_len = $ - http_delete
