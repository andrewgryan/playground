;; Match prefix
;; ------------
;; E.g. 'GET / ' in a HTTP request header
;;
;; rdi int prefix length
;; rsi *char prefix buffer
;; rdx *char string buffer
;;
match_prefix:
    xor rax, rax ;; Set rax to 0
    push r8      ;; Save r8
    push rdi     ;; Save prefix length
    push rsi     ;; Save string pointer
    push rdx     ;; Save string pointer
    push rbp     ;; Save base pointer
    mov rbp, rsp ;; Set base pointer to stack pointer
    sub rsp, 2   ;; Allocate space for variables

.loop:
    ;; Assign chars to local variables
    mov byte al, [rsi]
    mov byte [rbp-2], al
    mov byte al, [rdx]
    mov byte [rbp-1], al

    ;; Compare ASCII values
    mov byte al, [rbp-2]
    mov byte cl, [rbp-1]
    cmp al, cl
    jne .not_equal

    ;; Increment buffer pointers
    inc rsi
    inc rdx

    ;; Decrement prefix length
    dec rdi
    cmp rdi, 0
    je .equal

    jmp .loop

.not_equal:
    mov r8, 1
    jmp .done

.equal:
    mov r8, 0
    jmp .done

.done:
    ;; Reset stack and base pointers
    mov rsp, rbp
    pop rbp

    ;; Set rax
    mov rax, r8

    ;; Restore registers
    pop rdx
    pop rsi
    pop rdi
    pop r8
    ret
