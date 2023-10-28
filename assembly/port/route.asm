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

.loop:
    ;; Compare ASCII values
    mov byte al, [rsi]
    mov byte cl, [rdx]
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
    ;; Set rax
    mov rax, r8

    ;; Restore registers
    pop rdx
    pop rsi
    pop rdi
    pop r8
    ret
