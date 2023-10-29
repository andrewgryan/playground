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
    mov byte r9b, [rsi+rdi]
    mov byte r10b, [rdx+rdi]
    cmp r9b, r10b
    jne .not_equal

    ;; Decrement prefix length
    dec rdi
    cmp rdi, 0
    jl .equal

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

;; 4-argument function
macro fn4 method, a, b, c, d {
    mov rdi, a
    mov rsi, b
    mov rdx, c
    mov rcx, d
    call method
}

;; Match sized-strings
;;
;; rdi buf
;; rsi buf_len
;; rdx prefix
;; rcx prefix_len
;;
match_route:
    xor rax, rax
    xor r8, r8
    xor r9, r9

.while:
    ;; Exit if no buffer chars left
    cmp rsi, 0
    jle .done

    ;; Exit if no prefix chars left
    cmp rcx, 0
    jle .done

    ;; Compare chars
    mov r8b, byte [rdi] 
    mov r9b, byte [rdx] 
    cmp r8b, r9b
    jne .done
    
    ;; Next char
    dec rsi
    inc rdi

    ;; Next char
    dec rcx
    inc rdx

    jmp .while

.done:
    cmp rcx, 0
    je .equal

.not_equal:
    mov rax, -1
    ret

.equal:
    mov rax, 0
    ret
