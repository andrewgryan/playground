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
    mov rax, 1
    ret

.equal:
    mov rax, 0
    ret
