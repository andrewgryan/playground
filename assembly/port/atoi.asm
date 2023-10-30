;; ASCII to integer
;;
;; @param buffer
;; @param buffer length
;; @returns 64-bit integer
atoi:
        xor rax, rax
        xor r8, r8
        xor r9, r9
        xor r10, r10
        mov r10, 1          ;; Set factor register to 1
.while:
        ;; Check characters to parse
        cmp rsi, 0
        je .done

        ;; Read digits backwards
        mov r8b, byte [rdi + rsi - 1] ;; Read a character
        sub r8b, 0x30       ;; Subtract 48

        ;; Multiply factor by digit
        mov rax, r8         ;; rax = digit
        mul r10             ;; rax = rax * r10
        mov r8, rax         ;; r8 = rax

        ;; Add term
        add r9, r8          ;; r9 = r9 + r8 

        ;; Mutiply factor by 10
        mov rax, 0xa        ;; rax = 10
        mul r10             ;; rax = rax * r10
        mov r10, rax        ;; r10 = rax

        ;; Decrement buffer length
        dec rsi

        jmp .while
.done:
        mov rax, r9         ;; Copy total to return register
        ret

;; 2 argument function call
macro fn2 method, a, b {
    mov rdi, a
    mov rsi, b
    call method
}
