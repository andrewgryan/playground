format ELF64 executable

STDOUT = 1

;; MACROS
macro exit code {
    mov rax, 0x3c
    mov rdi, code
    syscall
}

macro write fd, buf, buf_len {
    mov rax, 0x01
    mov rdi, fd
    mov rsi, buf
    mov rdx, buf_len
    syscall
}

;; MAIN
segment readable executable
entry main

main:
    ;; Convert number to ASCII
    mov rdi, buf
    mov rsi, qword 8080
    call itoa
    
    ;; Print buffer
    write STDOUT, buf, 10
    exit 0

;; itoa integer to ascii representation
;; 
;; rdi - text buffer
;; rsi - number to convert
itoa:

    ;; TODO calculate length of buffer taken up by number
    xor r8, r8

    ;; Division
    xor eax, eax ;; clear dividend
    xor edx, edx ;; clear dividend
    mov eax, esi ;; dividend

.loop:
    inc r8 ;; increment digit counter
    xor edx, edx ;; clear dividend
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division

    cmp eax, 0 ;; check division non-zero 
    jnz .loop
    

    ;; TODO fill buffer with ASCII characters


    ;; Print loop counter
    add r8b, 48  ;; convert digit to ASCII
    mov byte [rdi], r8b

    ;; NULL terminate
    inc rdi
    mov byte [rdi], 0

    ret

;; DATA
segment readable writable
buf rb 10
