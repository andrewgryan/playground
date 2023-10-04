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

    ;; Detect number of chars
    xor edx, edx ;; clear dividend
    mov eax, esi ;; dividend

.loop:
    inc r8 ;; increment digit counter
    xor edx, edx ;; clear dividend
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division

    cmp eax, 0 ;; check division non-zero 
    jnz .loop
    

    ;; Fill buffer with ASCII characters
    xor edx, edx ;; clear dividend
    mov eax, esi ;; dividend

    ;; NULL terminate
    inc r8
    mov byte [rdi + r8], 0

    ;; Reset digit counter
    xor r9, r9
    xor r10, r10
.fill:
    dec r8 ;; decrement digit counter

    ;; Divide number by 10
    xor edx, edx ;; clear dividend
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division

    ;; Write ASCII character to buffer
    mov r9d, eax ;; Save division
    mov r10d, edx ;; Save remainder
    add r10d, 48 ;; convert remainder to ASCII
    mov byte [rdi+r8], r10b ;; place digit in buffer

    ;; Loop condition
    cmp r9d, 0 ;; check division non-zero 
    jnz .fill


    ret

;; DATA
segment readable writable
buf rb 10
