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
    mov rsi, qword 1234567890
    call itoa
    
    ;; Print buffer
    write STDOUT, buf, 10
    exit 0

;; itoa integer to ascii representation
;; 
;; rdi - text buffer
;; rsi - number to convert
itoa:
    ;; Clear additional registers
    xor r8, r8
    xor r10, r10

    ;; Detect number of chars
    mov eax, esi ;; Copy number into division register
.count_digits:
    inc r8 ;; increment digit counter
    xor edx, edx ;; clear dividend register
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division

    cmp eax, 0 ;; check division non-zero 
    jnz .count_digits
    
    ;; Fill buffer with ASCII characters
    mov eax, esi ;; Copy number into division register

    ;; NULL terminate buffer
    inc r8 ;; one position past digits
    mov byte [rdi + r8], 0 ;; NULL character

.write_digits:
    dec r8 ;; decrement digit counter

    ;; Divide number by 10
    xor edx, edx ;; clear dividend register
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division

    ;; Write ASCII character to buffer
    mov r10d, eax ;; Save division
    add edx, 48 ;; convert remainder to ASCII
    mov byte [rdi+r8], dl ;; place digit in buffer

    ;; Check eax value from div
    cmp r10d, 0 ;; check division non-zero 
    jnz .write_digits

    ret

;; DATA
segment readable writable
buf rb 11
