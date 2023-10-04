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
    mov sil, 42
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
    xor r10, r10

    ;; Division
    xor edx, edx ;; clear dividend
    mov eax, esi ;; dividend
    mov ecx, 0x0a ;; divisor
    div ecx ;; perform unsigned division
    

    ;; TODO fill buffer with ASCII characters


    add eax, 48  ;; convert digit to ASCII
    add edx, 48  ;; convert digit to ASCII
    mov byte [rdi], al

    inc rdi
    mov byte [rdi], dl

    ;; NULL terminate
    inc rdi
    mov byte [rdi], 0

    ret

;; DATA
segment readable writable
buf rb 10
