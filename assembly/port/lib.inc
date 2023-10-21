;; Library code
SYS_EXIT = 60
SYS_WRITE = 1
STDOUT = 1

;; Functions
macro fn method, a, b {
  mov rdi, a
  mov rsi, b
  call method
}

;; System calls
macro syscall1 number, a {
    mov rax, number
    mov rdi, a
    syscall
}

macro syscall2 number, a, b {
    mov rax, number
    mov rdi, a
    mov rsi, b
    syscall
}
macro syscall3 number, a, b, c {
    mov rax, number
    mov rdi, a
    mov rsi, b
    mov rdx, c
    syscall
}

;; Commands
macro exit code {
    syscall1 SYS_EXIT, code
}

macro print buf, count {
  syscall3 SYS_WRITE, STDOUT, buf, count
}

;; CODE

;; itoa integer to ascii representation
;; 
;; rdi - text buffer
;; rsi - number to convert
itoa:
    ;; Clear additional registers
    xor r8, r8
    xor r10, r10

    ;; Divisor is always 10 in this routine
    mov ecx, 0x0a

    ;; Detect number of chars
    mov eax, esi ;; Copy number into division register
.count_digits:
    inc r8 ;; increment digit counter
    xor edx, edx ;; clear dividend register
    div ecx ;; perform unsigned division by 10

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
    div ecx ;; perform unsigned division by 10

    ;; Write ASCII character to buffer
    mov r10d, eax ;; Save division
    add edx, 48 ;; convert remainder to ASCII
    mov byte [rdi+r8], dl ;; place digit in buffer

    ;; Check eax value from div
    cmp r10d, 0 ;; check division non-zero 
    jnz .write_digits

    ret
