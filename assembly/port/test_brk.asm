format ELF64 executable

; Example demonstrating dynamic allocation of data

SYS_write = 1
SYS_exit = 60
SYS_brk = 0x0c
STDOUT = 1

segment readable executable
entry main
main:
        ; Call brk to request memory address space
        mov rax, SYS_brk
        mov rdi, 0
        syscall

        ; Ask for a new breakpoint
        lea rdi, [rax + 256]
        mov rax, SYS_brk
        syscall

        ; Save breakpoint location
        mov r10, rax

        ; Write a character into the allocated address
        mov r8b, 33
.loop:
        mov [r10 + r8], byte r8b
        inc r8
        cmp r8, 255
        je .done
        jmp .loop

.done:
        ; Print
        mov rax, SYS_write
        mov rdi, STDOUT
        mov rsi, r10
        mov rdx, 256
        syscall

        ; Exit system call
        mov rax, SYS_exit
        mov rdi, 0
        syscall
