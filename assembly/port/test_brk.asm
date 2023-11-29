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

        ; Save address of breakpoint
        mov r8, rax

        ; Ask for a new breakpoint
        lea rdi, [rax + 3]
        mov rax, SYS_brk
        syscall

        ; Save breakpoint location
        mov r10, rax

        ; Write a character into the allocated address
        mov [r10], byte 49
        mov [r10 + 1], byte 50
        mov [r10 + 2], byte 10

        ; Print
        mov rax, SYS_write
        mov rdi, STDOUT
        mov rsi, r10
        mov rdx, 3
        syscall

        ; Exit system call
        mov rax, SYS_exit
        mov rdi, 0
        syscall
