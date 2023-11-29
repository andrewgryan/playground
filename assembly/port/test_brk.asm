format ELF64 executable

; Example demonstrating dynamic allocation of data

SYS_exit = 60
SYS_brk = 0x0c

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
        lea rdi, [rax + 2]
        mov rax, SYS_brk
        syscall

        ; Subtract breakpoint locations
        sub r8, rax

        ; Exit system call
        mov rax, SYS_exit
        mov rdi, r8
        syscall
