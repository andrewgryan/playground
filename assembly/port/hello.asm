format ELF64 executable

SYS_exit = 60
SYS_write = 1
STDOUT = 1

macro exit return_code {
	mov rax, SYS_exit
	mov rdi, return_code
	syscall
}

macro echo buf, len {
	mov rax, SYS_write
	mov rdi, STDOUT
	mov rsi, buf
	mov rdx, len
	syscall
}

macro call1 method, a {
	mov rdi, a
	call method
}


segment readable executable
entry main
main:
	mov rbp, rsp
	sub rbp, 32

	; Print alphabet
	mov r8, 26
	mov r9, 48

.loop:
	dec r8
	inc r9
	mov r9, [rbp + r8]
	cmp r8, 0
	jnz .loop

	mov rax, SYS_write
	mov rdi, STDOUT
	mov rsi, rbp
	mov rdx, 26
	syscall

	exit 0

.abort:
	echo msg, msg_len
	exit 1


;; string length
;;
;; rdi - address of str
strlen:
	xor r8, r8
	dec r8
.loop:
	inc r8
	cmp byte [rdi + r8], 0
	jnz .loop
	mov rax, r8
	ret
	

segment readable writable
	msg db "too few arguments", 0xa
	msg_len = $ - msg

	icon db 0x78
	icon_len = $ - icon
