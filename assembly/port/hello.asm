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
	; Stack storage
	mov rbp, rsp
	sub rbp, 16

	; Command line arguments
	pop rax
	cmp rax, 2
	jne .abort

	pop qword [rbp]  	;; discard program name
	pop qword [rbp]  	;; 1st arg
	call1 strlen, [rbp]	;; length of 1st arg
	mov [rbp + 8], rax

	; Echo argument 1
	echo [rbp], [rbp + 8]
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
