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
	sub rbp, 48

	; Command line arguments
	pop rax
	cmp rax, 3
	jne .abort

	pop qword [rbp]  		; discard program name
	pop qword [rbp]  		; 1st arg
	pop qword [rbp + 16]    	; 2nd arg

	call1 strlen, [rbp]		; 1st arg length
	mov [rbp + 8], rax		; 1st arg length save
	call1 strlen, [rbp + 16]	; 2nd arg length
	mov [rbp + 24], rax		; 2nd arg length save

	; echo [rbp + 0], [rbp + 8] 	; Echo arg 1
	; echo [rbp + 16], [rbp + 24] 	; Echo arg 2

	; Stack-based str
	call quote
	mov r10, rax
	echo r10, 12

	exit 0

.abort:
	echo msg, msg_len
	exit 1

quote:
	push rbp
	mov rbp, rsp
	sub rbp, 12
	lea r10, [rbp]
	mov [r10 + 0], dword 0x79646E41
	mov [r10 + 4], dword 0x6C757220
	mov [r10 + 8], dword 0x0a217365
	pop rbp
	mov rax, r10
	ret

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
