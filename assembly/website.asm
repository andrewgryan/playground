format ELF64 executable

SYS_write = 1
SYS_exit = 60
SYS_socket = 41
SYS_bind = 49

AF_INET = 2
SOCK_STREAM = 1
INADDR_ANY = 0
PORT = 14619 ;; 6969

STDOUT = 1
STDERR = 2

EXIT_SUCCESS = 0
EXIT_FAILURE = 1

macro write fd, buf, count
{
    mov rax, SYS_write
    mov rdi, fd
    mov rsi, buf
    mov rdx, count
    syscall
}

macro exit code
{
    mov rax, SYS_exit
    mov rdi, code
    syscall
}

;; int socket(int domain, int type, int protocol);
macro socket domain, type, protocol
{
    mov rax, SYS_socket
    mov rdi, domain
    mov rsi, type
    mov rdx, protocol
    syscall
}

macro syscall3 number, a, b, c
{
    mov rax, number
    mov rdi, a
    mov rsi, b
    mov rdx, c
    syscall
}

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
macro bind sockfd, addr, addrlen
{
    syscall3 SYS_bind, sockfd, addr, addrlen
}

segment readable executable
entry main
main:
    write STDOUT, start, start_len

    write STDOUT, socket_trace_msg, socket_trace_msg_len
    socket AF_INET, SOCK_STREAM, 0
    cmp rax, 0
    jl error
    mov qword [sockfd], rax
    write STDOUT, ok_msg, ok_msg_len

    write STDOUT, bind_trace_msg, bind_trace_msg_len
    mov word [servaddr.sin_family], AF_INET
    mov dword [servaddr.sin_addr], INADDR_ANY
    mov word [servaddr.sin_port], PORT
    bind [sockfd], servaddr.sin_family, sizeof_servaddr
    cmp rax, 0
    jl error
    write STDOUT, ok_msg, ok_msg_len

    exit EXIT_SUCCESS

error:
    write STDERR, error_msg, error_msg_len
    exit EXIT_FAILURE

;; db - 1 byte
;; dw - 2 byte
;; dd - 4 byte
;; dq - 8 byte
segment readable writeable
sockfd dq 0
servaddr.sin_family dw 0
servaddr.sin_port dw 0
servaddr.sin_addr dd 0
servaddr.sin_zero dq 0
sizeof_servaddr = $ - servaddr.sin_family

start db "INFO: Starting Web-site!", 10
start_len = $ - start
error_msg db "ERROR!", 10
error_msg_len = $ - error_msg
socket_trace_msg db "INFO: Starting a socket...", 10
socket_trace_msg_len = $ - socket_trace_msg
bind_trace_msg db "INFO: Bind the socket...", 10
bind_trace_msg_len = $ - bind_trace_msg
ok_msg db "INFO: OK", 10
ok_msg_len = $ - ok_msg

;; struct sockaddr_in {
;;     family 16 bits
;;     port   16 bits
;;     addr   32 bits
;;     zero[8] 64bits
;; }