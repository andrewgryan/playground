format ELF64 executable

SYS_write = 1
SYS_exit = 60
SYS_socket = 41

AF_INET = 2
SOCK_STREAM = 1

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

segment readable executable
entry main
main:
    write STDOUT, start, start_len

    write STDOUT, socket_trace_msg, socket_trace_msg_len
    socket AF_INET, SOCK_STREAM, 0
    cmp rax, 0
    jl error
    mov dword [sockfd], eax
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
sockfd dd 0
start db "INFO: Starting Web-site!", 10
start_len = $ - start
error_msg db "ERROR!", 10
error_msg_len = $ - error_msg
socket_trace_msg db "INFO: Starting a socket...", 10
socket_trace_msg_len = $ - socket_trace_msg
ok_msg db "INFO: OK", 10
ok_msg_len = $ - ok_msg
