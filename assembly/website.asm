format ELF64 executable

;; Include additional asm code
include "x86_64.inc"
include "socket.inc"

;; Socket programming
;; PORT = 14619 ;; 6969 -> hex -. big endian -> decimal
PORT = 36895 ;; 8080
MAX_CONN = 5

;; Program return code
EXIT_SUCCESS = 0
EXIT_FAILURE = 1

;; HTTP Message buffer size
REQUEST_CAP = 128*1024

struc servaddr_in
{
    .sin_family dw 0
    .sin_port dw 0
    .sin_addr dd 0
    .sin_zero dq 0
}

segment readable executable
entry main
main:
    ;; Greeting
    write STDOUT, start, start_len

    ;; Create a socket
    write STDOUT, socket_trace_msg, socket_trace_msg_len
    socket AF_INET, SOCK_STREAM, 0
    cmp rax, 0
    jl error
    mov qword [sockfd], rax
    write STDOUT, ok_msg, ok_msg_len

    ;; Bind socket on port
    write STDOUT, bind_trace_msg, bind_trace_msg_len
    mov word [servaddr.sin_family], AF_INET
    mov dword [servaddr.sin_addr], INADDR_ANY
    mov word [servaddr.sin_port], PORT
    bind [sockfd], servaddr.sin_family, servaddr_size
    cmp rax, 0
    jl error

    ;; Listen to HTTP connections
    write STDOUT, listen_trace_msg, listen_trace_msg_len
    listen [sockfd], MAX_CONN
    cmp rax, 0
    jl error

next_request:
    ;; Accept a request (blocking I/O)
    write STDOUT, accept_trace_msg, accept_trace_msg_len
    accept [sockfd], cliaddr.sin_family, cliaddr_size
    cmp rax, 0
    jl error

    ;; Store connection file descriptor
    mov qword [connfd], rax

    ;; TODO parse request header
    read [connfd], request, REQUEST_CAP
    cmp rax, 0
    jl error

    ;; Echo HTTP request to STDOUT
    mov [request_len], rax
    mov [request_cur], request
    write STDOUT, [request_cur], [request_len]

    ;; Handle HTTP request
    call handle_request

    ;; Send HTTP response
    write [connfd], [response_cur], [response_len]
    close [connfd]

    ;; Loop
    jmp next_request

    write STDOUT, ok_msg, ok_msg_len
    close [connfd]
    close [sockfd]
    exit EXIT_SUCCESS

error:
    write STDERR, error_msg, error_msg_len
    close [connfd]
    close [sockfd]
    exit EXIT_FAILURE


handle_request:
    ;; TODO implement branching logic and response
    write [connfd], form, form_len
    ret

;; db - 1 byte
;; dw - 2 byte
;; dd - 4 byte
;; dq - 8 byte
segment readable writeable
sockfd dq -1
connfd dq -1

servaddr servaddr_in
servaddr_size = $ - servaddr.sin_family

cliaddr servaddr_in
cliaddr_size dd servaddr_size

start db "INFO: Starting Web-site!", 10
start_len = $ - start
error_msg db "ERROR!", 10
error_msg_len = $ - error_msg
socket_trace_msg db "INFO: Starting a socket...", 10
socket_trace_msg_len = $ - socket_trace_msg
bind_trace_msg db "INFO: Bind the socket...", 10
bind_trace_msg_len = $ - bind_trace_msg
listen_trace_msg db "INFO: Listening to socket...", 10
listen_trace_msg_len = $ - listen_trace_msg
accept_trace_msg db "INFO: Waiting for client connections...", 10
accept_trace_msg_len = $ - accept_trace_msg
ok_msg db "INFO: OK", 10
ok_msg_len = $ - ok_msg

;; Hello, World message
hello db "Hello, from fast assembler!", 10
hello_len = $ - hello

;; Storage for request
request_len rq 1
request_cur rq 1
request rb REQUEST_CAP

response_len rq 1
response_cur rq 1
response rb REQUEST_CAP

;; GET /
form db "HTTP/1.1 200 OK", 13, 10
     db "Content-Type: text/html; charset=utf-8", 13, 10
     db "Connection: close", 13, 10
     db 13, 10
     db "<h1>Hello, from fast assembler</h1>", 10
     db "<form action='/' method='post'><button type='submit'>Submit</button></form>", 10
form_len = $ - form

 
;; struct sockaddr_in {
;;     family 16 bits
;;     port   16 bits
;;     addr   32 bits
;;     zero[8] 64bits
;; }