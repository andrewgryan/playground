format ELF64 executable

include "lib.inc"
include "../x86_64.inc"
include "../socket.inc"
include "structs.inc"
include "route.asm"

;; GLOBALS
CARRIAGE_RETURN = 10
PORT = 36895 ;; 8080
MAX_CONN = 5
EXIT_SUCCESS = 0
EXIT_FAILURE = 1
REQUEST_CAP = 128*1024

;; MAIN
segment readable executable
entry main
main:
    ;; Command line parsing
    pop r8  ;; Number of arguments
    cmp r8, 2
    jne .usage

    ;; Program name
    pop rsi

    ;; Parse first positional argument
    pop r9
    mov rdi, r9
    call strlen
    mov r10, rax

    ;; Parse integer
    mov rdi, r9
    call atoi

    ;; Big-endian number
    ;; mov rdi, rax
    ;; call endian

    ;; Integer to ASCII
    mov r9, rax
    fn itoa, buf, r9 

    ;; Start HTTP server
    call serve

    ;; Exit with return code
    exit EXIT_SUCCESS

.usage:
    print usage_msg, usage_msg_len
    exit EXIT_FAILURE


;; HTTP server
serve:
    ;; Greeting
    write STDOUT, start, start_len

    ;; Create a socket
    socket AF_INET, SOCK_STREAM, 0
    cmp rax, 0
    jl .error
    mov qword [sockfd], rax
    write STDOUT, ok_msg, ok_msg_len

    ;; Bind socket on port
    write STDOUT, bind_trace_msg, bind_trace_msg_len
    mov word [servaddr.sin_family], AF_INET
    mov dword [servaddr.sin_addr], INADDR_ANY
    mov word [servaddr.sin_port], PORT
    bind [sockfd], servaddr.sin_family, servaddr_size
    cmp rax, 0
    jl .error
    ;; Listen to HTTP connections
    write STDOUT, listen_trace_msg, listen_trace_msg_len
    listen [sockfd], MAX_CONN
    cmp rax, 0
    jl .error

.next_http_request:
    ;; Accept a request (blocking I/O)
    write STDOUT, accept_trace_msg, accept_trace_msg_len
    accept [sockfd], cliaddr.sin_family, cliaddr_size
    cmp rax, 0
    jl .error

    ;; Store connection file descriptor
    mov qword [connfd], rax

    ;; TODO parse request header
    read [connfd], request, REQUEST_CAP
    cmp rax, 0
    jl .error

    ;; Echo HTTP request to STDOUT
    mov [request_len], rax
    mov [request_cur], request
    write STDOUT, [request_cur], [request_len]

    ;; Handle HTTP request
    call handle_request

    ;; Send HTTP response
    write [connfd], [response_cur], [response_len]
    close [connfd]

    ;; Event loop
    jmp .next_http_request

    ;; OK and Close
    write STDOUT, ok_msg, ok_msg_len
    close [connfd]
    close [sockfd]

    ret

.error:
    write STDERR, error_msg, error_msg_len
    close [connfd]
    close [sockfd]
    exit EXIT_FAILURE


;; HTTP Request handler
handle_request:
    xor rax, rax

    ;; GET /
    fn4 match_route, [request_cur], [request_len], route_index, route_index_len
    cmp rax, 0
    je .index

    ;; GET /hello.jpg
    fn4 match_route, [request_cur], [request_len], route_image, route_image_len
    cmp rax, 0
    je .image

    ;; 404
    jmp .not_found

.index:
    write [connfd], ok_header, ok_header_len
    write [connfd], html_header, html_header_len
    write [connfd], index, index_len
    jmp .done

.image:
    write [connfd], ok_header, ok_header_len
    write [connfd], jpg_header, jpg_header_len
    write [connfd], image, image_len
    jmp .done

.not_found:
    write [connfd], error_header, error_header_len
    write [connfd], html_header, html_header_len
    write [connfd], not_found, not_found_len
    jmp .done

.done:
    ret

;; Null-terminated string length
strlen:
    xor rax, rax

.loop:
    mov byte dl, [rdi]
    cmp dl, 0
    je .done

    ;; Increment counter
    inc rax
    inc rdi
    jmp .loop

.done:
    ret

   
;; Append
;; Scan string for 0, replace with char and append 0
;; @param {*text} - rdi
;; @param {char} - rsi
;; @returns {*text} - rax
append:
    push rdi     ;; Save buf location on stack
    xor rax, rax ;; Reset buffer to zero

.next:
    mov al, byte[rdi] ;; Read byte into rax
    cmp rax, 0        ;; Check is zero
    je .done

    inc rdi ;; Move to next character
    jmp .next

.done:
    mov byte [rdi], sil   ;; Write rsi char to rdi
    mov byte [rdi + 1], 0 ;; Append 0
    pop rdi               ;; Restore original pointer
    ret

;; Endian
endian:
    ;; Move first byte
    mov rcx, 0xFF  ;; Bit mask 0000000011111111
    shl rcx, 8     ;; Bit mask 1111111100000000
    and rcx, rdi   ;; rcx aaaaaaaa00000000
    shr rcx, 8     ;; rcx 00000000aaaaaaaa
    mov rax, rcx   ;; rax 00000000aaaaaaaa

    ;; Move second byte
    mov rcx, 0xFF  ;; Bit mask 0000000011111111
    and rcx, rdi   ;; rcx 00000000bbbbbbbb
    shl rcx, 8     ;; rcx bbbbbbbb00000000
    or rax, rcx    ;; rax bbbbbbbbaaaaaaaa
    ret

;; ASCII to Integer
;; rdi - pointer to byte array
;; rax - return value
atoi:
    mov rdi, 2
    mov rax, 10
    mul rdi

    mov rdi, rax
    mov rax, 10
    mul rdi
    ret

;; DATA
segment readable writable
buf db "      ", CARRIAGE_RETURN
len = $ - buf

;; Error message
usage_msg db "Usage: ./main [port]", 10
usage_msg_len = $ - usage_msg

;; HTTP Header
ok_header db "HTTP/1.1 200 OK", 13, 10
ok_header_len = $ - ok_header

error_header db "HTTP/1.1 404 Not Found", 13, 10
error_header_len = $ - error_header

html_header db "Content-Type: text/html; charset=utf-8", 13, 10
            db "Connection: close", 13, 10
            db 13, 10
html_header_len = $ - html_header

jpg_header db "Content-Type: image/jpg", 13, 10
           db "Connection: close", 13, 10
           db 13, 10
jpg_header_len = $ - jpg_header

;; Route
route_index db "GET / "
route_index_len = $ - route_index
route_image db "GET /hello.jpg "
route_image_len = $ - route_image

;; File name
index file "index.html"
index_len = $ - index

image file "hello.jpg"
image_len = $ - image

not_found file "404.html"
not_found_len = $ - not_found

;; HTTP Server data
sockfd dq -1
connfd dq -1

servaddr servaddr_in
servaddr_size = $ - servaddr.sin_family

cliaddr servaddr_in
cliaddr_size dd servaddr_size

start db "INFO: Starting Web-site!", 10
start_len = $ - start
error_msg db "ERROR", 10
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

;; Storage for request
request_len rq 1
request_cur rq 1
request rb REQUEST_CAP

response_len rq 1
response_cur rq 1
response rb REQUEST_CAP

