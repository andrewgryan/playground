;; Socket programming
AF_INET = 2
SOCK_STREAM = 1
INADDR_ANY = 0

;; int socket(int domain, int type, int protocol);
macro socket domain, type, protocol {
    mov rax, SYS_socket
    mov rdi, domain
    mov rsi, type
    mov rdx, protocol
    syscall
}

;; int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
macro bind sockfd, addr, addrlen {
    syscall3 SYS_bind, sockfd, addr, addrlen
}

macro listen sockfd, backlog {
    syscall2 SYS_listen, sockfd, backlog
}

macro accept sockfd, addr, addrlen {
    syscall3 SYS_accept, sockfd, addr, addrlen
}

macro close fd {
    syscall1 SYS_close, fd
}
