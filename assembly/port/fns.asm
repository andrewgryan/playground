;; Function signatures
macro fn1 method, a {
        mov rdi, a
        call method
}

;; 2 argument function call
macro fn2 method, a, b {
        mov rdi, a
        mov rsi, b
        call method
}

;; 4 argument function call
macro fn4 method, a, b, c, d {
        mov rdi, a
        mov rsi, b
        mov rcx, c
        mov rdx, d
        call method
}
