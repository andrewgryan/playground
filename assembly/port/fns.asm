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
