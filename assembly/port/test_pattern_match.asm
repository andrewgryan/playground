format ELF64 executable
include "lib.inc"
include "fns.asm"

;; MAIN
segment readable executable
entry main
main:
        fn4 pattern_match, template, template_len, pattern, pattern_len
        exit 0

;; TODO: understand pattern match API
pattern_match:
        ;; Store parameters
        push rbp
        push rsp

        mov rbp, rsp
        sub rbp, 48                  ;; bytes of stack memory

        ; ;; Store arguments on the stack
        mov [rbp], rdi
        mov [rbp+8], rsi
        mov [rbp+16], rcx
        mov [rbp+24], rdx

        ;; Local variables
        mov [rbp+32], 0
        mov [rbp+40], 0

        ;; Find position of first char
        mov r8, byte [rbp+8]         ;; pattern char
        .loop:
                fn4 prefix_match [], [], [], []
                cmp rax, 0
                jl .loop
        .done:

        ;; Restore stack pointer
        pop rsp
        pop rbp
        ret

;; DATA
segment readable writable
        template file "template.html"
        template_len = $ - template
        pattern db "{{message}}"
        pattern_len = $ - pattern
        value db "Hello, World!"
        value_len = $ - value
