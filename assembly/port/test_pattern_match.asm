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
        sub rbp, 32  ;; Make 32 bytes of temporary memory

        ; ;; Store arguments on the stack
        mov [rbp], rdi
        mov [rbp+8], rsi
        mov [rbp+16], rcx
        mov [rbp+24], rdx

        ;; Find position of first char
        print [rbp], [rbp+8]
        print [rbp+16], [rbp+24]

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
