format ELF64 executable
include "lib.inc"

;; MAIN
segment readable executable
entry main
main:
        ;; System V ABI
        mov rdi, template
        mov rsi, template_len
        mov rcx, pattern
        mov rdx, pattern_len
        call pattern_match
        exit 0

;; TODO: understand pattern match API
pattern_match:
        mov r8, rdi
        mov r9, rsi
        print r8, r9
        ret

;; DATA
segment readable writable
        template file "template.html"
        template_len = $ - template
        pattern db "{{message}}"
        pattern_len = $ - pattern
        value db "Hello, World!"
        value_len = $ - value
