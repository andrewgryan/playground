format ELF64 executable
include "lib.inc"
include "fns.asm"
include "route.asm"

;; MAIN
segment readable executable
entry main
main:
        print template, template_len
        fn4 pattern_match, template, template_len, pattern, pattern_len
        mov r9, rax
        exit r9

;; TODO: understand pattern match API
pattern_match:
        ;; Store parameters
        push rbp
        push rsp

        mov rbp, rsp
        sub rbp, 48                  ;; bytes of stack memory

        ; ;; Store arguments on the stack
        mov [rbp + 0], rdi
        mov [rbp + 8], rsi
        mov [rbp + 16], rdx
        mov [rbp + 24], rcx

        ;; Local variable
        mov [rbp + 32], dword 0  ;; index = 0

        ;; Find position of first char
        .loop:
                ;; Bounds check
                cmp [rbp + 16], dword 0
                je .done
                cmp [rbp + 24], dword 0
                je .done

                ;; Search for match
                fn4 match_route, [rbp], [rbp + 8], [rbp + 16], [rbp + 24]
                cmp rax, 0
                je .done

                ;; Move forward one char
                inc dword [rbp]        ;; tmpl = tmpl[1:]
                dec dword [rbp + 8]    ;; len(tmpl) -= 1
                inc dword [rbp + 32]   ;; index += 1

                jmp .loop
        .done:
                cmp [rbp + 8], dword 0 ;; len(tmpl) == 0
                je .fail
                jmp .success

        .success:
                mov eax, [rbp + 32]  ;; index
                jmp .end

        .fail:
                mov eax, [rbp + 32]  ;; index
                jmp .end

        .end:
                ;; Restore stack pointer
                pop rsp
                pop rbp
                ret

;; DATA
segment readable writable
        debug db "Debug", 10
        debug_len = $ - debug
        template file "template.html"
        template_len = $ - template
        pattern db "{{message}}"
        pattern_len = $ - pattern
        value db "Hello, World!"
        value_len = $ - value
