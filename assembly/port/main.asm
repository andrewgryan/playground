format ELF64 executable
;; Convert PORT number from decimal to network endian decimal
;;
;; Algorithm
;; ---------
;; 1. Reverse the bytes
;;
include "lib.inc"

CARRIAGE_RETURN = 10

segment readable executable
entry main
main:
   ;; Put 1 in rdx buffer and bit shift left
   mov rdx, 1
   shl edx, 8

   ;; Integer to ASCII
   fn itoa, buf, rdx

   ;; Add carriage return
   fn append, buf, CARRIAGE_RETURN
   
   ;; Print buffer
   print buf, len

   ;; Exit with return code
   exit 0
   
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

segment readable writable
buf db "    ", CARRIAGE_RETURN
len = $ - buf
