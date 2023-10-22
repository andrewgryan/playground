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
   ;; Big-endian number
   mov rdi, 8080
   call endian

   ;; Integer to ASCII
   fn itoa, buf, rax

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

segment readable writable
buf db "      ", CARRIAGE_RETURN
len = $ - buf
