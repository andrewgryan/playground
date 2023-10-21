format ELF64 executable
;; Convert PORT number from decimal to network endian decimal
;;
;; Algorithm
;; 1. Convert from decimal to hexadecimal
;; 2. Reverse the bytes
;; 3. Convert back into decimal
;;
include "lib.inc"

segment readable executable
entry main
main:
   ;; Print Hello, World!
   print buf, len

   ;; Exit with return code
   exit 42
   

segment readable writable
buf db "Hello, World!", 10
len = $ - buf
