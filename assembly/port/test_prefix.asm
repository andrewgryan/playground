format ELF64 executable

include "lib.inc"
include "route.asm"

;; MAIN
segment readable executable
entry main
main:
        fn4 match_route, message, message_len, route_index, route_index_len
        cmp rax, 0
        jl .not_ok

.ok:
        print ok, ok_len
        jmp .done

.not_ok:
        print not_ok, not_ok_len
        jmp .done

.done:
        exit 0

;; DATA
segment readable writable

;; HTTP Message
message db "GET / HTTP/1.1"
message_len = $ - message

ok db "OK", 10
ok_len = $ - ok
not_ok db "NOT OK", 10
not_ok_len = $ - not_ok

;; Route
route_index db "GET / "
route_index_len = $ - route_index
route_image db "GET /hello.jpg "
route_image_len = $ - route_image
