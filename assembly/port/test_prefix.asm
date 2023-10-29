format ELF64 executable

include "lib.inc"
include "route.asm"

;; MAIN
segment readable executable
entry main
main:
        ;; Route handling
        fn4 match_route, message, message_len, route_index, route_index_len
        cmp rax, 0
        jl .handle_index

        fn4 match_route, message, message_len, route_image, route_image_len
        cmp rax, 0
        jl .handle_image

.handle_index:
        print message_index, message_index_len
        jmp .done

.handle_image:
        print message_image, message_image_len
        jmp .done

.done:
        exit 0

;; DATA
segment readable writable

;; HTTP Message
message db "GET / HTTP/1.1"
message_len = $ - message

message_index db "Route: /", 10
message_index_len = $ - message_index

message_image db "Route: /image.jpg", 10
message_image_len = $ - message_image

;; Route
route_index db "GET / "
route_index_len = $ - route_index
route_image db "GET /image.jpg "
route_image_len = $ - route_image
