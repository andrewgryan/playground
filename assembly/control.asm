format ELF64 executable

include "x86_64.inc"

;; Experiment with control flow
SUCCESS = 0

segment readable executable
entry main
main:
    exit SUCCESS
