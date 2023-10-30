all: bin/main bin/test_atoi

bin/main: main.asm index.html about.html style.css route.asm
	fasm main.asm bin/main

bin/test_atoi: test_atoi.asm atoi.asm lib.inc
	fasm test_atoi.asm bin/test_atoi
