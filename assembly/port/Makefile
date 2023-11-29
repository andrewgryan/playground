all: bin/main bin/test_atoi bin/test_pattern_match bin/hello bin/test_brk

bin/main: main.asm index.html about.html style.css route.asm
	fasm main.asm bin/main

bin/test_atoi: test_atoi.asm atoi.asm lib.inc
	fasm test_atoi.asm bin/test_atoi

bin/test_pattern_match: test_pattern_match.asm lib.inc
	fasm test_pattern_match.asm bin/test_pattern_match

bin/hello: hello.asm
	fasm hello.asm bin/hello

bin/test_brk: test_brk.asm
	fasm test_brk.asm bin/test_brk
