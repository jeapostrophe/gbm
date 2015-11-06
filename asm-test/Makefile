CFLAGS = -std=gnu99

.PHONY: fib
fib:
	as fib.s -o fib.o
	ld -lc -no_pie -macosx_version_min 10.8 fib.o -o fib
	gobjdump -x fib
	gobjcopy -O binary fib.o fib.bin
	./fib ; echo $$?
	cc -std=c99 fib-launch.c -o fib-launch
	ls -l fib*
	./fib-launch fib.bin

all: asm.c
	cc $(CFLAGS) -S -march=native -c $^
	cc $(CFLAGS) -march=native $^ -o asm
	./asm 5 ; echo $$?
	cat asm.s
