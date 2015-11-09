CFLAGS = -std=gnu99

.PHONY: fib fib2 asm all

all: fib2 fib3

fib3:
	time racket -t fib3.rkt

fib:
	as fib.s -o fib.o
	ld -lc -no_pie -macosx_version_min 10.8 fib.o -o fib
	gobjdump -x fib
	gobjcopy -O binary fib.o fib.bin
	./fib ; echo $$?
	cc -std=c99 fib-launch.c -o fib-launch
	ls -l fib*
	time ./fib-launch fib.bin

fib2:
	nasm -w+all -Ox -f bin fib2.s -o fib2.bin
	cc -std=c99 fib-launch.c -o fib-launch
	ls -l fib*
	time ./fib-launch fib2.bin

asm: asm.c
	cc $(CFLAGS) -S -march=native -c $^
	cc $(CFLAGS) -march=native $^ -o asm
	./asm 5 ; echo $$?
	cat asm.s
