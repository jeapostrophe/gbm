CFLAGS = -std=gnu99

.PHONY: fib fib2 asm all fib3 fib4 fib5 vm

all: fib2 fib4 fib3 fib5 vm

fib3:
	time racket -t fib3.rkt

fib: fib-launch
	as fib.s -o fib.o
	ld -lc -no_pie -macosx_version_min 10.8 fib.o -o fib
	gobjdump -x fib
	gobjcopy -O binary fib.o fib.bin
	./fib ; echo $$?
	ls -l fib.bin fib
	time ./fib-launch fib.bin

fib-launch: fib-launch.c
	cc -std=c99 fib-launch.c -o fib-launch

fib2: fib-launch
	nasm -w+all -Ox -f bin fib2.s -o fib2.bin
	ls -l fib2.bin
	time ./fib-launch fib2.bin

fib4:
	cc -O3 -std=c99 fib4.c -o fib4
	ls -l fib4
	time ./fib4 12
	rm -f fib4

asm: asm.c
	cc $(CFLAGS) -S -march=native -c $^
	cc $(CFLAGS) -march=native $^ -o asm
	time ./asm 5 ; echo $$?
	cat asm.s

fib5:
	cc -std=c99 -O3 -fPIC -nostdlib fib5.c -shared -o fib5.so
#	otool -tv fib5.so
	ls -l fib5.so
	file fib5.so
	otool -L fib5.so

vm:
	cc -std=c99 -O3 vm.c -o vm
	time ./vm 12
	rm -f vm
