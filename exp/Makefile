CFLAGS=-Werror -pedantic -std=c99 -O3 -mavx `pkg-config --cflags glfw3`
LDFLAGS=`pkg-config --static --libs glfw3` -llightning -lportaudio
TARGETS=gbm vm2

all: $(TARGETS) size vm2-test

vm2: vm2.c
	cc $(CFLAGS) -o $@ $^

vm2-test: vm2 bytecode-vm fac.bin
	#./vm2
	./bytecode-vm fac.bin 

fac.bin: fac.rkt bytecode.rkt
	raco test bytecode.rkt
	rk fac.rkt
	hexdump fac.bin

bytecode.h: bytecode.rkt
	rk bytecode.rkt > bytecode.h

bytecode-vm: bytecode-vm.c bytecode.h bytecode-vm2.o
	cc $(CFLAGS) bytecode-vm.c bytecode-vm2.o -o bytecode-vm
	cc $(CFLAGS) -S bytecode-vm.c -o bytecode-vm.s
	cc $(CFLAGS) -S -emit-llvm bytecode-vm.c -o bytecode-vm.ll

bytecode-vm2.s: bytecode-vm2.ll
	/usr/local/opt/llvm/bin/llc -O3 $^

bytecode-vm2.o: bytecode-vm2.s
	as $^ -o $@

gbm: gbm.o audio.o jit.o thread.o video.o vm.o
	cc $(CFLAGS) -o $@ $^ $(LDFLAGS)

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
