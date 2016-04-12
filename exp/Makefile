libsoundio=/usr/local/Cellar/libsoundio/1.0.3
CFLAGS=-Werror -std=gnu99 -O3 -mbmi -mavx -mavx2 -fvectorize -funroll-loops -fslp-vectorize -fslp-vectorize-aggressive -march=native `pkg-config --cflags glfw3` `pkg-config --cflags portaudio-2.0` -I$(libsoundio)/include
LDFLAGS=`pkg-config --static --libs glfw3` `pkg-config --static --libs portaudio-2.0` -L$(libsoundio)/lib -lsoundio
TARGETS=gbm vm2

all: $(TARGETS) size vm2-test

vm2: vm2.c
	cc $(CFLAGS) -o $@ $^

vm2-test: vm2 bytecode-vm fac.bin
	./bytecode-vm fac.bin
	./bytecode-vm 10000 12
	./vm2

fac.bin: fac.rkt bytecode.rkt
	raco test bytecode.rkt
	rk fac.rkt
	hexdump fac.bin

bytecode.h: bytecode.rkt
	rk bytecode.rkt > bytecode.h

bytecode-vm: bytecode-vm.c bytecode.h
	cc $(CFLAGS) bytecode-vm.c -o bytecode-vm
	cc $(CFLAGS) -S bytecode-vm.c -o bytecode-vm.s
	cc $(CFLAGS) -S -emit-llvm bytecode-vm.c -o bytecode-vm.ll

bytecode-vm2.s: bytecode-vm2.ll
	/usr/local/opt/llvm/bin/llc -O3 $^

bytecode-vm2.o: bytecode-vm2.s
	as $^ -o $@

bytecode-vm4.o: bytecode-vm4.s
	nasm -w+all -Ox -f macho64 $^ -o $@

gbm: gbm.o audio.o audio2.o thread.o video.o vm.o
	cc $(CFLAGS) -o $@ $^ $(LDFLAGS)

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
