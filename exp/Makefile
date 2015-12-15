CFLAGS=-Werror -pedantic -std=c99 -O3 `pkg-config --cflags glfw3`
LDFLAGS=`pkg-config --static --libs glfw3` -llightning -lportaudio
TARGETS=gbm vm2

all: $(TARGETS) size vm2-test

vm2: vm2.c
	cc $(CFLAGS) -o $@ $^

vm2-test: vm2 bytecode-vm
	./vm2
	./bytecode-vm

bytecode.h: bytecode.rkt
	rk bytecode.rkt > bytecode.h

bytecode-vm: bytecode-vm.c bytecode.h
	cc $(CFLAGS) bytecode-vm.c -o bytecode-vm
	cc $(CFLAGS) -S bytecode-vm.c -o bytecode-vm.s

gbm: gbm.o audio.o jit.o thread.o video.o vm.o
	cc $(CFLAGS) -o $@ $^ $(LDFLAGS)

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
