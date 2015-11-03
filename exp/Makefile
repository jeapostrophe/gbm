CFLAGS=-Werror -pedantic -std=c99 -O3 `pkg-config --cflags glfw3`
LDFLAGS=`pkg-config --static --libs glfw3` -llightning -lportaudio
TARGETS=gbm vm2

all: $(TARGETS) size vm2-test

vm2: vm2.c
	cc $(CFLAGS) -o $@ $^

vm2-test:
	./vm2

gbm: gbm.o audio.o jit.o thread.o video.o vm.o
	cc $(CFLAGS) -o $@ $^ $(LDFLAGS)

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS) *.o
