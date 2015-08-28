CFLAGS=-Werror -pedantic -std=c99 -O3 `pkg-config --cflags glfw3`
LDFLAGS=`pkg-config --static --libs glfw3` -llightning -lportaudio
TARGETS=gbm

all: $(TARGETS) size

gbm: gbm.o audio.o jit.o thread.o video.o vm.o
	cc $(CFLAGS) -o $@ $^ $(LDFLAGS)

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS)
