CFLAGS=-Werror -pedantic -std=c99
TARGETS=video vm

all: $(TARGETS) size

video: video.c
	cc $(CFLAGS) `pkg-config --cflags glfw3` -o $@ $^ `pkg-config --static --libs glfw3`

vm: vm.c
	cc $(CLFAGS) -o $@ $^

size:
	du -hac

clean:
	rm -f $(TARGETS)
