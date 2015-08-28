CFLAGS=-Werror -pedantic -std=c99 -O3
TARGETS=video vm jit audio thread

all: $(TARGETS) size

video: video.c
	cc $(CFLAGS) `pkg-config --cflags glfw3` -o $@ $^ `pkg-config --static --libs glfw3`

jit: jit.c
	cc $(CFLAGS) -o $@ $^ -llightning

audio: audio.c
	cc $(CFLAGS) -o $@ $^ -lportaudio

%: %.c
	cc $(CFLAGS) -o $@ $^

size:
	wc *.c
	du -hac $(TARGETS)

clean:
	rm -f $(TARGETS)
