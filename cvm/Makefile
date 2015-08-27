CFLAGS=-Werror -pedantic -std=c99

all: cvm size

cvm: cvm.c
	cc $(CFLAGS) `pkg-config --cflags glfw3` -o $@ $^ `pkg-config --static --libs glfw3`
	strip -u -r cvm

size:
	du -hac

clean:
	rm -f cvm
