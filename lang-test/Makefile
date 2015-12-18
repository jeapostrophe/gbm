# http://ptspts.blogspot.com/2013/12/how-to-make-smaller-c-and-c-binaries.html
CFLAGS=-Wall -Weverything -Wno-unused-function -Werror -pedantic -std=c99 -O3 -march=native -fno-stack-protector -ffunction-sections -fdata-sections -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-math-errno -fmerge-all-constants -fno-ident -fPIE -fPIC
LDFLAGS=-dead_strip
all: size

size: fac.o fac
	ls -l $^

fac: fac.o
	cc $(LDFLAGS) $^ -o $@

fac.o: fac.c
	cc $(CFLAGS) $^ -c -o $@

fac.c: fac.rkt r.rkt
	rk fac.rkt > $@ || (rm -f $@; false)
	cat $@

clean:
	rm -f fac.c fac.o
