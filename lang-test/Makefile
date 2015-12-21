# http://ptspts.blogspot.com/2013/12/how-to-make-smaller-c-and-c-binaries.html
CFLAGS=-Wall -Weverything -Wno-unused-parameter -Wno-unused-function -Werror -pedantic -std=c99 -O3 -march=native -fno-stack-protector -ffunction-sections -fdata-sections -fno-unwind-tables -fno-asynchronous-unwind-tables -fno-math-errno -fmerge-all-constants -fno-ident -fPIE -fPIC `pkg-config --cflags glfw3`
LDFLAGS=-dead_strip `pkg-config --libs glfw3`
all: fac-test gbm-test

gbm-test: gbm
	./$^

gbm: gbm.o
	cc $^ $(LDFLAGS) -o $@

gbm.o: gbm.c
	cc $(CFLAGS) $^ -c -o $@

gbm.c: gbm.rkt r.rkt
	rk gbm.rkt > $@ || (rm -f $@; false)

fac-test: fac
	./$^

fac: fac.o
	cc $(LDFLAGS) $^ -o $@

fac.o: fac.c
	cc $(CFLAGS) $^ -c -o $@

fac.c: fac.rkt r.rkt
	rk fac.rkt > $@ || (rm -f $@; false)

clean:
	rm -f fac.c fac.o gbm.c gbm.o
