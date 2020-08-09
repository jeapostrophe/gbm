CFLAGS=-std=c99 -Os -fno-stack-protector -fno-stack-check -fomit-frame-pointer -fno-asynchronous-unwind-tables -ffunction-sections 

dotest:
	stack build --fast
	stack exec gbmc | tee ex.c
	time cc $(CFLAGS) -c -o ex.o ex.c
	time cc $(CFLAGS) -o test test.c ex.o
	wc ex.* test
	time ./test
