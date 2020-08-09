dotest:
	stack build --fast
	stack exec gbmc | tee ex.c
	time cc -c -o ex.o ex.c
	time cc -o test test.c ex.o
	wc ex.* test
	time ./test
