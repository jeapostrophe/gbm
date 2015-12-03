#include <stdio.h>
#include <time.h>
#include <stdlib.h>

int f_while(int x) {
  int r = 1;
  while ( x > 0 ) {
    r = r * x;
    x = x - 1;
  }
  return r;
}

int f_goto(int x) {
  int r = 1;
 loop:
  if ( x == 0 ) goto end;
    r = r * x;
    x = x - 1;
    goto loop;
 end:
  return r;
}

int f_asmlike(int x) {
  int r = 1;
 loop:
  if ( x == 0 ) goto end;
    r *= x;
    x -= 1;
    goto loop;
 end:
    x = r;
  return x;
}

void test(const char* label, int (*f)(int), int n) {
  uint64_t r;
  clock_t before = clock();
  for (int i = 0; i < 10000; i++ ) 
    r = f(n);
  clock_t after = clock();
  clock_t span = after - before;

  double fspan = span;
  double fcps = CLOCKS_PER_SEC;
  double fspan_ms = fspan/fcps * 1000.0;

  printf("%s: %llu (%gms)\n", label, r, fspan_ms);
}


int main(int argc, char **argv) {
  int n = atoi(argv[1]);
  test("f_while", &f_while, n);
  test("f_goto", &f_goto, n);
  test("f_asmlike", &f_asmlike, n);
  return 0;
}
