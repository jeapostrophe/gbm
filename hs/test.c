#include <stdio.h>
#include <stdint.h>

typedef struct {
  uint32_t n;
  uint32_t ans;
} arg_t;

extern void test(arg_t *);

int main() {
  arg_t arg = {};
  for ( uint32_t n = 0; n < 10; n++ ) {
    arg.n = n;
    arg.ans = 0;
    test(&arg);
    printf("fib(%d) = %d\n", arg.n, arg.ans); }
  return 0; }
