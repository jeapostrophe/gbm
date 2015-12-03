#include <stdint.h>

uint64_t f_while(uint64_t x) {
  uint64_t r = 1;
  while ( x > 0 ) {
    r = r * x;
    x = x - 1;
  }
  return r;
}
