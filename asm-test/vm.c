#include <stdint.h>

typedef uint8_t inst_t;

inst_t program[] = {
  0, 1, 2, 3, 4, 5, 6, 7
};

uint64_t runnit(const inst_t program[], uint64_t ria0) {
  uint8_t pc = 0;
  uint64_t r9 = 0;
  uint64_t r0 = 0;
  uint8_t cmp_flag = 0;
  while (1) {
    switch (program[pc]) {
    case 0: r9 = 1; pc++; break;
    case 1: cmp_flag = (ria0 == 0); pc++; break;
    case 2: if (cmp_flag) { pc = 6; } else { pc++; }; break;
    case 3: r9 *= ria0; pc++; break;
    case 4: ria0--; pc++; break;
    case 5: pc = 1; break;
    case 6: r0 = r9; pc++; break;
    case 7: return r0;
    }
  }
}

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char **argv) {
  printf("inst_t = %lu\n", sizeof(inst_t));
  uint64_t n = atoi(argv[1]);

  clock_t before = clock();
  uint64_t r = runnit(program, n);
  clock_t after = clock();
  clock_t span = after - before;

  double fspan = span;
  double fcps = CLOCKS_PER_SEC;
  double fspan_ms = fspan/fcps * 1000.0;

  printf("%llu (%lu clocks @ %d = %gms)\n", r, span, CLOCKS_PER_SEC, fspan_ms);

}
