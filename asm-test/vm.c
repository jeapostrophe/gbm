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

uint64_t beit(uint64_t ria0) {
  uint8_t pc = 0;
  uint8_t cmp_flag = 0;
  uint64_t r9 = 0;
  uint64_t r0 = 0;

 i0: r9 = 1;
 i1: cmp_flag = (ria0 == 0);
 i2: if (cmp_flag) { goto i6; };
 i3: r9 *= ria0;
 i4: ria0--;
 i5: goto i1;
 i6: r0 = r9;
 i7: return r0;
}

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

uint64_t f_bytecode(uint64_t n) {
  return runnit(program, n);
}

uint64_t f_c(uint64_t n) {
  return beit(n);
}

void test(const char* label, uint64_t (*f)(uint64_t), int n) {
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
  printf("inst_t = %lu\n", sizeof(inst_t));
  uint64_t n = atoi(argv[1]);

  test("f_bytecode", &f_bytecode, n);
  test("f_c", &f_c, n);

  return 0;
}
