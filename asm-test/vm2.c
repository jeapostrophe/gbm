#include <stdint.h>

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
