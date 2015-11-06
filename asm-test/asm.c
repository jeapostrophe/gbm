#include <stdlib.h>
#include <stdint.h>

uint32_t go(const uint8_t *rom, uint8_t *ram) {
  register uint32_t i0 asm ("r8");
  register uint32_t i1 asm ("r9");
  register void *l asm ("rax");
  register void *sp asm ("rsp");
  
 main:
  i0 = *rom;
  l = &&main_ret;
  goto fac;
 main_ret: return i0;
  
 fac:
  i1 = 1;
 fac_loop:
  if (i0 == 0) { i0 = i1; goto *l; }
  else { i1 = i1 * i0; i0 = i0 - 1; goto fac_loop; }
}

int main(int argc, char **argv) {
  uint8_t *rom = (uint8_t *)calloc(1, sizeof(uint8_t));
  uint8_t *ram = (uint8_t *)calloc(1, sizeof(uint8_t));
  rom[0] = atoi(argv[1]);
  return go(rom, ram);
}
