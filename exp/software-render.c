#include <stdio.h>
#include <time.h>

#define H 256
#define W 256
#define SPRS 256
#define SPR 512

typedef struct {
  char x;
  char y;
  char spr;
} sprite_t;

int main () {
  char bm[W][H] = { {0} };
  char sm[SPRS] = { 0 };
  sprite_t spr[SPR] = { {0} };

  FILE *rand = fopen("/dev/random", "r");
  if (!rand) return 1;
  
  fread(&sm, sizeof(char), SPRS, rand);
  fread(&spr, sizeof(sprite_t), SPR, rand);
  
  fclose(rand);

  clock_t before = clock();
  for (int i = 0; i < SPR; i++) {
    // Calculate sprite position
    char ix = spr[i].x;
    char iy = spr[i].y;
    char d = sm[spr[i].spr];
    // Blit sprite onto display buffer
    for (int dx = 0; dx < 8; dx++ ) {
      for (int dy = 0; dy < 8; dy++ ) {
        bm[ix+dx][iy+dy] = d;
      }
    }
  }
  clock_t after = clock();

  clock_t span = after - before;

  double fspan = span;
  double fcps = CLOCKS_PER_SEC;
  double fspan_ms = fspan/fcps * 1000.0;

  printf("(%fms)\n", fspan_ms);

}
