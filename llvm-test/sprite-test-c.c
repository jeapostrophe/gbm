typedef struct {
  float dx; float dy;
  float mx; float my;
  float theta; float a;
  short spr; short pal;
  char layer;
  char r; char g; char b;
} sprite;

sprite spr_static[1024];
sprite spr_dynamic[1024];

void init(int i) {
  sprite st = { 5.0, 6.0, 1.0, 2.0, 3.0, 4.0, 25, 15, 1, 2, 3, 4 };
  spr_static[i] = st;
}

void copy(int i, int j) {
  spr_dynamic[i] = spr_static[j];
}

void copy_update(int i, int j) {
  sprite x = spr_static[j];
  x.dx += 1.0;
  spr_dynamic[i] = x;
}
