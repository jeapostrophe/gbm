#include <stdint.h>

/* The machine is designed to run at a fixed rate of external
   interaction */
#define VM_HZ 60

/* The virtual machine loads object files that are like ELF
   files. They have a header and a body of sections. The sections are
   always in this order:

   PROM - the program
    ROM - the read-only data
   VROM - the video ROM to load in

   The PROM is in a special byte-code format that is interpreted by
   the virtual machine. 

   The ROM is a block of memory that will be loaded in and the program
   will have access to it.

   The VROM is a block of memory that will be directly given to the
   video system are load-time and the program will not have access to
   it. */

/* Each component is limited to a small amount, but they should be big
   enough to realize lots of interesting games, compared to what the
   sizes were on consoles from the 80s and 90s. */

// XXX revise these after writing some programs
#define PROM_LIMIT (1<<19)
#define ROM_LIMIT (1<<19)
#define VROM_LIMIT (1<<19)
#define RAM_LIMIT (1<<20)

typedef struct {
  /* The header has a format version number (for future proofing),
     then an identifier to give this program a code name used in
     database, then the various section sizes are given. The amount of
     RAM is given as well, to be used by the loader. Finally, the rest
     of the cache line is filled out with metadata. */

  uint8_t version;      // Presently only 0 is valid
  uint8_t id[63];       // 63 to fit in one cache line
  uint32_t prom_size;   // limited to PROM_LIMIT
  uint32_t rom_size;    // limited to ROM_LIMIT
  uint32_t vrom_size;   // limited to VROM_LIMIT
  uint32_t ram_size;    // limited to RAM_LIMIT
  uint8_t metadata[48]; // fill up the rest of the cache line with metadata
} ROM_header_t;

/* At present, the metadata has no values, but in the future it would
   store things like what platforms are supported, what kind of
   controller is needed, whether it supports co-op or versus, and how
   many players, etc */

typedef struct {
  ROM_header_t header;
  uint8_t body[];
} ROM_t;

typedef struct {
  ROM_header_t header;
  uint8_t body[PROM_LIMIT + ROM_LIMIT + VROM_LIMIT];
} ROM_max_t;

// Video code

#define VRAM_LAYER_LIMIT 64
#define VRAM_STATIC_LIMIT (1<<16)
#define VRAM_DYNAMIC_LIMIT (1<<15)

/* The video system is based on a static set of image data and a small
   number of layers of sprites. The image data (the video ROM)
   contains a palette, a sprite database, and a sprite atlas. The VROM
   also specifies how many layers the program will use, up to some
   limit. Following this header, is the actual content in the order:
   palettes, sprite database, sprite atlas. */

typedef struct {
  uint16_t spr_count;
  uint16_t pal_count;
  uint8_t layer_count; // limited to VRAM_LAYER_LIMIT
  uint8_t body[];
} VROM_t;

/* Sprites can be displaced, magnified, rotated, alpha-blended, color
   masked and placed on some layer. The particular sprite and palette
   are specified explicitly. */

typedef struct {
  float dx; float dy;
  float mx; float my;
  float theta; float a;
  uint16_t spr; uint16_t pal;
  uint8_t layer;
  uint8_t r; uint8_t g; uint8_t b;
} VRAM_sprite_t;

/* Each layer has a center and dimensions. The layers can be scaled
   and rotated. The entire layer can have a "mode7" effect applied to
   it. The entire layer can be alpha-blended and
   color-masked. Finally, the layer can have wrapping in the X or Y
   direction. */

typedef struct {
  float cx; float cy;
  float hw; float hh;
  float mx; float my;
  float theta;
  float mode7coeff; float mode7hor; float mode7fov;
  float a; uint8_t r; uint8_t g; uint8_t b;
  uint8_t wrapx; uint8_t wrapy;
  uint8_t layer_padding[12];
} VRAM_layer_t;

/* The VRAM state contains two separate sprite trees, one that is
   large and is intended to not change regularly, but can, and the
   other that is small and intended to change every frame. The program
   can also change the layer configuration every frame. */

typedef struct {
  VRAM_sprite_t  static_st[VRAM_STATIC_LIMIT];
  VRAM_sprite_t dynamic_st[VRAM_DYNAMIC_LIMIT];
  VRAM_layer_t    layer_st[VRAM_LAYER_LIMIT]; // XXX maybe not fix
} VRAM_t;

/* The audio system is a simple stereo buffer with 16-bit samples
   running at the machine Hz */

#define AUDIO_CHANNELS 2
#define AUDIO_SAMPLE_RATE 44100
#define AUDIO_SAMPLES_PER_FRAME (AUDIO_SAMPLE_RATE/VM_HZ)

typedef int16_t AUDIO_sample_t;
typedef AUDIO_sample_t AUDIO_samples_t[AUDIO_CHANNELS];
typedef AUDIO_samples_t ARAM_t[AUDIO_SAMPLES_PER_FRAME];

/* Runtime system 

   The runtime system has an idle/active cycle. First, we describe the
   idle state.

   We need to store in a "static" way, the PROM & ROM and any
   configuration information. I assume that we don't need to keep the
   VROM around, but maybe we should for having a stack of machine.

   We need to keep track of the program's state /between frames/: the
   RAM. 

   We need to keep track of the dynamic "output" of the program, such
   as the VRAM and ARAM. We include a debug output as well.

   When the program starts running, it is going to get some "input",
   such as the input state, the frame count, and the display device.

   For network synchronization, we have to store a history of
   the RAM and input (including from the network) */

#define INPUT_LIMIT 8

typedef enum { INPUT_LOCAL, INPUT_REMOTE, INPUT_GHOST, INPUT_SPECTATOR } input_type_e;
typedef uint8_t input_type_t;

typedef enum { INPUT_GAMEPAD, INPUT_KEYMOUSE, INPUT_TOUCH } input_kind_e;
typedef uint8_t input_kind_t;

typedef struct {
  unsigned dpad_l:1;
  unsigned dpad_r:1;
  unsigned dpad_u:1;
  unsigned dpad_d:1;
  
  unsigned l:1;
  unsigned r:1;

  unsigned start:1;
  unsigned select:1;

  unsigned a:1;
  unsigned b:1;
  unsigned x:1;
  unsigned y:1;
} input_gamepad_t;

typedef uint16_t input_loc_t;

typedef struct {
  input_loc_t mx;
  input_loc_t my;
  uint8_t key;
  unsigned mouse_down:1;
  unsigned key_down:1;
  unsigned key_shift:1;
  unsigned key_m1:1;
  unsigned key_m2:1;
  unsigned key_m3:1;
} input_keymouse_t;

typedef struct {
  input_loc_t t1x;
  input_loc_t t1y;
  input_loc_t t2x;
  input_loc_t t2y;
  unsigned t1d:1;
  unsigned t2d:1;
} input_touch_t;

typedef struct {
  union {
    input_gamepad_t g;
    input_keymouse_t km;
    input_touch_t t;
  } i[INPUT_LIMIT];
  input_type_t type[INPUT_LIMIT];
  input_kind_t kind[INPUT_LIMIT];
} input_max_t;

// XXX choose this well
#define HISTORY_LIMIT 16

typedef struct {
  uint8_t RAM[RAM_LIMIT];
  input_max_t input;
} history_entry_max_t;

typedef struct {
  history_entry_max_t entries[HISTORY_LIMIT];
  uint8_t start;
  uint8_t count;
} history_max_t;

typedef enum { DISPLAY_CONSOLE, DISPLAY_PHONE_PORTRAIT, DISPLAY_PHONE_LANDSCAPE, DISPLAY_TABLET } display_e;
typedef uint8_t display_t;

typedef uint64_t frame_t;

typedef struct {
  uint8_t PROM[PROM_LIMIT];
  uint8_t ROM[ROM_LIMIT];
  uint8_t VROM[VROM_LIMIT];
  
  uint8_t RAM[RAM_LIMIT];
  
  VRAM_t VRAM;
  ARAM_t ARAM;
  char debug[141];

  frame_t frame;
  display_t display;
  input_max_t input;

  history_max_t history;
} VM_idle_state_max_t;

/* Now, we describe the active state.

   While a program is running, i.e. /inside a frame/, we also need to
   know more stuff: the register contents, the stack, the program
   counter, etc. These are actually left implicit from the perspective
   of the external interface, because we just jump to some machine
   code. */

/* A little test program to show off some statistics */

#include <stdio.h>

void show_size(const char *label, size_t limit) {
  double flimit = limit;
  printf("%21s = %8lu = %8.2f Ki = %5.2f Mi\n",
         label, limit, flimit/1024.0, (flimit/1024.0)/1024.0);
}

void show_sizes() {
  show_size("PROM limit", PROM_LIMIT);
  show_size("ROM limit", ROM_LIMIT);
  show_size("VROM limit", VROM_LIMIT);
  show_size("RAM limit", RAM_LIMIT);
  show_size("ROM_header_t", sizeof(ROM_header_t));
  show_size("ROM_max_t", sizeof(ROM_max_t));
  printf("\n");

  show_size("VRAM_sprite_t", sizeof(VRAM_sprite_t));
  show_size("VRAM_LAYER_LIMIT", VRAM_LAYER_LIMIT);
  show_size("VRAM_layer_t", sizeof(VRAM_layer_t));
  show_size("VRAM_STATIC_LIMIT", VRAM_STATIC_LIMIT);
  show_size("VRAM_DYNAMIC_LIMIT", VRAM_DYNAMIC_LIMIT);
  show_size("VRAM_t", sizeof(VRAM_t));
  printf("\n");

  show_size("AUDIO_sample_t", sizeof(AUDIO_sample_t));
  show_size("AUDIO_samples_t", sizeof(AUDIO_samples_t));
  show_size("ARAM_t", sizeof(ARAM_t));
  printf("\n");

  show_size("INPUT_LIMIT", INPUT_LIMIT);
  show_size("input_type_t", sizeof(input_type_t));
  show_size("input_kind_t", sizeof(input_kind_t));
  show_size("input_gamepad_t", sizeof(input_gamepad_t));
  show_size("input_keymouse_t", sizeof(input_keymouse_t));
  show_size("input_touch_t", sizeof(input_touch_t));
  show_size("input_max_t", sizeof(input_max_t));
  printf("\n");

  show_size("history_entry_max_t", sizeof(history_entry_max_t));
  show_size("history_max_t", sizeof(history_max_t));
  printf("\n");

  show_size("display_t", sizeof(display_t));
  show_size("VM_idle_state_max_t", sizeof(VM_idle_state_max_t));
  printf("\n");
  
  return;
}

int main (int argc, char **argv) {
  show_sizes();
  return 0;
}
