#include <stdint.h>

/* The machine is designed to run at a fixed rate of external
   interaction */
#define VM_HZ 120

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

/* The SNES has 4 layers, but this number is chosen to make all of the
   layer data on one 4k page. This number has to be smaller than the
   OpenGL texture count limit (given the default implementation
   technique.) */

#define VRAM_LAYER_LIMIT 256

/* These numbers are chosen so that you can have a number of static
   sprites (like for the background) equal to the number of tiles in
   all of Legend of Zelda (the entire map, above and below) and then a
   number of dynamic sprites equal to half that amount. The static
   limit works out to about 2,048 "screens" of content, assuming a
   256x256 screen with 8x8 tiles---of course, the tiles don't have to
   be 8x8, so there could really be much more. */

#define VRAM_STATIC_LIMIT (1<<16)
#define VRAM_DYNAMIC_LIMIT (1<<15)

/* The video system is based on a static set of image data and a small
   number of layers of sprites. The image data (the video ROM)
   contains a palette, a sprite database, and a sprite atlas. The VROM
   also specifies how many layers the program will use, up to some
   limit. Following this header, is the actual content in the order:
   palettes, sprite database, sprite atlas. */

/* A palette stores 8 colors from a 6-bit hardware palette (64
   colors). Each color is a single byte. If a high bit is set, then
   it is transparent in the palette. */
typedef struct {
  uint8_t colors[8];
} VROM_pal_t;

/* A sprite in the atlas is 8x8 and uses 3-bit indexed color (8
   colors), with one extra bit signaling transparency. This means that
   there are 32 bytes per sprite in the atlas. */
typedef struct {
  uint8_t color_indexed[32];
} VROM_sprite_atlas_t;

typedef struct {
  uint16_t pal_count;
  VROM_pal_t pal_db[(1<<16)];
  uint16_t spr_count;
  VROM_sprite_atlas_t spr_atlas[(1<<16)];
} VROM_max_t;

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
} VRAM_sprite_t_complicated;

typedef struct {
  uint16_t ul_x; uint16_t ul_y;
  uint16_t spr;
  uint8_t m; // = XX.xxYY.yy (xx = { 00 = 1, 01 = 2, 10 = 4, 11 = 8 })
  uint8_t pal; // 0-127 are 0-127 in pal_db, 128-255 are spr+[-64,+63] in pal_db
  // layer is implicit in location in sprite tree
} VRAM_sprite_t;

/* Each layer has a center and dimensions. The layers can be scaled
   and rotated. The entire layer can have a "mode7" effect applied to
   it. The entire layer can be alpha-blended and
   color-masked. Finally, the layer can have wrapping in the X or Y
   direction. */

// This could be shrunk to 16 bytes easily.

typedef struct {
  float cx; float cy;
  float hw; float hh;
  float mx; float my;
  float theta;
  float mode7coeff; float mode7hor; float mode7fov;
  float a; uint8_t r; uint8_t g; uint8_t b;
  uint8_t wrapx; uint8_t wrapy;
  uint8_t layer_padding[12];
} VRAM_layer_t_complicated;

typedef struct {
  uint16_t ul_x; uint16_t ul_y;
  uint16_t w; uint16_t h;

  uint16_t spr_span;

  uint16_t mode7_horizon; uint8_t mode7_fov;

  uint8_t mx; uint8_t my; // MMMMMM.mm where mm has same format as xx in sprite

  uint8_t flags;
  // 2 = { mode7-off, mode7-floor, mode7-ceiling, mode7-cylinder }
  // 1 = wrap-x
  // 1 = wrap-y
} VRAM_layer_t;

/* A screen is 256x256 and is represented in the 6-bit color space
   (where high bits are transparent.)
 */

typedef struct {
  uint8_t pixels[256][256];
} VRAM_screen_t;

/* The buffer (for the real screen) is in 32-bit space */

typedef struct {
  uint32_t pixels[256][256];
} VRAM_buffer_t;

typedef struct {
  VRAM_screen_t layers[16];
  VRAM_buffer_t buf;
} VRAM_screens_t;


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
   running at the machine Hz. */

#define AUDIO_CHANNELS 2
#define AUDIO_SAMPLE_RATE 44100
#define AUDIO_SAMPLES_PER_FRAME (AUDIO_SAMPLE_RATE/VM_HZ)
#define MAX_AUDIO_SAMPLES (1<<10) // This is ~17 seconds of audio

typedef int16_t AUDIO_sample_t;
typedef AUDIO_sample_t AUDIO_samples_t[AUDIO_CHANNELS];
typedef AUDIO_samples_t ARAM_t[AUDIO_SAMPLES_PER_FRAME];

// Alternatively I could assume that I have 2 Pulse, 2 Triangle, 3
// Noise, and 1 Sample buffer. This doesn't really seem to save that
// much space, because the sample buffer is so big. An alternative
// idea is to make audio and audio "ROM" and give a pointer to which
// sample is to be played. The problem with that is that it doesn't do
// the positional mixing I want (unless you build that in). Another
// problem with this is that it could put state into the machine
// between frames (the state of the oscillators) and if you didn't
// then certain waves couldn't be produced (For example: a pulse wave
// that overlaps the clock would be reset if you didn't save the pulse
// state between frames. Alternatively, this could be put into the
// program's RAM, which is pretty messy and ugly.)
//
// The other big problem with this is that the size of the audio ROM
// is huge for a reasonable amount of audio. On the other hand, these
// samples would just be in the normal ROM anyways.

// There are 7 bits of tone, because there are 88 piano keys.
typedef uint16_t
/* struct {
  unsigned tone:7;
  unsigned padding1:1;
  
  unsigned duty:2;
  unsigned volume:4;
  unsigned padding2:2;
  } */ SYNTH_pulse_t;

typedef uint8_t
/* struct {
  unsigned tone:7;
  unsigned on:1;
  } */ SYNTH_triangle_t;

typedef uint8_t
/* struct {
  unsigned period:4;
  unsigned volume:4;
  } */ SYNTH_noise_t;

typedef uint16_t /* struct {
 unsigned sample_no:10; // 2^10 different samples
 unsigned volume_scale:2; // 00 = quarter, 01 = half, 01 = normal, 11 = doubled
 unsigned bias:4; // (bias-7)/7 = position
} */ SYNTH_sample_t;

typedef struct {
  // Soprano + Alto
  SYNTH_pulse_t p[2];
  // Tenor + Bass
  SYNTH_triangle_t t[2];
  // A 3-piece drum kit (4-piece would have have 7)
  SYNTH_noise_t n[5];
  // 10 samples, chosen so the whole structure is 32 bytes
  SYNTH_sample_t s[10];
} SYNTH_t;

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
  printf("%25s = %8lu = %8.2f Ki = %5.2f Mi\n",
         label, limit, flimit/1024.0, (flimit/1024.0)/1024.0);
}

void show_sizes() {
  show_size("VM_HZ", VM_HZ);
  printf("\n");
  
  show_size("PROM limit", PROM_LIMIT);
  show_size("ROM limit", ROM_LIMIT);
  show_size("RAM limit", RAM_LIMIT);
  show_size("ROM_header_t", sizeof(ROM_header_t));
  show_size("ROM_max_t", sizeof(ROM_max_t));
  printf("\n");

  show_size("VROM limit", VROM_LIMIT);
  show_size("VROM_pal_t", sizeof(VROM_pal_t));
  show_size("VROM_sprite_atlas_t", sizeof(VROM_sprite_atlas_t));
  show_size("VROM_max_t", sizeof(VROM_max_t));
  printf("\n");

  show_size("VRAM_sprite_t", sizeof(VRAM_sprite_t));
  show_size("VRAM_LAYER_LIMIT", VRAM_LAYER_LIMIT);
  show_size("VRAM_layer_t", sizeof(VRAM_layer_t));
  show_size("VRAM_STATIC_LIMIT", VRAM_STATIC_LIMIT);
  show_size("VRAM_DYNAMIC_LIMIT", VRAM_DYNAMIC_LIMIT);
  show_size("VRAM_t", sizeof(VRAM_t));
  printf("\n");

  show_size("VRAM_screen_t", sizeof(VRAM_screen_t));
  show_size("VRAM_buffer_t", sizeof(VRAM_buffer_t));
  show_size("VRAM_screens_t", sizeof(VRAM_screens_t));
  printf("\n");

  show_size("AUDIO_SAMPLES_PER_FRAME", AUDIO_SAMPLES_PER_FRAME);
  show_size("AUDIO_sample_t", sizeof(AUDIO_sample_t));
  show_size("AUDIO_samples_t", sizeof(AUDIO_samples_t));
  show_size("ARAM_t", sizeof(ARAM_t));
  printf("\n");

  show_size("SYNTH_pulse_t", sizeof(SYNTH_pulse_t));
  show_size("SYNTH_triangle_t", sizeof(SYNTH_triangle_t));
  show_size("SYNTH_noise_t", sizeof(SYNTH_noise_t));
  show_size("SYNTH_sample_t", sizeof(SYNTH_sample_t));
  show_size("SYNTH_t", sizeof(SYNTH_t));
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
