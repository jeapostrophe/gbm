#include <string.h>

extern int audio_main();
extern int audio2_main();
// extern int jit_main();
extern int thread_main();
extern int video_main();
extern int vm_main();

int main(int argc, char **argv) {
  if ( strcmp(argv[1], "audio") == 0) {
    audio_main();
  } else if ( strcmp(argv[1], "audio2") == 0) {
    audio2_main();
  } else /*if ( strcmp(argv[1], "jit") == 0) {
    jit_main();
    } else */ if ( strcmp(argv[1], "thread") == 0) {
    thread_main();
  } else if ( strcmp(argv[1], "video") == 0) {
    video_main();
  } else if ( strcmp(argv[1], "vm") == 0) {
    vm_main();
  }
  return 0;
}
