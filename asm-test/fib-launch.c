#include <stdio.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <time.h>
#include <stdlib.h>

typedef int (*bin_fptr)(int);

int main(int argc, char **argv) {
  const char *bin_path = argv[1];
  FILE *bin_f = fopen(bin_path, "r");
  if (bin_f == NULL) {
    return -1;
  }
  int bin_fd = fileno(bin_f);

  struct stat bin_st;
  fstat(bin_fd, &bin_st);
  int bin_len = bin_st.st_size;

  void *bin_ptr =
    mmap(NULL, bin_len, PROT_EXEC | PROT_READ, MAP_FILE | MAP_PRIVATE, bin_fd, 0);
  if (bin_ptr == MAP_FAILED) {
    return -1;
  }

  printf("binary is at %p\n", bin_ptr);

  uint64_t r;
  clock_t before = clock();
  for (int i = 0; i < 10000; i++ ) 
    r = ((bin_fptr) bin_ptr)(12);
  clock_t after = clock();
  clock_t span = after - before;

  double fspan = span;
  double fcps = CLOCKS_PER_SEC;
  double fspan_ms = fspan/fcps * 1000.0;
  
  printf("result is %llu (%gms)\n", r, fspan_ms);

  if ( munmap(bin_ptr, bin_len) == -1 ) {
    return -1;
  }
  fclose(bin_f);
  return 0;
}
