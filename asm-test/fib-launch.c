#include <stdio.h>
#include <sys/mman.h>
#include <sys/file.h>
#include <sys/stat.h>

typedef int (*bin_fptr)();

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

  void *bin_ptr = mmap(NULL, bin_len, PROT_EXEC | PROT_READ, MAP_FILE | MAP_PRIVATE, bin_fd, 0);
  if (bin_ptr == MAP_FAILED) {
    return -1;
  }

  int r = ((bin_fptr) bin_ptr)();
  printf("result is %d\n", r);

  if ( munmap(bin_ptr, bin_len) == -1 ) {
    return -1;
  }
  fclose(bin_f);
  return 0;
}
