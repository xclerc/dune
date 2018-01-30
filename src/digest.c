#include "xxhash.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/unixsupport.h>
#include <sys/mman.h>

CAMLprim uint64_t jbuilder_digest_file(value fn)
{
  int fd, error;
  void *ptr;
  off_t len;
  uint64_t result;

  fd = open(String_val(fn), O_RDWR);
  if (fd < 0) uerror("open", fn);

  len = lseek(fd, 0, SEEK_END);
  lseek(fd, 0, SEEK_SET);

  if (len == 0 || len == (off_t)-1)
    result = XXH64("", 0, 0);
  else {
    ptr = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (ptr == MAP_FAILED) {
      error = errno;
      close(fd);
      unix_error(error, "mmap", fn);
    }
    result = XXH64(ptr, len, 0);
  }

  munmap(ptr, len);
  close(fd);

  return result;
}

CAMLprim value jbuilder_digest_file_byte(value fn)
{
  return caml_copy_int64(jbuilder_digest_file(fn));
}

CAMLprim uint64_t jbuilder_digest_string(value fn)
{
  return XXH64(String_val(fn), caml_string_length(fn), 0);
}

CAMLprim value jbuilder_digest_string_byte(value fn)
{
  return caml_copy_int64(jbuilder_digest_string(fn));
}
