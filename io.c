#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

#include <fcntl.h>
#include <unistd.h>
#include <uchar.h>

val_t read_byte(void)
{
  char c = getc(in);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int(c);  
}

val_t peek_byte(void)
{
  char c = getc(in);
  ungetc(c, in);
  return (c == EOF) ? val_wrap_eof() : val_wrap_int(c);
  
}

val_t write_byte(val_t c)
{
  putc((char) val_unwrap_int(c), out);
  return val_wrap_void();
}

// ****** Spite ********* 
// TODO lists:
// - Free stuff after malloc
// - Close files when done
// - Error handling when stuff goes wrong
// - Implement sockets (switch between the two)


// file/sock -> bool
val_t spite_close(val_t fs) {
  //TODO: Implement some switch for sockets, this is for files
  int64_t f = val_unwrap_file(fs);
  if (close(f) == 0) 
    return val_true;
  error_handler();

  // Should never happen!
  return val_false;
}

// string, char -> file
val_t spite_open(val_t path, val_t flag) {
  //TODO: Implement some switch for sockets, this is for files
  int64_t fd;
  int oflags;
  val_str_t* p = val_unwrap_str(path);
  val_char_t f = val_unwrap_char(flag);

  // setup oflags
  switch (f) {
    case 'r':
      oflags = O_RDONLY;
      break;
    case 'w':
      // Files will only be created with the w mode
      oflags = O_WRONLY | O_CREAT;
      break;
    case 'a':
      oflags = O_WRONLY | O_APPEND;
      break;
    default:
      error_handler();
  }

  p->codepoints[p->len] = '\0';
  puts("Here is the file:");
  puts((const char32_t*) p->codepoints);

  if ((fd = open((const char*) p->codepoints, oflags)) == -1) {
    perror("spite_open");
    exit(-1);
  }

  //TODO: do error handling!!!
  return val_wrap_file(fd);
}

// int -> string
val_t spite_read_stdin(val_t num_chars) {
  int64_t n = val_unwrap_int(num_chars);
  // TODO: Free this at some point???
  val_str_t *str = malloc(sizeof(val_str_t));
  str->len = n;

  if(fgets((char*)(str->codepoints), n, stdin) == NULL)
    error_handler();

  str->codepoints[n + 1] = '\0';

  return val_wrap_str(str);
}

// file/sock, int -> string
val_t spite_read(val_t fs, val_t num_chars) {
  int64_t n = val_unwrap_int(num_chars);
  int fd = val_unwrap_file(fs);
  int64_t bytes_read = 0;
  // TODO: Free this at some point???
  val_str_t *str = malloc(sizeof(val_str_t));
  str->len = n;

  if((bytes_read = read(fd, (char*)(str->codepoints), n)) == -1)
    error_handler();

  // TODO: Does this work or...?
  str->codepoints[bytes_read] = '\0';

  return val_wrap_str(str);
}

// string -> void
val_t spite_write_stdout(val_t string) {
  val_str_t *str = val_unwrap_str(string);

  uint64_t n = str->len;
  val_char_t *codepoints = str->codepoints;

  //TODO: Replace printf with something better
  printf("%.*s", (int) n, (char*)codepoints);

  return val_wrap_void();
}

// file/sock, string -> void
val_t spite_write(val_t fs, val_t string) {
  int fd = val_unwrap_file(fs);
  val_str_t *str = val_unwrap_str(string);

  uint64_t n = str->len;
  val_char_t *codepoints = str->codepoints;

  int64_t bytes_wrote = 0;

  //TODO: Replace printf with something better
  if((bytes_wrote = write(fd, (char*)(str->codepoints), n)) == -1)
    error_handler();

  // free(str); TODO: ???
  return val_wrap_void();
}
