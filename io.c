#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

#include <fcntl.h>
#include <unistd.h>
#include <wchar.h>
#include <string.h>

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
// - EOF handling
// - Implement sockets (switch between the two)


// Spite string to C string Given a spite string (UTF-32) (i think), converts
// it to a multibyte character string. Will malloc more data than it needs, so
// it should definitely be freed. This function assumes that wchar is 32 bits.
char *stoc_str(val_char_t *s, uint64_t len) {
  // Dirty solution here
  if (sizeof(wchar_t) != 4) exit(2);

  // Add a null byte on the end of our UTF-32 string
  wchar_t *wcs = malloc((len+1) * sizeof(wchar_t));
  wcsncpy(wcs, s, len);
  wcs[len] = L'\0';
  
  // Safety allocation, assuming every UTF-32 character will need 4 bytes
  int buf_size = ((len * 4) + 1) * sizeof(char); // Extra for NUL byte
  char *mb = malloc(buf_size);

  int bytes = wcstombs(mb, wcs, buf_size);

  free(wcs);
  return mb;
}


// C string to spite string. This function assumes that wchar is 32 bits.
val_char_t *ctos_str(char *s, uint64_t len) {
  // Dirty solution here
  if (sizeof(wchar_t) != 4) exit(2);

  int wc_buf_size = (len * 4) * sizeof(wchar_t); // NUL byte extra bit not necessary
  wchar_t *wc = malloc(wc_buf_size);

  size_t bytes = mbstowcs(wc, s, len);

  return (val_char_t*) wc;
}

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
  mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
  val_str_t* p = val_unwrap_str(path);
  val_char_t f = val_unwrap_char(flag);

  //TODO: Unallocate this buffer later
  //This assumes that each codepoint takes one byte, 

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

  //p->codepoints[p->len] = '\0';
  // Convert to C string
  char *filename = stoc_str(p->codepoints, p->len);

  if ((fd = open(filename, oflags, mode)) == -1) {
    perror("spite_open");
    exit(-1);
  }

  free(filename);
  return val_wrap_file(fd);
}

// int -> string
val_t spite_read_stdin(val_t num_chars) {
  int64_t n = val_unwrap_int(num_chars);
  char cstr[n + 1]; // +1 to include NUL byte

  if(fgets(cstr, n + 1, stdin) == NULL) {
    perror("spite_read_stdin");
    exit(-1);
  }

  // Concert string to spite string
  wchar_t *sstr = ctos_str(cstr, n);

  // Allocate space for our struct
  val_str_t *str = malloc(sizeof(uint64_t) + (n*sizeof(wchar_t)));
  str->len = strlen(cstr);
  wmemcpy(str->codepoints, sstr, n);

  free(sstr);
  return val_wrap_str(str);
}

// file/sock, int -> string
val_t spite_read(val_t fs, val_t num_chars) {
  int64_t n = val_unwrap_int(num_chars);
  int fd = val_unwrap_file(fs);
  int64_t bytes_read = 0;
  char cstr[n];
  val_char_t *codepoints;

  if((bytes_read = read(fd, cstr, n)) == -1) {
    perror("spite_read");
    exit(-1);
  } 

  wchar_t *sstr = ctos_str(cstr, n);
  val_str_t *str = malloc(sizeof(uint64_t) + (n*sizeof(wchar_t)));
  wmemcpy(str->codepoints, sstr, n);
  str->len = bytes_read;

  free(sstr);
  return val_wrap_str(str);
}

// string -> void
val_t spite_write_stdout(val_t string) {
  val_str_t *str = val_unwrap_str(string);
  char *c_str = stoc_str(str->codepoints, str->len);

  fputs(c_str, stdout);

  free(c_str);
  return val_wrap_void();
}

// file/sock, string -> void
val_t spite_write(val_t fs, val_t string) {
  int fd = val_unwrap_file(fs);
  val_str_t *str = val_unwrap_str(string);
  char *c_str = stoc_str(str->codepoints, str->len);
  //TODO: do we need to trim off NUL byte here?

  if(write(fd, c_str, strlen(c_str)) == -1)
    error_handler();

  free(c_str);
  return val_wrap_void();
}
