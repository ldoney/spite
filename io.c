#include <stdio.h>
#include <inttypes.h>
#include "types.h"
#include "values.h"
#include "runtime.h"

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
//

// file/sock -> bool
val_t close(val_t fs) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}

// string, char -> file
val_t open(val_t path, val_t flag) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}

// int -> string
val_t read_stdin(val_t num_chars) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}

// file/sock, int -> string
val_t read(val_t fs, val_t num_chars) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}

// string -> void
val_t write_stdout(val_t string) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}

// file/sock, string -> void
val_t write(val_t fs, val_t string) {
  printf("Unimplemented!\n");
  error_handler(1);
  return val_wrap_void();
}
