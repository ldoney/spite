#ifndef TYPES_H
#define TYPES_H

/*
  Bit layout of values

  Values are either:
  - Immediates: end in #b000
  - Pointers

  Immediates are either
  - Integers:   end in  #b0 000
  - Characters: end in #b01 000

  - File:    end in #b01 11 000
  - Socket:  end in #b11 11 000

  - True:              #b11 000
  - False:           #b1 11 000
  - Eof:            #b10 11 000
  - Void:           #b11 11 000
  - Empty:         #b100 11 000

*/
#define imm_shift        3
#define ptr_type_mask    ((1 << imm_shift) - 1)
#define box_type_tag     1
#define cons_type_tag    2
#define vect_type_tag    3
#define str_type_tag     4
#define proc_type_tag    5
#define int_shift        (1 + imm_shift) //4
#define int_type_mask    ((1 << int_shift) - 1)
#define int_type_tag     (0 << (int_shift - 1))
#define nonint_type_tag  (1 << (int_shift - 1))
#define char_shift       (int_shift + 1)  //5
#define char_type_mask   ((1 << char_shift) - 1)
#define char_type_tag    ((0 << (char_shift - 1)) | nonint_type_tag)
#define nonchar_type_tag ((1 << (char_shift - 1)) | nonint_type_tag)


#define file_shift (char_shift+2) //7
#define file_type_mask ((1<< file_shift) - 1) 
#define file_type_tag ((0 << (file_shift - 1)) | 56)

#define socket_shift (char_shift+2) //7
#define socket_type_mask ((1 << socket_shift) - 1)
#define socket_type_tag ((0 << (socket_shift - 1)) | 120)


#define val_true  ((0 << char_shift) | nonchar_type_tag)
#define val_false ((1 << char_shift) | nonchar_type_tag)
#define val_eof   ((2 << char_shift) | nonchar_type_tag)
#define val_void  ((3 << char_shift) | nonchar_type_tag)
#define val_empty ((4 << char_shift) | nonchar_type_tag)

#endif
