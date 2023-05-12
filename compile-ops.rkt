#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

(define rax 'rax) ; return
(define eax 'eax) ; 32-bit load/store
(define rbx 'rbx) ; heap
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg 2
(define rdx 'rdx) ; arg 3
(define r8  'r8)  ; scratch
(define r9  'r9)  ; scratch
(define r10 'r10) ; scratch
(define r15 'r15) ; stack pad (non-volatile)
(define rsp 'rsp) ; stack

;; Op0 -> Asm
(define (compile-op0 p)
  (match p
    ['void      (seq (Mov rax (value->bits (void))))]
    ['read-byte (seq pad-stack
                     (Call 'read_byte)
                     unpad-stack)]
    ['peek-byte (seq pad-stack
                     (Call 'peek_byte)
                     unpad-stack)]
    ['throw-error (seq (Jmp 'raise_error_align))]
                     ))

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1
     (seq (assert-integer rax)
          (Add rax (value->bits 1)))]
    ['sub1
     (seq (assert-integer rax)
          (Sub rax (value->bits 1)))]
    ['abs 
     (seq (Mov r9 rax)
          (twos-complement r9)
          (Cmp rax 0)
          (Cmovl rax r9))]
    ['- (twos-complement rax)]
    ['not (seq (Cmp rax val-false)
               (Mov rax (value->bits #f))
               (Mov r9  (value->bits #t))
               (Cmove rax r9))]
    ['zero?
     (seq (assert-integer rax)
          (eq-value 0))]
    ['char?
     (type-pred mask-char type-char)]
    ['char->integer
     (seq (assert-char rax)
          (Sar rax char-shift)
          (Sal rax int-shift))]
    ['integer->char
     (seq (assert-codepoint rax)
          (Sar rax int-shift)
          (Sal rax char-shift)
          (Xor rax type-char))]
    ['eof-object? (eq-value eof)]
    ['write-byte
     (seq (assert-byte rax)
          pad-stack
          (Mov rdi rax)
          (Call 'write_byte)
          unpad-stack
          (Mov rax (value->bits (void))))]
    ['box
     (seq (Mov (Offset rbx 0) rax)
          (Mov rax rbx)
          (Or rax type-box)
          (Add rbx 8))]
    ['unbox
     (seq (assert-box rax)
          (Xor rax type-box)
          (Mov rax (Offset rax 0)))]
    ['car
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 8)))]
    ['cdr
     (seq (assert-cons rax)
          (Xor rax type-cons)
          (Mov rax (Offset rax 0)))]
    ['empty? (eq-value '())]
    ['box?
     (type-pred ptr-mask type-box)]
    ['cons?
     (type-pred ptr-mask type-cons)]
    ['vector?
     (type-pred ptr-mask type-vect)]
    ['string?
     (type-pred ptr-mask type-str)]
    ['vector-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-vector rax)
            (Xor rax type-vect)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['string-length
     (let ((zero (gensym))
           (done (gensym)))
       (seq (assert-string rax)
            (Xor rax type-str)
            (Cmp rax 0)
            (Je zero)
            (Mov rax (Offset rax 0))
            (Sal rax int-shift)
            (Jmp done)
            (Label zero)
            (Mov rax 0)
            (Label done)))]
    ['close
     (seq (assert-file-or-socket rax)
          (Mov rdi rax)
          pad-stack
          (Call 'spite_close)
          unpad-stack)]
    ['read ;; One argument read does read from stdin
     (seq (assert-natural rax)
          (Mov rdi rax)
          pad-stack
          (Call 'spite_read_stdin)
          unpad-stack)]
    ['write ;; One argument write does write to stdout
     (seq (assert-string rax)
          (Mov rdi rax)
          pad-stack
          (Call 'spite_write_stdout)
          unpad-stack)]
    ['listen
     (seq (assert-natural rax)
          (Mov rdi rax)
          pad-stack
          (Call 'spite_listen)
          unpad-stack)]
    ['accept
     (seq (assert-socket rax)
          (Mov rdi rax)
          pad-stack
          (Call 'spite_accept)
          unpad-stack)]
    ['integer? (type-pred mask-int type-int)]
    ['boolean? (let ((ok (gensym 'ok))) 
                    (seq (Mov r9 rax)
                         (Mov rax val-true)
                         (generate-long-comparisons (list val-true val-false) ok)
                         (Mov rax val-false)
                         (Label ok)))]))

(define (generate-long-comparisons lst oklabel)
  (match lst
    ['() '()]
    [(cons val rst) 
     (seq (Cmp r9 val) 
          (Je oklabel) 
          (generate-long-comparisons rst oklabel))]))

;; Op2 -> Asm
(define (compile-op2 p)
  (match p
    ['+
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Add rax r8))]
    ['-
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Sub r8 rax)
          (Mov rax r8))]
    ['<
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          (if-lt))]
    ['=
     (seq (Pop r8)
          (assert-integer r8)
          (assert-integer rax)
          (Cmp r8 rax)
          (if-equal))]
    ['cons
     (seq (Mov (Offset rbx 0) rax)
          (Pop rax)
          (Mov (Offset rbx 8) rax)
          (Mov rax rbx)
          (Or rax type-cons)
          (Add rbx 16))]
    ['eq?
     (seq (Pop r8)
          (Cmp rax r8)
          (if-equal))]
    ['make-vector
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (Cmp r8 0) ; special case empty vector
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-vect)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Label loop)
            (Mov (Offset rbx 0) rax)
            (Add rbx 8)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-vect)
            (Label done)))]

    ['vector-ref
     (seq (Pop r8)
          (assert-vector r8)
          (assert-integer rax)
          (Cmp r8 type-vect)
          (Je 'raise_error_align) ; special case for empty vector
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)      ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 3)
          (Add r8 rax)
          (Mov rax (Offset r8 8)))]

    ['make-string
     (let ((loop (gensym))
           (done (gensym))
           (empty (gensym)))
       (seq (Pop r8)
            (assert-natural r8)
            (assert-char rax)
            (Cmp r8 0) ; special case empty string
            (Je empty)

            (Mov r9 rbx)
            (Or r9 type-str)

            (Sar r8 int-shift)
            (Mov (Offset rbx 0) r8)
            (Add rbx 8)

            (Sar rax char-shift)

            (Add r8 1) ; adds 1
            (Sar r8 1) ; when
            (Sal r8 1) ; len is odd

            (Label loop)
            (Mov (Offset rbx 0) eax)
            (Add rbx 4)
            (Sub r8 1)
            (Cmp r8 0)
            (Jne loop)

            (Mov rax r9)
            (Jmp done)

            (Label empty)
            (Mov rax type-str)
            (Label done)))]

    ['string-ref
     (seq (Pop r8)
          (assert-string r8)
          (assert-integer rax)
          (Cmp r8 type-str)
          (Je 'raise_error_align) ; special case for empty string
          (Cmp rax 0)
          (Jl 'raise_error_align)
          (Xor r8 type-str)       ; r8 = ptr
          (Mov r9 (Offset r8 0))  ; r9 = len
          (Sar rax int-shift)     ; rax = index
          (Sub r9 1)
          (Cmp r9 rax)
          (Jl 'raise_error_align)
          (Sal rax 2)
          (Add r8 rax)
          (Mov 'eax (Offset r8 8))
          (Sal rax char-shift)
          (Or rax type-char))]
    ['open
     (seq (Pop rdi)
          (assert-string rdi) ; WAS assert-file
          (assert-char rax)
          (Mov rsi rax)
          pad-stack
          (Call 'spite_open)
          unpad-stack)]
    ['read
     (seq (Pop rdi)
          (assert-file-or-socket rdi)
          (assert-natural rax)
          (Mov rsi rax)
          pad-stack
          (Call 'spite_read)
          unpad-stack)]
    ['write
     (seq (Pop rdi)
          (assert-file-or-socket rdi)
          (assert-string rax)
          (Mov rsi rax)
          pad-stack
          (Call 'spite_write)
          unpad-stack)]
    ['open-sock
     (seq (Pop rdi)
          (assert-string rdi)
          (assert-natural rax)
          (Mov rsi rax)
          pad-stack
          (Call 'spite_open_sock)
          unpad-stack)]
    ['on-message
     (let ([fun (gensym)]
           [end (gensym)])
       (seq (%% "on-message start")
            (Pop rdi)
            (assert-socket rdi)
            (assert-proc rax)
            ;; Call spite_on_message with sock, fun pointer, and proc
            (Lea rsi fun)
            (Mov rdx rax)
            pad-stack
            ;; Save r15 on the stack since C will call back into spite and r15
            ;; will be overwritten 
            (Push r15)
            (Call 'spite_on_message)
            (Pop r15)
            unpad-stack
            (Jmp end)

            ;; Function that C calls
            ;; rdi is msg
            ;; rsi is proc
            ;; this function here is the first time that our assembly is the
            ;; CALLEE rather than the CALLER except for the entry point. We
            ;; need to be sure to save any CALLEE-saved registers we use to the
            ;; stack
            (Label fun)
            (%% "Lambda_entry start")
            ;; return address to C *should* already be on the stack, so we
            ;; shouldn't need to jump back here at all
            (assert-string rdi)
            (assert-proc rsi)

            ;; Push proc and arguments onto stack so the stack is the same as
            ;; if msg was applied to the lambda. This assumes that the lambda
            ;; passed to on-message takes one argument without checking.
            (Mov rax rsi)
            (Push rsi)
            (Push rdi)

            (Mov 'rcx 1) ;; Set arity check

            ;; Get label address
            (Xor rax type-proc)
            (Mov rax (Offset rax 0))
            (Jmp rax) ;; jump to proc

            ;; End function
            (Label end)))]))

;; Op3 -> Asm
(define (compile-op3 p)
  (match p
    ['vector-set!
     (seq (Pop r10)
          (Pop r8)
          (assert-vector r8)
          (assert-integer r10)
          (Cmp r10 0)
          (Jl 'raise_error_align)
          (Xor r8 type-vect)       ; r8 = ptr
          (Mov r9 (Offset r8 0))   ; r9 = len
          (Sar r10 int-shift)      ; r10 = index
          (Sub r9 1)
          (Cmp r9 r10)
          (Jl 'raise_error_align)
          (Sal r10 3)
          (Add r8 r10)
          (Mov (Offset r8 8) rax)
          (Mov rax (value->bits (void))))]
    ['string-assign 
      (seq
        (%%% "string-asssign")
        ;rax holds the char 
        ;pop index into r8
        (Pop r8)
        ;pop string into r10
        (Pop r10)

        
        (assert-char rax)
        (assert-string r10)
        (assert-integer r8)
        ;get rid of the type branding for int to do math later
        ;(Xor r8 type-int)
        (Sar r8 int-shift)

        ;check that the index is less than the length
        ;(Cmp (Offset r10 0) r8)
        ;(Jg 'raise_error_align)

        
        ;do index * 4
        (Sal r8 2)

        
        ;char->integer
        (Sar rax char-shift)

        
        ;increment the place in memory to match the index
        (Add r10 r8)  

        ;move the char into the correct cell to replace the previous value
        (Mov (Offset r10 4) eax)  ; 4 to account for the length, shouldnt it be 8?
        
        (Sub r10 r8)
        (Mov rax r10)
        (%%% "end string assign")
      )]))

(define (compile-opN p)
  (match p
    ['+ (let ((opnloop (gensym 'opnloop)) (endloop (gensym 'endloop))) 
             (seq (Pop r10)
                  (Mov rax (value->bits 0))
                  (Label opnloop)
                  (Cmp r10 (value->bits 1))
                  (Jl endloop)
                  (Pop r8)
                  (assert-integer r8)
                  (Add rax r8)
                  (Sub r10 (value->bits 1))
                  (Jmp opnloop)
                  (Label endloop)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error_align))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (value->bits #t))
         (Je l)
         (Mov rax (value->bits #f))
         (Label l))))

(define assert-file
  (assert-type mask-file type-file))
(define assert-socket
  (assert-type mask-socket type-socket))
(define (assert-file-or-socket arg)
  (let ((good (gensym)))
    (seq (Mov r9 arg)
         (And r9 mask-file)
         (Cmp r9 type-file)
         (Je good)
         (Mov r9 arg)
         (And r9 mask-socket)
         (Cmp r9 type-socket)
         (Je good)
         (Jmp 'raise_error_align)
         (Label good))))

(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-vector
  (assert-type ptr-mask type-vect))
(define assert-string
  (assert-type ptr-mask type-str))
(define assert-proc
  (assert-type ptr-mask type-proc))

(define (assert-codepoint r)
  (let ((ok (gensym)))
    (seq (assert-integer r)
         (Cmp r (value->bits 0))
         (Jl 'raise_error_align)
         (Cmp r (value->bits 1114111))
         (Jg 'raise_error_align)
         (Cmp r (value->bits 55295))
         (Jl ok)
         (Cmp r (value->bits 57344))
         (Jg ok)
         (Jmp 'raise_error_align)
         (Label ok))))

(define (assert-byte r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'raise_error_align)
       (Cmp r (value->bits 255))
       (Jg 'raise_error_align)))

(define (assert-natural r)
  (seq (assert-integer r)
       (Cmp r (value->bits 0))
       (Jl 'raise_error_align)))

;; -> Asm
;; set rax to #t or #f based on given comparison
(define (if-compare c)
  (seq (Mov rax (value->bits #f))
       (Mov r9  (value->bits #t))
       (c rax r9)))

(define (if-equal) (if-compare Cmove))
(define (if-lt) (if-compare Cmovl))

;; Value -> Asm
(define (eq-value v)
  (seq (Cmp rax (value->bits v))
       (if-equal)))

;; Asm
;; Dynamically pad the stack to be aligned for a call
(define pad-stack
  (seq (Mov r15 rsp)
       (And r15 #b1000)
       (Sub rsp r15)))

;; Asm
;; Undo the stack alignment after a call
(define unpad-stack
  (seq (Add rsp r15)))


;; Reg
;; Performs twos complement on register
(define (twos-complement reg)
  (seq (Sar reg 1)
       (Not reg)
       (Add reg 1)
       (Sal reg 1)))
