#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>

/*
  The Idea of L.
  There are 2 powerful ideas in computer science.
  1. Lexical Analysis
  2. Code producing Code

  The current modern movement in programming languages is to attempt
  to unify these ideas elegantly, for example Zig. This is another
  attempt in the form of a lisp. The major idea of this language is to
  try and make all compiler internals available at runtime.
 */

/*
  Layer 0: transpiler to C89.
  Layer 1: JIT compilation/interactive debugging using TCC.
  Layer 2: Macro expansion using the JIT compiled functions.
*/

/* Syntax
   (dec function-predicate?
     int float char* '(const char*) bool) --> declare type
   (def function-predicate? (num flt first-s second-s)
    (printf "%i %f %s %s" num flt first-s second-s)
    (true)) --> definition
   (function-predicate? 123 5.6 "hello" "world") --> call produces true

   (decdef another-predicate? (num-a int num-b int)
     (num-a == num-b)) --> result of function is the final expression

   (decdef length (string char*) int
    (let ((total int 0) (chr char)) --> let makes a new scope
      (for (set chr string[0]) (++ chr) (!= chr 0)
        (++total))
      (total)))

   (defmacro foreach (varname type slice body)
     '(let ((varname type)
            (pos int 0))
       (for (set varname slice[0]) (!= slice[pos] 0) (++ pos)
         body)))

   (defmacro incset (arr pos val)
     (++ pos)
     (set arr[pos] val))

   Implementation details of macros are clear to me
   - Every argument to a macro has the same type LExpr
   - The output of a macro is always an LExpr

   (decdef concatenate (string-one char* string-two char*) char*
     (let
       ((length-one int (length string-one))
        (length-two int (length string-two))
	(length-new int (+ length-one length-two))
	(string-new char* (malloc (* (sizeof char) length-new)))
	(pos int 0)
	(spos int 0))
       (foreach chr :in string-one
         (incset string-new pos chr))
       (foreach chr :in string-two
         (incset string-new pos chr))
       string-new))
 */

/* Produced C code: don't TRY to make this readable.
   But if it is well nice.

   int length(char* string) {
     int total;
     char chr;
     total = 0;
     for (chr = string[0]; chr++; chr != 0) {
       total++;
     }
     return total;
   }
 */

// remember we need to store type information in here too

typedef struct LType {
  const char* name;
  
} LType;

// Arena for memory management
typedef struct {
  void* buffer;
  size_t capacity;
  size_t item_size;
} Region;

typedef struct {
  Region* start, end;
} Arena;

LExpr Lparse() {
  
}


int main(int argc, char** argv) {
  
}
