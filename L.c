#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "dn_hashmap/dn_hashmap.h"
#include "dn_hashmap/arena.h"
#include "komihash.h"

/*
  The Idea of L.
  There are 2 powerful ideas in computer science.
  1. Lexical Analysis and Transformation
    Macros
  2. Semantic Analysis and Transformation
    Type checking

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
     
   (defmacro decdef (name args type body)
     (let ((arg-names list) (arg-types list))
       (foreach (argname type) :in args (
         (append arg-names argname)
	 (append arg-types type)
       ))
       (dec name arg-types type)
       (def name arg-names body))
   )

   Core of the language seems to be
   (set position value)
   (cond bool-expr body)
   (+|-|*|\|'|'|&|~) all that good stuff
   (dec args)
   (let variables body)
   (type struct|enum|union)

   (defmacro while (boolexpr body)
     (label :start)
     (cond boolexpr (goto :end))
     (body)
     (goto :start)
     (label :end)
   )

   (defmacro for (init cond end-stmnt body)
     init
     (while cond
       (do body end-stmnt))
   )

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

/*
  The core unit of measurement of everything on a cpu is the byte.
  Every other type can be reduced to a number of bytes and a name.
  Off course you have some special names for frequent types like int, float etc
  Types form chains whereby a type can inherit another type.

  typedef int userid;

  so userid has a ancestor type reference to int.
 */

/*
  To actually generate code from this.
  1. Recursively expand all macros
  2. Find all decs
  3. Compile expanded code
 */

typedef struct LEnv {
  HashMap symbol_table; // string -> LSymbol
} LEnv;

typedef struct LType {
  const char* name;
  struct LType* parent;
} LType;

typedef struct {
  const char* chars;
  uint32_t length;
} LString;

typedef struct LVal LVal;

struct LVal {
  enum {
    Symbol,
    String,
    Number,
    Cons,
    Nil
  } ltype;
  union {
    struct {
      LVal* car;
      LVal* cdr;
    };
    LString symbol;
    LString string;
    long long int number;  
  };
  LType type;
};

LVal cons(struct LVal* car, struct LVal* cdr) {
  return (LVal){
    .ltype = Cons,
    .car = car,
    .cdr = cdr
  };
}

LVal* parse_number(Arena* arena, char** chr);
LVal* parse_string(Arena* arena, char** chr);
LVal* parse_symbol(Arena* arena, char** chr);

LVal* parse(Arena* arena, char** chr) {
  LVal *lval, *root;

  lval = (LVal*)arena_alloc(arena, sizeof(*lval));
  
  root = lval;
  root->ltype = Cons;

  while(**chr) {
    switch (**chr) {
    case ' ':
    case '\t':
    case '\n':
      (*chr)++;
      continue;
	
    case '(':
      (*chr)++;
      lval->car = parse(arena, chr);
      goto post_symbol;
      
    case ')':
      (*chr)++;
      lval->ltype = Nil;
      goto exit_parse;

    case '"':
      lval->car = parse_string(arena, chr);
      goto post_symbol;
    }

    if (isdigit(**chr)) {
      lval->car = parse_number(arena, chr);
    } else {
      // if not anything else it's a symbol
      lval->car = parse_symbol(arena, chr);
    }
    
    // append the new <value_here, next cons>
  post_symbol:
    lval->cdr = (LVal*)arena_alloc(arena, sizeof(*lval));
    lval = lval->cdr;
    lval->ltype = Cons;
  }

  // getting here -> reached end of input -> current lval = Nil
  lval->ltype = Nil;

 exit_parse:
  return root;
}

LVal* parse_number(Arena* arena, char** chr) {
  char buffer[1024];
  int i;
  LVal* lval;

  i = 0;
  while (isdigit(**chr)) {
    buffer[i] = **chr;
    i++;
    (*chr)++;
  }

  buffer[i] = 0;
  lval = arena_alloc(arena, sizeof(*lval));
  lval->ltype = Number;
  lval->number = atoi(buffer);
  return lval;
}

LVal* parse_string(Arena* arena, char** chr) {
  LVal* lval;
  int length;
  bool escaped = false;
  char* starting_position;

  starting_position = *chr;

  while ((**chr != '"') && !escaped) {
    if (**chr == '\\') {
      escaped = true;
    } else if (escaped) {
      escaped = false;
    }
    
    (*chr)++;
    length++;
  }

  lval = arena_alloc(arena, sizeof(*lval));
  lval->ltype = String;
  lval->string = (LString) {
    .chars = starting_position,
    .length = length
  };
  return lval;
}

LVal* parse_symbol(Arena* arena, char** chr) {
  //TODO: could be a litte more advanced
  LVal* lval;
  int length;
  char* start_position;

  length = 0;
  start_position = *chr;
  while(!isspace(**chr) && **chr != '\0') {
    length++;
    (*chr)++;
  }

  lval = (LVal*)arena_alloc(arena, sizeof(*lval));
  lval->ltype = Symbol;
  lval->symbol = (LString) {
    .chars = start_position,
    .length = length
  };

  return lval;
}

char* LString_cstring(LString lstring) {
  char* buffer;
  buffer = malloc(sizeof(char)*(lstring.length+1));
  memcpy(buffer, lstring.chars, lstring.length);
  buffer[lstring.length] = '\0';
  return buffer;
}

void recur_print(LVal* lval) {
  // XXX: TODO, fix such that it does not leak memory
  switch (lval->ltype) {
  case Symbol:
    printf("%s ", LString_cstring(lval->symbol));
    fflush(stdout);
    break;
  case String:
    printf("%s ", LString_cstring(lval->string));
    fflush(stdout);
    break;
  case Number:
    printf("%lli ", lval->number);
    fflush(stdout);
    break;
  case Nil:
    printf("nil ");
    fflush(stdout);
    break;
  case Cons:
    if (lval->car->ltype == Cons) {
      printf("[");
      fflush(stdout);
      
      recur_print(lval->car);
      
      printf("] ");
      fflush(stdout);
      
      recur_print(lval->cdr);
      
    } else {
      recur_print(lval->car);
      recur_print(lval->cdr);
    }
  }
} 

int main(int argc, char** argv) {
  Arena expr_arena = arena_init();

  char* expr = "(+ (* 1 2) (* 3 4))";
  LVal* lexpr = parse(&expr_arena, &expr);

  recur_print(lexpr);printf("\n");

  return 0;
}
