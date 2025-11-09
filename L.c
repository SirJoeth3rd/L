#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "dn_hashmap/dn_hashmap.h"
#include "dn_hashmap/arena.h"
#include "dn_hashmap/dn_string.h"
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
  HashMap symbol_table; // char* -> LType
} LEnv;

typedef struct LVal LVal;
typedef struct LType LType;

struct LType {
  const char* name;
  struct LType* parent;
  uint16_t members_len;
  struct LType* members;
};

#define SEED 0

#define INT "int"
#define CHAR "char"
#define BOOL "bool"

LEnv env_init(Arena* types_arena) {
  // initialize with the basic types
  HashMap symbol_table;
  LType* int_ptr, *char_ptr, *bool_ptr;
  LEnv env;
  
  symbol_table = hm_init(sizeof(LType));

  int_ptr = (LType*)arena_alloc(types_arena, sizeof(LType));
  int_ptr->name = (char*)arena_alloc_val(types_arena, sizeof(INT), INT);

  char_ptr = (LType*)arena_alloc(types_arena, sizeof(LType));
  char_ptr->name = (char*)arena_alloc_val(types_arena, sizeof(CHAR), CHAR);
  
  bool_ptr = (LType*)arena_alloc(types_arena, sizeof(LType));
  bool_ptr->name = (char*)arena_alloc_val(types_arena, sizeof(BOOL), BOOL);

  hm_put(&symbol_table, komihash(INT, sizeof(INT), SEED), int_ptr);
  hm_put(&symbol_table, komihash(CHAR, sizeof(CHAR), SEED), char_ptr);
  hm_put(&symbol_table, komihash(BOOL, sizeof(BOOL), SEED), bool_ptr);

  env.symbol_table = symbol_table;

  return env;
}

/*
  We would store char some_func(int, bool) in LType as
  name = some_func
  parent = *function_pointer
  members_len = 3
  members = [*char, *int, *bool]
 */

struct LVal {
  enum {
    LSymbol,
    LString,
    LNumber,
    LCons,
    LNil
  } ltype;
  union {
    struct {
      LVal* car;
      LVal* cdr;
    };
    String symbol;
    String string;
    long long int number;  
  };
};

typedef struct {
  bool valid;
  const char* mesg;
} LErr;

LVal cons(struct LVal* car, struct LVal* cdr) {
  return (LVal){
    .ltype = LCons,
    .car = car,
    .cdr = cdr
  };
}

//# Parsing

LVal* parse_number(Arena* arena, char** chr);
LVal* parse_string(Arena* arena, char** chr);
LVal* parse_symbol(Arena* arena, char** chr);

LVal* parse(Arena* arena, char** chr) {
  LVal *lval, *root;

  lval = (LVal*)arena_alloc(arena, sizeof(*lval));
  
  root = lval;
  root->ltype = LCons;

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
      lval->ltype = LNil;
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
    lval->ltype = LCons;
  }

  // getting here -> reached end of input -> current lval = Nil
  lval->ltype = LNil;

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
  lval->ltype = LNumber;
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
  lval->ltype = LString;
  lval->string = (String) {
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
  while(!isspace(**chr) && **chr != '\0' && **chr != ')') {
    length++;
    (*chr)++;
  }

  lval = (LVal*)arena_alloc(arena, sizeof(*lval));
  lval->ltype = LSymbol;
  lval->symbol = (String) {
    .chars = start_position,
    .length = length
  };

  return lval;
}

void print_ltype(LVal* lval) {
  switch (lval->ltype) {
  case LSymbol:
    printf("symbol");
    break;
  case LString:
    printf("string");
    break;
  case LNumber:
    printf("number");
    break;
  case LCons:
    printf("cons");
    break;
  case LNil:
    printf("nil");
    break;
  }
}

void recur_print(Arena* arena, LVal* lval) {
  // XXX: TODO, fix such that it does not leak memory
  switch (lval->ltype) {
  case LSymbol:
    printf("%s", String_cstring(arena, lval->symbol));
    fflush(stdout);
    break;
  case LString:
    printf("%s", String_cstring(arena, lval->string));
    fflush(stdout);
    break;
  case LNumber:
    printf("%lli", lval->number);
    fflush(stdout);
    break;
  case LNil:
    printf("nil");
    fflush(stdout);
    break;
  case LCons:
    printf("<"); fflush(stdout);
    recur_print(arena, lval->car);
    printf(","); fflush(stdout);
    recur_print(arena, lval->cdr);
    printf(">"); fflush(stdout);
  }
}

//# Lexical Analysis

LErr analyse_dec(Arena* arena, LVal* lval, LEnv* env);

LErr analyse(Arena* arena, LVal* lval, LEnv* env) { // (dec f (int int) int))
  if (lval->ltype == LCons) {
    if (lval->car->ltype == LCons) {
      if (lval->car->car->ltype == LSymbol) {
	if (String_cmp(lval->car->car->symbol, "dec")) {
	  analyse_dec(arena, lval->car->cdr, env);
	}
      }
    }
  }
  return (LErr){0};
}

LErr analyse_dec(Arena* arena, LVal* lval, LEnv* env) {
  // <f, <<int, <int, nil>>, <int, nil>>>
  LType function_type;

  //TODO allocate member types for this function and hm_get them.
  
  hm_put(&env->symbol_table, komihash(lval->car->symbol.chars, lval->car->symbol.length, SEED), &function_type);
  return (LErr){};
}

//# Code generation

char* translate(Arena arena, LVal* expr) {
  return NULL;
}


//# Main

int main(int argc, char** argv) {
  Arena expr_arena = arena_init();
  Arena tmp_arena = arena_init();

  char* expr = "(dec f (int int) int) (def f (x y) (+ x y))";
  LVal* lexpr = parse(&expr_arena, &expr);

  recur_print(&tmp_arena, lexpr);printf("\n");

  /* analyse(lexpr, NULL); */

  arena_free(&tmp_arena);
  arena_free(&expr_arena);

  return 0;
}
