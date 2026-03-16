#define NDEBUG

#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// NEW IMPORTS
#include "L.h"

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
// TODO, need a hierarchical hashmap for namespaces

//# Parsing

void print_ltype(LVal* lval) {
  switch (lval->ltype) {
  case LSymbol:
    printf("symbol");
    break;
  case LLString:
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

void recur_print(LVal* lval) {
  switch (lval->ltype) {
  case LSymbol:
		printf("%.*s", lval->symbol.length, lval->symbol.chars);
    fflush(stdout);
    break;
  case LLString:
		printf("%.*s", lval->symbol.length, lval->symbol.chars);
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
    recur_print(lval->car);
    printf(","); fflush(stdout);
    recur_print(lval->cdr);
    printf(">"); fflush(stdout);
  }
}

int Llist_length(LVal* list) {
  int len = 0;
  if (list->ltype != LCons) {
    return 0;
  }
  while (list->cdr->ltype == LCons) {
    if (list->car->ltype != LNil) {
      len++;
    }
    list = list->cdr;
  }
  if (list->car->ltype != LNil) {
    len++;
  }
  return len;
}

//# Code generation

void print_lval(int, LVal*);
void print_comma_seperated_list(int, LVal*);
void compile_plus(int, LEnv*, LVal*);
void compile_minus(int, LEnv*, LVal*);
void compile_mul(int, LEnv*, LVal*);
void compile_def(int, LEnv*, LVal*);
void compile_let(int, LEnv*, LVal*);
void compile(int, LEnv*, LVal*);

void print_lval(int fd, LVal* lval) {
  switch(lval->ltype) {
  case LSymbol:
    dprintf(fd, "%.*s", lval->symbol.length, lval->symbol.chars);
    break;
  case LNumber:
    dprintf(fd, "%lld", lval->number);
    break;
  case LLString:
    dprintf(fd, "\"%.*s\"", lval->string.length, lval->string.chars);
    break;
  default:
    printf("tried to print lval of type ");
    print_ltype(lval);
    printf("\n");
    break;
  }
}

void print_comma_seperated_list(int fd, LVal* list) {
  int i, length;
  length = Llist_length(list);
  for (i = 0; i < length - 1; i++) {
    print_lval(fd, list->car);
    list = list->cdr;
  }
  if (length > 0) {
    print_lval(fd, list->car);
  }
}

void compile(int fd, LEnv* env, LVal* lval) {
  //(+ 1 2 3) -> <+, <1, <2, <3, nil>>>>
  LType* func_type;

  while (lval->ltype != LNil && lval) {
    if (lval->ltype == LCons) {
      if (lval->car->ltype == LSymbol) {
	if (LString_cmp(lval->car->symbol, "+")) {
	  compile_plus(fd, env, lval->cdr);
	} else if (LString_cmp(lval->car->symbol, "-")) {
	  compile_minus(fd, env, lval->cdr);
	} else if (LString_cmp(lval->car->symbol, "def")) {
	  compile_def(fd, env, lval->cdr);
	} else if (LString_cmp(lval->car->symbol, "let")){
	  compile_let(fd, env, lval->cdr);
	} else if (LString_cmp(lval->car->symbol, "*")){
	  compile_mul(fd, env, lval->cdr);
	} else if (LString_cmp(lval->car->symbol, "dec")){
	  return;
	} else if (LString_cmp(lval->car->symbol, "return")){
	  //TODO: L code should not have to specify return.
	  dprintf(fd, "return ");
	  compile(fd, env, lval->cdr);
	  dprintf(fd, ";\n");
	} else {
		func_type = env_lookup(env, lval->car->symbol);
	  if (!func_type) {
	    printf("unrecgonized symbol -> %.*s", lval->car->symbol.length, lval->car->symbol.chars);
	  } else {
	    dprintf(fd, "%.*s(", lval->car->symbol.length, lval->car->symbol.chars);
	    print_comma_seperated_list(fd, lval->cdr);
	    dprintf(fd, ")");
	  }
	}
	// compile_... will consume the entire list ie we can exit out of compile
	return;
      } else if (lval->car->ltype == LCons) {
	compile(fd, env, lval->car);
      }
      lval = lval->cdr;
    }
  }
}

void compile_plus(int fd, LEnv* env, LVal* lval) {
  //(+ x y z)
  while (lval->cdr->ltype == LCons) {
    // TODO: assuming here that the type is symbol
    dprintf(fd, "%.*s + ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  dprintf(fd, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_minus(int fd, LEnv* env, LVal* lval) {
  //(+ x y z)
  while (lval->cdr->ltype == LCons) {
    // TODO: assuming here that the type is symbol
    dprintf(fd, "%.*s - ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  dprintf(fd, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_mul(int fd, LEnv* env, LVal* lval) {
  //(+ x y z)
  while (lval->cdr->ltype == LCons) {
    // TODO: assuming here that the type is symbol
    dprintf(fd, "%.*s * ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  dprintf(fd, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_def(int fd, LEnv* env, LVal* lval) {
  //<main,<<argc,<argv,nil>>,<<return,<0,nil>>,nil>>>
  //TODO: need to actually type check here
  LVal* func_name, *args, *body;
  LType* func_type;
  int i;
  
  func_name = lval->car;
  args = lval->cdr->car;
  body = lval->cdr->cdr;

  func_type = (LType*)hm_get(&env->symbol_table, String_hash(func_name->symbol));

  dprintf(fd, "%.*s ", func_type->parent->name.length, func_type->parent->name.chars);
  dprintf(fd, "%.*s(", func_name->symbol.length, func_name->symbol.chars);

  for (i = 0; i < func_type->members_len - 1; i++) {
    dprintf(fd, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    dprintf(fd, "%.*s,", args->car->symbol.length, args->car->symbol.chars);
    args = args->cdr;
  }

  if (func_type->members_len) {
    dprintf(fd, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    dprintf(fd, "%.*s) {\n", args->car->symbol.length, args->car->symbol.chars);
  }

  // TODO: update env to include function local variables

  compile(fd, env, body);
  
  dprintf(fd, "}\n");
}

void compile_let(int fd, LEnv* env, LVal* lval) {
  /* <<let,<<<x,<int,nil>>,<<y,<char,nil>>,nil>>,<<+,<x,<y,nil>>>,nil>>>,nil> */
  LVal* var_pairs, *body, *var_pair, *var_name, *var_type_symbol;
  int i, total_pairs;
  
  var_pairs = lval->car;
  body = lval->cdr;

  dprintf(fd, "{");

  total_pairs = Llist_length(var_pairs);
  for (i = 0; i < total_pairs; i++) {
    var_pair = var_pairs->car;
    var_name = var_pair->car;
    var_type_symbol = var_pair->cdr->car;

    dprintf(fd, "%.*s ", var_type_symbol->symbol.length, var_type_symbol->symbol.chars);
    dprintf(fd, "%.*s;\n", var_name->symbol.length, var_name->symbol.chars);
    var_pairs = var_pairs->cdr;
  }
  
  compile(fd, env, body);
  dprintf(fd, "}");
}

//# Main

char* read_file(const char* filename) {
  FILE* file;
  char* buffer, *bstart;
  long filesize;
  char c;

  file = fopen(filename, "r");
  fseek(file, 0L, SEEK_END);
  filesize = ftell(file);

  // move file pointer back to start
  fseek(file, 0, SEEK_SET);

  bstart = malloc(filesize*sizeof(char));
  buffer = bstart;

  c = fgetc(file);
  while(c != EOF) {
    *buffer = c;
    buffer++;
    c = fgetc(file);
  }

  fclose(file);
  return bstart;
}

int main(int argc, char** argv) {
  char *lcode_ptr, *lcode;
  LEnv env;

  Arena tmp_arena = arena_init();

  lcode = read_file("./code.l");
  lcode_ptr = lcode;
  LVal* lexpr = parse(&tmp_arena, &lcode_ptr);

  recur_print(&tmp_arena, lexpr);printf("\n");

  env = env_init(&tmp_arena);
  analyse(&tmp_arena, &env, lexpr);

  FILE* file = fopen("compiled.c", "w+");
  if (file == NULL) {
    perror("Error opening file\n");
    return EXIT_FAILURE;
  }

  int fd = fileno(file);
  compile(fd,&env,lexpr);

  fclose(file);
  free(lcode);
  arena_free(&tmp_arena);
  return 0;
}
