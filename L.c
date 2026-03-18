/* NEW IMPORTS */
#include "L.h"
#include "src/environment.c"
#include "src/parse.c"
#include "src/analyze.c"
#include "src/codegen.c"
#include "tinycc/libtcc.h"

#define ARENA_IMPLEMENTATION
#include "src/arena.h"

#define L_STRING_IMPLEMENTATION
#include "src/lstring.h"

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

/* Produced C code: won't try to make this readable.
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
  To actually generate code from this.
  1. Recursively expand all macros
  2. Find all decs
  3. Compile expanded code
 */

/* # Parsing */
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

/* # Main */
char* read_file(const char* filename) {
  FILE* file;
  char* buffer, *bstart;
  long filesize;
  char c;

  file = fopen(filename, "r");
  fseek(file, 0L, SEEK_END);
  filesize = ftell(file);

  /*  move file pointer back to start */
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

  recur_print(lexpr);printf("\n");

  env = env_init(&tmp_arena);
  analyse(&tmp_arena, &env, lexpr);

  FILE* file = fopen("compiled.c", "w+");
  if (file == NULL) {
    perror("Error opening file\n");
    return EXIT_FAILURE;
  }
	LSink filesink = lsink_file(file);
  compile(filesink,&env,lexpr);

  fclose(file);
  free(lcode);
  arena_free(&tmp_arena);
  return 0;
}
