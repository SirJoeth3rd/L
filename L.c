#define _GNU_SOURCE

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

#include <stdio.h>
#include <string.h>

/*
  The idea of L.
	I love 2 programming languages, C and lisp.
	L is attempt to unify these programming languages. 
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
       (do body end-stmnt)))

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

/*
	Produced C code: won't try to make this readable.
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

void pprint(LVal* l, int d) {
	switch (l->ltype) {
	case LCons:
		if (l->car->ltype == LCons) {
			for (int i = 0; i < (d); i++) {printf("  ");} /* print indent */
			printf("(\n");
			pprint(l->car, d+1);
			for (int i = 0; i < (d); i++) {printf("  ");} /* print indent */
			printf(")\n");
		} else {
			pprint(l->car, d);
		}
		pprint(l->cdr, d);
		break;
	case LNil:
		for (int i = 0; i < (d); i++) { printf("  ");} /* print indent */
		printf("nil\n");
		break;
	default:
		for (int i = 0; i < (d); i++) { printf("  ");} /* print indent */
		print_lval(stdout, l);
		printf("\n");
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

void handle_error(void *opaque, const char *msg) {
    fprintf(opaque, "%s\n", msg);
}

int main(int argc, char** argv) {
  char *lcode_ptr, *lcode;
  LEnv env;

  Arena tmp_arena = arena_init();

  lcode = read_file("./code.l");
  lcode_ptr = lcode;
  LVal* lexpr = parse(&tmp_arena, &lcode_ptr);

	pprint(lexpr, 0);
	printf("\n");
	recur_print(lexpr);
	printf("\n");

  FILE* file = fopen("compiled.c", "w+");
  if (file == NULL) {
    perror("Error opening file\n");
    return EXIT_FAILURE;
  }

  env = env_init(&tmp_arena);
  analyse_print(file, &env, lexpr);
	
  compile(file,&env,lexpr);
	fclose(file);

	char buffer[1000] = {0};
	FILE* buffile = fmemopen(buffer, sizeof(buffer), "w");
	compile(buffile, &env, lexpr);
	fclose(buffile);

	printf("%s\n", buffer);

	TCCState* tcc_state;
	tcc_state = tcc_new();
	if (!tcc_state) {
		fprintf(stderr, "Could not create tcc state\n");
		exit(1);
	}

	tcc_set_error_func(tcc_state, stderr, handle_error);
	tcc_set_output_type(tcc_state, TCC_OUTPUT_MEMORY);

	if (tcc_compile_string(tcc_state, buffer) == -1) {
		exit(1);
	}

	if (tcc_relocate(tcc_state) < 0) {
		exit(1);
	}

	int (*main_func)(int);
	main_func = tcc_get_symbol(tcc_state, "f");
	if (!main_func) {
		printf("Could not find symbol\n");
		exit(2);
	}

	int result = main_func(3);
	printf("result = %i\n", result);

	tcc_delete(tcc_state);

  free(lcode);
  arena_free(&tmp_arena);


	/* TESTING a theory */	
}
