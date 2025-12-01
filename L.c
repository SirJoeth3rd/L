#define NDEBUG

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
// TODO, need a hierarchical hashmap for namespaces

typedef struct LEnv {
  HashMap symbol_table; // char* -> LType
} LEnv;

typedef struct LVal LVal;
typedef struct LType LType;

struct LType {
  String name;
  struct LType* parent;
  uint16_t members_len;
  struct LType** members;
};

#define SEED 0

uint64_t String_hash(String str) {
  return komihash(str.chars, str.length, SEED);
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

#define INT "int"
#define CHAR "char"
#define BOOL "bool"
#define NIL "nil"

LEnv env_init(Arena* arena) {
  // initialize with the basic types
  HashMap symbol_table;
  LType *nil_ptr;
  LType new_type;
  LEnv env;

  LType *char_type, *char_ptr_type, *int_type;
  
  symbol_table = hm_init(sizeof(LType));

  new_type.name = (String){.chars = NIL, .length=sizeof(NIL)-1};
  new_type.members_len = 0;
  nil_ptr = (LType*)hm_put(&symbol_table, String_hash(new_type.name), &new_type);
  nil_ptr->parent = nil_ptr;

  new_type.name = (String){.chars = INT, .length=sizeof(INT)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  int_type = hm_put(&symbol_table, String_hash(new_type.name), &new_type);

  new_type.name = (String){.chars = CHAR, .length=sizeof(CHAR)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  char_type = hm_put(&symbol_table, String_hash(new_type.name), &new_type);

  new_type.name = (String){.chars = BOOL, .length=sizeof(BOOL)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  hm_put(&symbol_table, String_hash(new_type.name), &new_type);

  // technically we can have char*********; so this should actually be generalized and the
  // types for pointers should only be added during the analysis phase. (except maybe the first pointer type)

  new_type.name = (String){.chars = "char*", .length=(sizeof("char*")-1)};
  new_type.parent = char_type;
  new_type.members_len = 1;
  new_type.members = arena_alloc(arena, sizeof(LType*));
  *new_type.members = int_type;
  char_ptr_type = hm_put(&symbol_table, String_hash(new_type.name), &new_type);

  new_type.name = (String){.chars = "char**", .length=(sizeof("char**")-1)};
  new_type.parent = char_ptr_type;
  new_type.members_len = 1;
  new_type.members = arena_alloc(arena, sizeof(LType*));
  *new_type.members = int_type;
  hm_put(&symbol_table, String_hash(new_type.name), &new_type);

  // TODO: other base types

  env.symbol_table = symbol_table;

  return env;
}

LErr analyse_dec(Arena* arena, LEnv* env, LVal* lval);

LErr analyse(Arena* arena, LEnv* env, LVal* lval) { // (dec f (int int) int))
  while (lval && lval->ltype != LNil) {
    if (lval->ltype == LCons) {
      if (lval->car->ltype == LCons) {
	if (lval->car->car->ltype == LSymbol) {
	  if (String_cmp(lval->car->car->symbol, "dec")) {
	    analyse_dec(arena, env, lval->car->cdr);
	  }
	}
      }
      lval = lval->cdr;
    }
  }
  return (LErr){0};
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

LType* symbol_table_get(LEnv* st, String name) {
  return hm_get(&st->symbol_table, komihash(name.chars, name.length, SEED));
}

LErr analyse_dec(Arena* arena, LEnv* env, LVal* first_cons) {
  // <f, <<int, <int, nil>>, <int, nil>>> == example setup
  LType function_type;
  LType* member_type;
  LVal *params, *param, *return_param, *function_name;
  int param_count, i;

  params = first_cons->cdr->car;
  param = params;
  return_param = first_cons->cdr->cdr->car;
  function_name = first_cons->car;

  param_count = Llist_length(params);
  
  function_type.name = function_name->symbol;
  function_type.members_len = param_count;
  function_type.members = arena_alloc(arena, param_count*sizeof(LType*));

  for (i = 0; i < param_count - 1; i++) {
    if (param->car->ltype != LSymbol) { // TODO: error
      printf("Expected type LSymbol but got ");
      print_ltype(param->car);
      printf("\n");
    }

    member_type = symbol_table_get(env, param->car->symbol);

    if (!member_type) { // TODO: error
      printf("Could not find type %s\n", String_cstring(arena, param->symbol));
    }

    function_type.members[i] = member_type;
    param = param->cdr;
  }
  
  if (param->ltype == LCons && param->car->ltype == LSymbol) {
    member_type = symbol_table_get(env, param->car->symbol);
    function_type.members[param_count-1] = member_type;
  }// TODO: else error
  

  LType* return_type = symbol_table_get(env, return_param->symbol);

  if (!return_type) { // TODO: error
    printf("Could not find type %s\n", String_cstring(arena, param->symbol));
  } else {
    function_type.parent = return_type;
  }

  hm_put(&env->symbol_table, String_hash(function_type.name), &function_type);
  return (LErr){0};
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
  case LString:
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
	if (String_cmp(lval->car->symbol, "+")) {
	  compile_plus(fd, env, lval->cdr);
	} else if (String_cmp(lval->car->symbol, "-")) {
	  compile_minus(fd, env, lval->cdr);
	} else if (String_cmp(lval->car->symbol, "def")) {
	  compile_def(fd, env, lval->cdr);
	} else if (String_cmp(lval->car->symbol, "let")){
	  compile_let(fd, env, lval->cdr);
	} else if (String_cmp(lval->car->symbol, "*")){
	  compile_mul(fd, env, lval->cdr);
	} else if (String_cmp(lval->car->symbol, "dec")){
	  return;
	} else if (String_cmp(lval->car->symbol, "return")){
	  //TODO: L code should not have to specify return.
	  dprintf(fd, "return ");
	  compile(fd, env, lval->cdr);
	  dprintf(fd, ";\n");
	} else {
	  func_type = hm_get(&env->symbol_table, String_hash(lval->car->symbol));
	  if (!func_type) {
	    printf("unrecgonized symbol -> %s", String_cstring(NULL, lval->car->symbol));
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
