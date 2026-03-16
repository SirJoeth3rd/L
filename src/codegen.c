#include "../L.h"
#include <stdio.h>

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

  func_type = env_lookup(env, func_name->symbol);

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
