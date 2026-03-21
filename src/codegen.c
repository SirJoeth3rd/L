#include "../L.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

void print_lval(FILE*, LVal*);
void print_comma_seperated_list(FILE*, LVal*);
void compile_plus(FILE*, LEnv*, LVal*);
void compile_minus(FILE*, LEnv*, LVal*);
void compile_mul(FILE*, LEnv*, LVal*);
void compile_def(FILE*, LEnv*, LVal*);
void compile_let(FILE*, LEnv*, LVal*);
void compile(FILE*, LEnv*, LVal*);

void print_lval(FILE* file, LVal* lval) {
  switch(lval->ltype) {
  case LSymbol:
    fprintf(file, "%.*s", lval->symbol.length, lval->symbol.chars);
    break;
  case LNumber:
    fprintf(file, "%lld", lval->number);
    break;
  case LLString:
    fprintf(file, "\"%.*s\"", lval->string.length, lval->string.chars);
    break;
  default:
    printf("tried to print lval of type ");
    print_ltype(lval);
    break;
  }
}

void print_comma_seperated_list(FILE* file, LVal* list) {
  int i, length;
  length = Llist_length(list);
  for (i = 0; i < length - 1; i++) {
    print_lval(file, list->car);
    list = list->cdr;
  }
  if (length > 0) {
    print_lval(file, list->car);
  }
}

void compile(FILE* file, LEnv* env, LVal* lval) {
  /*(+ 1 2 3) -> <+, <1, <2, <3, nil>>>>*/
  LType* func_type;

	while (lval) {
		switch (lval->ltype) {
		case LCons:
			if (lval->car->ltype == LSymbol) {
				if (LString_cmp(lval->car->symbol, "+")) {
					compile_plus(file, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "-")) {
					compile_minus(file, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "def")) {
					compile_def(file, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "let")){
					compile_let(file, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "*")){
					compile_mul(file, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "dec")){
					return;
				} else if (LString_cmp(lval->car->symbol, "return")){
					/*TODO: L code should not have to specify return.*/
					fprintf(file, "return ");
					compile(file, env, lval->cdr);
					fprintf(file, ";\n");
				} else {
					func_type = env_lookup(env, lval->car->symbol);
					if (!func_type) {
						printf("unrecgonized symbol -> %.*s", lval->car->symbol.length, lval->car->symbol.chars);
						exit(1);
					} else {
						fprintf(file, "%.*s(", lval->car->symbol.length, lval->car->symbol.chars);
						print_comma_seperated_list(file, lval->cdr);
						fprintf(file, ")");
					}
				}
				/* compile_... will consume the entire list ie we can exit out of compile*/
				compile(file, env, lval->car);
				return;
      } else if (lval->car->ltype == LCons) {
				compile(file, env, lval->car);
      }
      lval = lval->cdr;
			break;
		case LNil:
			goto end;
		default:
			print_lval(file, lval);
		}
	}
 end:
}

void compile_plus(FILE* file, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    fprintf(file, "%.*s + ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  fprintf(file, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_minus(FILE* file, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    fprintf(file, "%.*s - ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  fprintf(file, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_mul(FILE* file, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    fprintf(file, "%.*s * ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  fprintf(file, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_def(FILE* file, LEnv* env, LVal* lval) {
  /*<main,<<argc,<argv,nil>>,<<return,<0,nil>>,nil>>>*/
  /*TODO: Type Checking Here? Or earlier in analysis.*/
  LVal* func_name, *args, *body;
  LType* func_type;
  int i;
  
  func_name = lval->car;
  args = lval->cdr->car;
  body = lval->cdr->cdr;

  func_type = env_lookup(env, func_name->symbol);

  fprintf(file, "%.*s ", func_type->parent->name.length, func_type->parent->name.chars);
  fprintf(file, "%.*s(", func_name->symbol.length, func_name->symbol.chars);

  for (i = 0; i < func_type->members_len - 1; i++) {
    fprintf(file, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    fprintf(file, "%.*s,", args->car->symbol.length, args->car->symbol.chars);
    args = args->cdr;
  }

  if (func_type->members_len) {
    fprintf(file, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    fprintf(file, "%.*s) {\n", args->car->symbol.length, args->car->symbol.chars);
  } else {
		fprintf(file, ") {\n");
	}

  /* TODO: update env to include function local variables*/

  compile(file, env, body);
  
  fprintf(file, "}\n");
}

void compile_let(FILE* file, LEnv* env, LVal* lval) {
  /* <<let,<<<x,<int,nil>>,<<y,<char,nil>>,nil>>,<<+,<x,<y,nil>>>,nil>>>,nil> */
  LVal* var_pairs, *body, *var_pair, *var_name, *var_type_symbol;
  int i, total_pairs;
  
  var_pairs = lval->car;
  body = lval->cdr;

  fprintf(file, "{");

  total_pairs = Llist_length(var_pairs);
  for (i = 0; i < total_pairs; i++) {
    var_pair = var_pairs->car;
    var_name = var_pair->car;
    var_type_symbol = var_pair->cdr->car;

    fprintf(file, "%.*s ", var_type_symbol->symbol.length, var_type_symbol->symbol.chars);
    fprintf(file, "%.*s;\n", var_name->symbol.length, var_name->symbol.chars);
    var_pairs = var_pairs->cdr;
  }
  
  compile(file, env, body);
  fprintf(file, "}");
}
