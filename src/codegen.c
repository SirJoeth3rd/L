#include "../L.h"
#include <stdio.h>
#include <stdarg.h>

void print_lval(LSink, LVal*);
void print_comma_seperated_list(LSink, LVal*);
void compile_plus(LSink, LEnv*, LVal*);
void compile_minus(LSink, LEnv*, LVal*);
void compile_mul(LSink, LEnv*, LVal*);
void compile_def(LSink, LEnv*, LVal*);
void compile_let(LSink, LEnv*, LVal*);
void compile(LSink, LEnv*, LVal*);

LSink lsink_buffer(LString string) {
	return (LSink) {
		.buffer = string,
		.tag = LSink_Buffer
	};
}

LSink lsink_file(FILE* file) {
	return (LSink) {
		.file = file,
		.tag = LSink_File
	};
}

int sinkprintf(LSink sink, const char* format, ...) {
	va_list args;
	int result;
	va_start(args, format);
	switch(sink.tag) {
	case LSink_Buffer:
		result = vsnprintf(sink.buffer.chars, sink.buffer.length, format, args);
		break;
	case LSink_File:
		result = vfprintf(sink.file, format, args);
		break;
	}
	va_end(args);
	return result;
}

void print_lval(LSink sink, LVal* lval) {
  switch(lval->ltype) {
  case LSymbol:
    sinkprintf(sink, "%.*s", lval->symbol.length, lval->symbol.chars);
    break;
  case LNumber:
    sinkprintf(sink, "%lld", lval->number);
    break;
  case LLString:
    sinkprintf(sink, "\"%.*s\"", lval->string.length, lval->string.chars);
    break;
  default:
    printf("tried to print lval of type ");
    print_ltype(lval);
    printf("\n");
    break;
  }
}

void print_comma_seperated_list(LSink sink, LVal* list) {
  int i, length;
  length = Llist_length(list);
  for (i = 0; i < length - 1; i++) {
    print_lval(sink, list->car);
    list = list->cdr;
  }
  if (length > 0) {
    print_lval(sink, list->car);
  }
}

void compile(LSink sink, LEnv* env, LVal* lval) {
  /*(+ 1 2 3) -> <+, <1, <2, <3, nil>>>>*/
  LType* func_type;

  while (lval->ltype != LNil && lval) {
    if (lval->ltype == LCons) {
      if (lval->car->ltype == LSymbol) {
				if (LString_cmp(lval->car->symbol, "+")) {
					compile_plus(sink, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "-")) {
					compile_minus(sink, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "def")) {
					compile_def(sink, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "let")){
					compile_let(sink, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "*")){
					compile_mul(sink, env, lval->cdr);
				} else if (LString_cmp(lval->car->symbol, "dec")){
					return;
				} else if (LString_cmp(lval->car->symbol, "return")){
					/*TODO: L code should not have to specify return.*/
					sinkprintf(sink, "return ");
					compile(sink, env, lval->cdr);
					sinkprintf(sink, ";\n");
				} else {
					func_type = env_lookup(env, lval->car->symbol);
					if (!func_type) {
						printf("unrecgonized symbol -> %.*s", lval->car->symbol.length, lval->car->symbol.chars);
					} else {
						sinkprintf(sink, "%.*s(", lval->car->symbol.length, lval->car->symbol.chars);
						print_comma_seperated_list(sink, lval->cdr);
						sinkprintf(sink, ")");
					}
				}
				/* compile_... will consume the entire list ie we can exit out of compile*/
				return;
      } else if (lval->car->ltype == LCons) {
				compile(sink, env, lval->car);
      }
      lval = lval->cdr;
    }
  }
}

void compile_plus(LSink sink, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    sinkprintf(sink, "%.*s + ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  sinkprintf(sink, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_minus(LSink sink, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    sinkprintf(sink, "%.*s - ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  sinkprintf(sink, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_mul(LSink sink, LEnv* env, LVal* lval) {
  /*(+ x y z)*/
  while (lval->cdr->ltype == LCons) {
    /* TODO: assuming here that the type is symbol*/
    sinkprintf(sink, "%.*s * ", lval->car->symbol.length, lval->car->symbol.chars);
    lval = lval->cdr;
  }
  sinkprintf(sink, "%.*s", lval->car->symbol.length, lval->car->symbol.chars);
}

void compile_def(LSink sink, LEnv* env, LVal* lval) {
  /*<main,<<argc,<argv,nil>>,<<return,<0,nil>>,nil>>>*/
  /*TODO: Type Checking Here? Or earlier in analysis.*/
  LVal* func_name, *args, *body;
  LType* func_type;
  int i;
  
  func_name = lval->car;
  args = lval->cdr->car;
  body = lval->cdr->cdr;

  func_type = env_lookup(env, func_name->symbol);

  sinkprintf(sink, "%.*s ", func_type->parent->name.length, func_type->parent->name.chars);
  sinkprintf(sink, "%.*s(", func_name->symbol.length, func_name->symbol.chars);

  for (i = 0; i < func_type->members_len - 1; i++) {
    sinkprintf(sink, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    sinkprintf(sink, "%.*s,", args->car->symbol.length, args->car->symbol.chars);
    args = args->cdr;
  }

  if (func_type->members_len) {
    sinkprintf(sink, "%.*s ", func_type->members[i]->name.length, func_type->members[i]->name.chars);
    sinkprintf(sink, "%.*s) {\n", args->car->symbol.length, args->car->symbol.chars);
  }

  /* TODO: update env to include function local variables*/

  compile(sink, env, body);
  
  sinkprintf(sink, "}\n");
}

void compile_let(LSink sink, LEnv* env, LVal* lval) {
  /* <<let,<<<x,<int,nil>>,<<y,<char,nil>>,nil>>,<<+,<x,<y,nil>>>,nil>>>,nil> */
  LVal* var_pairs, *body, *var_pair, *var_name, *var_type_symbol;
  int i, total_pairs;
  
  var_pairs = lval->car;
  body = lval->cdr;

  sinkprintf(sink, "{");

  total_pairs = Llist_length(var_pairs);
  for (i = 0; i < total_pairs; i++) {
    var_pair = var_pairs->car;
    var_name = var_pair->car;
    var_type_symbol = var_pair->cdr->car;

    sinkprintf(sink, "%.*s ", var_type_symbol->symbol.length, var_type_symbol->symbol.chars);
    sinkprintf(sink, "%.*s;\n", var_name->symbol.length, var_name->symbol.chars);
    var_pairs = var_pairs->cdr;
  }
  
  compile(sink, env, body);
  sinkprintf(sink, "}");
}
