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

void compile_binary_operator(FILE* file, LEnv* env, LVal* lval, LString operator_symbol) {
	/* (+ x y z) */
	fprintf(file, "(");
	while (lval->cdr->ltype == LCons) {
		compile(file, env, lval->car);
		fprintf(file, "%.*s", operator_symbol.length, operator_symbol.chars);
		lval = lval->cdr;
	}
	// TODO: what if final lval isn't LNil?
	compile(file, env, lval->car);
	fprintf(file,")");
}

bool is_binary_operator(LString symbol) {
	return
		LString_cmp(symbol, "+")
		|| LString_cmp(symbol, "-")
		|| LString_cmp(symbol, "*")
		|| LString_cmp(symbol, "/")
		|| LString_cmp(symbol, "&&")
		|| LString_cmp(symbol, "||");
}

bool compile_builtin(FILE* file, LEnv* env, LString symbol, LVal* lval) {
	if (is_binary_operator(symbol)) {
		compile_binary_operator(file, env, lval, symbol);
		return true;
	} else if (LString_cmp(symbol, "def")) { /* check keywords */
		compile_def(file, env, lval);
		return true;
	} else if (LString_cmp(symbol, "let")){
		compile_let(file, env, lval);
		return true;
	} else if (LString_cmp(symbol, "dec")){
		return true; /* just ignore dec */
	} else if (LString_cmp(symbol, "return")){
		/* TODO: L code should not have to specify return. */
		fprintf(file, "return ");
		compile(file, env, lval);
		fprintf(file, ";\n");
		return true;
	}

	return false;
}

void compile_funcall(FILE* file, LEnv* env, LString symbol, LVal* lval) {
	LType* func_type = env_lookup(env, symbol);
	if (!func_type) {
		printf("unrecgonized symbol -> %.*s", symbol.length, symbol.chars);
		exit(1);
	} 
	fprintf(file, "%.*s(", symbol.length, symbol.chars);
	/* comma seperated list */
	if (lval->ltype != LCons) {
		compile(file, env, lval);
	} else {
		while (lval->cdr->ltype == LCons) {
			compile(file, env, lval->car);
			fprintf(file, ",");
			lval = lval->cdr;
		}
		// TODO: what if final lval isn't LNil?
		compile(file, env, lval->car);
	}
	fprintf(file, ")");
}

void compile(FILE* file, LEnv* env, LVal* lval) {
	LType* ltype;
	switch (lval->ltype) {
	case LCons:
		switch (lval->car->ltype) {
		case LCons: /* (+ 8 8) (* 7 7) */
			compile(file, env, lval->car);
			compile(file, env, lval->cdr);
			break;
		case LSymbol:
			if (compile_builtin(file, env, lval->car->symbol, lval->cdr)) return;
			ltype = env_lookup(env, lval->car->symbol);
			if (!ltype) {
				print_lval(file, lval->car);
				//TODO check if this is valid.
			} else {
				compile_funcall(file, env, lval->car->symbol, lval->cdr);
			}
			return;
		case LNil:
			fprintf(file,"()");
			break;
		default:
			print_lval(file, lval->car);
		}
		break; /* I don't really understand but apparently I can just ignore lval->cdr in this case. */
	case LNil:
		break;
	default:
		print_lval(file, lval);
	}
}

void compile_def(FILE* file, LEnv* env, LVal* lval) {
  /*<main,<<argc,<argv,nil>>,<<return,<0,nil>>,nil>>>*/
  /* TODO: Type Checking Here? Or earlier in analysis. */
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
