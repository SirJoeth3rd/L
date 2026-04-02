#include "../L.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

void print_lval(FILE*, LVal*);
void print_comma_seperated_list(FILE*, LVal*);
void compile_plus(FILE*, LEnv*, LVal*);
void compile_minus(FILE*, LEnv*, LVal*);
void compile_mul(FILE*, LEnv*, LVal*);
void compile_def(FILE*, LEnv*, LVal*);
void compile_let(FILE*, LEnv*, LVal*);
void compile_plex(FILE*, LEnv*, LVal*);
void compile_variant(FILE*, LEnv*, LVal*);
void compile_variant_fields(FILE*, LEnv*, LVal*);
void compile(FILE*, LEnv*, LVal*);

//# print_lval
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

//# compile_binary_operator
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

//# compile builtin
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
	} else if (LString_cmp(symbol, "plex")){
		compile_plex(file, env, lval);
		return true;
	} else if (LString_cmp(symbol, "variant")){
		compile_variant(file, env, lval);
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

//# compile plex
void compile_plex_fields(FILE* file, LEnv* env, LVal* lval) {
	assert(lval->ltype == LCons && "syntax error plex fields");
	while(lval->ltype != LNil && lval->car->ltype != LNil) {
		if (LString_cmp(lval->car->car->symbol, "variant")) {
			compile_variant_fields(file, env, lval->car->cdr);
		} else {
			fprintf(file,
							"%.*s %.*s;\n",
							lval->car->car->symbol.length,
							lval->car->car->symbol.chars,
							lval->car->cdr->car->symbol.length,
							lval->car->cdr->car->symbol.chars);
		}
		lval = lval->cdr;
	}
}

void compile_plex(FILE* file, LEnv* env, LVal* lval) {
	assert(lval->ltype == LCons && "plex syntax error");

	if (lval->car->ltype == LSymbol) {
		// typedef
		fprintf(file,
						"struct %.*s {\n",
						lval->car->symbol.length,
						lval->car->symbol.chars);
		compile_plex_fields(file, env, lval->cdr);
		fprintf(file, "};\n");
	} else {
		// anonymous struct
		fprintf(file,"struct {\n");
		compile_plex_fields(file, env, lval->car);
		fprintf(file, "}");
	}
}

//# compile variant
void compile_variant_fields(FILE* file, LEnv* env, LVal* lval) {
	LVal* fields = lval;
	fprintf(file, "enum {\n");
	while (fields->ltype != LNil && fields->car->ltype == LCons) {
		fprintf(
						file,
						"%.*s\n",
						fields->car->car->symbol.length,
						fields->car->car->symbol.chars
						);
		if (fields->cdr->ltype != LNil && fields->cdr->car->ltype != LNil) {
			fprintf(file, ",");
		}
		fields = fields->cdr;
	}
	fprintf(file, "} variant;\n");

	fields = lval;
	fprintf(file, "union {\n");
	while (fields->ltype == LCons && fields->car->ltype == LCons) {
		if (fields->car->cdr->car->ltype == LCons && LString_cmp(fields->car->cdr->car->car->symbol, "plex")) {
			compile_plex(file, env, fields->car->cdr->car->cdr);
		} else if (fields->car->cdr->cdr->ltype == LNil) {
			fields = fields->cdr;
			continue;
		} else {
			fprintf(
							file,
							"%.*s ",
							fields->car->cdr->car->symbol.length,
							fields->car->cdr->car->symbol.chars
							);
		}
		fprintf(
						file,
						"%.*s;\n",
						fields->car->car->symbol.length,
						fields->car->car->symbol.chars
						);
		fields = fields->cdr;
	}
	fprintf(file, "};\n");
}

void compile_variant(FILE* file, LEnv* env, LVal* lval) {
	assert(lval->ltype == LCons && "plex syntax error");

	if (lval->car->ltype == LSymbol) {
		// typedef
		fprintf(
						file,
						"typedef struct %.*s {",
						lval->car->symbol.length,
						lval->car->symbol.chars
						);
		compile_variant_fields(file, env, lval);
		fprintf(file, "};\n");
	} else {
		// anonymous declaration
		fprintf(file,"struct {");
		compile_variant_fields(file, env, lval->car);
		fprintf(file, "};");
	}
}

//# compile funcall
void compile_funcall(FILE* file, LEnv* env, LString symbol, LType* ftype, LVal* lval) {
	fprintf(file, "%.*s", ftype->name.length, ftype->name.chars);
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

//# compile def
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

//# compile let
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


//# compile
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
			if (compile_builtin(file, env, lval->car->symbol, lval->cdr))
				return;
			ltype = env_lookup(env, lval->car->symbol);
			if (ltype) {
				compile_funcall(file, env, lval->car->symbol, ltype, lval->cdr);
			} else {
				print_lval(file, lval->car);
			}
			break;
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
