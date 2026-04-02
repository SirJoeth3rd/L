#include "../L.h"
#include "string.h"
#include <stdio.h>

/*
	Analyse is the natural place to implement header generation.
	If you don't actually want to generate a header during analysis, pass a nil FILE*.
 */

typedef struct {
	LVal* symbols[2048]; /*TODO can only support 2048 types now */
	int index;
} SymbolStack;

LErr analyse_plex(SymbolStack*, LEnv*, LVal*);
LErr analyse_dec(SymbolStack*, LEnv*, LVal*);
LErr analyse(SymbolStack*, LEnv*, LVal*);

LErr analyse_print(FILE* file, LEnv* env, LVal* lval) {
	SymbolStack symbolstack = (SymbolStack){.index = 0, .symbols = {0}};
	LString symbol;
	LType *type, *member_type;
	LType *plextype;

	plextype = env_lookup(env, TO_STRING("plex"));
	
	/*analyse*/
	analyse(&symbolstack, env, lval);

	// print structs
	fprintf(file, "/*Structure Declarations*/\n");
	for (int i = symbolstack.index - 1; i >= 0; i--) {
		symbol = symbolstack.symbols[i]->symbol;
		type = env_lookup(env, symbol);
		if (type->parent == plextype) {
			fprintf(
							file,
							"typedef struct %.*s %.*s;\n",
							symbol.length,
							symbol.chars,
							symbol.length,
							symbol.chars);
		}
	}

	fprintf(file, "\n/*Function Declarations*/\n");

	// print function declarations
	for (int i = symbolstack.index - 1; i >= 0; i--) {
		symbol = symbolstack.symbols[i]->symbol;
		type = env_lookup(env, symbol);
		if (type->parent != plextype) {
			fprintf(file, "%.*s ", type->parent->name.length, type->parent->name.chars);
			fprintf(file, "%.*s(", symbol.length, symbol.chars);

			int j = 0;
			for (;j < type->members_len - 1; j++) {
				member_type = type->members[j];
				fprintf(file, "%.*s,", member_type->name.length, member_type->name.chars);
			}
			if (type->members_len) {
				member_type = type->members[j];
				fprintf(file, "%.*s", member_type->name.length, member_type->name.chars);
			}
			fprintf(file, ");\n");
		}
	}

	fprintf(file,"\n/*End of Header Info*/\n");

	return (LErr){0};
}

LErr analyse(SymbolStack* sstack, LEnv* env, LVal* lval) {
	switch (lval->ltype) {
	case LCons:
		if (lval->car->ltype == LSymbol) {
			if (LString_cmp(lval->car->symbol, "dec")) {
				analyse_dec(sstack, env, lval->cdr);
			} else if (LString_cmp(lval->car->symbol, "plex")){
				analyse_plex(sstack, env, lval->cdr);
			}	else {
				/* TODO: analyse function call */
				analyse(sstack, env, lval->cdr);
			}
		} else {
			analyse(sstack, env, lval->car);
			analyse(sstack, env, lval->cdr);
		}
		break;
	case LSymbol:
		/* TODO: assign type */
		break;
		default:
			break;
	}
	return (LErr){0};
}

LErr assign_type(LEnv* env, LVal* lval) {
	// run up the tree
	return (LErr){0};
}

LErr analyse_plex(SymbolStack* sstack, LEnv* env, LVal* lval) {
	LType plextype;
	if (lval->car->ltype == LSymbol) {
		sstack->symbols[sstack->index] = lval->car;
		sstack->index++;

		/* TODO: members could reference the fields of the plex*/
		plextype.members = 0;
		plextype.members_len = 0;
		plextype.name = lval->car->symbol;
		plextype.parent = env_lookup(env, TO_STRING("plex"));

		env_put_global(env, lval->car->symbol, plextype);
	}

	return (LErr){0};
}

LErr analyse_dec(SymbolStack* sstack, LEnv* env, LVal* first_cons) {
  /* <f, <<int, <int, nil>>, <int, nil>>> == example setup*/
  LType function_type;
  LType* member_type;
  LVal *params, *param, *return_param, *function_name;
  int param_count, i;

  params = first_cons->cdr->car;
  param = params;
  return_param = first_cons->cdr->cdr->car;
  function_name = first_cons->car;

	sstack->symbols[sstack->index] = function_name;
	sstack->index++;

  param_count = Llist_length(params);
  
  function_type.name = function_name->symbol;
  function_type.members_len = param_count;
  function_type.members = arena_alloc(env->arena, param_count*sizeof(LType*));

  for (i = 0; i < param_count - 1; i++) {
    if (param->car->ltype != LSymbol) { /* TODO: error*/
      printf("Expected type LSymbol but got ");
      print_ltype(param->car);
      printf("\n");
    }

		member_type = env_lookup(env, param->car->symbol);

    if (!member_type) {
      printf("Could not find type %.*s\n", param->symbol.length, param->symbol.chars);
    }

    function_type.members[i] = member_type;
    param = param->cdr;
  }
  
  if (param->ltype == LCons && param->car->ltype == LSymbol) {
		member_type = env_lookup(env, param->car->symbol);
    function_type.members[param_count-1] = member_type;
  }/* TODO: else error*/
  

  LType* return_type = env_lookup(env, return_param->symbol);

  if (!return_type) {
    printf("Could not find type %.*s\n", param->symbol.length, param->symbol.chars);
  } else {
    function_type.parent = return_type;
  }

	env_put_global(env, function_type.name, function_type);
  return (LErr){0};
}
