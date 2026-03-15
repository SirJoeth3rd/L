#include "../L.h"

LErr analyse(Arena* arena, LEnv* env, LVal* lval) { // (dec f (int int) int))
  while (lval && lval->ltype != LNil) {
    if (lval->ltype == LCons) {
      if (lval->car->ltype == LCons) {
				if (lval->car->car->ltype == LSymbol) {
					if (LString_cmp(lval->car->car->symbol, "dec")) {
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
