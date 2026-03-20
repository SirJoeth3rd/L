#include "../L.h"
#include <ctype.h>
#include <stdlib.h>

/*
	A problem. How to represent the empty list? ().
	Some options.
	1. Nil in car position -> empty list.
	  (dec f () int) => <dec, <f, <nil, <int, nil>>>>
		                           _ -> nil in car implies empty list.
															 nil in cdr implies end of list.
		(concat () ()) => <concat, <nil, <nil, nil>>>

	Actually this looks like it will work let's try it out. 
	
 */

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
      /* if not anything else it's a symbol*/
      lval->car = parse_symbol(arena, chr);
    }
    
    /* append the new <value_here, next cons>*/
  post_symbol:
    lval->cdr = (LVal*)arena_alloc(arena, sizeof(*lval));
    lval = lval->cdr;
    lval->ltype = LCons;
  }

  /* getting here -> reached end of input -> current lval = Nil*/
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
  int length = 0;
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
  lval->ltype = LLString;
  lval->string = (LString) {
    .chars = starting_position,
    .length = length
  };
  return lval;
}

LVal* parse_symbol(Arena* arena, char** chr) {
  /*TODO: could be a litte more advanced*/
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
  lval->symbol = (LString) {
    .chars = start_position,
    .length = length
  };

  return lval;
}
