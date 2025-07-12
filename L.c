#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>

//stealing tsoding's arena
#define ARENA_IMPLEMENTATION
#include "./arena/arena.h"

/*
  The //# syntax is to enable code folding 
 */

//# CONSTS
static Arena global_arena = {0};
static Arena tmp_arena = {0};
static Arena* context_arena = &global_arena;

//# TYPES
typedef enum TokType {
  LBrack,
  RBrack,
  Symbol,
  Atom,
  Integer,
  Float,
  String
} TokType;

typedef struct Token {
  char* position;
  int length;
  TokType type;
} Token;

//HELPERS

void* context_alloc(size_t size) {
  assert(context_arena);
  return arena_alloc(context_arena, size);
}

//# TOKENIZATION
bool is_delim(char c) {
  switch (c) {
  case '(':
  case ')':
  case ':':
  case '\'':
  case '"':
    return true;
  }
  if (isspace(c)) {
    return true;
  }
  return false;
}

Token tokenize_symbol(char*);
Token tokenize_string(char*);
Token tokenize_number(char*);

Token* tokenize(char* expr_ptr) {
  context_arena = &tmp_arena;
  Token* tokens_ptr = context_alloc(sizeof(Token) * 100);

  while (expr_ptr) {
    if (*expr_ptr == '(') {
      *tokens_ptr = (Token){.position = expr_ptr, .type = LBrack};      
    } else if (*expr_ptr == ')') {
      *tokens_ptr = (Token){.position = expr_ptr, .type = RBrack};      
    } else if (*expr_ptr == ':') {
      expr_ptr++;
      *tokens_ptr = tokenize_symbol(expr_ptr);
      (*tokens_ptr).type = Atom;
    } else if (*expr_ptr == '"') {
      (*tokens_ptr) = tokenize_string(expr_ptr);
    } else if (isdigit(*expr_ptr)) {
      (*tokens_ptr) = tokenize_number(expr_ptr);
    }
    expr_ptr++;
  }
 

  return tokens_ptr;
}

Token tokenize_symbol(char* symbol_ptr) {
  int length = 0;
  Token token = (Token){.position = symbol_ptr, .type = Symbol};
  
  while (symbol_ptr) {
    if (is_delim(*symbol_ptr)) {
      break;
    }
    length++;
  }

  token.length = length;

  return token;
}

Token tokenize_string(char* string_ptr) {
  int length = 0;
  Token token = (Token){.position = string_ptr, .type = String};
  
  while (string_ptr) {
    if (is_delim(*string_ptr)) {
      break;
    }
    length++;
  }

  token.length = length;

  return token;
}

Token tokenize_number(char* number_ptr) {
  int length = 0;
  Token token = (Token){.position = number_ptr, .type = Integer};

  bool is_float = false;

  while (number_ptr) {
    if (isdigit(number_ptr)) {
      
    } else if (*number_ptr == '.') {
      if (is_float) {
	//found a syntax error	
      } else {
	token.type = Float;
      }
    } else {
      break;
    }
  }

  return token;
}

//# MAIN
int main() {
  printf("hello world\n");

}
