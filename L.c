#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>

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

typedef struct TokenizationResult {
  Token* tokens;
  bool success;
} TokenizationResult;

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

Token tokenize_symbol(char**);
Token tokenize_string(char**);
Token tokenize_number(char**);

TokenizationResult tokenize(char* expr_ptr) {
  context_arena = &tmp_arena;
  Token* tokens_ptr = context_alloc(sizeof(Token) * 100);

  while (*expr_ptr) {
    if (*expr_ptr == '(') {
      printf("lbrack\n");
      *tokens_ptr = (Token){.position = expr_ptr, .type = LBrack};
      tokens_ptr++;
    } else if (*expr_ptr == ')') {
      printf("rbrack\n");
      *tokens_ptr = (Token){.position = expr_ptr, .type = RBrack};
      tokens_ptr++;
    } else if (*expr_ptr == ':') {
      printf("atom\n");
      expr_ptr++;
      *tokens_ptr = tokenize_symbol(&expr_ptr);
      (*tokens_ptr).type = Atom;
      tokens_ptr++;
    } else if (*expr_ptr == '"') {
      printf("string\n");
      (*tokens_ptr) = tokenize_string(&expr_ptr);
      tokens_ptr++;
    } else if (isdigit(*expr_ptr)) {
      printf("digit\n");
      (*tokens_ptr) = tokenize_number(&expr_ptr);
      tokens_ptr++;
    } else if (!isspace(*expr_ptr)) {
      printf("symbol\n");
      (*tokens_ptr) = tokenize_symbol(&expr_ptr);
      tokens_ptr++;
    }
    printf("++\n");
    expr_ptr++;
  }
 
  return (TokenizationResult){.tokens = tokens_ptr, .success = true};
}

Token tokenize_symbol(char** symbol_ptr_ptr) {
  char* symbol_ptr = *symbol_ptr_ptr;
  int length = 0;
  Token token = (Token){.position = symbol_ptr, .type = Symbol};
  
  while (symbol_ptr) {
    if (is_delim(*symbol_ptr)) {
      break;
    }
    length++;
    symbol_ptr++;
  }

  token.length = length;

  symbol_ptr--;
  *symbol_ptr_ptr = symbol_ptr;
  return token;
}

Token tokenize_string(char** string_ptr_ptr) {
  char* string_ptr = *string_ptr_ptr;
  int length = 0;
  Token token = (Token){.position = string_ptr, .type = String};
  
  while (string_ptr) {
    if (*string_ptr == '"') {
      break;
    } else if (*string_ptr == '\\') {
      string_ptr += 2; // just ignore the next char for now
      length += 2;
    } else {
      string_ptr++;
      length++;
    }
  }

  token.length = length;

  string_ptr--;
  *string_ptr_ptr = string_ptr;
  return token;
}

Token tokenize_number(char** number_ptr_ptr) {
  char* number_ptr = *number_ptr_ptr;
  int length = 0;
  Token token = (Token){.position = number_ptr, .type = Integer};

  bool is_float = false;

  while (number_ptr) {
    if (isdigit(*number_ptr)) {
      length++;
      number_ptr++;
    } else if (*number_ptr == '.') {
      if (is_float) {
	//TODO: handle syntax error	
      } else {
	token.type = Float;
	length++;
	number_ptr++;
      }
    } else {
      break;
    }
  }

  number_ptr--;
  *number_ptr_ptr = number_ptr;
  token.length = length;
  return token;
}

//# MAIN
int main() {
  tokenize("(+ 1 2 3 :atom)");
}
