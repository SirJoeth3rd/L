#include <stdio.h>

//stealing tsoding's arena
#include "arena/arena.h"

/*
  The //# syntax is to enable code folding 
 */

//# CONSTS
static Arena global_arena = {0};
static Arena tmp_arena = {0};
static Arena* context_arena = &default_arena;

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

//# MAIN
int main() {
  
  printf("hello world\n");
}
