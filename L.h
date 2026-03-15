#ifndef L_H
#define L_H

#include <stdint.h>
#include "src/arena.h"
#include <stdbool.h>

typedef struct {
  char* chars;
  unsigned int length;
} LString;

typedef struct {
  bool valid;
  const char* mesg;
} LErr;

typedef struct LVal LVal;
typedef struct LType LType;

struct LType {
  LString name;
  LType* parent;
  uint16_t members_len;
  LType** members;
};

struct LVal {
  enum {
    LSymbol,
    LLString,
    LNumber,
    LCons,
    LNil
  } ltype;
  union {
    struct {
      LVal* car;
      LVal* cdr;
    };
    LString symbol;
    LString string;
    long long int number;  
  };
};

typedef struct {
	LType data;
	uint64_t key;
	uint8_t distance;
} LEnvKey;

// LEnv is fundamentally just a hashmap that keeps track of all symbols types.

typedef struct LEnv {
	LEnvKey* keys;
	unsigned int prime_index;
	unsigned int capacity;
	unsigned int balance;
} LEnv;

LEnv env_init(Arena*);
LType* env_lookup(LEnv*, LString);
LType* env_put(LEnv*, LString, LType);
void env_delete(LEnv*, LString);
void env_resize(LEnv*);

// Parser 
LVal* parse(Arena*, char**); // parse to lval

// Analysis
LErr analyse_dec(Arena*, LEnv*, LVal*); 
LErr analyse(Arena*, LEnv*, LVal*); // add type info

#endif // L_H
