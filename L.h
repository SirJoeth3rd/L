#ifndef L_H
#define L_H

#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stdio.h>
#include "src/arena.h"
#include "src/lstring.h"

typedef struct {
  bool valid;
  const char* mesg;
} LErr;

typedef struct LVal LVal;
typedef struct LType LType;
typedef struct LEnv LEnv;
typedef struct LEnvStack LEnvStack;

struct LType {
  LString name;
  LType* parent;
  uint16_t members_len;
  LType** members;
	enum {
		LBase, /* base type like int */
		LPlex, /* a product type */
		LEnum, /* a sum type */
		LFunc  /* a function type */
	} type_kind;
};

/*
	I'm not exactly concerned about the performance charecteristics
	of the precompilation lvals etc. The purpose of these are just
	to compile the real deal. 
 */

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
	LType* type;
	LVal* parent;
};

typedef struct {
	LType data;
	uint64_t key;
	uint8_t distance;
} LEnvKey;

/* LEnv is fundamentally just a hashmap that keeps track of all symbols types. */

struct LEnv {
	/* stack values used to keep track of scoped variables */
	uint64_t stack[2048];
	unsigned int stack_index;

	/* the core hashmap */
	LEnvKey* keys;
	unsigned int prime_index;
	unsigned int capacity;
	unsigned int balance;
};

/* Environment */
LEnv env_init(Arena*);
LType* env_lookup(LEnv*, LString);
void env_delete(LEnv*, LString);
void env_resize(LEnv*);
void env_push_scope(LEnv*);
void env_pop_scope(LEnv*);
LType* env_put_global(LEnv*, LString, LType);
LType* env_put_local(LEnv*, LString, LType);

/* Helpers */
void print_ltype(LVal*);
LVal cons(LVal*, LVal*);
void recur_print(LVal*);
void pprint(LVal*, int);
int Llist_length(LVal*);

/* Parser */
LVal* parse(Arena*, char**); /* parse to lval*/

/* Analysis */
LErr analyse(Arena*, LEnv*, LVal*); /* add type info*/

/* Code Gen */
void compile(FILE*, LEnv*, LVal*);

#endif /* L_H */
