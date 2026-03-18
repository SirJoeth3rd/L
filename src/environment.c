#include "../L.h"
#include <stdlib.h>
#include <stdint.h>

/*
	Symbol table implemented as a robin hood hashmap.
	The Environment stores a tree of types, each type has a parent type
	to which that type can be casted (at the cost of losing information)
	For example int* -> int, and everything to nil including itself.

	TODO: LEnv needs to be a hierarchical hashmap because we need to be able
	to store typeinformation inside lets and function definitions.
 */

#define MAX_PRIME_INDEX 20

static const unsigned int PRIMES[MAX_PRIME_INDEX + 1] = {
  1543,
  3079,
  6151,
  12289,
  24593,
  49157,
  98317,
  196613,
  393241,
  786433,
  1572869,
  3145739,
  6291469,
  12582917,
  25165843,
  50331653,
  100663319,
  201326611,
  402653189,
  805306457,
  1610612741
};


/* ai function here */
static uint64_t string_hash(const char* string, size_t length, uint64_t seed) {
    uint64_t h1, h2;
    size_t i;
    
    /* Initial hash values - mixed with seed */
    h1 = seed + 0x9e3779b97f4a7c15ULL;  /* Golden ratio constant */
    h2 = seed - 0xbf58476d1ce4e5b9ULL;
    
    /* Process 8 bytes at a time */
    i = 0;
    while (i + 8 <= length) {
        uint64_t chunk;
        
        /* Portable byte reading (endian-independent) */
        chunk = ((uint64_t)(unsigned char)string[i]) |
                ((uint64_t)(unsigned char)string[i+1] << 8) |
                ((uint64_t)(unsigned char)string[i+2] << 16) |
                ((uint64_t)(unsigned char)string[i+3] << 24) |
                ((uint64_t)(unsigned char)string[i+4] << 32) |
                ((uint64_t)(unsigned char)string[i+5] << 40) |
                ((uint64_t)(unsigned char)string[i+6] << 48) |
                ((uint64_t)(unsigned char)string[i+7] << 56);
        
        /* Mix chunk into hash */
        h1 ^= chunk;
        h1 *= 0x9e3779b97f4a7c15ULL;
        h1 ^= h1 >> 31;
        
        h2 += h1;
        
        i += 8;
    }
    
    /* Process remaining bytes */
    if (i < length) {
        uint64_t chunk = 0;
        size_t remaining = length - i;
        size_t j;
        
        for (j = 0; j < remaining; j++) {
            chunk |= ((uint64_t)(unsigned char)string[i + j] << (j * 8));
        }
        
        h1 ^= chunk;
        h1 *= 0x9e3779b97f4a7c15ULL;
        h1 ^= h1 >> 31;
        
        h2 += h1;
    }
    
    /* Final mixing */
    h1 += h2;
    h2 += h1;
    
    h1 ^= h1 >> 33;
    h1 *= 0xff51afd7ed558ccdULL;
    h1 ^= h1 >> 33;
    h1 *= 0xc4ceb9fe1a85ec53ULL;
    h1 ^= h1 >> 33;
    
    h2 ^= h2 >> 33;
    h2 *= 0xff51afd7ed558ccdULL;
    h2 ^= h2 >> 33;
    h2 *= 0xc4ceb9fe1a85ec53ULL;
    h2 ^= h2 >> 33;
    
    h1 += h2;
    
    return h1;
}

uint64_t LString_hash(LString str) {
	return string_hash(str.chars, str.length, 0);
}

LType* env_lookup(LEnv* env, LString name) {
	int distance = 0;
	uint64_t key = LString_hash(name);
  int index = key % env->capacity;

  while (env->keys[index].distance) {
    if (env->keys[index].key == key) {
      return &env->keys[index].data;
    }
    if (env->keys[index].distance < distance) {
      return NULL;
    }
    distance++;
    index = (index + 1) % env->capacity; /* rap around*/
  }
  return NULL;
}

LType* env_put_key(LEnv *env, uint64_t key, LType val) {
	LEnvKey tmp, curr;
	int index;

	curr.distance = 1;
	curr.data = val;
	curr.key = key;

	if (env->balance > env->capacity * 0.75) {
    env_resize(env);
  }

	index = key % env->capacity;

	while (env->keys[index].distance && env->keys[index].key != key) {
    if (env->keys[index].distance > curr.distance) {
      tmp = env->keys[index];
      env->keys[index] = curr;
      curr = tmp;
    }
    curr.distance++;
    index = (index + 1) % env->capacity;
  }
	env->keys[index] = curr;
	env->balance++;
	return &env->keys[index].data;
}

LType* env_put(LEnv* env, LString name, LType val) {
	return env_put_key(env, LString_hash(name), val);
}

void env_resize(LEnv* env) {
	LEnvKey* oldkeys;
  unsigned int oldlength;
  oldkeys = env->keys;
  oldlength = env->capacity;
  
  env->prime_index = env->prime_index + 1;
  env->capacity = PRIMES[env->prime_index];
  env->keys = calloc(env->capacity, sizeof(LEnvKey));

	unsigned int i;
  for (i = 0; i < oldlength; i++) {
    if (oldkeys[i].distance) {
      env_put_key(env, oldkeys[i].key, oldkeys[i].data);
    }
  }

  free(oldkeys);
}

void env_delete(LEnv* env, LString name) {
  int distance = 0;
	uint64_t key = LString_hash(name);
  int index = key % env->capacity;

  while (env->keys[index].distance) {
    if (env->keys[index].key == key) {
      env->keys[index] = (LEnvKey){0};
			unsigned int i;
      for (i = index; i+1 < env->capacity && env->keys[i+1].distance; i++) {
				if (env->keys[i+1].distance == 0) {
					break;
				} else {
					env->keys[i] = env->keys[i+1];
					env->keys[i].distance--;
				}
      }
    }
    
    if (env->keys[index].distance < distance) {
      break;
    }
    
    distance++;
    index = (index + 1) % env->capacity;
  }
}

#define INT "int"
#define CHAR "char"
#define BOOL "bool"
#define NIL "nil"

LEnv env_init(Arena* arena) {
  LEnv env;
  LType *nil_ptr;
  LType new_type;
  LType *char_type, *char_ptr_type, *int_type;

	env.prime_index = 0;
  env.capacity = PRIMES[env.prime_index];
  env.keys = calloc(env.capacity, sizeof(LType));
	env.balance = 0;

  new_type.name = (LString){.chars = NIL, .length=sizeof(NIL)-1};
  new_type.members_len = 0;
  nil_ptr = (LType*)env_put(&env, new_type.name, new_type);
  nil_ptr->parent = nil_ptr;

  new_type.name = (LString){.chars = INT, .length=sizeof(INT)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  int_type = env_put(&env, new_type.name, new_type);

  new_type.name = (LString){.chars = CHAR, .length=sizeof(CHAR)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  char_type = env_put(&env, new_type.name, new_type);

  new_type.name = (LString){.chars = BOOL, .length=sizeof(BOOL)-1};
  new_type.members_len = 0;
  new_type.parent = nil_ptr;
  env_put(&env, new_type.name, new_type);

  /* technically we can have char*********; so this should actually be generalized and the*/
  /* types for pointers should only be added during the analysis phase. (except maybe the first pointer type)*/

  new_type.name = (LString){.chars = "char*", .length=(sizeof("char*")-1)};
  new_type.parent = char_type;
  new_type.members_len = 1;
  new_type.members = arena_alloc(arena, sizeof(LType*));
  *new_type.members = int_type;
  char_ptr_type = env_put(&env, new_type.name, new_type);

  new_type.name = (LString){.chars = "char**", .length=(sizeof("char**")-1)};
  new_type.parent = char_ptr_type;
  new_type.members_len = 1;
  new_type.members = arena_alloc(arena, sizeof(LType*));
  *new_type.members = int_type;
  env_put(&env, new_type.name, new_type);

  /* XXX: other base types*/

  return env;
}
