#ifndef HASHMAP_H
#define HASHMAP_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>

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

typedef struct HashHead {
  void* data;
  uint64_t key;
  uint8_t distance;
} HashHead;

typedef struct SymbolTable {
  HashHead* keys;
  Vector vector;
  unsigned int prime_index;
  unsigned int capacity;
} HashMap;

#endif // HASHMAP_H

#ifdef HASHMAP_IMPLEMENTATION

#endif // HASHMAP_IMPLEMENTATION
