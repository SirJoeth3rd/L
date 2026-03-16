/*
stb style library for the concept of a allocator.
For now this is just internal to the L project, so not using prefixes etc.
 */

#include <stddef.h>

#ifndef ARENA_H
#define ARENA_H

/*
	The Allocator struct can be used for data structures that are meant
	to be allocator agnostic.  
 */

typedef struct {
	void* self;
	void* (*allocate)(void* self, size_t items);
	void (*free)(void* self);
} Allocator;

/*
	The Arena is a specific implementation of allocator.
	Originally stolen from Tsoding and then tortured.
 */

typedef struct Region Region;

struct Region {
	void* buffer;
	size_t balance;  //bytes already used
	size_t capacity; //total bytes
	Region *next;
};

typedef struct {
	Region *start;
	Region *end;
} Arena;

void arena_append_region(Arena*,size_t);
void* arena_alloc(Arena*, size_t);
void arena_free(Arena*);
Arena arena_init();

#define PAGE_SIZE 4096

#endif //ARENA_H

#ifdef ARENA_IMPLEMENTATION

void* arena_alloc(Arena* arena, size_t bytes) {
  void* ptr;

	if (bytes > PAGE_SIZE) {
		arena_append_region(arena, bytes);
	} else {
		arena_append_region(arena, PAGE_SIZE);
	}

  ptr = arena->end->buffer + arena->end->balance;
  arena->end->balance += bytes;
  return ptr;
}

void* arena_alloc_val(Arena* arena, size_t bytes, void* initial) {
  void* ptr;
  ptr = arena_alloc(arena, bytes);

  memcpy(ptr, initial, bytes);
  return ptr;
}

void arena_append_region(Arena* arena, size_t bytes) {
  Region* new_region;
  new_region = calloc(1, sizeof(*new_region));
  arena->end->next = new_region;
  arena->end = new_region;
  arena->end->buffer = malloc(bytes*sizeof(char));
  arena->end->balance = 0;
  arena->end->capacity = bytes;
}

void arena_free(Arena* arena) {
  Region* current_region, *next_region;
  current_region = arena->start;
  while (current_region) {
    free(current_region->buffer);
    next_region = current_region->next;
    free(current_region);
    current_region = next_region;
  }
}

Arena arena_init() {
  Arena arena;
  Region* first;
  first = calloc(1, sizeof(*first));
  first->buffer = malloc(PAGE_SIZE*sizeof(char));
  first->capacity = PAGE_SIZE;
  first->balance = 0;
  arena.start = first;
  arena.end = first;
  return arena;
}

/*
	Below is actually mostly just demonstration.
	It shows how to create a allocator object from an arena.
 */

void* arena_alloc_(void* arena, size_t items) {
	return arena_alloc((Arena*) arena, items);
}

void arena_free_(void* arena) {
	arena_free((Arena*) arena);
}

Allocator arena_allocator(Arena *arena) {
	return (Allocator){
		.self = (void*)arena,
		.allocate = arena_alloc_,
		.free = arena_free_,
	};
}

#endif
