#ifndef L_STRING_H
#define L_STRING_H

#include "arena.h"
#include "string.h"
#include <stdbool.h>
#include <assert.h>

#define TO_STRING(charbuf) (LString){.chars = charbuf, .length = strlen(charbuf)}

typedef struct {
  char* chars;
  unsigned int length;
} LString;

/* TODO
   make this library arena independent, or use an internal arena
   but perhaps the user does not want to use arena's.
*/

char* LString_cstring(Arena*, LString);
LString String_from_cstring(char*);
bool LString_cmp(LString, const char*);

#endif // L_STRING_H

#ifdef L_STRING_IMPLEMENTATION

char* LString_cstring(Arena* arena, LString str) {
  char* tmp_buffer;
  if (arena) {
    tmp_buffer = (char*) arena_alloc(arena, sizeof(char)*(str.length+1));
  } else {
#if defined(NDEBUG)
    tmp_buffer = (char*) malloc(sizeof(char)*(str.length+1));
#else
    assert(("LString_cstring requires arena to put chars in", arena));
#endif
  }
  memcpy(tmp_buffer, str.chars, str.length);
  tmp_buffer[str.length] = '\0';
  return tmp_buffer;
}

LString String_from_cstring(char* chars) {
  LString str;
  str.chars = chars;
  str.length = strlen(chars);
  return str;
}

bool LString_cmp(LString str1, const char* cstr) {
  unsigned int i;
  for (i = 0; i < str1.length; i++) {
    if (cstr[i] == '\0') {
      return false;
    }
    if (str1.chars[i] != cstr[i]) {
      return false;
    }
  }
  return true;
}

void LString_cpy_out(LString str, char* buffer) {
  memcpy(buffer, str.chars, str.length);
}

LString LString_cpy_in(LString str, char* buffer) {
  unsigned int buffer_len;
  buffer_len = strlen(buffer);
  if (buffer_len > str.length) {
    // TODO: error here
  }
  strcpy(str.chars, buffer);
  return str;
}

LString LString_append(LString str, LString app) {
  //TODO: fix (if my underlying chars is empty on the end just use that instead)
  LString new_string;
  new_string.length = str.length + app.length;
  new_string.chars = malloc(sizeof(char)*new_string.length);
  strncpy(new_string.chars, str.chars, str.length);
  strncpy(&new_string.chars[str.length], app.chars, app.length);
  return new_string;
}

LString LString_subslice(LString str, int start, int end) {
  LString new_string;
  //TODO: dew it
}

#endif // L_LString
