#include "../L.h"
#include <assert.h>

/*
	A macro is just a LVal* function(LVal*).
	Any function that matches this signature qualifies as a macro.
 */

LVal* car(LVal* cons) {
	assert(cons->ltype == LCons && "attempt to get car of non cons type.");
	return cons->car;
}

LVal* cdr(LVal* cons) {
	assert(cons->ltype == LCons && "attempt to get car of non cons type.");
	return cons->cdr;
}
