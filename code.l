(plex LString
			(char* chars)
			(int length))

(plex LType
			(LString name)
			(LType* parent)
			(uint64_t members_len)
			(LType** members))

(plex LVal
			(variant
			 (LSymbol LString)
			 (LLString LString)
			 (LNumber int64_t)
			 (LCons (plex ((LVal* car) (LVal* cdr))))
			 (LNil nil))
			(LType* type)
			(LVal* parent))

(dec f (int int) int)
