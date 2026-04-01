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
			 (LSymbol LString symbol)
			 (LLString LString string)
			 (LNumber int64_t number)
			 (LCons (plex ((LVal* car) (LVal* cdr))))
			 (LNil))
			(LType* type)
			(LVal* parent))
