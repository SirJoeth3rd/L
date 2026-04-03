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

(plex SymbolStack
			(arr LVal* 2048 symbols)
			(int index))

(dec analyse (SymbolStack* LEnv* LVal*) int)
(def analyse (sstack env lval)
	(match
	 lval
	 ((dec . any) (analyse_dec sstack env (cdr lval)))
	 ((plex . any) (analyse_plex sstack env (cdr lval)))
	 ((cons . cons) (do (analyse sstack env (car lval)) (analyse sstack env (cdr lval))))
	 (default (analyse (cdr lval)))))

(defmacro swap (a b) (cons b a))
(defmacro reverse (list)
	(match
	 list
	 ((a . nil) a)
	 ((a . any) (cons (reverse list) a))))
