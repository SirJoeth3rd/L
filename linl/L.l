;; An implementation of L in itself.

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
			 (LNumber '(long long int))
			 (LCons (plex (LVal* car) (LVal* cdr)))
			 (LNil nil))
			(LType* type))

(plex LEnvKey
			(LType data)
			(uint64_t key)
			(uint8_t distance))

(plex LEnv
			(LEnvKey* keys)
			((unsigned int) prime_index)
			((unsigned int) capacity)
			((unsigned int) balance))

;; declaration of parse, though implemented in parse.c
(dec parse (Arena* char**) LVal*)

;; for this first implementation we just need analyse_dec
(dec analyse (Arena* LEnv* LVal*) void)
(dec analyse_dec (Arena* LEnv* LVal*) void)

(def analyse (arena env lval)
		 (match lval
			 (('dec (cons c)) (analyse_dec c))
			 ((cons cons) (do (analyse (car lval)) (analyse (cdr lval))))
			 ((any cons) (analyse (cdr lval)))))

(defmacro set-fields (plexval fieldpairs)
	(for (int i 0) (< i (llength fieldpairs)) (++ i)
			 (match (idx fieldpairs i)
							(((lsymbol name) (lexpr val)) (yield (set (. plexval name) val))))))

(def analyse_dec (arena env lval)
		 (let
			 (LType function_type)
			 (LType* member_type)
			 (LType* params (car (cdr lval)))
			 (LType* return_param (car (cdr (cdr lval))))
			 (int param_count)
			 (int i)
			 (do
				(set-fields function_type
							(name (-> (car lval) symbol))
							(members_len (llength params))
							(members (arena_alloc arena (* param_count (sizeof LType*)))))
				 (for ((set i 0) (< i (- param_count 1)) (++ i))
							(do
							 (cond
								 (!= param->car->ltype LSymbol)
								 (lerror "Expected LType" (car param)))
							 (set member_type (env_lookup env (-> (car param) symbol)))
							 (cond (! member_type) (lerror "Could not find type" param))
							 (set function_type.members[i] member_type)
							 (set param (cdr param)))))))
