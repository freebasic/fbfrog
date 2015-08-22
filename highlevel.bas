'' Higher level code transformations

#include once "fbfrog.bi"

declare sub expandTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)

namespace hl
	dim shared api as ApiInfo ptr
	dim shared symbols as THash ptr

	'' Used by forward declaration addition pass
	'' data = hl.types array index
	dim shared typehash as THash ptr

	'' data = TEXT ASTNODE holding the new id/alias
	dim shared renamejobs as THash ptr

	'' Used by forward declaration addition pass
	type TYPENODE
		id		as zstring ptr  '' Type name
		definition	as ASTNODE ptr
		forwarduse	as integer
		firstuse	as ASTNODE ptr
	end type
	dim shared types as TYPENODE ptr
	dim shared as integer typecount, typeroom
	dim shared as ASTNODE ptr currentdecl

	'' Used by Extern block addition pass
	dim shared as integer need_extern, stdcalls, cdecls
	dim shared as integer mostusedcallconv

	'' Used by dtype use determination passes
	dim shared as integer uses_clong, uses_clongdouble
end namespace

private function hlLookupSymbolType(byval id as zstring ptr) as integer
	function = cint(hl.symbols->lookupDataOrNull(id))
end function

private sub hlCollectSymbol(byval id as zstring ptr, byval dtype as integer)
	select case dtype
	case TYPE_UDT, TYPE_PROC
		'' Don't bother tracking complex types - not worth it yet
		dtype = TYPE_NONE
	end select
	hl.symbols->addOverwrite(id, cptr(any ptr, dtype))
end sub

private function typeIsNumeric(byval dtype as integer) as integer
	select case dtype
	case TYPE_BYTE, TYPE_UBYTE, TYPE_SHORT, TYPE_USHORT, _
	     TYPE_LONG, TYPE_ULONG, TYPE_CLONG, TYPE_CULONG, _
	     TYPE_INTEGER, TYPE_UINTEGER, TYPE_LONGINT, TYPE_ULONGINT, _
	     TYPE_SINGLE, TYPE_DOUBLE
		function = TRUE
	end select
end function

'' Determine C UOP/BOP result dtype based on the operands' dtypes
'' Note: assignment BOPs excluded, we can't easily handle those for the purposes
'' of hlAddMathCasts() anyways, as the result type depends on the lhs, whose
'' type we can typically not determine...
private function typeCBop(byval astclass as integer, byval a as integer, byval b as integer) as integer
	'' Logic/relational operations: result always is a 32bit int
	select case as const astclass
	case ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, _
	     ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_CLOGNOT, ASTCLASS_CDEFINED
		return TYPE_LONG

	'' sizeof() always returns size_t
	case ASTCLASS_SIZEOF
		return TYPE_UINTEGER

	'' Bitshifts: Ignore the rhs type
	case ASTCLASS_SHL, ASTCLASS_SHR
		b = a

	case ASTCLASS_IIF, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_ADD, ASTCLASS_SUB, ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS _

	case else
		assert(FALSE)
	end select

	'' iif/math/bitwise operations: depends on operands

	'' Pointer arithmetic: don't bother tracking, doesn't seem necessary yet
	'' (preserving the ptr type would require preserving the subtype too)
	if (typeGetPtrCount(a) > 0) or (typeGetPtrCount(b) > 0) then
		return TYPE_NONE
	end if

	if (not typeIsNumeric(a)) or (not typeIsNumeric(b)) then
		return TYPE_NONE
	end if

	'' Floats have precedence over integers
	if (a = TYPE_DOUBLE) or (b = TYPE_DOUBLE) then return TYPE_DOUBLE
	if (a = TYPE_SINGLE) or (b = TYPE_SINGLE) then return TYPE_SINGLE

	'' Promote byte/short to int32 (this always happens, both on 32bit and 64bit)
	if a < TYPE_LONG then a = TYPE_LONG
	if b < TYPE_LONG then b = TYPE_LONG

	if a = b then return a

	#macro hCheckBop(aa, bb, result)
		if ((a = aa) and (b = bb)) or _
		   ((a = bb) and (b = aa)) then
			return result
		end if
	#endmacro

	'' If the operation involves a 64bit operand, then it's 64bit,
	'' otherwise it's only 32bit (this happens regardless of the
	'' pointer size).
	''
	'' This is easy with fixed-size types like int and long long,
	'' because we can immediately tell whether the operation is
	'' 32bit or 64bit. But we have to take care for types such as
	'' size_t which depend on the pointer size.
	''
	'' Example 1:
	''    ssize_t   + long long => ?         (source)
	''    int       + long long => long long (32bit)
	''    long long + long long => long long (64bit)
	'' Easy: since the rhs is always >= ssize_t, we can just use the
	'' rhs as the operation's result type.
	''
	'' Example 2:
	''    ssize_t   + int => ?         (source)
	''    int       + int => int       (32bit)
	''    long long + int => long long (64bit)
	'' Easy: we can just ssize_t as the result type.
	''
	'' Example 3:
	''    size_t + int64 => ?      (source)
	''    uint32 + int64 => int64  (32bit)
	''    uint64 + int64 => uint64 (64bit)
	'' Impossible: we know that the result will always be 64bit,
	'' but it can be either signed or unsigned.
	''

	'' unsigned long long (= ulongint = uint64) is >= all other
	'' types, so it's always safe to use
	hCheckBop(TYPE_ULONGINT, TYPE_LONGINT , TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_UINTEGER, TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_INTEGER , TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_CULONG  , TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_CLONG   , TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_ULONG   , TYPE_ULONGINT)
	hCheckBop(TYPE_ULONGINT, TYPE_LONG    , TYPE_ULONGINT)

	'' long long (= longint = int64) is < uint64 types, but >=
	'' anything else
	hCheckBop(TYPE_LONGINT, TYPE_INTEGER, TYPE_LONGINT)
	hCheckBop(TYPE_LONGINT, TYPE_CLONG  , TYPE_LONGINT)
	hCheckBop(TYPE_LONGINT, TYPE_ULONG  , TYPE_LONGINT)
	hCheckBop(TYPE_LONGINT, TYPE_LONG   , TYPE_LONGINT)

	'' size_t (= uinteger) is < [u]int64 (because of 32bit
	'' platforms), but >= anything else. In particular, we can be
	'' sure that it's > ssize_t and >= [unsigned] long.
	hCheckBop(TYPE_UINTEGER, TYPE_INTEGER, TYPE_UINTEGER)
	hCheckBop(TYPE_UINTEGER, TYPE_CULONG , TYPE_UINTEGER)
	hCheckBop(TYPE_UINTEGER, TYPE_CLONG  , TYPE_UINTEGER)
	hCheckBop(TYPE_UINTEGER, TYPE_ULONG  , TYPE_UINTEGER)
	hCheckBop(TYPE_UINTEGER, TYPE_LONG   , TYPE_UINTEGER)

	'' ssize_t (integer) is < [u]int64 (because of 32bit platforms)
	'' and < [unsigned] long (because of linux-x86_64), but >=
	'' anything else.
	hCheckBop(TYPE_INTEGER, TYPE_CLONG , TYPE_INTEGER)
	hCheckBop(TYPE_INTEGER, TYPE_LONG  , TYPE_INTEGER)

	'' unsigned long (culong) is always at least uint32, but it's
	'' different from size_t due to win64.
	hCheckBop(TYPE_CULONG, TYPE_ULONG, TYPE_CULONG)
	hCheckBop(TYPE_CULONG, TYPE_CLONG, TYPE_CULONG)
	hCheckBop(TYPE_CULONG, TYPE_LONG , TYPE_CULONG)

	'' long (clong) is always at least int32, but it's different
	'' from ssize_t due to win64.
	hCheckBop(TYPE_CLONG, TYPE_LONG, TYPE_CLONG)

	'' unsigned int (ulong) > int (long)
	hCheckBop(TYPE_ULONG, TYPE_LONG, TYPE_ULONG)

	function = TYPE_NONE
end function

private sub hlCalculateCTypes(byval n as ASTNODE ptr)
	'' No types should be set yet, except for type casts and atoms
	#if __FB_DEBUG__
		select case as const n->class
		case ASTCLASS_CAST, ASTCLASS_DATATYPE, _
		     ASTCLASS_CONSTI, ASTCLASS_CONSTF, _
		     ASTCLASS_STRING, ASTCLASS_CHAR
			assert(n->dtype <> TYPE_NONE)

		case ASTCLASS_IIF, ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
		    ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
		    ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, ASTCLASS_SHL, ASTCLASS_SHR, _
		    ASTCLASS_ADD, ASTCLASS_SUB, ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
		    ASTCLASS_CLOGNOT, ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS, _
		    ASTCLASS_SIZEOF, ASTCLASS_CDEFINED, ASTCLASS_TEXT
			assert(n->dtype = TYPE_NONE)

		end select
	#endif

	scope
		var i = n->head
		while i
			hlCalculateCTypes(i)
			i = i->next
		wend
	end scope

	select case as const n->class
	case ASTCLASS_TEXT
		n->dtype = hlLookupSymbolType(n->text)

	case ASTCLASS_CDEFINED
		n->dtype = TYPE_LONG

	case ASTCLASS_IIF, ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_CLOGNOT, ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS, _
	     ASTCLASS_SIZEOF
		'' IIF and BOPs will have two operands (ldtype/rdtype will be different),
		'' UOPs will have one operand (i.e. ldtype/rdtype will be the same)
		var ldtype = n->head->dtype
		var rdtype = n->tail->dtype
		n->dtype = typeCBop(n->class, ldtype, rdtype)
	end select
end sub

private function hIsUlongCast(byval n as ASTNODE ptr) as integer
	function = n andalso (n->class = ASTCLASS_CAST) andalso (n->dtype = TYPE_ULONG)
end function

''
'' Wrap all 32bit unsigned int IIFs/BOPs/UOPs in culng()/clng() casts, in order
'' to make sure the result is truncated to 32bit properly in FB even on 64bit,
'' where FB will do 64bit arithmetic.
''
'' For example, in C:
''    (0u - 100u)   =>    0xFFFFFF9Cu
'' but in FB:
''    (0ul - 100ul) =>            &hFFFFFF9Cu (32bit)
''    (0ul - 100ul) =>    &hFFFFFFFFFFFFFF9Cu (64bit)
'' and &hFFFFFFFFFFFFFF9Cu <> &hFFFFFF9Cu (no sign extension due to unsigned).
'' In order to make sure we always get &hFFFFFF9Cu in FB, we have to truncate
'' the operation's result to 32bit explicitly.
''
private function hlAddMathCasts(byval parent as ASTNODE ptr, byval n as ASTNODE ptr) as ASTNODE ptr
	var i = n->head
	while i
		i = astReplace(n, i, hlAddMathCasts(n, astClone(i)))
	wend

	select case as const n->class
	case ASTCLASS_IIF, ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
	     ASTCLASS_CLOGNOT, ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS, _
	     ASTCLASS_SIZEOF
		'' Wrap uint32 operation in a cast, unless there already is a cast
		'' TODO: don't add cast if n is a cast
		if (n->dtype = TYPE_ULONG) and (not hIsUlongCast(parent)) then
			n = astNew(ASTCLASS_CAST, n)
			n->dtype = TYPE_ULONG
		end if
	end select

	function = n
end function

private function astOpsC2FB(byval n as ASTNODE ptr, byval is_bool_context as integer) as ASTNODE ptr
	select case n->class
	case ASTCLASS_CLOGNOT, ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_IIF
		var l_is_bool_context = FALSE
		var r_is_bool_context = FALSE

		select case n->class
		'' ! operand is treated as bool
		case ASTCLASS_CLOGNOT
			l_is_bool_context = TRUE

		'' andalso/orelse operands are always treated as bools
		case ASTCLASS_CLOGOR, ASTCLASS_CLOGAND
			l_is_bool_context = TRUE
			r_is_bool_context = TRUE

		'' BOP operands may sometimes be in bool context:
		''    x = 0
		''    x <> 0
		'' (not x = -1 and x <> -1 because if checks check against 0 and <> 0,
		'' not 0 and -1)
		case ASTCLASS_CEQ, ASTCLASS_CNE
			if astIsCONSTI(n->tail) then
				l_is_bool_context = (astEvalConstiAsInt64(n->tail) = 0)
			end if

		'' iif() condition always is treated as bool
		case ASTCLASS_IIF
			n->expr = astOpsC2FB(n->expr, TRUE)
		end select

		astReplace(n, n->head, astOpsC2FB(astClone(n->head), l_is_bool_context))
		if n->head <> n->tail then
			assert(n->head->next = n->tail)
			astReplace(n, n->tail, astOpsC2FB(astClone(n->tail), r_is_bool_context))
		end if

	case else
		var i = n->head
		while i
			i = astReplace(n, i, astOpsC2FB(astClone(i), FALSE))
		wend
	end select

	select case n->class
	case ASTCLASS_CLOGNOT, ASTCLASS_CDEFINED, _
	     ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
	     ASTCLASS_CEQ, ASTCLASS_CNE, _
	     ASTCLASS_CLT, ASTCLASS_CLE, _
	     ASTCLASS_CGT, ASTCLASS_CGE

		select case n->class
		case ASTCLASS_CLOGNOT
			'' Turn C's "!x" into FB's "x = 0"
			n->class = ASTCLASS_EQ
			var zero = astNew(ASTCLASS_CONSTI, "0")
			astSetType(zero, TYPE_LONG, NULL)
			astAppend(n, zero)
		case ASTCLASS_CDEFINED : n->class = ASTCLASS_DEFINED
		case ASTCLASS_CLOGOR   : n->class = ASTCLASS_LOGOR
		case ASTCLASS_CLOGAND  : n->class = ASTCLASS_LOGAND
		case ASTCLASS_CEQ      : n->class = ASTCLASS_EQ
		case ASTCLASS_CNE      : n->class = ASTCLASS_NE
		case ASTCLASS_CLT      : n->class = ASTCLASS_LT
		case ASTCLASS_CLE      : n->class = ASTCLASS_LE
		case ASTCLASS_CGT      : n->class = ASTCLASS_GT
		case ASTCLASS_CGE      : n->class = ASTCLASS_GE
		case else              : assert(FALSE)
		end select

		'' Turn -1|0 into 1|0 if the value may be used in math calculations etc.
		if is_bool_context = FALSE then
			n = astNew(ASTCLASS_NEGATE, n)
		end if
	end select

	function = n
end function

private sub hlRemoveExpressionTypes(byval n as ASTNODE ptr)
	var i = n->head
	while i
		hlRemoveExpressionTypes(i)
		i = i->next
	wend

	if n->class <> ASTCLASS_CAST then
		select case as const n->class
		case ASTCLASS_IIF, ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
		     ASTCLASS_EQ, ASTCLASS_NE, ASTCLASS_LT, ASTCLASS_LE, ASTCLASS_GT, ASTCLASS_GE, _
		     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, ASTCLASS_SHL, ASTCLASS_SHR, _
		     ASTCLASS_ADD, ASTCLASS_SUB, ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD, _
		     ASTCLASS_NOT, ASTCLASS_NEGATE, ASTCLASS_UNARYPLUS, _
		     ASTCLASS_SIZEOF, ASTCLASS_DEFINED, ASTCLASS_TEXT
			astSetType(n, TYPE_NONE, NULL)

		case ASTCLASS_CLOGOR, ASTCLASS_CLOGAND, _
		     ASTCLASS_CEQ, ASTCLASS_CNE, ASTCLASS_CLT, ASTCLASS_CLE, ASTCLASS_CGT, ASTCLASS_CGE, _
		     ASTCLASS_CLOGNOT, ASTCLASS_CDEFINED
			assert(FALSE)
		end select
	end if
end sub

private function hlFixExpression(byval n as ASTNODE ptr, byval is_bool_context as integer) as ASTNODE ptr
	hlCalculateCTypes(n)

	n = hlAddMathCasts(NULL, n)

	n = astOpsC2FB(n, is_bool_context)

	'' Forget about types again, for better merging (on one side
	'' an identifier may have unknown type, on the other side it
	'' could be known - but they should still be merged)
	hlRemoveExpressionTypes(n)

	function = n
end function

private function hlFixExpressions(byval n as ASTNODE ptr) as integer
	''
	'' TODO: Don't process ASTNODE.expr only, because anything that isn't
	'' directly or indirectly stored in ASTNODE.expr will be missed.
	''
	'' In practice that's currently no problem though, because effectively
	'' everything (macro bodies, proc bodies, initializers) uses
	'' ASTNODE.expr. It would only be a problem with code (e.g. scope blocks
	'' containing assignments or calls) outside any macro/proc body, but
	'' that's not allowed in C anyways...
	''
	if n->expr then
		'' TODO: shouldn't assume is_bool_context=TRUE for #define bodies
		var is_bool_context = FALSE
		select case n->class
		case ASTCLASS_PPDEFINE, ASTCLASS_IFPART, ASTCLASS_ELSEIFPART, _
		     ASTCLASS_DOWHILE, ASTCLASS_WHILE
			is_bool_context = TRUE
		end select

		n->expr = hlFixExpression(n->expr, is_bool_context)
	end if

	'' Collect #defines/enumconsts so we can do lookups and determine the
	'' type behind such identifiers used in expressions
	select case n->class
	case ASTCLASS_PPDEFINE
		var body = n->expr
		if body andalso (body->class <> ASTCLASS_SCOPEBLOCK) then
			var dtype = body->dtype
			if dtype <> TYPE_NONE then
				hlCollectSymbol(n->text, dtype)
			end if
		end if
	case ASTCLASS_CONST
		hlCollectSymbol(n->text, TYPE_LONG)
	end select

	function = TRUE
end function

private function isEmptyReservedDefine(byval n as ASTNODE ptr) as integer
	function = (n->class = ASTCLASS_PPDEFINE) andalso _
	           (n->paramcount < 0) andalso (n->expr = NULL) andalso _
	           strIsReservedIdInC(n->text)
end function

private sub hlRemoveEmptyReservedDefines(byval ast as ASTNODE ptr)
	var i = ast->head
	while i
		var nxt = i->next
		if isEmptyReservedDefine(i) then
			astRemove(ast, i)
		end if
		i = nxt
	wend
end sub

private sub hlApplyRemoveOption(byval ast as ASTNODE ptr, byval astclass as integer, byval opt as integer)
	var i = ast->head
	while i
		var nxt = i->next

		if (i->text <> NULL) and (astclass = -1) or (i->class = astclass) then
			if hl.api->idopt(opt).matches(i->text) then
				astRemove(ast, i)
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyRemove1st(byval ast as ASTNODE ptr)
	dim removed as THash = THash(7, TRUE)
	var i = ast->head
	while i
		var nxt = i->next

		if i->text then
			if hl.api->idopt(OPT_REMOVE1ST).matches(i->text) and _
			   (not removed.contains(i->text, hashHash(i->text))) then
				removed.addOverwrite(i->text, NULL)
				astRemove(ast, i)
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyRemove2nd(byval ast as ASTNODE ptr)
	dim found1st as THash = THash(7, FALSE)
	var i = ast->head
	while i
		var nxt = i->next

		if i->text then
			if hl.api->idopt(OPT_REMOVE2ND).matches(i->text) then
				if found1st.contains(i->text, hashHash(i->text)) then
					astRemove(ast, i)
				else
					found1st.addOverwrite(i->text, NULL)
				end if
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyDropProcBodyOptions(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if (i->class = ASTCLASS_PROC) and (i->expr <> NULL) then
			if hl.api->idopt(OPT_DROPPROCBODY).matches(i->text) then
				astDelete(i->expr)
				i->expr = NULL
			end if
		end if

		i = i->next
	wend
end sub

private sub hlApplyMoveOption(byval ast as ASTNODE ptr, byref search as string, byref ref as string)
	dim as ASTNODE ptr decltomove, refdecl

	var i = ast->head
	while i
		if i->text then
			select case *i->text
			case search
				decltomove = i
			case ref
				refdecl = i
			end select
		end if
		i = i->next
	wend

	if (decltomove <> NULL) and (refdecl <> NULL) then
		astUnlink(ast, decltomove)
		astInsert(ast, decltomove, refdecl)
	end if
end sub

private function hlSetArraySizes(byval n as ASTNODE ptr) as integer
	if (n->text <> NULL) and (n->array <> NULL) then
		'' 1 dimension?
		if astHas1Child(n->array) then
			var dimension = n->array->head
			assert(dimension->class = ASTCLASS_DIMENSION)
			if dimension->expr->class = ASTCLASS_ELLIPSIS then
				dim size as zstring ptr = hl.api->setarraysizeoptions.lookupDataOrNull(n->text)
				if size then
					astDelete(dimension->expr)
					dimension->expr = astNewTEXT(size)
				end if
			end if
		end if
	end if
	function = TRUE
end function

private function hApplyRenameOption(byval opt as integer, byval n as ASTNODE ptr) as integer
	dim as zstring ptr newid = hl.api->renameopt(opt).lookupDataOrNull(n->text)
	if newid then
		if n->class = ASTCLASS_PPINCLUDE then
			'' Allow -rename to affect PPINCLUDE nodes, but without giving them
			'' alias strings (which could prevent merging and would add them to renamelists)
			astSetText(n, newid)
		else
			astRenameSymbol(n, newid)
		end if
		function = TRUE
	end if
end function

private function hlApplyRenameOption(byval n as ASTNODE ptr) as integer
	static inside_macro as integer

	if n->text then
		hApplyRenameOption(OPT_RENAME, n)
	end if

	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		if n->subtype->attrib and ASTATTRIB_TAGID then
			hApplyRenameOption(OPT_RENAMETAG, n->subtype)
		else
			hApplyRenameOption(OPT_RENAMETYPEDEF, n->subtype)
		end if
	end if

	select case n->class
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
		if n->text then
			hApplyRenameOption(OPT_RENAMETAG, n)
		end if

	case ASTCLASS_PROC
		if n->text then
			hApplyRenameOption(OPT_RENAMEPROC, n)
		end if

	case ASTCLASS_TYPEDEF
		hApplyRenameOption(OPT_RENAMETYPEDEF, n)

	case ASTCLASS_PPDEFINE
		hApplyRenameOption(OPT_RENAMEDEFINE, n)

		if hl.api->renameopt(OPT_RENAMEMACROPARAM).count > 0 then
			var param = n->head
			var renamed_param_here = FALSE
			while param
				assert(param->class = ASTCLASS_MACROPARAM)
				renamed_param_here or= hApplyRenameOption(OPT_RENAMEMACROPARAM, param)
				param = param->next
			wend

			'' Visit body to update references to renamed parameters,
			'' but only if this #define actually has a parameter that was renamed,
			'' otherwise we'd incorrectly rename references to something else that
			'' happened to have the same name but isn't a parameter of this #define.
			if renamed_param_here and (n->expr <> NULL) then
				assert(inside_macro = FALSE)
				inside_macro = TRUE
				astVisit(n->expr, @hlApplyRenameOption)
				inside_macro = FALSE
			end if
		end if

	case ASTCLASS_UNDEF
		hApplyRenameOption(OPT_RENAMEDEFINE, n)

	case ASTCLASS_TEXT, ASTCLASS_CALL
		hApplyRenameOption(OPT_RENAMEPROC, n)
		hApplyRenameOption(OPT_RENAMEDEFINE, n)
		if inside_macro then
			hApplyRenameOption(OPT_RENAMEMACROPARAM, n)
		end if

	end select

	function = TRUE
end function

type TypedefTable
	'' id -> TYPEDEF ASTNODEs holding id/dtype/subtype/array
	'' Strictly speaking we only need the dtype/array information, but
	'' having the whole TYPEDEF is nicer for error reporting, plus we own
	'' the id so it can be re-used by the hash table.
	table as THash = THash(8, FALSE)
	declare destructor()
	declare function lookup(byval id as zstring ptr) as ASTNODE ptr
	declare sub addOverwrite(byval n as ASTNODE ptr)
	declare operator let(byref as const TypedefTable) '' unimplemented
end type

destructor TypedefTable()
	'' Free the cloned TYPEDEFs
	for i as integer = 0 to table.room - 1
		var item = table.items + i
		if item->s then
			astDelete(item->data)
		end if
	next
end destructor

function TypedefTable.lookup(byval id as zstring ptr) as ASTNODE ptr
	function = table.lookupDataOrNull(id)
end function

sub TypedefTable.addOverwrite(byval n as ASTNODE ptr)
	var datatype = astClone(n)

	'' Resolve typedefs to the type they reference
	'' TODO: what about CONSTs/PTRs? it's not enough to walk to the
	'' inner-most typedef; we have to collect CONSTs/PTRs properly...
	while typeGetDt(datatype->dtype) = TYPE_UDT
		assert(astIsTEXT(n->subtype))
		var subtypetypedef = lookup(n->subtype->text)
		if subtypetypedef = NULL then exit while
		expandTypedef(subtypetypedef, datatype)
	wend

	var hash = hashHash(datatype->text)
	var item = table.lookup(datatype->text, hash)
	if item->s then
		'' Free existing ASTNODE before overwriting, or else it would be leaked
		astDelete(item->data)
		item->s = NULL '' became dangling due to this
	end if
	table.add(item, hash, datatype->text, datatype)
end sub

private sub oopsCantExpandTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr, byref reason as string)
	oops("can't expand " + astDumpPrettyDecl(typedef, TRUE) + " into " + astDumpPrettyDecl(n, TRUE) + ": " + reason)
end sub

'' Expand typedef assuming it's only a simple type, no array or procedure
private sub expandSimpleTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	var newdtype = typeExpand(n->dtype, typedef->dtype)
	if newdtype = TYPE_NONE then
		oopsCantExpandTypedef(typedef, n, "too many pointers")
	end if
	astSetType(n, newdtype, typedef->subtype)
end sub

private sub expandArrayTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	'' Pointer to array?
	if typeGetPtrCount(n->dtype) > 0 then
		'' FB doesn't support pointers to arrays either, but we can drop the array type,
		'' such that the pointer only points to "single array element". That's pretty much
		'' the best translation we can do here, and better than nothing...
		expandSimpleTypedef(typedef, n)
		exit sub
	end if

	'' Expand the array typedef
	astSetType(n, typeGetConst(n->dtype) or typedef->dtype, typedef->subtype)
	if n->array = NULL then
		n->array = astNew(ASTCLASS_ARRAY)
	end if
	astAppend(n->array, astCloneChildren(typedef->array))
end sub

''
'' Expand the function typedef used in the given declaration.
''
'' For example, given a declaration using a function typedef like this one:
''    typedef int (F)(int);
''
'' we want to expand the typedef into the declaration:
''    F f;             =>    int f1(int);            (what looked like a var really is a proc)
''    F *p;            =>    int (*p)(int);          (procptr var)
''    void f(F *p);    =>    void f(int (*p)(int));  (procptr param)
''    typedef F G;     =>    typedef int (G)(int);   (and then later we process G too)
''
'' because FB doesn't support function typedefs.
''
'' If there's a CONST on the function type, e.g.:
''    typedef int (T)(int);
''    T const f;
'' it will be overwritten and lost, i.e. we ignore it. According to
'' gcc -pedantic, "ISO C forbids qualified function types", and it seems like
'' the C++ standard requires cv-qualifiers to be ignored in such cases.
''
'' When expanding into a pointer, some of the existing CONSTs there need to be
'' preserved though:
''    a * const p1;
'' becomes:
''    int (* const p1)(int);
'' But as above, cv-qualifiers on the function type don't make sense and should
'' be ignored, e.g. in case of:
''    a const * p1;
''
private sub expandProcTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	select case n->class
	case ASTCLASS_VAR, ASTCLASS_FIELD
		'' Not a pointer?
		if typeGetPtrCount(n->dtype) = 0 then
			'' This is ok for vars/fields; they just become procedures themselves.
			'' (although with fields that can only really happen in C++ code)
			var proc = typedef->subtype
			assert(proc->class = ASTCLASS_PROC)

			'' Copy over the function result type (overwriting any previous cv-qualifiers),
			'' parameters, and callconv attributes
			n->class = ASTCLASS_PROC
			astSetType(n, proc->dtype, proc->subtype)
			assert(n->head = NULL)
			astAppend(n, astCloneChildren(proc))
			n->attrib or= proc->attrib

			exit sub
		end if

		'' var/field; it's a pointer to the function type; ok

	case ASTCLASS_TYPEDEF, ASTCLASS_PARAM
		'' Expanding typedef in another typedef; always ok
		'' Expanding into param, also ok (it becomes a function pointer;
		'' handled later in hlFixFunctionParameters())

	case else
		'' Anywhere else (function results, casts, sizeof):
		'' must be a pointer to the function type, or we can't expand the typedef
		'' TODO: check whether we can do better - do function types in
		'' casts or sizeof automatically become function pointers in C?
		if typeGetPtrCount(n->dtype) = 0 then
			oopsCantExpandTypedef(typedef, n, "can't have a function type as type for function result/cast/sizeof")
		end if
	end select

	'' Insert the typedef's type, overwriting the use of the typedef
	assert(typeGetDtAndPtr(typedef->dtype) = TYPE_PROC)
	astSetType(n, typeUnsetBaseConst(typeSetDt(n->dtype, TYPE_PROC)), typedef->subtype)
end sub

'' Expand a typedef into a declaration (or any ASTNODE really), assuming that
'' the declaration's dtype uses that typedef.
'' Expanding an array or procedure typedef requires special care, because then
'' the declaration becomes an array or a procedure.
'' It's not allowed/possible in C to have both at the same time (an array of
'' procedures), so we don't need to handle that.
private sub expandTypedef(byval typedef as ASTNODE ptr, byval n as ASTNODE ptr)
	if typedef->array then
		expandArrayTypedef(typedef, n)
	elseif typeGetDtAndPtr(typedef->dtype) = TYPE_PROC then
		expandProcTypedef(typedef, n)
	else
		expandSimpleTypedef(typedef, n)
	end if
end sub

type TypedefExpander
	typedefs as TypedefTable
	declare sub walkDecls(byval n as ASTNODE ptr)
	declare sub walkDefines(byval n as ASTNODE ptr)
	declare operator let(byref as const TypedefExpander) '' unimplemented
end type

sub TypedefExpander.walkDecls(byval n as ASTNODE ptr)
	'' Ignore #defines for now; they will be processed later by walkDefines()
	if n->class = ASTCLASS_PPDEFINE then exit sub

	'' Declaration using one of the typedefs?
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		var typedef = typedefs.lookup(n->subtype->text)
		if typedef then
			'' Expand the typedef into the declaration.
			'' The TypedefTable has alreay expanded this typedef itself as far as possible,
			'' by using the registered typedefs. Thus, expanding a typedef doesn't cause a
			'' need for further expansions. We're only expanding the registered typedefs.
			expandTypedef(typedef, n)
		end if
	end if

	if n->subtype then walkDecls(n->subtype)
	if n->array   then walkDecls(n->array  )
	if n->expr    then walkDecls(n->expr   )

	var i = n->head
	while i
		var nxt = i->next

		walkDecls(i)

		'' Register & drop typedefs
		if i->class = ASTCLASS_TYPEDEF then
			if (i->array <> NULL) orelse _
			   (typeGetDtAndPtr(i->dtype) = TYPE_PROC) orelse _
			   hl.api->idopt(OPT_EXPAND).matches(i->text) then
				typedefs.addOverwrite(i)
				astRemove(n, i)
			end if
		end if

		i = nxt
	wend
end sub

sub TypedefExpander.walkDefines(byval n as ASTNODE ptr)
	var i = n->head
	while i

		if (i->class = ASTCLASS_PPDEFINE) and (i->expr <> NULL) then
			walkDecls(i->expr)
		end if

		i = i->next
	wend
end sub

private function hlFixSpecialParameters(byval n as ASTNODE ptr) as integer
	if n->class = ASTCLASS_PARAM then
		select case typeGetDtAndPtr(n->dtype)
		'' Remap "byval as jmp_buf" to "byval as jmp_buf ptr"
		'' jmp_buf is defined as array type in C, and arrays are passed byref in C.
		'' Since FB's crt/setjmp.bi defines jmp_buf as a simple UDT, we have to handle
		'' the "byref" part manually. Strictly speaking fbfrog should produce "byref as jmp_buf",
		'' but traditionally FB's headers always used a "byval as jmp_buf ptr"...
		case TYPE_UDT
			assert(astIsTEXT(n->subtype))
			if *n->subtype->text = "jmp_buf" then
				n->dtype = typeAddrOf(n->dtype)
			end if

		'' Function type? It's really just a function pointer
		case TYPE_PROC
			n->dtype = typeAddrOf(n->dtype)

		end select

		'' C array parameter? It's really just a pointer (the array is passed byref).
		'' FB doesn't support C array parameters like that, so turn them into pointers:
		''    int a[5]  ->  byval a as long ptr
		if n->array then
			astDelete(n->array)
			n->array = NULL
			if typeGetPtrCount(n->dtype) = TYPEMAX_PTR then
				oops("too many pointers on " + astDumpPrettyDecl(n))
			end if
			n->dtype = typeAddrOf(n->dtype)
		end if
	end if
	function = TRUE
end function

private sub charArrayToFixLenStr(byval n as ASTNODE ptr)
	assert((typeGetDtAndPtr(n->dtype) = TYPE_ZSTRING) or (typeGetDtAndPtr(n->dtype) = TYPE_WSTRING))
	assert(n->array)

	'' Use the last (inner-most) array dimension as the fixed-length string size
	var d = n->array->tail
	assert(d->class = ASTCLASS_DIMENSION)
	assert(d->expr)
	assert(n->subtype = NULL)
	n->subtype = d->expr
	d->expr = NULL
	astRemove(n->array, d)

	'' If no dimensions left, remove the array type entirely
	if n->array->head = NULL then
		astDelete(n->array)
		n->array = NULL
	end if
end sub

private sub stringToByte(byval n as ASTNODE ptr)
	'' Turn zstring/wstring into byte/wchar_t, but preserve PTRs + CONSTs
	if typeGetDt(n->dtype) = TYPE_ZSTRING then
		n->dtype = typeSetDt(n->dtype, TYPE_BYTE)
	else
		n->dtype = typeSetDt(n->dtype, TYPE_WCHAR_T)
	end if

	'' z/wstring * N? Turn back into array
	if n->subtype then
		'' add ARRAY if needed
		if n->array = NULL then
			n->array = astNew(ASTCLASS_ARRAY)
		end if

		'' add fix-len size as last (inner-most) array dimension
		var d = astNew(ASTCLASS_DIMENSION)
		d->expr = n->subtype
		n->subtype = NULL
		astAppend(n->array, d)
	end if
end sub

private sub byteToString(byval n as ASTNODE ptr)
	'' Turn [u]byte/wchar_t into zstring/wstring, but preserve PTRs + CONSTs
	if typeGetDt(n->dtype) = TYPE_WCHAR_T then
		n->dtype = typeSetDt(n->dtype, TYPE_WSTRING)
	else
		n->dtype = typeSetDt(n->dtype, TYPE_ZSTRING)
	end if
	assert(n->subtype = NULL)

	'' If it's a plain string (no ptr), also try to turn array => fix-len string
	select case typeGetDtAndPtr(n->dtype)
	case TYPE_ZSTRING, TYPE_WSTRING
		if n->array then
			charArrayToFixLenStr(n)
		end if
	end select
end sub

private sub hlFindStringOptionMatches _
	( _
		byval parentparent as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byval n as ASTNODE ptr, _
		byval index as integer _
	)

	'' TODO: don't call DeclPatterns.matches() if CharStringPass won't care anyways
	'' 1. only check for match on nodes that have string/byte dtype
	'' 2. don't try 2nd lookup if first one succeeded (-string and -nostring
	''    on the same decl doesn't make sense anyways)
	'' but this only works if string typedefs are already expanded,
	'' so also try matching on nodes with UDT type?

	if hl.api->patterns(OPT_NOSTRING).matches(parentparent, parent, n, index) then
		n->attrib or= ASTATTRIB_NOSTRING
	end if

	if hl.api->patterns(OPT_STRING).matches(parentparent, parent, n, index) then
		n->attrib or= ASTATTRIB_STRING
	end if

	if n->subtype then hlFindStringOptionMatches(parent, n, n->subtype, 0)
	if n->array   then hlFindStringOptionMatches(parent, n, n->array  , 0)
	if n->expr    then hlFindStringOptionMatches(parent, n, n->expr   , 0)

	var i = n->head
	var childindex = 0
	while i
		hlFindStringOptionMatches(parent, n, i, childindex)
		i = i->next
		childindex += 1
	wend
end sub

''
'' Char/string handling pass
''
'' The C parser turned char/wchar_t into zstring/wstring. Now we need to turn
'' zstring/wstring into byte/wchar_t where it's probably not a string, and
'' apply -string/-nostring options.
''
'' The default behaviour is to translate char/wchar_t depending on context -
'' if it's a char/wchar_t pointer/array/typedef, we translate it as a string
'' type (i.e. zstring or wstring). If it's a single char/wchar_t, we translate
'' it as byte. For this, it is also necessary to expand char typedefs into the
'' contexts that use them, so we can then translate the char/wchar_t to
'' zstring/wstring or byte/wchar_t.
''
'' Furthermore, we support some options to override the defaults:
''   -string    =>  Force "[un]signed char" to be treated as zstring. If it's
''                  a typedef, it now needs to be expanded to get the
''                  context-specific behaviour like normal string typedefs.
''   -nostring  =>  prevent char/wchar_t from being treated as zstring/wstring
''
type CharStringPass
	typedefs as TypedefTable
	declare sub walkDecls(byval n as ASTNODE ptr)
	declare sub walkDefines(byval n as ASTNODE ptr)
	declare operator let(byref as const CharStringPass) '' unimplemented
end type

#define isAffectedByNoString(n) (((n)->attrib and ASTATTRIB_NOSTRING) <> 0)
#define isAffectedByString(n)   (((n)->attrib and ASTATTRIB_STRING  ) <> 0)

sub CharStringPass.walkDecls(byval n as ASTNODE ptr)
	'' Ignore string/char literals, which also have string/char types, but
	'' shouldn't be changed.
	'' Ignore #defines for now; they will be processed later by walkDefines()
	select case n->class
	case ASTCLASS_STRING, ASTCLASS_CHAR, ASTCLASS_PPDEFINE
		exit sub
	end select

	'' Expand "char/wchar_t" typedefs in this decl, unless the typedef or decl is affected by -nostring
	'' Expand "[un]signed char" typedef into decls affected by -string
	'' We don't expand pointer typedefs though (e.g. "char/wchar_t *" or "[un]signed char *")
	'' because they have enough context by themselves (we already know they're pointers).
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		var typedef = typedefs.lookup(n->subtype->text)
		if typedef then
			select case typeGetDtAndPtr(typedef->dtype)
			case TYPE_ZSTRING, TYPE_WSTRING
				if (not isAffectedByNoString(n)) and (not isAffectedByNoString(typedef)) then
					expandSimpleTypedef(typedef, n)
				end if
			case TYPE_BYTE, TYPE_UBYTE, TYPE_WCHAR_T
				if isAffectedByString(n) then
					expandSimpleTypedef(typedef, n)
				end if
			end select
		end if
	end if

	select case typeGetDt(n->dtype)
	'' Turn zstring/wstring (originally char/wchar_t in C) decls into byte/wchar_t (non-string types),
	'' if it's neither pointer/array/typedef, or if -nostring.
	case TYPE_ZSTRING, TYPE_WSTRING
		'' Affected by -nostring?
		if isAffectedByNoString(n) then
			'' Turn string into byte (even pointers)
			stringToByte(n)
		'' Don't turn string pointers into byte pointers
		elseif typeGetPtrCount(n->dtype) <> 0 then

		'' zstring + array (produced by C parser)? => fix-len zstring
		elseif n->array then
			charArrayToFixLenStr(n)

		elseif n->class <> ASTCLASS_TYPEDEF then
			stringToByte(n)
		end if

	'' Turn decls using "[un]signed char [*]" into "char [*]" if -string
	'' If it's a typedef and then just a "char" (not a pointer), it will be
	'' expanded, like any other "char" typedefs.
	'' Regarding wchar_t: Normally the C parser produced TYPE_WSTRING, but
	'' we can still see TYPE_WCHAR_T here in case a wchar_t typedef got
	'' expanded into this declaration.
	'' If it's a single char, it can't be turned into string though, that would
	'' be invalid FB, except in typedefs.
	case TYPE_BYTE, TYPE_UBYTE, TYPE_WCHAR_T
		if (n->class = ASTCLASS_TYPEDEF) or _
		   (typeGetPtrCount(n->dtype) <> 0) or _
		   (n->array <> NULL) then
			if isAffectedByString(n) then
				byteToString(n)
			end if
		end if
	end select

	'' Register plain byte/char/string, the ones we're going to expand
	'' - "char" typedefs were turned into string ones and should be expanded
	'' - if -nostring was given for one of them, it's now using byte, but
	''   should still be registered, to be expanded into -string decls,
	''   which will be turned from byte to string after the expansion.
	'' - if -string was given for a byte typedef, it was turned into a
	''   string one, and should be registered like normal string typedefs.
	'' - it can be a wchar_t typedef, in case we did stringToByte() above
	''   (which converted wstring => wchar_t)
	if n->class = ASTCLASS_TYPEDEF then
		select case typeGetDtAndPtr(n->dtype)
		case TYPE_ZSTRING, TYPE_WSTRING, TYPE_BYTE, TYPE_UBYTE, TYPE_WCHAR_T
			typedefs.addOverwrite(n)
		case TYPE_UDT
			'' Also register if it's a typedef to another typedef
			'' (that we already know to be a string/byte one)
			assert(astIsTEXT(n->subtype))
			if typedefs.lookup(n->subtype->text) then
				typedefs.addOverwrite(n)
			end if
		end select
	end if

	if n->subtype then walkDecls(n->subtype)
	if n->array   then walkDecls(n->array  )
	if n->expr    then walkDecls(n->expr   )

	var i = n->head
	while i
		walkDecls(i)
		i = i->next
	wend
end sub

sub CharStringPass.walkDefines(byval n as ASTNODE ptr)
	var i = n->head
	while i

		if (i->class = ASTCLASS_PPDEFINE) and (i->expr <> NULL) then
			walkDecls(i->expr)
		end if

		i = i->next
	wend
end sub

private sub hlRenameJobsBegin()
	hl.renamejobs = new THash(6, TRUE)
end sub

private function hlRenameJobAdd(byval oldid as zstring ptr, byval newname as ASTNODE ptr) as integer
	var hash = hashHash(oldid)
	var item = hl.renamejobs->lookup(oldid, hash)
	if item->s then
		'' Allow overwriting duplicate typedefs (if both oldid/newid are the same),
		'' but not if the newid differs (then it's a separate typedef which has to be
		'' preserved - we can't rename A => C when already renaming A => B).
		dim as ASTNODE ptr prevnewid = item->data
		if ucase(*prevnewid->text, 1) <> ucase(*newname->text, 1) then
			return FALSE
		end if
	end if

	'' When renaming, we don't just override the id, but also copy the alias
	'' and related flags. This way, if an UDT is given a renamed typedef's
	'' name, the UDT will become "renamed" too, and thus the type will
	'' continue to appear in renamelists.
	var newid = astNewTEXT(newname->text)
	astSetAlias(newid, newname->alias)
	newid->attrib = newname->attrib and ASTATTRIB_NORENAMELIST

	if item->s then
		'' Free existing newid if there already is a renamejob for this oldid,
		'' then the new newid can be stored
		astDelete(item->data)
		item->data = newid
	else
		hl.renamejobs->add(item, hash, oldid, newid)
	end if

	function = TRUE
end function

private sub hMaybeApplyRenameJob(byval n as ASTNODE ptr)
	'' Is there a renamejob for this oldid?
	dim as ASTNODE ptr newid = hl.renamejobs->lookupDataOrNull(n->text)
	if newid then
		'' Change the id if needed
		if *n->text <> *newid->text then
			astSetText(n, newid->text)
		end if

		'' Change alias too
		if newid->alias then
			astSetAlias(n, newid->alias)
		end if

		n->attrib or= newid->attrib and ASTATTRIB_NORENAMELIST

		'' Remove attributes even if id strictly speaking doesn't change
		n->attrib and= not (ASTATTRIB_TAGID or ASTATTRIB_GENERATEDID)
	end if
end sub

'' Check all type names (definitions and references): If there is a rename job
'' for an id, then change it to use the newid.
private function hlApplyRenameJobs(byval n as ASTNODE ptr) as integer
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		hMaybeApplyRenameJob(n->subtype)
	end if

	select case n->class
	case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
		if n->text then
			hMaybeApplyRenameJob(n)
		end if
	end select

	function = TRUE
end function

private sub hlRenameJobsEnd(byval ast as ASTNODE ptr)
	astVisit(ast, @hlApplyRenameJobs)
	scope
		'' Free the newids
		for i as integer = 0 to hl.renamejobs->room - 1
			var item = hl.renamejobs->items + i
			if item->s then
				astDelete(item->data)
			end if
		next
	end scope
	delete hl.renamejobs
	hl.renamejobs = NULL
end sub

private sub hlSolveOutTagIds(byval n as ASTNODE ptr)
	var i = n->head
	while i
		var nxt = i->next

		if i->class = ASTCLASS_TYPEDEF then
			if i->dtype = TYPE_UDT then
				assert(astIsTEXT(i->subtype))
				'if i->subtype->attrib and ASTATTRIB_TAGID then
				if i->subtype->attrib and ASTATTRIB_GENERATEDID then
					if hlRenameJobAdd(i->subtype->text, i) then
						astRemove(n, i)
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

'' Remove redundant typedefs where the typedef and the type have the same,
'' for example:
''    typedef struct A A;
''    typedef struct b B;  (FB is case-insensitive)
'' but not if there are pointers or CONSTs involved.
private sub hlRemoveSameIdTypedefs(byval n as ASTNODE ptr)
	var i = n->head
	while i
		var nxt = i->next

		if i->class = ASTCLASS_TYPEDEF then
			if i->dtype = TYPE_UDT then
				assert(astIsTEXT(i->subtype))
				if ucase(*i->subtype->text, 1) = ucase(*i->text, 1) then
					if hlRenameJobAdd(i->subtype->text, i) then
						astRemove(n, i)
					end if
				end if
			end if
		end if

		i = nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
'' Auto-add forward declarations for types that are used before/without being defined.
''
'' This is definitely good for types that are defined by the binding.
'' However, sometimes headers use types without defining them:
''  a) types from other headers, e.g. "struct time" or "time_t" or "FILE"
''      => shouldn't add forward declarations for these
''  b) opaque types
''      => should add forward declarations for these
'' but it's hard to tell the difference between a) and b).
'' TODO: Hard-code some names for commonly used "external" types and then don't
'' add forward declarations for them unless they're defined in the binding? Add
'' command-line option for specifying whether types are external or not, or
'' whether a forward declaration needs to be added for a certain type name?
''
'' In C, forward references are only possible with tags, not typedefs, but we
'' don't need to care about this. In our AST and in FB all types are the same.
''
'' Sometimes C headers already contain "forward declarations" of the form:
''    typedef struct A B;
'' If a type is only forward-referenced in such typedefs, then we don't need to
'' add yet-another forward declaration, because these typedefs act as the
'' forward declaration already.
''

'' Return = index into hl.types array
private function hLookupOrAddType(byval id as zstring ptr) as integer
	var hash = hashHash(id)
	var item = hl.typehash->lookup(id, hash)
	if item->s = NULL then
		'' Append to hl.types array
		if hl.typeroom = hl.typecount then
			if hl.typeroom = 0 then
				hl.typeroom = 512
			else
				hl.typeroom *= 2
			end if
			hl.types = reallocate(hl.types, hl.typeroom * sizeof(*hl.types))
		end if
		with hl.types[hl.typecount]
			.id = strDuplicate(id)
			.definition = NULL
			.forwarduse = FALSE
			.firstuse = NULL
			hl.typehash->add(item, hash, .id, cptr(any ptr, hl.typecount))
		end with
		function = hl.typecount
		hl.typecount += 1
	else
		function = cint(item->data)
	end if
end function

private function hlCollectForwardUses(byval n as ASTNODE ptr) as integer
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))

		var typeindex = hLookupOrAddType(n->subtype->text)
		var typ = hl.types + typeindex

		'' Record the first use (even if it's in a typedef), so the
		'' forward decl (if one is needed) can be inserted above it.
		if typ->firstuse = NULL then
			typ->firstuse = hl.currentdecl
		end if

		'' Collect forward references from everything except typedefs
		if n->class <> ASTCLASS_TYPEDEF then
			'' Type not defined yet? First forward use?
			if (typ->definition = NULL) and (not typ->forwarduse) then
				typ->forwarduse = TRUE
			end if
		end if
	end if

	'' Don't visit #define bodies - type references in them don't count as
	'' forward references.
	'' TODO: still add forward declarations for types that are only
	'' referenced from #defines (probably rare in practice though)
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

private sub hlScanForForwardUsedTypes(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		'' If this is an UDT definition (tag body or typedef), register the type as "defined"
		'' (before scanning the struct fields for potential forward-references, so that "recursive"
		'' references to this UDT aren't seen as forward references)
		select case i->class
		case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, ASTCLASS_TYPEDEF
			'' Named? (enums can be anonymous)
			if i->text then
				'' Mark type as defined
				var typeindex = hLookupOrAddType(i->text)
				hl.types[typeindex].definition = i
			end if
		end select

		hl.currentdecl = i
		astVisit(i, @hlCollectForwardUses)
		hl.currentdecl = NULL

		i = i->next
	wend
end sub

private function hShouldAddForwardDeclForType(byref typ as hl.TYPENODE) as integer
	'' Only if actually forward-referenced (as far as we can tell)
	if typ.forwarduse then
		if typ.definition then
			'' Defined in this binding, ok (most likely correct)
			function = TRUE
		else
			'' Not defined here; only if -addforwarddecl was given
			function = hl.api->idopt(OPT_ADDFORWARDDECL).matches(typ.id)
		end if
	end if
end function

'' Add forward decls for forward-referenced types
private sub hlAddForwardDecls(byval ast as ASTNODE ptr)
	for i as integer = 0 to hl.typecount - 1
		var typ = hl.types + i
		if hShouldAddForwardDeclForType(*typ) then
			var fwdid = *typ->id + "_"
			if typ->definition then
				assert(*typ->id = *typ->definition->text)
				astRenameSymbol(typ->definition, fwdid, FALSE)
			end if
			var fwd = astNew(ASTCLASS_TYPEDEF, typ->id)
			fwd->dtype = TYPE_UDT
			fwd->subtype = astNewTEXT(fwdid)
			fwd->location = typ->firstuse->location
			astInsert(ast, fwd, typ->firstuse)
		end if
	next
end sub

private function typeInvolvesUdt(byval dtype as integer, byval subtype as ASTNODE ptr) as integer
	select case typeGetDt(dtype)
	case TYPE_UDT
		return TRUE

	'' Function [pointer] type?
	case TYPE_PROC
		assert(subtype->class = ASTCLASS_PROC)

		'' Check function result
		if typeInvolvesUdt(subtype->dtype, subtype->subtype) then return TRUE

		'' Check parameters
		var param = subtype->head
		while param
			if typeInvolvesUdt(param->dtype, param->subtype) then return TRUE
			param = param->next
		wend
	end select

	function = FALSE
end function

''
'' Checks whether an expression is simple enough that it could be used in an
'' FB constant declaration (const FOO = <...>).
''
'' Thus, it shouldn't contain any function calls, addrof/deref operations, or
'' preprocessor operations, etc., but only a selected set of known-to-be-safe
'' math operations & co.
''
'' TODO: allow more here:
''  * references to other constants
''  * string literals
''  * "sizeof(datatype)", but not "datatype"
''  * cast(A, ...) if type A is declared above (#defines don't need to
''    care, but constants declarations do...)
''
private function hIsSimpleConstantExpression(byval n as ASTNODE ptr) as integer
	select case n->class
	'' Atoms
	case ASTCLASS_CONSTI, ASTCLASS_CONSTF

	'' UOPs
	case ASTCLASS_NOT, ASTCLASS_NEGATE
		if hIsSimpleConstantExpression(n->head) = FALSE then exit function

	'' Casts: only if the type doesn't involve a UDT (we would have to be
	'' sure that the UDT is declared above this constant to allow casts
	'' involving UDTs, but currently we don't track that)
	case ASTCLASS_CAST
		if typeInvolvesUdt(n->dtype, n->subtype) then exit function
		if hIsSimpleConstantExpression(n->head) = FALSE then exit function

	'' BOPs
	case ASTCLASS_LOGOR, ASTCLASS_LOGAND, _
	     ASTCLASS_OR, ASTCLASS_XOR, ASTCLASS_AND, _
	     ASTCLASS_EQ, ASTCLASS_NE, _
	     ASTCLASS_LT, ASTCLASS_LE, _
	     ASTCLASS_GT, ASTCLASS_GE, _
	     ASTCLASS_SHL, ASTCLASS_SHR, _
	     ASTCLASS_ADD, ASTCLASS_SUB, _
	     ASTCLASS_MUL, ASTCLASS_DIV, ASTCLASS_MOD
		if hIsSimpleConstantExpression(n->head) = FALSE then exit function
		if hIsSimpleConstantExpression(n->tail) = FALSE then exit function

	'' IIF
	case ASTCLASS_IIF
		if hIsSimpleConstantExpression(n->expr) = FALSE then exit function
		if hIsSimpleConstantExpression(n->head) = FALSE then exit function
		if hIsSimpleConstantExpression(n->tail) = FALSE then exit function

	case else
		exit function
	end select

	function = TRUE
end function

private function hIsUnsizedArray(byval n as ASTNODE ptr) as integer
	if n->array then
		assert(n->array->class = ASTCLASS_ARRAY)
		assert(n->array->head->class = ASTCLASS_DIMENSION)
		function = astHas1Child(n->array) and (n->array->head->expr->class = ASTCLASS_ELLIPSIS)
	end if
end function

private sub hFixUnsizedArray(byval ast as ASTNODE ptr, byval n as ASTNODE ptr)
	if hIsUnsizedArray(n) then
		var id = *n->text
		var tempid = "__" + id

		var def = astNewPPDEFINE(id)
		def->paramcount = 1
		astAppend(def, astNew(ASTCLASS_MACROPARAM, "i"))
		def->expr = astNewTEXT("((@" + tempid + ")[i])")
		def->location = n->location
		astInsert(ast, def, n)

		astRenameSymbol(n, tempid, FALSE)
		astDelete(n->array)
		n->array = NULL

		exit sub
	end if

	select case typeGetDtAndPtr(n->dtype)
	case TYPE_ZSTRING, TYPE_WSTRING
		if n->subtype andalso (n->subtype->class = ASTCLASS_ELLIPSIS) then
			var id = *n->text
			var tempid = "__" + id

			var def = astNewPPDEFINE(id)
			def->expr = astNewTEXT("(*cptr(" + emitType(typeAddrOf(n->dtype), NULL) + ", @" + tempid + "))")
			def->location = n->location
			astInsert(ast, def, n->next)

			astRenameSymbol(n, tempid, FALSE)
			astDelete(n->subtype)
			n->subtype = NULL
			stringToByte(n)
		end if
	end select
end sub

''
'' Search for arrays with unspecified size (can happen at least with extern vars)
'' and wrap them so they can be indexed arbitrarily as in C.
''     extern dtype array[];
'' =>
''     extern array(0 to ...) as dtype
'' =>
''     #define array(i) ((@__array)[i])
''     extern __array alias "array" as dtype
''
'' This also affects fix-len strings (which are arrays in C):
''     extern char s[];
'' =>
''     extern s as zstring * ...
'' =>
''     extern __s alias "s" as ubyte;
''     #define s (*cptr(zstring ptr, @__s))
''
'' There's no need to worry about multi-dimensional arrays (or string arrays),
'' because nested dimensions mustn't have unspecified size in C anyways.
''
private sub hlFixUnsizedArrays(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if i->class = ASTCLASS_VAR then
			hFixUnsizedArray(ast, i)
		end if

		i = i->next
	wend
end sub

'' Turns simple #defines into constants
private sub hlTurnDefinesIntoProperDeclarations(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if i->class = ASTCLASS_PPDEFINE then
			'' Object-like macro?
			if i->paramcount < 0 then
				'' Has a body?
				if i->expr then
					'' Body is a simple expression?
					if hIsSimpleConstantExpression(i->expr) then
						i->class = ASTCLASS_CONST
					end if
				end if
			end if
		end if

		i = i->next
	wend
end sub

private sub hlDropMacroBodyScopes(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if (i->class = ASTCLASS_PPDEFINE) andalso _
		   i->expr andalso (i->expr->class = ASTCLASS_SCOPEBLOCK) then
			if i->expr->head = NULL then
				astDelete(i->expr)
				i->expr = NULL
			elseif astHas1Child(i->expr) then
				var stmt = astClone(i->expr->head)
				astDelete(i->expr)
				i->expr = stmt
			end if
		end if

		i = i->next
	wend
end sub

'' Remove void casts, e.g.:
''    #define FOO (void)f()
'' =>
''    #define FOO f()
private sub hlRemoveVoidCasts(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if (i->class = ASTCLASS_PPDEFINE) andalso _
		   i->expr andalso _
		   (i->expr->class = ASTCLASS_CAST) andalso _
		   (i->expr->dtype = TYPE_ANY) then
			var expr = astClone(i->expr->head)
			astDelete(i->expr)
			i->expr = expr
		end if

		i = i->next
	wend
end sub

private sub hlAddUndefsAboveDecls(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if (i->class <> ASTCLASS_UNDEF) and (i->text <> NULL) then
			if hl.api->idopt(OPT_UNDEFBEFOREDECL).matches(i->text) then
				var undef = astNew(ASTCLASS_UNDEF, i->text)
				undef->location = i->location
				astInsert(ast, undef, i)
			end if
		end if

		i = i->next
	wend
end sub

private sub hlAddIfndefsAroundDecls(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		if (i->class <> ASTCLASS_UNDEF) andalso _
		   i->text andalso hl.api->idopt(OPT_IFNDEFDECL).matches(i->text) then
			i->attrib or= ASTATTRIB_IFNDEFDECL
		end if

		i = i->next
	wend
end sub

type ParamUsageChecker
	proc as ASTNODE ptr
	have_multiple_uses as integer
	mark_as_macroparam as integer
	declare constructor(byval proc as ASTNODE ptr, byval mark_as_macroparam as integer)
	declare function lookupParam(byval id as zstring ptr) as ASTNODE ptr
	declare sub onTEXT(byval n as ASTNODE ptr)
	declare destructor()
end type

constructor ParamUsageChecker(byval proc as ASTNODE ptr, byval mark_as_macroparam as integer)
	this.proc = proc
	this.mark_as_macroparam = mark_as_macroparam
end constructor

function ParamUsageChecker.lookupParam(byval id as zstring ptr) as ASTNODE ptr
	var param = proc->head
	while param
		if param->text andalso (*param->text = *id) then
			return param
		end if
		param = param->next
	wend
	function = NULL
end function

sub ParamUsageChecker.onTEXT(byval n as ASTNODE ptr)
	var param = lookupParam(n->text)
	if param then
		if param->attrib and ASTATTRIB_USED then
			have_multiple_uses = TRUE
		else
			param->attrib or= ASTATTRIB_USED
		end if

		if mark_as_macroparam then
			n->attrib or= ASTATTRIB_PARENTHESIZEDMACROPARAM
		end if
	end if
end sub

destructor ParamUsageChecker()
	var param = proc->head
	while param
		param->attrib and= not ASTATTRIB_USED
		param = param->next
	wend
end destructor

dim shared paramusage as ParamUsageChecker ptr

private function collectParamUses(byval n as ASTNODE ptr) as integer
	if astIsTEXT(n) then
		paramusage->onTEXT(n)
	end if
	function = TRUE
end function

'' Check whether a proc can be turned into a #define.
''  - it must only return an expression; no other statements in the body.
''  - parameters (if any) can be used only once (otherwise they'd be evaluated
''    multiple times if turned into macro parameters).
private sub maybeProc2Macro(byval proc as ASTNODE ptr)
	assert(proc->class = ASTCLASS_PROC)

	var body = proc->expr
	assert(body->class = ASTCLASS_SCOPEBLOCK)

	'' just a RETURN?
	if astHasOnlyChild(body, ASTCLASS_RETURN) then
		var ret = body->head

		'' does it return an expression?
		if ret->head then
			'' Check that each parameter is only used once by the expression
			paramusage = new ParamUsageChecker(proc, FALSE)
			astVisit(ret->head, @collectParamUses)
			var have_multiple_uses = paramusage->have_multiple_uses
			delete paramusage
			paramusage = NULL

			if have_multiple_uses = FALSE then
				'' Turn proc into #define
				''  - keep parameters
				''  - cast expression to function's result type if there
				''    is no cast yet. This may or may not be useful...

				'' Wrap references to macro parameters in parentheses
				paramusage = new ParamUsageChecker(proc, TRUE)
				astVisit(ret->head, @collectParamUses)
				delete paramusage
				paramusage = NULL

				proc->class = ASTCLASS_PPDEFINE

				'' Params: turn into MACROPARAMs, forget dtypes
				var param = proc->head
				while param
					param->class = ASTCLASS_MACROPARAM
					astSetType(param, TYPE_NONE, NULL)
					param = param->next
				wend

				'' Macro body, add cast if there is none yet
				var expr = astClone(ret->head)
				if astIsCastTo(expr, proc->dtype, proc->subtype) = FALSE then
					expr = astNew(ASTCLASS_CAST, expr)
					astSetType(expr, proc->dtype, proc->subtype)
				end if

				'' Proc body -> macro body
				astDelete(proc->expr)
				proc->expr = expr

				'' Forget function dtype & callconv
				astSetType(proc, TYPE_NONE, NULL)
				proc->attrib and= not ASTATTRIB__CALLCONV
			end if
		end if
	end if
end sub

private sub hlProcs2Macros(byval ast as ASTNODE ptr)
	var i = ast->head
	while i

		'' proc with a body?
		if (i->class = ASTCLASS_PROC) andalso i->expr then
			maybeProc2Macro(i)
		end if

		i = i->next
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hlCountCallConvs(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_PROC
		hl.need_extern = TRUE
		if n->attrib and ASTATTRIB_STDCALL then
			hl.stdcalls += 1
		else
			assert(n->attrib and ASTATTRIB_CDECL)
			hl.cdecls += 1
		end if
	case ASTCLASS_VAR
		hl.need_extern or= ((n->attrib and ASTATTRIB_EXTERN) <> 0) or _
		                   ((n->attrib and ASTATTRIB_STATIC) = 0)
	end select
	'' Don't count code in macro bodies
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

private function hlHideCallConv(byval n as ASTNODE ptr) as integer
	if n->class = ASTCLASS_PROC then
		if n->attrib and hl.mostusedcallconv then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if
	'' Don't hide callconvs in macro bodies, otherwise they could
	'' end up using the wrong callconv if expanded outside the
	'' header's Extern block.
	function = (n->class <> ASTCLASS_PPDEFINE)
end function

'' *.h CRT/POSIX headers for which FB has corresponding crt/*.bi versions
dim shared fbcrtheaders(0 to ...) as zstring ptr = _
{ _
	@"assert", @"ctype", @"errno", @"float", @"limits", @"locale", _
	@"math", @"setjmp", @"signal", @"stdarg", @"stddef", @"stdint", _
	@"stdio", @"stdlib", @"string", @"time", _
	@"sys/types", @"sys/socket", @"wchar" _
}

type IncludePass
	fbcrtheaderhash as THash = THash(5, TRUE)
	declare constructor()
	declare sub translateHeaderFileName(byref filename as string)
	declare sub work(byval ast as ASTNODE ptr, byref bioptions as ApiSpecificBiOptions)
	declare operator let(byref as const IncludePass) '' unimplemented
end type

constructor IncludePass()
	for i as integer = lbound(fbcrtheaders) to ubound(fbcrtheaders)
		fbcrtheaderhash.addOverwrite(fbcrtheaders(i), NULL)
	next
end constructor

'' Remap .h name to a .bi name. If it's a known system header, we can even remap
'' it to the corresponding FB header (in some cases it's not as simple as
'' replacing .h by .bi).
sub IncludePass.translateHeaderFileName(byref filename as string)
	filename = pathStripExt(filename)

	'' Is it one of the CRT headers? Remap it to crt/*.bi
	if fbcrtheaderhash.contains(filename, hashHash(filename)) then
		filename = "crt/" + filename
	end if

	filename += ".bi"
end sub

sub IncludePass.work(byval ast as ASTNODE ptr, byref bioptions as ApiSpecificBiOptions)
	'' Find node above which new #includes should be inserted. This should
	'' always be behind existing #includes at the top.
	var top = ast->head
	while top andalso (top->class = ASTCLASS_PPINCLUDE)
		top = top->next
	wend

	'' Start at this "top" node, otherwise we'd run an infinite loop trying
	'' to move things to the top that already are at the top (because we'd
	'' be *above* the "top" node, and moving #includes *down* to it, which
	'' means we'd visit them again and again, since we're cycling from top
	'' to bottom via the "next" pointers).
	var i = top
	while i
		var nxt = i->next

		'' #include?
		if i->class = ASTCLASS_PPINCLUDE then
			'' Move to top (outside of Extern block), without changing the order
			assert(i <> top)
			astUnlink(ast, i)
			astInsert(ast, i, top)
		end if

		i = nxt
	wend

	'' Add #includes from -addinclude options
	if bioptions.addincludes then
		astInsert(ast, bioptions.addincludes, top)
		bioptions.addincludes = NULL
	end if

	'' For each #include at the top, apply -removeinclude and remap *.h => *.bi
	i = ast->head
	while i <> top
		var nxt = i->next

		'' #include?
		if i->class = ASTCLASS_PPINCLUDE then
			var filename = *i->text
			if hl.api->removeinclude.contains(filename, hashHash(filename)) then
				astRemove(ast, i)
			else
				translateHeaderFileName(filename)
				astSetText(i, filename)
			end if
		end if

		i = nxt
	wend

	'' Remove duplicate #includes
	dim includes as THash = THash(6, TRUE)

	i = ast->head
	while i <> top
		var nxt = i->next

		'' #include?
		if i->class = ASTCLASS_PPINCLUDE then
			var hash = hashHash(i->text)
			var item = includes.lookup(i->text, hash)
			if item->s then
				astRemove(ast, i)
			else
				includes.add(item, hash, i->text, NULL)
			end if
		end if

		i = nxt
	wend
end sub

private function hlSearchSpecialDtypes(byval n as ASTNODE ptr) as integer
	select case typeGetDt(n->dtype)
	case TYPE_CLONG, TYPE_CULONG
		hl.uses_clong = TRUE
	case TYPE_CLONGDOUBLE
		hl.uses_clongdouble = TRUE
	end select
	function = TRUE
end function

private function hlBuildRenameList(byval n as ASTNODE ptr) as ASTNODE ptr
	var list = astNewGROUP()

	var i = n->head
	while i

		if (i->text <> NULL) and (i->alias <> NULL) and _
		   ((i->attrib and ASTATTRIB_NORENAMELIST) = 0) then
			astAppend(list, astNewTEXT( _
				astDumpPrettyClass(i->class) + " " + _
					*i->alias + " => " + *i->text))
		end if

		'' TODO: recurse into struct/union if renaming fields...
		''astAppend(list, hlBuildRenameList(i))

		if i->class = ASTCLASS_ENUM then
			'' Handle enumconsts - they're in the same scope as the enum itself anyways,
			'' so we don't need to worry about making a nested renamelist for the enum.
			astAppend(list, hlBuildRenameList(i))
		end if


		i = i->next
	wend

	function = list
end function

'' while (0) ...      =>   scope : ... : end scope
'' do ... while (0);  =>   scope : ... : end scope
'' (commonly used construct in C headers)
private function hlTurnWhile0IntoScope(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_DOWHILE, ASTCLASS_WHILE
		'' Loop condition is just a 0?
		if astIsConst0(n->expr) then
			astDelete(n->expr)
			n->expr = NULL
			n->class = ASTCLASS_SCOPEBLOCK
		end if
	end select
	function = TRUE
end function

private function hlSolveOutUnnecessaryScopeBlocks(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_SCOPEBLOCK, ASTCLASS_DOWHILE, ASTCLASS_WHILE, _
	     ASTCLASS_IFPART, ASTCLASS_ELSEIFPART, ASTCLASS_ELSEPART
		'' Contains just a SCOPEBLOCK?
		while astHasOnlyChild(n, ASTCLASS_SCOPEBLOCK)
			'' Move scope block's children in place of the scope block
			astAppend(n, astCloneChildren(n->head))
			astRemove(n, n->head)
		wend

	case ASTCLASS_PPDEFINE
		'' If the macro body is a scope block that contains just one if/loop block,
		'' then remove the scope block
		do
			if n->expr = NULL then exit do
			if n->expr->class <> ASTCLASS_SCOPEBLOCK then exit do

			var scopeblock = n->expr

			'' Exactly one statement?
			if astHas1Child(scopeblock) = FALSE then exit do

			'' It's a scope/if/loop?
			select case scopeblock->head->class
			case ASTCLASS_SCOPEBLOCK, ASTCLASS_IFBLOCK, ASTCLASS_DOWHILE, ASTCLASS_WHILE
				'' Remove the outer scope block, and directly use the inner block as macro body.
				var stmt = scopeblock->head
				astUnlink(scopeblock, stmt)
				astDelete(n->expr)
				n->expr = stmt

			case else
				exit do
			end select
		loop
	end select

	function = TRUE
end function

private function hlCreateElseIfs(byval n as ASTNODE ptr) as integer
	if n->class = ASTCLASS_IFBLOCK then
		do
			'' Has an else part?
			var elsepart = n->tail
			if (elsepart = NULL) orelse (elsepart->class <> ASTCLASS_ELSEPART) then
				exit do
			end if

			'' Contains just an IFBLOCK?
			if astHasOnlyChild(elsepart, ASTCLASS_IFBLOCK) = FALSE then
				exit do
			end if

			var nestedif = elsepart->head
			assert(nestedif->head->class = ASTCLASS_IFPART)

			'' Copy the if/elseif/else parts from the nested ifblock
			'' into the parent ifblock, behind the else part
			astAppend(n, astCloneChildren(nestedif))

			'' Turn the added if part into an elseif
			assert(elsepart->next->class = ASTCLASS_IFPART)
			elsepart->next->class = ASTCLASS_ELSEIFPART

			astRemove(n, elsepart)
		loop
	end if
	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hIsPPElseOrEnd(byval n as ASTNODE ptr) as integer
	select case n->class
	case ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, ASTCLASS_PPENDIF
		function = TRUE
	end select
end function

private function hAreSimilar(byval a as integer, byval b as integer) as integer
	if a = b then return TRUE

	'' FBCODE is probably ok
	if (a = ASTCLASS_FBCODE) or (b = ASTCLASS_FBCODE) then return TRUE

	'' Treat #defines/constants as equal, because they're often the same thing anyways
	'' (and then the formatting is the same no matter whether -disableconstants was used)
	#define check(x, y) if ((a = x) and (b = y)) or ((a = y) and (b = x)) then return TRUE
	check(ASTCLASS_PPDEFINE, ASTCLASS_CONST)

	function = FALSE
end function

private function hSkipStatementsInARow(byval i as ASTNODE ptr) as ASTNODE ptr
	select case i->class
	'' All parts of one #if block
	case ASTCLASS_PPIF
		do
			i = i->next
		loop while i andalso hIsPPElseOrEnd(i)
		return i

	case ASTCLASS_PPINCLUDE, ASTCLASS_INCLIB
		var astclass = i->class
		do
			i = i->next
		loop while i andalso (i->class = astclass)
		return i

	case ASTCLASS_PRAGMAONCE, ASTCLASS_RENAMELIST, ASTCLASS_TITLE, _
	     ASTCLASS_EXTERNBLOCKBEGIN, ASTCLASS_EXTERNBLOCKEND, _
	     ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, _
	     ASTCLASS_IFBLOCK, ASTCLASS_DOWHILE, ASTCLASS_WHILE
		return i->next

	'' Procedure body?
	case ASTCLASS_PROC
		if i->expr then
			return i->next
		end if
	end select

	'' Anything else (single-line declarations): collect all in a row as
	'' long as it's the same kind. However, small deviations are allowed,
	'' if it's just a few declarations in the middle of a bigger block.
	const BIGTHRESHOLD = 3
	var dominantclass = -1
	do
		var blockclass = i->class
		var length = 0
		var nxt = i
		do
			select case nxt->class
			case ASTCLASS_PPIF, ASTCLASS_INCLIB, ASTCLASS_PPINCLUDE, _
			     ASTCLASS_PRAGMAONCE, ASTCLASS_RENAMELIST, ASTCLASS_TITLE, _
			     ASTCLASS_EXTERNBLOCKBEGIN, ASTCLASS_EXTERNBLOCKEND, _
			     ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, _
			     ASTCLASS_IFBLOCK, ASTCLASS_DOWHILE, ASTCLASS_WHILE
				exit do
			case ASTCLASS_PROC
				if nxt->expr then
					exit do
				end if
			end select

			length += 1
			nxt = nxt->next
		loop while nxt andalso hAreSimilar(blockclass, nxt->class)

		'' Next statement can't be in a block?
		if length = 0 then
			exit do
		end if

		'' Block dominant due to its size?
		if length >= BIGTHRESHOLD then
			'' First?
			if dominantclass = -1 then
				dominantclass = i->class
			'' Others must have a similar astclass
			elseif hAreSimilar(dominantclass, i->class) = FALSE then
				exit do
			end if
		end if

		'' Skip the block
		i = nxt
	loop while i '' EOF?

	function = i
end function

''
'' Insert DIVIDERs (empty lines) between certain statements: i.e. some kind of
'' "pretty" auto-formatting, more or less.
''
''  * multiple similar single-line statements (e.g. a list of #defines or
''    functions) shouldn't be separated by empty lines
''
''  * lists of different kinds of symbols (e.g. #defines vs. functions) may be
''    separated by empty lines, but even that can be "wrong", e.g. if the
''    #defines are function wrappers.
''
''  * compound blocks (UDTs, function bodies, #if blocks) should be separated by
''    empty lines
''
''  * #pragma once/extern/end extern should be separated from the context
''
sub hlAutoAddDividers(byval ast as ASTNODE ptr)
	'' Recurse where needed
	scope
		var i = ast->head
		while i
			select case i->class
			case ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM, _
			     ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, _
			     ASTCLASS_SCOPEBLOCK, ASTCLASS_DOWHILE, ASTCLASS_WHILE
				hlAutoAddDividers(i)
			case ASTCLASS_IFBLOCK
				var part = i->head
				while part
					hlAutoAddDividers(part)
					part = part->next
				wend
			end select
			i = i->next
		wend
	end scope

	'' Insert dividers at this level
	var i = ast->head
	if i then
		do
			'' New block starting here; skip over it
			i = hSkipStatementsInARow(i)

			'' EOF? (not adding a divider there)
			if i = NULL then exit do

			'' Insert divider behind the block, to separate it from the next
			'' block. The divider isn't treated as part of a block.
			astInsert(ast, astNew(ASTCLASS_DIVIDER), i)
		loop
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Global (entire AST of an API) highlevel transformations, intended to run 1st
''
'' If any declarations are added to the AST here, care must be taken to set the
'' ASTNODE.location, so they can be assigned to the proper output .bi file.
''
sub hlGlobal(byval ast as ASTNODE ptr, byref api as ApiInfo)
	hl.api = @api

	hl.symbols = new THash(10, TRUE)
	astVisit(ast, @hlFixExpressions)
	delete hl.symbols
	hl.symbols = NULL

	if api.removeEmptyReservedDefines then
		hlRemoveEmptyReservedDefines(ast)
	end if

	if api.idopt(OPT_REMOVE).nonEmpty then
		hlApplyRemoveOption(ast, -1, OPT_REMOVE)
	end if

	if api.idopt(OPT_REMOVEPROC).nonEmpty then
		hlApplyRemoveOption(ast, ASTCLASS_PROC, OPT_REMOVEPROC)
	end if

	if api.idopt(OPT_REMOVEVAR).nonEmpty then
		hlApplyRemoveOption(ast, ASTCLASS_VAR, OPT_REMOVEVAR)
	end if

	if api.idopt(OPT_REMOVE1ST).nonEmpty then
		hlApplyRemove1st(ast)
	end if

	if api.idopt(OPT_REMOVE2ND).nonEmpty then
		hlApplyRemove2nd(ast)
	end if

	if api.idopt(OPT_DROPPROCBODY).nonEmpty then
		hlApplyDropProcBodyOptions(ast)
	end if

	astVisit(ast, @hlSetArraySizes)

	'' Apply -rename* options, if any
	if api.have_renames then
		astVisit(ast, @hlApplyRenameOption)
	end if

	'' Solve out array/function typedefs (not supported in FB),
	'' and any typedefs matched by an -expand option
	'' TODO: If possible, turn function typedefs into function pointer
	'' typedefs instead of solving them out
	'' All C declarations must be processed before processing #define
	'' bodies, because #define bodies may "forward-reference" typedefs etc.
	scope
		dim expander as TypedefExpander
		expander.walkDecls(ast)
		expander.walkDefines(ast)
	end scope

	'' Fix up parameters with certain special types:
	''   function => function pointer
	''   array => pointer to the element type
	''   jmp_buf => jmp_buf ptr (jmp_buf is an array type in C)
	astVisit(ast, @hlFixSpecialParameters)

	'' Handle char/string types
	scope
		hlFindStringOptionMatches(NULL, NULL, ast, 0)
		dim pass as CharStringPass
		pass.walkDecls(ast)
		pass.walkDefines(ast)
	end scope

	''
	'' Solve out various tags/typedefs
	''
	'' 1. Solve out tag ids (both explicitly given or auto-generated ones)
	'' if there is a typedef, but only once per tag id. (we don't want to
	'' lose additional typedefs). Auto-generated tag ids should be solved
	'' out where possible, since they're not part of the original API.
	'' Furthermore, C headers often use tag ids for internal purposes only,
	'' or specify them but don't use them, and programs are supposed to use
	'' the typedefs instead.
	''
	'' This can already take care of some exact/case-alias typedefs, but not
	'' necessarily all, e.g.:
	''      struct tagid A
	''      typedef a as tagid A
	''      typedef A as tagid A
	''  =>
	''      struct a
	''      typedef A as a
	''
	'' This can produce exact/case-alias typedefs:
	''      struct tagid A
	''      typedef b as tagid A
	''      typedef B as tagid A
	''  =>
	''      struct b
	''      typedef B as b
	''
	'' TODO: currently only auto-generated tag ids are solved out -
	'' enable solving out of normal tag ids too.
	''
	hlRenameJobsBegin()
	hlSolveOutTagIds(ast)
	hlRenameJobsEnd(ast)

	'' 2. Solve out remaining exact/case-alias typedefs - they're useless
	'' and/or impossible in FB, because it's case-insensitive and doesn't
	'' have separate tag/typedef namespaces.
	hlRenameJobsBegin()
	hlRemoveSameIdTypedefs(ast)
	hlRenameJobsEnd(ast)

	''
	'' Auto-add forward declarations for types (any UDT or typedef)
	''
	'' Even though typedefs can't be forward-referenced in C, the situation
	'' can occur in the fbfrog AST (e.g. due to tags/typedefs being solved
	'' out above), and it's possible in FB too...
	''
	'' In order to avoid adding false-positive forward decls for types like
	'' FILE/jmp_buf/time_t and tags such as "struct tm", we only add forward
	'' decls if we also have the corresponding definition in the AST. This
	'' can be overridden with -addforwarddecl.
	'' Presumably doing things this way is more convenient than the opposite
	'' solution: auto-adding forward decls for everything and maintaining
	'' lists for types/tags for which forward decls should *not* be added.
	''
	hl.typehash = new THash(8, FALSE)
	hlScanForForwardUsedTypes(ast)
	hlAddForwardDecls(ast)
	delete hl.typehash
	hl.typehash = NULL
	for i as integer = 0 to hl.typecount - 1
		deallocate(hl.types[i].id)
	next
	deallocate(hl.types)
	hl.types = NULL
	hl.typecount = 0
	hl.typeroom = 0

	if api.fixunsizedarrays then
		hlFixUnsizedArrays(ast)
	end if

	if api.disableconstants = FALSE then
		hlTurnDefinesIntoProperDeclarations(ast)
	end if

	if api.dropmacrobodyscopes then
		hlDropMacroBodyScopes(ast)
	end if

	hlRemoveVoidCasts(ast)

	'' Apply -moveabove options after renaming and removing, because then
	'' the movement is less depended on the .h content, and more on the
	'' .bi content, which is what matters. E.g., it's probably easier to
	'' move a declaration based on its name in the .bi file, rather than
	'' going through the .h file and find the original name there (thinking
	'' about tag names which are solved out in favour of typedefs, etc).
	if api.moveaboveoptions then
		var i = api.moveaboveoptions->head
		while i
			hlApplyMoveOption(ast, *i->text, *i->alias)
			i = i->next
		wend
	end if

	if api.idopt(OPT_UNDEFBEFOREDECL).nonEmpty then
		hlAddUndefsAboveDecls(ast)
	end if

	if api.idopt(OPT_IFNDEFDECL).nonEmpty then
		hlAddIfndefsAroundDecls(ast)
	end if

	hlProcs2Macros(ast)
end sub

''
'' .bi-file-specific highlevel transformations, intended to run on the
'' API-specific ASTs in each output .bi file.
''
sub hlFile(byval ast as ASTNODE ptr, byref api as ApiInfo, byref bioptions as ApiSpecificBiOptions)
	hl.api = @api
	hl.need_extern = FALSE
	hl.stdcalls = 0
	hl.cdecls = 0
	hl.mostusedcallconv = 0
	hl.uses_clong = FALSE
	hl.uses_clongdouble = FALSE

	'' Add Extern block
	''  * to preserve identifiers of global variables/procedures
	''  * to cover the most-used calling convention
	astVisit(ast, @hlCountCallConvs)
	if hl.need_extern then
		if hl.stdcalls > hl.cdecls then
			hl.mostusedcallconv = ASTATTRIB_STDCALL
		else
			hl.mostusedcallconv = ASTATTRIB_CDECL
		end if

		'' Remove the calling convention from all procdecls,
		'' the Extern block will take over
		astVisit(ast, @hlHideCallConv)

		var externblock = @"C"
		if hl.mostusedcallconv = ASTATTRIB_STDCALL then
			if api.windowsms then
				externblock = @"Windows-MS"
			else
				externblock = @"Windows"
			end if
		end if

		assert(ast->class = ASTCLASS_GROUP)
		astPrepend(ast, astNew(ASTCLASS_EXTERNBLOCKBEGIN, externblock))
		astAppend(ast, astNew(ASTCLASS_EXTERNBLOCKEND))
	end if

	'' If any symbols from this file were renamed, add a rename list at the top
	'' TODO: add it above #inclibs/#includes even
	if api.have_renames then
		var entries = hlBuildRenameList(ast)
		if entries->head then
			var renamelist = astNew(ASTCLASS_RENAMELIST, "The following symbols have been renamed:")
			astAppend(renamelist, entries)
			astPrepend(ast, renamelist)
		else
			astDelete(entries)
		end if
	end if

	'' Move existing #include statements outside the Extern block
	'' Remap *.h => *.bi
	'' Apply -removeinclude
	'' Remove duplicates
	scope
		dim pass as IncludePass
		pass.work(ast, bioptions)
	end scope

	'' Add #includes for "crt/long[double].bi" and "crt/wchar.bi" if the
	'' binding uses the clong[double]/wchar_t types
	astVisit(ast, @hlSearchSpecialDtypes)
	if hl.uses_clongdouble then
		astPrepend(ast, astNew(ASTCLASS_PPINCLUDE, "crt/longdouble.bi"))
	end if
	if hl.uses_clong then
		astPrepend(ast, astNew(ASTCLASS_PPINCLUDE, "crt/long.bi"))
	end if

	'' Prepend #inclibs/#undefs
	if bioptions.undefs then
		astPrepend(ast, bioptions.undefs)
		bioptions.undefs = NULL
	end if
	if bioptions.inclibs then
		astPrepend(ast, bioptions.inclibs)
		bioptions.inclibs = NULL
	end if

	astVisit(ast, @hlTurnWhile0IntoScope)
	astVisit(ast, @hlSolveOutUnnecessaryScopeBlocks)
	astVisit(ast, @hlCreateElseIfs)
end sub

function hlCountDecls(byval ast as ASTNODE ptr) as integer
	var n = 0

	var i = ast->head
	while i
		select case i->class
		case ASTCLASS_DIVIDER, ASTCLASS_PPINCLUDE, ASTCLASS_PPENDIF, _
		     ASTCLASS_EXTERNBLOCKBEGIN, ASTCLASS_EXTERNBLOCKEND, _
		     ASTCLASS_INCLIB, ASTCLASS_PRAGMAONCE, _
		     ASTCLASS_UNKNOWN, ASTCLASS_RENAMELIST, ASTCLASS_TITLE

		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE
			n += hlCountDecls(i)

		case ASTCLASS_PPDEFINE
			if (i->expr = NULL) orelse (i->expr->class <> ASTCLASS_UNKNOWN) then
				n += 1
			end if

		case else
			n += 1
		end select

		i = i->next
	wend

	function = n
end function

function hlCountTodos(byval ast as ASTNODE ptr) as integer
	var n = 0

	var i = ast->head
	while i
		select case i->class
		case ASTCLASS_UNKNOWN
			n += 1

		case ASTCLASS_PPIF, ASTCLASS_PPELSEIF, ASTCLASS_PPELSE, _
		     ASTCLASS_STRUCT, ASTCLASS_UNION, ASTCLASS_ENUM
			n += hlCountTodos(i)

		case ASTCLASS_PROC, ASTCLASS_PPDEFINE
			if i->expr then
				if i->expr->class = ASTCLASS_UNKNOWN then
					n += 1
				else
					n += hlCountTodos(i->expr)
				end if
			end if
		end select

		i = i->next
	wend

	function = n
end function
