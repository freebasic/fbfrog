'' Higher level code transformations

#include once "highlevel.bi"

#include once "emit.bi"
#include once "util-path.bi"
#include once "util.bi"

#include once "crt.bi"

declare sub expandTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr)

namespace hl
	dim shared api as ApiInfo ptr

	'' Used by forward declaration addition pass
	'' data = hl.types array index
	dim shared typehash as THash ptr

	'' Used by forward declaration addition pass
	type TYPENODE
		id		as zstring ptr  '' Type name
		definition	as AstNode ptr
		forwarduse	as integer
		firstuse	as AstNode ptr
	end type
	dim shared types as TYPENODE ptr
	dim shared as integer typecount, typeroom
	dim shared as AstNode ptr currentdecl

	'' Used by Extern block addition pass
	dim shared as integer need_extern, stdcalls, cdecls
	dim shared as integer mostusedcallconv

	'' Used by dtype use determination passes
	dim shared as integer uses_clong, uses_clongdouble
end namespace

type ForwardInfo
	defs as AstNode ptr ptr
	defcount as integer
	declare destructor()
	declare sub append(byval def as AstNode ptr)
	declare sub remove(byval i as integer)
end type

destructor ForwardInfo()
	deallocate(defs)
end destructor

sub ForwardInfo.append(byval def as AstNode ptr)
	var i = defcount
	defcount += 1
	defs = reallocate(defs, sizeof(*defs) * defcount)
	defs[i] = def
end sub

sub ForwardInfo.remove(byval i as integer)
	assert((i >= 0) and (i < defcount))
	var p = defs + i
	defcount -= 1
	'' Remove array element from the middle of the array: move all elements
	'' behind it to the front, by 1 slot, to close the gap.
	var tail = defcount - i
	if tail > 0 then
		memmove(p, p + 1, tail * sizeof(*defs))
	end if
end sub

type ExpressionFixUp
	symbols as THash = THash(10, TRUE)
	forwards as ForwardInfo
	declare sub collectSymbol(byval id as zstring ptr, byval dtype as integer)
	declare function calculateCTypes(byval n as AstNode ptr) as integer
	declare sub collectSymbolIfExprTypeIsKnown(byval id as zstring ptr, byval expr as AstNode ptr)
	declare sub maybeCollectSymbol(byval n as AstNode ptr)
	declare sub collectSymbolsAndCalculateTypes(byval n as AstNode ptr)
	declare sub handleForwards()
	declare function fixExpression(byval n as AstNode ptr, byval is_bool_context as integer) as AstNode ptr
	declare sub fixExpressions(byval n as AstNode ptr)
	declare operator let(byref as const ExpressionFixUp) '' unimplemented
end type

sub ExpressionFixUp.collectSymbol(byval id as zstring ptr, byval dtype as integer)
	select case dtype
	case TYPE_UDT, TYPE_PROC
		'' Don't bother tracking complex types - not worth it yet
		dtype = TYPE_NONE
	end select
	symbols.addOverwrite(id, cptr(any ptr, dtype))
end sub

private function typeIsNumeric(byval dtype as integer) as integer
	select case as const dtype
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
private function typeCBop(byval astkind as integer, byval a as integer, byval b as integer) as integer
	'' Logic/relational operations: result always is a 32bit int
	select case as const astkind
	case ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
	     ASTKIND_CEQ, ASTKIND_CNE, ASTKIND_CLT, _
	     ASTKIND_CLE, ASTKIND_CGT, ASTKIND_CGE, _
	     ASTKIND_CLOGNOT, ASTKIND_CDEFINED
		return TYPE_LONG

	'' sizeof() always returns size_t
	case ASTKIND_SIZEOF
		return TYPE_UINTEGER

	'' Bitshifts: Ignore the rhs type
	case ASTKIND_SHL, ASTKIND_SHR
		b = a

	case ASTKIND_IIF, _
	     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, _
	     ASTKIND_ADD, ASTKIND_SUB, ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD, _
	     ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_UNARYPLUS _

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

'' Returns FALSE if some id reference couldn't be resolved (i.e. we couldn't
'' determine the expression's type)
function ExpressionFixUp.calculateCTypes(byval n as AstNode ptr) as integer
	var allresolved = TRUE

	scope
		var i = n->head
		while i
			allresolved and= calculateCTypes(i)
			i = i->nxt
		wend
	end scope

	select case as const n->kind
	case ASTKIND_TEXT
		var item = symbols.lookup(n->text, hashHash(n->text))
		if item->s then
			n->setType(cint(item->data), NULL)
		else
			n->setType(TYPE_NONE, NULL)
			allresolved = FALSE
		end if

	case ASTKIND_CDEFINED
		n->setType(TYPE_LONG, NULL)

	case ASTKIND_IIF, ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
	     ASTKIND_CEQ, ASTKIND_CNE, ASTKIND_CLT, ASTKIND_CLE, ASTKIND_CGT, ASTKIND_CGE, _
	     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, ASTKIND_SHL, ASTKIND_SHR, _
	     ASTKIND_ADD, ASTKIND_SUB, ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD, _
	     ASTKIND_CLOGNOT, ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_UNARYPLUS, _
	     ASTKIND_SIZEOF
		'' IIF and BOPs will have two operands (ldtype/rdtype will be different),
		'' UOPs will have one operand (i.e. ldtype/rdtype will be the same)
		var ldtype = n->head->dtype
		var rdtype = n->tail->dtype
		n->setType(typeCBop(n->kind, ldtype, rdtype), NULL)
	end select

	function = allresolved
end function

sub ExpressionFixUp.collectSymbolIfExprTypeIsKnown(byval id as zstring ptr, byval expr as AstNode ptr)
	if expr->dtype <> TYPE_NONE then
		collectSymbol(id, expr->dtype)
	end if
end sub

sub ExpressionFixUp.maybeCollectSymbol(byval n as AstNode ptr)
	select case n->kind
	case ASTKIND_PPDEFINE
		if n->expr andalso n->expr->kind <> ASTKIND_SCOPEBLOCK then
			collectSymbolIfExprTypeIsKnown(n->text, n->expr)
		end if
	case ASTKIND_CONST
		if n->attrib and ASTATTRIB_ENUMCONST then
			'' enumconsts are always ints
			collectSymbol(n->text, TYPE_LONG)
		elseif n->expr then
			collectSymbolIfExprTypeIsKnown(n->text, n->expr)
		end if
	end select
end sub

''
'' Collect #defines/constants/enumconsts and determine their types so we can do
'' lookups and determine the type behind such identifiers used in expressions.
''
'' We walk from top to bottom. If a #define constant forward-references another
'' one defined later, we have to add it to a list for processing later in
'' handleForwards().
''
sub ExpressionFixUp.collectSymbolsAndCalculateTypes(byval n as AstNode ptr)
	if n->expr then
		var result = calculateCTypes(n->expr)
		if result = FALSE then
			'' Couldn't resolve some reference; store for later
			forwards.append(n)
		end if
	end if

	maybeCollectSymbol(n)

	if n->subtype then collectSymbolsAndCalculateTypes(n->subtype)
	if n->array   then collectSymbolsAndCalculateTypes(n->array  )
	if n->expr    then collectSymbolsAndCalculateTypes(n->expr   )

	var i = n->head
	while i
		collectSymbolsAndCalculateTypes(i)
		i = i->nxt
	wend
end sub

''
'' Handle the constants that forward-reference others and whose types couldn't
'' be determined in the first pass.
''
'' We process the list, in the hope that the forward-references can now be
'' resolved and removed from the list. It may be necessary to process the list
'' multiple times, until it's either empty or we can't resolve anything anymore.
''
sub ExpressionFixUp.handleForwards()
	while forwards.defcount > 0

		'' For each list entry...
		var listchanged = FALSE
		var i = 0
		while i < forwards.defcount
			var n = forwards.defs[i]

			if n->expr andalso calculateCTypes(n->expr) then
				'' The type of this one could now be determined
				forwards.remove(i)
				i -= 1
				listchanged = TRUE
			end if

			maybeCollectSymbol(n)
			i += 1
		wend

		if listchanged = FALSE then exit while
	wend
end sub

'' Fix up casts on string literals by adding an address-of operation:
''    cptr(foo ptr, "bar") => cptr(foo ptr, @"bar")
private sub hlFixAddrOfStrLit(byval n as AstNode ptr)
	var i = n->head
	while i
		hlFixAddrOfStrLit(i)
		i = i->nxt
	wend

	if (n->kind = ASTKIND_CAST) andalso _
	   ((n->head->kind = ASTKIND_STRING) or _
	    (n->head->kind = ASTKIND_STRCAT)) then
		var strlit = n->head
		n->unlink(n->head)
		strlit = astNew(ASTKIND_ADDROF, strlit)
		n->prepend(strlit)
	end if
end sub

private function hIsUlongCast(byval n as AstNode ptr) as integer
	function = n andalso (n->kind = ASTKIND_CAST) andalso (n->dtype = TYPE_ULONG)
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
private function hlAddMathCasts(byval parent as AstNode ptr, byval n as AstNode ptr) as AstNode ptr
	var i = n->head
	while i
		i = n->replace(i, hlAddMathCasts(n, i->clone()))
	wend

	select case as const n->kind
	case ASTKIND_IIF, ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
	     ASTKIND_CEQ, ASTKIND_CNE, ASTKIND_CLT, ASTKIND_CLE, ASTKIND_CGT, ASTKIND_CGE, _
	     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, ASTKIND_SHL, ASTKIND_SHR, _
	     ASTKIND_ADD, ASTKIND_SUB, ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD, _
	     ASTKIND_CLOGNOT, ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_UNARYPLUS, _
	     ASTKIND_SIZEOF
		'' Wrap uint32 operation in a cast, unless there already is a cast
		'' TODO: don't add cast if n is a cast
		if (n->dtype = TYPE_ULONG) and (not hIsUlongCast(parent)) then
			n = astNew(ASTKIND_CAST, n)
			n->dtype = TYPE_ULONG
		end if
	end select

	function = n
end function

private function astOpsC2FB(byval n as AstNode ptr, byval is_bool_context as integer) as AstNode ptr
	select case n->kind
	case ASTKIND_CLOGNOT, ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
	     ASTKIND_CEQ, ASTKIND_CNE, ASTKIND_IIF
		var l_is_bool_context = FALSE
		var r_is_bool_context = FALSE

		select case n->kind
		'' ! operand is treated as bool
		case ASTKIND_CLOGNOT
			l_is_bool_context = TRUE

		'' andalso/orelse operands are always treated as bools
		case ASTKIND_CLOGOR, ASTKIND_CLOGAND
			l_is_bool_context = TRUE
			r_is_bool_context = TRUE

		'' BOP operands may sometimes be in bool context:
		''    x = 0
		''    x <> 0
		'' (not x = -1 and x <> -1 because if checks check against 0 and <> 0,
		'' not 0 and -1)
		case ASTKIND_CEQ, ASTKIND_CNE
			if astIsCONSTI(n->tail) then
				l_is_bool_context = (n->tail->evalConstiAsInt64() = 0)
			end if

		'' iif() condition always is treated as bool
		case ASTKIND_IIF
			n->expr = astOpsC2FB(n->expr, TRUE)
		end select

		n->replace(n->head, astOpsC2FB(n->head->clone(), l_is_bool_context))
		if n->head <> n->tail then
			assert(n->head->nxt = n->tail)
			n->replace(n->tail, astOpsC2FB(n->tail->clone(), r_is_bool_context))
		end if

	case else
		var i = n->head
		while i
			i = n->replace(i, astOpsC2FB(i->clone(), FALSE))
		wend
	end select

	select case n->kind
	case ASTKIND_CLOGNOT, ASTKIND_CDEFINED, _
	     ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
	     ASTKIND_CEQ, ASTKIND_CNE, _
	     ASTKIND_CLT, ASTKIND_CLE, _
	     ASTKIND_CGT, ASTKIND_CGE

		select case n->kind
		case ASTKIND_CLOGNOT
			'' Turn C's "!x" into FB's "x = 0"
			n->kind = ASTKIND_EQ
			var zero = astNew(ASTKIND_CONSTI, "0")
			zero->setType(TYPE_LONG, NULL)
			n->append(zero)
		case ASTKIND_CDEFINED : n->kind = ASTKIND_DEFINED
		case ASTKIND_CLOGOR   : n->kind = ASTKIND_LOGOR
		case ASTKIND_CLOGAND  : n->kind = ASTKIND_LOGAND
		case ASTKIND_CEQ      : n->kind = ASTKIND_EQ
		case ASTKIND_CNE      : n->kind = ASTKIND_NE
		case ASTKIND_CLT      : n->kind = ASTKIND_LT
		case ASTKIND_CLE      : n->kind = ASTKIND_LE
		case ASTKIND_CGT      : n->kind = ASTKIND_GT
		case ASTKIND_CGE      : n->kind = ASTKIND_GE
		case else              : assert(FALSE)
		end select

		'' Turn -1|0 into 1|0 if the value may be used in math calculations etc.
		if is_bool_context = FALSE then
			n = astNew(ASTKIND_NEGATE, n)
		end if
	end select

	function = n
end function

private sub hlRemoveExpressionTypes(byval n as AstNode ptr)
	var i = n->head
	while i
		hlRemoveExpressionTypes(i)
		i = i->nxt
	wend

	if n->kind <> ASTKIND_CAST then
		select case as const n->kind
		case ASTKIND_IIF, ASTKIND_LOGOR, ASTKIND_LOGAND, _
		     ASTKIND_EQ, ASTKIND_NE, ASTKIND_LT, ASTKIND_LE, ASTKIND_GT, ASTKIND_GE, _
		     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, ASTKIND_SHL, ASTKIND_SHR, _
		     ASTKIND_ADD, ASTKIND_SUB, ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD, _
		     ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_UNARYPLUS, _
		     ASTKIND_SIZEOF, ASTKIND_DEFINED, ASTKIND_TEXT
			n->setType(TYPE_NONE, NULL)

		case ASTKIND_CLOGOR, ASTKIND_CLOGAND, _
		     ASTKIND_CEQ, ASTKIND_CNE, ASTKIND_CLT, ASTKIND_CLE, ASTKIND_CGT, ASTKIND_CGE, _
		     ASTKIND_CLOGNOT, ASTKIND_CDEFINED
			assert(FALSE)
		end select
	end if
end sub

function ExpressionFixUp.fixExpression(byval n as AstNode ptr, byval is_bool_context as integer) as AstNode ptr
	hlFixAddrOfStrLit(n)
	n = hlAddMathCasts(NULL, n)

	n = astOpsC2FB(n, is_bool_context)

	'' Forget about types again, for better merging (on one side
	'' an identifier may have unknown type, on the other side it
	'' could be known - but they should still be merged)
	hlRemoveExpressionTypes(n)

	function = n
end function

sub ExpressionFixUp.fixExpressions(byval n as AstNode ptr)
	if n->expr then
		'' TODO: shouldn't assume is_bool_context=TRUE for #define bodies
		var is_bool_context = FALSE
		select case n->kind
		case ASTKIND_PPDEFINE, ASTKIND_IFPART, ASTKIND_ELSEIFPART, _
		     ASTKIND_DOWHILE, ASTKIND_WHILE
			is_bool_context = TRUE
		end select

		n->expr = fixExpression(n->expr, is_bool_context)
	end if

	if n->subtype then fixExpressions(n->subtype)
	if n->array   then fixExpressions(n->array  )
	if n->expr    then fixExpressions(n->expr   )

	var i = n->head
	while i
		fixExpressions(i)
		i = i->nxt
	wend
end sub

private function isEmptyReservedDefine(byval n as AstNode ptr) as integer
	function = (n->kind = ASTKIND_PPDEFINE) andalso _
	           (n->paramcount < 0) andalso (n->expr = NULL) andalso _
	           strIsReservedIdInC(n->text)
end function

private sub hlRemoveEmptyReservedDefines(byval ast as AstNode ptr)
	var i = ast->head
	while i
		var nxt = i->nxt
		if isEmptyReservedDefine(i) then
			ast->remove(i)
		end if
		i = nxt
	wend
end sub

private sub hlApplyRemoveOption(byval ast as AstNode ptr, byval astkind as integer, byval opt as integer)
	var i = ast->head
	while i
		var nxt = i->nxt

		if (i->text <> NULL) and (astkind = -1) or (i->kind = astkind) then
			if hl.api->idopt(opt).matches(i->text) then
				ast->remove(i)
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyRemove1st(byval ast as AstNode ptr)
	dim removed as THash = THash(7, TRUE)
	var i = ast->head
	while i
		var nxt = i->nxt

		if i->text then
			if hl.api->idopt(tktokens.OPT_REMOVE1ST).matches(i->text) and _
			   (not removed.contains(i->text, hashHash(i->text))) then
				removed.addOverwrite(i->text, NULL)
				ast->remove(i)
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyRemove2nd(byval ast as AstNode ptr)
	dim found1st as THash = THash(7, FALSE)
	var i = ast->head
	while i
		var nxt = i->nxt

		if i->text then
			if hl.api->idopt(tktokens.OPT_REMOVE2ND).matches(i->text) then
				if found1st.contains(i->text, hashHash(i->text)) then
					ast->remove(i)
				else
					found1st.addOverwrite(i->text, NULL)
				end if
			end if
		end if

		i = nxt
	wend
end sub

private sub hlApplyDropProcBodyOptions(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if (i->kind = ASTKIND_PROC) and (i->expr <> NULL) then
			if hl.api->idopt(tktokens.OPT_DROPPROCBODY).matches(i->text) then
				delete i->expr
				i->expr = NULL
			end if
		end if

		i = i->nxt
	wend
end sub

private sub hlApplyMoveOption(byval ast as AstNode ptr, byref search as string, byref ref as string)
	dim as AstNode ptr decltomove, refdecl

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
		i = i->nxt
	wend

	if (decltomove <> NULL) and (refdecl <> NULL) then
		ast->unlink(decltomove)
		ast->insert(decltomove, refdecl)
	end if
end sub

'' Apply -setarraysize options, to arrays' "first" dimension
private function hlSetArraySizes(byval n as AstNode ptr) as integer
	if (n->text <> NULL) and (n->array <> NULL) then
		var dimension = n->array->head
		assert(dimension->kind = ASTKIND_DIMENSION)
		if dimension->expr->kind = ASTKIND_ELLIPSIS then
			dim size as zstring ptr = hl.api->setarraysizeoptions.lookupDataOrNull(n->text)
			if size then
				delete dimension->expr
				dimension->expr = astNewTEXT(size)
			end if
		end if
	end if
	function = TRUE
end function

private sub doRename(byval n as AstNode ptr, byval newid as zstring ptr)
	if n->kind = ASTKIND_PPINCLUDE then
		'' Allow -rename to affect PPINCLUDE nodes, but without giving them
		'' alias strings (which could prevent merging and would add them to renamelists)
		n->setText(newid)
	else
		n->renameSymbol(newid)
	end if
end sub

private function hApplyRenameOption(byval opt as integer, byval n as AstNode ptr) as integer
	dim as zstring ptr newid = hl.api->renameopt(opt).lookupDataOrNull(n->text)
	if newid then
		doRename(n, newid)
		function = TRUE
	end if
end function

private function hlApplyRenameOption(byval n as AstNode ptr) as integer
	static inside_macro as integer

	if n->text then
		'' Apply -rename[_] options to any declaration,
		'' but not string literals and such
		select case n->kind
		case ASTKIND_STRING, ASTKIND_CHAR

		case else
			hApplyRenameOption(tktokens.OPT_RENAME, n)

			if hl.api->idopt(tktokens.OPT_RENAME_).matches(n->text) then
				doRename(n, *n->text + "_")
			end if
		end select
	end if

	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		if n->subtype->attrib and ASTATTRIB_TAGID then
			hApplyRenameOption(tktokens.OPT_RENAMETAG, n->subtype)
		else
			hApplyRenameOption(tktokens.OPT_RENAMETYPEDEF, n->subtype)
		end if
	end if

	select case n->kind
	case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
		if n->text then
			hApplyRenameOption(tktokens.OPT_RENAMETAG, n)
		end if

	case ASTKIND_PROC
		if n->text then
			hApplyRenameOption(tktokens.OPT_RENAMEPROC, n)
		end if

	case ASTKIND_TYPEDEF
		hApplyRenameOption(tktokens.OPT_RENAMETYPEDEF, n)

	case ASTKIND_PPDEFINE
		hApplyRenameOption(tktokens.OPT_RENAMEDEFINE, n)

		if hl.api->renameopt(tktokens.OPT_RENAMEMACROPARAM).count > 0 then
			var param = n->head
			var renamed_param_here = FALSE
			while param
				assert(param->kind = ASTKIND_MACROPARAM)
				renamed_param_here or= hApplyRenameOption(tktokens.OPT_RENAMEMACROPARAM, param)
				param = param->nxt
			wend

			'' Visit body to update references to renamed parameters,
			'' but only if this #define actually has a parameter that was renamed,
			'' otherwise we'd incorrectly rename references to something else that
			'' happened to have the same name but isn't a parameter of this #define.
			if renamed_param_here and (n->expr <> NULL) then
				assert(inside_macro = FALSE)
				inside_macro = TRUE
				n->expr->visit(@hlApplyRenameOption)
				inside_macro = FALSE
			end if
		end if

	case ASTKIND_UNDEF
		hApplyRenameOption(tktokens.OPT_RENAMEDEFINE, n)

	case ASTKIND_TEXT
		hApplyRenameOption(tktokens.OPT_RENAMEPROC, n)
		hApplyRenameOption(tktokens.OPT_RENAMEDEFINE, n)
		if inside_macro then
			hApplyRenameOption(tktokens.OPT_RENAMEMACROPARAM, n)
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
	declare function lookup(byval id as zstring ptr) as AstNode ptr
	declare sub addOverwrite(byval n as AstNode ptr)
	declare operator let(byref as const TypedefTable) '' unimplemented
end type

destructor TypedefTable()
	'' Free the cloned TYPEDEFs
	for i as integer = 0 to table.room - 1
		var item = table.items + i
		if item->s then
			delete cptr(AstNode ptr, item->data)
		end if
	next
end destructor

function TypedefTable.lookup(byval id as zstring ptr) as AstNode ptr
	function = table.lookupDataOrNull(id)
end function

sub TypedefTable.addOverwrite(byval n as AstNode ptr)
	var datatype = n->clone()

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
		'' Free existing AstNode before overwriting, or else it would be leaked
		delete cptr(AstNode ptr, item->data)
		item->s = NULL '' became dangling due to this
	end if
	table.add(item, hash, datatype->text, datatype)
end sub

private sub oopsCantExpandTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr, byref reason as string)
	oops("can't expand " + typedef->dumpPrettyDecl(TRUE) + " into " + n->dumpPrettyDecl(TRUE) + ": " + reason)
end sub

'' Expand typedef assuming it's only a simple type, no array or procedure
private sub expandSimpleTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr)
	var newdtype = typeExpand(n->dtype, typedef->dtype)
	if newdtype = TYPE_NONE then
		oopsCantExpandTypedef(typedef, n, "too many pointers, or ref to ref")
	end if
	n->setType(newdtype, typedef->subtype)
end sub

private sub expandArrayTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr)
	'' Pointer to array?
	if typeGetPtrCount(n->dtype) > 0 then
		'' FB doesn't support pointers to arrays either, but we can drop the array type,
		'' such that the pointer only points to "single array element". That's pretty much
		'' the best translation we can do here, and better than nothing...
		expandSimpleTypedef(typedef, n)
		exit sub
	end if

	'' Expand the array typedef
	n->setType(typeGetConst(n->dtype) or typedef->dtype, typedef->subtype)
	if n->array = NULL then
		n->array = astNew(ASTKIND_ARRAY)
	end if
	n->array->append(typedef->array->cloneChildren())
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
private sub expandProcTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr)
	select case n->kind
	case ASTKIND_VAR, ASTKIND_FIELD
		'' Not a pointer?
		if typeGetPtrCount(n->dtype) = 0 then
			'' This is ok for vars/fields; they just become procedures themselves.
			'' (although with fields that can only really happen in C++ code)
			var proc = typedef->subtype
			assert(proc->kind = ASTKIND_PROC)

			'' Copy over the function result type (overwriting any previous cv-qualifiers),
			'' parameters, and callconv attributes
			n->kind = ASTKIND_PROC
			n->setType(proc->dtype, proc->subtype)
			assert(n->head = NULL)
			n->append(proc->cloneChildren())
			n->attrib or= proc->attrib

			exit sub
		end if

		'' var/field; it's a pointer to the function type; ok

	case ASTKIND_TYPEDEF, ASTKIND_PARAM
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
	n->setType(typeUnsetBaseConst(typeSetDt(n->dtype, TYPE_PROC)), typedef->subtype)
end sub

'' Expand a typedef into a declaration (or any AstNode really), assuming that
'' the declaration's dtype uses that typedef.
'' Expanding an array or procedure typedef requires special care, because then
'' the declaration becomes an array or a procedure.
'' It's not allowed/possible in C to have both at the same time (an array of
'' procedures), so we don't need to handle that.
private sub expandTypedef(byval typedef as AstNode ptr, byval n as AstNode ptr)
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
	declare sub walkDecls(byval n as AstNode ptr)
	declare sub walkDefines(byval n as AstNode ptr)
	declare operator let(byref as const TypedefExpander) '' unimplemented
end type

sub TypedefExpander.walkDecls(byval n as AstNode ptr)
	'' Ignore #defines for now; they will be processed later by walkDefines()
	if n->kind = ASTKIND_PPDEFINE then exit sub

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
		var nxt = i->nxt

		walkDecls(i)

		'' Register & drop typedefs
		if i->kind = ASTKIND_TYPEDEF then
			if (i->array <> NULL) orelse _
			   (typeGetDtAndPtr(i->dtype) = TYPE_PROC) orelse _
			   hl.api->idopt(tktokens.OPT_EXPAND).matches(i->text) then
				typedefs.addOverwrite(i)
				n->remove(i)
			end if
		end if

		i = nxt
	wend
end sub

sub TypedefExpander.walkDefines(byval n as AstNode ptr)
	var i = n->head
	while i

		if (i->kind = ASTKIND_PPDEFINE) and (i->expr <> NULL) then
			walkDecls(i->expr)
		end if

		i = i->nxt
	wend
end sub

private function hlFixSpecialParameters(byval n as AstNode ptr) as integer
	if n->kind = ASTKIND_PARAM then
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
			delete n->array
			n->array = NULL
			if typeGetPtrCount(n->dtype) = TYPEMAX_PTR then
				oops("too many pointers on " + n->dumpPrettyDecl(FALSE))
			end if
			n->dtype = typeAddrOf(n->dtype)
		end if
	end if
	function = TRUE
end function

private sub charArrayToFixLenStr(byval n as AstNode ptr)
	assert((typeGetDtAndPtr(n->dtype) = TYPE_ZSTRING) or (typeGetDtAndPtr(n->dtype) = TYPE_WSTRING))
	assert(n->array)

	'' Use the last (inner-most) array dimension as the fixed-length string size
	var d = n->array->tail
	assert(d->kind = ASTKIND_DIMENSION)
	assert(d->expr)
	assert(n->subtype = NULL)
	n->subtype = d->expr
	d->expr = NULL
	n->array->remove(d)

	'' If no dimensions left, remove the array type entirely
	if n->array->head = NULL then
		delete n->array
		n->array = NULL
	end if
end sub

private sub stringToByte(byval n as AstNode ptr)
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
			n->array = astNew(ASTKIND_ARRAY)
		end if

		'' add fix-len size as last (inner-most) array dimension
		var d = astNew(ASTKIND_DIMENSION)
		d->expr = n->subtype
		n->subtype = NULL
		n->array->append(d)
	end if
end sub

private sub byteToString(byval n as AstNode ptr)
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
		byval parentparent as AstNode ptr, _
		byval parent as AstNode ptr, _
		byval n as AstNode ptr, _
		byval index as integer _
	)

	'' TODO: don't call DeclPatterns.matches() if CharStringPass won't care anyways
	'' 1. only check for match on nodes that have string/byte dtype
	'' 2. don't try 2nd lookup if first one succeeded (-string and -nostring
	''    on the same decl doesn't make sense anyways)
	'' but this only works if string typedefs are already expanded,
	'' so also try matching on nodes with UDT type?

	if hl.api->patterns(tktokens.OPT_NOSTRING).matches(parentparent, parent, n, index) then
		n->attrib or= ASTATTRIB_NOSTRING
	end if

	if hl.api->patterns(tktokens.OPT_STRING).matches(parentparent, parent, n, index) then
		n->attrib or= ASTATTRIB_STRING
	end if

	if n->subtype then hlFindStringOptionMatches(parent, n, n->subtype, 0)
	if n->array   then hlFindStringOptionMatches(parent, n, n->array  , 0)
	if n->expr    then hlFindStringOptionMatches(parent, n, n->expr   , 0)

	var i = n->head
	var childindex = 0
	while i
		hlFindStringOptionMatches(parent, n, i, childindex)
		i = i->nxt
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
'' it as byte. For this, it is necessary to check the contexts where char
'' typedefs are used and possibly expand them. Expansion is needed for single
'' chars so they can be turned into byte/wchar_t -- this also covers the case
'' of char arrays that should be turned into fixed-length strings.
''
'' Furthermore, we support some options to override the defaults:
''   -string    =>  Force "[un]signed char" to be treated as zstring. If it's
''                  a typedef, it now needs to be expanded to get the
''                  context-specific behaviour like normal string typedefs.
''   -nostring  =>  prevent char/wchar_t from being treated as zstring/wstring
''
type CharStringPass
	typedefs as TypedefTable
	declare sub walkDecls(byval n as AstNode ptr)
	declare sub walkDefines(byval n as AstNode ptr)
	declare operator let(byref as const CharStringPass) '' unimplemented
end type

#define isAffectedByNoString(n) (((n)->attrib and ASTATTRIB_NOSTRING) <> 0)
#define isAffectedByString(n)   (((n)->attrib and ASTATTRIB_STRING  ) <> 0)

private function needsStringTypedefExpansion(byval n as AstNode ptr) as integer
	function = (typeGetPtrCount(n->dtype) = 0)
end function

sub CharStringPass.walkDecls(byval n as AstNode ptr)
	'' Ignore string/char literals, which also have string/char types, but
	'' shouldn't be changed.
	'' Ignore #defines for now; they will be processed later by walkDefines()
	select case n->kind
	case ASTKIND_STRING, ASTKIND_CHAR, ASTKIND_PPDEFINE
		exit sub
	end select

	'' Expand "char/wchar_t" typedefs in this decl, unless the typedef or decl is affected by -nostring
	'' Expand "[un]signed char" typedef into decls affected by -string
	'' We don't expand pointer typedefs though (e.g. "char/wchar_t *" or "[un]signed char *")
	'' because they have enough context by themselves (we already know they're pointers).
	if (typeGetDt(n->dtype) = TYPE_UDT) then
		assert(astIsTEXT(n->subtype))
		var typedef = typedefs.lookup(n->subtype->text)
		if typedef then
			select case typeGetDtAndPtr(typedef->dtype)
			case TYPE_ZSTRING, TYPE_WSTRING
				if (not isAffectedByNoString(n)) and (not isAffectedByNoString(typedef)) and _
				   needsStringTypedefExpansion(n) then
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

		elseif n->kind <> ASTKIND_TYPEDEF then
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
		if (n->kind = ASTKIND_TYPEDEF) or _
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
	if n->kind = ASTKIND_TYPEDEF then
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
		i = i->nxt
	wend
end sub

sub CharStringPass.walkDefines(byval n as AstNode ptr)
	var i = n->head
	while i

		if (i->kind = ASTKIND_PPDEFINE) and (i->expr <> NULL) then
			walkDecls(i->expr)
		end if

		i = i->nxt
	wend
end sub

type RenameJobs
	'' data = TEXT AstNode holding the new id/alias
	table as THash = THash(6, TRUE)
	declare function add(byval oldid as zstring ptr, byval newname as AstNode ptr) as integer
	declare sub maybeApply(byval n as AstNode ptr)
	declare sub walkAndApply(byval n as AstNode ptr)
	declare destructor()
	declare operator let(byref as const RenameJobs) '' unimplemented
end type

function RenameJobs.add(byval oldid as zstring ptr, byval newname as AstNode ptr) as integer
	var hash = hashHash(oldid)
	var item = table.lookup(oldid, hash)
	if item->s then
		'' Allow overwriting duplicate typedefs (if both oldid/newid are the same),
		'' but not if the newid differs (then it's a separate typedef which has to be
		'' preserved - we can't rename A => C when already renaming A => B).
		dim as AstNode ptr prevnewid = item->data
		if ucase(*prevnewid->text, 1) <> ucase(*newname->text, 1) then
			return FALSE
		end if
	end if

	'' When renaming, we don't just overwrite the id, but also copy the
	'' origid. This way, if an UDT is given a renamed typedef's name, the
	'' UDT will become "renamed" too, and thus the type will continue to
	'' appear in renamelists.
	var newid = astNewTEXT(newname->text)
	newid->copyOrigId(newname)

	if item->s then
		'' Free existing newid if there already is a renamejob for this oldid,
		'' then the new newid can be stored
		delete cptr(AstNode ptr, item->data)
		item->data = newid
	else
		table.add(item, hash, oldid, newid)
	end if

	function = TRUE
end function

sub RenameJobs.maybeApply(byval n as AstNode ptr)
	'' Is there a renamejob for this oldid?
	dim as AstNode ptr newid = table.lookupDataOrNull(n->text)
	if newid then
		'' Change the id if needed
		if *n->text <> *newid->text then
			n->setText(newid->text)
		end if

		'' Overwrite origid too
		n->copyOrigId(newid)

		'' Remove attributes even if id strictly speaking doesn't change
		n->attrib and= not (ASTATTRIB_TAGID or ASTATTRIB_GENERATEDID)
	end if
end sub

'' Check all type names (definitions and references): If there is a rename job
'' for an id, then change it to use the newid.
sub RenameJobs.walkAndApply(byval n as AstNode ptr)
	if typeGetDt(n->dtype) = TYPE_UDT then
		assert(astIsTEXT(n->subtype))
		maybeApply(n->subtype)
	end if

	select case n->kind
	case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, ASTKIND_TYPEDEF
		if n->text then
			maybeApply(n)
		end if
	end select

	if n->subtype then walkAndApply(n->subtype)
	if n->array   then walkAndApply(n->array  )
	if n->bits    then walkAndApply(n->bits   )
	if n->expr    then walkAndApply(n->expr   )

	var i = n->head
	while i
		walkAndApply(i)
		i = i->nxt
	wend
end sub

destructor RenameJobs()
	'' Free the newids
	for i as integer = 0 to table.room - 1
		var item = table.items + i
		if item->s then
			delete cptr(AstNode ptr, item->data)
		end if
	next
end destructor

private sub hlSolveOutTagIds(byval n as AstNode ptr, byref renames as RenameJobs)
	var i = n->head
	while i
		var nxt = i->nxt

		if i->kind = ASTKIND_TYPEDEF then
			if i->dtype = TYPE_UDT then
				assert(astIsTEXT(i->subtype))
				'if i->subtype->attrib and ASTATTRIB_TAGID then
				if i->subtype->attrib and ASTATTRIB_GENERATEDID then
					if renames.add(i->subtype->text, i) then
						n->remove(i)
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
private sub hlRemoveSameIdTypedefs(byval n as AstNode ptr, byref renames as RenameJobs)
	var i = n->head
	while i
		var nxt = i->nxt

		if i->kind = ASTKIND_TYPEDEF then
			if i->dtype = TYPE_UDT then
				assert(astIsTEXT(i->subtype))
				if ucase(*i->subtype->text, 1) = ucase(*i->text, 1) then
					if renames.add(i->subtype->text, i) then
						n->remove(i)
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

private function hlCollectForwardUses(byval n as AstNode ptr) as integer
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
		if n->kind <> ASTKIND_TYPEDEF then
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
	function = (n->kind <> ASTKIND_PPDEFINE)
end function

private sub hlScanForForwardUsedTypes(byval ast as AstNode ptr)
	var i = ast->head
	while i

		'' If this is an UDT definition (tag body or typedef), register the type as "defined"
		'' (before scanning the struct fields for potential forward-references, so that "recursive"
		'' references to this UDT aren't seen as forward references)
		select case i->kind
		case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, ASTKIND_TYPEDEF
			'' Named? (enums can be anonymous)
			if i->text then
				'' Mark type as defined
				var typeindex = hLookupOrAddType(i->text)
				hl.types[typeindex].definition = i
			end if
		end select

		hl.currentdecl = i
		i->visit(@hlCollectForwardUses)
		hl.currentdecl = NULL

		i = i->nxt
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
			function = hl.api->idopt(tktokens.OPT_ADDFORWARDDECL).matches(typ.id)
		end if
	end if
end function

'' Add forward decls for forward-referenced types
private sub hlAddForwardDecls(byval ast as AstNode ptr)
	for i as integer = 0 to hl.typecount - 1
		var typ = hl.types + i
		if hShouldAddForwardDeclForType(*typ) then
			var fwdid = *typ->id + "_"
			var fwd = astNew(ASTKIND_TYPEDEF, typ->id)
			fwd->dtype = TYPE_UDT
			fwd->subtype = astNewTEXT(fwdid)
			fwd->location = typ->firstuse->location
			ast->insert(fwd, typ->firstuse)
			if typ->definition then
				assert(*typ->id = *typ->definition->text)
				typ->definition->setText(fwdid)
				'' Move the type's alias/origid onto the forward decl,
				'' since that now declares that type and should be what
				'' appears in renamelists.
				fwd->takeAliasAndOrigId(typ->definition)
			end if
		end if
	next
end sub

private function hIsUnsizedArray(byval n as AstNode ptr) as integer
	if n->array then
		assert(n->array->kind = ASTKIND_ARRAY)
		assert(n->array->head->kind = ASTKIND_DIMENSION)
		function = n->array->has1Child() and (n->array->head->expr->kind = ASTKIND_ELLIPSIS)
	end if
end function

private sub hFixUnsizedArray(byval ast as AstNode ptr, byval n as AstNode ptr)
	if hIsUnsizedArray(n) then
		var id = *n->text
		var tempid = "__" + id

		var def = astNewPPDEFINE(id)
		def->paramcount = 1
		def->append(astNew(ASTKIND_MACROPARAM, "i"))
		def->expr = astNewTEXT("((@" + tempid + ")[i])")
		def->location = n->location
		ast->insert(def, n)

		def->takeOrigId(n)
		n->renameSymbolWithoutSettingOrigId(tempid)

		delete n->array
		n->array = NULL

		exit sub
	end if

	select case typeGetDtAndPtr(n->dtype)
	case TYPE_ZSTRING, TYPE_WSTRING
		if n->subtype andalso (n->subtype->kind = ASTKIND_ELLIPSIS) then
			var id = *n->text
			var tempid = "__" + id

			var def = astNewPPDEFINE(id)
			def->expr = astNewTEXT("(*cptr(" + emitFbType(typeAddrOf(n->dtype), NULL) + ", @" + tempid + "))")
			def->location = n->location
			ast->insert(def, n->nxt)

			def->takeOrigId(n)
			n->renameSymbolWithoutSettingOrigId(tempid)

			delete n->subtype
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
private sub hlFixUnsizedArrays(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if i->kind = ASTKIND_VAR then
			hFixUnsizedArray(ast, i)
		end if

		i = i->nxt
	wend
end sub

private sub hlDropMacroBodyScopes(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if (i->kind = ASTKIND_PPDEFINE) andalso _
		   i->expr andalso (i->expr->kind = ASTKIND_SCOPEBLOCK) then
			if i->expr->head = NULL then
				delete i->expr
				i->expr = NULL
			elseif i->expr->has1Child() then
				var stmt = i->expr->head->clone()
				delete i->expr
				i->expr = stmt
			end if
		end if

		i = i->nxt
	wend
end sub

'' Remove void casts, e.g.:
''    #define FOO (void)f()
'' =>
''    #define FOO f()
private sub hlRemoveVoidCasts(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if (i->kind = ASTKIND_PPDEFINE) andalso _
		   i->expr andalso _
		   (i->expr->kind = ASTKIND_CAST) andalso _
		   (i->expr->dtype = TYPE_ANY) then
			var expr = i->expr->head->clone()
			delete i->expr
			i->expr = expr
		end if

		i = i->nxt
	wend
end sub

private sub hlAddUndefsAboveDecls(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if (i->kind <> ASTKIND_UNDEF) and (i->text <> NULL) then
			if hl.api->idopt(tktokens.OPT_UNDEFBEFOREDECL).matches(i->text) then
				var undef = astNew(ASTKIND_UNDEF, i->text)
				undef->location = i->location
				ast->insert(undef, i)
			end if
		end if

		i = i->nxt
	wend
end sub

private sub hlAddIfndefsAroundDecls(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if (i->kind <> ASTKIND_UNDEF) andalso _
		   i->text andalso hl.api->idopt(tktokens.OPT_IFNDEFDECL).matches(i->text) then
			i->attrib or= ASTATTRIB_IFNDEFDECL
		end if

		i = i->nxt
	wend
end sub

type ParamUsageChecker
	proc as AstNode ptr
	have_multiple_uses as integer
	mark_as_macroparam as integer
	declare constructor(byval proc as AstNode ptr, byval mark_as_macroparam as integer)
	declare function lookupParam(byval id as zstring ptr) as AstNode ptr
	declare sub onTEXT(byval n as AstNode ptr)
	declare destructor()
end type

constructor ParamUsageChecker(byval proc as AstNode ptr, byval mark_as_macroparam as integer)
	this.proc = proc
	this.mark_as_macroparam = mark_as_macroparam
end constructor

function ParamUsageChecker.lookupParam(byval id as zstring ptr) as AstNode ptr
	var param = proc->head
	while param
		if param->text andalso (*param->text = *id) then
			return param
		end if
		param = param->nxt
	wend
	function = NULL
end function

sub ParamUsageChecker.onTEXT(byval n as AstNode ptr)
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
		param = param->nxt
	wend
end destructor

dim shared paramusage as ParamUsageChecker ptr

private function collectParamUses(byval n as AstNode ptr) as integer
	if astIsTEXT(n) then
		paramusage->onTEXT(n)
	end if
	function = TRUE
end function

'' Check that each parameter is only used once by the expression,
'' unless -forcefunction2macro was given
private function allowedToTurnProc2Macro(byval proc as AstNode ptr, byval expr as AstNode ptr) as integer
	if hl.api->idopt(tktokens.OPT_FORCEFUNCTION2MACRO).matches(proc->text) then
		return TRUE
	end if

	paramusage = new ParamUsageChecker(proc, FALSE)
	if paramusage = NULL then
		oops("ParamUsageChecker memory allocation failed")
	end if
	expr->visit(@collectParamUses)
	var have_multiple_uses = paramusage->have_multiple_uses
	delete paramusage
	paramusage = NULL

	function = (not have_multiple_uses)
end function

'' Check whether a proc can be turned into a #define, and do it if possible.
''  - it must only return an expression; no other statements in the body.
''  - parameters (if any) can be used only once (otherwise they'd be evaluated
''    multiple times if turned into macro parameters).
private sub maybeProc2Macro(byval proc as AstNode ptr)
	assert(proc->kind = ASTKIND_PROC)

	var body = proc->expr
	assert(body->kind = ASTKIND_SCOPEBLOCK)

	'' just a RETURN?
	if body->hasOnlyChild(ASTKIND_RETURN) then
		var ret = body->head

		'' does it return an expression?
		if ret->head then
			if allowedToTurnProc2Macro(proc, ret->head) then
				'' Turn proc into #define
				''  - keep parameters
				''  - cast expression to function's result type if there
				''    is no cast yet. This may or may not be useful...

				'' Wrap references to macro parameters in parentheses
				paramusage = new ParamUsageChecker(proc, TRUE)
				if paramusage = NULL then
					oops("ParamUsageChecker memory allocation failed")
				end if
				ret->head->visit(@collectParamUses)
				delete paramusage
				paramusage = NULL

				proc->kind = ASTKIND_PPDEFINE

				'' Params: turn into MACROPARAMs, forget dtypes
				var param = proc->head
				while param
					param->kind = ASTKIND_MACROPARAM
					param->setType(TYPE_NONE, NULL)
					param = param->nxt
				wend

				'' Macro body, add cast if there is none yet
				var expr = ret->head->clone()
				if expr->isCastTo(proc->dtype, proc->subtype) = FALSE then
					expr = astNew(ASTKIND_CAST, expr)
					expr->setType(proc->dtype, proc->subtype)
				end if

				'' Proc body -> macro body
				delete proc->expr
				proc->expr = expr

				'' Forget function dtype & callconv
				proc->setType(TYPE_NONE, NULL)
				proc->attrib and= not ASTATTRIB__CALLCONV
			end if
		end if
	end if
end sub

private sub hlProcs2Macros(byval ast as AstNode ptr)
	var i = ast->head
	while i

		'' proc with a body?
		if (i->kind = ASTKIND_PROC) andalso i->expr then
			maybeProc2Macro(i)
		end if

		i = i->nxt
	wend
end sub

private sub maybeRemoveSelfDefine(byval ast as AstNode ptr, byval i as AstNode ptr, byval aliasedid as zstring ptr)
	'' Aliasing same id?
	if *i->text = *aliasedid then
		ast->remove(i)
	end if
end sub

'' Some C headers use self-aliasing #defines, for example
''    typedef int A;
''    #define A A
'' these aren't needed/useful/possible in FB, and should be removed completely.
private sub hlRemoveSelfDefines(byval ast as AstNode ptr)
	var i = ast->head
	while i
		var nxt = i->nxt

		'' Process alias #defines
		if (i->kind = ASTKIND_PPDEFINE) andalso (i->paramcount = -1) andalso i->expr then
			select case i->expr->kind
			case ASTKIND_TEXT
				maybeRemoveSelfDefine(ast, i, i->expr->text)
			case ASTKIND_DATATYPE
				var datatype = i->expr
				if datatype->dtype = TYPE_UDT then
					assert(astIsTEXT(datatype->subtype))
					maybeRemoveSelfDefine(ast, i, datatype->subtype->text)
				end if
			end select
		end if

		i = nxt
	wend
end sub

''
'' Turn #defines into proper declarations, for example alias #defines:
''    type T1 as integer
''    #define T2 T1
''    declare sub function1
''    #define function2 function1
'' =>
''    type T1 as integer
''    type T2 as T1
''    declare sub function1
''    declare sub function2 alias "function1"
''
'' but also typedefs:
''    #define T integer ptr
'' =>
''    type T as integer ptr
''
'' The advantage is that proper declarations are less intrusive than #defines
'' in the global namespace.
''
'' #defines are commonly used to provide aliases for constants, typedefs and
'' functions (e.g. in the Windows API headers). We can have two kinds of alias
'' defines:
''   1. identifier aliases (macro body is a TEXT node)
''   2. type aliases (macro body is a DATATYPE node)
''
'' Normally the main declaration comes first and the alias #defines follow it,
'' but if it's the other way round (which is legal C code afterall) we may have
'' to move the alias declarations behind the main one.
''  - There can be multiple forward alias #defines for one declaration
''    (i.e. we have to keep a list of #defines per identifier, and then move
''    all of them behind the declaration)
''  - What if there are multiple declarations using the same id? It can happen
''    for example with an UDT and procedure of the same name.  We don't know
''    which the alias refers to, it can stand for both depending on where it's
''    used. Thus, such alias #defines mustn't be turned into declarations.
''    For this, we have to do a separate pass that just counts the declarations
''    per identifier, so we can lookup that count during the main pass.
''  - If the alias is for an enumconst, the #define must be inserted behind the
''    enum body, not inside it.
''  - Typedef #defines generally don't need to be moved, because typedefs allow
''    forward references anyways. There still can be cases where it's needed
''    though, for example:
''        #define T sub(byval as UDT)
''        type UDT ...
''    because FB doesn't allow byval parameters to be forward references.
''    But this is probably not a big deal in practice, and will be caught by fbc
''    anyways.
''

type Define2Decl
	counts as THash = THash(12, FALSE)
	decls as THash = THash(12, FALSE)
	forwards as THash = THash(8, FALSE)
	declare operator let(byref as const Define2Decl) '' unimplemented
	declare destructor()
	declare sub countDecl(byval id as zstring ptr)
	declare sub countDecls(byval ast as AstNode ptr)
	declare function mayTurnDefs2Decl(byval decl as AstNode ptr) as integer
	declare function exprInvolvesUndeclaredId(byval n as AstNode ptr) as integer
	declare function isConstantExpr(byval n as AstNode ptr) as integer
	declare sub addForward(byval aliasedid as zstring ptr, byval def as AstNode ptr)
	declare sub handleAliasDefine(byval n as AstNode ptr, byval aliasedid as zstring ptr)
	declare sub workDecl(byval ast as AstNode ptr, byval decl as AstNode ptr, byval insertbehind as AstNode ptr)
	declare sub work(byval ast as AstNode ptr)
end type

destructor Define2Decl()
	for i as integer = 0 to forwards.room - 1
		var item = forwards.items + i
		if item->s andalso item->data then
			delete cptr(ForwardInfo ptr, item->data)
		end if
	next
end destructor

sub Define2Decl.countDecl(byval id as zstring ptr)
	var idhash = hashHash(id)
	var item = counts.lookup(id, idhash)
	if item->s then
		'' Entry already exists, increment counter
		item->data = cptr(any ptr, cint(item->data) + 1)
	else
		'' New entry, start with count=1
		counts.add(item, idhash, id, cptr(any ptr, 1))
	end if
end sub

'' 1st pass: Count declarations per identifier
'' (This has to count even #defines, at least those that may be turned into
'' declarations later, because they themselves will be considered as alias-able
'' declarations)
sub Define2Decl.countDecls(byval ast as AstNode ptr)
	var i = ast->head
	while i

		if i->text then
			countDecl(i->text)
		end if

		if i->kind = ASTKIND_ENUM then
			'' Register enum constants too
			var enumconst = i->head
			while enumconst
				if enumconst->kind = ASTKIND_CONST then
					countDecl(enumconst->text)
				end if
				enumconst = enumconst->nxt
			wend
		end if

		i = i->nxt
	wend
end sub

function Define2Decl.mayTurnDefs2Decl(byval decl as AstNode ptr) as integer
	assert(counts.contains(decl->text, hashHash(decl->text)))
	var count = cint(counts.lookupDataOrNull(decl->text))
	function = (count = 1)
end function

function Define2Decl.exprInvolvesUndeclaredId(byval n as AstNode ptr) as integer
	if astIsTEXT(n) then
		if decls.contains(n->text, hashHash(n->text)) = FALSE then return TRUE
	end if

	if n->subtype then if exprInvolvesUndeclaredId(n->subtype) then return TRUE
	if n->array   then if exprInvolvesUndeclaredId(n->array  ) then return TRUE
	if n->expr    then if exprInvolvesUndeclaredId(n->expr   ) then return TRUE
	var i = n->head
	while i
		if exprInvolvesUndeclaredId(i) then return TRUE
		i = i->nxt
	wend
	function = FALSE
end function

''
'' Checks whether an expression is a compile-time constant, such that it could
'' be used in an FB constant declaration (const FOO = <...>).
''
'' Thus, it shouldn't contain any function calls, addrof/deref operations, or
'' preprocessor operations, etc., but only a selected set of known-to-be-safe
'' math operations & co, and atoms must be number literals or identifiers
'' referring to other, known constants.
''
'' TODO: allow more here:
''  * string/char literals
''  * "sizeof(datatype)", but not "datatype"
''
function Define2Decl.isConstantExpr(byval n as AstNode ptr) as integer
	select case n->kind
	'' Atoms
	case ASTKIND_CONSTI, ASTKIND_CONSTF

	case ASTKIND_TEXT
		'' Referring to another constant?
		dim as AstNode ptr decl = decls.lookupDataOrNull(n->text)
		if (decl = NULL) orelse (decl->kind <> ASTKIND_CONST) then exit function

	'' UOPs
	case ASTKIND_NOT, ASTKIND_NEGATE, ASTKIND_CAST
		if isConstantExpr(n->head) = FALSE then exit function

	'' BOPs
	case ASTKIND_LOGOR, ASTKIND_LOGAND, _
	     ASTKIND_OR, ASTKIND_XOR, ASTKIND_AND, _
	     ASTKIND_EQ, ASTKIND_NE, _
	     ASTKIND_LT, ASTKIND_LE, _
	     ASTKIND_GT, ASTKIND_GE, _
	     ASTKIND_SHL, ASTKIND_SHR, _
	     ASTKIND_ADD, ASTKIND_SUB, _
	     ASTKIND_MUL, ASTKIND_DIV, ASTKIND_MOD
		if isConstantExpr(n->head) = FALSE then exit function
		if isConstantExpr(n->tail) = FALSE then exit function

	'' IIF
	case ASTKIND_IIF
		if isConstantExpr(n->expr) = FALSE then exit function
		if isConstantExpr(n->head) = FALSE then exit function
		if isConstantExpr(n->tail) = FALSE then exit function

	case else
		exit function
	end select

	function = TRUE
end function

sub Define2Decl.addForward(byval aliasedid as zstring ptr, byval def as AstNode ptr)
	var aliasedidhash = hashHash(aliasedid)
	var item = forwards.lookup(aliasedid, aliasedidhash)
	if item->s then
		cptr(ForwardInfo ptr, item->data)->append(def)
	else
		var info = new ForwardInfo
		if info = NULL then
			oops("ForwardInfo memory allocation failed")
		end if
		info->append(def)
		forwards.add(item, aliasedidhash, aliasedid, info)
	end if
end sub

private sub copyAttribsTypeEtcAndChooseAlias(byval n as AstNode ptr, byval decl as AstNode ptr)
	n->kind = decl->kind
	n->attrib or= decl->attrib and (ASTATTRIB_DLLIMPORT or ASTATTRIB__CALLCONV or ASTATTRIB_EXTERN)
	n->setType(decl->dtype, decl->subtype)
	if decl->array then
		n->array = decl->array->clone()
	end if
	n->append(decl->cloneChildren())
	n->setAlias(iif(decl->alias_, decl->alias_, decl->text))
end sub

private sub astDropExpr(byval n as AstNode ptr)
	delete n->expr
	n->expr = NULL
end sub

private sub turnDefine2Decl(byval n as AstNode ptr, byval decl as AstNode ptr)
	'' This #define aliases a previous declaration
	select case decl->kind
	case ASTKIND_CONST
		'' const A = ...
		'' #define B A    =>  const B = A
		n->kind = ASTKIND_CONST

	case ASTKIND_TYPEDEF, ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
		'' type A as ...
		'' #define B A    =>  type B as A
		'' Note: typedef-style #defines (with DATATYPE body) are handled
		'' by turnDefine2Typedef() now, but this case here can still
		'' happen, if a define body refers to a struct but wasn't parsed
		'' as data type - i.e. because it's just the tag id, and the
		'' "struct" keyword was missing.
		n->kind = ASTKIND_TYPEDEF
		n->dtype = TYPE_UDT
		n->subtype = astNewTEXT(decl->text)
		if decl->array then
			n->array = decl->array->clone()
		end if
		astDropExpr(n)

	case ASTKIND_PROC
		'' declare sub/function A
		'' declare sub/function C alias "X"
		'' #define B A    =>    declare sub/function B alias "A"
		'' #define D C    =>    declare sub/function D alias "X"
		copyAttribsTypeEtcAndChooseAlias(n, decl)
		astDropExpr(n)

	case ASTKIND_VAR
		if decl->attrib and (ASTATTRIB_LOCAL or ASTATTRIB_STATIC) then exit sub

		'' extern A as ...
		'' extern C alias "X" as ...
		'' #define B A    =>    extern B alias "A"
		'' #define D C    =>    extern D alias "X"
		copyAttribsTypeEtcAndChooseAlias(n, decl)
		astDropExpr(n)
	end select
end sub

private sub turnDefine2Typedef(byval n as AstNode ptr)
	'' #define A dtype    =>    type A as dtype
	var datatype = n->expr
	assert(datatype->kind = ASTKIND_DATATYPE)
	n->kind = ASTKIND_TYPEDEF
	n->dtype = datatype->dtype
	n->subtype = datatype->subtype
	datatype->subtype = NULL
	astDropExpr(n)
end sub

sub Define2Decl.handleAliasDefine(byval def as AstNode ptr, byval aliasedid as zstring ptr)
	dim as AstNode ptr decl = decls.lookupDataOrNull(aliasedid)
	if decl then
		turnDefine2Decl(def, decl)
	else
		'' This #define may alias a following declaration
		'' Add it to a list for processing later. We have to lookup
		'' following declarations until the one that's being aliased is
		'' found, then the #define can be moved behind it, and be turned
		'' into a proper declaration.
		addForward(aliasedid, def)
	end if
end sub

sub Define2Decl.workDecl(byval ast as AstNode ptr, byval decl as AstNode ptr, byval insertbehind as AstNode ptr)
	if mayTurnDefs2Decl(decl) = FALSE then exit sub

	'' Check whether we saw an alias #define for this declaration before
	var item = forwards.lookup(decl->text, hashHash(decl->text))
	if item->s then
		dim info as ForwardInfo ptr = item->data
		if info then
			var insertbefore = insertbehind->nxt

			'' Move the collected #defines behind the declaration and process them
			for i as integer = 0 to info->defcount - 1
				var aliasdef = info->defs[i]
				ast->unlink(aliasdef)
				ast->insert(aliasdef, insertbefore)

				'' This may even move the #define into a different header file
				aliasdef->location = insertbehind->location

				turnDefine2Decl(aliasdef, decl)
			next

			'' And "remove" the alias #defines from the collected list,
			'' so they won't be matched/moved again.
			delete info
			item->data = NULL
		end if
	end if

	decls.addOverwrite(decl->text, decl)
end sub

sub Define2Decl.work(byval ast as AstNode ptr)
	var i = ast->head
	while i

		'' Process alias #defines
		if (i->kind = ASTKIND_PPDEFINE) andalso (i->paramcount = -1) andalso i->expr then
			select case i->expr->kind
			case ASTKIND_TEXT
				handleAliasDefine(i, i->expr->text)
			case ASTKIND_DATATYPE
				turnDefine2Typedef(i)
			case else
				'' Body is a constant expression?
				if isConstantExpr(i->expr) andalso (not exprInvolvesUndeclaredId(i->expr)) then
					i->kind = ASTKIND_CONST
				end if
			end select
		end if

		'' Register declarations that can be aliased via #defines
		'' (This also handles declarations produced above by turning
		'' #defines into declarations)
		select case i->kind
		case ASTKIND_CONST, ASTKIND_PROC, ASTKIND_TYPEDEF, _
		     ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM
			if i->text then
				workDecl(ast, i, i)
			end if

			if i->kind = ASTKIND_ENUM then
				'' Register enum constants too
				var enumconst = i->head
				while enumconst
					if enumconst->kind = ASTKIND_CONST then
						workDecl(ast, enumconst, i)
					end if
					enumconst = enumconst->nxt
				wend
			end if

		case ASTKIND_VAR
			if (i->attrib and (ASTATTRIB_LOCAL or ASTATTRIB_STATIC)) = 0 then
				workDecl(ast, i, i)
			end if
		end select

		i = i->nxt
	wend
end sub

private sub hlDefine2Decl(byval ast as AstNode ptr)
	dim pass as Define2Decl
	pass.countDecls(ast)
	pass.work(ast)
end sub

type EmittedIds
	ids as THash = THash(8, TRUE) '' data = boolean: id ever used as FB keyword?
	declare operator let(byref as const EmittedIds) '' unimplemented
	declare sub collect(byval ast as AstNode ptr)
end type

sub EmittedIds.collect(byval ast as AstNode ptr)
	dim fbcode as emit.CodeGen
	fbcode.emitCode(ast)
	for i as integer = 0 to fbcode.tokens.count - 1
		var t = fbcode.tokens.get(i)
		if t >= emit.TK_ID then
			'' Register used id, if not yet done
			var is_fb_keyword = (t > emit.TK_ID)
			var text = iif(t = emit.TK_ID, fbcode.tokens.strings.p[fbcode.tokens.p[i].payload], emit.tokentext(t))
			var hash = hashHash(text)
			var item = ids.lookup(text, hash)
			if item->s then
				'' Already known used id, but update "used as FB keyword" status
				item->data = cptr(any ptr, cint(item->data) or is_fb_keyword)
			else
				ids.add(item, hash, text, cptr(any ptr, is_fb_keyword))
			end if
		end if
	next
end sub

private function idsDifferInCaseOnly(byref a as const string, byref b as const string) as integer
	'' Not a reference to the macro param (from C POV),
	'' but from FB POV? (due to case insensitivity)
	return (a <> b) andalso (lcase(a, 1) = lcase(b, 1))
end function

private function macroParamConflictsWithIdInBody(byref param as const string, byref ids as EmittedIds) as integer
	for i as integer = 0 to ids.ids.room - 1
		var item = ids.ids.items + i
		if item->s then
			var usedid = *item->s
			var is_fb_keyword = cint(item->data)

			if param = usedid then
				'' It's a reference to the macro param (no conflict), or an FB keyword (conflict)
				return is_fb_keyword
			end if

			if lcase(param, 1) = lcase(usedid, 1) then
				'' It's an id or FB keyword that differs from the macro param only in case (conflict)
				return TRUE
			end if
		end if
	next
	return FALSE
end function

private function macroParamConflictsWithMacroParam(byref param as const string, byval macro as AstNode ptr) as integer
	var otherparam = macro->head
	while otherparam
		if idsDifferInCaseOnly(*otherparam->text, param) then
			return TRUE
		end if
		otherparam = otherparam->nxt
	wend
	return FALSE
end function

type MacroParamRenamer
	as string oldname, newname
	declare constructor(byref oldname as string, byref newname as string)
	declare sub walk(byval n as AstNode ptr)
end type

constructor MacroParamRenamer(byref oldname as string, byref newname as string)
	this.oldname = oldname
	this.newname = newname
end constructor

sub MacroParamRenamer.walk(byval n as AstNode ptr)
	if n->kind = ASTKIND_TEXT then
		if *n->text = oldname then
			n->setText(newname)
		end if
	end if

	if n->subtype then walk(n->subtype)
	if n->array   then walk(n->array  )
	if n->expr    then walk(n->expr   )

	var i = n->head
	while i
		walk(i)
		i = i->nxt
	wend
end sub

''
'' Check for macro parameter names that would conflict with other identifiers,
'' e.g. typedefs, variables, function names) or FB keywords used in the macro
'' body.
''
private sub autoRenameConflictingMacroParams(byval macro as AstNode ptr)
	var found_conflict = FALSE
	do
		'' Collect ids used in the macro body
		dim ids as EmittedIds
		ids.collect(macro->expr)

		''
		'' Check used ids against macro params. If we find a conflict,
		'' auto-rename the macro param and re-check everything.
		''
		'' Macro params can conflict with any alphanumeric string in the macro
		'' body, such as FB keywords or variable/function/typedef identifiers.
		'' We have to auto-detect this because fbc won't.
		''
		'' Macro params can also conflict with each-other, but fbc will detect
		'' that, so we don't have to handle it. Just take care not to run an
		'' infinite loop when auto-renaming.
		''
		found_conflict = FALSE
		var param = macro->head
		do
			var paramname = *param->text
			if macroParamConflictsWithIdInBody(paramname, ids) and _
			   (not macroParamConflictsWithMacroParam(paramname, macro)) then
				'' Auto-rename
				var newname = paramname + "_"
				param->setText(newname)
				scope
					dim renamer as MacroParamRenamer = MacroParamRenamer(paramname, newname)
					renamer.walk(macro->expr)
				end scope
				found_conflict = TRUE
			end if
			param = param->nxt
		loop while param

		'sleep
		while inkey <> "" : wend
	loop while found_conflict
end sub

private sub hlAutoRenameConflictingMacroParams(byval ast as AstNode ptr)
	var i = ast->head
	while i
		if i->kind = ASTKIND_PPDEFINE then
			if (i->expr <> NULL) and (i->paramcount > 0) then
				autoRenameConflictingMacroParams(i)
			end if
		end if
		i = i->nxt
	wend
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hlCountCallConvs(byval n as AstNode ptr) as integer
	select case n->kind
	case ASTKIND_PROC
		hl.need_extern = TRUE
		if n->attrib and ASTATTRIB_STDCALL then
			hl.stdcalls += 1
		else
			assert(n->attrib and ASTATTRIB_CDECL)
			hl.cdecls += 1
		end if
	case ASTKIND_VAR
		hl.need_extern or= ((n->attrib and ASTATTRIB_EXTERN) <> 0) or _
		                   ((n->attrib and ASTATTRIB_STATIC) = 0)
	end select
	'' Don't count code in macro bodies
	function = (n->kind <> ASTKIND_PPDEFINE)
end function

private function hlHideCallConv(byval n as AstNode ptr) as integer
	if n->kind = ASTKIND_PROC then
		if n->attrib and hl.mostusedcallconv then
			n->attrib or= ASTATTRIB_HIDECALLCONV
		end if
	end if
	'' Don't hide callconvs in macro bodies, otherwise they could
	'' end up using the wrong callconv if expanded outside the
	'' header's Extern block.
	function = (n->kind <> ASTKIND_PPDEFINE)
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
	declare sub work(byval ast as AstNode ptr, byref bioptions as ApiSpecificBiOptions)
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

sub IncludePass.work(byval ast as AstNode ptr, byref bioptions as ApiSpecificBiOptions)
	'' Find node above which new #includes should be inserted. This should
	'' always be behind existing #includes at the top.
	var top = ast->head
	while top andalso (top->kind = ASTKIND_PPINCLUDE)
		top = top->nxt
	wend

	'' Start at this "top" node, otherwise we'd run an infinite loop trying
	'' to move things to the top that already are at the top (because we'd
	'' be *above* the "top" node, and moving #includes *down* to it, which
	'' means we'd visit them again and again, since we're cycling from top
	'' to bottom via the "next" pointers).
	var i = top
	while i
		var nxt = i->nxt

		'' #include?
		if i->kind = ASTKIND_PPINCLUDE then
			'' Move to top (outside of Extern block), without changing the order
			assert(i <> top)
			ast->unlink(i)
			ast->insert(i, top)
		end if

		i = nxt
	wend

	'' Add #includes from -addinclude options
	if bioptions.addincludes then
		ast->insert(bioptions.addincludes, top)
		bioptions.addincludes = NULL
	end if

	'' For each #include at the top, apply -removeinclude and remap *.h => *.bi
	i = ast->head
	while i <> top
		var nxt = i->nxt

		'' #include?
		if i->kind = ASTKIND_PPINCLUDE then
			var filename = *i->text
			if hl.api->removeinclude.contains(filename, hashHash(filename)) then
				ast->remove(i)
			else
				translateHeaderFileName(filename)
				i->setText(filename)
			end if
		end if

		i = nxt
	wend

	'' Remove duplicate #includes
	dim includes as THash = THash(6, TRUE)

	i = ast->head
	while i <> top
		var nxt = i->nxt

		'' #include?
		if i->kind = ASTKIND_PPINCLUDE then
			var hash = hashHash(i->text)
			var item = includes.lookup(i->text, hash)
			if item->s then
				ast->remove(i)
			else
				includes.add(item, hash, i->text, NULL)
			end if
		end if

		i = nxt
	wend
end sub

private function hlSearchSpecialDtypes(byval n as AstNode ptr) as integer
	select case typeGetDt(n->dtype)
	case TYPE_CLONG, TYPE_CULONG
		hl.uses_clong = TRUE
	case TYPE_CLONGDOUBLE
		hl.uses_clongdouble = TRUE
	end select
	function = TRUE
end function

private function hlBuildRenameList(byval n as AstNode ptr) as AstNode ptr
	var list = astNewGROUP()

	var i = n->head
	while i

		if (i->text <> NULL) and (i->origid <> NULL) and _
		   (i->kind <> ASTKIND_UNDEF) then
			var msg = astDumpPrettyKind(i->kind)
			msg += " " + *i->origid + " => " + *i->text
			list->append(astNewTEXT(msg))
		end if

		'' TODO: recurse into struct/union if renaming fields...
		''list->append(hlBuildRenameList(i))

		if i->kind = ASTKIND_ENUM then
			'' Handle enumconsts - they're in the same scope as the enum itself anyways,
			'' so we don't need to worry about making a nested renamelist for the enum.
			list->append(hlBuildRenameList(i))
		end if


		i = i->nxt
	wend

	function = list
end function

'' while (0) ...      =>   scope : ... : end scope
'' do ... while (0);  =>   scope : ... : end scope
'' (commonly used construct in C headers)
private function hlTurnWhile0IntoScope(byval n as AstNode ptr) as integer
	select case n->kind
	case ASTKIND_DOWHILE, ASTKIND_WHILE
		'' Loop condition is just a 0?
		if n->expr->isConst0() then
			delete n->expr
			n->expr = NULL
			n->kind = ASTKIND_SCOPEBLOCK
		end if
	end select
	function = TRUE
end function

private function hlSolveOutUnnecessaryScopeBlocks(byval n as AstNode ptr) as integer
	select case n->kind
	case ASTKIND_SCOPEBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE, _
	     ASTKIND_IFPART, ASTKIND_ELSEIFPART, ASTKIND_ELSEPART
		'' Contains just a SCOPEBLOCK?
		while n->hasOnlyChild(ASTKIND_SCOPEBLOCK)
			'' Move scope block's children in place of the scope block
			n->append(n->head->cloneChildren())
			n->remove(n->head)
		wend

	case ASTKIND_PPDEFINE
		'' If the macro body is a scope block that contains just one if/loop block,
		'' then remove the scope block
		do
			if n->expr = NULL then exit do
			if n->expr->kind <> ASTKIND_SCOPEBLOCK then exit do

			var scopeblock = n->expr

			'' Exactly one statement?
			if scopeblock->has1Child() = FALSE then exit do

			'' It's a scope/if/loop?
			select case scopeblock->head->kind
			case ASTKIND_SCOPEBLOCK, ASTKIND_IFBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE
				'' Remove the outer scope block, and directly use the inner block as macro body.
				var stmt = scopeblock->head
				scopeblock->unlink(stmt)
				delete n->expr
				n->expr = stmt

			case else
				exit do
			end select
		loop
	end select

	function = TRUE
end function

private function hlCreateElseIfs(byval n as AstNode ptr) as integer
	if n->kind = ASTKIND_IFBLOCK then
		do
			'' Has an else part?
			var elsepart = n->tail
			if (elsepart = NULL) orelse (elsepart->kind <> ASTKIND_ELSEPART) then
				exit do
			end if

			'' Contains just an IFBLOCK?
			if elsepart->hasOnlyChild(ASTKIND_IFBLOCK) = FALSE then
				exit do
			end if

			var nestedif = elsepart->head
			assert(nestedif->head->kind = ASTKIND_IFPART)

			'' Copy the if/elseif/else parts from the nested ifblock
			'' into the parent ifblock, behind the else part
			n->append(nestedif->cloneChildren())

			'' Turn the added if part into an elseif
			assert(elsepart->nxt->kind = ASTKIND_IFPART)
			elsepart->nxt->kind = ASTKIND_ELSEIFPART

			n->remove(elsepart)
		loop
	end if
	function = TRUE
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

private function hIsPPElseOrEnd(byval n as AstNode ptr) as integer
	select case n->kind
	case ASTKIND_PPELSEIF, ASTKIND_PPELSE, ASTKIND_PPENDIF
		function = TRUE
	end select
end function

private function hAreSimilar(byval a as integer, byval b as integer) as integer
	if a = b then return TRUE

	'' FBCODE is probably ok
	if (a = ASTKIND_FBCODE) or (b = ASTKIND_FBCODE) then return TRUE

	'' Treat #defines/constants as equal, because they're often the same thing anyways
	#define check(x, y) if ((a = x) and (b = y)) or ((a = y) and (b = x)) then return TRUE
	check(ASTKIND_PPDEFINE, ASTKIND_CONST)

	function = FALSE
end function

private function hSkipStatementsInARow(byval i as AstNode ptr) as AstNode ptr
	select case i->kind
	'' All parts of one #if block
	case ASTKIND_PPIF
		do
			i = i->nxt
		loop while i andalso hIsPPElseOrEnd(i)
		return i

	case ASTKIND_PPINCLUDE, ASTKIND_INCLIB
		var astkind = i->kind
		do
			i = i->nxt
		loop while i andalso (i->kind = astkind)
		return i

	case ASTKIND_PRAGMAONCE, ASTKIND_RENAMELIST, ASTKIND_TITLE, _
	     ASTKIND_EXTERNBLOCKBEGIN, ASTKIND_EXTERNBLOCKEND, _
	     ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, _
	     ASTKIND_IFBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE
		return i->nxt

	'' Procedure body?
	case ASTKIND_PROC
		if i->expr then
			return i->nxt
		end if
	end select

	'' Anything else (single-line declarations): collect all in a row as
	'' long as it's the same kind. However, small deviations are allowed,
	'' if it's just a few declarations in the middle of a bigger block.
	const BIGTHRESHOLD = 3
	var dominantkind = -1
	do
		var blockkind = i->kind
		var length = 0
		var nxt = i
		do
			select case nxt->kind
			case ASTKIND_PPIF, ASTKIND_INCLIB, ASTKIND_PPINCLUDE, _
			     ASTKIND_PRAGMAONCE, ASTKIND_RENAMELIST, ASTKIND_TITLE, _
			     ASTKIND_EXTERNBLOCKBEGIN, ASTKIND_EXTERNBLOCKEND, _
			     ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, _
			     ASTKIND_IFBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE
				exit do
			case ASTKIND_PROC
				if nxt->expr then
					exit do
				end if
			end select

			length += 1
			nxt = nxt->nxt
		loop while nxt andalso hAreSimilar(blockkind, nxt->kind)

		'' Next statement can't be in a block?
		if length = 0 then
			exit do
		end if

		'' Block dominant due to its size?
		if length >= BIGTHRESHOLD then
			'' First?
			if dominantkind = -1 then
				dominantkind = i->kind
			'' Others must have a similar astkind
			elseif hAreSimilar(dominantkind, i->kind) = FALSE then
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
sub hlAutoAddDividers(byval ast as AstNode ptr)
	'' Recurse where needed
	scope
		var i = ast->head
		while i
			select case i->kind
			case ASTKIND_STRUCT, ASTKIND_UNION, ASTKIND_ENUM, _
			     ASTKIND_PPIF, ASTKIND_PPELSEIF, ASTKIND_PPELSE, _
			     ASTKIND_SCOPEBLOCK, ASTKIND_DOWHILE, ASTKIND_WHILE
				hlAutoAddDividers(i)
			case ASTKIND_IFBLOCK
				var part = i->head
				while part
					hlAutoAddDividers(part)
					part = part->nxt
				wend
			end select
			i = i->nxt
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
			ast->insert(astNew(ASTKIND_DIVIDER), i)
		loop
	end if
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''
'' Global (entire AST of an API) highlevel transformations, intended to run 1st
''
'' If any declarations are added to the AST here, care must be taken to set the
'' AstNode.location, so they can be assigned to the proper output .bi file.
''
sub hlGlobal(byval ast as AstNode ptr, byref api as ApiInfo)
	hl.api = @api

	if api.removeEmptyReservedDefines then
		hlRemoveEmptyReservedDefines(ast)
	end if

	if api.idopt(tktokens.OPT_REMOVE).nonEmpty then
		hlApplyRemoveOption(ast, -1, tktokens.OPT_REMOVE)
	end if

	if api.idopt(tktokens.OPT_REMOVEPROC).nonEmpty then
		hlApplyRemoveOption(ast, ASTKIND_PROC, tktokens.OPT_REMOVEPROC)
	end if

	if api.idopt(tktokens.OPT_REMOVEVAR).nonEmpty then
		hlApplyRemoveOption(ast, ASTKIND_VAR, tktokens.OPT_REMOVEVAR)
	end if

	if api.idopt(tktokens.OPT_REMOVE1ST).nonEmpty then
		hlApplyRemove1st(ast)
	end if

	if api.idopt(tktokens.OPT_REMOVE2ND).nonEmpty then
		hlApplyRemove2nd(ast)
	end if

	if api.idopt(tktokens.OPT_DROPPROCBODY).nonEmpty then
		hlApplyDropProcBodyOptions(ast)
	end if

	ast->visit(@hlSetArraySizes)

	'' Apply -rename* options, if any
	if api.have_renames then
		ast->visit(@hlApplyRenameOption)
	end if

	'' Remove exact-alias #defines (e.g. <#define A A>), they're neither
	'' needed nor possible in FB. This also prevents Define2Decl from
	'' counting such #defines as declarations.
	hlRemoveSelfDefines(ast)

	'' Run Define2Decl before typedef expansion passes, so that they get to
	'' see the typedefs produced here.
	hlDefine2Decl(ast)

	scope
		dim pass as ExpressionFixUp
		pass.collectSymbolsAndCalculateTypes(ast)
		pass.handleForwards()
		pass.fixExpressions(ast)
	end scope

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
	ast->visit(@hlFixSpecialParameters)

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
	scope
		dim renames as RenameJobs
		hlSolveOutTagIds(ast, renames)
		renames.walkAndApply(ast)
	end scope

	'' 2. Solve out remaining exact/case-alias typedefs - they're useless
	'' and/or impossible in FB, because it's case-insensitive and doesn't
	'' have separate tag/typedef namespaces.
	scope
		dim renames as RenameJobs
		hlRemoveSameIdTypedefs(ast, renames)
		renames.walkAndApply(ast)
	end scope

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
	if hl.typehash = NULL then
		oops("hl.typehash THash memory allocation failed")
	end if
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

	'' Run Define2Decl again - typedef expansion inside #define bodies may
	'' allow additional #defines to be turned into constants now, etc.
	'' (e.g. <#define A (FOO*)0> may have become <#define A (int*)0>)
	hlDefine2Decl(ast)

	if api.fixunsizedarrays then
		hlFixUnsizedArrays(ast)
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
			hlApplyMoveOption(ast, *i->text, *i->alias_)
			i = i->nxt
		wend
	end if

	if api.idopt(tktokens.OPT_UNDEFBEFOREDECL).nonEmpty then
		hlAddUndefsAboveDecls(ast)
	end if

	if api.idopt(tktokens.OPT_IFNDEFDECL).nonEmpty then
		hlAddIfndefsAroundDecls(ast)
	end if

	hlProcs2Macros(ast)

	hlAutoRenameConflictingMacroParams(ast)
end sub

''
'' .bi-file-specific highlevel transformations, intended to run on the
'' API-specific ASTs in each output .bi file.
''
sub hlFile(byval ast as AstNode ptr, byref api as ApiInfo, byref bioptions as ApiSpecificBiOptions)
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
	ast->visit(@hlCountCallConvs)
	if hl.need_extern then
		if hl.stdcalls > hl.cdecls then
			hl.mostusedcallconv = ASTATTRIB_STDCALL
		else
			hl.mostusedcallconv = ASTATTRIB_CDECL
		end if

		'' Remove the calling convention from all procdecls,
		'' the Extern block will take over
		ast->visit(@hlHideCallConv)

		var externblock = @"C"
		if hl.mostusedcallconv = ASTATTRIB_STDCALL then
			if api.windowsms then
				externblock = @"Windows-MS"
			else
				externblock = @"Windows"
			end if
		end if

		assert(ast->kind = ASTKIND_GROUP)
		ast->prepend(astNew(ASTKIND_EXTERNBLOCKBEGIN, externblock))
		ast->append(astNew(ASTKIND_EXTERNBLOCKEND))
	end if

	'' If any symbols from this file were renamed, add a rename list at the top
	'' TODO: add it above #inclibs/#includes even
	if api.have_renames then
		var entries = hlBuildRenameList(ast)
		if entries->head then
			var renamelist = astNew(ASTKIND_RENAMELIST, "The following symbols have been renamed:")
			renamelist->append(entries)
			ast->prepend(renamelist)
		else
			delete entries
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
	ast->visit(@hlSearchSpecialDtypes)
	if hl.uses_clongdouble then
		ast->prepend(astNew(ASTKIND_PPINCLUDE, "crt/longdouble.bi"))
	end if
	if hl.uses_clong then
		ast->prepend(astNew(ASTKIND_PPINCLUDE, "crt/long.bi"))
	end if

	'' Prepend #inclibs/#undefs
	if bioptions.undefs then
		ast->prepend(bioptions.undefs)
		bioptions.undefs = NULL
	end if
	if bioptions.inclibs then
		ast->prepend(bioptions.inclibs)
		bioptions.inclibs = NULL
	end if

	ast->visit(@hlTurnWhile0IntoScope)
	ast->visit(@hlSolveOutUnnecessaryScopeBlocks)
	ast->visit(@hlCreateElseIfs)
end sub

function hlCountDecls(byval ast as AstNode ptr) as integer
	var n = 0

	var i = ast->head
	while i
		select case i->kind
		case ASTKIND_DIVIDER, ASTKIND_PPINCLUDE, ASTKIND_PPENDIF, _
		     ASTKIND_EXTERNBLOCKBEGIN, ASTKIND_EXTERNBLOCKEND, _
		     ASTKIND_INCLIB, ASTKIND_PRAGMAONCE, _
		     ASTKIND_UNKNOWN, ASTKIND_RENAMELIST, ASTKIND_TITLE

		case ASTKIND_PPIF, ASTKIND_PPELSEIF, ASTKIND_PPELSE
			n += hlCountDecls(i)

		case ASTKIND_PPDEFINE
			if (i->expr = NULL) orelse (i->expr->kind <> ASTKIND_UNKNOWN) then
				n += 1
			end if

		case else
			n += 1
		end select

		i = i->nxt
	wend

	function = n
end function

function hlCountTodos(byval n as AstNode ptr) as integer
	var count = 0

	if n->kind = ASTKIND_UNKNOWN then
		count += 1
	end if

	if n->subtype then count += hlCountTodos(n->subtype)
	if n->array   then count += hlCountTodos(n->array  )
	if n->expr    then count += hlCountTodos(n->expr   )

	var i = n->head
	while i
		count += hlCountTodos(i)
		i = i->nxt
	wend

	function = count
end function
